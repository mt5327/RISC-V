library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.ALL;

entity FP_Divider is
	generic (
		P : NATURAL;
		E : NATURAL; 
		M : NATURAL);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		result_o : out FP_RESULT);
end FP_Divider; 

architecture behavioral of FP_Divider is

	
	constant BIAS : signed(E downto 0) := to_signed(2 ** (E - 1) - 1, E + 1);
	constant BIAS_DIV_2 : signed(E-1 downto 0) := to_signed(2 ** (E - 2) - 1, E);
	constant INFINITY : STD_LOGIC_VECTOR (P - 2 downto 0) := (P - 2 downto P - E - 1 => '1', others => '0');
    constant NUM_ITERS : natural := (M-2) / 2;

	signal counter : unsigned (num_bits(NUM_ITERS) downto 0);

	signal sign, sign_reg, div, sqrt, div_reg, sqrt_reg, inexact, inexact_reg, div_by_zero, sign_d : STD_LOGIC := '0';
	signal invalid, invalid_div, invalid_sqrt, special_case : STD_LOGIC := '0';
	signal mantissa_x, mantissa_y : STD_LOGIC_VECTOR (M - 1 downto 0);
    signal exponent_div, exponent_x, exponent_y : signed(E downto 0);
    signal exp, exponent_div_reg, exponent_div_final, exponent_div_minus_one, exponent_sqrt, exponent_sqrt_reg : STD_LOGIC_VECTOR (E-1 downto 0);
   
	alias exp_odd : STD_LOGIC is x_i(P-E-1);
	alias sign_x : STD_LOGIC is x_i(P - 1);
	alias sign_y : STD_LOGIC is y_i(P - 1);

    signal digit_sign : STD_LOGIC;

    signal digit, last_digit, sign_rem : signed(2 downto 0);

	signal q, qm : STD_LOGIC_VECTOR(q_length(M-2) downto 0) := (others => '0');	
    signal mantissa : STD_LOGIC_VECTOR (q_length(M) downto 0) := (others => '0');

	signal special_value : STD_LOGIC_VECTOR (P-1 downto 0);
     
    signal mantissa_final : STD_LOGIC_VECTOR (M-1 downto 0);
    signal mantissa_round : STD_LOGIC_VECTOR (P-2 downto 0);

	signal square_estimate, estimate, rm : STD_LOGIC_VECTOR (2 downto 0);

	signal a, b : STD_LOGIC_VECTOR (1 downto 0);
	
	signal cmp : STD_LOGIC_VECTOR (3 downto 0);
        
  	signal append_one, append_two : STD_LOGIC_VECTOR(q'left+2 downto 0);
    signal x_reg, y_reg, k, y, y_init : STD_LOGIC_VECTOR (q'left+2 downto 0);

    signal F, PR, PR_mul, PR_init, PR_sqrt, PR_div, PR_new : STD_LOGIC_VECTOR (q'left+4 downto 0);
   
    signal carry : unsigned (q'left+4 downto 0);
  
	signal lda, ldb : STD_LOGIC;
	signal position : NATURAL range q'left downto 0;
        
	type state_type is (IDLE, DIVIDE, ROUND, FINALIZE);
	signal state, next_state : state_type;
    
    type selection_constants_t is array(0 to 7, 0 to 3) of signed(6 downto 0);
    constant SEL_CONSTANTS : selection_constants_t := ( 
        ( to_signed(12, 7), to_signed(4, 7), to_signed(-4, 7), to_signed(-13, 7) ),
        ( to_signed(14, 7), to_signed(4, 7), to_signed(-5, 7), to_signed(-15, 7) ),
        ( to_signed(15, 7), to_signed(4, 7), to_signed(-6, 7), to_signed(-16, 7) ),
        ( to_signed(16, 7), to_signed(4, 7), to_signed(-6, 7), to_signed(-17, 7) ),
        ( to_signed(18, 7), to_signed(6, 7), to_signed(-6, 7), to_signed(-18, 7) ),
        ( to_signed(20, 7), to_signed(6, 7), to_signed(-8, 7), to_signed(-20, 7) ),
        ( to_signed(20, 7), to_signed(8, 7), to_signed(-8, 7), to_signed(-22, 7) ),
        ( to_signed(22, 7), to_signed(8, 7), to_signed(-8, 7), to_signed(-23, 7) ) 
    );
    
    component FP_Classifier is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            fp_class_o : out FP_INFO);
    end component FP_Classifier;
    
    component rounder is
        generic (SIZE : NATURAL);
        port (
            x_i : in unsigned (SIZE - 1 downto 0);
            sign_i : in STD_LOGIC;
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
            z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
    end component rounder;

    
    signal fp_info_x, fp_info_y : FP_INFO;
     
begin

    FP_CLASS_X : FP_Classifier generic map(P, E, M) port map(x_i, fp_info_x);
    FP_CLASS_Y : FP_Classifier generic map(P, E, M) port map(y_i, fp_info_y);
    
    SYNC_PROC : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                state <= IDLE;
            else
                state <= next_state;
            end if;
        end if;
    end process;
    
    NEXT_STATE_DECODE : process (all)
    begin
        next_state <= state;
        case state is
            when IDLE => 
                if enable_i = '1' then
                    if invalid = '1' or div_by_zero = '1' then 
                        next_state <= FINALIZE;
                    else
                        next_state <= DIVIDE;
                    end if; 
                end if;
            when DIVIDE => 
                if counter = 0 then
                    next_state <= ROUND;
                end if;
            when ROUND => next_state <= FINALIZE;
            when FINALIZE => next_state <= IDLE;
            when others => next_state <= IDLE;
        end case;
    end process;

    mantissa_x <= fp_info_x.normal & x_i(M - 2 downto 0);
    mantissa_y <= fp_info_y.normal & y_i(M - 2 downto 0);
    
    exponent_x <= signed('0' & x_i(P - 2 downto P - E - 1));
    exponent_y <= signed('0' & y_i(P - 2 downto P - E - 1));
    
    exponent_div <= exponent_x - exponent_y + BIAS + 2 - ( ( (E downto 1 => '0') & fp_info_x.normal) +
                    ( (E downto 1 => '0') & fp_info_y.normal ) );
   
    exponent_sqrt <= STD_LOGIC_VECTOR(('0' & exponent_x(E - 1 downto 1)) + BIAS_DIV_2 + exp_odd);
    
    sign_d <= sign_x xor sign_y;
   
    div <= '1' when fp_op_i = FPU_DIV else '0';
    sqrt <= '1' when fp_op_i = FPU_SQRT else '0';
    
    invalid_div <= fp_info_x.nan or fp_info_y.nan or ( fp_info_x.zero and fp_info_y.inf ) or ( fp_info_x.inf and fp_info_y.zero );
    invalid_sqrt <= fp_info_x.nan or ( ( not fp_info_x.zero ) and sign_x );
         
    div_by_zero <= fp_info_y.zero and div;
     
    PR_div <= STD_LOGIC_VECTOR(
            ("0" & unsigned(mantissa_x) & (PR_div'left-M-1 downto 0 => '0')) +
            ("1" & unsigned(not mantissa_y) & (PR_div'left-M-1 downto 0 => '1')) +
            to_unsigned(1, PR_div'length));
     
    y_init <= "0" & mantissa_y & (y_init'length - mantissa_y'length - 2 downto 0 => '0');

    PR_sqrt <= "110" & mantissa_x & (PR_sqrt'length - mantissa_x'length-4 downto 0 => '0') when exp_odd = '1' else 
               "11" & mantissa_x & (PR_sqrt'length - mantissa_x'length-3 downto 0 => '0');
     
    SPECIAL_CASES : process (all)
    begin
        -- Canonical NaN
		special_value <= (P - 2 downto P - E - 2 => '1', others => '0');
        if fp_info_y.zero and div then
            special_value <= sign_d & INFINITY;
        end if;
    end process;
    
    PR_init <= PR_div when div = '1' else PR_sqrt;                                               

    DIVISION : process (clk_i)
    begin
        if rising_edge(clk_i) then
            case state is
                when IDLE =>
                    qm <= (others => '0');
                    q <= (q'left => '1', others => '0');
                    exponent_sqrt_reg <= exponent_sqrt;
                    exponent_div_reg <= STD_LOGIC_VECTOR(exponent_div(E-1 downto 0));
                    append_one <= (append_one'left-1 downto append_one'left-3 => sqrt, others => '0');
                    append_two <= (append_two'left downto append_two'left-1 => sqrt, others => '0');
                    PR <= PR_init;
                    div_reg <= div;
                    special_case <= invalid or div_by_zero;
                    rm <= rm_i;
                    sqrt_reg <= sqrt;
                    y <= y_init;
                    k <= (k'left => '1', others => div);
                    sign <= sign_reg; 
                    position <= q'left-1;   
                    counter <= to_unsigned(NUM_ITERS, counter'length);
                when DIVIDE =>
                    k <= "11" & k(k'left downto 2);
                    counter <= counter - 1;
                    PR <= PR_mul;
                    append_one <= "00" & append_one(append_one'left downto 2);
                    append_two <= "00" & append_two(append_two'left downto 2);
                    position <= position - 2;
                    if lda = '1' then
                        q <= qm;
                    end if;

                    if ldb = '1' then
                        qm <= q;
                    end if;
                    
                    q(position downto position - 1) <= a;
                    qm(position downto position - 1) <= b;
                when ROUND => 
                    inexact_reg <= inexact;
                    mantissa <= q & STD_LOGIC_VECTOR(digit(1 downto 0)) when digit_sign = '0' else 
                               qm & STD_LOGIC_VECTOR(digit(1 downto 0));                    
                when FINALIZE =>
                    special_case <= '0';          
                when others =>
                
            end case;
        end if;
    end process;    
    
    digit <= (digit_sign & signed(a)) - sign_rem;                      

               
    MANTISSA_FINAL_GEN: if M = 24 generate 
        mantissa_final <= mantissa(mantissa'left-1 downto 2) when mantissa(mantissa'left) = '1' else 
                          mantissa(mantissa'left-2 downto 1);     
    else generate 
        mantissa_final <= mantissa(mantissa'left-1 downto 1) when mantissa(mantissa'left) = '1' else 
                          mantissa(mantissa'left-2 downto 0);         
    end generate;   

    ROUNDING: rounder generic map(P-1) port map (
        unsigned(exp) & unsigned(mantissa_final(mantissa_final'left downto 1)), 
        sign, 
        rm,
        mantissa_final(0) & inexact, 
        mantissa_round);      
              
    inexact <= or PR;
    exponent_div_minus_one <= STD_LOGIC_VECTOR(unsigned(exponent_div_reg(E-1 downto 0)) - 1);
    sign_reg <= sign_d and div;

    lda <= PR(PR'left);
    ldb <= (not PR(PR'left)) and (or cmp(1 downto 0));
    
    digit_sign <= or cmp(3 downto 2);
    sign_rem <= "00" & PR_mul(PR'left);

    a <= (cmp(0) or cmp(2) or cmp(3)) & (cmp(1) or cmp(2));
    
    with a select 
        b <= "11" when "00",
             "00" when "01",
             "01" when "10",
             "10" when "11",
             "--" when others;

    x_reg <= q & "00" when sqrt_reg = '1' else y;     
    y_reg <= qm & "00" when sqrt_reg = '1' else y;
    
    F(F'left) <= cmp(1) or cmp(0);
    F(F'left-1) <= cmp(1) or (cmp(0) and not x_reg(x_reg'left) and k(x_reg'left) ) or ( cmp(3) and y_reg(y_reg'left) );
    
    F_GEN: for i in F'left-2 downto 1 generate
        F(i) <= ( cmp(0) and not x_reg(i-1) and k(i-1) ) or (cmp(1) and not x_reg(i) and k(i) ) or ( cmp(2) and y_reg(i) ) or (cmp(3) and y_reg(i-1) ) or
                ( ( cmp(2) or cmp(1) ) and append_one(i) ) or
                ( ( cmp(3) or cmp(0) ) and append_two(i) );  
    end generate;
     
    F(0) <= ( div_reg and cmp(0) ) or (cmp(1) and not x_reg(0) and k(0) ) or (cmp(2) and y_reg(0) ); 
    
    square_estimate <= "100" when counter = NUM_ITERS else
                       "111" when q(q'left) = '1' else
                       q(q'left - 2 downto q'left - 4);

    estimate <= square_estimate when sqrt_reg = '1' else mantissa_y(M-2 downto M-4);
 
   	cmp(3) <= '1' when signed(PR(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 3) else '0'; -- -2       
    cmp(2) <= '1' when signed(PR(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 2) and cmp(3) = '0' else '0'; -- -1       
    
    cmp(1) <= '1' when signed(PR(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 1) and cmp(0) = '0' else '0'; -- 1       
    cmp(0) <= '1' when signed(PR(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 0) else '0';  -- 2       
   	
   	carry <= (0 => '1', others => '0') when PR(PR'left) = '0' and (or cmp ) = '1' and div_reg = '1' else (others => '0');
    PR_new <= STD_LOGIC_VECTOR(unsigned(PR) + unsigned(F) + carry);
    PR_mul <= PR_new(PR_new'left-2 downto 0) & "00";                
        
    invalid <= ( invalid_sqrt and sqrt ) or (invalid_div and div );

    exponent_div_final <= exponent_div_reg when mantissa(mantissa'left) = '1' else 
                          exponent_div_minus_one; 
   
    exp <= exponent_div_final when div_reg = '1' else 
           exponent_sqrt_reg when sqrt_reg = '1' else (others => '0');                               
    
    result_o.fflags <= invalid & div_by_zero & "000" when special_case = '1' else
                       "0000" & inexact;
                            
    
    RESULT_GEN: if P = 32 generate 
        result_o.value <= (63 downto 32 => '1') & sign_reg & mantissa_round when special_case = '0' else 
                          (63 downto 32 => '1') & special_value;
    else generate
        result_o.value <= sign_reg & mantissa_round when special_case = '0' else 
                          special_value; 
    end generate;

        
    result_o.valid <= '1' when state = FINALIZE else '0';
   
   
   
 end behavioral;  