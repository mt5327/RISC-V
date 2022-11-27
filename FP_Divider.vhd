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
		x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		y_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		result_o : out FP_RESULT);
end FP_Divider;

architecture behavioral of FP_Divider is

	constant BIAS : signed(E downto 0) := to_signed(2 ** (E - 1) - 1, E + 1);
	constant BIAS_DIV_2 : signed(E downto 0) := to_signed(2 ** (E - 2) - 1, E + 1);
	constant INFINITY : STD_LOGIC_VECTOR (P - 2 downto 0) := (P - 2 downto P - E - 1 => '1', others => '0');

	signal sign, div, sqrt, inexact, start_div, div_by_zero, sign_d : STD_LOGIC;
	signal invalid, invalid_div, invalid_sqrt, remainder_is_zero, remainder_is_zero_reg : STD_LOGIC;
	signal mantissa_x, mantissa_y : STD_LOGIC_VECTOR (M - 1 downto 0);
	
    signal exponent_div, exponent_sqrt, exponent_x, exponent_y : signed(E downto 0);
	signal exponent_div_final, exp, exponent_div_minus_one : unsigned(E-1 downto 0);
    signal exponent_sqrt_reg : unsigned(E-1 downto 0);

	alias exp_odd : STD_LOGIC is x_i(P-E-1);
	alias sign_x : STD_LOGIC is x_i(P - 1);
	alias sign_y : STD_LOGIC is y_i(P - 1);

    signal PR_select : STD_LOGIC_VECTOR(6 downto 0);

    signal norm : STD_LOGIC;

    signal rounded_digit : STD_LOGIC_VECTOR (2 downto 0);

	signal q, qm, qp, k : STD_LOGIC_VECTOR(next_even(M+1) downto 0);	
	signal x_reg, y_reg : STD_LOGIC_VECTOR (q'left downto 0);
	signal y : STD_LOGIC_VECTOR (q'left downto 0);
	signal counter : unsigned (num_bits(M/2) downto 0);
	signal special_value : STD_LOGIC_VECTOR (63 downto 0);

	signal square_estimate, estimate : STD_LOGIC_VECTOR (2 downto 0);

	signal a, b, round_sticky : STD_LOGIC_VECTOR (1 downto 0);
	
	signal cmp : STD_LOGIC_VECTOR (3 downto 0);
        
  	signal minus_one, minus_two, one, two, append_one, append_two, F : STD_LOGIC_VECTOR(M+5 downto 0);	
    signal PR, PR_mul, PR_new, PR_init, PR_div, PR_div_mul : STD_LOGIC_VECTOR (M+5 downto 0);
    signal carry : unsigned (M+5 downto 0);

	signal overflow, underflow, lda, ldb, special_case, sign_reg, subtract : STD_LOGIC;
	signal position : NATURAL range q'left downto 0;
	signal mantissa : unsigned (M - 2 downto 0);
	signal mantissa_final : STD_LOGIC_VECTOR (P - 2 downto 0);    
    
	type state_type is (IDLE, DIVIDE, FINALIZE);
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
            x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
            fp_class_o : out FP_INFO);
    end component FP_Classifier;
    
    signal fp_info_x, fp_info_y : FP_INFO;
    
    component rounder is
        generic (SIZE : NATURAL);
        port (
            x_i : in unsigned (SIZE - 1 downto 0);
            sign_i : in STD_LOGIC;
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
            z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
    end component rounder;
     
begin

    FP_CLASS_X : FP_Classifier generic map(P, E, M) port map(x_i(P - 2 downto 0), fp_info_x);
    FP_CLASS_Y : FP_Classifier generic map(P, E, M) port map(y_i(P - 2 downto 0), fp_info_y);
    
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
    
    NEXT_STATE_DECODE : process (state, start_div, counter)
    begin
        next_state <= state;
        case state is
            when IDLE => 
                if start_div = '1' then
                    next_state <= DIVIDE; 
                end if;
            when DIVIDE => 
                if counter = 0 then
                    next_state <= FINALIZE;
                end if;
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
    
    exponent_div_minus_one <= unsigned(exponent_div(E-1 downto 0)) - 1;
    exponent_sqrt <= ('0' & exponent_x(E - 1 downto 1)) + BIAS_DIV_2 + exp_odd;
    
    sign_d <= sign_x xor sign_y;
    
    div <= '1' when fp_op_i = FPU_DIV else '0';
    sqrt <= '1' when fp_op_i = FPU_SQRT else '0';
    
    invalid_div <= (fp_info_y.nan or ( fp_info_x.zero and fp_info_y.inf ) or ( fp_info_x.inf and fp_info_y.zero )) and div;
    invalid_sqrt <= (not fp_info_x.zero ) and sign_x and sqrt;
    
    invalid <= fp_info_x.nan or invalid_div or invalid_sqrt;
    div_by_zero <= fp_info_y.zero and div;
    special_case <= invalid or div_by_zero;
     
    SPECIAL_CASES : process (div_by_zero, sign_d)
    begin
        -- Canonical NaN
		special_value <= (P - 2 downto P - E - 2 => '1', others => '0');
        if div_by_zero = '1' then
            special_value <= (63 downto P - 1 => sign_d) & INFINITY;
        end if;
        
    end process;
        
    PR_INITIALIZE: process (all)
    begin
        if sqrt = '1' then
            if exp_odd = '1' then
                PR_init <= "110" & mantissa_x & "000";
            else
                PR_init <= "11" & mantissa_x & "0000";
            end if;
        else 
            PR_init <= PR_div_mul;                                               
        end if;
    end process;
    
    start_div <= '1' when enable_i = '1' and state = IDLE and invalid = '0' else '0';
    
    sign <= sign_x when sqrt = '1' else sign_d;

    PR_div <= STD_LOGIC_VECTOR(("0" & unsigned(mantissa_x) & "00000") +
              ("1" & unsigned(not mantissa_y) & "11111") +
              to_unsigned(1, PR_DIV'length));
    
    PR_div_mul <= PR_div(PR_div'left downto 2) & "00";
    
         
     DIVISION : process (clk_i)
    begin
        if rising_edge(clk_i) then
            case state is
                when IDLE =>
                    if enable_i = '1' then
                        qm <= (others => '0');
                        q <= (q'left => '1', others => '0');
                        qp <= (others => '0');
                        exponent_sqrt_reg <= unsigned(exponent_sqrt(E-1 downto 0));
                        PR <= PR_init;
                        y <= "0" & mantissa_y & (q'length-M-2 downto 0 => '0'); 
                        sign_reg <= sign;
                        k <= (k'left => '1', others => '0');
                        position <= q'left-1;                            
                        if sqrt = '1' then
                            append_one <= (append_one'left-3 downto append_one'left-5 => '1', others => '0');
                            append_two <= (append_two'left-2 downto append_two'left-3 => '1', others => '0');
                        else 
                            append_one <= (others => '0');
                            append_two <= (others => '0');
                        end if;         
                        counter <= to_unsigned(M/2, counter'length);
                    end if;
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
                when others =>
            end case;
        end if;
    end process;
       
    lda <= PR(PR'left);
    ldb <= (not PR(PR'left)) and (or cmp(1 downto 0));
         
    with cmp select 
        a <= "00" when "0000",
             "10" when "0001",
             "01" when "0010",
             "11" when "0100",
             "10" when "1000",
             "--" when others;

    with a select
        b <= "11" when "00",
             "00" when "01",
             "01" when "10",
             "10" when "11",
             "--" when others;
    
    x_reg <= (not q) and k when sqrt = '1' else not y; 
    y_reg <= qm when sqrt = '1' else y;

    one <= ("11" & x_reg & (M+3-q'length downto 0 => '0')) or append_one;
    two <= ("1" & x_reg & (M+4-q'length downto 0 => '0')) or append_two;
    minus_one <= ("00" & y_reg & (M+3-q'length downto 0 => '0')) or append_one;
    minus_two <= ("0" & y_reg & (M+4-q'length downto 0 => '0')) or append_two;
   
    square_estimate <= "100" when counter = (M/2) else
                       "111" when q(q'left) = '1' else
                       q(q'left - 2 downto q'left - 4);
  
    estimate <= square_estimate when sqrt = '1' else mantissa_y(mantissa_y'left - 1 downto mantissa_y'left - 3);
    
    PR_select <= PR(PR'left downto PR'left-6);        
   	cmp(3) <= '1' when signed(PR(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 3) else '0'; -- -2       
    cmp(2) <= '1' when signed(PR(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 2) and cmp(3) = '0' else '0'; -- -1       
    
    cmp(1) <= '1' when signed(PR(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 1) and cmp(0) = '0' else '0'; -- 1       
    cmp(0) <= '1' when signed(PR(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 0) else '0';  -- 2       
   	
   	carry <= (0 => '1', others => '0') when PR(PR'left) = '0' and sqrt = '0' else (others => '0');

	with cmp select 
        F <= one when "0010",
             two when "0001",
             minus_one when "0100",
             minus_two when "1000",
             (others => '0') when others;            
     
           
    PR_new <= STD_LOGIC_VECTOR((unsigned(PR)) + (unsigned(F)) + carry);
    PR_mul <= PR_new(PR_new'left-2 downto 0) & "00";                
     
    ONE_BIT_NORMALIZATION : process (all)
	begin
		exponent_div_final <= unsigned(exponent_div(E-1 downto 0));
		mantissa <= unsigned(q(q'left-1 downto q'left - M+1));
        if q(q'left) = '0' or ( sqrt = '1' and exp_odd = '0') then
		    exponent_div_final <= exponent_div_minus_one;
            mantissa <= unsigned(q(q'left-2 downto q'left - M));
        end if;
	end process;

    inexact <= or round_sticky;

    round_sticky <= not PR(PR'left) & (or PR(PR'left-1 downto 0));
        
    result_o.fflags <= "0000" & inexact when special_case = '0' else
                        invalid & div_by_zero & "000";
    
    exp <= exponent_div_final when div = '1' else exponent_sqrt_reg;
   
    ROUNDING : rounder generic map(P - 1) port map(exp & mantissa, sign_reg, rm_i, round_sticky, mantissa_final);
    
    result_o.value <= special_value when special_case = '1' else (63 downto P - 1 => sign_reg) & mantissa_final;
    result_o.valid <= '1' when state = FINALIZE or invalid = '1' else '0';

end behavioral;