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
	constant BIAS_DIV_2 : signed(E-1 downto 0) := to_signed(2 ** (E - 2) - 1, E);
	constant INFINITY : STD_LOGIC_VECTOR (P - 2 downto 0) := (P - 2 downto P - E - 1 => '1', others => '0');

	signal sign, div, div_reg, sqrt, sqrt_reg, inexact, inexact_reg, start_div, finish, finish_reg, div_by_zero, sign_d : STD_LOGIC;
	signal invalid, invalid_div, invalid_sqrt, special_case : STD_LOGIC;
	signal mantissa_x, mantissa_y : STD_LOGIC_VECTOR (M - 1 downto 0);
    signal exponent_div, exponent_x, exponent_y : signed(E downto 0);
    signal exp, exponent_div_reg, exponent_div_final, exponent_div_minus_one, exponent_sqrt, exponent_sqrt_reg : STD_LOGIC_VECTOR (E-1 downto 0);

	alias exp_odd : STD_LOGIC is x_i(P-E-1);
	alias sign_x : STD_LOGIC is x_i(P - 1);
	alias sign_y : STD_LOGIC is y_i(P - 1);

    signal round_up : STD_LOGIC;
   
    signal rounded_digit, rounded_digit_reg : STD_LOGIC_VECTOR (2 downto 0);

    signal sign_rem, norm, norm_reg, digit : signed(2 downto 0);

	signal mantissa : STD_LOGIC_VECTOR(next_even(M) downto 0);	
	signal q, qm, q_next, qm_next, k : STD_LOGIC_VECTOR(next_even(M-2) downto 0) := (others => '0');	
	signal x_reg, y_reg : STD_LOGIC_VECTOR (q'left+2 downto 0);
	signal y, y_init : STD_LOGIC_VECTOR (q'left+2 downto 0);
	signal counter : unsigned (num_bits(M/2-1) downto 0);
	signal special_value : STD_LOGIC_VECTOR (63 downto 0);
    signal mantissa_final : STD_LOGIC_VECTOR (M-2 downto 0);

	signal square_estimate, estimate, first_estimate : STD_LOGIC_VECTOR (2 downto 0);

	signal a, b, a_new, a_init : STD_LOGIC_VECTOR (1 downto 0);
	
	signal cmp, cmp_new, cmp_init : STD_LOGIC_VECTOR (3 downto 0);
        
  	signal minus_one, minus_two, one, two, append_one, append_two, F : STD_LOGIC_VECTOR(q'left+4 downto 0);	
    signal PR, PR_mul, PR_init, PR_sqrt, PR_div, PR_new, PR_select : STD_LOGIC_VECTOR (q'left+4 downto 0);
   
    signal carry : unsigned (q'left+1 downto 0);

	signal overflow, underflow, lda, ldb : STD_LOGIC;
	signal position : NATURAL range q'left downto 0;
    
	type state_type is (IDLE, DIVIDE);
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
    
    NEXT_STATE_DECODE : process (all)
    begin
        next_state <= state;
        case state is
            when IDLE => 
                if start_div = '1' then
                    next_state <= DIVIDE; 
                end if;
            when DIVIDE => 
                if finish_reg = '1' then
                    next_state <= IDLE;
                end if;
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
     
    Y_GEN : if M = 24 generate
        y_init <= "0" & mantissa_y;

        PR_sqrt <= "110" & mantissa_x when exp_odd = '1' else 
                   "11" & mantissa_x & "0";
    else generate
        y_init <= "0" & mantissa_y & "0";
        PR_sqrt <= "110" & mantissa_x & "0" when exp_odd = '1' else 
                   "11" & mantissa_x & "00";
    end generate; 
     
    SPECIAL_CASES : process (all)
    begin
        -- Canonical NaN
		special_value <= (P - 2 downto P - E - 2 => '1', others => '0');
        if fp_info_y.zero and div then
            special_value <= (63 downto P - 1 => sign_x xor sign_y) & INFINITY;
        end if;
    end process;
    
    PR_init <= PR_div when div = '1' else PR_sqrt;                                               

    start_div <= enable_i and not special_case;
    finish <= '1' when counter = 1 else '0';

    DIVISION : process (clk_i)
    begin
        if rising_edge(clk_i) then
            case state is
                when IDLE =>
                    if start_div = '1' then
                        qm <= (others => '0');
                        q <= (q'left => '1', others => '0');
                        exponent_sqrt_reg <= exponent_sqrt;
    --                    exponent_div_reg <= STD_LOGIC_VECTOR(exponent_div(E-1 downto 0));
      --                  exponent_div_minus_one <= STD_LOGIC_VECTOR(exponent_div(E-1 downto 0) - 1);
                        PR <= PR_init;
                        div_reg <= div;
                        cmp <= cmp_init;
                        y <= y_init;
                        k <= (k'left => '1', others => '0');
                        a <= a_init;
                        sqrt_reg <= sqrt;
                        position <= q'left-1;
                        if div then
                            sign <= sign_d;
                        else 
                            sign <= '0';
                        end if;
                       -- if sqrt = '1' then
        --                    append_one <= (append_one'left-3 downto append_one'left-5 => '1', others => '0');
          --                  append_two <= (append_two'left-2 downto append_two'left-3 => '1', others => '0');
                     --   else 
            --                append_one <= (others => '0');
              --              append_two <= (others => '0');
                   --     end if;         
                    end if;
                    counter <= to_unsigned(M/2-1, counter'length);

                when DIVIDE =>
                    a <= a_new;
                    k <= "11" & k(k'left downto 2);
                    counter <= counter - 1;
                    finish_reg <= finish;
                    PR <= PR_mul;
                    cmp <= cmp_new;
                    norm_reg <= norm;
                    rounded_digit_reg <= rounded_digit;
                    inexact_reg <= inexact;
             --           append_one <= "00" & append_one(append_one'left downto 2);
           --             append_two <= "00" & append_two(append_two'left downto 2);
                     position <= position - 2;
                     q <= q_next;
                     qm <= qm_next;
                when others =>
            end case;
        end if;
    end process;

    UPDATE_ROOT: process(all)
    begin
        q_next <= q;
        qm_next <= qm;
        if lda = '1' then
            q_next <= qm;
        end if;
 
        if ldb = '1' then
            qm_next <= q;
        end if;
      
        q_next(position downto position - 1) <= a;
        qm_next(position downto position - 1) <= b;
    end process;
    inexact <= or PR_new(PR_new'left-2 downto 0);
       
    lda <= PR(PR'left);
    ldb <= (not PR(PR'left)) and (or cmp(1 downto 0));
    
    digit <= signed((or cmp_new(3 downto 2)) & a);
    sign_rem <= "00" & ((not PR_mul(PR'left)) and round_up );                             
    norm <= "00" & ( qm_next(qm_next'left) and div_reg) when PR_mul(PR'left) = '1' else   
            "00" & ( q_next(q_next'left) and div_reg);
    
    rounded_digit <= STD_LOGIC_VECTOR(digit + norm + sign_rem);

    a_init <= (cmp_init(0) or cmp_init(2) or cmp_init(3)) & (cmp_init(1) or cmp_init(2));
    a_new <= (cmp_new(0) or cmp_new(2) or cmp_new(3)) & (cmp_new(1) or cmp_new(2));

    with a select
        b <= "11" when "00",
             "00" when "01",
             "01" when "10",
             "10" when "11",
             "--" when others;

    x_reg <= --((not q ) and k) & "00" when sqrt_reg = '1' else 
    not y;     
    y_reg <= --qm & "00" when sqrt_reg = '1' else 
    y;
--    two <= ("1" & x_reg & div_reg); -- or append_two;
--    one <= ("11" & x_reg ); --or append_one;
--    minus_one <= ("00" & y_reg ); -- or append_one;
--    minus_two <= ("0" & y_reg & "0");  --or append_two;

    F(F'left) <= cmp(1) or cmp(0);
    F(F'left-1) <= cmp(1) or ( x_reg(x_reg'left) and cmp(0) ) or ( y_reg(y_reg'left) and cmp(3) );
    
    F_GEN: for i in F'left-2 downto 1 generate
        F(i) <= ( cmp(1) and x_reg(i-1) ) or (cmp(0) and x_reg(i) ) or ( cmp(3) and y_reg(i) ) or (cmp(2) and y_reg(i-1) );  
    end generate;
    
    F(0) <= ( div_reg and cmp(0) ) or (cmp(1) and x_reg(0) ) or (cmp(2) and y_reg(0) ); 
    
    square_estimate <=  "111" when q(q'left) = '1' else
                        q(q'left - 2 downto q'left - 4);
                        
    mantissa <= q & rounded_digit_reg(1 downto 0) when rounded_digit_reg(2) = '0' else
                qm & rounded_digit_reg(1 downto 0);
    
    mantissa_final <= mantissa(mantissa'left-1 downto mantissa'left - M + 1) when norm_reg(0) = '1' else 
                      mantissa(mantissa'left-2 downto mantissa'left - M);     
    
    first_estimate <= "100" when sqrt else y_i(M-2 downto M-4);
    
    estimate <= --square_estimate when sqrt_reg = '1' else
     y_i(M-2 downto M-4);

   	cmp_init(3) <= '1' when signed(PR_init(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(first_estimate)), 3) else '0'; -- -2       
    cmp_init(2) <= '1' when signed(PR_init(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(first_estimate)), 2) and cmp_init(3) = '0' else '0'; -- -1       
    
    cmp_init(1) <= '1' when signed(PR_init(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(first_estimate)), 1) and cmp_init(0) = '0' else '0'; -- 1       
    cmp_init(0) <= '1' when signed(PR_init(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(first_estimate)), 0) else '0';  -- 2       
   	
   	cmp_new(3) <= '1' when signed(PR_mul(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 3) else '0'; -- -2       
    cmp_new(2) <= '1' when signed(PR_mul(PR'left downto PR'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 2) and cmp_new(3) = '0' else '0'; -- -1       
    
    cmp_new(1) <= '1' when signed(PR_mul(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 1) and cmp_new(0) = '0' else '0'; -- 1       
    cmp_new(0) <= '1' when signed(PR_mul(PR'left downto PR'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 0) else '0';  -- 2       
   	
   	--carry <= (0 => '1', others => '0') when PR(PR'left) = '0' and (or cmp) = '1' and sqrt_reg = '0' else (others => '0');

    PR_new <= STD_LOGIC_VECTOR(unsigned(PR) + unsigned(F)); -- + carry);
    PR_mul <= PR_new(PR_new'left-2 downto 0) & "00";                
    
  --  exponent_div_final <= exponent_div_reg when norm_reg(0) = '1' else 
    --                      exponent_div_minus_one; 
    

    invalid <= ( invalid_sqrt and sqrt ) or (invalid_div and div );

    with rm_i select 
        round_up <= '1' when RNE, 
       sign and inexact when RDN,
   not sign and inexact when RUP,
                inexact when RMM,
                    '0' when others;

 --   exp <= exponent_div_final when div_reg = '1' else exponent_sqrt_reg;                                 
    
    result_o.fflags <= invalid & div_by_zero & "000" when special_case = '1' else
                       "0000" & inexact_reg;
                            
    result_o.value <= special_value when special_case = '1' else 
                      (63 downto P - 1 => '0') & exponent_sqrt_reg & mantissa_final;
    
    special_case <= invalid or div_by_zero;
    
    result_o.valid <= finish_reg or special_case;
    

end behavioral;  