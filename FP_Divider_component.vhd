library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Divider_component is
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
		z_o : out STD_LOGIC_VECTOR (63 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		div_valid_o : out STD_LOGIC);
end FP_Divider_component;

architecture behavioral of FP_Divider_component is

	constant BIAS : signed(E downto 0) := to_signed(2 ** (E - 1) - 1, E + 1);
	constant BIAS_DIV_2 : signed(E downto 0) := to_signed(2 ** (E - 2) - 1, E + 1);
	constant INFINITY : STD_LOGIC_VECTOR (P - 2 downto 0) := (P - 2 downto P - E - 1 => '1', others => '0');

	signal sign, div, sqrt, fp_valid, inexact, start_div, div_by_zero, sign_d : STD_LOGIC;
	signal invalid, invalid_div, invalid_sqrt : STD_LOGIC;
	signal mantissa_x, mantissa_y : STD_LOGIC_VECTOR (M - 1 downto 0);
	signal exponent_div, exponent_sqrt, exponent_x, exponent_y : signed(E downto 0);

	alias exp_odd : STD_LOGIC is x_i(P - E - 1);
	alias sign_x : STD_LOGIC is x_i(P - 1);
	alias sign_y : STD_LOGIC is y_i(P - 1);

	signal q, q_comp, qm, k : STD_LOGIC_VECTOR(M + 5 downto 0);
	signal y : STD_LOGIC_VECTOR(M - 1 downto 0);
	signal PR, PR_init, PR_mul, PR_new, F : STD_LOGIC_VECTOR (q'left + 2 downto 0);

	signal counter : unsigned (num_bits(M/2) + 1 downto 0);
	signal special_value : STD_LOGIC_VECTOR (63 downto 0);

	signal square_estimate, estimate, q_digit : STD_LOGIC_VECTOR (2 downto 0);

	signal a, b, round_sticky : STD_LOGIC_VECTOR (1 downto 0);
	signal cmp : STD_LOGIC_VECTOR (3 downto 0);
	signal append_one, append_two, minus_one, minus_two, one, two : STD_LOGIC_VECTOR(q'left + 2 downto 0);
	signal overflow, underflow, lda, ldb, special_case : STD_LOGIC;
	signal position : NATURAL range q'left downto 0;
	signal carry : unsigned (q'left + 2 downto 0);
	signal val : unsigned (P - 2 downto 0);
	signal mantissa : STD_LOGIC_VECTOR (P - 2 downto 0);

	type state_type is (IDLE, DIVIDE, FINALIZE);
	signal state, next_state : state_type;
  
    type selection_constants_t is array(0 to 7, 0 to 3) of signed(6 downto 0);
    constant SEL_CONSTANTS : selection_constants_t := ( 
        ( to_signed(12, 7), to_signed(4, 7), to_signed(-4, 7), to_signed(-13, 7) ),
        ( to_signed(14, 7), to_signed(4, 7), to_signed(-5, 7), to_signed(-15, 7) ),
        ( to_signed(15, 7), to_signed(4, 7), to_signed(-6, 7), to_signed(-16, 7) ),
        ( to_signed(16, 7), to_signed(4, 7), to_signed(-6, 7), to_signed(-17, 7) ),
        ( to_signed(17, 7), to_signed(6, 7), to_signed(-6, 7), to_signed(-17, 7) ),
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
            when IDLE => if start_div = '1' then
                next_state <= DIVIDE;
        end if;
        when DIVIDE => if counter = 0 then
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
    
    exponent_div <= exponent_x - exponent_y + BIAS + 2 - ((E downto 1 => '0') & fp_info_x.normal) -
    ((E downto 1 => '0') & fp_info_y.normal);
    exponent_sqrt <= ('0' & exponent_x(E - 1 downto 1)) + BIAS_DIV_2 + exp_odd;
    
    sign_d <= sign_x xor sign_y;
    sign <= sign_x when sqrt = '1' else sign_d;
    
    div <= '1' when fp_op_i = FPU_DIV else '0';
    sqrt <= '1' when fp_op_i = FPU_SQRT else '0';
    
    invalid_div <= (fp_info_y.nan or (fp_info_x.zero and fp_info_y.inf) or (fp_info_x.inf and fp_info_y.zero)) and div;
    invalid_sqrt <= (not fp_info_x.zero and sign_x) and sqrt;
    carry <= (0 => '1', others => '0') when sqrt = '0' and PR(PR'left) = '0' else (others => '0');
    
    invalid <= fp_info_x.nan or invalid_div or invalid_sqrt;
    div_by_zero <= fp_info_y.zero and div;
    special_case <= (invalid or div_by_zero) and enable_i;
    
    SPECIAL_CASES : process (div_by_zero, sign_d)
    begin
        -- Canonical NaN
        special_value <= (P - 2 downto P - E => '1', others => '0');
        if div_by_zero = '1' then
            special_value <= (63 downto P - 1 => sign_d) & INFINITY;
        end if;
    end process;
    
    PR_init <= "11" & mantissa_x & (PR'left - mantissa_x'length - 2 downto 0 => '0') when sqrt = '1' else
    "00" & mantissa_x & (PR'left - mantissa_x'length - 2 downto 0 => '0');
    start_div <= '1' when enable_i = '1' and state = IDLE and invalid = '0' else '0';
    
    DIVISION : process (clk_i)
    begin
        if rising_edge(clk_i) then
            case state is
                when IDLE =>
                    if enable_i = '1' then
                        q <= (q'left => '1', others => '0');
                        qm <= (others => '0');
                        position <= q'left - 1;
                        append_one <= (append_one'left - 3 downto append_one'left - 5 => '1', others => '0');
                        append_two <= (append_two'left - 2 downto append_two'left - 3 => '1', others => '0');
                        y <= mantissa_y;
                        PR <= PR_init;
                        k <= (k'left => '1', others => '1');
                        counter <= to_unsigned(M/2 + 1, counter'length);
                    end if;
                when DIVIDE =>
                    PR <= PR_mul;
                    counter <= counter - 1;
                    append_one <= "00" & append_one(append_one'left downto 2);
                    append_two <= "00" & append_two(append_two'left downto 2);
                    position <= position - 2;
                    k <= "11" & k(k'left downto 2);
    
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
    ldb <= not PR(PR'left) and (or q_digit);
    
    div_valid_o <= '1' when state = FINALIZE else '0';
    
    with q_digit select a <=
    q_digit(1 downto 0) when "000" | "001" | "010",
    "11" when "101",
    "10" when "110",
    "00" when others;
    
    --     with q_digit select a <=
    --         "00" when "0000",
    --         "10" when "0001",
    --         "11" when "0010",
    --         "01" when "0100",
    --         "10" when "1000",
    --         "--" when others;
    
    with a select b <=
    "11" when "00",
    "00" when "01",
    "01" when "10",
    "10" when "11",
    "00" when others;
    
    q_comp <= (not q) and k;
    
    one <= "111" & (not y) & "00000" when sqrt = '0' else ("11" & q_comp) or append_one;
    two <= "11" & (not y) & "1" & "00000" when sqrt = '0' else ("1" & q_comp & "1") or append_two;
    minus_one <= "000" & y & "00000" when sqrt = '0' else ("00" & qm) or append_one;
    minus_two <= "00" & y & "0" & "00000" when sqrt = '0' else ("0" & qm & "0") or append_two;
    
    with q_digit select F <=
    one when "001",
    two when "010",
    minus_one when "101",
    minus_two when "110",
    (others => '0') when others;
    
    --       with q_digit select F <=
    --          minus_two when "0001",
    --          minus_one when "0010",
    --          one when "0100",
    --          two when "1000",
    --         (others => '0') when others;
    square_estimate <= "100" when counter = (M/2 + 1) else
    "111" when q(q'left) = '1' else
    q(q'left - 2 downto q'left - 4);
    
    estimate <= square_estimate when sqrt = '1' else y(y'left - 1 downto y'left - 3);
    PR_new <= STD_LOGIC_VECTOR(unsigned(PR) + unsigned(F) + carry);
    PR_mul <= PR_new(PR_new'left - 2 downto 0) & "00";
    
    --    cmp(0) <= '1' when signed(P(P'left downto P'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 0) else '0';        
    --    cmp(1) <= '1' when signed(P(P'left downto P'left-6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), 1) else '0';        
    
    --    cmp(2) <= '1' when signed(P(P'left downto P'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 2) else '0';        
    --    cmp(3) <= '1' when signed(P(P'left downto P'left-6)) < SEL_CONSTANTS(to_integer(unsigned(estimate)), 3) else '0';        
    
    COMPARATORS : for i in 0 to 3 generate
        cmp(i) <= '1' when signed(PR(PR'left downto PR'left - 6)) >= SEL_CONSTANTS(to_integer(unsigned(estimate)), i) else '0';
    end generate;
    
    --    DIGIT_SELECT: process(cmp)
    --    begin
    --        case cmp is
    --            when "0000" => q_digit <= "000";
    --            when "0001" => q_digit <= "110";
    --            when "0010" => q_digit <= "101";
    --            when "0100" => q_digit <= "001";
    --            when "1000" => q_digit <= "110";
    --            when others => q_digit <= "---";
    --        end case;
    --    end process;   
    
    DIGIT_SELECT : process (cmp)
    begin
        if cmp(0) = '1' then
            q_digit <= "010";
            elsif cmp(1) = '1' then
            q_digit <= "001";
            elsif cmp(2) = '1' then
            q_digit <= "000";
            elsif cmp(3) = '1' then
            q_digit <= "101";
            else q_digit <= "110";
        end if;
    end process;
    
    val <= unsigned(exponent_div(E - 1 downto 0)) & unsigned(q(q'left - 2 downto q'left - M));
    round_sticky <= q(M - 1) & (or PR);
    inexact <= or round_sticky;
    
    fflags_o <= "0000" & inexact when special_case = '0' else
    invalid & div_by_zero & "000";
    
    ROUNDING : rounder generic map(P - 1) port map(val, sign, rm_i, round_sticky, mantissa);
    
    z_o <= special_value when special_case = '1' else (63 downto P - 1 => sign) & mantissa;
    div_valid_o <= '1' when state = FINALIZE else '0';
    
end behavioral;
