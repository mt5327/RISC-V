library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter_float_to_float is
	generic (
		SRC_P : NATURAL;
		SRC_E : NATURAL;
		SRC_M : NATURAL;
		DST_P : NATURAL;
		DST_E : NATURAL;
		DST_M : NATURAL);	
	port (
        clk_i : in STD_LOGIC;
        rst_i : in STD_LOGIC;
        enable_i : in STD_LOGIC; 
		x_i : in STD_LOGIC_VECTOR (SRC_P-1 downto 0);
		is_boxed_i : in STD_LOGIC;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		result_o : out FP_RESULT);
end FP_Converter_float_to_float;

architecture behavioral of FP_Converter_float_to_float is

    constant SRC_BIAS : signed(11 downto 0) := to_signed(2**(SRC_E-1)-1, 12);
    constant DST_BIAS : signed(11 downto 0) := to_signed(2**(DST_E-1)-1, 12);
    constant MAX_EXP : signed(11 downto 0) := to_signed(2**DST_E-1, 12);

	signal exp_reg, exp : signed(11 downto 0);
	signal exp_final, exp_final_reg : signed(DST_E-1 downto 0);
	signal x_mantissa, x_mantissa_reg, mantissa_norm : unsigned(SRC_M-1 downto 0);
	signal mantissa, mantissa_reg, mantissa_shifted, mantissa_shifted_reg : unsigned(2 * 52 downto 0);
 
	signal sign, special_case, special_case_reg, overflow, underflow, inexact, valid : STD_LOGIC;
	signal round_sticky: STD_LOGIC_VECTOR (1 downto 0);
    
    signal exp_underflow : STD_LOGIC;
    
    signal special_value, special_value_reg : STD_LOGIC_VECTOR (DST_P-1 downto 0);
    
    signal mantissa_lzc, mantissa_lzc_reg : unsigned(num_bits(SRC_M) downto 0);
    signal denorm_shamt, denorm_shamt_underflow, denorm_shamt_reg : unsigned(5 downto 0);
    signal signed_mantissa_lzc : signed(mantissa_lzc'length downto 0);

    signal num : unsigned (DST_P-2 downto 0);
    signal rounded_num : STD_LOGIC_VECTOR (DST_P-2 downto 0);
    signal result, result_reg : STD_LOGIC_VECTOR (63 downto 0);
    signal fflags, fflags_reg : STD_LOGIC_VECTOR (4 downto 0);
    signal rm : STD_LOGIC_VECTOR (2 downto 0);

	component rounder is
		generic (SIZE : NATURAL);
		port (
			x_i : in unsigned (SIZE - 1 downto 0);
			sign_i : in STD_LOGIC;
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
			z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
	end component rounder;
	
	component FP_Classifier is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            x_i : in STD_LOGIC_VECTOR (P-2 downto 0);
            is_boxed_i : in STD_LOGIC;
            fp_class_o : out FP_INFO);
    end component FP_Classifier;

    signal fp_class : FP_INFO;
	
    type state_type is (IDLE, CONVERT, ROUND, FINALIZE);
	signal state, next_state : state_type;
	
begin

    CLASSIFY : FP_Classifier generic map(SRC_P, SRC_E, SRC_M) port map(x_i(SRC_P-2 downto 0), is_boxed_i, fp_class);

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

    NEXT_STATE_DECODE : process (enable_i, state)
    begin
        next_state <= state;
        case state is
            when IDLE => 
                if enable_i = '1' then
                    next_state <= CONVERT;
                end if;
            when CONVERT => next_state <= ROUND; 
            when ROUND => next_state <= FINALIZE;
            when FINALIZE => next_state <= IDLE;
            when others => next_state <= IDLE;
        end case;
    end process;
     
    mantissa_lzc <= leading_zero_counter(x_mantissa, mantissa_lzc'length) when fp_class.subnormal else (others => '0');
    signed_mantissa_lzc <= '0' & signed(mantissa_lzc);
    exp <= resize(signed(x_i(SRC_P-2 downto SRC_P - SRC_E - 1)), exp'length) - resize(signed_mantissa_lzc, exp'length) + ((11 downto 1 => '0') & fp_class.subnormal) - SRC_BIAS + DST_BIAS;   
    x_mantissa <= fp_class.normal & unsigned(x_i(SRC_M-2 downto 0));    
    special_case <= fp_class.zero or fp_class.nan;

    process (clk_i) 
    begin
        if rising_edge(clk_i) then
            x_mantissa_reg <= x_mantissa;
            mantissa_lzc_reg <= mantissa_lzc;
            special_case_reg <= special_case;
            exp_reg <= exp;
        end if;
    end process;

    exp_underflow <= '1' when exp_reg < 1 else '0';
    mantissa_norm <= shift_left(x_mantissa_reg, to_integer(mantissa_lzc_reg));
    mantissa_reg <= mantissa_norm & (mantissa'length - mantissa_norm'length - 1 downto 0 => '0');
   
    special_value <= (DST_P-1 => x_i(SRC_P-1), others => '0') when fp_class.zero = '1' else
                     (DST_P-2 downto DST_P-DST_E-2 => '1', others => '0'); 
    
    denorm_shamt_underflow <= unsigned(resize(exp_reg+1, denorm_shamt'length));
    
    DENORM_SHIFT_AMOUNT: process(exp_underflow, denorm_shamt_underflow)
    begin
        denorm_shamt <= (others => '0');
        if exp_underflow = '1' then
            denorm_shamt <= denorm_shamt_underflow;
        end if;
    end process;

    mantissa_shifted <= shift_right(mantissa_reg, to_integer(denorm_shamt));

    process (clk_i)
    begin
        if rising_edge(clk_i) then
            mantissa_shifted_reg <= mantissa_shifted;
            special_value_reg <= special_value;
            sign <= x_i(SRC_P-1);
            rm <= rm_i;
        end if;
    end process;
 
    exp_final_reg <= exp_reg(DST_E-1 downto 0) when exp_underflow = '0' else (others => '0');
    
    
    round_sticky <= mantissa_shifted_reg(53) & (or mantissa_shifted_reg(52 downto 0));    
    
    num <= unsigned(exp_final_reg) & mantissa_shifted_reg(mantissa_shifted'left-1 downto mantissa_shifted'left-(DST_M-1));
     
    ROUNDING : rounder generic map(DST_P-1) port map(num, sign, rm, round_sticky, rounded_num);

    overflow <= (and rounded_num(DST_P-2 downto DST_P-DST_E-1));
    underflow <= nor rounded_num(DST_P-2 downto DST_P-DST_E-1);
    inexact <= ( or round_sticky ) or overflow;
    
    fflags <= "10000" when special_case_reg = '1' else 
              "00" & overflow & underflow & inexact;
    valid <= '1' when state = FINALIZE else '0';
    
    RESULT_GEN: if SRC_P = 64 generate
        result <= (63 downto 32 => '1') & special_value_reg when special_case_reg = '1' else (63 downto 32 => '1') & sign & rounded_num;
    else generate
        result <= special_value_reg when special_case_reg = '1' else sign & rounded_num;
    end generate;
    
    process (clk_i)  
    begin
        if rising_edge(clk_i) then
            result_reg <= result;
            fflags_reg <= fflags;
        end if;
    end process; 
    
    result_o <= (result_reg, fflags_reg, valid);
    
end behavioral;