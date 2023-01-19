library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter_float_to_float is
	generic (
		SRC_P : NATURAL;
		SRC_E : NATURAL;
		SRC_M : NATURAL);	
	port (
        clk_i : in STD_LOGIC;
        rst_i : in STD_LOGIC;
        enable_i : in STD_LOGIC; 
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		result_o : out FP_RESULT);
end FP_Converter_float_to_float;

architecture behavioral of FP_Converter_float_to_float is

    constant DST_FP_FORMAT : FP_FORMAT := dst_fp_format(SRC_P);

    alias DST_P : NATURAL is DST_FP_FORMAT.P;
    alias DST_E : NATURAL is DST_FP_FORMAT.E;
    alias DST_M : NATURAL is DST_FP_FORMAT.M;

    constant SRC_BIAS : signed(11 downto 0) := to_signed(2**(SRC_E-1)-1, 12);
    constant DST_BIAS : signed(11 downto 0) := to_signed(2**(DST_E-1)-1, 12);
    constant MAX_EXP : signed(11 downto 0) := to_signed(2**DST_E-1, 12);

	signal exp, exp_final : signed(11 downto 0);
	signal x_mantissa, mantissa_norm : unsigned(SRC_M-1 downto 0);
	signal mantissa, mantissa_shifted : unsigned(2 * 52 downto 0);

	signal special_case, overflow, underflow, inexact, valid : STD_LOGIC;
	signal round_sticky, round_sticky_reg : STD_LOGIC_VECTOR (1 downto 0);

    signal special_value : STD_LOGIC_VECTOR (63 downto 0);

    signal mantissa_lzc, denorm_shamt : unsigned(5 downto 0);

    signal num : unsigned (DST_P-2 downto 0);
    signal rounded_num : STD_LOGIC_VECTOR (DST_P-2 downto 0);
    signal result, result_reg : STD_LOGIC_VECTOR (63 downto 0);
    signal fflags, fflags_reg : STD_LOGIC_VECTOR (4 downto 0);

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
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            fp_class_o : out FP_INFO);
    end component FP_Classifier;

    signal fp_class : FP_INFO;
	
    type state_type is (IDLE, CONVERT, ROUND, FINALIZE);
	signal state, next_state : state_type;
	
begin

    CLASSIFY : FP_Classifier generic map(SRC_P, SRC_E, SRC_M) port map(x_i, fp_class);

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
                    next_state <= CONVERT;
                end if;
            when CONVERT => next_state <= ROUND; 
            when ROUND => next_state <= FINALIZE;
            when FINALIZE => next_state <= IDLE;
            when others => next_state <= IDLE;
        end case;
    end process;

    exp <= resize(signed(x_i(SRC_P-2 downto SRC_P - SRC_E - 1)), exp'length) + ((11 downto 1 => '0') & fp_class.subnormal) - SRC_BIAS -
           signed(resize(("0" & mantissa_lzc), exp'length)) + DST_BIAS;
    
    x_mantissa <= fp_class.normal & unsigned(x_i(SRC_M-2 downto 0));    
  
    mantissa_lzc <= leading_zero_counter(x_mantissa, mantissa_lzc'length);
 
    mantissa_norm <= shift_left(x_mantissa, to_integer(mantissa_lzc));
    mantissa <= mantissa_norm & (mantissa'length - mantissa_norm'length - 1 downto 0 => '0');
   
    special_value <= (63 downto DST_P-1 => x_i(SRC_P-1), others => '0') when fp_class.zero = '1' else
                      (DST_P-2 downto DST_P-DST_E-2 => '1', others => '0'); 
  
    MANTISSA_AND_EXPONENT_ADJUSTMENT: process (exp, fp_class.inf)
    begin
        exp_final <= exp;
        denorm_shamt <= to_unsigned(0, denorm_shamt'length);  
        if exp > MAX_EXP or fp_class.inf = '1' then
            exp_final <= MAX_EXP;
        elsif exp < 1 then
            exp_final <= (others => '0');
            denorm_shamt <= to_unsigned(1 - to_integer(exp), denorm_shamt'length); 
        end if;          
    end process;
  
    mantissa_shifted <= shift_right(mantissa, to_integer(denorm_shamt));
    round_sticky <= mantissa_shifted(53) & (or mantissa_shifted(52 downto 0));

    
    process (clk_i) 
    begin
        if rising_edge(clk_i) then
            num <= unsigned(exp_final(DST_E-1 downto 0)) & mantissa_shifted(mantissa_shifted'left-1 downto mantissa_shifted'left-(DST_M-1));
            round_sticky_reg <= round_sticky;
            special_case <= fp_class.zero or fp_class.nan;
        end if;
    end process;    
      
    ROUNDING : rounder generic map(DST_P-1) port map(num, x_i(SRC_P-1), rm_i, round_sticky_reg, rounded_num);

    overflow <= (not fp_class.inf) and (and rounded_num(DST_P-2 downto DST_E-1));
    underflow <= nor rounded_num(DST_P-2 downto DST_P-DST_E-1);
    inexact <= ( or round_sticky ) or overflow;
    
    fflags <= "10000" when special_case = '1' else "00" & overflow & underflow & inexact;
    valid <= '1' when state = FINALIZE else '0';
    
    RESULT_GEN: if SRC_P = 64 generate
        result <= special_value when special_case = '1' else (63 downto 32 => '1') & x_i(63) & rounded_num;
    else generate
        result <= special_value when special_case = '1' else x_i(31) & rounded_num;
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