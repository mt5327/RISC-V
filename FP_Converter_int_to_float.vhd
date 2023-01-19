library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter_int_to_float is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
        clk_i : in STD_LOGIC;
        rst_i : in STD_LOGIC;
        enable_i : in STD_LOGIC;
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		mode_i : in STD_LOGIC_VECTOR (1 downto 0);
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		result_o : out FP_RESULT);
end FP_Converter_int_to_float;

architecture behavioral of FP_Converter_int_to_float is

	constant BIAS : unsigned(E - 1 downto 0) := to_unsigned(2 ** (E - 1) - 1, E);

	signal int_sign, long_sign, valid : STD_LOGIC := '0';

	signal lz_counter : unsigned(5 downto 0);

    signal result, result_reg : STD_LOGIC_VECTOR (63 downto 0);
    signal fflags, fflags_reg : STD_LOGIC_VECTOR (4 downto 0);
   
	signal int_mantissa, long_mantissa : unsigned(63 downto 0);
	signal exp, exp_init, exp_final : unsigned(E - 1 downto 0);
	signal sign, sign_reg : STD_LOGIC;
	signal round_sticky : STD_LOGIC_VECTOR (1 downto 0);
	signal invalid, overflow, underflow, inexact : STD_LOGIC;
	signal num, is_zero : unsigned(P - 2 downto 0);
	signal rounded_num : STD_LOGIC_VECTOR(P - 2 downto 0);
    constant int_exp_init : unsigned(E-1 downto 0) := to_unsigned(31, E) + BIAS;
    constant long_exp_init : unsigned(E-1 downto 0) := to_unsigned(63, E) + BIAS;
    signal mantissa : unsigned(63 downto 0);
    signal shifted_mantissa : unsigned(63 downto 0);
	alias exp_result : STD_LOGIC_VECTOR (E-1 downto 0) is rounded_num(P - 2 downto P - E - 1);

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

	type state_type is (IDLE, CONVERT, ROUND, FINALIZE);
	signal state, next_state : state_type;

begin

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
    
	int_sign <= x_i(31) and (not mode_i(0));
	long_sign <= x_i(63) and (not mode_i(0));
	
	int_mantissa <= unsigned(-signed(x_i(31 downto 0))) & (31 downto 0 => '0') when int_sign = '1' else unsigned(x_i(31 downto 0)) & (31 downto 0 => '0');
    long_mantissa <= unsigned(-signed(x_i)) when long_sign = '1' else unsigned(x_i);
    
    lz_counter <= leading_zero_counter(mantissa, lz_counter'length);
    
    mantissa <= int_mantissa when mode_i(1) = '0' else long_mantissa; 
    exp_init <= int_exp_init when mode_i(1) = '0' else long_exp_init; 
    sign <= int_sign when mode_i(1) = '0' else long_sign;

    process(clk_i) 
    begin 
        if rising_edge(clk_i) then
           shifted_mantissa <= shift_left(mantissa, to_integer(lz_counter));
           exp <= exp_init - resize(lz_counter, E);
           sign_reg <= sign;
           rm <= rm_i;
         end if;
    end process;
    
    exp_final <= exp when shifted_mantissa(63) = '1' else (others => '0');
    num <= exp_final & shifted_mantissa(62 downto 63-M+1);
    round_sticky <= shifted_mantissa(63-M) & ( or shifted_mantissa(63-M-1 downto 0));

	ROUNDING: rounder generic map (num'length) port map (num, sign_reg, rm_i, round_sticky, rounded_num);

	overflow <= and exp_result;
    underflow <= ( nand exp_result ) and inexact;	
	inexact <= or round_sticky;

    process (clk_i) 
    begin
        if rising_edge(clk_i) then
            result_reg <= result;
            fflags_reg <= fflags;
        end if;
    end process;

	result <= (63 downto P - 1 => sign_reg) & rounded_num; 
	fflags <= "00" & overflow & underflow & inexact;
    valid <= '1' when state = FINALIZE else '0';
    
    result_o <= (result_reg, fflags_reg, valid);
    
end behavioral;