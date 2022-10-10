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
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		mode_i : in STD_LOGIC_VECTOR (1 downto 0);
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		result_o : out STD_LOGIC_VECTOR (63 downto 0));
end FP_Converter_int_to_float;

architecture behavioral of FP_Converter_int_to_float is

	constant BIAS : unsigned(E - 1 downto 0) := to_unsigned(2 ** (E - 1) - 1, E);

	signal int_sign, long_sign : STD_LOGIC;

	signal int_lz_counter : unsigned(4 downto 0);
	signal long_lz_counter : unsigned(5 downto 0);

	signal int_mantissa, shifted_int_mantissa : unsigned(31 downto 0);
	signal x, long_mantissa, shifted_long_mantissa : unsigned (63 downto 0);
	signal int_exp, long_exp : unsigned(E - 1 downto 0);

	signal sign, sticky_bit, round_bit : STD_LOGIC;
	signal invalid, overflow, underflow, inexact : STD_LOGIC;
	signal num : unsigned(P - 2 downto 0);
	signal rounded_num : STD_LOGIC_VECTOR(P - 2 downto 0);

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

	int_sign <= x_i(31) and (not mode_i(0));
	long_sign <= x_i(63) and (not mode_i(0));

	int_mantissa <= unsigned(-signed(x_i(31 downto 0))) when int_sign = '1' else unsigned(x_i(31 downto 0));
	long_mantissa <= unsigned(-signed(x_i)) when long_sign = '1' else unsigned(x_i);

	int_lz_counter <= leading_zero_counter(int_mantissa, int_lz_counter'length);
	long_lz_counter <= leading_zero_counter(long_mantissa, long_lz_counter'length);

	int_exp <= to_unsigned(31, E) + BIAS - resize(int_lz_counter, E);
	long_exp <= to_unsigned(63, E) + BIAS - resize(long_lz_counter, E);

	shifted_int_mantissa <= shift_left(int_mantissa, to_integer(int_lz_counter));
	shifted_long_mantissa <= shift_left(long_mantissa, to_integer(long_lz_counter));

	SINGLE_PRECISION : if P = 32 generate
		num <= int_exp & shifted_int_mantissa(30 downto 8) when mode_i(1) = '0' else
		long_exp & shifted_long_mantissa(62 downto 40);

		round_bit <= shifted_int_mantissa(7) when mode_i(1) = '0' else
		shifted_long_mantissa(39);

		sticky_bit <= or shifted_int_mantissa(6 downto 0) when mode_i(1) = '0' else
		or shifted_long_mantissa(38 downto 0);
	end generate;

	DOUBLE_PRECISION : if P = 64 generate
		num <= int_exp & shifted_int_mantissa(30 downto 0) & (20 downto 0 => '0') when mode_i(1) = '0' else
		long_exp & shifted_long_mantissa(62 downto 11);
		round_bit <= shifted_long_mantissa(10) and mode_i(1);
		sticky_bit <= (or shifted_long_mantissa(9 downto 0)) and mode_i(1);
	end generate;

	--ROUNDING: rounder generic map (num'length) port map (num, sign, rm_i, round_bit & sticky_bit, rounded_num);

	sign <= int_sign when mode_i(1) = '0' else long_sign;

	rounded_num <= STD_LOGIC_VECTOR(num);
	result_o <= (63 downto P - 1 => sign) & rounded_num;

	invalid <= and rounded_num(P - 2 downto P - E - 1);
	overflow <= and rounded_num(P - 2 downto P - E - 1);
	underflow <= (or rounded_num(P - 2 downto P - E - 1)) and inexact;
	inexact <= round_bit or sticky_bit;

	fflags_o <= invalid & '0' & overflow & underflow & inexact;

end behavioral;