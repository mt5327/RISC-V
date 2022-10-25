library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter_double_to_float is
	port (
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		result_o : out STD_LOGIC_VECTOR (63 downto 0));
end FP_Converter_double_to_float;

architecture behavioral of FP_Converter_double_to_float is

	signal exp_sp : unsigned(7 downto 0);
	signal mantissa_sp : unsigned(22 downto 0);

	signal sticky_bit, overflow, underflow, inexact : STD_LOGIC;

	signal num : unsigned(30 downto 0);
	signal rounded_num : STD_LOGIC_VECTOR(30 downto 0);

	signal fp_class : FP_INFO;

	component FP_Classifier is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
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

begin

	CLASSIFY : FP_Classifier generic map(64, 11, 53) port map(x_i(62 downto 0), fp_class);

	exp_sp <= resize(unsigned(x_i(62 downto 52)) - BIAS_DIFF, 8);
	mantissa_sp <= unsigned(x_i(51 downto 29));
	sticky_bit <= or x_i(27 downto 0);

	num <= exp_sp & mantissa_sp;

	ROUND : rounder generic map(num'length) port map(num, x_i(63), rm_i, x_i(28) & sticky_bit, rounded_num);

	result_o <= (63 downto 31 => x_i(63)) & STD_LOGIC_VECTOR(rounded_num) when fp_class.nan = '0' else
		        (30 downto 22 => '1', others => '0');

	overflow <= (not fp_class.inf) and (and rounded_num(30 downto 23));
	underflow <= or rounded_num(30 downto 23);
	inexact <= x_i(28) or sticky_bit or overflow;

	fflags_o <= "00" & overflow & underflow & inexact when fp_class.nan = '0' else
		         fp_class.signaling_nan & "0000";

end behavioral;