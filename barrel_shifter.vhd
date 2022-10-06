library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.constants.all;

entity barrel_shifter is
	generic (SIZE : NATURAL);
	port (
		op_i : in ALU_OP;
		x_i : in STD_LOGIC_VECTOR (SIZE - 1 downto 0);
		shamt_i : in STD_LOGIC_VECTOR (num_bits(SIZE) - 1 downto 0);
		result_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
end barrel_shifter;

architecture behavioral of barrel_shifter is

	type bs_level_t is array (shamt_i'length downto 0) of STD_LOGIC_VECTOR(SIZE - 1 downto 0);
	signal bs_level : bs_level_t;
	signal result : STD_LOGIC_VECTOR(SIZE - 1 downto 0);
	signal shift_left, pad_bit : STD_LOGIC;

begin

	with op_i select
		shift_left <= '1' when ALU_SLL | ALU_SLLW,
		'0' when others;

	with op_i select
		pad_bit <= x_i(SIZE - 1) when ALU_SRA | ALU_SRAW,
		'0' when others;

	bs_level(shamt_i'length) <= reverse(x_i) when shift_left = '1' else x_i;

	BARREL_SHIFTER : for i in shamt_i'left downto 0 generate
		bs_level(i) <= ((2 ** i) - 1 downto 0 => pad_bit) & bs_level(i + 1)(SIZE - 1 downto 2 ** i) when shamt_i(i) = '1' else bs_level(i + 1);
	end generate;

	result_o <= reverse(bs_level(0)) when shift_left = '1' else bs_level(0);

end behavioral;