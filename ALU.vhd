library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity ALU is
	port ( 
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		z_o : out STD_LOGIC_VECTOR (63 downto 0);
		op_i : in ALU_OP);
end ALU;

architecture behavioral of ALU is

	component barrel_shifter is
		generic (SIZE : NATURAL);
		port (
			op_i : in ALU_OP;
			x_i : in STD_LOGIC_VECTOR (SIZE - 1 downto 0);
			shamt_i : in STD_LOGIC_VECTOR (num_bits(SIZE) - 1 downto 0);
			result_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
	end component barrel_shifter;

	signal add_sub_r : STD_LOGIC_VECTOR (64 downto 0);
	signal lt_r : STD_LOGIC;
	signal xor_r, or_r, and_r : STD_LOGIC_VECTOR (63 downto 0);
	signal shift32_r : STD_LOGIC_VECTOR (31 downto 0);
	signal shift64_r : STD_LOGIC_VECTOR (63 downto 0);
	signal arithmetic_shift, shift_left, is_sub, is_signed, lt : STD_LOGIC;
	signal x, y : STD_LOGIC_VECTOR (64 downto 0);

begin

	BS_32 : barrel_shifter
	generic map(SIZE => 32)
	port map(
		op_i => op_i,
		x_i => x_i(31 downto 0),
		shamt_i => y_i(4 downto 0),
		result_o => shift32_r
	);

	BS_64 : barrel_shifter
	generic map(SIZE => 64)
	port map(
		op_i => op_i,
		x_i => x_i,
		shamt_i => y_i(5 downto 0),
		result_o => shift64_r
	);

	with op_i select
		is_sub <= '1' when ALU_SUB | ALU_SUBW,
		          '0' when others;

    is_signed <= '1' when op_i = ALU_SLT else '0';
 
	x <= x_i & '1';
	y <= (y_i & '0') xor (64 downto 0 => is_sub);
	
	add_sub_r <= STD_LOGIC_VECTOR(unsigned(x) + unsigned(y));

	xor_r <= x_i xor y_i;
	or_r <= x_i or y_i;
	and_r <= x_i and y_i;

	lt_r <= '1' when signed((x_i(63) and is_signed) & x_i) < signed((y_i(63) and is_signed) & y_i) else '0';

	MUX_OUTPUT : process (add_sub_r, lt_r, xor_r, or_r, and_r, shift64_r, shift32_r, op_i)
	begin
		case op_i is
			when ALU_ADD | ALU_SUB => z_o <= add_sub_r(64 downto 1);
			when ALU_ADDW | ALU_SUBW => z_o <= (63 downto 32 => add_sub_r(32)) & add_sub_r(32 downto 1);
			when ALU_SLT | ALU_SLTU => z_o <= (0 => lt_r, others => '0');
			when ALU_XOR => z_o <= xor_r;
			when ALU_OR => z_o <= or_r;
			when ALU_AND => z_o <= and_r;
			when ALU_SLL | ALU_SRL | ALU_SRA => z_o <= shift64_r;
			when ALU_SLLW | ALU_SRLW | ALU_SRAW => z_o <= (63 downto 32 => shift32_r(31)) & shift32_r;
			when others => z_o <= (others => '0');
		end case;
	end process;

end behavioral;