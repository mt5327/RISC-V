library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FMA is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		fp_precision_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		z_i : in STD_LOGIC_VECTOR (63 downto 0);
		result_o : out FP_RESULT);
end FMA;

architecture behavioral of FMA is

	signal enable, valid : STD_LOGIC_VECTOR := "00";
	signal result_sp : STD_LOGIC_VECTOR (31 downto 0);
	signal result_dp : STD_LOGIC_VECTOR (63 downto 0);
	signal fflags_sp, fflags_dp : STD_LOGIC_VECTOR (4 downto 0);

	component FMA_component is
		generic (
			P : NATURAL;
			M : NATURAL;
			E : NATURAL);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			enable_i : in STD_LOGIC;
			fp_op_i : in FPU_OP;
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
			y_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
			z_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
			result_o : out STD_LOGIC_VECTOR (P - 1 downto 0);
			fp_valid_o : out STD_LOGIC;
			fflags_o : out STD_LOGIC_VECTOR (4 downto 0));
	end component FMA_component;

begin

	FMA_SP : FMA_component
	generic map(P => 32, E => 8, M => 24)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable(0),
		fp_op_i => fp_op_i,
		rm_i => rm_i,
		x_i => x_i(31 downto 0),
		y_i => y_i(31 downto 0),
		z_i => z_i(31 downto 0),
		result_o => result_sp,
		fp_valid_o => valid(0),
		fflags_o => fflags_sp
	);

	FMA_DP : FMA_component
	generic map(P => 64, E => 11, M => 53)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable(1),
		fp_op_i => fp_op_i,
		rm_i => rm_i,
		x_i => x_i,
		y_i => y_i,
		z_i => z_i,
		result_o => result_dp,
		fp_valid_o => valid(1),
		fflags_o => fflags_dp
	);
	
	

	enable(0) <= '1' when enable_i = '1' and fp_precision_i = '0' and valid(0) = '0' else '0';
	enable(1) <= '1' when enable_i = '1' and fp_precision_i = '1' and valid(1) = '0' else '0';

	result_o.value <= (63 downto 32 => result_sp(31)) & result_sp when fp_precision_i = '0' else result_dp;
	result_o.fflags <= fflags_sp when fp_precision_i = '0' else fflags_dp;

	result_o.valid <= or valid;

end behavioral;