library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Divider is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		fp_precision_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		result_o : out STD_LOGIC_VECTOR (63 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		fp_valid_o : out STD_LOGIC);
end FP_Divider;

architecture behavioral of FP_Divider is

	signal result_sp, result_dp : STD_LOGIC_VECTOR (63 downto 0);
	signal fflags_sp, fflags_dp : STD_LOGIC_VECTOR (4 downto 0);
	signal enable, valid : STD_LOGIC_VECTOR (1 downto 0);

	component FP_Divider_component is
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
	end component FP_Divider_component;

begin

	FP_DIV_SP : FP_Divider_component
	generic map(P => 32, E => 8, M => 24)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable(0),
		fp_op_i => fp_op_i,
		rm_i => rm_i,
		x_i => x_i(31 downto 0),
		y_i => Y_i(31 downto 0),
		z_o => result_sp,
		fflags_o => fflags_sp,
		div_valid_o => valid(0)
	);

	--    FP_DIV_DP: FP_Divider_component
	--    generic map ( P => 64, M => 53, E => 11 )
	--    port map (
	--        clk_i => clk_i,
	--        rst_i => rst_i,
	--        enable_i => enable(1),
	--        rm_i => rm_i,
	--        x_i => X_i,
	--        y_i => Y_i,
	--        z_o => result_dp,
	--        fflags_o => fflags_dp,
	--        div_valid_o => valid_dp
	--    );

	enable(0) <= '1' when enable_i = '1' and valid(0) = '0' and fp_precision_i = '0' else '0';
	enable(1) <= '1' when enable_i = '1' and valid(1) = '0' and fp_precision_i = '1' else '0';

	result_o <= result_sp when fp_precision_i = '0' else result_dp;
	fflags_o <= fflags_sp when fp_precision_i = '0' else fflags_dp;
	fp_valid_o <= or valid;

end behavioral;