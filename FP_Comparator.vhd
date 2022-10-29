library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Comparator is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
		x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		y_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		fp_op_i : in FPU_OP;
		funct3_i : in STD_LOGIC_VECTOR (2 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		result_o : out STD_LOGIC_VECTOR(P - 1 downto 0));
end FP_Comparator;

architecture behavioral of FP_Comparator is

	signal cmp, lt, eq, nan, signaling_nan : STD_LOGIC;

	signal fflags : STD_LOGIC_VECTOR (4 downto 0);

	signal fp_infos : fp_infos_t(0 to 1);

	component FP_Classifier is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
			fp_class_o : out FP_INFO);
	end component FP_Classifier;

	signal invalid : STD_LOGIC;

	signal min_max_result, fp_min, fp_max : STD_LOGIC_VECTOR (P - 1 downto 0);

begin

	FP_CLASS_X : FP_Classifier generic map(P, E, M) port map(x_i, fp_infos(0));
	FP_CLASS_Y : FP_Classifier generic map(P, E, M) port map(y_i, fp_infos(1));

	lt <= '1' when unsigned(x_i) < unsigned(y_i) else '0';
	eq <= '1' when (unsigned(x_i) = unsigned(y_i)) or (fp_infos(0).zero = '1' and fp_infos(0).zero = '1') else '0';
	fp_min <= x_i when lt = '1' and fp_infos(0).nan = '0' else y_i;
	fp_max <= x_i when lt = '0' and fp_infos(0).nan = '0' else y_i;

	invalid <= fp_infos(0).signaling_nan or fp_infos(1).signaling_nan;

	MIN_MAX : process (fp_min, fp_max, funct3_i, fp_infos)
	begin
		if fp_infos(0).nan = '1' and fp_infos(1).nan = '1' then
			min_max_result <= (P - 2 downto P - E => '1', others => '0');
		else
			case funct3_i is
				when "000" => min_max_result <= fp_min;
				when "001" => min_max_result <= fp_max;
				when others =>
			end case;
		end if;
	end process;

	nan <= fp_infos(0).nan or fp_infos(1).nan;
	COMPARE : process (funct3_i, lt, eq, nan)
	begin
		case funct3_i is
			when FLE =>
			    cmp <= (lt or eq) and (not nan);
			when FLT =>
			    cmp <= lt and (not eq) and (not nan);
			when FEQ =>
				cmp <= eq and (not nan);
			when others => cmp <= '0';
		end case;
	end process;

	fflags_o <= fflags;

	with fp_op_i select
		result_o <= min_max_result when FPU_MINMAX,
			        (0 => cmp, others => '0') when FPU_CMP,
		            (others => '0') when others;

end behavioral;