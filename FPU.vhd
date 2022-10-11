library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FPU is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
		fp_precision_i : in STD_LOGIC;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		z_i : in STD_LOGIC_VECTOR (63 downto 0);
		x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
		result_o : out FP_RESULT);
end FPU;

architecture behavioral of FPU is

	component FMA is
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
	end component FMA;

	component FP_Divider is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			enable_i : in STD_LOGIC;
			fp_precision_i : in STD_LOGIC;
			fp_op_i : in FPU_OP;
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			result_o : out FP_RESULT);
	end component FP_Divider;

	component FP_Converter is
		port (
			clk_i : in STD_LOGIC;
			fp_precision_i : in STD_LOGIC;
			fp_op_i : in FPU_OP;
			mode_i : in STD_LOGIC_VECTOR (1 downto 0);
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
			result_o : out FP_RESULT);
	end component FP_Converter;

	component FP_Comparator is
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
	end component FP_Comparator;

	component FP_Classifier is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
			fp_class_o : out FP_INFO);
	end component FP_Classifier;

    signal result : FP_RESULT;
	signal result_fma, result_cvt, result_div : FP_RESULT;
	signal enable_fma, enable_div_sqrt, fp_valid, sgnj_out : STD_LOGIC;
	signal fp_info_sp, fp_info_dp : FP_INFO;
	signal output_select : STD_LOGIC_VECTOR (2 downto 0);
	signal fp_class_sp, fp_class_dp, result_class : STD_LOGIC_VECTOR (9 downto 0);

	signal fp_sgnj_sp : STD_LOGIC_VECTOR (63 downto 0);
	signal fp_sgnj_dp, fp_sgnj, result_sgnj : STD_LOGIC_VECTOR (63 downto 0);

begin

	FP_FMA : FMA
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable_fma,
		fp_precision_i => fp_precision_i,
		fp_op_i => fp_op_i,
		rm_i => rm_i,
		x_i => x_i,
		y_i => y_i,
		z_i => z_i,
		result_o => result_fma
	);

	FP_DIV : FP_Divider
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable_div_sqrt,
		fp_precision_i => fp_precision_i,
		fp_op_i => fp_op_i,
		rm_i => rm_i,
		x_i => x_i,
		y_i => y_i,
		result_o => result_div
	);

	FP_CVT : FP_Converter
	port map(
		clk_i => clk_i,
		fp_precision_i => fp_precision_i,
		fp_op_i => fp_op_i,
		mode_i => z_i(1 downto 0),
		rm_i => rm_i,
		x_i => x_i,
		x_int_i => x_int_i,
		result_o => result_cvt
	);

	--    FP_CMP: FP_Comparator 
	--    port map (
	--        x_i => x_i,
	--        y_i => y_i,
	--        fp_op_i => fp_op_i,
	--        funct3_i => rm_i,
	--        result_o => cmp,
	--        fflags_o => fflags_cmp
	--    );  

	FP_CLASSIFIER_SP : FP_Classifier generic map(32, 8, 24) port map(x_i(30 downto 0), fp_info_sp);
	FP_CLASSIFIER_DP : FP_Classifier generic map(64, 11, 53) port map(x_i(62 downto 0), fp_info_dp);
	
	fp_class_sp <= fp_classify(fp_info_sp, x_i(31));
	fp_class_dp <= fp_classify(fp_info_dp, x_i(63));
	
	result_class <= fp_class_sp when fp_precision_i = '0' else fp_class_dp;

	fp_sgnj_sp <= x_i(63 downto 32) & fp_sign_injection(x_i(31 downto 0), y_i(31), rm_i);
	fp_sgnj_dp <= fp_sign_injection(x_i, y_i(63), rm_i);
	fp_sgnj <= fp_sgnj_sp when fp_precision_i = '0' else fp_sgnj_dp;
	sgnj_out <= '1' when fp_op_i = FPU_SGNJ else '0';
	result_sgnj <= fp_sgnj when sgnj_out = '1' else (others => '0');

	with fp_op_i select enable_fma <= 
		'1' when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB, 
		'0' when others;
	
	with fp_op_i select enable_div_sqrt <= '1' 
		when FPU_DIV | FPU_SQRT, '0' 
		when others;

	MUX_RESULT_FP_OUTPUT : process (result_cvt, fp_op_i)
	begin
		case fp_op_i is
			--when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB => result_o <= result_fma;
		--	when FPU_DIV | FPU_SQRT => result_o <= result_div;
		--	when FPU_SGNJ => result_o <= result_sgnj;
			when FPU_CVT_FI => result_o <= result_cvt;
			when others => result_o <= ((others => '0'), (others => '0'), '0');
		end case;
	end process;

	--    MUX_FFLAGS_OUTPUT: process(fflags_fma, fflags_div, fp_op_i)
	--    begin       
	--        case fp_op_i is  
	--            when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB => fflags_o <= fflags_fma;
	--            when FPU_DIV | FPU_SQRT => fflags_o <= fflags_div;
	--            when others => fflags_o <= (others => '0');
	--        end case;
	--    end process; 
end behavioral;