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
		result_o : out FP_RESULT;
		result_int_o : out STD_LOGIC_VECTOR (63 downto 0));
end FPU;

architecture behavioral of FPU is

    component FMA is
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
            z_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
            result_o : out FP_RESULT);
    end component FMA;
    
    component FP_Divider is
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
            result_o : out FP_RESULT);
    end component FP_Divider;

    component FP_Converter is
        port (
            fp_precision_i : in STD_LOGIC;
            fp_op_i : in FPU_OP;
            mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
            result_if_o : out STD_LOGIC_VECTOR (63 downto 0);
            result_fi_o : out STD_LOGIC_VECTOR (63 downto 0);
            result_ff_o : out STD_LOGIC_VECTOR (63 downto 0);
            fflags_if_o : out STD_LOGIC_VECTOR (4 downto 0);
            fflags_fi_o : out STD_LOGIC_VECTOR (4 downto 0);
            fflags_ff_o : out STD_LOGIC_VECTOR (4 downto 0));
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

    signal result_fma, result_div : FP_RESULT;
    signal result_cvt_fi, result_cvt_if, result_cvt_ff : STD_LOGIC_VECTOR (63 downto 0);
	signal enable_fma, enable_div_sqrt, fp_valid : STD_LOGIC_VECTOR (1 downto 0);
	signal fp_info_sp, fp_info_dp : FP_INFO;
	signal fp_class_sp, fp_class_dp, result_class : STD_LOGIC_VECTOR (9 downto 0);
    signal fflags_cvt_fi, fflags_cvt_if, fflags_cvt_ff : STD_LOGIC_VECTOR (4 downto 0);
	signal fp_sgnj_sp : STD_LOGIC_VECTOR (63 downto 0);
	signal fp_sgnj_dp, fp_sgnj, result_sgnj : STD_LOGIC_VECTOR (63 downto 0);
    signal fused_multiply_add, sgnj_out, output_fi, div_sqrt : STD_LOGIC;

    type fp_results is array (0 to 1) of FP_RESULT;
    signal results_fma, results_div : fp_results;

    constant zero_fp_result : FP_RESULT := ((others => '0'), (others => '0'), '0');


begin

	FMA_COMPONENTS : for i in 0 to 1 generate 
        FP_FMA: FMA
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
        port map(
            clk_i => clk_i,
            rst_i => rst_i,
            enable_i => enable_fma(i),
            fp_op_i => fp_op_i,
            rm_i => rm_i,
            x_i => x_i(FP_FORMATS(i).P-1 downto 0),
            y_i => y_i(FP_FORMATS(i).P-1 downto 0),
            z_i => z_i(FP_FORMATS(i).P-1 downto 0),
            result_o => results_fma(i));
    end generate;
   
        
	FP_DIV_COMPONENTS : for i in 0 to 0 generate
        FP_DIV: FP_Divider
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
    	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable_div_sqrt(i),
		fp_op_i => fp_op_i,
		rm_i => rm_i,
		x_i => x_i(FP_FORMATS(i).P-1 downto 0),
		y_i => y_i(FP_FORMATS(i).P-1 downto 0),
		result_o => results_div(i)
	);
	end generate;

	FP_CVT : FP_Converter
	port map(
		fp_precision_i => fp_precision_i,
		fp_op_i => fp_op_i,
		mode_i => z_i(1 downto 0),
		rm_i => rm_i,
		x_i => x_i,
		x_int_i => x_int_i,
		result_fi_o => result_cvt_fi,
		result_if_o => result_cvt_if,
		result_ff_o => result_cvt_ff,
		fflags_fi_o => fflags_cvt_fi,
		fflags_if_o => fflags_cvt_if,
		fflags_ff_o => fflags_cvt_ff
	);

--	    FP_CMP: FP_Comparator 
--	    port map (
--	        x_i => x_i,
--	        y_i => y_i,
--	        fp_op_i => fp_op_i,
--	        funct3_i => rm_i,
--	        result_o => result_cmp,
--	        fflags_o = fflags_cmp
--	    );  

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

	with fp_op_i select 
	  fused_multiply_add <= '1' when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB, 
		                    '0' when others;
		                    
    enable_fma(0)<= '1' when fused_multiply_add = '1' and fp_precision_i = '0' and results_fma(0).valid = '0' else '0';
    enable_fma(1) <= '1' when fused_multiply_add = '1' and fp_precision_i = '1' and results_fma(1).valid = '0' else '0';
    
    result_fma <= results_fma(0) when fp_precision_i = '0' else results_fma(1);
    result_div <= results_div(0) when fp_precision_i = '0' else results_div(1);

	with fp_op_i select 
	   div_sqrt <= '1' when FPU_DIV | FPU_SQRT, 
                   '0' when others;

    enable_div_sqrt(0) <= '1' when div_sqrt = '1' and fp_precision_i = '0' and results_div(0).valid = '0' else '0';                   
    enable_div_sqrt(1) <= '1' when div_sqrt = '1' and fp_precision_i = '1' and results_div(0).valid = '0' else '0';                   

	MUX_RESULT_FP_OUTPUT : process (all)
	begin
		case fp_op_i is 
	    	when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB => result_o.value <= result_fma.value;
			when FPU_CVT_IF => result_o.value <= result_cvt_if;
			when FPU_CVT_FF => result_o.value <= result_cvt_ff;
			when FPU_DIV | FPU_SQRT => result_o.value <= results_div(0).value;
			when others => result_o.value <= (others => '0');
		end case; 
	end process;
         
   
   with fp_op_i select result_o.valid <= 
        result_fma.valid when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB,
        '1' when FPU_CVT_FI | FPU_CVT_IF | FPU_CVT_FF, '0' when others;
	
	
	process(all)
	begin
	   case fp_op_i is 
	       when FPU_CVT_FI => result_int_o <= result_cvt_fi; 
	       when FPU_CLASS => result_int_o <= (63 downto 10 => '0') & result_class;
	       when others => result_int_o <= (others => '0'); 
	   end case;
	end process;
	
	process (all)
	begin
        case fp_op_i is
            when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB => result_o.fflags <= result_fma.fflags;
			when FPU_CVT_FI => result_o.fflags <= fflags_cvt_fi;
			when FPU_CVT_IF => result_o.fflags <= fflags_cvt_if;
			when FPU_CVT_FF => result_o.fflags <= fflags_cvt_ff;
			when FPU_DIV | FPU_SQRT => result_o.fflags <= results_div(0).fflags;
			when others => result_o.fflags <= (others => '0'); 
        end case;
	end process;
	
end behavioral;