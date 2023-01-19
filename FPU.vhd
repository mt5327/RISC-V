library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FPU is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
	    enable_fpu_subunit_i : in STD_LOGIC_VECTOR (4 downto 0);
	--	fp_i : in STD_LOGIC;
		fp_precision_i : in STD_LOGIC_VECTOR (1 downto 0);
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		cvt_mode_i : in STD_LOGIC_VECTOR (1 downto 0);
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		z_i : in STD_LOGIC_VECTOR (63 downto 0);
		x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
		result_o : out FP_RESULT);
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
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            y_i : in STD_LOGIC_VECTOR (63 downto 0);
            z_i : in STD_LOGIC_VECTOR (63 downto 0);
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
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            y_i : in STD_LOGIC_VECTOR (63 downto 0);
            result_o : out FP_RESULT);
    end component FP_Divider;

    component FP_Converter is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            clk_i : in STD_LOGIC;
            rst_i : in STD_LOGIC;
            enable_i : in STD_LOGIC_VECTOR (2 downto 0);
            mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
            result_if_o : out FP_RESULT;
            result_fi_o : out FP_RESULT;
            result_ff_o : out FP_RESULT);
    end component FP_Converter;

    component FP_Comparator is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            y_i : in STD_LOGIC_VECTOR (63 downto 0);
            funct3_i : in STD_LOGIC_VECTOR (2 downto 0);
            result_cmp_o : out STD_LOGIC;
            result_min_max_o : out FP_RESULT;
            fflags_cmp_o : out STD_LOGIC_VECTOR (4 downto 0));
    end component FP_Comparator;

	component FP_Classifier is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			fp_class_o : out FP_INFO);
	end component FP_Classifier;


    signal result_fma, result_div, result_cvt_fi, result_cvt_if, result_cvt_ff : FP_RESULT;
	signal enable_fma, enable_div_sqrt : STD_LOGIC_VECTOR (1 downto 0);
	signal enable_cvt : STD_LOGIC_VECTOR (5 downto 0);
	signal fp_info_sp, fp_info_dp : FP_INFO;
    signal fflags_cvt_fi, fflags_cvt_if, fflags_cvt_ff, fflags_cmp, fflags_min_max : STD_LOGIC_VECTOR (4 downto 0);
	signal fp_sgnj_sp : STD_LOGIC_VECTOR (31 downto 0);
	signal fp_sgnj_dp, fp_sgnj, result_sgnj : STD_LOGIC_VECTOR (63 downto 0);
    signal fused_multiply_add, div_sqrt, result_cvt_valid : STD_LOGIC := '0';
    
    signal is_boxed_x, is_boxed_y, sign_x, sign_y : STD_LOGIC;
    signal x, x_int : STD_LOGIC_VECTOR (63 downto 0);

    signal fflags_cmp_sp_dp, fflags_min_max_sp_dp : STD_LOGIC_VECTOR (9 downto 0);
    signal result_min_max, min_max : FP_RESULT;
    signal results_cmp : STD_LOGIC_VECTOR (1 downto 0);
    signal result_cmp : STD_LOGIC;
	signal fp_class_sp, fp_class_dp, result_class : STD_LOGIC_VECTOR (9 downto 0);

    signal x_sgnj_sp : STD_LOGIC_VECTOR (30 downto 0);

    type fp_results is array (0 to 1) of FP_RESULT;
    signal results_fma, results_div, results_cvt_fi, results_cvt_if, results_cvt_ff, results_min_max : fp_results;

begin

	FPU_COMPONENTS : for i in 0 to 1 generate 
        FP_FMA: FMA
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
        port map(
            clk_i => clk_i,
            rst_i => rst_i,
            enable_i => enable_fma(i),
            fp_op_i => fp_op_i,
            rm_i => rm_i,
            x_i => x_i,
            y_i => y_i,
            z_i => z_i,
            result_o => results_fma(i)
        );
        
        FP_DIV: FP_Divider
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
    	port map(
            clk_i => clk_i,
            rst_i => rst_i,
            enable_i => enable_div_sqrt(i),
            fp_op_i => fp_op_i,
            rm_i => rm_i,
            x_i => x_i,
            y_i => y_i,
            result_o => results_div(i)
	    );
	    
	    FP_CVT : FP_Converter
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
        port map(
            clk_i => clk_i,
            rst_i => rst_i,
            enable_i => enable_cvt(i * 3 + 2 downto i * 3),
            mode_i => cvt_mode_i,
            rm_i => rm_i,
            x_i => x_i,
            x_int_i => x_int_i,
            result_fi_o => results_cvt_fi(i),
            result_if_o => results_cvt_if(i),
            result_ff_o => results_cvt_ff(i)
		);
        	
	    FP_CMP: FP_Comparator 
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
	    port map (
	        x_i => x_i,
	        y_i => y_i,
	        funct3_i => rm_i,
	        result_cmp_o => results_cmp(i),
	        result_min_max_o => results_min_max(i),
	        fflags_cmp_o => fflags_cmp_sp_dp(i * 5 + 4 downto i * 5)
	    );
	end generate;

	FP_CLASSIFIER_SP : FP_Classifier generic map(32, 8, 24) port map(x_i, fp_info_sp);
    FP_CLASSIFIER_DP : FP_Classifier generic map(64, 11, 53) port map(x_i, fp_info_dp);

	fp_class_sp <= fp_classify(fp_info_sp, x_i(31));
	fp_class_dp <= fp_classify(fp_info_dp, x_i(63));
    result_class <= fp_class_sp when fp_precision_i(0) = '1' else fp_class_dp; 
    --result_class <= fp_classify(fp_info_x, x_i(31)) when fp_precision_i(0) = '1' else fp_classify(fp_info_dp, x_i(63)); 

    is_boxed_x <= and x_i(63 downto 32);
    is_boxed_y <= and y_i(63 downto 32);
    
    sign_x <= x_i(31) and is_boxed_x;
    sign_y <= y_i(31) and is_boxed_y;

    x_sgnj_sp <= x_i(30 downto 0) when is_boxed_x = '1' else (30 downto 22 => '1', others => '0');
	fp_sgnj_sp <= fp_sign_injection(x_sgnj_sp, sign_x, sign_y, rm_i);
	fp_sgnj_dp <= fp_sign_injection(x_i(62 downto 0), x_i(63),  y_i(63), rm_i);
    result_sgnj <= (63 downto 32 => '1') & fp_sgnj_sp when fp_precision_i(0) = '1' else fp_sgnj_dp;
	
	ENABLE_GEN: for i in 0 to 1 generate
        enable_fma(i) <= enable_fpu_subunit_i(0) and fp_precision_i(i) and not results_fma(i).valid;
        enable_div_sqrt(i) <= enable_fpu_subunit_i(1) and fp_precision_i(i) and not results_div(i).valid;                   
        enable_cvt(i * 3) <= enable_fpu_subunit_i(2) and fp_precision_i(i) and not results_cvt_fi(i).valid;
        enable_cvt(i * 3 + 1) <= enable_fpu_subunit_i(3) and fp_precision_i(i) and not results_cvt_if(i).valid;
        ENABLE_CVT_FF: if i = 0 generate
            enable_cvt(i * 3 + 2) <= enable_fpu_subunit_i(4) and fp_precision_i(1) and not results_cvt_ff(i).valid;
        else generate
            enable_cvt(i * 3 + 2) <= enable_fpu_subunit_i(4) and fp_precision_i(0) and not results_cvt_ff(i).valid;
        end generate;
    end generate;
    
    result_fma <= results_fma(0) when fp_precision_i(0) = '1' else results_fma(1);
    result_div <= results_div(0) when fp_precision_i(0) = '1' else results_div(1);
    result_cvt_fi <= results_cvt_fi(0) when fp_precision_i(0) = '1' else results_cvt_fi(1);
    result_cvt_if <= results_cvt_if(0) when fp_precision_i(0) = '1' else results_cvt_if(1);
    result_cvt_ff <= results_cvt_ff(0) when cvt_mode_i(0) = '0' else results_cvt_ff(1);
    result_min_max <= results_min_max(0) when fp_precision_i(0) = '1' else results_min_max(1); 
    
    result_cmp <= results_cmp(0) when fp_precision_i(0) = '1' else results_cmp(1);	
    fflags_cmp <= fflags_cmp_sp_dp(4 downto 0) when fp_precision_i(0) = '1' else fflags_cmp_sp_dp(9 downto 5);
 
    x <= (63 downto 32 => x_i(31)) & x_i(31 downto 0) when fp_precision_i(0) = '1' else x_i; 
    x_int <= (63 downto 32 => '1') & x_int_i(31 downto 0) when fp_precision_i(0) = '1' else x_int_i;
	
	MUX_RESULT_FP_OUTPUT : process (all)
	begin
		case fp_op_i is 
	    	when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB => result_o <= result_fma;
			when FPU_DIV | FPU_SQRT => result_o <= result_div;
		    when FPU_CVT_FI => result_o <= result_cvt_fi;
		    when FPU_CVT_IF => result_o <= result_cvt_if;
			when FPU_CVT_FF => result_o <= result_cvt_ff;
			when FPU_MINMAX => result_o <= result_min_max; 
	        when FPU_CMP => result_o <= ((63 downto 1 => '0') & result_cmp, fflags_cmp, '1');
			when FPU_SGNJ => result_o <= (result_sgnj, "00000", '1');
	        when FPU_CLASS => result_o <= ((63 downto 10 => '0') & result_class, "00000", '1');
	        when FPU_MV_FX => result_o <= (x, "00000", '1'); 
			when FPU_MV_XF => result_o <= (x_int, "00000", '1');
		    when others => result_o <= ((others => '-'), (others => '-'), '0');
		end case; 
	end process;
         		    
	
end behavioral; 