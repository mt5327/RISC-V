library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FPU is
	generic ( 
	    P : natural; 
	    E : natural; 
	    M : natural);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
	    enable_fpu_subunit_i : in STD_LOGIC_VECTOR (4 downto 0);
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
            x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            z_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            is_boxed_i : in STD_LOGIC_VECTOR (2 downto 0);
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
            x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            is_boxed_i : in STD_LOGIC_VECTOR (1 downto 0);
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
            is_boxed_i : in STD_LOGIC;
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
            x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
            is_boxed_i : in STD_LOGIC_VECTOR (1 downto 0);
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
			x_i : in STD_LOGIC_VECTOR (P-2 downto 0);
			is_boxed_i : in STD_LOGIC;
			fp_class_o : out FP_INFO);
	end component FP_Classifier;

    signal result_fma, result_div, result_cvt_fi, result_cvt_if, result_cvt_ff : FP_RESULT;
	signal enable_fma, enable_div_sqrt : STD_LOGIC_VECTOR (1 downto 0);
	signal enable_cvt : STD_LOGIC_VECTOR (5 downto 0);
	signal fp_info_x : FP_INFO;
    signal fflags_cvt_fi, fflags_cvt_if, fflags_cvt_ff, fflags_cmp, fflags_min_max : STD_LOGIC_VECTOR (4 downto 0);
	signal result_sgnj : STD_LOGIC_VECTOR (63 downto 0);
    signal fused_multiply_add, div_sqrt, result_cvt_valid : STD_LOGIC := '0';
    
    signal is_boxed : STD_LOGIC_VECTOR (2 downto 0);
    signal x, x_int : STD_LOGIC_VECTOR (63 downto 0);

    signal fflags_cmp_sp_dp, fflags_min_max_sp_dp : STD_LOGIC_VECTOR (9 downto 0);
    signal result_min_max, min_max : FP_RESULT;
    signal result_cmp : STD_LOGIC;
	signal result_class : STD_LOGIC_VECTOR (9 downto 0);

    signal x_sgnj_sp : STD_LOGIC_VECTOR (30 downto 0);

begin

    FP_FMA: FMA
    generic map (P, E, M)
    port map(
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_fpu_subunit_i(0),
        fp_op_i => fp_op_i,
        rm_i => rm_i,
        x_i => x_i(P-1 downto 0),
        y_i => y_i(P-1 downto 0),
        z_i => z_i(P-1 downto 0),
        is_boxed_i => is_boxed,
        result_o => result_fma
    );
    
    FP_DIV: FP_Divider
    generic map (P, E, M)
    port map(
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_fpu_subunit_i(1),
        fp_op_i => fp_op_i,
        rm_i => rm_i,
        x_i => x_i(P-1 downto 0),
        y_i => y_i(P-1 downto 0),
        is_boxed_i => is_boxed(1 downto 0),
        result_o => result_div
    );
    
    FP_CVT : FP_Converter
    generic map (P, E, M)
    port map(
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_fpu_subunit_i(4 downto 2),
        mode_i => cvt_mode_i,
        rm_i => rm_i,
        x_i => x_i,
        x_int_i => x_int_i,
        is_boxed_i => is_boxed(0),
        result_fi_o => result_cvt_fi,
        result_if_o => result_cvt_if,
        result_ff_o => result_cvt_ff
    );
        
    FP_CMP: FP_Comparator 
    generic map (P, E, M)
    port map (
        x_i => x_i(P-1 downto 0),
        y_i => y_i(P-1 downto 0),
        is_boxed_i => is_boxed(1 downto 0),
        funct3_i => rm_i,
        result_cmp_o => result_cmp,
        result_min_max_o => result_min_max,
        fflags_cmp_o => fflags_cmp
    );

    NAN_BOXING: if P = 32 generate
        is_boxed(0) <= and x_i(63 downto 32);
        is_boxed(1) <= and y_i(63 downto 32);
        is_boxed(2) <= and z_i(63 downto 32);
    else generate
        is_boxed <= "111";    
    end generate;

	CLASSIFY : FP_Classifier generic map(P, E, M) port map(x_i(P-2 downto 0), is_boxed(0), fp_info_x);
	result_class <= fp_classify(fp_info_x, x_i(P-1));
    
    SGNJ_RESULT_GEN : if P = 32 generate
            signal sign_x, sign_y : STD_LOGIC;
            signal x_sgnj_sp : STD_LOGIC_VECTOR (30 downto 0);
        begin           
            sign_x <= x_i(31) and is_boxed(0);
            sign_y <= y_i(31) and is_boxed(1);
            x_sgnj_sp <= x_i(30 downto 0) when is_boxed(0) = '1' else (30 downto 22 => '1', others => '0');
            result_sgnj <= (63 downto 32 => '1') & fp_sign_injection(x_sgnj_sp, sign_x, sign_y, rm_i);
        end;
    else generate 
           result_sgnj <= fp_sign_injection(x_i(P-2 downto 0), x_i(P-1),  y_i(P-1), rm_i);
    end generate;
    
    FP_MV_GEN: if P = 32 generate 
        x <= (63 downto 32 => x_i(31)) & x_i(31 downto 0);
        x_int <= (63 downto 32 => '1') & x_int_i(31 downto 0);
    else generate
        x <= x_i;
        x_int <= x_int_i;
    end generate;
	
	MUX_RESULT_FP_OUTPUT : process (all)
	begin
		case fp_op_i is 
--	    	when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB => result_o <= result_fma;
--  		when FPU_DIV | FPU_SQRT => result_o <= result_div;
--		    when FPU_CVT_FI => result_o <= result_cvt_fi;
--		    when FPU_CVT_IF => result_o <= result_cvt_if;
--			when FPU_CVT_FF => result_o <= result_cvt_ff;
--			when FPU_MINMAX => result_o <= result_min_max; 
--	        when FPU_CMP => result_o <= ((63 downto 1 => '0') & result_cmp, fflags_cmp, '1');
--			when FPU_SGNJ => result_o <= (result_sgnj, "00000", '1');
--	        when FPU_CLASS => result_o <= ((63 downto 10 => '0') & result_class, "00000", '1');
	        when FPU_MV_FX => result_o <= (x, "00000", '1'); 
			when FPU_MV_XF => result_o <= (x_int, "00000", '1');
		    when others => result_o <= ((others => '-'), (others => '-'), '0');
		end case; 
	end process;
         		    
	
end behavioral; 