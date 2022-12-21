library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter is
	port (
	    clk_i : in STD_LOGIC;
	    enable_i : in STD_LOGIC;
		fp_precision_i : in STD_LOGIC_VECTOR (1 downto 0);
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
end FP_Converter;

architecture behavioral of FP_Converter is

	signal signaling_nan, valid, valid_res : STD_LOGIC;
    signal enable_cvt_fi : STD_LOGIC_VECTOR (1 downto 0);

	signal result_fi_sp, result_fi_dp, result_if_dp, result_ff_dp : STD_LOGIC_VECTOR (63 downto 0);
	signal result_if_sp, result_ff_sp : STD_LOGIC_VECTOR (63 downto 0);

	signal exp_dp : STD_LOGIC_VECTOR(10 downto 0);

    signal x_sp : STD_LOGIC_VECTOR (31 downto 0); 
    signal x_dp : STD_LOGIC_VECTOR (63 downto 0);

	signal result_fi, result_if, result_ff : STD_LOGIC_VECTOR (63 downto 0);

	signal fflags_fi_sp, fflags_fi_dp, fflags_if_sp, fflags_if_dp, fflags_fi, fflags_if : STD_LOGIC_VECTOR (4 downto 0);
	signal fflags_ff_sp, fflags_ff_dp, fflags_ff : STD_LOGIC_VECTOR (4 downto 0);

	component FP_Converter_float_to_int is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			clk_i : in STD_LOGIC;
		    enable_i : in STD_LOGIC;
			x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
			mode_i : in STD_LOGIC_VECTOR (1 downto 0);
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
			result_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component FP_Converter_float_to_int;

	component FP_Converter_int_to_float is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
		    clk_i : in STD_LOGIC;
		    enable_i : in STD_LOGIC;
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			mode_i : in STD_LOGIC_VECTOR (1 downto 0);
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
			result_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component FP_Converter_int_to_float;

	component FP_Converter_double_to_float is
		port (
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
			result_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component FP_Converter_double_to_float;
	
begin

	FP_CVT_IF_SP : FP_Converter_int_to_float generic map(32, 8, 24) port map(clk_i, enable_i, x_int_i, mode_i, rm_i, fflags_if_sp, result_if_sp);
	FP_CVT_IF_DP : FP_Converter_int_to_float generic map(64, 11, 53) port map(clk_i, enable_i, x_int_i, mode_i, rm_i, fflags_if_dp, result_if_dp);
	FP_CVT_FI_SP : FP_Converter_float_to_int generic map(32, 8, 24) port map(clk_i, enable_i, x_i(31 downto 0), mode_i, rm_i, fflags_fi_sp, result_fi_sp);
	FP_CVT_FI_DP : FP_Converter_float_to_int generic map(64, 11, 53) port map(clk_i, enable_i, x_i, mode_i, rm_i, fflags_fi_dp, result_fi_dp);

	FP_CVT_FF : FP_Converter_double_to_float port map(x_i, rm_i, fflags_ff_sp, result_ff_sp);
	
	exp_dp <= STD_LOGIC_VECTOR(("000" & unsigned(x_i(30 downto 23))) + BIAS_DIFF);
	result_ff_dp <= x_i(31) & exp_dp & x_i(22 downto 0) & (28 downto 0 => '0');
	signaling_nan <= (and x_i(30 downto 22)) and (nor x_i(21 downto 0));
	fflags_ff_dp <= signaling_nan & "0000";
	
	result_fi_o <= result_fi_sp when fp_precision_i(0) = '1' else result_fi_dp;    
    result_if_o <= result_if_sp when fp_precision_i(0) = '1' else result_if_dp;
    result_ff_o <= result_ff_sp when fp_precision_i(0) = '1' else result_ff_dp;
    
	fflags_fi_o <= fflags_fi_sp when fp_precision_i(0) = '1' else fflags_fi_dp;
	fflags_if_o <= fflags_if_sp when fp_precision_i(0) = '1' else fflags_if_dp;
	fflags_ff_o <= fflags_ff_sp when fp_precision_i(0) = '1' else fflags_ff_dp; 

end behavioral;