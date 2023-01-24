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
		x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
		y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
		is_boxed_i : in STD_LOGIC_VECTOR (1 downto 0);
		funct3_i : in STD_LOGIC_VECTOR (2 downto 0);
		result_cmp_o : out STD_LOGIC;
	    fflags_cmp_o : out STD_LOGIC_VECTOR (4 downto 0);
		result_min_max_o : out FP_RESULT);
end FP_Comparator;

architecture behavioral of FP_Comparator is

	signal cmp, cmp_fle, cmp_flt, cmp_feq, lt, lt_abs, eq, x_nan, y_nan, nan, signaling_nan : STD_LOGIC;

	signal fp_infos : fp_infos_t(0 to 1);

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

	signal invalid_nan : STD_LOGIC;

	signal min_max_result, fp_min, fp_max : STD_LOGIC_VECTOR (P - 1 downto 0);
    signal invalid_sp_x, invalid_sp_y  : STD_LOGIC;
begin

	FP_CLASS_X : FP_Classifier generic map(P, E, M) port map(x_i(P-2 downto 0), is_boxed_i(0), fp_infos(0));
	FP_CLASS_Y : FP_Classifier generic map(P, E, M) port map(y_i(P-2 downto 0), is_boxed_i(1), fp_infos(1));

	lt_abs <= '1' when unsigned(x_i(P-1 downto 0)) < unsigned(y_i(P-1 downto 0)) else '0';
	
	eq <= '1' when (unsigned(x_i(P-1 downto 0)) = unsigned(y_i(P-1 downto 0))) or (fp_infos(0).zero = '1' and fp_infos(1).zero = '1') else '0';
	lt <= lt_abs xor (x_i(P-1) or y_i(P-1));
	fp_min <= x_i(P-1 downto 0) when lt = '1' and fp_infos(0).nan = '0' else y_i(P-1 downto 0);
	fp_max <= x_i(P-1 downto 0) when lt = '0' and fp_infos(0).nan = '0' else y_i(P-1 downto 0);

	signaling_nan <= fp_infos(0).signaling_nan or fp_infos(1).signaling_nan;
 
	MIN_MAX : process (fp_min, fp_max, funct3_i, fp_infos(0).nan, fp_infos(1).nan)
	begin
		if fp_infos(0).nan = '1' and fp_infos(1).nan = '1' then
			min_max_result <= (P - 2 downto P - E - 2 => '1', others => '0');
		else
			case funct3_i is
				when "000" => min_max_result <= fp_min;
				when "001" => min_max_result <= fp_max;
				when others => min_max_result <= (others => '-');
			end case;
		end if;
	end process;
    
    cmp_fle <= (lt or eq) and (not nan);
    cmp_flt <= lt and (not eq) and (not nan);
    cmp_feq <= eq and (not nan);
    
	nan <= fp_infos(0).nan or fp_infos(1).nan;
	
	COMPARE : process (funct3_i, cmp_fle, cmp_flt, cmp_feq, nan)
	begin
		case funct3_i is
			when FLE =>
			    invalid_nan <= nan;
			    cmp <= cmp_fle;
			when FLT =>
			    invalid_nan <= nan;
			    cmp <= cmp_flt;
			when FEQ =>
			    invalid_nan <= '0';
				cmp <= cmp_feq;
			when others => cmp <= '0'; invalid_nan <= '0';
		end case;
	end process;

    fflags_cmp_o <= (signaling_nan or invalid_nan ) & "0000";
    result_cmp_o <= cmp;
    
    MIN_MAX_OUTPUT: if P = 32 generate 
        result_min_max_o <= ((63 downto 32 => '1') & min_max_result, signaling_nan & "0000", '1');
    else generate 
        result_min_max_o <= (min_max_result, signaling_nan & "0000", '1');
    end generate;
    
end behavioral;