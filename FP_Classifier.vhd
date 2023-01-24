library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.constants.all;

entity FP_Classifier is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
		x_i : in STD_LOGIC_VECTOR (P-2 downto 0);
		is_boxed_i : STD_LOGIC;
		fp_class_o : out FP_INFO);
end FP_Classifier;

architecture behavioral of FP_Classifier is

	alias exp : STD_LOGIC_VECTOR(E - 1 downto 0) is x_i(P - 2 downto P - E - 1);
	alias man : STD_LOGIC_VECTOR(M - 2 downto 0) is x_i(M - 2 downto 0);

    signal nan, signaling_nan : STD_LOGIC;

begin

    nan <=  (and exp) and (or man);
    signaling_nan <= nan and (not x_i(M-2) );

	fp_class_o.normal <= is_boxed_i and ( (or exp) and (nand exp) );
	fp_class_o.subnormal <= is_boxed_i and ( (nor exp) and (or man) );
	fp_class_o.zero <= is_boxed_i and ( nor x_i(P - 2 downto 0) );
	fp_class_o.inf <= is_boxed_i and ( (and exp) and (nor man) );
	fp_class_o.nan <= not is_boxed_i or nan;
	fp_class_o.signaling_nan <= is_boxed_i and signaling_nan;
	fp_class_o.quiet_nan <= not is_boxed_i and nan and not signaling_nan;

end behavioral; 