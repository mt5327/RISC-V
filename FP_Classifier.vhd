library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.constants.all;

entity FP_Classifier is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
		x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
		fp_class_o : out FP_INFO);
end FP_Classifier;

architecture behavioral of FP_Classifier is

	alias exp : STD_LOGIC_VECTOR(E - 1 downto 0) is x_i(P - 2 downto P - E - 1);
	alias man : STD_LOGIC_VECTOR(M - 2 downto 0) is x_i(M - 2 downto 0);

begin

	fp_class_o.normal <= (or exp) and (nand exp);
	fp_class_o.subnormal <= (nor exp) and (or man);
	fp_class_o.zero <= nor x_i(P - 2 downto 0);
	fp_class_o.inf <= (and exp) and (nor man);
	fp_class_o.nan <= (and exp) and (or man);
	fp_class_o.signaling_nan <= (and exp) and x_i(M - 2) and (nor x_i(M - 3 downto 0));
	fp_class_o.quiet_nan <= (and exp) and (or man) and (not x_i(M - 2) or (or x_i(M - 3 downto 0)));

end behavioral;