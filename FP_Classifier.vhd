library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.constants.all;

entity FP_Classifier is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		fp_class_o : out FP_INFO);
end FP_Classifier;

architecture behavioral of FP_Classifier is

	alias exp : STD_LOGIC_VECTOR(E - 1 downto 0) is x_i(P - 2 downto P - E - 1);
	alias man : STD_LOGIC_VECTOR(M - 2 downto 0) is x_i(M - 2 downto 0);

    signal is_boxed : STD_LOGIC;

begin

    SP_IS_BOXED: if P = 32 generate
        is_boxed <= and x_i(63 downto 32);
    else generate
        is_boxed <= '1';
    end generate;

	fp_class_o.normal <= is_boxed and ( (or exp) and (nand exp) );
	fp_class_o.subnormal <= is_boxed and ( (nor exp) and (or man) );
	fp_class_o.zero <= is_boxed and ( nor x_i(P - 2 downto 0) );
	fp_class_o.inf <= is_boxed and ( (and exp) and (nor man) );
	fp_class_o.nan <= not is_boxed or ( (and exp) and (or man) );
	fp_class_o.signaling_nan <= is_boxed and ( (and exp) and ( not  x_i(M - 2) ) and ( or x_i(M-3 downto 0)) );
	fp_class_o.quiet_nan <= (and exp) and ( x_i(M - 2) or ( nor x_i(M-3 downto 0)));

end behavioral; 