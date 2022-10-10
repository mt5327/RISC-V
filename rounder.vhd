library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity rounder is
	generic (SIZE : NATURAL);
	port (
		x_i : in unsigned (SIZE - 1 downto 0);
		sign_i : in STD_LOGIC;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
		z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
end rounder;

architecture behavioral of rounder is

	signal round_up : STD_LOGIC;

begin

	ROUNDING : process (rm_i, x_i, round_sticky_i, sign_i)
	begin
		case rm_i is
			when RNE =>
				case round_sticky_i is
					when "00" | "01" => round_up <= '0';
					when "10" => round_up <= x_i(0);
					when "11" => round_up <= '1';
					when others => round_up <= '0';
				end case;
			when RTZ => round_up <= '0';
			when RDN => round_up <= sign_i and (or round_sticky_i);
			when RUP => round_up <= not sign_i and (or round_sticky_i);
			when RMM => round_up <= round_sticky_i(1);
			when others => round_up <= '0';
		end case;
	end process;
    
	z_o <= STD_LOGIC_VECTOR(x_i + round_up);

end behavioral;