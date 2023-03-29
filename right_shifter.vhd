library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity right_shifter is
	generic (SIZE : NATURAL);
	port (
		x_i : in unsigned (SIZE - 1 downto 0);
		shamt_i : in unsigned (num_bits(SIZE) - 1 downto 0);
		z_o : out unsigned (SIZE - 1 downto 0);
		sticky_bit_o : out STD_LOGIC);
end right_shifter;

architecture behavioral of right_shifter is

	type bs_level_t is array (shamt_i'length downto 0) of unsigned(SIZE - 1 downto 0);
	signal bs_level : bs_level_t;

	signal sb_level : STD_LOGIC_VECTOR (shamt_i'left downto 0);

begin

	bs_level(shamt_i'length) <= x_i;

	BARREL_SHIFTER : for i in shamt_i'left downto 0 generate
		
        bs_level(i) <= ((2 ** i) - 1 downto 0 => '0') & bs_level(i + 1)(SIZE - 1 downto 2 ** i) when shamt_i(i) = '1' else 
                       bs_level(i + 1);
		
        STICKY : if i = shamt_i'left generate
			sb_level(i) <= (or bs_level(i + 1)(2 ** i - 1 downto 0)) and shamt_i(i);
		else generate
            sb_level(i) <= ((or bs_level(i + 1)(2 ** i - 1 downto 0)) and shamt_i(i)) or sb_level(i + 1);
		end generate;
	end generate;

	z_o <= bs_level(0);
	sticky_bit_o <= sb_level(0);

end behavioral;