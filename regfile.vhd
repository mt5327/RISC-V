library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity regfile is
	port (
		clk_i : in STD_LOGIC;
		rst_ni : in STD_LOGIC;
		reg_dst_i : in REG;
		registers_o : out reg_t);
end regfile;

architecture behavioral of regfile is

	signal registers : reg_t := (others => (others => '0'));

begin

	registers_o <= registers;

	REGISTER_WRITE : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_ni = '0' then
				registers <= (others => (others => '0'));
			else
				if reg_dst_i.write = '1' then
					registers(to_integer(unsigned(reg_dst_i.dest))) <= reg_dst_i.data;
				end if;
			end if;
		end if;
	end process;

end behavioral;