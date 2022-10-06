library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity regfile is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		cpu_enable_i : in STD_LOGIC;
		reg_dst_i : in REG;
		registers_o : out reg_t);
end regfile;

architecture behavioral of regfile is

	signal registers : reg_t := (others => (others => '0'));
	signal reg_write : STD_LOGIC;

begin

	registers_o <= registers;

	reg_write <= reg_dst_i.write and (or reg_dst_i.dest);

	REGISTER_WRITE : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or cpu_enable_i = '0' then
				registers <= (others => (others => '0'));
			else
				if reg_write = '1' then
					registers(to_integer(unsigned(reg_dst_i.dest))) <= reg_dst_i.data;
				end if;
			end if;
		end if;
	end process;

end behavioral;