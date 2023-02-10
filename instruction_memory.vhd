library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use std.textio.all;

entity instruction_memory is
	generic (
		RAM_FILENAME : STRING := "C:\\DigitalDesign\\hex\\fmin.hex";
		BLOCK_ADDRESS_WIDTH : NATURAL;
		BLOCK_SIZE : NATURAL);
	port (
        clk_i : in STD_LOGIC;

        mem_init_i : in STD_LOGIC;

        uart_data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE-1 downto 0);
        read_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
        write_address_i : in STD_LOGIC_VECTOR (BLOCK_ADDRESS_WIDTH - 1 downto 0);
        data_imem_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0));
end instruction_memory;

architecture behavioral of instruction_memory is

	type ram_type is array (0 to 2 ** (BLOCK_ADDRESS_WIDTH) - 1) of STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
	attribute ram_style : STRING;

	impure function initramfromfile (ramfilename : in STRING) return ram_type is
		file ramfile : text open READ_MODE is ramfilename;
		variable ramfileline : line;
		variable ram_content : ram_type;
	begin
		for i in 0 to 2 ** (BLOCK_ADDRESS_WIDTH) - 1 loop
		  if not endfile(ramfile) then
				readline (ramfile, ramfileline);
				hread (ramfileline, ram_content(i));
			else
				ram_content(i) := (others => '0');
			end if;
		end loop;
		return ram_content;
	end function;

	signal ram : ram_type := initramfromFile(RAM_FILENAME);
	attribute ram_style of ram : signal is "block";

    signal data_imem : STD_LOGIC_VECTOR (BLOCK_SIZE-1 downto 0);

begin

	WRITE_ACCESS : process (clk_i)
	begin
		if rising_edge(clk_i) then
		   if mem_init_i = '1' then
	           ram(to_integer(unsigned(write_address_i))) <= uart_data_i;
	       end if;
		end if;
	end process;

	READ_ACCESS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			data_imem <= ram(to_integer(unsigned(read_address_i)));
		end if;
	end process;
	
	data_imem_o <= data_imem;

end behavioral;
