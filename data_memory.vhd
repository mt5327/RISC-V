library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use std.textio.all;

entity data_memory is
	generic (
		RAM_FILENAME : STRING := "C:\\DigitalDesign\\hex\\lb.hex";
		BLOCK_ADDRESS_WIDTH : NATURAL;
		BLOCK_SIZE : NATURAL);
	port (
		clk_i : in STD_LOGIC;

		mem_init_i : in STD_LOGIC;
		mem_write_i : in STD_LOGIC;

		uart_data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
		read_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
		write_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
        write_address_uart_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0); 
		cache_line_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
		data_dmem_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0));
end data_memory;

architecture behavioral of data_memory is

	type ram_type is array (0 to 2 ** (BLOCK_ADDRESS_WIDTH) - 1) of STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
	attribute ram_style : STRING;

	impure function initramfromfile (ramfilename : in STRING) return ram_type is
		file ramfile : text open READ_MODE is ramfilename;
		variable ramfileline : line;
		variable ram_content : ram_type;
	begin
		for i in 0 to 2 ** (BLOCK_ADDRESS_WIDTH+1) - 1 loop
		  if i >= (2**(BLOCK_ADDRESS_WIDTH)) then
            if not endfile(ramfile) then
                readline (ramfile, ramfileline);
                hread (ramfileline, ram_content(i-(2**(BLOCK_ADDRESS_WIDTH))));
            else
                ram_content(i-(2**(BLOCK_ADDRESS_WIDTH))) := (others => '0');
            end if;
          elsif not endfile(ramfile) then
	           readline (ramfile, ramfileline);
          end if;
		end loop;
		return ram_content;
 	end function;

	signal MDR : STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);

	subtype octet_t is NATURAL range 31 downto 0;

	signal octet : octet_t := 31;
	signal MAR, uart_address : unsigned(BLOCK_ADDRESS_WIDTH - 1 downto 0) := (others => '0');

	signal mem_write : STD_LOGIC := '0';
	
	--signal ram : ram_type := initramfromFile(RAM_FILENAME);
	signal ram : ram_type := (others => (others => '0'));
	attribute ram_style of ram : signal is "block";

    signal data_dmem : STD_LOGIC_VECTOR (BLOCK_SIZE-1 downto 0);


begin

	mem_write <= mem_write_i or mem_init_i;
	MAR <= unsigned(write_address_uart_i) when mem_init_i = '1' else unsigned(write_address_i);
	MDR <= uart_data_i when mem_init_i = '1' else cache_line_i;

    WRITE_ACCESS : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if mem_write = '1' then
                ram(to_integer(MAR)) <= MDR;
            end if;
        end if;
    end process;

	READ_ACCESS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			data_dmem <= ram(to_integer(unsigned(read_address_i)));
		end if;
	end process;
    
	data_dmem_o <= data_dmem;

end behavioral;
