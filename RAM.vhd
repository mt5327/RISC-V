library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use std.textio.all;
use work.constants.all;

entity RAM is
	generic (
		RAM_FILENAME : STRING := "C:\\cygwin64\\home\\Mitja\\uart_test\\main.hex";
		BLOCK_ADDRESS_WIDTH : NATURAL;
		BLOCK_SIZE : NATURAL);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;

		exception_i : in STD_LOGIC;
		mem_init_i : in STD_LOGIC;
		mem_write_i : in STD_LOGIC;

		UART_data_i : in STD_LOGIC_VECTOR (3 downto 0);
		read_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
		write_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);

		cache_line_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
		DOUT_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0));
end entity;

architecture behavioral of RAM is

	type ram_type is array (0 to 2 ** (BLOCK_ADDRESS_WIDTH) - 1) of STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
	-- signal ram : ram_type := (others => (others => '0'));
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

	attribute ram_style : STRING;
	attribute ram_style of ram : signal is "block";

	signal DOUT, MDR, uart_reg : STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);

	subtype octet_t is NATURAL range 31 downto 0;

	signal octet : octet_t := 31;
	signal MAR, uart_address : unsigned(BLOCK_ADDRESS_WIDTH - 1 downto 0) := (others => '0');

	signal mem_write, uart_src : STD_LOGIC;

begin

	mem_write <= ( mem_write_i or uart_src );
	MAR <= uart_address when uart_src = '1' else unsigned(write_address_i);
	MDR <= uart_reg when uart_src = '1' else cache_line_i;

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
			DOUT <= ram(to_integer(unsigned(read_address_i)));
		end if;
	end process;

	UART_REGISTER : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or exception_i = '1' then
				uart_address <= (others => '0');
				octet <= 31;
				uart_src <= '0';
			else
				if mem_init_i = '1' then
					uart_reg(octet * 4 + 3 downto octet * 4) <= UART_data_i;
					if octet = 0 then
						octet <= 31;
						uart_src <= '1';
					else
						octet <= octet - 1;
					end if;
				elsif uart_src = '1' then
					uart_address <= uart_address + 1;
					uart_src <= '0';
				end if;
			end if;
		end if;
	end process;

	DOUT_o <= DOUT;
end behavioral;