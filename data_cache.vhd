library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity data_cache is
	generic (
		ADDRESS_WIDTH : NATURAL := 14;
		BLOCK_SIZE : NATURAL := 256;
		OFFSET_WIDTH : NATURAL;
		INDEX_WIDTH : NATURAL := 2);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_mem_i : in STD_LOGIC;

		memory_busy_i : in STD_LOGIC;
		cache_req_i : in CACHE_REQUEST (MAR(ADDRESS_WIDTH - 1 downto 0));

		mem_write_i : in STD_LOGIC;
		mem_write_o : out STD_LOGIC;

		cache_line_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
		read_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - OFFSET_WIDTH - 1 downto 0);

		write_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - OFFSET_WIDTH - 1 downto 0);
		data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
		data_o : out STD_LOGIC_VECTOR (63 downto 0);

		miss_o : out STD_LOGIC);
end data_cache;

architecture behavioral of data_cache is

	constant TAG_BITS : NATURAL := ADDRESS_WIDTH - INDEX_WIDTH - OFFSET_WIDTH;
	constant BLOCK_ADDRESS_WIDTH : NATURAL := ADDRESS_WIDTH - OFFSET_WIDTH;

	type cache_t is array (0 to 2 ** INDEX_WIDTH - 1) of STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
	signal cache : cache_t := (others => (others => '0'));

	type tags_t is array (0 to 2 ** INDEX_WIDTH - 1) of STD_LOGIC_VECTOR (TAG_BITS - 1 downto 0);
	signal tags : tags_t := (others => (others => '0'));

	signal valid : STD_LOGIC_VECTOR (2 ** INDEX_WIDTH - 1 downto 0) := (others => '0');
	signal dirty : STD_LOGIC_VECTOR (2 ** INDEX_WIDTH - 1 downto 0) := (others => '0');

	signal miss : STD_LOGIC := '0';
	signal mem_write, cache_write, WriteAllocate : STD_LOGIC := '0';

	alias tag : STD_LOGIC_VECTOR (TAG_BITS - 1 downto 0) is cache_req_i.MAR(ADDRESS_WIDTH - 1 downto INDEX_WIDTH + OFFSET_WIDTH);
	alias memory_address : STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0) is cache_req_i.MAR(ADDRESS_WIDTH - 1 downto OFFSET_WIDTH);

	alias block_address : STD_LOGIC_VECTOR(INDEX_WIDTH - 1 downto 0) is cache_req_i.MAR(INDEX_WIDTH + OFFSET_WIDTH - 1 downto OFFSET_WIDTH);
	signal cache_offset : INTEGER range 0 to 64 * OFFSET_WIDTH-3;

	type state_type is (CHECK, WRITE_BACK, WRITE_ALLOCATE);
	signal state, next_state : state_type := CHECK;

begin

	cache_offset <= to_integer(unsigned(cache_req_i.MAR(OFFSET_WIDTH - 1 downto 0))) * 64;

	SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				state <= CHECK;
				mem_write_o <= '0';
				else
				state <= next_state;
				mem_write_o <= mem_write;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, miss, memory_busy_i, block_address, dirty, enable_mem_i)
	begin
		next_state <= state;
		case state is
			when CHECK =>
				if miss = '1' and memory_busy_i = '0' and enable_mem_i = '1' then
					if dirty(to_integer(unsigned(block_address))) = '1' then
						next_state <= WRITE_BACK;
					else
						next_state <= WRITE_ALLOCATE;
					end if;
				end if;
			when WRITE_BACK => next_state <= WRITE_ALLOCATE;
			when WRITE_ALLOCATE => next_state <= CHECK;
			when others => next_state <= CHECK;
		end case;
	end process;

	OUTPUT_DECODE : process (state, miss, mem_write_i)
	begin
		mem_write <= '0';
		cache_write <= '0';
		WriteAllocate <= '0';
		case state is
			when CHECK =>
				if miss = '0' and mem_write_i = '1' then
					cache_write <= '1';
				end if;
			when WRITE_BACK => mem_write <= '1';
			when WRITE_ALLOCATE => WriteAllocate <= '1';
			when others => mem_write <= '0';
				WriteAllocate <= '0';
				cache_write <= '0';
		end case;
	end process;

	WRITE_TO_CACHE : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				valid <= (others => '0');
				dirty <= (others => '0');
				else
				if cache_write = '1' then
					for i in 0 to 7 loop
						if cache_req_i.we(i) = '1' then
							cache(to_integer(unsigned(block_address)))(cache_offset + i * 8 + 7 downto cache_offset + i * 8) <= cache_req_i.MDR(i * 8 + 7 downto i * 8);
						end if;
					end loop;
					dirty(to_integer(unsigned(block_address))) <= '1';
					elsif WriteAllocate = '1' then
					valid(to_integer(unsigned(block_address))) <= '1';
					dirty(to_integer(unsigned(block_address))) <= '0';
					tags(to_integer(unsigned(block_address))) <= tag;
					cache(to_integer(unsigned(block_address))) <= data_i;
				end if;
			end if;
		end if;
	end process;

	CHECK_MISS : process (state, valid, tag, tags, block_address)
	begin
		miss <= '1';
		if state = CHECK and valid(to_integer(unsigned(block_address))) = '1' and
			(tag xor tags(to_integer(unsigned(block_address)))) = (TAG_BITS - 1 downto 0 => '0') then
			miss <= '0';
		end if;
	end process;

	miss_o <= miss and enable_mem_i;
	read_address_o <= memory_address;
	write_address_o <= tags(to_integer(unsigned(block_address))) & cache_req_i.MAR(INDEX_WIDTH + OFFSET_WIDTH - 1 downto OFFSET_WIDTH);
	cache_line_o <= cache(to_integer(unsigned(block_address)));

	DOUBLE_WORD_SELECT : for i in 0 to OFFSET_WIDTH - 4 generate
		data_o <= cache(to_integer(unsigned(block_address)))(i * 64 + 63 downto i * 64) when unsigned(cache_req_i.MAR(OFFSET_WIDTH - 1 downto 0)) = i else (others => 'Z');
	end generate;
	
end behavioral;