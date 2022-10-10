library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity instruction_cache is
	generic (
		ADDRESS_WIDTH : NATURAL := 14;
		BLOCK_SIZE : NATURAL := 128;
		OFFSET_WIDTH : NATURAL;
		INDEX_WIDTH : NATURAL := 2);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;

		instr_address_i : in STD_LOGIC_VECTOR (ADDRESS_WIDTH - 1 downto 0);
		read_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - OFFSET_WIDTH - 1 downto 0);
		data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
		IR_o : out STD_LOGIC_VECTOR (63 downto 0);
		miss_o : out STD_LOGIC);
end instruction_cache;

architecture behavioral of instruction_cache is

	constant TAG_WIDTh : NATURAL := ADDRESS_WIDTH - INDEX_WIDTH - OFFSET_WIDTH;
	constant BLOCK_ADDRESS_WIDTH : NATURAL := ADDRESS_WIDTH - OFFSET_WIDTH;
	constant CACHE_SET_SIZE : NATURAL := BLOCK_SIZE + TAG_WIDTh;

	type state_type is (CHECK, WAIT_MEM, WRITE_TO_CACHE);
	signal state, next_state : state_type;

	type cache_t is array (0 to 2 ** INDEX_WIDTH - 1) of STD_LOGIC_VECTOR (CACHE_SET_SIZE downto 0);
	signal cache : cache_t := (others => (others => '0'));

	signal read_address : STD_LOGIC_VECTOR (BLOCK_ADDRESS_WIDTH - 1 downto 0);
	signal miss, tag_eq, check_miss, we : STD_LOGIC;

	alias tag : STD_LOGIC_VECTOR (TAG_WIDTh - 1 downto 0) is instr_address_i(instr_address_i'left downto INDEX_WIDTH + OFFSET_WIDTH);
	alias block_address : STD_LOGIC_VECTOR (INDEX_WIDTH - 1 downto 0) is instr_address_i(INDEX_WIDTH + OFFSET_WIDTH - 1 downto OFFSET_WIDTh);

begin

	read_address <= tag & block_address;
	tag_eq <= '1' when tag = cache(to_integer(unsigned(block_address)))(CACHE_SET_SIZE - 1 downto CACHE_SET_SIZE - TAG_WIDTh) else '0';
	check_miss <= '1' when state = CHECK else '0';
	we <= '1' when state = WRITE_TO_CACHE else '0';

	miss <= cache(to_integer(unsigned(block_address)))(CACHE_SET_SIZE) and tag_eq and check_miss;

	SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				state <= CHECK;
				else
				state <= next_state;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, miss)
	begin
		next_state <= state;
		case state is
			when CHECK =>
				if miss = '1' then
					next_state <= WAIT_MEM;
				end if;
			when WAIT_MEM => next_state <= WRITE_TO_CACHE;
			when WRITE_TO_CACHE => next_state <= CHECK;
			when others => next_state <= CHECK;
		end case;
	end process;

	CACHE_WRITE : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				cache <= (others => (others => '0'));
				else
				if we = '1' then
					cache(to_integer(unsigned(block_address))) <= '1' & tag & data_i;
				end if;
			end if;
		end if;
	end process;

	DOUBLE_WORD_SELECT : for i in 0 to OFFSET_WIDTH - 4 generate
		IR_o <= cache(to_integer(unsigned(block_address)))(i * 64 + 63 downto i * 64) when unsigned(instr_address_i(OFFSET_WIDTH - 1 downto 0)) = i else (others => 'Z');
	end generate;

	miss_o <= miss;
	read_address_o <= read_address;

end behavioral;