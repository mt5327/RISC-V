library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity load_store_unit is
	generic (ADDRESS_WIDTH : NATURAL := 14);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		exception_i : in STD_LOGIC;
		mem_req_i : in MEMORY_REQUEST (MAR(ADDRESS_WIDTH - 1 downto 0));
		cache_req_o : out CACHE_REQUEST (MAR(ADDRESS_WIDTH - 1 downto 0));

		data_i : in STD_LOGIC_VECTOR (63 downto 0);
		data_o : out STD_LOGIC_VECTOR (63 downto 0));
end load_store_unit;

architecture behavioral of load_store_unit is

	signal we : STD_LOGIC_VECTOR (7 downto 0);
	signal unaligned_access, unaligned, is_signed : STD_LOGIC := '0';
	signal reg_data : STD_LOGIC_VECTOR (63 downto 0);
	signal data : STD_LOGIC_VECTOR (119 downto 0);

	signal b : STD_LOGIC_VECTOR (7 downto 0);
	signal h : STD_LOGIC_VECTOR (15 downto 0);
	signal w : STD_LOGIC_VECTOR (31 downto 0);
	signal lb, lh, lw, d, Data_mem, MDR : STD_LOGIC_VECTOR (63 downto 0);
	signal unaligned_address, unaligned_address_reg : STD_LOGIC_VECTOR (ADDRESS_WIDTH - 1 downto 0);
    signal halfword_unaligned : STD_LOGIC;
    signal int_column : integer range 0 to 7;
    
    alias column : STD_LOGIC_VECTOR (2 downto 0) is mem_req_i.MAR(2 downto 0);
    
begin

	CHECK_UNALIGNED : process (mem_req_i.MEMOp)
	begin
		unaligned <= '0';
		case mem_req_i.MEMOp is
			when LSU_LH | LSU_LHU | LSU_SH => unaligned <= and column;
			when LSU_LW | LSU_LWU | LSU_SW => unaligned <= column(2) and (or column(1 downto 0));
			when LSU_LD | LSU_SD => unaligned <= or column;
			when others => unaligned <= '0';
		end case;
	end process;

    int_column <= to_integer(unsigned(mem_req_i.MAR(2 downto 0)));
	BYTE_SELECT : process (all)
	begin
		if mem_req_i.enable_mem = '1' then
			we <= X"00";
			case mem_req_i.MEMOp is
				when LSU_SB => we(int_column) <= '1';
				when LSU_SH =>
					if unaligned_access = '0' then
						we(int_column + 1 downto int_column) <= "11";
					else
						we <= X"01";
					end if;
				when LSU_SW =>
					if unaligned_access = '0' then
						we(int_column + 3 downto int_column) <= "1111";
					else
						case int_column is
							when 5 => we <= X"01";
							when 6 => we <= X"03";
							when others => we <= X"07";
						end case;
					end if;
				when LSU_SD =>
					if unaligned_access = '0' then
						we <= (others => '1');
					else
						case int_column is
							when 1 => we <= X"01";
							when 2 => we <= X"03";
							when 3 => we <= X"07";
							when 4 => we <= X"0F";
							when 5 => we <= X"1F";
							when 6 => we <= X"3F";
							when others => we <= X"7F";
						end case;
					end if;
				when others => we <= X"00";
			end case;
		else
			we <= X"00";
		end if;
	end process;

	MDR_SELECT : process (column, mem_req_i.MDR)
	begin
		case column is
			when "000" => MDR <= mem_req_i.MDR;
			when "001" => MDR <= mem_req_i.MDR(55 downto 0) & mem_req_i.MDR(63 downto 56);
			when "010" => MDR <= mem_req_i.MDR(47 downto 0) & mem_req_i.MDR(63 downto 48);
			when "011" => MDR <= mem_req_i.MDR(39 downto 0) & mem_req_i.MDR(63 downto 40);
			when "100" => MDR <= mem_req_i.MDR(31 downto 0) & mem_req_i.MDR(63 downto 32);
			when "101" => MDR <= mem_req_i.MDR(23 downto 0) & mem_req_i.MDR(63 downto 24);
			when "110" => MDR <= mem_req_i.MDR(15 downto 0) & mem_req_i.MDR(63 downto 16);
			when "111" => MDR <= mem_req_i.MDR(7 downto 0) & mem_req_i.MDR(63 downto 8);
			when others => MDR <= (others => '0');
		end case;
	end process;

	DATA_REGISTER : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if unaligned = '1' then
				reg_data <= data_i;
			end if;
		end if;
	end process;

	process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or exception_i = '1' then
				unaligned_access <= '0';
			else
				if mem_req_i.enable_mem = '1' then
					unaligned_access <= unaligned;
					unaligned_address_reg <= unaligned_address;
				end if;
			end if;
		end if;
	end process;

	data <= data_i(55 downto 0) & reg_data;

	b <= data_i(int_column * 8 + 7 downto int_column * 8);		
--	with int_column select 
--	   h <= data_i(15 downto 0) when 0,
--	        data_i(23 downto 8) when 1,
--	        data_i(31 downto 16) when 2,
--	        data_i(39 downto 24) when 3,
--	        data_i(47 downto 32) when 4,
--	        data_i(55 downto 40) when 5,  
--	        data_i(63 downto 48) when 6,
--	        data(71 downto 56) when 7,                         
--	        (others => '0') when others;
	    	
	h <= data_i(int_column * 8 + 15 downto int_column * 8) when unaligned = '0' else 
	     data(71 downto 56);
	w <= data_i(int_column * 8 + 31 downto int_column * 8) when unaligned = '0' else 
	     data(int_column * 8 + 31 downto int_column * 8);
	d <= data_i when unaligned = '0' else data(int_column * 8 + 63 downto int_column * 8);

--    with mem_req_i.column select 
--        d <= data_i when 0,
--             data(71 downto 8) when 1,
--             data(79 downto 16) when 2,
--             data(87 downto 24) when 3,
--             data(95 downto 32) when 4,
--             data(103 downto 40) when 5,
--             data(111 downto 48) when 6,
--             data(119 downto 56) when 7,
--             (others => '0') when others;
             
	with mem_req_i.MEMOp select
	   is_signed <= '1' when LSU_LB | LSU_LH | LSU_LW,
	 	            '0' when others;
	
    lb <= (63 downto 8 => ( b(7) and is_signed ) ) & b;
	lh <= (63 downto 16 => ( h(15) and is_signed ) ) & h;
	lw <= (63 downto 32 => ( w(31) and is_signed ) ) & w;

	with mem_req_i.MEMOp select
	   data_mem <= lb when LSU_LB | LSU_LBU,
		           lh when LSU_LH | LSU_LHU,
		           lw when LSU_LW | LSU_LWU,
		           d when LSU_LD,
		           (others => '0') when others;

	unaligned_address <= STD_LOGIC_VECTOR(unsigned(mem_req_i.MAR) + 1);
	
	cache_req_o.MAR <= mem_req_i.MAR when unaligned_access = '0' else
	                   unaligned_address_reg;

	cache_req_o.MDR <= MDR;
	cache_req_o.we <= we;

	data_o <= data_mem;

end behavioral;