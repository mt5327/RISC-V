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
	    MAR_i : in STD_LOGIC_VECTOR (ADDRESS_WIDTH - 1 downto 0);
		MDR_i : in STD_LOGIC_VECTOR (63 downto 0);
	    memory_operation_i : in MEM_OP;	
		cache_req_o : out CACHE_REQUEST (MAR(ADDRESS_WIDTH - 4 downto 0));
		unaligned_access_o : out STD_LOGIC;
		miss_i : in STD_LOGIC;
		data_i : in STD_LOGIC_VECTOR (63 downto 0);
		data_o : out STD_LOGIC_VECTOR (63 downto 0));
end load_store_unit;

architecture behavioral of load_store_unit is
    
	signal we : STD_LOGIC_VECTOR (7 downto 0);
	signal unaligned_access, unaligned, is_signed : STD_LOGIC := '0';
	signal reg_data, data_mem : STD_LOGIC_VECTOR (63 downto 0);
	signal b : STD_LOGIC_VECTOR (7 downto 0);
	signal h : STD_LOGIC_VECTOR (15 downto 0);
	signal w : STD_LOGIC_VECTOR (31 downto 0);
	signal lb, lh, lw, d, MDR : STD_LOGIC_VECTOR (63 downto 0);
	signal addr, unaligned_address, unaligned_address_reg : STD_LOGIC_VECTOR (ADDRESS_WIDTH - 4 downto 0);
    signal int_column : integer range 0 to 7;
        
    alias column : STD_LOGIC_VECTOR (2 downto 0) is MAR_i(2 downto 0);
    
begin

	CHECK_UNALIGNED : process (memory_operation_i, column)
	begin
		unaligned <= '0';
		case memory_operation_i is
			when LSU_LH | LSU_LHU | LSU_SH => unaligned <= and column;
			when LSU_LW | LSU_LWU | LSU_SW => unaligned <= column(2) and (or column(1 downto 0));
			when LSU_LD | LSU_SD => unaligned <= or column;
			when others => unaligned <= '0';
		end case;
	end process;
    
    int_column <= to_integer(unsigned(MAR_i(2 downto 0)));
	
	BYTE_SELECT : process (all)
	begin
	    we <= (others => '0');
        case memory_operation_i is
            when LSU_SB => we(int_column) <= '1';
            when LSU_SH =>
                if unaligned_access = '0' then
                    case int_column is
                        when 7 => we <= X"80";
                        when others => we(int_column + 1 downto int_column) <= "11";
                    end case;
                else
                    we <= X"01";
                end if;
            when LSU_SW | LSU_FSW =>
                if unaligned_access = '0' then
                    case int_column is
                        when 5 => we <= X"E0";
                        when 6 => we <= X"C0";
                        when 7 => we <= X"80";
                        when others => we(int_column + 3 downto int_column) <= "1111";
                    end case;
                else
                    case int_column is
                        when 5 => we <= X"01";
                        when 6 => we <= X"03";
                        when 7 => we <= X"07";
                        when others => we <= X"00";
                    end case;
                end if;
            when LSU_SD | LSU_FSD =>
                if unaligned_access = '0' then
                    case int_column is
                        when 0 => we <= X"FF";
                        when 1 => we <= X"FE";
                        when 2 => we <= X"FC";
                        when 3 => we <= X"F8";
                        when 4 => we <= X"F0";
                        when 5 => we <= X"E0";
                        when 6 => we <= X"C0";
                        when 7 => we <= X"80";
                        when others => we <= X"00";   
                    end case;             
                else
                    case int_column is
                        when 1 => we <= X"01";
                        when 2 => we <= X"03";
                        when 3 => we <= X"07";
                        when 4 => we <= X"0F";
                        when 5 => we <= X"1F";
                        when 6 => we <= X"3F";
                        when 7 => we <= X"7F";
                        when others => we <= X"00";
                    end case;
                end if;
            when others => we <= X"00";
        end case;
	end process;

	MDR_SELECT : process (column, MDR_i)
	begin
		case column is 
			when "000" => MDR <= MDR_i;
			when "001" => MDR <= MDR_i(55 downto 0) & MDR_i(63 downto 56);
			when "010" => MDR <= MDR_i(47 downto 0) & MDR_i(63 downto 48);
			when "011" => MDR <= MDR_i(39 downto 0) & MDR_i(63 downto 40);
			when "100" => MDR <= MDR_i(31 downto 0) & MDR_i(63 downto 32);
			when "101" => MDR <= MDR_i(23 downto 0) & MDR_i(63 downto 24);
			when "110" => MDR <= MDR_i(15 downto 0) & MDR_i(63 downto 16);
			when "111" => MDR <= MDR_i(7 downto 0) & MDR_i(63 downto 8);
			when others => MDR <= (others => '0'); 
		end case;
	end process;

	DATA_REGISTER : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if unaligned = '1' and miss_i = '0' and unaligned_access = '0' then
				reg_data <= data_i;
			end if;
		end if;
	end process;

    process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' or exception_i = '1' then
                unaligned_access <= '0';
                unaligned_address_reg <= (others => '0'); 
            else
                if miss_i = '0' then      
                    if unaligned = '1' and unaligned_access = '0' then
                        unaligned_access <= '1';
                        unaligned_address_reg <= unaligned_address;
                    else 
                        unaligned_address_reg <= (others => '0');
                        unaligned_access <= '0';
                    end if;
                end if;
            end if;
        end if;  
    end process;

    with column select
        b <= data_i(7 downto 0) when "000",
             data_i(15 downto 8) when "001",
             data_i(23 downto 16) when "010",
             data_i(31 downto 24) when "011",
             data_i(39 downto 32) when "100",
             data_i(47 downto 40) when "101",
             data_i(55 downto 48) when "110",
	         data_i(63 downto 56) when "111",
	         (others => '0') when others;	    	

    with column select
        h <= data_i(15 downto 0) when "000",
             data_i(23 downto 8) when "001",
             data_i(31 downto 16) when "010",
             data_i(39 downto 24) when "011",
             data_i(47 downto 32) when "100",
             data_i(55 downto 40) when "101",
             data_i(63 downto 48) when "110",
             data_i(7 downto 0) & reg_data(63 downto 56) when "111",
	         (others => '0') when others;	
	  

	with column select 
        w <= data_i(31 downto 0) when "000",
             data_i(39 downto 8) when "001",
             data_i(47 downto 16) when "010",
             data_i(55 downto 24) when "011",
             data_i(63 downto 32) when "100",
             data_i(7 downto 0) & reg_data(63 downto 40) when "101",
             data_i(15 downto 0) & reg_data(63 downto 48) when "110",
             data_i(23 downto 0) & reg_data(63 downto 56) when "111",
             (others => '0') when others;
	
    with column select 
        d <= data_i when "000",
             data_i(7 downto 0) & reg_data(63 downto 8) when "001",
             data_i(15 downto 0) & reg_data(63 downto 16) when "010",
             data_i(23 downto 0) & reg_data(63 downto 24) when "011",
             data_i(31 downto 0) & reg_data(63 downto 32) when "100",
             data_i(39 downto 0) & reg_data(63 downto 40) when "101",
             data_i(47 downto 0) & reg_data(63 downto 48) when "110",
             data_i(55 downto 0) & reg_data(63 downto 56) when "111",
             (others => '0') when others;
             
	with memory_operation_i select
	   is_signed <= '1' when LSU_LB | LSU_LH | LSU_LW,
	 	            '0' when others;
	
    lb <= (63 downto 8 => ( b(7) and is_signed ) ) & b;
	lh <= (63 downto 16 => ( h(15) and is_signed ) ) & h;
	lw <= (63 downto 32 => ( w(31) and is_signed ) ) & w;

	with memory_operation_i select
	   data_mem <= lb when LSU_LB | LSU_LBU,
		           lh when LSU_LH | LSU_LHU,
		           lw when LSU_LW | LSU_LWU,
		           (63 downto 32 => '1') & w when LSU_FLW,
		           d when LSU_FLD | LSU_LD,
		           (others => '0') when others;

    unaligned_address <= STD_LOGIC_VECTOR(unsigned(MAR_i(ADDRESS_WIDTH-1 downto 3)) + 1);
	unaligned_access_o <= unaligned and (not unaligned_access);

	data_o <= data_mem;
	
	addr <= MAR_i(ADDRESS_WIDTH-1 downto 3) when unaligned_access = '0' else unaligned_address_reg;
	cache_req_o.MAR <= addr;

	cache_req_o.MDR <= MDR;
	cache_req_o.we <= we;

end behavioral;