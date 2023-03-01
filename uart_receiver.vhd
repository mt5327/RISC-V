library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity uart_receiver is
    Generic ( BLOCK_SIZE : NATURAL; 
              BLOCK_ADDRESS_WIDTH : NATURAL);
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           exception_i : in STD_LOGIC;
           rx_i : in STD_LOGIC;
           cpu_enable_i : in STD_LOGIC;
          -- cpu_enable_o : out STD_LOGIC;
           mem_init_imem_o : out STD_LOGIC;
           mem_init_dmem_o : out STD_LOGIC;
           uart_data_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
           write_address_uart_o : out STD_LOGIC_VECTOR (BLOCK_ADDRESS_WIDTH-1 downto 0);
           rx_error_o : out STD_LOGIC);
end uart_receiver;

architecture behavioral of uart_receiver is

	signal counter : unsigned (9 downto 0) := (others => '0');

	signal uart_reg : STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);

	subtype octet_t is NATURAL range 63 downto 0;

	signal octet : octet_t := 63;
    signal rx_bit_counter : unsigned (2 downto 0) := "000";

	signal rx_data : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal sh_reg_enable, reg_write, mem_write, mem_write_reg, parity_error, parity_bit, rx_done, stop_bit, start_bit : STD_LOGIC := '0';

	signal uart_clk, uart_clk_enable, uart_rx_start : STD_LOGIC := '0';
    signal rx, rx_reg : STD_LOGIC := '1';
	signal uart_address : unsigned(BLOCK_ADDRESS_WIDTH downto 0) := (others => '0');

	type state_type is (READY, START, DATA, PARITY, STOP);
	signal state, next_state : state_type;

	function to_hex(char : STD_LOGIC_VECTOR(7 downto 0)) return STD_LOGIC_VECTOR is
		variable hex : STD_LOGIC_VECTOR (3 downto 0);
	begin
		if char(7 downto 4) = X"3" then
			hex := char(3 downto 0);
		else
			case char is
				when X"41" | X"61" => hex := "1010";
				when X"42" | X"62" => hex := "1011";
				when X"43" | X"63" => hex := "1100";
				when X"44" | X"64" => hex := "1101";
				when X"45" | X"65" => hex := "1110";
				when X"46" | X"66" => hex := "1111";
				when others => hex := "0000";
			end case;
		end if;
		return hex;
	end function;

begin

	PRESCALER : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				counter <= (others => '0');
			else
				if uart_clk_enable = '1' then
					if uart_clk = '1' then
						counter <= (others => '0');
					else
			 			counter <= counter + 1;
					end if;
				else
					counter <= (others => '0');
				end if;
			end if;
		end if;
	end process;
	
    SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				state <= READY;
			else
				state <= next_state;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, uart_clk, uart_rx_start, rx_bit_counter, rx, parity_error)
	begin
		next_state <= state;
		case (state) is
			when READY =>
				if rx = '0' and parity_error = '0' then
					next_state <= START;
				end if;
			when START =>
				if uart_rx_start = '1' then
					if rx = '0' then
						next_state <= DATA;
					else next_state <= READY;
					end if;
				end if;
			when DATA =>
				if uart_clk = '1' and rx_bit_counter = "111" then
					next_state <= PARITY;
				end if;
			when PARITY =>
				if uart_clk = '1' then
					next_state <= STOP;
				end if;
			when STOP =>
				if uart_clk = '1' then
					next_state <= READY;
				end if;
			when others => next_state <= READY;
		end case;
	end process;

	OUTPUT_DECODE : process (state)
	begin
		case state is
			when START =>
				uart_clk_enable <= '1';
				start_bit <= '1';
				sh_reg_enable <= '0';
				parity_bit <= '0';
				stop_bit <= '0';
			when DATA =>
				uart_clk_enable <= '1';
				start_bit <= '0';
				sh_reg_enable <= '1';
				parity_bit <= '0';
				stop_bit <= '0';
			when PARITY =>
				uart_clk_enable <= '1';
				start_bit <= '0';
				sh_reg_enable <= '0';
				parity_bit <= '1';
				stop_bit <= '0';
			when STOP =>
				uart_clk_enable <= '1';
				start_bit <= '0';
				sh_reg_enable <= '0';
				parity_bit <= '0';
				stop_bit <= '1';
			when others =>
				uart_clk_enable <= '0';
				start_bit <= '0';
				sh_reg_enable <= '0';
				parity_bit <= '0';
				stop_bit <= '0';
		end case;
	end process;

	SHIFT_REGISTER : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or (uart_rx_start = '1' and rx = '0') then
				rx_data <= (others => '0');
			else
				if uart_clk = '1' and sh_reg_enable = '1' then
					rx_data <= rx & rx_data(7 downto 1);
				end if;
			end if;
		end if;
	end process;
	
	UART_RX_COUNTER: process(clk_i)
	begin
        if rising_edge(clk_i) then
            if rst_i = '1' or state = READY then
                rx_bit_counter <= "000";
            else 
                if uart_clk = '1' and sh_reg_enable = '1' then
                    if rx_bit_counter = "111" then
                        rx_bit_counter <= (others => '0');
                    else
                        rx_bit_counter <= rx_bit_counter + 1;
                    end if;
                end if;
            end if;    
        end if;
	end process;
	
	UART_RX_CDC : process (clk_i)
	begin
		if rising_edge(clk_i) then
		  if rst_i = '1' then
		      rx_reg <= '1';
		      rx <= '1';
		  else
    	      rx_reg <= rx_i;
	   		  rx <= rx_reg;
	       end if;
		end if;  
	end process;
	
	-- EVEN PARITY    
	PARITY_CHECK : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				parity_error <= '0';
			else
				if uart_clk = '1' and parity_bit = '1' then
					parity_error <= (xor rx_data) xor rx;
				end if;
			end if;
		end if;
	end process;
	
	UART_REGISTER : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or cpu_enable_i = '1' or exception_i = '1' then
				uart_address <= (others => '0');
				octet <= 63;
				mem_write <= '0';
			else
				if reg_write = '1' then
					uart_reg(octet * 4 + 3 downto octet * 4) <= to_hex(rx_data);
					if octet = 0 then
						octet <= 63;
					    mem_write <= '1';
					else
						octet <= octet - 1;
					end if;
				elsif mem_write = '1' then
					uart_address <= uart_address + 1;
					mem_write <= '0';
				end if;
			end if;
		end if;
	end process;

	uart_clk <= '1' when counter = MAX_VALUE - 1 else '0';
	uart_rx_start <= '1' when counter = HALF_MAX_VALUE - 1 and start_bit = '1' else '0';
	rx_done <= uart_clk and stop_bit;

	reg_write <= '1' when unsigned(rx_data) >= 48 and rx_done = '1' else '0';
	--cpu_enable <= '1' when (rx_data = X"04" and uart_clk_enable = '0' and parity_error = '0') or cpu_enable_i = '1' else '0';

 --   cpu_enable_o <= cpu_enable;

	rx_error_o <= parity_error;
    mem_init_imem_o <= mem_write and not uart_address(BLOCK_ADDRESS_WIDTH);
    mem_init_dmem_o <= mem_write and uart_address(BLOCK_ADDRESS_WIDTH);
    write_address_uart_o <= STD_LOGIC_VECTOR(uart_address(BLOCK_ADDRESS_WIDTH-1 downto 0));
    uart_data_o <= uart_reg;
	
end behavioral;
