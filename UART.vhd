library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity UART is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		rx_i : in STD_LOGIC;

		rx_error_o : out STD_LOGIC;
		cpu_enable_o : out STD_LOGIC;
		mem_write_o : out STD_LOGIC;
		DIN_o : out STD_LOGIC_VECTOR (3 downto 0));
end UART;

architecture behavioral of UART is

	constant MAX_VALUE : NATURAL := 868; -- ( 100 * 10^6 ) / 115200
	constant HALF_MAX_VALUE : NATURAL := MAX_VALUE / 2;
	
	signal counter : unsigned (9 downto 0) := (others => '0');
	signal uart_clk, uart_clk_enable, uart_rx_start, rx, rx_reg, cpu_enable : STD_LOGIC;

	type state_type is (READY, START, B0, B1, B2, B3, B4, B5, B6, B7, PARITY, STOP);
	signal state, next_state : state_type;

	signal rx_data : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal sh_reg_enable, mem_write, parity_error, parity_bit, rx_done, stop_bit, start_bit : STD_LOGIC := '0';

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
				when others => hex := "XXXX";
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

	UART_RX_CDC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			rx_reg <= rx_i;
			rx <= rx_reg;
		end if;
	end process;
	uart_clk <= '1' when counter = MAX_VALUE - 1 else '0';
	uart_rx_start <= '1' when counter = HALF_MAX_VALUE - 1 and start_bit = '1' else '0';
	rx_done <= uart_clk and stop_bit;
	mem_write <= '1' when unsigned(rx_data) >= 48 else '0';
	cpu_enable <= '1' when rx_data = X"04" and uart_clk_enable = '0' and parity_error = '0' else '0';
	-- synthesis translate_off
	-- cpu_enable <= '1';
	-- synthesis translate_on
	SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				state <= READY;
				cpu_enable_o <= '0';
				mem_write_o <= '0';
			else
				state <= next_state;
				cpu_enable_o <= cpu_enable;
				mem_write_o <= rx_done and mem_write;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, uart_clk, uart_rx_start, rx, parity_error)
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
						next_state <= B0;
					else next_state <= READY;
					end if;
				end if;
			when B0 =>
				if uart_clk = '1' then
					next_state <= B1;
				end if;
			when B1 =>
				if uart_clk = '1' then
					next_state <= B2;
				end if;
			when B2 =>
				if uart_clk = '1' then
					next_state <= B3;
				end if;
			when B3 =>
				if uart_clk = '1' then
					next_state <= B4;
				end if;
			when B4 =>
				if uart_clk = '1' then
					next_state <= B5;
				end if;
			when B5 =>
				if uart_clk = '1' then
					next_state <= B6;
				end if;
			when B6 =>
				if uart_clk = '1' then
					next_state <= B7;
				end if;
			when B7 =>
				if uart_clk = '1' then
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
			when B0 | B1 | B2 | B3 | B4 | B5 | B6 | B7 =>
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

	DIN_o <= to_hex(rx_data);

	-- EVEN PARITY    
	PARITY_CHECK : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or cpu_enable = '1' then
				parity_error <= '0';
			else
				if uart_clk = '1' and parity_bit = '1' then
					parity_error <= (xor rx_data) xor rx;
				end if;
			end if;
		end if;
	end process;

	rx_error_o <= parity_error;

end behavioral;