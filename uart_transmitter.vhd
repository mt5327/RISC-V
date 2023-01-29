library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity uart_transmitter is
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           uart_tx_enable_i : in STD_LOGIC;
           DOUT_i : in STD_LOGIC_VECTOR (7 downto 0);
           tx_o : out STD_LOGIC;
           uart_tx_busy_o : out STD_LOGIC);
end uart_transmitter;

architecture behavioral of uart_transmitter is

	signal counter : unsigned (9 downto 0) := (others => '0');

    signal tx_bit_counter : unsigned (2 downto 0) := "000";

    signal uart_tx_fin : STD_LOGIC;
    
	signal uart_clk, uart_clk_enable, uart_tx_data_bits : STD_LOGIC;
    signal tx : STD_LOGIC := '1';

	type state_type is (READY, START, DATA, PARITY, STOP);
	signal state, next_state : state_type;

    signal tx_data : STD_LOGIC_VECTOR (10 downto 0) := (others => '1');

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

	NEXT_STATE_DECODE : process (state, uart_clk, uart_tx_enable_i, tx_bit_counter)
	begin
		next_state <= state;
		case (state) is
			when READY =>
				if uart_tx_enable_i = '1' then
			       next_state <= START;
				end if;
			when START =>
				if uart_clk = '1' then
				   next_state <= DATA;
				end if;
			when DATA =>
				if uart_clk = '1' and tx_bit_counter = "111" then
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
	
	OUTPUT_DECODE : process (state, uart_tx_enable_i)
	begin
		case state is
		    when READY =>
		        uart_tx_data_bits <= '0';
		        uart_clk_enable <= '0';
			when START | PARITY | STOP =>
			    uart_tx_data_bits <= '0';
				uart_clk_enable <= '1';
		    when DATA =>
			    uart_tx_data_bits <= '1';
				uart_clk_enable <= '1';		      
			when others =>
			    uart_tx_data_bits <= '0';
				uart_clk_enable <= '0';
		end case;
	end process;
	
	SHIFT_REGISTER : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i ='1' then
                tx <= '1';
                tx_data <= (others => '1');
            else
                if state = READY then
                    if uart_tx_enable_i = '1' then
                        tx_data <= '1' & ( (xor DOUT_i) xor '0' ) & DOUT_i & '0';
                    end if; 
                elsif uart_clk_enable = '1' then
                    if uart_clk = '1' then
                        tx_data <= '1' & tx_data(10 downto 1);
                    end if;
                end if;
                tx <= tx_data(0);
            end if;
        end if;
    end process;
    
    uart_tx_fin <= '1' when state = STOP and uart_clk = '1' else '0'; 
    tx_o <= tx;
    
    uart_tx_busy_o <= '1' when uart_tx_enable_i and not uart_tx_fin else '0';
	
	UART_TX_COUNTER: process(clk_i)
	begin
        if rising_edge(clk_i) then
            if rst_i = '1' or state = READY then
                tx_bit_counter <= "000";
            else 
                if uart_clk = '1' and uart_tx_data_bits = '1' then
                    if tx_bit_counter = "111" then
                        tx_bit_counter <= (others => '0');
                    else
                        tx_bit_counter <= tx_bit_counter + 1;
                    end if;
                end if;
            end if;    
        end if;
	end process;
	
	uart_clk <= '1' when counter = MAX_VALUE - 1 else '0';

end behavioral;
