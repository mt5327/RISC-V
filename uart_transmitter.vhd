library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity uart_transmitter is
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           uart_tx_enable_i : in STD_LOGIC;
           addr_i : in STD_LOGIC_VECTOR (63 downto 0);
           DOUT_i : in STD_LOGIC_VECTOR (7 downto 0);
           tx_o : out STD_LOGIC;
           uart_tx_busy_o : out STD_LOGIC);
end uart_transmitter;

architecture behavioral of uart_transmitter is

	signal counter : unsigned (9 downto 0) := (others => '0');

    signal tx_bit_counter : unsigned (2 downto 0) := "000";

    signal uart_tx_fin, uart_tx_enable, enable : STD_LOGIC;
    
	signal uart_clk, uart_clk_enable, sh_reg_enable, tx : STD_LOGIC;

	type state_type is (READY, START, DATA, STOP);
	signal state, next_state : state_type;

    signal tx_data : STD_LOGIC_VECTOR (9 downto 0);

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

	NEXT_STATE_DECODE : process (state, uart_clk, uart_tx_enable, tx_bit_counter)
	begin
		next_state <= state;
		case (state) is
			when READY =>
				if uart_tx_enable = '1' then
					next_state <= START;
				end if;
			when START =>
				if uart_clk = '1' then
				    next_state <= DATA;
				end if;
			when DATA =>
				if uart_clk = '1' and tx_bit_counter = "111" then
					next_state <= STOP;
				end if;
			when STOP =>
				if uart_clk = '1' then
					next_state <= READY;
				end if;
			when others => next_state <= READY;
		end case;
	end process;
	
	OUTPUT_DECODE : process (state, enable, uart_tx_enable_i)
	begin
		case state is
		    when READY =>
		        uart_tx_enable <= enable; 
		        uart_clk_enable <= '0';
		        sh_reg_enable <= '0';
			when START | DATA | STOP =>
				uart_tx_enable <= '0';
				uart_clk_enable <= '1';
				sh_reg_enable <= '1';
			when others =>
			    uart_tx_enable <= '0';
				uart_clk_enable <= '0';
				sh_reg_enable <= '0';
		end case;
	end process;
	
	SHIFT_REGISTER : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i ='1' then
                tx_data <= (others => '0');
            elsif uart_tx_enable = '1' then
                tx_data <= '1' & DOUT_i & '0';
            elsif sh_reg_enable = '1' then
                tx_data <= '0' & tx_data(9 downto 1);
                tx <= tx_data(0);
            end if;
        end if;
    end process;
    
    uart_tx_fin <= '1' when state = STOP and uart_clk = '1' else '0'; 
    tx_o <= tx;
    
    enable <= '1' when uart_tx_enable_i = '1' and addr_i = X"FFFFFFFFFFFFFFFF" else '0';
    uart_tx_busy_o <= '1' when uart_tx_enable and not uart_tx_fin else '0';
	
	UART_TX_COUNTER: process(clk_i)
	begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                tx_bit_counter <= "000";
            else 
                if uart_clk_enable = '1' and uart_clk = '1' and sh_reg_enable = '1' then
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
