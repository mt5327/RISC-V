library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity memory_controller is
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           exception_i : in STD_LOGIC;
           mem_i : in MEMORY_ACCESS;
                             
           MAR_o : out unsigned (11 downto 0);           
           MDR_o : out STD_LOGIC_VECTOR (63 downto 0);  
                                
           DOUT_i : in STD_LOGIC_VECTOR (63 downto 0);
           
           Data_o : out STD_LOGIC_VECTOR (63 downto 0);
           we_o : out STD_LOGIC_VECTOR (7 downto 0); 
           memory_busy_o : out STD_LOGIC);
end memory_controller;

architecture Behavioral of memory_controller is

    type state_type is (IDLE, ALIGNED, WAIT1, UNALIGNED,  DONE);
    signal state, next_state : state_type;

    constant BYTE_SELECTOR : unsigned (15 downto 0) := X"0001";
    constant HALFWORD_SELECTOR : unsigned (15 downto 0) := X"0003";
    constant WORD_SELECTOR : unsigned (15 downto 0) := X"000F";
    constant DOUBLEWORD_SELECTOR : unsigned (15 downto 0) := X"00FF";
        
    signal we, we_high, we_low : STD_LOGIC_VECTOR (7 downto 0);
    signal we_mem : STD_LOGIC_VECTOR (15 downto 0);
    signal unaligned_access, enable_mem, busy, enable_mar_reg, enable_mdr_reg, enable_reg, enable_output_reg : STD_LOGIC := '0';
    signal reg_data : STD_LOGIC_VECTOR (63 downto 0);

    signal b : STD_LOGIC_VECTOR (7 downto 0);
    signal h : STD_LOGIC_VECTOR (15 downto 0);
    signal w : STD_LOGIC_VECTOR (31 downto 0);
    signal d, MDR_high, MDR_low, MDR : STD_LOGIC_VECTOR (63 downto 0);
    signal lb, lh, lw, Data_mem : STD_LOGIC_VECTOR (63 downto 0);
    signal data : STD_LOGIC_VECTOR (119 downto 0);
    signal MDR_reg, MDR_mem : STD_LOGIC_VECTOR (127 downto 0);
    signal MAR, MAR_reg, MAR_unaligned_reg : unsigned (11 downto 0);
    signal column : ram_column;
    signal length : STD_LOGIC_VECTOR (1 downto 0);
    signal is_unsigned : STD_LOGIC;
    
begin 
    
    enable_mem <= mem_i.enable_mem;
    column <= mem_i.column;
    length <= mem_i.access_type(1 downto 0);
    is_unsigned <= mem_i.access_type(2);
    

    SYNC_PROC: process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                state <= IDLE;
            else
                state <= next_state;
            end if;
        end if;
    end process;

    NEXT_STATE_DECODE: process (state, enable_mem, unaligned_access)
    begin
        next_state <= state;
        case (state) is
            when IDLE =>
                if enable_mem = '1' then
                    next_state <= ALIGNED;
                end if;
            when ALIGNED => next_state <= WAIT1;
            when WAIT1 =>
                if unaligned_access = '1' then
                    next_state <= UNALIGNED;                       
                else 
                    next_state <= DONE;
                end if;
            when UNALIGNED => next_state <= DONE;
            when DONE => next_state <= IDLE;
            when others => next_state <= IDLE;
        end case;
    end process;
    
    OUTPUT_DECODE: process (state, enable_mem, we_high, we_low, MDR_low, MDR_high)
    begin
        we <= X"00";
        MDR <= (others => '0');
        case (state) is  
            when IDLE =>
                busy <= enable_mem;
            when ALIGNED => 
                we <= we_low;
                MDR <= MDR_low;               
                busy <= '1';
            when WAIT1 => 
                busy <= '1'; 
            when UNALIGNED =>                
                we <= we_high;
                MDR <= MDR_high; 
                busy <= '1'; 
            when others => 
                busy <= '0';
        end case;
    end process;
    
    MAR_o <= MAR_unaligned_reg when state = UNALIGNED else MAR_reg;
    MDR_o <= MDR;

    data <= DOUT_i(55 downto 0) & reg_data;
    
    b <= reg_data(column * 8 + 7 downto column * 8);
    h <= reg_data(column * 8 + 15 downto column * 8) when column < 7 else data(column * 8 + 15 downto column * 8);
    w <= reg_data(column * 8 + 31 downto column * 8) when column < 5 else data(column * 8 + 31 downto column * 8);
    d <= reg_data when column = 0 else data(column * 8 + 63 downto column * 8);
    
    unaligned_access <= '1' when ( length = HALFWORD and column = 7 ) or
                                 ( length = WORD and column >= 5 ) or 
                                 ( length = DOUBLEWORD and column /= 0 ) else '0';
    
    
    process (length, column)
    begin 
        case length is 
            when BYTE => we_mem <= STD_LOGIC_VECTOR(shift_left(BYTE_SELECTOR, column));
            when HALFWORD => we_mem <= STD_LOGIC_VECTOR(shift_left(HALFWORD_SELECTOR, column));
            when WORD => we_mem <= STD_LOGIC_VECTOR(shift_left(WORD_SELECTOR, column));
            when DOUBLEWORD => we_mem <= STD_LOGIC_VECTOR(shift_left(DOUBLEWORD_SELECTOR, column));
            when others => we_mem <= X"0000";
        end case;
    end process;
    
    we_high <= we_mem(15 downto 8) when mem_i.write = '1' and exception_i = '0' else X"00";
    we_low <= we_mem(7 downto 0) when mem_i.write = '1' and exception_i = '0' else X"00";
    
    MDR_high <= MDR_reg(127 downto 64);
    MDR_low <= MDR_reg(63 downto 0);
        
    enable_mar_reg <= enable_mem when state = IDLE else '0';
    enable_mdr_reg <= mem_i.write when state = IDLE else '0';
    enable_reg <= '1' when state = WAIT1 and mem_i.write = '0' else '0';

    MAR_REGISTER: process(clk_i)
    begin
        if rising_edge(clk_i) then    
            if enable_mar_reg = '1' then   
                MAR_reg <= mem_i.MAR;
                MAR_unaligned_reg <= mem_i.MAR + 1;
            end if;
        end if;
    end process;

    MDR_mem <= STD_LOGIC_VECTOR(shift_left(resize(unsigned(mem_i.MDR), 128), 8 * column));
    MDR_REGISTER: process(clk_i)
    begin
        if rising_edge(clk_i) then    
            if enable_mdr_reg = '1' then   
                MDR_reg <= MDR_mem;
            end if;
        end if;
    end process;

    process(clk_i)
    begin
        if rising_edge(clk_i) then    
            if enable_reg = '1' then
                reg_data <= DOUT_i;
            end if;
        end if;
    end process;
   
    
    lb <= STD_LOGIC_VECTOR(resize(signed(b), 64)) when is_unsigned = '0' else STD_LOGIC_VECTOR(resize(unsigned(b), 64));
    lh <= STD_LOGIC_VECTOR(resize(signed(h), 64)) when is_unsigned = '0' else STD_LOGIC_VECTOR(resize(unsigned(h), 64)) ;
    lw <= STD_LOGIC_VECTOR(resize(signed(w), 64)) when is_unsigned = '0' else STD_LOGIC_VECTOR(resize(unsigned(w), 64)) ;

    with length select Data_mem <=
        lb when BYTE,
        lh when HALFWORD,
        lw when WORD,
        d when others;
 
    memory_busy_o <= busy;
    Data_o <= Data_mem;    
    we_o <= we;
    

end Behavioral;