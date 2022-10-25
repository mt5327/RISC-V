library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity divider_fp is
    Generic (SIZE : natural);
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           enable_i : in STD_LOGIC;
           x_i : in STD_LOGIC_VECTOR (SIZE-1 downto 0);
           y_i : in STD_LOGIC_VECTOR (SIZE-1 downto 0);
           z_o : out STD_LOGIC_VECTOR (2 * SIZE  - 1 downto 0);
           op_i : in ALU_OP;
           div_valid_o : out STD_LOGIC);
end divider_fp;

architecture behavioral of divider_fp is
    
    signal y : STD_LOGIC_VECTOR(SIZE-1 downto 0);
    signal PR, PR_new : STD_LOGIC_VECTOR (SIZE downto 0);
    signal q : STD_LOGIC_VECTOR (SIZE-1 downto 0);
    signal counter : unsigned (5 downto 0);
    signal sign : STD_LOGIC;
       
    type state_type is (IDLE, DIVIDE, FINALIZE);
    signal state, next_state : state_type;
    
    signal overflow, finish : STD_LOGIC;
     
begin

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
    
    NEXT_STATE_DECODE: process (state, enable_i, counter)
    begin
        next_state <= state;
        case (state) is
            when IDLE => if enable_i = '1' then next_state <= DIVIDE; end if;
            when DIVIDE => if ( or counter ) = '0' then next_state <= FINALIZE; end if; 
            when others => next_state <= IDLE;
        end case;
    end process;        

    PR_new <= STD_LOGIC_VECTOR(unsigned(PR) - unsigned('0' & y));

    DIVISION: process(clk_i)
    begin
        if rising_edge(clk_i) then
            case state is
                when IDLE => 
                    if enable_i = '1' then
                        q <= x_i(SIZE-2 downto 0) & '0';  
                        y <= y_i; 
                        PR <= (0 => x_i(SIZE-1), others => '0');
                        counter <= to_unsigned(SIZE-1,  counter'length);
                    end if;
                when DIVIDE =>            
                    q <= q(SIZE-2 downto 0) & ( not PR_new(SIZE) );
                    if PR_new(SIZE) = '0' then
                        PR <= PR_new(SIZE-1 downto 0) & q(SIZE-1);
                    else 
                        PR <= PR(SIZE-1 downto 0) & q(SIZE-1);                    
                    end if;
                    counter <= counter - 1;
                when others => 
            end case;
        end if;
    end process;
        
    z_o <= q & PR(SIZE downto 1);
    div_valid_o <= '1' when state = FINALIZE else '0';
    
end behavioral;
