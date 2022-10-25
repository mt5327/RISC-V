library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mul_dsp is
    Generic (X_SIZE : natural; Y_SIZE : natural);
    Port( clk_i: in STD_LOGIC;
          enable_i : in STD_LOGIC;
          x_i: in unsigned (X_SIZE-1 downto 0);
          y_i: in unsigned (Y_SIZE-1 downto 0);
          result_o: out unsigned (X_SIZE + Y_SIZE - 1 downto 0));
end mul_dsp;

architecture behavioral of mul_dsp is
        
    signal X : unsigned(X_SIZE - 1 downto 0);
    signal Y : unsigned(Y_SIZE - 1 downto 0);
    signal P : unsigned(X_SIZE + Y_SIZE - 1 downto 0);
        
    signal product : unsigned(X_SIZE + Y_SIZE - 1 downto 0);    
                   
begin
    
    MULTIPLICATION: process(clk_i)
    begin
        if rising_edge(clk_i) then           
            if enable_i = '1' then    
                X <= unsigned(x_i);
                Y <= unsigned(y_i);
            end if;
            product <= X * Y;    
        end if;
    end process;
    
    result_o <= product;

end behavioral;