library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity clz is
    generic ( SIZE : NATURAL := 64 );
    port ( x_i : in STD_LOGIC_VECTOR (SIZE-1 downto 0);
           z_o : out unsigned (num_bits(SIZE)-1 downto 0));
end clz;

architecture Behavioral of clz is

    signal d1 : STD_LOGIC_VECTOR (31 downto 0);
    signal d2 : STD_LOGIC_VECTOR (15 downto 0);
    signal d3 : STD_LOGIC_VECTOR (7 downto 0);
    signal d4 : STD_LOGIC_VECTOR (2 downto 0);
    
    signal z : unsigned (num_bits(SIZE)-1 downto 0);

begin
    
    CLZ_128: if SIZE = 128 generate
        signal d0 : STD_LOGIC_VECTOR (63 downto 0);
    begin
        z(6) <= '1' when x_i(127 downto 64) = (63 downto 0 => '0') else '0';
        d0 <= x_i(127 downto 63) when z(5) = '1' else x_i(127 downto 64);    
        
        z(5) <= '1' when d0(63 downto 32) = (31 downto 0 => '0') else '0';
        d1 <= d0(31 downto 0) when z(5) = '1' else d0(63 downto 32);            
    else generate
        z(5) <= '1' when x_i(63 downto 32) = (31 downto 0 => '0') else '0';
        d1 <= x_i(31 downto 0) when z(5) = '1' else x_i(63 downto 32);
    end generate;

    z(4) <= '1' when d1(31 downto 16) = (15 downto 0 => '0') else '0'; 
    d2 <= d1(15 downto 0) when z(4) = '1' else d1(31 downto 16);

    z(3) <= '1' when d2(15 downto 8) = (7 downto 0 => '0') else '0';
    d3 <= d2(7 downto 0) when z(3) = '1' else d2(15 downto 8);

    z(2) <= '1' when d3(7 downto 4) = (3 downto 0 => '0') else '0';
    d4 <= d3(3 downto 1) when z(2) = '1' else d3(7 downto 5);

    z(1) <= '1' when d4(2 downto 1) = (1 downto 0 => '0') else '0';
    z(0) <= not d4(0) when z(1) = '1' else not d4(2); 
        
    z_o <= z;

end Behavioral;
