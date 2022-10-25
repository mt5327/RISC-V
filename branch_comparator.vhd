library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL; 

use work.constants.ALL;

entity branch_comparator is
    Port ( x_i : in STD_LOGIC_VECTOR (63 downto 0);
           y_i : in STD_LOGIC_VECTOR (63 downto 0);
           op_i : in STD_LOGIC_VECTOR (2 downto 0);
           cmp_o : out STD_LOGIC);
end branch_comparator;

architecture Behavioral of branch_comparator is

    signal eq, lt : STD_LOGIC;
    signal X, Y : STD_LOGIC_VECTOR (64 downto 0);
  
begin

    X <= ( X_i(63) and (not op_i(1) ) ) & x_i;
    Y <= ( Y_i(63) and (not op_i(1) ) ) & y_i;

    eq <= '1' when unsigned(x_i) = unsigned(y_i) else '0';
    lt <= '1' when signed(X) < signed(Y) else '0';

    with op_i select cmp_o <= eq when BEQ,
                          not eq when BNE,
                              lt when BLT | BLTU,
                          not lt when BGE | BGEU,
                             '0' when others; 
end Behavioral;
