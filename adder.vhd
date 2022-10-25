library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity adder is
    Port ( x_i : in STD_LOGIC_VECTOR (63 downto 0);
           y_i : in STD_LOGIC_VECTOR (63 downto 0);
           op_i : in ALU_OP;
           z_o : out STD_LOGIC_VECTOR (63 downto 0);
           carry_o : out STD_LOGIC);
end adder;

architecture Behavioral of adder is
    
    signal z : STD_LOGIC_VECTOR (64 downto 0);
    signal is_signed, is_sub : STD_LOGIC;

begin

    is_signed <= '1' when op_i = ALU_SLTU else '0';  
    with op_i select is_sub <= '1' when ALU_SUB | ALU_SUBW, '0' when others;      
    process(x_i, y_i, is_signed, is_sub)
        variable x, b, y : STD_LOGIC_VECTOR (64 downto 0);
        variable carry : unsigned (64 downto 0);
    begin
        x := ( x_i(63) and is_signed) & x_i;
        b := ( y_i(63) and is_signed) & y_i;
        carry := (others => '0'); 

        if is_sub = '1' then
            y := not b;
            carry(0) := '1';
        else
            y := b;
        end if;
        z <= STD_LOGIC_VECTOR(unsigned(x) + unsigned(y) + carry);
    end process;
    
    z_o <= z(63 downto 0);
    carry_o <= z(64);
    
    
end Behavioral;

