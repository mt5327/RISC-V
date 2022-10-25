library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity MAC is
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           enable_i : in STD_LOGIC;
           fp_precision_i : in STD_LOGIC;
           fp_op_i : in FPU_OP;
           rm_i : in STD_LOGIC_VECTOR (2 downto 0);
           x_i : in STD_LOGIC_VECTOR (63 downto 0);
           y_i : in STD_LOGIC_VECTOR (63 downto 0);
           z_i : in STD_LOGIC_VECTOR (63 downto 0);
           result_o : out STD_LOGIC_VECTOR (63 downto 0);
           fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
           fp_valid_o : out STD_LOGIC);
end MAC;

architecture behavioral of MAC is

    signal enable_sp, enable_dp, valid_sp, valid_dp : STD_LOGIC := '0';
    signal result_sp : STD_LOGIC_VECTOR (31 downto 0);
    signal result_dp : STD_LOGIC_VECTOR (63 downto 0);
    signal fflags_sp, fflags_dp : STD_LOGIC_VECTOR (4 downto 0);
 
    component MAC_component is
        Generic ( P : natural; M : natural; E : natural); 
        Port ( clk_i : in STD_LOGIC;
               rst_i : in STD_LOGIC;
               enable_i : in STD_LOGIC;
               fp_op_i : in FPU_OP;
               rm_i : in STD_LOGIC_VECTOR (2 downto 0);
               x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
               y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
               z_i : in STD_LOGIC_VECTOR (P-1 downto 0);
               result_o : out STD_LOGIC_VECTOR (P-1 downto 0);
               fp_valid_o : out STD_LOGIC;
               fflags_o : out STD_LOGIC_VECTOR (4 downto 0));
    end component MAC_component;

begin

    FP_MAC_SP: MAC_component 
    generic map ( P => 32, E => 8, M => 24 )
    port map (
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_sp,
        fp_op_i => fp_op_i,
        rm_i => rm_i,
        x_i => x_i(31 downto 0),
        y_i => y_i(31 downto 0),
        z_i => z_i(31 downto 0),
        result_o => result_sp,
        fp_valid_o => valid_sp,       
        fflags_o => fflags_sp
    );
    
    FP_MAC_DP: MAC_component 
    generic map ( P => 64, E => 11, M => 53 )
    port map (
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_dp,
        fp_op_i => fp_op_i,
        rm_i => rm_i,
        x_i => x_i,
        y_i => y_i,
        z_i => z_i,
        result_o => result_dp,
        fp_valid_o => valid_dp, 
        fflags_o => fflags_dp      
    ); 
    
    enable_sp <= '1' when enable_i = '1' and fp_precision_i = '0' and valid_sp = '0' else '0';  
    enable_dp <= '1' when enable_i = '1' and fp_precision_i = '1' and valid_dp = '0' else '0';
                
    result_o <= (63 downto 32 => result_sp(31)) & result_sp when fp_precision_i = '0' else result_dp;        
    fflags_o <= fflags_sp when fp_precision_i = '0' else fflags_dp;

    fp_valid_o <= valid_sp or valid_dp;            

end behavioral;
  