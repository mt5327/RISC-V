library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.constants.ALL;

entity forwarding_unit is
    port ( 
        reg_cmp1_mem_i : in STD_LOGIC;
        reg_cmp1_wb_i : in STD_LOGIC;
        
        reg_cmp2_mem_i : in STD_LOGIC;
        reg_cmp2_wb_i : in STD_LOGIC;

        reg_cmp3_mem_i : in STD_LOGIC;
        reg_cmp3_wb_i : in STD_LOGIC;

        csr_cmp_mem_i : in STD_LOGIC;
        csr_cmp_wb_i : in STD_LOGIC;

        reg_write_mem_i : in STD_LOGIC;
        reg_write_wb_i : in STD_LOGIC;
        
        reg_fp_write_mem_i : in STD_LOGIC;
        reg_fp_write_wb_i : in STD_LOGIC;
        
        csr_write_i : in STD_LOGIC;
        csr_write_mem_i : in STD_LOGIC;
        csr_write_wb_i : in STD_LOGIC; 
                
        x_mux_sel_o : out STD_LOGIC_VECTOR (1 downto 0);
        y_mux_sel_o : out STD_LOGIC_VECTOR (1 downto 0);
     
        x_fp_mux_sel_o : out STD_LOGIC_VECTOR (1 downto 0);
        y_fp_mux_sel_o : out STD_LOGIC_VECTOR (1 downto 0);
        z_fp_mux_sel_o : out STD_LOGIC_VECTOR (1 downto 0);
        
        csr_mux_sel_o : out STD_LOGIC_VECTOR (2 downto 0));
end forwarding_unit;

architecture behavioral of forwarding_unit is

	signal x_fwd_mem, x_fwd_wb, y_fwd_mem, y_fwd_wb : STD_LOGIC;
	signal x_fp_fwd_mem, x_fp_fwd_wb, y_fp_fwd_mem, y_fp_fwd_wb, z_fp_fwd_mem, z_fp_fwd_wb : STD_LOGIC;

    signal csr_fwd_mem, csr_fwd_wb : STD_LOGIC;
    
begin

 	-- FORWARDING LOGIC INTEGER
	x_fwd_mem <= reg_cmp1_mem_i and reg_write_mem_i; 
	x_fwd_wb <= reg_cmp1_wb_i and reg_write_wb_i;

    y_fwd_mem <= reg_cmp2_mem_i and reg_write_mem_i;
	y_fwd_wb <= reg_cmp2_wb_i and reg_write_wb_i;

    x_mux_sel_o <= "01" when x_fwd_mem = '1' else                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                 "10" when x_fwd_wb = '1' else 
                 "11";
                 
    y_mux_sel_o <= "01" when y_fwd_mem = '1' else 
                 "10" when y_fwd_wb = '1' else 
                 "11";

 	-- FORWARDING LOGIC FLOATING POINT
	x_fp_fwd_mem <= reg_cmp1_mem_i and reg_fp_write_mem_i;
	x_fp_fwd_wb <= reg_cmp1_wb_i and reg_fp_write_wb_i;

    y_fp_fwd_mem <= reg_cmp2_mem_i and reg_fp_write_mem_i;
	y_fp_fwd_wb <= reg_cmp2_wb_i and reg_fp_write_wb_i;
    
    z_fp_fwd_mem <= reg_cmp3_mem_i and reg_fp_write_mem_i;
	z_fp_fwd_wb <= reg_cmp3_wb_i and reg_fp_write_wb_i;

    x_fp_mux_sel_o <= "01" when x_fp_fwd_mem = '1' else 
                    "10" when x_fp_fwd_wb = '1' else 
                    "11";
                 
    y_fp_mux_sel_o <= "01" when y_fp_fwd_mem = '1' else 
                    "10" when y_fp_fwd_wb = '1' else 
                    "11"; 
    
    z_fp_mux_sel_o <= "01" when z_fp_fwd_mem = '1' else
                    "10" when z_fp_fwd_wb = '1' else
                    "11";
                                          
    -- FORWARDING LOGIC CSR
    csr_fwd_mem <= csr_cmp_mem_i and csr_write_mem_i;
    csr_fwd_wb <= csr_cmp_wb_i and csr_write_wb_i;
    
    csr_mux_sel_o <= "001" when csr_fwd_mem = '1' else
                   "010" when csr_fwd_wb = '1' else
                   "100" when csr_write_i = '1' else "000";
                 
end behavioral;
