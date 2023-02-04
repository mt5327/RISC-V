library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity csr_regfile is
    port (
        clk_i : in STD_LOGIC;
        rst_i : in STD_LOGIC;
        cpu_enable_i : in STD_LOGIC;
        csr_i : in CSR;
        csr_read_addr_i : in STD_LOGIC_VECTOR (11 downto 0);
        csr_data_o : out STD_LOGIC_VECTOR (63 downto 0);
        mpc_o : out STD_LOGIC_VECTOR (63 downto 0);
        fcsr_o : out STD_LOGIC_VECTOR (7 downto 0);
        exception_num_o : out STD_LOGIC_VECTOR (3 downto 0);
        system_time_o : out STD_LOGIC_VECTOR (63 downto 0));
end csr_regfile;

architecture behavioral of csr_regfile is

    signal fflags_reg : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal frm_reg : STD_LOGIC_VECTOR (2 downto 0) := (others => '0');
    signal mepc_reg, mtvec_reg, mtval_reg : STD_LOGIC_VECTOR (63 downto 0) := (others => '0');
    signal cycles, instret_reg : STD_LOGIC_VECTOR (63 downto 0) := (others => '0');
    signal mcause_reg : STD_LOGIC_VECTOR (3 downto 0) := NO_EXCEPTION;
    signal exception, write : STD_LOGIC;

begin

    exception <= nand csr_i.exception_id when cpu_enable_i = '1' else '0';
    
    CSR_WRITE : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' or cpu_enable_i = '0' then
                frm_reg <= (others => '0');
                fflags_reg <= (others => '0');
                mepc_reg <= (others => '0');
                mcause_reg <= NO_EXCEPTION;
                cycles <= (others => '0');
                instret_reg <= (others => '0');
            else
                cycles <= STD_LOGIC_VECTOR(unsigned(cycles) + 1);
                if exception = '1' then
                    mepc_reg <= csr_i.epc;
                    mcause_reg <= csr_i.exception_id;
                else
                    if csr_i.instr_valid = '1' then
                        instret_reg <= STD_LOGIC_VECTOR(unsigned(instret_reg) + 1); 
                    end if; 
                    
                    if csr_i.write = '1' then
                        case csr_i.write_addr is
                            when FFLAGS =>
                                fflags_reg <= csr_i.data(4 downto 0);
                            when FRM =>
                                frm_reg <= csr_i.data(2 downto 0);
                            when FCSR =>
                                fflags_reg <= csr_i.data(4 downto 0);
                                frm_reg <= csr_i.data(7 downto 5);
                            when CYCLE =>
                                cycles <= csr_i.data;
                            when INSTRET =>
                                instret_reg <= csr_i.data;
                            when MEPC =>
                                mepc_reg <= csr_i.epc;
                            when MCAUSE =>
                                mcause_reg <= csr_i.data(3 downto 0);
                            when MTVAL =>
                                mtval_reg <= csr_i.data;
                            when others =>
                        end case;
                    end if;
                end if;
            end if;
        end if;
    end process;

    with csr_read_addr_i select 
        csr_data_o <= (63 downto 5 => '0') & fflags_reg when FFLAGS,
                      (63 downto 3 => '0') & frm_reg when FRM,
                      (63 downto 8 => '0') & frm_reg & fflags_reg when FCSR,
                      X"8000000000001128" when MISA,
                      mepc_reg when MEPC,
                      mtval_reg when MTVAL,
                      (63 downto 4 => '0') & mcause_reg when MCAUSE,
                      cycles when CYCLE,
                      instret_reg when INSTRET,
                      (others => '0') when others;

    exception_num_o <= mcause_reg;
    fcsr_o <= frm_reg & fflags_reg;
    mpc_o <= mepc_reg;
    system_time_o <= cycles;
end behavioral;