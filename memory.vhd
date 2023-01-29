library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.constants.all;

entity memory is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;

		pipeline_stall_i : in STD_LOGIC;
        mem_read_i : in STD_LOGIC;
        mem_operator_i : in MEM_OP;
		reg_dst_i : in REG;
        
		reg_write_fp_i : in STD_LOGIC;
        result_fp_i : in STD_LOGIC_VECTOR (63 downto 0);
        
		mem_data_i : in STD_LOGIC_VECTOR (63 downto 0);
       
		reg_dst_o : out REG;
		reg_dst_fp_o : out REG;

		csr_i : in CSR;
		csr_o : out CSR);
end memory;

architecture behavioral of memory is

	signal data, data_fp : STD_LOGIC_VECTOR (63 downto 0);
	signal csr : CSR := ('0', X"000", NO_EXCEPTION, (others => '0'), (others => '0'));
	signal reg_dst, reg_dst_fp : REG;
        
begin
       
    data <= mem_data_i when mem_read_i = '1' else reg_dst_i.data;
	data_fp <= mem_data_i when mem_read_i = '1' else result_fp_i;

	REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				reg_dst.write <= '0';
			else
				if pipeline_stall_i = '0' then
					reg_dst.data <= data;
					reg_dst.write <= reg_dst_i.write;
					reg_dst.dest <= reg_dst_i.dest;
				end if;
 			end if;
		end if;
	end process;

	FP_REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				reg_dst_fp.write <= '0';
			else
				if pipeline_stall_i = '0' then
					reg_dst_fp.data <= data_fp;
					reg_dst_fp.write <= reg_write_fp_i;
					reg_dst_fp.dest <= reg_dst_i.dest;
				end if;
			end if;
		end if;
	end process;

	CSR_REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				csr.write <= '0';
				csr.exception_id <= NO_EXCEPTION;
			else
				if pipeline_stall_i = '0' then
					csr <= csr_i;
				end if;
			end if;
		end if;
	end process;

	reg_dst_o <= reg_dst;
	reg_dst_fp_o <= reg_dst_fp;

	csr_o <= csr;

end behavioral;