library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity execute is
	generic (
		ADDRESS_WIDTH : NATURAL := 21;
		BHT_INDEX_WIDTH : NATURAL := 2);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;

		pipeline_stall_i : in STD_LOGIC;

		mem_read_i : in STD_LOGIC;
        mem_read_o : out STD_LOGIC;
    
		mem_write_i : in STD_LOGIC;
		mem_write_o : out STD_LOGIC;

		multicycle_op_o : out STD_LOGIC;

		pc_src_i : in STD_LOGIC;
		imm_src_i : in STD_LOGIC;
		ctrl_flow_i : in STD_LOGIC;
		fp_i : in STD_LOGIC;
        
		imm_i : in STD_LOGIC_VECTOR (63 downto 0);
		pc_i : in STD_LOGIC_VECTOR (63 downto 0);

		branch_predict_i : in BRANCH_PREDICTION;
		branch_info_o : out BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
        branch_next_pc_i : in STD_LOGIC_VECTOR (63 downto 0);

		reg_write_i : in STD_LOGIC;

		reg_dst_i : in STD_LOGIC_VECTOR (4 downto 0);
		reg_dst_o : out REG;

		alu_operator_i : in ALU_OP;
		mem_operator_i : in MEM_OP;
        result_fp_o : out STD_LOGIC_VECTOR (63 downto 0);

        x_i : in STD_LOGIC_VECTOR (63 downto 0);
        y_i : in STD_LOGIC_VECTOR (63 downto 0);

		result_fwd_wb_i : in STD_LOGIC_VECTOR (63 downto 0);
		result_fwd_fp_wb_i : in STD_LOGIC_VECTOR (63 downto 0);
		
		x_mux_sel_i, y_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
		x_fp_mux_sel_i, y_fp_mux_sel_i, z_fp_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
		
		mem_req_o : out MEMORY_REQUEST;

		fp_regs_idex_i : in FP_IDEX;
		reg_write_fp_o : out STD_LOGIC;

		csr_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
		csr_data_wb_i : in STD_LOGIC_VECTOR (63 downto 0);

		csr_i : in CSR;
		csr_operator_i : in STD_LOGIC_VECTOR (1 downto 0);
		csr_o : out CSR);
end execute;

architecture behavioral of execute is

	component ALU is
		port (
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			z_o : out STD_LOGIC_VECTOR (63 downto 0);
			cmp_o : out STD_LOGIC;
			op_i : in ALU_OP);
	end component ALU;

	component branch_unit is
		generic (BHT_INDEX_WIDTH : NATURAL := 2);
		port (
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			pc_i : in STD_LOGIC_VECTOR (BHT_INDEX_WIDTH-1 downto 0);
			offset_i : in STD_LOGIC_VECTOR (63 downto 0);
			branch_next_pc_i : in STD_LOGIC_VECTOR (63 downto 0);
			ctrl_flow_i : in STD_LOGIC;
			alu_cmp_i : in STD_LOGIC;
			branch_predict_i : in BRANCH_PREDICTION;
			branch_info_o : out BRANCH_INFO); 
	end component branch_unit;

	component multiplier is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			enable_i : in STD_LOGIC;

			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			op_i : in ALU_OP;

			mul_valid_o : out STD_LOGIC;
			result_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component multiplier;

	component divider is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			enable_i : in STD_LOGIC;
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			op_i : in ALU_OP;
			z_o : out STD_LOGIC_VECTOR (63 downto 0);
			div_valid_o : out STD_LOGIC);
	end component divider;

	component FPU is
		port (
            clk_i : in STD_LOGIC;
            rst_i : in STD_LOGIC;
            fp_op_i : in FPU_OP;
            fp_precision_i : in STD_LOGIC;
            cvt_mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            y_i : in STD_LOGIC_VECTOR (63 downto 0);
            z_i : in STD_LOGIC_VECTOR (63 downto 0);
            x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
            write_fflags_o : out STD_LOGIC;
            result_o : out FP_RESULT;
            result_int_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component FPU;

    signal result_fp_reg, z, result_int, csr_data, csr_data_sel, csr_data_mux : STD_LOGIC_VECTOR (63 downto 0);
    signal result_fp : FP_RESULT;
	signal result, result_mul, result_div, Y_fp, x, y, x_sel, y_sel, x_fp_sel, y_fp_sel, z_fp_sel, MDR : STD_LOGIC_VECTOR (63 downto 0);
	signal instr_misaligned, mem_write, reg_write_fp, write_fflags : STD_LOGIC := '0';
	signal alu_cmp, div_valid, mul_valid, enable_mul, enable_div, enable_fp, enable_mem, alu_out, csr_write, mem_read, multiply, divide : STD_LOGIC := '0';
	signal mem_req : MEMORY_REQUEST := ('0', (others => '0'), LSU_NONE);
	signal reg_dst : REG;
	signal exception_id : STD_LOGIC_VECTOR (3 downto 0);
	signal branch_inf : BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
	signal csr, csr_reg : CSR := ('0', (others => '0'), NO_EXCEPTION, (others => '0'), (others => '0'));

begin

	RISCV_ALU : ALU
	port map(
		x_i => x,
		y_i => y,
		z_o => z,
		cmp_o => alu_cmp,
		op_i => alu_operator_i
	);

	BU : branch_unit
	generic map(BHT_INDEX_WIDTH => BHT_INDEX_WIDTH)
	port map(
		x_i => x_sel,
		pc_i => pc_i(BHT_INDEX_WIDTH + 2 - 1 downto 2),
		offset_i => imm_i,
		branch_next_pc_i => branch_next_pc_i,
		ctrl_flow_i => ctrl_flow_i,
		alu_cmp_i => alu_cmp,
		branch_predict_i => branch_predict_i,
		branch_info_o => branch_inf
	);

	MUL : multiplier
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable_mul,
		x_i => x_sel,
		y_i => y_sel,
		op_i => alu_operator_i,
		mul_valid_o => mul_valid,
		result_o => result_mul
	);

	DIV : divider
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable_div,
		x_i => x_sel,
		y_i => y_sel,
		op_i => alu_operator_i,
		div_valid_o => div_valid,
		z_o => result_div
	);

	RISCV_FPU : FPU
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		fp_op_i => fp_regs_IDEX_i.fp_op,
		fp_precision_i => fp_regs_idex_i.precision,
		rm_i => fp_regs_IDEX_i.rm,
		cvt_mode_i => imm_i(1 downto 0),
		x_i => x_fp_sel,
		y_i => y_fp_sel,
		z_i => z_fp_sel,
		x_int_i => x_sel,
		write_fflags_o => write_fflags,
		result_o => result_fp,
		result_int_o => result_int
	);

	x <= pc_i when pc_src_i = '1' else
	     x_sel;

    y <= imm_i when imm_src_i = '1' else
	  	 y_sel;

	result <= result_mul when multiply = '1' else
		      result_div when divide = '1' else
		      result_int when fp_i = '1' else
		      csr_data_mux when or csr_operator_i else 
		      z;
    
    with x_mux_sel_i select 
        x_sel <= reg_dst.data when "01",
                 result_fwd_wb_i when "10",
                 x_i when others;

    with y_mux_sel_i select 
        y_sel <= reg_dst.data when "01",
                 result_fwd_wb_i when "10",
                 y_i when others;                 

   with x_fp_mux_sel_i select 
        x_fp_sel <= result_fp_reg when "01",
                 result_fwd_fp_wb_i when "10",
                 fp_regs_idex_i.x when others;    
                 
    with y_fp_mux_sel_i select 
        y_fp_sel <= result_fp_reg when "01",
                 result_fwd_fp_wb_i when "10",
                 fp_regs_idex_i.y when others;
   
    with z_fp_mux_sel_i select                     
        z_fp_sel <= result_fp_reg when "01",
                 result_fwd_fp_wb_i when "10",
                 fp_regs_idex_i.z when others;


	instr_misaligned <= (or branch_inf.target_address(1 downto 0)) and branch_inf.mispredict;

	--    Y_fp <= X"FFFFFFFF" & fp_regs_IDEX_i.y(31 downto 0) when (or fp_regs_IDEX_i.y(63 downto 31)) = '0' else 
	--                                       fp_regs_IDEX_i.y; 


	with mem_operator_i select 
	   MDR <= y_fp_sel when LSU_FSW | LSU_FSD,
		      y_sel when LSU_SB | LSU_SH | LSU_SW | LSU_SD,
		      (others => '0') when others;

	REGS : process (clk_i)
	begin  
		if rising_edge(clk_i) then
			if rst_i = '1' then
				reg_dst.write <= '0';
				reg_write_fp <= '0';
				mem_req.enable_mem <= '0';
				mem_write <= '0';
				mem_read <= '0';
			else
				if pipeline_stall_i = '0' then
                    reg_write_fp <= fp_regs_idex_i.write;
					result_fp_reg <= result_fp.value;
					reg_dst.data <= result;
					reg_dst.write <= reg_write_i;
					reg_dst.dest <= reg_dst_i;
                    mem_read <= mem_read_i;
					mem_req.enable_mem <= enable_mem;
					mem_write <= mem_write_i;
					mem_req.MEMOp <= mem_operator_i;
					mem_req.MDR <= MDR;
				end if;
			end if;
		end if;
	end process;
	
	CS_REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				csr_reg.write <= '0';
				csr_reg.exception_id <= NO_EXCEPTION;
			else
				if pipeline_stall_i = '0' then
					csr_reg <= csr;
				end if;
			end if;
		end if;
	end process;
	
		with alu_operator_i select
		multiply <= '1' when ALU_MUL | ALU_MULH | ALU_MULHSU | ALU_MULHU | ALU_MULW,
		            '0' when others;

	with alu_operator_i select
		divide <= '1' when ALU_DIV | ALU_DIVU | ALU_DIVW | ALU_DIVUW | ALU_REM | ALU_REMU | ALU_REMW | ALU_REMUW,
		          '0' when others;



	enable_mem <= mem_write_i or mem_read_i;
	enable_mul <= multiply and (not mul_valid);
	enable_div <= divide and (not div_valid );
	enable_fp <= fp_i and (not result_fp.valid);

	multicycle_op_o <= enable_mul or enable_div or enable_fp;

	reg_dst_o <= reg_dst;

	reg_write_fp_o <= reg_write_fp;
	result_fp_o <= result_fp_reg;
	
	with csr_mux_sel_i select 
	   csr_data_mux <= csr_reg.data when "01",
	               csr_data_wb_i when "10",
	               csr_i.data when others; 

    csr_data_sel <= imm_i when imm_src_i = '1' else x_sel;         
	csr.exception_id <= INSTRUCTION_ADDRESS_MISALIGN when instr_misaligned = '1' else csr_i.exception_id;

	with csr_operator_i select
		csr_data <= csr_data_sel when CSR_RW,
		            csr_data_mux or csr_data_sel when CSR_RS,
		            csr_data_mux and (not csr_data_sel) when CSR_RC,
		            (others => '0') when others;
	
	csr.write <= csr_i.write or write_fflags;
	csr.write_addr <= FFLAGS when write_fflags = '1' else 
	                  csr_i.write_addr when csr_i.write = '1' else (others => '0');
	
	csr.data <= (63 downto 5 => '0') & result_fp.fflags when write_fflags = '1' else 
	            csr_data when csr_i.write = '1' else (others => '0');
	
	csr.epc <= csr_i.epc; 

	csr_o <= csr_reg;

	mem_req_o <= mem_req;
	mem_write_o <= mem_write;
	mem_read_o <= mem_read;
	branch_info_o <= branch_inf;
	
end behavioral;