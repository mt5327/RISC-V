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

		mem_read_i : in STD_LOGIC_VECTOR (1 downto 0);    
		mem_write_i : in STD_LOGIC_VECTOR (1 downto 0);

		multicycle_op_o : out STD_LOGIC;

		pc_src_i : in STD_LOGIC;
		imm_src_i : in STD_LOGIC;
		ctrl_flow_i : in STD_LOGIC;
		
	    result_select_i : in STD_LOGIC_VECTOR (3 downto 0);
		
        funct3_i : in STD_LOGIC_VECTOR (2 downto 0);
        
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
        
        csr_write_i : in STD_LOGIC;
        csr_write_addr_i : in STD_LOGIC_VECTOR (11 downto 0);
        csr_exception_id_i : in STD_LOGIC_VECTOR (3 downto 0);
        csr_data_i : in STD_LOGIC_VECTOR (63 downto 0);
		csr_operator_i : in STD_LOGIC_VECTOR (1 downto 0);
		csr_o : out CSR);
end execute;

architecture behavioral of execute is

	component ALU is
		port (
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			z_o : out STD_LOGIC_VECTOR (63 downto 0);
			op_i : in ALU_OP);
	end component ALU;

	component branch_unit is
		generic (BHT_INDEX_WIDTH : NATURAL := 2);
		port (
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			op_i : in STD_LOGIC_VECTOR (2 downto 0);
			pc_i : in STD_LOGIC_VECTOR (BHT_INDEX_WIDTH-1 downto 0);
			offset_i : in STD_LOGIC_VECTOR (63 downto 0);
			branch_next_pc_i : in STD_LOGIC_VECTOR (63 downto 0);
			ctrl_flow_i : in STD_LOGIC;
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
    	    enable_fpu_subunit_i : in STD_LOGIC_VECTOR (4 downto 0);
       --     fp_i : in STD_LOGIC;
            fp_precision_i : in STD_LOGIC_VECTOR (1 downto 0);
            cvt_mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            y_i : in STD_LOGIC_VECTOR (63 downto 0);
            z_i : in STD_LOGIC_VECTOR (63 downto 0);
            x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
            result_o : out FP_RESULT);
	end component FPU;

    signal result_fp_reg, z, csr_data, csr_result, csr_data_sel, csr_data_mux : STD_LOGIC_VECTOR (63 downto 0);
    signal result_fp : FP_RESULT;
	signal result, result_mul, result_div, x, y, x_sel, y_sel, x_fp_sel, y_fp_sel, z_fp_sel, MDR : STD_LOGIC_VECTOR (63 downto 0);
	signal instr_misaligned, reg_write_fp, reg_write_fp_reg, reg_write : STD_LOGIC := '0';
	signal div_valid, mul_valid, enable_mul, enable_div, enable_fp, alu_out : STD_LOGIC := '0';
	signal mem_req : MEMORY_REQUEST := ('0', '0', (others => '0'), LSU_NONE);
	signal reg_dst : REG;
	signal exception_id : STD_LOGIC_VECTOR (3 downto 0);
	signal branch_inf : BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
	signal csr : CSR := ('0', (others => '0'), NO_EXCEPTION, (others => '0'), (others => '0'));

begin

	RISCV_ALU : ALU
	port map(
		x_i => x,
		y_i => y,
		z_o => z,
		op_i => alu_operator_i
	);

	BU : branch_unit
	generic map(BHT_INDEX_WIDTH)
	port map(
		x_i => x_sel,
		y_i => y_sel,
		op_i => funct3_i,
		pc_i => pc_i(BHT_INDEX_WIDTH + 2 - 1 downto 2),
		offset_i => imm_i,
		branch_next_pc_i => branch_next_pc_i,
		ctrl_flow_i => ctrl_flow_i,
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
		fp_op_i => fp_regs_idex_i.fp_op,
		enable_fpu_subunit_i => fp_regs_idex_i.enable_fpu_subunit,
		fp_precision_i => fp_regs_idex_i.precision,
		--fp_i => fp_i,
		rm_i => funct3_i,
		cvt_mode_i => imm_i(1 downto 0),
		x_i => x_fp_sel,
		y_i => y_fp_sel,
		z_i => z_fp_sel,
		x_int_i => x_sel,
		result_o => result_fp
	);

	x <= pc_i when pc_src_i = '1' else
	     x_sel;

    y <= imm_i when imm_src_i = '1' else
	  	 y_sel;
	  	 	  	 
	with result_select_i select 
	   result <= z when "0000",
	             result_mul when "0001",
	             result_div when "0010",
	             result_fp.value when "0100",
	             csr_data_mux when "1000",
	             (others => '-') when others;
    
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

	instr_misaligned <= ( or branch_inf.target_address(1 downto 0) ) and branch_inf.taken;

	with mem_write_i select 
	   MDR <= y_sel when "01",
	          y_fp_sel when "10",
		      (others => '0') when others;

	REGS : process (clk_i)
	begin  
		if rising_edge(clk_i) then
			if rst_i = '1' then
				reg_dst.write <= '0';
				reg_write_fp_reg <= '0';
				mem_req.read <= '0';
		        mem_req.write <= '0';
			else
				if pipeline_stall_i = '0' then
                    reg_write_fp_reg <= reg_write_fp;
					result_fp_reg <= result_fp.value;
					reg_dst.data <= result;
					reg_dst.write <= reg_write;
					reg_dst.dest <= reg_dst_i;
					mem_req.read <= or mem_read_i;
					mem_req.write <= or mem_write_i;
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
				csr.write <= '0';
				csr.exception_id <= NO_EXCEPTION;
			else
				if pipeline_stall_i = '0' then
                    csr <= (csr_write_i, csr_write_addr_i, csr_exception_id_i, pc_i, csr_data);
				end if;
			end if;
		end if;
	end process;
  
	enable_mul <= result_select_i(0) and (not mul_valid);
	enable_div <= result_select_i(1) and (not div_valid);
	enable_fp <= result_select_i(2) and (not result_fp.valid);

    reg_write <= reg_write_i or ( mem_read_i(0) and ( nor z(63 downto ADDRESS_WIDTH ) ) ); 	
	reg_write_fp <= fp_regs_idex_i.write or ( mem_read_i(1) and ( nor z(63 downto ADDRESS_WIDTH ) ) );
 
	multicycle_op_o <= enable_mul or enable_div or enable_fp;
	reg_dst_o <= reg_dst;

	reg_write_fp_o <= reg_write_fp_reg;
	result_fp_o <= result_fp_reg;
	
	with csr_mux_sel_i select 
	    csr_data_mux <= csr.data when "01",
	                    csr_data_wb_i when "10",
	                    csr_data_i when "11",
	                    (others => '0') when others;

    csr_data_sel <= imm_i when imm_src_i = '1' else x_sel;         
	
	exception_id <= INSTRUCTION_ADDRESS_MISALIGN when instr_misaligned = '1' else csr_exception_id_i;

	with csr_operator_i select
	   csr_result <= csr_data_mux when CSR_RW,
		           csr_data_mux or csr_data_sel when CSR_RS,
		           csr_data_mux and (not csr_data_sel ) when CSR_RC,
	               (others => '0') when others;

    csr_data <= ((63 downto 5 => '0') & result_fp.fflags) or csr_result;

	csr_o <= csr;

    mem_req_o <= mem_req;
	branch_info_o <= branch_inf;
	
end behavioral; 