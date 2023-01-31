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
        
        IR_i : in STD_LOGIC_VECTOR (31 downto 0);
        
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
		
		x_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
        y_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
        x_fp_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
        y_fp_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
        z_fp_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
		
		mem_req_o : out MEMORY_REQUEST;
    
		fp_regs_idex_i : in FP_IDEX;
		reg_write_fp_o : out STD_LOGIC;
		
		uart_tx_enable_o : out STD_LOGIC;
		
		csr_mux_sel_i : in STD_LOGIC_VECTOR (2 downto 0);
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
        generic ( 
            P : natural; 
            E : natural; 
            M : natural);		
	    port (
            clk_i : in STD_LOGIC;
            rst_i : in STD_LOGIC;
            fp_op_i : in FPU_OP;
    	    enable_fpu_subunit_i : in STD_LOGIC_VECTOR (4 downto 0);
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
	signal mem_req : MEMORY_REQUEST := ('0', '0', (others => '0'), (others => '0'), LSU_NONE);
	signal reg_dst : REG;
	signal mem_read, mem_read_fp, mem_write, mem_write_fp : STD_LOGIC;
	signal exception_id : STD_LOGIC_VECTOR (3 downto 0);
	signal branch_inf : BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
	signal csr : CSR := ('0', (others => '0'), NO_EXCEPTION, (others => '0'), (others => '0'));
    signal memory_address : STD_LOGIC_VECTOR (63 downto 0);
    
    signal uart_tx_enable, uart_tx_enable_reg : STD_LOGIC;
    
    type fp_results is array (0 to 1) of FP_RESULT;
    signal results_fp : fp_results;
    
    signal enable_fpu_subunit : STD_LOGIC_VECTOR (9 downto 0);

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

	FPU_UNITS : for i in 0 to 1 generate 
        FPU_UNIT: FPU 
        generic map (FP_FORMATS(i).P, FP_FORMATS(i).E, FP_FORMATS(i).M)
        port map(
            clk_i => clk_i,
            rst_i => rst_i,
            fp_op_i => fp_regs_idex_i.fp_op,
            enable_fpu_subunit_i => enable_fpu_subunit(i*5+4 downto i*5),
            rm_i => funct3_i,
            cvt_mode_i => imm_i(1 downto 0),
            x_i => x_fp_sel,
            y_i => y_fp_sel,
            z_i => z_fp_sel,
            x_int_i => x_sel,
            result_o => results_fp(i)
        );
    end generate;

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
	             (others => '0') when others;
    
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
		      (others => '-') when others;

	REGS : process (clk_i)
	begin  
		if rising_edge(clk_i) then
			if rst_i = '1' then
				reg_dst.write <= '0';
				reg_write_fp_reg <= '0';
				mem_req.read <= '0';
		        mem_req.write <= '0';
		        uart_tx_enable_reg <= '0';
		        mem_req.MEMOp <= LSU_NONE;
		        reg_dst.dest <= (others => '0');
			else
				if pipeline_stall_i = '0' then
                    reg_write_fp_reg <= reg_write_fp;
					result_fp_reg <= result_fp.value;
					reg_dst.data <= result;
					reg_dst.write <= reg_write;
					reg_dst.dest <= reg_dst_i;
					mem_req.read <= or mem_read_i;
					mem_req.write <= mem_write or mem_write_fp;
					mem_req.MEMOp <= mem_operator_i;
					mem_req.MDR <= MDR;
					mem_req.MAR <= memory_address;
					uart_tx_enable_reg <= uart_tx_enable;
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
	enable_div <= result_select_i (1) and (not div_valid);
	enable_fp <= '0'; -- result_select_i(2) and (not result_fp.valid);
    
    memory_address <= STD_LOGIC_VECTOR(unsigned(x_sel) + unsigned(imm_i));  
    
    mem_write <= mem_write_i(0) and memory_address(ADDRESS_WIDTH) and (nor memory_address(63 downto ADDRESS_WIDTH+1)); 
    mem_write_fp <= mem_write_i(1) and memory_address(ADDRESS_WIDTH) and (nor memory_address(63 downto ADDRESS_WIDTH+1)); 
    
    uart_tx_enable <= '1' when mem_write_i(0) = '1' and memory_address = X"FFFFFFFFFFFFFFF0" else '0';
    process (uart_tx_enable) 
    begin
        if uart_tx_enable = '1' then
            report "UART start";
        end if;
    end process;
 
    reg_write <= reg_write_i; 	
	reg_write_fp <= fp_regs_idex_i.write;
 
	multicycle_op_o <= enable_mul or enable_div or enable_fp;
	reg_dst_o <= reg_dst;

	reg_write_fp_o <= reg_write_fp_reg;
	result_fp_o <= result_fp_reg;
	
	with csr_mux_sel_i select 
	    csr_data_mux <= csr.data when "001",
	                    csr_data_wb_i when "010",
	                    csr_data_i when "100",
	                    (others => '0') when others;

    csr_data_sel <= imm_i when imm_src_i = '1' else x_sel;         
	
	exception_id <= INSTRUCTION_ADDRESS_MISALIGN when instr_misaligned = '1' else csr_exception_id_i;

	with csr_operator_i select
	   csr_result <= csr_data_sel when CSR_RW,
		           csr_data_mux or csr_data_sel when CSR_RS,
		           csr_data_mux and (not csr_data_sel ) when CSR_RC,
	               (others => '0') when others;

    with result_select_i(3 downto 2) select 
        csr_data <= ((63 downto 5 => '0') & result_fp.fflags) when "01",
        csr_result when "10", 
        (others => '0') when others;


    with fp_regs_idex_i.precision select
        result_fp <= results_fp(0) when "01",
                     results_fp(1) when "10",
                     ((others => '0'), (others => '0'), '0') when others;
                     
    ENABLE_FPU_SUBUNIT_GEN: for i in 0 to 1 generate 
        enable_fpu_subunit(i*5+4 downto i*5) <= fp_regs_idex_i.enable_fpu_subunit(4 downto 0) and 
                                      (4 downto 0 => fp_regs_idex_i.precision(i)) and
                                      (4 downto 0 => not results_fp(i).valid);
    end generate;

	csr_o <= csr;

    mem_req_o <= mem_req;
	branch_info_o <= branch_inf;
	
	uart_tx_enable_o <= uart_tx_enable_reg;
	
end behavioral; 