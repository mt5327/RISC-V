library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity RISCV is
	generic (
		RAM_FILENAME : STRING := "C:\\cygwin64\\home\\Mitja\\matmul\\main.hex";
		ADDRESS_WIDTH : NATURAL := 17;
		BLOCK_SIZE : NATURAL := 256;
		INDEX_WIDTH : NATURAL := 7;
		BHT_INDEX_WIDTH : NATURAL := 3);
	port (
		clk_i : in STD_LOGIC;
		rst_ni : in STD_LOGIC;
		-- UART
		rx_i : in STD_LOGIC;
    	tx_o : out STD_LOGIC;
		-- VGA            
		hsync_o : out STD_LOGIC;
		vsync_o : out STD_LOGIC;
		rgb_o : out STD_LOGIC_VECTOR (11 downto 0);

		LED_o : out STD_LOGIC_VECTOR (2 downto 0);
        -- Seven-segment display       
		anode_o : out STD_LOGIC_VECTOR (7 downto 0);
		cathode_o : out STD_LOGIC_VECTOR (7 downto 0));
        
	-- !!! SIMULATION ONLY !!! 
 	 --   test_number_o : out STD_LOGIC_VECTOR (63 downto 0); 
     --   system_time_o : out STD_LOGIC_VECTOR (63 downto 0));
end RISCV;

architecture behavioral of RISCV is

	component fetch is
		generic (
			ADDRESS_WIDTH : NATURAL := 18;
			BHT_INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_ni : in STD_LOGIC;

			pipeline_stall_i : in STD_LOGIC;
			branch_info_i : in BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
			IR_i : in STD_LOGIC_VECTOR(31 downto 0);
			instr_address_o : out STD_LOGIC_VECTOR(ADDRESS_WIDTH - 3 downto 0);
			pc_o : out STD_LOGIC_VECTOR(63 downto 0);
			IR_o : out STD_LOGIC_VECTOR(31 downto 0);
			branch_predict_o : out BRANCH_PREDICTION);
	end component fetch;

	component decode is
        port (
            clk_i : in STD_LOGIC;
            rst_ni : in STD_LOGIC;
            load_hazard_o : out STD_LOGIC;
    
            flush_i : in STD_LOGIC;
            pipeline_stall_i : in STD_LOGIC;
    
            IR_i : in STD_LOGIC_VECTOR (31 downto 0);
            frm_i : in STD_LOGIC_VECTOR (2 downto 0);
            funct3_o : out STD_LOGIC_VECTOR (2 downto 0);
            pc_i : in STD_LOGIC_VECTOR (63 downto 0);
    
            branch_predict_i : in BRANCH_PREDICTION;
    
            csr_data_i : in STD_LOGIC_VECTOR (63 downto 0);
            csr_read_address_o : out STD_LOGIC_VECTOR (11 downto 0);
            
            pc_o : out STD_LOGIC_VECTOR (63 downto 0);
            
            branch_predict_o : out BRANCH_PREDICTION;
    
            mem_read_o : out STD_LOGIC_VECTOR (1 downto 0);
            mem_write_o : out STD_LOGIC_VECTOR (1 downto 0);
    
            pc_src_o : out STD_LOGIC;
            imm_src_o : out STD_LOGIC;
            ctrl_flow_o : out STD_LOGIC;
            
            result_select_o : out STD_LOGIC_VECTOR (3 downto 0);
            
            imm_o : out STD_LOGIC_VECTOR (63 downto 0);
            branch_next_pc_o : out STD_LOGIC_VECTOR (63 downto 0);
            
            alu_operator_o : out ALU_OP;
            mem_operator_o : out MEM_OP;
    
            reg_write_o : out STD_LOGIC;
            reg_dst_o : out STD_LOGIC_VECTOR (4 downto 0);
            
            reg_dst_i : in REG;
            reg_dst_fp_i : in REG;
            
            reg_mem_i : in STD_LOGIC_VECTOR (4 downto 0);
            csr_mem_address_i : in STD_LOGIC_VECTOR (11 downto 0);
            
            x_o : out STD_LOGIC_VECTOR (63 downto 0);
            y_o : out STD_LOGIC_VECTOR (63 downto 0);
                    
            csr_write_o : out STD_LOGIC;
            csr_write_address_o : out STD_LOGIC_VECTOR (11 downto 0);
            csr_exception_id_o : out STD_LOGIC_VECTOR (3 downto 0);
            csr_data_o : out STD_LOGIC_VECTOR (63 downto 0);
            
            reg_cmp1_mem_o : out STD_LOGIC;
            reg_cmp1_wb_o : out STD_LOGIC;
    
            reg_cmp2_mem_o : out STD_LOGIC;
            reg_cmp2_wb_o : out STD_LOGIC;
            
            reg_cmp3_mem_o : out STD_LOGIC;
            reg_cmp3_wb_o : out STD_LOGIC;
            
            csr_cmp_mem_o : out STD_LOGIC; 
            csr_cmp_wb_o : out STD_LOGIC;
                    
            registers_o : out reg_t;
            registers_fp_o : out reg_t;
            
            fp_regs_IDEX_o : out FP_IDEX;
            csr_operator_o : out STD_LOGIC_VECTOR (1 downto 0));
	end component decode;

	component execute is
        generic (
            ADDRESS_WIDTH : NATURAL := 21;
            BHT_INDEX_WIDTH : NATURAL := 2);
        port (
            clk_i : in STD_LOGIC;
            rst_ni : in STD_LOGIC;
    
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
            csr_write_address_i : in STD_LOGIC_VECTOR (11 downto 0);
            csr_exception_id_i : in STD_LOGIC_VECTOR (3 downto 0);
            csr_data_i : in STD_LOGIC_VECTOR (63 downto 0);
            csr_operator_i : in STD_LOGIC_VECTOR (1 downto 0);
            csr_o : out CSR);
	end component execute;

	component memory is
		port (
            clk_i : in STD_LOGIC;
            rst_ni : in STD_LOGIC;
    
            pipeline_stall_i : in STD_LOGIC;
            mem_read_i : in STD_LOGIC;
            reg_dst_i : in REG;
            
            reg_write_fp_i : in STD_LOGIC;
            result_fp_i : in STD_LOGIC_VECTOR (63 downto 0);
            
            mem_data_i : in STD_LOGIC_VECTOR (63 downto 0);
           
            reg_dst_o : out REG;
            reg_dst_fp_o : out REG;
    
            csr_i : in CSR;
            csr_o : out CSR);
	end component memory;

	component csr_regfile is
		port (
			clk_i : in STD_LOGIC;
			rst_ni : in STD_LOGIC;
			csr_i : in CSR;
			csr_read_address_i : in STD_LOGIC_VECTOR (11 downto 0);
			CSR_data_o : out STD_LOGIC_VECTOR (63 downto 0);
			mpc_o : out STD_LOGIC_VECTOR (63 downto 0);
			fcsr_o : out STD_LOGIC_VECTOR (7 downto 0);
			exception_num_o : out STD_LOGIC_VECTOR (3 downto 0);
			system_time_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component csr_regfile;

    component forwarding_unit is
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
    end component forwarding_unit;

	component load_store_unit is
		generic (ADDRESS_WIDTH : NATURAL := 14);
		port (
            clk_i : in STD_LOGIC;
            rst_ni : in STD_LOGIC;
            exception_i : in STD_LOGIC;
            enable_mem_i : in STD_LOGIC;
            MAR_i : in STD_LOGIC_VECTOR (ADDRESS_WIDTH - 1 downto 0);
            MDR_i : in STD_LOGIC_VECTOR (63 downto 0);
            memory_operation_i : in MEM_OP;	
            cache_req_o : out CACHE_REQUEST (MAR(ADDRESS_WIDTH - 4 downto 0));
            unaligned_access_o : out STD_LOGIC;
            miss_i : in STD_LOGIC;
            data_i : in STD_LOGIC_VECTOR (63 downto 0);
            data_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component load_store_unit;

	component instruction_cache is
		generic (
			ADDRESS_WIDTH : NATURAL := 19;
			BLOCK_SIZE : NATURAL := 256;
			INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_ni : in STD_LOGIC;
			instr_address_i : in STD_LOGIC_VECTOR (ADDRESS_WIDTH - 3 downto 0);
			read_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8) - 1 downto 0);
			data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
			IR_o : out STD_LOGIC_VECTOR (31 downto 0);
			miss_o : out STD_LOGIC);
	end component instruction_cache;
    
    component instruction_memory is
	   generic (
	       RAM_FILENAME : STRING := "C:\\DigitalDesign\\hex\\fmin.hex";
	       BLOCK_ADDRESS_WIDTH : NATURAL;
	       BLOCK_SIZE : NATURAL);
	   port (
           clk_i : in STD_LOGIC;

           mem_init_i : in STD_LOGIC;
    
           uart_data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE-1 downto 0);
           read_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
           write_address_i : in STD_LOGIC_VECTOR (BLOCK_ADDRESS_WIDTH - 1 downto 0);
           data_imem_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0));
    end component instruction_memory;

    component data_memory is
	   generic (
	       RAM_FILENAME : STRING := "C:\\DigitalDesign\\hex\\lb.hex";
	       BLOCK_ADDRESS_WIDTH : NATURAL;
	       BLOCK_SIZE : NATURAL);
	   port (
           clk_i : in STD_LOGIC;

           mem_init_i : in STD_LOGIC;
           mem_write_i : in STD_LOGIC;
    
           uart_data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
           read_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
           write_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
           write_address_uart_i : in STD_LOGIC_VECTOR (BLOCK_ADDRESS_WIDTH-1 downto 0);
           cache_line_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
           data_dmem_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0));
    end component data_memory;
    
	component data_cache is
		generic (
			ADDRESS_WIDTH : NATURAL := 14;
			BLOCK_SIZE : NATURAL := 128;
			INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_ni : in STD_LOGIC;
			enable_mem_i : in STD_LOGIC;
			cache_req_i : in CACHE_REQUEST (MAR(ADDRESS_WIDTH - 3 - 1 downto 0));
			mem_write_i : in STD_LOGIC;
			mem_write_o : out STD_LOGIC;
			cache_line_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
			read_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8) - 1 downto 0);
			write_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8) - 1 downto 0);
			data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
			data_o : out STD_LOGIC_VECTOR (63 downto 0);
			miss_o : out STD_LOGIC);
	end component data_cache;

    component uart_receiver is
        Generic ( BAUD_RATE : INTEGER := 115200;
                  BLOCK_SIZE : NATURAL; 
                  BLOCK_ADDRESS_WIDTH : NATURAL);
        Port ( clk_i : in STD_LOGIC;
               rst_ni : in STD_LOGIC;
               exception_i : in STD_LOGIC;
               rx_i : in STD_LOGIC;
               mem_init_imem_o : out STD_LOGIC;
               mem_init_dmem_o : out STD_LOGIC;
               uart_data_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
               write_address_uart_o : out STD_LOGIC_VECTOR (BLOCK_ADDRESS_WIDTH-1 downto 0);
               rx_error_o : out STD_LOGIC);
    end component uart_receiver;
    
    component uart_transmitter is
        Generic ( BAUD_RATE : INTEGER := 115200 );
        Port ( clk_i : in STD_LOGIC;
               rst_ni : in STD_LOGIC;
               uart_tx_enable_i : in STD_LOGIC;
               DOUT_i : in STD_LOGIC_VECTOR (7 downto 0);
               tx_o : out STD_LOGIC;
               uart_tx_busy_o : out STD_LOGIC);
    end component uart_transmitter;

	component VGA is
		port (
			clk_i : in STD_LOGIC;
			rst_ni : in STD_LOGIC;

			hsync_o : out STD_LOGIC;
			vsync_o : out STD_LOGIC;

			registers_i : in reg_t;
			registers_fp_i : in reg_t;

			fcsr_i : in STD_LOGIC_VECTOR (7 downto 0);
			pc_i : in STD_LOGIC_VECTOR (63 downto 0);
			rgb_o : out STD_LOGIC_VECTOR (11 downto 0));
	end component VGA;

	signal pipeline_stall, pipeline_stall_if : STD_LOGIC := '0';
	signal load_hazard, exception, uart_tx_enable, uart_tx_busy : STD_LOGIC := '0';
	signal multicycle_op, miss_instr, miss_data : STD_LOGIC;

	signal mem_init_imem, mem_init_dmem, mem_write_ram : STD_LOGIC;
    signal mem_read, mem_write : STD_LOGIC_VECTOR (1 downto 0);
	signal x, y : STD_LOGIC_VECTOR (63 downto 0);
	signal uart_data : STD_LOGIC_VECTOR (BLOCK_SIZE-1 downto 0);

	signal alu_operator : ALU_OP;
	signal mem_operator : MEM_OP;
	
	signal result_fp : STD_LOGIC_VECTOR (63 downto 0);

	signal pc_decode, pc_execute, mem_data : STD_LOGIC_VECTOR (63 downto 0);
	signal IR, IR_decode : STD_LOGIC_VECTOR(31 downto 0);

    signal csr_operator : STD_LOGIC_VECTOR (1 downto 0);

    signal result_select : STD_LOGIC_VECTOR (3 downto 0);

	signal imm : STD_LOGIC_VECTOR (63 downto 0);

	signal pc_src, imm_src, ctrl_flow, unaligned_access : STD_LOGIC;

	signal fp_op : STD_LOGIC_VECTOR (6 downto 0);

	signal registers, registers_fp : reg_t;
	signal fcsr_data : STD_LOGIC_VECTOR (7 downto 0);

	signal reg_dst_execute : STD_LOGIC_VECTOR (4 downto 0);
	signal csr_write_execute, reg_write, reg_write_fp : STD_LOGIC := '0';

	signal fp_regs_idex : FP_IDEX;
	signal reg_dst_memory, reg_dst, reg_dst_fp : REG;
	signal csr_write_memory, csr_write : CSR;
	signal mpc, data, branch_next_pc, system_time : STD_LOGIC_VECTOR (63 downto 0);

    signal csr_exception_id : STD_LOGIC_VECTOR (3 downto 0);

	signal mem_req : MEMORY_REQUEST;
	signal cache_req : CACHE_REQUEST (MAR(ADDRESS_WIDTH - 3 - 1 downto 0));
	signal branch_inf : BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0)) := ('0', '0', '0', (others => '0'), (others => '0'));

	signal csr_read_data, csr_data, csr_data_execute : STD_LOGIC_VECTOR (63 downto 0);

    signal funct3 : STD_LOGIC_VECTOR (2 downto 0);

	signal cache_line, data_imem, data_dmem : STD_LOGIC_VECTOR(BLOCK_SIZE - 1 downto 0);
	signal instr_address : STD_LOGIC_VECTOR (ADDRESS_WIDTH - 3 downto 0);
	signal read_address_imem, read_address_dmem, write_address_uart, write_address_dmem : STD_LOGIC_VECTOR (ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8) - 1 downto 0);
	signal branch_predict_id, branch_predict : BRANCH_PREDICTION;

	signal x_mux_sel, y_mux_sel, x_fp_mux_sel, y_fp_mux_sel, z_fp_mux_sel : STD_LOGIC_VECTOR (1 downto 0);
	signal csr_mux_sel : STD_LOGIC_VECTOR (2 downto 0);
	signal csr_read_address, csr_write_address : STD_LOGIC_VECTOR (11 downto 0);

    signal frm_data : STD_LOGIC_VECTOR (2 downto 0);

	-- 7 segment display
	signal cathode : STD_LOGIC_VECTOR (7 downto 0) := (others => '1');
	signal exception_num : STD_LOGIC_VECTOR (3 downto 0) := NO_EXCEPTION;
    
    signal reg_cmp1_mem, reg_cmp1_wb, reg_cmp2_mem, reg_cmp2_wb, reg_cmp3_mem, reg_cmp3_wb, csr_cmp_mem, csr_cmp_wb : STD_LOGIC;

begin
    
	IF_Stage : fetch
	generic map(
		ADDRESS_WIDTH => ADDRESS_WIDTH,
		BHT_INDEX_WIDTH => BHT_INDEX_WIDTH
	)
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,

		pipeline_stall_i => pipeline_stall_if,
		branch_info_i => branch_inf,
		branch_predict_o => branch_predict_id,

		instr_address_o => instr_address,
		IR_i => IR,
		pc_o => pc_decode,
		IR_o => IR_decode
	);

	ID_Stage : decode
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,
		flush_i => branch_inf.mispredict,
		load_hazard_o => load_hazard,
		pipeline_stall_i => pipeline_stall,
		frm_i => frm_data,

		pc_i => pc_decode,
		pc_o => pc_execute,

		pc_src_o => pc_src,
		imm_src_o => imm_src,
		ctrl_flow_o => ctrl_flow,
			
        result_select_o => result_select,
        csr_data_i => csr_data,
		mem_read_o => mem_read,
		mem_write_o => mem_write,

		IR_i => IR_decode,
		reg_dst_i => reg_dst,
		reg_dst_fp_i => reg_dst_fp,
        funct3_o => funct3,
        
		alu_operator_o => alu_operator,
		mem_operator_o => mem_operator,

		imm_o => imm,
		
		branch_predict_i => branch_predict_id,
		branch_predict_o => branch_predict,
        branch_next_pc_o => branch_next_pc, 
		fp_regs_idex_o => fp_regs_idex,

		reg_write_o => reg_write,
        reg_mem_i => reg_dst_memory.dest,
		csr_mem_address_i => csr_write_memory.write_address,

        reg_cmp1_mem_o => reg_cmp1_mem,
        reg_cmp1_wb_o => reg_cmp1_wb,
        
        reg_cmp2_mem_o => reg_cmp2_mem,
        reg_cmp2_wb_o => reg_cmp2_wb,             

        reg_cmp3_mem_o => reg_cmp3_mem,
        reg_cmp3_wb_o => reg_cmp3_wb,
		reg_dst_o => reg_dst_execute,
		csr_operator_o => csr_operator,
		csr_read_address_o => csr_read_address,
		csr_write_address_o => csr_write_address,
		csr_exception_id_o => csr_exception_id,
		csr_data_o => csr_data_execute,
		csr_write_o => csr_write_execute,
		
		csr_cmp_mem_o => csr_cmp_mem,
		csr_cmp_wb_o => csr_cmp_wb,
		registers_o => registers,
		registers_fp_o => registers_fp,
		x_o => x,
		y_o => y
	);

	EX_Stage : execute
	generic map(
		ADDRESS_WIDTH => ADDRESS_WIDTH,
		BHT_INDEX_WIDTH => BHT_INDEX_WIDTH
	)
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,
		pipeline_stall_i => pipeline_stall,
		pc_i => pc_execute,
		pc_src_i => pc_src,
		imm_src_i => imm_src,
		ctrl_flow_i => ctrl_flow,
	    result_select_i => result_select,
	
		multicycle_op_o => multicycle_op,
        funct3_i => funct3,
		imm_i => imm,
		alu_operator_i => alu_operator,
		mem_operator_i => mem_operator,

		mem_req_o => mem_req,

		reg_dst_i => reg_dst_execute,
		reg_dst_o => reg_dst_memory,
		reg_write_i => reg_write,

		reg_write_fp_o => reg_write_fp,
        result_fp_o => result_fp,

		mem_write_i => mem_write,
		mem_read_i => mem_read,
              
		branch_predict_i => branch_predict,
		branch_info_o => branch_inf,
        branch_next_pc_i => branch_next_pc,
		fp_regs_idex_i => fp_regs_idex,

        uart_tx_enable_o => uart_tx_enable,

		csr_write_i => csr_write_execute,
		csr_write_address_i => csr_write_address,
        csr_exception_id_i => csr_exception_id,
        csr_data_i => csr_data_execute,
		
		csr_o => csr_write_memory,
	    csr_data_wb_i => csr_write.data,
	    csr_operator_i => csr_operator,
	    
        x_i => x,
        y_i => y,

        csr_mux_sel_i => csr_mux_sel,
	    
	    result_fwd_wb_i => reg_dst.data,
	    result_fwd_fp_wb_i => reg_dst_fp.data,
	    x_mux_sel_i => x_mux_sel,
	    y_mux_sel_i => y_mux_sel,
	    x_fp_mux_sel_i => x_fp_mux_sel,
	    y_fp_mux_sel_i => y_fp_mux_sel,
	    z_fp_mux_sel_i => z_fp_mux_sel
	);

	MEM_Stage : memory
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,

		mem_read_i => mem_req.read,
		pipeline_stall_i => pipeline_stall,

		reg_write_fp_i => reg_write_fp,
		reg_dst_fp_o => reg_dst_fp,

		reg_dst_i => reg_dst_memory,
		reg_dst_o => reg_dst,
        result_fp_i => result_fp,
		mem_data_i => mem_data,
		csr_i => csr_write_memory,
		csr_o => csr_write
	);

	CSRS : csr_regfile
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,
		csr_i => csr_write,
		csr_read_address_i => csr_read_address,
		csr_data_o => csr_read_data,
		mpc_o => mpc,
		fcsr_o => fcsr_data,
		exception_num_o => exception_num,
		system_time_o => system_time
	);
	
	FU : forwarding_unit
    port map (
        reg_cmp1_mem_i => reg_cmp1_mem,
        reg_cmp1_wb_i => reg_cmp1_wb,
        
        reg_cmp2_mem_i => reg_cmp2_mem,
        reg_cmp2_wb_i => reg_cmp2_wb,

        reg_cmp3_mem_i => reg_cmp3_mem,
        reg_cmp3_wb_i => reg_cmp3_wb,

        csr_cmp_mem_i => csr_cmp_mem,
        csr_cmp_wb_i => csr_cmp_wb,

        reg_write_mem_i => reg_dst_memory.write,
        reg_write_wb_i => reg_dst.write,
        
        reg_fp_write_mem_i => reg_write_fp,
        reg_fp_write_wb_i => reg_dst_fp.write,
        
        csr_write_i => result_select(3),
        csr_write_mem_i => csr_write_memory.write,
        csr_write_wb_i => csr_write.write,
                
        x_mux_sel_o => x_mux_sel,
        y_mux_sel_o => y_mux_sel,
     
        x_fp_mux_sel_o => x_fp_mux_sel,
        y_fp_mux_sel_o => y_fp_mux_sel,
        z_fp_mux_sel_o => z_fp_mux_sel,
        
        csr_mux_sel_o => csr_mux_sel  
    );    
    
	LSU : load_store_unit
	generic map(ADDRESS_WIDTH => ADDRESS_WIDTH)
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,
		exception_i => exception,
		enable_mem_i => mem_req.read or mem_req.write,
		MAR_i => mem_req.MAR(ADDRESS_WIDTH-1 downto 0),
		MDR_i => mem_req.MDR,
		memory_operation_i => mem_req.MEMOp,
	    unaligned_access_o => unaligned_access,
		miss_i => miss_data,
		cache_req_o => cache_req,
		data_i => data,
		data_o => mem_data
	); 

    IMEM: instruction_memory 
	generic map(
		RAM_FILENAME => RAM_FILENAME,
		BLOCK_SIZE => BLOCK_SIZE,
		BLOCK_ADDRESS_WIDTH => ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8)
    )
	port map(
		clk_i => clk_i,

		mem_init_i => mem_init_imem,

		uart_data_i => uart_data,
		data_imem_o => data_imem,
    
		read_address_i => read_address_imem,
		write_address_i => write_address_uart
    );
    
    DMEM: data_memory 
	generic map(
	    RAM_FILENAME => RAM_FILENAME,
		BLOCK_SIZE => BLOCK_SIZE,
		BLOCK_ADDRESS_WIDTH => ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8)
    )
	port map(
		clk_i => clk_i,

		mem_init_i => mem_init_dmem,
		mem_write_i => mem_write_ram,
        
		uart_data_i => uart_data,
		data_dmem_o => data_dmem,
       
		read_address_i => read_address_dmem,
		write_address_i => write_address_dmem,
		write_address_uart_i => write_address_uart,
		cache_line_i => cache_line
    );

	ICACHE : instruction_cache
	generic map(
		ADDRESS_WIDTH => ADDRESS_WIDTH,
		BLOCK_SIZE => BLOCK_SIZE,
		INDEX_WIDTH => INDEX_WIDTH)
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,

		instr_address_i => instr_address,
		data_i => data_imem,
		read_address_o => read_address_imem,
		IR_o => IR,
		miss_o => miss_instr
	);

	DCACHE : data_cache
	generic map(
		ADDRESS_WIDTH => ADDRESS_WIDTH,
		BLOCK_SIZE => BLOCK_SIZE,
		INDEX_WIDTH => INDEX_WIDTH)
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,
		enable_mem_i => mem_req.read or mem_req.write,
		cache_req_i => cache_req,
		mem_write_i => mem_req.write,
		mem_write_o => mem_write_ram,
		data_i => data_dmem,
		data_o => data,
		read_address_o => read_address_dmem,
		write_address_o => write_address_dmem,
		miss_o => miss_data,
		cache_line_o => cache_line
	);

	UART_RX : uart_receiver
    generic map (
        BLOCK_SIZE => BLOCK_SIZE,
		BLOCK_ADDRESS_WIDTH => ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8))
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,
		rx_i => rx_i,
        exception_i => exception,
		rx_error_o => LED_o(1),
		mem_init_imem_o => mem_init_imem,
		mem_init_dmem_o => mem_init_dmem,
		uart_data_o => uart_data,
		write_address_uart_o => write_address_uart
	);
	
	UART_TX : uart_transmitter
    port map ( 
        clk_i => clk_i,
        rst_ni => rst_ni,
        uart_tx_enable_i => uart_tx_enable,
        tx_o => tx_o,
        uart_tx_busy_o => uart_tx_busy,
        DOUT_i => mem_req.MDR(7 downto 0)
    );

	C_VGA : VGA
	port map(
		clk_i => clk_i,
		rst_ni => rst_ni,

		pc_i => mpc,
		fcsr_i => fcsr_data,
		registers_i => registers,
		registers_fp_i => registers_fp,
		
		hsync_o => hsync_o,
		vsync_o => vsync_o,
	    rgb_o => rgb_o
	);

	exception <= nand exception_num;
	
	with exception_num select 
	   cathode <= "10000001" when "0000",
	              "01001001" when "0010",
	              "01100001" when "0011",
	              "00000001" when "1000",
	              "11111111" when others;
	
		
	-- HAZARD AND STALL CHECK  
 
	pipeline_stall <= multicycle_op or miss_data or miss_instr or exception or unaligned_access or uart_tx_busy;
	
	pipeline_stall_if <= pipeline_stall or load_hazard;

    frm_data <= 
        csr_data_execute(2 downto 0) when csr_write_address = FRM and csr_write_execute = '1' else
        csr_data_execute(7 downto 5) when csr_write_address = FCSR and csr_write_execute = '1' else
        csr_write_memory.data(2 downto 0) when csr_write_memory.write_address = FRM and csr_write_memory.write = '1' else
        csr_write_memory.data(7 downto 5) when csr_write_memory.write_address = FCSR and csr_write_memory.write = '1' else
        csr_write.data(2 downto 0) when csr_write.write_address = FRM and csr_write.write = '1' else
        csr_write.data(7 downto 5) when csr_write.write_address = FCSR and csr_write.write = '1' else fcsr(7 downto 5);
   
    csr_data <= csr_write.data when csr_write.write_address = csr_read_address and csr_write.write = '1' else csr_read_data;
                        
	LED_o(0) <= not rst_ni;
	LED_o(2) <= exception;

	anode_o <= X"FE";
	cathode_o <= cathode;
	
    -- !!!!! SIMULATION ONLY !!!
 --   test_number_o <= registers(10);
 --   system_time_o <= system_time;
    
end behavioral; 