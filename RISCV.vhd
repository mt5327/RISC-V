library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity RISCV is
	generic (
		RAM_FILENAME : STRING := "C:\\DigitalDesign\\hex\\simple.hex";
		ADDRESS_WIDTH : NATURAL := 18;
		BLOCK_SIZE : NATURAL := 256;
		INDEX_WIDTH : NATURAL := 2;
		BHT_INDEX_WIDTH : NATURAL := 2);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		-- UART
		rx_i : in STD_LOGIC;
		-- VGA            
		hsync_o : out STD_LOGIC;
		vsync_o : out STD_LOGIC;
		rgb_o : out STD_LOGIC_VECTOR (11 downto 0);

		LED_o : out STD_LOGIC_VECTOR (3 downto 0);
       
		anode_o : out STD_LOGIC;
		cathode_o : out STD_LOGIC_VECTOR (6 downto 0);
        
	-- !!! SIMULATION ONLY !!! 
	    test_number_o : out STD_LOGIC_VECTOR (63 downto 0)); 

end RISCV;

architecture behavioral of RISCV is

	component fetch is
		generic (
			ADDRESS_WIDTH : NATURAL := 18;
			BHT_INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;

			cpu_enable_i : in STD_LOGIC;
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
            rst_i : in STD_LOGIC;
            cpu_enable_i: in STD_LOGIC;
            flush_i : in STD_LOGIC;
            pipeline_stall_i : in STD_LOGIC;
    
            IR_i : in STD_LOGIC_VECTOR (31 downto 0);
            frm_i : in STD_LOGIC_VECTOR (2 downto 0);
            pc_i : in STD_LOGIC_VECTOR (63 downto 0);
    
            branch_predict_i : in BRANCH_PREDICTION;
    
            csr_data_i : in STD_LOGIC_VECTOR (63 downto 0);
            csr_read_addr_o : out STD_LOGIC_VECTOR (11 downto 0);
            
            pc_o : out STD_LOGIC_VECTOR (63 downto 0);
    
            branch_predict_o : out BRANCH_PREDICTION;
    
            mem_read_o : out STD_LOGIC;
            mem_write_o : out STD_LOGIC;
    
            pc_src_o : out STD_LOGIC;
            imm_src_o : out STD_LOGIC;
            ctrl_flow_o : out STD_LOGIC;
            fp_o : out STD_LOGIC;
    
            r4_type_o : out STD_LOGIC;
    
            imm_o : out STD_LOGIC_VECTOR (63 downto 0);
    
            alu_operator_o : out ALU_OP;
            mem_operator_o : out MEM_OP;
    
            reg_write_o : out STD_LOGIC;
            reg_dst_o : out STD_LOGIC_VECTOR (4 downto 0);
            
            reg_dst_i : in REG;
            reg_dst_fp_i : in REG;
            
            reg_mem_i : in STD_LOGIC_VECTOR (4 downto 0);
            x_o : out STD_LOGIC_VECTOR (63 downto 0);
            y_o : out STD_LOGIC_VECTOR (63 downto 0);
        
            rs1_valid_o : out STD_LOGIC;
            rs2_valid_o : out STD_LOGIC;
            
            reg_cmp1_mem_o : out STD_LOGIC;
            reg_cmp1_wb_o : out STD_LOGIC;
    
            reg_cmp2_mem_o : out STD_LOGIC;
            reg_cmp2_wb_o : out STD_LOGIC;
                    
            registers_o : out reg_t;
            registers_fp_o : out reg_t;
            
            reg_src3_o : out STD_LOGIC_VECTOR (4 downto 0);
    
            fp_regs_IDEX_o : out FP_IDEX;
            csr_o : out CSR);
	end component decode;

	component execute is
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
            x_fp_mux_sel_i, y_fp_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
            
            mem_req_o : out MEMORY_REQUEST;
    
            fp_regs_idex_i : in FP_IDEX;
            reg_write_fp_o : out STD_LOGIC;
    
            csr_mux_sel_i : in STD_LOGIC_VECTOR (1 downto 0);
            csr_data_wb_i : in STD_LOGIC_VECTOR (63 downto 0);
    
            csr_i : in CSR;
            csr_o : out CSR);
	end component execute;

	component memory is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;

			pipeline_stall_i : in STD_LOGIC;
			mem_read_i : in STD_LOGIC;
		    result_fp_i : in STD_LOGIC_VECTOR (63 downto 0);

			reg_dst_i : in REG;
			reg_write_fp_i : in STD_LOGIC;

			mem_data_i : in STD_LOGIC_VECTOR (63 downto 0);

			reg_dst_o : out REG;
			reg_dst_fp_o : out REG;

			csr_i : in CSR;
			csr_o : out CSR);
	end component memory;

	component csr_regfile is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			cpu_enable_i : in STD_LOGIC;
			csr_i : in CSR;
			CSR_read_addr_i : in STD_LOGIC_VECTOR (11 downto 0);
			CSR_data_o : out STD_LOGIC_VECTOR (63 downto 0);
			mpc_o : out STD_LOGIC_VECTOR (63 downto 0);
			fcsr_o : out STD_LOGIC_VECTOR (7 downto 0);
			exception_num_o : out STD_LOGIC_VECTOR (3 downto 0));
	end component csr_regfile;

	component load_store_unit is
		generic (ADDRESS_WIDTH : NATURAL := 14);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			exception_i : in STD_LOGIC;
		    unaligned_access_o : out STD_LOGIC;
			mem_req_i : in MEMORY_REQUEST;
    		MAR_i : in STD_LOGIC_VECTOR (ADDRESS_WIDTH - 1 downto 0);
			cache_req_o : out CACHE_REQUEST (MAR(ADDRESS_WIDTH - 3 - 1 downto 0));
			data_i : in STD_LOGIC_VECTOR (63 downto 0);
			data_o : out STD_LOGIC_VECTOR (63 downto 0));
	end component load_store_unit;

	component RAM is
		generic (
			RAM_FILENAME : STRING := "C:\\cygwin64\\riscv\\hex\\fact.hex";
			BLOCK_ADDRESS_WIDTH : NATURAL;
			BLOCK_SIZE : NATURAL);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;

			exception_i : in STD_LOGIC;
			mem_init_i : in STD_LOGIC;
			mem_write_i : in STD_LOGIC;
			UART_data_i : in STD_LOGIC_VECTOR (3 downto 0);
			read_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
			write_address_i : in STD_LOGIC_VECTOR(BLOCK_ADDRESS_WIDTH - 1 downto 0);
 
			cache_line_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
			DOUT_o : out STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0));
	end component;

	component instruction_cache is
		generic (
			ADDRESS_WIDTH : NATURAL := 18;
			BLOCK_SIZE : NATURAL := 256;
			INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;

			instr_address_i : in STD_LOGIC_VECTOR (ADDRESS_WIDTH - 3 downto 0);
			read_address_o : out STD_LOGIC_VECTOR (ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8) - 1 downto 0);
			data_i : in STD_LOGIC_VECTOR (BLOCK_SIZE - 1 downto 0);
			IR_o : out STD_LOGIC_VECTOR (31 downto 0);
			miss_o : out STD_LOGIC);
	end component instruction_cache;

	component data_cache is
		generic (
			ADDRESS_WIDTH : NATURAL := 14;
			BLOCK_SIZE : NATURAL := 128;
			INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			enable_mem_i : in STD_LOGIC;
			memory_busy_i : in STD_LOGIC;
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
	
	component UART is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC; 
			rx_i : in STD_LOGIC;
			rx_error_o : out STD_LOGIC;
			mem_write_o : out STD_LOGIC;
			cpu_enable_o : out STD_LOGIC;
			DIN_o : out STD_LOGIC_VECTOR (3 downto 0));
	end component UART;

	component VGA is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;

			hsync_o : out STD_LOGIC;
			vsync_o : out STD_LOGIC;

			registers_i : in reg_t;
			registers_fp_i : in reg_t;

			fcsr_i : in STD_LOGIC_VECTOR (7 downto 0);
			pc_i : in STD_LOGIC_VECTOR (63 downto 0);
			rgb_o : out STD_LOGIC_VECTOR (11 downto 0));
	end component VGA;

	signal pipeline_stall, pipeline_stall_if, pipeline_stall_id : STD_LOGIC := '0';
	signal load_hazard, exception, cpu_enable : STD_LOGIC := '0';
	signal multicycle_op, r4_type, miss_instr, miss_data, id_flush : STD_LOGIC;

	signal mem_init, mem_write_execute, mem_write_lsu, mem_write, mem_read_execute, mem_read_memory : STD_LOGIC;

	signal x_fwd, y_fwd, result_fwd, x, y, reg_src1_data, reg_src2_data : STD_LOGIC_VECTOR (63 downto 0);
	signal x_fwd_fp, y_fwd_fp, z_fwd_fp: STD_LOGIC_VECTOR (63 downto 0);
	signal UART_data : STD_LOGIC_VECTOR (3 downto 0);

	signal alu_operator : ALU_OP;
	signal mem_operator : MEM_OP;
	
	signal result_fp : STD_LOGIC_VECTOR (63 downto 0);

	signal pc_decode, pc_execute, mem_data : STD_LOGIC_VECTOR (63 downto 0);
	signal IR, IR_decode : STD_LOGIC_VECTOR(31 downto 0);

	signal imm : STD_LOGIC_VECTOR (63 downto 0);

	signal pc_src, imm_src, ctrl_flow, float, unaligned_access : STD_LOGIC;

	signal fp_op : STD_LOGIC_VECTOR (6 downto 0);

	signal registers, registers_fp : reg_t;
	signal fcsr : STD_LOGIC_VECTOR (7 downto 0);

	signal reg_src3, reg_dst_execute : STD_LOGIC_VECTOR (4 downto 0);
	signal reg_write, reg_write_fp, reg_src1_valid, reg_src2_valid : STD_LOGIC := '0';

	signal fp_regs_idex : FP_IDEX;
	signal reg_dst_memory, reg_dst, reg_dst_fp : REG;
	signal csr_write_execute, csr_write_memory, csr_write : CSR;
	signal mpc, data : STD_LOGIC_VECTOR (63 downto 0);

	signal mem_req : MEMORY_REQUEST;
	signal cache_req : CACHE_REQUEST (MAR(ADDRESS_WIDTH - 3 - 1 downto 0));
	signal branch_inf : BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0)) := ('0', '0', '0', (others => '0'), (others => '0'));

	signal csr_read_data, csr_data : STD_LOGIC_VECTOR (63 downto 0);

	signal counter : unsigned (19 downto 0) := (others => '0');

	signal cache_line, DOUT : STD_LOGIC_VECTOR(BLOCK_SIZE - 1 downto 0);
	signal instr_address : STD_LOGIC_VECTOR (ADDRESS_WIDTH - 3 downto 0);
	signal read_address, read_address_instr, read_address_data, write_address : STD_LOGIC_VECTOR (ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8) - 1 downto 0);
	signal branch_predict_id, branch_predict : BRANCH_PREDICTION;

	signal x_mux_sel, y_mux_sel, x_fp_mux_sel, y_fp_mux_sel, csr_mux_sel : STD_LOGIC_VECTOR (1 downto 0);
	signal CSR_read_addr : STD_LOGIC_VECTOR (11 downto 0);

	-- 7 segment display
	signal cathode : STD_LOGIC_VECTOR (6 downto 0) := (others => '0');
	signal exception_num : STD_LOGIC_VECTOR (3 downto 0) := NO_EXCEPTION;
	signal anode : STD_LOGIC := '1';

	signal x_fwd_mem, x_fwd_wb, y_fwd_mem, y_fwd_wb, x_fp_fwd_mem, x_fp_fwd_wb, y_fp_fwd_mem, y_fp_fwd_wb, csr_fwd_mem, csr_fwd_wb : STD_LOGIC;

    signal  reg_cmp1_mem, reg_cmp1_wb, reg_cmp2_mem, reg_cmp2_wb : STD_LOGIC;

begin

	IF_Stage : fetch
	generic map(
		ADDRESS_WIDTH => ADDRESS_WIDTH,
		BHT_INDEX_WIDTH => BHT_INDEX_WIDTH
	)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,

		cpu_enable_i => cpu_enable,
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
		rst_i => rst_i,
		cpu_enable_i => cpu_enable,
		flush_i => id_flush,
		pipeline_stall_i => pipeline_stall,
		frm_i => fcsr(7 downto 5),

		pc_i => pc_decode,
		pc_o => pc_execute,

		pc_src_o => pc_src,
		imm_src_o => imm_src,
		ctrl_flow_o => ctrl_flow,
		fp_o => float,
        csr_data_i => csr_read_data,
		mem_read_o => mem_read_execute,
		mem_write_o => mem_write_execute,

 		r4_type_o => r4_type,

		IR_i => IR_decode,
		reg_dst_i => reg_dst,
		reg_dst_fp_i => reg_dst_fp,

		alu_operator_o => alu_operator,
		mem_operator_o => mem_operator,

		imm_o => imm,

		branch_predict_i => branch_predict_id,
		branch_predict_o => branch_predict,

		fp_regs_idex_o => fp_regs_idex,

		reg_write_o => reg_write,
        reg_mem_i => reg_dst_memory.dest,
		reg_src3_o => reg_src3,

        reg_cmp1_mem_o => reg_cmp1_mem,
        reg_cmp1_wb_o => reg_cmp1_wb,
    
        rs1_valid_o => reg_src1_valid,
        rs2_valid_o => reg_src2_valid,
    
        reg_cmp2_mem_o => reg_cmp2_mem,
        reg_cmp2_wb_o => reg_cmp2_wb,             

		reg_dst_o => reg_dst_execute,
		csr_o => csr_write_execute,
		CSR_read_addr_o => CSR_read_addr,
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
		rst_i => rst_i,
		pipeline_stall_i => pipeline_stall,

		pc_i => pc_execute,
		pc_src_i => pc_src,
		imm_src_i => imm_src,
		ctrl_flow_i => ctrl_flow,
		fp_i => float,

		multicycle_op_o => multicycle_op,

		imm_i => imm,
		alu_operator_i => alu_operator,
		mem_operator_i => mem_operator,

		mem_req_o => mem_req,
  
		reg_dst_i => reg_dst_execute,
		reg_dst_o => reg_dst_memory,
		reg_write_i => reg_write,

		reg_write_fp_o => reg_write_fp,
        result_fp_o => result_fp,

		mem_write_i => mem_write_execute,
		mem_write_o => mem_write_lsu,

		mem_read_i => mem_read_execute,
        mem_read_o => mem_read_memory,
        
		branch_predict_i => branch_predict,
		branch_info_o => branch_inf,

		fp_regs_idex_i => fp_regs_idex,

		csr_i => csr_write_execute,
		csr_o => csr_write_memory,
	    csr_data_wb_i => csr_write.data,
	    
        x_i => x,
        y_i => y,

        csr_mux_sel_i => csr_mux_sel,
	    result_fwd_wb_i => reg_dst.data,
	    result_fwd_fp_wb_i => reg_dst_fp.data,
	    x_mux_sel_i => x_mux_sel,
	    y_mux_sel_i => y_mux_sel,
	    x_fp_mux_sel_i => x_fp_mux_sel,
	    y_fp_mux_sel_i => y_fp_mux_sel
	);

	MEM_Stage : memory
	port map(
		clk_i => clk_i,
		rst_i => rst_i,

		mem_read_i => mem_req.enable_mem,
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
		rst_i => rst_i,
		cpu_enable_i => cpu_enable,
		csr_i => csr_write,
		csr_read_addr_i => CSR_read_addr,
		CSR_data_o => CSR_read_data,
		mpc_o => mpc,
		fcsr_o => fcsr,
		exception_num_o => exception_num
	);

	LSU : load_store_unit
	generic map(ADDRESS_WIDTH => ADDRESS_WIDTH)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		exception_i => exception,
		mem_req_i => mem_req,
	    unaligned_access_o => unaligned_access,
		MAR_i => reg_dst_memory.data(ADDRESS_WIDTH - 1 downto 0),
		cache_req_o => cache_req,
		data_i => data,
		data_o => mem_data
	);

	C_RAM : RAM
	generic map(
		BLOCK_SIZE => BLOCK_SIZE,
		BLOCK_ADDRESS_WIDTH => ADDRESS_WIDTH - num_bits(BLOCK_SIZE/8),
		RAM_FILENAME => RAM_FILENAME)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,

		mem_init_i => mem_init,
		mem_write_i => mem_write,
		exception_i => exception,

		UART_data_i => UART_data,
		DOUT_o => DOUT,

		read_address_i => read_address,
		write_address_i => write_address,
		cache_line_i => cache_line
	);

	ICACHE : instruction_cache
	generic map(
		ADDRESS_WIDTH => ADDRESS_WIDTH,
		BLOCK_SIZE => BLOCK_SIZE,
		INDEX_WIDTH => INDEX_WIDTH)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,

		instr_address_i => instr_address,
		data_i => DOUT,
		read_address_o => read_address_instr,
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
		rst_i => rst_i,
		enable_mem_i => mem_req.enable_mem,
		cache_req_i => cache_req,
		memory_busy_i => miss_instr,
		mem_write_i => mem_write_lsu,
		mem_write_o => mem_write,
		data_i => DOUT,
		data_o => data,
		read_address_o => read_address_data,
		write_address_o => write_address,
		miss_o => miss_data,
		cache_line_o => cache_line
	);

	C_UART : UART
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		rx_i => rx_i,

		rx_error_o => LED_o(2),
		cpu_enable_o => cpu_enable,
		mem_write_o => mem_init,
		DIN_o => UART_data
	);

	C_VGA : VGA
	port map(
		clk_i => clk_i,
		rst_i => rst_i,


		pc_i => mpc,
		fcsr_i => fcsr,
		registers_i => registers,
		registers_fp_i => registers_fp,
		
		hsync_o => hsync_o,
		vsync_o => vsync_o,
	    rgb_o => rgb_o
	);

    ANODE_ENABLE : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                anode <= '1';
                counter <= (others => '0');
            else
                if counter = REFRESH_RATE then
                    anode <= not anode;
                    counter <= (others => '0');
                else
                    counter <= counter + 1;
                end if;
            end if;
        end if;
    end process;

	exception <= nand exception_num;
	
	with exception_num select 
	   cathode <= "1000000" when "0000",
	              "0100100" when "0010",
	              "0110000" when "0011",
	              "0000000" when "1000",
	              "1111111" when others;
	
    read_address <= read_address_instr when miss_instr = '1' else
                    read_address_data;
	
	-- HAZARD AND STALL CHECK  
	--load_hazard <= '1' when mem_read = '1' and (x_fwd_ex = '1' or y_fwd_ex = '1') else '0';

	pipeline_stall <= multicycle_op or miss_data or miss_instr or exception or unaligned_access;
	pipeline_stall_if <= pipeline_stall; -- or load_hazard;
	--  pipeline_stall_id <= 

	id_flush <= branch_inf.mispredict;

	x_fwd_mem <= reg_cmp1_mem and reg_src1_valid and reg_dst_memory.write;
	x_fwd_wb <= reg_cmp1_wb and reg_src1_valid and reg_dst.write;

    y_fwd_mem <= reg_cmp2_mem and reg_src2_valid and reg_dst_memory.write;
	y_fwd_wb <= reg_cmp2_wb and reg_src2_valid and reg_dst.write;

    x_mux_sel <= "01" when x_fwd_mem = '1' else                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                 "10" when x_fwd_wb = '1' else 
                 "11";
                 
    y_mux_sel <= "01" when y_fwd_mem = '1' else 
                 "10" when y_fwd_wb = '1' else 
                 "11";

 	-- FORWARDING LOGIC FLOATING POINT
	x_fp_fwd_mem <= fp_regs_idex.write and reg_cmp1_mem and reg_write_fp;
	x_fp_fwd_wb <= fp_regs_idex.write and reg_cmp1_wb and reg_dst_fp.write;

    y_fp_fwd_mem <= fp_regs_idex.write and reg_cmp2_mem and reg_write_fp;
	y_fp_fwd_wb <= fp_regs_idex.write and reg_cmp2_wb and reg_dst_fp.write;

    x_fp_mux_sel <= "01" when x_fp_fwd_mem = '1' else 
                      "10" when x_fp_fwd_wb = '1' else 
                      "11";
                 
    y_fp_mux_sel <= "01" when y_fp_fwd_mem = '1' else 
                      "10" when y_fp_fwd_wb = '1' else 
                      "11"; 
                      
    -- FORWARDING LOGIC CSR
    csr_fwd_mem <= '1' when csr_write_memory.write = '1' and csr_write_execute.write_addr = csr_write_memory.write_addr else '0'; 
    csr_fwd_wb <= '1' when csr_write.write = '1' and csr_write_execute.write_addr = csr_write.write_addr else '0';
    
    csr_mux_sel <= "01" when csr_fwd_mem = '1' else
                   "10" when csr_fwd_wb = '1' else
                   "11";
	LED_o(0) <= rst_i;
	LED_o(1) <= cpu_enable;
	LED_o(3) <= exception;

	anode_o <= anode;
	cathode_o <= cathode;
	
    -- !!!!! SIMULATION ONLY !!!
	 test_number_o <= registers(10);

end behavioral;