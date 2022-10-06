library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity execute is
	generic (
		ADDRESS_WIDTH : NATURAL := 14;
		BHT_INDEX_WIDTH : NATURAL := 2);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;

		pipeline_stall_i : in STD_LOGIC;

		mem_read_i : in STD_LOGIC;

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

		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);

		result_fwd_o : out STD_LOGIC_VECTOR (63 downto 0);
		result_fp_fwd_o : out STD_LOGIC_VECTOR (63 downto 0);

		mem_req_o : out MEMORY_REQUEST;

		fp_regs_IDEX_i : in FP_IDEX;
		reg_write_fp_o : out STD_LOGIC;

		csr_fwd_o : out CSR;
		csr_data_i : in STD_LOGIC_VECTOR (63 downto 0);

		csr_i : in CSR;
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
			pc_i : in STD_LOGIC_VECTOR (63 downto 0);
			offset_i : in STD_LOGIC_VECTOR (63 downto 0);
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
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			x_i : in STD_LOGIC_VECTOR (63 downto 0);
			y_i : in STD_LOGIC_VECTOR (63 downto 0);
			z_i : in STD_LOGIC_VECTOR (63 downto 0);
			x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
			result_o : out STD_LOGIC_VECTOR (63 downto 0);
			fp_valid_o : out STD_LOGIC;
			fflags_o : out STD_LOGIC_VECTOR (4 downto 0));
	end component FPU;

	signal result, result_fp, result_mul, result_div, Y_fp, x, y, z, csr_data, MDR : STD_LOGIC_VECTOR (63 downto 0);
	signal fp_valid, instr_misaligned, instr_fault, load_fault, store_fault, mem_write_reg, reg_write_fp : STD_LOGIC := '0';
	signal alu_cmp, multiply, divide, div_valid, mul_valid, enable_mul, enable_div, enable_fp, enable_mem, csr_write : STD_LOGIC := '0';
	signal fflags_r : STD_LOGIC_VECTOR (4 downto 0);
	signal column : ram_column;
	signal mem_req : MEMORY_REQUEST (MAR (ADDRESS_WIDTH - 1 downto 0));
	signal reg_dst : REG;
	signal MAR : STD_LOGIC_VECTOR (ADDRESS_WIDTH - 1 downto 0);
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
		x_i => x_i,
		pc_i => pc_i,
		offset_i => imm_i,
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
		x_i => x_i,
		y_i => y_i,
		op_i => alu_operator_i,
		mul_valid_o => mul_valid,
		result_o => result_mul
	);

	DIV : divider
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		enable_i => enable_div,
		x_i => x_i,
		y_i => y_i,
		op_i => alu_operator_i,
		div_valid_o => div_valid,
		z_o => result_div
	);

	RISCV_FPU : FPU
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		fp_op_i => fp_regs_IDEX_i.fp_op,
		fp_precision_i => fp_regs_IDEX_i.fp_precision,
		rm_i => fp_regs_IDEX_i.rm,
		x_i => fp_regs_IDEX_i.x,
		y_i => fp_regs_IDEX_i.y,
		z_i => imm_i,
		x_int_i => x_i,
		result_o => result_fp,
		fp_valid_o => fp_valid,
		fflags_o => fflags_r
	);

	x <= pc_i when pc_src_i = '1' else
		x_i;

	y <= imm_i when imm_src_i = '1' else
		y_i;

	result <= result_mul when multiply = '1' else
		result_div when divide = '1' else
		result_fp when fp_i = '1' else
		z;

	instr_misaligned <= (or branch_inf.target_address(1 downto 0)) and branch_inf.mispredict;
	instr_fault <= (or branch_inf.target_address(14 downto 13)) and branch_inf.mispredict;
	load_fault <= (nor z(14 downto 13)) and mem_read_i;
	store_fault <= (nor z(14 downto 13)) and mem_write_i;

	--    Y_fp <= X"FFFFFFFF" & fp_regs_IDEX_i.y(31 downto 0) when (or fp_regs_IDEX_i.y(63 downto 31)) = '0' else 
	--                                       fp_regs_IDEX_i.y; 

	MAR <= z(ADDRESS_WIDTH - 1 + 3 downto 3);
	column <= to_integer(unsigned(Z(2 downto 0)));

	with mem_operator_i select MDR <= y_fp when LSU_FSW | LSU_FSD,
		y_i when others;

	REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				reg_dst.write <= '0';
				reg_write_fp <= '0';
				mem_req.enable_mem <= '0';
				mem_write_reg <= '0';
			else
				if pipeline_stall_i = '0' or branch_predict_i.cf_type = "01" then
					reg_dst.write <= reg_write_i;
					reg_dst.dest <= reg_dst_i;
					reg_dst.data <= result;

					reg_write_fp <= fp_regs_IDEX_i.write;
					mem_write_reg <= mem_write_i;
					mem_req.enable_mem <= enable_mem;
					mem_req.MEMOp <= mem_operator_i;

					mem_req.column <= column;
					mem_req.MAR <= MAR;
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

	enable_mem <= (mem_write_i or mem_read_i) and (z(14) or z(13));
	enable_mul <= multiply and (not mul_valid);
	enable_div <= divide and (not div_valid);
	enable_fp <= fp_i and (not fp_valid);

	multicycle_op_o <= enable_mul or enable_div or enable_fp;

	reg_dst_o <= reg_dst;

	result_fwd_o <= result;
	--result_fp_fwd_o <= result_fp;

	reg_write_fp_o <= reg_write_fp;

	csr.exception_id <= INSTRUCTION_ADDRESS_MISALIGN when instr_misaligned = '1' else
	INSTRUCTION_ACCESS_FAULT when instr_fault = '1' else
	LOAD_ACCESS_FAULT when load_fault = '1' else
	STORE_ACCESS_FAULT when store_fault = '1' else csr_i.exception_id;
	csr.write <= csr_i.write or fp_i;
	csr.write_addr <= FFLAGS when fp_i = '1' else csr_i.write_addr;
	csr.data <= (63 downto 5 => '0') & fflags_r when fp_i = '1' else csr_i.data;
	csr.epc <= csr_i.epc;

	--csr_o <= csr_reg;
	--csr_fwd_o <= csr;

	mem_req_o <= mem_req;
	mem_write_o <= mem_write_reg;
	branch_info_o <= branch_inf;
end behavioral;