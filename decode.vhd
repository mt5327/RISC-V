library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity decode is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
        cpu_enable_i: in STD_LOGIC;
        load_hazard_o : out STD_LOGIC;

		flush_i : in STD_LOGIC;
		pipeline_stall_i : in STD_LOGIC;

		IR_i : in STD_LOGIC_VECTOR (31 downto 0);
		frm_i : in STD_LOGIC_VECTOR (2 downto 0);
		funct3_o : out STD_LOGIC_VECTOR (2 downto 0);
		pc_i : in STD_LOGIC_VECTOR (63 downto 0);

		branch_predict_i : in BRANCH_PREDICTION;

		csr_data_i : in STD_LOGIC_VECTOR (63 downto 0);
        csr_read_addr_o : out STD_LOGIC_VECTOR (11 downto 0);
		
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
		csr_mem_addr_i : in STD_LOGIC_VECTOR (11 downto 0);
		
		x_o : out STD_LOGIC_VECTOR (63 downto 0);
	    y_o : out STD_LOGIC_VECTOR (63 downto 0);
        
        csr_write_o : out STD_LOGIC;
        csr_write_addr_o : out STD_LOGIC_VECTOR (11 downto 0);
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
end decode;

architecture behavioral of decode is

	signal x, y, x_data, y_data, x_data_reg, y_data_reg, x_fp, y_fp, z_fp, pc, imm, imm_reg, branch_target_address : STD_LOGIC_VECTOR (63 downto 0);
    signal branch_next_pc, branch_next_pc_reg : STD_LOGIC_VECTOR (63 downto 0);
	signal reg_dst : STD_LOGIC_VECTOR (4 downto 0);
	signal pc_src, pc_src_reg, imm_src, imm_src_reg, ctrl_flow, ctrl_flow_reg, reg_write, reg_write_reg : STD_LOGIC := '0';
	signal mem_read, mem_read_reg, mem_write, mem_write_reg : STD_LOGIC_VECTOR (1 downto 0) := "00";
		
	signal reg_write_fp : STD_LOGIC := '0';
	signal imm_b, imm_s : STD_LOGIC_VECTOR(11 downto 0);
	signal fp_regs_IDEX : FP_IDEX;
	signal alu_operator, alu_operator_reg : ALU_OP := ALU_NONE;
	signal fpu_operator : FPU_OP := FPU_NONE;
	signal mem_operator, mem_operator_reg : MEM_OP := LSU_NONE;
	signal branch_predict : BRANCH_PREDICTION;
    signal write_fflags : STD_LOGIC;
    signal enable_fpu_subunit : STD_LOGIC_VECTOR (4 downto 0);
    signal csr_write_addr, csr_write_addr_reg : STD_LOGIC_VECTOR (11 downto 0);
	signal load_hazard_int, load_hazard_fp, flush, invalid_instruction, csr_write, csr_write_reg : STD_LOGIC;

    signal csr_data : STD_LOGIC_VECTOR (63 downto 0);

    signal reg_cmp1_mem, reg_cmp1_wb : STD_LOGIC;
    signal reg_cmp2_mem, reg_cmp2_wb : STD_LOGIC;
    signal reg_cmp3_mem, reg_cmp3_wb : STD_LOGIC;
    signal csr_cmp_mem, csr_cmp_wb : STD_LOGIC;

    signal reg_cmp1_mem_reg, reg_cmp1_wb_reg : STD_LOGIC;
    signal reg_cmp2_mem_reg, reg_cmp2_wb_reg : STD_LOGIC;
    signal reg_cmp3_mem_reg, reg_cmp3_wb_reg : STD_LOGIC;
    signal csr_cmp_mem_reg, csr_cmp_wb_reg : STD_LOGIC;

    signal reg_dst_not_zero : STD_LOGIC;

	alias opcode : STD_LOGIC_VECTOR(6 downto 0) is IR_i(6 downto 0);

	alias reg_src1 : STD_LOGIC_VECTOR(4 downto 0) is IR_i(19 downto 15);
	alias reg_src2 : STD_LOGIC_VECTOR(4 downto 0) is IR_i(24 downto 20);
	alias reg_src3 : STD_LOGIC_VECTOR(4 downto 0) is IR_i(31 downto 27);

	alias funct3 : STD_LOGIC_VECTOR(2 downto 0) is IR_i(14 downto 12);
	alias funct5 : STD_LOGIC_VECTOR(4 downto 0) is IR_i(31 downto 27);
	alias funct7 : STD_LOGIC_VECTOR(6 downto 0) is IR_i(31 downto 25);

    signal registers, registers_fp : reg_t;
    signal csr_operator, csr_operator_reg : STD_LOGIC_VECTOR (1 downto 0) := "00";

    signal result_select, result_select_reg : STD_LOGIC_VECTOR(3 downto 0) := "0000";

    signal csr_exception_id, csr_exception_id_reg : STD_LOGIC_VECTOR (3 downto 0);

    signal fun3, funct3_reg : STD_LOGIC_VECTOR (2 downto 0);

    signal reg_src1_valid, reg_src2_valid, reg_fp_src1_valid, reg_fp_src2_valid, reg_fp_src3_valid : STD_LOGIC;

	component regfile is
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			cpu_enable_i : in STD_LOGIC;

			reg_dst_i : REG;

			registers_o : out reg_t);
	end component regfile;

begin

	REGS : regfile
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		cpu_enable_i => cpu_enable_i,
		reg_dst_i => reg_dst_i,
		registers_o => registers
	);

	FP_REGS : regfile
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		cpu_enable_i => cpu_enable_i,
		reg_dst_i => reg_dst_fp_i,
		registers_o => registers_fp
	);

	OP_DECODE : process (IR_i, frm_i)
		variable funct : STD_LOGIC_VECTOR (9 downto 0);
	begin
		alu_operator <= ALU_NONE;
		fpu_operator <= FPU_NONE;
		mem_operator <= LSU_NONE;
		csr_operator <= CSR_NONE;
		invalid_instruction <= '0';
		funct := funct7 & funct3;
		case opcode is
		    when LUI | AUIPC | JAL | JALR => alu_operator <= ALU_ADD;
			when BRANCH =>
				case funct3 is
					when "000" | "001" | "100" | "101" | "110" | "111" => invalid_instruction <= '0';
					when others => invalid_instruction <= '1';
				end case;
	        -- LOAD / STORE
			when LOAD =>
				case funct3 is
					when "000" => mem_operator <= LSU_LB;
					when "001" => mem_operator <= LSU_LH;
					when "010" => mem_operator <= LSU_LW;
					when "011" => mem_operator <= LSU_LD;
					when "100" => mem_operator <= LSU_LBU;
					when "101" => mem_operator <= LSU_LHU;
					when "110" => mem_operator <= LSU_LWU;
					when others => invalid_instruction <= '1';
				end case;
			when STORE =>
				case funct3 is
					when "000" => mem_operator <= LSU_SB;
					when "001" => mem_operator <= LSU_SH;
					when "010" => mem_operator <= LSU_SW;
					when "011" => mem_operator <= LSU_SD;
					when others => invalid_instruction <= '1';
				end case;
			when LOAD_FP =>
				case funct3 is
					when "010" => mem_operator <= LSU_FLW;
					when "011" => mem_operator <= LSU_FLD;
					when others => invalid_instruction <= '1';
				end case;
			when STORE_FP =>
				case funct3 is
					when "010" => mem_operator <= LSU_FSW;
					when "011" => mem_operator <= LSU_FSD;
					when others => invalid_instruction <= '1';
				end case;
	        -- Integer Arithmetic
			when RI =>
				case funct3 is
					when "000" => alu_operator <= ALU_ADD;
					when "010" => alu_operator <= ALU_SLT;
					when "011" => alu_operator <= ALU_SLTU;
					when "100" => alu_operator <= ALU_XOR;
					when "110" => alu_operator <= ALU_OR;
					when "111" => alu_operator <= ALU_AND;
					when "001" => alu_operator <= ALU_SLL;
						invalid_instruction <= or IR_i(31 downto 26);
					when "101" =>
						case IR_i(31 downto 26) is
							when "000000" => alu_operator <= ALU_SRL;
							when "010000" => alu_operator <= ALU_SRA;
							when others => invalid_instruction <= '1';
						end case;
					when others => invalid_instruction <= '1';
				end case;
			when RI32 =>
				case funct3 is
					when "000" => alu_operator <= ALU_ADDW;
					when "001" => alu_operator <= ALU_SLLW;
						invalid_instruction <= or funct7;
					when "101" =>
						case funct7 is
							when "0000000" => alu_operator <= ALU_SRLW;
							when "0100000" => alu_operator <= ALU_SRAW;
							when others => invalid_instruction <= '1';
						end case;
					when others =>
						invalid_instruction <= '1';
				end case;
			when RR =>
				case funct is
					when "0000000000" => alu_operator <= ALU_ADD;
					when "0100000000" => alu_operator <= ALU_SUB;
					when "0000000010" => alu_operator <= ALU_SLT;
					when "0000000011" => alu_operator <= ALU_SLTU;
					when "0000000100" => alu_operator <= ALU_XOR;
					when "0000000110" => alu_operator <= ALU_OR;
					when "0000000111" => alu_operator <= ALU_AND;
					when "0000000001" => alu_operator <= ALU_SLL;
					when "0000000101" => alu_operator <= ALU_SRL;
					when "0100000101" => alu_operator <= ALU_SRA;
						-- Multiplication
					when "0000001000" => alu_operator <= ALU_MUL;
					when "0000001001" => alu_operator <= ALU_MULH;
					when "0000001010" => alu_operator <= ALU_MULHSU;
					when "0000001011" => alu_operator <= ALU_MULHU;
						-- Division
					when "0000001100" => alu_operator <= ALU_DIV;
					when "0000001101" => alu_operator <= ALU_DIVU;
					when "0000001110" => alu_operator <= ALU_REM;
					when "0000001111" => alu_operator <= ALU_REMU;

					when others => invalid_instruction <= '1';
				end case;

			when RR32 =>
				case funct is
					when "0000000000" => alu_operator <= ALU_ADDW;
					when "0100000000" => alu_operator <= ALU_SUBW;
					when "0000000001" => alu_operator <= ALU_SLLW;
					when "0000000101" => alu_operator <= ALU_SRLW;
					when "0100000101" => alu_operator <= ALU_SRAW;
					when "0000001000" => alu_operator <= ALU_MULW;
						-- Division
					when "0000001100" => alu_operator <= ALU_DIVW;
					when "0000001101" => alu_operator <= ALU_DIVUW;
					when "0000001110" => alu_operator <= ALU_REMW;
					when "0000001111" => alu_operator <= ALU_REMUW;
					when others => invalid_instruction <= '1';
				end case;
			-- FPU 
			when FMADD => fpu_operator <= FPU_FMADD;
				invalid_instruction <= IR_i(26) or check_rm(funct3, frm_i);
			when FMSUB => fpu_operator <= FPU_FMSUB;
				invalid_instruction <= IR_i(26) or check_rm(funct3, frm_i);
			when FNMADD => fpu_operator <= FPU_FNMADD;
				invalid_instruction <= IR_i(26) or check_rm(funct3, frm_i);
			when FNMSUB => fpu_operator <= FPU_FNMSUB;
				invalid_instruction <= IR_i(26) or check_rm(funct3, frm_i);
			when FP =>
				if IR_i(26) = '0' then
					case funct5 is
						when "00000" => fpu_operator <= FPU_ADD;
							invalid_instruction <= check_rm(funct3, frm_i);
						when "00001" => fpu_operator <= FPU_SUB;
							invalid_instruction <= check_rm(funct3, frm_i);
						when "00010" => fpu_operator <= FPU_MUL;
							invalid_instruction <= check_rm(funct3, frm_i);
						when "00011" => fpu_operator <= FPU_DIV;
							invalid_instruction <= check_rm(funct3, frm_i);
						when "00100" => fpu_operator <= FPU_SGNJ;
							invalid_instruction <= funct3(2) or (and funct3(1 downto 0));
						when "00101" => fpu_operator <= FPU_MINMAX;
							invalid_instruction <= or funct3(2 downto 1);
						when "01000" => fpu_operator <= FPU_CVT_FF;
							invalid_instruction <= or IR_i(24 downto 21);
						when "01011" => fpu_operator <= FPU_SQRT;
							invalid_instruction <= (or reg_src2) or check_rm(funct3, frm_i);
						when "10100" => fpu_operator <= FPU_CMP;
						when "11000" => fpu_operator <= FPU_CVT_FI;
							invalid_instruction <= or IR_i(24 downto 22);
						when "11010" => fpu_operator <= FPU_CVT_IF;
							invalid_instruction <= or IR_i(24 downto 22);
						when "11100" =>
							case funct3 is
								when "000" => fpu_operator <= FPU_MV_FX;
									invalid_instruction <= or reg_src2;
								when "001" => fpu_operator <= FPU_CLASS;
									invalid_instruction <= or reg_src2;
								when others => invalid_instruction <= '1';
							end case;
						when "11110" => fpu_operator <= FPU_MV_XF;
							invalid_instruction <= or reg_src2;
						when others => invalid_instruction <= '1';
					end case;
				else
					invalid_instruction <= '1';
				end if;
			when SYSTEM =>
				case funct3 is
					when "000" =>
					when "001" | "010" | "011" | "101" | "110" | "111" =>
						csr_operator <= IR_i(13 downto 12);
					when others =>
						invalid_instruction <= '1';
				end case;
			when others => 
		       alu_operator <= ALU_NONE;
		end case;
	end process;

    process (opcode, funct5)
    begin
        case opcode is 
            when FMADD | FMSUB | FNMADD | FNMSUB => write_fflags <= '1';
            when FP => 
                case funct5 is 
                    when "00000" | "00001" | "00010" | "00011" | "00100" | "00101" | "01000" | "01011" | "10100" | "11000" | "11010" => write_fflags <= '1'; 
                    when others => write_fflags <= '0';
                end case;
            when others => write_fflags <= '0';
        end case;
    end process;

	REGISTER_WRITE : process (all)
	begin
		case opcode is
			when LUI | AUIPC | LOAD | JAL | JALR | RI | RI32 | RR | RR32 | SYSTEM =>
				reg_write <= or IR_i(11 downto 7);
				reg_write_fp <= '0';
			when LOAD_FP | FMADD | FMSUB | FNMADD | FNMSUB =>
				reg_write <= '0';
				reg_write_fp <= '1';
			when FP =>
				case funct5 is
					when "10100" | "11000" | "11100" =>
						reg_write <= or IR_i(11 downto 7);
						reg_write_fp <= '0';
					when others =>
						reg_write <= '0';
						reg_write_fp <= '1';
				end case;
			when others =>
				reg_write <= '0'; 
				reg_write_fp <= '0';
		end case;
	end process;

	imm_b <= IR_i(31) & IR_i(7) & IR_i(30 downto 25) & IR_i(11 downto 8);
	imm_s <= IR_i(31 downto 25) & IR_i(11 downto 7);
	
    branch_target_address <= STD_LOGIC_VECTOR(unsigned(pc_i) + unsigned(resize(signed(imm_b) & "0", 64)));
    branch_next_pc <= STD_LOGIC_VECTOR(unsigned(pc_i) + FOUR);

    load_hazard_int <= mem_read_reg(0) and ( ( reg_cmp1_mem and reg_src1_valid ) or 
                                          ( reg_cmp2_mem and reg_src2_valid ) );

    load_hazard_fp <= mem_read_reg(1) and ( ( reg_cmp1_mem and reg_fp_src1_valid ) or 
                                         ( reg_cmp2_mem and reg_fp_src2_valid ) or
                                         ( reg_cmp3_mem and reg_fp_src3_valid ) );
    
    flush <= ( load_hazard_int or load_hazard_fp or flush_i ) and not pipeline_stall_i;

	OFFSET_SELECT : process (IR_i, imm_s, branch_target_address)
	begin
		case opcode is
			when LUI | AUIPC => imm <= STD_LOGIC_VECTOR(resize(signed(IR_i(31 downto 12)) & X"000", 64));
			when BRANCH => imm <= branch_target_address;
			when STORE | STORE_FP => imm <= STD_LOGIC_VECTOR(resize(signed(imm_s), 64));
			when JALR | LOAD | LOAD_FP | RI | RI32 | FP => imm <= STD_LOGIC_VECTOR(resize(signed(IR_i(31 downto 20)), 64));
			when SYSTEM => imm <= (63 downto 5 => '0') & IR_i(19 downto 15);
			when others => imm <= (others => '0');
		end case;
    end process;
    
    reg_cmp1_mem <= '1' when reg_src1 = reg_dst and (reg_src1_valid or reg_fp_src1_valid ) = '1' else '0';
    reg_cmp1_wb <= '1' when reg_src1 = reg_mem_i and (reg_src1_valid or reg_fp_src1_valid ) = '1' else '0';
    
    reg_cmp2_mem <= '1' when reg_src2 = reg_dst else '0';
    reg_cmp2_wb <= '1' when reg_src2 = reg_mem_i else '0';

    reg_cmp3_mem <= '1' when reg_src3 = reg_dst else '0'; 
    reg_cmp3_wb <= '1' when reg_src3 = reg_mem_i else '0';

    csr_cmp_mem <= '1' when IR_i(31 downto 20) = csr_write_addr else '0';
    csr_cmp_wb <= '1' when IR_i(31 downto 20) = csr_mem_addr_i else '0';
    
	with opcode select 
	    pc_src <= '1' when AUIPC | JAL | JALR, 
	              '0' when others;
	
	with opcode select 
	    imm_src <= '1' when LUI | AUIPC | LOAD | LOAD_FP | STORE | STORE_FP | RI | RI32, 
	               IR_i(14) when SYSTEM,
	               '0' when others;

	with opcode select 
	    ctrl_flow <= '1' when JALR | BRANCH, 
	                 '0' when others;
	
	with opcode select 
	    result_select(2) <= '1' when FP | FMADD | FMSUB | FNMADD | FNMSUB, 
	                        '0' when others;

	with opcode select 
	    mem_write <= "01" when STORE, 
	                 "10" when STORE_FP, 
	                 "00" when others;

	with opcode select 
	    mem_read <= "0" & or IR_i(11 downto 7) when LOAD, 
	                "10" when LOAD_FP, 
	                "00" when others;
        
    fun3 <= frm_i when funct3 = "111" and result_select(2) = '1' else funct3;
    result_select(3) <= or csr_operator; 
    
    with opcode select 
        x_data <= x_data_reg when JALR | BRANCH | LOAD | LOAD_FP | STORE | STORE_FP | RI | RI32 | RR | RR32 | FP | SYSTEM,
                  (others => '0') when others;
 
	with opcode select
		y_data <= (2 => '1', others => '0') when JAL | JALR,
		          y_data_reg when BRANCH | STORE | RR | RR32,
		          (others => '0') when others;
		          

	csr_exception_id <= ILLEGAL_INSTRUCTION when invalid_instruction = '1' else
		               BREAKPOINT when IR_i = X"00100073" else
		               ENVIROMENT_CALL_USER_MODE when IR_i = X"00000073" else
		               NO_EXCEPTION;

    process (clk_i)
 	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or flush = '1' then
				ctrl_flow_reg <= '0';
				reg_write_reg <= '0';
				mem_read_reg <= "00";
				mem_write_reg <= "00";
				alu_operator_reg <= ALU_NONE;
				mem_operator_reg <= LSU_NONE;
		        branch_predict.cf_type <= "00";
		        reg_dst <= (others => '0');
		        result_select_reg <= "0000";
	            reg_cmp1_mem_reg <= '0';
			    reg_cmp1_wb_reg <= '0';
			    reg_cmp2_mem_reg <= '0';
			    reg_cmp2_wb_reg <= '0';
			    reg_cmp3_mem_reg <= '0';
			    reg_cmp3_wb_reg <= '0';
			else
				if pipeline_stall_i = '0' then
				    x <= x_data;
				    y <= y_data;
				    funct3_reg <= fun3;
			        reg_cmp1_mem_reg <= reg_cmp1_mem;
			        reg_cmp1_wb_reg <= reg_cmp1_wb;
			        reg_cmp2_mem_reg <= reg_cmp2_mem;
			        reg_cmp2_wb_reg <= reg_cmp2_wb;
			        reg_cmp3_mem_reg <= reg_cmp3_mem;
			        reg_cmp3_wb_reg <= reg_cmp3_wb;
					pc_src_reg <= pc_src;
					imm_src_reg <= imm_src;
					reg_dst <= IR_i(11 downto 7);
					result_select_reg <= result_select;
					alu_operator_reg <= alu_operator;
					imm_reg <= imm;
					branch_next_pc_reg <= branch_next_pc;
					reg_write_reg <= reg_write;
 					pc <= pc_i;
					branch_predict <= branch_predict_i;
					ctrl_flow_reg <= ctrl_flow;
					mem_operator_reg <= mem_operator;
				    mem_read_reg <= mem_read;
					mem_write_reg <= mem_write;
				end if;
			end if;
		end if;
	end process;

    x_data_reg <= reg_dst_i.data when reg_src1 = reg_dst_i.dest and reg_dst_i.write = '1' else registers(to_integer(unsigned(reg_src1)));
    y_data_reg <= reg_dst_i.data when reg_src2 = reg_dst_i.dest and reg_dst_i.write = '1' else registers(to_integer(unsigned(reg_src2)));

    x_fp <= reg_dst_fp_i.data when reg_src1 = reg_dst_fp_i.dest and reg_dst_fp_i.write = '1' else registers_fp(to_integer(unsigned(reg_src1)));
    y_fp <= reg_dst_fp_i.data when reg_src2 = reg_dst_fp_i.dest and reg_dst_fp_i.write = '1' else registers_fp(to_integer(unsigned(reg_src2)));
    z_fp <= reg_dst_fp_i.data when reg_src3 = reg_dst_fp_i.dest and reg_dst_fp_i.write = '1' else registers_fp(to_integer(unsigned(reg_src3))); 
    
	process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or flush = '1' then
				fp_regs_IDEX.write <= '0';
				fp_regs_IDEX.fp_op <= FPU_NONE;
				fp_regs_IDEX.enable_fpu_subunit <= (others => '0');
			else
				if pipeline_stall_i = '0' then
					fp_regs_IDEX.fp_op <= fpu_operator;
					fp_regs_IDEX.write <= reg_write_fp;
					fp_regs_IDEX.x <= x_fp;
					fp_regs_IDEX.y <= y_fp;
					fp_regs_IDEX.z <= z_fp;
					fp_regs_idex.precision <= IR_i(25) & ( not IR_i(25) );
					fp_regs_idex.enable_fpu_subunit <= enable_fpu_subunit;
				end if;
			end if;
		end if;
	end process;
	
	CS_REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or flush = '1' then
				csr_write_reg <= '0';
				csr_exception_id_reg <= NO_EXCEPTION;
			else
				if pipeline_stall_i = '0' then
   
                	csr_write_reg <= csr_write;
				    csr_operator_reg <= csr_operator;
				    csr_exception_id_reg <= csr_exception_id;
				    csr_cmp_mem_reg <= csr_cmp_mem;
				    csr_cmp_wb_reg <= csr_cmp_wb;
				    csr_data <= csr_data_i;
				end if;
			end if;
		end if;
	end process;
	
	csr_data_o <= csr_data;
    csr_write_addr <= IR_i(31 downto 20) when write_fflags = '0' else FFLAGS; 
	csr_write <= (csr_operator(1) and (or IR_i(19 downto 15))) or csr_operator(0) or write_fflags;			  
	
	with alu_operator select
		result_select(1 downto 0) <= "01" when ALU_MUL | ALU_MULH | ALU_MULHSU | ALU_MULHU | ALU_MULW,
	               "10" when ALU_DIV | ALU_DIVU | ALU_DIVW | ALU_DIVUW | ALU_REM | ALU_REMU | ALU_REMW | ALU_REMUW,
                   "00" when others;


    process (IR_i)
    begin
        case opcode is
            when JALR | BRANCH | LOAD | LOAD_FP | STORE | STORE_FP | RI | RI32 | RR | RR32 => reg_src1_valid <= '1';
            when FP =>
                case funct5 is 
                    when "11010" | "11110" => reg_src1_valid <= '1';
                    when others => reg_src1_valid <= '0';
                end case;
            when SYSTEM =>
                case funct3 is
                    when "001" | "010" | "011" => reg_src1_valid <= '1';
                    when others => reg_src1_valid <= '0';
                end case;
            when others => reg_src1_valid <= '0';
        end case;
    end process;             
	
    with opcode select
		reg_src2_valid <= '1' when BRANCH | STORE | RR | RR32,
		                  '0' when others;
		                  
		                  
    with fpu_operator select 
		enable_fpu_subunit <= "00001" when FPU_ADD | FPU_SUB | FPU_MUL | FPU_FMADD | FPU_FMSUB | FPU_FNMADD | FPU_FNMSUB, 
		                     "00010" when FPU_DIV | FPU_SQRT,
		                     "00100" when FPU_CVT_FI,
		                     "01000" when FPU_CVT_IF,
		                     "10000" when FPU_CVT_FF,
		                     "00000" when others;	                  

    process (IR_i)
    begin
        case opcode is
            when STORE_FP =>
                reg_fp_src1_valid <= '0';
                reg_fp_src2_valid <= '1';
                reg_fp_src3_valid <= '0';
            when FMADD | FMSUB | FNMADD | FNMSUB => 
                reg_fp_src1_valid <= '1';
                reg_fp_src2_valid <= '1';
                reg_fp_src3_valid <= '1';
            when FP =>
                reg_fp_src1_valid <= '1';
                reg_fp_src2_valid <= '1';
                reg_fp_src3_valid <= '0';
            when others => 
                reg_fp_src1_valid <= '0';
                reg_fp_src2_valid <= '0';
                reg_fp_src3_valid <= '0';
        end case;
    end process;    

    load_hazard_o <= load_hazard_int or load_hazard_fp;
	pc_src_o <= pc_src_reg;
	imm_src_o <= imm_src_reg;
	branch_next_pc_o <= branch_next_pc_reg;

	mem_read_o <= mem_read_reg;
	mem_write_o <= mem_write_reg;

	alu_operator_o <= alu_operator_reg;
	mem_operator_o <= mem_operator_reg;

	imm_o <= imm_reg;

	reg_write_o <= reg_write_reg;

	reg_dst_o <= reg_dst;

	pc_o <= pc;
	fp_regs_IDEX_o <= fp_regs_IDEX;
	csr_read_addr_o <= IR_i(31 downto 20);

    x_o <= x;
	
    y_o <= y;
	branch_predict_o <= branch_predict;
	ctrl_flow_o <= ctrl_flow_reg;
	
	funct3_o <= funct3_reg;
	
	reg_cmp1_mem_o <= reg_cmp1_mem_reg;
    reg_cmp2_mem_o <= reg_cmp2_mem_reg;
	reg_cmp3_mem_o <= reg_cmp3_mem_reg;
	
	reg_cmp1_wb_o <= reg_cmp1_wb_reg;
	reg_cmp2_wb_o <= reg_cmp2_wb_reg;
	reg_cmp3_wb_o <= reg_cmp3_wb_reg;

	registers_o <= registers;
    registers_fp_o <= registers_fp;
    
    csr_operator_o <= csr_operator_reg;
    csr_cmp_mem_o <= csr_cmp_mem_reg;
    csr_cmp_wb_o <= csr_cmp_wb_reg;
    csr_write_addr_o <= csr_write_addr;

    csr_write_o <= csr_write_reg;
    csr_exception_id_o <= csr_exception_id_reg;

    result_select_o <= result_select_reg;

end behavioral;