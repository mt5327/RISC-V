library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity fetch is
	generic (
		ADDRESS_WIDTH : NATURAL := 18;
		BHT_INDEX_WIDTH : NATURAL := ADDRESS_WIDTH);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		cpu_enable_i : in STD_LOGIC;
		pipeline_stall_i : in STD_LOGIC;
		branch_info_i : in BRANCH_INFO;
		IR_i : in STD_LOGIC_VECTOR(31 downto 0);
		instr_address_o : out STD_LOGIC_VECTOR(ADDRESS_WIDTH - 3 downto 0);
		pc_o : out STD_LOGIC_VECTOR(63 downto 0);
		IR_o : out STD_LOGIC_VECTOR(31 downto 0);
		branch_predict_o : out BRANCH_PREDICTION);
end fetch;

architecture behavioral of fetch is

	component branch_prediction_unit is
		generic (BHT_INDEX_WIDTH : NATURAL := 2);
		port (
			clk_i : in STD_LOGIC;
			rst_i : in STD_LOGIC;
			pc_i : in unsigned (63 downto 0);
			branch_info_i : in BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
			current_instruction_i : in STD_LOGIC_VECTOR (31 downto 0);
			branch_predict_o : out BRANCH_PREDICTION);
	end component branch_prediction_unit;

	signal pc : unsigned(63 downto 0) := (others => '0');
	signal pc_reg : unsigned(63 downto 0) := (others => '0');

	signal IR : STD_LOGIC_VECTOR(31 downto 0) := NOP;

	signal branch_predict, branch_predict_reg : BRANCH_PREDICTION;
	signal predict_taken : STD_LOGIC;

begin

	BPU : branch_prediction_unit
	generic map(BHT_INDEX_WIDTH => BHT_INDEX_WIDTH)
	port map(
		clk_i => clk_i,
		rst_i => rst_i,
		pc_i => pc,
		branch_info_i => branch_info_i,
		current_instruction_i => IR_i,
		branch_predict_o => branch_predict
	);

	NEXT_PC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or cpu_enable_i = '0' then
				pc <= (others => '0');
			else
				if branch_info_i.mispredict = '1' then
					pc <= branch_info_i.target_address;
				elsif pipeline_stall_i = '1' then
					pc <= pc;
				elsif predict_taken = '1' then
					pc <= branch_predict.predicted_address;
				else
					pc <= pc + FOUR;
				end if;
			end if;
		end if;
	end process;

	REGS : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' or branch_info_i.mispredict = '1' or cpu_enable_i = '0' then
				pc_reg <= (others => '0');
				IR <= NOP;
				branch_predict_reg.cf_type <= "00";
			else
				if pipeline_stall_i = '0' then
					pc_reg <= pc;
					IR <= IR_i;
					branch_predict_reg <= branch_predict;
				end if;
			end if;
		end if;
	end process;

	with IR_i(6 downto 0) select
	   predict_taken <= '1' when JAL,
		                and branch_predict.cf_type when BRANCH,
		                '0' when others; 


	instr_address_o <= STD_LOGIC_VECTOR(pc(ADDRESS_WIDTH-1 downto 2));
	pc_o <= STD_LOGIC_VECTOR(pc_reg);
	IR_o <= IR;
	branch_predict_o <= branch_predict_reg;

end behavioral;