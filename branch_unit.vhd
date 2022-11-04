library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity branch_unit is
	generic (BHT_INDEX_WIDTH : NATURAL := 2);
	port (
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		pc_i : in STD_LOGIC_VECTOR (BHT_INDEX_WIDTH-1 downto 0);
		offset_i : in STD_LOGIC_VECTOR (63 downto 0);
		branch_next_pc_i : in STD_LOGIC_VECTOR (63 downto 0);
		ctrl_flow_i : in STD_LOGIC;
		alu_cmp_i : in STD_LOGIC;
		branch_predict_i : in BRANCH_PREDICTION;
		branch_info_o : out BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0)));
end branch_unit;

architecture behavioral of branch_unit is

	signal wrong_target, mispredict, jalr : STD_LOGIC;
	signal jalr_address, target_address : unsigned (63 downto 0);
   
begin

    jalr <= '1' when branch_predict_i.cf_type = "01" else '0';
	mispredict <= ( alu_cmp_i xor branch_predict_i.cf_type(0) ) or ( wrong_target and branch_predict_i.cf_type(0) );
	wrong_target <= '1' when unsigned(offset_i) /= branch_predict_i.predicted_address else '0';

    jalr_address <= unsigned(x_i) + unsigned(offset_i);

	target_address <= unsigned(offset_i) when alu_cmp_i = '1' else unsigned(branch_next_pc_i);
	
 	
	BRANCH_INFO : process (ctrl_flow_i, alu_cmp_i, pc_i, target_address, mispredict, jalr)
	begin
		branch_info_o.pc <= (others => '0');
		branch_info_o.taken <= '0';
		branch_info_o.mispredict <= '0';
		branch_info_o.target_address <= (others => '0');
		if ctrl_flow_i = '1' then
			branch_info_o.pc <= pc_i;
			branch_info_o.taken <= alu_cmp_i or jalr;
		    branch_info_o.target_address <= target_address;
		    branch_info_o.mispredict <= mispredict;
		end if;
	end process;
	
	branch_info_o.valid <= ctrl_flow_i;

end behavioral;