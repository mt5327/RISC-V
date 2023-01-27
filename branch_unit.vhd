library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity branch_unit is
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
		branch_info_o : out BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0)));
end branch_unit;

architecture behavioral of branch_unit is

	signal eq_r, lt_r, cmp : STD_LOGIC;
	signal wrong_target, mispredict, jalr : STD_LOGIC;
	signal jalr_address, branch_target_address, target_address : unsigned (63 downto 0);
   
begin

	eq_r <= '1' when x_i = y_i else '0';

	lt_r <= '1' when signed((x_i(63) and (not op_i(1))) & x_i) < signed((y_i(63) and ( not op_i(1) )) & y_i) else '0';

	CMP_OUTPUT : process (op_i, eq_r, lt_r)
	begin
		case op_i is
			when EQ => cmp <= eq_r;
			when NE => cmp <= not eq_r;
			when LT | LTU => cmp <= lt_r;
			when GE | GEU => cmp <= not lt_r;
			when others => cmp <= '0';
		end case;
	end process;

    jalr <= not branch_predict_i.cf_type(1) and branch_predict_i.cf_type(0);
	mispredict <= ( cmp xor branch_predict_i.cf_type(0) ) or ( wrong_target and branch_predict_i.cf_type(0) );
	wrong_target <= '1' when unsigned(offset_i) /= branch_predict_i.predicted_address else '0';

    jalr_address <= unsigned(x_i) + unsigned(offset_i);

	branch_target_address <= unsigned(offset_i) when cmp = '1' else unsigned(branch_next_pc_i);
    target_address <= jalr_address when jalr = '1' else branch_target_address;
 	
	BRANCH_INFO : process (ctrl_flow_i, cmp, pc_i, target_address, mispredict, jalr)
	begin
	    branch_info_o.mispredict <= '0';
		branch_info_o.target_address <= (others => '0');
		branch_info_o.pc <= (others => '0');
		branch_info_o.taken <= '0';
		if ctrl_flow_i = '1' then
		    branch_info_o.target_address <= target_address;
		    branch_info_o.mispredict <= mispredict;
			branch_info_o.pc <= pc_i;
			branch_info_o.taken <= cmp or jalr;		
		end if;
	end process;
	
	branch_info_o.valid <= ctrl_flow_i;

 end behavioral;