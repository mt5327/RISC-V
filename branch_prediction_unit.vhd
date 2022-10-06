library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity branch_prediction_unit is
    generic ( BHT_INDEX_WIDTH : NATURAL := 2 );
    port (
        clk_i : in STD_LOGIC;
        rst_i : in STD_LOGIC;
        pc_i : in unsigned (63 downto 0);
        branch_info_i : in BRANCH_INFO (pc(BHT_INDEX_WIDTH - 1 downto 0));
        current_instruction_i : in STD_LOGIC_VECTOR (31 downto 0);
        branch_predict_o : out BRANCH_PREDICTION);
end branch_prediction_unit;

architecture behavioral of branch_prediction_unit is

    signal offset : STD_LOGIC_VECTOR(19 downto 0);

    alias opcode : STD_LOGIC_VECTOR(6 downto 0) is current_instruction_i(6 downto 0);
    alias if_ix : unsigned(BHT_INDEX_WIDTH - 1 downto 0) is pc_i(BHT_INDEX_WIDTH - 1 + 2 downto 2);

    type bht_t is array(0 to 2 ** BHT_INDEX_WIDTH - 1) of unsigned(1 downto 0);
    signal bht : bht_t := (others => (others => '0'));

    type btb_t is array(0 to 2 ** BHT_INDEX_WIDTH - 1) of unsigned(63 downto 0);
    signal btb : btb_t := (others => (others => '0'));

begin

    offset <= current_instruction_i(31) & current_instruction_i(19 downto 12) & current_instruction_i(20) & current_instruction_i(30 downto 21);

    with opcode select 
        branch_predict_o.predicted_address <= pc_i + unsigned(resize(signed(offset) & "0", 64)) when JAL,
        btb(to_integer(if_ix)) when BRANCH,
        (others => '0') when others;

    with opcode select 
        branch_predict_o.cf_type <= "01" when JALR,
                                    '1' & bht(to_integer(if_ix))(1) when BRANCH,
                                    "00" when others;

    BHT_UPDATE : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                bht <= (others => (others => '0'));
            else
                if branch_info_i.valid = '1' then
                    case bht(to_integer(unsigned(branch_info_i.pc))) is
                        when "11" => 
                            if branch_info_i.taken = '0' then
                                bht(to_integer(unsigned(branch_info_i.pc))) <= "10";
                            end if;
                        when "00" => 
                            if branch_info_i.taken = '1' then
                                bht(to_integer(unsigned(branch_info_i.pc))) <= "01";
                            end if;
                        when "01" | "10" =>
                            if branch_info_i.taken = '1' then
                                bht(to_integer(unsigned(branch_info_i.pc))) <= bht(to_integer(unsigned(branch_info_i.pc))) + 1;
                            else
                                bht(to_integer(unsigned(branch_info_i.pc))) <= bht(to_integer(unsigned(branch_info_i.pc))) - 1;
                            end if;
                        when others =>
                    end case;
                end if;
            end if;
        end if;
    end process;

    BTB_UPDATE : process (clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                btb <= (others => (others => '0'));
            else
                if branch_info_i.mispredict = '1' then
                    btb(to_integer(unsigned(branch_info_i.pc))) <= branch_info_i.target_address;
                end if;
            end if;
        end if;
    end process;

end behavioral;