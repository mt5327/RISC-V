library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter is
    generic (
        P : NATURAL;
        E : NATURAL;
        M : NATURAL);
    port (
        clk_i : in STD_LOGIC;
        rst_i : in STD_LOGIC;
        enable_i : in STD_LOGIC_VECTOR (2 downto 0);
        mode_i : in STD_LOGIC_VECTOR (1 downto 0);
        rm_i : in STD_LOGIC_VECTOR (2 downto 0);
        x_i : in STD_LOGIC_VECTOR (63 downto 0);
        x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
        result_fi_o : out FP_RESULT;
        result_if_o : out FP_RESULT;
        result_ff_o : out FP_RESULT);
end FP_Converter;

architecture behavioral of FP_Converter is

    component FP_Converter_float_to_int is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            clk_i : in STD_LOGIC;
            rst_i : in STD_LOGIC;
            enable_i : in STD_LOGIC;
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            result_o : out FP_RESULT);
    end component FP_Converter_float_to_int;

    component FP_Converter_int_to_float is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            clk_i : in STD_LOGIC;
            rst_i : in STD_LOGIC;
            enable_i : in STD_LOGIC;
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            result_o : out FP_RESULT);
    end component FP_Converter_int_to_float;

    component FP_Converter_float_to_float is
        generic (
            SRC_P : NATURAL;
            SRC_E : NATURAL;
            SRC_M : NATURAL);	
        port (
            clk_i : in STD_LOGIC;
            rst_i : in STD_LOGIC;
            enable_i : in STD_LOGIC;
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            result_o : out FP_RESULT);
    end component FP_Converter_float_to_float;
    
begin
    
    FP_CVT_FI : FP_Converter_float_to_int 
    generic map(P, E, M) 
    port map (
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_i(0), 
        x_i => x_i, 
        mode_i => mode_i, 
        rm_i => rm_i, 
        result_o => result_fi_o
    );
    
    FP_CVT_IF : FP_Converter_int_to_float 
    generic map(P, E, M) 
    port map (
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_i(1), 
        x_i => x_int_i, 
        mode_i => mode_i, 
        rm_i => rm_i, 
        result_o => result_if_o
    );
    
    FP_CVT_FF : FP_Converter_float_to_float 
    generic map(P, E, M) 
    port map (
        clk_i => clk_i,
        rst_i => rst_i,
        enable_i => enable_i(2), 
        x_i => x_i, 
        rm_i => rm_i, 
        result_o => result_ff_o
    );

end behavioral;  