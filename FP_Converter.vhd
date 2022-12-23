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
        enable_i : in STD_LOGIC;
        mode_i : in STD_LOGIC_VECTOR (1 downto 0);
        rm_i : in STD_LOGIC_VECTOR (2 downto 0);
        x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
        x_int_i : in STD_LOGIC_VECTOR (63 downto 0);
        result_if_o : out FP_RESULT;
        result_fi_o : out FP_RESULT;
        result_ff_o : out FP_RESULT);
end FP_Converter;

architecture behavioral of FP_Converter is

    signal result_fi, result_if : FP_RESULT;

    component FP_Converter_float_to_int is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            clk_i : in STD_LOGIC;
            enable_i : in STD_LOGIC;
            x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
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
            enable_i : in STD_LOGIC;
            x_i : in STD_LOGIC_VECTOR (63 downto 0);
            mode_i : in STD_LOGIC_VECTOR (1 downto 0);
            rm_i : in STD_LOGIC_VECTOR (2 downto 0);
            result_o : out FP_RESULT);
    end component FP_Converter_int_to_float;

    component FP_Classifier is
        generic (
            P : NATURAL;
            E : NATURAL;
            M : NATURAL);
        port (
            x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
            fp_class_o : out FP_INFO);
    end component FP_Classifier;

    signal fp_class : FP_INFO;

begin

    CLASSIFY : FP_Classifier generic map(P, E, M) port map(x_i(P-2 downto 0), fp_class);
    
    FP_CVT_FI : FP_Converter_float_to_int generic map(P, E, M) port map(clk_i, enable_i, x_i, mode_i, rm_i, result_fi_o);
    FP_CVT_IF : FP_Converter_int_to_float generic map(P, E, M) port map(clk_i, enable_i, x_int_i, mode_i, rm_i, result_if_o);
    
    FP_CVT_FF : case M generate 
        when 24 =>
            signal exp_dp : STD_LOGIC_VECTOR(10 downto 0);
        begin
            exp_dp <= STD_LOGIC_VECTOR(("000" & unsigned(x_i(30 downto 23))) + BIAS_DIFF);
            result_ff_o.value <= x_i(31) & exp_dp & x_i(22 downto 0) & (28 downto 0 => '0');
        	result_ff_o.fflags <= fp_class.signaling_nan & "0000";
            result_ff_o.valid <= '1';
        
        when 53 =>

            signal exp_sp : unsigned(7 downto 0);
            signal mantissa_sp : unsigned(22 downto 0);

            signal sticky_bit, overflow, underflow, inexact : STD_LOGIC;

            signal num : unsigned(30 downto 0);
            signal rounded_num : STD_LOGIC_VECTOR(30 downto 0);

            component rounder is
                generic (SIZE : NATURAL);
                port (
                    x_i : in unsigned (SIZE - 1 downto 0);
                    sign_i : in STD_LOGIC;
                    rm_i : in STD_LOGIC_VECTOR (2 downto 0);
                    round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
                    z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
            end component rounder;
        
        begin
            exp_sp <= resize(unsigned(x_i(62 downto 52)) - BIAS_DIFF, 8);
            mantissa_sp <= unsigned(x_i(51 downto 29));
            sticky_bit <= or x_i(27 downto 0);
            num <= exp_sp & mantissa_sp;

            ROUND : rounder generic map(num'length) port map(num, x_i(63), rm_i, x_i(28) & sticky_bit, rounded_num);

            result_ff_o.value <= (63 downto 31 => x_i(63)) & rounded_num when fp_class.nan = '0' else
                                 (30 downto 22 => '1', others => '0');

            overflow <= (not fp_class.inf) and (and rounded_num(30 downto 23));
            underflow <= or rounded_num(30 downto 23);
            inexact <= x_i(28) or sticky_bit or overflow;

            result_ff_o.fflags <= "00" & overflow & underflow & inexact when fp_class.nan = '0' else
                           fp_class.signaling_nan & "0000";
            result_ff_o.valid <= '1';
    end generate;

end behavioral;