library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FP_Converter_float_to_int is
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
end FP_Converter_float_to_int;

architecture behavioral of FP_Converter_float_to_int is

	constant BIAS : signed(E - 1 downto 0) := to_signed(2 ** (E - 1) - 1, E);
    
	alias exp : STD_LOGIC_VECTOR(E - 1 downto 0) is x_i(P - 2 downto P - E - 1);

	type int_width is array(0 to 3) of signed(E - 1 downto 0);
	constant INT_WIDTHS : int_width := (to_signed(31, E), to_signed(32, E), to_signed(63, E), to_signed(64, E));

	alias sign : STD_LOGIC is x_i(P - 1);

	signal int, int_fin, signed_int, int_neg : STD_LOGIC_VECTOR (63 downto 0);

	signal exponent : signed (E - 1 downto 0);
	signal inexact, valid : STD_LOGIC := '0';

	component FP_Classifier is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
			fp_class_o : out FP_INFO);
	end component FP_Classifier;

	component rounder is
		generic (SIZE : NATURAL);
		port (
			x_i : in unsigned (SIZE - 1 downto 0);
			sign_i : in STD_LOGIC;
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
			z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
	end component rounder;

	signal less_than_one, overflow, overflow_reg, special_case, not_zero, sign_reg : STD_LOGIC;
	signal overflow_value, overflow_value_final, overflow_value_reg : STD_LOGIC_VECTOR(63 downto 0);
	signal fp_class : FP_INFO;
	
	signal mantissa_shifted_reg : unsigned(63 downto 0);
	signal overflows : STD_LOGIC_VECTOR (3 downto 0);
	signal shamt : unsigned(6 downto 0);
	signal mantissa, mantissa_shifted : unsigned(64 + M downto 0);
	signal round_sticky, round_sticky_reg, mode: STD_LOGIC_VECTOR (1 downto 0);

    signal rm : STD_LOGIC_VECTOR (2 downto 0);

begin 

	FP_CLASSIFY : FP_Classifier generic map(P, E, M) port map(x_i(P - 2 downto 0), fp_class);

	exponent <= signed(exp) - BIAS;
	shamt <= unsigned(63 - resize(exponent, 7));

	less_than_one <= '1' when exponent < -1 else '0';

    OVERFLOW_CHECK: for i in 0 to 3 generate 
        overflows(i) <= '1' when exponent >= INT_WIDTHS(i) else '0';
    end generate; 
      
    overflow <= overflows(to_integer(unsigned(mode_i))) or (and exp);
    overflow_value <= (30 downto 0 => '1', others => '0') when mode_i = "00" else
    	              (63 => mode_i(0), others => '1');
 
 	overflow_value_final <= not overflow_value when sign = '1' and fp_class.nan = '0' else 
		                    overflow_value;
 
	mantissa <= fp_class.normal & unsigned(x_i(M - 2 downto 0)) & (64 downto 0 => '0');
    mantissa_shifted <= shift_right(mantissa, to_integer(shamt)) when less_than_one = '0' else 
                        (64 downto 0 => '0') & fp_class.normal & unsigned(x_i(M - 2 downto 0));

	round_sticky <= mantissa_shifted(M) & (or mantissa_shifted(M - 1 downto 0));

	process(clk_i) begin 
	    if rising_edge(clk_i) then
            valid <= enable_i;
            mantissa_shifted_reg <= mantissa_shifted(mantissa_shifted'left downto M + 1);
            round_sticky_reg <= round_sticky;
            overflow_value_reg <= overflow_value_final;
            overflow_reg <= overflow;
            mode <= mode_i; 
            sign_reg <= sign;
        end if; 
    end process;
    
    inexact <= or round_sticky_reg;
 
	ROUNDING : rounder generic map(64) port map(mantissa_shifted_reg, sign_reg, rm_i, round_sticky_reg, int);	

    special_case <= overflow_reg or ( sign_reg and mode(0) and not_zero );

	int_neg <= STD_LOGIC_VECTOR(-signed(int));
	
    signed_int <= int_neg when sign_reg = '1' else int;
	not_zero <= or int;
	int_fin <= (63 downto 32 => signed_int(31)) & signed_int(31 downto 0) when mode(1) = '0' else
	           signed_int;
    
	result_o <= ( overflow_value_reg, "10000", valid) when special_case = '1' else (int_fin, "0000" & inexact, valid); 
        
end behavioral; 