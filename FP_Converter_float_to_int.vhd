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
		x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		mode_i : in STD_LOGIC_VECTOR (1 downto 0);
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		result_o : out STD_LOGIC_VECTOR (63 downto 0));
end FP_Converter_float_to_int;

architecture behavioral of FP_Converter_float_to_int is

	constant BIAS : signed(E - 1 downto 0) := to_signed(2 ** (E - 1) - 1, E);

	alias exp : STD_LOGIC_VECTOR(E - 1 downto 0) is x_i(P - 2 downto P - E - 1);

	type int_width is array(0 to 3) of signed(E - 1 downto 0);
	constant INT_WIDTHS : int_width := (to_signed(31, E), to_signed(32, E), to_signed(63, E), to_signed(64, E));

	alias sign : STD_LOGIC is x_i(P - 1);
	
	signal int, signed_int, int_neg : STD_LOGIC_VECTOR (63 downto 0);
	signal exponent : signed (E - 1 downto 0);
	signal sign_reg, inexact, inexact_reg : STD_LOGIC;

	component FP_Classifier is
		generic (
			P : NATURAL;
			E : NATURAL;
			M : NATURAL);
		port (
			x_i : in STD_LOGIC_VECTOR (P - 2 downto 0);
			fp_class_o : out FP_INFO);
	end component FP_Classifier;

	component right_shifter is
		generic (SIZE : NATURAL);
		port (
			x_i : in unsigned (SIZE - 1 downto 0);
			shamt_i : in unsigned (num_bits(SIZE) - 1 downto 0);
			z_o : out unsigned (SIZE - 1 downto 0);
			sticky_bit_o : out STD_LOGIC);
	end component right_shifter;

	component rounder is
		generic (SIZE : NATURAL);
		port (
			x_i : in unsigned (SIZE - 1 downto 0);
			sign_i : in STD_LOGIC;
			rm_i : in STD_LOGIC_VECTOR (2 downto 0);
			round_sticky_i : in STD_LOGIC_VECTOR (1 downto 0);
			z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
	end component rounder;

	signal less_than_one, overflow, overflow_reg, special_case, not_zero, not_zero_neg, not_zero_pos : STD_LOGIC;
	signal overflow_value, overflow_value_final, overflow_value_reg : STD_LOGIC_VECTOR(63 downto 0);
	signal fp_class : FP_INFO;
	
	signal mantissa_shifted_reg : unsigned(63 downto 0);
	signal overflows : STD_LOGIC_VECTOR (3 downto 0);
	signal shamt : unsigned(6 downto 0);
	signal mantissa : unsigned(64 + M downto 0);
	signal mantissa_shifted : unsigned(64 + M downto 0);
	signal round_sticky, round_sticky_reg, mode : STD_LOGIC_VECTOR (1 downto 0);

    signal rm : STD_LOGIC_VECTOR (2 downto 0);

begin 

	FP_CLASSIFY : FP_Classifier generic map(P, E, M) port map(x_i(P - 2 downto 0), fp_class);

	exponent <= signed(exp) - BIAS;
	shamt <= unsigned(63 - resize(exponent, 7));

	less_than_one <= '1' when exponent < -1 else '0';

    OVERFLOW_CHECK: for i in 0 to 3 generate 
        overflows(i) <= '1' when exponent >= INT_WIDTHS(i) else '0';
    end generate; 
      
	with mode_i select overflow <= overflows(0) when "00",
	                               overflows(1) when "01",
	                               overflows(2) when "10",
	                               overflows(3) when "11",
	                               '0' when others;

    overflow_value <= (30 downto 0 => '1', others => '0') when mode_i = "00" else
    	              (63 => mode_i(0), others => '1');

	overflow_value_final <= not overflow_value when sign = '1' and fp_class.nan = '0' else 
		                    overflow_value;
	
	mantissa <= fp_class.normal & unsigned(x_i(M - 2 downto 0)) & (64 downto 0 => '0');
    mantissa_shifted <= shift_right(mantissa, to_integer(shamt)) when less_than_one = '0' else 
                        (64 downto 0 => '0') & fp_class.normal & unsigned(x_i(M - 2 downto 0));

    inexact <= or round_sticky;
	round_sticky <= mantissa_shifted(M) & (or mantissa_shifted(M - 1 downto 0));

	process(clk_i) begin 
	    if rising_edge(clk_i) then
            sign_reg <= sign;
            mantissa_shifted_reg <= mantissa_shifted(mantissa_shifted'left downto M + 1);
            round_sticky_reg <= round_sticky;
            overflow_value_reg <= overflow_value_final;
            rm <= rm_i;
            overflow_reg <= overflow or fp_class.nan or fp_class.inf;
            mode <= mode_i;
            inexact_reg <= inexact;

        end if; 
    end process;

	ROUNDING : rounder generic map(64) port map(mantissa_shifted_reg, sign_reg, rm, round_sticky_reg, int);	

    special_case <= overflow_reg or ( sign_reg and mode(0) and not_zero );

	int_neg <= STD_LOGIC_VECTOR(-signed(int));
	
    signed_int <= int_neg when sign_reg = '1' else int;
	not_zero_neg <= or (-signed(int));
	not_zero_pos <= or int;
	not_zero <= not_zero_neg when sign_reg = '1' else not_zero_pos;
	
	result_o <= overflow_value_reg when special_case = '1' else 
	            (63 downto 32 => signed_int(31)) & signed_int(31 downto 0) when mode(1) = '0' else
	            signed_int;
    
    
    fflags_o <= "10000" when special_case = '1' else "0000" & inexact_reg;
        
end behavioral;