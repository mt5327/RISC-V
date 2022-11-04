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

	signal int, rounded_int : STD_LOGIC_VECTOR (63 downto 0);
	signal exponent : signed (E - 1 downto 0);
	signal sticky_bit : STD_LOGIC;

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

	signal less_than_one, overflow, underflow : STD_LOGIC;
	signal overflow_value, overflow_value_final : STD_LOGIC_VECTOR(63 downto 0);
	signal fp_class : FP_INFO;
	
	signal overflows : STD_LOGIC_VECTOR (3 downto 0);
	signal shamt, normal_shamt : unsigned(6 downto 0);
	signal mantissa : unsigned(64 + M downto 0);
	signal mantissa_shifted : unsigned(64 + M downto 0);
	signal round_sticky : STD_LOGIC_VECTOR (1 downto 0);
	signal rm : STD_LOGIC_VECTOR (2 downto 0);
	constant MAX_SHIFT : unsigned(6 downto 0) := to_unsigned(65, shamt'length);

begin

	FP_CLASSIFY : FP_Classifier generic map(P, E, M) port map(x_i(P - 2 downto 0), fp_class);

	exponent <= signed(exp) - BIAS;
	normal_shamt <= unsigned(63 - resize(exponent, 7));

	less_than_one <= '1' when exponent < -1 else '0';

    OVERFLOW_CHECK: for i in 0 to 3 generate 
        overflows(i) <= '1' when exponent >= INT_WIDTHS(i) else '0';
    end generate; 


	with mode_i select overflow <= overflows(0) when "00",
	                               overflows(1) when "01",
	                               overflows(2) when "10",
	                               overflows(3) when "11",
	                               '0' when others;

	SHIFT_AMOUNT : process (all)
	begin
		shamt <= normal_shamt;
		if overflow = '1' then
			shamt <= (others => '0');
		elsif less_than_one then
			shamt <= MAX_SHIFT;
		end if;
	end process;

	with mode_i select 
		overflow_value <= (30 downto 0 => '1', others => '0') when "00",
	                      (31 downto 0 => '1', others => '0') when "01",
	                      (63 => '0', others => '1') when "10",
	                      (others => '1') when others;

	overflow_value_final <= not overflow_value when sign = '1' and fp_class.nan = '0' else 
		                    overflow_value;
	
	mantissa <= '1' & unsigned(x_i(M - 2 downto 0)) & (64 downto 0 => '0');
	process(clk_i) begin if rising_edge(clk_i) then
    	mantissa_shifted <= shift_right(mantissa, to_integer(shamt));
    end if; end process;
	
	round_sticky <= mantissa_shifted(M) & (or mantissa_shifted(M - 1 downto 0));

	ROUNDING : rounder generic map(64) port map(mantissa_shifted(mantissa_shifted'left downto M + 1), sign, rm_i, round_sticky, int);
	
	result_o <= overflow_value when overflow = '1' else int;
    fflags_o <= overflow & "000" & ( (or round_sticky ) and ( not overflow ));
        
end behavioral;