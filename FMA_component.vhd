library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FMA_component is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		fp_op_i : in FPU_OP;
		rm_i : in STD_LOGIC_VECTOR (2 downto 0);
		x_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		y_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		z_i : in STD_LOGIC_VECTOR (P - 1 downto 0);
		result_o : out STD_LOGIC_VECTOR (P - 1 downto 0);
		fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
		fp_valid_o : out STD_LOGIC);
end FMA_component;

architecture behavioral of FMA_component is

	constant BIAS : signed(E downto 0) := to_signed(2 ** (E - 1) - 1, E + 1);
	constant E_MIN : signed(E downto 0) := 1 - BIAS;
	constant FP_ONE : STD_LOGIC_VECTOR(P - 1 downto 0) := (P - 3 downto P - E - 1 => '1', others => '0');
	constant LOWER_SUM_WIDTH : NATURAL := 2 * M + 3;
	constant SHIFT_SIZE : NATURAL := num_bits(3 * M + 4);

	constant PRODUCT_SIZE : NATURAL := 2 * M;
	constant INFINITY : STD_LOGIC_VECTOR (P - 2 downto 0) := (P - 2 downto P - E - 1 => '1', others => '0');

	alias sign_y : STD_LOGIC is y_i(P - 1);

	signal product : unsigned(PRODUCT_SIZE - 1 downto 0);

	signal valid, sign_x, sign_z, sign_p, sign_p_reg, effective_substraction, effective_substraction_reg, sign, sign_reg, sticky_bit, is_product_anchored, is_product_anchored_reg : STD_LOGIC := '0';

	-- Exceptions
	signal invalid, inexact, underflow, overflow, special_case, special_case_reg : STD_LOGIC;

	signal exponent_x, exponent_y, exponent_z : signed(E downto 0);
	signal exponent_a, exponent_p : signed(E downto 0);
	signal exponent_diff, exponent_diff_reg : signed(E downto 0);
	signal exponent_tent : signed(E - 1 downto 0);

	signal exponent_p_reg, exponent_pa : unsigned(E downto 0);

	signal exp, exp_res, exp_fin, exponent_tent_reg : unsigned (E - 1 downto 0);
	signal t, k, f : unsigned(LOWER_SUM_WIDTH - 1 downto 0);
	signal mantissa_x, mantissa_y : unsigned (M - 1 downto 0);
	signal P_mantissa, P_mantissa_reg, A_mantissa, A_mantissa_reg, mantissa_sum, mantissa_sum_reg, s : unsigned (3 * M + 4 downto 0);
	signal mantissa_z, mantissa_z_reg, shifted_mantissa_z : unsigned(3 * M + 4 downto 0);

	signal add_sticky_bit, sum_sticky_bit, final_normalization, final_normalization_reg : STD_LOGIC;

	signal rounded_num : STD_LOGIC_VECTOR(P - 2 downto 0) := (others => '0');
	signal num : unsigned (P - 2 downto 0);
	signal add_shamt, add_shamt_reg, norm_shamt, norm_shamt_pa, norm_shamt_subnormal, norm_shamt_subnormal_reg : unsigned(SHIFT_SIZE - 1 downto 0);
	signal lz_counter, lz_counter_reg : unsigned(num_bits(LOWER_SUM_WIDTH) - 1 downto 0);

	signal y, result, result_reg : STD_LOGIC_VECTOR (P - 1 downto 0);

	signal mantissa, mantissa_reg : unsigned(3 * M + 4 downto 0);
	signal mantissa_final : unsigned(M downto 0);
	signal special_value, special_value_reg : STD_LOGIC_VECTOR (P - 1 downto 0);

	type state_type is (IDLE, MULTIPLY, SUM, ST1, NORMALIZE, ROUND, FINALIZE);
	signal state, next_state : state_type;
	signal fp_infos : fp_infos_t(0 to 2);

	type inputs_type is array (0 to 2) of STD_LOGIC_VECTOR (P - 2 downto 0);
	signal inputs : inputs_type;
	signal exponent_plus_1, exponent_minus_1 : unsigned(E - 1 downto 0);
	signal rm : STD_LOGIC_VECTOR (2 downto 0);

	signal enable, result_is_subnormal : STD_LOGIC;

	component mul_dsp_unsigned is
		generic (
			X_SIZE : NATURAL := 24;
			Y_SIZE : NATURAL := 17);
		port (
			clk_i : in STD_LOGIC;
			enable_i : in STD_LOGIC;
			x_i : in unsigned (X_SIZE - 1 downto 0);
			y_i : in unsigned (Y_SIZE - 1 downto 0);
			result_o : out unsigned (X_SIZE + Y_SIZE - 1 downto 0));
	end component mul_dsp_unsigned;

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
			round_sticky_i : in STD_LOGIC_VECTOR(1 downto 0);
			z_o : out STD_LOGIC_VECTOR (SIZE - 1 downto 0));
	end component rounder;

	signal nan, inf, product_inf : STD_LOGIC;

	alias sum_carry : STD_LOGIC is mantissa_sum_reg(mantissa_sum'left);

	component LZC_110_F100_uid2 is
		port (
			clk : in STD_LOGIC;
			I : in unsigned(109 downto 0);
			O : out unsigned(6 downto 0));
	end component;

	component normalizer is
		generic (SIZE : NATURAL);
		port (
			x_i : in unsigned (SIZE - 1 downto 0);
			shamt_i : in unsigned (num_bits(SIZE) - 1 downto 0);
			z_o : out unsigned (SIZE - 1 downto 0));
	end component normalizer;

begin
	-- y is 1.0 if addition/subtraction
	with fp_op_i select y <=
		FP_ONE when FPU_ADD | FPU_SUB,
		y_i when others;

	-- Classify inputs
	inputs(0) <= x_i(P - 2 downto 0);
	inputs(1) <= y_i(P - 2 downto 0);
	inputs(2) <= z_i(P - 2 downto 0);
	CLASSIFY : for i in 0 to 2 generate
		FP_CLASS : FP_Classifier generic map(P, E, M) port map(inputs(i), fp_infos(i));
	end generate;

	nan <= fp_infos(0).nan or fp_infos(1).nan or fp_infos(2).nan;
	inf <= fp_infos(0).inf or fp_infos(1).inf or fp_infos(2).inf;
	special_case <= (inf or nan) and enable_i;
	product_inf <= fp_infos(0).inf or fp_infos(1).inf;

	SPECIAL_CASES : process (fp_infos, effective_substraction, sign_z, sign_p, product_inf, nan)
	begin
		invalid <= '0';
		-- Canonical NaN
		special_value <= (P - 2 downto P - E => '1', others => '0');
		if (fp_infos(0).inf and fp_infos(1).zero) or
			(fp_infos(1).inf and fp_infos(0).zero) then
			invalid <= '1';
		elsif nan = '1' then
			invalid <= fp_infos(0).signaling_nan or fp_infos(1).signaling_nan or fp_infos(2).signaling_nan;
		elsif product_inf and fp_infos(2).inf and effective_substraction then
			invalid <= '1';
		elsif product_inf then
			special_value <= sign_p & INFINITY;
		elsif fp_infos(2).inf then
			special_value <= sign_z & INFINITY;
		end if;
	end process;

	SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				state <= IDLE;
			else
				state <= next_state;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, enable_i, special_case)
	begin
		next_state <= state;
		case state is
			when IDLE =>
				if enable_i = '1' then
					if special_case = '1' then
						next_state <= ROUND;
					else
						next_state <= MULTIPLY;
					end if;
				end if;
			when MULTIPLY => next_state <= SUM;
			when SUM => next_state <= NORMALIZE;
			when NORMALIZE => next_state <= ROUND;
			when ROUND => next_state <= FINALIZE;
			when FINALIZE => next_state <= IDLE;
			when others => next_state <= IDLE;
		end case;
	end process;

	with fp_op_i select sign_x <=
		not x_i(P - 1) when FPU_FNMADD | FPU_FNMSUB,
		x_i(P - 1) when others;

	with fp_op_i select sign_z <= not z_i(P - 1) when FPU_SUB | FPU_FMSUB | FPU_FNMSUB,
		z_i(P - 1) when others;

	effective_substraction <= sign_x xor sign_y xor sign_z;

	exponent_x <= signed('0' & x_i(P - 2 downto P - E - 1));
	exponent_y <= signed('0' & y(P - 2 downto P - E - 1));
	exponent_z <= signed('0' & z_i(P - 2 downto P - E - 1));

	mantissa_x <= unsigned(fp_infos(0).normal & x_i(M - 2 downto 0));
	mantissa_y <= unsigned(fp_infos(1).normal & y(M - 2 downto 0));
	mantissa_z <= unsigned(fp_infos(2).normal & z_i(M - 2 downto 0)) & (2 * M + 4 downto 0 => '0');

	-- Calculate sign of the product
	sign_p <= sign_x xor sign_y;
	sign <= '1' when effective_substraction_reg = '1' and sum_carry /= sign_p_reg else
		'0' when effective_substraction_reg = '1' else sign_p_reg;
	-- Calculate exponent of the product
	exponent_p <= exponent_x + exponent_y - BIAS + 2 - ((E downto 1 => '0') & fp_infos(0).normal) -
		((E downto 1 => '0') & fp_infos(1).normal);

	exponent_a <= exponent_z + 1 - ((E downto 1 => '0') & fp_infos(2).normal);
	exponent_diff <= exponent_z - exponent_p;
	exponent_tent <= exponent_p(E - 1 downto 0) when exponent_diff <= 2 else exponent_a(E - 1 downto 0);

	final_normalization <= '1' when exponent_diff > 2 and fp_infos(2).subnormal = '0' else '0';
	enable <= '1' when enable_i = '1' and state = IDLE else '0';
	-- enable_sum_regs <= '1' when state = SUM else '0';
	-- enable_norm_regs <= '1' when state = NORMALIZE else '0';

	-- IDLE
	process (clk_i) begin
		if rising_edge(clk_i) then
			if enable = '1' then
				mantissa_z_reg <= mantissa_z;
				exponent_p_reg <= unsigned(exponent_p);
				exponent_diff_reg <= exponent_diff;
				exponent_tent_reg <= unsigned(exponent_tent);
				final_normalization_reg <= final_normalization;
				rm <= rm_i;
				sign_p_reg <= sign_p;
			end if;
		end if;
	end process;

	-- Multiply mantissas
	MANTISSA_MUL :
	case M generate

		when 24 =>
			alias A0 : unsigned (23 downto 0) is mantissa_x;
			alias B0 : unsigned (16 downto 0) is mantissa_y(16 downto 0);
			alias B1 : unsigned (6 downto 0) is mantissa_y(23 downto 17);

			signal A0B0 : unsigned(40 downto 0);
			signal A0B1 : unsigned(30 downto 0);
		begin

			MUL_SP_A0B0 : mul_dsp_unsigned
			port map(clk_i, enable, A0, B0, A0B0);

			MUL_SP_A0B1 : mul_dsp_unsigned generic map(24, 7)
			port map(clk_i, enable, A0, B1, A0B1);

			product <= (A0B1 & X"0000" & '0') + (X"0" & "00" & A0B0);

		when 53 =>
			alias A0 : unsigned (4 downto 0) is mantissa_x(4 downto 0);
			alias A1 : unsigned (23 downto 0) is mantissa_x(28 downto 5);
			alias A2 : unsigned (23 downto 0) is mantissa_x(52 downto 29);

			alias B0 : unsigned (1 downto 0) is mantissa_y(1 downto 0);
			alias B1 : unsigned (16 downto 0) is mantissa_y(18 downto 2);
			alias B2 : unsigned (16 downto 0) is mantissa_y(35 downto 19);
			alias B3 : unsigned (16 downto 0) is mantissa_y(52 downto 36);

			signal A0B0 : unsigned(6 downto 0);
			signal A0B1 : unsigned(23 downto 0);

			signal A1B0 : unsigned(30 downto 0);
			signal A0B2, A1B1, A1B2, A2B3 : unsigned(40 downto 0);
			signal A0B3, A1B3, A2B0, A2B1, A2B2 : unsigned(40 downto 0);

			signal P0 : unsigned(54 downto 0);
			signal P5, S0 : unsigned(57 downto 0);

			signal P1 : unsigned(71 downto 0);
			signal P4, S1 : unsigned(81 downto 0);

			signal P2 : unsigned (88 downto 0);
			signal P3, S2, S : unsigned(105 downto 0);
		begin

			MUL_DP_A0B0 : mul_dsp_unsigned generic map(5, 2) port map(clk_i, enable, A0, B0, A0B0);

			MUL_DP_A0B1 : mul_dsp_unsigned generic map(7, 17) port map(clk_i, enable, A0 & "00", B1, A0B1);

			MUL_DP_A0B2 : mul_dsp_unsigned port map(clk_i, enable, A0 & X"0000" & "000", B2, A0B2);

			MUL_DP_A0B3 : mul_dsp_unsigned port map(clk_i, enable, A0 & X"0000" & "000", B3, A0B3);

			MUL_DP_A1B0 : mul_dsp_unsigned generic map(24, 7) port map(clk_i, enable, A1, B0 & X"0" & "0", A1B0);

			MUL_DP_A1B1 : mul_dsp_unsigned port map(clk_i, enable, A1, B1, A1B1);

			MUL_DP_A1B2 : mul_dsp_unsigned port map(clk_i, enable, A1, B2, A1B2);

			MUL_DP_A1B3 : mul_dsp_unsigned port map(clk_i, enable, A1, B3, A1B3);

			MUL_DP_A2B0 : mul_dsp_unsigned port map(clk_i, enable, A2, B0 & X"000" & "000", A2B0);

			MUL_DP_A2B1 : mul_dsp_unsigned port map(clk_i, enable, A2, B1, A2B1);

			MUL_DP_A2B2 : mul_dsp_unsigned port map(clk_i, enable, A2, B2, A2B2);

			MUL_DP_A2B3 : mul_dsp_unsigned port map(clk_i, enable, A2, B3, A2B3);

			P0 <= A2B0 & X"000" & "00";
			P1 <= A2B1 & A1B0;
			P2 <= A2B2 & A1B1 & A0B0;
			P3 <= A2B3 & A1B2 & A0B1;
			P4 <= A1B3 & A0B2;
			P5 <= A0B3 & X"0000" & "0";

			--      S0 <= resize(P0, P5'length) + P5;                    
			--    S1 <= resize(P1, P4'length) + P4;
			--  S2 <= resize(P2, P3'length) + P3;
			product <= resize(P0, 106) + resize(P1, 106) + resize(P2, 106) + P3 + resize(P4, 106) + resize(P5, 106);
	end generate;

	SUMMAND_SHIFT_AMOUNT : process (exponent_diff_reg)
	begin
		if exponent_diff_reg <= - 2 * M + 1 then
			add_shamt <= to_unsigned(3 * M + 4, add_shamt'length);
		elsif exponent_diff_reg <= M + 2 then
			add_shamt <= to_unsigned(M + 4 - to_integer(exponent_diff_reg), add_shamt'length);
		else
			add_shamt <= (others => '0');
		end if;
	end process;

	-- Shift addend right, calculate sticky
	ADD_SHIFTER : right_shifter generic map(mantissa_z'length)
	port map(x_i => mantissa_z_reg, shamt_i => add_shamt, z_o => shifted_mantissa_z, sticky_bit_o => add_sticky_bit);

	P_mantissa <= (3 * M + 4 downto PRODUCT_SIZE + 2 => '0') & product & "00";
	A_mantissa <= not shifted_mantissa_z when effective_substraction_reg = '1' else
		shifted_mantissa_z;

	process (clk_i) begin
		if rising_edge(clk_i) then
			P_mantissa_reg <= P_mantissa;
			A_mantissa_reg <= A_mantissa;
		end if;
	end process;

	mantissa_sum <= P_mantissa_reg + A_mantissa_reg;

	t <= P_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0) xor (A_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0));
	k <= P_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0) nor (A_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0));
	f <= t xor (not k(LOWER_SUM_WIDTH - 2 downto 0) & '1');

	lz_counter <= leading_zero_counter(f, lz_counter'length);

	norm_shamt_subnormal <= "+"(to_unsigned(M + 4, norm_shamt'length), exponent_tent_reg)(num_bits(3 * M + 4) - 1 downto 0);
	is_product_anchored <= '1' when exponent_diff_reg <= 2 else '0';

	process (clk_i)
	begin
		if rising_edge(clk_i) then
			mantissa_sum_reg <= mantissa_sum;
			norm_shamt_subnormal_reg <= norm_shamt_subnormal;
			sign_reg <= sign;
			is_product_anchored_reg <= is_product_anchored;
			add_shamt_reg <= add_shamt;
			lz_counter_reg <= lz_counter;
		end if;
	end process;

	s <= unsigned(-signed(mantissa_sum_reg)) when ((not effective_substraction_reg) and sum_carry) = '1' else
		mantissa_sum_reg;

	exponent_pa <= exponent_p_reg - resize(lz_counter_reg, exponent_p'length);

	norm_shamt_pa <= to_unsigned(M + 2, norm_shamt_pa'length) + resize(lz_counter_reg, norm_shamt_pa'length);

	exp <= (others => '0') when is_product_anchored_reg = '1' and exponent_pa(exponent_pa'left) = '1' else
		exponent_pa(E - 1 downto 0) when is_product_anchored_reg = '1' else
		exponent_tent_reg(E - 1 downto 0);

	norm_shamt <= norm_shamt_pa when (is_product_anchored_reg and (not exponent_pa(exponent_pa'left))) = '1' else
		norm_shamt_subnormal_reg when is_product_anchored = '1' else
		add_shamt_reg;

	mantissa <= shift_left(s, to_integer(norm_shamt));
	--NORMALIZATION: normalizer generic map (s'length) port map (s_reg, norm_shamt, mantissa);

	process (clk_i)
	begin
		if rising_edge(clk_i) then
			mantissa_reg <= mantissa;
		end if;
	end process;

	exponent_plus_1 <= exp + 1;
	exponent_minus_1 <= exp - 1;
	ONE_BIT_NORMALIZATION : process (exp, mantissa_reg, final_normalization_reg, exponent_plus_1, exponent_minus_1)
	begin
		exp_fin <= exp;
		mantissa_final <= mantissa_reg(mantissa_reg'left - 1 downto mantissa_reg'left - M - 1);
		if final_normalization_reg = '1' then
			if mantissa_reg(mantissa'left) = '1' then
				exp_fin <= exponent_plus_1;
				mantissa_final <= mantissa_reg(mantissa'left - 2 downto mantissa'left - M - 2);
			elsif mantissa_reg(mantissa'left - 1) = '0' then
				exp_fin <= exponent_minus_1;
				mantissa_final <= mantissa_reg(mantissa'left downto mantissa'left - M);
			end if;
		end if;
	end process;
	ROUNDING : rounder generic map(P - 1) port map(exp_fin & mantissa_final(M - 1 downto 1), sign_reg, rm, mantissa_final(0) & '0', rounded_num);

	underflow <= (nor rounded_num(rounded_num'left downto M - 1)) and inexact;
	overflow <= and rounded_num(rounded_num'left downto M - 1);
	inexact <= mantissa_final(0) or '0' or overflow;

	result <= sign_reg & rounded_num when special_case = '0' else special_value;

	result_o <= result_reg;
	process (clk_i) begin
		if rising_edge(clk_i) then
			result_reg <= result;
		end if;
	end process;
	fflags_o <= "00" & overflow & underflow & inexact when special_case = '0' else
		invalid & "0000";

	fp_valid_o <= '1' when state = FINALIZE else '0';

end behavioral;