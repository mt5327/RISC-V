library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity FMA is
	generic (
		P : NATURAL;
		E : NATURAL;
		M : NATURAL);
	port (
        clk_i : in STD_LOGIC;
        rst_ni : in STD_LOGIC;
        enable_i : in STD_LOGIC;
        fp_op_i : in FPU_OP;
        rm_i  : in STD_LOGIC_VECTOR (2 downto 0);
        x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
        y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
        z_i : in STD_LOGIC_VECTOR (P-1 downto 0);
        is_boxed_i : in STD_LOGIC_VECTOR (2 downto 0);
        result_o : out FP_RESULT);
end FMA;

architecture behavioral of FMA is

	constant BIAS : signed(E+1 downto 0) := to_signed(2 ** (E - 1) - 1, E + 2);
	constant E_MIN : signed(E+1 downto 0) := 1 - BIAS;
	constant FP_ONE : STD_LOGIC_VECTOR(P - 1 downto 0) := (P - 3 downto P - E - 1 => '1', others => '0');
	constant LOWER_SUM_WIDTH : NATURAL := 2 * M + 3;
	constant SHIFT_SIZE : NATURAL := num_bits(3 * M + 4);

	constant PRODUCT_SIZE : NATURAL := 2 * M;
	constant INFINITY : STD_LOGIC_VECTOR (P - 2 downto 0) := (P - 2 downto P - E - 1 => '1', others => '0');

	signal product : unsigned(PRODUCT_SIZE - 1 downto 0);

	signal valid, sign_x, sign_z, sign_p, sign_p_reg, effective_substraction, effective_substraction_reg, sign, sign_reg, sticky_bit, is_product_anchored, is_product_anchored_reg : STD_LOGIC := '0';

	-- Exceptions
	signal invalid, invalid_reg, inexact, underflow, overflow_before_round, underflow_before_round, underflow_after_round, overflow, overflow_after_round, special_case, special_case_reg : STD_LOGIC;

	signal exponent_x, exponent_y, exponent_z : signed(E+1 downto 0);
	signal exponent_a, exponent_p, exponent_p_reg, exponent_pa : signed(E+1 downto 0);
	signal exponent_diff, exponent_diff_reg : signed(E+1 downto 0) := (others => '0');
	signal exponent_tent, exponent_a_reg : signed(E+1 downto 0);
	signal exp, exp_reg, exp_fin, exponent_tent_reg : signed (E+1 downto 0);

    signal exp_final : unsigned(E-1 downto 0);

    signal round_sticky : STD_LOGIC_VECTOR (1 downto 0);
	signal t, k, f : unsigned(LOWER_SUM_WIDTH - 1 downto 0);
	signal mantissa_x, mantissa_y : unsigned (M - 1 downto 0);
	signal P_mantissa, P_mantissa_reg, A_mantissa, A_mantissa_reg, s : unsigned (3 * M + 4 downto 0);
	signal mantissa_sum, mantissa_sum_reg : unsigned (3 * M + 5 downto 0);
	
	signal mantissa_z, mantissa_z_reg, shifted_mantissa_z : unsigned(3 * M + 4 downto 0);

	signal add_sticky_bit, add_sticky_bit_reg, sum_sticky_bit : STD_LOGIC;

	signal rounded_num : STD_LOGIC_VECTOR(P - 2 downto 0) := (others => '0');
	signal add_shamt, add_shamt_reg, norm_shamt, norm_shamt_pa, norm_shamt_subnormal, norm_shamt_subnormal_reg : unsigned(SHIFT_SIZE - 1 downto 0);
	signal lz_counter, lz_counter_reg : unsigned(num_bits(LOWER_SUM_WIDTH) - 1 downto 0);

	signal y, z: STD_LOGIC_VECTOR (P-1 downto 0);
    signal fp_valid : STD_LOGIC := '0';

	signal mantissa, mantissa_reg : unsigned(3 * M + 5 downto 0);
	signal mantissa_final : unsigned(M-1 downto 0);
	signal special_value, special_value_reg : STD_LOGIC_VECTOR (P-1 downto 0) := (others => '0');

    signal result, result_reg : STD_LOGIC_VECTOR (63 downto 0) := (others => '0');

    signal fflags_fma, fflags_reg : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');

	type state_type is (IDLE, MULTIPLY, ADD, SUM, NORMALIZE, ROUND, FINALIZE);
	signal state, next_state : state_type;
	signal fp_infos : fp_infos_t(0 to 2);

	type inputs_type is array (0 to 2) of STD_LOGIC_VECTOR (P-2 downto 0);
	signal inputs : inputs_type;
	signal rm : STD_LOGIC_VECTOR (2 downto 0);

	signal is_zero, is_zero_reg, enable, enable_output_regs, result_is_subnormal : STD_LOGIC;

	alias sign_y : STD_LOGIC is y(P - 1);

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
			x_i : in STD_LOGIC_VECTOR (P-2 downto 0);
			is_boxed_i : in STD_LOGIC;
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

    signal is_boxed : STD_LOGIC_VECTOR(2 downto 0);

	alias sum_carry : STD_LOGIC is mantissa_sum_reg(mantissa_sum_reg'left);

	component normalizer is
		generic (SIZE : NATURAL);
		port (
			x_i : in unsigned (SIZE - 1 downto 0);
			shamt_i : in unsigned (num_bits(SIZE) - 1 downto 0);
			z_o : out unsigned (SIZE - 1 downto 0));
	end component normalizer;

begin

    with fp_op_i select 
        is_boxed <=  '1' & is_boxed_i(1 downto 0) when FPU_ADD | FPU_SUB | FPU_MUL,
                     is_boxed_i when others;

	-- y is 1.0 if addition/subtraction
    with fp_op_i select y <=
        FP_ONE when FPU_ADD | FPU_SUB,
        y_i when others;

    with fp_op_i select z <=
        y_i when FPU_ADD | FPU_SUB,
        (others => '0') when FPU_MUL,
        z_i when others;

    
	nan <= fp_infos(0).nan or fp_infos(1).nan or fp_infos(2).nan;
	inf <= fp_infos(0).inf or fp_infos(1).inf or fp_infos(2).inf;
	special_case <= (inf or nan);
	product_inf <= fp_infos(0).inf or fp_infos(1).inf;

	-- Classify inputs

	inputs(0) <= x_i(P-2 downto 0);
	inputs(1) <= y(P-2 downto 0);
	inputs(2) <= z(P-2 downto 0);
	
	CLASSIFY : for i in 0 to 2 generate
		FP_CLASS : FP_Classifier generic map(P, E, M) port map(inputs(i), is_boxed(i), fp_infos(i));
	end generate;
	
	SPECIAL_CASES : process (fp_infos, effective_substraction, sign_z, sign_p, product_inf, nan)
	begin
		invalid <= '0';
		-- Canonical NaN
		special_value <= (P - 2 downto P - E - 2 => '1', others => '0');
		if (fp_infos(0).inf and fp_infos(1).zero) or
			(fp_infos(1).inf and fp_infos(0).zero) then
			invalid <= '1';
		elsif nan = '1' then
			invalid <= fp_infos(0).signaling_nan or fp_infos(1).signaling_nan or fp_infos(2).signaling_nan;
		elsif product_inf and fp_infos(2).inf and effective_substraction then
			invalid <= '1';
		elsif product_inf = '1' then
			special_value <= sign_p & INFINITY;
		elsif fp_infos(2).inf = '1' then
			special_value <= sign_z & INFINITY;
		end if;
	end process;

	SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_ni = '0' then
				state <= IDLE;
			else
				state <= next_state;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, enable, special_case)
	begin
		next_state <= state;
		case state is
			when IDLE =>
				if enable = '1' then
					if special_case = '1' then
						next_state <= ROUND;
					else
						next_state <= MULTIPLY;
					end if;
				end if;
			when MULTIPLY => next_state <= ADD; 
			when ADD => next_state <= SUM;
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

	with fp_op_i select sign_z <= not z(P - 1) when FPU_SUB | FPU_FMSUB | FPU_FNMSUB,
		z(P - 1) when others;

	effective_substraction <= sign_x xor sign_y xor sign_z;

	exponent_x <= "00" & signed(x_i(P - 2 downto P - E - 1));
	exponent_y <= "00" & signed(y(P - 2 downto P - E - 1));
	exponent_z <= "00" & signed(z(P - 2 downto P - E - 1));

	mantissa_x <= unsigned(fp_infos(0).normal & x_i(M - 2 downto 0));
	mantissa_y <= unsigned(fp_infos(1).normal & y(M - 2 downto 0));
	mantissa_z <= unsigned(fp_infos(2).normal & z(M - 2 downto 0)) & (2 * M + 4 downto 0 => '0');

	-- Calculate sign of the product
	sign_p <= sign_x xor sign_y;
	sign <= '1' when effective_substraction_reg = '1' and sum_carry = sign_p_reg else
		    '0' when effective_substraction_reg = '1' else sign_p_reg;
	
	-- Calculate exponent of the product
	exponent_p <= E_MIN when fp_infos(0).zero = '1' or fp_infos(1).zero = '1' else
	              exponent_x + exponent_y - BIAS + 2 - ((E downto 1 => '0') & fp_infos(0).normal) -
		          ((E downto 1 => '0') & fp_infos(1).normal);

	exponent_a <= exponent_z + 1 - ((E downto 1 => '0') & fp_infos(2).normal);
	exponent_diff <= exponent_z - exponent_p;
	exponent_tent <= exponent_p_reg when exponent_diff_reg <= 2 else exponent_a_reg;
    
    enable <= '1' when enable_i = '1' and state = IDLE else '0';
    enable_output_regs <= '1' when state = ROUND else '0';
 
	-- IDLE
	process (clk_i) 
	begin
	   if rising_edge(clk_i) then
	       if enable = '1' then
            mantissa_z_reg <= mantissa_z;
            special_value_reg <= special_value;
            sign_p_reg <= sign_p; 
            effective_substraction_reg <= effective_substraction;
            special_case_reg <= special_case;
            invalid_reg <= invalid;
            exponent_diff_reg <= exponent_diff;
            rm <= rm_i;
            exponent_p_reg <= exponent_p;
            exponent_a_reg <= exponent_a;
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
			
			product <= resize(P0, 106) + resize(P1, 106) + resize(P2, 106) + P3 + resize(P4, 106) + resize(P5, 106);
	end generate;

	SUMMAND_SHIFT_AMOUNT : process (exponent_diff_reg)
	begin
		if exponent_diff_reg <= -2 * M + 1 then
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
	   
	t <= P_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0) xor (A_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0));
	k <= P_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0) nor (A_mantissa_reg(LOWER_SUM_WIDTH - 1 downto 0));
	f <= t xor (not k(LOWER_SUM_WIDTH - 2 downto 0) & '1');

	lz_counter <= leading_zero_counter(f, lz_counter_reg'length);

	norm_shamt_subnormal <= unsigned("+"(to_signed(M + 4, norm_shamt'length), exponent_p_reg)(SHIFT_SIZE - 1 downto 0));
	is_product_anchored <= '1' when exponent_diff_reg <= 2 else '0';
    is_zero <= nor product;
	mantissa_sum <= ('0' & P_mantissa_reg) + ('0' & A_mantissa_reg) + effective_substraction_reg;

    process (clk_i) begin 
        if rising_edge(clk_i) then
            mantissa_sum_reg <= mantissa_sum;
            lz_counter_reg <= lz_counter;
            add_sticky_bit_reg <= add_sticky_bit;
            add_shamt_reg <= add_shamt;
            is_product_anchored_reg <= is_product_anchored;
            norm_shamt_subnormal_reg <= norm_shamt_subnormal;
            exponent_tent_reg <= exponent_tent;
            is_zero_reg <= is_zero;
        end if;
    end process;

	exponent_pa <= exponent_p_reg - signed(resize(lz_counter_reg, exponent_p'length)) + 2;

	s <= unsigned(-signed(mantissa_sum_reg(mantissa_sum_reg'left-1 downto 0))) when (effective_substraction_reg and not sum_carry) = '1' else
		 mantissa_sum_reg(mantissa_sum_reg'left-1 downto 0);
		 
	norm_shamt_pa <= to_unsigned(M + 2, norm_shamt_pa'length) + resize(lz_counter_reg, norm_shamt_pa'length);

	NORM_SHIFT_AMOUNT : process (all)
	begin
	   exp <= exponent_tent_reg;
	   norm_shamt <= add_shamt_reg;
	   if is_product_anchored_reg = '1' then
	       if exponent_pa(exponent_pa'left) = '0' and is_zero_reg = '0' then
	           exp <= exponent_pa;
	           norm_shamt <= norm_shamt_pa;
	       else
	     	   exp <= (others => '0');
	           norm_shamt <= norm_shamt_subnormal_reg;
	       end if;
	   end if;
    end process;

	mantissa <= shift_left('0' & s, to_integer(norm_shamt));
	
	process (clk_i) begin 
	   if rising_edge(clk_i) then 
	       mantissa_reg <= mantissa; 
	       sign_reg <= sign; 
	       exp_reg <= exp;
	   end if; 
    end process;
      
	ONE_BIT_NORMALIZATION : process (mantissa_reg, exp_reg)
    begin
		mantissa_final <= mantissa_reg(mantissa'left - 2 downto mantissa'left - M - 1);
        sticky_bit <= or mantissa_reg(mantissa'left-M-2 downto 0);
        exp_fin <= exp_reg;
        if mantissa_reg(mantissa_reg'left) = '1' then
            mantissa_final <= mantissa_reg(mantissa'left - 1 downto mantissa'left - M);
            sticky_bit <= or mantissa_reg(mantissa'left-M-1 downto 0);
            exp_fin <= exp_reg + 1;
        elsif mantissa_reg(mantissa_reg'left - 1) = '0' then
            mantissa_final <= mantissa_reg(mantissa'left-3 downto mantissa'left - M - 2);
            sticky_bit <= or mantissa_reg(mantissa'left-M-3 downto 0);
            exp_fin <= exp_reg - 1;
        end if;
	end process;

    round_sticky <= mantissa_final(0) & (sticky_bit or add_sticky_bit_reg );
	
	exp_final <= to_unsigned(2**E - 2, E) when overflow_before_round = '1' else unsigned(exp_fin(E-1 downto 0));
	
	ROUNDING : rounder generic map(P - 1) port map(exp_final & mantissa_final(M - 1 downto 1), sign_reg, rm, round_sticky, rounded_num);
 
    RESULT_GEN: if P = 32 generate 
        result <= (63 downto 32 => '1') & sign_reg & rounded_num when special_case_reg = '0' else 
                   (63 downto 32 => '1') & special_value_reg;
    else generate
        result <= sign_reg & rounded_num when special_case_reg = '0' else 
                  special_value_reg; 
    end generate;
    
    process (clk_i) 
    begin 
        if rising_edge(clk_i) then
            if enable_output_regs = '1' then
                result_reg <= result;
                fflags_reg <= fflags_fma;
            end if;
        end if;
    end process; 
    
    fp_valid <= '1' when state = FINALIZE else '0';
	result_o <= ( result_reg, fflags_reg, fp_valid );
            
    underflow_after_round <= nor rounded_num(rounded_num'left downto M - 1);
    
    overflow_before_round <= '1' when exp_fin >= 2**E - 1 else '0';
    overflow_after_round <= and rounded_num(rounded_num'left downto M - 1);

	overflow <= overflow_before_round or overflow_after_round;
	underflow <= underflow_after_round and inexact;
	inexact <= ( or round_sticky ) or overflow;
    	
	fflags_fma <= "00" & overflow & underflow & inexact when special_case_reg = '0' else
		          invalid_reg & "0000";

 end behavioral;   