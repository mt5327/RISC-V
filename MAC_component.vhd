library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.constants.ALL;

entity MAC_component is
    Generic ( P : natural; E : natural; M : natural);
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           enable_i : in STD_LOGIC;
           fp_op_i : in FPU_OP;
           rm_i : in STD_LOGIC_VECTOR (2 downto 0);
           x_i : in STD_LOGIC_VECTOR (P-1 downto 0);
           y_i : in STD_LOGIC_VECTOR (P-1 downto 0);
           z_i : in STD_LOGIC_VECTOR (P-1 downto 0);
           result_o : out STD_LOGIC_VECTOR (P-1 downto 0);
           fflags_o : out STD_LOGIC_VECTOR (4 downto 0);
           fp_valid_o : out STD_LOGIC );
end MAC_component;

architecture behavioral of MAC_component is
    
    constant BIAS : signed(E downto 0) := to_signed(2**(E-1) - 1, E+1);
    constant E_MIN : signed(E downto 0) := 1 - BIAS;    
    constant FP_ONE : STD_LOGIC_VECTOR(P-1 downto 0) := (P-3 downto P-E-1 => '1', others => '0');
    constant LOWER_SUM_WIDTH : natural := 2 * M + 3;

    constant PRODUCT_SIZE : natural := 2 * M;
    constant INFINITY : STD_LOGIC_VECTOR (P-2 downto 0) := (P-2 downto P-E-1 => '1', others => '0');

    alias sign_y : STD_LOGIC is Y_i(P-1); 
    
    signal product : unsigned(PRODUCT_SIZE - 1 downto 0);
    
    signal sign_x, sign_z, sign_p, effective_substraction, sign, sticky_bit_reg, round_bit_reg : STD_LOGIC := '0';
    
    -- Exceptions
    signal invalid, inexact, underflow, overflow, special_case : STD_LOGIC := '0';    
    
    signal exponent_x, exponent_y, exponent_z, exponent_p, exponent_a, exponent : signed (E downto 0);
    signal tentative_exp, exp_res : signed (E-1 downto 0);
    signal exp_fin : unsigned(E-1 downto 0);
    
    signal mantissa_x, mantissa_y : unsigned (M-1 downto 0); 
    signal P_mantissa, P_mantissa_reg, A_mantissa, A_mantissa_reg, mantissa_sum, sum, carry, carry_reg : unsigned (3 * M + 4 downto 0);        
    signal mantissa_z, shifted_mantissa_z : unsigned(3 * M + 4 downto 0);
            
    signal add_sticky_bit, sum_sticky_bit, sticky_bit, final_normalization : STD_LOGIC;
    
    signal num, num_reg : unsigned (P-2 downto 0);
    signal rounded_num : STD_LOGIC_VECTOR(P-2 downto 0);
        
    signal add_shamt, norm_shamt : unsigned(num_bits(3 * M + 4)-1 downto 0);
    signal lz_counter : unsigned(E-1 downto 0);

    signal y : STD_LOGIC_VECTOR (P-1 downto 0);
    
    signal mantissa : unsigned(M+2 downto 0);
    signal mantissa_final : unsigned(M-2 downto 0);
    signal special_value : STD_LOGIC_VECTOR (P-1 downto 0);
    
    type state_type is (IDLE, MULTIPLY, SHIFT_ADD, ADD, NORMALIZE, ROUND, FINALIZE);
    signal state, next_state : state_type;
    signal fp_infos : fp_infos_t(0 to 2);

    type inputs_type is array (0 to 2) of STD_LOGIC_VECTOR (P-2 downto 0);
    signal inputs : inputs_type;
    
    signal rm : STD_LOGIC_VECTOR (2 downto 0);
                
    signal enable_mul : STD_LOGIC;
    
   component mul_dsp_unsigned is
        Generic (X_SIZE : natural := 24; Y_SIZE : natural := 17);
        Port( clk_i: in STD_LOGIC;
              enable_i : in STD_LOGIC;
              x_i: in unsigned (X_SIZE-1 downto 0);
              y_i: in unsigned (Y_SIZE-1 downto 0);
              result_o: out unsigned (X_SIZE + Y_SIZE - 1 downto 0));
    end component mul_dsp_unsigned;
    
    component FP_Classifier is
        Generic (P : natural; E : natural; M : natural );
        Port ( x_i : in STD_LOGIC_VECTOR (P-2 downto 0);
               fp_class_o : out FP_INFO);
    end component FP_Classifier;

    component right_shifter is
        Generic ( SIZE : natural );
        Port ( x_i : in unsigned (SIZE-1 downto 0);
               shamt_i : in unsigned (num_bits(SIZE)-1 downto 0);
               z_o : out unsigned (SIZE-1 downto 0);
               sticky_bit_o : out STD_LOGIC);
    end component right_shifter;
    
    component rounder is
        Generic ( SIZE : natural );
        Port ( x_i : in unsigned (SIZE-1 downto 0);
               sign_i : in STD_LOGIC;
               rm_i : in STD_LOGIC_VECTOR (2 downto 0);
               round_sticky_i : in STD_LOGIC_VECTOR(1 downto 0);
               z_o : out STD_LOGIC_VECTOR (SIZE-1 downto 0));
    end component rounder;

    component LZC is
        Generic (M : natural );
        Port ( x_i : in unsigned(2 * M + 2 downto 0);
               z_o : out unsigned(num_bits(3 * M + 4)-1 downto 0));
    end component LZC;

    signal nan, inf, product_inf : STD_LOGIC;

    alias mantissa_lower : unsigned (LOWER_SUM_WIDTH-1 downto 0) is sum(LOWER_SUM_WIDTH-1 downto 0);
    alias sum_carry : STD_LOGIC is mantissa_sum(mantissa_sum'left);
    signal round_bit : STD_LOGIC;
    
    component normalizer is
        Generic (SIZE : natural; M : natural );
        Port ( x_i : in unsigned (SIZE-1 downto 0);
               shamt_i : in unsigned (num_bits(SIZE)-1 downto 0);
               mantissa_final_o : out unsigned (M+2 downto 0);
               sticky_bit_o : out STD_LOGIC);
    end component normalizer;
    
begin
    -- y input is 1.0 if addition/subtraction
    with fp_op_i select y <= 
        FP_ONE when FPU_ADD | FPU_SUB, 
           y_i when others;
    
    -- Classify inputs
    inputs(0) <= x_i(P-2 downto 0); inputs(1) <= y_i(P-2 downto 0); inputs(2) <= z_i(P-2 downto 0);
    CLASSIFY: for i in 0 to 2 generate
        FP_CLASS: FP_Classifier generic map ( P, E, M ) port map ( inputs(i), fp_infos(i));
    end generate;

    nan <= fp_infos(0).nan or fp_infos(1).nan or fp_infos(2).nan;
    inf <= fp_infos(0).inf or fp_infos(1).inf or fp_infos(2).inf;
    special_case <= ( inf or nan ) and enable_i;
    product_inf <= fp_infos(0).inf or fp_infos(1).inf;

    SPECIAL_CASES: process(fp_infos, effective_substraction, sign_z, sign_p , product_inf, nan)    
    begin
        invalid <= '0';
        -- Canonical NaN
        special_value <= (P-2 downto P-E => '1', others => '0');        
        if ( fp_infos(0).inf and fp_infos(1).zero ) or 
           ( fp_infos(1).inf and fp_infos(0).zero ) then
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

    SYNC_PROC: process(clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                state <= IDLE;
            else
                state <= next_state;
            end if; 
        end if;
    end process;
    
    NEXT_STATE_DECODE: process(state, enable_i, special_case)
    begin
        next_state <= state;
        case state is
            when IDLE => 
                if enable_i = '1' then 
                    if special_case = '1' then
                        next_state <= FINALIZE; 
                    else 
                        next_state <= MULTIPLY;
                    end if;
                end if;
            when MULTIPLY => next_state <= SHIFT_ADD;
            when SHIFT_ADD => next_state <= NORMALIZE;
            when NORMALIZE => next_state <= FINALIZE;
            when FINALIZE => next_state <= IDLE;
            when others => next_state <= IDLE;
        end case;     
    end process;
    
    enable_mul <= '1' when enable_i = '1' and state = IDLE else '0';
      
    with fp_op_i select sign_x <=
       not x_i(P-1) when FPU_FNMADD | FPU_FNMSUB,
       x_i(P-1) when others;
        
    with fp_op_i select sign_z <= not z_i(P-1) when FPU_SUB | FPU_FMSUB | FPU_FNMSUB,
                                      z_i(P-1) when others; 
    
    effective_substraction <= sign_x xor sign_y xor sign_z;
        
    exponent_x <= signed('0' & x_i(P-2 downto P-E-1));   
    exponent_y <= signed('0' & y(P-2 downto P-E-1));
    exponent_z <= signed('0' & z_i(P-2 downto P-E-1));
        
    mantissa_x <= unsigned(fp_infos(0).normal & x_i(M-2 downto 0));
    mantissa_y <= unsigned(fp_infos(1).normal & y(M-2 downto 0));
    mantissa_z <= unsigned(fp_infos(2).normal & z_i(M-2 downto 0)) & (2 * M + 4 downto 0 => '0');

    exponent <= exponent_z - exponent_p; 
    final_normalization <= '1' when exponent > 2 and fp_infos(2).subnormal = '0' else '0';
    
    -- Calculate sign and exponent of products
    sign_p <= sign_x xor sign_y;
    exponent_p <= exponent_x + exponent_y - BIAS + 2 - ((E downto 1 => '0') & fp_infos(0).normal) -
                                        ((E downto 1 => '0') & fp_infos(1).normal);

    exponent_a <= exponent_z + 1 - ((E downto 1 => '0') & fp_infos(2).normal);
     
    tentative_exp <= exponent_z(E-1 downto 0) when exponent > 0 else exponent_p(E-1 downto 0);
    exp_res <= exponent_p(E-1 downto 0) - signed(lz_counter) + 2 when exponent <= 2 else tentative_exp;               

    -- Multiply mantissas
    MANTISSA_MUL: 
    case M generate
        
        when 24 =>
            alias A0 : unsigned (23 downto 0) is mantissa_x;
            alias B0 : unsigned (16 downto 0) is mantissa_y(16 downto 0);
            alias B1 : unsigned (6 downto 0) is  mantissa_y(23 downto 17);

            signal A0B0 : unsigned(40 downto 0);
            signal A0B1 : unsigned(30 downto 0); 
        begin

            MUL_SP_A0B0: mul_dsp_unsigned
                port map ( clk_i, enable_mul, A0, B0, A0B0);
           
            MUL_SP_A0B1: mul_dsp_unsigned generic map ( 24, 7 )
                port map ( clk_i, enable_mul, A0, B1, A0B1);                    
                                        
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

            MUL_DP_A0B0: mul_dsp_unsigned generic map ( 5, 2 ) port map ( clk_i, enable_mul, A0, B0, A0B0);

            MUL_DP_A0B1: mul_dsp_unsigned generic map ( 7, 17 ) port map ( clk_i, enable_mul, A0 & "00", B1, A0B1 );
                
            MUL_DP_A0B2: mul_dsp_unsigned port map ( clk_i, enable_mul, A0 & X"0000" & "000", B2, A0B2);
                
            MUL_DP_A0B3: mul_dsp_unsigned port map ( clk_i, enable_mul, A0 & X"0000" & "000", B3, A0B3 ); 
            
            MUL_DP_A1B0: mul_dsp_unsigned generic map ( 24, 7 ) port map ( clk_i, enable_mul, A1, B0 & X"0" & "0", A1B0 );
             
            MUL_DP_A1B1: mul_dsp_unsigned port map ( clk_i, enable_mul, A1, B1, A1B1 );
                     
            MUL_DP_A1B2: mul_dsp_unsigned port map ( clk_i, enable_mul, A1, B2, A1B2 ); 
                     
            MUL_DP_A1B3: mul_dsp_unsigned port map ( clk_i, enable_mul, A1, B3, A1B3 );         
            
            MUL_DP_A2B0: mul_dsp_unsigned port map ( clk_i, enable_mul, A2, B0 & X"000" & "000", A2B0 );            
            
            MUL_DP_A2B1: mul_dsp_unsigned port map ( clk_i, enable_mul, A2, B1, A2B1 );       
                     
            MUL_DP_A2B2: mul_dsp_unsigned port map ( clk_i, enable_mul, A2, B2, A2B2 );    
            
            MUL_DP_A2B3: mul_dsp_unsigned port map ( clk_i, enable_mul, A2, B3, A2B3 );    
            
            P0 <= A2B0 & X"000" & "00";               
            P1 <= A2B1 & A1B0;
            P2 <= A2B2 & A1B1 & A0B0;
            P3 <= A2B3 & A1B2 & A0B1;
            P4 <= A1B3 & A0B2;
            P5 <= A0B3 & X"0000" & "0";
             
            S0 <= resize(P0, P5'length) + P5;                    
            S1 <= resize(P1, P4'length) + P4;
            S2 <= resize(P2, P3'length) + P3;
            product <= resize(S0, 106) + resize(S1, 106) + S2;
    end generate;

    P_mantissa <= (3 * M + 4 downto PRODUCT_SIZE+2 => '0') & product & "00";

    SUMMAND_SHIFT_AMOUNT: process(exponent)
    begin
        if exponent <= -2*M + 1 then
            add_shamt <= to_unsigned(3 * M + 4, add_shamt'length);
        elsif exponent <= M + 2 then
            add_shamt <= to_unsigned(M + 4 - to_integer(exponent), add_shamt'length);
        else
            add_shamt <= (others => '0');
        end if;    
    end process;
    
    -- Shift addend right, calculate sticky
    ADD_SHIFTER: right_shifter generic map(mantissa_z'length)
    port map (x_i => mantissa_z, shamt_i => add_shamt, z_o => shifted_mantissa_z, sticky_bit_o => add_sticky_bit);
    
    A_mantissa <= not shifted_mantissa_z when effective_substraction = '1' else shifted_mantissa_z;   

    carry(carry'left downto 1) <= (others => '0');
    carry(0) <= effective_substraction;
    
    process(clk_i)
    begin
        if rising_edge(clk_i) then
            P_mantissa_reg <= P_mantissa;
            A_mantissa_reg <= A_mantissa;
            carry_reg <= carry;
        end if;
    end process;

    mantissa_sum <= P_mantissa_reg + A_mantissa_reg + carry_reg;

    lz_counter <= leading_zero_counter(mantissa_lower, lz_counter'length);
    
    sign <= '1' when effective_substraction = '1' and sum_carry /= sign_p else  
            '0' when effective_substraction = '1' else sign_p;

    NORMALIZATION_SHIFT_AMOUNT: process(exponent, lz_counter, add_shamt)
    begin
        norm_shamt <= add_shamt;
        if exponent <= 2 then
            norm_shamt <= to_unsigned(M + 2, norm_shamt'length) + resize(lz_counter, norm_shamt'length);
        end if;
    end process;
    
    sum <= unsigned(-signed(mantissa_sum)) when effective_substraction = '1' and sum_carry = '1' else mantissa_sum;    
    
    NORMALIZATION: normalizer generic map (sum'length, M) port map (sum, norm_shamt, mantissa, sum_sticky_bit);
    
    ONE_BIT_NORMALIZATION: process(exp_res, mantissa, add_sticky_bit, sum_sticky_bit, final_normalization)
    begin            
        exp_fin <= unsigned(exp_res);
        mantissa_final <= mantissa(M downto 2);
        round_bit <= mantissa(1); 
        sticky_bit <= mantissa(0) or add_sticky_bit or sum_sticky_bit;
        if final_normalization = '1' then
            if mantissa(M+2) = '1' then
                exp_fin <= unsigned(exp_res) + 1;
                mantissa_final <= mantissa(M+1 downto 3);  
                round_bit <= mantissa(2);
                sticky_bit <= mantissa(1) or mantissa(0) or add_sticky_bit or sum_sticky_bit;                
            elsif mantissa(M+1) = '0' then  
                exp_fin <= unsigned(exp_res) - 1; 
                mantissa_final <= mantissa(M-1 downto 1);  
                round_bit <= mantissa(0);
                sticky_bit <= add_sticky_bit or sum_sticky_bit;
            end if;   
        end if;       
    end process;      
    
    num <= exp_fin & mantissa_final;
    process(clk_i)
    begin
        if rising_edge(clk_i) then
            num_reg <= num;
            round_bit_reg <= round_bit;
            sticky_bit_reg <= sticky_bit;
            rm <= rm_i;
        end if;
    end process;
    
    ROUNDING: rounder generic map (num_reg'length)
    port map (num_reg, sign, rm, round_bit_reg & sticky_bit_reg, rounded_num);
    
    underflow <= ( nor rounded_num(rounded_num'left downto M-1) ) and inexact;  
    overflow <= and rounded_num(rounded_num'left downto M-1);
    inexact <= round_bit_reg or sticky_bit_reg or overflow;
    
    result_o <= STD_LOGIC_VECTOR(sign & rounded_num) when special_case = '0' else special_value;
      
    fflags_o <= "00" & overflow & underflow & inexact when special_case = '0' else
                invalid & "0000";

    fp_valid_o <= '1' when state = FINALIZE else '0';
          
end behavioral;
   