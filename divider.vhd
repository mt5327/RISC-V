library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity divider is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		z_o : out STD_LOGIC_VECTOR (63 downto 0);
		op_i : in ALU_OP;
		div_valid_o : out STD_LOGIC);
end divider;

architecture behavioral of divider is

	signal a : unsigned (64 downto 0); -- sign bit
	signal quotient, new_q : STD_LOGIC_VECTOR (63 downto 0);
	signal x, y, remainder, new_r, b : unsigned(63 downto 0);
	signal normalized_divisor, estimated_divisor, estimated_divisor_div_2, divisor : unsigned(63 downto 0);
	signal is_signed, is_word, is_word_reg, sign, sign_reg, div_out, div_out_reg, div_by_zero, div_by_zero_reg, terminate, div_valid, x_sign, y_sign : STD_LOGIC := '0';
	signal clz_r, clz_y, clz_delta : unsigned (5 downto 0);

	signal q_bit1, q_bit2, result, z, z_reg, z_signed : STD_LOGIC_VECTOR (63 downto 0);

	type state_type is (IDLE, DIVIDE, FINALIZE);
	signal state, next_state : state_type;
 
	constant NEW_BIT_MASK : unsigned(63 downto 0) := (0 => '1', others => '0');

begin

	SYNC_PROC : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i = '1' then
				state <= IDLE;
				div_valid_o <= '0';
			else
				state <= next_state;
				div_valid_o <= div_valid;
			end if;
		end if;
	end process;

	NEXT_STATE_DECODE : process (state, enable_i, terminate, div_by_zero)
	begin
		next_state <= state;
		case (state) is
			when IDLE =>
				if enable_i = '1' then
				    if div_by_zero = '1' then
				        next_state <= FINALIZE;
				    else
    			        next_state <= DIVIDE;
    			    end if;
				end if;
			when DIVIDE =>
				if terminate = '1' then
					next_state <= FINALIZE;
				end if;
			when FINALIZE => next_state <= IDLE;
			when others => next_state <= IDLE;
		end case;
	end process;
	
	with op_i select 
	   is_word <= '1' when ALU_DIVW | ALU_REMW, '0' when others;

	with op_i select
	   is_signed <= '1' when ALU_DIV | ALU_REM | ALU_DIVW | ALU_REMW,
	                '0' when others;

	with op_i select
	   div_out <= '1' when ALU_DIV | ALU_DIVU | ALU_DIVW | ALU_DIVUW,
	              '0' when others;
	
	x <= unsigned(-signed(x_i)) when x_sign = '1' else unsigned(x_i);
	y <= unsigned(-signed(y_i)) when y_sign = '1' else unsigned(y_i);

	clz_r <= leading_zero_counter(remainder, clz_r'length);
	clz_y <= leading_zero_counter(divisor, clz_y'length);

	clz_delta <= clz_y - clz_r;
    normalized_divisor <= shift_left(divisor, to_integer(clz_y));
    x_sign <= x_i(x_i'left) and is_signed;
    y_sign <= y_i(y_i'left) and is_signed;
    
	q_bit1 <= STD_LOGIC_VECTOR(shift_left(NEW_BIT_MASK, to_integer(clz_delta)));
	q_bit2 <= '0' & q_bit1(63 downto 1);

	estimated_divisor <= shift_right(normalized_divisor, to_integer(clz_r));
	estimated_divisor_div_2 <= '0' & estimated_divisor(63 downto 1);

	a <= ('0' & remainder) - ('0' & estimated_divisor);
	b <= remainder - estimated_divisor_div_2;

	new_q <= quotient or q_bit2 when a(a'left) = '1' else
	         quotient or q_bit1;

	new_r <= b when a(a'left) = '1' else
	         a(63 downto 0);
	         
    with op_i select sign <=
		x_i(x_i'left) xor y_i(y_i'left) when ALU_DIV | ALU_DIVW, 
	 	 x_i(x_i'left) when ALU_REM | ALU_REMW,
			'0'	when others;
			
	div_by_zero <= nor y_i;
	
	terminate <= '1' when remainder < divisor else '0';

	DIVISION : process (clk_i)
	begin
		if rising_edge(clk_i) then
            case state is
                when IDLE => 
                    remainder <= x;
                    divisor <= y;
                    quotient <= (others => div_by_zero);
                    div_by_zero_reg <= div_by_zero;
                    sign_reg <= sign;
                    div_out_reg <= div_out;
                    is_word_reg <= is_word;
                    div_valid <= '0';
                when DIVIDE =>
                    if terminate = '0' then
                        quotient <= new_q;
                        remainder <= new_r;
                    else
                        div_valid <= '1';
                        z_reg <= z;
                    end if;
                when others =>
                    div_valid <= '0';
                    div_by_zero_reg <= '0';
                    div_out_reg <= '0';
                    is_word_reg <= '0';
            end case;
		end if; 
	end process;

	result <= quotient when div_out_reg = '1' else
	          STD_LOGIC_VECTOR(remainder);

	z_signed <= STD_LOGIC_VECTOR(-signed(result)) when sign_reg = '1' and div_by_zero_reg = '0' else
	     result;
	     
	z <= (63 downto 32 => z_signed(31)) & z_signed(31 downto 0) when is_word_reg = '1' else z_signed; 
	z_o <= z_reg;
    
end behavioral;