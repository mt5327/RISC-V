library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.constants.all;

entity multiplier is
	port (
		clk_i : in STD_LOGIC;
		rst_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		x_i : in STD_LOGIC_VECTOR (63 downto 0);
		y_i : in STD_LOGIC_VECTOR (63 downto 0);
		op_i : in ALU_OP;

		mul_valid_o : out STD_LOGIC;
		result_o : out STD_LOGIC_VECTOR (63 downto 0));
end multiplier;

architecture behavioral of multiplier is

	signal x, y : signed (64 downto 0);

	alias A0 : signed(15 downto 0) is x(15 downto 0);
	alias A1 : signed(23 downto 0) is x(39 downto 16);
	alias A2 : signed(24 downto 0) is x(64 downto 40);

	alias B0 : signed(12 downto 0) is y(12 downto 0);
	alias B1 : signed(16 downto 0) is y(29 downto 13);
	alias B2 : signed(16 downto 0) is y(46 downto 30);
	alias B3 : signed(17 downto 0) is y(64 downto 47);

	signal A0B0 : signed(28 downto 0);
	signal A0B1, A0B2, A1B0, A1B1, A1B2, A2B3 : signed(40 downto 0);
	signal A0B3, A1B3, A2B0, A2B1, A2B2 : signed(42 downto 0);

	signal P0 : signed(78 downto 0);
	signal P5 : signed(81 downto 0);

	signal P1 : signed(95 downto 0);
	signal P4 : signed(105 downto 0);

	signal P2 : signed (112 downto 0);
	signal P3, S : signed(127 downto 0);

	signal x_is_signed, y_is_signed, start_mul, mul_valid : STD_LOGIC;

	signal MULR : STD_LOGIC_VECTOR (127 downto 0);

	signal result : STD_LOGIC_VECTOR (63 downto 0);

	type state_type is (IDLE, MULTIPLY, FINALIZE);
	signal state, next_state : state_type;

	component mul_dsp_signed is
		generic (
			X_SIZE : NATURAL := 25;
			Y_SIZE : NATURAL := 18;
			SIZE : NATURAL := 41);
		port (
			clk_i : in STD_LOGIC;
			enable_i : in STD_LOGIC;
			x_i : in signed (X_SIZE - 1 downto 0);
			y_i : in signed (Y_SIZE - 1 downto 0);
			result_o : out signed (SIZE - 1 downto 0));
	end component mul_dsp_signed;

begin

	with op_i select
	   x_is_signed <= '1' when ALU_MULH | ALU_MULHSU, 
	                  '0' when others;
	
	y_is_signed <= '1' when op_i = ALU_MULH else '0';
	
	start_mul <= '1' when state = IDLE and enable_i = '1' else '0';

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

	NEXT_STATE_DECODE : process (state, enable_i)
	begin
		next_state <= state;
		case (state) is
			when IDLE =>
				if enable_i = '1' then
					next_state <= MULTIPLY;
				end if;
			when MULTIPLY => next_state <= FINALIZE;
			when others => next_state <= IDLE;
		end case;
	end process;

	x <= signed((x_i(x_i'left) and x_is_signed) & x_i);
	y <= signed((y_i(y_i'left) and y_is_signed) & y_i);

	MUL_A0B0 : mul_dsp_signed generic map(17, 14, 29) port map(clk_i, start_mul, ('0' & A0), ('0' & B0), A0B0);
	MUL_A0B1 : mul_dsp_signed port map(clk_i, start_mul, ('0' & A0 & X"00"), ('0' & B1), A0B1);
	MUL_A0B2 : mul_dsp_signed port map(clk_i, start_mul, ('0' & A0 & X"00"), ('0' & B2), A0B2);
	MUL_A0B3 : mul_dsp_signed generic map(SIZE => 43) port map(clk_i, start_mul, ('0' & A0 & X"00"), B3, A0B3);

	MUL_A1B0 : mul_dsp_signed port map(clk_i, start_mul, ('0' & A1), ('0' & B0 & X"0"), A1B0);
	MUL_A1B1 : mul_dsp_signed port map(clk_i, start_mul, ('0' & A1), ('0' & B1), A1B1);
	MUL_A1B2 : mul_dsp_signed port map(clk_i, start_mul, ('0' & A1), ('0' & B2), A1B2);
	MUL_A1B3 : mul_dsp_signed generic map(SIZE => 43) port map(clk_i, start_mul, ('0' & A1), B3, A1B3);

	MUL_A2B0 : mul_dsp_signed generic map(SIZE => 43) port map(clk_i, start_mul, A2, ('0' & B0 & X"0"), A2B0);
	MUL_A2B1 : mul_dsp_signed generic map(SIZE => 43) port map(clk_i, start_mul, A2, ('0' & B1), A2B1);
	MUL_A2B2 : mul_dsp_signed generic map(SIZE => 43) port map(clk_i, start_mul, A2, ('0' & B2), A2B2);
	MUL_A2B3 : mul_dsp_signed port map(clk_i, start_mul, A2, B3, A2B3);

	P0 <= A2B0 & X"000000000";
	P1 <= A2B1 & A1B0 & X"000";
	P2 <= A2B2 & A1B1 & A0B0;
	P3 <= A2B3 & A1B2 & A0B1 & "00000";
	P4 <= A1B3 & A0B2 & X"00000" & "00";
	P5 <= A0B3 & X"000000000" & "000";

	MULR <= STD_LOGIC_VECTOR(resize(P0, 128) + resize(P1, 128) + resize(P2, 128) + P3 + resize(P4, 128) + resize(P5, 128));

	MUX_OUTPUT : process (op_i, MULR)
	begin
        case op_i is
            when ALU_MULH | ALU_MULHU | ALU_MULHSU => result_o <= MULR(127 downto 64);
            when ALU_MULW => result_o <= (63 downto 32 => MULR(31)) & MULR(31 downto 0);
            when ALU_MUL => result_o <= MULR(63 downto 0);
            when others => result_o <= (others => '-');
        end case;
	end process;

	mul_valid_o <= '1' when state = FINALIZE else '0';

end behavioral;