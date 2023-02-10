library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity mul_dsp_signed is
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
end mul_dsp_signed;

architecture behavioral of mul_dsp_signed is

	signal x : signed(X_SIZE - 1 downto 0);
	signal y : signed(Y_SIZE - 1 downto 0);

	signal product : signed(SIZE - 1 downto 0);

begin

	MULTIPLICATION : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if enable_i = '1' then
				x <= x_i;
				y <= y_i;
			end if;
			product <= "*"(x, y)(SIZE - 1 downto 0);
		end if;
	end process;
	
    result_o <= product;

end behavioral;