library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity mul_dsp_unsigned is
	generic (
		X_SIZE : NATURAL := 24;
		Y_SIZE : NATURAL := 17);
	port (
		clk_i : in STD_LOGIC;
		enable_i : in STD_LOGIC;
		x_i : in unsigned (X_SIZE - 1 downto 0);
		y_i : in unsigned (Y_SIZE - 1 downto 0);
		result_o : out unsigned (X_SIZE + Y_SIZE - 1 downto 0));
end mul_dsp_unsigned;

architecture behavioral of mul_dsp_unsigned is

	signal X : unsigned(X_SIZE - 1 downto 0);
	signal Y : unsigned(Y_SIZE - 1 downto 0);

	signal product : unsigned(X_SIZE + Y_SIZE - 1 downto 0);
	attribute USE_DSP : STRING;
	attribute USE_DSP of product : signal is "TRUE";

begin

	MULTIPLICATION : process (clk_i)
	begin
		if rising_edge(clk_i) then
			if enable_i = '1' then
				X <= x_i;
				Y <= y_i;
			end if;
			product <= X * Y;
		end if;
	end process;

	result_o <= product;

end behavioral;