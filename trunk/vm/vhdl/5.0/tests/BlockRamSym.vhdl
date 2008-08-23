library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_textio.all;
use std.textio.all;

-- Note: XST cannot infer block RAM with two writable ports

entity BlockRam is
    -- mode : read-first
    -- clock: rise
    -- write enable: high
    -- clock enable: high
    port (
	clock  : in std_logic;
	en0    : in std_logic;
	en1    : in std_logic;
	we0    : in std_logic;
	we1    : in std_logic;
	a0     : in std_logic_vector(8 downto 0);
	a1     : in std_logic_vector(8 downto 0);
	di0    : in std_logic_vector(31 downto 0);
	di1    : in std_logic_vector(31 downto 0);
	do0    : out std_logic_vector(31 downto 0);
	do1    : out std_logic_vector(31 downto 0));
end BlockRam;


architecture Behaviour of BlockRam is
    subtype address is integer range 0 to 2**9 - 1;
    type ram_array is array (address) of std_logic_vector(31 downto 0);
    signal ram: ram_array := (ram_array'range => "00000000000000000000000000000000");
begin
    process (clock)
    begin
	if clock'event and clock = '1' and en0 = '1' then
            do0 <= ram(conv_integer(unsigned(a0)));
        end if;
	if clock'event and clock = '1' and en1 = '1' then
            do1 <= ram(conv_integer(unsigned(a1)));
        end if;
	if clock'event and clock = '1' and en0 = '1' and we0 = '1' then
            assert en1 = '0' or we1 = '0' or a1 /= a0 report "BlockRam write conflict" severity failure;
	    ram(conv_integer(unsigned(a0))) <= di0;
        end if;
	if clock'event and clock = '1' and en1 = '1' and we1 = '1' then
            assert en0 = '0' or we0 = '0' or a1 /= a0 report "BlockRam write conflict" severity failure;
	    ram(conv_integer(unsigned(a1))) <= di1;
        end if;
    end process;
end architecture Behaviour;
