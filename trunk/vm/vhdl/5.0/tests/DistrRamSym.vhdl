library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_textio.all;

entity DistrRam is
    port (
        D    : in std_logic;
        WE   : in std_logic;
        WCLK : in std_logic;
        A    : in std_logic_vector(3 downto 0);
        DPRA : in std_logic_vector(3 downto 0);
        DPO  : out std_logic);
end DistrRam;

architecture Behaviour of DistrRam is
    subtype address is integer range 0 to 15;
    type ram_array is array (address) of std_logic;
    signal ram: ram_array := "0000000000000000";
begin

    DPO <= ram(conv_integer(unsigned(DPRA)));

    process (WCLK)
    begin
        if WCLK'event and WCLK = '1' and WE = '1' then
            ram(conv_integer(unsigned(A))) <= D;
        end if;
    end process;
end architecture Behaviour;
