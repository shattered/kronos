library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library unisim;
use unisim.Vcomponents.all;

entity ChipScope is
    port (
        clk   : in std_logic;
        data  : in std_logic_vector(31 downto 0);
        trig  : in std_logic_vector(7 downto 0));
end ChipScope;

architecture Behavioral of ChipScope is

    component icon port (
        control0: inout std_logic_vector(35 downto 0));
    end component;

    component ila port (
        control     : inout std_logic_vector(35 downto 0);
        clk         : in std_logic;
        data        : in std_logic_vector(31 downto 0);
        trig0       : in std_logic_vector(7 downto 0));
    end component;

    signal control : std_logic_vector(35 downto 0);

begin

    i_icon : icon port map (
        control0    => control);

    i_ila : ila port map (
          control   => control,
          clk       => clk,
          data      => data,
          trig0     => trig);

end Behavioral;
