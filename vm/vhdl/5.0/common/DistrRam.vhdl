library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

library unisim;
use unisim.vcomponents.all;

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
begin
    ram: RAM16X1D port map ( 
        D     => D,
        WE    => WE,
        WCLK  => WCLK,
        A0    => A(0),
        A1    => A(1),
        A2    => A(2),
        A3    => A(3),
        DPRA0 => DPRA(0),
        DPRA1 => DPRA(1),
        DPRA2 => DPRA(2),
        DPRA3 => DPRA(3),
        DPO   => DPO);
end architecture Behaviour;
