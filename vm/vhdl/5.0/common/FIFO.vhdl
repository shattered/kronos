library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

library unisim;
use unisim.vcomponents.all;

entity FIFO is
        generic (
            bits   : in integer := 8);
        port (
            clock  : in std_logic;
            reset  : in std_logic;
            rd     : in std_logic;
            wr     : in std_logic;
            empty  : out std_logic;
            full   : out std_logic;
            full25 : out std_logic; -- more then 25% full
            full75 : out std_logic; -- more then 75% full
            d_in   : in std_logic_vector(bits - 1 downto 0);
            d_out  : out std_logic_vector(bits - 1 downto 0));

end FIFO;

architecture Behavioral of FIFO is
    signal cnt: std_logic_vector(3 downto 0);
    signal empty_i: std_logic;
    signal full_i: std_logic;
begin

    data_bits: for i in 0 to bits - 1 generate
    begin
        data_srl: SRL16E
        port map(   D => d_in(i),
                   CE => wr,
                  CLK => clock,
                   A0 => cnt(0),
                   A1 => cnt(1),
                   A2 => cnt(2),
                   A3 => cnt(3),
                    Q => d_out(i));
    end generate data_bits;

    empty <= empty_i;
    full <= full_i;
    full_i <= '1' when cnt = "1111" else '0';
    full25 <= cnt(3) or cnt(2);
    full75 <= cnt(3) and cnt(2);
    
    process (clock)
    begin
        if clock'event and clock = '1' then
            if reset = '1' then
                empty_i <= '1';
                cnt <= "0000";
            elsif wr = '1' and rd = '0' then
                if empty_i = '1' then
                    empty_i <= '0';
                elsif full_i = '0' then
                    cnt <= cnt + 1;
                end if;
            elsif wr = '0' and rd = '1' then
                if cnt = "0000" then
                    empty_i <= '1';
                else
                    cnt <= cnt - 1;
                end if;
            end if;
        end if;
    end process; 

end Behavioral;