library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity ATA33 is
        port (
            -- Hard Disc Drive
            dd       : inout std_logic_vector(15 downto 0);
            dmarq    : in std_logic;
            dior     : out std_logic;
            diow     : out std_logic;
            iordy    : in std_logic;
            dmack    : out std_logic;
            da       : out std_logic_vector(2 downto 0);
            cs0      : out std_logic;
            cs1      : out std_logic;
            dasp     : in std_logic;
            -- Wishbone
            clk_i    : in std_logic;
            rst_i    : in std_logic;
            adr_i    : in std_logic_vector(3 downto 0);
            dat_i    : in std_logic_vector(15 downto 0);
            dat_o    : out std_logic_vector(15 downto 0);
            stb_i    : in std_logic;
            we_i     : in std_logic;
            ack_o    : out std_logic);
end ATA33;

architecture Behavioral of ATA33 is

    constant mode   : natural range 0 to 4 := 2;

    signal state         : integer range 0 to 61;
    signal state_dd_off  : boolean;
    signal state_dio_on  : boolean;
    signal state_dio_off : boolean;
    signal state_iordy   : boolean;
    signal state_last    : boolean;

begin

    dmack <= '1';

    mode0_timing : if mode = 0 generate
    begin
        state_dio_on  <= state = 10;
        state_iordy   <= state = 16;
        state_dio_off <= state = 40;
        state_dd_off  <= state = 44;
        state_last    <= state = 61;
    end generate;

    mode1_timing : if mode = 1 generate
    begin
        state_dio_on  <= state = 8;
        state_iordy   <= state = 13;
        state_dio_off <= state = 26;
        state_dd_off  <= state = 29;
        state_last    <= state = 40;
    end generate;

    mode2_timing : if mode = 2 generate
    begin
        state_dio_on  <= state = 6;
        state_iordy   <= state = 11;
        state_dio_off <= state = 18;
        state_dd_off  <= state = 20;
        state_last    <= state = 25;
    end generate;

    mode3_timing : if mode = 3 generate
    begin
        state_dio_on  <= state = 6;
        state_iordy   <= state = 11;
        state_dio_off <= state = 15;
        state_dd_off  <= state = 17;
        state_last    <= state = 19;
    end generate;

    mode4_timing : if mode = 4 generate
    begin
        -- Note: state_last must not be equal state_dd_off
        state_dio_on  <= state = 5;
        state_iordy   <= state = 9;
        state_dio_off <= state = 12;
        state_dd_off  <= state = 13;
        state_last    <= state = 14;
    end generate;

    process (clk_i)
    begin
        if clk_i'event and clk_i = '1' then
            if rst_i = '1' then
                state <= 0;
                ack_o <= '0';
                dior <= '1';
                diow <= '1';
                da <= "000";
                cs0 <= '1';
                cs1 <= '1';
                dd <= (others => 'Z');
            else
                if state = 0 and stb_i = '1' then
                    da <= adr_i(2 downto 0);
                    cs0 <= adr_i(3);
                    cs1 <= not adr_i(3);
                elsif state = 1 and we_i = '1' then
                    dd(7 downto 0) <= dat_i(7 downto 0);
                elsif state = 2 and we_i = '1' and adr_i = "0000" then
                    dd(15 downto 8) <= dat_i(15 downto 8);
                elsif state_dd_off then
                    da <= "000";
                    cs0 <= '1';
                    cs1 <= '1';
                    dd <= (others => 'Z');
                end if;
                if state_dd_off then
                    ack_o <= stb_i;
                else
                    ack_o <= '0';
                end if;
                if state_dio_on then
                    diow <= not we_i;
                    dior <= we_i;
                elsif state_dio_off then
                    diow <= '1';
                    dior <= '1';
                    dat_o <= dd;
                end if;
                if state = 0 and stb_i = '0' then
                    null;
                elsif state_iordy and iordy = '0' then
                    null;
                elsif state_last then
                    state <= 0;
                else
                    state <= state + 1;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
