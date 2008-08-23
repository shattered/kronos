library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use work.Kronos_Types.all;


entity SerialPort is
    port (
        -- Wishbone
        clk_i   : in std_logic;
        rst_i   : in std_logic;
        dat_i   : in std_logic_vector(7 downto 0);
        dat_o   : out std_logic_vector(7 downto 0);
        stb_i   : in std_logic;
        we_i    : in std_logic;
        ack_o   : out std_logic;
        -- Misc
        rx_int  : out std_logic;
	clock_sp: in std_logic;
        -- RS232
        TxD     : out std_logic;
        RxD     : in std_logic);
end SerialPort;

architecture Behavioral of SerialPort is
    signal state : integer := 0;
begin

    dat_o <= (dat_o'range => '0');
    rx_int <= '0';
    TxD <= '1';

    process (clk_i)
	variable ln : line;
    begin
        if clk_i'event and clk_i = '1' then
            if stb_i = '1' then
                if state < 6 then 
                    state <= state + 1;
                    ack_o <= '0';
                elsif state = 6 then 
                    state <= state + 1;
                    ack_o <= '1';
                else
                    if we_i = '0' then
                        assert false report "Serial port receiver not implemented" severity failure;
                    else
                        write(ln, "SerialPort: ");
                        write(ln, " dat_i ");
	                write_hex(ln, dat_i);
                        writeline(std.textio.output, ln);
                    end if;
                    state <= 0;
                    ack_o <= '0';
                end if;
            else
                ack_o <= '0';
            end if;
        end if;
    end process;

end Behavioral;
