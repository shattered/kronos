library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity SerialPort is
    generic (
        clock_sp_rate : in real := 200000000.0 / 7;
        baud_rate : in integer := 115200);
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
    constant divider_max : natural := integer(clock_sp_rate / (31.0 * real(baud_rate))) - 1;
    type STATE_TYPE is (IDLE, START_BIT, BIT0, BIT1, BIT2, BIT3, BIT4, BIT5, BIT6, BIT7);
    signal tx_dt      : std_logic_vector(7 downto 0);
    signal tx_rg      : std_logic_vector(7 downto 0);
    signal rx_rg      : std_logic_vector(7 downto 0);
    signal tx_state   : STATE_TYPE;
    signal rx_state   : STATE_TYPE;
    signal tx_phase   : natural range 0 to 30;
    signal rx_phase   : natural range 0 to 30;
    signal tx_done    : std_logic;
    signal rx_done    : std_logic;
    signal tx_req     : std_logic;
    signal rx_req     : std_logic;
    signal tx_req_sp  : std_logic;
    signal rx_req_sp  : std_logic;
    signal tx_ack_sp  : std_logic;
    signal rx_ack_sp  : std_logic;
    signal tx_ack     : std_logic;
    signal rx_ack     : std_logic;
    signal tx_fifo_rd : std_logic;
    signal rx_fifo_rd : std_logic;
    signal tx_fifo_wr : std_logic;
    signal rx_fifo_wr : std_logic;
    signal tx_empty   : std_logic;
    signal rx_empty   : std_logic;
    signal tx_full    : std_logic;
    signal rx_full    : std_logic;
    signal divider    : natural range 0 to divider_max;
    signal reset_sp   : std_logic;
begin

    tx_fifo : entity work.FIFO
    	port map (
            clock  => clk_i,
            reset  => rst_i,
            rd     => tx_fifo_rd,
            wr     => tx_fifo_wr,
            empty  => tx_empty,
            full   => tx_full,
            d_in   => dat_i,
            d_out  => tx_dt);

    rx_fifo : entity work.FIFO
    	port map (
            clock  => clk_i,
            reset  => rst_i,
            rd     => rx_fifo_rd,
            wr     => rx_fifo_wr,
            empty  => rx_empty,
            full   => rx_full,
            d_in   => rx_rg,
            d_out  => dat_o);

    rx_int <= not rx_empty;
    tx_fifo_wr <= stb_i and we_i and not tx_full;
    tx_fifo_rd <= tx_done and not tx_ack;
    rx_fifo_wr <= rx_done and not rx_ack;
    rx_fifo_rd <= stb_i and not we_i and not rx_empty;
    ack_o <= tx_fifo_wr or rx_fifo_rd;

    process (clk_i)
    begin
        if clk_i'event and clk_i = '1' then
            if rst_i = '1' then
                tx_req <= '0';
                rx_req <= '0';
                tx_done <= '0';
                rx_done <= '0';
            else
                tx_ack <= tx_ack_sp;
                rx_ack <= rx_ack_sp;
                if tx_ack = '1' then
                    tx_req <= '0';
                    tx_done <= '1';
                elsif tx_done = '1' then
                    tx_done <= '0';
                elsif tx_empty = '0' then
                    tx_req <= '1';
                    if tx_req = '0' then
                        tx_rg <= tx_dt;
                    end if;
                end if;
                if rx_ack = '1' then
                    rx_req <= '0';
                    rx_done <= '1';
                elsif rx_done = '1' then
                    rx_done <= '0';
                elsif rx_full = '0' then
                    rx_req <= '1';
                end if;
            end if;
        end if;
    end process;

    process (clock_sp)
    begin
        if clock_sp'event and clock_sp = '1' then
            reset_sp <= rst_i;
            if reset_sp = '1' then
                tx_state <= IDLE;
                rx_state <= IDLE;
                tx_ack_sp <= '0';
                rx_ack_sp <= '0';
            else
                tx_req_sp <= tx_req;
                rx_req_sp <= rx_req;
                if tx_req_sp = '0' then
                    tx_ack_sp <= '0';
                end if;
                if rx_req_sp = '0' then
                    rx_ack_sp <= '0';
                end if;
                if divider = divider_max then
                    divider <= 0;
                    if tx_phase = 30 then
                        case tx_state is
       	                when IDLE =>
                            if tx_req_sp = '1' and tx_ack_sp = '0' then
                                TxD <= '0';
                                tx_state <= START_BIT;
                            else
                                TxD <= '1';
                            end if;
       	                when START_BIT =>
                            TxD <= tx_rg(0);
                            tx_state <= BIT0;
                        when BIT0 =>
                            TxD <= tx_rg(1);
                            tx_state <= BIT1;
       	                when BIT1 =>
                            TxD <= tx_rg(2);
                            tx_state <= BIT2;
       	                when BIT2 =>
                            TxD <= tx_rg(3);
                            tx_state <= BIT3;
       	                when BIT3 =>
                            TxD <= tx_rg(4);
                            tx_state <= BIT4;
       	                when BIT4 =>
                            TxD <= tx_rg(5);
                            tx_state <= BIT5;
                        when BIT5 =>
                            TxD <= tx_rg(6);
                            tx_state <= BIT6;
       	                when BIT6 =>
                            TxD <= tx_rg(7);
                            tx_state <= BIT7;
                            tx_ack_sp <= tx_req_sp;
                        when BIT7 =>
                            TxD <= '1';
                            tx_state <= IDLE;
                        end case;
                        tx_phase <= 0;
                    else
                        tx_phase <= tx_phase + 1;
                    end if;
                    if rx_phase = 30 then
                        case rx_state is
       	                when IDLE =>
                            if RxD = '0' then
                                rx_state <= START_BIT;
                            end if;
       	                when START_BIT =>
                            rx_rg(0) <= RxD;
                            rx_state <= BIT0;
       	                when BIT0 =>
                            rx_rg(1) <= RxD;
                            rx_state <= BIT1;
       	                when BIT1 =>
                            rx_rg(2) <= RxD;
                            rx_state <= BIT2;
       	                when BIT2 =>
                            rx_rg(3) <= RxD;
                            rx_state <= BIT3;
       	                when BIT3 =>
                            rx_rg(4) <= RxD;
                            rx_state <= BIT4;
       	                when BIT4 =>
                            rx_rg(5) <= RxD;
                            rx_state <= BIT5;
       	                when BIT5 =>
                            rx_rg(6) <= RxD;
                            rx_state <= BIT6;
       	                when BIT6 =>
                            rx_rg(7) <= RxD;
                            rx_state <= BIT7;
                            rx_ack_sp <= rx_req_sp;
       	                when BIT7 =>
                            rx_state <= IDLE;
	                end case;
                        rx_phase <= 0;
                    elsif rx_state = IDLE and RxD = '1' then
                        rx_phase <= 24;
                    else
                        rx_phase <= rx_phase + 1;
                    end if;
                else
                    divider <= divider + 1;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
