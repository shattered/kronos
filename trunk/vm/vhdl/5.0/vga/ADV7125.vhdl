library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity ADV7125 is
    port (
        -- VGA
        vga_r       : out std_logic_vector(7 downto 0);
        vga_g       : out std_logic_vector(7 downto 0);
        vga_b       : out std_logic_vector(7 downto 0);
        vga_blank_n : out std_logic;
        vga_psave_n : out std_logic;
        vga_sync_n  : out std_logic;
        vga_vsync   : out std_logic;
        vga_hsync   : out std_logic;
        vga_clk     : out std_logic;
        -- Memory
        reset       : in std_logic;
        clk         : in std_logic;
        dat         : in std_logic_vector(95 downto 0);
        vsync       : out std_logic;
        req0        : out std_logic; 
        req1        : out std_logic;
        ack         : in std_logic);
end ADV7125;

architecture Behaviour of ADV7125 is

    constant x_res    : natural := 800;
    constant h_sync_s : natural := 840;
    constant h_sync_e : natural := 968;
    constant x_size   : natural := 1056;

    constant y_res    : natural := 600;
    constant v_sync_s : natural := 601;
    constant v_sync_e : natural := 605;
    constant y_size   : natural := 628;

    signal x_pos      : std_logic_vector(10 downto 0);
    signal y_pos      : std_logic_vector(10 downto 0);
    signal blank      : std_logic;
    signal vsync_int  : std_logic;
    signal bf_reset   : std_logic;
    signal bf_read    : std_logic;
    signal bf_empty   : std_logic;
    signal bf_full    : std_logic;
    signal bf_full25  : std_logic;
    signal bf_full75  : std_logic;
    signal bf_dat     : std_logic_vector(95 downto 0);
    signal pixel      : std_logic_vector(23 downto 0);

begin

    pixel_buffer : entity work.FIFO
        generic map (
            bits   => 96)
    	port map (
            clock  => clk,
            reset  => bf_reset,
            rd     => bf_read,
            wr     => ack,
            empty  => bf_empty,
            full   => bf_full,
            full25 => bf_full25,
            full75 => bf_full75,
            d_in   => dat,
            d_out  => bf_dat);

    vga_psave_n <= '1';
    vga_sync_n <= '0';
    vga_clk <= clk;
    vsync <= vsync_int;

    pixel <=
        bf_dat(23 downto 0) when x_pos(1 downto 0) = "00" else
        bf_dat(47 downto 24) when x_pos(1 downto 0) = "01" else
        bf_dat(71 downto 48) when x_pos(1 downto 0) = "10" else
        bf_dat(95 downto 72);

    bf_reset <= reset or vsync_int;
    bf_read <= not blank and x_pos(1) and x_pos(0);
    req0 <= not (vsync_int or bf_full75);
    req1 <= not (vsync_int or bf_full25);
    blank <= '1' when x_pos >= x_res or y_pos >= y_res else '0';

    process (clk)
    begin 
        if clk'event and clk = '1' then
            vga_r <= pixel(23 downto 16);
            vga_g <= pixel(15 downto 8);
            vga_b <= pixel(7 downto 0);
            if reset = '1' then
                x_pos <= (x_pos'range => '0');
                y_pos <= (y_pos'range => '0');
            elsif x_pos = x_size - 1 then
                x_pos <= (x_pos'range => '0');
                if y_pos = y_size - 1 then
                    y_pos <= (y_pos'range => '0');
                else
                    y_pos <= y_pos + 1;
                end if;
            else
                x_pos <= x_pos + 1;
            end if;
            vga_blank_n <= not blank;
            if reset = '1' or x_pos = h_sync_e then
                vga_hsync <= '0';
            elsif x_pos = h_sync_s then
                vga_hsync <= '1';
            end if;
            if reset = '1' or y_pos = v_sync_e then
                vga_vsync <= '0';
                vsync_int <= '0';
            elsif y_pos = v_sync_s then
                vga_vsync <= '1';
                vsync_int <= '1';
            end if;
        end if;
    end process;

end Behaviour;
