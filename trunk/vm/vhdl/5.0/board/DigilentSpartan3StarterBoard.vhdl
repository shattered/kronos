library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library unisim;
use unisim.Vcomponents.all;

entity DigilentSpartan3StarterBoard is
    port (
        clock_in  : in std_logic;
	hex_seg   : out std_logic_vector(6 downto 0);
	hex_an    : out std_logic_vector(3 downto 0);
	hex_dot   : out std_logic;
	button    : in std_logic_vector(3 downto 0);
	led       : out std_logic_vector(7 downto 0);
        -- 256K x 32 SRAM
        sram_adr  : out std_logic_vector(17 downto 0);
        sram_dat  : inout std_logic_vector(31 downto 0) bus;
        sram_ce1  : out std_logic;
        sram_ub1  : out std_logic;
        sram_lb1  : out std_logic;
        sram_ce2  : out std_logic;
        sram_ub2  : out std_logic;
        sram_lb2  : out std_logic;
        sram_we   : out std_logic;
        sram_oe   : out std_logic;
        -- ATA interface
        ata_reset : out std_logic;
        ata_dd    : inout std_logic_vector(15 downto 0) bus;
        ata_dmarq : in std_logic;
        ata_dior  : out std_logic;
        ata_diow  : out std_logic;
        ata_iordy : in std_logic;
        ata_dmack : out std_logic;
        ata_intrq : in std_logic;
        ata_da    : out std_logic_vector(2 downto 0);
        ata_cs0   : out std_logic;
        ata_cs1   : out std_logic;
        ata_dasp  : in std_logic;
        -- Serial port
        TxD       : out std_logic;
        RxD       : in std_logic);
end DigilentSpartan3StarterBoard;

architecture Behaviour of DigilentSpartan3StarterBoard is

    constant CLOCK_RATE   : integer := 50; -- MHz

    signal display     : std_logic_vector(15 downto 0);
    signal clock       : std_logic;
    signal clock_bus   : std_logic;
    signal reset       : std_logic;
    signal hex_led_cnt : std_logic_vector(15 downto 0);
    signal hex         : std_logic_vector(3 downto 0);
    signal hex_an0     : std_logic_vector(3 downto 0);
    signal reset_timer : std_logic_vector(19 downto 0);
    signal clk_in_ibufg: std_logic;
    signal clk_cpu_buf : std_logic;
    signal clk_bus_buf : std_logic;
    signal ireq        : std_logic_vector(3 downto 0);
    signal adr         : std_logic_vector(18 downto 0);
    signal dat_i       : std_logic_vector(31 downto 0);
    signal dat_cpu     : std_logic_vector(31 downto 0);
    signal dat_ata     : std_logic_vector(15 downto 0);
    signal dat_sio     : std_logic_vector(7 downto 0);
    signal halted      : std_logic;
    signal idle        : std_logic;
    signal we          : std_logic;
    signal cyc         : std_logic;
    signal stb         : std_logic;
    signal ack         : std_logic;
    signal err         : std_logic;
    signal lock        : std_logic;
    signal stb_cpu     : std_logic;
    signal stb_mem     : std_logic;
    signal stb_sio     : std_logic;
    signal stb_ata     : std_logic;
    signal ack_mem     : std_logic;
    signal ack_sio     : std_logic;
    signal ack_ata     : std_logic;
    signal int_sio     : std_logic;
    signal sram_dat_en : std_logic;

    subtype timer_type is integer range 0 to CLOCK_RATE * 20000 - 1;
    signal timer       : timer_type;
    signal int_time    : std_logic;

begin

    clk_in_ibufg_inst : IBUFG
        port map (
            i => clock_in,
            o => clk_in_ibufg);

    dcm_cpu_clock : DCM
        generic map (
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 2,
            CLKFX_MULTIPLY => 2,
            CLKIN_DIVIDE_BY_2 => false,
            CLKIN_PERIOD => 20.0,
            CLKOUT_PHASE_SHIFT => "NONE",
            CLK_FEEDBACK => "1X",
            DESKEW_ADJUST => "SYSTEM_SYNCHRONOUS",
            DFS_FREQUENCY_MODE => "LOW",
            DLL_FREQUENCY_MODE => "LOW",
            DUTY_CYCLE_CORRECTION => true,
            FACTORY_JF => X"C080",
            PHASE_SHIFT => 0,
            STARTUP_WAIT => true)
        port map (
            CLKIN    => clk_in_ibufg,
            CLKFB    => clock,
            RST      => '0',
            PSEN     => '0',
            PSINCDEC => '0',
            PSCLK    => '0',
            DSSEN    => '0',
            CLK0     => clk_cpu_buf);

    clock_cpu_bufg : BUFG
        port map (
            i => clk_cpu_buf,
            o => clock);

    dcm_bus_clock : DCM
        generic map (
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 2,
            CLKFX_MULTIPLY => 2,
            CLKIN_DIVIDE_BY_2 => false,
            CLKIN_PERIOD => 20.0,
            CLKOUT_PHASE_SHIFT => "NONE",
            CLK_FEEDBACK => "1X",
            DESKEW_ADJUST => "SYSTEM_SYNCHRONOUS",
            DFS_FREQUENCY_MODE => "LOW",
            DLL_FREQUENCY_MODE => "LOW",
            DUTY_CYCLE_CORRECTION => true,
            FACTORY_JF => X"C080",
            PHASE_SHIFT => 0,
            STARTUP_WAIT => true)
        port map (
            CLKIN    => clk_in_ibufg,
            CLKFB    => clock_bus,
            RST      => '0',
            PSEN     => '0',
            PSINCDEC => '0',
            PSCLK    => '0',
            DSSEN    => '0',
            CLK0     => clk_bus_buf);

    clock_bus_bufg : BUFG
        port map (
            i => clk_bus_buf,
            o => clock_bus);

    cpu : entity Kronos 
        generic map (
            address_size => 19)
        port map (
            rs_mode  => "00",
            halt     => button(2),
	    halted   => halted,
            idle     => idle,
            ireq     => ireq,
            -- Wishbone
            clk_i    => clock,
	    rst_i    => reset,
            adr_o    => adr,
	    dat_i    => dat_i,
	    dat_o    => dat_cpu,
            we_o     => we,
            stb_o    => stb,
            cyc_o    => cyc,
            ack_i    => ack,
            err_i    => err,
            lock_o   => lock);

    sio_port : entity SerialPort
        generic map (
            clock_sp_rate => real(CLOCK_RATE * 1000000),
            baud_rate => 38400)
        port map (
            rx_int   => int_sio,
  	    clock_sp => clock,
            TxD      => TxD,
            RxD      => RxD,
            -- Wishbone
            clk_i    => clock,
            rst_i    => reset,
            dat_i    => dat_cpu(7 downto 0),
            dat_o    => dat_sio,
            stb_i    => stb_sio,
            we_i     => we,
            ack_o    => ack_sio);

    ata_port : entity ATA33
        port map (
            dd       => ata_dd,
            dmarq    => ata_dmarq,
            dior     => ata_dior,
            diow     => ata_diow,
            iordy    => ata_iordy,
            dmack    => ata_dmack,
            da       => ata_da,
            cs0      => ata_cs0,
            cs1      => ata_cs1,
            dasp     => ata_dasp,
            -- Wishbone
            clk_i    => clock_bus,
            rst_i    => reset,
            adr_i    => adr(3 downto 0),
            dat_i    => dat_cpu(15 downto 0),
            dat_o    => dat_ata,
            stb_i    => stb_ata,
            we_i     => we,
            ack_o    => ack_ata);

    int_time <= '1' when timer = timer_type'high else '0';
    ireq <= button(1) & ata_intrq & int_time & int_sio;

    ack <= ack_mem or ack_sio or ack_ata;

    stb_cpu <= cyc and stb;
    stb_mem <= stb_cpu when adr(18 downto 18) = "0" else '0';
    stb_sio <= stb_cpu when adr(18 downto  0) = "1111111000000000000" else '0';
    stb_ata <= stb_cpu when adr(18 downto  4) = "111111100000001" else '0';

    dat_i <=
        (31 downto 8 => '0') & dat_sio when stb_sio = '1' else
        (31 downto 16 => '0') & dat_ata when stb_ata = '1' else
        sram_dat;

    sram_adr <= adr(17 downto 0);
    sram_ce1 <= '0';
    sram_ce2 <= '0';
    sram_ub1 <= '0';
    sram_lb1 <= '0';
    sram_ub2 <= '0';
    sram_lb2 <= '0';
    sram_oe  <= we;
    sram_dat <= dat_cpu when sram_dat_en = '1' else (sram_dat'range => 'Z');

    reset_driver : process (clock)
    begin
        if clock'event and clock = '1' then
	    if button(3) = '1' then
	        reset <= '1';
		reset_timer <= "00000000000000000000";
	    elsif reset_timer(19 downto 16) = "1111" then
	    	reset <= '0';
	    else 
	        reset <= '1';
		reset_timer <= reset_timer + 1;
	    end if;	        
	end if;
    end process reset_driver;

    hex_led_driver : process (clock)
    begin
       	if clock'event and clock = '1' then
	    hex_led_cnt <= hex_led_cnt + 1;
	    case hex_led_cnt(15 downto 14) is
	    when "00" =>
	        hex_an0 <= "1110";
		hex <= display(3 downto 0);
	    when "01" =>
	        hex_an0 <= "1101";
		hex <= display(7 downto 4);
	    when "10" =>
	        hex_an0 <= "1011";
		hex <= display(11 downto 8);
	    when others =>
	        hex_an0 <= "0111";
		hex <= display(15 downto 12);
	    end case;
	    hex_an <= hex_an0;
	    case hex is
       	    when "0001" => hex_seg <= "1111001";   --1
       	    when "0010" => hex_seg <= "0100100";   --2
       	    when "0011" => hex_seg <= "0110000";   --3
       	    when "0100" => hex_seg <= "0011001";   --4
       	    when "0101" => hex_seg <= "0010010";   --5
       	    when "0110" => hex_seg <= "0000010";   --6
       	    when "0111" => hex_seg <= "1111000";   --7
       	    when "1000" => hex_seg <= "0000000";   --8
       	    when "1001" => hex_seg <= "0010000";   --9
       	    when "1010" => hex_seg <= "0001000";   --A
       	    when "1011" => hex_seg <= "0000011";   --b
       	    when "1100" => hex_seg <= "1000110";   --C
       	    when "1101" => hex_seg <= "0100001";   --d
       	    when "1110" => hex_seg <= "0000110";   --E
       	    when "1111" => hex_seg <= "0001110";   --F
       	    when others => hex_seg <= "1000000";   --0
	    end case;
	    hex_dot <= '1';
	end if;
    end process hex_led_driver;

    process (clock)
    begin
        if clock'event and clock = '1' then
            if stb_mem = '1' and ack_mem = '0' then
                ack_mem <= '1';
            else
                ack_mem <= '0';
            end if;
        end if;
        if clock'event and clock = '0' then
            if stb_mem = '1' and ack_mem = '0' and we = '1' then
                sram_dat_en <= '1';
                sram_we  <= '0';
            else
                sram_dat_en <= '0';
                sram_we  <= '1';
            end if;
        end if;
    end process;

    process (clock)
    begin
        if clock'event and clock = '1' then
            if stb_mem = '1' then
                display <= adr(display'range);
            end if;
            if int_time = '1' then
                timer <= 0;
            else
                timer <= timer + 1;
            end if;
            if stb_cpu = '1' and stb_mem = '0' and stb_sio = '0' and stb_ata = '0' and err = '0' then
                err <= '1';
            else
                err <= '0';
            end if;
            led(0) <= halted;
            led(1) <= idle;
            led(2) <= stb;
            led(3) <= stb_mem;
            led(4) <= stb_sio;
            led(5) <= stb_ata;
            led(6) <= ireq(3) or ireq(2) or ireq(1) or ireq(0);
            led(7) <= reset;
            ata_reset <= not reset;
        end if;
    end process;

end Behaviour;