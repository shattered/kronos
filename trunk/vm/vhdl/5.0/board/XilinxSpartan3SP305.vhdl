library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
Library unisim;
use unisim.Vcomponents.all;

entity XilinxSpartan3SP305 is
    port (
        clock_in       : in std_logic;
	btn_reset      : in std_logic;
	led            : out std_logic_vector(3 downto 0);
        -- SRAM and FLASH
        sram_flash_a   : out std_logic_vector(22 downto 0);
        sram_flash_d   : inout std_logic_vector(31 downto 0) bus;
        flash_a0       : out std_logic;
        flash_a23      : out std_logic;
        flash_ce2      : out std_logic;
        flash_oe_n     : out std_logic;
        flash_byte_n   : out std_logic;
        flash_reset_n  : out std_logic;
        sram_bw        : out std_logic_vector(3 downto 0);
        sram_avd_ld_n  : out std_logic;
        sram_ce1_n     : out std_logic;
        sram_flash_we_n: out std_logic;
        sram_oe_n      : out std_logic;
        sram_zz        : out std_logic;
        sram_mode      : out std_logic;
        sram_dqp       : inout std_logic_vector(3 downto 0) bus;
        sram_clk       : out std_logic;
        sram_clk_fb    : in std_logic;
        -- 64MB DDR SDRAM
        dram_ck1_p     : out std_logic;
        dram_ck1_n     : out std_logic;
        dram_ck1_p_fb  : in std_logic;
        dram_ras_n     : out std_logic;
        dram_cas_n     : out std_logic;
        dram_cs_n      : out std_logic;
        dram_cke       : out std_logic;
        dram_we_n      : out std_logic;
        dram_loop      : in std_logic_vector(1 downto 0);
        dram_a         : out std_logic_vector(12 downto 0);
        dram_ba        : out std_logic_vector(1 downto 0);
        dram_d         : inout std_logic_vector(31 downto 0);
        dram_dqs       : inout std_logic_vector(3 downto 0);
        dram_dm        : out std_logic_vector(3 downto 0);
        -- USB, CY7C67300
--        usb_d          : inout std_logic_vector(15 downto 0);
--        usb_a          : out std_logic_vector(1 downto 0);
--        usb_cs_n       : out std_logic;
--        usb_wr_n       : out std_logic;
--        usb_rd_n       : out std_logic;
--        usb_reset      : out std_logic;
--        usb_int        : in std_logic;
        -- VGA
        vga_r          : out std_logic_vector(7 downto 0);
        vga_g          : out std_logic_vector(7 downto 0);
        vga_b          : out std_logic_vector(7 downto 0);
        vga_blank_n    : out std_logic;
        vga_psave_n    : out std_logic;
        vga_sync_n     : out std_logic;
        vga_vsync      : out std_logic;
        vga_hsync      : out std_logic;
        vga_clk        : out std_logic;
        -- Serial port
        TxD            : out std_logic;
        RxD            : in std_logic);
end XilinxSpartan3SP305;

architecture Behaviour of XilinxSpartan3SP305 is

    constant CLOCK_RATE   : integer := 40; -- MHz, valid values: 50, 40, 30

    signal clk_2x       : std_logic;
    signal clk_2x90     : std_logic;
    signal clk_2x180    : std_logic;
    signal clk_2x270    : std_logic;
    signal clk_cpu      : std_logic;
    signal clk_2x_ext   : std_logic;
    signal clk_2x180_ext: std_logic;

    signal clk_2xFX     : std_logic;
    signal clk_in_ibufg : std_logic;
    signal clk_2xFX_fb  : std_logic;
    signal clk_2xFX_bf  : std_logic;
    signal clk_2x_bf    : std_logic;
    signal clk_2x90_bf  : std_logic;
    signal clk_2x180_bf : std_logic;
    signal clk_2x270_bf : std_logic;
    signal clk_cpu_bf   : std_logic;

    signal dcm_reset   : std_logic;
    signal dcm_reset1  : std_logic;
    signal reset       : std_logic;
    signal reset_timer : std_logic_vector(19 downto 0);
    signal sram_clk_ibf: std_logic;
    signal main_dcm_ok : std_logic;
    signal dcm0_locked : std_logic;
    signal dcm1_locked : std_logic;
    signal dcm2_locked : std_logic;
    signal ireq        : std_logic_vector(3 downto 0);
    signal adr         : std_logic_vector(24 downto 0);
    signal dat_i       : std_logic_vector(31 downto 0);
    signal dat_cpu     : std_logic_vector(31 downto 0);
    signal dat_sio     : std_logic_vector(7 downto 0);
    signal dat_mem     : std_logic_vector(31 downto 0);
    signal we          : std_logic;
    signal cyc         : std_logic;
    signal stb         : std_logic;
    signal ack         : std_logic;
    signal err         : std_logic;
    signal lock        : std_logic;
    signal stb_cpu     : std_logic;
    signal stb_ddr     : std_logic;
    signal stb_mem     : std_logic;
    signal stb_sio     : std_logic;
    signal stb_flash   : std_logic;
    signal ack_ddr     : std_logic;
    signal ack_ddr0    : std_logic;
    signal ack_mem     : std_logic;
    signal ack_mem0    : std_logic;
    signal ack_sio     : std_logic;
    signal ack_flash   : std_logic;
    signal int_sio     : std_logic;
    signal halted      : std_logic;
    signal idle        : std_logic;

    subtype timer_type is integer range 0 to CLOCK_RATE * 20000 - 1;
    signal timer       : timer_type;
    signal int_time    : std_logic;

    subtype cache_tag_address is integer range 0 to 2**13 - 1;
    type cache_tags_array is array (cache_tag_address) of std_logic_vector(7 downto 0);
    signal cache_tags: cache_tags_array := (cache_tags_array'range => "00000000");

    subtype sram_state_type is integer range 0 to 3;
    signal sram_state : sram_state_type;

    type ddr_operation is (OP_IDLE, OP_REFRESH, OP_READ, OP_WRITE);
    type ddr_stage is record
        op  : ddr_operation;
        adr : std_logic_vector(10 downto 0);
        vga : std_logic;
    end record ddr_stage;
    type ddr_state_array is array (0 to 8) of ddr_stage;
    signal ddr_state     : ddr_state_array;
    signal ddr_bank_busy : std_logic_vector(3 downto 0);
    subtype ddr_ref_timer_type is integer range 0 to 7 * CLOCK_RATE - 1;
    signal ddr_ref_timer : ddr_ref_timer_type;
    signal ddr_ini_state : integer range 0 to 255;
    signal ddr_ref_reqs  : integer range 0 to 7;
    signal ddr_state_rd0 : std_logic;
    signal ddr_state_rd1 : std_logic;
    signal ddr_state_wr  : std_logic;
    signal ddr_state_wra : std_logic_vector(1 downto 0);

    signal dat_ddr0      : std_logic_vector(31 downto 0);
    signal dat_ddr1      : std_logic_vector(31 downto 0);
    signal dat_ddr2      : std_logic_vector(31 downto 0);
    signal dat_ddr3      : std_logic_vector(31 downto 0);
    signal dat_ddr_cpu   : std_logic_vector(31 downto 0);

    signal ddr_out_en    : std_logic;
    signal ddr_dqs       : std_logic_vector(3 downto 0);
    signal ddr_out       : std_logic_vector(31 downto 0);
    signal ddr_dm0       : std_logic;
    signal ddr_dm1       : std_logic;
    signal ddr_dm2       : std_logic;
    signal ddr_dm3       : std_logic;

    signal vgamem_dat    : std_logic_vector(95 downto 0);
    signal vgamem_ack    : std_logic;
    signal vgamem_ack0   : std_logic;
    signal vgamem_vsync  : std_logic;
    signal vgamem_req0   : std_logic;
    signal vgamem_req1   : std_logic;
    signal adr_vga       : std_logic_vector(19 downto 0);

begin

    clk_in_ibufg_inst : IBUFG port map (i => clock_in, o => clk_in_ibufg);

    dcm_reset_gen : SRL16
        generic map (
            INIT => X"00FF")
        port map (
            Q => dcm_reset,
            A0 => '1',
            A1 => '1',
            A2 => '1',
            A3 => '1',
            CLK => clk_in_ibufg,
            D => '0');

    dcm_fx_clock : DCM
        generic map (
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 5,
            CLKFX_MULTIPLY => CLOCK_RATE / 10,
            CLKIN_DIVIDE_BY_2 => false,
            CLKIN_PERIOD => 10.0,
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
            CLKFB    => clk_2xFX_fb,
            RST      => dcm_reset,
            PSEN     => '0',
            PSINCDEC => '0',
            PSCLK    => '0',
            DSSEN    => '0',
            CLK0     => clk_2xFX_fb,
            CLKFX    => clk_2xFX_bf,
            LOCKED   => main_dcm_ok);

    clk_2xFX_bufg : BUFG port map (i => clk_2xFX_bf, o => clk_2xFX);

    dcm_reset1 <= not main_dcm_ok;

    dcm0_sram_clock : DCM
        generic map (
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 1,
            CLKFX_MULTIPLY => 2,
            CLKIN_DIVIDE_BY_2 => false,
            CLKIN_PERIOD => 500.0 / real(CLOCK_RATE),
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
            CLKIN    => clk_2xFX,
            CLKFB    => sram_clk_ibf,
            RST      => dcm_reset1,
            PSEN     => '0',
            PSINCDEC => '0',
            PSCLK    => '0',
            DSSEN    => '0',
            CLK0     => clk_2x_ext,
            CLK180   => clk_2x180_ext,
            LOCKED   => dcm0_locked);

    sram_clk_inp : IBUFG port map (i => dram_ck1_p_fb, o => sram_clk_ibf);

    dcm1_sram_clock : DCM
        generic map (
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 1,
            CLKFX_MULTIPLY => 2,
            CLKIN_DIVIDE_BY_2 => false,
            CLKIN_PERIOD => 500.0 / real(CLOCK_RATE),
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
            CLKIN    => clk_2xFX,
            CLKFB    => clk_2x,
            RST      => dcm_reset1,
            PSEN     => '0',
            PSINCDEC => '0',
            PSCLK    => '0',
            DSSEN    => '0',
            CLK0     => clk_2x_bf,
            CLK90    => clk_2x90_bf,
            CLK180   => clk_2x180_bf,
            CLK270   => clk_2x270_bf,
            LOCKED   => dcm1_locked);

    dcm2_sram_clock : DCM
        generic map (
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 1,
            CLKFX_MULTIPLY => 2,
            CLKIN_DIVIDE_BY_2 => true,
            CLKIN_PERIOD => 500.0 / real(CLOCK_RATE),
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
            CLKIN    => clk_2xFX,
            CLKFB    => clk_cpu,
            RST      => dcm_reset1,
            PSEN     => '0',
            PSINCDEC => '0',
            PSCLK    => '0',
            DSSEN    => '0',
            CLK0     => clk_cpu_bf,
            LOCKED   => dcm2_locked);

    clk_2x_bufg    : BUFG port map (i => clk_2x_bf, o => clk_2x);    
    clk_2x90_bufg  : BUFG port map (i => clk_2x90_bf, o => clk_2x90);    
    clk_2x180_bufg : BUFG port map (i => clk_2x180_bf, o => clk_2x180);    
    clk_2x270_bufg : BUFG port map (i => clk_2x270_bf, o => clk_2x270);    
    clk_cpu_bufg   : BUFG port map (i => clk_cpu_bf, o => clk_cpu);    

    dram_ck1_p <= clk_2x_ext;
    dram_ck1_n <= clk_2x180_ext;
    sram_clk <= clk_2x_ext;

    cpu : entity Kronos 
        generic map (
            address_size => 25)
        port map (
            rs_mode  => "00",
            halt     => '0',
	    halted   => halted,
            idle     => idle,
            ireq     => ireq,
            -- Wishbone
            clk_i    => clk_cpu,
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
  	    clock_sp => clk_cpu,
            TxD      => TxD,
            RxD      => RxD,
            -- Wishbone
            clk_i    => clk_cpu,
            rst_i    => reset,
            dat_i    => dat_cpu(7 downto 0),
            dat_o    => dat_sio,
            stb_i    => stb_sio,
            we_i     => we,
            ack_o    => ack_sio);

    vga : entity ADV7125
        port map (
            -- VGA
            vga_r       => vga_r,
            vga_g       => vga_g,
            vga_b       => vga_b,
            vga_blank_n => vga_blank_n,
            vga_psave_n => vga_psave_n,
            vga_sync_n  => vga_sync_n,
            vga_vsync   => vga_vsync,
            vga_hsync   => vga_hsync,
            vga_clk     => vga_clk,
            -- Memory
            reset       => reset,
            clk         => clk_cpu,
            ack         => vgamem_ack,
            vsync       => vgamem_vsync,
            dat         => vgamem_dat,
            req0        => vgamem_req0, 
            req1        => vgamem_req1);

    int_time <= '1' when timer = timer_type'high else '0';
    ireq <= "00" & int_time & int_sio;

    ack <= ack_ddr or ack_mem or ack_sio or ack_flash;

    stb_cpu <= cyc and stb;
    stb_ddr <= stb_cpu when adr(24) = '0' else '0';
    stb_mem <= stb_cpu when adr(24 downto 18) = "1000000" else '0';
    stb_flash <= stb_cpu when adr(24 downto 21) = "1001" else '0';
    stb_sio <= stb_cpu when adr(24 downto  0) = "1111111111111000000000000" else '0';

    dat_i <=
        (31 downto 8 => '0') & dat_sio when stb_sio = '1' else
        dat_ddr_cpu when stb_ddr = '1' else
        dat_mem;

    sram_flash_d <= dat_cpu when we = '1' else (sram_flash_d'range => 'Z');

    sram_dqp <= "0000" when we = '1' else (sram_dqp'range => 'Z');
    sram_oe_n <= we;
    sram_bw <= "0000";
    sram_avd_ld_n <= '0';
    sram_zz <= '0';
    sram_mode <= '0';

    flash_a0 <= '0';
    flash_a23 <= '0';
    flash_ce2 <= stb_flash;
    flash_oe_n <= not (stb_flash and not we);
    flash_byte_n <= '1';
    flash_reset_n <= not reset;

    dram_cs_n <= '0';
    dram_dqs <= (dram_dqs'range => 'Z') when ddr_out_en = '0' else ddr_dqs;
    dram_d <= (dram_d'range => 'Z') when ddr_out_en = '0' else ddr_out;

    reset_driver : process (clk_cpu)
    begin
        if clk_cpu'event and clk_cpu = '1' then
	    if btn_reset = '0' or dcm_reset1 = '1' or dcm0_locked = '0' or dcm1_locked = '0' or dcm2_locked = '0' then
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

    process (clk_cpu)
    begin
        if clk_cpu'event and clk_cpu = '1' then
            if int_time = '1' then
                timer <= 0;
            else
                timer <= timer + 1;
            end if;
            if stb_cpu = '1' and stb_ddr = '0' and stb_mem = '0' and stb_sio = '0' and stb_flash = '0' and err = '0' then
                err <= '1';
            else
                err <= '0';
            end if;
            if stb_flash = '1' and ack_flash = '0' then
                ack_flash <= '1';
            else
                ack_flash <= '0';
            end if;
            ack_ddr <= ack_ddr0;
            ack_mem <= ack_mem0;
            vgamem_ack <= vgamem_ack0;
            led(0) <= halted;
            led(1) <= idle;
            led(2) <= stb_ddr;
            led(3) <= stb_sio;
        end if;
    end process;

    process (clk_2x)
        variable next_state : sram_state_type;
        variable ce : std_logic;
    begin
        if clk_2x'event and clk_2x = '1' then
            if ack_mem = '1' then
                ack_mem0 <= '0';
            end if;
            ce := '0';
            if reset = '1' then
                sram_state <= 0;
            else
                next_state := sram_state + 1;
                case sram_state is
                when 0 =>
                    if stb_mem = '1' and ack_mem0 = '0' and ack_mem = '0' then
                        ce := '1';
                        sram_flash_a <= adr(sram_flash_a'range);
                        sram_flash_we_n <= not we;
                        sram_state <= next_state;
                    end if;
                when 1 =>
                    sram_state <= next_state;
                when 2 =>
                    ack_mem0 <= '1';
                    sram_state <= next_state;
                when 3 =>
                    dat_mem <= sram_flash_d;
                    sram_state <= 0;
                end case;
            end if;
            sram_ce1_n <= not ce;
        end if;
    end process;

    ddr_data_out: for i in dram_d'range generate
    begin
        ddr_data_out_bit : FDDRRSE 
        port map (
            Q => ddr_out(i), 
            C0 => clk_2x270, 
            C1 => clk_2x90, 
            CE => '1', 
            D0 => dat_cpu(i), 
            D1 => dat_cpu(i), 
            R => '0', 
            S => '0'); 
    end generate ddr_data_out;

    ddr_dm_out: for i in dram_dm'range generate
    begin
        ddr_dm_out_bit : FDDRRSE 
        port map (
            Q => dram_dm(i), 
            C0 => clk_2x270, 
            C1 => clk_2x90, 
            CE => '1', 
            D0 => ddr_dm0, 
            D1 => ddr_dm1, 
            R => '0', 
            S => '0'); 
    end generate ddr_dm_out;

    ddr_dqs_out: for i in dram_dqs'range generate
    begin
        ddr_dqs_out_bit : FDDRRSE 
        port map (
            Q => ddr_dqs(i), 
            C0 => clk_2x, 
            C1 => clk_2x180, 
            CE => '1', 
            D0 => '1', 
            D1 => '0', 
            R => '0', 
            S => '0'); 
    end generate ddr_dqs_out;

    process (clk_2x)
    begin
        if clk_2x'event and clk_2x = '1' then
            if ddr_state(5).op = OP_READ then
                ddr_state_rd0 <= '1';
            else
                ddr_state_rd0 <= '0';
            end if;
            ddr_state_rd1 <= ddr_state_rd0;
            if ddr_state_rd0 = '1' then
                dat_ddr1 <= dram_d;
            end if;
            if ddr_state_rd1 = '1' then
                dat_ddr3 <= dram_d;
            end if;
        end if;
    end process;

    process (clk_2x90)
    begin
        if clk_2x90'event and clk_2x90 = '1' then
            if ddr_state_wr = '1' then
                ddr_dm0 <= ddr_state_wra(1) or ddr_state_wra(0);
                ddr_dm1 <= ddr_state_wra(1) or not ddr_state_wra(0);
                ddr_dm2 <= not ddr_state_wra(1) or ddr_state_wra(0);
                ddr_dm3 <= not ddr_state_wra(1) or not ddr_state_wra(0);
            else
                ddr_dm0 <= ddr_dm2;
                ddr_dm1 <= ddr_dm3;
                ddr_dm2 <= '1';
                ddr_dm3 <= '1';
            end if;
        end if;
    end process;

    process (clk_2x180)
    begin
        if clk_2x180'event and clk_2x180 = '1' then
            if ddr_state_rd0 = '1' then
                dat_ddr0 <= dram_d;
            end if;
            if ddr_state_rd1 = '1' then
                dat_ddr2 <= dram_d;
            end if;
            if ddr_state(8).op = OP_READ then
                if ddr_state(8).vga = '0' then
                    if ddr_state(8).adr(1 downto 0) = "00" then
                        dat_ddr_cpu <= dat_ddr0;
                    elsif ddr_state(8).adr(1 downto 0) = "01" then
                        dat_ddr_cpu <= dat_ddr1;
                    elsif ddr_state(8).adr(1 downto 0) = "10" then
                        dat_ddr_cpu <= dat_ddr2;
                    else
                        dat_ddr_cpu <= dat_ddr3;
                    end if;
                else
                    vgamem_dat(23 downto  0) <= dat_ddr0(23 downto 0);
                    vgamem_dat(47 downto 24) <= dat_ddr1(23 downto 0);
                    vgamem_dat(71 downto 48) <= dat_ddr2(23 downto 0);
                    vgamem_dat(95 downto 72) <= dat_ddr3(23 downto 0);
                end if;
            end if;
        end if;
    end process;

    process (clk_2x270)
    begin
        if clk_2x270'event and clk_2x270 = '1' then
            if ddr_state(3).op = OP_WRITE then
                ddr_state_wr <= '1';
                ddr_state_wra <= ddr_state(3).adr(1 downto 0);
            else
                ddr_state_wr <= '0';
            end if;
        end if;
    end process;

    ddr_sdram_refresh_timer : process (clk_2x)
        variable ref_inc, ref_dec : boolean;
    begin
        if clk_2x'event and clk_2x = '1' then
            ref_inc := ddr_ref_timer = ddr_ref_timer_type'high;
            ref_dec := ddr_state(0).op = OP_REFRESH;
            if reset = '1' then
                ddr_ref_timer <= 0;
            elsif ref_inc then
                ddr_ref_timer <= 0;
            else
                ddr_ref_timer <= ddr_ref_timer + 1;
            end if;
            if reset = '1' then
                ddr_ref_reqs <= 0;
            elsif ref_inc and ref_dec then
                null;
            elsif ref_inc then
                ddr_ref_reqs <= ddr_ref_reqs + 1;
            elsif ref_dec then
                ddr_ref_reqs <= ddr_ref_reqs - 1;
            end if;
        end if;
    end process ddr_sdram_refresh_timer;

    ddr_sdram_controller : process (clk_2x)
        constant DDR_NOP   : std_logic_vector(2 downto 0) := "111";
        constant DDR_ACT   : std_logic_vector(2 downto 0) := "011";
        constant DDR_READ  : std_logic_vector(2 downto 0) := "101";
        constant DDR_WRITE : std_logic_vector(2 downto 0) := "100";
        constant DDR_TERM  : std_logic_vector(2 downto 0) := "110";
        constant DDR_PRE   : std_logic_vector(2 downto 0) := "010";
        constant DDR_AR    : std_logic_vector(2 downto 0) := "001";
        constant DDR_LMR   : std_logic_vector(2 downto 0) := "000";

        -- More registers values for Micron MT 46V16M16-6T parts, burst length = 4
        constant LMR_CODE1 : std_logic_vector(12 downto 0) := "0000000000000";
        constant LMR_CODE2 : std_logic_vector(12 downto 0) := "0000100100010";
        constant LMR_CODE3 : std_logic_vector(12 downto 0) := "0000000100010";

        variable cmd : std_logic_vector(2 downto 0); -- RAS & CAS & WE
        variable bank_busy, bank_free : std_logic_vector(3 downto 0);
        variable rd_ok, wr_ok : boolean;
    begin
        if clk_2x'event and clk_2x = '1' then
            if ack_ddr = '1' then
                ack_ddr0 <= '0';
            end if;
            if vgamem_ack = '1' then
                vgamem_ack0 <= '0';
            end if;
            if vgamem_vsync = '1' then
                adr_vga <= (adr_vga'range => '0');
            end if;
            ddr_state(1) <= ddr_state(0);
            ddr_state(2) <= ddr_state(1);
            ddr_state(3) <= ddr_state(2);
            ddr_state(4) <= ddr_state(3);
            ddr_state(5) <= ddr_state(4);
            ddr_state(6) <= ddr_state(5);
            ddr_state(7) <= ddr_state(6);
            ddr_state(8) <= ddr_state(7);
            cmd := DDR_NOP;
            if reset = '1' then
                dram_cke <= '0';
                ddr_ini_state <= 0;
                ddr_state(0).op <= OP_IDLE;
                ddr_state(0).adr <= "00000000000";
                ddr_state(0).vga <= '0';
                ddr_bank_busy <= "0000";
                ddr_out_en <= '0';
                dram_a <= (dram_a'range => '0');
                dram_ba <= "00";
            elsif ddr_ini_state /= 255 then
                ddr_ini_state <= ddr_ini_state + 1;
                case ddr_ini_state is
                when 0 => 
                    dram_cke <= '1';
                when 1 =>
                    cmd := DDR_PRE;
                    dram_a <= (dram_a'range => '1');
                when 3 =>
                    cmd := DDR_LMR;
                    dram_a <= LMR_CODE1;
                    dram_ba <= "01";
                when 5 =>
                    cmd := DDR_LMR;
                    dram_a <= LMR_CODE2;
                    dram_ba <= "00";
                when 7 =>
                    cmd := DDR_PRE;
                    dram_a <= (dram_a'range => '1');
                when 9 =>
                    cmd := DDR_AR;
                when 17 =>
                    cmd := DDR_AR;
                when 25 =>
                    cmd := DDR_LMR;
                    dram_a <= LMR_CODE3;
                    dram_ba <= "00";
                when others =>
                    null;
                end case;
            else
                bank_busy := "0000";
                rd_ok := ddr_ref_reqs < 6 and ddr_state(0).op = OP_IDLE and
                    ddr_state(1).op /= OP_WRITE and ddr_state(3).op /= OP_WRITE;
                wr_ok := ddr_ref_reqs < 6 and ddr_state(0).op = OP_IDLE and
                    ddr_state(1).op /= OP_READ;
                if ddr_state(2).op = OP_READ or ddr_state(2).op = OP_WRITE then
                    if ddr_state(2).op = OP_READ then
                        cmd := DDR_READ;
                    else
                        cmd := DDR_WRITE;
                    end if;
                    dram_ba <= ddr_state(2).adr(3 downto 2);
                    dram_a <= "0010" & ddr_state(2).adr(10 downto 4) & "00";
                    ddr_state(0).op <= OP_IDLE;
                elsif stb_ddr = '1' and ack_ddr0 = '0' and ack_ddr = '0' and vgamem_req1 = '0' and
                        (rd_ok or we = '1') and (wr_ok or we = '0') and
                        ddr_bank_busy(conv_integer(unsigned(adr(3 downto 2)))) = '0' then
                    cmd := DDR_ACT;
                    bank_busy(conv_integer(unsigned(adr(3 downto 2)))) := '1';
                    dram_a <= adr(23 downto 11);
                    dram_ba <= adr(3 downto 2);
                    if we = '0' then
                        ddr_state(0).op <= OP_READ;
                    else
                        ddr_state(0).op <= OP_WRITE;
                    end if;
                    ddr_state(0).adr <= adr(10 downto 0);
                    ddr_state(0).vga <= '0';
                elsif vgamem_req0 = '1' and rd_ok and (ddr_state(1).op /= OP_READ or ddr_state(1).vga = '0') and
                        ddr_bank_busy(conv_integer(unsigned(adr_vga(3 downto 2)))) = '0' then
                    cmd := DDR_ACT;
                    bank_busy(conv_integer(unsigned(adr_vga(3 downto 2)))) := '1';
                    dram_a <= "0000" & adr_vga(19 downto 11);
                    dram_ba <= adr_vga(3 downto 2);
                    ddr_state(0).op <= OP_READ;
                    ddr_state(0).adr <= adr_vga(10 downto 0);
                    ddr_state(0).vga <= '1';
                    adr_vga <= adr_vga + 4;
                elsif ddr_ref_reqs /= 0 and ddr_bank_busy = "0000" then
                    cmd := DDR_AR;
                    bank_busy := "1111";
                    ddr_state(0).op <= OP_REFRESH;
                else
                    ddr_state(0).op <= OP_IDLE;
                end if;
                if ddr_state(3).op = OP_WRITE or ddr_state(4).op = OP_WRITE or ddr_state(5).op = OP_WRITE then
                    ddr_out_en <= '1';
                else
                    ddr_out_en <= '0';
                end if;
                if ddr_state(3).op = OP_WRITE then
                    ack_ddr0 <= '1';
                end if;
                if ddr_state(6).op = OP_READ then
                    if ddr_state(6).vga = '0' then
                        ack_ddr0 <= '1';
                    else
                        vgamem_ack0 <= '1';
                    end if;
                end if;
                bank_free := "0000";
                if ddr_state(7).op = OP_REFRESH then
                    bank_free := "1111";
                end if;
                if ddr_state(7).op = OP_READ then
                    bank_free(conv_integer(unsigned(ddr_state(7).adr(3 downto 2)))) := '1';
                end if;
                if ddr_state(8).op = OP_WRITE then
                    bank_free(conv_integer(unsigned(ddr_state(8).adr(3 downto 2)))) := '1';
                end if;
                ddr_bank_busy <= (ddr_bank_busy and not bank_free) or bank_busy;
            end if;
            dram_ras_n <= cmd(2);
            dram_cas_n <= cmd(1);
            dram_we_n <= cmd(0);
        end if;
    end process ddr_sdram_controller;

end Behaviour;