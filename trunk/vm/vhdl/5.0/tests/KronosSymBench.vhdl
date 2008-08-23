library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use work.Kronos_Types.all;

entity KronosSymBench is
end KronosSymBench;

architecture Behaviour of KronosSymBench is

    signal clock     : std_logic;
    signal reset     : std_logic := '1';
    signal halted    : std_logic;

    signal TxD       : std_logic;
    signal RxD       : std_logic;

    signal ata_dd    : std_logic_vector(15 downto 0);
    signal ata_dmarq : std_logic;
    signal ata_dior  : std_logic;
    signal ata_diow  : std_logic;
    signal ata_iordy : std_logic;
    signal ata_dmack : std_logic;
    signal ata_intrq : std_logic;
    signal ata_da    : std_logic_vector(2 downto 0);
    signal ata_cs0   : std_logic;
    signal ata_cs1   : std_logic;
    signal ata_dasp  : std_logic;

    signal ireq      : std_logic_vector(3 downto 0);
    signal adr       : std_logic_vector(19 downto 0);
    signal dat_i     : std_logic_vector(31 downto 0);
    signal dat_cpu   : std_logic_vector(31 downto 0);
    signal dat_ata   : std_logic_vector(15 downto 0);
    signal dat_sio   : std_logic_vector(7 downto 0);
    signal dat_mem   : std_logic_vector(31 downto 0);
    signal we        : std_logic;
    signal cyc       : std_logic;
    signal stb       : std_logic;
    signal ack       : std_logic;
    signal err       : std_logic;
    signal lock      : std_logic;
    signal stb_mem   : std_logic;
    signal stb_sio   : std_logic;
    signal stb_ata   : std_logic;
    signal ack_mem   : std_logic := '0';
    signal ack_sio   : std_logic := '0';
    signal ack_ata   : std_logic := '0';
    signal err_mem   : std_logic := '0';
    signal int_sio   : std_logic;
    signal int_ata   : std_logic;

    constant unit_delay : Time := 2 ns;
    constant clock_period : Time := unit_delay * 50;

begin
    reset_driver :
	reset <= '1' after unit_delay, '0' after clock_period * 4 + unit_delay;

    RxD <= '1';
    ack <= ack_mem or ack_sio or ack_ata;
    err <= cyc and stb and not (stb_sio or stb_ata or stb_mem);

    dat_i <=
        (31 downto 8 => '0') & dat_sio when stb_sio = '1' else
        (31 downto 16 => '0') & dat_ata when stb_ata = '1' else
        dat_mem;

    stb_sio <= cyc and stb when adr = "11111111000000000000" else '0';
    stb_ata <= cyc and stb when adr(19 downto 4) = "1111111100000001" else '0';
    stb_mem <= cyc and stb when adr(19 downto 16) = "0000" else '0';

    clock_driver : process
	variable cnt : natural := 0;
	variable ln : line;
    begin
	if halted /= '1' then
	    clock <= '0' after clock_period / 2, '1' after clock_period;
	    cnt := cnt + 1;
            if cnt mod 100000 = 0 then
	        write(ln, "Simulated ");
  	        write(ln, cnt);
	        write(ln, " clock cycles");
	        writeline(std.textio.output, ln);
            end if;
        else
	    write(ln, "Processor halted after ");
	    write(ln, cnt);
	    write(ln, " clock cycles");
	    writeline(std.textio.output, ln);
	    wait until reset = '1';
	end if;
	wait for clock_period;
    end process clock_driver;

    cpu : entity work.Kronos
        generic map (
            address_size => 20)
        port map (
            rs_mode  => "01",
            halt     => '0',
	    halted   => halted,
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

    sio_port : entity work.SerialPort
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

    ata_port : entity work.ATA33
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
            clk_i    => clock,
            rst_i    => reset,
            adr_i    => adr(3 downto 0),
            dat_i    => dat_cpu(15 downto 0),
            dat_o    => dat_ata,
            stb_i    => stb_ata,
            we_i     => we,
            ack_o    => ack_ata);

    ram : process (clock)
	type memory_array is array (integer range 0 to 16#FFFF#) of std_logic_vector(31 downto 0);
	variable memory : memory_array;
	variable addr   : natural;
	variable ln_mem : line;
	variable inited : boolean := false;

	procedure load_ini_file is
	    variable buf : std_logic_vector(63 downto 0);
	    variable buf_pos : natural;
	    variable ch : character;
	    variable addr : natural;
	    variable ln_ini : line;
	    file ini_file : text open read_mode is "krest.ini" ; 
	begin
	    addr := memory'low;
	    while not endfile(ini_file) loop 
		readline(ini_file, ln_ini);
		buf_pos := 64;
		buf := (buf'range => '0');
		while ln_ini'length > 0 loop
		    read(ln_ini, ch);
		    exit when ch = '#';
		    if ch = ' ' then
			-- nothing
		    elsif ch >= '0' and ch <= '9' then
			buf_pos := buf_pos - 4;
			buf(buf_pos + 3 downto buf_pos) := conv_std_logic_vector(character'pos(ch) - character'pos('0'), 4);
		    elsif ch >= 'a' and ch <= 'f' then
			buf_pos := buf_pos - 4;
			buf(buf_pos + 3 downto buf_pos) := conv_std_logic_vector(character'pos(ch) - character'pos('a') + 10, 4);
		    else
			assert false severity failure;
		    end if;
		    if buf_pos = 0 then
			memory(addr) := buf(31 downto 0);
			addr := addr + 1;
			memory(addr) := buf(63 downto 32);
			addr := addr + 1;
			buf_pos := 64;
		    end if;
		end loop;
		assert buf_pos = 64 severity failure;
		deallocate(ln_ini);
	    end loop;
	    while addr < memory'high loop
		memory(addr) := (31 downto 0 => '0');
		addr := addr + 1;
	    end loop;
	    inited := true;
	end load_ini_file;

    begin
        if clock'event and clock = '1' then
	    if not inited then
	        load_ini_file;
	    end if;
            addr := conv_integer(unsigned(adr(15 downto 0)));
            if stb_mem = '1' and ack_mem = '0' then
		assert stb_sio = '0' severity failure;
		assert stb_ata = '0' severity failure;
		assert ack_sio = '0' severity failure;
		assert ack_ata = '0' severity failure;
                ack_mem <= '1';
                if we = '1' then
                    memory(addr) := dat_cpu;
                end if;
                dat_mem <= memory(addr) after unit_delay;
                if trace and we = '1' then
	            write(ln_mem, "Memory: wr, addr ");
	            write_hex(ln_mem, adr);
	            write(ln_mem, ", data ");
	            write_hex(ln_mem, dat_cpu);
	            writeline(std.textio.output, ln_mem);
                end if;
                if trace and we = '0' then
	            write(ln_mem, "Memory: rd, addr ");
	            write_hex(ln_mem, adr);
	            write(ln_mem, ", data ");
	            write_hex(ln_mem, memory(addr));
	            writeline(std.textio.output, ln_mem);
                end if;            
            elsif ack_mem = '1' then
		assert cyc = '1' severity failure;
		assert stb_mem = '1' severity failure;
		assert stb_sio = '0' severity failure;
		assert stb_ata = '0' severity failure;
		assert ack_sio = '0' severity failure;
		assert ack_ata = '0' severity failure;
                ack_mem <= '0';
            end if;
        end if;
    end process ram;

end Behaviour;
