library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_textio.all;
use std.textio.all;
use work.Kronos_Types.all;

-- CacheEn Write Hit Dirty
--    0      0     0     0      Read
--    0      0     0     1      Read
--    0      0     1     0      Read
--    0      0     1     1      ????
--    0      1     0     0      Write
--    0      1     0     1      Write
--    0      1     1     0      Write
--    0      1     1     1      Write
--    1      0     0     0      Read
--    1      0     0     1      Flush, Read
--    1      0     1     0      Cache
--    1      0     1     1      Cache
--    1      1     0     0      Cache
--    1      1     0     1      Cache, Flush
--    1      1     1     0      Cache
--    1      1     1     1      Cache

entity DataCache is
    generic (
        address_size : in integer := 32);
    port (
        -- Wishbone interface
	adr_o   : out std_logic_vector(address_size - 1 downto 0);
	dat_i   : in std_logic_vector(31 downto 0);
	dat_o   : out std_logic_vector(31 downto 0);
	we_o    : out std_logic;
	cyc_o   : out std_logic;
	stb_o   : out std_logic;
        lock_o  : out std_logic;
	ack_i   : in std_logic;
	err_i   : in std_logic;
        -- Port 0 signals
	a0_bus  : in std_logic_vector(31 downto 0);
	a0_read : in std_logic;
	a0_write: in std_logic;
	a0_wtlb : in std_logic;
	a0_wbase: in std_logic;
	d0_in   : in std_logic_vector(31 downto 0);
	d0_out  : out std_logic_vector(31 downto 0);
	d0_error: out std_logic;
	d0_ready: out std_logic;
        -- Port 1 signals
	a1_bus  : in std_logic_vector(31 downto 0);
	a1_read : in std_logic;
	d1_out  : out std_logic_vector(31 downto 0);
	d1_error: out std_logic;
	d1_ready: out std_logic;
        -- Clock
        stall   : in std_logic;
	clock   : in std_logic;
	reset   : in std_logic);
end DataCache;

architecture Variant1 of DataCache is

    -- Directly mapped two port cache
    -- Write-back

    type STATE_TYPE is (IDLE, CACHE, FLUSH, WRITE, READ);

    constant page_offs_bits  : integer := 12;
    constant cache_hash_bits : integer := 9;
    constant tlb_hash_bits   : integer := 9;

    -- Offset inside a page:
    subtype va_offs is integer range page_offs_bits - 1 downto 0;
    -- Virtual page no:
    subtype va_page is integer range a0_bus'high downto page_offs_bits;
    -- Part of page offset that is not cache index:
    subtype va_df is integer range page_offs_bits - 1 downto cache_hash_bits;
    -- Virtual page hash - TLB index:
    subtype va_tlb_hash is integer range tlb_hash_bits + page_offs_bits - 1 downto page_offs_bits;
    -- Un-hashed part of virtual page no:
    subtype va_tlb_key is integer range a0_bus'high downto tlb_hash_bits + page_offs_bits;
    -- Virtual address hash - cache index:
    subtype va_cache_hash is integer range cache_hash_bits - 1 downto 0;
    -- Un-hashed part of virtual address:
    subtype va_cache_key is integer range address_size - 1 downto cache_hash_bits;

    -- Cache entry fields:
    subtype cache_key is integer range va_cache_key'high - va_cache_key'low downto 0;
    constant cache_valid : integer := cache_key'high + 1;
    constant cache_dirty : integer := cache_valid + 1;

    -- TLB entry fields:
    constant tlb_tag_re : integer := 0; -- read enable
    constant tlb_tag_we : integer := 1; -- write enable
    constant tlb_tag_ce : integer := 2; -- cache enable
    -- TLB entry field: frame, 20 bits max
    subtype tlb_frame is integer range tlb_tag_ce + address_size - va_page'low downto tlb_tag_ce + 1;
    -- TLB entry field: page key, 11 bits
    subtype tlb_key is integer range tlb_frame'high + va_tlb_key'high + 1 - va_tlb_key'low downto tlb_frame'high + 1;
    -- TLB entry field: translation table base, 20 bits max
    subtype tlb_base is integer range tlb_key'high + address_size - va_page'low downto tlb_key'high + 1;

    signal rd0_state   : STATE_TYPE;
    signal rd1_state   : STATE_TYPE;
    signal a0_write1   : std_logic;
    signal a0_stb      : std_logic;
    signal a1_stb      : std_logic;
    signal d0_in1      : std_logic_vector(31 downto 0);
    signal a0_bus1     : std_logic_vector(31 downto 0);
    signal a1_bus1     : std_logic_vector(31 downto 0);
    signal di0_data    : std_logic_vector(31 downto 0);
    signal di1_data    : std_logic_vector(31 downto 0);
    signal do0_data    : std_logic_vector(31 downto 0);
    signal do1_data    : std_logic_vector(31 downto 0);
    signal di0_keys    : std_logic_vector(31 downto 0);
    signal di1_keys    : std_logic_vector(31 downto 0);
    signal do0_keys    : std_logic_vector(31 downto 0);
    signal do1_keys    : std_logic_vector(31 downto 0);
    signal cache_hash0 : std_logic_vector(va_cache_hash);
    signal cache_hash1 : std_logic_vector(va_cache_hash);
    signal cache_en0   : std_logic;
    signal cache_en1   : std_logic;
    signal cache_hit0  : std_logic;
    signal cache_hit1  : std_logic;
    signal d0_ready_i  : std_logic;
    signal d1_ready_i  : std_logic;
    signal cache_ready0: std_logic;
    signal cache_ready1: std_logic;
    signal do0_rg_data : std_logic_vector(31 downto 0);
    signal do0_rg_error: std_logic;
    signal do1_rg_data : std_logic_vector(31 downto 0);
    signal do1_rg_error: std_logic;
    signal we0         : std_logic;
    signal we0key      : std_logic;
    signal we0dat      : std_logic;
    signal we1         : std_logic;
    signal ack_any     : std_logic;
    signal collision   : std_logic;
    signal d0_ready1   : std_logic;
    signal tlb_data0   : std_logic_vector(63 downto 0);
    signal tlb_data1   : std_logic_vector(63 downto 0);
    signal tlb_in      : std_logic_vector(63 downto 0);
    signal tlb_match0  : std_logic;
    signal tlb_match1  : std_logic;
    signal tlb_tags_ok0: std_logic;
    signal tlb_tags_ok1: std_logic;
    signal tlb_frame0  : std_logic_vector(tlb_frame);
    signal tlb_frame1  : std_logic_vector(tlb_frame);
    signal base        : std_logic_vector(address_size - 1 downto va_page'low);
    signal tlb_en      : std_logic;
    signal tlb_off     : std_logic;
    signal tlb_hit0    : std_logic;
    signal tlb_hit1    : std_logic;

begin

    -- synthesis translate_off
    print_trace : process (clock)
	variable ln : line;
    begin
       	if trace and clock'event and clock = '1' then
            if stall = '0' and (a0_read = '1' or a0_write = '1' or a1_read = '1') then
                write(ln, "DataCache:");
                write(ln, " a0_bus ");
                write_hex(ln, a0_bus);
                write(ln, ", a1_bus ");
                write_hex(ln, a1_bus);
                write(ln, ", d0_in ");
                write_hex(ln, d0_in);
                write(ln, ", a0_rd ");
                write(ln, a0_read);
                write(ln, ", a0_wr ");
                write(ln, a0_write);
                write(ln, ", a1_rd ");
                write(ln, a1_read);
                writeline(std.textio.output, ln);
            end if;
            if rd0_state = CACHE then
                write(ln, "DataCache:");
                write(ln, " rd0_state = CACHE");
                write(ln, ", hit ");
                write(ln, cache_hit0);
                write(ln, ", valid ");
                write(ln, do0_keys(cache_valid));
                write(ln, ", dirty ");
                write(ln, do0_keys(cache_dirty));
                write(ln, ", cache en ");
                write(ln, tlb_data0(tlb_tag_ce));
                write(ln, ", ready ");
                write(ln, d0_ready);
                writeline(std.textio.output, ln);
            end if;
            if rd0_state = FLUSH then
                write(ln, "DataCache:");
                write(ln, " rd0_state = FLUSH");
                write(ln, ", ready ");
                write(ln, d0_ready);
                writeline(std.textio.output, ln);
            end if;
            if rd0_state = WRITE then
                write(ln, "DataCache:");
                write(ln, " rd0_state = WRITE");
                write(ln, ", ready ");
                write(ln, d0_ready);
                writeline(std.textio.output, ln);
            end if;
            if rd0_state = READ then
                write(ln, "DataCache:");
                write(ln, " rd0_state = READ");
                write(ln, ", ready ");
                write(ln, d0_ready);
                writeline(std.textio.output, ln);
            end if;
            if rd1_state = CACHE then
                write(ln, "DataCache:");
                write(ln, " rd1_state = CACHE");
                write(ln, ", hit ");
                write(ln, cache_hit1);
                write(ln, ", valid ");
                write(ln, do1_keys(cache_valid));
                write(ln, ", dirty ");
                write(ln, do1_keys(cache_dirty));
                write(ln, ", cache en ");
                write(ln, tlb_data1(tlb_tag_ce));
                write(ln, ", ready ");
                write(ln, d1_ready);
                writeline(std.textio.output, ln);
            end if;
            if rd1_state = FLUSH then
                write(ln, "DataCache:");
                write(ln, " rd1_state = FLUSH");
                write(ln, ", ready ");
                write(ln, d1_ready);
                writeline(std.textio.output, ln);
            end if;
            if rd1_state = READ then
                write(ln, "DataCache:");
                write(ln, " rd1_state = READ");
                write(ln, ", ready ");
                write(ln, d1_ready);
                writeline(std.textio.output, ln);
            end if;
            if we0key = '1' then
                write(ln, "DataCache:");
                write(ln, " wr cache port 0 key, index ");
                write_hex(ln, cache_hash0);
                write(ln, ", key ");
                write_hex(ln, di0_keys(cache_key));
                write(ln, ", valid ");
                write(ln, di0_keys(cache_valid));
                write(ln, ", dirty ");
                write(ln, di0_keys(cache_dirty));
                writeline(std.textio.output, ln);
            end if;
            if we0dat = '1' then
                write(ln, "DataCache:");
                write(ln, " wr cache port 0 data, index ");
                write_hex(ln, cache_hash0);
                write(ln, ", data ");
                write_hex(ln, di0_data);
                writeline(std.textio.output, ln);
            end if;
            if we1 = '1' then
                write(ln, "DataCache:");
                write(ln, " wr cache port 1, index ");
                write_hex(ln, cache_hash1);
                write(ln, ", tag ");
                write_hex(ln, di1_keys(cache_key));
                write(ln, ", data ");
                write_hex(ln, di1_data);
                write(ln, ", valid ");
                write(ln, di1_keys(cache_valid));
                writeline(std.textio.output, ln);
            end if;
	end if;
    end process print_trace;
    -- synthesis translate_on

    cache_keys : entity work.BlockRam
	port map (
	    clock => clock,
	    en0   => '1',
	    en1   => '1',
	    we0   => we0key,
	    we1   => we1,
	    a0    => cache_hash0,
	    a1    => cache_hash1,
            di0   => di0_keys,
            di1   => di1_keys,
            do0   => do0_keys,
            do1   => do1_keys);

    cache_data : entity work.BlockRam
	port map (
	    clock => clock,
	    en0   => '1',
	    en1   => '1',
	    we0   => we0dat,
	    we1   => we1,
	    a0    => cache_hash0,
	    a1    => cache_hash1,
	    di0   => di0_data,
	    di1   => di1_data,
	    do0   => do0_data,
	    do1   => do1_data);

    tlb0 : entity work.BlockRam
	port map (
	    clock => clock,
	    en0   => tlb_en,
	    en1   => a1_stb,
	    we0   => a0_wtlb,
	    we1   => '0',
	    a0    => a0_bus(va_tlb_hash),
	    a1    => a1_bus(va_tlb_hash),
	    di0   => tlb_in(31 downto 0),
	    di1   => (31 downto 0 => '0'),
	    do0   => tlb_data0(31 downto 0),
	    do1   => tlb_data1(31 downto 0));

    tlb1 : entity work.BlockRam
	port map (
	    clock => clock,
	    en0   => tlb_en,
	    en1   => a1_stb,
	    we0   => a0_wtlb,
	    we1   => '0',
	    a0    => a0_bus(va_tlb_hash),
	    a1    => a1_bus(va_tlb_hash),
	    di0   => tlb_in(63 downto 32),
	    di1   => (31 downto 0 => '0'),
	    do0   => tlb_data0(63 downto 32),
	    do1   => tlb_data1(63 downto 32));

    lock_o <= '0'; -- TODO: implement 'lock_o'

    ack_any <= ack_i or err_i;

    d0_ready_i <= '1' when rd0_state = IDLE else cache_ready0 when rd0_state = CACHE else '0';
    d1_ready_i <= '1' when rd1_state = IDLE else cache_ready1 when rd1_state = CACHE else '0';
    d0_ready <= d0_ready_i;
    d1_ready <= d1_ready_i;

    tlb_off <= '1' when base = (base'range => '0') else '0';

    a0_stb <= not stall and (a0_read or a0_write);
    a1_stb <= not stall and a1_read;

    cache_ready0 <= cache_hit0 and (not a0_write1 or do0_keys(cache_dirty));
    cache_ready1 <= cache_hit1;

    cache_hash0 <= a0_bus(va_cache_hash) when d0_ready_i = '1' else a0_bus1(va_cache_hash);
    cache_hash1 <= a1_bus(va_cache_hash) when d1_ready_i = '1' else a1_bus1(va_cache_hash);

    collision <= '1' when a0_bus1(va_cache_hash) = a1_bus1(va_cache_hash) and
        (rd0_state = CACHE or d0_ready1 = '0') else '0';

    cache_en0 <= (tlb_off and not a0_bus1(31)) or (tlb_hit0 and tlb_data0(tlb_tag_ce));
    cache_en1 <= tlb_off or (tlb_hit1 and tlb_data1(tlb_tag_ce));

    cache_hit0 <= cache_en0 and do0_keys(cache_valid) when do0_keys(cache_key) = di0_keys(cache_key) else '0';
    cache_hit1 <= cache_en1 and do1_keys(cache_valid) and not collision when do1_keys(cache_key) = di1_keys(cache_key) else '0';

    we0 <= ack_any and cache_en0 when rd0_state = READ else '0';
    we1 <= ack_any and cache_en1 when rd1_state = READ else '0';

    we0dat <= we0 or (a0_write and not stall);
    we0key <= not d0_ready_i when rd0_state = CACHE else we0;

    tlb_en <= a0_stb or a0_wtlb;
    tlb_in(tlb_tag_re) <= d0_in(0);
    tlb_in(tlb_tag_we) <= d0_in(1);
    tlb_in(tlb_tag_ce) <= d0_in(2);
    tlb_in(tlb_frame)  <= d0_in(address_size - 1 downto va_page'low);
    tlb_in(tlb_key)    <= a0_bus(va_tlb_key);
    tlb_in(tlb_base)   <= base;

    tlb_match0 <= '1' when tlb_data0(tlb_base) & tlb_data0(tlb_key) = base & a0_bus1(va_tlb_key) else '0';
    tlb_match1 <= '1' when tlb_data1(tlb_base) & tlb_data1(tlb_key) = base & a1_bus1(va_tlb_key) else '0';

    tlb_tags_ok0 <= tlb_data0(tlb_tag_re) when a0_write1 = '0' else tlb_data0(tlb_tag_we);
    tlb_tags_ok1 <= tlb_data1(tlb_tag_re);

    tlb_hit0 <= tlb_off or (tlb_match0 and tlb_tags_ok0);
    tlb_hit1 <= tlb_off or (tlb_match1 and tlb_tags_ok1);

    tlb_frame0 <= a0_bus1(address_size - 1 downto page_offs_bits) when tlb_off = '1' else tlb_data0(tlb_frame);
    tlb_frame1 <= a1_bus1(address_size - 1 downto page_offs_bits) when tlb_off = '1' else tlb_data1(tlb_frame);

    di0_keys(cache_key) <= tlb_frame0 & a0_bus1(va_df);
    di1_keys(cache_key) <= tlb_frame1 & a1_bus1(va_df);
    di0_keys(cache_valid) <= ack_i when rd0_state = READ else tlb_hit0 and tlb_data0(tlb_tag_ce) and a0_write1;
    di1_keys(cache_valid) <= ack_i;
    di0_keys(cache_dirty) <= '0' when rd0_state = READ else di0_keys(cache_valid);
    di1_keys(cache_dirty) <= '0';
    di0_data <= dat_i when rd0_state = READ else d0_in;
    di1_data <= dat_i;
    d0_out <= do0_data when rd0_state = CACHE else do0_rg_data;
    d1_out <= do1_data when rd1_state = CACHE else do1_rg_data;
    d0_error <= do0_rg_error;
    d1_error <= do1_rg_error;

    process (clock)
        variable rd : boolean;
        variable wr : boolean;
        variable de_in : boolean;
        variable de_o0 : boolean;
        variable de_o1 : boolean;
        variable ae_a0 : boolean;
        variable ae_a1 : boolean;
        variable ae_o0 : boolean;
        variable ae_o1 : boolean;
    begin
	if clock'event and clock = '1' then
            if stall = '0' then
                if a0_wbase = '1' then
                    base <= d0_in(base'range);
                end if;
                if a1_read = '1' then
                    a1_bus1 <= a1_bus;
                end if;
                if a0_read = '1' or a0_write = '1' then
                    a0_bus1 <= a0_bus;
                end if;
                if a0_write = '1' then
                    d0_in1 <= d0_in;
                end if;
                a0_write1 <= a0_write;
            end if;

            rd := false;
            wr := false;
            de_in := false;
            de_o0 := false;
            de_o1 := false;
            ae_a0 := false;
            ae_a1 := false;
            ae_o0 := false;
            ae_o1 := false;
            
            d0_ready1 <= d0_ready_i;

            if reset = '1' then
                rd0_state <= IDLE;
                do0_rg_error <= '1';
            elsif rd0_state = IDLE then
                if a0_stb = '1' then
                    rd0_state <= CACHE;
                    do0_rg_error <= '0';
                end if;
            elsif rd0_state = CACHE then
                if cache_ready0 = '1' then
                    do0_rg_data <= do0_data;
                    do0_rg_error <= '0';
                    if a0_stb = '0' then
                        rd0_state <= IDLE;
                    end if;
                elsif tlb_hit0 = '0' then
                    do0_rg_error <= '1';
                    rd0_state <= IDLE;
                elsif do0_keys(cache_dirty) = '1' then
                    rd0_state <= FLUSH;
                    ae_o0 := true;
                    de_o0 := true;
                    wr := true;
                elsif a0_write1 = '1' and (tlb_hit0 = '0' or tlb_data0(tlb_tag_ce) = '1') then
                    -- only update dirty, valid, key and error
                    rd0_state <= IDLE;
                    do0_rg_error <= not tlb_hit0;
                elsif a0_write1 = '1' then
                    rd0_state <= WRITE;
                    ae_a0 := true;
                    de_in := true;
                    wr := true;
                else
                    rd0_state <= READ;
                    ae_a0 := true;
                    rd := true;
                end if;
            elsif rd0_state = FLUSH then
                if ack_any = '0' then
                    wr := true;
                elsif a0_write1 = '1' and (tlb_hit0 = '0' or tlb_data0(tlb_tag_ce) = '1') then
                    rd0_state <= IDLE;
                    do0_rg_error <= not tlb_hit0;
                elsif a0_write1 = '1' then
                    rd0_state <= WRITE;
                    ae_a0 := true;
                    de_in := true;
                    wr := true;
                else
                    rd0_state <= READ;
                    ae_a0 := true;
                    rd := true;
                end if;
            elsif rd0_state = READ then
                if ack_any = '0' then
                    rd := true;
                else
                    rd0_state <= IDLE;
                    do0_rg_data <= dat_i;
                    do0_rg_error <= err_i;
                end if;
            elsif rd0_state = WRITE then
                if ack_any = '0' then
                    wr := true;
                else
                    rd0_state <= IDLE;
                    do0_rg_error <= err_i;
                end if;
            end if;

            if reset = '1' then
                rd1_state <= IDLE;
                do1_rg_error <= '0';
            elsif rd1_state = IDLE then
                if a1_stb = '1' then
                    rd1_state <= CACHE;
                    do1_rg_error <= '0';
                end if;
            elsif rd1_state = CACHE then
                if cache_ready1 = '1' then
                    do1_rg_data <= do1_data;
                    do1_rg_error <= '0';
                    if a1_stb = '0' then
                        rd1_state <= IDLE;
                    end if;
                elsif tlb_hit1 = '0' then
                    do1_rg_error <= '1';
                    rd1_state <= IDLE;
                elsif rd or wr or collision = '1' then
                    null;
                elsif do1_keys(cache_dirty) = '1' and cache_en1 = '1' then
                    rd1_state <= FLUSH;
                    ae_o1 := true;
                    de_o1 := true;
                    wr := true;
                else
                    rd1_state <= READ;
                    ae_a1 := true;
                    rd := true;
                end if;
            elsif rd1_state = FLUSH then
                if ack_any = '0' then
                    wr := true;
                else
                    rd1_state <= READ;
                    ae_a1 := true;
                    rd := true;
                end if;
            elsif rd1_state = READ then
                if ack_any = '0' then
                    rd := true;
                else
                    rd1_state <= IDLE;
                    do1_rg_data <= dat_i;
                    do1_rg_error <= err_i;
                end if;
            end if;

            -- synthesis translate_off
            assert not (rd and wr) severity failure;
            assert not ae_a0 or rd or wr severity failure;
            assert not ae_a1 or rd severity failure;
            assert not ae_o0 or wr severity failure;
            assert not ae_o1 or wr severity failure;
            assert not (ae_a0 and ae_a1) severity failure;
            assert we0key = '0' or stall = '1' severity failure;
            assert we1 = '0' or stall = '1' severity failure;
            -- synthesis translate_on

            if ae_a0 then
                adr_o <= tlb_frame0 & a0_bus1(va_offs);
            end if;
            if ae_a1 then
                adr_o <= tlb_frame1 & a1_bus1(va_offs);
            end if;
            if ae_o0 then
                adr_o <= do0_keys(cache_key) & a0_bus1(va_cache_hash);
            end if;
            if ae_o1 then
                adr_o <= do1_keys(cache_key) & a1_bus1(va_cache_hash);
            end if;

            if de_in then
                dat_o <= d0_in1;
            end if;
            if de_o0 then
                dat_o <= do0_data;
            end if;
            if de_o1 then
                dat_o <= do1_data;
            end if;

            if rd then
                cyc_o <= '1';
                stb_o <= '1';
                we_o <= '0';
            elsif wr then
                cyc_o <= '1';
                stb_o <= '1';
                we_o <= '1';
            else
                cyc_o <= '0';
                stb_o <= '0';
                we_o <= '0';
            end if;
	end if;
    end process;

end architecture Variant1;

