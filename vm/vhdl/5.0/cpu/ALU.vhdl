library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.uCmdBits.all;

entity ALU is
    generic (bits : in integer := 32);
    port (
    	op    : in std_logic_vector(4 downto 0);
    	x_bus : in std_logic_vector(bits - 1 downto 0);
    	y_bus : in std_logic_vector(bits - 1 downto 0);
    	z_bus : out std_logic_vector(bits - 1 downto 0);
        flag_c: out std_logic;
        clock : in std_logic;
        stall : in std_logic);
end ALU;

architecture Behavioral of ALU is
    signal byte_index : std_logic_vector(1 downto 0);
    signal mul1 : std_logic_vector(bits - 1 downto 0);
    signal mul2 : std_logic_vector(bits - 1 downto 0);
    signal rot_0, rot_1, rot_2, rot_3, rot_4: std_logic_vector(bits - 1 downto 0);
    signal sh   : std_logic_vector(4 downto 0);
    signal flag_c0 : std_logic;
begin

    flag_c <= flag_c0;

    -- Barrel shifter
    sh <= y_bus(4 downto 0) when op(0) = '1' else (not y_bus(4 downto 0)) + 1;
    rot_0 <= x_bus when sh(0) = '0' else x_bus(30 downto 0) & x_bus(31);
    rot_1 <= rot_0 when sh(1) = '0' else rot_0(29 downto 0) & rot_0(31 downto 30);
    rot_2 <= rot_1 when sh(2) = '0' else rot_1(27 downto 0) & rot_1(31 downto 28);
    rot_3 <= rot_2 when sh(3) = '0' else rot_2(23 downto 0) & rot_2(31 downto 24);
    rot_4 <= rot_3 when sh(4) = '0' else rot_3(15 downto 0) & rot_3(31 downto 16);

    process (clock)
    begin
        if clock'event and clock = '1' and stall = '0' then
            if op = alu_add_xb then
                byte_index <= y_bus(1 downto 0);
            elsif op = alu_y_xb0 then
                byte_index <= "01";
            elsif (op = alu_lxb) or (op = alu_sxb) then
                byte_index <= byte_index + 1;
            end if;
        end if;
    end process;

    process (clock)
        variable z    : std_logic_vector(bits - 1 downto 0);
        variable equ  : std_logic;
        variable lss  : std_logic;
        variable c    : std_logic;
        variable tmp0 : std_logic_vector(16 downto 0);
        variable addsub: std_logic_vector(bits downto 0);
        variable x_and_y : std_logic_vector(bits - 1 downto 0);
        variable x_or_y  : std_logic_vector(bits - 1 downto 0);
    begin
        if clock'event and clock = '1' and stall = '0' then
            if op = alu_add then
                addsub := ('0' & x_bus) + ('0' & y_bus);
            else
                addsub := ('0' & x_bus) - ('0' & y_bus);
            end if;
            x_and_y := x_bus and y_bus;
            x_or_y := x_bus or y_bus;
            if addsub(bits - 1 downto 0) = (bits - 1 downto 0 => '0') then
                equ := '1';
            else
                equ := '0';
            end if;
            lss :=
                (not addsub(bits) and x_bus(bits-1) and not y_bus(bits-1)) or
                (addsub(bits) and not x_bus(bits-1) and not y_bus(bits-1)) or
                (addsub(bits) and x_bus(bits-1) and y_bus(bits-1));
            c := '0';
            case op is
            when alu_y | alu_y_xb0 => z := y_bus;
            when alu_not_x  => z := not x_bus;
            when alu_and    => z := x_and_y;
            when alu_or     => z := x_or_y;
            when alu_xor    => z := x_or_y and not x_and_y;
            when alu_add | alu_sub => z := addsub(bits - 1 downto 0); c := addsub(bits);
            when alu_bic    => z := x_bus and not y_bus;
            when alu_add_xb => z := x_bus + ("00" & y_bus(bits - 1 downto 2));
            when alu_lxb | alu_lxb_nc  => 
                z(bits - 1 downto 8) := (bits - 1 downto 8 => '0');
                case byte_index is
                when "00" =>
                    z(7 downto 0) := x_bus( 7 downto  0);
                when "01" =>
                    z(7 downto 0) := x_bus(15 downto  8);
                when "10" =>
                    z(7 downto 0) := x_bus(23 downto 16);
                when others =>
                    z(7 downto 0) := x_bus(31 downto 24);
                end case;
            when alu_sxb =>
                case byte_index is
                when "00" =>
                    z( 7 downto  0) := y_bus( 7 downto  0);
                    z(31 downto  8) := x_bus(31 downto  8);
                when "01" =>
                    z( 7 downto  0) := x_bus( 7 downto  0);
                    z(15 downto  8) := y_bus( 7 downto  0);
                    z(31 downto 16) := x_bus(31 downto 16);
                when "10" =>
                    z(15 downto  0) := x_bus(15 downto  0);
                    z(23 downto 16) := y_bus( 7 downto  0);
                    z(31 downto 24) := x_bus(31 downto 24);
                when others =>
                    z(23 downto  0) := x_bus(23 downto  0);
                    z(31 downto 24) := y_bus( 7 downto  0);
                end case;
            when alu_lss   => z := (bits - 1 downto 1 => '0') & lss;
            when alu_leq   => z := (bits - 1 downto 1 => '0') & (lss or equ);
            when alu_gtr   => z := (bits - 1 downto 1 => '0') & (not (lss or equ));
            when alu_geq   => z := (bits - 1 downto 1 => '0') & (not lss);
            when alu_equ   => z := (bits - 1 downto 1 => '0') & equ;
            when alu_neq   => z := (bits - 1 downto 1 => '0') & (not equ);
            when alu_shr   => z := '0' & x_bus(bits - 1 downto 1); c := x_bus(0);
            when alu_shrs  => z := '0' & x_bus(bits - 1 downto 2) & (x_bus(1) or x_bus(0));
            when alu_ror_xy | alu_rol_xy => z := rot_4;
            when alu_mul0  =>
                z := x_bus(15 downto  0) * y_bus(15 downto  0);
                mul1 <= x_bus(31 downto 16) * y_bus(15 downto  0);
                mul2 <= x_bus(15 downto  0) * y_bus(31 downto 16);
                if x_bus(31 downto 16) /= 0 and y_bus(31 downto 16) /= 0 then
                    c := '1';
                end if;
            when alu_mul1  =>
                tmp0 := ('0' & mul1(15 downto 0)) + ('0' & mul2(15 downto 0));
                z := tmp0(15 downto 0) & "0000000000000000";
                if flag_c0 = '1' or mul1(31 downto 16) /= 0 or mul2(31 downto 16) /= 0 or tmp0(16) = '1' then
                    c := '1';
                end if;
            when others => z := x_bus;
            end case;
            flag_c0 <= c;
            z_bus <= z;
        end if;
    end process;

end Behavioral;
