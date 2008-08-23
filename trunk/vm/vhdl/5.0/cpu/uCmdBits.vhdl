library ieee;
use ieee.std_logic_1164.all;

package uCmdBits is

    subtype uadr_bits  is integer range 10 downto  0;
    subtype ucmd_bits  is integer range 39 downto  0;
    subtype ucmd_bits0 is integer range 24 downto  0;
    subtype ub_reg_x   is integer range  4 downto  0;
    subtype ub_reg_y   is integer range  9 downto  5;
    subtype ub_reg_z   is integer range 14 downto 10;
    subtype ub_alu     is integer range 19 downto 15;
    subtype ub_flag    is integer range 22 downto 20;
    subtype ub_goto    is integer range 24 downto 23;
    subtype ub_addr    is integer range 35 downto 25;
    subtype ub_const   is integer range 32 downto 25;
    subtype ub_trap    is integer range 39 downto 36;

    constant alu_x      : std_logic_vector(ub_alu) := "00000";
    constant alu_y      : std_logic_vector(ub_alu) := "00001";
    constant alu_not_x  : std_logic_vector(ub_alu) := "00010";
    constant alu_and    : std_logic_vector(ub_alu) := "00011";
    constant alu_or     : std_logic_vector(ub_alu) := "00100";
    constant alu_xor    : std_logic_vector(ub_alu) := "00101";
    constant alu_add    : std_logic_vector(ub_alu) := "00110";
    constant alu_sub    : std_logic_vector(ub_alu) := "00111";
    constant alu_bic    : std_logic_vector(ub_alu) := "01000";
    constant alu_mul0   : std_logic_vector(ub_alu) := "01010";
    constant alu_mul1   : std_logic_vector(ub_alu) := "01011";
    constant alu_add_xb : std_logic_vector(ub_alu) := "01110";
    constant alu_lxb    : std_logic_vector(ub_alu) := "01111";
    constant alu_sxb    : std_logic_vector(ub_alu) := "10000";
    constant alu_lss    : std_logic_vector(ub_alu) := "10001";
    constant alu_leq    : std_logic_vector(ub_alu) := "10010";
    constant alu_gtr    : std_logic_vector(ub_alu) := "10011";
    constant alu_geq    : std_logic_vector(ub_alu) := "10100";
    constant alu_equ    : std_logic_vector(ub_alu) := "10101";
    constant alu_neq    : std_logic_vector(ub_alu) := "10110";
    constant alu_y_xb0  : std_logic_vector(ub_alu) := "10111";
    constant alu_shr    : std_logic_vector(ub_alu) := "11000";
    constant alu_lxb_nc : std_logic_vector(ub_alu) := "11010";
    constant alu_shrs   : std_logic_vector(ub_alu) := "11011";
    constant alu_ror_xy : std_logic_vector(ub_alu) := "11110";
    constant alu_rol_xy : std_logic_vector(ub_alu) := "11111";

    constant rg_bs      : std_logic_vector(4 downto 0) := "01001";
    constant rg_p       : std_logic_vector(4 downto 0) := "01010";
    constant rg_g       : std_logic_vector(4 downto 0) := "01011";
    constant rg_l       : std_logic_vector(4 downto 0) := "01100";
    constant rg_m       : std_logic_vector(4 downto 0) := "01101";
    constant rg_s       : std_logic_vector(4 downto 0) := "01110";
    constant rg_f       : std_logic_vector(4 downto 0) := "01111";
    -- sregs:
    constant rg_stk     : std_logic_vector(4 downto 0) := "10000";
    constant rg_h       : std_logic_vector(4 downto 0) := "10001";
    constant rg_pc      : std_logic_vector(4 downto 0) := "10010";
    constant rg_imm     : std_logic_vector(4 downto 0) := "10011";
    constant rg_io      : std_logic_vector(4 downto 0) := "10100";
    constant rg_mrd     : std_logic_vector(4 downto 0) := "10101";
    constant rg_mwr     : std_logic_vector(4 downto 0) := "10110";
    constant rg_drop_stk: std_logic_vector(4 downto 0) := "10111";
    constant rg_tlb     : std_logic_vector(4 downto 0) := "11000";
    constant rg_base    : std_logic_vector(4 downto 0) := "11001";
    constant rg_copt    : std_logic_vector(4 downto 0) := "11010";
    constant rg_nil     : std_logic_vector(4 downto 0) := "11011";
    constant rg_swap    : std_logic_vector(4 downto 0) := "11100";
    constant rg_trap    : std_logic_vector(4 downto 0) := "11101";
    constant rg_const   : std_logic_vector(4 downto 0) := "11111";

end uCmdBits;
