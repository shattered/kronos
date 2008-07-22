DEFINITION MODULE inCmd; (* Sem 26-Feb-91. (c) KRONOS *)

VAR
  code : DYNARR OF CHAR;
  cnt  : INTEGER;

PROCEDURE b(n: INTEGER);
PROCEDURE w(n: INTEGER);
PROCEDURE new_code(n: INTEGER): INTEGER;

TYPE
  condition = (c_o,c_no,c_c,c_nc,c_z,c_nz,c_cz,c_ncz,
               c_s,c_ns,c_p,c_np,c_l,c_ge,c_le,c_g);

CONST
-------------------------------------------------------------
add_bmr = 00h;  add_wmr = 01h;  add_brm = 02h;  add_wrm = 03h;
add_bai = 04h;  add_wai = 05h;  push_es = 06h;  pop_es  = 07h;
adc_bmr = 10h;  adc_wmr = 11h;  adc_brm = 12h;  adc_wrm = 13h;
adc_bai = 14h;  adc_wai = 15h;  push_ss = 16h;  pop_ss  = 17h;
and_bmr = 20h;  and_wmr = 21h;  and_brm = 22h;  and_wrm = 23h;
and_bai = 24h;  and_wai = 25h;  seg_es  = 26h;  daa     = 27h;
xor_bmr = 30h;  xor_wmr = 31h;  xor_brm = 32h;  xor_wrm = 33h;
xor_bai = 34h;  xor_wai = 35h;  seg_ss  = 36h;  aaa     = 37h;
-------------------------------------------------------------
inc_ax  = 40h;  inc_cx  = 41h;  inc_dx  = 42h;  inc_bx  = 43h;
inc_sp  = 44h;  inc_bp  = 45h;  inc_si  = 46h;  inc_di  = 47h;
push_ax = 50h;  push_cx = 51h;  push_dx = 52h;  push_bx = 53h;
push_sp = 54h;  push_bp = 55h;  push_si = 56h;  push_di = 57h;
pusha   = 60h;  popa    = 61h;  bound   = 62h;
jo      = 70h;  jno     = 71h;  jb      = 72h;  jnb     = 73h;
je      = 74h;  jne     = 75h;  jbe     = 76h;  jnbe    = 77h;
-------------------------------------------------------------
imm_bm  = 80h;  imm_wm  = 81h;                  imm_wbm = 83h;
test_bm = 84h;  test_wm = 85h;  xchg_bmr= 86h;  xchg_wmr= 87h;
nop     = 90h;  xchg_cx = 91h;  xchg_dx = 92h;  xchg_bx = 93h;
xchg_sp = 94h;  xchg_bp = 95h;  xchg_si = 96h;  xchg_di = 97h;
mov_alm =0A0h;  mov_axm =0A1h;  mov_mal =0A2h;  mov_max =0A3h;
movs_b  =0A4h;  movs_w  =0A5h;  cmps_b  =0A6h;  cmps_w  =0A7h;
mov_ali =0B0h;  mov_cli =0B1h;  mov_dli =0B2h;  mov_bli =0B3h;
mov_ahi =0B4h;  mov_chi =0B5h;  mov_dhi =0B6h;  mov_bhi =0B7h;
-------------------------------------------------------------
shift_bmi=0C0h; shift_wmi=0C1h; ret_sp  =0C2h;  ret     =0C3h;
les     =0C4h;  lds     =0C5h;  mov_bmi =0C6h;  mov_wmi =0C7h;
shift_bm1=0D0h; shift_wm1=0D1h; shift_bmc=0D2h; shift_wmc=0D3h;
aam     =0D4h;  aad     =0D5h;                  xlat    =0D7h;
loopne  =0E0h;  loope   =0E1h;  loop    =0E2h;  jcxz    =0E3h;
in_b    =0E4h;  in_w    =0E5h;  out_b   =0E6h;  out_w   =0E7h;
lock    =0F0h;                  rep     =0F2h;  rep_z   =0F3h;
hlt     =0F4h;  cmc     =0F5h;  grp1_bm =0F6h;  grp1_wm =0F7h;
-------------------------------------------------------------
or_bmr  = 08h;  or_wmr  = 09h;  or_brm  = 0Ah;  or_wrm  = 0Bh;
or_bai  = 0Ch;  or_wai  = 0Dh;  push_cs = 0Eh;
sbb_bmr = 18h;  sbb_wmr = 19h;  sbb_brm = 1Ah;  sbb_wrm = 1Bh;
sbb_bai = 1Ch;  sbb_wai = 1Dh;  push_ds = 1Eh;  pop_ds  = 1Fh;
sub_bmr = 28h;  sub_wmr = 29h;  sub_brm = 2Ah;  sub_wrm = 2Bh;
sub_bai = 2Ch;  sub_wai = 2Dh;  seg_cs  = 2Eh;  das     = 2Fh;
cmp_bmr = 38h;  cmp_wmr = 39h;  cmp_brm = 3Ah;  cmp_wrm = 3Bh;
cmp_bai = 3Ch;  cmp_wai = 3Dh;  seg_ds  = 3Eh;  aas     = 3Fh;
-------------------------------------------------------------
dec_ax  = 48h;  dec_cx  = 49h;  dec_dx  = 4Ah;  dec_bx  = 4Bh;
dec_sp  = 4Ch;  dec_bp  = 4Dh;  dec_si  = 4Eh;  dec_di  = 4Fh;
pop_ax  = 58h;  pop_cx  = 59h;  pop_dx  = 5Ah;  pop_bx  = 5Bh;
pop_sp  = 5Ch;  pop_bp  = 5Dh;  pop_si  = 5Eh;  pop_di  = 5Fh;
push_wi = 68h;  imul_rwim=69h;  push_bi = 6Ah;  imul_rbim=6Bh;
ins_b   = 6Ch;  ins_w   = 6Dh;  outs_b  = 6Eh;  outs_w  = 6Fh;
js      = 78h;  jns     = 79h;  jp      = 7Ah;  jnp     = 7Bh;
jl      = 7Ch;  jge     = 7Dh;  jle     = 7Eh;  jg      = 7Fh;
-------------------------------------------------------------
mov_bmr = 88h;  mov_wmr = 89h;  mov_brm = 8Ah;  mov_wrm = 8Bh;
mov_msr = 8Ch;  lea     = 8Dh;  mov_srm = 8Eh;  pop_m   = 0Fh;
cbw     = 98h;  cwd     = 99h;  call_id = 9Ah;  wait    = 9Bh;
pushf   = 9Ch;  popf    = 9Dh;  sahf    = 9Eh;  lahf    = 9Fh;
test_bi =0A8h;  test_wi =0A9h;  stos_b  =0AAh;  stos_w  =0ABh;
lods_b  =0ACh;  lods_w  =0ADh;  scas_b  =0AEh;  scas_w  =0AFh;
mov_axi =0B8h;  mov_cxi =0B9h;  mov_dxi =0BAh;  mov_bxi =0BBh;
mov_spi =0BCh;  mov_bpi =0BDh;  mov_sii =0BEh;  mov_dii =0BFh;
-------------------------------------------------------------
enter   =0C8h;  leave   =0C9h;  ret_isp =0CAh;  ret_i   =0CBh;
int_3   =0CCh;  int     =0CDh;  into    =0CEh;  iret    =0CFh;
esc     =0D8h;
call    =0E8h;  jmp     =0E9h;  jmp_i   =0EAh;  jmp_s   =0EBh;
in_vb   =0ECh;  in_vw   =0EDh;  out_vb  =0EEh;  out_vw  =0EFh;
clc     =0F8h;  stc     =0F9h;  cli     =0FAh;  sti     =0FBh;
cld     =0FCh;  std     =0FDh;  grp2_b  =0FEh;  grp2_w  =0FFh;
-------------------------------------------------------------
-- imm
i_add   = 00b;  i_or    = 10b;  i_adc   = 20b;  i_sbb   = 30b;
i_and   = 40b;  i_sub   = 50b;  i_xor   = 60b;  i_cmp   = 70b;
-- shift
s_rol   = 00b;  s_ror   = 10b;  s_rcl   = 20b;  s_rcr   = 30b;
s_shl   = 40b;  s_shr   = 50b;  s_sal   = 60b;  s_sar   = 70b;
-- grp1
g1_test = 00b;                  g1_not  = 20b;  g1_neg  = 30b;
g1_mul  = 40b;  g1_imul = 50b;  g1_div  = 60b;  g1_idiv = 70b;
-- grp2
g2_inc  = 00b;  g2_dec  = 10b;  g2_call_i=20b;  g2_call_ii=30b;
g2_jmp_i= 40b;  g2_jmp_ii=50b;  g2_push = 60b;
-- mode
md_0    =000b;  md_b    =100b;  md_w    =200b;  md_reg  =300b;
md_abs  =006b;
-- reg / mem
rm_bx_si=  0b;  rm_bx_di=  1b;  rm_bp_si=  2b;  rm_bp_di=  3b;
rm_si   =  4b;  rm_di   =  5b;  rm_bp   =  6b;  rm_bx   =  7b;
-- reg
AX = 0; CX = 1; DX = 2; BX = 3; SP = 4; BP = 5; SI = 6; DI = 7;
AL = 0; CL = 1; DL = 2; BL = 3; AH = 4; CH = 5; DH = 6; BH = 7;
ES = 0; CS = 1; SS = 2; DS = 3;

END inCmd.
