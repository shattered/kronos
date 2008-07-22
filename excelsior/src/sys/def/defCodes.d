DEFINITION MODULE defCodes; (* Shu 11-Jul-86. (c) KRONOS *)
                            (* Ned 28-Sep-89. (c) KRONOS *)

(* Модуль содержит мнемоники команд процессора. *)

CONST

   li0  = 00h;   llw   =  20h;   lxb   =  40h;   lsw0  =  60h;
   li1  = 01h;   lgw   =  21h;   lxw   =  41h;   lsw1  =  61h;
   li2  = 02h;   lew   =  22h;   lgw2  =  42h;   lsw2  =  62h;
   li3  = 03h;   lsw   =  23h;   lgw3  =  43h;   lsw3  =  63h;
   li4  = 04h;   llw4  =  24h;   lgw4  =  44h;   lsw4  =  64h;
   li5  = 05h;   llw5  =  25h;   lgw5  =  45h;   lsw5  =  65h;
   li6  = 06h;   llw6  =  26h;   lgw6  =  46h;   lsw6  =  66h;
   li7  = 07h;   llw7  =  27h;   lgw7  =  47h;   lsw7  =  67h;

   li8  = 08h;   llw8  =  28h;   lgw8  =  48h;   lsw8  =  68h;
   li9  = 09h;   llw9  =  29h;   lgw9  =  49h;   lsw9  =  69h;
   li0A = 0Ah;   llw0A =  2Ah;   lgw0A =  4Ah;   lsw0A =  6Ah;
   li0B = 0Bh;   llw0B =  2Bh;   lgw0B =  4Bh;   lsw0B =  6Bh;
   li0C = 0Ch;   llw0C =  2Ch;   lgw0C =  4Ch;   lsw0C =  6Ch;
   li0D = 0Dh;   llw0D =  2Dh;   lgw0D =  4Dh;   lsw0D =  6Dh;
   li0E = 0Eh;   llw0E =  2Eh;   lgw0E =  4Eh;   lsw0E =  6Eh;
   li0F = 0Fh;   llw0F =  2Fh;   lgw0F =  4Fh;   lsw0F =  6Fh;

   lib  = 10h;   slw   =  30h;   sxb   =  50h;   ssw0  =  70h;
   lid  = 11h;   sgw   =  31h;   sxw   =  51h;   ssw1  =  71h;
   liw  = 12h;   sew   =  32h;   sgw2  =  52h;   ssw2  =  72h;
   lin  = 13h;   ssw   =  33h;   sgw3  =  53h;   ssw3  =  73h;
   lla  = 14h;   slw4  =  34h;   sgw4  =  54h;   ssw4  =  74h;
   lga  = 15h;   slw5  =  35h;   sgw5  =  55h;   ssw5  =  75h;
   lsa  = 16h;   slw6  =  36h;   sgw6  =  56h;   ssw6  =  76h;
   lea  = 17h;   slw7  =  37h;   sgw7  =  57h;   ssw7  =  77h;

   jflc = 18h;   slw8  =  38h;   sgw8  =  58h;   ssw8  =  78h;
   jfl  = 19h;   slw9  =  39h;   sgw9  =  59h;   ssw9  =  79h;
   jfsc = 1Ah;   slw0A =  3Ah;   sgw0A =  5Ah;   ssw0A =  7Ah;
   jfs  = 1Bh;   slw0B =  3Bh;   sgw0B =  5Bh;   ssw0B =  7Bh;
   jblc = 1Ch;   slw0C =  3Ch;   sgw0C =  5Ch;   ssw0C =  7Ch;
   jbl  = 1Dh;   slw0D =  3Dh;   sgw0D =  5Dh;   ssw0D =  7Dh;
   jbsc = 1Eh;   slw0E =  3Eh;   sgw0E =  5Eh;   ssw0E =  7Eh;
   jbs  = 1Fh;   slw0F =  3Fh;   sgw0F =  5Fh;   ssw0F =  7Fh;



   reset= 80h;   lss   = 0A0h;   move  = 0C0h;   incl  = 0E0h;
   quit = 81h;   leq   = 0A1h;   chknil= 0C1h;   excl  = 0E1h;
   getm = 82h;   gtr   = 0A2h;   lsta  = 0C2h;   inl   = 0E2h;
   setm = 83h;   geq   = 0A3h;   comp  = 0C3h;   quot  = 0E3h;
   trap = 84h;   equ   = 0A4h;   gb    = 0C4h;   inc1  = 0E4h;
   tra  = 85h;   neq   = 0A5h;   gb1   = 0C5h;   dec1  = 0E5h;
   tr   = 86h;   abs   = 0A6h;   chk   = 0C6h;   inc   = 0E6h;
   idle = 87h;   neg   = 0A7h;   chkz  = 0C7h;   dec   = 0E7h;

   add  = 88h;   or    = 0A8h;   alloc = 0C8h;   stot  = 0E8h;
   sub  = 89h;   and   = 0A9h;   entr  = 0C9h;   lodt  = 0E9h;
   mul  = 8Ah;   xor   = 0AAh;   rtn   = 0CAh;   lxa   = 0EAh;
   div  = 8Bh;   bic   = 0ABh;   nop   = 0CBh;   lpc   = 0EBh;
   shl  = 8Ch;   in    = 0ACh;   cx    = 0CCh;   bbu   = 0ECh;
   shr  = 8Dh;   bit   = 0ADh;   ci    = 0CDh;   bbp   = 0EDh;
   rol  = 8Eh;   not   = 0AEh;   cf    = 0CEh;   bblt  = 0EEh;
   ror  = 8Fh;   mod   = 0AFh;   cl    = 0CFh;   pdx   = 0EFh;

   io0  = 90h;   decs  = 0B0h;   cl0   = 0D0h;   swap  = 0F0h;
   io1  = 91h;   drop  = 0B1h;   cl1   = 0D1h;   lpa   = 0F1h;
   io2  = 92h;   lodfv = 0B2h;   cl2   = 0D2h;   lpw   = 0F2h;
   io3  = 93h;   store = 0B3h;   cl3   = 0D3h;   spw   = 0F3h;
   io4  = 94h;   stofv = 0B4h;   cl4   = 0D4h;   sswu  = 0F4h;
   rcmp = 95h;   copt  = 0B5h;   cl5   = 0D5h;   rchk  = 0F5h;
   wmv  = 96h;   cpcop = 0B6h;   cl6   = 0D6h;   rchkz = 0F6h;
   bmv  = 97h;   pcop  = 0B7h;   cl7   = 0D7h;   cm    = 0F7h;

   fadd = 98h;   for1  = 0B8h;   cl8   = 0D8h;   chkbox= 0F8h;
   fsub = 99h;   for2  = 0B9h;   cl9   = 0D9h;   bmg   = 0F9h;
   fmul = 9Ah;   entc  = 0BAh;   cl0A  = 0DAh;   activ = 0FAh;
   fdiv = 9Bh;   xit   = 0BBh;   cl0B  = 0DBh;   usr   = 0FBh;
   fcmp = 9Ch;   addpc = 0BCh;   cl0C  = 0DCh;   sys   = 0FCh;
   fabs = 9Dh;   jump  = 0BDh;   cl0D  = 0DDh;   nii   = 0FDh;
   fneg = 9Eh;   orjp  = 0BEh;   cl0E  = 0DEh;   dot   = 0FEh;
   ffct = 9Fh;   andjp = 0BFh;   cl0F  = 0DFh;   invld = 0FFh;

--------------------------------------------------------------
--            Команды ввода/вывода 90h..95h                 --
---------------+---------------------------------+------------
-- Kronos 2.2  |  Kronos 2.5   |                 |          --
-- Kronos 2.6q |               |                 |          --
---------------+---------------+-----------------+------------

   inp  = io0;
   out  = io1;
                  trb  = io3;

----------------------------------------------------------------

CONST -- ffct
  ffct_float = 0;
  ffct_trunc = 1;

CONST -- quot
  quot_shr   = 0;
  quot_quo   = 1;
  quot_and   = 2;
  quot_rem   = 3;

CONST -- sys
  sys_cpu    = 0;
  sys_dot    = 1;
  sys_vers   = 2;

CONST -- bmg
  bmg_inrect = 0;
  bmg_dvl    = 1;
  bmg_bblt   = 2;
  bmg_dch    = 3;
  bmg_clip   = 4;
  bmg_line   = 5;
  bmg_circ   = 6;
  bmg_arc    = 7;
  bmg_ftri   = 8;
  bmg_fcirc  = 9;

END defCodes.
