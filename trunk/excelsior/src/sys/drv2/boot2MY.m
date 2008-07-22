MODULE boot2MY; (*$T-$N-$I- Leo 20-Dec-89. (c) KRONOS *)

(* booter for Kronos-2 MY *)

IMPORT  SYSTEM;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

PROCEDURE move(a,b,c: INTEGER);    CODE 0C0h  END move;
PROCEDURE quit;                    CODE  81h  END quit;
PROCEDURE inp(a: WORD): BITSET;    CODE  90h  END inp;
PROCEDURE out(a,b: WORD);          CODE  91h  END out;
PROCEDURE transfer(VAR f,t: WORD); CODE  85h  END transfer;
PROCEDURE self(): WORD;            CODE 0 60h END self;

CONST
  UNIT = 0;
  CSR  = 172140b DIV 2;
  DTR  = CSR+1;
  READ = 001b;

VAR
  BUF: ADDRESS;
 BCB0: ADDRESS;
 BCB1: ADDRESS;

PROCEDURE read_block(b: INTEGER; Buf: ADDRESS);
  VAR bcb: ADDRESS; TYPE c=BITSET;
BEGIN
  b:=b*4;
  REPEAT
    BCB0^:=c(b DIV 5 MOD 2*4+UNIT)+c(BUF<<18);
    BCB1^:=c(b MOD 5+1)+c(b DIV 10)<<8+c(2048<<16);
(*
    bcb^[0]:=c(b DIV 5 MOD 2*4+UNIT);
    bcb^[1]:=0c;
    bcb^[2]:=c(BUF*4 MOD 100h);
    bcb^[3]:=c(BUF*4 DIV 100h);
    bcb^[4]:=c(b MOD 5+1);
    bcb^[5]:=c(b DIV 10);
    bcb^[6]:=c(2048 MOD 100h);
    bcb^[7]:=c(2048 DIV 100h);
*)
    REPEAT UNTIL {5}*inp(CSR)#{};
    out(CSR,READ);
    REPEAT UNTIL {5,7}*inp(CSR)={7};
    out(DTR,BCB0*4);
    REPEAT UNTIL  {5}*inp(CSR)#{};
    move(Buf,BUF,1024)
  UNTIL {15}*inp(CSR)={}
END read_block;

---------------------------- BOOTER ----------------------------
                            --------

CONST          KB = 256; (* words in 1KB *)
      system_base = 64*KB;

VAR top : ADDRESS;  (* :=MemoryTop() *)
    size: INTEGER;

PROCEDURE memory_top(): ADDRESS;
  VAR A,B: ADDRESS; N: INTEGER;
        S: WORD;    P: POINTER TO RECORD g,l,m,pc,s,h,t: INTEGER END;
BEGIN
  P:=self();  A:=0;  N:=2048;
  LOOP
    P^.t:=0;
    WHILE (A<=NIL) & (A^=A^) & (P^.t#3) DO INC(A,N) END;
    DEC(A,N);
    IF N=1 THEN RETURN A END;
    N:=1
  END
END memory_top;

PROCEDURE read_system;
  CONST long={2};
  VAR adr: ADDRESS;     buf: ADDRESS;
      lim: ADDRESS;     bno: INTEGER;
      ino: ADDRESS;     ref: ADDRESS;
     mode: POINTER TO BITSET;
     link: POINTER TO INTEGER;
      eof: INTEGER;
BEGIN
  ino :=system_base-1024*2;
  ref :=system_base-1024*1;
  BCB0:=system_base-1024*4;   BCB1:=BCB0+1;
  BUF :=system_base-1024*8;
  read_block(2,ino);
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^;   eof:=(eof+4095) DIV 4096;
  link:=ino+9;
  mode:=ino+8;
  IF link^<=0 THEN quit END;
  IF long*mode^={} THEN
    move(ref,ino,eof)
  ELSE
    read_block(ino^,ref)
  END;
  buf:=system_base;
  lim:=top-256;
  bno:=ref^;
  size:=0;
  WHILE eof>0 DO
   (*
    IF buf+1024>=lim THEN quit END;
    IF bno<=0 THEN quit END;
   *)
    read_block(bno,buf);
    INC(buf,1024); INC(ref); bno:=ref^; INC(size,1024); DEC(eof)
  END
END read_system;

TYPE process = POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

PROCEDURE start_system;
  TYPE c = BITSET;
  VAR adr: ADDRESS;
        p: process;
BEGIN
  adr:=system_base+88h; adr^:=top;
  adr:=top-256;

  p:=adr;
  p^.G:=adr+08; p^.L :=adr+10;
  p^.M:={};     p^.PC:=0;
  p^.H:=top;    p^.S :=adr+20;
  p^.T:=0;      p^.G^:=adr+09;

  (* stack *)
  adr  :=p^.S;
  DEC(adr);     adr^:=5;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=1;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=system_base;
  DEC(adr);     adr^:=size;
  (* code *)
  adr:=p^.G^;
  adr^:=c(0C0h)+c(085h<<8)+c(081h<<16);
  transfer(p,p);
END start_system;

BEGIN
  top:=memory_top();
  read_system;
  start_system
END boot2MY.
