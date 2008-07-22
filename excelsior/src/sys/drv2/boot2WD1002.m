MODULE boot2WD1002; (*$T-$N-$I- Leo 20-Dec-89. (c) KRONOS *)

(* booter for Kronos-2 WD1002 *)

IMPORT  SYSTEM;
IMPORT  cod : defCodes;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

PROCEDURE move(a,b,c: INTEGER);    CODE cod.move END move;
PROCEDURE quit;                    CODE cod.quit END quit;
PROCEDURE inp(a: WORD): BITSET;    CODE cod.inp  END inp;
PROCEDURE out(a,b: WORD);          CODE cod.out  END out;
PROCEDURE transfer(VAR f,t: WORD); CODE cod.tra  END transfer;
PROCEDURE self(): WORD;            CODE cod.li0 cod.lsw0 END self;

CONST
  UNIT = 0;
  ok  = 0;
  fail= 1;
  DAT = 176400b DIV 2;
  ERR = DAT+1;
  SNO = DAT+2;
  SEC = DAT+3;
  CYLa= DAT+4;
  CYLb= DAT+5;
  DRV = DAT+6;
  CSR = DAT+7;

  secsz = 512;
  secno = 16;
  heads = 2;
  trkno = 612;

PROCEDURE write_char(c: CHAR);
  CONST csr=177564b DIV 2; dtr=csr+1;
BEGIN
  REPEAT UNTIL inp(csr)*{7}#{}; out(dtr,c);
END write_char;

PROCEDURE write_string(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0; WHILE s[i]#0c DO write_char(s[i]); INC(i) END;
END write_string;

PROCEDURE write_num(n: INTEGER);
BEGIN
  IF n<0 THEN write_char('-'); n:=-n END;
  IF n>9 THEN write_num(n DIV 10) END;
  write_char(CHAR(ORD('0')+n MOD 10));
END write_num;

PROCEDURE inp4(n: INTEGER): INTEGER;
CODE
  cod.copt cod.inp cod.swap
  cod.copt cod.inp cod.swap
  cod.copt cod.inp cod.swap
  cod.inp
  8 cod.rol cod.or
  8 cod.rol cod.or
  8 cod.rol cod.or
END inp4;

PROCEDURE read_sec(u,sec,len: INTEGER; a: ADDRESS): INTEGER;
  VAR
    bf  : POINTER TO ARRAY [0..127] OF INTEGER;
    head: INTEGER;
    trk : INTEGER;
    i   : INTEGER;
BEGIN
  WHILE inp(CSR)*{7}#{} DO END;
  IF inp(CSR)*{3}#{} THEN
    WHILE inp(CSR)*{3}#{} DO out(DAT,0) END;
    RETURN fail;
  END;
  head:=sec  DIV secno;
  trk :=head DIV heads;
  head:=head MOD heads;
  sec :=sec  MOD secno;
  out(DRV,u*8+20h+head);
  out(CYLa,trk MOD 100h);
  out(CYLb,trk DIV 100h);
  out(SNO,len);
  out(SEC,sec);
  out(CSR,24h); -- read sectors
  WHILE len>0 DO
    REPEAT
      IF inp(CSR)*{0}#{} THEN RETURN fail END;
    UNTIL inp(CSR)*{3}#{};
    bf:=a; INC(a,128);
    FOR i:=0 TO 127 DO bf^[i]:=inp4(DAT) END;
    DEC(len);
  END;
  WHILE inp(CSR)*{7}#{} DO END;
  IF inp(CSR)*{3}#{} THEN
    WHILE inp(CSR)*{3}#{} DO IF inp(CSR)#{} THEN END END;
    RETURN fail;
  END;
  IF inp(CSR)*{0}#{} THEN RETURN fail END;
  RETURN ok;
END read_sec;

PROCEDURE read_block(b: INTEGER; Buf: ADDRESS);
BEGIN
  REPEAT UNTIL read_sec(UNIT,b*8,8,Buf)=ok;
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
  read_block(2,ino);
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^;
  IF eof<=0 THEN write_string('Empty system file.'15c 12c); quit END;
  eof:=(eof+4095) DIV 4096;
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
    IF buf+1024>=lim THEN quit END;
    IF bno<=0 THEN quit END;
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
  write_string('cpu: Kronos-2, drive: WD1002, ');
  top:=memory_top();
  write_string('memory: ');
  write_num((top+1) DIV KB);
  write_string('K.'15c 12c);
  read_system;
  start_system
END boot2WD1002.
