MODULE booter; (*$T-$N-$I- Leo 20-Dec-89. (c) KRONOS *)
                       (* Hady 02-Mar-90. (c) KRONOS *)

(* booter from VD0 for 2.6 with Q_bus *)

IMPORT  cod: defCodes;
IMPORT  sys: SYSTEM;

TYPE ADDRESS = sys.ADDRESS;
        WORD = sys.WORD;

PROCEDURE move(to,from: ADDRESS; words: INTEGER); CODE cod.move END move;
PROCEDURE quit;                           CODE cod.quit  END quit;
PROCEDURE transfer(VAR from,to: ADDRESS); CODE cod.tra   END transfer;
PROCEDURE S_reg(): ADDRESS;               CODE cod.li0   cod.alloc END S_reg;
PROCEDURE inp(reg: INTEGER): BITSET;      CODE cod.inp   END inp;
PROCEDURE out(reg: INTEGER; w: sys.WORD); CODE cod.out   END out;
PROCEDURE reset;                          CODE cod.reset END reset;
PROCEDURE getm(): BITSET ; CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;

----------------------------  TTY   ----------------------------

CONST
  CSR=177564b DIV 2;
  DTR=CSR+1;

PROCEDURE Write(ch: CHAR);
BEGIN
  REPEAT UNTIL 7 IN inp(CSR);
  out(DTR,ORD(ch) MOD 128);
END Write;

PROCEDURE write_str(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO Write(s[i]); INC(i) END;
END write_str;

PROCEDURE write_int(i: INTEGER; n: INTEGER);

  VAR str: ARRAY [0..15] OF CHAR; s: INTEGER;

  PROCEDURE putDig(i: INTEGER; n: INTEGER);
    VAR d: INTEGER;
  BEGIN
    d:=i MOD 10; i:=i DIV 10; DEC(n);
    IF i=0 THEN
      WHILE n>0 DO str[s]:=' '; INC(s); DEC(n) END
    ELSE
      putDig(i,n)
    END;
    str[s]:=CHAR(ORD('0')+d); INC(s);
  END putDig;

BEGIN s:=0;
  IF n<=0 THEN n:=1 END;
  IF i<0 THEN str[s]:='-'; INC(s); DEC(n); i:=ABS(i) END;
  putDig(i,n);
  str[s]:=0c; write_str(str);
END write_int;

PROCEDURE write_hex(w: sys.WORD; n: INTEGER);
  CONST hex="0123456789ABCDEF";
  VAR str: ARRAY [0..15] OF CHAR; s: INTEGER;
      i,d: INTEGER;
BEGIN s:=0;
  IF n<=0 THEN n:=8 END;
  WHILE n>8 DO str[s]:=' '; INC(s); DEC(n) END;
  i:=8-n;
  WHILE i>0 DO w:=(BITSET(w)<<4); DEC(i) END;
  i:=1;
  WHILE i<=n DO
    w:=(BITSET(w)<<4); str[s]:=hex[INTEGER(w) MOD 16]; INC(s); INC(i);
  END;
  str[s]:=0c; write_str(str);
END write_hex;

----------------------------  DISK  ----------------------------

VAR boot_device: ARRAY [0..3] OF CHAR;

PROCEDURE read(b: INTEGER; buf: ADDRESS; sz: INTEGER): BOOLEAN;
CODE 92h END read;

PROCEDURE read_block(n: INTEGER; a: ADDRESS; VAR res: INTEGER);
BEGIN
  IF read(n,a,4096) THEN res:=1 ELSE res:=0 END;
END read_block;

---------------------------- BOOTER ----------------------------
                            --------

CONST system_base = 64*256;
               KB = 256; (* words in 1KB *)

VAR top : ADDRESS;  (* :=MemoryTop() *)
    size: INTEGER;

PROCEDURE memory_top(): ADDRESS;
  CONST bank = 256*KB;  pattern=12345678h;
  VAR a: ADDRESS;
BEGIN
  a:=S_reg(); move(a,0,bank-a); -- filling first bank
  a:=bank;
  LOOP
   a^:=pattern;
   IF INTEGER(a^)#pattern THEN EXIT END;
   move(a,0,bank); a:=a+bank;
   IF a>=40000h          THEN EXIT END;
  END;
  RETURN a-1;
END memory_top;

PROCEDURE read_system;
  CONST long={2};
  VAR adr: ADDRESS;     buf: ADDRESS;   err: INTEGER;
      lim: ADDRESS;     bno: INTEGER;
      ino: ADDRESS;     ref: ADDRESS;
     mode: POINTER TO BITSET;
     link: POINTER TO INTEGER;
      eof: INTEGER;

  PROCEDURE bad;
  BEGIN write_str("illegal SYSTEM.BOOT file" 12c 15c); quit END bad;

BEGIN
  ino:=system_base-1024*2;
  ref:=system_base-1024*1;
  REPEAT read_block(2,ino,err) UNTIL err=0;
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^;   eof:=(eof+4095) DIV 4096;
  link:=ino+9;
  mode:=ino+8;
  IF link^<=0 THEN bad END;
  IF long*mode^={} THEN
    IF eof>8 THEN bad END;
    move(ref,ino,eof)
  ELSE
    IF INTEGER(ino^)<=0 THEN bad END;
    REPEAT read_block(ino^,ref,err) UNTIL err=0
  END;
  buf:=system_base;
  lim:=top-256;
  bno:=ref^;
  size:=0;
  WHILE eof>0 DO
    IF buf+1024>=lim THEN
      write_str("#booter: too small memory" 12c 15c); quit
    END;
    IF bno<=0 THEN bad END;
    read_block(bno,buf,err);
    IF err=0 THEN
      INC(buf,1024); INC(ref); bno:=ref^; INC(size,1024); DEC(eof)
    END
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
      dev: POINTER TO ARRAY [0..3] OF CHAR;
        p: process;
BEGIN

  adr:=system_base+88h; adr^:=top;
  adr:=system_base+89h; dev :=adr;
  dev^:=boot_device;

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
  adr^:=c(cod.move)+c(cod.tra<<8)+c(cod.quit<<16);
  transfer(p,p);
END start_system;

VAR i: INTEGER;

BEGIN
  top:=memory_top();
  boot_device:="vd0";
  write_str("" 15c 12c);
  write_int((top+1) DIV 256,4); write_str("KB memory"15c 12c);
  read_system;
  write_str("Loaded OK" 15c 12c);
  start_system;
END booter.
