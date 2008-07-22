MODULE run; (*$U+ Sem 17-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, ADR;

IMPORT  tty : Terminal;
IMPORT  arg : tskArgs;
IMPORT  bio : BIO;
IMPORT  str : Strings;
IMPORT  mcd : defCodes;
IMPORT  os  : osKernel;
IMPORT  mem : Heap;

WITH STORAGE : mem;

CONST
  BASE    = ADDRESS(0F00000h);
  channel = 8;

VAR
  BCB   : POINTER TO ARRAY [0..7] OF CHAR;
  buffer: DYNARR OF CHAR;
  booter: DYNARR OF CHAR;

-- booter
-- 16 - sp
-- 18 - ss
-- 20 - ip
-- 22 - cs

PROCEDURE run_cpu(ip,cs: INTEGER);
BEGIN
  BCB^[4]:=CHAR(ip);
  BCB^[5]:=CHAR(ip>>8);
  BCB^[6]:=CHAR(cs);
  BCB^[7]:=CHAR(cs>>8);
  IF arg.flag('-','x') THEN HALT END;
  BCB^[0]:=1c;
  REPEAT UNTIL BCB^[0]=0c;
END run_cpu;

PROCEDURE load_file(s: ARRAY OF CHAR);
  VAR nm: ARRAY [0..79] OF CHAR;
  PROCEDURE chk;
  BEGIN
    IF bio.done THEN RETURN END;
    tty.perror(bio.error,'I/O error, file "%s": %%s.\n',nm);
    HALT;
  END chk;
  PROCEDURE bad;
  BEGIN
    tty.print('Invalid exe file "%s".\n',nm); HALT;
  END bad;
  VAR
    f        : bio.FILE;
    hdr      : ARRAY [0..1Bh] OF CHAR;
    tbl      : DYNARR OF INTEGER;
    i,j,n    : INTEGER;
    hdr_len  : INTEGER;
    cod_len  : INTEGER;
    start_seg: INTEGER;
BEGIN
  str.print(nm,'%s.exe',s);
  bio.open(f,nm,'r'); chk;
  bio.read(f,ADR(hdr),BYTES(hdr)); chk;
  IF (ORD(hdr[0])#5Ah) OR (ORD(hdr[1])#4Dh) THEN bad END;
  NEW(tbl,ORD(hdr[6])+ORD(hdr[7])*100h);
  bio.seek(f,ORD(hdr[24])+ORD(hdr[25])*100h,0); chk;
  bio.read(f,ADR(tbl),BYTES(tbl)); chk;
  hdr_len:=ORD(hdr[8])*10h+ORD(hdr[9])*1000h;
  cod_len:=(ORD(hdr[4])+ORD(hdr[5])*100h)*512-hdr_len;
  IF cod_len>BYTES(buffer) THEN tty.print('Too long code.\n'); HALT END;
  bio.seek(f,hdr_len,0); chk;
  bio.read(f,ADR(buffer),cod_len); chk;
  bio.close(f); chk;
  start_seg:=ADR(buffer) DIV 4 MOD 10000h;
  FOR i:=0 TO HIGH(tbl) DO
    n:=tbl[i] MOD 10000h + tbl[i] DIV 10000h * 10h;
    j:=ORD(buffer[n])+ORD(buffer[n+1])*100h+start_seg;
    buffer[n]:=CHAR(j); buffer[n+1]:=CHAR(j>>8);
  END;
  DISPOSE(tbl);
  booter[30h]:=hdr[16]; booter[31h]:=hdr[17]; -- sp
  j:=ORD(hdr[14])+ORD(hdr[15])*100h+start_seg;
  booter[32h]:=CHAR(j); booter[33h]:=CHAR(j>>8); -- ss
  booter[34h]:=hdr[20]; booter[35h]:=hdr[21]; -- ip
  j:=ORD(hdr[22])+ORD(hdr[23])*100h+start_seg;
  booter[36h]:=CHAR(j); booter[37h]:=CHAR(j>>8); -- cs
END load_file;

PROCEDURE init_booter;
  CONST cod =
    '' 36c 6c 214c 310c 216c 330c 211c 46c 70c 0c 214c 26c 72c 0c 372c
    '' 213c 46c 60c 0c 216c 26c 62c 0c 373c 36c 377c 36c 64c 0c 37c 372c
    '' 213c 46c 70c 0c 216c 26c 72c 0c 373c 7c 37c 313c;
  VAR i: INTEGER;
BEGIN
(*
  push ds
  push es
  mov ax,cs
  mov ds,ax
  mov [38h],sp
  mov [3Ah],ss
  cli
  mov sp,[30h]
  mov ss,[32h]
  sti
  push ds
  calll [34h]
  pop ds
  cli
  mov sp,[38h]
  mov ss,[3Ah]
  sti
  pop es
  pop ds
  retl
*)
  FOR i:=0 TO HIGH(booter) DO
    IF i>HIGH(cod) THEN booter[i]:=0c ELSE booter[i]:=cod[i] END;
  END;
END init_booter;

PROCEDURE buf_def(VAL s: ARRAY OF CHAR;
                  VAR a: ADDRESS; VAR size: INTEGER): BOOLEAN;
  VAR i: INTEGER; done: BOOLEAN;
BEGIN
  i:=1;
  str.skip(s,i,' ');
  IF i+2>HIGH(s) THEN RETURN FALSE END;
  IF (s[i]='V') & (s[i+1]='D') & (s[i+2]='U') THEN i:=i+3
  ELSE RETURN FALSE
  END;
  str.skip(s,i,' ');
  str.iscan(a,s,i,done);
  IF NOT done OR (s[i]#' ') THEN RETURN FALSE END;
  str.skip(s,i,' ');
  str.iscan(size,s,i,done);
  IF NOT done THEN RETURN FALSE END;
  IF (i<=HIGH(s)) & (s[i]='K') THEN size:=size*1024; INC(i) END;
  IF size<=0 THEN RETURN FALSE END;
  str.skip(s,i,' ');
  RETURN (s[i]=0c) OR (s[i]='%');
END buf_def;

PROCEDURE init_buf;
  VAR
    i   : INTEGER;
    adr : ADDRESS;
    str : STRING;
    size: INTEGER;
BEGIN
  i:=0;
  WHILE os.get_sys_parm(i,str) DO
    IF buf_def(str,adr,size) THEN
--      tty.print('buffer adr=%$8hh, bytes=%$8hh.\n',adr,size);
      ASSERT(adr MOD 4 = 0);
      booter^.ADR:=adr+0;  booter^.HIGH:=63;
      buffer^.ADR:=adr+16; buffer^.HIGH:=size-65;
      RETURN;
    END
  END;
  tty.print('VDU buffer not found.\n'); HALT;
END init_buf;

PROCEDURE di(): BITSET; CODE mcd.getm mcd.copt 3h mcd.bic mcd.setm END di;
PROCEDURE ei(m: BITSET); CODE mcd.setm END ei;

VAR
  i : INTEGER;
  m : BITSET;

BEGIN
  BCB:=BASE+(0F8010h+channel*4) DIV 4;
  IF BCB^[2]#3c THEN tty.print('I8086 buffer not found.\n'); HALT END;
  ASSERT(ORD(BCB^[0]) MOD 4 = 0);
  BCB:=BASE+(0F0000h+ORD(BCB^[0])+ORD(BCB^[1])*100h) DIV 4;
  IF LEN(arg.words)#1 THEN tty.print('run <file name>\n'); HALT END;
  init_buf;
  init_booter;
  load_file(arg.words[0]);
  i:=INTEGER(ADR(booter)-BASE)*4;
  m:=di();
  run_cpu(i MOD 10h, i DIV 10h);
  REPEAT FOR i:=0 TO 10000 DO END UNTIL BCB^[1]=0c;
  ei(m);
  i:=ORD(booter[3Ch]);
  IF (i=0) OR (i=47h) THEN HALT ELSE HALT(i) END;
END run.
