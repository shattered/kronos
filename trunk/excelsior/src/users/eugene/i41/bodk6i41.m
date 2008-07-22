IMPLEMENTATION MODULE bootDSK; (* 04-Dec-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM mCodeMnem  IMPORT  setm, getm, tra;

-- драйвер резервирует область памяти на И41 0C0000h..0C1FFFh

CONST
-----------------------------------------------------------------------

        unit=4;

-----------------------------------------------------------------------
  base   = 0EFE0h;
  port   = base<<30;
  adrPW  = base*10h;
  adrPUK = 0C0000h;
  adrPWK = adrPUK+16;
  adrPWW = adrPWK+16;
  adrWSP = adrPWW+30;
  adrBUF = adrWSP+12;
  adrFREE= adrBUF+4096;

add=30;

  ReadError    = BOOLEAN(add+00);    WriteError     = BOOLEAN(add+01);
  ReadProtect  = BOOLEAN(add+02);    WriteProtect   = BOOLEAN(add+03);
  NoSuchBlock  = BOOLEAN(add+04);    NoSuchDrive    = BOOLEAN(add+05);
  NoSuchCFE    = BOOLEAN(add+06);    NoVolume       = BOOLEAN(add+07);
  CheckError   = BOOLEAN(add+08);    EndOfVolume    = BOOLEAN(add+09);
  HardError    = BOOLEAN(add+10);    IllegalAccess  = BOOLEAN(add+11);
  TimeOut      = BOOLEAN(add+12);    NotImplemented = BOOLEAN(add+13);
  TooLong      = BOOLEAN(add+14);    IllegalTransNo = BOOLEAN(add+15);
  CheckSum     = BOOLEAN(add+16);    IllegalChanel  = BOOLEAN(add+17);
  TapeMark     = BOOLEAN(add+18);    IllegalHeader  = BOOLEAN(add+19);
  NotStandard  = BOOLEAN(add+20);    ReadAfterEOF   = BOOLEAN(add+21);

  SeekError    = BOOLEAN(add+25);    NotFoundTrack0 = BOOLEAN(add+26);
  IllegalDMA   = BOOLEAN(add+27);    HeaderCRC      = BOOLEAN(add+28);
  DataCRC      = BOOLEAN(add+29);    NotFoundSID    = BOOLEAN(add+30);
  NotFoundDID  = BOOLEAN(add+31);    NotReady       = BOOLEAN(add+32);
  BadBlock     = BOOLEAN(add+33);

TYPE
  func=(clear,stat,format,readID,read,readCHK,write,writeBF,seek);
  status_bit=(done,seek_done,frame,floppy,drive0,drive1,hard_err,error);
  status=SET OF status_bit;

VAR
  puk    : POINTER TO ARRAY [0..15] OF CHAR; -- поле управления каналом
  pwk    : POINTER TO ARRAY [0..15] OF CHAR; -- поле вызова контроллера
  pww    : POINTER TO ARRAY [0..29] OF CHAR;
  wsp    : POINTER TO ARRAY [0..11] OF CHAR;
  buf    : POINTER TO ARRAY [0..4095] OF CHAR;
  spec   : ARRAY [0..5] OF RECORD
    cyls   : INTEGER;
    heads  : INTEGER;
    sec_no : INTEGER;
    sec_sz : INTEGER;
    mount  : BOOLEAN;
  END;
  op_stat : status;

PROCEDURE MOVE(a,b: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;
PROCEDURE out(a,v: INTEGER); CODE 93h END out;
PROCEDURE SETM(m: BITSET); CODE setm END SETM;
PROCEDURE GETM(): BITSET; CODE getm END GETM;

PROCEDURE exec(): BOOLEAN;
  VAR s: status; d: INTEGER;
BEGIN
  puk^[1]:=377c;
  out(port,1);
  LOOP
    IF pwk^[3]#0c THEN
      s:=status(pwk^[1]); pwk^[3]:=0c;
      IF frame IN s THEN
        d:=0;
        IF drive0 IN s THEN INC(d,1) END;
        IF drive1 IN s THEN INC(d,2) END;
        IF NOT (floppy IN s) THEN INC(d,4) END;
        spec[d].mount:=FALSE;
      END;
      IF done IN s THEN op_stat:=s; EXIT END;
    END;
  END;
  RETURN FALSE;
END exec;

PROCEDURE reset(): BOOLEAN;
  VAR p: POINTER TO ARRAY [0..15] OF CHAR; r: BOOLEAN;
BEGIN
  out(port,2); out(port,0);
  p:=ADDRESS(0F00000h+adrPW DIV 4);
  p^[0]:=1c;
  p^[1]:=0c;
  p^[2]:=CHAR(adrPUK MOD 10h);
  p^[3]:=0c;
  p^[4]:=CHAR(adrPUK DIV 10h);
  p^[5]:=CHAR(adrPUK DIV 1000h);
  puk^[0]:=1c;
  puk^[2]:=CHAR((adrPWK+4) MOD 16);
  puk^[3]:=0c;
  puk^[4]:=CHAR((adrPWK+4) DIV 10h);
  puk^[5]:=CHAR((adrPWK+4) DIV 1000h);
  puk^[8]:=1c;
  puk^[10]:=CHAR((adrPUK+14) MOD 16);
  puk^[11]:=0c;
  puk^[12]:=CHAR((adrPUK+14) DIV 10h);
  puk^[13]:=CHAR((adrPUK+14) DIV 1000h);
  puk^[14]:=4c;
  puk^[15]:=0c;
  pwk^[2]:=0c;
  pwk^[3]:=0c;
  pwk^[4]:=0c;
  pwk^[5]:=0c;
  pwk^[6]:=0c;
  pwk^[7]:=0c;
  puk^[1]:=377c;
  out(port,1);
  REPEAT UNTIL puk^[1]=0c;
  pwk^[ 3]:=0c;
  pwk^[ 8]:=CHAR(adrPWW MOD 16);
  pwk^[ 9]:=0c;
  pwk^[10]:=CHAR(adrPWW DIV 10h);
  pwk^[11]:=CHAR(adrPWW DIV 1000h);
  pww^[0]:=0c; pww^[1]:=0c; pww^[2]:=0c; pww^[3]:=0c;
  pww^[26]:=0c; pww^[27]:=0c; pww^[28]:=0c; pww^[29]:=0c;
  RETURN FALSE;
END reset;

PROCEDURE wrPWW(drv,cyl,head,sec,sz: INTEGER; f: func; buf: ADDRESS);
  VAR i: INTEGER;
BEGIN
  IF drv<4 THEN pww^[8]:=3c ELSE pww^[8]:=0c END;
  pww^[9]:=0c;
  pww^[10]:=CHAR(drv MOD 4);
  pww^[11]:=CHAR(f);
  pww^[12]:=1c;
  pww^[13]:=0c;
  pww^[14]:=CHAR(cyl);
  pww^[15]:=CHAR(cyl>>8);
  pww^[16]:=CHAR(head);
  pww^[17]:=CHAR(sec);
  IF buf=NIL THEN
    pww^[18]:=0c;
    pww^[19]:=0c;
    pww^[20]:=0c;
    pww^[21]:=0c;
  ELSE
    i:=(INTEGER(buf)-0F00000h)*4;
    ASSERT(i>=0);
    pww^[18]:=CHAR(i MOD 16);
    pww^[19]:=0c;
    pww^[20]:=CHAR(i DIV 10h);
    pww^[21]:=CHAR(i DIV 1000h);
  END;
  pww^[22]:=CHAR(sz);
  pww^[23]:=CHAR(sz>>8);
  pww^[24]:=CHAR(sz>>16);
  pww^[25]:=CHAR(sz>>24);
END wrPWW;

PROCEDURE set_type(drv: INTEGER): BOOLEAN;
BEGIN
  WITH spec[drv] DO
    wsp^[0]:=CHAR(cyls);
    wsp^[1]:=CHAR(cyls>>8);
    wsp^[2]:=0c;
    wsp^[3]:=CHAR(heads);
    wsp^[4]:=CHAR(sec_no);
    wsp^[5]:=CHAR(sec_sz);
    wsp^[6]:=CHAR(sec_sz>>8);
    IF drv<4 THEN wsp^[7]:=1c ELSE wsp^[7]:=0c END;
  END;
  wrPWW(drv,0,0,0,8,clear,wsp);
  RETURN exec();
END set_type;

PROCEDURE err?(VAR retry: BOOLEAN): BOOLEAN;
  VAR i: INTEGER; s: status;
BEGIN
  retry:=FALSE;
  IF op_stat#status{} THEN
    s:=op_stat; op_stat:=status{};
    IF NOT (error IN s) THEN RETURN FALSE END;
    retry:=NOT (hard_err IN s);
    i:=0;
    IF drive0 IN s THEN INC(i) END;
    IF drive1 IN s THEN INC(i,2) END;
    wrPWW(i,0,0,0,12,stat,wsp);
    IF exec() THEN RETURN TimeOut END;
--IF 3 IN BITSET(wsp^[0]) THEN Show('  ошибка ОЗУ') END;
--IF 4 IN BITSET(wsp^[0]) THEN Show('  ошибка ПЗУ') END;
--IF 6 IN BITSET(wsp^[0]) THEN Show('  нелагальная альтернативная дорожка') END;
--IF 0 IN BITSET(wsp^[1]) THEN Show('  нелегальнай размер сектора') END;
    IF 2 IN BITSET(wsp^[1]) THEN RETURN NotReady END;
    IF 3 IN BITSET(wsp^[1]) THEN RETURN NotImplemented END;
    IF 4 IN BITSET(wsp^[1]) THEN RETURN NotFoundSID END;
--IF 5 IN BITSET(wsp^[1]) THEN Show('  нелегальнай адрес памяти') END;
    IF 6 IN BITSET(wsp^[1]) THEN RETURN NotReady END;
    IF 7 IN BITSET(wsp^[1]) THEN RETURN WriteProtect END;
    IF 3 IN BITSET(wsp^[2]) THEN RETURN DataCRC END;
    IF 4 IN BITSET(wsp^[2]) THEN RETURN HeaderCRC END;
    IF 5 IN BITSET(wsp^[2]) THEN RETURN HardError END; -- неисправен
    IF 6 IN BITSET(wsp^[2]) THEN RETURN SeekError END;
    IF 7 IN BITSET(wsp^[2]) THEN RETURN SeekError END;
    RETURN TRUE;
  ELSE
    RETURN TimeOut;
  END;
END err?;

PROCEDURE read_sec(d,sec: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR r,c: BOOLEAN; t,i,head,cyl: INTEGER;
BEGIN
  WITH spec[d] DO
    head:=sec DIV sec_no; sec:=sec MOD sec_no;
    cyl:=head DIV heads; head:=head MOD heads;
  END;
  IF d<4 THEN INC(sec) END;
  t:=8;
  REPEAT
    wrPWW(d,cyl,head,sec,sz,read,a);
    r:=exec();
    IF NOT r THEN r:=err?(c) ELSE c:=FALSE END;
    DEC(t);
  UNTIL (NOT r) OR (NOT c) OR (t=0);
  RETURN r;
END read_sec;

PROCEDURE mount(d: INTEGER): BOOLEAN;
  VAR r,c: BOOLEAN; s: BITSET;
BEGIN
  r:=read_sec(d,0,buf,1);
  IF r THEN RETURN r END;
  wrPWW(d,0,0,1,5,readID,buf);
  r:=exec();
  IF NOT r THEN r:=err?(c) END;
  IF NOT r THEN
    s:=BITSET(buf^[4])*{4,5};
    IF s={} THEN spec[d].sec_sz:=128 END;
    IF s={4} THEN spec[d].sec_sz:=256 END;
    IF s={5} THEN spec[d].sec_sz:=512 END;
    IF s={4,5} THEN spec[d].sec_sz:=1024 END;
  END;
  RETURN r;
END mount;

PROCEDURE DKRead(d,b: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR  r,i41: BOOLEAN; i,sec: INTEGER;
       to: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  IF (d<0) OR (d>HIGH(spec)) THEN RETURN NoSuchDrive END;
  sec:=b*4096 DIV spec[d].sec_sz;
  i41:=(a>=ADDRESS(0F00000h)) & (a<=ADDRESS(0F3FFFFh));
  IF i41 THEN
    r:=read_sec(d,sec,a,sz);
  ELSE
    r:=read_sec(d,sec,buf,sz);
    MOVE(a,buf,sz DIV 4); to:=a;
    FOR i:=INTEGER(BITSET(sz)-{0..1}) TO sz-1 DO to^[i]:=buf^[i] END;
  END;
  RETURN r;
END DKRead;

PROCEDURE InitDK(): BOOLEAN;
  VAR r,c: BOOLEAN;
BEGIN
  puk:=ADDRESS(0F00000h+adrPUK DIV 4);
  pwk:=ADDRESS(0F00000h+adrPWK DIV 4);
  pww:=ADDRESS(0F00000h+adrPWW DIV 4);
  wsp:=ADDRESS(0F00000h+adrWSP DIV 4);
  buf:=ADDRESS(0F00000h+adrBUF DIV 4);
  op_stat:=status{};
  IF reset() THEN RETURN TimeOut END;
  IF set_type(unit) THEN RETURN TimeOut END;
  r:=err?(c); IF r & (r#NotReady) THEN RETURN r END;
  RETURN FALSE;
END InitDK;

PROCEDURE read_block(bno: INTEGER; buf: ADDRESS; VAR ERR: INTEGER);
BEGIN
  ERR:=INTEGER(DKRead(unit,bno,buf,4096));
END read_block;

BEGIN
  WITH spec[0] DO
    cyls:=80; heads:=1; sec_no:=4; sec_sz:=1024; mount:=FALSE;
  END;
  WITH spec[1] DO
    cyls:=40; heads:=2; sec_no:=5; sec_sz:=1024; mount:=FALSE;
  END;
  WITH spec[2] DO
    cyls:=40; heads:=2; sec_no:=5; sec_sz:=1024; mount:=FALSE;
  END;
  WITH spec[3] DO
    cyls:=40; heads:=2; sec_no:=5; sec_sz:=1024; mount:=FALSE;
  END;
  WITH spec[4] DO
    cyls:=320; heads:=8; sec_no:=9; sec_sz:=1024; mount:=TRUE;
  END;
  WITH spec[5] DO
    cyls:=320; heads:=8; sec_no:=9; sec_sz:=1024; mount:=TRUE;
  END;
  boot_device:="dk0";
  boot_device[2]:=CHAR(ORD("0")+unit);
  IF InitDK() THEN END;
END bootDSK.
