IMPLEMENTATION MODULE bootDSK; (* 04-Dec-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
IMPORT mcd : mCodeMnem;

CONST
--------------------------------------------------------------------------
  unit      = 0;
  heads_no  = 4;
  tracks_no = 306;
  secs_no   = 16;
  step_time = 400;
--------------------------------------------------------------------------

  csr = 386h<<30;
  chA = 380h<<30;
  chB = 382h<<30;
  chC = 384h<<30;
  CMD = 388h<<30;

  sel0  = 0;
  sel1  = 1;
  step  = 2;
  head0 = 3;
  head1 = 4;
  redwr = 5;
  dir   = 6;
  cnt   = 7;

  tries = 15;

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

VAR
  ctrk : ARRAY [0..1] OF INTEGER;
  cmd  : BITSET;
  spec : ARRAY [0..1] OF RECORD
    heads : INTEGER;
    tracks: INTEGER;
    secno : INTEGER;
  END;
  inited: BOOLEAN;

PROCEDURE out(a: INTEGER; v: WORD); CODE 93h END out;
PROCEDURE inp(a: INTEGER): BITSET; CODE 92h END inp;
PROCEDURE setm(m: BITSET); CODE mcd.setm END setm;
PROCEDURE getm(): BITSET; CODE mcd.getm END getm;
PROCEDURE MOVE(a,b: ADDRESS; s: INTEGER); CODE mcd.move END MOVE;

PROCEDURE put(n: INTEGER; b: WORD);
  VAR p: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  IF ODD(n) THEN p:=ADDRESS(0F37200h); DEC(n) ELSE p:=ADDRESS(0F37000h) END;
  p^[n]:=b;
END put;

PROCEDURE get(n: INTEGER): CHAR;
  VAR p: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  IF ODD(n) THEN p:=ADDRESS(0F37200h); DEC(n) ELSE p:=ADDRESS(0F37000h) END;
  RETURN p^[n];
END get;

PROCEDURE seek0(d: INTEGER): BOOLEAN;
  VAR i,k: INTEGER;
BEGIN
  ctrk[d]:=-1;
  IF d=0 THEN cmd:=cmd+{sel0}-{sel1} ELSE cmd:=cmd-{sel0}+{sel1} END;
  EXCL(cmd,dir);
  out(chA,cmd);
  IF 1 IN inp(chC) THEN RETURN NotReady END;
  IF 3 IN inp(chC) THEN RETURN NotReady END;
  i:=4000;
  WHILE 0 IN inp(chC) DO
    IF i=0 THEN RETURN NotFoundTrack0 END;
    INCL(cmd,step);
    out(chA,cmd);
    EXCL(cmd,step);
    out(chA,cmd);
    FOR k:=0 TO step_time DO END;
    DEC(i);
  END;
  k:=50000;
  REPEAT
    DEC(k);
    IF k=0 THEN RETURN SeekError END;
  UNTIL NOT (2 IN inp(chC));
  ctrk[d]:=0;
  RETURN FALSE;
END seek0;

PROCEDURE seek(d,t: INTEGER): BOOLEAN;
  VAR i,k: INTEGER; r: BOOLEAN;
BEGIN
  IF ctrk[d]<0 THEN
    r:=seek0(d);
    IF r THEN RETURN r END;
    ctrk[d]:=0;
  END;
  i:=t-ctrk[d]; ctrk[d]:=-1;
  IF d=0 THEN cmd:=cmd+{sel0}-{sel1} ELSE cmd:=cmd-{sel0}+{sel1} END;
  out(chA,cmd);
  IF 1 IN inp(chC) THEN RETURN NotReady END;
  IF 3 IN inp(chC) THEN RETURN NotReady END;
  IF i<0 THEN i:=-i; EXCL(cmd,dir) ELSE INCL(cmd,dir) END;
  out(chA,cmd);
  WHILE i>0 DO
    INCL(cmd,step);
    out(chA,cmd);
    EXCL(cmd,step);
    out(chA,cmd);
    DEC(i);
    FOR k:=0 TO step_time DO END;
  END;
  k:=20000;
  REPEAT
    DEC(k);
    IF k=0 THEN RETURN SeekError END;
  UNTIL NOT (2 IN inp(chC));
  ctrk[d]:=t;
  RETURN FALSE;
END seek;

PROCEDURE move_head(d,t: INTEGER): BOOLEAN;
  VAR r: BOOLEAN; h: INTEGER;
BEGIN
  h:=t MOD spec[d].heads;
  r:=seek(d,t DIV spec[d].heads);
  IF 0 IN BITSET(h) THEN INCL(cmd,head0) ELSE EXCL(cmd,head0) END;
  IF 1 IN BITSET(h) THEN INCL(cmd,head1) ELSE EXCL(cmd,head1) END;
  out(chA,cmd);
  RETURN r;
END move_head;

PROCEDURE read_sec(d,s: INTEGER; a: ADDRESS): BOOLEAN;
  VAR r: BOOLEAN; i,h,t: INTEGER; p: POINTER TO ARRAY [0..511] OF CHAR;
    mem0: POINTER TO ARRAY [0..4095] OF CHAR;
    mem1: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  t:=s DIV spec[d].secno; s:=s MOD spec[d].secno;
  r:=move_head(d,t);
  IF r THEN RETURN r END;
  mem0:=ADDRESS(0F37000h);
  mem1:=ADDRESS(0F37200h);
  out(CMD,0);
  EXCL(cmd,cnt); out(chA,cmd);
  h:=t MOD spec[d].heads;
  t:=t DIV spec[d].heads;
  mem0^[0]:=0c;
  mem1^[0]:=0c;
  put(2h,0A1h);
  put(3h,0FEh);
  put(4h,t);
  put(5h,h);
  put(6h,s);
  FOR i:=7 TO 21h DO put(i,11h) END;
  put(22h,0A1h);
  put(23h,s);
  out(CMD,40h);
  INCL(cmd,cnt); out(chA,cmd);
  out(CMD,48h);
  REPEAT UNTIL (0 IN inp(chB)) OR NOT (1 IN inp(chB));
  cmd:=cmd-{sel0,sel1,cnt}; out(chA,cmd);
  IF NOT (1 IN inp(chB)) THEN
    r:=FALSE;
  ELSIF 3 IN inp(chB) THEN
    r:=DataCRC;
  ELSE
    r:=NotFoundSID;
  END;
  out(CMD,0);
  p:=a;
  FOR i:=0 TO 511 BY 2 DO p^[i]:=mem0^[i+24h] END;
  FOR i:=1 TO 511 BY 2 DO p^[i]:=mem1^[i+23h] END;
  RETURN r;
END read_sec;

PROCEDURE init_drv;
  VAR
    i  : INTEGER;
BEGIN
  out(csr,{7,3,1,0});
  cmd:={};
  out(CMD,{});
  FOR i:=0 TO 1 DO
    ctrk[i]:=-1;
    spec[i].heads:=heads_no;
    spec[i].tracks:=tracks_no;
    spec[i].secno:=secs_no;
  END;
END init_drv;

PROCEDURE read_block(block: INTEGER; a: ADDRESS; VAR error: INTEGER);
  VAR r,r0: BOOLEAN; t,s: INTEGER;
BEGIN
  IF NOT inited THEN
    init_drv; inited:=TRUE;
  END;
  error:=0;
  r:=FALSE;
  FOR s:=block*8 TO block*8+7 DO
    t:=tries;
    REPEAT
      r0:=read_sec(unit,s,a);
      DEC(t);
      IF r0 & seek0(unit) THEN END;
    UNTIL NOT r0 OR (t=0);
    r:=r OR r0; INC(a,128);
  END;
  IF r THEN error:=INTEGER(r) END;
END read_block;

BEGIN
  inited:=FALSE;
  boot_device:='dk0';
  boot_device[2]:=CHAR(ORD('0')+unit);
END bootDSK.
