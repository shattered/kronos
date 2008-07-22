IMPLEMENTATION MODULE bootDSK; (* 27-Jan-90. (c) KRONOS *)


FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD ;
IMPORT  mcd: mCodeMnem;

PROCEDURE SETM(m: BITSET); CODE mcd.setm END SETM;
PROCEDURE GETM(): BITSET;  CODE mcd.getm END GETM;
PROCEDURE MOVE(t,f: ADDRESS; sz: INTEGER); CODE mcd.move END MOVE;

CONST
---------------------------------------------------------------------
  buffer   = 0F6000h;   -- адрес буфера размером 8К на И41
  unit     = 0;
  tries    = 4;
  track_no = 40;
---------------------------------------------------------------------

  csr = 5Ch<<30;
  cyl = 59h<<30;
  rsc = 5Ah<<30;
  dtr = 5Bh<<30;
  dma_adr0 = 50h<<30;
  dma_cnt0 = 51h<<30;
  dma_adr1 = 52h<<30;
  dma_cnt1 = 53h<<30;
  dma_adr2 = 54h<<30;
  dma_cnt2 = 55h<<30;
  dma_adr3 = 56h<<30;
  dma_cnt3 = 57h<<30;
  dma_csr  = 58h<<30;
  seg = 5Eh<<30;
  sel = 5Fh<<30;

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
  buf    : POINTER TO ARRAY [0..1024*8-1] OF CHAR;
  ctrk   : ARRAY [0..1] OF INTEGER;
  cdrv   : INTEGER;
  spec   : ARRAY [0..HIGH(ctrk)] OF RECORD
    tracks : INTEGER;
    secno  : INTEGER;
    secsize: INTEGER;
    heads  : INTEGER;
    restrk : INTEGER;
    status : BITSET;
  END;
  v_seg    : BITSET;
  inited   : BOOLEAN;

PROCEDURE out0(a,v: WORD); CODE 93h END out0;
PROCEDURE inp0(a: INTEGER): WORD; CODE 92h END inp0;

PROCEDURE out(a,v: WORD);
BEGIN
  IF 30 IN BITSET(a) THEN out0(a,v<<8) ELSE out0(a,v) END;
END out;

PROCEDURE inp(a: INTEGER): WORD;
BEGIN
  IF 30 IN BITSET(a) THEN RETURN inp0(a)>>8 ELSE RETURN inp0(a) END;
END inp;

PROCEDURE exec(cmd: BITSET; t: INTEGER);
  VAR ttt: INTEGER; s: BITSET;
BEGIN
  out(seg,v_seg);
  out(csr,cmd);
  REPEAT
    REPEAT
      FOR ttt:=0 TO 99 DO END;
      s:=BITSET(inp(csr));
    UNTIL NOT (0 IN s);
    s:=BITSET(inp(csr));
  UNTIL NOT (0 IN s);
END exec;

PROCEDURE select(d: INTEGER);
-- обеспечивает выбор устройства и разгон шпинделя
  VAR ei,s: BITSET; i: INTEGER;
BEGIN
  ei:=GETM(); SETM(ei-{0..1});
  IF inp(seg)=0 THEN END; -- запуск двигателя
  IF cdrv=d THEN SETM(ei); RETURN END;
  s:={2};
  out(sel,s);
  cdrv:=d;
  s:=s+BITSET(d+2)+{d+6};
  out(sel,s);
  FOR i:=0 TO 20000 DO END; -- ожидание разгона шпинделя
  IF ctrk[d]>=0 THEN out(cyl,ctrk[d]) END;
  IF inp(seg)=0 THEN END; -- запуск двигателя
  SETM(ei);
END select;

PROCEDURE seek00(drv: INTEGER): BOOLEAN;
BEGIN
  select(drv);
  exec({3,1,0},150); -- recalibrate
  IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
  IF 7 IN BITSET(inp(csr)) THEN RETURN NoVolume END;
  IF NOT (2 IN BITSET(inp(csr))) THEN RETURN NotFoundTrack0 END;
  ctrk[cdrv]:=0; out(cyl,0);
  RETURN FALSE;
END seek00;

PROCEDURE step(drv: INTEGER): BOOLEAN;
BEGIN
  select(drv);
  exec({6,4,3,1,0},50); -- step forward
  IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
  IF 7 IN BITSET(inp(csr)) THEN RETURN NoVolume END;
  INC(ctrk[cdrv]);
  IF ctrk[cdrv]#inp(cyl) THEN RETURN SeekError END;
  RETURN FALSE;
END step;

PROCEDURE move_head(drv,trk: INTEGER): BOOLEAN;
  VAR t: INTEGER; r: BOOLEAN;
BEGIN
  select(drv);
  IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
  IF trk#ctrk[cdrv] THEN
    IF ctrk[cdrv]<0 THEN
      r:=seek00(drv); IF r THEN RETURN r END;
    END;
    out(cyl,ctrk[cdrv]);
    out(dtr,trk);
    exec({4,3,2},200);
    ctrk[cdrv]:=-1;
    IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
    IF 7 IN BITSET(inp(csr)) THEN RETURN NoVolume END;
    IF 4 IN BITSET(inp(csr)) THEN RETURN NotFoundSID END;
    IF (trk=0) & NOT (2 IN BITSET(inp(csr))) THEN RETURN NotFoundTrack0 END;
    ctrk[cdrv]:=trk;
    out(cyl,trk);
  END;
  RETURN FALSE;
END move_head;

PROCEDURE fd_buf;
BEGIN
  out(dma_csr,0h);
  out(dma_adr2,buffer);
  out(dma_adr2,buffer>>8);
  out(dma_cnt2,0);
  out(dma_cnt2,54h);
  out(seg,v_seg);
  out(dma_csr,4h);
END fd_buf;

PROCEDURE read(drv,blk: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR trk,side,sec,i,j,ttt: INTEGER; r: BOOLEAN; end: ADDRESS;
      ptr: POINTER TO ARRAY [0..4095] OF CHAR; s: BITSET;
BEGIN
  IF (drv<0) OR (drv>HIGH(spec)) THEN RETURN NoSuchDrive END;
  WITH spec[drv] DO
    sec :=blk*(4096 DIV secsize);
    trk :=sec DIV secno;
    side:=trk MOD heads;
    trk :=trk DIV heads + restrk;
    sec :=sec MOD secno;
    LOOP
      r:=move_head(drv,trk); IF r THEN RETURN r END;
      IF side>0 THEN INCL(v_seg,6) ELSE EXCL(v_seg,6) END;
      fd_buf;
      i:=(sz+secsize-1) DIV secsize;
      IF i>secno-sec THEN i:=secno-sec END;
      j:=i;
      out(rsc,sec+1);
      INC(sec,i);
      REPEAT
        IF side=0 THEN out(csr,{7}) ELSE out(csr,{7,3}) END;
        REPEAT
          REPEAT
            FOR ttt:=0 TO 99 DO END;
            s:=BITSET(inp(csr));
          UNTIL NOT (0 IN s);
          s:=BITSET(inp(csr));
        UNTIL NOT (0 IN s);
        IF 7 IN s THEN RETURN NoVolume END;
        IF s*{1,2}#{} THEN RETURN IllegalDMA END;
        IF s*{4,5,3}#{} THEN RETURN ReadError END;
        out(rsc,INTEGER(inp(rsc))+1);
        DEC(i);
      UNTIL i=0;
      j:=secsize DIV 4 * j;
      MOVE(a,buf,j);
      a:=a+j;
      DEC(sz,j*4);
      IF (inp(rsc)#sec+1) OR (BITSET(inp(csr))*{4,5,3}#{}) THEN
        RETURN ReadError END;
      IF sz<=0 THEN EXIT END;
      sec:=0;
      INC(side);
      IF side=heads THEN side:=0; INC(trk) END;
    END;
  END;
  RETURN FALSE;
END read;

PROCEDURE init_driver;
  VAR adr: ADDRESS; i,j: INTEGER;
BEGIN
  out(sel,0);
  out(sel,{1,6});
  out(sel,{1,6,2});
  v_seg:=BITSET(buffer DIV (1024*64));
  out(seg,v_seg);
  buf:=ADDRESS(0F00000h+buffer DIV 4);
  FOR i:=0 TO HIGH(buf^) DO buf^[i]:=0c END;
  WHILE 0 IN BITSET(inp(csr)) DO END;
  out(sel,4h);
  cdrv:=-1;
  FOR i:=0 TO HIGH(ctrk) DO ctrk[i]:=-1 END;
  out(sel,{2});
  out(csr,{7,6,4});
  FOR i:=0 TO HIGH(spec) DO
    WITH spec[i] DO
      tracks:=track_no; heads:=1; secno:=5; secsize:=1024;
      restrk:=0; status:={};
    END;
  END;
END init_driver;

PROCEDURE read_block(block: INTEGER; a: ADDRESS; VAR error: INTEGER);
  VAR r: BOOLEAN; t: INTEGER;
BEGIN
  IF NOT inited THEN
    init_driver; inited:=TRUE;
  END;
  error:=0;
  t:=tries;
  REPEAT
    r:=read(unit,block,a,4096); DEC(t);
    IF r THEN ctrk[unit]:=-1 END;
  UNTIL NOT r OR (t=0);
  IF r THEN error:=INTEGER(r) END;
END read_block;

BEGIN
  inited:=FALSE;
  boot_device:='fd0';
  boot_device[2]:=CHAR(ORD('0')+unit);
END bootDSK.
