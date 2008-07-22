MODULE cold_fd; (* 01-Feb-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD ;
IMPORT  mcd: mCodeMnem;

PROCEDURE load;

MODULE fd;

IMPORT ADDRESS, ADR, WORD, mcd;
EXPORT read_fd, fd_ready?;

PROCEDURE SETM(m: BITSET); CODE mcd.setm END SETM;
PROCEDURE GETM(): BITSET;  CODE mcd.getm END GETM;
PROCEDURE MOVE(t,f: ADDRESS; sz: INTEGER); CODE mcd.move END MOVE;

CONST
---------------------------------------------------------------------
  buffer   = 0F6000h;   -- адрес буфера размером 8К на И41
  unit     = 0;
  tries    = 4;
  track_no = 80;
  head_no  = 2;
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
  absent   : BOOLEAN;

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
      FOR ttt:=0 TO 999 DO END;
      s:=BITSET(inp(csr));
      DEC(t);
      IF t=0 THEN RETURN END;
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
    exec({4,3,2},100);
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
  cdrv:=-1;
  FOR i:=0 TO HIGH(ctrk) DO ctrk[i]:=-1 END;
  FOR i:=0 TO HIGH(spec) DO
    WITH spec[i] DO
      tracks:=track_no; heads:=head_no; secno:=5; secsize:=1024;
      restrk:=0; status:={};
    END;
  END;
  out(sel,0);
  out(sel,{1,6});
  out(sel,{1,6,2});
  IF inp(seg)=0 THEN END; -- запуск двигателя
  v_seg:=BITSET(buffer DIV (1024*64));
  out(seg,v_seg);
  buf:=ADDRESS(0F00000h+buffer DIV 4);
  FOR i:=0 TO HIGH(buf^) DO buf^[i]:=0c END;
  WHILE 0 IN BITSET(inp(csr)) DO END;
  out(cyl,01);
  absent:=inp(cyl)#1;
  IF absent THEN RETURN END;
  out(sel,4h);
  out(sel,{2});
  out(csr,{7,6,4});
END init_driver;

PROCEDURE fd_ready?(): BOOLEAN;
BEGIN
  IF absent THEN RETURN FALSE END;
  IF NOT read(unit,0,buf,spec[unit].secsize) THEN RETURN TRUE END;
  IF NOT read(unit,0,buf,spec[unit].secsize) THEN RETURN TRUE END;
  RETURN FALSE;
END fd_ready?;

PROCEDURE read_fd(block: INTEGER; a: ADDRESS; VAR error: INTEGER);
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
END read_fd;

BEGIN
  init_driver;
END fd;

MODULE wd;

IMPORT ADR, ADDRESS, WORD, mcd;
EXPORT read_wd;

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
  i:=1000;
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

PROCEDURE read_wd(block: INTEGER; a: ADDRESS; VAR error: INTEGER);
  VAR r: BOOLEAN; t,s: INTEGER;
BEGIN
  IF NOT inited THEN
    init_drv; inited:=TRUE;
  END;
  error:=0;
  FOR s:=block*8 TO block*8+7 DO
    t:=tries;
    REPEAT
      r:=read_sec(unit,s,a);
      DEC(t);
      IF r & seek0(unit) THEN END;
    UNTIL NOT r OR (t=0);
    IF r THEN error:=INTEGER(r); RETURN END;
    INC(a,128);
  END;
END read_wd;

BEGIN
  inited:=FALSE;
END wd;

MODULE tty;

IMPORT WORD, ADDRESS;

EXPORT Write, Read, BusyRead;

TYPE
  cs=SET OF (TxEN,RxIE,RxEN,SBRK,ER,TxIE,RESET,EH);
  ss=SET OF (TxRDY,RxRDY,TxE,PE,OE,FE,SINDET,DSR);

CONST
  CR   = 15c;
  LF   = 12c;
  NL   = 36c;
  csr  = 0Ah<<30;
  dtr  = 08h<<30;
  tcmd = 1Eh<<30;
  tcnt = 18h<<30;

VAR
  cmd : cs;

CONST
(* MODE *)
  x1  = {0};            bits5 = {};             odd     = {4};
  x16 = {1};            bits6 = {2};            even    = {4,5};
  x64 = {0,1};          bits7 = {3};            stop1   = {6};
                        bits8 = {2,3};          stop1_5 = {7};
                                                stop2   = {6,7};

PROCEDURE inp(n: INTEGER): WORD; CODE 92h END inp;
PROCEDURE out(n: INTEGER; v: WORD); CODE 93h END out;

PROCEDURE Read(): CHAR;
  VAR Ch: CHAR;
BEGIN
  LOOP
    IF RxRDY IN ss(inp(csr)) THEN
      Ch:=CHAR(inp(dtr));
      out(csr,cmd+cs{ER});
      RETURN CHAR(BITSET(Ch)*{0..6});
    END;
  END
END Read;

PROCEDURE BusyRead(): CHAR;
  VAR Ch: CHAR;
BEGIN
  IF RxRDY IN ss(inp(csr)) THEN
    Ch:=CHAR(inp(dtr));
    out(csr,cmd+cs{ER});
    RETURN CHAR(BITSET(Ch)*{0..6});
  ELSE
    RETURN 0c;
  END;
END BusyRead;

PROCEDURE Write(ch: CHAR);
  PROCEDURE put(c: CHAR);
  BEGIN
    REPEAT UNTIL TxRDY IN ss(inp(csr));
    out(dtr,c);
  END put;
BEGIN
  IF ch=NL THEN
    put(CR); put(LF);
  ELSE
    put(CHAR(BITSET(ch)*{0..6}));
  END;
END Write;

PROCEDURE ini_tty;
  VAR i,n: INTEGER; r: BOOLEAN; a: ADDRESS;
BEGIN
  out(tcmd,{4,5,2,1});
  out(tcnt,13);
  out(tcnt,0);
  out(csr,{});
  out(csr,{});
  out(csr,{});
  out(csr,{});
  out(csr,cs{RESET});
  out(csr,x16+stop2+bits8);
  cmd:=cs{RxEN,RxIE,TxEN};
  out(csr,cmd);
END ini_tty;

BEGIN
  ini_tty;
END tty;

MODULE PrintFmt;

IMPORT WORD, Write;
EXPORT print;

PROCEDURE write(p: WORD; VAL s: ARRAY OF CHAR; len: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO len-1 DO Write(s[i]) END;
END write;

PROCEDURE format
  (p: WORD; fmt: ARRAY OF CHAR; SEQ args: WORD);

  TYPE flags  = SET OF (lj,sg,zr,bs,sp,cap,w1,w2,cn);

  CONST CAP_DIG = "0123456789ABCDEF";
        SML_DIG = "0123456789abcdef";

  CONST max=127;

  VAR out       : ARRAY [0..max] OF CHAR;
      buf       : ARRAY [0..255] OF CHAR;
      bcnt,bptr : INTEGER;
      ocnt,fcnt : INTEGER;
      acnt      : INTEGER;
      ch        : CHAR;
      base      : CHAR;
      val       : WORD;
      wd1,wd2   : INTEGER;
      flg       : flags;


  PROCEDURE Ovr;
  BEGIN
    buf:="*** print_error: more then 256 bytes for argument ***";
    bptr:=0; bcnt:=53;
  END Ovr;

  PROCEDURE appInt;
    VAR j,v: INTEGER; sig: BOOLEAN;
  BEGIN
    v:=val; sig:=v<0; v:=ABS(v);
    bcnt:=HIGH(buf)+1; bptr:=bcnt;
    IF NOT (w2 IN flg) THEN wd2:=1 END;
    IF bptr-wd2<1 THEN wd2:=bptr-1 END;
    WHILE (wd2>0) OR (v#0) DO
      j:=v MOD 10; v:=v DIV 10; DEC(wd2);
      DEC(bptr); buf[bptr]:=CHAR(j+ORD('0'));
    END;
    IF (zr IN flg) & (w1 IN flg) THEN j:=bcnt-wd1;
      IF j<0 THEN j:=0 END;
      IF ( flags{sp,sg}*flg # flags{} ) OR sig THEN INC(j) END;
      WHILE j<bptr DO DEC(bptr); buf[bptr]:='0' END;
    END;
    IF sp IN flg THEN
      DEC(bptr); IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:=' ' END;
    ELSIF sg IN flg THEN
      DEC(bptr); IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:='+' END;
    ELSIF sig THEN
      DEC(bptr); buf[bptr]:='-';
    END;
  END appInt;

  PROCEDURE appCar(n: INTEGER);
    VAR j: INTEGER; m: BITSET;
      dig: ARRAY [0..16] OF CHAR;
  BEGIN
    IF cap IN flg THEN dig:=CAP_DIG ELSE dig:=SML_DIG END;
    m:=BITSET(INTEGER(1<<n)-1);
    bcnt:=HIGH(buf)+1; bptr:=bcnt;
    IF bs IN flg THEN DEC(bptr); buf[bptr]:=base END;
    IF NOT (w2 IN flg) THEN wd2:=1 END;
    IF bptr-wd2<1 THEN wd2:=bptr-1 END;
    WHILE (wd2>0) OR (INTEGER(val)#0) DO
      j:=INTEGER(BITSET(val)*m);
      DEC(bptr); buf[bptr]:=dig[j];
      val:=(BITSET(val)-m)>>n; DEC(wd2);
    END;
    IF (zr IN flg) & (w1 IN flg) THEN
      j:=bcnt-wd1; IF j<0 THEN j:=0 END;
      WHILE j<bptr DO DEC(bptr); buf[bptr]:='0' END;
    END;
  END appCar;

  PROCEDURE appStr;
    VAR ptr: POINTER TO ARRAY [0..0FFFFFh] OF CHAR;
          i: INTEGER;
  BEGIN
    ptr:=val; bptr:=0; bcnt:=0;
    IF ptr=NIL THEN RETURN END;
    IF flags{w1,w2}*flg=flags{} THEN i:=0;
      WHILE ptr^[i]#0c DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=ptr^[i]; INC(ocnt); INC(i);
      END;
      RETURN
    END;
    IF w2 IN flg THEN
      WHILE (bcnt<=HIGH(buf)) & (ptr^[bcnt]#0c) & (wd2>0) DO
        buf[bcnt]:=ptr^[bcnt]; INC(bcnt); DEC(wd2);
      END;
    ELSE
      WHILE (bcnt<=HIGH(buf)) & (ptr^[bcnt]#0c) DO
        buf[bcnt]:=ptr^[bcnt]; INC(bcnt)
      END;
    END;
  END appStr;

  PROCEDURE appSet;

    VAR  i,Cou: INTEGER;
         Emp  : BOOLEAN;

    PROCEDURE appNum(n: INTEGER);
    BEGIN
      IF n DIV 10 > 0 THEN appNum(n DIV 10) END;
      buf[bcnt]:=CHAR(n MOD 10 + ORD('0')); INC(bcnt);
    END appNum;

  BEGIN
    Cou:=0;  Emp:=TRUE;   bcnt:=1; bptr:=0; buf[0]:='{';
    i:=0;
    REPEAT
      IF i IN BITSET(val) THEN
        INC(Cou)
      ELSIF Cou>0 THEN
        IF NOT Emp THEN buf[bcnt]:=','; INC(bcnt) END;
        IF Cou=1 THEN
          appNum(i-1);
        ELSE
          appNum(i-Cou);
          buf[bcnt]:='.'; INC(bcnt); buf[bcnt]:='.'; INC(bcnt);
          appNum(i-1);
        END;
        Cou:=0; Emp:=FALSE;
      END;
      i:=i+1;
    UNTIL i>32;
    buf[bcnt]:='}'; INC(bcnt);
  END appSet;

  PROCEDURE appFloatF;
    VAR i,j,e: INTEGER; sig: BOOLEAN; r: REAL;
  BEGIN
    sig:=31 IN BITSET(val); val:=BITSET(val)-{31};
    IF (w2 IN flg) & (wd2=0) THEN
      IF bs IN flg THEN
        bptr:=HIGH(buf); bcnt:=bptr+1; buf[bptr]:='.';
      ELSE
        bptr:=HIGH(buf)+1; bcnt:=bptr;
      END;
    ELSE
      IF NOT (w2 IN flg) THEN wd2:=6 END;
      bptr:=HIGH(buf)-wd2; bcnt:=HIGH(buf)+1;
      IF bptr<1 THEN Ovr; RETURN END;
      buf[bptr]:='.'; r:=REAL(val);
      IF r>1.e+9 THEN r:=0. ELSE r:=r-FLOAT(TRUNC(r)) END;
      FOR i:=1 TO wd2 DO
        r:=r*10.0; j:=TRUNC(r); ASSERT((j>=0)&(j<=9));
        buf[bptr+i]:=CHAR(ORD('0')+j);
        r:=r-FLOAT(j);
      END;
    END;
    r:=REAL(val);
    IF r<1.0 THEN
      DEC(bptr); IF bptr<1 THEN Ovr; RETURN END;
      buf[bptr]:='0';
    ELSE
      e:=0;
      REPEAT r:=r/10.; DEC(bptr); INC(e) UNTIL r<1.0;
      IF bptr<1 THEN Ovr; RETURN END;
      FOR i:=0 TO e-1 DO
        r:=r*10.0; j:=TRUNC(r); r:=r-FLOAT(j);
        buf[bptr+i]:=CHAR(ORD('0')+j);
      END;
    END;
    IF (zr IN flg)&(w1 IN flg) THEN
      j:=bcnt-wd1; IF j<1 THEN j:=1 END;
      WHILE j<bptr DO DEC(bptr); buf[bptr]:='0' END;
    END;
    ASSERT(bptr>=1); -- оставлено место для знака
    IF sg IN flg THEN
      DEC(bptr);
      IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:='+' END;
    ELSIF sp IN flg THEN
      DEC(bptr);
      IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:=' ' END;
    ELSIF sig THEN
      DEC(bptr);  buf[bptr]:='-';
    END;
  END appFloatF;

  PROCEDURE appFloatE;
    VAR sig: BOOLEAN; r: REAL; i,j,e: INTEGER;
  BEGIN
    sig:=31 IN BITSET(val); val:=BITSET(val)-{31};
    bptr:=0; bcnt:=0;
    IF sg IN flg THEN
      IF sig THEN buf[bcnt]:='-' ELSE buf[bcnt]:='+' END; INC(bcnt);
    ELSIF sp IN flg THEN
      IF sig THEN buf[bcnt]:='-' ELSE buf[bcnt]:=' ' END; INC(bcnt);
    ELSIF sig THEN
      buf[bcnt]:='-'; INC(bcnt);
    END;
    r:=REAL(val); e:=0;
    WHILE r>=10. DO INC(e); r:=r/10. END;
    WHILE (r< 1.0) & (e>-50) DO DEC(e); r:=r*10. END;
    IF e<=-50 THEN e:=0; r:=0. END;
    i:=TRUNC(r); r:=r-FLOAT(i);
    buf[bcnt]:=CHAR(ORD('0')+i); INC(bcnt);
    IF (w2 IN flg) & (wd2=0) THEN
      IF bs IN flg THEN buf[bcnt]:='.'; INC(bcnt) END;
    ELSE
      IF NOT (w2 IN flg) THEN wd2:=6 END;
      IF wd2>(HIGH(buf)-bcnt-8) THEN wd2:=HIGH(buf)-bcnt-8 END;
      buf[bcnt]:='.'; INC(bcnt);
      FOR i:=1 TO wd2 DO
        r:=r*10.0; j:=TRUNC(r); r:=r-FLOAT(j);
        ASSERT((j>=0)&(j<=9)); buf[bcnt]:=CHAR(ORD('0')+j); INC(bcnt);
      END;
    END;
    IF cap IN flg THEN buf[bcnt]:='E' ELSE buf[bcnt]:='e' END; INC(bcnt);
    IF e<0 THEN buf[bcnt]:='-'; e:=ABS(e) ELSE buf[bcnt]:='+' END; INC(bcnt);
    buf[bcnt]:=CHAR(ORD('0')+(e DIV 10)); INC(bcnt);
    buf[bcnt]:=CHAR(ORD('0')+(e MOD 10)); INC(bcnt);
    ASSERT(bcnt<=HIGH(buf)+1);
  END appFloatE;

  PROCEDURE appFloatG;
    VAR r: REAL; e: INTEGER;
  BEGIN
    r:=ABS(REAL(val)); e:=0;
    IF r#0.0 THEN
      WHILE r>=10. DO INC(e); r:=r/10. END;
      WHILE (r<1.0) & (e>-40) DO DEC(e); r:=r*10. END;
    END;
    IF NOT (w2 IN flg) THEN wd2:=6; INCL(flg,w2) END;
    IF (e<-4) OR (e>=wd2) THEN
      DEC(wd2); appFloatE;
    ELSE
      wd2:=wd2-e-1; appFloatF;
      IF NOT (bs IN flg) & (wd2#0) THEN
        WHILE buf[bcnt-1]='0' DO DEC(bcnt) END;
        IF buf[bcnt-1]='.' THEN DEC(bcnt) END;
      END;
    END;
  END appFloatG;

  PROCEDURE Next;
  BEGIN
    IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
  END Next;

  PROCEDURE SubFormat(): BOOLEAN;

    PROCEDURE width(n: INTEGER);
    BEGIN
      IF w2 IN flg THEN wd2:=n ELSE INCL(flg,w1); wd1:=n END;
    END width;

    PROCEDURE scan_format;
      VAR n: INTEGER; done: BOOLEAN;
    BEGIN wd1:=0; wd2:=0; flg:=flags{}; done:=FALSE;
      REPEAT
        CASE ch OF
        |'#': INCL(flg,bs);            |'+': INCL(flg,sg);
        |'$': INCL(flg,zr);            |' ': INCL(flg,sp);
        |'-': INCL(flg,lj);            |'.': INCL(flg,w2);
        |'|': INCL(flg,cn);
        |'*': IF acnt<=HIGH(args) THEN width(args[acnt]); INC(acnt) END;
        |'0'..'9':
              n:=ORD(ch)-ORD('0'); Next;
              IF (n=0) & ('0'<=ch) & (ch<='9') & NOT (w2 IN flg) THEN
                INCL(flg,zr)
              END;
              WHILE ('0'<=ch) & (ch<='9') DO
                n:=n*10+ORD(ch)-ORD('0'); Next
              END;
              width(n); fcnt:=fcnt-1;
        ELSE  base:=ch; done:=TRUE
        END;
        IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
      UNTIL done;
    END scan_format;

    VAR spaces: INTEGER;

    PROCEDURE left_justify;
    BEGIN
      WHILE bptr<bcnt DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=buf[bptr]; INC(ocnt); INC(bptr);
      END;
      WHILE spaces>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(spaces)
      END;
    END left_justify;

    PROCEDURE right_justify;
    BEGIN
      WHILE spaces>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(spaces);
      END;
      WHILE bptr<bcnt DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=buf[bptr]; INC(ocnt); INC(bptr);
      END;
    END right_justify;

    PROCEDURE center;
      VAR tail: INTEGER;
    BEGIN
      tail:=spaces DIV 2; spaces:=spaces-tail;
      WHILE spaces>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(spaces);
      END;
      WHILE bptr<bcnt DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=buf[bptr]; INC(ocnt); INC(bptr);
      END;
      WHILE tail>0 DO
        IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
        out[ocnt]:=' '; INC(ocnt); DEC(tail);
      END;
    END center;

  BEGIN
    scan_format;
    (* Take argument *)
    IF acnt<=HIGH(args) THEN
      val:=args[acnt]; INC(acnt)
    ELSE (* misstake *)
      RETURN TRUE
    END;
    IF (base>='A') & (base<='Z') THEN
      base:=CHAR(ORD(base)+40b)
    ELSE
      INCL(flg,cap)
    END;
    CASE base OF
     |'d': appInt;      |'h': appCar(4);
     |'f': appFloatF;   |'x': appCar(4);
     |'e': appFloatE;   |'b': appCar(3);
     |'g': appFloatG;   |'o': appCar(3);
     |'s': appStr;      |'c': bcnt:=1; bptr:=0; buf[0]:=CHAR(val);
     |'{': IF ch='}' THEN Next; appSet ELSE DEC(acnt); RETURN TRUE END;
    ELSE (* illegal base, unget argument *)
      DEC(acnt); RETURN TRUE;
    END;
    ASSERT(bcnt<=HIGH(buf)+1);
    ASSERT(bptr>=0);
    IF    w1 IN flg THEN spaces:=wd1-bcnt+bptr ELSE spaces:=0 END;
    IF    lj IN flg THEN  left_justify
    ELSIF cn IN flg THEN  center
    ELSE                 right_justify
    END;
    RETURN FALSE;
  END SubFormat;

  PROCEDURE Format(): BOOLEAN;
    VAR f,a: INTEGER;
  BEGIN
    f:=fcnt; a:=acnt;
    IF SubFormat() THEN fcnt:=f; acnt:=a; ch:=fmt[fcnt-1]; RETURN TRUE END;
    RETURN FALSE;
  END Format;

BEGIN
  IF HIGH(fmt)<=0 THEN RETURN END;
  ch:=fmt[0]; fcnt:=1; ocnt:=0; acnt:=0;
  REPEAT
    IF ch='\' THEN
      (*Next:*)
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
      IF ocnt>=max-1 THEN out[ocnt]:=0c; write(p,out,ocnt); ocnt:=0 END;
      IF     ch='n' THEN ch:=12c;
        out[ocnt]:=15c; INC(ocnt);
      ELSIF  ch='r' THEN ch:=15c;
      ELSIF  ch='l' THEN ch:=12c;
      ELSIF  ch='g' THEN ch:=07c;
      ELSIF  ch='\' THEN ch:='\';
      ELSIF  ch=0c  THEN ch:='\';
      ELSE
        out[ocnt]:='\'; INC(ocnt);
      END;
      out[ocnt]:=ch; INC(ocnt);
      (*Next:*)
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
    ELSIF ch='%' THEN
      (*Next:*)
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
      IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
      IF (ch=0c) OR (ch='%') THEN
        out[ocnt]:='%'; INC(ocnt); Next;
      ELSIF Format() THEN
        out[ocnt]:='%'; INC(ocnt);
      END;
    ELSE
      IF ocnt>=max THEN out[max]:=0c; write(p,out,max); ocnt:=0 END;
      out[ocnt]:=ch; INC(ocnt);
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
    END;
  UNTIL ch=0c;
  out[ocnt]:=0c; write(p,out,ocnt);
END format;

PROCEDURE print(fmt: ARRAY OF CHAR; SEQ arg: WORD);
BEGIN
  format(0,fmt,arg);
END print;

END PrintFmt;

PROCEDURE consol;
  PROCEDURE inp(n: INTEGER): INTEGER; CODE 90h END inp;
  PROCEDURE out(n,v: INTEGER); CODE 91h END out;
  VAR
    adr : ADDRESS;
    dig_no,val,io_adr: INTEGER;
    ch  : CHAR;
    io  : BOOLEAN;
BEGIN
  print('Consol program for Kronos-P2.6 processor.\n');
  adr:=0; io_adr:=0; io:=FALSE;
  LOOP
    adr:=ADDRESS(BITSET(adr)*{0..23});
    IF io THEN
      print('io  %$8h=%$4h ',io_adr,inp(io_adr<<30));
    ELSE
      print('%$8h=%$8h ',adr,adr^);
    END;
    dig_no:=0; val:=0;
    LOOP
      ch:=Read();
      CASE ch OF
        |'0'..'9':
          val:=INTEGER(BITSET(val<<4)-{0..3}+BITSET(ch)*{0..3});
          INC(dig_no); print('%c',ch);
        |'A'..'F':
          val:=INTEGER(BITSET(val<<4)-{0..3}+BITSET(ORD(ch)-7)*{0..3});
          INC(dig_no); print('%c',ch);
        |'a'..'f':
          val:=INTEGER(BITSET(val<<4)-{0..3}+BITSET(ORD(ch)-47b)*{0..3});
          INC(dig_no); print('%c',ORD(ch)-40b);
        |12c:
          IF io THEN
            IF dig_no#0 THEN out(io_adr<<30,val) END; INC(io_adr);
          ELSE
            IF dig_no#0 THEN adr^:=val END; INC(adr);
          END;
          EXIT
        |15c:
          IF io THEN
            IF dig_no#0 THEN out(io_adr<<30,val) END; DEC(io_adr);
          ELSE
            IF dig_no#0 THEN adr^:=val END; DEC(adr);
          END;
          EXIT
        |'/':
          IF io THEN
            IF dig_no#0 THEN io_adr:=val ELSE print('?') END;
          ELSE
            IF dig_no#0 THEN adr:=val ELSE adr:=adr^ END;
          END;
          print('\n'); EXIT
        |'.': io:=NOT io; print('\n'); EXIT;
        |'Q': print('\nQuit\n'); RETURN;
      ELSE
        print('%c?',ch); EXIT
      END;
    END;
    print('\n');
  END;
END consol;

PROCEDURE di; CODE 0 mcd.setm END di;

VAR
  err: INTEGER;
  fd?: BOOLEAN;

BEGIN
  di;
  print('\nKronos-P2.6 cold loader.\n');
  LOOP
    fd?:=fd_ready?();
    IF fd? THEN
      read_fd(0,0,err);
      IF err#0 THEN
        print('fd read error %d.\n',err);
      ELSIF BusyRead()#0c THEN
        consol;
      ELSE
        EXIT
      END;
    ELSE
      print('Floppy disk not ready.\n');
      IF BusyRead()#0c THEN
        consol;
      ELSE
        read_wd(0,0,err);
        IF err#0 THEN
          print('wd read error %d.\n',err);
        ELSIF BusyRead()#0c THEN
          consol;
        ELSE
          EXIT
        END;
      END;
    END;
  END;
END load;

PROCEDURE start; CODE 0 1 mcd.tra END start;

BEGIN
  load;
  start;
END cold_fd.
