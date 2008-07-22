MODULE wd; (* 04-Dec-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM Scheduler  IMPORT  Gate, InitGate, EnterGate, ExitGate, ?1, ?0, MyTask,
                        ProcessId, Wait, TimeSignal, Sendings, Signal,
                        InitSignal, Send, Awaited, InsertAction,
                        RemoveAction, new_process;
FROM FsDrv      IMPORT  IamDriver, dMode, RemDriver;
FROM Terminal   IMPORT  print, Read;
FROM Resource   IMPORT  Final;

IMPORT fp  : FsPublic;
IMPORT mcd : mCodeMnem;

CONST
  step_time=400;

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

VAR
  ctrk : ARRAY [0..1] OF INTEGER;
  cmd  : BITSET;
  spec : ARRAY [0..1] OF RECORD
    heads : INTEGER;
    tracks: INTEGER;
    secno : INTEGER;
  END;
  Name : ARRAY [0..63] OF CHAR;
  buf  : ARRAY [0..511] OF CHAR;
  Lock : Gate;
  tries: INTEGER;

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
  IF 1 IN inp(chC) THEN RETURN fp.NotReady END;
  IF 3 IN inp(chC) THEN RETURN fp.NotReady END;
  i:=4000;
  WHILE 0 IN inp(chC) DO
    IF i=0 THEN RETURN fp.NotFoundTrack0 END;
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
    IF k=0 THEN RETURN fp.SeekError END;
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
  IF 1 IN inp(chC) THEN RETURN fp.NotReady END;
  IF 3 IN inp(chC) THEN RETURN fp.NotReady END;
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
    IF k=0 THEN RETURN fp.SeekError END;
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

PROCEDURE test_buf;
  VAR n,i,j: INTEGER;
BEGIN
  FOR n:=0 TO 15 DO
    FOR i:=0 TO 2047 DO put(i,CHAR((i+n) MOD 255)) END;
    FOR i:=0 TO 2047 DO
      j:=INTEGER(get(i));
      IF j#(i+n) MOD 255 THEN
        print('error %6h %$3h->%$3h,%$3h\n',i,(i+n) MOD 255,j,get(i));
      END;
    END;
  END;
END test_buf;

PROCEDURE print_buf;
  VAR i: INTEGER;
BEGIN
  print('\n');
  FOR i:=0 TO 2047 DO
    print(' %$2h',get(i));
    IF (i MOD 16)=15 THEN print('\n') END;
  END;
END print_buf;

PROCEDURE format(d,t: INTEGER): BOOLEAN;
  VAR
    r: BOOLEAN; sec,i,h: INTEGER; ei: BITSET;
    mem: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  ?1;
  r:=move_head(d,t);
  IF r THEN ?0; RETURN r END;
  out(CMD,{});
  EXCL(cmd,cnt); out(chA,cmd);
  h:=t MOD spec[d].heads;
  t:=t DIV spec[d].heads;
  FOR i:=0 TO 9 DO put(i,04Eh) END;
  FOR i:=0Ah TO 289h DO put(i,0) END;
  FOR i:=31h TO 230h DO put(i,04Eh) END;
  put(16h,0A1h);
  put(17h,0FEh);
  put(18h,t);
  put(19h,h);
  put(1Ah,0);
  put(2Fh,0A1h);
  put(30h,0h);
  ei:=getm();
  sec:=0;
  mem:=ADDRESS(0F37000h);
  out(CMD,40h);
  setm(ei-{0,1});
  REPEAT UNTIL 6 IN inp(chB);
  WHILE 6 IN inp(chB) DO END;
  out(CMD,7Dh);
  REPEAT
    REPEAT UNTIL 7 IN inp(chB);
    INC(sec);
    mem^[1Ah]:=CHAR(sec);
    mem^[30h]:=CHAR(sec);
    REPEAT UNTIL NOT (7 IN inp(chB));
  UNTIL sec=spec[d].secno-1;
  REPEAT UNTIL 7 IN inp(chB);
  out(CMD,5Dh);
  REPEAT UNTIL NOT (1 IN inp(chB));
  cmd:=cmd-{sel0,sel1,cnt}; out(chA,cmd);
  setm(ei);
  ?0;
  out(CMD,0);
  RETURN FALSE;
END format;

PROCEDURE read_sec(d,s: INTEGER; a: ADDRESS): BOOLEAN;
  VAR r: BOOLEAN; i,h,t: INTEGER; p: POINTER TO ARRAY [0..511] OF CHAR;
    mem0: POINTER TO ARRAY [0..4095] OF CHAR;
    mem1: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  ?1;
  t:=s DIV spec[d].secno; s:=s MOD spec[d].secno;
  r:=move_head(d,t);
  IF r THEN ?0; RETURN r END;
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
    r:=fp.DataCRC;
  ELSE
    r:=fp.NotFoundSID;
  END;
  out(CMD,0);
  p:=a;
  FOR i:=0 TO 511 BY 2 DO p^[i]:=mem0^[i+24h] END;
  FOR i:=1 TO 511 BY 2 DO p^[i]:=mem1^[i+23h] END;
  ?0;
  RETURN r;
END read_sec;

PROCEDURE write_sec(d,s: INTEGER; a: ADDRESS): BOOLEAN;
  VAR r: BOOLEAN; i,h,t: INTEGER; p: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  ?1;
  t:=s DIV spec[d].secno; s:=s MOD spec[d].secno;
  r:=move_head(d,t);
  IF r THEN ?0; RETURN r END;
  out(CMD,0);
  EXCL(cmd,cnt); out(chA,cmd);
  h:=t MOD spec[d].heads;
  t:=t DIV spec[d].heads;
  FOR i:=0 TO 27Fh DO put(i,0) END;
  p:=a;
  FOR i:=0 TO 511 DO put(i+1Ch,p^[i]) END;
  put(2h,0A1h);
  put(3h,0FEh);
  put(4h,t);
  put(5h,h);
  put(6h,s);
  put(1Ah,0A1h);
  put(1Bh,80h);
  WHILE get(80h)#p^[80h-1Ch] DO put(80h,p^[80h-1ch]) END;
  out(CMD,40h);
  INCL(cmd,cnt); out(chA,cmd);
  out(CMD,4Ch);
  REPEAT UNTIL (0 IN inp(chB)) OR NOT (1 IN inp(chB));
  cmd:=cmd-{sel0,sel1,cnt}; out(chA,cmd);
  IF NOT (1 IN inp(chB)) THEN
    r:=FALSE;
  ELSE
    r:=fp.NotFoundSID;
  END;
  out(CMD,0);
  ?0;
  RETURN r;
END write_sec;

PROCEDURE DKRead(d,b: INTEGER; Buf: ADDRESS; Sz: INTEGER): BOOLEAN;
  VAR
    r,r1: BOOLEAN; t,sec,i: INTEGER;
    p: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  EnterGate(Lock);
  r:=FALSE; sec:=b*8;
  WHILE Sz>=512 DO
    t:=tries;
    REPEAT
      r1:=read_sec(d,sec,Buf);
      IF r1 & (t=tries) & seek0(d) THEN END;
      DEC(t);
    UNTIL NOT r1 OR (t=0);
    INC(Buf,128); INC(sec); DEC(Sz,512);
    r:=r OR r1;
  END;
  IF Sz>0 THEN
    t:=tries;
    REPEAT
      r1:=read_sec(d,sec,ADR(buf));
      IF r1 & (t=tries) & seek0(d) THEN END;
      DEC(t);
    UNTIL NOT r1 OR (t=0);
    p:=Buf;
    FOR i:=0 TO Sz-1 DO p^[i]:=buf[i] END;
    r:=r OR r1;
  END;
  ExitGate(Lock);
  RETURN r;
END DKRead;

PROCEDURE DKWrite(d,b: INTEGER; Buf: ADDRESS; Sz: INTEGER): BOOLEAN;
  VAR
    r,r1: BOOLEAN; t,sec,i: INTEGER;
    p: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  EnterGate(Lock);
  r:=FALSE; sec:=b*8;
  WHILE Sz>=512 DO
    t:=tries;
    REPEAT
      r1:=write_sec(d,sec,Buf);
      IF r1 & (t=tries) & seek0(d) THEN END;
      DEC(t);
    UNTIL NOT r1 OR (t=0);
    INC(Buf,128); INC(sec); DEC(Sz,512);
    r:=r OR r1;
  END;
  IF Sz>0 THEN
    t:=tries;
    REPEAT
      r1:=read_sec(d,sec,ADR(buf));
      IF r1 & (t=tries) & seek0(d) THEN END;
      DEC(t);
    UNTIL NOT r1 OR (t=0);
    IF NOT r1 THEN
      p:=Buf;
      FOR i:=0 TO Sz-1 DO buf[i]:=p^[i] END;
      t:=tries;
      REPEAT
        r1:=write_sec(d,sec,ADR(buf));
        IF r1 & (t=tries) & seek0(d) THEN END;
        DEC(t);
      UNTIL NOT r1 OR (t=0);
    END;
    r:=r OR r1;
  END;
  ExitGate(Lock);
  RETURN r;
END DKWrite;

PROCEDURE DKFormat(d,b: INTEGER): BOOLEAN;
  VAR r: BOOLEAN; i,s,t,k: INTEGER;
BEGIN
  IF NOT (d IN {0..1}) THEN RETURN fp.NoSuchDrive END;
  IF b#0 THEN RETURN FALSE END;
  FOR i:=0 TO spec[d].heads*spec[d].tracks-1 DO
    t:=4;
    REPEAT
      r:=format(d,i);
      s:=i*spec[d].secno;
      FOR k:=s TO s+spec[d].secno-1 DO
        r:=r OR read_sec(d,k,ADR(buf));
      END;
      DEC(t);
    UNTIL NOT r OR (t=0);
    print('%6d\r',i);
  END;
  RETURN FALSE;
END DKFormat;

PROCEDURE DKCFE(d,Op: INTEGER; Info: ADDRESS): BOOLEAN;
  VAR r  : BOOLEAN;
      i,n: INTEGER;
      p  : POINTER TO ARRAY [0..15] OF CHAR;
      sp : POINTER TO RECORD h,t,s,z: INTEGER END;
BEGIN
  EnterGate(Lock);
  r:=FALSE;
  CASE Op OF
     0: Info^:=spec[d].heads*spec[d].tracks*spec[d].secno DIV 8;
    |1: p:=Info; p^:="DK6WD";
    |2: (* power off *)
    |3: tries:=Info^;
        IF tries<1 THEN tries:=1 END;
        IF tries>20 THEN tries:=20 END;
    |4:
    |5: IF Info#NIL THEN i:=Info^ ELSE i:=-1 END;
        r:=DKFormat(d,i);
    |6: IF Info=NIL THEN
          r:=fp.NoSuchCFE;
        ELSE
          sp:=Info;
          IF sp^.h>0 THEN spec[d].heads:=sp^.h END;
          IF sp^.t>0 THEN spec[d].tracks:=sp^.t END;
          IF sp^.s>0 THEN spec[d].secno:=sp^.s END;
          IF spec[d].heads>4 THEN spec[d].heads:=4 END;
          IF spec[d].secno>18 THEN spec[d].secno:=18 END;
          print('Winchester %d: %d heads, %d tracks, %d sectors per track.\n',
                 d,spec[d].heads,spec[d].tracks,spec[d].secno);
        END;
   |10:
  ELSE  r:=fp.NoSuchCFE
  END;
  ExitGate(Lock);
  RETURN r;
END DKCFE;

PROCEDURE stop;
BEGIN
  IF RemDriver(Name) THEN END;
END stop;

PROCEDURE init_drv;
  VAR
    i  : INTEGER;
    p  : ProcessId;
    r  : BOOLEAN;
    s  : ARRAY [0..79] OF CHAR;
BEGIN
  out(csr,{7,3,1,0});
  cmd:={};
  out(CMD,{});
  FOR i:=0 TO 1 DO
    ctrk[i]:=-1;
    spec[i].heads:=4;
    spec[i].tracks:=306;
    spec[i].secno:=16;
  END;
  tries:=16;
  InitGate(Lock);
  p:=MyTask();
  MOVE(ADR(Name),p^.ParmLink,1); Name[2]:=0c;
  IF Name='' THEN Name:='dk' END;
  r:=IamDriver(Name,{0,1},blocked,DKRead,DKWrite,DKCFE);
  IF r THEN fp.VisFSerr(r,s); print('%s\n',s); HALT(1) END;
  Final(stop);
  EnterGate(Lock);
--  test_buf;
  ExitGate(Lock);
END init_drv;

VAR qqq: Signal;

BEGIN
  init_drv;
  InitSignal(qqq);
  Wait(qqq);
END wd.
