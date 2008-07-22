MODULE DK6I41; (* 12-Oct-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM Scheduler  IMPORT  Gate, InitGate, EnterGate, ExitGate, ?1, ?0, MyTask,
                        ProcessId, Wait, Signal, PROCESS, new_process,
                        InitSignal, Send, InsertAction, RemoveAction;
FROM FsDrv      IMPORT  IamDriver, dMode, RemDriver;
FROM FsPublic   IMPORT  ReadError, WriteError, NoSuchDrive, WriteProtect,
                        NoSuchCFE, NoSuchBlock, NoVolume, CheckError, VisFSerr,
                        HardError, TimeOut, NotReady, NotImplemented,
                        NotFoundSID, DataCRC, HeaderCRC, SeekError;
FROM Terminal   IMPORT  print;
FROM Resource   IMPORT  Final;
FROM mCodeMnem  IMPORT  setm, getm, tra;

-- драйвер резервирует область памяти на И41 0C0000h..0C1FFFh

CONST
  VEC    = 18h;
  base   = 0EFE0h;
  port   = base<<30;
  adrPW  = base*10h;
  adrPUK = 0C0000h;
  adrPWK = adrPUK+16;
  adrPWW = adrPWK+16;
  adrWSP = adrPWW+32;
  adrBUF = adrWSP+12;
  adrFREE= adrBUF+4096;

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
  ready   : Signal;
  lock    : Gate;
  time    : INTEGER;
  time_out: BOOLEAN;
  Name    : ARRAY [0..63] OF CHAR;
  tries   : INTEGER;
  op_stat : status;
  ipted   : PROCESS;

PROCEDURE MOVE(a,b: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;
PROCEDURE out(a,v: INTEGER); CODE 93h END out;
PROCEDURE SETM(m: BITSET); CODE setm END SETM;
PROCEDURE GETM(): BITSET; CODE getm END GETM;
PROCEDURE TRANSFER(VAR fr,to: PROCESS); CODE tra END TRANSFER;

PROCEDURE ticker;
BEGIN
  IF time>0 THEN
    DEC(time);
    IF time=0 THEN
      time:=0; time_out:=puk^[1]#0c;
      puk^[1]:=0c; Send(ready);
    END;
  END;
END ticker;

PROCEDURE trap_proc;
  VAR s: status; d: INTEGER;
BEGIN
  LOOP
    out(port,0);
    IF pwk^[3]#0c THEN
      s:=status(pwk^[1]); pwk^[3]:=0c;
      IF frame IN s THEN
        d:=0;
        IF drive0 IN s THEN INC(d,1) END;
        IF drive1 IN s THEN INC(d,2) END;
        IF NOT (floppy IN s) THEN INC(d,4) END;
        spec[d].mount:=FALSE;
      END;
      IF done IN s THEN op_stat:=s; Send(ready); time:=0 END;
    END;
    TRANSFER(ipted,ipted);
  END;
END trap_proc;

PROCEDURE exec(): BOOLEAN;
BEGIN
  puk^[1]:=377c;
  out(port,1);
  InitSignal(ready);
  time_out:=FALSE;
  time:=200;
  Wait(ready);
  IF time_out THEN RETURN TimeOut END;
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
  r:=exec();
  pwk^[ 3]:=0c;
  pwk^[ 8]:=CHAR(adrPWW MOD 16);
  pwk^[ 9]:=0c;
  pwk^[10]:=CHAR(adrPWW DIV 10h);
  pwk^[11]:=CHAR(adrPWW DIV 1000h);
  pww^[0]:=0c; pww^[1]:=0c; pww^[2]:=0c; pww^[3]:=0c;
  pww^[26]:=0c; pww^[27]:=0c; pww^[28]:=0c; pww^[29]:=0c;
  RETURN r;
END reset;

PROCEDURE wrPWW(drv,cyl,head,sec,sz: INTEGER; f: func; buf: ADDRESS);
  VAR i: INTEGER;
BEGIN
  IF drv<4 THEN pww^[8]:=3c ELSE pww^[8]:=0c END;
  pww^[9]:=0c;
  pww^[10]:=CHAR(drv MOD 4);
  pww^[11]:=CHAR(f);
  pww^[12]:=0c;
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
  t:=tries;
  REPEAT
    wrPWW(d,cyl,head,sec,sz,read,a);
    r:=exec();
    IF NOT r THEN r:=err?(c) ELSE c:=FALSE END;
    DEC(t);
  UNTIL (NOT r) OR (NOT c) OR (t=0);
  RETURN r;
END read_sec;

PROCEDURE write_sec(d,sec: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR r,c: BOOLEAN; t,i,head,cyl: INTEGER;
BEGIN
  WITH spec[d] DO
    head:=sec DIV sec_no; sec:=sec MOD sec_no;
    cyl:=head DIV heads; head:=head MOD heads;
  END;
  IF d<4 THEN INC(sec) END;
  t:=tries;
  REPEAT
    wrPWW(d,cyl,head,sec,sz,write,a);
    r:=exec();
    IF NOT r THEN r:=err?(c) ELSE c:=FALSE END;
    DEC(t);
  UNTIL (NOT r) OR (NOT c) OR (t=0);
  RETURN r;
END write_sec;

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
  EnterGate(lock); ?1;
  sec:=b*4096 DIV spec[d].sec_sz;
  i41:=(a>=ADDRESS(0F00000h)) & (a<=ADDRESS(0F3FFFFh));
  IF i41 THEN
    r:=read_sec(d,sec,a,sz);
  ELSE
    r:=read_sec(d,sec,buf,sz);
    MOVE(a,buf,sz DIV 4); to:=a;
    FOR i:=INTEGER(BITSET(sz)-{0..1}) TO sz-1 DO to^[i]:=buf^[i] END;
  END;
  ExitGate(lock); ?0;
  RETURN r;
END DKRead;

PROCEDURE DKWrite(d,b: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR r,c,i41: BOOLEAN; sec,i,sec_no,tail_w,tail_r: INTEGER;
      from: POINTER TO ARRAY [0..4095] OF CHAR;
BEGIN
  IF (d<0) OR (d>HIGH(spec)) THEN RETURN NoSuchDrive END;
  EnterGate(lock); ?1;
  WITH spec[d] DO
    sec:=b*4096 DIV sec_sz;
    tail_w:=sz-(sz DIV sec_sz)*sec_sz;
    tail_r:=(sec_sz-tail_w) MOD sec_sz;
    sz:=sz-tail_w;
  END;
  i41:=(a>=ADDRESS(0F00000h)) & (a<=ADDRESS(0F3FFFFh));
  IF tail_r>0 THEN
    r:=read_sec(d,sec+sz DIV spec[d].sec_sz,
                ADDRESS(buf)+(sz DIV 4),spec[d].sec_sz);
    IF r THEN ExitGate(lock); ?0; RETURN r END;
  END;
  IF i41 THEN
    r:=write_sec(d,sec,a,sz);
    IF NOT r & (tail_w>0) THEN
      r:=write_sec(d,sec+sz DIV spec[d].sec_sz,
                   ADDRESS(buf)+(sz DIV 4),spec[d].sec_sz);
    END;
  ELSE
    MOVE(buf,a,(sz+tail_w) DIV 4); from:=a;
    FOR i:=INTEGER(BITSET(sz+tail_w)-{0..1}) TO sz+tail_w-1 DO
      buf^[i]:=from^[i]
    END;
    r:=write_sec(d,sec,buf,sz+tail_w+tail_r);
  END;
  ExitGate(lock); ?0;
  RETURN r;
END DKWrite;

PROCEDURE DKFormat(d,b: INTEGER): BOOLEAN;
  VAR r,c: BOOLEAN; head,cyl: INTEGER;
BEGIN
  IF (d<0) OR (d>HIGH(spec)) THEN RETURN NoSuchDrive END;
  spec[d].mount:=TRUE;
  IF b#0 THEN RETURN FALSE END;
  WITH spec[d] DO
    FOR cyl:=0 TO cyls-1 DO
      FOR head:=0 TO heads-1 DO
        buf^[0]:=0c;
        buf^[1]:=CHAR(0E5h);
        buf^[2]:=CHAR(0E5h);
        buf^[3]:=CHAR(0E5h);
        buf^[4]:=CHAR(0E5h);
        buf^[5]:=1c;
        wrPWW(d,cyl,head,1,6,format,buf);
        r:=exec();
        IF NOT r THEN r:=err?(c) END;
        IF r THEN RETURN r END;
      END;
    END;
  END;
  RETURN FALSE;
END DKFormat;

PROCEDURE InitDK(info: ADDRESS): BOOLEAN;
  VAR r,c: BOOLEAN;
BEGIN
  tries:=8; time:=0; op_stat:=status{};
  IF InsertAction(ticker) THEN HALT(1) END;
  IF reset() THEN RETURN TimeOut END;
  IF set_type(4) THEN RETURN TimeOut END;
  r:=err?(c); IF r & (r#NotReady) THEN RETURN r END;
  IF set_type(0) THEN RETURN TimeOut END;
  r:=err?(c); IF r & (r#NotReady) THEN RETURN r END;
  IF set_type(1) THEN RETURN TimeOut END;
  r:=err?(c); IF r & (r#NotReady) THEN RETURN r END;
  IF set_type(2) THEN RETURN TimeOut END;
  r:=err?(c); IF r & (r#NotReady) THEN RETURN r END;
  IF set_type(3) THEN RETURN TimeOut END;
  r:=err?(c); IF r & (r#NotReady) THEN RETURN r END;
  RETURN FALSE;
END InitDK;

PROCEDURE DKCFE(d,Op: INTEGER; Info: ADDRESS): BOOLEAN;
  VAR r: BOOLEAN; p: POINTER TO ARRAY [0..79] OF CHAR;
      i: POINTER TO RECORD h,t,s,z: INTEGER END;
BEGIN
  EnterGate(lock); ?1;
  r:=FALSE;
  CASE Op OF
     0: WITH spec[d] DO
          IF d<4 THEN
            Info^:=heads*cyls*sec_no*sec_sz DIV 4096;
          ELSE
            Info^:=heads*(cyls-1)*sec_no*sec_sz DIV 4096;
          END;
        END;
    |1: p:=Info; p^:="DK6I41";
    |2: (* power off *)
    |3: tries:=Info^;
        IF tries<1 THEN tries:=1 END;
        IF tries>8 THEN tries:=8 END;
    |4: r:=InitDK(Info);
    |5: IF Info#NIL THEN
          r:=DKFormat(d,Info^);
        ELSE
          r:=NoSuchCFE;
        END;
    |6: i:=Info;
        IF i#NIL THEN
          WITH spec[d] DO
            IF i^.t>0 THEN cyls:=i^.t END;
            IF i^.h>0 THEN heads:=i^.h END;
            IF i^.s>0 THEN sec_no:=i^.s END;
            IF i^.z>0 THEN sec_sz:=i^.z END;
            print('Drive %d: heads=%d tracks=%d secno=%d secsize=%d\n'
                  ,d,heads,cyls,sec_no,sec_sz);
          END;
        ELSE
          r:=NoSuchCFE
        END;
  ELSE  r:=NoSuchCFE
  END;
  ExitGate(lock); ?0;
  RETURN r;
END DKCFE;

PROCEDURE Stop;
  VAR s: ARRAY [0..79] OF CHAR; r: BOOLEAN;
BEGIN
  RemoveAction(ticker);
  r:=RemDriver(Name);
  IF r THEN VisFSerr(r,s); print('%s\n',s) END;
END Stop;

PROCEDURE InitDrv;
  VAR
    p  : ProcessId;
    r  : BOOLEAN;
    s  : ARRAY [0..79] OF CHAR;
BEGIN
  puk:=ADDRESS(0F00000h+adrPUK DIV 4);
  pwk:=ADDRESS(0F00000h+adrPWK DIV 4);
  pww:=ADDRESS(0F00000h+adrPWW DIV 4);
  wsp:=ADDRESS(0F00000h+adrWSP DIV 4);
  buf:=ADDRESS(0F00000h+adrBUF DIV 4);
  InitGate(lock);
  InitSignal(ready);
  p:=MyTask();
  MOVE(ADR(Name),p^.ParmLink,1); Name[2]:=0c;
  Final(Stop);
  r:=IamDriver(Name,{0..5},blocked,DKRead,DKWrite,DKCFE);
  IF r THEN VisFSerr(r,s); print('%s\n',s); HALT(1) END;
END InitDrv;

VAR
  qqq: Signal;
  mem: ARRAY [0..99] OF INTEGER;
  a  : ADDRESS;
  prs: PROCESS;

BEGIN
  SETM(GETM()-{0..1});
  new_process(trap_proc,ADR(mem),SIZE(mem),prs);
  a:=VEC*2; a^:=prs; INC(a); a^:=ADR(ipted);
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
  InitDrv;
  InitSignal(qqq);
  Wait(qqq);
END DK6I41.
