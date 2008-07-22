MODULE fd; (* 04-May-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD ;
FROM Scheduler  IMPORT  Ticker, Signal, Wait, Send, InsertAction,
                        RemoveAction, InitSignal, Gate, InitGate,
                        EnterGate, ExitGate, ?1, ?0, ProcessId, MyTask;
FROM FsPublic   IMPORT  NoVolume, HardError, TimeOut, NoSuchDrive,
                        ReadError, WriteProtect, WriteError, VisFSerr,
                        NoSuchCFE, NotFoundTrack0, NotFoundSID,
                        SeekError, IllegalDMA;
FROM FsDrv      IMPORT  IamDriver, RemDriver, blocked;
FROM Terminal   IMPORT  print;
FROM Resource   IMPORT  Final;
IMPORT  mcd: mCodeMnem;
IMPORT  sch: Scheduler;

TYPE PROCESS = ADDRESS;

PROCEDURE SETM(m: BITSET); CODE mcd.setm END SETM;
PROCEDURE GETM(): BITSET;  CODE mcd.getm END GETM;
PROCEDURE MOVE(t,f: ADDRESS; sz: INTEGER); CODE mcd.move END MOVE;
PROCEDURE TRANSFER(VAR f,t: PROCESS); CODE mcd.tra END TRANSFER;

CONST
---------------------------------------------------------------------
  buffer = 0F6000h;   -- адрес буфера размером 8К на И41
  int_en = FALSE;
  VEC    = 18h;
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

VAR
  buf    : POINTER TO ARRAY [0..1024*8-1] OF CHAR;
  ctrk   : ARRAY [0..1] OF INTEGER;
  cdrv   : INTEGER;
  ipted  : PROCESS;
  prs    : PROCESS;
  Ready  : Signal;
  time   : INTEGER;
  pause  : INTEGER;
  spec   : ARRAY [0..HIGH(ctrk)] OF RECORD
    tracks : INTEGER;
    secno  : INTEGER;
    secsize: INTEGER;
    heads  : INTEGER;
    restrk : INTEGER;
    status : BITSET;
  END;
  chain    : INTEGER;
  chain_cmd: BITSET;
  Name     : ARRAY [0..3] OF CHAR;
  Lock     : Gate;
  tries    : INTEGER;
  time_out : BOOLEAN;
  v_seg    : BITSET;

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

PROCEDURE int;
BEGIN
  LOOP
    out(seg,v_seg);
    IF chain>0 THEN
      IF BITSET(inp(csr))*{0..7}#{} THEN
        IF 5 IN chain_cmd THEN chain:=1 END;
        WHILE chain>0 DO Send(Ready); DEC(chain) END;
        time:=0;
      ELSE
        DEC(chain); out(rsc,INTEGER(inp(rsc))+1);
        IF chain=0 THEN
          time:=0; Send(Ready);
        ELSE
          out(csr,chain_cmd); out(seg,v_seg+{7});
          IF NOT (5 IN chain_cmd) THEN Send(Ready) END;
        END;
      END;
    ELSE
      time:=0; Send(Ready);
    END;
    TRANSFER(prs,ipted);
  END;
END int;

PROCEDURE argus;
BEGIN
  IF time>0 THEN
    DEC(time);
    IF NOT int_en & NOT(0 IN BITSET(inp(csr)))
                  & NOT(0 IN BITSET(inp(csr))) THEN
      IF chain>0 THEN
        IF BITSET(inp(csr))*{0..7}#{} THEN
          IF 5 IN chain_cmd THEN chain:=1 END;
          WHILE chain>0 DO Send(Ready); DEC(chain) END;
          time:=0;
        ELSE
          DEC(chain); out(rsc,INTEGER(inp(rsc))+1);
          IF chain=0 THEN
            time:=0; Send(Ready);
          ELSE
            time:=200;
            out(csr,chain_cmd);
            IF NOT (5 IN chain_cmd) THEN Send(Ready) END;
          END;
        END;
      ELSE
        time:=0; Send(Ready);
      END;
    ELSIF time=0 THEN
      IF (chain=0) OR (5 IN chain_cmd) THEN chain:=1 END;
      WHILE chain>0 DO Send(Ready); DEC(chain) END;
      time_out:=TRUE; out(seg,v_seg);
    END;
  ELSIF pause>0 THEN
    DEC(pause);
    IF pause=0 THEN cdrv:=-1; out(sel,{2}) END;
  END;
END argus;

PROCEDURE exec(cmd: BITSET; t: INTEGER);
BEGIN
  Ready:=Signal(NIL); time:=t; time_out:=FALSE;
  out(csr,cmd);
  IF int_en THEN out(seg,v_seg+{7}) ELSE out(seg,v_seg) END;
  Wait(Ready);
END exec;

PROCEDURE select(d: INTEGER);
-- обеспечивает выбор устройства и разгон шпинделя
  VAR ei,s: BITSET;
BEGIN
  ei:=GETM(); SETM(ei-{0..1});
  pause:=0;
  IF inp(seg)=0 THEN END; -- запуск двигателя
  IF cdrv=d THEN SETM(ei); RETURN END;
  s:={2};
  out(sel,s);
  cdrv:=d;
  s:=s+BITSET(d+2)+{d+6};
  out(sel,s);
  Ready:=Signal(NIL); time:=50;
  Wait(Ready);         -- ожидание разгона шпинделя
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

PROCEDURE buf_fd;
BEGIN
  out(dma_csr,0h);
  out(dma_adr2,buffer);
  out(dma_adr2,buffer>>8);
  out(dma_cnt2,0);
  out(dma_cnt2,94h);
  out(seg,v_seg);
  out(dma_csr,4h);
END buf_fd;

PROCEDURE buf_fd2;
BEGIN
  out(dma_csr,0h);
  out(dma_adr2,buffer);
  out(dma_adr2,buffer>>8);
  out(dma_cnt2,0);
  out(dma_cnt2,0B0h);
  out(seg,v_seg);
  out(dma_csr,4h);
END buf_fd2;

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
  VAR trk,side,sec,i,j: INTEGER; r: BOOLEAN; end: ADDRESS;
      ptr: POINTER TO ARRAY [0..4095] OF CHAR;
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
      chain:=i;
      out(rsc,sec+1);
      IF side=0 THEN chain_cmd:={7} ELSE chain_cmd:={7,3} END;
      Ready:=Signal(NIL);
      time:=200; time_out:=FALSE;
      out(csr,chain_cmd);
      IF int_en THEN out(seg,v_seg+{7}) ELSE out(seg,v_seg) END;
      ptr:=a; j:=0;
      REPEAT
        Wait(Ready);
        IF sz>=secsize THEN
          MOVE(a,ADR(buf^)+j DIV 4,secsize DIV 4);
          a:=a+secsize DIV 4;
          INC(j,secsize); DEC(sz,secsize);
        ELSE
          REPEAT ptr^[j]:=buf^[j]; INC(j); DEC(sz) UNTIL sz=0;
        END;
        DEC(i);
        INC(sec);
      UNTIL i=0;
      IF time_out  THEN RETURN TimeOut END;
      IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
      IF 7 IN BITSET(inp(csr)) THEN RETURN NoVolume END;
      IF BITSET(inp(csr))*{1,2}#{} THEN RETURN IllegalDMA END;
      IF (inp(rsc)#sec+1) OR (BITSET(inp(csr))*{4,5,3}#{}) THEN RETURN ReadError END;
      IF sz=0 THEN EXIT END;
      sec:=0;
      INC(side);
      IF side=heads THEN side:=0; INC(trk) END;
    END;
  END;
  RETURN FALSE;
END read;

PROCEDURE write(drv,blk: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR trk,side,sec,i,j: INTEGER; r: BOOLEAN;
      ptr: POINTER TO ARRAY [0..4095] OF CHAR;
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
      IF 6 IN BITSET(inp(csr)) THEN RETURN WriteProtect END;
      IF sz<secsize THEN
        IF side>0 THEN INCL(v_seg,6) ELSE EXCL(v_seg,6) END;
        fd_buf;
        out(rsc,sec+1);
        IF side=0 THEN exec({7,2,1},200) ELSE exec({7,3,2,1},200) END;
        IF time_out  THEN RETURN TimeOut END;
        IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
        IF BITSET(inp(csr))*{1,2}#{} THEN RETURN IllegalDMA END;
        IF BITSET(inp(csr))*{4,5,3,7}#{} THEN RETURN ReadError END;
        ptr:=a; j:=0;
        REPEAT buf^[j]:=ptr^[j]; INC(j) UNTIL j=sz;
        sz:=0;
        chain:=1;
      ELSE
        chain:=sz DIV secsize;
        IF chain>secno-sec THEN chain:=secno-sec END;
        i:=chain*secsize;
        ptr:=a; j:=0;
        REPEAT buf^[j]:=ptr^[j]; INC(j) UNTIL j=i;
        DEC(sz,i); INC(a,i DIV 4);
      END;
      IF side>0 THEN INCL(v_seg,6) ELSE EXCL(v_seg,6) END;
      buf_fd;
      IF side=0 THEN chain_cmd:={7,5} ELSE chain_cmd:={7,5,3} END;
      out(rsc,sec+1); i:=chain;
      exec(chain_cmd+{1,2},200);
      IF time_out  THEN RETURN TimeOut END;
      IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
      IF 7 IN BITSET(inp(csr)) THEN RETURN NoVolume END;
      IF BITSET(inp(csr))*{1,2}#{} THEN RETURN IllegalDMA END;
      IF (inp(rsc)#sec+i+1) OR
         (BITSET(inp(csr))*{4,5,3,6}#{}) THEN RETURN WriteError END;
      IF sz=0 THEN EXIT END;
      INC(sec,i);
      IF sec>=secno THEN
        DEC(sec,secno);
        INC(side);
        IF side=heads THEN side:=0; INC(trk) END;
      END;
    END;
  END;
  RETURN FALSE;
END write;

PROCEDURE format(d,blk: INTEGER): BOOLEAN;
  VAR
    format: ARRAY [0..HIGH(buf^)] OF CHAR;
    pos   : INTEGER;
    id    : ARRAY [0..31] OF INTEGER;
    data  : ARRAY [0..31] OF INTEGER;
  PROCEDURE put(c: INTEGER; len: INTEGER);
  BEGIN
    WHILE len>0 DO format[pos]:=CHAR(c); INC(pos); DEC(len) END;
  END put;
  PROCEDURE pre_trk;
    VAR gap1,gap3,size_code,sec,i: INTEGER; dd: BOOLEAN;
    CONST minsec=1; skew_f=1;
    VAR skew: ARRAY [0..31] OF INTEGER;
  BEGIN
    pos:=0;
    WITH spec[d] DO
      FOR sec:=1 TO secno DO skew[sec]:=-1 END;
      i:=1;
      FOR sec:=1 TO secno DO
        WHILE skew[i]#-1 DO
          INC(i);
          IF i>secno THEN DEC(i,secno) END;
        END;
        skew[i]:=sec; INC(i,skew_f);
        IF i>secno THEN DEC(i,secno) END;
      END;
      IF    secsize= 128 THEN size_code:=0
      ELSIF secsize= 256 THEN size_code:=1
      ELSIF secsize= 512 THEN size_code:=2
      ELSE size_code:=3 END;
      dd:=TRUE;
      IF dd THEN
        CASE size_code OF
          |0: gap1:=32; gap3:=32;
          |1: gap1:=32; gap3:=32;
          |2: CASE secno OF
                |8 : gap1:=60; gap3:=32;
                |9 : gap1:=32; gap3:=32;
                |10: gap1:=32; gap3:=30;
              END;
          |3: gap1:=48; gap3:=40;
        END;
      ELSE
        CASE size_code OF
         |0: gap1:=40; gap3:=10;
         |1: gap1:=40; gap3:=10;
         |2: gap1:=20; gap3:=10;
         |3: gap1:=20; gap3:=10;
        END;
      END;
      IF dd THEN
        put(4Eh,gap1);
        FOR sec:=minsec TO minsec+secno-1 DO
          put(0,10);
          put(0F5h,3); put(0FEh,1);
          id[skew[sec]]:=pos;
          put(0,2); put(skew[sec],1); put(size_code,1); put(0F7h,1);
          put(4Eh,22); put(0,12); put(0F5h,3); put(0FBh,1);
          data[skew[sec]]:=pos;
          put(0E5h,secsize); put(0F7h,1); put(4Eh,gap3);
        END;
        put(4Eh,HIGH(format)+1-pos);
      ELSE
        put(0FFh,gap1);
        FOR sec:=minsec TO minsec+secno-1 DO
          put(0,6); put(0FEh,1);
          id[sec]:=pos;
          put(0,2); put(sec,1); put(size_code,1); put(0F7h,1);
          put(0FFh,11); put(0,6); put(0FBh,1);
          data[sec]:=pos;
          put(0E5h,secsize); put(0F7h,1); put(0FFh,gap3);
        END;
        put(0FFh,HIGH(format)+1-pos);
      END;
    END;
  END pre_trk;
  VAR trk,side,sec: INTEGER; r: BOOLEAN;
BEGIN
  IF blk#0 THEN RETURN FALSE END;
  pre_trk;
  FOR pos:=0 TO HIGH(format) DO buf^[pos]:=format[pos] END;
  FOR trk:=0 TO spec[d].tracks-1 DO
    IF trk=0 THEN r:=seek00(d) ELSE r:=step(d) END;
    IF r THEN RETURN r END;
    IF 6 IN BITSET(inp(csr)) THEN RETURN WriteProtect END;
    FOR side:=0 TO spec[d].heads-1 DO
      print("%$2d:%d\r",trk,side);
      FOR sec:=1 TO spec[d].secno DO
        buf^[id[sec]]:=CHAR(trk);
        buf^[id[sec]+1]:=CHAR(side);
      END;
      IF side>0 THEN INCL(v_seg,6) ELSE EXCL(v_seg,6) END;
      buf_fd2;
      exec({7,6,5,4,2},300);
      IF 0 IN BITSET(inp(csr)) THEN RETURN TimeOut END;
      IF 7 IN BITSET(inp(csr)) THEN RETURN NoVolume END;
      IF 2 IN BITSET(inp(csr)) THEN RETURN IllegalDMA END;
    END;
  END;
  RETURN FALSE;
END format;

PROCEDURE stop;
BEGIN
  out(csr,{7,6,4});
  out(sel,0);
  out(seg,0);
  cdrv:=-1;
  RemoveAction(argus);
  IF RemDriver(Name) THEN print('Can not remove driver.\n') END;
END stop;

VAR wsp: ARRAY [0..149] OF INTEGER;

PROCEDURE init_driver;
  VAR adr: ADDRESS; ei: BITSET; i,j: INTEGER;
BEGIN
  IF buffer MOD (1024*8) # 0 THEN
    print('Адрес буфера на И41 должен быть кратен 8К байт.\n'); HALT(1)
  END;
  out(sel,0);
  out(sel,{1,6});
  out(sel,{1,6,2});
  v_seg:=BITSET(buffer DIV (1024*64));
  out(seg,v_seg);
  buf:=ADDRESS(0F00000h+buffer DIV 4);
  FOR i:=0 TO HIGH(buf^) DO buf^[i]:=0c END;
  WHILE 0 IN BITSET(inp(csr)) DO END;
  out(sel,{2});
  FOR i:=0 TO 255 DO
    out(cyl,i); j:=inp(cyl);
    IF i#j THEN
      print('cyl %$2h %$8h, controller unavailable.\n',i,j);
      HALT(1);
    END;
  END;
  FOR i:=0 TO 255 DO
    out(rsc,i);
    IF i#inp(rsc) THEN
      print('rsc %$2h %$2h, controller unavailable.\n',i,inp(rsc));
      HALT(1);
    END;
  END;
  cdrv:=-1;
  time:=0; pause:=0; chain:=0;
  InitSignal(Ready);
  IF InsertAction(argus) THEN HALT(1) END;
  FOR i:=0 TO HIGH(ctrk) DO ctrk[i]:=-1 END;
  out(sel,{2});
  out(csr,{7,6,4});
  ei:=GETM(); SETM(ei-{0..1});
  IF int_en THEN
    sch.new_process(int,ADR(wsp),SIZE(wsp),prs);
    adr:=VEC*2; adr^:=prs;
    adr:=adr+1; adr^:=ADR(ipted);
  END;
  FOR i:=0 TO HIGH(spec) DO
    WITH spec[i] DO
      tracks:=40; heads:=1; secno:=5; secsize:=1024; restrk:=0; status:={};
    END;
  END;
  Final(stop);
  SETM(ei);
END init_driver;

PROCEDURE fdRead(d,b: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR r: BOOLEAN; t: INTEGER;
BEGIN
  EnterGate(Lock);
  ?1;
  t:=tries;
  REPEAT
    r:=read(d,b,a,sz); DEC(t);
    IF r & (r#IllegalDMA) THEN ctrk[d]:=-1 END;
  UNTIL NOT r OR (r=NoVolume) OR (t=0);
  pause:=200;
  ExitGate(Lock);
  ?0;
  RETURN r;
END fdRead;

PROCEDURE fdWrite(d,b: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR r: BOOLEAN; t: INTEGER;
BEGIN
  EnterGate(Lock);
  ?1;
  t:=tries;
  REPEAT
    r:=write(d,b,a,sz); DEC(t);
    IF r THEN ctrk[cdrv]:=-1 END;
  UNTIL NOT r OR (r=NoVolume) OR (t=0);
  pause:=200;
  ExitGate(Lock);
  ?0;
  RETURN r;
END fdWrite;

PROCEDURE fdCFE(d,Op: INTEGER; Info: ADDRESS): BOOLEAN;
  VAR r: BOOLEAN; p: POINTER TO ARRAY [0..63] OF CHAR;
      i: POINTER TO RECORD h,t,s,z: INTEGER END;
BEGIN
  EnterGate(Lock);
  ?1;
  r:=FALSE;
  CASE Op OF
     0: Info^:=(spec[d].tracks-spec[d].restrk)*spec[d].heads*spec[d].secno*
               spec[d].secsize DIV 4096;
    |1: p:=Info; p^:="Kronos FDC";
    |2: (* power off *)
    |3: tries:=Info^;
        IF tries<0 THEN tries:=0 END;
        IF tries>8 THEN tries:=8 END;
    |4: r:=FALSE;
    |5: r:=format(d,Info^); pause:=200;
    |6: i:=Info;
        IF i#NIL THEN
          WITH spec[d] DO
            IF i^.t>0 THEN tracks:=i^.t END;
            IF i^.h>0 THEN heads:=i^.h END;
            IF i^.s>0 THEN secno:=i^.s END;
            IF i^.z>0 THEN secsize:=i^.z END;
            print('Minifloppy %d: heads=%d tracks=%d secno=%d secsize=%d\n'
                  ,d,heads,tracks,secno,secsize);
          END;
        ELSE
          r:=NoSuchCFE
        END;
  ELSE  r:=NoSuchCFE
  END;
  ExitGate(Lock);
  ?0;
  RETURN r;
END fdCFE;

VAR
  never: sch.Signal;
  r    : BOOLEAN;
  s    : ARRAY [0..63] OF CHAR;
  p    : ProcessId;

BEGIN
  init_driver;
  tries:=8;
  InitGate(Lock);
  p:=MyTask();
  MOVE(ADR(Name),p^.ParmLink,1); Name[2]:=0c;
  IF Name='' THEN Name:='mf' END;
  r:=IamDriver(Name,{0,1},blocked,fdRead,fdWrite,fdCFE);
  IF r THEN VisFSerr(r,s); print(s); HALT(1) END;
  InitSignal(never);
  Wait(never);
END fd.
