MODULE DKqRX02; (*$T- Leo 30-Nov-87. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  cod: defCodes;
IMPORT   os: osKernel;
IMPORT   fs: osFiles;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  env: tskEnv;
IMPORT  low: lowLevel;

TYPE ADDRESS = sys.ADDRESS;
     PROCESS = ADDRESS;

CONST  CSR = 177170b DIV 2;  DTR = CSR + 1;  VEC = 264b DIV 4;

  sectors =  26;        geterror = 017b;
  readcom = 107b;       writecom = 105b;
  fillbuf = 001b;       bufread  = 003b;

PROCEDURE wait(bit: BITSET); (*
BEGIN REPEAT UNTIL i*BITSET(QIN(CSR))#{} END wait;*)
CODE cod.copt cod.lid CSR MOD 100h CSR DIV 100h
     cod.inp  cod.and cod.jbsc 8 cod.drop
END wait;

PROCEDURE r;
CODE
  cod.copt

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp     cod.and  cod.jbsc 8  cod.drop

  cod.lid DTR MOD 100h DTR DIV 100h
  cod.inp

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp     cod.and  cod.jbsc 8  cod.drop

  cod.lid DTR MOD 100h DTR DIV 100h cod.inp      8 cod.rol cod.or

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp     cod.and  cod.jbsc 8 cod.drop

  cod.lid DTR MOD 100h DTR DIV 100h cod.inp cod.lib 16 cod.rol cod.or

  cod.lib 80h cod.copt cod.lid  CSR MOD 100h CSR DIV 100h
  cod.inp     cod.and  cod.jbsc 8  cod.drop

  cod.lid DTR MOD 100h DTR DIV 100h cod.inp 8 cod.ror cod.or

  cod.ssw0 1 cod.add
END r;

PROCEDURE push(w: sys.WORD); CODE END push;

PROCEDURE drop; CODE cod.drop END drop;

PROCEDURE inp(reg: INTEGER): BITSET; CODE cod.inp END inp;

PROCEDURE out(reg: INTEGER; data: sys.WORD); CODE cod.out END out;

PROCEDURE TRANSFER(VAR from,to: PROCESS); CODE cod.tra END TRANSFER;

PROCEDURE setm(m: BITSET); CODE cod.setm END setm;
PROCEDURE getm(): BITSET;  CODE cod.getm END getm;


VAR   tries: ARRAY [0..1] OF INTEGER;
        max: INTEGER;
    LastTry: BOOLEAN;
     Finish: os.signal_rec;
      Ipted: PROCESS;
     Driver: PROCESS;
     wrBuff: ARRAY [0..127] OF CHAR;

PROCEDURE w;
CODE
  cod.copt cod.stot cod.lsw0
  8 cod.rol cod.copt cod.stot
  8 cod.rol cod.copt cod.stot
  8 cod.rol cod.copt cod.stot
  8 cod.rol cod.stot
  cod.lid DTR MOD 100h DTR DIV 100h

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp cod.and cod.jbsc 8 cod.drop

  cod.copt cod.lodt cod.out

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp cod.and cod.jbsc 8 cod.drop

  cod.copt cod.lodt cod.out

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp cod.and cod.jbsc 8 cod.drop

  cod.copt cod.lodt cod.out

  cod.lib 80h cod.copt cod.lid CSR MOD 100h CSR DIV 100h
  cod.inp cod.and cod.jbsc 8 cod.drop

       cod.lodt cod.out
  cod.lodt 1 cod.add
END w;

PROCEDURE readSector(drv,trk,sec: INTEGER; buf: ADDRESS): INTEGER;

  VAR error: BITSET;          i: INTEGER;
        res: INTEGER;  ei,et,di: BITSET;
        adr: POINTER TO ARRAY [0..127] OF CHAR;

BEGIN
  res:=err.ok;  out(CSR,0);
  IF inp(CSR)#{5} THEN RETURN err.hw_fail END;

  et:=getm(); ei:=et-{1}; di:=ei-{0};
  wait({5}); out(CSR,readcom+drv*16);
  wait({7}); out(DTR,sec);
  setm(di);
  wait({7}); out(DTR,trk);
  TRANSFER(Driver,Ipted); error:=inp(CSR); out(CSR,0);
  setm(ei); wait({5});
  IF 15 IN error THEN res:=err.ecc_err END;
  out(CSR,bufread);

  push(buf); i:=0;
  REPEAT  r;r;r;r; r;r;r;r; i:=i+1 UNTIL i=4;
  drop;

  setm(et); RETURN res;
END readSector;

VAR LayOutR,LayOutW: ARRAY [0..3] OF BITSET;
    RoundsR,RoundsW: INTEGER;

PROCEDURE read(drv,bno: INTEGER; buf: ADDRESS; szB: INTEGER;
               VAR OK: BITSET; VAR errtrk: INTEGER;
              ): INTEGER;
  VAR
    error,res: INTEGER;
    SizeInSec: INTEGER;
     SectorNo: INTEGER;
     FirstSec: INTEGER;
      StartNo: INTEGER;
      TrackNo: INTEGER;
        round: INTEGER;
     LogSecNo: INTEGER;
     PhySecNo: INTEGER;
      sz,size: INTEGER;
            i: INTEGER;

BEGIN
  res:=err.ok;  out(CSR,0);
  IF inp(CSR)#{5} THEN RETURN err.hw_fail END;
  size:=szB;
  SizeInSec:=szB;

  FirstSec:=bno MOD 26; (* first sector on track *)
  TrackNo :=bno DIV 26;
  SectorNo:=0;
  WHILE (size>0) DO
    StartNo:=SectorNo;  round:=0;
    WHILE (round<RoundsR) & (size>0) DO
      PhySecNo:=FirstSec;       LogSecNo:=StartNo;
      WHILE  (PhySecNo<26) & (size>0) & (LogSecNo<SizeInSec) DO
        IF PhySecNo IN LayOutR[round] THEN
          IF NOT (LogSecNo IN OK) THEN
            error:=readSector(drv,TrackNo,PhySecNo+1,buf+LogSecNo*32);
            IF  error=err.ok THEN INCL(OK,LogSecNo)
            ELSIF res=err.ok THEN errtrk:=TrackNo;
            END;
            res:=res+error;
          END;
          DEC(size);     INC(SectorNo);
          INC(PhySecNo); INC(LogSecNo)
        ELSE
          INC(PhySecNo); INC(LogSecNo)
        END
      END;
      INC(round)
    END;
    FirstSec:=0; INC(TrackNo);
  END;
  RETURN res
END read;

PROCEDURE readSect(drv,trk,sec: INTEGER): INTEGER;

  VAR i: INTEGER;  error,di,ei,et: BITSET;  res: INTEGER;
BEGIN
  res:=err.ok;  out(CSR,0);
  IF inp(CSR)#{5} THEN RETURN err.hw_fail END;

  et:=getm(); ei:=et-{1}; di:=ei-{0};
  out(CSR,0);
  wait({5}); out(CSR,readcom+drv*16);
  wait({7}); out(DTR,sec);
  setm(di);
  wait({7}); out(DTR,trk);
  TRANSFER(Driver,Ipted); error:=inp(CSR); out(CSR,0);
  setm(ei);
  wait({5});
  IF 15 IN error THEN res:=err.chk_err END;
  setm(et); RETURN res
END readSect;

PROCEDURE writeSect(drv,trk,sec: INTEGER; buf: ADDRESS): INTEGER;

  VAR ei,di,et,error: BITSET;  i: INTEGER;

BEGIN
  out(CSR,0);
  IF inp(CSR)#{5} THEN RETURN err.hw_fail END;

  wait({5}); out(CSR,fillbuf);
  et:=getm(); ei:=et-{1}; di:=ei-{0};
  push(buf); i:=0;
  REPEAT  w;w;w;w; w;w;w;w; i:=i+1 UNTIL i=4;
  drop;
  setm(di);  out(CSR,0); setm(ei);
  wait({5});   out(CSR,writecom+drv*16);
  wait({7});   out(DTR,sec);
  setm(di);
  wait({7});   out(DTR,trk);
  TRANSFER(Driver,Ipted); error:=inp(CSR); out(CSR,0);
  setm(ei);  wait({5});
  setm(et);
  IF 15 IN error THEN RETURN err.ecc_err ELSE RETURN err.ok END
END writeSect;

PROCEDURE write(drv,bno: INTEGER; buf: ADDRESS; szB: INTEGER;
                VAR OK: BITSET; VAR errtrk: INTEGER;
               ): INTEGER;

  VAR
    error,res: INTEGER;
    SizeInSec: INTEGER;
     SectorNo: INTEGER;
     FirstSec: INTEGER;
      LastSec: INTEGER;
      StartNo: INTEGER;
      TrackNo: INTEGER;
        round: INTEGER;
     LogSecNo: INTEGER;
     PhySecNo: INTEGER;
      sz,size: INTEGER;
            i: INTEGER;
          adr: POINTER TO ARRAY [0..127] OF CHAR;

BEGIN
  res:=err.ok;  out(CSR,0);
  IF inp(CSR)#{5} THEN RETURN err.hw_fail END;

  size:=szB;
  SizeInSec:=szB;

  FirstSec:=bno MOD 26; (* first sector on track *)
  TrackNo :=bno DIV 26;
  SectorNo:=0;
  WHILE (size>0) DO
    StartNo:=SectorNo;  round:=0;  LastSec:=0;
    WHILE (round<RoundsW) & (size>0) DO
      PhySecNo:=FirstSec;       LogSecNo:=StartNo;
      WHILE  (PhySecNo<26) & (size>0) & (LogSecNo<SizeInSec) DO
        IF PhySecNo IN LayOutW[round] THEN
          IF NOT (LogSecNo IN OK) THEN
            IF PhySecNo>LastSec THEN LastSec:=PhySecNo END;
            error:=writeSect(drv,TrackNo,PhySecNo+1,buf+LogSecNo*32);
            IF  error=err.ok THEN INCL(OK,LogSecNo)
            ELSIF res=err.ok THEN errtrk:=TrackNo
            END;
            res:=res+error
          END;
          DEC(size);     INC(SectorNo);
          INC(PhySecNo); INC(LogSecNo)
        ELSE
          INC(PhySecNo); INC(LogSecNo)
        END
      END;
      INC(round)
    END;
    FirstSec:=0; INC(TrackNo)
  END;
  RETURN res
END write;

CONST READ=1;
     WRITE=2;
     RETRY=3;

VAR op,Drv,Block,Size
      ,try,errtrk,Trk: INTEGER;
                 Buff: ADDRESS;
                  Res: INTEGER;
              wOK,rOK: BITSET;

PROCEDURE DoIO;
BEGIN
  LOOP
    out(CSR,0);
    setm(getm()+{0});
    CASE op OF
      | READ: Res:= read(Drv,Block,Buff,Size,rOK,errtrk);
      |WRITE: Res:=write(Drv,Block,Buff,Size,wOK,errtrk);
      |RETRY: Res:=readSect(Drv,Trk,0);
    END;
    setm(getm()-{0}); out(CSR,0);
    os.send(Finish); TRANSFER(Driver,Ipted);
  END
END DoIO;

PROCEDURE Read(VAR r: req.REQUEST);
BEGIN
  op:=READ;  Buff:=r.buf; Size:=r.len; Block:=r.ofs; Drv:=r.drn; Trk:=0;
  TRANSFER(Ipted,Driver);
  IF os.wait_del(0,Finish)=1 THEN r.res:=err.ipted_op; RETURN END;
  r.res:=Res
END Read;

PROCEDURE Write(VAR r: req.REQUEST);
BEGIN
  op:=WRITE; Buff:=r.buf; Size:=r.len; Block:=r.ofs; Drv:=r.drn; Trk:=0;
  TRANSFER(Ipted,Driver);
  IF os.wait_del(0,Finish)=1 THEN r.res:=err.ipted_op; RETURN END;
  r.res:=Res
END Write;

PROCEDURE reset;
BEGIN
  REPEAT
    REPEAT out(CSR,40000b) UNTIL inp(CSR)={};
    REPEAT UNTIL inp(CSR)*{15,5,7}#{};
  UNTIL inp(CSR)-{15}={5};
END reset;

PROCEDURE Retry(drv,trk: INTEGER);
  VAR i: BOOLEAN;
BEGIN
  op:=RETRY;
  Size:=0; Block:=0; Drv:=drv; Trk:=trk;
  TRANSFER(Ipted,Driver); os.wait(Finish)
END Retry;

PROCEDURE retry(d: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  DEC(try);
  IF Res=err.hw_fail THEN reset END;
  CASE try MOD 4 OF
    |1: IF errtrk>00 THEN Retry(d,errtrk-1) END
    |2: IF errtrk<76 THEN Retry(d,errtrk+1) END
    |3: Retry(d,errtrk)
  ELSE
    reset
  END
END retry;

PROCEDURE RXRead(VAR r: req.REQUEST);
BEGIN
  IF NOT (r.drn IN {0,1})           THEN r.res:=err.inv_dad; RETURN END;
  IF (r.ofs<0) OR (r.ofs+r.len>max) THEN r.res:=err.inv_dad; RETURN END;

  try:=tries[r.drn]*4; rOK:={}; wOK:={};
  LOOP
    Read(r);
    IF (r.res=err.ok) OR (try<=0) THEN EXIT END;
    retry(r.drn)
  END
END RXRead;

PROCEDURE RXWrite(VAR r: req.REQUEST);
BEGIN
  IF NOT (r.drn IN {0,1})           THEN r.res:=err.inv_dad; RETURN END;
  IF (r.ofs<0) OR (r.ofs+r.len>max) THEN r.res:=err.inv_dad; RETURN END;

  try:=tries[r.drn]*4; rOK:={}; wOK:={};
  LOOP
    LastTry:=(try<=0);
    Write(r);
    IF (r.res=err.ok) OR (try<=0) THEN EXIT END;
    retry(r.drn)
  END
END RXWrite;

(*
PROCEDURE RdTrack(d,Trk: INTEGER; Buf: ADDRESS);
  VAR i: INTEGER;  c: BITSET; Sec,s: INTEGER;
BEGIN
  FOR Sec:=1 TO 26 DO s:=Sec*4 MOD 26 + ORD(Sec>12);
    wait({5}); out(CSR,BITSET(readcom)-{6}+BITSET(d*16));
    wait({7}); out(DTR,s); wait({7}); out(DTR,Trk);

    wait({5}); out(CSR,bufread); push(Buf); i:=0;
    REPEAT
      r;r;r;r; r;r;r;r; i:=i+1;
    UNTIL i=4;
    drop; INC(Buf,32);
    wait({5});
  END;
END RdTrack;

PROCEDURE WrTrack(d,Trk: INTEGER; Buf: ADDRESS);
  VAR i: INTEGER;  c: BITSET; Sec,s: INTEGER;
BEGIN
  FOR Sec:=1 TO 26 DO s:=Sec*4 MOD 26 + ORD(Sec>12);
    wait({5}); out(CSR,fillbuf); push(Buf); i:=0;
    REPEAT
      w;w;w;w; w;w;w;w; i:=i+1;
    UNTIL i=4;
    drop; INC(Buf,32);
    wait({5}); out(CSR,BITSET(writecom)-{6}+BITSET(d*16));
    wait({7}); out(DTR,s);
    wait({7}); out(DTR,Trk); wait({5});
  END;
END WrTrack;

PROCEDURE RXFormat(d,b: INTEGER);
  VAR Buff: ARRAY [0..26*128-1] OF CHAR; Trk,s: INTEGER;
BEGIN
  Trk:=b*32 DIV sectors; s:=0;
  REPEAT
    RdTrack(d,Trk,sys.ADR(Buff)); wait({5});
    out(CSR,11b);            wait({7});
    out(DTR,125b);           wait({7});
    out(DTR,Trk);            wait({5});
    WrTrack(d,Trk,sys.ADR(Buff));
    REPEAT INC(s) UNTIL ((b*32 + s) DIV sectors # Trk) OR (s>=32);
    Trk:=(b*32+s) DIV sectors;
  UNTIL (Trk>76) OR (s>32);
END RXFormat;
*)

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    dsecs  :=max;
    heads  :=  1;
    minsec :=  1;
    maxsec := 26;
    cyls   := 77;
    ssc    :=  7;
    secsize:=128;
    ressec :=  0;
  END
END get_spec;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  r.res:=err.ok;
  CASE r.op OF
    |req.NOP      :
    |req.READ     : RXRead(r)
    |req.WRITE    : RXWrite(r)
    |req.MOUNT    :
    |req.UNMOUNT  :
    |req.POWER_OFF:
    |req.GET_SPEC : get_spec(r)
  ELSE
    r.res:=err.inv_op
  END
END doio;

PROCEDURE SkewFactor(read,write: INTEGER);

  PROCEDURE skew(VAR rou: INTEGER; VAR layout: ARRAY OF BITSET);
  BEGIN
    IF rou<1 THEN rou:=1 END;
    IF rou>4 THEN rou:=4 END;
    CASE rou OF
    |1: layout[0]:={0..25};

    |2: layout[0]:={00,02,04,06,08,10,12,14,16,18,20,22,24};
        layout[1]:={01,03,05,07,09,11,13,15,17,19,21,23,25};

    |3: layout[0]:={00,03,06,09,12,15,18,21,24};
        layout[1]:={02,05,08,11,14,17,20,23   };
        layout[2]:={01,04,07,10,13,16,19,22,25};

    |4: layout[0]:={00,04,08,12,16,20,24};
        layout[1]:={02,06,10,14,18,22   };
        layout[2]:={01,05,09,13,17,21,25};
        layout[3]:={03,07,11,15,19,23   };
    END;
  END skew;

BEGIN
  skew(read ,LayOutR);
  RoundsR:=read;
  skew(write,LayOutW);
  RoundsW:=write;
END SkewFactor;

PROCEDURE install;
  VAR r: INTEGER;
BEGIN
  r:=fs.define_driver("rx0",""   ,0,fs.disk,doio);
  IF r#err.ok THEN HALT(r) END;
  r:=fs.define_driver("rx1","rx0",1,fs.disk,doio);
  IF r#err.ok THEN HALT(r) END;
  env.put_str(env.info,"rx0 rx1",TRUE)
END install;

PROCEDURE self(): ADDRESS; CODE cod.activ END self;

PROCEDURE init;
  VAR  ei: BITSET;
      adr: ADDRESS;
BEGIN
  IF    low.cpu=2 THEN SkewFactor(4,4);
  ELSIF low.cpu=6 THEN SkewFactor(2,3);
  ELSE HALT(2)
  END;

  max:=77*26; tries[0]:=4; tries[1]:=4;  out(CSR,40000b);
  os.ini_signal(Finish,{},0);
  ei:=getm(); setm(ei-{0,1});

  Driver:=self();
  adr:=VEC*2; adr^:=Driver;
  adr:=adr+1; adr^:=sys.ADR(Ipted);
  os.suspend(os.active(),-1);
  DoIO
END init;

BEGIN
  install;
  env.become_ipr;
  init
END DKqRX02.
