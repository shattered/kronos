MODULE DKqMY; (*$T-$N-$I- Leo 03-Nov-89. (c) KRONOS *)

IMPORT  cod: defCodes;          IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT   os: osKernel;
IMPORT  req: defRequest;        IMPORT   fs: osFiles;
IMPORT  env: tskEnv;

(* DONT FORGER to say RES=1300  to config! *)

PROCEDURE move(a,b,c: INTEGER); CODE cod.move END move;

CONST ok = err.ok;

CONST
  CSR   = 172140b DIV 2;
  DTR   = CSR+1;
  BASE  = 10000h DIV 4;
  TRIES = 8;

VAR
  BUFFER: SYSTEM.ADDRESS;
  BCB   : SYSTEM.ADDRESS;

CONST
  READ   = 001b;
  WRITE  = 003b;
  FORMAT = 015b;
  SEEK   = 017b;
  SETSPC = 021b;
  RDERR  = 023b;

TYPE
  bs      = BITSET;
  int     = INTEGER;
  ADDRESS = SYSTEM.ADDRESS;
  WORD    = SYSTEM.WORD;
  SPEC    = RECORD
              dmode  : BITSET;
              dsecs  : INTEGER; (* device size in secs *)
              ssc    : INTEGER; (* sector size code    *)
              secsize: INTEGER; (* sector len 2**ssc   *)
              cyls   : INTEGER; (* # cylinders         *)
              heads  : INTEGER; (* # heads             *)
              minsec : INTEGER; (* min sector # on trk *)
              maxsec : INTEGER; (* max sector # on trk *)
              ressec : INTEGER; (* reserved sectors    *)
              precomp: INTEGER; (* precompensation     *)
              rate   : INTEGER; (* heads stepping      *)
            END;

PROCEDURE inp(reg: WORD): BITSET; CODE cod.inp  END inp;
PROCEDURE out(reg,data: WORD);    CODE cod.out  END out;
PROCEDURE QUIT; CODE cod.quit END QUIT;

VAR
  ports: ARRAY [0..1] OF SPEC;


PROCEDURE clear;
  VAR i,time: INTEGER;
BEGIN
  i:=4;
  REPEAT
    DEC(i); out(CSR,{14,0}); time:=5000;
    REPEAT DEC(time) UNTIL (time=0) OR (inp(CSR)={5})
  UNTIL (time#0) OR (i=0)
END clear;

PROCEDURE reset(drv: INTEGER);
  VAR i,time: INTEGER;
BEGIN
  clear;
  out(CSR,SEEK);
  time:=5000;
  REPEAT DEC(time) UNTIL (time=0) OR (inp(CSR)*{7}#{});
  IF time=0 THEN RETURN END;
  out(DTR,drv); time:=50000;
  REPEAT DEC(time) UNTIL (time=0) OR (inp(CSR)*{5}#{});
  FOR i:=0 TO 20000 DO END;
  clear
END reset;


PROCEDURE exec(op: INTEGER; drv,trk,sur,sec,wlen: INTEGER): BITSET;
  TYPE c=CHAR;
  VAR bcb: POINTER TO ARRAY [0..7] OF CHAR;
     time: INTEGER;
    i,wrd: INTEGER;
BEGIN
  IF op IN {READ,WRITE} THEN
    bcb:=BCB;
    bcb^[0]:=c(sur*4+drv);
    bcb^[1]:=0c;
    bcb^[2]:=c(BUFFER*4 MOD 100h);
    bcb^[3]:=c(BUFFER*4 DIV 100h);
    bcb^[4]:=c(sec);
    bcb^[5]:=c(trk);
    wrd:=wlen*2;
    bcb^[6]:=c(wrd MOD 100h);
    bcb^[7]:=c(wrd DIV 100h);
  END;
  time:=5000;
  REPEAT DEC(time) UNTIL (time=0) OR ( {5}*inp(CSR)#{} );
  IF time=0 THEN reset(drv); RETURN {8} END;

  out(CSR,op);
  time:=5000;
  REPEAT DEC(time) UNTIL (time=0) OR ( {5,7}*inp(CSR)={7} );
  IF time=0 THEN reset(drv); RETURN {8} END;


  IF op IN {READ,WRITE} THEN  out(DTR,BCB*4)
  ELSIF op=SEEK   THEN        out(DTR,drv+trk*256)
  ELSIF op=FORMAT THEN        out(DTR,drv+sur*4+wrd*256+sec*8)
  ELSE
    RETURN {8}
  END;

  time:=50000;
  REPEAT DEC(time) UNTIL (time=0) OR ( {5}*inp(CSR)#{} );
  IF time=0 THEN reset(drv); RETURN {8} END;

  IF {15}*inp(CSR)#{} THEN RETURN inp(DTR)+{9} ELSE RETURN {} END
END exec;

PROCEDURE error(r: BITSET): BITSET;
 CONST
   ERR = ARRAY OF INTEGER {
     err.data_crc   ,err.head_crc    ,err.ok          ,err.seek_0
    ,err.seek_err   ,err.miss_sid    ,err.bad_block   ,err.not_ready
    ,err.time_out   ,err.ok          ,err.ok          ,err.inv_dma
    ,err.frm_err    ,err.miss_did    ,err.unsuitable  ,err.hw_fail };
  VAR i: INTEGER; e: BITSET;
BEGIN
  e:=BITSET(err.io_error); r:=r/{7}-{2,9,10};
  FOR i:=0 TO 15 DO
    IF i IN r THEN e:=e+BITSET(ERR[i]) END
  END;
  RETURN e
END error;

PROCEDURE execio(op: INTEGER; drv,trk,sur,sec,wlen: INTEGER; VAR r: INTEGER);
  VAR io: BITSET;
BEGIN
  io:=exec(op,drv,trk,sur,sec,wlen);
  IF io={} THEN RETURN END;
  r:=INTEGER(BITSET(r)+error(io))
END execio;

PROCEDURE read(VAR r: req.REQUEST);
  VAR sec: INTEGER;  secs: INTEGER;
      trk: INTEGER;   buf: ADDRESS;
      hed: INTEGER;   len: INTEGER;
     size: INTEGER;   tai: INTEGER;
BEGIN
  r.res:=ok;
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+ressec;   secs:=maxsec-minsec+1;
    trk:=sec DIV secs;    hed:=trk MOD heads;
    sec:=sec MOD secs;    trk:=trk DIV heads;
    LOOP
      tai:=len;
      IF sec+tai>secs THEN tai:=secs-sec END;
      size :=tai<<(ssc-2);
      execio(READ,r.drn,trk,hed,sec+minsec,size,r.res);
      move(buf,BUFFER,size);
      DEC(len,tai);
      IF len=0       THEN RETURN END;

      INC(sec,tai);  INC(buf,size);
      IF sec=secs    THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END read;

PROCEDURE write(VAR r: req.REQUEST);
  VAR sec: INTEGER;  secs: INTEGER;
      trk: INTEGER;   buf: ADDRESS;
      hed: INTEGER;   len: INTEGER;
     size: INTEGER;   tai: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+ressec; secs:=maxsec-minsec+1;
    trk:=sec DIV secs;    hed:=trk MOD heads;
    sec:=sec MOD secs;    trk:=trk DIV heads;
    LOOP
      tai:=len;
      IF sec+tai>secs THEN tai:=secs-sec END;
      size :=tai<<(ssc-2);
      move(BUFFER,buf,size);
      execio(WRITE,r.drn,trk,hed,sec+minsec,size,r.res);
      DEC(len,tai);
      IF len=0       THEN RETURN END;
      INC(sec,tai);  INC(buf,size);
      IF sec=secs    THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END write;

PROCEDURE delay;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 5000 DO END
END delay;

PROCEDURE format(VAR r: req.REQUEST);
  VAR sec,trk,secs,hed: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    sec:=r.ofs+ressec;   secs:=maxsec-minsec+1;
    trk:=sec DIV secs;    hed:=trk MOD heads;
    sec:=sec MOD secs;    trk:=trk DIV heads;
    IF r.ofs=0 THEN
      reset(r.drn); delay;
      IF exec(SEEK,r.drn,000,0,1,0)={} THEN END;
      delay; delay; delay; delay
    END;
    REPEAT UNTIL inp(CSR)*{5}#{};
    out(CSR,SEEK);
    REPEAT UNTIL inp(CSR)*{7}#{};
    out(DTR,r.drn+trk*256);
    REPEAT UNTIL inp(CSR)*{5}#{};
    delay;
  
    REPEAT UNTIL inp(CSR)*{5}#{};
    out(CSR,FORMAT);
    REPEAT UNTIL inp(CSR)*{7}#{};
    delay;

    out(DTR,r.drn+hed*4+(80h+trk*2)*256+(ssc-7)*8);
    REPEAT UNTIL inp(CSR)*{5}#{};
    IF inp(CSR)*{15}={} THEN r.res:=ok; RETURN END;
    r.res:=INTEGER(error(inp(DTR)))
  END
END format;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  WITH ports[r.drn] DO
    r.dmode  :=dmode;
    r.dsecs  :=dsecs;
    r.ssc    :=ssc;
    r.secsize:=secsize;
    r.cyls   :=cyls;
    r.heads  :=heads;
    r.minsec :=minsec;
    r.maxsec :=maxsec;
    r.ressec :=ressec;
    r.rate   :=rate;
    r.precomp:=precomp;
  END
END get_spec;

PROCEDURE set_spec(VAR r: req.REQUEST);
BEGIN
  r.res:=ok;
  WITH ports[r.drn] DO
    dmode  :=r.dmode;
    dsecs  :=r.dsecs;
    ssc    :=r.ssc;
    secsize:=r.secsize;
    cyls   :=r.cyls;
    heads  :=r.heads;
    minsec :=r.minsec;
    maxsec :=r.maxsec;
    ressec :=r.ressec;
    rate   :=r.rate;
    precomp:=r.precomp;
  END;
END set_spec;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 1 DO
    WITH ports[i] DO
      dmode :=req.ready+req.floppy+req.fmttrk;
      maxsec:=  5;      minsec :=1;
      heads :=  2;      ressec:= 0;
      ssc   := 10;      secsize:=1<<ssc;
      cyls  := 80;      dsecs  :=(maxsec-minsec+1)*heads*cyls-ressec;
      rate  :=  0;      precomp:=0
    END
  END
END init;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR try: INTEGER;
BEGIN
  try:=TRIES;
  WITH ports[r.drn] DO
    REPEAT
      r.res:=ok;
      CASE r.op OF
        |req.LOCK     :
        |req.UNLOCK   :
        |req.MOUNT    :
        |req.UNMOUNT  :
        |req.FORMAT   : format(r); RETURN
        |req.READ     : read(r)
        |req.WRITE    : write(r)
        |req.SET_SPEC : set_spec(r)
        |req.GET_SPEC : get_spec(r)
      ELSE
        r.res:=err.inv_op; RETURN
      END;
      IF r.res=ok THEN RETURN END;
      try:=try-1; reset(r.drn);
    UNTIL try=0
  END
END doio;

PROCEDURE insert;
  VAR r: INTEGER;
BEGIN
  init;
  r:=fs.define_driver("fd0",""   ,0,fs.disk,doio);
  IF r#ok THEN HALT(r) END;
  r:=fs.define_driver("fd1","fd0",1,fs.disk,doio);
  IF r#ok THEN HALT(r) END;
END insert;

BEGIN
  BCB:=0A0h; BUFFER:=BCB+16;
  insert;
  env.become_ipr;
  os.suspend(os.active(),-1)
END DKqMY.
