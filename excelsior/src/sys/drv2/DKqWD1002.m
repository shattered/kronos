MODULE DKqWD1002; (* 03-Nov-89. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD, ADDRESS, ADR;

IMPORT  cod: defCodes;          IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT   os: osKernel;
IMPORT  req: defRequest;        IMPORT   fs: osFiles;
IMPORT  env: tskEnv;            IMPORT  tty: Terminal;

CONST
  ok = err.ok;
  DAT = 176400b DIV 2;
  ERR = DAT+1;
  SNO = DAT+2;
  SEC = DAT+3;
  CYLa= DAT+4;
  CYLb= DAT+5;
  DRV = DAT+6;
  CSR = DAT+7;

  secsz = 512;
  secno = 16;
  heads = 2;
  trkno = 612;

VAR
  time : INTEGER;

PROCEDURE inp(reg: WORD): BITSET; CODE cod.inp  END inp;
PROCEDURE out(reg,data: WORD);    CODE cod.out  END out;

PROCEDURE error(): INTEGER;
BEGIN
  IF NOT (6 IN inp(CSR)) THEN RETURN err.not_ready END;
  IF 5 IN inp(CSR) THEN RETURN err.write_fail END;
  IF NOT (4 IN inp(CSR)) THEN RETURN err.seek_err END;
  IF inp(ERR)#{} THEN RETURN err.data_crc END;
  IF 0 IN inp(CSR) THEN RETURN err.io_error END;
  RETURN ok;
END error;

PROCEDURE inp4(n: INTEGER): INTEGER;
CODE
  cod.copt cod.inp cod.swap
  cod.copt cod.inp cod.swap
  cod.copt cod.inp cod.swap
  cod.inp
  8 cod.rol cod.or
  8 cod.rol cod.or
  8 cod.rol cod.or
END inp4;

PROCEDURE read_sec(u,sec,len: INTEGER; a: ADDRESS): INTEGER;
  VAR
    bf  : POINTER TO ARRAY [0..127] OF INTEGER;
    head: INTEGER;
    trk : INTEGER;
    i   : INTEGER;
BEGIN
  WHILE inp(CSR)*{7}#{} DO END;
  IF inp(CSR)*{3}#{} THEN
    WHILE inp(CSR)*{3}#{} DO out(DAT,0) END;
    RETURN err.prog_err;
  END;
  head:=sec  DIV secno;
  trk :=head DIV heads;
  head:=head MOD heads;
  sec :=sec  MOD secno;
  out(DRV,u*8+20h+head);
  out(CYLa,trk MOD 100h);
  out(CYLb,trk DIV 100h);
  out(SNO,len);
  out(SEC,sec);
  out(CSR,24h); -- read sectors
  WHILE len>0 DO
    REPEAT
      IF inp(CSR)*{0}#{} THEN RETURN error() END;
    UNTIL inp(CSR)*{3}#{};
    bf:=a; INC(a,128);
(*$T-*)
    FOR i:=0 TO 127 BY 4 DO
      bf^[i  ]:=inp4(DAT);
      bf^[i+1]:=inp4(DAT);
      bf^[i+2]:=inp4(DAT);
      bf^[i+3]:=inp4(DAT);
    END;
(*$T+*)
    DEC(len);
  END;
  WHILE inp(CSR)*{7}#{} DO END;
  IF inp(CSR)*{3}#{} THEN
    WHILE inp(CSR)*{3}#{} DO IF inp(CSR)#{} THEN END END;
    RETURN err.prog_err;
  END;
  IF inp(CSR)*{0}#{} THEN RETURN error() END;
  RETURN ok;
END read_sec;

PROCEDURE write_sec(u,sec,len: INTEGER; a: ADDRESS): INTEGER;
  VAR
    bf  : POINTER TO ARRAY [0..511] OF CHAR;
    head: INTEGER;
    trk : INTEGER;
    i   : INTEGER;
BEGIN
  WHILE inp(CSR)*{7}#{} DO END;
  IF inp(CSR)*{3}#{} THEN
    WHILE inp(CSR)*{3}#{} DO out(DAT,0) END; RETURN err.prog_err
  END;
  head:=sec  DIV secno;
  trk :=head DIV heads;
  head:=head MOD heads;
  sec :=sec  MOD secno;
  out(DRV,u*8+20h+head);
  out(CYLa,trk MOD 100h);
  out(CYLb,trk DIV 100h);
  out(SNO,len);
  out(SEC,sec);
  out(CSR,34h); -- write sectors
  WHILE len>0 DO
    bf:=a; INC(a,128);
    REPEAT UNTIL inp(CSR)*{0,3}#{};
    IF inp(CSR)*{0}#{} THEN RETURN error() END;
(*$T-*)
    FOR i:=0 TO 511 DO out(DAT,bf^[i]) END;
(*$T+*)
    DEC(len);
  END;
  WHILE inp(CSR)*{7}#{} DO END;
  IF inp(CSR)*{0}#{} THEN RETURN error() END;
  RETURN ok;
END write_sec;

PROCEDURE format_trk(u,trk,head: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  WHILE 7 IN inp(CSR) DO END;
  out(DRV,u*8+20h+head);
  out(CYLa,trk MOD 100h);
  out(CYLb,trk DIV 100h);
  IF trk=0 THEN
    out(CSR,10h); -- restore
  ELSE
    out(CSR,70h); -- seek
  END;
  WHILE 7 IN inp(CSR) DO END;
  IF 0 IN inp(CSR) THEN RETURN error() END;
  out(SNO,16);
  out(CSR,50h); -- format
  FOR i:=0 TO 255 DO
    REPEAT UNTIL inp(CSR)*{3}#{}; out(DAT,00h);
    REPEAT UNTIL inp(CSR)*{3}#{}; out(DAT,i MOD 16);
  END;
  WHILE 7 IN inp(CSR) DO END;
  IF 0 IN inp(CSR) THEN RETURN error() END;
  RETURN ok;
END format_trk;

PROCEDURE readw(VAR r: req.REQUEST);
BEGIN
  r.res:=read_sec(r.drn,r.ofs,r.len,r.buf);
END readw;

PROCEDURE writew(VAR r: req.REQUEST);
BEGIN
  r.res:=write_sec(r.drn,r.ofs,r.len,r.buf);
END writew;

PROCEDURE format(VAR r: req.REQUEST);
BEGIN
  IF r.ofs<0 THEN RETURN END;
  r.res:=format_trk(r.drn,r.ofs DIV secno DIV heads,r.ofs DIV secno MOD heads);
END format;

PROCEDURE seek(VAR r: req.REQUEST);
BEGIN
END seek;

PROCEDURE park(VAR r: req.REQUEST);
BEGIN
  WHILE 7 IN inp(CSR) DO END;
  out(DRV,r.drn*8+20h);
  out(CYLa,(trkno-1) MOD 100h);
  out(CYLb,(trkno-1) DIV 100h);
  out(CSR,70h); -- seek
  WHILE 7 IN inp(CSR) DO END;
  out(DRV,8+20h);
END park;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  r.dmode  :=req.ready+req.wint+req.fmttrk;
  r.dsecs  :=secno*heads*trkno;
  r.ssc    :=9;
  r.secsize:=secsz;
  r.cyls   :=trkno;
  r.heads  :=heads;
  r.minsec :=0;
  r.maxsec :=15;
  r.ressec :=0;
  r.rate   :=0;
  r.precomp:=0;
END get_spec;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR try: INTEGER;
BEGIN
  try:=3;
  REPEAT
    r.res:=ok;
    CASE r.op OF
      |req.MOUNT    :
      |req.UNMOUNT  :
      |req.READ     : readw(r); time:=10;
      |req.WRITE    : writew(r); time:=10;
      |req.POWER_OFF: park(r);   RETURN
      |req.FORMAT   : format(r); time:=10; RETURN
--    |req.SET_SPEC : set_spec(r)
      |req.GET_SPEC : get_spec(r)
--    |req.SEEK     : seek(r)
    ELSE
      r.res:=err.inv_op; RETURN
    END;
    IF r.res=ok           THEN RETURN END;
    try:=try-1
  UNTIL try=0
END doio;

PROCEDURE insert;
  VAR r: INTEGER;
BEGIN
  WHILE 7 IN inp(CSR) DO
    WHILE 3 IN inp(CSR) DO IF inp(DAT)#{} THEN END END;
  END;
  out(CSR,10h);
  WHILE 7 IN inp(CSR) DO END;
  r:=fs.define_driver("wd0",""   ,0,fs.disk,doio);
  IF r#ok THEN HALT(r) END;
END insert;

VAR off: req.REQUEST; wd0: fs.FILE;

BEGIN
  insert;
  time:=10;
  env.become_ipr;
  LOOP
    os.suspend(os.active(),100);
    IF time=1 THEN
      time:=0;
      off.res:=fs.open_dev(wd0,'wd0',{});
      IF off.res#ok THEN HALT(off.res) END;
      off.res:=ok; off.op:=req.POWER_OFF; off.drn:=0;
      off.ofs:=-1; off.buf:=NIL; off.len:=0;
      off.res:=fs.doio(wd0,off);
      off.res:=fs.close(wd0);
    ELSIF time>1 THEN DEC(time);
    END;
  END;
END DKqWD1002.
