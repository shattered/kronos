MODULE SCqIO; (* Leo 22-Oct-89. (c) KRONOS *)  IMPORT  SYSTEM;

IMPORT  err: defErrors;         IMPORT  fs : osFiles;
IMPORT  sio: SIOqBUS;           IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  str: Strings;

CONST ok = err.ok;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.CONTROL:
    |req.LOCK   :
    |req.UNLOCK :
  ELSE
    sio.doio(r)
  END
END doio;

PROCEDURE init(csr, trap, tno: INTEGER);
  VAR r: INTEGER;
   name: ARRAY [0..3] OF CHAR;
BEGIN
  r:=sio.init(csr,trap);
  IF r#ok THEN HALT(r) END;
  env.final(sio.stop);
  name:="si0"; name[2]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(name,"",tno,fs.tty,doio);
  IF r#ok THEN HALT(r) END;
END init;

PROCEDURE parms(VAR csr, trap, tno: INTEGER);
  PROCEDURE default; BEGIN tno:=0; csr:=177560b; trap:=60b END default;
  VAR pos: INTEGER;
       ps: STRING; done: BOOLEAN;
BEGIN
  env.get_str(env.args,ps);
  IF NOT env.done THEN HALT(err.bad_parm) END;
  pos:=0;
  str.iscan(tno ,ps,pos,done); IF NOT done THEN HALT(err.bad_parm) END;
  str.iscan(csr ,ps,pos,done); IF NOT done THEN HALT(err.bad_parm) END;
  str.iscan(trap,ps,pos,done); IF NOT done THEN HALT(err.bad_parm) END;
END parms;

PROCEDURE monitor;
BEGIN
  LOOP sio.put(sio.get()) END
END monitor;

VAR r,csr,trap,tno: INTEGER;

BEGIN
  parms(csr,trap,tno);
  init(csr,trap,tno);
  sio.monitor(monitor);
END SCqIO.
