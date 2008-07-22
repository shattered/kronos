MODULE VD; (* Sem 19-Oct-95. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  os : osKernel;
IMPORT   fs: osFiles;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  bio: BIO;
FROM SYSTEM IMPORT ADDRESS;

CONST
  SSC   =   12;
  SECSZ = 1<<SSC;
  BLK   = 4096;

PROCEDURE get_spec(VAR r: req.REQUEST);
  VAR sz: INTEGER;
BEGIN
  IF r.drn=0 THEN
    sz:=4896;
  ELSE
    sz:=4896;
  END;
  r.dmode :=req.ready+req.wint;
  r.ssc   :=SSC;       r.secsize:=SECSZ;
  r.minsec:=-1;        r.maxsec :=-1;
  r.cyls  :=-1;        r.ressec :=-1;
  r.heads :=-1;        r.precomp:=-1;
  r.rate  :=-1;        r.dsecs  :=sz;
END get_spec;

PROCEDURE set_spec(VAR r: req.REQUEST);
BEGIN
END set_spec;

PROCEDURE drive(d: INTEGER): BOOLEAN;
CODE 94h END drive;

PROCEDURE read(b: INTEGER; buf: ADDRESS; szB: INTEGER): BOOLEAN;
CODE 92h END read;

PROCEDURE write(b: INTEGER; buf: ADDRESS; szB: INTEGER): BOOLEAN;
CODE 93h END write;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    res:=err.ok;
    CASE r.op OF
      |req.NOP      :
      |req.READ     : IF drive(drn) OR
                         read(ofs,buf,len*SECSZ) THEN res:=err.io_error END;
      |req.WRITE    : IF drive(drn) OR
                         write(ofs,buf,len*SECSZ) THEN res:=err.io_error END;
      |req.MOUNT    :
      |req.UNMOUNT  :
      |req.SEEK     :
      |req.POWER_OFF:
      |req.GET_SPEC : get_spec(r)
      |req.SET_SPEC : set_spec(r)
      |req.FORMAT   :
    ELSE
      r.res:=err.inv_op;
    END;
  END;
END doio;

PROCEDURE define(nm: ARRAY OF CHAR; drn: INTEGER);
  VAR   i: INTEGER;
BEGIN
  i:=fs.define_driver(nm,nm,drn,fs.disk,doio);
  IF i#err.ok THEN ASSERT(FALSE,i) END;
  env.put_str(env.info,nm,TRUE);
END define;

BEGIN
  define('vd0',0);
  define('vd1',1);
  define('vd2',2);
  define('vd3',3);
  env.become_ipr;
  os.suspend(os.active(),-1)
END VD.
