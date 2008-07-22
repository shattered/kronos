MODULE XD; (* Leo 20-Jan-98. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  os : osKernel;
IMPORT   fs: osFiles;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  bio: BIO;
IMPORT  tim: Time;
FROM SYSTEM IMPORT ADDRESS;

CONST
  SSC   =   9;
  SECSZ = 1<<SSC;
  BLK   = 4096;

  MOUNT = 1;
  DISMOUNT  = 2;
  GETSIZE4K = 3;
  READ  = 4;
  WRITE = 5;
  TIME  = 6;
  GETSPEC = 8;

PROCEDURE diskop(op,d,s: INTEGER; buf: ADDRESS; len: INTEGER): BOOLEAN;
CODE 92h END diskop;

PROCEDURE get_spec(VAR r: req.REQUEST);
  VAR sz: INTEGER;
BEGIN
  IF diskop(GETSPEC, r.drn, 0, SYSTEM.ADR(r), 0) THEN
     RETURN;
  END;
  IF NOT diskop(GETSIZE4K, r.drn, 0, SYSTEM.ADR(sz), 0) THEN
     r.res := err.inv_op;
     RETURN
  END;
  r.dmode :=req.ready+req.wint;
  r.ssc   :=SSC;       r.secsize:=SECSZ;
  r.minsec:=-1;        r.maxsec :=-1;
  r.cyls  :=-1;        r.ressec := 0;
  r.heads :=-1;        r.precomp:=-1;
  r.rate  :=-1;        r.dsecs  :=sz*(4096/SECSZ);
  r.res   := err.ok;
END get_spec;

PROCEDURE set_spec(VAR r: req.REQUEST);
BEGIN
END set_spec;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR t: RECORD y,m,d,hr,mn,sc: INTEGER END;
   time: INTEGER;
BEGIN
  IF diskop(TIME, 0, 0, SYSTEM.ADR(t), SIZE(t)) THEN
    WITH t DO
      time := tim.pack(y,m,d,hr,mn,sc);
      tim.set_time(time)
    END
  END;
  WITH r DO
    res := err.ok;
    CASE op OF
      |req.NOP      :
      |req.READ     : IF NOT diskop(READ, drn,ofs,buf,len*SECSZ) THEN
                        res:=err.io_error
                      END;
      |req.WRITE    : IF NOT diskop(WRITE,drn,ofs,buf,len*SECSZ) THEN
                        res:=err.io_error
                      END;
      |req.MOUNT    :
                      IF NOT diskop(MOUNT, r.drn, 0, NIL, 0) THEN
                         res := err.inv_op;
                      END
      |req.UNMOUNT  :
                      IF NOT diskop(DISMOUNT, r.drn, 0, NIL, 0) THEN
                         res := err.inv_op;
                      END
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
  define('xd0',0);
  define('xd1',1);
  define('xd2',2);
  define('xd3',3);
  define('xd4',4);
  define('xd5',5);
  define('xd6',6);
  define('xd7',7);
  env.become_ipr;
  os.suspend(os.active(),-1)
END XD.
