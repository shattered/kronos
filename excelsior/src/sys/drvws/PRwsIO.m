MODULE PRwsIO; (*$U+ Fil 10-Dec-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT   os: osKernel;
IMPORT   fs: osFiles;
IMPORT  req: defRequest;
IMPORT  env: tskEnv;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST ok = err.ok;

VAR name: ARRAY [0..7] OF CHAR;

    BA,BB,BC,CR: POINTER TO BITSET;

PROCEDURE put(ch: CHAR);
  VAR b: BITSET;
BEGIN
  REPEAT b:=BC^ UNTIL (NOT(4 IN b)&(7 IN b));
  BA^:=BITSET(ch);
END put;

PROCEDURE pr_doio(VAR r: req.REQUEST);
  VAR i,p,l: INTEGER;
        str: STRING;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.GET_SPEC: r.baud:=0; r.xon:=0c; r.xoff:=0c; r.smode:={};
    |req.WRITE   :
      p:=r.pos;     str^.ADR :=r.buf;
      l:=r.len;     str^.HIGH:=p+l;    r.len:=0;
      WHILE l>0 DO
        put(str[p]); INC(p); DEC(l); INC(r.len);
      END;
  ELSE
    r.res:=err.inv_op
  END
END pr_doio;

--------------------------  DRIVER  ----------------------------
                          ----------

PROCEDURE halt; BEGIN IF fs.remove_driver(name)=0 THEN END END halt;

PROCEDURE init;
  VAR r: INTEGER;
BEGIN
  BA:=ADDRESS(8200F0h+8);
  BB:=ADDRESS(8200F0h+9);
  BC:=ADDRESS(8200F0h+0Ah);
  CR:=ADDRESS(8200F0h+0Bh);
  CR^:={7,5,3,2,1};
  name:='pr0'; env.final(halt);
  r:=fs.define_driver(name,'',0,fs.tty,pr_doio);
  IF r#ok THEN HALT(r) END;
  env.put_str(env.info,name,TRUE);
END init;

BEGIN
  init; env.become_ipr;
  os.suspend(os.active(),-1);
END PRwsIO.
