IMPLEMENTATION MODULE CPD; (* Leg 21-Sep-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  bio: BIO;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  tsk: tskEnv;
IMPORT  tim: Time;
IMPORT  def: defCPD;
IMPORT  low: lowLevel;

CONST ok = err.ok;

TYPE WORD = SYSTEM.WORD;

VAR  file: bio.FILE;
 restore?: BOOLEAN;

     save: STATUS;
    dummy: STATUS;
     doio: PROCEDURE (bio.FILE, VAR ARRAY OF WORD);

PROCEDURE bioctl(file: bio.FILE; op: INTEGER; SEQ args: WORD);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.CONTROL+op*256;
  r.buf:=SYSTEM.ADR(args); r.len:=HIGH(args)+1; r.ofs:=0; r.pos:=0;
  bio.doio(file,r);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END bioctl;

PROCEDURE start(file: bio.FILE; VAR r: ARRAY OF WORD);
  VAR x: POINTER TO req.REQUEST;
   info: def.STATE;
BEGIN
  IF file#bio.null THEN
    doio:=bio.doio;  ioctl(def._reset);  doio(file,r);  RETURN
  END;                                                  ------
  error:=err.bad_desc;
  (* to prevent rarely calls for to not attached CPD: *)
  x:=SYSTEM.ADR(r);
  IF (x^.op=req.READ) OR (x^.op=req.WAIT) THEN tim.delay(2,tim.sec) END
END start;

PROCEDURE attach(VAL name: ARRAY OF CHAR);
  VAR new: bio.FILE;  s: STATE;
BEGIN
  bio.open(new,name,'r'); done:=bio.done;
  IF NOT done THEN error:=bio.error; RETURN END;
  bioctl(new,def._info,SYSTEM.ADR(s));
  IF NOT done THEN error:=bio.error; RETURN END;
  IF file#bio.null THEN bio.close(file) END;
  file:=new; doio:=start; state:=s; save:=state^; restore?:=TRUE
END attach;

PROCEDURE START(xxx: bio.FILE; VAR r: ARRAY OF WORD);
  VAR s: STRING;
BEGIN
  tsk.get_str(tsk.cpd,s);
  IF tsk.done THEN attach(s) ELSE done:=FALSE; error:=tsk.error END;
  IF done     THEN start(file,r) END
END START;

PROCEDURE ioctl(op: INTEGER; SEQ args: WORD);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.CONTROL+op*256;
  r.buf:=SYSTEM.ADR(args); r.len:=HIGH(args)+1; r.ofs:=0; r.pos:=0;
  doio(file,r);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END ioctl;

PROCEDURE restore(VAL state: STATUS);
BEGIN ioctl(def._restore,state) END restore;

PROCEDURE finish; BEGIN IF restore? THEN restore(save) END END finish;

----------------------------------------------------------------

PROCEDURE read(VAR x,y: INTEGER; VAR keys: BITSET);
  VAR r: req.REQUEST;
     it: RECORD x,y: INTEGER; keys: BITSET END;
BEGIN
  r.op:=req.READ;  r.buf:=SYSTEM.ADR(it);
  r.pos:=0;        r.len:=-1;  r.ofs:=0;
  done:=TRUE;      doio(file,r);
  IF done & NOT bio.done THEN
    done:=FALSE; error:=bio.error;
    x:=0;         y:=0;         keys:={}
  ELSE
    x:=it.x;      y:=it.y;      keys:=it.keys;
    state^.x:=x;  state^.y:=y;  state^.keys:=keys
  END
END read;

PROCEDURE ready(): INTEGER;
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.READY;  r.buf:=NIL;  r.pos:=0;  r.len:=0;  r.ofs:=0;
  done:=TRUE; doio(file,r);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error; RETURN 0 END;
  IF NOT done THEN error:=r.res; RETURN 0 ELSE RETURN r.len END
END ready;

PROCEDURE wait(time: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  IF time<0 THEN time:=0 END;
  r.op:=req.WAIT; r.buf:=NIL;  r.len:=time;  r.pos:=0;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END
END wait;

PROCEDURE reset; BEGIN ioctl(def._reset)       END reset;

PROCEDURE nop;
  VAR r: req.REQUEST;
BEGIN r.op:=req.NOP; doio(file,r) END nop;

BEGIN
  low.zero(dummy);
  file:=bio.null; restore?:=FALSE; doio:=START; error:=ok;
  state:=SYSTEM.ADR(dummy);
  tsk.final(finish);
END CPD.
