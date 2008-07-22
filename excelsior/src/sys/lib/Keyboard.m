IMPLEMENTATION MODULE Keyboard; (* Ned 21-Sep-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  bio: BIO;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  def: defKeyboard;
IMPORT  tsk: tskEnv;
IMPORT  tim: Time;
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
  (* to prevent rarely calls for to not attached keyboard: *)
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
  file:=new; doio:=start; state:=s; save:=state^;
  state^.buf:=0c;  state^.togs:={};
  restore?:=TRUE
END attach;

PROCEDURE START(xxx: bio.FILE; VAR r: ARRAY OF WORD);
  VAR s: STRING;
BEGIN
  tsk.get_str(tsk.key,s);
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

PROCEDURE read(VAR ch: CHAR);
  VAR r: req.REQUEST;
BEGIN
  ch:=0c;
  r.op:=req.READ;  r.buf:=SYSTEM.ADR(ch);  r.pos:=0;  r.len:=1;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END;
  IF done THEN state^.buf:=ch END
END read;

PROCEDURE scan(VAR ch: CHAR; VAR togs: BITSET);
  VAR s: ARRAY [0..3] OF CHAR;
BEGIN
  ch:=0c; togs:={}; s:="" 0c 0c 0c;
  done:=TRUE;
  ioctl(def._readtogs,SYSTEM.ADR(s));
  IF done THEN
    ch:=s[0]; togs:=BITSET(s[1]); state^.buf:=ch; state^.togs:=togs
  END
END scan;

PROCEDURE drop;
  VAR ch: CHAR;
BEGIN
  read(ch)
END drop;

PROCEDURE swallow;
BEGIN
  WHILE ready()>0 DO drop END
END swallow;

PROCEDURE ready(): INTEGER;
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.READY;  r.buf:=NIL;  r.pos:=0;  r.len:=0;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
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

----------------------------------------------------------------

PROCEDURE bell(n: INTEGER);
BEGIN
  IF n<=0 THEN n:=1 END;
  ioctl(def._bell,n)
END bell;

PROCEDURE set_raw    (on: INTEGER); BEGIN ioctl(def._raw,on)      END set_raw;
PROCEDURE set_bell  (f,d: INTEGER); BEGIN ioctl(def._bell_fd,f,d) END set_bell;
PROCEDURE user_break (b : BREAK);   BEGIN ioctl(def._ubreak,b)    END user_break;
PROCEDURE set_break  (on: INTEGER); BEGIN ioctl(def._break,on)    END set_break;
PROCEDURE set_shift  (on: INTEGER); BEGIN ioctl(def._shift,on)    END set_shift;
PROCEDURE set_caps   (on: INTEGER); BEGIN ioctl(def._caps,on)     END set_caps;
PROCEDURE set_nums   (on: INTEGER); BEGIN ioctl(def._nums,on)     END set_nums;
PROCEDURE set_foreign(on: INTEGER); BEGIN ioctl(def._foreign,on)  END set_foreign;
PROCEDURE set_autorep(on: INTEGER); BEGIN ioctl(def._autorep,on)  END set_autorep;
PROCEDURE reset;                    BEGIN ioctl(def._reset)       END reset;

PROCEDURE nop;
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.NOP;
  doio(file,r);
END nop;

PROCEDURE break0(n: INTEGER); END break0;

BEGIN
  low.zero(dummy); dummy.ubrk:=break0;
  file:=bio.null; restore?:=FALSE; doio:=START; error:=ok;
  state:=SYSTEM.ADR(dummy);
  tsk.final(finish)
END Keyboard.
