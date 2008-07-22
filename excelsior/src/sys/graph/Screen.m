IMPLEMENTATION MODULE Screen; (* Leo & nick 18-Apr-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  def: defScreen;         IMPORT  bio: BIO;
IMPORT  err: defErrors;         IMPORT  low: lowLevel;
IMPORT  req: defRequest;        IMPORT  env: tskEnv;

VAR file: bio.FILE;
    doio: PROCEDURE (bio.FILE, VAR ARRAY OF SYSTEM.WORD);
   dummy: STATUS;

PROCEDURE bio_error; BEGIN done:=FALSE; error:=bio.error END bio_error;

PROCEDURE set_palette(p: PALETTE; from,len: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  r.op :=def._set_rgb*100h+req.CONTROL;
  r.buf:=SYSTEM.ADR(p);
  r.pos:=from; r.len:=len;
  done:=TRUE;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END set_palette;

PROCEDURE nop;
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.NOP;
  done:=TRUE;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END nop;

PROCEDURE _refresh(op,x,y,w,h: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  done:=TRUE;
  IF (w<=0) OR (h<=0) THEN RETURN END;
  IF (x+w>state^.W) OR (x<0) OR (y+h>state^.H) OR (y<0) THEN
    done:=FALSE; error:=err.bad_parm; RETURN
  END;
  r.op:=req.CONTROL+op*256;
  r.pos:=x+y*10000h;
  r.len:=w+h*10000h;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END _refresh;

PROCEDURE refresh(x,y,w,h: INTEGER);
BEGIN _refresh(def._refresh,x,y,w,h) END refresh;

PROCEDURE refreshw(x,y,w,h: INTEGER);
BEGIN _refresh(def._refreshw,x,y,w,h) END refreshw;

PROCEDURE set_ldcx(x: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  r.op:=def._set_ldcx*100h+req.CONTROL;  r.len:=x;
  done:=TRUE;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END set_ldcx;

PROCEDURE set_ldcy(y: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  r.op:=def._set_ldcy*100h+req.CONTROL;  r.len:=y;
  done:=TRUE;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END set_ldcy;

PROCEDURE op(cmd: INTEGER; SEQ args: SYSTEM.WORD);
  VAR r: req.REQUEST;
BEGIN
  done:=TRUE;
  r.op:=cmd*10000h+req.CONTROL+def.CONTROL;
  r.buf:=SYSTEM.ADR(args);
  r.len:=HIGH(args)+1;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END op;

PROCEDURE loophole(kind: INTEGER; VAR ext: SYSTEM.WORD);
BEGIN
  done:=TRUE;
  IF state=SYSTEM.ADR(dummy) THEN nop END;
  IF NOT done THEN RETURN END;
  IF state^.kind#kind THEN done:=FALSE; error:=err.unsuitable; RETURN END;
  ext:=state^.ext
END loophole;

PROCEDURE lock(f: bio.FILE; VAR R: ARRAY OF SYSTEM.WORD);
  VAR r: req.REQUEST;
BEGIN
  done:=TRUE;
  r.op:=req.LOCK;
  bio.doio(f,r);
  IF NOT bio.done THEN bio_error; bio.close(file); RETURN END;
  r.op:=req.CONTROL+def._init*256;
  bio.doio(f,r);
  IF NOT bio.done THEN bio_error; RETURN END;
  state:=r.buf;
  doio:=bio.doio;
  doio(f,R);
  IF NOT bio.done THEN bio_error; RETURN END
END lock;

PROCEDURE attach(VAL name: ARRAY OF CHAR);
  VAR s: STATE;
      f: bio.FILE;
      r: req.REQUEST;
BEGIN
  done:=TRUE;
  bio.open(f,name,'rw');
  IF NOT bio.done THEN bio_error; RETURN END;
  IF bio.equal(f,file) THEN bio.close(f); RETURN END;
  r.op:=req.UNLOCK;
  bio.doio (file,r);
  bio.close(file);
  state:=SYSTEM.ADR(dummy);
  file:=f;
  doio:=lock
END attach;

PROCEDURE start(f: bio.FILE; VAR r: ARRAY OF SYSTEM.WORD);
  VAR s: STRING;
BEGIN
  env.get_str(env.screen,s);
  IF NOT env.done THEN error:=env.error; done:=FALSE; RETURN END;
  attach(s);
  IF NOT done THEN RETURN END;
  doio(file,r);
  IF NOT done     THEN RETURN    END;
  IF NOT bio.done THEN bio_error END
END start;

PROCEDURE final;
  VAR r: req.REQUEST;
BEGIN
  IF file=bio.null THEN RETURN END;
  r.op:=req.UNLOCK;
  bio.doio(file,r);
  bio.close(file);
END final;

BEGIN
  state:=SYSTEM.ADR(dummy);
  low.zero(dummy);
  dummy.ext:=NIL;
  NEW(dummy.pal);
  file:=bio.null;
  doio:=start;
  env.final(final);
  error:=err.ok; done:=TRUE
END Screen.
