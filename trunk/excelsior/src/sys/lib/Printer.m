IMPLEMENTATION MODULE Printer; (* Leg 21-Jan-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  tsk: tskEnv;
IMPORT  def: defPrinter;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  bio: BIO;
IMPORT  low: lowLevel;

CONST ok = err.ok;

TYPE WORD = SYSTEM.WORD;

VAR  file: bio.FILE;
 restore?: BOOLEAN;

     save: STATUS;
    dummy: STATUS;

     doio: PROCEDURE(bio.FILE, VAR ARRAY OF WORD);

PROCEDURE bioctl(file: bio.FILE; op: INTEGER; SEQ args: WORD);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.CONTROL+op*256;
  r.buf:=SYSTEM.ADR(args); r.len:=HIGH(args)+1; r.ofs:=0; r.pos:=0;
  bio.doio(file,r);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END bioctl;

PROCEDURE start(xxx: bio.FILE; VAR r: ARRAY OF WORD);
  VAR info: def.STATE;
BEGIN
  doio:=bio.doio;   ioctl(def._reset);
  doio(file,r);     done:=bio.done;
  IF NOT done THEN error:=bio.error END
END start;

PROCEDURE attach(VAL name: ARRAY OF CHAR);
  VAR new: bio.FILE;  s: STATE;
BEGIN
  bio.open(new,name,'w');
  done:=bio.done;
  IF NOT done THEN error:=bio.error; RETURN END;
  bioctl(new,def._info,SYSTEM.ADR(s)); done:=bio.done;
  IF NOT done THEN bio.close(new); RETURN END;
  IF file#bio.null THEN bio.close(file) END;
  file:=new;  doio:=start;  state:=s;  save:=state^;  restore?:=TRUE
END attach;

PROCEDURE START(xxx: bio.FILE; VAR r: ARRAY OF WORD);
  VAR s: STRING;
BEGIN
  tsk.get_str(tsk.lp,s);
  IF tsk.done THEN attach(s) ELSE done:=FALSE; error:=tsk.error; RETURN END;
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

---------------------------  OUTPUT  --------------------------
                           ----------

PROCEDURE Write(ch: CHAR);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.WRITE;  r.buf:=SYSTEM.ADR(ch);  r.pos:=0;  r.len:=1;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END
END Write;

PROCEDURE WriteString(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
      r: req.REQUEST;
BEGIN
  (*$<*) (*$T-*)
  IF (HIGH(s)<0) OR (s[0]=0c) THEN RETURN END;
  i:=0;
  REPEAT i:=i+1 UNTIL (i>HIGH(s)) OR (s[i]=0c);
  r.op:=req.WRITE;  r.buf:=SYSTEM.ADR(s);  r.pos:=0;  r.len:=i;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END;
  (*$>*)
END WriteString;

PROCEDURE WriteLn; BEGIN ioctl(def._write_ln) END WriteLn;

PROCEDURE repeat(ch: CHAR; no: INTEGER);
BEGIN ioctl(def._repeat,ch,no) END repeat;

PROCEDURE write(VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.WRITE;  r.buf:=SYSTEM.ADR(s);  r.pos:=pos;  r.len:=len;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END
END write;


----------------------- TYPEFACE CONTROL ----------------------
                       ------------------

PROCEDURE set_font(no: INTEGER);
BEGIN ioctl(def._font,no) END set_font;

PROCEDURE set_something(on: INTEGER);
BEGIN ioctl(def._something,on) END set_something;

PROCEDURE set_reverse(on: INTEGER);
BEGIN ioctl(def._reverse,on) END set_reverse;

PROCEDURE set_underline(on: INTEGER);
BEGIN ioctl(def._underline,on) END set_underline;

PROCEDURE set_Wx2(on: INTEGER);
BEGIN ioctl(def._Wx2,on) END set_Wx2;

PROCEDURE set_Hx2(on: INTEGER);
BEGIN ioctl(def._Hx2,on) END set_Hx2;

PROCEDURE set_ground(c: INTEGER);
BEGIN ioctl(def._ground,c) END set_ground;

--------------------- CARRIGE MOVEMENT ------------------------
                     ------------------

PROCEDURE fflf(n: INTEGER); (* Forward Full Line Feed *)
BEGIN ioctl(def._fflf,n) END fflf;

PROCEDURE fhlf(n: INTEGER); (* Forward Half Line Feed *)
BEGIN ioctl(def._fhlf,n) END fhlf;

PROCEDURE bflf(n: INTEGER); (* Back    Full Line Feed *)
BEGIN ioctl(def._bflf,n) END bflf;

PROCEDURE bhlf(n: INTEGER); (* Back    Half Line Feed *)
BEGIN ioctl(def._bhlf,n) END bhlf;

PROCEDURE fwd (pixels: INTEGER);
BEGIN ioctl(def._fwd,pixels) END fwd;

PROCEDURE back(pixels: INTEGER);
BEGIN ioctl(def._back,pixels) END back;

PROCEDURE right(pixels: INTEGER); (* move carrige left  *)
BEGIN ioctl(def._right,pixels) END right;

PROCEDURE left (pixels: INTEGER); (* move carrige right *)
BEGIN ioctl(def._left,pixels) END left;

PROCEDURE eject; BEGIN ioctl(def._eject) END eject;

--------------------------   GRAPHIC  --------------------------
                          ------------

PROCEDURE set_density(no: INTEGER);
BEGIN ioctl(def._density,no) END set_density;

PROCEDURE paint(VAL map: ARRAY OF SYSTEM.WORD; w,h: INTEGER; dx,dy: INTEGER);
BEGIN ioctl(def._paint,map,w,h,dx,dy) END paint;

PROCEDURE set_awp(on: INTEGER);
BEGIN ioctl(def._autowrap,on) END set_awp;

PROCEDURE set_raw(on: INTEGER);
BEGIN ioctl(def._raw,on) END set_raw;

PROCEDURE load_font(no: INTEGER; from,to: CHAR;
                    VAL font: ARRAY OF WORD);
BEGIN
  ioctl(def._load_font,no,from,to,font,SIZE(font));
END load_font;

PROCEDURE set_attr(no,val: INTEGER);
BEGIN ioctl(def._set_attr,no,val) END set_attr;

PROCEDURE get_attr(no: INTEGER): INTEGER;
  VAR val: INTEGER;
BEGIN
  ioctl(def._get_attr,no,SYSTEM.ADR(val));
  IF done THEN RETURN val ELSE RETURN 0 END
END get_attr;

PROCEDURE reset;
  VAR ignore: def.STATE;
BEGIN ioctl(def._reset) END reset;

PROCEDURE nop;
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.NOP;
  doio(file,r);
END nop;

----------------------------------------------------------------

PROCEDURE init;
BEGIN
  low.zero(dummy);
  file:=bio.null; restore?:=FALSE; doio:=START; error:=ok;
  state:=SYSTEM.ADR(dummy);
  tsk.final(finish)
END init;

BEGIN
  init
END Printer.
