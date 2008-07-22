IMPLEMENTATION MODULE Terminal; (* Ned 25-Aug-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  def: defTerminal;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  fmt: Formats;
IMPORT  bio: BIO;
IMPORT  tsk: tskEnv;
IMPORT  lex: Lexicon;
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
  tsk.get_str(tsk.tty,s);
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

PROCEDURE restore(VAL st: STATUS);
BEGIN ioctl(def._restore,st) END restore;

PROCEDURE finish; BEGIN IF restore? THEN restore(save) END END finish;

---------------------------  OUTPUT  --------------------------
                           ----------

PROCEDURE Write(ch: CHAR);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.WRITE;  r.buf:=SYSTEM.ADR(ch);  r.pos:=0;  r.len:=1;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  iolen:=r.len;
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
  iolen:=r.len;
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END;
  (*$>*)
END WriteString;

PROCEDURE write(VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.WRITE;  r.buf:=SYSTEM.ADR(s);  r.pos:=pos;  r.len:=len;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  iolen:=r.len;
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END
END write;

PROCEDURE ws(x: WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.WRITE;  r.buf:=SYSTEM.ADR(s);  r.pos:=pos;  r.len:=len;  r.ofs:=0;
  done:=TRUE;
  doio(file,r);
  INC(iolen,r.len);
  IF done & NOT bio.done THEN done:=FALSE; error:=bio.error END;
END ws;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ args: WORD);
BEGIN
  done:=TRUE; iolen:=0; fmt.format(0,ws,format,args)
END print;

PROCEDURE perror(code: INTEGER; VAL f: ARRAY OF CHAR; SEQ args: WORD);
  VAR s: ARRAY [0..127] OF CHAR;
BEGIN
  s:="";
  lex.perror(s,code,f,args);
  WriteString(s);
  IF NOT done THEN RETURN END;
  done:=lex.done;
  IF NOT done THEN error:=lex.error END
END perror;

PROCEDURE WriteLn;
BEGIN
  write("" 15c 12c,0,2)
END WriteLn;

PROCEDURE Show(VAL s: ARRAY OF CHAR);
BEGIN WriteString(s); WriteLn END Show;

---------------------------  SCREEN  --------------------------
                           ----------


PROCEDURE home;   BEGIN ioctl(def._home) END home;

PROCEDURE bottom; BEGIN ioctl(def._bottom) END bottom;

PROCEDURE erase(how: INTEGER);
BEGIN ioctl(def._erase,how) END erase;

PROCEDURE erase_line(how: INTEGER);
BEGIN ioctl(def._erase_line,how) END erase_line;

PROCEDURE erase_chars(no: INTEGER);
BEGIN ioctl(def._erase_chars,no) END erase_chars;

PROCEDURE repeat(ch: CHAR; no: INTEGER);
BEGIN ioctl(def._repeat,ch,no) END repeat;

PROCEDURE roll_down(n: INTEGER);
BEGIN ioctl(def._roll_down,n) END roll_down;

PROCEDURE roll_up(n: INTEGER);
BEGIN ioctl(def._roll_up,n) END roll_up;

PROCEDURE scroll_down(n: INTEGER);
BEGIN ioctl(def._scroll_down,n) END scroll_down;

PROCEDURE scroll_up(n: INTEGER);
BEGIN ioctl(def._scroll_up,n) END scroll_up;

PROCEDURE set_pos(line: INTEGER; col: INTEGER);
BEGIN ioctl(def._set_pos,line,col) END set_pos;

PROCEDURE up(n: INTEGER);
BEGIN ioctl(def._up,n) END up;

PROCEDURE down(n: INTEGER);
BEGIN ioctl(def._down ,n) END down;

PROCEDURE left(n: INTEGER);
BEGIN ioctl(def._left ,n) END left;

PROCEDURE right(n: INTEGER);
BEGIN ioctl(def._right,n) END right;

PROCEDURE ins_char(n: INTEGER);
BEGIN ioctl(def._ins_char,n) END ins_char;

PROCEDURE del_char(n: INTEGER);
BEGIN ioctl(def._del_char,n) END del_char;

PROCEDURE ins_line(n: INTEGER);
BEGIN ioctl(def._ins_line,n) END ins_line;

PROCEDURE del_line(n: INTEGER);
BEGIN ioctl(def._del_line,n) END del_line;

PROCEDURE get_state(VAR s: STATE);
BEGIN ioctl(def._info,s) END get_state;

PROCEDURE set_smooth(on: INTEGER);
BEGIN ioctl(def._smooth_scroll,on) END set_smooth;

PROCEDURE set_awp(on: INTEGER);
BEGIN ioctl(def._autowrap,on) END set_awp;

PROCEDURE set_raw(on: INTEGER);
BEGIN ioctl(def._raw,on) END set_raw;

PROCEDURE set_scr(no: INTEGER);
BEGIN ioctl(def._screen,no) END set_scr;

PROCEDURE set_cursor(on: INTEGER);
BEGIN ioctl(def._cursor,on) END set_cursor;

PROCEDURE set_reverse(on: INTEGER);
BEGIN ioctl(def._reverse,on) END set_reverse;

PROCEDURE set_underline(on: INTEGER);
BEGIN ioctl(def._underline,on) END set_underline;

PROCEDURE set_color(c: INTEGER);
BEGIN ioctl(def._color,c) END set_color;

PROCEDURE set_back(c: INTEGER);
BEGIN ioctl(def._background,c) END set_back;

PROCEDURE set_blinking(on: INTEGER);
BEGIN ioctl(def._blinking,on) END set_blinking;

PROCEDURE set_something(on: INTEGER);
BEGIN ioctl(def._something,on) END set_something;

PROCEDURE set_cinter(on: INTEGER);
BEGIN ioctl(def._cinter,on) END set_cinter;

PROCEDURE set_font(no: INTEGER);
BEGIN ioctl(def._font,no) END set_font;

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
END Terminal.
