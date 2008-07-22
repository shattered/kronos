IMPLEMENTATION MODULE bcText; (* brd  21-Apr-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  bio: BIO;
IMPORT  tty: Terminal;
IMPORT  str: Strings;
IMPORT  wnd: pmWnd;
IMPORT  mem: Heap;

TYPE  process  = POINTER TO RECORD G,L,PC,M,S,H,T: INTEGER END;

VAR pmagic: INTEGER;

PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;

---------------------------- MEMORY ----------------------------

PROCEDURE allocate(VAR a: SYSTEM.ADDRESS; size: INTEGER);
BEGIN
  mem.allocate(a,size); done:=mem.done;
  IF NOT done THEN mem_error END
END allocate;

PROCEDURE reallocate(VAR    a: SYSTEM.ADDRESS;
                     VAR high: INTEGER; len,bytesperelem: INTEGER);
BEGIN
  mem.reallocate(a,high,len,bytesperelem); done:=mem.done;
  IF NOT done THEN mem_error END
END reallocate;

WITH STORAGE (NEW    : allocate;
              DISPOSE: mem.deallocate;
              RESIZE : reallocate);

----------------------------------------------------------------

PROCEDURE setm(m: BITSET); CODE cod.setm END setm;
PROCEDURE getm(): BITSET ; CODE cod.getm END getm;
PROCEDURE active(): process; CODE cod.activ END active;
PROCEDURE ca(VAL s: ARRAY OF INTEGER): SYSTEM.ADDRESS; CODE cod.drop END ca;

PROCEDURE muldiv(w: INTEGER; mul,div: INTEGER): INTEGER;
  VAR W: INTEGER; p: process;
BEGIN
  W:=w; p:=active(); p^.T:=0;
  setm(getm()-{31}); w:=(w*mul) DIV div; setm(getm()+{31});
  IF p^.T=41h THEN
    IF W>mul THEN w:=(W DIV div)*mul ELSE w:=(mul DIV div)*W END;
  END;
  RETURN w
END muldiv;

(*
PROCEDURE muldiv(a,b,c: INTEGER): INTEGER;  (* a*b/c *)
  VAR i: INTEGER;
BEGIN
  IF a=MIN(INTEGER) THEN RETURN a DIV c * b END;
  IF b=MIN(INTEGER) THEN RETURN b DIV c * a END;
  IF ABS(a)>ABS(b)  THEN i:=a; a:=b; b:=i   END;
  IF a > MAX(INTEGER) DIV b THEN RETURN a DIV c * b END;
  RETURN a *b DIV c
END muldiv;
*)

PROCEDURE slope(f: PFONT; VAR x,y: INTEGER);
  VAR s,c,a,b: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  s:=f^.ss; c:=f^.sc; a:=x; b:=y;           -- /x\   / sin b   cos b \ /a\
  x:= muldiv(a,c,1000)- muldiv(b,s,1000);   -- | | = |               | | |
  y:= muldiv(a,s,1000)+ muldiv(b,c,1000)    -- \y/   \ cos b  -sin b / \b/
END slope;

PROCEDURE write(W: WINDOW; T: TOOL; f: PFONT; VAR x,y: INTEGER; ch: CHAR);

  PROCEDURE compute(VAR a,b,a1,b1: INTEGER);
  BEGIN
    WITH f^ DO
      a:= muldiv(a,W,w); a1:= muldiv(a1,W,w);
      b:= muldiv(b,H,h); b1:= muldiv(b1,H,h);
      a :=a + muldiv(b ,f^.ic,1000); b := muldiv(b ,f^.is,1000);
      a1:=a1+ muldiv(b1,f^.ic,1000); b1:= muldiv(b1,f^.is,1000);
      slope(f,a,b); slope(f,a1,b1)
    END
  END compute;

VAR i,p,a,b,A,B,l,h: INTEGER;

BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  l:=muldiv(f^.propW[ch],f^.W,f^.w); h:=0;
  slope(f,l,h);
  FOR i:=0 TO HIGH(f^.ptr[ch]) DO
    p:=f^.ptr[ch][i];
    a:=INTEGER(BITSET(p)*{0..7}); p:=p >> 8;
    b:=INTEGER(BITSET(p)*{0..7}); p:=p >> 8;
    A:=INTEGER(BITSET(p)*{0..7}); p:=p >> 8;
    B:=INTEGER(BITSET(p)*{0..7});
    compute(a,b,A,B);
    wnd.line(W,T,a+x,b+y,A+x,B+y);
  END;
  x:=x+l;
  y:=y+h
END write;

PROCEDURE write_ch(W: WINDOW; T: TOOL; f: PFONT; VAR x,y: INTEGER; ch: CHAR);
  VAR dx,dy: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  write(W,T,f,x,y,ch)
END write_ch;

PROCEDURE write_str(W: WINDOW; T: TOOL; f: PFONT; VAR x,y: INTEGER; s: ARRAY OF CHAR);
  VAR i,dx,dy: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO
    write(W,T,f,x,y,s[i]); INC(i)
  END
END write_str;

PROCEDURE newstr(VAR s: STRING; VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  NEW(s,0);  done:=TRUE;
  IF (HIGH(fmt)<0) OR (fmt="") THEN RETURN END;
  REPEAT
    RESIZE(s,BYTES(s)+32);
    IF NOT done THEN RETURN END;
    str.print(s,fmt,arg);
  UNTIL (BYTES(s)>=256) OR (str.len(s)<HIGH(s))
END newstr;

PROCEDURE print (wnd: WINDOW; tool: TOOL; fnt: PFONT; VAR x,y: INTEGER;
                               fmt: ARRAY OF CHAR; SEQ  arg: SYSTEM.WORD);
  VAR s: STRING;
BEGIN
  newstr(s,fmt,arg);
  write_str(wnd,tool,fnt,x,y,s)
END print;

PROCEDURE len(f: PFONT; fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;
  VAR l,i: INTEGER;
        s: STRING;
BEGIN
  l:=0;
  newstr(s,fmt,arg);
  FOR i:=0 TO HIGH(s) DO l:= l+f^.propW[s[i]] END;
  RETURN l * muldiv(1000,f^.W,f^.w) DIV 1000
END len;

PROCEDURE sqrt(a: INTEGER): INTEGER;
  VAR s,b: INTEGER;
BEGIN
  ASSERT(a>=0);
  IF a IN {0,1} THEN RETURN a END;
  s:=2; b:=a DIV s;
  WHILE (ABS(s-b)>1) DO s:=(a DIV b + b) DIV 2; b:=a DIV s END;
  IF s>b THEN RETURN s ELSE RETURN b END
END sqrt;

PROCEDURE font_ital(VAR f: PFONT; x,y: INTEGER);
  VAR l: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  IF (x=0) & (y=0) THEN f^.ic:=1; f^.is:=0 END;
  l:=sqrt(x*x+y*y);
  f^.ic:= muldiv(x,1000,l);
  f^.is:= muldiv(y,1000,l);
END font_ital;

PROCEDURE font_size(VAR f: PFONT; w,h: INTEGER);
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  f^.W:=w; f^.H:=h
END font_size;

PROCEDURE font_slop(VAR f: PFONT; x,y: INTEGER);
  VAR l: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  IF (x=0) & (y=0) THEN f^.sc:=1; f^.ss:=0; RETURN END;
  l:=sqrt(x*x+y*y);
  f^.sc:= muldiv(x,1000,l);
  f^.ss:= muldiv(y,1000,l)
END font_slop;

PROCEDURE dispose_font(VAR f: PFONT);
  VAR c: CHAR;
BEGIN
  IF (f=NIL) OR (f^.magic#pmagic) THEN bad_desc; RETURN END;
  WITH f^ DO
    magic:=pmagic+20;
    FOR c:=0c TO 377c DO DISPOSE(ptr[c]) END
  END;
  DISPOSE(f)
END dispose_font;

PROCEDURE load(VAR f: PFONT; name: ARRAY OF CHAR);
  VAR hi,i: INTEGER;
      file: bio.FILE;
      ch  : CHAR;

BEGIN
  done:= TRUE;
  NEW(f); IF NOT done THEN RETURN END;
  bio.open(file,name,'r');
  IF NOT bio.done THEN error:=bio.error; RETURN  END;
  WITH f^ DO
    FOR ch:=0c TO 377c DO NEW(ptr[ch]) END;
    bio.read(file,SYSTEM.ADR(w),4);
    IF NOT bio.done THEN error:=bio.error; RETURN END;
    bio.read(file,SYSTEM.ADR(h),4);
    IF NOT bio.done THEN error:=bio.error; RETURN END;
    W:=w; H:=h;
    sc:=1000; ss:=0;
    ic:=0; is:=1000;
    magic:= pmagic;
    FOR ch:=0c TO 377c DO
      bio.read(file,SYSTEM.ADR(hi),4);
      IF NOT bio.done THEN error:=bio.error;  RETURN END;
      IF hi>=0 THEN
        NEW(ptr[ch],hi+1);
        IF NOT done THEN  RETURN END;
        bio.read(file,SYSTEM.ADR(ptr[ch]),BYTES(ptr[ch]));
        IF NOT bio.done THEN error:=bio.error; RETURN END
      END
    END;
    FOR ch:=0c TO 377c DO
     bio.read(file,SYSTEM.ADR(propW[ch]),4);
     IF NOT bio.done THEN error:=bio.error;  RETURN END
    END
  END;
  done := bio.done;
  error:= bio.error
END load;

BEGIN
  pmagic:= 50464F54h;
  load(font,'bcMain.plf');
  IF NOT done THEN tty.perror(error,'file bcMain.plf'); HALT END;
  font_slop(font,100,0);
  font_ital(font,0,100);
  font_size(font,10,10)
END bcText.
