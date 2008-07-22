IMPLEMENTATION MODULE bcPM; (*$N+$U+$X+ Leo 18-Jan-91. (c) KRONOS  popup version  *)
                            (*          brd 28-Jun-91. (c) KRONOS  window version *)

IMPORT       SYSTEM;            IMPORT  bio: BIO;
IMPORT       ASCII;             IMPORT  cpd: CPD;
IMPORT  cod: defCodes;          IMPORT  tim: Time;
IMPORT  err: defErrors;         IMPORT  mem: Heap;
IMPORT  scr: Screen;            IMPORT  reg: regExpr;
IMPORT  fnt: Fonts;             IMPORT  key: Keyboard;
IMPORT  wnd: pmWnd;             IMPORT  str: Strings;
IMPORT  crs: pmCrs;             IMPORT  lex: Lexicon;

IMPORT  pwm: pmWM;
IMPORT  tty: Terminal;


TYPE WORD    = SYSTEM.WORD;
     ADDRESS = SYSTEM.WORD;
     TOOL    = wnd.TOOL;

     ROLLER  = POINTER TO Roller;
     DIREX   = POINTER TO Direx;
     TABLET  = POINTER TO Tablet;
     BAR     = POINTER TO Bar;

     STACK   = POINTER TO Node;
     Node    = RECORD
                 next : STACK;
                 data0: SYSTEM.WORD;
                 data1: SYSTEM.WORD;
               END;

     Roller  = RECORD
                 magic: INTEGER;
                 tit  : STRING;
                 hot  : STRING;
                 cap  : STRING;
                 wind : WINDOW;
                 fnt  : fnt.FONT;
                 sfnt : fnt.FONT;
                 txt  : TEXT;
                 tol  : TOOL;
                 main : BLOCK;
                 ttl  : BLOCK;
                 off  : BLOCK;
                 up   : BLOCK;
                 rlr  : BLOCK;
                 dw   : BLOCK;
                 top  : INTEGER;
                 ln   : INTEGER;
                 lns  : INTEGER;
                 xit  : BITSET;
                 out  : BOOLEAN;
               END;

     Menu    = RECORD
                 magic: INTEGER;
                 rol  : ROLLER;
               END;

     Direx   = RECORD
                 magic: INTEGER;
                 rol  : ROLLER;
                 itm  : BITSET;
                 rge  : reg.EXPR;
                 cd   : bio.FILE;
                 tim  : INTEGER;
                 ext  : STRING;
                 ent  : STRING;
                 dir  : TEXT;
               END;

      Tablet    = RECORD
                    magic: INTEGER;
                    tit  : STRING;
                    hot  : STRING;
                    cap  : STRING;
                    wind : WINDOW;
                    but  : DYNARR OF wnd.BLOCK;
                    dis  : DYNARR OF BOOLEAN;
                    off  : wnd.BLOCK;
                    ttl  : wnd.BLOCK;
                    sel  : INTEGER;
                    col  : INTEGER;
                    lns  : INTEGER;
                    xit  : BITSET
                  END;

     Bar  =    RECORD
                   magic : INTEGER;
                   tit   : STRING;
                   hot   : STRING;
                   cap   : STRING;
                   wind  : WINDOW;
                   but   : DYNARR OF BLOCK;
                   dis   : DYNARR OF BOOLEAN;
                   ttl   : BLOCK;
                   sel   : INTEGER;
                   xit   : BITSET
                END;

VAR
  ank: BITSET;
    F: STACK;       act: BITSET;
   TM: STACK;       nlr: INTEGER;

   rmagic: INTEGER;
   mmagic: INTEGER;
   dmagic: INTEGER;
   tmagic: INTEGER;
   bmagic: INTEGER;

   _mouse: BOOLEAN;

----------------------------------------------------------------

PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;
PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;
PROCEDURE undef;     BEGIN done:=FALSE; error:=err.undef    END undef;

---------------------------- Memory ----------------------------

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

------------------------- Miscelaneouse ------------------------

PROCEDURE move(i,j,k: INTEGER); CODE cod.move END move;

PROCEDURE zero(VAR a: ARRAY OF WORD);
BEGIN a[0]:=0; move(SYSTEM.ADR(a[1]),SYSTEM.ADR(a[0]),HIGH(a)) END zero;

PROCEDURE index(VAL s: ARRAY OF CHAR; ch: CHAR; VAR inx: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF ch=0c THEN RETURN FALSE END;
  FOR i:=0 TO HIGH(s) DO
    IF s[i]=ch THEN inx:=i; RETURN TRUE END
  END;
  RETURN FALSE
END index;

----------------------- Window procedures ----------------------

PROCEDURE vline(VAL W: WINDOW; t: TOOL; xb,yb,ye: INTEGER);
BEGIN wnd.line(W,t,xb,yb,xb,ye) END vline;

PROCEDURE hline(VAL W: WINDOW; t: TOOL; xb,yb,xe: INTEGER);
BEGIN wnd.line(W,t,xb,yb,xe,yb) END hline;

----------------------------------------------------------------

PROCEDURE push(VAR s: STACK; d0,d1: SYSTEM.WORD);
  VAR n: STACK;
BEGIN
  NEW(n);
  IF NOT done THEN RETURN END;
  n^.next:=s; n^.data0:=d0; n^.data1:=d1; s:=n; done:=TRUE
END push;

PROCEDURE pop(VAR s: STACK; VAR d0,d1: SYSTEM.WORD);
  VAR n: STACK;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  n:=s; d0:=n^.data0; d1:=n^.data1; s:=s^.next; DISPOSE(n); done:=TRUE
END pop;

---------------------------- timeout ---------------------------

PROCEDURE pushtimeout(t: INTEGER);
BEGIN
  push(TM,timeout,0);
  IF NOT done THEN RETURN END;
  IF t<=0 THEN timeout:=MAX(INTEGER) ELSE timeout:=t END;
  time:=timeout
END pushtimeout;

PROCEDURE poptimeout;
  VAR i: INTEGER;
BEGIN
  pop(TM,timeout,i); time:=timeout
END poptimeout;

VAR _start: INTEGER;

PROCEDURE startwait;
BEGIN
  _start:=tim.sys_time(tim.milisec); time:=timeout
END startwait;

PROCEDURE awaited(): BOOLEAN;
BEGIN
  time:=timeout-(tim.sys_time(tim.milisec)-_start);
  IF time<=0 THEN time:=0 END;
  RETURN time>0
END awaited;

----------------------------- fonts ----------------------------
                             -------

PROCEDURE make_marks;

  VAR i,j,k,n,e: INTEGER;
    c0,c1,c2,c3: DYNARR OF BITSET;  d: BOOLEAN;
  h,h1,h2,h3,h4: INTEGER;

  PROCEDURE put(ch: CHAR; VAL c: ARRAY OF BITSET);
  BEGIN
    move(sfont^.BASE+h*ORD(ch),SYSTEM.ADR(c),h)
  END put;

  PROCEDURE undo;
  BEGIN DISPOSE(c0); DISPOSE(c1); DISPOSE(c2); DISPOSE(c3) END undo;

BEGIN
  h:=font^.H;  sfont:=NIL;
  IF h>31 THEN h:=31 END;
  e:=ORD(NOT ODD(h)); (* even *)
  NEW(c0);        NEW(c1);        NEW(c2);        NEW(c3);
  RESIZE(c0,h);   d:=done;        RESIZE(c1,h);   d:=d&done;
  RESIZE(c2,h);   d:=d&done;      RESIZE(c3,h);   d:=d&done;
  IF NOT d THEN undo; RETURN END;
  fnt.new(sfont,h,h,0c,10c,{}); done:=fnt.done;
  IF NOT done THEN error:=fnt.error; undo; sfont:=NIL; RETURN END;
  h1:=h-1;        h2:=h DIV 2;    h4:=h DIV 4;    h3:=h DIV 3;
  zero(c0);       zero(c1);       zero(c2);       zero(c3);
  FOR i:=0 TO h3+1 DO
    FOR j:=-i TO i-e DO  k:=h3-1-e+i+h MOD 2;  n:=h2+j;
      INCL(c0[k],n);    INCL(c1[n],k);
      INCL(c2[h1-k],n); INCL(c3[n],h1-k)
    END
  END;
  put(utria,c0);  put(dtria,c2);  put(ltria,c1);  put(rtria,c3);
  undo
END make_marks;

PROCEDURE setplanes(sh,nr,br: BITSET);
BEGIN
  shadow:=sh;      normal:=nr;      bright:=br;
  act:=bright+normal+shadow;
  done:=TRUE
END setplanes;

PROCEDURE setcolors;
  VAR i: INTEGER;
BEGIN
  i:=scr.state^.RGB DIV 4;
  IF i<=0 THEN bad_parm; RETURN END;
  WITH scr.state^.pal[INTEGER(black )]        DO r:=i*0; g:=i*0; b:=i*0 END;
  WITH scr.state^.pal[INTEGER(shadow)]        DO r:=i*1; g:=i*1; b:=i*1 END;
  WITH scr.state^.pal[INTEGER(normal)]        DO r:=i*2; g:=i*2; b:=i*2 END;
  WITH scr.state^.pal[INTEGER(bright)]        DO r:=i*3; g:=i*3; b:=i*3 END;
  scr.set_palette(scr.state^.pal,0,HIGH(scr.state^.pal)+1);
  done:=scr.done;
  IF NOT done THEN error:=scr.error END
END setcolors;

----------------------------------------------------------------

PROCEDURE clipinto(VAR x,y: INTEGER; w: WINDOW; VAL b: BLOCK);
BEGIN
  x:= x-w^.x; y:=y-w^.y;
  IF x<b.x THEN x:=b.x ELSIF x>=b.x+b.w THEN x:=b.x+b.w-1 END;
  IF y<b.y THEN y:=b.y ELSIF y>=b.y+b.h THEN y:=b.y+b.h-1 END;
END clipinto;

PROCEDURE inblock(x,y: INTEGER; W: WINDOW; VAL b: BLOCK): BOOLEAN;
BEGIN
  x:= x- W^.x-W^.inner.zX; y:= y- W^.y-W^.inner.zY;
  RETURN (b.x<=x) & (x<=b.x+b.w-1) & (b.y<=y) & (y<=b.y+b.h-1)
END inblock;

PROCEDURE inblocks(X,Y: INTEGER; W: WINDOW; SEQ b: BLOCK): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(b) DO
    WITH b[i] DO
      IF inblock(X,Y,W,b[i]) THEN RETURN i END
    END
  END;
  RETURN -1
END inblocks;

----------------------- keyboard & mouse -----------------------

PROCEDURE _read(delay: INTEGER; VAR p,s: BITSET; m: BOOLEAN);
  VAR dx,dy: INTEGER;
BEGIN
  ch:=0c;
  IF (delay>0) & (key.ready()=0) THEN cpd.wait(delay) END;
  IF cpd.ready()>0 THEN
    p:=cpd.state^.keys;  cpd.read(dx,dy,s);
    IF p#s THEN RETURN END;
    IF ABS(dx)+ABS(dy)#0 THEN
      IF NOT m THEN mx:=mx+dx; my:=my+dy
      ELSE
        mx:=mx+dx; my:=my+dy; crs.move(mx,my); _mouse:=TRUE; crs.toggle(_mouse)
      END
    END
  ELSIF cpd.error#err.time_out THEN key.wait(20)
  END;
  IF key.ready()>0 THEN key.read(ch) END
END _read;

PROCEDURE read(W: WINDOW; SEQ b: BLOCK);
  VAR s,p: BITSET;
BEGIN
  IF _mouse THEN crs.move(mx,my); crs.toggle(NOT crs.on) END;
  s:={}; p:={};
  LOOP
    _read(20,p,s,TRUE);
    IF    (p*ank={}) & (s*ank#{})                            THEN EXIT
    ELSIF (p*{0}={}) & (s*{0}#{}) & (inblocks(mx,my,W,b)>=0) THEN EXIT
    END;
    IF (ch#0c) OR NOT awaited() THEN EXIT END
  END;
  IF _mouse THEN crs.move(mx,my); crs.toggle(NOT crs.on) END
END read;

PROCEDURE mwait(keys: BITSET; SEQ k: CHAR);
  VAR i: INTEGER;
    x,y: INTEGER;
    p,s: BITSET;
BEGIN
  startwait;
  done:=TRUE;  ch:=0c;  p:={};  s:={};
  LOOP
    _read(20,p,s,FALSE);
    IF ch#0c THEN
      IF ((ch=15c) OR (ch=33c)) & (HIGH(k)<0) THEN EXIT END;
      IF (HIGH(k)=0) & (k[0]=0c)              THEN EXIT END;
      IF index(k,ch,i)                        THEN EXIT END
    END;
    IF (p*keys={}) & (s*keys#{}) THEN EXIT END;
    IF NOT awaited()             THEN EXIT END
  END
END mwait;

PROCEDURE wait(SEQ k: CHAR);
BEGIN mwait({0..cpd.state^.nokeys-1},k) END wait;

---------------------------- blocks ----------------------------

CONST FRAME=4;

PROCEDURE _block(W: WINDOW; VAR b: BLOCK; fill,pressed: BOOLEAN);
  VAR t: TOOL;
BEGIN
  t:=W^.inner;
  WITH b DO
    t.color:=black;
    wnd.frame(W,t,x,y,x+w,y+h);
    DEC(w,2);  DEC(h,2);
    INC(x); INC(y);
    IF fill THEN
      t.color:=normal;
      WITH b DO wnd.rect(W,t,x+1,y+1,x+w-1,y+h-1) END
    END;
    t.mask:={0..3};
    IF pressed THEN t.color:=shadow ELSE t.color:=bright END;
    wnd.line(W,t,x,y+1,x,y+h);
    wnd.line(W,t,x,y+h,x+w,y+h);
    IF pressed THEN t.color:=bright ELSE t.color:=shadow END;
    wnd.line(W,t,x,y,x+w-1,y);
    wnd.line(W,t,x+w,y,x+w,y+h-1);
    INC(x,1); INC(y,1); DEC(w,2); DEC(h,2)
  END
END _block;

PROCEDURE block(w: WINDOW; b: BLOCK; f,p: BOOLEAN);
BEGIN _block(w,b,f,p) END block;

PROCEDURE button(w: WINDOW; b: BLOCK; p: BOOLEAN);
BEGIN _block(w,b,FALSE,p) END button;

PROCEDURE _switch(W: WINDOW; b: BLOCK; pressed: BOOLEAN);
  VAR t: TOOL;
      a: BLOCK;
    i,j: INTEGER;
BEGIN
  IF (b.w<=4) OR (b.h<=4) THEN RETURN END;
  i:=(b.w+1) DIV 3; j:=(b.h+1) DIV 3;
  WITH b DO x:=x+i-1; y:=y+j; w:=i+2; h:=j END;
  t:=W^.inner; t.mask:={0..3}; t.color:=shadow;
  _block(W,b,NOT pressed,pressed)
END _switch;

PROCEDURE switch(W: WINDOW; b: BLOCK; p: BOOLEAN);
BEGIN
  WITH b DO INC(x,2); INC(y,2); DEC(w,4); DEC(h,4) END;
  _switch(W,b,p)
END switch;

PROCEDURE panel(W: WINDOW; b: BLOCK; VAR m: BLOCK; f,p: BOOLEAN);
BEGIN
  IF (m.w<=4) OR (m.h<=4) THEN RETURN END;
  _block(W,b,f,p);
  _block(W,m,FALSE,FALSE);
  WITH m DO INC(x,2); INC(y, 2); DEC(w,4); DEC(h,4) END;
END panel;

PROCEDURE _blockoff(W: WINDOW; b: BLOCK);
  VAR t: TOOL;
BEGIN
  t:=W^.inner;
  t.mask:=act; t.clip:=b;
  WITH t.clip DO
    t.color:=normal;
    DEC(x); DEC(y); INC(w,2); INC(h,2);
    wnd.frame(W,t,0,0,w-1,h-1);
    wnd.frame(W,t,1,1,w-2,h-2);
    wnd.frame(W,t,2,2,w-3,h-3);
  END
END _blockoff;

PROCEDURE buttonoff(W: WINDOW; b: BLOCK);
BEGIN
  IF (b.w<4) OR (b.h<4) THEN bad_parm; RETURN END;
  _blockoff(W,b); done:=TRUE
END buttonoff;

PROCEDURE panel1(W: WINDOW; b: BLOCK; VAR m,off,tit: BLOCK;
           VAL fmt: ARRAY OF CHAR;   SEQ arg: SYSTEM.WORD);
  VAR t: TOOL;
BEGIN
  IF (b.w<=FRAME) OR (b.h<=FRAME+font^.H+4) THEN bad_parm; RETURN END;
  block(W,b,TRUE,FALSE);
  WITH off DO x:=0; h:=font^.H+4; y:=b.h-h; w:=h END;
  tit:=off;
  WITH tit DO x:=x+off.w; w:=b.w-off.w; END;
  WITH m DO x:=2; y:=2; w:=b.w-4; h:=b.h-tit.h-2 END;
   block(W,off,FALSE,FALSE);
  _block(W,tit,FALSE,FALSE);
  _switch(W,off,FALSE);
  IF (HIGH(fmt)<0) OR (fmt="") THEN RETURN END;
  t:= W^.inner;   t.mask:=act;
  t.clip:=m;   t.mode:=wnd.rep;   t.color:=bright;
  wnd.line(W,t,0,m.h+1,b.w,m.h+1);
  t.clip:=tit; t.mode:=wnd.xor;   t.color:=normal/black;
  wnd.print(W,t,tit.x,tit.y,font,fmt,arg)
END panel1;

PROCEDURE panel2(W: WINDOW; b: BLOCK;
                 VAR m,off,tit,u,r,d: BLOCK;
                 VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR t: TOOL;
BEGIN
  IF (b.w<=FRAME+font^.H+4) OR (b.h<=FRAME+font^.H+4) THEN bad_parm; RETURN END;
  block(W,b,TRUE,FALSE);
  WITH off DO x:=0; h:=font^.H+4; y:=b.h-h; w:=h END;
  u:=off;
  WITH u DO x:=b.w-w;  END;
  tit:=off;
  WITH tit DO x:=x+off.w; w:=b.w-off.w-u.w; END;
  WITH m DO x:=2; y:=2; w:=b.w-4-u.w; h:=b.h-tit.h-2 END;
  r:=u;
  WITH r DO y:=u.h;  h:= b.h- 2*u.h END;
  d:=u;
  WITH d DO y:=0; END;
   block(W,off,FALSE,FALSE);
  switch(W,off,FALSE);
  _block(W,tit,FALSE,FALSE);
  _block(W,u,FALSE,FALSE);
  _block(W,d,FALSE,FALSE);
  _block(W,r,FALSE,FALSE);
  IF (HIGH(fmt)<0) OR (fmt="") THEN RETURN END;
  t:= W^.inner;
  t.mode:=wnd.rep;
  t.color:=bright;  wnd.line(W,t,0,m.h+1,m.w+3,m.h+1);
  t.color:=shadow;  wnd.line(W,t,m.w+3,1,m.w+3,m.h+1);
  t.mode:=wnd.xor; t.clip:=tit; t.mask:=act; t.color:=normal/black;
  wnd.print(W,t,tit.x,tit.y,font,fmt,arg)
END panel2;

--------------------------- dialogbox --------------------------

PROCEDURE _message(VAR wind: WINDOW;
                 xc,yc,add: INTEGER;
                 VAR b,off: BLOCK;
                 VAL   fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR i: INTEGER;
      t: TOOL;
BEGIN
  wind:=NIL;
  b.w:=wnd.lenght(font,fmt,args)+add+font^.W+4+sfont^.W+FRAME;
  b.h:=font^.H+FRAME+1;
  b.x:=0;
  b.y:=0;
  wnd.new(wind); done:=wnd.done;
  IF NOT done THEN error:=wnd.error; RETURN END;
  wnd.resize(wind,b.w,b.h);   done:=wnd.done;
  IF NOT done THEN wnd.dispose(wind); error:=wnd.error; RETURN END;
  IF (xc -b.w DIV 2)< 0 THEN wnd.move(wind,0,0)
  ELSIF  (xc +b.w DIV 2)> wnd.scrW THEN wnd.move(wind,wnd.scrW -b.w,0)
  ELSE  wnd.move(wind,xc - b.w DIV 2,yc - b.h DIV 2) END;
  IF (yc -b.h DIV 2)< 0 THEN wnd.move(wind,wind^.x,0)
  ELSIF  (yc +b.h DIV 2)> wnd.scrH THEN wnd.move(wind,wind^.x,wnd.scrH-b.h)
  ELSE  wnd.move(wind,wind^.x,yc - b.h DIV 2) END;
  off:=b; off.w:=off.h;
  _block(wind,b,TRUE,FALSE);
  button(wind,off,FALSE);
  switch(wind,off,FALSE);
  t:=wind^.full;
  t.color:= bright; t.mode:= wnd.rep;
  wnd.line(wind,t,off.w+1,2,off.w+1,off.h);
  t.clip:=b;
  t.color:=normal/black; t.mode:=wnd.xor; t.mask:=act;
  i:=off.w+3+font^.W DIV 2;
  wnd.print(wind,t,i,3,font,fmt,args);
  b:=t.clip; INC(b.x,i); DEC(b.w,i); INC(b.y,1);
  wnd.ontop(wind);
  wnd.open(wind)
END _message;

PROCEDURE message(xc,yc: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
  VAR wind: WINDOW;
       b,o: BLOCK;
       s,r: BITSET;
       mus: BOOLEAN;
     dx,dy: INTEGER;
BEGIN
  _message(wind,xc,yc,0,b,o,fmt,a);
  IF NOT done THEN RETURN END;
  startwait;
  mx:=o.x+o.w-2; my:=o.y+1; mus:=_mouse; _mouse:=TRUE;
  REPEAT
    read(wind,o); r:=cpd.state^.keys;
    IF (ch=15c) OR (ch=33c) OR NOT awaited() THEN wnd.dispose(wind); RETURN END
  UNTIL r#{};
  _switch(wind,o,TRUE);  _mouse:=mus;
  REPEAT cpd.read(dx,dy,s) UNTIL s*r={};
  wnd.dispose(wind)
END message;

PROCEDURE perror(err,xc,yc: INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
  VAR i: INTEGER;
      s: STRING;
BEGIN
  NEW(s);
  REPEAT
    RESIZE(s,BYTES(s)+64);
    IF NOT done THEN RETURN END;
    lex.perror(s,err,f,a)
  UNTIL (BYTES(s)>=256) OR (str.len(s)<HIGH(s));
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO
    IF s[i]<40c THEN s[i]:=' ' END; INC(i)
  END;
  message(xc,yc,"%s",s);
  DISPOSE(s)
END perror;

PROCEDURE char(c: CHAR): BOOLEAN;
BEGIN
  IF (c=15c) OR (c=33c) OR (c=10c) OR (c=16c) OR (c=17c)THEN RETURN FALSE END;
  RETURN ((0c<c) & (c<177c)) OR ((c>=254c) & (c<377c))
END char;

PROCEDURE _diabox(xc,yc,w: INTEGER;
                  VAR   s: ARRAY OF CHAR;
                  VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

  VAR t: TOOL;     wind: WINDOW;
      c: TOOL;    off,b: BLOCK;
      i: INTEGER;       x,o: INTEGER;
     sh: INTEGER;       r,k: BITSET;
    mus: BOOLEAN;       xit: BOOLEAN;

  PROCEDURE marker(erase: BOOLEAN);
  BEGIN
    c.back:=normal;  c.mode:=wnd.rep;  c.color:={};
    IF x+sfont^.W>t.clip.w THEN sfont^.W:=t.clip.w-x END;
    wnd.writech(wind,c,x,3,sfont,empty);
    sfont^.W:=sh DIV 2;
    IF x+sfont^.W>t.clip.w THEN sfont^.W:=t.clip.w-x END;
    IF NOT erase THEN
      c.back:=normal/bright; c.color:={}; c.mode:=wnd.xor;
      wnd.writech(wind,c,x,3,sfont,empty)
    END;
    sfont^.W:=sh
  END marker;

  PROCEDURE roll(left: BOOLEAN);
    VAR r: TOOL;
      j,w: INTEGER;
  BEGIN
    IF left THEN
      j:=0; w:=0;
      WHILE x+font^.W-w>t.clip.w+t.clip.x DO
        w:=w+wnd.lenght(font,"%c",s[o+j]); INC(j)
      END;
      r:=t; r.color:=normal; r.mode:=wnd.rep;
      r.clip.x:=r.clip.x+x; r.clip.w:=r.clip.w-x;
      wnd.scroll(wind,r,w,0); o:=o+j; x:=x-w
    ELSE
      DEC(o);
      w:=wnd.lenght(font,"%c",s[o]);
      r:=t; r.color:=normal; r.mode:=wnd.rep;
      r.clip.x:=r.clip.x+x; r.clip.w:=r.clip.w-x;
      wnd.scroll(wind,r,-w,0); x:=x+w;
      wnd.write(wind,t,x,3,font,s,o,1)
    END
  END roll;

BEGIN
  IF HIGH(s)<=0  THEN bad_parm; RETURN END;
  IF w<font^.W*8 THEN w:=font^.W*8 END;
  _message(wind,xc,yc,w,b,off,fmt,arg);
  IF NOT done THEN RETURN END;
  t:= wind^.full; t.mask:=act;
  t.clip:=b;
  IF t.clip.w<font^.W*4 THEN bad_parm; RETURN END;
  sh:=sfont^.H;
  t.color:=normal/black;  t.mode:=wnd.xor; c:=t;
  x:=off.w+8 + wnd.lenght(font,fmt,arg);
  t.clip.x:=x;
  t.clip.w:= wind^.w-x-4;
  mx:= wind^.x;
  my:= wind^.y;
  i:=0; o:=0;
  IF s#"" THEN
    WHILE (i<HIGH(s)) & (s[i]#0c) DO
      x:=wnd.xwrite(wind,t,x,3,font,s,i,1); INC(i);
      IF x+font^.W>t.clip.w+t.clip.x THEN roll(TRUE) END
    END
  END;
  startwait; mus:=_mouse; _mouse:=FALSE; xit:=FALSE;
  LOOP
    marker(FALSE);
    read(wind,off,t.clip); r:=cpd.state^.keys;
    IF    (r*{0}#{}) & inblock(mx,my,wind,t.clip)  THEN            EXIT
    ELSIF  r*ank#{}                                THEN xit:=TRUE; EXIT
    ELSIF (r*{0}#{}) & inblock(mx,my,wind,off)     THEN xit:=TRUE; EXIT
    ELSIF (i>0) & ((ch=key.back) OR (ch=key.left)) THEN
      marker(TRUE);  DEC(i);  DEC(x,wnd.lenght(font,"%c",s[i]));
      WHILE (o>0) & (x+font^.W*2<t.clip.w+t.clip.x) DO roll(FALSE) END;
      _mouse:=FALSE
    ELSIF char(ch) & (i<HIGH(s)) THEN
      marker(TRUE);
      s[i]:=ch; x:=wnd.xwrite(wind,t,x,3,font,s,i,1); INC(i);
      IF x+font^.W>t.clip.w+t.clip.x THEN
--   tty.print('roll \n');
      roll(TRUE) END;
      _mouse:=FALSE
    ELSIF  ch=15c                   THEN            EXIT
    ELSIF (ch=33c) OR NOT awaited() THEN xit:=TRUE; EXIT
    END;
    s[i]:=0c
  END;
  _mouse:=mus;
  IF xit THEN s[0]:=0c; _switch(wind,off,TRUE) END;
  k:=r;
  WHILE k*r#{} DO cpd.read(mx,my,k) END;
  mx:=wind^.x+off.w DIV 2;
  my:=wind^.y+off.h DIV 2;
  wnd.dispose(wind);  done:=TRUE
END _diabox;

PROCEDURE diabox(xc,yc,w: INTEGER;
                 VAR   s: ARRAY OF CHAR;
                 VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  s:=""; _diabox(xc,yc,w,s,fmt,arg)
END diabox;

PROCEDURE confirm(xc,yc,w: INTEGER;
                  VAR   s: ARRAY OF CHAR;
                  VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  _diabox(xc,yc,w,s,fmt,arg)
END confirm;

---------------------------- rollers ---------------------------

PROCEDURE rnew(VAR rol: ROLLER; X,Y,W,H: INTEGER; exit: BITSET;
               VAL fmt: ARRAY OF CHAR;         SEQ arg: SYSTEM.WORD);
BEGIN
  NEW(rol);
  IF NOT done THEN RETURN END;
  WITH rol^ DO  tol.mask:=act; --++
    wnd.new(wind);  done:= wnd.done;
    IF NOT done THEN error:=wnd.error; rdispose(rol); RETURN END;
    wnd.resize(wind,W,H);   done:= wnd.done;
    IF NOT done THEN error:=wnd.error; rdispose(rol); RETURN END;
    wnd.move(wind,X,Y);
    tol:= wind^.full;
    top:=0;
    ln:=0;
    NEW(tit);
    IF NOT done THEN rdispose(rol); RETURN END;
    NEW(hot);
    IF NOT done THEN rdispose(rol); RETURN END;
    NEW(txt);
    IF NOT done THEN rdispose(rol); RETURN END;
    NEW(cap);
    IF NOT done THEN rdispose(rol); RETURN END;
    fnt:=font;  sfnt:=sfont;
    xit :=exit;
    out:=TRUE;
    WITH up DO w:=0; h:=0; x:=0; y:=0 END;
    ttl:=up; dw:=up; rlr:=up;
    main:= wind^.full.clip;
    magic:=rmagic;
    newstr(tit,fmt,arg);
    IF NOT done THEN rdispose(rol); RETURN END;
    lns:=(tol.clip.h-FRAME*2 -fnt^.H) DIV fnt^.H;
    IF (lns<2) OR ((tol.clip.w-FRAME-fnt^.W+4) DIV fnt^.W<2) THEN
      bad_parm; rdispose(rol); RETURN
    END;
    WITH tol DO color:=bright; back:=black; mode:=wnd.bic END
  END
END rnew;

PROCEDURE rdispose(VAR r: ROLLER);
  VAR e: INTEGER;  d: BOOLEAN;
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN RETURN END;
  d:=done; e:=error;
  wnd.dispose(r^.wind);  r^.magic:=0;  DISPOSE(r);
  done:=d; error:=e
END rdispose;

PROCEDURE rtoptoY(r: ROLLER; tp: INTEGER; VAR b: BLOCK);
  VAR i: INTEGER;
      N: INTEGER;
BEGIN
  ASSERT((r#NIL) & (r^.magic=rmagic));
  WITH r^ DO
    N:=HIGH(txt)+1; ASSERT(N>lns);
    b:=rlr;  i:=b.w;
    WITH b DO
      h:=lns*b.h DIV N;      INC(x,1);   DEC(w,1);
      IF h<i THEN h:=i DIV 2 END;
      i:=(rlr.h-h-4);
      y:=y+rlr.h-2-h - (tp*i) DIV (N-lns)
    END
  END
END rtoptoY;

PROCEDURE lifter(W: WINDOW; t: TOOL;  c0,c1: BITSET);
BEGIN
  WITH t.clip DO
    t.color:=normal/c0;
    wnd.line(W,t,x,y+1,x,y+h-1);
    wnd.line(W,t,x+1,y+h-1,x+w-2,y+h-1);
    t.color:=normal/c1;
    wnd.line(W,t,x,y,x+w-1,y);
    wnd.line(W,t,x+w-1,y+1,x+w-1,y+h-1)
  END
END lifter;

PROCEDURE rollerman(r: ROLLER; dt: INTEGER);
  VAR c: CHAR;
      N: INTEGER;
      t: TOOL;
   b,b1: BLOCK;
BEGIN
  ASSERT((r#NIL) & (r^.magic=rmagic));
  WITH r^ DO
    N:=HIGH(txt)+1;
    IF N<=lns THEN RETURN END;
    rtoptoY(r,top+dt,b1);
    t:=tol;   t.mode:=wnd.xor;
    IF dt#0   THEN
      rtoptoY(r,top,b);
      IF b1.y#b.y THEN t.clip:=b; lifter(wind,t,bright,shadow) END
    END;
    t.clip:=b1;  lifter(wind,t,bright,shadow);
    IF (dt=0) OR ((top=0) # (top+dt=0)) THEN
      t:=tol; t.clip:=up;
      IF top+dt>0 THEN
        t.mode:=wnd.bic; t.back:=black;    t.color:=bright;   c:=utria
      ELSE
        t.mode:=wnd.rep; t.back:=normal;   t.color:={};       c:=empty
      END;
      wnd.writech(wind,t,t.clip.x,t.clip.y,sfnt,c)
    END;
    IF (dt=0) OR ((top+lns#N) # (top+lns+dt#N)) THEN
      t:=tol; t.clip:=dw;
      IF top+lns+dt<N THEN
        t.mode:=wnd.bic; t.back:=black;  t.color:=bright;  c:=dtria
      ELSE
        t.mode:=wnd.rep; t.back:=normal;  t.color:=black;  c:=empty
      END;
      wnd.writech(wind,t,t.clip.x,t.clip.y,sfnt,c)
    END
  END
END rollerman;

PROCEDURE rY(r: ROLLER; line: INTEGER): INTEGER;
BEGIN RETURN r^.tol.clip.h-(line+1)*r^.fnt^.H+ r^.tol.clip.y END rY;

PROCEDURE rxor(r: ROLLER; ln: INTEGER);
  VAR t: TOOL;
BEGIN
  WITH r^ DO
    t:=tol;
    WITH t DO
      mode:=wnd.xor; mask:=act;
      WITH t.clip DO
        DEC(x,2); INC(w,4);
        h:=fnt^.H+1;
        y:= rY(r,ln-top)-1;
        color:=normal/bright;
        wnd.line(wind,t,x+1,y,x+w-1,y);
        wnd.line(wind,t,x+w-1,y+1,x+w-1,y+h-1);
        color:=normal/shadow;
        wnd.line(wind,t,x,y,x,y+h-1);
        wnd.line(wind,t,x+1,y+h-1,x+w-1,y+h-1)
      END
    END
  END
END rxor;

PROCEDURE rshow(r: ROLLER; refresh: BOOLEAN);
  VAR t: TOOL;
  i,j,Y: INTEGER;
BEGIN
  WITH r^ DO
    IF NOT refresh THEN
      WITH up DO w:=0; h:=0; x:=0; y:=0 END;  dw:=up; rlr:=up;
      IF HIGH(txt)+1<=lns THEN
        panel1(wind,wind^.full.clip,main,off,ttl,"%s",tit)
      ELSE
        panel2(wind,wind^.full.clip,main,off,ttl,up,rlr,dw,"%s",tit)
      END;
      IF NOT done THEN RETURN END
    END;
    tol.clip:=main;
    WITH tol.clip DO
      i:=h MOD fnt^.H;
      IF i=0 THEN i:=fnt^.H END;
      INC(y,i DIV 2); DEC(h,i); i:=fnt^.W;
      INC(x,i DIV 2); DEC(w,i)
    END;
    IF refresh THEN
      t:= wind^.full; t.mask:=act;  t.mode:=wnd.rep;  t.color:=normal;
      t.clip:=rlr;
      wnd.rect(wind,t,rlr.x,rlr.y,rlr.x+rlr.w-1,rlr.y+rlr.h-1);
      t.clip:=tol.clip
    END;
    j:=lns;
    IF top+j>HIGH(txt)+1 THEN j:=HIGH(txt)+1-top END;
    FOR i:=0 TO j-1 DO
      Y:=rY(r,i);
      IF refresh THEN
        wnd.rect(wind,t,t.clip.x,Y,t.clip.x+t.clip.w-1,fnt^.H-1+Y)
      END;
      wnd.print(wind,tol,tol.clip.x,Y,fnt,"%s",txt[top+i])
    END;
    IF refresh & (j<lns) THEN
      Y:=rY(r,j);    i:=rY(r,lns-1);
      wnd.rect(wind,t,t.clip.x,i,t.clip.x+t.clip.w-1,fnt^.H-1+Y)
    END;
    tol.mode:=wnd.bic; rollerman(r,0)
  END
END rshow;

PROCEDURE rsetstr(r: ROLLER;           line: INTEGER;
            VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR t: TOOL; Y: INTEGER;
BEGIN
  IF (r=NIL ) OR (r^.magic#rmagic)   THEN bad_desc; RETURN END;
  IF (line<0) OR (line>HIGH(r^.txt)) THEN bad_parm; RETURN END;
  done:=TRUE;
  IF HIGH(r^.txt[line])<0 THEN RETURN END;
  str.print(r^.txt[line],fmt,arg);
  IF (line<r^.top) OR (line>=r^.top+r^.lns) THEN RETURN END;
  Y:=rY(r,line-r^.top);
  t:=r^.tol; t.color:=normal; t.mode:=wnd.rep;
  WITH t.clip DO
    wnd.rect(r^.wind,t,x,Y,x+w-1,r^.fnt^.H-1+Y);
    wnd.print(r^.wind,t,x,Y,r^.fnt,"%s",r^.txt[line])
  END
END rsetstr;

PROCEDURE rsettext(r: ROLLER; text: TEXT; t,l: INTEGER);
  VAR H0,H1: INTEGER;
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  done:=TRUE;
  WITH r^ DO
    H0:=HIGH(txt);  H1:=HIGH(text);
    txt^:=text^;
    IF t>=0 THEN top:=t END;
    IF l>=0 THEN ln :=l END;
    IF ln>HIGH(txt)              THEN ln :=HIGH(txt)       END;
    IF (top+lns<=ln) OR (top>ln) THEN top:=ln-lns DIV 2    END;
    IF top+lns>HIGH(txt)         THEN top:=HIGH(txt)+1-lns END;
    IF top<0                     THEN top:=0               END;
    rshow(r,(H0<=lns)=(H1<=lns))
  END
END rsettext;

PROCEDURE rchoose(r: ROLLER; line: INTEGER);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  WITH r^ DO
    IF (line<0) OR (line>HIGH(txt)) THEN bad_parm; RETURN END;
    IF (line>=top) & (line<top+lns) THEN ln:=line; RETURN END;
    rsettext(r,txt,-1,line)
  END
END rchoose;

PROCEDURE rgettext(r: ROLLER; VAR text: TEXT);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  done:=TRUE;
  text^:=r^.txt^
END rgettext;

PROCEDURE ropen(r: ROLLER);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  IF NOT r^.wind^.closed  THEN done:=FALSE; error:=err.duplicate; RETURN END;
  wnd.ontop(r^.wind);
  rshow(r,FALSE);
  wnd.open(r^.wind)
END ropen;

PROCEDURE rclose(r: ROLLER);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  IF r^.wind^.closed THEN done:=FALSE; error:=err.duplicate; RETURN END;
  wnd.close(r^.wind)
END rclose;

PROCEDURE rselect(r: ROLLER);

  VAR N: INTEGER;
    pop: BOOLEAN;
    mus: BOOLEAN;
   lbut: BOOLEAN;
   rbut: BOOLEAN;
  p,s,k: BITSET;
  ox,oy: INTEGER;

 PROCEDURE in(b: BLOCK): BOOLEAN; BEGIN RETURN inblock(mx,my,r^.wind,b) END in;

  PROCEDURE _jump(line: INTEGER; xor: BOOLEAN);
    VAR t: TOOL;
  BEGIN
    WITH r^ DO
      IF (line<0) OR (line>=N) OR (line=ln) THEN RETURN END;
      IF xor THEN rxor(r,ln) END;
      t:=tol; t.mask:=act;
      IF    line<top      THEN ASSERT(line=top-1);
        t.color:=normal;  t.mode:=wnd.rep; wnd.scroll(wind,t,0,+fnt^.H);
        rollerman(r,-1);  DEC(top);
        wnd.print(wind,tol,tol.clip.x,rY(r,line-top),fnt,"%s",txt[line])
      ELSIF line>=top+lns THEN ASSERT(line=top+lns);
        t.color:=normal;  t.mode:=wnd.rep; wnd.scroll(wind,t,0,-fnt^.H);
        rollerman(r,+1);  INC(top);
        wnd.print(wind,tol,tol.clip.x,rY(r,line-top),fnt,"%s",txt[line])
      END;
      ln:=line;
      IF xor THEN rxor(r,ln) END
    END
  END _jump;

  PROCEDURE jump(line: INTEGER);
  BEGIN
    WITH r^ DO
      IF (line>=top-1) & (line<=top+lns) THEN _jump(line,TRUE); RETURN END;
      rxor(r,ln); rsettext(r,txt,-1,line); rxor(r,ln)
    END
  END jump;

  PROCEDURE _mselect(): BOOLEAN;

    PROCEDURE roll(n: INTEGER; VAL b: BLOCK);
      VAR i,d: INTEGER; but: BLOCK;
    BEGIN
      WITH r^ DO
        i:=ln-top;
        crs.toggle(FALSE);
        rxor(r,ln);  d:=200;
        but:=b; DEC(but.x,2); DEC(but.y,2); INC(but.w,4); INC(but.h,4);
        _block(wind,but,FALSE,TRUE);
        WHILE (top+n>=0) & (top+n<N) & lbut DO
          _jump(top+n,FALSE);
          _read(d,p,s,FALSE);
          d:=0;
          lbut:=(s*{0}#{}) & in(b)
        END;
        but:=b; DEC(but.x,2); DEC(but.y,2); INC(but.w,4); INC(but.h,4);
        _block(wind,but,FALSE,FALSE);
        ln:=top+i;
        IF ln>HIGH(txt) THEN ln:=HIGH(txt) END;
        rxor(r,ln);
        crs.toggle(TRUE)
      END
    END roll;

    PROCEDURE roller;
      VAR t: TOOL;
          b: BLOCK;
          c: BLOCK;
        x,y: INTEGER;
      ox,oy: INTEGER;
      i,k,l: INTEGER;
    BEGIN
      WITH r^ DO
        crs.toggle(FALSE);
        i:=top;  l:=ln-top;
        t:=tol;  t.mode:=wnd.xor; t.mask:=act; t.color:=t.mask;
        rtoptoY(r,i,b); t.clip:=b; lifter(wind,t,bright,shadow);
        c:=rlr;  c.h:=rlr.h-b.h-4;
        x:=mx;   y:=my;
        IF    my<b.y      THEN  my:=my-4;     b.y:=my
        ELSIF my>=b.y+b.h THEN  my:=my-4-b.h; b.y:=my
        ELSE                    my:=b.y-4
        END;
        clipinto(mx,my,wind,c);
        WITH b DO k:=(N-lns)*(c.h-1-(my-c.y)) DIV (c.h-1) END;
        IF k<0 THEN k:=0 ELSIF k>N-lns THEN k:=N-lns END;
        i:=k; rtoptoY(r,i,b); t.clip:=b;
        lifter(wind,t,shadow,bright);
        LOOP
          ox:=mx; oy:=my;
          REPEAT _read(20,p,s,FALSE) UNTIL (s*{0}={}) OR (my#oy);
          INC(x,mx-ox);
          INC(y,my-oy);
          IF s*{0}={} THEN EXIT END;
          clipinto(mx,my,wind,c);
          WITH b DO k:=(N-lns)*(c.h-1-(my-c.y)) DIV (c.h-1) END;
          IF k<0 THEN k:=0 ELSIF k>N-lns THEN k:=N-lns END;
          IF k#i THEN
            i:=k; rtoptoY(r,i,b);
            lifter(wind,t,shadow,bright);
            t.clip:=b;
            lifter(wind,t,shadow,bright)
          END
        END;
        lifter(wind,t,shadow,bright);
        lifter(wind,t,bright,shadow);
        IF i#top THEN rxor(r,ln); rsettext(r,txt,i,i+l); rxor(r,ln) END;
        mx:=x;  my:=y;  crs.move(x,y);  crs.toggle(TRUE)
      END
    END roller;

  BEGIN
    WITH r^ DO
      IF in(up)  & (top>0)     THEN roll( -1,up); RETURN FALSE END;
      IF in(dw)  & (top+lns<N) THEN roll(lns,dw); RETURN FALSE END;
      IF in(rlr) & (N>lns)     THEN roller;       RETURN FALSE END;
      IF in(off)               THEN out:=TRUE;    RETURN TRUE  END;
      IF in(tol.clip)          THEN               RETURN TRUE  END;
      IF xit={}                THEN               RETURN FALSE END;
      WITH wind^.full.clip DO
        out:=(xit*xlf#{}) & (mx<x) OR (xit*xrg#{}) & (mx>=x+w)
          OR (xit*xdw#{}) & (my<y) OR (xit*xup#{}) & (my>=y+h);
        RETURN out
      END
    END
  END _mselect;

  PROCEDURE _kselect(): BOOLEAN;

    PROCEDURE page(n: INTEGER);
      VAR i,j: INTEGER;
    BEGIN
      WITH r^ DO
        IF (n<0) & (ln=0) OR (n>0) & (ln=N-1) THEN RETURN END;
        IF N<=lns THEN RETURN END;
        i:=ln-top;   j:=lns-2;
        IF j<=0 THEN j:=1 END;
        IF (n<0) & (ln-j>=top-1)   THEN _jump(ln-j,TRUE); RETURN END;
        IF (n>0) & (ln+j<=top+lns) THEN _jump(ln+j,TRUE); RETURN END;
        rxor(r,ln);
        WHILE (top+n>=0) & (top+n<N) & (j>0) DO _jump(top+n,FALSE); DEC(j) END;
        IF n<0    THEN ln:=top+i-j ELSE ln:=top+i+j END;
        IF ln<top THEN ln:=top ELSIF ln>=top+lns THEN ln:=top+lns-1 END;
        IF ln>=N  THEN ln:=N-1 END;
        rxor(r,ln)
      END
    END page;

    VAR i: INTEGER;
  BEGIN
    crs.toggle(FALSE);
    _mouse:=FALSE;
    WITH r^ DO
      IF index(hot,    ch ,i) THEN jump(i); RETURN TRUE END;
      IF index(cap,CAP(ch),i) THEN jump(i); RETURN TRUE END;
      CASE ch OF
      |15c      :            RETURN TRUE
      |33c      : out:=TRUE; RETURN TRUE
      |key.up   : IF ln=0           THEN out:=xup*xit#{}; RETURN out END;
                  _jump(ln-1,TRUE)
      |key.dw   : IF ln=HIGH(txt)   THEN out:=xdw*xit#{}; RETURN out END;
                  _jump(ln+1,TRUE)
      |key.right: out:=xrg*xit#{};  RETURN out
      |key.left : out:=xlf*xit#{};  RETURN out
      |key.pgup : page(-1 )
      |key.pgdw : page(lns)
      |key.home : IF top=0       THEN _jump(0,TRUE)   ELSE jump(0)   END
      |key.end  : IF top+lns=N   THEN _jump(N-1,TRUE) ELSE jump(N-1) END
      ELSE
      END;
      IF in(tol.clip) THEN my:=tol.clip.y+rY(r,ln-top)+fnt^.H DIV 2 END;
      RETURN FALSE
    END
  END _kselect;

  VAR i: INTEGER;

BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  IF HIGH(r^.txt)<0               THEN undef;    RETURN END;
  done:=TRUE;
  WITH r^ DO
    N:=HIGH(txt)+1;
    mus:=_mouse; _mouse:=FALSE;
    pop:= wind^.closed;
    IF pop      THEN ropen(r) END;
    IF NOT done THEN RETURN   END;
    mx:=tol.clip.x+fnt^.W DIV 2 +wind^.x;
    my:=tol.clip.y+rY(r,ln-top)+fnt^.H DIV 2+wind^.y;
    rxor(r,ln);
    p:={}; s:={}; ch:=0c;  out:=FALSE;
    LOOP
      ox:=mx; oy:=my; p:=s; startwait;
      REPEAT
        _read(20,p,s,TRUE)
      UNTIL (ox#mx) OR (oy#my) OR (p#s) OR (p#s) OR (ch#0c) OR NOT awaited();
      IF  NOT  awaited() THEN out:=TRUE; EXIT END;
      lbut:=(p*{0}={}) & (s*{0}#{});
      rbut:=(p*ank={}) & (s*ank#{});
      IF    rbut   THEN out:=TRUE; EXIT
      ELSIF lbut & _mselect() THEN EXIT
      ELSIF ((ox#mx) OR (oy#my)) & in(tol.clip) THEN
        i:=top + (tol.clip.h-1-(my-tol.clip.y-wind^.y)) DIV fnt^.H;
        IF i#ln THEN crs.toggle(FALSE); _jump(i,TRUE); crs.toggle(_mouse) END
      END;
      IF (ch#0c) & _kselect() THEN EXIT END
    END;
    crs.toggle(FALSE);
    rxor(r,ln);
    IF out THEN  _switch(wind,off,TRUE)   END;
    k:=s;  _mouse:=FALSE;
    WHILE s*k#{} DO _read(20,p,s,FALSE) END;
    IF out THEN  _switch(wind,off,FALSE)  END;
    _mouse:=mus;
    IF pop THEN rclose(r) END
  END
END rselect;

PROCEDURE rselected(r: ROLLER): BOOLEAN;
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN RETURN FALSE END;
  RETURN NOT r^.out
END rselected;

PROCEDURE ralt(r: ROLLER): INTEGER;
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN RETURN -1 END;
  RETURN r^.ln
END ralt;

PROCEDURE rblocks(r: ROLLER; VAR main,txt,tit,off,up,rlr,dw: BLOCK);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic)     THEN bad_desc; RETURN END;
  IF (r^.main.w<=0) OR (r^.main.h<=0) THEN undef;    RETURN END;
  main:=r^.main;        up :=r^.up;
  tit :=r^.ttl;         dw :=r^.dw;
  off :=r^.off;         rlr:=r^.rlr;    txt:=r^.tol.clip;
  done:=TRUE
END rblocks;

PROCEDURE rwindow(r: ROLLER; VAR w: WINDOW);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  w:= r^.wind
END rwindow;

----------------------------- direx ----------------------------

PROCEDURE BIO_error(d: DIREX; VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR e: INTEGER;
BEGIN
  WITH d^.rol^.wind^.full.clip DO
    e:=bio.error; perror(e,x+w DIV 2,y+h DIV 4,fmt,arg);
    done:=FALSE;   error:=e
  END
END BIO_error;

PROCEDURE collect(d: DIREX);

  PROCEDURE match(VAL name: ARRAY OF CHAR; mode: BITSET): BOOLEAN;
  BEGIN
    IF name=".." THEN RETURN TRUE END;
    IF (mode*bio.e_dir#{})    & (d^.itm*dirs#{})    THEN RETURN TRUE  END;
    IF NOT reg.match(d^.rge,name,0)                 THEN RETURN FALSE END;
    IF (mode*bio.e_hidden#{}) & (d^.itm*hidden={})  THEN RETURN FALSE END;
    IF (mode*bio.e_esc   #{}) & (d^.itm*devices={}) THEN RETURN FALSE END;
    IF (mode*bio.e_dir={})    & (d^.itm*files={})   THEN RETURN FALSE END;
    RETURN TRUE
  END match;

  VAR co: INTEGER;
     wtm: INTEGER;
     i,h: INTEGER;
    mode: BITSET;
    name: ARRAY [0..31] OF CHAR;
    save: ARRAY [0..31] OF CHAR;

BEGIN
  WITH d^ DO
    bio.get_attr(cd,bio.a_wtime,wtm);
    IF NOT bio.done THEN BIO_error(d,"get_attr %%s"); RETURN END;
    IF wtm=tim      THEN done:=TRUE; RETURN END;
    save:="";
    IF (rol^.ln>0) & (rol^.ln<=HIGH(dir)) THEN save:=dir[rol^.ln] END;
    REPEAT
      bio.dir_walk(cd,bio.s_none);
      IF NOT bio.done THEN BIO_error(d,"dir_walk %%s");  RETURN END;
      co:=0+ORD(dirs*itm#{}); (* one for "." *)
      WHILE bio.get_entry(cd,name,mode) DO
        IF match(name,mode) THEN INC(co) END
      END;
      IF NOT bio.done THEN BIO_error(d,"get_entry %%s");  RETURN END;
      bio.end_walk(cd);
      IF NOT bio.done THEN BIO_error(d,"end_walk %%s"); RETURN END;

      h:=HIGH(dir);
      RESIZE(dir,co+1);
      IF NOT done THEN RETURN END;
      FOR i:=h+1 TO HIGH(dir) DO NEW(dir[i]) END;
      RESIZE(ent,co+1);
      IF NOT done THEN RETURN END;
      FOR i:=h+1 TO HIGH(ent) DO ent[i]:=0c  END;

      FOR i:=h+1 TO HIGH(dir) DO
        RESIZE(dir[i],32);
        IF NOT done THEN RETURN END
      END;

      bio.dir_walk(cd,bio.s_name+bio.s_dirfwd);
      IF NOT bio.done THEN BIO_error(d,"dir_walk %%s");  RETURN END;
      co:=0+ORD(dirs*itm#{}); (* one for "." *)
      IF co>0 THEN dir[0]:="."0c; ent[0]:=0c END;
      WHILE bio.get_entry(cd,name,mode) DO
        IF match(name,mode) THEN
          IF co<=HIGH(dir) THEN
            dir[co]:=name; ent[co]:=CHAR(mode*bio.e_dir#{})
          END; INC(co)
        END
      END;
      IF NOT bio.done THEN BIO_error(d,"get_entry %%s");  RETURN END;
      bio.end_walk(cd);
      IF NOT bio.done THEN BIO_error(d,"end_walk %%s"); RETURN END
    UNTIL co=HIGH(dir);
    dir[co]:="  ";  ent[co]:=0c;
    tim:=wtm;
    IF save="" THEN i:=-1
    ELSE i:=0;
      WHILE (i<=HIGH(dir)) & (save#dir[i]) DO INC(i) END;
      IF i>HIGH(dir) THEN i:=-1 END
    END;
    rsettext(rol,dir,-1,i)
  END;
  done:=TRUE
END collect;

PROCEDURE dnew(VAR d: DIREX;  x,y,w,h: INTEGER; exit: BITSET;
             cdname : ARRAY OF CHAR;
         VAL pattern: ARRAY OF CHAR;
               items: BITSET);
BEGIN
  NEW(d);
  IF NOT done THEN RETURN END;
  d^.rol  :=rnull;
  d^.magic:=dmagic;
  d^.rge  :=reg.null;
  d^.cd   :=bio.null;
  d^.itm  :=items;
  d^.tim  :=-1;
  NEW(d^.ext);
  NEW(d^.dir);
  NEW(d^.ent);
  reg.compile(pattern,d^.rge);  done:=reg.done;
  IF NOT done THEN error:=reg.error; ddispose(d); RETURN END;
  rnew(d^.rol,x,y,w,h,exit,"");
  IF NOT done THEN ddispose(d); RETURN END;
  RESIZE(d^.ext,256);
  IF NOT done THEN ddispose(d); RETURN END;
  bio.open(d^.cd,cdname,'rx');
  IF NOT bio.done THEN BIO_error(d,'"%s" %%s',cdname); ddispose(d); RETURN END;
  d^.ext:=""
END dnew;

PROCEDURE ddispose(VAR d: DIREX);
  VAR i,e: INTEGER;   dn: BOOLEAN;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN RETURN END;
  dn:=done; e:=error;
  FOR i:=0 TO HIGH(d^.dir) DO DISPOSE(d^.dir[i]) END;
  DISPOSE(d^.dir);  DISPOSE(d^.ent);  DISPOSE(d^.ext);
  reg.dispose(d^.rge);
  bio.close(d^.cd);
  rdispose(d^.rol);  d^.magic:=0;     DISPOSE(d);
  done:=dn; error:=e
END ddispose;

PROCEDURE dopen (d: DIREX);
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  collect(d);
  IF NOT done THEN RETURN END;
  ropen(d^.rol);
END dopen;

PROCEDURE dclose(d: DIREX);
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  rclose(d^.rol);
END dclose;

PROCEDURE dchoose(d: DIREX; VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  collect(d);
  IF NOT done THEN RETURN END;
  i:=0;
  WHILE (i<=HIGH(d^.dir)) & (d^.dir[i]#name) DO INC(i) END;
  IF i>HIGH(d^.dir) THEN done:=FALSE; error:=err.no_entry; RETURN END;
  rchoose(d^.rol,i)
END dchoose;

PROCEDURE dselect(d: DIREX);
  VAR f: bio.FILE;
      t: TOOL;
      i: INTEGER;
    cdn: ARRAY [0..31] OF CHAR;
    pop: BOOLEAN;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  done:=TRUE;
  WITH d^ DO
    pop:=rol^.wind^.closed;
    IF pop      THEN dopen(d) END;
    IF NOT done THEN RETURN   END;
    LOOP
      collect(d);
      IF NOT done THEN RETURN END;
      bio.fname(cd,ext);
      IF bio.done THEN bio.splitpathname(ext,cdn) END;
      IF NOT bio.done THEN cdn:="" END;
      ext:="";
      t:=rol^.tol;  t.clip:=rol^.ttl;
      t.mode:=wnd.rep; t.color:=normal;
      WITH t.clip DO wnd.rect(rol^.wind,t,x,y,x+w-1,y+h-1) END;
      t.mode:=wnd.bic; t.color:=bright;
      i:=t.clip.w-wnd.lenght(rol^.fnt,"%s",cdn);
      wnd.print(rol^.wind,t,i DIV 2+t.clip.x,t.clip.y,rol^.fnt,"%s",cdn);
      rselect(rol);
      IF NOT done              THEN EXIT END;
      IF NOT rselected(rol) THEN EXIT END;
      IF rol^.ln=HIGH(dir) THEN
        ext:="";
        WITH rol^.wind^ DO
          diabox(x+w DIV 2,y+font^.H+4+FRAME,w,ext,"")
        END;
        IF NOT done  THEN EXIT END;
        IF ext#""    THEN EXIT END
      ELSE
        IF ent[rol^.ln]=0c THEN RETURN END;
        dopenfile(d,f,"rx");
        IF done & (bio.kind(f)*bio.is_dir#{}) THEN
          bio.close(cd); cd:=f; tim:=-1; rol^.top:=0; rol^.ln:=0
        END
      END
    END;
    IF pop THEN dclose(d) END
  END
END dselect;

PROCEDURE dselected(d: DIREX): BOOLEAN;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN RETURN FALSE END;
  RETURN rselected(d^.rol)
END dselected;

PROCEDURE dfilename(d: DIREX; VAR fname: ARRAY OF CHAR);
  VAR a: INTEGER;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  IF NOT dselected(d)             THEN undef;    RETURN END;
  a:=d^.rol^.ln;
  IF a<HIGH(d^.dir) THEN str.copy(fname,d^.dir[a])
  ELSE                   str.copy(fname,d^.ext)
  END;
  done:=TRUE
END dfilename;

PROCEDURE dfullname(d: DIREX; VAR fname: ARRAY OF CHAR);
  VAR a: INTEGER;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  IF NOT dselected(d)             THEN undef;    RETURN END;
  a:=d^.rol^.ln;
  IF a<HIGH(d^.dir) THEN
    bio.fname(d^.cd,fname);
    IF NOT bio.done THEN BIO_error(d,"fname %%s"); RETURN END;
    IF fname#"/"0c THEN str.append(fname,"/%s",d^.dir[a])
    ELSE                str.append(fname,"%s" ,d^.dir[a])
    END
  ELSE
    IF d^.ext[0]='/' THEN str.copy(fname,d^.ext);     RETURN END;
    bio.fname(d^.cd,fname);
    IF NOT bio.done  THEN BIO_error(d,"fname %%s"); RETURN END;
    IF fname#"/"0c   THEN str.append(fname,"/%s",d^.ext)
    ELSE                  str.append(fname,"%s" ,d^.ext)
    END
  END;
  done:=TRUE
END dfullname;

PROCEDURE dopenfile(d: DIREX; VAR f: bio.FILE; mode: ARRAY OF CHAR);
  VAR a: INTEGER;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  IF NOT dselected(d)             THEN undef;    RETURN END;
  a:=d^.rol^.ln;
  IF a<HIGH(d^.dir) THEN bio.fopen((d^.cd),f,d^.dir[a],mode)
  ELSE                   bio.fopen((d^.cd),f,d^.ext,mode)
  END;
  IF bio.done THEN done:=TRUE; RETURN END;
  IF a<HIGH(d^.dir) THEN BIO_error(d,'open("%s","%s") %%s',d^.dir[a],mode)
  ELSE                   BIO_error(d,'open("%s","%s") %%s',d^.ext,mode)
  END
END dopenfile;

PROCEDURE dcd(d: DIREX; VAR cd: bio.FILE);
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  cd:=d^.cd; done:=TRUE;
END dcd;

PROCEDURE dblocks(d: DIREX; VAR main,txt,tit,off,up,rl,dw: BLOCK);
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  rblocks(d^.rol,main,txt,tit,off,up,rl,dw)
END dblocks;

PROCEDURE dwindow(d: DIREX; VAR w: WINDOW);
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  rwindow(d^.rol,w)
END dwindow;

-------------------------- tablet mous & key --------------------

PROCEDURE clear_mou;
 VAR i,x,y: INTEGER; b: BITSET;
BEGIN FOR i:=0 TO cpd.ready()-1 DO cpd.read(x,y,b) END;
END clear_mou;

VAR state: BITSET;

PROCEDURE x_read(VAR c: CHAR; VAR x,y: INTEGER;);
VAR s: BITSET;
    S: INTEGER;
BEGIN
  LOOP
    IF key.ready()>0 THEN key.read(c); x:=0; y:=0; RETURN  END;
    IF cpd.ready()>0 THEN
      cpd.read(x,y,s);
      IF s = state THEN c:=0c; RETURN  END;
      state:=s;
      S:=INTEGER(s);
      CASE S OF
         1: c:=015c; clear_mou; RETURN
        |2: c:=220c; clear_mou; RETURN
        |4: c:=033c; clear_mou; RETURN
      ELSE  c:=0c; RETURN END
    END
  END
END x_read;

PROCEDURE _area(W: WINDOW; xit: BITSET): BOOLEAN;
BEGIN
  WITH W^ DO
    IF (mx<x)   & (xit*xlf#{}) THEN RETURN TRUE END;
    IF (mx>x+w) & (xit*xrg#{}) THEN RETURN TRUE END;
    IF (my<y)   & (xit*xdw#{}) THEN RETURN TRUE END;
    IF (my>y+h) & (xit*xup#{}) THEN RETURN TRUE END;
    RETURN FALSE
  END
END _area;

PROCEDURE _exit;
  VAR x,y,i: INTEGER;
          s: BITSET;
BEGIN
  crs.toggle(FALSE);
  FOR i:=0 TO cpd.ready()-1 DO cpd.read(x,y,s) END
END _exit;

PROCEDURE inb(w: WINDOW; b: BLOCK): BOOLEAN;
BEGIN RETURN inblock(mx,my,w,b) END inb;

PROCEDURE blen(fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;
BEGIN RETURN wnd.lenght(font,fmt,arg) END blen;

PROCEDURE slen(fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;
BEGIN RETURN wnd.lenght(ufont,fmt,arg) END slen;

-------------------------- DIALOG LINE -------------------------

VAR DIA: WINDOW;

PROCEDURE dia_lin(VAR st: ARRAY OF CHAR; prompt: ARRAY OF CHAR): BOOLEAN;
  VAR mod,cy: INTEGER;
       cl,bc: BITSET;
          dn: BOOLEAN;
           T: TOOL;

PROCEDURE monitor;
 VAR p,l,x,x0,h: INTEGER;
         cursor: CHAR;
              s: STRING;

  PROCEDURE new_char;
  BEGIN
    s[p]:=ch;
    IF p<h THEN
      s[p+1]:=0c;
      wnd.print(DIA,T,x,cy,font,'%c%c',ch,cursor);
      x:= x0 + wnd.lenght(font,s); INC(p)
    END
  END new_char;

  PROCEDURE del_char;
  BEGIN
    IF p>0 THEN DEC(p);  s[p]:=0c;
      IF str.len(s)=0 THEN x:=x0 ELSE x:= x0 + wnd.lenght(font,s) END;
      wnd.print(DIA,T,x,cy,font,'%c%s',cursor,'  ')
    END
  END del_char;

BEGIN
  x0:= T.clip.x + 4 + wnd.lenght(font,prompt);
  h:=HIGH(st); cursor:=255c;
  NEW(s,h+1);  s:=st;
  p:=str.len(s);
  IF p=0 THEN x:=x0 ELSE x:=x0+wnd.lenght(font,s) END;
  wnd.print(DIA,T,x0,cy,font,'%s%c',s,cursor);
  LOOP
    IF cpd.ready()>2 THEN dn:=FALSE; EXIT END;
    IF key.ready()>0 THEN
      key.read(ch);
      CASE ch OF
         key.back: del_char
        |key.cr  : st:=s; dn:=TRUE; EXIT
        |33c     : dn:= FALSE; EXIT
      ELSE
        IF char(ch) THEN new_char END
      END
    END
  END;
  DISPOSE(s)
END monitor;

BEGIN
  T:= DIA^.inner;
  T.color:= black;
  T.back:=  normal;
  wnd.ontop(DIA);
  wnd.open(DIA);
  block(DIA,T.clip,TRUE,FALSE); cy:=2;
  wnd.write(DIA,T,T.clip.x+2,cy,font,prompt,0,str.len(prompt));
  monitor;
  wnd.close(DIA);
--  wnd.onbottom(DIA);
  clear_mou;
  RETURN dn
END dia_lin;

PROCEDURE dia_move(x,y: INTEGER); BEGIN wnd.move(DIA,x,y) END dia_move;

---------------------------- TABLET ----------------------------


PROCEDURE tdispose(VAR t: TABLET);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN RETURN END;
  WITH t^ DO
    DISPOSE(tit);
    DISPOSE(hot);
    DISPOSE(cap);
    wnd.dispose(wind);
    DISPOSE(but)
  END;
  DISPOSE(t)
END tdispose;

PROCEDURE tnew    (VAR t: TABLET; n,m,X,Y,W,H: INTEGER; exit: BITSET;
                  titfmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR i,j: INTEGER;
        T: TOOL;
       ch: CHAR;
BEGIN
  NEW(t);
  IF NOT done THEN  RETURN END;
  WITH t^ DO
    magic:= tmagic;
    newstr(tit,titfmt,arg);  IF NOT done THEN tdispose(t); RETURN END;
    NEW(hot,m*n);            IF NOT done THEN tdispose(t); RETURN END;
    NEW(cap,m*n);            IF NOT done THEN tdispose(t); RETURN END;
    FOR i:=0 TO HIGH(hot) DO hot[i]:=0c; cap[i]:=0c END;
    wnd.new(wind);
    IF NOT wnd.done THEN tdispose(t); error:= wnd.error; RETURN END;
    wnd.resize(wind,W*n,H*m+font^.H+4);
    IF NOT wnd.done THEN tdispose(t); error:= wnd.error; RETURN END;
    IF X+t^.wind^.w> wnd.scrW THEN X:=wnd.scrW-t^.wind^.w END;
    IF X < 0 THEN X:=0 END;
    IF Y+t^.wind^.h> wnd.scrH THEN Y:=wnd.scrH-t^.wind^.h END;
    IF Y < 0 THEN Y:=0 END;
    wnd.move(wind,X,Y);
    T:= wind^.inner;
    T.color:= normal;
    wnd.rect(wind,T,0,0,W*n,H*m+font^.H+4);
    NEW(but,n*m);
    IF NOT done THEN tdispose(t); RETURN END;
    FOR i:=1 TO m DO
      FOR j:=1 TO n DO
        WITH but[(i-1)*n+(j-1)] DO
          x:=W*(j-1); w:=W; y:=H*(m-i); h:=H;
          button(wind,but[(i-1)*n+(j-1)],FALSE)
        END
      END
    END;
    NEW(dis,n*m);
    IF NOT done THEN tdispose(t); RETURN END;
    FOR i:=0 TO HIGH(dis) DO dis[i]:= TRUE END;
    WITH off DO x:=0; w:=font^.H+4; h:=w; y:=H*m END;
    button(wind,off,FALSE);
    switch(wind,off,FALSE);
    WITH ttl DO
      x:=off.x+off.w; y:=off.y; h:=off.h; w:=W*n-off.w
    END;
    button(wind,ttl,FALSE);
    T:= wind^.inner;
    WITH T DO color:= normal; back := {}; mode := wnd.bic END;
    wnd.write(wind,T,ttl.x+3,ttl.y+3,font,tit,0,str.len(tit));
    sel:= -1;
    col:=n;
    lns:= m;
    xit:= exit
  END
END tnew;

PROCEDURE thotkey(t: TABLET; alt: INTEGER; hotkey: CHAR; capequ: BOOLEAN);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  WITH t^ DO
    IF (alt<0) OR (alt>=lns*col) THEN bad_parm; RETURN END;
    hot[alt]:=hotkey;  done:=TRUE;
    IF capequ THEN
      hot[alt]:=ASCII.SMALL  (hotkey);
      cap[alt]:=ASCII.CAPITAL(hotkey)
    END
  END
END thotkey;

PROCEDURE tmove(t: TABLET; x,y: INTEGER);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic)     THEN bad_desc; RETURN END;
  IF x+t^.wind^.w> wnd.scrW THEN x:=wnd.scrW-t^.wind^.w END;
  IF x<0 THEN x:=0 END;
  IF y+t^.wind^.h> wnd.scrH THEN y:=wnd.scrH-t^.wind^.h END;
  IF y<0 THEN y:=0 END;
  wnd.move(t^.wind,x,y)
END tmove;

PROCEDURE topen(t: TABLET);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  IF NOT t^.wind^.closed THEN done:=FALSE; error:=err.duplicate; RETURN END;
  mx:=t^.wind^.x+t^.wind^.w DIV 2;
  my:=t^.wind^.y+t^.wind^.h DIV 2;
  wnd.ontop(t^.wind);
  wnd.open(t^.wind)
END topen;

PROCEDURE tclose(t: TABLET);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN  END;
  IF t^.wind^.closed THEN bad_parm; RETURN  END;
  wnd.close(t^.wind)
END tclose;

PROCEDURE tselect(t: TABLET);
  VAR dx,dy,i: INTEGER;
           st: BITSET;

PROCEDURE exit;
BEGIN
  _exit;
  switch(t^.wind,t^.off,TRUE);
  tim.delay(100,tim.milisec);
  switch(t^.wind,t^.off,FALSE)
END exit;

PROCEDURE new_sel(k: INTEGER);
BEGIN
  crs.toggle(FALSE);
  IF t^.sel#-1 THEN button(t^.wind,t^.but[t^.sel],FALSE) END;
  t^.sel:=k; button(t^.wind,t^.but[t^.sel],TRUE);
  crs.toggle(TRUE)
END new_sel;

BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN  END;
  IF t^.wind^.closed THEN topen(t) END;
  crs.move(mx,my);
  crs.toggle(TRUE);
  LOOP
    IF (key.ready()#0) OR (cpd.ready()#0) THEN
      x_read(ch,dx,dy);
      CASE ch OF
         033c     : exit; RETURN
        |015c     : IF inb(t^.wind,t^.off) THEN exit; RETURN END;
                    IF _area(t^.wind,t^.xit) THEN exit; RETURN END;
                    FOR i:=0 TO HIGH(t^.but) DO
                      IF inb(t^.wind,t^.but[i]) & t^.dis[i] THEN
                        new_sel(i);
                        IF (xsel*t^.xit#{}) THEN exit; RETURN END
                      END
                    END
        |key.left : DEC(mx,1)
        |key.right: INC(mx,1)
        |key.f3   : DEC(mx,24)
        |key.f4   : INC(mx,24)
        |key.dw   : DEC(my,1)
        |key.up   : INC(my,1)
      ELSE
        IF  index(t^.hot,ch,i) THEN
          new_sel(i);
          IF (xsel*t^.xit#{}) THEN exit; RETURN END
        ELSIF  index(t^.cap,ch,i) THEN
          new_sel(i);
          IF (xsel*t^.xit#{}) THEN exit; RETURN END
        ELSE   INC(mx,dx); INC(my,dy)
        END
      END;
      crs.move(mx,my)
    END
  END
END tselect;

PROCEDURE tselected(t: TABLET): BOOLEAN;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN FALSE END;
  RETURN  (t^.sel >=0) & (t^.sel<=t^.lns*t^.col)
END tselected;

PROCEDURE tbutton(t: TABLET; x,y: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN -1 END;
  FOR i:=0 TO HIGH(t^.but) DO
    IF inblock(x,y,t^.wind,t^.but[i]) THEN RETURN i END
  END;
  RETURN -1
END tbutton;

PROCEDURE tunselect(t: TABLET);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  IF (t^.sel>=0) & (t^.sel<=HIGH(t^.but)) THEN
    button(t^.wind,t^.but[t^.sel],FALSE)
  END;
  t^.sel:= -1
END tunselect;

PROCEDURE tchoose(t: TABLET; alt: INTEGER);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  WITH t^ DO
    IF (0>alt) OR (alt >= lns*col) THEN bad_parm; RETURN END;
    IF (t^.sel>=0) & (t^.sel<=HIGH(t^.but)) THEN
      button(t^.wind,t^.but[t^.sel],FALSE)
    END;
    button(wind,but[alt],TRUE);
    t^.sel:=alt
  END
END tchoose;

PROCEDURE talt(t: TABLET): INTEGER;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN -1 END;
  RETURN t^.sel
END talt;

PROCEDURE tdisable(t: TABLET; alt: INTEGER);
 VAR b: BLOCK;
     T: TOOL;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN  END;
  IF (0> alt) OR (alt >= t^.lns*t^.col) THEN bad_parm; RETURN END;
  IF NOT t^.dis[alt] THEN RETURN END;
  tblocks(t,alt,b);
  T:= t^.wind^.full;
  T.color:= normal;
  T.mode := wnd.xor;
  T.mask := {0..3};
  wnd.frame(t^.wind,T,b.x+1,b.y+1,b.x+b.w-1,b.y+b.h-1);
--  wnd.line(t^.wind,T,b.x+1,b.y+1,b.x+b.w-1,b.y+b.h-1);
--  wnd.line(t^.wind,T,b.x+1,b.y+b.h-1,b.x+b.w-1,b.y+1);
  t^.dis[alt]:=FALSE
END tdisable;

PROCEDURE tundisable (t: TABLET; alt: INTEGER);
  VAR b: BLOCK;
      T: TOOL;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN  END;
  IF (0> alt) OR (alt >= t^.lns*t^.col) THEN bad_parm; RETURN END;
  IF t^.dis[alt] THEN RETURN END;
  tblocks(t,alt,b);
  T:= t^.wind^.full;
  T.color:= normal;
  T.mode := wnd.xor;
  T.mask := {0..3};
  wnd.frame(t^.wind,T,b.x+1,b.y+1,b.x+b.w-1,b.y+b.h-1);
  t^.dis[alt]:= TRUE
END tundisable;

PROCEDURE tontop(t: TABLET);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  wnd.ontop(t^.wind)
END tontop;

PROCEDURE tonbottom(t: TABLET);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  wnd.onbottom(t^.wind)
END tonbottom;

PROCEDURE twindow(t: TABLET; VAR w: WINDOW);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic) THEN bad_desc; RETURN END;
  w:= t^.wind
END twindow;

PROCEDURE tblocks(t: TABLET; alt: INTEGER; VAR b: BLOCK);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic)     THEN bad_desc; RETURN END;
  IF (0> alt) OR (alt >= t^.lns*t^.col) THEN bad_parm; RETURN END;
  WITH t^.but[alt] DO b.x:= x+2; b.y:= y+2; b.w:= w-4; b.h:= h-4 END
END tblocks;

PROCEDURE ttitle(t: TABLET; VAR b: BLOCK);
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic)     THEN bad_desc; RETURN END;
  WITH t^.ttl DO b.x:= x+2; b.y:= y+2; b.w:= w-4; b.h:= h-4 END
END ttitle;

PROCEDURE tprint(t: TABLET; alt: INTEGER; s: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR S: STRING;
      T: TOOL;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic)     THEN bad_desc; RETURN END;
  IF (0> alt) OR (alt >= t^.lns*t^.col) THEN bad_parm; RETURN END;
  newstr(S,s,arg);
  IF NOT done THEN  RETURN END;
  WITH t^ DO
    T:= wind^.inner;
    T.color:= black;
    T.back := normal;
    T.mode := wnd.rep;
    tblocks(t,alt,T.clip);
    wnd.write(wind,T,but[alt].x+5,but[alt].y+4,font,S,0,str.len(S))
  END
END tprint;

PROCEDURE tsprint(t: TABLET; alt: INTEGER; s: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR S: STRING;
      T: TOOL;
BEGIN
  IF (t=NIL) OR (t^.magic#tmagic)     THEN bad_desc; RETURN END;
  IF (0> alt) OR (alt >= t^.lns*t^.col) THEN bad_parm; RETURN END;
  newstr(S,s,arg);
  IF NOT done THEN  RETURN END;
  WITH t^ DO
    T:= wind^.inner;
    T.color:= black;
    T.back := normal;
    T.mode := wnd.rep;
    tblocks(t,alt,T.clip);
    wnd.write(wind,T,but[alt].x+3,but[alt].y+3,ufont,S,0,str.len(S))
  END
END tsprint;

-------------------------- ACTION BAR --------------------------

PROCEDURE bdispose(VAR b: BAR);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  WITH b^ DO
    DISPOSE(tit);
    DISPOSE(hot);
    DISPOSE(cap);
    DISPOSE(but);
    wnd.dispose(wind)
  END;
  DISPOSE(b)
END bdispose;

PROCEDURE bnew (VAR b: BAR; txt: ARRAY OF STRING; X,Y: INTEGER; exit: BITSET;
                  titfmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
VAR  i,l: INTEGER;
       T: TOOL;
BEGIN
  NEW(b);
  IF NOT done THEN  RETURN END;
  WITH b^ DO
    magic:= bmagic;
    newstr(tit,titfmt,arg);
    IF NOT done THEN bdispose(b); RETURN END;
    NEW(hot,HIGH(txt)+1);
    IF NOT done THEN bdispose(b); RETURN END;
    NEW(cap,HIGH(txt)+1);
    IF NOT done THEN bdispose(b); RETURN END;
    FOR i:=0 TO HIGH(hot) DO hot[i]:=0c; cap[i]:=0c END;
    NEW(but,HIGH(txt)+1);
    IF NOT done THEN bdispose(b); RETURN END;
    but[0].x:=0;
    but[0].y:=0;
    but[0].w:= wnd.lenght(font,txt[0])+6;
    but[0].h:= font^.H+4; IF but[0].h<14 THEN but[0].h:=14 END;
    l:= but[0].w;
    FOR i:=1 TO HIGH(txt) DO
      WITH but[i] DO
        x:= but[i-1].x+but[i-1].w;  y:=but[0].y;
        w:= wnd.lenght(font,txt[i])+6;
        h:= but[0].h;
        l:= l+w
      END
    END;
    NEW(dis,HIGH(txt)+1);
    IF NOT done THEN bdispose(b); RETURN END;
    FOR i:=0 TO HIGH(dis) DO dis[i]:= TRUE END;
    WITH but[HIGH(but)] DO
      ttl.x:= x+w;
      ttl.y:= y;
      ttl.h:= h;
      IF str.len(tit)<1 THEN ttl.w:=0
      ELSE ttl.w:= wnd.lenght(font,tit)+6 END;
      l:= l+ ttl.w +14; (*14 for left upper WM button *)
    END;
--    wnd.new(wind);
    pwm.new(wind);
    IF NOT wnd.done THEN error:=wnd.error; bdispose(b); RETURN  END;
    pwm.disable(wind);
    wnd.resize(wind,l,ttl.h+1);
    IF NOT wnd.done THEN error:=wnd.error; bdispose(b); RETURN  END;
    wnd.inner(wind,14,0,wind^.w-14,wind^.h);
    wnd.move(wind,X,Y);
    T:= wind^.inner;
    T.color:= black;
    T.back := normal;
    T.mode := wnd.rep;
    T.clip := ttl;
    block(wind,ttl,TRUE,FALSE);
    wnd.write(wind,T,ttl.x+3,ttl.y+3,font,tit,0,str.len(tit));
    FOR i:=0 TO HIGH(but) DO
      block(wind,but[i],TRUE,FALSE);
      T.clip:= but[i];
      wnd.write(wind,T,but[i].x+3,but[i].y+3,font,txt[i],0,str.len(txt[i]))
    END;
    sel:= -1;
    xit:= exit
  END
END bnew;

PROCEDURE bhotkey(b: BAR; alt: INTEGER; hotkey: CHAR; capequ: BOOLEAN);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  WITH b^ DO
    IF (alt<0) OR (alt> HIGH(but)) THEN bad_parm; RETURN END;
    hot[alt]:=hotkey;  done:=TRUE;
    IF capequ THEN
      hot[alt]:=ASCII.SMALL  (hotkey);
      cap[alt]:=ASCII.CAPITAL(hotkey)
    END
  END
END bhotkey;

PROCEDURE bmove(b: BAR; x,y: INTEGER);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  wnd.move(b^.wind,x,y)
END bmove;

PROCEDURE bopen (b: BAR);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  wnd.ontop(b^.wind);
  wnd.open(b^.wind)
END bopen;

PROCEDURE bclose(b: BAR);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  wnd.close(b^.wind)
END bclose;

PROCEDURE bselect  (b: BAR);
  VAR old: WINDOW;
        i: INTEGER;

PROCEDURE new_sel(k: INTEGER);
BEGIN
  crs.toggle(FALSE);
  WITH b^ DO
    IF sel#-1 THEN button(wind,but[sel],FALSE) END;
    sel:=k; button(wind,but[sel],TRUE)
  END;
  crs.toggle(TRUE)
END new_sel;

PROCEDURE _mselect(): BOOLEAN;
  VAR i: INTEGER;
BEGIN
 IF _area(b^.wind,b^.xit) & (pwm.active=old) THEN RETURN TRUE END;
 FOR i:=0 TO HIGH(b^.but) DO
   IF inb(b^.wind,b^.but[i]) & b^.dis[i] THEN
     new_sel(i);
     IF (xsel*b^.xit#{}) THEN RETURN TRUE END;
     RETURN FALSE
    END
  END;
  RETURN FALSE
END _mselect;

BEGIN
  old:=pwm.active;
  IF (b=NIL) OR (b^.magic#bmagic) THEN bad_desc; RETURN  END;
  crs.toggle(TRUE);
  crs.toggle(TRUE);
  LOOP
    IF (key.ready()#0) THEN
      key.read(ch);
      CASE ch OF
         033c     : _exit; RETURN
        |015c     : IF _mselect() THEN _exit; RETURN END;
        |key.left : DEC(mx,1)
        |key.right: INC(mx,1)
        |key.f3   : DEC(mx,24)
        |key.f4   : INC(mx,24)
        |key.dw   : DEC(my,1)
        |key.up   : INC(my,1)
      ELSE
        IF index(b^.hot,ch,i) THEN
          new_sel(i);
          IF (xsel*b^.xit#{}) THEN crs.toggle(FALSE); RETURN END
        ELSIF  index(b^.cap,ch,i) THEN
          new_sel(i);
          IF (xsel*b^.xit#{}) THEN
            crs.toggle(FALSE); pwm.toggle(b^.wind,i,FALSE); RETURN
          END
        END
      END;
      crs.move(mx,my)
    ELSE
      pwm.monitor; wnd.ontop(b^.wind);
      mx:=crs.x; my:=crs.y;
      IF (pwm.active=b^.wind) THEN
        IF  pwm.moved THEN
          wnd.move(b^.wind,pwm.moveX,pwm.moveY)
        ELSE
          IF    cpd.state^.keys={2} THEN _exit; RETURN
          ELSIF cpd.state^.keys={0} THEN
            IF _mselect() THEN  _exit; RETURN END
          END
        END
      ELSE  RETURN
      END
    END
  END
END bselect;

PROCEDURE bselected(b: BAR): BOOLEAN;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic) THEN bad_desc; RETURN FALSE END;
  RETURN  (b^.sel >=0) & (b^.sel <= HIGH(b^.but))
END bselected;

PROCEDURE bbutton(b: BAR; x,y: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic) THEN bad_desc; RETURN -1 END;
  FOR i:=0 TO HIGH(b^.but) DO
    IF inblock(x,y,b^.wind,b^.but[i]) THEN RETURN i END
  END;
  RETURN -1
END bbutton;

PROCEDURE bunselect(b: BAR);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  IF (b^.sel>=0) & (b^.sel<=HIGH(b^.but)) THEN
    button(b^.wind,b^.but[b^.sel],FALSE);
    b^.sel:= -1
  END
END bunselect;

PROCEDURE bchoose(b: BAR; alt: INTEGER);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  WITH b^ DO
    IF (alt<0) OR (alt> HIGH(but)) THEN bad_parm; RETURN END;
    IF (b^.sel>=0) & (b^.sel<=HIGH(b^.but)) THEN
      button(b^.wind,b^.but[b^.sel],FALSE)
    END;
    button(wind,but[alt],TRUE);
    b^.sel:=alt
  END
END bchoose;

PROCEDURE bdisable(b: BAR; alt: INTEGER);
  VAR  B: BLOCK;
       T: TOOL;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic) THEN bad_desc; RETURN END;
  IF (alt<0) OR (alt> HIGH(b^.but)) THEN bad_parm; RETURN END;
  IF NOT b^.dis[alt] THEN RETURN END;
  bblocks(b,alt,B);
  T:= b^.wind^.full;
  T.color:= normal;
  T.mode := wnd.xor;
  T.mask := {0..3};
  wnd.frame(b^.wind,T,B.x+1,B.y+1,B.x+B.w-1,B.y+B.h-1);
  b^.dis[alt]:= FALSE
END bdisable;

PROCEDURE bundisable(b: BAR; alt: INTEGER);
  VAR  B: BLOCK;
       T: TOOL;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic) THEN bad_desc; RETURN END;
  IF (alt<0) OR (alt> HIGH(b^.but)) THEN bad_parm; RETURN END;
  IF b^.dis[alt] THEN RETURN END;
  bblocks(b,alt,B);
  T:= b^.wind^.full;
  T.color:= normal;
  T.mode := wnd.xor;
  T.mask := {0..3};
  wnd.frame(b^.wind,T,B.x+1,B.y+1,B.x+B.w-1,B.y+B.h-1);
  b^.dis[alt]:= TRUE
END bundisable;

PROCEDURE balt(b: BAR): INTEGER;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN -1 END;
  RETURN b^.sel
END balt;

PROCEDURE bontop(b: BAR);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  wnd.ontop(b^.wind)
END bontop;

PROCEDURE bonbottom(b: BAR);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  wnd.onbottom(b^.wind)
END bonbottom;

PROCEDURE bwindow(b: BAR; VAR w: WINDOW);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  w:= b^.wind
END bwindow;

PROCEDURE bblocks(b: BAR; alt: INTEGER; VAR block: BLOCK);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  IF (alt<0) OR (alt> HIGH(b^.but)) THEN bad_parm; RETURN END;
  WITH b^.but[alt] DO
    block.x:=x+2; block.y:=y+2; block.w:=w-4; block.h:=h-4
  END
END bblocks;

PROCEDURE btitle (b: BAR; VAR t: BLOCK);
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  WITH b^.ttl DO t.x:=x+1; t.y:= y+1; t.w:=w+1; t.h:=h+1 END
END btitle;

PROCEDURE bprint(b: BAR; alt: INTEGER; s: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR S: STRING;
      t: TOOL;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)     THEN bad_desc; RETURN END;
  IF (alt<0) OR (alt> HIGH(b^.but)) THEN bad_parm; RETURN END;
  newstr(S,s,arg);
  IF NOT done THEN  RETURN END;
  WITH b^ DO
    t:= wind^.inner;
    t.color:= black;
    t.back := normal;
    t.mode := wnd.rep;
    bblocks(b,alt,t.clip);
    wnd.write(wind,t,but[alt].x+2,but[alt].y+4,font,S,0,str.len(S))
  END
END bprint;

PROCEDURE bsprint(b: BAR; alt: INTEGER; s: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR S: STRING;
      t: TOOL;
BEGIN
  IF (b=NIL) OR (b^.magic#bmagic)   THEN bad_desc; RETURN END;
  IF (alt<0) OR (alt> HIGH(b^.but)) THEN bad_parm; RETURN END;
  newstr(S,s,arg);
  IF NOT done THEN  RETURN END;
  WITH b^ DO
    t:= wind^.inner;
    t.color:= black;
    t.back := normal;
    t.mode := wnd.rep;
    bblocks(b,alt,t.clip);
    wnd.write(wind,t,but[alt].x+2,but[alt].y+2,ufont,S,0,str.len(S))
  END
END bsprint;

----------------------------- QUERY ----------------------------


PROCEDURE query(X,Y: INTEGER; prompt,yes,no: ARRAY OF CHAR;
                       hy,hn:CHAR; cp,df: BOOLEAN): BOOLEAN;
VAR l,lp,ly,ln,h,i: INTEGER;
             by,bn: BLOCK;
                tq: TABLET;
                 W: WINDOW;
                 T: TOOL;
BEGIN
  lp:= wnd.lenght(font,prompt)+16;
  ly:= wnd.lenght(font,yes);
  ln:= wnd.lenght(font,no );
  IF ly<ln THEN l:=ln END;
  IF lp>l  THEN l:=lp END;
  h:= (font^.H+6);
  tnew(tq,1,2,0,0,l+6,h,xsel,prompt);
  thotkey(tq,0,hn,cp);
  thotkey(tq,1,hy,cp);
  twindow(tq,W);
  T:= W^.inner;
  T.color:=normal;
  T.back:={};
  T.mode:=wnd.bic;
  tblocks(tq,0,bn); T.clip:=bn;
  wnd.write(W,T,(bn.w-ln) DIV 2,bn.y+1,font,no ,0,str.len(no ));
  tblocks(tq,1,by); T.clip:=by;
  wnd.write(W,T,(by.w-ly) DIV 2,by.y+1,font,yes,0,str.len(yes));
  tmove(tq,X- W^.w DIV 2,Y- W^.h DIV 2);
  topen(tq);
  IF df THEN mx:= by.w DIV 2 + W^.x; my:= by.h DIV 2 + W^.y
  ELSE       mx:= bn.w DIV 2 + W^.x; my:= bn.h DIV 2 + W^.y END;
  tselect(tq);
  IF tselected(tq) THEN df:=BOOLEAN(talt(tq)) END;
  tdispose(tq); RETURN df
END query;

----------------------------- Init -----------------------------

VAR i,i0,i1: INTEGER;
BEGIN
  done:=TRUE;  error:=err.ok;
  fnt.read(font,'bcPM.fnt');
  IF NOT fnt.done THEN font:=fnt.font END;
  fnt.unpack(font); ASSERT(fnt.done);
  wnd.new(DIA);
  ASSERT(wnd.done);
  wnd.ontop(DIA);
  wnd.resize(DIA,wnd.scrW-1,font^.H+4);
  ASSERT(wnd.done);
  make_marks; ASSERT(done);
  rnull:=NIL;           rmagic:=4C4C4F52h;
  dnull:=NIL;           dmagic:=00585244h;
  bnull:=NIL;           bmagic:=424152h;
  tnull:=NIL;           tmagic:=5441424Ch;
  time :=MAX(INTEGER); timeout:=MAX(INTEGER);
  ch:=0c;
  mx:=0;
  my:=0;
  state:={};
  F:=NIL;                   TM:=NIL;
  i1:=-1;  nlr:=0;
  black :={};
  IF scr.state^.type#49h THEN
    shadow:={i0};  normal:={i1};     bright:={i0,i1};
  ELSE
    shadow:={3};   normal:={0,1,2};  bright:={0..3};
  END;
  act:=bright+normal+shadow;
  cpd.nop;
  IF cpd.state^.nokeys>1 THEN ank:={cpd.state^.nokeys-1} END;
  setcolors;
  _mouse:=FALSE;
END bcPM.
