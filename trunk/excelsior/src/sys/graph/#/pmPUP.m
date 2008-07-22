IMPLEMENTATION MODULE pmPUP; (*$N+$U+$X+ Leo 18-Jan-91. (c) KRONOS *)

IMPORT       SYSTEM;            IMPORT  bio: BIO;
IMPORT       ASCII;             IMPORT  cpd: CPD;
IMPORT  cod: defCodes;          IMPORT  tim: Time;
IMPORT  err: defErrors;         IMPORT  mem: Heap;
IMPORT  scr: Screen;            IMPORT  reg: regExpr;
IMPORT  fnt: Fonts;             IMPORT  key: Keyboard;
IMPORT  bmg: BMG;               IMPORT  str: Strings;
IMPORT  crs: pmCrs;             IMPORT  lex: Lexicon;

IMPORT std: StdIO;

TYPE WORD    = SYSTEM.WORD;
     ADDRESS = SYSTEM.WORD;
     POPUP   = POINTER TO PopUp;
     MENU    = POINTER TO Menu;
     ROLLER  = POINTER TO Roller;
     DIREX   = POINTER TO Direx;
     DEBUG   = POINTER TO Debug;

     STACK   = POINTER TO Node;
     Node    = RECORD
                 next : STACK;
                 data0: SYSTEM.WORD;
                 data1: SYSTEM.WORD;
               END;

     PopUp   = RECORD
                 magic: INTEGER;
                 block: bmg.BLOCK;
                 ofs  : INTEGER; (* x offset in words   *)
                 words: INTEGER; (* line width in words *)
                 close: BOOLEAN;
                 save : DYNARR OF SYSTEM.WORD;
               END;

     Debug   = RECORD
                 magic: INTEGER;
                 pup  : POPUP;
                 tol  : bmg.TOOL;
                 y    : INTEGER;
                 fnt  : fnt.FONT;
               END;

     Roller  = RECORD
                 magic: INTEGER;
                 tit  : STRING;
                 hot  : STRING;
                 cap  : STRING;
                 pup  : POPUP;
                 fnt  : fnt.FONT;
                 sfnt : fnt.FONT;
                 txt  : TEXT;
                 tol  : bmg.TOOL;
                 main : bmg.BLOCK;
                 ttl  : bmg.BLOCK;
                 off  : bmg.BLOCK;
                 up   : bmg.BLOCK;
                 rlr  : bmg.BLOCK;
                 dw   : bmg.BLOCK;
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

VAR
   B: bmg.BITMAP;  wpl: INTEGER;
   T: bmg.TOOL;    ank: BITSET;
   F: STACK;       act: BITSET;
  TM: STACK;       nlr: INTEGER;

   pmagic: INTEGER;
   mmagic: INTEGER;
   rmagic: INTEGER;
   dmagic: INTEGER;
   gmagic: INTEGER;

   _mouse: BOOLEAN;

----------------------------------------------------------------

PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;
PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;
PROCEDURE undef;     BEGIN done:=FALSE; error:=err.undef    END undef;

---------------------------- Memory ----------------------------
                            --------

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
                         ---------------

PROCEDURE move(i,j,k: INTEGER); CODE cod.move END move;

PROCEDURE zero(VAR a: ARRAY OF WORD);
BEGIN a[0]:=0; move(SYSTEM.ADR(a[1]),SYSTEM.ADR(a[0]),HIGH(a)) END zero;

PROCEDURE dvl(m: INTEGER; b: bmg.BITMAP; x,y,len: INTEGER);
CODE cod.bmg cod.bmg_dvl END dvl;

PROCEDURE bbltg(mode,des,des_ofs,sou,sou_ofs,nobits: INTEGER);
CODE cod.bmg cod.bmg_bblt END bbltg;

PROCEDURE index(VAL s: ARRAY OF CHAR; ch: CHAR; VAR inx: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF ch=0c THEN RETURN FALSE END;
  FOR i:=0 TO HIGH(s) DO
    IF s[i]=ch THEN inx:=i; RETURN TRUE END
  END;
  RETURN FALSE
END index;

---------------------- Display procedures ----------------------
                      --------------------

PROCEDURE vline(VAL t: bmg.TOOL; xb,yb,ye: INTEGER);
BEGIN
  bmg.line(B,t,t.clip.x+xb,t.clip.y+yb,t.clip.x+xb,t.clip.y+ye)
END vline;

PROCEDURE hline(VAL t: bmg.TOOL; xb,yb,xe: INTEGER);
BEGIN
  bmg.line(B,t,t.clip.x+xb,t.clip.y+yb,t.clip.x+xe,t.clip.y+yb)
END hline;

PROCEDURE line(VAL t: bmg.TOOL; xb,yb,xe,ye: INTEGER);
BEGIN
  bmg.line(B,t,t.clip.x+xb,t.clip.y+yb,t.clip.x+xe,t.clip.y+ye)
END line;

PROCEDURE rect(VAL t: bmg.TOOL; xb,yb,xe,ye: INTEGER);
BEGIN
  bmg.rect(B,t,t.clip.x+xb,t.clip.y+yb,t.clip.x+xe,t.clip.y+ye)
END rect;

PROCEDURE frame(VAL t: bmg.TOOL; xb,yb,xe,ye: INTEGER);
BEGIN
  bmg.frame(B,t,t.clip.x+xb,t.clip.y+yb,t.clip.x+xe,t.clip.y+ye)
END frame;

PROCEDURE writech(VAL t: bmg.TOOL; x,y: INTEGER; f: fnt.FONT; ch: CHAR);
BEGIN
  bmg.writech(B,t,x+t.clip.x,y+t.clip.y,f,ch)
END writech;

PROCEDURE write(VAL t: bmg.TOOL;   x,y: INTEGER;
                    f: fnt.FONT; VAL s: ARRAY OF CHAR; p,l: INTEGER);
BEGIN
  bmg.write(B,t,x+t.clip.x,y+t.clip.y,f,s,p,l)
END write;

PROCEDURE xwrite(VAL t: bmg.TOOL;   x,y: INTEGER;
                     f: fnt.FONT; VAL s: ARRAY OF CHAR; p,l: INTEGER): INTEGER;
BEGIN
  RETURN bmg.xwrite(B,t,x+t.clip.x,y+t.clip.y,f,s,p,l)-t.clip.x
END xwrite;

PROCEDURE xprint(VAL t: bmg.TOOL;   x,y: INTEGER;
                     f: fnt.FONT; VAL s: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
BEGIN
  RETURN bmg.xprint(B,t,x+t.clip.x,y+t.clip.y,f,s,a)-t.clip.x
END xprint;

PROCEDURE print(VAL t: bmg.TOOL;   x,y: INTEGER;
                    f: fnt.FONT; VAL s: ARRAY OF CHAR; SEQ a: WORD);
BEGIN
  bmg.print(B,t,x+t.clip.x,y+t.clip.y,f,s,a)
END print;

PROCEDURE dot(VAL t: bmg.TOOL; x,y: INTEGER);
BEGIN
  bmg.dot(B,t,x+t.clip.x,y+t.clip.y)
END dot;

PROCEDURE circle(VAL t: bmg.TOOL; x,y,r: INTEGER);
BEGIN
  bmg.circle(B,t,x+t.clip.x,y+t.clip.y,r)
END circle;

PROCEDURE scroll(VAL t: bmg.TOOL; xs,ys: INTEGER);
BEGIN
  bmg.scroll(B,t,xs,ys)
END scroll;


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
                            ---------

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

(*
PROCEDURE pushfont(f: fnt.FONT);
BEGIN
  push(F,font,sfont);
  IF done THEN
    font:=f; make_marks;
    IF NOT done (* of make marks *) THEN sfont:=NIL; popfont END
  END
END pushfont;

PROCEDURE popfont;
BEGIN
  IF (F#NIL) & (sfont#NIL) THEN fnt.dispose(sfont) END;
  pop(F,font,sfont)
END popfont;
*)

PROCEDURE setplanes(sh,nr,br: BITSET);
BEGIN
  IF NOT (sh+nr+br<=T.mask) THEN bad_parm; RETURN END;
  IF T.mask/(sh+nr+br)={}   THEN bad_parm; RETURN END;
  shadow:=sh;      normal:=nr;      bright:=br;
  act:=bright+normal+shadow;
  T.color:=normal; T.back:=black;   done:=TRUE
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

PROCEDURE clipinto(VAR x,y: INTEGER; VAL b: bmg.BLOCK);
BEGIN
  IF x<b.x THEN x:=b.x ELSIF x>=b.x+b.w THEN x:=b.x+b.w-1 END;
  IF y<b.y THEN y:=b.y ELSIF y>=b.y+b.h THEN y:=b.y+b.h-1 END
END clipinto;

PROCEDURE shiftin(VAR b: bmg.BLOCK);
BEGIN
  IF b.x<0 THEN b.x:=0 ELSIF b.x+b.w>T.clip.w THEN b.x:=T.clip.w-b.w END;
  IF b.y<0 THEN b.y:=0 ELSIF b.y+b.h>T.clip.w THEN b.y:=T.clip.h-b.h END;
  bmg.cross(b,b,T.clip)
END shiftin;



PROCEDURE inblock(x,y: INTEGER; VAL b: bmg.BLOCK): BOOLEAN;
BEGIN
  RETURN (b.x<=x) & (x<=b.x+b.w-1) & (b.y<=y) & (y<=b.y+b.h-1)
END inblock;

PROCEDURE inblocks(X,Y: INTEGER; SEQ b: bmg.BLOCK): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(b) DO
    WITH b[i] DO
      IF (x<=X) & (X<=x+w-1) & (y<=Y) & (Y<=y+h-1) THEN RETURN i END
    END
  END;
  RETURN -1
END inblocks;

----------------------- keyboard & mouse -----------------------
                       ------------------

PROCEDURE _read(delay: INTEGER; VAR p,s: BITSET; m: BOOLEAN);
  VAR dx,dy: INTEGER;
BEGIN
  ch:=0c;
  IF (delay>0) & (key.ready()=0) THEN cpd.wait(delay) END;
  IF cpd.ready()>0 THEN
    p:=cpd.state^.keys;  cpd.read(dx,dy,s);
--std.print("s=%{}\n", s);
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

PROCEDURE read(SEQ b: bmg.BLOCK);
  VAR s,p: BITSET;
BEGIN
  IF _mouse THEN crs.move(mx,my); crs.toggle(NOT crs.on) END;
  s:={}; p:={};
  LOOP
    _read(20,p,s,TRUE);
    IF    (p*ank={}) & (s*ank#{})                          THEN EXIT
    ELSIF (p*{0}={}) & (s*{0}#{}) & (inblocks(mx,my,b)>=0) THEN EXIT
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

---------------------------- popups ----------------------------
                            --------

PROCEDURE pnew(VAR pup: POPUP; X,Y,W,H: INTEGER);
BEGIN
  NEW(pup);
  IF NOT done THEN RETURN END;
  NEW(pup^.save,0);
  WITH pup^.block DO x:=X; y:=Y; w:=W; h:=H END;
  bmg.cross(pup^.block,T.clip,pup^.block);
  WITH pup^.block DO
    IF (w<=0) OR (h<=0) THEN bad_parm; DISPOSE(pup); RETURN END;
    pup^.ofs:=x DIV 32;
    pup^.words:=(w+x MOD 32+31) DIV 32;
    NEW(pup^.save,h*pup^.words*nlr);
    IF NOT done THEN DISPOSE(pup); RETURN END
  END;
  pup^.magic:=pmagic;
  pup^.close:=TRUE;
END pnew;

PROCEDURE pdispose(VAR pup: POPUP);
  VAR d: BOOLEAN; e: INTEGER;
BEGIN
  IF (pup=NIL) OR (pup^.magic#pmagic) THEN RETURN END;
  d:=done; e:=error;
  IF NOT pup^.close THEN pclose(pup) END;
  DISPOSE(pup^.save);  pup^.magic:=0;  DISPOSE(pup);
  done:=d; error:=e
END pdispose;

PROCEDURE popen(pup: POPUP);
  VAR l,o,ptr,end,sav: INTEGER; m: BITSET;
BEGIN
  IF (pup=NIL) OR (pup^.magic#pmagic) THEN bad_desc; RETURN END;
  IF NOT pup^.close THEN done:=FALSE; error:=err.duplicate; RETURN END;
  sav:=SYSTEM.ADR(pup^.save);
  o:=(B^.H-1-pup^.block.y)*wpl+pup^.ofs;
  m:=T.mask;
  l:=0;
  WHILE m#{} DO
    IF B^.layers[l]#NIL THEN
      ptr:=B^.layers[l]+o; end:=ptr-pup^.block.h*wpl;
      REPEAT
        move(sav,ptr,pup^.words);  DEC(ptr,wpl);  INC(sav,pup^.words)
      UNTIL ptr=end;
    END;
    INC(l); m:=(m-{0})>>1
  END;
  pup^.close:=FALSE; done:=TRUE
END popen;

PROCEDURE pclose(pup: POPUP);
  VAR l,o,ptr,end,sav: INTEGER; m: BITSET;
BEGIN
  IF (pup=NIL) OR (pup^.magic#pmagic) THEN bad_desc; RETURN END;
  IF  pup^.close THEN done:=FALSE; error:=err.duplicate; RETURN END;
  sav:=SYSTEM.ADR(pup^.save);
  o:=(B^.H-1-pup^.block.y)*wpl+pup^.ofs;
  m:=T.mask;
  l:=0;
  WHILE m#{} DO
    IF B^.layers[l]#NIL THEN
      ptr:=B^.layers[l]+o; end:=ptr-pup^.block.h*wpl;
      REPEAT
        move(ptr,sav,pup^.words);  DEC(ptr,wpl);  INC(sav,pup^.words)
      UNTIL ptr=end;
    END;
    INC(l); m:=(m-{0})>>1
  END;
  pup^.close:=TRUE; done:=TRUE
END pclose;

PROCEDURE pclosed(pup: POPUP): BOOLEAN;
BEGIN
  IF (pup=NIL) OR (pup^.magic#pmagic) THEN RETURN TRUE END;
  RETURN pup^.close
END pclosed;

PROCEDURE pblock(pup: POPUP; VAR b: bmg.BLOCK);
BEGIN
  IF (pup=NIL) OR (pup^.magic#pmagic) THEN bad_desc; RETURN END;
  b:=pup^.block; done:=TRUE
END pblock;


---------------------------- blocks ----------------------------
                            --------

CONST FRAME=4;

PROCEDURE _block(VAR b: bmg.BLOCK; fill,pressed: BOOLEAN);
  VAR t: bmg.TOOL;
BEGIN
  t:=T;
  t.clip:=b;
  WITH t.clip DO
    t.color:=black;
    frame(t,0,0,w-1,h-1);
    INC(x); INC(y); DEC(w,2); DEC(h,2);
    IF fill THEN
      t.color:=normal;
      WITH t.clip DO rect(t,0,0,w-1,h-1) END
    END;
    t.mask:=act; --++
    IF pressed THEN t.color:=shadow ELSE t.color:=bright END;
    line(t,0,1,0,h-1);    line(t,0,h-1,w-1,h-1);
    IF pressed THEN t.color:=bright ELSE t.color:=shadow END;
    line(t,0,0,w-1,0);    line(t,w-1,0,w-1,h-2);
    INC(x); INC(y);   DEC(w,2); DEC(h,2);
    b:=t.clip
  END
END _block;

PROCEDURE _blockoff(b: bmg.BLOCK);
  VAR t: bmg.TOOL;
BEGIN
  t:=T;
  t.mask:=act; t.clip:=b;
  WITH t.clip DO
    t.color:=normal;
    DEC(x); DEC(y); INC(w,2); INC(h,2);
    frame(t,0,0,w-1,h-1);
    frame(t,1,1,w-2,h-2);
    frame(t,2,2,w-3,h-3);
  END
END _blockoff;

PROCEDURE block(b: bmg.BLOCK; fill,pressed: BOOLEAN);
BEGIN
  _block(b,fill,pressed)
END block;

PROCEDURE _switch(b: bmg.BLOCK; pressed: BOOLEAN);
  VAR t: bmg.TOOL;
    i,j: INTEGER;
BEGIN
  bmg.cross(b,b,T.clip);
  IF (b.w<=4) OR (b.h<=4) THEN bad_parm; RETURN END;
  i:=(b.w+1) DIV 3; j:=(b.h+1) DIV 3;
  WITH b DO x:=x+i-1; y:=y+j; w:=i+2; h:=j END;
  t:=T;  t.clip:=b;  t.mask:=act; --++
  WITH t.clip DO
    DEC(x); DEC(y); INC(w,2); INC(h,2); rect(t,0,0,w-1,h-1)
  END;
  _block(b,NOT pressed,pressed); done:=TRUE
END _switch;

PROCEDURE switch(b: bmg.BLOCK; pressed: BOOLEAN);
BEGIN
  WITH b DO INC(x,2); INC(y,2); DEC(w,4); DEC(h,4) END;
  _switch(b,pressed)
END switch;

PROCEDURE button(b: bmg.BLOCK; pressed: BOOLEAN);
BEGIN
  bmg.cross(b,b,T.clip);
  IF (b.w<4) OR (b.h<4) THEN bad_parm; RETURN END;
  _block(b,FALSE,pressed); done:=TRUE
END button;

PROCEDURE buttonoff(b: bmg.BLOCK);
BEGIN
  bmg.cross(b,b,T.clip);
  IF (b.w<4) OR (b.h<4) THEN bad_parm; RETURN END;
  _blockoff(b); done:=TRUE
END buttonoff;

PROCEDURE panel(b: bmg.BLOCK; VAR m: bmg.BLOCK;
         fill,pressed: BOOLEAN);
BEGIN
  bmg.cross(m,b,T.clip);
  IF (m.w<=FRAME) OR (m.h<=FRAME) THEN bad_parm; RETURN END;
  _block(m,fill ,pressed);
  WITH m DO INC(x,2); INC(y, 2); DEC(w,4); DEC(h,4) END;
  _block(m,FALSE,FALSE); done:=TRUE
END panel;

PROCEDURE panel0(b: bmg.BLOCK; VAR m,off: bmg.BLOCK; switch: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  bmg.cross(m,b,T.clip);
  IF (m.w<=FRAME) OR (m.h<=FRAME) THEN bad_parm; RETURN END;
  _block(m,TRUE,FALSE);
  off:=m; i:=font^.H+4;
  WITH off DO x:=x-2; y:=y+h+2-i; w:=i; h:=i END;
  IF switch THEN
    _block(off,FALSE,FALSE); _switch(off,FALSE)
  END
END panel0;

PROCEDURE panel1(b: bmg.BLOCK; VAR m,off,tit: bmg.BLOCK;
           VAL fmt: ARRAY OF CHAR;   SEQ arg: SYSTEM.WORD);
  VAR t: bmg.TOOL;
BEGIN
  bmg.cross(m,b,T.clip);
  IF (m.w<=FRAME) OR (m.h<=FRAME+font^.H+4) THEN bad_parm; RETURN END;
  panel0(b,m,off,TRUE);
  tit:=off;
  WITH tit DO x:=x+off.w+1; w:=m.w-off.w+1; DEC(y,2); INC(h,4) END;
  _block(tit,FALSE,FALSE);
  DEC(m.h,off.h+3);
  IF (HIGH(fmt)<0) OR (fmt="") THEN RETURN END;
  t:=T;        t.mask:=act;
  t.clip:=m;   t.mode:=bmg.rep;   t.color:=bright;
  WITH t.clip DO
    DEC(x); INC(w,2);  DEC(y); INC(h,2);  line(t,0,h-1,w-1,h-1)
  END;
  t.clip:=tit; t.mode:=bmg.xor;   t.color:=normal/black;
  print(t,0,0,font,fmt,arg);
END panel1;

PROCEDURE panel2(b: bmg.BLOCK;
                 VAR m,off,tit,u,r,d: bmg.BLOCK;
                 VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR t: bmg.TOOL;
BEGIN
  bmg.cross(m,b,T.clip);
  IF (m.w<=FRAME+font^.H+4) OR (m.h<=FRAME+font^.H+4) THEN bad_parm; RETURN END;
  panel1(b,m,off,tit,"");
  u:=off;
  WITH u DO x:=x+tit.w+1; DEC(y,2); INC(h,4); INC(w,4) END;
  _block(u,FALSE,FALSE);
  r:=u;
  WITH r DO y:=m.y+u.h+1; DEC(x,2); INC(w,4); h:=m.h-u.h+1 END;
  _block(r,FALSE,FALSE);
  d:=u;
  WITH d DO y:=m.y-2; DEC(x,2); INC(h,4); INC(w,4) END;
  _block(d,FALSE,FALSE);
  DEC(m.w,u.w+3);  DEC(tit.w,u.w+3);

  IF (HIGH(fmt)<0) OR (fmt="") THEN RETURN END;
  t:=T; t.mode:=bmg.xor; t.clip:=tit; t.mask:=act; t.color:=normal/black;
  print(t,0,0,font,fmt,arg)
END panel2;

---------------------------- debugs ----------------------------
                            --------

PROCEDURE gnew(VAR g: DEBUG; X,Y,W,H: INTEGER);
  VAR i: INTEGER;
BEGIN
  NEW(g);
  IF NOT done THEN RETURN END;
  g^.tol:=T;
  WITH g^ DO tol.mask:=act; --++
    WITH tol.clip DO
      x:=X; y:=Y; w:=W; h:=H;
      shiftin(tol.clip);
      fnt:=font;
      pup:=NIL;
      magic:=gmagic;
      pnew(pup,x,y,w,h);
      IF NOT done THEN gdispose(g); RETURN END;
      tol.clip:=pup^.block;
      i:=h MOD fnt^.H;
      IF i=0 THEN i:=fnt^.H END;
      INC(x,FRAME DIV 2 + 2);    DEC(w,FRAME+4);
      INC(y,FRAME DIV 2);        DEC(h,FRAME);
      INC(y,i DIV 2);            DEC(h,i);
      INC(x,fnt^.W DIV 4);       DEC(w,fnt^.W DIV 2);
      g^.y:=h;
      IF (g^.y<fnt^.H) OR (w DIV fnt^.W<2) THEN
        bad_parm; gdispose(g); RETURN
      END;
      WITH tol DO color:=bright; back:=black; mode:=bmg.bic END
    END
  END
END gnew;

PROCEDURE gdispose(VAR g: DEBUG);
  VAR e: INTEGER;  d: BOOLEAN;
BEGIN
  IF (g=NIL) OR (g^.magic#gmagic) THEN RETURN END;
  d:=done; e:=error;
  pdispose(g^.pup);  g^.magic:=0;  DISPOSE(g);
  done:=d; error:=e
END gdispose;

PROCEDURE gopen (g: DEBUG);
  VAR b: bmg.BLOCK;
BEGIN
  IF (g=NIL) OR (g^.magic#gmagic) THEN bad_desc; RETURN END;
  IF NOT g^.pup^.close THEN done:=FALSE; error:=err.duplicate; RETURN END;
  popen(g^.pup);
  IF NOT done THEN RETURN END;
  panel(g^.pup^.block,b,TRUE,FALSE)
END gopen;

PROCEDURE gclose(g: DEBUG);
BEGIN
  IF (g=NIL) OR (g^.magic#gmagic) THEN bad_desc; RETURN END;
  IF g^.pup^.close THEN done:=FALSE; error:=err.duplicate; RETURN END;
  pclose(g^.pup)
END gclose;

PROCEDURE gprint(g: DEBUG; VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR s: bmg.TOOL;
BEGIN
  IF (g=NIL) OR (g^.magic#gmagic) THEN bad_desc; RETURN END;
  IF g^.y>=g^.fnt^.H THEN
    DEC(g^.y,g^.fnt^.H)
  ELSE
    s:=g^.tol; s.color:=normal; s.mode:=bmg.rep;  scroll(s,0,-g^.fnt^.H)
  END;
  print(g^.tol,g^.fnt^.W DIV 2,g^.y,g^.fnt,fmt,arg)
END gprint;

--------------------------- dialogbox --------------------------
                           -----------

PROCEDURE _message(VAR pup: POPUP;
                 xc,yc,add: INTEGER;
                 VAR b,off: bmg.BLOCK;
                 VAL   fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR i: INTEGER;
      t: bmg.TOOL;
BEGIN
  pup:=NIL;
  b.w:=bmg.lenght(font,fmt,args)+add+font^.W+4+sfont^.W+4+FRAME;
  b.h:=font^.H+FRAME;
  b.x:=xc - b.w DIV 2;
  b.y:=yc - b.h DIV 2;
  shiftin(b);
  pnew(pup,b.x,b.y,b.w,b.h);
  IF NOT done THEN RETURN END;
  popen(pup);
  IF NOT done THEN pdispose(pup); RETURN END;
  panel0(b,b,off,TRUE);
  IF NOT done THEN pdispose(pup); RETURN END;
  t:=T; t.clip:=b;
  t.color:=normal/black; t.mode:=bmg.xor; t.mask:=act;
  i:=off.w+3+font^.W DIV 2;
  INC(t.clip.x,i);   DEC(t.clip.w,i);
  i:=xprint(t,font^.W DIV 2,0,font,fmt,args)+font^.W DIV 2;
  b:=t.clip; INC(b.x,i); DEC(b.w,i)
END _message;

PROCEDURE message(xc,yc: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
  VAR p: POPUP;
    b,o: bmg.BLOCK;
    s,r: BITSET;
    mus: BOOLEAN;
  dx,dy: INTEGER;
BEGIN
  _message(p,xc,yc,0,b,o,fmt,a);
  IF NOT done THEN RETURN END;
  startwait;
  mx:=o.x+o.w-2; my:=o.y+1; mus:=_mouse; _mouse:=TRUE;
  REPEAT
    read(o); r:=cpd.state^.keys;
    IF (ch=15c) OR (ch=33c) OR NOT awaited() THEN pdispose(p); RETURN END
  UNTIL r#{};
  _switch(o,TRUE);  _mouse:=mus;
  REPEAT cpd.read(dx,dy,s) UNTIL s*r={};
  pdispose(p)
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

PROCEDURE _diabox(xc,yc,w: INTEGER;
                  VAR   s: ARRAY OF CHAR;
                  VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

  VAR t: bmg.TOOL;      pup: POPUP;
      c: bmg.TOOL;      off: bmg.BLOCK;
      i: INTEGER;       x,o: INTEGER;
     sh: INTEGER;       r,k: BITSET;
    mus: BOOLEAN;       xit: BOOLEAN;

  PROCEDURE marker(erase: BOOLEAN);
  BEGIN
    c.back:=normal;  c.mode:=bmg.rep;  c.color:={};
    IF x+sfont^.W>t.clip.w THEN sfont^.W:=t.clip.w-x END;
    writech(c,x,0,sfont,empty);
    sfont^.W:=sh DIV 2;
    IF x+sfont^.W>t.clip.w THEN sfont^.W:=t.clip.w-x END;
    IF NOT erase THEN
      c.back:=normal/bright; c.color:={}; c.mode:=bmg.xor;
      writech(c,x,0,sfont,empty)
    END;
    sfont^.W:=sh
  END marker;

  PROCEDURE roll(left: BOOLEAN);
    VAR r: bmg.TOOL;
      j,w: INTEGER;
  BEGIN
    IF left THEN
      j:=0; w:=0;
      WHILE x+font^.W-w>t.clip.w DO w:=w+bmg.lenght(font,"%c",s[o+j]); INC(j) END;
      r:=t; r.color:=normal; r.mode:=bmg.rep;
      scroll(r,w,0); o:=o+j; x:=x-w
    ELSE
      DEC(o);
      w:=bmg.lenght(font,"%c",s[o]);
      r:=t; r.color:=normal; r.mode:=bmg.rep;
      scroll(r,-w,0); x:=x+w;
      write(t,0,0,font,s,o,1)
    END;
  END roll;

BEGIN
  IF HIGH(s)<=0  THEN bad_parm; RETURN END;
  IF w<font^.W*8 THEN w:=font^.W*8 END;
  t:=T; t.mask:=act;
  _message(pup,xc,yc,w,t.clip,off,fmt,arg);
  IF NOT done THEN RETURN END;
  IF t.clip.w<font^.W*4 THEN bad_parm; RETURN END;
  sh:=sfont^.H;
  t.color:=normal/black;  t.mode:=bmg.xor; c:=t;
  x:=0;
  mx:=t.clip.x+t.clip.w DIV 2; my:=t.clip.y+1;
  i:=0; o:=0;
  IF s#"" THEN
    WHILE (i<HIGH(s)) & (s[i]#0c) DO
      x:=xwrite(t,x,0,font,s,i,1); INC(i);
      IF x+font^.W>t.clip.w THEN roll(TRUE) END
    END
  END;
  startwait; mus:=_mouse; _mouse:=FALSE; xit:=FALSE;
  LOOP
    marker(FALSE);
    read(off,t.clip); r:=cpd.state^.keys;
    IF    (r*{0}#{}) & inblock(mx,my,t.clip) THEN            EXIT
    ELSIF  r*ank#{}                          THEN xit:=TRUE; EXIT
    ELSIF (r*{0}#{}) & inblock(mx,my,off)    THEN xit:=TRUE; EXIT
    ELSIF (i>0) & ((ch=key.back) OR (ch=key.left)) THEN
      marker(TRUE);  DEC(i);  DEC(x,bmg.lenght(font,"%c",s[i]));
      WHILE (o>0) & (x+font^.W*2<t.clip.w) DO roll(FALSE) END;
      _mouse:=FALSE
    ELSIF (ORD(ch) MOD 128 >= 32) & (i<HIGH(s)) THEN
      marker(TRUE);
      s[i]:=ch; x:=xwrite(t,x,0,font,s,i,1); INC(i);
      IF x+font^.W>t.clip.w THEN roll(TRUE) END;
      _mouse:=FALSE
    ELSIF  ch=15c                   THEN            EXIT
    ELSIF (ch=33c) OR NOT awaited() THEN xit:=TRUE; EXIT
    END;
    s[i]:=0c
  END;
  _mouse:=mus;
  IF xit THEN s[0]:=0c; _switch(off,TRUE) END;
  k:=r;
  WHILE k*r#{} DO cpd.read(mx,my,k) END;
  pdispose(pup);  done:=TRUE
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
                            ---------

PROCEDURE rnew(VAR rol: ROLLER; X,Y,W,H: INTEGER; exit: BITSET;
               VAL fmt: ARRAY OF CHAR;         SEQ arg: SYSTEM.WORD);
BEGIN
  NEW(rol);
  IF NOT done THEN RETURN END;
  rol^.tol:=T;
  WITH rol^ DO  tol.mask:=act; --++
    WITH tol.clip DO
      x:=X;  y:=Y;  w:=W;  h:=H;  top:=0;  ln:=0;
      shiftin(tol.clip);
      NEW(tit);   NEW(hot);
      NEW(txt);   NEW(cap);
      fnt:=font;  sfnt:=sfont;
      pup:=NIL;   xit :=exit;
      out:=TRUE;
      WITH up DO w:=0; h:=0; x:=0; y:=0 END;
      ttl:=up; dw:=up; rlr:=up; main:=up;
      magic:=rmagic;
      newstr(tit,fmt,arg);
      IF NOT done THEN rdispose(rol); RETURN END;
      pnew(pup,x,y,w,h);
      IF NOT done THEN rdispose(rol); RETURN END;
      tol.clip:=pup^.block;
      lns:=(h-FRAME-(fnt^.H+2*2)) DIV fnt^.H;
      IF (lns<2) OR ((w-FRAME-fnt^.W+4) DIV fnt^.W<2) THEN
        bad_parm; rdispose(rol); RETURN
      END;
      WITH tol DO color:=bright; back:=black; mode:=bmg.bic END
    END
  END
END rnew;

PROCEDURE rdispose(VAR r: ROLLER);
  VAR e: INTEGER;  d: BOOLEAN;
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN RETURN END;
  d:=done; e:=error;
  pdispose(r^.pup);  r^.magic:=0;  DISPOSE(r);
  done:=d; error:=e
END rdispose;

PROCEDURE rtoptoY(r: ROLLER; tp: INTEGER; VAR b: bmg.BLOCK);
  VAR i: INTEGER;
      N: INTEGER;
BEGIN
  ASSERT((r#NIL) & (r^.magic=rmagic) & NOT r^.pup^.close);
  WITH r^ DO
    N:=HIGH(txt)+1; ASSERT(N>lns);
    b:=rlr;  i:=b.w;
    WITH b DO
      h:=lns*b.h DIV N;      INC(x,2);   DEC(w,4);
      IF h<i THEN h:=i DIV 2 END;
      i:=(rlr.h-h-4);
      y:=y+rlr.h-2-h - (tp*i) DIV (N-lns)
    END
  END
END rtoptoY;

PROCEDURE lifter(t: bmg.TOOL; c0,c1: BITSET);
BEGIN
  WITH t.clip DO
    t.color:=normal/c0;  line(t,0,1,0,h-1);  line(t,1,h-1,w-2,h-1);
    t.color:=normal/c1;  line(t,0,0,w-1,0);  line(t,w-1,1,w-1,h-1)
  END
END lifter;

PROCEDURE rollerman(r: ROLLER; dt: INTEGER);
  VAR c: CHAR;
      N: INTEGER;
      t: bmg.TOOL;
   b,b1: bmg.BLOCK;
BEGIN
  ASSERT((r#NIL) & (r^.magic=rmagic) & NOT r^.pup^.close);
  WITH r^ DO
    N:=HIGH(txt)+1;
    IF N<=lns THEN RETURN END;
    rtoptoY(r,top+dt,b1);
    t:=tol;   t.mode:=bmg.xor;
    IF dt#0   THEN
      rtoptoY(r,top,b);
      IF b1.y#b.y THEN t.clip:=b; lifter(t,bright,shadow) END
    END;
    t.clip:=b1;  lifter(t,bright,shadow);
    IF (dt=0) OR ((top=0) # (top+dt=0)) THEN
      t:=tol; t.clip:=up;
      IF top+dt>0 THEN
        t.mode:=bmg.bic; t.back:=black;    t.color:=bright;   c:=utria
      ELSE
        t.mode:=bmg.rep; t.back:=normal;   t.color:={};       c:=empty
      END;
      writech(t,0,0,sfnt,c)
    END;
    IF (dt=0) OR ((top+lns#N) # (top+lns+dt#N)) THEN
      t:=tol; t.clip:=dw;
      IF top+lns+dt<N THEN
        t.mode:=bmg.bic; t.back:=black;  t.color:=bright;  c:=dtria
      ELSE
        t.mode:=bmg.rep; t.back:=normal;  t.color:=black;  c:=empty
      END;
      writech(t,0,0,sfnt,c)
    END
  END
END rollerman;

PROCEDURE rY(r: ROLLER; line: INTEGER): INTEGER;
BEGIN RETURN r^.tol.clip.h-(line+1)*r^.fnt^.H END rY;

PROCEDURE rxor(r: ROLLER; ln: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
  WITH r^ DO
    t:=tol;
    WITH t DO
      mode:=bmg.xor; mask:=act;
      WITH t.clip DO
        DEC(x,2); INC(w,4);
        h:=fnt^.H+1;
        y:=tol.clip.y+rY(r,ln-top)-1;
        color:=normal/bright;  line(t,1,0,w-1,0);  line(t,w-1,1,w-1,h-1);
        color:=normal/shadow;  line(t,0,0,0,h-1);  line(t,1,h-1,w-1,h-1)
      END
    END
  END
END rxor;

PROCEDURE rshow(r: ROLLER; refresh: BOOLEAN);
  VAR t: bmg.TOOL;
  i,j,Y: INTEGER;
BEGIN
  WITH r^ DO
    IF NOT refresh THEN
      WITH up DO w:=0; h:=0; x:=0; y:=0 END; ttl:=up; dw:=up; rlr:=up;
      IF HIGH(txt)+1<=lns THEN
        panel1(pup^.block,main,off,ttl,"%s",tit)
      ELSE
        panel2(pup^.block,main,off,ttl,up,rlr,dw,"%s",tit)
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
      t:=T;  t.mask:=act;  t.mode:=bmg.rep;  t.color:=normal;
      t.clip:=rlr;  rect(t,0,0,t.clip.w-1,t.clip.h-1); t.clip:=tol.clip
    END;
    j:=lns;
    IF top+j>HIGH(txt)+1 THEN j:=HIGH(txt)+1-top END;
    FOR i:=0 TO j-1 DO
      Y:=rY(r,i);
      IF refresh THEN rect(t,0,Y,t.clip.w-1,fnt^.H-1+Y) END;
      print(tol,0,Y,fnt,"%s",txt[top+i])
    END;
    IF refresh & (j<lns) THEN
      Y:=rY(r,j);    i:=rY(r,lns-1);
      rect(t,0,i,t.clip.w-1,fnt^.H-1+Y)
    END;
    tol.mode:=bmg.bic; rollerman(r,0)
  END
END rshow;

PROCEDURE rsetstr(r: ROLLER;           line: INTEGER;
            VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR t: bmg.TOOL; Y: INTEGER;
BEGIN
  IF (r=NIL ) OR (r^.magic#rmagic)   THEN bad_desc; RETURN END;
  IF (line<0) OR (line>HIGH(r^.txt)) THEN bad_parm; RETURN END;
  done:=TRUE;
  IF HIGH(r^.txt[line])<0 THEN RETURN END;
  str.print(r^.txt[line],fmt,arg);
  IF r^.pup^.close OR (line<r^.top) OR (line>=r^.top+r^.lns) THEN RETURN END;
  Y:=rY(r,line-r^.top);
  t:=r^.tol; t.color:=normal; t.mode:=bmg.rep;
  rect(t,0,Y,t.clip.w-1,r^.fnt^.H-1+Y);
  print(r^.tol,0,Y,r^.fnt,"%s",r^.txt[line])
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
    IF NOT pup^.close THEN rshow(r,(H0<=lns)=(H1<=lns)) END
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
  IF NOT r^.pup^.close  THEN done:=FALSE; error:=err.duplicate; RETURN END;
  popen(r^.pup);
  IF NOT done THEN RETURN END;
  rshow(r,FALSE)
END ropen;

PROCEDURE rclose(r: ROLLER);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic) THEN bad_desc; RETURN END;
  IF r^.pup^.close THEN done:=FALSE; error:=err.duplicate; RETURN END;
  pclose(r^.pup)
END rclose;

PROCEDURE rselect(r: ROLLER);

  VAR N: INTEGER;
    pop: BOOLEAN;
    mus: BOOLEAN;
   lbut: BOOLEAN;
   rbut: BOOLEAN;
  p,s,k: BITSET;
  ox,oy: INTEGER;

  PROCEDURE in(b: bmg.BLOCK): BOOLEAN; BEGIN RETURN inblock(mx,my,b) END in;

  PROCEDURE _jump(line: INTEGER; xor: BOOLEAN);
    VAR t: bmg.TOOL;
  BEGIN
    WITH r^ DO
      IF (line<0) OR (line>=N) OR (line=ln) THEN RETURN END;
      IF xor THEN rxor(r,ln) END;
      t:=tol; t.mask:=act;
      IF    line<top      THEN ASSERT(line=top-1);
        t.color:=normal;  t.mode:=bmg.rep; scroll(t,0,+fnt^.H);
        rollerman(r,-1);  DEC(top);
        print(tol,0,rY(r,line-top),fnt,"%s",txt[line])
      ELSIF line>=top+lns THEN ASSERT(line=top+lns);
        t.color:=normal;  t.mode:=bmg.rep; scroll(t,0,-fnt^.H);
        rollerman(r,+1);  INC(top);
        print(tol,0,rY(r,line-top),fnt,"%s",txt[line])
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

    PROCEDURE roll(n: INTEGER; VAL b: bmg.BLOCK);
      VAR i,d: INTEGER; but: bmg.BLOCK;
    BEGIN
      WITH r^ DO
        i:=ln-top;
        crs.toggle(FALSE);
        rxor(r,ln);  d:=200;
        but:=b; DEC(but.x,2); DEC(but.y,2); INC(but.w,4); INC(but.h,4);
        _block(but,FALSE,TRUE);
        WHILE (top+n>=0) & (top+n<N) & lbut DO
          _jump(top+n,FALSE);
          _read(d,p,s,FALSE);
          d:=0;
          lbut:=(s*{0}#{}) & in(b)
        END;
        but:=b; DEC(but.x,2); DEC(but.y,2); INC(but.w,4); INC(but.h,4);
        _block(but,FALSE,FALSE);
        ln:=top+i;
        IF ln>HIGH(txt) THEN ln:=HIGH(txt) END;
        rxor(r,ln);
        crs.toggle(TRUE)
      END
    END roll;

    PROCEDURE roller;
      VAR t: bmg.TOOL;
          b: bmg.BLOCK;
          c: bmg.BLOCK;
        x,y: INTEGER;
      ox,oy: INTEGER;
      i,k,l: INTEGER;
    BEGIN
      WITH r^ DO
        crs.toggle(FALSE);
        i:=top;  l:=ln-top;
        t:=tol;  t.mode:=bmg.xor; t.mask:=act; t.color:=t.mask;
        rtoptoY(r,i,b); t.clip:=b; lifter(t,bright,shadow);
        c:=rlr;  c.h:=rlr.h-b.h-4;
        x:=mx;   y:=my;
        IF    my<b.y      THEN  my:=my-4;     b.y:=my
        ELSIF my>=b.y+b.h THEN  my:=my-4-b.h; b.y:=my
        ELSE                    my:=b.y-4
        END;
        clipinto(mx,my,c);
        WITH b DO k:=(N-lns)*(c.h-1-(my-c.y)) DIV (c.h-1) END;
        IF k<0 THEN k:=0 ELSIF k>N-lns THEN k:=N-lns END;
        i:=k; rtoptoY(r,i,b); t.clip:=b;
        lifter(t,shadow,bright);
        LOOP
          ox:=mx; oy:=my;
          REPEAT _read(20,p,s,FALSE) UNTIL (s*{0}={}) OR (my#oy);
          INC(x,mx-ox);
          INC(y,my-oy);
          IF s*{0}={} THEN EXIT END;
          clipinto(mx,my,c);
          WITH b DO k:=(N-lns)*(c.h-1-(my-c.y)) DIV (c.h-1) END;
          IF k<0 THEN k:=0 ELSIF k>N-lns THEN k:=N-lns END;
          IF k#i THEN
            i:=k; rtoptoY(r,i,b);
            lifter(t,shadow,bright); t.clip:=b; lifter(t,shadow,bright)
          END
        END;
        lifter(t,shadow,bright);
        lifter(t,bright,shadow);
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
      WITH pup^.block DO
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
    pop:=pup^.close;
    IF pop      THEN ropen(r) END;
    IF NOT done THEN RETURN   END;
    mx:=tol.clip.x+fnt^.W DIV 2;
    my:=tol.clip.y+rY(r,ln-top)+fnt^.H DIV 2;
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
        i:=top + (tol.clip.h-1-(my-tol.clip.y)) DIV fnt^.H;
        IF i#ln THEN crs.toggle(FALSE); _jump(i,TRUE); crs.toggle(_mouse) END
      END;
      IF (ch#0c) & _kselect() THEN EXIT END
    END;
    crs.toggle(FALSE);
    rxor(r,ln);
    IF out THEN  _switch(off,TRUE)   END;
    k:=s;  _mouse:=FALSE;
    WHILE s*k#{} DO _read(20,p,s,FALSE) END;
    IF out THEN  _switch(off,FALSE)  END;
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

PROCEDURE rblocks(r: ROLLER; VAR main,txt,tit,off,up,rlr,dw: bmg.BLOCK);
BEGIN
  IF (r=NIL) OR (r^.magic#rmagic)     THEN bad_desc; RETURN END;
  IF (r^.main.w<=0) OR (r^.main.h<=0) THEN undef;    RETURN END;
  main:=r^.main;        up :=r^.up;
  tit :=r^.ttl;         dw :=r^.dw;
  off :=r^.off;         rlr:=r^.rlr;    txt:=r^.tol.clip;
  done:=TRUE
END rblocks;

----------------------------- menus ----------------------------
                             -------

PROCEDURE mnew(VAR m: MENU; x,y,w,h: INTEGER; exit: BITSET;
             VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR i: INTEGER;
BEGIN
  NEW(m);
  IF NOT done THEN RETURN END;
  m^.rol  :=rnull;
  m^.magic:=mmagic;
  rnew(m^.rol,x,y,w,h,exit,fmt,arg);
  IF NOT done THEN mdispose(m); RETURN END;
  WITH m^.rol^ DO
    NEW(txt,lns);
    IF NOT done THEN mdispose(m); RETURN END;
    FOR i:=0 TO HIGH(txt) DO NEW(txt[i]) END;
    FOR i:=0 TO HIGH(txt) DO
      RESIZE(txt[i],tol.clip.w*2 DIV fnt^.W);
      IF NOT done THEN mdispose(m); RETURN END;
      txt[i][0]:=0c
    END;
    RESIZE(hot,lns);
    IF NOT done THEN mdispose(m); RETURN END;
    RESIZE(cap,lns);
    IF NOT done THEN mdispose(m); RETURN END;
    FOR i:=0 TO lns-1 DO hot[i]:=0c; cap[i]:=0c END
  END
END mnew;

PROCEDURE mdispose(VAR m: MENU);
  VAR i,e: INTEGER;    d: BOOLEAN;
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN RETURN END;
  d:=done; e:=error;
  IF m^.rol#NIL THEN
    WITH m^.rol^ DO
      FOR i:=0 TO HIGH(txt) DO DISPOSE(txt[i]) END;
      DISPOSE(txt);  DISPOSE(hot);  DISPOSE(cap)
    END
  END;
  rdispose(m^.rol);  m^.magic:=0;  DISPOSE(m);
  done:=d; error:=e
END mdispose;

PROCEDURE mprint(m: MENU;               alt: INTEGER;
           VAL fmt: ARRAY OF CHAR;  SEQ arg: SYSTEM.WORD);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  rsetstr(m^.rol,alt,fmt,arg)
END mprint;

PROCEDURE mread(m: MENU; alt: INTEGER; VAR s: ARRAY OF CHAR);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  WITH m^.rol^ DO
    IF (alt<0) OR (alt>=lns) THEN bad_parm; RETURN END;
    str.copy(s,txt[alt]); done:=TRUE
  END
END mread;

PROCEDURE mhotkey(m: MENU; no: INTEGER; hotkey: CHAR; capequ: BOOLEAN);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  WITH m^.rol^ DO
    IF (no<0) OR (no>=lns) THEN bad_parm; RETURN END;
    hot[no]:=hotkey;  done:=TRUE;
    IF capequ THEN
      hot[no]:=ASCII.SMALL  (hotkey);
      cap[no]:=ASCII.CAPITAL(hotkey)
    END
  END
END mhotkey;

PROCEDURE mopen (m: MENU);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  ropen(m^.rol)
END mopen;

PROCEDURE mclose(m: MENU);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  rclose(m^.rol)
END mclose;

PROCEDURE mselect(m: MENU);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  rselect(m^.rol)
END mselect;

PROCEDURE mselected(m: MENU): BOOLEAN;
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN RETURN FALSE END;
  RETURN rselected(m^.rol)
END mselected;

PROCEDURE mchoose(m: MENU; alt: INTEGER);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  rchoose(m^.rol,alt)
END mchoose;

PROCEDURE malt(m: MENU): INTEGER;
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN -1 END;
  RETURN ralt(m^.rol)
END malt;

PROCEDURE mblocks(m: MENU; VAR main,txt,tit,off: bmg.BLOCK);
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  WITH m^ DO
    IF (rol^.main.w<=0) OR (rol^.main.h<=0) THEN undef; RETURN END;
    main:=rol^.main;    txt:=rol^.tol.clip;
    tit :=rol^.ttl;     off:=rol^.off;
    done:=TRUE
  END
END mblocks;

----------------------------- direx ----------------------------
                             -------

PROCEDURE BIO_error(d: DIREX; VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
  VAR e: INTEGER;
BEGIN
  WITH d^.rol^.pup^.block DO
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
      t: bmg.TOOL;
      i: INTEGER;
    cdn: ARRAY [0..31] OF CHAR;
    pop: BOOLEAN;
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  done:=TRUE;
  WITH d^ DO
    pop:=rol^.pup^.close;
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
      t.mode:=bmg.rep; t.color:=normal; rect(t,0,0,t.clip.w-1,t.clip.h-1);
      t.mode:=bmg.bic; t.color:=bright;
      i:=t.clip.w-bmg.lenght(rol^.fnt,"%s",cdn);
      print(t,i DIV 2,0,rol^.fnt,"%s",cdn);
      rselect(rol);
      IF NOT done              THEN EXIT END;
      IF NOT rselected(rol) THEN EXIT END;
      IF rol^.ln=HIGH(dir) THEN
        ext:="";
        WITH rol^.pup^.block DO
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

PROCEDURE dblocks(d: DIREX; VAR main,txt,tit,off,up,rl,dw: bmg.BLOCK);
BEGIN
  IF (d=NIL) OR (d^.magic#dmagic) THEN bad_desc; RETURN END;
  rblocks(d^.rol,main,txt,tit,off,up,rl,dw)
END dblocks;

----------------------------- zoom -----------------------------
                             ------

PROCEDURE zoom;

  VAR R: ARRAY CHAR OF BITSET;

  CONST four0={0..3};           four1=four0<<4;
        four2=four1<<4;         four3=four2<<4;
        four01=four0+four1;     four12=four1+four2;
        four02=four0+four2;     four13=four1+four3;
        four03=four0+four3;     four23=four2+four3;

  CONST tini = ARRAY OF BITSET {
          {},          four0,          four1,         four01,
       four2,         four02,         four12,   four12+four0,
       four3,         four03,         four13,   four13+four0,
      four23,   four23+four0,   four23+four1,   four23+four01 };

  VAR rect: BOOLEAN;

  PROCEDURE _zoom(x,y,xc,yc: INTEGER);  (*$<$T-*)

    PROCEDURE bitmove(d,do,s,so,l: SYSTEM.ADDRESS); CODE cod.bmv END bitmove;

    TYPE WA = ARRAY [0..2] OF BITSET;
         BA = ARRAY [0..2] OF CHAR;

    VAR zoo: ARRAY [0..HIGH(B^.layers)] OF ARRAY [0..22] OF WA;
        img: ARRAY [0..HIGH(B^.layers)] OF ARRAY [0..22] OF BA;
          m: BITSET;           a0: INTEGER;
       L,im: INTEGER;          i0: POINTER TO BA;
  i,o,a,b,n: INTEGER;        z,z0: SYSTEM.ADDRESS;

    CONST W=ARRAY OF INTEGER
    {14,24,30,36,40,44,48,50,54,56,58,60,64,66,68,68,70,72,74,76,76,78,80
    ,80,82,82,84,84,86,86,86,88,88,88,90,90,90,90,90,92,92,92,92,92,92,92
    ,92,92,92,92,92,92,92,92,90,90,90,90,90,88,88,88,86,86,86,84,84,82,82
    ,80,80,78,76,76,74,72,70,68,68,66,64,60,58,56,54,50,48,44,40,36,30,24,14};
    CONST O=ARRAY OF INTEGER
    {40,35,32,29,27,25,23,22,20,19,18,17,15,14,13,13,12,11,10, 9, 9, 8, 7
    , 7, 6, 6, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1
    , 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6
    , 7, 7, 8, 9, 9,10,11,12,13,13,14,15,17,18,19,20,22,23,25,27,29,32,35,40};

  BEGIN
    x:=x-11;    xc:=xc-46;
    y:=y-11;    yc:=yc-46;
    IF x<0 THEN x:=0 ELSIF x+23>T.clip.w THEN x:=T.clip.w-23 END;
    IF y<0 THEN y:=0 ELSIF y+23>T.clip.h THEN y:=T.clip.h-23 END;
    L:=0;  m:=T.mask;  a0:=(B^.H-1-y)*wpl+x DIV 32;  b:=x MOD 32;
    REPEAT
      IF m*{0}#{} THEN
        im:=SYSTEM.ADR(img[L]);  a:=B^.layers[L]+a0;      i:=23;
        REPEAT bitmove(im,0,a,b,24); DEC(a,wpl); INC(im); i:=i-1 UNTIL i=0
      END;  m:=(m-{0})>>1; INC(L)
    UNTIL m={};
    L:=0; m:=T.mask;  a0:=(B^.H-1-yc)*wpl+xc DIV 32;  b:=xc MOD 32;
    REPEAT
      IF m*{0}#{} THEN
        a:=B^.layers[L]+a0; z:=SYSTEM.ADR(zoo[L]); i0:=SYSTEM.ADR(img[L]);
        n:=92;
        REPEAT
          z0:=z;
          z^:=R[i0^[0]]; INC(z); z^:=R[i0^[1]]; INC(z); z^:=R[i0^[2]]; INC(z);
          IF rect THEN
            move(a,z0,3); DEC(a,wpl);  move(a,z0,3); DEC(a,wpl);
            move(a,z0,3); DEC(a,wpl);  move(a,z0,3); DEC(a,wpl); DEC(n,4)
          ELSE
            o:=O[n]; bitmove(a,b+o,z0,o,W[n]); DEC(a,wpl); DEC(n);
            o:=O[n]; bitmove(a,b+o,z0,o,W[n]); DEC(a,wpl); DEC(n);
            o:=O[n]; bitmove(a,b+o,z0,o,W[n]); DEC(a,wpl); DEC(n);
            o:=O[n]; bitmove(a,b+o,z0,o,W[n]); DEC(a,wpl); DEC(n);
          END;
          i0:=SYSTEM.ADDRESS(i0)+1
        UNTIL n=0
      END;
      m:=(m-{0})>>1; INC(L)
    UNTIL m={}
  END _zoom;  (*$>*)

  VAR t: bmg.TOOL;
      r: bmg.TOOL;
      b: bmg.BLOCK;
      m: bmg.BLOCK;
    off: bmg.BLOCK;
    pup: POPUP;
  zx,zy: INTEGER;
 corner: INTEGER;

  PROCEDURE _new;
    VAR i: bmg.BLOCK;
        s: bmg.TOOL;
  BEGIN
    WITH b DO
      w:=96+64; h:=96+FRAME+7;
      CASE corner OF
        |0: y:=0;          x:=0
        |1: y:=0;          x:=T.clip.w-w
        |2: y:=T.clip.h-h; x:=T.clip.w-w
        |3: y:=T.clip.h-h; x:=0
      END;
      corner:=(corner+1) MOD 4;
      pdispose(pup);
      pnew(pup,x,y,w,h);
      IF NOT done THEN RETURN END;
      popen(pup);
      IF NOT done THEN RETURN END
    END;
    panel0(b,m,off,FALSE);
    r:=T; r.clip:=m; r.back:=normal; r.color:=shadow; r.mode:=bmg.rep;
    zx:=m.x+m.w DIV 2;   zy:=m.y+m.h DIV 2;
    WITH m DO DEC(x,12); DEC(y,12); INC(w,24); INC(h,24) END;
  END _new;

  PROCEDURE cursor(x,y: INTEGER; toprint: BOOLEAN);
    VAR i,j: INTEGER;
  BEGIN
    IF rect THEN frame (t,x-11,y-11,x+12,y+11)
    ELSE         circle(t,x,y,13)
    END;
    IF NOT toprint THEN RETURN END;
    i:=r.clip.w-2-fnt.font^.W*4;
    j:=r.clip.h DIV 2-fnt.font^.H DIV 2;
    print(r,i,j,fnt.font,"%-4d",y);
    print(r,1,j,fnt.font,"%4d" ,x)
  END cursor;

  PROCEDURE round; BEGIN panel0(b,m,off,FALSE); rect:=NOT rect END round;

  VAR dx,dy,ox,oy,i: INTEGER; s,p,k: BITSET;

BEGIN
  FOR i:=0 TO 377b DO R[CHR(i)]:=tini[i MOD 16]+tini[i DIV 16]<<16 END;
  pup:=pnull;
  corner:=2;  rect:=FALSE;
  _new;
  t:=T;  t.color:=B^.mask;  t.mode:=bmg.xor;
  mx:=T.clip.w DIV 2;
  my:=T.clip.h DIV 2;
  LOOP
    clipinto(mx,my,T.clip);
    ox:=mx; oy:=my;  p:={}; s:={};
    dot(t,ox,oy);
    _zoom(ox,oy,zx,zy);
    cursor(ox,oy,TRUE);
    IF cpd.ready()>0 THEN
      REPEAT
        startwait; p:=cpd.state^.keys; cpd.read(dx,dy,s); mx:=mx+dx; my:=my+dy
      UNTIL cpd.ready()=0
    ELSE
      REPEAT
        startwait; _read(20,p,s,FALSE)
      UNTIL (p#s) OR (mx#ox) OR (my#oy) OR (ch#0c) OR (time=0)
    END;
    dot(t,ox,oy);
    cursor(ox,oy,FALSE);
    CASE ch OF
    |key.right: mx:=mx+8
    |key.left : mx:=mx-8
    |key.up   : my:=my+8
    |key.dw   : my:=my-8
    ELSE
    END;
    IF (p*ank={}) & (s*ank#{}) OR (ch=33c) OR (time=0) THEN EXIT  END;
    IF (p*{0}={}) & (s*{0}#{}) OR (ch=15c)             THEN round END;
    IF inblock(mx,my,m) THEN _new END
  END;
  REPEAT _read(20,p,s,FALSE) UNTIL s*ank={};
  pdispose(pup)
END zoom;

----------------------------------------------------------------

VAR i,i0,i1: INTEGER;

BEGIN
  done:=TRUE;  error:=err.ok;
  fnt.read(font,'PM.fnt');
  std.print("fnt.done=%d\n", fnt.done);
  IF NOT fnt.done THEN font:=fnt.font END;
  fnt.unpack(font);
  make_marks; ASSERT(done);
  scr.loophole(scr.bitmap,B);
  IF NOT scr.done THEN HALT(scr.error) END;
  wpl  :=B^.WPL;
  pnull:=NIL;           pmagic:=00505550h;
  mnull:=NIL;           mmagic:=554E454Dh;
  rnull:=NIL;           rmagic:=4C4C4F52h;
  dnull:=NIL;           dmagic:=00585244h;
  gnull:=NIL;           gmagic:=00475542h;
  time :=MAX(INTEGER); timeout:=MAX(INTEGER);
  ch:=0c;
  mx:=0;
  my:=0;
  F:=NIL;                   TM:=NIL;
  WITH T.clip DO x:=0; y:=0; w:=scr.state^.W; h:=scr.state^.H END;
  T.zX:=0;
  T.zY:=0;
  T.mode:=bmg.rep;
  T.mask:={};  i0:=-1;
  T.back:={};  i1:=-1;  nlr:=0;
  FOR i:=HIGH(B^.layers) TO 0 BY -1 DO
    IF B^.layers[i]#NIL THEN INCL(T.mask,i); INC(nlr);
      IF    i1<0 THEN i1:=i
      ELSIF i0<0 THEN i0:=i
      END
    END
  END;
  black :={};
  IF scr.state^.type#49h THEN
    shadow:={i0};  normal:={i1};     bright:={i0,i1};
  ELSE
    shadow:={3};   normal:={0,1,2};  bright:={0..3};
  END;
  act:=bright+normal+shadow;
  T.color:=normal;
  cpd.nop;
  IF cpd.state^.nokeys>1 THEN ank:={cpd.state^.nokeys-1} END;
  setcolors;
  _mouse:=FALSE;
END pmPUP.
