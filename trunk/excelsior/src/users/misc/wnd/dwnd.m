MODULE dwnd; (*  11-Apr-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT       PCX;
IMPORT  wnd: pmWnd;
IMPORT  wm : pmWM;
IMPORT  crs: pmCrs;
IMPORT  pup: pmPUP;
IMPORT  cod: defCodes;
IMPORT  key: Keyboard;
IMPORT  fnt: Fonts;
IMPORT  scr: Screen;
IMPORT  cpd: CPD;
IMPORT  bmg: BMG;
IMPORT  bio: BIO;
IMPORT  low: lowLevel;
IMPORT  mem: Heap;
IMPORT  rnd: Random;

WITH STORAGE: mem;

PROCEDURE wait;
  VAR ch: CHAR;
BEGIN
  key.read(ch)
END wait;

VAR   B: bmg.BITMAP;
      T: bmg.TOOL;
  color: INTEGER;
 header: PCX.HEADER;
 _paper: bmg.BITMAP;
  paper: bmg.BITMAP;

   zoom: wnd.WINDOW;
    open: wnd.WINDOW;
   cascade: wnd.WINDOW;
quit: wnd.WINDOW;

PROCEDURE bmv(a,b,c,d,e: INTEGER); CODE cod.bmv END bmv;


PROCEDURE unpack;
  VAR i,w,h,s0,s1,d0,d1,d2,d3: INTEGER;
ch: CHAR;
BEGIN
  NEW(paper);
  paper^.W:=512;
  paper^.H:=512;
  paper^.WPL:=16;
  paper^.mask:={0..3};
  FOR i:=0 TO 7 DO paper^.layers[i]:=NIL END;
  FOR i:=0 TO 3 DO
    mem.allocate(paper^.layers[i],paper^.WPL*paper^.H);
    low._zero(paper^.layers[i],paper^.WPL*paper^.H);
  END;
  bmg.offset(_paper,0,0,0,s0,i);
  bmg.offset(_paper,0,0,1,s1,i);
  bmg.offset( paper,0,0,0,d0,i);
  bmg.offset( paper,0,0,1,d1,i);
  bmg.offset( paper,0,0,2,d2,i);
  bmg.offset( paper,0,0,3,d3,i);
  h:=-_paper^.H*paper^.WPL;
  w:=_paper^.W;
  FOR i:=0 TO _paper^.H-1 DO
    bmv(d0,0,s1,0,w);   bmv(d0+h,0,s1,0,w);
    bmv(d0,w,s1,0,w);   bmv(d0+h,w,s1,0,w);
    bmv(d1,0,d0,0,w*2); bmv(d1+h,0,d0,0,w*2);
    bmv(d2,0,d0,0,w*2); bmv(d2+h,0,d0,0,w*2);
    bmv(d3,0,s0,0,w);   bmv(d3+h,0,s0,0,w);
    bmv(d3,w,s0,0,w);   bmv(d3+h,w,s0,0,w);
    DEC(d0,paper^.WPL);  DEC(d2,paper^.WPL);  DEC(s0,_paper^.WPL);
    DEC(d1,paper^.WPL);  DEC(d3,paper^.WPL);  DEC(s1,_paper^.WPL);
  END;
  FOR i:=0 TO 1 DO
    mem.deallocate(_paper^.layers[i],_paper^.WPL*_paper^.H)
  END;
  DISPOSE(_paper)
END unpack;

PROCEDURE ground(x,y,w,h: INTEGER);
  VAR i,s: INTEGER;
      zero: ARRAY [0..15] OF INTEGER;
      z : INTEGER;
      d0,s0: INTEGER;
      d1,s1: INTEGER;
      d2,s2: INTEGER;
      d3,s3: INTEGER;
BEGIN
  FOR i:=0 TO 15 DO zero[i]:=0 END;
  z:=SYSTEM.ADR(zero);
  bmg.offset(B,0,y,0,d0,i);
  bmg.offset(B,0,y,1,d1,i);
  bmg.offset(B,0,y,2,d2,i);
  bmg.offset(B,0,y,3,d3,i);
  bmg.offset(paper,0,y,0,s0,i);
  bmg.offset(paper,0,y,1,s1,i);
  bmg.offset(paper,0,y,2,s2,i);
  bmg.offset(paper,0,y,3,s3,i);
  FOR i:=0 TO h-1 DO
    bmv(d0,x,s0,x,w);   bmv(d2,x,s2,x,w);
    bmv(d1,x,s1,x,w);   bmv(d3,x,s3,x,w);
    DEC(s0,16);  DEC(d0,16);
    DEC(s1,16);  DEC(d1,16);
    DEC(s2,16);  DEC(d2,16);
    DEC(s3,16);  DEC(d3,16);
  END
END ground;

PROCEDURE fill(win: wnd.WINDOW);
  VAR t: bmg.TOOL;
BEGIN
 t:=win^.inner; t.mode:=bmg.bic;
 wnd.rect(win,t,0,0,t.clip.w-1,t.clip.h-1); ASSERT(wnd.done,wnd.error)
END fill;

PROCEDURE activate(VAR w: wnd.WINDOW; x,y: INTEGER);
  VAR t: bmg.TOOL;
      n: wnd.WINDOW;
BEGIN
  n:=wnd.locate(x,y);
  IF  n=NIL                    THEN RETURN END;
  IF (n=cascade) OR (n=quit) THEN RETURN END;
  IF (n=zoom) OR (n=open)     THEN RETURN END;
  IF w#NIL THEN
    t:=w^.full;
    t.mode:=bmg.bic;
    wnd.dot(w,t,2,2);     ASSERT(wnd.done,wnd.error);
  END;
  w:=n;
  wnd.dot(w,w^.full,2,2);     ASSERT(wnd.done,wnd.error);
END activate;

PROCEDURE new(x,y: INTEGER);
  VAR w: wnd.WINDOW;
BEGIN
  wm.new   (w);                   ASSERT(wm.done,wm.error);
  wm.button(w,1,wm.luc,22,-16,13,13);
  wm.button(w,2,wm.luc,40,-16,13,13);
  wm.print (w,1,wm.ssfont,"%c",wm.sszoomin);
  wm.print (w,2,wm.ssfont,"%c",wm.sszoomout);

  wnd.mask  (w,{0..3}-{color},{color});  ASSERT(wnd.done,wnd.error);
  wnd.resize(w,90,50);             ASSERT(wnd.done,wnd.error);
  wnd.move  (w,x,y);               ASSERT(wnd.done,wnd.error);
  wnd.open  (w);                   ASSERT(wnd.done,wnd.error);
  fill(w);
  IF color=3 THEN color:=0 ELSE color:=color+1 END;
END new;

PROCEDURE Cascade;
  VAR v: wnd.WINDOW; x,y: INTEGER;
BEGIN
  x:=0;
  y:=330;
  v:=wnd.bottom;
  WHILE v#NIL DO
    IF NOT v^.closed & (v#cascade)
                     & (v#zoom)
                     & (v#open)
                     & (v#quit)
    THEN
      wnd.move(v,x,y-v^.h); DEC(y,10); INC(x,3);
    END;
    v:=wnd.up(v)
  END
END Cascade;


VAR rand: ARRAY [0..255] OF INTEGER;  rx: INTEGER;

PROCEDURE random(l,h: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=h-l+1;
  IF i<8 THEN i:=8 END;
  i:=rand[rx] MOD i + l; rx:=(rx+1) MOD (HIGH(rand)+1); RETURN i
END random;

PROCEDURE monitor;
  VAR w,u: wnd.WINDOW;
    x,y,dx,dy,i,j,r,r1,x0,x1,x2,y0,y1,y2: INTEGER; s,keys: BITSET;
    ch: CHAR; t,l,t1: bmg.TOOL;
BEGIN
  x:=240; y:=180;
  t.zX    :=0;
  t.zY    :=0;
  t.mask  :={0,3};
  t.mode  :=bmg.xor;
  t.clip.x:=0;
  t.clip.w:=480;
  t.clip.y:=0;
  t.clip.h:=360;
  t.color :={0,3};
  t.back  :={};
  w:=NIL;  ch:=' ';
  LOOP
    crs.toggle(TRUE);
    keys:=cpd.state^.keys;
    REPEAT
      wm.monitor;
      s:=cpd.state^.keys;
    UNTIL s#keys;
    crs.toggle(FALSE);
    dx:=x-crs.x;
    dy:=y-crs.y;
    x:=crs.x;
    y:=crs.y;

    IF (s*{1}#{}) THEN
      REPEAT cpd.read(dx,dy,s) UNTIL s*{1}={}; new(x,y); activate(w,x,y)
    END;

    IF (s*{0}#{}) & (wnd.locate(x,y)=zoom) THEN
      pup.zoom;
    ELSIF (s*{0}#{}) & (wnd.locate(x,y)=open) THEN
      u:=wnd.top;
      WHILE u#NIL DO wnd.open(u); u:=wnd.dw(u) END
    ELSIF (s*{0}#{}) & (wnd.locate(x,y)=cascade) THEN
      Cascade
    ELSIF (s*{0}#{}) & (wnd.locate(x,y)=quit)  THEN
      HALT
    ELSIF (s*{2}#{}) & (w#NIL) THEN
      l:=w^.inner; l.mode:=bmg.xor;
      FOR j:=0 TO 29 DO
        x0:=random(-8,w^.w+8);
        y0:=random(-8,w^.h+8);
  --    x1:=random(-8,w^.w+8);
  --    y1:=random(-8,w^.h+8);
  --    x2:=random(-8,w^.w+8);
  --    y2:=random(-8,w^.h+8);
        r :=random(4,w^.w DIV 8);
        r1:=random(4,w^.w DIV 8);
        i :=random(1,15);
        l.color:=BITSET(i);
        wnd.ring(w,l,x0,y0,r,r1)
      END;
      l.clip.x:=crs.x-w^.x-l.zX;
      l.clip.y:=crs.y-w^.y-l.zY;
      l.clip.w:=w^.w DIV 8;
      l.clip.h:=w^.h DIV 8;
      t1:=l;
      l .color:=w^.fore;
      t1.color:=w^.fore;
      WHILE s*{2}#{} DO
        l .color:={0};
        t1.color:={1};
        t1.mode:=bmg.xor;
        l .mode:=bmg.xor;  u:=w;
        crs.monitor;
        t1.clip.x:=crs.x-w^.x-t1.zX;
        t1.clip.y:=crs.y-w^.y-t1.zY;
        t1.clip.w:=w^.w DIV 8;
        t1.clip.h:=w^.h DIV 8;

        t1.mode:=bmg.rep;
        l .color:=w^.fore;
        l.clip.w:=w^.w DIV 8;
        l.clip.h:=w^.h DIV 8;
        t1.color:=w^.fore;
        bmg.cross(t1.clip,t1.clip,w^.inner.clip);
        bmg.cross(l .clip,l .clip,w^.inner.clip);
        wnd.bblt(w,t1,t1.clip.x,t1.clip.y,w,l,l.clip);
        s:=cpd.state^.keys;
      END
    ELSIF s*{0}#{} THEN
      activate(w,x,y)
    END
  END
END monitor;

PROCEDURE p(VAR w: wnd.WINDOW; txt: ARRAY OF CHAR; x,y: INTEGER);
BEGIN
  wnd.new(w);                ASSERT(wnd.done,wnd.error);
  wnd.move(w,x,y);           ASSERT(wnd.done,wnd.error);
  wnd.resize(w,wnd.lenght(fnt.font,"%s",txt)+4,fnt.font^.H+2);
  ASSERT(wnd.done,wnd.error);
  wnd.move(w,x,y);                              ASSERT(wnd.done,wnd.error);
  wnd.mask(w,{0..2},{3});                       ASSERT(wnd.done,wnd.error);
  fill(w);
  wnd.print(w,w^.inner,2,1,fnt.font,"%s",txt);  ASSERT(wnd.done,wnd.error);
  wnd.open(w);                                  ASSERT(wnd.done,wnd.error);
END p;

VAR f: bio.FILE;
    i: INTEGER;

BEGIN
  FOR i:=0 TO HIGH(rand) DO rnd.random(rand[i]) END; rx:=0;
  scr.loophole(scr.bitmap,B);
  T.mask:={0..3};  T.color:={0..3};
  T.back:={};
  T.mode:=bmg.rep;
  T.clip.x:=0;
  T.clip.y:=0;
  T.clip.w:=480;
  T.clip.h:=360;
  T.zX:=0;
  T.zY:=0;

  bio.open(f,"PAPER.pcx",'r');    ASSERT(bio.done,bio.error);
  PCX.read(f,_paper,header);      ASSERT(PCX.done,PCX.error);
  bio.close(f);                   ASSERT(bio.done,bio.error);
  unpack;
(*
  wnd.background(ground);
*)
  color:=3;

  p(zoom,"zoom",0,340);
  p(open ,"open all",80,340);
  p(cascade,"cascade",160,340);
  p(quit,"quit",240,340);

  monitor;
END dwnd.
