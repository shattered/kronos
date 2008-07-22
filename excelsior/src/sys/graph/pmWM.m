IMPLEMENTATION MODULE pmWM; (*$N- Leo 23-Apr-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  low: lowLevel;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  fnt: Fonts;
IMPORT  wnd: pmWnd;
IMPORT  crs: pmCrs;
IMPORT  scr: Screen;
IMPORT  bmg: BMG;
IMPORT  cpd: CPD;
IMPORT  key: Keyboard;
IMPORT  mem: Heap;
FROM SYSTEM  IMPORT ADR;

VAR bug: CHAR;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

CONST
  N    = 13;
  inrL =  4; inrR =   4;  minW = N+2;
  inrD =  4; inrU = N+2;  minH = N+2;

VAR MAGIC: INTEGER;  CONST _MAGIC = 574D57h;

TYPE
  WORD    = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

  BUTTON  = POINTER TO btnDSC;

  wmWND   = POINTER TO wmDSC;

  wmDSC   = RECORD
              magic: INTEGER;
              board: wnd.PAINT;
              cntrl: BOOLEAN;
              butts: BUTTON;
            END;

  btnDSC  = RECORD
              next  : BUTTON;
              no    : INTEGER;
              x,y   : INTEGER;
              w,h   : INTEGER;
              fore  : BITSET;
              press : BITSET;
              back  : BITSET;
              image : ADDRESS;
              onoff : BOOLEAN;
              corner: INTEGER;
            END;

VAR B: bmg.BITMAP;
    S: bmg.BLOCK;
    T: bmg.TOOL;
    R: BITSET;  (* right key on mouse *)

PROCEDURE wnd_error; BEGIN done:=FALSE; error:=wnd.error      END wnd_error;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error      END mem_error;
PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc   END bad_desc;
PROCEDURE unsuit;    BEGIN done:=FALSE; error:=err.unsuitable END unsuit;

PROCEDURE _inrect(x,y,w,h: INTEGER): BOOLEAN;
BEGIN
  RETURN (x>=0) & (x<w) & (y>=0) & (y<h)
END _inrect;

PROCEDURE boarder(v: WINDOW; cx,cy,cw,ch: INTEGER); FORWARD;

PROCEDURE new(VAR v: WINDOW);
  VAR wm: wmWND;
BEGIN
  NEW(wm);
  IF NOT mem.done THEN mem_error; RETURN END;
  wnd.new(v);
  IF NOT wnd.done THEN wnd_error; DISPOSE(wm); RETURN END;
  wnd.resize(v,minW,minH);
  IF NOT wnd.done THEN wnd_error; DISPOSE(wm); dispose(v); RETURN END;
  wnd.inner(v,inrL,inrD,v^.w-inrL-inrR,v^.h-inrU-inrD);
  IF NOT wnd.done THEN wnd_error; DISPOSE(wm); dispose(v); RETURN END;
  wm^.cntrl:=TRUE;
  wm^.butts:=NIL;
  wm^.board:=v^.board;
  wnd.boarder(v,boarder);
  wnd.manager(v,wm);
  wm^.magic:=MAGIC;
  done:=TRUE
END new;

PROCEDURE dispose(VAR v: WINDOW);
  VAR wm: wmWND;
BEGIN
  IF v=NIL THEN RETURN END;
  IF active=v THEN active:=NIL; abutton:=-1 END;
  wm:=v^.mgr;
  wnd.dispose(v);
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN RETURN END;
  DISPOSE(wm)
END dispose;

PROCEDURE disable(v: WINDOW);
  VAR wm: wmWND;
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN unsuit; RETURN END;
  wm^.cntrl:=FALSE; done:=TRUE
END disable;

PROCEDURE enable(v: WINDOW);
  VAR wm: wmWND;
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN unsuit; RETURN END;
  wm^.cntrl:=TRUE; done:=TRUE
END enable;

PROCEDURE showbutton(t: bmg.TOOL; x,y,w,h: INTEGER; onoff: BOOLEAN);
  VAR c0,c1: BITSET;
BEGIN
  IF onoff THEN c0:=bright; c1:=shadow ELSE c0:=shadow; c1:=bright END;
  t.mode :=bmg.rep;
  t.color:=c0;
  bmg.line(B,t,x-1,y,x-1,y+h);
  bmg.line(B,t,x,y+h,x+w-1,y+h);
  t.color:=c1;
  bmg.line(B,t,x-1,y-1,x+w,y-1);
  bmg.line(B,t,x+w,y,x+w,y+h);
END showbutton;

PROCEDURE buttonxy(v: WINDOW; b: BUTTON; VAR x,y: INTEGER);
BEGIN
  CASE b^.corner OF
  |ldc:  x:=b^.x;       y:=b^.y;
  |rdc:  x:=b^.x+v^.w;  y:=b^.y;
  |luc:  x:=b^.x;       y:=b^.y+v^.h;
  |ruc:  x:=b^.x+v^.w;  y:=b^.y+v^.h;
  END;
END buttonxy;

PROCEDURE picto(v: WINDOW; t: bmg.TOOL; b: BUTTON);
  VAR s: bmg.TOOL;         c: bmg.BLOCK;
    bmd: bmg.BITMAP;    _bmd: bmg.BMD;
    x,y: INTEGER;      fg,bg: BITSET;    msk: BITSET;
BEGIN
  buttonxy(v,b,x,y);
  showbutton(t,x,y,b^.w,b^.h,b^.onoff);
  bmd:=ADR(_bmd);
  bmd^.layers[0]:=b^.image;
  bmd^.W:=b^.w;   bmd^.WPL :=(b^.w+31) DIV 32;
  bmd^.H:=b^.h;   bmd^.mask:={0};
  s.color:={0};
  s.zX:=0;  s.clip.x:=0;  s.clip.w:=b^.w;  s.mask:={0};
  s.zY:=0;  s.clip.y:=0;  s.clip.h:=b^.h;  s.back:={ };
  t.mode:=bmg.rep;
  c.x:=x; c.w:=b^.w;
  c.y:=y; c.h:=b^.h;
  bmg.cross(t.clip,c,t.clip);
  msk:=B^.mask;
  IF b^.image=NIL THEN
    t.color:=normal; bmg.rect(B,t,x,y,x+b^.w-1,y+b^.h-1); RETURN
  END;
  t.color:={0};
  IF b^.onoff THEN fg:=b^.fore ELSE fg:=b^.press END;
  bg:=b^.back<<1;
  WHILE msk#{} DO
    t.mask:=t.color;
    CASE INTEGER(fg*{0}+bg*{1}) OF
    |0: t.mode:=bmg.bic;  bmg.rect(B,t,x,y,x+b^.w-1,y+b^.h-1);
    |1: t.mode:=bmg.rep;  bmg.bblt(B,t,x,y,bmd,s,s.clip)
    |2: t.mode:=bmg.rep;  bmg.rect(B,t,x,y,x+b^.w-1,y+b^.h-1);
        t.mode:=bmg.bic;  bmg.bblt(B,t,x,y,bmd,s,s.clip)
    |3: t.mode:=bmg.rep;  bmg.rect(B,t,x,y,x+b^.w-1,y+b^.h-1)
    END;
    msk:=msk-t.color;   t.color:=t.color<<1;
    bg:=(bg-{0})>>1;         fg:=(fg-{0})>>1
  END
END picto;

PROCEDURE showbuttons(v: WINDOW; VAL t: bmg.TOOL);
  VAR b: BUTTON;
     wm: wmWND;
BEGIN
  wm:=v^.mgr;
  b:=wm^.butts;
  WHILE b#NIL DO
    IF (b^.w>0) & (b^.h>0) THEN picto(v,t,b) END;
    b:=b^.next
  END
END showbuttons;

PROCEDURE frames(v: WINDOW; t: bmg.TOOL; x,y,w,h: INTEGER);
BEGIN
  t.color:=bright;
  bmg.line(B,t,0,1,0,v^.h-1);
  bmg.line(B,t,1,v^.h-1,v^.w-2,v^.h-1);
  IF (w>0) & (h>0) THEN
    bmg.line(B,t,x,y-1,x+w,y-1);  bmg.line(B,t,x+w,y-1,x+w,y+h-1)
  END;
  t.color:=shadow;
  bmg.line(B,t,0,0,v^.w-1,0);
  bmg.line(B,t,v^.w-1,0,v^.w-1,v^.h-1);
  IF (w>0) & (h>0) THEN
    bmg.line(B,t,x-1,y-1,x-1,y+h);  bmg.line(B,t,x,y+h,x+w,y+h)
  END
END frames;

PROCEDURE boarder(v: WINDOW; cx,cy,cw,ch: INTEGER);

  VAR t: bmg.TOOL;
    c,b: bmg.BLOCK;
     wm: wmWND;
    x,y: INTEGER;
    w,h: INTEGER;
   mode: BITSET;

  PROCEDURE mainbutton;
    VAR x,y: INTEGER;
  BEGIN
    x:=1; y:=v^.h-1-N;
    showbutton(t,x,y,N,N,TRUE);
    t.mode:=bmg.rep;
    t.color:=black;
    bmg.frame(B,t,x+2,y+2,x+N-3,y+N-3);
    bmg.rect (B,t,x+2,y+N-6,x+N-4,y+N-4);
  END mainbutton;

  PROCEDURE erase;
    VAR bmd: bmg.BITMAP;  e: bmg.TOOL;
  BEGIN
    e:=t;
    bmd:=v^.desc; (* erase v^.bitmap under frame *)
    e.zX:=0; e.zY:=0; e.mode:=bmg.bic; e.mask:=v^.fore; e.color:=v^.fore;
    bmg.rect(bmd,e,x+w,0,v^.w-1,v^.h-1);     (* r *)
    bmg.rect(bmd,e,0,y+h,v^.w-1,v^.h-1);     (* u *)
  END erase;

BEGIN
  IF (v=NIL) OR (v^.mgr=NIL) OR v^.closed THEN RETURN END;
  wm:=v^.mgr;
  IF wm^.magic#MAGIC THEN RETURN END;

  mode:=v^.mode;
  wnd.mode(v,wnd.scr+wnd.deep);

  t:=v^.full;
  c.x:=cx; c.y:=cy; c.w:=cw; c.h:=ch;
  bmg.cross(t.clip,c,t.clip);
  t.mode:=bmg.rep;  t.zX:=v^.x;
  t.mask:=B^.mask;  t.zY:=v^.y;

  x:=v^.inner.zX;   w:=v^.inner.clip.w;
  y:=v^.inner.zY;   h:=v^.inner.clip.h;

  b.x:=-t.zX;  b.w:=S.w;
  b.y:=-t.zY;  b.h:=S.h;
  bmg.cross(t.clip,b,t.clip);

  t.color:=normal;
  bmg.rect(B,t,1,1,v^.w-2,y-2);          (* d *)
  bmg.rect(B,t,1,y-1,x-2,y+h);           (* l *)
  bmg.rect(B,t,x+w+1,y-1,v^.w-2,y+h);    (* r *)
  bmg.rect(B,t,1,y+h+1,v^.w-2,v^.h-2);   (* u *)
  frames(v,t,x,y,w,h);
  IF  v^.image & (v^.desc#NIL)   THEN erase      END;
  IF (v^.h>=minH) & (v^.w>=minW) THEN mainbutton END;

  wnd.mode(v,mode);
  DEC(c.x,v^.inner.zX);
  DEC(c.y,v^.inner.zY);
  bmg.cross(c,v^.inner.clip,c);
  INC(c.x,v^.inner.zX);
  INC(c.y,v^.inner.zY);
  IF v^.image THEN wm^.board(v,c.x,c.y,c.w,c.h)
  ELSE             v ^.paint(v,c.x,c.y,c.w,c.h)
  END;

  wnd.mode(v,wnd.scr+wnd.deep);  showbuttons(v,t);  wnd.mode(v,mode)
END boarder;

PROCEDURE mainboarder(m: WINDOW; cx,cy,cw,ch: INTEGER);

  VAR t: bmg.TOOL;
    b,c: bmg.BLOCK;

  PROCEDURE sh(n: INTEGER; ch: CHAR; onoff: BOOLEAN);
    VAR x,y: INTEGER;
  BEGIN
    x:=1+(N+2)*n; y:=1;
    showbutton(t,x,y,N,N,onoff);
    t.mode:=bmg.rep;  t.color:=black;  t.back:=normal;
    bmg.writech(B,t,x+1,y+1,ssfont,ch)
  END sh;

BEGIN
  t:=m^.full;
  c.x:=cx; c.y:=cy; c.w:=cw; c.h:=ch;
  bmg.cross(t.clip,c,t.clip);
  t.mode:=bmg.rep;  t.zX:=m^.x;
  t.mask:=B^.mask;  t.zY:=m^.y;

  b.x:=-t.zX;  b.w:=S.w;
  b.y:=-t.zY;  b.h:=S.h;
  bmg.cross(t.clip,b,t.clip);
  t.color:=normal;
  bmg.rect(B,t,1,1,m^.w-2,m^.h-2);
  frames(m,t,0,0,0,0);
  sh(0,ssontop ,TRUE);
  sh(1,ssmove  ,TRUE);
  sh(2,ssresize,TRUE);
  sh(3,ssonbot ,TRUE);
  sh(4,ssclose ,TRUE);
END mainboarder;

PROCEDURE mainmonitor(v: WINDOW);

  VAR m: WINDOW;     wm: wmWND;
      i: INTEGER;   cha: BITSET;
      x: INTEGER;   key: BITSET;

  PROCEDURE btn(): INTEGER;
    VAR i,y: INTEGER;
  BEGIN
    y:=crs.y-m^.y;
    FOR i:=0 TO 4 DO
      IF _inrect(crs.x-(m^.x+(N+2)*i),y,N+1,N+1) THEN RETURN i END
    END;
    RETURN -1
  END btn;

  PROCEDURE waitrelease(s: BITSET);
  BEGIN
    REPEAT crs.monitor UNTIL cpd.state^.keys*s={}
  END waitrelease;

  PROCEDURE move;
    VAR xt: bmg.TOOL;
       off: BOOLEAN;
       x,y: INTEGER;
    PROCEDURE f;
    BEGIN bmg.frame(B,xt,crs.x-1,crs.y-1,crs.x+v^.w,crs.y-v^.h) END f;
  BEGIN
    xt:=T;  xt.color:=crs.color;  xt.mode:=bmg.xor;
    crs.move(v^.x,v^.y+v^.h);
    crs.toggle(TRUE); f;
    REPEAT
      crs.read(x,y); f;
      IF y<N     THEN y:=N     END;
      IF x>S.w-N THEN x:=S.w-N END;
      crs.move(x,y); f;
      off:=(cpd.state^.keys*R#{})
    UNTIL off OR (cpd.state^.keys*{0}#{});  f;
    crs.toggle(FALSE);
    waitrelease(cpd.state^.keys*({0}+R));
    IF NOT off THEN
      moved:=TRUE;  moveX:=x;  moveY:=y-v^.h;
      IF wm^.cntrl THEN wnd.move(v,x,y-v^.h) END
    END
  END move;

  PROCEDURE resize;
    VAR xt: bmg.TOOL;
       off: BOOLEAN;
       x,y: INTEGER;
    PROCEDURE f;
    BEGIN bmg.frame(B,xt,v^.x-1,v^.y-1,crs.x-1,crs.y-1) END f;
  BEGIN
    xt:=T;  xt.color:=crs.color;  xt.mode:=bmg.xor;
    crs.move(v^.x+v^.w+1,v^.y+v^.h+1);
    crs.toggle(TRUE); f;
    REPEAT
      crs.read(x,y); f;
      IF x<v^.x+minW THEN x:=v^.x+minW END;
      IF y<v^.y+minH THEN y:=v^.y+minH END;
      crs.move(x,y); f;
      off:=(cpd.state^.keys*R#{})
    UNTIL off OR (cpd.state^.keys*{0}#{});  f;
    crs.toggle(FALSE);
    waitrelease(cpd.state^.keys*({0}+R));
    IF NOT off THEN
      resized:=TRUE;  resizeW:=x-v^.x;  resizeH:=y-v^.y;
      IF wm^.cntrl THEN wnd.resize(v,resizeW,resizeH) END
    END
  END resize;

  PROCEDURE close;
  BEGIN
    crs.toggle(FALSE);  wnd.dispose(m)
  END close;

BEGIN
  wm:=v^.mgr;
  wnd.new(m);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  wnd.image (m,FALSE);
  wnd.mask  (m,B^.mask,{});
  wnd.resize(m,(N+2)*5,N+2);
  x:=v^.x;
  IF x<0        THEN x:=0        END;
  IF x+m^.w>S.w THEN x:=S.w-m^.w END;
  wnd.move(m,x,v^.y+v^.h-N-2);
  wnd.mode(m,wnd.deep+wnd.scr);
  wnd.painter(m,mainboarder);
  wnd.boarder(m,mainboarder);
  wnd.ontop(m);
  crs.toggle(FALSE);
  wnd.open(m);
  crs.move(m^.x+3+N DIV 2,m^.y+3+N DIV 2);
  crs.toggle(TRUE);
  LOOP
    cha:=cpd.state^.keys;
    crs.monitor;
    key:=cpd.state^.keys;
    cha:=cha/key;
    IF cha*key* R #{} THEN waitrelease(R); EXIT END;
    IF cha*key*{0}#{} THEN
      i:=btn();
      IF i>=0 THEN waitrelease({0});
        CASE i OF
        |0: close; wnd.ontop(v);    RETURN
        |1: close; move;            RETURN
        |2: close; resize;          RETURN
        |3: close; wnd.onbottom(v); RETURN
        |4: close; closed:=TRUE;
                   IF wm^.cntrl THEN wnd.close(v) END;
                                    RETURN
        END
      END
    END
  END;
  close
END mainmonitor;

PROCEDURE monitor;

  PROCEDURE into(v: WINDOW): BOOLEAN;
    VAR x,y: INTEGER;
  BEGIN
    IF (v^.inner.clip.h<=0) OR (v^.inner.clip.w<=0) THEN RETURN FALSE END;
    x:=crs.x-v^.x;
    y:=crs.y-v^.y;
    RETURN _inrect(x-v^.inner.zX,y-v^.inner.zY,v^.inner.clip.w,v^.inner.clip.h)
  END into;

  PROCEDURE inmain(v: WINDOW): BOOLEAN;
    VAR x,y: INTEGER;
  BEGIN
    x:=crs.x-v^.x;
    y:=crs.y-v^.y;
    RETURN _inrect(x-1,y-(v^.h-1-N),N,N)
  END inmain;

  PROCEDURE onboard(v: WINDOW);
    VAR wm: wmWND;  bx,by,x,y: INTEGER;  b: BUTTON;
  BEGIN
    wm:=v^.mgr;
    IF (wm=NIL) OR (wm^.magic#MAGIC) THEN RETURN END;
    x:=crs.x-v^.x;
    y:=crs.y-v^.y;
    b:=wm^.butts;
    WHILE b#NIL DO
      buttonxy(v,b,bx,by);
      IF _inrect(x-bx,y-by,b^.w,b^.h) THEN
        crs.toggle(FALSE); abutton:=b^.no; toggle(v,b^.no,b^.onoff)
      END;
      b:=b^.next
    END
  END onboard;

  VAR v: WINDOW;  wm: wmWND;  cha,keys: BITSET;

BEGIN
  active :=NIL;
  abutton:=-1;
  closed :=FALSE;
  resized:=FALSE;
  moved  :=FALSE;
  moveX:=0;  resizeW:=0;
  moveY:=0;  resizeH:=0;
  IF NOT crs.on THEN crs.toggle(TRUE) END;
  LOOP
    cha:=cpd.state^.keys;
    crs.monitor;
    keys:=cpd.state^.keys;
    cha :=cha/keys;
    IF cha#{} THEN
      v:=wnd.locate(crs.x,crs.y);  active:=v;
      IF v#NIL THEN wm:=v^.mgr ELSE wm:=NIL END;
      IF (cha*keys*{0}#{}) & (wm#NIL) & (wm^.magic=MAGIC) THEN
        IF inmain(v) THEN mainmonitor(v);    RETURN END;
        onboard(v);
        IF into  (v) THEN crs.toggle(FALSE); RETURN END;
        RETURN
      ELSE
        RETURN
      END
    END;
    IF key.ready()>0 THEN RETURN END
  END
END monitor;

PROCEDURE disposebutton(VAR b: BUTTON);
BEGIN
  mem.deallocate(b^.image,b^.h*((b^.w+31) DIV 32)); b^.next:=NIL; DISPOSE(b)
END disposebutton;

PROCEDURE tie(wm: wmWND; b: BUTTON);
  VAR k,p: BUTTON;
BEGIN
  k:=wm^.butts; p:=NIL;
  WHILE (k#NIL) & (k^.no#b^.no) DO p:=k; k:=k^.next END;
  IF k=NIL THEN
    b^.next:=wm^.butts; wm^.butts:=b
  ELSE
    b^.next:=k^.next;
    disposebutton(k);
    IF p=NIL THEN wm^.butts:=b ELSE p^.next:=b END
  END
END tie;

PROCEDURE refreshbutton(v: WINDOW; b: BUTTON);
  VAR x,y: INTEGER;
BEGIN
  IF v^.closed THEN RETURN END;
  crs.toggle(FALSE);
  buttonxy(v,b,x,y);
  wnd.refreshboard(v,x-2,y-2,b^.w+4,b^.h+4)
END refreshbutton;

PROCEDURE button(v: WINDOW; no,c,x,y,w,h: INTEGER); (* inner sizes *)
  VAR i: INTEGER;
      b: BUTTON;
     wm: wmWND;
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN unsuit; RETURN END;
  done:=TRUE;
  NEW(b);
  IF NOT mem.done THEN mem_error; RETURN END;
  b^.x:=x;   b^.w:=w;  b^.image:=NIL;   b^.no  :=no;    b^.fore:=black;
  b^.y:=y;   b^.h:=h;  b^.onoff:=TRUE;  b^.next:=NIL;   b^.back:=normal;
  b^.corner:=c MOD 4;                                  b^.press:=shadow;
  i:=((w+31) DIV 32)*h;
  mem.allocate(b^.image,i);
  IF NOT mem.done THEN mem_error; disposebutton(b); RETURN END;
  low._zero(b^.image,i);
  tie(wm,b);
  refreshbutton(v,b)
END button;

PROCEDURE buttoncolors(v: WINDOW; no: INTEGER; fore,press,back: BITSET);
  VAR k: BUTTON;
     wm: wmWND;
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN unsuit; RETURN END;
  done:=TRUE;
  k:=wm^.butts;
  WHILE (k#NIL) & (k^.no#no) DO k:=k^.next END;
  IF k=NIL THEN done:=FALSE; error:=err.no_entry; RETURN END;
  k^.fore :=fore*B^.mask;
  k^.back :=back*B^.mask;
  k^.press:=press*B^.mask;
  refreshbutton(v,k)
END buttoncolors;

PROCEDURE pressed(v: WINDOW; no: INTEGER): BOOLEAN;
  VAR k: BUTTON;
     wm: wmWND;
BEGIN
  IF v=NIL THEN RETURN FALSE END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN RETURN FALSE END;
  done:=TRUE;
  k:=wm^.butts;
  WHILE (k#NIL) & (k^.no#no) DO k:=k^.next END;
  IF k=NIL THEN RETURN FALSE END;
  RETURN NOT k^.onoff
END pressed;

PROCEDURE toggle(v: WINDOW; no: INTEGER; prs: BOOLEAN);
  VAR k: BUTTON;
     wm: wmWND;
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN unsuit; RETURN END;
  done:=TRUE;
  k:=wm^.butts;
  WHILE (k#NIL) & (k^.no#no) DO k:=k^.next END;
  IF k=NIL THEN done:=FALSE; error:=err.no_entry; RETURN END;
  IF k^.onoff=NOT prs THEN RETURN END;
  k^.onoff:=NOT prs;
  refreshbutton(v,k)
END toggle;

PROCEDURE print(v: WINDOW;     no: INTEGER;           font: fnt.FONT;
                      VAL  format: ARRAY OF CHAR; SEQ args: WORD);
  VAR k: BUTTON;
      t: bmg.TOOL;
     wm: wmWND;
    x,y: INTEGER;
    bmd: bmg.BITMAP;
   _bmd: bmg.BMD;
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  wm:=v^.mgr;
  IF (wm=NIL) OR (wm^.magic#MAGIC) THEN unsuit; RETURN END;
  done:=TRUE;
  k:=wm^.butts;
  WHILE (k#NIL) & (k^.no#no) DO k:=k^.next END;
  IF k=NIL THEN done:=FALSE; error:=err.no_entry; RETURN END;
  IF (k^.w=0) OR (k^.h=0) THEN RETURN END;
  bmd:=ADR(_bmd);
  bmd^.layers[0]:=k^.image;
  bmd^.W:=k^.w;   bmd^.WPL :=(k^.w+31) DIV 32;
  bmd^.H:=k^.h;   bmd^.mask:={0};
  low._zero(k^.image,bmd^.WPL*bmd^.H);
  t.mode :=bmg.rep;    t.zX:=0;  t.clip.x:=0;  t.clip.w:=k^.w;  t.mask:={0};
  t.color:={0};        t.zY:=0;  t.clip.y:=0;  t.clip.h:=k^.h;  t.back:={ };
  x:=bmg.lenght(font,format,args);
  IF x>k^.w THEN x:=0 ELSE x:=(k^.w-x) DIV 2 END;
  y:=font^.H;
  IF y>k^.h THEN y:=0 ELSE y:=(k^.h-y) DIV 2 END;
  bmg.print(bmd,t,x,y,font,format,args);
  refreshbutton(v,k)
END print;


PROCEDURE ssmake;

  PROCEDURE put(ch: CHAR; VAL contence: ARRAY OF BITSET);
  BEGIN
    low.move(ssfont^.BASE+ORD(ch)*ssfont^.H,ADR(contence),ssfont^.H)
  END put;

  CONST
   _ssmove    = ARRAY OF BITSET {
    {             04   ,06   ,08   ,10 },
    {                                  },
    {             04               ,10 },
    { 00,01,02,03   ,05,06,07          },
    { 00         ,04      ,07      ,10 },
    { 00                  ,07          },
    { 00         ,04   ,06   ,08   ,10 },
    { 00                  ,07          },
    { 00                  ,07          },
    { 00                  ,07          },
    { 00,01,02,03,04,05,06,07          } };

   _ssresize  = ARRAY OF BITSET {
    { 00   ,02   ,04   ,06   ,08   ,10 },
    {                                  },
    { 00                           ,10 },
    {                                  },
    { 00                           ,10 },
    { 00,01,02,03,04,05                },
    { 00            ,05            ,10 },
    { 00            ,05                },
    { 00            ,05            ,10 },
    { 00            ,05                },
    { 00,01,02,03,04,05,06   ,08   ,10 } };

   _sscascade = ARRAY OF BITSET {
    { 00,01,02,03,04,05,06             },
    { 00,               06             },
    { 00,   02,03,04,05,06,07,08       },
    { 00,   02,               08       },
    { 00,   02,               08       },
    { 00,01,02,   04,05,06,07,08,09,10 },
    {       02,   04,               10 },
    {       02,03,04,               10 },
    {             04,               10 },
    {             04,               10 },
    {             04,05,06,07,08,09,10 } };


   _ssgrid = ARRAY OF BITSET {
    {    01,      04,      07,      10 },
    {                                  },
    {                                  },
    {    01,      04,      07,      10 },
    {                                  },
    {                                  },
    {    01,      04,      07,      10 },
    {                                  },
    {                                  },
    {    01,      04,      07,      10 },
    {                                  } };


   (*
   _ssfire = ARRAY OF BITSET {
    {                            09    },
    {             04,         08,09    },
    {             04,      07,08,  10  },
    {          03,04,         08,  10  },
    {          03,04,      07          },
    {       02,   04,      07          },
    {       02,   04,   06             },
    {    01,      04,   06             },
    {    01,      04,05                },
    { 00,         04,05                },
    { 00,         04                   } };
   *)

   _ssfire = ARRAY OF BITSET {
    {          03,            08,09,10 },
    { 00,      03,04,      07,   09    },
    { 00,01,   03,04,   06,   08       },
    { 00,   02,03,   05,06,   08       },
    { 00,01,02,03,04,05,06,07,08       },
    {                      07,08,09    },
    { 00,01,02,03,04,05,   07,   09,10 },
    { 00,            05,   07,08,09,10 },
    { 00,            05,   07,      10 },
    { 00,            05,   07,08       },
    { 00,01,02,03,04,05,   07,   09    } };

   _sstool = ARRAY OF BITSET {
    {                   06,07,08       },
    {                05,06             },
    {             04,05,            10 },
    {             04,05,            10 },
    {                05,06,      09,10 },
    {             04,05,06,07,08,09    },
    {          03,04,05,   07,08       },
    {       02,03,04                   },
    {    01,02,03                      },
    { 00,01,02                         },
    {    01                            } };

   _sspan  = ARRAY OF BITSET {
    { 00,01,02,03,04,05,06,07,08,09,10 },
    { 00,                           10 },
    { 00,               06,07,08,   10 },
    { 00,                  07,08,   10 },
    { 00,               06,   08,   10 },
    { 00,   02,03,04,05,            10 },
    { 00,   02,      05,            10 },
    { 00,   02,      05,            10 },
    { 00,   02,03,04,05,            10 },
    { 00,                           10 },
    { 00,01,02,03,04,05,06,07,08,09,10 } };

   _sseye = ARRAY OF BITSET {
    {                05                },
    {    01,         05,         09    },
    {       02,      05,      08       },
    { 00,                           10 },
    {       02,03,   05,   07          },
    {    01,      04,05,06,   08       },
    { 00,      03,04,   06,07,   09    },
    { 00,      03,      06,07,      10 },
    {    01,   03,04,05,06,07,   09    },
    {       02,   04,05,06,   08       },
    {          03,   05,   07          } };

   _sszoom = ARRAY OF BITSET {
    {                   06             },
    {             04,05,   07,08       },
    {          03,               09    },
    {          03,   05,06,      09    },
    {       02,      05,   07,      10 },
    {          03,   05,         09    },
    {          03,               09    },
    {       02,03,04,05,   07,08       },
    {    01,02,03,      06             },
    { 00,01,02                         },
    {    01                            } };

   _ssclose   = ARRAY OF BITSET {
    {                                  },
    {       02               ,08       },
    {    01,02,03         ,07,08,09    },
    {       02,03,04   ,06,07,08       },
    {          03,04,05,06,07          },
    {             04,05,06             },
    {          03,04,05,06,07          },
    {       02,03,04   ,06,07,08       },
    {    01,02,03         ,07,08,09    },
    {       02               ,08       },
    {                                  } };

   _ssicon = ARRAY OF BITSET {
    {          03,04,05,06,07          },
    {       02,               08       },
    {    01,                     09    },
    {    01,      04,05,06,      09    },
    {       02,03,         07,08       },
    {          03,         07          },
    {          04,         07          },
    {             04,   06             },
    {       02,03,         07,08       },
    {    01                     ,09    },
    {    01                     ,09    },
    {    01,02,03,04,05,06,07,08,09    } };


   _ssontop   = ARRAY OF BITSET {
    {             04,05                },
    {          03,04,05,06             },
    {       02,03,04,05,06,07          },
    {             04,05                },
    {             04,05                },
    {    01,02,03,04,05,06,07,08,09    },
    {    01      ,04,05         ,09    },
    {    01      ,04,05         ,09    },
    {    01                     ,09    },
    {    01                     ,09    },
    {    01,02,03,04,05,06,07,08,09    } };

   _ssonbot   = ARRAY OF BITSET {
    {    01,02,03,04,05,06,07,08,09    },
    {    01                     ,09    },
    {    01                     ,09    },
    {    01      ,04,05         ,09    },
    {    01      ,04,05         ,09    },
    {    01,02,03,04,05,06,07,08,09    },
    {             04,05                },
    {             04,05                },
    {       02,03,04,05,06,07          },
    {          03,04,05,06             },
    {             04,05                } };

   _sszoomin  = ARRAY OF BITSET {
    {    01                     ,09    },
    { 00,01,02   ,04   ,06   ,08,09,10 },
    {    01,02,03,04   ,06,07,08,09    },
    {       02,03,04   ,06,07,08       },
    {    01,02,03,04   ,06,07,08,09    },
    {                                  },
    {    01,02,03,04   ,06,07,08,09    },
    {       02,03,04   ,06,07,08       },
    {    01,02,03,04   ,06,07,08,09    },
    { 00,01,02   ,04   ,06   ,08,09,10 },
    {    01                     ,09    } };

   _sszoomout = ARRAY OF BITSET {
    { 00,01,02,03         ,07,08,09,10 },
    { 00,01,02               ,08,09,10 },
    { 00,01,02,03         ,07,08,09,10 },
    { 00   ,02,03,04   ,06,07,08   ,10 },
    {          03         ,07          },
    {                                  },
    {          03         ,07          },
    { 00   ,02,03,04   ,06,07,08   ,10 },
    { 00,01,02,03         ,07,08,09,10 },
    { 00,01,02               ,08,09,10 },
    { 00,01,02,03         ,07,08,09,10 } };

   _ssleft    = ARRAY OF BITSET {
    {                      07          },
    {                   06,07          },
    {                05,06,07          },
    {             04,05,06,07          },
    {          03,04,05,06,07          },
    {       02,03,04,05,06,07          },
    {          03,04,05,06,07          },
    {             04,05,06,07          },
    {                05,06,07          },
    {                   06,07          },
    {                      07          } };

   _ssright   = ARRAY OF BITSET {
    {          03                      },
    {          03,04                   },
    {          03,04,05                },
    {          03,04,05,06             },
    {          03,04,05,06,07          },
    {          03,04,05,06,07,08       },
    {          03,04,05,06,07          },
    {          03,04,05,06             },
    {          03,04,05                },
    {          03,04                   },
    {          03                      } };

   _ssup      = ARRAY OF BITSET {
    {                                  },
    {                                  },
    {                05                },
    {             04,05,06             },
    {          03,04,05,06,07          },
    {       02,03,04,05,06,07,08       },
    {    01,02,03,04,05,06,07,08,09    },
    { 00,01,02,03,04,05,06,07,08,09,10 },
    {                                  },
    {                                  },
    {                                  } };

   _ssdw      = ARRAY OF BITSET {
    {                                  },
    {                                  },
    {                                  },
    { 00,01,02,03,04,05,06,07,08,09,10 },
    {    01,02,03,04,05,06,07,08,09    },
    {       02,03,04,05,06,07,08       },
    {          03,04,05,06,07          },
    {             04,05,06             },
    {                05                },
    {                                  },
    {                                  } };

BEGIN
  fnt.new(ssfont,11,11,0c,30c,{});
  IF NOT fnt.done THEN HALT(fnt.error) END;
  put(ssmove   ,_ssmove    );
  put(ssresize ,_ssresize  );
  put(ssclose  ,_ssclose   );
  put(ssontop  ,_ssontop   );
  put(ssonbot  ,_ssonbot   );
  put(sszoomin ,_sszoomin  );
  put(sszoomout,_sszoomout );
  put(ssleft   ,_ssleft    );
  put(ssright  ,_ssright   );
  put(ssup     ,_ssup      );
  put(ssdw     ,_ssdw      );
  put(sspan    ,_sspan     );
  put(sseye    ,_sseye     );
  put(sszoom   ,_sszoom    );
  put(ssgrid   ,_ssgrid    );
  put(ssfire   ,_ssfire    );
  put(sscascade,_sscascade );
  put(ssicon   ,_ssicon    );
  put(sstool   ,_sstool    );
END ssmake;

BEGIN
  MAGIC:=_MAGIC; done:=TRUE; error:=0;
  scr.loophole(scr.bitmap,B);
  IF NOT scr.done THEN HALT(scr.error) END;
  S.x:=0;  S.w:=scr.state^.W;
  S.y:=0;  S.h:=scr.state^.H;
  T.mask:=B^.mask;  T.zX:=0;  T.color:=T.mask;
  T.mode:=bmg.rep;  T.zY:=0;  T.back :={};
  T.clip:=S;
  closed :=FALSE;
  resized:=FALSE;
  moved  :=FALSE;
  moveX:=0;  resizeW:=0;
  moveY:=0;  resizeH:=0;
  ssmake;
  cpd.nop;
  IF cpd.state^.nokeys<=1 THEN R:={} ELSE R:={cpd.state^.nokeys-1} END;
  black :={ };  normal:={0..2};
  shadow:={3};  bright:={0..3}
END pmWM.
