IMPLEMENTATION MODULE pmWnd; (* Leo 09-Aug-91. (c) KRONOS *)

IMPORT       SYSTEM;

IMPORT  low: lowLevel;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;

IMPORT  bmg: BMG;
IMPORT  mem: Heap;
IMPORT  psc: Screen;
IMPORT  fmt: Formats;

FROM SYSTEM  IMPORT ADR;


IMPORT  fnt: Fonts; (* must be changed to defFont after debug! *)

IMPORT key: Keyboard; VAR bug: CHAR;    -- must be discarded after debug;
                          BUG: BOOLEAN; -- must be discarded after debug;

PROCEDURE debug(on: BOOLEAN); BEGIN BUG:=on END debug;


VAR MAGIC: INTEGER;             CONST _MAGIC = 444E57h;

TYPE

  BITMAP  = bmg.BITMAP;

  WORD    = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

  REQPTR  = POINTER TO REQUEST;

  OBJECTS = POINTER TO OBJECT;
  OBJECT  = RECORD
              next: OBJECTS;
              name: ARRAY [0..15] OF CHAR;
              val : SYSTEM.WORD;
            END;

  Window  = POINTER TO WnDesc;

  WnDesc  = RECORD
              out    : WNDESC;
              d      : WNDESC;
              magic  : INTEGER;
              up,dw  : Window;
              actor  : PAINT;
              state  : BITSET;
              req    : REQPTR;
              obj    : OBJECTS;
              iter   : OBJECTS;
              iterp  : OBJECTS;
              start  : Window;
              stop   : Window;
              desc   : BITMAP;
            END;

CONST (* state *)
  ground    = {0};
  callimage = {1};
  store     = {2};
  inrefresh = {3};

TYPE
  REQUEST = RECORD
              op   : INTEGER;
              tool : POINTER TO TOOL;
              stool: POINTER TO TOOL;
              block: POINTER TO BLOCK;
              x0,y0: INTEGER;
              x1,y1: INTEGER;
              x2,y2: INTEGER;
              r    : INTEGER;
              font : fnt.FONT;
              arg  : ADDRESS;
            END;

  TYPE LINE = POINTER TO ARRAY [0..4096 DIV 32-1] OF BITSET;

VAR
  DESK: Window;
  T   : TOOL;
 _fill: LINE;
 _zero: LINE;

PROCEDURE bmv(des,des_ofs,sou,sou_ofs,nobits: ADDRESS);
CODE cod.bmv END bmv;

PROCEDURE _move(a,b,c: ADDRESS); CODE cod.move END _move;

PROCEDURE min(x,y: INTEGER): INTEGER;
BEGIN
  IF x<y THEN RETURN x ELSE RETURN y END
END min;

PROCEDURE max(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN x ELSE RETURN y END
END max;

---------------------------- errors ----------------------------
                            --------

PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm  END bad_parm;
PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc  END bad_desc;
PROCEDURE too_large; BEGIN done:=FALSE; error:=err.too_large END too_large;

PROCEDURE allocate(VAR a: ADDRESS; size: INTEGER);
BEGIN
  mem.allocate(a,size); done:=mem.done;
  IF NOT done THEN error:=mem.error END
END allocate;

PROCEDURE reallocate(VAR a: ADDRESS; VAR h: INTEGER; l,bsize: INTEGER);
BEGIN
  mem.reallocate(a,h,l,bsize); done:=mem.done;
  IF NOT done THEN error:=mem.error END
END reallocate;

WITH STORAGE (NEW: allocate; DISPOSE: mem.deallocate; RESIZE: reallocate);

----------------------------------------------------------------

PROCEDURE untie(w: Window);
  VAR t,b,d: Window;
BEGIN
  ASSERT(w^.up#NIL);
  ASSERT(w^.dw#NIL);
  d:=Window(w^.d.desk);
  IF w^.up=w THEN
    d^.d.top:=NIL; d^.d.bottom:=NIL
  ELSE
    t:=Window(d^.d.top);
    b:=Window(d^.d.bottom);
    IF t=w THEN d^.d.top   :=WINDOW(t^.dw) END;
    IF b=w THEN d^.d.bottom:=WINDOW(b^.up) END;
    w^.up^.dw:=w^.dw;
    w^.dw^.up:=w^.up
  END;
  d^.out:=d^.d;
  w^.up:=NIL;
  w^.dw:=NIL;
END untie;

(* tie "w" over "u" *)
PROCEDURE tie(w,u: Window; ontop: BOOLEAN);
  VAR d: Window;
BEGIN
  ASSERT(w^.up=NIL);
  ASSERT(w^.dw=NIL);
  d:=Window(w^.d.desk);
  IF u=NIL THEN
    ASSERT(d^.d.top   =NIL);
    ASSERT(d^.d.bottom=NIL);
    d^.d.top:=WINDOW(w);  d^.d.bottom:=WINDOW(w);
    w^.up:=w;  w^.dw:=w;
    d^.out:=d^.d
  ELSE
    ASSERT(d=Window(u^.d.desk));
    ASSERT(d^.d.top   #NIL);
    ASSERT(d^.d.bottom#NIL);
    w^.up:=u^.up;
    w^.dw:=u;
    w^.up^.dw:=w;
    w^.dw^.up:=w;
    IF u=Window(d^.d.top) THEN
      IF ontop THEN d^.d.top:=WINDOW(w) ELSE d^.d.bottom:=WINDOW(w) END;
      d^.out:=d^.d
    END
  END
END tie;

----------------------------------------------------------------

PROCEDURE disposebmd(w: Window);
  VAR i: INTEGER;
    bmd: BITMAP;
BEGIN
  IF w^.desc=NIL THEN RETURN END;
  bmd:=w^.desc;
  FOR i:=0 TO HIGH(bmd^.layers) DO
    IF bmd^.layers[i]#NIL THEN
      mem.deallocate(bmd^.layers[i],bmd^.WPL*bmd^.H)
    END
  END;
  bmd^.mask:={}; DISPOSE(bmd); w^.desc:=NIL; w^.out:=w^.d
END disposebmd;

PROCEDURE newbmd(w: Window; W,H: INTEGER);

  PROCEDURE dispose(VAR bmd: BITMAP);
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(bmd^.layers) DO
      IF bmd^.layers[i]#NIL THEN
        mem.deallocate(bmd^.layers[i],bmd^.H*bmd^.WPL);
      END
    END;
    DISPOSE(bmd)
  END dispose;

  VAR i: INTEGER;
    bmd: BITMAP;
    ptr: BITMAP;

BEGIN
  NEW(bmd);
  IF NOT done THEN RETURN END;
  bmd^.W:=W;  bmd^.mask:=w^.d.fore;    bmd^.BASE:=NIL;
  bmd^.H:=H;  bmd^.WPL :=(W+31) DIV 32;
  low.fill(bmd^.layers,NIL);
  IF (W#0) & (H#0) THEN
    INC(bmd^.W,w^.d.sx MOD 32);
    FOR i:=0 TO HIGH(bmd^.layers) DO
      IF i IN bmd^.mask THEN
        allocate(bmd^.layers[i],bmd^.H*bmd^.WPL);
        IF NOT done THEN dispose(bmd); RETURN END;
        low._zero(bmd^.layers[i],bmd^.H*bmd^.WPL)
      END
    END
  ELSE
    bmd^.mask:={}
  END;
  w^.desc:=bmd;  w^.out:=w^.d
END newbmd;

PROCEDURE resizebmd(v: Window; W,H: INTEGER);

  PROCEDURE dispose(bmd: bmg.BMD);
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(bmd.layers) DO
      IF bmd.layers[i]#NIL THEN
        mem.deallocate(bmd.layers[i],bmd.H*bmd.WPL)
      END
    END
  END dispose;

  PROCEDURE copylayer(VAR bmd,old: bmg.BMD; i: INTEGER);
    VAR h,w: INTEGER;   sou: ADDRESS;
        len: INTEGER;   des: ADDRESS;
       mask: BITSET;    adr: ADDRESS;
  BEGIN
    allocate(bmd.layers[i],bmd.H*bmd.WPL);
    IF NOT done THEN RETURN END;
    low._zero(bmd.layers[i],bmd.H*bmd.WPL);
    h  :=min(bmd.H,old.H);
    len:=min(bmd.WPL,old.WPL);
    IF h=0 THEN RETURN END;
    CASE v^.d.corner OF
    |ruc:
      sou:=old.layers[i]+(old.H-1)*old.WPL;
      des:=bmd.layers[i]+(bmd.H-1)*bmd.WPL;
      w:=bmd.W MOD 32;
      IF w=0 THEN mask:={} ELSE mask:={w..31} END;
      w:=bmd.WPL-len;
      REPEAT
        _move(des,sou,len);  adr:=des+len-1;      adr^:=BITSET(adr^)-mask;
        DEC(des,bmd.WPL);    DEC(sou,old.WPL);    DEC(h)
      UNTIL h=0
    |rdc:
      sou:=old.layers[i];
      des:=bmd.layers[i];
      w:=bmd.W MOD 32;
      IF w=0 THEN mask:={} ELSE mask:={w..31} END;
      w:=bmd.WPL-len;
      REPEAT
        _move(des,sou,len);  adr:=des+len-1;      adr^:=BITSET(adr^)-mask;
        INC(des,bmd.WPL);    INC(sou,old.WPL);    DEC(h)
      UNTIL h=0
    ELSE
      ASSERT(FALSE) (* not implemented yet *)
    END
  END copylayer;

  VAR i,j: INTEGER;
      bmd: bmg.BMD;
      old: BITMAP;
BEGIN
  old:=v^.desc;
  IF old=NIL THEN newbmd(v,W,H); RETURN END;
  bmd.W:=W;  bmd.mask:=v^.d.fore;     bmd.BASE:=NIL;
  bmd.H:=H;  bmd.WPL :=(W+31) DIV 32;
  low.fill(bmd.layers,NIL);
  IF (W#0) & (H#0) THEN
    FOR i:=0 TO HIGH(bmd.layers) DO
      IF i IN bmd.mask THEN
        copylayer(bmd,old^,i);
        IF NOT done THEN dispose(bmd); RETURN  END;
        IF bmd.layers[i]=NIL THEN EXCL(bmd.mask,i) END
      END
    END;
  ELSE
    bmd.mask:={}
  END;
  dispose(old^);
  old^:=bmd
END resizebmd;

----------------------------------------------------------------

PROCEDURE nopresize(wnd: WINDOW; move: BOOLEAN; w,h: INTEGER); END nopresize;

PROCEDURE create (VAR wnd: WINDOW; desk: WINDOW; X,Y,W,H: INTEGER;
                fore,back: BITSET; fresh: PAINT);
  VAR v,dsk: Window;
BEGIN
  dsk:=Window(desk);
  IF (DESK#NIL) & ((dsk=NIL) OR (dsk^.magic#MAGIC)) THEN bad_desc; RETURN END;
  NEW(v);
  IF NOT done THEN wnd:=NIL; RETURN END;
  v^.magic :=0;
  WITH v^ DO
    d.tool:=T;
    d.tool.clip.w:=W;     d.x:=X; d.w:=W;
    d.tool.clip.h:=H;     d.y:=Y; d.h:=H;
    d.back :=back*scrM;   d.fore:=fore*B^.mask;
    d.mask :=d.fore;      d.mode:=normal;

    d.closed :=TRUE;
    d.visible:=FALSE;
    d.desk  :=desk;
    d.top   :=NIL;
    d.bottom:=NIL;
    d.image :=(fresh=image);

    d.refresh:=fresh;

    d.resize :=nopresize; d.corner:=ruc;
    d.minw:=0;            d.maxw:=4096;
    d.minh:=0;            d.maxh:=01000h;

    actor  :=fresh;
    desc :=NIL;
    up   :=NIL;
    dw   :=NIL;
    desk :=NIL;
    obj  :=NIL;
    iter :=NIL;
    iterp:=NIL;
    start:=NIL;
    stop :=NIL;
    req  :=NIL;
    state:=ground;
    magic:=MAGIC
  END;
  IF v^.d.image THEN
    newbmd(v,W,H);
    IF NOT done THEN DISPOSE(v); RETURN END;
  END;

  IF DESK#NIL THEN
    tie(v,Window(dsk^.d.top),TRUE);
    v^.d.sx:=X+dsk^.d.sx; v^.d.sy:=Y+dsk^.d.sy
  ELSE
    v^.d.sx:=0; v^.d.sy:=0
  END;
  v^.out:=v^.d;
  wnd:=WINDOW(v);
  done:=TRUE
END create;

PROCEDURE dispose(VAR wnd: WINDOW);
  VAR w: Window;      son: WINDOW;
      o: OBJECTS;    disp: PROCEDURE(WINDOW);
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) OR (w=DESK) THEN RETURN END;

  o:=w^.obj;
  WHILE (o#NIL) & (o^.name#"DISPOSE") DO o:=o^.next END;
  IF o#NIL THEN disp:=o^.val; disp(wnd) END;

  (* kill sons: *)
  w:=Window(w^.d.top);
  WHILE w#NIL DO son:=WINDOW(w); dispose(son); w:=w^.dw END;
  (* kill self: *)
  w:=Window(wnd);
  IF NOT w^.d.closed THEN close(wnd) END;
  disposebmd(w); untie(w);
  w^.magic:=0; DISPOSE(w); wnd:=NIL
END dispose;

----------------------------------------------------------------

PROCEDURE save(wnd: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR t: TOOL;
      w: Window;            dx,dy,dh,dw: INTEGER;
    bmd: BITMAP;            sx,sy,so,do: INTEGER;
   mask: BITSET;            sb,db      : ADDRESS;
BEGIN
  w:=Window(wnd);
  t:=T;
  t.color:=w^.d.fore;
  t.mask :=w^.d.fore;
  bmd    :=w^.desc;
  sx:=cx+w^.d.sx;
  sy:=cy+w^.d.sy;
  IF bmd=NIL THEN RETURN END;
  dw:=cw;  dx:=cx+w^.d.sx;
  dh:=ch;  dy:=cy+w^.d.sy;
  IF dx<0           THEN dw:=dw+dx; cx:=cx-dx; dx:=0 END;
  IF dy<0           THEN dh:=dh+dy; cy:=cy-dy; dy:=0 END;
  IF dx+dw>T.clip.w THEN dw:=T.clip.w-dx END;
  IF dy+dh>T.clip.h THEN dh:=T.clip.h-dy END;
  IF (dw<=0) OR (dh<=0) THEN RETURN END;
  so:=(bmd^.H-cy-dh)*bmd^.WPL;
  do:=(B^  .H-dy-dh)*B^.  WPL;
  REPEAT
    mask:=t.mask*B^.mask;  db:=ADR(B^.layers[0]);  sb:=ADR(bmd^.layers[0]);
    REPEAT
      IF mask*{0}#{} THEN
        mask:=mask-{0}; bmv(INTEGER(sb^)+so,cx,INTEGER(db^)+do,dx,dw)
      END;
      mask:=mask>>1; INC(sb); INC(db)
    UNTIL mask={};
    INC(so,bmd^.WPL); INC(do,B^.WPL); DEC(dh)
  UNTIL dh=0
END save;

PROCEDURE image(wnd: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR w: Window;
    bmd: BITMAP;            fore,back  : BITSET;
   mask: BITSET;            dx,dy,dh,dw: INTEGER;
  so,do: INTEGER;           sb,db      : ADDRESS;
BEGIN
  w:=Window(wnd);
  IF w^.state*callimage={} THEN RETURN END;
  bmd:=w^.desc;
  IF bmd=NIL THEN RETURN END;
(*
bmg.print(B,T,0,300,fnt.font,"image(%08h,%d,%d,%d,%d)   ",w,cx,cy,cw,ch);
key.read(bug);
bmg.print(B,T,0,300,fnt.font,"                             ",w,cx,cy,cw,ch);
*)
  dw:=cw;  dx:=cx+w^.d.sx;
  dh:=ch;  dy:=cy+w^.d.sy;
  IF dx<0           THEN dw:=dw+dx; cx:=cx-dx; dx:=0 END;
  IF dy<0           THEN dh:=dh+dy; cy:=cy-dy; dy:=0 END;
  IF dx+dw>T.clip.w THEN dw:=T.clip.w-dx END;
  IF dy+dh>T.clip.h THEN dh:=T.clip.h-dy END;
  IF (dw<=0) OR (dh<=0) THEN RETURN END;
  so:=(bmd^.H-cy-dh)*bmd^.WPL;
  do:=(B^  .H-dy-dh)*B^.  WPL;
  IF w^.state*ground={} THEN
    REPEAT
      mask:=w^.d.fore*B^.mask;  db:=ADR(B^.layers[0]); sb:=ADR(bmd^.layers[0]);
      REPEAT
        IF mask*{0}#{} THEN
          mask:=mask-{0}; bmv(INTEGER(db^)+do,dx,INTEGER(sb^)+so,cx,dw)
        END;
        mask:=mask>>1; INC(sb); INC(db)
      UNTIL mask={};
      INC(so,bmd^.WPL); INC(do,B^.WPL); DEC(dh)
    UNTIL dh=0
  ELSE
    REPEAT
      mask:=B^.mask;
      fore:=w^.d.fore;          sb:=ADR(bmd^.layers[0]);
      back:=w^.d.back;          db:=ADR(B^.layers[0]);
      REPEAT
        IF fore*{0}#{} THEN
          bmv(INTEGER(db^)+do,dx,INTEGER(sb^)+so,cx,dw)
        ELSIF back*{0}#{} THEN
          bmv(INTEGER(db^)+do,dx,_fill,dx MOD 32,dw)
        ELSE
          bmv(INTEGER(db^)+do,dx,_zero,dx MOD 32,dw)
        END;
        mask:=(mask-{0})>>1; fore:=fore>>1; back:=back>>1; INC(sb); INC(db)
      UNTIL mask={};
      INC(so,bmd^.WPL); INC(do,B^.WPL); DEC(dh)
    UNTIL dh=0
  END
END image;

PROCEDURE draw(w,ignore: Window; cx,cy,cw,ch: INTEGER);
  VAR t: TOOL;  save: BITSET;
BEGIN
(*
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"draw(%08h,%d,%d,%d,%d)  %d,%d,%d,%d   ",w,cx,cy,cw,ch,w^.d.sx,w^.d.sy,w^.d.w,w^.d.h);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                        ")
END;
*)
ASSERT((w#NIL) & (w^.magic=MAGIC));
ASSERT(NOT w^.d.closed);
ASSERT(w^.d.visible);

  IF (cw<=0) OR (ch<=0) THEN RETURN END;

  DEC(cx,w^.d.sx);
  DEC(cy,w^.d.sy);
  IF cx<0 THEN DEC(cw,-cx); cx:=0 END;
  IF cy<0 THEN DEC(ch,-cy); cy:=0 END;
  IF cx+cw>w^.d.w THEN cw:=w^.d.w-cx END;
  IF cy+ch>w^.d.h THEN ch:=w^.d.h-cy END;

  IF (cw<=0) OR (ch<=0) THEN RETURN END;

  IF (w^.state*ground#{}) & (w^.d.back#{}) THEN
    t:=w^.d.tool;
    t.color:=w^.d.back;
    t.mask :=w^.d.back;
    t.mode :=rep;
    bmg.rect(B,t,w^.d.sx+cx,w^.d.sy+cy,w^.d.sx+cx+cw-1,w^.d.sy+cy+ch-1)
  END;

  save:=w^.state;
  w^.state:=w^.state+callimage+inrefresh;
  w^.actor(WINDOW(w),cx,cy,cw,ch);
  w^.state:=save
END draw;

PROCEDURE _ground(w: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR t: TOOL;
BEGIN
  t:=T;
  t.mode  :=bmg.bic;
  t.color :=t.mask;
  bmg.rect(B,t,cx,cy,cx+cw-1,cy+ch-1)
END _ground;

PROCEDURE notcross(u: Window; cx,cy,cw,ch: INTEGER): BOOLEAN;
BEGIN
  RETURN (cx+cw<=u^.d.sx) OR (u^.d.sx+u^.d.w<=cx)
      OR (cy+ch<=u^.d.sy) OR (u^.d.sy+u^.d.h<=cy)
END notcross;

TYPE
  ACTION    = PROCEDURE (Window,Window,INTEGER,INTEGER,INTEGER,INTEGER);

PROCEDURE x_traverse(w,u: Window; cx,cy,cw,ch: INTEGER; do0,do1: ACTION);
  VAR i: INTEGER; bot,dsk: Window;
BEGIN
(*
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"x_tra(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                        ")
END;
*)

  IF (ch<=0) OR (cw<=0) THEN (* empty *) RETURN END;
  dsk:=Window(u^.d.desk);
  bot:=Window(dsk^.d.bottom);
  IF (u=NIL) OR ((u=bot) & (dsk=NIL)) OR (u=w^.stop) THEN
    do0(w,u,cx,cy,cw,ch); RETURN
  END;
  IF u=bot THEN
    x_traverse(w,Window(dsk^.up),cx,cy,cw,ch,do0,do1); RETURN
  END;
  IF (NOT u^.d.visible) OR notcross(u,cx,cy,cw,ch) THEN
    x_traverse(w,u^.up,cx,cy,cw,ch,do0,do1); RETURN
  END;
  IF cy <= u^.d.sy THEN
    i:=min(ch,u^.d.sy-cy);
    x_traverse(w,u^.up,cx,cy,cw,i,do0,do1); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.sy <= cy THEN
    i:=min(ch,(u^.d.sy+u^.d.h-1)-cy+1);
    x_traverse(w,u^.up,cx,cy+i,cw,ch-i,do0,do1); ch:=i
  END;
  IF cx <= u^.d.sx THEN
    i:=min(cw,u^.d.sx-cx);
    x_traverse(w,u^.up,cx,cy,i,ch,do0,do1);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.sx <= cx THEN
    i:=min(cw,(u^.d.sx+u^.d.w-1)-cx+1);
    x_traverse(w,u^.up,cx+i,cy,cw-i,ch,do0,do1); cw:=i
  END;
  do1(w,u,cx,cy,cw,ch)
END x_traverse;

PROCEDURE i_traverse(w,u: Window; cx,cy,cw,ch: INTEGER; do0,do1: ACTION);
  VAR i: INTEGER; up,top: Window;
BEGIN
ASSERT(w^.d.visible);

  IF (ch<=0) OR (cw<=0) THEN (* empty *) RETURN END;
  IF u=NIL THEN  do0(w,u,cx,cy,cw,ch);  RETURN  END;
  top:=Window(w^.d.top);
  IF u=top THEN up:=NIL ELSE up:=u^.up END;
  IF (NOT u^.d.visible) OR notcross(u,cx,cy,cw,ch) THEN
    i_traverse(w,up,cx,cy,cw,ch,do0,do1);  RETURN
  END;
  IF cy <= u^.d.sy THEN
    i:=min(ch,u^.d.sy-cy);
    i_traverse(w,up,cx,cy,cw,i,do0,do1);  cy:=cy+i;  ch:=ch-i
  END;
  IF u^.d.sy <= cy THEN
    i:=min(ch,(u^.d.sy+u^.d.h-1)-cy+1);
    i_traverse(w,up,cx,cy+i,cw,ch-i,do0,do1); ch:=i
  END;
  IF cx <= u^.d.sx THEN
    i:=min(cw,u^.d.sx-cx);
    i_traverse(w,up,cx,cy,i,ch,do0,do1);  cx:=cx+i;  cw:=cw-i
  END;
  IF u^.d.sx <= cx THEN
    i:=min(cw,(u^.d.sx+u^.d.w-1)-cx+1);
    i_traverse(w,up,cx+i,cy,cw-i,ch,do0,do1);  cw:=i
  END;
  do1(w,u,cx,cy,cw,ch)
END i_traverse;

PROCEDURE nop(w,u: Window; cx,cy,cw,ch: INTEGER);
END nop;


PROCEDURE i_ref(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
ASSERT(w^.d.visible);

  u:=Window(w^.d.top);
  IF u#NIL THEN
    LOOP
      IF (u^.d.visible) & NOT notcross(u,cx,cy,cw,ch) THEN
        u^.stop:=Window(w^.d.bottom);
        x_traverse(u,u^.up,cx,cy,cw,ch,i_ref,nop);
        u^.stop:=NIL
      END;
      IF u=Window(w^.d.bottom) THEN EXIT END;
      u:=u^.dw
    END
  END;
  i_traverse(w,Window(w^.d.bottom),cx,cy,cw,ch,draw,nop)
END i_ref;

PROCEDURE clipdesk(w: Window; VAR cx,cy,cw,ch: INTEGER);
  VAR d: Window;  dx,dy,dw,dh: INTEGER;
BEGIN
  IF w=DESK THEN RETURN END;
  d:=Window(w^.d.desk);
  dx:=d^.d.sx;  dw:=d^.d.w;
  dy:=d^.d.sy;  dh:=d^.d.h;
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"clipdesk(%d,%d,%d,%d  %d %d %d %d)   ",cx,cy,cw,ch,dx,dy,dw,dh);
END;
  IF cx<dx THEN DEC(cw,dx-cx); cx:=dx END;
  IF cy<dy THEN DEC(ch,dy-cy); cy:=dy END;
  IF cx+cw>dx+dw THEN cw:=dx+dw-cx END;
  IF cy+ch>dy+dh THEN ch:=dy+dh-cy END;
IF BUG THEN
  bmg.print(B,T,0,280,fnt.font,"clipdesk(%d,%d,%d,%d)   ",cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                           ");
  bmg.print(B,T,0,280,fnt.font,"                                           ")
END;
END clipdesk;

PROCEDURE _refresh(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
  clipdesk(w,cx,cy,cw,ch);
  x_traverse(w,w^.up,cx,cy,cw,ch,i_ref,nop)
END _refresh;

PROCEDURE i_ref0(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
  u:=Window(w^.d.top);
  i_traverse(w,Window(w^.d.bottom),cx,cy,cw,ch,draw,nop)
END i_ref0;

PROCEDURE _refresh0(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
  clipdesk(w,cx,cy,cw,ch);
  x_traverse(w,w^.up,cx,cy,cw,ch,i_ref0,nop)
END _refresh0;

PROCEDURE _refreshX(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
  clipdesk(w,cx,cy,cw,ch);
  x_traverse(w,w^.up,cx,cy,cw,ch,draw,nop)
END _refreshX;

PROCEDURE _ref_desk(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR dsk: Window;
BEGIN
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"refdsk(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                        ")
END;
  dsk:=Window(w^.d.desk);
ASSERT(dsk^.d.visible);
                          _refresh(dsk,dsk^.up,cx,cy,cw,ch)
END _ref_desk;

PROCEDURE _clear(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"clear(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                        ")
END;
  clipdesk(w,cx,cy,cw,ch);
  x_traverse(w,u,cx,cy,cw,ch,_ref_desk,nop)
END _clear;

PROCEDURE _sinkup(w,u: Window; cx,cy,cw,ch: INTEGER);
BEGIN
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"sinkup(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                 ")
END;
  _refresh(u,u^.up,cx,cy,cw,ch)
END _sinkup;

PROCEDURE _sink(w,up: Window; cx,cy,cw,ch: INTEGER);
BEGIN
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"sink(%08h,%08h,%d,%d,%d,%d)   ",w,dw,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                 ")
END;
  clipdesk(w,cx,cy,cw,ch);
  w^.stop:=up;
  x_traverse(w,w^.up,cx,cy,cw,ch,nop,_sinkup);
  w^.stop:=NIL
END _sink;

PROCEDURE _emergeup(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR save: Window;
BEGIN
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"emergeup(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                 ")
END;
  save :=w^.up;
  w^.up:=w^.start;
  _refresh(w,w^.up,cx,cy,cw,ch);
  w^.up:=save
END _emergeup;

PROCEDURE _emerge(w,u,t: Window; cx,cy,cw,ch: INTEGER);
BEGIN
IF BUG THEN
  bmg.print(B,T,0,300,fnt.font,"emerge(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
  key.read(bug);
  bmg.print(B,T,0,300,fnt.font,"                                 ")
END;
  clipdesk(w,cx,cy,cw,ch);
  w^.start:=t;
  w^.stop:=u^.up;
  x_traverse(w,w^.up,cx,cy,cw,ch,nop,_emergeup);
  w^.stop:=NIL;
  w^.start:=NIL;
END _emerge;

----------------------------------------------------------------

PROCEDURE refreshsub(w: Window);
BEGIN
  _refresh(w,w^.up,w^.d.sx,w^.d.sy,w^.d.w,w^.d.h)
END refreshsub;

PROCEDURE resizer(wnd: WINDOW; r: RESIZE);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  w^.d.resize:=r; w^.out.resize:=r;
  done:=TRUE
END resizer;

PROCEDURE minmax(wnd: WINDOW; minW,minH,maxW,maxH: INTEGER);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF minW<=0 THEN minW:=0 END;
  IF minH<=0 THEN minH:=0 END;
  IF (maxW<=0) OR (maxW>4096)   OR (maxW<minW) THEN bad_parm; RETURN END;
  IF (maxH<=0) OR (maxH>10000h) OR (maxH<minH) THEN bad_parm; RETURN END;

  w^.d.minw:=minW;      w^.out.minw:=minW;
  w^.d.minh:=minH;      w^.out.minh:=minH;
  w^.d.maxw:=maxW;      w^.out.maxw:=maxW;
  w^.d.maxh:=maxH;      w^.out.maxh:=maxH;

  done:=TRUE
END minmax;

PROCEDURE corner(wnd: WINDOW; corner: INTEGER);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC)       THEN bad_desc; RETURN END;
  IF NOT (corner IN {luc,ldc,ruc,rdc}) THEN bad_parm; RETURN END;
IF NOT (corner IN {ruc,rdc})         THEN bad_parm; RETURN END;
  w^.d.corner:=corner;  w^.out.corner:=corner;
  done:=TRUE
END corner;

PROCEDURE painter(wnd: WINDOW; p: PAINT);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;

  done:=TRUE;
  IF w^.d.refresh=p THEN RETURN END;
  IF p=image THEN
    ASSERT(NOT w^.d.image);
    w^.d.image:=TRUE;  w^.out.image:=TRUE;
    newbmd(w,w^.d.w,w^.d.h);
    IF NOT done THEN RETURN END
  ELSIF w^.d.image THEN
    disposebmd(w);
    w^.d.image:=FALSE;  w^.out.image:=FALSE
  END;
  w^.d.refresh:=p; w^.out.refresh:=p; w^.actor:=p;
  IF w^.d.visible THEN refreshsub(w) END
END painter;

PROCEDURE closetree(w: Window);
  VAR t,b: Window;
BEGIN
  w^.d.visible:=FALSE; w^.out.visible:=FALSE;
  IF w^.d.closed THEN RETURN END;
  t:=Window(w^.d.top);
  b:=Window(w^.d.bottom);
  IF t=NIL THEN RETURN END;
  LOOP
    closetree(t);
    IF t=b THEN RETURN END;
    t:=t^.dw
  END
END closetree;

PROCEDURE opentree(w: Window);
  VAR t,b: Window;
BEGIN
  IF w^.d.closed THEN RETURN END;
  w^.d.visible:=TRUE; w^.out.visible:=TRUE;
  t:=Window(w^.d.top);
  b:=Window(w^.d.bottom);
  IF t=NIL THEN RETURN END;
  LOOP
    opentree(t);
    IF t=b THEN RETURN END;
    t:=t^.dw
  END
END opentree;

PROCEDURE open(wnd: WINDOW);
  VAR w,d: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF (wnd=desktop) OR NOT w^.d.closed THEN RETURN END;
  w^.d.closed:=FALSE;  w^.out.closed:=FALSE;
  d:=Window(w^.d.desk);
  IF NOT d^.d.visible THEN RETURN END;
  opentree(w);
  refreshsub(w)
END open;

PROCEDURE close(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=DESK THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF w^.d.closed THEN RETURN END;
  closetree(w);
  w^.d.closed:=TRUE;  w^.out.closed:=TRUE;
  IF w^.d.visible THEN _clear(w,w^.up,w^.d.sx,w^.d.sy,w^.d.w,w^.d.h) END
END close;

PROCEDURE moveall(w: Window; dx,dy: INTEGER);
  VAR t,b: Window;
BEGIN
  INC(w^.d.sx,dx);    INC(w^.out.sx,dx);
  INC(w^.d.sy,dy);    INC(w^.out.sy,dy);
  t:=Window(w^.d.top);
  b:=Window(w^.d.bottom);
  IF t=NIL THEN RETURN END;
  LOOP
    moveall(t,dx,dy);
    IF t=b THEN RETURN END;
    t:=t^.dw
  END
END moveall;

PROCEDURE move(wnd: WINDOW; X,Y: INTEGER);
  VAR w: Window; ox,oy,ow,oh,dx,dy,y1: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=DESK THEN done:=FALSE; error:=err.unsuitable; RETURN END;
  done:=TRUE;
  IF (w^.d.sx=X) & (w^.d.sy=Y) THEN RETURN END;
  ox:=w^.d.sx;    ow:=w^.d.w;   dx:=X-w^.d.x;
  oy:=w^.d.sy;    oh:=w^.d.h;   dy:=Y-w^.d.y;
  w^.d.x:=X;   w^.out.x:=X;
  w^.d.y:=Y;   w^.out.y:=Y;
  moveall(w,dx,dy);
  IF NOT w^.d.visible THEN RETURN END;
  refreshsub(w);
  IF notcross(w,ox,oy,ow,oh) THEN
    _clear(w, w^.up, ox,oy,ow,oh)
  ELSE
    IF w^.d.sy<oy THEN
      _clear(w, w^.up, ox, w^.d.sy+oh, ow, ABS(dy) )
    ELSE
      _clear(w, w^.up, ox, oy, ow, ABS(dy));  oy:=w^.d.sy
    END;
    IF w^.d.sx<ox THEN ox:=w^.d.sx+ow END;
    _clear(w, w^.up, ox, oy,  ABS(dx), oh-ABS(dy) )
  END;
END move;

PROCEDURE movesub(w: Window; dx,dy: INTEGER);
  VAR t,b: Window;
BEGIN
  t:=Window(w^.d.top);
  b:=Window(w^.d.bottom);
  IF t=NIL THEN RETURN END;
  LOOP
    INC(t^.d.x,dx);    INC(t^.out.x,dx);
    INC(t^.d.y,dy);    INC(t^.out.y,dy);
    IF t=b THEN RETURN END;
    t:=t^.dw
  END
END movesub;

PROCEDURE resize(wnd: WINDOW; W,H: INTEGER);
  VAR w,u: Window;  i,dh,dw,ow,oh: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=DESK THEN done:=FALSE; error:=err.unsuitable; RETURN END;
  IF W>4096 THEN too_large; RETURN END;
  done:=TRUE;
  IF H<0 THEN H:=0 END;
  IF W<0 THEN W:=0 END;
  IF (w^.d.w=W) & (w^.d.h=H) THEN RETURN END;
  ow:=w^.d.w;  dw:=W-ow;
  oh:=w^.d.h;  dh:=H-oh;
  IF w^.d.image THEN
    resizebmd(w,W,H);
    IF NOT done THEN RETURN END
  END;
  wnd^.resize(wnd,FALSE,W,H);
  IF w^.d.visible THEN
    closetree(w);
    CASE w^.d.corner OF

    |ruc: _clear(w, w^.up, w^.d.sx  , w^.d.sy+H ,  ow, -dh      );
          _clear(w, w^.up, w^.d.sx+W, w^.d.sy   , -dw, min(oh,H))

    |rdc: _clear(w, w^.up, w^.d.sx  , w^.d.sy   ,  ow, -dh      );
          i:=w^.d.sy+max(0,-dh);
          _clear(w, w^.up, w^.d.sx+W, i         , -dw, min(oh,H))
    ELSE
      ASSERT(FALSE)
    END;
    opentree(w)
  END;

  w^.d.w:=W;  w^.d.tool.clip.w:=W;
  w^.d.h:=H;  w^.d.tool.clip.h:=H;
  IF w^.d.corner=rdc THEN
    movesub(w,0,dh);
    DEC(w^.d.y,dh);  DEC(w^.d.sy,dh)
  END;

  w^.out:=w^.d;
  IF NOT w^.d.visible THEN RETURN END;

  u:=w^.up;
  CASE w^.d.corner OF
  |ruc:
    _refresh(w, u, w^.d.sx   , w^.d.sy+oh, max(ow,W), dh       );
    _refresh(w, u, w^.d.sx+ow, w^.d.sy   , dw       , max(oh,H))
  |rdc:
    _refresh(w, u, w^.d.sx   , w^.d.sy   , W        , dh       );
    i:=w^.d.sy+max(0,dh);
    _refresh(w, u, w^.d.sx+ow, i         , dw       , H        )
  ELSE
    ASSERT(FALSE)
  END
END resize;

PROCEDURE moveandresize(wnd: WINDOW; X,Y,W,H: INTEGER);
  VAR w: Window; ox,oy,ow,oh,dx,dy,dw,dh,y1: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=DESK THEN done:=FALSE; error:=err.unsuitable; RETURN END;
  IF W>4096 THEN too_large; RETURN END;
  done:=TRUE;
  IF H<0 THEN H:=0 END;
  IF W<0 THEN W:=0 END;
  IF (w^.d.x=X) & (w^.d.y=Y) & (w^.d.w=W) & (w^.d.h=H) THEN RETURN END;
  IF (w^.d.x=X) & (w^.d.y=Y) THEN resize(wnd,W,H); RETURN END;
  IF (w^.d.w=W) & (w^.d.h=H) THEN move  (wnd,X,Y); RETURN END;
  IF w^.d.image THEN
    resizebmd(w,W,H);
    IF NOT done THEN RETURN END
  END;
  wnd^.resize(wnd,TRUE,W,H);
  ow:=w^.d.w;  dw:=W-ow;
  oh:=w^.d.h;  dh:=H-oh;
  IF w^.d.visible THEN
    (* here cutting always doing as when ruc moved *)
    closetree(w);
    _clear(w, w^.up, w^.d.sx  , w^.d.sy+H,  ow, -dh      );
    _clear(w, w^.up, w^.d.sx+W, w^.d.sy  , -dw, min(oh,H));
    opentree(w)
  END;

  ox:=w^.d.sx;    dx:=X-w^.d.x;
  oy:=w^.d.sy;    dy:=Y-w^.d.y;
  IF w^.d.corner=rdc THEN DEC(dy,dh) END;
  w^.d.x:=X;
  w^.d.y:=Y;
  moveall(w,dx,dy);
  w^.d.w:=W;   w^.d.tool.clip.w:=W;
  w^.d.h:=H;   w^.d.tool.clip.h:=H;
  w^.out:=w^.d;
  IF NOT w^.d.visible THEN RETURN END;
  refreshsub(w);
  W:=min(W,ow);
  H:=min(H,oh);
  IF notcross(w,ox,oy,W,H) THEN
    _clear(w, w^.up, ox,oy,W,H)
  ELSE
    IF w^.d.sy<oy THEN
      _clear(w, w^.up, ox, w^.d.sy+H, W, ABS(dy) )
    ELSE
      _clear(w, w^.up, ox, oy, W, ABS(dy));  oy:=w^.d.sy
    END;
    IF w^.d.sx<ox THEN ox:=w^.d.sx+W END;
    _clear(w, w^.up, ox, oy,  ABS(dx), H-ABS(dy) )
  END
END moveandresize;



PROCEDURE pass(wnd: WINDOW; desk: WINDOW);
  VAR w,d: Window;  closed: BOOLEAN; dx,dy: INTEGER;
BEGIN
  w:=Window(wnd);
  d:=Window(desk);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF (d=NIL) OR (d^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF w=DESK THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF w^.d.desk=desk THEN RETURN END;
  dx:=d^.d.sx-w^.d.sx;
  dy:=d^.d.sy-w^.d.sy;
  closed:=w^.d.closed;
  IF NOT closed THEN close(wnd) END;
  untie(w); w^.d.desk:=desk; tie(w,Window(d^.d.top),TRUE);
  w^.out:=w^.d;
  moveall(w,dx,dy);
  IF NOT closed THEN open(wnd) END
END pass;

PROCEDURE _upperthan(v,u: Window): BOOLEAN;
  VAR w,d,top,bot: Window;
BEGIN
  IF v=u THEN RETURN TRUE END;
  top:=Window(u^.d.top);
  bot:=Window(u^.d.bottom);
  IF top#NIL THEN
    w:=bot;
    LOOP
      IF w=v THEN RETURN TRUE END;
      IF (w^.d.top#NIL) & _upperthan(v,Window(w^.d.top)) THEN
        RETURN TRUE
      END;
      IF w^.up=bot THEN EXIT END;
      w:=w^.up
    END
  END;
  d:=Window(u^.d.desk); bot:=Window(d^.d.bottom);
  w:=u^.up;
  RETURN (w#NIL) & (w#bot) & _upperthan(v,w)
END _upperthan;

PROCEDURE upperthen(wnd,und: WINDOW): BOOLEAN;
  VAR w,u: Window;
BEGIN
  w:=Window(wnd);
  u:=Window(und);
  IF u=w THEN RETURN FALSE END;
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN FALSE END;
  IF (u=NIL) OR (u^.magic#MAGIC) THEN RETURN TRUE  END;
  IF  w=DESK THEN RETURN FALSE END;
  IF  u=DESK THEN RETURN TRUE  END;
  RETURN _upperthan(w,u)
END upperthen;

PROCEDURE putunder(wnd,und: WINDOW);
  VAR w,u,d,up: Window;
  PROCEDURE retie; BEGIN untie(w); tie(w,u^.up,FALSE) END retie;
BEGIN
  w:=Window(wnd);
  u:=Window(und);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (u=NIL) OR (u^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (w=DESK) OR (u=DESK)        THEN done:=FALSE; error:=err.unsuitable END;
  IF (w^.d.desk#u^.d.desk)       THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE; d:=Window(w^.d.desk);
  IF (w=u) OR (w#Window(d^.d.top)) & (w^.up=u) THEN RETURN END;
  IF NOT w^.d.visible  THEN retie; RETURN END;
  IF upperthen(wnd,und) THEN
    up:=w^.up;
    retie;
    _sink  (w,up,      w^.d.sx,w^.d.sy,w^.d.w,w^.d.h)
  ELSE
    _emerge(w,u^.dw,u, w^.d.sx,w^.d.sy,w^.d.w,w^.d.h);
    retie
  END
END putunder;

PROCEDURE putover(wnd,und: WINDOW);
  VAR w,u,d,up: Window;
  PROCEDURE retie; BEGIN untie(w); tie(w,u,TRUE) END retie;
BEGIN
  w:=Window(wnd);
  u:=Window(und);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (u=NIL) OR (u^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (w=DESK)                    THEN done:=FALSE; error:=err.unsuitable END;
  IF (w^.d.desk#u^.d.desk)       THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  d:=Window(w^.d.desk);
  IF (w=u) OR (w#Window(d^.d.bottom)) & (w^.dw=u) THEN RETURN END;
  IF NOT w^.d.visible THEN retie; RETURN END;
  IF upperthen(wnd,und) THEN
    up:=w^.up;
    retie;
    _sink  (w,up,      w^.d.sx,w^.d.sy,w^.d.w,w^.d.h)
  ELSE
    _emerge(w,u,u^.up, w^.d.sx,w^.d.sy,w^.d.w,w^.d.h);
    retie
  END
END putover;

PROCEDURE ontop(wnd: WINDOW);
  VAR w,d: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF w=DESK THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  d:=Window(w^.d.desk);
  IF w=Window(d^.d.top) THEN RETURN END;
  putover(wnd,d^.d.top)
END ontop;

PROCEDURE onbottom(wnd: WINDOW);
  VAR w,d: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF w=DESK THEN RETURN END;
  d:=Window(w^.d.desk);
  IF w=Window(d^.d.bottom) THEN RETURN END;
  putunder(wnd,d^.d.bottom)
END onbottom;

----------------------------------------------------------------

PROCEDURE refreshbox(wnd: WINDOW; X,Y,W,H: INTEGER); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF (NOT w^.d.visible) OR (W<=0) OR (H<=0) THEN RETURN END;
  w^.state:=w^.state-ground;
  _refresh0(w,w^.up,w^.d.sx+X,w^.d.sy+Y,W,H);
  w^.state:=w^.state+ground
END refreshbox;

PROCEDURE refresh(wnd: WINDOW); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  WITH w^.d.tool DO refreshbox(wnd,zX,zY,clip.w,clip.h) END
END refresh;

PROCEDURE refreshall(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  refreshsub(w)
END refreshall;

PROCEDURE savebox(wnd: WINDOW; X,Y,W,H: INTEGER); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF (NOT w^.d.visible) OR (W<=0) OR (H<=0) OR NOT w^.d.image THEN RETURN END;
  w^.actor:=save;
  _refresh0(w,w^.up,w^.d.sx+X,w^.d.sy+Y,W,H);
  w^.actor:=w^.d.refresh
END savebox;

PROCEDURE _locate(v: Window; x,y: INTEGER; VAR l: Window): BOOLEAN;
  TYPE s=BITSET;
  VAR d,top,bot: Window; X,Y: INTEGER;
BEGIN
  d:=Window(v^.d.desk);
  IF (v^.up#NIL) & (v^.up#Window(d^.d.bottom)) & _locate(v^.up,x,y,l) THEN
    RETURN TRUE
  END;
  IF NOT v^.d.visible THEN RETURN FALSE END;
  (* it's no need to clip window by it`s desktop because this
   * locate was called only if X,Y into fathers frame!
   *)
  X:=x-v^.d.sx;  Y:=y-v^.d.sy;
  IF s(X>=0) * s(X<v^.d.w) * s(Y>=0) * s(Y<v^.d.h) = {} THEN
    RETURN FALSE (* out of window *)
  END;
  IF (v^.d.bottom#NIL) & _locate(Window(v^.d.bottom),x,y,l) THEN
    RETURN TRUE (* one of subwindow located *)
  END;
  l:=v;  (* no subwindows located, so window itself *)
  RETURN TRUE
END _locate;

PROCEDURE locate(x,y: INTEGER): WINDOW;
  VAR l: Window;
BEGIN
  IF _locate(DESK,x,y,l) THEN RETURN WINDOW(l) ELSE RETURN NIL END;
END locate;

PROCEDURE up(wnd: WINDOW ): WINDOW;
  VAR w,top: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN NIL END;
  top:=Window(w^.d.desk); top:=Window(top^.d.top);
  IF w=top THEN RETURN NIL END;
  RETURN WINDOW(w^.up)
END up;

PROCEDURE dw(wnd: WINDOW ): WINDOW;
  VAR w,bot: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN NIL END;
  bot:=Window(w^.d.desk); bot:=Window(bot^.d.bottom);
  IF w=bot THEN RETURN NIL END;
  RETURN WINDOW(w^.dw)
END dw;

----------------------------------------------------------------


PROCEDURE assign(wnd: WINDOW; VAL name: ARRAY OF CHAR; obj: SYSTEM.WORD);
  VAR w: Window; o: OBJECTS; i: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  o:=w^.obj;
  WHILE (o#NIL) & (o^.name#name) DO o:=o^.next END;
  IF o#NIL THEN done:=TRUE; o^.val:=obj; RETURN END;
  NEW(o);
  IF NOT done THEN RETURN END;
  i:=0;
  WHILE (i<=HIGH(o^.name)) & (i<=HIGH(name)) & (name[i]#0c) DO INC(i) END;
  IF i>HIGH(o^.name) THEN too_large; DISPOSE(o); RETURN END;
  o^.name[i]:=0c;
  WHILE i>=0 DO o^.name[i]:=name[i]; DEC(i) END;
  o^.val:=obj;
  IF (w^.iter=NIL) OR (w^.iterp=NIL) THEN
    o^.next:=w^.obj; w^.obj:=o; w^.iter:=o
  ELSE
    ASSERT(w^.iterp^.next=w^.iter);
    w^.iterp^.next:=o;  o^.next:=w^.iter; w^.iter:=o
  END
END assign;

PROCEDURE object(wnd: WINDOW; VAL name: ARRAY OF CHAR; VAR obj: SYSTEM.WORD);
  VAR w: Window; o: OBJECTS;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  o:=w^.obj;
  WHILE (o#NIL) & (o^.name#name) DO o:=o^.next END;
  IF o=NIL THEN done:=FALSE; error:=err.no_entry; RETURN END;
  done:=TRUE; obj:=o^.val
END object;

PROCEDURE delete(wnd: WINDOW; VAL name: ARRAY OF CHAR);
  VAR w: Window; o,p: OBJECTS;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  o:=w^.obj;  p:=NIL;
  WHILE (o#NIL) & (o^.name#name) DO p:=o; o:=p^.next END;
  IF o=NIL THEN done:=FALSE; error:=err.no_entry; RETURN END;
  IF p=NIL THEN w^.obj:=o^.next ELSE p^.next:=o^.next END;
  IF    w^.iter =o THEN w^.iter :=o^.next
  ELSIF w^.iterp=o THEN w^.iterp:=p
  END;
  o^.next:=NIL; o^.val:=NIL; o^.name:="";
  DISPOSE(o);
  done:=TRUE
END delete;

PROCEDURE iterobjects(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  w^.iter:=w^.obj; w^.iterp:=NIL; done:=TRUE
END iterobjects;

PROCEDURE nextobject(wnd: WINDOW; VAR name: ARRAY OF CHAR;
                                  VAR obj : SYSTEM.WORD): BOOLEAN;
  VAR w: Window; o: OBJECTS; i: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN FALSE END;
  done:=TRUE;
  IF w^.iter=NIL THEN RETURN FALSE END;
  o:=w^.iter;
  i:=0;
  WHILE (i<=HIGH(name)) & (i<=HIGH(o^.name)) & (o^.name[i]#0c) DO INC(i) END;
  IF i>HIGH(name) THEN too_large; RETURN FALSE END;
  name[i]:=0c;
  WHILE i>=0 DO name[i]:=o^.name[i]; DEC(i) END;
  obj:=o^.val;
  w^.iter:=o^.next;
  IF w^.iter#NIL THEN w^.iterp:=o ELSE w^.iterp:=NIL END;
  RETURN TRUE
END nextobject;

----------------------------------------------------------------

PROCEDURE mode(wnd: WINDOW; mode: BITSET);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN END;
  w^.out.mode:=mode; w^.d.mode:=mode; done:=TRUE
END mode;

--------------------------- pictures ---------------------------
                           ----------

CONST
  _erase   = 01;
  _pattern = 02;
  _greed   = 03;
  _bblt    = 04;
  _dot     = 05;
  _line    = 06;
  _dline   = 07;
  _rect    = 08;
  _frame   = 09;
  _scroll  = 10;
  _xwrite  = 11;
  _trif    = 12;
  _circle  = 13;
  _circlef = 14;
  _ring    = 15;
  _arc     = 16;
  _arc3    = 17;

PROCEDURE doit(wnd: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR v: Window;
      c: bmg.BLOCK;
      t: TOOL;
    bmd: bmg.BITMAP;
    str: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
  words: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
BEGIN
  v:=Window(wnd);
  WITH v^.req^ DO
    t:=tool^;
    WITH c DO x:=cx-t.zX; y:=cy-t.zY; w:=cw; h:=ch END;
    INC(t.zX,v^.d.sx);
    INC(t.zY,v^.d.sy);
    bmg.cross(t.clip,c,t.clip);
    WITH c DO x:=0-t.zX; y:=0-t.zY; w:=B^.W; h:=B^.H END;
    bmg.cross(t.clip,c,t.clip);
    IF (t.clip.w<=0) OR (t.clip.h<=0) THEN RETURN END;
    IF v^.d.mode*deep#{} THEN t.mask:=t.mask*T.mask
    ELSE                      t.mask:=t.mask*v^.d.fore
    END;
    CASE op OF
    |_pattern: words:=arg;    bmg.pattern(B,t,block^,x0,y0,words^)
    |_xwrite : str:=arg;  x0:=bmg.xwrite (B,t,x0,y0,font,str^,x1,y1)
    |_bblt   : bmd:=arg;  bmg.bblt(B,t,x0,y0,bmd,stool^,block^)
    |_erase  : bmg.erase  (B)
    |_greed  : bmg.grid   (B,t,block^,x0,y0)
    |_dot    : bmg.dot    (B,t,x0,y0)
    |_line   : bmg.line   (B,t,x0,y0,x1,y1)
    |_dline  : bmg.dline  (B,t,x0,y0,x1,y1,r)
    |_rect   : bmg.rect   (B,t,x0,y0,x1,y1)
    |_frame  : bmg.frame  (B,t,x0,y0,x1,y1)
    |_circle : bmg.circle (B,t,x0,y0,r)
    |_circlef: bmg.circlef(B,t,x0,y0,r)
    |_ring   : bmg.ring   (B,t,x0,y0,x1,y1)
    |_trif   : bmg.trif   (B,t,x0,y0,x1,y1,x2,y2)
    |_arc3   : bmg.arc3   (B,t,x0,y0,x1,y1,x2,y2)
    |_arc    : bmg.arc    (B,t,x0,y0,x1,y1,x2,y2,r)
    |_scroll : bmg.scroll (B,t,x0,y0)
    END
  END
END doit;

PROCEDURE refreshtool(v: Window; VAL t: TOOL; X,Y,W,H: INTEGER);
  VAR c: bmg.BLOCK;  save: BITSET;
BEGIN
  IF (NOT v^.d.visible) OR (v^.d.mode*scr={}) OR (W<=0) OR (H<=0) THEN
    RETURN
  END;
  c.x:=X; c.y:=Y; c.w:=W; c.h:=H;
  bmg.cross(c,c,t.clip);
  save:=v^.state;
  v^.state:=v^.state-ground+callimage;
  IF inrefresh*v^.state#{} THEN
    draw     (v,v^.up,v^.d.sx+t.zX+c.x,v^.d.sy+t.zY+c.y,c.w,c.h)
  ELSIF v^.d.mode*glass#{} THEN
    _refreshX(v,v^.up,v^.d.sx+t.zX+c.x,v^.d.sy+t.zY+c.y,c.w,c.h)
  ELSE
    _refresh0(v,v^.up,v^.d.sx+t.zX+c.x,v^.d.sy+t.zY+c.y,c.w,c.h)
  END;
  v^.state:=save
END refreshtool;

PROCEDURE redraw(v: Window; VAR req: REQUEST);
  VAR save: BITSET;
BEGIN
  v^.req  :=ADR(req);
  v^.actor:=doit;
  save:=v^.state;
  v^.state:=v^.state-ground+callimage;
  WITH req.tool^.clip DO
    IF inrefresh*v^.state#{} THEN
      draw    (v,v^.up,v^.d.sx+x+req.tool^.zX,v^.d.sy+y+req.tool^.zY,w,h)
    ELSIF v^.d.mode*glass#{} THEN
      _refreshX(v,v^.up,v^.d.sx+x+req.tool^.zX,v^.d.sy+y+req.tool^.zY,w,h)
    ELSE
      _refresh0(v,v^.up,v^.d.sx+x+req.tool^.zX,v^.d.sy+y+req.tool^.zY,w,h)
    END
  END;
  v^.state:=save;
  v^.actor:=v^.d.refresh
END redraw;

PROCEDURE erase(wnd: WINDOW);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.erase(bmd);
    IF v^.d.mode*scr={} THEN RETURN END;
    refresh(wnd)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_erase;  redraw(v,req)
  END
END erase;

PROCEDURE pattern(wnd: WINDOW;  VAL t: TOOL;  VAL b: BLOCK;
                  W,H: INTEGER; VAL p: ARRAY OF WORD);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.pattern(bmd,t,b,W,H,p);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,t,b.x,b.y,b.w,b.h)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_pattern;   req.arg  :=ADR(p);
    req.x0:=W;          req.tool :=ADR(t);
    req.y0:=H;          req.block:=ADR(b);
    redraw(v,req)
  END
END pattern;

PROCEDURE fill(wnd: WINDOW; VAL t: TOOL; VAL b: BLOCK; W: INTEGER; SEQ p: WORD);
BEGIN
  pattern(wnd,t,b,W,HIGH(p)+1,p)
END fill;

PROCEDURE grid(wnd: WINDOW; t: TOOL; b: BLOCK; xstep,ystep: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.grid(bmd,t,b,xstep,ystep);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,t,b.x,b.y,b.w,b.h)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_greed;     req.tool :=ADR(t);
    req.x0:=xstep;      req.block:=ADR(b);
    req.y0:=ystep;      redraw(v,req)
  END
END grid;

PROCEDURE dot(wnd: WINDOW; VAL t: TOOL; x,y: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.dot(bmd,t,x,y);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,t,x,y,1,1)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_dot;    req.tool:=ADR(t);
    req.x0:=x;       req.y0  :=y;
    redraw(v,req)
  END
END dot;

PROCEDURE line(wnd: WINDOW; VAL t: TOOL; x0,y0,x1,y1: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.line(bmd,t,x0,y0,x1,y1)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_line;    req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.x1:=x1;       req.y1  :=y1;
    redraw(v,req)
  END
END line;

PROCEDURE dline(wnd: WINDOW; VAL t: TOOL; x0,y0,x1,y1: INTEGER; VAR r: WORD);
  VAR v: Window;
      i: INTEGER;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    i:=r;
    bmg.dline(bmd,t,x0,y0,x1,y1,r)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_dline;   req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.x1:=x1;       req.y1  :=y1;
    req.r :=i;        redraw(v,req);      r:=req.r
  END
END dline;

PROCEDURE hline(wnd: WINDOW; VAL t: TOOL; x0,y0,x1: INTEGER);
  VAR v: Window;
      i: INTEGER;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.line(bmd,t,x0,y0,x1,y0);
    IF v^.d.mode*scr={} THEN RETURN END;
    IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
    refreshtool(v,t,x0,y0,x1-x0+1,1)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_line;    req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.x1:=x1;       req.y1  :=y0;
    redraw(v,req)
  END
END hline;

PROCEDURE vline(wnd: WINDOW; VAL t: TOOL; x0,y0,y1: INTEGER);
  VAR v: Window;
      i: INTEGER;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.line(bmd,t,x0,y0,x0,y1);
    IF v^.d.mode*scr={} THEN RETURN END;
    IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
    refreshtool(v,t,x0,y0,1,y1-y0+1)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_line;    req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.x1:=x0;       req.y1  :=y1;
    redraw(v,req)
  END
END vline;

PROCEDURE rect(wnd: WINDOW; VAL t: TOOL; x0,y0,x1,y1: INTEGER);
  VAR v: Window;
      i: INTEGER;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.rect(bmd,t,x0,y0,x1,y1);
    IF v^.d.mode*scr={} THEN RETURN END;
    IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
    IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
    refreshtool(v,t,x0,y0,x1-x0+1,y1-y0+1)
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_rect;    req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.x1:=x1;       req.y1  :=y1;
    redraw(v,req)
  END
END rect;

PROCEDURE frame(wnd: WINDOW; VAL t: TOOL; x0,y0,x1,y1: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    W,H: INTEGER;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.frame(bmd,t,x0,y0,x1,y1);
    IF v^.d.closed OR (v^.d.mode*scr={}) THEN RETURN END;
    IF x0>x1 THEN W:=x0; x0:=x1; x1:=W END;
    IF y0>y1 THEN H:=y0; y0:=y1; y1:=H END;
    W:=x1-x0+1;
    H:=y1-y0+1;
    IF (H=1) OR (W=1) THEN refreshtool(v,t,x0,y0,W,H); RETURN END;
    refreshtool(v,t,x0,y0,W,1);  refreshtool(v,t,x0,y0,1,H);
    refreshtool(v,t,x0,y1,W,1);  refreshtool(v,t,x1,y0,1,H);
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_frame;   req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.x1:=x1;       req.y1  :=y1;
    redraw(v,req)
  END
END frame;

PROCEDURE bblt(des: WINDOW; VAL dt: TOOL;      x,y: INTEGER;
               sou: WINDOW; VAL st: TOOL;  VAL blk: BLOCK);
  VAR v,o: Window;
     dbmd: BITMAP;
     sbmd: BITMAP;
      req: REQUEST;
BEGIN
  v:=Window(des);
  o:=Window(sou);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF (o=NIL) OR (o^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) & o^.d.image THEN
    dbmd:=v^.desc;
    IF dbmd=NIL THEN RETURN END;
    sbmd:=o^.desc;
    IF sbmd=NIL THEN RETURN END;
    bmg.bblt(dbmd,dt,x,y,sbmd,st,blk);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,dt,x,y,blk.w,blk.h)
  ELSIF (o^.d.image) & (v^.d.mode*scr#{}) & (v^.d.visible) THEN
    sbmd:=o^.desc;
    IF sbmd=NIL THEN RETURN END;
    req.op :=_bblt;      req.tool :=ADR(dt);
    req.arg:=sbmd;       req.stool:=ADR(st);
    req.x0 :=x;          req.block:=ADR(blk);
    req.y0 :=y;          redraw(v,req)
  ELSE
    done:=FALSE; error:=err.unsuitable
  END
END bblt;

PROCEDURE scroll(wnd: WINDOW; VAL t: TOOL; x,y: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.scroll(bmd,t,x,y);
    IF v^.d.mode*scr={} THEN RETURN END;
    WITH t.clip DO refreshtool(v,t,x,y,w,h) END
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_scroll;    req.tool:=ADR(t);
    req.x0:=x;          req.y0  :=y;
    redraw(v,req)
  END
END scroll;

PROCEDURE circle(wnd: WINDOW; VAL t: TOOL; x0,y0,r: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.circle(bmd,t,x0,y0,r)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_circle;  req.tool:=ADR(t);
    req.x0:=x0;       req.y0  :=y0;
    req.r :=r;        redraw(v,req)
  END
END circle;

PROCEDURE circlef(wnd: WINDOW; VAL t: TOOL; x0,y0,r: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.circlef(bmd,t,x0,y0,r)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_circlef;   req.tool:=ADR(t);
    req.x0:=x0;         req.y0  :=y0;
    req.r :=r;          redraw(v,req)
  END
END circlef;

PROCEDURE trif(wnd: WINDOW; VAL t: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.trif(bmd,t,x0,y0,x1,y1,x2,y2)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_trif;      req.tool:=ADR(t);
    req.x0:=x0;         req.y0  :=y0;
    req.x1:=x1;         req.y1  :=y1;
    req.x2:=x2;         req.y2  :=y2;
    redraw(v,req)
  END
END trif;

PROCEDURE arc(wnd: WINDOW; VAL t: TOOL; xc,yc,x1,y1,x2,y2,r: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.arc(bmd,t,xc,yc,x1,y1,x2,y2,r)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_arc;       req.tool:=ADR(t);
    req.x0:=xc;         req.y0  :=yc;
    req.x1:=x1;         req.y1  :=y1;
    req.x2:=x2;         req.y2  :=y2;
    req.r :=r ;         redraw(v,req)
  END
END arc;

PROCEDURE arc3(wnd: WINDOW; VAL t: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.arc3(bmd,t,x0,y0,x1,y1,x2,y2)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_arc3;      req.tool:=ADR(t);
    req.x0:=x0;         req.y0  :=y0;
    req.x1:=x1;         req.y1  :=y1;
    req.x2:=x2;         req.y2  :=y2;
    redraw(v,req)
  END
END arc3;

PROCEDURE ring(wnd: WINDOW; VAL t: TOOL; xc,yc,r0,r1: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.ring(bmd,t,xc,yc,r0,r1)
  END;
  IF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op:=_ring;      req.tool:=ADR(t);
    req.x0:=xc;         req.y0  :=yc;
    req.x1:=r0;         req.y1  :=r1;
    redraw(v,req)
  END
END ring;

PROCEDURE xwrite(wnd: WINDOW; VAL t: TOOL; x,y: INTEGER;
                font: fnt.FONT;
             VAL str: ARRAY OF CHAR;   pos,len: INTEGER): INTEGER;
  VAR v: Window;
     xr: INTEGER;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN x END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.desc;
    IF bmd=NIL THEN RETURN x END;
    xr:=bmg.xwrite(bmd,t,x,y,font,str,pos,len);
    IF v^.d.mode*scr#{} THEN refreshtool(v,t,x,y-font^.bline,xr-x+1,font^.H) END
  ELSIF (v^.d.visible) & (v^.d.mode*scr#{}) THEN
    req.op :=_xwrite;    req.tool:=ADR(t);
    req.x0 :=x;          req.y0  :=y;
    req.x1 :=pos;        req.y1  :=len;
    req.arg:=ADR(str);   req.font:=font;
    redraw(v,req);       xr:=req.x0
  END;
  RETURN xr
END xwrite;

TYPE
  STRPTR  = POINTER TO STRREC;
  STRREC  = RECORD
              wnd : WINDOW;
              x,y : INTEGER;
              fnt : FONT;
              tool: POINTER TO TOOL;
            END;

PROCEDURE pwrite(link: WORD; VAL str: ARRAY OF CHAR; pos,len: INTEGER);
  VAR p: STRPTR;
BEGIN
  p:=link;  p^.x:=xwrite(p^.wnd,p^.tool^,p^.x,p^.y,p^.fnt,str,pos,len)
END pwrite;

PROCEDURE xprint(wnd: WINDOW; VAL tool: TOOL; x,y: INTEGER; font: FONT;
          VAL format: ARRAY OF CHAR; SEQ arg: WORD): INTEGER;
  VAR str: STRREC;
BEGIN
  str.wnd:=wnd; str.x:=x; str.y:=y; str.fnt:=font; str.tool:=ADR(tool);
  fmt.format(ADR(str),pwrite,format,arg);
  RETURN str.x
END xprint;

PROCEDURE print(wnd: WINDOW; VAL t: TOOL; x,y: INTEGER;
               font: fnt.FONT;
            VAL fmt: ARRAY OF CHAR;  SEQ  arg: WORD);
BEGIN
  x:=xprint(wnd,t,x,y,font,fmt,arg)
END print;

PROCEDURE write(wnd: WINDOW; VAL t: TOOL; x,y: INTEGER;
               font: fnt.FONT;
            VAL str: ARRAY OF CHAR;   pos,len: INTEGER);
BEGIN
  x:=xwrite(wnd,t,x,y,font,str,pos,len)
END write;

PROCEDURE writech(wnd: WINDOW; VAL t: TOOL; x,y: INTEGER;
                 font: fnt.FONT;             ch:  CHAR);
  VAR ptr: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  ptr:=ADR(ch);
  x:=xwrite(wnd,t,x,y,font,ptr^,0,1)
END writech;

PROCEDURE lenght(font: fnt.FONT; VAL f: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
BEGIN
  RETURN bmg.lenght(font,f,a)
END lenght;

PROCEDURE width (font: fnt.FONT; VAL f: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
BEGIN
  RETURN bmg.width(font,f,a)
END width;

PROCEDURE margin(font: fnt.FONT; VAL f: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
BEGIN
  RETURN bmg.margin(font,f,a)
END margin;

VAR dsk: WINDOW;

BEGIN
fnt.unpack(fnt.font);
BUG:=FALSE;
  psc.loophole(psc.bitmap,B);
  IF NOT psc.done THEN HALT(psc.error) END;
  NEW(_fill);
  IF NOT mem.done THEN HALT(mem.error) ELSE low.fill(_fill^,-1) END;
  NEW(_zero);
  IF NOT mem.done THEN HALT(mem.error) ELSE low.zero(_zero^) END;
  done  :=TRUE;
  error :=err.ok;
  MAGIC :=_MAGIC;
  scrW  :=psc.state^.W;
  scrH  :=psc.state^.H;
  scrM  :=B^.mask;
  WITH T DO
    mask:=B^.mask; color:=mask;    zX:=0;
    mode:=bmg.rep; back :={};      zY:=0;
    clip.x:=0;     clip.w:=psc.state^.W;
    clip.y:=0;     clip.h:=psc.state^.H
  END;
  desktop:=NIL;
  DESK   :=NIL;
  create (desktop,NIL,0,0,scrW,scrH,scrM,{},_ground);
  DESK   :=Window(desktop);
  DESK^.d.closed:=FALSE;
  DESK^.out.closed:=FALSE;
  opentree(DESK);
  draw(DESK,DESK,0,0,scrW,scrH)
END pmWnd.

(*
xt:=T; xt.mode:=bmg.xor; xt.mask:={3};
bmg.frame(B,xt,w^.d.sx+cx,w^.d.sy+cy,w^.d.sx+cx+cw,w^.d.sy+cy+ch);
key.read(bug);
bmg.frame(B,xt,w^.d.sx+cx,w^.d.sy+cy,w^.d.sx+cx+cw,w^.d.sy+cy+ch);
*)


RESIZE 0,0 -> rdc !!!
RESIZER (move: BOOLEAN,x,y);
