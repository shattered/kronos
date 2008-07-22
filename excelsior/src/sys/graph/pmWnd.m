IMPLEMENTATION MODULE pmWnd; (*$+16 Leo 09-Apr-91. (c) KRONOS *)
(* $N+ *)

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

IMPORT key: Keyboard; VAR bug: CHAR; -- must be discarded after debug;

VAR MAGIC: INTEGER;             CONST _MAGIC = 444E57h;

TYPE

  BITMAP  = bmg.BITMAP;

  WORD    = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

  REQPTR  = POINTER TO REQUEST;

  Window  = POINTER TO WnDesc;

  WnDesc  = RECORD
              out,d: WNDESC;
              magic: INTEGER;
              up,dw: Window;
              actor: PAINT;
              req  : REQPTR
            END;

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
  desk: Window;
  T: TOOL;      _fill: LINE;
  B: BITMAP;    _zero: LINE;

PROCEDURE bmv(des,des_ofs,sou,sou_ofs,nobits: ADDRESS);
CODE cod.bmv END bmv;

PROCEDURE _move(a,b,c: ADDRESS); CODE cod.move END _move;

PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
CODE
  cod.li1 cod.sub cod.swap
  cod.li1 cod.sub cod.swap
  cod.bmg cod.bmg_inrect
(*RETURN NOT ((x<0) OR (y<0) OR (x>=w) OR (y>=h))*)
END in_rect;

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

PROCEDURE bad_parm; BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;
PROCEDURE bad_desc; BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;

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


---------------------------- windows ---------------------------
                            ---------


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
  bmd    :=w^.d.desc;
  sx:=cx+w^.d.x;
  sy:=cy+w^.d.y;
  IF bmd=NIL THEN RETURN END;
  dw:=cw;  dx:=cx+w^.d.x;
  dh:=ch;  dy:=cy+w^.d.y;
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

PROCEDURE paint(wnd: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR w: Window;            dx,dy,dh,dw: INTEGER;
    bmd: BITMAP;            so,do      : INTEGER;
   mask: BITSET;            sb,db      : ADDRESS;
BEGIN
  w:=Window(wnd);
  bmd:=w^.d.desc;
  IF bmd=NIL THEN RETURN END;
  dw:=cw;  dx:=cx+w^.d.x;
  dh:=ch;  dy:=cy+w^.d.y;
  IF dx<0           THEN dw:=dw+dx; cx:=cx-dx; dx:=0 END;
  IF dy<0           THEN dh:=dh+dy; cy:=cy-dy; dy:=0 END;
  IF dx+dw>T.clip.w THEN dw:=T.clip.w-dx END;
  IF dy+dh>T.clip.h THEN dh:=T.clip.h-dy END;
  IF (dw<=0) OR (dh<=0) THEN RETURN END;
  so:=(bmd^.H-cy-dh)*bmd^.WPL;
  do:=(B^  .H-dy-dh)*B^.  WPL;
  REPEAT
    mask:=w^.d.fore*B^.mask;  db:=ADR(B^.layers[0]);  sb:=ADR(bmd^.layers[0]);
    REPEAT
      IF mask*{0}#{} THEN
        mask:=mask-{0}; bmv(INTEGER(db^)+do,dx,INTEGER(sb^)+so,cx,dw)
      END;
      mask:=mask>>1; INC(sb); INC(db)
    UNTIL mask={};
    INC(so,bmd^.WPL); INC(do,B^.WPL); DEC(dh)
  UNTIL dh=0
END paint;

PROCEDURE paintwithboard(wnd: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR w: Window;
    bmd: BITMAP;       dx,dy: INTEGER;
   mask: BITSET;       dh,dw: INTEGER;
   back: BITSET;       so,do: INTEGER;
   fore: BITSET;       sb,db: ADDRESS;
BEGIN
  w:=Window(wnd);
  bmd:=w^.d.desc;
  IF bmd=NIL THEN RETURN END;
  dw:=cw;  dx:=cx+w^.d.x;
  dh:=ch;  dy:=cy+w^.d.y;
  IF dx<0           THEN dw:=dw+dx; cx:=cx-dx; dx:=0 END;
  IF dy<0           THEN dh:=dh+dy; cy:=cy-dy; dy:=0 END;
  IF dx+dw>T.clip.w THEN dw:=T.clip.w-dx END;
  IF dy+dh>T.clip.h THEN dh:=T.clip.h-dy END;
  IF (dw<=0) OR (dh<=0) THEN RETURN END;
  so:=(bmd^.H-cy-dh)*bmd^.WPL;
  do:=(B^  .H-dy-dh)*B^.  WPL;
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
END paintwithboard;

PROCEDURE draw(w: Window; cx,cy,cw,ch: INTEGER);
BEGIN
  IF cx<0            THEN cw:=cw+cx; cx:=0 END;
  IF cy<0            THEN ch:=ch+cy; cy:=0 END;
  IF cx+cw>w^.d.w    THEN cw:=w^.d.w-cx    END;
  IF cy+ch>w^.d.h    THEN ch:=w^.d.h-cy    END;
  IF (cw>0) & (ch>0) THEN w^.actor(WINDOW(w),cx,cy,cw,ch) END
END draw;

PROCEDURE clear(cx,cy,cw,ch: INTEGER);
BEGIN
  IF cx<0 THEN INC(cw,cx); cx:=0 END;
  IF cy<0 THEN INC(ch,cy); cy:=0 END;
  IF cx+cw>T.clip.w  THEN cw:=T.clip.w-cx END;
  IF cy+ch>T.clip.h  THEN ch:=T.clip.h-cy END;
  IF (cw>0) & (ch>0) THEN desk^.d.board(desktop,cx,cy,cw,ch) END
END clear;

PROCEDURE _ground(w: WINDOW; cx,cy,cw,ch: INTEGER);
  VAR t: TOOL;
BEGIN
  t:=T;
  t.mode  :=bmg.bic;
  t.color :=t.mask;
  bmg.rect(B,t,cx,cy,cx+cw-1,cy+ch-1)
END _ground;

----------------------------------------------------------------

PROCEDURE tieontop(w: Window);
  VAR t,b: Window;
BEGIN
  IF top=NIL THEN top:=WINDOW(w); bottom:=top; w^.up:=w; w^.dw:=w; RETURN END;
  t:=Window(top);     t^.up:=w;  w^.dw:=t;
  b:=desk;            b^.dw:=w;  w^.up:=b;   top:=WINDOW(w);
  IF bottom=desktop THEN bottom:=top END
END tieontop;

PROCEDURE tieonbot(w: Window);
  VAR t,b: Window;
BEGIN
  IF top=NIL THEN top:=WINDOW(w); bottom:=top; w^.up:=w; w^.dw:=w; RETURN END;
  t:=desk;            t^.up:=w;  w^.dw:=t;
  b:=Window(bottom);  b^.dw:=w;  w^.up:=b;   bottom:=WINDOW(w);
  IF top=desktop THEN top:=bottom END
END tieonbot;

PROCEDURE untie(w: Window);
  VAR t,b: Window;
BEGIN
  ASSERT(top#bottom); -- ????
  t:=Window(top);
  b:=Window(bottom);
  IF t=w THEN top   :=WINDOW(t^.dw) END;
  IF b=w THEN bottom:=WINDOW(b^.up) END;
  w^.up^.dw:=w^.dw;
  w^.dw^.up:=w^.up;
END untie;

PROCEDURE tieover(w,u: Window);  (* "w" over "u" *)
  VAR t,b: Window;
BEGIN
  t:=Window(top);
  b:=Window(bottom);
  IF u=t THEN top:=WINDOW(w) END;
  w^.up:=u^.up;
  w^.dw:=u;
  w^.up^.dw:=w;
  w^.dw^.up:=w;
  ASSERT(top#bottom)
END tieover;

PROCEDURE tieunder(w,u: Window);  (* "w" under "u" *)
  VAR t,b: Window;
BEGIN
  t:=Window(top);
  b:=Window(bottom);
  IF u=b THEN bottom:=WINDOW(w) END;
  w^.up:=u;
  w^.dw:=u^.dw;
  w^.up^.dw:=w;
  w^.dw^.up:=w;
  ASSERT(top#bottom)
END tieunder;


PROCEDURE disposebmd(w: Window);
  VAR i: INTEGER;
    bmd: BITMAP;
BEGIN
  IF w^.d.desc=NIL THEN RETURN END;
  bmd:=w^.d.desc;
  FOR i:=0 TO HIGH(bmd^.layers) DO
    IF bmd^.layers[i]#NIL THEN
      mem.deallocate(bmd^.layers[i],bmd^.WPL*bmd^.H)
    END
  END;
  bmd^.mask:={}; DISPOSE(bmd); w^.d.desc:=NIL; w^.out:=w^.d
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
    INC(bmd^.W,w^.d.x MOD 32);
    FOR i:=0 TO HIGH(bmd^.layers) DO
      IF i IN bmd^.mask THEN
        allocate(bmd^.layers[i],bmd^.H*bmd^.WPL);
        IF NOT done THEN dispose(bmd); RETURN END;
        IF bmd^.layers[i]=NIL THEN EXCL(bmd^.mask,i)
        ELSE low._zero(bmd^.layers[i],bmd^.H*bmd^.WPL)
        END
      END
    END;
  ELSE
    bmd^.mask:={}
  END;
  w^.d.desc:=bmd;  w^.out:=w^.d
END newbmd;

PROCEDURE new(VAR wnd: WINDOW);
  VAR w: Window;
BEGIN
  NEW(w);
  IF NOT done THEN wnd:=NIL; RETURN END;
  w^.magic :=0;
  w^.d.desc:=NIL;
  wnd:=WINDOW(w);
  newbmd(w,0,0);
  IF NOT done THEN DISPOSE(w); RETURN END;
  WITH w^ DO
    d.full:=T;
    d.full.clip.w:=0;     d.x:=0; d.w:=0;
    d.full.clip.h:=0;     d.y:=0; d.h:=0;
    d.inner:=d.full;      d.image :=TRUE;
    d.back :={};          d.fore  :=B^.mask;
    d.mask :=d.fore;      d.mode  :=normal;
    d.paint:=paint;       d.board :=paintwithboard;
    d.obj  :=NIL;         d.closed:=TRUE;
    d.mgr  :=NIL;
    out    :=d;

    req    :=NIL;
    actor  :=paintwithboard;
    magic  :=MAGIC
  END;
  tieontop(w);  done:=TRUE
END new;

PROCEDURE dispose(VAR wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) OR (w=desk) THEN RETURN END;
  IF NOT w^.d.closed THEN close(wnd) END;
  disposebmd(w); untie(w);
  w^.magic:=0; DISPOSE(w); wnd:=NIL
END dispose;

PROCEDURE resizebmd(w: Window; W,H: INTEGER);

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
    sou:=old.layers[i]+(old.H-1)*old.WPL;
    des:=bmd.layers[i]+(bmd.H-1)*bmd.WPL;
    w:=bmd.W MOD 32;
    IF w=0 THEN mask:={} ELSE mask:={w..31} END;
    w:=bmd.WPL-len;
    REPEAT
      _move(des,sou,len);  adr:=des+len-1;      adr^:=BITSET(adr^)-mask;
      DEC(des,bmd.WPL);    DEC(sou,old.WPL);    DEC(h)
    UNTIL h=0
  END copylayer;

  VAR i,j: INTEGER;
      bmd: bmg.BMD;
      old: BITMAP;
BEGIN
  old:=w^.d.desc;
  IF old=NIL THEN newbmd(w,W,H); RETURN END;
  bmd.W:=W;  bmd.mask:=w^.d.fore;     bmd.BASE:=NIL;
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


PROCEDURE notcross(u: Window; cx,cy,cw,ch: INTEGER): BOOLEAN;
BEGIN
  RETURN (cx+cw<=u^.d.x) OR (u^.d.x+u^.d.w<=cx)
      OR (cy+ch<=u^.d.y) OR (u^.d.y+u^.d.h<=cy)
END notcross;

PROCEDURE _refresh(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (ch<=0) OR (cw<=0) THEN (* empty *) RETURN END;

  IF u=desk THEN (* no one window upper then "w" in (cx,cy,cw,ch) *)
    draw(w,cx-w^.d.x,cy-w^.d.y,cw,ch); RETURN
  END;

  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _refresh(w,u^.up,cx,cy,cw,ch); RETURN
  END;

  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy); _refresh(w,u^.up,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1); _refresh(w,u^.up,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx); _refresh(w,u^.up,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1); _refresh(w,u^.up,cx+i,cy,cw-i,ch); cw:=i
  END
END _refresh;

PROCEDURE _cleardw(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
(*
bmg.print(B,T,0,0,fnt.font,"_cleardw(%08h,%08h,%d,%d,%d,%d)   ",w,u,cx,cy,cw,ch);
key.read(bug);
bmg.print(B,T,0,0,fnt.font,"                                             ");
*)
  IF (ch<=0) OR (cw<=0) THEN (* empty *) RETURN END;
  IF u=desk THEN
    clear(cx,cy,cw,ch); RETURN
  END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _cleardw(w,u^.dw,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy); _cleardw(w,u^.dw,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1); _cleardw(w,u^.dw,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx); _cleardw(w,u^.dw,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1); _cleardw(w,u^.dw,cx+i,cy,cw-i,ch); cw:=i
  END;
  draw(u,cx-u^.d.x,cy-u^.d.y,cw,ch)
END _cleardw;

PROCEDURE _clear(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (ch<=0) OR (cw<=0) THEN (* empty *) RETURN END;
  IF u=desk THEN
    _cleardw(w,w^.dw,cx,cy,cw,ch); RETURN
  END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _clear(w,u^.up,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy); _clear(w,u^.up,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1); _clear(w,u^.up,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx); _clear(w,u^.up,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1); _clear(w,u^.up,cx+i,cy,cw-i,ch); cw:=i
  END
END _clear;

PROCEDURE _ontop(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (ch<=0) OR (cw<=0) OR (u=desk) THEN (* empty *) RETURN END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _ontop(w,u^.up,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy);
    _ontop(w,u^.up,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1);
    _ontop(w,u^.up,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx);
    _ontop(w,u^.up,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1);
    _ontop(w,u^.up,cx+i,cy,cw-i,ch); cw:=i
  END;
  draw(w,cx-w^.d.x,cy-w^.d.y,cw,ch)
END _ontop;

PROCEDURE _onbotdw(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
--bmg.print(B,T,0,340,fnt.font,"_onbotdw(%d,%d,%d,%d) 0     ",cx,cy,cw,ch);
  IF (ch<=0) OR (cw<=0) OR (u=desk) THEN RETURN END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _onbotdw(w,u^.dw,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy);
    _onbotdw(w,u^.dw,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1);
    _onbotdw(w,u^.dw,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx);
    _onbotdw(w,u^.dw,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1);
    _onbotdw(w,u^.dw,cx+i,cy,cw-i,ch); cw:=i
  END;
--bmg.print(B,T,0,320,fnt.font,"_onbotdw(%d,%d,%d,%d) 1     ",cx,cy,cw,ch);
  draw(u,cx-u^.d.x,cy-u^.d.y,cw,ch)
END _onbotdw;

PROCEDURE _onbottom(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (ch<=0) OR (cw<=0) THEN RETURN END;
  IF u=desk THEN
    _onbotdw(w,w^.dw,cx,cy,cw,ch); RETURN
  END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _onbottom(w,u^.up,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy);
    _onbottom(w,u^.up,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1);
    _onbottom(w,u^.up,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx);
    _onbottom(w,u^.up,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1);
    _onbottom(w,u^.up,cx+i,cy,cw-i,ch); cw:=i
  END
END _onbottom;

PROCEDURE _emerge(w,u,t: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (ch<=0) OR (cw<=0) OR (u=w) THEN RETURN END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _emerge(w,u^.dw,t,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy);
    _emerge(w,u^.dw,t,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1);
    _emerge(w,u^.dw,t,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx);
    _emerge(w,u^.dw,t,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1);
    _emerge(w,u^.dw,t,cx+i,cy,cw-i,ch); cw:=i
  END;
  _refresh(w,t,cx,cy,cw,ch)
END _emerge;

PROCEDURE _sink(w,u: Window; cx,cy,cw,ch: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (ch<=0) OR (cw<=0) OR (u=w) THEN RETURN END;
  IF u^.d.closed OR notcross(u,cx,cy,cw,ch) THEN
    _sink(w,u^.dw,cx,cy,cw,ch); RETURN
  END;
  IF cy <= u^.d.y THEN
    i:=min(ch,u^.d.y-cy);
    _sink(w,u^.dw,cx,cy,cw,i); cy:=cy+i; ch:=ch-i
  END;
  IF u^.d.y <= cy THEN
    i:=min(ch,(u^.d.y+u^.d.h-1)-cy+1);
    _sink(w,u^.dw,cx,cy+i,cw,ch-i); ch:=i
  END;
  IF cx <= u^.d.x THEN
    i:=min(cw,u^.d.x-cx);
    _sink(w,u^.dw,cx,cy,i,ch);  cx:=cx+i; cw:=cw-i
  END;
  IF u^.d.x <= cx THEN
    i:=min(cw,(u^.d.x+u^.d.w-1)-cx+1);
    _sink(w,u^.dw,cx+i,cy,cw-i,ch); cw:=i
  END;
  _refresh(u,u^.up,cx,cy,cw,ch)
END _sink;

PROCEDURE open(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF NOT w^.d.closed THEN RETURN END;
  w^.d.closed:=FALSE;
  w^.out:=w^.d;
  _refresh(w,w^.up,w^.d.x,w^.d.y,w^.d.w,w^.d.h)
END open;

PROCEDURE close(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF w^.d.closed THEN RETURN END;
  w^.d.closed:=TRUE;
  w^.out:=w^.d;
  _clear(w,w^.up,w^.d.x,w^.d.y,w^.d.w,w^.d.h)
END close;

PROCEDURE move(wnd: WINDOW; X,Y: INTEGER);
  VAR w: Window; ox,oy,ow,oh,dx,dy,y1: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF (w^.d.x=X) & (w^.d.y=Y) THEN RETURN END;
  ox:=w^.d.x;  ow:=w^.d.w;  dx:=X-ox;
  oy:=w^.d.y;  oh:=w^.d.h;  dy:=Y-oy;
  w^.d.x:=X;   w^.out.x:=X;
  w^.d.y:=Y;   w^.out.y:=Y;
  IF w^.d.closed  THEN RETURN END;
  _refresh(w, w^.up, w^.d.x, w^.d.y, w^.d.w, w^.d.h);
  w^.d.closed:=TRUE;
  IF notcross(w,ox,oy,ow,oh) THEN
    _clear(w, w^.up, ox,oy,ow,oh)
  ELSE
    IF Y<oy THEN
      _clear(w, w^.up, ox, Y+oh, ow, ABS(dy) )
    ELSE
      _clear(w, w^.up, ox, oy, ow, ABS(dy));  oy:=Y
    END;
    IF X<ox THEN ox:=X+ow END;
    _clear(w, w^.up, ox, oy,  ABS(dx), oh-ABS(dy) )
  END;
  w^.d.closed:=FALSE
END move;

PROCEDURE resize(wnd: WINDOW; W,H: INTEGER);
  VAR w: Window;  dh,dw,ow,oh,id,iu,il,ir: INTEGER;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END;
  IF W>4096      THEN done:=FALSE; error:=err.too_large  END;
  done:=TRUE;
  IF H<0 THEN H:=0 END;
  IF W<0 THEN W:=0 END;
  IF (w^.d.w=W) & (w^.d.h=H) THEN RETURN END;
  ow:=w^.d.w;  dw:=W-ow;
  oh:=w^.d.h;  dh:=H-oh;
  IF NOT w^.d.closed THEN
    w^.d.closed:=TRUE;
    _clear(w, w^.up, w^.d.x  , w^.d.y+H,  ow, -dh      );
    _clear(w, w^.up, w^.d.x+W, w^.d.y  , -dw, min(oh,H));
    w^.d.closed:=FALSE
  END;
  IF w^.d.image THEN
    resizebmd(w,W,H);
    IF NOT done THEN RETURN END
  END;
  WITH w^.d.inner DO
    il:=zX; ir:=w^.d.w-il-clip.w;
    id:=zY; iu:=w^.d.h-id-clip.h
  END;
  w^.d.w:=W;  w^.d.full.clip.w:=W;  INC(w^.d.inner.clip.w,dw);
  w^.d.h:=H;  w^.d.full.clip.h:=H;  INC(w^.d.inner.clip.h,dh);
  w^.out:=w^.d;
  IF NOT w^.d.closed THEN
    _refresh(w, w^.up, w^.d.x   , w^.d.y+oh-iu, max(ow,W), dh       );
    _refresh(w, w^.up, w^.d.x+ow-ir, w^.d.y   , dw       , min(oh,H));


    _refresh(w, w^.up, w^.d.x, w^.d.y     , W, id);
    _refresh(w, w^.up, w^.d.x, w^.d.y+H-iu, W, iu);

    _refresh(w, w^.up, w^.d.x,      w^.d.y+id,il,H-iu-id);
    _refresh(w, w^.up, w^.d.x+W-ir, w^.d.y+id,ir,H-iu-id);
  END
END resize;

PROCEDURE mask(wnd: WINDOW; fore,back: BITSET);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  fore:=fore*B^.mask;
  back:=(back-fore)*B^.mask;
  IF (fore=w^.d.fore) & (back=w^.d.back) THEN RETURN END;
  w^.d.fore:=fore*B^.mask;
  w^.d.back:=(back-fore)*B^.mask;
  IF w^.d.image THEN newbmd(w,w^.d.w,w^.d.h) END;
  w^.out:=w^.d;
  IF done & NOT w^.d.closed THEN
    _refresh(w,w^.up,w^.d.x,w^.d.y,w^.d.w,w^.d.h)
  END
END mask;

PROCEDURE image(wnd: WINDOW; on: BOOLEAN);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF w^.d.image=on THEN RETURN END;
  IF on THEN
    newbmd(w,w^.d.w,w^.d.h);
    IF NOT done THEN RETURN END;
    w^.d.paint:=paint;
  ELSE
    disposebmd(w)
  END;
  w^.d.image:=on; w^.out:=w^.d
END image;

PROCEDURE painter(wnd: WINDOW; p: PAINT);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE; w^.d.paint:=p; w^.out.paint:=p
END painter;

PROCEDURE boarder(wnd: WINDOW; p: PAINT);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE; w^.d.board:=p; w^.out.board:=p; w^.actor:=w^.d.board
END boarder;

PROCEDURE object(wnd: WINDOW; obj: SYSTEM.WORD);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE; w^.d.obj:=obj; w^.out.obj:=obj
END object;

PROCEDURE manager(wnd: WINDOW; mgr: SYSTEM.WORD);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END; -- ???
  done:=TRUE; w^.d.mgr:=mgr; w^.out.mgr:=mgr
END manager;

PROCEDURE inner(wnd: WINDOW; X,Y,W,H: INTEGER);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END;
  IF X<0 THEN INC(W,X); X:=0 END;
  IF Y<0 THEN INC(H,Y); Y:=0 END;
  w^.d.inner.clip:=w^.d.full.clip;
  WITH w^.d.inner.clip DO
    x:=0; y:=0;
    IF X+W>w THEN w:=w-X ELSE w:=W END;
    IF Y+H>h THEN h:=h-Y ELSE h:=H END
  END;
  w^.d.inner.zX:=X;
  w^.d.inner.zY:=Y;
  w^.out:=w^.d; done:=TRUE
END inner;

PROCEDURE ontop(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF w=Window(top) THEN RETURN END;
  IF NOT w^.d.closed THEN
    _ontop(w,w^.up,w^.d.x,w^.d.y,w^.d.w,w^.d.h)
  END;
  (* there is at least one other window in the ring now, *)
  (* cause w#Window(top) see below *)
  untie(w); tieontop(w)
END ontop;

PROCEDURE onbottom(wnd: WINDOW);
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF w=desk THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF w=Window(bottom) THEN RETURN END;
  IF NOT w^.d.closed  THEN
    _onbottom(w,w^.up,w^.d.x,w^.d.y,w^.d.w,w^.d.h)
  END;
  untie(w); tieonbot(w)
END onbottom;

PROCEDURE upperthen(wnd,und: WINDOW): BOOLEAN;
  VAR w,u,v: Window;
BEGIN
  w:=Window(wnd);
  u:=Window(und);
  IF u=w THEN RETURN FALSE END;
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN FALSE END;
  IF (u=NIL) OR (u^.magic#MAGIC) THEN RETURN TRUE  END;
  IF  w=desk THEN RETURN FALSE END;
  v:=w^.dw;
  WHILE v#Window(top) DO
    IF v=u THEN RETURN TRUE END;
    v:=v^.dw
  END;
  RETURN FALSE
END upperthen;

PROCEDURE putunder(wnd,und: WINDOW);
  VAR w,u,dw: Window;
  PROCEDURE retie; BEGIN untie(w); tieunder(w,u) END retie;
BEGIN
  w:=Window(wnd);
  u:=Window(und);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (u=NIL) OR (u^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (w=desk) OR (u=desk)        THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF (w=u) OR (w^.up=u) THEN RETURN END;
  IF w^.d.closed  THEN retie; RETURN END;
  IF upperthen(wnd,und) THEN
    dw:=w^.dw; retie;
    _sink  (w,dw,      w^.d.x,w^.d.y,w^.d.w,w^.d.h)
  ELSE
    _emerge(w,u^.dw,u, w^.d.x,w^.d.y,w^.d.w,w^.d.h);
    retie
  END
END putunder;

PROCEDURE putover(wnd,und: WINDOW);
  VAR w,u,dw: Window;
  PROCEDURE retie; BEGIN untie(w); tieover(w,u) END retie;
BEGIN
  IF und=top     THEN ontop(wnd);    RETURN END;
  IF und=desktop THEN onbottom(wnd); RETURN END;
  w:=Window(wnd);
  u:=Window(und);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (u=NIL) OR (u^.magic#MAGIC) THEN bad_desc; RETURN   END;
  IF (w=desk)                    THEN done:=FALSE; error:=err.unsuitable END;
  done:=TRUE;
  IF (w=u) OR (w^.dw=u) THEN RETURN END;
  IF w^.d.closed  THEN retie; RETURN END;
  IF upperthen(wnd,und) THEN
    dw:=w^.dw; retie;
    _sink  (w,dw,      w^.d.x,w^.d.y,w^.d.w,w^.d.h)
  ELSE
    _emerge(w,u,u^.up, w^.d.x,w^.d.y,w^.d.w,w^.d.h);
    retie
  END
END putover;

PROCEDURE refreshboard(wnd: WINDOW; X,Y,W,H: INTEGER); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF w^.d.closed OR (W<=0) OR (H<=0) THEN RETURN END;
  _refresh(w,w^.up,w^.d.x+X,w^.d.y+Y,W,H)
END refreshboard;

PROCEDURE refreshbox(wnd: WINDOW; X,Y,W,H: INTEGER); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF w^.d.closed OR (W<=0) OR (H<=0) THEN RETURN END;
  w^.actor:=w^.d.paint;
    _refresh(w,w^.up,w^.d.x+X,w^.d.y+Y,W,H);
  w^.actor:=w^.d.board;
END refreshbox;

PROCEDURE refresh(wnd: WINDOW); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  WITH w^.d.inner DO refreshbox(wnd,zX,zY,clip.w,clip.h) END
END refresh;

PROCEDURE refreshall; (* all layers refreshed *)
  VAR w: Window;
BEGIN
  done:=TRUE;
  w:=Window(top);
  IF w=NIL THEN RETURN END;
  REPEAT
    IF NOT w^.d.closed THEN _refresh(w,w^.up,w^.d.x,w^.d.y,w^.d.w,w^.d.h) END;
    w:=w^.dw
  UNTIL w=Window(top)
END refreshall;

PROCEDURE savebox(wnd: WINDOW; X,Y,W,H: INTEGER); (* only fore refreshed *)
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF w^.d.closed OR (W<=0) OR (H<=0) OR NOT w^.d.image THEN RETURN END;
  w^.actor:=save;
  _refresh(w,w^.up,w^.d.x+X,w^.d.y+Y,W,H);
  w^.actor:=w^.d.board
END savebox;

PROCEDURE locate(x,y: INTEGER): WINDOW;
  VAR w: Window;
BEGIN
  w:=Window(top);
  IF w=NIL THEN RETURN NIL END;
  REPEAT
    IF NOT w^.d.closed & in_rect(x-w^.d.x,y-w^.d.y,w^.d.w,w^.d.h) THEN
      --IF w=desk THEN RETURN NIL ELSE RETURN WINDOW(w) END ???
      RETURN WINDOW(w) -- ???
    END;
    w:=w^.dw
  UNTIL w=Window(top);
  ASSERT(FALSE);  (* cause of desktop *)
--RETURN NIL
END locate;

PROCEDURE up(wnd: WINDOW ): WINDOW;
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN NIL END;
  IF w=Window(top) THEN RETURN NIL END;
  RETURN WINDOW(w^.up)
END up;

PROCEDURE dw(wnd: WINDOW ): WINDOW;
  VAR w: Window;
BEGIN
  w:=Window(wnd);
  IF (w=NIL) OR (w^.magic#MAGIC) THEN RETURN NIL END;
  IF w=Window(bottom)  THEN RETURN NIL END;
  RETURN WINDOW(w^.dw)
END dw;

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
    INC(t.zX,v^.d.x);
    INC(t.zY,v^.d.y);
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
  VAR c: bmg.BLOCK;
BEGIN
  IF v^.d.closed OR (v^.d.mode*scr={}) OR (W<=0) OR (H<=0) THEN RETURN END;
  c.x:=X; c.y:=Y; c.w:=W; c.h:=H;
  bmg.cross(c,c,t.clip);
  v^.actor:=v^.d.paint;
  _refresh(v,v^.up,v^.d.x+t.zX+c.x,v^.d.y+t.zY+c.y,c.w,c.h);
  v^.actor:=v^.d.board
END refreshtool;

PROCEDURE redraw(v: Window; VAR req: REQUEST);
BEGIN
  v^.req:=ADR(req);
  v^.actor:=doit;
  WITH req.tool^.clip DO
    _refresh(v,v^.up,v^.d.x+x+req.tool^.zX,v^.d.y+y+req.tool^.zY,w,h)
  END;
  v^.actor:=v^.d.board
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.erase(bmd);
    IF v^.d.mode*scr={} THEN RETURN END;
    refresh(wnd)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.pattern(bmd,t,b,W,H,p);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,t,b.x,b.y,b.w,b.h)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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

PROCEDURE grid(wnd: WINDOW; VAL t: TOOL; VAL b: BLOCK; xstep,ystep: INTEGER);
  VAR v: Window;
    bmd: BITMAP;
    req: REQUEST;
BEGIN
  v:=Window(wnd);
  IF (v=NIL) OR (v^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF v^.d.image & (v^.d.mode*img#{}) THEN
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.grid(bmd,t,b,xstep,ystep);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,t,b.x,b.y,b.w,b.h)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.dot(bmd,t,x,y);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,t,x,y,1,1)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.line(bmd,t,x0,y0,x1,y1);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_line;    req.tool:=ADR(t);
  req.x0:=x0;       req.y0  :=y0;
  req.x1:=x1;       req.y1  :=y1;
  redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    i:=r;
    bmg.dline(bmd,t,x0,y0,x1,y1,r);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_dline;   req.tool:=ADR(t);
  req.x0:=x0;       req.y0  :=y0;
  req.x1:=x1;       req.y1  :=y1;
  req.r :=i;        redraw(v,req);      r:=req.r
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.line(bmd,t,x0,y0,x1,y0);
    IF v^.d.mode*scr={} THEN RETURN END;
    IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
    refreshtool(v,t,x0,y0,x1-x0+1,1)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.line(bmd,t,x0,y0,x0,y1);
    IF v^.d.mode*scr={} THEN RETURN END;
    IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
    refreshtool(v,t,x0,y0,1,y1-y0+1)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.rect(bmd,t,x0,y0,x1,y1);
    IF v^.d.mode*scr={} THEN RETURN END;
    IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
    IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
    refreshtool(v,t,x0,y0,x1-x0+1,y1-y0+1)
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
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
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    dbmd:=v^.d.desc;
    IF dbmd=NIL THEN RETURN END;
    sbmd:=o^.d.desc;
    IF sbmd=NIL THEN RETURN END;
    bmg.bblt(dbmd,dt,x,y,sbmd,st,blk);
    IF v^.d.mode*scr={} THEN RETURN END;
    refreshtool(v,dt,x,y,blk.w,blk.h)
  ELSIF (o^.d.image) & (v^.d.mode*scr#{}) THEN
    sbmd:=o^.d.desc;
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.scroll(bmd,t,x,y);
    IF v^.d.mode*scr={} THEN RETURN END;
    WITH t.clip DO refreshtool(v,t,x,y,w,h) END
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.circle(bmd,t,x0,y0,r);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_circle;  req.tool:=ADR(t);
  req.x0:=x0;       req.y0  :=y0;
  req.r :=r;        redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.circlef(bmd,t,x0,y0,r);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_circlef;   req.tool:=ADR(t);
  req.x0:=x0;         req.y0  :=y0;
  req.r :=r;          redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.trif(bmd,t,x0,y0,x1,y1,x2,y2);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_trif;      req.tool:=ADR(t);
  req.x0:=x0;         req.y0  :=y0;
  req.x1:=x1;         req.y1  :=y1;
  req.x2:=x2;         req.y2  :=y2;
  redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.arc(bmd,t,xc,yc,x1,y1,x2,y2,r);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_arc;       req.tool:=ADR(t);
  req.x0:=xc;         req.y0  :=yc;
  req.x1:=x1;         req.y1  :=y1;
  req.x2:=x2;         req.y2  :=y2;
  req.r :=r ;         redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.arc3(bmd,t,x0,y0,x1,y1,x2,y2);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_arc3;      req.tool:=ADR(t);
  req.x0:=x0;         req.y0  :=y0;
  req.x1:=x1;         req.y1  :=y1;
  req.x2:=x2;         req.y2  :=y2;
  redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN END;
    bmg.ring(bmd,t,xc,yc,r0,r1);
    IF v^.d.mode*scr={} THEN RETURN END
  END;
  IF v^.d.closed THEN RETURN END;
  req.op:=_ring;      req.tool:=ADR(t);
  req.x0:=xc;         req.y0  :=yc;
  req.x1:=r0;         req.y1  :=r1;
  redraw(v,req)
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
    bmd:=v^.d.desc;
    IF bmd=NIL THEN RETURN x END;
    xr:=bmg.xwrite(bmd,t,x,y,font,str,pos,len);
    IF v^.d.mode*scr#{} THEN refreshtool(v,t,x,y-font^.bline,xr-x+1,font^.H) END
  ELSIF NOT v^.d.closed & (v^.d.mode*scr#{}) THEN
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
  kind:=bitmap;
  psc.loophole(psc.bitmap,B);
  IF NOT psc.done THEN HALT(psc.error) END;
  NEW(_fill);
  IF NOT mem.done THEN HALT(mem.error) ELSE low.fill(_fill^,-1) END;
  NEW(_zero);
  IF NOT mem.done THEN HALT(mem.error) ELSE low.zero(_zero^) END;
  done  :=TRUE;
  error :=err.ok;
  MAGIC :=_MAGIC;
  top   :=NIL;
  bottom:=NIL;
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
  new    (dsk);
  image  (dsk,FALSE);
  mask   (dsk,scrM,{});
  resize (dsk,scrW,scrH);
  painter(dsk,_ground);
  boarder(dsk,_ground);
  open   (dsk);
  desktop:=dsk;
  desk:=Window(desktop)
END pmWnd.

(*
xt:=T; xt.mode:=bmg.xor;
bmg.frame(B,xt,w^.d.x+cx,w^.d.y+cy,w^.d.x+cx+cw,w^.d.y+cy+ch);
key.read(bug);
bmg.frame(B,xt,w^.d.x+cx,w^.d.y+cy,w^.d.x+cx+cw,w^.d.y+cy+ch);
*)
