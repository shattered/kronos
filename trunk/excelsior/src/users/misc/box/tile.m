IMPLEMENTATION MODULE tile; (* $W- Igo & Co 11-Oct-87. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  err: defErrors;
IMPORT  mem: Heap;

CONST TILESINSLOT = 1024;

VAR HEAD: TILE;

PROCEDURE newslot;
  VAR n: TILE; i: INTEGER;
BEGIN
  mem.allocate(HEAD,TILESINSLOT*SIZE(tile));
  done:=mem.done;
  IF NOT done THEN error:=mem.error; RETURN END;
  n:=HEAD;
  FOR i:=1 TO TILESINSLOT-1 DO
    n^.up:=SYSTEM.ADR(n^)+SIZE(tile); n:=n^.up
  END;
  n^.up:=NIL
END newslot;

PROCEDURE dealloc(VAR l: LAY; t: TILE);
BEGIN
  IF t=l^.head THEN l^.head:=t^.lf END;
  t^.up:=HEAD; HEAD:=t; t:=NIL
END dealloc;

PROCEDURE alloc(VAR t: TILE; type,x,y: INTEGER);
BEGIN
  IF HEAD=NIL THEN
    newslot; IF NOT done THEN t:=NIL; RETURN END
  END;
  t:=HEAD; HEAD:=HEAD^.up;
  t^.type:=type;        t^.x :=x;
  t^.y   :=y;           t^.up:=NIL;
  t^.dw  :=NIL;         t^.lf:=NIL;
  t^.rg  :=NIL
END alloc;

CONST magic    =  10;
      bndHight =  MAX(INTEGER) DIV 2 - magic*4;
      bndLow   = -bndHight;

PROCEDURE new(VAR l: LAY);

  VAR upB,dwB,lfB,rgB: TILE;

  PROCEDURE d;
  BEGIN
    dealloc(l,upB); dealloc(l,dwB); dealloc(l,lfB); dealloc(l,rgB);
    dealloc(l,l^.head^.up); dealloc(l,l^.head^.dw);
    dealloc(l,l^.head^.lf); dealloc(l,l^.head^.rg);
    dealloc(l,l^.head);
    mem.deallocate(l,SIZE(l^))
  END d;

  PROCEDURE init_boards;
  BEGIN
    upB:=NIL; dwB:=NIL; lfB:=NIL; rgB:=NIL;
    alloc(upB, bnd ,bndLow-magic,bndHight);     IF NOT done THEN RETURN END;
    alloc(dwB, bnd ,bndLow-magic,bndLow-magic); IF NOT done THEN RETURN END;
    alloc(lfB, bnd ,bndLow-magic,bndLow);       IF NOT done THEN RETURN END;
    alloc(rgB, bnd ,bndHight,bndLow);           IF NOT done THEN RETURN END;
    dwB^.up:=rgB;   upB^.dw:=lfB;
    lfB^.up:=upB;   lfB^.dw:=dwB;   lfB^.rg:=l^.head^.up;
    rgB^.up:=upB;   rgB^.dw:=dwB;   rgB^.lf:=l^.head^.dw;
  END init_boards;

BEGIN
  upB:=NIL; dwB:=NIL; lfB:=NIL; rgB:=NIL;
  mem.allocate(l,SIZE(l^)); done:=mem.done;
  IF NOT done THEN error:=mem.error; RETURN END;
  alloc(l^.head,spc,0,0);
  IF NOT done THEN mem.deallocate(l,SIZE(l^)); RETURN END;
  WITH l^.head^ DO
    up:=NIL; dw:=NIL; lf:=NIL; rg:=NIL;
    alloc(up, bnd ,bndLow,wsize);  IF NOT done THEN d; RETURN END;
    alloc(dw, bnd ,bndLow,bndLow); IF NOT done THEN d; RETURN END;
    alloc(lf, bnd ,bndLow,0);      IF NOT done THEN d; RETURN END;
    alloc(rg, bnd ,wsize,0);       IF NOT done THEN d; RETURN END;
    init_boards;                   IF NOT done THEN d; RETURN END;
    WITH up^ DO up:=upB; rg:=rgB; lf:=lfB; dw:=l^.head^.lf END;
    WITH dw^ DO dw:=dwB; lf:=lfB; rg:=rgB; up:=l^.head^.rg END;
    WITH lf^ DO
      dw:=l^.head^.dw; lf:=lfB; rg:=l^.head; up:=l^.head^.up
    END;
    WITH rg^ DO
      dw:=l^.head^.dw; rg:=rgB; lf:=l^.head; up:=l^.head^.up
    END
  END
END new;

PROCEDURE where(l: LAY; x,y: INTEGER): TILE;
  VAR t: TILE; find: BOOLEAN;
BEGIN t:=l^.head;
  REPEAT
    find:=TRUE;
    IF t^.x>x      THEN find:=FALSE; REPEAT t:=t^.lf UNTIL t^.x<=x    END;
    IF t^.y>y      THEN find:=FALSE; REPEAT t:=t^.dw UNTIL t^.y<=y    END;
    IF t^.rg^.x<=x THEN find:=FALSE; REPEAT t:=t^.rg UNTIL t^.rg^.x>x END;
    IF t^.up^.y<=y THEN find:=FALSE; REPEAT t:=t^.up UNTIL t^.up^.y>y END
  UNTIL find;
  l^.head:=t; RETURN t
END where;

PROCEDURE _dealloc0(t: TILE): BOOLEAN;
BEGIN
  t^.up:=HEAD; HEAD:=t; RETURN FALSE
END _dealloc0;

PROCEDURE dealloc0(t: TILE);
BEGIN t^.up:=HEAD; HEAD:=t END dealloc0;

PROCEDURE dispose(VAR l: LAY);
  VAR b: TILE;
BEGIN
  b:=where(l,-1,-1);
  iterate(l,0,0,wsize,wsize,_dealloc0);
  dealloc0(b^.dw);
  dealloc0(b^.rg);
  dealloc0(b^.up^.up^.up);
  dealloc0(b^.up^.up^.dw);
  dealloc0(b^.up^.up);
  dealloc0(b^.up);
  dealloc0(b^.lf);
  dealloc0(b);
  mem.deallocate(l,SIZE(l^))
END dispose;

PROCEDURE makeup(to: TILE);
  VAR  pt: TILE; buf: INTEGER;
BEGIN
  buf:=to^.x; pt:=to^.up;
  WHILE pt^.x>=buf DO pt^.dw:=to; pt:=pt^.lf END
END makeup;

PROCEDURE makerg(to: TILE);
  VAR  pt: TILE; buf: INTEGER;
BEGIN buf:=to^.y; pt:=to^.rg;
  WHILE pt^.y>=buf DO pt^.lf:=to; pt:=pt^.dw; END
END makerg;

PROCEDURE makelf(to: TILE);
  VAR  pt: TILE; buf: INTEGER;
BEGIN buf:=to^.up^.y; pt:=to^.lf;
  WHILE pt^.up^.y<=buf DO pt^.rg:=to; pt:=pt^.up END
END makelf;

PROCEDURE makedw(to: TILE);
  VAR  pt: TILE; buf: INTEGER;
BEGIN buf:=to^.rg^.x; pt:=to^.dw;
  WHILE pt^.rg^.x<=buf DO pt^.up:=to; pt:=pt^.rg END
END makedw;

PROCEDURE make(t: TILE);
BEGIN makerg(t); makelf(t); makeup(t); makedw(t) END make;

PROCEDURE divide2(t: TILE; yline: INTEGER);
  VAR new, pt: TILE;  buf: INTEGER;
BEGIN
  IF (yline>=t^.up^.y) OR (yline<=t^.y) THEN RETURN END;
  alloc(new,t^.type,t^.x,yline);
  IF NOT done THEN RETURN END;
  pt:=t^.lf; WHILE pt^.up^.y<=yline DO pt:=pt^.up END;
  WITH new^ DO
    lf:=pt; rg:=t^.rg;  -- установка указателей
    dw:=t;  up:=t^.up;  -- у созданного тайла
  END;
  pt:=t^.rg; WHILE pt^.y>=yline DO pt:=pt^.dw END;
  WITH t^ DO rg:=pt; up:=new END;       -- смена указателей у старого тайла

  buf:=new^.x;     pt:=new^.up; WHILE pt^.x>=buf     DO pt^.dw:=new; pt:=pt^.lf END;
  buf:=new^.up^.y; pt:=new^.lf; WHILE pt^.up^.y<=buf DO pt^.rg:=new; pt:=pt^.up END;
  buf:=new^.y;     pt:=new^.rg; WHILE pt^.y>=buf     DO pt^.lf:=new; pt:=pt^.dw; END
  (* makeup(new); makelf(new); makerg(new) *)
END divide2;

PROCEDURE div2(t: TILE; xline: INTEGER): TILE;
  VAR new,pt: TILE;   buf: INTEGER;
BEGIN
  alloc(new,t^.type,xline,t^.y); IF NOT done THEN RETURN NIL END;
  pt:=t^.dw; WHILE pt^.rg^.x<=xline DO pt:=pt^.rg END;
  WITH new^ DO
    lf:=t; rg:=t^.rg;
    up:=t^.up; dw:=pt;
  END;
  pt:=t^.up; WHILE pt^.x>=xline DO pt:=pt^.lf END;
  WITH t^ DO up:=pt; rg:=new END;

  buf:=new^.x;     pt:=new^.up; WHILE pt^.x>=buf     DO pt^.dw:=new; pt:=pt^.lf END;
  buf:=new^.y;     pt:=new^.rg; WHILE pt^.y>=buf     DO pt^.lf:=new; pt:=pt^.dw; END;

  (*makeup(new); makerg(new);*)

  makedw(new);
  RETURN new
END div2;

PROCEDURE pasteupdw(l: LAY; t: TILE);
  VAR s: TILE;
      c: BOOLEAN;
BEGIN
  c:=FALSE;
  WITH t^ DO
    IF (type=dw^.type)&(x=dw^.x)&(rg^.x=dw^.rg^.x) THEN
      y :=dw^.y; s :=dw;
      dw:=s^.dw; lf:=s^.lf;
      makedw(t); dealloc(l,s);
      c :=TRUE
    END;
    IF (type=up^.type)&(x=up^.x)&(rg^.x=up^.rg^.x) THEN
      s :=up;
      up:=s^.up; rg:=s^.rg;
      makeup(t); dealloc(l,s);
      c :=TRUE
    END
  END;
  IF c THEN makelf(t); makerg(t) END
END pasteupdw;

PROCEDURE catV(l: LAY; t: TILE);
(*  paste this tile with right upper tile if possible *)
  VAR s: TILE;
BEGIN
  WITH t^ DO
    IF (type#up^.type) OR (x#up^.x) OR (rg^.x#up^.rg^.x) THEN RETURN END;
    s:=up; rg:=s^.rg; up:=s^.up;
  END;
  makelf(t); makeup(t); makerg(t); dealloc(l,s)
END catV;

PROCEDURE catH(l: LAY; t: TILE);
  VAR s: TILE;
BEGIN
  WITH t^ DO
    IF (type#rg^.type) OR (y#rg^.y) OR (up^.y#rg^.up^.y) THEN RETURN END;
    s:=rg; rg:=s^.rg; up:=s^.up; make(t); dealloc(l,s);
  END
END catH;

PROCEDURE cutear(t: TILE);
  VAR ear: TILE;
BEGIN
  WITH t^ DO
    ear:=lf; WHILE ear^.up^.y<up^.y DO ear:=ear^.up END;
    IF ear^.type=type THEN divide2(ear,up^.y); IF NOT done THEN RETURN END END;
    IF  rg^.type=type THEN divide2(rg,up^.y);  IF NOT done THEN RETURN END END;
    ear:=rg; WHILE ear^.y>y DO ear:=ear^.dw END;
    IF ear^.type=type THEN divide2(ear,y);     IF NOT done THEN RETURN END END;
    IF  lf^.type=type THEN divide2(lf,y);      IF NOT done THEN RETURN END END
  END
END cutear;

PROCEDURE cutmyself(t: TILE; typ,x,y,sx,sy: INTEGER);
  VAR pt,pt1: TILE;
          my: INTEGER;
BEGIN
  my:=y+sy;
  pt:=t^.lf; pt1:=t^.rg;
  WHILE pt^.y<my DO
    WITH pt^ DO
      IF (type=typ) OR (up^.type=typ) THEN
        divide2(rg,up^.y); IF NOT done THEN RETURN END;
        IF rg^.rg^.type=typ THEN
          divide2(rg^.rg,up^.y); IF NOT done THEN RETURN END
        END
      END
    END;
    pt:=pt^.up;
  END;
  WHILE pt1^.up^.y<my DO pt1:=pt1^.up END;
  WHILE pt1^.y>=y DO
    WITH pt1^ DO
      IF (type=typ) OR (dw^.type=typ) THEN
        divide2(lf,y); IF NOT done THEN RETURN END;
        IF lf^.lf^.type=typ THEN
          divide2(lf^.lf,y); IF NOT done THEN RETURN END
        END
      END
    END;
    pt1:=pt1^.dw;
  END
END cutmyself;

PROCEDURE pastev(l: LAY; t: TILE);
  VAR s,pt: TILE;
BEGIN
  WITH t^ DO
    pt:=lf;
    IF pt^.y<y THEN pt:=pt^.up END;
    WHILE pt^.up^.y<up^.y DO
      s:=pt^.up; catV(l,pt);
      IF pt^.up=s THEN pt:=pt^.up END;
    END;
    pt:=rg^.dw;
    IF pt^.up^.y>=up^.y THEN pt:=pt^.dw END;
    WHILE pt^.y>=y  DO catV(l,pt); pt:=pt^.dw END
  END
END pastev;

PROCEDURE pastelf(l: LAY; t: TILE);
  VAR n: TILE;
BEGIN
  t^.x :=t^.lf^.x; n    :=t^.lf;
  t^.dw:=n^.dw;    t^.lf:=n^.lf;
  makelf(t); dealloc(l,n)
END pastelf;

PROCEDURE pasterg(l: LAY; t: TILE);
  VAR n: TILE;
BEGIN
  n    :=t^.rg;
  t^.up:=n^.up; t^.rg:=n^.rg;
  makerg(t); dealloc(l,n)
END pasterg;

PROCEDURE pasteh(l: LAY; x,y,sy: INTEGER);
  VAR n,nn: TILE; c: BOOLEAN;
BEGIN
  n:=where(l,x,y);
  y:=y+sy; c:=FALSE;
  WHILE n^.y<y DO
    nn:=n^.up;
    IF n^.type=n^.lf^.type THEN c:=TRUE; pastelf(l,n) END;
    IF n^.type=n^.rg^.type THEN c:=TRUE; pasterg(l,n) END;
    IF c THEN makeup(n); makedw(n); c:=FALSE END;
    n:=nn
  END;
  IF n^.type=n^.dw^.type THEN catV(l,n^.dw) END
END pasteh;

PROCEDURE canon(l: LAY; t: TILE);
VAR n: TILE;
    x,y,sx,sy,tp: INTEGER;
BEGIN
  pasteupdw(l,t);
  x :=t^.x; sx:=t^.rg^.x-x;
  y :=t^.y; sy:=t^.up^.y-y;
  tp:=t^.type;
  cutear(t);
  IF NOT done THEN RETURN END;
  pastev(l,t);                    -- unite all t-friends by vertical line
  cutmyself(t,tp,x,y,sx,sy);      -- tile "t" is dividing by least pices & destroed...
  IF NOT done THEN RETURN END;
  pasteh(l,x,y,sy)                -- unite all tiles in area by horizontal line...
END canon;

PROCEDURE start(VAR it: ITERCTX; l: LAY; x,y,sx,sy: INTEGER);
  VAR s: TILE;
BEGIN
  it.x :=x;    it.y :=y;
  it.x1:=x+sx; it.y1:=y+sy;
  WITH it DO
    pt:=where(l,x,y1-1);
    s:=pt;
    REPEAT
      pt:=s; s:=s^.rg;
      IF s^.x>=x1 THEN RETURN END;
      LOOP
        IF s^.lf#pt THEN RETURN END;
        IF s^.y<y1  THEN EXIT END;
        s:=s^.dw
      END;
    UNTIL s^.y<=y
  END
END start;

PROCEDURE next(VAR it: ITERCTX; VAR t: TILE): BOOLEAN;
  VAR s,n: TILE;
BEGIN
  WITH it DO
    t:=pt;
    IF pt^.y<=y THEN
      IF pt^.rg^.x>=x1 THEN
        RETURN TRUE
      ELSE
        pt:=pt^.rg; WHILE pt^.y>y DO pt:=pt^.dw END
      END
    ELSIF pt^.x<=x THEN
      (* pt^.y>y *)
      pt:=pt^.dw; WHILE pt^.rg^.x<=x DO pt:=pt^.rg END
    ELSE
      n:=pt^.dw;
      IF (n^.lf=pt^.lf)&(n^.y>y) THEN
        pt:=n                              (* IF HaveMinorBrother THEN *)
      ELSE
        pt:=pt^.lf;                        (* Father(pt) *)
        RETURN FALSE
      END
    END;
    s:=pt;
    REPEAT
      pt:=s; s:=s^.rg;
      IF (s^.x>=x1) OR (s^.lf#pt) THEN RETURN FALSE END;
      IF s^.y>=y1 THEN
        REPEAT
          s:=s^.dw;
          IF s^.lf#pt THEN RETURN FALSE END
        UNTIL s^.y<y1
      END
    UNTIL s^.y<=y
  END;
  RETURN FALSE
END next;

PROCEDURE iterate(l: LAY; x,y,sx,sy: INTEGER; f: iterator);
  VAR    x1,y1: INTEGER;
          pt,s: TILE;
             n: TILE;
BEGIN
  x1:=x+sx; y1:=y+sy;
  pt:=where(l,x,y1-1);
  LOOP
    s:=pt;
    LOOP
      pt:=s; s:=s^.rg;
      IF s^.x>=x1 THEN EXIT END;
      WHILE (s^.lf=pt) & (s^.y>=y1) DO s:=s^.dw END;
      IF (s^.lf#pt)OR(s^.y<=y) THEN EXIT END
    END;
    LOOP
      s:=pt;
      IF pt^.y<=y THEN
        IF pt^.rg^.x>=x1 THEN
          IF f(s) THEN END; RETURN
        ELSE
          pt:=pt^.rg; WHILE pt^.y>y DO pt:=pt^.dw END;
          EXIT
        END
      ELSIF pt^.x<=x THEN
        (* pt^.y>y *)
        pt:=pt^.dw; WHILE pt^.rg^.x<=x DO pt:=pt^.rg END;
        EXIT
      ELSE
        n:=pt^.dw;
        IF (n^.lf=pt^.lf)&(n^.y>y) THEN
          pt:=n;                             (* IF HaveMinorBrother THEN *)
          EXIT
        ELSE
          pt:=pt^.lf;                        (* Father(pt) *)
          IF f(s) THEN RETURN END
        END
      END;
    END;
    IF f(s) THEN RETURN END;
  END;
END iterate;

PROCEDURE mark(l: LAY; x,y,sx,sy: INTEGER; m: BITSET);
  VAR    x1,y1: INTEGER;
          pt,s: TILE;
             n: TILE;
BEGIN
  x1:=x+sx; y1:=y+sy;
  pt:=where(l,x,y1-1);
  LOOP
    s:=pt;
    LOOP
      pt:=s; s:=s^.rg;
      IF s^.x>=x1 THEN EXIT END;
      WHILE (s^.lf=pt) & (s^.y>=y1) DO s:=s^.dw END;
      IF (s^.lf#pt)OR(s^.y<=y) THEN EXIT END
    END;
    LOOP
      pt^.type:=INTEGER(BITSET(pt^.type)+m);
      IF pt^.y<=y THEN
        IF pt^.rg^.x>=x1 THEN RETURN
        ELSE
          pt:=pt^.rg; WHILE pt^.y>y DO pt:=pt^.dw END;
          EXIT
        END
      ELSIF pt^.x<=x THEN
        (* pt^.y>y *)
        pt:=pt^.dw; WHILE pt^.rg^.x<=x DO pt:=pt^.rg END;
        EXIT
      ELSE
        n:=pt^.dw;
        IF (n^.lf=pt^.lf)&(n^.y>y) THEN
          pt:=n;                             (* IF HaveMinorBrother THEN *)
          EXIT
        ELSE
          pt:=pt^.lf                         (* Father(pt) *)
        END
      END;
    END
  END
END mark;

VAR X0,Y0,X1,Y1: INTEGER;
        NEWTYPE: INTEGER;
              F: tl_type;
              L: LAY;

PROCEDURE _mark(t: TILE): BOOLEAN;
BEGIN
  t^.type:=INTEGER(BITSET(t^.type)+{31}); RETURN FALSE
END _mark;

PROCEDURE _unmark(t: TILE): BOOLEAN;
BEGIN
  t^.type:=INTEGER(BITSET(t^.type)-{31}); RETURN FALSE
END _unmark;

PROCEDURE clip(VAR t: TILE);
BEGIN
  IF t^.up^.y>Y1 THEN divide2(t,Y1); IF NOT done THEN RETURN END END;
  IF t^    .y<Y0 THEN divide2(t,Y0); IF NOT done THEN RETURN END; t:=t^.up END;
  IF t^    .x<X0 THEN t:=div2(t,X0); IF NOT done THEN RETURN END END;
  IF t^.rg^.x>X1 THEN t:=div2(t,X1); IF NOT done THEN RETURN END; t:=t^.lf END
END clip;

PROCEDURE _create(t: TILE): BOOLEAN;
  VAR tn: INTEGER;
BEGIN
  t^.type:=INTEGER(BITSET(t^.type)-{31});
  tn:=F(t^.type,NEWTYPE);
  IF tn=t^.type THEN canon(L,t); RETURN NOT done END;
  clip(t);
  IF NOT done THEN RETURN TRUE END;
  t^.type:=tn;
  canon(L,t);
  RETURN NOT done
END _create;

PROCEDURE insert(l: LAY; tp: SYSTEM.WORD; x,y,sx,sy: INTEGER; f: tl_type);
BEGIN
  done:=TRUE;
  IF 31 IN BITSET(tp) THEN done:=FALSE; error:=err.bad_parm; RETURN END;
  L :=l;    F :=f; NEWTYPE:=tp;
  X0:=x;    Y0:=y;
  X1:=x+sx; Y1:=y+sy;
  --iterate(l,x,y,sx,sy,_mark);
  mark(l,x,y,sx,sy,{31});
  iterate(l,x,y,sx,sy,_create);
  IF NOT done THEN iterate(l,x,y,sx,sy,_unmark) END
END insert;

PROCEDURE free(l: LAY; x,y,sx,sy: INTEGER): BOOLEAN;
  VAR c: ITERCTX;
   stop: BOOLEAN;
      t: TILE;
BEGIN
  done:=TRUE;
  start(c,l,x,y,sx,sy);
  REPEAT
    stop:=next(c,t);
    IF t^.type#spc THEN RETURN FALSE END
  UNTIL stop;
  RETURN TRUE
END free;

BEGIN
  HEAD:=NIL;
  done:=TRUE; error:=err.ok
END tile.
