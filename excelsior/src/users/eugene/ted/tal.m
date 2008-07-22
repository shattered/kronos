IMPLEMENTATION MODULE tal; (* 26-Oct-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;
FROM VIDEO      IMPORT  rect, rep, color, mode, start, finish, patt, vect, com;
FROM Terminal   IMPORT  print, Read;

CONST
  min_integer=-10000h;
  max_integer=10000h;
  board=min_integer;
  free=0;

VAR mem: tile;

PROCEDURE Alloc(VAR t: tile);
BEGIN
  IF mem=NIL THEN
    ALLOCATE(t,SIZE(t^));
  ELSE
    t:=mem; mem:=t^.up;
  END;
END Alloc;

PROCEDURE Dealloc(t: tile);
BEGIN
  t^.up:=mem; mem:=t;
END Dealloc;

PROCEDURE show(t: tile);
  VAR x,y,x1,y1: INTEGER;
BEGIN
  x:=t^.x; y:=t^.y;
  x1:=t^.rt^.x-1; y1:=t^.up^.y-1;
  mode(rep);
  IF (t^.dt=board) OR (t^.dt=0) THEN color(0) ELSE color(t^.dt) END;
  rect(x,y,x1,y1);
  mode(com); color(3);
  vect(x,y,x,y1);
  vect(x,y,x1,y);
END show;

PROCEDURE go(x,y: INTEGER);
BEGIN
  LOOP
    IF    wsp^.up^.y<=y THEN wsp:=wsp^.up
    ELSIF wsp^.y>y      THEN wsp:=wsp^.dw
    ELSIF wsp^.x>x      THEN wsp:=wsp^.lf
    ELSIF wsp^.rt^.x<=x THEN wsp:=wsp^.rt
    ELSE RETURN
    END;
  END;
END go;

PROCEDURE div_h(t: tile; y: INTEGER);
  VAR s,u: tile;
BEGIN
  IF t^.y=y THEN RETURN END;
  Alloc(u);
  u^:=t^;
  u^.y:=y; u^.dw:=t; t^.up:=u;
  s:=t^.lf; WHILE s^.up^.y<=y DO s:=s^.up END;
  u^.lf:=s; WHILE s^.up^.y<=u^.up^.y DO s^.rt:=u; s:=s^.up END;
  s:=u^.rt; WHILE s^.y>=y DO s^.lf:=u; s:=s^.dw END; t^.rt:=s;
  s:=u^.up; WHILE s^.x>=u^.x DO s^.dw:=u; s:=s^.lf END;
END div_h;

PROCEDURE cat_h(t: tile);
  VAR s,u: tile;
BEGIN
  u:=t^.up;
ASSERT(u^.x=t^.x);
ASSERT(u^.rt^.x=t^.rt^.x);
ASSERT(u^.dt=t^.dt);
            t^.up:=u^.up; t^.rt:=u^.rt;
  s:=u^.lf; WHILE s^.up^.y<=u^.up^.y DO s^.rt:=t; s:=s^.up END;
  s:=u^.rt; WHILE s^.y>=u^.y DO s^.lf:=t; s:=s^.dw END;
  s:=u^.up; WHILE s^.x>=u^.x DO s^.dw:=t; s:=s^.lf END;
  Dealloc(u);
  wsp:=t;
END cat_h;

PROCEDURE div_v(t: tile; x: INTEGER);
  VAR s,u: tile;
BEGIN
  IF t^.x=x THEN RETURN END;
  Alloc(u);
  u^:=t^; u^.x:=x; u^.lf:=t; t^.rt:=u;
  s:=t^.dw; WHILE s^.rt^.x<=x DO s:=s^.rt END;
  u^.dw:=s; WHILE s^.rt^.x<=u^.rt^.x DO s^.up:=u; s:=s^.rt END;
  s:=u^.up; WHILE s^.x>=x DO s^.dw:=u; s:=s^.lf END; t^.up:=s;
  s:=u^.rt; WHILE s^.y>=u^.y DO s^.lf:=u; s:=s^.dw END;
END div_v;

PROCEDURE cat_v(t: tile);
  VAR s,u: tile;
BEGIN
  u:=t^.rt;
ASSERT(u^.dt=t^.dt);
ASSERT(u^.y=t^.y);
ASSERT(u^.up^.y=t^.up^.y);
  t^.rt:=u^.rt; t^.up:=u^.up;
  s:=u^.dw; WHILE s^.rt^.x<=u^.rt^.x DO s^.up:=t; s:=s^.rt END;
  s:=u^.up; WHILE s^.x>=u^.x DO s^.dw:=t; s:=s^.lf END;
  s:=u^.rt; WHILE s^.y>=u^.y DO s^.lf:=t; s:=s^.dw END;
  Dealloc(u);
  wsp:=t;
END cat_v;

PROCEDURE cat(VAR t: tile);
BEGIN
  IF t^.dt=board THEN RETURN END;
  LOOP
    IF t^.rt^.dt=t^.dt THEN
      IF t^.y<t^.rt^.y THEN div_h(t,t^.rt^.y); t:=t^.up END;
      IF t^.y>t^.rt^.y THEN div_h(t^.rt,t^.y) END;
      IF t^.up^.y>t^.rt^.up^.y THEN div_h(t,t^.rt^.up^.y) END;
      IF t^.up^.y<t^.rt^.up^.y THEN div_h(t^.rt,t^.up^.y) END;
      cat_v(t);
    ELSIF t^.lf^.dt=t^.dt THEN
      IF t^.y<t^.lf^.y THEN div_h(t,t^.lf^.y); t:=t^.up END;
      IF t^.y>t^.lf^.y THEN div_h(t^.lf,t^.y) END;
      IF t^.up^.y>t^.lf^.up^.y THEN div_h(t,t^.lf^.up^.y) END;
      IF t^.up^.y<t^.lf^.up^.y THEN div_h(t^.lf,t^.up^.y) END;
      t:=t^.lf; cat_v(t);
    ELSIF (t^.up^.dt=t^.dt) & (t^.x=t^.up^.x) & (t^.rt^.x=t^.up^.rt^.x) THEN
      cat_h(t)
    ELSIF (t^.dw^.dt=t^.dt) & (t^.x=t^.dw^.x) & (t^.rt^.x=t^.dw^.rt^.x) THEN
      t:=t^.dw; cat_h(t);
    ELSE EXIT
    END;
  END;
END cat;

VAR
  tbl: ARRAY [0..399] OF tile;
  lf : ARRAY [0..HIGH(tbl)] OF tile;
  rt : ARRAY [0..HIGH(tbl)] OF tile;
  up : ARRAY [0..HIGH(tbl)] OF tile;
  dw : ARRAY [0..HIGH(tbl)] OF tile;
  tx : ARRAY [0..HIGH(tbl)] OF INTEGER;
  ty : ARRAY [0..HIGH(tbl)] OF INTEGER;
  rm : ARRAY [0..HIGH(tbl)] OF BOOLEAN;
  cnt: INTEGER;

PROCEDURE ins(t: tile);
BEGIN
  tbl[cnt]:=t; INC(cnt);
END ins;

PROCEDURE app(x,y,sx,sy,dt: INTEGER);
  VAR
    new: tile;
    t,s: tile;
    i  : INTEGER;
BEGIN
  cnt:=0; box(x-1,y-1,sx+2,sy+2,ins);
  Alloc(new);
  new^.lf:=NIL; new^.rt:=NIL;
  new^.up:=NIL; new^.dw:=NIL;
  new^.x :=x;   new^.y :=y;
  new^.an:=0;   new^.dt:=dt;
  FOR i:=0 TO cnt-1 DO
    t:=tbl[i];
    IF (t^.x<x+sx) & (t^.rt^.x>x+sx) THEN
      IF (t^.y<y) & (t^.up^.y>y) THEN div_h(t,y); ins(t); t:=t^.up END;
      IF (t^.y<y+sy) & (t^.up^.y>y+sy) THEN div_h(t,y+sy); ins(t^.up) END;
    END;
    IF (t^.x<x) & (t^.rt^.x>x) THEN
      IF (t^.y<y) & (t^.up^.y>y) THEN div_h(t,y); ins(t); t:=t^.up END;
      IF (t^.y<y+sy) & (t^.up^.y>y+sy) THEN div_h(t,y+sy); ins(t^.up) END;
    END;
    IF (t^.y<y) & (t^.up^.y>y+sy) THEN div_h(t,y+sy); ins(t^.up) END;
    IF (t^.x<x) & (t^.rt^.x>x+sx) THEN div_v(t,x+sx); ins(t^.rt) END;
    tbl[i]:=t;
  END;
  FOR i:=0 TO cnt-1 DO
    t:=tbl[i]; rm[i]:=FALSE;
    up[i]:=t^.up; dw[i]:=t^.dw;
    rt[i]:=t^.rt; lf[i]:=t^.lf;
    tx[i]:=t^.x;  ty[i]:=t^.y;
    IF t^.x=x+sx THEN
      IF t^.y<y+sy THEN
        IF t^.y>=y THEN lf[i]:=new END;
        IF t^.up^.y>=y+sy THEN new^.rt:=t END;
      END;
    ELSIF t^.y=y+sy THEN
      IF t^.x<x+sx THEN
        IF t^.x>=x THEN dw[i]:=new END;
        IF t^.rt^.x>=x+sx THEN new^.up:=t END;
      END;
    ELSIF t^.rt^.x=x THEN
      IF t^.up^.y>y THEN
        IF t^.up^.y<=y+sy THEN rt[i]:=new END;
        IF t^.y<=y THEN new^.lf:=t END;
      END;
    ELSIF t^.up^.y=y THEN
      IF t^.rt^.x>x THEN
        IF t^.rt^.x<=x+sx THEN up[i]:=new END;
        IF t^.x<=x THEN new^.dw:=t END;
      END;
    ELSIF t^.x<x THEN
      IF t^.y=y THEN new^.lf:=t END;
      rt[i]:=new; s:=t^.up;
      WHILE s^.x>=x DO s:=s^.lf END;
      up[i]:=s;
    ELSIF t^.y<y THEN
      IF t^.x=x THEN new^.dw:=t END;
      up[i]:=new; s:=t^.rt;
      WHILE s^.y>=y DO s:=s^.dw END;
      rt[i]:=s;
    ELSIF t^.rt^.x>x+sx THEN
      IF t^.up^.y=y+sy THEN new^.rt:=t END;
      lf[i]:=new; s:=t^.dw;
      WHILE s^.rt^.x<=x+sx DO s:=s^.rt END;
      dw[i]:=s; tx[i]:=x+sx;
    ELSIF t^.up^.y>y+sy THEN
      IF t^.rt^.x=x+sx THEN new^.up:=t END;
      dw[i]:=new; s:=t^.lf;
      WHILE s^.up^.y<=y+sy DO s:=s^.up END;
      lf[i]:=s; ty[i]:=y+sy;
    ELSE
      rm[i]:=TRUE;
    END;
  END;
  FOR i:=0 TO cnt-1 DO
    t:=tbl[i];
    IF rm[i] THEN
      Dealloc(t);
    ELSE
      t^.up:=up[i]; t^.dw:=dw[i];
      t^.rt:=rt[i]; t^.lf:=lf[i];
      t^.x :=tx[i]; t^.y :=ty[i];
    END;
  END;
ASSERT(new^.rt#NIL);
ASSERT(new^.up#NIL);
ASSERT(new^.dw#NIL);
ASSERT(new^.lf#NIL);
  wsp:=new;
  go(x,y-1); s:=wsp; WHILE s^.x<=x+sx DO cat(s); s:=s^.rt END;
  go(x+sx-1,y+sy); s:=wsp; WHILE s^.rt^.x>x DO cat(s); s:=s^.lf END;
  go(x-1,y); s:=wsp; WHILE s^.y<=y+sy DO cat(s); s:=s^.up END;
  go(x+sx,y+sy-1); s:=wsp; WHILE s^.up^.y>y DO cat(s); s:=s^.dw END;
END app;

PROCEDURE box(x,y,sx,sy: INTEGER; p: iter_proc);
  PROCEDURE bend(t: tile);
    VAR y0: INTEGER;
  BEGIN
    y0:=t^.y; t:=t^.rt;
    IF t^.x>=x+sx THEN RETURN END;
    IF y0<=y THEN y0:=y+1 END;
    WHILE t^.y>=y+sy DO t:=t^.dw END;
    WHILE t^.y>=y0 DO bend(t); p(t); t:=t^.dw END;
  END bend;
  VAR t,s: tile;
BEGIN
  go(x,y); s:=wsp;
  bend(s); p(s);
  t:=s^.rt;
  WHILE t^.x<x+sx DO
    WHILE t^.y>y DO t:=t^.dw END;
    bend(t); p(t); t:=t^.rt;
  END;
  t:=s^.up;
  WHILE t^.y<y+sy DO
    WHILE t^.x>x DO t:=t^.lf END;
    bend(t); p(t); t:=t^.up;
  END;
END box;

PROCEDURE nab(s: tile; p: iter_proc);
  VAR t: tile;
BEGIN
  t:=s^.dw;
  IF (t^.x=s^.x) & (s^.lf^.y=s^.y) THEN
    t:=t^.lf;
    WHILE t^.up^.y<s^.y DO t:=t^.up END;
  END;
  REPEAT p(t); t:=t^.rt UNTIL t^.x>s^.rt^.x;
  t:=s^.lf;
  REPEAT p(t); t:=t^.up UNTIL t^.y>s^.up^.y;
  t:=s^.up;
  IF (t^.rt^.x=s^.rt^.x) & (s^.rt^.up^.y=s^.up^.y) THEN
    t:=t^.rt;
    WHILE t^.y>s^.up^.y DO t:=t^.dw END;
  END;
  REPEAT p(t); t:=t^.lf UNTIL t^.rt^.x<=s^.x;
  t:=s^.rt;
  REPEAT p(t); t:=t^.dw UNTIL t^.up^.y<=s^.y;
END nab;

VAR
  t: ARRAY [0..10] OF tile_rec;
  i: INTEGER;

PROCEDURE brd(n,x,y,lf,dw,rt,up: INTEGER);
BEGIN
  t[n].x:=x; t[n].y:=y;
  t[n].lf:=ADR(t[lf]); t[n].rt:=ADR(t[rt]);
  t[n].dw:=ADR(t[dw]); t[n].up:=ADR(t[up]);
  t[n].an:=0; t[n].dt:=board;
END brd;

BEGIN
  brd( 0,min_integer  ,min_integer  ,9,10,1,3);
  brd( 1,min_integer+1,min_integer  ,0,10,2,4);
  brd( 2,max_integer-1,min_integer  ,1,10,9,5);
  brd( 3,min_integer  ,min_integer+1,9,0,4,6);
  brd( 4,min_integer+1,min_integer+1,3,1,5,7);
  brd( 5,max_integer-1,min_integer+1,4,2,9,8);
  brd( 6,min_integer  ,max_integer-1,9,3,7,10);
  brd( 7,min_integer+1,max_integer-1,6,4,8,10);
  brd( 8,max_integer-1,max_integer-1,7,5,9,10);
  brd( 9,max_integer  ,min_integer  ,2,10,6,10);
  brd(10,min_integer  ,max_integer  ,10,6,10,9);
  t[4].dt:=free;
  wsp:=ADR(t[4]);
  mem:=NIL;
END tal.
