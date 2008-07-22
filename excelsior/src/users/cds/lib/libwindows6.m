IMPLEMENTATION MODULE libWindows; (* Sem 31-Dec-88. (c) KRONOS *)

IMPORT  cod : defCodes;
IMPORT  cpd : MouseDrv;
IMPORT  mem : libHeap;
IMPORT  err : libCrash;
IMPORT  os  : osKernel;

FROM SYSTEM      IMPORT ADDRESS, ADR;

WITH STORAGE : mem;

CONST
  crs_sx=13;
  crs_sy=6;

TYPE
  blk=ARRAY [0..crs_sy-1] OF BITSET;

VAR
  hsize : INTEGER;
  vsize : INTEGER;
  linesz: INTEGER;
  lsz   : INTEGER;
  lsz2  : INTEGER;
  lsz3  : INTEGER;
  lay   : ADDRESS;
  zr    : ADDRESS;
  one   : ADDRESS;
  back  : ADDRESS;
  cpu   : INTEGER;
  crsX  : INTEGER;
  crsY  : INTEGER;
  crsON : BOOLEAN;
  crs_w : window;
  lock  : os.mutex_rec;
  crsPic: blk;
  savPic: blk;
  ident : INTEGER;
  pal   : ARRAY [0..15] OF RECORD r,g,b: INTEGER END;

PROCEDURE MOVE(a,b: ADDRESS; n: INTEGER); CODE cod.move END MOVE;
PROCEDURE bblt(a1: ADDRESS; n1: INTEGER;
               a2: ADDRESS; n2,sz: INTEGER); CODE cod.bblt END bblt;

PROCEDURE empty(w: window; x,y: INTEGER; ch: CHAR);
BEGIN
END empty;

PROCEDURE create(sx,sy: INTEGER): window;
  VAR sz,wpl: INTEGER; w: window; e: err.trap;
BEGIN
  wpl:=(sx+31) DIV 32;
  sz:=wpl*sy*lsno;
  IF err.enter(e) THEN
    IF w#NIL THEN DISPOSE(w) END;
    err.re_raise(e);
  END;
  NEW(w); mem.ALLOCATE(w^.mem,sz);
  err.exit(e);
  w^.base:=w^.mem;
  w^.x:=0;
  w^.y:=0;
  w^.sx:=sx;
  w^.sy:=sy;
  w^.wpl:=wpl;
  w^.wpp:=wpl*sy;
  w^.open:=FALSE;
  w^.fwd:=w;
  w^.bck:=w;
  w^.mem^:=0;
  MOVE(w^.mem+1,w^.mem,sz-1);
  w^.patt:={0..31};
  w^.mode:=0;
  w^.color:=0;
  w^.job:=empty;
  w^.info:=NIL;
  w^.W:=0;
  w^.E:=sx-1;
  w^.S:=0;
  w^.N:=sy-1;
  w^.ident:=ident; INC(ident);
  RETURN w;
END create;

PROCEDURE remove(VAR w: window);
  VAR sz: INTEGER;
BEGIN
  IF w=NIL THEN RETURN END;
  ASSERT(w^.mem#NIL);
  ASSERT(w^.wpp=w^.wpl*w^.sy);
  close(w);
  sz:=w^.wpp*lsno;
  mem.DEALLOCATE(w^.mem,sz);
  DISPOSE(w);
END remove;

PROCEDURE drow_crs(x,y: INTEGER); FORWARD;

PROCEDURE move(w: window; x,y,sx,sy: INTEGER);
  TYPE s=BITSET;
  VAR wpp,wpp2,wpp3,fx,n: INTEGER; fr,to: ADDRESS;
BEGIN
  IF x<0 THEN DEC(sx,-x); x:=0 END;
  IF y<0 THEN DEC(sy,-y); y:=0 END;
  IF x+sx>hsize THEN sx:=hsize-x END;
  IF y+sy>vsize THEN sy:=vsize-y END;
  IF (sx<=0) OR (sy<=0) THEN RETURN END;
  to:=lay+(vsize-y-1)*linesz;
  fr:=w^.mem+(w^.sy-y+w^.y-1)*w^.wpl;
  wpp:=w^.wpp; wpp2:=wpp+wpp; wpp3:=wpp2+wpp;
  n:=sy; fx:=x-w^.x;
  IF s(fx)+s(sx)+s(x)*{0..4}={} THEN
    to:=to + x DIV 32; fr:=fr + fx DIV 32; sx:=sx DIV 32;
    REPEAT
      MOVE(to     ,fr    ,sx);
      MOVE(to+lsz ,fr+wpp,sx);
      MOVE(to+lsz2,fr+wpp2,sx);
      MOVE(to+lsz3,fr+wpp3,sx);
      DEC(n); DEC(to,linesz); DEC(fr,w^.wpl);
    UNTIL n=0;
  ELSE
    REPEAT
      bblt(to     ,x,fr    ,fx,sx);
      bblt(to+lsz ,x,fr+wpp,fx,sx);
      bblt(to+lsz2,x,fr+wpp2,fx,sx);
      bblt(to+lsz3,x,fr+wpp3,fx,sx);
      DEC(n); DEC(to,linesz); DEC(fr,w^.wpl);
    UNTIL n=0;
  END;
END move;

PROCEDURE zero(x,y,sx,sy: INTEGER);
  VAR n: INTEGER; to: ADDRESS;
BEGIN
  IF x<0 THEN DEC(sx,-x); x:=0 END;
  IF y<0 THEN DEC(sy,-y); y:=0 END;
  IF x+sx>hsize THEN sx:=hsize-x END;
  IF y+sy>vsize THEN sy:=vsize-y END;
  IF (sx<=0) OR (sy<=0) THEN RETURN END;
  to:=lay+(vsize-y-1)*linesz;
  n:=sy;
  REPEAT
    bblt(to     ,x,zr,0,sx);
    bblt(to+lsz ,x,zr,0,sx);
    bblt(to+lsz2,x,zr,0,sx);
    IF ODD(y) THEN
      IF y MOD 4 =3 THEN
        bblt(to+lsz3,x,back,x,sx);
      ELSE
        bblt(to+lsz3,x,back,x+1,sx);
      END;
    ELSE
      bblt(to+lsz3,x,zr,0,sx);
    END;
    DEC(n); DEC(to,linesz); INC(y);
  UNTIL n=0;
END zero;

PROCEDURE ref_bx(w: window; bxy,bxdy: INTEGER);
  PROCEDURE ref(t: window; x,y,sx,sy: INTEGER);
    VAR dy: INTEGER;
  BEGIN
    IF t=w THEN move(w,x,y,sx,sy); RETURN END;
    IF x>=t^.x+t^.sx THEN ref(t^.fwd,x,y,sx,sy); RETURN END;
    IF y>=t^.y+t^.sy THEN ref(t^.fwd,x,y,sx,sy); RETURN END;
    IF x+sx<=t^.x    THEN ref(t^.fwd,x,y,sx,sy); RETURN END;
    IF y+sy<=t^.y    THEN ref(t^.fwd,x,y,sx,sy); RETURN END;
    IF t^.y>y THEN ref(t^.fwd,x,y,sx,t^.y-y); DEC(sy,t^.y-y); y:=t^.y END;
    dy:=t^.y+t^.sy-y; IF dy>sy THEN dy:=sy END;
    IF x<t^.x THEN ref(t^.fwd,x,y,t^.x-x,dy) END;
    IF x+sx>t^.x+t^.sx THEN ref(t^.fwd,t^.x+t^.sx,y,x+sx-t^.x-t^.sx,dy) END;
    INC(y,dy); DEC(sy,dy);
    IF sy>0 THEN ref(t^.fwd,x,y,sx,sy) END;
  END ref;
  VAR crs: BOOLEAN;
BEGIN
  IF NOT w^.open THEN RETURN END;
  crs:=crsON & (crsX+crs_sx>w^.x) & (crsX<w^.x+w^.sx) &
               (crsY>=w^.y) & (crsY-crs_sy+1<w^.y+w^.sy);
  IF crs THEN del_crs END;
  ref(top,w^.x,w^.y+bxy,w^.sx,bxdy);
  IF crs THEN drow_crs(crsX,crsY) END;
END ref_bx;

PROCEDURE ref_box(w: window; y,sy: INTEGER);
BEGIN
  os.acquire(lock);
  IF y<0 THEN sy:=sy+y; y:=0 END;
  IF y+sy>w^.sy THEN sy:=w^.sy-y END;
  ref_bx(w,y,sy);
  os.release(lock);
END ref_box;

PROCEDURE refresh(w: window);
BEGIN
  os.acquire(lock);
  ref_bx(w,0,w^.sy);
  os.release(lock);
END refresh;

PROCEDURE open(w: window);
BEGIN
  IF (w=NIL) OR w^.open THEN RETURN END;
  os.acquire(lock);
  IF top#NIL THEN
    w^.fwd:=top; w^.bck:=top^.bck;
    w^.fwd^.bck:=w; w^.bck^.fwd:=w;
  END;
  top:=w;
  w^.open:=TRUE;
  ref_bx(w,0,w^.sy);
  os.release(lock);
END open;

PROCEDURE close(w: window);
  PROCEDURE ref(t: window; x,y,sx,sy: INTEGER);
    VAR x0,x1,dy: INTEGER; nxt: window;
  BEGIN
    IF t=NIL THEN zero(x,y,sx,sy); RETURN END;
    nxt:=t^.fwd; IF nxt=top THEN nxt:=NIL END;
    IF x>=t^.x+t^.sx THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF y>=t^.y+t^.sy THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF x+sx<=t^.x    THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF y+sy<=t^.y    THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF t^.y>y THEN ref(nxt,x,y,sx,t^.y-y); DEC(sy,t^.y-y); y:=t^.y END;
    dy:=t^.y+t^.sy-y; IF dy>sy THEN dy:=sy END;
    IF x<t^.x THEN ref(nxt,x,y,t^.x-x,dy); x0:=t^.x ELSE x0:=x END;
    IF x+sx>t^.x+t^.sx THEN
      ref(nxt,t^.x+t^.sx,y,x+sx-t^.x-t^.sx,dy); x1:=t^.x+t^.sx
    ELSE
      x1:=x+sx
    END;
    move(t,x0,y,x1-x0,dy);
    INC(y,dy); DEC(sy,dy);
    IF sy>0 THEN ref(nxt,x,y,sx,sy) END;
  END ref;
  VAR crs: BOOLEAN;
BEGIN
  IF (w=NIL) OR NOT w^.open THEN RETURN END;
  os.acquire(lock);
  crs:=crsON; del_crs;
  IF crs_w=w THEN crs_w:=NIL END;
  IF top=w THEN top:=top^.fwd END;
  IF top=w THEN
    top:=NIL
  ELSE
    w^.fwd^.bck:=w^.bck;
    w^.bck^.fwd:=w^.fwd;
  END;
  w^.fwd:=w; w^.bck:=w;
  ref(top,w^.x,w^.y,w^.sx,w^.sy);
  w^.open:=FALSE;
  IF crs THEN drow_crs(crsX,crsY) END;
  os.release(lock);
END close;

PROCEDURE ontop(w: window);
  VAR crs: BOOLEAN;
BEGIN
  IF NOT w^.open THEN RETURN END;
  os.acquire(lock);
  IF top#w THEN
    crs:=crsON; del_crs;
    w^.fwd^.bck:=w^.bck; w^.bck^.fwd:=w^.fwd;
    w^.fwd:=top; w^.bck:=top^.bck;
    w^.fwd^.bck:=w; w^.bck^.fwd:=w;
    top:=w;
    ref_bx(w,0,w^.sy);
    IF crs THEN drow_crs(crsX,crsY) END;
  END;
  os.release(lock);
END ontop;

PROCEDURE ontop?(w: window): BOOLEAN;
  VAR r: BOOLEAN; t: window;
BEGIN
  IF NOT w^.open THEN RETURN FALSE END;
  os.acquire(lock);
  t:=top;
  LOOP
    IF t=w THEN r:=TRUE; EXIT END;
    IF (t^.x<w^.x+w^.sx) & (t^.x+t^.sx>w^.x) &
       (t^.y<w^.y+w^.sy) & (t^.y+t^.sy>w^.y) THEN r:=FALSE; EXIT END;
    t:=t^.fwd;
  END;
  os.release(lock);
  RETURN r;
END ontop?;

PROCEDURE onbottom(w: window);
  PROCEDURE ref(t: window; x,y,sx,sy: INTEGER);
    VAR x0,x1,dy: INTEGER; nxt: window;
  BEGIN
    IF t=w THEN RETURN END;
    nxt:=t^.fwd;
    IF x>=t^.x+t^.sx THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF y>=t^.y+t^.sy THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF x+sx<=t^.x    THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF y+sy<=t^.y    THEN ref(nxt,x,y,sx,sy); RETURN END;
    IF t^.y>y THEN ref(nxt,x,y,sx,t^.y-y); DEC(sy,t^.y-y); y:=t^.y END;
    dy:=t^.y+t^.sy-y; IF dy>sy THEN dy:=sy END;
    IF x<t^.x THEN ref(nxt,x,y,t^.x-x,dy); x0:=t^.x ELSE x0:=x END;
    IF x+sx>t^.x+t^.sx THEN
      ref(nxt,t^.x+t^.sx,y,x+sx-t^.x-t^.sx,dy); x1:=t^.x+t^.sx
    ELSE
      x1:=x+sx
    END;
    move(t,x0,y,x1-x0,dy);
    INC(y,dy); DEC(sy,dy);
    IF sy>0 THEN ref(nxt,x,y,sx,sy) END;
  END ref;
  VAR crs: BOOLEAN;
BEGIN
  IF NOT w^.open THEN RETURN END;
  os.acquire(lock);
  IF top^.bck#w THEN
    crs:=crsON; del_crs;
    IF top=w THEN top:=top^.fwd END;
    ASSERT(top#w);
    w^.fwd^.bck:=w^.bck; w^.bck^.fwd:=w^.fwd;
    w^.fwd:=top; w^.bck:=top^.bck;
    w^.fwd^.bck:=w; w^.bck^.fwd:=w;
    ref(top,w^.x,w^.y,w^.sx,w^.sy);
    IF crs THEN drow_crs(crsX,crsY) END;
  END;
  os.release(lock);
END onbottom;

PROCEDURE BBP(a: ADDRESS; x,s: INTEGER; v: BITSET); CODE 0EDh END BBP;
PROCEDURE BBU(a: ADDRESS; x,s: INTEGER): BITSET; CODE 0ECh END BBU;

PROCEDURE drow_crs(x,y: INTEGER);
  VAR i: INTEGER; a,b: ADDRESS;
BEGIN
  IF (x<0) OR (x>hsize-crs_sx) THEN RETURN END;
  IF (y<SIZE(crsPic)-1) OR (y>=vsize) THEN RETURN END;
  IF NOT crsON THEN
    b:=lay+lsz+linesz*(vsize-y-1);
    FOR i:=0 TO HIGH(crsPic) DO
      savPic[i]:=BBU(b,x,crs_sx);
      BBP(b,x,crs_sx,savPic[i]/crsPic[i]);
      INC(b,linesz);
    END;
  ELSIF y>=crsY THEN
    IF (crsY=y) & (crsX=x) THEN RETURN END;
    a:=lay+lsz+linesz*(vsize-crsY-1);
    b:=lay+lsz+linesz*(vsize-y-1);
    FOR i:=0 TO HIGH(crsPic) DO
      BBP(a,crsX,crs_sx,savPic[i]);
      INC(a,linesz);
      savPic[i]:=BBU(b,x,crs_sx);
      BBP(b,x,crs_sx,savPic[i]/crsPic[i]);
      INC(b,linesz);
    END;
  ELSE
    a:=lay+lsz+(vsize-crsY-1+HIGH(crsPic))*linesz;
    b:=lay+lsz+(vsize-y-1+HIGH(crsPic))*linesz;
    FOR i:=HIGH(crsPic) TO 0 BY -1 DO
      BBP(a,crsX,crs_sx,savPic[i]);
      DEC(a,linesz);
      savPic[i]:=BBU(b,x,crs_sx);
      BBP(b,x,crs_sx,savPic[i]/crsPic[i]);
      DEC(b,linesz);
    END;
  END;
  crsON:=TRUE; crsX:=x; crsY:=y;
END drow_crs;

PROCEDURE del_crs;
  VAR i: INTEGER; a: ADDRESS;
BEGIN
  IF crsON THEN
    a:=lay+lsz+(vsize-crsY-1)*linesz;
    FOR i:=0 TO HIGH(crsPic) DO
      BBP(a,crsX,crs_sx,savPic[i]); INC(a,linesz)
    END;
    crsON:=FALSE;
  END;
END del_crs;

PROCEDURE mouse_xy(dx,dy: INTEGER);
BEGIN
  IF cpd.rel THEN
    INC(mouseX,dx); INC(mouseY,dy);
  ELSE
    mouseX:=dx; mouseY:=dy;
  END;
  IF mouseX<0 THEN mouseX:=0 END;
  IF mouseY<SIZE(crsPic)-1 THEN mouseY:=SIZE(crsPic)-1 END;
  IF mouseX>hsize-crs_sx THEN mouseX:=hsize-crs_sx END;
  IF mouseY>=vsize THEN mouseY:=vsize-1 END;
END mouse_xy;

PROCEDURE job;
-- job не должна вызывать open, close, remove, ontop при ch=0c
  PROCEDURE search(): window;
    VAR t: window;
  BEGIN
    t:=top;
    IF t=NIL THEN RETURN t END;
    REPEAT
      IF (t^.y<=mouseY) & (t^.y+t^.sy>mouseY) &
         (t^.x<=mouseX) & (t^.x+t^.sx>mouseX) THEN RETURN t END;
      t:=t^.fwd;
    UNTIL t=top;
    RETURN NIL;
  END search;
  PROCEDURE do_job(w: window; x,y: INTEGER; c: CHAR): window;
  BEGIN
    os.release(lock); w^.job(w,x,y,c); os.acquire(lock); RETURN search();
  END do_job;
  VAR dx,dy,id: INTEGER; ch: CHAR;
BEGIN
  os.acquire(lock);
  crs_w:=search();
  drow_crs(mouseX,mouseY);
  LOOP
    os.release(lock);
    cpd.wait; cpd.first(dx,dy,ch); cpd.drop;
    os.acquire(lock);
    IF (dx#0) OR (dy#0) OR NOT cpd.rel THEN
      mouse_xy(dx,dy);
      IF crs_w=NIL THEN
        crs_w:=search();
      ELSIF search()#crs_w THEN
        crs_w:=do_job(crs_w,-1000,-1000,0c)
      END;
      IF crs_w#NIL THEN
        id:=crs_w^.ident; crs_w:=do_job(crs_w,mouseX,mouseY,0c);
      END;
      WHILE (crs_w#NIL) & (crs_w^.ident#id) DO
        id:=crs_w^.ident; crs_w:=do_job(crs_w,mouseX,mouseY,0c);
      END;
    END;
    IF (ch#0c) & (crs_w#NIL) THEN
      id:=crs_w^.ident; crs_w:=do_job(crs_w,mouseX,mouseY,ch);
      WHILE (crs_w#NIL) & (crs_w^.ident#id) DO
        id:=crs_w^.ident; crs_w:=do_job(crs_w,mouseX,mouseY,0c);
      END;
    END;
    drow_crs(mouseX,mouseY);
  END;
END job;

PROCEDURE vector(x1,y1,x2,y2: INTEGER);
  PROCEDURE dln(mode: INTEGER; bmd: ADDRESS; x,y,x1,y1: INTEGER);
  CODE 0F9h 005h END dln;
  PROCEDURE clp(cp: ADDRESS; w,h:INTEGER): BOOLEAN;
  CODE 0F9h 004h END clp;
  VAR cp: RECORD xa,ya,xb,yb: INTEGER END;
BEGIN
  screen^.patt:={0..31};
  WITH cp DO
--    IF (x=x1) & (w^.patt={0..31})  THEN vline(w,x,y,y1); RETURN END;
--    IF (y=y1) & (w^.patt={0..31})  THEN hline(w,x,y,x1); RETURN END;
    IF x2<x1 THEN xa:=x2; xb:=x1; ya:=y2; yb:=y1;
    ELSE xa:=x1; xb:=x2; ya:=y1; yb:=y2;
    END;
    IF NOT clp(ADR(cp),hsize-1,vsize-1) THEN RETURN END;
    y1:=vsize-1-ya; y2:=vsize-1-yb;
    screen^.base:=lay+lsz;
    dln(2,screen,xa,y1,xb,y2)
  END;
END vector;

PROCEDURE read_point(m: rp_mode; VAR x,y,sx,sy: INTEGER; VAR ch: CHAR);
  VAR dx,dy,mx,my,x1,y1,x2,y2: INTEGER;
BEGIN
  ch:=0c;
  IF (m=rp_flow) & ((sx#mouseX) OR (sy#mouseY)) THEN
    sx:=mouseX; sy:=mouseY; RETURN
  END;
  os.acquire(lock);
  mx:=mouseX; my:=mouseY;
  LOOP
    drow_crs(mouseX,mouseY);
    IF m=rp_vect THEN
      vector(x,y,mouseX,mouseY);
    ELSIF m=rp_move THEN
      x1:=x+mouseX-mx; y1:=y+mouseY-my;
      x2:=x+sx+mouseX-mx; y2:=y+sy+mouseY-my;
      vector(x1,y1,x1,y2-1); vector(x1,y2,x2-1,y2);
      vector(x2,y2,x2,y1+1); vector(x2,y1,x1+1,y1);
    ELSIF m=rp_size THEN
      x1:=x; y1:=y;
      x2:=x+sx+mouseX-mx; y2:=y+sy+mouseY-my;
      IF x2-5<x1 THEN x2:=x1+5 END;
      IF y2-5<y1 THEN y2:=y1+5 END;
      vector(x1,y1,x1,y2-1); vector(x1,y2,x2-1,y2);
      vector(x2,y2,x2,y1+1); vector(x2,y1,x1+1,y1);
    END;
    cpd.wait; cpd.first(dx,dy,ch); cpd.drop;
    IF m=rp_vect THEN
      vector(x,y,mouseX,mouseY);
    ELSIF (m=rp_move) OR (m=rp_size) THEN
      vector(x1,y1,x1,y2-1); vector(x1,y2,x2-1,y2);
      vector(x2,y2,x2,y1+1); vector(x2,y1,x1+1,y1);
    END;
    mouse_xy(dx,dy);
    IF (ch#0c) OR (m=rp_flow) THEN
      IF m=rp_move THEN
        x:=x+mouseX-mx; y:=y+mouseY-my;
      ELSIF m=rp_size THEN
        sx:=sx+mouseX-mx; sy:=sy+mouseY-my;
      ELSE
        sx:=mouseX; sy:=mouseY;
      END;
      os.release(lock); RETURN
    END;
  END;
END read_point;

PROCEDURE wait;
BEGIN
  cpd.wait;
END wait;

PROCEDURE first(VAR x,y: INTEGER; VAR c: CHAR);
  VAR dx,dy: INTEGER;
BEGIN
  cpd.first(dx,dy,c);
  x:=mouseX; y:=mouseY;
  IF cpd.rel THEN INC(x,dx); INC(y,dy) ELSE x:=dx; y:=dy END;
  IF x<0 THEN x:=0 END;
  IF y<SIZE(crsPic)-1 THEN y:=SIZE(crsPic)-1 END;
  IF x>hsize-crs_sx THEN x:=hsize-crs_sx END;
  IF y>=vsize THEN y:=vsize-1 END;
END first;

PROCEDURE drop;
  VAR dx,dy: INTEGER; c: CHAR;
BEGIN
  cpd.first(dx,dy,c);
  mouse_xy(dx,dy);
  cpd.drop;
END drop;

PROCEDURE color(n,r,g,b: INTEGER);
  PROCEDURE p(n: INTEGER): INTEGER;
  BEGIN
    CASE n MOD 16 OF
      | 0.. 3: RETURN 15;
      | 4.. 7: RETURN 13;
      | 8..11: RETURN 14;
      |12..15: RETURN 12;
    END;
  END p;
  VAR a: ADDRESS;
BEGIN
  a:=ADDRESS(1F0010h)+n;
  a^:=p(r)+p(g)*16+p(b)*256;
END color;

PROCEDURE get_pal(n: INTEGER; VAR r,g,b: INTEGER);
BEGIN
  r:=pal[n].r; g:=pal[n].g; b:=pal[n].b;
END get_pal;

PROCEDURE put_pal(n: INTEGER; r,g,b: INTEGER);
BEGIN
  pal[n].r:=r; pal[n].g:=g; pal[n].b:=b;
  color(n,r,g,b);
END put_pal;

PROCEDURE init;
  VAR a,aa: ADDRESS; i,j: INTEGER;
BEGIN
  put_pal(00h, 0, 0, 0  );
  put_pal(01h, 4, 4, 4  );
  put_pal(02h, 8, 8, 8  );
  put_pal(03h,12,12,12  );
  put_pal(04h,12, 0, 0  );
  put_pal(05h,12, 4, 8  );
  put_pal(06h,12, 0, 8  );
  put_pal(07h,12, 0,12  );
  put_pal(08h, 0,12, 0  );
  put_pal(09h, 8,12, 8  );
  put_pal(0Ah, 0,12, 8  );
  put_pal(0Bh, 0,12,12  );
  put_pal(0Ch,12,12, 0  );
  put_pal(0Dh,12, 8, 4  );
  put_pal(0Eh, 8, 8,12  );
  put_pal(0Fh, 8, 4,12  );
  a :=ADDRESS(1F0000h);
  a^:=01FFh*200h*400h;
  zero(0,0,hsize,vsize);
END init;

VAR zero_ln,ones_ln,back_ln: ARRAY [0..24] OF INTEGER; i: INTEGER;

BEGIN
  ident:=0;
  crs_w:=NIL;
  os.ini_mutex(lock);
  lsno:=4; linesz:=16; hsize:=480; vsize:=360;
  lsz:=2000h; lsz2:=4000h; lsz3:=6000h; lay:=1F8000h;
  NEW(screen);
  screen^.mem :=lay;
  screen^.base:=lay;
  screen^.x   :=0;
  screen^.y   :=0;
  screen^.sx  :=480;
  screen^.sy  :=360;
  screen^.wpl :=16;
  screen^.wpp :=16*512;
  screen^.open:=FALSE;
  screen^.fwd :=screen;
  screen^.bck :=screen;
  screen^.patt:={0..31};
  screen^.mode:=0;
  screen^.color:=0;
  screen^.job :=empty;
  screen^.info:=NIL;
  screen^.W   :=0;
  screen^.E   :=479;
  screen^.S   :=0;
  screen^.N   :=359;
  mouseX:=hsize DIV 2; mouseY:=vsize DIV 2; crsON:=FALSE;
  crsPic[0]:={0..9};
  crsPic[1]:={0..1,7..8};
  crsPic[2]:={0..1,8..10};
  crsPic[3]:={0..1,3..5,10..12};
  crsPic[4]:={0..2,5..7,9..10};
  crsPic[5]:={7..8};
  top:=NIL;
  FOR i:=0 TO HIGH(zero_ln) DO
    zero_ln[i]:=0;
    ones_ln[i]:=-1;
    back_ln[i]:=INTEGER({0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30});
  END;
  zr:=ADR(zero_ln); one:=ADR(ones_ln); back:=ADR(back_ln);
  init;
END libWindows.
