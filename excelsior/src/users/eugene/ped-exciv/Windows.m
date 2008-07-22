IMPLEMENTATION MODULE Windows; (* Sem 31-Dec-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;
IMPORT  mcd: defCodes;
IMPORT  h  : cdsHeap;
IMPORT  sch: osKernel;
IMPORT  ex : ModelPbl;
IMPORT  VG;
FROM pedMouse   IMPORT  wait, first, drop, rel;

VAR linesz: INTEGER;
    lsz   : INTEGER;
    vsize : INTEGER;
    lay   : ADDRESS;
    zr    : ADDRESS;
    one   : ADDRESS;
    crs_ON: BOOLEAN;
    lock, crs_lock: sch.mutex_rec;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE mcd.move END MOVE;

PROCEDURE empty(w: window; x,y: INTEGER; ch: CHAR); END empty;

PROCEDURE emp(w: WORD); END emp;

PROCEDURE create(sx,sy: INTEGER): window;
  VAR sz: INTEGER; w: window;
BEGIN
  sz:=sx*sy*lsno;
  h.Allocate(w,SIZE(w^));
  h.Allocate(w^.mem,sz);
  w^.x:=0; w^.y:=0;
  w^.dsx:=sx*32;
  w^.sx:=sx; w^.sy:=sy;
  w^.open:=FALSE;
  w^.fwd:=w; w^.bck:=w;
  w^.mem^:=0;
  MOVE(w^.mem+1,w^.mem,sz-1);
  w^.cx:=0;
  w^.cy:=0;
  w^.cl:=w^.mem;
  w^.job:=empty;
  w^.info:=NIL;
  w^.ini_proc:=emp;
  w^.patt:={0..31};
  sch.ini_mutex(w^.lock);
  RETURN w;
END create;

PROCEDURE remove(w: window);
  VAR sz: INTEGER;
BEGIN
  ASSERT(w#NIL);
  ASSERT(w^.mem#NIL);
  ASSERT(w^.info=NIL);
  close(w);
  sz:=w^.sx*w^.sy*lsno;
  h.Deallocate(w^.mem,sz);
  h.Deallocate(w,SIZE(w^));
END remove;

PROCEDURE resize(w: window; sx,sy: INTEGER);
  VAR sz0,sz1: INTEGER;
BEGIN
  close(w);
  sz0:=w^.sx*w^.sy*lsno;
  sz1:=sx*sy*lsno;
  IF NOT h.Reallocate(w^.mem,sz0,sz1) THEN
    open(w);
    ex.Message:='Переполнена динамическая память, очень жаль...';
    ex.RaiseInMe(ex.MemoryOverflow);
    RETURN
  END;
  w^.dsx:=sx*32;
  w^.sx:=sx; w^.sy:=sy;
  w^.open:=FALSE;
  w^.fwd:=w; w^.bck:=w;
  w^.mem^:=0;
  MOVE(w^.mem+1,w^.mem,sz1-1);
END resize;

PROCEDURE dr_crs; FORWARD;

PROCEDURE ref_box(w: window; bxy,bxdy: INTEGER);

  PROCEDURE move(x,y,sx,sy: INTEGER);
    VAR dl,n: INTEGER; fr,to: ADDRESS;
  BEGIN
    IF x<0 THEN DEC(sx,-x); x:=0 END;
    IF y<0 THEN DEC(sy,-y); y:=0 END;
    IF x+sx>linesz THEN sx:=linesz-x END;
    IF y+sy>vsize  THEN sy:=vsize-y END;
    IF (sx<=0) OR (sy<=0) THEN RETURN END;
    to:=lay+y*linesz+x;
    fr:=w^.mem+(y-w^.y)*w^.sx+(x-w^.x);
    dl:=w^.sx*w^.sy; n:=sy;
    WHILE n>0 DO
      MOVE(to,fr,sx);
      MOVE(to+lsz,fr+dl,sx);
      MOVE(to+lsz*2,fr+dl*2,sx);
      MOVE(to+lsz*3,fr+dl*3,sx);
      DEC(n); INC(to,linesz); INC(fr,w^.sx);
    END;
  END move;

  PROCEDURE ref(t: window; x,y,sx,sy: INTEGER);
    VAR dy: INTEGER;
  BEGIN
    IF t=w THEN move(x,y,sx,sy); RETURN END;
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

BEGIN
  sch.acquire(lock);
  IF NOT w^.open THEN sch.release(lock); RETURN END;
  del_crs;
  IF bxy<0     THEN bxdy:=bxdy+bxy; bxy:=0 END;
  IF bxdy<0    THEN bxdy:=0 END;
  IF bxy>=w^.sy THEN bxy :=w^.sy-1     END;
  IF (bxy+bxdy)>w^.sy THEN bxdy:=w^.sy-bxy END;
  ref(top,w^.x,w^.y+bxy,w^.sx,bxdy);
  IF crs_ON THEN dr_crs END;
  sch.release(lock);
END ref_box;

PROCEDURE refresh(w: window);
BEGIN
  ref_box(w,0,w^.sy);
END refresh;

PROCEDURE open(w: window);
BEGIN
  sch.acquire(lock);
  IF w^.open THEN sch.release(lock); RETURN END;
  IF top#NIL THEN
    w^.fwd:=top; w^.bck:=top^.bck;
    w^.fwd^.bck:=w; w^.bck^.fwd:=w;
  END;
  top:=w;
  w^.open:=TRUE;
  sch.release(lock);
  refresh(w);
END open;

PROCEDURE close(w: window);
  PROCEDURE move(t: window; x,y,sx,sy: INTEGER);
    VAR dl,n: INTEGER; fr,to: ADDRESS;
  BEGIN
    IF x<0 THEN DEC(sx,-x); x:=0 END;
    IF y<0 THEN DEC(sy,-y); y:=0 END;
    IF x+sx>linesz THEN sx:=linesz-x END;
    IF y+sy>vsize  THEN sy:=vsize-y END;
    IF (sx<=0) OR (sy<=0) THEN RETURN END;
    to:=lay+y*linesz+x;
    fr:=t^.mem+(y-t^.y)*t^.sx+(x-t^.x);
    dl:=t^.sx*t^.sy; n:=sy;
    WHILE n>0 DO
      MOVE(to,fr,sx);
      MOVE(to+lsz,fr+dl,sx);
      MOVE(to+lsz*2,fr+dl*2,sx);
      MOVE(to+lsz*3,fr+dl*3,sx);
      DEC(n); INC(to,linesz); INC(fr,t^.sx);
    END;
  END move;
  PROCEDURE zero(x,y,sx,sy: INTEGER);
    VAR dl,n: INTEGER; to: ADDRESS;
  BEGIN
    IF x<0 THEN DEC(sx,-x); x:=0 END;
    IF y<0 THEN DEC(sy,-y); y:=0 END;
    IF x+sx>linesz THEN sx:=linesz-x END;
    IF y+sy>vsize  THEN sy:=vsize-y END;
    IF (sx<=0) OR (sy<=0) THEN RETURN END;
    to:=lay+y*linesz+x; n:=sy;
    WHILE n>0 DO
      MOVE(to,zr,sx);
      MOVE(to+lsz,zr,sx);
      MOVE(to+lsz*2,zr,sx);
      MOVE(to+lsz*3,zr,sx);
      DEC(n); INC(to,linesz);
    END;
  END zero;
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
BEGIN
  sch.acquire(lock);
  IF NOT w^.open THEN sch.release(lock); RETURN END;
  del_crs;
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
  IF crs_ON THEN dr_crs END;
  sch.release(lock);
END close;

PROCEDURE ontop(w: window);
BEGIN
  IF w=top THEN RETURN END;
  close(w); open(w);
END ontop;

PROCEDURE BBP(a: ADDRESS; x,s: INTEGER; v: BITSET); CODE 0EDh END BBP;
PROCEDURE BBU(a: ADDRESS; x,s: INTEGER): BITSET; CODE 0ECh END BBU;

TYPE blk=ARRAY [0..11] OF BITSET;

VAR crsX   : INTEGER;
    crsY   : INTEGER;
    mouseX : INTEGER;
    mouseY : INTEGER;
    crsON  : BOOLEAN;
    crsPic : blk;
    savPic : blk;
    layer: ARRAY [0..VG.layers-1] OF VG.BMD;

PROCEDURE drow_frame(x,y,sx,sy: INTEGER);
BEGIN
  VG.mode(layer[VG.layers-1],VG.xor);
  VG.frame(layer[VG.layers-1],x,layer[VG.layers-1].h-y-1,
           x+sx,layer[VG.layers-1].h-y-sy-1);
END drow_frame;

PROCEDURE fe(i: INTEGER); CODE 0FEh END fe;


PROCEDURE drow_crs(x,y: INTEGER);
  VAR i: INTEGER; a,b: ADDRESS;
BEGIN
  sch.acquire(crs_lock);
  IF NOT crsON THEN
    b:=lay+(VG.layers-1)*lsz+linesz*y;
    FOR i:=0 TO HIGH(crsPic) DO
      savPic[i]:=BBU(b,x,13);
      BBP(b,x,13,savPic[i]+crsPic[i]);
      INC(b,linesz);
    END;
  ELSIF y<=crsY THEN
    IF crsON & (crsY=y) & (crsX=x) THEN sch.release(crs_lock); RETURN END;
    a:=lay+(VG.layers-1)*lsz+linesz*crsY;
    b:=lay+(VG.layers-1)*lsz+linesz*y;
    FOR i:=0 TO HIGH(crsPic) DO
      BBP(a,crsX,13,savPic[i]);
      INC(a,linesz);
      savPic[i]:=BBU(b,x,13);
      BBP(b,x,13,savPic[i]+crsPic[i]); INC(b,linesz);
    END;
  ELSE
    a:=lay+(VG.layers-1)*lsz+(crsY+HIGH(crsPic))*linesz;
    b:=lay+(VG.layers-1)*lsz+(y+HIGH(crsPic))*linesz;
    FOR i:=HIGH(crsPic) TO 0 BY -1 DO
      BBP(a,crsX,13,savPic[i]);
      DEC(a,linesz);
      savPic[i]:=BBU(b,x,13);
      BBP(b,x,13,savPic[i]+crsPic[i]);
      DEC(b,linesz);
    END;
  END;
  crsON:=TRUE; crsX:=x; crsY:=y;
  sch.release(crs_lock);
END drow_crs;

PROCEDURE del_crs;
  VAR i: INTEGER; a: ADDRESS;
BEGIN
  sch.acquire(crs_lock);
  IF crsON THEN
    a:=lay+(VG.layers-1)*lsz+crsY*linesz;
    FOR i:=0 TO HIGH(crsPic) DO
      BBP(a,crsX,13,savPic[i]); INC(a,linesz)
    END;
    crsON:=FALSE;
  END;
  sch.release(crs_lock);
END del_crs;

PROCEDURE search(): window;
  VAR t: window;
BEGIN
  sch.acquire(lock);
  t:=top;
  IF t=NIL THEN sch.release(lock); RETURN t END;
  REPEAT
    IF (t^.y<=mouseY) & (t^.y+t^.sy>mouseY) &
       (t^.x*32<=mouseX) & ((t^.x+t^.sx)*32>mouseX) THEN
      sch.release(lock);
      RETURN t
    END;
    t:=t^.fwd;
  UNTIL t=top;
  sch.release(lock);
  RETURN NIL;
END search;

VAR First: BOOLEAN;
    never: sch.signal_rec;

PROCEDURE job;

-- job не должна вызывать open, close, remove, ontop при ch=0c

  VAR w,w1: window; dx,dy: INTEGER; ch: CHAR;

BEGIN
  IF NOT First THEN sch.wait(never) END;
  First:=FALSE;
  drow_crs(mouseX,mouseY);
  w:=search();
  LOOP
    wait; first(dx,dy,ch); drop;
    IF (dx#0) OR (dy#0) OR NOT rel THEN
      sch.acquire(lock);
      IF rel THEN
        INC(mouseX,dx); DEC(mouseY,dy);
      ELSE
        mouseX:=dx; mouseY:=490-dy;
      END;
      IF mouseX<0   THEN mouseX:=0   END;
      IF mouseX>(VG.dots-1) THEN mouseX:=VG.dots-1 END;
      IF mouseY<0   THEN mouseY:=0   END;
      IF mouseY>vsize-1 THEN mouseY:=vsize-1 END;
      sch.release(lock);
      w1:=search();
      IF (w1#w) & (w#NIL) THEN w^.job(w,-1,-1,0c) END;
      w:=w1;
      IF w#NIL THEN w^.job(w,mouseX,mouseY,0c) END;
    END;
    IF (ch#0c) & (w#NIL) THEN
      w^.job(w,mouseX,mouseY,ch);
      IF w#search() THEN w:=NIL END;
    END;
    crs_ON:=TRUE;
    drow_crs(mouseX,mouseY);
  END;
END job;

PROCEDURE dr_crs;
BEGIN drow_crs(mouseX,mouseY) END dr_crs;

PROCEDURE read_point(
          mode: INTEGER; w: window; VAR nx,ny: INTEGER; VAR ch: CHAR);
  VAR dx,dy: INTEGER;
      cx,cy: INTEGER;
BEGIN
  CASE mode OF
   0,2: drow_crs(mouseX,mouseY);
    |1: cx:=w^.x*32-mouseX;
        cy:=w^.y-mouseY;
        del_crs; crs_ON:=FALSE;
        drow_frame(mouseX+cx,mouseY+cy,w^.sx*32,w^.sy);
    |3: del_crs; crs_ON:=FALSE;
  ELSE END;
  IF mode=4 THEN
    del_crs; crs_ON:=FALSE;
    cx:=w^.dsx-mouseX;
    cy:=w^.sy -mouseY;
    drow_frame(w^.x*32,w^.y,mouseX+cx,mouseY+cy);
  ELSIF mode=5 THEN
    first(dx,dy,ch);
    IF (dx#0) OR (dy#0) OR NOT rel THEN
      sch.acquire(lock);
      IF rel THEN
        INC(mouseX,dx); DEC(mouseY,dy);
      ELSE
        mouseX:=dx; mouseY:=490-dy;
      END;
      IF mouseX<0   THEN mouseX:=0   END;
      IF mouseX>(VG.dots-1) THEN mouseX:=VG.dots-1 END;
      IF mouseY<0   THEN mouseY:=0   END;
      IF mouseY>vsize-1 THEN mouseY:=vsize-1 END;
      sch.release(lock);
    END;
    nx:=mouseX; ny:=mouseY; RETURN;
  END;
  LOOP
    wait; first(dx,dy,ch); drop;
    IF    mode=1 THEN drow_frame(mouseX+cx,mouseY+cy,w^.sx*32,w^.sy)
    ELSIF mode=4 THEN drow_frame(w^.x*32,w^.y,mouseX+cx,mouseY+cy) END;
    IF (dx#0) OR (dy#0) OR NOT rel THEN
      sch.acquire(lock);
      IF rel THEN
        INC(mouseX,dx); DEC(mouseY,dy);
      ELSE
        mouseX:=dx; mouseY:=490-dy;
      END;
      IF mouseX<0   THEN mouseX:=0   END;
      IF mouseX>(VG.dots-1) THEN mouseX:=VG.dots-1 END;
      IF mouseY<0   THEN mouseY:=0   END;
      IF mouseY>vsize-1 THEN mouseY:=vsize-1 END;
      sch.release(lock);
    END;
    IF (ch#0c) OR (mode IN {2,3}) THEN nx:=mouseX; ny:=mouseY; RETURN END;
    CASE mode OF
      |0,2: drow_crs(mouseX,mouseY);
      |1: drow_frame(mouseX+cx,mouseY+cy,w^.sx*32,w^.sy);
      |4: drow_frame(w^.x*32,w^.y,mouseX+cx,mouseY+cy);
    ELSE END;
  END;
END read_point;

PROCEDURE lock_window(w: window);
BEGIN sch.acquire(w^.lock); END lock_window;

PROCEDURE release_window(w: window);
BEGIN sch.release(w^.lock); END release_window;

VAR
  zero,ones: ARRAY [0..24] OF INTEGER;
  i: INTEGER;

BEGIN
  First:=TRUE;
  sch.ini_signal(never,{},0);
  sch.ini_mutex(lock);
  sch.ini_mutex(crs_lock);
  lsno:=VG.layers; linesz:=16; lsz:=16*512; vsize:=VG.lines;
  lay:=VG.layer[0].base;
  FOR i:=0 TO VG.layers-1 DO layer[i]:=VG.layer[i] END;
  FOR i:=0 TO VG.layers-1 DO VG.erase(layer[i]) END;
  VG.init_palette;
  mouseX:=0; mouseY:=0;
  crsON:=FALSE; crs_ON:=FALSE;
  crsPic[0]:={0};
  crsPic[1]:={1..2};
  crsPic[2]:={1..4};
  crsPic[3]:={2..6};
  crsPic[4]:={2..8};
  crsPic[5]:={3..7};
  crsPic[6]:={3..8};
  crsPic[7]:={4..9};
  crsPic[8]:={4,6..10};
  crsPic[9]:={7..11};
  crsPic[10]:={8..10};
  crsPic[11]:={9};
  top:=NIL;
  FOR i:=0 TO HIGH(zero) DO zero[i]:=0; ones[i]:=-1 END;
  zr:=ADR(zero); one:=ADR(ones);
END Windows.
