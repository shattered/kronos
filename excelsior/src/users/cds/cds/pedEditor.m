IMPLEMENTATION MODULE pedEditor; (*  10-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADR, ADDRESS;

IMPORT  os  : osKernel;
IMPORT  mcd : defCodes;
IMPORT  err : libCrash;
IMPORT  mem : libHeap;
IMPORT  wnd : libWindows;
IMPORT  vg  : cdsGrafic;
IMPORT  men : cdsMenu;
IMPORT  icn : cdsIcon;
IMPORT  mdl : pedModel;
IMPORT  pcu : pedCU;
IMPORT  mac : pedMacro;

IMPORT  tty : Terminal;

WITH STORAGE : mem;

TYPE
  picture = POINTER TO picture_rec;
  model   = POINTER TO model_rec;
  picture_rec = RECORD
    next     : picture;
    mdl      : model;
    window   : wnd.window;
    -- picture mode
    green_on : BOOLEAN;
    red_on   : BOOLEAN;
    other_on : BOOLEAN;
    green_lay: INTEGER;
    red_lay  : INTEGER;
    chips_on : BOOLEAN;
    pins_on  : BOOLEAN;
    text_on  : BOOLEAN;
    mode_wnd : wnd.window;
    mode_icn : icn.ICON;
    -- greed mode
    greed    : INTEGER;
    greed_on : BOOLEAN;
    greed_wnd: wnd.window;
    greed_icn: icn.ICON;
    -- tool
    fixed    : BOOLEAN;
    tl_size  : INTEGER;
    tl_vias  : INTEGER;
    tl_pipe  : INTEGER;
    -----
    lay      : INTEGER;
    signal   : mdl.signal;
    -- состояние экрана
    s: RECORD
      w_x,w_y: INTEGER; -- координаты окна относительно платы
      w_on   : BOOLEAN;
      w_w,w_e: INTEGER; -- границы правильной картинки относит. платы
      w_s,w_n: INTEGER;
      scale  : INTEGER;
    END;
    -- состояние редактора
    e: RECORD
      w_x,w_y: INTEGER;
      scale  : INTEGER;
    END;
  END;
  model_rec = RECORD
    brd      : mdl.board;
    pictures : picture;
    window   : wnd.window;
    changed  : BOOLEAN;
    process  : os.process;
    -- for painter
    wakeup   : os.signal_rec;
    lock     : os.mutex_rec;
    stop     : BOOLEAN;
  END;

CONST
  pic_top_sy = 20;
  pic_top_sx = 192;
  stp        = 16;

VAR
  op_x,op_y: INTEGER;

PROCEDURE picture_mode_job(w: wnd.window; x,y: INTEGER; c: CHAR);
  VAR p: picture;
  PROCEDURE ref;
    VAR m: model;
  BEGIN
    m:=p^.mdl; m^.stop:=TRUE; os.acquire(m^.lock); m^.stop:=FALSE;
    p^.s.w_on:=FALSE; os.send(m^.wakeup); os.release(m^.lock);
  END ref;
BEGIN
  IF c=0c THEN RETURN END;
  x:=x-w^.x; y:=y-w^.y; p:=w^.info;
  IF icn.do_icon(p^.mode_icn,x,y,c) THEN ref; RETURN END;
  IF c=wnd.l_on THEN
    IF (x>=w^.sx-stp-2) & (x<w^.sx-2) & (y>=2) & (y<2+stp) THEN
      wnd.remove(p^.mode_wnd); icn.remove(p^.mode_icn); RETURN
    ELSIF (x>=w^.sx-stp-2) & (x<w^.sx-2) & (y>=2+stp) & (y<2+stp*2) THEN
      x:=w^.x; y:=w^.y;
      REPEAT wnd.read_point(wnd.rp_move,x,y,w^.sx,w^.sy,c) UNTIL c#0c;
      wnd.close(w); w^.x:=x; w^.y:=y; wnd.open(w); RETURN
    END;
  END;
  IF c=wnd.r_on THEN wnd.onbottom(w);
  ELSIF c=wnd.l_on THEN wnd.ontop(w);
  END;
END picture_mode_job;

PROCEDURE open_picture_mode(p: picture);
  VAR w: wnd.window; r: BOOLEAN;
BEGIN
  IF p^.mode_wnd#NIL THEN wnd.ontop(p^.mode_wnd); RETURN END;
  w:=wnd.create(230,6*stp+9); p^.mode_wnd:=w;
  w^.x:=(wnd.screen^.sx-w^.sx) DIV 2;
  w^.y:=(wnd.screen^.sy-w^.sy) DIV 2;
  men.white_box(w,0,0,w^.sx,w^.sy);
  vg.mode(w,vg.rep); vg.color(w,1);
  vg.frame(w,4,4,w^.sx-5,w^.sy-5);
  vg.mode(w,vg.bic); vg.color(w,2);
  vg.vect(w,4,4,4,w^.sy-5);
  vg.vect(w,4,w^.sy-5,w^.sx-5,w^.sy-5);
  vg.mode(w,vg.or); vg.color(w,3); vg.prop_font(w,TRUE);
  vg.write_string(w,7,6+stp*5,'зеленый слой');
  vg.write_string(w,7,6+stp*4,'красный слой');
  vg.write_string(w,7,6+stp*3,'остальные слои');
  vg.write_string(w,7,6+stp*2,'контакты');
  vg.write_string(w,7,6+stp*1,'компоненты');
  vg.write_string(w,7,6+stp*0,'надписи');
  icn.on_off(w,p^.mode_icn,150,5+stp*5,p^.green_on);
  icn.on_off(w,p^.mode_icn,150,5+stp*4,p^.red_on);
  icn.on_off(w,p^.mode_icn,150,5+stp*3,p^.other_on);
  icn.on_off(w,p^.mode_icn,150,5+stp*2,p^.pins_on);
  icn.on_off(w,p^.mode_icn,150,5+stp*1,p^.chips_on);
  icn.on_off(w,p^.mode_icn,150,5+stp*0,p^.text_on);
  icn.number(w,p^.mode_icn,190,5+stp*5,30,0,7,p^.green_lay);
  icn.number(w,p^.mode_icn,190,5+stp*4,30,0,7,p^.red_lay);
  vg.mode(w,vg.rep); vg.color(w,15);
  vg.sign(w,w^.sx-stp-2,2,vg.sg_close);
  vg.sign(w,w^.sx-stp-2,2+stp,vg.sg_move);
  w^.job:=picture_mode_job;
  w^.info:=p;
  wnd.open(w);
END open_picture_mode;

PROCEDURE picture_greed_job(w: wnd.window; x,y: INTEGER; c: CHAR);
  VAR p: picture;
  PROCEDURE ref;
    VAR m: model;
  BEGIN
    m:=p^.mdl; m^.stop:=TRUE; os.acquire(m^.lock); m^.stop:=FALSE;
    p^.s.w_on:=FALSE; os.send(m^.wakeup); os.release(m^.lock);
  END ref;
BEGIN
  IF c=0c THEN RETURN END;
  x:=x-w^.x; y:=y-w^.y; p:=w^.info;
  IF icn.do_icon(p^.greed_icn,x,y,c) THEN ref; RETURN END;
  IF (c=wnd.l_on) & (y>=w^.sy-2-stp) & (y<w^.sy-2) THEN
    IF (x>=w^.sx-stp-2) & (x<w^.sx-2) THEN
      wnd.remove(p^.greed_wnd); icn.remove(p^.greed_icn); RETURN
    ELSIF (x>=w^.sx-stp*2-2) & (x<w^.sx-stp-2) THEN
      x:=w^.x; y:=w^.y;
      REPEAT wnd.read_point(wnd.rp_move,x,y,w^.sx,w^.sy,c) UNTIL c#0c;
      wnd.close(w); w^.x:=x; w^.y:=y; wnd.open(w); RETURN
    END;
  END;
  IF c=wnd.r_on THEN wnd.onbottom(w);
  ELSIF c=wnd.l_on THEN wnd.ontop(w);
  END;
END picture_greed_job;

PROCEDURE open_picture_greed(p: picture);
  VAR w: wnd.window;
BEGIN
  IF p^.greed_wnd#NIL THEN wnd.ontop(p^.greed_wnd); RETURN END;
  w:=wnd.create(232,2*stp+9); p^.greed_wnd:=w;
  w^.x:=(wnd.screen^.sx-w^.sx) DIV 2;
  w^.y:=(wnd.screen^.sy-w^.sy) DIV 2;
  men.white_box(w,0,0,w^.sx,w^.sy);
  vg.mode(w,vg.rep); vg.color(w,1);
  vg.frame(w,4,4,w^.sx-5,w^.sy-5);
  vg.mode(w,vg.bic); vg.color(w,2);
  vg.vect(w,4,4,4,w^.sy-5);
  vg.vect(w,4,w^.sy-5,w^.sx-5,w^.sy-5);
  vg.mode(w,vg.or); vg.color(w,3); vg.prop_font(w,TRUE);
  vg.write_string(w,50,6+stp,'сетка');
  icn.on_off(w,p^.greed_icn,150,5+stp,p^.greed_on);
  icn.pcb_number(w,p^.greed_icn,5,5,p^.greed);
  vg.mode(w,vg.rep); vg.color(w,15);
  vg.sign(w,w^.sx-stp  -2,w^.sy-2-stp,vg.sg_close);
  vg.sign(w,w^.sx-stp*2-2,w^.sy-2-stp,vg.sg_move);
  w^.job:=picture_greed_job;
  w^.info:=p;
  wnd.open(w);
END open_picture_greed;

PROCEDURE tie_to_greed(p: picture; VAR x,y: INTEGER);
BEGIN
  x:=(x+p^.greed DIV 2) DIV p^.greed * p^.greed;
  y:=(y+p^.greed DIV 2) DIV p^.greed * p^.greed;
END tie_to_greed;

PROCEDURE b_w(p: picture; bx,by: INTEGER; VAR x,y: INTEGER);
BEGIN
  x:=(bx-p^.e.w_x) DIV p^.e.scale;
  y:=(by-p^.e.w_y) DIV p^.e.scale;
END b_w;

PROCEDURE b_s(p: picture; bx,by: INTEGER; VAR x,y: INTEGER);
BEGIN
  x:=(bx-p^.e.w_x) DIV p^.e.scale + p^.window^.x;
  y:=(by-p^.e.w_y) DIV p^.e.scale + p^.window^.y;
END b_s;

PROCEDURE w_b(p: picture; wx,wy: INTEGER; VAR x,y: INTEGER);
BEGIN
  x:=wx*p^.e.scale+p^.e.w_x;
  y:=wy*p^.e.scale+p^.e.w_y;
END w_b;

PROCEDURE s_b(p: picture; sx,sy: INTEGER; VAR x,y: INTEGER);
BEGIN
  x:=(sx-p^.window^.x)*p^.e.scale+p^.e.w_x;
  y:=(sy-p^.window^.y)*p^.e.scale+p^.e.w_y;
END s_b;

PROCEDURE line(wd: wnd.window; x0,y0,x1,y1,w: INTEGER);
  VAR h,v,l: INTEGER;
      dx,dy: INTEGER;
    x0u,x0d: INTEGER;   x1u,x1d: INTEGER;
    y0u,y0d: INTEGER;   y1u,y1d: INTEGER;
  PROCEDURE SQRT(n:INTEGER): INTEGER;
    VAR l,r: INTEGER;
  BEGIN
    IF n<0 THEN RETURN SQRT(-n) END;
    IF n<2 THEN RETURN n END;
    l:=1; r:=n;
    REPEAT r:=(l+r)DIV 2; l:=n DIV r UNTIL l>=r;
    RETURN r
  END SQRT;
BEGIN
  IF w<2 THEN vg.vect(wd,x0,y0,x1,y1); RETURN END;
  h:=x1-x0; v:=y1-y0;
  IF h=0 THEN l:=ABS(v)
  ELSIF v=0 THEN l:=ABS(h)
  ELSE l:=SQRT(h*h+v*v);
  END;
  IF l=0 THEN RETURN END;
  dx:=v*w/l;
  dy:=h*w/l;
  IF (dx=0) & (dy=0) THEN vg.vect(wd,x0,y0,x1,y1); RETURN END;
  x0u:=x0-dx; x0d:=x0+dx;
  y0u:=y0+dy; y0d:=y0-dy;
  x1u:=x1-dx; x1d:=x1+dx;
  y1u:=y1+dy; y1d:=y1-dy;
  vg.arc(wd,x1,y1,x1d,y1d,x1u,y1u,w);
  vg.arc(wd,x0,y0,x0u,y0u,x0d,y0d,w);
  vg.vect(wd,x0u,y0u,x1u,y1u);
  vg.vect(wd,x1d,y1d,x0d,y0d);
END line;

PROCEDURE drow_conductor(p: picture; wd: wnd.window;
                         md: INTEGER; VAL s: pcu.segment);
-- md: 0 normal, 1 bright, 2 bright only;
  VAR x,y,x1,y1,c,r: INTEGER; l: BITSET;
BEGIN
  l:=s.lays;
  IF md#0 THEN c:=2 ELSE c:=0 END;
  IF md#2 THEN
    IF p^.red_on & (p^.red_lay IN l) THEN c:=c+4; EXCL(l,p^.red_lay) END;
    IF p^.green_on & (p^.green_lay IN l) THEN c:=c+8; EXCL(l,p^.green_lay) END;
    IF p^.other_on & (l#{}) THEN c:=c+1 END;
  END;
  vg.color(wd,c);
  IF (s.x1=s.x2) & (s.y1=s.y2) THEN
    b_w(p,s.x1,s.y1,x,y);
    r:=s.size DIV p^.e.scale;
    vg.circ(wd,x,y,r);
    IF s.pipe#0 THEN
      r:=s.pipe DIV p^.e.scale;
      vg.color(wd,15); vg.circ(wd,x,y,r);
    END;
  ELSE
    b_w(p,s.x1,s.y1,x,y); b_w(p,s.x2,s.y2,x1,y1);
    line(wd,x,y,x1,y1,s.size DIV p^.e.scale);
  END;
END drow_conductor;

PROCEDURE edit_proc(m: model; s: mdl.signal; n: INTEGER; ins: BOOLEAN);
  VAR seg: pcu.segment; p: picture;
BEGIN
  m^.changed:=TRUE;
  pcu.unpack(s,n,seg);
  p:=m^.pictures;
  WHILE p#NIL DO
    IF ins THEN
      vg.mode(p^.window,vg.rep);
      IF seg.fix THEN p^.window^.patt:={0..31}
      ELSE p^.window^.patt:={0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30}
      END;
    ELSE vg.mode(p^.window,vg.bic); p^.window^.patt:={0..31};
    END;
    p^.window^.N:=p^.window^.sy-pic_top_sy-2;
    IF s=p^.signal THEN drow_conductor(p,p^.window,1,seg);
    ELSE drow_conductor(p,p^.window,0,seg);
    END;
    p^.window^.N:=p^.window^.sy-1;
    p:=p^.next;
  END;
END edit_proc;

PROCEDURE drow_greed(p: picture; w: wnd.window; md: INTEGER;
                     VAL box: pcu.range);
-- md:  0 erase + drow greed, 1 drow rgeed only
  VAR
    x1,y1,x2,y2,d,n,i: INTEGER;
    ln: ARRAY [0..31] OF BITSET; a: ADDRESS;
  PROCEDURE bblt(t: ADDRESS; ot: INTEGER;
                 f: ADDRESS; of,s: INTEGER); CODE mcd.bblt END bblt;
BEGIN
  b_w(p,box.x1,box.y1,x1,y1);
  b_w(p,box.x2,box.y2,x2,y2);
  IF x1<w^.W THEN x1:=w^.W END;
  IF x2>w^.E THEN x2:=w^.E END;
  IF y1<w^.S THEN y1:=w^.S END;
  IF y2>w^.N THEN y2:=w^.N END;
  IF md=0 THEN vg.color(w,15); vg.mode(w,vg.bic); vg.box(w,x1,y1,x2,y2) END;
  IF NOT p^.greed_on THEN RETURN END;
  d:=p^.greed DIV p^.e.scale;
  IF d<3 THEN RETURN END;
  IF p^.greed MOD p^.e.scale # 0 THEN RETURN END;
  IF SIZE(ln)*32<w^.sx THEN RETURN END;
  FOR i:=0 TO HIGH(ln) DO ln[i]:={} END;
  n:=(p^.e.w_x + p^.e.scale DIV 2) DIV p^.e.scale;
  FOR i:=0 TO w^.sx-1 DO
    IF (n MOD d)=0 THEN INCL(ln[i DIV 32],i MOD 32) END; INC(n);
  END;
  y1:=y1+(d-(y1+(p^.e.w_y + p^.e.scale DIV 2) DIV p^.e.scale)) MOD d;
  a:=w^.mem+(w^.sy-y1-1)*w^.wpl; n:=w^.wpl*d;
  WHILE y1<=y2 DO
    bblt(a,x1,ADR(ln),x1,x2-x1+1);
    DEC(a,n); INC(y1,d);
  END;
END drow_greed;

PROCEDURE drow_signal(p: picture; w: wnd.window; md: INTEGER; del: BOOLEAN;
                      s: mdl.signal; VAL box: pcu.range);
-- md:  0 nirmal, 1 bright only;
  VAR n,m: INTEGER; seg: pcu.segment;
BEGIN
  IF HIGH(s^.cu)<0 THEN RETURN END;
  IF md=1 THEN m:=2 ELSIF s=p^.signal THEN m:=1 ELSE m:=0 END;
  IF del THEN vg.mode(w,vg.bic) ELSE vg.mode(w,vg.rep) END;
  n:=0;
  pcu.SkipSegments(s,box,n);
  WHILE n<=HIGH(s^.cu) DO
    pcu.unpack(s,n,seg); INC(n);
    IF del OR seg.fix THEN w^.patt:={0..31}
    ELSE w^.patt:={0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30}
    END;
    drow_conductor(p,w,m,seg);
    pcu.SkipSegments(s,box,n);
  END;
END drow_signal;

PROCEDURE drow_chip(p: picture; w: wnd.window;
                    c: mdl.chip; VAL box: pcu.range);
  VAR x1,y1,x2,y2: INTEGER;
BEGIN
  IF NOT p^.chips_on THEN RETURN END;
  x1:=c^.x; y1:=c^.y;
  CASE c^.r OF
    |0: x1:=c^.x; y1:=c^.y; x2:=c^.x+c^.type^.x-1; y2:=c^.y+c^.type^.y-1;
    |1: x1:=c^.x; y2:=c^.y; x2:=c^.x+c^.type^.y-1; y1:=c^.y-c^.type^.x+1;
    |2: x2:=c^.x; y2:=c^.y; x1:=c^.x-c^.type^.x+1; y1:=c^.y-c^.type^.y+1;
    |3: x2:=c^.x; y1:=c^.y; x1:=c^.x-c^.type^.y+1; y2:=c^.y+c^.type^.x-1;
  END;
  IF (x1>box.x2) OR (x2<box.x1) THEN RETURN END;
  IF (y1>box.y2) OR (y2<box.y1) THEN RETURN END;
  b_w(p,x1,y1,x1,y1);
  b_w(p,x2,y2,x2,y2);
  vg.color(w,2); vg.mode(w,vg.rep); w^.patt:={0..31};
  vg.frame(w,x1,y1,x2,y2);
tty.print('%d,%d-%d,%d\n',x1,y1,x2,y2);
END drow_chip;

PROCEDURE update_box_picture(m: model; p: picture; box: pcu.range);
  VAR i,x1,x2,y1,y2: INTEGER; b: mdl.board; w: wnd.window;
BEGIN
  IF NOT p^.s.w_on THEN RETURN END;
  IF p^.s.w_w>box.x1 THEN box.x1:=p^.s.w_w END;
  IF p^.s.w_e<box.x2 THEN box.x2:=p^.s.w_e END;
  IF box.x1>box.x2 THEN RETURN END;
  IF p^.s.w_s>box.y1 THEN box.y1:=p^.s.w_s END;
  IF p^.s.w_n<box.y2 THEN box.y2:=p^.s.w_n END;
  IF box.y1>box.y2 THEN RETURN END;
  w:=p^.window; b:=m^.brd; w^.N:=w^.sy-pic_top_sy-2;
  drow_greed(p,w,0,box);
  FOR i:=0 TO HIGH(b^.sigs) DO drow_signal(p,w,0,FALSE,b^.sigs[i],box) END;
  FOR i:=0 TO HIGH(b^.chps) DO drow_chip(p,w,b^.chps[i],box) END;
  w^.N:=w^.sy-1;
  wnd.refresh(w);
END update_box_picture;

PROCEDURE update_box_model(m: model; VAL box: pcu.range);
  VAR p: picture;
BEGIN
  p:=m^.pictures;
  WHILE p#NIL DO
    update_box_picture(m,p,box);
    p:=p^.next;
  END;
END update_box_model;

PROCEDURE update_picture(p: picture; w: wnd.window);
  VAR
    m: model; b: mdl.board;
    i,j,W,E,S,N: INTEGER;
    box: pcu.range;
BEGIN
  m:=p^.mdl; b:=m^.brd;
  IF p^.s.scale#p^.e.scale THEN
    p^.s.w_on:=FALSE; p^.s.scale:=p^.e.scale;
  END;
  W:=w^.W*p^.s.scale; E:=w^.E*p^.s.scale;
  S:=w^.S*p^.s.scale; N:=w^.N*p^.s.scale;
  IF p^.s.w_on THEN
    IF p^.e.w_x+W>p^.s.w_w THEN p^.s.w_w:=p^.e.w_x+W END;
    IF p^.e.w_x+E<p^.s.w_e THEN p^.s.w_e:=p^.e.w_x+E END;
    IF p^.e.w_y+S>p^.s.w_s THEN p^.s.w_s:=p^.e.w_y+S END;
    IF p^.e.w_y+N<p^.s.w_n THEN p^.s.w_n:=p^.e.w_y+N END;
    IF (p^.s.w_w>p^.s.w_e) OR (p^.s.w_s>p^.s.w_n) THEN
      p^.s.w_on:=FALSE;
    ELSE
      i:=(p^.s.w_x-p^.e.w_x) DIV p^.e.scale;
      j:=(p^.s.w_y-p^.e.w_y) DIV p^.e.scale;
      IF (i#0) OR (j#0) THEN vg.roll(w,i,j) END;
    END;
  END;
  p^.s.w_x:=p^.e.w_x; p^.s.w_y:=p^.e.w_y;
  IF NOT p^.s.w_on THEN
    p^.s.w_w:=p^.s.w_x+W; p^.s.w_e:=p^.s.w_x+E;
    p^.s.w_s:=p^.s.w_y+S; p^.s.w_n:=p^.s.w_y+N;
    box.x1:=p^.s.w_w; box.x2:=p^.s.w_e;
    box.y1:=p^.s.w_s; box.y2:=p^.s.w_n;
    drow_greed(p,w,0,box);
    vg.mode(w,vg.rep);
    FOR i:=0 TO HIGH(b^.sigs) DO
      drow_signal(p,w,0,FALSE,b^.sigs[i],box);
      IF m^.stop THEN RETURN END;
    END;
    FOR i:=0 TO HIGH(b^.chps) DO
      drow_chip(p,w,b^.chps[i],box);
      IF m^.stop THEN RETURN END;
    END;
    p^.s.w_on:=TRUE;
  END;
  IF p^.s.w_w>p^.s.w_x+W THEN
    box.x1:=p^.s.w_x+W; box.x2:=p^.s.w_w-1;
    box.y1:=p^.s.w_s; box.y2:=p^.s.w_n;
    drow_greed(p,w,1,box);
    FOR i:=0 TO HIGH(b^.sigs) DO
      drow_signal(p,w,0,FALSE,b^.sigs[i],box);
      IF m^.stop THEN RETURN END;
    END;
    FOR i:=0 TO HIGH(b^.chps) DO
      drow_chip(p,w,b^.chps[i],box);
      IF m^.stop THEN RETURN END;
    END;
    p^.s.w_w:=p^.s.w_x+W;
  END;
  IF p^.s.w_e<p^.s.w_x+E THEN
    box.x1:=p^.s.w_e+1; box.x2:=p^.s.w_x+E;
    box.y1:=p^.s.w_s; box.y2:=p^.s.w_n;
    drow_greed(p,w,1,box);
    FOR i:=0 TO HIGH(b^.sigs) DO
      drow_signal(p,w,0,FALSE,b^.sigs[i],box);
      IF m^.stop THEN RETURN END;
    END;
    FOR i:=0 TO HIGH(b^.chps) DO
      drow_chip(p,w,b^.chps[i],box);
      IF m^.stop THEN RETURN END;
    END;
    p^.s.w_e:=p^.s.w_x+E;
  END;
  IF p^.s.w_s>p^.s.w_y+S THEN
    box.x1:=p^.s.w_w; box.x2:=p^.s.w_e;
    box.y1:=p^.s.w_y+S; box.y2:=p^.s.w_s-1;
    drow_greed(p,w,1,box);
    FOR i:=0 TO HIGH(b^.sigs) DO
      drow_signal(p,w,0,FALSE,b^.sigs[i],box);
      IF m^.stop THEN RETURN END;
    END;
    FOR i:=0 TO HIGH(b^.chps) DO
      drow_chip(p,w,b^.chps[i],box);
      IF m^.stop THEN RETURN END;
    END;
    p^.s.w_s:=p^.s.w_y+S;
  END;
  IF p^.s.w_n<p^.s.w_y+N THEN
    box.x1:=p^.s.w_w; box.x2:=p^.s.w_e;
    box.y1:=p^.s.w_n+1; box.y2:=p^.s.w_y+N;
    drow_greed(p,w,1,box);
    FOR i:=0 TO HIGH(b^.sigs) DO
      drow_signal(p,w,0,FALSE,b^.sigs[i],box);
      IF m^.stop THEN RETURN END;
    END;
    FOR i:=0 TO HIGH(b^.chps) DO
      drow_chip(p,w,b^.chps[i],box);
      IF m^.stop THEN RETURN END;
    END;
    p^.s.w_n:=p^.s.w_y+N;
  END;
  vg.color(w,1); vg.mode(w,vg.rep);
  b_w(p,0,0,i,j); vg.sign(w,i,j,vg.sg_empty);
END update_picture;

PROCEDURE painter;
  PROCEDURE POP(): model; CODE END POP;
  VAR
    m: model; p: picture; w: wnd.window;
    buf: ARRAY [0..SIZE(w^)-1] OF INTEGER;
BEGIN
  m:=POP(); w:=ADR(buf);
  LOOP
    os.wait(m^.wakeup);
    os.acquire(m^.lock);
    p:=m^.pictures;
    WHILE NOT m^.stop & (p#NIL) DO
      w^:=p^.window^;
      w^.N:=w^.sy-pic_top_sy-2;
      update_picture(p,w);
      wnd.refresh(p^.window); -- ,0,p^.window^.sy-pic_top_sy);
      p:=p^.next;
    END;
    os.release(m^.lock);
  END;
END painter;

PROCEDURE close_picture(VAR p: picture);
  VAR ap: POINTER TO picture;
BEGIN
  wnd.remove(p^.mode_wnd);
  icn.remove(p^.mode_icn);
  wnd.remove(p^.greed_wnd);
  icn.remove(p^.greed_icn);
  os.acquire(p^.mdl^.lock);
  ap:=ADR(p^.mdl^.pictures);
  WHILE ap^#NIL DO
    IF ap^=p THEN ap^:=p^.next ELSE ap:=ADR(ap^^.next) END;
  END;
  wnd.remove(p^.window);
  os.release(p^.mdl^.lock);
  DISPOSE(p);
END close_picture;

PROCEDURE move_picture(m: model; p: picture; w: wnd.window; x,y: INTEGER);
  VAR x1,y1: INTEGER; c: CHAR;
BEGIN
  x:=x+w^.x; y:=y+w^.y;
  m^.stop:=TRUE; os.acquire(m^.lock); m^.stop:=FALSE;
  REPEAT wnd.read_point(wnd.rp_vect,x,y,x1,y1,c) UNTIL c=wnd.m_off;
  p^.e.w_x:=p^.e.w_x+(x-x1)*p^.e.scale;
  p^.e.w_y:=p^.e.w_y+(y-y1)*p^.e.scale;
  os.send(m^.wakeup); os.release(m^.lock);
END move_picture;

PROCEDURE move_picture_lock(m: model; p: picture; w: wnd.window; x,y: INTEGER);
  VAR x1,y1: INTEGER; c: CHAR;
BEGIN
  x:=x+w^.x; y:=y+w^.y;
  REPEAT wnd.read_point(wnd.rp_vect,x,y,x1,y1,c) UNTIL c=wnd.m_off;
  p^.e.w_x:=p^.e.w_x+(x-x1)*p^.e.scale;
  p^.e.w_y:=p^.e.w_y+(y-y1)*p^.e.scale;
  w^.N:=w^.sy-pic_top_sy-2;
  update_picture(p,w);
  w^.N:=w^.sy-1;
END move_picture_lock;

PROCEDURE set_bright_signal(m: model; p: picture; w: wnd.window; s: mdl.signal);
  VAR bx: pcu.range;
BEGIN
  IF p^.signal=s THEN RETURN END;
  os.acquire(m^.lock);
  IF p^.s.w_on THEN
    w^.N:=w^.sy-pic_top_sy-2;
    bx.x1:=p^.s.w_w; bx.x2:=p^.s.w_e; bx.y1:=p^.s.w_s; bx.y2:=p^.s.w_n;
    IF p^.signal#NIL THEN
      drow_signal(p,w,1,TRUE,p^.signal,bx);
    END;
    IF s#NIL THEN
      drow_signal(p,w,1,FALSE,s,bx);
    END;
    w^.N:=w^.sy-1;
    wnd.refresh(w);
  END;
  p^.signal:=s;
  os.release(m^.lock);
END set_bright_signal;

PROCEDURE set_lay(p: picture; w: wnd.window; l: INTEGER);
  VAR c: CHAR;
BEGIN
  IF l=p^.red_lay THEN c:=vg.sg_red
  ELSIF l=p^.green_lay THEN c:=vg.sg_green
  ELSE c:=vg.sg_empty
  END;
  p^.lay:=l;
  vg.mode(w,vg.rep); vg.color(w,15);
  vg.sign(w,w^.sx-(vg.sign_w+2)*5,w^.sy-vg.sign_h-2,c);
  wnd.refresh(w);
END set_lay;

PROCEDURE track_seq(m: model; p: picture; w: wnd.window;
                    x,y: INTEGER; ins: BOOLEAN);
  VAR
    c,crs: CHAR; rng: pcu.range; x1,y1,cx,cy: INTEGER; r: BOOLEAN;
    pp: picture;
BEGIN
  IF NOT ins THEN
    REPEAT wnd.read_point(wnd.rp_free,x,y,x,y,c) UNTIL c=wnd.l_off;
    x:=x-w^.x; y:=y-w^.y;
  END;
  w_b(p,x,y,rng.x1,rng.y1);
  tie_to_greed(p,rng.x1,rng.y1);
  os.acquire(m^.lock);
  LOOP
    IF ins THEN crs:=vg.sg_cursor1 ELSE crs:=vg.sg_cursor2 END;
    b_w(p,rng.x1,rng.y1,cx,cy);
    x:=cx+w^.x; DEC(cx,vg.sign_w DIV 2);
    y:=cy+w^.y; DEC(cy,vg.sign_h DIV 2);
    IF p^.lay=p^.green_lay THEN vg.color(w,11) ELSE vg.color(w,7) END;
    vg.mode(w,vg.xor);
    vg.sign(w,cx,cy,crs); wnd.ref_box(w,cy,vg.sign_h);
    REPEAT wnd.read_point(wnd.rp_vect,x,y,x1,y1,c) UNTIL
       (c=wnd.l_on) OR (c=wnd.m_on) OR (c=wnd.r_on);
    vg.sign(w,cx,cy,crs); wnd.ref_box(w,cy,vg.sign_h);
    x1:=x1-w^.x; y1:=y1-w^.y;
    IF c=wnd.l_on THEN
      w_b(p,x1,y1,rng.x2,rng.y2);
      tie_to_greed(p,rng.x2,rng.y2);
      IF (rng.x1=rng.x2) & (rng.y1=rng.y2) THEN
        IF ins THEN
          r:=pcu.InsertVias(m^.brd,p^.signal,edit_proc,m,rng.x1,rng.y1,
            p^.tl_vias,p^.tl_pipe,m^.brd^.lays,p^.fixed);
        ELSE
          pcu.DeleteVias(p^.signal,edit_proc,m,rng.x1,rng.y1); r:=FALSE;
        END;
        IF p^.green_on & p^.red_on THEN
          IF p^.lay=p^.green_lay THEN p^.lay:=p^.red_lay
          ELSE p^.lay:=p^.green_lay
          END;
        END;
      ELSIF ins THEN
        r:=pcu.InsertRange(m^.brd,p^.signal,edit_proc,m,
          rng,p^.tl_size,p^.lay,p^.fixed);
      ELSE
        pcu.DeleteRange(p^.signal,edit_proc,m,rng,p^.lay); r:=FALSE;
      END;
      rng.x1:=rng.x2; rng.y1:=rng.y2;
      pp:=m^.pictures;
      WHILE pp#NIL DO wnd.refresh(pp^.window); pp:=pp^.next END;
    ELSIF c=wnd.m_on THEN
      move_picture_lock(m,p,w,x1,y1); wnd.refresh(w);
    ELSIF c=wnd.r_on THEN EXIT
    END;
  END;
  os.release(m^.lock);
END track_seq;

PROCEDURE cu_track(m: model; p: picture; w: wnd.window; x,y: INTEGER);
  VAR
    sn : INTEGER;
    s  : mdl.signal;
    c  : CHAR;
    rng: pcu.range;
BEGIN
  IF NOT wnd.ontop?(w) THEN wnd.ontop(w); RETURN END;
  w_b(p,x,y,rng.x1,rng.y1);
  s:=pcu.FindSignal(m^.brd,NIL,rng.x1,rng.y1,p^.lay,1,sn);
  IF s#NIL THEN set_bright_signal(m,p,w,s) END;
  IF p^.signal=NIL THEN
    men.message(-1,w^.y+y-20,0,4,'Укажите сигнал.');
  ELSE
    REPEAT wnd.read_point(wnd.rp_free,x,y,x,y,c) UNTIL c#0c;
    x:=x-w^.x; y:=y-w^.y;
    IF c=wnd.l_off THEN track_seq(m,p,w,x,y,TRUE);
    ELSIF c=wnd.r_on THEN track_seq(m,p,w,x,y,FALSE);
    END;
  END;
END cu_track;

PROCEDURE copy_cu(m: model; p: picture; w: wnd.window; md: INTEGER);
-- md: 0 copy to macro, 1 delete box;
  VAR x,y,sx,sy,x2,y2,cx,cy,cx2,cy2: INTEGER; c: CHAR;
    rng: pcu.range; new: mdl.board; pp: picture;
BEGIN
  LOOP
    x:=wnd.mouseX; y:=wnd.mouseY; sx:=20; sy:=20;
    REPEAT wnd.read_point(wnd.rp_move,x,y,sx,sy,c) UNTIL c#0c;
    IF c=wnd.r_on THEN RETURN END;
    IF c=wnd.l_on THEN EXIT END;
    IF c=wnd.m_on THEN
      m^.stop:=TRUE; os.acquire(m^.lock); m^.stop:=FALSE;
      move_picture_lock(m,p,w,x-w^.x,y-w^.y);
      wnd.refresh(w); os.release(m^.lock);
    END;
  END;
  os.acquire(m^.lock); w^.N:=w^.sy-pic_top_sy-2;
  s_b(p,x,y,rng.x1,rng.y1);
  tie_to_greed(p,rng.x1,rng.y1);
  b_s(p,rng.x1,rng.y1,x,y);
  b_w(p,rng.x1,rng.y1,cx,cy);
  vg.color(w,3); vg.mode(w,vg.xor); w^.patt:={0..31};
  vg.sign(w,cx-vg.sign_w DIV 2,cy-vg.sign_h DIV 2,vg.sg_cursor1);
  wnd.refresh(w); INC(wnd.mouseX,sx); INC(wnd.mouseY,sy);
  LOOP
    REPEAT wnd.read_point(wnd.rp_size,x,y,sx,sy,c) UNTIL c#0c;
    IF c=wnd.r_on THEN
      vg.sign(w,cx-vg.sign_w DIV 2,cy-vg.sign_h DIV 2,vg.sg_cursor1);
      w^.N:=w^.sy-1; wnd.refresh(w); os.release(m^.lock); RETURN
    END;
    IF c=wnd.l_on THEN EXIT END;
  END;
  s_b(p,x+sx-1,y+sy-1,rng.x2,rng.y2);
  tie_to_greed(p,rng.x2,rng.y2);
  b_s(p,rng.x2,rng.y2,x2,y2);
  b_w(p,rng.x2,rng.y2,cx2,cy2);
  IF (cx2<=cx) OR (cy2<=cy) THEN
    vg.sign(w,cx-vg.sign_w DIV 2,cy-vg.sign_h DIV 2,vg.sg_cursor1);
    w^.N:=w^.sy-1; wnd.refresh(w); os.release(m^.lock); RETURN
  END;
  vg.sign(w,cx2-vg.sign_w DIV 2,cy2-vg.sign_h DIV 2,vg.sg_cursor1);
  vg.frame(w,cx,cy,cx2,cy2);
  wnd.refresh(w);
  REPEAT wnd.read_point(wnd.rp_free,x2,y2,x2,y2,c) UNTIL
    (c=wnd.r_on) OR (c=wnd.l_on);
  vg.frame(w,cx,cy,cx2,cy2);
  vg.sign(w,cx2-vg.sign_w DIV 2,cy2-vg.sign_h DIV 2,vg.sg_cursor1);
  vg.sign(w,cx-vg.sign_w DIV 2,cy-vg.sign_h DIV 2,vg.sg_cursor1);
  IF c=wnd.l_on THEN
    IF md=0 THEN
      mac.define_macro(m^.brd,rng.x1,rng.y1,rng.x2,rng.y2,new);
      open_model(new);
      wnd.refresh(w);
    ELSIF md=1 THEN
      mac.delete_box(m^.brd,rng);
      update_box_model(m,rng);
    END;
  ELSE wnd.refresh(w);
  END;
  w^.N:=w^.sy-1;
  os.release(m^.lock);
END copy_cu;

PROCEDURE picture_menu(m: model; p: picture; w: wnd.window);
  CONST lno=5;
  VAR n: INTEGER;
BEGIN
  n:=men.tmp_menu(w^.x+w^.sx-8*(vg.sign_w+2),w^.y+w^.sy-lno*vg.char_h-8,
     '  MENU\n'
     'скопировать метализацию\n'
     'удалить метализацию\n'
     'параметры окна\n'
     'параметры сетки');
  CASE n OF
    |0:
    |1: copy_cu(m,p,w,0);
    |2: copy_cu(m,p,w,1);
    |3: open_picture_mode(p);
    |4: open_picture_greed(p);
  END;
END picture_menu;

PROCEDURE set_scale(p: picture; n: INTEGER);
BEGIN
  p^.e.scale:=n;
  p^.e.w_x:=p^.e.w_x DIV n * n - n DIV 2;
  p^.e.w_y:=p^.e.w_y DIV n * n - n DIV 2;
END set_scale;

PROCEDURE drow_picture(p: picture; w: wnd.window); FORWARD;

PROCEDURE resize_picture(p: picture);
  VAR sx,sy,x,y: INTEGER; c: CHAR; t: err.trap;
BEGIN
  sx:=p^.window^.sx; sy:=p^.window^.sy;
  x:=p^.window^.x; y:=p^.window^.y;
  REPEAT wnd.read_point(wnd.rp_size,x,y,sx,sy,c) UNTIL c#0c;
  os.acquire(p^.mdl^.lock);
  IF err.enter(t) THEN
    close_picture(p);
  ELSE
    wnd.remove(p^.window);
    sx:=(sx+16) DIV 32 * 32;
    IF sy<pic_top_sy+3 THEN sy:=pic_top_sy+3 END;
    IF sx<pic_top_sx THEN sx:=pic_top_sx END;
    p^.window:=wnd.create(sx,sy);
    p^.window^.x:=x; p^.window^.y:=y;
    drow_picture(p,p^.window);
    err.exit(t);
  END;
  os.release(p^.mdl^.lock);
END resize_picture;

PROCEDURE picture_job(w: wnd.window; x,y: INTEGER; c: CHAR);
  VAR p: picture; m: model; on: BOOLEAN; sx,sy,n,i,j: INTEGER; t: err.trap;
BEGIN
  p:=w^.info; m:=p^.mdl; x:=x-w^.x; y:=y-w^.y;
  IF (x<0) OR (x>=w^.sx) OR (y<0) OR (y>=w^.sy) THEN RETURN END;
  IF err.enter(t) THEN men.message(-1,-1,1,4,'%s',t.txt); RETURN END;
  IF y>=w^.sy-pic_top_sy THEN
    on:=((c=wnd.l_on) OR (c=wnd.m_on) OR (c=wnd.r_on)) &
        (y>=w^.sy-vg.sign_h-2) & (y<=w^.sy-3);
    j:=w^.sx-(vg.sign_w+2)*5-3; i:=j-(vg.char_w+2)*4-3;
    IF on & (x>=2) & (x<=1+vg.sign_w) THEN close_picture(p)
    ELSIF on & (x>=i) & (x<=j) THEN picture_menu(m,p,w);
    ELSIF on & (x>=w^.sx-(vg.sign_w+2)*5) & (x<=w^.sx-(vg.sign_w+2)*4-3) THEN
      IF p^.lay=p^.green_lay THEN set_lay(p,w,p^.red_lay);
      ELSE set_lay(p,w,p^.green_lay);
      END;
    ELSIF on & (x>=w^.sx-(vg.sign_w+2)*4) & (x<=w^.sx-(vg.sign_w+2)*3-3) THEN
      IF p^.e.scale<48 THEN
        m^.stop:=TRUE; os.acquire(m^.lock); m^.stop:=FALSE;
        IF p^.e.scale<3 THEN
          n:=3; vg.mode(w,vg.rep); vg.color(w,15);
          vg.sign(w,w^.sx-(vg.sign_w+2)*3,w^.sy-vg.sign_h-2,vg.sg_out);
          wnd.refresh(w);
        ELSIF p^.e.scale>=24 THEN
          n:=48; vg.mode(w,vg.rep); vg.color(w,15);
          vg.sign(w,w^.sx-(vg.sign_w+2)*4,w^.sy-vg.sign_h-2,vg.sg_empty);
          wnd.refresh(w);
        ELSE n:=p^.e.scale*2
        END;
        INC(p^.e.w_x,w^.sx*(p^.e.scale-n) DIV 2);
        INC(p^.e.w_y,(w^.sy-pic_top_sy)*(p^.e.scale-n) DIV 2);
        set_scale(p,n); os.send(m^.wakeup); os.release(m^.lock);
      END;
    ELSIF on & (x>=w^.sx-(vg.sign_w+2)*3) & (x<=w^.sx-(vg.sign_w+2)*2-3) THEN
      IF p^.e.scale>1 THEN
        m^.stop:=TRUE; os.acquire(m^.lock); m^.stop:=FALSE;
        IF p^.e.scale<=3 THEN
          n:=1; vg.mode(w,vg.rep); vg.color(w,15);
          vg.sign(w,w^.sx-(vg.sign_w+2)*3,w^.sy-vg.sign_h-2,vg.sg_empty);
          wnd.refresh(w);
        ELSIF p^.e.scale>=48 THEN
          n:=24; vg.mode(w,vg.rep); vg.color(w,15);
          vg.sign(w,w^.sx-(vg.sign_w+2)*4,w^.sy-vg.sign_h-2,vg.sg_in);
          wnd.refresh(w);
        ELSE n:=p^.e.scale DIV 2
        END;
        INC(p^.e.w_x,w^.sx*(p^.e.scale-n) DIV 2);
        INC(p^.e.w_y,(w^.sy-pic_top_sy)*(p^.e.scale-n) DIV 2);
        set_scale(p,n); os.send(m^.wakeup); os.release(m^.lock);
      END;
    ELSIF on & (x>=w^.sx-(vg.sign_w+2)*2) & (x<=w^.sx-vg.sign_w-5) THEN
      sx:=w^.sx; sy:=w^.sy; x:=w^.x; y:=w^.y;
      REPEAT wnd.read_point(wnd.rp_move,x,y,sx,sy,c) UNTIL c#0c;
      wnd.close(w);
      w^.x:=(x+16) DIV 32 *32; w^.y:=y;
      IF w^.x+w^.sx>wnd.screen^.sx THEN w^.x:=wnd.screen^.sx-w^.sx END;
      wnd.open(w);
    ELSIF on & (x>=w^.sx-vg.sign_w-2) & (x<=w^.sx-3) THEN resize_picture(p);
    ELSIF c=wnd.l_on THEN wnd.ontop(w)
    ELSIF c=wnd.r_on THEN wnd.onbottom(w)
    END;
  ELSIF c=wnd.l_on THEN cu_track(m,p,w,x,y);
  ELSIF c=wnd.m_on THEN move_picture(m,p,w,x,y);
  ELSIF c=wnd.r_on THEN wnd.onbottom(w);
  END;
  err.exit(t);
END picture_job;

PROCEDURE drow_picture(p: picture; w: wnd.window);
  VAR ky,i,j: INTEGER;
BEGIN
  p^.s.scale:=0;
  vg.mode(w,vg.rep); w^.patt:={0..31};
  vg.color(w,1); vg.frame(w,0,w^.sy-pic_top_sy,w^.sx-1,w^.sy-1);
  vg.color(w,2); vg.box(w,0,w^.sy-pic_top_sy+1,w^.sx-2,w^.sy-1);
  vg.color(w,15); vg.frame(w,0,0,w^.sx-1,w^.sy-pic_top_sy-1);
  ky:=w^.sy-vg.sign_h-2;
  vg.sign(w,2,ky,vg.sg_close);
  vg.sign(w,w^.sx-(vg.sign_w+2)*5,ky,vg.sg_green);
  vg.sign(w,w^.sx-(vg.sign_w+2)*4,ky,vg.sg_in);
  vg.sign(w,w^.sx-(vg.sign_w+2)*3,ky,vg.sg_out);
  vg.sign(w,w^.sx-(vg.sign_w+2)*2,ky,vg.sg_move);
  vg.sign(w,w^.sx-vg.sign_w-2,ky,vg.sg_resize);
  j:=w^.sx-(vg.sign_w+2)*5-3;
  i:=j-(vg.char_w+2)*4-3;
  vg.color(w,1); vg.frame(w,i,ky,j,ky+vg.sign_h-1);
  vg.color(w,2); vg.mode(w,vg.bic);
  vg.vect(w,i+1,ky,j,ky); vg.vect(w,j,ky,j,ky+vg.sign_h-1);
  vg.write_string(w,i+2,ky+2,'MENU');
  w^.W:=1; w^.E:=w^.sx-2; w^.S:=1; w^.N:=w^.sy-2;
  w^.info:=p; w^.job:=picture_job;
  wnd.open(w);
  os.send(p^.mdl^.wakeup);
END drow_picture;

PROCEDURE open_picture(m: model; x,y,sx,sy: INTEGER);
  VAR p: picture; w: wnd.window; t: err.trap;
BEGIN
  NEW(p);
  os.acquire(m^.lock);
  IF err.enter(t) THEN
    close_picture(p); os.release(m^.lock); err.re_raise(t);
  END;
  p^.window:=NIL;
  p^.signal:=NIL;
  p^.green_on:=TRUE;
  p^.red_on:=TRUE;
  p^.other_on:=FALSE;
  p^.green_lay:=0;
  p^.red_lay:=1;
  p^.chips_on:=TRUE;
  p^.pins_on:=FALSE;
  p^.text_on:=FALSE;
  p^.mode_wnd:=NIL;
  p^.mode_icn:=icn.nil;
  p^.greed_on:=TRUE;
  p^.greed:=48;
  p^.greed_wnd:=NIL;
  p^.greed_icn:=icn.nil;
  p^.fixed:=FALSE;
  p^.tl_size:=40;
  p^.tl_vias:=80;
  p^.tl_pipe:=30;
  p^.next:=m^.pictures;
  p^.mdl:=m;
  m^.pictures:=p;
  IF sy<pic_top_sy+3 THEN sy:=pic_top_sy+3 END;
  IF sx<pic_top_sx THEN sx:=pic_top_sx END;
  w:=wnd.create(sx,sy);
  err.exit(t);
  p^.window:=w;
  w^.x:=x; w^.y:=y;
  p^.e.w_x:=(m^.brd^.x-w^.sx*12) DIV 2;
  p^.e.w_y:=(m^.brd^.y-w^.sy*12) DIV 2;
  set_scale(p,12);
  p^.lay:=0;
  drow_picture(p,w);
  os.release(m^.lock);
END open_picture;

PROCEDURE close_model(VAR m: model);
  VAR p: picture; i: INTEGER;
BEGIN
  os.acquire(m^.lock);
  WHILE m^.pictures#NIL DO
    p:=m^.pictures; m^.pictures:=p^.next; close_picture(p)
  END;
  wnd.remove(m^.window);
  mdl.del_board(m^.brd);
  IF m^.process#NIL THEN
    m^.process^.halt:=NIL;
    os.stop(m^.process,TRUE);
    ASSERT(os.rem_process(m^.process)=0);
  END;
  os.release(m^.lock);
  DISPOSE(m);
END close_model;

PROCEDURE model_edit(m: model);
  VAR nm: mdl.string; t: err.trap; i: INTEGER;
BEGIN
  IF err.enter(t) THEN
    men.message(-1,-1,1,4,'%s',t.txt); RETURN
  END;
  CASE
    men.tmp_menu(m^.window^.x,m^.window^.y-vg.char_h*3,
    'EDITOR\n'
    'компоненты\n'
    'метализация\n'
    'типы компонентов\n'
    'записать в файл\n'
    'переименовать')
  OF
    |0:
    |1: men.message(-1,-1,1,4,'Не реализовано');
    |2: open_picture(m,0,0,wnd.screen^.sx,wnd.screen^.sy DIV 3 *2);
    |3: men.message(-1,-1,1,4,'Не реализовано');
    |4: mdl.write_model(m^.brd^.name,m^.brd);
        men.message(-1,-1,0,8,'Записано под именем\n"%s".',m^.brd^.name);
        m^.changed:=FALSE;
    |5: nm:='';
        men.readln(-1,-1,'Новое имя модели:',nm);
        IF nm#'' THEN
          m^.brd^.name:=nm; m^.changed:=TRUE;
          vg.color(m^.window,1); vg.mode(m^.window,vg.bic);
          vg.box(m^.window,2,vg.sign_h+2,m^.window^.sx-2,m^.window^.sy-2);
          i:=0; WHILE nm[i]#0c DO INC(i) END;
          i:=(HIGH(nm)-i) DIV 2;
          vg.mode(m^.window,vg.rep);
          vg.write_string(m^.window,2+i*(vg.char_w+2),vg.sign_h+3,nm);
          wnd.refresh(m^.window);
        END;
  END;
  err.exit(t);
END model_edit;

PROCEDURE model_job(w: wnd.window; x,y: INTEGER; c: CHAR);
  VAR m: model; on: BOOLEAN;
BEGIN
  x:=x-w^.x; y:=y-w^.y; m:=w^.info;
  on:=( (c=wnd.l_on) OR (c=wnd.m_on) OR (c=wnd.r_on) ) & (y>=0) & (y<stp);
  IF (x>=11*stp) & (x<12*stp) & on THEN
    x:=w^.x; y:=w^.y;
    REPEAT wnd.read_point(wnd.rp_move,x,y,w^.sx,w^.sy,c);
    UNTIL (c=wnd.l_off) OR (c=wnd.m_off) OR (c=wnd.r_off);
    wnd.close(w); w^.x:=x; w^.y:=y; wnd.open(w);
  ELSIF (x>=6*stp) & (x<11*stp) & on THEN model_edit(m);
  ELSIF (x>=0) & (x<stp) & on THEN
    IF NOT m^.changed OR
      men.qwest(w^.x,w^.y,
      'Вы уверены что не хотите\nзаписать изменения в файл?')
    THEN
      close_model(m);
    END;
  ELSIF c=wnd.l_on THEN wnd.ontop(w);
  ELSIF c=wnd.r_on THEN wnd.onbottom(w);
  END;
END model_job;

PROCEDURE open_model(b: mdl.board);
  VAR t: err.trap; m: model; sx,sy,i,j: INTEGER; w: wnd.window;
BEGIN
  m:=NIL;
  IF err.enter(t) THEN close_model(m); mdl.del_board(b); err.re_raise(t) END;
  NEW(m);
  m^.brd:=NIL; m^.pictures:=NIL; m^.window:=NIL; m^.changed:=FALSE;
  os.ini_signal(m^.wakeup,{},0); os.ini_mutex(m^.lock);
  m^.stop:=FALSE; m^.process:=NIL;
  ----------------------------------------------
  sx:=stp*12; sy:=stp*2;
  w:=wnd.create(sx,sy);
  m^.window:=w;
  w^.x:=op_x; w^.y:=op_y;
  INC(op_x,10); DEC(op_y,10);
  IF op_x>80 THEN op_x:=0 END;
  IF op_y<50 THEN op_y:=300 END;
  men.text_frame(w,0,stp,sx,stp,3,b^.name);
  men.text_frame(w,stp,0,5*stp,stp,3,'<PCB>');
  men.text_frame(w,6*stp,0,5*stp,stp,0,'EDITOR');
  vg.color(w,15); vg.mode(w,vg.rep);
  vg.sign(w,0,0,vg.sg_close);
  vg.sign(w,11*stp,0,vg.sg_move);
  w^.info:=m; w^.job:=model_job;
  IF os.make_process(m^.process,painter,1000)#0 THEN
    err.raise('Переполнена динамическая память,\n  очень жаль...');
  END;
  DEC(m^.process^.pp^.S);
  m^.process^.pp^.S^:=m;
  INC(m^.process^.pp^.S);
  m^.process^.pp^.S^:=1;
  INC(m^.process^.pp^.S);
  os.start(m^.process);
  ----------------------------------------------
  err.exit(t);
  m^.brd:=b;
  wnd.open(w);
END open_model;

BEGIN
  op_x:=0; op_y:=100;
  ASSERT(stp=vg.sign_w);
  ASSERT(stp=vg.sign_h);
END pedEditor.
