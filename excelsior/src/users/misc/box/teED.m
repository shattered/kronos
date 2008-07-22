IMPLEMENTATION MODULE teED; (*  02-Jul-91. (c) KRONOS *)

IMPORT  wn: pmWnd;      IMPORT  bio: BIO;
IMPORT  wm: pmWM;
IMPORT  pm: pmPUP;
IMPORT  cr: pmCrs;
IMPORT  md: teModel;
IMPORT  sm: teMS;
IMPORT  mm: Heap;
IMPORT  ms: CPD;

TYPE EDITOR = POINTER TO Editor;
     Editor = RECORD
                type: INTEGER;
                   o: wn.WINDOW;
                   v: md.VIEW;
                   t: INTEGER;
               fx,fy: INTEGER;
                   m: BITSET
              END;

CONST _zoomin  = 0;
      _zoomout = 1;
      _left    = 2;
      _right   = 3;
      _up      = 4;
      _dw      = 5;
      _file    = 6;
      _edit    = 7;
      _tool    = 8;
      _pan     = 9;
      _name    = 20;

CONST _full = {_zoomin.._pan};

CONST minw = 180;
      minh = 150;

      _rol = 0;
      _ins = 1;
      _rub = 2;

VAR B : ARRAY [0.._pan ] OF RECORD
                              c: INTEGER;
                              b: wn.BLOCK
                            END;

PROCEDURE min(x,y: INTEGER): INTEGER;
BEGIN
  IF x<y THEN RETURN x ELSE RETURN y END
END min;

PROCEDURE max(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN x ELSE RETURN y END
END max;

PROCEDURE _inrect(x,y,w,h: INTEGER): BOOLEAN;
BEGIN
(*
CODE
  cod.li1 cod.sub cod.swap
  cod.li1 cod.sub cod.swap
  cod.bmg cod.bmg_inrect
*)
  RETURN NOT ((x<0) OR (y<0) OR (x>=w) OR (y>=h))
END _inrect;

PROCEDURE zoomin(e: EDITOR);
BEGIN
  md.zoom(e^.v,-1)
END zoomin;

PROCEDURE zoomout(e: EDITOR);
BEGIN
  md.zoom(e^.v,+1)
END zoomout;

PROCEDURE scroll(e: EDITOR; sx,sy: INTEGER);
BEGIN
  md.scroll(e^.v,32*sx,32*sy)
END scroll;

PROCEDURE definebuttons;

  PROCEDURE button(no,c,x,y,w,h: INTEGER);
  BEGIN
    B[no].c:=c;
    B[no].b.x:=x; B[no].b.w:=w;
    B[no].b.y:=y; B[no].b.h:=h
  END button;

BEGIN
  button (_zoomin ,wm.luc, 01,-29,13,13);
  button (_zoomout,wm.luc, 01,-44,13,13);
  button (_tool   ,wm.luc, 01,-59,13,13);
  button (_pan    ,wm.luc, 01,-74,13,13);
  button (_up     ,wm.ldc, 01, 16,13,13);
  button (_dw     ,wm.ldc, 01, 01,13,13);
  button (_right  ,wm.ruc,-14,-14,13,13);
  button (_left   ,wm.ruc,-29,-14,13,13);
  button (_file   ,wm.luc, 16,-14,27,13);
  button (_edit   ,wm.luc,16+27+3,-14,27,13)
END definebuttons;

PROCEDURE putname(e: EDITOR);
  VAR   fn,n: ARRAY [0..63] OF CHAR;
BEGIN
  md.getname(e^.v,fn);
  bio.splitpathname(fn,n);
  wm.button (e^.o,_name,wm.ruc,-16*2-64,-13,64,11); IF NOT wm.done THEN HALT(1) END;
  wm.print  (e^.o,_name,pm.font,"%s",n);
  wm.toggle (e^.o,_name,TRUE)
END putname;

PROCEDURE _new(VAR e: EDITOR; x,y,w,h: INTEGER);
  VAR i: INTEGER;
      o: wn.WINDOW;
BEGIN
  w:=max(w,minw);
  h:=max(h,minh);

  wm.new    (o);     IF NOT wm.done THEN HALT(1) END;
  wn.move   (o,x,y); IF NOT wm.done THEN HALT(1) END;
  wn.resize (o,w,h); IF NOT wm.done THEN HALT(1) END;
  wn.inner  (o,15,4,max(0,w-15-4),max(0,h-15-4));

  wn.painter(o,sm.paint);

  FOR i:=0 TO HIGH(B) DO
    wm.button(o,i,B[i].c,B[i].b.x,B[i].b.y,B[i].b.w,B[i].b.h);
    IF NOT wm.done THEN HALT(1) END
  END;

  wm.print  (o,_zoomin ,wm.ssfont,"%c",wm.sszoomin );
  wm.print  (o,_zoomout,wm.ssfont,"%c",wm.sszoomout);
  wm.print  (o,_tool   ,wm.ssfont,"%c",wm.sstool   );
  wm.print  (o,_pan    ,wm.ssfont,"%c",wm.sspan    );
  wm.print  (o,_up     ,wm.ssfont,"%c",wm.ssup     );
  wm.print  (o,_dw     ,wm.ssfont,"%c",wm.ssdw     );
  wm.print  (o,_right  ,wm.ssfont,"%c",wm.ssright  );
  wm.print  (o,_left   ,wm.ssfont,"%c",wm.ssleft   );
  wm.print  (o,_file   ,pm.font  ,"FILE");
  wm.print  (o,_edit   ,pm.font  ,"EDIT");

  mm.allocate(e,SIZE(e^)); IF NOT mm.done THEN HALT(1) END;
  e^.type:=type;
  e^.o   :=o;
  e^.v   :=NIL;
  e^.t   :=1;
  e^.m   :={_rol,_ins};
  wn.object(o,e);
  md.new(e^.v,NIL); IF NOT md.done THEN HALT(md.error) END;
  md.setwindow(e^.v,e^.o);
  md.setname(e^.v,'noname');
  md.setview(e^.v,0,0,1);
  putname(e);

  wm.disable(o)
END _new;

PROCEDURE new(x,y,w,h: INTEGER);
  VAR e : EDITOR;
      on: BOOLEAN;
BEGIN
  _new(e,x,y,w,h);
  on:=cr.on;
  cr.toggle(FALSE);
  wn.open(e^.o);
  cr.toggle(on)
END new;

PROCEDURE file(e: EDITOR);

  PROCEDURE fname(VAR s: ARRAY OF CHAR; x,y,w,h: INTEGER): BOOLEAN;
    VAR dbx: pm.DIREX;
  BEGIN
    pm.dnew(dbx,x,y,w,h,{},'.','*',pm.standard);
    pm.dselect(dbx);
    IF pm.dselected(dbx) THEN
      pm.dfullname(dbx,s);
      pm.ddispose(dbx);
      RETURN TRUE
    ELSE
      pm.ddispose(dbx);
      RETURN FALSE
    END
  END fname;

  PROCEDURE setname(e: EDITOR; VAL fn: ARRAY OF CHAR);
  BEGIN
    md.setname(e^.v,fn);
    putname(e)
  END setname;

  PROCEDURE name(e: EDITOR);
    VAR n: ARRAY [0..63] OF CHAR;
  BEGIN
    md.getname(e^.v,n);
    pm.confirm(e^.o^.x+15,e^.o^.y+e^.o^.h-15-25,100,n,"name: ");
    IF n[0]#0c THEN setname(e,n) END
  END name;

  PROCEDURE read(e: EDITOR; VAR m: pm.MENU);
    VAR s: ARRAY [0..63] OF CHAR;
  BEGIN
    IF NOT fname(s,e^.o^.x+15+64,e^.o^.y+e^.o^.h-15-100,70,100) THEN RETURN END;
    pm.mclose(m);
    md.dispose(e^.v);
    md.read(e^.v,s); IF NOT md.done THEN RETURN END;
    setname(e,s);
    md.setwindow(e^.v,e^.o)
  END read;

  PROCEDURE writeas(e: EDITOR);
    VAR s: ARRAY [0..63] OF CHAR;
  BEGIN
    IF NOT fname(s,e^.o^.x+15+64,e^.o^.y+e^.o^.h-15-100,70,100) THEN RETURN END;
    md.write(e^.v,s);
    IF NOT md.done THEN HALT(md.error) END;
  END writeas;

  PROCEDURE write(e: EDITOR);
    VAR n: ARRAY [0..31] OF CHAR;
  BEGIN
    md.getname(e^.v,n); IF NOT md.done THEN HALT(md.error) END;
    md.write  (e^.v,n); IF NOT md.done THEN HALT(md.error) END
  END write;

  PROCEDURE enew(e: EDITOR; VAR m: pm.MENU);
  BEGIN
    pm.mclose(m);
    md.dispose(e^.v);
    md.new(e^.v,NIL); IF NOT md.done THEN HALT(md.error) END;
    md.setwindow(e^.v,e^.o);
    setname(e,"noname")
  END enew;

  PROCEDURE connect(e: EDITOR; m: pm.MENU);
    VAR o: wn.WINDOW; ed: EDITOR; ch: BITSET;
  BEGIN
    cr.style (cr.cross);
    cr.toggle(TRUE);
    LOOP
      ch:=ms.state^.keys;
      cr.monitor;
      ch:=(ch/ms.state^.keys)*ms.state^.keys;
      IF    2 IN ch THEN RETURN
      ELSIF 0 IN ch THEN
        o:=wn.locate(cr.x,cr.y);
        IF (o#NIL)&(o#e^.o) THEN
          ed:=o^.obj;
          IF (ed#NIL)&(ed^.type=type) THEN
            cr.toggle(FALSE);
            cr.style(cr.arrow);
            pm.mclose(m);
            md.dispose(e^.v);
            md.new(e^.v,ed^.v); IF NOT md.done THEN HALT(1) END;
            md.setwindow(e^.v,e^.o);
            putname(e);
            RETURN
          END
        END
      END
    END
  END connect;

  PROCEDURE clear(e: EDITOR; m: pm.MENU);
    VAR n: ARRAY [0..63] OF CHAR; x,y,s: INTEGER;
  BEGIN
    x:=e^.v^.x; y:=e^.v^.y; s:=e^.v^.s;
    md.getname(e^.v,n);
    pm.mclose (m);
    md.dispose(e^.v  );
    md.new    (e^.v,NIL); IF NOT md.done THEN HALT(md.error) END;
    md.setwindow(e^.v,e^.o);
    md.zoom  (e^.v,s-e^.v^.s);
    md.scroll(e^.v,x-e^.v^.x,y-e^.v^.y);
    setname(e,n)
  END clear;

  PROCEDURE makefilemenu(o: wn.WINDOW; VAR m: pm.MENU);
  BEGIN
    pm.mnew(m,o^.x+15,o^.y+o^.h-15-98,64,100,{},'FILE');

    pm.mprint(m,0,"new     ");  pm.mhotkey(m,0,"n",TRUE);
    pm.mprint(m,1,"name    ");
    pm.mprint(m,2,"connect ");  pm.mhotkey(m,2,"c",TRUE);
    pm.mprint(m,3,"read    ");  pm.mhotkey(m,3,"r",TRUE);
    pm.mprint(m,4,"write   ");  pm.mhotkey(m,4,"w",TRUE);
    pm.mprint(m,5,"write as");  pm.mhotkey(m,5,"s",TRUE);
    pm.mprint(m,6,"clear   ");  pm.mhotkey(m,6,"d",TRUE);
    pm.mopen(m)
  END makefilemenu;

  VAR m: pm.MENU;

BEGIN
  makefilemenu(e^.o,m);
  LOOP
    pm.mselect(m);
    IF NOT pm.mselected(m) THEN EXIT END;
    CASE pm.malt(m) OF
      |0: enew   (e,m)
      |1: name   (e  )
      |2: connect(e,m)
      |3: read   (e,m)
      |4: write  (e  )
      |5: writeas(e  )
      |6: clear  (e,m)
    ELSE
    END
  END;
  pm.mdispose(m)
END file;

PROCEDURE locator(e: EDITOR);
  VAR          t: wn.TOOL;
     x0,y0,x1,y1: INTEGER;
BEGIN
  wn.mode(e^.o,wn.scr);
  t:=e^.o^.full;
  t.mode:=wn.xor;
  t.color:={0};
  t.mask :={0};
  x0:=cr.x-e^.o^.x;
  y0:=cr.y-e^.o^.y;
  wn.line(e^.o,t, 0,y0,e^.o^.w-1,       y0);
  wn.line(e^.o,t,x0, 0,       x0,e^.o^.h-1);
  IF e^.m*{_rub}#{} THEN
    x1:=e^.fx*e^.v^.s-e^.v^.x+e^.o^.inner.zX;
    y1:=e^.fy*e^.v^.s-e^.v^.y+e^.o^.inner.zY;
    wn.line(e^.o,t,x1,y1,x1,y0);
    wn.line(e^.o,t,x1,y1,x0,y1)
  END
END locator;

PROCEDURE readmouse(e: EDITOR; VAR keys: BITSET);

  PROCEDURE auto_scroll(VAR xsc,ysc: INTEGER);
    CONST SHIFT = 4;
  BEGIN
    WITH e^ DO
      IF xsc<o^.x-20 THEN
        md.scroll(v,-(ABS(xsc-o^.x)/(o^.w DIV SHIFT)+1)*32,0); xsc:=o^.x
      ELSIF xsc>(o^.x+o^.w+20) THEN
        md.scroll(v,+((xsc-o^.x-o^.w)/(o^.w DIV SHIFT)+1)*32,0); xsc:=o^.x+o^.w-1
      END;
      IF    ysc<o^.y-20 THEN
        md.scroll(v,0,-(ABS(ysc-o^.y)/(o^.h DIV SHIFT)+1)*32); ysc:=o^.y
      ELSIF ysc>(o^.y+o^.h+20) THEN
        md.scroll(v,0,+((ysc-o^.y-o^.h)/(o^.h DIV SHIFT)+1)*32); ysc:=o^.y+o^.h-1
      END
    END
  END auto_scroll;

  VAR mstate,change: BITSET; i,j: INTEGER;
                 xscroll,yscroll: INTEGER;
                           dx,dy: INTEGER;
BEGIN
  WITH e^ DO
    xscroll:=cr.x; yscroll:=cr.y; dx:=0; dy:=0;
    LOOP
      cr.move(cr.x+dx,cr.y+dy);
      locator(e);
      dx:=0; dy:=0;
      REPEAT
        change:=ms.state^.keys;
        ms.read(i,j,mstate);
        dx:=dx+i; dy:=dy+j;
        xscroll:=xscroll+i;
        yscroll:=yscroll+j;
        IF    cr.x+dx< o^.x      THEN dx:=o^.x       -cr.x
        ELSIF cr.x+dx>=o^.x+o^.w THEN dx:=o^.x+o^.w-1-cr.x
        ELSE           xscroll:=cr.x+dx
        END;
        IF    cr.y+dy< o^.y      THEN dy:=o^.y       -cr.y
        ELSIF cr.y+dy>=o^.y+o^.h THEN dy:=o^.y+o^.h-1-cr.y
        ELSE           yscroll:=cr.y+dy
        END;
        change:=change/mstate
      UNTIL (ms.ready()<=0)OR(change*mstate*{0,2}#{});
      locator(e);
      keys:=change*mstate;
      IF keys#{} THEN EXIT END;
      IF m*{_rol}#{} THEN auto_scroll(xscroll,yscroll) END
    END
  END
END readmouse;

PROCEDURE doit(e: EDITOR; cmd: INTEGER); FORWARD;

PROCEDURE pointxy(o: wn.WINDOW; VAR x,y: INTEGER);
BEGIN
  x:=cr.x-o^.x-o^.inner.zX;
  y:=cr.y-o^.y-o^.inner.zY
END pointxy;

PROCEDURE read(e: EDITOR; mask: BITSET; VAR key: BITSET);

  PROCEDURE buttonxy(o: wn.WINDOW; VAR x,y: INTEGER; VAL b: wn.BLOCK; c: INTEGER);
  BEGIN
    x:=b.x+o^.x;
    y:=b.y+o^.y;
    CASE c OF
    |wm.ldc:
    |wm.rdc:  x:=x+o^.w;
    |wm.luc:  y:=y+o^.h;
    |wm.ruc:  x:=x+o^.w;  y:=y+o^.h
    END
  END buttonxy;

  PROCEDURE board(e: EDITOR);
    VAR i,x,y: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(B) DO
      IF i IN mask THEN
        WITH B[i] DO
          buttonxy(e^.o,x,y,b,c);
          IF _inrect(cr.x-x,cr.y-y,b.w,b.h) THEN
            wm.toggle(e^.o,i,TRUE);
            doit(e,i);
            wm.toggle(e^.o,i,FALSE);
            RETURN
          END
        END
      END
    END
  END board;

  PROCEDURE in(e: EDITOR): BOOLEAN;
    VAR cx,cy: INTEGER;
  BEGIN
    cx:=cr.x-e^.o^.x-e^.o^.inner.zX;
    cy:=cr.y-e^.o^.y-e^.o^.inner.zY;
    RETURN _inrect(cx,cy,e^.o^.inner.clip.w,e^.o^.inner.clip.h)
  END in;

  VAR k: BITSET;
      i: INTEGER;
BEGIN
  LOOP
    readmouse(e,k); key:=k;
    IF in(e)    THEN RETURN END;
    IF 2 IN key THEN RETURN END;
    IF 0 IN key THEN board(e) END
  END
END read;

PROCEDURE scalepoint(e: EDITOR; VAR X,Y: INTEGER; x,y: INTEGER);
BEGIN
  X:=(e^.v^.x+(x+e^.v^.s DIV 2)) DIV e^.v^.s;
  Y:=(e^.v^.y+(y+e^.v^.s DIV 2)) DIV e^.v^.s
END scalepoint;

PROCEDURE makebox(e: EDITOR; VAR b: wn.BLOCK; s: BOOLEAN; mask: BITSET): BOOLEAN;

  PROCEDURE start;
    VAR x,y: INTEGER;
  BEGIN
    IF NOT s THEN e^.m:=e^.m-{_rub}; RETURN END;
    e^.m:=e^.m+{_rub};
    pointxy(e^.o,x,y);
    scalepoint(e,e^.fx,e^.fy,x,y)
  END start;

  PROCEDURE set(): BOOLEAN;
    VAR x,y: INTEGER;
  BEGIN
    pointxy(e^.o,x,y);
    IF e^.m*{_rub}#{} THEN
      scalepoint(e,x,y,x,y);
      b.x:=min(x,e^.fx); b.w:=ABS(x-e^.fx); IF b.w=0 THEN b.w:=1 END;
      b.y:=min(y,e^.fy); b.h:=ABS(y-e^.fy); IF b.h=0 THEN b.h:=1 END;
      e^.m:=e^.m/{_rub};
      RETURN TRUE
    ELSE
      scalepoint(e,e^.fx,e^.fy,x,y);
      e^.m:=e^.m/{_rub};
      RETURN FALSE
    END
  END set;

  VAR key: BITSET;

BEGIN
  start;
  LOOP
    read(e,mask,key);
    IF 2 IN key THEN
      IF e^.m*{_rub}={} THEN RETURN FALSE END;
      e^.m:=e^.m-{_rub}
    END;
    IF 0 IN key THEN
      IF set() THEN RETURN TRUE END
    END
  END
END makebox;

PROCEDURE findwindow(VAR o: wn.WINDOW): BOOLEAN;
  VAR ch: BITSET;
BEGIN
  cr.toggle(TRUE);
  o:=NIL;
  LOOP
    ch:=ms.state^.keys;
    cr.monitor;
    ch:=(ch/ms.state^.keys)*ms.state^.keys;
    IF    2 IN ch THEN EXIT
    ELSIF 0 IN ch THEN
      o:=wn.locate(cr.x,cr.y);
      IF o#NIL THEN EXIT END
    END
  END;
  cr.toggle(FALSE);
  RETURN o#NIL
END findwindow;

PROCEDURE edit(e: EDITOR);
  VAR key: BITSET; b: wn.BLOCK;
BEGIN
  LOOP
    IF NOT makebox(e,b,FALSE,_full-{_edit}) THEN RETURN END;
    md.insbox(e^.v,e^.t,b); IF NOT md.done THEN HALT(md.error) END
  END
END edit;

PROCEDURE tool(e: EDITOR);
  PROCEDURE maketoolmenu(o: wn.WINDOW; VAR m: pm.MENU);
    PROCEDURE sa(n: INTEGER; s: ARRAY OF CHAR; h: CHAR);
    BEGIN
      IF n+1=e^.t THEN s[6]:='*' END;
      pm.mprint(m,n,s);  pm.mhotkey(m,n,h,TRUE)
    END sa;
  BEGIN
    pm.mnew(m,o^.x+1,o^.y+o^.h-59-50,64,50,{},'TOOL');
    IF NOT pm.done THEN HALT(pm.error) END;

    sa(0,"poly  ",'p');
    sa(1,"diff  ",'d');

    pm.mopen(m)
  END maketoolmenu;

  VAR m: pm.MENU;

BEGIN
  maketoolmenu(e^.o,m);
  pm.mselect(m);
  IF pm.mselected(m) THEN
    e^.t:=pm.malt(m)+1;
  END;
  pm.mdispose(m)
END tool;

PROCEDURE pan(e: EDITOR);

  PROCEDURE find(e: EDITOR; VAR ed: EDITOR): BOOLEAN;
    VAR o: wn.WINDOW; ch: BITSET;
  BEGIN
    cr.style (cr.cross);
    LOOP
      IF NOT findwindow(o) THEN ed:=NIL; EXIT END;
      ed:=o^.obj;
      IF (ed#NIL)&(ed^.type=type)&md.mequal(e^.v,ed^.v) THEN EXIT END
    END;
    cr.style(cr.arrow);
    RETURN ed#NIL
  END find;

  VAR key: BITSET; ed: EDITOR; b: wn.BLOCK; x,y: INTEGER;

BEGIN
  x:=cr.x; y:=cr.y;
  LOOP
    IF NOT find(e,ed) THEN cr.move(x,y); RETURN END;
    LOOP
      IF NOT makebox(ed,b,TRUE,_full-{_edit,_pan,_file}) THEN EXIT END;
      md.pan(e^.v,b);
      cr.move(x,y);
      RETURN
    END
  END
END pan;

PROCEDURE typecheck(VAR e: EDITOR; o: wn.WINDOW);
BEGIN
  ASSERT(o#NIL);
  e:=o^.obj;
  ASSERT(e^.type=type)
END typecheck;

PROCEDURE move;
BEGIN
  wn.move(wm.active,wm.moveX,wm.moveY)
END move;

PROCEDURE close;
  VAR e: EDITOR;
BEGIN
  typecheck(e,wm.active);
  IF sm.query(100,100,'Are you sure?') THEN
    md.dispose(e^.v); wn.dispose(e^.o); mm.deallocate(e,SIZE(e^))
  END
END close;

PROCEDURE resize;
  VAR ow,w: INTEGER;
      oh,h: INTEGER;
         e: EDITOR;
BEGIN
  typecheck(e,wm.active);
  ow:=e^.o^.inner.clip.w; w:=max(wm.resizeW,minw);
  oh:=e^.o^.inner.clip.h; h:=max(wm.resizeH,minh);
  wn.resize(e^.o,w,h);
  w :=e^.o^.inner.clip.w;
  h :=e^.o^.inner.clip.h;
  IF h>oh THEN md.refreshbox(e^.v,0 ,oh,w, h) END;
  IF w>ow THEN md.refreshbox(e^.v,ow,0 ,w,oh) END
END resize;

PROCEDURE doit(e: EDITOR; cmd: INTEGER);
BEGIN
  CASE cmd OF
  |_zoomin : zoomin (e)
  |_zoomout: zoomout(e)
  |_left   : scroll (e,-1, 0)
  |_right  : scroll (e, 1, 0)
  |_dw     : scroll (e, 0,-1)
  |_up     : scroll (e, 0, 1)
  |_file   : file(e)
  |_edit   : edit(e)
  |_tool   : tool(e)
  |_pan    : pan (e)
  ELSE
  END
END doit;

PROCEDURE switch;
  VAR e: EDITOR;
BEGIN
  typecheck(e,wm.active);
  doit(e,wm.abutton);
  wm.toggle(wm.active,wm.abutton,FALSE)
END switch;

PROCEDURE info(e: EDITOR; VAR i: INFO);
  VAR o : wn.WINDOW;
      ed: EDITOR;
BEGIN
  ASSERT(e^.type=type);
  i.i[0]:=e^.t;
  i.i[1]:=e^.v^.x;
  i.i[2]:=e^.v^.y;
  i.i[3]:=e^.v^.s;
  i.i[4]:=0;
  md.getname(e^.v,i.s);
  o:=wn.top;
  WHILE o#e^.o DO
    ed:=o^.obj;
    IF (ed#NIL)&(ed^.type=type)&md.mequal(ed^.v,e^.v) THEN i.i[4]:=1 END;
    o:=wn.dw(o)
  END;
END info;

PROCEDURE restore(x,y,w,h: INTEGER; VAL i: INFO);
  VAR e,ed: EDITOR;
      o   : wn.WINDOW;
      n   : ARRAY [0..63] OF CHAR;
      v   : md.VIEW;
BEGIN
  _new(e,x,y,w,h);
  wn.onbottom(e^.o);
  e^.t:=i.i[0];
  v:=NIL;
  IF i.i[4]=1 THEN
    o:=wn.top;
    LOOP
      IF o=e^.o THEN EXIT END;
      ed:=o^.obj;
      IF (ed#NIL)&(ed^.type=type) THEN
        md.getname(ed^.v,n);
        IF n=i.s THEN v:=ed^.v; EXIT END
      END;
      o:=wn.dw(o)
    END
  END;
  IF v#NIL THEN
    md.new(e^.v,v); IF NOT md.done THEN HALT(md.error) END
  ELSE
    md.read(v,i.s); IF NOT md.done THEN wn.open(e^.o); RETURN END;
    md.dispose(e^.v);
    e^.v:=v;
    md.setname(e^.v,i.s)
  END;
  md.setwindow(e^.v,e^.o);
  md.setview(e^.v,i.i[1],i.i[2],i.i[3]);
  putname(e);

  wn.open(e^.o)
END restore;

PROCEDURE initmodel;
  VAR i: INTEGER;
BEGIN
  md.setlayno(2);
  FOR i:=0 TO HIGH(md.class.tlay) DO md.settlay(0,i) END;
  md.settlay(1,4);
  md.shape[0]:={};
  md.shape[1]:={8,0};
  md.shape[2]:={9,1};
  md.shape[3]:={0..1};
  md.shape[4]:={0};
  md.shape[5]:={1};
  md.shape[6]:={8,1..2};
  md.shape[7]:={9,0};
  md.setxcross(0,0,0); md.setxcross(0,1,1); md.setxcross(0,2,2); md.setxcross(0,3,3); md.setxcross(0,4,4);
                       md.setxcross(1,1,1); md.setxcross(1,2,3); md.setxcross(1,3,3); md.setxcross(1,4,4);
                                            md.setxcross(2,2,2); md.setxcross(2,3,3); md.setxcross(2,4,4);
                                                                 md.setxcross(3,3,3); md.setxcross(3,4,4);
                                                                                      md.setxcross(4,4,4);
END initmodel;

BEGIN
  initmodel;
  definebuttons
END teED.
