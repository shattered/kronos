MODULE t; (*  05-Sep-91. (c) KRONOS *)

IMPORT  wnd: pmWnd;
IMPORT  crs: pmCrs;
IMPORT  bpm: bcPM;
IMPORT  bmt: bcMath;
IMPORT  key: Keyboard;
IMPORT  mou: CPD;
IMPORT  tty: Terminal;

VAR mx,my,dx,dy: INTEGER;
           wind: wnd.WINDOW;
            blc: wnd.BLOCK;
             ch: CHAR;
              t: wnd.TOOL;

PROCEDURE _long(x,y: INTEGER);
 VAR tx,ty: INTEGER;
BEGIN
  tx:= mx+x; ty:=my+y;
  IF tx*tx+ty*ty <= 2500 THEN mx:=tx; my:=ty END
END _long;

PROCEDURE curs;  BEGIN wnd.line(wind,t,75,75,mx+75,my+75) END curs;

BEGIN
  WITH blc DO x:=0; y:=0; w:=250; h:=150 END;
  wnd.new(wind);
  wnd.resize(wind,blc.w,blc.h);
  wnd.move(wind,100,100);
  bpm.block(wind,blc,TRUE,FALSE);
  t:= wind^.inner;
  t.back:= bpm.normal;

  t.color:= bpm.bright;
  wnd.circle(wind,t,75,75,50);
  wnd.circle(wind,t,75,75,25);
  wnd.line(wind,t,75,20,75,130);
  wnd.line(wind,t,21,75,131,75);

  t.color:= bpm.black;
  wnd.circle(wind,t,74,76,50);
  wnd.circle(wind,t,74,76,25);
  wnd.line(wind,t,74,20,74,130);
  wnd.line(wind,t,20,76,130,76);
  wnd.print(wind,t,110,135,bpm.font,'3D View');
  wnd.writech(wind,t,132,70,bpm.font,'X');
  wnd.writech(wind,t,71,132,bpm.font,'Y');

  t.mode:= wnd.xor;
  t.color:= bpm.bright;
  wnd.open(wind);

  mx:=0; my:=0;
  crs.move(mx+75+wind^.x,my+75+wind^.y);
  curs;
  crs.style(crs.cross);
  crs.setcolor(crs.cross,bpm.shadow);
  crs.toggle(TRUE);
  LOOP
    IF (key.ready()#0) OR (mou.ready()#0) THEN
      bpm.x_read(ch,dx,dy);
      curs;
      CASE ch OF
         033c     :
        |015c     :  tty.print('%d %d \n',mx,my)
        |key.left : _long(-1,0)
        |key.right: _long(1 ,0)
        |key.dw   : _long(0,-1)
        |key.up   : _long(0,1)
      ELSE
        _long(dx,dy)
      END;
      crs.move(mx+75+wind^.x,my+75+wind^.y);
      curs
    END
  END
END t.
