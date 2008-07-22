IMPLEMENTATION MODULE bcBase; (*$X+ brd 10-Jan-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  err: defErrors;
IMPORT  key: Keyboard;
IMPORT  mou: CPD;
IMPORT  str: Strings;
IMPORT  fnt: Fonts;
IMPORT  mth: realMath;
IMPORT  def: bcDef;
IMPORT  obj: bcObj;
IMPORT  tex: bcText;
IMPORT  bmt: bcMath;
IMPORT  bpm: bcPM;
IMPORT  wnd: pmWnd;
IMPORT  pwm: pmWM;
IMPORT  set: bcSet;
IMPORT  scr: Screen;
IMPORT  mem: Heap;

IMPORT  tty: Terminal;

CONST first =  015c;
      second=  220c;
      therd =  033c;

VAR     WCOORD: wnd.WINDOW;
        ZM,MRK: VIEW;
         pfont: tex.PFONT;

---------------------------- Memory ----------------------------

PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;
PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;
PROCEDURE undef;     BEGIN done:=FALSE; error:=err.undef    END undef;

PROCEDURE allocate(VAR a: SYSTEM.ADDRESS; size: INTEGER);
BEGIN
  mem.allocate(a,size); done:=mem.done;
  IF NOT done THEN mem_error END
END allocate;

PROCEDURE reallocate(VAR    a: SYSTEM.ADDRESS;
                     VAR high: INTEGER; len,bytesperelem: INTEGER);
BEGIN
  mem.reallocate(a,high,len,bytesperelem); done:=mem.done;
  IF NOT done THEN mem_error END
END reallocate;

WITH STORAGE (NEW    : allocate;
              DISPOSE: mem.deallocate;
              RESIZE : reallocate);


PROCEDURE no_mem;
BEGIN
  done:= TRUE; error:= err.ok;
  IF  ENG THEN bpm.message(SW DIV 2,SH DIV 2,'No memory')
  ELSE         bpm.message(SW DIV 2,SH DIV 2,'нет памяти') END
END no_mem;

-------------------------- CALC COORD --------------------------

PROCEDURE b_s(v: VIEW; t: def.VERTEX; VAR x,y :INTEGER);
  VAR T: def.VERTEX;
BEGIN
  WITH v^ DO
    IF vmode THEN
      bmt.VxM(T,t,matrix);
      x:= mth.round(T[Xc]);
      y:= mth.round(t[Yc])
    ELSE
      x:= mth.round((t[Xc] - X_lf) * scale);
      y:= mth.round((t[Yc] - Y_dw) * scale)
    END
  END
END b_s;

PROCEDURE s_b(v: VIEW; x,y: INTEGER; VAR t: def.VERTEX);
BEGIN
  WITH v^ DO
    t[Xc]:= FLOAT(x) / scale + X_lf;
    t[Yc]:= FLOAT(y) / scale + Y_dw;
    t[Zc]:= cntx^.c_Z
  END
END s_b;

PROCEDURE char(c: CHAR): BOOLEAN;
BEGIN
  IF (c=15c) OR (c=33c) OR (c=10c) OR (c=16c) OR (c=17c)THEN RETURN FALSE END;
  RETURN ((0c<c) & (c<177c)) OR ((c>=254c) & (c<377c))
END char;

PROCEDURE numb(c: CHAR): BOOLEAN;
BEGIN RETURN (c ='-') OR (c='+') OR ((c >= 060c) & (c <= 71c)) END numb;

PROCEDURE board?(top: def.VERTEX): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 2 DO
    IF ABS(top[i]) > Max THEN
      IF ENG THEN
        bpm.message(240,12,'too big coordinate %f>%f',top[i],Max)
      ELSE
        bpm.message(240,12,'слишком большая координата %f>%f',top[i],Max)
      END;
    RETURN FALSE END
  END;
  RETURN TRUE
END board?;

PROCEDURE erase_view(v: VIEW);
  VAR t: wnd.TOOL;
BEGIN
  t:= v^.tool;
  t.color:= BITSET(bckg); t.mode := wnd.rep; t.mask := {0..3};
  WITH t.clip DO wnd.rect(v^.wind,t,x,y,x+w,y+h) END
END erase_view;

PROCEDURE extr(m: VMODEL; top: def.VERTEX);
BEGIN
  WITH m^.area DO
    IF top[0] < x THEN x:= top[0]-0.01 END;
    IF top[0] > X THEN X:= top[0]+0.01 END;
    IF top[1] < y THEN y:= top[1]-0.01 END;
    IF top[1] > Y THEN Y:= top[1]+0.01 END;
    IF top[2] < z THEN z:= top[2]-0.01 END;
    IF top[2] > Z THEN Z:= top[2]+0.01 END
  END
END extr;

PROCEDURE extr1(m :VMODEL; top: def.VERTEX; r:REAL);
BEGIN
  r:=ABS(r);
  WITH m^.area DO
    IF top[0]-r < x THEN x:= top[0]-r-0.01 END;
    IF top[0]+r > X THEN X:= top[0]+r+0.01 END;
    IF top[1]-r < y THEN y:= top[1]-r-0.01 END;
    IF top[1]+r > Y THEN Y:= top[1]+r+0.01 END;
    IF top[2]-r < z THEN z:= top[2]-r-0.01 END;
    IF top[2]+r > Z THEN Z:= top[2]+r+0.01 END
  END
END extr1;

PROCEDURE extr_lin(m: VMODEL; l: def.Line);
BEGIN extr(m,l.xyz); extr(m,l.XYZ) END extr_lin;

PROCEDURE extr_cir(m: VMODEL; c: def.Circ);
BEGIN extr1(m,c.centr,c.r) END extr_cir;

PROCEDURE extr_arc(m: VMODEL; a: def.Arc);
BEGIN
  extr(m,a.xyz1); extr(m,a.xyz2); extr(m,a.xyz3)
END extr_arc;

PROCEDURE extr_ell(m: VMODEL; a: def.Ellips);
BEGIN
END extr_ell;

PROCEDURE extr_pln(m: VMODEL; p: def.Pline);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(p.body) DO extr(m,p.body[i]) END
END extr_pln;

PROCEDURE extr_txt(m: VMODEL; t: def.Text);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(t.pict) DO
    extr(m,t.pict[i,0]);
    extr(m,t.pict[i,1])
  END
END extr_txt;

PROCEDURE extr_srf(m: VMODEL; s: def.Surf);
BEGIN
END extr_srf;

PROCEDURE extr_grp(m: VMODEL; g: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH g DO
    FOR i:=0 TO HIGH(line)   DO  extr_lin(m,line[i])   END;
    FOR i:=0 TO HIGH(circ)   DO  extr_cir(m,circ[i])   END;
    FOR i:=0 TO HIGH(arc)    DO  extr_arc(m,arc[i])    END;
    FOR i:=0 TO HIGH(ellips) DO  extr_ell(m,ellips[i]) END;
    FOR i:=0 TO HIGH(pline)  DO  extr_pln(m,pline[i])  END;
    FOR i:=0 TO HIGH(txt)    DO  extr_txt(m,txt[i])    END;
    FOR i:=0 TO HIGH(surf)   DO  extr_srf(m,surf[i])   END
  END
END extr_grp;

---------------------------- CURSORS ---------------------------

PROCEDURE cross(X,Y: INTEGER);
BEGIN
  WITH cview^.tool.clip DO
    wnd.line(cview^.wind,cview^.tool,x,Y,w+x,Y);
    wnd.line(cview^.wind,cview^.tool,X,y,X,h+y)
  END
END cross;

PROCEDURE cross_line(X,Y: INTEGER);
BEGIN
  WITH cview^.tool.clip DO
    wnd.line(cview^.wind,cview^.tool,x,Y,w+x,Y);
    wnd.line(cview^.wind,cview^.tool,X,y,X,h+y)
  END;
  wnd.line(cview^.wind,cview^.tool,x_old,y_old,X,Y)
END cross_line;

PROCEDURE cross_vline(X,Y: INTEGER);
BEGIN
  WITH cview^.tool.clip DO
    wnd.line(cview^.wind,cview^.tool,x,Y,w+x,Y);
    wnd.line(cview^.wind,cview^.tool,X,y,X,h+y)
  END;
  wnd.line(cview^.wind,cview^.tool,x_old,y_old,x_old,Y)
END cross_vline;

PROCEDURE cross_hline(X,Y: INTEGER);
BEGIN
  WITH cview^.tool.clip DO
    wnd.line(cview^.wind,cview^.tool,x,Y,w+x,Y);
    wnd.line(cview^.wind,cview^.tool,X,y,X,h+y)
  END;
  wnd.line(cview^.wind,cview^.tool,x_old,y_old,X,y_old)
END cross_hline;

PROCEDURE cross_cir(X,Y: INTEGER);
  VAR dx,dy,r: INTEGER;
BEGIN
  dx:=x_old-X;
  dy:=y_old-Y;
  r:=bmt.sqrt(dx*dx+dy*dy);
  WITH cview^.tool.clip DO
    wnd.line(cview^.wind,cview^.tool,x,Y,w+x,Y);
    wnd.line(cview^.wind,cview^.tool,X,y,X,h+y)
  END;
  wnd.circle(cview^.wind,cview^.tool,x_old,y_old,r)
END cross_cir;

PROCEDURE cross_box(X,Y: INTEGER);
BEGIN
  wnd.line(cview^.wind,cview^.tool,X,Y,x_old,Y);
  wnd.line(cview^.wind,cview^.tool,X,y_old,X,Y);
  wnd.line(cview^.wind,cview^.tool,x_old,y_old,x_old,Y);
  wnd.line(cview^.wind,cview^.tool,x_old,y_old,X,y_old)
END cross_box;

PROCEDURE cross_arc(X,Y: INTEGER);
  VAR xc,yc,r,t: INTEGER;
BEGIN
  WITH cview^.tool.clip DO
    wnd.line(cview^.wind,cview^.tool,x,Y,w+x,Y);
    wnd.line(cview^.wind,cview^.tool,X,y,X,h+y)
  END;
  wnd.arc3(cview^.wind,cview^.tool,x_old,y_old,X,Y,x_old1,y_old1)
END cross_arc;

PROCEDURE marker(x,y: INTEGER);
BEGIN wnd.circle(cview^.wind,cview^.tool,x,y,3); wnd.dot(cview^.wind,cview^.tool,x,y) END marker;

VAR  dx_mrk,dy_mrk: INTEGER;

PROCEDURE set_marked(base: def.VERTEX);
  VAR _w,_h: INTEGER;
BEGIN
 WITH cview^ DO
    _w:= mth.round((X_mrk-x_mrk)*scale);
    IF _w > 2 * wind^.w THEN _w:=2* wind^.w END;
    _h:= mth.round((Y_mrk-y_mrk)*scale);
    IF _h > 2 * wind^.h THEN _h:=2* wind^.h END;
  END;
  WITH MRK^ DO
    model:= cview^.model;
    cntx:= cview^.cntx;
    scale:= cview^.scale;
    Xc:= cview^.Xc;
    Yc:= cview^.Yc;
    Zc:= cview^.Zc;
    vmode:= cview^.vmode;
    matrix:= cview^.matrix;
    wnd.resize(wind,_w+1,_h+1); done:= wnd.done; error:=wnd.error;
    IF NOT done THEN no_mem; RETURN END;
    X_lf:=x_mrk;
    Y_dw:=y_mrk;
    dx_mrk:=mth.round((base[Xc]-x_mrk)*scale);
    dy_mrk:=mth.round((base[Yc]-y_mrk)*scale);
    tool:= wind^.inner;
    WITH tool DO
      mode:= wnd.rep;
      color:= BITSET(rubb);
      mask:=  {3}
    END;
    wnd.erase(wind);
    show_mrk(MRK)
  END
END set_marked;

PROCEDURE cross_marked(x,y: INTEGER);
  VAR b: wnd.BLOCK;
BEGIN
  cross(x,y);
  WITH MRK^ DO
    wnd.bblt(cview^.wind,cview^.tool,x-dx_mrk,y-dy_mrk,wind,tool,tool.clip)
  END
END cross_marked;

PROCEDURE end_marked;
BEGIN
  MRK^.model:=NIL;
  wnd.resize(MRK^.wind,1,1);
  done:= wnd.done; error:=wnd.error;
  IF NOT done THEN no_mem; wnd.erase(MRK^.wind); RETURN END;
END end_marked;

----------------------------- SHOW -----------------------------

PROCEDURE clip?(c1,c2: obj.Clip): BOOLEAN;
BEGIN
  WITH c1 DO RETURN (x>c2.X) OR (X<c2.x) OR (y>c2.Y) OR (Y<c2.y) END
END clip?;

PROCEDURE _show_vector(v: VIEW; t1,t2: def.VERTEX);
  VAR x1,y1,x2,y2: INTEGER;
BEGIN
  b_s(v,t1,x1,y1); b_s(v,t2,x2,y2);
  wnd.line(v^.wind,v^.tool,x1,y1,x2,y2)
END _show_vector;

PROCEDURE xshow_vector(v: VIEW; t1,t2: def.VERTEX; c: COLOR);
BEGIN
  v^.tool.color:=BITSET(c);
  v^.tool.mode:= wnd.rep;
  v^.tool.mask:= {0..3};
  _show_vector(v,t1,t2)
END xshow_vector;

PROCEDURE _show_vpic(v: VIEW; p: def.VPICTURE);
  VAR x,y,x1,y1: INTEGER;
              i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(p) DO
    b_s(v,p[i,0],x,y);
    b_s(v,p[i,1],x1,y1);
    wnd.line(v^.wind,v^.tool,x,y,x1,y1)
  END
END _show_vpic;

PROCEDURE _show_line(v: VIEW; lin: def.Line);
  VAR x1,y1,x2,y2: INTEGER;
                i: INTEGER;
BEGIN
  WITH lin DO
    IF HIGH(pict) <0 THEN _show_vector(v,xyz,XYZ)
    ELSE _show_vpic(v,pict)
    END
  END
END _show_line;

PROCEDURE _tool(VAR t: wnd.TOOL; ltyp: INTEGER; c: COLOR);
BEGIN
  WITH t DO
    mode:= wnd.rep; mask:= {0..3};
    IF ltyp#4 THEN color:= BITSET(c)
    ELSE           color:= BITSET(c+8) END
  END
END _tool;

PROCEDURE show_line(model: VMODEL; lin: def.Line);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _tool(views[i]^.tool,lin.ltyp,lin.color); _show_line(views[i],lin)
    END
  END
END show_line;

PROCEDURE _xtool(VAR t: wnd.TOOL; c: COLOR);
BEGIN
  WITH t DO color:=BITSET(c); mode:= wnd.rep; mask:= {0..3} END;
END _xtool;

PROCEDURE xshow_line(model: VMODEL; lin: def.Line; c: COLOR);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _xtool(views[i]^.tool,c);
      _show_line(views[i],lin)
    END
  END
END xshow_line;

PROCEDURE _show_pic(v: VIEW; p: def.PICTURE);
  VAR x,y,x1,y1: INTEGER;
            i,j: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(p) DO
    b_s(v,p[i,0],x,y);
    FOR j:=1 TO HIGH(p[i]) DO
      b_s(v,p[i,j],x1,y1);
      wnd.line(v^.wind,v^.tool,x,y,x1,y1);
      x:=x1; y:=y1;
    END
  END
END _show_pic;

PROCEDURE _show_cir(v: VIEW; cir: def.Circ);
  VAR x,y: INTEGER;
BEGIN
  WITH cir DO
    IF HIGH(pict)<0 THEN
      b_s(v,centr,x,y);
      wnd.circle(v^.wind,v^.tool,x,y,mth.round(r * v^.scale))
    ELSE _show_pic(v,pict) END
  END
END _show_cir;

PROCEDURE show_cir(model: VMODEL; cir: def.Circ);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _tool(views[i]^.tool,cir.ltyp,cir.color); _show_cir(views[i],cir)
    END
  END
END show_cir;

PROCEDURE xshow_cir(model: VMODEL; cir: def.Circ; c: COLOR);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _xtool(views[i]^.tool,c); _show_cir(views[i],cir)
    END
  END
END xshow_cir;

PROCEDURE _show_pline(v: VIEW; pln: def.Pline);
  VAR x,y,x1,y1,i: INTEGER;
BEGIN
  WITH pln DO
    IF HIGH(pict)< 0 THEN
      b_s(v,body[0],x,y);
      FOR i:=1 TO HIGH(body) DO
        b_s(v,body[i],x1,y1);
        wnd.line(v^.wind,v^.tool,x,y,x1,y1);
        x:= x1; y:=y1
      END
    ELSE _show_pic(v,pict) END
  END
END _show_pline;

PROCEDURE show_pline(model: VMODEL; pln: def.Pline);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _tool(views[i]^.tool,pln.ltyp,pln.color); _show_pline(views[i],pln)
    END
  END
END show_pline;

PROCEDURE xshow_pline(model: VMODEL; pln: def.Pline; c: COLOR);
  VAR i: INTEGER;
BEGIN
   WITH model^ DO
     FOR i:=0 TO HIGH(views) DO
       _xtool(views[i]^.tool,c); _show_pline(views[i],pln)
     END
  END
END xshow_pline;

PROCEDURE _show_arc(v: VIEW; arc: def.Arc);
VAR x1,y1,x3,y3,x,y: INTEGER;
BEGIN
  WITH arc DO
    IF HIGH(pict) <0 THEN
      b_s(v,xyz1,x1,y1);
      b_s(v,xyz3,x3,y3);
      b_s(v,xyz2,x,y);
      wnd.arc3(v^.wind,v^.tool,x1,y1,x,y,x3,y3)
    ELSE _show_pic(v,pict) END
  END
END _show_arc;

PROCEDURE show_arc(model: VMODEL; arc: def.Arc);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _tool(views[i]^.tool,arc.ltyp,arc.color);
      _show_arc(views[i],arc)
    END
  END
END show_arc;

PROCEDURE xshow_arc(model: VMODEL; arc: def.Arc; c: COLOR);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _xtool(views[i]^.tool,c);  _show_arc(views[i],arc)
    END
  END
END xshow_arc;

PROCEDURE _show_ellips(v: VIEW; ell: def.Ellips);
BEGIN
END _show_ellips;

PROCEDURE show_ellips(model: VMODEL; ell: def.Ellips);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _tool(views[i]^.tool,ell.ltyp,ell.color); _show_ellips(views[i],ell)
    END
  END
END show_ellips;

PROCEDURE xshow_ellips(model: VMODEL; ell: def.Ellips; c: COLOR);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _xtool(views[i]^.tool,c); _show_ellips(views[i],ell)
    END
  END
END xshow_ellips;

PROCEDURE _show_text(v: VIEW; t: def.Text);
BEGIN _show_vpic(v,t.pict) END _show_text;

PROCEDURE show_text(model: VMODEL; txt: def.Text);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
     _tool(views[i]^.tool,0,txt.color);
     _show_text(views[i],txt)
    END
  END
END show_text;

PROCEDURE xshow_text(model: VMODEL; txt: def.Text; c: COLOR);
  VAR i: INTEGER;
BEGIN
  WITH model^ DO
    FOR i:=0 TO HIGH(views) DO
      _xtool(views[i]^.tool,c);
      _show_text(views[i],txt)
    END
  END
END xshow_text;

PROCEDURE show_surf(model: VMODEL; srf: def.Surf);
BEGIN
END show_surf;

PROCEDURE xshow_surf(model: VMODEL; srf: def.Surf);
BEGIN
END xshow_surf;

PROCEDURE _rshow_group(v: VIEW; grp: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line)   DO
      _tool(v^.tool,line[i].ltyp,line[i].color);
      _show_line(v,line[i])
    END;
    FOR i:=0 TO HIGH(circ)   DO
      _tool(v^.tool,circ[i].ltyp,circ[i].color);
      _show_cir(v,circ[i])
    END;
    FOR i:=0 TO HIGH(arc)    DO
      _tool(v^.tool,arc[i].ltyp,arc[i].color);
      _show_arc(v,arc[i])
    END;
    FOR i:=0 TO HIGH(ellips) DO
      _tool(v^.tool,ellips[i].ltyp,ellips[i].color);
      _show_ellips(v,ellips[i])
    END;
    FOR i:=0 TO HIGH(pline)  DO
      _tool(v^.tool,pline[i].ltyp,pline[i].color);
      _show_pline (v,pline[i])  END;
    FOR i:=0 TO HIGH(txt)    DO
      _xtool(v^.tool,txt[i].color);
      _show_text(v,txt[i])
    END
  END
END _rshow_group;

PROCEDURE _show_group(v: VIEW; grp: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line)   DO _show_line(v,line[i])     END;
    FOR i:=0 TO HIGH(circ)   DO _show_cir(v,circ[i])      END;
    FOR i:=0 TO HIGH(arc)    DO _show_arc(v,arc[i])       END;
    FOR i:=0 TO HIGH(ellips) DO _show_ellips(v,ellips[i]) END;
    FOR i:=0 TO HIGH(pline)  DO _show_pline (v,pline[i])  END;
    FOR i:=0 TO HIGH(txt)    DO _show_text(v,txt[i])      END
  END
END _show_group;

PROCEDURE show_group(model: VMODEL; grp: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line)   DO show_line(model,line[i])     END;
    FOR i:=0 TO HIGH(circ)   DO show_cir (model,circ[i])     END;
    FOR i:=0 TO HIGH(arc )   DO show_arc (model,arc[i] )     END;
    FOR i:=0 TO HIGH(ellips) DO show_ellips(model,ellips[i]) END;
    FOR i:=0 TO HIGH(txt)    DO show_text(model,txt[i])      END;
    FOR i:=0 TO HIGH(pline)  DO show_pline(model,pline[i])   END
  END
END show_group;

PROCEDURE xshow_group(model: VMODEL; grp: def.Group; c: COLOR);
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line)   DO xshow_line  (model,line[i],c)   END;
    FOR i:=0 TO HIGH(circ)   DO xshow_cir   (model,circ[i],c)   END;
    FOR i:=0 TO HIGH(arc )   DO xshow_arc   (model,arc[i],c )   END;
    FOR i:=0 TO HIGH(ellips) DO xshow_ellips(model,ellips[i],c) END;
    FOR i:=0 TO HIGH(txt)    DO xshow_text  (model,txt[i],c)    END;
    FOR i:=0 TO HIGH(pline)  DO xshow_pline (model,pline[i],c)  END
  END
END xshow_group;

PROCEDURE show_mrk(v: VIEW);
  VAR i: INTEGER;
BEGIN
 FOR i:=0 TO HIGH(obj.mark_lin) DO _show_line  (v,obj.mark_lin[i]^.line  ) END;
 FOR i:=0 TO HIGH(obj.mark_cir) DO _show_cir   (v,obj.mark_cir[i]^.circ  ) END;
 FOR i:=0 TO HIGH(obj.mark_arc) DO _show_arc   (v,obj.mark_arc[i]^.arc   ) END;
 FOR i:=0 TO HIGH(obj.mark_pln) DO _show_pline (v,obj.mark_pln[i]^.pline ) END;
 FOR i:=0 TO HIGH(obj.mark_txt) DO _show_text  (v,obj.mark_txt[i]^.text  ) END;
 FOR i:=0 TO HIGH(obj.mark_ell) DO _show_ellips(v,obj.mark_ell[i]^.ellips) END;
 FOR i:=0 TO HIGH(obj.mark_grp) DO _show_group (v,obj.mark_grp[i]^.group ) END
END show_mrk;

------------------------- coord & grid -------------------------

VAR TCOORD: wnd.TOOL;

PROCEDURE show_coord;
BEGIN
  WITH cview^ DO
    WITH cntx^ DO
      c_X:= FLOAT(sX)/ scale + X_lf;
      c_Y:= FLOAT(sY)/ scale + Y_dw;
      wnd.print(WCOORD,TCOORD,8,3,bpm.font,' %.2f  %.2f   ',c_X,c_Y)
    END
  END
END show_coord;

PROCEDURE show_grid(v: VIEW);
  VAR dx,dy: INTEGER;
       corn: def.VERTEX;
          b: wnd.BLOCK;
          t: wnd.TOOL;
BEGIN
  WITH v^ DO
    WITH cntx^ DO
      dx:= mth.round(gstep_x * scale);
      dy:= mth.round(gstep_y * scale);
      IF (dx<5) OR (dy<5) OR (dx>tool.clip.w) OR (dy>tool.clip.h) THEN RETURN END;
      corn[Xc]:= gstep_x* FLOAT(TRUNC(X_lf/ gstep_x));
      corn[Yc]:= gstep_y* FLOAT(TRUNC(Y_dw/ gstep_y));
      b_s(v,corn,b.x,b.y);
      IF b.x < 0 THEN b.x:= b.x+dx END;
      IF b.y < 0 THEN b.y:= b.y+dy END;
      b.w:= wind^.inner.clip.w;
      b.h:= wind^.inner.clip.h;
      t:= tool;
      tool.color:= BITSET(gray);
      tool.mode := wnd.or;
      tool.mask := {3};
      wnd.grid(wind,tool,b,dx,dy);
      tool:= t
    END
  END
END show_grid;

PROCEDURE erase_grid(v: VIEW);
  VAR dx,dy: INTEGER;
       corn: def.VERTEX;
          b: wnd.BLOCK;
          t: wnd.TOOL;
BEGIN
  WITH v^ DO
    WITH cntx^ DO
      dx:= mth.round(gstep_x * scale);
      dy:= mth.round(gstep_y * scale);
      IF (dx<5) OR (dy<5) OR (dx>tool.clip.w) OR (dy>tool.clip.h) THEN RETURN END;
      corn[Xc]:= gstep_x* FLOAT(TRUNC(X_lf/ gstep_x));
      corn[Yc]:= gstep_y* FLOAT(TRUNC(Y_dw/ gstep_y));
      b_s(v,corn,b.x,b.y);
      IF b.x < 0 THEN b.x:= b.x+dx END;
      IF b.y < 0 THEN b.y:= b.y+dy END;
      b.w:= wind^.inner.clip.w;
      b.h:= wind^.inner.clip.h;
      t:= tool;
      tool.color:= BITSET(gray);
      tool.mode := wnd.bic;
      tool.mask := {3};
      wnd.grid(wind,tool,b,dx,dy);
      tool:= t
    END
  END
END erase_grid;

---------------------------- REFRESH ---------------------------

PROCEDURE mrefresh;
  VAR i: INTEGER;
BEGIN
  WITH cview^.model^ DO
    FOR i:=0 TO HIGH(views) DO
      vredraw(views[i])
    END
  END
END mrefresh;

PROCEDURE vrefresh;
BEGIN
END vrefresh;

PROCEDURE vredraw(v: VIEW);
  VAR wclip: obj.Clip;

PROCEDURE ref_line(lay: obj.Layer);
  VAR lin: obj.lin_ptr;
BEGIN
  WITH lay^ DO
    lin:= lines;
    IF lin#NIL THEN
      REPEAT
        WITH lines^ DO
          IF NOT clip?(rect,wclip) THEN
            _tool(v^.tool,line.ltyp,line.color);
            _show_line(v,line)
          END;
          lines:= next
        END
      UNTIL lin = lines
    END
  END
END ref_line;

PROCEDURE ref_circle(lay: obj.Layer);
  VAR cir: obj.cir_ptr;
BEGIN
  WITH lay^ DO
    cir:= circs;
    IF cir#NIL THEN
      REPEAT
        WITH circs^ DO
          IF NOT clip?(rect,wclip) THEN
            _tool(v^.tool,circ.ltyp,circ.color);
            _show_cir(v,circ)
          END;
          circs:= next
        END
      UNTIL cir = circs
    END
  END;
END ref_circle;

PROCEDURE ref_arc(lay: obj.Layer);
  VAR a: obj.arc_ptr;
BEGIN
  WITH lay^ DO
    a:= arcs;
    IF a # NIL THEN
      REPEAT
        WITH arcs^ DO
          IF NOT clip?(rect,wclip) THEN
            _tool(v^.tool,arc.ltyp,arc.color);
            _show_arc(v,arc)
          END;
          arcs:= next
        END
      UNTIL a = arcs
    END
  END
END ref_arc;

PROCEDURE ref_pline(lay: obj.Layer);
  VAR plin: obj.pln_ptr;
BEGIN
  WITH lay^ DO
    plin:= plins;
    IF plin#NIL THEN
      REPEAT
        WITH plins^ DO
          IF NOT clip?(rect,wclip) THEN
            _tool(v^.tool,pline.ltyp,pline.color);
            _show_pline(v,pline)
          END;
          plins:= next
        END
      UNTIL plin = plins
    END
  END
END ref_pline;

PROCEDURE ref_txt(lay: obj.Layer);
  VAR txt: obj.txt_ptr;
        j: INTEGER;
BEGIN
  WITH lay^ DO
    txt:= texts;
    IF txt#NIL THEN
      REPEAT
        WITH texts^ DO
          IF NOT clip?(rect,wclip) THEN
            _xtool(v^.tool,text.color);
            _show_text(v,text)
          END;
          texts:= next
        END
      UNTIL txt = texts
    END
  END
END ref_txt;

PROCEDURE ref_grp(lay: obj.Layer);
  VAR grp: obj.grp_ptr;
        j: INTEGER;
BEGIN
  WITH lay^ DO
    grp:= grps;
    IF grp#NIL THEN
      REPEAT
        WITH grps^ DO
          IF NOT clip?(rect,wclip) THEN
            _rshow_group(v,group)
          END;
          grps:= next
        END
      UNTIL grp = grps
    END
  END
END ref_grp;

  VAR ld,ur: def.VERTEX;
          i: INTEGER;
BEGIN
  WITH v^ DO
    IF icon THEN RETURN  END;
    wnd.mode(wind,wnd.img);
    erase_view(v);
    IF cntx^.GRID THEN show_grid(v) END;
    tool.mode:= wnd.rep;
    tool.mask:= {0..3};
    WITH tool.clip DO s_b(v,x,y,ld); s_b(v,x+w,y+h,ur) END;
    wclip.x:= mth.round(ld[Xc]);  wclip.y:= mth.round(ld[Yc]);
    wclip.X:= mth.round(ur[Xc]);  wclip.Y:= mth.round(ur[Yc]);
    WITH model^ DO
      IF mbody=NIL THEN bad_desc; RETURN END;
      FOR i:=0 TO HIGH(mbody^) DO
        IF cntx^.mask[i] # 0 THEN
          ref_line  (mbody^[i]);
          ref_circle(mbody^[i]);
          ref_arc   (mbody^[i]);
          ref_pline (mbody^[i]);
          ref_txt   (mbody^[i]);
          ref_grp   (mbody^[i]);
        END
      END
    END;
    IF MRK^.model= model THEN
      tool.color:= BITSET(mark);
      tool.mask := BITSET(mark);
      show_mrk(v)
    END;
    wnd.mode(wind,wnd.normal);
    wnd.refresh(wind)
  END
END vredraw;

PROCEDURE mredraw(mod: VMODEL);
 VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(mod^.views) DO vredraw(mod^.views[i]) END
END mredraw;

----------------------------- READ -----------------------------

VAR state: BITSET;  (* last pressed mous button *)

PROCEDURE rd_point(prompt,conf: ARRAY OF CHAR; VAR point: def.VERTEX): BOOLEAN;
  VAR dn: BOOLEAN;
       p: INTEGER;
BEGIN
  WITH cview^ DO
    p:=0;
    IF NOT bpm.dia_lin(conf,prompt) THEN  RETURN FALSE END;
    str.rscan(point[Xc],conf,p,dn);
    IF (NOT dn) THEN RETURN FALSE END;
    str.rscan(point[Yc],conf,p,dn);
    IF (NOT dn) THEN RETURN FALSE END;
    str.rscan(point[Zc],conf,p,dn);
    IF (NOT dn) THEN point[Zc]:= cntx^.c_Z END;
    RETURN board?(point)
  END
END rd_point;

PROCEDURE rd_numb(prompt,conf: ARRAY OF CHAR; VAR x:REAL): BOOLEAN;
  VAR p: INTEGER;
      d: BOOLEAN;
BEGIN
  p:=0; d:=FALSE;
  IF NOT bpm.dia_lin(conf,prompt) THEN RETURN FALSE END;
  str.rscan(x,conf,p,d);
  RETURN d
END rd_numb;

PROCEDURE rd_numb1(prompt: ARRAY OF CHAR; VAR r:REAL; x,y: INTEGER): BOOLEAN;
  VAR s: ARRAY [0..31] OF CHAR;
      p: INTEGER;
      d: BOOLEAN;
      q: REAL;
BEGIN
  p:=0; d:=FALSE;
  str.print(s,' %c',0c);
  bpm.diabox(x,y,120,s,'%s <%f>',prompt,r);
  str.rscan(q,s,p,d);
  IF d THEN r:=q END;
  RETURN d
END rd_numb1;

VAR  sstep_x,sstep_y: INTEGER; (* step cursor at screen                *)

PROCEDURE step;
 VAR sx,sy: REAL;
       top: def.VERTEX;
BEGIN
  WITH cview^ DO
    WITH cntx^ DO
      IF STEP THEN
        sstep_x:= mth.round(cstep_x * cview^.scale);
        sstep_y:= mth.round(cstep_y * cview^.scale);
        IF sstep_x=0 THEN sstep_x:=1 END;
        IF sstep_y=0 THEN sstep_y:=1 END;
        sx:= FLOAT(sX)/ cview^.scale + cview^.X_lf;
        sy:= FLOAT(sY)/ cview^.scale + cview^.Y_dw;
        top[Xc]:= gstep_x* FLOAT(TRUNC(sx / gstep_x));
        top[Yc]:= gstep_y* FLOAT(TRUNC(sy / gstep_y));
        b_s(cview,top,sX,sY)
      ELSE sstep_x:=1; sstep_y:=1 END
    END
  END
END step;

VAR xo,yo: INTEGER; (* положение последнего нарисованного курсора *)
    shift: BOOLEAN;

PROCEDURE pan(view: VIEW; dir: INTEGER);
  VAR  dx,dy: INTEGER;
         T,t: wnd.TOOL;

PROCEDURE _set;
BEGIN
  T:= view^.tool;
  view^.tool.mask:= {0..3};
  t:= view^.tool;
  WITH view^.tool.clip DO
    CASE dir OF
      0: w:=33;             dx:=-32; dy:=  0;
     |1: x:=x+w-33; w:=33;  dx:= 32; dy:=  0;
     |2: h:=33;             dx:=  0; dy:=-32;
     |3: y:=y+h-33; h:=33;  dx:=  0; dy:= 32;
    ELSE END
  END
END _set;

BEGIN
  _set;
  shift:=TRUE;
  WITH view^ DO
    wnd.mode(wind,wnd.img);
    X_lf:= X_lf+FLOAT(dx)/scale;
    Y_dw:= Y_dw+FLOAT(dy)/scale;
    x_old:= x_old -dx; x_old1:= x_old1 -dx;
    y_old:= y_old -dy; y_old1:= y_old1 -dy;
    wnd.scroll(wind,t,dx,dy);
    vredraw(view);
    wnd.mode(wind,wnd.scr);
    tool:= T;
    bpm.clear_mou
  END
END pan;

CONST _lf=0; _rg=1; _dw=2; _up=3;

PROCEDURE in_dec(ch: CHAR; dx,dy:INTEGER);
  VAR d_y,d_x,D_y,D_x: INTEGER;
BEGIN
  WITH cview^.cntx^ DO
    CASE ch OF
       key.up   : INC(sY,sstep_y)
      |key.dw   : DEC(sY,sstep_y)
      |key.left : DEC(sX,sstep_x)
      |key.right: INC(sX,sstep_x)
      |key.f3   : DEC(sX,sstep_x*10)
      |key.f4   : INC(sX,sstep_x*10)
      |key.pgup:  INC(sY,sstep_y*10)
      |key.pgdw:  DEC(sY,sstep_y*10)
    ELSE
      IF STEP THEN INC(sX,sstep_x*dx); INC(sY,dy*sstep_y)
      ELSE         INC(sX,dx);         INC(sY,dy)       END
    END;
    WITH cview^.tool.clip DO
      IF PAN THEN
        IF    sX > w+x THEN
          pan(cview,_rg); sX:= w+x; step; xo:= cview^.cntx^.sX
        ELSIF sX < x   THEN
          pan(cview,_lf); sX:= x;   step; xo:= cview^.cntx^.sX
        END;
        IF    sY > y+h THEN
          pan(cview,_up); sY:= y+h; step; yo:= cview^.cntx^.sY
        ELSIF sY < y   THEN
          pan(cview,_dw); sY:= y;   step; yo:= cview^.cntx^.sY
        END;
      ELSE
        IF    sX > w+x THEN sX:= (w+x) DIV sstep_x * sstep_x
        ELSIF sX < x   THEN sX:=   x   DIV sstep_x * sstep_x END;
        IF    sY > y+h THEN sY:= (h+y) DIV sstep_y * sstep_y
        ELSIF sY < y   THEN sY:=   y   DIV sstep_y * sstep_y END
      END
    END
  END;
  show_coord
END in_dec;

     (* спасение всякой xерни для zoom'а *)
  VAR old1,old: def.VERTEX;
         mview: VIEW;
            zm: BOOLEAN;

PROCEDURE zoom_scr;
  VAR top: def.VERTEX;
BEGIN
  s_b(cview,x_old,y_old,old);
  s_b(cview,x_old1,y_old1,old1);
  mview:= cview;
  WITH cview^ DO
    wnd.move(ZM^.wind,wind^.x+tool.clip.x+cntx^.sX-50,wind^.y+tool.clip.y+cntx^.sY-50);
    s_b(cview,cntx^.sX,cntx^.sY,top)
  END;
  WITH ZM^ DO
    scale:=cview^.scale*2.;
    X_lf:= top[Xc]-FLOAT(wind^.inner.clip.w DIV 2) /scale;
    Y_dw:= top[Yc]-FLOAT(wind^.inner.clip.h DIV 2) /scale;
    Xc:= cview^.Xc;
    Yc:= cview^.Yc;
    Zc:= cview^.Zc;
    vmode:= cview^.vmode;
    model:= cview^.model;
    cntx:= cview^.cntx;
    cntx^.sX:=50;
    cntx^.sY:=50;
--    wnd.ontop(wind);
    wnd.object(wind,ZM);
    wnd.putover(wind,cview^.wind);
    vredraw(ZM);
    tool.mode:= wnd.xor;
    tool.mask:= {3};
    tool.color:= BITSET(rubb);
    wnd.open(wind);
  END;
  b_s(ZM,old,x_old,y_old);
  b_s(ZM,old1,x_old1,y_old1);
  cview:= ZM;
  zm:= TRUE
END zoom_scr;

PROCEDURE zoom_more;
BEGIN
END zoom_more;

PROCEDURE un_zoom;
  VAR top: def.VERTEX;
BEGIN
  s_b(cview,cview^.cntx^.sX,cview^.cntx^.sY,top);
  wnd.close(cview^.wind);
  cview:= mview;
  b_s(cview,old,x_old,y_old);
  b_s(cview,old1,x_old1,y_old1);
  b_s(cview,top,cview^.cntx^.sX,cview^.cntx^.sY);
  zm:= FALSE
END un_zoom;

PROCEDURE _coord;
BEGIN
  IF WCOORD^.closed THEN wnd.open(WCOORD); show_coord
  ELSE wnd.close(WCOORD) END
END _coord;

PROCEDURE _color;
BEGIN
  WITH cview^ DO set.color(wind^.x+cntx^.sX,wind^.y+cntx^.sY) END
END _color;

PROCEDURE _ltyp;
BEGIN
  WITH cview^ DO set.type_line(wind^.x+cntx^.sX,wind^.y+cntx^.sY) END
END _ltyp;

PROCEDURE _layer;
BEGIN
  WITH cview^ DO set.layers(wind^.x+cntx^.sX,wind^.y+cntx^.sY) END
END _layer;

PROCEDURE _step;
BEGIN
  WITH cview^ DO set.step(wind^.x+cntx^.sX,wind^.y+cntx^.sY) END
END _step;

PROCEDURE _grid;
BEGIN
  WITH cview^ DO set.grid(wind^.x+cntx^.sX,wind^.y+cntx^.sY) END
END _grid;

VAR dX,dY: INTEGER;

PROCEDURE read_point(prompt: ARRAY OF CHAR; cursor: CURSOR; monitor: MONITOR;
                                           VAR TOP: def.VERTEX):BOOLEAN;
 VAR x,y,dx,dy: INTEGER;
      conf: ARRAY [0..31] OF CHAR;
        ch:CHAR;
BEGIN
  bpm.clear_mou;
  zm:=FALSE;
  step;
  WITH cview^ DO
    WITH cntx^ DO
      tool.color:= BITSET(rubb);
      tool.mode := wnd.xor;
      tool.mask := {3};
      show_coord;
      wnd.mode(wind,wnd.scr);
      IF QUERY THEN
        str.print(conf,'%c',0c);
        IF rd_point(prompt,conf,TOP) THEN
          wnd.mode(wind,wnd.normal); RETURN TRUE
        END
      END;
      xo:=sX; yo:=sY;
      dx:=0; dy:=0;
       x:=0;  y:=0;
      IF sstep_x<10 THEN dX:=10 ELSE dX:=sstep_x END;
      IF sstep_y<10 THEN dY:=10 ELSE dY:=sstep_y END;
      cursor(xo,yo);
      LOOP
        IF (dx>dX) OR (dy>dY) THEN
          cursor(xo,yo);  xo:=sX; yo:=sY;
          cursor(sX,sY);  dx:=0; dy:=0
        ELSIF (key.ready()>0) OR (mou.ready()>0) THEN
          bpm.x_read(ch,x,y);
          CASE ch OF
            therd    : bpm.clear_mou; cursor(xo,yo);
                       IF zm THEN un_zoom; xo:=sX; yo:=sY; cursor(xo,yo) ELSE
                       wnd.mode(wind,wnd.normal); RETURN FALSE END
           |first    : bpm.clear_mou; s_b(cview,sX,sY,TOP);  cursor(xo,yo);
                       IF zm THEN un_zoom END;
                       wnd.mode(wind,wnd.normal); RETURN TRUE
           |second   : cursor(xo,yo); IF zm THEN  un_zoom END;
                       IF monitor() THEN RETURN FALSE END;
                       xo:=sX; yo:=sY; cursor(sX,sY)
           |'z','Z'  : cursor(xo,yo);
                       IF NOT zm THEN zoom_scr; cursor(xo,yo) END
           |'g','G'  : cursor(xo,yo); xo:=sX; yo:=sY; _grid; cursor(sX,sY)
           |'s','S'  : cursor(xo,yo); xo:=sX; yo:=sY; _step; cursor(sX,sY)
           |'c','C'  : cursor(xo,yo); xo:=sX; yo:=sY; _coord; cursor(sX,sY)
           |'r','R'  : cursor(xo,yo); xo:=sX; yo:=sY; _color; cursor(sX,sY)
           |'t','T'  : cursor(xo,yo); xo:=sX; yo:=sY; _ltyp;  cursor(sX,sY)
           |'l','L'  : cursor(xo,yo); xo:=sX; yo:=sY; _layer; cursor(sX,sY)
          ELSE
            IF numb(ch) THEN
              str.print(conf,'%c%c',ch,0c);
              IF rd_point(prompt,conf,TOP) THEN
                IF zm THEN un_zoom END;
                cursor(xo,yo);
                wnd.mode(wind,wnd.normal);
                RETURN TRUE
              ELSE cursor(xo,yo); xo:=sX; yo:=sY; cursor(xo,yo) END
            END;
            in_dec(ch,x,y); dx:=ABS(xo-sX); dy:=ABS(yo-sY);
            IF shift THEN cursor(xo,yo); shift:=FALSE END
          END
        ELSIF (sX#xo) OR (sY#yo) THEN
          cursor(xo,yo); xo:=sX; yo:=sY; cursor(sX,sY)
        END
      END
    END
  END
END read_point;

PROCEDURE read_long(top: def.VERTEX; prompt: ARRAY OF CHAR; typ: BITSET;
                   cursor: CURSOR; monitor: MONITOR;VAR R:REAL):BOOLEAN;
 VAR x,y,dx,dy: INTEGER;
           TOP: def.VERTEX;
          conf: ARRAY [0..31] OF CHAR;
            ch:CHAR;

PROCEDURE _long;
BEGIN
  WITH cview^ DO
    s_b(cview,cntx^.sX,cntx^.sY,TOP);
    IF    typ* long#{} THEN R:= bmt.dist1(top,TOP)
    ELSIF typ*hlong#{} THEN R:= ABS(TOP[Xc] - top[Xc])
    ELSIF typ*vlong#{} THEN R:= ABS(TOP[Yc] - top[Yc])
    END
  END
END _long;

BEGIN
  bpm.clear_mou;
  zm:= FALSE;
  step;
  WITH cview^DO
    WITH cntx^ DO
      tool.color:= BITSET(rubb);
      tool.mode := wnd.xor;
      tool.mask := {3};
      show_coord;
      wnd.mode(wind,wnd.scr);
      IF QUERY THEN
        str.print(conf,'%c',0c);
        IF rd_numb(prompt,conf,R) THEN
          wnd.mode(wind,wnd.normal); RETURN TRUE
        END
      END;
      xo:=sX; yo:=sY;
      dx:=0; dy:=0;
       x:=0;  y:=0;
      IF sstep_x<10 THEN dX:=10 ELSE dX:=sstep_x END;
      IF sstep_y<10 THEN dY:=10 ELSE dY:=sstep_y END;
      cursor(xo,yo);
      LOOP
        IF (dx>dX) OR (dy>dY) THEN
          cursor(xo,yo);  xo:=sX; yo:=sY;
          cursor(sX,sY);  dx:=0; dy:=0
        ELSIF (key.ready()>0) OR (mou.ready()>0) THEN
          bpm.x_read(ch,x,y);
          CASE ch OF
            first    : bpm.clear_mou; cursor(xo,yo); _long;
                       IF zm THEN un_zoom END;
                       wnd.mode(wind,wnd.normal); RETURN TRUE
           |second   : cursor(xo,yo); IF zm THEN  un_zoom END;
                       IF monitor() THEN RETURN FALSE END;
                       xo:=sX; yo:=sY; cursor(sX,sY)
           |therd    : bpm.clear_mou; cursor(xo,yo);
                       IF zm THEN un_zoom; xo:=sX; yo:=sY; cursor(xo,yo) ELSE
                       wnd.mode(wind,wnd.normal); RETURN FALSE END
           |'z','Z'  : cursor(xo,yo);
                       IF NOT zm THEN zoom_scr; cursor(xo,yo); END
           |'g','G'  : cursor(xo,yo); xo:=sX; yo:=sY; _grid;  cursor(sX,sY)
           |'s','S'  : cursor(xo,yo); xo:=sX; yo:=sY; _step;  cursor(sX,sY)
           |'c','C'  : cursor(xo,yo); xo:=sX; yo:=sY; _coord; cursor(sX,sY)
           |'r','R'  : cursor(xo,yo); xo:=sX; yo:=sY; _color; cursor(sX,sY)
           |'t','T'  : cursor(xo,yo); xo:=sX; yo:=sY; _ltyp;  cursor(sX,sY)
           |'l','L'  : cursor(xo,yo); xo:=sX; yo:=sY; _layer; cursor(sX,sY)
          ELSE
            IF numb(ch) THEN
              str.print(conf,'%c%c',ch,0c);
              IF rd_numb(prompt,conf,R) THEN
                IF zm THEN un_zoom END;
                cursor(xo,yo); wnd.mode(wind,wnd.normal); RETURN TRUE
              ELSE cursor(xo,yo); xo:=sX; yo:=sY; cursor(xo,yo) END
            END;
            in_dec(ch,x,y); dx:=ABS(xo-sX); dy:=ABS(yo-sY);
            IF shift THEN cursor(xo,yo); shift:=FALSE END
          END
        ELSIF (sX#xo) OR (sY#yo) THEN
          cursor(xo,yo); xo:=sX; yo:=sY; cursor(sX,sY)
        END
      END
    END
  END
END read_long;

PROCEDURE read_angle(VAR top1,top2,top3: def.VERTEX): BOOLEAN;
VAR  s1,s2,s3: ARRAY [0..15] OF CHAR;
BEGIN
  IF ENG THEN
   s1:='Vertex angle:';
   s2:='First line:';
   s3:='Second line:';
  ELSE
    s1:='Вершина угла:';
    s2:='Первый луч:';
    s3:='Второй луч';
  END;
  WITH cview^ DO
    LOOP
      IF read_point(s1,cross,null_monitor,top1) THEN
        b_s(cview,top1,x_old,y_old);
        IF read_point(s2,cross_line,null_monitor,top2) THEN
          IF (top1[Xc]=top2[Xc]) & (top1[Yc]=top2[Yc]) THEN RETURN FALSE END;
          _show_vector(cview,top1,top2);
          IF read_point(s3,cross_line,null_monitor,top3) THEN
            IF (top1[Xc]=top3[Xc]) & (top1[Yc]=top3[Yc]) THEN RETURN FALSE END;
            _show_vector(cview,top1,top2); RETURN TRUE
          END
        END
      END
    END
  END
END read_angle;
--------------------------- READ TEXT --------------------------

PROCEDURE read_size(prompt: ARRAY OF CHAR; cursor: CURSOR; VAR kb: BOOLEAN;
                    VAR top: def.VERTEX): BOOLEAN;
VAR  dx,dy: INTEGER;
      conf: ARRAY [0..31] OF CHAR;
        ch: CHAR;
BEGIN
  zm:= FALSE;
  bpm.clear_mou;
  step;
  WITH cview^ DO
    WITH cntx^ DO
      tool.color:= BITSET(rubb);
      tool.mode := wnd.xor;
      tool.mask := {3};
      wnd.mode(wind,wnd.scr);
      show_coord;
      IF QUERY THEN
        str.print(conf,'%c',0c);
        IF rd_point(prompt,conf,top) THEN
          wnd.mode(wind,wnd.normal); kb:= TRUE; RETURN TRUE
        END
      END;
      cursor(sX,sY);
      dx:=0; dy:=0;
      LOOP
        IF (key.ready()#0) OR (mou.ready()#0) THEN
          bpm.x_read(ch,dx,dy);
          cursor(sX,sY);
          CASE ch OF
            033c     : wnd.mode(wind,wnd.normal); RETURN FALSE
           |015c     : s_b(cview,sX,sY,top);
                       IF zm THEN un_zoom; cursor(sX,sY) END;
                       wnd.mode(wind,wnd.normal); kb:=FALSE; RETURN TRUE
           |'g','G'  : cursor(xo,yo); xo:=sX; yo:=sY; _grid; cursor(sX,sY)
           |'s','S'  : cursor(xo,yo); xo:=sX; yo:=sY; _step; cursor(sX,sY)
           |'c','C'  : cursor(xo,yo); xo:=sX; yo:=sY; _coord; cursor(sX,sY)
           |'r','R'  : cursor(xo,yo); xo:=sX; yo:=sY; _color; cursor(sX,sY)
           |'t','T'  : cursor(xo,yo); xo:=sX; yo:=sY; _ltyp;  cursor(sX,sY)
           |'l','L'  : cursor(xo,yo); xo:=sX; yo:=sY; _layer; cursor(sX,sY)
          ELSE
            IF numb(ch) THEN
              str.print(conf,'%c',ch);
              IF rd_point(prompt,conf,top) THEN
                wnd.mode(wind,wnd.normal); kb:=TRUE; RETURN TRUE
              END
            END;
            in_dec(ch,dx,dy); cursor(sX,sY)
          END
        END
      END
    END
  END
END read_size;

PROCEDURE _read(VAR c: CHAR);
VAR x,y: INTEGER;
      s: BITSET;
      S: INTEGER;
BEGIN
  LOOP
    IF key.ready()>0 THEN key.read(c); RETURN END;
    IF mou.ready()>0 THEN
      mou.read(x,y,s);
      IF s = state THEN c:=0c; RETURN  END;
      state:=s; S:=INTEGER(s);
      CASE S OF
         1: c:=015c; bpm.clear_mou; RETURN
        |2: c:=220c; bpm.clear_mou; RETURN
        |4: c:=033c; bpm.clear_mou; RETURN
      ELSE  c:=0c; RETURN END
    END
  END
END _read;

PROCEDURE make_str(f: tex.PFONT; VAR txt: def.Text);

  PROCEDURE slope(f: tex.PFONT; VAR x,y: INTEGER);
    VAR s,c,a,b: INTEGER;
  BEGIN
    s:=f^.ss; c:=f^.sc; a:=x; b:=y;
    x:= bmt.muldiv(a,c,1000)- bmt.muldiv(b,s,1000);
    y:= bmt.muldiv(a,s,1000)+ bmt.muldiv(b,c,1000)
  END slope;

PROCEDURE make_ch(VAR x,y: INTEGER; ch: CHAR);

  PROCEDURE compute(VAR a,b,a1,b1: INTEGER);
  BEGIN
    WITH f^ DO
      a:= bmt.muldiv(a,W,w); a1:= bmt.muldiv(a1,W,w);
      b:= bmt.muldiv(b,H,h); b1:= bmt.muldiv(b1,H,h);
      a :=a + bmt.muldiv(b ,f^.ic,1000); b := bmt.muldiv(b ,f^.is,1000);
      a1:=a1+ bmt.muldiv(b1,f^.ic,1000); b1:= bmt.muldiv(b1,f^.is,1000);
      slope(f,a,b); slope(f,a1,b1)
    END
  END compute;

VAR j,k,n,w,a,b,l,h,A,B: INTEGER;
              pic: DYNARR OF def.VECTOR;

BEGIN
  NEW(pic,HIGH(f^.ptr[ch])+1);
  IF NOT done THEN RETURN END;
  k:=0;
  l:= bmt.muldiv(f^.propW[ch],f^.W,f^.w); h:=0;
  slope(f,l,h);
  FOR j:=0 TO HIGH(f^.ptr[ch]) DO
    w:=f^.ptr[ch][j];
    a:=INTEGER(BITSET(w)*{0..7}); w:=w >> 8;
    b:=INTEGER(BITSET(w)*{0..7}); w:=w >> 8;
    A:=INTEGER(BITSET(w)*{0..7}); w:=w >> 8;
    B:=INTEGER(BITSET(w)*{0..7});
    compute(a,b,A,B);
    s_b(cview,a+x,b+y,pic[k,0]);
    s_b(cview,A+x,B+y,pic[k,1]); INC(k)
  END;
  x:=x+l; y:=y+h;
  WITH txt DO
    n:= HIGH(pict);
    RESIZE(pict,n+HIGH(pic)+2);
    IF NOT done THEN DISPOSE(pic); DISPOSE(pict); RETURN END;
    FOR j:= n+1 TO HIGH(pict) DO pict[j]:= pic[j-n-1] END
  END;
  DISPOSE(pic)
END make_ch;

  VAR     i,dx,dy,x,y: INTEGER;
                    s: REAL;
BEGIN
  NEW(txt.pict);
  IF NOT done THEN RETURN END;
  IF str.len(txt.stxt) <=0 THEN RETURN END;
  WITH cview^ DO
    s:=scale; scale:=500.;
    tex.font_size(f,mth.round(txt.w*scale),mth.round(txt.h*scale));
    x:=mth.round(mth.sin(txt.i)*100.);
    y:=mth.round(mth.cos(txt.i)*100.);
    tex.font_ital(f,x,y);
    x:= mth.round(mth.cos(txt.a)*100.);
    y:= mth.round(mth.sin(txt.a)*100.);
    tex.font_slop(f,x,y);
    b_s(cview,txt.xyz,x,y);
    i:=0;
    WHILE (i<=HIGH(txt.stxt)) & (txt.stxt[i]#0c) DO
      make_ch(x,y,txt.stxt[i]); INC(i)
    END;
    scale:= s
  END
END make_str;

VAR  txt_h,txt_w,txt_a,txt_it,scl: REAL;

PROCEDURE read_text(VAR top: def.VERTEX; VAR TEXT: def.Text): BOOLEAN;
  VAR sx,sy: DYNARR OF INTEGER;
      x,y,h: INTEGER;
          T: wnd.TOOL;
          s: STRING;
          c: CHAR;

PROCEDURE curs;
  VAR cx,cy: INTEGER;
BEGIN
  cx:= x; cy:= y;
  tex.write_ch(cview^.wind,T,tex.font,cx,cy,177c)
END curs;

PROCEDURE del_char;
BEGIN
  IF h<0 THEN RETURN END;
  curs;
  x:= sx[h]; y:=sy[h];
  tex.write_ch(cview^.wind,T,pfont,sx[h],sy[h],s[h]);
  RESIZE(s,h);
  IF NOT done THEN  RETURN  END;
  RESIZE(sy,h);
  IF NOT done THEN  RETURN  END;
  RESIZE(sx,h);
  IF NOT done THEN  RETURN  END;
  h:= HIGH(s);
  curs
END del_char;

PROCEDURE new_char(c: CHAR);
BEGIN
  curs;
  RESIZE(s,HIGH(s)+2);
  IF NOT done THEN RETURN END;
  h:= HIGH(s);
  RESIZE(sx,h+1);
  IF NOT done THEN RETURN END;
  RESIZE(sy,h+1);
  IF NOT done THEN RETURN END;
  sx[h]:= x; sy[h]:= y;
  s[h]:=c;
  tex.write_ch(cview^.wind,T,pfont,x,y,c);
  curs
END new_char;

PROCEDURE new_str;
  VAR k: INTEGER;
BEGIN
  curs;
  IF HIGH(s) >= 0 THEN
    WITH TEXT DO
      xyz:= top;
      w:= txt_w;
      h:= txt_h;
      i:= txt_it;
      a:= txt_a;
      RESIZE(stxt,HIGH(s)+1);
      IF NOT done THEN RETURN END;
      stxt:= s
    END;
    tex.write_str(cview^.wind,T,pfont,sx[0],sy[0],s);
    make_str(pfont,TEXT);
    IF NOT done THEN RETURN END;
  END;
  WITH cview^ DO
    top[Yc]:= top[Yc]-txt_h*mth.cos(txt_a)*1.1;
    top[Xc]:= top[Xc]+txt_h*mth.sin(txt_a)*1.1;
    b_s(cview,top,x,y);
    cntx^.sX:= x;
    cntx^.sY:= y
  END;
  DISPOSE(sx);
  DISPOSE(sy);
  DISPOSE(s);
  h:=0
END new_str;

PROCEDURE ital_txt;
  VAR top1,top2: def.VERTEX;
           done: BOOLEAN;
            x,y: INTEGER;
            str: ARRAY [0..31] OF CHAR;
BEGIN
  b_s(cview,top,x_old,y_old);
  WITH cview^ DO
    top1[Xc]:= top[Xc];
    top1[Yc]:= top[Yc]+20.;
    top2[Zc]:= Max+ 12.; --for mark
    cntx^.sY:= y_old + mth.round(3. * txt_h * mth.cos(txt_it));
    cntx^.sX:= x_old + mth.round(3. * txt_h * mth.sin(txt_it));
    IF ENG THEN str:='Italic char (deg.):'
    ELSE        str:='Наклон символа (град):' END;
    IF read_point(str,cross_line,null_monitor,top2) THEN
      IF top2[Zc] = Max+12. THEN
        txt_it:=  mth.dtor(top2[Xc])
      ELSE
        txt_it:= bmt.angle_1(top,top2,top1);
        IF top2[Xc] < top[Xc] THEN txt_it:= -txt_it END
      END;
      y:=  TRUNC(mth.cos(txt_it)*100.);
      x:=  TRUNC(mth.sin(txt_it)*100.)
    END;
    tex.font_ital(pfont,x,y);
    tex.font_ital(tex.font,x,y)
  END
END ital_txt;

PROCEDURE turn_txt;
  VAR top1,top2: def.VERTEX;
           done: BOOLEAN;
            x,y: INTEGER;
            str: ARRAY [0..31] OF CHAR;
BEGIN
  b_s(cview,top,x_old,y_old);
  IF ENG THEN str:='Angle string (deg):'
  ELSE        str:='Наклон сторки (град):' END;
  WITH cview^ DO
    top1[Xc]:= top[Xc]+20.;
    top1[Yc]:= top[Yc];
    top2[Zc]:= Max+12.; --for mark
    cntx^.sX:= x_old + TRUNC(20. * mth.cos(txt_a));
    cntx^.sY:= y_old + TRUNC(20. * mth.sin(txt_a));
    IF read_point(str,cross_line,null_monitor,top2) THEN
      IF top2[Zc] = Max+12. THEN txt_a:= mth.dtor(top2[Xc])
      ELSE
        txt_a:= bmt.angle_0(top,top2);
        IF top2[Yc] < top[Yc] THEN txt_a:= mth.pi*2.-txt_a END
      END;
      x:= TRUNC(mth.cos(txt_a) * 100.);
      y:= TRUNC(mth.sin(txt_a) * 100.)
    END;
    tex.font_slop(pfont,x,y);
    tex.font_slop(tex.font,x,y)
  END
END turn_txt;

PROCEDURE size_txt;
  VAR t: def.VERTEX;
   done: BOOLEAN;
    str: ARRAY [0..31] OF CHAR;
     kb: BOOLEAN;
BEGIN
  b_s(cview,top,x_old,y_old);
  IF ENG THEN str:='Size char <w h>:'
  ELSE        str:='Размер букв <w,h>:' END;
  WITH cview^ DO
    cntx^.sY:= y_old + mth.round(txt_h* scale);
    cntx^.sX:= x_old + mth.round(txt_w* scale);
    IF NOT read_size(str,cross_box,kb,t) THEN RETURN END;
    IF kb THEN
      IF t[Xc]>0. THEN txt_w:=t[Xc] END;
      IF t[Yc]>0. THEN txt_h:=t[Yc] END
    ELSE
      txt_w:= ABS(t[Xc]-top[Xc]);  txt_h:= ABS(t[Yc]-top[Yc])
    END;
    tex.font_size(pfont,mth.round(txt_w*scale),mth.round(txt_h*scale));
    tex.font_size(tex.font,mth.round(txt_w*scale),mth.round(txt_h*scale))
  END
END size_txt;

PROCEDURE font(X,Y: INTEGER);
  VAR name: ARRAY [0..255] OF CHAR;
      dbx: bpm.DIREX;
      x,y: INTEGER;

PROCEDURE ext(s: ARRAY OF CHAR): BOOLEAN;
  VAR h: INTEGER;
      d: BOOLEAN;
BEGIN
  h:=str.len(s)-4; IF h<0 THEN RETURN FALSE END;
  str.scan(s,h,'.plf',d); RETURN d
END ext;

BEGIN
  bpm.dnew(dbx,X,Y-90,100,90,{},'.','*.plf',bpm.standard);
  LOOP
    bpm.dselect(dbx);
    IF NOT bpm.dselected(dbx) THEN bpm.ddispose(dbx); EXIT
    ELSE
      bpm.dfullname(dbx,name);
      IF NOT ext(name) THEN
        IF ENG THEN bpm.message(140,y-20,'bad name %s',name)
        ELSE     bpm.message(140,y-20,'неправильное имя  %s',name) END
      ELSE
        tex.load(pfont,name);
        IF NOT tex.done THEN
          IF ENG THEN
             bpm.message(140,y-20,'error in file %s',name)
           ELSE
             bpm.message(140,y-20,'ошибка при чтении шрифта %s',name)
           END;
          RETURN
        END;
        bpm.ddispose(dbx);
        WITH cview^ DO
          tex.font_size(pfont,mth.round(txt_w*scale),mth.round(txt_h*scale));
          x:= TRUNC(mth.cos(txt_a) * 100.);
          y:= TRUNC(mth.sin(txt_a) * 100.);
          tex.font_slop(pfont,x,y);
          y:=  TRUNC(mth.cos(txt_it)*100.);
          x:=  TRUNC(mth.sin(txt_it)*100.);
          tex.font_ital(pfont,x,y);
          RETURN
        END
      END
    END
  END
END font;

PROCEDURE monitor;
  VAR  ttx: bpm.TABLET;
        hf: INTEGER;
BEGIN
  hf:=bpm.font^.H+5;
  IF ENG THEN
    bpm.tnew(ttx,1,4,0,SH-hf*5,90,hf,bpm.xsel,' Text');
    bpm.tprint(ttx,0,"Size  ");  bpm.thotkey(ttx,0,"s",TRUE);
    bpm.tprint(ttx,1,"Italic");  bpm.thotkey(ttx,1,"i",TRUE);
    bpm.tprint(ttx,2,"Angle ");  bpm.thotkey(ttx,2,"a",TRUE);
    bpm.tprint(ttx,3,"Font  ");  bpm.thotkey(ttx,3,"f",TRUE);
  ELSE
    bpm.tnew(ttx,1,4,0,SH-hf*5,120,hf,bpm.xsel,' Текст');
    bpm.tprint(ttx,0,"Размер      ");  bpm.thotkey(ttx,0,"р",TRUE);
    bpm.tprint(ttx,1,"наклон Букв ");  bpm.thotkey(ttx,1,"б",TRUE);
    bpm.tprint(ttx,2,"наклон Строк");  bpm.thotkey(ttx,2,"с",TRUE);
    bpm.tprint(ttx,3,"Шрифт       ");  bpm.thotkey(ttx,3,"ш",TRUE);
  END;
  bpm.topen(ttx);
  LOOP
    bpm.tselect(ttx);
    IF NOT bpm.tselected(ttx) THEN bpm.tdispose(ttx); RETURN END;
    CASE bpm.talt(ttx) OF
       0: bpm.tclose(ttx);
          size_txt; bpm.topen(ttx); bpm.tunselect(ttx)
      |1: bpm.tclose(ttx);
          ital_txt; bpm.topen(ttx); bpm.tunselect(ttx)
      |2: bpm.tclose(ttx);
          turn_txt; bpm.topen(ttx); bpm.tunselect(ttx)
      |3: font(10,SH-10); bpm.tunselect(ttx)
    ELSE END
  END
END monitor;

PROCEDURE exit;
BEGIN
  IF (HIGH(sx)<0) OR (HIGH(sy)<0) THEN RETURN END;
  tex.write_str(cview^.wind,T,pfont,sx[0],sy[0],s);
  curs;
  DISPOSE(sx); DISPOSE(sy); DISPOSE(s)
END exit;

PROCEDURE restore_font;
  VAR  x,y: INTEGER;
BEGIN
  WITH cview^ DO
    y:=mth.round(mth.cos(txt_it)*1000.);
    x:=mth.round(mth.sin(txt_it)*1000.);
    tex.font_ital(pfont,x,y);
    tex.font_ital(tex.font,x,y);
    x:= mth.round(mth.cos(txt_a)*1000.);
    y:= mth.round(mth.sin(txt_a)*1000.);
    tex.font_slop(tex.font,x,y);
    tex.font_slop(pfont,x,y);
    tex.font_size(tex.font,mth.round(txt_w*scale),mth.round(txt_h*scale));
    tex.font_size(pfont,mth.round(txt_w*scale),mth.round(txt_h*scale))
  END
END restore_font;

PROCEDURE _dispose;
BEGIN
  IF HIGH(s)>0  THEN  DISPOSE(s)  END;
  IF HIGH(sx)>0 THEN  DISPOSE(sx) END;
  IF HIGH(sy)>0 THEN  DISPOSE(sy) END;
  WITH TEXT DO
--    IF HIGH(pict)>0 THEN DISPOSE(pict) END;
--    IF HIGH(stxt)>0 THEN DISPOSE(stxt) END
  END
END _dispose;

BEGIN
  restore_font;
  T:= cview^.tool;
  T.mode:= wnd.xor;
  T.mask:= {3};
  T.color:= BITSET(rubb);
  wnd.mode(cview^.wind,wnd.scr);
  NEW(s);
  IF NOT done THEN _dispose; RETURN FALSE END;
  NEW(sx);
  IF NOT done THEN _dispose; RETURN FALSE END;
  NEW(sy);
  IF NOT done THEN _dispose; RETURN FALSE END;
  h:=-1;
  NEW(TEXT.pict);
  IF NOT done THEN _dispose; RETURN FALSE END;
  NEW(TEXT.stxt);
  IF NOT done THEN _dispose; RETURN FALSE END;
  b_s(cview,top,x,y);
  curs;
  LOOP
    _read(c);
    CASE c OF
       key.back: del_char
      |first   : new_str; IF NOT done THEN _dispose; RETURN FALSE END;
                 DISPOSE(s); DISPOSE(sx); DISPOSE(sy);
                 RETURN TRUE
      |second  : IF h<1 THEN curs; monitor; curs END
      |therd   : exit; _dispose; RETURN FALSE
    ELSE
      IF char(c) THEN
        new_char(c); IF NOT done THEN _dispose; RETURN FALSE END
      END
    END
  END
END read_text;

------------------------ window manager ------------------------

CONST frame=2; title=13;

PROCEDURE show_name(v: VIEW);
BEGIN
  WITH v^ DO
    pwm.button(wind,0,pwm.luc,28,-12,wind^.w-30,11);
--    pwm.button(wind,0,pwm.luc,28,-12,bpm.blen(' %s ',model^.name),11);
    IF v= cview THEN
      pwm.buttoncolors(v^.wind,0,BITSET(white),BITSET(gray),BITSET(black))
    END;
    pwm.print(wind,0,bpm.font,' %s ',model^.name)
  END
END show_name;

PROCEDURE _buttons(v: VIEW);
BEGIN
  show_name(v);
  WITH v^ DO
    pwm.button(wind,1,pwm.ldc, 26,1,11,11);
    pwm.print(wind,1,pwm.ssfont,pwm.sszoomout);

    pwm.button(wind,2,pwm.ldc,  1,1,11,11);
    pwm.print(wind,2,bpm.font,'-');

    pwm.button(wind,3,pwm.ldc, 14,1,11,11);
    pwm.print(wind,3,bpm.font,'+');

    pwm.button(wind,4,pwm.rdc,-12,1,11,11);
    pwm.print(wind,4,pwm.ssfont,pwm.ssright);

    pwm.button(wind,5,pwm.rdc,-25,1,11,11);
    pwm.print(wind,5,pwm.ssfont,pwm.ssleft);

    pwm.button(wind,6,pwm.rdc,-38,1,11,11);
    pwm.print(wind,6,pwm.ssfont,pwm.ssup);

    pwm.button(wind,7,pwm.rdc,-51,1,11,11);
    pwm.print(wind,7,pwm.ssfont,pwm.ssdw);

    pwm.button(wind,8,pwm.luc,15,-12,11,11);   (*  icon view *)
    pwm.print(wind,8,pwm.ssfont,pwm.sszoomin)
  END
END _buttons;

VAR boom_view: DYNARR OF VIEW;

PROCEDURE wmonitor;
  VAR center: def.VERTEX;
       rw,rh: INTEGER;
        view: VIEW;

   PROCEDURE _icon;
     VAR old: wnd.WINDOW;
   BEGIN
     IF view= cview THEN  RETURN END;
     WITH view^ DO
       old:=wind;
       pwm.new(wind);
       IF NOT pwm.done THEN done:=pwm.done; error:= pwm.error; RETURN END;
       wnd.object(wind,view);
       pwm.disable(wind);
       wnd.resize(wind,2*title+bpm.blen(' %s ',model^.name)+8,title+2);
       IF NOT wnd.done THEN
         DISPOSE(wind); wind:=old;
         done:= wnd.done; error:= wnd.error; RETURN
       END;
       icon:= TRUE;
       wnd.inner(wind,wind^.w-1,0,1,0);
       pwm.button(wind,8,pwm.luc,15,-12,12,11);
       pwm.print(wind,8,pwm.ssfont,pwm.sszoomout);
       show_name(view);
       tool.clip.x:= old^.x;
       tool.clip.y:= old^.y;
       wnd.dispose(old);
       wnd.open(wind)
     END
   END _icon;

   PROCEDURE _unicon;
     VAR old: wnd.WINDOW;
   BEGIN
     WITH view^ DO
       old:=wind;
       pwm.new(wind);
       IF NOT pwm.done THEN done:=pwm.done; error:= pwm.error; RETURN END;
       wnd.object(wind,view);
       pwm.disable(wind);
       wnd.resize(wind,tool.clip.w+2*frame,tool.clip.h+2*title);
       IF NOT wnd.done THEN
         DISPOSE(wind); wind:=old;
         done:= wnd.done; error:= wnd.error; RETURN
       END;
       wnd.move(wind,tool.clip.x,tool.clip.y);
       wnd.inner(wind,frame,title,wind^.w-frame*2,wind^.h- 2* title);
       tool.clip:= wind^.inner.clip;
       wnd.dispose(old);
       icon:= FALSE;
       _buttons(view);
       vredraw(view);
       wnd.open(wind)
     END;
   END _unicon;

   PROCEDURE _center; BEGIN
   WITH view^.wind^.inner.clip DO s_b(view, w DIV 2,h DIV 2,center) END
   END _center;

   PROCEDURE new_corner;
   BEGIN
     WITH view^ DO
       X_lf:= center[Xc]-FLOAT(wind^.inner.clip.w DIV 2) /scale;
       Y_dw:= center[Yc]-FLOAT(wind^.inner.clip.h DIV 2) /scale;
     END
   END new_corner;

   PROCEDURE _zoom_in;
   BEGIN
     IF view^.scale>= view^.cntx^.max_scale THEN RETURN END;
     _center; view^.scale:= view^.scale*2.;
     new_corner; vredraw(view);
   END _zoom_in;

   PROCEDURE _zoom_out;
   BEGIN
     _center; view^.scale:= view^.scale/2.;
     new_corner; vredraw(view);
   END _zoom_out;

   PROCEDURE _cview;
     VAR new,old: VIEW;
   BEGIN
     IF pwm.active^.obj= cview THEN  RETURN END;
     new:= pwm.active^.obj;
     IF new^.icon THEN RETURN  END;
     old:=cview;
     cview:= new;
     show_name(old);
     show_name(cview)
   END _cview;

   PROCEDURE _boom;
   BEGIN
     RESIZE(boom_view,HIGH(boom_view)+2);
     IF NOT done THEN no_mem; RETURN END;
     boom_view[HIGH(boom_view)]:= pwm.active^.obj;
     WITH boom_view[HIGH(boom_view)]^ DO
       wnd.close(wind);
       wnd.resize(wind,SW+2*frame,SH+2*title);
       IF NOT wnd.done THEN no_mem;  wnd.open(wind); RETURN  END;
       tool.clip:= wind^.inner.clip;
       wnd.move(wind,-frame,-title);
       wnd.open(wind)
     END;
     vredraw(boom_view[HIGH(boom_view)])
   END _boom;

   PROCEDURE _vdispose;
     VAR i,n :INTEGER;
   BEGIN
     IF view= NIL THEN  bad_desc; RETURN END;
     IF view= cview THEN  RETURN END;
     IF HIGH(view^.model^.views) = 0 THEN
       IF ENG THEN bpm.message(240,180,'In model only one view!')
       ELSE        bpm.message(240,180,'В модели остался один вид!') END;
       RETURN
     END;
     WITH view^.model^ DO
        FOR i:=0 TO HIGH(views) DO
          IF views[i]=view THEN n:=i END
        END;
        FOR i:=n TO HIGH(views)-1 DO
          views[i]:= views[i+1]
        END;
        views[HIGH(views)]:= NIL;
        RESIZE(views,HIGH(views))
     END;
     dispose_view(view)
   END _vdispose;

BEGIN
  wnd.ontop(WCOORD);
  show_coord;
  view:= pwm.active^.obj;
  IF pwm.resized & NOT view^.icon THEN
    IF pwm.resizeH < 3*title THEN rh:=3*title ELSE rh:= pwm.resizeH END;
    IF pwm.resizeW < 7*title THEN rw:=7*title ELSE rw:= pwm.resizeW END;
    wnd.resize(view^.wind,rw,rh);
    IF NOT wnd.done THEN RETURN  END;
    view^.tool:= view^.wind^.inner;
    vredraw(view);
    show_name(view)
  ELSIF   pwm.moved THEN
    wnd.move(view^.wind,pwm.moveX,pwm.moveY)
  ELSIF  pwm.closed THEN
    _vdispose
  ELSE
    CASE pwm.abutton OF
      0:_cview
     |1:_boom
     |2:_zoom_out
     |3:_zoom_in
     |4: pan(view,_lf); shift:=FALSE
     |5: pan(view,_rg); shift:=FALSE
     |6: pan(view,_dw); shift:=FALSE
     |7: pan(view,_up); shift:=FALSE
     |8: IF view^.icon THEN _unicon ELSE _icon END
    ELSE  END;
    pwm.toggle(view^.wind,pwm.abutton,FALSE)
  END
END wmonitor;

PROCEDURE unzoom_views;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(boom_view) DO
    WITH boom_view[i]^ DO
      wnd.move(wind,0,-title*2);
    END;
    show_name(boom_view[i]);
    boom_view[i]:= NIL
  END;
  NEW(boom_view,0)
END unzoom_views;

PROCEDURE dispose_view(v: VIEW);
BEGIN
  WITH v^ DO
    DISPOSE(cntx);
    model:= NIL;
    wnd.dispose(wind);
  END;
  DISPOSE(v)
END dispose_view;

PROCEDURE new_view(vmod: VMODEL);
  VAR i: INTEGER;
BEGIN
  WITH vmod^ DO
    RESIZE(views,HIGH(views)+2);
    IF NOT  done THEN RETURN END;
    NEW(views[HIGH(views)]);
    IF NOT  done THEN RETURN END;
    WITH views[HIGH(views)]^ DO
      NEW(cntx);
      IF NOT  done THEN  RETURN END;
      NEW(model);
      IF NOT  done THEN  RETURN END;
      pwm.new(wind);
      IF NOT pwm.done THEN done:=pwm.done; error:= pwm.error; RETURN END;
      wnd.object(wind,views[HIGH(views)]);
      pwm.disable(wind);
      wnd.resize(wind,160,100);
      IF NOT wnd.done THEN
        DISPOSE(model); DISPOSE(cntx);
        wnd.dispose(wind); RESIZE(views,HIGH(views));
        done:= wnd.done; error:= wnd.error; RETURN
      END;
      wnd.inner(wind,frame,title,wind^.w-frame*2,wind^.h- 2* title);
      wnd.move(wind,50,240);
      wnd.open(wind);
      X_lf:=0.;
      Y_dw:=0.;
      scale:=1.;
      vmode:= FALSE;
      Zc:=def.Z;  Yc:=def.Y; Xc:=def.X;
      tool:=wind^.inner;
      icon:= FALSE;
      model:= vmod;
      WITH cntx^ DO
        gstep_x:=10.; gstep_y:=10.;
        cstep_x:=0.1; cstep_y:=0.1;
        c_X:=100.;  c_Y:=100.;  c_Z:=0.;
        sX:=100; sY:=100;
        max_scale:=100.;
        QUERY:= FALSE;
        GRID:=  TRUE;
        STEP:=  FALSE;
        PAN:=   TRUE;
        IF HIGH(mbody^) < 0 THEN
          DISPOSE(model); DISPOSE(cntx);
          wnd.dispose(wind); RESIZE(views,HIGH(views));
          bad_parm; RETURN
        END;
        NEW(mask,HIGH(mbody^)+1);
        IF NOT done THEN
          DISPOSE(model); DISPOSE(cntx);
          wnd.dispose(wind); RESIZE(views,HIGH(views)); RETURN
        END;
        FOR i:=0 TO HIGH(mask) DO mask[i]:=1 END;
        _buttons(views[HIGH(views)]);
        work:=0; mask[0]:=2;
        vredraw(views[HIGH(views)])
      END
    END
  END
END new_view;

PROCEDURE make_view(mod: VMODEL; VAR v: VIEW);
BEGIN
  new_view(mod);
  IF NOT done THEN no_mem; RETURN END;
  v:=mod^.views[HIGH(mod^.views)]
END make_view;

PROCEDURE create_view;
BEGIN
  new_view(cview^.model);
  IF NOT done THEN no_mem; RETURN END
END create_view;

PROCEDURE new_model(VAR newmodel: VMODEL);
BEGIN
  RESIZE(models,HIGH(models)+2); IF NOT done THEN no_mem; RETURN END;
  NEW(newmodel);
  models[HIGH(models)]:= newmodel;
  IF NOT done THEN  RETURN END;
  WITH newmodel^ DO
    name:='';
    NEW(mbody);
    IF NOT done THEN DISPOSE(newmodel);  RETURN END;
    NEW(mbody^);
    IF NOT done THEN
      DISPOSE(mbody); DISPOSE(newmodel);  RETURN
     END;
    obj.new_layer(mbody);
    IF NOT obj.done THEN
      DISPOSE(mbody^); DISPOSE(mbody); DISPOSE(newmodel); RETURN
    END;
    mbody^[0]^.name:='Default';
    NEW(views,0);
    IF NOT done THEN
      obj.rem_lay(newmodel^.mbody,0); DISPOSE(mbody^[0]);
      DISPOSE(mbody^); DISPOSE(mbody); DISPOSE(newmodel);
      RETURN
    END;
    new_view(newmodel);
    IF NOT done THEN
      DISPOSE(views);
      obj.rem_lay(newmodel^.mbody,0); DISPOSE(mbody^[0]);
      DISPOSE(mbody^); DISPOSE(mbody); DISPOSE(newmodel);
      RETURN
    END;
    area:= null_area
  END
END new_model;

PROCEDURE remove_model(mod: VMODEL);
  VAR i,n: INTEGER;
BEGIN
  IF mod= NIL THEN  RETURN END;
  WITH mod^ DO
    FOR i:=0 TO HIGH(views) DO
      IF views[i]=cview THEN RETURN END
    END;
    obj.rem_all(mbody);
    FOR i:=0 TO HIGH(views) DO dispose_view(views[i]) END;
    DISPOSE(views);
    DISPOSE(mbody)
  END;
  FOR i:=0 TO HIGH(models) DO
    IF models[i]=mod THEN n:=i END
  END;
  FOR i:=n TO HIGH(models)-1 DO models[i]:= models[i+1] END;
  DISPOSE(mod);
  RESIZE(models,HIGH(models))
END remove_model;

VAR  dfnt1,dfnt2: fnt.FONT;

PROCEDURE _desk(wn: wnd.WINDOW; x,y,w,h: INTEGER);
  CONST s1='BCAD';
        s2='ABCDEFGHIJKLMNOPQRSTUVWXYZ[\';

  VAR t: wnd.TOOL;
BEGIN
  t:= wn^.full;
  t.clip.x:=x;
  t.clip.y:=y;
  t.clip.w:=w;
  t.clip.h:=h;
  t.mode:=wnd.rep;
  t.color:= {2};
  wnd.fill(wn,t,wn^.full.clip,32,0AAAAAAAAh,055555555h);

  t.back := {};
  t.mode:=wnd.or;
  t.color:= BITSET(pale_blue);
  t.mask:=  {2}+BITSET(pale_blue);
  wnd.print(wn,t,15,340,bpm.font,'  base Computer Aided Design  Draft Editor  Version 1.02 %c 1991 W.Malukh',260c);
  wnd.write(wn,t,120,120,dfnt1,s1,0,4);
  wnd.write(wn,t,50,10,dfnt2,s2,0,28);
END _desk;
----------------------------- INIT -----------------------------

PROCEDURE ini_pal;
 VAR p: scr.PALETTE;
BEGIN
  NEW(p,16); ASSERT(done);
            (* normal colors *)
  WITH p[0]  DO r:=0; g:=0; b:=0 END;        (* black     *)
  WITH p[1]  DO r:=2; g:=0; b:=0 END;        (* red       *)
  WITH p[2]  DO r:=0; g:=1; b:=0 END;        (* green     *)
  WITH p[3]  DO r:=2; g:=2; b:=0 END;        (* yellow    *)
  WITH p[4]  DO r:=0; g:=0; b:=1 END;        (* blue      *)
  WITH p[5]  DO r:=1; g:=0; b:=1 END;        (* cyan      *)
  WITH p[6]  DO r:=0; g:=1; b:=1 END;        (* pale_blue *)
  WITH p[7]  DO r:=2; g:=2; b:=2 END;        (* white     *)
           (* light colors *)
  WITH p[8]  DO r:=1; g:=1; b:=1 END;        (* gray      *)
  WITH p[9]  DO r:=3; g:=0; b:=0 END;        (* red       *)
  WITH p[10] DO r:=0; g:=3; b:=0 END;        (* green     *)
  WITH p[11] DO r:=3; g:=3; b:=0 END;        (* yellow    *)
  WITH p[12] DO r:=0; g:=1; b:=3 END;        (* blue      *)
  WITH p[13] DO r:=3; g:=0; b:=3 END;        (* cyan      *)
  WITH p[14] DO r:=0; g:=3; b:=3 END;        (* pale_blue *)
  WITH p[15] DO r:=3; g:=3; b:=3 END;        (* white     *)
  scr.set_palette(p,0,16);
  IF NOT scr.done THEN HALT(scr.error) END;
  bpm.setplanes(BITSET(gray),BITSET(white),BITSET(white_l));
  DISPOSE(p)
END ini_pal;

PROCEDURE ini_wnd;
  VAR w,h: INTEGER;
BEGIN
  SH:=wnd.scrH; SW:=wnd.scrW;
  NEW(MRK); ASSERT(done);
  WITH MRK^ DO
    wnd.new(wind); ASSERT(wnd.done);
    wnd.mask(wind,{3},{});
    wnd.resize(wind,96,72); ASSERT(wnd.done); ASSERT(wnd.done);
    wnd.close(wind);
    wnd.mode(wind,wnd.img);
    tool:= wind^.inner;
    Xc:= def.X;
    Yc:= def.Y;
    Zc:= def.Z;
    X_lf:=0.;
    Y_dw:=0.;
    scale:=1.;
    model:= NIL;
    cntx := NIL;
  END;

  NEW(ZM); ASSERT(done);
  WITH ZM^ DO
    Xc:= def.X;
    Yc:= def.Y;
    Zc:= def.Z;
    X_lf:=0.;
    Y_dw:=0.;
    scale:=1.;
    model:= NIL;
    cntx := NIL;
    icon:= FALSE;
    pwm.new(wind);    ASSERT(pwm.done);
    wnd.close(wind);
    wnd.resize(wind,104,104); ASSERT(wnd.done);
    wnd.inner(wind,2,2,100,100);
    tool:=wind^.inner
  END;

  wnd.new(WCOORD);
  w:=wnd.lenght(bpm.font,'-16000.00 -16000.00')+6;
  h:=bpm.font^.H+6;
  wnd.resize(WCOORD,w,h);
  wnd.move(WCOORD,480-w,360-h);
  bpm.block(WCOORD,WCOORD^.full.clip,TRUE,FALSE);
  TCOORD:= WCOORD^.inner;
  wnd.mode(WCOORD,wnd.scr);
  WITH  TCOORD DO
    clip.x:=2;
    clip.y:=2;
    clip.w:=w-4;
    clip.h:=w-4;
    color:= bpm.black ;
    back := bpm.normal;
    mode:=  wnd.rep;
    mask:=  bpm.normal
  END
END ini_wnd;

PROCEDURE init_model;
  VAR new: VMODEL;
BEGIN
  NEW(models,0);   ASSERT(done);
  new_model(new); ASSERT(done);
  cview:= new^.views[0];
  cview^.model^.name:='bcad.bdf';
  show_name(cview)
END init_model;

PROCEDURE dummy_monitor(): BOOLEAN; BEGIN RETURN FALSE END dummy_monitor;

PROCEDURE init;
  VAR  top: def.VERTEX;
         i: INTEGER;
BEGIN
  error:=err.ok; done:=TRUE;
  Max:= 16000.;
  null_monitor:= dummy_monitor;
  WITH null_area DO
    x:= Max; X:= -Max;
    y:= Max; Y:= -Max;
    z:= Max; Z:= -Max
  END;
  x_old:=0;   y_old:=0; x_old1:=0;  y_old1:=0;
  txt_w:=5.; txt_h:=8.; txt_a:=0.; txt_it:=0.;
  bckg:= black;
  mark:= gray;
  rubb:= gray;
  fnt.read(dfnt1,'bcDesk1.fnt'); ASSERT(fnt.done);
  fnt.read(dfnt2,'bcDesk2.fnt'); ASSERT(fnt.done);
  fnt.unpack(dfnt1);             ASSERT(fnt.done);
  fnt.unpack(dfnt2);             ASSERT(fnt.done);
  wnd.painter(wnd.desktop,_desk);
  wnd.boarder(wnd.desktop,_desk);
  wnd.refresh(wnd.desktop);

  tex.load(pfont,'bcMain.plf');     ASSERT(tex.done);
  init_model;
  NEW(boom_view,0); ASSERT(done);
  WITH cview^ DO
    tex.font_size(   pfont,mth.round(txt_w * scale),mth.round(txt_h * scale));
    tex.font_size(tex.font,mth.round(txt_w * scale),mth.round(txt_h * scale));
    WITH cntx^ DO xo:=sX; yo:=sY END
  END;
  show_coord;
  state:={};
  shift:=FALSE;
  show_grid(cview);
  vredraw(cview);
  bpm.clear_mou
END init;

BEGIN
  error:=err.ok; done:=TRUE;
  ini_pal;
  ini_wnd;
  init
END bcBase.
