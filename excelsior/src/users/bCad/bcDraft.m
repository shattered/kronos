IMPLEMENTATION MODULE bcDraft; (*$X+ brd 10-Jan-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  key: Keyboard;
IMPORT  mou: CPD;
IMPORT  str: Strings;
IMPORT  err: defErrors;
IMPORT  mth: realMath;
IMPORT  bas: bcBase;
IMPORT  def: bcDef;
IMPORT  obj: bcObj;
IMPORT  bmt: bcMath;
IMPORT  bpm: bcPM;
IMPORT  wnd: pmWnd;
IMPORT  mem: Heap;

IMPORT  tty: Terminal;

---------------------------- Memory ----------------------------

PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;

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

----------------------------- DRAW -----------------------------

PROCEDURE control_line(): BOOLEAN;
BEGIN
  RETURN FALSE
END control_line;


PROCEDURE make_line(pattern: PATTERN; VAR lin: def.Line);
  VAR x,y,sin,cos,l,dx,dy: REAL;
                    n,i,j: INTEGER;
BEGIN
  WITH bas.cview^ DO
    WITH lin DO
      IF HIGH(pattern)<0 THEN NEW(pict); RETURN  END;
      dx:=(XYZ[Xc]-xyz[Xc]);
      dy:=(XYZ[Yc]-xyz[Yc]);
      IF (dx = 0.) & (dy = 0.) THEN NEW(pict); RETURN END;
      l:=mth.sqrt(dx*dx+dy*dy);
      sin:= dy/l;
      cos:= dx/l;
      dx:= dx/l * pattern[0];
      dy:= dy/l * pattern[0];
      n:= TRUNC(l/pattern[0]) * HIGH(pattern) DIV 2 + 1;
      NEW(pict,n);
      x:= xyz[Xc];
      y:= xyz[Yc];
      i:= 0;
      LOOP
        FOR j:=1 TO HIGH(pattern) BY 2 DO
          pict[i,0][Xc]:= x; x:= x+pattern[j]*cos;
          pict[i,0][Yc]:= y; y:= y+pattern[j]*sin;

          pict[i,1][Xc]:= x; x:= x+pattern[j+1]*cos;
          pict[i,1][Yc]:= y; y:= y+pattern[j+1]*sin;
          INC(i);
          IF i > HIGH(pict) THEN pict[i-1,1]:= XYZ; RETURN END
        END
      END
    END
  END
END make_line;

PROCEDURE line;
  VAR new: def.Line;
      clp: obj.Clip;
    s1,s2: ARRAY [0..31] OF CHAR;

BEGIN
  IF bas.ENG THEN s1:='First point:';  s2:='Second point:'
  ELSE        s1:='Первая точка:'; s2:='Вторая точка:' END;
  WITH new DO
    LOOP
      IF bas.read_point(s1,bas.cross,control_line,xyz) THEN
        bas.b_s(bas.cview,xyz,bas.x_old,bas.y_old);
        IF bas.read_point(s2,bas.cross_line,bas.null_monitor,XYZ) THEN
          make_line(patts[c_ltype],new);
          color:= c_color;
          ltyp := c_ltype;
          obj.new_line(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
          bas.extr_lin(bas.cview^.model,new);
          bas.show_line(bas.cview^.model,new)
        ELSE RETURN END
      ELSE RETURN END
    END
  END
END line;

PROCEDURE make_vpatt(v: def.POLYLINE; pattern: PATTERN; VAR pict: def.VPICTURE);
BEGIN
END make_vpatt;

PROCEDURE make_patt(v: def.POLYLINE; pattern: PATTERN; VAR pict: def.PICTURE);
 VAR lp,ld,sn,cs: REAL;
           i,k,m: INTEGER;
             pln: def.POLYLINE;
               t: def.VERTEX;
BEGIN
  NEW(pict);
  NEW(pln,1); pln[0]:= v[0];
  i:= 1; k:= 1; m:= 0;
  ld:= bmt.dist1(v[0],v[1]);
  WITH bas.cview^ DO
    IF ld=0. THEN
      sn:=1.; cs:=0.
    ELSE
      sn:= (v[i,Yc]- v[i-1,Yc])/ ld;
      cs:= (v[i,Xc]- v[i-1,Xc])/ ld;
    END;
    lp:= pattern[k];
    LOOP
      REPEAT
        IF ld <= lp THEN
          REPEAT
            RESIZE(pln,m+2); INC(m);
            pln[m]:= v[i];   INC(i);
            lp:= lp-ld;
            IF i > HIGH(v) THEN
              RESIZE(pict,HIGH(pict)+2);
              NEW(pict[HIGH(pict)],HIGH(pln)+1);
              pict[HIGH(pict)]:= pln;
              DISPOSE(pln); EXIT
            END;
            ld:= bmt.dist1(v[i],v[i-1]);
           IF ld=0. THEN
               sn:=1.; cs:=0.
            ELSE
              sn:= (v[i,Yc]- v[i-1,Yc])/ ld;
              cs:= (v[i,Xc]- v[i-1,Xc])/ ld
            END
          UNTIL ld > lp
        ELSE
          RESIZE(pln,m+2);   INC(m);
          pln[m,Xc]:= pln[m-1,Xc]+ lp*cs;
          pln[m,Yc]:= pln[m-1,Yc]+ lp*sn;
          ld:= bmt.dist1(v[i],pln[m]);
          t:= pln[m];
          lp:= 0.
        END
      UNTIL lp<=0.;
      RESIZE(pict,HIGH(pict)+2);
      NEW(pict[HIGH(pict)],HIGH(pln)+1);
      pict[HIGH(pict)]:= pln;
      k:= (k+1) MOD (HIGH(pattern)+1);
      IF k=0 THEN k:=1 END;
      lp:= pattern[k];
      RESIZE(pln,1);   m:=0;
      pln[0]:= t;
      IF NOT ODD(k) THEN
        REPEAT
          IF ld <= lp THEN
            REPEAT
              pln[0]:= v[i]; INC(i);
              lp:= lp-ld;
              IF i > HIGH(v) THEN
                RESIZE(pict,HIGH(pict)+2);
                RESIZE(pln,2);
                pln[1]:= v[HIGH(v)];
                NEW(pict[HIGH(pict)],2);
                pict[HIGH(pict)]:= pln;
                DISPOSE(pln); EXIT
              END;
              ld:= bmt.dist1(v[i],v[i-1]);
              IF ld=0. THEN
                 sn:=1.; cs:=0.
              ELSE
                sn:= (v[i,Yc]- v[i-1,Yc])/ ld;
                cs:= (v[i,Xc]- v[i-1,Xc])/ ld
              END
            UNTIL ld > lp
          ELSE
            pln[0,Xc]:= pln[0,Xc]+ lp*cs;
            pln[0,Yc]:= pln[0,Yc]+ lp*sn;
            ld:= bmt.dist1(v[i],pln[0]);
            lp:= 0.
          END
        UNTIL lp<=0.;
        k:= (k+1) MOD (HIGH(pattern)+1);
        IF k=0 THEN k:=1 END;
        lp:= pattern[k]
      END
    END
  END
END make_patt;

PROCEDURE set_cir(): BOOLEAN;
  VAR  old,i: INTEGER;
        CTYP: bpm.TABLET;
          wn: wnd.WINDOW;
BEGIN
  bpm.tnew(CTYP,6,1,50,50,30,30,bpm.xsel,' Type circle');
  FOR i:=0 TO 5 DO
    bpm.tsprint(CTYP,i,'%c',CHAR(i+24));
    bpm.tdisable(CTYP,i)
  END;
  bpm.tundisable(CTYP,0);
  bpm.tchoose(CTYP,cir_type); old:=cir_type;
  bpm.twindow(CTYP,wn);
  WITH bas.cview^ DO
   bpm.tmove(CTYP,cntx^.sX-wn^.w DIV 2+ wind^.x,cntx^.sY- wn^.h DIV 2+wind^.y);
  END;
  bpm.topen(CTYP);
  bpm.tselect(CTYP);
  IF bpm.tselected(CTYP) THEN cir_type:= bpm.talt(CTYP) END;
  bpm.tdispose(CTYP);
  RETURN cir_type#old
END set_cir;

PROCEDURE circle;
  VAR new: def.Circ;
      clp: obj.Clip;

PROCEDURE make_cir;
  VAR x,y,da,a: REAL;
         i,j,n: INTEGER;
             v: def.POLYLINE;
BEGIN
  WITH bas.cview^ DO
    WITH new DO
      IF HIGH(patts[c_ltype])<0 THEN NEW(pict); RETURN END;
      x:=centr[Xc];
      y:=centr[Yc];
      n:= TRUNC(mth.pi * r/step_p);
      da:= mth.pi/FLOAT(n);
      n:= mth.round(mth.pi/da);
      a:= 0.;
      NEW(v,n*2);
      FOR i:=0 TO n DIV 2 DO
        v[i,Xc]:= x+r*mth.cos(a);
        v[i,Yc]:= y+r*mth.sin(a);
        a:=a+da
      END;
      FOR i:=n DIV 2 +1 TO n DO
        j:= n-i;
        v[i,Yc]:= v[j,Yc];
        v[i,Xc]:= 2.*x- v[j,Xc]
      END;
      FOR i:=n+1 TO HIGH(v)-1 DO
        j:= 2*n-i;
        v[i,Xc]:=v[j,Xc];
        v[i,Yc]:= 2.*y - v[j,Yc]
      END;
      v[HIGH(v)]:=v[0];
      make_patt(v,patts[c_ltype],pict);
      DISPOSE(v)
    END
  END
END make_cir;

VAR s1,s2,s3 : ARRAY [0..31] OF CHAR;
(*
PROCEDURE read_cir;
BEGIN
  CASE cir_type OF
  ELSE END
END read_cir;
*)
BEGIN
  IF bas.ENG THEN s1:='First point:'; s2:='Radius:'
  ELSE    s1:='Первая точка:'; s2:='Радиус:'    END;
  WITH new DO
    LOOP
      IF bas.read_point(s1,bas.cross,set_cir,centr) THEN
        bas.b_s(bas.cview,centr,bas.x_old,bas.y_old);
        IF bas.read_long(centr,s2,bas.long,bas.cross_cir,bas.null_monitor,r)
        THEN
          make_cir;
          color:= c_color;
          ltyp := c_ltype;
          obj.new_circ(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
          bas.extr_cir(bas.cview^.model,new);
          bas.show_cir(bas.cview^.model,new)
        ELSE RETURN END
      ELSE RETURN  END
    END
  END
END circle;

PROCEDURE arca;
  VAR new: def.Arc;
      clp: obj.Clip;
      top: def.VERTEX;
        R: REAL;

PROCEDURE make_arc;
  VAR a,a1,a2,a3,da: REAL;
                i,n: INTEGER;
                  v: def.POLYLINE;
                  b: BOOLEAN;  --tmp
BEGIN
  WITH bas.cview^ DO
    WITH new DO
      b:=bmt.calc_arc(top[Xc],top[Yc],R,xyz1,xyz2,xyz3);
      a1:= bmt.angle_0(top,xyz1);
      a2:= bmt.angle_0(top,xyz2);
      a3:= bmt.angle_0(top,xyz3);
      IF a1 > a3 THEN a:=a3; a3:=a1; a1:=a END;
      IF (a2 < a3) & (a2 > a1) THEN
        a:= a1; n:= TRUNC((a3-a1)*R/step_p);
        da:= (a3-a1)/FLOAT(n)
      ELSE
        a:= a3; n:= TRUNC((2.*mth.pi-a3+a1)*R/step_p);
        da:= (2.*mth.pi-a3+a1)/FLOAT(n)
      END;
      NEW(v,n+1);
      FOR i:=0 TO n DO
        v[i,Xc]:= top[Xc]+R*mth.cos(a);
        v[i,Yc]:= top[Yc]+R*mth.sin(a);
        a:=a+da
      END;
      IF HIGH(patts[c_ltype]) < 0 THEN
        NEW(pict,1); NEW(pict[0],HIGH(v)+1);
        pict[0]:=v
      ELSE make_patt(v,patts[c_ltype],pict) END;
      DISPOSE(v)
    END
  END
END make_arc;

VAR s1,s2,s3: ARRAY [0..31] OF CHAR;

BEGIN
  IF bas.ENG THEN s1:='Begin arca:'; s2:='End of arca'; s3:='Thread point:'
  ELSE  s1:='Начало дуги:'; s2:='Конец дуги:'; s3:='третья точка:' END;
  WITH new DO
    LOOP
      IF bas.read_point(s1,bas.cross,bas.null_monitor,xyz1) THEN
        bas.b_s(bas.cview,xyz1,bas.x_old,bas.y_old);
        IF bas.read_point(s2,bas.cross_line,bas.null_monitor,xyz3) THEN
          bas.b_s(bas.cview,xyz3,bas.x_old1,bas.y_old1);
          IF bas.read_point(s3,bas.cross_arc,bas.null_monitor,xyz2) THEN
            make_arc;
            color:= c_color;
            ltyp := c_ltype;
            obj.new_arc(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
            bas.extr_arc(bas.cview^.model,new);
            bas.show_arc(bas.cview^.model,new)
          ELSE RETURN END
        ELSE RETURN END
      ELSE RETURN  END
    END
  END
END arca;

PROCEDURE ellips;
BEGIN
END ellips;

PROCEDURE pline;
  VAR  new: def.Pline;
       clp: obj.Clip;
       t,T: def.VERTEX;
          i: INTEGER;

PROCEDURE make_pline;
BEGIN
  IF HIGH(patts[c_ltype])<0 THEN NEW(new.pict)
  ELSE make_patt(new.body,patts[c_ltype],new.pict) END
END make_pline;

VAR  s1,s2: ARRAY [0..31] OF CHAR;
BEGIN
  IF bas.ENG THEN  s1:='First point:'; s2:='Next point:'
  ELSE   s1:='Первая точка:'; s2:='Следуюшая точка:' END;
  i:=1;
  WITH new DO
    NEW(body,1);
    LOOP
      IF bas.read_point(s1,bas.cross,bas.null_monitor,body[0]) THEN
        LOOP
          bas.b_s(bas.cview,body[i-1],bas.x_old,bas.y_old);
          RESIZE(body,i+1);
          IF bas.read_point(s2,bas.cross_line,bas.null_monitor,body[i]) THEN
            WITH bas.cview^ DO
              IF  (body[i,Xc] = body[i-1,Xc]) &
                  (body[i,Yc] = body[i-1,Yc]) &
                  (body[i,Zc] = body[i-1,Zc])
              THEN
                RESIZE(body,HIGH(body)); DEC(i)
              ELSE
                bas.xshow_vector(bas.cview,body[i-1],body[i],bas.rubb)
              END
            END
          ELSIF HIGH(body)>1 THEN
            RESIZE(body,i);
            FOR i:=1 TO HIGH(body) DO
              bas.xshow_vector(bas.cview,body[i],body[i-1],bas.bckg)
            END;
            make_pline;
            ltyp:=  c_ltype;
            color:= c_color;
            type:= 0;
            obj.new_pline(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
            bas.extr_pln(bas.cview^.model,new);
            bas.show_pline(bas.cview^.model,new);
            DISPOSE(body);
            DISPOSE(pict);
            RETURN
          ELSE DISPOSE(body); RETURN END;
          INC(i)
        END
      ELSE DISPOSE(body); RETURN  END
    END
  END
END pline;

PROCEDURE box;
  VAR new: def.Pline;
      clp: obj.Clip;
        i: INTEGER;

PROCEDURE make_pline;
BEGIN
  IF HIGH(patts[c_ltype]) <0 THEN NEW(new.pict) ELSE
  make_patt(new.body,patts[c_ltype],new.pict) END
END make_pline;

VAR s1,s2: ARRAY [0..31] OF CHAR;

BEGIN
  IF bas.ENG THEN  s1:='First corner:'; s2:='second corner:'
  ELSE   s1:='Первый угол:'; s2:='Второй угол:' END;
  WITH new DO
    NEW(body,5);
    LOOP
      IF bas.read_point(s1,bas.cross,bas.null_monitor,body[0]) THEN
        bas.b_s(bas.cview,body[0],bas.x_old,bas.y_old);
        IF bas.read_point(s2,bas.cross_box,bas.null_monitor,body[2]) THEN
          WITH bas.cview^ DO
            body[2][Zc]:=body[0][Zc];
            body[1][Xc]:=body[2][Xc];
            body[1][Yc]:=body[0][Yc];
            body[1][Zc]:=body[0][Zc];

            body[3][Xc]:=body[0][Xc];
            body[3][Yc]:=body[2][Yc];
            body[3][Zc]:=body[0][Zc];
            body[4]:=body[0]
          END;
          make_pline;
          ltyp:=  c_ltype;
          color:= c_color;
          type:= 0;
          obj.new_pline(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
          bas.extr_pln(bas.cview^.model,new);
          bas.show_pline(bas.cview^.model,new);
          DISPOSE(pict);
        ELSE DISPOSE(body); RETURN END
      ELSE DISPOSE(body); RETURN END
    END
  END
END box;

----------------------------- TEXT -----------------------------

PROCEDURE text;
  VAR new: def.Text;
      top: def.VERTEX;
      clp: obj.Clip;
        j: INTEGER;

VAR  s: ARRAY [0..31] OF CHAR;
BEGIN
  IF bas.ENG THEN  s:='begin point text:'
  ELSE   s:='начальная точка текста:' END;
  WITH new DO
    IF bas.read_point(s,bas.cross,bas.null_monitor,top) THEN
      LOOP
        IF NOT bas.read_text(top,new) THEN EXIT END;
        IF str.len(stxt) > 0 THEN
          color:= c_color;
          obj.new_text(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
          bas.extr_txt(bas.cview^.model,new);
          bas.show_text(bas.cview^.model,new);
        END
      END
    END
  END
END text;

----------------------------- HATCH ----------------------------

VAR Xc,Yc,Zc: INTEGER;

PROCEDURE clock_wise(VAR p: def.POLYLINE): BOOLEAN;
  VAR ax,ay,bx,by: REAL;
              top: def.VERTEX;
           direct: INTEGER;
            i,h,H: INTEGER;

PROCEDURE clock_dir?;
  VAR TOP: def.VERTEX;
        i: INTEGER;
BEGIN
  bpm.message(240,180,'Укажите точку внутри контура:');
  IF bas.read_point('Внутренняя точка контура:',bas.cross,bas.null_monitor,TOP) THEN
    FOR i:=0 TO HIGH(p) DO
      ax:=p[(i+1) MOD H][Xc]-p[i][Xc];
      ay:=p[(i+1) MOD H][Yc]-p[i][Yc];
      bx:=TOP[Xc]-p[i][Xc];
      by:=TOP[Yc]-p[i][Yc];
      IF (ax*by-ay*bx>0.) THEN DEC(direct) ELSE INC(direct) END
    END;
  END
END clock_dir?;

BEGIN
  h:=HIGH(p); H:=h+1; direct:=0;
  FOR i:=1 TO h DO
    ax:=p[i][Xc]-p[i-1][Xc];
    ay:=p[i][Yc]-p[i-1][Yc];
    bx:=p[(i+1) MOD H][Xc]-p[i-1][Xc];
    by:=p[(i+1) MOD H][Yc]-p[i-1][Yc];
    IF (ax*by-ay*bx>0.) THEN DEC(direct) ELSE INC(direct) END
  END;
  ax:=p[0][Xc]-p[h][Xc];
  ay:=p[0][Yc]-p[h][Yc];
  bx:=p[1][Xc]-p[h][Xc];
  by:=p[1][Yc]-p[h][Yc];
  IF (ax*by-ay*bx>0.) THEN DEC(direct) ELSE INC(direct) END;
  IF direct=0 THEN clock_dir? END;
  IF direct=0 THEN RETURN FALSE END;
  IF direct < 0 THEN
    FOR i:=0 TO (h-1) DIV 2 DO top:=p[i]; p[i]:=p[h-i]; p[h-i]:=top END
  END;
  RETURN TRUE
END clock_wise;

PROCEDURE convex?(poly: def.POLYLINE): INTEGER;
  VAR ax,ay,bx,by: REAL;
            i,h,H: INTEGER;
BEGIN
  h:=HIGH(poly); H:=h+1;
  IF H<3 THEN RETURN -1 END;
  FOR i:=0 TO h DO
    ax:=poly[i][Xc]-poly[(i+h) MOD H][Xc];
    ay:=poly[i][Yc]-poly[(i+h) MOD H][Yc];
    bx:=poly[(i+1) MOD H][Xc]-poly[(i+h) MOD H][Xc];
    by:=poly[(i+1) MOD H][Yc]-poly[(i+h) MOD H][Yc];
    IF (ax*by-ay*bx>0.) THEN RETURN i END
  END;
  RETURN -1
END convex?;

(* точкa пересечения прямых, проходящих через точки top1,top2 и top_1,top_2
 и наxодящаяся на отрезке top_1,top_2 *)

PROCEDURE top?(top1,top2,top_1,top_2: def.VERTEX; VAR top: def.VERTEX): BOOLEAN;
  VAR a1,b1,c1,a2,b2,c2,d: REAL;
BEGIN
  a1:=top1[Yc]-top2[Yc];
  b1:=top2[Xc]-top1[Xc];
  c1:=top1[Xc]*top2[Yc]-top1[Yc]*top2[Xc];
  a2:=top_1[Yc]-top_2[Yc];
  b2:=top_2[Xc]-top_1[Xc];
  c2:=top_1[Xc]*top_2[Yc]-top_1[Yc]*top_2[Xc];
  d:=a1*b2-a2*b1;
  IF d=0. THEN RETURN FALSE END;
  top[Zc]:=top1[Zc];
  top[Xc]:=(b1*c2-b2*c1)/d;
  top[Yc]:=(c1*a2-c2*a1)/d;
  RETURN (top[Xc]<=mth.max(top_1[Xc],top_2[Xc])+0.01) &
         (top[Xc]>=mth.min(top_1[Xc],top_2[Xc])-0.01) &
         (top[Yc]<=mth.max(top_1[Yc],top_2[Yc])+0.01) &
         (top[Yc]>=mth.min(top_1[Yc],top_2[Yc])-0.01)
END top?;

PROCEDURE top_l(top1,top: def.VERTEX): REAL;
  VAR x,y,z: REAL;
BEGIN
  IF (top[Xc]=top1[Xc]) & (top[Yc]=top1[Yc]) THEN RETURN 0.
  ELSIF top[Yc]=top1[Yc] THEN RETURN ABS(top[Xc]-top1[Xc])
  ELSIF top[Xc]=top1[Xc] THEN RETURN ABS(top[Yc]-top1[Yc]) END;
  x:=top[Xc]-top1[Xc];
  y:=top[Yc]-top1[Yc];
  RETURN mth.sqrt(x*x+y*y)
END top_l;

PROCEDURE cut(n:INTEGER; poly: def.POLYLINE; VAR p1,p2: def.POLYLINE);
  VAR K,H,h,i: INTEGER;
          L,l: REAL;
          T,t: def.VERTEX;
BEGIN
  h:=HIGH(poly); H:=h+1;
  L:=mth.maxREAL;
  i:=(n+1) MOD H;  K:=i; T:=poly[(i+1) MOD H];
  REPEAT
    IF top?(poly[(n+h) MOD H],poly[n],poly[i],poly[(i+1) MOD H],t) THEN
      l:=top_l(poly[n],t);
      IF (l < L) & (top_l(t,poly[(n+h) MOD H]) > top_l(t,poly[n])) THEN
        L:=l; T:=t; K:=i
      END
    END;
    i:=(i+1) MOD H
  UNTIL i=(n+h-1) MOD H;
            (* first polygon *)
  IF K<n THEN NEW(p1,H+K-n+2) ELSE NEW(p1,K-n+2) END;
  FOR i:=0 TO HIGH(p1)-1 DO p1[i]:=poly[(n+i) MOD H] END;
  p1[HIGH(p1)]:=T;
            (* second polygon *)
  IF K<n THEN NEW(p2,n-K+1) ELSE NEW(p2,H-K+n)END;
  p2[0]:=T;
  FOR i:=1 TO HIGH(p2) DO p2[i]:=poly[(K+i) MOD H] END;
END cut;

PROCEDURE crossing(poly: def.POLYLINE): BOOLEAN;
  VAR xl,xr,yu,yd: REAL;
            i,j,h: INTEGER;
              top: def.VERTEX;
                d: REAL;
BEGIN
  h:=HIGH(poly)-1;
  FOR i:=0 TO h DO
    xr:=mth.max(poly[i][Xc],poly[i+1][Xc]);
    xl:=mth.min(poly[i][Xc],poly[i+1][Xc]);
    yd:=mth.min(poly[i][Yc],poly[i+1][Yc]);
    yu:=mth.max(poly[i][Yc],poly[i+1][Yc]);
    FOR j:=i+2 TO h DO
      IF top?(poly[i],poly[i+1],poly[j],poly[j+1],top) &
             (top[Xc]<xr) & (top[Xc]> xl) &
             (top[Yc]<yu) & (top[Yc]> yd)
      THEN RETURN TRUE END
    END
  END;
  RETURN FALSE
END crossing;

PROCEDURE area?(poly: def.POLYLINE; VAR lf,rg,dw,up: REAL);
   VAR i,r: INTEGER;
BEGIN
  r:=0;
  lf:=poly[0][Xc]; rg:=poly[0][Xc];
  dw:=poly[0][Yc]; up:=poly[0][Yc];
  FOR i:=1 TO HIGH(poly) DO
    IF poly[i][Xc]<lf THEN lf:=poly[i][Xc]
    ELSIF poly[i][Xc]> rg THEN r:=i; rg:=poly[i][Xc] END;
    IF poly[i][Yc]<dw THEN dw:=poly[i][Yc]
    ELSIF poly[i][Yc]>up THEN up:=poly[i][Yc] END
  END;
  up:=up+0.1; dw:=dw-0.1;
  lf:=FLOAT(TRUNC(lf-(up-dw)/ABS(h_tgA)))
END area?;


PROCEDURE htype?(VAR c: INTEGER; X,Y: INTEGER);
  VAR HTYP: bpm.TABLET;
         W: wnd.WINDOW;
         i: INTEGER;
BEGIN
  bpm.tnew(HTYP,5,2,50,50,28,28,bpm.xsel,'   Type hatch');
  bpm.twindow(HTYP,W);
  FOR i:=0 TO 9 DO
    bpm.tsprint(HTYP,i,'%c',CHAR(i+1));
    bpm.tdisable(HTYP,i)
  END;
  bpm.tchoose(HTYP,c);
  bpm.twindow(HTYP,W);
  bpm.tmove(HTYP,X,Y-W^.h);
  bpm.tundisable(HTYP,0);
  bpm.tundisable(HTYP,1);
  bpm.tundisable(HTYP,4);
  bpm.tundisable(HTYP,5);
  bpm.tundisable(HTYP,8);
  bpm.topen(HTYP);
  bpm.tselect(HTYP);
  IF bpm.tselected(HTYP) THEN h_type:= bpm.talt(HTYP) END;
  bpm.tdispose(HTYP)
END htype?;

PROCEDURE st?(VAR S: REAL; prmt: ARRAY OF CHAR; x,y: INTEGER): BOOLEAN;
  VAR s: ARRAY [0..7] OF CHAR;
     dn: BOOLEAN; p: INTEGER;
BEGIN
  s:='x x'; p:=0; bpm.diabox(x,y,150,s,prmt);
  IF s='x x' THEN  RETURN FALSE END;
  str.rscan(S,s,p,dn);
  RETURN dn
END st?;

PROCEDURE set_hat(): BOOLEAN;
  VAR s1,s2: ARRAY [0..31] OF CHAR;
       that: bpm.TABLET;
        X,Y: INTEGER;
         wn: wnd.WINDOW;
          A: REAL;
          d: BOOLEAN;

BEGIN
  IF bas.ENG THEN
    s1:='step'; s2:='angle';
    bpm.tnew(that,1,3,0,0,90,bpm.font^.H+5,bpm.xsel,' Hatch');
    bpm.tprint(that,0,"Type hatch");       bpm.thotkey(that,0,"t",TRUE);
    bpm.tprint(that,1,'Step  %f',h_step);  bpm.thotkey(that,1,"s",TRUE);
    bpm.tprint(that,2,'Angle %f%c',mth.rtod(mth.arctg(h_tgA)),07c);
                                           bpm.thotkey(that,3,"a",TRUE)
  ELSE
    s1:='шаг'; s2:='угол';
    bpm.tnew(that,1,3,0,0,100,bpm.font^.H+5,bpm.xsel,' Штриховка');
    bpm.tprint(that,0,"Тип штриховки");  bpm.thotkey(that,0,"т",TRUE);
    bpm.tprint(that,1,'Шаг  %f',h_step); bpm.thotkey(that,1,"ш",TRUE);
    bpm.tprint(that,2,'Угол %f%c',mth.rtod(mth.arctg(h_tgA)),07c);
                                         bpm.thotkey(that,3,"у",TRUE);
  END;
  bpm.twindow(that,wn);
  bpm.tmove(that,bas.cview^.cntx^.sX-wn^.w DIV 2,bas.cview^.cntx^.sY-wn^.h DIV 2);
  bpm.twindow(that,wn);
  X:=wn^.x; Y:=wn^.y+ wn^.h;
  bpm.topen(that);
  LOOP
    bpm.tselect(that);
    IF NOT bpm.tselected(that) THEN bpm.tdispose(that); RETURN FALSE END;
    CASE bpm.talt(that) OF
       0: htype?(h_type,X-20,Y+50);
      |1: IF st?(h_step,s1,X+20,Y+46) THEN
            bpm.tprint(that,1,'%s %f',s1,h_step)
          END;
      |2: IF st?(A,s2,X+20,Y+36) THEN
            IF mth.round(A)=90 THEN h_tgA:=100000. ELSE
            h_tgA:= mth.tg(mth.dtor(A)) END;
            bpm.tprint(that,2,'%s %f%c',s2,mth.rtod(mth.arctg(h_tgA)),07c)
          END
    ELSE END;
    bpm.tunselect(that)
  END
END set_hat;


PROCEDURE set_cont(): BOOLEAN;
BEGIN RETURN FALSE
END set_cont;


PROCEDURE hatch;
VAR xl,xr,yu,yd: REAL;
           new: def.Group;
          cont: def.POLYLINE;
         POLYS: DYNARR OF def.POLYLINE;
           top: def.VERTEX;
           i,j: INTEGER;
            dn: BOOLEAN;

(*
PROCEDURE Convex(p: def.POLYLINE);
  VAR p1,p2: def.POLYLINE;
      t,i,c: INTEGER;
BEGIN
  NEW(POLYS,1);
  NEW(POLYS[0],HIGH(p)+1);
  POLYS[0] :=p;
  REPEAT
    c:= HIGH(POLYS);
    FOR i:=0 TO HIGH(POLYS) DO
      t:= convex?(POLYS[i]);
      IF t # -1  THEN
        cut(t,POLYS[i],p1,p2);
        RESIZE(POLYS[i],HIGH(p1)+1);
        POLYS[HIGH(POLYS)]:= p1;
        RESIZE(POLYS,HIGH(POLYS)+2);
        NEW(POLYS[HIGH(POLYS)],HIGH(p2)+1);
        POLYS[HIGH(POLYS)]:= p2
      END
    END
  UNTIL c#HIGH(POLYS)
END Convex;
*)

PROCEDURE Convex(poly: def.POLYLINE);
  VAR pol1,pol2: def.POLYLINE;
              i: INTEGER;
BEGIN
  i:=convex?(poly);
  IF i=-1 THEN
    IF HIGH(poly)>1 THEN
      RESIZE(POLYS,HIGH(POLYS)+2);
      NEW(POLYS[HIGH(POLYS)],HIGH(poly)+1);
      POLYS[HIGH(POLYS)]:=poly
    END;
    RETURN
  END;
  cut(i,poly,pol1,pol2);
  Convex(pol1);
  Convex(pol2)
END Convex;

PROCEDURE eqv(t1,t2: def.VERTEX): BOOLEAN;
BEGIN RETURN (t1[Xc]= t2[Xc]) & (t1[Yc]= t2[Yc]) END eqv;

VAR v: DYNARR OF def.VECTOR;

PROCEDURE fuse;
  VAR i: INTEGER;
BEGIN
  FOR i:=HIGH(v) TO 1 BY -1 DO
    IF     eqv(v[i,0],v[i-1,0]) THEN v[i-1,0]:=v[i,1]; RESIZE(v,HIGH(v))
    ELSIF  eqv(v[i,1],v[i-1,1]) THEN v[i-1,1]:=v[i,0]; RESIZE(v,HIGH(v))
    ELSIF  eqv(v[i,0],v[i-1,1]) THEN v[i-1,1]:=v[i,1]; RESIZE(v,HIGH(v))
    ELSIF  eqv(v[i,1],v[i-1,0]) THEN v[i-1,0]:=v[i,0]; RESIZE(v,HIGH(v))
    END
  END
END fuse;

PROCEDURE _hat0(poly: ARRAY OF def.POLYLINE);
  VAR top,tp1,tp2,top1,top2: def.VERTEX;
               first,second: BOOLEAN;
                    H,i,j,k: INTEGER;

BEGIN
  WITH new DO
    NEW(v);
    first:= FALSE;
    second:=FALSE;
    top1[Zc]:= poly[0,0][Zc];
    top2[Zc]:= poly[0,0][Zc];
    IF h_tgA > 0. THEN tp1[Yc]:=yd; tp2[Yc]:=yu
    ELSE               tp2[Yc]:=yd; tp1[Yc]:=yu END;
    tp1[Xc]:= xl;
    tp2[Xc]:= xl+(tp2[Yc]-tp1[Yc])/h_tgA;
    REPEAT
      FOR k:= 0 TO HIGH(poly) DO
        H:= HIGH(poly[k])+1;
        i:= 0;
        REPEAT
          IF top?(tp1,tp2,poly[k,i],poly[k,(i+1) MOD H],top) THEN
            IF NOT first THEN
              top1:=top; first:=TRUE
            ELSE
              top2:=top; second:=TRUE;
              RESIZE(v,HIGH(v)+2);
              v[HIGH(v),0]:=top1;
              v[HIGH(v),1]:=top2
            END
          END; INC(i)
        UNTIL  second OR (i=H);
        first := FALSE;
        second:= FALSE
      END;
      fuse;
      k:= HIGH(line)+1; j:=0;
      RESIZE(line,HIGH(line)+HIGH(v)+2);
      FOR i:=k TO HIGH(line) DO
        line[i].xyz:= v[j,0];
        line[i].XYZ:= v[j,1];
        line[i].color:=c_color;
        line[i].ltyp:=0;
        NEW(line[i].pict);
        INC(j)
      END;
      NEW(v,0);
      tp1[Xc]:=tp1[Xc]+h_step;
      tp2[Xc]:=tp2[Xc]+h_step
    UNTIL tp1[Xc]>=xr;
    DISPOSE(v)
  END
END _hat0;

PROCEDURE _hat0v(poly: ARRAY OF def.POLYLINE);
  VAR top,tp1,tp2,top1,top2: def.VERTEX;
               first,second: BOOLEAN;
                    H,i,j,k: INTEGER;
BEGIN
  WITH new DO
    first:= FALSE; second:=FALSE;
    top1[Zc]:=poly[0,0][Zc];
    top2[Zc]:=poly[0,0][Zc];
    tp1[Xc]:=xl; tp2[Xc]:=xr;
    tp1[Yc]:=yd; tp2[Yc]:=yd;
    REPEAT
      NEW(v);
      FOR k:= 0 TO HIGH(poly) DO
        H:=HIGH(poly[k])+1; i:=0;
        REPEAT
          IF top?(tp1,tp2,poly[k,i],poly[k,(i+1) MOD H],top) THEN
            IF NOT first THEN
              top1:=top; first:=TRUE
            ELSE
              top2:=top;
              second:=TRUE;
              RESIZE(v,HIGH(v)+2);
              v[HIGH(v)][0]:=top1;
              v[HIGH(v)][1]:=top2
            END
          END; INC(i)
        UNTIL  second OR (i=H);
        first:=FALSE;
        second:=FALSE
      END;
      fuse;
      k:= HIGH(line)+1; j:=0;
      RESIZE(line,HIGH(line)+HIGH(v)+2);
      FOR i:=k TO HIGH(line) DO
        line[i].xyz:= v[j,0];
        line[i].XYZ:= v[j,1];
        line[i].color:=c_color;
        line[i].ltyp:=0;
        NEW(line[i].pict);
        INC(j)
      END;
      tp1[Yc]:=tp1[Yc]+h_step;
      tp2[Yc]:=tp2[Yc]+h_step;
    UNTIL tp1[Yc]>=yu;
    DISPOSE(v)
  END
END _hat0v;

PROCEDURE hat0(poly: ARRAY OF def.POLYLINE);
BEGIN
 IF mth.round(h_tgA)=0 THEN _hat0v(poly); RETURN END;
 _hat0(poly)
END hat0;


PROCEDURE _hat1v(p: ARRAY OF def.POLYLINE);
  VAR tg: REAL;
BEGIN
  tg:=h_tgA; h_tgA:=100000.; _hat0(p);   h_tgA:=tg ;
  _hat0v(p)
END _hat1v;

PROCEDURE _hat1(p: ARRAY OF def.POLYLINE);
  VAR tg,st: REAL;
BEGIN
  _hat0(p);
  st:=h_step; h_step:=h_step*h_tgA;
  tg:=h_tgA; h_tgA:=-1./h_tgA;
  _hat0(p); h_step:=st; h_tgA:=tg
END _hat1;

PROCEDURE hat1(p: ARRAY OF def.POLYLINE);
BEGIN
 IF mth.round(h_tgA)=0 THEN _hat1v(p); RETURN END;
 _hat1(p)
END hat1;

PROCEDURE hat_n(poly: ARRAY OF def.POLYLINE; Z: INTEGER);
  VAR top,tp1,tp2,top1,top2: def.VERTEX;
               first,second: BOOLEAN;
                  H,i,j,k,z: INTEGER;
BEGIN
  WITH new DO
    z:= 0;
    NEW(v);
    first:= FALSE;
    second:=FALSE;
    top1[Zc]:= poly[0,0][Zc];
    top2[Zc]:= poly[0,0][Zc];
    IF h_tgA > 0. THEN tp1[Yc]:=yd; tp2[Yc]:=yu
    ELSE               tp2[Yc]:=yd; tp1[Yc]:=yu END;
    tp1[Xc]:= xl;
    tp2[Xc]:= xl+(tp2[Yc]-tp1[Yc])/ h_tgA;
    REPEAT
      FOR k:= 0 TO HIGH(poly) DO
        H:= HIGH(poly[k])+1;
        i:= 0;
        REPEAT
          IF top?(tp1,tp2,poly[k,i],poly[k,(i+1) MOD H],top) THEN
            IF NOT first THEN
              top1:=top; first:=TRUE
            ELSE
              top2:=top; second:=TRUE;
              RESIZE(v,HIGH(v)+2);
              v[HIGH(v),0]:=top1;
              v[HIGH(v),1]:=top2
            END
          END; INC(i)
        UNTIL  second OR (i=H);
        first := FALSE;
        second:= FALSE
      END;
      fuse;
      k:= HIGH(line)+1; j:=0;
      RESIZE(line,HIGH(line)+HIGH(v)+2);
      FOR i:=k TO HIGH(line) DO
        line[i].xyz:= v[j,0];
        line[i].XYZ:= v[j,1];
        line[i].color:=c_color;
        line[i].ltyp:=0;
        NEW(line[i].pict);
        INC(j)
      END;
      NEW(v,0);
      z:= (z+1) MOD Z;
      IF z = 0 THEN
        tp2[Xc]:=tp2[Xc]+h_step * 2.;
        tp1[Xc]:=tp1[Xc]+h_step * 2.
      ELSE
        tp1[Xc]:=tp1[Xc]+h_step;
        tp2[Xc]:=tp2[Xc]+h_step
      END
    UNTIL tp1[Xc]>=xr;
    DISPOSE(v)
  END
END hat_n;

PROCEDURE hat4(p: ARRAY OF def.POLYLINE); BEGIN   hat_n(p,2) END hat4;

PROCEDURE hat5(p: ARRAY OF def.POLYLINE);
  VAR i: INTEGER;
BEGIN
  hat0(p);
  WITH new DO
    FOR i:=0 TO HIGH(line) DO
      make_line(patts[2],line[i])
    END
  END
END hat5;

PROCEDURE hat8(p: ARRAY OF def.POLYLINE); BEGIN   hat_n(p,3) END hat8;

VAR clp: obj.Clip;

 PROCEDURE read_cont(VAR cont: def.POLYLINE);
 BEGIN
  IF bas.read_point('First point curve:',bas.cross,set_hat,cont[0]) THEN
    bas.b_s(bas.cview, cont[0],bas.x_old,bas.y_old);
    WHILE bas.read_point('Next point curve:',bas.cross_line,set_cont,top) DO
      RESIZE(cont,HIGH(cont)+2);
      cont[HIGH(cont)]:=top;
      IF crossing(cont) THEN
         RESIZE(cont,HIGH(cont));
         bpm.message(240,180,'Контур не должен иметь самопересечений !')
      ELSE
        bas.b_s(bas.cview, top,bas.x_old,bas.y_old);
        bas.xshow_vector(bas.cview,cont[HIGH(cont)],cont[HIGH(cont)-1],bas.rubb)
      END
    END;
    IF crossing(cont) THEN
      bpm.message(240,180,'Контур имеет самопересечения !!!');
      RETURN
    ELSE
      bas.xshow_vector(bas.cview,cont[HIGH(cont)],cont[0],bas.rubb)
    END
  END
 END read_cont;

BEGIN
  Xc:= bas.cview^.Xc;
  Yc:= bas.cview^.Yc;
  Zc:= bas.cview^.Zc;
  WITH new DO
    NEW(POLYS);  NEW(cont,1);
    read_cont(cont);
    NEW(line  );
    NEW(circ  );
    NEW(arc   );
    NEW(ellips);
    NEW(pline );
    NEW(txt   );
    NEW(surf  );
    IF HIGH(cont)<2 THEN RETURN END;
    area?(cont,xl,xr,yd,yu);
    IF  NOT clock_wise(cont) THEN  RETURN END;
    Convex(cont);
    CASE h_type OF
       0 : hat0 (POLYS)
      |1 : hat1 (POLYS)
      |4 : hat4 (POLYS)
      |5 : hat5 (POLYS)
      |8 : hat8 (POLYS)
    ELSE
      bpm.message(240,180,'нереализованный тип штриxовки');
    END;
    FOR i:=1 TO HIGH(cont) DO
      bas.xshow_vector(bas.cview,cont[i-1],cont[i],bas.bckg)
    END;
    obj.new_group(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
    bas.extr_grp(bas.cview^.model,new);
    bas.xshow_vector(bas.cview,cont[0],cont[HIGH(cont)],bas.bckg);
    bas.show_group(bas.cview^.model,new);
    DISPOSE(cont);
    FOR i:=0 TO HIGH(POLYS) DO DISPOSE(POLYS[i]) END;
    DISPOSE(POLYS);
    FOR i:=0 TO HIGH(line) DO obj.dispose_line(line[i]) END;
    DISPOSE(line)
  END
END hatch;

----------------------------- init -----------------------------

PROCEDURE ini_patt;
BEGIN
  NEW(patts[0],0);   (* сплошная линия *)
  NEW(patts[1],3);   (* пунктир *)
    patts[1,0]:=8.;
    patts[1,1]:=6.;
    patts[1,2]:=2.;
  NEW(patts[2],5);   (* штрихпунктир *)
    patts[2,0]:=11.;
    patts[2,1]:=6.;
    patts[2,2]:=2.;
    patts[2,3]:=1.;
    patts[2,4]:=2.;
  NEW(patts[3],7);     (* штриxпунктир с двумя точками *)
    patts[3,0]:=16.;
    patts[3,1]:=8.;
    patts[3,2]:=2.;
    patts[3,3]:=1.;
    patts[3,4]:=2.;
    patts[3,5]:=1.;
    patts[3,6]:=2.;
  NEW(patts[4],0);     (* утолщенная линия *)
END ini_patt;

PROCEDURE init;
BEGIN
  c_ltype:=0;-- NEW(pattern);
  c_color:=2;
  step_p:= 1.;
  cir_type:=0;
  arc_type:=0;
  pln_type:=0;
  ell_type:=0;
  h_type:= 0;
  h_tgA := 1.;
  h_step:= 5.
END init;

BEGIN
 ini_patt;
 init
END bcDraft.
