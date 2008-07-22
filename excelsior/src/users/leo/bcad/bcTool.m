IMPLEMENTATION MODULE bcTool; (*$X+ brd 19-Jan-91. (c) KRONOS *)

IMPORT  def: bcDef;
IMPORT  bas: bcBase;
IMPORT  dft: bcDraft;
IMPORT  obj: bcObj;
IMPORT  bmt: bcMath;
IMPORT  bpm: bcPM;
IMPORT  mth: realMath;
IMPORT  str: Strings;
IMPORT  mem: Heap;

IMPORT tty: Terminal;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

PROCEDURE max(x,y: REAL): REAL;          (* максимальное из x,y *)
BEGIN IF x>y THEN RETURN x END; RETURN y END max;

PROCEDURE min(x,y: REAL): REAL;          (* минимальное  из x,y *)
BEGIN IF x<y THEN RETURN x END; RETURN y END min;

PROCEDURE distance(top1,top2,top: def.VERTEX): REAL;
  VAR A,B,C: REAL;
      xc,yc: INTEGER;
BEGIN
  xc:= bas.cview^.Xc; yc:= bas.cview^.Yc;
  IF top1[xc] = top2[xc] THEN
    RETURN ABS(top1[xc]-top[xc])
  ELSIF top1[yc] = top2[yc] THEN
    RETURN ABS(top1[yc]-top[yc])
  ELSE
    A:=top2[yc]-top1[yc];
    B:=top1[xc]-top2[xc];
    C:=top1[yc]*top2[xc]-top1[xc]*top2[yc];
    RETURN ABS((A*top[xc]+B*top[yc]+C)/mth.sqrt(A*A+B*B))
  END
END distance;

PROCEDURE distance1(top1,top: def.VERTEX): REAL;
  VAR x1,y1,x2,y2: REAL;
BEGIN
  WITH bas.cview^ DO
    x1:=top[Xc]; x2:=top1[Xc];
    y1:=top[Yc]; y2:=top1[Yc]
  END;
  IF    x1=x2 THEN  RETURN ABS(y1-y2)
  ELSIF y1=y2 THEN  RETURN ABS(x1-x2)
  ELSE
    x1:=x1-x2; y1:=y1-y2; RETURN mth.sqrt(x1*x1+y1*y1)
  END
END distance1;

PROCEDURE mark_area(top: def.VERTEX);
BEGIN
  WITH bas.cview^ DO
    IF     top[Xc]<bas.x_mrk THEN bas.x_mrk:=top[Xc]
    ELSIF  top[Xc]>bas.X_mrk THEN bas.X_mrk:=top[Xc] END;
    IF     top[Yc]<bas.y_mrk THEN bas.y_mrk:=top[Yc]
    ELSIF  top[Yc]>bas.Y_mrk THEN bas.Y_mrk:=top[Yc] END
  END
END mark_area;

PROCEDURE mark_area1(top: def.VERTEX; r: REAL);
BEGIN
  WITH bas.cview^ DO
    IF top[Xc]-r<bas.x_mrk THEN bas.x_mrk:=top[Xc]-r END;
    IF top[Xc]+r>bas.X_mrk THEN bas.X_mrk:=top[Xc]+r END;
    IF top[Yc]-r<bas.y_mrk THEN bas.y_mrk:=top[Yc]-r END;
    IF top[Yc]+r>bas.Y_mrk THEN bas.Y_mrk:=top[Yc]+r END
  END
END mark_area1;

PROCEDURE mark_area2(grp: def.Group);
  VAR i,j: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line) DO
      mark_area(line[i].xyz);
      mark_area(line[i].XYZ)
    END;
    FOR i:=0 TO HIGH(circ) DO
      mark_area1(circ[i].centr,circ[i].r)
    END;
    FOR i:=0 TO HIGH(arc) DO
      mark_area(arc[i].xyz1);
      mark_area(arc[i].xyz2);
      mark_area(arc[i].xyz3)
    END;
    FOR i:=0 TO HIGH(ellips) DO
      mark_area1(ellips[i].centr,mth.max(ellips[i].a,ellips[i].a))
    END;
    FOR i:=0 TO HIGH(pline) DO
      FOR j:=0 TO HIGH(pline[i].body) DO
        mark_area(pline[i].body[j])
      END
    END;
    FOR i:=0 TO HIGH(txt) DO
      FOR j:=0 TO HIGH(txt[i].pict) DO
        mark_area(txt[i].pict[j,0]);
        mark_area(txt[i].pict[j,1])
      END
    END
  END
END mark_area2;

VAR DST: REAL;

PROCEDURE lin?(t: def.VERTEX; l: def.Line): BOOLEAN;
VAR  x,y,x1,y1,x2,y2: REAL;
BEGIN
  WITH bas.cview^ DO x:=t[Xc]; y:=t[Yc];
    WITH l DO
      x1:= min(xyz[Xc],XYZ[Xc])-DST;
      x2:= max(xyz[Xc],XYZ[Xc])+DST;
      y1:= min(xyz[Yc],XYZ[Yc])-DST;
      y2:= max(xyz[Yc],XYZ[Yc])+DST;
      RETURN  (x>x1) & (x<x2) & (y>y1) & (y<y2) & (distance(xyz,XYZ,t) <= DST)
    END
  END
END lin?;

PROCEDURE search_lin(i: INTEGER; top: def.VERTEX);
  VAR lin: obj.lin_ptr;

  PROCEDURE mark_lin?(l: obj.lin_ptr): BOOLEAN;
    VAR j: INTEGER;
  BEGIN
    FOR j:=0 TO HIGH(obj.mark_lin) DO
      IF obj.mark_lin[j]= l THEN RETURN FALSE END
    END;
    RETURN TRUE
  END mark_lin?;

BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    lin:= lines;
    IF lin#NIL THEN
      REPEAT
        WITH lines^ DO
          IF lin?(top,line) & (mark_lin?(lines)) THEN
            RESIZE(obj.mark_lin,HIGH(obj.mark_lin)+2);
            RESIZE(obj.klin,HIGH(obj.klin)+2);
            obj.mark_lin[HIGH(obj.mark_lin)]:=lines;
            obj.klin[HIGH(obj.klin)]:=i;
            mark_area(line.xyz);
            mark_area(line.XYZ);
            bas.xshow_line(bas.cview^.model,line,bas.mark)
          END
        END;
        lines:= lines^.next
      UNTIL (lin = lines)
    END
  END
END search_lin;

PROCEDURE cir?(top: def.VERTEX; c: def.Circ): BOOLEAN;
BEGIN RETURN  (ABS(bmt.dist1(c.centr,top)- c.r) < DST) END cir?;

PROCEDURE search_cir(i: INTEGER; top: def.VERTEX);
  VAR cir: obj.cir_ptr;

  PROCEDURE mark_cir?(c: obj.cir_ptr): BOOLEAN;
    VAR j: INTEGER;
  BEGIN
    FOR j:=0 TO HIGH(obj.mark_cir) DO
      IF obj.mark_cir[j]= c THEN RETURN FALSE END
    END;
    RETURN TRUE
  END mark_cir?;

BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    cir:= circs;
    IF cir#NIL THEN
      REPEAT
       WITH circs^ DO
          IF cir?(top,circ) &  mark_cir?(circs) THEN
            RESIZE(obj.mark_cir,HIGH(obj.mark_cir)+2);
            RESIZE(obj.kcir,HIGH(obj.kcir)+2);
            obj.mark_cir[HIGH(obj.mark_cir)]:=circs;
            obj.kcir[HIGH(obj.kcir)]:=i;
            mark_area1(circ.centr,circ.r);
            bas.xshow_cir(bas.cview^.model,circ,bas.mark)
          END;
          circs:=next
        END
      UNTIL (cir = circs)
    END
  END
END search_cir;

PROCEDURE vect?(t,t1,t2: def.VERTEX): BOOLEAN;
VAR x,y,x1,y1,x2,y2: REAL;
BEGIN
  WITH bas.cview^ DO
    x:=t[Xc];
    y:=t[Yc];
    x1:=min(t1[Xc],t2[Xc])-DST;
    x2:=max(t1[Xc],t2[Xc])+DST;
    y1:=min(t1[Yc],t2[Yc])-DST;
    y2:=max(t1[Yc],t2[Yc])+DST;
  END;
  RETURN  (x>x1) & (x<x2) & (y>y1) & (y<y2) & (distance(t1,t2,t) <= DST)
END vect?;

PROCEDURE pln?(top: def.VERTEX; pln: def.Pline): BOOLEAN;
VAR j,i: INTEGER;
BEGIN
  WITH pln DO
    IF HIGH(pict)<0 THEN
      FOR i:=1 TO HIGH(body) DO
        IF vect?(top,body[i-1],body[i]) THEN RETURN TRUE END
      END
    ELSE
      FOR i:=0 TO HIGH(pict) DO
        FOR j:=1 TO HIGH(pict[i]) DO
          IF vect?(top,pict[i,j-1],pict[i,j]) THEN RETURN TRUE END
        END
      END
    END
  END;
  RETURN FALSE
END pln?;

PROCEDURE search_pln(i: INTEGER; top: def.VERTEX);
  VAR   pln: obj.pln_ptr;
        j,k: INTEGER;

  PROCEDURE mark_pln?(p: obj.pln_ptr): BOOLEAN;
    VAR j: INTEGER;
  BEGIN
    FOR j:=0 TO HIGH(obj.mark_pln) DO
      IF obj.mark_pln[j]= p THEN RETURN FALSE END
    END;
    RETURN TRUE
  END mark_pln?;

BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    pln:= plins;
    IF pln#NIL THEN
      REPEAT
        WITH plins^ DO
          IF (pln?(top,pline)) & (mark_pln?(plins)) THEN
            RESIZE(obj.mark_pln,HIGH(obj.mark_pln)+2);
            RESIZE(obj.kpln,HIGH(obj.kpln)+2);
            obj.mark_pln[HIGH(obj.mark_pln)]:= plins;
            obj.kpln[HIGH(obj.kpln)]:= i;
            WITH pline DO
              IF HIGH(pict)<0 THEN
                FOR j:=0 TO HIGH(body) DO mark_area(body[j]) END
              ELSE
                FOR j:=0 TO HIGH(pict) DO
                  FOR k:=0 TO HIGH(pict[j]) DO mark_area(pict[j,k]) END
                END
              END
            END;
            bas.xshow_pline(bas.cview^.model,pline,bas.mark)
          END;
          plins:= next
        END
      UNTIL (pln = plins)
    END
  END
END search_pln;

(*
PROCEDURE arc?(top: def.VERTEX; arc: def.Arc): BOOLEAN;
 VAR centr: def.VERTEX;
         r: REAL;
  a1,ax,a3: REAL;
BEGIN
  WITH arc DO
    a1:= bmt.angle_0(centr,xyz1);
    a3:= bmt.angle_0(centr,xyz3);
    IF bmt.calc_arc(centr[bas.Xc],centr[bas.Yc],r,xyz1,xyz2,xyz3) THEN
      IF (ABS(distance1(top,centr)-r) <= DST) THEN
        IF (a1<a3) THEN
          ax:= bmt.angle_(centr,xyz1,xyz3);
          IF bmt.angle_(centr,xyz1,xyz2) <ax THEN
            RETURN bmt.angle_(centr,xyz1,top) < ax
          ELSE
            RETURN bmt.angle_(centr,xyz1,top) > ax
          END;
        ELSE
          ax:= bmt.angle_(centr,xyz3,xyz1);
          IF bmt.angle_(centr,xyz3,xyz2) <ax THEN
             RETURN bmt.angle_(centr,xyz3,top) < ax
           ELSE
            RETURN bmt.angle_(centr,xyz3,top) > ax
          END
        END
      END
    END
  END;
  RETURN FALSE
END arc?;

*)
PROCEDURE arc?(top: def.VERTEX; arc: def.Arc): BOOLEAN;
  VAR i,j: INTEGER;
BEGIN
  WITH arc DO
    FOR i:=0 TO HIGH(pict) DO
      FOR j:=1 TO HIGH(pict[i]) DO
        IF vect?(top,pict[i,j-1],pict[i,j]) THEN RETURN TRUE END
      END
    END
  END;
  RETURN FALSE
END arc?;

PROCEDURE search_arc(i: INTEGER; top: def.VERTEX);
  VAR arc: obj.arc_ptr;
        c: def.VERTEX;
        r: REAL;

  PROCEDURE mark_arc?(a: obj.arc_ptr): BOOLEAN;
    VAR j: INTEGER;
  BEGIN
    FOR j:=0 TO HIGH(obj.mark_arc) DO
      IF obj.mark_arc[j]= a THEN RETURN FALSE END
    END;
    RETURN TRUE
  END mark_arc?;

BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    arc:=arcs;
    IF arc#NIL THEN
      REPEAT
        WITH arcs^ DO
          IF (arc?(top,arc)) & (mark_arc?(arcs)) THEN
            RESIZE(obj.mark_arc,HIGH(obj.mark_arc)+2);
            RESIZE(obj.karc,HIGH(obj.karc)+2);
            obj.mark_arc[HIGH(obj.mark_arc)]:=arcs;
            obj.karc[HIGH(obj.karc)]:=i;
            WITH arc DO
              WITH bas.cview^ DO
                IF bmt.calc_arc(c[Xc],c[Yc],r,xyz1,xyz2,xyz3) THEN
                  mark_area1(c,r)
                ELSE
                  mark_area(xyz1);
                  mark_area(xyz3)
                END
              END
            END;
            bas.xshow_arc(bas.cview^.model,arcs^.arc,bas.mark)
          END
        END;
        arcs:=arcs^.next
      UNTIL arc=arcs
    END
  END
END search_arc;

PROCEDURE ell?(top: def.VERTEX; ell: def.Ellips): BOOLEAN;
BEGIN
END ell?;

PROCEDURE search_ell(i: INTEGER; top: def.VERTEX);
BEGIN
END search_ell;

PROCEDURE txt?(top: def.VERTEX; text: def.Text): BOOLEAN;
  VAR  i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(text.pict) DO
    IF vect?(top,text.pict[i,0],text.pict[i,1]) THEN RETURN TRUE END
  END;
  RETURN FALSE
END txt?;

PROCEDURE search_txt(i: INTEGER; top: def.VERTEX);
  VAR txt: obj.txt_ptr;
        j: INTEGER;

  PROCEDURE mark_txt?(t: obj.txt_ptr): BOOLEAN;
    VAR j: INTEGER;
  BEGIN
    FOR j:=0 TO HIGH(obj.mark_txt) DO
      IF obj.mark_txt[j]=t THEN RETURN FALSE END
    END;
    RETURN TRUE
  END mark_txt?;

BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    txt:=texts;
    IF txt#NIL THEN
      REPEAT
        WITH texts^ DO
          IF txt?(top,text) & mark_txt?(texts) THEN
            RESIZE(obj.mark_txt,HIGH(obj.mark_txt)+2);
            RESIZE(obj.ktxt,HIGH(obj.ktxt)+2);
            obj.mark_txt[HIGH(obj.mark_txt)]:=texts;
            obj.ktxt[HIGH(obj.ktxt)]:=i;
            FOR j:=0 TO HIGH(text.pict) DO
              mark_area(text.pict[j,0]);
              mark_area(text.pict[j,1])
            END;
            bas.xshow_text(bas.cview^.model,text,bas.mark)
          END;
          texts:=texts^.next;
        END
      UNTIL txt=texts
    END
  END
END search_txt;

PROCEDURE grp?(top: def.VERTEX; grp: def.Group): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line) DO
      IF lin?(top,line[i]) THEN RETURN TRUE END
    END;
    FOR i:=0 TO HIGH(circ) DO
      IF cir?(top,circ[i]) THEN RETURN TRUE END
    END;
    FOR i:=0 TO HIGH(arc) DO
      IF arc?(top,arc[i]) THEN RETURN TRUE  END
    END;
    FOR i:=0 TO HIGH(ellips) DO
      IF ell?(top,ellips[i]) THEN RETURN TRUE END
    END;
    FOR i:=0 TO HIGH(pline) DO
      IF pln?(top,pline[i]) THEN RETURN TRUE END
    END;
    FOR i:=0 TO HIGH(txt) DO
      IF txt?(top,txt[i]) THEN RETURN TRUE END
    END;
  END;
  RETURN FALSE
END grp?;

PROCEDURE search_grp(i: INTEGER; top: def.VERTEX);
  VAR  grp: obj.grp_ptr;

  PROCEDURE mark_grp?(g: obj.grp_ptr): BOOLEAN;
    VAR j: INTEGER;
  BEGIN
    FOR j:=0 TO HIGH(obj.mark_grp) DO
      IF obj.mark_grp[j]= g THEN RETURN FALSE END
    END;
    RETURN TRUE
  END mark_grp?;

BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    grp:=grps;
    IF grp#NIL THEN
      REPEAT
        IF (grp?(top,grps^.group)) & (mark_grp?(grps)) THEN
          RESIZE(obj.mark_grp,HIGH(obj.mark_grp)+2);
          RESIZE(obj.kgrp,HIGH(obj.kgrp)+2);
          obj.mark_grp[HIGH(obj.mark_grp)]:=grps;
          obj.kgrp[HIGH(obj.kgrp)]:=i;
          mark_area2(grps^.group);
          bas.xshow_group(bas.cview^.model,grps^.group,bas.mark)
        END;
        grps:=grps^.next
      UNTIL grp=grps
    END
  END
END search_grp;

PROCEDURE marked?(): BOOLEAN;
BEGIN
  RETURN (HIGH(obj.mark_lin)+ HIGH(obj.mark_cir)+
          HIGH(obj.mark_arc)+ HIGH(obj.mark_ell)+
          HIGH(obj.mark_pln)+ HIGH(obj.mark_txt)+
          HIGH(obj.mark_grp))#-7
END marked?;

PROCEDURE mark1(top: def.VERTEX);
  VAR i: INTEGER;
BEGIN
  DST:= 3./bas.cview^.scale;
  FOR i:=0 TO HIGH(bas.cview^.model^.mbody^) DO
    IF bas.cview^.cntx^.mask[i] # 0 THEN
      search_lin(i,top);
      search_cir(i,top);
      search_pln(i,top);
      search_arc(i,top);
      search_txt(i,top);
      search_grp(i,top)
    END
  END
END mark1;

PROCEDURE new_mark;
BEGIN
  bas.X_mrk:=-bas.Max; bas.Y_mrk:=-bas.Max;
  bas.x_mrk:= bas.Max; bas.y_mrk:= bas.Max;
  NEW(obj.mark_lin,0);  NEW(obj.klin,0);
  NEW(obj.mark_cir,0);  NEW(obj.kcir,0);
  NEW(obj.mark_arc,0);  NEW(obj.karc,0);
  NEW(obj.mark_ell,0);  NEW(obj.kell,0);
  NEW(obj.mark_pln,0);  NEW(obj.kpln,0);
  NEW(obj.mark_txt,0);  NEW(obj.ktxt,0);
  NEW(obj.mark_srf,0);  NEW(obj.ksrf,0);
  NEW(obj.mark_grp,0);  NEW(obj.kgrp,0)
END new_mark;

PROCEDURE select(): BOOLEAN;
 VAR top: def.VERTEX;
       s: BOOLEAN;
BEGIN
  WITH bas.cview^.cntx^ DO
    s:=STEP; STEP:=FALSE;
    new_mark;
    LOOP
      IF bas.read_point('Select object:',bas.marker,bas.null_monitor,top) THEN
        mark1(top)
      ELSE STEP:=s; bas.step; RETURN marked?() END
    END
  END
END select;

PROCEDURE unmark;
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(obj.mark_lin) DO
    WITH obj.mark_lin[i]^ DO bas.show_line(bas.cview^.model,line) END
  END;
  FOR i:=0 TO HIGH(obj.mark_cir) DO
    WITH obj.mark_cir[i]^ DO bas.show_cir(bas.cview^.model,circ) END
  END;
  FOR i:=0 TO HIGH(obj.mark_pln) DO
    WITH obj.mark_pln[i]^ DO bas.show_pline(bas.cview^.model,pline) END
  END;
  FOR i:=0 TO HIGH(obj.mark_arc) DO
    WITH obj.mark_arc[i]^ DO bas.show_arc(bas.cview^.model,arc) END
  END;
  FOR i:=0 TO HIGH(obj.mark_txt) DO
    WITH obj.mark_txt[i]^ DO bas.show_text(bas.cview^.model,text) END
  END;
  FOR i:=0 TO HIGH(obj.mark_grp) DO
    WITH obj.mark_grp[i]^ DO bas.show_group(bas.cview^.model,group) END
  END;
  bas.X_mrk:=-bas.Max; bas.Y_mrk:=-bas.Max;
  bas.x_mrk:= bas.Max; bas.y_mrk:= bas.Max;
  DISPOSE(obj.mark_lin); DISPOSE(obj.klin);
  DISPOSE(obj.mark_cir); DISPOSE(obj.kcir);
  DISPOSE(obj.mark_pln); DISPOSE(obj.kpln);
  DISPOSE(obj.mark_arc); DISPOSE(obj.karc);
  DISPOSE(obj.mark_txt); DISPOSE(obj.ktxt);
  DISPOSE(obj.mark_grp); DISPOSE(obj.kgrp)
END unmark;


PROCEDURE _delete;

  PROCEDURE del_line;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_lin) DO
      WITH obj.mark_lin[i]^ DO bas.xshow_line(bas.cview^.model,line,bas.bckg) END;
      obj.rem_line(bas.cview^.model^.mbody,obj.mark_lin[i],obj.klin[i]);
      obj.mark_lin[i]:=NIL
    END;
    DISPOSE(obj.mark_lin); DISPOSE(obj.klin)
  END del_line;

  PROCEDURE del_circle;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_cir) DO
      WITH obj.mark_cir[i]^ DO bas.xshow_cir(bas.cview^.model,circ,bas.bckg) END;
      obj.rem_circ(bas.cview^.model^.mbody,obj.mark_cir[i],obj.kcir[i]);
      obj.mark_cir[i]:=NIL
    END;
    DISPOSE(obj.mark_cir); DISPOSE(obj.kcir)
  END del_circle;

  PROCEDURE del_pline;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_pln) DO
      WITH obj.mark_pln[i]^ DO bas.xshow_pline(bas.cview^.model,pline,bas.bckg) END;
      obj.rem_pline(bas.cview^.model^.mbody,obj.mark_pln[i],obj.kpln[i]);
      obj.mark_pln[i]:=NIL
    END;
    DISPOSE(obj.mark_pln); DISPOSE(obj.kpln)
  END del_pline;

  PROCEDURE del_arc;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_arc) DO
      WITH obj.mark_arc[i]^ DO bas.xshow_arc(bas.cview^.model,arc,bas.bckg) END;
      obj.rem_arc(bas.cview^.model^.mbody,obj.mark_arc[i],obj.karc[i]);
      obj.mark_arc[i]:=NIL
    END;
    DISPOSE(obj.mark_arc); DISPOSE(obj.karc)
  END del_arc;

  PROCEDURE del_txt;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_txt) DO
      WITH obj.mark_txt[i]^ DO bas.xshow_text(bas.cview^.model,text,bas.bckg) END;
      obj.rem_text(bas.cview^.model^.mbody,obj.mark_txt[i],obj.ktxt[i]);
      obj.mark_txt[i]:=NIL
    END;
    DISPOSE(obj.mark_txt); DISPOSE(obj.ktxt)
  END del_txt;

  PROCEDURE del_surf;
  BEGIN
  END del_surf;

  PROCEDURE del_grp;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_grp) DO
      WITH obj.mark_grp[i]^ DO bas.xshow_group(bas.cview^.model,group,bas.bckg) END;
      obj.rem_group(bas.cview^.model^.mbody,obj.mark_grp[i],obj.kgrp[i]);
      obj.mark_grp[i]:=NIL
    END;
    DISPOSE(obj.mark_grp);
    DISPOSE(obj.kgrp)
  END del_grp;

BEGIN
  del_line;
  del_circle;
  del_pline;
  del_arc;
  del_txt;
  del_surf;
  del_grp;
  new_mark
END _delete;

PROCEDURE delete;

PROCEDURE _query(): BOOLEAN;
BEGIN
  IF bas.ENG THEN
    RETURN bpm.query(220,200,'Delete?','Yes','No','y','n',TRUE,FALSE)
  ELSE
    RETURN  bpm.query(220,200,'Удалить?','Да','Нет','д','н',TRUE,FALSE)
  END
END _query;

BEGIN
  new_mark;
  IF NOT select() THEN RETURN END;
  IF _query() THEN _delete; RETURN  END;
  unmark
END delete;

PROCEDURE _modify;

  VAR dx,dy,dz: REAL;

  PROCEDURE rd_shift(): BOOLEAN;
    VAR top1,top2: def.VERTEX;
        s1,s2: ARRAY [0..31] OF CHAR;
          crs: bas.CURSOR;
  BEGIN
    IF bas.ENG THEN s1:= 'Source point:';   s2:= 'Destinator point:'
    ELSE            s1:= 'Исходная точка:'; s2:= 'Конечная точка:' END;
    IF bas.read_point(s1,bas.cross,bas.null_monitor,top1) THEN
      bas.set_marked(top1);
      IF bas.done THEN crs:= bas.cross_marked
      ELSE
        bas.b_s(bas.cview,top1,bas.x_old,bas.y_old);
        crs:=bas.cross_line
      END;
      IF bas.read_point(s2,crs,bas.null_monitor,top2) THEN
        bas.end_marked;
        WITH bas.cview^ DO
          dx:=top2[Xc]-top1[Xc];
          dy:=top2[Yc]-top1[Yc];
          dz:=top2[Zc]-top1[Zc];
          RETURN  TRUE
        END
      END
    END;
    RETURN FALSE
  END rd_shift;

VAR anglet,ca,sa: REAL;
           cturn: def.VERTEX;

  PROCEDURE rd_turn(): BOOLEAN;
    VAR top1,top2: def.VERTEX;
  BEGIN
    IF bas.read_angle(cturn,top1,top2) THEN
      anglet:= bmt.angle_(cturn,top1,top2);
      ca:= mth.cos(anglet);
      sa:= mth.sin(anglet);
      RETURN TRUE
    END;
    RETURN FALSE
  END rd_turn;

VAR K1,K2,K3,K4: REAL;
          mmode: INTEGER;

  PROCEDURE rd_mirror(): BOOLEAN;
    VAR top1,top2: def.VERTEX;
            s1,s2: ARRAY [0..31] OF CHAR;
            a,b,c: REAL;
  BEGIN
    IF bas.ENG THEN
      s1:= 'First point mirror line:'; s2:= 'Second point mirror line:'
    ELSE
      s1:= 'Начло оси симметрии:'; s2:= 'Конец оси симметрии:'
    END;
    IF bas.read_point(s1,bas.cross,bas.null_monitor,top1) THEN
      bas.b_s(bas.cview,top1,bas.x_old,bas.y_old);
      IF bas.read_point(s2,bas.cross_line,bas.null_monitor,top2) THEN
        WITH bas.cview^ DO
          a:= top1[Yc]-top2[Yc];
          b:= top2[Xc]-top1[Xc];
          c:= top1[Xc]*top2[Yc]-top1[Yc]*top2[Xc];
          IF (a=0.) & (b=0.) THEN RETURN FALSE END;
          IF b=0. THEN
            mmode:=0; K1:= top1[Xc]; RETURN TRUE
          ELSIF a=0. THEN
            mmode:=1; K1:= top1[Yc]; RETURN TRUE
          ELSE
            mmode:= 2;
            K1:= (a*a-b*b)/(a*a+b*b);
            K2:= (-2.*a*b)/(a*a+b*b);
            K3:= (-2.*b*c)/(a*a+b*b);
            K4:= a/b;
            RETURN TRUE
          END
        END
      END
    END;
    RETURN FALSE
  END rd_mirror;

  PROCEDURE rd_mod(): BOOLEAN;
  BEGIN
    CASE OPERATION OF
      0: RETURN rd_shift()
     |1: RETURN rd_turn()
     |2: RETURN rd_mirror()
    ELSE END
  END rd_mod;

  PROCEDURE mod_top(top: def.VERTEX; VAR TOP: def.VERTEX);

    PROCEDURE _shift;
    BEGIN
      WITH bas.cview^ DO
        TOP[Xc]:= top[Xc]+dx;
        TOP[Yc]:= top[Yc]+dy;
        TOP[Zc]:= top[Zc]+dz
      END
    END _shift;

    PROCEDURE _turn;
    BEGIN
      WITH bas.cview^ DO
        dx:= top[Xc]-cturn[Xc];
        dy:= top[Yc]-cturn[Yc];
        TOP[Xc]:=cturn[Xc]+dx*ca-dy*sa;
        TOP[Yc]:=cturn[Yc]+dx*sa+dy*ca;
        TOP[Zc]:= top[Zc]
      END
    END _turn;

    PROCEDURE _mirr;
    BEGIN
      WITH bas.cview^ DO
        TOP[Zc]:= top[Zc];
        CASE mmode OF
           0: TOP[Xc]:= K1+K1-top[Xc];
              TOP[Yc]:= top[Yc]
          |1: TOP[Yc]:= K1+K1-top[Yc];
              TOP[Xc]:= top[Xc]
          |2: TOP[Yc]:= K1*top[Yc]+ K2*top[Xc] +K3;
              TOP[Xc]:= K4*(TOP[Yc]-top[Yc])+ top[Xc]
        ELSE  END
      END
    END _mirr;

  BEGIN
    CASE OPERATION OF
       0: _shift
      |1: _turn
      |2: _mirr
    ELSE END
  END mod_top;

  PROCEDURE mod_lin(old: def.Line; VAR new: def.Line);
    VAR j: INTEGER;
  BEGIN
    WITH old DO
      mod_top(xyz,new.xyz);
      mod_top(XYZ,new.XYZ);
      NEW(new.pict,HIGH(pict)+1);
      FOR j:=0 TO HIGH(pict) DO
        mod_top(pict[j,0],new.pict[j,0]);
        mod_top(pict[j,1],new.pict[j,1])
      END;
      new.color:= color;
      new.ltyp:= ltyp
    END
  END mod_lin;

  PROCEDURE modif_line;
    VAR new: def.Line;
        i,j: INTEGER;
        clp: obj.Clip;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_lin) DO
      WITH obj.mark_lin[i]^ DO
        IF MODE THEN
          bas.show_line(bas.cview^.model,line);
          mod_lin(line,new);
          obj.new_line(bas.cview^.model^.mbody,new,obj.klin[i]);
          bas.extr_lin(bas.cview^.model,new);
          bas.show_line(bas.cview^.model,new);
          obj.dispose_line(new)
        ELSE
          bas.xshow_line(bas.cview^.model,obj.mark_lin[i]^.line,bas.bckg);
          mod_lin(line,line);
          obj.clp_lin(line,rect);
          bas.extr_lin(bas.cview^.model,line);
          bas.show_line(bas.cview^.model,line)
        END
      END
    END;
    DISPOSE(obj.mark_lin);
    DISPOSE(obj.klin)
  END modif_line;

  PROCEDURE mod_pic(old: def.PICTURE; VAR  new: def.PICTURE);
    VAR i,j: INTEGER;
  BEGIN
    NEW(new,HIGH(old)+1);
    FOR i:=0 TO HIGH(new) DO
      NEW(new[i],HIGH(old[i])+1);
      FOR j:=0 TO HIGH(new[i]) DO
        mod_top(old[i,j],new[i,j])
      END
    END
  END mod_pic;

  PROCEDURE mod_cir(old: def.Circ; VAR new: def.Circ);
  BEGIN
    WITH old DO
      mod_top(centr,new.centr);
      mod_pic(pict,new.pict);
      new.color:= color;
      new.ltyp:= ltyp;
      new.r:=r
    END;
  END mod_cir;

  PROCEDURE modif_circle;
    VAR new: def.Circ;
        i,j: INTEGER;
        clp: obj.Clip;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_cir) DO
      WITH obj.mark_cir[i]^ DO
        IF MODE THEN
          bas.show_cir(bas.cview^.model,circ);
          mod_cir(circ,new);
          obj.new_circ(bas.cview^.model^.mbody,new,obj.kcir[i]);
          bas.extr_cir(bas.cview^.model,new);
          bas.show_cir(bas.cview^.model,new);
          obj.dispose_circ(new)
        ELSE
          bas.xshow_cir(bas.cview^.model,circ,bas.bckg);
          mod_cir(circ,circ);
          bas.extr_cir(bas.cview^.model,circ);
          obj.clp_cir(circ,rect);
          bas.show_cir(bas.cview^.model,circ)
        END
      END
    END;
    DISPOSE(obj.mark_cir);
    DISPOSE(obj.kcir)
  END modif_circle;

  PROCEDURE mod_arc(old: def.Arc; VAR new: def.Arc);
  BEGIN
    WITH old DO
      mod_top(xyz1,new.xyz1);
      mod_top(xyz2,new.xyz2);
      mod_top(xyz3,new.xyz3);
      mod_pic(pict,new.pict);
      new.color:= color;
      new.ltyp:= ltyp
    END
  END mod_arc;

  PROCEDURE modif_arc;
    VAR new: def.Arc;
        i,j: INTEGER;
        clp: obj.Clip;

  BEGIN
    FOR i:=0 TO HIGH(obj.mark_arc) DO
      WITH obj.mark_arc[i]^ DO
        IF MODE THEN
          bas.show_arc(bas.cview^.model,arc);
          mod_arc(arc,new);
          bas.show_arc(bas.cview^.model,new);
          obj.new_arc(bas.cview^.model^.mbody,new,obj.karc[i]);
          bas.extr_arc(bas.cview^.model,new);
          obj.dispose_arc(new)
        ELSE
          bas.xshow_arc(bas.cview^.model,obj.mark_arc[i]^.arc,bas.bckg);
          mod_arc(arc,arc);
          bas.extr_arc(bas.cview^.model,arc);
          obj.clp_arc(arc,rect);
          bas.show_arc(bas.cview^.model,arc)
        END
      END
    END;
    DISPOSE(obj.mark_arc);
    DISPOSE(obj.karc)
  END modif_arc;

  PROCEDURE mod_pln(old: def.Pline; VAR new: def.Pline);
    VAR j: INTEGER;
  BEGIN
    WITH old DO
      NEW(new.body,HIGH(body)+1);
      FOR j:=0 TO HIGH(new.body) DO  mod_top(body[j],new.body[j]) END;
      mod_pic(pict,new.pict);
      new.color:= color;
      new.ltyp:= ltyp
    END
  END mod_pln;

  PROCEDURE modif_pln;
  VAR new: def.Pline;
      i,j: INTEGER;
      clp: obj.Clip;
  BEGIN
    FOR i:=0 TO HIGH(obj.mark_pln) DO
      WITH obj.mark_pln[i]^ DO
        IF MODE THEN
          bas.show_pline(bas.cview^.model,obj.mark_pln[i]^.pline);
          mod_pln(pline,new);
          obj.new_pline(bas.cview^.model^.mbody,new,obj.kpln[i]);
          bas.extr_pln(bas.cview^.model,new);
          bas.show_pline(bas.cview^.model,new);
          obj.dispose_pln(new)
        ELSE
          bas.xshow_pline(bas.cview^.model,obj.mark_pln[i]^.pline,bas.bckg);
          mod_pln(pline,pline);
          bas.extr_pln(bas.cview^.model,pline);
          obj.clp_pln(pline,rect);
          bas.show_pline(bas.cview^.model,pline)
        END
      END
    END;
    DISPOSE(obj.mark_pln);
    DISPOSE(obj.kpln)
  END modif_pln;

  PROCEDURE mod_txt(old: def.Text; VAR new: def.Text);
    VAR j :INTEGER;
  BEGIN
    WITH old DO
      NEW(new.pict,HIGH(pict)+1);
      NEW(new.stxt,HIGH(stxt)+1);
      FOR j:=0 TO HIGH(pict) DO
        mod_top(pict[j,0],new.pict[j,0]);
        mod_top(pict[j,1],new.pict[j,1])
      END;
      mod_top(xyz,new.xyz);
      new.stxt:=stxt;
      new.w:=w;
      new.h:=h;
      new.i:=i;
      new.a:=a+anglet;
      new.color:= color
    END
  END mod_txt;

  PROCEDURE modif_txt;
  VAR new: def.Text;
      clp: obj.Clip;
      n,k: INTEGER;
  BEGIN
    FOR n:=0 TO HIGH(obj.mark_txt) DO
      WITH obj.mark_txt[n]^ DO
        IF MODE THEN
          bas.show_text(bas.cview^.model,text);
          mod_txt(text,new);
          obj.new_text(bas.cview^.model^.mbody,new,obj.ktxt[n]);
          bas.extr_txt(bas.cview^.model,new);
          bas.show_text(bas.cview^.model,new);
          obj.dispose_txt(new)
        ELSE
          bas.xshow_text(bas.cview^.model,text,bas.bckg);
          mod_txt(text,text);
          obj.clp_txt(text,rect);
          bas.extr_txt(bas.cview^.model,text);
          bas.show_text(bas.cview^.model,text)
        END
      END
    END;
    DISPOSE(obj.mark_txt);
    DISPOSE(obj.ktxt)
  END modif_txt;

  PROCEDURE mod_ell(old: def.Ellips; VAR new: def.Ellips);
  BEGIN
  END mod_ell;

  PROCEDURE mod_srf(old: def.Surf; VAR new: def.Surf);
  BEGIN
  END mod_srf;

  PROCEDURE modif_grp;
    VAR new: def.Group;
        clp: obj.Clip;
        n,i: INTEGER;
  BEGIN
    FOR n:=0 TO HIGH(obj.mark_grp) DO
      WITH obj.mark_grp[n]^.group DO
        IF MODE THEN
          bas.show_group(bas.cview^.model,obj.mark_grp[n]^.group);
          NEW(new.line,LEN(line));
          FOR i:=0 TO HIGH(line) DO mod_lin(line[i],new.line[i]) END;
          NEW(new.circ,LEN(circ));
          FOR i:=0 TO HIGH(circ) DO mod_cir(circ[i],new.circ[i]) END;
          NEW(new.arc,LEN(arc));
          FOR i:=0 TO HIGH(arc)  DO mod_arc(arc[i],new.arc[i]) END;
          NEW(new.ellips,LEN(ellips));
          FOR i:=0 TO HIGH(ellips) DO mod_ell(ellips[i],new.ellips[i]) END;
          NEW(new.pline,LEN(pline));
          FOR i:=0 TO HIGH(pline) DO mod_pln(pline[i],new.pline[i]) END;
          NEW(new.txt,LEN(txt));
          FOR i:=0 TO HIGH(txt) DO mod_txt(txt[i],new.txt[i]) END;
          NEW(new.surf,LEN(surf));
          FOR i:=0 TO HIGH(surf) DO mod_srf(surf[i],new.surf[i]) END;
          obj.new_group(bas.cview^.model^.mbody,new,obj.kgrp[n]);
          bas.extr_grp(bas.cview^.model,new);
          bas.show_group(bas.cview^.model,new);
          obj.dispose_grp(new)
        ELSE
          bas.xshow_group(bas.cview^.model,obj.mark_grp[n]^.group,bas.bckg);
          FOR i:=0 TO HIGH(line)   DO mod_lin(line[i]  ,line[i])   END;
          FOR i:=0 TO HIGH(circ)   DO mod_cir(circ[i]  ,circ[i])   END;
          FOR i:=0 TO HIGH(arc)    DO mod_arc(arc[i]   ,arc[i])    END;
          FOR i:=0 TO HIGH(ellips) DO mod_ell(ellips[i],ellips[i]) END;
          FOR i:=0 TO HIGH(pline)  DO mod_pln(pline[i] ,pline[i])  END;
          FOR i:=0 TO HIGH(txt)    DO mod_txt(txt[i]   ,txt[i])    END;
          FOR i:=0 TO HIGH(surf)   DO mod_srf(surf[i]  ,surf[i])   END;
          bas.extr_grp(bas.cview^.model,obj.mark_grp[n]^.group);
          obj.clp_grp(obj.mark_grp[n]^.group,obj.mark_grp[n]^.rect);
          bas.show_group(bas.cview^.model,obj.mark_grp[n]^.group)
        END
      END
    END;
    DISPOSE(obj.mark_grp);
    DISPOSE(obj.kgrp)
  END modif_grp;

VAR  squery: ARRAY [0..15] OF CHAR;

PROCEDURE _query(): BOOLEAN;
BEGIN
 IF bas.ENG THEN
   RETURN bpm.query(220,200,'Delete old objects?','Yes','No','y','n',TRUE,FALSE)
 ELSE
   RETURN bpm.query(220,200,'Удалить старое?','Дa','Нет','д','н',TRUE,FALSE)
 END
END _query;

PROCEDURE _query1(): BOOLEAN;
BEGIN
  IF bas.ENG THEN
    IF MODE THEN squery:='Copy?' ELSE squery:='Move?' END;
    RETURN  bpm.query(220,200,squery,'Yes','No','y','n',TRUE,FALSE)
  ELSE
    IF MODE THEN squery:='Kопировать?' ELSE squery:='Передвигать?' END;
    RETURN  bpm.query(220,200,squery,'Да','Нет','д','н',TRUE,FALSE)
  END
END _query1;

BEGIN
  anglet:=0.;
  IF rd_mod() THEN
    IF OPERATION>0 THEN
      IF _query() THEN
      MODE:=MOVE ELSE MODE:=COPY END;
    END;
    IF _query1() THEN
      modif_line;
      modif_circle;
      modif_arc;
      modif_pln;
      modif_txt;
      modif_grp
    ELSE  unmark
    END
  ELSE unmark END
END _modify;

PROCEDURE copy;
BEGIN
  new_mark;
  IF NOT select() THEN RETURN END;
  MODE:=COPY;
  OPERATION:=SHIFT;
  _modify
END copy;

PROCEDURE shift;
BEGIN
  new_mark;
  IF NOT select() THEN RETURN END;
  MODE:=MOVE;
  OPERATION:=SHIFT;
  _modify
END shift;

PROCEDURE mirror;
BEGIN
  new_mark;
  IF NOT select() THEN RETURN END;
  OPERATION:=MIRROR;
  _modify
END mirror;

PROCEDURE turn;
BEGIN
  new_mark;
  OPERATION:=ROTATE;
  IF NOT select() THEN RETURN END;
  _modify
END turn;

----------------------------- ZOOM -----------------------------

PROCEDURE round_scale(x: REAL): REAL;
  VAR y: REAL;
BEGIN
  RETURN x;
(*
  IF x<1. THEN  RETURN 1./round_scale(1./x) END;
  IF x< 3. THEN
    y:= FLOAT(TRUNC(x));
    IF    (x-y)<0.25 THEN RETURN y
    ELSIF (x-y)<0.75 THEN RETURN y+0.5
    ELSE RETURN  y+1.
    END
  ELSIF x<20. THEN
    y:= FLOAT(TRUNC(x));
    IF    (x-y)<0.5 THEN RETURN y
    ELSE RETURN  y+1.
    END
  ELSE
    RETURN  FLOAT(mth.round(x/5.))*5.
  END
*)
END round_scale;

PROCEDURE scale;
  VAR s1,s2,s3: ARRAY [0..31] OF CHAR;
           top: def.VERTEX;
             s: REAL;
BEGIN
  IF bas.ENG THEN  s1:='Scale:'; s2:='Max scale'; s3:='Min scale'
  ELSE s1:='Масштаб:'; s2:='Максимальный масштаб'; s3:='Минимальный масштаб' END;
  WITH bas.cview^ DO
    s:=scale;
    IF bas.rd_numb1(s1,s,bas.SW DIV 2,bas.SH * 3 DIV 4) THEN
      bas.s_b(bas.cview,wind^.inner.clip.w DIV 2,wind^.inner.clip.h DIV 2,top);
      IF s > cntx^.max_scale THEN
        bpm.message(240,180,'%s %f !',s2,cntx^.max_scale);
        scale:= cntx^.max_scale
      ELSIF  s<(0.5/bas.Max) THEN
        scale:=0.5/bas.Max;
        bpm.message(240,180,'%s %f !',s3,scale);
        bas.s_b(bas.cview,wind^.inner.clip.w DIV 2,wind^.inner.clip.h DIV 2,top);
      ELSE   scale:= round_scale(s) END;
      X_lf:= top[Xc] - FLOAT(wind^.inner.clip.w DIV 2) / scale;
      Y_dw:= top[Yc] - FLOAT(wind^.inner.clip.h DIV 2) / scale;
      bas.vredraw(bas.cview);
      bas.step
    END
  END
END scale;

PROCEDURE zoom_w;
  VAR s,lx,ly,xl,yd,cs: REAL;
             top1,top2: def.VERTEX;
             st1,st2: ARRAY [0..31] OF CHAR;
BEGIN
  IF bas.ENG THEN st1:='First corner:'; st2:='Second corner:'
  ELSE            st1:='Первый угол:';  st2:='Второй угол:'   END;
  s:=0.;
  WITH bas.cview^ DO
    IF bas.read_point(st1,bas.cross,bas.null_monitor,top1) THEN
      bas.b_s(bas.cview,top1,bas.x_old,bas.y_old);
      IF bas.read_point(st2,bas.cross_box,bas.null_monitor,top2) THEN
        cs:=FLOAT(wind^.inner.clip.h)/ FLOAT(wind^.inner.clip.w);
        lx:=ABS(top1[Xc]- top2[Xc]);
        ly:=ABS(top1[Yc]- top2[Yc]);
        xl:=min(top1[Xc],top2[Xc]);
        yd:=min(top1[Yc],top2[Yc]);
        IF (top1[Xc] # top2[Xc]) & (top1[Yc] # top2[Yc]) THEN
          s:= mth.min(FLOAT(tool.clip.h)/ly,FLOAT(tool.clip.w)/lx)
        END
      ELSE RETURN END
    ELSE RETURN END;
    IF s < cntx^.max_scale THEN
      IF (xl> -bas.Max) & (yd> -bas.Max) & (xl+lx< bas.Max) & (yd+ly< bas.Max)
      THEN
        X_lf:=xl; Y_dw:=yd;
        IF s # scale THEN
           scale:= round_scale(s);
           bas.vredraw(bas.cview);
           bas.step
        END
      ELSE bpm.message(240,180,'Max size work area < %f ',bas.Max) END
    ELSE scale:= cntx^.max_scale END
  END
END zoom_w;

PROCEDURE zoom_a;
  VAR dx,dy,cs: REAL;
BEGIN
  WITH bas.cview^ DO
    WITH model^.area DO
      dx:=X-x;  dy:=Y-y;
      IF (dx <= -2.*bas.Max) OR (dy<= -2.*bas.Max) THEN RETURN END;
      X_lf:= x; Y_dw:= y;
    END;
    scale:= round_scale(mth.min(FLOAT(tool.clip.h)/dy,FLOAT(tool.clip.w)/dx))
  END;
  bas.vredraw(bas.cview);
  bas.step
END zoom_a;

PROCEDURE zoom_d; BEGIN zoom_a; zoom_w END zoom_d;

PROCEDURE center;
  VAR top: def.VERTEX;
        s: ARRAY [0..63] OF CHAR;
BEGIN
  WITH bas.cview^ DO
    bas.s_b(bas.cview,wind^.inner.clip.w DIV 2,wind^.inner.clip.h DIV 2,top);
    IF bas.ENG THEN
      str.print(s,'Center screen <%.2f,%.2f>:',top[Xc],top[Yc])
    ELSE
       str.print(s,'Центр экрана <%.2f,%.2f>:',top[Xc],top[Yc])
    END;
    IF bas.read_point(s,bas.cross,bas.null_monitor,top) THEN
      X_lf:= top[Xc] - FLOAT(wind^.inner.clip.w DIV 2) / scale;
      Y_dw:= top[Yc] - FLOAT(wind^.inner.clip.h DIV 2) / scale;
      bas.vredraw(bas.cview)
    END
  END;
END center;

----------------------------- GROUP ----------------------------

PROCEDURE _make_grp(VAR new: def.Group; VAR clip: obj.Clip);
  VAR j: INTEGER;
BEGIN
  WITH clip DO x:=mth.round(bas.Max); y:=x; X:=- x; Y:=-y END;
  WITH new DO
    NEW(line,HIGH(obj.mark_lin)+1);
    FOR j:=0 TO HIGH(line) DO
      obj.fuse_clp(obj.mark_lin[j]^.rect,clip);
      WITH line[j] DO
        xyz  := obj.mark_lin[j]^.line.xyz;
        XYZ  := obj.mark_lin[j]^.line.XYZ;
        color:= obj.mark_lin[j]^.line.color;
        ltyp := obj.mark_lin[j]^.line.ltyp;
        obj.copy_vpict(pict,obj.mark_lin[j]^.line.pict)
      END
    END;
    NEW(circ,HIGH(obj.mark_cir)+1);
    FOR j:=0 TO HIGH(circ) DO
      obj.fuse_clp(obj.mark_cir[j]^.rect,clip);
      WITH circ[j] DO
        centr:= obj.mark_cir[j]^.circ.centr;
        r    := obj.mark_cir[j]^.circ.r;
        color:= obj.mark_cir[j]^.circ.color;
        ltyp := obj.mark_cir[j]^.circ.ltyp;
        obj.copy_pict(pict,obj.mark_cir[j]^.circ.pict)
      END
    END;
    NEW(arc,HIGH(obj.mark_arc)+1);
    FOR j:=0 TO HIGH(arc) DO
      obj.fuse_clp(obj.mark_arc[j]^.rect,clip);
      WITH arc[j] DO
        xyz1:=  obj.mark_arc[j]^.arc.xyz1;
        xyz2:=  obj.mark_arc[j]^.arc.xyz2;
        xyz3:=  obj.mark_arc[j]^.arc.xyz3;
        color:= obj.mark_arc[j]^.arc.color;
        ltyp := obj.mark_arc[j]^.arc.ltyp;
        obj.copy_pict(pict,obj.mark_arc[j]^.arc.pict)
      END
    END;
    NEW(ellips,HIGH(obj.mark_ell)+1);
    FOR j:=0 TO HIGH(ellips) DO
      obj.fuse_clp(obj.mark_ell[j]^.rect,clip);
      WITH ellips[j] DO
        centr:= obj.mark_ell[j]^.ellips.centr;
        a:=     obj.mark_ell[j]^.ellips.a;
        b:=     obj.mark_ell[j]^.ellips.b;
        angle:= obj.mark_ell[j]^.ellips.angle;
        color:= obj.mark_ell[j]^.ellips.color;
        ltyp := obj.mark_ell[j]^.ellips.ltyp;
        obj.copy_pict(pict,obj.mark_ell[j]^.ellips.pict)
      END
    END;
    NEW(pline,HIGH(obj.mark_pln)+1);
    FOR j:=0 TO HIGH(pline) DO
      obj.fuse_clp(obj.mark_pln[j]^.rect,clip);
      WITH pline[j] DO
        NEW(body,HIGH(obj.mark_pln[j]^.pline.body)+1);
        body:=  obj.mark_pln[j]^.pline.body;
        type:=  obj.mark_pln[j]^.pline.type;
        color:= obj.mark_pln[j]^.pline.color;
        ltyp := obj.mark_pln[j]^.pline.ltyp;
        obj.copy_pict(pict,obj.mark_pln[j]^.pline.pict)
      END
    END;
    NEW(txt,HIGH(obj.mark_txt)+1);
    FOR j:=0 TO HIGH(txt) DO
      obj.fuse_clp(obj.mark_txt[j]^.rect,clip);
      WITH txt[j] DO
        NEW(stxt,HIGH(obj.mark_txt[j]^.text.stxt)+1);
        NEW(pict,HIGH(obj.mark_txt[j]^.text.pict)+1);
        stxt := obj.mark_txt[j]^.text.stxt;
        pict := obj.mark_txt[j]^.text.pict;
        xyz  := obj.mark_txt[j]^.text.xyz;
        color:= obj.mark_txt[j]^.text.color;
        w:= obj.mark_txt[j]^.text.w;
        h:= obj.mark_txt[j]^.text.h;
        a:= obj.mark_txt[j]^.text.a;
        i:= obj.mark_txt[j]^.text.i
      END
    END;
    NEW(surf,HIGH(obj.mark_srf)+1);
    FOR j:= 0 TO HIGH(surf) DO
      obj.fuse_clp(obj.mark_srf[j]^.rect,clip);
      surf[j]:= obj.mark_srf[j]^.surf
    END
  END
END _make_grp;

PROCEDURE make_group;
  VAR new: def.Group;
        c: obj.Clip;

  PROCEDURE _query(): BOOLEAN;
  BEGIN
    IF bas.ENG THEN
      RETURN bpm.query(220,200,'Make group?','Yes','No','y','n',TRUE,FALSE)
    ELSE
      RETURN  bpm.query(220,200,'Собрать группу?','Да','Нет','д','н',TRUE,FALSE)
    END
  END _query;

BEGIN
  new_mark;
  IF NOT select() THEN RETURN END;
  IF NOT _query() THEN unmark; RETURN END;
  _make_grp(new,c);
  obj.new_group(bas.cview^.model^.mbody,new,bas.cview^.cntx^.work);
  RESIZE(obj.mark_grp,0); RESIZE(obj.kgrp,0);
  _delete;
  bas.show_group(bas.cview^.model,new);
  obj.dispose_grp(new)
END make_group;

PROCEDURE select_grp(): BOOLEAN;
 VAR top: def.VERTEX;
       s: BOOLEAN;
       i: INTEGER;
BEGIN
  WITH bas.cview^ DO
    s:= cntx^.STEP; cntx^.STEP:=FALSE;
    DST:= 3./scale;
    new_mark;
    LOOP
      IF bas.read_point('Select group:',bas.marker,bas.null_monitor,top) THEN
      FOR i:=0 TO HIGH(model^.mbody^) DO
        IF cntx^.mask[i] # 0 THEN search_grp(i,top) END
      END
      ELSE cntx^.STEP:=s; bas.step; RETURN HIGH(obj.mark_grp)>=0 END
    END
  END
END select_grp;

PROCEDURE _remake_group(VAR grp: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line) DO
      obj.new_line(bas.cview^.model^.mbody,line[i],bas.cview^.cntx^.work);
      bas.show_line(bas.cview^.model,line[i])
    END;
    FOR i:=0 TO HIGH(circ) DO
      obj.new_circ(bas.cview^.model^.mbody,circ[i],bas.cview^.cntx^.work);
      bas.show_cir(bas.cview^.model,circ[i])
    END;
    FOR i:=0 TO HIGH(arc) DO
      obj.new_arc(bas.cview^.model^.mbody,arc[i],bas.cview^.cntx^.work);
      bas.show_arc(bas.cview^.model,arc[i])
    END;
    FOR i:=0 TO HIGH(pline) DO
      obj.new_pline(bas.cview^.model^.mbody,pline[i],bas.cview^.cntx^.work);
      bas.show_pline(bas.cview^.model,pline[i])
    END;
    FOR i:=0 TO HIGH(txt) DO
      obj.new_text(bas.cview^.model^.mbody,txt[i],bas.cview^.cntx^.work);
      bas.show_text(bas.cview^.model,txt[i])
    END;
    FOR i:=0 TO HIGH(surf) DO
      obj.new_surf(bas.cview^.model^.mbody,surf[i],bas.cview^.cntx^.work);
      bas.show_surf(bas.cview^.model,surf[i])
    END
  END
END _remake_group;

PROCEDURE dest_group;
  VAR i: INTEGER;

  PROCEDURE _query(): BOOLEAN;
  BEGIN
    IF bas.ENG THEN
      RETURN bpm.query(220,200,'Remake group?','Yes','No','y','n',TRUE,FALSE)
    ELSE
      RETURN  bpm.query(220,200,'Разобрать группу?','Да','Нет','д','н',TRUE,FALSE)
    END
  END _query;

BEGIN
  IF NOT select_grp() THEN RETURN END;
  IF NOT _query() THEN  unmark; RETURN END;
  FOR i:=0 TO HIGH(obj.mark_grp) DO
    _remake_group(obj.mark_grp[i]^.group);
    obj.rem_group(bas.cview^.model^.mbody,obj.mark_grp[i],obj.kgrp[i]);
    obj.mark_grp[i]:= NIL;
  END;
  DISPOSE(obj.mark_grp);
  DISPOSE(obj.kgrp)
END dest_group;

------------------------- EDIT POLYLINE ------------------------


PROCEDURE search_1pln(i: INTEGER; top: def.VERTEX);
  VAR pln: obj.pln_ptr;
BEGIN
  WITH bas.cview^.model^.mbody^[i]^ DO
    pln:= plins;
    IF pln#NIL THEN
      REPEAT
        IF pln?(top,plins^.pline) THEN
          NEW(obj.mark_pln,1);
          NEW(obj.kpln,1);
          obj.mark_pln[0]:=plins;
          obj.kpln[0]:=i;
          bas.xshow_pline(bas.cview^.model,plins^.pline,bas.mark);
          RETURN
        END;
        plins:=plins^.next;
      UNTIL (pln = plins)
    END
  END;
END search_1pln;

PROCEDURE markp(top: def.VERTEX): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  unmark;
  DST:= 3./bas.cview^.scale;
  FOR i:=0 TO HIGH(bas.cview^.model^.mbody^) DO
    IF bas.cview^.cntx^.mask[i]#0 THEN search_1pln(i,top) END
  END;
  RETURN  HIGH(obj.mark_pln) = 0
END markp;

PROCEDURE spline_pln;
  VAR x,y,px,py,l: DYNARR OF REAL;
            L,X,Y: REAL;
            i,h,H: INTEGER;
BEGIN
  WITH obj.mark_pln[0]^.pline DO
    IF type=1 THEN RETURN END;
    h:=HIGH(body);
    NEW(l,h+1); NEW(px,h+1); NEW(py,h+1);
    NEW(x,h+1); NEW(y,h+1);
    FOR i:=0 TO h DO
      x[i]:=body[i,bas.cview^.Xc]; y[i]:=body[i,bas.cview^.Yc]
    END;
    bmt.long(x,y,l);
    bmt.t_pro(x,y,l,px,py);
    bas.xshow_pline(bas.cview^.model,obj.mark_pln[0]^.pline,bas.bckg);
    H:=TRUNC(l[h]/dft.step_p);
    NEW(pict,1);
    NEW(pict[0],H+1);
    pict[0,0]:=body[0];
    pict[0,H]:=body[h];
    L:=0.;
    FOR i:=0 TO H DO
      bmt.t_splin(x,y,px,py,l,L,X,Y);
      pict[0,i,bas.cview^.Xc]:=X;
      pict[0,i,bas.cview^.Yc]:=Y;
      L:=L+dft.step_p
    END;
    IF HIGH(dft.patts[dft.c_ltype])>0 THEN
      dft.make_patt(pict[0],dft.patts[dft.c_ltype],pict)
    END;
    type:=1;
    bas.xshow_pline(bas.cview^.model,obj.mark_pln[0]^.pline,bas.mark)
  END
END spline_pln;

PROCEDURE normal_pln;
  VAR i: INTEGER;
BEGIN
  WITH obj.mark_pln[0]^ DO
    bas.xshow_pline(bas.cview^.model,pline,bas.bckg);
    WITH pline DO
      IF type=0 THEN RETURN END;
      FOR i:=0 TO HIGH(pict) DO NEW(pict[i]) END;
      NEW(pict);
      IF HIGH(dft.patts[dft.c_ltype])>0 THEN
        dft.make_patt(body,dft.patts[dft.c_ltype],pict)
      END;
      type:=0;
    END;
    bas.xshow_pline(bas.cview^.model,pline,bas.mark)
  END
END normal_pln;

PROCEDURE move_vertex;
BEGIN
END move_vertex;

PROCEDURE insert_vertex;
BEGIN
END insert_vertex;

PROCEDURE delete_vertex;
BEGIN
END delete_vertex;

PROCEDURE ed_pline;
  VAR ted: bpm.TABLET;
       top: def.VERTEX;
        he: INTEGER;
         s: BOOLEAN;
BEGIN
  WITH bas.cview^ DO
    s:= cntx^.STEP; cntx^.STEP:=FALSE;
    LOOP
      IF bas.read_point('Select object:',bas.marker,bas.null_monitor,top) THEN
        IF markp(top) THEN cntx^.STEP:= s; bas.step; EXIT END
      ELSE cntx^.STEP:=s; bas.step; RETURN END
    END
  END;
  he:=bpm.font^.H+5;
  IF bas.ENG THEN
    bpm.tnew(ted,1,5,0,359-he*6,120,he,bpm.xsel,' Edit pline');
    bpm.tprint(ted,0,'Move  vertex   ');  bpm.thotkey(ted,0,"m",TRUE);
    bpm.tprint(ted,1,'Insert vertex  ');  bpm.thotkey(ted,1,"i",TRUE);
    bpm.tprint(ted,2,'Delete vertex  ');  bpm.thotkey(ted,2,"d",TRUE);
    bpm.tprint(ted,3,'Splin  polyline');  bpm.thotkey(ted,3,"s",TRUE);
    bpm.tprint(ted,4,'normal Polyline');  bpm.thotkey(ted,4,"p",TRUE);
  ELSE
    bpm.tnew(ted,1,5,0,359-he*6,150,he,bpm.xsel,' Редактор ломаных');
    bpm.tprint(ted,0,'Передвинуть вершину ');  bpm.thotkey(ted,0,"п",TRUE);
    bpm.tprint(ted,1,'Вставить вершину    ');  bpm.thotkey(ted,1,"в",TRUE);
    bpm.tprint(ted,2,'Удалить  вершину    ');  bpm.thotkey(ted,2,"у",TRUE);
    bpm.tprint(ted,3,'Изогнуть ломаную    ');  bpm.thotkey(ted,3,"и",TRUE);
    bpm.tprint(ted,4,'Нормальная ломаная  ');  bpm.thotkey(ted,4,"н",TRUE);
  END;
  bpm.topen(ted);
  LOOP
    bpm.tselect(ted);
    IF NOT bpm.tselected(ted) THEN EXIT END;
    CASE bpm.talt(ted) OF
       0: bpm.tclose(ted); move_vertex;    bpm.topen(ted); bpm.tunselect(ted)
      |1: bpm.tclose(ted); insert_vertex;  bpm.topen(ted); bpm.tunselect(ted)
      |2: bpm.tclose(ted); delete_vertex;  bpm.topen(ted); bpm.tunselect(ted)
      |3: bpm.tclose(ted); spline_pln;     bpm.topen(ted); bpm.tunselect(ted)
      |4: bpm.tclose(ted); normal_pln;     bpm.topen(ted); bpm.tunselect(ted)
    ELSE END
  END;
  bpm.tdispose(ted);
  WITH obj.mark_pln[0]^ DO
    obj.clp_pln(pline,rect); bas.show_pline(bas.cview^.model,pline)
  END;
  DISPOSE(obj.mark_pln);
  DISPOSE(obj.kpln)
END ed_pline;

PROCEDURE init;
BEGIN
  new_mark;
  OPERATION:=SHIFT;
  MODE:=COPY;
END init;

BEGIN
  init
END bcTool.
