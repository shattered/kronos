IMPLEMENTATION MODULE bcObj; (*$X+  brd 07-Dec-90. (c) KRONOS *)

IMPORT  def: bcDef;
IMPORT  err: defErrors;
IMPORT  SYSTEM;
IMPORT  mem: Heap;

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

---------------------------- Dispose ---------------------------

PROCEDURE dispose_line(VAR l: def.Line);
BEGIN  DISPOSE(l.pict) END dispose_line;

PROCEDURE pdispose(VAR pict: def.PICTURE);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(pict)DO DISPOSE(pict[i]) END; DISPOSE(pict)
END pdispose;

PROCEDURE vdispose(VAR pict: def.VPICTURE); BEGIN DISPOSE(pict) END vdispose;

PROCEDURE dispose_circ(VAR c: def.Circ);
BEGIN pdispose(c.pict) END dispose_circ;

PROCEDURE dispose_arc(VAR  a: def.Arc);
BEGIN pdispose(a.pict) END dispose_arc;

PROCEDURE dispose_ell(VAR e: def.Ellips);
BEGIN pdispose(e.pict) END dispose_ell;

PROCEDURE dispose_pln(VAR p: def.Pline);
BEGIN   DISPOSE(p.body); pdispose(p.pict) END dispose_pln;

PROCEDURE dispose_txt(VAR t: def.Text);
BEGIN DISPOSE(t.stxt); DISPOSE(t.pict) END dispose_txt;

PROCEDURE dispose_surf(VAR s: def.Surf);
BEGIN
  (* unreleased  *)

END dispose_surf;

PROCEDURE dispose_grp(VAR g: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH g DO
    FOR i:=0 TO HIGH(line)   DO  dispose_line(line[i])  END; DISPOSE(line);
    FOR i:=0 TO HIGH(circ)   DO  dispose_circ(circ[i])  END; DISPOSE(circ);
    FOR i:=0 TO HIGH(arc)    DO  dispose_arc (arc[i] )  END; DISPOSE(arc);
    FOR i:=0 TO HIGH(ellips) DO  dispose_ell(ellips[i]) END; DISPOSE(ellips);
    FOR i:=0 TO HIGH(pline)  DO  dispose_pln(pline[i])  END; DISPOSE(pline);
    FOR i:=0 TO HIGH(txt)    DO  dispose_txt(txt[i])    END; DISPOSE(txt);
    FOR i:=0 TO HIGH(surf)   DO  dispose_surf(surf[i])  END; DISPOSE(surf);
  END
END dispose_grp;

PROCEDURE dispose_mrk;
 VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(mark_lin) DO
    dispose_line(mark_lin[i]^.line);
    DISPOSE(mark_lin[i])
  END;
  FOR i:=0 TO HIGH(mark_cir) DO
    dispose_circ(mark_cir[i]^.circ);
    DISPOSE(mark_cir[i])
  END;
  FOR i:=0 TO HIGH(mark_arc) DO
    dispose_arc(mark_arc[i]^.arc);
    DISPOSE(mark_arc[i])
  END;
  FOR i:=0 TO HIGH(mark_lin) DO
    dispose_pln(mark_pln[i]^.pline);
    DISPOSE(mark_pln[i])
  END;
  FOR i:=0 TO HIGH(mark_ell) DO
    dispose_ell(mark_ell[i]^.ellips);
    DISPOSE(mark_ell[i])
  END;
  FOR i:=0 TO HIGH(mark_txt) DO
    dispose_txt(mark_txt[i]^.text);
    DISPOSE(mark_txt[i])
  END;
  FOR i:=0 TO HIGH(mark_srf) DO
    dispose_surf(mark_srf[i]^.surf);
    DISPOSE(mark_srf[i])
  END;
  FOR i:=0 TO HIGH(mark_grp) DO
    dispose_grp(mark_grp[i]^.group);
    DISPOSE(mark_grp[i])
  END;
  DISPOSE(mark_lin); DISPOSE(klin);
  DISPOSE(mark_cir); DISPOSE(kcir);
  DISPOSE(mark_pln); DISPOSE(kpln);
  DISPOSE(mark_arc); DISPOSE(karc);
  DISPOSE(mark_txt); DISPOSE(ktxt);
  DISPOSE(mark_srf); DISPOSE(ksrf);
  DISPOSE(mark_grp); DISPOSE(kgrp)
END dispose_mrk;

----------------------------- Clip -----------------------------

PROCEDURE _clp_vct(t1,t2: def.VERTEX; VAR clip: Clip);
 VAR x1,y1,x2,y2: INTEGER;
BEGIN
  x1:=TRUNC(t1[0]);
  y1:=TRUNC(t1[1]);
  x2:=TRUNC(t2[0]);
  y2:=TRUNC(t2[1]);
  WITH clip DO
   IF x1<x2 THEN x:=x1; X:=x2 ELSE x:=x2; X:=x1 END;
   IF y1<y2 THEN y:=y1; Y:=y2 ELSE y:=y2; Y:=y1 END
  END
END _clp_vct;

PROCEDURE clp_vct(vct: def.VECTOR; VAR clip: Clip);
BEGIN _clp_vct(vct[0],vct[1],clip) END clp_vct;

PROCEDURE clp_lin(line: def.Line;  VAR clip: Clip);
BEGIN WITH line DO _clp_vct(xyz,XYZ,clip) END END clp_lin;

PROCEDURE clp_cir( circ: def.Circ;  VAR clip: Clip);
  VAR x0,y0,R: INTEGER;
BEGIN
  x0:= TRUNC(circ.centr[0]);
  y0:= TRUNC(circ.centr[1]);
  R := TRUNC(circ.r);
  WITH clip DO x:= x0-R; y:=y0-R; X:=x0+R; Y:=y0+R END
END clp_cir;

PROCEDURE fuse_clp(clp: Clip; VAR clip: Clip);
BEGIN
  WITH clp DO
    IF x < clip.x THEN clip.x:=x END;
    IF X > clip.X THEN clip.X:=X END;
    IF y < clip.y THEN clip.y:=y END;
    IF Y > clip.Y THEN clip.Y:=Y END
  END
END fuse_clp;

PROCEDURE clp_ell(ell: def.Ellips; VAR clip: Clip);
BEGIN
  clip:= null_clip;
END clp_ell;

PROCEDURE clp_arc(arc: def.Arc; VAR clip: Clip);
  VAR tmp: Clip;
BEGIN
  clip:= null_clip;
  WITH arc DO
    _clp_vct(xyz1,xyz2,tmp );
    _clp_vct(xyz1,xyz2,clip);
    fuse_clp(tmp,clip)
  END
END clp_arc;

PROCEDURE clp_pic(pict: def.PICTURE; VAR clip: Clip);
  VAR i,j: INTEGER;
      tmp: Clip;
BEGIN
  clip:= null_clip;
  FOR i:=0 TO HIGH(pict) DO
    FOR j:=1 TO HIGH(pict[i]) DO
      _clp_vct(pict[i,j],pict[i,j-1],tmp);
      fuse_clp(tmp,clip)
    END
  END
END clp_pic;

PROCEDURE clp_pln(pline: def.Pline; VAR clip: Clip);
 VAR tmp: Clip;
      i :INTEGER;
BEGIN
  clip:= null_clip;
  WITH pline DO
    IF    HIGH(body) < 0 THEN RETURN
    ELSE
      _clp_vct(body[0],body[1],clip);
      FOR i:=2 TO HIGH(body) DO
        _clp_vct(body[i-1],body[i],tmp);
        fuse_clp(tmp,clip)
      END
    END
  END
END clp_pln;

PROCEDURE clp_vpc(p: def.VPICTURE; VAR clip: Clip);
  VAR  tmp: Clip;
         i: INTEGER;
BEGIN
  clip:= null_clip;
  IF HIGH(p) < 0 THEN RETURN
  ELSE
    _clp_vct(p[0,0],p[0,1],clip);
    FOR i:=1 TO HIGH(p) DO
      _clp_vct(p[i,0],p[i,1],tmp);
      fuse_clp(tmp,clip)
    END
  END
END clp_vpc;

PROCEDURE clp_txt(txt: def.Text; VAR clip: Clip);
 VAR tmp: Clip;
       l: INTEGER;
BEGIN
  clip:= null_clip;
  WITH txt DO
    IF HIGH(pict)<0 THEN RETURN END;
    clp_vct(pict[0],clip);
    FOR l:=1 TO HIGH(pict) DO
      clp_vct(pict[l],tmp);
      fuse_clp(tmp,clip)
    END
  END
END clp_txt;

PROCEDURE clp_surf(surf: def.Surf; VAR clip: Clip);
BEGIN
  clip:= null_clip;
END clp_surf;

PROCEDURE clp_grp(grp: def.Group; VAR clip: Clip);
  VAR tmp: Clip;
        i: INTEGER;
BEGIN
  clip:= null_clip;
  tmp:= clip;
  WITH grp DO
    FOR i:=0 TO HIGH(line) DO
      clp_lin(line[i],tmp); fuse_clp(tmp,clip)
    END;
    FOR i:=0 TO HIGH(circ) DO
      clp_cir(circ[i],tmp); fuse_clp(tmp,clip)
    END;
    FOR i:=0 TO HIGH(pline) DO
      clp_pln(pline[i],tmp); fuse_clp(tmp,clip)
    END;
    FOR i:=0 TO HIGH(txt) DO
      clp_txt(txt[i],tmp); fuse_clp(tmp,clip)
    END;
    FOR i:=0 TO HIGH(arc) DO
      clp_arc(arc[i],tmp); fuse_clp(tmp,clip)
    END
  END
END clp_grp;

--------------------------- Inserting --------------------------

PROCEDURE new_line(model: Model; lin: def.Line; lay: INTEGER);
  VAR new: lin_ptr;
        i: INTEGER;
BEGIN
  NEW(new); IF NOT done THEN RETURN END;
  WITH new^.line DO
    xyz:= lin.xyz;
    XYZ:= lin.XYZ;
    color:= lin.color;
    ltyp := lin.ltyp;
    NEW(pict,HIGH(lin.pict)+1);
    IF NOT done THEN DISPOSE(new); RETURN END;
    FOR i:=0 TO HIGH(pict) DO pict[i]:= lin.pict[i] END
  END;
  clp_lin(new^.line,new^.rect);
  IF model^[lay]^.lines = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.lines:=new
  ELSE
    new^.next:=model^[lay]^.lines^.next;
    new^.prev:=model^[lay]^.lines;
    model^[lay]^.lines^.next^.prev:=new;
    model^[lay]^.lines^.next:=new;
    model^[lay]^.lines:=new
  END
END new_line;

PROCEDURE copy_pict(VAR dst: def.PICTURE; sou: def.PICTURE);
  VAR i,j: INTEGER;
BEGIN
  IF HIGH(sou) < 0 THEN NEW(dst); RETURN END;
  NEW(dst,HIGH(sou)+1);
  IF NOT done THEN RETURN END;
  FOR i:=0 TO HIGH(sou) DO
    NEW(dst[i],HIGH(sou[i])+1);
    IF NOT done THEN
      FOR j:=0 TO i DO DISPOSE(dst[j]) END;
      DISPOSE(dst); RETURN
    END;
    FOR j:=0 TO HIGH(dst[i]) DO dst[i,j]:= sou[i,j] END
  END
END copy_pict;

PROCEDURE copy_vpict(VAR dst: def.VPICTURE; sou: def.VPICTURE);
  VAR i: INTEGER;
BEGIN
  IF HIGH(sou) < 0 THEN NEW(dst); RETURN END;
  NEW(dst,HIGH(sou)+1);
  IF NOT done THEN DISPOSE(dst); RETURN END;
  FOR i:=0 TO HIGH(dst) DO dst[i]:= sou[i] END
END copy_vpict;

PROCEDURE new_circ(model: Model; cir: def.Circ; lay: INTEGER);
  VAR new: cir_ptr;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  WITH new^.circ DO
    centr:= cir.centr;
    r:= cir.r;
    color:= cir.color;
    ltyp := cir.ltyp;
    copy_pict(pict,cir.pict);
    IF NOT done THEN DISPOSE(new); RETURN END
  END;
  clp_cir(new^.circ,new^.rect);
  IF model^[lay]^.circs = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.circs:=new;
  ELSE
    new^.next:=model^[lay]^.circs^.next;
    new^.prev:=model^[lay]^.circs;
    model^[lay]^.circs^.next^.prev:=new;
    model^[lay]^.circs^.next:=new;
    model^[lay]^.circs:=new;
  END
END new_circ;

PROCEDURE new_arc(model: Model; arc: def.Arc; lay: INTEGER);
  VAR new: arc_ptr;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  WITH new^.arc DO
    xyz1:= arc.xyz1;
    xyz2:= arc.xyz2;
    xyz3:= arc.xyz3;
    color:= arc.color;
    ltyp := arc.ltyp;
    copy_pict(pict,arc.pict);
    IF NOT done THEN DISPOSE(new); RETURN END
  END;
  clp_arc(new^.arc,new^.rect);
  IF model^[lay]^.arcs = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.arcs:=new
  ELSE
    new^.next:=model^[lay]^.arcs^.next;
    new^.prev:=model^[lay]^.arcs;
    model^[lay]^.arcs^.next^.prev:=new;
    model^[lay]^.arcs^.next:=new;
    model^[lay]^.arcs:=new
  END
END new_arc;

PROCEDURE new_pline(model: Model; pln: def.Pline; lay: INTEGER);
  VAR new: pln_ptr;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  WITH new^.pline DO
    NEW(body,HIGH(pln.body)+1);
    IF NOT done THEN DISPOSE(new); RETURN END;
    body:= pln.body;
    type:= pln.type;
    color:= pln.color;
    ltyp := pln.ltyp;
    copy_pict(pict,pln.pict);
    IF NOT done THEN DISPOSE(body); DISPOSE(new); RETURN END
  END;
  clp_pln(new^.pline,new^.rect);
  IF model^[lay]^.plins = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.plins:=new
  ELSE
    new^.next:=model^[lay]^.plins^.next;
    new^.prev:=model^[lay]^.plins;
    model^[lay]^.plins^.next^.prev:=new;
    model^[lay]^.plins^.next:=new;
    model^[lay]^.plins:=new
  END
END new_pline;

PROCEDURE new_ell(model: Model; ell: def.Ellips; lay: INTEGER);
  VAR new: ell_ptr;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  WITH new^.ellips DO
    centr:=ell.centr;
    a:= ell.a;
    b:= ell.b;
    angle:= ell.angle;
    color:= ell.color;
    ltyp := ell.ltyp;
    copy_pict(pict,ell.pict);
    IF NOT done THEN DISPOSE(new); RETURN END
  END;
  clp_ell(new^.ellips,new^.rect);
  IF model^[lay]^.ellps = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.ellps:=new
  ELSE
    new^.next:=model^[lay]^.ellps^.next;
    new^.prev:=model^[lay]^.ellps;
    model^[lay]^.ellps^.next^.prev:=new;
    model^[lay]^.ellps^.next:=new;
    model^[lay]^.ellps:=new
  END
END new_ell;

PROCEDURE new_text(model: Model; txt: def.Text; lay: INTEGER);
  VAR new: txt_ptr;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  WITH new^.text DO
    NEW(stxt,HIGH(txt.stxt)+1);
    IF NOT done THEN DISPOSE(new); RETURN END;
    stxt:= txt.stxt;
    NEW(pict,HIGH(txt.pict)+1);
    IF NOT done THEN DISPOSE(stxt); DISPOSE(new); RETURN END;
    pict := txt.pict;
    xyz  := txt.xyz;
    color:= txt.color;
    w:= txt.w;
    h:= txt.h;
    a:= txt.a;
    i:= txt.i
  END;
  clp_txt(new^.text,new^.rect);
  IF model^[lay]^.texts = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.texts:=new
  ELSE
    new^.next:=model^[lay]^.texts^.next;
    new^.prev:=model^[lay]^.texts;
    model^[lay]^.texts^.next^.prev:=new;
    model^[lay]^.texts^.next:=new;
    model^[lay]^.texts:=new
  END
END new_text;

PROCEDURE new_surf (model: Model; srf: def.Surf; lay: INTEGER);
 VAR  new: surf_ptr;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  new^.surf:= srf;

(*
  WITH new^.surf DO

    NEW(sbody,HIGH(srf.sbody)+1);
    IF NOT done THEN DISPOSE(new); RETURN END;
    sbody:= srf.sbody;
    color:= srf.color
  END;
*)
  clp_surf(new^.surf,new^.rect);
  IF model^[lay]^.surfs = NIL THEN
    new^.next:= new;
    new^.prev:= new;
    model^[lay]^.surfs:= new
  ELSE
    new^.next:= model^[lay]^.surfs^.next;
    new^.prev:= model^[lay]^.surfs;
    model^[lay]^.surfs^.next^.prev:= new;
    model^[lay]^.surfs^.next:= new;
    model^[lay]^.surfs:= new
  END
END new_surf;

PROCEDURE new_group(model: Model; grp: def.Group; lay: INTEGER);
  VAR new: grp_ptr;
      i,j: INTEGER;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  WITH new^.group DO
    NEW(line,HIGH(grp.line)+1);
    IF NOT done THEN DISPOSE(new); RETURN END;
    FOR j:=0 TO HIGH(line) DO
      WITH line[j] DO
        xyz :=  grp.line[j].xyz;
        XYZ :=  grp.line[j].XYZ;
        color:= grp.line[j].color;
        ltyp := grp.line[j].ltyp;
        NEW(pict,HIGH(grp.line[j].pict)+1);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
        FOR i:=0 TO HIGH(pict) DO pict[i]:= grp.line[j].pict[i] END
      END
    END;
    NEW(circ,HIGH(grp.circ)+1);
    IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
    FOR j:=0 TO HIGH(circ) DO
      WITH circ[j] DO
        centr:= grp.circ[j].centr;
        r:=     grp.circ[j].r;
        color:= grp.circ[j].color;
        ltyp := grp.circ[j].ltyp;
        copy_pict(pict,grp.circ[j].pict);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END
      END
    END;
    NEW(arc,HIGH(grp.arc)+1);
    IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
    FOR j:=0 TO HIGH(arc) DO
      WITH arc[j] DO
        xyz1:=  grp.arc[j].xyz1;
        xyz2:=  grp.arc[j].xyz2;
        xyz3:=  grp.arc[j].xyz3;
        color:= grp.arc[j].color;
        ltyp := grp.arc[j].ltyp;
        copy_pict(pict,grp.arc[j].pict);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END
      END
    END;
    NEW(ellips,HIGH(grp.ellips)+1);
    IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
    FOR j:=0 TO HIGH(ellips) DO
      WITH ellips[j] DO
        centr:= grp.ellips[j].centr;
        a:=     grp.ellips[j].a;
        b:=     grp.ellips[j].b;
        angle:= grp.ellips[j].angle;
        color:= grp.ellips[j].color;
        ltyp := grp.ellips[j].ltyp;
        copy_pict(pict,grp.ellips[j].pict);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END
      END
    END;
    NEW(pline,HIGH(grp.pline)+1);
    IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
    FOR j:=0 TO HIGH(pline) DO
      WITH pline[j] DO
        NEW(body,HIGH(grp.pline[j].body)+1);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
        body:=  grp.pline[j].body;
        type:=  grp.pline[j].type;
        color:= grp.pline[j].color;
        ltyp := grp.pline[j].ltyp;
        copy_pict(pict,grp.pline[j].pict);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END
      END
    END;
    NEW(txt,HIGH(grp.txt)+1);
    IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
    FOR j:=0 TO HIGH(txt) DO
      WITH txt[j] DO
        NEW(stxt,HIGH(grp.txt[j].stxt)+1);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
        NEW(pict,HIGH(grp.txt[j].pict)+1);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
        stxt := grp.txt[j].stxt;
        pict := grp.txt[j].pict;
        xyz  := grp.txt[j].xyz;
        color:= grp.txt[j].color;
        w:= grp.txt[j].w;
        h:= grp.txt[j].h;
        a:= grp.txt[j].a;
        i:= grp.txt[j].i
      END
    END;
    NEW(surf,HIGH(grp.surf)+1);
    IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
    FOR j:= 0 TO HIGH(surf) DO
      surf:= grp.surf

(*
      WITH surf[j] DO
        NEW(sbody,HIGH(grp.surf[i].sbody)+1);
        IF NOT done THEN dispose_grp(new^.group); DISPOSE(new); RETURN END;
        sbody:= grp.surf[j].sbody;
        color:= grp.surf[j].color
      END
 *)

    END
  END;
  clp_grp(new^.group,new^.rect);
  IF model^[lay]^.grps = NIL THEN
    new^.next:=new;
    new^.prev:=new;
    model^[lay]^.grps:=new
  ELSE
    new^.next:=model^[lay]^.grps^.next;
    new^.prev:=model^[lay]^.grps;
    model^[lay]^.grps^.next^.prev:=new;
    model^[lay]^.grps^.next:=new;
    model^[lay]^.grps:=new
  END
END new_group;

PROCEDURE new_layer(model: Model);
  VAR new: Layer;
BEGIN
  NEW(new);
  IF NOT done THEN RETURN END;
  RESIZE(model^,HIGH(model^)+2);
  IF NOT done THEN DISPOSE(new); RETURN END;
  model^[HIGH(model^)]:= new;
  WITH model^[HIGH(model^)]^ DO
    name[0]:=0c;
    lines:=NIL;
    circs:=NIL;
    arcs :=NIL;
    plins:=NIL;
    ellps:=NIL;
    texts:=NIL;
    surfs:=NIL;
    grps :=NIL
  END
END new_layer;

--------------------------- Deleting ---------------------------

PROCEDURE rem_line(model: Model; del_lin: lin_ptr; lay:INTEGER);
BEGIN
  IF del_lin=NIL THEN bad_desc; RETURN END;
  WITH del_lin^ DO
    dispose_line(line);
    IF next = del_lin THEN
      model^[lay]^.lines:=NIL;
    ELSIF model^[lay]^.lines = del_lin THEN
      model^[lay]^.lines:=model^[lay]^.lines^.next
    END;
    next^.prev:= prev;
    prev^.next:= next
  END
END rem_line;

PROCEDURE rem_circ(model: Model; del_cir: cir_ptr; lay:INTEGER);
BEGIN
  IF del_cir=NIL THEN bad_desc; RETURN END;
  WITH del_cir^ DO
    dispose_circ(circ);
    IF next = del_cir THEN
      model^[lay]^.circs:=NIL;
    ELSIF model^[lay]^.circs = del_cir THEN
      model^[lay]^.circs:=model^[lay]^.circs^.next
    END;
    next^.prev:=prev;
    prev^.next:=next;
  END
END rem_circ;

PROCEDURE rem_arc(model: Model;  del_arc: arc_ptr; lay:INTEGER);
BEGIN
  IF del_arc=NIL THEN bad_desc; RETURN END;
  WITH del_arc^ DO
    dispose_arc(arc);
    IF next = del_arc THEN
      model^[lay]^.arcs:=NIL;
    ELSIF model^[lay]^.arcs = del_arc THEN
      model^[lay]^.arcs:=model^[lay]^.arcs^.next
    END;
    next^.prev:=prev;
    prev^.next:=next
  END
END rem_arc;

PROCEDURE rem_pline(model: Model; del_pln: pln_ptr; lay:INTEGER);
BEGIN
  IF del_pln=NIL THEN bad_desc; RETURN END;
  WITH del_pln^ DO
    dispose_pln(pline);
    IF next = del_pln THEN
      model^[lay]^.plins:=NIL;
    ELSIF model^[lay]^.plins = del_pln THEN
      model^[lay]^.plins:=model^[lay]^.plins^.next
    END;
    next^.prev:=prev;
    prev^.next:=next
  END
END rem_pline;

PROCEDURE rem_ellip(model: Model; del_ell: ell_ptr; lay:INTEGER);
BEGIN
  IF del_ell=NIL THEN bad_desc; RETURN END;
  WITH del_ell^ DO
    dispose_ell(ellips);
    IF next = del_ell THEN
      model^[lay]^.ellps:=NIL;
    ELSIF model^[lay]^.ellps = del_ell THEN
      model^[lay]^.ellps:=model^[lay]^.ellps^.next
    END;
    next^.prev:=prev;
    prev^.next:=next
  END
END rem_ellip;

PROCEDURE rem_text(model: Model; del_txt: txt_ptr; lay:INTEGER);
  VAR i: INTEGER;
BEGIN
  IF del_txt=NIL THEN bad_desc; RETURN END;
  WITH del_txt^ DO
    dispose_txt(text);
    IF next = del_txt THEN
      model^[lay]^.texts:=NIL;
    ELSIF model^[lay]^.texts = del_txt THEN
      model^[lay]^.texts:=model^[lay]^.texts^.next
    END;
    next^.prev:=prev;
    prev^.next:=next
  END
END rem_text;


PROCEDURE rem_surf (model: Model; del_surf: surf_ptr;  lay: INTEGER);
BEGIN
  IF del_surf=NIL THEN bad_desc; RETURN END;
  WITH del_surf^ DO
    dispose_surf(surf);
    IF next = del_surf THEN
      model^[lay]^.surfs:=NIL;
    ELSIF model^[lay]^.surfs = del_surf THEN
      model^[lay]^.surfs:= model^[lay]^.surfs^.next
    END;
    next^.prev:=prev;
    prev^.next:=next
  END
END rem_surf;

PROCEDURE rem_group(model: Model; del_grp: grp_ptr; lay:INTEGER);
BEGIN
  IF del_grp=NIL THEN bad_desc; RETURN END;
  dispose_grp(del_grp^.group);
  WITH del_grp^ DO
    IF next = del_grp THEN
      model^[lay]^.grps:=NIL;
    ELSIF model^[lay]^.grps = del_grp THEN
      model^[lay]^.grps:=model^[lay]^.grps^.next
    END;
    next^.prev:= prev;
    prev^.next:= next
  END
END rem_group;

PROCEDURE rem_lay(model: Model; l: INTEGER);
VAR  lin: lin_ptr;  cir: cir_ptr;
     arc: arc_ptr;  elp: ell_ptr;
     pln: pln_ptr;  txt: txt_ptr;
     srf: surf_ptr; grp: grp_ptr;
BEGIN
  WITH model^[l]^ DO
    IF lines# NIL THEN
      lines^.prev^.next:= NIL;
      WHILE lines# NIL DO
        lin:=lines;   lines:= lines^.next;
        rem_line(model,lin,l)
      END
    END;
    IF circs#NIL THEN
      circs^.prev^.next:= NIL;
      WHILE circs# NIL DO
        cir:=circs;   circs:= circs^.next;
        rem_circ(model,cir,l)
      END
    END;
    IF arcs#NIL THEN
      arcs^.prev^.next:=  NIL;
      WHILE arcs# NIL DO
        arc:=arcs;  arcs:= arcs^.next;
        rem_arc(model,arc,l)
      END
    END;
    IF ellps# NIL THEN
      ellps^.prev^.next:= NIL;
      WHILE ellps# NIL DO
        elp:=ellps; ellps:= ellps^.next;
        rem_ellip(model,elp,l)
      END
    END;
    IF plins# NIL THEN
      plins^.prev^.next:= NIL;
      WHILE plins#NIL DO
        pln:=plins; plins:= plins^.next;
        rem_pline(model,pln,l)
      END
    END;
    IF texts#NIL THEN
      texts^.prev^.next:= NIL;
      WHILE texts#NIL DO
        txt:=texts;  texts:=texts^.next;
        rem_text(model,txt,l)
      END
    END;
    IF  surfs#NIL THEN
      surfs^.prev^.next:=  NIL;
      WHILE surfs#NIL DO
        srf:= surfs; surfs:= surfs^.next;
        rem_surf(model,srf,l)
      END
    END;
    IF grps# NIL THEN
      grps^.prev^.next:=  NIL;
      WHILE grps# NIL DO
        grp:=grps; grps:=grps^.next;
        rem_group(model,grp,l)
      END
    END
  END
END rem_lay;

PROCEDURE rem_all(model: Model);
  VAR i: INTEGER;
BEGIN FOR i:=0 TO HIGH(model^) DO rem_lay(model,i) END
END rem_all;

BEGIN
 WITH null_clip DO x:= MAX(INTEGER); y:=x; X:=-x; Y:=-y END;
 done:=TRUE; error:=err.ok;
END bcObj.
