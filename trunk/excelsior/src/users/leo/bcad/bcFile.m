IMPLEMENTATION MODULE bcFile; (*  brd 31-Jan-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  bpm: bcPM;
IMPORT  def: bcDef;
IMPORT  bmt: bcMath;
IMPORT  mth: realMath;
IMPORT  obj: bcObj;
IMPORT  tol: bcTool;
IMPORT  bas: bcBase;
IMPORT  set: bcSet;
IMPORT  wnd: pmWnd;
IMPORT  mem: Heap;

IMPORT  tty: Terminal;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

CONST vers = 102;    (*  03-08-1991  brd *)

VAR HEAD,BHEAD: DYNARR OF SYSTEM.WORD;
    name: ARRAY [0..15] OF CHAR;
    file: bio.FILE;

PROCEDURE write(VAL a: ARRAY OF SYSTEM.WORD; len: INTEGER);
BEGIN
  bio.put(file,a,len);
  IF NOT bio.done THEN ASSERT(FALSE,bio.error) END
END write;

PROCEDURE wr(i: SYSTEM.WORD);
BEGIN
  bio.write(file,SYSTEM.ADR(i),BYTES(i));
  IF NOT bio.done THEN ASSERT(FALSE,bio.error) END
END wr;

PROCEDURE write_line(lin: def.Line);
BEGIN
  WITH lin DO
    write(xyz,BYTES(xyz));
    write(XYZ,BYTES(XYZ));
    wr (HIGH(pict)+1);  write(pict,BYTES(pict));
    wr(ltyp);
    wr(color)
  END
END write_line;

PROCEDURE write_pict(VAR p: def.PICTURE);
  VAR i: INTEGER;
BEGIN
  wr(HIGH(p)+1);
  FOR i:=0 TO HIGH(p) DO
    wr(HIGH(p[i])+1);
    write(p[i],BYTES(p[i]))
  END
END write_pict;

PROCEDURE write_circle(cir: def.Circ);
BEGIN
  WITH cir DO
    write(centr,BYTES(centr));
    wr(r);
    write_pict(pict);
    wr(ltyp);
    wr(color)
  END
END write_circle;

PROCEDURE write_arc(arc: def.Arc);
BEGIN
  WITH arc DO
    write(xyz1,BYTES(xyz1));
    write(xyz2,BYTES(xyz2));
    write(xyz3,BYTES(xyz3));
    write_pict(pict);
    wr(ltyp);
    wr(color)
  END
END write_arc;

PROCEDURE write_ellp(ell: def.Ellips);
BEGIN
  WITH ell DO
    write(centr,BYTES(centr));
    wr(a);
    wr(b);
    wr(angle);
    write_pict(pict);
    wr(ltyp);
    wr(color)
  END
END write_ellp;

PROCEDURE write_pline(pln: def.Pline);
BEGIN
  WITH pln DO
    wr(HIGH(body)+1); write(body,BYTES(body));
    write_pict(pict);
    wr(type);
    wr(ltyp);
    wr(color)
  END
END write_pline;

PROCEDURE write_txt(txt: def.Text);
BEGIN
  WITH txt DO
    wr(LEN(stxt)); write(stxt,BYTES(stxt));
    wr(LEN(pict)); write(pict,BYTES(pict));
    write(xyz,BYTES(xyz));
    wr(w); wr(h); wr(a); wr(i);
    wr(color)
  END
END write_txt;

PROCEDURE write_surf(s: def.Surf);
BEGIN
END write_surf;

PROCEDURE write_group(grp: def.Group);
  VAR l,i: INTEGER;
BEGIN
  WITH grp DO
    wr(HIGH(line)+1);
    FOR i:=0 TO HIGH(line) DO write_line(line[i]) END;
    wr(HIGH(circ)+1);
    FOR i:=0 TO HIGH(circ) DO write_circle(circ[i]) END;
    wr(HIGH(arc)+1);
    FOR i:=0 TO HIGH(arc) DO write_arc(arc[i]) END;
    wr(HIGH(ellips)+1);
    FOR i:=0 TO HIGH(ellips) DO write_ellp(ellips[i]) END;
    wr(HIGH(pline)+1);
    FOR i:=0 TO HIGH(pline) DO write_pline(pline[i]) END;
    wr(HIGH(txt)+1);
    FOR i:=0 TO HIGH(txt) DO write_txt(txt[i]) END;
    wr(HIGH(surf)+1);
    FOR i:=0 TO HIGH(surf) DO write_surf(surf[i]) END
  END
END write_group;

PROCEDURE write_lay(VAL lay: obj.Layer);
  VAR l,i: INTEGER;
    lin: obj.lin_ptr;  cir: obj.cir_ptr;
    arc: obj.arc_ptr;  pln: obj.pln_ptr;
    ell: obj.ell_ptr;  txt: obj.txt_ptr;
    srf: obj.surf_ptr; grp: obj.grp_ptr;

BEGIN
  WITH lay^ DO
    write(name,BYTES(name));
    l:=0; lin:=lines;
    IF lines=NIL THEN  wr(0)
    ELSE
      REPEAT
        INC(l);
        lin:=lin^.next
      UNTIL lin=lines;
      wr(l);
      lin:=lines;
      FOR i:=0 TO l-1 DO write_line(lin^.line); lin:=lin^.next END;
    END;
    l:=0; cir:=circs;
    IF circs=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); cir:=cir^.next UNTIL cir=circs;
      wr(l); cir:=circs;
      FOR i:=0 TO l-1 DO write_circle(cir^.circ); cir:=cir^.next END
    END;
    l:=0; arc:=arcs;
    IF arcs=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); arc:=arc^.next UNTIL arc=arcs;
      wr(l); arc:=arcs;
      FOR i:=0 TO l-1 DO write_arc(arc^.arc); arc:=arc^.next END
    END;
    l:=0; ell:=ellps;
    IF ellps=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); ell:=ell^.next UNTIL ell=ellps;
      wr(l); ell:=ellps;
      FOR i:=0 TO l-1 DO write_ellp(ell^.ellips); ell:=ell^.next END
    END;
    l:=0; pln:=plins;
    IF plins=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); pln:=pln^.next UNTIL pln=plins;
      wr(l); pln:=plins;
      FOR i:=0 TO l-1 DO write_pline(pln^.pline); pln:=pln^.next END
    END;
    l:=0; txt:= texts;
    IF texts=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); txt:=txt^.next UNTIL txt=texts;
      wr(l); txt:=texts;
      FOR i:=0 TO l-1 DO write_txt(txt^.text); txt:=txt^.next END
    END;
    l:=0; srf:=surfs;
    IF surfs=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); srf:=srf^.next UNTIL srf=surfs;
      wr(l); srf:=surfs;
      FOR i:=0 TO l-1 DO write_surf(srf^.surf); srf:=srf^.next END
    END;
    l:=0; grp:=grps;
    IF grps=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); grp:=grp^.next UNTIL grp=grps;
      wr(l); grp:=grps;
      FOR i:=0 TO l-1 DO write_group(grp^.group); grp:=grp^.next END
    END
  END
END write_lay;

PROCEDURE write_file(file_name: ARRAY OF CHAR);
  VAR l,i: INTEGER;

BEGIN
  bio.create(file,file_name,'w',0);
  IF NOT bio.done THEN
    bpm.perror(bio.error,240,180,'ошибка при записи %s',file_name); RETURN
  END;
  wr(HIGH(HEAD)+1); write(HEAD,BYTES(HEAD));
  l:=0;
  WHILE (l<= HIGH(bas.cview^.model^.mbody^)) DO INC(l) END;
  wr(l);
  FOR i:=0 TO l-1 DO write_lay(bas.cview^.model^.mbody^[i]) END;
  bio.close(file)
END write_file;

PROCEDURE write_block(bl_name: ARRAY OF CHAR; top: def.VERTEX);
VAR  i: INTEGER;
BEGIN
  bio.create(file,bl_name,'w',0);
  IF NOT bio.done THEN
    bpm.perror(bio.error,240,180,'ошибка при записи %s',bl_name); RETURN
  END;
  wr(HIGH(BHEAD)+1); write(BHEAD,BYTES(BHEAD));
  wr(bas.x_mrk); wr(bas.y_mrk);
  wr(bas.X_mrk); wr(bas.Y_mrk);
  write(top,BYTES(top));
  wr(HIGH(obj.mark_lin)+1);
  FOR i:=0 TO HIGH(obj.mark_lin) DO write_line(obj.mark_lin[i]^.line) END;
  wr(HIGH(obj.mark_cir)+1);
  FOR i:=0 TO HIGH(obj.mark_cir) DO write_circle(obj.mark_cir[i]^.circ)  END;
  wr(HIGH(obj.mark_arc)+1);
  FOR i:=0 TO HIGH(obj.mark_arc) DO write_arc(obj.mark_arc[i]^.arc)  END;
  wr(HIGH(obj.mark_ell)+1);
  FOR i:=0 TO HIGH(obj.mark_ell) DO write_ellp(obj.mark_ell[i]^.ellips)  END;
  wr(HIGH(obj.mark_pln)+1);
  FOR i:=0 TO HIGH(obj.mark_pln) DO write_pline(obj.mark_pln[i]^.pline)  END;
  wr(HIGH(obj.mark_txt)+1);
  FOR i:=0 TO HIGH(obj.mark_txt) DO write_txt(obj.mark_txt[i]^.text)  END;
  wr(HIGH(obj.mark_srf)+1);
  FOR i:=0 TO HIGH(obj.mark_srf) DO write_surf(obj.mark_srf[i]^.surf)  END;
  wr(HIGH(obj.mark_grp)+1);
  FOR i:=0 TO HIGH(obj.mark_grp) DO write_group(obj.mark_grp[i]^.group)  END;
  bio.close(file)
END write_block;

----------------------------- MOVE -----------------------------

VAR DX,DY,DZ: REAL;

PROCEDURE mov_top(VAR top: def.VERTEX);
BEGIN
  WITH bas.cview^ DO
    top[Xc]:= top[Xc]+DX;
    top[Yc]:= top[Yc]+DY;
    top[Zc]:= top[Zc]+DZ
  END
END mov_top;

PROCEDURE mov_vpic(VAR pic: def.VPICTURE);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(pic) DO mov_top(pic[i,0]); mov_top(pic[i,1]) END
END mov_vpic;

PROCEDURE mov_pic(VAR pic: def.PICTURE);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(pic) DO
    FOR j:=0 TO HIGH(pic[i]) DO mov_top(pic[i,j]) END
  END
END mov_pic;

PROCEDURE mov_clp(VAR clip: obj.Clip);
  BEGIN  WITH clip DO
    x:=x+mth.round(DX); y:=y+mth.round(DY);
    X:=X+mth.round(DX); Y:=Y+mth.round(DY)
  END
END mov_clp;

PROCEDURE mov_lin(VAR lin: def.Line);
  BEGIN WITH lin DO mov_top(xyz); mov_top(XYZ); mov_vpic(pict) END
END mov_lin;

PROCEDURE mov_cir(VAR cir: def.Circ);
  BEGIN WITH cir DO mov_top(centr); mov_pic(pict) END
END mov_cir;

PROCEDURE mov_arc(VAR  arc: def.Arc);
BEGIN
  WITH arc DO mov_top(xyz1); mov_top(xyz2); mov_top(xyz3); mov_pic(pict) END
END mov_arc;

PROCEDURE mov_ell(VAR  ell: def.Ellips);
  BEGIN WITH ell DO mov_top(centr); mov_pic(pict) END
END mov_ell;

PROCEDURE mov_pln(VAR pln: def.Pline);
  VAR i: INTEGER;
BEGIN
  WITH pln DO
    FOR i:=0 TO HIGH(body) DO  mov_top(body[i]) END;
    mov_pic(pict)
  END
END mov_pln;

PROCEDURE mov_txt(VAR txt: def.Text);
BEGIN WITH txt DO mov_top(xyz); mov_vpic(pict) END; END mov_txt;

PROCEDURE mov_srf(VAR srf: def.Surf);
BEGIN
END mov_srf;

PROCEDURE mov_grp(VAR grp: def.Group);
  VAR i: INTEGER;
BEGIN
  WITH grp DO
    FOR i:=0 TO HIGH(line)   DO mov_lin(line[i])   END;
    FOR i:=0 TO HIGH(circ)   DO mov_cir(circ[i])   END;
    FOR i:=0 TO HIGH(arc)    DO mov_arc(arc[i])    END;
    FOR i:=0 TO HIGH(ellips) DO mov_ell(ellips[i]) END;
    FOR i:=0 TO HIGH(pline)  DO mov_pln(pline[i])  END;
    FOR i:=0 TO HIGH(txt)    DO mov_txt(txt[i])    END;
    FOR i:=0 TO HIGH(surf)   DO mov_srf(surf[i])   END
  END
END mov_grp;

----------------------------- READ -----------------------------

PROCEDURE read(VAR a: ARRAY OF SYSTEM.WORD; len: INTEGER);
BEGIN
  bio.get(file,a,len);
  IF NOT bio.done THEN ASSERT(FALSE,bio.error) END
END read;

PROCEDURE rd(VAR i: SYSTEM.WORD);
BEGIN
  bio.read(file,SYSTEM.ADR(i),BYTES(i));
  IF NOT bio.done THEN ASSERT(FALSE,bio.error) END
END rd;

PROCEDURE read_line(VAR new: def.Line);
  VAR  h: INTEGER;
BEGIN
  WITH new DO
    read(xyz,BYTES(xyz));
    read(XYZ,BYTES(XYZ));
    rd(h); NEW(pict,h);
    read(pict,BYTES(pict));
    rd(ltyp);
    rd(color)
  END
END read_line;

PROCEDURE read_pict(VAR p: def.PICTURE);
  VAR i,l: INTEGER;
BEGIN
  rd(l); NEW(p,l);
  FOR i:=0 TO l-1 DO rd(l); NEW(p[i],l); read(p[i],BYTES(p[i])) END
END read_pict;

PROCEDURE dispose(VAR p: def.PICTURE);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(p) DO DISPOSE(p[i]) END; DISPOSE(p)
END dispose;

PROCEDURE read_circle(VAR new: def.Circ);
BEGIN
  WITH new DO
    read(centr,BYTES(centr));
    rd(r);
    read_pict(pict);
    rd(ltyp);
    rd(color)
  END
END read_circle;

PROCEDURE read_arc(VAR new: def.Arc);
  VAR t: def.VERTEX;
      r: REAL;

BEGIN
  WITH new DO
    read(xyz1,BYTES(xyz1));
    read(xyz2,BYTES(xyz2));
    read(xyz3,BYTES(xyz3));
    read_pict(pict);
    rd(ltyp);
    rd(color)
  END
END read_arc;

PROCEDURE read_ellp(VAR new: def.Ellips);
BEGIN
  WITH new DO
    read(centr,BYTES(centr));
    rd(a); rd(b); rd(angle);
    read_pict(pict);
    rd(ltyp);
    rd(color)
  END
END read_ellp;

PROCEDURE read_pline(VAR new: def.Pline);
  VAR h: INTEGER;
BEGIN
  WITH new DO
    rd(h); NEW(body,h);
    read(body,BYTES(body));
    read_pict(pict);
    rd(type);
    rd(ltyp);
    rd(color)
  END
END read_pline;

PROCEDURE read_txt(VAR new: def.Text);
  VAR j: INTEGER;
BEGIN
  WITH new DO
    rd(j); NEW(stxt,j); read(stxt,BYTES(stxt));
    rd(j); NEW(pict,j); read(pict,BYTES(pict));
    read(xyz ,BYTES(xyz));
    rd(w);
    rd(h);
    rd(a);
    rd(i);
    rd(color)
  END
END read_txt;

PROCEDURE read_surf(VAR new: def.Surf);
BEGIN
END read_surf;

PROCEDURE read_group(VAR new: def.Group);
  VAR l,i: INTEGER;
BEGIN
  WITH new DO
    rd(l); NEW(line,l);
    FOR i:=0 TO HIGH(line) DO read_line(line[i]) END;
    rd(l); NEW(circ,l);
    FOR i:=0 TO HIGH(circ) DO read_circle(circ[i]) END;
    rd(l); NEW(arc,l);
    FOR i:=0 TO HIGH(arc) DO read_arc(arc[i]) END;
    rd(l); NEW(ellips,l);
    FOR i:=0 TO HIGH(ellips) DO read_ellp(ellips[i]) END;
    rd(l); NEW(pline,l);
    FOR i:=0 TO HIGH(pline) DO read_pline(pline[i]) END;
    rd(l); NEW(txt,l);
    FOR i:=0 TO HIGH(txt) DO read_txt(txt[i]) END;
    rd(l); NEW(surf,l);
    FOR i:=0 TO HIGH(surf) DO read_surf(surf[i]) END
  END
END read_group;

PROCEDURE read_body(lay: INTEGER);
  VAR clip: obj.Clip;
       lin: def.Line;
       cir: def.Circ;
       arc: def.Arc;
       ell: def.Ellips;
       pln: def.Pline;
       txt: def.Text;
       srf: def.Surf;
       grp: def.Group;
         t: def.VERTEX;
       l,i: INTEGER;
         r: REAL;
BEGIN
  WITH bas.cview^.model^ DO
    rd(l); FOR i:=0 TO l-1 DO
      read_line(lin);
      obj.new_line(mbody,lin,lay);
      bas.extr_lin(bas.cview^.model,lin);
      obj.dispose_line(lin)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_circle(cir);
      obj.new_circ(mbody,cir,lay);
      bas.extr_cir(bas.cview^.model,cir);
      obj.dispose_circ(cir)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_arc(arc);
      obj.new_arc(mbody,arc,lay);
      bas.extr_arc(bas.cview^.model,arc);
      obj.dispose_arc(arc)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_ellp(ell);
      obj.new_ell(mbody,ell,lay);
      bas.extr_ell(bas.cview^.model,ell);
      obj.dispose_ell(ell)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_pline(pln);
      obj.new_pline(mbody,pln,lay);
      bas.extr_pln(bas.cview^.model,pln);
      obj.dispose_pln(pln)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_txt(txt);
      obj.new_text(mbody,txt,lay);
      bas.extr_txt(bas.cview^.model,txt);
      obj.dispose_txt(txt)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_surf(srf);
      obj.new_surf(mbody,srf,lay);
      bas.extr_srf(bas.cview^.model,srf);
      obj.dispose_surf(srf)
    END;
    rd(l); FOR i:=0 TO l-1 DO
      read_group(grp);
      obj.new_group(mbody,grp,lay);
      bas.extr_grp(bas.cview^.model,grp);
      obj.dispose_grp(grp)
    END
  END
END read_body;

PROCEDURE read_mrk;
  VAR  l,i: INTEGER;
BEGIN
  rd(l);
  NEW(obj.mark_lin,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_lin[i]);
    WITH obj.mark_lin[i]^ DO
      read_line(line);
      obj.clp_lin(line,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_cir,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_cir[i]);
    WITH obj.mark_cir[i]^ DO
      read_circle(circ);
      obj.clp_cir(circ,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_arc,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_arc[i]);
    WITH obj.mark_arc[i]^ DO
      read_arc(arc);
      obj.clp_arc(arc,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_ell,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_ell[i]);
    WITH obj.mark_ell[i]^ DO
      read_ellp(ellips);
      obj.clp_ell(ellips,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_pln,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_pln[i]);
    WITH obj.mark_pln[i]^ DO
      read_pline(pline);
      obj.clp_pln(pline,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_txt,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_txt[i]);
    WITH obj.mark_txt[i]^ DO
      read_txt(text);
      obj.clp_txt(text,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_srf,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_srf[i]);
    WITH obj.mark_srf[i]^ DO
      read_surf(surf);
      obj.clp_surf(surf,rect);
      next:=NIL;
      prev:=NIL
    END
  END;
  rd(l);
  NEW(obj.mark_grp,l);
  FOR i:=0 TO l-1 DO
    NEW(obj.mark_grp[i]);
    WITH obj.mark_grp[i]^ DO
      read_group(group);
      obj.clp_grp(group,rect);
      next:=NIL;
      prev:=NIL
    END
  END
END read_mrk;

PROCEDURE read_lay(lay: INTEGER);
BEGIN
  WITH bas.cview^.model^.mbody^[lay]^ DO
    read(name,BYTES(name));
    lines:= NIL;
    circs:= NIL;
    arcs := NIL;
    ellps:= NIL;
    plins:= NIL;
    texts:= NIL;
    surfs:= NIL;
    grps := NIL
  END;
  read_body(lay)
END read_lay;

PROCEDURE bad_vers;
BEGIN
  IF bas.ENG THEN
    bpm.message(bas.SW DIV 2,bas.SH DIV 2,'Bad file version')
  ELSE
    bpm.message(bas.SW DIV 2,bas.SH DIV 2,'Неправильная версия файла')
  END
END bad_vers;

PROCEDURE read_file(mname,file_name: ARRAY OF CHAR);
  VAR l,i,k: INTEGER;
       head: DYNARR OF SYSTEM.WORD;

  PROCEDURE exit; BEGIN DISPOSE(head); bio.close(file) END exit;

BEGIN
  bio.open(file,file_name,'r');
  IF NOT bio.done THEN
    bpm.perror(bio.error,240,180,'файл %s не найден',file_name); exit; RETURN
  END;
  rd(l); NEW(head,l);
  IF HIGH(head)< 0 THEN bad_vers; exit; RETURN END;
  read(head,BYTES(head));
  IF head[0]#vers THEN bad_vers; exit; RETURN END;

  obj.rem_all(bas.cview^.model^.mbody);
  rd(l);
  NEW(bas.cview^.model^.mbody^,0);
  bas.cview^.model^.area:= bas.null_area;
  FOR i:=0 TO l-1 DO
    obj.new_layer(bas.cview^.model^.mbody);
    read_lay(i)
  END;
  WITH bas.cview^.model^ DO
    name:=mname;
    fname:= file_name;
    FOR i:=0 TO HIGH(views) DO
      NEW(views[i]^.cntx^.mask,HIGH(mbody^)+1);
      FOR k:=0 TO HIGH(mbody^) DO views[i]^.cntx^.mask[k]:=1 END;
      views[i]^.cntx^.work:=0;
      views[i]^.cntx^.mask[0]:=2;
      bas.show_name(views[i]);
    END
  END;
  exit
END read_file;

VAR bX,bY,bx,by: INTEGER;

PROCEDURE cross_block(X,Y: INTEGER);
BEGIN
  wnd.frame(bas.cview^.wind,bas.cview^.tool,X+bx,Y+by,X+bX,Y+bY);
  WITH bas.cview^.tool.clip DO
    wnd.line(bas.cview^.wind,bas.cview^.tool,x,Y,w+x,Y);
    wnd.line(bas.cview^.wind,bas.cview^.tool,X,y,X,h+y)
  END
END cross_block;

PROCEDURE read_block(bl_name: ARRAY OF CHAR);
  VAR sou,dst: def.VERTEX;
          crs: bas.CURSOR;
         head: DYNARR OF SYSTEM.WORD;
          i,l: INTEGER;
            s: ARRAY [0..31] OF CHAR;

  PROCEDURE exit; BEGIN DISPOSE(head); bio.close(file) END exit;

BEGIN
  IF bas.ENG THEN s:='Base point:' ELSE s:='Базовая точка:' END;
  bio.open(file,bl_name,'r');
  IF NOT bio.done THEN
    bpm.perror(bio.error,240,180,'файл %s не найден',bl_name); RETURN
  END;

  rd(l); NEW(head,l);
  IF HIGH(head)< 0 THEN bad_vers; exit; RETURN END;
  read(head,BYTES(head));
  IF head[0]#vers THEN bad_vers; exit; RETURN END;

  rd(bas.x_mrk); rd(bas.y_mrk); rd(bas.X_mrk); rd(bas.Y_mrk);
  read(sou,BYTES(sou));
  read_mrk;
  bas.set_marked(sou);
  crs:=bas.cross_marked;
  IF NOT bas.done THEN
    WITH bas.cview^ DO
      bX:= TRUNC((bas.X_mrk-sou[Xc])*scale);
      bY:= TRUNC((bas.Y_mrk-sou[Yc])*scale);
      bx:= TRUNC((bas.x_mrk-sou[Xc])*scale);
      by:= TRUNC((bas.y_mrk-sou[Yc])*scale);
    END;
    crs:=cross_block
  END;
  IF NOT bas.read_point(s,crs,bas.null_monitor,dst) THEN
    obj.dispose_mrk;
    bas.end_marked;
    RETURN
  END;
  WITH bas.cview^ DO
    DX:=dst[Xc] - sou[Xc];
    DY:=dst[Yc] - sou[Yc];
    DZ:=dst[Zc] - sou[Zc];
    FOR i:=0 TO HIGH(obj.mark_lin) DO
      WITH obj.mark_lin[i]^ DO
        mov_lin(line);
        obj.new_line(model^.mbody,line,cntx^.work);
        bas.extr_lin(model,line)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_cir) DO
      WITH obj.mark_cir[i]^ DO
        mov_cir(circ);
        obj.new_circ(model^.mbody,circ,cntx^.work);
        bas.extr_cir(model,circ)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_arc) DO
      WITH obj.mark_arc[i]^ DO
        mov_arc(arc);
        obj.new_arc(model^.mbody,arc,cntx^.work);
        bas.extr_arc(model,arc)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_ell) DO
      WITH obj.mark_ell[i]^ DO
        mov_ell(ellips);
        obj.new_ell(model^.mbody,ellips,cntx^.work);
        bas.extr_ell(model,ellips)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_pln) DO
      WITH obj.mark_pln[i]^ DO
        mov_pln(pline);
        obj.new_pline(model^.mbody,pline,cntx^.work);
        bas.extr_pln(model,pline)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_txt) DO
      WITH obj.mark_txt[i]^ DO
        mov_txt(text);
        obj.new_text(model^.mbody,text,cntx^.work);
        bas.extr_txt(model,text)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_srf) DO
      WITH obj.mark_srf[i]^ DO
        mov_srf(surf);
        obj.new_surf(model^.mbody,surf,cntx^.work);
        bas.extr_srf(model,surf)
      END
    END;
    FOR i:=0 TO HIGH(obj.mark_grp) DO
      WITH obj.mark_grp[i]^ DO
        mov_grp(group);
        obj.new_group(model^.mbody,group,cntx^.work);
        bas.extr_grp(model,group)
      END
    END
  END;
  obj.dispose_mrk;
  bas.end_marked;
  bio.close(file)
END read_block;

PROCEDURE ext(s,p: ARRAY OF CHAR): BOOLEAN;
  VAR h: INTEGER;
     dn: BOOLEAN;
BEGIN
  h:=str.len(s)-str.len(p); IF h<0 THEN RETURN FALSE END;
  str.scan(s,h,p,dn); RETURN dn
END ext;

PROCEDURE file?(x,y: INTEGER; VAR n,fn: ARRAY OF CHAR;
                              VAL ex: ARRAY OF CHAR): BOOLEAN;
VAR dbx: bpm.DIREX;
     pr: ARRAY [0..5] OF CHAR;
BEGIN
  str.print(pr,'*%s',ex);
  bpm.dnew(dbx,x,y-120,100,120,{},'.',pr,bpm.files+bpm.dirs);
  bpm.dopen(dbx);
  bpm.dchoose(dbx,name);
  bpm.dselect(dbx);
  IF bpm.dselected(dbx) THEN
    bpm.dfilename(dbx,n);
    bpm.dfullname(dbx,fn);
    IF NOT ext(n,ex) THEN
      str.print(fn,'%s%s',fn,ex);
      str.print(n,'%s%s',n,ex)
    END;
    bpm.ddispose(dbx); RETURN TRUE
  ELSE bpm.ddispose(dbx); RETURN FALSE END
END file?;

PROCEDURE block?(x,y: INTEGER; VAR n: ARRAY OF CHAR): BOOLEAN;
VAR dbx: bpm.DIREX;
BEGIN
  bpm.dnew(dbx,x,y-120,100,120,{},'.','*.mcr',bpm.files+bpm.dirs);
  bpm.dopen(dbx);
  bpm.dselect(dbx);
  IF bpm.dselected(dbx) THEN
    bpm.dfullname(dbx,n);
    IF NOT ext(n,'.mcr') THEN str.print(n,'%s.mcr',n) END;
    bpm.ddispose(dbx); RETURN TRUE
  ELSE bpm.ddispose(dbx); RETURN FALSE END
END block?;

PROCEDURE save(x,y: INTEGER);
  VAR fname: ARRAY [0..255] OF CHAR;
       name: ARRAY [0..31] OF CHAR;
BEGIN
  IF file?(x,y,name,fname,'.bdf') THEN  write_file(fname) END
END save;

PROCEDURE load(x,y: INTEGER): BOOLEAN;
  VAR fname: ARRAY [0..255] OF CHAR;
       name: ARRAY [0..31] OF CHAR;
BEGIN
  IF file?(x,y,name,fname,'.bdf') THEN read_file(name,fname); RETURN TRUE
  ELSE RETURN FALSE  END
END load;

PROCEDURE save_block(x,y: INTEGER);
  VAR bname: ARRAY [0..127] OF CHAR;
        top: def.VERTEX;
          s: ARRAY [0..31] OF CHAR;
BEGIN
  IF bas.ENG THEN s:='Base point:'
  ELSE            s:='Базовая точка:' END;
  IF tol.select() THEN
    IF bas.read_point(s,bas.cross,bas.null_monitor,top) THEN
      IF block?(x,y,bname) THEN
        write_block(bname,top);
        tol.unmark
      END
    END
  END
END save_block;

PROCEDURE load_block(x,y: INTEGER): BOOLEAN;
  VAR bname: ARRAY [0..127] OF CHAR;
BEGIN
  IF block?(x,y,bname) THEN
    read_block(bname); RETURN TRUE
  ELSE RETURN FALSE END
END load_block;

PROCEDURE quit(x,y: INTEGER);
BEGIN
  IF bas.ENG THEN
    IF bpm.query(x,y,'Quit without writing?','Yes','No','y','n',TRUE,FALSE)
    THEN HALT END
  ELSE
    IF bpm.query(x,y,'Выход без записи?','Да','Нет','y','n',TRUE,FALSE)
    THEN HALT END
  END
END quit;

PROCEDURE save_DXF(x,y: INTEGER);
BEGIN
END save_DXF;

PROCEDURE load_DXF(x,y: INTEGER): BOOLEAN;
BEGIN      RETURN FALSE
END load_DXF;

PROCEDURE read_model (x,y: INTEGER);
  VAR mod: bas.VMODEL;
      old: bas.VIEW;
      fn: ARRAY [0..255] OF CHAR;
      mn: ARRAY [0..31] OF CHAR;
BEGIN
  IF file?(x+10,y-10,mn,fn,'.bdf') THEN
    old:= bas.cview;
    bas.new_model(mod);
    bas.cview:= mod^.views[0];
    read_file(mn,fn);
    bas.cview:= old;
    bas.show_name(mod^.views[0]);
    bas.vredraw(mod^.views[0])
  END
END read_model;

PROCEDURE write_model(x,y: INTEGER);
  VAR m: bas.VMODEL;
      v: bas.VIEW;

  PROCEDURE q(): BOOLEAN;
    VAR s: ARRAY [0..63] OF CHAR;
  BEGIN
    IF bas.ENG THEN
      str.print(s,'Write %s?',m^.name);
      RETURN  bpm.query(x,y,s,'Yes','No','y','n',TRUE,FALSE)
    ELSE
      str.print(s,'Записать %s?',m^.name);
      RETURN  bpm.query(x,y,s,'Да','Нет','y','n',TRUE,FALSE)
    END
  END q;

BEGIN
  m:=NIL;
  set.select_model(x,y,m);
  IF m# NIL THEN
    IF NOT q() THEN RETURN END;
    v:=bas.cview;
    bas.cview:=m^.views[0];
    write_file(m^.fname);
    bas.cview:=v
  END
END write_model;

PROCEDURE _write_all;
  VAR old: bas.VIEW;
        i: INTEGER;
BEGIN
  old:= bas.cview;
  FOR i:=0 TO HIGH(bas.models) DO
    WITH bas.models[i]^ DO
      bas.cview := views[0];
      IF NOT ext(name,'.bdf') THEN str.print(name,'%s.bdf',name) END;
      write_file(fname)
    END
  END;
  bas.cview:= old
END _write_all;

PROCEDURE write_all(x,y: INTEGER);
BEGIN
  IF bas.ENG THEN
    IF NOT bpm.query(x,y,'Write all models?','Yes','No','y','n',TRUE,FALSE)
    THEN RETURN  END
  ELSE
    IF NOT bpm.query(x,y,'Записать все модели?','Да','Нет','y','n',TRUE,FALSE)
    THEN RETURN  END
  END;
  _write_all
END write_all;

PROCEDURE create_model(x,y: INTEGER);
  VAR p,nm: ARRAY [0..26] OF CHAR;
       new: bas.VMODEL;
BEGIN
  IF  bas.ENG THEN p:='Name model:'
  ELSE         p:='Имя модели:' END;
  bpm.diabox(x,y,120,nm,p);
  IF nm[0]=0c THEN RETURN END;
  bas.new_model(new);
  IF NOT bas.done THEN  RETURN  END;
  IF ext(nm,'.bdf') THEN  str.print(new^.name,'%s',nm)
  ELSE                    str.print(new^.name,'%s.bdf',nm) END;
  bas.show_name(new^.views[0])
END create_model;

PROCEDURE remove_model(x,y: INTEGER);
  VAR m: bas.VMODEL;

  PROCEDURE q(): BOOLEAN;
    VAR s: ARRAY [0..63] OF CHAR;
  BEGIN
    IF bas.ENG THEN
      str.print(s,'Remove model %s?',m^.name);
      RETURN  bpm.query(x,y,s,'Yes','No','y','n',TRUE,FALSE)
    ELSE
      str.print(s,'Удалить из памяти модель %s?',m^.name);
      RETURN  bpm.query(x,y,s,'Да','Нет','y','n',TRUE,FALSE)
    END
  END q;
BEGIN
  m:= NIL;
  set.select_model(x,y,m);
  IF m#NIL THEN
    IF NOT q() THEN  RETURN END;
    bas.remove_model(m)
  END
END remove_model;

PROCEDURE exit(x,y: INTEGER);
  VAR old: bas.VIEW;
        i: INTEGER;
BEGIN
  IF bas.ENG THEN
    IF NOT bpm.query(240,180,'Write all models and exit?','Yes','No','y','n',TRUE,FALSE)
    THEN RETURN  END
  ELSE
    IF NOT bpm.query(240,180,'Записать все модели и выйти?','Да','Нет','y','n',TRUE,FALSE)
    THEN RETURN  END
  END;
  _write_all;
  HALT
END exit;

----------------------------- plot -----------------------------

PROCEDURE plot_line(lin: def.Line);
BEGIN
  WITH lin DO
    write(xyz,BYTES(xyz));
    write(XYZ,BYTES(XYZ));
    wr (HIGH(pict)+1);  write(pict,BYTES(pict));
    wr(ltyp);
    wr(color)
  END
END plot_line;

PROCEDURE plot_pict(VAR p: def.PICTURE);
  VAR i: INTEGER;
BEGIN
  wr(HIGH(p)+1);
  FOR i:=0 TO HIGH(p) DO
    wr(HIGH(p[i])+1);
    write(p[i],BYTES(p[i]))
  END
END plot_pict;

PROCEDURE plot_circle(cir: def.Circ);
BEGIN
  WITH cir DO
    write(centr,BYTES(centr));
    wr(r);
    plot_pict(pict);
    wr(ltyp);
    wr(color)
  END
END plot_circle;

PROCEDURE plot_arc(arc: def.Arc);
BEGIN
  WITH arc DO
    write(xyz1,BYTES(xyz1));
    write(xyz2,BYTES(xyz2));
    write(xyz3,BYTES(xyz3));
    plot_pict(pict);
    wr(ltyp);
    wr(color)
  END
END plot_arc;

PROCEDURE plot_ellp(ell: def.Ellips);
BEGIN
  WITH ell DO
    write(centr,BYTES(centr));
    wr(a);
    wr(b);
    wr(angle);
    plot_pict(pict);
    wr(ltyp);
    wr(color)
  END
END plot_ellp;

PROCEDURE plot_pline(pln: def.Pline);
BEGIN
  WITH pln DO
    wr(HIGH(body)+1); write(body,BYTES(body));
    plot_pict(pict);
    wr(type);
    wr(ltyp);
    wr(color)
  END
END plot_pline;

PROCEDURE plot_txt(txt: def.Text);
BEGIN
  WITH txt DO
    wr(LEN(stxt)); write(stxt,BYTES(stxt));
    wr(LEN(pict)); write(pict,BYTES(pict));
    write(xyz,BYTES(xyz));
    wr(w); wr(h); wr(a); wr(i);
    wr(color)
  END
END plot_txt;

PROCEDURE plot_surf(s: def.Surf);
BEGIN
END plot_surf;

PROCEDURE plot_group(grp: def.Group);
  VAR l,i: INTEGER;
BEGIN
  WITH grp DO
    wr(HIGH(line)+1);
    FOR i:=0 TO HIGH(line) DO plot_line(line[i]) END;
    wr(HIGH(circ)+1);
    FOR i:=0 TO HIGH(circ) DO plot_circle(circ[i]) END;
    wr(HIGH(arc)+1);
    FOR i:=0 TO HIGH(arc) DO plot_arc(arc[i]) END;
    wr(HIGH(ellips)+1);
    FOR i:=0 TO HIGH(ellips) DO plot_ellp(ellips[i]) END;
    wr(HIGH(pline)+1);
    FOR i:=0 TO HIGH(pline) DO plot_pline(pline[i]) END;
    wr(HIGH(txt)+1);
    FOR i:=0 TO HIGH(txt) DO plot_txt(txt[i]) END;
    wr(HIGH(surf)+1);
    FOR i:=0 TO HIGH(surf) DO plot_surf(surf[i]) END
  END
END plot_group;

PROCEDURE plot_lay(VAL lay: obj.Layer);
  VAR l,i: INTEGER;
    lin: obj.lin_ptr;  cir: obj.cir_ptr;
    arc: obj.arc_ptr;  pln: obj.pln_ptr;
    ell: obj.ell_ptr;  txt: obj.txt_ptr;
    srf: obj.surf_ptr; grp: obj.grp_ptr;
BEGIN
  WITH lay^ DO
    l:=0; lin:=lines;
    IF lines=NIL THEN  wr(0)
    ELSE
      REPEAT
        INC(l);
        lin:=lin^.next
      UNTIL lin=lines;
      wr(l);
      lin:=lines;
      FOR i:=0 TO l-1 DO plot_line(lin^.line); lin:=lin^.next END;
    END;
    l:=0; cir:=circs;
    IF circs=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); cir:=cir^.next UNTIL cir=circs;
      wr(l); cir:=circs;
      FOR i:=0 TO l-1 DO plot_circle(cir^.circ); cir:=cir^.next END
    END;
    l:=0; arc:=arcs;
    IF arcs=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); arc:=arc^.next UNTIL arc=arcs;
      wr(l); arc:=arcs;
      FOR i:=0 TO l-1 DO plot_arc(arc^.arc); arc:=arc^.next END
    END;
    l:=0; ell:=ellps;
    IF ellps=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); ell:=ell^.next UNTIL ell=ellps;
      wr(l); ell:=ellps;
      FOR i:=0 TO l-1 DO plot_ellp(ell^.ellips); ell:=ell^.next END
    END;
    l:=0; pln:=plins;
    IF plins=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); pln:=pln^.next UNTIL pln=plins;
      wr(l); pln:=plins;
      FOR i:=0 TO l-1 DO plot_pline(pln^.pline); pln:=pln^.next END
    END;
    l:=0; txt:= texts;
    IF texts=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); txt:=txt^.next UNTIL txt=texts;
      wr(l); txt:=texts;
      FOR i:=0 TO l-1 DO plot_txt(txt^.text); txt:=txt^.next END
    END;
    l:=0; srf:=surfs;
    IF surfs=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); srf:=srf^.next UNTIL srf=surfs;
      wr(l); srf:=surfs;
      FOR i:=0 TO l-1 DO plot_surf(srf^.surf); srf:=srf^.next END
    END;
    l:=0; grp:=grps;
    IF grps=NIL THEN  wr(0)
    ELSE
      REPEAT INC(l); grp:=grp^.next UNTIL grp=grps;
      wr(l); grp:=grps;
      FOR i:=0 TO l-1 DO plot_group(grp^.group); grp:=grp^.next END
    END
  END
END plot_lay;

PROCEDURE plot_file(file_name: ARRAY OF CHAR);
  VAR l,i: INTEGER;

BEGIN
  bio.create(file,file_name,'w',0);
  IF NOT bio.done THEN
    bpm.perror(bio.error,240,180,'ошибка при записи %s',file_name); RETURN
  END;
  wr(HIGH(HEAD)+1); write(HEAD,BYTES(HEAD));
  l:=0;
  FOR i:=0 TO HIGH(bas.cview^.model^.mbody^) DO
    IF bas.cview^.cntx^.mask[i]#0 THEN INC(l) END
  END;
  wr(l);
  FOR i:=0 TO HIGH(bas.cview^.model^.mbody^) DO
    IF bas.cview^.cntx^.mask[i]#0  THEN
      plot_lay(bas.cview^.model^.mbody^[i])
    END
  END;
  WITH bas.cview^ DO
    wr(Xc);
    wr(Yc);
    wr(Zc);
    wr(X_lf);
    wr(Y_dw);
    wr(scale);
  END;
  bio.close(file)
END plot_file;

PROCEDURE plot(x,y: INTEGER);
  VAR n,fn: ARRAY [0..128] OF CHAR;
BEGIN
  IF file?(x,y,n,fn,'.plt') THEN plot_file(fn) END
END plot;

----------------------------------------------------------------


PROCEDURE init;
BEGIN
  name:='bcad.bdf';
  NEW(HEAD,2);
  HEAD[0]:= vers;
  NEW(BHEAD,2);
  BHEAD[0]:= vers;
END init;

BEGIN init
END bcFile.
