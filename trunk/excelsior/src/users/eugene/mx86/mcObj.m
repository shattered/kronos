IMPLEMENTATION MODULE mcObj; (* Ned 15-Oct-87. (c) KRONOS *)
                             (* Ned 28-Jul-88. (c) KRONOS *)

IMPORT  inter: pcSystem;
IMPORT    ers: coErrors;
IMPORT   scan: mcScan;
IMPORT     pc: pcTab;

WITH STORAGE: inter;

VAR types: type_ptr;

-------------------------  NEW & REM  ------------------------
                         -------------

VAR
  null_obj : obj_rec;
  null_type: type_rec;

PROCEDURE new_obj(VAR o: obj_ptr; m: Mode);
BEGIN
  NEW(o);
  o^:=null_obj; o^.mode:=m;
END new_obj;

PROCEDURE new_type(VAR t: type_ptr; m: TypeMode);
BEGIN
  NEW(t);
  t^:=null_type; t^.mode:=m;
  t^.next:=types; types:=t;
END new_type;

PROCEDURE new_header(VAR x: header_ptr);
BEGIN
  NEW(x);
  x^.next:=NIL; x^.self:=NIL; x^.locals:=NIL;
  x^.ctime:=-1;
END new_header;

-------------------------  VISIBILITY  -----------------------
                         --------------

VAR obs: DYNARR OF obj_ptr;
   levs: header_ptr;

PROCEDURE ini_vis;
  VAR i: INTEGER;
BEGIN levs:=NIL;
  FOR i:=0 TO HIGH(obs) DO obs[i]:=NIL END;
END ini_vis;

PROCEDURE flush(o: obj_ptr); BEGIN o^.mode:=inv; o^.type:=Any END flush;

PROCEDURE vis?(id: INTEGER; VAR o: obj_ptr): BOOLEAN;
  VAR l: obj_ptr; x: header_ptr;
BEGIN x:=levs;
  REPEAT
    l:=obs[id];
    WHILE l#NIL DO
      IF l^.host=x THEN
        IF undefined IN l^.tags THEN
          scan.err_id(ers.recursive,id); flush(l)
        END;
        o:=l;
        RETURN TRUE
      END;
      l:=l^.sake;
    END;
    IF x^.self^.mode=module THEN -- ищем проникающие
      l:=obs[id]; x:=SuperProc^.head;
      WHILE l#NIL DO
        IF (l^.host=x) & (penetrate IN l^.tags) THEN
          IF undefined IN l^.tags THEN
            scan.err_id(ers.recursive,id); flush(l)
          END;
          o:=l;
          RETURN TRUE
        END;
        l:=l^.sake;
      END;
      o:=Ilg;
      RETURN FALSE
    ELSE x:=x^.next;
    END;
  UNTIL x=NIL;
  o:=Ilg;
  RETURN FALSE
END vis?;

PROCEDURE find_glo(nm: ARRAY OF CHAR): pc.ref;
  CONST valid = Modes{cons,econs,vari};
  VAR l: obj_ptr; x: header_ptr; id: INTEGER;
BEGIN
  x:=CurBlock^.head;
  id:=scan.str_id(nm);
  IF (id<0) OR (id>HIGH(obs)) THEN RETURN NIL END;
  l:=obs[id];
  WHILE l#NIL DO
    IF l^.host=x THEN
      IF undefined IN l^.tags THEN RETURN NIL END;
      IF l^.mode IN valid THEN RETURN l^.gen ELSE RETURN NIL END;
    END;
    l:=l^.sake;
  END;
  RETURN NIL;
END find_glo;

PROCEDURE vis(id: INTEGER): obj_ptr;
  VAR o: obj_ptr;
BEGIN
  IF NOT vis?(id,o) THEN
    new_obj(o,inv); o^.type:=Any; dcl(id,o); scan.err_id(ers.invisible,id);
  END;
  RETURN o
END vis;

PROCEDURE vis_in?(scope: header_ptr; id: INTEGER; VAR o: obj_ptr): BOOLEAN;
  VAR l: obj_ptr;
BEGIN
  l:=obs[id];
  WHILE l#NIL DO
    IF l^.host=scope THEN
      IF undefined IN l^.tags THEN scan.err_id(ers.recursive,id); flush(l) END;
      o:=l;
      RETURN TRUE
    END;
    l:=l^.sake;
  END;
  RETURN FALSE
END vis_in?;

PROCEDURE vis_in(scope: header_ptr; id: INTEGER): obj_ptr;
  VAR o: obj_ptr;
BEGIN
  IF NOT vis_in?(scope,id,o) THEN
    new_obj(o,inv); o^.type:=Any; dcl_in(scope,id,o);
    scan.err_id(ers.invisible,id);
  END;
  RETURN o
END vis_in;

PROCEDURE vis_out(id: INTEGER): obj_ptr;
  VAR o: obj_ptr; x: header_ptr; r: BOOLEAN;
BEGIN
  x:=levs; levs:=levs^.next;
    r:=vis?(id,o);
  levs:=x;
  IF NOT r THEN scan.err_id(ers.invisible,id); RETURN Ilg END;
  RETURN o
END vis_out;

PROCEDURE dcl(id: INTEGER; o: obj_ptr);
  VAR old,b: obj_ptr;
BEGIN
  IF vis?(id,old) & (old^.host=levs) THEN
    scan.err_id(ers.duplicate,id);
  ELSE
    o^.id:=id;
    o^.host:=levs;
    o^.next:=levs^.locals; levs^.locals:=o;
    o^.sake:=obs[id]; obs[id]:=o;
  END;
END dcl;

PROCEDURE dcl_in(scope: header_ptr; id: INTEGER; o: obj_ptr);
BEGIN
  IF vis_in?(scope,id,o) THEN scan.err_id(ers.duplicate,id); RETURN END;
  o^.id:=id; o^.sake:=obs[id]; obs[id]:=o;
  o^.host:=scope; o^.next:=scope^.locals; scope^.locals:=o;
END dcl_in;

PROCEDURE undo_dcl(o: obj_ptr);
  VAR l: obj_ptr;
BEGIN
  IF obs[o^.id]=o THEN obs[o^.id]:=o^.sake; RETURN END;
  l:=obs[o^.id];
  IF l#NIL THEN
    WHILE l^.sake#NIL DO
      IF o=l^.sake THEN l^.sake:=o^.sake; RETURN END;
      l:=l^.sake;
    END;
  END;
  ASSERT(o^.id=scan.DmId);
END undo_dcl;

---------------------------  BLOCKS  -------------------------
                           ----------

PROCEDURE EnterBlock(block: obj_ptr);
BEGIN
  ASSERT(block^.mode IN Modes{inv,proc,module});
  IF block^.head=NIL THEN
    new_header(block^.head); block^.head^.self:=block;
  END;
  block^.head^.next:=levs; levs:=block^.head;
  CurBlock:=block;
END EnterBlock;

PROCEDURE ExitProc;
  VAR x: header_ptr; l: obj_ptr;
BEGIN
  ASSERT(CurBlock^.mode IN Modes{proc,inv});
  x:=levs; levs:=levs^.next; x^.next:=NIL;
  IF levs#NIL THEN CurBlock:=levs^.self ELSE CurBlock:=Ilg END;
  l:=x^.locals;
  WHILE l#NIL DO undo_dcl(l); l:=l^.next END;
END ExitProc;

PROCEDURE ExitModule(vos: Conflict);

  PROCEDURE export(o: obj_ptr);
    VAR new: obj_ptr;
  BEGIN
    IF vis?(o^.id,new) THEN vos(new,o);
    ELSE
      new_obj(new,o^.mode); new^:=o^; dcl(o^.id,new);
      INCL(new^.tags,duplicate);
    END;
  END export;

  VAR l: obj_ptr; x: header_ptr; not_qua: BOOLEAN;
BEGIN
  ASSERT(CurBlock^.mode IN Modes{module,inv});
  x:=levs; levs:=levs^.next; ASSERT(levs#NIL);
  x^.next:=NIL;
  CurBlock:=levs^.self;
  not_qua:=NOT (qualified IN x^.self^.tags);
  l:=x^.locals;
  WHILE l#NIL DO
    IF exported IN l^.tags THEN
      l^.tags:=l^.tags-{exported};
      IF not_qua THEN export(l) END;
    ELSE undo_dcl(l);
    END;
    l:=l^.next;
  END;
END ExitModule;

-------------------------  TRANSPORT  ------------------------
                         -------------

PROCEDURE Import(o: obj_ptr; id: INTEGER);
  VAR l: obj_ptr;
BEGIN
  new_obj(l,o^.mode); l^:=o^; dcl(id,l);
  INCL(l^.tags,duplicate);
  IF (o^.mode=type) & (o^.type^.mode=enum) THEN
    l:=o^.type^.list;
    WHILE l#NIL DO Import(l,l^.id); l:=l^.list END;
  END;
END Import;

PROCEDURE FromImport(mdl: obj_ptr; id: INTEGER);
  VAR o: obj_ptr;
BEGIN
  IF vis_in?(mdl^.head,id,o) THEN Import(o,o^.id)
  ELSE new_obj(o,inv); o^.type:=Any; scan.err_id(ers.invisible,id)
  END;
END FromImport;

PROCEDURE Export(id: INTEGER);
  VAR o,l: obj_ptr;
BEGIN
  IF NOT vis?(id,o) THEN scan.err_id(ers.invisible,id); RETURN END;
  INCL(o^.tags,exported);
  IF (o^.mode=type) & (o^.type^.mode=enum) THEN
    l:=o^.type^.list;
    WHILE l#NIL DO INCL(l^.tags,exported); l:=l^.list END;
  END;
END Export;

-------------------------  EXTERNALS  ------------------------
                         -------------

VAR modno: INTEGER;

PROCEDURE lookupModule(id: INTEGER; VAR mdl: obj_ptr): BOOLEAN;
  VAR l,x: header_ptr;
BEGIN
  IF vis_in?(SuperProc^.head,id,mdl) THEN
    IF mdl^.mode#module THEN scan.Fault(ers.module,''); mdl:=Ilg END;
    RETURN FALSE
  END;
  new_header(x);
  new_obj(mdl,module); mdl^.head:=x;
  mdl^.no:=modno; INC(modno);
  x^.self:=mdl;
  l:=externals;
  IF l=NIL THEN externals:=x;
  ELSE
    WHILE l^.next#NIL DO l:=l^.next END;
    l^.next:=x;
  END;
  dcl_in(SuperProc^.head,id,mdl);
  RETURN TRUE
END lookupModule;

-----------------------  ПРОВЕРКИ ТИПОВ  ----------------------
                       ------------------

PROCEDURE chkType(o: obj_ptr);
BEGIN
  IF NOT (o^.mode IN Modes{inv,type}) THEN
    scan.err(ers.type); o^.mode:=inv; o^.type:=Any;
  END;
END chkType;

PROCEDURE chkScalar(VAR t: type_ptr);
BEGIN
  IF NOT (t^.mode IN SCALARs) THEN scan.err(ers.scalar); t:=Any END;
END chkScalar;

--------------------------  HIDDENs  --------------------------
                          -----------

PROCEDURE set_hidden(hid,typ: type_ptr);

  PROCEDURE scan_obj_list(l: obj_ptr; hid,typ: type_ptr);
  BEGIN
    WHILE l#NIL DO
      IF l^.mode=module THEN scan_obj_list(l^.head^.locals,hid,typ)
      ELSIF (l^.mode IN Modes{vari,field,type,param}) & (l^.type=hid) THEN
        l^.type:=typ
      END;
      l:=l^.next;
    END;
  END scan_obj_list;

  PROCEDURE scan_type_list(hid,type: type_ptr);
    VAR l: type_ptr;
  BEGIN
    l:=types;
    WHILE l#NIL DO
      CASE l^.mode OF
        |functype: scan_obj_list(l^.plist,hid,type);
                   IF l^.base=hid THEN l^.base:=type END;
        |proctype: scan_obj_list(l^.plist,hid,type);
        |arr     : IF l^.base=hid THEN l^.base:=type END;
                   IF l^.inx =hid THEN l^.inx :=type END;
        |rec     : IF l^.head#NIL THEN
                     scan_obj_list(l^.head^.locals,hid,type);
                   END;
        |ptr,settype,farr,dynarr
                 : IF l^.base=hid THEN l^.base:=type END;
      ELSE (* nothing *)
      END;
      l:=l^.next;
    END;
  END scan_type_list;

BEGIN
  scan_type_list(hid,typ);
  scan_obj_list(levs^.locals,hid,typ);
END set_hidden;

---------------------  КОНСТРУКТОРЫ ТИПОВ  --------------------
                     ----------------------

PROCEDURE MakeArr(inx,elem: type_ptr): type_ptr;
  VAR t: type_ptr;
BEGIN
  chkScalar(inx);
  new_type(t,arr);
  t^.base:=elem; t^.inx:=inx;
  RETURN t
END MakeArr;

PROCEDURE MakeSet(base: type_ptr): type_ptr;
  VAR l,h: INTEGER; t: type_ptr;
BEGIN
  chkScalar(base);
  new_type(t,settype);
  t^.base:=base;
  RETURN t
END MakeSet;

PROCEDURE MakeFlx(elem: type_ptr): type_ptr;
  VAR t: type_ptr;
BEGIN
  new_type(t,farr); t^.base:=elem; RETURN t
END MakeFlx;

PROCEDURE MakePtr(base: type_ptr): type_ptr;
  VAR t: type_ptr;
BEGIN
  new_type(t,ptr); t^.base:=base; t^.tid:=-1; RETURN t
END MakePtr;

PROCEDURE MakeFwdPtr(id: INTEGER): type_ptr;
  VAR t: type_ptr;
BEGIN
  new_type(t,ptr); t^.base:=NIL; t^.tid:=id; RETURN t
END MakeFwdPtr;

PROCEDURE MakeRange(base: type_ptr; mode: TypeMode): type_ptr;
  VAR t: type_ptr;
BEGIN
  ASSERT(mode IN Types{range,newrange});
  chkScalar(base);
  new_type(t,mode);
  IF base^.mode=range THEN t^.base:=base^.base ELSE t^.base:=base END;
  RETURN t
END MakeRange;

PROCEDURE MakeHidden(size: INTEGER): type_ptr;
  VAR t: type_ptr;
BEGIN
  ASSERT(size=1);
  new_type(t,hidden); RETURN t
END MakeHidden;

PROCEDURE MakeEnum(): type_ptr;
  VAR t: type_ptr;
BEGIN
  new_type(t,enum); t^.list:=NIL; RETURN t
END MakeEnum;

PROCEDURE AppEnum(t: type_ptr): obj_ptr;
  VAR o: obj_ptr;
BEGIN
  new_obj(o,econs);
  o^.type:=t;
  o^.list:=NIL;
  RETURN o
END AppEnum;

PROCEDURE MakeRec(): type_ptr;
  VAR t: type_ptr;
BEGIN
  new_type(t,rec); new_header(t^.head); RETURN t
END MakeRec;

PROCEDURE AppField(rec: type_ptr; id: INTEGER; f: type_ptr): obj_ptr;
  VAR o: obj_ptr;
BEGIN
  new_obj(o,field); o^.type:=f;
  dcl_in(rec^.head,id,o);
  RETURN o
END AppField;

PROCEDURE MakeDynArr(base: type_ptr; dim: INTEGER): type_ptr;
  VAR t: type_ptr; f: obj_ptr;
BEGIN
  ASSERT(dim=1);
  new_type(t,dynarr); t^.base:=base;
  new_type(t^.desc,dyndesc);
  RETURN t
END MakeDynArr;

PROCEDURE MakeProcType(): type_ptr;
  VAR t: type_ptr;
BEGIN
  new_type(t,invtype);  -- see ProcType, FuncType
  t^.base:=NIL; t^.plist:=NIL;
  RETURN t
END MakeProcType;

PROCEDURE AppParm(proc: type_ptr; id: INTEGER; t: type_ptr; k: BITSET): obj_ptr;
  VAR p: obj_ptr;
BEGIN
  new_obj(p,param);
  p^.tags:=k; p^.type:=t; p^.id:=id;
  p^.gen:=NIL;
  p^.next:=proc^.plist; proc^.plist:=p;
  RETURN p
END AppParm;

PROCEDURE inverse(proc: type_ptr);
  VAR a,b,c: obj_ptr;
BEGIN
  b:=proc^.plist;
  IF b=NIL THEN RETURN END;
  a:=NIL; c:=b^.next;
  WHILE c#NIL DO
    b^.next:=a;
    a:=b; b:=c; c:=b^.next;
  END;
  b^.next:=a;
  proc^.plist:=b;
END inverse;

PROCEDURE ProcType(proc: type_ptr);
BEGIN inverse(proc); proc^.mode:=proctype;
END ProcType;

PROCEDURE FuncType(proc,res: type_ptr);
BEGIN inverse(proc); proc^.base:=res; proc^.mode:=functype;
END FuncType;

------------------------ ИНИЦИАЛИЗАЦИЯ ------------------------

PROCEDURE Ini;
BEGIN
  NEW(obs,scan.noIdents);
  types:=NIL;
  ini_vis;
  externals:=NIL;
  modno:=1;
  new_obj(SuperProc,proc);
  SuperProc^.type:=NIL;
  EnterBlock(SuperProc);
  new_type(Any,invtype);  Any^.base:=Any;
  new_obj(Ilg,inv); Ilg^.type:=Any;
END Ini;

PROCEDURE Exi;
BEGIN
  ExitProc;
  DISPOSE(obs);
END Exi;

BEGIN
  WITH null_obj DO
    mode:=inv;  id:=scan.DmId;  tags:={};
    sake:=NIL;  next:=NIL;      host:=NIL;
    no  :=0;    head:=NIL;      gen :=NIL;
  END;
  WITH null_type DO
    mode:=invtype;  obj :=NIL;  ref :=-1;
    base:=NIL;      head:=NIL;  next:=NIL;
    gen :=NIL;
  END;
  pc.find_var:=find_glo;
END mcObj.
