IMPLEMENTATION MODULE mxObj; (* Ned 15-Oct-87. (c) KRONOS *)
                             (* Ned 28-Jul-88. (c) KRONOS *)

IMPORT  inter: coolSystem;
IMPORT   scan: mxScan;

WITH STORAGE: inter;

VAR types: TypePtr; -- list of type values

-------------------------  NEW & REM  ------------------------
                         -------------

VAR
  null_obj : Obj;
  null_type: TypeVal;

PROCEDURE NewObj(VAR o: ObjPtr; m: Mode);
BEGIN
  NEW(o);
  o^:=null_obj; o^.mode:=m;
END NewObj;

PROCEDURE NewType(VAR t: TypePtr; m: TypeMode);
BEGIN
  NEW(t);
  t^:=null_type; t^.mode:=m;
  t^.next:=types; types:=t;
END NewType;

PROCEDURE NewHeader(VAR x: header_ptr);
BEGIN
  NEW(x);
  x^.next:=NIL; x^.bound:=NIL; x^.self:=NIL; x^.locals:=NIL;
  x^.extno:=0; x^.ctime:=-1;
END NewHeader;

PROCEDURE RemType(VAR t: TypePtr);
  VAR p: TypePtr;
BEGIN
  IF types=t THEN types:=t^.next;
  ELSE p:=types; ASSERT(p#NIL);
    WHILE p^.next#t DO p:=p^.next; ASSERT(p#NIL) END;
    p^.next:=t^.next;
  END;
  DISPOSE(t);
END RemType;

PROCEDURE release_list(o: ObjPtr); FORWARD;

PROCEDURE release_types(end: TypePtr);
  VAR l,n: TypePtr;
BEGIN
  l:=types;
  WHILE l#end DO
    IF l^.mode=rec THEN
      release_list(l^.head^.locals); DISPOSE(l^.head)
    ELSIF l^.mode IN PROCs THEN
      release_list(l^.plist);
    END;
    n:=l^.next; DISPOSE(l); l:=n;
  END;
  types:=end;
END release_types;

PROCEDURE release_obj(o: ObjPtr);
BEGIN
  ASSERT(o#NIL);
  IF (o^.mode=modul) & NOT (duplicate IN o^.tags) THEN
    release_list(o^.head^.locals); DISPOSE(o^.head);
  END;
  IF o^.mode#param THEN undoDcl(o) END;
  DISPOSE(o);
END release_obj;

PROCEDURE release_list(l: ObjPtr);
  VAR next: ObjPtr;
BEGIN
  WHILE l#NIL DO next:=l^.next; release_obj(l); l:=next END;
END release_list;

-------------------------  VISIBILITY  -----------------------
                         --------------

VAR obs: DYNARR OF ObjPtr;
   levs: header_ptr;

PROCEDURE ini_vis;
  VAR i: INTEGER;
BEGIN
  levs:=NIL;
  FOR i:=0 TO HIGH(obs) DO obs[i]:=NIL END;
END ini_vis;

PROCEDURE flush(o: ObjPtr); BEGIN o^.mode:=inv; o^.type:=Any END flush;

PROCEDURE Vis?(id: INTEGER; VAR o: ObjPtr): BOOLEAN;
  VAR l: ObjPtr; x: header_ptr;
BEGIN x:=levs;
  REPEAT
    l:=obs[id];
    WHILE l#NIL DO
      IF l^.host=x THEN
        IF undefined IN l^.tags THEN scan.err_id(17,id); flush(l) END;
        o:=l; RETURN TRUE
      END; l:=l^.sake;
    END;
    IF x^.self^.mode=modul THEN -- ищем проникающие
      l:=obs[id]; x:=SuperProc^.head;
      WHILE l#NIL DO
        IF (l^.host=x) & (penetrate IN l^.tags) THEN
          IF undefined IN l^.tags THEN scan.err_id(17,id); flush(l) END;
          o:=l; RETURN TRUE
        END; l:=l^.sake;
      END; o:=Ilg; RETURN FALSE
    ELSE x:=x^.next;
    END;
  UNTIL x=NIL;
  o:=Ilg; RETURN FALSE
END Vis?;

PROCEDURE Vis(id: INTEGER): ObjPtr;
  VAR o: ObjPtr;
BEGIN
  IF NOT Vis?(id,o) THEN
    NewObj(o,inv); o^.type:=Any; Dcl(id,o); scan.err_id(14,id);
  END; RETURN o
END Vis;

PROCEDURE VisInScope?(scope: header_ptr; id: INTEGER; VAR o: ObjPtr): BOOLEAN;
  VAR l: ObjPtr;
BEGIN
  l:=obs[id];
  WHILE l#NIL DO
    IF l^.host=scope THEN
      IF undefined IN l^.tags THEN scan.err_id(17,id); flush(l) END;
      o:=l; RETURN TRUE
    END; l:=l^.sake;
  END; RETURN FALSE
END VisInScope?;

PROCEDURE VisInScope(scope: header_ptr; id: INTEGER): ObjPtr;
  VAR o: ObjPtr;
BEGIN
  IF NOT VisInScope?(scope,id,o) THEN
    NewObj(o,inv); o^.type:=Any; DclInScope(scope,id,o);
    scan.err_id(14,id);
  END; RETURN o
END VisInScope;

PROCEDURE VisOut(id: INTEGER): ObjPtr;
  VAR o: ObjPtr; x: header_ptr; r: BOOLEAN;
BEGIN
  x:=levs; levs:=levs^.next;
    r:=Vis?(id,o);
  levs:=x;
  IF NOT r THEN scan.err_id(14,id); RETURN Ilg END;
  RETURN o
END VisOut;

PROCEDURE Dcl(id: INTEGER; o: ObjPtr);
  VAR old,b: ObjPtr;
BEGIN
  IF Vis?(id,old) THEN
--    IF (old^.host=levs)OR(CurBlock^.mode=modul)&(penetrate IN old^.tags) THEN
    IF old^.host=levs THEN
      scan.err_id(15,id); RETURN
    END;
  END;
  o^.id:=id;
  o^.host:=levs;
  o^.next:=levs^.locals; levs^.locals:=o;
  o^.sake:=obs[id]; obs[id]:=o;
END Dcl;

PROCEDURE DclInScope(scope: header_ptr; id: INTEGER; o: ObjPtr);
BEGIN
  IF VisInScope?(scope,id,o) THEN scan.err_id(15,id); RETURN END;
  o^.id:=id; o^.sake:=obs[id]; obs[id]:=o;
  o^.host:=scope; o^.next:=scope^.locals; scope^.locals:=o;
END DclInScope;

PROCEDURE undoDcl(o: ObjPtr);
  VAR l: ObjPtr;
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
END undoDcl;

PROCEDURE locals(): ObjPtr;
BEGIN RETURN levs^.locals
END locals;

---------------------------  BLOCKS  -------------------------
                           ----------

PROCEDURE EnterBlock(block: ObjPtr);
  VAR x: header_ptr;
BEGIN ASSERT(block^.mode IN Modes{inv,proc,modul});
  NewHeader(x); x^.self:=block; x^.bound:=types;
  x^.next:=levs; levs:=x;
  block^.head:=x;
  CurBlock:=block;
END EnterBlock;

PROCEDURE ExitProc;
  VAR x: header_ptr;
BEGIN ASSERT(CurBlock^.mode IN Modes{proc,inv});
  x:=levs; levs:=levs^.next;
  IF levs#NIL THEN CurBlock:=levs^.self ELSE CurBlock:=Ilg END;
  release_list(x^.locals);
  release_types(x^.bound);
  DISPOSE(x);
END ExitProc;

PROCEDURE ExitModule(vos: Conflict);

  VAR exp: ObjPtr; notqua?: BOOLEAN;

  PROCEDURE export(o: ObjPtr);
    VAR new: ObjPtr;
  BEGIN
    EXCL(o^.tags,exported);
    o^.next:=exp; exp:=o;
    IF notqua? THEN
      IF Vis?(o^.id,new) THEN vos(new,o);
      ELSE
        NewObj(new,o^.mode); new^:=o^; Dcl(o^.id,new);
        INCL(new^.tags,duplicate);
      END;
    END;
  END export;

  VAR l,next: ObjPtr; x: header_ptr;
BEGIN ASSERT(CurBlock^.mode IN Modes{modul,inv});
  x:=levs; levs:=levs^.next; ASSERT(levs#NIL);
  CurBlock:=levs^.self;
  l:=x^.locals;
  exp:=NIL; notqua?:=NOT (qualified IN x^.self^.tags);
  WHILE l#NIL DO
    next:=l^.next;
    IF exported IN l^.tags THEN export(l) ELSE release_obj(l) END;
    l:=next;
  END;
  x^.locals:=exp;
END ExitModule;

-------------------------  TRANSPORT  ------------------------
                         -------------

PROCEDURE Import(o: ObjPtr; id: INTEGER);
  VAR l: ObjPtr;
BEGIN
  NewObj(l,o^.mode); l^:=o^; Dcl(id,l);
  INCL(l^.tags,duplicate);
  IF (o^.mode=typ) & (o^.type^.mode=enum) THEN
    l:=o^.type^.list;
    WHILE l#NIL DO Import(l,l^.id); l:=l^.list END;
  END;
END Import;

PROCEDURE FromImport(mdl: ObjPtr; id: INTEGER);
  VAR o: ObjPtr;
BEGIN
  IF VisInScope?(mdl^.head,id,o) THEN Import(o,o^.id)
  ELSE NewObj(o,inv); o^.type:=Any; scan.err_id(14,id)
  END;
END FromImport;

PROCEDURE Export(id: INTEGER);
  VAR o,l: ObjPtr;
BEGIN
  IF NOT Vis?(id,o) THEN scan.err_id(14,id); RETURN END;
  INCL(o^.tags,exported);
  IF (o^.mode=typ) & (o^.type^.mode=enum) THEN
    l:=o^.type^.list;
    WHILE l#NIL DO INCL(l^.tags,exported); l:=l^.list END;
  END;
END Export;

-------------------------  EXTERNALS  ------------------------
                         -------------

VAR externals: header_ptr;

PROCEDURE lookupModule(id: INTEGER; VAR mdl: ObjPtr);
  VAR l,x: header_ptr;
BEGIN
  IF VisInScope?(SuperProc^.head,id,mdl) THEN
    IF mdl^.mode#modul THEN scan.Fault(42,''); mdl:=Ilg END; RETURN
  END;
  NewHeader(x);
  NewObj(mdl,modul); mdl^.head:=x; mdl^.addr:=0;
  x^.self:=mdl; x^.extno:=extNo; INC(extNo);
  l:=externals;
  IF l=NIL THEN externals:=x;
  ELSE
    WHILE l^.next#NIL DO l:=l^.next END;
    l^.next:=x;
  END;
  DclInScope(SuperProc^.head,id,mdl);
END lookupModule;

PROCEDURE iterModules(ip: iterProc);
  VAR l: header_ptr;
BEGIN l:=externals;
  WHILE l#NIL DO ip(l^.self); l:=l^.next END;
END iterModules;

PROCEDURE ini_externals;
BEGIN externals:=NIL; extNo:=1;
END ini_externals;

-----------------------  ПРОВЕРКИ ТИПОВ  ----------------------
                       ------------------

PROCEDURE chkType(o: ObjPtr);
BEGIN
  IF NOT (o^.mode IN Modes{inv,typ}) THEN
    scan.err_id(19,o^.id); o^.mode:=inv; o^.type:=Any;
  END;
END chkType;

PROCEDURE Scalar?(t: TypePtr): BOOLEAN;
BEGIN RETURN t^.mode IN ScalarTypes END Scalar?;

PROCEDURE chkScalar(VAR t: TypePtr);
BEGIN
  IF NOT (t^.mode IN ScalarTypes) THEN scan.err(22); t:=Any END;
END chkScalar;

PROCEDURE Simple?(t: TypePtr): BOOLEAN;
BEGIN RETURN t^.mode IN SimpleTypes END Simple?;

PROCEDURE chkSimple(VAR t: TypePtr);
BEGIN
  IF NOT (t^.mode IN SimpleTypes) THEN scan.err(24); t:=Any END;
END chkSimple;

PROCEDURE LoHi(t: TypePtr; VAR lo,hi: INTEGER);
BEGIN
  CASE t^.mode OF
   |invtype : lo:=0; hi:=0
   |int,word: lo:=MIN(INTEGER); hi:=MAX(INTEGER);
   |addr    : lo:=0; hi:=MAX(INTEGER);
   |char    : lo:=0; hi:=255;
   |bool    : lo:=0; hi:=1;
   |enum    : lo:=0; hi:=t^.size-1;
   |rang    : lo:=t^.min; hi:=t^.size
  ELSE scan.err(22); lo:=0; hi:=0
  END;
END LoHi;

PROCEDURE tsize(t: TypePtr): INTEGER; (* never return 0! *)
BEGIN
  IF    t^.mode IN SimpleTypes           THEN RETURN 1
  ELSIF t^.mode IN Types{arr,rec,dynarr} THEN RETURN t^.size
  ELSE ASSERT(FALSE,100h+ORD(t^.mode));
  END
END tsize;

PROCEDURE Char?(t: TypePtr): BOOLEAN;
BEGIN
  IF t^.mode=rang THEN t:=t^.base END;
  RETURN t^.mode=char
END Char?;

--------------------------  HIDDENs  --------------------------
                          -----------

PROCEDURE set_hidden(hid,type: TypePtr);

  PROCEDURE scan_obj_list(l: ObjPtr; hid,type: TypePtr);
  BEGIN
    WHILE l#NIL DO
      IF l^.mode=modul THEN scan_obj_list(l^.head^.locals,hid,type)
      ELSIF (l^.mode IN Modes{vari,field,typ,param}) & (l^.type=hid) THEN
        l^.type:=type
      END;
      l:=l^.next;
    END;
  END scan_obj_list;

  PROCEDURE scan_type_list(hid,type: TypePtr);
    VAR l: TypePtr;
  BEGIN l:=types;
    WHILE l#NIL DO
      CASE l^.mode OF
        |functype: scan_obj_list(l^.plist,hid,type);
                   IF l^.base=hid THEN l^.base:=type END;
        |proctype: scan_obj_list(l^.plist,hid,type);
        |arr     : IF l^.base=hid THEN l^.base:=type END;
                   IF l^.inx =hid THEN l^.inx :=type END;
        |rec     : scan_obj_list(l^.head^.locals,hid,type);
        |ptr,settype,farr,dynarr
                 : IF l^.base=hid THEN l^.base:=type END;
      ELSE (* nothing *)
      END;
      l:=l^.next;
    END;
  END scan_type_list;

BEGIN
  IF NOT Simple?(type) OR Char?(type) THEN scan.err(59); RETURN END;
  scan_type_list(hid,type);
  scan_obj_list(levs^.locals,hid,type);
END set_hidden;

---------------------  КОНСТРУКТОРЫ ТИПОВ  --------------------
                     ----------------------

CONST MaxArrSize  =1000000h;  BitsPerWord = 32;

PROCEDURE MakeArr(inx,elem: TypePtr): TypePtr;
  VAR l,h,sz: INTEGER; t: TypePtr;
BEGIN
  chkScalar(inx);
  IF inx^.mode IN Types{addr,word,int} THEN scan.err(23); RETURN Any END;
  LoHi(inx,l,h); sz:=h-l+1;
  IF Char?(elem) THEN
    IF sz>MaxArrSize*4 THEN scan.err(60); RETURN Any END;
    sz:=(sz+3) DIV 4;
  ELSIF sz>(MaxArrSize DIV tsize(elem)) THEN scan.err(60); RETURN Any
  ELSE sz:=sz*tsize(elem);
  END;
  NewType(t,arr);
  t^.base:=elem; t^.inx:=inx;
  t^.size:=sz;
  RETURN t
END MakeArr;

PROCEDURE MakeSet(base: TypePtr): TypePtr;
  VAR l,h: INTEGER; t: TypePtr;
BEGIN
  chkScalar(base); LoHi(base,l,h);
  IF (l<0) OR (h>=BitsPerWord) THEN scan.err(23); RETURN Any END;
  NewType(t,settype);
  t^.base:=base;
  RETURN t
END MakeSet;

PROCEDURE MakeFlx(elem: TypePtr): TypePtr;
  VAR t: TypePtr;
BEGIN
  NewType(t,farr); t^.base:=elem; RETURN t
END MakeFlx;

PROCEDURE MakePtr(base: TypePtr): TypePtr;
  VAR t: TypePtr;
BEGIN
  NewType(t,ptr); t^.base:=base; t^.tid:=-1; RETURN t
END MakePtr;

PROCEDURE MakeFwdPtr(id: INTEGER): TypePtr;
  VAR t: TypePtr;
BEGIN
  NewType(t,ptr); t^.base:=NIL; t^.tid:=id; RETURN t
END MakeFwdPtr;

PROCEDURE MakeRange(base: TypePtr; lo,hi: INTEGER): TypePtr;
  VAR t: TypePtr;
BEGIN chkScalar(base);
  IF lo>hi THEN lo:=0; hi:=0; scan.err(9) END;
  NewType(t,rang);
  IF base^.mode=rang THEN t^.base:=base^.base ELSE t^.base:=base END;
  t^.min:=lo; t^.size:=hi;
  RETURN t
END MakeRange;

PROCEDURE MakeHidden(size: INTEGER): TypePtr;
  VAR t: TypePtr;
BEGIN ASSERT(size=1);
  NewType(t,hidden); t^.size:=1; RETURN t
END MakeHidden;

PROCEDURE MakeEnum(): TypePtr;
  VAR t: TypePtr;
BEGIN
  NewType(t,enum); t^.size:=0; t^.list:=NIL; RETURN t
END MakeEnum;

PROCEDURE AppEnum(t: TypePtr; val: INTEGER): ObjPtr;
  VAR o: ObjPtr;
BEGIN
  NewObj(o,econs);
  o^.type:=t; o^.addr:=val; INC(t^.size);
  o^.list:=NIL;
  RETURN o
END AppEnum;

PROCEDURE MakeRec(): TypePtr;
  VAR t: TypePtr;
BEGIN
  NewType(t,rec); t^.size:=0; NewHeader(t^.head); RETURN t
END MakeRec;

PROCEDURE AppField(rec: TypePtr; id: INTEGER; f: TypePtr; ofs: INTEGER);
  VAR o: ObjPtr;
BEGIN
  NewObj(o,field); o^.type:=f;
  o^.addr:=ofs; INC(ofs,tsize(f));
  IF rec^.size<ofs THEN rec^.size:=ofs END;
  DclInScope(rec^.head,id,o);
END AppField;

PROCEDURE MakeDynArr(base: TypePtr; dim: INTEGER): TypePtr;
  VAR t: TypePtr;
BEGIN ASSERT(dim=1);
  NewType(t,dynarr); t^.base:=base; t^.size:=dim+1;
  t^.desc:=MakeRec();
  AppField(t^.desc,scan.str_id("ADR"),addrp,0);
  AppField(t^.desc,scan.str_id("HIGH"),intp,1);
  RETURN t
END MakeDynArr;

PROCEDURE MakeProcType(): TypePtr;
  VAR t: TypePtr;
BEGIN
  NewType(t,invtype);  -- see ProcType, FuncType
  t^.base:=NIL; t^.plist:=NIL;
  RETURN t
END MakeProcType;

PROCEDURE AppParm(proc: TypePtr; id: INTEGER; t: TypePtr; k: BITSET): ObjPtr;
  VAR p: ObjPtr;
BEGIN
  NewObj(p,param);
  p^.tags:=k; p^.type:=t; p^.id:=id;
  p^.next:=proc^.plist; proc^.plist:=p;
  RETURN p
END AppParm;

PROCEDURE inverse(proc: TypePtr);
  VAR a,b,c: ObjPtr;
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

PROCEDURE ProcType(proc: TypePtr);
BEGIN inverse(proc); proc^.mode:=proctype;
END ProcType;

PROCEDURE FuncType(proc,res: TypePtr);
BEGIN inverse(proc); proc^.base:=res; proc^.mode:=functype;
END FuncType;

---------------------  TYPE COMPATIBILITY  --------------------
                     ----------------------

PROCEDURE TypeCmp0(VAR t1,t2: TypePtr): BOOLEAN;
  VAR m1,m2: TypeMode;
BEGIN ASSERT((t1#NIL) & (t2#NIL));
  IF t1^.mode=rang THEN t1:=t1^.base END;
  IF t2^.mode=rang THEN t2:=t2^.base END;
  m1:=t1^.mode; m2:=t2^.mode;
  IF (t1=t2) OR (m1=invtype) OR (m2=invtype) THEN RETURN TRUE END;
  RETURN (m1=addr) & (m2 IN Types{int,ptr})
      OR (m2=addr) & (m1 IN Types{int,ptr})
END TypeCmp0;

PROCEDURE TypeCmp(t1,t2: TypePtr);
BEGIN
  IF (t1=t2) OR TypeCmp0(t1,t2) THEN RETURN END;
  scan.err(21);
END TypeCmp;

PROCEDURE ProcCmp?(t1,t2: TypePtr): INTEGER;

  PROCEDURE GeneralCmp(t1,t2: TypePtr; VAR err: INTEGER);
  BEGIN err:=0;
    IF t1=t2 THEN
    ELSIF t1^.mode IN PROCs THEN err:=ProcCmp?(t1,t2)
    ELSIF t1=wordp THEN chkSimple(t2)
    ELSIF t2=wordp THEN chkSimple(t1)
    ELSIF NOT TypeCmp0(t1,t2) THEN err:=21
    END;
  END GeneralCmp;

  CONST par_tags = {varpar,seqpar};

  VAR p1,p2: ObjPtr; err: INTEGER;
BEGIN
  IF (t1^.mode#t2^.mode) OR NOT (t1^.mode IN PROCs) THEN RETURN 21
  ELSIF t1^.mode=functype THEN
    GeneralCmp(t1^.base,t2^.base,err); IF err#0 THEN RETURN err END;
  END;
  p1:=t1^.plist; p2:=t2^.plist;
  WHILE (p1#NIL) & (p2#NIL) DO
    IF p1^.tags*par_tags#p2^.tags*par_tags THEN RETURN 21 END;
    t1:=p1^.type; t2:=p2^.type;
    IF t1^.mode=farr THEN
      IF t2^.mode=farr THEN t1:=t1^.base; t2:=t2^.base;
        GeneralCmp(t1,t2,err); IF err#0 THEN RETURN err END;
      ELSE RETURN 21
      END;
    ELSE
      GeneralCmp(t1,t2,err); IF err#0 THEN RETURN err END;
    END;
    IF p1^.addr#p2^.addr THEN RETURN 74 END;
    p1:=p1^.next; p2:=p2^.next;
  END;
  IF p1#p2 THEN RETURN 49 END;
  RETURN 0
END ProcCmp?;

PROCEDURE ProcCmp(t1,t2: TypePtr);
  VAR n: INTEGER;
BEGIN n:=ProcCmp?(t1,t2);
  IF n#0 THEN scan.err(n) END;
END ProcCmp;

PROCEDURE BaseCmp(t1,t2: TypePtr);
  VAR l1,h1,l2,h2: INTEGER;
BEGIN
  IF (t1^.mode=rang) OR (t2^.mode=rang) THEN
    LoHi(t1,l1,h1);
    LoHi(t2,l2,h2);
    IF (l2<l1) OR (h2>h1) THEN scan.err(21) END;
  ELSE
    TypeCmp(t1,t2);
  END;
END BaseCmp;

PROCEDURE AsgCmp(t1,t2: TypePtr);
BEGIN
  IF (t1=t2) OR TypeCmp0(t1,t2) THEN
  ELSIF t1^.mode=word           THEN chkSimple(t2);
  ELSIF t2^.mode=word           THEN chkSimple(t1);
  ELSIF t1^.mode IN PROCs       THEN ProcCmp(t1,t2);
  ELSIF (t1^.mode IN ARRs) & (t2^.mode IN ARRs) THEN
    IF (t1^.mode IN DYNs) OR (t2^.mode IN DYNs) THEN
      BaseCmp(t1^.base,t2^.base);
    ELSIF Char?(t1^.base) & Char?(t2^.base) & (tsize(t1)>=tsize(t2)) THEN
      BaseCmp(t1^.base,t2^.base);
    ELSE scan.err(21);
    END;
  ELSE scan.err(21);
  END;
END AsgCmp;

------------------------ ИНИЦИАЛИЗАЦИЯ ------------------------

PROCEDURE Ini;
BEGIN
  NEW(obs,scan.noIdents);
  types:=NIL;
  ini_vis;
  ini_externals;
  NewObj(SuperProc,proc);
  SuperProc^.type:=NIL; SuperProc^.addr:=0;
  EnterBlock(SuperProc);
  NewType(Any,invtype);  Any^.base:=Any;
  NewObj(Ilg,inv); Ilg^.type:=Any;
END Ini;

PROCEDURE Exi;
BEGIN
  ExitProc;
  DISPOSE(SuperProc);
  DISPOSE(Ilg);
  DISPOSE(obs);
END Exi;

BEGIN
  WITH null_obj DO
    mode:=inv;  id:=scan.DmId;  tags:={};
    sake:=NIL;  next:=NIL;      host:=NIL;
    scope:=0;   addr:=0;        head:=NIL;
  END;
  WITH null_type DO
    mode:=invtype;  obj :=NIL;  ref :=-1;   size:=1;
    next:=NIL;      base:=NIL;  head:=NIL;
  END;
END mxObj.
