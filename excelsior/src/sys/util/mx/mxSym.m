IMPLEMENTATION MODULE mxSym; (* Ned 21-May-88. (c) KRONOS *)

IMPORT   sys: SYSTEM;
IMPORT   sym: coolSym;
IMPORT  comp: coolDefs;
IMPORT inter: coolSystem;
IMPORT  scan: mxScan;
IMPORT   obs: mxObj;
IMPORT   gen: mxGen;

WITH STORAGE: inter;

CONST Modula_X = 'Modula-X';

----------------------------------------------------------------

VAR
  io   : comp.io_ptr;
  buf  : STRING;
  pos  : INTEGER;
  refNo: INTEGER;
  def? : BOOLEAN;

PROCEDURE flush;
BEGIN
  IF scan.fault THEN RETURN END;
  ASSERT(io^.done);
(*$<U+*)
  io^.buf^:=buf^;
(*$>*)
  io^.len :=pos;
  io^.doio(io);
  IF NOT io^.done THEN scan.fault:=TRUE END;
END flush;

PROCEDURE p(b: INTEGER);
BEGIN
  IF pos>HIGH(buf) THEN flush; pos:=0 END;
  buf[pos]:=CHAR(b); INC(pos);
END p;

PROCEDURE p2(b: INTEGER); BEGIN p(b MOD 100h); p(b DIV 100h) END p2;

PROCEDURE p4(b: INTEGER);
BEGIN
  p2(INTEGER(BITSET(b)*{0..15}));
  p2(INTEGER(BITSET(b>>16)*{0..15}));
END p4;

PROCEDURE pX(b: INTEGER);
BEGIN
  IF    (b>=-120) & (b<=127) THEN p(b+128)
  ELSIF (b>=0) & (b<=255)    THEN p(0); p(b)
  ELSIF (b<=0) & (b>=-255)   THEN p(1); p(-b)
  ELSIF (b>=0) & (b<0FFFFh)  THEN p(2); p2(b)
  ELSE                            p(3); p4(b);
  END;
END pX;

PROCEDURE pName(VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(name)) & (name[i]#0c) DO p(ORD(name[i])); INC(i) END;
  p(0);
END pName;

---------------------------------------------------------------

PROCEDURE put_header(def,imp: INTEGER);
  VAR head: sym.header;
BEGIN
  head.magic   :=sym.MAGIC;
  head.vers    :=sym.VERSION;
  head.offset  :=BYTES(head);
  head.def_time:=def;
  head.imp_time:=imp;
(*$<U+*)
  io^.buf^.ADR :=sys.ADR(head);
  io^.buf^.HIGH:=BYTES(head)-1;
(*$>*)
  io^.len      :=BYTES(head);
  io^.doio(io);
END put_header;

PROCEDURE inverse(VAR l: obs.ObjPtr);
  VAR a,b,c: obs.ObjPtr;
BEGIN
  b:=l;
  IF b=NIL THEN RETURN END;
  a:=NIL; c:=b^.next;
  WHILE c#NIL DO
    b^.next:=a;
    a:=b; b:=c; c:=b^.next;
  END;
  b^.next:=a;
  l:=b;
END inverse;

PROCEDURE import(o: obs.ObjPtr);
  VAR name: ARRAY [0..255] OF CHAR;
BEGIN
  ASSERT(o^.mode=obs.modul);
  p(sym.import); scan.id_str(o^.id,name); pName(name); pName(Modula_X);
  pX(o^.head^.ctime); p(sym.def); pX(o^.head^.extno);
END import;

PROCEDURE createsym(VAL name: ARRAY OF CHAR; unit,def_time,imp_time: INTEGER);
  VAR u: INTEGER;
BEGIN
  refNo:=32;
  def?:=(unit=comp.def);
  IF def? THEN u:=comp.sym_ou ELSE u:=comp.ref END;
  inter.ini(io,name,u,scan.io_fault);
  IF NOT io^.done THEN scan.fault:=TRUE; RETURN END;
  put_header(def_time,imp_time);
  IF NOT io^.done THEN scan.fault:=TRUE; RETURN END;
  NEW(buf,256); pos:=0;
  p(sym.import); pName(name); pName(Modula_X); pX(def_time);
  IF    unit=comp.def  THEN p(sym.def);
  ELSIF unit=comp.imp  THEN p(sym.imp);
  ELSIF unit=comp.main THEN p(sym.prog)
  ELSE ASSERT(FALSE);
  END;
  pX(0);
  obs.iterModules(import);
END createsym;

PROCEDURE closesym;
BEGIN
  IF scan.fault OR (scan.noErrors#0) THEN
    io^.done:=FALSE
  ELSE
    p(sym.end_CU); pX(gen.proc_no); pX(gen.local_no);
    p(sym.eosf);
    IF pos>0 THEN flush END;
  END;
  inter.exi(io);
  DISPOSE(buf);
END closesym;

PROCEDURE put_xpos(proc_no,pc,line,col: INTEGER);
BEGIN p(sym.xpos); pX(proc_no); pX(pc); p2(line); p(col);
END put_xpos;

PROCEDURE outscope;

  VAR ptrs: inter.QUEUE;
      name: ARRAY [0..255] OF CHAR;

  PROCEDURE ident(id: INTEGER);
  BEGIN scan.id_str(id,name); pName(name) END ident;

  PROCEDURE get_kind(kind: BITSET): INTEGER;
    VAR c: BITSET; i: INTEGER;
  BEGIN c:={};
    IF obs.varpar IN kind THEN INCL(c,sym.varparam) END;
    IF obs.seqpar IN kind THEN INCL(c,sym.seqparam) END;
    IF obs.RO     IN kind THEN INCL(c,sym.readonly) END;
    kind:=kind-{0..obs.private-1}; i:=obs.private;
    WHILE kind#{} DO
      IF i IN kind THEN
        EXCL(kind,i); INCL(c,i-obs.private+sym.private)
      END; INC(i);
    END;
    RETURN INTEGER(c)
  END get_kind;

  PROCEDURE outType(t: obs.TypePtr): INTEGER;

    PROCEDURE outParms(parm: obs.ParmPtr);
      VAR r: INTEGER; l: obs.ParmPtr;
    BEGIN l:=parm;
      WHILE l#NIL DO r:=outType(l^.type); l:=l^.next END;
      l:=parm;
      WHILE l#NIL DO
        p(sym.parm); pName(''); pX(l^.type^.ref);
        pX(l^.addr); pX(get_kind(l^.kind));
        l:=l^.next;
      END;
    END outParms;

    PROCEDURE outFields(f: obs.ObjPtr);
      VAR r: INTEGER; l: obs.ObjPtr;
    BEGIN l:=f;
      WHILE l#NIL DO r:=outType(l^.type); l:=l^.next END;
      l:=f;
      WHILE l#NIL DO
        p(sym.field); ident(l^.id); pX(l^.type^.ref); pX(l^.addr);
        l:=l^.next;
      END;
    END outFields;

    PROCEDURE inverse_enum(VAR l: obs.ObjPtr);
      VAR a,b,c: obs.ObjPtr;
    BEGIN
      b:=l;
      IF b=NIL THEN RETURN END;
      a:=NIL; c:=b^.list;
      WHILE c#NIL DO
        b^.list:=a;
        a:=b; b:=c; c:=b^.list;
      END;
      b^.list:=a;
      l:=b;
    END inverse_enum;

    PROCEDURE outEnums(l: obs.ObjPtr; type,modno: INTEGER);
    BEGIN
      WHILE l#NIL DO
        p(sym.enum); ident(l^.id); pX(type); pX(modno); pX(l^.addr);
        l:=l^.list;
      END;
    END outEnums;

    VAR b,i: INTEGER;
  BEGIN
    IF t^.ref>0 THEN RETURN t^.ref END;
    CASE t^.mode OF
      | obs.enum    : p(sym.enumtype);
      | obs.rec     : IF def? THEN inverse(t^.head^.locals) END;
                      outFields(t^.head^.locals);
                      p(sym.record); pX(sym.nan); pX(t^.size);
      | obs.ptr     : inter.push(ptrs,t); p(sym.pointer);
      | obs.arr     : i:=outType(t^.inx); b:=outType(t^.base);
                      p(sym.array); pX(i); pX(b);
      | obs.proctype: outParms(t^.plist);
                      p(sym.proctype);
      | obs.functype: b:=outType(t^.base); outParms(t^.plist);
                      p(sym.functype); pX(b);
      | obs.rang    : b:=outType(t^.base);
                      p(sym.range); pX(b); pX(t^.min); pX(t^.size);
      | obs.farr    : b:=outType(t^.base); p(sym.openarr); pX(b);
      | obs.dynarr  : b:=outType(t^.base); p(sym.dynarr);  pX(b); pX(t^.size-1)
      | obs.settype : b:=outType(t^.base); p(sym.set);     pX(b);
      | obs.hidden  : p(sym.hidden); pX(t^.size);
    ELSE ASSERT(FALSE);
    END;
    IF t^.obj#NIL THEN
      p(sym.type); ident(t^.obj^.id); pX(refNo); pX(t^.obj^.scope);
    END;
    t^.ref:=refNo; INC(refNo);
    IF t^.mode=obs.enum THEN
      IF t^.obj=NIL THEN i:=0 ELSE i:=t^.obj^.scope END;
      IF def? THEN inverse_enum(t^.list) END;
      outEnums(t^.list,t^.ref,i)
    END;
    RETURN refNo-1
  END outType;

  PROCEDURE out_struct(o: obs.ObjPtr);
    VAR r: INTEGER; struct: BOOLEAN;
  BEGIN
    r:=outType(o^.type);
    struct:=def? & (o^.scope=0);
    IF struct THEN p(sym.struct) ELSE p(sym.sconst) END;
    ident(o^.id); pX(r); pX(-o^.scope); pX(o^.addr);
    IF struct THEN pX(obs.tsize(o^.type)*4); gen.get_struct(o,p) END;
  END out_struct;

  PROCEDURE outObj(o: obs.ObjPtr);
    VAR r: INTEGER;
  BEGIN
    CASE o^.mode OF
      |obs.cons : IF obs.Simple?(o^.type) THEN r:=outType(o^.type);
                  p(sym.const); ident(o^.id); pX(r); pX(o^.addr);
                  ELSE out_struct(o);
                  END;
      |obs.vari : r:=outType(o^.type);
                  p(sym.var); ident(o^.id); pX(r);
                  pX(o^.scope); pX(o^.addr); pX(get_kind(o^.tags));
      |obs.proc : IF obs.code_proc IN o^.tags THEN RETURN END;
                  r:=outType(o^.type);
                  p(sym.proc);  ident(o^.id); pX(r);
                  pX(o^.scope); pX(o^.addr);
      |obs.modul: p(sym.module); ident(o^.id); pX(o^.addr);
      |obs.field: (* nothing *)
      |obs.econs: (* nothing *)
      |obs.inv  : ASSERT(FALSE);
    ELSE ASSERT(FALSE);
    END;
  END outObj;

  VAR l: obs.ObjPtr; t: obs.TypePtr; r: INTEGER; block: obs.header_ptr;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  inter.fifo(ptrs);
  block:=obs.CurBlock^.head;
  IF def? THEN inverse(block^.locals) END;
  l:=block^.locals;
  WHILE l#NIL DO
    IF (l^.mode=obs.typ) & NOT (obs.duplicate IN l^.tags) THEN
      r:=outType(l^.type);
      IF l^.type^.obj#l THEN
        p(sym.type); ident(l^.id); pX(r); pX(l^.scope);
      END;
    END;
    l:=l^.next;
  END;
  l:=block^.locals;
  WHILE l#NIL DO
    IF (l^.mode#obs.typ) & NOT (obs.duplicate IN l^.tags) THEN outObj(l) END;
    l:=l^.next;
  END;
  WHILE inter.pop(ptrs,t) DO
    r:=outType(t^.base); p(sym.linkage); pX(t^.ref); pX(r);
  END;
  inter.clear(ptrs);
  IF    obs.CurBlock^.mode=obs.proc  THEN p(sym.endproc)
  ELSIF obs.CurBlock^.mode=obs.modul THEN p(sym.endmodule)
  ELSE ASSERT(FALSE)
  END;
  pX(obs.CurBlock^.addr);
END outscope;

---------------------------------------------------------------

PROCEDURE g(VAR b: sys.WORD);
BEGIN b:=ORD(buf[pos]); INC(pos);
END g;

PROCEDURE get(): INTEGER;
BEGIN INC(pos); RETURN ORD(buf[pos-1]);
END get;

PROCEDURE g2(VAR b: INTEGER);
BEGIN b:=ORD(buf[pos])+ORD(buf[pos+1])*100h; INC(pos,2);
END g2;

PROCEDURE g4(VAR b: INTEGER);
  VAR h,l: INTEGER;
BEGIN g2(l); g2(h);
  b:=INTEGER( BITSET(l)+BITSET(h<<16) );
END g4;

PROCEDURE gX(VAR b: sys.WORD);
BEGIN
  b:=ORD(buf[pos]); INC(pos);
  IF    INTEGER(b)>=8 THEN b:=INTEGER(b)-128
  ELSIF INTEGER(b)=0  THEN g(b)
  ELSIF INTEGER(b)=1  THEN g(b); b:=-INTEGER(b);
  ELSIF INTEGER(b)=2  THEN g2(b)
  ELSIF INTEGER(b)=3  THEN g4(b)
  ELSE ASSERT(FALSE)
  END
END gX;

PROCEDURE gName(VAR name: ARRAY OF CHAR);
  VAR i: INTEGER; c: CHAR;
BEGIN
  i:=0;
  c:=buf[pos]; INC(pos);
  WHILE (i<HIGH(name)) & (c#0c) DO
    name[i]:=c; c:=buf[pos]; INC(pos); INC(i)
  END;
  ASSERT(c=0c); (* temporary: must be error message *)
  name[i]:=c;
END gName;

PROCEDURE read_sym(VAR mdl: obs.ObjPtr);

  VAR imps: DYNARR OF obs.ObjPtr;
     impNo: INTEGER;
     types: DYNARR OF obs.TypePtr;
    typeNo: INTEGER;
      name: ARRAY [0..255] OF CHAR;

  PROCEDURE clear_types;
    VAR i: INTEGER;
  BEGIN
    FOR i:=typeNo TO HIGH(types) DO types[i]:=NIL END;
  END clear_types;

  PROCEDURE newtype(t: obs.TypePtr);
  BEGIN
    IF typeNo>HIGH(types) THEN
      RESIZE(types,HIGH(types)+129);
      clear_types;
    END;
    ASSERT(types[typeNo]=NIL);
    t^.ref:=typeNo; types[typeNo]:=t; INC(typeNo);
  END newtype;

  PROCEDURE type?(VAR t: obs.TypePtr);
    VAR typeno: INTEGER;
  BEGIN gX(typeno); t:=types[typeno]; ASSERT(t#NIL);
  END type?;

  PROCEDURE module?(impno: INTEGER): INTEGER;
  BEGIN ASSERT(impno<impNo);
    RETURN imps[impno]^.head^.extno
  END module?;

  PROCEDURE ini;
    VAR i: INTEGER;
  BEGIN
    typeNo:=32; impNo:=0;
    NEW(imps,32);
    FOR i:=0 TO HIGH(imps)  DO imps[i] :=NIL END;
    NEW(types,256);
    FOR i:=0 TO HIGH(types) DO types[i]:=NIL END;
    types[sym.addr  ]:=obs.addrp;
    types[sym.word  ]:=obs.wordp;
    types[sym.int   ]:=obs.intp;
    types[sym.char  ]:=obs.charp;
    types[sym.real  ]:=obs.realp;
    types[sym.bool  ]:=obs.boolp;
    types[sym.bitset]:=obs.bitsetp;
    types[sym.string]:=obs.stringp;
  END ini;

  PROCEDURE id?(): INTEGER;
  BEGIN gName(name); RETURN scan.str_id(name)
  END id?;

  PROCEDURE linkage(p,base: obs.TypePtr);
  BEGIN
    ASSERT((p^.mode=obs.ptr) & (p^.tid=scan.DmId));
    p^.base:=base;
  END linkage;

  PROCEDURE replace(new,old: obs.TypePtr);

    PROCEDURE Parms(p: obs.ParmPtr);
      VAR d: obs.ParmPtr;
    BEGIN
      WHILE p#NIL DO d:=p; p:=p^.next; DISPOSE(d) END;
    END Parms;

    PROCEDURE Fields(f: obs.ObjPtr);
      VAR d: obs.ObjPtr;
    BEGIN
      WHILE f#NIL DO d:=f; f:=f^.next; obs.undoDcl(d); DISPOSE(d) END;
    END Fields;

  BEGIN
    IF new=old THEN RETURN END;
    types[new^.ref]:=old;
    CASE new^.mode OF
      | obs.rec     : Fields(new^.head^.locals); DISPOSE(new^.head);
      | obs.arr     : replace(new^.inx ,old^.inx );
                      replace(new^.base,old^.base);
      | obs.proctype: Parms(new^.plist);
      | obs.functype: Parms(new^.plist); replace(new^.base,old^.base);
      | obs.rang,obs.farr,obs.dynarr,obs.settype:
                      replace(new^.base,old^.base);
      | obs.hidden,obs.enum,obs.ptr   : (*nothing*)
    ELSE ASSERT(FALSE);
    END;
    obs.RemType(new);
  END replace;

  PROCEDURE declare(VAR o: obs.ObjPtr; modno: INTEGER);
    VAR m,old: obs.ObjPtr;
  BEGIN
    m:=imps[modno]; ASSERT(m#NIL);
    IF obs.VisInScope?(m^.head,o^.id,old) THEN
      ASSERT(o^.mode=old^.mode);
      IF o^.mode IN obs.Modes{obs.vari,obs.cons,obs.proc,obs.typ} THEN
        replace(o^.type,old^.type);
        DISPOSE(o);
      END;
    ELSE obs.DclInScope(m^.head,o^.id,o);
    END;
  END declare;

  PROCEDURE exit;
    VAR i: INTEGER;
  BEGIN
    FOR i:=32 TO typeNo-1 DO types[i]^.ref:=-1 END;
    DISPOSE(types);
    DISPOSE(imps);
  END exit;

  PROCEDURE import;

    PROCEDURE time_err(time: INTEGER);
      VAR msg: ARRAY [0..79] OF CHAR;
    BEGIN
      inter.sprint(msg,'Приходите после ');
      inter.app_time(msg,time);
      scan.Fault(0,'%s',msg);
    END time_err;

    VAR o: obs.ObjPtr; time,no: INTEGER;
  BEGIN
    obs.lookupModule(id?(),o);
    IF o^.mode#obs.modul THEN RETURN END;
    IF impNo>HIGH(imps) THEN RESIZE(imps,HIGH(imps)+33) END;
    imps[impNo]:=o; INC(impNo);
    gName(name);
    IF name#Modula_X THEN scan.Fault(62,'-- %s',name); RETURN END;
    gX(time);
    IF time>inter.time() THEN time_err(time); RETURN END;
    IF (o^.head^.ctime>=0) & (o^.head^.ctime#time) THEN
      scan.id_str(o^.id,name);
      scan.Fault(78,'%s',name);
      RETURN
    END;
    o^.head^.ctime:=time;
    g(no);
    IF no#sym.def THEN scan.Fault(61,'') END;
    gX(no);
    ASSERT(no=impNo-1);
  END import;

  PROCEDURE set_kind(x: INTEGER; VAR tags: BITSET);
    VAR c: BITSET; i: INTEGER;
  BEGIN c:=BITSET(x); tags:={};
    IF sym.varparam IN c THEN INCL(tags,obs.varpar) END;
    IF sym.seqparam IN c THEN INCL(tags,obs.seqpar) END;
    IF sym.readonly IN c THEN INCL(tags,obs.RO    ) END;
    c:=c-{0..sym.private-1}; i:=sym.private;
    WHILE c#{} DO
      IF i IN c THEN
        EXCL(c,i); INCL(tags,i-sym.private+obs.private);
      END;
      INC(i);
    END;
  END set_kind;

  VAR tag: INTEGER;
    t1,t2: obs.TypePtr;
       id: INTEGER;
        o: obs.ObjPtr;
    a1,a2: INTEGER;
      val: INTEGER;
    modno: INTEGER;
        p: obs.ParmPtr;
       eT: obs.TypePtr;  -- enum type
       rT: obs.TypePtr;  -- record type
       pT: obs.TypePtr;  -- proc type

BEGIN ini; eT:=NIL; rT:=NIL; pT:=NIL;
  LOOP
    tag:=ORD(buf[pos]); INC(pos);
    CASE tag OF

    |sym.enumtype: eT:=obs.MakeEnum(); newtype(eT);
    |sym.range   : type?(t1); gX(a1); gX(a2); newtype(obs.MakeRange(t1,a1,a2));
    |sym.array   : type?(t1); type?(t2); newtype(obs.MakeArr(t1,t2));
    |sym.openarr : type?(t1); newtype(obs.MakeFlx(t1));
    |sym.dynarr  : type?(t1); gX(a1); newtype(obs.MakeDynArr(t1,a1));
    |sym.pointer : newtype(obs.MakeFwdPtr(scan.DmId));
    |sym.record  : IF rT=NIL THEN rT:=obs.MakeRec() END;
                   gX(val); ASSERT(val=sym.nan);
                   gX(a1);
                   IF a1#rT^.size THEN ASSERT(FALSE) END;
                   newtype(rT); rT:=NIL;
    |sym.set     : type?(t1); newtype(obs.MakeSet(t1));
    |sym.proctype: IF pT=NIL THEN pT:=obs.MakeProcType() END;
                   obs.ProcType(pT); newtype(pT); pT:=NIL;
    |sym.functype: IF pT=NIL THEN pT:=obs.MakeProcType() END;
                   type?(t1); obs.FuncType(pT,t1);
                   newtype(pT); pT:=NIL
    |sym.hidden  : gX(a1); newtype(obs.MakeHidden(a1));
    |sym.linkage : type?(t1); type?(t2); linkage(t1,t2);

    |sym.parm    : IF pT=NIL THEN pT:=obs.MakeProcType() END;
                   g(a1); ASSERT(a1=0);  -- id:=id?();
                   type?(t1);
                   p:=obs.AppParm(pT,-1,t1,{});
                   gX(p^.addr); gX(a1); set_kind(a1,p^.kind);
    |sym.field   : IF rT=NIL THEN rT:=obs.MakeRec() END;
                   id:=id?(); type?(t1); gX(a1); obs.AppField(rT,id,t1,a1);
    |sym.enum    : id:=id?(); type?(t1); gX(modno); gX(val);
                   IF t1=eT THEN
                     o:=obs.AppEnum(eT,val); o^.id:=id; declare(o,modno);
                   END;
    |sym.const   : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.cons); gX(o^.addr);
                   o^.id:=id; o^.type:=t1; declare(o,0);
    |sym.struct  : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.cons); o^.id:=id; o^.type:=t1;
                   gX(a1); modno:=module?(a1);
                   gX(a1); gX(a2); gen.extStruct(o,modno,a1,a2,get);
                   declare(o,0);
    |sym.sconst  : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.cons); o^.id:=id; o^.type:=t1;
                   gX(a1); modno:=module?(a1);
                   gX(a1);gen.extStruct(o,modno,a1,0,get);
                   declare(o,0);
    |sym.var     : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.vari); o^.id:=id; o^.type:=t1;
                   gX(a1); ASSERT(a1=0); -- level=0
                   modno:=module?(0); gX(val);
                   gX(a1); set_kind(a1,o^.tags);
                   IF modno=0 THEN EXCL(o^.tags,obs.RO) END;
                   gen.extVar(o,modno,val); declare(o,0);
    |sym.proc    : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.proc); o^.id:=id; o^.type:=t1;
                   gX(a1); ASSERT(a1=0); -- level=0
                   modno:=module?(0);
                   IF modno=0 THEN INCL(o^.tags,obs.forward) END;
                   gX(a1); gen.extProc(o,modno,a1); declare(o,0);
    |sym.type    : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.typ); o^.id:=id; o^.type:=t1;
                   gX(modno); o^.scope:=module?(modno);
                   declare(o,modno);
                   IF (o#NIL) & (o^.type^.obj=NIL) THEN o^.type^.obj:=o END;

    |sym.import   : import
    |sym.module   : ASSERT(FALSE,51h);
    |sym.endmodule: gX(modno);
    |sym.end_CU   : IF module?(0)=0 THEN
                      gX(a1); gX(a2); gen.set_counts(a1,a2)
                    END;
                    mdl:=imps[0]; exit; RETURN
    ELSE
      scan.Fault(61,'tag=%$h',tag); RETURN
    END;
  END;
END read_sym;

PROCEDURE getsym(id: INTEGER);
  VAR  o: obs.ObjPtr;
    name: ARRAY [0..255] OF CHAR;
    head: POINTER TO sym.header;
BEGIN
  o:=NIL;
  scan.id_str(id,name);
  inter.ini(io,name,comp.sym_in,scan.io_fault);
  IF NOT io^.done THEN scan.fault:=TRUE; RETURN END;
(*$<U+*)
  buf^:=io^.buf^;
  head:=buf^.ADR;
(*$>*)
  IF head^.magic#sym.MAGIC THEN
    scan.Fault(55,'"%s"',name)
  ELSIF head^.vers#sym.VERSION THEN
    scan.Fault(56,'"%s"',name)
  ELSE
    pos:=head^.offset;
    read_sym(o);
  END;
  inter.exi(io);
  NEW(buf);
  IF (o=NIL) OR  (o^.mode=obs.inv) OR (o^.id#id) THEN
    scan.Fault(61,'"%s"',name); RETURN
  END;
  INCL(o^.tags,obs.complete);
END getsym;

-------------------------  INI & EXI  -------------------------
                         -------------

PROCEDURE Ini;
BEGIN
  obs.addrp  ^.ref:=sym.addr;
  obs.wordp  ^.ref:=sym.word;
  obs.intp   ^.ref:=sym.int;
  obs.charp  ^.ref:=sym.char;
  obs.realp  ^.ref:=sym.real;
  obs.boolp  ^.ref:=sym.bool;
  obs.bitsetp^.ref:=sym.bitset;
  obs.stringp^.ref:=sym.string;
END Ini;

PROCEDURE Exi; END Exi;

BEGIN
  NEW(buf);
END mxSym.
