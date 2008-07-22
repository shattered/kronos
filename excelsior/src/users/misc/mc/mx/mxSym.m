IMPLEMENTATION MODULE mxSym; (* Ned 21-May-88. (c) KRONOS *)

IMPORT   sys: SYSTEM;
IMPORT   sym: coolSym;
IMPORT   new: coSym;
IMPORT    kr: krSym;
IMPORT  comp: coolDefs;
IMPORT inter: coolSystem;
IMPORT  scan: mxScan;
IMPORT   obs: mxObj;
IMPORT   gen: mxGen;

WITH STORAGE: inter;

CONST Modula_X = 'Modula-X';

----------------------------------------------------------------

CONST DIRECTs = obs.SimpleTypes+obs.Types{obs.farr};

VAR
  io   : comp.io_ptr;
  buf  : STRING;
  pos  : INTEGER;
  refNo: INTEGER;
  def? : BOOLEAN;

VAR
  av: kr.access;
  ap: kr.access_proc;
  ar: kr.access_range;
  ai: kr.access_import;

PROCEDURE flush;
BEGIN
  IF scan.fault THEN pos:=0; RETURN END;
  ASSERT(io^.done);
(*$<U+*)
  io^.buf^:=buf^;
(*$>*)
  io^.len :=pos;
  io^.doio(io);
  IF NOT io^.done THEN scan.fault:=TRUE END;
  pos:=0;
END flush;

PROCEDURE p(b: INTEGER);
BEGIN
  IF pos>HIGH(buf) THEN flush END;
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

PROCEDURE put(VAR x: ARRAY OF sys.WORD);
BEGIN
  IF scan.fault THEN RETURN END;
(*$<U+*)
  io^.buf^.ADR :=sys.ADR(x);
  io^.buf^.HIGH:=BYTES(x)-1;
(*$>*)
  io^.len      :=BYTES(x);
  io^.doio(io);
END put;

PROCEDURE access(VAR x: ARRAY OF sys.WORD);
  VAR s: STRING; i: INTEGER;
BEGIN
  pX(BYTES(x));
(*$<U+T-*)
  s^.ADR :=sys.ADR(x);
  s^.HIGH:=BYTES(x)-1;
  FOR i:=0 TO HIGH(s) DO
    IF pos>HIGH(buf) THEN flush END;
    buf[pos]:=s[i]; INC(pos);
  END;
(*$>*)
END access;

PROCEDURE put_header(def,imp: INTEGER);
  VAR head: new.header;
BEGIN
  head.magic   :=new.MAGIC;
  head.vers    :=new.VERSION;
  head.offset  :=BYTES(head);
  head.def_time:=def;
  head.imp_time:=imp;
  put(head);
END put_header;

PROCEDURE access_cu;
  VAR w: gen.WORDs;
BEGIN
  NEW(w);
  gen.get_access(w);
  access(w);
  DISPOSE(w);
END access_cu;

PROCEDURE import(o: obs.ObjPtr);
  VAR name: ARRAY [0..255] OF CHAR;
BEGIN
  ASSERT(o^.mode=obs.modul);
  p(new.import); scan.id_str(o^.id,name); pName(name); pName(Modula_X);
  pX(o^.head^.ctime); p(new.def);
  ai.def_time:=o^.head^.ctime;
  ai.offset  :=o^.head^.extno;
  access(ai);
END import;

PROCEDURE createsym(VAL name: ARRAY OF CHAR; unit,def_time,imp_time: INTEGER);
  VAR u: INTEGER;
BEGIN
  refNo:=32;
  def?:=(unit=comp.def);
  IF def? THEN u:=comp.sym_ou ELSE u:=comp.ref END;
  inter.ini(io,name,u,scan.io_fault);
  IF NOT io^.done THEN scan.fault:=TRUE; RETURN END;
  NEW(buf,256); pos:=0;
  put_header(def_time,imp_time);
  IF NOT io^.done THEN scan.fault:=TRUE; RETURN END;
  p(new.import); pName(name); pName(Modula_X); pX(def_time);
  IF    unit=comp.def  THEN p(new.def);
  ELSIF unit=comp.imp  THEN p(new.imp);
  ELSIF unit=comp.main THEN p(new.prog)
  ELSE ASSERT(FALSE);
  END;
  ai.def_time:=def_time;
  ai.offset  :=0;
  access(ai);
  obs.iterModules(import);
END createsym;

PROCEDURE closesym;
BEGIN
  IF scan.fault OR (scan.noErrors#0) THEN
    io^.done:=FALSE
  ELSE
    p(new.end_CU);
    access_cu;
    p(new.eosf);
    IF pos>0 THEN flush END;
  END;
  inter.exi(io);
  DISPOSE(buf);
END closesym;

PROCEDURE put_xpos(proc_no,pc,line,col: INTEGER);
BEGIN p(new.xpos); pX(proc_no); pX(pc); p2(line); p(col);
END put_xpos;

PROCEDURE outscope;

  VAR ptrs: inter.QUEUE;
      name: ARRAY [0..255] OF CHAR;

  PROCEDURE ident(id: INTEGER);
  BEGIN scan.id_str(id,name); pName(name) END ident;

  PROCEDURE get_kind(kind: BITSET): INTEGER;
    VAR c: BITSET;
  BEGIN c:={};
    IF obs.varpar IN kind THEN INCL(c,new.varparam) END;
    IF obs.seqpar IN kind THEN INCL(c,new.seqparam) END;
    IF obs.RO     IN kind THEN INCL(c,new.readonly) END;
    RETURN INTEGER(c)
  END get_kind;

  PROCEDURE var_access(o: obs.ObjPtr);
    VAR direct: BOOLEAN; addr: INTEGER;
  BEGIN
    IF (o^.type^.mode IN obs.SimpleTypes) THEN
      direct:=NOT (obs.varpar IN o^.tags)
    ELSE
      direct:=(o^.type^.mode=obs.farr) OR (obs.direct IN o^.tags);
    END;
    IF o^.mode=obs.param THEN av.lvl:=1 ELSE av.lvl:=o^.scope END;
    addr:=o^.addr;
    IF av.lvl=0 THEN
      IF direct THEN
        av.am:=kr.am_G;  av.disp:=addr*32; av.no:=0;
      ELSE
        av.am:=kr.am_aG; av.disp:=0;       av.no:=addr;
      END;
    ELSE
      IF addr<=0 THEN DEC(addr) END;
      IF direct THEN
        av.am:=kr.am_L;  av.disp:=addr*32; av.no:=0;
      ELSE
        av.am:=kr.am_aL; av.disp:=0;       av.no:=addr;
      END;
    END;
    access(av);
  END var_access;

  PROCEDURE outType(t: obs.TypePtr): INTEGER;

    PROCEDURE outParms(parm: obs.ObjPtr);
      VAR r: INTEGER; l: obs.ObjPtr;
    BEGIN
      l:=parm;
      WHILE l#NIL DO r:=outType(l^.type); l:=l^.next END;
      av.lvl:=1;
      l:=parm;
      WHILE l#NIL DO
        p(new.parm); pName(''); pX(l^.type^.ref);
        pX(get_kind(l^.tags));
        var_access(l);
        l:=l^.next;
      END;
    END outParms;

    PROCEDURE outFields(f: obs.ObjPtr);
      VAR r: INTEGER; l: obs.ObjPtr; a: kr.access;
    BEGIN
      l:=f;
      WHILE l#NIL DO r:=outType(l^.type); l:=l^.next END;
      av.am:=kr.am_adr; av.lvl:=0; av.no:=0;
      l:=f;
      WHILE l#NIL DO
        p(new.field); ident(l^.id); pX(l^.type^.ref);
        av.disp:=l^.addr*32;
        access(av);
        l:=l^.next;
      END;
    END outFields;

    PROCEDURE outEnums(l: obs.ObjPtr; type,modno: INTEGER);
      VAR a: kr.access;
    BEGIN
      av.am:=kr.am_imm; av.lvl:=0; av.disp:=0;
      WHILE l#NIL DO
        p(new.enum); ident(l^.id); pX(type); pX(modno);
        av.no:=l^.addr;
        access(av);
        l:=l^.list;
      END;
    END outEnums;

    PROCEDURE range(t: obs.TypePtr);
    BEGIN
      ar.l.am:=kr.am_imm; ar.l.lvl:=0; ar.l.disp:=0; ar.l.no:=t^.min;
      ar.r.am:=kr.am_imm; ar.r.lvl:=0; ar.r.disp:=0; ar.r.no:=t^.size;
      access(ar);
    END range;

    VAR b,i: INTEGER;
  BEGIN
    IF t^.ref>0 THEN RETURN t^.ref END;
    CASE t^.mode OF
      | obs.enum    : p(new.enumtype);
      | obs.rec     : outFields(t^.head^.locals);
                      p(new.record); pX(new.nan);
      | obs.ptr     : inter.push(ptrs,t); p(new.pointer);
      | obs.arr     : i:=outType(t^.inx); b:=outType(t^.base);
                      IF obs.Char?(t^.base) THEN p(new.packed) END;
                      p(new.array); pX(i); pX(b);
      | obs.proctype: outParms(t^.plist);
                      p(new.proctype);
      | obs.functype: b:=outType(t^.base); outParms(t^.plist);
                      p(new.functype); pX(b);
      | obs.rang    : b:=outType(t^.base);
                      p(new.range); pX(b); range(t);
      | obs.farr    : b:=outType(t^.base);
                      IF obs.Char?(t^.base) THEN p(new.packed) END;
                      p(new.openarr); pX(b);
      | obs.dynarr  : b:=outType(t^.base);
                      IF obs.Char?(t^.base) THEN p(new.packed) END;
                      p(new.dynarr);  pX(b); pX(t^.size-1)
      | obs.settype : b:=outType(t^.base); p(new.set);     pX(b);
      | obs.hidden  : p(new.hidden);
    ELSE ASSERT(FALSE);
    END;
    IF t^.obj#NIL THEN
      p(new.type); ident(t^.obj^.id); pX(refNo); pX(t^.obj^.scope);
    END;
    t^.ref:=refNo; INC(refNo);
    IF t^.mode=obs.enum THEN
      IF t^.obj=NIL THEN i:=0 ELSE i:=t^.obj^.scope END;
      outEnums(t^.list,t^.ref,i)
    END;
    RETURN refNo-1
  END outType;

  PROCEDURE outObj(o: obs.ObjPtr);

    PROCEDURE proc(p: obs.ObjPtr);
      VAR parm: obs.ObjPtr; locs: INTEGER;
    BEGIN
      locs:=3;
      parm:=p^.type^.plist;
      WHILE parm#NIL DO
        IF parm^.addr>locs THEN locs:=parm^.addr END;
        parm:=parm^.next;
      END;
      ap.mod:=0;
      ap.lvl:=o^.scope+1;
      ap.disp:=o^.addr;
      ap.loc_sz:=locs+1;
      access(ap);
    END proc;

    VAR r: INTEGER;
  BEGIN
    CASE o^.mode OF
      |obs.cons : r:=outType(o^.type);
                  p(new.const); ident(o^.id); pX(r); pX(-o^.scope);
                  IF obs.Simple?(o^.type) THEN
                    av.am:=kr.am_imm;
                    av.lvl:=0; av.disp:=0; av.no:=o^.addr;
                  ELSE
                    av.am:=kr.am_STR;
                    av.lvl:=-o^.scope; av.disp:=o^.addr*32; av.no:=0;
                  END;
                  access(av);
      |obs.vari : r:=outType(o^.type);
                  p(new.var); ident(o^.id); pX(r);
                  pX(get_kind(o^.tags));
                  var_access(o);
      |obs.proc : IF obs.code_proc IN o^.tags THEN RETURN END;
                  r:=outType(o^.type);
                  p(new.proc);  ident(o^.id); pX(r); pX(o^.addr);
                  proc(o);
      |obs.modul: p(new.module); ident(o^.id); pX(o^.addr);
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
  l:=block^.locals;
  WHILE l#NIL DO
    IF (l^.mode=obs.typ) & NOT (obs.duplicate IN l^.tags) THEN
      r:=outType(l^.type);
      IF l^.type^.obj#l THEN
        p(new.type); ident(l^.id); pX(r); pX(l^.scope);
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
    r:=outType(t^.base); p(new.linkage); pX(t^.ref); pX(r);
  END;
  inter.clear(ptrs);
  IF    obs.CurBlock^.mode=obs.proc  THEN p(new.endproc)
  ELSIF obs.CurBlock^.mode=obs.modul THEN p(new.endmodule)
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

PROCEDURE read_sym(new_vers: BOOLEAN; VAR mdl: obs.ObjPtr);

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

    PROCEDURE Parms(p: obs.ObjPtr);
      VAR d: obs.ObjPtr;
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

  PROCEDURE access_cu;
    VAR s: STRING; n,i: INTEGER;
  BEGIN
    gX(n);
    NEW(s,n);
    FOR i:=0 TO n-1 DO s[i]:=buf[pos]; INC(pos) END;
    gen.put_access(s);
    DISPOSE(s);
  END access_cu;

  PROCEDURE access(VAR x: ARRAY OF sys.WORD);
    VAR s: STRING; n,i: INTEGER;
  BEGIN
(*$<U+*)
    s^.ADR:=sys.ADR(x);
    s^.HIGH:=BYTES(x)-1;
(*$>*)
    gX(n);
    ASSERT(n=BYTES(x));
    FOR i:=0 TO n-1 DO s[i]:=buf[pos]; INC(pos) END;
  END access;

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
    IF new_vers THEN access(ai)
    ELSE gX(no); ASSERT(no=impNo-1);
    END;
  END import;

  PROCEDURE set_kind(x: INTEGER; VAR tags: BITSET);
    VAR c: BITSET;
  BEGIN c:=BITSET(x); tags:={};
    IF sym.varparam IN c THEN INCL(tags,obs.varpar) END;
    IF sym.seqparam IN c THEN INCL(tags,obs.seqpar) END;
    IF sym.readonly IN c THEN INCL(tags,obs.RO    ) END;
    IF NOT new_vers THEN
      IF sym.private IN c THEN INCL(tags,obs.direct) END;
    END;
  END set_kind;

  VAR tag: INTEGER;
    t1,t2: obs.TypePtr;
       id: INTEGER;
        o: obs.ObjPtr;
    a1,a2: INTEGER;
      val: INTEGER;
    modno: INTEGER;
        p: obs.ObjPtr;
       eT: obs.TypePtr;  -- enum type
       rT: obs.TypePtr;  -- record type
       pT: obs.TypePtr;  -- proc type
     tail: obs.ObjPtr;

BEGIN
  ini; eT:=NIL; rT:=NIL; pT:=NIL; tail:=NIL;
  LOOP
    tag:=ORD(buf[pos]); INC(pos);
    CASE tag OF

    |sym.enumtype: eT:=obs.MakeEnum(); newtype(eT); tail:=NIL;
    |sym.range   : type?(t1);
                   IF new_vers THEN
                     access(ar);
                     a1:=ar.l.no;
                     a2:=ar.r.no;
                   ELSE gX(a1); gX(a2);
                   END;
                   newtype(obs.MakeRange(t1,a1,a2));
    |sym.array   : type?(t1); type?(t2); newtype(obs.MakeArr(t1,t2));
    |sym.openarr : type?(t1); newtype(obs.MakeFlx(t1));
    |sym.dynarr  : type?(t1); gX(a1); newtype(obs.MakeDynArr(t1,a1));
    |sym.pointer : newtype(obs.MakeFwdPtr(scan.DmId));
    |sym.record  : IF rT=NIL THEN rT:=obs.MakeRec() END;
                   gX(val); ASSERT(val=sym.nan);
                   IF NOT new_vers THEN
                     gX(a1);
                     IF a1#rT^.size THEN ASSERT(FALSE) END;
                   END;
                   newtype(rT); rT:=NIL;
    |sym.set     : type?(t1); newtype(obs.MakeSet(t1));
    |sym.proctype: IF pT=NIL THEN pT:=obs.MakeProcType() END;
                   obs.ProcType(pT); newtype(pT); pT:=NIL;
    |sym.functype: IF pT=NIL THEN pT:=obs.MakeProcType() END;
                   type?(t1); obs.FuncType(pT,t1);
                   newtype(pT); pT:=NIL
    |sym.hidden  : IF NOT new_vers THEN gX(a1); ASSERT(a1=1); END;
                   newtype(obs.MakeHidden(1));
    |sym.linkage : type?(t1); type?(t2); linkage(t1,t2);

    |sym.parm    : IF pT=NIL THEN pT:=obs.MakeProcType() END;
                   g(a1); ASSERT(a1=0);  -- id:=id?();
                   type?(t1);
                   p:=obs.AppParm(pT,-1,t1,{});
                   IF NOT new_vers THEN gX(p^.addr) END;
                   gX(a1); set_kind(a1,p^.tags);
                   IF new_vers THEN
                     access(av);
                     p^.addr:=av.disp DIV 32;
                     IF p^.addr<0 THEN INC(p^.addr) END;
                   END;
    |sym.field   : IF rT=NIL THEN rT:=obs.MakeRec() END;
                   id:=id?(); type?(t1);
                   IF new_vers THEN
                     access(av); a1:=av.disp DIV 32;
                   ELSE gX(a1);
                   END;
                   obs.AppField(rT,id,t1,a1);
    |sym.enum    : id:=id?(); type?(t1); gX(modno);
                   IF new_vers THEN access(av); val:=av.no
                   ELSE gX(val);
                   END;
                   IF t1=eT THEN
                     o:=obs.AppEnum(eT,val);
                     IF tail=NIL THEN eT^.list:=o ELSE tail^.list:=o END;
                     tail:=o;
                     o^.id:=id;
                     declare(o,modno);
                   END;
    |sym.const   : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.cons);
                   o^.id:=id; o^.type:=t1;
                   IF new_vers THEN
                     gX(a1); modno:=module?(a1);
                     access(av);
                     IF obs.Simple?(o^.type) THEN
                       ASSERT(av.am=kr.am_imm);
                       o^.scope:=0;
                       o^.addr:=av.no;
                     ELSE
                       ASSERT(av.am=kr.am_STR);
                       o^.scope:=-modno;
                       o^.addr:=av.disp DIV 32;
                     END;
                   ELSE gX(o^.addr);
                   END;
                   declare(o,0);
    |sym.struct  : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.cons); o^.id:=id; o^.type:=t1;
                   gX(a1); modno:=module?(a1);
                   gX(a1); gX(a2); gen.extStruct(o,modno,a1,a2,get);
                   declare(o,0);
    |sym.sconst  : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.cons); o^.id:=id; o^.type:=t1;
                   gX(a1); modno:=module?(a1);
                   gX(a1); gen.extStruct(o,modno,a1,0,get);
                   declare(o,0);
    |sym.var     : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.vari); o^.id:=id; o^.type:=t1;
                   IF NOT new_vers THEN
                     gX(a1); ASSERT(a1=0); -- level=0
                     gX(o^.addr);
                   END;
                   gX(a1); set_kind(a1,o^.tags);
                   IF new_vers THEN
                     access(av);
                     ASSERT(av.lvl=0);
                     IF    av.am=kr.am_G  THEN
                       o^.addr:=av.disp DIV 32;
                       IF NOT (o^.type^.mode IN DIRECTs) THEN
                         INCL(o^.tags,obs.direct);
                       END;
                     ELSIF av.am=kr.am_aG THEN
                       o^.addr:=av.no;
                     ELSE ASSERT(FALSE);
                     END;
                     IF o^.addr<0 THEN INC(o^.addr) END;
                   END;
                   modno:=module?(0);
                   IF modno=0 THEN EXCL(o^.tags,obs.RO) END;
                   gen.extVar(o,modno,o^.addr);
                   declare(o,0);
    |sym.proc    : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.proc); o^.id:=id; o^.type:=t1;
                   IF new_vers THEN
                     gX(a1);
                     access(ap);
                     ASSERT(ap.mod=0);
                     ASSERT(ap.lvl=1);
                     a1:=ap.disp;
                   ELSE
                     gX(a1); ASSERT(a1=0); -- level=0
                     gX(a1);
                   END;
                   modno:=module?(0);
                   IF modno=0 THEN INCL(o^.tags,obs.forward) END;
                   gen.extProc(o,modno,a1); declare(o,0);
    |sym.type    : id:=id?(); type?(t1);
                   obs.NewObj(o,obs.typ); o^.id:=id; o^.type:=t1;
                   gX(modno); o^.scope:=module?(modno);
                   declare(o,modno);
                   IF (o#NIL) & (o^.type^.obj=NIL) THEN o^.type^.obj:=o END;

    |sym.import   : import
    |sym.module   : ASSERT(FALSE,51h);
    |sym.endmodule: gX(modno);
    |new.packed   : (* ignore *)
    |sym.end_CU   : IF module?(0)=0 THEN
                      IF new_vers THEN
                        access_cu;
                      ELSE
                        gX(a1); gX(a2); gen.set_counts(a1,a2)
                      END;
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
  IF head^.magic=sym.MAGIC THEN
    IF (head^.vers#sym.VERSION) & (head^.vers#new.VERSION) THEN
      scan.Fault(56,'"%s"',name)
    ELSE pos:=head^.offset; read_sym(head^.vers=new.VERSION,o);
    END;
  ELSE scan.Fault(55,'"%s"',name)
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
