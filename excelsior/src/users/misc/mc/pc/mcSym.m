IMPLEMENTATION MODULE mcSym; (* Ned 21-May-88. (c) KRONOS *)

IMPORT   sys: SYSTEM;
IMPORT   sym: coSym;
IMPORT   ers: coErrors;
IMPORT  comp: coolDefs;
IMPORT inter: pcSystem;
IMPORT  scan: mcScan;
IMPORT   obs: mcObj;
IMPORT   gen: mcGen;

IMPORT    pc: pcTab;

WITH STORAGE: inter;

CONST Modula_X = 'Modula-X';

----------------------------------------------------------------

VAR
  io   : comp.io_ptr;
  buf  : STRING;
  acs  : STRING;
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

PROCEDURE access(r: pc.ref);
  VAR i: INTEGER;
BEGIN
  pc.get_code(r,acs);
  pX(LEN(acs));
  FOR i:=0 TO HIGH(acs) DO p(ORD(acs[i])) END;
END access;

PROCEDURE import(o: obs.obj_ptr);
  VAR name: ARRAY [0..255] OF CHAR;
BEGIN
  ASSERT(o^.mode=obs.module);
  p(sym.import); scan.id_str(o^.id,name); pName(name); pName(Modula_X);
  pX(o^.head^.ctime); p(sym.def);
  access(o^.gen);
END import;

PROCEDURE createsym(cu: obs.obj_ptr;
              VAL name: ARRAY OF CHAR; unit,def_time,imp_time: INTEGER);
  VAR u: INTEGER; l: obs.header_ptr;
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
  access(cu^.gen);
  l:=obs.externals;
  WHILE l#NIL DO import(l^.self); l:=l^.next END;
END createsym;

PROCEDURE closesym;
BEGIN
  IF scan.fault OR (scan.noErrors#0) THEN
    io^.done:=FALSE
  ELSE
    p(sym.end_CU);
    access(NIL);
    p(sym.eosf);
    IF pos>0 THEN flush END;
  END;
  inter.exi(io);
  DISPOSE(buf);
END closesym;

PROCEDURE outscope(block: obs.obj_ptr);

  VAR ptrs: inter.QUEUE;
      name: ARRAY [0..255] OF CHAR;

  PROCEDURE ident(id: INTEGER);
  BEGIN scan.id_str(id,name); pName(name) END ident;

  PROCEDURE get_kind(kind: BITSET): INTEGER;
    VAR c: BITSET;
  BEGIN
    c:={};
    IF obs.varpar IN kind THEN INCL(c,sym.varparam) END;
    IF obs.seqpar IN kind THEN INCL(c,sym.seqparam) END;
    IF obs.RO     IN kind THEN INCL(c,sym.readonly) END;
    RETURN INTEGER(c)
  END get_kind;

  PROCEDURE outType(t: obs.type_ptr): INTEGER;

    PROCEDURE outParms(parm: obs.obj_ptr);
      VAR r: INTEGER; l: obs.obj_ptr;
    BEGIN l:=parm;
      WHILE l#NIL DO r:=outType(l^.type); l:=l^.next END;
      l:=parm;
      WHILE l#NIL DO
        p(sym.parm); pName(''); pX(l^.type^.ref);
        pX(get_kind(l^.tags));
        access(l^.gen);
        l:=l^.next;
      END;
    END outParms;

    PROCEDURE outFields(f: obs.obj_ptr);
      VAR r: INTEGER; l: obs.obj_ptr;
    BEGIN l:=f;
      WHILE l#NIL DO r:=outType(l^.type); l:=l^.next END;
      l:=f;
      WHILE l#NIL DO
        p(sym.field); ident(l^.id); pX(l^.type^.ref);
        access(l^.gen);
        l:=l^.next;
      END;
    END outFields;

    PROCEDURE outEnums(l: obs.obj_ptr; type,modno: INTEGER);
    BEGIN
      WHILE l#NIL DO
        p(sym.enum); ident(l^.id); pX(type); pX(modno);
        access(l^.gen);
        l:=l^.list;
      END;
    END outEnums;

    VAR b,i: INTEGER;
  BEGIN
    IF t^.ref>0 THEN RETURN t^.ref END;
    CASE t^.mode OF
      | obs.enum    : p(sym.enumtype); access(t^.gen);
      | obs.rec     : outFields(t^.head^.locals);
                      IF t^.gen^.md=pc.packed_record THEN p(sym.packed) END;
                      p(sym.record); pX(sym.nan);
      | obs.ptr     : inter.push(ptrs,t); p(sym.pointer);
      | obs.arr     : i:=outType(t^.inx); b:=outType(t^.base);
                      IF t^.gen^.md=pc.packed_array THEN p(sym.packed) END;
                      p(sym.array); pX(i); pX(b); access(t^.gen);
      | obs.proctype: outParms(t^.plist);
                      p(sym.proctype);
      | obs.functype: b:=outType(t^.base); outParms(t^.plist);
                      p(sym.functype); pX(b);
      | obs.range   : b:=outType(t^.base);
                      p(sym.range); pX(b); access(t^.gen);
      | obs.newrange: b:=outType(t^.base);
                      p(sym.newrange); pX(b); access(t^.gen);
      | obs.farr    : b:=outType(t^.base);
                      IF t^.gen^.md=pc.packed_array_of THEN p(sym.packed) END;
                      p(sym.openarr); pX(b);
      | obs.dynarr  : b:=outType(t^.base);
                      IF t^.gen^.md=pc.packed_dynarr THEN p(sym.packed) END;
                      p(sym.dynarr);  pX(b); pX(1);
      | obs.settype : b:=outType(t^.base); p(sym.set); pX(b); access(t^.gen);
      | obs.hidden  : p(sym.hidden);
    ELSE ASSERT(FALSE);
    END;
    IF t^.obj#NIL THEN
      p(sym.type); ident(t^.obj^.id); pX(refNo); pX(t^.obj^.no);
    END;
    t^.ref:=refNo; INC(refNo);
    IF t^.mode=obs.enum THEN
      IF t^.obj=NIL THEN i:=0 ELSE i:=t^.obj^.no END;
      outEnums(t^.list,t^.ref,i)
    END;
    RETURN refNo-1
  END outType;

  PROCEDURE outObj(o: obs.obj_ptr);
    VAR r: INTEGER;
  BEGIN
    CASE o^.mode OF
      |obs.cons : r:=outType(o^.type);
                  p(sym.const); ident(o^.id); pX(r); pX(o^.no);
                  access(o^.gen);
      |obs.vari : r:=outType(o^.type);
                  p(sym.var); ident(o^.id); pX(r);
                  pX(get_kind(o^.tags));
                  access(o^.gen);
      |obs.proc : IF obs.code_proc IN o^.tags THEN RETURN END;
                  r:=outType(o^.type);
                  p(sym.proc);  ident(o^.id); pX(r); pX(o^.no);
                  access(o^.gen);
      |obs.module: p(sym.module); ident(o^.id); pX(o^.no);
      |obs.field: (* nothing *)
      |obs.econs: (* nothing *)
      |obs.inv  : ASSERT(FALSE);
    ELSE ASSERT(FALSE);
    END;
  END outObj;

  CONST
    blocks = obs.Modes{obs.proc,obs.module};
    skip   = {obs.duplicate,obs.code_proc};
  VAR l: obs.obj_ptr; t: obs.type_ptr; r: INTEGER;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  inter.fifo(ptrs);
  IF NOT def? THEN
    l:=block^.head^.locals;
    WHILE l#NIL DO
      IF (l^.mode IN blocks) & (l^.tags*skip={}) THEN
        outscope(l^.head^.self);
      END;
      l:=l^.next;
    END;
  END;
  l:=block^.head^.locals;
  WHILE l#NIL DO
    IF (l^.mode=obs.type) & NOT (obs.duplicate IN l^.tags) THEN
      r:=outType(l^.type);
      IF l^.type^.obj#l THEN
        p(sym.type); ident(l^.id); pX(r); pX(l^.no);
      END;
    END;
    l:=l^.next;
  END;
  l:=block^.head^.locals;
  WHILE l#NIL DO
    IF (l^.mode#obs.type) & NOT (obs.duplicate IN l^.tags) THEN outObj(l) END;
    l:=l^.next;
  END;
  WHILE inter.pop(ptrs,t) DO
    r:=outType(t^.base); p(sym.linkage); pX(t^.ref); pX(r);
  END;
  inter.clear(ptrs);
  IF    block^.mode=obs.proc   THEN p(sym.endproc)
  ELSIF block^.mode=obs.module THEN p(sym.endmodule)
  ELSE ASSERT(FALSE)
  END;
  pX(block^.no);
END outscope;

PROCEDURE put_sym(cu: obs.obj_ptr; unit,def_time,imp_time: INTEGER);
  VAR name: ARRAY [0..255] OF CHAR;
BEGIN
  NEW(acs);
  scan.id_str(cu^.id,name);
  createsym(cu,name,unit,def_time,imp_time);
  outscope(cu);
  closesym;
END put_sym;

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

PROCEDURE read_sym(cu: obs.obj_ptr; own_def: BOOLEAN; VAR mdl: obs.obj_ptr);

  VAR imps: DYNARR OF obs.obj_ptr;
     impNo: INTEGER;
     types: DYNARR OF obs.type_ptr;
    typeNo: INTEGER;
      name: ARRAY [0..255] OF CHAR;

  PROCEDURE clear_types;
    VAR i: INTEGER;
  BEGIN
    FOR i:=typeNo TO HIGH(types) DO types[i]:=NIL END;
  END clear_types;

  PROCEDURE newtype(t: obs.type_ptr);
  BEGIN
    IF typeNo>HIGH(types) THEN
      RESIZE(types,HIGH(types)+129);
      clear_types;
    END;
    ASSERT(types[typeNo]=NIL);
    t^.ref:=typeNo; types[typeNo]:=t; INC(typeNo);
  END newtype;

  PROCEDURE type?(VAR t: obs.type_ptr);
    VAR typeno: INTEGER;
  BEGIN
    gX(typeno); t:=types[typeno]; ASSERT(t#NIL);
  END type?;

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
    types[sym.uint  ]:=obs.uintp;
  END ini;

  PROCEDURE id?(): INTEGER;
  BEGIN gName(name); RETURN scan.str_id(name)
  END id?;

  PROCEDURE replace(new,old: obs.type_ptr);

    PROCEDURE Parms(p: obs.obj_ptr);
      VAR d: obs.obj_ptr;
    BEGIN
      WHILE p#NIL DO d:=p; p:=p^.next; DISPOSE(d) END;
    END Parms;

    PROCEDURE Fields(f: obs.obj_ptr);
      VAR d: obs.obj_ptr;
    BEGIN
      WHILE f#NIL DO d:=f; f:=f^.next; obs.undo_dcl(d); DISPOSE(d) END;
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
      | obs.range,obs.newrange,obs.farr,obs.dynarr,obs.settype:
                      replace(new^.base,old^.base);
      | obs.hidden,obs.enum,obs.ptr   : (*nothing*)
    ELSE ASSERT(FALSE);
    END;
  END replace;

  PROCEDURE declare(VAR o: obs.obj_ptr; modno: INTEGER): BOOLEAN;
    VAR m,old: obs.obj_ptr;
  BEGIN
    m:=imps[modno]; ASSERT(m#NIL);
    IF obs.vis_in?(m^.head,o^.id,old) THEN
      ASSERT(o^.mode=old^.mode);
      IF o^.mode IN obs.Modes{obs.vari,obs.cons,obs.proc,obs.type} THEN
        replace(o^.type,old^.type);
        DISPOSE(o);
      END;
      RETURN FALSE
    ELSE
      obs.dcl_in(m^.head,o^.id,o);
      RETURN TRUE
    END;
  END declare;

  PROCEDURE exit;
    VAR i: INTEGER;
  BEGIN
    FOR i:=32 TO typeNo-1 DO types[i]^.ref:=-1 END;
    DISPOSE(types);
    DISPOSE(imps);
  END exit;

  PROCEDURE skip;
    VAR n: INTEGER; c: CHAR;
  BEGIN
    gX(n);
    WHILE n>0 DO g(c); DEC(n); END;
  END skip;

  PROCEDURE access(r: pc.ref; mod: obs.obj_ptr);
    VAR n,i: INTEGER; c: CHAR; x: pc.ref;
  BEGIN
    gX(n);
    RESIZE(acs,n);
    FOR i:=0 TO n-1 DO g(c); acs[i]:=c END;
    IF own_def THEN x:=NIL
    ELSE x:=mod^.gen
    END;
    pc.put_code(r,x,acs);
  END access;

  PROCEDURE import;

    PROCEDURE time_err(time: INTEGER);
      VAR msg: ARRAY [0..79] OF CHAR;
    BEGIN
      inter.sprint(msg,'Приходите после ');
      inter.app_time(msg,time);
      scan.Fault(ers.blank,'%s',msg);
    END time_err;

    VAR o: obs.obj_ptr; time,no: INTEGER;
  BEGIN
    IF obs.lookupModule(id?(),o) THEN
      gen.new(o^.gen,pc.module);
      o^.gen^.nm:=o^.id;
    ELSIF o^.mode#obs.module THEN RETURN
    END;
    gName(name);
    IF name#Modula_X THEN
      scan.Fault(ers.sym_language,'-- %s',name); RETURN
    END;
    gX(time);
    IF time>inter.time() THEN time_err(time); RETURN END;
    IF (o^.head^.ctime>=0) & (o^.head^.ctime#time) THEN
      scan.id_str(o^.id,name);
      scan.Fault(ers.sym_times,'%s',name);
      RETURN
    END;
    o^.head^.ctime:=time;
    g(no);
    IF no#sym.def THEN scan.Fault(ers.sym_data,'') END;
    IF impNo=0 THEN access(o^.gen,cu);
    ELSE            access(o^.gen,imps[0]);
    END;
    IF impNo>HIGH(imps) THEN RESIZE(imps,LEN(imps)+32) END;
    imps[impNo]:=o; INC(impNo);
  END import;

  PROCEDURE set_kind(x: INTEGER; VAR tags: BITSET);
    VAR c: BITSET; i: INTEGER;
  BEGIN
    c:=BITSET(x); tags:={};
    IF sym.varparam IN c THEN INCL(tags,obs.varpar) END;
    IF sym.seqparam IN c THEN INCL(tags,obs.seqpar) END;
    IF sym.readonly IN c THEN INCL(tags,obs.RO    ) END;
  END set_kind;

  VAR tag: INTEGER;
        t: obs.type_ptr;
    t1,t2: obs.type_ptr;
       id: INTEGER;
        o: obs.obj_ptr;
    a1,a2: INTEGER;
      val: INTEGER;
    modno: INTEGER;
       eT: obs.type_ptr;  -- enum type
       rT: obs.type_ptr;  -- record type
       pT: obs.type_ptr;  -- proc type
     tail: obs.obj_ptr;   -- for enum list
   packed: BOOLEAN;

BEGIN
  ini; eT:=NIL; rT:=NIL; pT:=NIL; tail:=NIL; packed:=FALSE;
  LOOP
    tag:=ORD(buf[pos]); INC(pos);
    CASE tag OF

    |sym.enumtype: eT:=obs.MakeEnum(); newtype(eT); tail:=NIL;
                   gen.new(eT^.gen,pc.enumeration);
                   access(eT^.gen,imps[0]);
    |sym.range   : type?(t1);
                   t:=obs.MakeRange(t1,obs.range);
                   newtype(t);
                   gen.new(t^.gen,pc.subtype);
                   t^.gen^.dw:=t1^.gen;
                   access(t^.gen,imps[0]);
    |sym.newrange: type?(t1);
                   t:=obs.MakeRange(t1,obs.newrange);
                   newtype(t);
                   gen.new(t^.gen,pc.range);
                   t^.gen^.dw:=t1^.gen;
                   access(t^.gen,imps[0]);
    |sym.array   : type?(t1); type?(t2); t:=obs.MakeArr(t1,t2); newtype(t);
                   IF packed THEN
                     gen.new(t^.gen,pc.packed_array); packed:=FALSE;
                   ELSE
                     gen.new(t^.gen,pc.array);
                   END;
                   t^.gen^.l:=t^.inx^.gen;
                   t^.gen^.r:=t^.base^.gen;
                   access(t^.gen,imps[0]);
    |sym.openarr : type?(t1); t:=obs.MakeFlx(t1); newtype(t);
                   IF packed THEN
                     gen.new(t^.gen,pc.packed_array_of); packed:=FALSE;
                   ELSE
                     gen.new(t^.gen,pc.array_of);
                   END;
                   t^.gen^.r:=t1^.gen;
    |sym.dynarr  : type?(t1); gX(a1); t:=obs.MakeDynArr(t1,a1); newtype(t);
                   IF packed THEN
                     gen.new(t^.gen,pc.packed_dynarr); packed:=FALSE;
                   ELSE
                     gen.new(t^.gen,pc.dynarr);
                   END;
                   t^.gen^.r:=t1^.gen;
    |sym.pointer : t:=obs.MakeFwdPtr(scan.DmId);  newtype(t);
                   gen.new(t^.gen,pc.pointer);
    |sym.set     : type?(t1); t:=obs.MakeSet(t1); newtype(t);
                   gen.new(t^.gen,pc.set); t^.gen^.dw:=t1^.gen;
                   access(t^.gen,imps[0]);
    |sym.record  : IF rT=NIL THEN
                     rT:=obs.MakeRec(); gen.new(rT^.gen,pc.record);
                   END;
                   IF packed THEN
                     rT^.gen^.md:=pc.packed_record; packed:=FALSE
                   END;
                   gX(val); ASSERT(val=sym.nan);
                   newtype(rT); rT:=NIL;
    |sym.proctype: IF pT=NIL THEN
                     pT:=obs.MakeProcType(); gen.new(pT^.gen,pc.profile);
                   ELSE
                     gen.inverse(pT^.gen^.dw);
                   END;
                   obs.ProcType(pT); newtype(pT);
                   pT:=NIL
    |sym.functype: IF pT=NIL THEN
                     pT:=obs.MakeProcType(); gen.new(pT^.gen,pc.profile);
                   ELSE
                     gen.inverse(pT^.gen^.dw);
                   END;
                   type?(t1); obs.FuncType(pT,t1); pT^.gen^.l:=t1^.gen;
                   newtype(pT); pT:=NIL
    |sym.hidden  : t:=obs.MakeHidden(1);
                   newtype(t); t^.gen:=obs.addrp^.gen;
    |sym.linkage : type?(t1); type?(t2);
                   ASSERT((t1^.mode=obs.ptr) & (t1^.tid=scan.DmId));
                   t1^.base:=t2;
                   t1^.gen^.dw:=t2^.gen;
    |sym.parm    : IF pT=NIL THEN
                     pT:=obs.MakeProcType(); gen.new(pT^.gen,pc.profile);
                   END;
                   g(a1); ASSERT(a1=0);  -- id:=id?();
                   type?(t1);
                   o:=obs.AppParm(pT,0,t1,{});
                   gX(a1); set_kind(a1,o^.tags);
                   gen.param(pT^.gen^.dw,o);
                   access(o^.gen,imps[0]);
    |sym.field   : IF rT=NIL THEN
                     rT:=obs.MakeRec(); gen.new(rT^.gen,pc.record);
                   END;
                   id:=id?(); type?(t1);
                   o:=obs.AppField(rT,id,t1);
                   gen.var(rT^.gen^.dw,o);
                   access(o^.gen,imps[0]);
    |sym.enum    : id:=id?(); type?(t1); gX(modno);
                   IF t1=eT THEN
                     o:=obs.AppEnum(eT);
                     IF tail=NIL THEN eT^.list:=o ELSE tail^.list:=o END;
                     o^.id:=id;
                     IF declare(o,modno) THEN
                       gen.new(o^.gen,pc.const);
                       IF tail=NIL THEN
                         eT^.gen^.nxt:=o^.gen
                       ELSE
                         tail^.gen^.nxt:=o^.gen
                       END;
                       o^.gen^.l:=t1^.gen;
                       o^.gen^.nm:=o^.id;
                       access(o^.gen,imps[modno]);
                     ELSE skip
                     END;
                     tail:=o;   --!!
                   ELSE skip
                   END;
    |sym.const   : id:=id?(); type?(t1);
                   obs.new_obj(o,obs.cons);
                   gX(modno); o^.no:=imps[modno]^.no;
                   o^.id:=id; o^.type:=t1;
                   IF declare(o,0) THEN
                     gen.new(o^.gen,pc.const);
                     o^.gen^.l:=t1^.gen;
                     o^.gen^.nm:=o^.id;
                     access(o^.gen,imps[0]);
                   ELSE skip
                   END;
    |sym.var     : id:=id?(); type?(t1);
                   obs.new_obj(o,obs.vari); o^.id:=id; o^.type:=t1;
                   gX(a1); set_kind(a1,o^.tags);
                   IF imps[0]^.no=0 THEN EXCL(o^.tags,obs.RO) END;
                   IF declare(o,0) THEN
                     gen.new(o^.gen,pc.var);
                     o^.gen^.l:=t1^.gen;
                     o^.gen^.nm:=o^.id;
                     access(o^.gen,imps[0]);
                   ELSE skip
                   END;
    |sym.proc    : id:=id?(); type?(t1);
                   obs.new_obj(o,obs.proc); o^.id:=id; o^.type:=t1;
                   gX(o^.no);
                   IF imps[0]^.no=0 THEN INCL(o^.tags,obs.forward) END;
                   IF declare(o,0) THEN
                     gen.new(o^.gen,pc.procedure);
                     o^.gen^.l:=t1^.gen;
                     o^.gen^.nm:=o^.id;
                     access(o^.gen,imps[0]);
                   ELSE skip
                   END;
    |sym.type    : id:=id?(); type?(t1);
                   obs.new_obj(o,obs.type); o^.id:=id; o^.type:=t1;
                   gX(modno); o^.no:=imps[modno]^.no;
                   IF declare(o,modno) THEN
                     IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
                     o^.gen:=o^.type^.gen;
                   END;
    |sym.import   : import
    |sym.module   : ASSERT(FALSE,51h);
    |sym.endmodule: gX(modno);
    |sym.end_CU   : mdl:=imps[0];
                    IF mdl^.no=0 THEN access(NIL,NIL) END;
                    exit; RETURN
    |sym.packed   : packed:=TRUE;
    ELSE
      scan.Fault(ers.sym_data,'tag=%$h',tag); RETURN
    END;

  END;
END read_sym;

PROCEDURE get_sym(cu: obs.obj_ptr; id: INTEGER);
  VAR  o: obs.obj_ptr;
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
  IF head^.magic#sym.MAGIC THEN scan.Fault(ers.sym_header,'"%s"',name)
  ELSIF head^.vers#sym.VERSION THEN scan.Fault(ers.sym_vers,'"%s"',name)
  ELSE pos:=head^.offset; read_sym(cu,cu^.id=id,o);
  END;
  inter.exi(io);
  NEW(buf);
  IF (o=NIL) OR  (o^.mode=obs.inv) OR (o^.id#id) THEN
    scan.Fault(ers.sym_data,'"%s"',name); RETURN
  END;
  INCL(o^.tags,obs.complete);
END get_sym;

-------------------------  INI & EXI  -------------------------
                         -------------

PROCEDURE Ini;
BEGIN
  obs.addrp  ^.ref:=sym.addr;
  obs.wordp  ^.ref:=sym.word;
  obs.intp   ^.ref:=sym.int;
  obs.uintp  ^.ref:=sym.uint;
  obs.charp  ^.ref:=sym.char;
  obs.realp  ^.ref:=sym.real;
  obs.boolp  ^.ref:=sym.bool;
  obs.bitsetp^.ref:=sym.bitset;
  obs.stringp^.ref:=sym.string;
  NEW(buf);
  NEW(acs);
END Ini;

PROCEDURE Exi;
BEGIN
END Exi;

END mcSym.
