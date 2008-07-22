IMPLEMENTATION MODULE mcPars; (* Ned 10-Nov-87. (c) KRONOS *)
                              (* Ned 11-Aug-90. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT   ers: coErrors;
IMPORT  comp: coDefs;
IMPORT inter: pcSystem;
IMPORT  scan: mcScan;
IMPORT   obs: mcObj;
IMPORT   sym: mcSym;
IMPORT   gen: mcGen;
IMPORT   vis: mcVis;

IMPORT    pc: pcTab;

FROM mcScan IMPORT  sy, GetSy;

WITH STORAGE: inter;

CONST
  r_check = ORD('R')-ORD('A');
  b_check = ORD('T')-ORD('A');

VAR
  CU     : obs.obj_ptr;  -- compilation unit
  mod_no : INTEGER;
  proc_no: INTEGER;

  withs  : obs.obj_ptr;
  types  : pc.ref;        -- list of string and aggregate types

TYPE
  NODE = RECORD
           mode : obs.Mode;
           type : obs.type_ptr;
           tags : BITSET;
           pno  : INTEGER;      -- for standard procedure
           modno: INTEGER;      -- for consts
           gen  : pc.ref;
         END;

---------------------------------------------------------------

PROCEDURE CheckGet(S: scan.Symbol);
BEGIN
  IF sy#S THEN scan.expc(S) END; GetSy
END CheckGet;

PROCEDURE SkipTo(SEQ s: scan.Symbol);
  VAR i: INTEGER;
BEGIN
  LOOP
    IF sy=scan.end THEN RETURN END;
    i:=0;
    WHILE (i<=HIGH(s)) & (s[i]#sy) DO INC(i) END;
    IF i<=HIGH(s) THEN RETURN END;
    GetSy;
  END;
END SkipTo;

---------------------  TYPE COMPATIBILITY  --------------------
                     ----------------------

PROCEDURE chkSimple(t: obs.type_ptr);
BEGIN
  IF NOT (t^.mode IN obs.SIMPLEs) THEN scan.err(ers.simple) END;
END chkSimple;

PROCEDURE Char?(t: obs.type_ptr): BOOLEAN;
BEGIN
  IF t^.mode=obs.range THEN
    RETURN t^.base^.mode=obs.char
  ELSE
    RETURN t^.mode=obs.char
  END;
END Char?;

PROCEDURE TypeCmp0(VAR t1,t2: obs.type_ptr): BOOLEAN;
  VAR m1,m2: obs.TypeMode;
BEGIN
  ASSERT((t1#NIL) & (t2#NIL));
  IF t1^.mode=obs.range THEN t1:=t1^.base END;
  IF t2^.mode=obs.range THEN t2:=t2^.base END;
  m1:=t1^.mode; m2:=t2^.mode;
  IF (t1=t2) OR (m1=obs.invtype) OR (m2=obs.invtype) THEN RETURN TRUE END;
  RETURN (m1=obs.addr) & (m2 IN obs.Types{obs.int,obs.ptr})
      OR (m2=obs.addr) & (m1 IN obs.Types{obs.int,obs.ptr})
END TypeCmp0;

PROCEDURE TypeCmp(t1,t2: obs.type_ptr);
BEGIN
  IF (t1=t2) OR TypeCmp0(t1,t2) THEN RETURN END;
  scan.err(ers.incompatible);
END TypeCmp;

PROCEDURE TypeRep(VAR n: NODE; t: obs.type_ptr);
  CONST r_types=obs.Types{obs.word,obs.int,obs.char,obs.range,obs.newrange};
  VAR r: pc.ref;
BEGIN
  IF (n.type^.mode=obs.uint) & (t^.mode IN r_types) THEN
    IF (n.gen^.md=pc.number) OR (t^.mode=obs.int) THEN
      n.gen^.dw:=t^.gen;
    ELSE
      gen.new(r,pc.type_transfer);
      r^.r:=n.gen; r^.dw:=t^.gen; n.gen:=r;
    END;
    n.type:=t;
  END;
END TypeRep;

PROCEDURE ProcCmp?(t1,t2: obs.type_ptr): ers.T;

  PROCEDURE GeneralCmp(t1,t2: obs.type_ptr; VAR err: ers.T);
  BEGIN
    err:=ers.blank;
    IF    t1=t2 THEN
    ELSIF t1^.mode IN obs.PROCs THEN err:=ProcCmp?(t1,t2)
    ELSIF t1=obs.wordp THEN chkSimple(t2)
    ELSIF t2=obs.wordp THEN chkSimple(t1)
    ELSIF NOT TypeCmp0(t1,t2) THEN err:=ers.incompatible
    END;
  END GeneralCmp;

  CONST par_tags = {obs.varpar,obs.seqpar};

  VAR p1,p2: obs.obj_ptr; err: ers.T;
BEGIN
  IF (t1^.mode#t2^.mode) OR NOT (t1^.mode IN obs.PROCs) THEN
    RETURN ers.incompatible
  ELSIF t1^.mode=obs.functype THEN
    GeneralCmp(t1^.base,t2^.base,err);
    IF err#ers.blank THEN RETURN err END;
  END;
  p1:=t1^.plist; p2:=t2^.plist;
  WHILE (p1#NIL) & (p2#NIL) DO
    IF p1^.tags*par_tags#p2^.tags*par_tags THEN RETURN ers.parm_spec END;
    t1:=p1^.type; t2:=p2^.type;
    IF t1^.mode=obs.farr THEN
      IF t2^.mode=obs.farr THEN t1:=t1^.base; t2:=t2^.base;
        GeneralCmp(t1,t2,err);
        IF err#ers.blank THEN RETURN err END;
      ELSE RETURN ers.incompatible
      END;
    ELSE
      GeneralCmp(t1,t2,err);
      IF err#ers.blank THEN RETURN err END;
    END;
--  IF p1^.addr#p2^.addr THEN RETURN 74 END;
    p1:=p1^.next; p2:=p2^.next;
  END;
  IF p1#p2 THEN RETURN ers.parm_quan END;
  RETURN ers.blank
END ProcCmp?;

PROCEDURE ProcCmp(t1,t2: obs.type_ptr);
  VAR n: ers.T;
BEGIN
  n:=ProcCmp?(t1,t2);
  IF n#ers.blank THEN scan.err(n) END;
END ProcCmp;

PROCEDURE AsgCmp(t1,t2: obs.type_ptr);
BEGIN
  IF (t1=t2) OR TypeCmp0(t1,t2) THEN
  ELSIF t1^.mode=obs.word           THEN chkSimple(t2);
  ELSIF t2^.mode=obs.word           THEN chkSimple(t1);
  ELSIF t1^.mode IN obs.PROCs       THEN ProcCmp(t1,t2);
  ELSIF (t1^.mode IN obs.ARRs) & (t2^.mode IN obs.ARRs) THEN
    IF (t1^.mode IN obs.DYNs) OR (t2^.mode IN obs.DYNs) THEN
      TypeCmp(t1^.base,t2^.base);
    ELSIF Char?(t1^.base) & Char?(t2^.base) THEN
    (*& (tsize(t1)>=tsize(t2))*)
    ELSE scan.err(ers.incompatible);
    END;
  ELSE scan.err(ers.incompatible);
  END;
END AsgCmp;

-------------------------  DESIGNATOR  ------------------------
                         --------------
PROCEDURE Expr(VAR node: NODE); FORWARD;

PROCEDURE err?(VAR n: NODE; no: ers.T);
BEGIN
  IF n.mode#obs.inv THEN scan.err(no) END;
  n.mode:=obs.inv;
END err?;

PROCEDURE unary(VAR o: NODE; m: pc.mode);
  VAR x: pc.ref;
BEGIN
  gen.new(x,m);
  x^.dw:=o.type^.gen;
  x^.r :=o.gen;
  o.gen:=x;
  o.modno:=0;
END unary;

PROCEDURE qualident(VAR v: obs.obj_ptr);
BEGIN
  IF sy#scan.ident THEN scan.expc(scan.ident); v:=obs.Ilg; RETURN END;
  v:=obs.vis(scan.Id); GetSy;
  WHILE (sy=scan.period) & (v^.mode=obs.module) DO
    GetSy;
    IF sy#scan.ident THEN scan.expc(scan.ident); RETURN END;
    v:=obs.vis_in(v^.head,scan.Id); GetSy;
  END;
END qualident;

PROCEDURE designator(VAR node: NODE);

  PROCEDURE vis_in_with?(VAR v: obs.obj_ptr): BOOLEAN;
    VAR l: obs.obj_ptr; t: obs.type_ptr;
  BEGIN
    l:=withs;
    WHILE l#NIL DO
      t:=l^.type;
      IF   (t^.mode=obs.ptr) & (t^.base^.mode=obs.rec)
         & obs.vis_in?(t^.base^.head,scan.Id,v)
      THEN
        v:=l; RETURN TRUE
      END;
      l:=l^.next;
    END; RETURN FALSE
  END vis_in_with?;

  PROCEDURE deref(VAR n: NODE);
    VAR x: pc.ref;
  BEGIN
    CASE n.type^.mode OF
      |obs.ptr   : n.type:=n.type^.base
      |obs.dynarr: n.type:=n.type^.desc;
      |obs.addr  : n.type:=obs.wordp
    ELSE
      scan.err(ers.incompatible); n.mode:=obs.inv
    END;
    gen.new(x,pc.deref);
    x^.l:=n.gen;
    x^.dw:=n.type^.gen;
    n.gen:=x;
  END deref;

  PROCEDURE index(VAR n: NODE);
    VAR ex: NODE; inx: obs.type_ptr; x: pc.ref;
  BEGIN
    Expr(ex);
    IF n.type^.mode=obs.arr THEN
      inx:=n.type^.inx; TypeRep(ex,inx); TypeCmp(inx,ex.type);
      IF (n.mode=obs.cons) & (ex.mode#obs.cons) THEN n.mode:=obs.expr END;
      IF (b_check IN scan.opts) THEN
        unary(ex,pc.range_check);
        ex.gen^.dw:=inx^.gen;
      END;
    ELSIF n.type^.mode IN obs.Types{obs.farr,obs.dynarr} THEN
      inx:=obs.intp; TypeRep(ex,inx); TypeCmp(inx,ex.type);
    ELSE
      err?(n,ers.incompatible);
    END;
    n.type:=n.type^.base;
    gen.new(x,pc.index);
    IF (obs.seqpar IN n.tags) & (obs.varpar IN n.tags) THEN
      x^.dw:=obs.addrp^.gen;
      x^.l :=n.gen;
      x^.r :=ex.gen;
      gen.new(n.gen,pc.deref);
      n.gen^.l:=x;
      n.gen^.dw:=n.type^.gen
    ELSE
      x^.dw:=n.type^.gen;
      x^.l:=n.gen;
      x^.r:=ex.gen;
      n.gen:=x;
    END;
    n.tags:=n.tags-{obs.varpar,obs.seqpar};
  END index;

  PROCEDURE access(VAR n: NODE; id: INTEGER);
    VAR f: obs.obj_ptr; x: pc.ref;
  BEGIN
    IF n.type^.mode=obs.dynarr THEN
      IF id=scan.str_id('ADR') THEN
        gen.new(x,pc.adr); n.type:=obs.addrp; x^.dw:=obs.addrp^.gen;
        x^.r:=n.gen; n.gen:=x;
      ELSIF id=scan.str_id('HIGH') THEN
        gen.new(x,pc.high); n.type:=obs.intp; x^.dw:=obs.intp^.gen;
        x^.r:=n.gen; n.gen:=x;
      ELSE scan.err(ers.record); n.mode:=obs.inv;
      END;
      RETURN
    ELSIF n.type^.mode=obs.dyndesc THEN
      IF id=scan.str_id('ADR') THEN
        ASSERT(n.gen^.md=pc.deref);
        n.type:=obs.addrp; n.gen^.dw:=obs.addrp^.gen;
        n.gen^.md:=pc.adr; n.gen^.r:=n.gen^.l; n.gen^.l:=NIL;
      ELSIF id=scan.str_id('HIGH') THEN
        ASSERT(n.gen^.md=pc.deref);
        n.type:=obs.intp; n.gen^.dw:=obs.intp^.gen;
        n.gen^.md:=pc.high; n.gen^.r:=n.gen^.l; n.gen^.l:=NIL;
      ELSE scan.err(ers.record); n.mode:=obs.inv;
      END;
      RETURN
    END;
    IF n.type^.mode#obs.rec THEN
      scan.err(ers.record); n.mode:=obs.inv; RETURN
    END;
    f:=obs.vis_in(n.type^.head,id);
    IF f^.mode#obs.field THEN n.mode:=obs.inv; RETURN END;
    n.type:=f^.type;
    gen.new(x,pc.field);
    x^.l:=n.gen;
    x^.r:=f^.gen;
    x^.dw:=n.type^.gen;
    n.gen:=x;
  END access;

  VAR v: obs.obj_ptr;
BEGIN
  IF sy#scan.ident THEN
    scan.err(ers.ident); Expr(node); RETURN
  END;
  IF (withs#NIL) & vis_in_with?(v) THEN
    node.mode:=obs.vari;  node.type:=v^.type;
    node.tags:=v^.tags;   node.gen :=v^.gen;
    gen.usage(node.gen,v);
    deref(node);
    access(node,scan.Id);
    GetSy;
  ELSE
    qualident(v);
    node.mode :=v^.mode;   node.type:=v^.type;
    node.tags :=v^.tags;   node.gen :=NIL;
    node.modno:=v^.no;
    CASE node.mode OF
      |obs.inv     :
      |obs.cons    :
      |obs.econs   : node.mode:=obs.cons;
      |obs.vari    :
      |obs.proc    : node.mode:=obs.cons;
      |obs.std_proc: node.pno:=v^.no; gen.usage(node.gen,v); RETURN;
      |obs.module  : err?(node,ers.no_value);
      |obs.type    : node.gen:=v^.gen; RETURN
      |obs.field   : ASSERT(FALSE);
      |obs.expr    : ASSERT(FALSE);
    ELSE ASSERT(FALSE);
    END;
    gen.usage(node.gen,v);
  END;
  LOOP
    IF    sy=scan.bar    THEN
      deref(node); GetSy
    ELSIF sy=scan.period THEN
      GetSy;
      IF sy#scan.ident THEN scan.expc(scan.ident); RETURN END;
      access(node,scan.Id);
      GetSy;
    ELSIF sy=scan.lbr THEN GetSy;
      index(node);
      IF sy=scan.coma THEN
        sy:=scan.lbr (* "," ==  "][" *)
      ELSE CheckGet(scan.rbr)
      END;
    ELSE RETURN
    END;
  END;
END designator;

PROCEDURE vari(VAR node: NODE);
BEGIN
  IF node.mode#obs.vari THEN err?(node,ers.variable) END;
END vari;

-------------------------  PROC CALLs  ------------------------
                         --------------

CONST    -- СТАНДАРТНЫЕ ПРОЦЕДУРЫ --
  _inc  =-1;     _incl  =-2;       _halt   =-3;
  _dec  =-4;     _excl  =-5;       _assert =-6;
  _new  =-7;     _resize=-8;       _dispose=-9;

  _adr  = 0;     _odd   = 1;       _ord    = 2;
  _min  = 3;     _trunc = 4;       _cap    = 5;
  _max  = 6;     _float = 7;       _chr    = 8;
  _size = 9;     _bits  =10;       _bytes  =11;
  _high =12;     _abs   =13;       _ref    =14;
  _len  =15;     _low   =16;

PROCEDURE sProcCall(VAL proc: NODE; VAR head: pc.ref);

  PROCEDURE inc(VAR o: NODE; m: pc.mode);
    VAR e: NODE;
  BEGIN
    designator(o); vari(o);
    gen.tie(head,m);
    head^.l:=o.gen;
    obs.chkScalar(o.type);
    IF sy=scan.coma THEN
      GetSy; Expr(e);
      TypeRep(e,o.type); TypeCmp(o.type,e.type);
      head^.r:=e.gen;
    END;
  END inc;

  PROCEDURE incl(VAR o: NODE; m: pc.mode);
    VAR e: NODE;
  BEGIN
    designator(o); vari(o);
    gen.tie(head,m); head^.l:=o.gen;
    CheckGet(scan.coma); Expr(e); head^.r:=e.gen;
    IF NOT (o.type^.mode IN obs.SETs) THEN err?(o,ers.incompatible)
    ELSE
      TypeRep(e,o.type^.base); TypeCmp(o.type^.base,e.type);
      IF (r_check IN scan.opts) OR (e.mode=obs.cons) THEN
        unary(e,pc.range_check);
        e.gen^.dw:=o.type^.base^.gen;
      END;
    END;
  END incl;

  PROCEDURE set(p: pc.ref; id: INTEGER);
  BEGIN
    IF (p=NIL) OR (p^.r=NIL) THEN scan.err_id(ers.no_RTS,id) END;
    ASSERT(p^.md=pc.usage);
    head^.dw:=p;
  END set;

  PROCEDURE resize;
    VAR p: obs.obj_ptr; id: INTEGER; r: pc.ref;
  BEGIN
    id:=scan.str_id("RESIZE");
    IF NOT obs.vis_in?(obs.SuperProc^.head,id,p) THEN ASSERT(FALSE) END;
    gen.usage(r,p); set(r,id);
  END resize;

  VAR o: NODE;
BEGIN
  IF proc.pno=_halt THEN
    gen.tie(head,pc.halt);
    IF sy=scan.lpar THEN GetSy;
      IF sy#scan.rpar THEN Expr(o); head^.r:=o.gen END;
      CheckGet(scan.rpar);
    END;
  ELSE
    CheckGet(scan.lpar);
    CASE proc.pno OF
      |_inc    : inc(o,pc.inc);
      |_dec    : inc(o,pc.dec);
      |_incl   : incl(o,pc.incl);
      |_excl   : incl(o,pc.excl);
      |_assert : Expr(o);
                 TypeCmp(o.type,obs.boolp);
                 gen.tie(head,pc.assert); head^.l:=o.gen;
                 IF sy=scan.coma THEN
                   GetSy; Expr(o); obs.chkScalar(o.type);
                   head^.r:=o.gen;
                 END;
      |_new    : designator(o); vari(o);
                 gen.tie(head,pc.new); head^.l:=o.gen;
                 IF    o.type^.mode=obs.ptr    THEN set(proc.gen,-1);
                 ELSIF o.type^.mode=obs.dynarr THEN
                   IF sy=scan.coma THEN
                     GetSy; Expr(o);
                     TypeRep(o,obs.intp); TypeCmp(o.type,obs.intp);
                     head^.r:=o.gen;
                   END;
                   resize;
                 ELSE err?(o,ers.incompatible)
                 END;
      |_dispose: designator(o); vari(o);
                 gen.tie(head,pc.dispose); head^.l:=o.gen;
                 IF    o.type^.mode=obs.ptr    THEN set(proc.gen,-1);
                 ELSIF o.type^.mode=obs.dynarr THEN resize;
                 ELSE err?(o,ers.incompatible)
                 END;
      |_resize : designator(o); vari(o);
                 gen.tie(head,pc.resize); head^.l:=o.gen;
                 IF o.type^.mode#obs.dynarr THEN err?(o,ers.incompatible) END;
                 CheckGet(scan.coma); Expr(o);
                 TypeRep(o,obs.intp); TypeCmp(o.type,obs.intp);
                 head^.r:=o.gen;
                 set(proc.gen,-1);
    ELSE ASSERT(FALSE);
    END;
    CheckGet(scan.rpar);
  END;
END sProcCall;

PROCEDURE make_arr(VAR t: obs.type_ptr; hi: INTEGER; base: obs.type_ptr);
  VAR l,r: pc.ref;
BEGIN
  t:=obs.MakeRange(obs.intp,obs.range);
  gen.range(types,t,NIL,NIL);
  gen.number(t^.gen^.l,0 ,obs.intp);
  gen.number(t^.gen^.r,hi,obs.intp);
  t:=obs.MakeArr(t,base);
  gen.array(types,t);
END make_arr;

PROCEDURE char_to_string(VAR n: NODE);
  VAR x: pc.ref;
BEGIN
  gen.new(x,pc.aggregate);
  make_arr(n.type,1,n.type);
  x^.dw:=n.type^.gen; x^.l:=x^.dw;
  gen.number(x^.r,0,n.type^.base);
  n.gen^.nxt:=x^.r;
  x^.r:=n.gen;
  n.gen:=x;
END char_to_string;

PROCEDURE parmlist(parm: obs.obj_ptr; VAR head: pc.ref);

  PROCEDURE string_cmp(t1,t2: obs.type_ptr): BOOLEAN;
  BEGIN
    RETURN (t1^.mode=obs.arr) & Char?(t1^.base)
         & (t2^.mode=obs.arr) & Char?(t2^.base)
  END string_cmp;

  PROCEDURE parm_cmp(p: obs.obj_ptr; VAR node: NODE);
  BEGIN
    IF p^.type^.mode=obs.farr THEN
      IF p^.type^.base=obs.wordp THEN
        IF node.type^.mode IN obs.SIMPLEs THEN scan.err(ers.incompatible) END;
      ELSE
        IF NOT (node.type^.mode IN obs.ARRs) THEN scan.err(ers.incompatible)
        ELSE TypeCmp(p^.type^.base,node.type^.base);
        END;
      END;
    ELSIF p^.type=obs.wordp THEN
      chkSimple(node.type); node.type:=p^.type;
      IF obs.varpar IN p^.tags THEN unary(node,pc.size_check);
      ELSE unary(node,pc.type_transfer);
      END;
    ELSIF node.type=obs.wordp THEN chkSimple(p^.type);
    ELSIF (p^.type^.mode=obs.arr) & (p^.type^.base=obs.wordp) THEN
      node.type:=p^.type; unary(node,pc.size_check);
    ELSIF p^.type^.mode IN obs.PROCs THEN ProcCmp(p^.type,node.type)
    ELSIF obs.varpar IN p^.tags THEN TypeCmp(p^.type,node.type)
    ELSIF (node.mode=obs.cons) & string_cmp(p^.type,node.type) THEN
    ELSE TypeRep(node,p^.type); TypeCmp(p^.type,node.type)
    END;
  END parm_cmp;

  PROCEDURE transport(VAR node: NODE; parm: obs.obj_ptr);
    VAR base: obs.type_ptr;
  BEGIN
    ASSERT(node.type^.mode=obs.farr);
    base:=parm^.type^.base;
    IF base^.mode IN obs.PROCs THEN
      ProcCmp(node.type^.base,base)
    ELSE
      TypeCmp(node.type^.base,base);
    END;
    IF (obs.varpar IN parm^.tags)#(obs.varpar IN node.tags) THEN
      scan.err(ers.parm_spec)
    END;
  END transport;

  PROCEDURE sequence(parm: obs.obj_ptr; empty: BOOLEAN);
    VAR node: NODE; l: pc.ref; no: INTEGER; t,base: obs.type_ptr;
  BEGIN
    base:=parm^.type^.base;
    gen.tie(head,pc.assign);
    head^.l:=parm^.gen;
    l:=NIL; no:=0;
    IF NOT empty THEN
      LOOP
        IF sy=scan.rpar THEN EXIT END;
        IF obs.varpar IN parm^.tags THEN
          designator(node); vari(node);
          IF obs.RO IN node.tags THEN scan.err(ers.readonly) END;
        ELSE
          Expr(node);
        END;
        IF (obs.seqpar IN node.tags) & (no=0) THEN
          transport(node,parm);
          head^.r:=node.gen;
          CheckGet(scan.rpar); RETURN
        END;
        IF base=obs.wordp THEN
        ELSIF base^.mode IN obs.PROCs THEN ProcCmp(node.type,base)
        ELSIF (node.mode=obs.cons) & string_cmp(base,node.type) THEN
        ELSE TypeRep(node,base); TypeCmp(node.type,base)
        END;
        IF obs.varpar IN parm^.tags THEN unary(node,pc.adr)
        ELSIF base=obs.wordp THEN
          node.type:=obs.intp; unary(node,pc.worder)
        END;
        node.gen^.nxt:=l; l:=node.gen;
        INC(no);
        IF sy=scan.coma THEN GetSy ELSE EXIT END;
      END;
      CheckGet(scan.rpar);
      gen.inverse(l);
    END;
    gen.new(head^.r,pc.aggregate);
    make_arr(t,no-1,obs.intp);
    head^.r^.r :=l;
    head^.r^.l :=t^.gen;
    head^.r^.dw:=t^.gen;
  END sequence;

  PROCEDURE empty_list;
  BEGIN
    IF parm=NIL THEN RETURN END;
    IF obs.seqpar IN parm^.tags THEN sequence(parm,TRUE);
    ELSE scan.err(ers.parm_quan)
    END;
  END empty_list;

  CONST arrs = obs.Types{obs.arr,obs.farr};
  VAR ok: BOOLEAN; n: NODE;
BEGIN
  IF sy#scan.lpar THEN empty_list; RETURN
  ELSE GetSy;
    IF sy=scan.rpar THEN empty_list; GetSy; RETURN END;
  END;
  ok:=TRUE;
  LOOP
    IF ok & (parm=NIL) THEN scan.err(ers.parm_quan); ok:=FALSE END;
    IF NOT ok THEN Expr(n);
    ELSIF obs.seqpar IN parm^.tags THEN sequence(parm,FALSE); RETURN
    ELSIF obs.varpar IN parm^.tags THEN
      designator(n); vari(n);
      IF obs.RO IN n.tags THEN scan.err(ers.readonly) END;
      parm_cmp(parm,n);
    ELSE
      Expr(n);
      IF (n.mode=obs.cons) & Char?(n.type) & (parm^.type^.mode IN arrs) THEN
        TypeCmp(n.type,parm^.type^.base);
        char_to_string(n);
      ELSE
        parm_cmp(parm,n);
      END;
    END;
    IF ok THEN
      gen.tie(head,pc.assign);
      head^.l:=parm^.gen;
      head^.r:=n.gen;
      parm:=parm^.next
    END;
    IF sy=scan.coma THEN GetSy ELSE EXIT END;
  END;
  IF ok THEN empty_list END;
  CheckGet(scan.rpar);
END parmlist;

PROCEDURE skip_parms;
  VAR node: NODE;
BEGIN
  IF sy#scan.lpar THEN RETURN END;
  GetSy;
  LOOP
    IF sy=scan.rpar THEN EXIT END;
    Expr(node);
    IF sy=scan.coma THEN GetSy ELSE EXIT END;
  END;
  GetSy;
END skip_parms;

PROCEDURE ProcCall(VAR n: NODE; VAR head: pc.ref);
BEGIN
  IF n.mode=obs.std_proc THEN
    IF n.pno<0 THEN sProcCall(n,head)
    ELSE scan.err(ers.func_call); skip_parms;
    END;
    RETURN
  END;
  IF NOT (n.mode IN obs.Modes{obs.cons,obs.vari}) THEN
    err?(n,ers.procedure);
  END;
  IF n.type^.mode#obs.proctype THEN
    IF    n.type^.mode=obs.functype THEN scan.err(ers.func_call);
    ELSIF n.type^.mode#obs.invtype  THEN scan.err(ers.procedure);
    END;
    skip_parms; RETURN
  END;
  gen.tie(head,pc.call);
  head^.l:=n.gen;
  parmlist(n.type^.plist,head^.r);
  gen.inverse(head^.r);
END ProcCall;

PROCEDURE FuncCall(VAR n: NODE);
  VAR call: pc.ref;
BEGIN
  IF NOT (n.mode IN obs.Modes{obs.cons,obs.vari}) THEN
    err?(n,ers.procedure);
  END;
  IF n.type^.mode#obs.functype THEN
    IF    n.type^.mode=obs.proctype THEN scan.err(ers.proc_call);
    ELSIF n.type^.mode#obs.invtype  THEN scan.err(ers.procedure);
    END;
    skip_parms;
    n.mode:=obs.inv; n.type:=obs.Any;
    RETURN
  END;
  gen.new(call,pc.call);
  parmlist(n.type^.plist,call^.r);
  gen.inverse(call^.r);
  call^.l:=n.gen;
  n.gen:=call;
  n.mode:=obs.expr;
  n.type:=n.type^.base;
  call^.dw:=n.type^.gen;
END FuncCall;

------------------------  EXPRESSIONs  ------------------------
                        ---------------

PROCEDURE Expr(VAR e1: NODE);

  PROCEDURE sFuncCall(no: INTEGER; VAR o: NODE);

    PROCEDURE size(VAR o: NODE; m: pc.mode);
      CONST
       valid   = obs.Modes{obs.type,obs.cons,obs.vari,obs.proc};
       dynamic = obs.Types{obs.farr,obs.dynarr};
    BEGIN
      designator(o);
      IF  NOT (o.mode IN valid)  THEN err?(o,ers.no_value)
      ELSIF o.type^.mode IN dynamic THEN o.mode:=obs.expr
      ELSE o.mode:=obs.cons
      END;
      o.type:=obs.intp;
      unary(o,m);
    END size;

    PROCEDURE low_high(VAR o: NODE; m: pc.mode);
       CONST valid   = obs.Modes{obs.cons,obs.vari};
    BEGIN
      designator(o);
      IF NOT (o.mode IN valid) THEN err?(o,ers.no_value)
      ELSIF NOT (o.type^.mode IN obs.ARRs) THEN err?(o,ers.array)
      ELSIF o.type^.mode=obs.arr THEN
        o.mode:=obs.cons; o.type:=o.type^.inx;
      ELSE
        o.mode:=obs.expr; o.type:=obs.intp;
      END;
      unary(o,m);
    END low_high;

    PROCEDURE len(VAR o: NODE; m: pc.mode);
       CONST valid   = obs.Modes{obs.cons,obs.vari};
    BEGIN
      designator(o);
      IF NOT (o.mode IN valid) THEN err?(o,ers.no_value)
      ELSIF NOT (o.type^.mode IN obs.ARRs) THEN err?(o,ers.array)
      ELSIF o.type^.mode=obs.arr THEN
        o.mode:=obs.cons; o.type:=obs.intp;
      ELSE
        o.mode:=obs.expr; o.type:=obs.intp;
      END;
      unary(o,m);
    END len;

    VAR v: obs.obj_ptr;
  BEGIN
    CheckGet(scan.lpar);
    CASE no OF
      |_min,_max:
         qualident(v);
         IF v^.mode#obs.type THEN err?(o,ers.type) END;
         o.mode:=v^.mode; o.type:=v^.type; o.gen:=v^.gen;
         IF    o.type^.mode=obs.bitset  THEN o.type:=obs.intp;
         ELSIF o.type^.mode=obs.settype THEN o.type:=o.type^.base;
         ELSE obs.chkScalar(o.type);
         END;
         IF no=_max THEN unary(o,pc.max) ELSE unary(o,pc.min) END;
         o.mode:=obs.cons;
      |_adr :
         designator(o); vari(o);
         o.type:=obs.addrp; o.mode:=obs.expr;
         unary(o,pc.adr);
      |_ref :
         designator(o); vari(o);
         CheckGet(scan.coma); qualident(v);
         IF    v^.mode#obs.type      THEN scan.err(ers.type);
         ELSIF v^.type^.mode#obs.ptr THEN scan.err(ers.pointer);
         ELSE
           TypeCmp(o.type,v^.type^.base);
           o.type:=v^.type;
           o.mode:=obs.expr;
         END;
         unary(o,pc.adr);
      |_size : size(o,pc.size);
      |_bytes: size(o,pc.bytes);
      |_bits : size(o,pc.bits);
      |_high : low_high(o,pc.high);
      |_low  : low_high(o,pc.low);
      |_len  : len(o,pc.len);
      |_abs  :
         Expr(o);
         IF o.type^.mode#obs.real THEN obs.chkScalar(o.type) END;
         unary(o,pc.abs);
      |_odd :
         Expr(o);
         obs.chkScalar(o.type); o.type:=obs.boolp;
         unary(o,pc.odd);
      |_ord :
         Expr(o);
         obs.chkScalar(o.type); o.type:=obs.intp;
         unary(o,pc.type_transfer);
      |_chr :
         Expr(o);
         obs.chkScalar(o.type); o.type:=obs.charp;
         unary(o,pc.range_check);
      |_cap :
         Expr(o);
         TypeCmp(o.type,obs.charp); o.type:=obs.charp;
         unary(o,pc.cap);
      |_float:
         Expr(o);
         obs.chkScalar(o.type); o.type:=obs.realp;
         unary(o,pc.float);
      |_trunc:
         Expr(o);
         TypeCmp(o.type,obs.realp); o.type:=obs.intp;
         unary(o,pc.trunc);
    ELSE ASSERT(FALSE);
    END;
    CheckGet(scan.rpar);
  END sFuncCall;

  PROCEDURE set(VAR e: NODE; type: obs.type_ptr);
    VAR a,b: NODE; l: pc.ref;
  BEGIN
    e.mode:=obs.cons;
    e.type:=type;
    l:=NIL;
    GetSy;
    IF sy#scan.rbrace THEN
      LOOP
        Expr(a);
        TypeRep(a,type^.base);
        TypeCmp(a.type,type^.base);
        IF a.mode#obs.cons THEN e.mode:=obs.expr END;
        IF sy=scan.range  THEN
          GetSy; Expr(b);
          TypeRep(b,type^.base);
          TypeCmp(b.type,type^.base);
          IF b.mode#obs.cons THEN e.mode:=obs.expr END;
          gen.tie(l,pc.range);
          l^.dw:=a.type^.gen;
          l^.l:=a.gen;
          l^.r:=b.gen;
        ELSE
          a.gen^.nxt:=l; l:=a.gen;
        END;
        IF sy=scan.rbrace THEN GetSy; EXIT END;
        IF sy=scan.coma   THEN GetSy
        ELSE scan.expc(scan.rbrace);  EXIT
        END
      END;
    ELSE GetSy
    END;
    gen.new(e.gen,pc.aggregate);
    e.gen^.l :=type^.gen;
    e.gen^.dw:=type^.gen;
    gen.inverse(l);
    e.gen^.r:=l;
  END set;

  PROCEDURE expr_seq(VAR e: NODE; VAR n: INTEGER; base: obs.type_ptr);
    VAR expr: NODE; l: pc.ref;
  BEGIN
    e.mode:=obs.cons;
    l:=NIL; n:=0;
    GetSy;
    LOOP
      Expr(expr);
      TypeRep(expr,base);
      TypeCmp(expr.type,base);
      IF expr.mode#obs.cons THEN e.mode:=obs.expr  END;
      expr.gen^.nxt:=l; l:=expr.gen;
      INC(n);
      IF sy=scan.rbrace THEN GetSy; EXIT END;
      IF sy=scan.coma   THEN GetSy
      ELSE scan.expc(scan.rbrace); EXIT
      END;
    END;
    gen.new(e.gen,pc.aggregate);
    gen.inverse(l);
    e.gen^.r:=l;
    e.gen^.l :=e.type^.gen;
    e.gen^.dw:=e.type^.gen;
  END expr_seq;

  PROCEDURE array(VAR e: NODE; type: obs.type_ptr);
    VAR n: INTEGER;
  BEGIN
    expr_seq(e,n,type^.base);
    e.gen^.l :=e.type^.gen;
    e.gen^.dw:=e.type^.gen;
--  size_equ
  END array;

  PROCEDURE open_array(VAR e: NODE; base: obs.type_ptr);
    VAR n: INTEGER;
  BEGIN
    IF sy#scan.lbrace THEN
      scan.expc(scan.lbrace); RETURN
    END;
    expr_seq(e,n,base);
    make_arr(e.type,n-1,base);
    e.gen^.l :=e.type^.gen;
    e.gen^.dw:=e.type^.gen;
  END open_array;

  PROCEDURE iden(VAR e: NODE);
    VAR e1: NODE;
  BEGIN
    designator(e);
    IF sy=scan.lpar THEN
      IF e.mode=obs.type THEN
        GetSy; Expr(e1); CheckGet(scan.rpar);
        e1.type:=e.type;
        unary(e1,pc.type_transfer);
        e:=e1;
      ELSIF e.mode=obs.std_proc THEN
        IF e.pno>=0 THEN sFuncCall(e.pno,e);
        ELSE scan.err(ers.proc_call); skip_parms;
        END;
      ELSE
        FuncCall(e);
      END;
    ELSIF sy=scan.lbrace THEN
      IF e.mode#obs.type THEN err?(e,ers.type)
      ELSIF e.type^.mode IN obs.SETs THEN set(e,e.type);
      ELSIF e.type^.mode=obs.arr     THEN array(e,e.type);
      ELSE err?(e,ers.ill_expr);
      END;
      e.modno:=0;
    ELSIF NOT (e.mode IN obs.Modes{obs.vari,obs.proc,obs.cons,obs.expr}) THEN
      err?(e,ers.no_value);
    END;
  END iden;

  PROCEDURE literal(VAR e: NODE; type: obs.type_ptr);
  BEGIN
    e.mode:=obs.cons;
    e.type:=type;
    e.tags:={};
    e.modno:=0;
    gen.number(e.gen,scan.cVal,type);
  END literal;

  PROCEDURE string(VAR e: NODE; VAL val: ARRAY OF CHAR; len: INTEGER);
    VAR t: obs.type_ptr; i: INTEGER;
  BEGIN
    make_arr(e.type,scan.sLen-1,obs.charp);
    e.mode:=obs.cons;
    gen.new(e.gen,pc.string);
    e.gen^.dw:=e.type^.gen;
    e.modno:=0;
    NEW(e.gen^.str,len);
    FOR i:=0 TO len-1 DO e.gen^.str[i]:=val[i] END;
  END string;

  PROCEDURE Factor(VAR e: NODE);
    VAR v: obs.obj_ptr;
  BEGIN
    e.mode:=obs.inv; e.type:=obs.Any; e.modno:=0;
    CASE sy OF
      |scan.intval : literal(e,obs.uintp); GetSy;
      |scan.charval: literal(e,obs.charp); GetSy;
      |scan.realval: literal(e,obs.realp); GetSy;
      |scan.string : string(e,scan.sVal,scan.sLen); GetSy;
      |scan.lpar   : GetSy; Expr(e); CheckGet(scan.rpar);
      |scan.ident  : iden(e);
      |scan.lbrace : set(e,obs.bitsetp);
      |scan.not    : GetSy; Factor(e);
                     TypeCmp(e.type,obs.boolp); e.type:=obs.boolp;
                     unary(e,pc.not);
      |scan.array  : GetSy; CheckGet(scan.of);
                     qualident(v); obs.chkType(v); open_array(e,v^.type);
    ELSE
      scan.err(ers.ill_expr);
      SkipTo(scan.rpar,scan.semic);
      e.mode:=obs.cons; e.type:=obs.Any; e.gen:=NIL; e.tags:={};
    END;
  END Factor;

  PROCEDURE binary(VAR e: NODE; VAL e1: NODE; m: pc.mode);
    VAR x: pc.ref;
  BEGIN
    IF e.type^.mode=obs.range THEN e.type:=e.type^.base END;
    IF e1.mode#obs.cons THEN e.mode:=obs.expr END;
    gen.new(x,m);
    x^.l:=e.gen;
    x^.r:=e1.gen;
    x^.dw:=e.type^.gen;
    e.gen:=x;
    e.modno:=0;
  END binary;

  PROCEDURE check(t: obs.type_ptr; valid: obs.Types);
  BEGIN
    IF t^.mode IN valid THEN RETURN END;
    IF (t^.mode=obs.range) & (t^.base^.mode IN valid) THEN RETURN END;
    IF (t^.mode=obs.newrange) & (t^.base^.mode IN valid) THEN RETURN END;
    err?(e1,ers.incompatible);
  END check;

  PROCEDURE term(symbol: scan.Symbol; VAR e1: NODE; VAR e2: NODE);
    VAR op: pc.mode;
  BEGIN
    CASE symbol OF
      |scan.and:
         TypeCmp(e1.type,obs.boolp);
         TypeCmp(e2.type,obs.boolp);
         op:=pc.star;
      |scan.rol,scan.ror:
         chkSimple(e1.type);
         check(e2.type,obs.INTs);
         IF NOT (e1.type^.mode IN obs.Types{obs.int,obs.bitset}) THEN
           e1.type:=obs.wordp
         END;
         IF symbol=scan.rol THEN op:=pc.rol ELSE op:=pc.ror END;
      |scan.slash:
         TypeRep(e1,e2.type); TypeRep(e2,e1.type); TypeCmp(e1.type,e2.type);
         check(e1.type,obs.INTs+obs.SETs+obs.Types{obs.real});
         op:=pc.slash;
      |scan.times:
         TypeRep(e1,e2.type); TypeRep(e2,e1.type); TypeCmp(e1.type,e2.type);
         check(e1.type,obs.INTs+obs.SETs+obs.Types{obs.real});
         op:=pc.star;
      |scan.div:
         TypeRep(e1,e2.type); TypeRep(e2,e1.type); TypeCmp(e1.type,e2.type);
         check(e1.type,obs.INTs);
         op:=pc.div;
      |scan.mod:
         TypeRep(e1,e2.type); TypeRep(e2,e1.type); TypeCmp(e1.type,e2.type);
         check(e1.type,obs.INTs);
         op:=pc.mod;
      |scan.rem:
         TypeRep(e1,e2.type); TypeRep(e2,e1.type); TypeCmp(e1.type,e2.type);
         check(e1.type,obs.INTs);
         op:=pc.rem;
    ELSE ASSERT(FALSE);
    END;
    binary(e1,e2,op);
  END term;

  PROCEDURE Term(VAR e1: NODE);
    VAR e2: NODE; op: scan.Symbol;
  BEGIN
    Factor(e1);
    LOOP
      op:=sy;
      CASE op OF
        |scan.times,scan.div,scan.mod,scan.slash,scan.rem,
         scan.rol,scan.ror,scan.and:
      ELSE RETURN
      END;
      GetSy;
      Factor(e2);
      term(op,e1,e2);
    END;
  END Term;

  PROCEDURE Simple(VAR e1: NODE);
    CONST valid = obs.INTs + obs.SETs + obs.Types{obs.real};
    VAR e2: NODE; op: scan.Symbol;
  BEGIN
    IF    sy=scan.plus  THEN GetSy; Term(e1);
      check(e1.type,valid);
    ELSIF sy=scan.minus THEN GetSy; Term(e1);
      check(e1.type,valid);
      IF e1.type^.mode=obs.range THEN e1.type:=e1.type^.base END;
      unary(e1,pc.minus);
    ELSE Term(e1)
    END;
    LOOP
      op:=sy;
      IF (op#scan.plus) & (op#scan.minus) & (op#scan.or) THEN RETURN END;
      GetSy;
      Term(e2);
      IF op=scan.or THEN
        TypeCmp(e1.type,obs.boolp);
        TypeCmp(e2.type,obs.boolp);
        binary(e1,e2,pc.plus);
      ELSE (* +,- *)
        IF e1.type^.mode IN obs.Types{obs.addr,obs.ptr} THEN
          TypeRep(e2,obs.intp);
          TypeCmp(e2.type,obs.intp)
        ELSE
          TypeRep(e1,e2.type);
          TypeRep(e2,e1.type);
          TypeCmp(e1.type,e2.type);
        END;
        check(e1.type,valid);
        IF    op=scan.minus THEN binary(e1,e2,pc.minus);
        ELSIF op=scan.plus  THEN binary(e1,e2,pc.plus);
        ELSE ASSERT(FALSE);
        END;
      END;
    END;
  END Simple;

  PROCEDURE apply(op: scan.Symbol; VAR e1,e2: NODE);
    CONST
      set0 = obs.Types{obs.arr,obs.farr,obs.dynarr};
      set1 = obs.PROCs+obs.Types{obs.ptr,obs.word};
      set2 = obs.SETs;
      set3 = obs.INTs+obs.Types{obs.real,obs.char,obs.bool,
             obs.enum,obs.range,obs.newrange};
    VAR
      m: obs.TypeMode; mode: pc.mode;
  BEGIN
    IF op=scan.in THEN
      IF NOT (e2.type^.mode IN obs.SETs) THEN err?(e1,ers.incompatible)
      ELSE
        TypeRep(e1,e2.type^.base); TypeCmp(e2.type^.base,e1.type);
      END;
      mode:=pc.in;
    ELSE
      IF (e1.type^.mode IN set0) OR (e2.type^.mode IN set0) THEN
        IF e1.type^.mode IN set0 THEN TypeCmp(e1.type^.base,obs.charp);
        ELSIF e1.mode=obs.cons   THEN TypeCmp(e1.type,obs.charp);
          char_to_string(e1);
        ELSE err?(e1,ers.incompatible)
        END;
        IF e2.type^.mode IN set0 THEN TypeCmp(e2.type^.base,obs.charp);
        ELSIF e2.mode=obs.cons   THEN TypeCmp(e2.type,obs.charp);
          char_to_string(e2);
        ELSE err?(e2,ers.incompatible)
        END;
      ELSE
        m:=e1.type^.mode;
        TypeRep(e1,e2.type); TypeRep(e2,e1.type); TypeCmp(e1.type,e2.type);
        IF       (m IN set1) & ((op#scan.equ) & (op#scan.neq))
          OR     (m IN set2) & ((op=scan.leq) OR (op=scan.geq))
          OR NOT (m IN set1+set2+set3)
        THEN err?(e1,ers.incompatible)
        END;
      END;
      CASE op OF
        |scan.equ: mode:=pc.equal;
        |scan.neq: mode:=pc.inequality;
        |scan.lss: mode:=pc.less;
        |scan.gtr: mode:=pc.greater;
        |scan.leq: mode:=pc.less_equal;
        |scan.geq: mode:=pc.greater_equal;
      ELSE ASSERT(FALSE);
      END;
    END;
    e1.type:=obs.boolp;
    binary(e1,e2,mode);
  END apply;

  VAR e2: NODE; op: scan.Symbol;
BEGIN
  Simple(e1);
  LOOP
    op:=sy;
    CASE sy OF
      |scan.equ,scan.neq,scan.lss,scan.gtr,scan.leq,scan.geq,scan.in:
    ELSE RETURN
    END;
    GetSy;
    Simple(e2);
    apply(op,e1,e2);
  END;
END Expr;

PROCEDURE const_expr(VAR e: NODE);
BEGIN
  Expr(e);
  IF e.mode#obs.cons THEN err?(e,ers.const_expr) END;
END const_expr;

-------------------------  STATEMENTs  ------------------------
                         --------------

PROCEDURE StatSeq(VAR head: pc.ref; loop: pc.ref);

  PROCEDURE check_range(l: obs.type_ptr; VAR r: NODE);
  BEGIN
    IF  (l^.mode IN obs.Types{obs.enum,obs.range})
      & ((r_check IN scan.opts) OR (r.mode=obs.cons))
    THEN
      unary(r,pc.range_check);
      r.gen^.dw:=l^.gen;
    END;
  END check_range;

  PROCEDURE assign(VAR l,r: NODE);
  BEGIN
    IF l.mode#obs.vari  THEN err?(l,ers.variable); RETURN END;
    IF obs.RO IN l.tags THEN err?(l,ers.readonly); RETURN END;
    IF  (r.mode=obs.cons) & Char?(r.type)
      & (l.type^.mode IN obs.ARRs) & Char?(l.type^.base)
    THEN
      char_to_string(r);
    ELSE
      TypeRep(r,l.type);
      AsgCmp(l.type,r.type);
      check_range(l.type,r);
    END;
    gen.tie(head,pc.assign);
    head^.l:=l.gen;
    head^.r:=r.gen;
  END assign;

  PROCEDURE ident;
    VAR l,r: NODE;
  BEGIN
    designator(l);
    IF sy=scan.becomes THEN
      vari(l); GetSy; Expr(r); assign(l,r);
    ELSE
      ProcCall(l,head);
    END;
  END ident;

  PROCEDURE if(VAR l: pc.ref);
    VAR n: NODE;
  BEGIN
    gen.tie(l,pc.if);
    Expr(n);
    l^.dw:=n.gen;
    CheckGet(scan.then);            StatSeq(l^.l,loop);
    IF    sy=scan.else  THEN GetSy; StatSeq(l^.r,loop);
    ELSIF sy=scan.elsif THEN GetSy; if(l^.r);
    END;
  END if;

  PROCEDURE return;
    VAR o: NODE;
  BEGIN
    gen.tie(head,pc.return);
    GetSy;
    IF obs.CurBlock^.type^.mode=obs.functype THEN
      Expr(o);
      TypeRep(o,obs.CurBlock^.type^.base);
      AsgCmp(obs.CurBlock^.type^.base,o.type);
      check_range(obs.CurBlock^.type^.base,o);
      head^.dw:=o.gen;
    END;
  END return;

  PROCEDURE with;
    VAR v: obs.obj_ptr; o: NODE;
  BEGIN
    GetSy;
    gen.tie(head,pc.block);
    designator(o); vari(o);
    unary(o,pc.adr);
    obs.new_obj(v,obs.vari); v^.next:=withs; withs:=v;
    IF o.mode#obs.vari      THEN scan.err(ers.no_value) END;
    IF o.type^.mode#obs.rec THEN scan.err(ers.record)   END;
    IF obs.RO IN o.tags     THEN INCL(v^.tags,obs.RO)   END;
    v^.type:=obs.MakePtr(o.type);
    gen.pointer(head^.dw,v^.type);
    gen.var(head^.dw,v);
    gen.tie(head^.dw,pc.assign);
    gen.usage(head^.dw^.l,v);
    o.gen^.dw:=v^.type^.gen;
    head^.dw^.r:=o.gen;
    CheckGet(scan.do); StatSeq(head^.dw,loop); CheckGet(scan.end);
    ASSERT(v=withs);
    withs:=withs^.next;
  END with;

  PROCEDURE for;
    VAR o: NODE; v: obs.obj_ptr; id: INTEGER; l: pc.ref;
  BEGIN
    GetSy; id:=scan.Id;
    IF sy#scan.ident THEN scan.expc(scan.ident); id:=scan.DmId END;
    v:=obs.vis(id);
    IF v^.mode#obs.vari THEN
      IF v^.mode#obs.inv THEN scan.err(ers.variable) END;
    ELSE
      IF obs.RO     IN v^.tags THEN scan.err(ers.readonly) END;
      IF obs.varpar IN v^.tags THEN scan.err(ers.loop_var) END;
    END;
    INCL(v^.tags,obs.RO);
    obs.chkScalar(v^.type);
    GetSy;
    CheckGet(scan.becomes);
    Expr(o);
    TypeRep(o,v^.type);
    TypeCmp(v^.type,o.type);
    gen.tie(head,pc.loop);
    gen.new(l,pc.assign); l^.r:=o.gen;
    gen.usage(l^.l,v);
    CheckGet(scan.to);
    Expr(o);
    gen.tie(l,pc.exit); l^.dw:=o.gen;
    TypeRep(o,v^.type);
    TypeCmp(v^.type,o.type);
    IF sy=scan.by THEN
      GetSy; Expr(o); TypeRep(o,obs.intp);
      IF o.type^.mode#obs.int THEN scan.err(ers.incompatible) END;
      gen.tie(l,pc.plus); l^.r:=o.gen;
    END;
    gen.inverse(l); head^.l:=l;
    CheckGet(scan.do);
    StatSeq(head^.dw,loop);
    CheckGet(scan.end);
    EXCL(v^.tags,obs.RO);
  END for;

  PROCEDURE case;

    PROCEDURE Variant(VAR e: NODE; v: pc.ref);
      VAR o,o1: NODE;
    BEGIN
      LOOP
        const_expr(o);
        TypeRep(o,e.type);
        TypeCmp(e.type,o.type);
        check_range(e.type,o);
        gen.tie(v^.l,pc.range);
        v^.l^.l:=o.gen;
        IF sy=scan.range THEN
          GetSy; const_expr(o1);
          TypeRep(o1,e.type);
          TypeCmp(e.type,o1.type);
          check_range(e.type,o1);
          v^.l^.r:=o1.gen;
        END;
        IF sy=scan.coma THEN GetSy
        ELSE
          IF sy#scan.colon THEN scan.expc(scan.colon) END; EXIT
        END;
      END;
      GetSy;
      StatSeq(v^.r,loop);
      IF (sy#scan.else) & (sy#scan.end) THEN CheckGet(scan.sep) END;
    END Variant;

    VAR e: NODE; empty: BOOLEAN;
  BEGIN
    gen.tie(head,pc.case);
    GetSy;
    Expr(e); obs.chkScalar(e.type);
    CheckGet(scan.of);
    head^.dw:=e.gen;
    empty:=TRUE;
    WHILE (sy#scan.end) & (sy#scan.else) DO
      IF sy=scan.sep THEN GetSy
      ELSE
        gen.tie(head^.l,pc.select);
        Variant(e,head^.l);
        empty:=FALSE;
      END;
    END;
    IF empty THEN scan.err(ers.empty_case) END;
    gen.inverse(head^.l);
    IF sy=scan.else THEN
      gen.new(head^.r,pc.block);
      GetSy; StatSeq(head^.r^.dw,loop)
    END;
    CheckGet(scan.end);
  END case;

  VAR n: NODE;
BEGIN
  LOOP
    CASE sy OF
     |scan.ident : ident;
     |scan.if    : GetSy; if(head); CheckGet(scan.end);
     |scan.while : gen.tie(head,pc.loop);
                   GetSy; Expr(n);
                   head^.l:=n.gen;
                   CheckGet(scan.do); StatSeq(head^.dw,loop);
                   CheckGet(scan.end);
     |scan.repeat: gen.tie(head,pc.loop);
                   GetSy; StatSeq(head^.dw,loop); CheckGet(scan.until);
                   Expr(n);
                   head^.r:=n.gen;
     |scan.loop  : gen.tie(head,pc.loop);
                   GetSy; StatSeq(head^.dw,head); CheckGet(scan.end);
     |scan.exit  : IF loop=NIL THEN scan.err(ers.ill_exit) END;
                   gen.tie(head,pc.exit); head^.dw:=loop;
                   GetSy;
     |scan.return: return;
     |scan.for   : for
     |scan.case  : case
     |scan.with  : with
(*
     |scan.rol   : GetSy;
                   IF sy=scan.ident THEN gen.mark(scan.Id); GetSy
                   ELSE scan.expc(scan.ident)
                   END;
     |scan.ror   : GetSy;
                   IF sy=scan.ident THEN gen.goto(scan.Id); GetSy
                   ELSE scan.expc(scan.ident)
                   END;
*)
     |scan.semic :
     |scan.end,scan.else,scan.elsif,scan.until,scan.sep: EXIT
    ELSE
      scan.err(ers.statement);
      SkipTo(scan.semic,scan.else,scan.elsif,scan.until,scan.sep);
    END;
    CASE sy OF
     |scan.end,scan.else,scan.elsif,scan.until,scan.sep: EXIT
    ELSE CheckGet(scan.semic);
    END;
  END;
  gen.inverse(head);
END StatSeq;

---------------------------  TYPEs  ---------------------------
                           ---------

VAR FWD: inter.QUEUE; -- очередь указателей вперед

PROCEDURE checkFWD;
  VAR t: obs.type_ptr; o: obs.obj_ptr;
BEGIN
  WHILE inter.pop(FWD,t) DO
    IF t^.mode=obs.ptr THEN
      ASSERT(t^.tid>=0);
      o:=obs.vis(t^.tid); obs.chkType(o);
      t^.base:=o^.type; t^.tid:=-1;
      t^.gen^.dw:=o^.type^.gen;
    ELSE ASSERT(t^.mode=obs.invtype);
    END;
  END;
  inter.clear(FWD);
END checkFWD;

PROCEDURE IdList(VAR q: inter.QUEUE; term: scan.Symbol);
BEGIN
  IF sy=term THEN
    scan.expc(scan.ident); GetSy; RETURN
  END;
  LOOP
    IF sy=scan.ident THEN inter.push(q,scan.Id) END;
    CheckGet(scan.ident);
    IF    sy=scan.coma  THEN GetSy
    ELSIF sy=term       THEN EXIT
    ELSIF sy=scan.ident THEN scan.expc(scan.coma)
    ELSE EXIT
    END
  END;
  IF sy#term THEN scan.expc(term) END;
END IdList;

PROCEDURE FormalType(VAR head: pc.ref): obs.type_ptr;
  VAR o: obs.obj_ptr; f: BOOLEAN; t: obs.type_ptr;
BEGIN
  f:=(sy=scan.array);
  IF f THEN GetSy; CheckGet(scan.of) END;
  qualident(o); obs.chkType(o);
  IF f THEN
    t:=obs.MakeFlx(o^.type);
    gen.array_of(head,t);
    RETURN t
  ELSE RETURN o^.type
  END;
END FormalType;

PROCEDURE function?(proc: obs.type_ptr);
  VAR o: obs.obj_ptr;
BEGIN
  IF sy=scan.colon THEN GetSy;
    qualident(o); obs.chkType(o);
    obs.FuncType(proc,o^.type);
    proc^.gen^.l:=o^.type^.gen;
  ELSE obs.ProcType(proc);
  END;
END function?;

PROCEDURE type(VAR head: pc.ref; VAR t: obs.type_ptr);

  PROCEDURE Range(VAR t: obs.type_ptr; mode: obs.TypeMode);
    VAR e1,e2: NODE;
  BEGIN
    const_expr(e1); CheckGet(scan.range); const_expr(e2);
    TypeRep(e1,e2.type); TypeRep(e2,e1.type);
    IF e1.type^.mode=obs.uint THEN TypeRep(e1,obs.intp) END;
    IF e2.type^.mode=obs.uint THEN TypeRep(e2,obs.intp) END;
    TypeCmp(e1.type,e2.type);
    t:=obs.MakeRange(e1.type,mode);
    gen.range(head,t,e1.gen,e2.gen);
  END Range;

  PROCEDURE Enum(VAR t: obs.type_ptr);
    VAR val: INTEGER; o,l: obs.obj_ptr; x: pc.ref;
  BEGIN
    t:=obs.MakeEnum(); val:=0; l:=NIL;
    gen.enum(head,t);
    IF sy=scan.rpar THEN scan.expc(scan.ident); GetSy; RETURN END;
    LOOP
      IF sy=scan.ident THEN
        o:=obs.AppEnum(t);
        IF l=NIL THEN t^.list:=o ELSE l^.list:=o END;
        l:=o;
        gen.number(x,val,t);
        gen.const(t^.gen^.dw,o,x);
        obs.dcl(scan.Id,o);
        INC(val);
      END;
      CheckGet(scan.ident);
      IF    sy=scan.coma  THEN GetSy
      ELSIF sy=scan.rpar  THEN EXIT
      ELSIF sy=scan.ident THEN scan.expc(scan.coma)
      ELSE EXIT END
    END;
    gen.inverse(t^.gen^.dw);
    CheckGet(scan.rpar);
  END Enum;

  PROCEDURE Array(VAR t: obs.type_ptr);
    VAR ix,elem: obs.type_ptr;
  BEGIN
    type(head,ix);
    IF sy=scan.coma THEN sy:=scan.array ELSE CheckGet(scan.of) END;
    type(head,elem);
    t:=obs.MakeArr(ix,elem);
    gen.array(head,t);
  END Array;

  PROCEDURE Pointer(VAR t: obs.type_ptr);
    VAR o: obs.obj_ptr;
  BEGIN
    CheckGet(scan.to);
    IF sy#scan.ident THEN
      type(head,t);
      t:=obs.MakePtr(t);
    ELSIF obs.vis?(scan.Id,o) THEN
      qualident(o); obs.chkType(o);
      t:=obs.MakePtr(o^.type)
    ELSE
      t:=obs.MakeFwdPtr(scan.Id); GetSy;
      inter.push(FWD,t);
    END;
    gen.pointer(head,t);
  END Pointer;

  PROCEDURE Record(VAR r: obs.type_ptr);

    PROCEDURE FieldList(VAR x: pc.ref); FORWARD;

    PROCEDURE SimpleFields(VAR x: pc.ref);
      VAR t: obs.type_ptr; id: INTEGER; que: inter.QUEUE;
          f: obs.obj_ptr;
    BEGIN
      inter.fifo(que);
      IdList(que,scan.colon); GetSy;
      type(head,t);
      WHILE inter.pop(que,id) DO
        f:=obs.AppField(r,id,t);
        gen.var(x,f);
      END;
      inter.clear(que);
    END SimpleFields;

    PROCEDURE VariantPart(VAR x: pc.ref);

      VAR labTyp: obs.type_ptr;

      PROCEDURE Variant(case: pc.ref);
        VAR e: NODE;
      BEGIN
        IF sy=scan.else THEN
          GetSy;
          FieldList(case^.r);
        ELSE
          WHILE sy#scan.colon DO
            const_expr(e); TypeRep(e,labTyp); TypeCmp(e.type,labTyp);
            IF sy=scan.range THEN GetSy;
              const_expr(e); TypeRep(e,labTyp); TypeCmp(e.type,labTyp);
            END;
            IF sy#scan.colon THEN CheckGet(scan.coma) END
          END;
          GetSy;
          gen.tie(case^.l,pc.select);
          FieldList(case^.l^.r);
        END;
        IF (sy#scan.else) & (sy#scan.end) THEN CheckGet(scan.sep) END;
      END Variant;

      VAR id: INTEGER; o: obs.obj_ptr;
    BEGIN
      CheckGet(scan.case);
      IF sy=scan.colon THEN (* unnamed tag *) id:=scan.DmId
      ELSIF sy#scan.ident THEN
        scan.expc(scan.ident); SkipTo(scan.end); RETURN
      ELSE id:=scan.Id; GetSy;
      END;
      CheckGet(scan.colon);
      qualident(o); obs.chkType(o);
      labTyp:=o^.type; obs.chkScalar(labTyp);
      IF id#scan.DmId THEN
        o:=obs.AppField(r,id,labTyp);
        gen.var(x,o);
      END;
      CheckGet(scan.of);
      gen.tie(x,pc.case);
      WHILE sy#scan.end DO
        IF sy=scan.sep THEN GetSy
        ELSE Variant(x);
        END;
      END;
      GetSy;
    END VariantPart;

    PROCEDURE FieldList(VAR x: pc.ref);
    BEGIN
      LOOP
        IF    sy=scan.ident THEN SimpleFields(x)
        ELSIF sy=scan.case  THEN VariantPart(x)
        ELSIF (sy=scan.end) OR (sy=scan.sep) OR (sy=scan.else) THEN EXIT
        ELSIF sy#scan.semic THEN
          scan.expc(scan.end); SkipTo(scan.end); GetSy; EXIT
        END;
        IF (sy#scan.end) & (sy#scan.sep) & (sy#scan.else) THEN
          CheckGet(scan.semic)
        END;
      END;
      gen.inverse(x);
    END FieldList;

  BEGIN
    r:=obs.MakeRec();
    IF ORD('P')-ORD('A') IN scan.opts THEN
      gen.new(r^.gen,pc.packed_record);
    ELSE
      gen.new(r^.gen,pc.record);
    END;
    FieldList(r^.gen^.dw);
    r^.gen^.nxt:=head; head:=r^.gen;
    CheckGet(scan.end);
  END Record;

  PROCEDURE procType(VAR proc: obs.type_ptr);
    VAR parm: obs.type_ptr; kind: BITSET; o: obs.obj_ptr; p: obs.obj_ptr;
  BEGIN
    proc:=obs.MakeProcType();
    gen.proctype(head,proc);
    IF sy#scan.lpar THEN obs.ProcType(proc); RETURN END;
    GetSy;
    LOOP
      kind:={};
      IF    sy=scan.var THEN GetSy; INCL(kind,obs.varpar);
      ELSIF sy=scan.val THEN GetSy; scan.err(ers.val_in_type);
      ELSIF (sy#scan.ident) & (sy#scan.array) THEN EXIT
      END;
      parm:=FormalType(proc^.gen^.dw);
      p:=obs.AppParm(proc,scan.DmId,parm,kind);
      gen.param(proc^.gen^.dw,p);
      IF sy#scan.rpar THEN CheckGet(scan.coma) END;
    END;
    IF sy=scan.seq THEN
      GetSy; kind:={obs.seqpar,obs.RO};
      IF sy=scan.var THEN GetSy; kind:=kind/{obs.varpar,obs.RO} END;
      qualident(o); obs.chkType(o);
      parm:=obs.MakeFlx(o^.type);
      gen.array_of(proc^.gen^.dw,parm);
      p:=obs.AppParm(proc,scan.DmId,parm,kind);
      gen.param(proc^.gen^.dw,p);
    END;
    CheckGet(scan.rpar);
    function?(proc);
    gen.inverse(proc^.gen^.dw);
  END procType;

  VAR o: obs.obj_ptr; lsy: scan.Symbol;
BEGIN
  IF sy=scan.ident THEN
    qualident(o); obs.chkType(o); t:=o^.type; RETURN
  END;
  t:=obs.Any;
  lsy:=sy; GetSy;
  CASE lsy OF
   |scan.lbr      : Range(t,obs.range);    CheckGet(scan.rbr);
   |scan.lbrace   : Range(t,obs.newrange); CheckGet(scan.rbrace);
   |scan.lpar     : Enum(t)
   |scan.set      : CheckGet(scan.of); type(head,t); t:=obs.MakeSet(t);
                    gen.set(head,t);
   |scan.procedure: procType(t)
   |scan.record   : Record(t);
   |scan.array    : Array(t)
   |scan.dynarr   : CheckGet(scan.of); type(head,t); t:=obs.MakeDynArr(t,1);
                    gen.dynarr(head,t);
   |scan.pointer  : Pointer(t)
  ELSE scan.err(ers.ill_type)
  END;
END type;

---------------------------  IMPORT  --------------------------
                           ----------

TYPE visout = PROCEDURE (INTEGER): obs.obj_ptr;

PROCEDURE importlist(outside: visout);
  VAR id: INTEGER; o : obs.obj_ptr; from?: BOOLEAN;
BEGIN
  WHILE (sy=scan.import) OR (sy=scan.from) DO
    from?:=(sy=scan.from);
    IF from? THEN GetSy;
      IF sy#scan.ident THEN scan.expc(scan.ident); o:=obs.Ilg;
      ELSE o:=outside(scan.Id); GetSy;
        IF o^.mode#obs.module THEN
          scan.err_id(ers.module,o^.id); o:=obs.Ilg
        END;
      END;
    END;
    CheckGet(scan.import);
    LOOP
      IF sy=scan.ident THEN id:=scan.Id; GetSy;
        IF   from? THEN obs.FromImport(o,id)
        ELSIF sy=scan.colon THEN GetSy;
          IF sy=scan.ident THEN obs.Import(outside(scan.Id),id) END;
          CheckGet(scan.ident);
        ELSE obs.Import(outside(id),id);
        END;
      ELSE CheckGet(scan.ident);
      END;
      IF    sy=scan.coma  THEN GetSy
      ELSIF sy=scan.semic THEN EXIT
      ELSIF sy=scan.ident THEN scan.expc(scan.coma)
      ELSE EXIT
      END
    END;
    CheckGet(scan.semic);
  END;
END importlist;

PROCEDURE conflict(out,in: obs.obj_ptr);
  VAR n: ers.T;
BEGIN
  IF (out^.mode=obs.proc) & (obs.forward IN out^.tags)
   & (in ^.mode=obs.proc) & NOT (obs.duplicate IN in^.tags)
  THEN
    n:=ProcCmp?(in^.type,out^.type);
    IF n#ers.blank THEN scan.err_id(n,out^.id) END;
    EXCL(out^.tags,obs.forward);
    ASSERT(FALSE);
--  gen.ProcEqu(in,out);
  ELSIF (in^.mode=obs.type) & (out^.mode=obs.type)
                            & (out^.type^.mode=obs.hidden) THEN
    IF obs.CurBlock#CU THEN scan.err(ers.only_in_cu); RETURN END;
    obs.set_hidden(out^.type,in^.type)
  ELSE scan.err_id(ers.ill_export,in^.id)
  END;
END conflict;

----------------------------  USE  ----------------------------
                            -------

PROCEDURE IniSTORAGE;

  PROCEDURE new(no: INTEGER; t: obs.type_ptr; VAL s: ARRAY OF CHAR);
    VAR o: obs.obj_ptr;
  BEGIN
    obs.new_obj(o,obs.std_proc); o^.no:=no;
    o^.type:=t; o^.tags:={obs.penetrate,obs.forward};
    obs.dcl_in(obs.SuperProc^.head,scan.str_id(s),o);
  END new;

  VAR t: obs.type_ptr; p: obs.obj_ptr;
BEGIN
  t:=obs.MakeProcType();
  p:=obs.AppParm(t,scan.DmId,obs.addrp,{obs.varpar});
  p:=obs.AppParm(t,scan.DmId,obs.intp,{});
  obs.ProcType(t);
  new(_new,t,'NEW');
  new(_dispose,t,'DISPOSE');
  t:=obs.MakeProcType();
  p:=obs.AppParm(t,scan.DmId,obs.addrp,{obs.varpar});
  p:=obs.AppParm(t,scan.DmId,obs.intp ,{obs.varpar});
  p:=obs.AppParm(t,scan.DmId,obs.intp ,{});
  p:=obs.AppParm(t,scan.DmId,obs.intp ,{});
  obs.ProcType(t);
  new(_resize,t,'RESIZE');
END IniSTORAGE;

PROCEDURE compare(rts,o: obs.obj_ptr);
  VAR err: ers.T;
BEGIN
  IF (rts^.mode=obs.inv) OR (o^.mode=obs.inv) THEN RETURN END;
  IF o^.mode#obs.proc THEN scan.err_id(ers.procedure,o^.id); RETURN END;
  err:=ProcCmp?(rts^.type,o^.type);
  IF err#ers.blank THEN scan.err_id(err,o^.id)
  ELSE
    IF NOT (obs.forward IN rts^.tags) THEN
      scan.err_id(ers.duplicate,rts^.id)
    END;
    EXCL(rts^.tags,obs.forward);
    rts^.gen:=o^.gen;
  END;
END compare;

PROCEDURE compare_storage(m: obs.obj_ptr);

  PROCEDURE check(VAL s1,s2: ARRAY OF CHAR);
    VAR p1,p2: obs.obj_ptr;
  BEGIN
    IF NOT obs.vis_in?(obs.SuperProc^.head,scan.str_id(s1),p1) THEN
      ASSERT(FALSE)
    END;
    IF NOT obs.vis_in?(m^.head,scan.str_id(s2),p2) THEN RETURN END;
    compare(p1,p2);
  END check;

BEGIN
  IF m^.mode#obs.module THEN scan.err(ers.module); RETURN END;
  check('NEW'    ,'ALLOCATE');
  check('DISPOSE','DEALLOCATE');
  check('RESIZE' ,'REALLOCATE');
END compare_storage;

PROCEDURE rts_list;
  VAR o,rts: obs.obj_ptr;
BEGIN ASSERT(sy=scan.with);
  GetSy;
  IF sy#scan.ident THEN scan.expc(scan.ident); RETURN END;
  IF scan.str_id('STORAGE')#scan.Id THEN scan.err_id(ers.invisible,scan.Id) END;
  GetSy;
  IF sy=scan.lpar THEN GetSy;
    LOOP
      IF sy#scan.ident THEN scan.expc(scan.ident); EXIT END;
      rts:=obs.vis_in(obs.SuperProc^.head,scan.Id);
      GetSy;
      IF sy#scan.colon THEN scan.expc(scan.colon); EXIT END;
      GetSy;
      qualident(o);
      compare(rts,o);
      IF sy=scan.semic THEN GetSy ELSE EXIT END;
    END;
    CheckGet(scan.rpar);
  ELSIF sy=scan.colon THEN GetSy;
    IF sy#scan.ident THEN scan.expc(scan.ident);
    ELSE
      o:=obs.vis(scan.Id); GetSy;
      compare_storage(o);
    END;
  ELSE scan.err(ers.ill_decl);
  END;
  CheckGet(scan.semic);
END rts_list;

----------------------------  DCLs  ---------------------------
                            --------

PROCEDURE Block(block: pc.ref);

  PROCEDURE ConstDcl(VAR head: pc.ref);
    VAR o: obs.obj_ptr; n: NODE;
  BEGIN
    ASSERT(sy=scan.const);
    GetSy;
    WHILE sy=scan.ident DO
      obs.new_obj(o,obs.cons); obs.dcl(scan.Id,o); GetSy;
      IF sy#scan.equ THEN scan.expc(scan.equ); RETURN END;
      GetSy;
      INCL(o^.tags,obs.undefined);
      const_expr(n);
      o^.type:=n.type;
      o^.no  :=n.modno;
      o^.mode:=n.mode;
      EXCL(o^.tags,obs.undefined);
      gen.const(head,o,n.gen);
      IF    sy=scan.semic THEN GetSy
      ELSIF sy=scan.ident THEN scan.expc(scan.semic)
      ELSE CheckGet(scan.semic); RETURN
      END;
    END;
  END ConstDcl;

  PROCEDURE VarDcl(VAR head: pc.ref; val?: BOOLEAN);

    PROCEDURE VarDclList;
      VAR f: inter.QUEUE; id: INTEGER;
          o: obs.obj_ptr; tags: BITSET; t: obs.type_ptr;
    BEGIN ASSERT(sy=scan.ident);
      inter.fifo(f); IdList(f,scan.colon); GetSy;
      IF val? THEN tags:={obs.RO} ELSE tags:={} END;
      type(head,t);
      WHILE inter.pop(f,id) DO
        obs.new_obj(o,obs.vari); o^.type:=t; o^.tags:=tags;
        obs.dcl(id,o);
        gen.var(head,o);
      END;
      inter.clear(f);
    END VarDclList;

  BEGIN GetSy;
    WHILE sy=scan.ident DO
      VarDclList;
      CheckGet(scan.semic);
    END;
  END VarDcl;

  PROCEDURE TypeDcl(VAR head: pc.ref);
    VAR id: INTEGER; o,hid: obs.obj_ptr; t: obs.type_ptr;
  BEGIN
    ASSERT(sy=scan.type); GetSy;
    WHILE sy=scan.ident DO id:=scan.Id; GetSy;
      IF sy=scan.equ THEN GetSy;
        IF obs.vis_in?(obs.CurBlock^.head,id,hid) THEN
          IF (hid^.mode=obs.type) & (hid^.type^.mode=obs.hidden) THEN
            IF obs.CurBlock#CU THEN scan.err(ers.only_in_cu) END;
            type(head,t);
            IF t^.mode#obs.ptr THEN gen.hidden(head,t) END;
            obs.set_hidden(hid^.type,t);
          ELSE scan.err_id(ers.duplicate,id); type(head,t);
          END;
        ELSE
          obs.new_obj(o,obs.type); obs.dcl(id,o); o^.no:=0;
          INCL(o^.tags,obs.undefined);
            type(head,o^.type);
          EXCL(o^.tags,obs.undefined);
          IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
          o^.gen:=o^.type^.gen;
        END;
      ELSIF (sy=scan.semic) & (unit=comp.def) THEN
        obs.new_obj(o,obs.type);  o^.no:=0;
        o^.type:=obs.MakeHidden(1);
        o^.type^.gen:=obs.addrp^.gen;
        obs.dcl(id,o);
        IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
        o^.gen:=o^.type^.gen;
      ELSE scan.expc(scan.equ)
      END; CheckGet(scan.semic);
    END;
  END TypeDcl;

  PROCEDURE ModuleDcl(VAR head: pc.ref);
    VAR id,mid: INTEGER; exps: inter.QUEUE; o: obs.obj_ptr;
          save: BITSET;
  BEGIN
    ASSERT(sy=scan.module); GetSy;
    IF sy#scan.ident THEN scan.Fault(ers.ident,'') END;
    mid:=scan.Id; GetSy; CheckGet(scan.semic);
    obs.new_obj(o,obs.module); o^.no:=mod_no; INC(mod_no);
    obs.dcl(mid,o);
    obs.EnterBlock(o);
    importlist(obs.vis_out);
    inter.fifo(exps);
    IF sy=scan.export THEN GetSy;
      IF sy=scan.qualified THEN INCL(o^.tags,obs.qualified); GetSy END;
      IdList(exps,scan.semic); GetSy;
    END;
    gen.module(head,o);
    Block(head);
    save:=scan.opts;
      INCL(scan.opts,ORD('A')-ORD('A'));
      WHILE inter.pop(exps,id) DO obs.Export(id) END;
      inter.clear(exps);
      obs.ExitModule(conflict);
    scan.opts:=save;
    IF sy=scan.ident THEN
      IF scan.Id#mid THEN scan.err_id(ers.block_name,mid) END; GetSy
    ELSE scan.expc(scan.ident)
    END;
    CheckGet(scan.semic);
  END ModuleDcl;

  PROCEDURE proc_comp(proc: obs.type_ptr);
    VAR o : obs.obj_ptr;   f,val: BOOLEAN;
      parm: obs.obj_ptr;  kind: BITSET;  id: INTEGER;
       que: inter.QUEUE;
  BEGIN
    IF sy#scan.lpar THEN
      IF proc^.plist#NIL THEN scan.err(ers.parm_quan) END; RETURN
    END;
    parm:=proc^.plist;
    GetSy;
    inter.fifo(que);
    LOOP
      kind:={}; val:=FALSE;
      IF    sy=scan.var   THEN GetSy; INCL(kind,obs.varpar);
      ELSIF sy=scan.val   THEN GetSy; val:=TRUE;
      ELSIF sy#scan.ident THEN EXIT
      END;
      IdList(que,scan.colon); GetSy; f:=(sy=scan.array);
      IF f THEN GetSy; CheckGet(scan.of) END;
      qualident(o); obs.chkType(o);
      WHILE (parm#NIL) & inter.pop(que,id) DO
        IF f THEN
          IF parm^.type^.mode#obs.farr THEN scan.err_id(ers.incompatible,id)
          ELSE TypeCmp(parm^.type^.base,o^.type);
            parm^.type^.base:=o^.type;
          END;
        ELSE TypeCmp(parm^.type,o^.type); parm^.type:=o^.type;
        END;
        IF kind#parm^.tags*{obs.varpar} THEN scan.err_id(ers.parm_spec,id) END;
        IF val THEN
          INCL(parm^.tags,obs.RO);
          gen.app_valparm(parm^.gen^.l);
        END;
        parm^.id:=id;
        parm:=parm^.next;
      END;
      IF (parm=NIL) & inter.pop(que,id) THEN scan.err_id(ers.parm_quan,id) END;
      IF sy#scan.rpar THEN CheckGet(scan.semic) END;
    END;
    inter.clear(que);
    IF sy=scan.seq THEN
      GetSy; kind:={obs.seqpar,obs.RO};
      IF sy=scan.var THEN GetSy; kind:=kind/{obs.varpar,obs.RO} END;
      IF sy=scan.ident THEN parm^.id:=scan.Id ELSE scan.expc(scan.ident) END;
      GetSy; CheckGet(scan.colon);
      qualident(o); obs.chkType(o);
      IF parm=NIL THEN scan.err(ers.parm_quan)
      ELSE
        IF parm^.type^.mode#obs.farr THEN
          scan.err_id(ers.incompatible,parm^.id)
        ELSE TypeCmp(parm^.type^.base,o^.type);
          parm^.type^.base:=o^.type;
        END;
        IF parm^.tags#kind THEN scan.err_id(ers.parm_spec,parm^.id) END;
        parm:=parm^.next;
      END;
    END;
    CheckGet(scan.rpar);
    IF parm#NIL THEN scan.err(ers.parm_quan) END;
    IF sy=scan.colon THEN GetSy;
      qualident(o); obs.chkType(o);
      IF proc^.mode=obs.functype THEN
        TypeCmp(proc^.base,o^.type); proc^.base:=o^.type;
      ELSE scan.err(ers.incompatible)
      END;
    ELSIF proc^.mode#obs.proctype THEN scan.err(ers.incompatible)
    END;
  END proc_comp;

  PROCEDURE headType(VAR head: pc.ref): obs.type_ptr;
    VAR proc,parm: obs.type_ptr; kind: BITSET; o: obs.obj_ptr; p: obs.obj_ptr;
        que: inter.QUEUE; id: INTEGER;
  BEGIN
    proc:=obs.MakeProcType();
    gen.proctype(head,proc);
    IF sy#scan.lpar THEN obs.ProcType(proc); RETURN proc END;
    GetSy;
    inter.fifo(que);
    LOOP
      kind:={};
      IF    sy=scan.var THEN GetSy; INCL(kind,obs.varpar);
      ELSIF sy=scan.val THEN GetSy;
        IF unit=comp.def THEN scan.err(ers.not_in_def)
        ELSE INCL(kind,obs.RO)
        END;
      ELSIF sy#scan.ident THEN EXIT
      END;
      IdList(que,scan.colon); GetSy;
      parm:=FormalType(proc^.gen^.dw);
      WHILE inter.pop(que,id) DO
        p:=obs.AppParm(proc,id,parm,kind);
        gen.param(proc^.gen^.dw,p);
      END;
      IF sy#scan.rpar THEN CheckGet(scan.semic) END
    END;
    inter.clear(que);
    IF sy=scan.seq THEN
      GetSy; kind:={obs.seqpar,obs.RO};
      IF sy=scan.var THEN GetSy; kind:=kind/{obs.varpar,obs.RO} END;
      IF sy#scan.ident THEN scan.expc(scan.ident); id:=scan.DmId
      ELSE id:=scan.Id
      END;
      GetSy; CheckGet(scan.colon);
      qualident(o); obs.chkType(o);
      parm:=obs.MakeFlx(o^.type);
      gen.array_of(proc^.gen^.dw,parm);
      p:=obs.AppParm(proc,id,parm,kind);
      gen.param(proc^.gen^.dw,p);
    END;
    CheckGet(scan.rpar);
    function?(proc);
    gen.inverse(proc^.gen^.dw);
    RETURN proc
  END headType;

  PROCEDURE ProcHead(VAR p: obs.obj_ptr; VAR head: pc.ref);
    VAR pre?: BOOLEAN; pid: INTEGER;
  BEGIN
    ASSERT(sy=scan.procedure); GetSy;
    IF sy#scan.ident THEN scan.expc(scan.ident); pid:=scan.DmId
    ELSE pid:=scan.Id
    END;
    GetSy;
    pre?:=obs.vis_in?(obs.CurBlock^.head,pid,p) & (obs.forward IN p^.tags);
    IF pre? THEN
      proc_comp(p^.type);
    ELSE
      obs.new_obj(p,obs.proc); p^.type:=headType(head);
      p^.no:=proc_no; INC(proc_no);
      obs.dcl(pid,p);
      gen.proc(head,p);
    END;
    CheckGet(scan.semic);
  END ProcHead;

  PROCEDURE CodeProc(VAR head: pc.ref);
    VAR i: INTEGER;
  BEGIN
    gen.tie(head,pc.string);
    NEW(head^.str,128); i:=0;
    LOOP
      IF i>HIGH(head^.str) THEN RESIZE(head^.str,LEN(head^.str)*2) END;
      head^.str[i]:=scan.GetChar();
      IF head^.str[i]=0c THEN head^.str[i]:=36c END;
      IF (head^.str[i]='D') &
         (head^.str[i-1]='N') & (head^.str[i-2]='E') THEN EXIT END;
      INC(i);
    END;
    head^.str[i-2]:=0c;
    RESIZE(head^.str,i-1);
    scan.GetSy;
  END CodeProc;

  PROCEDURE dclParms(p: obs.obj_ptr);
    VAR o: obs.obj_ptr; t: pc.ref;
  BEGIN
    WHILE (p#NIL) & (p^.id>=0) DO
      obs.new_obj(o,obs.vari);
      o^.type:=p^.type;
      o^.tags:=p^.tags;
      obs.dcl(p^.id,o);
      o^.gen:=p^.gen;
      t:=o^.type^.gen;
      IF (t^.md=pc.val_prm) OR (t^.md=pc.var_prm) THEN
        o^.type^.gen:=t^.dw;
      END;
      p:=p^.next;
    END;
  END dclParms;

  PROCEDURE ProcDcl(VAR head: pc.ref);
    VAR p: obs.obj_ptr;
  BEGIN
    ProcHead(p,head);
    IF unit=comp.def THEN RETURN END;
    IF sy=scan.forward THEN
      GetSy; CheckGet(scan.semic);
      IF obs.forward IN p^.tags THEN scan.err(ers.dup_forward) END;
      INCL(p^.tags,obs.forward); RETURN
    ELSIF sy=scan.code THEN
      IF obs.forward IN p^.tags THEN scan.err(ers.duplicate) END;
      INCL(p^.tags,obs.code_proc);
      gen.tie(head,pc.code_body);
      head^.l:=p^.gen;
      CodeProc(head^.dw);
      ASSERT(p^.gen^.md=pc.procedure);
      p^.gen^.md:=pc.inline;
    ELSE
      EXCL(p^.tags,obs.forward);
      obs.EnterBlock(p);
        dclParms(p^.type^.plist);
        gen.tie(head,pc.proc_body);
        head^.l:=p^.gen;
        Block(head);
      obs.ExitProc;
    END;
    IF sy=scan.ident THEN
      IF (p^.id#scan.DmId) & (p^.id#scan.Id) THEN
        scan.err_id(ers.block_name,p^.id)
      END;
      GetSy
    ELSE scan.expc(scan.ident)
    END;
    CheckGet(scan.semic);
  END ProcDcl;

  PROCEDURE checkFWDproc;
    CONST mask={obs.forward,obs.duplicate};
    VAR l: obs.obj_ptr;
  BEGIN
    l:=obs.CurBlock^.head^.locals;
    WHILE l#NIL DO
      IF (l^.mode=obs.proc) & (l^.tags*mask={obs.forward}) THEN
        scan.err_id(ers.unimpl_proc,l^.id)
      END;
      l:=l^.next;
    END;
  END checkFWDproc;

  VAR l: pc.ref; save: pc.ref; opts: BITSET;

BEGIN   (* Block *)
  inter.fifo(FWD);
  save:=types; types:=NIL;
  l:=block^.dw;
  LOOP
    CASE sy OF
     |scan.const    : ConstDcl(l);
     |scan.var      : VarDcl(l,FALSE);
     |scan.val      : IF unit#comp.def THEN scan.err(ers.only_in_def) END;
                      VarDcl(l,TRUE);
     |scan.type     : TypeDcl(l)
     |scan.module   : checkFWD; ModuleDcl(l); inter.fifo(FWD);
     |scan.procedure: checkFWD; ProcDcl(l);   inter.fifo(FWD);
     |scan.with     : IF unit=comp.def THEN scan.err(ers.not_in_def) END;
                      IF obs.CurBlock#CU THEN scan.err(ers.only_in_cu) END;
                      rts_list;
     |scan.begin,scan.end: checkFWD; EXIT
    ELSE scan.err(ers.ill_decl); GetSy
    END;
  END;
  IF unit#comp.def THEN gen.tie(l,pc.block) END;
  IF (sy=scan.begin) THEN
    GetSy;
    IF unit=comp.def THEN scan.err(ers.not_in_def)
    ELSIF sy#scan.end THEN StatSeq(l^.dw,NIL)
    END;
  END;
  gen.inverse(l);
  block^.dw:=l;
  IF types#NIL THEN
    gen.inverse(types);
    l:=types;
    WHILE l^.nxt#NIL DO l:=l^.nxt END;
    l^.nxt:=block^.dw;
    block^.dw:=types;
  END;
  types:=save;
  CheckGet(scan.end);
  IF unit#comp.def THEN
    opts:=scan.opts;
      INCL(scan.opts,ORD('A')-ORD('A'));
      checkFWDproc;
    scan.opts:=opts;
  END;
--  IF ORD('U')-ORD('A') IN scan.opts THEN vis.vis_tree(block) END;
END Block;

----------------------------------------------------------------

PROCEDURE external(id: INTEGER): obs.obj_ptr;
  VAR o: obs.obj_ptr;
BEGIN
  IF obs.vis_in?(obs.SuperProc^.head,id,o) & (obs.complete IN o^.tags) THEN
    RETURN o
  END;
  sym.get_sym(CU,id);
  o:=obs.vis_in(obs.SuperProc^.head,id);
  gen.inverse(o^.gen);
--vis.vis_tree(o^.gen);
  RETURN o
END external;

PROCEDURE compilationUnit;
  VAR def_time,imp_time,tag,cu_id: INTEGER;
BEGIN
  IF    sy=scan.definition     THEN GetSy; unit:=comp.def;
  ELSIF sy=scan.implementation THEN GetSy; unit:=comp.imp;
  ELSE unit:=comp.main;
  END;
  IF sy#scan.module THEN scan.Fault(ers.ill_header,''); RETURN END;
  GetSy;
  IF sy#scan.ident  THEN scan.Fault(ers.ill_header,''); RETURN END;
  cu_id:=scan.Id; scan.id_str(scan.Id,name);
  GetSy;
  tag:=0;
  IF sy=scan.lbr THEN GetSy;
    IF sy=scan.intval THEN tag:=scan.cVal; GetSy END;
--  ASSERT(tag=0);
    CheckGet(scan.rbr);
  END;
  CheckGet(scan.semic);
  pc.ini_gen;
  obs.new_obj(CU,obs.module); CU^.no:=0;
  obs.dcl(cu_id,CU);
  gen.new(CU^.gen,pc.module); CU^.gen^.nm:=cu_id;
  obs.EnterBlock(CU);
  def_time:=inter.time(); imp_time:=-1;
  IF unit=comp.imp THEN
    sym.get_sym(CU,cu_id);
    imp_time:=def_time; def_time:=CU^.head^.ctime;
  ELSIF unit=comp.main THEN
    CU^.head^.ctime:=def_time; imp_time:=def_time
  END;
  importlist(external);
  CU^.gen^.dw:=types; types:=NIL;  -- standard types
  Block(CU^.gen);
  IF (sy#scan.ident) OR (scan.Id#CU^.id) THEN scan.err(ers.block_name)
  ELSE GetSy;
    IF sy#scan.period THEN scan.expc(scan.period) END
  END;
  IF ORD('V')-ORD('A') IN scan.opts THEN vis.vis_tree(CU^.gen) END;
  IF scan.noErrors=0 THEN
    IF unit=comp.def THEN
      pc.gen_def(CU^.gen,def_time)
    ELSE
      pc.gen_code(CU^.gen,def_time,imp_time,code)
    END;
  END;
  obs.ExitModule(conflict);
  IF scan.noErrors=0 THEN
    sym.put_sym(CU,unit,def_time,imp_time);
  END;
END compilationUnit;

--------------------------  COMPILE  --------------------------
                          -----------

VAR end?: BOOLEAN;

PROCEDURE IniSTANDARD;

  VAR stnd?: BOOLEAN;

  PROCEDURE type0(t: obs.type_ptr; VAL s: ARRAY OF CHAR);
    VAR o: obs.obj_ptr;
  BEGIN
    obs.new_obj(o,obs.type); o^.type:=t; o^.no:=0;
    obs.dcl(scan.str_id(s),o);
    IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
    o^.gen:=o^.type^.gen;
    IF stnd? THEN INCL(o^.tags,obs.penetrate) END;
  END type0;

  PROCEDURE type(VAR t: obs.type_ptr; m: obs.TypeMode;
                 VAL s: ARRAY OF CHAR; pm: pc.mode);
  BEGIN
    obs.new_type(t,m);
    gen.tie(types,pm); t^.gen:=types;
    type0(t,s);
  END type;

  PROCEDURE proc(VAL s: ARRAY OF CHAR; no: INTEGER);
    VAR o: obs.obj_ptr;
  BEGIN
    obs.new_obj(o,obs.std_proc); o^.no:=no;
    o^.type:=obs.Any; -- NIL;
    IF stnd? THEN INCL(o^.tags,obs.penetrate) END;
    obs.dcl(scan.str_id(s),o);
  END proc;

  PROCEDURE const(s: ARRAY OF CHAR; val: INTEGER; t: obs.type_ptr): obs.obj_ptr;
    VAR o: obs.obj_ptr; r: pc.ref;
  BEGIN
    obs.new_obj(o,obs.cons);
    o^.type:=t; o^.tags:={obs.penetrate};
    obs.dcl(scan.str_id(s),o);
    gen.number(r,val,t);
    gen.const(types,o,r);
    RETURN o
  END const;

  PROCEDURE IniSYSTEM;
    VAR system: obs.obj_ptr; o: obs.obj_ptr;
  BEGIN
    obs.new_obj(system,obs.module);
    system^.tags:={obs.qualified,obs.complete};
    obs.dcl(scan.str_id("SYSTEM"),system);
    obs.EnterBlock(system);
      type(obs.wordp,obs.word,"WORD",pc.integer);
      type(obs.addrp,obs.addr,"ADDRESS",pc.pointer);
      proc("ADR",_adr);
      o:=system^.head^.locals;
      WHILE o#NIL DO obs.Export(o^.id); o:=o^.next END;
    obs.ExitModule(conflict);
  END IniSYSTEM;

  VAR o: obs.obj_ptr;
BEGIN
  stnd?:=FALSE; IniSYSTEM; stnd?:=TRUE;

  type(obs.intp,obs.int,"INTEGER",pc.integer);
  type(obs.uintp,obs.uint,".INTEGER",pc.integer);
  type(obs.charp,obs.char,"CHAR",pc.char);
  type(obs.procp,obs.proctype,"PROC",pc.profile);
  obs.procp^.base:=NIL; obs.procp^.plist:=NIL;
  type(obs.realp,obs.real,"REAL",pc.real);

  type(obs.boolp,obs.bool,"BOOLEAN",pc.boolean);
  o:=const("FALSE",0,obs.boolp);
  o:=const("TRUE" ,1,obs.boolp);

  obs.new_type(obs.bitsetp,obs.bitset);
  obs.bitsetp^.base:=obs.MakeRange(obs.intp,obs.range);
  gen.bitset(types,obs.bitsetp);
  type0(obs.bitsetp,"BITSET");

  obs.stringp:=obs.MakeDynArr(obs.charp,1);
  gen.dynarr(types,obs.stringp);
  type0(obs.stringp,"STRING");

  o:=const("NIL",0,obs.addrp);
  o^.gen^.dw^.md:=pc.nil;

----------------------------------------------------------------
  proc("ABS"  ,_abs);    proc("ODD"  ,_odd);
  proc("ORD"  ,_ord);    proc("SIZE" ,_size);
  proc("BYTES",_bytes);  proc("BITS" ,_bits);
  proc("FLOAT",_float);  proc("TRUNC",_trunc);
  proc("CAP"  ,_cap);    proc("CHR"  ,_chr);
  proc("MIN"  ,_min);    proc("MAX"  ,_max);
  proc("HIGH" ,_high);   proc("REF"  ,_ref);
  proc("LOW"  ,_low);    proc("LEN"  ,_len);
----------------------------------------------------------------
  proc("INC" ,_inc);     proc("DEC"   ,_dec);
  proc("INCL",_incl);    proc("EXCL"  ,_excl);
  proc("HALT",_halt);    proc("ASSERT",_assert);
END IniSTANDARD;

PROCEDURE compile(text : comp.io_ptr; opts : BITSET);
  CONST debug = { ORD('F')-ORD('A'), ORD('D')-ORD('A') };
BEGIN
  scan.opts:=opts;
  name[0]:=0c;
  code:=0;
  withs:=NIL;
  types:=NIL;
  mod_no:=1;
  proc_no:=1;
  scan.Ini(text);
  obs.Ini; IniSTANDARD; IniSTORAGE;
  sym.Ini;
  pc.get_name:=scan.id_str;
end?:=(debug*scan.opts={});
  compilationUnit;
end?:=TRUE;
  sym.Exi;
  obs.Exi;
  scan.Exi;
END compile;

PROCEDURE get_import(text  : comp.io_ptr; import: ONE_MODULE);
  VAR name: ARRAY [0..255] OF CHAR;
     from?: BOOLEAN; id: INTEGER;
BEGIN
  scan.opts:=opts;
  name[0]:=0c;
  code:=0;
  scan.Ini(text);

  IF    sy=scan.definition     THEN unit:=comp.def; GetSy
  ELSIF sy=scan.implementation THEN unit:=comp.imp; GetSy
  ELSE                              unit:=comp.main;
  END;
  IF sy#scan.module THEN scan.Fault(ers.ill_header,''); RETURN END;
  GetSy;
  IF sy#scan.ident  THEN scan.Fault(ers.ill_header,''); RETURN END;
  scan.id_str(scan.Id,name);
  GetSy;
  IF sy=scan.lbr THEN GetSy;
    IF sy=scan.intval THEN GetSy END;
    CheckGet(scan.rbr);
  END;
  CheckGet(scan.semic);
  WHILE (sy=scan.import) OR (sy=scan.from) DO
    from?:=(sy=scan.from);
    IF from? THEN GetSy;
      IF sy#scan.ident THEN scan.expc(scan.ident);
      ELSE scan.id_str(scan.Id,name); import(name); GetSy;
      END;
    END;
    CheckGet(scan.import);
    LOOP
      IF sy=scan.ident THEN id:=scan.Id; GetSy;
        IF   from? THEN
        ELSIF sy=scan.colon THEN GetSy;
          IF sy=scan.ident THEN scan.id_str(scan.Id,name); import(name) END;
          CheckGet(scan.ident);
        ELSE scan.id_str(id,name); import(name);
        END;
      ELSE CheckGet(scan.ident);
      END;
      IF sy#scan.coma THEN EXIT END;
      GetSy;
    END;
    CheckGet(scan.semic);
  END;
  scan.Exi;
  inter.release;
END get_import;

PROCEDURE fault(VAL format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  scan.Fault(ers.blank,format,args);
END fault;

PROCEDURE final;
  VAR s: ARRAY [0..63] OF CHAR;
BEGIN
  IF end? THEN RETURN END;
  inter.xprint('*****************************\n');
  inter.xprint('textpos=%d.%d    ',scan.line,scan.col);
  IF scan.sy=scan.ident THEN scan.id_str(scan.Id,s);
    inter.xprint('sy="%s"',s);
  ELSE scan.vis_sym(sy,s); inter.xprint('sy=%s [%d]',s,sy);
  END;
  inter.xprint('\n*****************************\n');
END final;

BEGIN
  vers:='Modula XP v1.1 /01-Mar-91/';
  gen_opts:={
         ORD('N')-ORD('A')
        ,ORD('T')-ORD('A')
        ,ORD('R')-ORD('A')
        ,ORD('I')-ORD('A')
        };
  opts:={
         ORD('T')-ORD('A')
        ,ORD('R')-ORD('A')
        ,ORD('I')-ORD('A')
        ,ORD('N')-ORD('A')
        };
  unit:=-1;
  code:=0;
  inter.final(final); end?:=TRUE;
END mcPars.
