IMPLEMENTATION MODULE pcO2; (* Ned 10-Nov-87. (c) KRONOS *)
                            (* Ned 11-Aug-90. (c) KRONOS *)

IMPORT  pcK, pcM, pcS, pcO, pcB;

TYPE SHORTINT = INTEGER;

CONST incomp = 30;

CONST
  r_check   = pcS.range_check;
  b_check   = pcS.index_check;
  nil_check = pcS.nil_check;

CONST (* conversion functions *)
  chr   = -1;   ord   = -2;
  long  = -3;   short = -4;
  entier= -5;   valfn = -6;

TYPE ITEM = pcB.ITEM;

VAR
  CU    : pcK.OBJECT;
  sy    : pcS.Symbol;
  ptrs  : pcK.STRUCT;
  headTD: pcK.NODE;
  tailTD: pcK.NODE;

(*---------------------------------------------------------------*)
PROCEDURE Expr(VAR e: ITEM); FORWARD;
PROCEDURE type(VAR t: pcK.STRUCT); FORWARD;
(*---------------------------------------------------------------*)

PROCEDURE check_get(S: pcS.Symbol);
BEGIN
  IF sy#S THEN pcS.expc(S) END;
  pcS.get(sy)
END check_get;

PROCEDURE check_id;
BEGIN
  IF sy#pcS.ident THEN pcS.err(7) END;
  pcS.get(sy)
END check_id;

(*----------------------------------------------------------------*)

PROCEDURE qualident(VAR v: pcK.OBJECT);
  VAR q: pcK.OBJECT;
BEGIN
  IF sy#pcS.ident THEN pcS.err(7); v:=pcO.inv_obj; RETURN END;
  pcO.vis(pcS.name,v); q:=v; pcS.get(sy);
  IF sy=pcS.period THEN
    IF q^.mode=pcK.module THEN
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.err(7)
      ELSE pcO.vis_in(pcS.name,q,v); pcS.get(sy);
      END;
    ELSIF q^.mode=pcK.inv THEN
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.err(7) ELSE pcS.get(sy) END;
    END;
  END;
END qualident;

PROCEDURE type_qua(VAR v: pcK.OBJECT);
BEGIN
  qualident(v);
  IF v^.mode#pcK.type THEN pcS.err(31) END;
END type_qua;

PROCEDURE designator(VAR i: ITEM);
  VAR v: pcK.OBJECT; ex: ITEM;
BEGIN
  IF sy#pcS.ident THEN pcS.err(7); Expr(i); RETURN END;
  qualident(v); pcB.set_item(v,i);
  LOOP
    IF sy=pcS.bar THEN
      IF i.mode=pcK.method THEN EXIT END;
      pcB.deref(i); pcS.get(sy);
    ELSIF sy=pcS.period THEN
      IF i.type^.mode=pcK.pointer THEN pcB.deref(i) END;
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.err(7); RETURN END;
      pcB.access(i);
      pcS.get(sy);
    ELSIF sy=pcS.lbr THEN
      IF i.type^.mode=pcK.pointer THEN pcB.deref(i) END;
      pcS.get(sy);
      LOOP
        Expr(ex); pcB.index(i,ex);
        IF sy=pcS.coma THEN pcS.get(sy) ELSE EXIT END;
      END;
      check_get(pcS.rbr)
    ELSIF (sy=pcS.lpar) & (i.mode=pcK.var) & (i.type^.mode#pcK.proctype) THEN
      pcS.get(sy);
      type_qua(v); pcB.type_test(i,v,TRUE);
      check_get(pcS.rpar)
    ELSE EXIT
    END;
  END;
END designator;

PROCEDURE var_designator(VAR i: ITEM);
BEGIN
  designator(i);
  IF    i.mode#pcK.var   THEN pcB.err(i,54)
  ELSIF pcK.RO IN i.tags THEN pcS.err(123)
  END;
END var_designator;

(*----------------------------------------------------------------*)

PROCEDURE boolexpr(VAR i: ITEM);
BEGIN
  Expr(i);
  IF i.type^.mode#pcK.boolean THEN pcB.err(i,incomp) END;
END boolexpr;

PROCEDURE sProcCall(n: pcK.NODE);
  VAR l,r: ITEM;
BEGIN
  IF n^.mode=pcK.halt THEN
    IF sy=pcS.lpar THEN
      pcS.get(sy);
      IF sy#pcS.rpar THEN
        Expr(l); n^.l:=l.node;
        pcB.type_in(l.type,pcK.WHOLEs);
      END;
      check_get(pcS.rpar);
    END;
  ELSE
    check_get(pcS.lpar);
    CASE n^.mode OF
      |pcK.inc,pcK.dec:
         var_designator(l); n^.l:=l.node;
         pcB.chkScalar(l.type);
         IF sy=pcS.coma THEN
           pcS.get(sy); Expr(r); pcB.type_in(r.type,pcK.WHOLEs); n^.r:=r.node;
         END;
      |pcK.incl,pcK.excl:
         IF r_check IN pcS.opts THEN n^.sub:=pcK.rcheck END;
         var_designator(l); n^.l:=l.node;
         check_get(pcS.coma); Expr(r); n^.r:=r.node;
         IF NOT (l.type^.mode IN pcK.SETs) THEN pcB.err(l,incomp)
         ELSE
           pcB.scalar_cmp(l.type^.base,r.type);
         END;
      |pcK.assert:
         boolexpr(l); n^.l:=l.node;
         IF sy=pcS.coma THEN
           pcS.get(sy); Expr(r); pcB.type_in(r.type,pcK.WHOLEs); n^.r:=r.node;
         END;
      |pcK.new:
         var_designator(l); n^.l:=l.node;
         IF l.type^.mode=pcK.dynarr  THEN
           IF sy=pcS.coma THEN
             pcS.get(sy); Expr(r); pcB.type_in(r.type,pcK.WHOLEs); n^.r:=r.node;
           END;
         ELSIF l.type^.mode#pcK.pointer THEN pcB.err(l,incomp)
         END;
      |pcK.resize:
         var_designator(l); n^.l:=l.node;
         IF l.type^.mode#pcK.dynarr THEN pcB.err(l,58) END;
         check_get(pcS.coma); Expr(r);
         pcB.type_in(r.type,pcK.WHOLEs); n^.r:=r.node;
      |pcK.origin:
         var_designator(l); n^.l:=l.node;
         IF l.type^.mode#pcK.dynarr THEN pcB.err(l,58) END;
         pcB.new(n^.r,pcK.node);
         check_get(pcS.coma); Expr(r);
         pcB.type_in(r.type,pcK.WHOLEs); n^.r^.l:=r.node;
         check_get(pcS.coma); Expr(r);
         pcB.type_in(r.type,pcK.WHOLEs); n^.r^.r:=r.node;
      |pcK.move:
         Expr(l); n^.l:=l.node;    pcB.type_in(l.type,pcK.WHOLEs);
         pcB.new(n^.r,pcK.node); check_get(pcS.coma);
         Expr(r); n^.r^.l:=r.node; pcB.type_in(r.type,pcK.WHOLEs);
         check_get(pcS.coma);
         Expr(r); n^.r^.r:=r.node; pcB.type_in(r.type,pcK.WHOLEs);
      |pcK.dcopy:
         Expr(l); n^.l:=l.node; check_get(pcS.coma);
         IF l.type^.mode#pcK.dynarr THEN pcB.err(l,58) END;
         var_designator(r); n^.r:=r.node;
         IF l.type#r.type THEN pcB.err(l,incomp) END;
      |pcK.copy:
         Expr(l); check_get(pcS.coma); var_designator(r);
         pcB.copy_fn(l,r);
         n^.l:=l.node; n^.r:=r.node;
    ELSE pcM.abort;
    END;
    check_get(pcS.rpar);
  END;
END sProcCall;

PROCEDURE parmlist(VAR ap: pcK.NODE; fp: pcK.OBJECT);

  CONST SEQs = {pcK.seq,pcK.varseq};
  VAR tail: pcK.NODE;

  PROCEDURE transport(node: pcK.NODE);
    VAR base: pcK.STRUCT;
  BEGIN
    base:=fp^.type^.base;
    IF base^.mode=pcK.proctype THEN
      pcB.proc_cmp(base,node^.type^.base,FALSE)
    ELSE
      pcB.type_cmp(base,node^.type^.base);
    END;
    IF node^.obj^.mode#fp^.mode THEN pcS.err(39) END;
    pcB.app(ap,tail,node);
  END transport;

  PROCEDURE sequence(empty: BOOLEAN);
    VAR vari: BOOLEAN; h,l: pcK.NODE; i: ITEM; base: pcK.STRUCT; no: LONGINT;
  BEGIN
    base:=fp^.type^.base;
    vari:=(fp^.mode=pcK.varseq);
    l:=NIL; h:=NIL; no:=0;
    IF NOT empty THEN
      LOOP
        IF sy=pcS.rpar THEN EXIT END;
        IF vari THEN var_designator(i) ELSE Expr(i) END;
        IF (l=NIL) & (i.node^.mode=pcK.var) & (i.node^.obj^.mode IN SEQs) THEN
          transport(i.node); check_get(pcS.rpar); RETURN
        END;
        INC(no);
        IF    base^.mode=pcK.word     THEN
        ELSIF base^.mode=pcK.proctype THEN
          pcB.proc_value(i); pcB.proc_cmp(base,i.type,FALSE)
        ELSIF vari THEN pcB.type_cmp(base,i.type)
        ELSE pcB.assign_cmp(base,i)
        END;
        IF l=NIL THEN h:=i.node ELSE l^.next:=i.node END;
        l:=i.node;
        IF sy=pcS.coma THEN pcS.get(sy) ELSE EXIT END;
      END;
      check_get(pcS.rpar);
    END;
    pcB.tie(ap,tail,pcK.sequence); tail^.l:=h; tail^.pos:=no;
  END sequence;

  PROCEDURE empty_list;
  BEGIN
    IF fp=NIL THEN RETURN END;
    IF fp^.mode IN SEQs THEN sequence(TRUE);
    ELSE pcS.err(38)
    END;
  END empty_list;

  CONST arrs = pcK.Forms{pcK.array,pcK.array_of,pcK.vector};
  VAR ok: BOOLEAN; i: ITEM;
BEGIN
  IF sy#pcS.lpar THEN empty_list; RETURN
  ELSE pcS.get(sy);
    IF sy=pcS.rpar THEN empty_list; pcS.get(sy); RETURN END;
  END;
  ok:=TRUE; tail:=NIL;
  LOOP
    IF ok & (fp=NIL) THEN pcS.err(38); ok:=FALSE END;
    IF ok THEN
      IF fp^.mode IN SEQs THEN sequence(FALSE); RETURN
      ELSE
        IF fp^.mode=pcK.varpar THEN var_designator(i) ELSE Expr(i) END;
        pcB.parm_cmp(fp,i);
      END;
      pcB.app(ap,tail,i.node);
      fp:=fp^.next;
    ELSE Expr(i);
    END;
    IF sy=pcS.coma THEN pcS.get(sy) ELSE EXIT END;
  END;
  IF ok THEN empty_list END;
  check_get(pcS.rpar);
END parmlist;

PROCEDURE skip_parms;
  VAR i: ITEM;
BEGIN
  IF sy#pcS.lpar THEN RETURN END;
  pcS.get(sy);
  LOOP
    IF sy=pcS.rpar THEN EXIT END;
    Expr(i);
    IF sy=pcS.coma THEN pcS.get(sy) ELSE EXIT END;
  END;
  pcS.get(sy);
END skip_parms;

PROCEDURE ProcCall(n: pcK.NODE; func,super: BOOLEAN);

  PROCEDURE method_call(n,proc: pcK.NODE; super: BOOLEAN);
    VAR parms,m: pcK.OBJECT; x: pcK.NODE;
  BEGIN
    parms:=proc^.type^.next;
    IF (parms^.type^.mode=pcK.pointer) & (proc^.l^.mode#pcK.deref) THEN
      pcS.err(67)
    END;
    parmlist(n^.r,parms^.next);
    m:=proc^.obj;
    IF super THEN
      IF (m^.type^.base=NIL) OR (m^.head#pcO.scope) THEN pcS.err(68)
      ELSIF NOT pcO.try_in_rec(m^.name,m^.type^.base,m) THEN pcS.err(68)
      ELSE
        proc^.mode:=pcK.proc; proc^.obj:=m^.head;
        x:=proc^.l;
        IF x^.mode=pcK.deref THEN x:=x^.l END;
        x^.next:=n^.r; n^.r:=x;
      END;
    ELSIF (proc^.l^.mode=pcK.var) & (proc^.l^.obj^.mode=pcK.var) THEN
      (* static call *)
      proc^.mode:=pcK.proc; proc^.obj:=m^.head;
      x:=proc^.l; x^.next:=n^.r; n^.r:=x;
    END;
  END method_call;

  VAR proc: pcK.NODE;
BEGIN
  proc:=n^.l;
  IF (proc^.mode=pcK.inv) OR (proc^.type^.mode#pcK.proctype) THEN
    IF proc^.type^.mode#pcK.invtype THEN pcS.err(55) END;
    skip_parms; RETURN
  END;
  IF proc^.type^.base#pcO.undef THEN
     IF func THEN n^.type:=proc^.type^.base ELSE pcS.err(90) END;
  ELSIF func THEN pcS.err(91); n^.type:=pcO.invtype;
  END;
  IF proc^.mode=pcK.method THEN method_call(n,proc,super);
  ELSE parmlist(n^.r,proc^.type^.next);
  END;
  IF proc^.mode=pcK.proc THEN n^.obj:=proc^.obj; pcB.dispose(n^.l) END;
END ProcCall;

(*----------------------------------------------------------------*)

PROCEDURE Expr(VAR e1: ITEM);

  PROCEDURE sFuncCall(VAR i: ITEM);
    VAR v: ITEM; op: SHORTINT;
  BEGIN
    op:=SHORTINT(i.node^.obj^.adr); pcB.dispose(i.node);
    check_get(pcS.lpar);
    CASE op OF
      |pcK.min,pcK.max:
         designator(i); pcB.min_max_fn(i,pcK.Sub(op));
      |pcK.adr:
         var_designator(i); pcB.adr_fn(i);
         i.type:=pcO.longint; i.node^.type:=i.type;
      |pcK.size,pcK.bytes,pcK.bits:
         designator(i); pcB.size_fn(i,pcK.Sub(op));
      |pcK.high,pcK.len:
         designator(i);
         IF sy=pcS.coma THEN
           pcS.get(sy); Expr(v);
           pcB.len_fn(i,v,FALSE,pcK.Sub(op));
         ELSE
           pcB.len_fn(i,v,TRUE ,pcK.Sub(op));
         END;
      |pcK.abs:
         Expr(i); pcB.type_in(i.type,pcK.NUMs);
         IF i.type^.mode=pcK.range THEN i.type:=i.type^.base END;
         pcB.unary(i,pcK.abs,i.type);
      |pcK.odd:
         Expr(i); pcB.type_in(i.type,pcK.WHOLEs); pcB.unary(i,pcK.odd,pcO.boolean);
      |ord:
         Expr(i);
         pcB.type_in(i.type,pcK.Forms{pcK.enum,pcK.char,pcK.boolean,pcK.byte});
         i.type:=pcO.shortint;
      |chr:
         Expr(i); pcB.chkScalar(i.type); pcB.unary(i,pcK.rcheck,pcO.char);
      |pcK.cap:
         Expr(i); pcB.type_equ(i.type,pcK.char); pcB.unary(i,pcK.cap,pcO.char);
      |entier:
         Expr(i); pcB.type_in(i.type,pcK.REALs); pcB.convert(i,pcO.longint);
      |long:
         Expr(i); pcB.long_fn(i);
      |short:
         Expr(i); pcB.short_fn(i);
      |valfn:
         designator(v);
         IF v.mode#pcK.type THEN pcB.err(v,31) END;
         IF v.type^.mode IN pcK.Forms{pcK.array_of,pcK.dynarr} THEN pcS.err(incomp) END;
         check_get(pcS.coma); Expr(i);
         IF i.type^.mode IN pcK.Forms{pcK.array_of,pcK.dynarr} THEN pcS.err(incomp) END;
         pcB.unary(i,pcK.typetran,v.type);
      |pcK.ash:
         Expr(i); check_get(pcS.coma); Expr(v);
         pcB.ash_fn(i,v);
    ELSE pcM.abort;
    END;
    check_get(pcS.rpar);
  END sFuncCall;

  PROCEDURE set(VAR i: ITEM; type: pcK.STRUCT);
    VAR a,b: ITEM; tail: pcK.NODE;
  BEGIN
    pcB.new(i.node,pcK.aggregate); i.node^.type:=type;
    IF r_check IN pcS.opts THEN i.node^.sub:=pcK.rcheck END;
    i.mode:=pcK.cons; i.type:=type; i.obj:=NIL;
    tail:=NIL;
    pcS.get(sy);
    IF sy#pcS.rbrace THEN
      LOOP
        Expr(a);
        IF a.mode#pcK.cons THEN i.mode:=pcB.expr END;
        pcB.scalar_cmp(type^.base,a.type);
        IF sy=pcS.range  THEN
          pcS.get(sy);
          Expr(b);
          IF b.mode#pcK.cons THEN i.mode:=pcB.expr END;
          pcB.scalar_cmp(type^.base,b.type);
          pcB.tie(i.node^.l,tail,pcK.node);
          tail^.l:=a.node;
          tail^.r:=b.node;
        ELSE
          pcB.app(i.node^.l,tail,a.node);
        END;
        IF sy=pcS.rbrace THEN pcS.get(sy); EXIT END;
        IF sy=pcS.coma   THEN pcS.get(sy)
        ELSE pcS.expc(pcS.rbrace);  EXIT
        END
      END;
    ELSE pcS.get(sy)
    END;
  END set;

  PROCEDURE expr_seq(VAR i: ITEM; VAR n: LONGINT; base: pcK.STRUCT);
    VAR ex: ITEM; tail: pcK.NODE;
  BEGIN
    IF sy#pcS.lbrace THEN pcS.expc(pcS.lbrace); RETURN END;
    pcS.get(sy);
    pcB.new(i.node,pcK.aggregate);
    i.mode:=pcK.cons; i.obj:=NIL; tail:=NIL; n:=0;
    LOOP
      Expr(ex);
      IF ex.mode#pcK.cons THEN pcS.err(87) END;
      pcB.assign_cmp(base,ex);
      pcB.app(i.node^.l,tail,ex.node);
      INC(n);
      IF sy=pcS.rbrace THEN pcS.get(sy); EXIT END;
      IF sy=pcS.coma   THEN pcS.get(sy)
      ELSE pcS.expc(pcS.rbrace);  EXIT
      END
    END;
  END expr_seq;

  PROCEDURE array(VAR i: ITEM; type: pcK.STRUCT);
    VAR n,l: LONGINT;
  BEGIN
    expr_seq(i,n,type^.base);
    i.type:=type;
    i.node^.type:=type;
    IF n#pcB.len(type) THEN pcS.err(incomp) END;
  END array;

  PROCEDURE open_array(VAR i: ITEM; base: pcK.STRUCT);
    VAR n: LONGINT;
  BEGIN
    expr_seq(i,n,base);
    pcO.new_type(i.type,pcK.vector);
    i.type^.base:=base;
    i.type^.n :=n;
    i.node^.type:=i.type;
  END open_array;

  PROCEDURE iden(VAR i: ITEM);

    PROCEDURE call(VAR i: ITEM; super: BOOLEAN);
      VAR n: pcK.NODE;
    BEGIN
      pcB.new(n,pcK.call); n^.l:=i.node; n^.type:=pcO.invtype; i.node:=n;
      ProcCall(n,TRUE,super);
      i.type:=n^.type;
    END call;

  BEGIN
    designator(i);
    IF sy=pcS.lpar THEN
      IF    i.mode=pcK.sfunc THEN sFuncCall(i);
      ELSIF i.mode=pcK.sproc THEN pcS.err(91); skip_parms;
      ELSIF i.mode=pcK.type  THEN pcS.err(121); skip_parms;
      ELSE call(i,FALSE);
      END;
    ELSIF sy=pcS.lbrace THEN
      IF    i.mode#pcK.type          THEN pcB.err(i,31)
      ELSIF i.type^.mode IN pcK.SETs THEN set(i,i.type);
      ELSIF i.type^.mode=pcK.array   THEN array(i,i.type);
      ELSE pcB.err(i,81);
      END;
    ELSIF sy=pcS.bar THEN
      pcS.get(sy);
      IF sy=pcS.lpar THEN call(i,TRUE) ELSE pcS.expc(pcS.lpar) END;
    ELSIF i.mode IN {pcK.sproc,pcK.sfunc,pcK.type,pcK.method} THEN pcB.err(i,121)
    END;
  END iden;

  PROCEDURE Factor(VAR i: ITEM);
    VAR v: pcK.OBJECT;
  BEGIN
    IF    sy=pcS.ident   THEN iden(i);
    ELSIF sy=pcS.literal THEN pcB.value(i); pcS.get(sy);
    ELSIF sy=pcS.lpar    THEN pcS.get(sy); Expr(i); check_get(pcS.rpar);
    ELSIF sy=pcS.lbrace  THEN set(i,pcO.bitset);
    ELSIF sy=pcS.not     THEN
      pcS.get(sy); Factor(i); pcB.type_equ(i.type,pcK.boolean);
      pcB.unary(i,pcK.not,pcO.boolean);
    ELSIF sy=pcS.nil     THEN
      i.mode:=pcK.cons; i.obj:=NIL; i.type:=pcO.niltype;
      pcB.new(i.node,pcK.value); i.node^.val:=pcM.nilval;
      i.node^.type:=i.type; pcS.get(sy);
    ELSIF sy=pcS.array   THEN
      pcS.get(sy); check_get(pcS.of); type_qua(v); open_array(i,v^.type);
    ELSE
      pcS.err(81);
      WHILE (sy#pcS.end) & (sy#pcS.rpar) & (sy#pcS.semic) DO pcS.get(sy) END;
      i.mode:=pcK.inv; i.type:=pcO.invtype;
      i.node:=NIL;    i.tags:={};       i.obj:=NIL;
    END;
    IF i.mode=pcK.cons THEN pcB.eval_expr(i) END;
  END Factor;

  PROCEDURE term(symbol: pcS.Symbol; VAR e1,e2: ITEM);
  BEGIN
    CASE symbol OF
      |pcS.slash:
         IF e1.type^.mode IN pcK.SETs THEN
           IF e1.type#e2.type THEN pcS.err(incomp) END;
           pcB.binary(e1,e2,pcK.xor,e1.type);
         ELSE
           pcB.numeric(e1,e2,pcK.NUMs);
           IF e1.type^.mode IN pcK.WHOLEs THEN
             pcB.convert(e1,pcO.real); pcB.convert(e2,pcO.real);
           END;
           pcB.binary(e1,e2,pcK.slash,e1.type);
         END;
      |pcS.times:
         IF e1.type^.mode IN pcK.SETs THEN
           IF e1.type#e2.type THEN pcS.err(incomp) END;
           pcB.binary(e1,e2,pcK.and,e1.type);
         ELSE
           pcB.numeric(e1,e2,pcK.NUMs);
           pcB.binary(e1,e2,pcK.mul,e1.type);
         END;
      |pcS.div:
         pcB.numeric(e1,e2,pcK.WHOLEs);
         pcB.type_in(e1.type,pcK.WHOLEs);
         pcB.binary(e1,e2,pcK.div,e1.type);
      |pcS.mod:
         pcB.numeric(e1,e2,pcK.WHOLEs);
         pcB.type_in(e1.type,pcK.WHOLEs);
         pcB.binary(e1,e2,pcK.mod,e1.type);
      |pcS.rem:
         pcB.numeric(e1,e2,pcK.WHOLEs);
         pcB.type_in(e1.type,pcK.WHOLEs);
         pcB.binary(e1,e2,pcK.mod,e1.type);
    ELSE pcM.abort;
    END;
  END term;

  PROCEDURE cand(VAR e1: ITEM);
    VAR e2: ITEM;
  BEGIN
    pcB.type_equ(e1.type,pcK.boolean);
    Factor(e2); pcB.type_equ(e2.type,pcK.boolean);
    IF sy=pcS.and THEN pcS.get(sy); cand(e2) END;
    pcB.binary(e1,e2,pcK.cand,pcO.boolean);
  END cand;

  PROCEDURE Term(VAR e1: ITEM);
    VAR e2: ITEM; op: pcS.Symbol;
  BEGIN
    Factor(e1);
    LOOP
      op:=sy;
      CASE op OF
        |pcS.times,pcS.div,pcS.mod,pcS.slash,pcS.rem:
                  pcS.get(sy); Factor(e2); term(op,e1,e2);
        |pcS.and: pcS.get(sy); cand(e1);
      ELSE RETURN
      END;
    END;
  END Term;

  PROCEDURE cor(VAR e1: ITEM);
    VAR e2: ITEM;
  BEGIN
    pcB.type_equ(e1.type,pcK.boolean);
    Term(e2); pcB.type_equ(e2.type,pcK.boolean);
    IF sy=pcS.or THEN pcS.get(sy); cor(e2) END;
    pcB.binary(e1,e2,pcK.cor,pcO.boolean);
  END cor;

  PROCEDURE Simple(VAR e1: ITEM);
    VAR e2: ITEM; op: pcS.Symbol; t: pcK.STRUCT;
  BEGIN
    IF    sy=pcS.plus  THEN pcS.get(sy); Term(e1);
      pcB.type_in(e1.type,pcK.NUMs+pcK.SETs);
    ELSIF sy=pcS.minus THEN pcS.get(sy); Term(e1);
      IF e1.type^.mode IN pcK.SETs THEN pcB.unary(e1,pcK.compl,e1.type);
      ELSE
        pcB.type_in(e1.type,pcK.NUMs);
        IF e1.type^.mode=pcK.range THEN e1.type:=e1.type^.base END;
        pcB.unary(e1,pcK.minus,e1.type);
      END;
    ELSE Term(e1)
    END;
    LOOP
      op:=sy;
      IF (op#pcS.plus) & (op#pcS.minus) & (op#pcS.or) THEN RETURN END;
      pcS.get(sy);
      IF op=pcS.or THEN cor(e1)
      ELSIF op=pcS.minus THEN
        Term(e2);
        IF e1.type^.mode IN pcK.SETs THEN
          IF e1.type#e2.type THEN pcS.err(incomp) END;
          pcB.binary(e1,e2,pcK.bic,e1.type);
        ELSE
          pcB.numeric(e1,e2,pcK.NUMs);
          pcB.binary(e1,e2,pcK.minus,e1.type);
        END;
      ELSIF op=pcS.plus  THEN
        Term(e2); t:=e1.type;
        IF t^.mode=pcK.range THEN t:=t^.base END;
        IF t^.mode IN pcK.NUMs THEN
          pcB.numeric(e1,e2,pcK.NUMs);
          pcB.binary(e1,e2,pcK.plus,e1.type);
        ELSIF t^.mode IN pcK.SETs+pcK.Forms{pcK.boolean} THEN
          IF t#e2.type THEN pcS.err(incomp) END;
          pcB.binary(e1,e2,pcK.or,e1.type);
        ELSE pcB.concat(e1,e2);
        END;
      END;
    END;
  END Simple;

  VAR e2: ITEM; op: pcK.Sub; m1: pcK.Form; v: pcK.OBJECT;
BEGIN
  Simple(e1);
  IF (sy>=pcS.equ) & (sy<=pcS.geq) THEN
    CASE sy OF
      |pcS.equ: op:=pcK.equ
      |pcS.neq: op:=pcK.neq
      |pcS.lss: op:=pcK.lss
      |pcS.gtr: op:=pcK.gtr
      |pcS.leq: op:=pcK.leq
      |pcS.geq: op:=pcK.geq
    END;
    pcS.get(sy); Simple(e2);
    pcB.relation(e1,e2,op);
  ELSIF sy=pcS.in THEN
    pcS.get(sy);
    Simple(e2);
    IF NOT (e2.type^.mode IN pcK.SETs) THEN pcS.err(incomp)
    ELSE pcB.scalar_cmp(e2.type^.base,e1.type);
    END;
    pcB.binary(e1,e2,pcK.in,pcO.boolean);
  ELSIF sy=pcS.is THEN
    pcS.get(sy);
    type_qua(v); pcB.type_test(e1,v,FALSE)
  END;
END Expr;

(*----------------------------------------------------------------*)

PROCEDURE StatSeq(VAR stat: pcK.NODE; loop: pcK.NODE);

  PROCEDURE with(node: pcK.NODE);
    VAR v,t: pcK.OBJECT; type: pcK.STRUCT; i: ITEM;
  BEGIN
    pcS.get(sy);
    qualident(v); type:=v^.type;
    check_get(pcS.colon); type_qua(t);
    pcB.set_item(v,i); pcB.type_test(i,t,TRUE);
    v^.type:=t^.type; node^.l:=i.node;
    check_get(pcS.do); StatSeq(node^.r,loop); check_get(pcS.end);
    v^.type:=type;
  END with;

  PROCEDURE for(node: pcK.NODE);
    VAR r: pcK.NODE; v: pcK.OBJECT; i: ITEM;
  BEGIN
    pcS.get(sy);
    IF sy#pcS.ident THEN pcS.err(7); v:=pcO.inv_obj;
    ELSE
      pcO.vis(pcS.name,v);
      IF v^.mode#pcK.inv THEN
        IF NOT (v^.mode IN pcK.VARs) THEN pcS.err(54)
        ELSIF (v^.mode#pcK.var) OR (v^.scope#pcO.level) THEN pcS.err(128)
        ELSIF pcK.RO IN v^.tags  THEN pcS.err(123)
        END;
      END;
      INCL(v^.tags,pcK.RO);
      pcB.chkScalar(v^.type);
      node^.obj:=v;
    END;
    pcS.get(sy);
    check_get(pcS.becomes);
    pcB.new(r,pcK.node); node^.l:=r;
    Expr(i); pcB.assign_cmp(v^.type,i); r^.l:=i.node;
    check_get(pcS.to);
    Expr(i); pcB.assign_cmp(v^.type,i); r^.r:=i.node;
    IF sy=pcS.by THEN
      pcS.get(sy);
      Expr(i); pcB.const_val(i,r^.pos);
      pcB.type_in(i.type,pcK.WHOLEs);
    ELSE r^.pos:=1;
    END;
    check_get(pcS.do);
    StatSeq(node^.r,loop);
    check_get(pcS.end);
    EXCL(v^.tags,pcK.RO);
  END for;

  PROCEDURE case(node: pcK.NODE);

    PROCEDURE Variant(type: pcK.STRUCT; node: pcK.NODE; VAR min,max: LONGINT);
      VAR e: ITEM; tail: pcK.NODE;
    BEGIN
      tail:=NIL;
      LOOP
        pcB.tie(node^.l,tail,pcK.pair);
        Expr(e); pcB.const_val(e,tail^.a);
        pcB.scalar_cmp(type,e.type); pcB.check_range(type,e);
        tail^.b:=tail^.a;
        IF sy=pcS.range THEN
          pcS.get(sy);
          Expr(e); pcB.const_val(e,tail^.b);
          pcB.scalar_cmp(type,e.type); pcB.check_range(type,e);
          IF tail^.a>tail^.b THEN pcS.err(124) END;
        END;
        IF tail^.a<min THEN min:=tail^.a END;
        IF tail^.b>max THEN max:=tail^.b END;
        IF sy=pcS.coma THEN pcS.get(sy)
        ELSE
          IF sy#pcS.colon THEN pcS.expc(pcS.colon) END; EXIT
        END;
      END;
      pcS.get(sy);
      StatSeq(node^.r,loop);
      IF (sy#pcS.else) & (sy#pcS.end) THEN check_get(pcS.sep) END;
    END Variant;

    VAR tail,else: pcK.NODE; i: ITEM; min,max: LONGINT;
  BEGIN
    pcS.get(sy);
    Expr(i); node^.l:=i.node;
    pcB.chkScalar(i.type); check_get(pcS.of);
    pcB.new(else,pcK.casedo); node^.r:=else;
    tail:=NIL; min:=MAX(LONGINT); max:=MIN(LONGINT);
    WHILE (sy#pcS.end) & (sy#pcS.else) DO
      IF sy=pcS.sep THEN pcS.get(sy)
      ELSE
        pcB.tie(else^.l,tail,pcK.casedo);
        Variant(i.type,tail,min,max);
      END;
    END;
    IF min=MAX(LONGINT) THEN pcS.err(127) END;
    IF sy=pcS.else THEN
      else^.mode:=pcK.caselse;
      pcS.get(sy); StatSeq(else^.r,loop)
    END;
    pcB.new(tail,pcK.pair); tail^.a:=min; tail^.b:=max;
    else^.next:=tail;
    check_get(pcS.end);
  END case;

  PROCEDURE guard(VAR x: pcK.NODE);
    VAR z,n: pcK.NODE;
  BEGIN
    IF x^.mode=pcK.guard THEN z:=x^.l ELSE z:=x END;
    IF (z^.mode=pcK.deref) & (z^.l^.mode=pcK.guard) THEN
      z^.l:=z^.l^.l (* skip guard before deferencing *)
    END;
    IF (z^.mode=pcK.deref) OR (z^.mode=pcK.varpar) THEN
      pcB.new(n,pcK.eguard); n^.type:=x^.type; n^.l:=z;
      n^.obj:=x^.type^.obj; x:=n;
    END;
  END guard;

  VAR tail,n: pcK.NODE; i: ITEM; type: pcK.STRUCT;
BEGIN
  tail:=NIL;
  LOOP
    CASE sy OF
     |pcS.ident :
        designator(i);
        IF sy=pcS.becomes THEN
          pcB.tie(stat,tail,pcK.assign);
          IF i.type^.mode=pcK.record THEN guard(i.node) END;
          IF i.node^.mode=pcK.var THEN
            tail^.obj:=i.node^.obj; pcB.dispose(i.node);
          END;
          tail^.l:=i.node; type:=i.type;
          IF    i.mode#pcK.var   THEN pcB.err(i,54)
          ELSIF pcK.RO IN i.tags THEN pcS.err(123)
          END;
          pcS.get(sy);
          Expr(i); pcB.assign_cmp(type,i);
          tail^.r:=i.node;
          tail^.pos:=pcS.txtpos;
        ELSIF i.mode=pcK.sproc THEN
          n:=i.node;
          n^.mode:=pcK.Mode(n^.obj^.adr);
          pcB.app(stat,tail,n);
          sProcCall(n);
          n^.pos:=pcS.txtpos;
        ELSIF i.mode=pcK.sfunc THEN pcS.err(90);  skip_parms;
        ELSIF i.mode=pcK.type  THEN pcS.err(121); skip_parms;
        ELSE
          pcB.tie(stat,tail,pcK.call); tail^.l:=i.node;
          IF sy#pcS.bar THEN ProcCall(tail,FALSE,FALSE);
          ELSE pcS.get(sy);  ProcCall(tail,FALSE,TRUE);
          END;
          tail^.pos:=pcS.txtpos;
        END;
     |pcS.if    :
        pcS.get(sy);
        pcB.tie(stat,tail,pcK.ifelse);
        pcB.new(n,pcK.if); tail^.l:=n;
        LOOP
          boolexpr(i); n^.l:=i.node; check_get(pcS.then);
          StatSeq(n^.r,loop);
          IF sy#pcS.elsif THEN EXIT END;
          pcS.get(sy);
          pcB.new(n^.next,pcK.if);
          n:=n^.next;
        END;
        IF sy=pcS.else THEN pcS.get(sy); StatSeq(tail^.r,loop) END;
        tail^.pos:=pcS.txtpos;
        check_get(pcS.end);
     |pcS.while :
        pcB.tie(stat,tail,pcK.while);
        pcS.get(sy); boolexpr(i); tail^.l:=i.node;
        check_get(pcS.do); StatSeq(tail^.r,loop);
        tail^.pos:=pcS.txtpos;
        check_get(pcS.end);
     |pcS.repeat:
        pcB.tie(stat,tail,pcK.repeat);
        pcS.get(sy); StatSeq(tail^.l,loop);
        check_get(pcS.until); boolexpr(i); tail^.r:=i.node;
        tail^.pos:=pcS.txtpos;
     |pcS.loop  :
        pcB.tie(stat,tail,pcK.loop);
        pcS.get(sy); StatSeq(tail^.r,tail);
        tail^.pos:=pcS.txtpos;
        check_get(pcS.end);
     |pcS.exit  :
        IF loop=NIL THEN pcS.err(125) END;
        pcB.tie(stat,tail,pcK.exit); tail^.r:=loop;
        tail^.pos:=pcS.txtpos;
        pcS.get(sy);
     |pcS.return:
        pcB.tie(stat,tail,pcK.return);
        pcS.get(sy);
        type:=pcO.scope^.type^.base;
        IF type#pcO.undef THEN
          Expr(i); pcB.assign_cmp(type,i); tail^.l:=i.node
        END;
        tail^.pos:=pcS.txtpos;
     |pcS.for   :
        pcB.tie(stat,tail,pcK.for); for(tail);
        tail^.pos:=pcS.txtpos;
     |pcS.case  :
        pcB.tie(stat,tail,pcK.case); case(tail);
        tail^.pos:=pcS.txtpos;
     |pcS.with  :
        pcB.tie(stat,tail,pcK.wguard); with(tail);
        tail^.pos:=pcS.txtpos;
     |pcS.semic :
     |pcS.end,pcS.else,pcS.elsif,pcS.until,pcS.sep: EXIT
    ELSE
      pcS.err(86);
      LOOP
        CASE sy OF
          |pcS.semic,pcS.end,pcS.else,pcS.elsif,pcS.until,pcS.sep: EXIT
        ELSE END;
        pcS.get(sy);
      END;
    END;
    CASE sy OF
     |pcS.end,pcS.else,pcS.elsif,pcS.until,pcS.sep: EXIT
    ELSE check_get(pcS.semic);
    END;
  END;
END StatSeq;

(*----------------------------------------------------------------*)

PROCEDURE export_mark(o: pcK.OBJECT);
BEGIN
  pcS.get(sy);
  IF sy=pcS.times THEN
    IF pcO.level=0 THEN INCL(o^.tags,pcK.exported) ELSE pcS.err(96) END;
    pcS.get(sy);
  END;
END export_mark;

PROCEDURE FormalType(VAR t: pcK.STRUCT);
  VAR o: pcK.OBJECT; dim,i: LONGINT; a: pcK.STRUCT;
BEGIN
  dim:=0;
  WHILE sy=pcS.array DO
    pcS.get(sy); check_get(pcS.of); INC(dim);
  END;
  type_qua(o); t:=o^.type;
  i:=1;
  WHILE i<=dim DO
    pcO.new_type(a,pcK.array_of); a^.base:=t; a^.n:=i;
    t:=a;
    INC(i);
  END;
END FormalType;

PROCEDURE proctype(VAR type: pcK.STRUCT);
  VAR mode: SHORTINT; tags: BITSET; tail,l,p,o: pcK.OBJECT;
      parm: pcK.STRUCT;
BEGIN
  pcO.new_type(type,pcK.proctype); type^.base:=pcO.undef;
  IF sy#pcS.lpar THEN RETURN END;
  pcS.get(sy);
  tail:=NIL;
  LOOP
    mode:=pcK.var; tags:={};
    IF    sy=pcS.var   THEN pcS.get(sy); mode:=pcK.varpar;
    ELSIF sy#pcS.ident THEN EXIT
    END;
    l:=NIL;
    LOOP
      IF sy=pcS.ident THEN
        pcO.new_obj(o,mode); pcM.str_copy(o^.name,pcS.name);
        IF l=NIL THEN
          IF tail=NIL THEN type^.next:=o ELSE tail^.next:=o END; l:=o;
        ELSE tail^.next:=o;
        END;
        tail:=o;
      END;
      check_id;
      IF    sy=pcS.coma  THEN pcS.get(sy)
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.coma)
      ELSE EXIT
      END
    END;
    check_get(pcS.colon);
    FormalType(parm);
    IF (mode=pcK.var) & (parm^.mode=pcK.dynarr) THEN tags:={pcK.RO} END;
    WHILE l#NIL DO l^.type:=parm; l^.tags:=tags; l:=l^.next END;
    IF sy#pcS.rpar THEN check_get(pcS.semic) END
  END;
  IF sy=pcS.seq THEN
    pcS.get(sy); tags:={};
    IF sy=pcS.var THEN pcS.get(sy); mode:=pcK.varseq;
    ELSE tags:={pcK.RO}; mode:=pcK.seq;
    END;
    IF sy#pcS.ident THEN pcS.err(7)
    ELSE
      pcO.new_obj(o,mode); pcM.str_copy(o^.name,pcS.name); o^.tags:=tags;
      pcS.get(sy); check_get(pcS.colon);
      type_qua(p);
      pcO.new_type(o^.type,pcK.array_of);
      o^.type^.base:=p^.type; o^.type^.n:=1;
      IF tail=NIL THEN type^.next:=o ELSE tail^.next:=o END;
    END;
  END;
  check_get(pcS.rpar);
  IF sy=pcS.colon THEN
    pcS.get(sy); type_qua(o); type^.base:=o^.type;
  END;
END proctype;

PROCEDURE type_dcl(VAR t: pcK.STRUCT);

  PROCEDURE enum(VAR t: pcK.STRUCT);
    VAR o: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    pcO.new_type(t,pcK.enum);
    t^.n:=0; t^.m:=-1;
    IF sy=pcS.rpar THEN pcS.err(7); pcS.get(sy); RETURN END;
    LOOP
      IF sy=pcS.ident THEN
        pcO.new_obj(o,pcK.cons); pcM.str_copy(o^.name,pcS.name);
        o^.type:=t;
        INC(t^.m); o^.adr:=t^.m;
        o^.next:=t^.next; t^.next:=o;
        pcO.dcl(o);
      END;
      check_id;
      IF    sy=pcS.coma  THEN pcS.get(sy)
      ELSIF sy=pcS.rpar  THEN EXIT
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.coma)
      ELSE EXIT END
    END;
    check_get(pcS.rpar);
  END enum;

  PROCEDURE pointer(VAR t: pcK.STRUCT);
    VAR o: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    check_get(pcS.to);
    pcO.new_type(t,pcK.pointer);
    IF (sy=pcS.ident) & NOT pcO.try(pcS.name,o) THEN
      pcO.new_obj(t^.next,pcK.inv); pcM.str_copy(t^.next^.name,pcS.name);
      t^.base:=pcO.undef;
      t^.inx:=ptrs; ptrs:=t;
      pcS.get(sy);
    ELSE
      type(t^.base);
      IF NOT (t^.base^.mode IN pcK.Forms{pcK.record,pcK.array,pcK.vector}) THEN
        (*dynarr?*)
        pcS.err(60);
      END;
    END;
  END pointer;

  PROCEDURE record(VAR rec: pcK.STRUCT);

    PROCEDURE fields(VAR head,tail: pcK.OBJECT);
      VAR t: pcK.STRUCT; l,o: pcK.OBJECT;
    BEGIN
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          pcO.new_obj(o,pcK.field); pcM.str_copy(o^.name,pcS.name);
          pcO.dcl_in_rec(rec,o);
          export_mark(o);
          IF l=NIL THEN
            IF tail=NIL THEN head:=o ELSE tail^.next:=o END; l:=o;
          ELSE tail^.next:=o;
          END;
          tail:=o;
        ELSE check_id;
        END;
        IF    sy=pcS.coma  THEN pcS.get(sy)
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.coma)
        ELSE EXIT
        END
      END;
      check_get(pcS.colon);
      type(t);
      WHILE l#NIL DO l^.type:=t; l:=l^.next END;
    END fields;

    VAR v,tail: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    pcO.new_type(rec,pcK.record);
    IF sy=pcS.lpar THEN
      pcS.get(sy);
      type_qua(v);
      IF v^.type^.mode#pcK.record THEN pcS.err(51)
      ELSE rec^.base:=v^.type; rec^.n:=v^.type^.n+1;
      END;
      check_get(pcS.rpar);
    END;
    tail:=NIL;
    LOOP
      IF    sy=pcS.ident THEN fields(rec^.next,tail)
      ELSIF sy=pcS.end THEN EXIT
      ELSIF sy#pcS.semic THEN
        pcS.expc(pcS.end);
        WHILE sy#pcS.end DO pcS.get(sy) END;
        pcS.get(sy); EXIT
      END;
      IF (sy#pcS.end) & (sy#pcS.sep) & (sy#pcS.else) THEN
        check_get(pcS.semic)
      END;
    END;
    check_get(pcS.end);
    pcB.tie(headTD,tailTD,pcK.inittd); tailTD^.type:=rec;
  END record;

  PROCEDURE array(VAR t: pcK.STRUCT);
    VAR i: ITEM;
  BEGIN
    pcS.get(sy);
    pcO.new_type(t,pcK.vector);
    Expr(i); pcB.const_val(i,t^.n);
    IF NOT (i.type^.mode IN pcK.WHOLEs) THEN pcS.err(incomp) END;
    IF sy=pcS.coma THEN sy:=pcS.array ELSE check_get(pcS.of) END;
    type(t^.base);
    pcB.tie(headTD,tailTD,pcK.inittd); tailTD^.type:=t;
  END array;

  VAR o: pcK.OBJECT;
BEGIN
  IF sy=pcS.ident THEN
     type_qua(o); t:=o^.type;
     IF t=pcO.undef THEN pcS.err(21) END;
  ELSIF sy=pcS.pointer   THEN pointer(t)
  ELSIF sy=pcS.record    THEN record(t);
  ELSIF sy=pcS.array     THEN array(t);
  ELSIF sy=pcS.procedure THEN pcS.get(sy); proctype(t)
  ELSIF sy=pcS.lpar      THEN enum(t)
  ELSIF sy=pcS.dynarr    THEN
    pcS.get(sy); check_get(pcS.of);
    pcO.new_type(t,pcK.dynarr); t^.n:=1; type(t^.base);
  ELSE pcS.err(83); t:=pcO.invtype;
  END;
END type_dcl;

PROCEDURE type(VAR t: pcK.STRUCT);
BEGIN
  type_dcl(t);
  IF (t^.mode=pcK.pointer) & (t^.base=pcO.undef) & (t^.obj=NIL) THEN
    pcS.err_id(20,t^.next^.name);
    t^.mode:=pcK.invtype;
  END;
END type;

(*----------------------------------------------------------------*)

PROCEDURE Block(enter: pcK.NODE);

  PROCEDURE ConstDcl;
    VAR o: pcK.OBJECT; e: ITEM;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      pcO.new_obj(o,pcK.cons); pcM.str_copy(o^.name,pcS.name); export_mark(o);
      IF sy#pcS.equ THEN pcS.expc(pcS.equ); RETURN END;
      pcS.get(sy);
      Expr(e);
      IF e.mode=pcK.cons THEN pcB.eval(e.node,o^.adr,o^.ext)
      ELSE pcB.err(e,87)
      END;
      pcB.dispose(e.node);
      o^.type:=e.type;
      IF e.obj#NIL THEN o^.scope:=e.obj^.scope ELSE o^.scope:=0 END;
      pcO.dcl(o);
      IF    sy=pcS.semic THEN pcS.get(sy)
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.semic)
      ELSE check_get(pcS.semic); RETURN
      END;
    END;
  END ConstDcl;

  PROCEDURE VarDcl(VAR head,tail: pcK.OBJECT);
    VAR l,o: pcK.OBJECT; t: pcK.STRUCT;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          pcO.new_obj(o,pcK.var); pcM.str_copy(o^.name,pcS.name); pcO.dcl(o);
          export_mark(o);
          IF l=NIL THEN
            IF tail=NIL THEN head:=o ELSE tail^.next:=o END; l:=o;
          ELSE tail^.next:=o;
          END;
          tail:=o;
        ELSE check_id;
        END;
        IF    sy=pcS.coma  THEN pcS.get(sy)
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.coma)
        ELSE EXIT
        END
      END;
      check_get(pcS.colon);
      type(t);
      WHILE l#NIL DO l^.type:=t; l:=l^.next END;
      check_get(pcS.semic);
    END;
  END VarDcl;

  PROCEDURE check_ptr(o: pcK.OBJECT);
    VAR l,p: pcK.STRUCT;
  BEGIN
    l:=ptrs; p:=NIL;
    WHILE (l#NIL) & NOT pcM.str_equ(l^.next^.name,o^.name) DO
      p:=l; l:=l^.inx
    END;
    IF l#NIL THEN
      l^.base:=o^.type;
      IF p=NIL THEN ptrs:=l^.inx ELSE p^.inx:=l^.inx END;
    END;
  END check_ptr;

  PROCEDURE TypeDcl;
    VAR o: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      pcO.new_obj(o,pcK.type); pcM.str_copy(o^.name,pcS.name); export_mark(o);
      check_get(pcS.equ);
      o^.type:=pcO.undef; pcO.dcl(o);
      type_dcl(o^.type);
      IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
      check_ptr(o);
      check_get(pcS.semic);
    END;
  END TypeDcl;

  PROCEDURE CodeProc(p: pcK.OBJECT);
    VAR n,tail: pcK.NODE; ex: ITEM;
  BEGIN
    p^.mode:=pcK.cproc;
    pcB.new(n,pcK.aggregate); n^.type:=p^.type;
    tail:=NIL;
    LOOP
      Expr(ex);
      IF ex.mode#pcK.cons THEN pcS.err(87) END;
      pcB.app(n^.l,tail,ex.node);
      IF sy=pcS.end  THEN pcS.get(sy); EXIT END;
      IF sy=pcS.coma THEN pcS.get(sy) END;
    END;
    pcB.eval(n,p^.adr,p^.ext);
    IF sy#pcS.ident THEN pcS.err(7)
    ELSE
      IF NOT pcM.str_equ(pcS.name,p^.name) THEN pcS.err_id(88,p^.name) END;
      pcS.get(sy);
    END;
  END CodeProc;

  PROCEDURE procbody(p: pcK.OBJECT; VAR x: pcK.NODE);
    VAR l: pcK.OBJECT;
  BEGIN
    EXCL(p^.tags,pcK.forward);
    l:=p^.type^.next;
    WHILE l#NIL DO pcO.dcl(l); l:=l^.next END;
    pcB.new(x,pcK.proc); x^.obj:=p;
    Block(x);
    check_get(pcS.semic);
  END procbody;

  PROCEDURE method(VAR x: pcK.NODE);
    VAR mode: pcK.Mode; class,type,super: pcK.STRUCT;
       proc,m,v,self: pcK.OBJECT; redef,fwd,pre: BOOLEAN;
  BEGIN
    x:=NIL;
    IF pcO.level#0 THEN pcS.err(95) END;
    redef:=(sy=pcS.slash);
    IF redef THEN pcS.get(sy) END;
    check_get(pcS.lpar);
    IF sy=pcS.var THEN mode:=pcK.varpar; pcS.get(sy) ELSE mode:=pcK.var END;
    IF sy=pcS.ident THEN
      pcO.new_obj(self,mode); pcM.str_copy(self^.name,pcS.name); pcS.get(sy);
      check_get(pcS.colon);
      type_qua(v);
      self^.type:=v^.type; self^.scope:=1; class:=pcO.invtype;
      IF mode=pcK.varpar THEN
        IF v^.type^.mode=pcK.record THEN class:=v^.type ELSE pcS.err(51) END;
      ELSIF v^.type^.mode#pcK.pointer THEN pcS.err(66)
      ELSE class:=v^.type^.base;
        IF class^.mode#pcK.record THEN pcS.err(62); class:=pcO.invtype END;
      END;
      IF class^.mno#0 THEN pcS.err(65) END;
    ELSE pcS.err(7);
      WHILE (sy#pcS.end) & (sy#pcS.rpar) DO pcS.get(sy) END;
    END;
    check_get(pcS.rpar);
    fwd:=(sy=pcS.bar);
    IF fwd THEN pcS.get(sy) END;
    IF sy=pcS.ident THEN
      pre:=NOT fwd & (class^.mode=pcK.record) & pcO.try_in_rec(pcS.name,class,v);
      IF pre THEN
        IF v^.mode#pcK.method THEN pcS.err(63); pre:=FALSE
        ELSE pre:=redef OR (pcK.forward IN v^.tags);
        END;
      ELSIF redef THEN pcS.err(64); redef:=FALSE;
      END;
      IF redef OR NOT pre THEN
        pcO.new_obj(m,pcK.method); pcM.str_copy(m^.name,pcS.name);
        IF class^.mode=pcK.record THEN pcO.dcl_in_rec(class,m) END;
        IF redef THEN INCL(m^.tags,pcK.redefine) END;
        m^.type:=class;
        pcO.new_obj(proc,pcK.xproc); pcM.str_copy(proc^.name,m^.name);
        m^.head:=proc;
      ELSE m:=v; proc:=m^.head;
      END;
    ELSE pcS.err(7); RETURN
    END;
    export_mark(m);
    pcO.enter_scope(proc);
      proctype(type); self^.next:=type^.next; type^.next:=self;
      IF pre THEN
        IF redef THEN
          super:=v^.head^.type;
          IF super^.next^.type^.mode#self^.type^.mode THEN pcS.err(69) END;
          pcB.cmp_params(super^.next^.next,self^.next,FALSE);
          IF super^.base#type^.base THEN pcS.err(44) END;
          proc^.type:=type;
        ELSE pcB.proc_cmp(proc^.type,type,TRUE)
        END;
      ELSE proc^.type:=type;
      END;
      check_get(pcS.semic);
      IF fwd THEN INCL(proc^.tags,pcK.forward) ELSE procbody(proc,x) END;
    pcO.exit_scope;
  END method;

  PROCEDURE procedure(VAR x: pcK.NODE);
    CONST FWD = {pcK.xproc,pcK.proc};
    VAR p: pcK.OBJECT; type: pcK.STRUCT; pre,fwd: BOOLEAN;
       mode: SHORTINT;
  BEGIN
    fwd:=FALSE; mode:=pcK.proc;
    IF sy#pcS.ident THEN
      IF    sy=pcS.bar   THEN pcS.get(sy); fwd:=TRUE;
      ELSIF sy=pcS.plus  THEN pcS.get(sy); mode:=pcK.iproc;
        IF pcO.level>0 THEN pcS.err(95) END;
      ELSIF sy=pcS.minus THEN pcS.get(sy); mode:=pcK.cproc;
      ELSIF sy=pcS.times THEN pcS.get(sy);
      END;
    END;
    IF sy#pcS.ident THEN pcS.err(7); RETURN END;
    pre:=NOT fwd & pcO.try_in(pcS.name,pcO.scope,p)
                 & (p^.mode IN FWD) & (pcK.forward IN p^.tags);
    IF NOT pre THEN
      pcO.new_obj(p,mode); pcM.str_copy(p^.name,pcS.name); pcO.dcl(p)
    END;
    export_mark(p);
    IF pcK.exported IN p^.tags THEN p^.mode:=pcK.xproc END;
    pcO.enter_scope(p);
      proctype(type);
      IF pre THEN pcB.proc_cmp(p^.type,type,TRUE); pcO.rem_type(type);
      ELSE p^.type:=type
      END;
      check_get(pcS.semic);
      IF fwd THEN INCL(p^.tags,pcK.forward);
      ELSIF mode=pcK.cproc THEN
        check_get(pcS.code); CodeProc(p); check_get(pcS.semic);
      ELSE procbody(p,x);
      END;
    pcO.exit_scope;
  END procedure;

  VAR objects: pcK.OBJECT;

  PROCEDURE traverse(o: pcK.OBJECT; exp: BOOLEAN);
    VAR x: pcK.OBJECT;
  BEGIN
    IF o=NIL THEN RETURN END;
    traverse(o^.l,exp);
    IF (pcK.exported IN o^.tags)=exp THEN
      IF o^.mode=pcK.method THEN
        o^.next:=o^.type^.link; o^.type^.link:=o; x:=o^.head;
      ELSE x:=o
      END;
      IF pcK.forward IN x^.tags THEN
        pcS.err_id(89,x^.name); EXCL(x^.tags,pcK.forward);
      END;
      IF x^.mode IN pcK.PROCs+{pcK.cons,pcK.type} THEN
        x^.next:=objects; objects:=x;
      END;
    END;
    traverse(o^.r,exp);
  END traverse;

  PROCEDURE inverse(VAR l: pcK.OBJECT);
    VAR a,b,c: pcK.OBJECT;
  BEGIN
    b:=l; a:=NIL;
    WHILE b#NIL DO
      c:=b^.next; b^.next:=a; a:=b; b:=c;
    END;
    l:=a;
  END inverse;

  PROCEDURE append_methods(export: BOOLEAN);
    VAR l: pcK.NODE;
  BEGIN
    l:=headTD;
    WHILE l#NIL DO
      IF l^.type^.mode=pcK.record THEN
        traverse(l^.type^.locs,export);
        IF NOT export THEN inverse(l^.type^.link) END;
      END;
      l:=l^.next;
    END;
  END append_methods;

  VAR proc,x: pcK.NODE; save: pcK.STRUCT; vars,tail,header: pcK.OBJECT;
BEGIN
  save:=ptrs; ptrs:=NIL;
  proc:=NIL;  vars:=NIL; tail:=NIL;
  LOOP
    IF    sy=pcS.var       THEN VarDcl(vars,tail);
    ELSIF sy=pcS.const     THEN ConstDcl;
    ELSIF sy=pcS.type      THEN TypeDcl;
    ELSIF sy=pcS.procedure THEN
      pcS.get(sy); x:=NIL;
      IF (sy=pcS.lpar) OR (sy=pcS.slash) THEN method(x)
      ELSE procedure(x)
      END;
      IF x#NIL THEN
        IF proc=NIL THEN enter^.l:=x ELSE proc^.next:=x END;
        proc:=x;
      END;
    ELSIF (sy=pcS.begin) OR (sy=pcS.end) THEN  EXIT
    ELSE pcS.err(82); pcS.get(sy)
    END;
  END;
  WHILE ptrs#NIL DO pcS.err_id(97,ptrs^.next^.name); ptrs:=ptrs^.inx END;
  ptrs:=save;
  header:=enter^.obj^.head; objects:=NIL;
  IF pcO.level=0 THEN
    traverse(header^.r,TRUE);
    append_methods(TRUE);
    append_methods(FALSE);
  END;
  traverse(header^.r,FALSE);
  tail:=objects;
  inverse(objects);
  IF tail=NIL THEN objects:=vars ELSE tail^.next:=vars END;
  header^.next:=objects;
  IF (sy=pcS.begin) THEN
    pcS.get(sy);
    IF sy#pcS.end THEN StatSeq(enter^.r,NIL) END;
  END;
  check_get(pcS.end);
  IF sy#pcS.ident THEN pcS.err(7)
  ELSE
    IF NOT pcM.str_equ(pcS.name,pcO.scope^.name) THEN pcS.err(88) END;
    pcS.get(sy);
  END;
END Block;

(*----------------------------------------------------------------*)

PROCEDURE import;
  VAR o: pcK.OBJECT;
BEGIN
  WHILE sy=pcS.import DO
    pcS.get(sy);
    LOOP
      IF sy=pcS.ident THEN
        pcO.new_obj(o,pcK.module); pcM.str_copy(o^.name,pcS.name);
        pcS.get(sy);
        IF (sy=pcS.colon) OR (sy=pcS.becomes) THEN
          pcS.get(sy);
          IF sy=pcS.ident THEN pcO.import(o,pcS.name,FALSE); END;
          check_id;
        ELSE pcO.import(o,o^.name,FALSE);
        END;
        pcO.dcl(o);
      ELSE check_id;
      END;
      IF    sy=pcS.coma  THEN pcS.get(sy)
      ELSIF sy=pcS.semic THEN EXIT
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.coma)
      ELSE EXIT
      END
    END;
    check_get(pcS.semic);
  END;
END import;

PROCEDURE compile(opts: BITSET; VAR cu: pcK.NODE);
BEGIN
  headTD:=NIL; tailTD:=NIL;
  pcS.opts:=opts;
  pcS.Ini; pcO.Ini;
  pcO.standard(pcK.sfunc,"ENTIER",entier);
  pcO.standard(pcK.sfunc,"CHR",chr);
  pcO.standard(pcK.sfunc,"ORD",ord);
  pcO.standard(pcK.sfunc,"LONG",long);
  pcO.standard(pcK.sfunc,"SHORT",short);
  pcO.system(pcK.sfunc,"VAL",valfn);
  pcO.system(pcK.sproc,"ORIGIN",pcK.origin);
  pcO.system(pcK.sproc,"DCOPY",pcK.dcopy);
  pcO.system(pcK.sproc,"MOVE",pcK.move);
  pcM.pass2:=FALSE;
  pcO.def:=FALSE; pcO.imp:=TRUE;
  pcS.get(sy);
  IF sy#pcS.module THEN pcS.Fault(80,''); RETURN END;
  pcS.get(sy);
  IF sy#pcS.ident  THEN pcS.Fault(80,''); RETURN END;
  pcM.str_copy(pcO.cu_name,pcS.name);
  pcO.new_obj(CU,pcK.module);
  pcM.str_copy(CU^.name,pcS.name);
  pcB.new(cu,pcK.module); cu^.obj:=CU;
  pcB.ini_gen;
  pcO.enter_scope(CU);
    pcO.exts[0]:=CU^.head; CU^.scope:=0; pcO.ext_no:=1;
    pcS.get(sy);
    IF sy=pcS.lbr THEN
      pcS.get(sy);
      IF sy=pcS.literal THEN (*tag:=pcS.value;*) pcS.get(sy) END;
      check_get(pcS.rbr);
    END;
    check_get(pcS.semic);
    import;
    Block(cu);
  pcO.exit_scope;
  cu^.next:=headTD;
  IF sy#pcS.period THEN pcS.expc(pcS.period) END;
  pcM.pass2:=TRUE;
END compile;

BEGIN
  vers:='Oberon PX  v0.21 /30-Jun-91/';
  opts:={r_check,b_check,nil_check,pcS.proc_check,pcS.type_check};
  ptrs:=NIL;
END pcO2.
