IMPLEMENTATION MODULE pcM2; (* Ned 10-Nov-87. (c) KRONOS *)
                            (* Ned 11-Aug-90. (c) KRONOS *)

IMPORT pcK, pcM, pcS, pcO, pcB;
IMPORT pcVis;

TYPE SHORTINT = INTEGER;

CONST incompatible = 30;

CONST
  r_check   = pcS.range_check;
  b_check   = pcS.index_check;
  nil_check = pcS.nil_check;
  INTADR    = pcK.WHOLEs+pcK.Forms{pcK.addr};

CONST (* conversion functions *)
  chr   = -1;   ord   = -2;
  long  = -3;   short = -4;
  trunc = -5;   float = -6;

TYPE ITEM = pcB.ITEM;

VAR
  CU   : pcK.OBJECT;  (* compilation unit *)
  sy   : pcS.Symbol;
  withs: pcK.OBJECT;
  ptrs : pcK.STRUCT;

(*---------------------------------------------------------------*)
PROCEDURE Expr(VAR e: ITEM); FORWARD;
PROCEDURE type(VAR t: pcK.STRUCT); FORWARD;
(*---------------------------------------------------------------*)

PROCEDURE copy_str(VAR s1: pcK.NAME; s2: pcK.NAME);
  VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO HIGH(s1) DO s1[i]:=s2[i] END;
END copy_str;

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

  PROCEDURE try_in_with(VAR v: pcK.OBJECT): BOOLEAN;
    VAR l: pcK.OBJECT; t: pcK.STRUCT;
  BEGIN
    l:=withs;
    WHILE l#NIL DO
      t:=l^.type;
      IF   (t^.mode=pcK.pointer) & (t^.base^.mode=pcK.record)
         & pcO.try_in_rec(pcS.name,t,v)
      THEN
        v:=l; RETURN TRUE
      END;
      l:=l^.next;
    END;
    RETURN FALSE
  END try_in_with;

  VAR v: pcK.OBJECT; ex: ITEM;
BEGIN
  IF sy#pcS.ident THEN pcS.err(7); Expr(i); RETURN END;
  IF (withs#NIL) & try_in_with(v) THEN
    pcB.new(i.node,pcK.var);
    i.node^.obj:=v;  i.node^.type:=v^.type;
    i.mode:=pcK.var;  i.type:=v^.type; i.tags:=v^.tags*{pcK.RO}; i.obj:=v;
    pcB.deref(i); i.node^.sub:=0; (* no check nil *)
    pcB.access(i);
    pcS.get(sy);
  ELSE
    qualident(v); pcB.set_item(v,i);
  END;
  LOOP
    IF sy=pcS.bar THEN pcB.deref(i); pcS.get(sy);
    ELSIF sy=pcS.period THEN
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.err(7); RETURN END;
      pcB.access(i);
      pcS.get(sy);
    ELSIF sy=pcS.lbr THEN
      pcS.get(sy); Expr(ex);
      pcB.index(i,ex);
      IF sy=pcS.coma THEN
        sy:=pcS.lbr (* "," ==  "][" *)
      ELSE check_get(pcS.rbr)
      END;
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
  IF i.type^.mode#pcK.boolean THEN pcB.err(i,incompatible) END;
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
           pcS.get(sy); Expr(r); pcB.type_in(r.type,INTADR); n^.r:=r.node;
         END;
      |pcK.incl,pcK.excl:
         IF r_check IN pcS.opts THEN n^.sub:=pcK.rcheck END;
         var_designator(l); n^.l:=l.node;
         check_get(pcS.coma); Expr(r); n^.r:=r.node;
         IF NOT (l.type^.mode IN pcK.SETs) THEN pcB.err(l,incompatible)
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
         ELSIF l.type^.mode#pcK.pointer THEN pcB.err(l,incompatible)
         END;
      |pcK.dispose:
         var_designator(l); n^.l:=l.node;
         IF NOT (l.type^.mode IN pcK.Forms{pcK.pointer,pcK.dynarr}) THEN
           pcB.err(l,incompatible)
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
         pcB.type_cmp(r.type,pcO.addr); n^.r^.l:=r.node;
         check_get(pcS.coma); Expr(r);
         pcB.type_in(r.type,pcK.WHOLEs);   n^.r^.r:=r.node;
      |pcK.dcopy:
         Expr(l); n^.l:=l.node; check_get(pcS.coma);
         IF l.type^.mode#pcK.dynarr THEN pcB.err(l,58) END;
         var_designator(r); n^.r:=r.node;
         IF l.type#r.type THEN pcB.err(l,incompatible) END;
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

PROCEDURE ProcCall(n: pcK.NODE; func: BOOLEAN);
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
  parmlist(n^.r,proc^.type^.next);
  IF proc^.mode=pcK.proc THEN n^.obj:=proc^.obj; pcB.dispose(n^.l) END;
END ProcCall;

(*----------------------------------------------------------------*)

PROCEDURE Expr(VAR e1: ITEM);

  PROCEDURE sFuncCall(VAR i: ITEM);
    VAR v: ITEM; op: SHORTINT;
  BEGIN
    op:=SHORTINT(i.node^.obj^.adr);
    pcB.dispose(i.node);
    check_get(pcS.lpar);
    CASE op OF
      |pcK.min,pcK.max:
         designator(i); pcB.min_max_fn(i,pcK.Sub(op));
      |pcK.adr:
         designator(i);
         IF sy=pcS.coma THEN
           pcS.get(sy); designator(v); pcB.ref_fn(i,v);
         ELSE pcB.adr_fn(i)
         END;
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
         pcB.unary(i,pcK.abs,i.type);
      |pcK.odd:
         Expr(i); pcB.type_in(i.type,INTADR); pcB.unary(i,pcK.odd,pcO.boolean);
      |pcK.cap:
         Expr(i); pcB.type_equ(i.type,pcK.char); pcB.unary(i,pcK.cap,pcO.char);
      |ord:
         Expr(i);
         pcB.type_in(i.type,pcK.Forms{pcK.enum,pcK.char,pcK.boolean,pcK.byte});
         i.type:=pcO.shortint;
      |chr:
         Expr(i); pcB.chkScalar(i.type); pcB.unary(i,pcK.rcheck,pcO.char);
      |trunc:
         Expr(i); pcB.type_in(i.type,pcK.REALs); pcB.convert(i,pcO.longint);
      |float:
         Expr(i); pcB.type_in(i.type,pcK.WHOLEs); pcB.convert(i,pcO.real);
      |long:
         Expr(i); pcB.long_fn(i);
      |short:
         Expr(i); pcB.short_fn(i);
      |pcK.ash:
         Expr(i); Expr(v); pcB.ash_fn(i,v);
(*
      |pcS.rol,pcS.ror:
         IF e1.type^.mode IN pcK.INTs THEN
           IF e1.type^.mode<pcK.longint THEN pcB.convert(e1,pcO.longint) END;
         ELSIF NOT (e1.type^.mode IN pcK.Forms{pcK.word,pcK.bitset}) THEN
           pcS.err(incompatible); e1.type:=pcO.invtype;
         END;
         pcB.type_in(e2.type,pcK.INTs);
         IF symbol=pcS.rol THEN pcB.binary(e1,e2,pcK.rol,e1.type);
         ELSE                   pcB.binary(e1,e2,pcK.ror,e1.type);
         END;
*)
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
    IF n#pcB.len(type) THEN pcS.err(incompatible) END;
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
    VAR ex: ITEM; n: pcK.NODE;
  BEGIN
    designator(i);
    IF sy=pcS.lpar THEN
      IF i.mode=pcK.type THEN
        pcS.get(sy); Expr(ex); check_get(pcS.rpar);
        i.mode:=ex.mode; i.obj:=ex.obj;
        n:=i.node; n^.mode:=pcK.unary; n^.sub:=pcK.typetran; n^.l:=ex.node;
      ELSIF i.mode=pcK.sfunc THEN sFuncCall(i);
      ELSIF i.mode=pcK.sproc THEN pcS.err(91); skip_parms;
      ELSE
        pcB.new(n,pcK.call); n^.l:=i.node; n^.type:=pcO.invtype; i.node:=n;
        ProcCall(n,TRUE);
        i.type:=n^.type;
      END;
    ELSIF sy=pcS.lbrace THEN
      IF    i.mode#pcK.type          THEN pcB.err(i,31)
      ELSIF i.type^.mode IN pcK.SETs THEN set(i,i.type);
      ELSIF i.type^.mode=pcK.array   THEN array(i,i.type);
      ELSE pcB.err(i,81);
      END;
    ELSIF i.mode IN {pcK.sproc,pcK.sfunc,pcK.type} THEN pcB.err(i,121)
    END;
  END iden;

  PROCEDURE Factor(VAR i: ITEM);
    VAR v: pcK.OBJECT;
  BEGIN
    CASE sy OF
      |pcS.literal: pcB.value(i); pcS.get(sy);
      |pcS.nil    : i.mode:=pcK.cons; i.obj:=NIL; i.type:=pcO.niltype;
                    pcB.new(i.node,pcK.value); i.node^.val:=pcM.nilval;
                    i.node^.type:=i.type;
                    pcS.get(sy);
      |pcS.lpar   : pcS.get(sy); Expr(i); check_get(pcS.rpar);
      |pcS.ident  : iden(i);
                    IF i.type^.mode=pcK.range THEN i.type:=i.type^.base END;
      |pcS.lbrace : set(i,pcO.bitset);
      |pcS.not    : pcS.get(sy); Factor(i);
                    pcB.type_equ(i.type,pcK.boolean);
                    pcB.unary(i,pcK.not,pcO.boolean);
      |pcS.array  : pcS.get(sy); check_get(pcS.of);
                    type_qua(v); open_array(i,v^.type);
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
           IF e1.type#e2.type THEN pcS.err(incompatible) END;
           pcB.binary(e1,e2,pcK.xor,e1.type);
         ELSE
           pcB.numeric(e1,e2,pcK.NUMs);
           pcB.binary(e1,e2,pcK.slash,e1.type);
         END;
      |pcS.times:
         IF e1.type^.mode IN pcK.SETs+pcK.Forms{pcK.boolean} THEN
           IF e1.type#e2.type THEN pcS.err(incompatible) END;
           pcB.binary(e1,e2,pcK.and,e1.type);
         ELSE
           pcB.numeric(e1,e2,pcK.NUMs);
           pcB.binary(e1,e2,pcK.mul,e1.type);
         END;
      |pcS.div:
         pcB.numeric(e1,e2,pcK.WHOLEs);
         pcB.binary(e1,e2,pcK.div,e1.type);
      |pcS.mod:
         pcB.numeric(e1,e2,pcK.WHOLEs);
         pcB.binary(e1,e2,pcK.mod,e1.type);
      |pcS.rem:
         pcB.numeric(e1,e2,pcK.WHOLEs);
         pcB.binary(e1,e2,pcK.rem,e1.type);
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
      IF e1.type^.mode IN pcK.SETs THEN
        pcB.unary(e1,pcK.compl,e1.type);
      ELSE
        pcB.type_in(e1.type,pcK.NUMs);
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
          IF e1.type#e2.type THEN pcS.err(incompatible) END;
          pcB.binary(e1,e2,pcK.bic,e1.type);
        ELSIF (e1.type=pcO.addr) & (e2.type#pcO.addr) THEN
          pcB.numeric(e1,e2,pcK.NUMs);
          pcB.binary(e1,e2,pcK.plus,pcO.addr);
        ELSE
          pcB.numeric(e1,e2,pcK.NUMs);
          pcB.binary(e1,e2,pcK.minus,e1.type);
        END;
      ELSIF op=pcS.plus  THEN
        Term(e2); t:=e1.type;
        IF t^.mode IN pcK.NUMs THEN
          pcB.numeric(e1,e2,pcK.NUMs);
          pcB.binary(e1,e2,pcK.plus,e1.type);
        ELSIF t^.mode IN pcK.SETs+pcK.Forms{pcK.boolean} THEN
          IF t#e2.type THEN pcS.err(incompatible) END;
          pcB.binary(e1,e2,pcK.or,e1.type);
        ELSIF (t=pcO.addr) OR (e2.type=pcO.addr) THEN
          pcB.numeric(e1,e2,pcK.NUMs);
          pcB.binary(e1,e2,pcK.plus,pcO.addr);
        ELSE pcB.concat(e1,e2);
        END;
      END;
    END;
  END Simple;

  VAR e2: ITEM; op: pcK.Sub;
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
    IF NOT (e2.type^.mode IN pcK.SETs) THEN pcS.err(incompatible)
    ELSE pcB.scalar_cmp(e2.type^.base,e1.type);
    END;
    pcB.binary(e1,e2,pcK.in,pcO.boolean);
  END;
END Expr;

(*----------------------------------------------------------------*)

PROCEDURE StatSeq(VAR stat: pcK.NODE; loop: pcK.NODE);

  PROCEDURE with(node: pcK.NODE);
    VAR v: pcK.OBJECT; i: ITEM;
  BEGIN
    pcS.get(sy);
    var_designator(i);
    pcO.new_obj(v,pcK.var); v^.next:=withs; withs:=v;
    IF i.type^.mode#pcK.record THEN pcS.err(51) END;
    pcO.new_type(v^.type,pcK.pointer);
    v^.type^.base:=i.type;
    node^.obj:=v; node^.l:=i.node;
    check_get(pcS.do); StatSeq(node^.r,loop); check_get(pcS.end);
    withs:=withs^.next;
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

  VAR tail,n: pcK.NODE; i: ITEM; type: pcK.STRUCT;
BEGIN
  tail:=NIL;
  LOOP
    CASE sy OF
     |pcS.ident :
        designator(i);
        IF sy=pcS.becomes THEN
          pcB.tie(stat,tail,pcK.assign);
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
        ELSIF i.mode=pcK.sfunc THEN pcS.err(90); skip_parms;
        ELSIF i.mode=pcK.type  THEN pcS.err(121); skip_parms;
        ELSE
          pcB.tie(stat,tail,pcK.call); tail^.l:=i.node;
          ProcCall(tail,FALSE);
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
        pcB.tie(stat,tail,pcK.with); with(tail);
        tail^.pos:=pcS.txtpos;
     |pcS.semic :
     |pcS.end,pcS.else,pcS.elsif,pcS.until,pcS.sep: EXIT
    ELSE
      pcS.err(86);
      CASE sy OF
       |pcS.semic,pcS.end,pcS.else,pcS.elsif,pcS.until,pcS.sep: EXIT
      ELSE
      END;
      pcS.get(sy);
    END;
    CASE sy OF
     |pcS.end,pcS.else,pcS.elsif,pcS.until,pcS.sep: EXIT
    ELSE check_get(pcS.semic);
    END;
  END;
END StatSeq;

(*----------------------------------------------------------------*)

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
        pcO.new_obj(o,pcK.cons); copy_str(o^.name,pcS.name);
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
      pcO.new_obj(t^.next,pcK.inv); copy_str(t^.next^.name,pcS.name);
      t^.base:=pcO.undef;
      t^.inx:=ptrs; ptrs:=t;
      pcS.get(sy);
    ELSE
      type(t^.base);
    END;
  END pointer;

  PROCEDURE record(VAR rec: pcK.STRUCT);

    PROCEDURE fields(VAR x,tail: pcK.OBJECT); FORWARD;

    PROCEDURE SimpleFields(VAR head,tail: pcK.OBJECT);
      VAR t: pcK.STRUCT; l,o: pcK.OBJECT;
    BEGIN
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          pcO.new_obj(o,pcK.field); copy_str(o^.name,pcS.name);
          pcO.dcl_in_rec(rec,o);
          IF l=NIL THEN
            IF tail=NIL THEN head:=o ELSE tail^.next:=o END; l:=o;
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
      type(t);
      WHILE l#NIL DO l^.type:=t; l:=l^.next END;
    END SimpleFields;

    PROCEDURE VariantPart(VAR head,tail: pcK.OBJECT);

      PROCEDURE Variant(tag: pcK.OBJECT);
        VAR e: ITEM; a,b: LONGINT; x,tail: pcK.OBJECT;
      BEGIN
        pcO.new_obj(x,pcK.header);
        x^.next:=tag^.r; tag^.r:=x;
        tail:=NIL;
        IF sy=pcS.else THEN
          pcS.get(sy); fields(x^.r,tail);
        ELSE
          WHILE sy#pcS.colon DO
            Expr(e); pcB.const_val(e,a); pcB.scalar_cmp(tag^.type,e.type);
            IF sy=pcS.range THEN
              pcS.get(sy);
              Expr(e); pcB.const_val(e,b); pcB.scalar_cmp(tag^.type,e.type);
              IF a>b THEN pcS.err(124) END;
            END;
            IF sy#pcS.colon THEN check_get(pcS.coma) END
          END;
          pcS.get(sy);
          fields(x^.r,tail);
          IF (sy#pcS.else) & (sy#pcS.end) THEN check_get(pcS.sep) END;
        END;
      END Variant;

      VAR tag,o: pcK.OBJECT;
    BEGIN
      pcS.get(sy);
      pcO.new_obj(tag,pcK.header);
      IF sy=pcS.colon THEN (* unnamed tag *)
      ELSIF sy#pcS.ident THEN
        pcS.err(7);
        WHILE sy#pcS.end DO pcS.get(sy) END;
        RETURN
      ELSE
        pcO.new_obj(tag^.l,pcK.field); copy_str(tag^.l^.name,pcS.name);
        pcO.dcl_in_rec(rec,tag^.l);
        pcS.get(sy);
      END;
      check_get(pcS.colon);
      type_qua(o); pcB.chkScalar(o^.type);
      tag^.type:=o^.type;
      IF tag^.l#NIL THEN tag^.l^.type:=o^.type END;
      IF tail=NIL THEN head:=tag ELSE tail^.next:=tag END;
      tail:=tag;
      check_get(pcS.of);
      WHILE sy#pcS.end DO
        IF sy=pcS.sep THEN pcS.get(sy) ELSE Variant(tag) END;
      END;
      pcS.get(sy);
    END VariantPart;

    PROCEDURE fields(VAR head,tail: pcK.OBJECT);
    BEGIN
      LOOP
        IF    sy=pcS.ident THEN SimpleFields(head,tail)
        ELSIF sy=pcS.case  THEN VariantPart (head,tail)
        ELSIF (sy=pcS.end) OR (sy=pcS.sep) OR (sy=pcS.else) THEN EXIT
        ELSIF sy#pcS.semic THEN
          pcS.expc(pcS.end);
          WHILE sy#pcS.end DO pcS.get(sy) END;
          pcS.get(sy); EXIT
        END;
        IF (sy#pcS.end) & (sy#pcS.sep) & (sy#pcS.else) THEN
          check_get(pcS.semic)
        END;
      END;
    END fields;

    VAR tail: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    pcO.new_type(rec,pcK.record);
    tail:=NIL;
    fields(rec^.next,tail);
    check_get(pcS.end);
  END record;

  PROCEDURE proctype(VAR proc: pcK.STRUCT);
    VAR mode: pcK.Mode; l,p,o: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    pcO.new_type(proc,pcK.proctype); proc^.base:=pcO.undef;
    IF sy#pcS.lpar THEN RETURN END;
    pcS.get(sy);
    l:=NIL;
    LOOP
      mode:=pcK.var;
      IF    sy=pcS.var THEN pcS.get(sy); mode:=pcK.varpar;
      ELSIF sy=pcS.val THEN pcS.get(sy); pcS.err(92);
      END;
      IF (sy=pcS.ident) OR (sy=pcS.array) THEN
        pcO.new_obj(p,mode);
        FormalType(p^.type);
        IF l=NIL THEN proc^.next:=p; l:=p ELSE l^.next:=p; l:=p END;
      ELSE pcS.err(7);
      END;
      IF (sy=pcS.rpar) OR (sy=pcS.seq) THEN EXIT END;
      check_get(pcS.coma);
    END;
    IF sy=pcS.seq THEN
      pcS.get(sy);
      IF sy=pcS.var THEN
           pcO.new_obj(p,pcK.varseq); pcS.get(sy);
      ELSE pcO.new_obj(p,pcK.seq); p^.tags:={pcK.RO};
      END;
      type_qua(o);
      pcO.new_type(p^.type,pcK.array_of);
      p^.type^.base:=o^.type; p^.type^.n:=1;
      IF l=NIL THEN proc^.next:=p ELSE l^.next:=p END;
    END;
    check_get(pcS.rpar);
    IF sy=pcS.colon THEN
      pcS.get(sy);
      type_qua(o);
      proc^.base:=o^.type;
    END;
  END proctype;

  PROCEDURE range(VAR t: pcK.STRUCT);
    VAR e: ITEM; t1,t2: pcK.STRUCT;
  BEGIN
    pcS.get(sy);
    pcO.new_type(t,pcK.range);
    Expr(e); pcB.const_val(e,t^.n); pcB.chkScalar(e.type);
    IF e.type^.mode=pcK.range THEN t1:=e.type^.base ELSE t1:=e.type END;
    check_get(pcS.range);
    Expr(e); pcB.const_val(e,t^.m); pcB.chkScalar(e.type);
    IF e.type^.mode=pcK.range THEN t2:=e.type^.base ELSE t2:=e.type END;
    IF t1^.mode IN pcK.WHOLEs THEN
      pcB.type_in(t2,pcK.WHOLEs);
      IF t1^.mode>t2^.mode THEN t^.base:=t1 ELSE t^.base:=t2 END;
    ELSE
      pcB.type_cmp(t1,t2);
      t^.base:=t1;
    END;
    IF t^.n>t^.m THEN pcS.err(124) END;
    IF t^.base^.mode=pcK.shortIC THEN t^.base:=pcO.shortcard END;
    IF t^.base^.mode=pcK.IC THEN t^.base:=pcO.cardinal END;
    check_get(pcS.rbr);
  END range;

  VAR o: pcK.OBJECT;
BEGIN
  CASE sy OF
   |pcS.ident    : type_qua(o); t:=o^.type;
                   IF t=pcO.undef THEN pcS.err(21); t:=pcO.invtype END;
   |pcS.lbr      : range(t)
   |pcS.lpar     : enum(t)
   |pcS.procedure: proctype(t)
   |pcS.record   : record(t);
   |pcS.set      : pcS.get(sy); check_get(pcS.of);
                   pcO.new_type(t,pcK.set); type(t^.base);
                   pcB.chkScalar(t^.base);
   |pcS.array    : pcS.get(sy); pcO.new_type(t,pcK.array);
                   type(t^.inx); pcB.chkScalar(t^.inx);
                   IF sy=pcS.coma THEN sy:=pcS.array ELSE check_get(pcS.of) END;
                   type(t^.base);
   |pcS.dynarr   : pcS.get(sy); check_get(pcS.of);
                   pcO.new_type(t,pcK.dynarr); t^.n:=1; type(t^.base);
   |pcS.pointer  : pointer(t)
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

  PROCEDURE ConstDcl(VAR head,tail: pcK.OBJECT);
    VAR o: pcK.OBJECT; e: ITEM;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      pcO.new_obj(o,pcK.cons); copy_str(o^.name,pcS.name); pcS.get(sy);
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
      IF tail=NIL THEN head:=o ELSE tail^.next:=o END;
      tail:=o;
      IF    sy=pcS.semic THEN pcS.get(sy)
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.semic)
      ELSE check_get(pcS.semic); RETURN
      END;
    END;
  END ConstDcl;

  PROCEDURE VarDcl(VAR head,tail: pcK.OBJECT; tags: BITSET);
    VAR l,o: pcK.OBJECT; t: pcK.STRUCT;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          pcO.new_obj(o,pcK.var); copy_str(o^.name,pcS.name); pcO.dcl(o);
          o^.tags:=tags;
          IF l=NIL THEN
            IF tail=NIL THEN head:=o ELSE tail^.next:=o END; l:=o;
          ELSE tail^.next:=o;
          END;
          tail:=o
        END;
        check_id;
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

  PROCEDURE one(o: pcK.OBJECT; opaque,base: pcK.STRUCT);

    PROCEDURE change(VAR type: pcK.STRUCT; opaque,base: pcK.STRUCT);
      VAR l: pcK.OBJECT;
    BEGIN
      IF type=opaque THEN type:=base; RETURN END;
      CASE type^.mode OF
        |pcK.pointer : IF type^.base^.obj=NIL THEN
                         change(type^.base,opaque,base);
                       END;
        |pcK.array   : change(type^.base,opaque,base);
        |pcK.vector  : change(type^.base,opaque,base);
        |pcK.array_of: change(type^.base,opaque,base);
        |pcK.dynarr  : change(type^.base,opaque,base);
        |pcK.proctype: l:=type^.next;
                      WHILE l#NIL DO
                        change(l^.type,opaque,base); l:=l^.next
                      END;
                      change(type^.base,opaque,base);
        |pcK.record  : one(type^.locs,opaque,base);
      ELSE (* nothing *)
      END;
    END change;

  BEGIN
    IF o=NIL THEN RETURN END;
    one(o^.l,opaque,base);
    IF o^.mode#pcK.module THEN change(o^.type,opaque,base) END;
    one(o^.r,opaque,base);
  END one;

  PROCEDURE TypeDcl(VAR head,tail: pcK.OBJECT);
    VAR o: pcK.OBJECT; opaque,t: pcK.STRUCT; i: INTEGER;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      pcO.new_obj(o,pcK.type); copy_str(o^.name,pcS.name); pcS.get(sy);
      IF sy=pcS.equ THEN
        IF (pcO.scope=CU) & pcO.try_in(pcS.name,pcO.scope,o) THEN
          IF (o^.mode=pcK.type) & (o^.type^.mode=pcK.opaque) THEN
            pcS.get(sy);
            type_dcl(t);
            IF t^.mode IN pcK.Forms{pcK.opaque,pcK.pointer,pcK.addr} THEN
              opaque:=o^.type;
              FOR i:=0 TO pcO.ext_no-1 DO one(pcO.exts[i]^.r,opaque,t) END;
            ELSE pcS.err(52); o^.type:=pcO.invtype;
            END;
         ELSE pcS.err(22); pcS.get(sy); type_dcl(t);
          END;
          o:=NIL;
        ELSE
          pcS.get(sy);
          o^.type:=pcO.undef; pcO.dcl(o);
          type_dcl(t); o^.type:=t;
          IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
          check_ptr(o);
        END;
      ELSIF (sy=pcS.semic) & pcO.def THEN
        pcO.new_type(o^.type,pcK.opaque); o^.type^.obj:=o;
        pcO.dcl(o);
      ELSE pcS.expc(pcS.equ)
      END;
      check_get(pcS.semic);
      IF o#NIL THEN
        IF tail=NIL THEN head:=o ELSE tail^.next:=o END;
        tail:=o;
      END;
    END;
  END TypeDcl;

  PROCEDURE prochead(VAR type: pcK.STRUCT);
    VAR mode: pcK.Mode; tags: BITSET; tail,l,p,o: pcK.OBJECT;
        parm: pcK.STRUCT;
  BEGIN
    pcO.new_type(type,pcK.proctype); type^.base:=pcO.undef;
    IF sy#pcS.lpar THEN RETURN END;
    pcS.get(sy);
    tail:=NIL;
    LOOP
      mode:=pcK.var; tags:={};
      IF    sy=pcS.var THEN pcS.get(sy); mode:=pcK.varpar;
      ELSIF sy=pcS.val THEN pcS.get(sy);
        IF pcO.def THEN pcS.err(93) ELSE tags:={pcK.RO} END;
      ELSIF sy#pcS.ident THEN EXIT
      END;
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          pcO.new_obj(o,mode); copy_str(o^.name,pcS.name);
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
      IF (mode=pcK.var) & (parm^.mode=pcK.dynarr) THEN INCL(tags,pcK.RO) END;
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
        pcO.new_obj(o,mode); copy_str(o^.name,pcS.name); o^.tags:=tags;
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
  END prochead;

  PROCEDURE CodeProc(p: pcK.OBJECT);
    VAR n,tail: pcK.NODE; ex: ITEM;
  BEGIN
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

  PROCEDURE ProcDcl(VAR head,tail: pcK.OBJECT; VAR x: pcK.NODE);
    CONST FWD = {pcK.xproc,pcK.proc};
    VAR p,l: pcK.OBJECT; type: pcK.STRUCT; pre: BOOLEAN;
  BEGIN
    x:=NIL;
    pcS.get(sy);
    IF sy#pcS.ident THEN pcS.err(7); RETURN END;
    pre:=pcO.try_in(pcS.name,pcO.scope,p) & (p^.mode IN FWD) & (pcK.forward IN p^.tags);
    IF NOT pre THEN
      pcO.new_obj(p,pcK.proc); copy_str(p^.name,pcS.name); pcO.dcl(p);
      IF pcO.def THEN p^.mode:=pcK.xproc END;
      IF pcS.external IN pcS.opts THEN
        IF p^.scope>0 THEN pcS.err(0)
        ELSE p^.mode:=pcK.xproc; INCL(p^.tags,pcK.external);
        END;
      END;
    END;
    pcS.get(sy);
    pcO.enter_scope(p);
      prochead(type);
      IF pre THEN
        pcB.proc_cmp(p^.type,type,TRUE); pcO.rem_type(type);
      ELSE p^.type:=type;
      END;
      IF NOT pre OR (pcK.exported IN p^.tags) THEN
        IF tail=NIL THEN head:=p ELSE tail^.next:=p END;
        tail:=p;
      END;
      check_get(pcS.semic);
      IF pcK.external IN p^.tags THEN
        EXCL(p^.tags,pcK.forward)
      ELSIF NOT pcO.def THEN
        IF sy=pcS.forward THEN
          IF pcK.forward IN p^.tags THEN pcS.err(23) END;
          INCL(p^.tags,pcK.forward);
          pcS.get(sy);
        ELSIF sy=pcS.code THEN
          p^.mode:=pcK.cproc;
          IF pcK.forward IN p^.tags THEN pcS.err(22) END;
          pcS.get(sy);
          CodeProc(p);
        ELSE
          EXCL(p^.tags,pcK.forward);
          l:=p^.type^.next;
          WHILE l#NIL DO pcO.dcl(l); l:=l^.next END;
          pcB.new(x,pcK.proc); x^.obj:=p;
          Block(x);
        END;
        check_get(pcS.semic);
      END;
    pcO.exit_scope;
  END ProcDcl;

  PROCEDURE rts_list(VAR head,tail: pcK.OBJECT);

    PROCEDURE append(rts,o: pcK.OBJECT);
    BEGIN
      IF rts^.mode#pcK.sproc THEN pcS.err_id(57,rts^.name)
      ELSIF (rts^.adr<pcK.new) OR (rts^.adr>pcK.resize) THEN
        pcS.err_id(129,rts^.name)
      ELSIF rts^.head#NIL THEN pcS.err_id(130,rts^.name)
      ELSE
        rts^.head:=o;
        IF tail=NIL THEN head:=rts ELSE tail^.next:=rts END;
        tail:=rts;
      END;
    END append;

    PROCEDURE define(mod: pcK.OBJECT; rn,on: pcK.NAME);
      VAR rts,o: pcK.OBJECT;
    BEGIN
      IF NOT pcO.try(rn,rts) THEN pcM.abort END;
      IF pcO.try_in(on,mod,o) THEN append(rts,o) END;
    END define;

    VAR o,rts: pcK.OBJECT;
  BEGIN
    pcS.get(sy);
    IF sy#pcS.ident THEN pcS.err(7); RETURN END;
    IF NOT pcM.str_equ(pcS.name,'STORAGE') THEN pcS.err_id(20,pcS.name) END;
    pcS.get(sy);
    IF sy=pcS.lpar THEN pcS.get(sy);
      LOOP
        IF sy#pcS.ident THEN pcS.err(7); EXIT END;
        pcO.vis(pcS.name,rts); pcS.get(sy);
        IF sy#pcS.colon THEN pcS.expc(pcS.colon); EXIT END;
        pcS.get(sy);
        qualident(o);
        append(rts,o);
        IF sy=pcS.semic THEN pcS.get(sy) ELSE EXIT END;
      END;
      check_get(pcS.rpar);
    ELSIF sy=pcS.colon THEN pcS.get(sy);
      IF sy#pcS.ident THEN pcS.err(7);
      ELSE
        pcO.vis(pcS.name,o);
        IF o^.mode#pcK.module THEN pcS.err_id(56,o^.name)
        ELSE
          define(o,'NEW','ALLOCATE');
          define(o,'DISPOSE','DEALLOCATE');
          define(o,'RESIZE','REALLOCATE');
        END;
        pcS.get(sy);
      END;
    ELSE pcS.err(82)
    END;
    check_get(pcS.semic);
  END rts_list;

  PROCEDURE traverse(o: pcK.OBJECT);
  BEGIN
    IF o#NIL THEN
      traverse(o^.l);
      IF pcK.forward IN o^.tags THEN pcS.err_id(89,o^.name) END;
      traverse(o^.r);
    END;
  END traverse;

  VAR head,tail: pcK.OBJECT; proc,x: pcK.NODE; save: pcK.STRUCT;
BEGIN
  save:=ptrs; ptrs:=NIL;
  proc:=NIL; head:=NIL; tail:=NIL;
  LOOP
    CASE sy OF
     |pcS.const    : ConstDcl(head,tail);
     |pcS.var      : VarDcl(head,tail,{});
     |pcS.val      : IF NOT pcO.def THEN pcS.err(94) END;
                     VarDcl(head,tail,{pcK.RO_export});
     |pcS.type     : TypeDcl(head,tail);
     |pcS.with     : IF pcO.def THEN pcS.err(93) END;
                     IF pcO.level#0 THEN pcS.err(95) END;
                     rts_list(head,tail);
     |pcS.procedure: ProcDcl(head,tail,x);
                     IF x#NIL THEN
                       IF proc=NIL THEN enter^.l:=x ELSE proc^.next:=x END;
                       proc:=x;
                     END;
     |pcS.begin,pcS.end: EXIT
    ELSE pcS.err(82); pcS.get(sy)
    END;
  END;
  WHILE ptrs#NIL DO pcS.err_id(97,ptrs^.next^.name); ptrs:=ptrs^.inx END;
  ptrs:=save;
  enter^.obj^.head^.next:=head;
  traverse(enter^.obj^.head^.r);
  IF (sy=pcS.begin) THEN
    pcS.get(sy);
    IF pcO.def THEN pcS.err(93)
    ELSIF sy#pcS.end THEN StatSeq(enter^.r,NIL)
    END;
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
        pcO.new_obj(o,pcK.module); copy_str(o^.name,pcS.name);
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
  pcS.opts:=opts;
  withs:=NIL;
  pcS.Ini; pcO.Ini;
  pcO.system  (pcK.sproc,"ORIGIN",pcK.origin);
  pcO.standard(pcK.sproc,"DISPOSE",pcK.dispose);
  pcO.standard(pcK.sproc,"DCOPY",pcK.dcopy);
  pcO.standard(pcK.sfunc,"BITS",pcK.bits);
  pcO.standard(pcK.sfunc,"BYTES",pcK.bytes);
  pcO.standard(pcK.sfunc,"TRUNC",trunc);
  pcO.standard(pcK.sfunc,"FLOAT",float);
  pcO.standard(pcK.sfunc,"CHR",chr);
  pcO.standard(pcK.sfunc,"ORD",ord);
  pcO.standard(pcK.sfunc,"LONG",long);
  pcO.standard(pcK.sfunc,"SHORT",short);
  pcM.pass2:=FALSE;
  pcO.def:=FALSE; pcO.imp:=FALSE;
  pcS.get(sy);
  IF    sy=pcS.definition     THEN pcS.get(sy); pcO.def:=TRUE;
  ELSIF sy=pcS.implementation THEN pcS.get(sy); pcO.imp:=TRUE;
  END;
  IF sy#pcS.module THEN pcS.Fault(80,''); RETURN END;
  pcS.get(sy);
  IF sy#pcS.ident  THEN pcS.Fault(80,''); RETURN END;
  copy_str(pcO.cu_name,pcS.name);
  pcO.new_obj(CU,pcK.module);
  copy_str(CU^.name,pcS.name);
  pcB.new(cu,pcK.module); cu^.obj:=CU;
  pcB.ini_gen;
  pcO.enter_scope(CU);
    pcO.exts[0]:=CU^.head; CU^.scope:=0; pcO.ext_no:=1;
    IF pcO.imp THEN pcO.import(CU,pcO.cu_name,TRUE) END;
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
  IF sy#pcS.period THEN pcS.expc(pcS.period) END;
  pcM.pass2:=TRUE;
END compile;

BEGIN
  vers:='Modula PX  v0.41 /30-Jun-91/';
  opts:={r_check,b_check,nil_check};
  ptrs:=NIL;
END pcM2.
