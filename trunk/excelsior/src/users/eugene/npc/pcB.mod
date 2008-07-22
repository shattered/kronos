IMPLEMENTATION MODULE pcB; (* Ned 29-Jun-91. (c) KRONOS *)

IMPORT pcK, pcM, pcS, pcO;

TYPE SHORTINT = INTEGER;

CONST incomp = 30;

CONST
  r_check   = pcS.range_check;
  b_check   = pcS.index_check;
  nil_check = pcS.nil_check;
  INTADR    = pcK.WHOLEs + pcK.Forms{pcK.addr};

(*---------------------------------------------------------------*)

PROCEDURE new(VAR x: pcK.NODE; mode: pcK.Mode);
BEGIN
  pcM.ALLOCATE(x,SIZE(x^));
  x^:=pcK.null_node;
  x^.mode:=mode;
  x^.pos:=pcS.txtpos;
END new;

PROCEDURE dispose(VAR x: pcK.NODE);
BEGIN
  pcM.DEALLOCATE(x,SIZE(x^));
END dispose;

PROCEDURE tie(VAR head,tail: pcK.NODE; mode: pcK.Mode);
BEGIN
  IF head=NIL THEN new(head,mode); tail:=head;
  ELSE new(tail^.next,mode); tail:=tail^.next;
  END;
END tie;

PROCEDURE app(VAR head,tail: pcK.NODE; n: pcK.NODE);
BEGIN
  IF head=NIL THEN head:=n ELSE tail^.next:=n END;
  tail:=n;
END app;

PROCEDURE int_type(VAR v: LONGINT; VAR type: pcK.STRUCT);
BEGIN
  IF v>=0 THEN
    IF    v<=pcM.max_sint  THEN type:=pcO.shortIC
    ELSIF v<=pcM.max_scard THEN type:=pcO.shortcard
    ELSIF v<=pcM.max_int   THEN type:=pcO.IC
    ELSIF v<=pcM.max_card  THEN type:=pcO.cardinal
    ELSIF v<=pcM.max_lint  THEN type:=pcO.longint
    ELSE v:=1; type:=pcO.shortint; pcS.err(201)
    END;
  ELSIF v>=pcM.min_sint THEN type:=pcO.shortint
  ELSIF v>=pcM.min_int  THEN type:=pcO.integer
  ELSIF v>=pcM.min_lint THEN type:=pcO.longint
  ELSE v:=1; type:=pcO.shortint; pcS.err(201)
  END;
END int_type;

PROCEDURE eval_expr(VAR x: ITEM);
  VAR val: LONGINT; ext: pcK.EXT; set_int: BOOLEAN;
BEGIN
  IF x.node^.mode=pcK.cons  THEN RETURN END;
  set_int:=(x.node^.sub#pcK.conv) & (x.node^.sub#pcK.typetran);
  IF x.node^.mode#pcK.value THEN
    IF pcS.no_errs=0 THEN eval(x.node,val,ext)
    ELSE val:=1; ext:=NIL
    END;
    x.node^.mode:=pcK.value;
    x.node^.val :=val; x.node^.ext :=ext;
  END;
  IF set_int & (x.type^.mode IN pcK.WHOLEs) THEN
    int_type(x.node^.val,x.type);
    x.node^.type:=x.type;
  END;
END eval_expr;

PROCEDURE unary(VAR i: ITEM; op: pcK.Sub; type: pcK.STRUCT);
  VAR x: pcK.NODE;
BEGIN
  new(x,pcK.unary);
  x^.sub:=op;
  x^.l:=i.node;
  x^.type:=type;
  i.type:=type;
  i.node :=x;
  i.obj:=NIL;
  IF i.mode=pcK.cons THEN eval_expr(i) END;
END unary;

PROCEDURE binary(VAR e,e1: ITEM; op: pcK.Sub; type: pcK.STRUCT);
  VAR x: pcK.NODE;
BEGIN
  new(x,pcK.binary); x^.sub:=op;
  x^.l:=e.node;
  x^.r:=e1.node;
  x^.type:=type;
  e.type:=type;
  e.node:=x;
  e.obj:=NIL;
  IF (e.mode=pcK.cons) & (e1.mode=pcK.cons) THEN eval_expr(e);
  ELSE e.mode:=expr
  END;
END binary;

PROCEDURE convert(VAR i: ITEM; type: pcK.STRUCT);
  VAR f: pcK.Form; l: pcK.NODE;
BEGIN
  IF (i.mode#pcK.cons) & (i.node^.sub=pcK.conv) THEN
    f:=i.node^.type^.mode; l:=i.node^.l;
    IF (l^.type^.mode < f) OR (f > type^.mode) THEN
      IF l^.type=type THEN i.node:=i.node^.l END;
      i.type:=type; i.node^.type:=type;
      RETURN
    END;
  END;
  unary(i,pcK.conv,type);
END convert;

PROCEDURE char2str(VAR i: ITEM);
  VAR x: pcK.NODE;
BEGIN
  new(x,pcK.char2str);
  x^.l:=i.node; i.node:=x; i.obj:=NIL;
  i.node^.type:=pcO.char;
END char2str;

PROCEDURE err(i: ITEM; no: INTEGER);
BEGIN
  IF i.mode#pcK.inv THEN pcS.err(no) END;
  i.mode:=pcK.inv; i.type:=pcO.invtype;
END err;

(*----------------------------------------------------------------*)

PROCEDURE chkScalar(VAR t: pcK.STRUCT);
BEGIN
  IF NOT (t^.mode IN pcK.SCALARs+pcK.Forms{pcK.invtype,pcK.range}) THEN
    pcS.err(32); t:=pcO.invtype;
  END;
END chkScalar;

PROCEDURE char(t: pcK.STRUCT): BOOLEAN;
BEGIN
  IF t^.mode=pcK.range THEN
    RETURN t^.base^.mode=pcK.char
  ELSE
    RETURN t^.mode=pcK.char
  END;
END char;

PROCEDURE type_in(t: pcK.STRUCT; valid: pcK.Forms);
BEGIN
  IF t^.mode=pcK.range THEN t:=t^.base END;
  IF NOT (t^.mode IN valid) THEN pcS.err(incomp) END;
END type_in;

PROCEDURE type_equ(t: pcK.STRUCT; mode: pcK.Form);
BEGIN
  IF t^.mode=pcK.range THEN t:=t^.base END;
  IF t^.mode#mode THEN pcS.err(incomp) END;
END type_equ;

PROCEDURE type_cmp(t1,t2: pcK.STRUCT);
  CONST valid = pcK.Forms{pcK.longint,pcK.pointer};
  VAR m1,m2: pcK.Form;
BEGIN
  IF t1^.mode=pcK.range THEN t1:=t1^.base END;
  IF t2^.mode=pcK.range THEN t2:=t2^.base END;
  IF t1=t2 THEN RETURN END;
  m1:=t1^.mode; m2:=t2^.mode;
  IF (m1=pcK.addr) & (m2 IN valid) OR (m2=pcK.addr) & (m1 IN valid) THEN
  ELSIF (m1#pcK.invtype) & (m2#pcK.invtype) THEN
    pcS.err(incomp)
  END;
END type_cmp;

PROCEDURE scalar_cmp(t1,t2: pcK.STRUCT);
(* indexation, in set, case *)
  VAR m1,m2: pcK.Form;
BEGIN
  IF t1^.mode=pcK.range THEN t1:=t1^.base END;
  IF t2^.mode=pcK.range THEN t2:=t2^.base END;
  IF t1#t2 THEN
    m1:=t1^.mode; m2:=t2^.mode;
    IF (m1 IN pcK.WHOLEs) & (m2 IN pcK.WHOLEs) THEN
    ELSIF (m1#pcK.invtype) & (m2#pcK.invtype) THEN
      pcS.err(incomp)
    END;
  END;
END scalar_cmp;

PROCEDURE cmp_params(p1,p2: pcK.OBJECT; copy: BOOLEAN);

  PROCEDURE proc_equ(t1,t2: pcK.STRUCT);
  BEGIN
    IF t1#t2 THEN
      IF t1^.mode=pcK.proctype THEN proc_cmp(t1,t2,FALSE)
      ELSE pcS.err(incomp)
      END;
    END;
  END proc_equ;

  VAR t1,t2: pcK.STRUCT; i: CARDINAL;
BEGIN
  WHILE (p1#NIL) & (p2#NIL) DO
    t1:=p1^.type; t2:=p2^.type;
    WHILE (t1^.mode=pcK.array_of) & (t2^.mode=pcK.array_of) DO
      t1:=t1^.base; t2:=t2^.base;
    END;
    IF p1^.mode#p2^.mode THEN pcS.err_id(39,p1^.name);
    ELSE proc_equ(t1,t2);
    END;
    IF copy THEN
      FOR i:=0 TO HIGH(p1^.name) DO
        p1^.name[i]:=p2^.name[i];
      END;
      p1^.tags:=p2^.tags
    END;
    p1:=p1^.next; p2:=p2^.next;
  END;
  IF p1#p2 THEN pcS.err(38) END;
END cmp_params;

PROCEDURE proc_cmp(proc1,proc2: pcK.STRUCT; copy: BOOLEAN);
BEGIN
  IF proc2^.mode=pcK.proctype THEN
    cmp_params(proc1^.next,proc2^.next,copy);
    IF proc1^.base#proc2^.base THEN pcS.err(44) END;
  ELSE pcS.err(55);
  END;
END proc_cmp;

PROCEDURE check_range(l: pcK.STRUCT; VAR r: ITEM);
BEGIN
  IF l^.mode IN pcK.Forms{pcK.enum,pcK.range} THEN
    IF (r.mode=pcK.cons) OR (r_check IN pcS.opts) THEN
      unary(r,pcK.rcheck,l);
    END;
  END;
END check_range;

PROCEDURE proc_value(VAR r: ITEM);
BEGIN
  IF r.mode=pcK.proc THEN
    IF (r.obj^.mode=pcK.proc) & (r.obj^.scope=0) THEN r.obj^.mode:=pcK.xproc
    ELSIF r.obj^.mode IN {pcK.cproc,pcK.iproc}   THEN pcS.err(121)
    END;
  END;
END proc_value;

PROCEDURE len(t: pcK.STRUCT): LONGINT;
BEGIN
  IF t^.mode=pcK.array THEN
    CASE t^.inx^.mode OF
      |pcK.range,pcK.enum: RETURN t^.inx^.m-t^.inx^.n+1
      |pcK.char         : RETURN 256
      |pcK.boolean      : RETURN 2
      |pcK.shortint     : RETURN pcM.max_sint-pcM.min_sint+1
    ELSE pcM.abort;
    END;
  ELSIF t^.mode=pcK.vector THEN RETURN t^.n
  ELSE pcM.abort;
  END;
END len;

PROCEDURE assign_cmp(l: pcK.STRUCT; VAR r: ITEM);

  PROCEDURE ass_cmp(t: pcK.STRUCT; forms: pcK.Forms);
  BEGIN
    IF NOT (t^.mode IN forms) THEN pcS.err(42) END;
  END ass_cmp;

  VAR lt,rt,p,q: pcK.STRUCT; rm: pcK.Form;
BEGIN
  IF l^.mode=pcK.range THEN lt:=l^.base ELSE lt:=l END;
  IF r.type^.mode=pcK.range THEN rt:=r.type^.base ELSE rt:=r.type END;
  rm:=rt^.mode;
  IF lt#rt THEN
    CASE lt^.mode OF
      |pcK.invtype  :
      |pcK.undef    : pcM.abort;
      |pcK.shortint : ass_cmp(rt,pcK.Forms{pcK.shortint,pcK.shortIC});
      |pcK.shortcard: ass_cmp(rt,pcK.Forms{pcK.shortcard,pcK.shortIC});
      |pcK.integer  : ass_cmp(rt,pcK.Forms{pcK.shortIC..pcK.integer});
      |pcK.cardinal : ass_cmp(rt,pcK.Forms{pcK.shortIC,pcK.shortcard,pcK.IC});
      |pcK.longint  : ass_cmp(rt,INTADR);
      |pcK.real     : ass_cmp(rt,pcK.INTs+pcK.Forms{pcK.real});
      |pcK.longreal : ass_cmp(rt,pcK.NUMs);
      |pcK.byte     : ass_cmp(rt,pcK.BYTEs+pcK.Forms{pcK.shortIC});
      |pcK.word     : IF NOT (rm IN WORDs) THEN pcS.err(33) END;
      |pcK.addr     : ass_cmp(rt,pcK.WHOLEs+pcK.Forms{pcK.pointer,pcK.addr,pcK.niltype});
      |pcK.range    : pcM.abort;
      |pcK.pointer  :
         IF rm=pcK.pointer THEN
           p:=lt^.base; q:=rt^.base;
           IF (p^.mode=pcK.record) & (q^.mode=pcK.record) THEN
             WHILE (q#NIL) & (q#p) DO q:=q^.base END;
             IF q=NIL THEN pcS.err(42) END;
           ELSE pcS.err(42)
           END;
         ELSE ass_cmp(rt,pcK.Forms{pcK.niltype,pcK.addr});
         END;
      |pcK.record  :
         IF rm=pcK.record THEN
           q:=rt^.base;
           WHILE (q#NIL) & (q#lt) DO q:=q^.base END;
           IF q=NIL THEN pcS.err(42) END;
         ELSE pcS.err(42)
         END;
      |pcK.proctype:
         IF rm#pcK.niltype THEN proc_value(r); proc_cmp(lt,rt,FALSE) END;
      |pcK.array,pcK.vector:
         IF (r.mode=pcK.cons) & char(lt^.base) THEN
           IF rm IN pcK.Forms{pcK.vector,pcK.array} THEN
             IF NOT char(rt^.base) OR (len(l)<rt^.n) THEN pcS.err(42) END;
           ELSIF rm=pcK.char THEN char2str(r);
           ELSE pcS.err(42);
           END;
         ELSE pcS.err(42);
         END;
    ELSE
      pcS.err(42);
    END;
  END;
  check_range(l,r);
  IF (r.mode=pcK.cons) & (lt^.mode IN pcK.NUMs) & (rm IN pcK.NUMs) & (lt^.mode>rm) THEN
    unary(r,pcK.conv,lt);
  END;
END assign_cmp;

PROCEDURE parm_cmp(p: pcK.OBJECT; VAR i: ITEM);

  PROCEDURE array_of_cmp(fp,ap: pcK.STRUCT; any: BOOLEAN; valid: pcK.Forms);
    CONST LOCs = pcK.Forms{pcK.byte,pcK.word};
  BEGIN
    IF any & (fp^.base^.mode IN LOCs) THEN (* ok *)
    ELSIF ap^.mode IN valid THEN
      IF fp^.base^.mode=pcK.array_of THEN
        array_of_cmp(fp^.base,ap^.base,any,pcK.ARRs-pcK.Forms{pcK.dynarr});
      ELSE type_cmp(fp^.base,ap^.base)
      END;
    ELSE pcS.err(50)
    END;
  END array_of_cmp;

  VAR q: pcK.STRUCT; mode: pcK.Form;
BEGIN
  mode:=p^.type^.mode;
  IF mode=pcK.array_of THEN
    IF p^.mode=pcK.varpar THEN
      array_of_cmp(p^.type,i.type,TRUE,pcK.ARRs);
    ELSIF (i.mode=pcK.cons) & char(i.type) & char(p^.type^.base) THEN
      char2str(i);
    ELSE array_of_cmp(p^.type,i.type,NOT pcM.oberon,pcK.ARRs);
    END;
  ELSIF mode=pcK.proctype THEN
    proc_value(i); proc_cmp(p^.type,i.type,FALSE)
  ELSIF p^.mode=pcK.varpar THEN
    IF (mode=pcK.record) & (i.type^.mode=pcK.record) THEN
      q:=i.type;
      WHILE (q#NIL) & (q#p^.type) DO q:=q^.base END;
      IF q=NIL THEN pcS.err(30) END;
    ELSIF mode=pcK.byte THEN type_in(i.type,pcK.BYTEs);
    ELSIF mode=pcK.word THEN type_in(i.type,WORDs);
    ELSE type_cmp(p^.type,i.type);
    END;
  ELSE assign_cmp(p^.type,i);
  END;
END parm_cmp;

PROCEDURE value(VAR i: ITEM);
BEGIN
  i.mode:=pcK.cons; i.obj:=NIL;
  CASE pcS.lit OF
    |pcS.intval : int_type(pcS.int,i.type);
    |pcS.charval: i.type:=pcO.char;
    |pcS.realval: i.type:=pcO.real;
    |pcS.lrlval : i.type:=pcO.longreal;
    |pcS.strval : pcO.new_type(i.type,pcK.vector);
                   i.type^.base:=pcO.char;
                   i.type^.n:=LONGINT(pcS.len);
  ELSE pcM.abort;
  END;
  new(i.node,pcK.value);
  i.node^.type:=i.type;
  literal(i.node);
END value;

PROCEDURE numeric(VAR o1,o2: ITEM; valid: pcK.Forms);

  PROCEDURE card2int(VAR o1,o2: ITEM);
  BEGIN
    IF o2.type^.mode>o1.type^.mode THEN
      convert(o1,o2.type);
    ELSIF o1.type^.mode<=pcK.shortcard THEN
      convert(o1,pcO.integer);
      convert(o2,pcO.integer);
    ELSE
      convert(o1,pcO.longint);
      convert(o2,pcO.longint);
    END;
  END card2int;

  VAR m1,m2: pcK.Form;
BEGIN
  IF o1.type=pcO.addr THEN o1.type:=pcO.longint END;
  IF o2.type=pcO.addr THEN o2.type:=pcO.longint END;
  m1:=o1.type^.mode; m2:=o2.type^.mode;
  IF (m1 IN valid) & (m2 IN valid) THEN
    IF (m1 IN pcK.CARDs)#(m2 IN pcK.CARDs) THEN
      IF m1 IN pcK.CARDs THEN card2int(o1,o2) ELSE card2int(o2,o1) END;
    ELSIF m1>m2 THEN convert(o2,o1.type);
    ELSIF m1<m2 THEN convert(o1,o2.type);
    END;
  ELSE err(o1,incomp);
  END;
END numeric;

PROCEDURE concat(VAR e1,e2: ITEM);

  PROCEDURE clen(t: pcK.STRUCT; VAR n: LONGINT);
  BEGIN
    IF t^.mode=pcK.vector THEN INC(n,t^.n-1);
    ELSIF char(t) THEN INC(n);
    ELSE pcS.err(incomp);
    END;
  END clen;

  VAR n: LONGINT; t: pcK.STRUCT;
BEGIN
  IF (e1.mode#pcK.cons) OR (e2.mode#pcK.cons) THEN pcS.err(87); RETURN END;
  n:=0; clen(e1.type,n); clen(e2.type,n);
  pcO.new_type(t,pcK.vector); t^.base:=pcO.char; t^.n:=n+1;
  binary(e1,e2,pcK.concat,t);
END concat;

PROCEDURE relation(VAR e1,e2: ITEM; op: pcK.Sub);
BEGIN
  CASE e1.type^.mode OF
    |pcK.shortIC..pcK.longreal:
       numeric(e1,e2,pcK.NUMs+pcK.Forms{pcK.addr});
    |pcK.addr:
       IF NOT (e2.type^.mode IN pcK.Forms{pcK.pointer,pcK.niltype,pcK.addr}) THEN
         numeric(e1,e2,INTADR);
       END;
    |pcK.set,pcK.bitset:
       IF (e1.type#e2.type) OR (op=pcK.lss) OR (op=pcK.gtr) THEN
         pcS.err(incomp)
       END;
    |pcK.array,pcK.vector,pcK.array_of,pcK.dynarr:
       type_equ(e1.type^.base,pcK.char);
       IF e2.type^.mode IN pcK.ARRs THEN type_equ(e2.type^.base,pcK.char);
       ELSE pcS.err(incomp)
       END;
    |pcK.enum,pcK.boolean,pcK.char:
       type_cmp(e1.type,e2.type);
    |pcK.opaque:
       IF (op#pcK.equ) & (op#pcK.neq)  THEN pcS.err(incomp)
       ELSIF e2.type^.mode#pcK.niltype THEN type_cmp(e1.type,e2.type);
       END;
    |pcK.pointer:
       IF (op#pcK.equ) & (op#pcK.neq) THEN pcS.err(incomp)
       ELSIF NOT (e2.type^.mode IN pcK.Forms{pcK.niltype,pcK.addr}) THEN
         type_cmp(e1.type,e2.type);
       END;
    |pcK.niltype:
       IF   (op#pcK.equ) & (op#pcK.neq) THEN pcS.err(incomp)
       ELSE
         type_in(e2.type,pcK.Forms{pcK.pointer,pcK.proctype,pcK.niltype,pcK.addr});
       END;
    |pcK.proctype:
       IF   (op#pcK.equ) & (op#pcK.neq) THEN pcS.err(incomp)
       ELSIF e2.type^.mode#pcK.niltype  THEN proc_cmp(e1.type,e2.type,FALSE)
       END;
    |pcK.byte,pcK.word:
       IF (op#pcK.equ) & (op#pcK.neq) THEN pcS.err(incomp)
       ELSE type_cmp(e1.type,e2.type);
       END;
  ELSE
    pcS.err(incomp)
  END;
  binary(e1,e2,op,pcO.boolean);
END relation;

PROCEDURE const_val(VAR i: ITEM; VAR val: LONGINT);
  VAR e: pcK.EXT;
BEGIN
  IF i.mode#pcK.cons THEN err(i,87); val:=1;
  ELSE
    IF (i.node^.mode#pcK.value) & (i.node^.mode#pcK.cons) THEN pcM.abort END;
    IF i.type^.mode IN pcK.SCALARs THEN val:=i.node^.val;
    ELSE err(i,32); val:=1;
    END;
  END;
END const_val;

(*----------------------------------------------------------------*)

PROCEDURE deref(VAR i: ITEM);
  VAR x: pcK.NODE;
BEGIN
  IF i.type^.mode=pcK.pointer THEN
    IF i.type^.base=pcO.undef THEN
      pcS.err_id(97,i.type^.next^.name); i.type^.base:=pcO.invtype;
    END;
    i.type:=i.type^.base;
  ELSIF i.type^.mode=pcK.addr THEN i.type:=pcO.word
  ELSE err(i,incomp);
  END;
  new(x,pcK.deref);
  IF (nil_check IN pcS.opts) & (i.node^.mode#pcK.guard) THEN
    x^.sub:=pcK.rcheck
  END;
  x^.type:=i.type;
  x^.l:=i.node; i.node:=x;
  EXCL(i.tags,pcK.RO);
END deref;

PROCEDURE index(VAR i,ex: ITEM);
  VAR x: pcK.NODE;
BEGIN
  IF i.type^.mode=pcK.array THEN
    scalar_cmp(i.type^.inx,ex.type);
  ELSIF i.type^.mode IN pcK.Forms{pcK.array_of,pcK.dynarr,pcK.vector} THEN
    type_in(ex.type,pcK.WHOLEs);
  ELSE
    err(i,incomp);
  END;
  new(x,pcK.index);
  i.type:=i.type^.base; x^.type:=i.type;
  x^.l:=i.node;
  x^.r:=ex.node;
  IF b_check IN pcS.opts THEN x^.sub:=pcK.rcheck END;
  i.node:=x;
  IF i.mode=pcK.cons THEN
    IF ex.mode=pcK.cons THEN eval_expr(i) ELSE i.mode:=expr END;
  END;
END index;

PROCEDURE access(VAR i: ITEM);
  VAR f: pcK.OBJECT; x: pcK.NODE;
BEGIN
  IF i.type^.mode#pcK.record THEN
    IF i.mode#pcK.inv THEN pcS.err(51); i.mode:=pcK.inv END;
    RETURN
  END;
  pcO.vis_in_rec(pcS.name,i.type,f);
  new(x,f^.mode);
  x^.l:=i.node;
  x^.obj:=f;
  IF f^.mode=pcK.method THEN
    i.mode:=pcK.method; i.type:=f^.head^.type;
  ELSE i.type:=f^.type;
  END;
  x^.type:=i.type;
  i.node:=x;
  IF i.mode=pcK.cons THEN eval_expr(i) END;
END access;

PROCEDURE set_item(v: pcK.OBJECT; VAR i: ITEM);
BEGIN
  new(i.node,v^.mode);
  i.node^.obj:=v;    i.node^.type:=v^.type;
  i.mode:=v^.mode;   i.type:=v^.type; i.tags:=v^.tags*{pcK.RO};
  i.obj:=v;
  IF    v^.mode IN pcK.VARs  THEN i.mode:=pcK.var;  i.node^.mode:=pcK.var;
  ELSIF v^.mode IN pcK.PROCs THEN i.mode:=pcK.proc; i.node^.mode:=pcK.proc;
  ELSIF v^.mode=pcK.cons     THEN i.node^.val:=v^.adr; i.node^.ext:=v^.ext;
  ELSIF v^.mode=pcK.module   THEN pcS.err(121); i.mode:=pcK.inv; RETURN
  END;
END set_item;

PROCEDURE type_test(VAR i: ITEM; obj: pcK.OBJECT; guard: BOOLEAN);

  VAR lg: pcK.STRUCT; (* lg = last guard type *)

  PROCEDURE GTT(t0,t1: pcK.STRUCT);
    VAR x: pcK.NODE;
  BEGIN
    IF (t0^.mode#pcK.record) OR (t1^.mode#pcK.record) THEN pcS.err(59)
    ELSIF t0#t1 THEN
      WHILE (t1#NIL) & (t1#lg) DO t1:=t1^.base END ;
      IF t1#NIL THEN
        IF guard THEN new(x,pcK.guard); x^.l:=i.node; i.node:=x;
        ELSE unary(i,pcK.is,pcO.boolean);
        END;
        i.node^.obj:=obj;
      ELSE pcS.err(41)
      END
    ELSIF NOT guard THEN set_item(pcO.true,i);
    END
  END GTT;

BEGIN
  lg:=i.type;
  IF i.node^.mode=pcK.guard   THEN i.node:=i.node^.l END;  (* skip last guard *)
  IF i.type^.mode=pcK.pointer THEN
    lg:=lg^.base;
    IF obj^.type^.mode#pcK.pointer THEN pcS.err(59)
    ELSE GTT(i.type^.base,obj^.type^.base)
    END
  ELSIF (i.type^.mode=pcK.record) & (i.node^.mode=pcK.var) & (i.node^.obj^.mode=pcK.varpar) THEN
    GTT(i.type,obj^.type)
  ELSE pcS.err(59)
  END;
  IF guard THEN i.type:=obj^.type ELSE i.type:=pcO.boolean END
END type_test;

(*----------------------------------------------------------------*)

PROCEDURE copy_fn(VAR l,r: ITEM);
  CONST carr = pcK.Forms{pcK.array,pcK.vector};
  VAR s,d: pcK.Form;
BEGIN
  s:=l.type^.mode;
  d:=r.type^.mode;
  IF NOT (d IN pcK.ARRs) THEN pcS.err(50); RETURN END;
  IF s IN pcK.ARRs THEN
    type_cmp(l.type^.base,r.type^.base);
    IF (s IN carr) & (d IN carr) THEN
      IF (l.mode=pcK.cons) & char(l.type^.base) THEN
        IF len(l.type)>len(r.type)  THEN pcS.err(incomp) END;
      ELSIF len(l.type)#len(r.type) THEN pcS.err(incomp);
      END;
    END;
  ELSIF (l.mode=pcK.cons) & char(l.type) THEN
    type_equ(r.type^.base,pcK.char); char2str(l);
  ELSE pcS.err(50);
  END;
END copy_fn;

PROCEDURE min_max_fn(VAR i: ITEM; op: pcK.Sub);
BEGIN
  IF i.mode#pcK.type THEN err(i,31) END;
  IF    i.type^.mode=pcK.bitset THEN i.type:=pcO.shortint;
  ELSIF i.type^.mode=pcK.set    THEN i.type:=i.type^.base;
    IF i.type^.mode=pcK.range THEN i.type:=i.type^.base END;
  ELSE type_in(i.type,pcK.SCALARs+pcK.REALs);
  END;
  i.mode:=pcK.cons; unary(i,pcK.Sub(op),i.type)
END min_max_fn;

PROCEDURE size_fn(VAR i: ITEM; op: pcK.Sub);
BEGIN
  IF i.mode IN {pcK.sproc,pcK.sfunc} THEN pcS.err(121) END;
  IF i.type^.mode IN pcK.Forms{pcK.array_of,pcK.dynarr} THEN i.mode:=expr;
  ELSE i.mode:=pcK.cons;
  END;
  unary(i,pcK.Sub(op),pcO.longint);
END size_fn;

PROCEDURE len_fn(VAR i,v: ITEM; one: BOOLEAN; op: pcK.Sub);
  CONST arrays = pcK.Forms{pcK.array,pcK.array_of,pcK.vector};
  VAR type: pcK.STRUCT; dim: LONGINT;
BEGIN
  IF i.mode IN {pcK.inv,pcK.type,pcK.sproc,pcK.sfunc} THEN err(i,121)
  ELSIF NOT (i.type^.mode IN pcK.ARRs) THEN err(i,50)
  END;
  type:=i.type;
  IF NOT one THEN
    IF v.mode#pcK.cons THEN err(v,87); dim:=0 ELSE dim:=v.node^.val END;
    type_equ(v.type,pcK.shortint);
    WHILE (dim>0) & (type^.mode IN arrays) DO
      type:=type^.base; DEC(dim)
    END;
    IF (dim#0) OR NOT (type^.mode IN arrays) THEN pcS.err(61) END;
  END;
  IF type^.mode=pcK.array_of THEN
    WHILE i.node^.mode=pcK.index DO i.node:=i.node^.l END;
  END;
  i.node^.type:=type;
  IF type^.mode=pcK.array THEN
    i.mode:=pcK.cons; type:=type^.inx;
    IF type^.mode=pcK.range THEN type:=type^.base END;
  ELSIF type^.mode=pcK.vector THEN i.mode:=pcK.cons; type:=pcO.longint;
  ELSE i.mode:=expr; type:=pcO.longint;
  END;
  unary(i,op,type);
END len_fn;

PROCEDURE adr_fn(VAR i: ITEM);
BEGIN
  IF i.mode#pcK.var THEN err(i,54) END;
  i.type:=pcO.addr;
  i.mode:=expr; unary(i,pcK.adr,i.type);
END adr_fn;

PROCEDURE ref_fn(VAR i,v: ITEM);
BEGIN
  IF i.mode#pcK.var THEN err(i,54) END;
  IF    v.mode#pcK.type          THEN pcS.err(31);
  ELSIF v.type^.mode#pcK.pointer THEN pcS.err(52);
  ELSE type_cmp(i.type,v.type^.base);
  END;
  i.type:=v.type;
  i.mode:=expr; unary(i,pcK.adr,i.type);
END ref_fn;

PROCEDURE long_fn(VAR i: ITEM);
  VAR type: pcK.STRUCT;
BEGIN
  type:=i.type;
  IF    type^.mode=pcK.shortint  THEN type:=pcO.integer
  ELSIF type^.mode=pcK.shortcard THEN type:=pcO.cardinal
  ELSIF type^.mode=pcK.integer   THEN type:=pcO.longint
  ELSIF type^.mode=pcK.real      THEN type:=pcO.longreal
  ELSE pcS.err(incomp);
  END;
  convert(i,type);
END long_fn;

PROCEDURE short_fn(VAR i: ITEM);
  VAR type: pcK.STRUCT;
BEGIN
  type:=i.type;
  IF    type^.mode=pcK.integer  THEN type:=pcO.shortint
  ELSIF type^.mode=pcK.cardinal THEN type:=pcO.shortcard
  ELSIF type^.mode=pcK.longint  THEN type:=pcO.integer
  ELSIF type^.mode=pcK.longreal THEN type:=pcO.real
  ELSE pcS.err(incomp);
  END;
  convert(i,type);
END short_fn;

PROCEDURE ash_fn(VAR i,v: ITEM);
BEGIN
  type_in(i.type,pcK.WHOLEs);
  IF i.type^.mode<pcK.longint THEN convert(i,pcO.longint) END;
  type_in(v.type,pcK.WHOLEs);
  binary(i,v,pcK.ash,pcO.longint);
END ash_fn;

PROCEDURE rot_fn(VAR i,v: ITEM);
BEGIN pcM.abort;
(*
         IF e1.type^.mode IN pcK.INTs THEN
           IF e1.type^.mode<pcK.longint THEN convert(e1,pcO.longint) END;
         ELSIF NOT (e1.type^.mode IN pcK.Forms{pcK.word,pcK.bitset}) THEN
           pcS.err(incomp); e1.type:=pcO.invtype;
         END;
         type_in(e2.type,pcK.INTs);
         IF symbol=pcS.rol THEN binary(e1,e2,pcK.rol,e1.type);
         ELSE                   binary(e1,e2,pcK.ror,e1.type);
         END;
*)
END rot_fn;

PROCEDURE _eval(x: pcK.NODE; VAR v: LONGINT; VAR e: pcK.EXT);
BEGIN pcM.abort END _eval;

PROCEDURE _lit (x: pcK.NODE); BEGIN pcM.abort END _lit;
PROCEDURE _ini_gen; BEGIN pcM.abort END _ini_gen;

BEGIN
  eval:=_eval;
  literal:=_lit;
  ini_gen:=_ini_gen;
  WORDs:=pcK.Forms{pcK.invtype};
END pcB.
