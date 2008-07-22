IMPLEMENTATION MODULE mcGen; (* Ned 29-Oct-90. (c) KRONOS *)

IMPORT inter: pcSystem;
IMPORT  scan: mcScan;
IMPORT   obs: mcObj;
IMPORT    pc: pcTab;

WITH STORAGE: inter;

PROCEDURE new(VAR r: pc.ref; m: pc.mode);
BEGIN
  NEW(r);
  r^.nm:=0;
  r^.md :=m;
  r^.nxt:=NIL;
  r^.dw :=NIL;
  r^.l  :=NIL;
  r^.r  :=NIL;
  r^.pos:=scan.col+scan.line*1000h;
  r^.adr:=0;
END new;

PROCEDURE tie(VAR r: pc.ref; m: pc.mode);
  VAR x: pc.ref;
BEGIN
  new(x,m);
  x^.nxt:=r; r:=x;
END tie;

PROCEDURE inverse(VAR p: pc.ref);
  VAR a,b,c: pc.ref;
BEGIN
  b:=p;
  IF b=NIL THEN RETURN END;
  a:=NIL; c:=b^.nxt;
  WHILE c#NIL DO
    b^.nxt:=a;
    a:=b; b:=c; c:=b^.nxt;
  END;
  b^.nxt:=a;
  p:=b;
END inverse;

PROCEDURE number(VAR n: pc.ref; val: INTEGER; type: obs.type_ptr);
BEGIN
  new(n,pc.number);
  n^.val:=val;
  n^.dw:=type^.gen;
END number;

PROCEDURE usage(VAR u: pc.ref; v: obs.obj_ptr);
BEGIN
  new(u,pc.usage);
  u^.r :=v^.gen;
  u^.dw:=v^.type^.gen;
END usage;

PROCEDURE app_valparm(VAR type: pc.ref);
  VAR x: pc.ref;
BEGIN
  IF type^.md#pc.val_prm THEN
    new(x,pc.val_prm);
    x^.dw:=type;
    type:=x;
  END;
END app_valparm;

---------------------------------------------------------------

PROCEDURE range(VAR head: pc.ref; t: obs.type_ptr; l,r: pc.ref);
BEGIN
  IF t^.mode=obs.range THEN tie(head,pc.subtype);
  ELSE                      tie(head,pc.range);
  END;
  t^.gen:=head;
  head^.l:=l;
  head^.r:=r;
  head^.dw:=t^.base^.gen;
  IF (l#NIL) & (r#NIL) THEN
    tie(head,pc.program_check);
    new(head^.l,pc.less_equal);
    head^.l^.dw:=obs.boolp^.gen;
    head^.l^.l:=l;
    head^.l^.r:=r;
  END;
END range;

PROCEDURE enum(VAR head: pc.ref; t: obs.type_ptr);
BEGIN
  tie(head,pc.enumeration);
  t^.gen:=head;
END enum;

PROCEDURE packed(base: obs.type_ptr): BOOLEAN;
BEGIN
  RETURN (base^.mode=obs.char)
      OR (base^.mode=obs.range) & (base^.base^.mode=obs.char)
      OR (ORD('P')-ORD('A') IN scan.opts)
END packed;

PROCEDURE array(VAR head: pc.ref; t: obs.type_ptr);
BEGIN
  IF packed(t^.base) THEN tie(head,pc.packed_array);
  ELSE                    tie(head,pc.array);
  END;
  t^.gen:=head;
  t^.gen^.l:=t^.inx^.gen;
  t^.gen^.r:=t^.base^.gen;
END array;

PROCEDURE array_of(VAR head: pc.ref; t: obs.type_ptr);
BEGIN
  IF packed(t^.base) THEN tie(head,pc.packed_array_of);
  ELSE                    tie(head,pc.array_of);
  END;
  t^.gen:=head;
  t^.gen^.r:=t^.base^.gen;
END array_of;

PROCEDURE pointer(VAR head: pc.ref; t: obs.type_ptr);
BEGIN
  tie(head,pc.pointer);
  t^.gen:=head;
  IF t^.base#NIL THEN t^.gen^.dw:=t^.base^.gen END;
END pointer;

PROCEDURE set(VAR head: pc.ref; t: obs.type_ptr);
BEGIN
  tie(head,pc.set);
  t^.gen:=head;
  head^.dw:=t^.base^.gen;
END set;

PROCEDURE dynarr(VAR head: pc.ref; t: obs.type_ptr);
  VAR l: obs.obj_ptr;
BEGIN
  IF packed(t^.base) THEN tie(head,pc.packed_dynarr);
  ELSE                    tie(head,pc.dynarr);
  END;
  t^.gen:=head;
  head^.r:=t^.base^.gen;
  new(t^.desc^.gen,pc.dynarr_desc);
  t^.desc^.gen^.dw:=t^.gen;
END dynarr;

PROCEDURE proctype(VAR head: pc.ref; t: obs.type_ptr);
BEGIN
  tie(head,pc.profile);
  t^.gen:=head;
END proctype;

PROCEDURE hidden(VAR head: pc.ref; t: obs.type_ptr);

  PROCEDURE bits(VAR x: pc.ref; t: obs.type_ptr);
  BEGIN
    new(x,pc.bits);
    x^.dw:=obs.intp^.gen;
    new(x^.r,pc.usage);
    x^.r^.dw:=obs.intp^.gen;
    x^.r^.r :=t^.gen;
  END bits;

BEGIN
  tie(head,pc.assert);
  new(head^.l,pc.equal);
  head^.l^.dw:=obs.intp^.gen;
  bits(head^.l^.l,obs.addrp);
  bits(head^.l^.r,t);
END hidden;

PROCEDURE bitset(VAR head: pc.ref; t: obs.type_ptr);
  VAR l,r: pc.ref;
BEGIN
  number(l,0,obs.intp);
  new(r,pc.minus);
  r^.dw:=obs.intp^.gen;
  number(r^.r,1,obs.intp);
  new(r^.l,pc.bits);
  r^.l^.dw:=obs.intp^.gen;
  usage(r^.l^.r,obs.intp^.obj);
  tie(head,pc.subtype);
  head^.l:=l; head^.r:=r; head^.dw:=obs.intp^.gen;
  t^.base^.gen:=head;
  r:=head;
  tie(head,pc.set);
  head^.dw:=r;
  t^.gen:=head;
END bitset;

PROCEDURE const(VAR head: pc.ref; o: obs.obj_ptr; val: pc.ref);
BEGIN
  tie(head,pc.const);
  o^.gen:=head;
  head^.l:=o^.type^.gen;
  head^.dw:=val;
  head^.nm:=o^.id;
END const;

PROCEDURE param(VAR head: pc.ref; p: obs.obj_ptr);
  VAR type,x: pc.ref;
BEGIN
  type:=p^.type^.gen;
  IF p^.tags*{obs.seqpar,obs.RO}#{} THEN
    new(x,pc.val_prm); x^.dw:=type; type:=x;
  ELSIF obs.varpar IN p^.tags THEN
    new(x,pc.var_prm); x^.dw:=type; type:=x;
  END;
  tie(head,pc.var);
  head^.l:=type;
  p^.gen:=head;
  head^.nm:=p^.id;
END param;

PROCEDURE var(VAR head: pc.ref; o: obs.obj_ptr);
BEGIN
  tie(head,pc.var);
  o^.gen:=head;
  head^.l:=o^.type^.gen;
  head^.nm:=o^.id;
END var;

PROCEDURE proc(VAR head: pc.ref; o: obs.obj_ptr);
BEGIN
  tie(head,pc.procedure);
  o^.gen:=head;
  head^.l:=o^.type^.gen;
  head^.nm:=o^.id;
END proc;

PROCEDURE module(VAR head: pc.ref; o: obs.obj_ptr);
BEGIN
  tie(head,pc.module);
  o^.gen:=head;
  head^.nm:=o^.id;
END module;

END mcGen.
