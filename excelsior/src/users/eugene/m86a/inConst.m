IMPLEMENTATION MODULE inConst; (* Sem 26-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

IMPORT  pc  : pcTab;
IMPORT  mem : pcSystem;

IMPORT  vrs : inVars;
IMPORT  sym : inSym;

WITH STORAGE : mem;

CONST
  glo_var_limit = 16;

  assert  = 'error in compiler';

VAR
  txt_pos: INTEGER;
  proc   : pc.ref;
  level  : INTEGER;

PROCEDURE error(l: pc.ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF l#NIL THEN txt_pos:=l^.pos END;
  pc.error(txt_pos,TRUE,s,x);
END error;

PROCEDURE chk(l: pc.ref);
BEGIN
  IF l^.md#pc.number THEN error(l,'must be constant') END;
END chk;

PROCEDURE alloc_var(sz: INTEGER; ind: BOOLEAN): INTEGER;
  VAR n: INTEGER;
BEGIN
  ASSERT(sz>0);
  n:=vrs.new_var();
  IF level=0 THEN
    IF ind THEN
      vrs.vars[n].am   :=sym.am_aG;
      vrs.vars[n].n    :=vrs.new_str(sz);
    ELSE
      vrs.vars[n].am   :=sym.am_G;
      vrs.vars[n].disp :=vrs.new_str(sz);
    END;
  ELSE
    IF ind THEN
      vrs.vars[n].level:=level;
      vrs.vars[n].am   :=sym.am_aL;
      WITH vrs.prcs[proc^.l^.adr] DO
        INC(loc_sz,sz); vrs.vars[n].n:=-loc_sz;
      END;
    ELSE
      vrs.vars[n].level:=level;
      vrs.vars[n].am   :=sym.am_L;
      WITH vrs.prcs[proc^.l^.adr] DO
        INC(loc_sz,sz); vrs.vars[n].disp:=-loc_sz;
      END;
    END;
  END;
  RETURN n;
END alloc_var;

PROCEDURE bytes(u: pc.ref; VAR n: INTEGER); FORWARD;

PROCEDURE min(u: pc.ref; VAR n: INTEGER);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.usage      : min(u^.dw,n);
    |pc.index      : min(u^.dw,n);
    |pc.field      : min(u^.dw,n);
    |pc.deref      : min(u^.dw,n);
    |pc.aggregate  : min(u^.dw,n);
    |pc.boolean    : n:=0;
    |pc.integer    : n:=INTEGER({31});
    |pc.range      : n:=vrs.rngs[u^.adr].l.n;
    |pc.subtype    : n:=vrs.rngs[u^.adr].l.n;
    |pc.enumeration: n:=vrs.rngs[u^.adr].l.n;
    |pc.char       : n:=0;
  ELSE
    error(u,'MIN: illegal type');
  END;
END min;

PROCEDURE max(u: pc.ref; VAR n: INTEGER);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.usage      : max(u^.dw,n);
    |pc.index      : max(u^.dw,n);
    |pc.field      : max(u^.dw,n);
    |pc.deref      : max(u^.dw,n);
    |pc.aggregate  : max(u^.dw,n);
    |pc.boolean    : n:=1;
    |pc.integer    : n:=INTEGER({0..30});
    |pc.enumeration: n:=vrs.rngs[u^.adr].r.n;
    |pc.range      : n:=vrs.rngs[u^.adr].r.n;
    |pc.subtype    : n:=vrs.rngs[u^.adr].r.n;
    |pc.char       : n:=255;
  ELSE
    error(u,'MAX: illegal type');
  END;
END max;

PROCEDURE record_bytes(l: pc.ref; VAR n: INTEGER);
  PROCEDURE field(f: pc.ref; VAR pos: INTEGER);
    VAR sz: INTEGER;
  BEGIN
    ASSERT(f^.md=pc.var); bytes(f^.l,sz); INC(pos,sz);
  END field;
  PROCEDURE record_case(c: pc.ref; VAR pos: INTEGER); FORWARD;
  PROCEDURE field_list(l: pc.ref; VAR pos: INTEGER);
  BEGIN
    WHILE l#NIL DO
      IF l^.md=pc.var THEN field(l,pos);
      ELSIF l^.md=pc.case THEN record_case(l,pos);
      END;
      l:=l^.nxt;
    END;
  END field_list;
  PROCEDURE record_case(c: pc.ref; VAR pos: INTEGER);
    VAR max,min: INTEGER; s,v: pc.ref;
  BEGIN
    IF c^.dw#NIL THEN field(c^.dw,pos) END;
    s:=c^.l; max:=pos; min:=pos;
    WHILE s#NIL DO
      IF s^.md=pc.select THEN field_list(s^.r,pos) END;
      IF pos>max THEN max:=pos END; pos:=min; s:=s^.nxt;
    END;
    field_list(c^.r,pos);
    IF pos<max THEN pos:=max END;
  END record_case;
BEGIN
  n:=0; field_list(l^.dw,n);
END record_bytes;

PROCEDURE array_bytes(u: pc.ref; VAR n: INTEGER);
  VAR sz: INTEGER;
BEGIN
  n:=vrs.rngs[u^.adr].r.n-vrs.rngs[u^.adr].l.n+1;
  bytes(u^.r,sz); n:=n*sz;
END array_bytes;

PROCEDURE set_bytes(u: pc.ref; VAR n: INTEGER);
BEGIN
  n:=vrs.rngs[u^.adr].r.n-vrs.rngs[u^.adr].l.n+1;
  n:=(n+7) DIV 8;
  IF n=3 THEN n:=4 END;
END set_bytes;

PROCEDURE bytes(u: pc.ref; VAR n: INTEGER);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.enumeration    : n:=vrs.rngs[u^.adr].size;
    |pc.range          : n:=vrs.rngs[u^.adr].size;
    |pc.subtype        : n:=vrs.rngs[u^.adr].size;
    |pc.boolean        : n:=1;
    |pc.char           : n:=1;
    |pc.integer        : n:=4;
    |pc.pointer        : n:=4;
    |pc.real           : n:=4;
    |pc.profile        : n:=4;
    |pc.dynarr         : n:=8;
    |pc.packed_dynarr  : n:=8;
    |pc.dynarr_desc    : n:=8;
    |pc.array_of       : n:=8;
    |pc.packed_array_of: n:=8;
    |pc.usage          : bytes(u^.dw,n);
    |pc.aggregate      : bytes(u^.dw,n);
    |pc.number         : bytes(u^.dw,n);
    |pc.record         : record_bytes(u,n);
    |pc.packed_record  : record_bytes(u,n);
    |pc.array          : array_bytes(u,n);
    |pc.packed_array   : array_bytes(u,n);
    |pc.set            : set_bytes(u,n);
    |pc.field          : bytes(u^.dw,n);
    |pc.index          : bytes(u^.dw,n);
    |pc.deref          : bytes(u^.dw,n);
    |pc.high           : bytes(u^.dw,n);
    |pc.adr            : n:=4;
  ELSE bytes(u^.dw,n);
  END;
END bytes;

PROCEDURE gen_var(l: pc.ref);
  VAR n,i: INTEGER;
BEGIN
  bytes(l^.l,n);
  IF  (level#0) OR (n<=glo_var_limit) THEN
    IF n>0FFFFh THEN
      error(l,'local variable with size > 0FFFFh is not allowed');
    END;
    l^.adr:=alloc_var(n,FALSE);
  ELSE  -- level=0
    l^.adr:=alloc_var(4,TRUE); i:=vrs.new_mg(1);
    vrs.mg[i].offset:=vrs.vars[l^.adr].n;
    vrs.mg[i].size:=n;
    vrs.mg[i].name:=l^.nm;
  END;
END gen_var;

PROCEDURE expression(VAR l: pc.ref); FORWARD;

PROCEDURE gen_const(l: pc.ref);
  VAR i,j,k: INTEGER;
BEGIN
  expression(l^.dw);
  IF l^.dw^.md=pc.string THEN
    i:=vrs.new_str(BYTES(l^.dw^.str));
    FOR k:=0 TO HIGH(l^.dw^.str) DO vrs.scode[i+k]:=l^.dw^.str[k] END;
    l^.adr:=vrs.new_var();
    vrs.vars[l^.adr].am:=sym.am_Gstr;
    vrs.vars[l^.adr].disp:=i;
    DISPOSE(l^.dw^.str); DISPOSE(l^.dw);
  ELSIF l^.dw^.md=pc.number THEN
    l^.adr:=vrs.new_var();
    vrs.vars[l^.adr].am:=sym.am_imm;
    vrs.vars[l^.adr].n:=l^.dw^.val;
    DISPOSE(l^.dw);
  ELSIF (l^.dw^.md=pc.usage) & (l^.dw^.r^.md=pc.procedure) THEN
    l^.adr:=vrs.new_var();
    vrs.vars[l^.adr].am:=sym.am_G;
    vrs.vars[l^.adr].disp:=vrs.prcs[l^.dw^.r^.adr].ofs;
    ASSERT(vrs.prcs[l^.adr].ofs>=0);
  ELSE error(l,'must be constant');
  END;
END gen_const;

PROCEDURE alloc_range(fr,to,sz: INTEGER; sg: BOOLEAN): INTEGER;
  VAR i,n: INTEGER;
BEGIN
  n:=vrs.new_rng();
  vrs.rngs[n].sign:=sg;
  vrs.rngs[n].size:=sz;
(*
  IF sz<4 THEN
    i:=vrs.new_str(4);
    vrs.rngs[n].l.am  :=sym.am_Gimm;
    vrs.rngs[n].l.disp:=i;
    vrs.rngs[n].l.n   :=fr;
    vrs.rngs[n].r.am  :=sym.am_Gimm;
    vrs.rngs[n].r.disp:=i+2;
    vrs.rngs[n].r.n   :=to;
    vrs.scode[i+0]:=CHAR(fr);
    vrs.scode[i+1]:=CHAR(fr>>8);
    vrs.scode[i+2]:=CHAR(to);
    vrs.scode[i+3]:=CHAR(to>>8);
  ELSE
*)
    vrs.rngs[n].l.am  :=sym.am_imm;
    vrs.rngs[n].l.n   :=fr;
    vrs.rngs[n].r.am  :=sym.am_imm;
    vrs.rngs[n].r.n   :=to;
(*
  END;
*)
  RETURN n;
END alloc_range;

PROCEDURE gen_array(l: pc.ref);
  VAR fr,to,sz: INTEGER;
BEGIN
  IF l^.l^.md=pc.range THEN l^.adr:=l^.l^.adr;
  ELSIF l^.l^.md=pc.subtype THEN l^.adr:=l^.l^.adr;
  ELSIF l^.l^.md=pc.enumeration THEN l^.adr:=l^.l^.adr;
  ELSE
    min(l^.l,fr); max(l^.l,to); bytes(l^.l,sz);
    l^.adr:=alloc_range(fr,to,sz,vrs.sign?(l^.l));
  END;
END gen_array;

PROCEDURE gen_record(l: pc.ref);
  PROCEDURE field(f: pc.ref; VAR pos: INTEGER);
    VAR sz: INTEGER;
  BEGIN
    ASSERT(f^.md=pc.var);
    f^.adr:=vrs.new_var();
    vrs.vars[f^.adr].am:=sym.am_abs;
    vrs.vars[f^.adr].disp:=pos;
    vrs.vars[f^.adr].n:=0;
    bytes(f^.l,sz); INC(pos,sz);
  END field;
  PROCEDURE record_case(c: pc.ref; VAR pos: INTEGER); FORWARD;
  PROCEDURE field_list(l: pc.ref; VAR pos: INTEGER);
  BEGIN
    WHILE l#NIL DO
      IF l^.md=pc.var THEN field(l,pos);
      ELSIF l^.md=pc.case THEN record_case(l,pos);
      ELSE error(l,assert);
      END;
      l:=l^.nxt;
    END;
  END field_list;
  PROCEDURE record_case(c: pc.ref; VAR pos: INTEGER);
    VAR max,min: INTEGER; s,v: pc.ref;
  BEGIN
    IF c^.dw#NIL THEN field(c^.dw,pos) END;
    s:=c^.l; max:=pos; min:=pos;
    WHILE s#NIL DO
      IF s^.md=pc.select THEN field_list(s^.r,pos)
      ELSE error(s,assert)
      END;
      IF pos>max THEN max:=pos END; pos:=min; s:=s^.nxt;
    END;
    field_list(c^.r,pos);
    IF pos<max THEN pos:=max END;
  END record_case;
  VAR adr: INTEGER;
BEGIN
  adr:=0; field_list(l^.dw,adr);
END gen_record;

PROCEDURE gen_profile(l: pc.ref);
-- вичисляет методы доступа для параметров
  VAR v,i,t: pc.ref; adr,b: INTEGER; var: BOOLEAN;
BEGIN
  v:=NIL; i:=l^.dw;
  WHILE i#NIL DO
    IF i^.md=pc.var THEN i^.r:=v; v:=i END;
    i:=i^.nxt;
  END;
  -- v - список параметров в обратном порядке
  adr:=0;
  WHILE v#NIL DO
    v^.adr:=vrs.new_var();
    t:=v^.l; var:=FALSE;
    LOOP
      IF t=NIL THEN EXIT
      ELSIF t^.md=pc.var_prm THEN t:=t^.dw; var:=TRUE;
      ELSIF t^.md=pc.val_prm THEN t:=t^.dw
      ELSE EXIT
      END;
    END;
    IF vrs.array_of?(t) THEN
      vrs.vars[v^.adr].am:=sym.am_PB8;
      vrs.vars[v^.adr].disp:=adr; INC(adr,8);
    ELSE
      IF var THEN
        vrs.vars[v^.adr].am:=sym.am_aPB;
        vrs.vars[v^.adr].n:=adr; INC(adr,4);
      ELSE
        bytes(v^.l,b);
        IF b<=4 THEN
          IF b<=2 THEN
            vrs.vars[v^.adr].am:=sym.am_PB2;
            vrs.vars[v^.adr].disp:=adr; INC(adr,2);
          ELSE
            vrs.vars[v^.adr].am:=sym.am_PB4;
            vrs.vars[v^.adr].disp:=adr; INC(adr,4);
          END;
        ELSE
          vrs.vars[v^.adr].am:=sym.am_aPB;
          vrs.vars[v^.adr].n:=adr; INC(adr,4);
        END;
      END;
    END;
    vrs.vars[v^.adr].level:=level+1;
    v:=v^.r;
  END;
END gen_profile;

PROCEDURE gen_procedure(l: pc.ref);
BEGIN
  l^.r:=proc^.l; l^.adr:=vrs.new_proc();
  WITH vrs.prcs[l^.adr] DO
    mod:=0; loc_sz:=0;
    WITH vrs.cu DO no:=prc_sz; INC(prc_sz) END;
    lvl:=level+1;
  END;
  vrs.prcs_nm[l^.adr]:=l^.nm;
END gen_procedure;

PROCEDURE gen_code_body(l: pc.ref);
BEGIN
  IF l^.dw^.md#pc.string THEN error(l,assert) END;
  l^.l^.dw:=l;
END gen_code_body;

PROCEDURE gen_range(u: pc.ref);
  VAR sg: BOOLEAN; sz: INTEGER;
  PROCEDURE pack_range(l,r: INTEGER);
  BEGIN
    IF l>r THEN sz:=1; sg:=TRUE; RETURN END;
    IF    (l>=-80h)   & (r<=7Fh)    THEN sz:=1; sg:=TRUE;
    ELSIF (l>=0h)     & (r<=0FFh)   THEN sz:=1; sg:=FALSE;
    ELSIF (l>=-8000h) & (r<=7FFFh)  THEN sz:=2; sg:=TRUE;
    ELSIF (l>=0h)     & (r<=0FFFFh) THEN sz:=2; sg:=FALSE;
    ELSE  sz:=4; sg:=TRUE;
    END;
  END pack_range;
  VAR l,r: INTEGER;
BEGIN
  expression(u^.l); expression(u^.r);
  chk(u^.l); chk(u^.r);
  l:=u^.l^.val; r:=u^.r^.val;
  DISPOSE(u^.l); DISPOSE(u^.r);
  IF u^.md=pc.range THEN pack_range(l,r)
  ELSE sg:=vrs.sign?(u^.dw); bytes(u^.dw,sz);
  END;
  u^.adr:=alloc_range(l,r,sz,sg);
END gen_range;

PROCEDURE gen_enumeration(u: pc.ref);
  PROCEDURE const(l: pc.ref; VAR n: INTEGER);
  BEGIN
    expression(l^.dw); chk(l^.dw);
    l^.adr:=vrs.new_var();
    vrs.vars[l^.adr].am:=sym.am_imm;
    n:=l^.dw^.val;
    vrs.vars[l^.adr].n:=n;
    DISPOSE(l^.dw);
  END const;
  VAR l,r,i,sz: INTEGER; c: pc.ref;
BEGIN
  c:=u^.dw;
  r:=MIN(INTEGER); l:=MAX(INTEGER);
  WHILE c#NIL DO
    IF (c^.md#pc.const) OR (c^.l#u) THEN error(c,assert) END;
    const(c,i);
    IF i<l THEN l:=i END;
    IF i>r THEN r:=i END;
    c:=c^.nxt;
  END;
  IF l>r THEN sz:=1; r:=-1; l:=0;
  ELSIF (l>=-80h) & (r<=7Fh) THEN sz:=1;
  ELSIF (l>=-8000h) & (r<=7FFFh) THEN sz:=2;
  ELSE sz:=4
  END;
  u^.adr:=alloc_range(l,r,sz,TRUE);
END gen_enumeration;

PROCEDURE gen_set(l: pc.ref);
  VAR fr,to: INTEGER; sg: BOOLEAN; sz: INTEGER;
BEGIN
  IF l^.dw=NIL THEN error(l,assert)
  ELSIF l^.dw^.md=pc.range THEN l^.adr:=l^.dw^.adr;
  ELSIF l^.dw^.md=pc.subtype THEN l^.adr:=l^.dw^.adr;
  ELSIF l^.dw^.md=pc.enumeration THEN l^.adr:=l^.dw^.adr;
  ELSE
    min(l^.dw,fr); max(l^.dw,to);
    sg:=vrs.sign?(l^.l); bytes(l^.dw,sz);
    l^.adr:=alloc_range(fr,to,sz,sg);
  END;
END gen_set;

PROCEDURE type(l: pc.ref): pc.ref;
BEGIN
  IF (l^.md>=pc.nil) & (l^.md<=pc.usage) THEN l:=l^.dw
  ELSIF (l^.md>=pc.const_check) & (l^.md<=pc.aggregate) THEN l:=l^.dw
  END;
  RETURN l;
END type;

PROCEDURE flx_arr?(l: pc.ref): BOOLEAN;
-- TRUE если l обозначает объект или тип переменного размера
BEGIN
  l:=type(l);
  RETURN (l^.md=pc.packed_dynarr) OR (l^.md=pc.dynarr) OR
         (l^.md=pc.packed_array_of) OR (l^.md=pc.array_of);
END flx_arr?;

PROCEDURE gen_bits(l: pc.ref);
  VAR n: INTEGER;
BEGIN
  IF flx_arr?(l^.r) THEN
    expression(l^.r);
  ELSE
    bytes(l^.r,n); DISPOSE(l^.r);
    l^.md:=pc.number; l^.val:=n*8;
  END;
END gen_bits;

PROCEDURE gen_size(l: pc.ref);
BEGIN
  IF flx_arr?(l^.r) THEN
    expression(l^.r);
  ELSE
    bytes(l^.r,l^.val); DISPOSE(l^.r);
    l^.md:=pc.number;
  END;
END gen_size;

PROCEDURE gen_high(l: pc.ref);
  VAR n: INTEGER; t: pc.ref;
BEGIN
  IF flx_arr?(l^.r) THEN
    expression(l^.r);
  ELSE
    t:=type(l^.r);
    CASE t^.md OF
      |pc.array          : max(t^.l,n);
      |pc.packed_array   : max(t^.l,n);
    ELSE
      error(l,'HIGH: illegal type');
    END;
    l^.md:=pc.number; l^.val:=n;
  END;
END gen_high;

PROCEDURE gen_low(l: pc.ref);
  VAR n: INTEGER; t: pc.ref;
BEGIN
  t:=type(l^.r);
  CASE t^.md OF
    |pc.array          : min(t^.l,n);
    |pc.packed_array   : min(t^.l,n);
  ELSE n:=0;
  END;
  l^.md:=pc.number; l^.val:=n;
END gen_low;

PROCEDURE gen_len(l: pc.ref);
  VAR n,m: INTEGER; t: pc.ref;
BEGIN
  IF flx_arr?(l^.r) THEN
    expression(l^.r);
  ELSE
    t:=type(l^.r);
    CASE t^.md OF
      |pc.array          : max(t^.l,n); min(t^.l,m);
      |pc.packed_array   : max(t^.l,n); min(t^.l,m);
    ELSE
      error(l,'HIGH: illegal type');
    END;
    l^.md:=pc.number; l^.val:=n-m+1;
  END;
END gen_len;

PROCEDURE gen_abs(l: pc.ref);
BEGIN
  expression(l^.r);
  IF l^.r^.md#pc.number THEN RETURN END;
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer : l^.val:=ABS(l^.r^.val);
    |vrs.vm_cardinal: l^.val:=l^.r^.val;
  ELSE RETURN
  END;
  l^.md:=pc.number;
END gen_abs;

PROCEDURE gen_odd(l: pc.ref);
BEGIN
  expression(l^.r);
  IF l^.r^.md#pc.number THEN RETURN END;
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer    : l^.val:=INTEGER(ODD(l^.r^.val));
    |vrs.vm_cardinal   : l^.val:=INTEGER(ODD(l^.r^.val));
  ELSE RETURN
  END;
  l^.md:=pc.number;
END gen_odd;

PROCEDURE gen_cap(l: pc.ref);
BEGIN
  expression(l^.r);
  IF l^.r^.md#pc.number THEN RETURN END;
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer    : l^.val:=INTEGER(CAP(CHAR(l^.r^.val)));
    |vrs.vm_cardinal   : l^.val:=INTEGER(CAP(CHAR(l^.r^.val)));
  ELSE RETURN
  END;
  l^.md:=pc.number;
END gen_cap;

PROCEDURE gen_compare(l: pc.ref);
  TYPE s=BITSET;
BEGIN
  expression(l^.l); expression(l^.r);
  IF l^.l^.md#pc.number THEN RETURN END;
  IF l^.r^.md#pc.number THEN RETURN END;
  CASE vrs.vm(l^.l^.dw) OF
    |vrs.vm_integer,vrs.vm_cardinal:
      CASE l^.md OF
        |pc.equal        : l^.val:=INTEGER(l^.l^.val=l^.r^.val);
        |pc.inequality   : l^.val:=INTEGER(l^.l^.val#l^.r^.val);
        |pc.less         : l^.val:=INTEGER(l^.l^.val<l^.r^.val);
        |pc.less_equal   : l^.val:=INTEGER(l^.l^.val<=l^.r^.val);
        |pc.greater      : l^.val:=INTEGER(l^.l^.val>l^.r^.val);
        |pc.greater_equal: l^.val:=INTEGER(l^.l^.val>=l^.r^.val);
      END;
    |vrs.vm_set:
      CASE l^.md OF
        |pc.equal        : l^.val:=INTEGER(s(l^.l^.val)=s(l^.r^.val));
        |pc.inequality   : l^.val:=INTEGER(s(l^.l^.val)#s(l^.r^.val));
        |pc.less_equal   : l^.val:=INTEGER(s(l^.l^.val)-s(l^.r^.val)={});
        |pc.greater_equal: l^.val:=INTEGER(s(l^.r^.val)-s(l^.l^.val)={});
      END;
  ELSE RETURN
  END;
  l^.md:=pc.number;
END gen_compare;

PROCEDURE rotate(VAR v: INTEGER; n,sz: INTEGER);
  TYPE s=BITSET; i=INTEGER;
BEGIN
  IF sz=4 THEN v:=v<<n; RETURN END;
  n:=n MOD 64;
  WHILE n#0 DO
    IF sz=1 THEN
      v:=i(s(v)-{8..31})<<1; IF 8 IN s(v) THEN v:=i(s(v)+{0}-{8}) END;
    ELSE
      v:=i(s(v)-{16..31})<<1; IF 16 IN s(v) THEN v:=i(s(v)+{0}-{16}) END;
    END;
  END;
END rotate;

PROCEDURE gen_math(l: pc.ref);
  TYPE s=BITSET; i=INTEGER; VAR fzr,sz: INTEGER;
BEGIN
  IF l^.l=NIL THEN
    expression(l^.r);
    IF l^.r^.md#pc.number THEN RETURN END;
    CASE vrs.vm(l^.dw) OF
      |vrs.vm_integer,vrs.vm_cardinal:
        CASE l^.md OF
          |pc.plus  : l^.val:=l^.r^.val;
          |pc.minus :
            pc.gen_const(l^.pos,pc.minus,pc.integer,0,l^.r^.val,l^.val);
          |pc.trunc :
            pc.gen_const(l^.pos,pc.trunc,pc.real,l^.r^.val,0,l^.val);
        END;
      |vrs.vm_set:
        CASE l^.md OF
          |pc.plus  : l^.val:=l^.r^.val;
          |pc.minus : l^.val:=i(-s(l^.r^.val));
        END;
      |vrs.vm_real:
        CASE l^.md OF
          |pc.plus  : l^.val:=l^.r^.val;
          |pc.minus :
            pc.gen_const(l^.pos,pc.float,pc.real,0,0,fzr);
            pc.gen_const(l^.pos,pc.minus,pc.real,fzr,l^.r^.val,l^.val);
          |pc.float :
            pc.gen_const(l^.pos,pc.float,pc.real,l^.r^.val,0,l^.val);
        END;
      |vrs.vm_boolean:
        CASE l^.md OF
          |pc.not   : l^.val:=INTEGER(l^.r^.val=0);
        END;
    ELSE RETURN
    END;
    l^.md:=pc.number; RETURN;
  END;
  expression(l^.l); expression(l^.r);
  IF l^.l^.md#pc.number THEN RETURN END;
  IF l^.r^.md#pc.number THEN RETURN END;
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer,vrs.vm_cardinal:
      CASE l^.md OF
        |pc.plus  :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.minus :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.star  :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.slash :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.div   :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.mod   :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.rem   :
          pc.gen_const(l^.pos,l^.md,pc.integer,l^.l^.val,l^.r^.val,l^.val);
        |pc.rol   :
          bytes(l^.dw,sz); l^.val:=l^.l^.val; rotate(l^.val,l^.r^.val,sz);
        |pc.ror   :
          bytes(l^.dw,sz); l^.val:=l^.l^.val; rotate(l^.val,-l^.r^.val,sz);
      END;
    |vrs.vm_set:
      CASE l^.md OF
        |pc.plus  : l^.val:=i(s(l^.l^.val)+s(l^.r^.val));
        |pc.minus : l^.val:=i(s(l^.l^.val)-s(l^.r^.val));
        |pc.star  : l^.val:=i(s(l^.l^.val)*s(l^.r^.val));
        |pc.slash : l^.val:=i(s(l^.l^.val)/s(l^.r^.val));
        |pc.rol   :
          bytes(l^.dw,sz); l^.val:=l^.l^.val; rotate(l^.val,l^.r^.val,sz);
        |pc.ror   :
          bytes(l^.dw,sz); l^.val:=l^.l^.val; rotate(l^.val,-l^.r^.val,sz);
      END;
    |vrs.vm_real:
      CASE l^.md OF
        |pc.plus  :
          pc.gen_const(l^.pos,l^.md,pc.real,l^.l^.val,l^.r^.val,l^.val);
        |pc.minus :
          pc.gen_const(l^.pos,l^.md,pc.real,l^.l^.val,l^.r^.val,l^.val);
        |pc.star  :
          pc.gen_const(l^.pos,l^.md,pc.real,l^.l^.val,l^.r^.val,l^.val);
        |pc.slash :
          pc.gen_const(l^.pos,l^.md,pc.real,l^.l^.val,l^.r^.val,l^.val);
        |pc.rol   : l^.val:=l^.l^.val << l^.r^.val;
        |pc.ror   : l^.val:=l^.l^.val >> l^.r^.val;
      END;
  ELSE RETURN
  END;
  l^.md:=pc.number;
END gen_math;

PROCEDURE gen_in(l: pc.ref);
BEGIN
  expression(l^.l); expression(l^.r);
  IF l^.l^.md#pc.number THEN RETURN END;
  IF l^.r^.md#pc.number THEN RETURN END;
  l^.val:=INTEGER(l^.l^.val IN BITSET(l^.r^.val));
  l^.md:=pc.number;
END gen_in;

PROCEDURE gen_worder(l: pc.ref);
BEGIN
  expression(l^.r);
  IF l^.r^.md#pc.number THEN RETURN END;
  l^.val:=l^.r^.val; l^.md:=pc.number;
END gen_worder;

PROCEDURE set_aggregate(l: pc.ref; sz: INTEGER);
  VAR s: BITSET; fr,to,i,j: INTEGER; e: pc.ref;
BEGIN
  IF sz>4 THEN error(l,assert) END;
  min(l^.l^.dw,fr); max(l^.l^.dw,to); s:={};
  e:=l^.r;
  WHILE e#NIL DO
    IF e^.md=pc.range THEN
      expression(e^.l); expression(e^.r);
    ELSE
      expression(e);
    END;
    e:=e^.nxt;
  END;
  e:=l^.r;
  WHILE e#NIL DO
    IF e^.md=pc.range THEN
      IF e^.l^.md#pc.number THEN RETURN END;
      IF e^.r^.md#pc.number THEN RETURN END;
      i:=e^.l^.val; j:=e^.r^.val;
      IF (i<fr) OR (i>to) THEN error(e,'index check error')
      ELSIF (j<fr) OR (j>to) THEN error(e,'index check error')
      ELSIF (i<=j) THEN s:=s+{(i-fr)..(j-fr)}
      END;
    ELSE
      IF e^.md#pc.number THEN RETURN END;
      i:=e^.val;
      IF (i<fr) OR (i>to) THEN error(e,'index check error')
      ELSE s:=s+{i-fr}
      END;
    END;
    e:=e^.nxt;
  END;
  l^.val:=INTEGER(s); l^.md:=pc.number;
END set_aggregate;

PROCEDURE array_aggregate(l: pc.ref; sz: INTEGER);
  VAR
    c: BOOLEAN; i,esz,pos: INTEGER; e: pc.ref;
    bf: RECORD
          CASE : INTEGER OF
          |0: c: ARRAY [0..3] OF CHAR;
          |1: i: INTEGER
          END
        END;
BEGIN
  e:=l^.r; c:=TRUE;
  WHILE e#NIL DO
    expression(e);
    IF (e^.md#pc.number) & (e^.md#pc.string) THEN c:=FALSE END;
    e:=e^.nxt;
  END;
  IF NOT c THEN RETURN END;
  NEW(l^.str,sz);
  e:=l^.r; pos:=0; bytes(l^.l^.r,esz);
  WHILE e#NIL DO
    IF e^.md=pc.number THEN
      FOR i:=0 TO esz-1 DO
        l^.str[pos+i]:=CHAR(e^.val); e^.val:=e^.val >> 8;
      END;
    ELSE
      FOR i:=0 TO esz-1 DO l^.str[pos+i]:=e^.str[i] END;
    END;
    e:=e^.nxt; INC(pos,esz);
  END;
  l^.md:=pc.string;
END array_aggregate;

PROCEDURE gen_aggregate(l: pc.ref);
  VAR sz: INTEGER;
BEGIN
  bytes(l^.l,sz);
  IF sz<=0 THEN RETURN END;
  IF l^.l^.md=pc.set THEN
    set_aggregate(l,sz);
  ELSIF (l^.l^.md=pc.array) OR (l^.l^.md=pc.packed_array) THEN
    array_aggregate(l,sz);
  ELSE error(l,assert);
  END;
END gen_aggregate;

PROCEDURE gen_usage(l: pc.ref; call: BOOLEAN);
  VAR v: pc.ref; ll,i: INTEGER;
BEGIN
  v:=l^.r;
  IF v=NIL THEN error(l,assert) END;
  IF v^.md=pc.var THEN
    ll:=0;
    CASE vrs.vars[v^.adr].am OF
      |sym.am_L   : ll:=vrs.vars[v^.adr].level;
      |sym.am_Limm: ll:=vrs.vars[v^.adr].level;
      |sym.am_aL  : ll:=vrs.vars[v^.adr].level;
      |sym.am_PB2 : ll:=vrs.vars[v^.adr].level;
      |sym.am_PB4 : ll:=vrs.vars[v^.adr].level;
      |sym.am_PB8 : ll:=vrs.vars[v^.adr].level;
      |sym.am_aPB : ll:=vrs.vars[v^.adr].level;
    ELSE
    END;
    IF (ll#0) & (ll#level) THEN
      vrs.prcs[proc^.l^.adr].slink:=TRUE;
      ASSERT(level>1);
    END;
  ELSIF v^.md=pc.const THEN
    CASE vrs.vars[v^.adr].am OF
      |sym.am_imm  : l^.val:=vrs.vars[v^.adr].n;
      |sym.am_Gimm : l^.val:=vrs.vars[v^.adr].n;
      |sym.am_Gstr : RETURN
      |sym.am_G    : RETURN
      |sym.am_Limm : l^.val:=vrs.vars[v^.adr].n;
    ELSE error(v,'must be constant'); RETURN;
    END;
    l^.md:=pc.number;
  ELSIF v^.md=pc.procedure THEN
    IF call & (vrs.prcs[v^.adr].mod=0) THEN
      IF (level>1) & (vrs.prcs[v^.adr].lvl>1) THEN
        vrs.prcs[proc^.l^.adr].slink:=TRUE;
      END;
    ELSIF vrs.prcs[v^.adr].lvl#1 THEN error(v,assert);
    ELSIF vrs.prcs[v^.adr].ofs<0 THEN
      WITH vrs.prcs[v^.adr] DO export:=TRUE; ofs:=vrs.new_str(4) END;
    END;
  ELSIF v^.md=pc.inline THEN
  ELSE error(l,assert);
  END;
END gen_usage;

PROCEDURE gen_prm(l: pc.ref);
BEGIN
  IF l^.md#pc.assign THEN error(l,assert) END;
  expression(l^.r);
END gen_prm;

PROCEDURE gen_range_check(l: pc.ref);
  VAR fr,to: INTEGER;
BEGIN
  expression(l^.r);
  IF l^.r^.md=pc.number THEN
    min(l^.dw,fr); max(l^.dw,to);
    IF (l^.r^.val<fr) OR (l^.r^.val>to) THEN
      error(l,'range check error');  l^.r^.val:=fr;
    END;
    l^.md:=pc.number; l^.val:=l^.r^.val;
  END;
END gen_range_check;

PROCEDURE gen_size_check(l: pc.ref);
BEGIN
  expression(l^.r);
  IF l^.r^.md=pc.number THEN l^.md:=pc.number; l^.val:=l^.r^.val END;
END gen_size_check;

PROCEDURE gen_type_transfer(l: pc.ref);
BEGIN
  expression(l^.r);
  IF l^.r^.md=pc.number THEN l^.md:=pc.number; l^.val:=l^.r^.val END;
END gen_type_transfer;

PROCEDURE expression(VAR l: pc.ref);
  VAR k: pc.ref;
BEGIN
  txt_pos:=l^.pos;
  CASE l^.md OF
    |pc.const_check  : expression(l^.r); chk(l^.r); l:=l^.r;
    |pc.size_check   : gen_size_check(l);
    |pc.type_transfer: gen_type_transfer(l);
    |pc.range_check  : gen_range_check(l);
    |pc.bits         : gen_bits(l);
    |pc.nil          : l^.md:=pc.number; l^.val:=0;
    |pc.number       :
    |pc.string       :
    |pc.high         : gen_high(l);
    |pc.low          : gen_low(l);
    |pc.len          : gen_len(l);
    |pc.min          : min(l^.r,l^.val); l^.md:=pc.number;
    |pc.max          : max(l^.r,l^.val); l^.md:=pc.number;
    |pc.adr          : expression(l^.r);
    |pc.size         : gen_size(l);
    |pc.bytes        : gen_size(l);
    |pc.abs          : gen_abs(l);
    |pc.odd          : gen_odd(l);
    |pc.cap          : gen_cap(l);
    |pc.equal        : gen_compare(l);
    |pc.inequality   : gen_compare(l);
    |pc.less         : gen_compare(l);
    |pc.less_equal   : gen_compare(l);
    |pc.greater      : gen_compare(l);
    |pc.greater_equal: gen_compare(l);
    |pc.in           : gen_in(l);
    |pc.plus         : gen_math(l);
    |pc.minus        : gen_math(l);
    |pc.star         : gen_math(l);
    |pc.slash        : gen_math(l);
    |pc.div          : gen_math(l);
    |pc.mod          : gen_math(l);
    |pc.rem          : gen_math(l);
    |pc.rol          : gen_math(l);
    |pc.ror          : gen_math(l);
    |pc.trunc        : gen_math(l);
    |pc.float        : gen_math(l);
    |pc.not          : gen_math(l);
    |pc.index        : expression(l^.l); expression(l^.r);
    |pc.field        : expression(l^.l);
    |pc.worder       : gen_worder(l);
    |pc.deref        : expression(l^.l);
    |pc.aggregate    : gen_aggregate(l);
    |pc.call         :
      IF l^.l^.md=pc.usage THEN gen_usage(l^.l,TRUE);
      ELSE expression(l^.l);
      END;
      k:=l^.r; WHILE k#NIL DO gen_prm(k); k:=k^.nxt END;
    |pc.usage        : gen_usage(l,FALSE);
  ELSE error(l,assert);
  END;
END expression;

PROCEDURE statement(VAR l: pc.ref); FORWARD;

PROCEDURE gen_case(l: pc.ref);
  VAR k,s,r: pc.ref;
BEGIN
  expression(l^.dw);
  s:=l^.l;
  WHILE s#NIL DO
    IF s^.md#pc.select THEN error(s,assert) END;
    r:=s^.l;
    WHILE r#NIL DO
      IF r^.md#pc.range THEN error(r,assert) END;
      expression(r^.l); chk(r^.l);
      IF r^.r#NIL THEN expression(r^.r); chk(r^.r) END;
      r:=r^.nxt;
    END;
    k:=s^.r; WHILE k#NIL DO statement(k); k:=k^.nxt END;
    s:=s^.nxt;
  END;
  k:=l^.r; WHILE k#NIL DO statement(k); k:=k^.nxt END;
END gen_case;

PROCEDURE gen_for(l: pc.ref);
  VAR m: vrs.val_mode; i: pc.ref;
BEGIN
  IF l^.l^.l^.md#pc.usage THEN error(l^.l^.l,'must be variable') END;
  m:=vrs.vm(l^.l^.l^.dw);
  IF (m#vrs.vm_integer) & (m#vrs.vm_cardinal) THEN error(l,'illegal type') END;
  expression(l^.l^.l);
  expression(l^.l^.r);
  i:=l^.l^.nxt;
  WHILE i#NIL DO
    IF i^.md=pc.exit THEN expression(i^.dw);
    ELSIF i^.md=pc.plus THEN expression(i^.r);
    ELSE error(i,assert);
    END;
    i:=i^.nxt;
  END;
  i:=l^.dw;
  WHILE i#NIL DO statement(i); i:=i^.nxt END;
END gen_for;

PROCEDURE gen_loop(l: pc.ref);
  VAR i: pc.ref;
BEGIN
  IF l^.l#NIL THEN
    IF l^.l^.md=pc.assign THEN
      gen_for(l);
    ELSE
      expression(l^.l);
      i:=l^.dw; WHILE i#NIL DO statement(i); i:=i^.nxt END;
    END;
  ELSIF l^.r#NIL THEN
    i:=l^.dw; WHILE i#NIL DO statement(i); i:=i^.nxt END;
    expression(l^.r);
  ELSE
    i:=l^.dw; WHILE i#NIL DO statement(i); i:=i^.nxt END;
  END;
END gen_loop;

PROCEDURE statement(VAR l: pc.ref);
  VAR k: pc.ref; n: INTEGER;
BEGIN
  txt_pos:=l^.pos;
  CASE l^.md OF
    |pc.module   : k:=l^.dw; WHILE k#NIL DO statement(k); k:=k^.nxt END;
    |pc.proc_body:
      l^.r:=proc; proc:=l; INC(level); l^.adr:=-1;
      k:=l^.dw; WHILE k#NIL DO statement(k); k:=k^.nxt END;
      proc:=l^.r; DEC(level);
    |pc.code_body: gen_code_body(l);
    |pc.block    : k:=l^.dw; WHILE k#NIL DO statement(k); k:=k^.nxt END;
    |pc.assign   : expression(l^.l); expression(l^.r);
    |pc.call     :
      IF l^.l^.md=pc.usage THEN gen_usage(l^.l,TRUE);
      ELSE expression(l^.l);
      END;
      k:=l^.r; WHILE k#NIL DO gen_prm(k); k:=k^.nxt END;
    |pc.if       : expression(l^.dw);
      IF l^.dw^.md=pc.number THEN
        n:=l^.dw^.val; DISPOSE(l^.dw); l^.md:=pc.block;
        IF n=0 THEN l^.dw:=l^.r ELSE l^.dw:=l^.l END;
        k:=l^.dw; WHILE k#NIL DO statement(k); k:=k^.nxt END;
      ELSE
        k:=l^.l; WHILE k#NIL DO statement(k); k:=k^.nxt END;
        k:=l^.r; WHILE k#NIL DO statement(k); k:=k^.nxt END;
      END;
    |pc.case     : gen_case(l);
    |pc.loop     : gen_loop(l);
    |pc.exit     :
    |pc.return   : IF l^.dw#NIL THEN expression(l^.dw) END; l^.nxt:=NIL;
    |pc.incl     : expression(l^.l); expression(l^.r);
    |pc.excl     : expression(l^.l); expression(l^.r);
    |pc.goto     :
    |pc.continue :
    |pc.inc      : expression(l^.l); IF l^.r#NIL THEN expression(l^.r) END;
    |pc.dec      : expression(l^.l); IF l^.r#NIL THEN expression(l^.r) END;
    |pc.program_check:
      expression(l^.l);
      IF l^.l^.md#pc.number THEN error(l,assert) END;
      IF l^.l^.val=0 THEN error(l,'something wrong') END;
      l^.md:=pc.continue;
    |pc.assert   : expression(l^.l); IF l^.r#NIL THEN expression(l^.r) END;
    |pc.halt     : IF l^.r#NIL THEN expression(l^.r) END;
    |pc.new      :
      IF l^.dw^.md=pc.usage THEN gen_usage(l^.dw,TRUE);
      ELSE expression(l^.dw);
      END;
      expression(l^.l);
      IF l^.r#NIL THEN expression(l^.r) END;
    |pc.dispose  :
      IF l^.dw^.md=pc.usage THEN gen_usage(l^.dw,TRUE);
      ELSE expression(l^.dw);
      END;
      expression(l^.l);
    |pc.resize   :
      IF l^.dw^.md=pc.usage THEN gen_usage(l^.dw,TRUE);
      ELSE expression(l^.dw);
      END;
      expression(l^.l); expression(l^.r);
    -----
    |pc.procedure  : gen_procedure(l);
    |pc.inline     :
    |pc.var        : gen_var(l);
    |pc.const      : gen_const(l);
    -- конструкторы типов
    |pc.enumeration: gen_enumeration(l);
    |pc.boolean    :
    |pc.char       :
    |pc.range      : gen_range(l);
    |pc.subtype    : gen_range(l);
    |pc.integer    :
    |pc.real       :
    |pc.set        : gen_set(l);
    |pc.array      : gen_array(l);
    |pc.dynarr     :
    |pc.record     : gen_record(l);
    |pc.packed_array : gen_array(l);
    |pc.packed_dynarr:
    |pc.packed_record: gen_record(l);
    |pc.pointer    :
    |pc.profile    : gen_profile(l);
  ELSE error(l,assert);
  END;
END statement;

PROCEDURE do_const(l: pc.ref);
  VAR k: pc.ref;
BEGIN
  ASSERT(l^.md=pc.proc_body);
  proc:=NIL; level:=-1;
  l^.r:=proc; proc:=l; INC(level); k:=l^.dw;
  WHILE k#NIL DO statement(k); k:=k^.nxt END;
  proc:=l^.r; DEC(level);
END do_const;

BEGIN
  txt_pos:=0;
END inConst.
