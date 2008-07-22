IMPLEMENTATION MODULE inVars; (* Sem 27-Feb-91. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT pc  : pcTab;
IMPORT mem : pcSystem;
IMPORT sym : inSym;

IMPORT  tty : Terminal;

FROM pcTab      IMPORT  ref;
FROM inSym      IMPORT  adr_mode;

WITH STORAGE : mem;

CONST
  assert      = 'panic: error in compiler';
  range_error = 'range check error';
  addr_error  = 'ADR: illegal parameter';
  unrealized  = 'unrealized';

VAR
  txt_pos: INTEGER;

PROCEDURE error(l: pc.ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l#NIL)&(l^.md#pc.procedure)&(l^.md#pc.number) THEN txt_pos:=l^.pos END;
  pc.error(txt_pos,TRUE,s,x);
END error;

PROCEDURE new_var(): INTEGER;
BEGIN
  IF vars_no>HIGH(vars) THEN RESIZE(vars,vars_no*2) END;
  WITH vars[vars_no] DO am:=sym.am_G; n:=0; disp:=0; level:=0 END;
  INC(vars_no);
  RETURN vars_no-1;
END new_var;

PROCEDURE new_rng(): INTEGER;
BEGIN
  IF rngs_no>HIGH(rngs) THEN RESIZE(rngs,rngs_no*2) END;
  WITH rngs[rngs_no] DO
    l.am:=sym.am_G; l.n:=0; l.disp:=0; l.level:=0;
    r.am:=sym.am_G; r.n:=0; r.disp:=0; r.level:=0;
    sign:=TRUE; size:=4;
  END;
  INC(rngs_no);
  RETURN rngs_no-1;
END new_rng;

PROCEDURE new_mg(n: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  IF mg_no+n>HIGH(mg)+1 THEN RESIZE(mg,mg_no*2+n) END;
  FOR i:=mg_no TO mg_no+n-1 DO
    mg[i].offset:=0;
    mg[i].size:=0;
    mg[i].name:=-1;
  END;
  INC(mg_no,n);
  RETURN mg_no-n;
END new_mg;

PROCEDURE new_str(n: INTEGER): INTEGER;
  VAR i,f: INTEGER;
BEGIN
  ASSERT(n>0);
  CASE n OF
    |1: f:=0;
    |2: f:=(2-scnt) MOD 2;
  ELSE  f:=(4-scnt) MOD 4;
  END;
  IF scnt+n+f>LEN(scode) THEN RESIZE(scode,scnt+n+f+10000) END;
  FOR i:=scnt TO scnt+n+f-1 DO scode[i]:=0c END;
  INC(scnt,n+f);
  RETURN scnt-n;
END new_str;

PROCEDURE new_mod(): INTEGER;
BEGIN
  IF mdls_no>HIGH(mdls) THEN
    RESIZE(mdls,mdls_no*2); RESIZE(mdls_nm,LEN(mdls));
  END;
  WITH mdls[mdls_no] DO def_time:=-1; ofs:=-1; no:=0 END;
  mdls_nm[mdls_no]:=-1;
  INC(mdls_no);
  RETURN mdls_no-1;
END new_mod;

PROCEDURE new_proc(): INTEGER;
BEGIN
  IF prcs_no>HIGH(prcs) THEN
    RESIZE(prcs,prcs_no*2);
    RESIZE(prcs_nm,prcs_no*2);
  END;
  prcs_nm[prcs_no]:=-1;
  WITH prcs[prcs_no] DO
    mod:=0;
    ofs:=-1;
    lvl:=0;
    no:=0;
    loc_sz:=0;
    export:=FALSE;
    slink:=FALSE;
  END;
  INC(prcs_no);
  RETURN prcs_no-1;
END new_proc;

PROCEDURE dynarr?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & ((l^.md=pc.dynarr) OR (l^.md=pc.packed_dynarr));
END dynarr?;

PROCEDURE array_of?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & ((l^.md=pc.array_of) OR (l^.md=pc.packed_array_of));
END array_of?;

PROCEDURE pointer?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=pc.pointer);
END pointer?;

PROCEDURE string?(l: ref): BOOLEAN;
  VAR r: ref;
BEGIN
  IF l=NIL THEN RETURN FALSE END;
  IF (l^.md#pc.packed_array) &
     (l^.md#pc.packed_dynarr) &
     (l^.md#pc.packed_array_of) THEN RETURN FALSE END;
  r:=l^.r;
  LOOP
    IF r=NIL THEN RETURN FALSE;
    ELSIF r^.md=pc.subtype THEN r:=r^.dw;
    ELSE RETURN r^.md=pc.char;
    END;
  END;
END string?;

PROCEDURE boolean?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=pc.boolean);
END boolean?;

PROCEDURE set?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=pc.set);
END set?;

PROCEDURE sign?(u: pc.ref): BOOLEAN;
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.enumeration    : RETURN rngs[u^.adr].sign;
    |pc.range          : RETURN rngs[u^.adr].sign;
    |pc.subtype        : RETURN rngs[u^.adr].sign;
    |pc.boolean        : RETURN FALSE;
    |pc.set            : RETURN FALSE;
    |pc.char           : RETURN FALSE;
    |pc.integer        : RETURN TRUE;
    |pc.pointer        : RETURN FALSE;
    |pc.profile        : RETURN FALSE;
    |pc.dynarr         : RETURN FALSE;
    |pc.packed_dynarr  : RETURN FALSE;
    |pc.dynarr_desc    : RETURN FALSE;
    |pc.array_of       : RETURN FALSE;
    |pc.packed_array_of: RETURN FALSE;
    |pc.usage          : RETURN sign?(u^.dw);
    |pc.aggregate      : RETURN sign?(u^.dw);
    |pc.number         : RETURN sign?(u^.dw);
    |pc.record         : RETURN FALSE;
    |pc.packed_record  : RETURN FALSE;
    |pc.array          : RETURN FALSE;
    |pc.packed_array   : RETURN FALSE;
    |pc.field          : RETURN sign?(u^.dw);
    |pc.index          : RETURN sign?(u^.dw);
    |pc.deref          : RETURN sign?(u^.dw);
    |pc.high           : RETURN sign?(u^.dw);
    |pc.adr            : RETURN FALSE;
  ELSE error(u,'SIGN: illegal type');
  END;
END sign?;

PROCEDURE vm(l: pc.ref): val_mode;
  PROCEDURE str?(r: pc.ref): val_mode;
  BEGIN
    IF r^.md=pc.char    THEN RETURN vm_string END;
    IF r^.md=pc.subtype THEN RETURN str?(r^.dw) END;
    RETURN vm_undef;
  END str?;
BEGIN
  CASE l^.md OF
    |pc.enumeration    : RETURN vm_integer;
    |pc.char           : RETURN vm_cardinal;
    |pc.boolean        : RETURN vm_boolean;
    |pc.range          : IF rngs[l^.adr].sign THEN RETURN vm_integer
                         ELSE RETURN vm_cardinal
                         END;
    |pc.subtype        : RETURN vm(l^.dw);
    |pc.integer        : RETURN vm_integer;
    |pc.real           : RETURN vm_real;
    |pc.set            : RETURN vm_set;
    |pc.packed_array   : RETURN str?(l^.r);
    |pc.packed_dynarr  : RETURN str?(l^.r);
    |pc.packed_array_of: RETURN str?(l^.r);
    |pc.pointer        : RETURN vm_address;
    |pc.profile        : RETURN vm_cardinal;
  ELSE RETURN vm_undef;
  END;
END vm;

PROCEDURE module_ref_offset(m: INTEGER): INTEGER;
  VAR ofs: INTEGER;
BEGIN
  ofs:=mdls[m].ofs;
  IF ofs<0 THEN ofs:=new_str(2); mdls[m].ofs:=ofs END;
  RETURN ofs;
END module_ref_offset;

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
    |pc.range      : n:=rngs[u^.adr].l.n;
    |pc.subtype    : n:=rngs[u^.adr].l.n;
    |pc.enumeration: n:=rngs[u^.adr].l.n;
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
    |pc.enumeration: n:=rngs[u^.adr].r.n;
    |pc.range      : n:=rngs[u^.adr].r.n;
    |pc.subtype    : n:=rngs[u^.adr].r.n;
    |pc.char       : n:=255;
  ELSE
    error(u,'MAX: illegal type');
  END;
END max;

PROCEDURE record_bytes(l: ref; VAR n: INTEGER);
  PROCEDURE field(f: ref; VAR pos: INTEGER);
    VAR sz: INTEGER;
  BEGIN
    ASSERT(f^.md=pc.var);
    bytes(f^.l,sz);
    ASSERT(sz>=0);
    CASE sz OF
      |0:
      |1:
      |2: pos:=pos+(2-pos) MOD 2;
    ELSE  pos:=pos+(4-pos) MOD 4;
    END;
    INC(pos,sz);
  END field;
  PROCEDURE record_case(c: ref; VAR pos: INTEGER); FORWARD;
  PROCEDURE field_list(l: ref; VAR pos: INTEGER);
  BEGIN
    WHILE l#NIL DO
      IF l^.md=pc.var THEN field(l,pos);
      ELSIF l^.md=pc.case THEN record_case(l,pos);
      END;
      l:=l^.nxt;
    END;
  END field_list;
  PROCEDURE record_case(c: ref; VAR pos: INTEGER);
    VAR max,min: INTEGER; s,v: ref;
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
  IF n>2 THEN n:=n+(4-n) MOD 4 END;
END record_bytes;

PROCEDURE array_bytes(u: pc.ref; VAR n: INTEGER);
  VAR sz: INTEGER;
BEGIN
  n:=rngs[u^.adr].r.n-rngs[u^.adr].l.n+1;
  bytes(u^.r,sz);
  IF sz>2 THEN sz:=sz+(4-sz) MOD 4 END;
  n:=n*sz;
  IF n>2 THEN n:=n+(4-n) MOD 4 END;
END array_bytes;

PROCEDURE set_bytes(u: pc.ref; VAR n: INTEGER);
BEGIN
  n:=rngs[u^.adr].r.n-rngs[u^.adr].l.n+1;
  n:=(n+7) DIV 8;
  IF n>2 THEN n:=n+(4-n) MOD 4 END;
END set_bytes;

PROCEDURE bytes(u: pc.ref; VAR n: INTEGER);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.enumeration    : n:=rngs[u^.adr].size;
    |pc.range          : n:=rngs[u^.adr].size;
    |pc.subtype        : n:=rngs[u^.adr].size;
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

BEGIN
  txt_pos:=0;
END inVars.
