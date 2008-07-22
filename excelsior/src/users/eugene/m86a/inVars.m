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
  VAR i: INTEGER;
BEGIN
  ASSERT(n>=0);
  IF scnt+n>HIGH(scode)+1 THEN RESIZE(scode,scnt*2+n) END;
  FOR i:=scnt TO scnt+n-1 DO scode[i]:=0c END;
  INC(scnt,n);
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

BEGIN
  txt_pos:=0;
END inVars.
