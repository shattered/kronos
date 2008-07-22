IMPLEMENTATION MODULE nsDesig; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT sym : nsSym;
IMPORT put : nsCmd;
IMPORT mcd : defCodes;

FROM pcTab      IMPORT  ref;
FROM nsSym      IMPORT  adr_mode, index_mode;

WITH STORAGE : mem;

CONST
  assert      = 'panic: error in compiler';
  range_error = 'range check error';
  addr_error  = 'ADR: illegal parameter';

VAR
  const  : sym.access;
  reg    : sym.access;
  aFP    : sym.access;
  aaFP   : sym.access;
  aRG    : sym.access;
  txt_pos: INTEGER;


PROCEDURE move(a,b: ADDRESS; s: INTEGER); CODE mcd.move END move;
PROCEDURE bblt(a: ADDRESS; a1: INTEGER;
               b: ADDRESS; b1: INTEGER; sz: INTEGER); CODE mcd.bblt END bblt;

PROCEDURE error(l: ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l=NIL) OR (l^.md=tbl.procedure) OR (l^.md=tbl.number) THEN
    tbl.error(txt_pos DIV 1000h,txt_pos MOD 1000h,s,x);
  ELSE
    tbl.error(l^.pos DIV 1000h,l^.pos MOD 1000h,s,x);
  END;
  HALT(1);
END error;

PROCEDURE new_var(): INTEGER;
BEGIN
  IF vars_no>HIGH(vars) THEN RESIZE(vars,vars_no*2) END;
  WITH vars[vars_no] DO
    am:=sym.am_aSB; xm:=xm_off; rg:=0; rg_x:=0;
    n:=0; disp:=0; level:=0;
  END;
  INC(vars_no);
  RETURN vars_no-1;
END new_var;

PROCEDURE new_rng(): INTEGER;
BEGIN
  IF rngs_no>HIGH(rngs) THEN RESIZE(rngs,rngs_no*2) END;
  WITH rngs[rngs_no] DO
    l.am:=sym.am_imm; l.xm:=xm_off; l.rg:=0; l.rg_x:=0;
    l.n:=0; l.disp:=0; l.level:=0;
    r.am:=sym.am_imm; r.xm:=xm_off; r.rg:=0; r.rg_x:=0;
    r.n:=0; r.disp:=0; r.level:=0;
    sign:=TRUE; size:=4;
  END;
  INC(rngs_no);
  RETURN rngs_no-1;
END new_rng;

PROCEDURE new_mg(n: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  IF mg_no+n>HIGH(mg)+1 THEN RESIZE(mg,mg_no*2+n) END;
  FOR i:=mg_no TO mg_no+n-1 DO mg[i].offset:=0; mg[i].size:=0 END;
  INC(mg_no,n);
  RETURN mg_no-n;
END new_mg;

PROCEDURE new_str(n: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  ASSERT(n>0);
  IF scnt+n>HIGH(scode)+1 THEN RESIZE(scode,scnt*2+n) END;
  FOR i:=scnt TO scnt+n-1 DO scode[i]:=0 END;
  INC(scnt,n);
  RETURN scnt-n;
END new_str;

PROCEDURE new_mod(): INTEGER;
BEGIN
  IF mdls_no>HIGH(mdls) THEN
    RESIZE(mdls,mdls_no*2); RESIZE(mdls_nm,LEN(mdls));
  END;
  WITH mdls[mdls_no] DO def_time:=-1; offset:=mdls_no END;
  mdls_nm[mdls_no]:=-1;
  INC(mdls_no);
  RETURN mdls_no-1;
END new_mod;

PROCEDURE new_proc(): INTEGER;
BEGIN
  IF prcs_no>HIGH(prcs) THEN RESIZE(prcs,prcs_no*2) END;
  WITH prcs[prcs_no] DO mod:=0; lvl:=0; disp:=0; loc_sz:=0 END;
  INC(prcs_no);
  RETURN prcs_no-1;
END new_proc;

PROCEDURE new_inln(): INTEGER;
BEGIN
  IF inln_no>HIGH(inln) THEN RESIZE(inln,inln_no*2) END;
  NEW(inln[inln_no]);
  INC(inln_no);
  RETURN inln_no-1;
END new_inln;

PROCEDURE alloc_reg(VAR a: sym.access);
  VAR n: INTEGER;
BEGIN
  WITH free_regs DO
    IF stk>=res THEN error(NIL,assert) END;
    n:=stk MOD regs_no; INC(stk);
    IF stk>max THEN max:=stk END;
  END;
  a:=reg; a.rg:=n;
END alloc_reg;

PROCEDURE dealloc_reg(VAL a: sym.access; min: INTEGER);
  VAR n: INTEGER;
BEGIN
  ASSERT(min>=free_regs.lim-regs_no);
  ASSERT(min<=free_regs.lim);
  LOOP
    IF free_regs.stk<=min THEN RETURN END;
    n:=(free_regs.stk-1) MOD regs_no;
    IF (a.xm#xm_off) & (a.rg_x=n) THEN RETURN END;
    IF ((a.am=am_RG) OR (a.am=am_aRG)) & (a.rg=n) THEN RETURN END;
    DEC(free_regs.stk);
  END;
END dealloc_reg;

PROCEDURE save(VAR r: trap);
BEGIN
  r.cnt :=put.cnt;
  r.scnt:=scnt;
  r.vcnt:=vars_no;
  r.pcnt:=prcs_no;
  r.mcnt:=mdls_no;
  r.rcnt:=rngs_no;
  r.mgcnt:=mg_no;
  r.freg :=free_regs;
END save;

PROCEDURE restore(VAL r: trap);
BEGIN
  put.cnt:=r.cnt;
  scnt   :=r.scnt;
  vars_no:=r.vcnt;
  prcs_no:=r.pcnt;
  mdls_no:=r.mcnt;
  rngs_no:=r.rcnt;
  mg_no  :=r.mgcnt;
  free_regs:=r.freg;
END restore;

PROCEDURE const?(VAL a: sym.access): BOOLEAN;
BEGIN
  IF a.xm#xm_off THEN RETURN FALSE END;
  IF a.am=am_imm THEN RETURN TRUE END;
  IF a.am=am_aSBimm THEN RETURN TRUE END;
  IF a.am=am_aFPimm THEN RETURN TRUE END;
  RETURN FALSE;
END const?;

PROCEDURE imm?(VAL a: sym.access);
BEGIN
  IF a.am#am_imm THEN error(NIL,assert) END;
END imm?;

PROCEDURE gb(VAR a: sym.access);
  VAR i: INTEGER;
BEGIN
  IF (a.am=am_aFP) OR (a.am=am_aFPimm) THEN
    IF a.level>=level THEN RETURN END;
    IF a.level=level-1 THEN a.am:=am_aaFP; a.n:=8; a.level:=level; RETURN END;
    alloc_reg(reg); aaFP.n:=8; aaFP.disp:=8;
    put.cmd(put.mov,aaFP,reg,4);
    FOR i:=a.level TO level-3 DO
      aRG.rg:=reg.rg; aRG.disp:=8;
      put.cmd(put.mov,aRG,reg,4);
    END;
    a.am:=am_aRG; a.rg:=reg.rg;
  ELSIF a.am=am_aaFP THEN
    IF a.level>=level THEN RETURN END;
    alloc_reg(reg);
    IF a.level=level-1 THEN
      aaFP.n:=8; aaFP.disp:=a.n;
      put.cmd(put.mov,aaFP,reg,4);
    ELSE
      aaFP.n:=8; aaFP.disp:=8;
      put.cmd(put.mov,aaFP,reg,4);
      FOR i:=a.level TO level-3 DO
        aRG.rg:=reg.rg; aRG.disp:=8;
        put.cmd(put.mov,aRG,reg,4);
      END;
      aRG.rg:=reg.rg; aRG.disp:=a.n;
      put.cmd(put.mov,aRG,reg,4);
    END;
    a.am:=am_aRG; a.rg:=reg.rg;
  END;
END gb;

PROCEDURE integer(VAR a: sym.access; sign: BOOLEAN; sz: INTEGER);
  VAR b: sym.access;
BEGIN
  ASSERT(sz IN {1,2,4});
  IF a.xm#xm_off THEN
    IF sz<4 THEN
      b:=a; alloc_reg(a);
      IF sign THEN put.movx(b,a,sz,4) ELSE put.movz(b,a,sz,4) END;
    END;
    RETURN
  ELSIF a.am=am_aSBimm THEN
    bblt(ADR(a.n),0,ADR(scode),a.disp*8,sz*8);
    a.am:=am_imm; a.disp:=0;
  ELSIF a.am=am_aFPimm THEN
    a.am:=am_imm; a.disp:=0;
  ELSIF a.am#am_imm THEN
    gb(a);
    IF sz<4 THEN
      b:=a; alloc_reg(a);
      IF sign THEN put.movx(b,a,sz,4) ELSE put.movz(b,a,sz,4) END;
    END;
    RETURN;
  END;
  IF sz<4 THEN
    IF sign & ((sz*8-1) IN BITSET(a.n)) THEN
      a.n:=INTEGER(BITSET(a.n)+{sz*8..31})
    ELSE
      a.n:=INTEGER(BITSET(a.n)-{sz*8..31})
    END;
  END;
END integer;

PROCEDURE imm_int?(VAR a: sym.access; sign: BOOLEAN; size: INTEGER): BOOLEAN;
BEGIN
  ASSERT(size IN {1,2,4});
  IF a.am=am_aFPimm THEN
    a.am:=am_imm; a.disp:=0;
  ELSIF a.am=am_aSBimm THEN
    bblt(ADR(a.n),0,ADR(scode),a.disp*8,size*8);
    a.am:=am_imm; a.disp:=0;
  ELSIF a.am#am_imm THEN
    RETURN FALSE
  END;
  IF size<4 THEN
    IF sign & ((size*8-1) IN BITSET(a.n)) THEN
      a.n:=INTEGER(BITSET(a.n)+{size*8..31});
    ELSE
      a.n:=INTEGER(BITSET(a.n)-{size*8..31});
    END;
  END;
  RETURN TRUE;
END imm_int?;

PROCEDURE dynarr?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & ((l^.md=tbl.dynarr) OR (l^.md=tbl.packed_dynarr));
END dynarr?;

PROCEDURE array_of?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & ((l^.md=tbl.array_of) OR (l^.md=tbl.packed_array_of));
END array_of?;

PROCEDURE pointer?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=tbl.pointer);
END pointer?;

PROCEDURE string?(l: ref): BOOLEAN;
  VAR r: ref;
BEGIN
  IF l=NIL THEN RETURN FALSE END;
  IF (l^.md#tbl.packed_array) &
     (l^.md#tbl.packed_dynarr) &
     (l^.md#tbl.packed_array_of) THEN RETURN FALSE END;
  r:=l^.r;
  LOOP
    IF r=NIL THEN RETURN FALSE;
    ELSIF r^.md=tbl.subtype THEN r:=r^.dw;
    ELSE RETURN r^.md=tbl.char;
    END;
  END;
END string?;

PROCEDURE boolean?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=tbl.boolean);
END boolean?;

PROCEDURE set?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=tbl.set);
END set?;

PROCEDURE gb_imm(VAR a: sym.access);
BEGIN
  IF a.am=am_aFPimm THEN a.am:=am_imm; a.disp:=0;
  ELSIF a.am=am_aSBimm THEN
    bblt(ADR(a.n),0,ADR(scode),a.disp*8,32);
    a.am:=am_imm; a.disp:=0;
  ELSIF (a.am=am_aFP) OR (a.am=am_aaFP) THEN gb(a)
  END;
END gb_imm;

PROCEDURE min(u: ref; VAR a: sym.access);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |tbl.usage      : min(u^.dw,a);
    |tbl.index      : min(u^.dw,a);
    |tbl.field      : min(u^.dw,a);
    |tbl.deref      : min(u^.dw,a);
    |tbl.aggregate  : min(u^.dw,a);
    |tbl.boolean    : a:=const; a.n:=0;
    |tbl.integer    : a:=const; a.n:=INTEGER({31});
    |tbl.range      : WITH rngs[u^.adr] DO a:=l; gb_imm(a) END;
    |tbl.subtype    : WITH rngs[u^.adr] DO a:=l; gb_imm(a) END;
    |tbl.enumeration: WITH rngs[u^.adr] DO a:=l; gb_imm(a) END;
    |tbl.char       : a:=const; a.n:=0;
  ELSE
    error(u,'MIN: illegal type');
  END;
END min;

PROCEDURE max(u: ref; VAR a: sym.access);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |tbl.usage      : max(u^.dw,a);
    |tbl.index      : max(u^.dw,a);
    |tbl.field      : max(u^.dw,a);
    |tbl.deref      : max(u^.dw,a);
    |tbl.aggregate  : max(u^.dw,a);
    |tbl.boolean    : a:=const; a.n:=1;
    |tbl.integer    : a:=const; a.n:=INTEGER({0..30});
    |tbl.enumeration: WITH rngs[u^.adr] DO a:=r; gb_imm(a) END;
    |tbl.range      : WITH rngs[u^.adr] DO a:=r; gb_imm(a) END;
    |tbl.subtype    : WITH rngs[u^.adr] DO a:=r; gb_imm(a) END;
    |tbl.char       : a:=const; a.n:=255;
  ELSE
    error(u,'MAX: illegal type');
  END;
END max;

PROCEDURE low(l: ref; VAR a: sym.access);
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.array          : min(l^.l,a);
    |tbl.packed_array   : min(l^.l,a);
    |tbl.dynarr         : a:=const; a.n:=0;
    |tbl.packed_dynarr  : a:=const; a.n:=0;
    |tbl.array_of       : a:=const; a.n:=0;
    |tbl.packed_array_of: a:=const; a.n:=0;
    |tbl.usage          : low(l^.dw,a);
    |tbl.index          : low(l^.dw,a);
    |tbl.field          : low(l^.dw,a);
    |tbl.deref          : low(l^.dw,a);
    |tbl.aggregate      : low(l^.dw,a);
    |tbl.string         : low(l^.dw,a);
  ELSE
    error(l,'LOW: illegal type');
  END;
END low;

PROCEDURE add_offset(VAR a: sym.access; offset: INTEGER);
BEGIN
  IF (a.am=am_RG) OR (a.am=am_imm) OR (a.am=am_TOS) THEN
    error(NIL,assert);
  END;
  INC(a.disp,offset);
END add_offset;

PROCEDURE high(l: ref; VAR a: sym.access);
  PROCEDURE dyn_high;
  BEGIN
    IF dynarr?(l^.dw) OR array_of?(l^.dw) THEN
      designator(l,a); add_offset(a,4);
    ELSE high(l^.dw,a);
    END;
  END dyn_high;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.array          : max(l^.l,a);
    |tbl.packed_array   : max(l^.l,a);
    |tbl.usage          : dyn_high;
    |tbl.index          : dyn_high;
    |tbl.field          : dyn_high;
    |tbl.deref          : dyn_high;
    |tbl.aggregate      : high(l^.dw,a);
    |tbl.string         : high(l^.dw,a);
  ELSE
    error(l,'HIGH: illegal type');
  END;
END high;

PROCEDURE len(u: ref; VAR a: sym.access);
  PROCEDURE dyn_len;
    VAR b: sym.access;
  BEGIN
    IF dynarr?(u^.dw) OR array_of?(u^.dw) THEN
      designator(u,b); add_offset(b,4); alloc_reg(a);
      put.cmd(put.mov,b,a,4); const.n:=1; put.cmd(put.add,const,a,4);
    ELSE len(u^.dw,a);
    END;
  END dyn_len;
  PROCEDURE rng_len;
    VAR fr,to: sym.access;
  BEGIN
    WITH rngs[u^.adr] DO
      fr:=l; to:=r; gb_imm(fr); gb_imm(to);
      integer(fr,sign,size); integer(to,sign,size);
      IF (fr.am=am_imm) & (to.am=am_imm) THEN
        a:=const; a.n:=to.n-fr.n+1;
      ELSIF (fr.am=am_imm) & (to.am=am_RG) THEN
        a:=to; const.n:=fr.n-1; put.cmd(put.sub,const,a,4);
      ELSIF fr.am=am_imm THEN
        alloc_reg(a); put.cmd(put.mov,to,a,4);
        const.n:=fr.n-1; put.cmd(put.sub,const,a,4);
      ELSIF to.am=am_RG THEN
        a:=to; const.n:=1;
        put.cmd(put.sub,fr,a,4); put.cmd(put.add,const,a,4);
      ELSE
        alloc_reg(a); put.cmd(put.mov,to,a,4);
        put.cmd(put.sub,fr,a,4); const.n:=1; put.cmd(put.add,const,a,4);
      END;
    END;
  END rng_len;
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |tbl.boolean        : a:=const; a.n:=2;
    |tbl.char           : a:=const; a.n:=256;
    |tbl.integer        : error(u,'LEN: integer overflow');
    |tbl.enumeration    : rng_len;
    |tbl.range          : rng_len;
    |tbl.subtype        : rng_len;
    |tbl.array          : len(u^.l,a);
    |tbl.packed_array   : len(u^.l,a);
    |tbl.usage          : dyn_len;
    |tbl.index          : dyn_len;
    |tbl.field          : dyn_len;
    |tbl.deref          : dyn_len;
    |tbl.aggregate      : len(u^.dw,a);
    |tbl.string         : len(u^.dw,a);
  ELSE
    error(u,'LEN: illegal type');
  END;
END len;

PROCEDURE record_bytes(l: ref; VAR a: sym.access);
  PROCEDURE field(f: ref; VAR pos: INTEGER);
    VAR sz: sym.access;
  BEGIN
    ASSERT(f^.md=tbl.var);
    bytes(f^.l,sz);
    IF sz.am#am_imm THEN error(f,'record field size must be constant') END;
    INC(pos,sz.n);
  END field;
  PROCEDURE record_case(c: ref; VAR pos: INTEGER); FORWARD;
  PROCEDURE field_list(l: ref; VAR pos: INTEGER);
  BEGIN
    WHILE l#NIL DO
      IF l^.md=tbl.var THEN field(l,pos);
      ELSIF l^.md=tbl.case THEN record_case(l,pos);
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
      IF s^.md=tbl.select THEN field_list(s^.r,pos) END;
      IF pos>max THEN max:=pos END; pos:=min; s:=s^.nxt;
    END;
    field_list(c^.r,pos);
    IF pos<max THEN pos:=max END;
  END record_case;
BEGIN
  a:=const; a.n:=0;
  field_list(l^.dw,a.n);
END record_bytes;

PROCEDURE array_bytes(u: ref; VAR a: sym.access);
  VAR sz: sym.access;
BEGIN
  len(u^.l,a); bytes(u^.r,sz);
  IF a.am=am_imm THEN
    IF sz.am=am_imm THEN a.n:=a.n*sz.n
    ELSIF sz.am=am_RG THEN put.cmd(put.mul,a,sz,4); a:=sz;
    ELSE error(u,assert);
    END;
  ELSIF a.am=am_RG THEN put.cmd(put.mul,sz,a,4);
  ELSE error(u,assert);
  END;
END array_bytes;

PROCEDURE set_bytes(u: ref; VAR a: sym.access);
  VAR sz: sym.access;
BEGIN
  len(u^.dw,a);
  IF a.am=am_imm THEN
    a.n:=(a.n+7) DIV 8;
    IF a.n=3 THEN a.n:=4 END;
  ELSIF a.am=am_RG THEN
    const.n:=7; put.cmd(put.add,const,a,4);
    const.n:=8; put.cmd(put.div,const,a,4);
  ELSE error(u,assert);
  END;
END set_bytes;

PROCEDURE bytes(u: ref; VAR a: sym.access);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |tbl.enumeration    : a:=const; a.n:=rngs[u^.adr].size;
    |tbl.range          : a:=const; a.n:=rngs[u^.adr].size;
    |tbl.subtype        : a:=const; a.n:=rngs[u^.adr].size;
    |tbl.boolean        : a:=const; a.n:=1;
    |tbl.char           : a:=const; a.n:=1;
    |tbl.integer        : a:=const; a.n:=4;
    |tbl.pointer        : a:=const; a.n:=4;
    |tbl.real           : a:=const; a.n:=4;
    |tbl.profile        : a:=const; a.n:=4;
    |tbl.dynarr         : a:=const; a.n:=12;
    |tbl.packed_dynarr  : a:=const; a.n:=12;
    |tbl.dynarr_desc    : a:=const; a.n:=12;
    |tbl.array_of       : a:=const; a.n:=12;
    |tbl.packed_array_of: a:=const; a.n:=12;
    |tbl.usage          : bytes_d(u^.dw,a);
    |tbl.aggregate      : bytes_d(u^.dw,a);
    |tbl.number         : bytes_d(u^.dw,a);
    |tbl.record         : record_bytes(u,a);
    |tbl.packed_record  : record_bytes(u,a);
    |tbl.array          : array_bytes(u,a);
    |tbl.packed_array   : array_bytes(u,a);
    |tbl.set            : set_bytes(u,a);
    |tbl.field          : bytes(u^.dw,a);
    |tbl.index          : bytes(u^.dw,a);
    |tbl.deref          : bytes(u^.dw,a);
    |tbl.high           : bytes(u^.dw,a);
    |tbl.adr            : a:=const; a.n:=4;
  ELSE bytes(u^.dw,a);
  END;
END bytes;

PROCEDURE sign?(u: ref): BOOLEAN;
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |tbl.enumeration    : RETURN rngs[u^.adr].sign;
    |tbl.range          : RETURN rngs[u^.adr].sign;
    |tbl.subtype        : RETURN rngs[u^.adr].sign;
    |tbl.boolean        : RETURN FALSE;
    |tbl.set            : RETURN FALSE;
    |tbl.char           : RETURN FALSE;
    |tbl.integer        : RETURN TRUE;
    |tbl.pointer        : RETURN FALSE;
    |tbl.profile        : RETURN FALSE;
    |tbl.dynarr         : RETURN FALSE;
    |tbl.packed_dynarr  : RETURN FALSE;
    |tbl.dynarr_desc    : RETURN FALSE;
    |tbl.array_of       : RETURN FALSE;
    |tbl.packed_array_of: RETURN FALSE;
    |tbl.usage          : RETURN sign?(u^.dw);
    |tbl.aggregate      : RETURN sign?(u^.dw);
    |tbl.number         : RETURN sign?(u^.dw);
    |tbl.record         : RETURN FALSE;
    |tbl.packed_record  : RETURN FALSE;
    |tbl.array          : RETURN FALSE;
    |tbl.packed_array   : RETURN FALSE;
    |tbl.field          : RETURN sign?(u^.dw);
    |tbl.index          : RETURN sign?(u^.dw);
    |tbl.deref          : RETURN sign?(u^.dw);
    |tbl.high           : RETURN sign?(u^.dw);
    |tbl.adr            : RETURN FALSE;
  ELSE error(u,'SIGN: illegal type');
  END;
END sign?;

PROCEDURE bytes_d(u: ref; VAR a: sym.access);
BEGIN
  bytes(u,a);
  IF a.am=am_imm THEN
    a.n:=INTEGER(BITSET(a.n+3)-{0,1});
  ELSIF a.am=am_RG THEN
    const.n:=3; put.cmd(put.add,const,a,4); put.cmd(put.bic,const,a,4);
  ELSE error(u,assert);
  END;
END bytes_d;

PROCEDURE bytes_w(u: ref; VAR a: sym.access);
BEGIN
  bytes(u,a);
  IF a.am=am_imm THEN
    a.n:=INTEGER(BITSET(a.n+1)-{0});
  ELSIF a.am=am_RG THEN
    const.n:=1; put.cmd(put.add,const,a,4); put.cmd(put.bic,const,a,4);
  ELSE error(u,assert);
  END;
END bytes_w;

PROCEDURE bytes_u(u: ref; VAR a: sym.access);
  VAR b: sym.access;
BEGIN
  CASE u^.md OF
    |tbl.usage,tbl.deref,tbl.field,tbl.index:
      IF dynarr?(u^.dw) OR array_of?(u^.dw) THEN
        designator(u,b); add_offset(b,4);
        alloc_reg(a); put.cmd(put.mov,b,a,4);
        const.n:=1; put.cmd(put.add,const,a,4);
        bytes(u^.r,b); put.cmd(put.mul,b,a,4);
      ELSE bytes(u,a);
      END;
  ELSE bytes(u,a);
  END;
END bytes_u;

PROCEDURE adr(u: ref; VAR a: sym.access);
  VAR d: sym.access;
BEGIN
  designator(u,d);
  IF d.xm=xm_off THEN
    CASE d.am OF
      |am_RG    : error(u,addr_error);
      |am_aRG   : IF d.disp=0 THEN a:=d; a.am:=am_RG; RETURN END;
      |am_aaFP  :
        IF d.disp=0 THEN a:=d; a.am:=am_aFP; a.disp:=d.n; a.n:=0; RETURN END;
      |am_aaSP  :
        IF d.disp=0 THEN a:=d; a.am:=am_aSP; a.disp:=d.n; a.n:=0; RETURN END;
      |am_aaSB  :
        IF d.disp=0 THEN a:=d; a.am:=am_aSB; a.disp:=d.n; a.n:=0; RETURN END;
      |am_imm   : error(u,addr_error);
      |am_abs   : a:=d; a.am:=am_imm; a.n:=a.disp; a.disp:=0; RETURN;
      |am_EXT   :
      |am_TOS   :
      |am_aFP   :
      |am_aSP   :
      |am_aSB   :
      |am_aSBimm:
      |am_aPC   :
    END;
  END;
  alloc_reg(a); put.cmd(put.addr,d,a,4);
END adr;

PROCEDURE adr_u(u: ref; VAR a: sym.access);
BEGIN
  CASE u^.md OF
    |tbl.usage,tbl.field,tbl.index,tbl.deref:
      IF dynarr?(u^.dw) OR array_of?(u^.dw) THEN designator(u,a);
      ELSE adr(u,a);
      END;
  ELSE adr(u,a);
  END;
END adr_u;

PROCEDURE sub_base(VAR x: sym.access; sg: BOOLEAN; size: INTEGER);
BEGIN
  IF (size<4) & NOT sg THEN
    IF size=1 THEN const.n:=80h ELSE const.n:=8000h END;
    IF imm_int?(x,FALSE,size) THEN DEC(x.n,const.n)
    ELSIF x.am=am_RG THEN put.cmd(put.sub,const,x,size)
    ELSE
      alloc_reg(reg); put.cmd(put.mov,x,reg,size);
      put.cmd(put.sub,const,reg,size); x:=reg;
    END;
  END;
END sub_base;

PROCEDURE check_index(u: ref; VAR base,ix: sym.access; VAR dyn: BOOLEAN);
-- in : u,base
-- out: ix
-- вычисляет индекс
-- проверяет вхождение индекса в диапазон границ массива
-- вычетает из индекса нижнюю границу
-- приводит результат к 32-х разрядному беззнаковому целому
  VAR
    x    : sym.access;
    fr,to: sym.access;
    check: BOOLEAN;
    arr  : ref;
BEGIN
  dyn:=FALSE;
  ASSERT(u^.md=tbl.index);
  arr:=u^.l^.dw;
  IF arr=NIL THEN error(u,'must be array')
  ELSIF (arr^.md=tbl.array) OR (arr^.md=tbl.packed_array) THEN
    check:=(u^.r^.md=tbl.range_check);
    IF check THEN expression(u^.r^.r,x) ELSE expression(u^.r,x) END;
    WITH rngs[arr^.adr] DO
      ASSERT(sign);
      sub_base(x,sign?(u^.r^.dw),size);
      fr:=l;
      IF imm_int?(x,TRUE,size) & imm_int?(fr,TRUE,size) THEN
        IF check THEN
          IF x.n<fr.n THEN error(u,range_error) END;
          to:=r;
          IF imm_int?(to,TRUE,size) THEN
            IF x.n>to.n THEN error(u,range_error) END;
          ELSE
            alloc_reg(ix); to:=r; gb(to);
            put.check(to,x,ix.rg,size); put.flag;
          END;
        END;
        DEC(x.n,fr.n); ix:=x;
      ELSE
        IF x.am=am_RG THEN ix:=x
        ELSIF x.am=am_aRG THEN ix:=reg; ix.rg:=x.rg
        ELSE alloc_reg(ix)
        END;
        to:=r; gb(to); put.check(to,x,ix.rg,size);
        IF check THEN put.flag END;
      END;
    END;
  ELSIF (arr^.md=tbl.dynarr) OR (arr^.md=tbl.packed_dynarr) OR
        (arr^.md=tbl.array_of) OR (arr^.md=tbl.packed_array_of) THEN
    dyn:=TRUE;
    check:=(u^.r^.md=tbl.range_check);
    IF check THEN expression(u^.r^.r,x) ELSE expression(u^.r,x) END;
    IF x.am#am_RG THEN alloc_reg(ix) ELSE ix:=x END;
    to:=base; add_offset(to,4); put.check(to,x,ix.rg,4);
    IF check THEN put.flag END;
  ELSE error(u,'must be array');
  END;
END check_index;

PROCEDURE move_to_RG(VAR a: sym.access; sz: INTEGER);
  VAR b: sym.access;
BEGIN
  alloc_reg(b); put.cmd(put.mov,a,b,sz); a:=b;
END move_to_RG;

PROCEDURE index_length(u: ref; VAR len: sym.access; VAR sz: INTEGER);
  VAR fr,to: sym.access;
BEGIN
  ASSERT((u^.md=tbl.array) OR (u^.md=tbl.packed_array));
  WITH rngs[u^.adr] DO
    fr:=l; to:=r;
    integer(fr,sign,size);
    integer(to,sign,size);
    IF fr.am=am_imm THEN
      IF to.am=am_imm THEN len:=to; len.n:=len.n-fr.n;
      ELSIF fr.n=0 THEN gb(to); len:=to;
      ELSIF to.am=am_RG THEN len:=to; put.cmd(put.sub,fr,len,size);
      ELSE
        alloc_reg(len);
        put.cmd(put.mov,to,len,size);
        put.cmd(put.sub,fr,len,size);
      END;
    ELSIF to.am=am_RG THEN
      len:=to; put.cmd(put.sub,fr,len,size);
    ELSE
      alloc_reg(len);
      put.cmd(put.mov,to,len,size);
      put.cmd(put.sub,fr,len,size);
    END;
    IF size<4 THEN sz:=size*2 ELSE sz:=4 END;
  END;
END index_length;

PROCEDURE array_base(l: ref; VAR base,accum: sym.access; VAR dyn: BOOLEAN);
-- вычисляет базу и индекс, индекс приводится к INTEGER
  VAR len,ix: sym.access; sz: INTEGER; dyn1: BOOLEAN;
BEGIN
  ASSERT(l^.md=tbl.index);
  IF (l^.l#NIL) & (l^.l^.md=tbl.index) & (l^.l^.dw#NIL) &
     ((l^.l^.dw^.md=tbl.array) OR (l^.l^.dw^.md=tbl.array)) THEN
    array_base(l^.l,base,accum,dyn);
    check_index(l,const,ix,dyn1); ASSERT(NOT dyn1);
    index_length(l^.l^.dw,len,sz);
    IF (len.am=am_imm) & (accum.am=am_imm) THEN
      integer(len,TRUE,4); accum.n:=accum.n*(len.n+1);
      IF ix.am=am_imm THEN accum.n:=accum.n+ix.n;
      ELSE put.cmd(put.add,accum,ix,4); accum:=ix;
      END;
    ELSIF (len.am=am_imm) & (len.n=0) THEN
      put.cmd(put.add,ix,accum,4);
    ELSE
      IF accum.am#am_RG THEN move_to_RG(accum,4) END;
      put.index(len,ix,accum.rg,sz);
    END;
  ELSE
    designator(l^.l,base); check_index(l,base,accum,dyn);
  END;
END array_base;

PROCEDURE index(i: ref; VAR a: sym.access);
  VAR sz,base,index,b: sym.access; dyn: BOOLEAN;
BEGIN
  ASSERT(i^.md=tbl.index);
  array_base(i,base,index,dyn);
  IF dyn THEN
    IF base.xm#xm_off THEN move_to_RG(base,4) END;
    CASE base.am OF
      |am_RG    : base.am:=am_aRG;
      |am_aRG   : move_to_RG(base,4); base.am:=am_aRG;
      |am_aaFP  : move_to_RG(base,4); base.am:=am_aRG;
      |am_aaSP  : move_to_RG(base,4); base.am:=am_aRG;
      |am_aaSB  : move_to_RG(base,4); base.am:=am_aRG;
      |am_abs   : move_to_RG(base,4); base.am:=am_aRG;
      |am_EXT   : move_to_RG(base,4); base.am:=am_aRG;
      |am_aFP   : base.am:=am_aaFP; base.n:=base.disp;
      |am_aFPimm: base.am:=am_aaFP; base.n:=base.disp;
      |am_aSP   : base.am:=am_aaSP; base.n:=base.disp;
      |am_aSB   : base.am:=am_aaSB; base.n:=base.disp;
      |am_aSBimm: base.am:=am_aaSB; base.n:=base.disp;
      |am_aPC   : move_to_RG(base,4); base.am:=am_aRG;
    END;
    base.disp:=0;
  END;
  bytes(i^.l^.dw^.r,sz);
  IF (sz.am=am_imm) & (index.am=am_imm) THEN
    a:=base; add_offset(a,index.n*sz.n);
  ELSIF (sz.am=am_imm) & (sz.n IN {1,2,4,8}) & (base.xm=xm_off) THEN
    ASSERT(index.am=am_RG);
    a:=base; a.rg_x:=index.rg;
    IF    sz.n=1 THEN a.xm:=xm_b;
    ELSIF sz.n=2 THEN a.xm:=xm_w;
    ELSIF sz.n=4 THEN a.xm:=xm_d;
    ELSIF sz.n=8 THEN a.xm:=xm_q;
    END;
  ELSE
    IF index.am=am_imm THEN
      IF sz.am=am_imm THEN
        index.n:=index.n*sz.n; a:=index;
      ELSE
        alloc_reg(a); put.cmd(put.mov,index,a,4); put.cmd(put.mul,sz,a,4);
      END;
    ELSIF index.am=am_RG THEN
      a:=index; put.cmd(put.mul,sz,a,4);
    ELSE error(i,assert);
    END;
    -- a = (index-low)*sz
    IF (base.am=am_aRG) & (base.xm=xm_off) THEN
      base.am:=am_RG; put.cmd(put.add,a,base,4); a:=base; a.am:=am_aRG;
    ELSIF base.xm=xm_b THEN
      b:=reg; b.rg:=base.rg_x; put.cmd(put.add,a,b,4); a:=base;
    ELSE
      alloc_reg(b); put.cmd(put.addr,base,b,4);
      put.cmd(put.add,a,b,4); a:=b; a.am:=am_aRG;
    END;
  END;
END index;

PROCEDURE field(l: ref; VAR a: sym.access);
  VAR n: INTEGER;
BEGIN
  IF (l^.md#tbl.field) OR
     (l^.r=NIL) OR (l^.r^.md#tbl.var) THEN error(l,assert) END;
  designator(l^.l,a);
  WITH vars[l^.r^.adr] DO
    IF am#am_abs THEN error(l^.r,assert) END;
    add_offset(a,disp);
  END;
END field;

PROCEDURE deref(l: ref; VAR a: sym.access);
BEGIN
  IF dynarr?(l^.l^.dw) THEN
    designator(l^.l,a);
  ELSE
    expression(l^.l,a);
    IF a.xm#xm_off THEN move_to_RG(a,4) END;
    CASE a.am OF
      |am_RG    : a.am:=am_aRG; a.disp:=0;
      |am_imm   : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_aRG   : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
      |am_aaFP  : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
      |am_aaSP  : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
      |am_aaSB  : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
      |am_abs   : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
      |am_EXT   : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
      |am_aFP   : a.am:=am_aaFP; a.n:=a.disp; a.disp:=0;
      |am_aFPimm: a.am:=am_aaFP; a.n:=a.disp; a.disp:=0;
      |am_aSP   : a.am:=am_aaSP; a.n:=a.disp; a.disp:=0;
      |am_aSB   : a.am:=am_aaSB; a.n:=a.disp; a.disp:=0;
      |am_aSBimm: a.am:=am_aaSB; a.n:=a.disp; a.disp:=0;
      |am_aPC   : move_to_RG(a,4); a.am:=am_aRG; a.disp:=0;
    END;
--    IF tbl.chk_nil THEN put.c(mcd.chknil) END;
  END;
END deref;

PROCEDURE string(l: ref; VAR a: sym.access);
  VAR p: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR; i,n: INTEGER;
BEGIN
  i:=(HIGH(l^.str)+4) DIV 4;
  n:=new_str(i); p:=ADR(scode[n]); i:=0;
  WHILE i<=HIGH(l^.str) DO p^[i]:=l^.str[i]; INC(i) END;
  WHILE (i MOD 4)#0 DO p^[i]:=0c; INC(i) END;
  a.am  :=am_aSBimm;
  a.xm  :=xm_off;
  a.rg  :=0;
  a.rg_x:=0;
  a.n   :=0;
  a.disp:=n*4;
  a.level:=0;
END string;

PROCEDURE desig_addr(l: ref; VAR a: sym.access);
BEGIN
  IF NOT dynarr?(l^.r^.dw) THEN error(l,'must be dynarr') END;
  designator(l^.r,a);
END desig_addr;

PROCEDURE desig_high(l: ref; VAR a: sym.access);
BEGIN
  IF NOT dynarr?(l^.r^.dw) THEN error(l,'must be dynarr') END;
  designator(l^.r,a); add_offset(a,4);
END desig_high;

PROCEDURE proc_const(u: ref; VAR a: sym.access);
BEGIN
  error(u,'formal procedures unrealised');
END proc_const;

PROCEDURE usage(l: ref; VAR a: sym.access);
  VAR v: ref;
BEGIN
  v:=l^.r;
  IF v=NIL THEN error(l,assert) END;
  IF v^.md=tbl.var THEN a:=vars[v^.adr];
  ELSIF v^.md=tbl.const THEN a:=vars[v^.adr];
  ELSIF v^.md=tbl.procedure THEN proc_const(v,a);
  ELSE error(l,assert);
  END;
  gb(a);
END usage;

PROCEDURE bitset_aggregate(l: ref; VAR res: sym.access; set_sz: INTEGER);
  VAR min,max,desc: sym.access; rng_sz: INTEGER;
  PROCEDURE gen_bit(VAL ofs,rg,dest: sym.access);
  BEGIN
    ASSERT(rg.am=am_RG);
    gb(desc);
    put.check(desc,ofs,rg.rg,rng_sz); put.flag;
    put.cmd(put.sbit,rg,dest,4);
  END gen_bit;
  PROCEDURE chk_imm(VAL b: sym.access);
  BEGIN
    IF (b.n<min.n) OR (b.n>max.n) THEN error(l,range_error) END;
  END chk_imm;
  VAR
    b,rg : sym.access;
    bl,br: sym.access;
    e    : ref;
    imm  : BITSET;
    e_sg : BOOLEAN;
    e_sz : sym.access;
BEGIN
  WITH rngs[l^.l^.adr] DO
    desc:=r; min:=l; max:=r; rng_sz:=size; ASSERT(sign);
    IF NOT imm_int?(min,TRUE,size) THEN error(NIL,assert) END;
    IF NOT imm_int?(max,TRUE,size) THEN error(NIL,assert) END;
  END;
  res:=const; e:=l^.r; imm:={};
  WHILE e#NIL DO
    IF e^.md=tbl.range THEN
      e_sg:=sign?(e^.l^.dw); bytes(e^.l^.dw,e_sz); imm?(e_sz);
      expression(e^.l,bl); expression(e^.r,br);
      IF imm_int?(bl,e_sg,e_sz.n) & imm_int?(br,e_sg,e_sz.n) THEN
        IF br.n>=bl.n THEN
          chk_imm(bl); chk_imm(br);
          imm:=imm+{(bl.n-min.n) .. (br.n-min.n)};
        END;
      ELSE
        error(e,'{i .. j} - unrealized');
      END;
    ELSE
      e_sg:=sign?(e^.dw); bytes(e^.dw,e_sz); imm?(e_sz);
      expression(e,b);
      IF imm_int?(b,e_sg,e_sz.n) THEN
        imm:=imm+{b.n-min.n};
      ELSIF res.am=am_imm THEN
        IF e_sz.n<rng_sz THEN integer(b,TRUE,rng_sz) END;
        alloc_reg(res); alloc_reg(rg);
        const.n:=INTEGER(imm); put.cmd(put.mov,const,res,set_sz); imm:={};
        gen_bit(b,rg,res);
      ELSE
        IF e_sz.n<rng_sz THEN integer(b,TRUE,rng_sz) END;
        gen_bit(b,rg,res);
      END;
    END;
    e:=e^.nxt;
  END;
  IF res.am=am_imm THEN res.n:=INTEGER(imm)
  ELSIF imm#{} THEN const.n:=INTEGER(imm); put.cmd(put.or,const,res,set_sz);
  END;
END bitset_aggregate;

PROCEDURE const_aggregate(l: ref; adr: ADDRESS; agg_sz: INTEGER): BOOLEAN;
  VAR
    e      : ref;
    pos    : INTEGER;
    rng_sz : INTEGER;
    e_sz,b : sym.access;
    min,max: sym.access;
    desc   : sym.access;
BEGIN
  e:=l^.r;
  IF (l^.l^.md=tbl.packed_array) OR (l^.l^.md=tbl.array) THEN
    bytes(l^.l^.r,e_sz); imm?(e_sz); pos:=0;
    WHILE e#NIL DO
      IF pos>=agg_sz THEN error(l,assert) END;
      expression(e,b);
      IF b.am=am_aSBimm THEN
        bblt(adr,pos*8,ADR(scode),b.disp*8,e_sz.n*8);
      ELSIF (b.am=am_aFPimm) OR (b.am=am_imm) THEN
        bblt(adr,pos*8,ADR(b.n),0,e_sz.n*8);
      ELSE
        RETURN FALSE;
      END;
      e:=e^.nxt; INC(pos,e_sz.n);
    END;
    RETURN TRUE;
  ELSIF l^.l^.md=tbl.set THEN
    WITH rngs[l^.l^.adr] DO
      desc:=r; min:=l; max:=r; rng_sz:=size; ASSERT(sign);
      IF NOT imm_int?(min,TRUE,size) THEN error(NIL,assert) END;
      IF NOT imm_int?(max,TRUE,size) THEN error(NIL,assert) END;
    END;
    ASSERT((min.n-max.n+8) DIV 8 = agg_sz);
    adr^:=0; move(adr+1,adr,(agg_sz+3) DIV 4);
    WHILE e#NIL DO
      expression(e,b);
HALT(1);
      e:=e^.nxt;
    END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END const_aggregate;

PROCEDURE alloc_var(VAR a: sym.access; sz: INTEGER);
BEGIN
  ASSERT(sz>0);
  sz:=(sz+3) DIV 4 * 4;
  a.xm:=xm_off; a.rg:=0; a.rg_x:=0; a.n:=0; a.level:=0;
  IF level=0 THEN
    a.am   :=am_aSB;
    a.disp :=new_str(sz DIV 4)*4;
  ELSE
    a.level:=level;
    a.am   :=am_aFP;
    WITH prcs[proc^.l^.adr] DO INC(loc_sz,sz); a.disp:=-loc_sz END;
  END;
END alloc_var;

PROCEDURE aggregate(l: ref; VAR res: sym.access);
  VAR
    v,e     : ref;
    of,pos  : INTEGER;
    a,ea    : sym.access;
    sz,esz,b: sym.access;
    t       : trap;
BEGIN
  bytes(l^.l,sz);
  IF sz.am#am_imm THEN error(l,'aggregate size must be constant') END;
  IF sz.n<=0 THEN
    -- пустой агрегат должен лежать по абсолютному адресу NIL
    res.am:=am_abs;
    res.xm:=xm_off;
    res.rg:=0;
    res.rg_x:=0;
    res.n :=0;
    res.disp:=0;
    res.level:=0;
    RETURN;
  END;
  IF (sz.n<=4) & (l^.l^.md=tbl.set) THEN
    bitset_aggregate(l,res,sz.n); RETURN
  END;
  save(t);
  pos:=new_str((sz.n+3) DIV 4);
  IF const_aggregate(l,ADR(scode[pos]),sz.n) THEN
    res.am:=am_aSBimm;
    res.xm:=xm_off;
    res.rg:=0;
    res.rg_x:=0;
    res.n :=0;
    res.disp:=pos*4;
    res.level:=0;
    RETURN;
  END;
  restore(t);
  alloc_var(res,sz.n);
  e:=l^.r;
  IF (l^.l^.md=tbl.array) OR (l^.l^.md=tbl.packed_array) THEN
    bytes(l^.l^.r,esz);
    IF esz.am#am_imm THEN error(l,'aggregate size must be constant') END;
    a:=res;
    WHILE e#NIL DO assign(e,a,esz); e:=e^.nxt; add_offset(a,esz.n) END;
(*
  ELSIF l^.l^.md=tbl.set THEN
    convert_to_address(res);
HALT(1);
*)
  ELSE error(l,'unrealized');
  END;
END aggregate;

PROCEDURE designator(u: ref; VAR a: sym.access);
BEGIN
  txt_pos:=u^.pos;
  CASE u^.md OF
    |tbl.string   : string(u,a);
    |tbl.usage    : usage(u,a);
    |tbl.aggregate: aggregate(u,a);
    |tbl.index    : index(u,a);
    |tbl.deref    : deref(u,a);
    |tbl.field    : field(u,a);
    |tbl.var      : a:=vars[u^.adr];
    |tbl.adr      : desig_addr(u,a);
    |tbl.high     : desig_high(u,a);
  ELSE
    error(u,'must be designator');
  END;
END designator;

PROCEDURE designator_u(u: ref; VAR a: sym.access);
BEGIN
  txt_pos:=u^.pos;
  CASE u^.md OF
    |tbl.string   : string(u,a); RETURN;
    |tbl.usage    : usage(u,a);
    |tbl.aggregate: aggregate(u,a);
    |tbl.index    : index(u,a);
    |tbl.deref    : deref(u,a);
    |tbl.field    : field(u,a);
    |tbl.var      : a:=vars[u^.adr];
    |tbl.adr      : desig_addr(u,a); RETURN;
    |tbl.high     : desig_high(u,a); RETURN;
  ELSE
    error(u,'must be designator');
  END;
  IF dynarr?(u^.dw) OR array_of?(u^.dw) THEN
    IF a.xm#xm_off THEN move_to_RG(a,4) END;
    CASE a.am OF
      |am_RG    : a.am:=am_aRG;
      |am_aRG   : move_to_RG(a,4); a.am:=am_aRG;
      |am_aaFP  : move_to_RG(a,4); a.am:=am_aRG;
      |am_aaSP  : move_to_RG(a,4); a.am:=am_aRG;
      |am_aaSB  : move_to_RG(a,4); a.am:=am_aRG;
      |am_abs   : move_to_RG(a,4); a.am:=am_aRG;
      |am_EXT   : move_to_RG(a,4); a.am:=am_aRG;
      |am_aFP   : a.am:=am_aaFP; a.n:=a.disp;
      |am_aFPimm: a.am:=am_aaFP; a.n:=a.disp;
      |am_aSP   : a.am:=am_aaSP; a.n:=a.disp;
      |am_aSB   : a.am:=am_aaSB; a.n:=a.disp;
      |am_aSBimm: a.am:=am_aaSB; a.n:=a.disp;
      |am_aPC   : move_to_RG(a,4); a.am:=am_aRG;
    END;
    a.disp:=0;
  END;
END designator_u;

PROCEDURE assign_halt(e: ref; VAL a,sz: sym.access);
BEGIN
  error(e,assert);
END assign_halt;

BEGIN
  WITH const DO am:=am_imm; xm:=xm_off; rg:=0; rg_x:=0; disp:=0; level:=0 END;
  WITH reg DO am:=am_RG; xm:=xm_off; rg_x:=0; n:=0; disp:=0; level:=0 END;
  WITH aRG DO am:=am_aRG; xm:=xm_off; rg_x:=0; n:=0; disp:=0; level:=0 END;
  WITH aFP DO am:=am_aFP; xm:=xm_off; rg:=0; rg_x:=0; n:=0; level:=0 END;
  WITH aaFP DO am:=am_aaFP; xm:=xm_off; rg:=0; rg_x:=0; disp:=0; level:=0 END;
  expression:=designator;
  assign:=assign_halt;
  level:=0;
  txt_pos:=0;
END nsDesig.
