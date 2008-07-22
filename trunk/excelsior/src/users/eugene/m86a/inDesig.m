IMPLEMENTATION MODULE inDesig; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT pc  : pcTab;
IMPORT mem : pcSystem;
IMPORT sym : inSym;
IMPORT put : inCmd;
IMPORT cmd : inCmds;
IMPORT vrs : inVars;
IMPORT flw : inFlow;
IMPORT eml : inEmul;

FROM pcTab      IMPORT  ref;
FROM inSym      IMPORT  adr_mode;

WITH STORAGE : mem;

TYPE
  am_set = SET OF sym.adr_mode;

CONST
  assert      = 'panic: error in compiler';
  range_error = 'range check error';
  addr_error  = 'ADR: illegal parameter';
  unrealized  = 'unrealized statement';

  on_stack = am_set{sym.am_STK,sym.am_aSTK};
  am_const = am_set{sym.am_imm,sym.am_Gimm,sym.am_Limm};

VAR
  const  : sym.access;
  txt_pos: INTEGER;

PROCEDURE error(l: pc.ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l#NIL)&(l^.md#pc.procedure)&(l^.md#pc.number) THEN txt_pos:=l^.pos END;
  pc.error(txt_pos,TRUE,s,x);
END error;

PROCEDURE imm?(l: pc.ref; VAL a: sym.access);
BEGIN
  IF a.am IN am_const THEN RETURN END;
  error(l,assert);
END imm?;

PROCEDURE save(VAR r: trap);
BEGIN
  r.cnt  :=put.cnt;
  r.scnt :=vrs.scnt;
  r.vcnt :=vrs.vars_no;
  r.pcnt :=vrs.prcs_no;
  r.mcnt :=vrs.mdls_no;
  r.rcnt :=vrs.rngs_no;
  r.mgcnt:=vrs.mg_no;
  r.block:=flw.block;
END save;

PROCEDURE restore(VAL r: trap);
BEGIN
  put.cnt    :=r.cnt;
  vrs.scnt   :=r.scnt;
  vrs.vars_no:=r.vcnt;
  vrs.prcs_no:=r.pcnt;
  vrs.mdls_no:=r.mcnt;
  vrs.rngs_no:=r.rcnt;
  vrs.mg_no  :=r.mgcnt;
  ASSERT(flw.block=r.block);
END restore;

PROCEDURE min(u: ref; VAR a: sym.access);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.usage      : min(u^.dw,a);
    |pc.index      : min(u^.dw,a);
    |pc.field      : min(u^.dw,a);
    |pc.deref      : min(u^.dw,a);
    |pc.aggregate  : min(u^.dw,a);
    |pc.boolean    : a:=const; a.n:=0;
    |pc.integer    : a:=const; a.n:=INTEGER({31});
    |pc.range      : WITH vrs.rngs[u^.adr] DO a:=l END;
    |pc.subtype    : WITH vrs.rngs[u^.adr] DO a:=l END;
    |pc.enumeration: WITH vrs.rngs[u^.adr] DO a:=l END;
    |pc.char       : a:=const; a.n:=0;
  ELSE
    error(u,'MIN: illegal type');
  END;
END min;

PROCEDURE max(u: ref; VAR a: sym.access);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.usage      : max(u^.dw,a);
    |pc.index      : max(u^.dw,a);
    |pc.field      : max(u^.dw,a);
    |pc.deref      : max(u^.dw,a);
    |pc.aggregate  : max(u^.dw,a);
    |pc.boolean    : a:=const; a.n:=1;
    |pc.integer    : a:=const; a.n:=INTEGER({0..30});
    |pc.enumeration: WITH vrs.rngs[u^.adr] DO a:=r END;
    |pc.range      : WITH vrs.rngs[u^.adr] DO a:=r END;
    |pc.subtype    : WITH vrs.rngs[u^.adr] DO a:=r END;
    |pc.char       : a:=const; a.n:=255;
  ELSE
    error(u,'MAX: illegal type');
  END;
END max;

PROCEDURE low(l: ref; VAR a: sym.access);
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |pc.array          : min(l^.l,a);
    |pc.packed_array   : min(l^.l,a);
    |pc.dynarr         : a:=const; a.n:=0;
    |pc.packed_dynarr  : a:=const; a.n:=0;
    |pc.array_of       : a:=const; a.n:=0;
    |pc.packed_array_of: a:=const; a.n:=0;
    |pc.usage          : low(l^.dw,a);
    |pc.index          : low(l^.dw,a);
    |pc.field          : low(l^.dw,a);
    |pc.deref          : low(l^.dw,a);
    |pc.aggregate      : low(l^.dw,a);
    |pc.string         : low(l^.dw,a);
  ELSE
    error(l,'LOW: illegal type');
  END;
END low;

PROCEDURE add_offset(VAR a: sym.access; offset: INTEGER);
BEGIN
  CASE a.am OF
    |am_imm  : error(NIL,assert);
    |am_abs  : a.disp:=a.disp+offset;
    |am_aSTK : a.disp:=a.disp+offset;
    |am_G    : a.disp:=a.disp+offset;
    |am_Gstr : a.disp:=a.disp+offset;
    |am_L    : a.disp:=a.disp+offset;
    |am_Gimm : a.disp:=a.disp+offset; a.am:=am_G;
    |am_Limm : a.disp:=a.disp+offset; a.am:=am_L;
    |am_aG   : a.disp:=a.disp+offset;
    |am_aL   : a.disp:=a.disp+offset;
    |am_PB2  : a.disp:=a.disp+offset;
    |am_PB4  : a.disp:=a.disp+offset;
    |am_PB8  : a.disp:=a.disp+offset;
    |am_aPB  : a.disp:=a.disp+offset;
  END;
END add_offset;

PROCEDURE high(l: ref; VAR a: sym.access);
  PROCEDURE dyn_high;
  BEGIN
    IF vrs.dynarr?(l^.dw) OR vrs.array_of?(l^.dw) THEN
      designator(l,a); add_offset(a,4);
    ELSE high(l^.dw,a);
    END;
  END dyn_high;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |pc.array          : max(l^.l,a);
    |pc.packed_array   : max(l^.l,a);
    |pc.usage          : dyn_high;
    |pc.index          : dyn_high;
    |pc.field          : dyn_high;
    |pc.deref          : dyn_high;
    |pc.aggregate      : high(l^.dw,a);
    |pc.string         : high(l^.dw,a);
  ELSE
    error(l,'HIGH: illegal type');
  END;
END high;

PROCEDURE integer(VAR a: sym.access; sg: BOOLEAN; sz: INTEGER);
BEGIN
  IF a.am IN am_const THEN RETURN END;
  cmd.load(a,sz); a.am:=sym.am_STK; cmd.set_size(4,sg);
END integer;

PROCEDURE len(u: ref; VAR a: sym.access);
  PROCEDURE dyn_len;
    VAR b: sym.access;
  BEGIN
    IF vrs.dynarr?(u^.dw) OR vrs.array_of?(u^.dw) THEN
      designator(u,b); add_offset(b,4); cmd.load(b,4);
      const.n:=1; cmd.cmd_stk_m(cmd.c_add,const); a.am:=am_STK;
    ELSE
      len(u^.dw,a);
    END;
  END dyn_len;
  PROCEDURE rng_len;
    VAR fr,to: sym.access;
  BEGIN
    WITH vrs.rngs[u^.adr] DO
      fr:=l; to:=r; integer(fr,sign,size); integer(to,sign,size);
      IF (fr.am IN am_const) & (to.am IN am_const) THEN
        a:=const; a.n:=to.n-fr.n+1;
      ELSE error(u,'range must be constant');
      END;
    END;
  END rng_len;
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.boolean        : a:=const; a.n:=2;
    |pc.char           : a:=const; a.n:=256;
    |pc.integer        : error(u,'LEN: integer overflow');
    |pc.enumeration    : rng_len;
    |pc.range          : rng_len;
    |pc.subtype        : rng_len;
    |pc.array          : len(u^.l,a);
    |pc.packed_array   : len(u^.l,a);
    |pc.usage          : dyn_len;
    |pc.index          : dyn_len;
    |pc.field          : dyn_len;
    |pc.deref          : dyn_len;
    |pc.aggregate      : len(u^.dw,a);
    |pc.string         : len(u^.dw,a);
  ELSE
    error(u,'LEN: illegal type');
  END;
END len;

PROCEDURE record_bytes(l: ref; VAR a: sym.access);
  PROCEDURE field(f: ref; VAR pos: INTEGER);
    VAR sz: sym.access;
  BEGIN
    ASSERT(f^.md=pc.var);
    bytes(f^.l,sz);
    IF NOT (sz.am IN am_const) THEN
      error(f,'record field size must be constant')
    END;
    INC(pos,sz.n);
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
  a:=const; a.n:=0;
  field_list(l^.dw,a.n);
END record_bytes;

PROCEDURE array_bytes(u: ref; VAR a: sym.access);
  VAR sz: sym.access;
BEGIN
  len(u^.l,a); bytes(u^.r,sz);
  IF a.am=am_imm THEN
    IF sz.am=am_imm THEN a.n:=a.n*sz.n
    ELSIF sz.am=am_STK THEN cmd.imul(u^.pos,a); a.am:=am_STK;
    ELSE error(u,assert);
    END;
  ELSIF a.am=am_STK THEN cmd.imul(u^.pos,sz);
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
  ELSIF a.am=am_STK THEN
    const.n:=7; cmd.cmd_stk_m(cmd.c_add,const);
    const.n:=8; cmd.idiv(u^.pos,const);
  ELSE error(u,assert);
  END;
END set_bytes;

PROCEDURE bytes(u: ref; VAR a: sym.access);
BEGIN
  IF u=NIL THEN error(u,assert) END;
  CASE u^.md OF
    |pc.enumeration    : a:=const; a.n:=vrs.rngs[u^.adr].size;
    |pc.range          : a:=const; a.n:=vrs.rngs[u^.adr].size;
    |pc.subtype        : a:=const; a.n:=vrs.rngs[u^.adr].size;
    |pc.boolean        : a:=const; a.n:=1;
    |pc.char           : a:=const; a.n:=1;
    |pc.integer        : a:=const; a.n:=4;
    |pc.pointer        : a:=const; a.n:=4;
    |pc.real           : a:=const; a.n:=4;
    |pc.profile        : a:=const; a.n:=4;
    |pc.dynarr         : a:=const; a.n:=8;
    |pc.packed_dynarr  : a:=const; a.n:=8;
    |pc.dynarr_desc    : a:=const; a.n:=8;
    |pc.array_of       : a:=const; a.n:=8;
    |pc.packed_array_of: a:=const; a.n:=8;
    |pc.usage          : bytes(u^.dw,a);
    |pc.aggregate      : bytes(u^.dw,a);
    |pc.number         : bytes(u^.dw,a);
    |pc.record         : record_bytes(u,a);
    |pc.packed_record  : record_bytes(u,a);
    |pc.array          : array_bytes(u,a);
    |pc.packed_array   : array_bytes(u,a);
    |pc.set            : set_bytes(u,a);
    |pc.field          : bytes(u^.dw,a);
    |pc.index          : bytes(u^.dw,a);
    |pc.deref          : bytes(u^.dw,a);
    |pc.high           : bytes(u^.dw,a);
    |pc.adr            : a:=const; a.n:=4;
  ELSE bytes(u^.dw,a);
  END;
END bytes;

PROCEDURE bytes_u(u: ref; VAR a: sym.access);
  VAR b: sym.access;
BEGIN
  CASE u^.md OF
    |pc.usage,pc.deref,pc.field,pc.index:
      IF vrs.dynarr?(u^.dw) OR vrs.array_of?(u^.dw) THEN
        designator(u,b); add_offset(b,4); cmd.load(b,4);
        const.n:=1; cmd.cmd_stk_m(cmd.c_add,const);
        bytes(u^.dw^.r,b); cmd.imul(u^.pos,b); a.am:=am_STK;
      ELSE bytes(u,a);
      END;
  ELSE bytes(u,a);
  END;
END bytes_u;

PROCEDURE adr(u: ref; VAR a: sym.access);
BEGIN
  designator(u,a);
  CASE a.am OF
    |am_imm   : error(u,assert);
    |am_abs   : a.am:=am_imm; a.n:=a.disp;
    |am_aSTK  : cmd.load_adr(a); a.am:=am_STK;
    |am_G     : cmd.load_adr(a); a.am:=am_STK;
    |am_Gstr  : cmd.load_adr(a); a.am:=am_STK;
    |am_L     : cmd.load_adr(a); a.am:=am_STK;
    |am_PB2   : cmd.load_adr(a); a.am:=am_STK;
    |am_PB4   : cmd.load_adr(a); a.am:=am_STK;
    |am_PB8   : cmd.load_adr(a); a.am:=am_STK;
    |am_Gimm  : cmd.load_adr(a); a.am:=am_STK;
    |am_Limm  : cmd.load_adr(a); a.am:=am_STK;
    |am_aG    : IF a.disp=0 THEN a.am:=am_G; a.disp:=a.n;
                ELSE cmd.load_adr(a); a.am:=am_STK;
                END;
    |am_aL    : IF a.disp=0 THEN a.am:=am_L; a.disp:=a.n;
                ELSE cmd.load_adr(a); a.am:=am_STK;
                END;
    |am_aPB   : IF a.disp=0 THEN a.am:=am_PB4; a.disp:=a.n;
                ELSE cmd.load_adr(a); a.am:=am_STK;
                END;
  END;
END adr;

PROCEDURE adr_u(u: ref; VAR a: sym.access);
BEGIN
  CASE u^.md OF
    |pc.usage,pc.field,pc.index,pc.deref:
      IF vrs.dynarr?(u^.dw) OR vrs.array_of?(u^.dw) THEN designator(u,a);
      ELSE adr(u,a);
      END;
  ELSE adr(u,a);
  END;
END adr_u;

PROCEDURE check_range(rr: pc.ref);
  VAR i,j: INTEGER;
BEGIN
  WITH vrs.rngs[rr^.adr] DO
    imm?(rr,l); imm?(rr,r);
    cmd.cmd_stk_m(cmd.c_sub,l);
    IF sign THEN put.b(put.jl) ELSE put.b(put.jb) END;
    i:=put.cnt; put.b(0);
    IF (size=4) & (r.n-l.n<10000h) THEN
      put.b(put.jne);
      j:=put.cnt; put.b(0);
      cmd.siz[cmd.stk-1]:=2;
      const.n:=r.n-l.n;
      cmd.cmd_stk_m(cmd.c_cmp,const);
      put.b(put.jbe); put.b(2);
      put.code[j]:=CHAR(put.cnt-j-1);
    ELSIF size=4 THEN
      const.n:=r.n-l.n+1;
      cmd.copt; cmd.cmd_stk_m(cmd.c_sub,const); cmd.drop;
      put.b(put.jb); put.b(2);
    ELSE
      const.n:=r.n-l.n;
      cmd.cmd_stk_m(cmd.c_cmp,const);
      put.b(put.jbe); put.b(2);
    END;
    put.code[i]:=CHAR(put.cnt-i-1);
    put.b(put.int); put.b(4);
  END;
END check_range;

(*
PROCEDURE load_index_G(i,arr: pc.ref; e_sz: INTEGER): BOOLEAN;
BEGIN
  IF (a.am#sym.am_G) OR (a.disp+(r.n-l.n+1)*e_sz>10000h) THEN RETURN FALSE END;
  chk:=i^.r^.md=pc.range_check;
  WITH vrs.rngs[arr^.adr] DO
    IF chk THEN i:=i^.r^.r ELSE i:=i^.r END;
    expression(i^.r^.r,a);
    IF size=1 THEN cmd.load(a,1); a.am:=sym.am_STK; cmd.set_size(2,sign) END;
    IF a.am=sym.am_STK THEN
      DEC(cmd.stk);
      IF cmd.pos[cmd.stk]=cmd.da THEN
        put.b(put.mov_wrm); put.b(put.md_reg+put.AX+put.SI*8);
      ELSIF cmd.pos[cmd.stk]=cmd.bc THEN
        put.b(put.mov_wrm); put.b(put.md_reg+put.CX+put.SI*8);
      ELSE
        put.b(put.pop_si);
        IF size=4 THEN put.b(put.pop_di) END;
      END;
    ELSE
      d.cop:=cmd.c_mov; d.size:=2; d.mode:=m_rm; d.r0:=put.SI;
      cmd.gen_cmd(d,a);
    END;
    WITH vrs.rngs[rr^.adr] DO
      IF chk OR (l.n#0) THEN
        put.b(put.imm_wm); put.b(put.md_reg+put.SI+put.i_sub); put.w(l.n);
      END;
      IF chk THEN
        IF sign THEN put.b(put.jl) ELSE put.b(put.jb) END;
        i:=put.cnt; put.b(0);
        put.b(put.imm_wm); put.b(put.md_reg+put.SI+put.i_cmp); put.w(r.n-l.n);
        put.b(put.jbe); put.b(2);
        put.code[i]:=CHAR(put.cnt-i-1);
        put.b(put.int); put.b(4);
      END;
    END;
    d.
    put.alloc_reg(e_sz);
    IF cmd.pos[cmd.stk]=cmd.da THEN ax:=put.AX; dx:=put.DX;
    ELSE ax:=put.CX; dx:=put.BX;
    END;
    CASE e_sz OF
      |1: put.b(put.mov_brm); put.b(put.
    END;
  END;
END load_index_G;
*)

PROCEDURE index_dynarr(VAL desc: sym.access; expr: ref; expr_sz: INTEGER);
  VAR a,desc1,desc2: sym.access; i,dx: INTEGER;
BEGIN
  ASSERT(desc.am#sym.am_STK);
  IF desc.am=sym.am_aSTK THEN
    alloc_var(a,4); cmd.store(a);
    desc1:=a; desc1.am:=sym.am_aG;
    desc1.n:=desc1.disp; desc1.disp:=0;
  ELSE
    desc1:=desc;
  END;
  desc2:=desc1; add_offset(desc2,4);
  expression(expr,a); cmd.load(a,expr_sz);
  IF cmd.pos[cmd.stk-1]=cmd.da THEN dx:=put.DX ELSE dx:=put.BX END;
  put.b(put.and_wrm); put.b(put.md_reg+dx+dx*8);
  put.b(put.jl); put.b(0); i:=put.cnt;
  cmd.copt; cmd.load(desc2,4);
  cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); cmd.drop;
  put.b(put.jge); put.b(2);
  put.code[i-1]:=CHAR(put.cnt-i);
  put.b(put.int); put.b(4);
  cmd.load(desc1,4); cmd.swap;
END index_dynarr;

PROCEDURE index(i: ref; VAR a: sym.access);
  VAR i_sz,o_sz,e_sz,ax,dx,cx,bx: INTEGER; arr: ref; chk: BOOLEAN;
BEGIN
  ASSERT(i^.md=pc.index);
  arr:=i^.l^.dw;
  bytes(arr^.r,a); imm?(arr,a); e_sz:=a.n; i_sz:=4; o_sz:=4;
  designator(i^.l,a);
  IF (arr^.md=pc.array_of) OR (arr^.md=pc.packed_array_of) THEN
    index_dynarr(a,i^.r,i_sz);
  ELSIF (arr^.md=pc.dynarr) OR (arr^.md=pc.packed_dynarr) THEN
    index_dynarr(a,i^.r,i_sz);
  ELSIF i^.r^.md=pc.number THEN
    add_offset(a,(i^.r^.val-vrs.rngs[arr^.adr].l.n)*e_sz); RETURN
--  ELSIF ld & load_index_G(i,arr,e_sz) THEN a.am:=sym.am_STK; RETURN
  ELSE
    chk:=i^.r^.md=pc.range_check;
    WITH vrs.rngs[arr^.adr] DO
      cmd.load_adr(a);
      IF chk THEN
        expression(i^.r^.r,a); cmd.load(a,size); check_range(arr);
      ELSE
        expression(i^.r,a); cmd.load(a,size);
        IF l.n#0 THEN cmd.cmd_stk_m(cmd.c_sub,l) END;
      END;
      IF (size=4) & (r.n-l.n<=0FFFFh) THEN i_sz:=2 ELSE i_sz:=size END;
      IF (r.n-l.n)*e_sz<=0FFFFh THEN o_sz:=2 ELSE o_sz:=4 END;
    END;
  END;
  IF e_sz IN {2,4,8} THEN
    cmd.set_size(o_sz,FALSE);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=put.AX; dx:=put.DX;
    ELSE ax:=put.CX; dx:=put.BX;
    END;
    REPEAT
      CASE o_sz OF
        |1: put.b(put.shift_bm1); put.b(put.md_reg+ax+put.s_shl);
        |2: put.b(put.shift_wm1); put.b(put.md_reg+ax+put.s_shl);
        |4: put.b(put.shift_wm1); put.b(put.md_reg+ax+put.s_shl);
            put.b(put.shift_wm1); put.b(put.md_reg+dx+put.s_rcl);
      END;
      e_sz:=e_sz>>1;
    UNTIL e_sz=1;
  ELSIF e_sz#1 THEN
    IF i_sz=4 THEN
      const.n:=e_sz; cmd.load(const,4);
      cmd.save; eml.emul_call(i^.pos,'imul4');
      DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
    ELSE
      const.n:=e_sz; cmd.load(const,i_sz); cmd.pop_reg(2);
      IF i_sz=2 THEN put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_mul);
      ELSE           put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_mul);
      END;
      DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
    END;
    cmd.siz[cmd.stk-1]:=o_sz;
  END;
  cmd.add_adr(cmd.top);
  a.am:=am_aSTK;
  a.disp:=0;
END index;

PROCEDURE field(l: ref; VAR a: sym.access);
  VAR n: INTEGER;
BEGIN
  IF l^.md#pc.field THEN error(l,assert) END;
  IF (l^.r=NIL) OR (l^.r^.md#pc.var) THEN error(l,assert) END;
  designator(l^.l,a);
  WITH vrs.vars[l^.r^.adr] DO
    IF am#am_abs THEN error(l^.r,assert) END;
    add_offset(a,disp);
  END;
END field;

PROCEDURE deref(l: ref; VAR a: sym.access);
BEGIN
  IF vrs.dynarr?(l^.l^.dw) THEN
    designator(l^.l,a);
  ELSE
    expression(l^.l,a);
    CASE a.am OF
      |am_imm   : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_Gimm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_Limm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_STK   : a.am:=am_aSTK; a.disp:=0;
      |am_G     : a.am:=am_aG; a.n:=a.disp; a.disp:=0;
      |am_Gstr  : a.am:=am_aG; a.n:=a.disp; a.disp:=0;
      |am_L     : a.am:=am_aL; a.n:=a.disp; a.disp:=0;
      |am_PB2   : a.am:=am_aPB; a.n:=a.disp; a.disp:=0;
      |am_PB4   : a.am:=am_aPB; a.n:=a.disp; a.disp:=0;
      |am_PB8   : a.am:=am_aPB; a.n:=a.disp; a.disp:=0;
    ELSE cmd.load(a,4); a.am:=am_aSTK; a.disp:=0;
    END;
  END;
END deref;

PROCEDURE string(l: ref; VAR a: sym.access);
  VAR p: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR; i,n: INTEGER;
BEGIN
  i:=LEN(l^.str);
  n:=vrs.new_str(i); i:=0;
  a.am:=am_Gstr; a.disp:=n;
  a.n:=0; a.level:=0;
  WHILE i<=HIGH(l^.str) DO vrs.scode[n]:=l^.str[i]; INC(i); INC(n) END;
END string;

PROCEDURE desig_addr(l: ref; VAR a: sym.access);
BEGIN
  IF NOT vrs.dynarr?(l^.r^.dw) THEN error(l,'must be dynarr') END;
  designator(l^.r,a);
END desig_addr;

PROCEDURE desig_high(l: ref; VAR a: sym.access);
BEGIN
  IF NOT vrs.dynarr?(l^.r^.dw) THEN error(l,'must be dynarr') END;
  designator(l^.r,a); add_offset(a,4);
END desig_high;

PROCEDURE proc_const(u: ref; VAR a: sym.access);
BEGIN
  WITH vrs.prcs[u^.adr] DO
    IF ofs<0 THEN error(u,assert) END;
    a.am:=am_G; a.disp:=ofs; a.level:=0;
  END;
END proc_const;

PROCEDURE usage(l: ref; VAR a: sym.access);
  VAR v: ref;
BEGIN
  v:=l^.r;
  IF v=NIL THEN error(l,assert) END;
  IF v^.md=pc.var THEN a:=vrs.vars[v^.adr];
  ELSIF v^.md=pc.const THEN a:=vrs.vars[v^.adr];
  ELSIF v^.md=pc.procedure THEN proc_const(v,a);
  ELSE error(l,assert);
  END;
END usage;

PROCEDURE alloc_var(VAR a: sym.access; sz: INTEGER);
  VAR l: INTEGER;
BEGIN
  ASSERT(sz>0);
  l:=vrs.prcs[cmd.proc^.adr].lvl;
  IF l=0 THEN
    a.am   :=am_G;
    a.disp :=vrs.new_str(sz);
    a.level:=0;
  ELSE
    a.level:=l;
    a.am   :=am_L;
    WITH vrs.prcs[cmd.proc^.adr] DO INC(loc_sz,sz); a.disp:=-loc_sz END;
  END;
END alloc_var;

PROCEDURE make_bitset(sz: INTEGER);
  VAR nsz: INTEGER;
BEGIN
  ASSERT(sz IN {1,2,4});
  nsz:=cmd.siz[cmd.stk-1];
  cmd.alloc_reg(sz); cmd.pop_reg(2);
  IF cmd.pos[cmd.stk-2]=cmd.bc THEN cmd.pos[cmd.stk-2]:=cmd.da;
  ELSE put.b(put.xchg_cx);
  END;
  put.b(put.mov_axi); put.w(1);
  IF sz<4 THEN
    put.b(put.shift_wmc); put.b(put.md_reg+put.AX+put.s_shl);
  ELSE
    IF nsz=1 THEN put.b(put.mov_chi); put.b(0) END;
    put.b(put.mov_dxi); put.w(0);
    put.b(put.jcxz); put.b(7);
    put.b(put.shift_wm1); put.b(put.md_reg+put.AX+put.s_shl);
    put.b(put.shift_wm1); put.b(put.md_reg+put.DX+put.s_rcl);
    put.b(put.dec_cx); put.b(put.jne); put.b(-7);
  END;
  cmd.siz[cmd.stk-2]:=sz; DEC(cmd.stk);
END make_bitset;

PROCEDURE bitset_aggregate(l: ref; VAR res: sym.access; sz: INTEGER);
  VAR fr,to,nsz,i,j: INTEGER; e: pc.ref; a,b: sym.access;
BEGIN
  IF sz>4 THEN error(l,assert) END;
  min(l^.l^.dw,a); imm?(l^.l^.dw,a); fr:=a.n;
  max(l^.l^.dw,a); imm?(l^.l^.dw,a); to:=a.n;
  bytes(l^.l^.dw,a); imm?(l^.l^.dw,a); nsz:=a.n;
  res.am:=sym.am_imm; res.n:=0;
  e:=l^.r;
  WHILE e#NIL DO
    IF e^.md=pc.range THEN
      IF (e^.l^.md=pc.number) & (e^.r^.md=pc.number) THEN
        i:=e^.l^.val; j:=e^.r^.val;
        IF (i<fr) OR (i>to) THEN error(e,range_error)
        ELSIF (j<fr) OR (j>to) THEN error(e,range_error)
        ELSIF i<=j THEN res.n:=INTEGER(BITSET(res.n)+{i-fr..j-fr});
        END;
      ELSE
        expression(e^.l,a); cmd.load(a,nsz); check_range(l^.l);
        expression(e^.r,a); cmd.load(a,nsz); check_range(l^.l);
        cmd.save; eml.emul_call(l^.pos,'setrng');
        DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
        cmd.siz[cmd.stk-1]:=sz;
        IF res.am=sym.am_STK THEN cmd.cmd_stk_m(cmd.c_or,cmd.top);
        ELSE res.am:=sym.am_STK
        END;
      END;
    ELSE
      expression(e,a);
      IF a.am IN am_const THEN
        IF (a.n<fr) OR (a.n>to) THEN error(e,range_error)
        ELSE res.n:=INTEGER(BITSET(res.n)+{i-fr});
        END;
      ELSE
        cmd.load(a,nsz);
        check_range(l^.l);
        make_bitset(sz);
        IF res.am=sym.am_STK THEN cmd.cmd_stk_m(cmd.c_or,cmd.top);
        ELSE res.am:=sym.am_STK
        END;
      END;
    END;
    e:=e^.nxt;
  END;
  IF (res.am=sym.am_STK) & (res.n#0) THEN
    const.n:=res.n; cmd.cmd_stk_m(cmd.c_or,const);
  END;
END bitset_aggregate;

PROCEDURE aggregate(l: ref; VAR res: sym.access);
  VAR
    v,e     : ref;
    of,pos  : INTEGER;
    a,ea    : sym.access;
    sz,esz,b: sym.access;
    t       : trap;
BEGIN
  bytes(l^.l,sz);
  IF NOT (sz.am IN am_const) THEN
    error(l,'aggregate size must be constant')
  END;
  IF sz.n<=0 THEN
    -- пустой агрегат должен лежать по абсолютному адресу NIL
    res.disp:=0; res.am:=am_abs; RETURN;
  END;
  IF (sz.n<=4) & (l^.l^.md=pc.set) THEN
    bitset_aggregate(l,res,sz.n); RETURN
  END;
  alloc_var(res,sz.n);
  e:=l^.r;
  IF (l^.l^.md=pc.array) OR (l^.l^.md=pc.packed_array) THEN
    bytes(l^.l^.r,esz); imm?(l,esz);
    a:=res;
    WHILE e#NIL DO
      expression(e,ea);
      cmd.load_store(a,ea,esz.n);
      e:=e^.nxt;
      add_offset(a,esz.n);
    END;
  ELSE error(l,unrealized);
  END;
END aggregate;

PROCEDURE gen_size_check(l: pc.ref; VAR a: sym.access);
  VAR sz1,sz2: sym.access;
BEGIN
  expression(l^.r,a); bytes_u(l^.r^.dw,sz1); bytes_u(l^.dw,sz2);
  IF NOT ((sz1.am IN am_const) & (sz2.am IN am_const)) THEN
    error(l,'illegal type conversion')
  ELSIF sz1.n#sz2.n THEN
    error(l,'illegal type conversion')
  END;
END gen_size_check;

PROCEDURE designator(u: ref; VAR a: sym.access);
BEGIN
  IF (u^.md#pc.number) & (u^.md#pc.string) THEN txt_pos:=u^.pos END;
  CASE u^.md OF
    |pc.string    : string(u,a);
    |pc.usage     : usage(u,a);
    |pc.aggregate : aggregate(u,a);
    |pc.index     : index(u,a);
    |pc.deref     : deref(u,a);
    |pc.field     : field(u,a);
    |pc.var       : a:=vrs.vars[u^.adr];
    |pc.adr       : desig_addr(u,a);
    |pc.high      : desig_high(u,a);
    |pc.size_check: gen_size_check(u,a);
  ELSE
    error(u,'must be designator');
  END;
END designator;

PROCEDURE designator_u(u: ref; VAR a: sym.access);
BEGIN
  IF (u^.md#pc.number) & (u^.md#pc.string) THEN txt_pos:=u^.pos END;
  CASE u^.md OF
    |pc.string    : string(u,a); RETURN;
    |pc.usage     : usage(u,a);
    |pc.aggregate : aggregate(u,a);
    |pc.index     : index(u,a);
    |pc.deref     : deref(u,a);
    |pc.field     : field(u,a);
    |pc.var       : a:=vrs.vars[u^.adr];
    |pc.adr       : desig_addr(u,a); RETURN;
    |pc.high      : desig_high(u,a); RETURN;
    |pc.size_check: gen_size_check(u,a);
  ELSE
    error(u,'must be designator');
  END;
  IF vrs.dynarr?(u^.dw) OR vrs.array_of?(u^.dw) THEN
    CASE a.am OF
      |am_imm   : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_Gimm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_Limm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_STK   : a.am:=am_aSTK; a.disp:=0;
      |am_G     : a.am:=am_aG; a.n:=a.disp; a.disp:=0;
      |am_Gstr  : a.am:=am_aG; a.n:=a.disp; a.disp:=0;
      |am_L     : a.am:=am_aL; a.n:=a.disp; a.disp:=0;
      |am_PB2   : a.am:=am_aPB; a.n:=a.disp; a.disp:=0;
      |am_PB4   : a.am:=am_aPB; a.n:=a.disp; a.disp:=0;
      |am_PB8   : a.am:=am_aPB; a.n:=a.disp; a.disp:=0;
    ELSE cmd.load(a,4); a.am:=am_aSTK; a.disp:=0;
    END;
  END;
END designator_u;

PROCEDURE assign_halt(e: ref; VAL a,sz: sym.access);
BEGIN
  error(e,assert);
END assign_halt;

BEGIN
  WITH const DO am:=am_imm; disp:=0; level:=0 END;
  expression:=designator;
  txt_pos:=0;
END inDesig.
