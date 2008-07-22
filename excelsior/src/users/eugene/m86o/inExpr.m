IMPLEMENTATION MODULE inExpr; (* Sem 04-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

IMPORT  pc  : pcTab;
IMPORT  flw : inFlow;
IMPORT  vrs : inVars;
IMPORT  put : inCmd;
IMPORT  cmd : inCmds;
IMPORT  fpp : inFPP;
IMPORT  sym : inSym;
IMPORT  des : inDesig;

TYPE
  am_set = SET OF sym.adr_mode;

CONST
  assert   = 'error in compiler';
  ill_type = 'illegal type';

  on_stack = am_set{sym.am_STK,sym.am_aSTK};
  am_const = am_set{sym.am_imm,sym.am_Gimm,sym.am_Limm};

VAR
  stat_pos: INTEGER;
  const   : sym.access;

PROCEDURE error(l: pc.ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l#NIL)&(l^.md#pc.procedure)&(l^.md#pc.number) THEN stat_pos:=l^.pos END;
  pc.error(stat_pos,TRUE,s,x);
END error;

PROCEDURE power_2(v: INTEGER; VAR n: INTEGER): BOOLEAN;
  VAR s,i: INTEGER;
BEGIN
  FOR i:=0 TO 30 DO
    s:=INTEGER({i});
    IF v<s THEN RETURN FALSE END;
    IF v=s THEN n:=i; RETURN TRUE END;
  END;
  RETURN FALSE;
END power_2;

PROCEDURE imm?(l: pc.ref; VAL a: sym.access);
BEGIN
  IF a.am IN am_const THEN RETURN END;
  error(l,assert);
END imm?;

PROCEDURE bytes_i(l: pc.ref): INTEGER;
  VAR sz: sym.access;
BEGIN
  des.bytes(l,sz); imm?(l,sz); RETURN sz.n;
END bytes_i;

PROCEDURE load(l: pc.ref);
  VAR a: sym.access; sz: INTEGER;
BEGIN
  expression(l,a);
  IF a.am#sym.am_STK THEN sz:=bytes_i(l^.dw); cmd.load(a,sz) END;
END load;

PROCEDURE load_2_swap(l,r: pc.ref; VAR a: sym.access);
  VAR aa: sym.access; sz: INTEGER;
BEGIN
  expression(l,aa); expression(r,a);
  IF aa.am=sym.am_STK THEN RETURN END;
  IF NOT (a.am IN on_stack) & NOT (aa.am IN am_const) THEN
    sz:=bytes_i(l^.dw); cmd.load(aa,sz);
  ELSE
    IF a.am#sym.am_STK THEN sz:=bytes_i(l^.dw); cmd.load(a,sz) END;
    IF aa.am=sym.am_aSTK THEN cmd.swap END;
    a:=aa;
  END;
END load_2_swap;

PROCEDURE load_2(l,r: pc.ref; VAR a: sym.access);
  VAR aa: sym.access; sz: INTEGER;
BEGIN
  sz:=bytes_i(l^.dw);
  ASSERT(sz IN {1,2,4});
  expression(l,aa);
  IF aa.am=sym.am_aSTK THEN cmd.load(aa,sz); aa.am:=sym.am_STK END;
  expression(r,a);
  IF aa.am=sym.am_STK THEN RETURN END;
  cmd.load(aa,sz);
  IF (a.am=sym.am_STK) OR (a.am=sym.am_aSTK) THEN cmd.swap END;
END load_2;

PROCEDURE bic;
BEGIN
  cmd.pop_reg(2);
  IF cmd.pos[cmd.stk-1]=cmd.da THEN
    CASE cmd.siz[cmd.stk-1] OF
      |1: put.b(put.grp1_bm); put.b(put.md_reg+put.AL+put.g1_not);
          put.b(put.and_brm); put.b(put.md_reg+put.AL+put.CL*8);
      |2: put.b(put.grp1_wm); put.b(put.md_reg+put.AX+put.g1_not);
          put.b(put.and_wrm); put.b(put.md_reg+put.AX+put.CX*8);
      |4: put.b(put.grp1_wm); put.b(put.md_reg+put.AX+put.g1_not);
          put.b(put.and_wrm); put.b(put.md_reg+put.AX+put.CX*8);
          put.b(put.grp1_wm); put.b(put.md_reg+put.DX+put.g1_not);
          put.b(put.and_wrm); put.b(put.md_reg+put.DX+put.BX*8);
    END;
  ELSE
    CASE cmd.siz[cmd.stk-1] OF
      |1: put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_not);
          put.b(put.and_brm); put.b(put.md_reg+put.CL+put.AL*8);
      |2: put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_not);
          put.b(put.and_wrm); put.b(put.md_reg+put.CX+put.AX*8);
      |4: put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_not);
          put.b(put.and_wrm); put.b(put.md_reg+put.CX+put.AX*8);
          put.b(put.grp1_wm); put.b(put.md_reg+put.BX+put.g1_not);
          put.b(put.and_wrm); put.b(put.md_reg+put.BX+put.DX*8);
    END;
  END;
  DEC(cmd.stk);
END bic;

PROCEDURE set_z;
BEGIN
  IF cmd.siz[cmd.stk-1]>2 THEN
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      put.b(put.or_wrm); put.b(put.md_reg+put.DX+put.AX*8);
    ELSE
      put.b(put.or_wrm); put.b(put.md_reg+put.BX+put.CX*8);
    END;
  END;
  DEC(cmd.stk);
END set_z;

PROCEDURE gen_flags_set(l: pc.ref; VAR c: put.condition);
  VAR a: sym.access;
BEGIN
  CASE c OF
    |put.c_le:
      load_2(l^.l,l^.r,a);
      cmd.load(a,cmd.siz[cmd.stk-1]);
      bic; set_z; c:=put.c_z;
    |put.c_ge:
      load_2(l^.l,l^.r,a);
      cmd.load(a,cmd.siz[cmd.stk-1]);
      cmd.swap; bic; set_z; c:=put.c_z;
    |put.c_z,put.c_nz:
      load_2_swap(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN cmd.cmd_stk_m(cmd.c_xor,a); set_z;
      ELSE cmd.cmd_stk_m(cmd.c_sub,a); DEC(cmd.stk);
      END;
  END;
END gen_flags_set;

PROCEDURE gen_flags_cardinal(l: pc.ref; VAR c: put.condition);
  VAR a: sym.access;
BEGIN
  CASE c OF
    |put.c_l :
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop; c:=put.c_c;
    |put.c_ge:
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop; c:=put.c_nc;
    |put.c_le:
      load_2(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=put.c_nc;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a); c:=put.c_cz;
      END;
      cmd.drop;
    |put.c_g :
      load_2(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=put.c_c;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a); c:=put.c_ncz;
      END;
      cmd.drop;
    |put.c_z,put.c_nz:
      load_2_swap(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN cmd.cmd_stk_m(cmd.c_xor,a); set_z;
      ELSE cmd.cmd_stk_m(cmd.c_sub,a); DEC(cmd.stk);
      END;
  END;
END gen_flags_cardinal;

PROCEDURE gen_flags_address(l: pc.ref; VAR c: put.condition);
BEGIN
  gen_flags_cardinal(l,c);
END gen_flags_address;

PROCEDURE gen_flags_integer(l: pc.ref; VAR c: put.condition);
  VAR aa,a: sym.access; sz: INTEGER;
BEGIN
  expression(l^.l,aa); expression(l^.r,a);
  IF aa.am#sym.am_STK THEN
    IF NOT (a.am IN on_stack) & NOT (aa.am IN am_const) THEN
      sz:=bytes_i(l^.l^.dw); cmd.load(aa,sz);
    ELSE
      IF a.am#sym.am_STK THEN sz:=bytes_i(l^.r^.dw); cmd.load(a,sz) END;
      IF aa.am=sym.am_aSTK THEN cmd.swap END;
      a:=aa;
      CASE c OF
        |put.c_l : c:=put.c_g;      |put.c_ge: c:=put.c_le;
        |put.c_le: c:=put.c_ge;     |put.c_g : c:=put.c_l;
      ELSE
      END;
    END;
  END;
  CASE c OF
    |put.c_l : cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop;
    |put.c_ge: cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop;
    |put.c_le:
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=put.c_ge;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a);
      END;
      cmd.drop;
    |put.c_g :
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=put.c_l;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a);
      END;
      cmd.drop;
    |put.c_z,put.c_nz:
      IF cmd.siz[cmd.stk-1]=4 THEN cmd.cmd_stk_m(cmd.c_xor,a); set_z;
      ELSE cmd.cmd_stk_m(cmd.c_sub,a); DEC(cmd.stk);
      END;
  END;
END gen_flags_integer;

PROCEDURE gen_flags_real(l: pc.ref; VAR c: put.condition);
  VAR sz: INTEGER; a: sym.access;
BEGIN
  sz:=bytes_i(l^.l^.dw);
  expression(l^.l,a); fpp.load(fpp.c_mov,a,sz);
  expression(l^.r,a); fpp.load(fpp.c_cmp,a,sz);
  const.n:=41h; cmd.cmd_stk_m(cmd.c_and,const);
  CASE c OF
    |put.c_l : const.n:=1; cmd.cmd_stk_m(cmd.c_cmp,const); c:=put.c_z;
    |put.c_le: c:=put.c_nz;
    |put.c_ge: const.n:=1; cmd.cmd_stk_m(cmd.c_cmp,const); c:=put.c_nz;
    |put.c_g : c:=put.c_z;
    |put.c_z : const.n:=40h; cmd.cmd_stk_m(cmd.c_cmp,const); c:=put.c_z;
    |put.c_nz: const.n:=40h; cmd.cmd_stk_m(cmd.c_cmp,const); c:=put.c_nz;
  END;
  DEC(cmd.stk);
END gen_flags_real;

PROCEDURE gen_flags_string(l: pc.ref; VAR c: put.condition);
  VAR a: sym.access;
BEGIN
  des.adr_u(l^.l,a); cmd.load(a,4);
  des.adr_u(l^.r,a); cmd.load(a,4);
  cmd.save;
  flw.external_call(l^.pos,'$CmpByt');
  DEC(cmd.stk,2);
  CASE c OF
    |put.c_l : c:=put.c_c;      |put.c_ge: c:=put.c_nc;
    |put.c_le: c:=put.c_cz;     |put.c_g : c:=put.c_ncz;
  ELSE
  END;
END gen_flags_string;

PROCEDURE gen_flags(l: pc.ref; VAR c: put.condition);
BEGIN
  CASE vrs.vm(l^.l^.dw) OF
    |vrs.vm_integer : gen_flags_integer (l,c);
    |vrs.vm_real    : gen_flags_real    (l,c);
    |vrs.vm_cardinal: gen_flags_cardinal(l,c);
    |vrs.vm_boolean : gen_flags_cardinal(l,c);
    |vrs.vm_set     : gen_flags_set     (l,c);
    |vrs.vm_string  : gen_flags_string  (l,c);
    |vrs.vm_address : gen_flags_address (l,c);
  ELSE error(l,ill_type);
  END;
END gen_flags;

PROCEDURE gen_compare(l: pc.ref; VAR a: sym.access; c: put.condition);
  VAR i: INTEGER;
BEGIN
  gen_flags(l,c);
  const.n:=1; cmd.load(const,1);
  put.b(put.jo+INTEGER(c)); put.b(0);
  i:=put.cnt;
  cmd.drop; const.n:=0; cmd.load(const,1);
  put.code[i-1]:=CHAR(put.cnt-i);
  a.am:=sym.am_STK;
END gen_compare;

PROCEDURE gen_compare_condition
  (l: pc.ref; c: put.condition; then,else: flw.node);
BEGIN
  gen_flags(l,c);
  flw.block^.md:=flw.nm_cond;
  flw.block^.goto:=then;
  flw.block^.else:=else;
  flw.block^.flag:=c;
END gen_compare_condition;

PROCEDURE gen_in_condition(l: pc.ref; then,else: flw.node);
  VAR a,b,c: sym.access; fr,to,lsz,rsz: INTEGER; sg: BOOLEAN;
BEGIN
  expression(l^.l,a); expression(l^.r,c);
  IF (a.am IN on_stack) & (c.am IN on_stack) THEN cmd.swap END;
  sg:=vrs.sign?(l^.l^.dw); lsz:=bytes_i(l^.l^.dw); rsz:=bytes_i(l^.r^.dw);
  IF l^.r^.dw^.md#pc.set THEN error(l^.r,assert) END;
  des.min(l^.r^.dw^.dw,b); imm?(l^.r,b); fr:=b.n;
  IF a.am=sym.am_imm THEN
    des.max(l^.r^.dw^.dw,b); imm?(l^.r,b); to:=b.n;
    IF (a.n<fr) OR (a.n>to) THEN a.n:=0 ELSE a.n:=INTEGER({a.n-fr}) END;
  ELSE
    cmd.load(a,lsz); a.am:=sym.am_STK;
    IF c.am IN on_stack THEN cmd.pop_reg(2) END;
    --^-- иначе при переходе на false останется лишнее на стеке
    cmd.cmd_stk_m(cmd.c_sub,b);
    IF sg THEN flw.block^.flag:=put.c_ge;
    ELSE flw.block^.flag:=put.c_nc;
    END;
    flw.block^.else:=else; flw.block^.goto:=flw.new();
    flw.block^.md:=flw.nm_cond; flw.start(flw.block^.goto);
    IF lsz=4 THEN
      cmd.siz[cmd.stk-1]:=2; flw.block^.flag:=put.c_z;
      flw.block^.else:=else; flw.block^.goto:=flw.new();
      flw.block^.md:=flw.nm_cond; flw.start(flw.block^.goto);
    END;
    des.max(l^.r^.dw^.dw,b); imm?(l^.r,b); to:=b.n;
    b.n:=to-fr; cmd.cmd_stk_m(cmd.c_cmp,b);
    flw.block^.flag:=put.c_cz;
    flw.block^.else:=else; flw.block^.goto:=flw.new();
    flw.block^.md:=flw.nm_cond; flw.start(flw.block^.goto);
    des.make_bitset(rsz);
  END;
  IF a.am=sym.am_STK THEN
    IF c.am=sym.am_aSTK THEN cmd.swap END;
    cmd.cmd_stk_m(cmd.c_and,c);
  ELSE cmd.load(c,rsz); cmd.cmd_stk_m(cmd.c_and,a);
  END;
  IF rsz>2 THEN
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      put.b(put.or_wrm); put.b(put.md_reg+put.DX+put.AX*8);
    ELSE
      put.b(put.or_wrm); put.b(put.md_reg+put.BX+put.CX*8);
    END;
  END;
  cmd.drop;
  flw.block^.md:=flw.nm_cond; flw.block^.flag:=put.c_nz;
  flw.block^.else:=else; flw.block^.goto:=then;
END gen_in_condition;

PROCEDURE gen_condition(cond: pc.ref; then,else: flw.node);
  VAR h: flw.node;
BEGIN
  CASE cond^.md OF
    |pc.not          : gen_condition(cond^.r,else,then);
    |pc.plus         : h:=flw.new(); gen_condition(cond^.l,then,h);
                       flw.start(h); gen_condition(cond^.r,then,else);
    |pc.star         : h:=flw.new(); gen_condition(cond^.l,h,else);
                       flw.start(h); gen_condition(cond^.r,then,else);
    |pc.less         : gen_compare_condition(cond,put.c_l,then,else);
    |pc.less_equal   : gen_compare_condition(cond,put.c_le,then,else);
    |pc.greater      : gen_compare_condition(cond,put.c_g,then,else);
    |pc.greater_equal: gen_compare_condition(cond,put.c_ge,then,else);
    |pc.equal        : gen_compare_condition(cond,put.c_z,then,else);
    |pc.inequality   : gen_compare_condition(cond,put.c_nz,then,else);
    |pc.in           : gen_in_condition(cond,then,else);
  ELSE
    load(cond); cmd.set_flag_z;
    flw.block^.md:=flw.nm_cond;
    flw.block^.flag:=put.c_nz;
    flw.block^.goto:=then;
    flw.block^.else:=else;
  END;
END gen_condition;

PROCEDURE gen_neg(l: pc.ref; VAR a: sym.access);
  VAR ax,dx,sz: INTEGER;
BEGIN
  expression(l^.r,a);
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_cardinal:
    |vrs.vm_integer :
      sz:=bytes_i(l^.r^.dw);
      cmd.load(a,sz); a.am:=sym.am_STK; cmd.pop_reg(1);
      IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=put.AX; dx:=put.DX;
      ELSE ax:=put.CX; dx:=put.BX;
      END;
      CASE sz OF
        |1: put.b(put.grp1_bm); put.b(put.md_reg+ax+put.g1_neg);
        |2: put.b(put.grp1_wm); put.b(put.md_reg+ax+put.g1_neg);
        |4: put.b(put.grp1_wm); put.b(put.md_reg+dx+put.g1_neg);
            put.b(put.grp1_wm); put.b(put.md_reg+ax+put.g1_neg);
            put.b(put.imm_wbm); put.b(put.md_reg+dx+put.i_sbb); put.b(0);
      END;
    |vrs.vm_real:
      sz:=bytes_i(l^.r^.dw); fpp.load(fpp.c_mov,a,sz);
      fpp.ucm(fpp.u_neg); a.am:=sym.am_FPP;
    |vrs.vm_set :
      sz:=bytes_i(l^.dw); cmd.load(a,sz);
      des.min(l^.dw^.dw,a); imm?(l,a); ax:=a.n;
      des.max(l^.dw^.dw,a); imm?(l,a); ax:=a.n-ax;
      const.n:=INTEGER({0..ax}); a.am:=sym.am_STK;
      cmd.cmd_stk_m(cmd.c_xor,const);
  ELSE error(l,ill_type);
  END;
END gen_neg;

PROCEDURE gen_minus(l: pc.ref; VAR a: sym.access);
  VAR sz: INTEGER;
BEGIN
  IF l^.l=NIL THEN gen_neg(l,a); RETURN END;
  CASE vrs.vm(l^.l^.dw) OF
    |vrs.vm_integer :
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); a.am:=sym.am_STK;
    |vrs.vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); a.am:=sym.am_STK;
    |vrs.vm_address :
      IF vrs.vm(l^.r^.dw)#vrs.vm_integer THEN error(l^.r,ill_type) END;
      load_2(l^.l,l^.r,a); cmd.sub_adr(a); a.am:=sym.am_STK;
    |vrs.vm_set     : load(l^.l); load(l^.r); bic; a.am:=sym.am_STK;
    |vrs.vm_real    :
      sz:=bytes_i(l^.dw);
      expression(l^.l,a); fpp.load(fpp.c_mov,a,sz);
      expression(l^.r,a); fpp.load(fpp.c_sub,a,sz);
      a.am:=sym.am_FPP;
  ELSE error(l,ill_type);
  END;
END gen_minus;

PROCEDURE gen_plus(l: pc.ref; VAR a: sym.access);
  VAR then,else,next: flw.node; sz: INTEGER; b: sym.access;
BEGIN
  CASE vrs.vm(l^.l^.dw) OF
    |vrs.vm_integer :
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_add,a);
      a.am:=sym.am_STK;
    |vrs.vm_cardinal:
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_add,a);
      a.am:=sym.am_STK;
    |vrs.vm_address :
      IF vrs.vm(l^.r^.dw)#vrs.vm_integer THEN error(l^.r,ill_type) END;
      load_2(l^.l,l^.r,a); cmd.add_adr(a); a.am:=sym.am_STK;
    |vrs.vm_set     :
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_or,a);
      a.am:=sym.am_STK;
    |vrs.vm_boolean :
      then:=flw.new(); else:=flw.new(); next:=flw.new();
      gen_condition(l,then,else);
      flw.start(then); const.n:=1;
      cmd.load(const,1); cmd.drop; flw.block^.goto:=next;
      flw.start(else); const.n:=0;
      cmd.load(const,1); flw.block^.goto:=next;
      flw.start(next); a.am:=sym.am_STK;
    |vrs.vm_real    :
      sz:=bytes_i(l^.dw);
      expression(l^.l,a); expression(l^.r,b);
      IF a.am=sym.am_FPP THEN fpp.load(fpp.c_add,b,sz);
      ELSIF b.am=sym.am_FPP THEN fpp.load(fpp.c_add,a,sz);
      ELSE fpp.load(fpp.c_mov,b,sz); fpp.load(fpp.c_add,a,sz);
      END;
      a.am:=sym.am_FPP;
  ELSE error(l,ill_type);
  END;
END gen_plus;

PROCEDURE cmul(pos: INTEGER);
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.save; flw.external_call(pos,'$UnsMul');
  ELSE
    cmd.pop_reg(2);
    IF cmd.siz[cmd.stk-1]=2 THEN
      put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_mul);
    ELSE
      put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_mul);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END cmul;

PROCEDURE gen_star(l: pc.ref; VAR a: sym.access);
  VAR then,else,next: flw.node; sz: INTEGER; b: sym.access;
BEGIN
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer :
      load_2_swap(l^.l,l^.r,a); cmd.imul(l^.pos,a); a.am:=sym.am_STK;
    |vrs.vm_cardinal:
      load_2_swap(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cmul(l^.pos); a.am:=sym.am_STK;
    |vrs.vm_set     :
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_and,a);
      a.am:=sym.am_STK;
    |vrs.vm_boolean :
      then:=flw.new(); else:=flw.new(); next:=flw.new();
      gen_condition(l,then,else);
      flw.start(then); const.n:=1;
      cmd.load(const,1); cmd.drop; flw.block^.goto:=next;
      flw.start(else); const.n:=0;
      cmd.load(const,1); flw.block^.goto:=next;
      flw.start(next); a.am:=sym.am_STK;
    |vrs.vm_real    :
      sz:=bytes_i(l^.dw);
      expression(l^.l,a); expression(l^.r,b);
      IF a.am=sym.am_FPP THEN fpp.load(fpp.c_mul,b,sz);
      ELSIF b.am=sym.am_FPP THEN fpp.load(fpp.c_mul,a,sz);
      ELSE fpp.load(fpp.c_mov,b,sz); fpp.load(fpp.c_mul,a,sz);
      END;
      a.am:=sym.am_FPP;
  ELSE error(l,ill_type);
  END;
END gen_star;

PROCEDURE iquot(pos: INTEGER);
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.save; flw.external_call(pos,'$SgnQuo');
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN put.b(put.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      put.b(put.cwd);
      put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_idiv);
    ELSE
      put.b(put.cbw);
      put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_idiv);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END iquot;

PROCEDURE cdiv(pos: INTEGER);
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.save; flw.external_call(pos,'$UnsDiv');
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN put.b(put.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      put.b(put.mov_dxi); put.w(0);
      put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_div);
    ELSE
      put.b(put.mov_ahi); put.b(0);
      put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_div);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END cdiv;

PROCEDURE gen_slash(l: pc.ref; VAR a: sym.access);
  VAR sz: INTEGER;
BEGIN
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer :
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      iquot(l^.pos); a.am:=sym.am_STK;
    |vrs.vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cdiv(l^.pos); a.am:=sym.am_STK;
    |vrs.vm_set     :
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_xor,a);
      a.am:=sym.am_STK;
    |vrs.vm_real    :
      sz:=bytes_i(l^.dw);
      expression(l^.l,a); fpp.load(fpp.c_mov,a,sz);
      expression(l^.r,a); fpp.load(fpp.c_div,a,sz);
      a.am:=sym.am_FPP;
  ELSE error(l,ill_type);
  END;
END gen_slash;

PROCEDURE gen_div(l: pc.ref; VAR a: sym.access);
BEGIN
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer :
      load_2(l^.l,l^.r,a); cmd.idiv(l^.pos,a); a.am:=sym.am_STK;
    |vrs.vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cdiv(l^.pos); a.am:=sym.am_STK;
  ELSE error(l,ill_type);
  END;
END gen_div;

PROCEDURE irem(pos: INTEGER);
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.save; flw.external_call(pos,'$SgnRem');
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN put.b(put.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      put.b(put.cwd);
      put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_idiv);
      put.b(put.mov_wrm); put.b(put.md_reg+put.DX+put.AX*8);
    ELSE
      put.b(put.cbw);
      put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_idiv);
      put.b(put.mov_brm); put.b(put.md_reg+put.AH+put.AL*8);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END irem;

PROCEDURE imod(pos: INTEGER; VAR a: sym.access);
  VAR i: INTEGER;
BEGIN
  IF (a.am IN am_const) & power_2(a.n,i) THEN
    IF i=0 THEN DEC(cmd.stk); a.n:=0; a.am:=sym.am_imm; RETURN END;
    a.n:=INTEGER({0..i-1}); a.am:=sym.am_imm;
    cmd.cmd_stk_m(cmd.c_and,a); a.am:=sym.am_STK;
  ELSE
    cmd.save; cmd.stot(a,cmd.siz[cmd.stk-1]);
    CASE cmd.siz[cmd.stk-1] OF
      |1: flw.external_call(pos,'$SgnModByt');
      |2: flw.external_call(pos,'$SgnModWrd');
      |4: flw.external_call(pos,'$SgnMod');
    END;
    cmd.pos[cmd.stk-1]:=cmd.da; a.am:=sym.am_STK;
  END;
END imod;

PROCEDURE cmod(pos: INTEGER);
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.save; flw.external_call(pos,'$UnsRem');
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN put.b(put.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      put.b(put.mov_dxi); put.w(0);
      put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_div);
      put.b(put.mov_wrm); put.b(put.md_reg+put.DX+put.AX*8);
    ELSE
      put.b(put.mov_ahi); put.b(0);
      put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_div);
      put.b(put.mov_brm); put.b(put.md_reg+put.AH+put.AL*8);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END cmod;

PROCEDURE gen_rem(l: pc.ref; VAR a: sym.access);
BEGIN
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer :
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      irem(l^.pos); a.am:=sym.am_STK;
    |vrs.vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cmod(l^.pos); a.am:=sym.am_STK;
  ELSE error(l,ill_type);
  END;
END gen_rem;

PROCEDURE gen_mod(l: pc.ref; VAR a: sym.access);
BEGIN
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_integer : load_2(l^.l,l^.r,a); imod(l^.pos,a);
    |vrs.vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cmod(l^.pos); a.am:=sym.am_STK;
  ELSE error(l,ill_type);
  END;
END gen_mod;

PROCEDURE gen_bits_u(l: pc.ref; VAR a: sym.access);
BEGIN
  des.bytes_u(l^.r^.dw,a);
  IF a.am IN am_const THEN a.n:=a.n*8; a.am:=sym.am_imm;
  ELSE cmd.load(a,4); a.am:=sym.am_STK; const.n:=8; cmd.imul(l^.pos,const);
  END;
END gen_bits_u;

PROCEDURE gen_type_transfer(l: pc.ref; VAR a: sym.access);
  TYPE v_set = SET OF vrs.val_mode;
  CONST simp = v_set{vrs.vm_integer,vrs.vm_cardinal,vrs.vm_set,
                     vrs.vm_address,vrs.vm_boolean};
  VAR sz1,sz2: sym.access; m1,m2: vrs.val_mode;
BEGIN
  expression(l^.r,a);
  des.bytes_u(l^.r^.dw,sz1); m1:=vrs.vm(l^.r^.dw);
  des.bytes_u(l^.dw,sz2); m2:=vrs.vm(l^.dw);
  IF NOT ((sz1.am IN am_const) & (sz2.am IN am_const)) THEN
    error(l,'illegal type conversion')
  ELSIF (m1 IN simp) & (m2 IN simp) THEN
    IF sz1.n<sz2.n THEN
      cmd.load(a,sz1.n); a.am:=sym.am_STK;
      cmd.set_size(sz2.n,m1=vrs.vm_integer);
    ELSIF a.am=sym.am_STK THEN cmd.siz[cmd.stk-1]:=sz2.n;
    END;
  ELSIF sz1.n#sz2.n THEN error(l,'illegal type conversion')
  END;
END gen_type_transfer;

PROCEDURE gen_size_check(l: pc.ref; VAR a: sym.access);
  VAR sz1,sz2: sym.access;
BEGIN
  expression(l^.r,a); des.bytes_u(l^.r^.dw,sz1); des.bytes_u(l^.dw,sz2);
  IF NOT ((sz1.am IN am_const) & (sz2.am IN am_const)) THEN
    error(l,'illegal type conversion')
  ELSIF sz1.n#sz2.n THEN
    error(l,'illegal type conversion')
  END;
END gen_size_check;

PROCEDURE gen_range_check(rr: pc.ref; VAR a: sym.access);
  VAR i: INTEGER;
BEGIN
  expression(rr^.r,a);
  WITH vrs.rngs[rr^.dw^.adr] DO
    cmd.load(a,size); a.am:=sym.am_STK;
    IF size=4 THEN
      cmd.copt; cmd.cmd_stk_m(cmd.c_sub,l); cmd.drop;
      IF sign THEN put.b(put.jl) ELSE put.b(put.jb) END;
      i:=put.cnt; put.b(0);
      const.n:=r.n+1;
      cmd.copt; cmd.cmd_stk_m(cmd.c_sub,const); cmd.drop;
      IF sign THEN put.b(put.jl) ELSE put.b(put.jb) END;
      put.b(2);
    ELSE
      cmd.cmd_stk_m(cmd.c_cmp,l);
      IF sign THEN put.b(put.jl) ELSE put.b(put.jb) END;
      i:=put.cnt; put.b(0);
      cmd.cmd_stk_m(cmd.c_cmp,r);
      IF sign THEN put.b(put.jle) ELSE put.b(put.jbe) END;
      put.b(2);
    END;
    put.code[i]:=CHAR(put.cnt-i-1);
    put.b(put.int); put.b(4);
  END;
END gen_range_check;

PROCEDURE gen_not(l: pc.ref; VAR a: sym.access);
  VAR i: INTEGER;
BEGIN
  expression(l^.r,a); cmd.load(a,1);
  cmd.set_flag_z;
  const.n:=1; cmd.load(const,1);
  put.b(put.je); put.b(0);
  i:=put.cnt;
  cmd.drop; const.n:=0; cmd.load(const,1);
  put.code[i-1]:=CHAR(put.cnt-i);
  a.am:=sym.am_STK;
END gen_not;

PROCEDURE gen_in(l: pc.ref; VAR a: sym.access);
  VAR then,else,next: flw.node;
BEGIN
  next:=flw.new(); then:=flw.new(); else:=flw.new();
  gen_in_condition(l,then,else);
  flw.start(then); const.n:=1;
  cmd.load(const,1); cmd.drop; flw.block^.goto:=next;
  flw.start(else); const.n:=0;
  cmd.load(const,1); flw.block^.goto:=next; flw.start(next);
  a.am:=sym.am_STK;
END gen_in;

PROCEDURE gen_abs(l: pc.ref; VAR a: sym.access);
  VAR ax,dx,sz: INTEGER;
BEGIN
  expression(l^.r,a);
  CASE vrs.vm(l^.dw) OF
    |vrs.vm_cardinal:
    |vrs.vm_integer :
      sz:=bytes_i(l^.r^.dw);
      cmd.load(a,sz); a.am:=sym.am_STK; cmd.pop_reg(1);
      IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=put.AX; dx:=put.DX;
      ELSE ax:=put.CX; dx:=put.BX;
      END;
      CASE sz OF
        |1: put.b(put.and_brm); put.b(put.md_reg+ax+ax*8);
            put.b(put.jns); put.b(2);
            put.b(put.grp1_bm); put.b(put.md_reg+ax+put.g1_neg);
        |2: put.b(put.and_wrm); put.b(put.md_reg+ax+ax*8);
            put.b(put.jns); put.b(2);
            put.b(put.grp1_wm); put.b(put.md_reg+ax+put.g1_neg);
        |4: put.b(put.and_wrm); put.b(put.md_reg+dx+dx*8);
            put.b(put.jns); put.b(7);
            put.b(put.grp1_wm); put.b(put.md_reg+dx+put.g1_neg);
            put.b(put.grp1_wm); put.b(put.md_reg+ax+put.g1_neg);
            put.b(put.imm_wbm); put.b(put.md_reg+dx+put.i_sbb); put.b(0);
      END;
    |vrs.vm_real   :
      sz:=bytes_i(l^.dw); fpp.load(fpp.c_mov,a,sz);
      fpp.ucm(fpp.u_abs); a.am:=sym.am_FPP;
  ELSE error(l,ill_type);
  END;
END gen_abs;

PROCEDURE gen_worder(l: pc.ref; VAR a: sym.access);
BEGIN
  CASE vrs.vm(l^.r^.dw) OF
    |vrs.vm_integer : load(l^.r); cmd.set_size(4,TRUE);  a.am:=sym.am_STK;
    |vrs.vm_cardinal: load(l^.r); cmd.set_size(4,FALSE); a.am:=sym.am_STK;
    |vrs.vm_boolean : load(l^.r); cmd.set_size(4,FALSE); a.am:=sym.am_STK;
    |vrs.vm_set     : load(l^.r); cmd.set_size(4,FALSE); a.am:=sym.am_STK;
    |vrs.vm_real    : expression(l^.r,a);
    |vrs.vm_address : expression(l^.r,a);
    |vrs.vm_string  : des.adr_u(l^.r,a);
    |vrs.vm_undef   : des.adr_u(l^.r,a);
  END;
END gen_worder;

PROCEDURE gen_rot(l: pc.ref; VAR a: sym.access; rg: BOOLEAN);
  VAR lsz,ax,dx,cx,i: INTEGER;
BEGIN
  lsz:=bytes_i(l^.l^.dw); expression(l^.l,a); cmd.load(a,lsz);
  IF lsz=4 THEN
    expression(l^.r,a);
    IF a.am IN am_const THEN
      IF rg THEN a.n:=-a.n END;
      a.n:=a.n MOD 32;
      cmd.pop_reg(1);
      IF a.n>=16 THEN
        IF cmd.pos[cmd.stk-1]=cmd.da THEN put.b(put.xchg_dx);
        ELSE put.b(put.xchg_wmr); put.b(put.md_reg+put.CX*8+put.BX);
        END;
        a.n:=a.n-16;
      END;
      IF a.n<=3 THEN
        IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=put.AX; dx:=put.DX;
        ELSE ax:=put.CX; dx:=put.BX;
        END;
        FOR i:=0 TO a.n-1 DO
          put.b(put.shift_wm1); put.b(put.md_reg+ax+put.s_shl);
          put.b(put.shift_wm1); put.b(put.md_reg+dx+put.s_rcl);
          put.b(put.imm_bm); put.b(put.md_reg+ax+put.i_adc); put.b(0);
        END;
        a.am:=sym.am_STK; RETURN;
      END;
    END;
    cmd.load(a,1); cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=put.CX; dx:=put.BX; cx:=put.AX;
    ELSE ax:=put.AX; dx:=put.DX; cx:=put.CX;
    END;
    IF NOT (a.am IN am_const) THEN
      IF rg THEN put.b(put.grp1_bm); put.b(put.md_reg+cx+put.g1_neg) END;
      put.b(put.imm_bm); put.b(put.md_reg+cx+put.i_and); put.b(1Fh);
      put.b(put.je); put.b(11);
    END;
    put.b(put.shift_wm1); put.b(put.md_reg+ax+put.s_shl);
    put.b(put.shift_wm1); put.b(put.md_reg+dx+put.s_rcl);
    put.b(put.imm_bm); put.b(put.md_reg+ax+put.i_adc); put.b(0);
    put.b(put.grp2_b); put.b(put.md_reg+cx+put.g2_dec);
    put.b(put.jne); put.b(-11);
    DEC(cmd.stk);
  ELSE
    load(l^.r); cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN put.b(put.xchg_cx) END;
    IF lsz=1 THEN put.b(put.shift_bmc) ELSE put.b(put.shift_wmc) END;
    IF rg THEN put.b(put.md_reg+put.s_ror);
    ELSE put.b(put.md_reg+put.s_rol);
    END;
    DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
  END;
  a.am:=sym.am_STK;
END gen_rot;

PROCEDURE gen_odd(l: pc.ref; VAR a: sym.access);
BEGIN
  expression(l^.r,a); cmd.load(a,1); a.am:=sym.am_STK;
  const.n:=1; cmd.cmd_stk_m(cmd.c_and,const);
END gen_odd;

PROCEDURE gen_cap(l: pc.ref; VAR a: sym.access);
BEGIN
  expression(l^.r,a); cmd.load(a,1); a.am:=sym.am_STK;
  const.n:=0DFh; cmd.cmd_stk_m(cmd.c_and,const);
END gen_cap;

PROCEDURE gen_float(l: pc.ref; VAR a: sym.access);
  VAR sz: INTEGER;
BEGIN
  sz:=bytes_i(l^.r^.dw);
  expression(l^.r,a);
  fpp.load_integer(fpp.c_mov,a,sz);
  a.am:=sym.am_FPP;
END gen_float;

PROCEDURE gen_trunc(l: pc.ref; VAR a: sym.access);
  VAR sz: INTEGER;
BEGIN
  sz:=bytes_i(l^.r^.dw);
  expression(l^.r,a);
  fpp.load(fpp.c_mov,a,sz);
  sz:=bytes_i(l^.dw);
  des.alloc_var(a,sz);
  fpp.store_integer(a,sz);
END gen_trunc;

PROCEDURE expression(l: pc.ref; VAR a: sym.access);
BEGIN
  IF l=NIL THEN error(NIL,assert) END;
  IF l^.md#pc.number THEN stat_pos:=l^.pos END;
  CASE l^.md OF
    |pc.call         : gen_call(l); a.am:=sym.am_STK;
    |pc.size_check   : gen_size_check(l,a);
    |pc.type_transfer: gen_type_transfer(l,a);
    |pc.number       : a.am:=sym.am_imm; a.n:=l^.val;
    |pc.minus        : gen_minus  (l,a);
    |pc.plus         : gen_plus   (l,a);
    |pc.star         : gen_star   (l,a);
    |pc.slash        : gen_slash  (l,a);
    |pc.rem          : gen_rem    (l,a);
    |pc.mod          : gen_mod    (l,a);
    |pc.rol          : gen_rot    (l,a,FALSE);
    |pc.ror          : gen_rot    (l,a,TRUE);
    |pc.div          : gen_div    (l,a);
    |pc.float        : gen_float  (l,a);
    |pc.trunc        : gen_trunc  (l,a);
    |pc.cap          : gen_cap    (l,a);
    |pc.odd          : gen_odd    (l,a);
    |pc.worder       : gen_worder (l,a);
    |pc.abs          : gen_abs    (l,a);
    |pc.in           : gen_in     (l,a);
    |pc.not          : gen_not    (l,a);
    |pc.range_check  : gen_range_check(l,a);
    |pc.less         : gen_compare(l,a,put.c_l);
    |pc.less_equal   : gen_compare(l,a,put.c_le);
    |pc.greater      : gen_compare(l,a,put.c_g);
    |pc.greater_equal: gen_compare(l,a,put.c_ge);
    |pc.equal        : gen_compare(l,a,put.c_z);
    |pc.inequality   : gen_compare(l,a,put.c_nz);
    |pc.adr          : des.adr_u  (l^.r,a);
    |pc.min          : des.min    (l^.r,a);
    |pc.max          : des.max    (l^.r,a);
    |pc.low          : des.low    (l^.r,a);
    |pc.high         : des.high   (l^.r,a);
    |pc.len          : des.len    (l^.r,a);
    |pc.size         : des.bytes_u(l^.r,a);
    |pc.bytes        : des.bytes_u(l^.r,a);
    |pc.bits         : gen_bits_u (l^.r,a);
  ELSE des.designator_u(l,a);
  END;
END expression;

BEGIN
  stat_pos:=0;
  const.am:=sym.am_imm;
  des.expression:=expression;
END inExpr.
