IMPLEMENTATION MODULE inStat; (* Sem 04-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD, ADR;

IMPORT  mcd : lowLevel;
IMPORT  tty : Terminal;

IMPORT  pc  : pcTab;
IMPORT  mem : pcSystem;
IMPORT  sym : inSym;
IMPORT  flw : inFlow;
IMPORT  vrs : inVars;
IMPORT  put : inCmd;
IMPORT  cmd : inCmds;
IMPORT  fpp : inFPP;
IMPORT  exp : inExpr;
IMPORT  des : inDesig;
IMPORT  asm : inAsm;

WITH STORAGE : mem;

TYPE
  val_mode_set = SET OF vrs.val_mode;
  adr_mode_set = SET OF sym.adr_mode;

CONST
  assert      = 'error in compiler';
  ill_type    = 'illegal type';
  range_error = 'range check error';

  int_case   = 45h;
  int_return = 46h;
  int_halt   = 47h;
  int_assert = 48h;
  int_size   = 4Ah;

  scalars = val_mode_set {
    vrs.vm_integer,vrs.vm_cardinal,vrs.vm_boolean,
    vrs.vm_set,vrs.vm_real,vrs.vm_address
  };
  -- значения статического размера 1,2,4 байтов

  on_stack=adr_mode_set{sym.am_STK,sym.am_aSTK};
  am_const=adr_mode_set{sym.am_imm,sym.am_Gimm,sym.am_Limm};
  am_value=adr_mode_set{sym.am_imm,sym.am_STK,sym.am_FPP};

VAR
  stat_pos   : INTEGER;
  proc       : pc.ref;
  proc_a     : POINTER TO pc.ref;
  level      : INTEGER;
  const      : sym.access;

PROCEDURE error(l: pc.ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l#NIL)&(l^.md#pc.procedure)&(l^.md#pc.number) THEN stat_pos:=l^.pos END;
  pc.error(stat_pos,TRUE,s,x);
END error;

PROCEDURE imm?(l: pc.ref; VAL a: sym.access);
BEGIN
  IF a.am IN am_const THEN RETURN END;
  error(l,assert);
END imm?;

PROCEDURE one(l: pc.ref); FORWARD;

PROCEDURE profile_psz(l: pc.ref): INTEGER;
  VAR v,t: pc.ref; n: INTEGER;
BEGIN
  v:=l^.dw; n:=0;
  WHILE v#NIL DO
    IF v^.md=pc.var THEN
      CASE vrs.vars[v^.adr].am OF
        |sym.am_PB2: INC(n,2);
        |sym.am_PB4: INC(n,4);
        |sym.am_PB8: INC(n,8);
        |sym.am_aPB: INC(n,4);
      END;
    END;
    v:=v^.nxt;
  END;
  RETURN n;
END profile_psz;

PROCEDURE alloc_var(sz: INTEGER; ind: BOOLEAN): INTEGER;
  VAR n: INTEGER;
BEGIN
  ASSERT(sz>0);
  n:=vrs.new_var();
  des.alloc_var(vrs.vars[n],sz);
  IF ind THEN
    IF level=0 THEN vrs.vars[n].am:=sym.am_aG;
    ELSE            vrs.vars[n].am:=sym.am_aL;
    END;
    vrs.vars[n].n:=vrs.vars[n].disp;
    vrs.vars[n].disp:=0;
  END;
  RETURN n;
END alloc_var;

PROCEDURE trap(n: INTEGER);
BEGIN
  IF n=3 THEN put.b(put.int_3) ELSE put.b(put.int); put.b(n) END;
END trap;

PROCEDURE exit_trap(n: INTEGER);
BEGIN
  put.b(put.mov_axi); put.w(4C00h+n MOD 100h);
  put.b(put.int); put.b(21h);
END exit_trap;

PROCEDURE gen_var(l: pc.ref);
  VAR b: sym.access;
BEGIN
  IF pc.ini_nil THEN
    IF l^.l^.md=pc.pointer THEN
      const.n:=0; cmd.load_store(vrs.vars[l^.adr],const,4);
    ELSIF (l^.l^.md=pc.packed_dynarr) OR (l^.l^.md=pc.dynarr)  THEN
      const.n:=0; b:=vrs.vars[l^.adr];
      cmd.load_store(b,const,4);
      const.n:=-1; des.add_offset(b,4);
      cmd.load_store(b,const,4);
    END;
  END;
END gen_var;

PROCEDURE assign_access(l: pc.ref; VAL d,lsz: sym.access);
  VAR e,rsz: sym.access; m: vrs.val_mode;
BEGIN
  m:=vrs.vm(l^.dw);
  IF m = vrs.vm_real THEN
    ASSERT((lsz.am=sym.am_imm) & (lsz.n IN {4,8}));
    exp.expression(l,e);
    IF (e.am#sym.am_FPP) & (lsz.n=4) THEN cmd.load_store(d,e,lsz.n);
    ELSE fpp.load(fpp.c_mov,e,lsz.n); fpp.store(d,lsz.n);
    END;
  ELSIF m IN scalars THEN
    ASSERT((lsz.am=sym.am_imm) & (lsz.n IN {1,2,4}));
    exp.expression(l,e); cmd.load_store(d,e,lsz.n);
  ELSIF lsz.am IN am_const THEN
    des.bytes_u(l,rsz);
    IF rsz.am IN am_const THEN
      IF rsz.n>lsz.n THEN error(l,'left size less then right one') END;
      cmd.load_adr(d); des.designator_u(l,e);
      cmd.load_adr(e); cmd.mov_c(rsz.n);
    ELSE
      cmd.load(rsz,4); cmd.load(lsz,4); cmd.swap;
      cmd.cmd_stk_m(cmd.c_sub,cmd.top);
      INC(cmd.stk); cmd.swap; DEC(cmd.stk);
      put.b(put.jnb); put.b(2);
      put.b(put.int); put.b(4);
      IF d.am IN on_stack THEN cmd.swap END;
      cmd.load_adr(d); des.designator_u(l,e);
      cmd.load_adr(e); cmd.move;
    END;
  ELSE
    cmd.load(lsz,4);
    des.bytes_u(l,rsz);
    IF rsz.am IN am_const THEN
      cmd.cmd_stk_m(cmd.c_sub,rsz); DEC(cmd.stk);
      put.b(put.jnb); put.b(2);
      put.b(put.int); put.b(4);
      cmd.load_adr(d); des.designator_u(l,e);
      cmd.load_adr(e); cmd.mov_c(rsz.n);
    ELSE
      cmd.load(rsz,4);
      cmd.cmd_stk_m(cmd.c_sub,cmd.top);
      INC(cmd.stk); cmd.swap; DEC(cmd.stk);
      put.b(put.jnb); put.b(2);
      put.b(put.int); put.b(4);
      IF d.am IN on_stack THEN cmd.swap END;
      cmd.load_adr(d); des.designator_u(l,e);
      cmd.load_adr(e); cmd.move;
    END;
  END;
END assign_access;

PROCEDURE gen_assign(l: pc.ref);
  VAR d,lsz: sym.access;
BEGIN
  des.designator_u(l^.l,d);
  des.bytes_u(l^.l,lsz);
  IF d.am IN am_value THEN error(l^.r,assert) END;
  assign_access(l^.r,d,lsz);
END gen_assign;

PROCEDURE gen_return_cmd;
  VAR n: INTEGER;
BEGIN
  n:=profile_psz(proc^.l^.l);
  put.b(put.mov_wrm); put.b(put.md_reg+put.BP+put.SP*8);
  IF vrs.prcs[proc^.l^.adr].export THEN
    put.b(put.pop_bp); put.b(put.pop_ds);
    IF n=0 THEN put.b(put.ret_i) ELSE put.b(put.ret_isp); put.w(n) END;
  ELSIF vrs.prcs[proc^.l^.adr].slink THEN
    put.b(put.pop_bp); put.b(put.ret_sp); put.w(n+2);
  ELSE
    put.b(put.pop_bp);
    IF n=0 THEN put.b(put.ret) ELSE put.b(put.ret_sp); put.w(n) END;
  END;
END gen_return_cmd;

PROCEDURE gen_return(l: pc.ref);
  VAR e,sz: sym.access;
BEGIN
  IF proc^.l^.l^.l#NIL THEN
    des.bytes(proc^.l^.l^.l,sz);
    exp.expression(l^.dw,e);
    cmd.load(e,sz.n); cmd.hold_out;
  END;
  gen_return_cmd;
  flw.start(flw.new());
END gen_return;

PROCEDURE val_array?(VAR a,sz: sym.access; v: pc.ref): BOOLEAN;
  VAR e: sym.access; p: pc.ref;
BEGIN
  p:=v^.dw;
  IF p=NIL THEN RETURN FALSE END;
  IF (p^.md=pc.array) OR (p^.md=pc.packed_array) OR
     (p^.md=pc.array_of) OR (p^.md=pc.packed_array_of) OR
     (p^.md=pc.dynarr) OR (p^.md=pc.packed_dynarr)
  THEN
    des.bytes(p^.r,e); imm?(v,e);
    IF e.n#sz.n THEN RETURN FALSE END;
    des.high(v,a); RETURN TRUE;
  END;
  RETURN FALSE;
END val_array?;

PROCEDURE gen_parms(pf,parms: pc.ref);
  VAR ass,prm,val,t,tv: pc.ref; a,sz: sym.access;
BEGIN
  ASSERT(pf^.md=pc.profile);
  ass:=parms;
  WHILE ass#NIL DO
    IF ass^.md=pc.assign THEN
      prm:=ass^.l; val:=ass^.r;
      IF prm^.md#pc.var THEN error(ass,assert) END;
      t:=prm^.l;
      LOOP
        IF t=NIL THEN error(ass,assert)
        ELSIF t^.md=pc.var_prm THEN t:=t^.dw
        ELSIF t^.md=pc.val_prm THEN t:=t^.dw
        ELSE EXIT
        END;
      END;
      IF vrs.array_of?(t) THEN
        ASSERT(vrs.vars[prm^.adr].am=sym.am_PB8);
        des.bytes(t^.r,sz); imm?(prm,sz);
        IF NOT val_array?(a,sz,val) THEN
          des.bytes_u(val,a);
          IF a.am=sym.am_imm THEN a.n:=a.n DIV sz.n - 1
          ELSE
            cmd.load(a,4); a.am:=sym.am_STK;
            cmd.idiv(ass^.pos,sz); const.n:=1; cmd.cmd_stk_m(cmd.c_sub,const);
          END;
        END;
        cmd.stot(a,4);
        des.adr_u(val,a);
        cmd.stot(a,4);
      ELSIF vrs.vars[prm^.adr].am=sym.am_PB2 THEN
        des.bytes(t,sz); imm?(ass,sz);
        IF sz.n>2 THEN error(ass,assert) END;
        exp.expression(val,a); cmd.stot(a,sz.n);
      ELSIF vrs.vars[prm^.adr].am=sym.am_PB4 THEN
        exp.expression(val,a);
        IF a.am=sym.am_FPP THEN des.alloc_var(a,4); fpp.store(a,4) END;
        cmd.stot(a,4);
      ELSIF vrs.vars[prm^.adr].am=sym.am_aPB THEN
        IF vrs.array_of?(val^.dw) THEN des.designator(val,a)
        ELSE des.adr(val,a)
        END;
        cmd.stot(a,4);
      ELSE error(val,assert);
      END;
    END;
    ass:=ass^.nxt;
  END;
END gen_parms;

PROCEDURE gen_call_procedure(prc: pc.ref);
  VAR n,j: INTEGER; next: flw.node;
BEGIN
  ASSERT(prc^.md=pc.procedure);
  IF vrs.prcs[prc^.adr].mod#0 THEN
    WITH vrs.prcs[prc^.adr] DO
      ASSERT(ofs>=0);
      ASSERT(export);
      ASSERT(ofs<=0FFFCh);
      put.b(put.grp2_w);
      put.b(put.g2_call_ii+put.md_abs); put.w(ofs);
    END;
    RETURN;
  ELSIF vrs.prcs[prc^.adr].slink THEN
    ASSERT(vrs.prcs[prc^.adr].mod=0);
    n:=vrs.prcs[prc^.adr].lvl;
    ASSERT(n<=level+1);
    IF n=level+1 THEN
      put.b(put.push_bp)
    ELSIF n=level THEN
      put.b(put.grp2_w); put.b(put.md_b+put.rm_bp+put.g2_push); put.b(4);
    ELSE
      put.b(put.mov_wrm); put.b(put.md_b+put.rm_bp+put.SI*8); put.b(4);
      FOR j:=n TO level-2 DO
        put.b(46b+put.SS*8);
        put.b(put.mov_wrm); put.b(put.md_b+put.rm_si+put.SI*8); put.b(4);
      END;
      put.b(46b+put.SS*8);
      put.b(put.grp2_w); put.b(put.md_b+put.rm_si+put.g2_push); put.b(4);
    END;
  END;
  next:=flw.new(); flw.block^.md:=flw.nm_call;
  flw.block^.proc:=prc^.adr; flw.block^.true:=next;
  flw.start(next);
END gen_call_procedure;

PROCEDURE gen_call(l: pc.ref);
  VAR
    prc        : pc.ref;
    a,va,fpp_sv: sym.access;
    res_fpp    : BOOLEAN;
    am,sr,ofs  : INTEGER;
BEGIN
  res_fpp:=fpp.stk>0;
  IF res_fpp THEN
    des.alloc_var(fpp_sv,94);
    cmd.gen_access(fpp_sv,1,am,sr,ofs);
    cmd.put_access(am,sr,ofs,60b,09Bh,0DDh);
  END;
  cmd.save;
  ASSERT(l^.md=pc.call);
  IF l^.l=NIL THEN error(l,'call^.l=NIL');
  ELSIF (l^.l^.md=pc.usage) & (l^.l^.r#NIL) &
    ((l^.l^.r^.md=pc.procedure) OR (l^.l^.r^.md=pc.inline)) THEN
    prc:=l^.l^.r;
    IF prc^.md=pc.procedure THEN
      gen_parms(prc^.l,l^.r);
      gen_call_procedure(prc);
    ELSIF prc^.md=pc.inline THEN
      ASSERT(prc^.dw^.md=pc.code_body);
      ASSERT(prc^.dw^.dw^.md=pc.string);
      gen_parms(prc^.l,l^.r);
      asm.gen_code(prc^.pos,FALSE,prc^.dw^.dw^.str);
    ELSE error(l,'must be procedure or inline')
    END;
  ELSE
    exp.expression(l^.l,a);
    IF a.am IN on_stack THEN des.alloc_var(va,4); cmd.load_store(va,a,4);
    ELSE va:=a;
    END;
    gen_parms(l^.l^.dw,l^.r);
    cmd.gen_access(va,4,am,sr,ofs);
    cmd.put_access(am,sr,ofs,put.g2_call_ii,put.grp2_w);
  END;
  IF l^.l^.dw^.l#NIL THEN
    des.bytes(l^.l^.dw^.l,a); ASSERT(a.am=sym.am_imm);
    cmd.func_ret(a.n);
  END;
  IF res_fpp THEN
    cmd.gen_access(fpp_sv,1,am,sr,ofs);
    cmd.put_access(am,sr,ofs,40b,09Bh,0DDh);
  END;
END gen_call;

PROCEDURE gen_if(l: pc.ref);
  VAR n: pc.ref; next,then,else: flw.node;
BEGIN
  then:=flw.new(); else:=flw.new(); next:=flw.new();
  exp.gen_condition(l^.dw,then,else);
  flw.start(then); n:=l^.l;
  WHILE n#NIL DO one(n); n:=n^.nxt END;
  flw.block^.true:=next; flw.start(else); n:=l^.r;
  WHILE n#NIL DO one(n); n:=n^.nxt END;
  flw.block^.true:=next; flw.start(next);
END gen_if;

PROCEDURE gen_for(l: pc.ref; next: flw.node);
  VAR
    loop       : flw.node;
    i,r_to,r_by: pc.ref;
    m          : vrs.val_mode;
    va         : sym.access;
    fr,to,by   : sym.access;
    n,vsz      : INTEGER;
    dsc        : cmd.cmd_desc;
BEGIN
  m:=vrs.vm(l^.l^.l^.dw);
  IF (m#vrs.vm_integer) & (m#vrs.vm_cardinal) THEN error(l,ill_type) END;
  vsz:=exp.bytes_i(l^.l^.l^.dw);
  des.designator(l^.l^.l,va);
  IF va.am=sym.am_aSTK THEN error(l,assert) END;
  exp.expression(l^.l^.r,fr); cmd.load_store(va,fr,vsz);
  i:=l^.l^.nxt; r_to:=NIL; r_by:=NIL;
  WHILE i#NIL DO
    IF i^.md=pc.exit THEN r_to:=i;
    ELSIF i^.md=pc.plus THEN r_by:=i;
    ELSE error(i,assert);
    END;
    i:=i^.nxt;
  END;
  IF r_to#NIL THEN
    exp.expression(r_to^.dw,to);
    IF to.am#sym.am_imm THEN
      n:=alloc_var(vsz,FALSE);
      cmd.load_store(vrs.vars[n],to,vsz);
      to:=vrs.vars[n];
    END;
  END;
  IF r_by#NIL THEN
    exp.expression(r_by^.r,by);
    IF by.am#sym.am_imm THEN error(r_by,assert) END;
  ELSE by:=const; by.n:=1;
  END;
  IF (r_to#NIL) & (to.am=sym.am_imm) &
     (fr.am=sym.am_imm) & (by.am=sym.am_imm) THEN
    IF by.n>=0 THEN IF fr.n>to.n THEN RETURN END;
    ELSE IF fr.n<to.n THEN RETURN END;
    END;
  END;
  loop:=flw.new();
  flw.block^.true:=loop; flw.start(loop);
  IF r_to#NIL THEN
    IF by.n>=0 THEN
      cmd.load(to,vsz); cmd.cmd_stk_m(cmd.c_sub,va); cmd.drop;
    ELSE
      cmd.load(va,vsz); cmd.cmd_stk_m(cmd.c_sub,to); cmd.drop;
    END;
    flw.block^.md:=flw.nm_cond;
    flw.block^.true:=flw.new();
    flw.block^.false:=next;
    IF m=vrs.vm_cardinal THEN flw.block^.flag:=put.c_nc
    ELSE flw.block^.flag:=put.c_ge
    END;
    flw.start(flw.block^.true);
  END;
  i:=l^.dw; WHILE i#NIL DO one(i); i:=i^.nxt END;
  cmd.cmd_m_imm(cmd.c_add,va,by.n,vsz);
  flw.block^.true:=loop;
  flw.start(next);
END gen_for;

PROCEDURE gen_loop(l: pc.ref);
  VAR i: pc.ref; loop,next,body: flw.node;
BEGIN
  next:=flw.new(); l^.adr:=INTEGER(next);
  IF l^.l#NIL THEN
    IF l^.l^.md=pc.assign THEN
      gen_for(l,next);
    ELSE
      body:=flw.new(); loop:=flw.new();
      flw.block^.true:=loop; flw.start(loop);
      exp.gen_condition(l^.l,body,next);
      flw.start(body); i:=l^.dw;
      WHILE i#NIL DO one(i); i:=i^.nxt END;
      flw.block^.true:=loop; flw.start(next);
    END;
  ELSIF l^.r#NIL THEN
    loop:=flw.new(); flw.block^.true:=loop;
    flw.start(loop); i:=l^.dw;
    WHILE i#NIL DO one(i); i:=i^.nxt END;
    exp.gen_condition(l^.r,next,loop);
    flw.start(next);
  ELSE
    loop:=flw.new(); flw.block^.true:=loop;
    flw.start(loop); i:=l^.dw;
    WHILE i#NIL DO one(i); i:=i^.nxt END;
    flw.block^.true:=loop; flw.start(next);
  END;
  l^.adr:=0;
END gen_loop;

PROCEDURE gen_exit(l: pc.ref);
BEGIN
  IF (l^.dw=NIL) OR (l^.dw^.md#pc.loop) OR (l^.dw^.adr=0) THEN
    error(l,assert);
  END;
  flw.block^.true:=flw.node(l^.dw^.adr);
  flw.start(flw.new());
END gen_exit;

PROCEDURE gen_case(l: pc.ref);
  VAR
    lbs           : DYNARR OF INTEGER;
    base,min,max  : INTEGER;
    sel_no        : INTEGER;
    sg            : BOOLEAN;
    size          : INTEGER;
  PROCEDURE var_case;
    VAR
      else,next,case: flw.node;
      k,k1          : pc.ref;
  BEGIN
    else:=flw.new(); next:=flw.new();
    const.n:=min; cmd.cmd_stk_m(cmd.c_sub,const);
    IF sg THEN flw.block^.flag:=put.c_ge;
    ELSE flw.block^.flag:=put.c_nc;
    END;
    flw.block^.false:=else; flw.block^.true:=flw.new();
    flw.block^.md:=flw.nm_cond; flw.start(flw.block^.true);
    IF size=1 THEN cmd.set_size(2,FALSE); size:=2 END;
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      put.b(put.mov_wmr); put.b(put.md_reg+put.SI+put.AX*8);
    ELSE
      put.b(put.mov_wmr); put.b(put.md_reg+put.SI+put.CX*8);
    END;
    const.n:=max-min+1; cmd.cmd_stk_m(cmd.c_sub,const); cmd.drop;
    flw.block^.flag:=put.c_c;
    flw.block^.false:=else; flw.block^.true:=flw.new();
    flw.block^.md:=flw.nm_cond; flw.start(flw.block^.true);
    case:=flw.block; case^.md:=flw.nm_case; case^.false:=else;
    NEW(case^.ctbl,max-min+1);
    mcd.move(ADR(case^.ctbl),ADR(lbs[min-base]),(max-min+1)*SIZE(INTEGER));
    NEW(case^.alts,sel_no);
    k1:=l^.l; sel_no:=0;
    WHILE k1#NIL DO
      case^.alts[sel_no]:=flw.new(); flw.start(case^.alts[sel_no]); INC(sel_no);
      k:=k1^.r;
      WHILE k#NIL DO one(k); k:=k^.nxt END;
      flw.block^.true:=next; k1:=k1^.nxt;
    END;
    flw.start(else);
    k:=l^.r;
    IF k=NIL THEN exit_trap(int_case);
    ELSE WHILE k#NIL DO one(k); k:=k^.nxt END;
    END;
    flw.block^.true:=next; flw.start(next);
  END var_case;
  PROCEDURE const_case(v: INTEGER);
    VAR k: pc.ref;
  BEGIN
    IF (v<min) OR (v>max) THEN v:=-1 ELSE v:=lbs[v-base] END;
    IF v>=0 THEN
      ASSERT(v<sel_no);
      k:=l^.l; sel_no:=0;
      WHILE sel_no<v DO k:=k^.nxt; INC(sel_no) END;
      k:=k^.r;
    ELSE
      k:=l^.r;
      IF k=NIL THEN error(l,'case without else') END;
    END;
    WHILE k#NIL DO one(k); k:=k^.nxt END;
  END const_case;
  VAR s,r: pc.ref; b: sym.access; m,n,i: INTEGER;
BEGIN
  NEW(lbs,200);
  base:=0;
  sg:=vrs.sign?(l^.dw^.dw);
  des.bytes(l^.dw^.dw,b);
  ASSERT(b.am=sym.am_imm);
  size:=b.n;
  LOOP
    min:=MAX(INTEGER); max:=MIN(INTEGER);
    FOR i:=0 TO HIGH(lbs) DO lbs[i]:=-1 END;
    sel_no:=0; s:=l^.l;
    IF s=NIL THEN
      exp.expression(l^.dw,b); const_case(0); DISPOSE(lbs); RETURN
    END;
    WHILE s#NIL DO
      IF s^.md=pc.select THEN
        r:=s^.l;
        WHILE r#NIL DO
          IF r^.md=pc.range THEN
            exp.expression(r^.l,b);
            ASSERT(b.am=sym.am_imm);
            m:=b.n; n:=b.n;
            IF r^.r#NIL THEN
              exp.expression(r^.r,b);
              ASSERT(b.am=sym.am_imm);
              m:=b.n;
            END;
            IF n<=m THEN
              IF n<min THEN min:=n END;
              IF m>max THEN max:=m END;
              IF (n>=base) & (m<=base+HIGH(lbs)) THEN
                WHILE n<=m DO
                  IF lbs[n-base]>=0 THEN error(r,'label already exist') END;
                  lbs[n-base]:=sel_no; INC(n);
                END;
              END;
            END;
          ELSE one(r)
          END;
          r:=r^.nxt;
        END;
        INC(sel_no);
      ELSE one(s)
      END;
      s:=s^.nxt;
    END;
    IF (min>=base) & (max<=base+HIGH(lbs)) THEN EXIT END;
    IF max-min>HIGH(lbs) THEN DISPOSE(lbs); NEW(lbs,max-min+1) END;
    base:=min;
  END;
  exp.expression(l^.dw,b);
  IF b.am=sym.am_imm THEN const_case(b.n);
  ELSE cmd.load(b,size); var_case;
  END;
  DISPOSE(lbs);
END gen_case;

PROCEDURE not_top;
BEGIN
  cmd.pop_reg(1);
  IF cmd.pos[cmd.stk-1]=cmd.da THEN
    CASE cmd.siz[cmd.stk-1] OF
      |1: put.b(put.grp1_bm); put.b(put.md_reg+put.AL+put.g1_not);
      |2: put.b(put.grp1_wm); put.b(put.md_reg+put.AX+put.g1_not);
      |4: put.b(put.grp1_wm); put.b(put.md_reg+put.AX+put.g1_not);
          put.b(put.grp1_wm); put.b(put.md_reg+put.DX+put.g1_not);
    END;
  ELSE
    CASE cmd.siz[cmd.stk-1] OF
      |1: put.b(put.grp1_bm); put.b(put.md_reg+put.CL+put.g1_not);
      |2: put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_not);
      |4: put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_not);
          put.b(put.grp1_wm); put.b(put.md_reg+put.BX+put.g1_not);
    END;
  END;
END not_top;

PROCEDURE gen_incl_excl(excl: BOOLEAN; i: pc.ref);
  VAR a,c: sym.access; lsz: INTEGER; sg,chk: BOOLEAN; set: pc.ref;
BEGIN
  set:=i^.l^.dw;
  IF set^.md#pc.set THEN error(i^.l,assert) END;
  des.designator_u(i^.l,a);
  chk:=i^.r^.md=pc.range_check;
  IF chk THEN exp.expression(i^.r^.r,c) ELSE exp.expression(i^.r,c) END;
  lsz:=exp.bytes_i(set);
  WITH vrs.rngs[set^.adr] DO
    imm?(set,l); imm?(set,r);
    IF c.am IN am_const THEN
      IF (c.n<l.n) OR (c.n>r.n) THEN error(i^.r,range_error);
      ELSE c.n:=INTEGER({c.n-l.n})
      END;
    ELSE
      cmd.load(c,size); c.am:=sym.am_STK;
      IF chk THEN des.check_range(set);
      ELSIF l.n#0 THEN cmd.cmd_stk_m(cmd.c_sub,l);
      END;
      des.make_bitset(lsz);
    END;
  END;
  IF excl THEN
    IF c.am IN am_const THEN
      cmd.cmd_m_imm(cmd.c_and,a,INTEGER(-BITSET(c.n)),lsz);
    ELSE not_top; cmd.cmd_m_stk(cmd.c_and,a); cmd.drop;
    END;
  ELSE
    IF c.am IN am_const THEN cmd.cmd_m_imm(cmd.c_or,a,c.n,lsz);
    ELSE cmd.cmd_m_stk(cmd.c_or,a); cmd.drop;
    END;
  END;
END gen_incl_excl;

PROCEDURE gen_inc_dec(dec: BOOLEAN; l: pc.ref);
  VAR e,d,sz: sym.access;
BEGIN
  CASE vrs.vm(l^.l^.dw) OF
    |vrs.vm_integer,vrs.vm_cardinal:
      des.designator_u(l^.l,d); des.bytes(l^.l^.dw,sz); imm?(l^.l,sz);
      IF l^.r=NIL THEN e.am:=sym.am_imm; e.n:=1
      ELSE exp.expression(l^.r,e);
      END;
      IF e.am=sym.am_imm THEN
        IF dec THEN cmd.cmd_m_imm(cmd.c_sub,d,e.n,sz.n)
        ELSE cmd.cmd_m_imm(cmd.c_add,d,e.n,sz.n)
        END;
      ELSE
        cmd.load(e,sz.n);
        IF dec THEN cmd.cmd_m_stk(cmd.c_sub,d)
        ELSE cmd.cmd_m_stk(cmd.c_add,d)
        END;
        cmd.drop;
      END;
    |vrs.vm_address:
      des.designator_u(l^.l,d);
      IF d.am IN on_stack THEN cmd.copt END;
      cmd.load(d,4);
      IF l^.r=NIL THEN e.am:=sym.am_imm; e.n:=1
      ELSE exp.expression(l^.r,e);
      END;
      IF dec THEN cmd.sub_adr(e) ELSE cmd.add_adr(e) END;
      cmd.store(d);
  ELSE error(l^.l,ill_type);
  END;
END gen_inc_dec;

PROCEDURE gen_halt(l: pc.ref);
  VAR a: sym.access;
BEGIN
  IF l^.r=NIL THEN exit_trap(int_halt);
  ELSIF l^.r^.md=pc.number THEN exit_trap(l^.r^.val);
  ELSE
    exp.expression(l^.r,a); cmd.load(a,1); cmd.hold_out;
    put.b(put.mov_ahi); put.b(4Ch); put.b(put.int); put.b(21h);
  END;
END gen_halt;

PROCEDURE gen_assert(l: pc.ref);
  VAR true,false: flw.node; a: sym.access;
BEGIN
  true:=flw.new(); false:=flw.new();
  exp.gen_condition(l^.l,true,false);
  flw.start(false);
  IF l^.r=NIL THEN exit_trap(int_assert);
  ELSIF l^.r^.md=pc.number THEN exit_trap(l^.r^.val);
  ELSE
    exp.expression(l^.r,a); cmd.load(a,1); cmd.hold_out;
    put.b(put.mov_ahi); put.b(4Ch); put.b(put.int); put.b(21h);
  END;
  flw.block^.true:=true;
  flw.start(true);
END gen_assert;

PROCEDURE gen_call_rts(l: pc.ref);
  VAR
    a,va     : sym.access;
    am,sr,ofs: INTEGER;
BEGIN
  IF (l^.md=pc.usage) & (l^.r#NIL)  THEN
    IF l^.r^.md=pc.procedure THEN gen_call_procedure(l^.r);
    ELSE error(l,'must be procedure');
    END;
  ELSE
    exp.expression(l,a);
    IF a.am IN on_stack THEN des.alloc_var(va,4); cmd.load_store(va,a,4);
    ELSE va:=a;
    END;
    cmd.gen_access(va,4,am,sr,ofs);
    cmd.put_access(am,sr,ofs,put.g2_call_ii,put.grp2_w);
  END;
END gen_call_rts;

PROCEDURE gen_call_resize(l: pc.ref; VAR a: sym.access; new: BOOLEAN);
  VAR b: sym.access; r: cmd.reg;
BEGIN
  IF new THEN
    b:=a; des.add_offset(b,4);
    const.n:=-1; cmd.load_store(b,const,4); -- high:=-1;
    IF a.am=sym.am_aSTK THEN INC(cmd.stk) END;
  END;
  cmd.load_adr(a); r:=cmd.pos[cmd.stk-1]; cmd.save; cmd.pos[cmd.stk-1]:=r;
  const.n:=4; cmd.add_adr(const); cmd.save; DEC(cmd.stk);
  IF l^.r#NIL THEN
    exp.expression(l^.r,a); cmd.stot(a,4);
  ELSE
    const.n:=0; cmd.stot(const,4);
  END;
  des.bytes(l^.l^.dw^.r,a); cmd.stot(a,4);
  gen_call_rts(l^.dw);
END gen_call_resize;

PROCEDURE gen_new(l: pc.ref);
  VAR a,b: sym.access;
BEGIN
  IF (l^.l#NIL) & vrs.pointer?(l^.l^.dw) THEN
    des.adr(l^.l,a); cmd.stot(a,4);
    des.bytes(l^.l^.dw^.dw,a); cmd.stot(a,4);
    gen_call_rts(l^.dw);
  ELSIF (l^.l#NIL) & vrs.dynarr?(l^.l^.dw) THEN
    des.designator(l^.l,a);
    IF (l^.r=NIL) OR (l^.r^.md=pc.number) & (l^.r^.val<=0) THEN
      const.n:=0; cmd.load_store(a,const,4);
      IF a.am=sym.am_aSTK THEN INC(cmd.stk) END;
      des.add_offset(a,4);
      const.n:=-1; cmd.load_store(a,const,4);
    ELSE
      gen_call_resize(l,a,TRUE);
    END;
  ELSE error(l^.l,'must be dynarr or pointer');
  END;
END gen_new;

PROCEDURE gen_resize(l: pc.ref);
  VAR a: sym.access;
BEGIN
  IF (l^.l#NIL) & vrs.dynarr?(l^.l^.dw) THEN
    des.designator(l^.l,a); gen_call_resize(l,a,FALSE);
  ELSE error(l^.l,'gen resize: must be dynarr');
  END;
END gen_resize;

PROCEDURE gen_dispose(l: pc.ref);
  VAR a: sym.access;
BEGIN
  IF (l^.l#NIL) & vrs.pointer?(l^.l^.dw) THEN
    des.adr(l^.l,a); cmd.stot(a,4);
    des.bytes(l^.l^.dw^.dw,a); cmd.stot(a,4);
    gen_call_rts(l^.dw);
  ELSIF (l^.l#NIL) & vrs.dynarr?(l^.l^.dw) THEN
    des.designator(l^.l,a); gen_call_resize(l,a,FALSE);
  ELSE error(l^.l,'must be dynarr or pointer');
  END;
END gen_dispose;

PROCEDURE one(l: pc.ref);
BEGIN
  stat_pos:=l^.pos;
  CASE l^.md OF
    |pc.var            : gen_var(l);
    |pc.proc_body      : proc_a^:=l; proc_a:=ADR(l^.r); l^.r:=NIL;
    |pc.block          : l:=l^.dw; WHILE l#NIL DO one(l); l:=l^.nxt END;
    |pc.procedure      : l^.r:=proc^.l;
    |pc.inline         :
    |pc.program_check  :
    |pc.return         : flw.mark(l^.pos); gen_return(l);
    |pc.module         : l:=l^.dw; WHILE l#NIL DO one(l); l:=l^.nxt END;
    |pc.const          :
    |pc.array          :
    |pc.array_of       :
    |pc.dynarr         :
    |pc.packed_array_of:
    |pc.packed_dynarr  :
    |pc.record         :
    |pc.packed_array   :
    |pc.packed_record  :
    |pc.enumeration    :
    |pc.profile        :
    |pc.boolean        :
    |pc.char           :
    |pc.range          :
    |pc.subtype        :
    |pc.set            :
    |pc.integer        :
    |pc.real           :
    |pc.pointer        :
    |pc.code_body      :
    |pc.call           : flw.mark(l^.pos); gen_call(l);
    |pc.assign         : flw.mark(l^.pos); gen_assign(l);
    |pc.if             : flw.mark(l^.pos); gen_if(l);
    |pc.loop           : gen_loop(l);
    |pc.exit           : gen_exit(l);
    |pc.continue       :
    |pc.inc            : flw.mark(l^.pos); gen_inc_dec(FALSE,l);
    |pc.dec            : flw.mark(l^.pos); gen_inc_dec(TRUE,l);
    |pc.case           : flw.mark(l^.pos); gen_case(l);
    |pc.incl           : flw.mark(l^.pos); gen_incl_excl(FALSE,l);
    |pc.excl           : flw.mark(l^.pos); gen_incl_excl(TRUE,l);
    |pc.halt           : flw.mark(l^.pos); gen_halt(l);
    |pc.assert         : flw.mark(l^.pos); gen_assert(l);
    |pc.new            : gen_new(l);
    |pc.resize         : gen_resize(l);
    |pc.dispose        : gen_dispose(l);
  ELSE
    error(l,assert);
  END;
  IF cmd.stk#0 THEN error(l,assert) END;
  IF fpp.stk#0 THEN error(l,assert) END;
END one;

PROCEDURE save_parms;
  VAR flag,i,j: INTEGER;
BEGIN
  IF level=0 THEN
    put.b(put.push_ds); put.b(put.seg_cs);
    put.b(put.mov_srm); put.b(put.md_abs+put.DS*8); put.w(0);
    flag:=vrs.new_str(1);
    put.b(0F6h); put.b(put.md_abs); put.w(flag); put.b(0FFh);
    put.b(put.je); put.b(2);
    put.b(put.pop_ds); put.b(put.ret_i);
    put.b(0C6h); put.b(put.md_abs); put.w(flag); put.b(0FFh);
    FOR i:=1 TO vrs.mdls_no-1 DO
      j:=vrs.new_proc();
      vrs.prcs_nm[j]:=vrs.mdls_nm[i];
      WITH vrs.prcs[j] DO
        export:=TRUE; mod:=i; ofs:=vrs.new_str(4);
        ASSERT(ofs<=0FFFCh);
        put.b(put.grp2_w); put.b(put.g2_call_ii+put.md_abs); put.w(ofs);
      END;
    END;
  ELSE
    WITH vrs.prcs[proc^.l^.adr] DO
      IF export THEN
        put.b(put.push_ds); put.b(put.seg_cs);
        put.b(put.mov_srm); put.b(put.md_abs+put.DS*8); put.w(0);
      END;
      put.b(put.push_bp);
      put.b(put.mov_wrm); put.b(put.md_reg+put.SP+put.BP*8);
      IF loc_sz#0 THEN
        IF ODD(loc_sz) THEN INC(loc_sz) END;
        put.b(put.imm_wm); put.b(put.i_sub+put.md_reg+put.SP); put.w(loc_sz);
      END;
    END;
  END;
END save_parms;

PROCEDURE copy_parms;
  PROCEDURE move(ofs: INTEGER);
  BEGIN
    put.b(put.sub_wrm); put.b(put.md_reg+put.CX+put.SP*8);
    put.b(put.mov_wrm); put.b(put.md_w+put.rm_bp+put.SI*8); put.w(ofs);
    put.b(put.mov_wmr); put.b(put.md_w+put.rm_bp+put.SP*8); put.w(ofs);
    put.b(put.mov_wrm); put.b(put.md_reg+put.SP+put.DI*8);
    put.b(put.push_ds);
    put.b(put.mov_srm); put.b(put.md_w+put.rm_bp+put.DS*8); put.w(ofs+2);
    put.b(put.mov_msr); put.b(put.md_w+put.rm_bp+put.SS*8); put.w(ofs+2);
    put.b(put.mov_msr); put.b(put.md_reg+put.BX+put.SS*8);
    put.b(put.mov_srm); put.b(put.md_reg+put.BX+put.ES*8);
    put.b(put.cld); put.b(put.rep_z); put.b(put.movs_b);
    put.b(put.pop_ds);
  END move;
  VAR l,t: pc.ref; ofs: INTEGER; sz: sym.access;
BEGIN
  IF level=0 THEN RETURN END;
  l:=proc^.l^.l^.dw;
  WHILE l#NIL DO
    IF l^.md#pc.var THEN
      one(l);
    ELSE
      t:=l^.l;
      IF t=NIL THEN error(l,assert)
      ELSIF t^.md=pc.val_prm THEN -- nothing
      ELSIF t^.md=pc.var_prm THEN -- nothing
      ELSIF vrs.array_of?(t) THEN
        ASSERT(vrs.vars[l^.adr].am=sym.am_PB8);
        ofs:=vrs.vars[l^.adr].disp; cmd.parm_ofs(ofs,level);
        des.bytes(t^.r,sz); ASSERT(sz.am=sym.am_imm);
        IF sz.n#1 THEN
          put.b(put.mov_axi); put.w(1);
          put.b(put.add_wrm); put.b(put.md_w+put.rm_bp+put.AX*8); put.w(ofs+4);
          put.b(put.mov_cxi); put.w(sz.n);
          put.b(put.grp1_wm); put.b(put.md_reg+put.CX+put.g1_mul);
          put.b(put.mov_wrm); put.b(put.md_reg+put.AX+put.CX*8);
        ELSE
          put.b(put.mov_cxi); put.w(1);
          put.b(put.add_wrm); put.b(put.md_w+put.rm_bp+put.CX*8); put.w(ofs+4);
        END;
        move(ofs);
      ELSIF vrs.vars[l^.adr].am=sym.am_aPB THEN
        ASSERT(vrs.vars[l^.adr].disp=0);
        ofs:=vrs.vars[l^.adr].n; cmd.parm_ofs(ofs,level);
        des.bytes(t,sz); ASSERT(sz.am=sym.am_imm);
        put.b(put.mov_cxi); put.w(sz.n);
        move(ofs);
      END;
    END;
    l:=l^.nxt;
  END;
END copy_parms;

PROCEDURE proc_body(main: BOOLEAN);
  VAR l: pc.ref; n: INTEGER;
BEGIN
  ASSERT(proc^.md=pc.proc_body);
  l:=proc^.dw;
  WHILE l#NIL DO one(l); l:=l^.nxt END;
  IF level=0 THEN
    put.b(put.pop_ds);
    IF main THEN exit_trap(0) ELSE put.b(put.ret_i) END;
  ELSIF proc^.l^.l^.l#NIL THEN
    exit_trap(int_return);
  ELSE
    gen_return_cmd;
  END;
END proc_body;

PROCEDURE do_flow(main: BOOLEAN): flw.node;
  VAR first,second: flw.node;
BEGIN
  ASSERT(proc#NIL);
  ASSERT(proc^.md=pc.proc_body);
  ASSERT(proc^.l^.md=pc.procedure);
  ASSERT(vrs.prcs[proc^.l^.adr].mod=0);
  level:=vrs.prcs[proc^.l^.adr].lvl;
  stat_pos:=0;
  flw.block:=NIL;
  second:=flw.new(); flw.start(second);
  proc_body(main);
  first:=flw.new(); flw.start(first);
  save_parms;
  copy_parms;
  flw.block^.true:=second;
  flw.finish;
  RETURN first;
END do_flow;

PROCEDURE gen_code(m: pc.ref; main: BOOLEAN);
  VAR n: INTEGER; f: flw.node; nm: ARRAY [0..79] OF CHAR;
BEGIN
  proc:=m; proc^.r:=NIL; proc_a:=ADR(proc^.r);
  WHILE proc#NIL DO
    pc.get_name(proc^.l^.nm,nm);
    tty.print('%s',nm); tty.erase_line(0); tty.Write(15c);
    cmd.proc:=proc^.l;
    n:=vrs.prcs[proc^.l^.adr].no;
    f:=do_flow(main);
    flw.proc_flow[n]:=f;
    proc:=proc^.r;
  END;
END gen_code;

BEGIN
  const.am:=sym.am_imm;
  exp.gen_call:=gen_call;
END inStat.
