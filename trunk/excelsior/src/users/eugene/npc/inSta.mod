IMPLEMENTATION MODULE inSta;

IMPORT inCmd,inExp,pcK,pcS,pcO,FIO,pcM;
FROM Storage IMPORT ALLOCATE;

CONST
  cmd ::= inCmd;
  exp ::= inExp;
  pc  ::= pcK;
  bio ::= FIO;

TYPE
  val_mode_set = SET OF exp.val_mode;
  adr_mode_set = SET OF cmd.adr_mode;
  byte         = SET OF [0..07];
  word         = SET OF [0..15];
  dword        = SET OF [0..31];
  glo_list     = POINTER TO glo_list_rec;
  glo_list_rec = RECORD
    adr   : ADDRESS;
    size  : CARDINAL;
    ofs   : CARDINAL;
    next  : glo_list;
  END;
  pcc_list     = POINTER TO pcc_list_rec;
  pcc_list_rec = RECORD
    obj   : pc.OBJECT;
    ofs   : CARDINAL;
    next  : pcc_list;
  END;

CONST
  int_case   = 45H;
  int_return = 46H;
  int_halt   = 47H;
  int_assert = 48H;
  int_size   = 4AH;

  val_size_4 = val_mode_set {
    exp.vm_integer,exp.vm_cardinal,
    exp.vm_boolean,exp.vm_set,exp.vm_address
  };

  on_stack=adr_mode_set{cmd.am_STK,cmd.am_aSTK};
  am_const=adr_mode_set{cmd.am_imm,cmd.am_Gimm,cmd.am_Limm};
  am_value=adr_mode_set{cmd.am_imm,cmd.am_STK,cmd.am_FPP};

VAR
  proc       : pc.OBJECT;
  module     : pc.OBJECT;
  const      : cmd.access;
  usage      : pc.NODE;
  globals    : glo_list;
  pconsts    : pcc_list;
  proc_no    : CARDINAL;

PROCEDURE error(n: INTEGER);
BEGIN
  pcS.err(n); pcM.abort;
END error;

PROCEDURE error_ps(ps: LONGINT; n: INTEGER);
BEGIN
  pcS.txtpos:=ps; pcS.err(n); pcM.abort;
END error_ps;

PROCEDURE imm(VAR a: cmd.access);
BEGIN
  IF a.am IN am_const THEN RETURN END;
  error(87);
END imm;

PROCEDURE alloc_tmp_var(VAR a: cmd.access; sz: CARDINAL);
  VAR e: exp.EXT;
BEGIN
  IF cmd.level=0 THEN
    e:=module^.ext; a.am:=cmd.am_G; a.disp:=e^.n; INC(e^.n,LONGINT(sz));
    IF e^.n>=10000H THEN error(220) END;
    a.mod:=0;
  ELSE
    e:=proc^.ext; a.am:=cmd.am_L; DEC(e^.n,LONGINT(sz)); a.disp:=e^.n;
    IF e^.n<=-10000H THEN error(220) END;
    a.level:=cmd.level;
  END;
END alloc_tmp_var;

PROCEDURE glo_const(ofs: LONGINT; adr: ADDRESS; size: LONGINT);
  VAR n: glo_list;
BEGIN
  NEW(n); n^.ofs:=CARDINAL(ofs); n^.size:=CARDINAL(size); n^.adr:=adr;
  n^.next:=globals; globals:=n;
END glo_const;

PROCEDURE new_glo_const(sz: CARDINAL; VAR a: ADDRESS; VAR ofs: CARDINAL);
  VAR ext: exp.EXT;
BEGIN
  ext:=module^.ext;
  ofs:=CARDINAL(ext^.n); INC(ext^.n,LONGINT(sz));
  ALLOCATE(a,sz);
  glo_const(LONGINT(ofs),a,LONGINT(sz));
END new_glo_const;

PROCEDURE new_proc_const(o: pc.OBJECT; VAR ofs: CARDINAL);
  VAR ext: exp.EXT; n: pcc_list;
BEGIN
  ext:=module^.ext;
  ofs:=CARDINAL(ext^.n); INC(ext^.n,4);
  NEW(n); n^.obj:=o; n^.ofs:=ofs;
  n^.next:=pconsts; pconsts:=n;
END new_proc_const;

PROCEDURE profile_psz(l: pc.STRUCT): CARDINAL;
BEGIN
  RETURN CARDINAL(l^.adr);
END profile_psz;

PROCEDURE trap(n: CARDINAL);
BEGIN
  IF n=3 THEN cmd.b(cmd.int_3) ELSE cmd.b(cmd.int); cmd.b(n) END;
  cmd.b(cmd.nop);
END trap;

PROCEDURE exit_trap(n: CARDINAL);
BEGIN
  cmd.b(cmd.mov_axi); cmd.w(4C00H+n MOD 100H); trap(21H);
END exit_trap;

PROCEDURE gen_assign(l: pc.NODE);
  VAR d,e,lsz,rsz: cmd.access; m: exp.val_mode; sz: CARDINAL;
BEGIN
  IF l^.l=NIL THEN
    l^.l:=usage; usage^.mode:=pc.var;
    usage^.type:=l^.obj^.type; usage^.obj:=l^.obj;
  END;
  exp.bytes_u(l^.l,lsz); m:=exp.vm(l^.r^.type);
  IF m = exp.vm_real THEN
    exp.designator(l^.l,d); exp.expression(l^.r,e);
    sz:=CARDINAL(l^.r^.type^.size);
    IF d.am IN am_value THEN error_ps(l^.pos,233) END;
    IF (e.am#cmd.am_FPP) & (sz=4) & (rsz.n=4) THEN
      cmd.load_store(d,e,sz);
    ELSE
      cmd.fpp_load(cmd.f_mov,e,sz); cmd.fpp_store(d,CARDINAL(lsz.n));
    END;
  ELSIF m IN val_size_4 THEN
    exp.designator(l^.l,d);
    IF d.am IN am_value THEN error_ps(l^.pos,233) END;
    exp.range_check(l^.r,e,l^.l^.type,TRUE);
    cmd.load_store(d,e,CARDINAL(l^.l^.type^.size));
  ELSIF l^.r^.mode=pc.char2str THEN
    exp.designator_u(l^.l,d); const.n:=l^.r^.r^.val;
    cmd.load_store(d,const,2);
  ELSE
    IF lsz.am IN am_const THEN
      exp.bytes_u(l^.r,rsz);
      IF rsz.am IN am_const THEN
        IF rsz.n>lsz.n THEN rsz.n:=lsz.n END;
        cmd.stot(rsz,4);
      ELSE
        cmd.load(rsz,4); cmd.load(lsz,4); cmd.swap;
        cmd.cmd_stk_m(cmd.c_sub,cmd.top);
        INC(cmd.stk); cmd.swap; DEC(cmd.stk);
        cmd.b(cmd.jnb); cmd.b(3); trap(4);
        cmd.stot(cmd.top,4);
      END;
    ELSE
      cmd.load(lsz,4);
      exp.bytes_u(l^.r,rsz);
      IF rsz.am IN am_const THEN
        cmd.cmd_stk_m(cmd.c_sub,rsz); DEC(cmd.stk);
        cmd.b(cmd.jnb); cmd.b(3); trap(4);
        cmd.stot(rsz,4);
      ELSE
        cmd.load(rsz,4); cmd.cmd_stk_m(cmd.c_sub,cmd.top);
        INC(cmd.stk); cmd.swap; DEC(cmd.stk);
        cmd.b(cmd.jnb); cmd.b(3); trap(4);
        cmd.stot(cmd.top,4);
      END;
    END;
    exp.designator_u(l^.l,d); cmd.stot_adr(d);
    exp.designator_u(l^.r,e); cmd.stot_adr(e);
    cmd.call_rts(14);
  END;
END gen_assign;

PROCEDURE gen_return_cmd;
  VAR n: CARDINAL;
BEGIN
  n:=profile_psz(proc^.type);
  cmd.b(cmd.mov_wrm); cmd.b(cmd.md_reg+cmd.BP+cmd.SP*8); cmd.b(cmd.pop_bp);
  IF (proc^.mode=pc.xproc) OR (proc^.mode=pc.iproc) THEN
    cmd.b(cmd.pop_ds); cmd.b(cmd.pop_di); cmd.b(cmd.pop_si);
    IF n=0 THEN cmd.b(cmd.ret_i) ELSE cmd.b(cmd.ret_isp); cmd.w(n) END;
  ELSIF proc^.scope>0 THEN
    cmd.b(cmd.ret_sp); cmd.w(n+2);
  ELSE
    IF n=0 THEN cmd.b(cmd.ret) ELSE cmd.b(cmd.ret_sp); cmd.w(n) END;
  END;
END gen_return_cmd;

PROCEDURE gen_return(l: pc.NODE);
  VAR e,sz: cmd.access;
BEGIN
  IF proc^.type^.base^.mode#pc.undef THEN
    exp.range_check(l^.l,e,proc^.type^.base,TRUE);
    cmd.load(e,CARDINAL(proc^.type^.base^.size));
    cmd.hold_out;
  END;
  gen_return_cmd;
  cmd.start(cmd.new_node());
END gen_return;

PROCEDURE val_array(VAR a: cmd.access; sz: LONGINT; v: pc.NODE): BOOLEAN;
BEGIN
  IF NOT (v^.type^.mode IN pc.ARRs) THEN RETURN FALSE END;
  IF sz#v^.type^.base^.size THEN RETURN FALSE END;
  exp.high(v,a); RETURN TRUE;
END val_array;

PROCEDURE gen_parms(fp: pc.OBJECT; ap: pc.NODE);
  CONST structs = pc.ARRs+pc.Forms{pc.record,pc.longreal};
  VAR sz: LONGINT; a: cmd.access;
BEGIN
  WHILE ap#NIL DO
    IF fp^.type^.mode=pc.array_of THEN
      sz:=fp^.type^.base^.size;
      IF NOT val_array(a,sz,ap) THEN
        exp.bytes_u(ap,a);
        IF a.am IN am_const THEN
          a.n:=a.n DIV sz - 1;
        ELSE
          cmd.load(a,4); a.am:=cmd.am_STK;
          const.n:=sz; cmd.idiv(const);
          const.n:=1; cmd.cmd_stk_m(cmd.c_sub,const);
        END;
      END;
      cmd.stot(a,4);
      exp.designator_u(ap,a);
      cmd.stot_adr(a);
    ELSIF (fp^.mode=pc.varpar) OR (fp^.type^.mode IN structs) THEN
      exp.designator(ap,a);
      IF ap^.type^.mode=pc.array_of THEN cmd.stot(a,4) ELSE cmd.stot_adr(a) END;
    ELSIF fp^.type^.size<=2 THEN
      exp.range_check(ap,a,fp^.type,TRUE);
      cmd.stot(a,CARDINAL(fp^.type^.size));
    ELSIF fp^.type^.size=4 THEN
      exp.range_check(ap,a,fp^.type,TRUE);
      IF a.am=cmd.am_FPP THEN alloc_tmp_var(a,4); cmd.fpp_store(a,4) END;
      cmd.stot(a,4);
    ELSE error_ps(fp^.pos,233);
    END;
    ap:=ap^.next; fp:=fp^.next;
  END;
END gen_parms;

PROCEDURE gen_call_procedure(prc: pc.OBJECT);
  VAR n,j: CARDINAL; next: cmd.node;
BEGIN
  IF (prc^.mode=pc.xproc) OR (prc^.mode=pc.iproc) THEN
    cmd.b(cmd.call_id); cmd.fixup;
    cmd.block^.fixup^.md:=cmd.fm_far_call;
    cmd.block^.fixup^.obj:=prc;
    cmd.w(0); cmd.w(0);
  ELSIF prc^.scope>0 THEN
    n:=CARDINAL(prc^.scope);
    IF n=cmd.level THEN
      cmd.b(cmd.push_bp)
    ELSIF n+1=cmd.level THEN
      cmd.b(cmd.grp2_w); cmd.b(cmd.md_b+cmd.rm_bp+cmd.g2_push); cmd.b(4);
    ELSE
      cmd.b(cmd.mov_wrm); cmd.b(cmd.md_b+cmd.rm_bp+cmd.SI*8); cmd.b(4);
      FOR j:=n+3 TO cmd.level DO
        cmd.b(cmd.seg_ss);
        cmd.b(cmd.mov_wrm); cmd.b(cmd.md_b+cmd.rm_si+cmd.SI*8); cmd.b(4);
      END;
      cmd.b(cmd.seg_ss);
      cmd.b(cmd.grp2_w); cmd.b(cmd.md_b+cmd.rm_si+cmd.g2_push); cmd.b(4);
    END;
    cmd.b(cmd.call); cmd.fixup;
    cmd.block^.fixup^.md:=cmd.fm_call;
    cmd.block^.fixup^.obj:=prc; cmd.w(0);
  ELSE
    cmd.b(cmd.call); cmd.fixup;
    cmd.block^.fixup^.md:=cmd.fm_call;
    cmd.block^.fixup^.obj:=prc; cmd.w(0);
  END;
END gen_call_procedure;

PROCEDURE gen_call(l: pc.NODE);
  VAR
    prc         : pc.OBJECT;
    a,va,fpp_sv : cmd.access;
    res_fpp     : BOOLEAN;
    am,sr,ofs   : CARDINAL;
    p_type      : pc.STRUCT;
    e           : exp.EXT;
    pb          : POINTER TO ARRAY [0..0FFFEH] OF BYTE;
BEGIN
  res_fpp:=cmd.fpp_stk>0;
  IF res_fpp THEN
    alloc_tmp_var(fpp_sv,94);
    cmd.gen_access(fpp_sv,1,am,sr,ofs);
    cmd.put_access2(am,sr,ofs,60B,09BH,0DDH);
  END;
  cmd.save;
  IF l^.l=NIL THEN
    prc:=l^.obj; p_type:=prc^.type;
    IF prc^.mode IN {pc.proc,pc.xproc,pc.iproc} THEN
      gen_parms(p_type^.next,l^.r);
      gen_call_procedure(prc);
    ELSIF prc^.mode=pc.cproc THEN
      gen_parms(p_type^.next,l^.r);
      e:=prc^.ext; pb:=e^.buf;
      FOR am:=0 TO CARDINAL(e^.n-1) DO cmd.b(word(pb^[am])) END;
    ELSE error_ps(l^.pos,233);
    END;
  ELSE
    p_type:=l^.l^.type;
    exp.expression(l^.l,a);
    IF a.am IN on_stack THEN alloc_tmp_var(va,4); cmd.load_store(va,a,4);
    ELSE va:=a;
    END;
    gen_parms(p_type^.next,l^.r);
    cmd.gen_access(va,4,am,sr,ofs);
    cmd.put_access(am,sr,ofs,cmd.g2_call_ii,cmd.grp2_w);
  END;
  IF p_type^.base^.mode#pc.undef THEN
    cmd.func_ret(CARDINAL(p_type^.base^.size));
  END;
  IF res_fpp THEN
    cmd.gen_access(fpp_sv,1,am,sr,ofs);
    cmd.put_access2(am,sr,ofs,40B,09BH,0DDH);
  END;
END gen_call;

PROCEDURE gen_statement(l: pc.NODE); FORWARD;

PROCEDURE stat_seq(l: pc.NODE);
BEGIN
  WHILE l#NIL DO gen_statement(l); l:=l^.next END;
END stat_seq;

PROCEDURE gen_if(l: pc.NODE);
  VAR next,then,else: cmd.node; m: pc.NODE;
BEGIN
  next:=cmd.new_node();
  m:=l^.l;
  REPEAT
    IF m^.mode#pc.if THEN error_ps(l^.pos,233) END;
    then:=cmd.new_node(); else:=cmd.new_node();
    exp.gen_condition(m^.l,then,else);
    cmd.start(then); stat_seq(m^.r);
    cmd.block^.goto:=next; cmd.start(else);
    m:=m^.next;
  UNTIL m=NIL;
  stat_seq(l^.r);
  cmd.block^.goto:=next; cmd.start(next);
END gen_if;

PROCEDURE gen_for(l: pc.NODE);
  VAR
    loop,next  : cmd.node;
    m          : exp.val_mode;
    va         : cmd.access;
    fr,to,by   : cmd.access;
    n,vsz      : CARDINAL;
  PROCEDURE cmp;
    VAR c: cmd.condition;
  BEGIN
    IF m=exp.vm_cardinal THEN c:=cmd.c_nc ELSE c:=cmd.c_ge END;
    IF vsz<4 THEN
      IF to.am IN am_const THEN
        cmd.cmd_m_imm(cmd.c_cmp,va,to.n,vsz);
      ELSE
        cmd.load(va,vsz); cmd.cmd_stk_m(cmd.c_cmp,to); cmd.drop;
      END;
      IF by.n>=0 THEN
        IF m=exp.vm_cardinal THEN c:=cmd.c_cz ELSE c:=cmd.c_le END;
      END;
    ELSIF by.n>=0 THEN
      cmd.load(to,vsz); cmd.cmd_stk_m(cmd.c_sub,va); cmd.drop;
    ELSE
      cmd.load(va,vsz); cmd.cmd_stk_m(cmd.c_sub,to); cmd.drop;
    END;
    cmd.block^.md:=cmd.nm_cond;
    cmd.block^.goto:=loop;
    cmd.block^.else:=next;
    cmd.block^.flag:=c;
  END cmp;
BEGIN
  m:=exp.vm(l^.obj^.type);
  IF (m#exp.vm_integer) & (m#exp.vm_cardinal) THEN error_ps(l^.pos,32) END;
  vsz:=CARDINAL(l^.obj^.type^.size);
  exp.gen_access(l^.obj,va);
  IF va.am=cmd.am_aSTK THEN error_ps(l^.pos,233) END;
  exp.range_check(l^.l^.l,fr,l^.obj^.type,TRUE);
  cmd.load_store(va,fr,vsz);
  exp.range_check(l^.l^.r,to,l^.obj^.type,TRUE);
  IF NOT (to.am IN am_const) THEN
    alloc_tmp_var(by,vsz);
    cmd.load_store(by,to,vsz);
    to:=by;
  END;
  by:=const; by.n:=l^.l^.pos;
  loop:=cmd.new_node(); next:=cmd.new_node();
  IF (to.am IN am_const) & (fr.am IN am_const) THEN
    IF by.n>=0 THEN IF fr.n>to.n THEN RETURN END;
    ELSE IF fr.n<to.n THEN RETURN END;
    END;
    cmd.block^.goto:=loop;
  ELSE cmp;
  END;
  cmd.start(loop);
  stat_seq(l^.r);
  cmd.cmd_m_imm(cmd.c_add,va,by.n,vsz);
  cmp;
  cmd.start(next);
END gen_for;

PROCEDURE gen_loop(l: pc.NODE);
  VAR loop,next: cmd.node;
BEGIN
  loop:=cmd.new_node();
  next:=cmd.new_node();
  cmd.block^.goto:=loop;
  cmd.start(loop);
  l^.l:=ADDRESS(next);
  stat_seq(l^.r);
  l^.l:=NIL;
  cmd.block^.goto:=loop;
  cmd.start(next);
END gen_loop;

PROCEDURE gen_while(l: pc.NODE);
  VAR loop,next,body: cmd.node;
BEGIN
  loop:=cmd.new_node();
  next:=cmd.new_node();
  body:=cmd.new_node();
  cmd.block^.goto:=loop;
  cmd.start(loop);
  exp.gen_condition(l^.l,body,next);
  cmd.start(body);
  stat_seq(l^.r);
  cmd.block^.goto:=loop;
  cmd.start(next);
END gen_while;

PROCEDURE gen_repeat(l: pc.NODE);
  VAR loop,next: cmd.node;
BEGIN
  loop:=cmd.new_node();
  next:=cmd.new_node();
  cmd.block^.goto:=loop;
  cmd.start(loop);
  stat_seq(l^.l);
  exp.gen_condition(l^.r,next,loop);
  cmd.start(next);
END gen_repeat;

PROCEDURE gen_exit(l: pc.NODE);
BEGIN
  cmd.block^.goto:=cmd.node(l^.r^.l);
  cmd.start(cmd.new_node());
END gen_exit;

PROCEDURE gen_case(l: pc.NODE);
  VAR
    min,max       : LONGINT;
    sg            : BOOLEAN;
    size,i,ofs    : CARDINAL;
    else,next     : cmd.node;
    alt,case      : cmd.node;
    s,r           : pc.NODE;
    m,n           : LONGINT;
    val           : cmd.access;
    cons          : BOOLEAN;
BEGIN
  exp.expression(l^.l,val);
  cons:=val.am IN am_const;
  min:=l^.r^.next^.a;
  max:=l^.r^.next^.b;
  sg:=exp.sign(l^.l^.type);
  size:=CARDINAL(l^.l^.type^.size);
  IF NOT cons THEN
    cmd.load(val,size);
    else:=cmd.new_node(); next:=cmd.new_node();
    const.n:=min; cmd.cmd_stk_m(cmd.c_sub,const);
    IF sg THEN cmd.block^.flag:=cmd.c_ge;
    ELSE cmd.block^.flag:=cmd.c_nc;
    END;
    cmd.block^.else:=else; cmd.block^.goto:=cmd.new_node();
    cmd.block^.md:=cmd.nm_cond; cmd.start(cmd.block^.goto);
    IF size=1 THEN cmd.set_size(2,FALSE); size:=2 END;
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      cmd.b(cmd.mov_wmr); cmd.b(cmd.md_reg+cmd.SI+cmd.AX*8);
    ELSE
      cmd.b(cmd.mov_wmr); cmd.b(cmd.md_reg+cmd.SI+cmd.CX*8);
    END;
    const.n:=max-min+1; cmd.cmd_stk_m(cmd.c_sub,const); cmd.drop;
    cmd.block^.flag:=cmd.c_c;
    cmd.block^.else:=else; cmd.block^.goto:=cmd.new_node();
    cmd.block^.md:=cmd.nm_cond; cmd.start(cmd.block^.goto);
    case:=cmd.block; case^.md:=cmd.nm_case; case^.goto:=else;
    case^.clen:=CARDINAL(max-min+1);
    new_glo_const(case^.clen*2,case^.jtbl,ofs);
    ALLOCATE(case^.ctbl,SIZE(cmd.node)*case^.clen);
    FOR i:=0 TO case^.clen-1 DO case^.ctbl^[i]:=else END;
    cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+cmd.SI+cmd.s_shl);
    cmd.b(cmd.grp2_w); cmd.b(cmd.md_w+cmd.g2_jmp_i+cmd.rm_si); cmd.w(ofs);
    cmd.finish;
  END;
  s:=l^.r^.l;
  WHILE s#NIL DO
    IF s^.mode#pc.casedo THEN error_ps(s^.pos,233) END;
    IF NOT cons THEN
      alt:=cmd.new_node(); cmd.start(alt);
      stat_seq(s^.r); cmd.block^.goto:=next;
    END;
    r:=s^.l;
    WHILE r#NIL DO
      IF r^.mode#pc.pair THEN error_ps(r^.pos,233) END;
      n:=r^.a; m:=r^.b;
      IF cons THEN
        IF (val.n>=n) & (val.n<=m) THEN stat_seq(s^.r); RETURN END;
      ELSE
        WHILE n<=m DO
          IF case^.ctbl^[CARDINAL(n-min)]#else THEN error_ps(r^.pos,126) END;
          case^.ctbl^[CARDINAL(n-min)]:=alt; INC(n);
        END;
      END;
      r:=r^.next;
    END;
    s:=s^.next;
  END;
  IF cons THEN
    IF l^.r^.mode=pc.casedo THEN exit_trap(int_case);
    ELSE stat_seq(l^.r^.r);
    END;
  ELSE
    cmd.start(else);
    IF l^.r^.mode=pc.casedo THEN exit_trap(int_case);
    ELSE stat_seq(l^.r^.r);
    END;
    cmd.block^.goto:=next;
    cmd.start(next);
  END;
END gen_case;

PROCEDURE not_top;
BEGIN
  cmd.pop_reg(1);
  IF cmd.pos[cmd.stk-1]=cmd.da THEN
    CASE cmd.siz[cmd.stk-1] OF
      |1: cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.AL+cmd.g1_not);
      |2: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.AX+cmd.g1_not);
      |4: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.AX+cmd.g1_not);
          cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.DX+cmd.g1_not);
    END;
  ELSE
    CASE cmd.siz[cmd.stk-1] OF
      |1: cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_not);
      |2: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_not);
      |4: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_not);
          cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.BX+cmd.g1_not);
    END;
  END;
END not_top;

PROCEDURE gen_incl_excl(l: pc.NODE);
  VAR a,c: cmd.access; lsz,v_sz: CARDINAL; set: pc.STRUCT;
BEGIN
  exp.designator_u(l^.l,a);
  set:=l^.l^.type; lsz:=CARDINAL(set^.size);
  exp.bounds_check(l^.r,c,set,l^.sub=pc.rcheck,v_sz);
  IF c.am IN am_const THEN c.n:=LONGINT(dword{CARDINAL(c.n)});
  ELSE exp.make_bitset(lsz,v_sz);
  END;
  IF l^.mode=pc.excl THEN
    IF c.am IN am_const THEN
      cmd.cmd_m_imm(cmd.c_and,a,LONGINT(dword(c.n)/dword{0..31}),lsz);
    ELSE
      not_top; cmd.cmd_m_stk(cmd.c_and,a); cmd.drop;
    END;
  ELSE
    IF c.am IN am_const THEN cmd.cmd_m_imm(cmd.c_or,a,c.n,lsz);
    ELSE cmd.cmd_m_stk(cmd.c_or,a); cmd.drop;
    END;
  END;
END gen_incl_excl;

PROCEDURE gen_inc_dec(l: pc.NODE);
  VAR e,d: cmd.access; sz,sz_e: CARDINAL; sg: BOOLEAN;
BEGIN
  CASE exp.vm(l^.l^.type) OF
    |exp.vm_integer,exp.vm_cardinal:
      exp.designator_u(l^.l,d); sz:=CARDINAL(l^.l^.type^.size);
      IF l^.r=NIL THEN e.am:=cmd.am_imm; e.n:=1
      ELSE exp.expression(l^.r,e);
      END;
      IF e.am=cmd.am_imm THEN
        IF l^.sub=pc.dec THEN cmd.cmd_m_imm(cmd.c_sub,d,e.n,sz)
        ELSE cmd.cmd_m_imm(cmd.c_add,d,e.n,sz)
        END;
      ELSE
        sz_e:=CARDINAL(l^.r^.type^.size);
        cmd.load(e,sz_e);
        IF sz_e<sz THEN cmd.set_size(sz,exp.sign(l^.r^.type)) END;
        IF l^.mode=pc.dec THEN cmd.cmd_m_stk(cmd.c_sub,d)
        ELSE cmd.cmd_m_stk(cmd.c_add,d)
        END;
        cmd.drop;
      END;
    |exp.vm_address:
      exp.designator_u(l^.l,d);
      IF d.am IN on_stack THEN cmd.copt END;
      cmd.load(d,4);
      IF l^.r=NIL THEN e.am:=cmd.am_imm; e.n:=1;
      ELSE exp.expression(l^.r,e);
      END;
      sz:=CARDINAL(l^.r^.type^.size); sg:=exp.sign(l^.r^.type);
      IF l^.mode=pc.dec THEN cmd.sub_adr(e,sz,sg)
      ELSE cmd.add_adr(e,sz,sg)
      END;
      cmd.store(d);
  END;
END gen_inc_dec;

PROCEDURE gen_halt(l: pc.NODE);
  VAR a: cmd.access;
BEGIN
  IF l^.r=NIL THEN exit_trap(int_halt); RETURN END;
  exp.expression(l^.r,a);
  IF a.am IN am_const THEN
    exit_trap(CARDINAL(a.n));
  ELSE
    cmd.load(a,1); cmd.hold_out;
    cmd.b(cmd.mov_ahi); cmd.b(4CH); trap(21H);
  END;
END gen_halt;

PROCEDURE gen_assert(l: pc.NODE);
  VAR true,false: cmd.node; a: cmd.access;
BEGIN
  true:=cmd.new_node(); false:=cmd.new_node();
  exp.gen_condition(l^.l,true,false);
  cmd.start(false);
  IF l^.r=NIL THEN
    exit_trap(int_assert);
  ELSE
    exp.expression(l^.r,a);
    IF a.am IN am_const THEN exit_trap(CARDINAL(a.n));
    ELSE
      cmd.load(a,1); cmd.hold_out;
      cmd.b(cmd.mov_ahi); cmd.b(4CH); trap(21H);
    END;
  END;
  cmd.block^.goto:=true;
  cmd.start(true);
END gen_assert;

(*
PROCEDURE gen_call_rts(l: pc.ref);
  VAR
    a,va     : cmd.access;
    am,sr,ofs: INTEGER;
BEGIN
  IF (l^.md=pc.usage) & (l^.r#NIL)  THEN
    IF l^.r^.md=pc.procedure THEN gen_call_procedure(l^.r);
    ELSE error(l,'must be procedure');
    END;
  ELSE
    exp.expression(l,a);
    IF a.am IN on_stack THEN exp.alloc_var(va,4); cmd.load_store(va,a,4);
    ELSE va:=a;
    END;
    cmd.gen_access(va,4,am,sr,ofs);
    cmd.put_access(am,sr,ofs,cmd.g2_call_ii,cmd.grp2_w);
  END;
END gen_call_rts;

PROCEDURE gen_call_resize(l: pc.ref; VAR a: cmd.access; new: BOOLEAN);
  VAR b: cmd.access; r: cmd.reg;
BEGIN
  IF new THEN
    b:=a; exp.add_offset(b,4);
    const.n:=-1; cmd.load_store(b,const,4); -- high:=-1;
    IF a.am=cmd.am_aSTK THEN INC(cmd.stk) END;
  END;
  cmd.load_adr(a); r:=cmd.pos[cmd.stk-1]; cmd.save; cmd.pos[cmd.stk-1]:=r;
  const.n:=4; cmd.add_adr(const); cmd.save; DEC(cmd.stk);
  IF l^.r#NIL THEN
    exp.expression(l^.r,a); cmd.stot(a,4);
  ELSE
    const.n:=0; cmd.stot(const,4);
  END;
  exp.bytes(l^.l^.dw^.r,a); cmd.stot(a,4);
  gen_call_rts(l^.dw);
END gen_call_resize;
*)

PROCEDURE gen_new(l: pc.NODE);
  VAR a,b: cmd.access;
BEGIN
(*
  IF (l^.l#NIL) & exp.pointer?(l^.l^.dw) THEN
    exp.adr(l^.l,a); cmd.stot(a,4);
    exp.bytes(l^.l^.dw^.dw,a); cmd.stot(a,4);
    gen_call_rts(l^.dw);
  ELSIF (l^.l#NIL) & exp.dynarr?(l^.l^.dw) THEN
    exp.designator(l^.l,a);
    IF (l^.r=NIL) OR (l^.r^.md=pc.number) & (l^.r^.val<=0) THEN
      const.n:=0; cmd.load_store(a,const,4);
      IF a.am=cmd.am_aSTK THEN INC(cmd.stk) END;
      exp.add_offset(a,4);
      const.n:=-1; cmd.load_store(a,const,4);
    ELSE
      gen_call_resize(l,a,TRUE);
    END;
  ELSE error(l^.l,'must be dynarr or pointer');
  END;
*)
END gen_new;

PROCEDURE gen_resize(l: pc.NODE);
  VAR a: cmd.access;
BEGIN
(*
  IF (l^.l#NIL) & exp.dynarr?(l^.l^.dw) THEN
    exp.designator(l^.l,a); gen_call_resize(l,a,FALSE);
  ELSE error(l^.l,'gen resize: must be dynarr');
  END;
*)
END gen_resize;

PROCEDURE gen_dispose(l: pc.NODE);
  VAR a: cmd.access;
BEGIN
(*
  IF (l^.l#NIL) & exp.pointer?(l^.l^.dw) THEN
    exp.adr(l^.l,a); cmd.stot(a,4);
    exp.bytes(l^.l^.dw^.dw,a); cmd.stot(a,4);
    gen_call_rts(l^.dw);
  ELSIF (l^.l#NIL) & exp.dynarr?(l^.l^.dw) THEN
    exp.designator(l^.l,a); gen_call_resize(l,a,FALSE);
  ELSE error(l^.l,'must be dynarr or pointer');
  END;
*)
END gen_dispose;

PROCEDURE gen_statement(l: pc.NODE);
BEGIN
  pcS.txtpos:=l^.pos;
  cmd.mark(exp.pos(l^.pos));
  CASE l^.mode OF
    |pc.return         : gen_return(l);
    |pc.call           : gen_call(l);
    |pc.assign         : gen_assign(l);
    |pc.ifelse         : gen_if(l);
    |pc.loop           : gen_loop(l);
    |pc.for            : gen_for(l);
    |pc.while          : gen_while(l);
    |pc.repeat         : gen_repeat(l);
    |pc.exit           : gen_exit(l);
    |pc.inc            : gen_inc_dec(l);
    |pc.dec            : gen_inc_dec(l);
    |pc.case           : gen_case(l);
    |pc.incl           : gen_incl_excl(l);
    |pc.excl           : gen_incl_excl(l);
    |pc.halt           : gen_halt(l);
    |pc.assert         : gen_assert(l);
    |pc.new            : gen_new(l);
    |pc.resize         : gen_resize(l);
    |pc.dispose        : gen_dispose(l);
  ELSE error_ps(l^.pos,233);
  END;
  IF cmd.stk#0 THEN error_ps(l^.pos,233) END;
  IF cmd.fpp_stk#0 THEN error_ps(l^.pos,233) END;
END gen_statement;

PROCEDURE save_parms;
  VAR i: CARDINAL; e: exp.EXT;
BEGIN
  IF proc^.mode=pc.xproc THEN
    cmd.b(cmd.push_si); cmd.b(cmd.push_di);
    cmd.b(cmd.push_ds); cmd.b(cmd.seg_cs);
    cmd.b(cmd.mov_srm); cmd.b(cmd.md_abs+cmd.DS*8);
    cmd.fixup; cmd.block^.fixup^.md:=cmd.fm_dft; cmd.w(0);
  END;
  cmd.b(cmd.push_bp); cmd.b(cmd.mov_wrm); cmd.b(cmd.md_reg+cmd.SP+cmd.BP*8);
  e:=proc^.ext; i:=CARDINAL(-e^.n);
  IF i#0 THEN
    IF ODD(i) THEN INC(i) END;
    cmd.b(cmd.imm_wm); cmd.b(cmd.i_sub+cmd.md_reg+cmd.SP); cmd.w(i);
  END;
END save_parms;

PROCEDURE copy_parms;
  PROCEDURE move(ofs: INTEGER);
  BEGIN
    cmd.b(cmd.sub_wrm); cmd.b(cmd.md_reg+cmd.CX+cmd.SP*8);
    cmd.b(cmd.mov_wrm); cmd.b(cmd.md_w+cmd.rm_bp+cmd.SI*8); cmd.w(ofs);
    cmd.b(cmd.mov_wmr); cmd.b(cmd.md_w+cmd.rm_bp+cmd.SP*8); cmd.w(ofs);
    cmd.b(cmd.mov_wrm); cmd.b(cmd.md_reg+cmd.SP+cmd.DI*8);
    cmd.b(cmd.push_ds);
    cmd.b(cmd.mov_srm); cmd.b(cmd.md_w+cmd.rm_bp+cmd.DS*8); cmd.w(ofs+2);
    cmd.b(cmd.mov_msr); cmd.b(cmd.md_w+cmd.rm_bp+cmd.SS*8); cmd.w(ofs+2);
    cmd.b(cmd.mov_msr); cmd.b(cmd.md_reg+cmd.BX+cmd.SS*8);
    cmd.b(cmd.mov_srm); cmd.b(cmd.md_reg+cmd.BX+cmd.ES*8);
    cmd.b(cmd.cld); cmd.b(cmd.rep_z); cmd.b(cmd.movs_b);
    cmd.b(cmd.pop_ds);
  END move;
  VAR l: pc.OBJECT; t: pc.STRUCT; ofs,sz: CARDINAL;
BEGIN
  l:=proc^.type^.next;
  WHILE l#NIL DO
    t:=l^.type;
    IF (l^.mode=pc.var) & NOT (pc.RO IN l^.tags)  THEN
      IF t^.mode=pc.array_of THEN
        ofs:=CARDINAL(word(l^.adr)); sz:=CARDINAL(t^.base^.size);
        IF sz#1 THEN
          cmd.b(cmd.mov_axi); cmd.w(1);
          cmd.b(cmd.add_wrm); cmd.b(cmd.md_w+cmd.rm_bp+cmd.AX*8); cmd.w(ofs+4);
          cmd.b(cmd.mov_cxi); cmd.w(sz);
          cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_mul);
          cmd.b(cmd.mov_wrm); cmd.b(cmd.md_reg+cmd.AX+cmd.CX*8);
        ELSE
          cmd.b(cmd.mov_cxi); cmd.w(1);
          cmd.b(cmd.add_wrm); cmd.b(cmd.md_w+cmd.rm_bp+cmd.CX*8); cmd.w(ofs+4);
        END;
        move(ofs);
      ELSIF t^.mode IN pc.Forms{pc.array,pc.vector,pc.record} THEN
        ofs:=CARDINAL(word(l^.adr)); sz:=CARDINAL(t^.size);
        cmd.b(cmd.mov_cxi); cmd.w(sz);
        move(ofs);
      END;
    END;
    l:=l^.next;
  END;
END copy_parms;

(*--------------------------------------------------------------------------*)

CONST
  PUBDEF = 090H;     (* public name def         *)
  COMDEF = 0B0H;     (* communal names def      *)
  LOCSYM = 092H;     (* local symbols           *)
  EXTDEF = 08CH;     (* external names def      *)
  LINNUM = 094H;     (* line number record      *)
  LEDATA = 0A0H;     (* logical enumerated data *)
  LIDATA = 0A2H;     (* logical iterated data   *)
  THEADR = 080H;     (* T-module header         *)
  LHEADR = 082H;     (* L-module header         *)
  LNAMES = 096H;     (* list of names           *)
  SEGDEF = 098H;     (* segment def             *)
  GRPDEF = 09AH;     (* group def               *)
  FIXUPP = 09CH;     (* fixup record            *)
  MODEND = 08AH;     (* module entry            *)
  COMENT = 088H;     (* comment                 *)

VAR
  flow_len : CARDINAL;
  flow     : cmd.node;
  last_flow: POINTER TO cmd.node;
  chk_summ : CARDINAL;
  rec_len  : CARDINAL;
  rec_pos  : LONGCARD;
  out      : bio.File;
  ext_cnt  : CARDINAL;
  cod_def  : LONGCARD;
  glo_def  : LONGCARD;
  cod_ofs  : CARDINAL;

PROCEDURE copy_node(VAR f: cmd.node);
  VAR l: cmd.node;
BEGIN
  IF (f#NIL) & (f^.adr>0) & (f^.len<=8) & (f^.cnt<2) THEN
    INC(f^.cnt); l:=cmd.new_node(); l^:=f^; f:=l;
    f^.adr:=0; f^.next:=NIL;
  END;
END copy_node;

PROCEDURE move_jump(VAR f: cmd.node);
  VAR i: CARDINAL;
BEGIN
  i:=10;
  REPEAT
    IF (f=NIL) OR (f^.len>0) OR (f^.md#cmd.nm_goto) THEN RETURN END;
    f:=f^.goto; DEC(i);
  UNTIL i=0;
END move_jump;

PROCEDURE tie_node(f: cmd.node);
  VAR l: cmd.node; i: CARDINAL;
BEGIN
  IF (f=NIL) OR (f^.adr>0) THEN RETURN END;
  f^.adr:=flow_len; INC(flow_len,f^.len+f^.tlen);
  last_flow^:=f; last_flow:=ADR(f^.next); INC(f^.cnt);
  CASE f^.md OF
    |cmd.nm_goto:
      move_jump(f^.goto); copy_node(f^.goto); tie_node(f^.goto);
    |cmd.nm_case:
      move_jump(f^.goto); tie_node(f^.goto);
      FOR i:=0 TO f^.clen-1 DO
        move_jump(f^.ctbl^[i]); tie_node(f^.ctbl^[i]);
      END;
    |cmd.nm_cond:
      IF (f^.else=NIL) OR (f^.goto=NIL) THEN error(233) END;
      move_jump(f^.else); move_jump(f^.goto);
      IF f^.else^.adr<=0 THEN
        tie_node(f^.else); tie_node(f^.goto);
      ELSIF f^.goto^.adr<=0 THEN
        tie_node(f^.goto); tie_node(f^.else);
      ELSE
        copy_node(f^.else);
        IF f^.else^.adr<=0 THEN
          tie_node(f^.else); tie_node(f^.goto);
        ELSE
          copy_node(f^.goto); tie_node(f^.goto); tie_node(f^.else);
        END;
      END;
  END;
END tie_node;

PROCEDURE mark_flow(f: cmd.node);
  VAR fail_no: CARDINAL; i,k: cmd.node; c,s: CARDINAL; j: INTEGER;
BEGIN
  c:=cmd.cnt; (* s:=vrs.scnt; *)
  REPEAT
    fail_no:=0; i:=f; cmd.cnt:=c; (* vrs.scnt:=s; *)
    WHILE i#NIL DO
      IF (i^.next#NIL) & (i^.next^.adr#i^.adr+i^.len+i^.tlen) THEN error(233) END;
      j:=cmd.gen_jump(i,cod_ofs)-INTEGER(i^.tlen);
      IF j#0 THEN
        k:=i^.next;
        WHILE k#NIL DO INC(k^.adr,j); k:=k^.next END;
        INC(fail_no); INC(flow_len,j); INC(i^.tlen,j);
      END;
      i:=i^.next;
    END;
  UNTIL fail_no=0;
END mark_flow;

PROCEDURE wr_byte(n: WORD);
BEGIN
  chk_summ:=(chk_summ+CARDINAL(n) MOD 256) MOD 256;
  INC(rec_len); bio.WrBin(out,n,1);
END wr_byte;

PROCEDURE wr_word(n: WORD);
BEGIN
  wr_byte(n); wr_byte(CARDINAL(n)>>8);
END wr_word;

PROCEDURE wr_byte_seq(VAR s: ARRAY OF BYTE; pos,ln: CARDINAL);
  VAR i: CARDINAL; a: ADDRESS;
BEGIN
  FOR i:=pos TO pos+ln-1 DO INC(chk_summ,ORD(s[i])); INC(rec_len) END;
  chk_summ:=chk_summ MOD 256;
  a:=ADR(s[pos]);
  bio.WrBin(out,a^,ln);
END wr_byte_seq;

PROCEDURE rec_start(n: CARDINAL);
BEGIN
  chk_summ:=0; rec_len:=0; wr_byte(n);
  rec_pos:=bio.GetPos(out);
  wr_word(0); rec_len:=0;
END rec_start;

PROCEDURE rec_end;
  VAR j: CARDINAL; i: LONGCARD;
BEGIN
  i:=bio.GetPos(out); bio.Seek(out,rec_pos);
  wr_word(rec_len+1);
  bio.Seek(out,i); wr_byte(word(-INTEGER(chk_summ)));
END rec_end;

PROCEDURE wr_string(s: ARRAY OF CHAR);
  VAR i,j: CARDINAL;
BEGIN
  i:=0; j:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0C) DO INC(i) END;
  wr_byte(i);
  WHILE j<i DO wr_byte(ORD(s[j])); INC(j) END;
END wr_string;

PROCEDURE wr_string2(s1,s2: ARRAY OF CHAR; sep: CHAR);
  VAR i1,i2,j: CARDINAL;
BEGIN
  i1:=0; i2:=0; j:=0;
  WHILE (i1<=HIGH(s1)) & (s1[i1]#0C) DO INC(i1) END;
  WHILE (i2<=HIGH(s2)) & (s2[i2]#0C) DO INC(i2) END;
  wr_byte(i1+i2+1);
  WHILE j<i1 DO wr_byte(ORD(s1[j])); INC(j) END;
  wr_byte(ORD(sep)); j:=0;
  WHILE j<i2 DO wr_byte(ORD(s2[j])); INC(j) END;
END wr_string2;

PROCEDURE wr_rts_name(n: CARDINAL);
BEGIN
  CASE n OF
    |01: wr_string('$SgnDivByt');
    |02: wr_string('$SgnDivWrd');
    |03: wr_string('$SgnDiv');
    |04: wr_string('$SgnMul');
    |05: wr_string('$UnsMul');
    |06: wr_string('$SgnQuo');
    |07: wr_string('$UnsDiv');
    |08: wr_string('$SgnRem');
    |09: wr_string('$SgnModByt');
    |10: wr_string('$SgnModWrd');
    |11: wr_string('$SgnMod');
    |12: wr_string('$UnsRem');
    |13: IF cmd.cpu_mode=0 THEN wr_string('$CmpBytR');
         ELSE wr_string('$CmpBytP');
         END;
    |14: IF cmd.cpu_mode=0 THEN wr_string('$MovBytR');
         ELSE wr_string('$MovBytP');
         END;
    |15: IF cmd.cpu_mode=0 THEN wr_string('$IniTskR');
         ELSE wr_string('$IniTskP');
         END;
  END;
END wr_rts_name;

PROCEDURE wr_proc_name(p: pc.OBJECT);
  VAR nm: ARRAY [0..255] OF CHAR;
BEGIN
  IF pc.external IN p^.tags THEN
    wr_string(p^.name);
  ELSIF p^.mode=pc.xproc THEN
    wr_string2(pcO.exts[-p^.scope]^.name,p^.name,'$');
  ELSE
    nm:=''; pcM.app_num(nm,p^.adr); wr_string2(pcO.cu_name,nm,'$');
  END;
END wr_proc_name;

PROCEDURE wr_index(n: CARDINAL);
BEGIN
  IF n<128 THEN wr_byte(n); RETURN END;
  wr_byte(n DIV 256 + 128); wr_byte(n);
END wr_index;

PROCEDURE enter_obj_file(main: BOOLEAN);
  TYPE ar10=ARRAY [0..4] OF INTEGER;
  CONST zz=ar10(0,0,0,0,0);
  VAR nm: ARRAY [0..255] OF CHAR; n,i: CARDINAL;
BEGIN
  ext_cnt:=0;
  nm:=''; pcM.app(nm,pcO.cu_name); pcM.app(nm,'.obj');
  out:=bio.Create(nm);
  (*------------------------------------------------------------------*)
  (*              module header                                       *)
  nm:=''; pcM.app(nm,pcO.cu_name); pcM.app(nm,'.mod');
  rec_start(THEADR); wr_string(nm); rec_end;
  rec_start(COMENT); wr_byte(0); wr_byte(0A1H);
  rec_end;
  rec_start(COMENT); wr_byte(0); wr_byte(0EEH);
  wr_byte(1); wr_byte(0); wr_byte(0); wr_byte(3); wr_byte(0);
  rec_end;
  (*------------------------------------------------------------------*)
  (*              define code & data segments                         *)
  rec_start(LNAMES);
  wr_string('CODE'); wr_string('DATA');
  IF main THEN wr_string('STACK'); wr_string('DGROUP') END;
  rec_end;
  glo_def:=bio.GetPos(out);
  bio.WrBin(out,zz,10);
  cod_def:=bio.GetPos(out);
  bio.WrBin(out,zz,10);
  IF main THEN
    rec_start(SEGDEF);
    wr_byte(074H); wr_word(8000H); wr_index(3); wr_index(3); wr_index(0);
    rec_end;
    rec_start(GRPDEF);
    wr_index(4); wr_byte(0FFH); wr_index(3);
    rec_end;
  END;
  rec_start(PUBDEF);
  wr_index(0); wr_index(1);
  wr_string2(pcO.cu_name,'','@');
  wr_byte(0); wr_byte(0); wr_index(0);
  rec_end;
  rec_start(PUBDEF);
  wr_index(0); wr_index(2);
  wr_string2(pcO.cu_name,'$DFT','$');
  wr_byte(0); wr_byte(0); wr_index(0);
  rec_end;
  rec_start(EXTDEF);
  FOR n:=0 TO pcO.ext_no-1 DO
    wr_string2(pcO.exts[n]^.name,'','@'); wr_index(0);
  END;
  rec_end;
  rec_start(LEDATA);
  wr_index(2); wr_word(0); cod_ofs:=pcO.ext_no*2;
  FOR n:=0 TO pcO.ext_no-1 DO wr_word(0) END;
  rec_end;
  rec_start(FIXUPP);
  FOR n:=0 TO pcO.ext_no-1 DO
    i:=n*2+0C800H; wr_byte(i>>8); wr_byte(i);
    wr_byte(56H); INC(ext_cnt); wr_index(ext_cnt);
  END;
  rec_end;
END enter_obj_file;

PROCEDURE exit_obj_file(main: BOOLEAN);
  VAR
    len: CARDINAL; e: exp.EXT;
    ptr: POINTER TO ARRAY [0..0FFFEH] OF BYTE;
    p: LONGCARD;
BEGIN
  e:=module^.ext; len:=CARDINAL(e^.n);
  p:=bio.GetPos(out); bio.Seek(out,glo_def);
  rec_start(SEGDEF);
  wr_byte(60H); wr_word(len); wr_index(2); wr_index(2); wr_index(0);
  rec_end;
  bio.Seek(out,cod_def);
  rec_start(SEGDEF);
  wr_byte(60H); wr_word(cod_ofs); wr_index(1); wr_index(1); wr_index(0);
  rec_end;
  bio.Seek(out,p);
  WHILE globals#NIL DO
    rec_start(LEDATA);
    wr_index(1); wr_word(globals^.ofs);
    ptr:=globals^.adr;
    wr_byte_seq(ptr^,0,globals^.size);
    rec_end; globals:=globals^.next;
  END;
  WHILE pconsts#NIL DO
    rec_start(EXTDEF);
    INC(ext_cnt); wr_proc_name(pconsts^.obj); wr_index(0);
    rec_end;
    rec_start(LEDATA);
    wr_index(1); wr_word(pconsts^.ofs); wr_word(0); wr_word(0);
    rec_end;
    rec_start(FIXUPP);
    wr_byte(0CCH); wr_byte(0); wr_byte(56H); wr_index(ext_cnt);
    rec_end;
    pconsts:=pconsts^.next;
  END;
  IF main THEN
    rec_start(EXTDEF);
    INC(ext_cnt); wr_string2(pcO.cu_name,'','$'); wr_index(0);
    rec_end;
  END;
  rec_start(MODEND);
  IF main THEN
    wr_byte(0C1H); wr_byte(56H); wr_index(ext_cnt);
  ELSE
    wr_byte(0);
  END;
  rec_end;
  bio.Close(out);
END exit_obj_file;

PROCEDURE write_proc_obj;
  VAR
    i,fe: CARDINAL; x: cmd.FIXUP; l: cmd.LINE; f: cmd.node;
BEGIN
  rec_start(PUBDEF); wr_index(0); wr_index(2);
  IF cmd.level=0 THEN wr_string2(pcO.cu_name,'','$');
  ELSE wr_proc_name(proc);
  END;
  wr_word(cod_ofs); wr_index(0); rec_end;
  f:=flow;
  WHILE f#NIL DO
    x:=f^.fixup;
    IF x#NIL THEN
      rec_start(EXTDEF); fe:=ext_cnt;
      REPEAT
        CASE x^.md OF
          |cmd.fm_far_call: wr_proc_name(x^.obj);
          |cmd.fm_mod_call: wr_string2(x^.obj^.name,'','$');
          |cmd.fm_rts     : wr_rts_name(x^.rts);
          |cmd.fm_call    : wr_proc_name(x^.obj);
          |cmd.fm_dft     : wr_string2(pcO.cu_name,'$DFT','$');
        END;
        x:=x^.next; INC(ext_cnt); wr_index(0);
      UNTIL x=NIL;
      rec_end;
    END;
    IF f^.len+f^.tlen>1024 THEN error(233) END;
    rec_start(LEDATA);
    wr_index(2); wr_word(cod_ofs);
    wr_byte_seq(cmd.code,f^.pos,f^.len);
    wr_byte_seq(cmd.code,f^.tpos,f^.tlen);
    rec_end;
    x:=f^.fixup;
    IF x#NIL THEN
      rec_start(FIXUPP);
      REPEAT
        CASE x^.md OF
          |cmd.fm_dft : i:=x^.ofs+0C400H;
          |cmd.fm_call: i:=x^.ofs+08400H;
          |cmd.fm_rts,cmd.fm_far_call,cmd.fm_mod_call: i:=x^.ofs+0CC00H;
        END;
        wr_byte(i>>8); wr_byte(i);
        wr_byte(56H); INC(fe); wr_index(fe); x:=x^.next;
      UNTIL x=NIL;
      rec_end;
    END;
    l:=f^.line;
    IF l#NIL THEN
      rec_start(LINNUM);
      wr_index(0); wr_index(2);
      REPEAT wr_word(l^.no); wr_word(l^.ofs+cod_ofs); l:=l^.next UNTIL l=NIL;
      rec_end;
    END;
    INC(cod_ofs,f^.len+f^.tlen); f:=f^.next;
  END;
END write_proc_obj;

PROCEDURE gen_procs(n: pc.NODE);
  VAR first,second: cmd.node;
BEGIN
  WHILE n#NIL DO
    IF n^.mode#pc.proc THEN error_ps(n^.pos,233) END;
    IF NOT (pc.external IN n^.obj^.tags) THEN
      INC(cmd.level); gen_procs(n^.l); DEC(cmd.level);
      pcS.txtpos:=n^.pos; proc:=n^.obj; cmd.block:=NIL;
      second:=cmd.new_node(); cmd.start(second);
      stat_seq(n^.r);
      IF proc^.type^.base^.mode#pc.undef THEN
        exit_trap(int_return);
      ELSE
        gen_return_cmd;
      END;
      first:=cmd.new_node(); cmd.start(first);
      cmd.mark(exp.pos(n^.pos));
      save_parms; copy_parms;
      cmd.block^.goto:=second; cmd.finish;
      flow:=NIL; last_flow:=ADR(flow); flow_len:=0;
      tie_node(first); mark_flow(flow); write_proc_obj; cmd.remove_nodes;
    END;
    n:=n^.next;
  END;
END gen_procs;

PROCEDURE gen_module(n: pc.NODE; main: BOOLEAN);
  VAR f: cmd.node; ptr: POINTER TO BYTE; i: CARDINAL;
BEGIN
  pcS.txtpos:=n^.pos; proc:=NIL; cmd.block:=NIL;
  f:=cmd.new_node(); cmd.start(f);
  cmd.mark(exp.pos(n^.pos));
  IF main THEN cmd.call_rts(15) END;
  cmd.b(cmd.push_ds); cmd.b(cmd.seg_cs);
  cmd.b(cmd.mov_srm); cmd.b(cmd.md_abs+cmd.DS*8);
  cmd.fixup; cmd.block^.fixup^.md:=cmd.fm_dft; cmd.w(0);
  IF NOT main THEN
    cmd.new_glo_const(1,ptr,i); ptr^:=0;
    cmd.b(cmd.grp1_bm); cmd.b(cmd.md_abs+cmd.g1_test); cmd.w(i); cmd.b(0FFH);
    cmd.b(cmd.je); cmd.b(2);
    cmd.b(cmd.pop_ds); cmd.b(cmd.ret_i);
    cmd.b(cmd.mov_bmi); cmd.b(cmd.md_abs); cmd.w(i); cmd.b(0FFH);
  END;
  FOR i:=1 TO pcO.ext_no-1 DO
    cmd.b(cmd.call_id); cmd.fixup;
    cmd.block^.fixup^.md:=cmd.fm_mod_call;
    cmd.block^.fixup^.obj:=pcO.exts[i];
    cmd.w(0); cmd.w(0);
  END;
  stat_seq(n^.r);
  cmd.b(cmd.pop_ds);
  IF main THEN exit_trap(0) ELSE cmd.b(cmd.ret_i) END;
  cmd.finish;
  flow:=NIL; last_flow:=ADR(flow); flow_len:=0;
  tie_node(f); mark_flow(flow); write_proc_obj; cmd.remove_nodes;
END gen_module;

PROCEDURE compile(n: pc.NODE; main: BOOLEAN; pno: CARDINAL);
BEGIN
  IF n^.mode#pc.module THEN error_ps(n^.pos,233) END;
  module:=n^.obj; proc_no:=pno;
  enter_obj_file(main);
  cmd.level:=0; gen_module(n,main);
  cmd.level:=1; gen_procs(n^.l);
  exit_obj_file(main);
END compile;

BEGIN
  const.am:=cmd.am_imm;
  exp.gen_call:=gen_call;
  proc:=NIL; cmd.level:=0;
  NEW(usage); globals:=NIL; pconsts:=NIL;
  usage^:=pc.null_node;
  cmd.alloc_tmp_var:=alloc_tmp_var;
  cmd.new_glo_const:=new_glo_const;
  cmd.new_proc_const:=new_proc_const;
END inSta.
