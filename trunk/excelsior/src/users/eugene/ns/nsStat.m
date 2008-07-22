IMPLEMENTATION MODULE nsStat; (* Sem 01-Dec-90. (c) KRONOS *)

(*$N+*)

IMPORT mcd : defCodes;
IMPORT mem : pcSystem;
IMPORT tbl : pcTab;
IMPORT des : nsDesig;
IMPORT exp : nsExpr;
IMPORT put : nsCmd;

FROM SYSTEM      IMPORT WORD, ADDRESS, ADR;
FROM nsSym       IMPORT access, adr_mode, index_mode;
FROM pcTab       IMPORT ref;
FROM nsDesig     IMPORT proc, free_regs;
FROM nsExpr      IMPORT block, val_mode, node, node_mode,
                        fpp_regs, fpp_regs_no;

WITH STORAGE : mem;

TYPE
  val_set=SET OF val_mode;

CONST
  assert  = 'assert';
  ill_type= 'illegal type';

  glo_var_limit = 1000000h;

  svcn_case   = 45h;
  svcn_return = 46h;
  svcn_halt   = 47h;
  svcn_assert = 48h;
  svcn_size   = 4Ah;

VAR
  glo_alloc_cnt : access;
  const         : access;
  reg           : access;
  freg          : access;
  aRG           : access;
  TOS           : access;
  aSP           : access;
  aFP           : access;
  aaFP          : access;
  tmp           : access;
  stat_pos      : INTEGER;

PROCEDURE move(a,b: ADDRESS; n: INTEGER); CODE mcd.move END move;
PROCEDURE bblt(a1: ADDRESS; s1: INTEGER;
               a2: ADDRESS; s2: INTEGER; sz: INTEGER); CODE mcd.bblt END bblt;

PROCEDURE error(l: ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l=NIL) OR (l^.md=tbl.procedure) OR (l^.md=tbl.number) THEN
    tbl.error(stat_pos DIV 1000h,stat_pos MOD 1000h,s,x);
  ELSE
    tbl.error(l^.val DIV 1000h,l^.val MOD 1000h,s,x);
  END;
  HALT(1);
END error;

PROCEDURE imm?(VAL a: access);
BEGIN
  IF a.am#am_imm THEN error(NIL,'must be constant') END;
END imm?;

PROCEDURE alloc_var(sz: INTEGER; ind: BOOLEAN): INTEGER;
  VAR n: INTEGER;
BEGIN
  ASSERT(sz>0);
  ASSERT(sz MOD 4 =0);
  n:=des.new_var();
  IF des.level=0 THEN
    IF ind THEN
      des.vars[n].am   :=am_aaSB;
      des.vars[n].n    :=des.new_str(sz DIV 4)*4;
    ELSE
      des.vars[n].am   :=am_aSB;
      des.vars[n].disp :=des.new_str(sz DIV 4)*4;
    END;
  ELSE
    IF ind THEN
      des.vars[n].level:=des.level;
      des.vars[n].am   :=am_aaFP;
      WITH des.prcs[proc^.l^.adr] DO
        INC(loc_sz,sz); des.vars[n].n:=-loc_sz;
      END;
    ELSE
      des.vars[n].level:=des.level;
      des.vars[n].am   :=am_aFP;
      WITH des.prcs[proc^.l^.adr] DO
        INC(loc_sz,sz); des.vars[n].disp:=-loc_sz;
      END;
    END;
  END;
  RETURN n;
END alloc_var;

PROCEDURE one(l: ref); FORWARD;

PROCEDURE profile_psz(l: ref): INTEGER;
  VAR v,t: ref; n: INTEGER;
BEGIN
  v:=l^.dw; n:=0;
  WHILE v#NIL DO
    IF v^.md=tbl.var THEN
      t:=v^.l;
      LOOP
        IF t=NIL THEN EXIT
        ELSIF t^.md=tbl.var_prm THEN t:=t^.dw;
        ELSIF t^.md=tbl.val_prm THEN t:=t^.dw
        ELSE EXIT
        END;
      END;
      IF des.array_of?(t) THEN INC(n,12)
      ELSE INC(n,4);
      END;
    END;
    v:=v^.nxt;
  END;
  RETURN n;
END profile_psz;

PROCEDURE trap(n: INTEGER);
BEGIN
  const.n:=n; put.cmd(put.mov,const,TOS,4); put.svc;
END trap;

PROCEDURE cmp_access(VAL a,b: access): BOOLEAN;
BEGIN
  RETURN
    (a.am=b.am) & (a.xm=a.xm) &
    (a.xm=xm_off) & (a.disp=b.disp) &
    ((a.am=am_aSB) OR (a.am=am_aFP));
END cmp_access;

PROCEDURE gen_plus_ass(l: ref; VAL dest: access; sz: INTEGER);
  VAR la,ra,a: access;
BEGIN
  CASE exp.vm(l^.l^.dw) OF
    |vm_integer:
      exp.expression(l^.l,la); exp.expression(l^.r,ra);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        const.n:=la.n+ra.n;
        put.cmd(put.mov,const,dest,sz);
      ELSIF cmp_access(dest,la) THEN
        put.cmd(put.add,ra,dest,sz); put.flag;
      ELSIF cmp_access(dest,ra) THEN
        put.cmd(put.add,la,dest,sz); put.flag;
      ELSIF la.am=am_RG THEN
        put.cmd(put.add,ra,la,sz); put.flag; put.cmd(put.mov,la,dest,sz);
      ELSIF ra.am=am_RG THEN
        put.cmd(put.add,la,ra,sz); put.flag; put.cmd(put.mov,ra,dest,sz);
      ELSE
        des.alloc_reg(reg); put.cmd(put.mov,la,reg,sz);
        put.cmd(put.add,ra,reg,sz); put.flag; put.cmd(put.mov,reg,dest,sz);
      END;
    |vm_cardinal:
      exp.expression(l^.l,la); exp.expression(l^.r,ra);
      IF des.imm_int?(la,FALSE,sz) & des.imm_int?(ra,FALSE,sz) THEN
        const.n:=la.n+ra.n; put.cmd(put.mov,const,dest,sz);
      ELSIF cmp_access(dest,la) THEN
        put.cmd(put.add,ra,dest,sz);
      ELSIF cmp_access(dest,ra) THEN
        put.cmd(put.add,la,dest,sz);
      ELSIF la.am=am_RG THEN
        put.cmd(put.add,ra,la,sz); put.cmd(put.mov,la,dest,sz);
      ELSIF ra.am=am_RG THEN
        put.cmd(put.add,la,ra,sz); put.cmd(put.mov,ra,dest,sz);
      ELSE
        des.alloc_reg(reg); put.cmd(put.mov,la,reg,sz);
        put.cmd(put.add,ra,reg,sz); put.cmd(put.mov,reg,dest,sz);
      END;
    |vm_real:
      exp.expression(l^.l,la); exp.expression(l^.r,ra);
      IF (sz=4) & des.imm_int?(la,FALSE,4) & des.imm_int?(ra,FALSE,4) THEN
        const.n:=INTEGER(REAL(la.n)+REAL(ra.n));
        put.cmd(put.movf,const,dest,sz);
      ELSIF cmp_access(dest,la) THEN
        put.cmd(put.addf,ra,dest,sz);
      ELSIF cmp_access(dest,ra) THEN
        put.cmd(put.addf,la,dest,sz);
      ELSIF (la.am=am_FPP4) OR (la.am=am_FPP8) THEN
        put.cmd(put.addf,ra,la,sz);
        put.cmd(put.movf,la,dest,sz);
      ELSIF (ra.am=am_FPP4) OR (ra.am=am_FPP8) THEN
        put.cmd(put.addf,la,ra,sz);
        put.cmd(put.movf,ra,dest,sz);
      ELSE
        exp.alloc_fpp_reg(a,sz);
        put.cmd(put.movf,la,a,sz);
        put.cmd(put.addf,ra,a,sz);
        put.cmd(put.movf,a,dest,sz);
      END;
  END;
END gen_plus_ass;

PROCEDURE expression_assign(l: ref; VAL des: access; m: val_mode; sz: INTEGER);
  VAR a: access;
BEGIN
  IF l=NIL THEN error(NIL,assert) END;
  CASE l^.md OF
    |tbl.plus: gen_plus_ass (l,des,sz);
  ELSE
    exp.expression(l,a);
    IF m=vm_real THEN
      put.cmd(put.movf,a,des,sz);
    ELSE
      put.cmd(put.mov,a,des,sz);
    END;
  END;
END expression_assign;

PROCEDURE gen_var(l: ref);
  VAR b: access; i: INTEGER;
BEGIN
  des.bytes_d(l^.l,b);
  IF (b.am=am_imm) & ((b.n<=glo_var_limit) OR (des.level#0)) THEN
    l^.adr:=alloc_var(b.n,FALSE);
  ELSIF (des.level=0) & (b.am=am_imm) THEN
    l^.adr:=alloc_var(4,TRUE); i:=des.new_mg(1);
    des.mg[i].offset:=des.vars[l^.adr].n; des.mg[i].size:=b.n;
  ELSIF des.level=0 THEN
    l^.adr:=alloc_var(4,TRUE);
    IF glo_alloc_cnt.am=am_imm THEN
      i:=alloc_var(4,FALSE);
      glo_alloc_cnt:=des.vars[i];
      put.cmd(put.mov,b,glo_alloc_cnt,4);
    ELSE
      put.cmd(put.add,b,glo_alloc_cnt,4);
    END;
    ASSERT(b.am=am_RG);
    put.cmd(put.neg,b,b,4);
    put.adjsp(b,4);
    b:=des.vars[l^.adr];
    b.disp:=b.n; b.am:=am_aSB;
    put.cmd(put.addr,TOS,b,4);
  ELSE
    l^.adr:=alloc_var(4,TRUE);
    put.cmd(put.neg,b,b,4);
    put.adjsp(b,4);
    b:=des.vars[l^.adr];
    b.disp:=b.n; b.am:=am_aFP;
    put.cmd(put.addr,TOS,b,4);
  END;
  IF tbl.ini_nil THEN
    IF l^.l^.md=tbl.pointer THEN
      const.n:=0;
      put.cmd(put.mov,const,des.vars[l^.adr],4);
    ELSIF (l^.l^.md=tbl.packed_dynarr) OR (l^.l^.md=tbl.dynarr)  THEN
      const.n:=0;
      b:=des.vars[l^.adr];
      put.cmd(put.mov,const,b,4);
      des.add_offset(b,4);
      put.cmd(put.mov,const,b,4);
      const.n:=-1;
      des.add_offset(b,4);
      put.cmd(put.mov,const,b,4);
    END;
  END;
END gen_var;

PROCEDURE put_move(fr,to,sz: access);
  PROCEDURE in_reg(VAL a: access; rg: INTEGER): BOOLEAN;
  BEGIN
    IF (a.am=am_RG) & (a.rg=rg) THEN RETURN TRUE END;
    IF (a.am=am_aRG) & (a.rg=rg) THEN RETURN TRUE END;
    IF (a.xm#xm_off) & (a.rg_x=rg) THEN RETURN TRUE END;
    RETURN FALSE;
  END in_reg;
  VAR rset: BITSET; i: INTEGER;
BEGIN
  rset:={0..des.regs_no-1};
  FOR i:=free_regs.stk TO free_regs.lim-1 DO
    rset:=rset/{i MOD des.regs_no}
  END;
  -- rset - set of durty registers
  put.save(rset*{0..2});
  ASSERT(fr.am#am_TOS); ASSERT(to.am#am_TOS);
  IF in_reg(to,0) THEN put.cmd(put.addr,to,TOS,4); to:=TOS END;
  IF in_reg(to,1) THEN put.cmd(put.addr,to,TOS,4); to:=TOS END;
  IF in_reg(fr,0) THEN put.cmd(put.addr,fr,TOS,4); fr:=TOS END;
  reg.rg:=0; put.cmd(put.mov,sz,reg,4);
  reg.rg:=1;
  IF fr.am=am_TOS THEN put.cmd(put.mov,TOS,reg,4);
  ELSE put.cmd(put.addr,fr,reg,4);
  END;
  reg.rg:=2;
  IF to.am=am_TOS THEN put.cmd(put.mov,TOS,reg,4);
  ELSE put.cmd(put.addr,to,reg,4);
  END;
  put.movs(1,FALSE,FALSE,FALSE);
  put.restore(rset*{0..2});
END put_move;

PROCEDURE put_move_csz(VAL fr,to: access; sz: INTEGER);
BEGIN
  IF sz<=0 THEN RETURN END;
  IF sz IN {1,2,4} THEN
    put.cmd(put.mov,fr,to,sz);
  ELSIF sz>16 THEN
    const.n:=sz; put_move(fr,to,const);
  ELSIF sz MOD 4 =0 THEN
    put.movm(fr,to,4,sz DIV 4);
  ELSIF sz MOD 2 =0 THEN
    put.movm(fr,to,2,sz DIV 2);
  ELSE
    put.movm(fr,to,1,sz);
  END;
END put_move_csz;

PROCEDURE put_check_move(VAL fr,to,fsz,tsz: access);
  VAR fail,ok,next: node;
BEGIN
  fail:=exp.new(); ok:=exp.new(); next:=exp.new();
  put.cmd(put.cmp,tsz,fsz,4);
  block^.md:=nm_cond; block^.flag:=put.LT;
  block^.true:=fail; block^.false:=ok;
  exp.start(fail); trap(svcn_size); block^.true:=next;
  exp.start(ok); put_move(fr,to,fsz); block^.true:=next;
  exp.start(next);
END put_check_move;

PROCEDURE gen_const(l: ref);
  VAR n,i: INTEGER; e,lsz,rsz: access;
BEGIN
  des.bytes_d(l^.l,lsz); des.bytes_u(l^.dw^.dw,rsz);
  IF (lsz.am=am_imm) & (rsz.am=am_imm) THEN
    n:=lsz.n;
    IF rsz.n<n THEN n:=rsz.n END;
    IF n<=0 THEN
      l^.adr:=des.new_var(); const.n:=0; des.vars[l^.adr]:=const;
    ELSE
      exp.expression(l^.dw,e);
      IF (e.am=am_imm) OR (e.am=am_aSBimm) THEN
        l^.adr:=des.new_var(); des.vars[l^.adr]:=e;
      ELSIF (n<=glo_var_limit) OR (des.level#0) THEN
        l^.adr:=alloc_var(n,FALSE); put_move_csz(e,des.vars[l^.adr],n);
      ELSE
        l^.adr:=alloc_var(4,TRUE); i:=des.new_mg(1);
        des.mg[i].offset:=des.vars[l^.adr].n; des.mg[i].size:=n;
        put_move_csz(e,des.vars[l^.adr],n);
      END;
    END;
  ELSIF des.level=0 THEN
    l^.adr:=alloc_var(4,TRUE);
    IF glo_alloc_cnt.am=am_imm THEN
      i:=alloc_var(4,FALSE);
      glo_alloc_cnt:=des.vars[i];
      put.cmd(put.mov,lsz,glo_alloc_cnt,4);
    ELSE
      put.cmd(put.add,lsz,glo_alloc_cnt,4);
    END;
    ASSERT(lsz.am=am_RG);
    put.cmd(put.neg,lsz,lsz,4); put.adjsp(lsz,4);
    e:=des.vars[l^.adr]; e.disp:=e.n; e.am:=am_aSB;
    put.cmd(put.addr,TOS,e,4);
    exp.expression(l^.dw,e); put_check_move(e,des.vars[l^.adr],rsz,lsz);
  ELSE
    l^.adr:=alloc_var(4,TRUE);
    ASSERT(lsz.am=am_RG);
    put.cmd(put.neg,lsz,lsz,4); put.adjsp(lsz,4);
    e:=des.vars[l^.adr]; e.disp:=e.n; e.am:=am_aFP;
    put.cmd(put.addr,TOS,e,4);
    exp.expression(l^.dw,e); put_check_move(e,des.vars[l^.adr],rsz,lsz);
  END;
END gen_const;

PROCEDURE alloc_range(adr: INTEGER; VAL fr,to: access;
                      sg: BOOLEAN; sz: INTEGER);
BEGIN
  WITH des.rngs[adr] DO
    IF (des.level=0) OR (fr.am=am_imm) & (to.am=am_imm) THEN
      r.disp:=des.new_str((2*sz+3) DIV 4)*4;
      l.disp:=r.disp+sz;
      IF fr.am=am_imm THEN
        l.am:=am_aSBimm;
        bblt(ADR(des.scode),l.disp*8,ADR(fr.n),0,sz*8);
      ELSE
        l.am:=am_aSB; put.cmd(put.mov,fr,l,sz);
      END;
      IF to.am=am_imm THEN
        r.am:=am_aSBimm;
        bblt(ADR(des.scode),r.disp*8,ADR(to.n),0,sz*8);
      ELSE
        r.am:=am_aSB; put.cmd(put.mov,to,r,sz);
      END;
    ELSE
      l.level:=des.level; r.level:=des.level;
      WITH des.prcs[proc^.l^.adr] DO
        INC(loc_sz,(2*sz+3) DIV 4 * 4); r.disp:=-loc_sz;
      END;
      l.disp:=r.disp+sz;
      IF fr.am=am_imm THEN
        l.am:=am_aFPimm; l.n:=fr.n; put.cmd(put.mov,fr,l,sz);
      ELSE
        l.am:=am_aFP; put.cmd(put.mov,fr,l,sz);
      END;
      IF to.am=am_imm THEN
        r.am:=am_aFPimm; r.n:=to.n; put.cmd(put.mov,to,r,sz);
      ELSE
        r.am:=am_aFP; put.cmd(put.mov,to,r,sz);
      END;
    END;
    sign:=sg; size:=sz;
  END;
END alloc_range;

PROCEDURE gen_array(l: ref);
  VAR c,sg: BOOLEAN; fr,to: access; sz: INTEGER;
BEGIN
  IF (l^.l^.md=tbl.range) & des.rngs[l^.l^.adr].sign THEN
    l^.adr:=l^.l^.adr;
  ELSIF (l^.l^.md=tbl.subtype) & des.rngs[l^.l^.adr].sign THEN
    l^.adr:=l^.l^.adr;
  ELSIF (l^.l^.md=tbl.enumeration) & des.rngs[l^.l^.adr].sign THEN
    l^.adr:=l^.l^.adr;
  ELSE
    l^.adr:=des.new_rng();
    des.min(l^.l,fr); des.max(l^.l,to);
    sg:=des.sign?(l^.l); sz:=exp.bytes(l^.l);
    des.sub_base(fr,sg,sz);
    des.sub_base(to,sg,sz);
    c:=des.imm_int?(fr,sg,sz);
    c:=des.imm_int?(to,sg,sz) & c;
    alloc_range(l^.adr,fr,to,TRUE,sz);
  END;
END gen_array;

PROCEDURE gen_record(l: ref);
  PROCEDURE field(f: ref; VAR pos: INTEGER);
    VAR sz: access;
  BEGIN
    ASSERT(f^.md=tbl.var);
    f^.adr:=des.new_var();
    des.vars[f^.adr].am:=am_abs; des.vars[f^.adr].disp:=pos;
    des.bytes(f^.l,sz);
    IF sz.am#am_imm THEN error(f,'record field must have constant size') END;
    INC(pos,sz.n);
  END field;
  PROCEDURE record_case(c: ref; VAR pos: INTEGER); FORWARD;
  PROCEDURE field_list(l: ref; VAR pos: INTEGER);
  BEGIN
    WHILE l#NIL DO
      IF l^.md=tbl.var THEN field(l,pos);
      ELSIF l^.md=tbl.case THEN record_case(l,pos);
      ELSE one(l);
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
      IF s^.md=tbl.select THEN field_list(s^.r,pos) ELSE one(s) END;
      IF pos>max THEN max:=pos END; pos:=min; s:=s^.nxt;
    END;
    field_list(c^.r,pos);
    IF pos<max THEN pos:=max END;
  END record_case;
  VAR adr: INTEGER;
BEGIN
  adr:=0; field_list(l^.dw,adr);
END gen_record;

PROCEDURE gen_profile(l: ref);
-- вичисляет методы доступа для параметров
  VAR v,i,t: ref; adr: INTEGER; var: BOOLEAN; b: access; trap: des.trap;
BEGIN
  v:=NIL; i:=l^.dw;
  WHILE i#NIL DO
    IF i^.md=tbl.var THEN i^.r:=v; v:=i END;
    i:=i^.nxt;
  END;
  adr:=12;
  WHILE v#NIL DO
    v^.adr:=des.new_var();
    t:=v^.l; var:=FALSE;
    LOOP
      IF t=NIL THEN EXIT
      ELSIF t^.md=tbl.var_prm THEN t:=t^.dw; var:=TRUE;
      ELSIF t^.md=tbl.val_prm THEN t:=t^.dw
      ELSE EXIT
      END;
    END;
    IF des.array_of?(t) THEN
      des.vars[v^.adr].am:=am_aFP;
      des.vars[v^.adr].disp:=adr;
      INC(adr,12);
    ELSE
      IF var THEN
        des.vars[v^.adr].am:=am_aaFP;
      ELSE
        des.save(trap); des.bytes_d(v^.l,b); des.restore(trap);
        IF (b.am=am_imm) & (b.n=4) THEN des.vars[v^.adr].am:=am_aFP;
        ELSE des.vars[v^.adr].am:=am_aaFP;
        END;
      END;
      IF des.vars[v^.adr].am=am_aFP THEN des.vars[v^.adr].disp:=adr;
      ELSE des.vars[v^.adr].n:=adr;
      END;
      INC(adr,4);
    END;
    des.vars[v^.adr].level:=des.level+1;
    v:=v^.r;
  END;
END gen_profile;

PROCEDURE gen_range(u: ref);
  VAR sg: BOOLEAN; sz: INTEGER;
  PROCEDURE pack_range(VAL l,r: access);
  BEGIN
    IF (l.am#am_imm) OR (r.am#am_imm) THEN RETURN END;
    IF l.n>r.n THEN sz:=1; sg:=TRUE; RETURN END;
    IF    (l.n>=-80h)   & (r.n<=7Fh)    THEN sz:=1; sg:=TRUE;
    ELSIF (l.n>=0h)     & (r.n<=0FFh)   THEN sz:=1; sg:=FALSE;
    ELSIF (l.n>=-8000h) & (r.n<=7FFFh)  THEN sz:=2; sg:=TRUE;
    ELSIF (l.n>=0h)     & (r.n<=0FFFFh) THEN sz:=2; sg:=FALSE;
    ELSE  sz:=4; sg:=TRUE;
    END;
  END pack_range;
  VAR el,er: access; c: BOOLEAN;
BEGIN
  u^.adr:=des.new_rng();
  sg:=des.sign?(u^.dw); sz:=exp.bytes(u^.dw);
  exp.expression(u^.l,el);
  exp.expression(u^.r,er);
  c:=des.imm_int?(el,sg,sz);
  c:=des.imm_int?(er,sg,sz) & c;
  IF c & (u^.md=tbl.range) THEN pack_range(el,er) END;
  alloc_range(u^.adr,el,er,sg,sz);
END gen_range;

PROCEDURE gen_enumeration(u: ref);
  VAR el,er: access; sz: INTEGER; c: ref;
BEGIN
  u^.adr:=des.new_rng();
  c:=u^.nxt;
  IF (c#NIL) & (c^.md=tbl.const) & (c^.l=u) THEN
    exp.expression(c^.dw,el);
    WHILE (c^.nxt#NIL) &
          (c^.nxt^.md=tbl.const) & (c^.nxt^.l=u) DO c:=c^.nxt END;
    exp.expression(c^.dw,er);
  ELSE
    er:=const; el:=const; el.n:=0; er.n:=-1;
  END;
  IF NOT des.imm_int?(el,TRUE,4) THEN error(u,assert) END;
  IF NOT des.imm_int?(er,TRUE,4) THEN error(u,assert) END;
  IF el.n>er.n THEN sz:=1; er.n:=-1; el.n:=0;
  ELSIF (el.n>=-80h) & (er.n<=7Fh) THEN sz:=1;
  ELSIF (el.n>=-8000h) & (er.n<=7FFFh) THEN sz:=2;
  ELSE sz:=4
  END;
  WITH des.rngs[u^.adr] DO
    r.disp:=des.new_str((2*sz+3) DIV 4)*4;
    l.disp:=r.disp+sz;
    l.am:=am_aSBimm;
    bblt(ADR(des.scode),l.disp*8,ADR(el.n),0,sz*8);
    r.am:=am_aSBimm;
    bblt(ADR(des.scode),r.disp*8,ADR(er.n),0,sz*8);
    sign:=TRUE; size:=sz;
  END;
END gen_enumeration;

PROCEDURE gen_set(l: ref);
  VAR fr,to: access; sg: BOOLEAN; sz: INTEGER;
BEGIN
  IF l^.dw=NIL THEN
    error(l,assert)
  ELSIF (l^.dw^.md=tbl.range) & des.rngs[l^.dw^.adr].sign THEN
    l^.adr:=l^.dw^.adr;
  ELSIF (l^.dw^.md=tbl.subtype) & des.rngs[l^.dw^.adr].sign THEN
    l^.adr:=l^.dw^.adr;
  ELSIF (l^.dw^.md=tbl.enumeration) & des.rngs[l^.dw^.adr].sign THEN
    l^.adr:=l^.dw^.adr;
  ELSE
    l^.adr:=des.new_rng();
    des.min(l^.dw,fr); des.max(l^.dw,to);
    sg:=des.sign?(l^.l); sz:=exp.bytes(l^.dw);
    des.sub_base(fr,sg,sz);
    des.sub_base(to,sg,sz);
    IF des.imm_int?(fr,sg,sz) THEN END;
    IF des.imm_int?(to,sg,sz) THEN END;
    alloc_range(l^.adr,fr,to,TRUE,sz);
  END;
END gen_set;

PROCEDURE gen_proc(l: ref);
BEGIN
  l^.adr:=des.new_proc();
  WITH des.prcs[l^.adr] DO
    mod:=0;
    WITH des.cu DO disp:=prc_sz; INC(prc_sz) END;
    lvl:=des.level+1;
    loc_sz:=0;
  END;
END gen_proc;

PROCEDURE gen_code_body(l: ref);
BEGIN
  HALT(1)
END gen_code_body;

PROCEDURE assign_access(l: ref; VAL d,lsz: access);
  VAR e,rsz: access; m: val_mode; r_stk,f_stk: INTEGER;
BEGIN
  r_stk:=free_regs.stk; f_stk:=exp.fpp_regs.stk;
  m:=exp.vm(l^.dw);
  IF m IN val_set{vm_integer,vm_cardinal,vm_real} THEN
    expression_assign(l,d,m,exp.bytes(l^.dw));
  ELSE
    exp.expression(l,e); des.bytes_u(l,rsz);
    IF (lsz.am=am_imm) & (rsz.am=am_imm) THEN
      IF rsz.n>lsz.n THEN error(l,'left size less then right one') END;
      put_move_csz(e,d,rsz.n);
    ELSE
      put_check_move(e,d,rsz,lsz);
    END;
  END;
  free_regs.stk:=r_stk; exp.fpp_regs.stk:=f_stk;
END assign_access;

PROCEDURE gen_assign(l: ref);
  VAR d,lsz: access;
BEGIN
  des.designator_u(l^.l,d);
  des.bytes_u(l^.l,lsz);
  assign_access(l^.r,d,lsz);
END gen_assign;

PROCEDURE gen_return(l: ref);
  VAR n: INTEGER; e,sz: access;
BEGIN
  n:=profile_psz(proc^.l^.l);
  IF proc^.l^.l^.l#NIL THEN
    des.bytes(proc^.l^.l^.l,sz);
    IF (sz.am#am_imm) OR NOT (sz.n IN {1,2,4}) THEN
      error(l,'illegal result size');
    END;
    exp.expression(l^.dw,e);
    IF (e.am#am_RG) OR (e.rg#0) THEN
      reg.rg:=0; put.cmd(put.mov,e,reg,sz.n);
    END;
  END;
  put.exit({});
  IF des.level=1 THEN put.rxp(n) ELSE put.ret(n+4) END;
  exp.start(exp.new());
END gen_return;

PROCEDURE gen_call_cmd(l: ref; func: BOOLEAN);
  PROCEDURE new_high(VAR sz,e,h: access);
  BEGIN
    IF (sz.am=am_imm) & (e.am=am_imm) THEN
      h:=const; h.n:=sz.n DIV e.n -1;
    ELSIF sz.am=am_RG THEN
      put.cmd(put.div,e,sz,4);
      const.n:=1; put.cmd(put.sub,const,sz,4);
      h:=sz;
    ELSE
      des.alloc_reg(reg);
      put.cmd(put.mov,sz,reg,4);
      put.cmd(put.div,e,reg,4);
      const.n:=1; put.cmd(put.sub,const,reg,4);
      h:=reg;
    END;
  END new_high;
  PROCEDURE gen_parms(pf: ref);
    VAR ass,prm,val,t: ref; b,e,h,sz: access; stk: INTEGER;
  BEGIN
    const.n:=profile_psz(pf);
    put.adjsp(const,4);
    stk:=free_regs.stk;
    ass:=l^.r;
    WHILE ass#NIL DO
      IF ass^.md=tbl.assign THEN
        prm:=ass^.l; val:=ass^.r;
        IF prm^.md#tbl.var THEN error(l,assert) END;
        t:=prm^.l;
        LOOP
          IF t=NIL THEN error(l,assert)
          ELSIF t^.md=tbl.var_prm THEN t:=t^.dw
          ELSIF t^.md=tbl.val_prm THEN t:=t^.dw
          ELSE EXIT
          END;
        END;
        IF des.array_of?(t) THEN
          des.adr_u(val,b); des.bytes_u(val,sz); des.bytes(t^.r,e);
          new_high(sz,e,h);
          aSP.disp:=des.vars[prm^.adr].disp-12;
          put.cmd(put.mov,b,aSP,4);
          INC(aSP.disp,4);
          put.cmd(put.mov,h,aSP,4);
          INC(aSP.disp,4);
          const.n:=0; put.cmd(put.mov,const,aSP,4);
        ELSIF des.vars[prm^.adr].am=am_aFP THEN
          CASE exp.vm(t) OF
            |vm_integer,vm_cardinal,vm_real:
              aSP.disp:=des.vars[prm^.adr].disp-12;
              expression_assign(val,aSP,exp.vm(t),exp.bytes(t));
          ELSE
            des.bytes(t,sz); imm?(sz);
            aSP.disp:=des.vars[prm^.adr].disp-12;
            exp.expression(val,e);
            put_move_csz(e,aSP,sz.n);
          END;
        ELSIF des.vars[prm^.adr].am=am_aaFP THEN
          IF des.array_of?(val^.dw) THEN des.designator(val,b)
          ELSE des.adr(val,b)
          END;
          aSP.disp:=des.vars[prm^.adr].n-12;
          put.cmd(put.mov,b,aSP,4);
        ELSE error(val,assert);
        END;
        free_regs.stk:=stk;
      END;
      ass:=ass^.nxt;
    END;
  END gen_parms;
  VAR prc: ref; n,i: INTEGER; b,v: access; next: node;
BEGIN
  ASSERT(l^.md=tbl.call);
  IF l^.l=NIL THEN error(l,'call^.l=NIL');
  ELSIF func & (l^.dw=NIL) THEN error(l,'func call^.dw=NIL');
  ELSIF (l^.l^.md=tbl.usage) & (l^.l^.r#NIL) &
    ((l^.l^.r^.md=tbl.procedure) OR (l^.l^.r^.md=tbl.inline)) THEN
    prc:=l^.l^.r; gen_parms(prc^.l);
    IF prc^.md=tbl.procedure THEN
      n:=des.prcs[prc^.adr].lvl;
      IF n>1 THEN
        ASSERT(des.level+1>=n);
        IF n=des.level+1 THEN
          put.spr(TOS,put.FP,4);
        ELSIF n=des.level THEN
          aFP.disp:=8; put.cmd(put.mov,aFP,TOS,4);
        ELSIF n=des.level-1 THEN
          aaFP.disp:=8; aaFP.n:=8; put.cmd(put.mov,aaFP,TOS,4);
        ELSE
          aRG.rg:=0; aRG.disp:=8; reg.rg:=0;
          aaFP.disp:=8; aaFP.n:=8; put.cmd(put.mov,aaFP,reg,4);
          FOR i:=n+3 TO des.level DO put.cmd(put.mov,aRG,reg,4) END;
          put.cmd(put.mov,aRG,TOS,4);
        END;
      END;
      next:=exp.new(); block^.md:=nm_call;
      block^.proc:=prc^.adr; block^.true:=next;
      exp.start(next);
    ELSIF prc^.md=tbl.inline THEN
      FOR n:=0 TO HIGH(des.inln[prc^.adr]) DO
        put.put(INTEGER(des.inln[prc^.adr][n]));
      END;
    ELSE error(l,'must be procedure')
    END;
  ELSE
    exp.expression(l^.l,b);
    IF (b.am=am_imm) OR (b.am=am_RG) THEN
      des.alloc_var(v,4); put.cmd(put.mov,b,v,4); b:=v;
    END;
    gen_parms(l^.l^.dw); put.cxpd(b);
  END;
END gen_call_cmd;

PROCEDURE gen_call(l: ref; func: BOOLEAN; VAR a: access);
  VAR
    rgs,fpp  : des.regs;
    rset,fset: BITSET;
    i,sz     : INTEGER;
    m        : val_mode;
BEGIN
  rgs:=free_regs; fpp:=fpp_regs;
  rset:={}; fset:={};
  FOR i:=rgs.lim TO rgs.stk+des.regs_no-1 DO INCL(rset,i MOD des.regs_no) END;
  FOR i:=fpp.lim TO fpp.stk+fpp_regs_no-1 DO INCL(fset,i MOD fpp_regs_no) END;
  -- rset - set of durty registers
  -- fset - set of durty fpp registers
  freg.am:=am_FPP4;
  FOR i:=0 TO fpp_regs_no-1 DO
    IF i IN fset THEN freg.rg:=i; put.cmd(put.movf,freg,TOS,4) END;
  END;
  put.save(rset);
  free_regs.stk:=0; free_regs.lim:=des.regs_no;
  free_regs.res:=des.regs_no; free_regs.max:=0;
  fpp_regs.stk:=0; fpp_regs.lim:=fpp_regs_no;
  fpp_regs.res:=fpp_regs_no; fpp_regs.max:=0;
  gen_call_cmd(l,func);
  IF fpp_regs.max>=fpp_regs.lim THEN error(l,'FPP registers overflow') END;
  IF free_regs.max>=free_regs.lim THEN error(l,'CPU registers overflow') END;
  free_regs:=rgs; fpp_regs:=fpp;
  IF func THEN
    sz:=exp.bytes(l^.dw);
    IF exp.vm(l^.dw)=vm_real THEN
      exp.alloc_fpp_reg(a,sz); freg.rg:=0; freg.am:=a.am;
      IF a.rg#0 THEN put.cmd(put.movf,freg,a,sz) END;
    ELSE
      des.alloc_reg(a); reg.rg:=0;
      IF a.rg#0 THEN put.cmd(put.mov,reg,a,sz) END;
    END;
  END;
  put.restore(rset); freg.am:=am_FPP4;
  FOR i:=fpp_regs_no-1 TO 0 BY -1 DO
    IF i IN fset THEN freg.rg:=i; put.cmd(put.movf,TOS,freg,4) END;
  END;
END gen_call;

PROCEDURE gen_if(l: ref);
  VAR n: ref; next,then,else: node;
BEGIN
  then:=exp.new(); else:=exp.new(); next:=exp.new();
  exp.gen_condition(l^.dw,then,else);
  exp.start(then); n:=l^.l;
  WHILE n#NIL DO one(n); n:=n^.nxt END;
  block^.true:=next; exp.start(else); n:=l^.r;
  WHILE n#NIL DO one(n); n:=n^.nxt END;
  block^.true:=next; exp.start(next);
END gen_if;

PROCEDURE gen_for(l: ref; next: node);
  VAR
    fr,to,by   : access;
    va         : access;
    loop       : node;
    i,r_to,r_by: ref;
    rgs        : INTEGER;
    m          : val_mode;
    vsz,tn     : INTEGER;
BEGIN
  rgs:=free_regs.stk;
  IF l^.l^.l^.md#tbl.usage THEN error(l^.l^.l,'must be variable') END;
  m:=exp.vm(l^.l^.l^.dw);
  IF (m#vm_integer) & (m#vm_cardinal) THEN error(l,ill_type) END;
  vsz:=exp.bytes(l^.l^.l^.dw);
  des.designator(l^.l^.l,va);
  IF (va.am=am_aRG) OR (va.xm#xm_off) THEN error(l,assert) END;
  exp.expression(l^.l^.r,fr);
  put.cmd(put.mov,fr,va,vsz);
  i:=l^.l^.nxt; r_to:=NIL; r_by:=NIL;
  WHILE i#NIL DO
    IF i^.md=tbl.exit THEN r_to:=i;
    ELSIF i^.md=tbl.plus THEN r_by:=i;
    ELSE error(i,assert);
    END;
    i:=i^.nxt;
  END;
  IF r_to#NIL THEN
    exp.expression(r_to^.dw,to);
    IF NOT des.imm_int?(to,m=vm_integer,vsz) THEN
      tn:=alloc_var(vsz,FALSE);
      put.cmd(put.mov,to,des.vars[tn],vsz);
      to:=des.vars[tn];
    END;
  END;
  IF r_by#NIL THEN
    exp.expression(r_by^.r,by);
    IF NOT des.imm_int?(by,m=vm_integer,vsz) THEN error(r_by,assert) END;
  ELSE by:=const; by.n:=1;
  END;
  free_regs.stk:=rgs;
  IF (r_to#NIL) & (to.am=am_imm) & (fr.am=am_imm) & (by.am=am_imm) THEN
    IF by.n>=0 THEN IF fr.n>to.n THEN RETURN END;
    ELSE IF fr.n<to.n THEN RETURN END;
    END;
  END;
  loop:=exp.new();
  block^.true:=loop; exp.start(block^.true);
  IF (r_to#NIL) & ((to.am#am_imm) OR (fr.am#am_imm)) THEN
    put.cmd(put.cmp,va,to,vsz);
    IF m=vm_integer THEN
      IF by.n>=0 THEN block^.flag:=put.LE ELSE block^.flag:=put.GE END;
    ELSE
      IF by.n>=0 THEN block^.flag:=put.LS ELSE block^.flag:=put.HS END;
    END;
    block^.false:=next; block^.true:=exp.new();
    block^.md:=nm_cond; exp.start(block^.true);
  END;
  i:=l^.dw;
  WHILE i#NIL DO one(i); i:=i^.nxt END;
  IF (r_to#NIL) & (fr.am=am_imm) & (to.am=am_imm) THEN
    IF by.n#0 THEN
      put.cmd(put.add,by,va,vsz);
      put.cmd(put.cmp,va,to,vsz);
      IF m=vm_integer THEN
        IF by.n>=0 THEN block^.flag:=put.LE ELSE block^.flag:=put.GE END;
      ELSE
        IF by.n>=0 THEN block^.flag:=put.LS ELSE block^.flag:=put.HS END;
      END;
      block^.false:=next; block^.true:=loop;
      block^.md:=nm_cond;
    ELSE
      block^.true:=loop;
    END;
  ELSE
    put.cmd(put.add,by,va,vsz);
    block^.true:=loop;
  END;
  exp.start(next);
END gen_for;

PROCEDURE gen_loop(l: ref);
  VAR i: ref; loop,next,body: node;
BEGIN
  next:=exp.new(); l^.adr:=INTEGER(next);
  IF l^.l#NIL THEN
    IF l^.l^.md=tbl.assign THEN
      gen_for(l,next);
    ELSE
      body:=exp.new(); loop:=exp.new(); block^.true:=loop; exp.start(loop);
      exp.gen_condition(l^.l,body,next);
      exp.start(body); i:=l^.dw;
      WHILE i#NIL DO one(i); i:=i^.nxt END;
      block^.true:=loop; exp.start(next);
    END;
  ELSIF l^.r#NIL THEN
    loop:=exp.new(); block^.true:=loop;
    exp.start(loop); i:=l^.dw;
    WHILE i#NIL DO one(i); i:=i^.nxt END;
    exp.gen_condition(l^.r,next,loop);
    exp.start(next);
  ELSE
    loop:=exp.new(); block^.true:=loop;
    exp.start(loop); i:=l^.dw;
    WHILE i#NIL DO one(i); i:=i^.nxt END;
    block^.true:=loop; exp.start(next);
  END;
  l^.adr:=0;
END gen_loop;

PROCEDURE gen_exit(l: ref);
BEGIN
  IF (l^.dw=NIL) OR (l^.dw^.md#tbl.loop) OR (l^.dw^.adr=0) THEN
    error(l,assert);
  END;
  block^.true:=node(l^.dw^.adr);
  exp.start(exp.new());
END gen_exit;

PROCEDURE gen_case(l: ref);
  VAR
    lbs           : ARRAY [0..999] OF INTEGER;
    base,min,max  : INTEGER;
    i,n,m         : INTEGER;
    sel_no        : INTEGER;
    b             : access;
    sg            : BOOLEAN;
    size          : INTEGER;
    stk           : des.regs;
  PROCEDURE var_case;
    VAR
      else,next,case: node;
      k,k1          : ref;
      str           : INTEGER;
      bounds        : access;
  BEGIN
    else:=exp.new(); next:=exp.new();
    IF NOT sg THEN des.integer(b,sg,size); size:=4 END;
    str:=des.new_str((size*2+3) DIV 4);
    bblt(ADR(des.scode[str]),0,ADR(max),0,size*8);
    bblt(ADR(des.scode[str]),size*8,ADR(min),0,size*8);
    bounds:=reg; bounds.am:=am_aSB; bounds.rg:=0; bounds.disp:=str*4;
    IF b.am=am_RG THEN reg.rg:=b.rg ELSE des.alloc_reg(reg) END;
    put.check(bounds,b,reg.rg,size);
    block^.md:=nm_cond; block^.flag:=put.FC;
    block^.false:=else; block^.true:=exp.new();
    exp.start(block^.true);
    free_regs:=stk;
    case:=block;
    case^.md:=nm_case;
    case^.false:=else;
    case^.proc:=reg.rg;
    NEW(case^.ctbl,max-min+1);
    move(ADR(case^.ctbl),ADR(lbs[min-base]),max-min+1);
    NEW(case^.alts,sel_no);
    k1:=l^.l; sel_no:=0;
    WHILE k1#NIL DO
      case^.alts[sel_no]:=exp.new(); exp.start(case^.alts[sel_no]); INC(sel_no);
      k:=k1^.r;
      WHILE k#NIL DO one(k); k:=k^.nxt END;
      block^.true:=next; k1:=k1^.nxt;
    END;
    exp.start(else);
    k:=l^.r;
    IF k=NIL THEN trap(svcn_case);
    ELSE WHILE k#NIL DO one(k); k:=k^.nxt END;
    END;
    block^.true:=next; exp.start(next);
  END var_case;
  PROCEDURE const_case(v: INTEGER);
    VAR k: ref;
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
  VAR s,r: ref; t: des.trap;
BEGIN
  base:=0;
  sg:=des.sign?(l^.dw^.dw);
  size:=exp.bytes(l^.dw^.dw);
  LOOP
    des.save(t);
    min:=MAX(INTEGER); max:=MIN(INTEGER);
    FOR i:=0 TO HIGH(lbs) DO lbs[i]:=-1 END;
    sel_no:=0; s:=l^.l;
    IF s=NIL THEN exp.expression(l^.dw,b); const_case(0); RETURN END;
    WHILE s#NIL DO
      IF s^.md=tbl.select THEN
        r:=s^.l;
        WHILE r#NIL DO
          IF r^.md=tbl.range THEN
            exp.expression(r^.l,b);
            IF NOT des.imm_int?(b,sg,size) THEN
              error(r,'case labels must be constant')
            END;
            m:=b.n; n:=b.n;
            IF r^.r#NIL THEN
              exp.expression(r^.r,b);
              IF NOT des.imm_int?(b,sg,size) THEN
                error(r,'case labels must be constant')
              END;
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
    des.restore(t);
    IF (min>=base) & (max<=base+HIGH(lbs)) THEN EXIT END;
    IF max-min>HIGH(lbs) THEN error(l,'too large labels range') END;
    base:=min;
  END;
  stk:=free_regs;
  exp.expression(l^.dw,b);
  IF des.imm_int?(b,sg,size) THEN const_case(b.n) ELSE var_case END;
END gen_case;

PROCEDURE gen_incl_excl(excl: BOOLEAN; l: ref);
  VAR la,ra,bnd: access; sg: BOOLEAN; sz: INTEGER;
BEGIN
  IF exp.vm(l^.l^.dw)#vm_set THEN error(l^.l,ill_type) END;
  des.designator(l^.l,la); exp.expression(l^.r,ra);
  sz:=exp.bytes(l^.r^.dw); sg:=des.sign?(l^.r^.dw); des.sub_base(ra,sg,sz);
  bnd:=des.rngs[l^.l^.dw^.adr].r; des.gb(bnd);
  IF ra.am=am_RG THEN reg:=ra ELSE des.alloc_reg(reg) END;
  put.check(bnd,ra,reg.rg,sz); put.flag;
  IF excl THEN put.cmd(put.cbit,reg,la,sz);
  ELSE put.cmd(put.sbit,reg,la,sz);
  END;
END gen_incl_excl;

PROCEDURE gen_inc_dec(dec: BOOLEAN; l: ref);
  VAR md: val_mode; sz: INTEGER; e,d: access;
BEGIN
  md:=exp.vm(l^.l^.dw);
  des.designator(l^.l,d);
  IF l^.r=NIL THEN e:=const; e.n:=1 ELSE exp.expression(l^.r,e) END;
  CASE md OF
    |vm_integer :
      sz:=exp.bytes(l^.l^.dw);
      IF dec THEN put.cmd(put.sub,e,d,sz) ELSE put.cmd(put.add,e,d,sz) END;
      put.flag;
    |vm_cardinal:
      sz:=exp.bytes(l^.l^.dw);
      IF dec THEN put.cmd(put.sub,e,d,sz) ELSE put.cmd(put.add,e,d,sz) END;
  ELSE error(l,ill_type);
  END;
END gen_inc_dec;

PROCEDURE gen_program_check(l: ref);
  VAR b: access;
BEGIN
  exp.expression(l^.l,b);
  IF NOT des.imm_int?(b,FALSE,1) THEN
    error(l,'program_check must be constant');
  END;
  IF b.n=0 THEN error(l,'PROGRAM CHECK: something wrong') END;
END gen_program_check;

PROCEDURE gen_halt(l: ref);
  VAR a: access;
BEGIN
  IF l^.r#NIL THEN
    exp.expression(l^.r,a); put.cmd(put.mov,a,TOS,4); put.svc;
  ELSE
    trap(svcn_halt);
  END;
END gen_halt;

PROCEDURE gen_assert(l: ref);
  VAR true,false: node; a: access;
BEGIN
  true:=exp.new(); false:=exp.new();
  exp.gen_condition(l^.l,true,false);
  exp.start(false);
  IF l^.r#NIL THEN
    exp.expression(l^.r,a); put.cmd(put.mov,a,TOS,4); put.svc;
  ELSE
    trap(svcn_assert);
  END;
  block^.true:=true;
  exp.start(true);
END gen_assert;

PROCEDURE gen_new(l: ref);
BEGIN
  HALT(1)
END gen_new;

PROCEDURE gen_resize(l: ref);
BEGIN
  HALT(1)
END gen_resize;

PROCEDURE gen_dispose(l: ref);
BEGIN
  HALT(1)
END gen_dispose;

PROCEDURE one(l: ref);
  VAR rgs,fpp: INTEGER;
BEGIN
  rgs:=free_regs.stk; fpp:=fpp_regs.stk;
  IF l^.md#tbl.number THEN stat_pos:=l^.pos END;
  CASE l^.md OF
    |tbl.procedure      : gen_proc(l);
    |tbl.inline         : l^.adr:=des.new_inln();
    |tbl.program_check  : gen_program_check(l);
    |tbl.block          : l:=l^.dw; WHILE l#NIL DO one(l); l:=l^.nxt END;
    |tbl.assign         : gen_assign(l);
    |tbl.var            : gen_var(l);
    |tbl.return         : gen_return(l);
    |tbl.module         : l:=l^.dw; WHILE l#NIL DO one(l); l:=l^.nxt END;
    |tbl.const          : gen_const(l);
    |tbl.array          : gen_array(l);
    |tbl.array_of       :
    |tbl.dynarr         :
    |tbl.packed_array   : gen_array(l);
    |tbl.packed_array_of:
    |tbl.packed_dynarr  :
    |tbl.number         :
    |tbl.record         : gen_record(l);
    |tbl.packed_record  : gen_record(l);
    |tbl.profile        : gen_profile(l);
    |tbl.call           : gen_call(l,FALSE,tmp);
    |tbl.if             : gen_if(l);
    |tbl.case           : gen_case(l);
    |tbl.loop           : gen_loop(l);
    |tbl.exit           : gen_exit(l);
    |tbl.incl           : gen_incl_excl(FALSE,l);
    |tbl.excl           : gen_incl_excl(TRUE,l);
    |tbl.inc            : gen_inc_dec(FALSE,l);
    |tbl.dec            : gen_inc_dec(TRUE,l);
    |tbl.enumeration    : gen_enumeration(l);
    |tbl.boolean        :
    |tbl.char           :
    |tbl.range          : gen_range(l);
    |tbl.subtype        : gen_range(l);
    |tbl.set            : gen_set(l);
    |tbl.integer        :
    |tbl.real           :
    |tbl.pointer        :
    |tbl.proc_body      : last_proc^.r:=l; last_proc:=l;
    |tbl.code_body      : gen_code_body(l);
    |tbl.halt           : gen_halt(l);
    |tbl.assert         : gen_assert(l);
    |tbl.new            : gen_new(l);
    |tbl.resize         : gen_resize(l);
    |tbl.dispose        : gen_dispose(l);
  ELSE
    error(l,assert);
  END;
  free_regs.stk:=rgs; fpp_regs.stk:=fpp;
END one;

PROCEDURE save_parms;
BEGIN
  IF des.level=0 THEN RETURN END;
  WITH des.prcs[proc^.l^.adr] DO put.enter({},loc_sz) END;
END save_parms;

PROCEDURE copy_parms;
  VAR l,t: ref; sz,prm,high: access;
BEGIN
  IF des.level=0 THEN RETURN END;
  l:=proc^.l^.l^.dw;
  WHILE l#NIL DO
    IF l^.md#tbl.var THEN
      one(l);
    ELSE
      t:=l^.l;
      IF t=NIL THEN error(l,assert)
      ELSIF t^.md=tbl.val_prm THEN -- nothing
      ELSIF t^.md=tbl.var_prm THEN -- nothing
      ELSIF des.array_of?(t) THEN
        prm:=des.vars[l^.adr];
        high:=prm; high.disp:=prm.disp+8;
        reg.rg:=0;
        free_regs.stk:=1;
        put.cmd(put.mov,high,reg,4);
        const.n:=1; put.cmd(put.add,const,reg,4);
        des.bytes(t^.r,sz);
        put.cmd(put.mul,sz,reg,4);
        put.adjsp(reg,4);
        reg.rg:=1; put.cmd(put.mov,prm,reg,4);
        reg.rg:=2; put.cmd(put.addr,TOS,reg,4);
        put.cmd(put.mov,reg,prm,4);
        put.movs(1,FALSE,FALSE,FALSE);
      ELSIF des.vars[l^.adr].am=am_aaFP THEN
        prm:=const; prm.am:=am_aFP; prm.n:=0;
        prm.disp:=des.vars[l^.adr].n;
        des.bytes_d(t,sz);
        reg.rg:=0;
        IF (sz.am#am_RG) OR (sz.rg#0) THEN
          put.cmd(put.mov,sz,reg,4);
        END;
        put.adjsp(reg,4);
        reg.rg:=1; put.cmd(put.mov,prm,reg,4);
        reg.rg:=2; put.cmd(put.addr,TOS,reg,4);
        put.cmd(put.mov,reg,prm,4);
        put.movs(1,FALSE,FALSE,FALSE);
      END;
      free_regs.stk:=0;
      free_regs.lim:=des.regs_no;
      free_regs.res:=des.regs_no;
      free_regs.max:=0;
    END;
    l:=l^.nxt;
  END;
END copy_parms;

PROCEDURE proc_body;
  VAR l: ref; n: INTEGER;
BEGIN
  ASSERT(proc^.md=tbl.proc_body);
  l:=proc^.dw;
  WHILE l#NIL DO one(l); l:=l^.nxt END;
  IF NOT LAST THEN RETURN END;
  IF des.level=0 THEN
    reg.rg:=0;
    put.cmd(put.mov,glo_alloc_cnt,reg,4);
    put.rxp(0);
  ELSE
    ASSERT(proc^.l#NIL);
    ASSERT(proc^.l^.l#NIL);
    WITH des.prcs[proc^.l^.adr] DO put.exit({}) END;
    n:=profile_psz(proc^.l^.l);
    IF proc^.l^.l^.l#NIL THEN trap(svcn_return) END;
    IF des.level=1 THEN put.rxp(n) ELSE put.ret(n+4) END;
  END;
END proc_body;

PROCEDURE do_flow(): node;
  VAR first,second: node;
BEGIN
  ASSERT(proc#NIL); ASSERT(proc^.md=tbl.proc_body);
  IF proc^.l^.md=tbl.module THEN
    des.level:=0
  ELSE
    ASSERT(proc^.l^.md=tbl.procedure);
    ASSERT(proc^.l^.l#NIL);
    ASSERT(proc^.l^.l^.md=tbl.profile);
    des.level:=des.prcs[proc^.l^.adr].lvl
  END;
  stat_pos:=0;
  free_regs.stk:=0; free_regs.lim:=des.regs_no;
  free_regs.res:=des.regs_no; free_regs.max:=0;
  fpp_regs.stk:=0; fpp_regs.lim:=des.regs_no;
  fpp_regs.res:=des.regs_no; fpp_regs.max:=0;
  block:=NIL;
  glo_alloc_cnt:=const;
  glo_alloc_cnt.n:=0;
  second:=exp.new(); exp.start(second);
  proc_body;
  first:=exp.new(); exp.start(first);
  save_parms;
  copy_parms;
  block^.true:=second;
  exp.finish;
  RETURN first;
END do_flow;

BEGIN
  WITH const DO
    am:=am_imm;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH reg DO
    am:=am_RG;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH freg DO
    am:=am_FPP4;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH aRG DO
    am:=am_aRG;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH TOS DO
    am:=am_TOS;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH aSP DO
    am:=am_aSP;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH aFP DO
    am:=am_aFP;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  WITH aaFP DO
    am:=am_aaFP;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  exp.gen_call:=gen_call;
  des.assign:=assign_access;
END nsStat.
