IMPLEMENTATION MODULE pcGenDesig; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT mcd : defCodes;
IMPORT put : pcGenCmd;
IMPORT exp : pcGenExpr;
IMPORT gen : krSym;

FROM pcTab      IMPORT  ref;
FROM pcGenExpr  IMPORT  type, load_size, load_min, load_max, gen_load,
                        calc_load, try_load, box, error, box_mode,
                        proc, load_val, load_bits, gen_var,
                        load_adr, save, restore, trap;

WITH STORAGE : mem;

CONST
  assert     = 'undefined error, compilation fault';
  unrealize  = 'unrealized, compilation fault';
  type_check = 'incompatible types';
  range_check= 'range check violation';

PROCEDURE move(a,b: ADDRESS; s: INTEGER); CODE mcd.move END move;
PROCEDURE incl(a: ADDRESS; n: INTEGER); CODE mcd.incl END incl;
PROCEDURE bblt(a: ADDRESS; a1: INTEGER;
               b: ADDRESS; b1: INTEGER; sz: INTEGER); CODE mcd.bblt END bblt;

PROCEDURE new_var(): INTEGER;
BEGIN
  IF vars_no>HIGH(vars) THEN RESIZE(vars,vars_no*2) END;
  WITH vars[vars_no] DO
    am:=gen.am_G; lvl:=0; no:=0; disp:=0;
  END;
  INC(vars_no);
  RETURN vars_no-1;
END new_var;

PROCEDURE new_rng(): INTEGER;
BEGIN
  IF rngs_no>HIGH(rngs) THEN RESIZE(rngs,rngs_no*2) END;
  WITH rngs[rngs_no] DO
    l.am:=gen.am_imm; l.lvl:=0; l.no:=0; l.disp:=0;
    r.am:=gen.am_imm; r.lvl:=0; r.no:=0; r.disp:=0;
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
  WITH prcs[prcs_no] DO mod:=0; lvl:=0; disp:=0; loc_sz:=4 END;
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

PROCEDURE new_ref(m: tbl.mode): ref;
  VAR r: ref;
BEGIN
  mem.ALLOCATE(r,SIZE(r^));
  r^.md:=m;
  r^.dw:=NIL;
  r^.nxt:=NIL;
  r^.r:=NIL;
  r^.l:=NIL;
  r^.adr:=0;
  r^.nm:=0;
  r^.val:=exp.stat_pos;
  RETURN r;
END new_ref;

PROCEDURE convert_toAGL(VAR a: gen.access);
BEGIN
  CASE a.am OF
    |gen.am_G :
      IF a.lvl#0 THEN
        put.lea(a.lvl,a.disp DIV 32); a.disp:=a.disp MOD 32; a.am:=gen.am_adr;
      END;
    |gen.am_aG:
      IF a.lvl=0 THEN put.lgw(a.no) ELSE put.lew(a.lvl,a.no) END;
      a.am:=gen.am_adr;
    |gen.am_L :
      IF a.lvl#exp.level THEN put.gb(exp.level-a.lvl); a.am:=gen.am_adr END;
    |gen.am_aL:
      IF a.lvl=exp.level THEN put.llw(a.no)
      ELSE put.gb(exp.level-a.lvl); put.lsw(a.no)
      END;
      a.am:=gen.am_adr;
    |gen.am_STR:
      IF a.lvl=0 THEN
        put.lsta(a.disp DIV 32); a.disp:=a.disp MOD 32; a.am:=gen.am_adr;
      ELSE put.lew(a.lvl,1); a.am:=gen.am_adr;
      END;
    |gen.am_adr:
    |gen.am_abs: put.li(a.no); a.am:=gen.am_adr;
    |gen.am_xw: put.c(mcd.add); a.am:=gen.am_adr;
  ELSE error(NIL,unrealize);
  END;
END convert_toAGL;

PROCEDURE convert_toABTmod32(VAR a: gen.access);
BEGIN
  CASE a.am OF
    |gen.am_L  :
      IF a.lvl=exp.level THEN put.lla(a.disp DIV 32);
      ELSE put.gb(exp.level-a.lvl); put.lsa(a.disp DIV 32);
      END;
      a.am:=gen.am_adr; a.disp:=a.disp MOD 32;
    |gen.am_aL :
      IF a.lvl=exp.level THEN put.llw(a.no);
      ELSE put.gb(exp.level-a.lvl); put.lsw(a.no);
      END;
      put.lsa(a.disp DIV 32); a.disp:=a.disp MOD 32; a.am:=gen.am_adr;
    |gen.am_G  :
      IF a.lvl=0 THEN put.lga(a.disp DIV 32)
      ELSE put.lea(a.lvl,a.disp DIV 32);
      END;
      a.am:=gen.am_adr; a.disp:=a.disp MOD 32;
    |gen.am_aG :
      IF a.lvl=0 THEN put.lgw(a.no) ELSE put.lew(a.lvl,a.no) END;
      put.lsa(a.disp DIV 32); a.disp:=a.disp MOD 32; a.am:=gen.am_adr;
    |gen.am_STR:
      IF a.lvl=0 THEN
        put.lsta(a.disp DIV 32); a.am:=gen.am_adr; a.disp:=a.disp MOD 32;
      ELSE
        put.lew(a.lvl,1); put.lsa(a.disp DIV 32);
        a.disp:=a.disp MOD 32; a.am:=gen.am_adr;
      END;
    |gen.am_adr: put.lsa(a.disp DIV 32); a.disp:=a.disp MOD 32;
    |gen.am_abs:
      put.li(a.no); put.lsa(a.disp DIV 32); a.disp:=a.disp MOD 32;
      a.am:=gen.am_adr;
    |gen.am_xw :
      put.lsa(a.disp DIV 32); a.disp:=a.disp MOD 32;
      put.c(mcd.add); a.am:=gen.am_adr;
    |gen.am_xb : put.lsa(a.disp DIV 8); a.disp:=a.disp MOD 8;
    |gen.am_xt : put.lsa(a.disp); a.disp:=0;
  ELSE error(NIL,unrealize);
  END;
END convert_toABTmod32;

PROCEDURE convert_toT(VAR a: gen.access);
BEGIN
  CASE a.am OF
    |gen.am_L  :
      IF a.lvl=exp.level THEN put.lla(a.disp DIV 32); put.li(a.disp MOD 32);
      ELSE put.gb(exp.level-a.lvl); put.li(a.disp);
      END;
      a.disp:=0;
    |gen.am_aL :
      IF a.lvl=exp.level THEN put.llw(a.no);
      ELSE put.gb(exp.level-a.lvl); put.lsw(a.no);
      END;
      put.li(a.disp); a.disp:=0;
    |gen.am_G  :
      IF a.lvl=0 THEN put.lga(a.disp DIV 32);
      ELSE put.lea(a.lvl,a.disp DIV 32);
      END;
      put.li(a.disp MOD 32); a.disp:=0;
    |gen.am_aG :
      IF a.lvl=0 THEN put.lgw(a.no) ELSE put.lew(a.lvl,a.no) END;
      put.li(a.disp); a.disp:=0;
    |gen.am_STR:
      IF a.lvl=0 THEN put.lsta(a.disp DIV 32); put.li(a.disp MOD 32);
      ELSE put.lew(a.lvl,1); put.li(a.disp);
      END;
      a.disp:=0;
    |gen.am_adr: put.li(a.disp); a.disp:=0;
    |gen.am_abs: put.li(a.no); put.li(a.disp); a.disp:=0;
    |gen.am_xw : put.c(mcd.add); put.li(a.disp); a.disp:=0;
    |gen.am_xb : put.li(3); put.c(mcd.shl);
    |gen.am_xt :
  ELSE error(NIL,assert);
  END;
  a.am:=gen.am_xt;
END convert_toT;

PROCEDURE convert_to_address(VAL a: gen.access);
  VAR d: INTEGER;
BEGIN
  IF a.disp MOD 32 # 0 THEN error(NIL,unrealize) END;
  d:=a.disp DIV 32;
  CASE a.am OF
    |gen.am_G : IF a.lvl#0 THEN put.lea(a.lvl,d) ELSE put.lga(d) END;
    |gen.am_aG:
      IF a.lvl=0 THEN put.lgw(a.no) ELSE put.lew(a.lvl,a.no) END;
      put.lsa(d);
    |gen.am_L :
      IF a.lvl=exp.level THEN put.lla(d)
      ELSE put.gb(exp.level-a.lvl); put.lsa(d)
      END;
    |gen.am_aL:
      IF a.lvl=exp.level THEN put.llw(a.no)
      ELSE put.gb(exp.level-a.lvl); put.lsw(a.no)
      END;
      put.lsa(d);
    |gen.am_STR:
      IF a.lvl=0 THEN put.lsta(d) ELSE put.lew(a.lvl,1); put.lsa(d) END;
    |gen.am_adr: put.lsa(d);
    |gen.am_abs: put.li(a.no+d);
    |gen.am_xw : put.c(mcd.add); put.lsa(d);
  ELSE error(NIL,unrealize);
  END;
END convert_to_address;

PROCEDURE gen_bit(fr,to: INTEGER);
BEGIN
  put.lsa(-fr);
  IF to-fr<31 THEN put.li(to-fr); put.c(mcd.chkz) END;
  put.c(mcd.bit);
END gen_bit;

PROCEDURE imm?(VAL b: box);
BEGIN
  IF b.bm#bm_imm THEN error(NIL,unrealize) END;
END imm?;

PROCEDURE stk?(VAL b: box);
BEGIN
  IF b.bm#bm_stk THEN error(NIL,unrealize) END;
END stk?;

PROCEDURE bitset_aggregate(l: ref; VAR res: gen.access);
  VAR min,max: box;
  PROCEDURE chk(VAL b: box);
  BEGIN
    IF (b.imm<min.imm) OR (b.imm>max.imm) THEN error(l,range_check) END;
  END chk;
  VAR b,bl,br: box; e: ref;
BEGIN
  load_min(l^.l^.dw,min); imm?(min);
  load_max(l^.l^.dw,max); imm?(max);
  res.no:=0; res.am:=gen.am_imm; e:=l^.r;
  WHILE e#NIL DO
    IF e^.md=tbl.range THEN
      try_load(e^.l,bl,load_val);
      try_load(e^.r,br,load_val);
      IF (bl.bm=bm_imm) & (br.bm=bm_imm) THEN
        IF br.imm>=bl.imm THEN
          chk(bl); chk(br);
          res.no:=INTEGER(BITSET(res.no)+{bl.imm-min.imm..br.imm-min.imm});
        END;
      ELSE
        IF br.bm=bm_imm THEN chk(br); DEC(br.imm,min.imm); put.li(br.imm) END;
        IF bl.bm#bm_imm THEN
          gen_load(e^.l,bl,load_val); stk?(bl);
          put.lsa(-min.imm); put.li(max.imm-min.imm); put.c(mcd.chkz);
        END;
        IF br.bm#bm_imm THEN
          gen_load(e^.r,br,load_val); stk?(br);
          put.lsa(-min.imm); put.li(max.imm-min.imm); put.c(mcd.chkz);
          put.c(mcd.copt); put.c(mcd.stot);
        END;
        IF bl.bm=bm_imm THEN chk(bl); DEC(bl.imm,min.imm); put.li(bl.imm);
        ELSIF br.bm#bm_imm THEN put.c(mcd.swap);
        END;
        put.c(mcd.sub); put.c(mcd.copt);
        put.c(mcd.li0); put.c(mcd.geq);
        put.c(mcd.li1); put.c(mcd.ror);
        put.c(mcd.swap); put.c(mcd.shr);
        IF br.bm=bm_imm THEN put.li(31-br.imm)
        ELSE put.li(31); put.c(mcd.lodt); put.c(mcd.sub);
        END;
        put.c(mcd.ror);
        IF res.am=gen.am_imm THEN res.am:=gen.am_stk ELSE put.c(mcd.or) END;
      END;
    ELSE
      calc_load(e,b,load_val);
      IF b.bm=bm_imm THEN
        IF (b.imm<min.imm) OR (b.imm>max.imm) THEN error(e,range_check) END;
        res.no:=INTEGER(BITSET(res.no)+{b.imm-min.imm});
      ELSIF b.bm=bm_stk THEN
        gen_bit(min.imm,max.imm);
        IF res.am=gen.am_imm THEN res.am:=gen.am_stk ELSE put.c(mcd.or) END;
      ELSE error(e,unrealize);
      END;
    END;
    e:=e^.nxt;
  END;
  IF (res.no#0) & (res.am=gen.am_stk) THEN put.li(res.no); put.c(mcd.or) END;
END bitset_aggregate;

PROCEDURE const_aggregate(l: ref; a: ADDRESS; VAL sz: box): BOOLEAN;
  VAR
    e  : ref;
    pos: INTEGER;
    ea : gen.access;
    esz,b,min,max: box;
BEGIN
  e:=l^.r;
  IF l^.l^.md=tbl.packed_array THEN
    load_bits(l^.l^.r,esz); imm?(esz); pos:=0;
    IF esz.imm<=32 THEN
      WHILE e#NIL DO
        load_val(e,b); IF b.bm#bm_imm THEN RETURN FALSE END;
        bblt(a,pos,ADR(b.imm),0,esz.imm);
        e:=e^.nxt; INC(pos,esz.imm);
      END;
    ELSE
      WHILE e#NIL DO
        load_access(e,ea); IF ea.am#gen.am_STR THEN RETURN FALSE END;
        bblt(a,pos,ADR(scode),ea.disp,esz.imm);
        e:=e^.nxt; INC(pos,esz.imm);
      END;
    END;
    RETURN TRUE;
  ELSIF l^.l^.md=tbl.array THEN
    load_size(l^.l^.r,esz); imm?(esz); pos:=0;
    IF esz.imm=1 THEN
      WHILE e#NIL DO
        load_val(e,b); IF b.bm#bm_imm THEN RETURN FALSE END;
        a^:=b.imm; INC(a); e:=e^.nxt;
      END;
    ELSE
      WHILE e#NIL DO
        load_access(e,ea); IF ea.am#gen.am_STR THEN RETURN FALSE END;
        bblt(a,pos,ADR(scode),ea.disp,esz.imm*32);
        e:=e^.nxt; INC(pos,esz.imm*32);
      END;
    END;
    RETURN TRUE;
  ELSIF l^.l^.md=tbl.set THEN
    load_min(l^.l^.dw,min); imm?(min);
    load_max(l^.l^.dw,max); imm?(max);
    ASSERT((min.imm-max.imm+32) DIV 32<=sz.imm);
    a^:=0; move(a+1,a,sz.imm-1);
    WHILE e#NIL DO
      load_val(e,b); IF b.bm#bm_imm THEN RETURN FALSE END;
      IF (b.imm<min.imm) OR (b.imm>max.imm) THEN error(e,range_check) END;
      incl(a,b.imm-min.imm); e:=e^.nxt;
    END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END const_aggregate;

PROCEDURE gen_aggregate(l: ref; VAR res: gen.access);
  VAR
    v,e: ref;
    of,pos: INTEGER;
    a,ea: gen.access;
    sz,esz,b: box;
    t: trap;
BEGIN
  load_size(l^.l,sz);
  IF sz.bm#bm_imm THEN error(l,'aggregate size must be constant') END;
  IF sz.imm<=0 THEN
    -- пустой агрегат должен лежать по абсолютному адресу NIL
    res.am:=gen.am_abs; res.lvl:=0; res.no:=7FFFFF80h; res.disp:=0; RETURN;
  END;
  IF (sz.imm=1) & (l^.l^.md=tbl.set) THEN bitset_aggregate(l,res); RETURN END;
  save(t);
  pos:=new_str(sz.imm);
  IF const_aggregate(l,ADR(scode[pos]),sz) THEN
    res.am:=gen.am_STR; res.lvl:=0; res.no:=0; res.disp:=pos*32; RETURN;
  END;
  restore(t);
  v:=new_ref(tbl.var); v^.l:=l^.l; gen_var(v); res:=vars[v^.adr];
  e:=l^.r;
  IF l^.l^.md=tbl.packed_array THEN
    try_load(l^.l^.r,esz,load_bits); imm?(esz);
    convert_to_address(res); pos:=0;
    IF esz.imm=8 THEN
      WHILE e#NIL DO
        IF e^.nxt#NIL THEN put.c(mcd.copt) END;
        put.li(pos); INC(pos);
        gen_load(e,b,load_val);
        IF b.bm#bm_stk THEN error(l,unrealize) END;
        put.c(mcd.sxb); e:=e^.nxt;
      END;
    ELSIF esz.imm=32 THEN
      WHILE e#NIL DO
        IF e^.nxt#NIL THEN put.c(mcd.copt) END;
        put.li(pos); INC(pos);
        gen_load(e,b,load_val);
        IF b.bm#bm_stk THEN error(l,unrealize) END;
        put.c(mcd.sxw); e:=e^.nxt;
      END;
    ELSIF esz.imm<32 THEN
      WHILE e#NIL DO
        IF e^.nxt#NIL THEN put.c(mcd.copt) END;
        put.li(pos); INC(pos,esz.imm);
        gen_load(e,b,load_val);
        IF b.bm#bm_stk THEN error(l,unrealize) END;
        put.li(esz.imm); put.c(mcd.bbp); e:=e^.nxt;
      END;
    ELSE
      WHILE e#NIL DO
        IF e^.nxt#NIL THEN put.c(mcd.copt) END;
        put.li(pos); INC(pos,esz.imm);
        load_access(e,ea); convert_toT(ea);
        put.lsa(ea.disp); put.li(esz.imm); put.c(mcd.bblt);
        e:=e^.nxt;
      END;
    END;
  ELSIF l^.l^.md=tbl.array THEN
    a:=res; convert_toAGL(a);
    ASSERT(a.disp MOD 32=0);
    of:=a.disp DIV 32; pos:=0;
    WHILE e#NIL DO
      load_size(type(e),esz);
      IF esz.bm#bm_imm THEN error(e,'aggregate size must be constant') END;
      IF esz.imm=1 THEN
        CASE a.am OF
          |gen.am_G  : put.sgw1(of+pos);
          |gen.am_L  : put.slw1(of+pos);
          |gen.am_adr:
            IF (of+pos<0)OR(of+pos>0Fh) THEN put.lsa(of+pos); of:=-pos END;
            IF e^.nxt#NIL THEN put.c(mcd.copt) END;
        END;
        gen_load(e,b,load_val);
        IF b.bm#bm_stk THEN error(l,unrealize) END;
        CASE a.am OF
          |gen.am_G  : put.sgw2(of+pos);
          |gen.am_L  : put.slw2(of+pos);
          |gen.am_adr: put.ssw(of+pos);
        END;
      ELSE
        CASE a.am OF
          |gen.am_G  : put.lga(of+pos);
          |gen.am_L  : put.lla(of+pos);
          |gen.am_adr:
            put.lsa(of+pos); of:=-pos;
            IF e^.nxt#NIL THEN put.c(mcd.copt) END;
        END;
        gen_load(e,b,load_adr);
        IF b.bm#bm_stk THEN error(l,unrealize) END;
        put.li(esz.imm); put.c(mcd.move);
      END;
      e:=e^.nxt; INC(pos,esz.imm);
    END;
    IF pos>sz.imm THEN error(l,assert) END;
  ELSIF l^.l^.md=tbl.set THEN
    convert_to_address(res);
HALT(1);
  ELSE error(l,unrealize);
  END;
END gen_aggregate;

PROCEDURE dynarr?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (
    (l^.md=tbl.dynarr) OR
    (l^.md=tbl.packed_dynarr)
  );
END dynarr?;

PROCEDURE access_proc(v: ref; VAR a: gen.access);
BEGIN
  WITH prcs[v^.adr] DO
    IF (lvl>1) OR (disp<0) OR (disp>0FFh) THEN error(v,unrealize) END;
    IF (mod<0) OR (mod>0FFh) THEN error(v,'too many modules in import') END;
    put.c(mcd.lpc); put.b(mod); put.b(disp);
  END;
  a.am:=gen.am_stk;
END access_proc;

PROCEDURE access_usage(l: ref; VAR a: gen.access);
  VAR v: ref;
BEGIN
  v:=l^.r;
  IF v=NIL THEN error(l,assert) END;
  IF v^.md=tbl.var THEN a:=vars[v^.adr];
  ELSIF v^.md=tbl.const THEN a:=vars[v^.adr];
  ELSIF v^.md=tbl.procedure THEN access_proc(v,a);
  ELSE error(l,assert);
  END;
END access_usage;

PROCEDURE access_index(l: ref; VAR a: gen.access);
  VAR v,arr: ref; check: BOOLEAN;
  PROCEDURE array_const_index(packed: BOOLEAN): BOOLEAN;
    VAR fr,sz,ix: box; t: trap;
  BEGIN
    save(t);
    load_val(l^.r,ix);
    IF ix.bm=bm_imm THEN
      load_min(arr^.l,fr);
      IF fr.bm=bm_imm THEN
        IF packed THEN load_bits(v,sz);
        ELSE load_size(v,sz); sz.imm:=sz.imm*32
        END;
        IF sz.bm=bm_imm THEN
          INC(a.disp,(ix.imm-fr.imm)*sz.imm); RETURN TRUE
        END;
      END;
    END;
    restore(t); RETURN FALSE;
  END array_const_index;
  PROCEDURE array_index(packed: BOOLEAN);
    VAR ix,fr,to,sz: box;
  BEGIN
    IF array_const_index(packed) THEN RETURN END;
    convert_toABTmod32(a);
    IF check THEN gen_load(l^.r^.r,ix,load_val);
    ELSE gen_load(l^.r,ix,load_val);
    END;
    stk?(ix);
    calc_load(arr^.l,fr,load_min);
    IF fr.bm=bm_imm THEN
      put.lsa(-fr.imm);
      IF check THEN
        calc_load(arr^.l,to,load_max);
        IF to.bm=bm_stk THEN put.lsa(-fr.imm);
        ELSIF to.bm=bm_imm THEN put.li(to.imm-fr.imm);
        ELSE error(l,unrealize)
        END;
        put.c(mcd.chkz);
      END;
    ELSIF fr.bm=bm_stk THEN
      IF check THEN
        gen_load(arr^.l,to,load_max); stk?(to); put.c(mcd.chk);
        gen_load(arr^.l,fr,load_min);
      END;
      put.c(mcd.sub);
    ELSE error(l,unrealize);
    END;
    IF packed THEN calc_load(v,sz,load_bits)
    ELSE calc_load(v,sz,load_size)
    END;
    IF sz.bm=bm_imm THEN
      IF NOT packed THEN
        CASE a.am OF
          |gen.am_adr:
            IF sz.imm=1 THEN a.am:=gen.am_xw
            ELSE put.li(sz.imm); put.c(mcd.lxa)
            END;
          |gen.am_xb : put.li(sz.imm*4); put.c(mcd.lxa);
          |gen.am_xt : put.li(sz.imm*32); put.c(mcd.lxa);
        END;
      ELSE
        CASE a.am OF
          |gen.am_adr:
            IF sz.imm=32 THEN a.am:=gen.am_xw
            ELSIF sz.imm=8 THEN a.am:=gen.am_xb
            ELSIF sz.imm MOD 32=0 THEN put.li(sz.imm DIV 32); put.c(mcd.lxa);
            ELSIF sz.imm MOD 8=0 THEN put.cmul(sz.imm DIV 8); a.am:=gen.am_xb;
            ELSE put.li(sz.imm); put.c(mcd.mul); a.am:=gen.am_xt;
            END;
          |gen.am_xb :
            IF sz.imm = 8  THEN put.c(mcd.add);
            ELSIF sz.imm MOD 8 = 0 THEN put.li(sz.imm DIV 8); put.c(mcd.lxa);
            ELSE
              put.li(3); put.c(mcd.shl);
              put.li(sz.imm); put.c(mcd.lxa); a.am:=gen.am_xt;
            END;
          |gen.am_xt :
            IF sz.imm=1 THEN put.c(mcd.add);
            ELSE put.li(sz.imm); put.c(mcd.lxa);
            END;
        END;
      END;
    ELSIF sz.bm=bm_stk THEN
      IF NOT packed THEN
        CASE a.am OF
          |gen.am_adr: put.c(mcd.lxa);
          |gen.am_xb : put.li(2); put.c(mcd.shl); put.c(mcd.lxa);
          |gen.am_xt : put.li(5); put.c(mcd.shl); put.c(mcd.lxa);
        END;
      ELSE
        CASE a.am OF
          |gen.am_adr: put.c(mcd.mul); a.am:=gen.am_xt;
          |gen.am_xb :
            put.c(mcd.stot); put.li(3); put.c(mcd.shl);
            put.c(mcd.lodt); put.c(mcd.lxa); a.am:=gen.am_xt;
          |gen.am_xt : put.c(mcd.lxa);
        END;
      END;
    ELSE error(l,unrealize)
    END;
  END array_index;
  PROCEDURE dynarr_index(packed: BOOLEAN);
    VAR ix,sz: box; pdx: BOOLEAN;
  BEGIN
    convert_toABTmod32(a);
    CASE a.am OF
      |gen.am_adr:
        IF a.disp#0 THEN
          put.c(mcd.copt); put.c(mcd.stot); put.c(mcd.stot);
        END;
      |gen.am_xb,gen.am_xt:
        put.c(mcd.copt); put.c(mcd.stot); put.c(mcd.swap);
        put.c(mcd.copt); put.c(mcd.stot); put.c(mcd.swap);
        put.c(mcd.stot); put.c(mcd.stot);
    END;
    gen_load(l^.r,ix,load_val);
    IF ix.bm#bm_stk THEN error(l,unrealize) END;
    pdx:=FALSE;
    CASE a.am OF
      |gen.am_xb,gen.am_xt:
        put.c(mcd.lodt); put.c(mcd.lodt);
        put.load(a.am,0,0,a.disp+32,32);
      |gen.am_adr:
        IF a.disp=0 THEN put.c(mcd.pdx); pdx:=TRUE;
        ELSE put.c(mcd.lodt); put.load(a.am,0,0,a.disp+32,32);
        END;
    END;
    IF NOT pdx THEN
      put.c(mcd.chkz);
      CASE a.am OF
        |gen.am_xb,gen.am_xt:
          put.c(mcd.lodt); put.c(mcd.lodt);
          put.load(a.am,0,0,a.disp,32);
        |gen.am_adr:
          put.c(mcd.lodt);
          put.load(a.am,0,0,a.disp,32);
      END;
      IF tbl.chk_nil THEN put.c(mcd.chknil) END;
      put.c(mcd.swap);
    END;
    IF packed THEN calc_load(v,sz,load_bits);
    ELSE calc_load(v,sz,load_size);
    END;
    a.disp:=0;
    IF sz.bm=bm_imm THEN
      IF packed THEN
        IF sz.imm=32 THEN a.am:=gen.am_xw;
        ELSIF sz.imm=8 THEN a.am:=gen.am_xb;
        ELSIF sz.imm=1 THEN a.am:=gen.am_xt;
        ELSIF sz.imm MOD 32 = 0 THEN
          put.li(sz.imm DIV 32); put.c(mcd.lxa); a.am:=gen.am_adr;
        ELSIF sz.imm MOD 8 = 0 THEN
          put.li(sz.imm DIV 8); put.c(mcd.lxa); a.am:=gen.am_xb;
        ELSE put.li(sz.imm); put.c(mcd.mul); a.am:=gen.am_xt;
        END;
      ELSE
        IF sz.imm=1 THEN a.am:=gen.am_xw
        ELSE put.li(sz.imm); put.c(mcd.lxa); a.am:=gen.am_adr;
        END;
      END;
    ELSE
      stk?(sz);
      IF packed THEN put.c(mcd.mul); a.am:=gen.am_xt;
      ELSE put.c(mcd.lxa); a.am:=gen.am_adr;
      END;
    END;
  END dynarr_index;
  PROCEDURE array_of_index(packed: BOOLEAN);
    VAR ix,sz: box;
  BEGIN
    IF a.am#gen.am_L THEN error(l,assert) END;
    put.load(a.am,a.lvl,a.no,a.disp,32);
    IF check THEN gen_load(l^.r^.r,ix,load_val);
    ELSE gen_load(l^.r,ix,load_val);
    END;
    IF ix.bm#bm_stk THEN error(l,unrealize) END;
    IF check THEN
      put.load(a.am,a.lvl,a.no,a.disp-32,32);
      put.c(mcd.chkz);
    END;
    IF packed THEN calc_load(v,sz,load_bits);
    ELSE calc_load(v,sz,load_size);
    END;
    a.disp:=0;
    IF sz.bm=bm_imm THEN
      IF packed THEN
        IF sz.imm=32 THEN a.am:=gen.am_xw;
        ELSIF sz.imm=8 THEN a.am:=gen.am_xb;
        ELSIF sz.imm=1 THEN a.am:=gen.am_xt;
        ELSIF sz.imm MOD 32 =0 THEN
          put.li(sz.imm DIV 32); put.c(mcd.lxa); a.am:=gen.am_adr;
        ELSE put.li(sz.imm); put.c(mcd.mul); a.am:=gen.am_xt;
        END;
      ELSE
        IF sz.imm=1 THEN a.am:=gen.am_xw
        ELSE put.li(sz.imm); put.c(mcd.lxa); a.am:=gen.am_adr;
        END;
      END;
    ELSIF sz.bm=bm_stk THEN
      IF packed THEN put.c(mcd.mul); a.am:=gen.am_xt;
      ELSE put.c(mcd.lxa); a.am:=gen.am_adr;
      END;
    ELSE error(l,unrealize)
    END;
  END array_of_index;
BEGIN
  arr:=type(l^.l); v:=type(l);
  IF (arr=NIL) OR (v=NIL) THEN error(l,assert) END;
  load_access(l^.l,a);
  check:=(l^.r#NIL) & (l^.r^.md=tbl.range_check);
  IF arr^.md=tbl.array THEN array_index(FALSE);
  ELSIF arr^.md=tbl.dynarr THEN dynarr_index(FALSE);
  ELSIF arr^.md=tbl.array_of THEN array_of_index(FALSE);
  ELSIF arr^.md=tbl.packed_array THEN array_index(TRUE);
  ELSIF arr^.md=tbl.packed_dynarr THEN dynarr_index(TRUE);
  ELSIF arr^.md=tbl.packed_array_of THEN array_of_index(TRUE);
  ELSE error(l^.l,'must be array');
  END;
END access_index;

PROCEDURE access_field(l: ref; VAR a: gen.access);
  VAR v: ref; sz: box;
BEGIN
  load_access(l^.l,a); v:=l^.r;
  IF (v=NIL) OR (v^.md#tbl.var) THEN error(l,assert) END;
  IF vars[v^.adr].am#gen.am_adr THEN error(v,assert) END;
  INC(a.disp,vars[v^.adr].disp);
END access_field;

PROCEDURE access_deref(l: ref; VAR a: gen.access);
  VAR b: box;
BEGIN
  IF dynarr?(l^.l^.dw) THEN
    load_access(l^.l,a);
  ELSE
    gen_load(l^.l,b,load_val);
    IF b.bm#bm_stk THEN error(l,unrealize) END;
    a.am:=gen.am_adr; a.disp:=0;
    IF tbl.chk_nil THEN put.c(mcd.chknil) END;
  END;
END access_deref;

PROCEDURE access_string(l: ref; VAR a: gen.access);
  VAR p: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR; i,n: INTEGER;
BEGIN
  i:=(HIGH(l^.str)+4) DIV 4;
  n:=new_str(i); p:=ADR(scode[n]); i:=0;
  WHILE i<=HIGH(l^.str) DO p^[i]:=l^.str[i]; INC(i) END;
  WHILE (i MOD 4)#0 DO p^[i]:=0c; INC(i) END;
  a.am:=gen.am_STR;
  a.lvl:=0;
  a.disp:=n*32;
END access_string;

PROCEDURE access_adr(l: ref; VAR a: gen.access);
BEGIN
  IF NOT dynarr?(l^.r^.dw) THEN error(l,'must be dynarr') END;
  load_access(l^.r,a);
END access_adr;

PROCEDURE access_high(l: ref; VAR a: gen.access);
BEGIN
  IF NOT dynarr?(l^.r^.dw) THEN error(l,'must be dynarr') END;
  load_access(l^.r,a); INC(a.disp,32);
END access_high;

PROCEDURE load_access(l: ref; VAR a: gen.access);
  VAR v: ref; b: box;
BEGIN
  CASE l^.md OF
    |tbl.number   : a.am:=gen.am_imm; a.no:=l^.val;
    |tbl.string   : access_string(l,a);
    |tbl.usage    : access_usage(l,a);
    |tbl.aggregate: gen_aggregate(l,a);
    |tbl.index    : access_index(l,a);
    |tbl.deref    : access_deref(l,a);
    |tbl.field    : access_field(l,a);
    |tbl.var      : a:=vars[l^.adr];
    |tbl.adr      : access_adr(l,a);
    |tbl.high     : access_high(l,a);
  ELSE
    error(l,'access loading failed');
  END;
END load_access;

END pcGenDesig.
