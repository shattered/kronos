IMPLEMENTATION MODULE pcGenExpr; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT mcd : defCodes;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT put : pcGenCmd;
IMPORT des : pcGenDesig;

IMPORT gen : krSym;

FROM pcTab      IMPORT  ref;
FROM krSym IMPORT access, access_mode;

WITH STORAGE : mem;

CONST
  return_without_res= 46h;
  stk_ovr_trap      = 4Ch;
  case_else         = 45h;
  halt_trap         = 47h;
  crash_trap        = 4Dh;
  abort_trap        = 50h;
  assert_trap       = 48h;
  range_trap        = 4Ah;
  size_limit        =  3;
  prm_limit         =  5;
  stk_limit         =  8;
  assert    = 'undefined error, compilation fault';
  unrealize = 'unrealized, compilation fault';
  type_check= 'incompatible types';

VAR
  block         : node;

  glo_alloc_cnt : INTEGER;

PROCEDURE move(a,b: ADDRESS; s: INTEGER); CODE mcd.move END move;
PROCEDURE bblt(a: ADDRESS; a1: INTEGER;
               b: ADDRESS; b1: INTEGER; sz: INTEGER); CODE mcd.bblt END bblt;

PROCEDURE save(VAR r: trap);
BEGIN
  r.cnt :=put.cnt;
  r.scnt:=des.scnt;
  r.vcnt:=des.vars_no;
  r.pcnt:=des.prcs_no;
  r.mcnt:=des.mdls_no;
  r.rcnt:=des.rngs_no;
  r.mgcnt:=des.mg_no;
  r.blk :=block;
  r.stk :=put.stk;
  r.mstk:=put.stk_max;
END save;

PROCEDURE restore(VAL r: trap);
BEGIN
  put.cnt:=r.cnt;
  des.scnt:=r.scnt;
  des.vars_no:=r.vcnt;
  des.prcs_no:=r.pcnt;
  des.mdls_no:=r.mcnt;
  des.rngs_no:=r.rcnt;
  des.mg_no:=r.mgcnt;
--ASSERT(block=r.blk);
  block:=r.blk;
  block^.true :=NIL;
  block^.false:=NIL;
  block^.md   :=nm_goto;
  NEW(block^.alts);
  put.stk:=r.stk;
  put.stk_max:=r.mstk;
END restore;

PROCEDURE error(l: ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l=NIL) OR (l^.md=tbl.procedure) OR (l^.md=tbl.number) THEN
    tbl.error(stat_pos DIV 1000h,stat_pos MOD 1000h,s,x);
  ELSE
    tbl.error(l^.val DIV 1000h,l^.val MOD 1000h,s,x);
  END;
  HALT(1);
END error;

TYPE
  process  = POINTER TO RECORD G,L,PC,M,S,H,T: INTEGER END;
  set_ptr  = POINTER TO BITSET;

PROCEDURE active(): process; CODE mcd.activ END active;
PROCEDURE tags(): BITSET;    CODE mcd.llw 2 END tags;
PROCEDURE getm(): BITSET;    CODE mcd.getm  END getm;
PROCEDURE setm(m: BITSET);   CODE mcd.setm  END setm;
PROCEDURE savem;             CODE mcd.getm mcd.slw 3 END savem;

PROCEDURE mask_overflow;
  VAR p: process;
BEGIN
  setm(getm()-{31});
  IF 30 IN tags() THEN savem END;
  p:=active(); p^.T:=0;
END mask_overflow;

PROCEDURE check_overflow;
  VAR p: process; s: set_ptr; T: INTEGER;
BEGIN
  p:=active(); T:=p^.T;
  setm(getm()+{31});
  IF 30 IN tags() THEN savem END;
  IF T#0 THEN error(NIL,'numeric overflow (%$2hh)',T) END;
END check_overflow;

PROCEDURE imm?(VAL b: box);
BEGIN
  IF b.bm#bm_imm THEN error(NIL,'must be constant') END;
END imm?;

PROCEDURE stk?(VAL b: box);
BEGIN
  IF b.bm#bm_stk THEN error(NIL,'can not load value on stack') END;
END stk?;

PROCEDURE new(): node;
  VAR r: node;
BEGIN
  mem.ALLOCATE(r,SIZE(r^));
  r^.true :=NIL;
  r^.false:=NIL;
  r^.md   :=nm_goto;
  r^.spos :=0;
  r^.slen :=0;
  NEW(r^.alts);
  r^.pos  :=0;
  r^.cmp  :=-1;
  r^.len  :=0;
  r^.adr  :=0;
  r^.cnt  :=0;
  r^.res  :=0;
  r^.next :=NIL;
  r^.mark :=FALSE;
  RETURN r;
END new;

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
  r^.val:=stat_pos;
  RETURN r;
END new_ref;

PROCEDURE finish;
BEGIN
  IF block#NIL THEN block^.len:=put.cnt-block^.pos END;
END finish;

PROCEDURE start(n: node);
BEGIN
  finish; block:=n; block^.pos:=put.cnt;
END start;

PROCEDURE calc_load(l: ref; VAR b: box; p: try_load_proc);
  VAR t: trap;
BEGIN
  IF put.stk_max>stk_limit THEN p(l,b); RETURN END;
  save(t); p(l,b);
  IF (put.stk_max>stk_limit) &
     (t.stk<stk_limit) & (t.stk>0) & (t.stk=put.stk-1) THEN
    restore(t); put.c(mcd.store); put.stk:=0; put.stk_max:=0; p(l,b);
    ASSERT(put.stk=1);
    put.c(mcd.lodfv); put.stk:=t.stk+1;
    IF put.stk_max<put.stk THEN put.stk_max:=put.stk END;
  END;
END calc_load;

PROCEDURE gen_load(l: ref; VAR b: box; p: try_load_proc);
BEGIN
  calc_load(l,b,p);
  IF b.bm=bm_imm THEN put.li(b.imm); b.bm:=bm_stk END;
END gen_load;

PROCEDURE try_load(l: ref; VAR b: box; p: try_load_proc);
  VAR t: trap;
BEGIN
  save(t); calc_load(l,b,p); restore(t);
END try_load;

PROCEDURE profile_pno(l: ref): INTEGER;
  VAR n: INTEGER; v,t: ref;
BEGIN
  IF l=NIL THEN RETURN 0 END;
  ASSERT(l^.md=tbl.profile);
  v:=l^.dw; n:=0;
  WHILE v#NIL DO
    IF v^.md=tbl.var THEN
      INC(n); t:=v^.l;
      WHILE (t^.md=tbl.val_prm) OR (t^.md=tbl.var_prm) DO t:=t^.dw END;
      IF (t^.md=tbl.array_of) OR (t^.md=tbl.packed_array_of) THEN INC(n) END;
    END;
    v:=v^.nxt
  END;
  RETURN n;
END profile_pno;

PROCEDURE profile_loc(l: ref): INTEGER;
  VAR n: INTEGER; v,t,i: ref;
BEGIN
  IF l=NIL THEN RETURN 0 END;
  ASSERT(l^.md=tbl.profile);
  i:=l^.dw; v:=NIL;
  WHILE i#NIL DO
    IF i^.md=tbl.var THEN i^.r:=v; v:=i END; i:=i^.nxt;
  END;
  n:=0;
  WHILE v#NIL DO
    IF v^.md=tbl.var THEN
      INC(n); t:=v^.l;
      LOOP
        IF t=NIL THEN error(t,'param type = NIL') END;
        IF (t^.md=tbl.val_prm) OR (t^.md=tbl.var_prm) THEN t:=t^.dw
        ELSE EXIT
        END;
      END;
      IF (t^.md=tbl.array_of) OR (t^.md=tbl.packed_array_of) THEN INC(n) END;
      IF n=prm_limit THEN RETURN n END;
      IF n>prm_limit THEN RETURN n-2 END;
    END;
    v:=v^.r;
  END;
  RETURN n;
END profile_loc;

PROCEDURE gen_cl(l: ref);
  VAR res: BOOLEAN;
BEGIN
  ASSERT(l^.md=tbl.procedure);
  res:=(l^.l#NIL) & (l^.l^.l#NIL);
  WITH des.prcs[l^.adr] DO
    IF (disp<0) OR (disp>=100h) THEN error(l,'too many procedures') END;
    IF (mod<0) OR (mod>=100h) THEN error(l,'too many modules') END;
    IF mod#0 THEN put.c(mcd.cx); put.b(mod); put.b(disp);
    ELSIF (lvl=0) OR (lvl=1) OR (lvl=level+1) THEN
      IF disp<10h THEN put.c(mcd.cl0+disp);
      ELSE put.c(mcd.cl); put.b(disp);
      END;
    ELSIF lvl<=level THEN put.gb(level-lvl+1); put.c(mcd.ci); put.b(disp);
    ELSE error(l,assert);
    END;
  END;
  IF res THEN put.stk:=1 ELSE put.stk:=0 END;
END gen_cl;

PROCEDURE alloc_var(sz: INTEGER): INTEGER;
  VAR n: INTEGER;
BEGIN
  IF level=0 THEN WITH des.cu DO n:=glo_sz; INC(glo_sz,sz) END;
  ELSE WITH des.prcs[proc^.l^.adr] DO n:=loc_sz; INC(loc_sz,sz) END;
  END;
  RETURN n;
END alloc_var;

PROCEDURE type(l: ref): ref;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.high,tbl.adr,tbl.size,tbl.bytes,tbl.min,tbl.max,tbl.aggregate,
     tbl.equal,tbl.inequality,tbl.less,tbl.less_equal,tbl.call,tbl.usage,
     tbl.greater,tbl.greater_equal,tbl.in,tbl.plus,tbl.minus,tbl.number,
     tbl.star,tbl.slash,tbl.div,tbl.mod,tbl.rem,tbl.rol,tbl.ror,tbl.len,
     tbl.not,tbl.deref,tbl.trunc,tbl.float,tbl.index,tbl.field,
     tbl.size_check,tbl.range_check,tbl.worder: RETURN l^.dw;
    |tbl.var:
      l:=l^.l;
      LOOP
        IF l^.md=tbl.val_prm THEN l:=l^.dw
        ELSIF l^.md=tbl.var_prm THEN l:=l^.dw
        ELSE RETURN l;
        END
      END;
  ELSE error(l,assert);
  END;
END type;

PROCEDURE vm(l: ref): val_mode;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  l:=l^.dw;
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.set: RETURN vm_set;
    |tbl.boolean: RETURN vm_boolean;
    |tbl.real: RETURN vm_real;
    |tbl.integer,tbl.char,tbl.range,tbl.pointer,
     tbl.subtype,tbl.enumeration: RETURN vm_integer;
    |tbl.packed_array,tbl.packed_dynarr,tbl.packed_array_of:
      IF (l^.r#NIL) & (l^.r^.md=tbl.char) THEN RETURN vm_string
      ELSE RETURN vm_undef
      END;
  ELSE RETURN vm_undef;
  END;
END vm;

PROCEDURE dynarr?(l: ref): BOOLEAN;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  RETURN (l^.md=tbl.dynarr) OR (l^.md=tbl.packed_dynarr)
END dynarr?;

PROCEDURE array_of?(l: ref): BOOLEAN;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  RETURN (l^.md=tbl.array_of) OR (l^.md=tbl.packed_array_of)
END array_of?;

PROCEDURE pointer?(l: ref): BOOLEAN;
BEGIN
  RETURN (l#NIL) & (l^.md=tbl.pointer);
END pointer?;

PROCEDURE load_val_gen(VAL a: access; VAR b: box);
BEGIN
  IF a.am=am_imm THEN b.bm:=bm_imm; b.imm:=a.no
  ELSIF (a.am=am_STR) & (a.lvl=0) THEN
    b.bm:=bm_imm; bblt(ADR(b.imm),0,ADR(des.scode),a.disp,32);
  ELSE put.load(a.am,a.lvl,a.no,a.disp,32); b.bm:=bm_stk
  END;
END load_val_gen;

PROCEDURE load_min(l: ref; VAR b: box);
BEGIN
  b.bm:=bm_imm;
  IF l=NIL THEN b.imm:=MIN(INTEGER); RETURN END;
  CASE l^.md OF
    |tbl.usage      : load_min(l^.dw,b);
    |tbl.aggregate  : load_min(l^.dw,b);
    |tbl.boolean    : b.imm:=0;
    |tbl.enumeration: b.imm:=0;
    |tbl.integer    : b.imm:=MIN(INTEGER);
    |tbl.range      : load_val_gen(des.rngs[l^.adr].l,b);
    |tbl.subtype    : load_val_gen(des.rngs[l^.adr].l,b);
    |tbl.char       : b.imm:=0;
    |tbl.val_prm    : load_min(l^.dw,b);
    |tbl.var_prm    : load_min(l^.dw,b);
  ELSE
    error(l,'must be scalar or set');
  END;
END load_min;

PROCEDURE load_max(l: ref; VAR b: box);
  VAR i: ref;
BEGIN
  b.bm:=bm_imm;
  IF l=NIL THEN b.imm:=MAX(INTEGER); RETURN END;
  CASE l^.md OF
    |tbl.usage      : load_max(l^.dw,b);
    |tbl.aggregate  : load_max(l^.dw,b);
    |tbl.boolean    : b.imm:=1;
    |tbl.enumeration:
      b.imm:=-1; i:=l^.nxt;
      WHILE (i#NIL) & (i^.md=tbl.const) & (i^.l=l) DO
        INC(b.imm); i:=i^.nxt
      END;
    |tbl.integer    : b.imm:=MAX(INTEGER);
    |tbl.range      : load_val_gen(des.rngs[l^.adr].r,b);
    |tbl.subtype    : load_val_gen(des.rngs[l^.adr].r,b);
    |tbl.char       : b.imm:=255;
    |tbl.val_prm    : load_max(l^.dw,b);
    |tbl.var_prm    : load_max(l^.dw,b);
  ELSE
    error(l,'must be scalar or set');
  END;
END load_max;

PROCEDURE load_low(l: ref; VAR b: box);
BEGIN
  IF l=NIL THEN error(l,'must be array') END;
  CASE l^.md OF
    |tbl.array,tbl.packed_array: load_min(l^.l,b);
    |tbl.dynarr,tbl.packed_dynarr,
     tbl.array_of,tbl.packed_array_of: b.imm:=0; b.bm:=bm_imm;
    |tbl.usage,tbl.number,tbl.val_prm,tbl.var_prm,tbl.aggregate,
     tbl.index,tbl.field,tbl.deref: load_low(l^.dw,b);
  ELSE
    error(l,'must be array');
  END;
END load_low;

PROCEDURE load_high(l: ref; VAR b: box);
  VAR a: access;
BEGIN
  IF l=NIL THEN error(l,'must be array') END;
  CASE l^.md OF
    |tbl.array,tbl.packed_array: load_max(l^.l,b);
    |tbl.number,tbl.aggregate,tbl.val_prm,tbl.var_prm: load_high(l^.dw,b);
    |tbl.string: b.imm:=HIGH(l^.str); b.bm:=bm_imm;
    |tbl.index,tbl.field,tbl.deref,tbl.usage:
      IF dynarr?(l^.dw) THEN
        b.bm:=bm_stk; des.load_access(l,a);
        put.load(a.am,a.lvl,a.no,a.disp+32,32);
      ELSIF array_of?(l^.dw) THEN
        b.bm:=bm_stk; des.load_access(l,a);
        put.load(a.am,a.lvl,a.no,a.disp-32,32);
      ELSE
        load_high(l^.dw,b);
      END;
  ELSE
    error(l,'must be array');
  END;
END load_high;

PROCEDURE load_len(l: ref; VAR b: box);
  VAR i: ref; a: access; m: box;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.char   : b.bm:=bm_imm; b.imm:=256;
    |tbl.boolean: b.bm:=bm_imm; b.imm:=2;
    |tbl.integer: error(l,'numeric overflow');
    |tbl.enumeration:
      b.bm:=bm_imm; b.imm:=-1; i:=l^.nxt;
      WHILE (i#NIL) & (i^.md=tbl.const) & (i^.l=l) DO
        INC(b.imm); i:=i^.nxt
      END;
    |tbl.array,tbl.packed_array: load_len(l^.l,b);
    |tbl.index,tbl.field,tbl.deref,tbl.usage:
      IF dynarr?(l^.dw) THEN
        b.bm:=bm_stk; des.load_access(l,a);
        put.load(a.am,a.lvl,a.no,a.disp+32,32); put.lsa(1);
      ELSIF array_of?(l^.dw) THEN
        b.bm:=bm_stk; des.load_access(l,a);
        put.load(a.am,a.lvl,a.no,a.disp-32,32); put.lsa(1);
      ELSE load_len(l^.dw,b);
      END;
    |tbl.range,tbl.subtype:
      WITH des.rngs[l^.adr] DO
        load_val_gen(r,b);
        IF b.bm=bm_imm THEN
          IF l.am=am_imm THEN
            mask_overflow; b.imm:=r.no-l.no+1; check_overflow;
          ELSE
            put.li(b.imm+1); put.load(l.am,l.lvl,l.no,l.disp,32);
            put.c(mcd.sub); b.bm:=bm_stk;
          END;
        ELSE
          IF l.am=am_imm THEN put.lsa(1-l.no);
          ELSE put.load(l.am,l.lvl,l.no,l.disp,32); put.c(mcd.sub); put.lsa(1);
          END;
        END;
      END;
  ELSE error(l,'illegal operand for LEN');
  END;
END load_len;

PROCEDURE cmul_round32(add,mul: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF mul<0 THEN put.c(mcd.neg); mul:=ABS(mul); add:=-add END;
  i:=5;
  WHILE (mul>0) & NOT ODD(mul) DO mul:=mul DIV 2; DEC(i) END;
  IF mul=0 THEN put.c(mcd.drop); put.c(mcd.li0); RETURN END;
  put.cmul(mul); add:=add*mul;
  IF i>0 THEN put.lsa(INTEGER({i})-1+add); put.li(i); put.c(mcd.shr);
  ELSIF i<0 THEN put.lsa(add); put.li(-i); put.c(mcd.shl);
  END;
END cmul_round32;

PROCEDURE load_size(l: ref; VAR b: box);
  VAR i: ref; sz: box;
BEGIN
  b.bm:=bm_imm;
  IF l=NIL THEN b.imm:=1; RETURN END;
  CASE l^.md OF
    |tbl.enumeration,tbl.integer,tbl.real,tbl.pointer,
     tbl.profile,tbl.char,tbl.range,tbl.subtype,tbl.boolean,
     tbl.array_of,tbl.packed_array_of: b.imm:=1;
    |tbl.usage,tbl.aggregate,tbl.number: load_size(l^.dw,b);
    |tbl.array:
      load_len(l^.l,b); calc_load(l^.r,sz,load_size);
      IF b.bm=bm_imm THEN
        IF sz.bm=bm_imm THEN
          mask_overflow; b.imm:=b.imm*sz.imm; check_overflow;
        ELSIF sz.bm=bm_stk THEN put.cmul(sz.imm); b.bm:=bm_stk;
        ELSE error(l,unrealize);
        END;
      ELSIF b.bm=bm_stk THEN
        IF sz.bm=bm_imm THEN put.cmul(sz.imm);
        ELSIF sz.bm=bm_stk THEN put.c(mcd.mul);
        ELSE error(l,unrealize);
        END;
      ELSE error(l,unrealize);
      END;
    |tbl.packed_array,tbl.packed_record,tbl.record,tbl.set:
      load_bits(l,b);
      IF b.bm=bm_imm THEN b.imm:=(b.imm+31) DIV 32
      ELSIF b.bm=bm_stk THEN put.lsa(31); put.li(5); put.c(mcd.shr);
      END;
    |tbl.dynarr,tbl.dynarr_desc,tbl.packed_dynarr: b.imm:=2;
    |tbl.val_prm,tbl.var_prm,tbl.index,tbl.field,
     tbl.deref,tbl.high,tbl.adr: load_size(l^.dw,b);
    |tbl.string: b.bm:=bm_imm; b.imm:=(HIGH(l^.str)+4) DIV 4;
  ELSE
    error(l,assert);
  END;
END load_size;

PROCEDURE load_32size(l: ref; VAR b: box);
BEGIN
  load_size(l,b);
  IF b.bm=bm_imm THEN b.imm:=b.imm*32
  ELSIF b.bm=bm_stk THEN put.li(5); put.c(mcd.shl);
  END;
END load_32size;

PROCEDURE load_record_bits(l: ref; VAR b: box); FORWARD;

PROCEDURE load_bits(l: ref; VAR b: box);
  VAR i: INTEGER; v: ref; sz: box;
BEGIN
  b.bm:=bm_imm;
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.enumeration:
      load_max(l,sz); imm?(sz); b.imm:=1; i:=2;
      WHILE sz.imm>=i DO INC(b.imm); i:=i*2 END;
    |tbl.integer,tbl.real,tbl.pointer,tbl.profile,
     tbl.array_of,tbl.packed_array_of: b.imm:=32;
    |tbl.char   : b.imm:=8;
    |tbl.subtype: load_bits(l^.dw,b);
    |tbl.range  :
      WITH des.rngs[l^.adr] DO
        IF (l.am#am_imm) OR (r.am#am_imm) OR
           (l.no<0) OR (r.no<0) THEN b.imm:=32
        ELSE
          b.imm:=1; i:=2;
          WHILE r.no>=i DO INC(b.imm); i:=i*2 END;
        END;
      END;
    |tbl.boolean: b.imm:=1;
    |tbl.usage,tbl.aggregate,tbl.number: load_32size(l,b);
    |tbl.set          : load_len(l^.dw,b);
    |tbl.array        : load_32size(l,b);
    |tbl.packed_array :
      load_len(l^.l,b); calc_load(l^.r,sz,load_bits);
      IF b.bm=bm_imm THEN
        IF sz.bm=bm_imm THEN
          mask_overflow; b.imm:=b.imm*sz.imm; check_overflow;
        ELSIF sz.bm=bm_stk THEN put.cmul(sz.imm); b.bm:=bm_stk;
        ELSE error(l,unrealize);
        END;
      ELSIF b.bm=bm_stk THEN
        IF sz.bm=bm_imm THEN put.cmul(sz.imm);
        ELSIF sz.bm=bm_stk THEN put.c(mcd.mul);
        ELSE error(l,unrealize);
        END;
      ELSE error(l,unrealize);
      END;
    |tbl.dynarr,tbl.dynarr_desc,tbl.packed_dynarr: b.imm:=64;
    |tbl.record,tbl.packed_record: load_record_bits(l,b);
    |tbl.val_prm,tbl.var_prm     : load_32size(l^.dw,b);
    |tbl.field  :
      IF l^.l^.dw^.md=tbl.packed_record THEN load_bits(l^.dw,b);
      ELSE load_32size(l^.dw,b);
      END;
    |tbl.index  :
      IF l^.l^.dw^.md=tbl.packed_array THEN load_bits(l^.dw,b);
      ELSIF l^.l^.dw^.md=tbl.packed_dynarr THEN load_bits(l^.dw,b);
      ELSIF l^.l^.dw^.md=tbl.packed_array_of THEN load_bits(l^.dw,b);
      ELSE load_32size(l^.dw,b);
      END;
    |tbl.deref,tbl.high,tbl.adr,tbl.string: load_32size(l^.dw,b);
  ELSE
    error(l,assert);
  END;
END load_bits;

PROCEDURE load_user_size(l: ref; VAR b: box); FORWARD;

PROCEDURE load_user_bits(l: ref; VAR b: box);
  VAR sz: box;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.usage,tbl.index,tbl.field,tbl.deref:
      IF (l^.dw^.md=tbl.packed_dynarr) OR (l^.dw^.md=tbl.packed_array_of) THEN
        gen_load(l,b,load_high);
        IF b.bm=bm_stk THEN
          put.lsa(1);
          calc_load(l^.dw^.r,sz,load_bits);
          IF sz.bm=bm_imm THEN put.cmul(sz.imm);
          ELSIF sz.bm=bm_stk THEN put.c(mcd.mul);
          ELSE b.bm:=sz.bm;
          END;
        END;
      ELSIF (l^.dw^.md=tbl.dynarr) OR (l^.dw^.md=tbl.array_of) THEN
        load_user_size(l,b);
        IF b.bm=bm_imm THEN b.imm:=b.imm*32
        ELSIF b.bm=bm_stk THEN put.li(5); put.c(mcd.shl);
        END;
      ELSE load_bits(l,b);
      END;
  ELSE load_bits(l,b);
  END;
END load_user_bits;

PROCEDURE load_user_base_high(l: ref; bs: INTEGER; VAR b: box);
  PROCEDURE mul_div(x,y: INTEGER);
  BEGIN
    IF x=y THEN RETURN END;
    WHILE NOT ODD(x) & NOT ODD(y) DO x:=x DIV 2; y:=y DIV 2 END;
    IF (y MOD x) = 0 THEN put.lsa(1-y DIV x); put.cmul(x); put.cdiv(y);
    ELSIF (x MOD y) = 0 THEN put.cmul(x); put.cdiv(y); put.lsa(x DIV y - 1);
    ELSE put.lsa(1); put.cmul(x); put.cdiv(y); put.lsa(-1);
    END;
  END mul_div;
  VAR sz: box;
BEGIN
  IF (l=NIL) OR (bs<=0) THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.usage,tbl.index,tbl.field,tbl.deref:
      IF (l^.dw^.md=tbl.packed_dynarr) OR (l^.dw^.md=tbl.packed_array_of) THEN
        load_bits(l^.dw^.r,sz); imm?(sz);
        IF sz.imm<=0 THEN error(l,assert) END;
        gen_load(l,b,load_high); stk?(b);
        mul_div(sz.imm,bs);
      ELSIF (l^.dw^.md=tbl.dynarr) OR (l^.dw^.md=tbl.array_of) THEN
        load_bits(l^.dw^.r,sz); imm?(sz);
        IF sz.imm<=0 THEN error(l,assert) END;
        sz.imm:=(sz.imm+31) DIV 32 * 32;
        gen_load(l,b,load_high); stk?(b);
        mul_div(sz.imm,bs);
      ELSE
        load_bits(l,b);
        IF b.bm=bm_imm THEN b.imm:=b.imm DIV bs -1;
        ELSE stk?(b); put.cdiv(bs); put.lsa(-1);
        END;
      END;
  ELSE
    load_bits(l,b);
    IF b.bm=bm_imm THEN b.imm:=b.imm DIV bs -1;
    ELSE stk?(b); put.cdiv(bs); put.lsa(-1);
    END;
  END;
END load_user_base_high;

PROCEDURE load_user_size(l: ref; VAR b: box);
  VAR sz: box;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  CASE l^.md OF
    |tbl.usage,tbl.index,tbl.field,tbl.deref:
      IF (l^.dw^.md=tbl.dynarr) OR (l^.dw^.md=tbl.array_of) THEN
        gen_load(l,b,load_high);
        IF b.bm=bm_stk THEN
          put.lsa(1);
          calc_load(l^.dw^.r,sz,load_size);
          IF sz.bm=bm_imm THEN put.cmul(sz.imm);
          ELSIF sz.bm=bm_stk THEN put.c(mcd.mul);
          ELSE b.bm:=sz.bm;
          END;
        END;
      ELSIF (l^.dw^.md=tbl.packed_dynarr) OR
            (l^.dw^.md=tbl.packed_array_of) THEN
        gen_load(l,b,load_high);
        IF b.bm=bm_stk THEN
          try_load(l^.dw^.r,sz,load_bits);
          IF sz.bm=bm_imm THEN cmul_round32(1,sz.imm);
          ELSIF sz.bm=bm_stk THEN
            put.lsa(1); gen_load(l^.dw^.r,sz,load_bits);
            put.c(mcd.mul); put.lsa(31); put.cdiv(32);
          ELSE b.bm:=sz.bm;
          END;
        END;
      ELSE load_size(l,b);
      END;
  ELSE load_size(l,b);
  END;
END load_user_size;

PROCEDURE load_user_adr(l: ref; VAR b: box); FORWARD;

PROCEDURE gen_call(l: ref; func: BOOLEAN);
  VAR dec_S: INTEGER;
  PROCEDURE gen_parms;
    VAR ass,prm,val,t: ref; b,e: box;
  BEGIN
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
        IF array_of?(t) THEN
          gen_load(val,b,load_user_adr); stk?(b);
          IF t^.md=tbl.packed_array_of THEN load_bits(t^.r,e);
          ELSE load_32size(t^.r,e);
          END;
          imm?(e);
          load_user_base_high(val,e.imm,b);
          IF b.bm=bm_imm THEN put.li(b.imm) ELSE stk?(b) END;
          IF des.vars[prm^.adr].disp<0 THEN
            put.c(mcd.stot); put.c(mcd.stot); INC(dec_S,2)
          END;
        ELSE
          IF des.vars[prm^.adr].am=am_L THEN
            gen_load(val,b,load_val); stk?(b);
            IF des.vars[prm^.adr].disp<0 THEN put.c(mcd.stot); INC(dec_S) END;
          ELSIF des.vars[prm^.adr].am=am_aL THEN
            gen_load(val,b,load_adr); stk?(b);
            IF des.vars[prm^.adr].no<0 THEN put.c(mcd.stot); INC(dec_S) END;
          ELSE error(val,assert);
          END;
        END;
      END;
      ass:=ass^.nxt;
    END;
  END gen_parms;
  VAR proc: ref; n: INTEGER; b: box;
BEGIN
  ASSERT(put.stk=0);
  ASSERT(l^.md=tbl.call);
  dec_S:=0;
  IF l^.l=NIL THEN error(l,'call^.l=NIL');
  ELSIF func & (l^.dw=NIL) THEN error(l,'func call^.dw=NIL');
  ELSIF (l^.l^.md=tbl.usage) & (l^.l^.r#NIL) &
    ((l^.l^.r^.md=tbl.procedure) OR (l^.l^.r^.md=tbl.inline)) THEN
    gen_parms; proc:=l^.l^.r;
    IF proc^.md=tbl.procedure THEN gen_cl(proc);
    ELSIF proc^.md=tbl.inline THEN
      FOR n:=0 TO HIGH(des.inln[proc^.adr]) DO
        put.b(INTEGER(des.inln[proc^.adr][n]));
      END;
    ELSE error(l,'must be procedure')
    END;
  ELSIF profile_pno(l^.l^.dw)<=prm_limit THEN
    gen_load(l^.l,b,load_val);
    IF b.bm#bm_stk THEN error(l^.l,unrealize) END;
    put.c(mcd.stot); gen_parms; put.c(mcd.cf);
  ELSE
    n:=alloc_var(1);
    gen_load(l^.l,b,load_val);
    IF b.bm#bm_stk THEN error(l^.l,unrealize) END;
    IF level=0 THEN put.sgw(n) ELSE put.slw(n) END;
    gen_parms;
    IF level=0 THEN put.lgw(n) ELSE put.llw(n) END;
    put.c(mcd.stot); put.c(mcd.cf);
  END;
  IF func THEN put.stk:=1 ELSE put.stk:=0 END;
  IF dec_S>0 THEN put.li(dec_S); put.c(mcd.decs) END;
END gen_call;

PROCEDURE gen_new(l: ref);
  VAR b,len: box;
BEGIN
  IF (l^.dw=NIL) OR (l^.dw^.md#tbl.procedure) THEN error(l,'new^.dw#procedure')
  ELSIF (l^.l#NIL) & pointer?(l^.l^.dw) THEN
    gen_load(l^.l,b,load_adr); stk?(b);
    gen_load(l^.l^.dw^.dw,b,load_size); stk?(b);
    gen_cl(l^.dw);
  ELSIF (l^.l#NIL) & dynarr?(l^.l^.dw) THEN
    gen_load(l^.l,b,load_adr); stk?(b);
    IF l^.r=NIL THEN len.bm:=bm_imm; len.imm:=0;
    ELSE try_load(l^.r,len,load_val);
    END;
    put.c(mcd.copt);
    IF (len.bm=bm_imm) & (len.imm<=0) THEN
      put.li(7FFFFF80h); put.c(mcd.ssw0);
      put.li(-1); put.c(mcd.ssw1);
    ELSE
      put.li(-1); put.c(mcd.ssw1);
      put.c(mcd.copt); put.lsa(1);
      IF l^.r=NIL THEN put.li(0);
      ELSE gen_load(l^.r,b,load_val); stk?(b);
      END;
      IF l^.l^.dw^.md=tbl.dynarr THEN
        calc_load(l^.l^.dw^.r,b,load_size);
        IF b.bm=bm_imm THEN mask_overflow; put.li(b.imm*4); check_overflow;
        ELSE stk?(b); put.cmul(4);
        END;
      ELSE
        calc_load(l^.l^.dw^.r,b,load_bits); imm?(b);
        IF b.imm MOD 8 # 0 THEN error(l,'illegal base size') END;
        mask_overflow; put.li(b.imm DIV 8); check_overflow;
      END;
      gen_cl(l^.dw);
    END;
  ELSE error(l^.l,'must be dynarr or pointer');
  END;
END gen_new;

PROCEDURE gen_resize(l: ref);
  VAR b: box;
BEGIN
  IF (l^.dw=NIL) OR (l^.dw^.md#tbl.procedure) THEN error(l,'new^.dw#procedure')
  ELSIF (l^.l#NIL) & dynarr?(l^.l^.dw) THEN
    gen_load(l^.l,b,load_adr); stk?(b);
    put.c(mcd.copt); put.lsa(1);
    IF l^.r=NIL THEN put.li(0);
    ELSE gen_load(l^.r,b,load_val); stk?(b);
    END;
    IF l^.l^.dw^.md=tbl.dynarr THEN
      calc_load(l^.l^.dw^.r,b,load_size);
      IF b.bm=bm_imm THEN mask_overflow; put.li(b.imm*4); check_overflow;
      ELSE stk?(b); put.cmul(4);
      END;
    ELSE
      calc_load(l^.l^.dw^.r,b,load_bits); imm?(b);
      IF b.imm MOD 8 # 0 THEN error(l,'illegal base size') END;
      mask_overflow; put.li(b.imm DIV 8); check_overflow;
    END;
    gen_cl(l^.dw);
  ELSE error(l^.l,'gen resize: must be dynarr');
  END;
END gen_resize;

PROCEDURE gen_dispose(l: ref);
  VAR b: box;
BEGIN
  IF (l^.dw=NIL) OR (l^.dw^.md#tbl.procedure) THEN error(l,'new^.dw#procedure')
  ELSIF (l^.l#NIL) & pointer?(l^.l^.dw) THEN
    gen_load(l^.l,b,load_adr); stk?(b);
    gen_load(l^.l^.dw^.dw,b,load_size); stk?(b);
    gen_cl(l^.dw);
  ELSIF (l^.l#NIL) & dynarr?(l^.l^.dw) THEN
    gen_load(l^.l,b,load_adr); stk?(b);
    put.c(mcd.copt); put.lsa(1);
    IF l^.r=NIL THEN put.li(0);
    ELSE gen_load(l^.r,b,load_val); stk?(b);
    END;
    IF l^.l^.dw^.md=tbl.dynarr THEN
      calc_load(l^.l^.dw^.r,b,load_size);
      IF b.bm=bm_imm THEN mask_overflow; put.li(b.imm*4); check_overflow;
      ELSE stk?(b); put.cmul(4);
      END;
    ELSE
      calc_load(l^.l^.dw^.r,b,load_bits); imm?(b);
      IF b.imm MOD 8 # 0 THEN error(l,'illegal base size') END;
      mask_overflow; put.li(b.imm DIV 8); check_overflow;
    END;
    gen_cl(l^.dw);
  ELSE error(l^.l,'must be dynarr or pointer');
  END;
END gen_dispose;

PROCEDURE gen_plus(l,r: ref; VAR b: box);
  VAR pos: INTEGER; vl: val_mode; bl,br: box;
BEGIN
  b.bm:=bm_stk; vl:=vm(l); IF vl#vm(r) THEN error(l,type_check) END;
  CASE vl OF
    |vm_boolean:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        IF bl.imm#0 THEN b.bm:=bm_imm; b.imm:=bl.imm ELSE load_val(r,b) END;
      ELSIF bl.bm=bm_stk THEN
        put.c(mcd.orjp); pos:=put.cnt; put.b(0); DEC(put.stk);
        load_val(r,br);
        IF (br.bm=bm_imm) & (br.imm=0) THEN
          put.cnt:=pos-1; INC(put.stk); RETURN
        END;
        IF br.bm=bm_imm THEN put.li(br.imm)
        ELSIF br.bm#bm_stk THEN b.bm:=br.bm; RETURN
        END;
        IF put.cnt-pos-1>255 THEN error(l,unrealize) END;
        put.code[pos]:=CHAR(put.cnt-pos-1);
        block^.cmp:=-1;
      ELSE b.bm:=bl.bm;
      END;
    |vm_real:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        IF REAL(bl.imm)=0.0 THEN load_val(r,b); RETURN END;
        load_val(r,br);
        IF br.bm=bm_imm THEN
          IF REAL(br.imm)=0.0 THEN b:=bl; RETURN END;
          b.bm:=bm_imm; b.imm:=INTEGER(REAL(bl.imm)+REAL(br.imm)); RETURN
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); put.c(mcd.fadd)
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF REAL(br.imm)=0.0 THEN b:=bl; RETURN END;
          put.li(br.imm); put.c(mcd.fadd)
        ELSIF br.bm=bm_stk THEN put.c(mcd.fadd)
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
    |vm_set:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        IF bl.imm=0 THEN load_val(r,b); RETURN END;
        load_val(r,br);
        IF br.bm=bm_imm THEN
          IF br.imm=0 THEN b:=bl; RETURN END;
          b.bm:=bm_imm; b.imm:=INTEGER(BITSET(bl.imm)+BITSET(br.imm));
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); put.c(mcd.or)
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF br.imm=0 THEN b:=bl; RETURN END;
          put.li(br.imm); put.c(mcd.or)
        ELSIF br.bm=bm_stk THEN put.c(mcd.or)
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
  ELSE
    load_val(l,bl);
    IF bl.bm=bm_imm THEN
      IF bl.imm=0 THEN load_val(r,b); RETURN END;
      load_val(r,br);
      IF br.bm=bm_imm THEN
        IF br.imm=0 THEN b:=bl; RETURN END;
        mask_overflow; b.bm:=bm_imm; b.imm:=bl.imm+br.imm; check_overflow;
      ELSIF br.bm=bm_stk THEN
        put.li(bl.imm); put.c(mcd.add)
      ELSE b.bm:=br.bm;
      END;
    ELSIF bl.bm=bm_stk THEN
      calc_load(r,br,load_val);
      IF br.bm=bm_imm THEN
        IF br.imm=0 THEN b:=bl; RETURN END;
        put.li(br.imm); put.c(mcd.add)
      ELSIF br.bm=bm_stk THEN put.c(mcd.add)
      ELSE b.bm:=br.bm;
      END;
    ELSE b.bm:=bl.bm;
    END;
  END;
END gen_plus;

PROCEDURE power2?(n: INTEGER; VAR i: INTEGER): BOOLEAN;
  VAR b: INTEGER;
BEGIN
  IF n<=0 THEN RETURN FALSE END;
  FOR b:=0 TO 30 DO
    IF b IN BITSET(n) THEN i:=b; RETURN BITSET(n)-{b}={} END;
  END;
END power2?;

PROCEDURE gen_star(l,r: ref; VAR b: box);
  VAR bl,br: box; pos,i: INTEGER; vl: val_mode;
BEGIN
  b.bm:=bm_stk; vl:=vm(l);
  IF vl#vm(r) THEN error(l,type_check) END;
  CASE vl OF
    |vm_boolean:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        IF bl.imm=0 THEN b.bm:=bm_imm; b.imm:=0 ELSE load_val(r,b) END;
      ELSIF bl.bm=bm_stk THEN
        put.c(mcd.andjp); pos:=put.cnt; put.b(0); DEC(put.stk);
        gen_load(r,br,load_val);
        IF br.bm#bm_stk THEN b.bm:=br.bm; RETURN END;
        IF put.cnt-pos-1>255 THEN error(l,unrealize) END;
        put.code[pos]:=CHAR(put.cnt-pos-1);
        block^.cmp:=-1;
      ELSE b.bm:=bl.bm;
      END;
    |vm_real:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        IF REAL(bl.imm)=1.0 THEN load_val(r,b); RETURN END;
        load_val(r,br);
        IF br.bm=bm_imm THEN
          IF REAL(br.imm)=1.0 THEN b:=bl; RETURN END;
          b.bm:=bm_imm; b.imm:=INTEGER(REAL(bl.imm)*REAL(br.imm)); RETURN
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); put.c(mcd.fmul);
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF REAL(br.imm)=1.0 THEN b:=bl; RETURN END;
          put.li(br.imm); put.c(mcd.fmul);
        ELSIF br.bm=bm_stk THEN put.c(mcd.fmul);
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
    |vm_set:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        IF bl.imm=-1 THEN load_val(r,b); RETURN END;
        load_val(r,br);
        IF br.bm=bm_imm THEN
          IF br.imm=-1 THEN b:=bl; RETURN END;
          b.bm:=bm_imm; b.imm:=INTEGER(BITSET(bl.imm)*BITSET(br.imm));
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); put.c(mcd.and)
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF br.imm=-1 THEN b:=bl; RETURN END;
          put.li(br.imm); put.c(mcd.and)
        ELSIF br.bm=bm_stk THEN put.c(mcd.and)
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
  ELSE
    load_val(l,bl);
    IF bl.bm=bm_imm THEN
      load_val(r,br);
      IF br.bm=bm_imm THEN
        mask_overflow; b.imm:=bl.imm*br.imm; b.bm:=bm_imm; check_overflow;
      ELSIF br.bm=bm_stk THEN put.cmul(bl.imm);
      ELSE b.bm:=br.bm;
      END;
    ELSIF bl.bm=bm_stk THEN
      calc_load(r,br,load_val);
      IF br.bm=bm_imm THEN put.cmul(br.imm);
      ELSIF br.bm=bm_stk THEN put.c(mcd.mul);
      ELSE b.bm:=br.bm;
      END;
    ELSE b.bm:=bl.bm;
    END;
  END;
END gen_star;

PROCEDURE gen_minus(l,r: ref; VAR b: box);
  VAR pos: INTEGER; vl: val_mode; bl,br: box;
BEGIN
  b.bm:=bm_stk; vl:=vm(l); IF vl#vm(r) THEN error(l,type_check) END;
  CASE vl OF
    |vm_real:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        try_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          b.bm:=bm_imm; b.imm:=INTEGER(REAL(bl.imm)-REAL(br.imm))
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); gen_load(r,br,load_val); put.c(mcd.fsub);
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF REAL(br.imm)=0.0 THEN b:=bl; RETURN END;
          put.li(br.imm); put.c(mcd.fsub);
        ELSIF br.bm=bm_stk THEN put.c(mcd.fsub);
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
    |vm_set:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        try_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          b.bm:=bm_imm; b.imm:=INTEGER(BITSET(bl.imm)-BITSET(br.imm))
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); gen_load(r,br,load_val); put.c(mcd.bic);
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF br.imm=0 THEN b:=bl; RETURN END;
          put.li(br.imm); put.c(mcd.bic);
        ELSIF br.bm=bm_stk THEN put.c(mcd.bic);
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
  ELSE
    load_val(l,bl);
    IF bl.bm=bm_imm THEN
      try_load(r,br,load_val);
      IF br.bm=bm_imm THEN
        mask_overflow; b.bm:=bm_imm; b.imm:=bl.imm-br.imm; check_overflow;
      ELSIF br.bm=bm_stk THEN
        IF bl.imm=0 THEN gen_load(r,br,load_val); put.c(mcd.neg);
        ELSE put.li(bl.imm); gen_load(r,br,load_val); put.c(mcd.sub);
        END;
      ELSE b.bm:=br.bm;
      END;
    ELSIF bl.bm=bm_stk THEN
      calc_load(r,br,load_val);
      IF br.bm=bm_imm THEN
        IF br.imm=0 THEN b:=bl
        ELSE put.li(br.imm); put.c(mcd.sub);
        END;
      ELSIF br.bm=bm_stk THEN put.c(mcd.sub);
      ELSE b.bm:=br.bm;
      END;
    ELSE b.bm:=bl.bm;
    END;
  END;
END gen_minus;

PROCEDURE gen_neg(l: ref; VAR b: box);
  VAR i: INTEGER; m: BITSET; fr,to: box;
BEGIN
  CASE vm(l) OF
    |vm_real:
      load_val(l,b);
      IF b.bm=bm_imm THEN b.imm:=INTEGER(-REAL(b.imm))
      ELSIF b.bm=bm_stk THEN put.c(mcd.fneg)
      END;
    |vm_set:
      load_min(l^.dw,fr);
      IF fr.bm#bm_imm THEN error(l,unrealize) END;
      load_max(l^.dw,to);
      IF to.bm#bm_imm THEN error(l,unrealize) END;
      load_val(l,b);
      m:={};
      FOR i:=0 TO to.imm-fr.imm DO INCL(m,i) END;
      IF b.bm=bm_imm THEN b.imm:=INTEGER(BITSET(b.imm)/m)
      ELSIF b.bm=bm_stk THEN put.li(INTEGER(m)); put.c(mcd.xor);
      END;
  ELSE
    load_val(l,b);
    IF b.bm=bm_imm THEN
      mask_overflow; b.imm:=-b.imm; check_overflow;
    ELSIF b.bm=bm_stk THEN put.c(mcd.neg)
    END;
  END;
END gen_neg;

PROCEDURE gen_abs(l: ref; VAR b: box);
BEGIN
  IF vm(l)=vm_real THEN
    load_val(l,b);
    IF b.bm=bm_imm THEN b.imm:=INTEGER(ABS(REAL(b.imm)))
    ELSIF b.bm=bm_stk THEN put.c(mcd.fabs)
    END;
  ELSE
    load_val(l,b);
    IF b.bm=bm_imm THEN b.imm:=ABS(b.imm)
    ELSIF b.bm=bm_stk THEN put.c(mcd.abs)
    END;
  END;
END gen_abs;

PROCEDURE gen_div(l,r: ref; VAR b: box);
  VAR br,bl: box; i: INTEGER;
BEGIN
  b.bm:=bm_stk;
  load_val(l,bl);
  IF bl.bm=bm_imm THEN
    try_load(r,br,load_val);
    IF br.bm=bm_imm THEN b.bm:=bm_imm; b.imm:=bl.imm DIV br.imm;
    ELSIF br.bm=bm_stk THEN put.li(bl.imm); load_val(r,br); put.c(mcd.div);
    ELSE b.bm:=br.bm;
    END;
  ELSIF bl.bm=bm_stk THEN
    calc_load(r,br,load_val);
    IF br.bm=bm_imm THEN put.cdiv(br.imm);
    ELSIF br.bm=bm_stk THEN put.c(mcd.div);
    ELSE b.bm:=br.bm;
    END;
  ELSE b.bm:=bl.bm;
  END;
END gen_div;

PROCEDURE gen_mod(l,r: ref; VAR b: box);
  VAR br,bl: box; i,j: INTEGER; m: BITSET;
BEGIN
  b.bm:=bm_stk;
  load_val(l,bl);
  IF bl.bm=bm_imm THEN
    try_load(r,br,load_val);
    IF br.bm=bm_imm THEN b.bm:=bm_imm; b.imm:=bl.imm MOD br.imm;
    ELSIF br.bm=bm_stk THEN put.li(bl.imm); load_val(r,br); put.c(mcd.mod);
    ELSE b.bm:=br.bm;
    END;
  ELSIF bl.bm=bm_stk THEN
    calc_load(r,br,load_val);
    IF br.bm=bm_imm THEN
      IF power2?(br.imm,i) THEN
        m:={}; FOR j:=0 TO i-1 DO INCL(m,j) END;
        put.li(INTEGER(m)); put.c(mcd.and);
      ELSE put.li(br.imm); put.c(mcd.mod);
      END;
    ELSIF br.bm=bm_stk THEN put.c(mcd.mod);
    ELSE b.bm:=br.bm;
    END;
  ELSE b.bm:=bl.bm;
  END;
END gen_mod;

PROCEDURE gen_slash(l,r: ref; VAR b: box);
  VAR br,bl: box; i: INTEGER; vl: val_mode;
BEGIN
  vl:=vm(l); IF vm(r)#vl THEN error(r,type_check) END;
  b.bm:=bm_stk;
  load_val(l,bl);
  CASE vl OF
    |vm_real:
      IF bl.bm=bm_imm THEN
        try_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          b.bm:=bm_imm; b.imm:=INTEGER(REAL(bl.imm)/REAL(br.imm));
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); load_val(r,br); put.c(mcd.fdiv);
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF REAL(br.imm)=1.0 THEN
          ELSIF REAL(br.imm)=-1.0 THEN put.c(mcd.fneg);
          ELSE put.li(br.imm); put.c(mcd.fdiv);
          END;
        ELSIF br.bm=bm_stk THEN put.c(mcd.fdiv);
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
    |vm_set:
      IF bl.bm=bm_imm THEN
        try_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          b.bm:=bm_imm; b.imm:=INTEGER(BITSET(bl.imm)/BITSET(br.imm));
        ELSIF br.bm=bm_stk THEN
          IF bl.imm=0 THEN load_val(r,b)
          ELSE put.li(bl.imm); load_val(r,br); put.c(mcd.xor);
          END;
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        calc_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          IF br.imm=0 THEN
          ELSE put.li(br.imm); put.c(mcd.xor);
          END;
        ELSIF br.bm=bm_stk THEN put.c(mcd.xor);
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
  ELSE
    IF bl.bm=bm_imm THEN
      try_load(r,br,load_val);
      IF br.bm=bm_imm THEN b.bm:=bm_imm; b.imm:=bl.imm/br.imm;
      ELSIF br.bm=bm_stk THEN
        put.li(bl.imm); load_val(r,br); put.c(mcd.quot); put.b(mcd.quot_quo);
      ELSE b.bm:=br.bm;
      END;
    ELSIF bl.bm=bm_stk THEN
      calc_load(r,br,load_val);
      IF br.bm=bm_imm THEN
        IF br.imm=1 THEN
        ELSIF br.imm=-1 THEN put.c(mcd.neg);
        ELSIF power2?(br.imm,i) THEN
          put.li(i); put.c(mcd.quot); put.b(mcd.quot_shr);
        ELSE put.li(br.imm); put.c(mcd.quot); put.b(mcd.quot_quo);
        END;
      ELSIF br.bm=bm_stk THEN put.c(mcd.quot); put.b(mcd.quot_quo);
      ELSE b.bm:=br.bm;
      END;
    ELSE b.bm:=bl.bm;
    END;
  END;
END gen_slash;

PROCEDURE gen_rem(l,r: ref; VAR b: box);
  VAR br,bl: box; i,j: INTEGER; m: BITSET;
BEGIN
  b.bm:=bm_stk;
  load_val(l,bl);
  IF bl.bm=bm_imm THEN
    try_load(r,br,load_val);
    IF br.bm=bm_imm THEN b.bm:=bm_imm; b.imm:=bl.imm REM br.imm;
    ELSIF br.bm=bm_stk THEN
      put.li(bl.imm); load_val(r,br); put.c(mcd.quot); put.b(mcd.quot_rem);
    ELSE b.bm:=br.bm;
    END;
  ELSIF bl.bm=bm_stk THEN
    calc_load(r,br,load_val);
    IF br.bm=bm_imm THEN
      IF power2?(br.imm,i) THEN
        m:={}; FOR j:=0 TO i-1 DO INCL(m,j) END;
        put.li(INTEGER(m)); put.c(mcd.quot); put.b(mcd.quot_and);
      ELSE put.li(br.imm); put.c(mcd.quot); put.b(mcd.quot_rem);
      END;
    ELSIF br.bm=bm_stk THEN put.c(mcd.quot); put.b(mcd.quot_rem);
    ELSE b.bm:=br.bm;
    END;
  ELSE b.bm:=bl.bm;
  END;
END gen_rem;

PROCEDURE gen_compare(l,r: ref; cc: INTEGER; goto: BOOLEAN; VAR b: box);
  VAR vl: val_mode; bl,br: box;
BEGIN
  b.bm:=bm_stk; vl:=vm(l);
  IF vl#vm(r) THEN error(l,type_check) END;
  CASE vl OF
    |vm_real:
      load_val(l,bl);
      IF bl.bm=bm_imm THEN
        try_load(r,br,load_val);
        IF br.bm=bm_imm THEN
          CASE cc OF
            |mcd.equ: b.imm:=INTEGER(REAL(bl.imm)=REAL(br.imm));
            |mcd.neq: b.imm:=INTEGER(REAL(bl.imm)#REAL(br.imm));
            |mcd.lss: b.imm:=INTEGER(REAL(bl.imm)<REAL(br.imm));
            |mcd.leq: b.imm:=INTEGER(REAL(bl.imm)<=REAL(br.imm));
            |mcd.gtr: b.imm:=INTEGER(REAL(bl.imm)>REAL(br.imm));
            |mcd.geq: b.imm:=INTEGER(REAL(bl.imm)>=REAL(br.imm));
          END;
          b.bm:=bm_imm;
        ELSIF br.bm=bm_stk THEN
          put.li(bl.imm); gen_load(r,br,load_val);
          put.c(mcd.fcmp); block^.cmp:=put.cnt; put.c(cc);
        ELSE b.bm:=br.bm;
        END;
      ELSIF bl.bm=bm_stk THEN
        gen_load(r,br,load_val);
        IF br.bm=bm_stk THEN put.c(mcd.fcmp); block^.cmp:=put.cnt; put.c(cc)
        ELSE b.bm:=br.bm;
        END;
      ELSE b.bm:=bl.bm;
      END;
    |vm_string:
      gen_load(l,bl,load_user_adr);
      IF bl.bm#bm_stk THEN b.bm:=bl.bm; RETURN END;
      gen_load(r,br,load_user_adr);
      IF br.bm#bm_stk THEN b.bm:=br.bm; RETURN END;
      put.c(mcd.comp); block^.cmp:=put.cnt; put.c(cc);
  ELSE
    load_val(l,bl);
    IF bl.bm=bm_imm THEN
      try_load(r,br,load_val);
      IF br.bm=bm_imm THEN
        CASE cc OF
          |mcd.equ: b.imm:=INTEGER(bl.imm=br.imm);
          |mcd.neq: b.imm:=INTEGER(bl.imm#br.imm);
          |mcd.lss: b.imm:=INTEGER(bl.imm<br.imm);
          |mcd.leq: b.imm:=INTEGER(bl.imm<=br.imm);
          |mcd.gtr: b.imm:=INTEGER(bl.imm>br.imm);
          |mcd.geq: b.imm:=INTEGER(bl.imm>=br.imm);
        END;
        b.bm:=bm_imm;
      ELSIF br.bm=bm_stk THEN
        IF goto & (bl.imm=0) & (cc=mcd.neq) THEN gen_load(r,b,load_val);
        ELSE
          put.li(bl.imm); gen_load(r,br,load_val);
          block^.cmp:=put.cnt; put.c(cc)
        END;
      ELSE b.bm:=br.bm;
      END;
    ELSIF bl.bm=bm_stk THEN
      gen_load(r,br,load_val);
      IF br.bm=bm_stk THEN
        IF goto & (bl.imm=0) & (cc=mcd.neq) THEN b:=br
        ELSE block^.cmp:=put.cnt; put.c(cc)
        END;
      ELSE b.bm:=br.bm
      END;
    ELSE b.bm:=bl.bm;
    END;
  END;
END gen_compare;

PROCEDURE gen_in(l,r: ref; VAR b: box);
  VAR bl,br,min,sz: box;
BEGIN
  b.bm:=bm_stk;
  IF vm(r)#vm_set THEN error(r,'must be set') END;
  gen_load(l,bl,load_val);
  IF bl.bm#bm_stk THEN error(l,unrealize) END;
  calc_load(r^.dw^.dw,min,load_min);
  IF min.bm=bm_imm THEN put.lsa(-min.imm)
  ELSIF min.bm=bm_stk THEN put.c(mcd.sub);
  ELSE error(l,unrealize)
  END;
  try_load(r^.dw,sz,load_size);
  IF (sz.bm=bm_imm) & (sz.imm=1) THEN
    gen_load(r,br,load_val);
    IF br.bm#bm_stk THEN error(r,assert) END;
    put.c(mcd.in);
  ELSE
    gen_load(r,br,load_adr);
    IF br.bm#bm_stk THEN error(r,unrealize) END;
    gen_load(r^.dw,sz,load_bits);
    IF sz.bm#bm_stk THEN error(r,unrealize) END;
    put.c(mcd.inl);
  END;
END gen_in;

PROCEDURE gen_rol(l,r: ref; VAR b: box);
  VAR br,bl: box;
BEGIN
  b.bm:=bm_stk;
  load_val(l,bl);
  IF bl.bm=bm_imm THEN
    try_load(r,br,load_val);
    IF br.bm=bm_imm THEN b.bm:=bm_imm; b.imm:=bl.imm << br.imm;
    ELSIF br.bm=bm_stk THEN put.li(bl.imm); load_val(r,br); put.c(mcd.rol);
    ELSE b.bm:=br.bm;
    END;
  ELSIF bl.bm=bm_stk THEN
    gen_load(r,br,load_val);
    IF br.bm=bm_stk THEN put.c(mcd.rol);
    ELSE b.bm:=br.bm;
    END;
  ELSE b.bm:=bl.bm;
  END;
END gen_rol;

PROCEDURE gen_ror(l,r: ref; VAR b: box);
  VAR br,bl: box;
BEGIN
  b.bm:=bm_stk;
  load_val(l,bl);
  IF bl.bm=bm_imm THEN
    try_load(r,br,load_val);
    IF br.bm=bm_imm THEN b.bm:=bm_imm; b.imm:=bl.imm >> br.imm;
    ELSIF br.bm=bm_stk THEN put.li(bl.imm); load_val(r,br); put.c(mcd.ror);
    ELSE b.bm:=br.bm;
    END;
  ELSIF bl.bm=bm_stk THEN
    gen_load(r,br,load_val);
    IF br.bm=bm_stk THEN put.c(mcd.ror);
    ELSE b.bm:=br.bm;
    END;
  ELSE b.bm:=bl.bm;
  END;
END gen_ror;

PROCEDURE gen_string(l: ref);
  VAR p: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR; i,n: INTEGER;
BEGIN
  i:=(HIGH(l^.str)+4) DIV 4;
  n:=des.new_str(i);
  p:=ADR(des.scode[n]); i:=0;
  WHILE i<=HIGH(l^.str) DO p^[i]:=l^.str[i]; INC(i) END;
  WHILE (i MOD 4)#0 DO p^[i]:=0c; INC(i) END;
  l^.adr:=des.new_var();
  des.vars[l^.adr].am:=am_STR;
  des.vars[l^.adr].lvl:=0;
  des.vars[l^.adr].disp:=n*32;
END gen_string;

PROCEDURE load_adr(l: ref; VAR b: box);
  VAR a: access;
BEGIN
  des.load_access(l,a); b.bm:=bm_stk;
  IF a.disp MOD 32 #0 THEN b.bm:=bm_adr; RETURN END;
  CASE a.am OF
    |am_xb,am_xt,am_stk,am_imm: b.bm:=bm_adr;
  ELSE des.convert_to_address(a);
  END;
END load_adr;

PROCEDURE load_user_adr(l: ref; VAR b: box);
  VAR a: access;
BEGIN
  CASE l^.md OF
    |tbl.index,tbl.usage,tbl.field,tbl.deref:
      IF dynarr?(l^.dw) OR array_of?(l^.dw) THEN
        des.load_access(l,a);
        put.load(a.am,a.lvl,a.no,a.disp,32);
        b.bm:=bm_stk;
      ELSE load_adr(l,b);
      END;
  ELSE load_adr(l,b);
  END;
END load_user_adr;

PROCEDURE load_range_check(u: ref; VAR b: box);
  CONST err='range check violation';
  VAR max: box;
BEGIN
  load_val(u^.r,b);
  IF u^.dw^.md=tbl.range THEN
    WITH des.rngs[u^.dw^.adr] DO
      IF (b.bm=bm_imm) & (l.am=am_imm) & (r.am=am_imm) THEN
        IF (b.imm<l.no) OR (b.imm>r.no) THEN error(u,err) END;
      ELSE
        IF b.bm=bm_imm THEN put.li(b.imm); b.bm:=bm_stk;
        ELSIF b.bm#bm_stk THEN RETURN
        END;
        IF (l.am=am_imm) & (l.no=0) THEN
          put.load(r.am,r.lvl,r.no,r.disp,32); put.c(mcd.chkz);
        ELSE
          put.load(l.am,l.lvl,l.no,l.disp,32);
          put.load(r.am,r.lvl,r.no,r.disp,32);
          put.c(mcd.chk);
        END;
      END;
    END;
  ELSIF u^.dw^.md=tbl.enumeration THEN
    ---  ASSERT( MIN ( enumeration ) = 0 ) !!!!
    load_max(u^.dw,max);
    IF max.bm#bm_imm THEN error(u,unrealize) END;
    IF b.bm=bm_imm THEN IF (b.imm<0) OR (b.imm>max.imm) THEN error(u,err) END;
    ELSIF b.bm=bm_stk THEN put.li(max.imm); put.c(mcd.chkz);
    END;
  ELSIF u^.dw^.md=tbl.char THEN
    IF b.bm=bm_imm THEN IF (b.imm<0) OR (b.imm>255) THEN error(u,err) END;
    ELSIF b.bm=bm_stk THEN put.li(255); put.c(mcd.chkz);
    END;
  ELSIF u^.dw^.md=tbl.boolean THEN
    IF b.bm=bm_imm THEN IF (b.imm<0) OR (b.imm>1) THEN error(u,err) END;
    ELSIF b.bm=bm_stk THEN put.li(1); put.c(mcd.chkz);
    END;
  ELSE error(u,'load_range_check, illegal type')
  END;
END load_range_check;

PROCEDURE load_worder(l: ref; VAR b: box);
  VAR sz: box;
BEGIN
  CASE l^.dw^.md OF
    |tbl.array,tbl.packed_array,tbl.dynarr,tbl.packed_dynarr,
     tbl.array_of,tbl.packed_array_of,tbl.record,tbl.packed_record:
      load_user_adr(l,b);
  ELSE
    try_load(l^.dw,sz,load_bits);
    IF (sz.bm=bm_imm) & (sz.imm<=32) THEN load_val(l,b);
    ELSE load_user_adr(l,b);
    END;
  END;
END load_worder;

PROCEDURE load_cap(l: ref; VAR b: box);
BEGIN
  load_val(l,b);
  IF b.bm=bm_imm THEN b.imm:=INTEGER(BITSET(b.imm)-{5,6})
  ELSIF b.bm=bm_stk THEN put.li(20h); put.c(mcd.bic);
  END;
END load_cap;

PROCEDURE load_odd(l: ref; VAR b: box);
BEGIN
  load_val(l,b);
  IF b.bm=bm_imm THEN b.imm:=INTEGER(BITSET(b.imm)*{0})
  ELSIF b.bm=bm_stk THEN put.c(mcd.li1); put.c(mcd.and);
  END;
END load_odd;

PROCEDURE load_val(l: ref; VAR b: box);
  VAR a: access; s: INTEGER; sz: box;
BEGIN
  IF l=NIL THEN error(l,assert) END;
  IF l^.md#tbl.number THEN stat_pos:=l^.val END;
  CASE l^.md OF
    |tbl.usage,tbl.aggregate,tbl.index,tbl.deref,
     tbl.field,tbl.string,tbl.number,tbl.var:
      des.load_access(l,a);
      try_load(l,sz,load_bits);
      IF (sz.bm#bm_imm) OR (sz.imm>32) THEN b.bm:=bm_size;
      ELSIF a.am=am_imm THEN b.bm:=bm_imm; b.imm:=a.no;
      ELSIF a.am=am_STR THEN
        b.bm:=bm_imm; b.imm:=0;
        bblt(ADR(b.imm),0,ADR(des.scode),a.disp,sz.imm);
      ELSIF a.am=am_stk THEN b.bm:=bm_stk;
      ELSE put.load(a.am,a.lvl,a.no,a.disp,sz.imm); b.bm:=bm_stk;
      END;
    |tbl.call    :
      IF put.stk#0 THEN
        put.c(mcd.store); s:=put.stk; put.stk:=0;
        gen_call(l,TRUE);
        put.c(mcd.lodfv); put.stk:=s+1;
        IF put.stk_max<put.stk THEN put.stk_max:=put.stk END;
      ELSE
        gen_call(l,TRUE);
      END;
      b.bm:=bm_stk;
    |tbl.nil        : b.bm:=bm_imm; b.imm:=7FFFFF80h;
    |tbl.cap        : load_cap(l^.r,b);
    |tbl.odd        : load_odd(l^.r,b);
    |tbl.low        : load_low(l^.r,b);
    |tbl.high       : load_high(l^.r,b);
    |tbl.min        : load_min(l^.r,b);
    |tbl.max        : load_max(l^.r,b);
    |tbl.adr        : load_user_adr(l^.r,b);
    |tbl.len        : load_len(l^.r,b);
    |tbl.worder     : load_worder(l^.r,b);
    |tbl.size_check : load_val(l^.r,b);
    |tbl.range_check: load_range_check(l,b);
    |tbl.size       : load_user_size(l^.r,b);
    |tbl.bits       : load_user_bits(l^.r,b);
    |tbl.bytes:
      load_user_bits(l^.r,b);
      IF b.bm=bm_imm THEN b.imm:=(b.imm+7) DIV 8
      ELSIF b.bm=bm_stk THEN put.lsa(7); put.c(mcd.li3); put.c(mcd.shr)
      END;
    |tbl.equal        : gen_compare(l^.l,l^.r,mcd.equ,FALSE,b);
    |tbl.inequality   : gen_compare(l^.l,l^.r,mcd.neq,FALSE,b);
    |tbl.less         : gen_compare(l^.l,l^.r,mcd.lss,FALSE,b);
    |tbl.less_equal   : gen_compare(l^.l,l^.r,mcd.leq,FALSE,b);
    |tbl.greater      : gen_compare(l^.l,l^.r,mcd.gtr,FALSE,b);
    |tbl.greater_equal: gen_compare(l^.l,l^.r,mcd.geq,FALSE,b);
    |tbl.plus         : gen_plus(l^.l,l^.r,b);
    |tbl.star         : gen_star(l^.l,l^.r,b);
    |tbl.minus        :
      IF l^.l=NIL THEN gen_neg(l^.r,b)
      ELSE gen_minus(l^.l,l^.r,b)
      END;
    |tbl.abs          : gen_abs(l^.r,b);
    |tbl.in           : gen_in(l^.l,l^.r,b);
    |tbl.slash        : gen_slash(l^.l,l^.r,b);
    |tbl.div          : gen_div(l^.l,l^.r,b);
    |tbl.mod          : gen_mod(l^.l,l^.r,b);
    |tbl.rem          : gen_rem(l^.l,l^.r,b);
    |tbl.rol          : gen_rol(l^.l,l^.r,b);
    |tbl.ror          : gen_ror(l^.l,l^.r,b);
    |tbl.not          :
      load_val(l^.r,b);
      IF b.bm=bm_imm THEN b.imm:=INTEGER(b.imm=0)
      ELSIF b.bm=bm_stk THEN block^.cmp:=put.cnt; put.c(mcd.not)
      END;
    |tbl.trunc:
      load_val(l^.r,b);
      IF b.bm=bm_imm THEN b.imm:=INTEGER(TRUNC(REAL(b.imm)))
      ELSIF b.bm=bm_stk THEN put.c(mcd.ffct); put.b(mcd.ffct_trunc);
      END;
    |tbl.float:
      load_val(l^.r,b);
      IF b.bm=bm_imm THEN b.imm:=INTEGER(FLOAT(b.imm))
      ELSIF b.bm=bm_stk THEN put.c(mcd.ffct); put.b(mcd.ffct_float);
      END;
  ELSE
    error(l,'gen loading failed');
  END;
END load_val;

PROCEDURE gen_var(l: ref);
  VAR b: box; n: INTEGER;
BEGIN
  l^.adr:=des.new_var();
  WITH des.vars[l^.adr] DO
    load_size(l^.l,b);
    IF (b.bm=bm_imm) & (b.imm<=size_limit) THEN
      disp:=alloc_var(b.imm)*32; lvl:=level;
      IF level=0 THEN am:=am_G ELSE am:=am_L END;
    ELSIF (level=0) & (b.bm=bm_imm) THEN
      no:=alloc_var(1); disp:=0; am:=am_aG; n:=des.new_mg(1);
      des.mg[n].offset:=no; des.mg[n].size:=b.imm;
    ELSIF level=0 THEN
      put.c(mcd.copt);
      IF glo_alloc_cnt<0 THEN glo_alloc_cnt:=alloc_var(1); put.lsa(4);
      ELSE put.lgw(glo_alloc_cnt); put.c(mcd.add);
      END;
      put.sgw(glo_alloc_cnt);
      no:=alloc_var(1); disp:=0; lvl:=level; am:=am_aG;
      stk?(b); put.c(mcd.alloc); put.sgw(no);
    ELSE
      IF b.bm=bm_imm THEN put.li(b.imm); b.bm:=bm_stk END;
      no:=alloc_var(1); disp:=0; lvl:=level; am:=am_aL;
      stk?(b); put.c(mcd.alloc); put.slw(no);
    END;
    IF tbl.ini_nil THEN
      IF l^.l^.md=tbl.pointer THEN
        put.store1(am,lvl,no,disp,32);
        put.li(7FFFFF80h);
        put.store2(am,lvl,no,disp,32);
      ELSIF (l^.l^.md=tbl.packed_dynarr) OR (l^.l^.md=tbl.dynarr)  THEN
        put.store1(am,lvl,no,disp,32);
        put.li(7FFFFF80h);
        put.store2(am,lvl,no,disp,32);
        put.store1(am,lvl,no,disp+32,32);
        put.li(-1);
        put.store2(am,lvl,no,disp+32,32);
      END;
    END;
  END;
END gen_var;

PROCEDURE gen_assign(l: ref);
  VAR al,ar: access; bl,br,sz,sz_l,sz_r: box; t: trap; next,abort: node;
BEGIN
  ASSERT(l#NIL); ASSERT(l^.md=tbl.assign);
  IF (l^.l=NIL) OR (l^.r=NIL) THEN error(l,assert) END;
  save(t);
  des.load_access(l^.l,al);
  try_load(l^.l,sz,load_user_bits);
  IF (sz.bm=bm_imm) & (sz.imm<=0) THEN restore(t);
  ELSIF (sz.bm=bm_imm) & (sz.imm<=32) THEN
    put.store1(al.am,al.lvl,al.no,al.disp,sz.imm);
    gen_load(l^.r,br,load_val);
    IF br.bm#bm_stk THEN error(l,unrealize) END;
    put.store2(al.am,al.lvl,al.no,al.disp,sz.imm);
  ELSE -- размер значения динамический или > 32
    -- move
    restore(t);
    gen_load(l^.l,bl,load_user_adr);
    gen_load(l^.r,br,load_user_adr);
    IF (bl.bm=bm_stk) & (br.bm=bm_stk) THEN
      calc_load(l^.r,sz_r,load_user_size);
      IF sz_r.bm=bm_imm THEN
        calc_load(l^.l,sz_l,load_user_size);
        IF sz_l.bm=bm_imm THEN
          IF sz_r.imm>sz_l.imm THEN error(l,'illegal size') END;
          put.li(sz_r.imm);
        ELSE
          abort:=new(); next:=new();
          stk?(sz_l); put.li(sz_r.imm);
          block^.cmp:=put.cnt; put.c(mcd.lss); DEC(put.stk);
          block^.true:=abort; block^.false:=next; block^.md:=nm_cond;
          start(abort); put.li(range_trap); put.c(mcd.trap); block^.true:=next;
          start(next); put.li(sz_r.imm);
        END;
      ELSE
        abort:=new(); next:=new();
        stk?(sz_r); put.c(mcd.copt);
        gen_load(l^.l,sz_l,load_user_size); stk?(sz_l);
        block^.cmp:=put.cnt; put.c(mcd.gtr); DEC(put.stk);
        block^.true:=abort; block^.false:=next; block^.md:=nm_cond;
        start(abort); put.li(range_trap); put.c(mcd.trap); block^.true:=next;
        start(next);
      END;
      put.c(mcd.move);
    ELSE -- bblt
      error(l,'bblt - unrealized');
    END;
  END;
  IF put.stk#0 THEN error(l,'assign: stk # 0') END;
END gen_assign;

PROCEDURE gen_return(l: ref);
  VAR b: box;
BEGIN
  IF level=0 THEN put.c(mcd.li0);
  ELSIF l^.dw#NIL THEN
    gen_load(l^.dw,b,load_val);
    IF b.bm#bm_stk THEN error(l,unrealize) END;
  END;
  put.c(mcd.rtn); put.stk:=0; start(new());
END gen_return;

PROCEDURE gen_incl_excl(excl: BOOLEAN; l: ref);
  VAR bl,br,min,max: box;
BEGIN
  IF vm(l^.l)#vm_set THEN error(l^.l,'must be set') END;
  gen_load(l^.l,bl,load_adr); stk?(bl);
  calc_load(l^.r,br,load_val);
  IF br.bm=bm_imm THEN put.li(br.imm) ELSE stk?(br) END;
  calc_load(l^.l^.dw^.dw,min,load_min);
  IF min.bm=bm_imm THEN
    put.lsa(-min.imm);
    calc_load(l^.l^.dw^.dw,max,load_max);
    IF max.bm=bm_imm THEN
      IF br.bm=bm_imm THEN
        IF (br.imm<min.imm) OR (br.imm>max.imm) THEN
          error(l^.r,'range check violation');
        END;
      ELSE put.li(max.imm-min.imm); put.c(mcd.chkz);
      END;
    ELSIF max.bm=bm_stk THEN put.lsa(-min.imm); put.c(mcd.chkz);
    ELSE error(l^.l,unrealize);
    END;
  ELSIF min.bm=bm_stk THEN
    gen_load(l^.l^.dw^.dw,max,load_max);
    IF max.bm#bm_stk THEN error(l^.l,unrealize) END;
    put.c(mcd.chk);
    gen_load(l^.l^.dw^.dw,min,load_min);
    put.c(mcd.sub);
  ELSE error(l^.l,unrealize);
  END;
  IF excl THEN put.c(mcd.excl) ELSE put.c(mcd.incl) END;
END gen_incl_excl;

PROCEDURE gen_condition(cond: ref; then,else: node);
  VAR h: node; b: box;
BEGIN
  IF cond=NIL THEN error(cond,assert) END;
  IF cond^.md=tbl.not THEN
    gen_condition(cond^.r,else,then);
  ELSIF (cond^.md=tbl.plus) &
        (vm(cond^.l)=vm_boolean) & (vm(cond^.r)=vm_boolean) THEN
    h:=new();
    gen_condition(cond^.l,then,h);
    start(h);
    gen_condition(cond^.r,then,else);
  ELSIF (cond^.md=tbl.star) &
        (vm(cond^.l)=vm_boolean) & (vm(cond^.r)=vm_boolean) THEN
    h:=new();
    gen_condition(cond^.l,h,else);
    start(h);
    gen_condition(cond^.r,then,else);
  ELSE
    calc_load(cond,b,load_val);
    IF b.bm=bm_stk THEN
      block^.true:=then; block^.false:=else; block^.md:=nm_cond;
      DEC(put.stk);
    ELSIF b.bm=bm_imm THEN
      IF b.imm#0 THEN block^.true:=then ELSE block^.true:=else END;
    ELSE error(cond,unrealize);
    END;
  END;
END gen_condition;

PROCEDURE one(l: ref); FORWARD;

PROCEDURE gen_if(l: ref);
  VAR n: ref; next,then,else: node;
BEGIN
  then:=new(); else:=new(); next:=new();
  gen_condition(l^.dw,then,else);
  start(then); n:=l^.l;
  WHILE n#NIL DO one(n); n:=n^.nxt END;
  block^.true:=next; start(else); n:=l^.r;
  WHILE n#NIL DO one(n); n:=n^.nxt END;
  block^.true:=next; start(next);
END gen_if;

PROCEDURE gen_proc(l: ref);
BEGIN
  l^.adr:=des.new_proc();
  WITH des.prcs[l^.adr] DO
    mod:=0;
    WITH des.cu DO disp:=prc_sz; INC(prc_sz) END;
    lvl:=level+1;
    loc_sz:=profile_loc(l^.l)+4;
  END;
END gen_proc;

PROCEDURE gen_profile(l: ref);
-- вичисляет методы доступа для параметров
  VAR v,i,t: ref; adr: INTEGER; var: BOOLEAN; b: box;
BEGIN
  v:=NIL; i:=l^.dw;
  WHILE i#NIL DO
    IF i^.md=tbl.var THEN i^.r:=v; v:=i END;
    i:=i^.nxt;
  END;
  adr:=4;
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
    IF array_of?(t) THEN
      des.vars[v^.adr].am:=am_L;
      IF adr>=prm_limit+3 THEN adr:=-1 END;
      IF adr>0 THEN INC(adr) END;
      des.vars[v^.adr].disp:=adr*32;
      IF adr>0 THEN INC(adr) ELSE DEC(adr,2) END;
    ELSE
      IF var THEN des.vars[v^.adr].am:=am_aL;
      ELSE try_load(v^.l,b,load_size);
        IF (b.bm=bm_imm) & (b.imm=1) THEN des.vars[v^.adr].am:=am_L;
        ELSE des.vars[v^.adr].am:=am_aL;
        END;
      END;
      IF adr>=prm_limit+4 THEN adr:=-1 END;
      IF des.vars[v^.adr].am=am_L THEN des.vars[v^.adr].disp:=adr*32;
      ELSE des.vars[v^.adr].disp:=0; des.vars[v^.adr].no:=adr;
      END;
      IF adr>0 THEN INC(adr) ELSE DEC(adr) END;
    END;
    des.vars[v^.adr].lvl:=level+1; v:=v^.r;
  END;
END gen_profile;

PROCEDURE gen_record(l: ref);
  VAR packed: BOOLEAN;
  PROCEDURE field(f: ref; VAR pos: INTEGER);
    VAR sz: box;
  BEGIN
    ASSERT(f^.md=tbl.var);
    f^.adr:=des.new_var();
    des.vars[f^.adr].am:=am_adr; des.vars[f^.adr].disp:=pos;
    IF packed THEN load_bits(f^.l,sz) ELSE load_size(f^.l,sz) END;
    IF sz.bm#bm_imm THEN error(f,'record field must have constant size') END;
    IF packed THEN INC(pos,sz.imm) ELSE INC(pos,sz.imm*32) END;
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
  VAR bits: INTEGER;
BEGIN
  packed:=l^.md=tbl.packed_record;
  bits:=0; field_list(l^.dw,bits);
END gen_record;

PROCEDURE load_record_bits(l: ref; VAR b: box);
  VAR packed: BOOLEAN;
  PROCEDURE field(f: ref; VAR pos: INTEGER);
    VAR sz: box;
  BEGIN
    ASSERT(f^.md=tbl.var);
    IF packed THEN load_bits(f^.l,sz) ELSE load_size(f^.l,sz) END;
    ASSERT(sz.bm=bm_imm);
    IF packed THEN INC(pos,sz.imm) ELSE INC(pos,sz.imm*32) END;
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
  packed:=l^.md=tbl.packed_record;
  b.bm:=bm_imm; b.imm:=0; field_list(l^.dw,b.imm);
END load_record_bits;

PROCEDURE gen_for(l: ref; next: node);
  VAR
    fr,to,by: box;
    vn,tn   : INTEGER;
    loop    : node;
    i,r_to,r_by: ref;
    va      : access;
BEGIN
  IF l^.l^.l^.md#tbl.usage THEN error(l^.l^.l,'must be variable') END;
  des.load_access(l^.l^.l,va); des.convert_toAGL(va);
  IF va.disp MOD 32 # 0 THEN error(l^.l^.l,unrealize) END;
  IF va.am=am_adr THEN
    vn:=alloc_var(1); put.c(mcd.copt);
    IF level=0 THEN put.sgw(vn) ELSE put.slw(vn) END;
  END;
  put.store1(va.am,va.lvl,va.no,va.disp,32);
  calc_load(l^.l^.r,fr,load_val);
  IF fr.bm=bm_imm THEN put.li(fr.imm)
  ELSIF fr.bm#bm_stk THEN error(l^.l^.l,unrealize)
  END;
  put.store2(va.am,va.lvl,va.no,va.disp,32);
  i:=l^.l^.nxt; r_to:=NIL; r_by:=NIL;
  WHILE i#NIL DO
    IF i^.md=tbl.exit THEN r_to:=i;
    ELSIF i^.md=tbl.plus THEN r_by:=i;
    ELSE error(i,assert);
    END;
    i:=i^.nxt;
  END;
  IF r_to#NIL THEN
    calc_load(r_to^.dw,to,load_val);
    IF to.bm=bm_stk THEN
      tn:=alloc_var(1);
      IF level=0 THEN put.sgw(tn) ELSE put.slw(tn) END;
    ELSIF to.bm#bm_imm THEN error(r_to,unrealize)
    END;
  ELSE to.bm:=bm_size;
  END;
  IF r_by#NIL THEN
    calc_load(r_by^.r,by,load_val);
    IF by.bm#bm_imm THEN error(r_by,unrealize)
    END;
  ELSE by.bm:=bm_imm; by.imm:=1;
  END;
  IF (to.bm=bm_imm) & (fr.bm=bm_imm) & (by.bm=bm_imm) THEN
    IF by.imm>=0 THEN IF fr.imm>to.imm THEN RETURN END;
    ELSE IF fr.imm<to.imm THEN RETURN END;
    END;
  END;
  loop:=new();
  block^.true:=loop; start(block^.true);
  IF (to.bm=bm_stk) OR (fr.bm=bm_stk) & (to.bm#bm_size) THEN
    IF va.am=am_adr THEN IF level=0 THEN put.lgw(vn) ELSE put.llw(vn) END END;
    put.load(va.am,va.lvl,va.no,va.disp,32);
    IF to.bm=bm_imm THEN put.li(to.imm);
    ELSIF level=0 THEN put.lgw(tn)
    ELSE put.llw(tn)
    END;
    block^.cmp:=put.cnt;
    IF by.imm>=0 THEN put.c(mcd.leq) ELSE put.c(mcd.geq) END;
    block^.false:=next; block^.true:=new();
    block^.md:=nm_cond; start(block^.true);
    DEC(put.stk);
  END;
  i:=l^.dw;
  WHILE i#NIL DO one(i); i:=i^.nxt END;
  IF (fr.bm=bm_imm) & (to.bm=bm_imm) THEN
    IF by.imm#0 THEN
      IF va.am=am_adr THEN
        IF level=0 THEN put.lgw(vn) ELSE put.llw(vn) END;
        des.convert_to_address(va);
        put.c(mcd.copt);
        IF by.imm<-1 THEN put.li(-by.imm); put.c(mcd.dec);
        ELSIF by.imm=-1 THEN put.c(mcd.dec1);
        ELSIF by.imm=1 THEN put.c(mcd.inc1);
        ELSE put.li(by.imm); put.c(mcd.inc);
        END;
        put.c(mcd.lsw0);
      ELSE
        put.load(va.am,va.lvl,va.no,va.disp,32);
        put.lsa(by.imm); put.c(mcd.copt);
        IF va.am=am_G THEN put.sgw(va.disp DIV 32)
        ELSE put.slw(va.disp DIV 32)
        END;
      END;
      put.li(to.imm);
      block^.cmp:=put.cnt;
      IF by.imm>=0 THEN put.c(mcd.leq) ELSE put.c(mcd.geq) END;
      block^.false:=next; block^.true:=loop;
      block^.md:=nm_cond; DEC(put.stk);
    ELSE
      block^.true:=loop;
    END;
  ELSE
    IF by.imm#0 THEN
      IF va.am=am_adr THEN IF level=0 THEN put.lgw(vn) ELSE put.llw(vn) END END;
      des.convert_to_address(va);
      IF by.imm<-1 THEN put.li(-by.imm); put.c(mcd.dec);
      ELSIF by.imm=-1 THEN put.c(mcd.dec1);
      ELSIF by.imm=1 THEN put.c(mcd.inc1);
      ELSE put.li(by.imm); put.c(mcd.inc);
      END;
    END;
    block^.true:=loop;
  END;
  start(next);
END gen_for;

PROCEDURE gen_loop(l: ref);
  VAR i: ref; loop,next,body: node; b: box;
BEGIN
  next:=new(); l^.adr:=INTEGER(next);
  IF l^.l#NIL THEN
    IF l^.l^.md=tbl.assign THEN gen_for(l,next);
    ELSE
      body:=new(); loop:=new(); block^.true:=loop; start(loop);
      gen_condition(l^.l,body,next);
      start(body); i:=l^.dw;
      WHILE i#NIL DO one(i); i:=i^.nxt END;
      block^.true:=loop; start(next);
    END;
  ELSIF l^.r#NIL THEN
    loop:=new(); block^.true:=loop;
    start(loop); i:=l^.dw;
    WHILE i#NIL DO one(i); i:=i^.nxt END;
    gen_condition(l^.r,next,loop);
    start(next);
  ELSE
    loop:=new(); block^.true:=loop;
    start(loop); i:=l^.dw;
    WHILE i#NIL DO one(i); i:=i^.nxt END;
    block^.true:=loop; start(next);
  END;
  l^.adr:=0;
END gen_loop;

PROCEDURE gen_inc_dec(dec: BOOLEAN; l: ref);
  VAR bl,br: box;
BEGIN
  gen_load(l^.l,bl,load_adr);
  IF bl.bm#bm_stk THEN error(l^.l,unrealize) END;
  IF l^.r=NIL THEN
    IF dec THEN put.c(mcd.dec1) ELSE put.c(mcd.inc1) END;
  ELSE
    gen_load(l^.r,br,load_val);
    IF br.bm#bm_stk THEN error(l^.r,unrealize) END;
    IF dec THEN put.c(mcd.dec) ELSE put.c(mcd.inc) END;
  END;
END gen_inc_dec;

PROCEDURE gen_exit(l: ref);
BEGIN
  IF (l^.dw=NIL) OR (l^.dw^.md#tbl.loop) OR (l^.dw^.adr=0) THEN
    error(l,assert);
  END;
  block^.true:=node(l^.dw^.adr);
  start(new());
END gen_exit;

PROCEDURE gen_case(l: ref);
  VAR
    lbs           : ARRAY [0..999] OF INTEGER;
    base,min,max  : INTEGER;
    i,n,m         : INTEGER;
    sel_no        : INTEGER;
  PROCEDURE var_case;
    VAR else,next,else_drop,case: node; k,k1: ref; str: INTEGER;
  BEGIN
    str:=des.new_str(max-min+1);
    move(ADR(des.scode[str]),ADR(lbs[min-base]),max-min+1);
    put.lsa(-min); put.c(mcd.copt); put.li(max-min); put.c(mcd.rchkz);
    else:=new(); next:=new(); else_drop:=new();
    block^.md:=nm_cond; block^.false:=else_drop; block^.true:=new();
    start(block^.true); DEC(put.stk);
    put.lsta(str); put.c(mcd.lxw); put.c(mcd.jump);
    case:=block; case^.spos:=str; case^.slen:=max-min+1;
    case^.md:=nm_case; case^.false:=else;
    NEW(case^.alts,sel_no);
    k1:=l^.l; sel_no:=0;
    WHILE k1#NIL DO
      case^.alts[sel_no]:=new(); start(case^.alts[sel_no]); INC(sel_no);
      k:=k1^.r;
      WHILE k#NIL DO one(k); k:=k^.nxt END;
      block^.true:=next; k1:=k1^.nxt;
    END;
    INC(put.stk); start(else_drop); put.c(mcd.drop);
    block^.true:=else; start(else);
    k:=l^.r;
    IF k=NIL THEN put.li(case_else); put.c(mcd.trap);
    ELSE WHILE k#NIL DO one(k); k:=k^.nxt END;
    END;
    block^.true:=next; start(next);
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
  VAR s,r: ref; b: box; t: trap;
BEGIN
  base:=0;
  LOOP
    save(t);
    min:=MAX(INTEGER); max:=MIN(INTEGER);
    FOR i:=0 TO HIGH(lbs) DO lbs[i]:=-1 END;
    sel_no:=0; s:=l^.l;
    IF s=NIL THEN const_case(0); RETURN END;
    WHILE s#NIL DO
      IF s^.md=tbl.select THEN
        r:=s^.l;
        WHILE r#NIL DO
          IF r^.md=tbl.range THEN
            load_val(r^.l,b); m:=b.imm; n:=b.imm;
            IF b.bm#bm_imm THEN error(r,'case labels must be constant') END;
            IF r^.r#NIL THEN
              load_val(r^.r,b); m:=b.imm;
              IF b.bm#bm_imm THEN error(r,'case labels must be constant') END;
            END;
            IF n<=m THEN
              IF n<min THEN min:=n END; IF m>max THEN max:=m END;
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
    IF max-min>HIGH(lbs) THEN error(l,'too large labels range') END;
    base:=min; restore(t);
  END;
  calc_load(l^.dw,b,load_val);
  IF b.bm=bm_imm THEN const_case(b.imm);
  ELSIF b.bm=bm_stk THEN var_case;
  ELSE error(l^.dw,unrealize);
  END;
END gen_case;

PROCEDURE gen_range(l: ref);
  PROCEDURE make_var(l: ref; VAR a: access);
    VAR b: box; n: INTEGER;
  BEGIN
    calc_load(l,b,load_val);
    IF b.bm=bm_imm THEN
      a.am:=am_imm; a.no:=b.imm;
    ELSIF b.bm=bm_stk THEN
      n:=alloc_var(1); a.disp:=n*32;
      IF level=0 THEN a.am:=am_G; put.sgw(n);
      ELSE a.lvl:=level; a.am:=am_L; put.slw(n);
      END;
    ELSE error(l,unrealize);
    END;
  END make_var;
BEGIN
  l^.adr:=des.new_rng();
  make_var(l^.l,des.rngs[l^.adr].l);
  make_var(l^.r,des.rngs[l^.adr].r);
END gen_range;

PROCEDURE gen_const(l: ref);
  VAR a: access; b,sz: box; t: trap; n: INTEGER;
BEGIN
  l^.adr:=des.new_var();
  IF l^.l=NIL THEN error(l,assert) END;
  IF (l^.l^.md#tbl.array) & (l^.l^.md#tbl.packed_array) &
     (l^.l^.md#tbl.record) & (l^.l^.md#tbl.packed_record) THEN
    save(t);
    calc_load(l^.dw,b,load_val);
    IF b.bm=bm_imm THEN
      des.vars[l^.adr].am:=am_imm;
      des.vars[l^.adr].no:=b.imm;
      RETURN
    ELSIF b.bm=bm_stk THEN
      WITH des.vars[l^.adr] DO
        n:=alloc_var(1); disp:=n*32;
        IF level=0 THEN am:=am_G; put.sgw(n);
        ELSE am:=am_L; lvl:=level; put.slw(n);
        END;
      END;
      RETURN
    END;
    restore(t);
  END;
  des.load_access(l^.dw,des.vars[l^.adr]);
  IF des.vars[l^.adr].am#am_STR THEN error(l,unrealize) END;
END gen_const;

PROCEDURE gen_code_body(l: ref);
  VAR n: INTEGER; i: ref; b: box;
BEGIN
  IF (l^.l=NIL) OR (l^.l^.md#tbl.inline) THEN
    error(l,'inline only for code')
  END;
  n:=0; i:=l^.dw;
  WHILE i#NIL DO INC(n); i:=i^.nxt END;
  RESIZE(des.inln[l^.l^.adr],n);
  n:=0; i:=l^.dw;
  WHILE i#NIL DO
    load_val(i,b);
    IF b.bm#bm_imm THEN error(i,'must be constant') END;
    IF (b.imm<0) OR (b.imm>255) THEN error(i,'must be in range 0..255') END;
    des.inln[l^.l^.adr][n]:=CHAR(b.imm); INC(n); i:=i^.nxt;
  END;
END gen_code_body;

PROCEDURE gen_program_check(l: ref);
  VAR b: box;
BEGIN
  load_val(l^.l,b);
  IF b.bm#bm_imm THEN error(l,'program_check must be constant') END;
  IF b.imm=0 THEN error(l,'something wrong') END;
END gen_program_check;

PROCEDURE gen_halt(l: ref);
  VAR b: box;
BEGIN
  IF l^.r=NIL THEN put.li(halt_trap)
  ELSE
    load_val(l^.r,b); imm?(b);
    CASE b.imm OF
      |0: put.li(halt_trap);
      |1: put.li(crash_trap);
      |2: put.li(abort_trap);
    ELSE error(l^.r,'illegal halt number');
    END;
  END;
  put.c(mcd.trap);
END gen_halt;

PROCEDURE gen_assert(l: ref);
  VAR next,body: node; b: box;
BEGIN
  next:=new(); body:=new();
  gen_condition(l^.l,next,body);
  start(body);
  IF l^.r=NIL THEN put.li(assert_trap);
  ELSE gen_load(l^.r,b,load_val); stk?(b);
  END;
  put.c(mcd.trap);
  block^.true:=next; start(next);
END gen_assert;

PROCEDURE one(l: ref);
BEGIN
  stat_pos:=l^.val;
  CASE l^.md OF
    |tbl.procedure      : gen_proc(l);
    |tbl.inline         : l^.adr:=des.new_inln();
    |tbl.program_check  : gen_program_check(l);
    |tbl.block          : l:=l^.dw; WHILE l#NIL DO one(l); l:=l^.nxt END;
    |tbl.assign         : gen_assign(l);
    |tbl.var            : gen_var(l);
    |tbl.return         : gen_return(l); RETURN
    |tbl.module         : l:=l^.dw; WHILE l#NIL DO one(l); l:=l^.nxt END;
    |tbl.const          : gen_const(l);
    |tbl.array          :
    |tbl.array_of       :
    |tbl.dynarr         :
    |tbl.packed_array   :
    |tbl.packed_array_of:
    |tbl.packed_dynarr  :
    |tbl.number         :
    |tbl.record         : gen_record(l);
    |tbl.packed_record  : gen_record(l);
    |tbl.profile        : gen_profile(l);
    |tbl.call           : gen_call(l,FALSE);
    |tbl.if             : gen_if(l);
    |tbl.case           : gen_case(l);
    |tbl.loop           : gen_loop(l);
    |tbl.exit           : gen_exit(l);
    |tbl.incl           : gen_incl_excl(FALSE,l);
    |tbl.excl           : gen_incl_excl(TRUE,l);
    |tbl.inc            : gen_inc_dec(FALSE,l);
    |tbl.dec            : gen_inc_dec(TRUE,l);
    |tbl.enumeration    :
    |tbl.boolean        :
    |tbl.char           :
    |tbl.range          : gen_range(l);
    |tbl.subtype        : gen_range(l);
    |tbl.integer        :
    |tbl.real           :
    |tbl.set            :
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
  ASSERT(put.stk=0);
  IF put.stk_max>stk_limit THEN error(l,assert) END;
END one;

PROCEDURE save_parms;
  VAR pn,no: INTEGER;
BEGIN
  IF level=0 THEN RETURN END;
  pn:=profile_loc(proc^.l^.l);
  no:=des.prcs[proc^.l^.adr].loc_sz-4;
  put.stk:=pn;
  IF    pn=0 THEN put.enter(no);
  ELSIF pn=1 THEN put.enter(no); put.c(mcd.slw4);
  ELSIF pn=2 THEN put.enter(no); put.c(mcd.slw4); put.c(mcd.slw5);
  ELSE
    put.c(mcd.store); put.stk:=0;
    IF no-pn-1>0 THEN put.enter(no-pn-1) END;
  END;
  ASSERT(put.stk=0);
END save_parms;

PROCEDURE copy_parms;
  PROCEDURE array_of_size(l: ref; n: INTEGER);
    VAR b: box; i: INTEGER;
  BEGIN
    put.llw(n-1);
    IF l^.md=tbl.packed_array_of THEN calc_load(l^.r,b,load_bits);
    ELSE calc_load(l^.r,b,load_32size);
    END;
    imm?(b);
    cmul_round32(1,b.imm);
  END array_of_size;
  VAR l,t: ref; b: box; n: INTEGER;
BEGIN
  IF level=0 THEN RETURN END;
  l:=proc^.l^.l^.dw;
  WHILE l#NIL DO
    IF l^.md#tbl.var THEN one(l);
    ELSE
      t:=l^.l;
      IF t=NIL THEN error(l,assert)
      ELSIF t^.md=tbl.val_prm THEN -- nothing
      ELSIF t^.md=tbl.var_prm THEN -- nothing
      ELSIF array_of?(t) THEN
        n:=des.vars[l^.adr].disp DIV 32;
        IF (n>=0) & (n<100h) THEN
          put.llw(n); array_of_size(t,n); put.c(mcd.pcop); put.b(n);
        ELSE
          array_of_size(t,n);
          put.c(mcd.alloc); put.c(mcd.copt);
          put.llw(n); put.c(mcd.swap); put.slw(n);
          array_of_size(t,n); put.c(mcd.move);
        END;
      ELSIF des.vars[l^.adr].am=am_aL THEN
        n:=des.vars[l^.adr].no;
        IF (n>=0) & (n<100h) THEN
          put.llw(n); gen_load(t,b,load_size);
          IF b.bm#bm_stk THEN error(l,unrealize) END;
          put.c(mcd.pcop); put.b(n);
        ELSE
          gen_load(t,b,load_size);
          IF b.bm#bm_stk THEN error(l,unrealize) END;
          put.c(mcd.alloc); put.c(mcd.copt);
          put.llw(n); put.c(mcd.swap); put.slw(n);
          gen_load(t,b,load_size); put.c(mcd.move);
        END;
      END;
    END;
    l:=l^.nxt;
  END;
  ASSERT(put.stk=0);
END copy_parms;

PROCEDURE proc_body;
  VAR l: ref;
BEGIN
  ASSERT(proc^.md=tbl.proc_body);
  l:=proc^.dw;
  WHILE l#NIL DO one(l); l:=l^.nxt END;
  IF NOT LAST THEN RETURN END;
  IF level=0 THEN
    IF glo_alloc_cnt>=0 THEN put.lgw(glo_alloc_cnt);
    ELSE put.c(mcd.li0);
    END;
    put.c(mcd.rtn); put.stk:=0;
  ELSE
    ASSERT(proc^.l#NIL);
    ASSERT(proc^.l^.l#NIL);
    IF proc^.l^.l^.l#NIL THEN
      put.li(return_without_res); put.c(mcd.trap);
    END;
    put.c(mcd.rtn);
  END;
END proc_body;

PROCEDURE do_flow(): node;
  VAR first,second: node;
BEGIN
  ASSERT(proc#NIL);
  ASSERT(proc^.md=tbl.proc_body);
  IF proc^.l^.md=tbl.module THEN level:=0
  ELSE
    ASSERT(proc^.l^.md=tbl.procedure);
    ASSERT(proc^.l^.l#NIL);
    ASSERT(proc^.l^.l^.md=tbl.profile);
    level:=des.prcs[proc^.l^.adr].lvl
  END;
  stat_pos:=0;
  put.stk:=0; put.stk_max:=0;
  block:=NIL; glo_alloc_cnt:=-1;
  second:=new(); start(second);
  proc_body;
  first:=new(); start(first);
  save_parms;
  copy_parms;
  block^.true:=second;
  finish;
  RETURN first;
END do_flow;

END pcGenExpr.
