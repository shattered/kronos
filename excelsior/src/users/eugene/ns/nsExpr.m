IMPLEMENTATION MODULE nsExpr; (* Sem 01-Dec-90. (c) KRONOS *)

(*$N+*)

IMPORT mcd : defCodes;
IMPORT mem : pcSystem;
IMPORT tbl : pcTab;
IMPORT des : nsDesig;
IMPORT put : nsCmd;

FROM SYSTEM      IMPORT WORD, ADDRESS, ADR;
FROM nsSym       IMPORT access, adr_mode, index_mode;
FROM nsDesig     IMPORT free_regs;
FROM pcTab       IMPORT ref;

WITH STORAGE : mem;

CONST
  assert  = 'assert';
  ill_type= 'illegal type';

VAR
  const         : access;
  reg           : access;
  TOS           : access;
  stat_pos      : INTEGER;

PROCEDURE error(l: ref; s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  IF (l=NIL) OR (l^.md=tbl.procedure) OR (l^.md=tbl.number) THEN
    tbl.error(stat_pos DIV 1000h,stat_pos MOD 1000h,s,x);
  ELSE
    tbl.error(l^.val DIV 1000h,l^.val MOD 1000h,s,x);
  END;
  HALT(1);
END error;

PROCEDURE alloc_fpp_reg(VAR a: access; sz: INTEGER);
  VAR n: INTEGER;
BEGIN
  ASSERT(sz IN {4,8});
  a:=reg;
  WITH fpp_regs DO
    LOOP
      IF stk>=res THEN error(NIL,assert) END;
      n:=stk MOD fpp_regs_no; INC(stk);
      IF sz=4 THEN a.am:=am_FPP4; EXIT END;
      IF NOT ODD(n) THEN
        IF stk>=res THEN error(NIL,assert) END;
        INC(stk); a.am:=am_FPP8; EXIT;
      END;
    END;
    IF stk>max THEN max:=stk END;
  END;
  a.rg:=n;
END alloc_fpp_reg;

PROCEDURE dealloc_fpp_reg(VAL a: access; min: INTEGER);
  VAR n: INTEGER;
BEGIN
  ASSERT(min>=fpp_regs.lim-fpp_regs_no);
  ASSERT(min<=fpp_regs.lim);
  LOOP
    IF fpp_regs.stk<=min THEN RETURN END;
    n:=(fpp_regs.stk-1) MOD fpp_regs_no;
    IF (a.am=am_FPP4) & (a.rg=n) THEN RETURN END;
    IF (a.am=am_FPP8) & (a.rg=n) THEN
      ASSERT(NOT ODD(n));
      INC(fpp_regs.stk);
      RETURN;
    END;
    DEC(fpp_regs.stk);
  END;
END dealloc_fpp_reg;

PROCEDURE new(): node;
  VAR r: node;
BEGIN
  NEW(r);
  WITH r^ DO
    true :=NIL;
    false:=NIL;
    md   :=nm_goto;
    proc :=0;
    NEW(alts);
    NEW(ctbl);
    pos  :=0;
    len  :=0;
    tpos :=0;
    tlen :=0;
    adr  :=0;
    cnt  :=0;
    next :=NIL;
    mark :=FALSE;
  END;
  RETURN r;
END new;

PROCEDURE finish;
BEGIN
  IF block#NIL THEN block^.len:=put.cnt-block^.pos; block:=NIL END;
END finish;

PROCEDURE start(n: node);
BEGIN
  finish; block:=n; block^.pos:=put.cnt;
END start;

PROCEDURE vm(l: ref): val_mode;
  PROCEDURE str?(r: ref): val_mode;
  BEGIN
    IF r^.md=tbl.char    THEN RETURN vm_string END;
    IF r^.md=tbl.subtype THEN RETURN str?(r^.dw) END;
    RETURN vm_undef;
  END str?;
BEGIN
  CASE l^.md OF
    |tbl.enumeration    : RETURN vm_integer;
    |tbl.char           : RETURN vm_cardinal;
    |tbl.boolean        : RETURN vm_boolean;
    |tbl.range          : RETURN vm(l^.dw);
    |tbl.subtype        : RETURN vm(l^.dw);
    |tbl.integer        : RETURN vm_integer;
    |tbl.real           : RETURN vm_real;
    |tbl.set            : RETURN vm_set;
    |tbl.packed_array   : RETURN str?(l^.r);
    |tbl.packed_dynarr  : RETURN str?(l^.r);
    |tbl.packed_array_of: RETURN str?(l^.r);
    |tbl.pointer        : RETURN vm_cardinal;
  ELSE RETURN vm_undef;
  END;
END vm;

PROCEDURE bytes(l: ref): INTEGER;
BEGIN
  CASE l^.md OF
    |tbl.enumeration    : RETURN des.rngs[l^.adr].size;
    |tbl.range          : RETURN des.rngs[l^.adr].size;
    |tbl.subtype        : RETURN des.rngs[l^.adr].size;
    |tbl.char           : RETURN 1;
    |tbl.boolean        : RETURN 1;
    |tbl.integer        : RETURN 4;
    |tbl.real           : RETURN 4;
    |tbl.pointer        : RETURN 4;
  END;
END bytes;

PROCEDURE gen_neg(l: ref; VAR a: access);
  VAR ra,len,min,max,rlen: access; sz: INTEGER; mask: BITSET; sg: BOOLEAN;
BEGIN
  CASE vm(l^.r^.dw) OF
    |vm_integer :
      des.expression(l^.r,ra); sz:=bytes(l^.r^.dw);
      IF des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=-ra.n;
      ELSIF ra.am=am_RG THEN
        put.cmd(put.neg,ra,ra,sz); put.flag; a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.neg,ra,a,sz); put.flag;
      END;
    |vm_real    :
      des.expression(l^.r,ra); sz:=bytes(l^.r^.dw);
      IF (ra.am=am_FPP4) OR (ra.am=am_FPP8) THEN
        put.cmd(put.negf,ra,ra,sz); a:=ra;
      ELSE
        alloc_fpp_reg(a,sz); put.cmd(put.negf,ra,a,sz);
      END;
    |vm_set     :
      des.expression(l^.r,ra); des.bytes(l^.dw,len);
      des.min(l^.dw^.dw,min); des.max(l^.dw^.dw,max);
      des.bytes(l^.dw^.dw,rlen); sg:=des.sign?(l^.dw^.dw);
      IF NOT des.imm_int?(len,TRUE,4) OR
         NOT des.imm_int?(rlen,TRUE,4) OR
         NOT des.imm_int?(min,sg,rlen.n) OR
         NOT des.imm_int?(max,sg,rlen.n) OR
         NOT (len.n IN {1,2,4}) THEN error(l,ill_type) END;
      mask:={0..max.n-min.n};
      IF des.imm_int?(ra,FALSE,len.n) THEN
        a:=const; a.n:=INTEGER(BITSET(ra.n)/mask);
      ELSIF ra.am=am_RG THEN
        const.n:=INTEGER(mask); put.cmd(put.xor,const,ra,len.n); a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,ra,a,len.n);
        const.n:=INTEGER(mask); put.cmd(put.xor,const,a,len.n);
      END;
    |vm_boolean:
      des.expression(l^.r,ra);
      IF des.imm_int?(ra,FALSE,1) THEN
        a:=const; a.n:=ORD(NOT BOOLEAN(ra.n));
      ELSIF ra.am=am_RG THEN
        put.cmd(put.not,ra,ra,1);
      ELSE
        des.alloc_reg(a); put.cmd(put.not,ra,a,1);
      END;
  ELSE error(l,ill_type);
  END;
END gen_neg;

PROCEDURE gen_plus(l: ref; VAR a: access);
  VAR
    la,ra,len: access;
    sz: INTEGER;
    true,false1,false2,next: node;
    c1,c2,b1,b2: BOOLEAN;
BEGIN
  stat_pos:=l^.pos;
  CASE vm(l^.l^.dw) OF
    |vm_integer:
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n+ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.add,ra,la,sz); put.flag; a:=la;
      ELSIF ra.am=am_RG THEN
        put.cmd(put.add,la,ra,sz); put.flag; a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.add,ra,a,sz); put.flag;
      END;
    |vm_cardinal:
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,FALSE,sz) & des.imm_int?(ra,FALSE,sz) THEN
        a:=const; a.n:=la.n+ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.add,ra,la,sz); a:=la;
      ELSIF ra.am=am_RG THEN
        put.cmd(put.add,la,ra,sz); a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz); put.cmd(put.add,ra,a,sz);
      END;
    |vm_real:
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF (sz=4) & des.imm_int?(la,FALSE,4) & des.imm_int?(ra,FALSE,4) THEN
        a:=const; a.n:=INTEGER(REAL(la.n)+REAL(ra.n));
      ELSIF (la.am=am_FPP4) OR (la.am=am_FPP8) THEN
        put.cmd(put.addf,ra,la,sz); a:=la;
      ELSIF (ra.am=am_FPP4) OR (ra.am=am_FPP8) THEN
        put.cmd(put.addf,la,ra,sz); a:=ra;
      ELSE
        alloc_fpp_reg(a,sz); put.cmd(put.movf,la,a,sz);
        put.cmd(put.addf,ra,a,sz);
      END;
    |vm_set:
      des.expression(l^.l,la); des.expression(l^.r,ra); des.bytes(l^.dw,len);
      IF (len.am#am_imm) OR NOT (len.n IN {1,2,4}) THEN error(l,ill_type) END;
      IF des.imm_int?(la,FALSE,len.n) & des.imm_int?(ra,FALSE,len.n) THEN
        a:=const; a.n:=INTEGER(BITSET(la.n)+BITSET(ra.n));
      ELSIF la.am=am_RG THEN
        put.cmd(put.or,ra,la,len.n); a:=la;
      ELSIF ra.am=am_RG THEN
        put.cmd(put.or,la,ra,len.n); a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,len.n);
        put.cmd(put.or,ra,a,len.n);
      END;
    |vm_boolean:
      true:=new(); false1:=new(); false2:=new();
      gen_condition(l^.l,true,false1);
      c1:=block^.md=nm_goto; b1:=block^.true=true;
      start(false1);
      gen_condition(l^.r,true,false2);
      c2:=block^.md=nm_goto; b2:=block^.true=true;
      IF c1 & (b1 OR c2) THEN
        a:=const; a.n:=ORD(b1 OR b2);
        start(true); block^.true:=false2; start(false2);
      ELSE
        des.alloc_reg(a); next:=new();
        start(true); const.n:=1;
        put.cmd(put.mov,const,a,1); block^.true:=next;
        start(false2); const.n:=0;
        put.cmd(put.mov,const,a,1); block^.true:=next;
        start(next);
      END;
  ELSE error(l,ill_type);
  END;
END gen_plus;

PROCEDURE gen_minus2(l: ref; VAR a: access);
  VAR la,ra,len: access; sz: INTEGER;
BEGIN
  stat_pos:=l^.pos;
  CASE vm(l^.l^.dw) OF
    |vm_integer :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n-ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.sub,ra,la,sz); put.flag; a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.sub,ra,a,sz); put.flag;
      END;
    |vm_cardinal:
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,FALSE,sz) & des.imm_int?(ra,FALSE,sz) THEN
        a:=const; a.n:=la.n-ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.sub,ra,la,sz); a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz); put.cmd(put.sub,ra,a,sz);
      END;
    |vm_real    :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF (la.am=am_FPP4) OR (la.am=am_FPP8) THEN
        put.cmd(put.subf,ra,la,sz); a:=la;
      ELSE
        alloc_fpp_reg(a,sz); put.cmd(put.movf,la,a,sz);
        put.cmd(put.subf,ra,a,sz);
      END;
    |vm_set     :
      des.expression(l^.l,la); des.expression(l^.r,ra); des.bytes(l^.dw,len);
      IF (len.am#am_imm) OR NOT (len.n IN {1,2,4}) THEN error(l,ill_type) END;
      IF des.imm_int?(la,FALSE,len.n) & des.imm_int?(ra,FALSE,len.n) THEN
        a:=const; a.n:=INTEGER(BITSET(la.n)-BITSET(ra.n));
      ELSIF la.am=am_RG THEN
        put.cmd(put.bic,ra,la,len.n); a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,len.n);
        put.cmd(put.bic,ra,a,len.n);
      END;
  ELSE error(l,ill_type);
  END;
END gen_minus2;

PROCEDURE gen_mul(l: ref; VAR a: access);
  VAR
    la,ra,len: access;
    sz: INTEGER;
    true1,true2,false,next: node;
    c1,c2,b1,b2: BOOLEAN;
BEGIN
  CASE vm(l^.l^.dw) OF
    |vm_integer :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n*ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.mul,ra,la,sz); put.flag; a:=la;
      ELSIF ra.am=am_RG THEN
        put.cmd(put.mul,la,ra,sz); put.flag; a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.mul,ra,a,sz); put.flag;
      END;
    |vm_real    :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF (la.am=am_FPP4) OR (la.am=am_FPP8) THEN
        put.cmd(put.mulf,ra,la,sz); a:=la;
      ELSIF (ra.am=am_FPP4) OR (ra.am=am_FPP8) THEN
        put.cmd(put.mulf,la,ra,sz); a:=ra;
      ELSE
        alloc_fpp_reg(a,sz); put.cmd(put.movf,la,a,sz);
        put.cmd(put.mulf,ra,a,sz);
      END;
    |vm_set     :
      des.expression(l^.l,la); des.expression(l^.r,ra); des.bytes(l^.dw,len);
      IF (len.am#am_imm) OR NOT (len.n IN {1,2,4}) THEN error(l,ill_type) END;
      IF des.imm_int?(la,FALSE,len.n) & des.imm_int?(ra,FALSE,len.n) THEN
        a:=const; a.n:=INTEGER(BITSET(la.n)*BITSET(ra.n));
      ELSIF la.am=am_RG THEN
        put.cmd(put.and,ra,la,len.n); a:=la;
      ELSIF ra.am=am_RG THEN
        put.cmd(put.and,la,ra,len.n); a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,len.n);
        put.cmd(put.and,ra,a,len.n);
      END;
    |vm_boolean:
      true1:=new(); true2:=new(); false:=new();
      gen_condition(l^.l,true1,false);
      c1:=block^.md=nm_goto; b1:=block^.true=true1;
      start(true1);
      gen_condition(l^.r,true2,false);
      c2:=block^.md=nm_goto; b2:=block^.true=true2;
      IF c1 & (NOT b1 OR c2) THEN
        a:=const; a.n:=ORD(b1 & b2);
        start(true2); block^.true:=false; start(false);
      ELSE
        des.alloc_reg(a); next:=new();
        start(true2); const.n:=1;
        put.cmd(put.mov,const,a,1); block^.true:=next;
        start(false); const.n:=0;
        put.cmd(put.mov,const,a,1); block^.true:=next;
        start(next);
      END;
  ELSE error(l,ill_type);
  END;
END gen_mul;

PROCEDURE gen_div(l: ref; VAR a: access);
  VAR la,ra: access; sz: INTEGER;
BEGIN
  CASE vm(l^.l^.dw) OF
    |vm_integer :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n DIV ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.div,ra,la,sz); put.flag; a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.div,ra,a,sz); put.flag;
      END;
  ELSE error(l,ill_type);
  END;
END gen_div;

PROCEDURE gen_mod(l: ref; VAR a: access);
  VAR la,ra: access; sz: INTEGER;
BEGIN
  CASE vm(l^.l^.dw) OF
    |vm_integer :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n MOD ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.mod,ra,la,sz); put.flag; a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.mod,ra,a,sz); put.flag;
      END;
  ELSE error(l,ill_type);
  END;
END gen_mod;

PROCEDURE gen_rot(l: ref; VAR a: access; right: BOOLEAN);
  VAR sz,ra: access; i: INTEGER; sg,rc: BOOLEAN;
BEGIN
  des.expression(l^.l,a); des.bytes_u(l^.l^.dw,sz); sg:=des.sign?(l^.l^.dw);
  IF (sz.am#am_imm) OR NOT (sz.n IN {1,2,4}) THEN error(l^.l,ill_type) END;
  des.expression(l^.r,ra); rc:=des.imm_int?(ra,FALSE,1);
  IF NOT right THEN
  ELSIF rc THEN ra.n:=-ra.n
  ELSIF ra.am=am_RG THEN put.cmd(put.neg,ra,ra,1)
  ELSE des.alloc_reg(reg); put.cmd(put.neg,ra,reg,1); ra:=reg;
  END;
  IF des.imm_int?(a,sg,sz.n) & rc THEN
    ra.n:=ra.n MOD (sz.n*8);
    FOR i:=1 TO ra.n DO
      a.n:=a.n<<1;
      IF sz.n=4 THEN
      ELSIF (sz.n*8) IN BITSET(a.n) THEN a.n:=INTEGER(BITSET(a.n)+{0})
      ELSE a.n:=INTEGER(BITSET(a.n)-{0})
      END;
    END;
    IF sg & ((sz.n*8-1) IN BITSET(a.n)) THEN
      a.n:=INTEGER(BITSET(a.n)+{(sz.n*8-1)..31})
    ELSE
      a.n:=INTEGER(BITSET(a.n)-{(sz.n*8-1)..31})
    END
  ELSIF a.am=am_RG THEN
    put.cmd(put.rot,ra,a,sz.n);
  ELSE
    des.alloc_reg(reg); put.cmd(put.mov,a,reg,sz.n); a:=reg;
    put.cmd(put.rot,ra,a,sz.n);
  END;
END gen_rot;

PROCEDURE gen_slash(l: ref; VAR a: access);
  VAR la,ra,len: access; sz: INTEGER;
BEGIN
  CASE vm(l^.l^.dw) OF
    |vm_integer :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n / ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.quo,ra,la,sz); a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.quo,ra,a,sz);
      END;
    |vm_real    :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF (sz=4) & des.imm_int?(la,FALSE,4) & des.imm_int?(ra,FALSE,4) THEN
        a:=const; a.n:=INTEGER(REAL(la.n)/REAL(ra.n));
      ELSIF (la.am=am_FPP4) OR (la.am=am_FPP8) THEN
        put.cmd(put.divf,ra,la,sz); a:=la;
      ELSE
        alloc_fpp_reg(a,sz); put.cmd(put.movf,a,a,sz);
        put.cmd(put.divf,ra,a,sz);
      END;
    |vm_set     :
      des.expression(l^.l,la); des.expression(l^.r,ra); des.bytes(l^.dw,len);
      IF (len.am#am_imm) OR NOT (len.n IN {1,2,4}) THEN error(l,ill_type) END;
      IF des.imm_int?(la,FALSE,len.n) & des.imm_int?(ra,FALSE,len.n) THEN
        a:=const; a.n:=INTEGER(BITSET(la.n)/BITSET(ra.n));
      ELSIF la.am=am_RG THEN
        put.cmd(put.xor,ra,la,len.n); a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,len.n);
        put.cmd(put.xor,ra,a,len.n);
      END;
  ELSE error(l,ill_type);
  END;
END gen_slash;

PROCEDURE gen_rem(l: ref; VAR a: access);
  VAR la,ra: access; sz: INTEGER;
BEGIN
  CASE vm(l^.l^.dw) OF
    |vm_integer :
      des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
      IF des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=la.n REM ra.n;
      ELSIF la.am=am_RG THEN
        put.cmd(put.rem,ra,la,sz); put.flag; a:=la;
      ELSE
        des.alloc_reg(a); put.cmd(put.mov,la,a,sz);
        put.cmd(put.rem,ra,a,sz); put.flag;
      END;
  ELSE error(l,ill_type);
  END;
END gen_rem;

PROCEDURE gen_abs(l: ref; VAR a: access);
  VAR ra: access; sz: INTEGER;
BEGIN
  CASE vm(l^.r^.dw) OF
    |vm_integer :
      des.expression(l^.r,ra); sz:=bytes(l^.r^.dw);
      IF des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=ABS(ra.n);
      ELSIF ra.am=am_RG THEN
        put.cmd(put.abs,ra,ra,sz); put.flag; a:=ra;
      ELSE
        des.alloc_reg(a); put.cmd(put.abs,ra,a,sz); put.flag;
      END;
    |vm_real    :
      des.expression(l^.r,ra); sz:=bytes(l^.r^.dw);
      IF (sz=4) & des.imm_int?(ra,FALSE,4) THEN
        a:=const; a.n:=INTEGER(ABS(REAL(ra.n)));
      ELSIF (ra.am=am_FPP4) OR (ra.am=am_FPP8) THEN
        put.cmd(put.absf,ra,ra,sz); a:=ra;
      ELSE
        alloc_fpp_reg(a,sz); put.cmd(put.absf,ra,a,sz);
      END;
  ELSE error(l,ill_type);
  END;
END gen_abs;

PROCEDURE gen_float(l: ref; VAR a: access);
  VAR ra: access; sz,fsz: INTEGER;
BEGIN
  CASE vm(l^.r^.dw) OF
    |vm_integer :
      des.expression(l^.r,ra); sz:=bytes(l^.r^.dw); fsz:=bytes(l^.dw);
      IF (fsz=4) & des.imm_int?(ra,TRUE,sz) THEN
        a:=const; a.n:=INTEGER(FLOAT(ra.n));
      ELSE
        alloc_fpp_reg(a,fsz); put.movif(ra,a,sz,fsz);
      END;
  ELSE error(l^.r,'must be integer type');
  END;
END gen_float;

PROCEDURE gen_trunc(l: ref; VAR a: access);
  VAR ra: access; sz,fsz: INTEGER;
BEGIN
  CASE vm(l^.r^.dw) OF
    |vm_real:
      des.expression(l^.r,ra); sz:=bytes(l^.dw); fsz:=bytes(l^.r^.dw);
      IF (fsz=4) & des.imm_int?(ra,FALSE,4) THEN
        a:=const; a.n:=TRUNC(REAL(ra.n));
      ELSE
        des.alloc_reg(a); put.trunc(ra,a,fsz,sz);
      END;
  ELSE error(l^.r,'must be real type');
  END;
END gen_trunc;

PROCEDURE gen_minus(l: ref; VAR a: access);
BEGIN
  IF l^.l=NIL THEN gen_neg(l,a) ELSE gen_minus2(l,a) END;
END gen_minus;

PROCEDURE gen_flags_string(l: ref; VAR flag: put.condition; VAR c,v: BOOLEAN);
  VAR rgs: des.regs; rset: BITSET; stk,i: INTEGER; VAR a: access;
BEGIN
  c:=FALSE;
  rgs:=free_regs; rset:={0..des.regs_no-1};
  FOR i:=rgs.stk TO rgs.lim-1 DO rset:=rset/{i MOD des.regs_no} END;
  WHILE free_regs.stk MOD des.regs_no # 0 DO INC(free_regs.stk) END;
  IF rset*{0,1,2}#{} THEN
    put.save(rset);
    free_regs.lim:=free_regs.stk+des.regs_no;
    free_regs.res:=free_regs.stk+des.regs_no;
  ELSE rset:={};
  END;
  const.n:=-1; reg.rg:=0;
  put.cmd(put.mov,const,reg,4);
  INC(free_regs.stk); stk:=free_regs.stk;
  des.adr_u(l^.l,a);
  IF (a.am#am_RG) OR (a.rg#1) THEN
    reg.rg:=1; put.cmd(put.mov,a,reg,4);
  END;
  INC(stk); free_regs.stk:=stk;
  des.adr_u(l^.r,a);
  IF (a.am#am_RG) OR (a.rg#2) THEN
    reg.rg:=2; put.cmd(put.mov,a,reg,4);
  END;
  put.cmps(1,FALSE,FALSE,FALSE);
  IF rset#{} THEN
    IF free_regs.max>free_regs.res THEN
      error(l,'registers overflow')
    END;
    put.restore(rset);
  END;
  free_regs:=rgs;
END gen_flags_string;

PROCEDURE gen_flags_set(l: ref; VAR c: put.condition; VAR s,v: BOOLEAN);
  VAR la,ra,len: access;
BEGIN
  des.expression(l^.l,la); des.expression(l^.r,ra); des.bytes(l^.dw,len);
  IF (len.am#am_imm) OR NOT (len.n IN {1,2,4}) THEN error(l,ill_type) END;
  s:=des.imm_int?(la,FALSE,len.n) & des.imm_int?(ra,FALSE,len.n);
  IF s THEN
    CASE c OF
      |put.NE: v:=la.n#ra.n;
      |put.EQ: v:=la.n=ra.n;
      |put.LS: v:=BITSET(la.n)<=BITSET(ra.n);
      |put.HS: v:=BITSET(la.n)>=BITSET(ra.n);
    ELSE error(l,'illegal operation for set');
    END;
  ELSE
    CASE c OF
      |put.NE: put.cmd(put.cmp,la,ra,len.n);
      |put.EQ: put.cmd(put.cmp,la,ra,len.n);
      |put.LS:
        IF la.am#am_RG THEN
          des.alloc_reg(reg); put.cmd(put.mov,la,reg,len.n); la:=reg;
        END;
        put.cmd(put.bic,ra,la,len.n); const.n:=0;
        put.cmd(put.cmp,const,la,len.n); c:=put.EQ;
      |put.HS:
        IF ra.am#am_RG THEN
          des.alloc_reg(reg); put.cmd(put.mov,ra,reg,len.n); ra:=reg;
        END;
        put.cmd(put.bic,la,ra,len.n); const.n:=0;
        put.cmd(put.cmp,const,ra,len.n); c:=put.EQ;
    ELSE error(l,'illegal operation for set');
    END;
  END;
END gen_flags_set;

PROCEDURE gen_flags_integer(l: ref; VAR c: put.condition; VAR s,v: BOOLEAN);
  VAR la,ra: access; sz: INTEGER;
BEGIN
  des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
  s:=des.imm_int?(la,TRUE,sz) & des.imm_int?(ra,TRUE,sz);
  IF s THEN
    CASE c OF
      |put.NE: v:=la.n#ra.n;
      |put.EQ: v:=la.n=ra.n;
      |put.LO: v:=la.n<ra.n;
      |put.HI: v:=la.n>ra.n;
      |put.LS: v:=la.n<=ra.n;
      |put.HS: v:=la.n>=ra.n;
    END;
  ELSE
    put.cmd(put.cmp,la,ra,sz);
    CASE c OF
      |put.NE:
      |put.EQ:
      |put.LO: c:=put.LT;
      |put.HI: c:=put.GT;
      |put.LS: c:=put.LE;
      |put.HS: c:=put.GE;
    END;
  END;
END gen_flags_integer;

PROCEDURE gen_flags_real(l: ref; VAR c: put.condition; VAR s,v: BOOLEAN);
  VAR la,ra: access; sz: INTEGER;
BEGIN
  des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
  s:=(sz=4) & des.imm_int?(la,FALSE,4) & des.imm_int?(ra,FALSE,4);
  IF s THEN
    CASE c OF
      |put.NE: v:=REAL(la.n)#REAL(ra.n);
      |put.EQ: v:=REAL(la.n)=REAL(ra.n);
      |put.LO: v:=REAL(la.n)<REAL(ra.n);
      |put.HI: v:=REAL(la.n)>REAL(ra.n);
      |put.LS: v:=REAL(la.n)<=REAL(ra.n);
      |put.HS: v:=REAL(la.n)>=REAL(ra.n);
    END;
  ELSE
    put.cmd(put.cmpf,la,ra,sz);
    CASE c OF
      |put.NE:
      |put.EQ:
      |put.LO: c:=put.LT;
      |put.HI: c:=put.GT;
      |put.LS: c:=put.LE;
      |put.HS: c:=put.GE;
    END;
  END;
END gen_flags_real;

PROCEDURE gen_flags_car(l: ref; VAR c: put.condition; VAR s,v: BOOLEAN);
  VAR la,ra: access; sz: INTEGER;
BEGIN
  des.expression(l^.l,la); des.expression(l^.r,ra); sz:=bytes(l^.l^.dw);
  s:=des.imm_int?(la,FALSE,sz) & des.imm_int?(ra,FALSE,sz);
  IF s THEN
    CASE c OF
      |put.NE: v:=la.n#ra.n;
      |put.EQ: v:=la.n=ra.n;
      |put.LO: v:=la.n<ra.n;
      |put.HI: v:=la.n>ra.n;
      |put.LS: v:=la.n<=ra.n;
      |put.HS: v:=la.n>=ra.n;
    END;
  ELSE
    put.cmd(put.cmp,la,ra,sz);
  END;
END gen_flags_car;

PROCEDURE gen_flags(l: ref; VAR c: put.condition; VAR s,v: BOOLEAN);
  VAR r_stk,f_stk: INTEGER;
BEGIN
  r_stk:=free_regs.stk; f_stk:=fpp_regs.stk;
  CASE vm(l^.l^.dw) OF
    |vm_integer : gen_flags_integer (l,c,s,v);
    |vm_real    : gen_flags_real    (l,c,s,v);
    |vm_cardinal: gen_flags_car     (l,c,s,v);
    |vm_boolean : gen_flags_car     (l,c,s,v);
    |vm_set     : gen_flags_set     (l,c,s,v);
    |vm_string  : gen_flags_string  (l,c,s,v);
  ELSE error(l,ill_type);
  END;
  free_regs.stk:=r_stk; fpp_regs.stk:=f_stk;
END gen_flags;

PROCEDURE gen_compare(l: ref; VAR a: access; c: put.condition);
  VAR s,v: BOOLEAN;
BEGIN
  gen_flags(l,c,s,v);
  IF s THEN a:=const; a.n:=ORD(v);
  ELSE des.alloc_reg(a); put.s(c,a,1);
  END;
END gen_compare;

PROCEDURE gen_in(u: ref; VAR a: access);
  VAR
    la,ra,bnd: access; sg: BOOLEAN; sz: INTEGER;
    true,false,next: node;
BEGIN
  IF vm(u^.r^.dw)#vm_set THEN error(u^.r,ill_type) END;
  des.expression(u^.l,la); des.expression(u^.r,ra);
  sz:=bytes(u^.l^.dw); sg:=des.sign?(u^.l^.dw); des.sub_base(la,sg,sz);
  bnd:=des.rngs[u^.r^.dw^.adr].r; des.gb(bnd);
  IF la.am=am_RG THEN a:=la ELSE des.alloc_reg(a) END;
  put.check(bnd,la,a.rg,sz);
  next:=new(); true:=new(); false:=new();
  block^.md:=nm_cond; block^.flag:=put.FC;
  block^.true:=true; block^.false:=false;
  start(true); put.cmd(put.tbit,a,ra,sz); put.s(put.FS,a,1); block^.true:=next;
  start(false); const.n:=0; put.cmd(put.mov,const,a,1); block^.true:=next;
  start(next);
END gen_in;

PROCEDURE gen_size_check(l: ref; VAR a: access);
  PROCEDURE int_int;
    VAR sz1,sz2: INTEGER;
  BEGIN
    des.expression(l^.r,a); sz1:=bytes(l^.r^.dw);
    IF des.imm_int?(a,TRUE,sz1) THEN RETURN END;
    sz2:=bytes(l^.dw);
    IF sz2<=sz1 THEN RETURN END;
    IF a.am=am_RG THEN put.movx(a,a,sz1,sz2)
    ELSE des.alloc_reg(reg); put.movx(a,reg,sz1,sz2); a:=reg;
    END;
  END int_int;
  PROCEDURE int_car;
    VAR sz1,sz2: INTEGER;
  BEGIN
    des.expression(l^.r,a); sz1:=bytes(l^.r^.dw);
    IF des.imm_int?(a,TRUE,sz1) THEN RETURN END;
    sz2:=bytes(l^.dw);
    IF sz2<=sz1 THEN RETURN END;
    IF a.am=am_RG THEN put.movx(a,a,sz1,sz2)
    ELSE des.alloc_reg(reg); put.movx(a,reg,sz1,sz2); a:=reg;
    END;
  END int_car;
  PROCEDURE int_set;
    VAR sz1: INTEGER; sz2: access;
  BEGIN
    des.expression(l^.r,a); sz1:=bytes(l^.r^.dw);
    IF des.imm_int?(a,TRUE,sz1) THEN RETURN END;
    des.bytes(l^.dw,sz2);
    IF (sz2.am#am_imm) OR NOT (sz2.n IN {1,2,4}) THEN error(l,ill_type) END;
    IF sz2.n<=sz1 THEN RETURN END;
    IF a.am=am_RG THEN put.movx(a,a,sz1,sz2.n)
    ELSE des.alloc_reg(reg); put.movx(a,reg,sz1,sz2.n); a:=reg;
    END;
  END int_set;
  PROCEDURE car_int;
    VAR sz1,sz2: INTEGER;
  BEGIN
    des.expression(l^.r,a); sz1:=bytes(l^.r^.dw);
    IF des.imm_int?(a,FALSE,sz1) THEN RETURN END;
    sz2:=bytes(l^.dw);
    IF sz2<=sz1 THEN RETURN END;
    IF a.am=am_RG THEN put.movz(a,a,sz1,sz2)
    ELSE des.alloc_reg(reg); put.movz(a,reg,sz1,sz2); a:=reg;
    END;
  END car_int;
  PROCEDURE car_set;
    VAR sz1: INTEGER; sz2: access;
  BEGIN
    des.expression(l^.r,a); sz1:=bytes(l^.r^.dw);
    IF des.imm_int?(a,FALSE,sz1) THEN RETURN END;
    des.bytes(l^.dw,sz2);
    IF (sz2.am#am_imm) OR NOT (sz2.n IN {1,2,4}) THEN error(l,ill_type) END;
    IF sz2.n<=sz1 THEN RETURN END;
    IF a.am=am_RG THEN put.movz(a,a,sz1,sz2.n)
    ELSE des.alloc_reg(reg); put.movz(a,reg,sz1,sz2.n); a:=reg;
    END;
  END car_set;
  PROCEDURE car_car;
    VAR sz1,sz2: INTEGER;
  BEGIN
    des.expression(l^.r,a); sz1:=bytes(l^.r^.dw);
    IF des.imm_int?(a,FALSE,sz1) THEN RETURN END;
    sz2:=bytes(l^.dw);
    IF sz2<=sz1 THEN RETURN END;
    IF a.am=am_RG THEN put.movz(a,a,sz1,sz2)
    ELSE des.alloc_reg(reg); put.movz(a,reg,sz1,sz2); a:=reg;
    END;
  END car_car;
  PROCEDURE set_int_car;
    VAR sz2: INTEGER; sz1: access;
  BEGIN
    des.expression(l^.r,a); des.bytes(l^.r^.dw,sz1);
    IF (sz1.am#am_imm) OR NOT (sz1.n IN {1,2,4}) THEN error(l,ill_type) END;
    IF des.imm_int?(a,FALSE,sz1.n) THEN RETURN END;
    sz2:=bytes(l^.dw);
    IF sz2<=sz1.n THEN RETURN END;
    IF a.am=am_RG THEN put.movz(a,a,sz1.n,sz2)
    ELSE des.alloc_reg(reg); put.movz(a,reg,sz1.n,sz2); a:=reg;
    END;
  END set_int_car;
  PROCEDURE set_set;
    VAR sz1,sz2: access;
  BEGIN
    des.expression(l^.r,a); des.bytes(l^.r^.dw,sz1);
    IF (sz1.am#am_imm) OR NOT (sz1.n IN {1,2,4}) THEN error(l,ill_type) END;
    IF des.imm_int?(a,FALSE,sz1.n) THEN RETURN END;
    des.bytes(l^.dw,sz2);
    IF (sz2.am#am_imm) OR NOT (sz2.n IN {1,2,4}) THEN error(l,ill_type) END;
    IF sz2.n<=sz1.n THEN RETURN END;
    IF a.am=am_RG THEN put.movz(a,a,sz1.n,sz2.n)
    ELSE des.alloc_reg(reg); put.movz(a,reg,sz1.n,sz2.n); a:=reg;
    END;
  END set_set;
  PROCEDURE check;
    VAR sz1,sz2: access; b: access;
  BEGIN
    des.expression(l^.r,a);
    des.bytes_u(l^.r^.dw,sz1);
    des.bytes_u(l^.dw,sz2);
    IF (sz1.am#am_imm) OR (sz2.am#am_imm) THEN error(l,ill_type) END;
    IF sz1.n#sz2.n THEN error(l,'illegal type conversion') END;
    IF (vm(l^.dw)=vm_real) & (vm(l^.r^.dw)#vm_real) & (a.am=am_RG) THEN
      des.alloc_var(b,sz1.n); put.cmd(put.mov,a,b,sz1.n); a:=b;
    ELSIF (vm(l^.r^.dw)=vm_real) & (vm(l^.dw)#vm_real) &
          ((a.am=am_FPP4) OR (a.am=am_FPP8)) THEN
      des.alloc_var(b,sz1.n); put.cmd(put.movf,a,b,sz1.n); a:=b;
    END;
  END check;
BEGIN
  CASE vm(l^.r^.dw) OF
    |vm_integer :
      CASE vm(l^.dw) OF
        |vm_integer : int_int;
        |vm_cardinal: int_car;
        |vm_boolean : int_car;
        |vm_set     : int_set;
      ELSE check;
      END;
    |vm_cardinal,vm_boolean:
      CASE vm(l^.dw) OF
        |vm_integer : car_int;
        |vm_cardinal: car_car;
        |vm_boolean : car_car;
        |vm_set     : car_set;
      ELSE check;
      END;
    |vm_set:
      CASE vm(l^.dw) OF
        |vm_integer : set_int_car;
        |vm_cardinal: set_int_car;
        |vm_boolean : set_int_car;
        |vm_set     : set_set;
      ELSE check;
      END;
  ELSE check;
  END;
END gen_size_check;

PROCEDURE gen_const_check(l: ref; VAR a: access);
  VAR ok: BOOLEAN;
BEGIN
  expression(l^.r,a);
  CASE vm(l^.dw) OF
    |vm_integer : ok:=des.imm_int?(a,TRUE,bytes(l^.dw));
    |vm_cardinal: ok:=des.imm_int?(a,FALSE,bytes(l^.dw));
    |vm_boolean : ok:=des.imm_int?(a,FALSE,1);
  ELSE ok:=FALSE;
  END;
  IF ok THEN RETURN END;
  error(l,'must be constant');
END gen_const_check;

PROCEDURE gen_range_check(l: ref; VAR a: access);
BEGIN
HALT(1);
END gen_range_check;

PROCEDURE gen_odd(l: ref; VAR a: access);
BEGIN
  IF vm(l^.dw)#vm_boolean THEN error(l,ill_type) END;
  expression(l^.r,a); const.n:=1;
  IF a.am=am_RG THEN
    put.cmd(put.and,const,a,1);
  ELSE
    des.alloc_reg(reg); put.cmd(put.mov,a,reg,1);
    put.cmd(put.and,const,reg,1); a:=reg;
  END;
END gen_odd;

PROCEDURE gen_cap(l: ref; VAR a: access);
BEGIN
  IF l^.dw^.md#tbl.char THEN error(l,ill_type) END;
  expression(l^.r,a); const.n:=20h;
  IF a.am=am_RG THEN
    put.cmd(put.bic,const,a,1);
  ELSE
    des.alloc_reg(reg); put.cmd(put.mov,a,reg,1);
    put.cmd(put.bic,const,reg,1); a:=reg;
  END;
END gen_cap;

PROCEDURE gen_not(l: ref; VAR a: access);
BEGIN
  IF vm(l^.dw)#vm_boolean THEN error(l,ill_type) END;
  expression(l^.r,a);
  IF a.am=am_RG THEN
    put.cmd(put.not,a,a,1);
  ELSE
    des.alloc_reg(reg); put.cmd(put.not,a,reg,1); a:=reg;
  END;
END gen_not;

PROCEDURE gen_worder(l: ref; VAR a: access);
  VAR sz: INTEGER; sa: access;
BEGIN
  CASE vm(l^.r^.dw) OF
    |vm_integer :
      expression(l^.r,a); sz:=bytes(l^.r^.dw);
      IF NOT des.imm_int?(a,TRUE,sz) & (sz#4) THEN
        IF a.am=am_RG THEN
          put.movx(a,a,sz,4);
        ELSE
          des.alloc_reg(reg); put.movx(a,reg,sz,4); a:=reg;
        END;
      END;
    |vm_cardinal,vm_boolean:
      expression(l^.r,a); sz:=bytes(l^.r^.dw);
      IF NOT des.imm_int?(a,FALSE,sz) & (sz#4) THEN
        IF a.am=am_RG THEN
          put.movz(a,a,sz,4);
        ELSE
          des.alloc_reg(reg); put.movz(a,reg,sz,4); a:=reg;
        END;
      END;
    |vm_real    :
      sz:=bytes(l^.r^.dw);
      IF sz=8 THEN
        des.adr_u(l^.r,a);
      ELSE
        expression(l^.r,a);
        IF NOT des.imm_int?(a,FALSE,4) THEN
          IF a.am=am_FPP4 THEN put.cmd(put.movf,a,TOS,4); a:=TOS END;
          des.alloc_reg(reg); put.cmd(put.mov,a,reg,4); a:=reg;
        END;
      END;
    |vm_set     :
      des.bytes(l^.r^.dw,sa);
      IF (sa.am=am_imm) & (sa.n IN {1,2,4}) THEN
        expression(l^.r,a);
        IF NOT des.imm_int?(a,FALSE,sa.n) & (sa.n#4) THEN
          IF a.am=am_RG THEN
            put.movz(a,a,sa.n,4);
          ELSE
            des.alloc_reg(reg); put.movz(a,reg,sa.n,4); a:=reg;
          END;
        END;
      ELSE
        des.adr_u(l^.r,a);
      END;
  ELSE
    des.adr_u(l^.r,a);
  END;
END gen_worder;

PROCEDURE expression(l: ref; VAR a: access);
  VAR stk,fpp: INTEGER;
BEGIN
  stk:=free_regs.stk; fpp:=fpp_regs.stk;
  IF l=NIL THEN error(NIL,assert) END;
  IF l^.md#tbl.number THEN stat_pos:=l^.pos END;
  CASE l^.md OF
    |tbl.call         : gen_call(l,TRUE,a);
    |tbl.size_check   : gen_size_check(l,a);
    |tbl.number       : a:=const; a.n:=l^.val;
    |tbl.nil          : a:=const; a.n:=0;
    |tbl.minus        : gen_minus  (l,a);
    |tbl.plus         : gen_plus   (l,a);
    |tbl.star         : gen_mul    (l,a);
    |tbl.div          : gen_div    (l,a);
    |tbl.mod          : gen_mod    (l,a);
    |tbl.slash        : gen_slash  (l,a);
    |tbl.rem          : gen_rem    (l,a);
    |tbl.abs          : gen_abs    (l,a);
    |tbl.float        : gen_float  (l,a);
    |tbl.trunc        : gen_trunc  (l,a);
    |tbl.rol          : gen_rot    (l,a,FALSE);
    |tbl.ror          : gen_rot    (l,a,TRUE);
    |tbl.less         : gen_compare(l,a,put.LO);
    |tbl.less_equal   : gen_compare(l,a,put.LS);
    |tbl.greater      : gen_compare(l,a,put.HI);
    |tbl.greater_equal: gen_compare(l,a,put.HS);
    |tbl.equal        : gen_compare(l,a,put.EQ);
    |tbl.inequality   : gen_compare(l,a,put.NE);
    |tbl.in           : gen_in     (l,a);
    |tbl.adr          : des.adr_u  (l^.r,a);
    |tbl.min          : des.min    (l^.r,a);
    |tbl.max          : des.max    (l^.r,a);
    |tbl.low          : des.low    (l^.r,a);
    |tbl.high         : des.high   (l^.r,a);
    |tbl.len          : des.len    (l^.r,a);
    |tbl.const_check  : gen_const_check(l,a);
    |tbl.range_check  : gen_range_check(l,a);
    |tbl.odd          : gen_odd    (l,a);
    |tbl.cap          : gen_cap    (l,a);
    |tbl.not          : gen_not    (l,a);
    |tbl.worder       : gen_worder (l,a);
    |tbl.size         : des.bytes_u(l^.r,a);
    |tbl.bytes        : des.bytes_u(l^.r,a);
    |tbl.bits         :
      des.bytes_u(l^.r^.dw,a);
      IF a.am=am_imm THEN a.n:=a.n*8
      ELSIF a.am=am_RG THEN
        const.n:=8; put.cmd(put.mul,const,a,4);
      ELSE
        des.alloc_reg(reg); put.cmd(put.mov,a,reg,4); a:=reg;
        const.n:=8; put.cmd(put.mul,const,a,4);
      END;
  ELSE des.designator_u(l,a);
  END;
  des.dealloc_reg(a,stk); dealloc_fpp_reg(a,fpp);
END expression;

PROCEDURE gen_in_condition(u: ref; then,else: node);
  VAR la,ra,a,bnd: access; sg: BOOLEAN; sz: INTEGER; tst: node;
BEGIN
  tst:=new();
  IF vm(u^.r^.dw)#vm_set THEN error(u^.r,ill_type) END;
  des.expression(u^.l,la); des.expression(u^.r,ra);
  sz:=bytes(u^.l^.dw); sg:=des.sign?(u^.l^.dw);
  des.sub_base(la,sg,sz);
  bnd:=des.rngs[u^.r^.dw^.adr].r; des.gb(bnd);
  IF la.am=am_RG THEN a:=la ELSE des.alloc_reg(a) END;
  put.check(bnd,la,a.rg,sz);
  block^.md:=nm_cond; block^.flag:=put.FC;
  block^.true:=tst; block^.false:=else;
  start(tst);
  put.cmd(put.tbit,a,ra,sz);
  block^.md:=nm_cond; block^.flag:=put.FS;
  block^.true:=then; block^.false:=else;
END gen_in_condition;

PROCEDURE gen_compare_condition(l: ref; c: put.condition; then,else: node);
  VAR s,v: BOOLEAN;
BEGIN
  gen_flags(l,c,s,v);
  IF s THEN
    IF v THEN block^.true:=then ELSE block^.false:=else END;
  ELSE
    block^.md:=nm_cond;
    block^.true:=then;
    block^.false:=else;
    block^.flag:=c;
  END;
END gen_compare_condition;

PROCEDURE gen_condition(cond: ref; then,else: node);
  VAR h: node; b: access; r_stk,f_stk: INTEGER;
BEGIN
  IF cond=NIL THEN error(cond,assert) END;
  IF vm(cond^.dw)#vm_boolean THEN error(cond,assert) END;
  r_stk:=free_regs.stk; f_stk:=fpp_regs.stk;
  CASE cond^.md OF
    |tbl.not          : gen_condition(cond^.r,else,then);
    |tbl.plus         : h:=new(); gen_condition(cond^.l,then,h);
                        start(h); gen_condition(cond^.r,then,else);
    |tbl.star         : h:=new(); gen_condition(cond^.l,h,else);
                        start(h); gen_condition(cond^.r,then,else);
    |tbl.less         : gen_compare_condition(cond,put.LO,then,else);
    |tbl.less_equal   : gen_compare_condition(cond,put.LS,then,else);
    |tbl.greater      : gen_compare_condition(cond,put.HI,then,else);
    |tbl.greater_equal: gen_compare_condition(cond,put.HS,then,else);
    |tbl.equal        : gen_compare_condition(cond,put.EQ,then,else);
    |tbl.inequality   : gen_compare_condition(cond,put.NE,then,else);
    |tbl.in           : gen_in_condition(cond,then,else);
  ELSE
    expression(cond,b);
    IF des.imm_int?(b,FALSE,1) THEN
      IF b.n#0 THEN block^.true:=then ELSE block^.true:=else END;
    ELSE
      const.n:=0; put.cmd(put.cmp,const,b,1);
      block^.md:=nm_cond;
      block^.flag:=put.NE;
      block^.true:=then;
      block^.false:=else;
    END;
  END;
  free_regs.stk:=r_stk; fpp_regs.stk:=f_stk;
END gen_condition;

PROCEDURE gen_call_halt(l: ref; func: BOOLEAN; VAR a: access);
BEGIN
  error(l,'call is unrealized');
END gen_call_halt;

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
  WITH TOS DO
    am:=am_TOS;
    xm:=xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
  des.expression:=expression;
  gen_call:=gen_call_halt;
  stat_pos:=0;
END nsExpr.
