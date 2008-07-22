IMPLEMENTATION MODULE mxGen; (* Ned 27-Jul-88. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT     M: defCodes;
IMPORT  comp: coolDefs;
IMPORT inter: coolSystem;
IMPORT  scan: mxScan;
IMPORT   obs: mxObj;
IMPORT   cmd: mxCmd;

WITH STORAGE: inter;

CONST _origin = -10;    _dcopy = -11;

--------------------------  CONTEXT  --------------------------
                          -----------

CONST local_ofs = 4;  global_ofs = 2; -- номера первых переменных

TYPE
  global_ptr = POINTER TO global_rec;
  global_rec = RECORD
                 ofs : INTEGER;
                 size: INTEGER;
                 next: global_ptr;
               END;

VAR
  temp_no  : INTEGER;   -- номер свободной временной переменной <=local_no
  locs_sum : INTEGER;   -- sum of size of multi locals
  max_locs : INTEGER;   -- max of locs_sum for internal procedures
  pTab     : ARRAY [0..255] OF INTEGER;
  globals  : global_ptr;
  glo_co   : INTEGER;
--put_xref : PUT_XREF;

PROCEDURE new_var(w: INTEGER; VAR addr: INTEGER);
  VAR xx: INTEGER;
BEGIN
  IF scan.noErrors#0 THEN local_no:=local_ofs END;
  IF local_no+w>255 THEN scan.Fault(79,'переменных'); local_no:=local_ofs END;
  addr:=local_no; INC(local_no,w); temp_no:=local_no;
END new_var;

PROCEDURE temp_var(VAR addr: INTEGER);
  VAR xx: INTEGER;
BEGIN
  IF (scan.noErrors#0) OR (temp_no=local_no) THEN new_var(1,addr)
  ELSE addr:=temp_no; INC(temp_no);
  END;
END temp_var;

PROCEDURE del_var(addr: INTEGER);
BEGIN
  IF addr=temp_no-1 THEN DEC(temp_no) END;       -- del if last temp var
END del_var;

PROCEDURE glo_alloc(ofs,size: INTEGER);
  VAR n: global_ptr;
BEGIN
  INC(glo_co);
  NEW(n);
  n^.ofs:=ofs; n^.size:=size;
  n^.next:=globals; globals:=n;
END glo_alloc;

--------------------------  OBJECTS  --------------------------
                          -----------

CONST ARRs = obs.ARRs + obs.Types{obs.invtype,obs.dynarr};

CONST -- private generation tags
  direct  = obs.private+0;  -- direct displacement of multi-values
  pdx     = obs.private+1;  -- use command pdx for indexation

TYPE
  Mode  = (inv, type, cons, proc, std_proc, vari, field, stk, adr, inx, dpair);
  Modes = SET OF Mode;
  ObjPtr= POINTER TO Obj;
  Obj   = RECORD
            mode : Mode;
            type : obs.TypePtr;
            tags : BITSET;
            scope: INTEGER;
            addr : INTEGER;
            no   : INTEGER; -- # of standard proc
          END;

-------------------------  NEW & REM  -------------------------
                         -------------

PROCEDURE loadconst(o: ObjPtr);
BEGIN
  IF obs.Simple?(o^.type) THEN cmd.li(o^.addr)
  ELSE cmd.loadconst(o^.scope,o^.addr)
  END;
END loadconst;

PROCEDURE gen_const(VAR new: ObjPtr; t: obs.TypePtr; val: INTEGER);
BEGIN
  NEW(new);
  new^.mode:=cons; new^.tags:={};
  new^.type:=t; new^.addr:=val; new^.scope:=0;
  loadconst(new);
END gen_const;

PROCEDURE gen_string(VAR o: ObjPtr; t: obs.TypePtr;
                     VAL s: ARRAY OF CHAR; len: INTEGER);
  VAR ofs: INTEGER;
BEGIN ofs:=cmd.app_str(s,len); gen_const(o,t,ofs);
END gen_string;

PROCEDURE constexpr(o: ObjPtr; VAR val: INTEGER; VAR type: obs.TypePtr);
BEGIN
  IF o^.mode#cons THEN
    IF o^.mode#inv THEN scan.err(25) END; val:=0; type:=obs.Any
  ELSE cmd.pop_const; val:=o^.addr; type:=o^.type;
  END; DISPOSE(o);
END constexpr;

PROCEDURE const_dcl(o: ObjPtr; c: obs.ObjPtr);
BEGIN
  c^.scope:=o^.scope;
  constexpr(o,c^.addr,c^.type);
END const_dcl;

PROCEDURE get_type(o: ObjPtr; VAR t: obs.TypePtr);
BEGIN
  IF o^.mode=inv THEN t:=obs.Any ELSE t:=o^.type END;
END get_type;

PROCEDURE type?(o: ObjPtr):BOOLEAN; BEGIN RETURN o^.mode=type END type?;

PROCEDURE standard_proc(o: ObjPtr; VAR no: INTEGER): BOOLEAN;
BEGIN no:=o^.no; RETURN o^.mode=std_proc
END standard_proc;

PROCEDURE genObj(VAR new: ObjPtr; o: obs.ObjPtr);
BEGIN
  NEW(new);
  new^.tags:={}; new^.type:=o^.type;
  new^.addr:=o^.addr; new^.scope:=o^.scope;
  CASE o^.mode OF
    |obs.typ     : new^.mode:=type;
    |obs.cons
    ,obs.econs   : new^.mode:=cons; loadconst(new);
    |obs.vari    : new^.mode:=vari; new^.tags:=o^.tags;
    |obs.proc    : new^.mode:=proc; new^.tags:=o^.tags;
    |obs.std_proc: new^.mode:=std_proc; new^.no:=o^.no; new^.tags:=o^.tags;
    |obs.field   : new^.mode:=field;
    |obs.modul   : new^.mode:=inv; new^.type:=obs.Any; scan.err(65);
  ELSE             new^.mode:=inv; new^.type:=obs.Any;
    ASSERT(scan.noErrors#0);
  END;
END genObj;

-------------------------  DESIGNATOR  ------------------------
                         --------------

PROCEDURE load_var_val(v: ObjPtr);
BEGIN
  IF direct IN v^.tags THEN cmd.loadadr(v^.scope,v^.addr)
  ELSE cmd.loadword(v^.scope,v^.addr);
    IF (obs.varpar IN v^.tags) & obs.Simple?(v^.type) THEN cmd.lsw(0) END;
  END;
  v^.mode:=stk;
END load_var_val;

PROCEDURE var_adr(v: ObjPtr);
BEGIN
  IF obs.RO IN v^.tags THEN scan.err(29); v^.mode:=inv; RETURN END;
  CASE v^.mode OF
    |inv   : RETURN
    |vari  : IF obs.Simple?(v^.type) THEN
               IF obs.varpar IN v^.tags THEN cmd.loadword(v^.scope,v^.addr)
               ELSE cmd.loadadr(v^.scope,v^.addr)
               END; v^.mode:=adr;
             ELSIF NOT (direct IN v^.tags) THEN
               cmd.loadword(v^.scope,v^.addr); v^.mode:=adr;
             ELSIF v^.type^.mode#obs.dynarr THEN
               cmd.loadadr(v^.scope,v^.addr);
               EXCL(v^.tags,direct); v^.mode:=adr;
             END;
    |inx   : IF obs.Char?(v^.type) THEN scan.err(29); v^.mode:=inv; RETURN END;
             cmd.lxa(obs.tsize(v^.type)); v^.mode:=adr;
    |field : IF v^.type^.mode#obs.dynarr THEN
               cmd.cADD(v^.addr); v^.mode:=adr
             END
    |adr   : (* nothing *)
  ELSE scan.err(29); v^.mode:=inv
  END;
END var_adr;

PROCEDURE load_dynamic_pair(v: ObjPtr);
  VAR n: INTEGER;
BEGIN
  IF direct IN v^.tags THEN ASSERT(v^.mode=vari);
    cmd.loadword(v^.scope,v^.addr);
    cmd.loadword(v^.scope,v^.addr+1);
    EXCL(v^.tags,direct);
  ELSE n:=0;
    IF    v^.mode=vari  THEN cmd.loadword(v^.scope,v^.addr)
    ELSIF v^.mode=inx   THEN cmd.lxa(v^.type^.size)
    ELSIF v^.mode=field THEN n:=v^.addr
    ELSIF v^.mode#adr   THEN ASSERT(FALSE);
    END;
    cmd.copt; cmd.lsw(n);
    cmd.c(M.swap); cmd.lsw(n+1);
  END; v^.mode:=dpair;
END load_dynamic_pair;

PROCEDURE load_dynamic_attr(v: ObjPtr; no: INTEGER);
  VAR n: INTEGER;
BEGIN
  CASE v^.mode OF
    |vari  : IF direct IN v^.tags THEN
               cmd.loadword(v^.scope,v^.addr+no); EXCL(v^.tags,direct);
             ELSE load_var_val(v); cmd.lsw(no)
             END;
    |inx   : cmd.lxa(v^.type^.size); cmd.lsw(no)
    |field : cmd.lsw(v^.addr+no)
    |adr   : cmd.lsw(no)
  ELSE scan.err(27); v^.mode:=inv; RETURN
  END;
  v^.mode:=stk;
  IF no=0 THEN v^.type:=obs.addrp ELSE v^.type:=obs.intp END;
END load_dynamic_attr;

PROCEDURE load_dynarr(v: ObjPtr);
BEGIN
  IF    v^.mode=vari  THEN load_var_val(v)
  ELSIF v^.mode=inx   THEN cmd.lxa(v^.type^.size);
  ELSIF v^.mode=field THEN cmd.cADD(v^.addr);
  ELSIF v^.mode=adr   THEN
  ELSIF v^.mode#inv   THEN scan.err(27); v^.mode:=inv;
  END;
END load_dynarr;

PROCEDURE load_value(v: ObjPtr);
BEGIN
  CASE v^.mode OF
    |inv   : RETURN
    |stk   : RETURN
    |vari  : IF v^.type^.mode#obs.dynarr THEN load_var_val(v) END;
    |inx   : IF obs.Simple?(v^.type) THEN
               IF obs.Char?(v^.type) THEN cmd.c(M.lxb) ELSE cmd.c(M.lxw) END;
               cmd.ePop; v^.mode:=stk
             ELSE cmd.lxa(obs.tsize(v^.type)); v^.mode:=adr;
             END;
    |field : IF obs.Simple?(v^.type) THEN
               IF v^.addr<=0FFh THEN cmd.lsw(v^.addr)
               ELSE cmd.li(v^.addr); cmd.c(M.lxw); cmd.ePop
               END; v^.mode:=stk;
             ELSIF v^.type^.mode#obs.dynarr THEN
                cmd.cADD(v^.addr); v^.mode:=adr;
             END
    |proc  : IF (obs.code_proc IN v^.tags) OR (v^.scope>0) THEN
               scan.err(30); v^.mode:=inv; RETURN
             END;
             cmd.c(M.lpc); cmd.b(-v^.scope); cmd.b(v^.addr); cmd.ePush;
             v^.mode:=stk;
    |adr   : IF obs.Simple?(v^.type) THEN cmd.lsw(0); v^.mode:=stk END;
    |cons  :
  ELSE scan.err(30); v^.mode:=inv;
  END;
END load_value;

PROCEDURE load_farr_high(o: ObjPtr);
BEGIN
  ASSERT(o^.type^.mode=obs.farr);
  cmd.loadword(o^.scope,o^.addr-1);
END load_farr_high;

PROCEDURE eval_high(v: ObjPtr; zero: BOOLEAN);
  VAR lo,hi: INTEGER;
BEGIN
  IF v^.mode=inv THEN RETURN END;
  v^.tags:=v^.tags-{obs.varpar,obs.seqpar};
  IF v^.type^.mode=obs.farr THEN
    load_farr_high(v); v^.type:=obs.intp; v^.mode:=stk;
  ELSIF v^.type^.mode=obs.dynarr THEN
    load_dynamic_attr(v,1);
  ELSIF v^.type^.mode=obs.arr THEN
    obs.LoHi(v^.type^.inx,lo,hi);
    IF zero THEN DEC(hi,lo) END;
    v^.mode:=cons; v^.scope:=0; v^.addr:=hi; v^.type:=v^.type^.inx;
    cmd.li(hi);
  ELSE scan.err(28); v^.mode:=inv
  END;
END eval_high;

PROCEDURE AllotVar(o: obs.ObjPtr);
  CONST ini_var = { ORD('N')-ORD('A'), ORD('I')-ORD('A') };
        nils    = obs.Types{ obs.ptr, obs.hidden };
  VAR sz,l,n: INTEGER;
BEGIN
  IF (o^.type^.mode IN nils) & (scan.opts*ini_var#{}) THEN
    cmd.li(INTEGER(NIL)); cmd.storeword(o^.scope,o^.addr);
  ELSIF obs.Simple?(o^.type) THEN RETURN
  ELSIF (o^.type^.mode=obs.dynarr) & (scan.opts*ini_var#{}) THEN
    cmd.li(INTEGER(NIL)); cmd.storeword(o^.scope,o^.addr);
    cmd.li(-1);           cmd.storeword(o^.scope,o^.addr+1);
  ELSIF direct IN o^.tags THEN RETURN
  ELSE sz:=obs.tsize(o^.type);
    IF cmd.curlevel=0 THEN glo_alloc(o^.addr,sz);
    ELSE
      cmd.alloc(sz); cmd.storeword(o^.scope,o^.addr); INC(locs_sum,sz);
    END;
  END;
END AllotVar;

PROCEDURE copyParm(o: obs.ObjPtr);
  VAR t: obs.TypePtr;
BEGIN t:=o^.type;
  IF t^.mode=obs.farr THEN
    cmd.copy_farr(o^.scope,o^.addr,o^.addr-1
               ,obs.Char?(t^.base),obs.tsize(t^.base))
  ELSE cmd.copy_struct(o^.scope,o^.addr,obs.tsize(t))
  END;
END copyParm;

PROCEDURE range_check0(o: ObjPtr; lo,hi: INTEGER);
BEGIN
  IF o^.mode=cons THEN
    IF (o^.addr<lo) OR (o^.addr>hi) THEN scan.err(44) END
  ELSIF ORD('R')-ORD('A') IN scan.opts THEN
    IF lo#0 THEN cmd.li(lo); cmd.li(hi); cmd.c(M.chk);  cmd.ePop; cmd.ePop
    ELSE                     cmd.li(hi); cmd.c(M.chkz); cmd.ePop
    END
  END
END range_check0;

CONST RANGEs = obs.Types{obs.rang,obs.enum};

PROCEDURE range_check(t: obs.TypePtr; o: ObjPtr);
  VAR lo,hi: INTEGER;
BEGIN
  IF t^.mode IN RANGEs THEN obs.LoHi(t,lo,hi); range_check0(o,lo,hi) END;
END range_check;

-------------------------  DESIGNATOR  ------------------------
                         --------------

PROCEDURE access(v: ObjPtr; id: INTEGER);
  VAR sz: INTEGER; f: obs.ObjPtr;
BEGIN
--IF v^.type^.mode=obs.dynarr THEN v^.type:=v^.type^.desc END;
  IF v^.type^.mode#obs.rec THEN scan.err(32); v^.mode:=inv; RETURN END;
  sz:=0;
  CASE v^.mode OF
    |inv  : RETURN
    |vari : IF NOT (direct IN v^.tags) THEN load_var_val(v) END;
    |inx  : cmd.lxa(obs.tsize(v^.type));
    |field: sz:=v^.addr; (* R.F1.F2 == R.F1+F2 *)
    |stk  :
    |adr  :
  ELSE scan.err(27); v^.mode:=inv; RETURN
  END;
  f:=obs.VisInScope(v^.type^.head,id);
  IF f^.mode#obs.field THEN v^.mode:=inv; RETURN END;
  v^.type:=f^.type;
  IF direct IN v^.tags THEN
    IF v^.addr+f^.addr<=255 THEN INC(v^.addr,f^.addr);
      IF obs.Simple?(f^.type) THEN EXCL(v^.tags,direct) END;
      RETURN
    END;
  END;
  v^.mode:=field; v^.addr:=f^.addr+sz;
END access;

PROCEDURE deref(v: ObjPtr);
  VAR t: obs.TypePtr;
BEGIN t:=v^.type;
  IF t^.mode=obs.dynarr THEN
    IF NOT (ORD('U')-ORD('A') IN scan.opts) THEN scan.err(57) END;
    v^.type:=t^.desc; RETURN
  END;
  IF NOT (t^.mode IN obs.PTRs) THEN scan.err(21); v^.mode:=inv END;
  CASE v^.mode OF
    |inv   : RETURN
    |vari  : load_var_val(v)
    |inx   : cmd.c(M.lxw); cmd.ePop;
    |field : cmd.lsw(v^.addr)
    |adr   : cmd.lsw(0)
  ELSE scan.err(27); v^.mode:=inv; RETURN
  END;
  IF ORD('N')-ORD('A') IN scan.opts THEN cmd.check_nil END;
  v^.mode:=adr;
  EXCL(v^.tags,obs.RO);
  IF t^.mode=obs.ptr THEN v^.type:=t^.base ELSE v^.type:=obs.wordp END;
END deref;

PROCEDURE index0(v: ObjPtr);
  VAR dyn?: BOOLEAN; n: INTEGER;
BEGIN
  IF NOT (v^.type^.mode IN ARRs) THEN
    scan.err(21); v^.mode:=inv; RETURN
  END;
  dyn?:=(v^.type^.mode=obs.dynarr);
  CASE v^.mode OF
    |vari  : cmd.loadword(v^.scope,v^.addr); v^.mode:=adr;
    |inx   : cmd.lxa(obs.tsize(v^.type)); v^.mode:=adr;
    |field : IF NOT dyn? THEN cmd.cADD(v^.addr); v^.mode:=adr END;
    |adr   :
    |stk   : ASSERT(FALSE);
    |cons  : RETURN
  ELSE scan.err(27); v^.mode:=inv;
  END;
  IF NOT dyn? THEN RETURN END;
  IF NOT (direct IN v^.tags) THEN
    IF v^.mode=field THEN n:=v^.addr ELSE n:=0 END;
    IF ORD('T') - ORD('A') IN scan.opts THEN
      IF cmd.CPU>=1 THEN
        cmd.cADD(n); INCL(v^.tags,pdx);
      ELSE
        cmd.copt; cmd.lsw(n); cmd.c(M.swap); cmd.lsw(n+1); v^.mode:=dpair
      END;
    ELSE cmd.lsw(n);
      IF ORD('N')-ORD('A') IN scan.opts THEN cmd.check_nil END;
    END;
  ELSIF  ORD('N')-ORD('A') IN scan.opts THEN cmd.check_nil
  END;
END index0;

PROCEDURE index1(v,ex: ObjPtr);

  PROCEDURE const_pair(base: obs.TypePtr; ix: INTEGER);
  BEGIN
    IF v^.scope#0 THEN scan.err(16) END;
    IF NOT obs.Simple?(base) THEN
      INC(v^.addr,ix*obs.tsize(base)); cmd.lsta(v^.addr);
    ELSE
      IF obs.Char?(base) THEN v^.addr:=cmd.get_byte(v^.addr,ix)
      ELSE                    v^.addr:=cmd.get_word(v^.addr,ix)
      END; cmd.li(v^.addr);
    END;
  END const_pair;

  PROCEDURE const_index(v: ObjPtr; ix: INTEGER);
    VAR base: obs.TypePtr;
  BEGIN base:=v^.type^.base;
    IF obs.Char?(base) THEN cmd.li(ix); v^.mode:=inx;
    ELSE v^.mode:=field; v^.addr:=obs.tsize(base)*ix;
    END;
  END const_index;

  PROCEDURE arr_index;
    VAR lo,hi: INTEGER; type: obs.TypePtr;
  BEGIN type:=v^.type;
    obs.TypeCmp(ex^.type,type^.inx); obs.LoHi(type^.inx,lo,hi);
    IF ex^.mode=cons THEN
      IF (ex^.addr<lo) OR (ex^.addr>hi) THEN scan.err(44) END;
    END;
    IF (v^.mode=cons) & (ex^.mode=cons) THEN
      cmd.pop_const; cmd.pop_const;
      const_pair(type^.base,ex^.addr-lo); RETURN
    END;
    IF ex^.mode=cons THEN cmd.pop_const; const_index(v,ex^.addr-lo);
    ELSE
      IF lo#0 THEN cmd.cADD(-lo); DEC(hi,lo) END;
      IF ORD('T')-ORD('A') IN scan.opts THEN
        cmd.li(hi); cmd.c(M.chkz); cmd.ePop
      END;
      v^.mode:=inx;
    END;
  END arr_index;

  PROCEDURE farr_index;
  BEGIN
    IF ORD('T')-ORD('A') IN scan.opts THEN
      load_farr_high(v); cmd.c(M.chkz); cmd.ePop;
    END;
    v^.mode:=inx;
    IF obs.seqpar IN v^.tags THEN EXCL(v^.tags,obs.seqpar);
      IF (obs.varpar IN v^.tags) THEN
        cmd.c(M.lxw); cmd.ePop; v^.mode:=adr; EXCL(v^.tags,obs.varpar);
      ELSIF NOT obs.Simple?(v^.type^.base) THEN
        cmd.c(M.lxw); cmd.ePop; v^.mode:=adr;
      END
    END;
  END farr_index;

  PROCEDURE dynarr_index;
  BEGIN
    IF v^.mode=dpair THEN cmd.c(M.swap); cmd.c(M.chkz); cmd.ePop
    ELSIF direct IN v^.tags THEN EXCL(v^.tags,direct);
      IF ORD('T')-ORD('A') IN scan.opts THEN
        cmd.loadword(v^.scope,v^.addr+1); cmd.c(M.chkz); cmd.ePop;
      END;
    ELSIF pdx IN v^.tags THEN cmd.c(M.pdx); EXCL(v^.tags,pdx);
    END;
    v^.mode:=inx;
  END dynarr_index;

BEGIN
  IF    v^.mode=inv              THEN DISPOSE(ex); RETURN
  ELSIF v^.type^.mode=obs.arr    THEN arr_index
  ELSIF v^.type^.mode=obs.farr   THEN
     obs.TypeCmp(ex^.type,obs.intp); farr_index;
  ELSIF v^.type^.mode=obs.dynarr   THEN
    obs.TypeCmp(ex^.type,obs.intp); dynarr_index
  ELSE scan.err(21); v^.mode:=inv
  END;
  v^.type:=v^.type^.base;
  DISPOSE(ex);
END index1;

-------------------------  ASSIGMENT  -------------------------
                         -------------

PROCEDURE assign0(o: ObjPtr);
BEGIN
  IF o^.mode IN Modes{inv,cons,type,proc,stk} THEN
    IF o^.mode#inv THEN scan.err(27); o^.mode:=inv END; RETURN
  END;
IF o^.type^.mode=obs.dynarr THEN
  IF NOT (ORD('X')-ORD('A') IN scan.opts) THEN scan.err(80) END;
END;
  IF o^.mode=vari THEN
    IF o^.type^.mode=obs.dynarr THEN
      load_dynamic_pair(o);
    ELSIF obs.varpar IN o^.tags THEN
      cmd.loadword(o^.scope,o^.addr); o^.mode:=adr;
    ELSIF NOT obs.Simple?(o^.type) THEN
      IF direct IN o^.tags THEN cmd.loadadr(o^.scope,o^.addr)
      ELSE cmd.loadword(o^.scope,o^.addr)
      END; o^.mode:=adr
    ELSE
      IF (o^.scope>0) & (o^.scope#cmd.curlevel) THEN
        IF (o^.addr<=0) OR (o^.addr>255) THEN
          cmd.loadadr(o^.scope,o^.addr); o^.mode:=adr
        ELSE cmd.getbase(o^.scope); o^.mode:=field;
        END;
      END;
    END;
  ELSIF NOT obs.Simple?(o^.type) THEN
    IF o^.type^.mode=obs.dynarr THEN load_dynamic_pair(o) ELSE var_adr(o) END;
  ELSIF (o^.mode=field) & (o^.addr>255) THEN cmd.cADD(o^.addr); o^.mode:=adr
  END;
  IF obs.RO IN o^.tags THEN scan.err(76) END;
END assign0;

PROCEDURE assign1(o,ex: ObjPtr);

  PROCEDURE assign_simple(v: ObjPtr);
  BEGIN
    CASE v^.mode OF
     |inv   :
     |adr   : cmd.ssw(0)
     |vari  : cmd.storeword(v^.scope,v^.addr);
     |field : cmd.ssw(v^.addr)
     |inx   : IF obs.Char?(v^.type) THEN cmd.c(M.sxb) ELSE cmd.c(M.sxw) END;
              cmd.ePop; cmd.ePop; cmd.ePop;
    ELSE scan.err(27); v^.mode:=inv
    END
  END assign_simple;

  PROCEDURE char_to_string(o: ObjPtr);
  BEGIN
    IF (o^.type^.mode IN ARRs) & obs.Char?(o^.type^.base) THEN
      IF o^.type^.mode#obs.arr THEN
        IF o^.type^.mode=obs.farr THEN
          cmd.li(0); load_farr_high(o);
        ELSIF o^.type^.mode=obs.dynarr THEN
          cmd.c(M.swap); cmd.li(0); cmd.c(M.swap);
        END;
        cmd.c(M.chkz); cmd.ePop; cmd.c(M.drop); cmd.ePop;
      END;
      cmd.ssw(0); -- зависит от нумерации байтов в слове
    ELSE scan.err(21)
    END;
  END char_to_string;

  PROCEDURE check(base: obs.TypePtr; swap: BOOLEAN);
    VAR size: INTEGER;
  BEGIN
    IF obs.Char?(base) THEN
      IF swap THEN cmd.c(M.swap) END;
      cmd.c(M.chkz); cmd.ePop; cmd.cADD(4); cmd.cDIV(4,FALSE);
    ELSE
      cmd.copt; cmd.c(M.chk); cmd.ePop; cmd.ePop;
      cmd.cADD(1);
      size:=obs.tsize(base);
      IF size>1 THEN cmd.cMUL(size) END;
    END;
  END check;

  PROCEDURE assign_struct(o,ex: ObjPtr);
    VAR base: obs.TypePtr; size: INTEGER;
  BEGIN
    IF obs.Char?(ex^.type) THEN char_to_string(o); RETURN END;
    obs.AsgCmp(o^.type,ex^.type);
    IF scan.noErrors#0 THEN RETURN END;
    IF (o^.type^.mode IN obs.DYNs) OR (ex^.type^.mode IN obs.DYNs) THEN
      base:=o^.type^.base;
      IF ex^.type^.mode=obs.dynarr THEN load_dynamic_pair(ex) END;
      IF o^.mode=dpair THEN
        IF ex^.mode=dpair THEN
          cmd.stot; cmd.c(M.swap); cmd.lodt; check(base,TRUE);
        ELSE
          cmd.c(M.swap); eval_high(ex,TRUE); check(base,TRUE);
        END;
      ELSIF ex^.mode=dpair THEN eval_high(o,TRUE); check(base,FALSE);
      ELSIF (o^.type^.mode=obs.farr) OR (ex^.type^.mode=obs.farr) THEN
        eval_high(ex,TRUE); eval_high(o,TRUE); check(base,FALSE);
      END;
      cmd.move;
    ELSE size:=obs.tsize(ex^.type);
      IF size=1 THEN cmd.lsw(0); cmd.ssw(0) ELSE cmd.li(size); cmd.move END;
    END;
  END assign_struct;

BEGIN
  IF scan.noErrors#0 THEN DISPOSE(o); DISPOSE(ex); RETURN END;
  IF obs.Simple?(o^.type) THEN
    obs.AsgCmp(o^.type,ex^.type);
    range_check(o^.type,ex);
    assign_simple(o);
  ELSE assign_struct(o,ex)
  END;
  cmd.mark_pos;
  DISPOSE(o); DISPOSE(ex);
IF (scan.noErrors=0) & (cmd.depth#0) THEN
  inter.xprint('\n*** depth=%d ***\n',cmd.depth); ASSERT(FALSE)
END;
END assign1;

----------------------  SET CONTSTRUCTOR  ---------------------
                      --------------------

PROCEDURE SetCmp(set: obs.TypePtr; o: ObjPtr);
  VAR lo,hi: INTEGER;
BEGIN
  IF set^.mode=obs.invtype THEN scan.err(52); RETURN END;
  obs.TypeCmp(set^.base,o^.type);
  IF o^.mode#cons THEN RETURN END;
  IF set^.mode=obs.settype THEN obs.LoHi(set^.base,lo,hi)
  ELSE lo:=0; hi:=BITS(BITSET)-1
  END;
  IF (o^.addr>hi) OR (o^.addr<lo) THEN scan.err(44); o^.addr:=lo END;
END SetCmp;

PROCEDURE enterset(o: ObjPtr);
BEGIN
  IF o^.mode#type THEN
    IF o^.mode=inv THEN scan.err(19) END;
    o^.mode:=inv; o^.type:=obs.Any; RETURN
  END;
  IF NOT (o^.type^.mode IN obs.SETs) THEN o^.type:=obs.Any; scan.err(52) END;
  o^.mode:=cons; o^.addr:=0;
END enterset;

PROCEDURE setelem(o,e: ObjPtr);
BEGIN
  IF scan.noErrors#0 THEN DISPOSE(e); RETURN END;
  SetCmp(o^.type,e);
  IF e^.mode=cons THEN cmd.pop_const;
    o^.addr:=INTEGER( BITSET(o^.addr)+{e^.addr} );
  ELSE cmd.c(M.bit);
    IF o^.mode=cons THEN o^.mode:=stk ELSE cmd.c(M.or); cmd.ePop END;
  END;
  DISPOSE(e);
END setelem;

PROCEDURE setrange(o,e1,e2: ObjPtr);
  CONST R = ORD('R')-ORD('A');
  VAR i: INTEGER; w: BITSET;
BEGIN
  IF scan.noErrors#0 THEN DISPOSE(e1); DISPOSE(e2); RETURN END;
  SetCmp(o^.type,e1); SetCmp(o^.type,e2);
  IF (e1^.mode=cons) & (e2^.mode=cons) THEN
    IF e1^.addr>e2^.addr THEN scan.err(9); RETURN END;
    cmd.pop_const; cmd.pop_const;
    w:=BITSET(o^.addr);
    FOR i:=e1^.addr TO e2^.addr DO INCL(w,i) END;
    o^.addr:=INTEGER(w);
  ELSE
    IF e2^.mode#cons THEN
      IF R IN scan.opts THEN cmd.li(31); cmd.c(M.chkz); cmd.ePop END;
      cmd.copt; cmd.stot
    END;
    cmd.c(M.swap);
    IF (e1^.mode#cons) & (R IN scan.opts) THEN
      cmd.li(31); cmd.c(M.chkz); cmd.ePop
    END;
    cmd.c(M.sub); cmd.ePop; cmd.copt;
    cmd.li(0); cmd.c(M.geq); cmd.ePop;
    IF cmd.CPU=0 THEN
      cmd.c(M.neg);  cmd.c(M.swap);
      cmd.c(M.neg);  cmd.cADD(31);
      cmd.c(M.shl);  cmd.ePop;
    ELSE
      cmd.li(1); cmd.c(M.ror); cmd.ePop;
      cmd.c(M.swap); cmd.c(M.shr); cmd.ePop;
    END;
    IF e2^.mode=cons THEN
      IF e2^.addr#31 THEN
        cmd.li(31-e2^.addr); cmd.c(M.ror); cmd.ePop;
      END;
    ELSE
      cmd.li(31); cmd.lodt; cmd.c(M.sub); cmd.ePop;
      cmd.c(M.ror); cmd.ePop;
    END;
    IF o^.mode=cons THEN o^.mode:=stk ELSE cmd.c(M.or); cmd.ePop END;
  END;
  DISPOSE(e1); DISPOSE(e2);
END setrange;

PROCEDURE exitset(o: ObjPtr);
BEGIN
  IF    o^.mode=inv  THEN RETURN
  ELSIF o^.mode=cons THEN cmd.li(o^.addr);
  ELSE ASSERT(o^.mode=stk);
    IF o^.addr#0 THEN cmd.li(o^.addr); cmd.c(M.or); cmd.ePop END;
  END;
END exitset;

----------------  ARRAY & RECORD CONSTRUCTORs  ----------------
                -------------------------------

VAR char_buf: ARRAY [0..3] OF CHAR; char_pos: INTEGER;

PROCEDURE create_open_array(VAR o: ObjPtr; base: obs.TypePtr);
BEGIN
  obs.chkSimple(base);
  IF obs.Char?(base) THEN char_pos:=0 END;
  gen_const(o,base,cmd.pool_ofs);
END create_open_array;

PROCEDURE close_open_array(o: ObjPtr; type: obs.TypePtr);
  VAR p: POINTER TO INTEGER;
BEGIN
  IF obs.Char?(o^.type) & (char_pos#0) THEN
    WHILE char_pos<=HIGH(char_buf) DO
      char_buf[char_pos]:=0c; INC(char_pos)
    END;
    p:=SYSTEM.ADR(char_buf); cmd.app_word(p^);
  END;
  o^.type:=type;
END close_open_array;

PROCEDURE app_elem(o,e: ObjPtr);
  VAR p: POINTER TO INTEGER;
    val: INTEGER; t: obs.TypePtr;
BEGIN
  constexpr(e,val,t);
  IF scan.noErrors#0 THEN RETURN END;
  obs.AsgCmp(o^.type,t);
  IF obs.Char?(o^.type) THEN
    char_buf[char_pos]:=CHAR(val); INC(char_pos);
    IF char_pos>HIGH(char_buf) THEN
      p:=SYSTEM.ADR(char_buf); cmd.app_word(p^); char_pos:=0;
    END;
  ELSE cmd.app_word(val);
  END;
END app_elem;

-------------------------  ConstApply  ------------------------
                         --------------

PROCEDURE constop(o1,o2: ObjPtr; op: INTEGER);

  PROCEDURE realop(r1,r2: REAL; op: INTEGER): SYSTEM.WORD;
  BEGIN
    CASE op OF
      |scan.plus : RETURN r1+r2   |scan.minus: RETURN r1-r2
      |scan.times: RETURN r1*r2   |scan.slash: RETURN r1/r2
      |scan.leq  : RETURN r1<=r2  |scan.geq  : RETURN r1>=r2
      |scan.lss  : RETURN r1< r2  |scan.gtr  : RETURN r1> r2
      |scan.equ  : RETURN r1=r2   |scan.neq  : RETURN r1#r2
    ELSE ASSERT(FALSE);
    END;
  END realop;

  PROCEDURE intop(c1,c2: INTEGER; op: INTEGER): SYSTEM.WORD;
  BEGIN
    CASE op OF
      |scan.plus : RETURN c1+c2   |scan.minus: RETURN c1-c2
      |scan.times: RETURN c1*c2
      |scan.div  : RETURN c1 DIV c2
      |scan.mod  : RETURN c1 MOD c2
      |scan.slash: RETURN TRUNC(FLOAT(c1)/FLOAT(c2))
      |scan.rem  : RETURN c1-c2*TRUNC(FLOAT(c1)/FLOAT(c2))
      |scan.leq  : RETURN c1<=c2  |scan.geq  : RETURN c1>=c2
      |scan.lss  : RETURN c1< c2  |scan.gtr  : RETURN c1> c2
      |scan.equ  : RETURN c1=c2   |scan.neq  : RETURN c1#c2
      |scan.rol  : RETURN c1<<c2
      |scan.ror  : RETURN c1>>c2
    ELSE ASSERT(FALSE);
    END;
  END intop;

  PROCEDURE setop(w1,w2: BITSET; op: INTEGER): SYSTEM.WORD;
  BEGIN
    CASE op OF
      |scan.in   : RETURN INTEGER(w1) IN w2
      |scan.plus : RETURN w1+w2   |scan.minus: RETURN w1-w2
      |scan.times: RETURN w1*w2   |scan.slash: RETURN w1/w2
      |scan.leq  : RETURN w1<=w2  |scan.geq  : RETURN w1>=w2
      |scan.equ  : RETURN w1=w2   |scan.neq  : RETURN w1#w2
    ELSE ASSERT(FALSE);
    END;
  END setop;

  CONST int_ops = obs.Types{obs.bool,obs.char,obs.enum}+obs.INTs+obs.PROCs;
  VAR m: obs.TypeMode;
BEGIN m:=o2^.type^.mode;
  cmd.pop_const; cmd.pop_const;
  cmd.mask_overflow;
  IF    m IN int_ops  THEN
    o1^.addr:=intop(o1^.addr,o2^.addr,op)
  ELSIF m IN obs.SETs THEN
    o1^.addr:=setop(BITSET(o1^.addr),BITSET(o2^.addr),op)
  ELSIF m=obs.real    THEN
    o1^.addr:=realop(REAL(o1^.addr),REAL(o2^.addr),op)
  ELSE scan.err(16)
  END;
  IF cmd.check_overflow() THEN scan.err(54) END;
  loadconst(o1);
END constop;

---------------------------  Apply  ---------------------------
                           ---------

PROCEDURE relop(o1,o2: ObjPtr; sy: INTEGER);
BEGIN
  IF (o2^.mode=cons) & (o2^.addr=0) & (sy IN {scan.equ,scan.neq}) THEN
    cmd.pop_const; cmd.comp0(sy); RETURN
  END;
  cmd.bin_op(sy,0);
END relop;

PROCEDURE stringop(o1,o2: ObjPtr; sy: INTEGER);

  PROCEDURE load(o: ObjPtr);
  BEGIN
    load_dynamic_attr(o,0);
    IF ORD('N')-ORD('A') IN scan.opts THEN cmd.check_nil END;
  END load;

  CONST inverse = ARRAY OF INTEGER{
                    scan.equ, scan.neq, -1,
                    scan.gtr, scan.lss, scan.geq, scan.leq
                                  };
BEGIN
  IF o1^.type^.mode=obs.dynarr THEN
    IF o2^.type^.mode=obs.dynarr THEN
      IF o1^.mode=vari THEN
        load(o1);
        IF o2^.mode#vari THEN cmd.c(M.swap) END;
        load(o2);
      ELSIF direct IN o2^.tags THEN load(o1); load(o2);
      ELSE
        load(o2); cmd.c(M.swap); load(o1);
        sy:=inverse[sy];
      END;
    ELSE
      IF o1^.mode#vari THEN cmd.c(M.swap) END;
      load(o1);
      sy:=inverse[sy];
    END;
  ELSIF o2^.type^.mode=obs.dynarr THEN load(o2)
  END;
  cmd.string_cmp(sy);
END stringop;

PROCEDURE addop(o1,o2: ObjPtr);
BEGIN
  IF o2^.mode=cons THEN cmd.pop_const; cmd.cADD(o2^.addr);
  ELSE cmd.bin_op(scan.plus,0);
  END;
  IF o2^.type^.mode=obs.addr THEN o1^.type:=obs.addrp END;
END addop;

PROCEDURE intmul(o1,o2: ObjPtr; sy: INTEGER);
BEGIN
  IF o2^.mode=cons THEN cmd.pop_const;
    IF    sy=scan.times THEN cmd.cMUL(o2^.addr);
    ELSIF sy=scan.div   THEN cmd.cDIV(o2^.addr,FALSE);
    ELSIF sy=scan.slash THEN cmd.cDIV(o2^.addr,TRUE);
    ELSIF sy=scan.mod   THEN cmd.cMOD(o2^.addr,FALSE);
    ELSIF sy=scan.rem   THEN cmd.cMOD(o2^.addr,TRUE);
    ELSE ASSERT(FALSE);
    END;
  ELSE cmd.bin_op(sy,0)
  END;
END intmul;

--------------------------------------------------------------

CONST
  noop     = 16;  -- число операций
  variants = ARRAY OF CHAR {

-- =   #  IN   <   >   <=  >=  *  DIV MOD /   -   +   <<  >>  REM
  15c,15c,15c,15c,15c,15c,15c,15c,15c,15c,15c,15c,15c,15c,15c,15c, -- invtype
   2c, 2c,77c, 2c, 2c, 2c, 2c, 4c, 4c, 4c, 4c, 7c, 5c, 1c, 1c, 4c, -- int
   2c, 2c,77c, 2c, 2c, 2c, 2c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- bool
   2c, 2c,77c, 2c, 2c, 2c, 2c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- char
   2c, 2c, 0c,77c,77c,12c,12c,10c,77c,77c,10c,10c,10c,77c,77c,77c, -- bitset
  13c,13c,77c,13c,13c,13c,13c,11c,77c,77c,11c,11c,11c,77c,77c,77c, -- real
   2c, 2c,77c, 2c, 2c, 2c, 2c, 4c, 4c, 4c, 4c, 7c, 5c, 1c, 1c, 4c, -- addr
  14c,14c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- word
   2c, 2c,77c, 2c, 2c, 2c, 2c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- enum
  77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- rang
   2c, 2c,77c, 2c, 2c, 2c, 2c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- ptr
   2c, 2c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- hidden
   6c, 6c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- proctype
   6c, 6c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- functype
   2c, 2c, 0c,77c,77c,12c,12c,10c,77c,77c,10c,10c,10c,77c,77c,77c, -- set
   3c, 3c,77c, 3c, 3c, 3c, 3c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- arr
   3c, 3c,77c, 3c, 3c, 3c, 3c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- farr
  77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c,77c, -- rec
   3c, 3c,77c, 3c, 3c, 3c, 3c,77c,77c,77c,77c,77c,77c,77c,77c,77c};-- dynarr

PROCEDURE apply(o1,o2: ObjPtr; sy: INTEGER);
  VAR t1,t2: obs.TypePtr; cons?: BOOLEAN;
BEGIN
  IF (o1^.mode=inv) OR (o2^.mode=inv) THEN
    o1^.mode:=inv; o1^.type:=obs.Any; DISPOSE(o2); RETURN
  END;
  t1:=o1^.type; IF t1^.mode=obs.rang THEN t1:=t1^.base; o1^.type:=t1 END;
  t2:=o2^.type; IF t2^.mode=obs.rang THEN t2:=t2^.base; o2^.type:=t2 END;
  cons?:=(o1^.mode=cons) & (o2^.mode=cons);
  CASE variants[ORD(t2^.mode)*noop+sy] OF
    | 0c: obs.TypeCmp(t1,t2^.base);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(scan.in,1) END;
          o1^.type:=obs.boolp;
    | 1c: obs.chkSimple(t1);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(sy,0) END;
          IF  NOT (t1^.mode IN obs.Types{obs.int,obs.bitset}) THEN
            o1^.type:=obs.wordp
          END;
    | 2c: IF t1=obs.wordp THEN obs.chkSimple(t2) ELSE obs.TypeCmp(t1,t2) END;
          IF cons? THEN constop(o1,o2,sy) ELSE relop(o1,o2,sy) END;
          o1^.type:=obs.boolp;
    | 3c: IF t1^.mode IN ARRs THEN
            obs.TypeCmp(t1^.base,obs.charp);
            obs.TypeCmp(t2^.base,obs.charp);
            IF cons? THEN constop(o1,o2,sy) ELSE stringop(o1,o2,sy) END;
            o1^.type:=obs.boolp;
          ELSE scan.err(21); o1^.mode:=inv
          END;
    | 4c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE intmul(o1,o2,sy) END;
    | 5c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE addop(o1,o2) END;
    | 6c: obs.ProcCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE relop(o1,o2,sy) END;
          o1^.type:=obs.boolp;
    | 7c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(sy,0) END;
    |10c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(sy,1) END;
    |11c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(sy,2) END;
    |12c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(sy,1) END;
          o1^.type:=obs.boolp;
    |13c: obs.TypeCmp(t1,t2);
          IF cons? THEN constop(o1,o2,sy) ELSE cmd.bin_op(sy,2) END;
          o1^.type:=obs.boolp;
    |14c: obs.chkSimple(t1); obs.chkSimple(t2);
          IF cons? THEN constop(o1,o2,sy) ELSE relop(o1,o2,sy) END;
          o1^.type:=obs.boolp;
    |15c: (* inv op *) o1^.type:=obs.Any;
  ELSE
    scan.err(21); o1^.mode:=inv;
  END;
  IF NOT cons? & (o1^.mode#inv) THEN o1^.mode:=stk END;
  DISPOSE(o2);
END apply;

PROCEDURE applyNOT(o: ObjPtr);
BEGIN
  IF o^.mode=inv THEN RETURN END;
  obs.TypeCmp(o^.type,obs.boolp);
  IF o^.mode=cons THEN
    cmd.pop_const; o^.addr:=INTEGER(o^.addr=0); cmd.li(o^.addr);
  ELSE cmd.logical_not(FALSE);
  END;
END applyNOT;

PROCEDURE applyNEG(o: ObjPtr);

  PROCEDURE set_inverse;
    VAR lo,hi: INTEGER; s: BITSET;
  BEGIN
    IF o^.type^.mode=obs.bitset THEN s:={0..31}
    ELSE obs.LoHi(o^.type^.base,lo,hi); s:={};
      WHILE lo<=hi DO INCL(s,lo); INC(lo) END;
    END;
    IF o^.mode=cons THEN
      cmd.pop_const; o^.addr:=INTEGER(s-BITSET(o^.addr)); cmd.li(o^.addr);
    ELSE cmd.li(INTEGER(s)); cmd.c(M.swap); cmd.c(M.bic); cmd.ePop;
    END;
  END set_inverse;

BEGIN
  IF o^.mode=inv THEN RETURN END;
  IF    o^.type^.mode IN obs.INTs THEN
    IF o^.mode=cons THEN cmd.pop_const;
      cmd.mask_overflow;
      o^.addr:=-o^.addr;
      IF cmd.check_overflow() THEN scan.err(54) END;
      cmd.li(o^.addr);
    ELSE cmd.c(M.neg)
    END;
  ELSIF o^.type^.mode=obs.real THEN
    IF o^.mode=cons THEN
      cmd.pop_const; o^.addr:=INTEGER(-REAL(o^.addr)); cmd.li(o^.addr);
    ELSE cmd.c(M.fneg)
    END
  ELSIF o^.type^.mode IN obs.SETs THEN set_inverse;
  ELSE scan.err(21)
  END
END applyNEG;

PROCEDURE typetransfer(o,ex: ObjPtr);
BEGIN
  IF (o^.mode=inv) OR (ex^.mode=inv) THEN o^.mode:=inv; DISPOSE(ex); RETURN END;
  IF (ex^.type^.mode=obs.farr) OR (obs.tsize(o^.type)#obs.tsize(ex^.type)) THEN
    scan.err(20);
  END;
  ex^.type:=o^.type; o^:=ex^;
  DISPOSE(ex);
END typetransfer;

----------------------  CONTROL STRUCTs  ----------------------
                      -------------------

PROCEDURE boolexpr(VAR o: ObjPtr): INTEGER;     -- destroy object!!!
  VAR val: INTEGER;
BEGIN obs.TypeCmp(o^.type,obs.boolp);
  IF o^.mode#cons THEN val:=2; cmd.ePop;
  ELSE cmd.pop_const; val:=ORD(o^.addr#0);
  END;
  DISPOSE(o); RETURN val
END boolexpr;

PROCEDURE return; BEGIN cmd.return END return;

PROCEDURE freturn(o: ObjPtr; res: obs.TypePtr);
BEGIN
  cmd.mark_pos;
  cmd.ePop; obs.AsgCmp(res,o^.type); range_check(res,o);
  cmd.return;
  DISPOSE(o);
END freturn;

PROCEDURE enterloop; BEGIN cmd.enterloop END enterloop;
PROCEDURE endloop;   BEGIN cmd.endloop   END endloop;
PROCEDURE exitloop;  BEGIN cmd.exitloop  END exitloop;

PROCEDURE enterwhile; BEGIN cmd.enterwhile END enterwhile;

PROCEDURE do(o: ObjPtr);
BEGIN
  cmd.mark_pos;
  cmd.do(boolexpr(o));
END do;

PROCEDURE endwhile; BEGIN cmd.endwhile END endwhile;

PROCEDURE repeat;   BEGIN cmd.repeat   END repeat;

PROCEDURE until(o: ObjPtr);
BEGIN
  cmd.mark_pos;
  cmd.until(boolexpr(o));
END until;

PROCEDURE if(o: ObjPtr);
BEGIN
  cmd.mark_pos;
  cmd.if(boolexpr(o));
END if;

PROCEDURE else;  BEGIN cmd.else  END else;
PROCEDURE endif; BEGIN cmd.endif END endif;

PROCEDURE entercond(o: ObjPtr; and: BOOLEAN);
BEGIN
  cmd.entercond(boolexpr(o),and);
END entercond;

PROCEDURE exitcond(expr: ObjPtr): ObjPtr;
  VAR res: ObjPtr; val: INTEGER;
BEGIN
  cmd.exitcond(boolexpr(expr),val);
  NEW(res); res^.mode:=stk; res^.type:=obs.boolp;
  IF val#2 THEN res^.mode:=cons; res^.addr:=val END;
  RETURN res
END exitcond;

PROCEDURE mark(n: INTEGER); BEGIN cmd.mark(n) END mark;
PROCEDURE goto(n: INTEGER); BEGIN cmd.goto(n) END goto;

----------------------------  FOR  ----------------------------
                            -------
VAR
  for_type : obs.TypePtr;
  for_queue: inter.QUEUE;

PROCEDURE enterfor(v: obs.ObjPtr);
  VAR vaddr,baddr: INTEGER;
BEGIN
  inter.push(for_queue,for_type);
  for_type:=v^.type;
  vaddr:=local_ofs;
  IF v^.mode=obs.vari THEN
    IF v^.scope#cmd.curlevel THEN scan.err(40)
    ELSE obs.chkScalar(v^.type); vaddr:=v^.addr;
    END;
  END;
  temp_var(baddr);
  cmd.enterfor(vaddr,baddr);
END enterfor;

PROCEDURE for_from(o: ObjPtr);
BEGIN
  obs.TypeCmp(o^.type,for_type);
  cmd.for_from(o^.mode=cons,o^.addr);
  DISPOSE(o);
END for_from;

PROCEDURE for_to(o: ObjPtr);
BEGIN
  obs.TypeCmp(o^.type,for_type);
  cmd.for_to(o^.mode=cons,o^.addr);
  DISPOSE(o);
END for_to;

PROCEDURE for_step(o: ObjPtr);
BEGIN
  IF o^.mode=inv THEN DISPOSE(o); RETURN END;
  IF o^.mode#cons THEN scan.err(34); RETURN END;
  IF o^.type^.mode#obs.int THEN scan.err(21) END;
  cmd.for_step(o^.addr);
  DISPOSE(o);
END for_step;

PROCEDURE for_do; BEGIN cmd.mark_pos; cmd.for_do END for_do;

PROCEDURE endfor;
  VAR baddr: INTEGER;
BEGIN
  cmd.endfor(baddr);
  del_var(baddr);
  IF NOT inter.pop(for_queue,for_type) THEN ASSERT(FALSE) END;
END endfor;

----------------------------  CASE  ---------------------------
                            --------

VAR
  case_type : obs.TypePtr;
  case_queue: inter.QUEUE;

PROCEDURE entercase(o: ObjPtr);
  VAR taddr: INTEGER;
BEGIN
  IF o^.mode#cons THEN new_var(1,taddr) END;
  cmd.entercase(o^.mode=cons,o^.addr,taddr);
  inter.push(case_queue,case_type);
  obs.chkScalar(o^.type);
  case_type:=o^.type;
  DISPOSE(o);
END entercase;

PROCEDURE caselabel(o: ObjPtr);
BEGIN
  obs.TypeCmp(case_type,o^.type); range_check(case_type,o);
  IF o^.mode#cons THEN scan.err(34);
  ELSE cmd.caselabel(o^.addr);
  END;
  DISPOSE(o);
END caselabel;

PROCEDURE caserange(o1,o2: ObjPtr);
BEGIN
  obs.TypeCmp(case_type,o1^.type); range_check(case_type,o1);
  obs.TypeCmp(case_type,o2^.type); range_check(case_type,o2);
  IF (o1^.mode#cons) OR (o2^.mode#cons) OR (o1^.addr>o2^.addr) THEN
    scan.err(9);
  ELSE cmd.caserange(o1^.addr,o2^.addr);
  END;
  DISPOSE(o1); DISPOSE(o2);
END caserange;

PROCEDURE exitvariant; BEGIN cmd.exitvariant END exitvariant;
PROCEDURE elsecase;    BEGIN cmd.elsecase    END elsecase;

PROCEDURE endcase;
BEGIN
  cmd.endcase;
  IF NOT inter.pop(case_queue,case_type) THEN ASSERT(FALSE) END;
END endcase;

---------------------------------------------------------------

PROCEDURE mark_pos; BEGIN cmd.mark_pos END mark_pos;

PROCEDURE ini_structs;
BEGIN
  inter.lifo(for_queue);   for_type :=NIL;
  inter.lifo(case_queue);  case_type:=NIL;
END ini_structs;

PROCEDURE exi_structs;
BEGIN
  inter.clear(for_queue); inter.clear(case_queue);
END exi_structs;

-----------------------  WITH statement  ----------------------
                       ------------------

PROCEDURE enterwith(o: ObjPtr; v: obs.ObjPtr);
BEGIN
  v^.type:=o^.type; temp_var(v^.addr); v^.scope:=cmd.curlevel;
  IF obs.RO IN o^.tags THEN INCL(v^.tags,obs.RO) END;
  IF v^.type^.mode#obs.rec THEN scan.err(32); v^.type:=obs.Any
  ELSE load_value(o); cmd.storeword(v^.scope,v^.addr);
  END;
  cmd.mark_pos;
  DISPOSE(o);
END enterwith;

PROCEDURE endwith(v: obs.ObjPtr); BEGIN del_var(v^.addr) END endwith;

-------------------------  PROC CALLs  ------------------------
                         --------------

CONST -- tags in proc call
  _pw     = 0;          -- proc has pw
  _seq    = 1;          -- proc has seq
  _stotseq= 2;          -- stot seq paramater (not ssw)
  _trans  = 3;          -- seq to seq transport
  _tempvar= 4;          -- temp var allocated for seq base address
  _charseq= 5;          -- SEQ OF CHAR

TYPE
  call_ptr = POINTER TO call_rec;
  call_rec = RECORD
              proc  : ObjPtr;
              tags  : BITSET;
              scope : INTEGER; --\ of var for formal proc
              addr  : INTEGER; --/
              depth : INTEGER;
              pw    : INTEGER;
              point0: cmd.POINT;
              point1: cmd.POINT;
              seq_co: INTEGER;
              sdepth: INTEGER;
              next  : call_ptr;
            END;

VAR calls: call_ptr;

PROCEDURE enterProcCall(p: ObjPtr);

  PROCEDURE set_attrs(c: call_ptr; p: ObjPtr);
    VAR parm: obs.ParmPtr;
  BEGIN c^.proc:=p; c^.tags:={};
    c^.pw:=0; c^.seq_co:=0;
    parm:=p^.type^.plist;
    IF parm=NIL THEN RETURN END;
    IF parm^.addr<=0 THEN INCL(c^.tags,_pw);
      WHILE parm^.addr<=0 DO
        IF parm^.type^.mode=obs.farr THEN INC(c^.pw,2) ELSE INC(c^.pw) END;
        parm:=parm^.next;
      END;
    END;
    WHILE parm^.next#NIL DO parm:=parm^.next END;
    IF obs.seqpar IN parm^.kind THEN INCL(c^.tags,_seq);
      IF obs.Char?(parm^.type^.base) THEN INCL(c^.tags,_charseq) END;
    END;
  END set_attrs;

  PROCEDURE formal_proc(c: call_ptr; VAR d: INTEGER);
  BEGIN
    IF {_pw,_seq}*c^.tags#{} THEN
      IF p^.mode=vari THEN c^.scope:=p^.scope; c^.addr:=p^.addr;
      ELSE INCL(c^.tags,_tempvar);
        load_value(p); temp_var(c^.addr); c^.scope:=cmd.curlevel;
        cmd.storeword(c^.scope,c^.addr);
      END; d:=cmd.depth;
      IF d>0 THEN cmd.store END;
    ELSE load_value(p); d:=cmd.depth-1;
      IF d>0 THEN cmd.stofv ELSE cmd.stot END;
    END;
  END formal_proc;

  VAR c: call_ptr;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  NEW(c);
  set_attrs(c,p);
  c^.depth:=cmd.depth;
  IF p^.mode#proc THEN formal_proc(c,c^.depth)
  ELSIF NOT (obs.code_proc IN p^.tags) & (cmd.depth>0) THEN cmd.store
  END;
  IF {_seq,_pw}<=c^.tags THEN cmd.set_point(c^.point0) END;
  IF (_pw IN c^.tags) & (c^.pw>1) THEN cmd.alloc(c^.pw); cmd.copt END;
  c^.next:=calls; calls:=c;
END enterProcCall;

PROCEDURE exitProcCall(VAR res: ObjPtr);
  VAR old: call_ptr; code: BOOLEAN; p: ObjPtr; decs: INTEGER;
BEGIN
  IF scan.noErrors#0 THEN genObj(res,obs.Ilg); RETURN END;
  p:=calls^.proc; cmd.set_depth(0);
  code:=(obs.code_proc IN p^.tags);
  IF code THEN cmd.insert_code_proc(p^.addr)
  ELSIF p^.mode=proc THEN cmd.call(p^.scope,p^.addr);
  ELSIF {_pw,_seq}*calls^.tags#{} THEN
    cmd.loadword(calls^.scope,calls^.addr); cmd.stot; cmd.c(M.cf);
    IF _tempvar IN calls^.tags THEN del_var(calls^.addr) END;
  ELSE cmd.c(M.cf);
  END;
  decs:=calls^.seq_co+calls^.pw;
  IF decs>0 THEN cmd.li(decs); cmd.c(M.decs); cmd.ePop END;
  IF p^.type^.mode=obs.functype THEN
    cmd.set_depth(calls^.depth);
    IF (cmd.depth#0) & NOT code THEN cmd.lodfv END; cmd.ePush;
    NEW(res);
    res^.mode:=stk; res^.tags:={}; res^.type:=p^.type^.base;
  ELSE
    cmd.mark_pos;
    ASSERT(cmd.depth=0); res:=NIL;
  END;
  DISPOSE(p);
  old:=calls; calls:=calls^.next;
  DISPOSE(old);
END exitProcCall;

-------------------------  PARAMETERs  ------------------------
                         --------------

PROCEDURE string_cmp(t1,t2: obs.TypePtr): BOOLEAN;
BEGIN
  IF (t1^.mode#obs.arr) OR NOT obs.Char?(t1^.base) THEN RETURN FALSE END;
  IF (t2^.mode#obs.arr) OR NOT obs.Char?(t2^.base) THEN RETURN FALSE END;
  IF t2^.size>t1^.size THEN scan.err(21) END;
  RETURN TRUE
END string_cmp;

PROCEDURE param(o: ObjPtr; parm: obs.ParmPtr);

  PROCEDURE save(addr: INTEGER);
  BEGIN
    IF calls^.pw=1 THEN cmd.stot;
    ELSE cmd.ssw(calls^.pw+addr-1); IF addr<-1 THEN cmd.copt END;
    END;
  END save;

  PROCEDURE save_high(addr: INTEGER);
  BEGIN
    IF addr>0 THEN RETURN END;
    cmd.ssw(calls^.pw+addr-2); IF addr<-1 THEN cmd.copt END;
  END save_high;

  PROCEDURE save_adr(addr: INTEGER);
  BEGIN
    IF addr>0 THEN RETURN END;
    cmd.ssw(calls^.pw+addr-1); IF addr<0 THEN cmd.copt END;
  END save_adr;

  PROCEDURE char_to_string(addr: INTEGER);
  BEGIN
    cmd.lsta(cmd.pool_ofs); cmd.app_word(o^.addr);
    IF parm^.type^.mode=obs.farr THEN
      save_adr(addr); cmd.li(1); save_high(addr);
    ELSIF addr<=0 THEN save(addr)
    END;
  END char_to_string;

  PROCEDURE eval_word_high(base: obs.TypePtr);
    VAR sz: INTEGER;
  BEGIN
    IF obs.Char?(base) THEN
      cmd.cADD(4); cmd.cDIV(4,FALSE); cmd.cADD(-1);
      -- not +3/4 see case HIGH=-1
    ELSE sz:=obs.tsize(base);
      IF sz#1 THEN cmd.cADD(1); cmd.cMUL(sz); cmd.cADD(-1) END;
    END;
  END eval_word_high;

  PROCEDURE load_parm_high(t: obs.TypePtr; o: ObjPtr);
  BEGIN
    IF t^.base=obs.wordp THEN
      IF o^.type^.mode=obs.farr THEN
        load_farr_high(o); eval_word_high(o^.type^.base);
      ELSIF NOT obs.Simple?(o^.type) THEN cmd.li(obs.tsize(o^.type)-1)
      ELSE scan.err(21)
      END;
    ELSIF (o^.type^.mode IN obs.Types{obs.arr,obs.farr}) THEN
      obs.TypeCmp(t^.base,o^.type^.base); eval_high(o,TRUE);
    ELSE scan.err(21)
    END;
  END load_parm_high;

  PROCEDURE dynamic_parm(addr: INTEGER; t: obs.TypePtr; o: ObjPtr);
  BEGIN
    IF t^.base#obs.wordp THEN obs.TypeCmp(t^.base,o^.type^.base) END;
    IF direct IN o^.tags THEN
      cmd.loadword(o^.scope,o^.addr); save_adr(addr);
      cmd.loadword(o^.scope,o^.addr+1);
    ELSE load_dynamic_pair(o);
      IF addr<=0 THEN cmd.stot; save_adr(addr); cmd.lodt END;
    END;
    IF t^.base=obs.wordp THEN eval_word_high(o^.type^.base) END;
    save_high(addr);
  END dynamic_parm;

  CONST arrs = obs.ARRs - obs.Types{obs.dynarr};
  VAR addr: INTEGER; t: obs.TypePtr; dyn: BOOLEAN;
BEGIN
  IF scan.noErrors#0 THEN DISPOSE(o); RETURN END;
  addr:=parm^.addr; t:=parm^.type;
  dyn:=(o^.type^.mode=obs.dynarr);
  IF obs.varpar IN parm^.kind THEN
    IF obs.RO IN o^.tags THEN scan.err(29); DISPOSE(o); RETURN END;
    IF o^.mode=cons      THEN scan.err(27); DISPOSE(o); RETURN END;
    IF NOT dyn THEN var_adr(o) END;
  ELSIF o^.mode=cons THEN
    IF obs.Char?(o^.type) & (t^.mode IN arrs) & obs.Char?(t^.base) THEN
      cmd.pop_const; char_to_string(addr); DISPOSE(o); RETURN
    END;
  END;
  IF t^.mode=obs.farr THEN
    IF dyn THEN dynamic_parm(addr,t,o)
    ELSE save_adr(addr); load_parm_high(t,o); save_high(addr)
    END;
  ELSE
    IF dyn THEN load_dynarr(o) END;
    IF          t=obs.wordp THEN obs.chkSimple(o^.type)
    ELSIF o^.type=obs.wordp THEN obs.chkSimple(t);
    ELSIF (t^.mode=obs.arr) & (t^.base=obs.wordp) THEN
      IF (o^.type^.mode=obs.farr) OR (obs.tsize(t)#obs.tsize(o^.type)) THEN
        scan.err(21)
      END;
    ELSIF t^.mode IN obs.PROCs     THEN obs.ProcCmp(t,o^.type)
    ELSIF obs.varpar IN parm^.kind THEN obs.TypeCmp(t,o^.type);
    ELSIF (o^.mode=cons) & string_cmp(t,o^.type) THEN
    ELSE obs.TypeCmp(t,o^.type);
    END;
    IF addr<=0 THEN save(addr) END;
  END;
  DISPOSE(o);
END param;

-----------------------  SEQ PARAMETERs  ----------------------
                       ------------------

CONST stack_lim = 1;

PROCEDURE enter_seq;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  cmd.set_point(calls^.point1);
  calls^.sdepth:=cmd.depth;
  IF (cmd.depth=2) & (calls^.tags*{_pw,_charseq}={}) THEN
    INCL(calls^.tags,_stotseq); cmd.ePush;
  ELSE
    IF cmd.depth>stack_lim THEN cmd.set_depth(0) END;
    cmd.ePush; cmd.ePush;  -- alloc(seq_co); copt
  END;
END enter_seq;

PROCEDURE seq_param(o: ObjPtr; parm: obs.ParmPtr);
  VAR base: obs.TypePtr;
BEGIN
  IF scan.noErrors#0 THEN DISPOSE(o); RETURN END;
  base:=parm^.type^.base;
  IF (obs.seqpar IN o^.tags) & (calls^.seq_co=0) THEN
    INCL(calls^.tags,_trans);
    cmd.ePop; cmd.ePop; -- see enter_seq
    IF base^.mode IN obs.PROCs THEN obs.ProcCmp(o^.type^.base,base)
    ELSE obs.TypeCmp(o^.type^.base,base);
    END;
    IF obs.varpar IN parm^.kind THEN
      IF obs.varpar IN o^.tags THEN var_adr(o) ELSE scan.err(21) END;
    END;
    load_farr_high(o);
  ELSIF _trans IN calls^.tags THEN scan.err(49)
  ELSE
    IF obs.varpar IN parm^.kind THEN
      IF obs.RO IN o^.tags THEN scan.err(29); DISPOSE(o); RETURN END;
      IF o^.mode=cons      THEN scan.err(27); DISPOSE(o); RETURN END;
      IF o^.type^.mode#obs.dynarr THEN var_adr(o) END;
    END;
    IF base=obs.wordp THEN ASSERT(o^.mode#dpair);
      IF o^.type^.mode=obs.dynarr THEN load_dynamic_attr(o,0) END;
    ELSE
      IF o^.type^.mode=obs.dynarr THEN load_dynarr(o) END;
      IF base^.mode IN obs.PROCs THEN obs.ProcCmp(o^.type,base)
      ELSIF (o^.mode#cons) OR NOT string_cmp(base,o^.type) THEN
        obs.TypeCmp(o^.type,base)
      END;
    END;
    IF    _stotseq IN calls^.tags THEN cmd.stot
    ELSIF _charseq IN calls^.tags THEN
      cmd.li(calls^.seq_co); cmd.c(M.swap);
      cmd.c(M.sxb); cmd.ePop; cmd.ePop; cmd.ePop; cmd.copt;
    ELSE cmd.ssw(calls^.seq_co); cmd.copt;
    END;
    INC(calls^.seq_co);
  END;
  DISPOSE(o);
END seq_param;

PROCEDURE end_seq;
  VAR stot: BOOLEAN; d,addr,size: INTEGER;
BEGIN
  IF (scan.noErrors#0) OR (_trans IN calls^.tags) THEN RETURN END;
  IF calls^.seq_co=0 THEN cmd.li(INTEGER(NIL)); cmd.li(-1); RETURN END;
  stot:=(_stotseq IN calls^.tags);
  IF NOT stot THEN cmd.undo; cmd.ePop END;
  cmd.mcode;
  d:=calls^.sdepth; cmd.set_depth(0);
  size:=calls^.seq_co;
  IF _charseq IN calls^.tags THEN size:=(size+3) DIV 4 END;
  IF _pw IN calls^.tags THEN
    temp_var(addr); cmd.alloc(size); cmd.storeword(cmd.curlevel,addr);
    cmd.insert(calls^.point0);
  END;
  cmd.set_depth(d);
  IF _pw IN calls^.tags THEN
    IF d>stack_lim THEN cmd.store END;
    cmd.loadword(cmd.curlevel,addr); cmd.copt; del_var(addr)
  ELSIF stot THEN cmd.alloc(0);
  ELSE cmd.alloc(size);
    IF d>stack_lim THEN cmd.stofv; cmd.lodt END;
    cmd.copt;
  END;
  cmd.insert(calls^.point1);
  IF NOT stot & (d>stack_lim) THEN cmd.lodfv  END;
  cmd.set_depth(d+1);
  cmd.li(calls^.seq_co-1);
  calls^.seq_co:=size;  -- for decs
END end_seq;

-----------------------  STANDARD CALLs  ----------------------
                       ------------------

TYPE
  scall_ptr = POINTER TO scall_rec;
  scall_rec = RECORD
                proc : ObjPtr;
                p1   : ObjPtr;
                p2   : ObjPtr;
                pos  : cmd.POSITION;
                next : scall_ptr;
              END;

VAR scalls: scall_ptr;

PROCEDURE enterStandardCall(o: ObjPtr);
  CONST undo = {_size,_bytes,_bits,_high,_len};
  VAR s: scall_ptr; no: INTEGER;
BEGIN
  ASSERT(o^.mode=std_proc);
  NEW(s);
  s^.next:=scalls; scalls:=s;
  s^.proc:=o;
  s^.p1:=NIL; s^.p2:=NIL;
  no:=o^.no;
  IF no=_assert THEN cmd.mcode END;
  IF (no IN undo) OR (no=_assert) THEN cmd.save_pos(s^.pos) END;
END enterStandardCall;

PROCEDURE standard_param(o: ObjPtr);
  VAR no: INTEGER;
BEGIN
  no:=scalls^.proc^.no;
  IF scalls^.p1=NIL THEN
    CASE no OF
      |_inc,_dec,_incl,_excl:
        var_adr(o)
      |_new,_dispose,_resize:
        IF o^.type^.mode=obs.dynarr THEN
          IF direct IN o^.tags THEN
            cmd.loadadr(o^.scope,o^.addr); cmd.loadadr(o^.scope,o^.addr+1);
          ELSE
            load_dynarr(o); cmd.copt; cmd.cADD(1);
          END;
          IF no=_new THEN cmd.copt; cmd.li(-1); cmd.ssw(0) END;
        ELSE var_adr(o)
        END;
      |_assert:
        IF o^.mode=cons THEN cmd.pop_const
        ELSE cmd.logical_not(TRUE); cmd.mark_pos; cmd.mcode
        END
      |_origin:
        IF o^.type^.mode#obs.dynarr THEN scan.err(21)
        ELSIF NOT (direct IN o^.tags) THEN load_dynarr(o); cmd.copt;
        END;
      |_dcopy:
        IF o^.type^.mode#obs.dynarr THEN scan.err(21) END;
        load_dynarr(o);
    ELSE (* nothing *)
    END;
    scalls^.p1:=o
  ELSE
    IF (no=_origin) & (scalls^.p2=NIL) THEN
      obs.TypeCmp(o^.type,obs.addrp);
      IF direct IN scalls^.p1^.tags THEN
        cmd.storeword(scalls^.p1^.scope,scalls^.p1^.addr);
      ELSE cmd.ssw(0);
      END;
    END;
    scalls^.p2:=o;
  END;
END standard_param;

PROCEDURE exitStandardCall(VAR o: ObjPtr);

  PROCEDURE procs(s: scall_ptr; p1,p2: ObjPtr);

    PROCEDURE assert(p1,p2: ObjPtr);
      VAR b: INTEGER;
    BEGIN
      obs.TypeCmp(p1^.type,obs.boolp);
      IF p2#NIL THEN obs.TypeCmp(p2^.type,obs.intp) END;
      IF p1^.mode#cons THEN b:=2 ELSE b:=ORD(p1^.addr#0) END;
      IF (b=1) & ((p2=NIL) OR (p2^.mode#cons)) THEN
        cmd.resume(s^.pos)
      END;
      cmd.assert(b,(p2=NIL));
      cmd.release(s^.pos);
    END assert;

    PROCEDURE high_to_size(base: obs.TypePtr);
    BEGIN
      IF obs.Char?(base) THEN cmd.cADD(4); cmd.cDIV(4,FALSE)
      ELSE cmd.cADD(1); cmd.cMUL(obs.tsize(base))
      END;
    END high_to_size;

    PROCEDURE find_resize;
      VAR p: obs.ObjPtr;
    BEGIN
      IF NOT obs.VisInScope?(obs.SuperProc^.head,scan.str_id("RESIZE"),p) THEN
        ASSERT(FALSE);
      END;
      DISPOSE(s^.proc);
      genObj(s^.proc,p);
    END find_resize;

    PROCEDURE call;
    BEGIN
      IF   obs.forward IN s^.proc^.tags THEN scan.err(69)
      ELSIF obs.varpar IN s^.proc^.tags THEN
        cmd.loadword(s^.proc^.scope,s^.proc^.addr);
        cmd.stot;
        cmd.c(M.cf);
      ELSE
        cmd.call(s^.proc^.scope,s^.proc^.addr);
      END;
    END call;

    PROCEDURE new_dynarr(p1,p2: ObjPtr);
    BEGIN
      IF p2=NIL THEN
--      cmd.li(-1);           cmd.ssw(0);
        cmd.c(M.drop); cmd.ePop;
        cmd.li(INTEGER(NIL)); cmd.ssw(0);
      ELSIF (p2^.mode=cons) & (p2^.addr=0) THEN
        obs.TypeCmp(p2^.type,obs.intp);
        cmd.pop_const;
--      cmd.li(-1);           cmd.ssw(0);
        cmd.c(M.drop); cmd.ePop;
        cmd.li(INTEGER(NIL)); cmd.ssw(0);
      ELSE
        obs.TypeCmp(p2^.type,obs.intp);
        IF obs.Char?(p1^.type^.base) THEN cmd.li(1)
        ELSE cmd.li(obs.tsize(p1^.type^.base)*4)
        END;
        find_resize;
        call; cmd.ePop; cmd.ePop; cmd.ePop; cmd.ePop;
      END;
    END new_dynarr;

    PROCEDURE dispose_dynarr(o: ObjPtr);
    BEGIN
      cmd.li(0);
      IF obs.Char?(p1^.type^.base) THEN cmd.li(1)
      ELSE cmd.li(obs.tsize(p1^.type^.base)*4)
      END;
      find_resize;
      call; cmd.ePop; cmd.ePop; cmd.ePop; cmd.ePop;
    END dispose_dynarr;

    PROCEDURE check_bit(t: obs.TypePtr; o: ObjPtr);
      VAR lo,hi: INTEGER;
    BEGIN
      IF t=obs.bitsetp THEN lo:=0; hi:=31 ELSE obs.LoHi(t^.base,lo,hi) END;
      range_check0(o,lo,hi);
    END check_bit;

  BEGIN
    IF (p1#NIL) & (p1^.mode=inv) THEN RETURN END;
    CASE s^.proc^.no OF
                       ----- procedures -----
      |_inc,_dec:
         obs.chkScalar(p1^.type);
         IF p2#NIL THEN obs.TypeCmp(p2^.type,obs.intp); cmd.ePop END;
         IF s^.proc^.no=_inc THEN
            IF p2=NIL THEN cmd.c(M.inc1) ELSE cmd.c(M.inc) END;
         ELSIF p2=NIL THEN cmd.c(M.dec1) ELSE cmd.c(M.dec)
         END;
         cmd.ePop;
      |_incl,_excl:
         IF NOT (p1^.type^.mode IN obs.SETs) THEN scan.err(21);
         ELSE obs.TypeCmp(p1^.type^.base,p2^.type);
           IF cmd.CPU>=1 THEN check_bit(p1^.type,p2) END;
           IF s^.proc^.no=_incl THEN cmd.c(M.incl) ELSE cmd.c(M.excl) END;
         END; cmd.ePop; cmd.ePop;
      |_halt:
         IF p1=NIL THEN cmd.raise(47h) ELSE cmd.ePop; cmd.raise(4Dh) END;
      |_assert: assert(p1,p2)
      |_new :
         IF    p1^.type^.mode=obs.ptr    THEN
           cmd.li(obs.tsize(p1^.type^.base));
           call; cmd.ePop; cmd.ePop;
         ELSIF p1^.type^.mode=obs.dynarr THEN new_dynarr(p1,p2)
         ELSE scan.err(21);
         END;
      |_dispose:
         IF    p1^.type^.mode=obs.dynarr THEN
           dispose_dynarr(p1)
         ELSIF p1^.type^.mode=obs.ptr    THEN
           cmd.li(obs.tsize(p1^.type^.base));
           call; cmd.ePop; cmd.ePop;
         ELSE scan.err(21)
         END;
      |_resize:
         IF p1^.type^.mode=obs.dynarr THEN
           obs.TypeCmp(p2^.type,obs.intp);
           IF obs.Char?(p1^.type^.base) THEN cmd.li(1)
           ELSE cmd.li(obs.tsize(p1^.type^.base)*4)
           END;
           call; cmd.ePop; cmd.ePop; cmd.ePop; cmd.ePop;
         ELSE scan.err(21)
         END;
      |_origin:
         obs.TypeCmp(p2^.type,obs.intp);
         IF obs.Char?(p1^.type^.base) THEN cmd.cMUL(4);
         ELSE cmd.cDIV(obs.tsize(p1^.type^.base),FALSE);
         END;
         cmd.cADD(-1);
         IF direct IN p1^.tags THEN
           cmd.storeword(p1^.scope,p1^.addr+1);
         ELSE cmd.ssw(1);
         END;
      |_dcopy:
         obs.TypeCmp(p1^.type,p2^.type);
         load_dynarr(p2);
         cmd.c(M.swap); cmd.li(2); cmd.move;
    ELSE ASSERT(scan.noErrors#0,100h+s^.proc^.no);
    END;
    IF p1#NIL THEN DISPOSE(p1) END;
    IF p2#NIL THEN DISPOSE(p2) END;
    cmd.mark_pos;
  END procs;

  PROCEDURE funcs(s: scall_ptr; p1,p2: ObjPtr);

    PROCEDURE high(v: ObjPtr; len: BOOLEAN; pos: cmd.POSITION);
      VAR lo,hi: INTEGER;
    BEGIN
      IF v^.mode=inv THEN RETURN END;
      IF v^.type^.mode=obs.arr THEN
        obs.LoHi(v^.type^.inx,lo,hi);
        IF    v^.mode=cons THEN cmd.pop_const
        ELSIF v^.mode#type THEN load_value(v); cmd.ePop; cmd.resume(pos);
        END;
        v^.mode:=cons;
        IF len THEN v^.addr:=hi-lo+1 ELSE v^.addr:=hi END;
        v^.type:=v^.type^.inx;
        cmd.li(v^.addr);
      ELSIF v^.type^.mode=obs.farr THEN
        load_farr_high(v);
        IF len THEN cmd.cADD(1) END;
        v^.mode:=stk; v^.type:=obs.intp;
        v^.tags:=v^.tags-{obs.seqpar,obs.varpar};
      ELSIF v^.type^.mode=obs.dynarr THEN
        load_dynamic_attr(v,1);
        IF len THEN cmd.cADD(1) END;
      ELSE scan.err(28); v^.mode:=inv;
      END;
      cmd.release(pos);
    END high;

    PROCEDURE size(v: ObjPtr; n: INTEGER; pos: cmd.POSITION);
                           -- n IN {1,4,32}
      VAR base: obs.TypePtr;
    BEGIN
      IF v^.mode=inv THEN cmd.release(pos); RETURN END;
      IF NOT (v^.type^.mode IN obs.Types{obs.farr,obs.dynarr}) THEN
        IF    v^.mode=cons THEN cmd.pop_const
        ELSIF v^.mode#type THEN load_value(v); cmd.ePop; cmd.resume(pos);
        END;
        v^.mode:=cons; v^.addr:=obs.tsize(v^.type)*n;
        v^.type:=obs.intp; cmd.li(v^.addr);
      ELSIF v^.mode=type THEN scan.err(38); v^.mode:=inv;
      ELSE base:=v^.type^.base;
        eval_high(v,TRUE);
        IF obs.Char?(base) THEN
          IF   n=32 THEN cmd.cADD(1); cmd.cMUL(8);       -- size in bits
          ELSIF n=4 THEN cmd.cADD(1);                  -- size in bytes
          ELSIF n=1 THEN cmd.cADD(4); cmd.cDIV(4,FALSE); -- size in words
          ELSE ASSERT(FALSE);
          END;
        ELSE cmd.cADD(1); cmd.cMUL(obs.tsize(base)*n)
        END;
      END;
      cmd.release(pos);
    END size;

    VAR lo,hi: INTEGER;
  BEGIN
    CASE s^.proc^.no OF
      |_adr :
         EXCL(p1^.tags,obs.RO);
         IF p1^.type^.mode=obs.dynarr THEN load_dynamic_attr(p1,0)
         ELSE var_adr(p1);
         END; p1^.type:=obs.addrp; p1^.mode:=stk;
      |_ref :
         IF p2^.mode#type          THEN scan.err(19); p1^.mode:=inv; RETURN END;
         IF p2^.type^.mode#obs.ptr THEN scan.err(51); p1^.mode:=inv; RETURN END;
         obs.TypeCmp(p1^.type,p2^.type^.base);
         var_adr(p1);
         p1^.type:=p2^.type;
      |_abs :
         IF p1^.type^.mode=obs.real THEN
           IF p1^.mode=cons THEN
             cmd.pop_const; p1^.addr:=INTEGER(ABS(REAL(p1^.addr))); cmd.li(p1^.addr);
           ELSE cmd.c(M.fabs)
           END;
         ELSE obs.TypeCmp(p1^.type,obs.intp);
           IF p1^.mode=cons THEN
             cmd.pop_const; p1^.addr:=ABS(p1^.addr); cmd.li(p1^.addr);
           ELSE cmd.c(M.abs)
           END;
         END;
         IF p1^.type^.mode=obs.rang THEN p1^.type:=p1^.type^.base END;
      |_odd :
         obs.TypeCmp(p1^.type,obs.intp); p1^.type:=obs.boolp;
         IF p1^.mode=cons THEN
           cmd.pop_const; p1^.addr:=INTEGER(ODD(p1^.addr)); cmd.li(p1^.addr);
         ELSE cmd.li(1); cmd.c(M.and); cmd.ePop;
         END;
      |_ord : obs.chkScalar(p1^.type); p1^.type:=obs.intp;
      |_chr :
         obs.chkScalar(p1^.type); p1^.type:=obs.charp;
         IF p1^.mode=cons THEN
           IF (p1^.addr<0) OR (p1^.addr>377b) THEN scan.err(44) END;
         ELSE cmd.li(377b); cmd.c(M.chkz); cmd.ePop;
         END;
      |_cap :
         obs.TypeCmp(p1^.type,obs.charp); p1^.type:=obs.charp;
         IF p1^.mode=cons THEN
           cmd.pop_const;
           p1^.addr:=INTEGER(BITSET(p1^.addr)-{5}); cmd.li(p1^.addr);
         ELSE cmd.li(INTEGER({5})); cmd.c(M.bic); cmd.ePop
         END;
      |_float:
         obs.TypeCmp(p1^.type,obs.intp); p1^.type:=obs.realp;
         IF p1^.mode=cons THEN
           cmd.pop_const; p1^.addr:=INTEGER(FLOAT(p1^.addr)); cmd.li(p1^.addr);
         ELSE cmd.c(M.ffct); cmd.b(0);
         END;
      |_trunc:
         obs.TypeCmp(p1^.type,obs.realp); p1^.type:=obs.intp;
         IF p1^.mode=cons THEN
           cmd.pop_const; p1^.addr:=TRUNC(REAL(p1^.addr)); cmd.li(p1^.addr);
         ELSE cmd.c(M.ffct); cmd.b(1);
         END;
      |_min,_max:
         IF p1^.mode#type THEN scan.err(22); p1^.mode:=inv; RETURN END;
         IF    p1^.type^.mode=obs.bitset  THEN lo:=0; hi:=31
         ELSIF p1^.type^.mode=obs.settype THEN obs.LoHi(p1^.type^.base,lo,hi)
         ELSE obs.chkScalar(p1^.type); obs.LoHi(p1^.type,lo,hi);
         END;
         IF s^.proc^.no=_min THEN p1^.addr:=lo ELSE p1^.addr:=hi END;
         p1^.mode:=cons; cmd.li(p1^.addr);
      |_high : high(p1,FALSE,s^.pos);
      |_len  : high(p1,TRUE ,s^.pos);
      |_size : size(p1,1 ,s^.pos);     -- число слов   в слове
      |_bytes: size(p1,4 ,s^.pos);     -- число байтов в слове
      |_bits : size(p1,32,s^.pos);     -- число битов  в слове
    ELSE ASSERT(scan.noErrors#0,100h+s^.proc^.no);
    END;
  END funcs;

  VAR s: scall_ptr;
BEGIN
  s:=scalls; scalls:=scalls^.next;
  IF s^.proc^.no>=0 THEN
    funcs(s,s^.p1,s^.p2); o:=s^.p1;
    IF s^.p2#NIL THEN DISPOSE(s^.p2) END;
  ELSE procs(s,s^.p1,s^.p2); o:=NIL;
  END;
  DISPOSE(s^.proc);
  DISPOSE(s);
END exitStandardCall;

-------------------------  PROC DCLs  -------------------------
                         -------------

TYPE
  context_ptr = POINTER TO context_rec;
  context_rec = RECORD
                 local_no: INTEGER;
                 on_stack: INTEGER;
                 locs_sum: INTEGER;
                 max_locs: INTEGER;
                 next    : context_ptr;
               END;

VAR
  context : context_ptr;   -- текущий контекст
  on_stack: INTEGER;       -- число слов параметров на стеке

PROCEDURE enterProc(proc: obs.ObjPtr);
  VAR new: context_ptr;
BEGIN
  NEW(new);
  new^.local_no:=local_no; local_no:=local_ofs; temp_no:=local_no;
  new^.on_stack:=on_stack; on_stack:=0;
  new^.locs_sum:=locs_sum; locs_sum:=0;
  new^.max_locs:=max_locs; max_locs:=0;
  new^.next:=context; context:=new;
  cmd.enter_proc;
END enterProc;

PROCEDURE exitProc(proc: obs.ObjPtr; put_xref: PUT_XREF);

  PROCEDURE proc_entry;
    VAR enter,ofs: INTEGER;
  BEGIN
    enter:=local_no-local_ofs;
    IF (on_stack=1) & (enter=1) THEN cmd.ePush; cmd.stot
    ELSE
      IF on_stack>3 THEN cmd.store; DEC(enter,on_stack+1); on_stack:=0 END;
      IF enter>0    THEN cmd.c(M.entr); cmd.b(enter) END;
      ofs:=local_ofs;
      WHILE on_stack>0 DO
        cmd.ePush; cmd.storeword(cmd.curlevel,ofs); INC(ofs); DEC(on_stack)
      END;
    END;
    cmd.save_mcode;
  END proc_entry;

  PROCEDURE restore_context;
    VAR old: context_ptr;
  BEGIN
    local_no:=context^.local_no; temp_no:=local_no;
    on_stack:=context^.on_stack;
    INC(max_locs,locs_sum);
    IF max_locs<context^.max_locs THEN max_locs:=context^.max_locs END;
    locs_sum:=context^.locs_sum;
    old:=context; context:=old^.next;
    DISPOSE(old);
  END restore_context;

BEGIN
--IF scan.noErrors#0 THEN RETURN END;
  IF proc^.type^.mode=obs.functype THEN cmd.raise(46h) END;
  return;
  pTab[proc^.addr]:=cmd.out_code_pos();
  proc_entry;
  cmd.exit_proc(proc^.addr,pTab[proc^.addr],put_xref);
  restore_context;
  ASSERT((cmd.depth=0) OR (scan.noErrors#0),100h+cmd.depth);
END exitProc;

PROCEDURE ProcEqu(real,pseudo: obs.ObjPtr);
BEGIN
  pTab[pseudo^.addr]:=pTab[real^.addr];
END ProcEqu;

PROCEDURE enterCodeProc(proc: obs.ObjPtr);
  VAR parm: obs.ParmPtr; d: INTEGER;
BEGIN ASSERT(obs.code_proc IN proc^.tags);
  parm:=proc^.type^.plist; d:=0;
  WHILE parm#NIL DO
    IF parm^.type^.mode=obs.farr THEN INC(d,2) ELSE INC(d) END;
    IF d>cmd.eStackDepth THEN scan.err(71) END;
    parm^.addr:=3;      -- доступ к параметру невозможен
    parm:=parm^.next
  END;
  cmd.enter_code_proc(proc^.addr);
END enterCodeProc;

PROCEDURE exitCodeProc(proc: obs.ObjPtr);
BEGIN ASSERT(obs.code_proc IN proc^.tags);
END exitCodeProc;

PROCEDURE code_expr(proc: obs.ObjPtr; o: ObjPtr);
  VAR t: obs.TypePtr; val: INTEGER;
BEGIN
  constexpr(o,val,t);
  obs.TypeCmp(t,obs.intp);
  IF (val<0) OR (val>255) THEN scan.err(48); RETURN END;
  cmd.app_code_expr(proc^.addr,val);
END code_expr;

----------------------------  DCLs  ---------------------------
                            --------

PROCEDURE dclProc(o: obs.ObjPtr);
BEGIN
  IF proc_no>255 THEN scan.Fault(79,'процедур'); proc_no:=0 END;
  o^.scope:=cmd.curlevel; o^.addr:=proc_no; INC(proc_no);
END dclProc;

PROCEDURE dclVar(o: obs.ObjPtr);
  VAR t: obs.TypePtr;
BEGIN t:=o^.type;
  IF (t^.mode=obs.dynarr) OR (t^.mode=obs.rec) & (t^.size<=2) THEN
    new_var(t^.size,o^.addr); INCL(o^.tags,direct);
  ELSE new_var(1,o^.addr);
  END;
  o^.scope:=cmd.curlevel;
  AllotVar(o);
END dclVar;

-------------------------  PARAMETERs  ------------------------
                         --------------

PROCEDURE words(p: obs.ParmPtr): INTEGER;
BEGIN
  IF p^.type^.mode=obs.farr THEN RETURN 2 ELSE RETURN 1 END;
END words;

PROCEDURE scanParms(p: obs.ParmPtr);
  VAR stack: inter.QUEUE; w,parmno,d,varno: INTEGER;
BEGIN inter.lifo(stack);
  WHILE p#NIL DO inter.push(stack,p); p:=p^.next END;
  d:=0; varno:=local_ofs;
  LOOP
    IF NOT inter.pop(stack,p) THEN inter.clear(stack); RETURN END;
    w:=words(p); INC(d,w);
    IF d>5 THEN EXIT END;        -- d>cmd.eStackDepth
    INC(varno,w-1);
    p^.addr:=varno; INC(varno);
  END; ASSERT(p#NIL);
  parmno:=0;
  LOOP
    p^.addr:=-parmno; INC(parmno,w);
    IF parmno>256 THEN scan.Fault(79,'параметров'); EXIT END;
    IF NOT inter.pop(stack,p) THEN EXIT END;
    w:=words(p);
  END;
  inter.clear(stack);
END scanParms;

PROCEDURE dclParm(o: obs.ObjPtr; p: obs.ParmPtr);
  VAR w: INTEGER;
BEGIN
  IF p^.addr>0 THEN
    w:=words(p); INC(local_no,w); temp_no:=local_no; INC(on_stack,w);
  END;
  o^.scope:=cmd.curlevel; o^.addr:=p^.addr;
  IF (p^.kind={}) & NOT obs.Simple?(o^.type) THEN copyParm(o) END;
END dclParm;

--------------------  SYM FILE GENERATION  --------------------
                    -----------------------

PROCEDURE set_counts(p_no,v_no: INTEGER);
BEGIN proc_no:=p_no; local_no:=v_no; temp_no:=local_no;
END set_counts;

PROCEDURE extProc(o: obs.ObjPtr; extno,ofs: INTEGER);
BEGIN ASSERT(o^.mode=obs.proc);
  o^.scope:=-extno; o^.addr:=ofs;
END extProc;

PROCEDURE extVar(o: obs.ObjPtr; extno,ofs: INTEGER);
BEGIN ASSERT(o^.mode=obs.vari);
  o^.scope:=-extno; o^.addr:=ofs;
  IF extno=0 THEN AllotVar(o) END;
END extVar;

PROCEDURE extStruct(o: obs.ObjPtr; extno,ofs,len: INTEGER; one: GET_BYTE);
  VAR x,size: INTEGER;
BEGIN
  o^.scope:=-extno; o^.addr:=ofs;
  size:=obs.tsize(o^.type);
  IF len#size THEN
    ASSERT(len MOD 4=0);
    len:=len DIV 4;
    ASSERT(len=size);
  END;
  IF extno=0 THEN
    cmd.put_bytes(ofs,len,one);
  ELSE
    len:=len*4;
    WHILE len>0 DO x:=one(); DEC(len) END;
  END;
END extStruct;

PROCEDURE get_struct(o: obs.ObjPtr; one: PUT_BYTE);
BEGIN
  ASSERT(o^.mode=obs.cons);
  ASSERT(o^.scope=0);
  cmd.iter_bytes(o^.addr,obs.tsize(o^.type),one);
END get_struct;

--------------------  CODE FILE GENERATION  -------------------
                    ------------------------

VAR    code: comp.io_ptr;
  externals: inter.QUEUE;

PROCEDURE save_ext(o: obs.ObjPtr);
BEGIN inter.push(externals,o)
END save_ext;

PROCEDURE write(VAL x: ARRAY OF SYSTEM.WORD; size: INTEGER);
BEGIN
(*$<U+*)
  code^.buf^.ADR :=SYSTEM.ADR(x);
  code^.buf^.HIGH:=BYTES(x)-1;
(*$>*)
  code^.len:=size*4;
  code^.doio(code);
END write;

PROCEDURE GenCode(VAL name: ARRAY OF CHAR;
                  put_xref: PUT_XREF;
                  def_time: INTEGER;
                  imp_time: INTEGER;
                       tag: INTEGER;
              VAR codesize: INTEGER);

  PROCEDURE final;
    VAR seg,len: INTEGER;
  BEGIN
    cmd.li(0); return;
    pTab[0]:=cmd.out_code_pos();
    cmd.exit_proc(0,pTab[0],put_xref);
    cmd.correct_case_tables(proc_no);
  END final;

  PROCEDURE write_atrs;

    VAR atrs: RECORD
                vers     : BITSET;
                def_time : INTEGER;
                imp_time : INTEGER;
                str_size : INTEGER;
                code_size: INTEGER;
                min_stack: INTEGER;
                add_stack: INTEGER;
                glo_size : INTEGER;
                no_exts  : INTEGER;
                no_proc  : INTEGER;
                no_mg    : INTEGER;
                unused__B: INTEGER;
                language : ARRAY [0..3] OF CHAR;
                mod_tag  : INTEGER;
                unused__E: INTEGER;
                unused__F: INTEGER;
              END;

  BEGIN
    atrs.vers     :=BITSET(102h)+BITSET(cmd.CPU)>>8;
    atrs.def_time :=def_time;
    atrs.imp_time :=imp_time;
    atrs.glo_size :=local_no+obs.extNo;
    atrs.min_stack:=0;
    atrs.add_stack:=max_locs+scan.add_stk;
    atrs.no_proc  :=proc_no;
    atrs.no_exts  :=obs.extNo;
    atrs.str_size :=cmd.pool_ofs;
    atrs.code_size:=codesize+proc_no;
    atrs.no_mg    :=glo_co;
    atrs.unused__B:=0;
    atrs.language :='mx';
    atrs.mod_tag  :=tag;
    atrs.unused__E:=0;
    atrs.unused__F:=0;
    write(atrs,16);
  END write_atrs;

  PROCEDURE proc_table;
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE i<proc_no DO INC(pTab[i],proc_no*4); INC(i) END;
  END proc_table;

  PROCEDURE write_ext(o: obs.ObjPtr);
    VAR name: ARRAY [0..31] OF CHAR; i: INTEGER;
        time: ARRAY [0..0] OF INTEGER;
  BEGIN
    scan.id_str(o^.id,name); i:=0;
    WHILE (i<=HIGH(name)) & (name[i]#0c) DO INC(i) END;
    REPEAT name[i]:=0c; INC(i) UNTIL (i MOD 4)=0;
    time[0]:=o^.head^.ctime;
    write(time,1);
    write(name,i DIV 4);
  END write_ext;

  PROCEDURE write_externals;
    VAR o: obs.ObjPtr;
  BEGIN
    inter.lifo(externals);
    obs.iterModules(save_ext);
    WHILE inter.pop(externals,o) DO write_ext(o) END;
    inter.clear(externals);
  END write_externals;

  VAR mg: ARRAY [0..1] OF INTEGER; x: global_ptr;
    mode: INTEGER;
BEGIN
  final; proc_table;
  codesize:=(cmd.out_code_pos()+3) DIV 4;
  IF def_time=imp_time THEN mode:=comp.mcode ELSE mode:=comp.code END;
  inter.ini(code,name,mode,scan.io_fault);
  IF NOT code^.done THEN RETURN END;
  write_atrs;
  cmd.write_pool(write);
  write(pTab,proc_no);
  cmd.write_code(write);
  WHILE globals#NIL DO
    x:=globals; globals:=x^.next;
    mg[0]:=x^.ofs; mg[1]:=x^.size;
    write(mg,SIZE(mg));
  END;
  write_externals;
  inter.exi(code);
END GenCode;

-------------------------  INI & EXI  -------------------------
                         -------------

PROCEDURE Ini(name: ARRAY OF CHAR; cpu: INTEGER);
  VAR i: INTEGER;
BEGIN
  cmd.CPU:=cpu;
  cmd.Ini;
  proc_no:=1; max_locs:=0; locs_sum:=0; globals:=NIL; glo_co:=0;
  ini_structs;
  calls:=NIL; scalls:=NIL;
  context:=NIL; on_stack:=0; local_no:=global_ofs; temp_no:=local_no;
  i:=0;
  WHILE (i<=HIGH(name)) & (name[i]#0c) DO INC(i) END;  INC(i);
  WHILE (i<=HIGH(name)) & (i MOD 4#0)  DO name[i]:=0c; INC(i) END;
  IF cmd.app_str(name,i)#0 THEN ASSERT(FALSE) END;
END Ini;

PROCEDURE Exi; BEGIN exi_structs; cmd.Exi END Exi;

PROCEDURE cpu(): INTEGER;
BEGIN RETURN cmd.CPU
END cpu;

END mxGen.
