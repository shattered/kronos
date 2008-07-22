IMPLEMENTATION MODULE adaGen; (* 30-Aug-90. (c) KRONOS *)

IMPORT tbl : adaTable;
IMPORT lex : adaLex;

FROM SYSTEM     IMPORT  WORD, ADR;
FROM adaTable   IMPORT  item;
FROM adaDebug   IMPORT  print_cmd;
FROM Terminal   IMPORT  print;

CONST
  Greg=1;
  Sreg=2;
  Hreg=3;

TYPE
  proc=PROCEDURE (item);

VAR
  Lreg    : INTEGER;
  code    : ARRAY [0..999] OF INTEGER;
  free    : INTEGER;
  restart : BOOLEAN;
  reg     : INTEGER;
  pack    : item;
  cur_proc: item;
  break   : INTEGER;
  delta_s : INTEGER; -- in bytes

PROCEDURE put(n: WORD);
BEGIN
  code[free]:=n; INC(free);
END put;

PROCEDURE operators(h: item; p: proc);
  PROCEDURE one(i: item);
  BEGIN
    WHILE i#NIL DO
      IF restart THEN RETURN END;
      IF i^.md=tbl.cl_pack THEN one(i^.pa_list)
      ELSIF i^.md=tbl.cl_operator THEN p(i)
      END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  IF h^.md=tbl.cl_pack THEN one(h^.pa_list) END;
  IF h^.md=tbl.cl_func THEN one(h^.fu_list) END;
  IF h^.md=tbl.cl_operator THEN one(h^.op_list) END;
END operators;

PROCEDURE local_vars(h: item; p: proc);
  PROCEDURE one(i: item);
  BEGIN
    WHILE i#NIL DO
      IF i^.md=tbl.cl_pack THEN one(i^.pa_list)
      ELSIF i^.md=tbl.cl_operator THEN one(i^.op_list)
      ELSIF i^.md=tbl.cl_var THEN p(i)
      END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  IF h^.md=tbl.cl_func THEN one(h^.fu_list) END;
END local_vars;

PROCEDURE procedures(h: item; p: proc);
  PROCEDURE one(i: item);
  BEGIN
    WHILE i#NIL DO
      IF i^.md=tbl.cl_pack THEN one(i^.pa_list);
      ELSIF i^.md=tbl.cl_func THEN one(i^.fu_list); p(i);
      ELSIF i^.md=tbl.cl_operator THEN one(i^.op_list);
      END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  ASSERT(h^.md=tbl.cl_pack);
  one(h^.pa_list);
END procedures;

PROCEDURE host?(i: item): item;
-- выдает еденицу компиляции или процедуру которой описан объект i
BEGIN
  ASSERT(i#NIL);
  LOOP
    IF i^.md=tbl.cl_func THEN RETURN i END;
    IF i^.host=NIL THEN RETURN i END;
    i:=i^.host;
  END;
END host?;

PROCEDURE put_li(val,reg: INTEGER);
BEGIN
  IF (val>=0FFFFF000h) & (val<=0FFFh) THEN
    put({13,19}+BITSET(reg<<25)+BITSET(val)*{0..12});
  ELSE
    put({22,23,24,30}+BITSET(reg<<25)+BITSET(val>>10)*{0..21});
    put({13,19}+BITSET(reg<<25)+BITSET(val)*{0..9});
  END;
END put_li;

PROCEDURE put_load_imm(reg_adr,off,reg_dest,size: INTEGER; signed: BOOLEAN);
  VAR s: BITSET;
BEGIN
  IF (off>=0FFFFF000h) & (off<=0FFFh) THEN
    s:={13,30,31}+BITSET(reg_dest<<25)+BITSET(reg_adr<<14)+
       BITSET(off)*{0..12};
    IF NOT signed THEN INCL(s,22) END;
    ASSERT((size MOD 8)=0);
    IF size=16 THEN INCL(s,20) END;
    IF size=32 THEN INCL(s,21) END;
    put(s);
  ELSE
    print('слишком большое смещение\n');
  END;
END put_load_imm;

PROCEDURE put_load(reg_adr,reg_off,reg_dest,size: INTEGER; signed: BOOLEAN);
  VAR s: BITSET;
BEGIN
  s:={30,31}+BITSET(reg_dest<<25)+BITSET(reg_adr<<14)+BITSET(reg_off);
  IF NOT signed THEN INCL(s,22) END;
  ASSERT((size MOD 8)=0);
  IF size=16 THEN INCL(s,20) END;
  IF size=32 THEN INCL(s,21) END;
  put(s);
END put_load;

PROCEDURE put_store_imm(reg_adr,off,reg_dat,size: INTEGER; signed: BOOLEAN);
  VAR s: BITSET;
BEGIN
  IF (off>=0FFFFF000h) & (off<=0FFFh) THEN
    s:={13,23,30,31}+BITSET(reg_dat<<25)+BITSET(reg_adr<<14)+
       BITSET(off)*{0..12};
    IF NOT signed THEN INCL(s,22) END;
    ASSERT((size MOD 8)=0);
    IF size=16 THEN INCL(s,20) END;
    IF size=32 THEN INCL(s,21) END;
    put(s);
  ELSE
    print('слишком большое смещение\n');
  END;
END put_store_imm;

PROCEDURE put_store(reg1,reg2,reg_dat,size: INTEGER; signed: BOOLEAN);
  VAR s: BITSET;
BEGIN
  s:={23,30,31}+BITSET(reg_dat<<25)+BITSET(reg1<<14)+BITSET(reg2);
  IF NOT signed THEN INCL(s,22) END;
  ASSERT((size MOD 8)=0);
  IF size=16 THEN INCL(s,20) END;
  IF size=32 THEN INCL(s,21) END;
  put(s);
END put_store;

PROCEDURE put_move(fr,to: INTEGER);
BEGIN
  IF fr=to THEN RETURN END;
  put({19}+BITSET(to<<25)+BITSET(fr));
END put_move;

PROCEDURE put_add_imm(fr,to,val: INTEGER);
BEGIN
  IF val=0 THEN put_move(fr,to); RETURN END;
  IF (val>=-8000h) & (val<=0FFFh) THEN
    put({13,19,20}+BITSET(fr<<14)+BITSET(to<<25)+BITSET(val)*{0..12});
  ELSE
    put_li(val,reg);
    put({19,20}+BITSET(fr<<14)+BITSET(to<<25)+BITSET(reg));
  END;
END put_add_imm;

PROCEDURE put_mul_imm(fr,to,val: INTEGER);
  VAR d,d0,d1,i: INTEGER; fst: BOOLEAN;
BEGIN
  ASSERT(val>0);
  IF val=1 THEN put_move(fr,to); RETURN END;
  fst:=TRUE;
  FOR i:=0 TO 30 DO
    IF i IN BITSET(val) THEN
      val:=INTEGER(BITSET(val)-{i});
      IF fst THEN
        fst:=FALSE;
        IF (fr=to) & (val#0) THEN d:=reg ELSE d:=to END;
        put({13,19,21}+BITSET(d<<25)+BITSET(fr<<14)+BITSET(i));
      ELSE
        IF d=reg THEN d0:=reg+1 ELSE d0:=reg END;
        put({13,19,21}+BITSET(d0<<25)+BITSET(fr<<14)+BITSET(i));
        IF (fr=to) & (val#0) THEN d1:=reg ELSE d1:=to END;
        put({19,20}+BITSET(d1<<25)+BITSET(d<<14)+BITSET(d0));
        d:=d1;
      END;
    END;
  END;
END put_mul_imm;

PROCEDURE put_block_move(r1,r2,sz: INTEGER);
  VAR r3,r4: INTEGER;
BEGIN
  r3:=reg; INC(reg); r4:=reg; INC(reg);
  put_li((sz+7) DIV 8,r3);
  put_load(r2,0,r4,8,FALSE);
  put_add_imm(r2,r2,1);
  put_store(r1,0,r4,8,FALSE);
  put({21,23,13,0}+BITSET(r3<<25)+BITSET(r3<<14)); -- sub CC
  put({30,25,26,27,24}+BITSET(-4)*{0..21}); -- br ^Z #-4
  put_add_imm(r1,r1,1);
  break:=free;
  DEC(reg,2);
END put_block_move;

PROCEDURE last_destination(n: INTEGER): BOOLEAN;
  VAR s: BITSET;
BEGIN
  IF free=break THEN RETURN TRUE END;
  s:=BITSET(code[free-1]);
  IF (s*{30,31,22,23,24}={30,22,23,24}) OR (s*{30,31,23}={30,31,23}) OR
     (s*{30,31}={}) THEN RETURN n=INTEGER(s*{25..29}>>25) END;
  RETURN TRUE;
END last_destination;

PROCEDURE array_size(t: item; n: INTEGER): INTEGER;
  VAR sz,i: INTEGER; k: item;
BEGIN
  ASSERT(t^.md=tbl.cl_subtype);
  k:=t^.su_type^.ty_type;
  ASSERT(k^.md=tbl.cl_array);
  sz:=k^.ar_elembits;
  ASSERT(sz MOD 8 = 0);
  FOR i:=n+1 TO k^.ar_index_no-1 DO
    sz:=sz*(t^.su_descr[i]^.su_last-t^.su_descr[i]^.su_first+1);
  END;
  RETURN sz;
END array_size;

PROCEDURE packed_subtype_size(t: item): INTEGER;
  VAR i,x: item; j,k: INTEGER;
BEGIN
  IF t=NIL THEN RETURN 0 END; ASSERT(t^.md=tbl.cl_subtype);
  IF t^.su_type=NIL THEN RETURN 0 END;
  IF t^.su_type^.ty_type=NIL THEN RETURN 0 END;
  i:=t^.su_type^.ty_type;
  CASE i^.md OF
    |tbl.cl_integer: RETURN i^.in_bits;
    |tbl.cl_enumeration: RETURN i^.en_bits;
    |tbl.cl_record: RETURN i^.re_bits;
    |tbl.cl_array:
      j:=1;
      FOR k:=0 TO i^.ar_index_no-1 DO
        x:=t^.su_descr[k]; IF x=NIL THEN RETURN 0 END;
        ASSERT(x^.md=tbl.cl_subtype);
        j:=(x^.su_last-x^.su_first+1)*j;
        IF j<0 THEN j:=0 END;
      END;
      RETURN j*i^.ar_elembits;
  ELSE
    RETURN 32;
  END;
END packed_subtype_size;

PROCEDURE subtype_size(t: item): INTEGER;
  VAR n: INTEGER;
BEGIN
  n:=packed_subtype_size(t);
  IF n=0 THEN RETURN 0 END;
  IF n<=8 THEN RETURN 8 END;
  IF n<=16 THEN RETURN 16 END;
  RETURN n+(32-n) MOD 32;
END subtype_size;

PROCEDURE signed?(v: item): BOOLEAN;
  VAR t: item;
BEGIN
  IF v^.md=tbl.cl_var THEN v:=v^.va_type END;
  ASSERT(v^.md=tbl.cl_subtype);
  t:=v^.su_type^.ty_type;
  CASE t^.md OF
    |tbl.cl_enumeration: RETURN t^.en_signed;
    |tbl.cl_integer: RETURN t^.in_signed;
  ELSE
    RETURN FALSE;
  END;
END signed?;

PROCEDURE constant?(i: item; VAR c: INTEGER): BOOLEAN;
BEGIN
  IF (i^.md=tbl.cl_var) & (i^.va_rl=1) THEN
    c:=i^.va_val0; RETURN TRUE;
  ELSIF i^.md=tbl.cl_operator THEN
    IF i^.op_lex=lex.int_number THEN
      c:=i^.op_val0; RETURN TRUE;
    ELSIF i^.op_lex=lex.procedure THEN
      IF i^.op_dw[0]^.fu_rl=2 THEN
        c:=i^.op_dw[0]^.fu_val1; RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END constant?;

PROCEDURE in_register?(i: item; VAR r: INTEGER): BOOLEAN;
  VAR c: INTEGER;
BEGIN
  IF constant?(i,c) THEN IF c=0 THEN r:=0 END; RETURN c=0 END;
  IF i^.md=tbl.cl_var THEN
    IF i^.va_rl=4 THEN r:=i^.va_val1; RETURN TRUE END;
  ELSIF i^.md=tbl.cl_prm THEN
    IF i^.pr_rl=1 THEN r:=i^.pr_val0; RETURN TRUE END;
  END;
  RETURN FALSE;
END in_register?;

PROCEDURE adr_in_register?(i: item; VAR r: INTEGER): BOOLEAN;
BEGIN
  IF i^.md=tbl.cl_var THEN
    IF i^.va_rl=5 THEN r:=i^.va_val1; RETURN TRUE END;
    IF (i^.va_rl=6) & (i^.va_val0=0) THEN r:=Lreg; RETURN TRUE END;
  ELSIF i^.md=tbl.cl_prm THEN
    IF i^.pr_rl=2 THEN r:=i^.pr_val0; RETURN TRUE END;
  END;
  RETURN FALSE;
END adr_in_register?;

PROCEDURE access_in_register?(i: item; VAR r: INTEGER): BOOLEAN;
BEGIN
  IF i^.md=tbl.cl_var THEN
    IF i^.va_type^.su_type^.ty_type^.md=tbl.cl_access THEN
      RETURN in_register?(i,r);
    ELSE
      RETURN adr_in_register?(i,r);
    END;
  ELSIF i^.md=tbl.cl_prm THEN
    IF i^.pr_type^.su_type^.ty_type^.md=tbl.cl_access THEN
      RETURN in_register?(i,r);
    ELSE
      RETURN adr_in_register?(i,r);
    END;
  END;
  RETURN FALSE;
END access_in_register?;

PROCEDURE short_constant?(i: item; VAR c: INTEGER): BOOLEAN;
BEGIN
  RETURN constant?(i,c) & (c>=-8000h) & (c<=0FFFh);
END short_constant?;

PROCEDURE value_to_reg(e: item; r: INTEGER; busy: BOOLEAN);
FORWARD;
PROCEDURE adr_to_reg(e: item; r: INTEGER);
FORWARD;
PROCEDURE gen_array_index(ar: item; VAR base,index: INTEGER;
                          VAR index_const: BOOLEAN; VAR type: item);
FORWARD;
PROCEDURE access_to_reg(e: item; r: INTEGER);
FORWARD;

PROCEDURE operation_to_reg(e: item; r: INTEGER; busy: BOOLEAN);
  VAR r1,r0,c,sv_reg: INTEGER; s: BITSET; left,right: item;
BEGIN
  ASSERT(e^.op_dw[0]^.md=tbl.cl_func);
  sv_reg:=reg;
  IF e^.op_dw_no=3 THEN
    left:=e^.op_dw[1]; right:=e^.op_dw[2];
    s:={}; r0:=0; r1:=0;
    IF short_constant?(right,c) THEN
      s:=s+{13}+BITSET(c)*{0..12};
      IF NOT in_register?(left,r0) THEN
        value_to_reg(left,r,busy); r0:=r;
      END;
    ELSIF (e^.op_dw[0]^.fu_val0#2) & short_constant?(left,c) THEN
      s:=s+{13}+BITSET(c)*{0..12};
      IF NOT in_register?(right,r0) THEN
        value_to_reg(right,r,busy); r0:=r;
      END;
    ELSE
      IF NOT in_register?(left,r0) THEN
        IF busy THEN
          r0:=reg; INC(reg);
          value_to_reg(left,r0,FALSE);
        ELSE
          value_to_reg(left,r,FALSE); r0:=r;
        END;
      END;
      IF NOT in_register?(right,r1) THEN
        r1:=reg; INC(reg);
        value_to_reg(right,r1,FALSE);
      END;
    END;
    s:=s+BITSET(r<<25)+BITSET(r0<<14)+BITSET(r1);
    CASE e^.op_dw[0]^.fu_val0 OF
      |1: put(s+{19,20}) -- add
      |2: put(s+{21})    -- sub
      |3: put(s)         -- and
      |4: put(s+{19})    -- or
      |5: put(s+{20})    -- xor
    ELSE
      lex.error(e^.lex_pos,'не реализована операция %d',e^.op_dw[0]^.fu_val0);
    END;
  ELSE
    lex.error(e^.lex_pos,'не реализована унарная операция');
  END;
  reg:=sv_reg;
END operation_to_reg;

PROCEDURE value_to_reg(e: item; r: INTEGER; busy: BOOLEAN);
-- порождает код для вычисления выражения e и записи
--   результата в регистр reg
-- busy - регистр нельзя использовать для промежуточного хранения
  VAR sv_reg,base,index,k: INTEGER; type: item; index_cns: BOOLEAN;
BEGIN
  IF constant?(e,k) THEN
    put_li(k,r);
  ELSIF e^.md=tbl.cl_var THEN
    IF e^.va_sz>32 THEN
      lex.error(e^.lex_pos,'здесь не допустимо мультизначение');
    ELSE
      CASE e^.va_rl OF
        |1: put_li(e^.va_val0,r);
        |2: IF host?(e)#pack THEN
              lex.error(e^.lex_pos,'не реализовано чтение чужих глобалов');
            ELSE
              put_load_imm(Greg,e^.va_val0,r,e^.va_sz,signed?(e));
            END;
        |4: put_move(e^.va_val1,r);
        |6: put_load_imm(2,e^.va_val0,r,e^.va_sz,signed?(e));
      ELSE
        lex.error(e^.lex_pos,'не реализовано va_rl %d',e^.va_rl);
      END;
    END;
  ELSIF (e^.md=tbl.cl_operator) &
        (e^.op_lex=lex.procedure) &
        (e^.op_dw[0]^.fu_rl=1) THEN
    -- вызов операции
    operation_to_reg(e,r,busy);
  ELSIF (e^.md=tbl.cl_operator) & (e^.op_lex=lex.array) THEN
    sv_reg:=reg;
    gen_array_index(e,base,index,index_cns,type);
    k:=array_size(type,e^.op_dw_no-1);
    IF k>32 THEN
      lex.error(e^.lex_pos,'не реализовано sz>32 -> reg');
    ELSE
      IF index_cns THEN
        IF last_destination(base) THEN put(0) END;
        put_load_imm(base,index,r,k,signed?(type^.su_type^.ty_type^.ar_elemtp));
      ELSE
        IF last_destination(base) OR last_destination(index) THEN put(0) END;
        put_load(base,index,r,k,signed?(type^.su_type^.ty_type^.ar_elemtp));
      END;
    END;
    reg:=sv_reg;
  ELSIF (e^.md=tbl.cl_operator) & (e^.op_lex=lex.point) THEN
    sv_reg:=reg;
    IF NOT access_in_register?(e^.op_dw[0],base) THEN
      base:=reg; INC(reg); access_to_reg(e^.op_dw[0],base);
    END;
    ASSERT(e^.op_dw[1]^.md=tbl.cl_field);
    k:=e^.op_dw[1]^.fi_bits;
    index:=e^.op_dw[1]^.fi_offs DIV 8;
    IF k>32 THEN
      lex.error(e^.lex_pos,'не реализовано sz>32 -> reg');
    ELSE
      IF last_destination(base) THEN put(0) END;
      put_load_imm(base,index,r,k,signed?(e^.op_dw[1]^.fi_type));
    END;
    reg:=sv_reg;
  ELSE
    lex.error(e^.lex_pos,'не реализовано value_to_reg');
  END;
END value_to_reg;

PROCEDURE constant_array_agregate(e: item; r: INTEGER): BOOLEAN;
  VAR sz,i,j,k,sv_free,rang,c: INTEGER; type: item;
    ptr: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
BEGIN
  sv_free:=free;
  type:=e^.op_dw[0]; rang:=e^.op_val0;
  ASSERT(type#NIL);
  sz:=array_size(type,rang);
  put({31}+BITSET(2+(sz+31) DIV 32)); -- call
  put_add_imm(31,r,8);
  ptr:=ADR(code[free]);
  INC(free,(sz+31) DIV 32);
  sz:=array_size(type,rang+1); j:=0;
  IF sz>32 THEN free:=sv_free; RETURN FALSE END;
  FOR i:=1 TO e^.op_dw_no-1 DO
    IF NOT constant?(e^.op_dw[i],c) THEN free:=sv_free; RETURN FALSE END;
    FOR k:=0 TO sz DIV 8 - 1 DO
      ptr^[j]:=CHAR(c); c:=c>>8; INC(j);
    END;
  END;
  break:=free;
  RETURN TRUE;
END constant_array_agregate;

PROCEDURE array_agregate(e: item; r: INTEGER);
  VAR sz,i,j,rang,r1: INTEGER; type: item; sig: BOOLEAN;
BEGIN
  type:=e^.op_dw[0]; rang:=e^.op_val0; sz:=array_size(type,rang);
  delta_s:=(sz+31) DIV 32; put_move(Sreg,r); put_add_imm(Sreg,Sreg,delta_s);
  sz:=array_size(type,rang+1); j:=0;
  IF sz>32 THEN
    lex.error(e^.lex_pos,'не реализовано'); RETURN
  END;
  ASSERT(sz MOD 8 = 0);
  IF rang+1=type^.su_last-1 THEN
    sig:=signed?(type^.su_type^.ty_type^.ar_elemtp)
  ELSE
    sig:=FALSE;
  END;
  r1:=reg; INC(reg);
  FOR i:=1 TO e^.op_dw_no-1 DO
    value_to_reg(e^.op_dw[i],r1,FALSE);
    put_store_imm(r,j,r1,sz,sig);
    INC(j,sz DIV 8);
  END;
  DEC(reg);
END array_agregate;

PROCEDURE adr_to_reg(e: item; r: INTEGER);
-- порождает код для вычисления адреса имени e и записи
--   результата в регистр reg
  VAR base,index: INTEGER;
BEGIN
  IF e^.md=tbl.cl_var THEN
    CASE e^.va_rl OF
      |2: IF host?(e)#pack THEN
            lex.error(e^.lex_pos,'не реализовано adr_to_reg чужого глобала');
          ELSE
            put_add_imm(Greg,r,e^.va_val0);
          END;
      |3: put_load_imm(Greg,e^.va_val0,r,32,FALSE);
      |5: put_move(e^.va_val1,r);
      |6: put_add_imm(Lreg,r,e^.va_val0);
    ELSE
      lex.error(e^.lex_pos,'не реализовано va_rl %d',e^.va_rl);
    END;
  ELSIF e^.md=tbl.cl_prm THEN
    CASE e^.pr_rl OF
      |1: lex.error(e^.lex_pos,'не реализовано adr_to_reg параметра 1');
      |2: put_move(e^.pr_val0,r);
    ELSE
      lex.error(e^.lex_pos,'не реализовано pr_rl %d',e^.pr_rl);
    END;
  ELSIF (e^.md=tbl.cl_operator) & (e^.op_lex=lex.left_parenthesis) THEN
    IF NOT constant_array_agregate(e,r) THEN array_agregate(e,r) END;
  ELSIF (e^.md=tbl.cl_operator) & (e^.op_lex=lex.point) THEN
    IF NOT access_in_register?(e^.op_dw[0],base) THEN
      base:=r; access_to_reg(e^.op_dw[0],base);
    END;
    IF (e^.op_dw[1]^.md=tbl.cl_operator) &
       (e^.op_dw[1]^.op_lex=lex.all) THEN
      index:=0;
    ELSE
      ASSERT(e^.op_dw[1]^.md=tbl.cl_field);
      index:=e^.op_dw[1]^.fi_offs DIV 8;
    END;
    put_add_imm(base,r,index);
  ELSE
    lex.error(e^.lex_pos,'не реализовано adr_to_reg');
  END;
END adr_to_reg;

PROCEDURE access_to_reg(e: item; r: INTEGER);
  VAR t: item; adr: BOOLEAN;
BEGIN
  IF e^.md=tbl.cl_var THEN
    adr:=e^.va_type^.su_type^.ty_type^.md#tbl.cl_access;
  ELSIF e^.md=tbl.cl_prm THEN
    adr:=e^.pr_type^.su_type^.ty_type^.md#tbl.cl_access;
  ELSIF (e^.md=tbl.cl_operator) & (e^.op_lex=lex.point) THEN
    IF (e^.op_dw[1]^.md=tbl.cl_operator) &
       (e^.op_dw[1]^.op_lex=lex.all) THEN
      t:=e^.op_dw[1]^.op_dw[0];
    ELSE
      ASSERT(e^.op_dw[1]^.md=tbl.cl_field);
      t:=e^.op_dw[1]^.fi_type;
    END;
    adr:=t^.su_type^.ty_type^.md#tbl.cl_access;
  ELSE
    lex.error(e^.lex_pos,'не реализовано access_to_reg'); RETURN;
  END;
  IF adr THEN adr_to_reg(e,r) ELSE value_to_reg(e,r,FALSE) END;
END access_to_reg;

PROCEDURE type?(i: item): item;
BEGIN
  IF i^.md=tbl.cl_var THEN
    RETURN i^.va_type;
  ELSE
    lex.error(i^.lex_pos,'не реализовано type? %.');
  END;
END type?;

PROCEDURE array_first(t: item; n: INTEGER): INTEGER;
  VAR k: item;
BEGIN
  ASSERT(t^.md=tbl.cl_subtype);
  k:=t^.su_type^.ty_type;
  ASSERT(k^.md=tbl.cl_array);
  RETURN t^.su_descr[n]^.su_first;
END array_first;

PROCEDURE gen_array_index(ar: item; VAR base,index: INTEGER;
                          VAR index_const: BOOLEAN; VAR type: item);
  VAR rr,r2,k,summ,cns: INTEGER; first: BOOLEAN;
BEGIN
  ASSERT(ar^.md=tbl.cl_operator);
  ASSERT(ar^.op_lex=lex.array);
  ASSERT(ar^.op_dw_no>1);
  IF NOT adr_in_register?(ar^.op_dw[0],base) THEN
    base:=reg; INC(reg); adr_to_reg(ar^.op_dw[0],base);
  END;
  type:=type?(ar^.op_dw[0]); summ:=0; first:=TRUE;
  FOR k:=1 TO ar^.op_dw_no-1 DO
    IF constant?(ar^.op_dw[k],cns) THEN
      summ:=summ+(cns-array_first(type,k-1))*(array_size(type,k-1) DIV 8);
    ELSIF first THEN
      index:=reg; INC(reg); first:=FALSE;
      IF in_register?(ar^.op_dw[1],rr) THEN
        IF array_first(type,0)#0 THEN
          put_add_imm(rr,index,-array_first(type,0)); rr:=index;
        END;
        put_mul_imm(rr,index,array_size(type,0) DIV 8);
      ELSE
        value_to_reg(ar^.op_dw[1],index,FALSE); rr:=index;
        put_add_imm(index,index,-array_first(type,0));
        put_mul_imm(index,index,array_size(type,0) DIV 8);
      END;
    ELSE
      r2:=reg; INC(reg);
      value_to_reg(ar^.op_dw[k],r2,FALSE);
      put_add_imm(r2,r2,-array_first(type,k));
      put_mul_imm(r2,r2,array_size(type,k-1) DIV 8);
      put({19,20}+BITSET(index<<25)+BITSET(index<<14)+BITSET(r2)); -- add
      DEC(reg);
    END;
  END;
  IF (summ#0) & first THEN
    index_const:=TRUE; index:=summ;
  ELSE
    index_const:=FALSE; put_add_imm(index,index,summ);
  END;
END gen_array_index;

PROCEDURE gen_cal_procedure(p: item);
  VAR k: INTEGER; dp,fp,prc: item;
BEGIN
  prc:=p^.op_dw[0];
  ASSERT(prc^.md=tbl.cl_func);
  dp:=prc^.fu_prm;
  FOR k:=1 TO p^.op_dw_no-1 DO
    ASSERT(dp#NIL);
    fp:=prc^.op_dw[k];
    IF dp^.pr_rl=1 THEN
      IF dp^.pr_in THEN value_to_reg(fp,dp^.pr_val0+16,FALSE) END;
    ELSIF dp^.pr_rl=2 THEN
      lex.error(p^.lex_pos,'не реализовано gen_cal_procedure');
    ELSE
      lex.error(p^.lex_pos,'не реализовано gen_cal_procedure');
    END;
    dp:=dp^.pr_nxt;
  END;
  put({31}+BITSET(prc^.fu_val1 DIV 4 - free)*{0..29});
  dp:=prc^.fu_prm;
  FOR k:=1 TO p^.op_dw_no-1 DO
    fp:=prc^.op_dw[k];
    IF dp^.pr_rl=1 THEN
      IF dp^.pr_out THEN
        IF fp^.md=tbl.cl_var THEN
          ASSERT(fp^.va_sz<=32);
          IF fp^.va_rl=1 THEN
            lex.error(p^.lex_pos,'константа не допустима');
          ELSIF fp^.va_rl=2 THEN
            IF host?(fp)#pack THEN
              -- глобальная переменная другого пакета
              lex.error(p^.lex_pos,'не реализовано');
            ELSE
              put_store_imm(Greg,fp^.va_val0,dp^.pr_val0+16,
                            fp^.va_sz,signed?(fp));
            END;
          ELSE
            lex.error(p^.lex_pos,'не реализовано');
          END;
        ELSE
          lex.error(p^.lex_pos,'не реализовано');
        END;
      END;
    END;
    dp:=dp^.pr_nxt;
  END;
END gen_cal_procedure;

PROCEDURE gen_item(i: item);
  VAR
    d0,d1,tp: item;
    r,r1,r2,r3,r4,rr,sv_reg,sv_free,k: INTEGER;
    base,index: INTEGER;
    index_cns: BOOLEAN;
BEGIN
  IF restart THEN RETURN END;
  ASSERT(i^.md=tbl.cl_operator);
  delta_s:=0;
  IF i^.op_lex=lex.assign THEN
    d0:=i^.op_dw[0]; d1:=i^.op_dw[1];
    ASSERT(d0#NIL);
    IF d0^.md=tbl.cl_var THEN
      IF d0^.va_sz>32 THEN
        r1:=reg; INC(reg); r2:=reg; INC(reg);
        adr_to_reg(d0,r1); adr_to_reg(d1,r2);
        put_block_move(r1,r2,d0^.va_sz);
        DEC(reg,2);
      ELSIF d0^.va_rl=1 THEN
        lex.error(d0^.lex_pos,'константа не допустима');
      ELSIF d0^.va_rl=2 THEN
        IF host?(d0)#pack THEN
          -- глобальная переменная другого пакета
          lex.error(i^.lex_pos,'не реализовано');
        ELSIF in_register?(d1,r) THEN
          put_store_imm(Greg,d0^.va_val0,r,d0^.va_sz,signed?(d0));
        ELSE
          INC(reg);
          value_to_reg(d1,reg-1,FALSE);
          put_store_imm(Greg,d0^.va_val0,reg-1,d0^.va_sz,signed?(d0));
          DEC(reg);
        END;
      ELSIF d0^.va_rl=4 THEN
        IF host?(d0)#cur_proc THEN
          -- локальная переменная другой процедуры
          lex.error(i^.lex_pos,'не реализовано');
        ELSE
          value_to_reg(d1,d0^.va_val1,TRUE);
        END;
      ELSE
        lex.error(i^.lex_pos,'не реализовано');
      END;
    ELSIF (d0^.md=tbl.cl_operator) & (d0^.op_lex=lex.array) THEN
      sv_reg:=reg;
      gen_array_index(d0,base,index,index_cns,tp);
      sv_free:=free;
      k:=array_size(tp,d0^.op_dw_no-1);
      IF k>32 THEN
        IF index_cns THEN
          r1:=reg; INC(reg);
          put_add_imm(base,r1,index);
        ELSE
          r1:=index;
          put({19,20}+BITSET(base<<14)+BITSET(index)+BITSET(r1<<25)); -- add
        END;
        r2:=reg; INC(reg);
        adr_to_reg(d1,r2); put_block_move(r1,r2,k);
      ELSE
        IF NOT in_register?(d1,r2) THEN
          r2:=reg; INC(reg); value_to_reg(d1,r2,FALSE);
        END;
        IF free=sv_free THEN put(0) END; -- nop
        IF index_cns THEN
          put_store_imm(base,index,r2,k,
                        signed?(tp^.su_type^.ty_type^.ar_elemtp));
        ELSE
          put_store(base,index,r2,k,
                        signed?(tp^.su_type^.ty_type^.ar_elemtp));
        END;
      END;
      reg:=sv_reg;
    ELSIF (d0^.md=tbl.cl_operator) & (d0^.op_lex=lex.point) THEN
      sv_reg:=reg;
      IF NOT access_in_register?(d0^.op_dw[0],base) THEN
        base:=reg; INC(reg); access_to_reg(d0^.op_dw[0],base);
      END;
      IF (d0^.op_dw[1]^.md=tbl.cl_operator) &
         (d0^.op_dw[1]^.op_lex=lex.all) THEN
        k:=subtype_size(d0^.op_dw[1]^.op_dw[0]);
        index:=0;
      ELSE
        ASSERT(d0^.op_dw[1]^.md=tbl.cl_field);
        k:=d0^.op_dw[1]^.fi_bits;
        index:=d0^.op_dw[1]^.fi_offs DIV 8;
      END;
      IF k>32 THEN
        IF reg=sv_reg THEN r1:=reg; INC(reg) ELSE r1:=base END;
        put_add_imm(base,r1,index);
        r2:=reg; INC(reg);
        adr_to_reg(d1,r2); put_block_move(r1,r2,k);
      ELSE
        IF NOT in_register?(d1,r2) THEN
          r2:=reg; INC(reg); value_to_reg(d1,r2,FALSE);
        END;
        IF last_destination(base) THEN put(0) END;
        put_store_imm(base,index,r2,k,signed?(d0^.op_dw[1]^.fi_type));
      END;
      reg:=sv_reg;
    ELSE
      lex.error(d0^.lex_pos,'не реализовано');
    END;
  ELSIF i^.op_lex=lex.procedure THEN
    gen_cal_procedure(i);
  ELSIF i^.op_lex=lex.for THEN
    ASSERT(i^.op_dw[0]#NIL);
    ASSERT(i^.op_dw[0]^.md=tbl.cl_var);
    ASSERT(i^.op_dw[0]^.va_rl=4);
    r1:=reg; INC(reg); r2:=reg; INC(reg);
    i^.op_dw[0]^.va_val1:=r1;
    r3:=i^.op_dw[0]^.va_type^.su_first;
    r4:=i^.op_dw[0]^.va_type^.su_last;
    IF r4>=r3 THEN
      put_li(r3,r1);
      put_li(r4-r3+1,r2);
      k:=free;
      operators(i,gen_item);
      put({21,23,13,0}+BITSET(r2<<25)+BITSET(r2<<14)); -- sub CC
      put({30,25..27,24}+BITSET(k-free)*{0..21}); -- br ^Z
      put_add_imm(r1,r1,1);
      break:=free;
    END;
    DEC(reg,2);
    i^.op_dw[0]^.va_val1:=0;
  ELSE
    lex.error(i^.lex_pos,'не реализовано');
  END;
  IF delta_s>0 THEN put_add_imm(Sreg,Sreg,-delta_s) END;
END gen_item;

PROCEDURE gen_local_var(v: item);
BEGIN
  ASSERT(v^.md=tbl.cl_var);
  IF v^.va_rl=5 THEN put_add_imm(Lreg,v^.va_val1,v^.va_val0) END;
END gen_local_var;

PROCEDURE gen_procedure(p: item);
  VAR f,n: INTEGER;
BEGIN
  IF p^.fu_rl IN {1,2} THEN RETURN END;
  f:=free; cur_proc:=p;
  ASSERT(p^.fu_rl IN {3,4});
  p^.fu_val1:=free*4;
  REPEAT
    IF lex.error_cnt()#0 THEN RETURN END;
    free:=f;
    restart:=FALSE;
    put({19,20,22}); -- save
    IF p^.fu_var_no#0 THEN
      Lreg:=p^.fu_reg_no; reg:=Lreg+1;
      put_move(Sreg,Lreg);
      local_vars(p,gen_local_var);
      n:=(p^.fu_var_no+3) DIV 4; n:=n*4;
      put_add_imm(Sreg,Sreg,n);
    ELSE
      reg:=p^.fu_reg_no; Lreg:=0;
    END;
    operators(p,gen_item);
    IF p^.fu_var_no#0 THEN
      put({20,22,13,14..17,3}); -- jsr i7
      put_move(Lreg,Sreg);
    ELSE
      put({20,22,23,13,14..17,3}); -- jsra i7
    END;
    put({19,20,22,23}); -- restore
  UNTIL NOT restart;
END gen_procedure;

PROCEDURE show_code(fr,to: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=fr TO to DO print_cmd(code[i],fr*4) END;
END show_code;

PROCEDURE do_package(p: item);
  VAR i,f,fr: INTEGER;
BEGIN
  pack:=p; cur_proc:=NIL;
  free:=(p^.pa_var_no+3) DIV 4;
  FOR i:=0 TO free-1 DO code[i]:=0 END;
  fr:=free; break:=free;
  procedures(p,gen_procedure);
  f:=free;
  REPEAT
    free:=f; break:=free; restart:=FALSE; reg:=8;
    put({19,20,22}); -- save
    operators(p,gen_item);
    put({20,22,23,13,14..17,3}); -- jsra i7
    put({19,20,22,23}); -- restore
    IF lex.error_cnt()#0 THEN RETURN END;
  UNTIL NOT restart;
  show_code(fr,free-1);

END do_package;

END adaGen.
