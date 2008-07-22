IMPLEMENTATION MODULE adaMem; (* 29-Aug-90. (c) KRONOS *)

IMPORT tbl : adaTable;
IMPORT lex : adaLex;
FROM adaTable   IMPORT  item;
FROM Terminal   IMPORT  print;

TYPE
  proc=PROCEDURE (item);

VAR
  free    : INTEGER;
  reg     : INTEGER;
  cur_proc: item;

PROCEDURE export(h: item; p: proc);
  PROCEDURE one(i,e: item);
  BEGIN
    WHILE i#e DO
      ASSERT(i#NIL);
      IF i^.md=tbl.cl_pack THEN one(i^.pa_list,i^.pa_priv)
      ELSIF i^.md=tbl.cl_operator THEN one(i^.op_list,NIL) ELSE p(i) END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  ASSERT(h^.md=tbl.cl_pack);
  one(h^.pa_list,h^.pa_priv);
END export;

PROCEDURE private(h: item; p: proc);
  PROCEDURE one(i: item);
  BEGIN
    WHILE i#NIL DO
      IF i^.md=tbl.cl_pack THEN one(i^.pa_priv)
      ELSIF i^.md=tbl.cl_operator THEN one(i^.op_list) ELSE p(i) END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  ASSERT(h^.md=tbl.cl_pack);
  one(h^.pa_priv);
END private;

PROCEDURE procedures(h: item; p: proc);
  PROCEDURE one(i: item);
  BEGIN
    WHILE i#NIL DO
      IF i^.md=tbl.cl_pack THEN one(i^.pa_list)
      ELSIF i^.md=tbl.cl_func THEN p(i); one(i^.fu_list)
      ELSIF i^.md=tbl.cl_operator THEN one(i^.op_list)
      END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  ASSERT(h^.md=tbl.cl_pack);
  one(h^.pa_list);
END procedures;

PROCEDURE locals(h: item; p: proc);
  PROCEDURE one(i: item);
  BEGIN
    WHILE i#NIL DO
      IF i^.md=tbl.cl_pack THEN one(i^.pa_list)
      ELSIF i^.md=tbl.cl_operator THEN one(i^.op_list)
      ELSE p(i);
      END;
      i:=i^.host_nxt;
    END;
  END one;
BEGIN
  ASSERT(h^.md=tbl.cl_func);
  one(h^.fu_list);
END locals;

PROCEDURE alloc_export(i: item);
BEGIN
  IF (i^.md=tbl.cl_func) & (i^.fu_rl=0) THEN
    free:=free+(4-free) MOD 4;
    i^.fu_rl:=3; i^.fu_val0:=free; INC(free,4);
  ELSIF (i^.md=tbl.cl_var) & (i^.va_rl=0) THEN
    IF i^.va_sz>=128 THEN
      free:=free+(4-free) MOD 4;
      i^.va_rl:=3; i^.va_val0:=free; INC(free,4);
    ELSIF i^.va_sz>16 THEN
      free:=free+(4-free) MOD 4;
      i^.va_rl:=2; i^.va_val0:=free; INC(free,4*((i^.va_sz+31) DIV 32));
    ELSIF i^.va_sz>8 THEN
      free:=free+(2-free) MOD 2;
      i^.va_rl:=2; i^.va_val0:=free; INC(free,2);
    ELSE
      i^.va_rl:=2; i^.va_val0:=free; INC(free,1);
    END;
  END;
END alloc_export;

PROCEDURE check_export(i: item);
  PROCEDURE assert(v: BOOLEAN);
  BEGIN
    IF v THEN RETURN END; HALT(1);
  END assert;
BEGIN
  IF (i^.md=tbl.cl_func) & NOT (i^.fu_rl IN {1,2}) THEN
    assert((i^.fu_rl=3) & (i^.fu_val0=free)); INC(free,4);
  ELSIF (i^.md=tbl.cl_var) & (i^.va_rl IN {2,3}) THEN
    IF i^.va_sz>=128 THEN
      free:=free+(4-free) MOD 4;
      assert((i^.va_rl=3) & (i^.va_val0=free)); INC(free,4);
    ELSIF i^.va_sz>16 THEN
      free:=free+(4-free) MOD 4;
      assert((i^.va_rl=2) & (i^.va_val0=free));
      INC(free,4*((i^.va_sz+31) DIV 32));
    ELSIF i^.va_sz>8 THEN
      free:=free+(2-free) MOD 2;
      assert((i^.va_rl=2) & (i^.va_val0=free)); INC(free,2);
    ELSE
      assert((i^.va_rl=2) & (i^.va_val0=free)); INC(free,1);
    END;
  END;
END check_export;

PROCEDURE alloc_private(i: item);
BEGIN
  IF (i^.md=tbl.cl_func) & (i^.fu_rl=0) THEN
    i^.fu_rl:=4; i^.fu_val0:=0;
  ELSIF (i^.md=tbl.cl_var) & (i^.va_rl=0) THEN
    IF i^.va_sz>=128 THEN
      free:=free+(4-free) MOD 4;
      i^.va_rl:=3; i^.va_val0:=free; INC(free,4);
    ELSIF i^.va_sz>16 THEN
      free:=free+(4-free) MOD 4;
      i^.va_rl:=2; i^.va_val0:=free; INC(free,4*((i^.va_sz+31) DIV 32));
    ELSIF i^.va_sz>8 THEN
      free:=free+(2-free) MOD 2;
      i^.va_rl:=2; i^.va_val0:=free; INC(free,2);
    ELSE
      i^.va_rl:=2; i^.va_val0:=free; INC(free,1);
    END;
  END;
END alloc_private;

PROCEDURE alloc_local_item(i: item);
BEGIN
  IF i^.md=tbl.cl_prm THEN
    IF reg>=16 THEN
      lex.error(cur_proc^.lex_pos,'слишком много параметров');
      reg:=MIN(INTEGER);
    END;
    i^.pr_val0:=reg; INC(reg);
    IF i^.pr_bits>32 THEN i^.pr_rl:=2 ELSE i^.pr_rl:=1 END
  ELSIF (i^.md=tbl.cl_var) & (i^.va_rl=0) THEN
    IF i^.va_sz>32 THEN
      free:=free+(4-free) MOD 4;
      IF (free>0) & (reg<23) THEN
        i^.va_rl:=5; i^.va_val1:=reg; INC(reg);
      ELSE
        i^.va_rl:=6;
      END;
      i^.va_val0:=free; INC(free,(i^.va_sz+7) DIV 8);
    ELSIF (reg>=23) OR i^.va_mem THEN
      IF reg<23 THEN
        i^.va_rl:=5; i^.va_val1:=reg; INC(reg)
      ELSE
        i^.va_rl:=6
      END;
      IF i^.va_sz>16 THEN
        free:=free+(4-free) MOD 4; i^.va_val0:=free; INC(free,4*((i^.va_sz+31) DIV 32));
      ELSIF i^.va_sz>8 THEN
        free:=free+(2-free) MOD 2; i^.va_val0:=free; INC(free,2);
      ELSE
        i^.va_val0:=free; INC(free,1);
      END;
    ELSE
      i^.va_rl:=4; i^.va_val1:=reg; INC(reg);
    END;
  END;
END alloc_local_item;

PROCEDURE alloc_locals(p: item);
BEGIN
  cur_proc:=p; free:=0; reg:=8;
  locals(p,alloc_local_item);
  p^.fu_var_no:=free;
  p^.fu_reg_no:=reg;
END alloc_locals;

PROCEDURE show(i: item);
BEGIN
  IF i^.md=tbl.cl_func THEN
    print('  func, rl %1d, val0 %4d, val1 %4d, %s\n',
          i^.fu_rl,i^.fu_val0,i^.fu_val1,i^.nm^.str);
  ELSIF i^.md=tbl.cl_var THEN
    print('  var,  rl %1d, val0 %4d, val1 %4d, %s\n',
          i^.va_rl,i^.va_val0,i^.va_val1,i^.nm^.str);
  ELSIF i^.md=tbl.cl_prm THEN
    print('  prm,  rl %1d, val0 %4d,          , %s\n',
          i^.pr_rl,i^.pr_val0,i^.nm^.str);
  END;
END show;

PROCEDURE show_locals(p: item);
BEGIN
  locals(p,show);
END show_locals;

PROCEDURE do_package(p: item; body: BOOLEAN);
  VAR i: item; n: INTEGER;
BEGIN
  ASSERT(p^.md=tbl.cl_pack);
  free:=0;
  -- распределяем область экспорта под константы, переменные и процедуры
  IF NOT body THEN
    export(p,alloc_export);
    RETURN
  END;
  export(p,check_export);
--export(p,show);
  print('  export   %5d bytes\n',free);
  n:=free;
  private(p,alloc_private);
--private(p,show);
  print('  private  %5d bytes\n',free-n);
  p^.pa_var_no:=free;
  procedures(p,alloc_locals);
--procedures(p,show_locals);
END do_package;

END adaMem.
