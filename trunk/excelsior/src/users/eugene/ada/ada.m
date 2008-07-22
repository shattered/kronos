MODULE ada; (* 27-Aug-90. (c) KRONOS *)

IMPORT lex : adaLex;
IMPORT tbl : adaTable;
IMPORT bug : adaDebug;
IMPORT tty : Terminal;
IMPORT arg : Args;

IMPORT adaMem;
IMPORT adaGen;
IMPORT adaType;

FROM SYSTEM     IMPORT  ADR;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;
FROM adaLex     IMPORT  sy, next_sy, next, lex_pos, val, error;
FROM adaTable   IMPORT  item;

TYPE
  item_ref=POINTER TO item;

VAR
  cnt          : INTEGER;
  comp_unit    : item;

PROCEDURE calculate(e: item): item;
  -- пытается свернуть выражение до константы, типа или дапазана
  VAR v,f,l,r: item; n: INTEGER;
BEGIN
  IF lex.error_cnt()#0 THEN RETURN e END;
  IF e^.md=tbl.cl_operator THEN
    IF e^.op_lex=lex.procedure THEN
      IF (e^.op_dw_no=0) OR (e^.op_dw[0]=NIL) OR
         (e^.op_dw[0]^.md#tbl.cl_func) THEN RETURN e END;
      f:=e^.op_dw[0];
      IF f^.fu_rl=1 THEN
        IF e^.op_dw_no=3 THEN
          l:=calculate(e^.op_dw[1]); r:=calculate(e^.op_dw[2]);
          IF (l^.md#tbl.cl_var) OR (r^.md#tbl.cl_var) OR
             (l^.va_rl#1) OR (r^.va_rl#1) THEN RETURN e END;
          CASE f^.fu_val0 OF
            |1: n:=l^.va_val0+r^.va_val0;
            |2: n:=l^.va_val0-r^.va_val0;
            |3: n:=INTEGER(BITSET(l^.va_val0)*BITSET(r^.va_val0));
            |4: n:=INTEGER(BITSET(l^.va_val0)+BITSET(r^.va_val0));
            |5: n:=INTEGER(BITSET(l^.va_val0)/BITSET(r^.va_val0));
          END;
          v:=tbl.def_item(tbl.cl_var);
          v^.va_type:=f^.fu_type; v^.va_rl:=1; v^.va_val0:=n;
          RETURN v;
        ELSE
          RETURN e;
        END;
      ELSIF f^.fu_rl=2 THEN
        v:=tbl.def_item(tbl.cl_var);
        v^.va_type:=f^.fu_type; v^.va_rl:=1; v^.va_val0:=f^.fu_val1;
        RETURN v;
      ELSE
        RETURN e;
      END;
    ELSIF e^.op_lex=lex.int_number THEN
      v:=tbl.def_item(tbl.cl_var);
      v^.va_type:=comp_unit^.pa_uni_int; v^.va_rl:=1; v^.va_val0:=e^.op_val0;
      RETURN v;
    ELSE
      RETURN e;
    END;
  ELSE
    RETURN e;
  END;
END calculate;

PROCEDURE size?(fr,to: INTEGER; VAR sign: BOOLEAN; VAR bits: INTEGER);
BEGIN
  sign:=fr<0;
  IF sign THEN
    bits:=1;
    WHILE (to>=1) OR (-fr>1) DO INC(bits); to:=to DIV 2; fr:=fr DIV 2 END;
  ELSE
    bits:=0;
    WHILE to>=1 DO INC(bits); to:=to DIV 2 END;
  END;
END size?;

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

PROCEDURE check_semicolon;
BEGIN
  IF sy#lex.semicolon THEN
    error(lex_pos,'где ";" ?');
    WHILE sy#lex.semicolon DO
      IF sy=lex.eot THEN RETURN END;
      IF sy=lex.end THEN RETURN END;
      next;
    END;
  END;
  next;
END check_semicolon;

PROCEDURE operators_sequence(pack: item; VAR last: item); -- 97.
FORWARD;

PROCEDURE declaration_partition(host: item; VAR last: item); -- 32.
FORWARD;

PROCEDURE expression(host: item; VAR last: item): item; -- 132.
FORWARD;

PROCEDURE subtype_indication(expr: item; pos: INTEGER; host: item;
                             VAR last: item; new: BOOLEAN): item; -- 58.
FORWARD;

PROCEDURE constant_expression(host,last,type: item): item;
  VAR l,e: item;
BEGIN
  l:=last;
  e:=expression(host,last);
  adaType.do_expression(e,type);
  e:=calculate(e);
  l^.host_nxt:=NIL;
  RETURN e;
END constant_expression;

PROCEDURE type_mark(expr: item; pos: INTEGER;
                    host: item; VAR last: item): item; -- 59.
BEGIN
  IF expr=NIL THEN
    pos:=lex_pos; expr:=constant_expression(host,last,NIL);
  END;
  IF expr^.md#tbl.cl_subtype THEN
    error(pos,'must be subtype'); expr:=comp_unit^.pa_uni_int;
  END;
  RETURN expr;
END type_mark;

PROCEDURE idents_list(host: item; VAR first,last: item; md: tbl.class); -- 38.
BEGIN
  first:=NIL;
  LOOP
    IF (sy#lex.ident) & ((md#tbl.cl_func) OR (sy#lex.char)) THEN
      error(lex_pos,'ожидался идентификатор');
      val.str:='???'; val.len:=3; sy:=lex.ident;
    END;
    tbl.new_name_item(md,host,last);
    IF first=NIL THEN first:=last END;
    next;
    IF sy#lex.comma THEN EXIT END;
    next;
  END;
END idents_list;

PROCEDURE spec_parm(host: item; VAR last: item; VAR pi: item_ref); -- 13.
  VAR in,out: BOOLEAN; type,first: item; bits: INTEGER;
BEGIN
  idents_list(host,first,last,tbl.cl_prm);
  IF sy#lex.colon THEN
    error(lex_pos,'ожидалось ":"');
  ELSE
    next;
  END;
  IF (sy=lex.in) & (next_sy=lex.out) THEN
    in:=TRUE; out:=TRUE; next; next;
  ELSIF sy=lex.in THEN
    in:=TRUE; out:=FALSE; next;
  ELSIF sy=lex.out THEN
    in:=FALSE; out:=TRUE; next;
  ELSE
    in:=TRUE; out:=FALSE;
  END;
  type:=type_mark(NIL,0,host,last);
  bits:=subtype_size(type);
  pi^:=first;
  LOOP
    ASSERT(first#NIL);
    first^.pr_type:=type;
    first^.pr_in:=in;
    first^.pr_out:=out;
    first^.pr_bits:=bits;
    INC(cnt);
    IF first=last THEN EXIT END;
    first^.pr_nxt:=first^.host_nxt;
    first:=first^.host_nxt;
  END;
  pi:=ADR(first^.pr_nxt);
END spec_parm;

PROCEDURE formal_parms(proc: item); -- 12.
  VAR last: item; pi: item_ref;
BEGIN
  next; last:=NIL; pi:=ADR(proc^.fu_prm);
  LOOP
    spec_parm(proc,last,pi); IF sy#lex.semicolon THEN EXIT END; next;
  END;
  IF sy#lex.right_parenthesis THEN
    error(lex_pos,'где ")" ?');
    WHILE sy#lex.right_parenthesis DO IF sy=lex.eot THEN RETURN END; next END;
  END;
  next;
END formal_parms;

PROCEDURE spec_procedure(host: item; VAR last: item); -- 9.
  VAR hdr: lex.lex_elem;
BEGIN
  hdr:=sy; next;
  IF (sy#lex.string) & (sy#lex.ident) THEN
    error(lex_pos,'ожидался идентификатор'); RETURN
  END;
  tbl.new_name_item(tbl.cl_func,host,last); next;
  cnt:=0;
  IF sy=lex.left_parenthesis THEN formal_parms(last) END;
  IF hdr=lex.function THEN
    IF sy#lex.return THEN error(lex_pos,'ожидалось RETURN'); RETURN END;
    next;
    last^.fu_type:=type_mark(NIL,0,host,last);
  END;
  last^.fu_prm_no:=cnt;
END spec_procedure;

PROCEDURE def_procedure(host: item; VAR last: item): INTEGER; -- 8. 23.
  VAR proc,plast: item; n: INTEGER;
BEGIN
  spec_procedure(host,last); -- 9.
  proc:=last;
  IF sy=lex.is THEN
    next;
    plast:=proc^.fu_list;
    IF plast#NIL THEN
      WHILE plast^.host_nxt#NIL DO plast:=plast^.host_nxt END;
    END;
    IF sy=lex.constant THEN
      n:=0; proc^.fu_rl:=1; next;
      IF sy#lex.int_number THEN
        error(lex_pos,'ожидалось целое число');
      ELSE
        proc^.fu_val0:=val.int; next;
      END;
      proc^.fu_rl:=1;
    ELSE
      n:=1;
      IF sy#lex.begin THEN
        -- 32. раздел описаний
        declaration_partition(proc,plast);
      END;
      IF sy#lex.begin THEN
        error(lex_pos,'ожидалось BEGIN процедуры %s%.',proc^.nm^.str);
      END;
      next;
      -- 97. последовательность операторов
      operators_sequence(proc,plast);
    END;
    IF sy#lex.end THEN
      error(lex_pos,'ожидалось END процедуры %s%.',proc^.nm^.str);
    END;
    next;
    IF sy=lex.ident THEN
      IF proc^.nm#tbl.find() THEN
        error(lex_pos,'ожидалось END процедуры %s%.',proc^.nm^.str);
      END;
      next;
    END;
    check_semicolon; RETURN n;
  ELSE
    check_semicolon; RETURN 0;
  END;
END def_procedure;

PROCEDURE integer_type_definition(type,subtype: item; VAR last: item); -- 61.
  PROCEDURE expr?(VAR n: INTEGER): BOOLEAN;
  BEGIN
    IF sy=lex.plus THEN n:=1; next
    ELSIF sy=lex.minus THEN n:=-1; next
    ELSE n:=1
    END;
    IF sy#lex.int_number THEN
      error(lex_pos,'ожидалось целое число'); RETURN FALSE;
    END;
    n:=n*val.int; next; RETURN TRUE;
  END expr?;
  VAR fr,to,pos: INTEGER;
BEGIN
  tbl.new_item(tbl.cl_integer,type^.host,last);
  type^.ty_type:=last;
  IF sy#lex.range THEN error(lex_pos,'ожидалось RANGE'); RETURN END;
  next;
  fr:=MIN(INTEGER); to:=MAX(INTEGER); pos:=lex_pos;
  IF expr?(fr) THEN
    IF sy#lex.double_dot THEN
      error(lex_pos,'ожидалось ".."');
    ELSE
      next;
      IF expr?(to) THEN END;
    END;
  END;
  IF fr>to THEN error(pos,'кривой диапазон'); fr:=to END;
  size?(fr,to,last^.in_signed,last^.in_bits);
  last^.in_universal:=FALSE;
  subtype^.su_first:=fr;
  subtype^.su_last :=to;
END integer_type_definition;

PROCEDURE enumeration_type_definition(type,subtype: item; VAR last: item);
  VAR n: INTEGER; first: item;
BEGIN
  tbl.new_item(tbl.cl_enumeration,type^.host,last);
  type^.ty_type:=last;
  IF sy#lex.left_parenthesis THEN error(lex_pos,'ожидалось "("'); RETURN END;
  next;
  idents_list(type^.host,first,last,tbl.cl_func);
  n:=0;
  LOOP
    ASSERT(first#NIL);
    first^.fu_type:=subtype;
    first^.fu_rl:=2;
    first^.fu_val0:=n;
    first^.fu_val1:=n;
    IF first=last THEN EXIT END;
    first:=first^.host_nxt; INC(n);
  END;
  size?(0,n,type^.ty_type^.en_signed,type^.ty_type^.en_bits);
  subtype^.su_first:=0;
  subtype^.su_last :=n;
  IF sy#lex.right_parenthesis THEN
    error(lex_pos,'ожидалось ")"');
  ELSE
    next;
  END;
END enumeration_type_definition;

PROCEDURE derived_type_definition(type,subtype: item; VAR last: item);
  VAR fr: item;
BEGIN
  IF sy#lex.new THEN error(lex_pos,'ожидалось NEW'); RETURN END;
  next;
  fr:=subtype_indication(NIL,0,type^.host,last,FALSE);
  ASSERT(fr#NIL); ASSERT(fr^.su_type#NIL); ASSERT(fr^.su_type^.ty_type#NIL);
  type^.ty_type:=fr^.su_type^.ty_type;
  subtype^.su_first:=fr^.su_first;
  subtype^.su_last :=fr^.su_last;
END derived_type_definition;

PROCEDURE discrete_type?(fr: item): BOOLEAN;
BEGIN
  RETURN
   (fr#NIL) & (fr^.md=tbl.cl_subtype) & (fr^.su_type#NIL) &
   (fr^.su_type^.ty_type#NIL) &
   (fr^.su_type^.ty_type^.md IN tbl.discrete_type);
END discrete_type?;

PROCEDURE type_range(type,fr: item; pos: INTEGER;
                     host: item; VAR last: item): item; -- 62.
  VAR to,j: item;
BEGIN
  IF fr=NIL THEN
    pos:=lex_pos; fr:=constant_expression(host,last,type);
  END;
  IF sy=lex.double_dot THEN
    tbl.new_item(tbl.cl_subtype,host,last); j:=last;
    IF lex.error_cnt()#0 THEN
    ELSIF (fr^.md#tbl.cl_var) OR (fr^.va_rl#1) THEN
      error(pos,'must be constant expression');
    ELSIF NOT discrete_type?(fr^.va_type) THEN
      error(pos,'must be discrete subtype');
    ELSE
      IF type=NIL THEN type:=fr^.va_type END;
      IF type^.su_first>fr^.va_val0 THEN
        error(lex_pos,'диапазон должен быть вложенным');
      ELSE
        j^.su_type:=type^.su_type; j^.su_first:=fr^.va_val0;
      END;
    END;
    next;
    pos:=lex_pos; to:=constant_expression(host,last,type);
    IF lex.error_cnt()#0 THEN
    ELSIF (to^.md#tbl.cl_var) OR (to^.va_rl#1) THEN
      error(pos,'must be constant expression');
    ELSIF NOT discrete_type?(to^.va_type) THEN
      error(pos,'must be discrete subtype');
    ELSIF type^.su_last<fr^.va_val0 THEN
      error(lex_pos,'диапазон должен быть вложенным');
    ELSE
      j^.su_last:=to^.va_val0;
    END;
  ELSIF (fr^.md=tbl.cl_operator) & (fr^.op_lex=lex.range) THEN
    tbl.new_item(tbl.cl_subtype,host,last); j:=last;
    to:=fr^.op_dw[1]; fr:=fr^.op_dw[0];
    IF lex.error_cnt()#0 THEN
    ELSIF (fr^.md#tbl.cl_var) OR (fr^.va_rl#1) THEN
      error(pos,'must be constant expression');
    ELSIF NOT discrete_type?(fr^.va_type) THEN
      error(pos,'must be discrete subtype');
    ELSE
      IF type=NIL THEN type:=fr^.va_type END;
      IF type^.su_first>fr^.va_val0 THEN
        error(lex_pos,'диапазон должен быть вложенным');
      ELSE
        j^.su_type:=type^.su_type; j^.su_first:=fr^.va_val0;
      END;
    END;
    IF lex.error_cnt()#0 THEN
    ELSIF (to^.md#tbl.cl_var) OR (to^.va_rl#1) THEN
      error(pos,'must be constant expression');
    ELSIF NOT discrete_type?(to^.va_type) THEN
      error(pos,'must be discrete subtype');
    ELSIF type^.su_last<fr^.va_val0 THEN
      error(lex_pos,'диапазон должен быть вложенным');
    ELSE
      j^.su_last:=to^.va_val0;
    END;
  ELSE
    error(lex_pos,'is it range?'); j:=type;
  END;
  RETURN j;
END type_range;

PROCEDURE type_discrete_range(type,fr: item; pos: INTEGER;
                              host: item; VAR last: item): item; -- 68.
BEGIN
  IF fr=NIL THEN
    pos:=lex_pos; fr:=constant_expression(host,last,type);
  END;
  IF fr^.md=tbl.cl_subtype THEN
    RETURN subtype_indication(fr,pos,host,last,FALSE)
  ELSE
    RETURN type_range(type,fr,pos,host,last);
  END;
END type_discrete_range;

PROCEDURE array_type_definition(type,subtype: item; VAR last: item); -- 48.
  VAR i,fr: item; pos: INTEGER;
BEGIN
  tbl.new_item(tbl.cl_array,type^.host,last); type^.ty_type:=last;
  type^.ty_type^.ar_index_no:=0;
  IF sy#lex.array THEN error(lex_pos,'ожидалось ARRAY'); RETURN END;
  next;
  IF sy#lex.left_parenthesis THEN error(lex_pos,'ожидалось "("'); RETURN END;
  next;
  LOOP
    pos:=lex_pos; fr:=constant_expression(type^.host,last,NIL);
    IF sy=lex.range THEN -- неограниченный индексируемый тип
      next;
      IF sy#lex.box THEN error(lex_pos,'"<>" expected') ELSE next END;
      IF lex.error_cnt()#0 THEN
      ELSIF NOT discrete_type?(fr) THEN
        error(pos,'must be discrete subtype');
      ELSE
        type^.ty_type^.ar_index[type^.ty_type^.ar_index_no]:=fr;
      END;
    ELSE
      fr:=type_discrete_range(NIL,fr,pos,type^.host,last);
      type^.ty_type^.ar_index[type^.ty_type^.ar_index_no]:=fr;
      subtype^.su_descr[type^.ty_type^.ar_index_no]:=fr;
    END;
    INC(type^.ty_type^.ar_index_no);
    IF sy#lex.comma THEN EXIT END; next;
  END;
  IF sy#lex.right_parenthesis THEN error(lex_pos,'ожидалось ")"'); RETURN END;
  next;
  IF sy#lex.of THEN error(lex_pos,'ожидалось OF') ELSE next END;
  i:=subtype_indication(NIL,0,type^.host,last,FALSE);
  ASSERT(i#NIL); ASSERT(i^.su_type#NIL); ASSERT(i^.su_type^.ty_type#NIL);
  type^.ty_type^.ar_elemtp:=i;
  type^.ty_type^.ar_elembits:=subtype_size(i);
  subtype^.su_last:=type^.ty_type^.ar_index_no;
END array_type_definition;

PROCEDURE record_type_definition(type,subtype: item; VAR last: item); -- 52.
  VAR sz: INTEGER; pi: item_ref; first,i: item;
BEGIN
  ASSERT(sy=lex.record); next;
  tbl.new_item(tbl.cl_record,type^.host,last); type^.ty_type:=last;
  type^.ty_type^.re_descr_no:=0;
  pi:=ADR(type^.ty_type^.re_fields);
  sz:=0;
  LOOP
    idents_list(type^.host,first,last,tbl.cl_field);
    IF sy#lex.colon THEN error(lex_pos,'пропущено ":"') ELSE next END;
    pi^:=first;
    i:=type_mark(NIL,0,type^.host,last);
    LOOP
      first^.fi_type:=i;
      first^.fi_bits:=subtype_size(i);
      IF first^.fi_bits>16 THEN
        INC(sz,(32-sz) MOD 32);
        INC(first^.fi_bits,(32-first^.fi_bits) MOD 32);
      ELSIF first^.fi_bits>8 THEN
        INC(sz,(16-sz) MOD 16);
        INC(first^.fi_bits,(16-first^.fi_bits) MOD 16);
      END;
      first^.fi_offs:=sz;
      first^.fi_descr:=-1;
      first^.fi_case:=-1;
      INC(sz,first^.fi_bits);
      IF first=last THEN EXIT END;
      first^.fi_nxt:=first^.host_nxt;
      first:=first^.host_nxt;
    END;
    check_semicolon;
    IF sy#lex.ident THEN EXIT END;
    pi:=ADR(last^.fi_nxt);
  END;
  type^.ty_type^.re_bits:=sz;
  IF (sy#lex.end) OR (next_sy#lex.record) THEN
    error(lex_pos,'ожидалось END RECORD');
  ELSE
    next; next;
  END;
END record_type_definition;

PROCEDURE access_type_definition(type,subtype: item; VAR last: item); -- 74.
BEGIN
  ASSERT(sy=lex.access); next;
  tbl.new_item(tbl.cl_access,type^.host,last); type^.ty_type:=last;
  type^.ty_type^.ac_type:=type_mark(NIL,0,type^.host,last);
END access_type_definition;

PROCEDURE type_definition(type,subtype: item; VAR last: item); --41.
BEGIN
  IF sy=lex.left_parenthesis THEN
    enumeration_type_definition(type,subtype,last);
  ELSIF sy=lex.range THEN
    integer_type_definition(type,subtype,last);
  ELSIF sy=lex.new THEN
    derived_type_definition(type,subtype,last);
  ELSIF sy=lex.array THEN
    array_type_definition(type,subtype,last);
  ELSIF sy=lex.record THEN
    record_type_definition(type,subtype,last);
  ELSIF sy=lex.access THEN
    access_type_definition(type,subtype,last);
  ELSE
    type^.ty_type:=comp_unit^.pa_uni_int;
    error(lex_pos,'type definition expected');
    WHILE (sy#lex.semicolon) & (sy#lex.eot) DO next END;
  END;
END type_definition;

PROCEDURE subtype_indication(expr: item; pos: INTEGER; host: item;
                             VAR last: item; new: BOOLEAN): item; -- 58.
  VAR i,j,fr,k: item; n: INTEGER;
BEGIN
  i:=type_mark(expr,pos,host,last); j:=i;
  IF i#NIL THEN
    CASE i^.su_type^.ty_type^.md OF
      |tbl.cl_integer:
        IF sy=lex.range THEN
          next; j:=type_range(i,NIL,0,host,last);
        END;
      |tbl.cl_float:  -- ??????
      |tbl.cl_array:
        IF sy=lex.left_parenthesis THEN
          next;
          tbl.new_item(tbl.cl_subtype,host,last); j:=last;
          j^.su_type:=i^.su_type; j^.su_descr:=i^.su_descr;
          j^.su_first:=i^.su_first; j^.su_last:=i^.su_last;
          n:=0;
          LOOP
            WHILE (n<j^.su_last) & (j^.su_descr[n]#NIL) DO INC(n) END;
            IF n>=j^.su_last THEN
              error(lex_pos,'тип уже ограничен'); EXIT;
            END;
            pos:=lex_pos; k:=j^.su_type^.ty_type^.ar_index[n];
            j^.su_descr[n]:=type_discrete_range(k,NIL,0,host,last);
            IF sy#lex.comma THEN EXIT END;
            next; INC(n);
          END;
          IF sy#lex.right_parenthesis THEN
            error(lex_pos,'")" expected')
          ELSE
            next;
          END;
        END;
      |tbl.cl_record:
    ELSE
    END;
    IF new & (j=i) THEN
      tbl.new_item(tbl.cl_subtype,host,last);
      j:=last; j^.su_type:=i^.su_type; j^.su_descr:=i^.su_descr;
      j^.su_first:=i^.su_first; j^.su_last:=i^.su_last;
    END;
  END;
  RETURN j;
END subtype_indication;

PROCEDURE subtype_declaration(host: item; VAR last: item); --  42.
  VAR subtype: item; nm: tbl.name;
BEGIN
  ASSERT(sy=lex.subtype); next;
  IF sy#lex.ident THEN
    error(lex_pos,'ожидался идентификатор');
  ELSE
    nm:=tbl.def_name(); next;
    IF sy#lex.is THEN
      error(lex_pos,'ожидалось IS');
    ELSE
      next;
      subtype:=subtype_indication(NIL,0,host,last,TRUE);
      tbl.tie_item(nm,subtype);
    END;
  END;
  check_semicolon;
END subtype_declaration;

PROCEDURE def_type(host: item; VAR last: item); --  39.
  VAR type,subtype,i: item; nm: tbl.name;
BEGIN
  ASSERT(sy=lex.type); next;
  IF sy#lex.ident THEN
    error(lex_pos,'ожидался идентификатор');
  ELSE
    subtype:=NIL; nm:=tbl.find();
    IF nm#NIL THEN
      i:=nm^.obj;
      WHILE (i#NIL) & (subtype=NIL) DO
        IF i^.host=host THEN subtype:=i END; i:=i^.nm_fwd;
      END;
    END;
    IF (subtype=NIL) OR
       (subtype^.su_type=NIL) OR
       (subtype^.su_type^.ty_type#NIL) THEN
      tbl.new_item(tbl.cl_type,host,last); type:=last;
      tbl.new_name_item(tbl.cl_subtype,host,last); subtype:=last;
      subtype^.su_type:=type;
      next;
      IF sy=lex.is THEN
        next;
        type_definition(type,subtype,last);
      END;
    ELSE
      type:=subtype^.su_type;
      next;
      IF sy=lex.is THEN
        next;
        type_definition(type,subtype,last);
      ELSE
        error(lex_pos,'ожидалось IS');
      END;
    END;
  END;
  check_semicolon;
END def_type;

PROCEDURE def_object_or_number(host: item; VAR last: item); -- 36. 37.
  VAR const: BOOLEAN; v,sz,rl: INTEGER; type,first: item;
BEGIN
  const:=FALSE; v:=0; sz:=0; rl:=0; type:=NIL;
  idents_list(host,first,last,tbl.cl_var);
  IF sy#lex.colon THEN error(lex_pos,'ожидалось ":"') ELSE next END;
  IF sy=lex.constant THEN
    next;
    IF sy#lex.assign THEN error(lex_pos,'ожидалось ":="') ELSE next END;
    IF sy#lex.int_number THEN error(lex_pos,'ожидалось число') END;
    v:=val.int; rl:=1; sz:=32; const:=TRUE; type:=comp_unit^.pa_uni_int; next;
  ELSE
    type:=type_mark(NIL,0,host,last); sz:=subtype_size(type);
  END;
  LOOP
    ASSERT(first#NIL);
    first^.va_type:=type;
    first^.va_const:=const;
    first^.va_val0:=v;
    first^.va_sz:=sz;
    first^.va_rl:=rl;
    IF first=last THEN EXIT END;
    first:=first^.host_nxt;
  END;
  check_semicolon;
END def_object_or_number;

PROCEDURE main_dcl_item(host: item; VAR last: item): INTEGER; -- 33. 34.
BEGIN
  IF sy=lex.type THEN def_type(host,last); RETURN 0;
  ELSIF sy=lex.subtype THEN subtype_declaration(host,last); RETURN 0;
  ELSIF (sy=lex.procedure) OR (sy=lex.function) THEN
    RETURN def_procedure(host,last);
  ELSE def_object_or_number(host,last); RETURN 0;
  END;
END main_dcl_item;

PROCEDURE make_uni_types(pack: item; VAR last: item);
  VAR s,t,k: item;
BEGIN
  sy:=lex.ident;
  -- universal integer
  val.str:='UNIVERSAL_INTEGER'; val.len:=17;
  tbl.new_name_item(tbl.cl_subtype,pack,last); s:=last;
  tbl.new_item(tbl.cl_type,pack,last); t:=last;
  tbl.new_item(tbl.cl_integer,pack,last); k:=last;
  s^.su_type:=t; t^.ty_type:=k;
  s^.su_first:=MIN(INTEGER); s^.su_last:=MAX(INTEGER);
  k^.in_bits:=32; k^.in_signed:=TRUE; k^.in_universal:=TRUE;
  pack^.pa_uni_int:=s;

  -- universal access
  val.str:='UNIVERSAL_ACCESS'; val.len:=16;
  tbl.new_name_item(tbl.cl_subtype,pack,last); s:=last;
  tbl.new_item(tbl.cl_type,pack,last); t:=last;
  tbl.new_item(tbl.cl_access,pack,last); k:=last;
  s^.su_type:=t; t^.ty_type:=k;
  k^.ac_type:=pack^.pa_uni_int; k^.ac_universal:=TRUE;
  pack^.pa_uni_acc:=s;
END make_uni_types;

PROCEDURE package(host: item; VAR pack: item); -- 15.
  VAR last: item; pos: INTEGER;
BEGIN
  IF sy#lex.package THEN error(lex_pos,'ожидалось PACKAGE%.') END;
  next;
  IF sy#lex.ident THEN error(lex_pos,'ожидался идентификатор%.') END;
  tbl.new_name_item(tbl.cl_pack,host,pack);
  comp_unit:=pack; next;
  IF sy#lex.is THEN error(lex_pos,'ожидалось IS%.') END;
  last:=NIL; make_uni_types(pack,last);
  next;
  WHILE sy#lex.end DO
    pos:=lex_pos;
    IF main_dcl_item(pack,last)=1 THEN
      error(pos,'такое описание не допустимо в определяющей части пакета%.');
    END;
  END;
  next;
  IF sy=lex.ident THEN
    IF val.str#pack^.nm^.str THEN
      error(lex_pos,'ожидалось имя пакета (%s)%.',pack^.nm^.str)
    END;
    next;
  END;
  check_semicolon;
END package;

PROCEDURE if_statement(host: item; VAR last: item); -- 113.
  VAR op_lst,t_lst,op,t: item; n: INTEGER;
BEGIN
  ASSERT(sy=lex.if);
  tbl.new_item(tbl.cl_operator,host,last); op:=last; op_lst:=NIL;
  next; n:=0;
  LOOP
    tbl.new_item(tbl.cl_operator,op,op_lst); t:=op_lst; t_lst:=NIL;
    op^.op_dw[n]:=t; INC(n); t^.op_lex:=lex.then; t^.op_dw_no:=1;
    t^.op_dw[0]:=expression(op,op_lst);
    t^.lex_pos:=lex_pos;
    IF sy#lex.then THEN error(lex_pos,'THEN expected') ELSE next END;
    operators_sequence(t,t_lst);
    IF sy#lex.elsif THEN EXIT END;
    next;
  END;
  IF sy=lex.else THEN
    tbl.new_item(tbl.cl_operator,op,op_lst); t:=op_lst; t_lst:=NIL;
    op^.op_dw[n]:=t; INC(n); t^.op_lex:=lex.then; t^.op_dw_no:=0;
    next;
    operators_sequence(t,t_lst);
  END;
  IF (sy#lex.end) OR (next_sy#lex.if) THEN
    error(lex_pos,'END IF expected');
  ELSE
    next; next;
  END;
  op^.op_dw_no:=n;
  check_semicolon;
END if_statement;

PROCEDURE loop_statement(host: item; VAR last: item); -- 117.
  VAR op,lst,v: item;
BEGIN
  tbl.new_item(tbl.cl_operator,host,last); op:=last; lst:=NIL;
  IF sy=lex.while THEN
    op^.op_lex:=lex.while; op^.op_dw_no:=1; next;
    op^.op_dw[0]:=expression(op,lst);
  ELSIF sy=lex.for THEN
    next;
    IF sy#lex.ident THEN
      error(lex_pos,'ident expected')
    ELSE
      tbl.new_name_item(tbl.cl_var,op,lst); v:=lst;
      v^.va_sz:=32; v^.va_rl:=4;
      op^.op_lex:=lex.for; op^.op_dw_no:=1; op^.op_dw[0]:=v;
      next;
      IF sy#lex.in THEN error(lex_pos,'IN expected') ELSE next END;
      v^.va_type:=type_discrete_range(NIL,NIL,0,op,lst);
    END;
  ELSE
    tbl.new_item(tbl.cl_operator,host,last); op:=last; lst:=NIL;
    op^.op_lex:=lex.loop;
  END;
  IF sy#lex.loop THEN error(lex_pos,'LOOP expected') ELSE next END;
  operators_sequence(op,lst);
  IF (sy#lex.end) OR (next_sy#lex.loop) THEN
    error(lex_pos,'END LOOP expected')
  ELSE
    next; next
  END;
  check_semicolon;
END loop_statement;

PROCEDURE case_select(host: item; VAR last: item; n: INTEGER); -- 116.
  VAR op,op_lst: item;
BEGIN
  IF sy#lex.when THEN error(lex_pos,'WHEN expected') ELSE next END;
  tbl.new_item(tbl.cl_operator,host,last); op:=last; op_lst:=NIL;
  host^.op_dw[n]:=op; op^.op_dw_no:=1;
  op^.op_dw[0]:=constant_expression(host,last,NIL);
  IF sy#lex.arrow THEN error(lex_pos,'"=>" expected') ELSE next END;
  operators_sequence(op,op_lst);
END case_select;

PROCEDURE case_statement(host: item; last: item); -- 115.
  VAR n: INTEGER; op,op_lst: item;
BEGIN
  ASSERT(sy=lex.case);
  tbl.new_item(tbl.cl_operator,host,last); op:=last; op_lst:=NIL;
  next;
  op^.op_dw[0]:=expression(op,op_lst);
  IF sy#lex.is THEN error(lex_pos,'IS expected') ELSE next END;
  n:=1;
  REPEAT
    case_select(op,op_lst,n); INC(n);
  UNTIL sy=lex.end;
  op^.op_dw_no:=n;
  IF next_sy#lex.case THEN
    error(lex_pos,'END CASE expected');
  ELSE
    next; next;
  END;
  check_semicolon;
END case_statement;

PROCEDURE block_operator(host: item; VAR last: item); -- 100.
BEGIN
  IF sy=lex.if THEN
    if_statement(host,last)
  ELSIF (sy=lex.while) OR (sy=lex.for) OR (sy=lex.loop) THEN
    loop_statement(host,last)
  ELSIF sy=lex.case THEN
    case_statement(host,last)
  ELSE
    error(lex_pos,'не реализовано'); check_semicolon;
  END;
END block_operator;

PROCEDURE proc_or_pack?(fr: item): item;
  VAR i: item;
BEGIN
  i:=fr;
  LOOP
    ASSERT(i#NIL);
    IF (i^.md=tbl.cl_func) OR (i^.md=tbl.cl_pack) THEN RETURN i END;
    i:=i^.host;
  END;
END proc_or_pack?;

PROCEDURE param_assign(proc,host: item; VAR last: item); -- 154.
BEGIN
  proc^.op_dw[proc^.op_dw_no]:=expression(host,last);
  INC(proc^.op_dw_no);
END param_assign;

PROCEDURE fact_param_partition(proc,host: item; VAR lst: item); -- 153.
BEGIN
  ASSERT(sy=lex.left_parenthesis);
  next;
  LOOP
    param_assign(proc,host,lst);
    IF sy#lex.comma THEN EXIT END;
    next;
  END;
  IF sy#lex.right_parenthesis THEN
    error(lex_pos,'ожидалось ")"')
  ELSE
    next;
  END;
END fact_param_partition;

PROCEDURE object_name(h: item; VAR l: item): item; -- 149.
  PROCEDURE oper_sign(): BOOLEAN;
  BEGIN
    IF sy#lex.string THEN RETURN FALSE END;
    CASE val.str[0] OF
      |'A': RETURN (val.str='AND') OR (val.str='ABS');
      |'O': RETURN (val.str='OR');
      |'X': RETURN (val.str='XOR');
      |'/': RETURN (val.str='/=') OR (val.str[1]=0c);
      |'<': RETURN (val.str[1]=0c) OR (val.str='<=');
      |'>': RETURN (val.str[1]=0c) OR (val.str='>=');
      |'+','-','&','=': RETURN (val.str[1]=0c);
      |'*': RETURN (val.str[1]=0c) OR (val.str='**');
      |'M': RETURN (val.str='MOD');
      |'R': RETURN (val.str='REM');
      |'N': RETURN (val.str='NOT');
    ELSE
      RETURN FALSE;
    END;
  END oper_sign;
  VAR elem_type: item;
  PROCEDURE array?(i: item): BOOLEAN;
  BEGIN
    IF i^.md=tbl.cl_var THEN i:=i^.va_type
    ELSIF i^.md=tbl.cl_prm THEN i:=i^.pr_type
    END;
    IF (i=NIL) OR (i^.md#tbl.cl_subtype) THEN RETURN FALSE END;
    i:=i^.su_type;
    IF (i=NIL) OR (i^.md#tbl.cl_type) THEN RETURN FALSE END;
    i:=i^.ty_type;
    IF i=NIL THEN RETURN FALSE END;
    IF i^.md=tbl.cl_array THEN elem_type:=i^.ar_elemtp; RETURN TRUE END;
    IF i^.md=tbl.cl_access THEN RETURN array?(i^.ac_type) END;
    RETURN FALSE;
  END array?;
  PROCEDURE var?(i: item): BOOLEAN;
  BEGIN
    RETURN (i^.md=tbl.cl_var) OR (i^.md=tbl.cl_prm);
  END var?;
  VAR op: item; func,arr: BOOLEAN; nm: tbl.name;
BEGIN
  func:=FALSE;
  IF (sy=lex.ident) OR (sy=lex.char) OR
     (sy=lex.string) & (next_sy=lex.left_parenthesis) & oper_sign() THEN
    op:=tbl.find_extended(h,nm);
    IF (nm=NIL) OR (op=NIL) THEN
      error(lex_pos,'не описанный идентификатор'); next; RETURN op;
    END;
    next;
    func:=(op^.md=tbl.cl_func) & (op^.fu_type#NIL);
    arr :=var?(op) & array?(op);
  ELSE
    error(lex_pos,'ожидался идентификатор'); RETURN NIL;
  END;
  IF func THEN
    tbl.new_item(tbl.cl_operator,h,l); l^.op_dw[0]:=op; op:=l;
    op^.op_lex:=lex.procedure; op^.op_dw_no:=1;
  END;
  LOOP
    IF sy=lex.left_parenthesis THEN
      IF func THEN
        -- вызов функции
        func:=FALSE; fact_param_partition(op,h,l);
      ELSIF arr THEN
        -- индексация или отрезок
        arr:=array?(elem_type);
        tbl.new_item(tbl.cl_operator,h,l); l^.op_dw[0]:=op; op:=l;
        op^.op_lex:=lex.array;
        op^.op_dw_no:=1;
        next;
        LOOP
          op^.op_dw[op^.op_dw_no]:=expression(h,l);
          INC(op^.op_dw_no);
          IF sy#lex.comma THEN EXIT END;
          next;
        END;
        IF sy#lex.right_parenthesis THEN
          error(lex_pos,'ожидалось )');
        ELSE
          next;
        END;
      ELSE
        RETURN op;
      END;
    ELSIF sy=lex.point THEN
      -- поле записи
      tbl.new_item(tbl.cl_operator,h,l); l^.op_dw[0]:=op; op:=l;
      op^.op_lex:=sy; op^.op_dw_no:=2; op^.op_dw[1]:=NIL; next;
      IF sy=lex.all THEN
        tbl.new_item(tbl.cl_operator,h,l); op^.op_dw[1]:=l;
        l^.op_lex:=sy; l^.op_dw_no:=1; next;
      ELSE
        IF sy#lex.ident THEN
          error(lex_pos,'ожидался идентификатор поля записи'); RETURN op;
        END;
        nm:=tbl.find();
        IF nm=NIL THEN
          error(lex_pos,'не описанный идентификатор'); next; RETURN op;
        END;
        op^.op_dw[1]:=nm^.obj;
        next;
      END;
    ELSIF sy=lex.apostrophe THEN
      -- атрибут
HALT(1);
    ELSE
      EXIT
    END;
  END;
  RETURN op;
END object_name;

PROCEDURE string_literal(host: item; VAR last: item): item; -- 146.
  VAR nm: tbl.name; op,f: item; i: INTEGER; str: ARRAY [0..3] OF CHAR;
BEGIN
  tbl.new_item(tbl.cl_operator,host,last); op:=last;
  op^.op_lex:=lex.left_parenthesis; op^.op_dw_no:=val.len+1;
  str:="' '";
  FOR i:=0 TO val.len-1 DO
    tbl.new_item(tbl.cl_operator,host,last); f:=last;
    f^.op_lex:=lex.procedure; f^.op_dw_no:=1; op^.op_dw[i+1]:=f;
    str[1]:=val.str[i]; nm:=tbl.find_nm(str);
    IF nm=NIL THEN
      error(lex_pos,'не описан %s',str);
    ELSE
      f^.op_dw[0]:=tbl.find_vis(nm,host);
      IF f^.op_dw[0]=NIL THEN
        error(lex_pos,'не виден %s',str);
      END;
    END;
  END;
  op^.op_dw[0]:=NIL; next; RETURN op;
END string_literal;

PROCEDURE primary(host: item; VAR last: item): item; -- 137.
  VAR op: item;
BEGIN
  IF sy=lex.int_number THEN
    tbl.new_item(tbl.cl_operator,host,last); op:=last;
    op^.op_lex:=sy; op^.op_dw_no:=0; op^.op_val0:=val.int; next;
    RETURN op;
  ELSIF sy=lex.null THEN
    tbl.new_item(tbl.cl_operator,host,last); op:=last;
    op^.op_lex:=sy; op^.op_dw_no:=0; next;
    RETURN op;
  ELSIF sy=lex.string THEN
    RETURN string_literal(host,last);
  ELSIF sy=lex.left_parenthesis THEN
    next;
    op:=expression(host,last);
    IF sy#lex.right_parenthesis THEN
      error(lex_pos,'ожидалось ")"')
    ELSE
      next;
    END;
    RETURN op;
  ELSE
    RETURN object_name(host,last);
  END;
END primary;

PROCEDURE factor(host: item; VAR last: item): item; -- 136.
  VAR l,m: item;
BEGIN
  IF sy=lex.abs THEN
    tbl.new_item(tbl.cl_operator,host,last); l:=last;
    l^.op_lex:=lex.procedure;
    l^.op_dw[0]:=tbl.find_vis(tbl.find_nm('abs'),host);
    IF l^.op_dw[0]=NIL THEN
      error(lex_pos,'операция не описана');
    END;
    l^.op_dw_no:=2; next;
    l^.op_dw[1]:=primary(host,last);
  ELSIF sy=lex.not THEN
    tbl.new_item(tbl.cl_operator,host,last); l:=last;
    l^.op_lex:=lex.procedure;
    l^.op_dw[0]:=tbl.find_vis(tbl.find_nm('not'),host);
    IF l^.op_dw[0]=NIL THEN
      error(lex_pos,'операция не описана');
    END;
    l^.op_dw_no:=2; next;
    l^.op_dw[1]:=primary(host,last);
  ELSE
    l:=primary(host,last);
    IF sy=lex.double_star THEN
      m:=l;
      tbl.new_item(tbl.cl_operator,host,last); l:=last;
      l^.op_lex:=lex.procedure;
      l^.op_dw[0]:=tbl.find_vis(tbl.find_nm('**'),host);
      IF l^.op_dw[0]=NIL THEN
        error(lex_pos,'операция не описана');
      END;
      l^.op_dw_no:=3; next;
      l^.op_dw[1]:=m;
      l^.op_dw[2]:=primary(host,last);
    END;
  END;
  RETURN l;
END factor;

PROCEDURE term(host: item; VAR last: item): item; -- 135.
  VAR l: item;
  PROCEDURE right_part(n: ARRAY OF CHAR): item;
    VAR m: item;
  BEGIN
    tbl.new_item(tbl.cl_operator,host,last); m:=last;
    m^.op_lex:=lex.procedure;
    m^.op_dw[0]:=tbl.find_vis(tbl.find_nm(n),host);
    IF m^.op_dw[0]=NIL THEN
      error(lex_pos,'операция "%s" не описана',n);
    END;
    next;
    m^.op_dw_no:=3; m^.op_dw[1]:=l; m^.op_dw[2]:=factor(host,last);
    RETURN m;
  END right_part;
BEGIN
  l:=factor(host,last);
  LOOP
    IF    sy=lex.star  THEN l:=right_part('*'0c)
    ELSIF sy=lex.slash THEN l:=right_part('/'0c)
    ELSIF sy=lex.mod   THEN l:=right_part('mod')
    ELSIF sy=lex.rem   THEN l:=right_part('rem')
    ELSE RETURN l;
    END;
  END;
END term;

PROCEDURE simple_expression(host: item; VAR last: item): item; --134.
  VAR l: item;
  PROCEDURE right_part(n: CHAR): item;
    VAR m: item;
  BEGIN
    tbl.new_item(tbl.cl_operator,host,last); m:=last;
    m^.op_lex:=lex.procedure;
    val.str[0]:=n; val.str[1]:=0c; val.len:=1;
    m^.op_dw[0]:=tbl.find_vis(tbl.find(),host);
    IF m^.op_dw[0]=NIL THEN
      error(lex_pos,'операция не описана');
    END;
    next;
    IF l=NIL THEN
      m^.op_dw_no:=2; m^.op_dw[1]:=term(host,last);
    ELSE
      m^.op_dw_no:=3; m^.op_dw[1]:=l; m^.op_dw[2]:=term(host,last);
    END;
    RETURN m;
  END right_part;
BEGIN
  l:=NIL;
  IF sy=lex.plus THEN l:=right_part('+');
  ELSIF sy=lex.minus THEN l:=right_part('-');
  ELSE l:=term(host,last);
  END;
  LOOP
    IF sy=lex.plus THEN l:=right_part('+');
    ELSIF sy=lex.minus THEN l:=right_part('-');
    ELSIF sy=lex.ampersand THEN l:=right_part('&');
    ELSE RETURN l;
    END;
  END;
END simple_expression;

PROCEDURE relation(host: item; VAR last: item): item; -- 133.
  VAR l: item;
  PROCEDURE right_part(n: ARRAY OF CHAR): item;
    VAR m: item; i: INTEGER;
  BEGIN
    tbl.new_item(tbl.cl_operator,host,last); m:=last;
    m^.op_lex:=lex.procedure;
    i:=0;
    WHILE n[i]#0c DO val.str[i]:=n[i]; INC(i) END;
    val.str[i]:=0c; val.len:=i;
    m^.op_dw[0]:=tbl.find_vis(tbl.find(),host);
    IF m^.op_dw[0]=NIL THEN
      error(lex_pos,'операция не описана');
    END;
    m^.op_dw_no:=3; m^.op_dw[1]:=l; next;
    m^.op_dw[2]:=simple_expression(host,last);
    RETURN m;
  END right_part;
  VAR m: item;
BEGIN
  l:=simple_expression(host,last);
  CASE sy OF
    |lex.equal        : RETURN right_part('='0c);
    |lex.inequality   :
      tbl.new_item(tbl.cl_operator,host,last); m:=last;
      m^.op_lex:=lex.procedure; val.str:='not'; val.len:=3;
      m^.op_dw[0]:=tbl.find_vis(tbl.find(),host);
      IF m^.op_dw[0]=NIL THEN
        error(lex_pos,'операция не описана');
      END;
      m^.op_dw_no:=2; m^.op_dw[1]:=right_part('='0c); RETURN m;
    |lex.less         : RETURN right_part('<'0c);
    |lex.less_equal   : RETURN right_part('<=');
    |lex.greater      : RETURN right_part('>'0c);
    |lex.greater_equal: RETURN right_part('>=');
  ELSE
    RETURN l;
(*
    IF (sy=lex.not) & (next_sy=lex.in) THEN
      m:=alloc(); next;
      m^.down:=rel_in(); RETURN m;
    ELSIF sy=lex.in THEN
      RETURN rel_in()
    ELSE
      RETURN l;
    END;
*)
  END;
END relation;

PROCEDURE expression(host: item; VAR last: item): item; -- 132.
  VAR l: item;
  PROCEDURE right_part(n: ARRAY OF CHAR): item;
    VAR m: item;
  BEGIN
    tbl.new_item(tbl.cl_operator,host,last); m:=last;
    m^.op_lex:=lex.procedure;
    m^.op_dw[0]:=tbl.find_vis(tbl.find_nm(n),host);
    IF m^.op_dw[0]=NIL THEN
      error(lex_pos,'операция "%s" не описана',n);
    END;
    m^.op_dw_no:=3; m^.op_dw[1]:=l; next;
    m^.op_dw[2]:=relation(host,last);
    RETURN m;
  END right_part;
BEGIN
  l:=relation(host,last);
  IF sy=lex.and THEN
    REPEAT l:=right_part('and') UNTIL sy#lex.end;
  ELSIF sy=lex.or THEN
    REPEAT l:=right_part('or') UNTIL sy#lex.or;
  ELSIF sy=lex.xor THEN
    REPEAT l:=right_part('xor') UNTIL sy#lex.xor;
  END;
  RETURN l
END expression;

PROCEDURE call_or_assign(host: item; VAR last: item); -- 104. 103.
  VAR op,lst: item;
BEGIN
  tbl.new_item(tbl.cl_operator,host,last); op:=last; lst:=NIL;
  op^.op_dw[0]:=object_name(op,lst);
  IF sy=lex.assign THEN
    op^.op_dw_no:=2; op^.op_lex:=sy; op^.lex_pos:=lex_pos; next;
    op^.op_dw[1]:=expression(op,lst);
  ELSE
    op^.op_dw_no:=1;
    op^.op_lex:=lex.procedure;
    IF sy=lex.left_parenthesis THEN fact_param_partition(op,op,lst) END;
  END;
  check_semicolon;
END call_or_assign;

PROCEDURE operator_return(host: item; VAR last: item);
  VAR ret,proc,elast: item;
BEGIN
  tbl.new_item(tbl.cl_operator,host,last); ret:=last;
  ret^.op_lex:=sy;
  ret^.op_dw_no:=1;
  proc:=proc_or_pack?(host);
  ret^.op_dw[0]:=proc;
  next;
  IF proc^.md=tbl.cl_pack THEN
    error(last^.lex_pos,'можно использовать только внутри процедуры');
  ELSIF proc^.fu_type#NIL THEN
    ret^.op_dw_no:=2; elast:=NIL;
    ret^.op_dw[1]:=expression(ret,elast);
  END;
  check_semicolon;
END operator_return;

PROCEDURE exit_statement(host: item; VAR last: item);
  VAR xit,loop: item;
BEGIN
  ASSERT(sy=lex.exit);
  tbl.new_item(tbl.cl_operator,host,last); xit:=last;
  xit^.op_dw_no:=1; next;
  loop:=host;
  LOOP
    IF loop=NIL THEN
      error(lex_pos,'можно использовать только внутри цикла'); EXIT;
    ELSIF (loop^.md=tbl.cl_operator) & (
          (loop^.op_lex=lex.loop) OR (loop^.op_lex=lex.for) OR
          (loop^.op_lex=lex.while) ) THEN EXIT
    END;
    loop:=loop^.host;
  END;
  xit^.op_dw[0]:=loop;
  check_semicolon;
END exit_statement;

PROCEDURE simple_operator(host: item; VAR last: item); -- 99.
BEGIN
  IF sy=lex.null THEN
    next; check_semicolon;
  ELSIF sy=lex.return THEN
    operator_return(host,last);
  ELSIF sy=lex.exit THEN
    exit_statement(host,last);
  ELSE
    call_or_assign(host,last);
  END;
END simple_operator;

PROCEDURE operator(host: item; VAR last: item); -- 98.
BEGIN
  IF (sy=lex.if) OR
     (sy=lex.case) OR
     (sy=lex.accept) OR
     (sy=lex.select) OR
     (sy=lex.while) OR
     (sy=lex.for) OR
     (sy=lex.loop) OR
     (sy=lex.declare) OR
     (sy=lex.begin) OR
     (sy=lex.ident) & (next_sy=lex.colon) THEN
    block_operator(host,last)
  ELSE
    simple_operator(host,last)
  END;
END operator;

PROCEDURE operators_sequence(host: item; VAR last: item); -- 97.
  VAR n: INTEGER;
BEGIN
  n:=0;
  LOOP
    IF (sy=lex.end) OR (sy=lex.else) OR
       (sy=lex.elsif) OR (sy=lex.when) THEN EXIT END;
    operator(host,last); INC(n);
  END;
  IF n=0 THEN error(lex_pos,'должен быть хотя бы один оператор') END;
END operators_sequence;

PROCEDURE package_body(host: item; VAR h_last: item); -- 28.
FORWARD;

PROCEDURE additional_dcl_item(host: item; VAR last: item);
  VAR i: INTEGER;
BEGIN
  IF (sy=lex.package) & (next_sy#lex.body) THEN
    package(host,last);
  ELSIF (sy=lex.package) & (next_sy=lex.body) THEN
    package_body(host,last);
  ELSIF (sy=lex.procedure) OR (sy=lex.function) THEN
    i:=def_procedure(host,last);
  ELSE
    error(lex_pos,'ожидалось какое-нибудь описание%.');
  END;
END additional_dcl_item;

PROCEDURE declaration_partition(host: item; VAR last: item); -- 32.
BEGIN
  LOOP
    IF (sy=lex.begin) OR (sy=lex.end) THEN EXIT END;
    -- основной элемент описания
    IF main_dcl_item(host,last)=1 THEN EXIT END;
  END;
  LOOP
    IF (sy=lex.begin) OR (sy=lex.end) THEN EXIT END;
    -- дополнительный элемент описания
    additional_dcl_item(host,last);
  END;
END declaration_partition;

PROCEDURE package_body(host: item; VAR h_last: item); -- 28.
  VAR pack,last,private: item;
BEGIN
  ASSERT((sy=lex.package) & (next_sy=lex.body));
  next; next;
  IF sy#lex.ident THEN error(lex_pos,'ожидалось имя пакета%.') END;
  pack:=tbl.find_vis(tbl.find(),host);
  IF pack=NIL THEN error(lex_pos,'пакет не описан%.') END;
  IF pack^.md#tbl.cl_pack THEN error(lex_pos,'это не пакет%.') END;
  next;
  comp_unit:=pack;
  last:=pack^.pa_list;
  IF last#NIL THEN
    WHILE last^.host_nxt#NIL DO last:=last^.host_nxt END;
  END;
  private:=last;
  IF sy#lex.is THEN error(lex_pos,'ожидалось IS%.') END;
  next;
  IF (sy#lex.begin) & (sy#lex.end) THEN
    -- 32. раздел описаний
    declaration_partition(pack,last);
  END;
  IF sy=lex.begin THEN
    next;
    -- 97. последовательность операторов
    operators_sequence(pack,last);
  END;
  IF sy#lex.end THEN
    error(lex_pos,'ожидалось END пакета %s%.',pack^.nm^.str)
  END;
  next;
  IF sy=lex.ident THEN
    IF pack^.nm#tbl.find() THEN
      error(lex_pos,'ожидалось END пакета %s%.',pack^.nm^.str)
    END;
    next;
  END;
  check_semicolon;
  IF (pack^.pa_priv=NIL) & (private#NIL) THEN
    pack^.pa_priv:=private^.host_nxt
  END;
END package_body;

VAR it: item;

BEGIN
  next;
  next;
  WHILE sy#lex.eot DO
    IF (sy=lex.package) & (next_sy=lex.body) THEN
      package_body(NIL,it);
      IF lex.error_cnt()=0 THEN
        adaType.do_package(it);
        tty.print('[ %s ] Variables allocation...\n',it^.nm^.str);
        adaMem.do_package(it,TRUE);
      END;
      IF lex.error_cnt()=0 THEN
        tty.print('[ %s ] Code generation...\n',it^.nm^.str);
        adaGen.do_package(it);
        IF arg.Flag?('t') THEN bug.print_item(it) END;
      END;
    ELSE
      package(NIL,it);
      IF lex.error_cnt()=0 THEN
        adaType.do_package(it);
        tty.print('[ %s ] Variables allocation...\n',it^.nm^.str);
        adaMem.do_package(it,FALSE);
      END;
    END;
  END;
  lex.put_errors;
END ada.
