IMPLEMENTATION MODULE adaType; (* 01-Sep-90. (c) KRONOS *)

IMPORT lex : adaLex;
IMPORT tbl : adaTable;

FROM Terminal   IMPORT  print;
FROM adaTable   IMPORT  item, class;
FROM adaLex     IMPORT  error;

VAR
  uni_integer: item;

PROCEDURE expression(e,t: item; pos: INTEGER): item;
FORWARD;

PROCEDURE check_type(t1,t2: item; pos: INTEGER);
  PROCEDURE err;
  BEGIN
    error(pos,'типы не совместимы');
  END err;
BEGIN
  IF (t1=NIL) OR (t2=NIL) THEN err; RETURN END;
  ASSERT(t1^.md=cl_subtype);
  ASSERT(t2^.md=cl_subtype);
  IF t1=t2 THEN RETURN END;
  IF t1^.su_type=t2^.su_type THEN RETURN END;
  t1:=t1^.su_type^.ty_type; t2:=t2^.su_type^.ty_type;
  IF (t1^.md=cl_integer) & (t2^.md=cl_integer) &
     (t1^.in_universal OR t2^.in_universal) THEN RETURN END;
  IF (t1^.md=cl_enumeration) & (t2^.md=cl_enumeration) &
     (t1^.en_universal OR t2^.en_universal) THEN RETURN END;
  IF (t1^.md=cl_access) & (t2^.md=cl_access) &
     (t1^.ac_universal OR t2^.ac_universal) THEN RETURN END;
  err;
END check_type;

PROCEDURE soft_check_type(t1,t2: item): BOOLEAN;
BEGIN
  IF (t1=NIL) OR (t2=NIL) THEN RETURN FALSE END;
  ASSERT(t1^.md=cl_subtype); ASSERT(t2^.md=cl_subtype);
  IF t1=t2 THEN RETURN TRUE END;
  IF t1^.su_type=t2^.su_type THEN RETURN TRUE END;
  IF t1^.su_type^.ty_type=t2^.su_type^.ty_type THEN RETURN TRUE END;
  t1:=t1^.su_type^.ty_type; t2:=t2^.su_type^.ty_type;
  IF (t1^.md=cl_integer) & (t2^.md=cl_integer) &
     (t1^.in_universal OR t2^.in_universal) THEN RETURN TRUE END;
  IF (t1^.md=cl_enumeration) & (t2^.md=cl_enumeration) &
     (t1^.en_universal OR t2^.en_universal) THEN RETURN TRUE END;
  RETURN FALSE;
END soft_check_type;

PROCEDURE var_or_access_type(j: item): item;
  -- return NIL если не subtype
  -- иначе return конструктор
BEGIN
  IF j=NIL THEN RETURN NIL END;
  ASSERT(j^.md=cl_subtype);
  IF (j^.su_type=NIL) OR (j^.su_type^.md#cl_type) OR
     (j^.su_type^.ty_type=NIL) THEN RETURN NIL END;
  j:=j^.su_type^.ty_type;
  IF j^.md=cl_access THEN
    IF (j^.ac_type=NIL) OR
       (j^.ac_type^.md#cl_subtype) OR (j^.ac_type^.su_type=NIL) OR
       (j^.ac_type^.su_type^.md#cl_type) OR
       (j^.ac_type^.su_type^.ty_type=NIL) THEN RETURN NIL END;
    j:=j^.ac_type^.su_type^.ty_type;
  END;
  RETURN j;
END var_or_access_type;

PROCEDURE check_constraint(t: item; pos: INTEGER);
  VAR k: item; i: INTEGER;
BEGIN
  k:=t^.su_type^.ty_type;
  IF k^.md=cl_array THEN
    FOR i:=0 TO k^.ar_index_no-1 DO
      IF t^.su_descr[i]=NIL THEN
        error(pos,'должен быть ограниченный тип'); RETURN
      END;
    END;
  END;
END check_constraint;

PROCEDURE array_agregate(e,t: item; level: INTEGER);
  VAR j,k: item; n,m: INTEGER;
BEGIN
  ASSERT(e#NIL); ASSERT(t#NIL);
  IF t^.su_type^.ty_type^.md#cl_array THEN
    error(e^.lex_pos,'должен быть массив'); RETURN
  END;
  IF (e^.md#cl_operator) OR (e^.op_lex#lex.left_parenthesis) THEN
    error(e^.lex_pos,'должен быть агрегат массива'); RETURN
  END;
  e^.op_dw[0]:=t; e^.op_val0:=level;
  m:=t^.su_descr[level+1]^.su_last-t^.su_descr[level+1]^.su_first+1;
  IF m<0 THEN m:=0 END;
  IF m#e^.op_dw_no-1 THEN
    error(e^.lex_pos,'не совпадает количество компонент'); RETURN
  END;
  IF level+1<t^.su_last-1 THEN
    FOR n:=1 TO m DO array_agregate(e^.op_dw[n],t,level+1) END;
  ELSE
    j:=t^.su_type^.ty_type^.ar_elemtp; ASSERT(j#NIL);
    FOR n:=0 TO m-1 DO
      ASSERT(e^.op_dw[n]#NIL);
      k:=expression(e^.op_dw[n+1],j,e^.lex_pos);
      IF k=NIL THEN
        error(e^.lex_pos,'типы не однозначны');
      ELSE
        check_type(j,k,e^.lex_pos);
      END;
    END;
  END;
END array_agregate;

PROCEDURE expression(e,t: item; pos: INTEGER): item;
  VAR j,k,l: item; n,m: INTEGER;
BEGIN
  CASE e^.md OF
    |cl_var:
      check_constraint(e^.va_type,pos);
      IF t#NIL THEN check_type(t,e^.va_type,pos) END;
      RETURN e^.va_type;
    |cl_prm:
      check_constraint(e^.pr_type,pos);
      IF t#NIL THEN check_type(t,e^.pr_type,pos) END;
      RETURN e^.pr_type;
    |cl_subtype:
      RETURN e;
    |cl_operator:
      IF e^.op_lex=lex.array THEN
        j:=expression(e^.op_dw[0],NIL,e^.lex_pos);
        IF j=NIL THEN
          error(e^.lex_pos,'типы не однозначны'); RETURN t;
        END;
        k:=var_or_access_type(j);
        IF (k=NIL) OR (k^.md#cl_array) THEN
          error(e^.lex_pos,'должен быть массив');
        ELSE
          IF k^.ar_index_no#e^.op_dw_no-1 THEN
            error(e^.lex_pos,'неправильное чило индексов');
          ELSE
            IF t#NIL THEN check_type(t,k^.ar_elemtp,pos) END;
            FOR n:=1 TO e^.op_dw_no-1 DO
              j:=expression(e^.op_dw[n],k^.ar_index[n-1],e^.lex_pos);
            END;
            RETURN k^.ar_elemtp;
          END;
        END;
      ELSIF e^.op_lex=lex.point THEN
        ASSERT(e^.op_dw_no=2);
        j:=expression(e^.op_dw[0],NIL,e^.lex_pos);
        k:=var_or_access_type(j);
        IF k=NIL THEN
          error(e^.lex_pos,'типы не однозначны'); RETURN t;
        ELSIF (e^.op_dw[1]^.md=cl_operator) &
              (e^.op_dw[1]^.op_lex=lex.all) THEN
          IF j^.su_type^.ty_type^.md#cl_access THEN
            error(e^.lex_pos,'должен быть указатель'); RETURN t;
          END;
          j:=j^.su_type^.ty_type^.ac_type;
          e^.op_dw[1]^.op_dw[0]:=j;
          RETURN j;
        ELSIF k^.md#cl_record THEN
          error(e^.lex_pos,'должна быть запись или указатель на запись');
          RETURN t;
        ELSE
          j:=e^.op_dw[1]; l:=k^.re_fields;
          WHILE (l#NIL) & (l^.nm#j^.nm) DO l:=l^.fi_nxt END;
          IF l=NIL THEN
            error(e^.lex_pos,'нет такого поля');
          ELSE
            e^.op_dw[1]:=l;
            IF t#NIL THEN check_type(t,l^.fi_type,pos) END;
            RETURN l^.fi_type;
          END;
        END;
      ELSIF e^.op_lex=lex.int_number THEN
        IF t#NIL THEN check_type(t,uni_integer,pos) END;
        RETURN uni_integer;
      ELSIF e^.op_lex=lex.procedure THEN
        j:=tbl.find_vis(e^.op_dw[0]^.nm,e^.host); k:=j; l:=NIL;
        WHILE k#NIL DO
          IF (e^.op_dw_no-1=k^.fu_prm_no) &
             ( (t=NIL) OR soft_check_type(t,k^.fu_type) ) THEN
            IF l#NIL THEN RETURN NIL END; l:=k;
          END;
          k:=k^.fu_nxt;
        END;
        IF l=NIL THEN
          error(pos,'типы не совместимы');
        ELSE
          IF t=NIL THEN t:=k^.fu_type END;
          e^.op_dw[0]:=l; k:=l^.fu_prm;
          FOR n:=1 TO e^.op_dw_no-1 DO
            IF t^.su_type^.ty_type=k^.pr_type^.su_type^.ty_type THEN
              j:=expression(e^.op_dw[n],t,e^.lex_pos);
            ELSE
              j:=expression(e^.op_dw[n],k^.pr_type,e^.lex_pos);
            END;
            IF j=NIL THEN error(e^.lex_pos,'типы не однозначны') END;
            k:=k^.pr_nxt;
          END;
          RETURN t;
        END;
      ELSIF e^.op_lex=lex.left_parenthesis THEN -- array agregate
        IF t=NIL THEN RETURN NIL END;
        array_agregate(e,t,-1);
        RETURN t;
      END;
  ELSE
  END;
  RETURN NIL;
END expression;

PROCEDURE one(i: item);
  VAR j,type1,type2: item;
BEGIN
  CASE i^.md OF
    |cl_pack: j:=i^.pa_list; WHILE j#NIL DO one(j); j:=j^.host_nxt END;
    |cl_func: j:=i^.fu_list; WHILE j#NIL DO one(j); j:=j^.host_nxt END;
    |cl_var:
    |cl_operator:
      IF i^.op_lex=lex.assign THEN
        type1:=expression(i^.op_dw[0],NIL,i^.lex_pos);
        IF type1#NIL THEN
          type2:=expression(i^.op_dw[1],type1,i^.lex_pos);
          IF type2=NIL THEN
            error(i^.lex_pos,'типы не однозначны');
          ELSE
            check_type(type1,type2,i^.lex_pos);
          END;
        ELSE
          type2:=expression(i^.op_dw[1],NIL,i^.lex_pos);
          IF type2#NIL THEN
            type1:=expression(i^.op_dw[0],type2,i^.lex_pos);
            IF type1=NIL THEN
              error(i^.lex_pos,'типы не однозначны');
            ELSE
              check_type(type1,type2,i^.lex_pos);
            END;
          ELSE
            error(i^.lex_pos,'типы не однозначны');
          END;
        END;
      ELSIF i^.op_lex=lex.for THEN
        j:=i^.op_list; WHILE j#NIL DO one(j); j:=j^.host_nxt END;
      END;
  ELSE
  END;
END one;

PROCEDURE do_package(pack: item);
BEGIN
  uni_integer:=pack^.pa_uni_int;
  ASSERT(uni_integer#NIL);
  one(pack);
END do_package;

PROCEDURE do_expression(e,type: item);
  VAR pack,i: item;
BEGIN
  IF lex.error_cnt()#0 THEN RETURN END;
  ASSERT(e#NIL);
  pack:=e;
  WHILE pack^.host#NIL DO pack:=pack^.host END;
  ASSERT(pack^.md=cl_pack);
  uni_integer:=pack^.pa_uni_int;
  ASSERT(uni_integer#NIL);
  i:=expression(e,type,lex.lex_pos);
  IF i=NIL THEN
    error(lex.lex_pos,'неоднозначность типов');
  END;
END do_expression;

END adaType.
