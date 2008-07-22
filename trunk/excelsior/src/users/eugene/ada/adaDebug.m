IMPLEMENTATION MODULE adaDebug; (* 28-Aug-90. (c) KRONOS *)

IMPORT lex : adaLex;
IMPORT tty : StdIO;

FROM SYSTEM     IMPORT  WORD;
FROM adaTable   IMPORT  item, class;
FROM StdIO      IMPORT  print;

PROCEDURE print_item(i: item);
  VAR level: INTEGER;
PROCEDURE print_item0(i: item);
  PROCEDURE name(t: item);
  BEGIN
    IF t^.nm=NIL THEN print('%8$h            ',t);
    ELSE print('%-20s',t^.nm^.str);
    END;
  END name;
  VAR j: item; k: INTEGER;
BEGIN
  FOR k:=0 TO level-1 DO print('  ') END;
  name(i);
  WITH i^ DO
    CASE md OF
      |cl_operator:
        print('operator ');
        CASE op_lex OF
          |lex.return: print('return ');
          |lex.assign: print(':= ');
          |lex.procedure: print('procedure ');
          |lex.point: print(' . ');
          |lex.array: print('array ');
        ELSE
          print('???');
        END;
        FOR k:=0 TO op_dw_no-1 DO name(op_dw[k]) END;
        print('\n');
        INC(level);
        j:=op_list; WHILE j#NIL DO print_item0(j); j:=j^.host_nxt END;
        DEC(level);
      |cl_enumeration:
        print('enumeration %3d bits',en_bits);
        IF en_signed THEN print(' signed') END;
        IF en_universal THEN print(' universal') END;
      |cl_integer:
        print('integer     %3d bits',in_bits);
        IF in_signed THEN print(' signed') END;
        IF in_universal THEN print(' universal') END;
      |cl_float:
        print('float       %3d bits, %3d digits, %3d delta',
              fl_bits,fl_digits,fl_delta);
        IF fl_universal THEN print(' universal') END;
      |cl_array:
        print('array ');
        FOR k:=0 TO ar_index_no-1 DO
          print('  %3d..%3d',ar_index[k]^.su_first,ar_index[k]^.su_last);
        END;
        print(' of %d bits',ar_elembits);
      |cl_record:
        print('record %3d bits',re_bits);
--        re_fields   : item;     -- список полей (must be field)
--        re_descr    : item_block; -- массив дискриминантов
--        re_descr_no : INTEGER;
      |cl_field:
        print('field %3d bits, %3d ofs, type ',fi_bits,fi_offs);
        name(fi_type);
--        fi_nxt      : item;     -- must be field
--        fi_descr    : INTEGER;
--        fi_case     : INTEGER;
      |cl_access:
--        ac_type     : item;     -- must be subtype
      |cl_task:
      |cl_var:
        print('variable ');
        name(va_type);
--        va_alloc    : item;
--        va_const    : BOOLEAN;
--        va_sz       : INTEGER;
--        va_adr      : ADDRESS;
--        va_rl       : INTEGER;  -- метод доступа
        print(', %1d',va_rl);
        print(', %$8h',va_val0);
        print(', %$8h',va_val1);
      |cl_func:
        print('function');
--        fu_import   : BOOLEAN;
--        fu_type     : item;     -- if NIL then proc else must be subtype
--        fu_list     : item;     -- список объектов
--        fu_prm      : item;     -- must be prm
--        fu_prm_no   : INTEGER;
--        fu_var_no   : INTEGER;
--        fu_inline   : BOOLEAN;
--        fu_code     : WORD;
--        fu_rl       : INTEGER;  -- метод вызова
        print('\n');
        INC(level);
        j:=fu_list; WHILE j#NIL DO print_item0(j); j:=j^.host_nxt END;
        DEC(level);
-- fu_rl=2 : константа перечислимого типа, val0 - номер, val1 - значение
--        fu_val0     : INTEGER;
--        fu_val1     : INTEGER;
--        fu_nxt      : item;     -- используется локально
      |cl_prm:
--        pr_type     : item;     -- must be subtype
--        pr_nxt      : item;     -- must be cl_prm
--        pr_in       : BOOLEAN;
--        pr_out      : BOOLEAN;
--        pr_val0     : INTEGER;
      |cl_pack:
        print('package');
--        pa_import   : BOOLEAN;
--        pa_ref      : BOOLEAN;
--        pa_list     : item;
--        pa_priv     : item;
--        pa_body     : item;
--        pa_use      : item;
--        pa_var_no   : INTEGER;
--        pa_code     : WORD;
        print('\n');
        INC(level);
        j:=pa_list; WHILE j#NIL DO print_item0(j); j:=j^.host_nxt END;
        DEC(level);
      |cl_use:
--        us_pack     : item;     -- must be cl_pack
--        us_nxt      : item;     -- must be cl_use
      |cl_subtype:
        print('subtype %$8h..%$8h, type ',su_first,su_last);
        name(su_type);
--        su_descr    : item_block;
--        su_private  : BOOLEAN;
--        su_limited  : BOOLEAN;
      |cl_type:
        print('type ');
        name(ty_type);
    END;
  END;
  print('\n');
END print_item0;

BEGIN
  level:=0; print_item0(i);
END print_item;

PROCEDURE print_cmd(c,pc: INTEGER);
  PROCEDURE reg(n: WORD);
  BEGIN
    IF    INTEGER(n)<8  THEN tty.print('g%d',n);
    ELSIF INTEGER(n)<16 THEN tty.print('i%d',INTEGER(n)-8);
    ELSIF INTEGER(n)<24 THEN tty.print('r%d',INTEGER(n)-16);
    ELSE tty.print('o%d',INTEGER(n)-24);
    END;
  END reg;
  VAR
    s,s1: BITSET;
    n   : INTEGER;
BEGIN
  s:=BITSET(c);
  IF s={} THEN
    tty.print('nop\n');
  ELSIF s*{30,31}={31} THEN
    IF 29 IN s THEN s:=s+{30,31} ELSE s:=s-{30,31} END;
    tty.print('call %5d [%$8h]\n',s,pc+INTEGER(s)*4);
  ELSIF s*{22,23,24,30,31}={22,23,24,30} THEN
    tty.print('set hi '); reg((s*{25..29})>>25);
    tty.print(', %$8h\n',(s*{0..21})<<10);
  ELSIF s*{30,31}={30} THEN
    CASE INTEGER(s*{22..24})>>22 OF
      |4: tty.print('branch integer');
      |5: tty.print('branch float  ');
      |6: tty.print('branch co-cpu ');
      |0: tty.print('trap   integer');
    ELSE
      tty.print('%$8h\n',s); RETURN
    END;
    CASE INTEGER(s*{25..28})>>25 OF
      | 0: tty.print(' N');
      | 1: tty.print(' C');
      | 2: tty.print(' V');
      | 3: tty.print(' Z');
      | 4: tty.print(' ^N');
      | 5: tty.print(' ^C');
      | 6: tty.print(' ^V');
      | 7: tty.print(' ^Z');
      |15: tty.print(' always');
    ELSE
      tty.print(' ???');
    END;
    IF 21 IN s THEN s1:=s+{22..31} ELSE s1:=s-{22..31} END;
    tty.print(' %3d [%$8h]',s1,pc+INTEGER(s1)*4);
    IF 29 IN s THEN
      tty.print(', annual\n')
    ELSE
      tty.print('\n')
    END;
  ELSIF s*{30,31}={30,31} THEN
    IF 24 IN s THEN
      IF 23 IN s THEN tty.print('store') ELSE tty.print('load') END;
      IF 22 IN s THEN tty.print('double') END;
      IF 21 IN s THEN tty.print('co-cpu') ELSE tty.print('fpu') END;
      IF 20 IN s THEN tty.print('queue\n')
      ELSIF 19 IN s THEN tty.print('status\n')
      ELSE tty.print('reg\n')
      END;
    ELSE
      IF 23 IN s THEN tty.print('store') ELSE tty.print('load') END;
      IF 22 IN s THEN tty.print(' unsigned') ELSE tty.print(' signed') END;
      IF 21 IN s THEN
        IF 20 IN s THEN tty.print(' double word') ELSE tty.print(' word') END;
      ELSE
        IF 20 IN s THEN tty.print(' half word') ELSE tty.print(' byte') END;
      END;
      IF 19 IN s THEN tty.print(' alternate') END;
      tty.print(' ( '); reg((s*{14..18})>>14);
      IF 13 IN s THEN
        IF 12 IN s THEN s1:=s+{13..31} ELSE s1:=s-{13..31} END;
        tty.print('+ %d )',s1);
      ELSE
        tty.print(' + '); reg(s*{0..4}); tty.print(' )');
      END;
      tty.print(' -> '); reg((s*{25..29})>>25);
      tty.print('\n');
    END;
  ELSIF s*{30,31}={} THEN
    CASE INTEGER(s*{19..22})>>19 OF
      | 0: n:=0; tty.print('and');
      | 1: n:=0; tty.print('or ');
      | 2: n:=0; tty.print('xor');
      | 3: n:=1; tty.print('add');
      | 4: n:=1; tty.print('sub');
      | 5: n:=2; tty.print('shift');
      | 6: n:=3; tty.print('tagged');
      | 7: n:=4;
      | 8: n:=5; tty.print('read');
      | 9: n:=5; tty.print('write');
      |10: n:=6;
      |11: n:=7;
    ELSE
      tty.print('%$8h\n',s); RETURN
    END;
    CASE n OF
      |0:
        IF 24 IN s THEN tty.print(' not') END;
        IF 23 IN s THEN tty.print(' set CC') END;
      |1:
        IF 24 IN s THEN tty.print(' extended') END;
        IF 23 IN s THEN tty.print(' set CC') END;
      |2:
        IF 24 IN s THEN tty.print(' right') ELSE tty.print(' left') END;
        IF 23 IN s THEN tty.print(' arithmetic') ELSE tty.print(' logical') END;
      |3:
        IF 24 IN s THEN tty.print(' sub') ELSE tty.print(' add') END;
        IF 23 IN s THEN tty.print(', trap on overflow') END;
      |4:
        tty.print('???')
      |5:
        IF 24 IN s THEN
          IF 23 IN s THEN tty.print(' trb') ELSE tty.print(' wim') END;
        ELSE
          IF 23 IN s THEN tty.print(' psr') ELSE tty.print(' y') END;
        END;
      |6:
        tty.print('jsr');
        IF 24 IN s THEN
          tty.print(' annual annual')
        ELSIF 23 IN s THEN
          tty.print(' annual')
        END;
      |7:
        IF 23 IN s THEN tty.print('restore') ELSE tty.print('save') END;
    END;
    tty.print('  '); reg((s*{14..18})>>14);
    IF 13 IN s THEN
      IF 12 IN s THEN s1:=s+{13..31} ELSE s1:=s-{13..31} END;
      tty.print(' %d',s1);
    ELSE
      tty.print('  '); reg(s*{0..4});
    END;
    tty.print(' -> '); reg((s*{25..29})>>25);
    tty.print('\n');
  END;
END print_cmd;

END adaDebug.
