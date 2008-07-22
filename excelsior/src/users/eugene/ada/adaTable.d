DEFINITION MODULE adaTable; (* 05-Apr-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, WORD;
FROM adaLex     IMPORT  lex_elem;

TYPE
  class=(cl_enumeration,cl_integer,cl_float,
         cl_array,cl_record,cl_access,cl_task,
         cl_type,cl_subtype,cl_var,cl_func,
         cl_prm,cl_field,cl_pack,cl_use,cl_operator);
  class_set=SET OF class;
CONST
  type_class   =class_set{cl_enumeration,cl_integer,cl_float,cl_array,
                          cl_record,cl_access,cl_task};
  discrete_type=class_set{cl_enumeration,cl_integer};
  scalar_type  =class_set{cl_enumeration,cl_integer,cl_float};
  numeric_type =class_set{cl_integer,cl_float};
  library_class=class_set{cl_func,cl_pack};
TYPE
  item       = POINTER TO item_rec;
  name       = POINTER TO name_rec;
  item_block = ARRAY [0..31] OF item;
  item_rec=RECORD
    nm        : name;
    nm_fwd    : item;    -- список наложений имени (private)
    host      : item;    -- declaration region
    host_nxt  : item;    -- next in declaration region
    host_cnt  : INTEGER;
    lex_pos   : INTEGER;
    CASE md : class OF
      |cl_operator:
        op_lex      : lex_elem;
        op_dw       : item_block;
        op_dw_no    : INTEGER;
        op_val0     : INTEGER;
        op_list     : item;
      |cl_enumeration:
        en_bits     : INTEGER;
        en_signed   : BOOLEAN;
        en_universal: BOOLEAN;
      |cl_integer:
        in_bits     : INTEGER;
        in_signed   : BOOLEAN;
        in_universal: BOOLEAN;
      |cl_float:
        fl_universal: BOOLEAN;
        fl_digits   : INTEGER;
        fl_delta    : INTEGER;
        fl_bits     : INTEGER;
      |cl_array:
        ar_elemtp   : item;     -- must be subtype
        ar_index    : item_block;     -- must be subtype
        ar_index_no : INTEGER;
        ar_elembits : INTEGER;
      |cl_record:
        re_fields   : item;     -- список полей (must be field)
        re_descr    : item_block; -- массив дискриминантов
        re_descr_no : INTEGER;
        re_bits     : INTEGER;
      |cl_field:
        fi_type     : item;     -- must be subtype
        fi_nxt      : item;     -- must be field
        fi_bits     : INTEGER;
        fi_offs     : INTEGER;
        fi_descr    : INTEGER;
        fi_case     : INTEGER;
      |cl_access:
        ac_type     : item;     -- must be subtype
        ac_universal: BOOLEAN;
      |cl_task:
      |cl_var:
        va_type     : item;     -- must be subtype
        va_const    : BOOLEAN;
        va_mem      : BOOLEAN;
        -- переменная обязательно должна лежать в памяти (не в регистре)
        va_sz       : INTEGER;
        -- минимальный размер памяти для хранения переменной (1 проход)
        va_rl       : INTEGER;
        -- метод доступа
        --  1  константа компиляции, значение в va_val0
        --  2  адрес переменной = G + va_val0
        --  3  адрес переменной = G + word(G + va_val0)
        --  4  переменная в регистре va_val1
        --  5  адрес переменной = L + va_val0, адрес в регистре va_val1
        --  6  адрес переменной = L + va_val0
        --  7  константа компиляции, значение в va_val2^,
        --       va_val1 = SIZE(va_val2^)
        va_val0     : INTEGER;
        va_val1     : INTEGER;
        va_val2     : ADDRESS;
      |cl_func:
        fu_import   : BOOLEAN;
        fu_type     : item;     -- if NIL then proc else must be subtype
        fu_list     : item;     -- список объектов
        fu_prm      : item;     -- must be prm
        fu_prm_no   : INTEGER;
        fu_var_no   : INTEGER;  -- размер статики в байтах (2 проход)
        fu_reg_no   : INTEGER;
        fu_inline   : BOOLEAN;
        fu_rl       : INTEGER;  -- метод вызова
        -- 2  константа перечислимого типа, val0 - номер, val1 - значение
        -- 3  адрес процедуры = G + word(G + fu_val0),
        --    val1 = word(G + fu_val0)  (val1 - 3 проход)
        -- 4  адрес процедуры = G + fu_val0, (val0 - 3 порход)
        fu_val0     : INTEGER;
        fu_val1     : INTEGER;
        fu_nxt      : item;     -- используется локально
      |cl_prm:
        pr_type     : item;     -- must be subtype
        pr_nxt      : item;     -- must be cl_prm
        pr_in       : BOOLEAN;
        pr_out      : BOOLEAN;
        pr_bits     : INTEGER;
        pr_rl       : INTEGER;
        --  1  значение параметра в регистре pr_val0
        --  2  адрес параметра в регистре pr_val0
        pr_val0     : INTEGER;
      |cl_pack:
        pa_import   : BOOLEAN;
        pa_ref      : BOOLEAN;
        pa_list     : item;
        pa_priv     : item;
        pa_body     : item;
        pa_use      : item;
        pa_uni_int  : item;
        pa_uni_flo  : item;
        pa_uni_enu  : item;
        pa_uni_acc  : item;
        pa_var_no   : INTEGER; -- размер области глоб. переменных
      |cl_use:
        us_pack     : item;     -- must be cl_pack
        us_nxt      : item;     -- must be cl_use
      |cl_subtype:
        su_type     : item;     -- must be type
        su_first    : INTEGER;
        su_last     : INTEGER;
        -- для массива su_last содержит количество индексов
        su_descr    : item_block;
        -- для массива su_descr содержит ограничения индекса
        su_private  : BOOLEAN;
        su_limited  : BOOLEAN;
      |cl_type:
        ty_type     : item;    -- must be in type_class
    END;
  END;
  name_rec=RECORD
    bck  : name;
    fwd  : name;
    obj  : item;
    len  : INTEGER;
    CASE : BOOLEAN OF
      |FALSE: val  : ARRAY [0.. 63] OF INTEGER;
      |TRUE : str  : ARRAY [0..255] OF CHAR;
    END;
  END;

PROCEDURE find(): name; -- name value in Lex.val.str
PROCEDURE find_nm(s: ARRAY OF CHAR): name;
PROCEDURE def_name(): name; -- name value in Lex.val.str
PROCEDURE def_item(c: class): item;
PROCEDURE tie_item(n: name; i: item);
PROCEDURE clear_table;
PROCEDURE write_lib(i: item);
PROCEDURE read_lib(lib_name: ARRAY OF CHAR): item;
PROCEDURE import(nm: name; visible: BOOLEAN): item;

PROCEDURE new_item(c: class; host: item; VAR last: item);
-- создает объект класса  c
-- в области описаний объекта host

PROCEDURE new_name_item(c: class; host: item; VAR last: item);
-- создает объект класса  c
-- с именем val.str
-- в области описаний объекта host
-- проверяет отсутствие повторного описания

PROCEDURE find_vis(nm: name; from: item): item;
-- ищет объект, с именем nm в точке from

PROCEDURE find_extended(from: item; VAR nm: name): item;
-- разбирает расширенное имя объекта

END adaTable.
