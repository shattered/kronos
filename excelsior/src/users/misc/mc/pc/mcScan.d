DEFINITION MODULE mcScan; (* Leo 26-Nov-85. (c) KRONOS *)
                          (* Ned 11-Aug-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT comp: coolDefs;

IMPORT  ers: coErrors;

TYPE String = ARRAY [0..255] OF CHAR;

TYPE
  Symbol = (

    ident, intval, realval, charval, string,

    equ,  neq,  lss,  gtr,  leq, geq, times, slash,
    minus, plus, rol, ror, sep, semic, bar, period,
    colon,  lbr,  rbr,  lpar, rpar, lbrace, rbrace,
    coma, becomes, range,

    and,  array,  begin,  by,  case,  code,  const,
    definition,  div, do, dynarr, else, elsif, end,
    exit,   export,   for,   forward,   from,   if,
    implementation,  import, in, loop, mod, module,
    not, of, or, packed, pointer,  procedure, qualified,
    record,  rem,  repeat,  return,  to,  seq, set,
    until, val, var, while, with, then, type

  );


VAR
  sy      : Symbol;   -- текущий символ
  opts    : BITSET;
  fault   : BOOLEAN;  -- была фатальная ошибка

VAL
  Id      : INTEGER;  -- текущий идент
  cVal    : INTEGER;  -- значение константы
  sVal    : String;   -- значение строки
  sLen    : INTEGER;  -- длина строки
  line    : INTEGER;  -- счетчик строк
  col     : INTEGER;  -- счетчик колонок
  noErrors: INTEGER;  -- счетчик ошибок
  add_stk : INTEGER;

------------------------  IDENTIFIERS  ------------------------
                        ---------------

CONST
  noIdents= 2048; -- размер хэш таблицы
  DmId    =    0; -- dummy идент

PROCEDURE str_id(str: ARRAY OF CHAR): INTEGER;
(* Перекодировка строки в идент. *)

PROCEDURE id_str(id: INTEGER; VAR str: ARRAY OF CHAR);
(* Перекодировка идентификатора в строку. *)

---------------------------  SCANNER  -------------------------
                           ----------

PROCEDURE GetSy;
PROCEDURE GetChar(): CHAR;

---------------------------  ERRORS  --------------------------
                           ----------

PROCEDURE err(n: ers.T);
(* Выдача строки и сообщения об ошибке. (error) *)

PROCEDURE expc(sy: Symbol);
(* Сообщение о пропуске символа *)

PROCEDURE err_id(n: ers.T; id: INTEGER);
(* Сообщение об ошибке связанной с использованием идентификатора *)

PROCEDURE Err(n: ers.T; f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE Fault(n: ers.T; f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
(* Сообщение о фатальной ошибке и прекращение компиляции *)

PROCEDURE io_fault(f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE vis_sym(sy: Symbol; VAR vis: ARRAY OF CHAR);
(* Визуализирует внутреннее представление символа во внешнее *)

-------------------------  INI & EXI  -------------------------
                         -------------

PROCEDURE Ini(text: comp.io_ptr);
(* Начало работы. делает первый GetSy *)

PROCEDURE Exi;
(* конец  работы. Печатает последнее сообщение об ошибке, *)
(* если таковое имеется.                                  *)

END mcScan.
