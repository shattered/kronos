DEFINITION MODULE mxScan; (* Leo 26-Nov-85. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT  comp: coolDefs;

TYPE String = ARRAY [0..255] OF CHAR;

CONST   -- symbols  !!! значения констант менять нельзя !!!
  equ    = 0;    neq    = 1;    in     = 2;
  lss    = 3;    gtr    = 4;    leq    = 5;    geq    = 6;
  times  = 7;    div    = 8;    mod    = 9;    slash  =10;
  minus  =11;    plus   =12;
  rol    =13;    ror    =14;
  rem    =15;

  if     =16;    for    =17;    case   =18;    return =19;
  repeat =20;    while  =21;    loop   =22;    exit   =23;
  with   =24;

  semic  =25;

  else   =26;    elsif  =27;    until  =28;    end    =29;
  sep    =30;

  ident  =31;

  intval =32;    realval=33;    charval=34;    string =35;
  not    =36;

  bar    =37;    period =38;    colon  =39;    lbr    =40;
  rbr    =41;    lpar   =42;    rpar   =43;    lbrace =44;
  rbrace =45;    coma   =46;    range  =47;    becomes=48;

  by     =50;    do     =51;    of     =52;    or     =53;
  to     =54;    and    =55;    set    =56;    seq    =57;
  val    =58;    var    =59;    code   =60;    from   =61;
  then   =62;    type   =63;    array  =64;    export =65;
  begin  =66;    const  =67;    import =68;
  module =70;    record =71;    forward=72;    pointer=73;
  procedure =74;                qualified     =75;
  definition=76;                implementation=77;
  dynarr    =78;

  lastsymbol=78;

CONST
  Relation  = {equ,neq,gtr,geq,lss,leq,in};
  Multiplop = {times,div,mod,slash,rem,rol,ror};
  StatTerm  = {end,else,elsif,until,sep};

VAR
  sy      : INTEGER;   -- текущий символ
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

---------------------------  ERRORS  --------------------------
                           ----------

PROCEDURE err(N: INTEGER);
(* Выдача строки и сообщения об ошибке. (error) *)

PROCEDURE expc(sy: INTEGER);
(* Сообщение о пропуске символа *)

PROCEDURE err_id(N: INTEGER; id: INTEGER);
(* Сообщение об ошибке связанной с использованием идентификатора *)

PROCEDURE Fault(n: INTEGER; f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
(* Сообщение о фатальной ошибке и прекращение компиляции *)

PROCEDURE io_fault(f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE vis_sym(sy: INTEGER; VAR vis: ARRAY OF CHAR);
(* Визуализирует внутреннее представление символа во внешнее *)


-------------------------  INI & EXI  -------------------------
                         -------------

PROCEDURE Ini(text: comp.io_ptr);
(* Начало работы. делает первый GetSy *)

PROCEDURE Exi;
(* конец  работы. Печатает последнее сообщение об ошибке, *)
(* если таковое имеется.                                  *)

END mxScan.
