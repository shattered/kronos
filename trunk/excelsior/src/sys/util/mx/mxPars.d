DEFINITION MODULE mxPars; (* Ned 06-Dec-87. (c) KRONOS *)

IMPORT SYSTEM;

IMPORT  comp: coolDefs;

VAL gen_opts: BITSET;           -- опции, влияющие на генерацию

VAL
  vers: ARRAY [0.. 63] OF CHAR; -- версия компилятора
  opts: BITSET;                 -- опции по умолчанию
  cpu : INTEGER;                -- по умолчанию

VAL
  name: ARRAY [0..63] OF CHAR;
  unit: INTEGER;                -- вид единицы компиляции
  code: INTEGER;                -- размер чистого кода (в словах)

PROCEDURE compile(text : comp.io_ptr;
                  ini  : comp.INI;
                  exi  : comp.EXI;
                  error: comp.ERROR;
                  print: comp.PRINT;
                  opts : BITSET;
                  cpu  : INTEGER;
                  );

TYPE ONE_MODULE = PROCEDURE (ARRAY OF CHAR);

PROCEDURE get_import(text  : comp.io_ptr;
                     error : comp.ERROR;
                     print : comp.PRINT;
                     import: ONE_MODULE;
                    );
(* Вызывает процедуру import со всеми именами импортируемых
   модулей. Проверяет только синтаксическую правильность текста.
*)

PROCEDURE fault(format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

END mxPars.
