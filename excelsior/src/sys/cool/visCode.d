DEFINITION MODULE visCode; (* Ned 28-Sep-89. (c) KRONOS *)

(* Визуализация кода и системы команд *)

IMPORT  SYSTEM;

VAL done: BOOLEAN;
   error: INTEGER;

TYPE
  INTs     = DYNARR OF INTEGER;
  GLOBAL   = RECORD ofs,size: INTEGER END;
  GLOBALs  = DYNARR OF GLOBAL;
  ext_ptr  = POINTER TO
               RECORD
                 time: INTEGER;
                 name: ARRAY [0..31] OF CHAR;
               END;
  EXTs     = DYNARR OF ext_ptr;

  code_ptr = POINTER TO code_rec;
  code_rec = RECORD
               vers: INTEGER;   -- версия кода
           compiler: STRING;    -- название компилятора
           def_time: INTEGER;   -- время компиляции определения
           imp_time: INTEGER;   -- время компиляции реализации
             no_glo: INTEGER;   -- число глобалов
          add_stack: INTEGER;
          min_stack: INTEGER;
                tag: INTEGER;

               name: STRING;
            strings: STRING;    -- строковый и структурный пул
            structs: INTs;      -- строковый и структурный пул
           proc_tab: INTs;      -- процедурная таблица
               code: STRING;    -- команды
          multi_glo: GLOBALs;   -- мульти-глобалы
               exts: EXTs;      -- внешние модули
             END;


PROCEDURE connect(VAR code: code_ptr;
                  VAR base: ARRAY OF SYSTEM.WORD);
(* Распаковка атрибутов и разделов кода *)

PROCEDURE disconnect(VAR code: code_ptr);
(* Освобождает память, занятую процедурой connect *)

-------------------------  COMMANDs  ---------------------------
                         ------------

VAL cmd_len: ARRAY [0..255] OF CHAR; -- длины команд

PROCEDURE vis_command(n: INTEGER; VAR s: ARRAY OF CHAR);
(* Выдает мнемонику команды *)

(***************************************************************

PROCEDURE connect(
-----------------
                  VAR code: code_ptr;
                  VAR base: ARRAY OF SYSTEM.WORD);

     base   определяет  местонахождение  и  размер  кодофайла  в
памяти.  Процедура  распаковывает  атрибуты кода и устанавливает
указатели (дин. массивы) на части кода.

ВНИМАНИЕ:

     Память  занятую  кодом нельзя освобождать до конца работы с
кодом.

ОШИБКИ:
        no_memory - не хватило памяти;
        ill_vers  - некорректная версия кода.

PROCEDURE disconnect(VAR code: code_ptr);
------------------
     Освобождает  не  только  дескриптор  кода,  но  и некоторую
дополнительную память, которую пришлось занять при распаковке.

PROCEDURE vis_command(n: INTEGER; VAR s: ARRAY OF CHAR);
---------------------

     Выдает 6-символьную мнемонику команды.

***************************************************************)

END visCode.
