DEFINITION MODULE CodeFiles; (* Ned 22-Dec-86. (c) KRONOS *)

(* Модуль поставляет процедуры работы с кодофайлами. *)

FROM SYSTEM    IMPORT   WORD;

TYPE
  Attrs = RECORD (* Все размеры в СЛОВАХ *)
                     CPU: INTEGER;
                Language: ARRAY [0..31] OF CHAR;
                 DefTime: INTEGER; (* Для программных модулей совпадает
                                      с ImpTime                         *)
                 ImpTime: INTEGER;
                  ProcCo: INTEGER;
                  ExtsCo: INTEGER; (* = размер LDFT *)
                CodeSize: INTEGER;
              ConstsSize: INTEGER; (* размер строкового пула *)
             GlobalsSize: INTEGER; (* без размера LDFT *)
            MinProcStack: INTEGER;
            OptimalStack: INTEGER;
          END;


PROCEDURE CreateCode(modname: ARRAY OF CHAR; error: PROC);
(* Создает кодофайл *)

PROCEDURE WriteInfo(VAR a: Attrs);
(* Записывает атрибуты в кодофайл *)

PROCEDURE WriteExt(no: INTEGER; nm: ARRAY OF CHAR);
(* Записывает имя внешнего модуля с номером -no- *)

PROCEDURE WriteProcTable(VAR a: Attrs; VAR pTab: ARRAY OF INTEGER);
(* Записывает процедурную таблицу *)

PROCEDURE WriteConsts(a: Attrs; VAR consts: ARRAY OF WORD);
(* Записывает строковый пул *)

PROCEDURE WriteCode(VAR a: Attrs; VAR Code: ARRAY OF WORD);
(* Записывает код в кодофайл *)

PROCEDURE CloseCode;
(* Закрывает кодофайл *)

END CodeFiles.
