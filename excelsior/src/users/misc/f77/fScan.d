DEFINITION MODULE fScan; (* Max *)

FROM SYSTEM IMPORT ADDRESS;

TYPE Symbol=( int, real, log, char, subr, func, prog, impl, comm, data, ext,
              equi, param, fwrd, dim, call, goto, cnt, rtn, stp, rd, wr,asgn,
              pr, then, else, elsif, endif, do, if, frm, end, coma, col, eqv,
              lpar, rpar, EOL, id, const, realconst, stringconst,
              power, slash, times, plus, minus, lt,  le,  eq,  ne,
              ge, gt,  and,  or,  not, false, true, invKW );

     Types =( Int, Real, Logic, Char, Undef );
     Work  =( kw, ass, dcl );
     Mode= ( Var, Func, Subr, LocFunc, Const, Array, Empty, LocVar );
           (* Var - переменная, Array - массив, Func - функция,
              Subr - подпрограмма, LocFunc - функция-формула,
              Const - параметр-константа, Empty - неопределенный
              LocVar- параметр функции-формулы, существует
                      только на время разбора функции     *)
     head      =RECORD lo,hi:CARDINAL; END;
     Descriptor=ARRAY [0..2] OF head;
     D         =POINTER TO Descriptor;
     ParList   =ARRAY [0..5] OF Types;
     PointP    =POINTER TO ParList;
     Filename  =ARRAY [0..79] OF CHAR;
     Idname    =ARRAY [0..15] OF CHAR;
     PointPS   =POINTER TO ParListS;
     ParListS  =RECORD m:Mode; t:Types; next:PointPS; END;

     Info=RECORD
       name     :CARDINAL;(* idno для об'екта *)
       offset   :CARDINAL;(* Var,Array - смещение;лок-ое, если param = TRUE,*)
                          (* Func,Subr - номер модуля, LocalFunc -номер пр.,*)
                          (* Const - значение, LocVar - смещение по L-регис.*)
       dimension:CARDINAL;(* Array - размерность;LocFunc - число параметров *)
       param    :BOOLEAN; (* TRUE, если об'ект параметр у Func, Subr *)
       abs      :CARDINAL;(* Array -(размер-1) для check bound *)
       desc     :D;       (* Array *)
       partypes :PointP;  (* LocFunc - указатель на список типов параметров *)
       parlist  :PointPS; (* Func,Subr - список спецификаций параметров *)
     END;

CONST NoIds=512;

VAR Default: ARRAY [0c..377c] OF CHAR;(* типизация по умолчанию : *)
    Ival: INTEGER;               (* Значение при sy=const *)
    Rval: REAL;                  (* Значение при sy=realconst *)
    Sval: ARRAY [0..79] OF CHAR; (* Значение при sy=stringconst *)
    Label: CARDINAL;             (* >0 если метка в строке есть, =0 иначе *)
    sy: Symbol;                  (* Текущий Символ *)
    ErrorCo:CARDINAL;            (* Счетчик ошибок *)
    BuffIsEmpty:BOOLEAN;         (* была взята последняя строка из программы*)
    idno: CARDINAL;              (* Принимает уникальное значение для каждогo
                                    Идентификатора (sy=id) *)

PROCEDURE GetSy; (* Берет следующую лексему *)

PROCEDURE NewLine;
(* Берет новую строку, если ее нет то Fault(12);
   если в строке есть метка, то возвращает Label=Метке.*)

PROCEDURE MarkPos; PROCEDURE MarkPos1; (*При разборе выражений*)
(* Маркируют позицию в строке для возврата при отходе *)

PROCEDURE BackUp;  PROCEDURE BackUp1;  (*При разборе выражений*)
(* Отход в маркированную позицию *)

PROCEDURE SetWork(Type:Work);
(* Устанавливает режим работы Type:*)

PROCEDURE Give(VAR p:ADDRESS;sz:CARDINAL);
PROCEDURE Free(VAR p:ADDRESS;sz:CARDINAL);
PROCEDURE ReleaseHeap;

PROCEDURE SetMode(d:CARDINAL;M:Mode);
(* Устанавливает идентификатору d вид M *)

PROCEDURE GetMode(d:CARDINAL):Mode;
(* Возвращает вид идентификатора d *)

PROCEDURE GenObj(I:Info);
(* по I генерирует новый об'ект *)

PROCEDURE Unpack(VAR I:Info);
(* по адресу распаковывает об'ект I *)

PROCEDURE DelObj(d:CARDINAL);
(* Удаляет об'ект *)

PROCEDURE SetType(d:CARDINAL;T:Types);
(* Устанавливает идентификатору d тип T *)

PROCEDURE GetType(d:CARDINAL):Types;
(* Возвращает тип идентификатора d *)

PROCEDURE SaveO(i:CARDINAL);
(* Запоминает об'ект для последующего восстановления *)

PROCEDURE RestoreO;
(* Восстанавливает все запомненные об'еткы *)

PROCEDURE SetDefaultTp(d:CARDINAL);
(* Устанавливает тип об'екта по умочанию *)
(*
PROCEDURE VisIdTable;
(* Визуализирует таблицу идентификаторов *)
*)
PROCEDURE WhatSy?(idno:CARDINAL):Symbol;
(* Возвращает sy  для idno *)

PROCEDURE StrId(Id:Idname):CARDINAL;
(* Вносит Id в таблицу идентификаторов; для fDcl *)

PROCEDURE IdStr(d:CARDINAL;VAR I:Idname);
(* Для fGen; выдает по idno имя об'екта *)

PROCEDURE DelId(d:CARDINAL);
(* Удаляет идентификатор из таблицы *)

PROCEDURE Fault(n:CARDINAL);
(* Фатальная ошибка *)

PROCEDURE FAULT;
(* Прекращение компиляции вследствие невозможности *)
(* свертки дерева разбора выражения при ошибке *)

PROCEDURE Error(n:CARDINAL);
(* Сообщение об ошибке  *)

PROCEDURE Expected(Sy:Symbol);
(* Сообщение o Sy, который должен был быть *)

PROCEDURE ErrorId(n:CARDINAL);
(* Сообщение об ошибке, связанное с идентификатором *)

PROCEDURE Warning(n:CARDINAL);
PROCEDURE GetLetter(VAR c:CHAR):BOOLEAN;
PROCEDURE InitScan;
PROCEDURE InitZeroScan;

END fScan.
