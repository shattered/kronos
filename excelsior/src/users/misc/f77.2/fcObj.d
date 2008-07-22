DEFINITION MODULE fcObj; (* 03-Nov-88.  *)

FROM SYSTEM   IMPORT  ADDRESS;
FROM fcDefs   IMPORT  NoIds;

 TYPE
     Types =( Int, Real, Double, Complex, Logic, Char, Holl, Undef );
     Mode= ( Proc, Var, xVar, Const, Array, Empty, LocVar );
           (* Proc - процедура,
              Var - переменная,
              xVar - вид переменной еще не определен,
              Array - массив,
              Const - параметр - константа,
              Empty - неопределенный,
              LocVar- параметр функции-формулы, существует
                      только на время разбора функции     *)
     Class= ( Func, Subr, Intr, Ext, Param, Local, Global  );
     Info=RECORD
       name     :INTEGER;(* idno для об'екта *)
       darea    :INTEGER;
       lenel    :INTEGER;
       cl       :Class;
       bits     :BITSET;
       offset   :INTEGER;
       commP    :INTEGER;
       equiP    :INTEGER;
       dim      :INTEGER;
       desc     :ADDRESS; (* Array descriptor *)
       farray   :BOOLEAN;
     END;
     PchImpl = POINTER TO rimpl;
     rimpl = RECORD
            len : INTEGER;
            c1,c2 : CHAR;
            next  : PchImpl;
            END;
     Idname = ARRAY [0..15] OF CHAR;

VAR Default: ARRAY [0c..377c] OF CHAR;(* типизация по умолчанию : *)

    idno: INTEGER;         (* Принимает уникальное значение для каждогo
                                    Идентификатора (sy=id) *)
    Ident: Idname;          (* Текущий идентификатор  *)

    Pimpl: PchImpl;

    Id:    ARRAY [0..NoIds] OF ADDRESS; (* Таблица идентификаторов *)

CONST databit=0; tpbit=1; indbit=2; subrbit=3; funcbit=4;


PROCEDURE InitObjects;

PROCEDURE SetMode(d:INTEGER;M:Mode);
(* Устанавливает идентификатору d вид M *)

PROCEDURE GetMode(d:INTEGER):Mode;
(* Возвращает вид идентификатора d *)

PROCEDURE GenObj(I:Info);
(* по I генерирует новый об'ект *)

PROCEDURE Unpack(VAR I:Info);
(* по адресу распаковывает об'ект I *)

PROCEDURE DelObj(d:INTEGER);
(* Удаляет об'ект *)

PROCEDURE GenLocal(VAR I:Info);

PROCEDURE Pack(I:Info);

PROCEDURE SetType(d:INTEGER;T:Types);
(* Устанавливает идентификатору d тип T *)

PROCEDURE GetType(d:INTEGER):Types;
(* Возвращает тип идентификатора d *)

PROCEDURE SaveO(i:INTEGER);
(* Запоминает об'ект для последующего восстановления *)

PROCEDURE RestoreO;
(* Восстанавливает все запомненные об'екты *)

PROCEDURE GetDefType(c:CHAR;VAR tp:Types;VAR len:INTEGER);
(* возвращает тип и длину об'екта по умолчанию *)

PROCEDURE SetImplType;

PROCEDURE VisIdTable;
(* Визуализирует таблицу идентификаторов *)

PROCEDURE StrId(Id:Idname):INTEGER;
(* Вносит Id в таблицу идентификаторов;  *)

PROCEDURE IdStr(d:INTEGER;VAR I:Idname);
(*  выдает по idno имя об'екта *)

PROCEDURE DelId(d:INTEGER);
(* Удаляет идентификатор из таблицы *)

END fcObj.
