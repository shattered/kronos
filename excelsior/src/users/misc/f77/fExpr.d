DEFINITION MODULE fExpr; (* Max *)

FROM fScan  IMPORT Types, Descriptor;
FROM SYSTEM IMPORT WORD;


PROCEDURE Expr(VAR tp: Types; VAR value: WORD): BOOLEAN;
(* Если выражение константное, то возвращает TRUE; *)
(* value,tp - значение и тип константного выр-я.   *)
(* В противном случае возвращает FALSE, генерирует *)
(* код для выражения, возвращая его тип.           *)

PROCEDURE LoadInd(d: Descriptor; dim,abs: INTEGER): BOOLEAN;
(* На Е-стеке база массива; грузит линеаризованный *)
(* индекс массива, TRUE, если все нормально *)

PROCEDURE SetFN(fn: INTEGER);
(* Устанавливает имя для разбираемой функции-формулы F: *)
(* fn=idno(F)-разбор есть , иначе fn=100000 *)

PROCEDURE ExtCall;
(* Генерация внешнего вызова *)

END fExpr.
