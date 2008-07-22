DEFINITION MODULE fExt; (* Max *)

FROM fScan  IMPORT Idname, Types, Mode, PointPS;

PROCEDURE WasRealised():BOOLEAN;
(* Разбираемая подпрограмма была уже реализавана *)

PROCEDURE Forward;
(* Обрабатывает описание FORWARD *)

PROCEDURE PutSubProg(VAR Id:Idname;tp:Types;Re:BOOLEAN):BOOLEAN;
(* Вносит в список FORWARD-описаний подрограмм Id; *)
(* Возвращает TRUE, если подпрограмма уже есть в списке.*)
(* Инициализируется список для дальнейшего заполнения *)
(* спецификаций параметров;Re=TRUE,если Id вносится не как Forward *)

PROCEDURE PutParam(m:Mode;t:Types);
(* Вносит спецификации очередного параметра в список,*)
(* проинициализированный в PutSubProg *)

PROCEDURE ChangeType(tp:Types);
(* Изменяет разбираемой функции тип *)

PROCEDURE SetParList(P:PointPS);
(* Устанавливает список спецификаций параметров для GetParam *)
(* Используется при непервом обращении к подпрограмме *)

PROCEDURE GetParam(VAR m:Mode;VAR t:Types):BOOLEAN;
(* Берет спецификации очередного параметра из *)
(* установленного списка параметров, FALSE , *)
(* если параметров больше нет *)

PROCEDURE SetSubProgList;
(* Устанавливает список подпрограмм на начало *)
(* для их последовательной выборки процедурой ExistSubProg *)

PROCEDURE ExistSubProg(VAR I:Idname; VAR T:Types;
                       VAR P:PointPS; VAR r:BOOLEAN):BOOLEAN;
(* Выбирает очередную подпрограмму. FALSE, если их больше нет *)

PROCEDURE InitForw;

END fExt.
