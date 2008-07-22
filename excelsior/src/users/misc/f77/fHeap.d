DEFINITION MODULE fHeap; (* ww 1-Feb-87 *)

FROM SYSTEM IMPORT ADDRESS;

PROCEDURE Allocate(VAR A:ADDRESS; Sz:CARDINAL);
(* Выдает из кучи место в Sz слов *)

PROCEDURE DeAllocate(VAR A:ADDRESS; Sz:CARDINAL);
(* Освобождение взятого места *)

PROCEDURE SetAbort(B:BOOLEAN);
(* Установка режима поведения кучи, при нехватке места *)
(* B=TRUE абортирование задачи, иначе возврат NILа     *)

PROCEDURE SetSpase(C:CARDINAL);
(* Утановка минимального зазора между P-стеком и кучей *)

PROCEDURE HeapSize():CARDINAL;
(* Выдает примерный размер памяти оставшейся в куче *)

END fHeap.
