DEFINITION MODULE fDcl; (* Max *)

TYPE unit=(main, function, subroutine);

VAR global:CARDINAL;  (* Первое свободное глобальное слово *)
    self:unit;        (* Тип текущей единицы компиляции *)
    ParNo:CARDINAL;   (* Число параметров Func, Subr; =0,если self=main *)
    ModIdno:CARDINAL; (* idno для Func и Subr *)

PROCEDURE GiveGW():CARDINAL;
(* Отводит глобальное слово *)

PROCEDURE declare;
(* Разбор всех описаний *)

PROCEDURE InitDcl;

END fDcl.
