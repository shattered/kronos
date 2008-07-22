DEFINITION MODULE exTex; (* Ned 18-Nov-87. (c) KRONOS *)

PROCEDURE form(par,l0,c0: INTEGER; VAR l1,c1: INTEGER);
(* форматирует строки l0..l1 в колонки c0..c1
   с абзацем длиной par. Возвращает в l1,c1 координаты
   конца абзаца.
*)

PROCEDURE centre(c0,c1: INTEGER);
(* Центрирует текст в текущей строке в диапазоне c0..c1.
   Не изменяет расположение пробелов внутри текста.
*)

END exTex.
