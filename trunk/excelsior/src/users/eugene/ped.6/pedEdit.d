DEFINITION MODULE pedEdit; (* 11-Mar-87. (c) KRONOS *)

(* Модуль    поставляет   процедуры   редактирования   строки.
   Ввод/вывод  осуществляются  через  модуль Terminal, поэтому
   все процедуры работают только с экраном!
*)

PROCEDURE ReadString(Ask: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
(* Выдает  на  экран  приглашение -Ask- и считывает с экрана в
   строку -s-.
*)

PROCEDURE Confirm   (Ask: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
(* То  же, что и ReadString, но выдает после введения с экрана
   содержимое строки -s- для подтверждения.
*)

TYPE Action=(term,validch,validkey,invalid,bell);
     How=(dummy,old,show,confirm);
     ValidProc=PROCEDURE (VAR CHAR, INTEGER, VAR ARRAY OF CHAR): Action;
     (*        PROCEDURE (VAR pushbutton,pos_in_string,string): Action  *)

PROCEDURE ReadValid(VAR s: ARRAY OF CHAR; valid: ValidProc; how: How);
(*  -valid- должен возвращать:
     term      для прекращения чтения;
     validch   для допустимого символа (ReadValid кладет его в
               -s-);
     validkey  для   допустимого   ключа  (ReadValid  пытается
               интерпретировать    его    как    редактирующее
               действие;
     invalid   для  недопустимого символа или ключа (ReadValid
               игнорирует его);
     bell      для  недопустимого символа или ключа (ReadValid
               игнорирует его и возбуждает звуковой сигнал).
   -valid- может изменять содержимое переменной -pushbutton- -
   первого   параметра   этой   процедуры,   как   это  делает
   стандартная процедура -valid- , которая читает следующий за
   ^T (DC4) байт и вставляет его в строку.

   pos_in_string  может  использоваться для отсекания строки с
   той позиции, где стоял курсор.
   ReadValid   возвращает   всю  строку,  включая  завершающие
   пробелы.
   Семантика констант типа How:
   dummy не показывает содержимое строки;
   show показывает содержимое строки и устанавливает курсор на
   ее начало;
   confirm показывает содержимое строки и устанавливает курсор
   на ее конец.
*)

PROCEDURE StandardValid(VAR pb: CHAR; ps: INTEGER; VAR s: ARRAY OF CHAR
                       ): Action;

TYPE readproc=PROCEDURE(): CHAR;

PROCEDURE SetRead(Read: readproc);

END pedEdit.
