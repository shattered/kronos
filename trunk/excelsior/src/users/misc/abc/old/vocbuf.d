DEFINITION MODULE abcBuf; (* Hady & Leg. 22-Apr-88. (c) KRONOS *)

   (*        Модуль  реализует  буфер  для строк.   *)
   (*   Положенные в буфер в произвольном порядке   *)
   (*   строки выбираются в алфавитном порядке.     *)

(* NOTE --  Procedures return result <0 if some fault takes place *)

PROCEDURE PutString(VAL s: ARRAY OF CHAR): INTEGER;

PROCEDURE GetString(VAR s: ARRAY OF CHAR): INTEGER;

TYPE showProc = PROCEDURE (ARRAY OF CHAR): BOOLEAN;
(* TRUE, returned by -showProc- stops ShowBuffer *)

PROCEDURE ShowBuffer(sh: showProc);
(* Show all buffer by using of -showProc *)


END abcBuf.
