DEFINITION MODULE abcTerm; (* Hady. 29-Apr-88. (c) KRONOS *)

  (* Модуль поддерживает экран;                     *)
  (* Жесткие строки -- рабочая строка (верхняя), с  *)
  (*                   возможностью редактирования  *)
  (*                   и информационная - нижняя;   *)

CONST (* modes of finish editing *)
    update = 1; -- To update string before RETURN
    normal = 2; -- To return current string;

VAR str_work: ARRAY [0..255] OF CHAR;       (* editing string *)
         pos: INTEGER;   (* position for next char insertion  *)
        char: CHAR;                       (* last entered key *)
       FINAL: INTEGER;   (* when >0 marks to terminate editing*)
     REFRESH: BOOLEAN;   (* when FALSE stops RefreshInfo & wrk*)

PROCEDURE StandRead(): CHAR;
PROCEDURE StandReaction(ch: CHAR);

PROCEDURE InitScreen;
PROCEDURE RefreshScreen;
PROCEDURE RefreshInfo;

PROCEDURE UP; PROCEDURE DW;  (* перемещения маркера строки *)

(* Работа с текущими режимами экрана *)
PROCEDURE Info? (): BOOLEAN;
PROCEDURE Bell? (): BOOLEAN;
PROCEDURE latin?(): BOOLEAN;
PROCEDURE Roll? (): BOOLEAN;
PROCEDURE Roll! (on_off: BOOLEAN);
PROCEDURE Bell! (on_off: BOOLEAN);
PROCEDURE latin!(on_off: BOOLEAN);
PROCEDURE Info! (on_off: BOOLEAN);
PROCEDURE File!(s: ARRAY OF CHAR);
PROCEDURE Write!(on_off: BOOLEAN);

(****** External work on workLine support ******)

PROCEDURE SaveWrk();     (* push current state to stack   *)
PROCEDURE RefreshWrk();  (* refresh work string on screen *)
PROCEDURE RestoreWrk();  (* pop state from stack          *)

PROCEDURE WriteMess(VAL s: ARRAY OF CHAR);

PROCEDURE WriteStr(s: ARRAY OF CHAR);       -- NOL ROLLS SCREEN
PROCEDURE ShowString(VAL s: ARRAY OF CHAR); --     ROLLS SCREEN
PROCEDURE MarkString(c: CHAR);

PROCEDURE GetLine(VAR s: ARRAY OF CHAR);
(* returns chosed  string  on  screen *)

PROCEDURE Bell;

TYPE Way = (old, new, conf);
PROCEDURE ReadWork(pmt: ARRAY OF CHAR;
                VAR st: ARRAY OF CHAR; sp: Way);

TYPE keyProc = PROCEDURE();
PROCEDURE SetReaction(key: CHAR; reactor: keyProc);

TYPE strProc = PROCEDURE (VAL ARRAY OF CHAR);
PROCEDURE SaveScreen(ws: strProc);

TYPE getString = PROCEDURE (VAR ARRAY OF CHAR): INTEGER;
PROCEDURE resScreen(gs: getString): INTEGER;

END abcTerm.
