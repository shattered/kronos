DEFINITION MODULE abcDefs; (* Hady. 24-Apr-88. (c) KRONOS *)

  (*    Модуль поставляет константы     *)
  (*    остальным модулям словаря       *)

CONST           -- S C R E E N   L A Y O U T    --
                --     fo screen driver         --
  INFO = 23;    -- StringNo for Info Line       --
  WRK  = 00;    -- StringNo for Work Line       --
  SCR  = 02;    -- Start of main screen         --
  SCRH = 20;    -- main screen: hight-1         --
  SCRW = 77;    -- main screen: weight-1        --
  LFTM = 02;    -- left margin of main screen   --

CONST -- ERRORS OF MODULES --
  ExistOpened= -100h;
  NotOpened  = -101h;
  NoSuchWord = -102h;
  NoMemory   = -103h;
  EmptyBuf   = -104h;

PROCEDURE VisError(error: INTEGER; VAR s: ARRAY OF CHAR);
(* дополняет строку -s- сообщением об ошибке *)

END abcDefs.
