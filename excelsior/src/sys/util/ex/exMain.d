DEFINITION MODULE exMain; (* Leo  05-Jun-87. (c) KRONOS *)


PROCEDURE SETUP;

PROCEDURE SetName(s: ARRAY OF CHAR);

PROCEDURE GetName(VAR s: ARRAY OF CHAR);

PROCEDURE GetKey(): CHAR;

PROCEDURE RefreshScreen(mem: BOOLEAN);
PROCEDURE SilentScreen;

PROCEDURE Left(left: CHAR);

PROCEDURE Right(right: CHAR);

PROCEDURE Rtab(rtab: CHAR);

PROCEDURE Ltab(ltab: CHAR);

PROCEDURE Del(del: CHAR);

PROCEDURE Up(up: CHAR);

PROCEDURE Dw(dw: CHAR);

PROCEDURE DelLn(delln: CHAR; wrap: BOOLEAN);

PROCEDURE InsLn(insln: CHAR; wrap: BOOLEAN);

PROCEDURE DupLn(dupln: CHAR);

PROCEDURE DupOver;

PROCEDURE SwapUp(upswp: CHAR);

PROCEDURE SwapDw(dwswp: CHAR);

PROCEDURE PullLeft;

PROCEDURE PullRight;

PROCEDURE PageUp(pg: CHAR);

PROCEDURE PageDw(pg: CHAR);

PROCEDURE CarriageReturn(cr: CHAR);

PROCEDURE LineFeed(lf: CHAR);

PROCEDURE NewLine(newln: CHAR);

PROCEDURE ClearTail;

PROCEDURE ValidChar(k: CHAR);

PROCEDURE InsCh(ch: CHAR);

PROCEDURE DelCh;

PROCEDURE DelWord0;
PROCEDURE RubWord0;
PROCEDURE WordLeft0;
PROCEDURE WordRight0;

PROCEDURE DelWord1;
PROCEDURE RubWord1;
PROCEDURE WordLeft1;
PROCEDURE WordRight1;

PROCEDURE InsRep(mode: INTEGER);
(* 0 -- togle  +1 -- on -1 off *)

PROCEDURE FileTop;
PROCEDURE FileBottom;

PROCEDURE LineLeft;
PROCEDURE LineRight;

PROCEDURE BeginMacro(gold: BOOLEAN);
PROCEDURE EndMacro(gold: BOOLEAN; delkeys: INTEGER);
PROCEDURE PopMacro(gold: BOOLEAN; k: CHAR);

PROCEDURE JumpTo(line,col: INTEGER);

PROCEDURE Where?(VAR line,col: INTEGER);

PROCEDURE JumpToPattern(pat: ARRAY OF CHAR; top?,inreg?,rect?: BOOLEAN);
PROCEDURE Replace  (pat,rep: ARRAY OF CHAR; top?,inreg?,rect?: BOOLEAN);

PROCEDURE MarkBegin; (* frame *)
PROCEDURE MarkEnd;   (* frame *)

PROCEDURE MarkLine;  (* frame *)

PROCEDURE JumpBegin; (* frame *)
PROCEDURE JumpEnd;   (* frame *)

PROCEDURE GetBegin(VAR line,col: INTEGER);
PROCEDURE GetEnd  (VAR line,col: INTEGER);

PROCEDURE frame?(VAR l0,c0,l1,c1: INTEGER);
PROCEDURE ResetFrame;

PROCEDURE DelFrame(rect: BOOLEAN);
PROCEDURE PutFrame(rect: BOOLEAN);
PROCEDURE EraFrame(rect: BOOLEAN);
PROCEDURE InsFrame(rect,vert: BOOLEAN);
PROCEDURE MovFrame(rect,vert: BOOLEAN);

PROCEDURE FmtFrame(rect: BOOLEAN);
PROCEDURE Centre  (rect: BOOLEAN);
PROCEDURE FmtPara;

PROCEDURE setlmargin;
PROCEDURE setrmargin;
PROCEDURE setfmargin;
(* Установка границ для форматирования и центрирования.
   В текущей позиции.
*)

PROCEDURE inform_on(str: ARRAY OF CHAR);
PROCEDURE inform_off;

PROCEDURE EmitTime;

PROCEDURE FINISH;

PROCEDURE Peep;

PROCEDURE SwapOld;

PROCEDURE UnDel;

PROCEDURE RestoreLine;

PROCEDURE UpdateInfo;

PROCEDURE break_mode(on: BOOLEAN);
PROCEDURE alarm_mode(on: BOOLEAN);

END exMain.
