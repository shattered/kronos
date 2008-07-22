DEFINITION MODULE WMUtil;
(* Module for Wordmaker by Andrus Moor *)

FROM SYSTEM            IMPORT WORD;

CONST
  Soft_CR=6; (*^F*)
  ht=9;

TYPE
  InterAct=(abort,continue,ask,escget);
  Int_Type=(none,status,eos,screen);

VAR
  IsAborted: BOOLEAN; (*True if user wants to abort*)
  Interrupt: Int_Type;
  Lines,Cols,MenuLn: CARDINAL;

PROCEDURE DirIn(): CHAR;
PROCEDURE KeyPressed(): BOOLEAN;
PROCEDURE GotoXY( x,y: CARDINAL );
PROCEDURE Remove_Line(): BOOLEAN;
PROCEDURE InsertLine(): BOOLEAN;
PROCEDURE CtrlOut( ch: CHAR );

PROCEDURE Control(  ch: CHAR ): CHAR;
PROCEDURE ComLet( ch: CHAR ): CHAR;
(*Return 7-bit upper case char*)
PROCEDURE Mes_Write( mesno: CARDINAL );
(* Write a message *)
PROCEDURE UnImplemented();
PROCEDURE Message( mesno, fileno: INTEGER; returncode: InterAct );
PROCEDURE YNGet(): BOOLEAN;
PROCEDURE Aborted(): BOOLEAN;
PROCEDURE ReadFN(mesno, fileno: CARDINAL): BOOLEAN;
PROCEDURE WriteMenu();
PROCEDURE WriteText( tno: CARDINAL );
PROCEDURE NormalVideo();
PROCEDURE BlVidOn();
PROCEDURE BlVidOff();
PROCEDURE LowVidOn();
PROCEDURE LowVidOff();
PROCEDURE DROP( w: WORD );
PROCEDURE Go_StatusLine();

END WMUtil.
