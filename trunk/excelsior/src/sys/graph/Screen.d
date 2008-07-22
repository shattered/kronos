DEFINITION MODULE Screen; (* Leo & nick 26-Mar-90. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  defScreen;

TYPE
  STATUS  = defScreen.STATUS;
  STATE   = defScreen.STATE;
  COLOR   = defScreen.COLOR;
  PALETTE = defScreen.PALETTE;

VAL done: BOOLEAN;
   error: INTEGER;
   state: STATE;

PROCEDURE nop;
PROCEDURE set_ldcx(x: INTEGER);
PROCEDURE set_ldcy(y: INTEGER);
PROCEDURE set_palette(p: PALETTE; from,len: INTEGER);
PROCEDURE refresh (x,y,w,h: INTEGER);
PROCEDURE refreshw(x,y,w,h: INTEGER);

PROCEDURE op(cmd: INTEGER; SEQ args: SYSTEM.WORD);

CONST
   bitmap = defScreen.bitmap;
   pelmap = defScreen.pelmap;
   other  = defScreen.other;

PROCEDURE loophole(kind: INTEGER; VAR ext: SYSTEM.WORD);

PROCEDURE attach(name: ARRAY OF CHAR);
(* attach screen other then $SCR *)

END Screen.
