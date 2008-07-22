DEFINITION MODULE teED; (*  02-Jul-91. (c) KRONOS *)

TYPE EDITOR;

CONST type = 1;

TYPE INFO = RECORD
              s: ARRAY [0..63] OF CHAR;
              i: ARRAY [0.. 4] OF INTEGER
            END;

PROCEDURE info   (e: EDITOR; VAR i: INFO);
PROCEDURE restore(x,y,w,h: INTEGER; i: INFO);

PROCEDURE move;
PROCEDURE resize;
PROCEDURE close ;
PROCEDURE switch;

PROCEDURE new(x,y,w,h: INTEGER);

END teED.
