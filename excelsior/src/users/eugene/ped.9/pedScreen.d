DEFINITION MODULE pedScreen; (* Sem 05-Mar-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;

VAR ScaleX ,ScaleY ,WindowX,WindowY: INTEGER;
    WindowW,WindowE,WindowS,WindowN: INTEGER;

PROCEDURE OpenWindow(b: ADDRESS; scale,x,y: INTEGER);

PROCEDURE SetWindow(scale,x,y: INTEGER);

PROCEDURE CloseWindow;

PROCEDURE Cursor(x1,y1,x2,y2: INTEGER);

PROCEDURE Text(ln: INTEGER; s: ARRAY OF CHAR);

PROCEDURE Drow(x1,y1,x2,y2,s: INTEGER; l: BITSET);

PROCEDURE Delete(x1,y1,x2,y2,s: INTEGER; l: BITSET);

END pedScreen.
