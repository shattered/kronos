DEFINITION MODULE pedScreen; (* Sem 05-Mar-87. (c) KRONOS *)

FROM Model      IMPORT  Object, Objects;

VAR ScaleX ,ScaleY ,WindowX,WindowY: INTEGER;
    WindowW,WindowE,WindowS,WindowN: INTEGER;

PROCEDURE OpenWindow(o: Object; scale,x,y: INTEGER);

PROCEDURE SetWindow(scale,x,y: INTEGER);

PROCEDURE CloseWindow;

PROCEDURE Cursor(x1,y1,x2,y2: INTEGER);

PROCEDURE Text(ln: INTEGER; s: ARRAY OF CHAR);

PROCEDURE Drow;

PROCEDURE Delete;

END pedScreen.
