DEFINITION MODULE pedScreen; (* Sem 05-Mar-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM Model      IMPORT  Object, Objects;
FROM pedEditor  IMPORT  Sheet;
IMPORT  wnd: Windows;

VAR stat: wnd.window;

PROCEDURE OpenWindow (sh: Sheet; scale,x,y: INTEGER);

PROCEDURE SetWindow  (sh: Sheet; scale,x,y: INTEGER);

PROCEDURE HardSetWindow(sh: Sheet; scale,x,y: INTEGER);

PROCEDURE CloseWindow(sh: Sheet);

PROCEDURE Cursor     (sh: Sheet; x1,y1,x2,y2: INTEGER);

PROCEDURE Cursord    (sh: Sheet; x1,y1,x2,y2: INTEGER);

PROCEDURE Text       (sh: Sheet; ln: INTEGER; fmt: ARRAY OF CHAR; SEQ a: WORD);

PROCEDURE text       (sh: Sheet; cl,ln,col: INTEGER;
                                 s: ARRAY OF CHAR; wt: BOOLEAN);

PROCEDURE Drow       (sh: Sheet);

PROCEDURE Delete     (sh: Sheet);

PROCEDURE UpdSig     (sh: Sheet);

PROCEDURE Chip       (sh: Sheet; chip: Object; x,y,r,m: INTEGER);

PROCEDURE CursorOff   (sh: Sheet);

END pedScreen.
