DEFINITION MODULE pedMacro; (* Sem 12-May-87. (c) KRONOS *)

FROM pedModel  IMPORT   board;

PROCEDURE DefineMacro(o: board; x1,y1,x2,y2: INTEGER);

PROCEDURE InsertMacro(x,y: INTEGER; mdl: board);

PROCEDURE InsertMetalMacro(x,y: INTEGER; mdl: board);

PROCEDURE DeleteBox(o: board; x1,y1,x2,y2: INTEGER);

END pedMacro.
