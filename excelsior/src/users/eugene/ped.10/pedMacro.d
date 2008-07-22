DEFINITION MODULE pedMacro; (* Sem 12-May-87. (c) KRONOS *)

FROM Model     IMPORT   Object;

PROCEDURE DefineMacro(o: Object; x1,y1,x2,y2: INTEGER);

PROCEDURE InsertMacro(x,y: INTEGER; mdl: Object);

PROCEDURE InsertMetalMacro(x,y: INTEGER; mdl: Object);

PROCEDURE DeleteBox(o: Object; x1,y1,x2,y2: INTEGER);

END pedMacro.
