DEFINITION MODULE pedMacro; (* Sem 12-May-87. (c) KRONOS *)

IMPORT  Model;
FROM pedEditor  IMPORT  Sheet;

TYPE chip_proc = PROCEDURE(Sheet, Model.Object, INTEGER, INTEGER);

PROCEDURE DefineMacro(sht: Sheet; x1,y1,x2,y2: INTEGER);

PROCEDURE InsertMacro(x,y: INTEGER; shtd,shts: Sheet);

PROCEDURE InsertMetalMacro(x,y: INTEGER; shtd,shts: Sheet);

PROCEDURE DeleteBox(sht: Sheet; x1,y1,x2,y2: INTEGER);

PROCEDURE DefineChipMacro(sht: Sheet; x1,y1,x2,y2: INTEGER);

PROCEDURE DoChipMacro(sht: Sheet; x,y: INTEGER; Do: chip_proc);

END pedMacro.
