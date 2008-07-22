DEFINITION MODULE pedChips; (* Ilx 14-Jun-89. (c) KRONOS *)

FROM Model      IMPORT  Object;
FROM pedEditor  IMPORT  Sheet;

PROCEDURE LookUpChip(mdl: Object; x,y: INTEGER): Object;

PROCEDURE LookUpPin (chip: Object; x,y: INTEGER): Object;

PROCEDURE UpdatePin(sht: Sheet; pin: Object);

PROCEDURE FindChip(mdl: Object; name: ARRAY OF CHAR): Object;

PROCEDURE MoveChip(sht: Sheet; chip: Object; x,y,r: INTEGER): BOOLEAN;

PROCEDURE NewChip (sht: Sheet; chip_type: Object;
                   name: ARRAY OF CHAR; VAR chip: Object): BOOLEAN;

PROCEDURE KillChip(sht: Sheet; chip: Object): BOOLEAN;

PROCEDURE RenamePin(sht: Sheet; p,s: Object; chk: BOOLEAN): BOOLEAN;

PROCEDURE DeleteMetal(sht: Sheet; chip: Object): BOOLEAN;

PROCEDURE InsertMetal(sht: Sheet; chip: Object): BOOLEAN;

PROCEDURE InsertAllPins(sht: Sheet; check?: BOOLEAN): BOOLEAN;

END pedChips.
