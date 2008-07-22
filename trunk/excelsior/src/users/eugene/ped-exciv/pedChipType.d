DEFINITION MODULE pedChipType; (* 09-Sep-89. (c) KRONOS *)

FROM Model      IMPORT  Object;

PROCEDURE CreateChipType(name: ARRAY OF CHAR): Object;

PROCEDURE FindChipType  (name: ARRAY OF CHAR; mdl: Object): Object;

PROCEDURE GetFromFile (name: ARRAY OF CHAR; VAR cht: Object): BOOLEAN;

PROCEDURE GetFromModel
          (name: ARRAY OF CHAR; mdl: Object; VAR cht: Object): BOOLEAN;

PROCEDURE LinkToModel (cht,mdl: Object): BOOLEAN;

PROCEDURE CopyToModel (cht,mdl: Object): BOOLEAN;

PROCEDURE CreateExtPin(no,x,y: INTEGER; cht: Object): BOOLEAN;

PROCEDURE KillExtPin  (no: INTEGER; cht: Object): BOOLEAN;

END pedChipType.
