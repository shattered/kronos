DEFINITION MODULE bmpGrafic; (* Sem 26-May-87. (c) KRONOS *)

FROM Model IMPORT Object;

PROCEDURE Print(mdl: Object; Scale: INTEGER; sg: ARRAY OF CHAR);

PROCEDURE PrintPins(mdl: Object; Scale: INTEGER);

PROCEDURE PrintChips(mdl: Object; Scale: INTEGER);

END bmpGrafic.
