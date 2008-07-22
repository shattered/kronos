DEFINITION MODULE Layout; (* 12-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Object, Objects;

PROCEDURE Layouter(mdl,sig: Object; tool: CARDINAL; VAR crsX, crsY: INTEGER);

VAR TotalGang: INTEGER;
    maxV     : INTEGER;

END Layout.
