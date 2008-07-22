DEFINITION MODULE Layout; (* 12-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Object, Objects;
FROM pedEditor  IMPORT  Sheet;

PROCEDURE Layouter(sht: Sheet; sig: Object;
                   tool: INTEGER; VAR crsX, crsY: INTEGER);

VAR TotalGang: INTEGER;
    maxV     : INTEGER;

END Layout.
