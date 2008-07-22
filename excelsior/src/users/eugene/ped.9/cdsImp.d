DEFINITION MODULE cdsImp; (* Sem 28-Dec-87. (c) KRONOS *)

FROM pedModel   IMPORT  signal;

VAR normX,normY: INTEGER;

PROCEDURE normal(x1,y1,x2,y2,x,y: INTEGER);

PROCEDURE delta(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE Bril(s: signal);

END cdsImp.
