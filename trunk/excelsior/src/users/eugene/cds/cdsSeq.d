DEFINITION MODULE cdsSeq; (* Sem 05-Jan-88. (c) KRONOS *)

FROM Model      IMPORT  Object;

PROCEDURE Next(sig: Object; VAR x,y: INTEGER);

TYPE IterProc=PROCEDURE (INTEGER,INTEGER,INTEGER,INTEGER);

PROCEDURE PinNet(sig: Object; ip: IterProc);

END cdsSeq.
