DEFINITION MODULE cdsSeq; (* Sem 05-Jan-88. (c) KRONOS *)

FROM pedModel   IMPORT  signal;

PROCEDURE Next(sig: signal; VAR x,y: INTEGER);

TYPE IterProc=PROCEDURE (INTEGER,INTEGER,INTEGER,INTEGER);

PROCEDURE PinNet(sig: signal; ip: IterProc);

END cdsSeq.
