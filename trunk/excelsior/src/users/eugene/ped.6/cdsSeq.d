DEFINITION MODULE cdsSeq; (* Sem 05-Jan-88. (c) KRONOS *)

FROM Model      IMPORT  Object;

PROCEDURE Next(sig: Object; VAR x,y: INTEGER; Epin: BOOLEAN);

TYPE IterProc=PROCEDURE (INTEGER,INTEGER,INTEGER,INTEGER);

PROCEDURE PinNet(sig: Object; ip: IterProc; Epin: BOOLEAN);

PROCEDURE DisConnect(mdl,sig: Object);

END cdsSeq.
