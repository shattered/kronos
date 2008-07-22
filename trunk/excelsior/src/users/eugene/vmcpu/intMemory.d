DEFINITION MODULE intMemory; (* 01-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS;

CONST
    RD=1;
    WR=2;
    RDph=3;
    WRph=4;
    RDck=5;
    WRck=6;

PROCEDURE Core(op: INTEGER; Adr: INTEGER; VAR Val: WORD);

PROCEDURE SetAdr(Adr: INTEGER; ph: INTEGER; we: BOOLEAN);

PROCEDURE ClearTlb(Adr: INTEGER);

PROCEDURE GetCode(Adr: INTEGER): ADDRESS;

END intMemory.
