IMPLEMENTATION MODULE intRAM; (* 02-Jun-88. (c) KRONOS *)

IMPORT intMemory;
FROM SYSTEM     IMPORT  WORD, ADDRESS, ADR;
FROM intMemory  IMPORT  Core, RD, WR, RDph, WRph, RDck, WRck;

VAR Tag: INTEGER;
    t,m: BITSET;

PROCEDURE CoreRD(Adr: INTEGER): WORD;
BEGIN
  IF FailPH THEN RETURN 0 END;
  FailAdr:=INTEGER(BITSET(Adr)-m); FailWR:=FALSE;
  Core(RD,INTEGER(BITSET(Adr)+t),FailDat);
  RETURN FailDat;
END CoreRD;

PROCEDURE CoreWR(Adr: INTEGER; Val: WORD);
BEGIN
  IF FailPH THEN RETURN END;
  FailAdr:=INTEGER(BITSET(Adr)-m); FailWR:=TRUE; FailDat:=Val;
  Core(WR,INTEGER(BITSET(Adr)+t),FailDat);
END CoreWR;

PROCEDURE CoreWRph(Adr: INTEGER; Val: WORD);
BEGIN
  Core(WRph,INTEGER(BITSET(Adr)-m),Val);
END CoreWRph;

PROCEDURE CoreRDph(Adr: INTEGER): WORD;
  VAR a: WORD;
BEGIN
  Core(RDph,INTEGER(BITSET(Adr)-m),a); RETURN a;
END CoreRDph;

PROCEDURE CheckPage(Adr: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF FailPH THEN RETURN END;
  FailAdr:=INTEGER(BITSET(Adr)-m); FailWR:=FALSE;
  Core(RDck,INTEGER(BITSET(Adr)+t),i);
END CheckPage;

PROCEDURE CheckWrPage(Adr: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF FailPH THEN RETURN END;
  FailAdr:=INTEGER(BITSET(Adr)-m); FailWR:=TRUE;
  Core(WRck,INTEGER(BITSET(Adr)+t),i);
END CheckWrPage;

PROCEDURE SetAdr(a: INTEGER; ph: INTEGER; wp: BOOLEAN);
BEGIN
  intMemory.SetAdr(a,ph,wp);
END SetAdr;

PROCEDURE SetTag(i: INTEGER);
BEGIN
  Tag:=i;
  CASE i OF
    |0: t:={};
    |1: t:={31};
  END;
END SetTag;

PROCEDURE Tag?(): INTEGER;
BEGIN
  RETURN Tag;
END Tag?;

PROCEDURE ClearAdr(Adr: INTEGER; tg: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=Tag?(); SetTag(tg);
  intMemory.ClearTlb(INTEGER(BITSET(Adr)-m+t));
  SetTag(i);
END ClearAdr;

PROCEDURE GetCode(f: INTEGER): ADDRESS;
BEGIN
  RETURN intMemory.GetCode(INTEGER(BITSET(f)-m));
END GetCode;

VAR i: INTEGER;

BEGIN
  m:={30,31}; SetTag(0); FailPH:=FALSE;
  CoreWRph(0,400h);
  CoreWRph(1,400h);
  CoreWRph(2,  4h);
  CoreWRph(3,  0h);
  CoreWRph(400h,81Fh);
  FOR i:=0 TO 1Fh DO CoreWRph(800h+i,0C00h+i*400h) END;
END intRAM.
