IMPLEMENTATION MODULE intRAM; (* 02-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS;
FROM intExcp    IMPORT  RaiseInMe, IllegalAdr, Reaction, Exception?,
                        NoPage, Exception, KillReaction;
IMPORT intMemory;

VAR Tag: INTEGER;
    t,m: BITSET;
    a  : INTEGER;
    rd : BOOLEAN;
    e  : Reaction;
    r  : BOOLEAN;
    v  : WORD;

PROCEDURE CoreRD(Adr: INTEGER): WORD;
BEGIN
  LOOP
    r:=Exception?(e);
    IF r THEN
      IF r=NoPage THEN CalcPH ELSE RaiseInMe(Exception(r)) END;
    ELSE
      a:=Adr; rd:=TRUE;
      v:=intMemory.CoreRD(INTEGER(BITSET(Adr)-m+t));
      KillReaction(e);
      RETURN v;
    END;
  END;
END CoreRD;

PROCEDURE CoreWR(Adr: INTEGER; Val: WORD);
BEGIN
  LOOP
    r:=Exception?(e);
    IF r THEN
      IF r=NoPage THEN CalcPH ELSE RaiseInMe(Exception(r)) END;
    ELSE
      a:=Adr; rd:=TRUE;
      intMemory.CoreWR(INTEGER(BITSET(Adr)-m+t),Val);
      KillReaction(e); EXIT;
    END;
  END;
END CoreWR;

PROCEDURE SetTag(i: INTEGER);
BEGIN
  Tag:=i;
  CASE i OF
    |0: t:={};
    |1: t:={30};
    |2: t:={31};
  END;
END SetTag;

PROCEDURE CalcPH;
  VAR n: INTEGER;
BEGIN
  ASSERT(NOT MMon);
  n:=INTEGER(BITSET(a)-m);
  IF (n<0)OR(n>0FFFFh)OR(Tag#0) THEN RaiseInMe(IllegalAdr) END;
  intMemory.SetAdr(n,n DIV 1024,TRUE);
  IF rd THEN n:=intMemory.RDdirect(a) END;
END CalcPH;

PROCEDURE GetCode(f: INTEGER): ADDRESS;
BEGIN
  f:=f MOD 100000h;
  RETURN intMemory.GetCode(f);
END GetCode;

PROCEDURE ShowStat;
BEGIN
  intMemory.ShowStat;
END ShowStat;

BEGIN
  m:={30,31}; SetTag(0); MMon:=FALSE;
END intRAM.
