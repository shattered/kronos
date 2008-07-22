IMPLEMENTATION MODULE intRAM; (* 02-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS, ADR;

VAR Tag: INTEGER;
    t,m: BITSET;
    mem: ARRAY [0..0FFFFh] OF WORD;

PROCEDURE CoreRD(Adr: INTEGER): WORD;
BEGIN
  RETURN mem[INTEGER(BITSET(Adr)-m+t)];
END CoreRD;

PROCEDURE CoreWR(Adr: INTEGER; Val: WORD);
BEGIN
  mem[INTEGER(BITSET(Adr)-m+t)]:=Val;
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
BEGIN
END CalcPH;

PROCEDURE GetCode(f: INTEGER): ADDRESS;
BEGIN
  f:=f MOD 100000h;
  RETURN ADR(mem[f]);
END GetCode;

PROCEDURE ShowStat;
BEGIN
END ShowStat;

BEGIN
  m:={30,31}; SetTag(0); MMon:=FALSE;
END intRAM.
