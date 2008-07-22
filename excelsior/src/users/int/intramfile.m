IMPLEMENTATION MODULE intRAM; (* 02-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS, ADR;
FROM BIO        IMPORT  OpenOnDir, checkHALT, CD, bWrite, Close, SetEof;
FROM FsPublic   IMPORT  File, FileName;
FROM Resource   IMPORT  Final;

VAR Tag: INTEGER;
    t,m: BITSET;
    mem: ARRAY [0..0FFFFh] OF WORD;
    buf: ARRAY [0..1023] OF INTEGER;
    blk,ptr: INTEGER;
    nm: FileName;
    fl: File;

PROCEDURE CoreRD(Adr: INTEGER): WORD;
BEGIN
  IF ptr>=1024 THEN
    checkHALT(bWrite(fl,blk,ADR(buf),4096),nm);
    INC(blk); ptr:=0;
  END;
  Adr:=INTEGER(BITSET(Adr)-m+t);
  buf[ptr]:=Adr; INC(ptr);
  RETURN mem[Adr];
END CoreRD;

PROCEDURE CoreWR(Adr: INTEGER; Val: WORD);
BEGIN
  IF ptr>=1024 THEN
    checkHALT(bWrite(fl,blk,ADR(buf),4096),nm);
    INC(blk); ptr:=0;
  END;
  Adr:=INTEGER(BITSET(Adr)-m+t);
  buf[ptr]:=INTEGER(BITSET(Adr)+{31}); INC(ptr);
  mem[Adr]:=Val;
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

PROCEDURE Finish;
BEGIN
  SetEof(fl,blk*4096+ptr*4);
  IF ptr>0 THEN
    checkHALT(bWrite(fl,blk,ADR(buf),ptr*4+4),nm);
  END;
  checkHALT(Close(fl),nm);
END Finish;

BEGIN
  nm:='ADR.ST';
  m:={30,31}; SetTag(0); MMon:=FALSE;
  blk:=0; ptr:=0;
  checkHALT(OpenOnDir(CD(),fl,nm),nm);
  Final(Finish);
END intRAM.
