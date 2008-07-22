MODULE Speed; (* Andy 11-Dec-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;
FROM Args       IMPORT  Num?;
FROM Random     IMPORT  RanGe;
FROM cdsHeap    IMPORT  Allocate, Deallocate;

CONST MaxReqs= 2048;
      ReqSz= 9; -- in words

VAR reqs: ARRAY [0..MaxReqs-1] OF ADDRESS;
    ind:  ARRAY [0..MaxReqs-1] OF INTEGER;

PROCEDURE puzzle;
 VAR i: INTEGER; j1, j2, tmp:INTEGER;
 CONST tries= MaxReqs;
BEGIN
  FOR i:=0 TO HIGH(ind) DO ind[i]:=i END;
  FOR i:=0 TO tries DO
    j1:=RanGe(0,HIGH(ind)); j2:=RanGe(0,HIGH(ind));
    tmp:=ind[j1]; ind[j1]:=ind[j2]; ind[j2]:=tmp;
  END;
END puzzle;

PROCEDURE ReqMem;
 VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(reqs) DO Allocate(reqs[i],ReqSz) END;
END ReqMem;

PROCEDURE FreeMem;
 VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(reqs) DO Deallocate(reqs[ind[i]],ReqSz) END;
END FreeMem;

VAR i,n:INTEGER;

BEGIN puzzle;
 IF Num?(n) THEN n:=1 END;
 FOR i:=0 TO n-1 DO ReqMem; FreeMem END;
END Speed.
