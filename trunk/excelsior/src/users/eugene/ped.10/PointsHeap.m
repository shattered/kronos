IMPLEMENTATION MODULE PointsHeap; (* Sem 31-Dec-87. (c) KRONOS *)

IMPORT cdsHeap;
FROM SYSTEM     IMPORT  ADR;
FROM Points     IMPORT  pPoint, Point;
FROM ModelPbl   IMPORT  RaiseInMe, MemoryOverflow;

CONST max=500;

VAR Wsp : ARRAY [0..max-1] OF Point;
    Free: ARRAY [0..max-1] OF pPoint;
    FreeCnt: INTEGER;
    ptr : pPoint;

PROCEDURE Allocate(VAR p: pPoint);
BEGIN
  IF FreeCnt>0 THEN
    DEC(FreeCnt); p:=Free[FreeCnt];
  ELSIF ptr#NIL THEN
    p:=ptr; ptr:=p^.Xfwd;
  ELSE
    cdsHeap.Allocate(p,SIZE(p^));
  END;
END Allocate;

PROCEDURE Deallocate(VAR p: pPoint);
BEGIN
  IF FreeCnt<=HIGH(Free) THEN
    Free[FreeCnt]:=p; INC(FreeCnt); p:=NIL;
  ELSE
    p^.Xfwd:=ptr; ptr:=p; p:=NIL;
  END;
END Deallocate;

VAR i: INTEGER;

BEGIN
  FOR i:=0 TO max-1 DO Free[i]:=ADR(Wsp[i]) END;
  FreeCnt:=max; ptr:=NIL;
END PointsHeap.
