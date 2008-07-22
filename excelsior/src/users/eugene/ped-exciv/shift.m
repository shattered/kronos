MODULE shift; (* 19-Sep-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;
IMPORT  mcd: mCodeMnem;
IMPORT  Args;
IMPORT  Model;
FROM Model      IMPORT  Objects;
IMPORT  io: ModelIO;
IMPORT  mm: ModelMisc;
IMPORT  tp: pedTopology;

VAR mdl: Model.Object;
    nm : ARRAY [0..15] OF CHAR;

PROCEDURE Tag(o: Model.Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE Seek(mdl: Model.Object); FORWARD;

PROCEDURE Seek0(s: Model.Object; ExtPin: BOOLEAN);
  TYPE pSegment = POINTER TO Model.Segment;
  VAR i,j: INTEGER; c: Model.Object; p,e,b: pSegment;
BEGIN
  IF s=NIL THEN RETURN END;
  IF ExtPin THEN
    IF (Tag(s)#externalpin)OR(s^.PinType=NIL) THEN RETURN END;
    c:=s^.PinType;
  ELSE
    IF Tag(s)=chiptype THEN Seek(s) END;
    IF (Tag(s)#signal)     OR(s^.ChainB =NIL) THEN RETURN END;
    c:=s^.ChainB;
  END;
  IF c^.cFree=0 THEN RETURN END;
(*$T-*)
  b:=ADR(c^.cType[0]);
  p:=ADR(c^.cType[0]);
  e:=ADR(c^.cType[c^.cFree-1]);
  mm.StartConductor(s,ExtPin);
  LOOP
    mm.UnPackSeg(p^);
    INC(mm.X1,8000h); INC(mm.X2,8000h);
    INC(mm.Y1,8000h); INC(mm.Y2,8000h);
    mm.PackSeg(p^);
    IF p=e THEN RETURN END;
    p:=ADDRESS(INTEGER(p)+SIZE(Model.Segment));
  END;
(*$T+*)
END Seek0;

PROCEDURE Seek(mdl: Model.Object);
  VAR i: INTEGER;
BEGIN
  Model.Iterate(mdl^.ExternalPins,Seek0,TRUE);
  Model.Iterate(mdl^.All,Seek0,FALSE);
END Seek;

BEGIN
  Args.ScanFlags;
  Args.TakeWord(nm); IF nm[0]=0c THEN HALT END;
  mdl:=io.ReadModel(nm);
  Seek(mdl);
  io.WriteModel(mdl);
END shift.
