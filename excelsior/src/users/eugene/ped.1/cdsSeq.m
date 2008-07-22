IMPLEMENTATION MODULE cdsSeq; (* Sem 05-Jan-88. (c) KRONOS *)

IMPORT ModelMisc;
IMPORT FROM ModelMisc;
FROM SYSTEM     IMPORT  ADR;
FROM Model      IMPORT  Object, Objects, Tag, Iterate;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM pedPbl     IMPORT  Overlay;
FROM pedTopology IMPORT Len;

PROCEDURE Min(x,y: INTEGER): INTEGER;
BEGIN IF x<y THEN RETURN x ELSE RETURN y END END Min;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;

TYPE Range=RECORD
             minX,minY,maxX,maxY: INTEGER;
             Id,X1,X2,Y1,Y2,Size: INTEGER;
             Layer: BITSET
           END;
     pRange=POINTER TO Range;
(*$T-*)

VAR Idents   : POINTER TO ARRAY [0..0] OF Range;
    RangeCnt : INTEGER;
    IdCnt    : INTEGER;
    IdPtr    : INTEGER;
    CurSignal: Object;
    CurIdent : INTEGER;

PROCEDURE Connect(x1,y1,x2,y2,sz: INTEGER; ls: BITSET; p: pRange): BOOLEAN;
  VAR r,r1,dx,dy,cdx,cdy: INTEGER;
BEGIN
  WITH p^ DO
    r1:=Size+sz-Overlay;
    r:=r1*r1;
    dx:=X2-X1; dy:=Y2-Y1;
    cdx:=x2-x1; cdy:=y2-y1;
    RETURN
      ((cdx*(Y1-y1)-cdy*(X1-x1)>=0)#(cdx*(Y2-y1)-cdy*(X2-x1)>=0))
      & ((dx*(y1-Y1)-dy*(x1-X1)>=0)#(dx*(y2-Y1)-dy*(x2-X1)>=0))
      OR (Len(x1,y1,x2,y2,X1,Y1)<r)
      OR (Len(x1,y1,x2,y2,X2,Y2)<r)
      OR (Len(X1,Y1,X2,Y2,x1,y1)<r)
      OR (Len(X1,Y1,X2,Y2,x2,y2)<r);
  END;
END Connect;

PROCEDURE Areas(sig: Object);
  VAR from,to,i: INTEGER; p,q,r: pRange;
BEGIN
  ASSERT(Tag(sig)=signal);
  IdCnt:=0; IdPtr:=1;
  StartConductor(sig); RangeCnt:=0;
  WHILE NOT Empty DO INC(RangeCnt); NextConductor END;
  Allocate(Idents,RangeCnt*SIZE(Range));
  StartConductor(sig); i:=0;
  WHILE NOT Empty DO
    ASSERT(i<RangeCnt);
    WITH Idents^[i] DO
      X1:=ModelMisc.X1; X2:=ModelMisc.X2;
      Y1:=ModelMisc.Y1; Y2:=ModelMisc.Y2;
      Size:=ModelMisc.Size;
      minX:=Min(X1,X2)-Size; maxX:=Max(X1,X2)+Size;
      minY:=Min(Y1,Y2)-Size; maxY:=Max(Y1,Y2)+Size;
      Layer:=ModelMisc.Layer;
      Id:=IdPtr; INC(IdPtr); INC(IdCnt);
    END;
    INC(i);
    NextConductor;
  END;
  r:=ADR(Idents^[RangeCnt]);
  FOR i:=0 TO RangeCnt-1 DO
    WITH Idents^[i] DO
      p:=ADR(Idents^[i+1]);
      WHILE p<r DO
        IF (Id#p^.Id)&(Layer*p^.Layer#{}) THEN
          IF NOT ( (minX>=p^.maxX)OR(p^.minX>=maxX)OR
                   (minY>=p^.maxY)OR(p^.minY>=maxY) ) &
                   Connect(X1,Y1,X2,Y2,Size,Layer,p) THEN
            from:=Id; to:=p^.Id;
            q:=ADR(Idents^[0]);
            REPEAT
              IF q^.Id=from THEN q^.Id:=to END;
              INC(INTEGER(q),SIZE(Range));
            UNTIL q>=r;
            DEC(IdCnt);
          END;
        END;
        INC(INTEGER(p),SIZE(Range));
      END;
    END;
  END;
END Areas;


TYPE PinRec=RECORD pin: Object; ident: INTEGER END;

VAR Pins    : ARRAY [0..399] OF PinRec;
    PinsCnt : INTEGER;
    MaxIdent: INTEGER;

PROCEDURE MarkPin(p: Object);
  VAR i,x,y: INTEGER;
BEGIN
  PinLocation(p,x,y);
  FOR i:=0 TO RangeCnt-1 DO
    WITH Idents^[i] DO
      IF Len(X1,Y1,X2,Y2,x,y)<Size*Size THEN
        IF PinsCnt<=HIGH(Pins) THEN
          Pins[PinsCnt].pin:=p;
          Pins[PinsCnt].ident:=Id;
          IF Id>MaxIdent THEN MaxIdent:=Id END;
          INC(PinsCnt);
        END;
        RETURN;
      END;
    END;
  END;
END MarkPin;

PROCEDURE Next(sig: Object; VAR x,y: INTEGER);
  VAR i,id: INTEGER;
BEGIN
  IF CurSignal#sig THEN
    Areas(sig);
    PinsCnt:=0; MaxIdent:=-1;
    Iterate(sig^.TiedPins,MarkPin);
    CurSignal:=sig; CurIdent:=MaxIdent;
    Deallocate(Idents,RangeCnt*SIZE(Range));
  END;
  IF PinsCnt=0 THEN RETURN END;
  id:=CurIdent; i:=PinsCnt;
  REPEAT
    IF i>=PinsCnt-1 THEN
      i:=0; INC(id); IF id>MaxIdent THEN id:=0 END;
    ELSE
      INC(i)
    END;
  UNTIL (Pins[i].ident=id)OR(id=CurIdent);
  CurIdent:=id;
  PinLocation(Pins[i].pin,x,y);
END Next;

PROCEDURE PinNet(sig: Object; ip: IterProc);
  VAR x,y,x1,y1,id,id1,i,j: INTEGER;
BEGIN
  IF CurSignal#sig THEN
    Areas(sig);
    PinsCnt:=0; MaxIdent:=-1;
    Iterate(sig^.TiedPins,MarkPin);
    CurSignal:=sig; CurIdent:=MaxIdent;
    Deallocate(Idents,RangeCnt*SIZE(Range));
  END;
  IF PinsCnt=0 THEN RETURN END;
  PinLocation(Pins[0].pin,x,y);
  id:=Pins[0].ident;
  FOR i:=1 TO PinsCnt-1 DO
    IF Pins[i].ident#id THEN
      id1:=Pins[i].ident;
      PinLocation(Pins[i].pin,x1,y1);
      ip(x,y,x1,y1);
      FOR j:=i TO PinsCnt-1 DO
        IF Pins[j].ident=id1 THEN Pins[j].ident:=id END;
      END;
    END;
  END;
END PinNet;

BEGIN
  CurSignal:=NIL; Idents:=NIL; IdCnt:=0;
END cdsSeq.
