IMPLEMENTATION MODULE ModelMisc; (* Sem 13-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Lget, Tie, NewObject,
                        Segment, Object, Objects;
FROM cdsHeap   IMPORT   Allocate, Deallocate, Reallocate;
FROM ModelPbl  IMPORT   RaiseInMe, CrashInModel, MemoryOverflow, Message;
FROM SYSTEM    IMPORT   ADR, ADDRESS;
IMPORT  mcd: defCodes;
IMPORT  str: Strings;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE setTag(o: Object; t: Objects);
CODE mcd.stot 0 mcd.lodt mcd.sxb END setTag;

PROCEDURE move(to,from: ADDRESS; n: INTEGER); CODE mcd.move END move;

PROCEDURE bidir_move(f,t:ADDRESS; s: INTEGER); CODE 096h END bidir_move;

PROCEDURE Error(s: ARRAY OF CHAR);
BEGIN
  str.print(Message,'Dange! Crash in model: %s',s);
  RaiseInMe(CrashInModel);
END Error;

VAR Chain: Object;
    mask : BITSET;
    CircleTool: BOOLEAN;

(* bits
  0 - fixed
  1 - main direction
  2 - circle mask
  3 - without metal in vias
*)

(*$T-*)
PROCEDURE PackSeg(VAR s: Segment);
  VAR x1,x2,y1,y2: INTEGER;
BEGIN
  INC(X1,8000h); INC(X2,8000h); INC(Y1,8000h); INC(Y2,8000h);
  WITH s DO
    IF X1>X2 THEN
      x1:=X2-Size; x2:=X1+Size;
      IF Y1>Y2 THEN
        y1:=Y2-Size; y2:=Y1+Size; size:=2;
      ELSE
        y1:=Y1-Size; y2:=Y2+Size; size:=0;
      END;
    ELSE
      x1:=X1-Size; x2:=X2+Size;
      IF Y1>Y2 THEN
        y1:=Y2-Size; y2:=Y1+Size; size:=0;
      ELSE
        y1:=Y1-Size; y2:=Y2+Size; size:=2;
      END;
    END;
    DEC(X1,8000h); DEC(X2,8000h); DEC(Y1,8000h); DEC(Y2,8000h);
    IF x1<0 THEN x1:=0 END;
    IF x2<0 THEN x2:=0 END;
    IF y1<0 THEN y1:=0 END;
    IF y2<0 THEN y2:=0 END;
    IF x1>0FFFFh THEN x1:=0FFFFh END;
    IF x2>0FFFFh THEN x2:=0FFFFh END;
    IF y1>0FFFFh THEN y1:=0FFFFh END;
    IF y2>0FFFFh THEN y2:=0FFFFh END;
    IF CircleTool THEN size:=INTEGER(BITSET(size)+{2}) END;
    IF Fixed      THEN size:=INTEGER(BITSET(size)+{0}) END;
    INC(size,INTEGER(Size>>12)+INTEGER(Layer<<12)+INTEGER(ViasSize<<4));
    start:=x1+INTEGER(y1>>16);
    end  :=x2+INTEGER(y2>>16);
  END;
END PackSeg;

PROCEDURE UnPackSeg(VAR s: Segment);
BEGIN
  WITH s DO
    Size:=INTEGER(BITSET(size<<12)*{0..11});
    IF 1 IN BITSET(size) THEN
      X1:=INTEGER(BITSET(start)*mask)+Size;
      Y1:=INTEGER(BITSET(start>>16)*mask)+Size;
      X2:=INTEGER(BITSET(end)*mask)-Size;
      Y2:=INTEGER(BITSET(end>>16)*mask)-Size;
    ELSE
      X1:=INTEGER(BITSET(start)*mask)+Size;
      Y1:=INTEGER(BITSET(end>>16)*mask)-Size;
      Y2:=INTEGER(BITSET(start>>16)*mask)+Size;
      X2:=INTEGER(BITSET(end)*mask)-Size;
    END;
    DEC(X1,8000h); DEC(X2,8000h); DEC(Y1,8000h); DEC(Y2,8000h);
    Layer:=BITSET(size>>12)*{0..7};
    ViasSize:=INTEGER(BITSET(size>>4)*{0..7});
    Fixed:=0 IN BITSET(size);
    CircleTool:=2 IN BITSET(size);
  END;
END UnPackSeg;

PROCEDURE StartConductor(s: Object; ExtPin: BOOLEAN);
BEGIN
  ASSERT(Tag(s)=signal#ExtPin);
  Signal:=s;
  IF ExtPin THEN Chain:=s^.PinType ELSE Chain:=s^.ChainB END;
  Ident:=-1;
  IF Chain=NIL THEN Empty:=TRUE; RETURN END;
  Empty:=FALSE;
  NextConductor;
END StartConductor;

PROCEDURE NextConductor;
BEGIN
  INC(Ident);
  IF Ident>=Chain^.cFree THEN Empty:=TRUE;
  ELSE UnPackSeg(Chain^.cType[Ident])
  END;
END NextConductor;

CONST delta=6;

PROCEDURE AppConductor(ExtPin: BOOLEAN);
BEGIN
  ASSERT(Tag(Signal)=signal#ExtPin);
  IF Chain=NIL THEN
    Allocate(Chain,3+SIZE(Segment)*delta);
    IF ExtPin THEN Signal^.PinType:=Chain ELSE Signal^.ChainB:=Chain END;
    setTag(Chain,conductor); Chain^.cLen:=delta; Chain^.cFree:=0;
  ELSE
    IF Chain^.cLen=Chain^.cFree THEN
      IF NOT Reallocate(Chain,3+SIZE(Segment)* Chain^.cLen,
                              3+SIZE(Segment)*(Chain^.cLen+delta)) THEN
        RaiseInMe(MemoryOverflow)
      END;
      IF ExtPin THEN Signal^.PinType:=Chain ELSE Signal^.ChainB:=Chain END;
      INC(Chain^.cLen,delta);
    END;
  END;
  PackSeg(Chain^.cType[Chain^.cFree]);
  Ident:=Chain^.cFree; INC(Chain^.cFree); Empty:=FALSE;
END AppConductor;

PROCEDURE CreateConductor(n: INTEGER; ExtPin: BOOLEAN);
BEGIN
  ASSERT(Tag(Signal)=signal#ExtPin);
  IF (Chain#NIL)&(Chain^.cLen>=n) THEN RETURN END;
  IF Chain#NIL THEN
    IF NOT Reallocate(Chain,3+Chain^.cLen*SIZE(Segment),
                            3+SIZE(Segment)*(n+delta)) THEN
      RaiseInMe(MemoryOverflow)
    END;
  ELSE
    Allocate(Chain,3+SIZE(Segment)*(n+delta));
    setTag(Chain,conductor);
    Chain^.cFree:=0;
  END;
  IF ExtPin THEN Signal^.PinType:=Chain ELSE Signal^.ChainB:=Chain END;
  Chain^.cLen:=n+delta;
  StartConductor(Signal,ExtPin);
END CreateConductor;

PROCEDURE DelConductor;
BEGIN
  IF Empty THEN RETURN END;
  DEC(Chain^.cFree);
  Chain^.cType[Ident]:=Chain^.cType[Chain^.cFree];
  DEC(Ident);
END DelConductor;

PROCEDURE TruncConductor(ExtPin: BOOLEAN);
  VAR id: INTEGER;
BEGIN
  ASSERT(Tag(Signal)=signal#ExtPin);
  IF Chain=NIL THEN RETURN END;
  id:=-1;
  LOOP
    REPEAT
      INC(id);
      IF id=Chain^.cFree THEN EXIT END;
    UNTIL Chain^.cType[id].size=0;
    REPEAT
      DEC(Chain^.cFree);
      IF id=Chain^.cFree THEN EXIT END;
    UNTIL Chain^.cType[Chain^.cFree].size#0;
    Chain^.cType[id]:=Chain^.cType[Chain^.cFree]
  END;
  IF Chain^.cFree=Chain^.cLen THEN RETURN END;
  IF NOT Reallocate(Chain,3+Chain^.cLen*SIZE(Segment),
                          3+Chain^.cFree*SIZE(Segment)) THEN
    RaiseInMe(MemoryOverflow)
  END;
  Chain^.cLen:=Chain^.cFree;
  IF ExtPin THEN Signal^.PinType:=Chain ELSE Signal^.ChainB:=Chain END;
END TruncConductor;

PROCEDURE FindConductor(id: INTEGER);
  VAR i: INTEGER;
BEGIN
  Ident:=id;
  IF (Chain=NIL) OR (Ident>=Chain^.cFree) THEN Empty:=TRUE;
  ELSE Empty:=FALSE; UnPackSeg(Chain^.cType[Ident]);
  END;
END FindConductor;
(*$T+*)

BEGIN
  Signal:=NIL;
  Chain:=NIL;
  Empty:=TRUE;
  mask:={0..15};
END ModelMisc.
