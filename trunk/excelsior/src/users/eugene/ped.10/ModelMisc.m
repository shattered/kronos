IMPLEMENTATION MODULE ModelMisc; (* Sem 13-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Lget, Tie, NewObject, Tag, setTag,
                        ObjectsNo, Segment, Object, Objects;
FROM cdsHeap   IMPORT   Allocate, Deallocate;
FROM ModelPbl  IMPORT   RaiseInMe, CrashInModel, Message;
FROM Strings   IMPORT   Str1, AppStr;
FROM KRONOS    IMPORT   MOVE;
FROM SYSTEM    IMPORT   ADR, ADDRESS;

PROCEDURE Error(s: ARRAY OF CHAR);
BEGIN
  Str1(Message,'Dange! Crash in model: ');
  AppStr(Message,s);
  RaiseInMe(CrashInModel);
END Error;

PROCEDURE PinLocation(o: Object; VAR X,Y: CARDINAL);
  VAR p: Object; X1,Y1,R: CARDINAL;
BEGIN
  IF (o=NIL)OR(Tag(o)#pin) THEN Error('chip without pin.') END;
  X:=0; Y:=0;
  IF o^.Chip=NIL THEN Error('pin without chip.') END;
  p:=o^.Chip^.ChipType;
  IF (p=NIL)OR(Tag(p)#chiptype) THEN Error('unknown chip.') END;
  X1:=o^.Chip^.XB;
  Y1:=o^.Chip^.YB;
  R :=o^.Chip^.RB;
  p:=Lget(p^.ExternalPins,o^.No);
  IF (p=NIL)OR(Tag(p)#externalpin) THEN Error('illegal chip type.') END;
  CASE R MOD 4 OF
    0: X:=X1+p^.PinX; Y:=Y1+p^.PinY;
   |1: X:=X1+p^.PinY; Y:=Y1-p^.PinX;
   |2: X:=X1-p^.PinX; Y:=Y1-p^.PinY;
   |3: X:=X1-p^.PinY; Y:=Y1+p^.PinX;
  END;
END PinLocation;

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
  WITH s DO
    size:=0;
    IF X1>X2 THEN
      x1:=X2-Size; x2:=X1+Size;
      IF Y1>Y2 THEN
        y1:=Y2-Size; y2:=Y1+Size; INCL(BITSET(size),1);
      ELSE
        y1:=Y1-Size; y2:=Y2+Size;
      END;
    ELSE
      x1:=X1-Size; x2:=X2+Size;
      IF Y1>Y2 THEN
        y1:=Y2-Size; y2:=Y1+Size;
      ELSE
        y1:=Y1-Size; y2:=Y2+Size; INCL(BITSET(size),1);
      END;
    END;
    x1:=x1+8000h; x2:=x2+8000h;
    y1:=y1+8000h; y2:=y2+8000h;
    IF x1<0 THEN x1:=0 END;
    IF x2<0 THEN x2:=0 END;
    IF y1<0 THEN y1:=0 END;
    IF y2<0 THEN y2:=0 END;
    IF x1>0FFFFh THEN x1:=0FFFFh END;
    IF x2>0FFFFh THEN x2:=0FFFFh END;
    IF y1>0FFFFh THEN y1:=0FFFFh END;
    IF y2>0FFFFh THEN y2:=0FFFFh END;
    IF CircleTool THEN INCL(BITSET(size),2) END;
    IF Fixed      THEN INCL(BITSET(size),0) END;
    INC(size,INTEGER(Size>>12)+INTEGER(Layer<<12)+INTEGER(ViasSize<<4));
    start:=x1+INTEGER(y1>>16);
    end  :=x2+INTEGER(y2>>16);
  END;
END PackSeg;

PROCEDURE UnPackSeg(VAL s: Segment);
BEGIN
  WITH s DO
    IF size=0 THEN Undef:=TRUE; RETURN END;
    Size:=CARDINAL(BITSET(size<<12)*{0..11});
    IF 1 IN BITSET(size) THEN
      X1:=CARDINAL(BITSET(start)*mask)+Size-8000h;
      Y1:=CARDINAL(BITSET(start>>16)*mask)+Size-8000h;
      X2:=CARDINAL(BITSET(end)*mask)-Size-8000h;
      Y2:=CARDINAL(BITSET(end>>16)*mask)-Size-8000h;
    ELSE
      X1:=CARDINAL(BITSET(start)*mask)+Size-8000h;
      Y2:=CARDINAL(BITSET(start>>16)*mask)+Size-8000h;
      X2:=CARDINAL(BITSET(end)*mask)-Size-8000h;
      Y1:=CARDINAL(BITSET(end>>16)*mask)-Size-8000h;
    END;
    Layer:=BITSET(size>>12)*{0..7};
    ViasSize:=CARDINAL(BITSET(size>>4)*{0..7});
    Fixed:=0 IN BITSET(size);
    CircleTool:=2 IN BITSET(size);
  END;
  Undef:=FALSE;
END UnPackSeg;

PROCEDURE StartConductor(s: Object);
BEGIN
  ASSERT(Tag(s)=signal);
  Signal:=s;
  Chain:=s^.ChainB;
  Ident:=-1; Empty:=FALSE;
  NextConductor;
END StartConductor;

PROCEDURE NextConductor;
BEGIN
  LOOP
    IF Empty THEN RETURN END;
    INC(Ident);
    IF (Chain=NIL)OR(Ident>=Chain^.cLen) THEN
      Empty:=TRUE; Undef:=TRUE;
    ELSE
      Empty:=FALSE;
      UnPackSeg(Chain^.cType[Ident]);
    END;
    IF NOT Undef THEN RETURN END;
  END;
END NextConductor;

PROCEDURE AppConductor;
  VAR o: Object; i: CARDINAL;
BEGIN
  IF Chain=NIL THEN
    Allocate(Chain,SIZE(Segment)+3); Signal^.ChainB:=Chain;
    setTag(Chain,conductor); Chain^.cLen:=1;
    Ident:=0; INC(ObjectsNo);
  ELSE
    IF (Ident<0)OR(Ident>=Chain^.cLen) THEN Ident:=0 END;
    i:=Ident;
    LOOP
      IF Chain^.cType[i].size=0 THEN Ident:=i; EXIT END;
      i:=(i+1)MOD Chain^.cLen;
      IF i=Ident THEN
        Ident:=Chain^.cLen;
        Allocate(o,3+SIZE(Segment)*(Chain^.cLen+1));
        MOVE(o,Chain,3+SIZE(Segment)*Chain^.cLen);
        INC(o^.cLen); Signal^.ChainB:=o;
        Deallocate(Chain,3+SIZE(Segment)*Chain^.cLen);
        Chain:=o;
        EXIT;
      END;
    END;
  END;
  PackSeg(Chain^.cType[Ident]);
  Empty:=FALSE; Undef:=FALSE;
END AppConductor;

PROCEDURE CreateConductor(n: INTEGER);
  VAR o: Object; i,x1,x2,y1,y2: CARDINAL;
BEGIN
  IF (Chain#NIL)&(Chain^.cLen>=n) THEN RETURN END;
  Allocate(o,3+SIZE(Segment)*n);
  IF Chain#NIL THEN
    MOVE(o,Chain,3+Chain^.cLen*SIZE(Segment));
    FOR i:=Chain^.cLen TO n-1 DO o^.cType[i].size:=0 END;
    Deallocate(Chain,3+Chain^.cLen*SIZE(Segment));
  ELSE
    FOR i:=0 TO n-1 DO o^.cType[i].size:=0 END;
    INC(ObjectsNo);
  END;
  o^.cLen:=n; setTag(o,conductor);
  Signal^.ChainB:=o;
  StartConductor(Signal);
END CreateConductor;

PROCEDURE DelConductor;
BEGIN
  IF Empty OR Undef THEN RETURN END;
  WITH Chain^.cType[Ident] DO start:=0; end:=0; size:=0 END;
  Undef:=TRUE;
END DelConductor;

PROCEDURE TruncConductor;
  VAR i,j,l: INTEGER; o: Object;
BEGIN
  i:=Chain^.cLen-1;
  WHILE (i>=0)&(Chain^.cType[i].size=0) DO DEC(i) END;
  j:=Chain^.cLen-i-1;
  IF j>0 THEN
    DEC(Chain^.cLen,j);
    l:=3+Chain^.cLen*SIZE(Segment);
    Allocate(o,l); MOVE(o,Chain,l);
    Deallocate(Chain,l+j*SIZE(Segment));
    Signal^.ChainB:=o; Chain:=o;
  END;
END TruncConductor;

PROCEDURE FindConductor(id: CARDINAL);
  VAR i: CARDINAL;
BEGIN
  Ident:=id;
  IF (Chain=NIL)OR(Ident>=Chain^.cLen) THEN
    Empty:=TRUE; Undef:=TRUE;
  ELSE
    Empty:=FALSE;
    UnPackSeg(Chain^.cType[Ident]);
  END;
END FindConductor;
(*$T+*)

BEGIN
  Signal:=NIL;
  Chain:=NIL;
  Empty:=TRUE;
  mask:={0..15};
END ModelMisc.
