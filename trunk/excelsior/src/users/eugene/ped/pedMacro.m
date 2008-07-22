IMPLEMENTATION MODULE pedMacro; (*$U+ Sem 12-May-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADR;
FROM Model      IMPORT  Objects, Object, NewObject, List, Iterate, Tie,
                        InitList, KillList, SigTypes, SigType, Segment;
FROM pedEditor  IMPORT  Sheet, Editor, EdtContext;
FROM pedTopology IMPORT SeekInBox, LineXs, LineXe, LineYs, LineYe, Side,
                        StrongSide, InsertVias, InsertRange, DeleteRange,
                        Shoted?, ShotedSignal, DeleteVias, App;
FROM ModelMisc  IMPORT  X1, X2, Y1, Y2, Layer, Size, ViasSize, Signal,
                        UnPackSeg, AppConductor, StartConductor, Fixed;
FROM ModelPbl   IMPORT  RaiseInMe, MemoryOverflow;
IMPORT  mcd: defCodes;
IMPORT  mem: cdsHeap;

WITH STORAGE (NEW    : mem.Allocate;
              DISPOSE: mem.Deallocate;
              RESIZE : mem.Reallocate);

TYPE cRec=RECORD
       x1,y1,x2,y2,size,vias: INTEGER;
       fixed: BOOLEAN;
       s,s1 : Object;
       layer: BITSET
     END;

VAR cBuf: DYNARR OF Object;
    Buf: DYNARR OF cRec;
    cBufPtr: INTEGER;
    BufPtr: INTEGER;
    X3,Y3,X4,Y4: INTEGER;
    null: Object;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE Point(VAR x,y: INTEGER);
  VAR a,b: REAL;
BEGIN
  a:=FLOAT((Y3-Y4)*(X2-X1)); b:=FLOAT((Y1-Y2)*(X4-X3));
  x:=TRUNC((FLOAT(Y3-Y1)*FLOAT(X4-X3)*FLOAT(X2-X1)-
            FLOAT(X1)*b+FLOAT(X3)*a)/(a-b));
  y:=TRUNC((FLOAT(X3-X1)*FLOAT(Y4-Y3)*FLOAT(Y2-Y1)-
            FLOAT(Y1)*a+FLOAT(Y3)*b)/(b-a));
END Point;

CONST delta=16;

PROCEDURE Seek(): BOOLEAN;
  VAR x,y: INTEGER;
BEGIN
  IF BufPtr>HIGH(Buf) THEN RESIZE(Buf,BufPtr+delta) END;
ASSERT((Signal#NIL)&((Tag(Signal)=signal) OR (Tag(Signal)=externalpin)));
  WITH Buf[BufPtr] DO
    layer:=Layer;
    size:=Size;
    vias:=ViasSize;
    fixed:=Fixed;
    s:=Signal; s1:=null;
    IF X1=X2 THEN
      IF Y2<Y1 THEN y:=Y1; Y1:=Y2; Y2:=y END;
      IF (LineXs>X1)OR(LineXe<X1) THEN RETURN FALSE END;
      IF (Y2<LineYs)OR(Y1>LineYe) THEN RETURN FALSE END;
      x1:=X1; x2:=X2;
      IF Y1<LineYs THEN y1:=LineYs; vias:=0 ELSE y1:=Y1 END;
      IF Y2>LineYe THEN y2:=LineYe ELSE y2:=Y2 END;
    ELSIF Y1=Y2 THEN
      IF X2<X1 THEN x:=X1; X1:=X2; X2:=x END;
      IF (LineYs>Y1)OR(LineYe<Y1) THEN RETURN FALSE END;
      IF (X2<LineXs)OR(X1>LineXe) THEN RETURN FALSE END;
      y1:=Y1; y2:=Y2;
      IF X1<LineXs THEN x1:=LineXs; vias:=0 ELSE x1:=X1 END;
      IF X2>LineXe THEN x2:=LineXe ELSE x2:=X2 END;
    ELSE
      IF (X1<LineXs)&(X2<LineXs)OR(X1>LineXe)&(X2>LineXe) THEN
        RETURN FALSE END;
      IF (Y1<LineYs)&(Y2<LineYs)OR(Y1>LineYe)&(Y2>LineYe) THEN
        RETURN FALSE END;
      x1:=X1; y1:=Y1; x2:=X2; y2:=Y2;
(* 1 *)
      X3:=LineXs; Y3:=LineYs; X4:=LineXe; Y4:=LineYs;
      Point(x,y);
      IF (Side(X3,Y3,X4,Y4,x,y)=0) & (StrongSide(x1,y1,x2,y2,x,y)=0) THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x1:=x; y1:=y;
        ELSE x2:=x; y2:=y; vias:=0 END;
      END;
(* 2 *)
      X3:=LineXe; Y3:=LineYs; X4:=LineXe; Y4:=LineYe;
      Point(x,y);
      IF (StrongSide(X3,Y3,X4,Y4,x,y)=0) & (StrongSide(x1,y1,x2,y2,x,y)=0) THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x2:=x; y2:=y;
        ELSE x1:=x; y1:=y; vias:=0 END;
      END;
(* 3 *)
      X3:=LineXe; Y3:=LineYe; X4:=LineXs; Y4:=LineYe;
      Point(x,y);
      IF (Side(X3,Y3,X4,Y4,x,y)=0) & (StrongSide(x1,y1,x2,y2,x,y)=0) THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x2:=x; y2:=y;
        ELSE x1:=x; y1:=y; vias:=0 END;
      END;
(* 4 *)
      X3:=LineXs; Y3:=LineYe; X4:=LineXs; Y4:=LineYs;
      Point(x,y);
      IF (StrongSide(X3,Y3,X4,Y4,x,y)=0) & (StrongSide(x1,y1,x2,y2,x,y)=0) THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x1:=x; y1:=y;
        ELSE x2:=x; y2:=y; vias:=0 END;
      END;
      IF (x1<LineXs)OR(x2<LineXs)OR(x1>LineXe)OR(x2>LineXe)OR
         (y1<LineYs)OR(y2<LineYs)OR(y1>LineYe)OR(y2>LineYe) THEN
        RETURN FALSE END;
    END;
    DEC(x1,LineXs); DEC(x2,LineXs);
    DEC(y1,LineYs); DEC(y2,LineYs);
    IF (x1=x2)&(y1=y2)&(vias=0) THEN RETURN FALSE END;
  END;
  INC(BufPtr);
  RETURN FALSE;
END Seek;

VAR Key: ARRAY [0..15] OF CHAR;
    Res: Object;

PROCEDURE IterProc(o: Object; info: WORD);
BEGIN
  IF o^.Name=Key THEN Res:=o END;
END IterProc;

PROCEDURE DefineMacro(sht: Sheet; x1,y1,x2,y2: INTEGER);

VAR SignalList: List;
VAR sht1: Sheet;
    macro: Object;
    l,i: INTEGER;

PROCEDURE FindSignal(nm: ARRAY OF CHAR): Object;
BEGIN
  Key:=nm; Res:=NIL;
  Iterate(SignalList,IterProc,0);
  IF Res#NIL THEN RETURN Res END;
  Res:=NewObject(signal);
  Res^.Name:=nm; Tie(SignalList,Res); Tie(sht1^.mdl^.All,Res);
  RETURN Res;
END FindSignal;

BEGIN
  BufPtr:=0;
  IF sht^.mdl=NIL THEN RETURN END;
  IF (x1>=x2)OR(y1>=y2) THEN RETURN END;
  LineXs:=x1; LineXe:=x2; LineYs:=y1; LineYe:=y2;
  SeekInBox(sht,Seek);
  macro:=NewObject(chiptype);
  macro^.Name:='macro';
  macro^.ctX:=x2-x1; macro^.ctY:=y2-y1;
  NEW(sht1);
  sht1^.mdl:=macro;
  sht1^.wnd:=NIL;
  sht1^.EditorContext:=EdtContext(NIL);
  sht1^.PublicContext:=NIL;
  sht1^.ScreenContext:=NIL;
  Editor(sht1);
  InitList(SignalList);
  FOR i:=0 TO BufPtr-1 DO
    WITH Buf[i] DO
      Res:=FindSignal(s^.Name); Res^.sType:=s^.sType;
      StartConductor(Res,FALSE);
      X1:=x1; X2:=x2; Y1:=y1; Y2:=y2;
      Layer:=layer; Size:=size; ViasSize:=vias; Fixed:=fixed;
      App(sht1);
    END;
  END;
  KillList(SignalList);
END DefineMacro;

PROCEDURE DeleteBox(sht: Sheet; xs,ys,xe,ye: INTEGER);
  VAR i,l,cnt: INTEGER;
BEGIN
  cnt:=0;
  REPEAT
    BufPtr:=0;
    IF sht^.mdl=NIL THEN RETURN END;
    IF (xs>=xe)OR(ys>=ye) THEN RETURN END;
    LineXs:=xs; LineXe:=xe; LineYs:=ys; LineYe:=ye;
    SeekInBox(sht,Seek);
    FOR i:=0 TO BufPtr-1 DO
      WITH Buf[i] DO
        IF vias>0 THEN
          LineXs:=x1+xs; LineXe:=x1+xs;
          LineYs:=y1+ys; LineYe:=y1+ys;
          DeleteVias(s,sht);
        END;
        IF (x1#x2)OR(y1#y2) THEN
          LineXs:=x1+xs; LineXe:=x2+xs;
          LineYs:=y1+ys; LineYe:=y2+ys;
          FOR l:=0 TO 7 DO IF l IN layer THEN DeleteRange(l,s,sht) END END;
        END;
      END;
    END;
    INC(cnt);
  UNTIL (BufPtr=0) OR (cnt>10);
END DeleteBox;

VAR cnd: Object;
    Ident: INTEGER;
    Empty: BOOLEAN;

(*$T-*)
PROCEDURE next_conductor;
BEGIN
  IF Empty THEN RETURN END;
  INC(Ident);
  IF Ident>=cnd^.cFree THEN
    Empty:=TRUE; RETURN;
  ELSE
    Empty:=FALSE; UnPackSeg(cnd^.cType[Ident]);
  END;
END next_conductor;
(*$T+*)

PROCEDURE start_conductor(s: Object);
BEGIN
  IF s=NIL THEN Empty:=TRUE; RETURN END;
  cnd:=s^.ChainB; Ident:=-1; Empty:=FALSE;
  next_conductor;
END start_conductor;

PROCEDURE FiSi(o: Object; info: WORD);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name=Key THEN Res:=o END;
END FiSi;

VAR sx,sy: INTEGER;

PROCEDURE cre_chain(sig: Object; shtd: Sheet);
  VAR sg: Object;

PROCEDURE FindSignal(nm: ARRAY OF CHAR; type: SigType): Object;
BEGIN
  Key:=nm; Res:=NIL;
  Iterate(shtd^.mdl^.All,FiSi,0);
  IF Res#NIL THEN RETURN Res END;
  Res:=NewObject(signal);
  IF (fixed  IN type) THEN INCL(Res^.sType,fixed ) END;
  IF (fantom IN type) THEN INCL(Res^.sType,fantom) END;
  IF (power  IN type) THEN INCL(Res^.sType,power ) END;
  Res^.Name:=nm; Tie(shtd^.mdl^.All,Res);
  RETURN Res;
END FindSignal;

  VAR l: INTEGER;
BEGIN
  IF Tag(sig)#signal THEN RETURN END;
  sg:=FindSignal(sig^.Name,sig^.sType);
  StartConductor(sg,FALSE);
  start_conductor(sig);
  WHILE NOT Empty DO
    LineXs:=sx+X1; LineXe:=sx+X2; LineYs:=sy+Y1; LineYe:=sy+Y2;
    IF (X1=X2)&(Y1=Y2) THEN
      IF InsertVias(Size,ViasSize,Fixed,shtd,sg,shtd^.PublicContext^.check_on)
      THEN END;
    ELSE
    IF 0 IN Layer THEN l:=0 ELSE l:=1 END;
      IF InsertRange(Size,l,Fixed,shtd,sg,shtd^.PublicContext^.check_on)
      THEN END;
    END;
    next_conductor
  END;
END cre_chain;

PROCEDURE InsertMacro(x,y: INTEGER; shtd,shts: Sheet);
BEGIN
  sx:=x; sy:=y;
  Iterate(shts^.mdl^.All,cre_chain,shtd);
END InsertMacro;

PROCEDURE InsertMetalMacro(x,y: INTEGER; shtd,shts: Sheet);
  VAR i,l,cnt,cnt1: INTEGER; b: BOOLEAN;
BEGIN
  BufPtr:=0;
  LineXs:=0; LineXe:=shts^.mdl^.ctX; LineYs:=0; LineYe:=shts^.mdl^.ctY;
  SeekInBox(shts,Seek);
  FOR i:=0 TO BufPtr-1 DO Buf[i].s1:=null END; cnt:=BufPtr;
  WHILE cnt>0 DO
    cnt1:=cnt;
    FOR i:=0 TO BufPtr-1 DO
      WITH Buf[i] DO
        FOR l:=0 TO 1 DO
          IF l IN layer THEN
            IF s1=null THEN
              LineXs:=x+x1; LineXe:=x+x2; LineYs:=y+y1; LineYe:=y+y2;
              IF Shoted?(size,l,shtd,null) THEN
                s1:=ShotedSignal;
                LineXs:=x+x1; LineXe:=x+x2; LineYs:=y+y1; LineYe:=y+y2;
                IF (x1=x2)&(y1=y2) THEN
                  b:=InsertVias(size,vias,TRUE,shtd,s1,TRUE);
                ELSE
                  b:=InsertRange(size,l,TRUE,shtd,s1,TRUE);
                END;
                DEC(cnt);
              END;
            END;
          END;
        END;
      END;
    END;
    IF cnt1=cnt THEN RETURN END;
  END;
END InsertMetalMacro;

TYPE pSegment=POINTER TO Segment;

PROCEDURE FindChip(s: Object; l: pSegment);
  PROCEDURE chk_box(p1,p2: pSegment): BOOLEAN; CODE 0F8h END chk_box;
  VAR  ch: Segment;
BEGIN
  IF Tag(s)#chip THEN RETURN END;
  CASE s^.RB OF
    -1: RETURN ;
    |0: ch.start:=s^.XB+8000h+INTEGER((s^.YB+8000h)<<16);
        ch.end  :=s^.XB+s^.ChipType^.ctX+8000h+
                  INTEGER((s^.YB+s^.ChipType^.ctY+8000h)<<16);
    |1: ch.start:=s^.XB+8000h+INTEGER((s^.YB-s^.ChipType^.ctX+8000h)<<16);
        ch.end  :=s^.XB+s^.ChipType^.ctY+8000h+INTEGER((s^.YB+8000h)<<16);
    |2: ch.start:=s^.XB-s^.ChipType^.ctX+8000h+
                  INTEGER((s^.YB-s^.ChipType^.ctY+8000h)<<16);
        ch.end  :=s^.XB+8000h+INTEGER((s^.YB+8000h)<<16);
    |3: ch.start:=s^.XB-s^.ChipType^.ctY+8000h+INTEGER((s^.YB+8000h)<<16);
        ch.end  :=s^.XB+8000h+INTEGER((s^.YB+s^.ChipType^.ctX+8000h)<<16);
  END;
  IF NOT chk_box(ADR(ch),l) THEN RETURN END;
  INC(cBufPtr);
  IF cBufPtr>HIGH(Buf) THEN RESIZE(cBuf,cBufPtr+delta) END;
  cBuf[cBufPtr]:=s;
END FindChip;

PROCEDURE DefineChipMacro(sht: Sheet; x1,y1,x2,y2: INTEGER);
  VAR l: Segment;
BEGIN
  cBufPtr:=-1;
  l.start:=x1+8000h+INTEGER((y1+8000h)<<16);
  l.end  :=x2+8000h+INTEGER((y2+8000h)<<16);
  Iterate(sht^.mdl^.All,FindChip,ADR(l));
END DefineChipMacro;

PROCEDURE DoChipMacro(sht: Sheet; x,y: INTEGER; Do: chip_proc);
  VAR i: INTEGER;
BEGIN
  IF cBufPtr<0 THEN RETURN END;
  FOR i:=0 TO cBufPtr DO Do(sht,cBuf[i],x,y) END;
END DoChipMacro;

BEGIN
  BufPtr:=0; null:=NewObject(signal);
END pedMacro.
