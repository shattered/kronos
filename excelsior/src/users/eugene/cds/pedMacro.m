IMPLEMENTATION MODULE pedMacro; (* Sem 12-May-87. (c) KRONOS *)

FROM Model       IMPORT   Objects, Tag, Object, NewObject;
FROM pedTopology IMPORT SeekInBox, LineXs, LineXe, LineYs, LineYe,
                        StrongSide, InsertVias, InsertRange, DeleteRange,
                        Shoted?, ShotedSignal, DeleteVias;
FROM ModelMisc   IMPORT X1, X2, Y1, Y2, Layer, Size, ViasSize, Signal;

TYPE cRec=RECORD x1,y1,x2,y2,size,vias: CARDINAL; s,s1: Object; layer: BITSET END;

VAR Buf: ARRAY [0..1999] OF cRec;
    BufPtr: CARDINAL;
    X3,Y3,X4,Y4: CARDINAL;
    null: Object;

PROCEDURE Point(VAR x,y: CARDINAL);
  VAR a,b: CARDINAL;
BEGIN
  a:=(Y3-Y4)*(X2-X1); b:=(Y1-Y2)*(X4-X3);
  x:=((Y3-Y1)*(X4-X3)*(X2-X1)-X1*b+X3*a)DIV(a-b);
  y:=((X3-X1)*(Y4-Y3)*(Y2-Y1)-Y1*a+Y3*b)DIV(b-a);
END Point;

PROCEDURE Seek(): BOOLEAN;
  VAR x,y: CARDINAL;
BEGIN
  IF BufPtr>HIGH(Buf) THEN RETURN FALSE END;
ASSERT(Signal#NIL);
ASSERT(Tag(Signal)=signal);
  WITH Buf[BufPtr] DO
    layer:=Layer;
    size:=Size;
    vias:=ViasSize;
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
      IF StrongSide(x1,y1,x2,y2,x,y)=0 THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x2:=x; y2:=y
        ELSE x1:=x; y1:=y; vias:=0 END;
      END;
(* 2 *)
      X3:=LineXe; Y3:=LineYe; X4:=LineXe; Y4:=LineYs;
      Point(x,y);
      IF StrongSide(x1,y1,x2,y2,x,y)=0 THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x2:=x; y2:=y
        ELSE x1:=x; y1:=y; vias:=0 END;
      END;
(* 3 *)
      X3:=LineXe; Y3:=LineYe; X4:=LineXs; Y4:=LineYe;
      Point(x,y);
      IF StrongSide(x1,y1,x2,y2,x,y)=0 THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x2:=x; y2:=y
        ELSE x1:=x; y1:=y; vias:=0 END;
      END;
(* 4 *)
      X3:=LineXs; Y3:=LineYs; X4:=LineXs; Y4:=LineYe;
      Point(x,y);
      IF StrongSide(x1,y1,x2,y2,x,y)=0 THEN
        IF StrongSide(x,y,x1,y1,x2,y2)<0 THEN x2:=x; y2:=y
        ELSE x1:=x; y1:=y; vias:=0 END;
      END;
    END;
    DEC(x1,LineXs); DEC(x2,LineXs);
    DEC(y1,LineYs); DEC(y2,LineYs);
    IF (x1=x2)&(y1=y2)&(vias=0) THEN RETURN FALSE END;
  END;
  INC(BufPtr);
  RETURN FALSE;
END Seek;

PROCEDURE DefineMacro(o: Object; x1,y1,x2,y2: CARDINAL);
BEGIN
  BufPtr:=0;
  IF o=NIL THEN RETURN END;
  IF (x1>=x2)OR(y1>=y2) THEN RETURN END;
  LineXs:=x1; LineXe:=x2; LineYs:=y1; LineYe:=y2;
  SeekInBox(o,Seek);
END DefineMacro;

PROCEDURE DeleteBox(o: Object; xs,ys,xe,ye: CARDINAL);
  VAR i,l: INTEGER;
BEGIN
  REPEAT
    BufPtr:=0;
    IF o=NIL THEN RETURN END;
    IF (xs>=xe)OR(ys>=ye) THEN RETURN END;
    LineXs:=xs; LineXe:=xe; LineYs:=ys; LineYe:=ye;
    SeekInBox(o,Seek);
    FOR i:=0 TO BufPtr-1 DO
      WITH Buf[i] DO
        IF vias>0 THEN
          LineXs:=x1; LineXe:=x1; LineYs:=y1; LineYe:=y1; DeleteVias(s);
        END;
        IF (x1#x2)OR(y1#y2) THEN
          LineXs:=x1; LineXe:=x2; LineYs:=y1; LineYe:=y2;
          FOR l:=0 TO 7 DO IF l IN layer THEN DeleteRange(l,s) END END;
        END;
      END;
    END;
  UNTIL BufPtr=0;
END DeleteBox;

PROCEDURE InsertMacro(x,y: CARDINAL; mdl: Object);
  VAR i,l: CARDINAL; b: BOOLEAN;
BEGIN
  FOR i:=0 TO BufPtr-1 DO
    WITH Buf[i] DO
      LineXs:=x+x1; LineXe:=x+x2; LineYs:=y+y1; LineYe:=y+y2;
      IF (x1=x2)&(y1=y2) THEN
        b:=InsertVias(size,vias,TRUE,mdl,s);
      ELSE
        IF 0 IN layer THEN l:=0 ELSE l:=1 END;
        b:=InsertRange(size,l,TRUE,mdl,s);
      END;
    END;
  END;
END InsertMacro;

PROCEDURE InsertMetalMacro(x,y: INTEGER; mdl: Object);
  VAR i,l,cnt,cnt1: CARDINAL; b: BOOLEAN;
BEGIN
  FOR i:=0 TO BufPtr-1 DO Buf[i].s1:=null END; cnt:=BufPtr;
  WHILE cnt>0 DO
    cnt1:=cnt;
    FOR i:=0 TO BufPtr-1 DO
      WITH Buf[i] DO
        FOR l:=0 TO 1 DO
          IF l IN layer THEN
            IF s1=null THEN
              LineXs:=x+x1; LineXe:=x+x2; LineYs:=y+y1; LineYe:=y+y2;
              IF Shoted?(size,l,mdl,null) THEN
                s1:=ShotedSignal;
                LineXs:=x+x1; LineXe:=x+x2; LineYs:=y+y1; LineYe:=y+y2;
                IF (x1=x2)&(y1=y2) THEN
                  b:=InsertVias(size,vias,TRUE,mdl,s1);
                ELSE
                  b:=InsertRange(size,l,TRUE,mdl,s1);
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

BEGIN
  BufPtr:=0; null:=NewObject(signal);
END pedMacro.
