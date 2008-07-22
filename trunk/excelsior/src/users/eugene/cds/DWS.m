IMPLEMENTATION MODULE DWS; (* Sem 16-Feb-87. (c) KRONOS *)

IMPORT ModelMisc;
FROM SYSTEM      IMPORT ADR, ADDRESS;
FROM KRONOS      IMPORT MOVE;
FROM Model       IMPORT Iterate, Tag, Objects, Object;
FROM cdsHeap     IMPORT Allocate, Deallocate;
FROM ModelMisc   IMPORT StartConductor, NextConductor, Empty,
                        Layer, Size, ViasSize, Fixed;
FROM pedTools    IMPORT Tools;
FROM pedPbl      IMPORT Clearens, MaxLayers, LayersNo, Grid;
FROM pedTopology IMPORT Len;
FROM mCodeMnem   IMPORT lxw, stot, lodt, lxb, sxb, add, shr;

(*$T-*)

TYPE Vec =POINTER TO ARRAY [0..0] OF INTEGER;
     Line=POINTER TO ARRAY [0..0] OF CHAR;
     Matr=POINTER TO ARRAY [0..0] OF Line;
     Patr=POINTER TO ARRAY [0..0] OF ADDRESS;
     Pws =POINTER TO ARRAY [0..0] OF Patr;

CONST mark=fixed+LayersNo;

VAR Created: BOOLEAN;
    Xsz: INTEGER;
    ptY,ptX: INTEGER;

PROCEDURE CreateDWS(o: Object);
  VAR l,i,j: INTEGER; a: Vec;
BEGIN
  IF Created THEN RemoveDWS END;
  ASSERT(Tag(o)=chiptype);
  Xsize:=o^.ctX DIV Grid +1;
  Xsz:=(Xsize+3) DIV 4;
  Ysize:=o^.ctY DIV Grid +1;
  FOR l:=0 TO HIGH(Buf) DO
    Allocate(Buf[l],Ysize);
    FOR j:=0 TO Ysize-1 DO
      Allocate(Buf[l]^[j],Xsz); a:=Vec(Buf[l]^[j]);
      FOR i:=0 TO Xsz-1 DO a^[i]:=0 END;
    END;
  END;
  ptX:=(Xsize+1)DIV 2;
  ptY:=(Ysize+1)DIV 2;
  Allocate(Ptr,ptY);
  FOR j:=0 TO ptY-1 DO
    Allocate(Ptr^[j],ptX);
    FOR i:=0 TO ptX-1 DO Ptr^[j]^[i]:=NIL END;
  END;
  Created:=TRUE;
END CreateDWS;

PROCEDURE RemoveDWS;
  VAR l,i: INTEGER;
BEGIN
  IF NOT Created THEN RETURN END;
  Created:=FALSE;
  FOR i:=0 TO ptY-1 DO Deallocate(Ptr^[i],ptX) END;
  Deallocate(Ptr,ptY);
  FOR l:=0 TO HIGH(Buf) DO
    FOR i:=0 TO Ysize-1 DO Deallocate(Buf[l]^[i],Xsz) END;
    Deallocate(Buf[l],Ysize);
  END;
END RemoveDWS;

PROCEDURE CleareWaves;
  VAR i,j,l: INTEGER; a: Vec;
BEGIN
  FOR l:=0 TO LayersNo-1 DO
    FOR j:=0 TO Ysize-1 DO
      a:=Vec(Buf[l]^[j]); a^[0]:=0; MOVE(ADR(a^[1]),a,Xsz-1);
    END;
  END;
  FOR j:=0 TO ptY-1 DO
    Ptr^[j]^[0]:=NIL;
    MOVE(ADR(Ptr^[j]^[1]),ADR(Ptr^[j]^[0]),ptX-1);
  END;
END CleareWaves;

PROCEDURE setPipe(x,y: INTEGER; Buf: Dws; v: BITSET);
CODE stot LayersNo*2+1 lxw lxw swap lodt sxb END setPipe;

PROCEDURE setBusy(x,y,l: INTEGER; VAR Buf: Dws; v: BOOLEAN);
CODE stot LayersNo add lxw lxw swap lodt sxb END setBusy;

PROCEDURE setVias(x,y: INTEGER; VAR Buf: Dws; v: BOOLEAN);
CODE stot LayersNo*2 lxw lxw swap lodt sxb END setVias;

TYPE it=PROCEDURE (INTEGER,INTEGER,INTEGER);

VAR Grid2   : INTEGER;
    delta1  : INTEGER;
    delta2  : INTEGER;
    defX1,defX2,defY1,defY2: INTEGER;
    Signal  : Object;
    UnPackProc: it;

PROCEDURE Min(x,y: INTEGER): INTEGER;
BEGIN IF x<y THEN RETURN x ELSE RETURN y END END Min;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;

PROCEDURE Pnt(p: it; delta,dfx,dfy: INTEGER);
  VAR xb1,yb1,xb2,yb2,sz,sz2,x,y,l,X1,X2,Y1,Y2: INTEGER;
      minX,maxX,minY,maxY: INTEGER;
BEGIN
  X1:=ModelMisc.X1-dfx; X2:=ModelMisc.X2-dfx;
  Y1:=ModelMisc.Y1-dfy; Y2:=ModelMisc.Y2-dfy;
  sz:=Size+delta; sz2:=sz*sz;
  minX:=Min(X1,X2); maxX:=Max(X1,X2);
  minY:=Min(Y1,Y2); maxY:=Max(Y1,Y2);
  IF (X1#X2)&(Y1=Y2)OR(Y1#Y2)&(X1=X2) THEN
    IF X1=X2 THEN
      xb1:=(X1-sz+(Grid-1)) DIV Grid; xb2:=(X1+sz) DIV Grid;
      yb1:=(minY-sz+(Grid-1)) DIV Grid; yb2:=(maxY+sz) DIV Grid;
      IF xb1<0 THEN xb1:=0 END;
      IF yb1<0 THEN yb1:=0 END;
      IF xb2>=Xsize THEN xb2:=Xsize-1 END;
      IF yb2>=Ysize THEN yb2:=Ysize-1 END;
      FOR y:=yb1 TO yb2 DO
        IF (y*Grid>=minY)&(y*Grid<=maxY) THEN
          FOR x:=xb1 TO xb2 DO
            IF {0}*Layer#{} THEN p(x,y,0) END;
            IF {1}*Layer#{} THEN p(x,y,1) END;
          END;
        ELSE
          FOR x:=xb1 TO xb2 DO
            IF Len(X1,Y1,X2,Y2,x*Grid,y*Grid)<=sz2 THEN
              IF {0}*Layer#{} THEN p(x,y,0) END;
              IF {1}*Layer#{} THEN p(x,y,1) END;
            END;
          END;
        END;
      END;
    ELSE
      xb1:=(minX-sz+(Grid-1)) DIV Grid; xb2:=(maxX+sz) DIV Grid;
      yb1:=(Y1-sz+(Grid-1)) DIV Grid; yb2:=(Y1+sz) DIV Grid;
      IF xb1<0 THEN xb1:=0 END;
      IF yb1<0 THEN yb1:=0 END;
      IF xb2>=Xsize THEN xb2:=Xsize-1 END;
      IF yb2>=Ysize THEN yb2:=Ysize-1 END;
      FOR x:=xb1 TO xb2 DO
        IF (x*Grid>=minX)&(x*Grid<=maxX) THEN
          FOR y:=yb1 TO yb2 DO
            IF {0}*Layer#{} THEN p(x,y,0) END;
            IF {1}*Layer#{} THEN p(x,y,1) END;
          END;
        ELSE
          FOR y:=yb1 TO yb2 DO
            IF Len(X1,Y1,X2,Y2,x*Grid,y*Grid)<=sz2 THEN
              IF {0}*Layer#{} THEN p(x,y,0) END;
              IF {1}*Layer#{} THEN p(x,y,1) END;
            END;
          END;
        END;
      END;
    END;
    RETURN
  END;
  xb1:=(Min(X1,X2)-sz+(Grid-1)) DIV Grid; xb2:=(Max(X1,X2)+sz) DIV Grid;
  yb1:=(Min(Y1,Y2)-sz+(Grid-1)) DIV Grid; yb2:=(Max(Y1,Y2)+sz) DIV Grid;
  FOR x:=xb1 TO xb2 DO FOR y:=yb1 TO yb2 DO
      IF Len(X1,Y1,X2,Y2,x*Grid,y*Grid)<=sz2 THEN
        FOR l:=0 TO LayersNo-1 DO
          IF (l IN Layer)&(x>=0)&(x<Xsize)&(y>=0)&(y<Ysize) THEN p(x,y,l) END
        END;
      END;
  END; END;
END Pnt;

PROCEDURE Points(o: Object; p: it; d,dfx,dfy: INTEGER);
BEGIN
  StartConductor(o);
  WHILE NOT Empty DO Pnt(p,d,dfx,dfy); NextConductor; END;
END Points;

PROCEDURE Via(o: Object; p: it);
  VAR x,y: INTEGER;
BEGIN
  StartConductor(o);
  WHILE NOT Empty DO
    IF ViasSize>0 THEN
      x:=ModelMisc.X1 DIV Grid; y:=ModelMisc.Y1 DIV Grid;
      IF (x>=0)&(x<Xsize)&(y>=0)&(y<Ysize) THEN p(x,y,0) END;
    END;
    NextConductor;
  END;
END Via;

PROCEDURE Mark(x,y,l: INTEGER);
BEGIN setPipe(x,y,Buf,Pipe(x,y,Buf)+{mark+l}) END Mark;

PROCEDURE Mark0(x,y,l: INTEGER);
BEGIN setPipe(x,y,Buf,Pipe(x,y,Buf)+{mark}) END Mark0;

PROCEDURE BusyS(x,y,l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (mark+l) IN Pipe(x,y,Buf) THEN
    setPipe(x,y,Buf,Pipe(x,y,Buf)-{mark+l});
    i:=INTEGER(Busy(x,y,l,Buf))+1; setBusy(x,y,l,Buf,BOOLEAN(i));
    IF Fixed THEN setPipe(x,y,Buf,Pipe(x,y,Buf)+{fixed+l}) END;
  END;
END BusyS;

PROCEDURE BusyV(x,y,l: INTEGER);
BEGIN
  IF mark IN Pipe(x,y,Buf) THEN
    setPipe(x,y,Buf,Pipe(x,y,Buf)-{mark});
    setVias(x,y,Buf,BOOLEAN(INTEGER(Vias(x,y,Buf))+1));
    IF Fixed THEN setPipe(x,y,Buf,Pipe(x,y,Buf)+{fixedvias}) END;
  END;
END BusyV;

PROCEDURE PipeVias(x,y,l: INTEGER);
BEGIN
  setPipe(x,y,Buf,Pipe(x,y,Buf)+{vias});
END PipeVias;

PROCEDURE FreeS(x,y,l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (mark+l) IN Pipe(x,y,Buf) THEN
    setPipe(x,y,Buf,Pipe(x,y,Buf)-{mark+l});
    i:=INTEGER(Busy(x,y,l,Buf))-1; setBusy(x,y,l,Buf,BOOLEAN(i));
  END;
END FreeS;

PROCEDURE FreeV(x,y,l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF mark IN Pipe(x,y,Buf) THEN
    setPipe(x,y,Buf,Pipe(x,y,Buf)-{mark});
    setVias(x,y,Buf,BOOLEAN(INTEGER(Vias(x,y,Buf))-1));
  END;
END FreeV;

PROCEDURE Call(x,y,l: INTEGER);
BEGIN
  IF (mark+l) IN Pipe(x,y,Buf) THEN
    setPipe(x,y,Buf,Pipe(x,y,Buf)-{mark+l});
    UnPackProc(x,y,l);
  END;
END Call;

TYPE unit=RECORD cnt,ptr: INTEGER; sig: Object END;

VAR svx,svy: ARRAY [0..299] OF INTEGER;
    svcnt,svptr: INTEGER;
    stk    : ARRAY [0..9] OF unit;
    stkptr : INTEGER;

PROCEDURE SaveVia(x,y,l: INTEGER);
BEGIN
  IF NOT Fixed THEN
    IF svcnt>HIGH(svx) THEN HALT(1) END;
    svx[svcnt]:=x; svy[svcnt]:=y; INC(svcnt);
  END;
END SaveVia;

PROCEDURE InsertSignal(o: Object);
BEGIN
  CASE Tag(o) OF
    signal:
        Points(o,Mark,delta1,defX1,defY1);
        Points(o,BusyS,delta1,defX1,defY1);
        Points(o,Mark0,delta2,defX2,defY2);
        Points(o,BusyV,delta2,defX2,defY2);
        Via(o,PipeVias);
   |chiptype:
        Iterate(o^.All,InsertSignal);
  ELSE
  END;
END InsertSignal;

PROCEDURE UnPackSignal(o: Object; p: IterProc);
BEGIN
  WITH stk[stkptr] DO
    cnt:=svcnt; ptr:=svptr; sig:=Signal; svptr:=svcnt;
  END;
  INC(stkptr);
  Signal:=o; UnPackProc:=p;
  Points(o,Mark,delta1,defX1,defY1);
  Points(o,FreeS,delta1,defX1,defY1);
  Points(o,Mark0,delta2,defX2,defY2);
  Points(o,FreeV,delta2,defX2,defY2);
  Via(o,SaveVia);
  Points(o,Mark,0,defX1,defY1);
  Points(o,Call,0,defX1,defY1);
END UnPackSignal;

PROCEDURE PackSignal;
  VAR i: INTEGER;
BEGIN
  FOR i:=svptr TO svcnt-1 DO
    setPipe(svx[i],svy[i],Buf,Pipe(svx[i],svy[i],Buf)-{vias});
  END;
  InsertSignal(Signal);
  DEC(stkptr);
  WITH stk[stkptr] DO
    svcnt:=cnt; svptr:=ptr; Signal:=sig;
  END;
  IF stkptr=0 THEN CleareWaves END;
END PackSignal;

PROCEDURE InitDWS(o: Object; t: INTEGER);
BEGIN
  delta1:=Tools[t].Size+Clearens;
  delta2:=Tools[t].VSize+Clearens;
  defX1 :=Tools[t].DefX;
  defY1 :=Tools[t].DefY;
  defX2 :=Tools[t].VDefX;
  defY2 :=Tools[t].VDefY;
  CreateDWS(o); InsertSignal(o);
END InitDWS;

BEGIN
  Created:=FALSE; Grid2:=Grid DIV 2;
  stkptr:=0; svcnt:=0; svptr:=0; Signal:=NIL;
END DWS.
