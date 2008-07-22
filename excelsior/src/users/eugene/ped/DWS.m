IMPLEMENTATION MODULE DWS; (* Sem 16-Feb-87. (c) KRONOS *)

IMPORT SYSTEM;
IMPORT  defCodes;
IMPORT  ModelMisc;
IMPORT  pcb: Model;
IMPORT  mem: cdsHeap;
IMPORT  tls: pedTools;
IMPORT  tpl: pedTopology;

CONST ptdiv=4;


TYPE Vec =POINTER TO ARRAY [0..0] OF INTEGER;
     Line=POINTER TO ARRAY [0..0] OF CHAR;
     Matr=POINTER TO ARRAY [0..0] OF Line;
     Patr=POINTER TO ARRAY [0..0] OF SYSTEM.ADDRESS;
     Pws =POINTER TO ARRAY [0..0] OF Patr;

-----------------------------------------------------------------------

TYPE Dws = ARRAY [0..tpl.LayersNo*2+1] OF Matr;

VAR Buf: Dws;
    Ptr: Pws;

VAR X: ARRAY [0..31] OF INTEGER;

PROCEDURE Tag(o: pcb.Object): pcb.Objects;
CODE 0 defCodes.lxb END Tag;

PROCEDURE MOVE(from,to: SYSTEM.ADDRESS; n: INTEGER); CODE defCodes.move END MOVE;

PROCEDURE final;
BEGIN
(*
  S.print('\n\nWS   = %6d %6d\n',X[0],X[1]);
  S.print('PT   = %6d %6d %6d\n',X[2],X[3],X[4]);
  S.print('Busy = %6d %6d\n',    X[5],X[8]);
  S.print('Vias = %6d %6d\n',    X[6],X[9]);
  S.print('Pipe = %6d %6d\n',    X[7],X[10]);
  S.print('-----------------------------------------------\n');
  S.print('Mark = %6d\n',X[11]);
  S.print('Mark0= %6d\n',X[12]);
  S.print('BusyS= %6d\n',X[13]);
  S.print('BusyV= %6d\n',X[14]);
  S.print('PipeV= %6d\n',X[15]);
  S.print('FreeS= %6d\n',X[16]);
  S.print('FreeV= %6d\n',X[17]);
  S.print('Call = %6d\n',X[18]);
*)
END final;

(*$T-*)

PROCEDURE WS(x,y,l: INTEGER): INTEGER;
BEGIN INC(X[0]);
  RETURN ORD(Buf[l]^[y]^[x])
END WS;

PROCEDURE setWS(x,y,l: INTEGER; v: INTEGER);
BEGIN INC(X[1]);
  Buf[l]^[y]^[x]:=CHAR(v)
END setWS;

PROCEDURE PT(x,y: INTEGER): SYSTEM.ADDRESS;
BEGIN INC(X[2]);
  RETURN Ptr^[y DIV ptdiv]^[x DIV ptdiv]
END PT;

PROCEDURE setPT(x,y: INTEGER; v: SYSTEM.ADDRESS);
BEGIN INC(X[3]);
  Ptr^[y DIV ptdiv]^[x DIV ptdiv]:=v
END setPT;

PROCEDURE adrPT(x,y: INTEGER): SYSTEM.ADDRESS;
BEGIN INC(X[4]);
  RETURN SYSTEM.ADR(Ptr^[y DIV ptdiv]^[x DIV ptdiv])
END adrPT;

PROCEDURE Busy(x,y,l: INTEGER): BOOLEAN;
BEGIN INC(X[5]);
  RETURN BOOLEAN(Buf[tpl.LayersNo+l]^[y]^[x])
END Busy;

PROCEDURE setBusy(x,y,l: INTEGER; v: BOOLEAN);
BEGIN INC(X[8]);
  Buf[tpl.LayersNo+l]^[y]^[x]:=CHAR(v)
END setBusy;

PROCEDURE Vias(x,y: INTEGER): BOOLEAN;
BEGIN INC(X[6]);
  RETURN BOOLEAN(Buf[tpl.LayersNo*2]^[y]^[x])
END Vias;

PROCEDURE setVias(x,y: INTEGER; v: BOOLEAN);
BEGIN INC(X[9]);
  Buf[tpl.LayersNo*2]^[y]^[x]:=CHAR(v)
END setVias;

PROCEDURE Pipe(x,y: INTEGER): BITSET;
BEGIN INC(X[7]);
  RETURN BITSET(Buf[tpl.LayersNo*2+1]^[y]^[x])
END Pipe;

PROCEDURE setPipe(x,y: INTEGER; v: BITSET);
BEGIN INC(X[10]);
  Buf[tpl.LayersNo*2+1]^[y]^[x]:=CHAR(v)
END setPipe;

-----------------------------------------------------------------------

CONST mark=fixed+tpl.LayersNo;

VAR Created: BOOLEAN;
    Xsz: INTEGER;
    ptY,ptX: INTEGER;

PROCEDURE CreateDWS(o: pcb.Object);
  VAR l,i,j: INTEGER; a: Vec;
VAR w: INTEGER;
BEGIN
w:=0;
  IF Created THEN RemoveDWS END;
  ASSERT(Tag(o)=pcb.chiptype);
  Xsize:=o^.ctX DIV tpl.Grid +1;
  Xsz:=(Xsize+3) DIV 4;
  Ysize:=o^.ctY DIV tpl.Grid +1;
  FOR l:=0 TO HIGH(Buf) DO
    mem.Allocate(Buf[l],Ysize);
INC(w,Ysize);
    FOR j:=0 TO Ysize-1 DO
      mem.Allocate(Buf[l]^[j],Xsz); a:=Vec(Buf[l]^[j]);
INC(w,Xsz);
      FOR i:=0 TO Xsz-1 DO a^[i]:=0 END;
    END;
  END;
  ptX:=(Xsize+ptdiv-1) DIV ptdiv;
  ptY:=(Ysize+ptdiv-1) DIV ptdiv;
  mem.Allocate(Ptr,ptY);
INC(w,ptY);
  FOR j:=0 TO ptY-1 DO
    mem.Allocate(Ptr^[j],ptX);
INC(w,ptX);
    FOR i:=0 TO ptX-1 DO Ptr^[j]^[i]:=NIL END;
  END;
  Created:=TRUE;

--S.print('DWS size = %d\n',w);
END CreateDWS;

PROCEDURE RemoveDWS;
  VAR l,i: INTEGER;
BEGIN
  IF NOT Created THEN RETURN END;
  Created:=FALSE;
  FOR i:=0 TO ptY-1 DO mem.Deallocate(Ptr^[i],ptX) END;
  mem.Deallocate(Ptr,ptY);
  FOR l:=0 TO HIGH(Buf) DO
    FOR i:=0 TO Ysize-1 DO mem.Deallocate(Buf[l]^[i],Xsz) END;
    mem.Deallocate(Buf[l],Ysize);
  END;
  final;
END RemoveDWS;

PROCEDURE CleareWaves;
  VAR i,j,l: INTEGER; a: Vec;
BEGIN
  FOR l:=0 TO tpl.LayersNo-1 DO
    FOR j:=0 TO Ysize-1 DO
      a:=Vec(Buf[l]^[j]); a^[0]:=0; MOVE(INTEGER(a)+1,a,Xsz-1);
    END;
  END;
  FOR j:=0 TO ptY-1 DO
    Ptr^[j]^[0]:=NIL;
    MOVE(SYSTEM.ADR(Ptr^[j]^[0])+1,SYSTEM.ADR(Ptr^[j]^[0]),ptX-1);
  END;
END CleareWaves;
(**)

-----------------------------------------------------------------------

TYPE it=PROCEDURE (INTEGER,INTEGER,INTEGER);

VAR Grid2   : INTEGER;
    delta1  : INTEGER;
    delta2  : INTEGER;
    defX1,defX2,defY1,defY2: INTEGER;
    Signal  : pcb.Object;
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
  sz:=ModelMisc.Size+delta; sz2:=sz*sz;
  minX:=Min(X1,X2); maxX:=Max(X1,X2);
  minY:=Min(Y1,Y2); maxY:=Max(Y1,Y2);
  IF (X1#X2)&(Y1=Y2)OR(Y1#Y2)&(X1=X2) THEN
    IF X1=X2 THEN
      xb1:=(X1  -sz+(tpl.Grid-1)) DIV tpl.Grid; xb2:=(X1  +sz) DIV tpl.Grid;
      yb1:=(minY-sz+(tpl.Grid-1)) DIV tpl.Grid; yb2:=(maxY+sz) DIV tpl.Grid;
      IF xb1<0 THEN xb1:=0 END;
      IF yb1<0 THEN yb1:=0 END;
      IF xb2>=Xsize THEN xb2:=Xsize-1 END;
      IF yb2>=Ysize THEN yb2:=Ysize-1 END;
      FOR y:=yb1 TO yb2 DO
        IF (y*tpl.Grid>=minY)&(y*tpl.Grid<=maxY) THEN
          FOR x:=xb1 TO xb2 DO
            IF {0}*ModelMisc.Layer#{} THEN p(x,y,0) END;
            IF {1}*ModelMisc.Layer#{} THEN p(x,y,1) END;
          END;
        ELSE
          FOR x:=xb1 TO xb2 DO
            IF tpl.Len(X1,Y1,X2,Y2,x*tpl.Grid,y*tpl.Grid)<=sz2 THEN
              IF {0}*ModelMisc.Layer#{} THEN p(x,y,0) END;
              IF {1}*ModelMisc.Layer#{} THEN p(x,y,1) END;
            END;
          END;
        END;
      END;
    ELSE
      xb1:=(minX-sz+(tpl.Grid-1)) DIV tpl.Grid; xb2:=(maxX+sz) DIV tpl.Grid;
      yb1:=(Y1  -sz+(tpl.Grid-1)) DIV tpl.Grid; yb2:=(Y1  +sz) DIV tpl.Grid;
      IF xb1<0 THEN xb1:=0 END;
      IF yb1<0 THEN yb1:=0 END;
      IF xb2>=Xsize THEN xb2:=Xsize-1 END;
      IF yb2>=Ysize THEN yb2:=Ysize-1 END;
      FOR x:=xb1 TO xb2 DO
        IF (x*tpl.Grid>=minX)&(x*tpl.Grid<=maxX) THEN
          FOR y:=yb1 TO yb2 DO
            IF {0}*ModelMisc.Layer#{} THEN p(x,y,0) END;
            IF {1}*ModelMisc.Layer#{} THEN p(x,y,1) END;
          END;
        ELSE
          FOR y:=yb1 TO yb2 DO
            IF tpl.Len(X1,Y1,X2,Y2,x*tpl.Grid,y*tpl.Grid)<=sz2 THEN
              IF {0}*ModelMisc.Layer#{} THEN p(x,y,0) END;
              IF {1}*ModelMisc.Layer#{} THEN p(x,y,1) END;
            END;
          END;
        END;
      END;
    END;
    RETURN
  END;
  xb1:=(Min(X1,X2)-sz+(tpl.Grid-1)) DIV tpl.Grid;
  xb2:=(Max(X1,X2)+sz) DIV tpl.Grid;
  yb1:=(Min(Y1,Y2)-sz+(tpl.Grid-1)) DIV tpl.Grid;
  yb2:=(Max(Y1,Y2)+sz) DIV tpl.Grid;
  FOR x:=xb1 TO xb2 DO FOR y:=yb1 TO yb2 DO
      IF tpl.Len(X1,Y1,X2,Y2,x*tpl.Grid,y*tpl.Grid)<=sz2 THEN
        FOR l:=0 TO tpl.LayersNo-1 DO
          IF (l IN ModelMisc.Layer)&(x>=0)&(x<Xsize)&(y>=0)&(y<Ysize) THEN
            p(x,y,l)
          END
        END;
      END;
  END; END;
END Pnt;

PROCEDURE Points(o: pcb.Object; p: it; d,dfx,dfy: INTEGER);
BEGIN
  ModelMisc.StartConductor(o,FALSE);
  WHILE NOT ModelMisc.Empty DO Pnt(p,d,dfx,dfy); ModelMisc.NextConductor; END;
END Points;

PROCEDURE Via(o: pcb.Object; p: it);
  VAR x,y: INTEGER;
BEGIN
  ModelMisc.StartConductor(o,FALSE);
  WHILE NOT ModelMisc.Empty DO
    IF ModelMisc.ViasSize>0 THEN
      x:=ModelMisc.X1 DIV tpl.Grid; y:=ModelMisc.Y1 DIV tpl.Grid;
      IF (x>=0)&(x<Xsize)&(y>=0)&(y<Ysize) THEN p(x,y,0) END;
    END;
    ModelMisc.NextConductor;
  END;
END Via;

PROCEDURE Mark(x,y,l: INTEGER);
BEGIN setPipe(x,y,Pipe(x,y)+{mark+l})
END Mark;

PROCEDURE Mark0(x,y,l: INTEGER);
BEGIN setPipe(x,y,Pipe(x,y)+{mark})
END Mark0;

PROCEDURE BusyS(x,y,l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (mark+l) IN Pipe(x,y) THEN
    setPipe(x,y,Pipe(x,y)-{mark+l});
    i:=INTEGER(Busy(x,y,l))+1; setBusy(x,y,l,BOOLEAN(i));
    IF ModelMisc.Fixed THEN setPipe(x,y,Pipe(x,y)+{fixed+l}) END;
  END;
END BusyS;

PROCEDURE BusyV(x,y,l: INTEGER);
BEGIN
  IF mark IN Pipe(x,y) THEN
    setPipe(x,y,Pipe(x,y)-{mark});
    setVias(x,y,BOOLEAN(INTEGER(Vias(x,y))+1));
    IF ModelMisc.Fixed THEN setPipe(x,y,Pipe(x,y)+{fixedvias}) END;
  END;
END BusyV;

PROCEDURE PipeVias(x,y,l: INTEGER);
BEGIN setPipe(x,y,Pipe(x,y)+{vias});
END PipeVias;

PROCEDURE FreeS(x,y,l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (mark+l) IN Pipe(x,y) THEN
    setPipe(x,y,Pipe(x,y)-{mark+l});
    i:=INTEGER(Busy(x,y,l))-1; setBusy(x,y,l,BOOLEAN(i));
  END;
END FreeS;

PROCEDURE FreeV(x,y,l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF mark IN Pipe(x,y) THEN
    setPipe(x,y,Pipe(x,y)-{mark});
    setVias(x,y,BOOLEAN(INTEGER(Vias(x,y))-1));
  END;
END FreeV;

PROCEDURE Call(x,y,l: INTEGER);
BEGIN
  IF (mark+l) IN Pipe(x,y) THEN
    setPipe(x,y,Pipe(x,y)-{mark+l});
    UnPackProc(x,y,l);
  END;
END Call;

-----------------------------------------------------------------------

TYPE unit=RECORD cnt,ptr: INTEGER; sig: pcb.Object END;

VAR svx,svy: ARRAY [0..299] OF INTEGER;
    svcnt,svptr: INTEGER;
    stk    : ARRAY [0..9] OF unit;
    stkptr : INTEGER;

PROCEDURE SaveVia(x,y,l: INTEGER);
BEGIN
  IF NOT ModelMisc.Fixed THEN
    IF svcnt>HIGH(svx) THEN HALT(1) END;
    svx[svcnt]:=x; svy[svcnt]:=y; INC(svcnt);
  END;
END SaveVia;

PROCEDURE InsertSignal(o: pcb.Object; info: INTEGER);
BEGIN
  CASE Tag(o) OF
    |pcb.signal:
        Points(o,Mark,delta1,defX1,defY1);
        Points(o,BusyS,delta1,defX1,defY1);
        Points(o,Mark0,delta2,defX2,defY2);
        Points(o,BusyV,delta2,defX2,defY2);
        Via(o,PipeVias);
   |pcb.chiptype:
        pcb.Iterate(o^.All,InsertSignal,0);
  ELSE
  END;
END InsertSignal;

PROCEDURE UnPackSignal(o: pcb.Object; p: IterProc);
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
    setPipe(svx[i],svy[i],Pipe(svx[i],svy[i])-{vias});
  END;
  InsertSignal(Signal,0);
  DEC(stkptr);
  WITH stk[stkptr] DO
    svcnt:=cnt; svptr:=ptr; Signal:=sig;
  END;
  IF stkptr=0 THEN CleareWaves END;
END PackSignal;

PROCEDURE InitDWS(o: pcb.Object; t: INTEGER);
BEGIN
  delta1:=tls.Tools[t].Size+ tpl.Clearens;
  delta2:=tls.Tools[t].VSize+tpl.Clearens;
  defX1 :=tls.Tools[t].DefX;
  defY1 :=tls.Tools[t].DefY;
  defX2 :=tls.Tools[t].VDefX;
  defY2 :=tls.Tools[t].VDefY;
  CreateDWS(o); InsertSignal(o,0);
END InitDWS;

VAR i: INTEGER;

BEGIN
  Created:=FALSE; Grid2:=tpl.Grid DIV 2;
  stkptr:=0; svcnt:=0; svptr:=0; Signal:=NIL;

  FOR i:=0 TO HIGH(X) DO X[i]:=0 END;
END DWS.

DWS size = 8528
