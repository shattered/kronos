IMPLEMENTATION MODULE Layout; (* Sem 12-Feb-87. (c) KRONOS *)

FROM pedScreen   IMPORT Text, Cursor;
FROM pedTools    IMPORT Tools;
FROM pedTopology IMPORT InsertVias, InsertRange, FindSignal, Areas,
                        LineXs, LineXe, LineYs, LineYe, Del, Len, SeekInBox,
                        LayersNo, Grid, Clearens;
FROM Points      IMPORT NewPoint, pPoint, FindPoint, FindOther, FindMin,
                        Cat, KilLastPoint, RemovePoints, NewIdent,
                        DeallocPoints, NewVeight;
FROM DWS         IMPORT mLup, mLdown, mE, mW, mN, mS, mSE, mSW, mNE, mNW,
                        free, start, front, finish, UnPackSignal, PackSignal,
                        WS, setWS, Vias, Busy, Pipe, Xsize, Ysize,
                        fixed, fixedvias, vias;
FROM Model       IMPORT Object, fantom;
FROM ModelPbl    IMPORT Exception?, Reaction, KillReaction, RaiseInMe,
                        MemoryOverflow;
IMPORT ModelMisc; --, RoutDeb;
FROM ModelMisc  IMPORT  StartConductor, NextConductor, Empty, Fixed,
                        Layer, X1, X2, Y1, Y2, Size, FindConductor;
FROM cdsImp     IMPORT  Bril;
FROM pedEditor  IMPORT  Sheet;

VAR Evr : ARRAY [0..31] OF INTEGER;
    Tool: INTEGER;
    Mdl     : Object;
    Signal  : Object;
    NewCrsXs, NewCrsYs: INTEGER;
    NewCrsXe, NewCrsYe: INTEGER;
    V: INTEGER;
    sht   : Sheet;

CONST
  Ea= 1;  (* -  *)
  Eb=31;  (* /  *)
  Ec= 8;  (* |  *)
  Ed=20;  (* o  *)
  Ebusy=100;

PROCEDURE Weight(x,y,l,d: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
-- если слой 1 то развернем на 90 градусов:
  IF l=1 THEN
    IF d IN {mE..mSE} THEN d:=(10-(d-mE)) MOD 8 +mE END;
  END;

-- направление сегмента:
  i:=Evr[d];
  IF (d IN {mN,mS}) & (V<4) THEN INC(i,4-V) END;

  IF d IN {mLup,mLdown} THEN
    IF Vias(x,y) THEN INC(i,Ebusy) END;
  ELSE
    IF Busy(x,y,l) THEN INC(i,Ebusy) END;
  END;
  RETURN i
END Weight;

VAR
    Ident,
    IdentCnt: INTEGER;
    Finished: BOOLEAN;
    FinishA,
    FinishB : pPoint;
    FinishV,
    minFinV : INTEGER;
    X,Y,L,
    D,ID    : INTEGER;

PROCEDURE WSfff(x,y,l: INTEGER): BOOLEAN;
BEGIN
  RETURN (WS(x,y,l) IN {free,front,finish}) &
         NOT (Busy(x,y,l) & ((fixed+l) IN Pipe(x,y)));
END WSfff;

PROCEDURE Correct(x,y,l,d: INTEGER);
  VAR v: INTEGER; p: pPoint;
BEGIN
  IF (l#L)& Vias(x,y) & (fixedvias IN Pipe(x,y)) THEN RETURN END;
  v:=V+Weight(x,y,l,d); p:=FindPoint(x,y,l,ID);
  IF p=NIL THEN
    NewPoint(x,y,l,v,d,ID,FALSE,sht);
    IF WS(x,y,l)=free THEN setWS(x,y,l,front) ELSE setWS(x,y,l,finish) END;
--  RoutDeb.DebPnt(x,y,l,v,d);
  ELSIF p^.v>v THEN
    ASSERT(NOT p^.stopped);
    NewVeight(p,v); p^.d:=d;
--  RoutDeb.DebPnt(x,y,l,v,d);
  END;
END Correct;

PROCEDURE ComeForward;
  VAR d: INTEGER; m: pPoint;
BEGIN
  ASSERT(FinishA^.stopped & FinishB^.stopped);
  Ident:=FinishA^.id;
  X:=FinishA^.x; Y:=FinishA^.y;
  L:=FinishA^.l; D:=FinishA^.d;
  ID:=FinishB^.id;
  setWS(X,Y,L,D);
  d:=FinishB^.d;
  LOOP
    CASE d OF
       start : DEC(IdentCnt); RETURN;
      |mE    : DEC(X);         D:=mW;
      |mNE   : DEC(X); DEC(Y); D:=mSW;
      |mN    :         DEC(Y); D:=mS;
      |mNW   : INC(X); DEC(Y); D:=mSE;
      |mW    : INC(X);         D:=mE;
      |mSW   : INC(X); INC(Y); D:=mNE;
      |mS    :         INC(Y); D:=mN;
      |mSE   : DEC(X); INC(Y); D:=mNW;
      |mLup  : DEC(L); D:=mLdown;
      |mLdown: INC(L); D:=mLup;
    END;
    d:=WS(X,Y,L);
    IF d=finish THEN m:=FindPoint(X,Y,L,ID); ASSERT(m#NIL); d:=m^.d END;
    setWS(X,Y,L,D);
  END;
END ComeForward;

PROCEDURE MoveWave;
  TYPE s=BITSET;
  VAR d,cnt,Xsize1,Ysize1,fv: INTEGER; m,p: pPoint;
BEGIN
  Finished:=FALSE; minFinV:=100000000;
  Xsize1:=Xsize-1; Ysize1:=Ysize-1;
  LOOP
    m:=FindMin();
    IF Finished & ((m=NIL) OR (FinishV<minFinV+m^.v)) THEN
      ComeForward; EXIT
    END;
    IF m=NIL THEN EXIT END;
    X:=m^.x; Y:=m^.y; L:=m^.l; V:=m^.v; ID:=m^.id; D:=m^.d;
    IF WS(X,Y,L)=finish THEN
      p:=FindOther(X,Y,L,ID);
      ASSERT(p#NIL);
      IF p^.stopped THEN
        fv:=m^.v+p^.v;
        IF NOT Finished OR (fv<FinishV) THEN
          FinishA:=m; FinishB:=p; FinishV:=fv; Finished:=TRUE;
        END;
      END;
      m^.stopped:=TRUE;
      IF minFinV>m^.v THEN minFinV:=m^.v END;
    ELSE
      ASSERT(D#finish);
      KilLastPoint; setWS(X,Y,L,D);
    END;
--  RoutDeb.DoPnt(X,Y,L,V,D);
    IF BOOLEAN(s(X>0)*s(X<Xsize1)*s(Y>0)*s(Y<Ysize1)) THEN
      IF WSfff(X-1,Y  ,L)   THEN Correct(X-1,Y  ,L,mW ) END;
      IF WSfff(X  ,Y-1,L)   THEN Correct(X  ,Y-1,L,mS ) END;
      IF WSfff(X  ,Y+1,L)   THEN Correct(X  ,Y+1,L,mN ) END;
      IF WSfff(X+1,Y  ,L)   THEN Correct(X+1,Y  ,L,mE ) END;
      IF (L=1)&WSfff(X,Y,0) THEN Correct(X  ,Y  ,0,mLdown) END;
      IF (L=0)&WSfff(X,Y,1) THEN Correct(X  ,Y  ,1,mLup) END;
    END;
  END;
END MoveWave;

PROCEDURE BreakIP(x,y,l: INTEGER);
BEGIN
END BreakIP;

VAR BrkSz: INTEGER;
    BrkLayer: BITSET;
    BrkX,BrkY: INTEGER;
    DelCnt: INTEGER;

PROCEDURE BreakIS(): BOOLEAN;
  VAR i,n: INTEGER;
BEGIN
  IF Fixed OR (Layer*BrkLayer={}) OR
       (Len(X1,Y1,X2,Y2,BrkX,BrkY)>(BrkSz+Size)*(BrkSz+Size)) THEN
    RETURN FALSE END;
  n:=ModelMisc.Signal^.sGang;
  i:=ModelMisc.Ident;
  UnPackSignal(ModelMisc.Signal,BreakIP);
  FindConductor(i); Del(sht); INC(DelCnt);
  PackSignal;
  ModelMisc.Signal^.sGang:=Areas(ModelMisc.Signal,sht);
  INC(TotalGang,ModelMisc.Signal^.sGang-n);
  RETURN FALSE;
END BreakIS;

PROCEDURE Break(x,y,l: INTEGER);
  VAR sz: INTEGER; ls: BITSET;
BEGIN
  INC(Signal^.sHard);
  IF l<0 THEN
    LineXs:=x*Grid+Tools[Tool].VDefX;
    LineYs:=y*Grid+Tools[Tool].VDefY;
    ls:={0,1}; sz:=Tools[Tool].VSize+Clearens+2;
  ELSE
    LineXs:=x*Grid+Tools[Tool].DefX;
    LineYs:=y*Grid+Tools[Tool].DefY;
    ls:={l}; sz:=Tools[Tool].Size+Clearens+2;
  END;
  BrkX:=LineXs; BrkY:=LineYs;
  LineXe:=LineXs+sz; LineYe:=LineYs+sz; DEC(LineXs,sz); DEC(LineYs,sz);
  BrkLayer:=ls; BrkSz:=sz;
  SeekInBox(sht,BreakIS);
END Break;

PROCEDURE ComeBack(): BOOLEAN;
  VAR  d,d1: INTEGER; p: pPoint; Xs,Ys,Ls: INTEGER;
BEGIN
  INCL(Signal^.sType,fantom); DelCnt:=0;
  d:=WS(X,Y,L);
  Xs:=X*Grid; Ys:=Y*Grid; Ls:=L;
  LOOP
    CASE d OF
      start : EXIT
     |mE    : DEC(X);
     |mNE   : DEC(X); DEC(Y);
     |mN    : DEC(Y);
     |mNW   : INC(X); DEC(Y);
     |mW    : INC(X);
     |mSW   : INC(X); INC(Y);
     |mS    : INC(Y);
     |mSE   : DEC(X); INC(Y);
     |mLup  : DEC(L);
     |mLdown: INC(L);
    END;
    d1:=WS(X,Y,L);
    IF d1=finish THEN p:=FindPoint(X,Y,L,Ident); d1:=p^.d END;
    IF (d1 IN {mLup,mLdown})& Vias(X,Y) THEN Break(X,Y,-1) END;
    IF Busy(X,Y,L) THEN Break(X,Y,L) END;
    d:=d1;
  END;
  IF DelCnt>0 THEN RETURN FALSE END;
  X:=Xs DIV Grid; Y:=Ys DIV Grid; L:=Ls;
  d:=WS(X,Y,L);
  LOOP
    CASE d OF
      start : EXIT
     |mE    : DEC(X);
     |mNE   : DEC(X); DEC(Y);
     |mN    : DEC(Y);
     |mNW   : INC(X); DEC(Y);
     |mW    : INC(X);
     |mSW   : INC(X); INC(Y);
     |mS    : INC(Y);
     |mSE   : DEC(X); INC(Y);
     |mLup  : DEC(L);
     |mLdown: INC(L);
    END;
    d1:=WS(X,Y,L);
    IF d1=finish THEN p:=FindPoint(X,Y,L,Ident); d1:=p^.d END;
    IF d1#d THEN
      LineXs:=Xs; LineYs:=Ys;
      LineXe:=X*24; LineYe:=Y*24;
      WITH Tools[Tool] DO
        IF d IN {mLup,mLdown} THEN
          INC(LineXs,VDefX); INC(LineXe,VDefX);
          INC(LineYs,VDefY); INC(LineYe,VDefY);
          IF InsertVias(VSize,DSize,FALSE,sht,Signal,TRUE) THEN END;
        ELSE
          INC(LineXs,DefX); INC(LineXe,DefX);
          INC(LineYs,DefY); INC(LineYe,DefY);
          IF InsertRange(Size,L,FALSE,sht,Signal,TRUE) THEN END;
        END;
      END;
      Xs:=X*Grid; Ys:=Y*Grid;
    END;
    d:=d1;
  END;
  EXCL(Signal^.sType,fantom);
  RETURN TRUE;
END ComeBack;

PROCEDURE Create(x,y,l: INTEGER);
  TYPE s=BITSET; b=BOOLEAN; VAR i: INTEGER;
  PROCEDURE Connect(x,y,l: INTEGER);
    VAR p: pPoint;
  BEGIN
    p:=FindOther(x,y,l,Ident); IF p=NIL THEN RETURN END;
    Cat(Ident,p^.id); Ident:=p^.id; DEC(IdentCnt);
  END Connect;
BEGIN
  IF b(s(x>0)*s(x<Xsize-1)*s(y>0)*s(y<Ysize-1)) THEN
    Ident:=NewIdent(); INC(IdentCnt);
    NewPoint(x,y,l,0,start,Ident,FALSE,sht);
    setWS(x,y,l,start);
    IF WS(x-1,y-1,l)=start THEN Connect(x-1,y-1,l) END;
    IF WS(x-1,y  ,l)=start THEN Connect(x-1,y  ,l) END;
    IF WS(x-1,y+1,l)=start THEN Connect(x-1,y+1,l) END;
    IF WS(x  ,y-1,l)=start THEN Connect(x  ,y-1,l) END;
    IF WS(x  ,y+1,l)=start THEN Connect(x  ,y+1,l) END;
    IF WS(x+1,y-1,l)=start THEN Connect(x+1,y-1,l) END;
    IF WS(x+1,y  ,l)=start THEN Connect(x+1,y  ,l) END;
    IF WS(x+1,y+1,l)=start THEN Connect(x+1,y+1,l) END;
    IF (WS(x,y,1-l)=start) &
       (vias IN Pipe(x,y)) THEN Connect(x,y,1-l)   END;
  END;
END Create;

PROCEDURE Layouter(sh: Sheet; sig: Object;
                   tool: INTEGER; VAR crsX, crsY: INTEGER);
(* Пытается понизить число компонент связности
   сигнала при помощи алгоритма Ли *)
  VAR i: INTEGER; e: Reaction; r: BOOLEAN;
      svGang: INTEGER; finish: BOOLEAN;
BEGIN
  sht:=sh;
  IF sig^.sGang=1 THEN RETURN END;
  svGang:=sig^.sGang;
  Mdl:=sht^.mdl; Signal:=sig; Tool:=tool;
  Text(sht,1,'Creating front...');
  IdentCnt:=0; UnPackSignal(Signal,Create);
  --RoutDeb.StartDeb(Signal);
  finish:=TRUE;
  IF IdentCnt>1 THEN
    i:=IdentCnt;
    Text(sht,1,'Layouting...');
    r:=Exception?(e);
    IF r THEN
      IF r#MemoryOverflow THEN RaiseInMe(r) ELSE DeallocPoints END;
    ELSE
      MoveWave; KillReaction(e);
    END;
    IF i>IdentCnt THEN
      Text(sht,1,'Come back...');
      finish:=ComeBack();
      Bril(Signal,sht);
    END;
  END;
  Signal^.sGang:=Areas(Signal,sht);
  DEC(TotalGang,svGang-Signal^.sGang);
  RemovePoints;
  PackSignal;
  --RoutDeb.FinishDeb;
  IF NOT finish THEN Layouter(sht,sig,tool,crsX,crsY) END;
END Layouter;

BEGIN
  Evr[mE ]   :=Ea;
  Evr[mNE]   :=Eb;
  Evr[mN ]   :=Ec;
  Evr[mNW]   :=Eb;
  Evr[mW ]   :=Ea;
  Evr[mSW]   :=Eb;
  Evr[mS ]   :=Ec;
  Evr[mSE]   :=Eb;
  Evr[mLup]  :=Ed;
  Evr[mLdown]:=Ed;
  Mdl:=NIL;
END Layout.
