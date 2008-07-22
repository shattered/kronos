IMPLEMENTATION MODULE Layout; (* Sem 12-Feb-87. (c) KRONOS *)

FROM Terminal    IMPORT print;
FROM Image       IMPORT image0;
FROM pedScreen   IMPORT Text, Cursor;
FROM Scheduler   IMPORT EnterGate, ExitGate, Wait, TimeSignal;
FROM pedTools    IMPORT Tools;
FROM pedPbl      IMPORT LayersNo, Grid, Clearens;
FROM pedTopology IMPORT InsertVias, InsertRange, FindSignal, Areas,
                        LineXs, LineXe, LineYs, LineYe, Del, Len, SeekInBox;
FROM Points      IMPORT NewPoint, pPoint, FindPoint, FindOther, FindMin,
                        Cat, KilLastPoint, RemovePoints, NewIdent,
                        DeallocPoints;
FROM DWS         IMPORT mLup, mLdown, mE, mW, mN, mS, mSE, mSW, mNE, mNW,
                        free, start, front, finish, UnPackSignal, PackSignal,
                        WS, setWS, Vias, Busy, Pipe, Buf, Xsize, Ysize,
                        fixed, fixedvias, vias;
FROM Model       IMPORT Object, fantom;
FROM ModelPbl    IMPORT Exception?, Reaction, KillReaction, RaiseInMe,
                        MemoryOverflow;
IMPORT ModelMisc, RoutDeb;
FROM ModelMisc  IMPORT  StartConductor, NextConductor, Empty, Fixed,
                        Layer, X1, X2, Y1, Y2, Size, FindConductor;
FROM cdsImp     IMPORT  Bril;

VAR Evr : ARRAY [0..31] OF INTEGER;
    Tool: INTEGER;
    Mdl     : Object;
    Signal  : Object;
    NewCrsXs, NewCrsYs: INTEGER;
    NewCrsXe, NewCrsYe: INTEGER;

CONST
 Ea= 1;  (* -  *)
 Eb=21;  (* /  *)
 Ec=10;  (* |  *)
 Ed=24;  (* o  *)
 Ee=100; (* _\ *)
 Ei=19;  (* _| *)
 Ef=15;  (* _/ *)
 Eg=3;   (* o| *)
 Eh=20;  (* | | *)
 Ebusy=1000;

PROCEDURE Weight(x,y,l,d1,d2: INTEGER): INTEGER;
  VAR i,j,d: INTEGER; s: BITSET; b,c: BOOLEAN; sig: Object;
BEGIN
  d:=d2;
-- если слой 1 то развернем на 90 градусов:
  IF l=1 THEN
    IF d1 IN {mE..mSE} THEN d1:=(10-(d1-mE))MOD 8 +mE END;
    IF d2 IN {mE..mSE} THEN d2:=(10-(d2-mE))MOD 8 +mE END;
  END;

-- направление сегмента:
  i:=Evr[d2];
(*
-- величина угла поворота:
  IF (d1 IN {mE..mSE})&(d2 IN {mE..mSE}) THEN
    j:=ABS(d1-d2);
    IF j>4 THEN j:=8-j END;
    CASE j OF
      4: INC(i,Ee);
     |3: INC(i,Ee);
     |2: INC(i,Ei);
     |1: INC(i,Ef);
     |0:
    END;
  END;
*)
-- удешевим сегменты (45') проходящие мимо занятых ячеек
--CASE d OF
--  mNE: b:=Busy(x,y+1,l,Buf) OR Busy(x+1,y,l,Buf);
-- |mNW: b:=Busy(x,y+1,l,Buf) OR Busy(x-1,y,l,Buf);
-- |mSW: b:=Busy(x,y-1,l,Buf) OR Busy(x-1,y,l,Buf);
-- |mSE: b:=Busy(x,y-1,l,Buf) OR Busy(x+1,y,l,Buf);
--ELSE   b:=FALSE;
--END;
--IF b THEN DEC(i,Eg) END;

--IF (l=0)&(d IN {mE,mW}) THEN
--  IF (WS(x,y-1,l,Buf)=start)OR(WS(x,y+1,l,Buf)=start)OR
--     (WS(x,y-2,l,Buf)=start)OR(WS(x,y+2,l,Buf)=start) THEN INC(i,Eh) END;
--END;
--IF (l=1)&(d IN {mN,mS}) THEN
--  IF (WS(x-1,y,l,Buf)=start)OR(WS(x+1,y,l,Buf)=start)OR
--     (WS(x-2,y,l,Buf)=start)OR(WS(x+2,y,l,Buf)=start) THEN INC(i,Eh) END;
--END;

  IF d IN {mLup,mLdown} THEN
    IF Vias(x,y,Buf) THEN
--    sig:=FindVias(x,y);
--    ASSERT(sig#NIL);
      INC(i,Ebusy);
    END;
  ELSE
    IF Busy(x,y,l,Buf) THEN
--    sig:=FindSegment(x,y,l);
--    ASSERT(sig#NIL);
      INC(i,Ebusy);
    END;
  END;
  IF i<=0 THEN i:=1 END;
  RETURN i
END Weight;

PROCEDURE VeightAng(d1,d2: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
(*IF (d1 IN {mE..mSE})&(d2 IN {mE..mSE}) THEN
    i:=ABS(d1-d2);
    IF i>4 THEN i:=8-i END;
    CASE i OF
      1: RETURN Ee;
     |2: RETURN Ei;
     |3: RETURN Ef;
     |4: RETURN 0;
    END;
  END;*)
  RETURN 0;
END VeightAng;

VAR
    Ident,
    IdentCnt: INTEGER;
    Finished: BOOLEAN;
    FinishA,
    FinishB : pPoint;
    FinishV,
    minFinV : INTEGER;
    X,Y,L,
    V,D,ID  : INTEGER;
    HD      : BOOLEAN;

PROCEDURE WSfff(x,y,l: INTEGER): BOOLEAN;
BEGIN
  RETURN (WS(x,y,l,Buf) IN {free,front,finish}) &
         NOT (Busy(x,y,l,Buf) & ((fixed+l) IN Pipe(x,y,Buf)));
END WSfff;

PROCEDURE Correct(x,y,l,d: INTEGER);
  VAR v: INTEGER; p: pPoint; hd: BOOLEAN;
BEGIN
  IF (l#L)& Vias(x,y,Buf) & (fixedvias IN Pipe(x,y,Buf)) THEN RETURN END;
  v:=V+Weight(X,Y,L,D,d); p:=FindPoint(x,y,l,ID);
  IF p=NIL THEN
    hd:=HD OR Busy(x,y,l,Buf);
    IF d IN {mLup,mLdown} THEN hd:=HD OR Vias(x,y,Buf) END;
    NewPoint(x,y,l,v,d,ID,hd);
    IF WS(x,y,l,Buf)=free THEN
      setWS(x,y,l,Buf,front);
    ELSE
      setWS(x,y,l,Buf,finish);
    END;
  ELSIF p^.v>v THEN
    ASSERT(NOT p^.stopped);
    IF p^.hard OR NOT HD THEN p^.v:=v; p^.d:=d; p^.hard:=p^.hard OR HD END;
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
  HD:=FinishA^.hard OR FinishB^.hard;
  setWS(X,Y,L,Buf,D);
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
    d:=WS(X,Y,L,Buf);
    IF d=finish THEN m:=FindPoint(X,Y,L,ID); ASSERT(m#NIL); d:=m^.d END;
    setWS(X,Y,L,Buf,D);
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
    IF Finished & ((FinishV<minFinV+V+100) OR (m=NIL)) THEN
      ComeForward; EXIT
    END;
    IF m=NIL THEN EXIT END;
    X:=m^.x; Y:=m^.y; L:=m^.l; V:=m^.v; ID:=m^.id; D:=m^.d; HD:=m^.hard;
    IF V>maxV THEN EXIT END;
    RoutDeb.DebPnt(X,Y,L,V,D);
    ASSERT(L IN {0..1});
    IF WS(X,Y,L,Buf)=finish THEN
      p:=FindOther(X,Y,L,ID);
      ASSERT(p#NIL);
      IF p^.stopped THEN
        fv:=m^.v+p^.v+VeightAng(p^.d,m^.d);
        IF NOT Finished OR(fv<FinishV) THEN
          FinishA:=m; FinishB:=p; FinishV:=fv; Finished:=TRUE;
        END;
      END;
      m^.stopped:=TRUE;
      IF minFinV>m^.v THEN minFinV:=m^.v END;
    ELSE
      ASSERT(D#finish);
      KilLastPoint; setWS(X,Y,L,Buf,D);
    END;
    ASSERT(L IN {0..1});
    IF BOOLEAN(s(X>0)*s(X<Xsize1)*s(Y>0)*s(Y<Ysize1)) THEN
--    IF WSfff(X-1,Y-1,L)   THEN Correct(X-1,Y-1,L,mSW) END;
      IF WSfff(X-1,Y  ,L)   THEN Correct(X-1,Y  ,L,mW ) END;
--    IF WSfff(X-1,Y+1,L)   THEN Correct(X-1,Y+1,L,mNW) END;
      IF WSfff(X  ,Y-1,L)   THEN Correct(X  ,Y-1,L,mS ) END;
      IF WSfff(X  ,Y+1,L)   THEN Correct(X  ,Y+1,L,mN ) END;
--    IF WSfff(X+1,Y-1,L)   THEN Correct(X+1,Y-1,L,mSE) END;
      IF WSfff(X+1,Y  ,L)   THEN Correct(X+1,Y  ,L,mE ) END;
--    IF WSfff(X+1,Y+1,L)   THEN Correct(X+1,Y+1,L,mNE) END;
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
  FindConductor(i); Del; INC(DelCnt);
  PackSignal;
  ModelMisc.Signal^.sGang:=Areas(ModelMisc.Signal);
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
  SeekInBox(Mdl,BreakIS);
END Break;

PROCEDURE ComeBack(): BOOLEAN;
  VAR  d,d1: INTEGER; p: pPoint; Xs,Ys,Ls,cnt: INTEGER;
BEGIN
  INCL(Signal^.sType,fantom); DelCnt:=0;
  d:=WS(X,Y,L,Buf);
  Xs:=X*Grid; Ys:=Y*Grid; Ls:=L;
  cnt:=0;
  LOOP
    IF cnt>10000 THEN
      EXCL(Signal^.sType,fantom);
      RETURN TRUE;
    END;
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
    d1:=WS(X,Y,L,Buf);
    IF d1=finish THEN p:=FindPoint(X,Y,L,Ident); d1:=p^.d END;
    IF (d1 IN {mLup,mLdown})& Vias(X,Y,Buf) THEN Break(X,Y,-1) END;
    IF Busy(X,Y,L,Buf) THEN Break(X,Y,L) END;
    d:=d1;
    INC(cnt);
  END;
  IF DelCnt>0 THEN RETURN FALSE END;
  X:=Xs DIV Grid; Y:=Ys DIV Grid; L:=Ls;
  d:=WS(X,Y,L,Buf);
  cnt:=0;
  LOOP
    IF cnt>10000 THEN
      EXCL(Signal^.sType,fantom);
      RETURN TRUE;
    END;
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
    d1:=WS(X,Y,L,Buf);
    IF d1=finish THEN p:=FindPoint(X,Y,L,Ident); d1:=p^.d END;
    IF d1#d THEN
      LineXs:=Xs; LineYs:=Ys;
      LineXe:=X*24; LineYe:=Y*24;
      WITH Tools[Tool] DO
        IF d IN {mLup,mLdown} THEN
          INC(LineXs,VDefX); INC(LineXe,VDefX);
          INC(LineYs,VDefY); INC(LineYe,VDefY);
          IF InsertVias(VSize,DSize,FALSE,Mdl,Signal) THEN END;
        ELSE
          INC(LineXs,DefX); INC(LineXe,DefX);
          INC(LineYs,DefY); INC(LineYe,DefY);
          IF InsertRange(Size,L,FALSE,Mdl,Signal) THEN END;
        END;
      END;
      Xs:=X*Grid; Ys:=Y*Grid;
    END;
    d:=d1;
    INC(cnt);
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
    NewPoint(x,y,l,0,start,Ident,FALSE);
    setWS(x,y,l,Buf,start);
    IF WS(x-1,y-1,l,Buf)=start THEN Connect(x-1,y-1,l) END;
    IF WS(x-1,y  ,l,Buf)=start THEN Connect(x-1,y  ,l) END;
    IF WS(x-1,y+1,l,Buf)=start THEN Connect(x-1,y+1,l) END;
    IF WS(x  ,y-1,l,Buf)=start THEN Connect(x  ,y-1,l) END;
    IF WS(x  ,y+1,l,Buf)=start THEN Connect(x  ,y+1,l) END;
    IF WS(x+1,y-1,l,Buf)=start THEN Connect(x+1,y-1,l) END;
    IF WS(x+1,y  ,l,Buf)=start THEN Connect(x+1,y  ,l) END;
    IF WS(x+1,y+1,l,Buf)=start THEN Connect(x+1,y+1,l) END;
    IF (WS(x,y,1-l,Buf)=start) &
       (vias IN Pipe(x,y,Buf)) THEN Connect(x,y,1-l)   END;
  END;
END Create;

PROCEDURE Layouter(mdl,sig: Object; tool: CARDINAL; VAR crsX, crsY: INTEGER);
(* Пытается понизить число компонент связности
   сигнала при помощи алгоритма Ли *)
  VAR i: INTEGER; e: Reaction; r: BOOLEAN;
      svGang: INTEGER; finish: BOOLEAN;
BEGIN
  IF sig^.sGang=1 THEN RETURN END;
  svGang:=sig^.sGang;
  Mdl:=mdl; Signal:=sig; Tool:=tool;
  Text(1,'Creating front...');
  IdentCnt:=0; UnPackSignal(Signal,Create);
  RoutDeb.StartDeb(Signal);
  finish:=TRUE;
  IF IdentCnt>1 THEN
    i:=IdentCnt;
    Text(1,'Layouting...');
    r:=Exception?(e);
    IF r THEN
      IF r#MemoryOverflow THEN RaiseInMe(r) ELSE DeallocPoints END;
    ELSE
      MoveWave; KillReaction(e);
    END;
    IF i>IdentCnt THEN
      Text(1,'Come back...');
      finish:=ComeBack();
      Bril(Signal);
    END;
  END;
  Signal^.sGang:=Areas(Signal);
  DEC(TotalGang,svGang-Signal^.sGang);
  RemovePoints;
  PackSignal;
  RoutDeb.FinishDeb;
  IF NOT finish THEN Layouter(mdl,sig,tool,crsX,crsY) END;
END Layouter;

BEGIN
  Evr[mE ]:=Ea;
  Evr[mNE]:=Eb;
  Evr[mN ]:=Ec;
  Evr[mNW]:=Eb;
  Evr[mW ]:=Ea;
  Evr[mSW]:=Eb;
  Evr[mS ]:=Ec;
  Evr[mSE]:=Eb;
  Evr[mLup]:=Ed;
  Evr[mLdown]:=Ed;
  Mdl:=NIL;
END Layout.
