IMPLEMENTATION MODULE pedScreen; (* Sem 05-Mar-87. (c) KRONOS *)

IMPORT Terminal;
IMPORT FROM VIDEO;
FROM SYSTEM      IMPORT ADR;
FROM History     IMPORT PutHis;
FROM Resource    IMPORT Final;
FROM Image       IMPORT image0;
FROM Scheduler   IMPORT Signal, Gate, InitSignal, InitGate, Wait, Send,
                        EnterGate, ExitGate, MakeProcess, Start,
                        SetHaltSignal, ProcessId;

FROM pedModel   IMPORT  signal, seg_arr, board, seg_rec;

VAR X1,X2,Y1,Y2   : INTEGER;
    Size          : INTEGER;
    Layer         : BITSET;
    Fixed,Empty   : BOOLEAN;
    Undef         : BOOLEAN;
    Ident         : INTEGER;
    CurSig        : signal;

CONST crsType=4;
TYPE  text=ARRAY [0..3] OF ARRAY [0..79] OF CHAR;
      requests=(wnd,crs,app,del,stop,txt);
      rs=SET OF requests;

VAR Wsp         : ARRAY [0..1999] OF INTEGER;
    Outer       : ProcessId;
    TextBuf     : text;
    Board       : board;
    Stop        : BOOLEAN;
    Open        : BOOLEAN;
    OutRequest  : Signal;
    OutHalted   : Signal;
    SegOK       : Signal;
    Lock        : Gate;
    crsX1,crsX2 : INTEGER;
    crsY1,crsY2 : INTEGER;
    wndX1,wndY1 : INTEGER;
    wndX2,wndY2 : INTEGER;
    wndsX,wndsY : INTEGER;
    scrX,scrY   : INTEGER;
    refcrsX1,refcrsX2: INTEGER;
    refcrsY1,refcrsY2: INTEGER;
    Req,Bad     : rs;
    WndAborted  : BOOLEAN;
    TextSet     : BITSET;
    RQS         : RECORD
      X1,Y1: INTEGER;
      X2,Y2: INTEGER;
      Size : INTEGER;
      Layer: BITSET;
    END;

PROCEDURE CursorOFF;
BEGIN
  IF crs IN Bad THEN RETURN END;
  INCL(Bad,crs);
  mode(com); color(3);
  IF (refcrsX1#refcrsX2)OR(refcrsY1#refcrsY2) THEN
    vect(refcrsX1,refcrsY1,refcrsX2,refcrsY2);
  END;
  cursor(refcrsX2,refcrsY2,crsType);
END CursorOFF;

PROCEDURE RefCursor;
  VAR x1,y1,x2,y2: INTEGER;
BEGIN
  EnterGate(Lock);
  x1:=(crsX1-scrX) DIV wndsX;
  x2:=(crsX2-scrX) DIV wndsX;
  y1:=(crsY1-scrY) DIV wndsY;
  y2:=(crsY2-scrY) DIV wndsY;
  EXCL(Req,crs);
  ExitGate(Lock);
  IF (x1#refcrsX1)OR(x2#refcrsX2)OR(y1#refcrsY1)OR(y2#refcrsY2) THEN
    CursorOFF;
  END;
  IF (rs{crs,wnd,stop}*Req)#rs{} THEN RETURN END;
  IF crs IN Bad THEN
    refcrsX1:=x1; refcrsX2:=x2; refcrsY1:=y1; refcrsY2:=y2;
    EXCL(Bad,crs);
    mode(com); color(3);
    IF (refcrsX1#refcrsX2)OR(refcrsY1#refcrsY2) THEN
      vect(refcrsX1,refcrsY1,refcrsX2,refcrsY2);
    END;
    cursor(refcrsX2,refcrsY2,crsType);
  END;
END RefCursor;

PROCEDURE line(x0,y0,x1,y1: INTEGER; w2: INTEGER);
  VAR h,v,l: INTEGER;
      dx,dy: INTEGER;       w,r: INTEGER;
    x0u,x0d: INTEGER;   x1u,x1d: INTEGER;
    y0u,y0d: INTEGER;   y1u,y1d: INTEGER;
    S,N,E,W: INTEGER;

  PROCEDURE SQRT(n:INTEGER): INTEGER;
    VAR l,r: INTEGER;
  BEGIN
    IF n<0 THEN RETURN SQRT(-n) END;
    IF n<2 THEN RETURN n END;
    l:=1; r:=n;
    REPEAT r:=(l+r)DIV 2; l:=n DIV r UNTIL l>=r;
    RETURN r
  END SQRT;

BEGIN
  IF w2<=0 THEN RETURN END;
  w:=w2 DIV 2;
  W:=clipW; E:=clipE; S:=clipS; N:=clipN;
  IF (x0<W) & (x1<W) OR (y0<S) & (y1<S) THEN RETURN END;
  IF (x0>E) & (x1>E) OR (y0>N) & (y1>N) THEN RETURN END;
  IF w<=2 THEN vect(x0,y0,x1,y1);
    IF w=2 THEN
      IF    x0=x1 THEN vect(x0+1,y0  ,x1+1,y1  )
      ELSIF y0=y1 THEN vect(x0  ,y0+1,x1  ,y1+1)
      ELSE             vect(x0+1,y0+1,x1+1,y1+1)
      END;
    END; RETURN
  END;
  IF ODD(w) THEN ELSE INC(w) END;
  h:=x1-x0;   v:=y1-y0;
  IF    h=0 THEN l:=ABS(v)
  ELSIF v=0 THEN l:=ABS(h)
  ELSE
    l:=SQRT(h*h+v*v);
  END;
  IF l=0 THEN RETURN END;
  dx:=v*w DIV l DIV 2;
  dy:=h*w DIV l DIV 2 * dotszX  DIV dotszY;

  IF (dx=0) & (dy=0) THEN vect(x0,y0,x1,y1); RETURN END;

  x0u:=x0-dx; x0d:=x0+dx;
  y0u:=y0+dy; y0d:=y0-dy;
  x1u:=x1-dx; x1d:=x1+dx;
  y1u:=y1+dy; y1d:=y1-dy;

  r:=w DIV 2;
  vect(x0u,y0u,x1u,y1u);
  circ(x1,y1,r);
  vect(x1d,y1d,x0d,y0d);
  circ(x0,y0,r);

END line;

PROCEDURE DoConductor;
  VAR x,y,r: INTEGER;
BEGIN
  IF (X1=X2) & (Y1=Y2) THEN
    x:=(X1-scrX) DIV wndsX; y:=(Y1-scrY) DIV wndsY; r:=Size DIV wndsX;
    color(INTEGER(Layer));
    IF r<=2 THEN dot(x,y) ELSE circ(x,y,r) END;
  ELSE
    color(INTEGER(Layer));
    line(
         (X1-scrX) DIV wndsX,(Y1-scrY) DIV wndsY,
         (X2-scrX) DIV wndsX,(Y2-scrY) DIV wndsY,
         Size*4 DIV wndsX);
  END;
  WndAborted:=WndAborted OR((rs{wnd,stop}*Req)#rs{});
END DoConductor;

VAR Chain: seg_arr;
    mask : BITSET;

(* bits
  0 - fixed
  1 - main direction
  2 - circle mask
  3 - without metal in vias
*)

PROCEDURE UnPackSeg(VAL s: seg_rec);
BEGIN
  WITH s DO
    IF c=0 THEN Undef:=TRUE; RETURN END;
    Size:=CARDINAL(BITSET(c<<12)*{0..11});
    IF 1 IN BITSET(c) THEN
      X1:=CARDINAL(BITSET(a)*mask)+Size;
      Y1:=CARDINAL(BITSET(a>>16)*mask)+Size;
      X2:=CARDINAL(BITSET(b)*mask)-Size;
      Y2:=CARDINAL(BITSET(b>>16)*mask)-Size;
    ELSE
      X1:=CARDINAL(BITSET(a)*mask)+Size;
      Y2:=CARDINAL(BITSET(a>>16)*mask)+Size;
      X2:=CARDINAL(BITSET(b)*mask)-Size;
      Y1:=CARDINAL(BITSET(b>>16)*mask)-Size;
    END;
    Layer:=BITSET(c>>12)*{0..7};
    Fixed:=0 IN BITSET(c);
  END;
  Undef:=FALSE;
END UnPackSeg;

PROCEDURE StartConductor(s: signal);
BEGIN
  CurSig:=s;
  Chain:=s^.cu;
  Ident:=-1; Empty:=FALSE;
  LOOP
    IF Empty THEN RETURN END;
    INC(Ident);
    IF (Chain=NIL)OR(Ident>=CurSig^.cno) THEN
      Empty:=TRUE; Undef:=TRUE;
    ELSE
      Empty:=FALSE;
      UnPackSeg(Chain^[Ident]);
    END;
    IF NOT Undef THEN RETURN END;
  END;
END StartConductor;

PROCEDURE FindConductor(id: CARDINAL);
  VAR i: CARDINAL;
BEGIN
  Ident:=id;
  IF (Chain=NIL)OR(Ident>=CurSig^.cno) THEN
    Empty:=TRUE; Undef:=TRUE;
  ELSE
    Empty:=FALSE;
    UnPackSeg(Chain^[Ident]);
  END;
END FindConductor;

VAR LineXs,LineYs,LineXe,LineYe: INTEGER;

PROCEDURE DoInBox;
  VAR i,j: INTEGER; c: seg_arr; p,e,b: POINTER TO seg_rec; s: signal;
BEGIN
  IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
  IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
  FOR i:=0 TO Board^.sno-1 DO
    s:=Board^.sigs^[i];
    IF (s^.cno>0) THEN
      c:=s^.cu;
      b:=ADR(c^[0]); p:=b;
      e:=ADR(c^[s^.cno-1]);
      LOOP
        IF BOOLEAN(p^.c) THEN
          IF BOOLEAN(BITSET(INTEGER(BITSET(p^.a)*mask)<=LineXe)*
                     BITSET(INTEGER(BITSET(p^.a>>16)*mask)<=LineYe)*
                     BITSET(INTEGER(BITSET(p^.b)*mask)>=LineXs)*
                     BITSET(INTEGER(BITSET(p^.b>>16)*mask)>=LineYs)) THEN
            CurSig:=s; Chain:=s^.cu; Empty:=FALSE;
            Ident:=(INTEGER(p)-INTEGER(b)) DIV SIZE(seg_rec);
            UnPackSeg(Chain^[Ident]);
            DoConductor;
            IF WndAborted THEN RETURN END;
          END;
        END;
        IF p=e THEN EXIT END;
        INC(INTEGER(p),SIZE(seg_rec));
      END;
    END;
  END;
END DoInBox;

PROCEDURE RefWindow;
  VAR sx,sy,x1,x2,y1,y2,i,rollXs,rollYs: INTEGER;
  PROCEDURE MoveScreen;
    VAR i: INTEGER;
  BEGIN
    LOOP
      IF (Req*rs{wnd,stop})#rs{} THEN RETURN END;
      IF    scrX-rollXs>x1 THEN
        IF scrX+10*wndsX>wndX1 THEN wndX1:=scrX+10*wndsX END;
        i:=(scrX-x1)DIV rollXs;
        rollE(i); DEC(scrX,rollXs*i);
        IF wndX2>scrX+799*wndsX THEN wndX2:=scrX+799*wndsX END;
      ELSIF scrX+rollXs<x1 THEN
        IF scrX+789*wndsX<wndX2 THEN wndX2:=scrX+789*wndsX END;
        i:=(x1-scrX)DIV rollXs;
        rollW(i); INC(scrX,rollXs*i);
        IF wndX1<scrX           THEN wndX1:=scrX END;
      ELSIF scrY-rollYs>y1 THEN
        i:=(scrY-y1)DIV rollYs;
        rollN(i); DEC(scrY,rollYs*i);
        IF wndY2>scrY+251*wndsY THEN wndY2:=scrY+251*wndsY END;
      ELSIF scrY+rollYs<y1 THEN
        i:=(y1-scrY)DIV rollYs;
        rollS(i); INC(scrY,rollYs*i);
        IF wndY1<scrY           THEN wndY1:=scrY END;
      ELSE RETURN
      END;
    END;
  END MoveScreen;
  PROCEDURE MoveWindow;
  BEGIN
    LOOP
      WndAborted:=FALSE;
      IF    wndX1-scrX>0 THEN
        LineXs:=scrX; LineXe:=wndX1; LineYs:=wndY1; LineYe:=wndY2;
        mode(add); DoInBox;
        IF NOT WndAborted THEN wndX1:=LineXs ELSE RETURN END;
      ELSIF scrX+799*wndsX-wndX2>0 THEN
        LineXs:=wndX2; LineXe:=scrX+799*wndsX; LineYs:=wndY1; LineYe:=wndY2;
        mode(add); DoInBox;
        IF NOT WndAborted THEN wndX2:=LineXe ELSE RETURN END;
      ELSIF wndY1-scrY>0 THEN
        LineXs:=wndX1; LineXe:=wndX2; LineYs:=scrY; LineYe:=wndY1;
        mode(add); DoInBox;
        IF NOT WndAborted THEN wndY1:=LineYs ELSE RETURN END;
      ELSIF scrY+251*wndsY-wndY2>0 THEN
        LineXs:=wndX1; LineXe:=wndX2; LineYs:=wndY2; LineYe:=scrY+251*wndsY;
        mode(add); DoInBox;
        IF NOT WndAborted THEN wndY2:=LineYe ELSE RETURN END;
      ELSE RETURN
      END;
    END;
  END MoveWindow;
BEGIN
  EnterGate(Lock);
  sx:=ScaleX; sy:=ScaleY; rollXs:=rollX*sx; rollYs:=rollY*sy;
  x1:=WindowW; x2:=WindowE; y1:=WindowS; y2:=WindowN;
  EXCL(Req,wnd);
  ExitGate(Lock);
  IF NOT(wnd IN Bad)&(sx=wndsX)&(sy=wndsY)&
    (wndX2-wndX1>=799*sx)&(wndY2-wndY1>=599*sy)&
    (ABS(x1-wndX1)<rollXs)&(ABS(y1-wndY1)<rollYs) THEN RETURN END;
  IF (wnd IN Bad)OR(sx#wndsX)OR(sy#wndsY)OR
    (ABS(wndX1-x1)>400*sx)OR(ABS(wndY1-y1)>150*sy)OR
    (ABS(wndX2-x2)>400*sx)OR(ABS(wndY2-y2)>150*sy) THEN
    fill(0); Bad:=Bad+rs{wnd,crs,txt}; TextSet:={0..3};
    WndAborted:=FALSE;
    scrX:=x1; scrY:=y1;
    wndX1:=x1; wndY1:=y1; wndX2:=x2; wndY2:=y2; wndsX:=sx; wndsY:=sy;
    LineXs:=x1; LineXe:=x2; LineYs:=y1; LineYe:=y2;
    mode(add); DoInBox;
    IF NOT WndAborted THEN EXCL(Bad,wnd) END;
  ELSE
    CursorOFF;
    MoveScreen;
    IF (Req*rs{wnd,stop})#rs{} THEN RETURN END;
    MoveWindow;
  END;
  mode(add); color(3);
  x1:=(        -scrX) DIV wndsX; y1:=(        -scrY) DIV wndsY;
  x2:=(Board^.x-scrX) DIV wndsX; y2:=(Board^.y-scrY) DIV wndsY;
  vect(x1,y1,x2,y1); vect(x2,y1,x2,y2); vect(x2,y2,x1,y2); vect(x1,y2,x1,y1);
END RefWindow;

PROCEDURE RefText;
  VAR bf: text; i: INTEGER; ts: BITSET;
BEGIN
  EnterGate(Lock);
  bf:=TextBuf; ts:=TextSet; TextSet:={}; EXCL(Req,txt);
  ExitGate(Lock);
  FOR i:=0 TO 3 DO IF i IN ts THEN print(i,0,'%s',bf[i]) END END;
  EXCL(Bad,txt);
END RefText;

PROCEDURE AppSeg;
BEGIN
  EXCL(Req,app);
  mode(add);
  X1:=RQS.X1;
  X2:=RQS.X2;
  Y1:=RQS.Y1;
  Y2:=RQS.Y2;
  Size:=RQS.Size;
  Layer:=RQS.Layer;
  DoConductor;
  Send(SegOK);
END AppSeg;

PROCEDURE DelSeg;
BEGIN
  EXCL(Req,del);
  mode(com);
  X1:=RQS.X1;
  X2:=RQS.X2;
  Y1:=RQS.Y1;
  Y2:=RQS.Y2;
  Size:=RQS.Size;
  Layer:=RQS.Layer;
  DoConductor;
  Send(SegOK);
END DelSeg;

PROCEDURE Out;
BEGIN
  LOOP
    Wait(OutRequest);
    WHILE (Req+Bad)#rs{} DO
      IF    stop IN Req       THEN fill(0); finish; HALT
      ELSIF wnd  IN (Req+Bad) THEN RefWindow;
      ELSIF crs  IN (Req+Bad) THEN RefCursor;
      ELSIF txt  IN (Req+Bad) THEN RefText;
      ELSIF app  IN  Req      THEN AppSeg;
      ELSIF del  IN  Req      THEN DelSeg;
      END;
    END;
  END;
END Out;

PROCEDURE OpenWindow(o: board; scale,x,y: INTEGER);
BEGIN
  IF o=NIL THEN CloseWindow; RETURN END;
  IF NOT Open THEN
    Board:=o; Req:=rs{}; Bad:=rs{wnd,crs,txt}; TextSet:={0..3};
    Outer:=MakeProcess(Out,ADR(Wsp),SIZE(Wsp));
    InitSignal(OutHalted); InitSignal(OutRequest); InitSignal(SegOK);
    SetHaltSignal(Outer,OutHalted);
    InitGate(Lock);
    start; fill(0);
    Start(Outer);
    Open:=TRUE;
  END;
  SetWindow(scale,x,y);
END OpenWindow;

PROCEDURE CloseWindow;
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  IF NOT Open THEN RETURN END;
  INCL(Req,stop);
  Send(OutRequest);
  Wait(OutHalted);
  Board:=NIL;
  Open:=FALSE;
  finish;
  PutHis(s,Outer^.pp);
  Terminal.print('%s\n',s);
END CloseWindow;

PROCEDURE SetWindow(s,x,y: INTEGER);
BEGIN
  IF NOT Open THEN RETURN END;
  EnterGate(Lock);
  ScaleX:=s; ScaleY:=ScaleX * dotszY DIV dotszX;
  WindowX:=x; WindowY:=y;
  WindowW:=WindowX- 400 *ScaleX;
  WindowE:=WindowX+ 399 *ScaleX;
  WindowS:=WindowY- 150 *ScaleY;
  WindowN:=WindowY+ 101 *ScaleY;
  crsX1:=x; crsX2:=x; crsY1:=y; crsY2:=y;
  INCL(Req,wnd); INCL(Req,crs);
  Send(OutRequest);
  ExitGate(Lock);
END SetWindow;

PROCEDURE Cursor(x1,y1,x2,y2: CARDINAL);
BEGIN
  IF NOT Open THEN RETURN END;
  EnterGate(Lock);
  crsX1:=x1; crsY1:=y1; crsX2:=x2; crsY2:=y2;
  INCL(Req,crs);
  Send(OutRequest);
  ExitGate(Lock);
END Cursor;

PROCEDURE Text(ln: INTEGER; s: ARRAY OF CHAR);
BEGIN
  IF NOT Open THEN RETURN END;
  EnterGate(Lock);
  image0(TextBuf[ln],'%s',s);
  INCL(TextSet,ln);
  INCL(Req,txt);
  Send(OutRequest);
  ExitGate(Lock);
END Text;

PROCEDURE Drow(x1,y1,x2,y2,s: INTEGER; l: BITSET);
BEGIN
  IF NOT Open THEN RETURN END;
  WITH RQS DO X1:=x1; Y1:=y1; X2:=x2; Y2:=y2; Size:=s; Layer:=l END;
  INCL(Req,app); Send(OutRequest); Wait(SegOK);
END Drow;

PROCEDURE Delete(x1,y1,x2,y2,s: INTEGER; l: BITSET);
BEGIN
  IF NOT Open THEN RETURN END;
  WITH RQS DO X1:=x1; Y1:=y1; X2:=x2; Y2:=y2; Size:=s; Layer:=l END;
  INCL(Req,del); Send(OutRequest); Wait(SegOK);
END Delete;

VAR i: INTEGER;

BEGIN
  Open:=FALSE;
  TextSet:={};
  FOR i:=0 TO 3 DO image0(TextBuf[i],'') END;
  Final(CloseWindow);
  CurSig:=NIL;
  Chain:=NIL;
  Empty:=TRUE;
  Undef:=TRUE;
  mask:={0..15};
END pedScreen.
