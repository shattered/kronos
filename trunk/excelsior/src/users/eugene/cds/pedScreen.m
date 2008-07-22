IMPLEMENTATION MODULE pedScreen; (* Sem 05-Mar-87. (c) KRONOS *)

IMPORT ModelMisc, Terminal;
IMPORT FROM VIDEO;
FROM SYSTEM      IMPORT ADR;
FROM History     IMPORT PutHis;
FROM Resource    IMPORT Final;
FROM Model       IMPORT Object, Segment, Tag, Iterate, signal;
FROM Image       IMPORT image0;
FROM Scheduler   IMPORT Signal, Gate, InitSignal, InitGate, Wait, Send,
                        EnterGate, ExitGate, MakeProcess, Start,
                        SetHaltSignal, ProcessId;

VAR X1,X2,Y1,Y2   : INTEGER;
    Size          : INTEGER;
    Layer         : BITSET;
    Fixed,Empty   : BOOLEAN;

CONST crsType=4;
TYPE  text=ARRAY [0..3] OF ARRAY [0..79] OF CHAR;
      requests=(wnd,crs,app,del,stop,txt);
      rs=SET OF requests;

VAR Wsp         : ARRAY [0..1999] OF INTEGER;
    Outer       : ProcessId;
    TextBuf     : text;
    Board       : Object;
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

VAR
  mask : BITSET;

(*$T-*)

PROCEDURE UnPackSeg(VAL s: Segment);
  CONST off=8000h;
BEGIN
  WITH s DO
    Size:=CARDINAL(BITSET(size<<12)*{0..11});
    IF 1 IN BITSET(size) THEN
      X1:=CARDINAL(BITSET(start)*mask)+Size-off;
      Y1:=CARDINAL(BITSET(start>>16)*mask)+Size-off;
      X2:=CARDINAL(BITSET(end)*mask)-Size-off;
      Y2:=CARDINAL(BITSET(end>>16)*mask)-Size-off;
    ELSE
      X1:=CARDINAL(BITSET(start)*mask)+Size-off;
      Y2:=CARDINAL(BITSET(start>>16)*mask)+Size-off;
      X2:=CARDINAL(BITSET(end)*mask)-Size-off;
      Y1:=CARDINAL(BITSET(end>>16)*mask)-Size-off;
    END;
    Layer:=BITSET(size>>12)*{0..7};
--  ViasSize:=CARDINAL(BITSET(size>>4)*{0..7});
    Fixed:=0 IN BITSET(size);
--  CircleTool:=2 IN BITSET(size);
  END;
END UnPackSeg;

(*$T+*)

VAR LineXs,LineYs,LineXe,LineYe: INTEGER;

PROCEDURE DoInBox0(s: Object);
  VAR
    i,j  : INTEGER;
    c    : Object;
    p,e,b: POINTER TO Segment;
BEGIN
  IF (Tag(s)#signal)OR(s^.ChainB=NIL)OR WndAborted THEN RETURN END;
  c:=s^.ChainB;
  IF c^.cLen=0 THEN RETURN END;
(*$T-*)
  b:=ADR(c^.cType[0]); p:=b;
  e:=ADR(c^.cType[c^.cLen]);
  REPEAT
    IF BOOLEAN(p^.size) THEN
      IF BOOLEAN(BITSET(INTEGER(BITSET(p^.start)*mask)<=LineXe)*
                 BITSET(INTEGER(BITSET(p^.start>>16)*mask)<=LineYe)*
                 BITSET(INTEGER(BITSET(p^.end)*mask)>=LineXs)*
                 BITSET(INTEGER(BITSET(p^.end>>16)*mask)>=LineYs)) THEN
        UnPackSeg(p^); DoConductor;
        IF WndAborted THEN RETURN END;
      END;
    END;
    INC(INTEGER(p),SIZE(Segment));
  UNTIL p=e;
(*$T+*)
END DoInBox0;

PROCEDURE DoInBox;
  VAR i: INTEGER;
BEGIN
  IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
  IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
  LineXs:=LineXs+8000h; LineXe:=LineXe+8000h;
  LineYs:=LineYs+8000h; LineYe:=LineYe+8000h;
  Iterate(Board^.All,DoInBox0);
  LineXs:=LineXs-8000h; LineXe:=LineXe-8000h;
  LineYs:=LineYs-8000h; LineYe:=LineYe-8000h;
END DoInBox;

PROCEDURE RefWindow;
  VAR sx,sy,x1,x2,y1,y2,i,rollXs,rollYs: INTEGER;
  PROCEDURE Min(x,y: INTEGER): INTEGER;
  BEGIN
    IF x<y THEN RETURN x ELSE RETURN y END
  END Min;
  PROCEDURE Max(x,y: INTEGER): INTEGER;
  BEGIN
    IF x>y THEN RETURN x ELSE RETURN y END
  END Max;
  PROCEDURE MoveScreen;
    VAR i,dx,dy: INTEGER;
  BEGIN
    IF (wndX1>wndX2) OR (wndY1>wndY2) THEN
      scrX:=x1; scrY:=y1; RETURN
    END;
    IF (wndsX#sx) OR (wndsY#sy) THEN
      fill(0); Bad:=Bad+rs{wnd,crs,txt}; TextSet:={0..3};
      wndX1:=0; wndX2:=-1; wndY1:=0; wndY2:=-1;
      scrX:=x1; scrY:=y1; RETURN;
    END;
    IF (wndX1>x2) OR (wndX2<x1) THEN
      dx:=0;
    ELSE
      dx:=Min(wndX2,x2)-Max(wndX1,x1);
    END;
    IF (wndY1>y2) OR (wndY2<y1) THEN
      dy:=0;
    ELSE
      dy:=Min(wndY2,y2)-Max(wndY1,y1);
    END;
    IF dx*dy<800*252 DIV 4 THEN
      fill(0); Bad:=Bad+rs{wnd,crs,txt}; TextSet:={0..3};
      wndX1:=0; wndX2:=-1; wndY1:=0; wndY2:=-1;
      scrX:=x1; scrY:=y1; RETURN;
    END;
    LOOP
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
      ELSE
        WndAborted:=WndAborted OR((rs{wnd,stop}*Req)#rs{});
        RETURN;
      END;
    END;
  END MoveScreen;
  PROCEDURE MoveWindow;
  BEGIN
    mode(add);
    LOOP
      IF WndAborted THEN
        RETURN
      ELSIF (wndX1>wndX2) OR (wndY1>wndY2) THEN
        LineXs:=scrX; LineXe:=scrX+799*wndsX;
        LineYs:=scrY; LineYe:=scrY+251*wndsY;
        DoInBox;
        IF NOT WndAborted THEN
          wndX1:=LineXs; wndX2:=LineXe;
          wndY1:=LineYs; wndY2:=LineYe;
        ELSE
          fill(0); Bad:=Bad+rs{wnd,crs,txt}; TextSet:={0..3};
        END;
      ELSIF wndX1-scrX>0 THEN
        LineXs:=scrX; LineXe:=wndX1; LineYs:=wndY1; LineYe:=wndY2;
        DoInBox;
        IF NOT WndAborted THEN wndX1:=LineXs END;
      ELSIF scrX+799*wndsX-wndX2>0 THEN
        LineXs:=wndX2; LineXe:=scrX+799*wndsX; LineYs:=wndY1; LineYe:=wndY2;
        DoInBox;
        IF NOT WndAborted THEN wndX2:=LineXe END;
      ELSIF wndY1-scrY>0 THEN
        LineXs:=wndX1; LineXe:=wndX2; LineYs:=scrY; LineYe:=wndY1;
        DoInBox;
        IF NOT WndAborted THEN wndY1:=LineYs END;
      ELSIF scrY+251*wndsY-wndY2>0 THEN
        LineXs:=wndX1; LineXe:=wndX2; LineYs:=wndY2; LineYe:=scrY+251*wndsY;
        DoInBox;
        IF NOT WndAborted THEN wndY2:=LineYe END;
      ELSE
        RETURN
      END;
    END;
  END MoveWindow;
BEGIN
  EnterGate(Lock);
  sx:=ScaleX; sy:=ScaleY; rollXs:=rollX*sx; rollYs:=rollY*sy;
  x1:=WindowW; x2:=WindowE; y1:=WindowS; y2:=WindowN;
  EXCL(Req,wnd);
  INCL(Bad,wnd);
  ExitGate(Lock);
  CursorOFF;
  WndAborted:=FALSE;
  MoveScreen;
  wndsX:=sx; wndsY:=sy;
  MoveWindow;
  IF NOT WndAborted THEN
    mode(add); color(3);
    x1:=(          -scrX) DIV wndsX; y1:=(          -scrY) DIV wndsY;
    x2:=(Board^.ctX-scrX) DIV wndsX; y2:=(Board^.ctY-scrY) DIV wndsY;
    vect(x1,y1,x2,y1); vect(x2,y1,x2,y2); vect(x2,y2,x1,y2); vect(x1,y2,x1,y1);
    EXCL(Bad,wnd);
  END;
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
  X1:=ModelMisc.X1;
  X2:=ModelMisc.X2;
  Y1:=ModelMisc.Y1;
  Y2:=ModelMisc.Y2;
  Size:=ModelMisc.Size;
  Layer:=ModelMisc.Layer;
  DoConductor;
  Send(SegOK);
END AppSeg;

PROCEDURE DelSeg;
BEGIN
  EXCL(Req,del);
  mode(com);
  X1:=ModelMisc.X1;
  X2:=ModelMisc.X2;
  Y1:=ModelMisc.Y1;
  Y2:=ModelMisc.Y2;
  Size:=ModelMisc.Size;
  Layer:=ModelMisc.Layer;
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

PROCEDURE OpenWindow(o: Object; scale,x,y: INTEGER);
BEGIN
  IF o=NIL THEN CloseWindow; RETURN END;
  IF NOT Open THEN
    wndX1:=0; wndX2:=-1; wndY1:=0; wndY2:=-1;
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
--PutHis(s,Outer^.pp);
--Terminal.print('%s\n',s);
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

PROCEDURE Drow;
BEGIN
  IF NOT Open THEN RETURN END;
  INCL(Req,app); Send(OutRequest); Wait(SegOK);
END Drow;

PROCEDURE Delete;
  VAR s: ARRAY [0..25] OF CHAR;
BEGIN
  IF NOT Open THEN RETURN END;
  INCL(Req,del); Send(OutRequest); Wait(SegOK);
END Delete;

VAR i: INTEGER;

BEGIN
  Open:=FALSE;
  TextSet:={};
  FOR i:=0 TO 3 DO image0(TextBuf[i],'') END;
  Final(CloseWindow);
  mask:={0..15};
END pedScreen.
