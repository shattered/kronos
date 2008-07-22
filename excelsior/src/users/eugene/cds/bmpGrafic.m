IMPLEMENTATION MODULE bmpGrafic; (* Sem 26-May-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Pattern    IMPORT  Match;
FROM Strings    IMPORT  Str1;
FROM Terminal   IMPORT  print, ClearLine;
FROM Model      IMPORT  Objects, Tag, Iterate, Object, fixed, Lget;
FROM ModelMisc  IMPORT  StartConductor, NextConductor, Empty,
                        X1, X2, Y1, Y2, Size, Layer, PinLocation;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM FsPublic   IMPORT  FileName, File;
FROM BIO        IMPORT  bWrite, Create, checkHALT, CD, Link, SetEof, Close,
                        OpenOnDir, bRead;
FROM Image      IMPORT  image0;
FROM cdsSeq     IMPORT  PinNet;

VAR  X,Y        : CARDINAL;
     Window     : POINTER TO ARRAY [0..0] OF BITSET;
     Wsz        : INTEGER;
     WindowW    : CARDINAL;
     WindowE    : CARDINAL;
     WindowS    : CARDINAL;
     WindowN    : CARDINAL;
     WindowX    : CARDINAL;
     WindowY    : CARDINAL;
     Board      : Object;
     Scale      : CARDINAL;
     Font       : ARRAY [0c..377c] OF ARRAY [0..11] OF CHAR;
     setPoint   : PROCEDURE (INTEGER,INTEGER);
     patt       : ARRAY [0..79] OF CHAR;

PROCEDURE Point(x,y: CARDINAL);
  TYPE b=BOOLEAN; s=BITSET; c=CARDINAL;
  VAR i: CARDINAL;
BEGIN
(*$T-*)
  IF b(s(x>=WindowW)*s(x<=WindowE)*s(y>=WindowS)*s(y<=WindowN)) THEN
    i:=(x-WindowW)+(y-WindowS)*WindowX;
    INCL(Window^[i DIV 32],c(s(i)*{0..4}));
  END;
(*$T+*)
END Point;

PROCEDURE Pnt?(x,y: CARDINAL): BOOLEAN;
  TYPE b=BOOLEAN; s=BITSET; c=CARDINAL;
  VAR i: CARDINAL;
BEGIN
(*$T-*)
  IF ODD(x)&ODD(y)&
    b(s(x>=WindowW)*s(x<=WindowE)*s(y>=WindowS)*s(y<=WindowN)) THEN
    i:=(x-WindowW)+(y-WindowS)*WindowX;
    RETURN c(s(i)*{0..4}) IN Window^[i DIV 32];
  END;
(*$T+*)
  RETURN FALSE;
END Pnt?;

PROCEDURE Vector(X,Y,x,y: CARDINAL);
  VAR dx,dy,Xcnt,Ycnt,adx,ady,i: CARDINAL;
BEGIN
  IF X=x THEN
    LOOP
      setPoint(X,Y);
      IF y=Y THEN RETURN END;
      IF y<Y THEN DEC(Y) ELSE INC(Y) END;
    END;
  END;
  IF Y=y THEN
    LOOP
      setPoint(X,Y);
      IF x=X THEN RETURN END;
      IF x<X THEN DEC(X) ELSE INC(X) END;
    END;
  END;
  dx:=x-X; dy:=y-Y;
  adx:=ABS(dx); ady:=ABS(dy);
  Xcnt:=adx DIV 2; Ycnt:=ady DIV 2;
  IF dx>dy THEN
    LOOP
      setPoint(X,Y);
      IF X=x THEN setPoint(x,y); RETURN END;
      IF dx>0 THEN INC(X) ELSE DEC(X) END;
      INC(Ycnt,ady);
      IF Ycnt>=adx THEN
        DEC(Ycnt,adx);
        IF dy>0 THEN INC(Y) ELSE DEC(Y) END;
      END;
    END;
  ELSE
    LOOP
      setPoint(X,Y);
      IF Y=y THEN setPoint(x,y); RETURN END;
      IF dy>0 THEN INC(Y) ELSE DEC(Y) END;
      INC(Xcnt,adx);
      IF Xcnt>=ady THEN
        DEC(Xcnt,ady);
        IF dx>0 THEN INC(X) ELSE DEC(X) END;
      END;
    END;
  END;
END Vector;

PROCEDURE Char(x,y,s: INTEGER;ch: CHAR);
  VAR i,j,ci,cj: INTEGER;
BEGIN
  FOR i:=0 TO 11 DO
    FOR j:=0 TO 7 DO
      IF j IN BITSET(Font[ch][i]) THEN
        FOR ci:=0 TO s-1 DO
          FOR cj:=0 TO s-1 DO setPoint(x+j*s+cj,y+(11-i)*s+ci) END;
        END;
      END;
    END;
  END;
END Char;

PROCEDURE Text(x,y: INTEGER; s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(s))&(s[i]#0c) DO Char(x,y,2,s[i]); INC(x,20); INC(i) END;
END Text;

PROCEDURE Sqrt(n: CARDINAL): CARDINAL;
  VAR r,l: CARDINAL;
BEGIN
  IF n<2 THEN RETURN n END;
  l:=1; r:=n;
  REPEAT r:=(l+r) DIV 2; l:=n DIV r UNTIL l>=r;
  RETURN r
END Sqrt;

PROCEDURE DrowSeg(l: INTEGER);
  VAR x1,x2,y1,y2,sz,rx,ry: CARDINAL;
      y1l,y2l,y1r,y2r,xmin,xmax,ymin,ymax: INTEGER;
      x1l,x2l,x1r,x2r: INTEGER;
      x,y: INTEGER;
BEGIN
  x1:=X1; x2:=X2; y1:=Y1; y2:=Y2; sz:=Size;
  IF y2<y1 THEN y:=y1; x:=x1; y1:=y2; x1:=x2; y2:=y; x2:=x END;
  IF x1=x2 THEN
    ry:=0; rx:=sz
  ELSIF y1=y2 THEN
    ry:=sz; rx:=0;
  ELSE
    ry:=Sqrt(TRUNC(FLOAT(sz*sz)*FLOAT((x2-x1)*(x2-x1))/
            FLOAT((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)) ));
    rx:=Sqrt(TRUNC(FLOAT(sz*sz)*FLOAT((y2-y1)*(y2-y1))/
            FLOAT((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)) ));
  END;
  y1:=y1 DIV Scale; y2:=y2 DIV Scale;
  x1:=x1 DIV Scale; x2:=x2 DIV Scale;
  ry:=(ry+Scale DIV 2) DIV Scale;
  rx:=(rx+Scale DIV 2) DIV Scale;
  sz:=(sz+Scale DIV 2) DIV Scale;
  ymin:=y1-sz; ymax:=y2+sz;
  IF x2>x1 THEN
    y1l:=y1+ry; y2l:=y2+ry; y1r:=y1-ry; y2r:=y2-ry;
  ELSE
    y1l:=y1-ry; y2l:=y2-ry; y1r:=y1+ry; y2r:=y2+ry;
  END;
  x1l:=x1-rx; x2l:=x2-rx; x1r:=x1+rx; x2r:=x2+rx;
  y:=ymin;
  FOR y:=ymin TO ymax DO
    IF (l#0) OR ODD(y) THEN
      IF y1l=y2l THEN
        IF x2>x1 THEN
          xmin:=x1-Sqrt(sz*sz-(y-y1)*(y-y1));
        ELSE
          xmin:=x2-Sqrt(sz*sz-(y-y2)*(y-y2));
        END;
      ELSIF y<=y1l THEN
        xmin:=x1-Sqrt(sz*sz-(y-y1)*(y-y1));
      ELSIF y>=y2l THEN
        xmin:=x2-Sqrt(sz*sz-(y-y2)*(y-y2));
      ELSE
        xmin:=x1-rx+(y-y1l)*(x2-x1) DIV (y2-y1);
      END;
      IF y1r=y2r THEN
        IF x2<x1 THEN
          xmax:=x1+Sqrt(sz*sz-(y-y1)*(y-y1));
        ELSE
          xmax:=x2+Sqrt(sz*sz-(y-y2)*(y-y2));
        END;
      ELSIF y<=y1r THEN
        xmax:=x1+Sqrt(sz*sz-(y-y1)*(y-y1));
      ELSIF y>=y2r THEN
        xmax:=x2+Sqrt(sz*sz-(y-y2)*(y-y2));
      ELSE
        xmax:=x1+rx+(y-y1r)*(x2-x1) DIV (y2-y1);
      END;
      FOR x:=xmin TO xmax DO
        IF (l=0)&ODD(x)&ODD(y) THEN setPoint(x,y) END;
        IF (l=1)&(
          ((x+y)MOD 4=1)OR(x MOD 4=(y+1) MOD 4)
          OR Pnt?(x+1,y+1) OR Pnt?(x,y+1)
          ) THEN
          setPoint(x,y)
        END;
      END;
    END;
  END;
END DrowSeg;

VAR layer: INTEGER;

PROCEDURE DrowObj(o: Object);
  VAR l: INTEGER;
BEGIN
  IF (Tag(o)=signal)& Match(patt,o^.Name) THEN
    print('%s\r',o^.Name);
    StartConductor(o);
    WHILE NOT Empty DO
      IF layer IN Layer THEN DrowSeg(layer) END;
      NextConductor;
    END;
    ClearLine;
  END;
END DrowObj;

PROCEDURE DrowPin(p: Object);
  VAR x,y,i: INTEGER;
BEGIN
  PinLocation(p,x,y);
  x:=x DIV Scale; y:=y DIV Scale;
  FOR i:=x-10 TO x+10 DO setPoint(i,y) END;
  FOR i:=y-10 TO y+10 DO setPoint(x,i) END;
END DrowPin;

PROCEDURE DrowNet(x1,y1,x2,y2: INTEGER);
BEGIN
  x1:=x1 DIV Scale; x2:=x2 DIV Scale; y1:=y1 DIV Scale; y2:=y2 DIV Scale;
  Vector(x1,y1,x2,y2);
  Vector(x1+1,y1,x2+1,y2);
  Vector(x1,y1+1,x2,y2+1);
END DrowNet;

PROCEDURE DrowSig(o: Object);
BEGIN
  IF Tag(o)=signal THEN
    IF NOT (fixed IN o^.sType) THEN PinNet(o,DrowNet) END;
    Iterate(o^.TiedPins,DrowPin);
  END;
END DrowSig;

VAR chX,chY,chR: INTEGER;

PROCEDURE chPoint(x,y: INTEGER);
  VAR x1,y1: INTEGER;
BEGIN
  CASE chR MOD 4 OF
    0: x1:=chX+x; y1:=chY+y;
   |1: x1:=chX+y; y1:=chY-x;
   |2: x1:=chX-x; y1:=chY-y;
   |3: x1:=chX-y; y1:=chY+x;
  END;
  Point(x1,y1);
END chPoint;

PROCEDURE DrowChipPin(p: Object);
  VAR t,e: Object; x,y: INTEGER;
BEGIN
  t:=p^.Chip; t:=t^.ChipType;
  e:=Lget(t^.ExternalPins,p^.No);
  x:=e^.PinX DIV Scale; y:=e^.PinY DIV Scale;
  Vector(x-8,y-8,x+8,y-8);
  Vector(x+8,y-8,x+8,y+8);
  Vector(x+8,y+8,x-8,y+8);
  Vector(x-8,y+8,x-8,y-8);
  IF p^.No=0 THEN
    Vector(x-8,y-8,x+8,y+8);
    Vector(x-8,y+8,x+8,y-8);
  END;
END DrowChipPin;

PROCEDURE DrowChip(o: Object);
  VAR x,y,xs,ys: INTEGER; t: Object;
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  chX:=o^.XB DIV Scale; chY:=o^.YB DIV Scale; chR:=o^.RB;
  t:=o^.ChipType;
  Vector(0,0,t^.ctX DIV Scale,0);
  Vector(t^.ctX DIV Scale,0,t^.ctX DIV Scale,t^.ctY DIV Scale);
  Vector(t^.ctX DIV Scale,t^.ctY DIV Scale,0,t^.ctY DIV Scale);
  Vector(0,t^.ctY DIV Scale,0,0);
  Iterate(o^.Pins,DrowChipPin);
  Text(10,10,o^.Name);
END DrowChip;

PROCEDURE WriteWindow;
  VAR Name: FileName; fl: File;
      wnd: RECORD sz,w,e,s,n: INTEGER END;
BEGIN
  image0(Name,'%s.bmp',Board^.Name);
  WITH wnd DO
    sz:=Wsz;
    w:=WindowW; e:=WindowE;
    s:=WindowS; n:=WindowN;
  END;
  checkHALT(Create(fl),Name);
  checkHALT(bWrite(fl,0,ADR(wnd),SIZE(wnd)*4),Name);
  checkHALT(bWrite(fl,1,Window,Wsz*4),Name);
  SetEof(fl,4096+Wsz*4);
  checkHALT(Link(CD(),Name,fl),Name);
  checkHALT(Close(fl),Name);
END WriteWindow;

PROCEDURE Drow;
  VAR i: CARDINAL;
BEGIN
  Wsz:=WindowX*WindowY DIV 32 +1;
  Allocate(Window,Wsz);
  FOR i:=0 TO Wsz-1 DO (*$T-*) Window^[i]:={} (*$T+*) END;
  FOR layer:=0 TO 1 DO
    Iterate(Board^.All,DrowObj);
  END;
  WriteWindow;
  Deallocate(Window,Wsz);
END Drow;

PROCEDURE DrowPins;
  VAR i: CARDINAL;
BEGIN
  Wsz:=WindowX*WindowY DIV 32 +1;
  Allocate(Window,Wsz);
  FOR i:=0 TO Wsz-1 DO (*$T-*) Window^[i]:={} (*$T+*) END;
  Iterate(Board^.All,DrowSig);
  WriteWindow;
  Deallocate(Window,Wsz);
END DrowPins;

PROCEDURE DrowChips;
  VAR i: CARDINAL;
BEGIN
  Wsz:=WindowX*WindowY DIV 32 +1;
  Allocate(Window,Wsz);
  FOR i:=0 TO Wsz-1 DO (*$T-*) Window^[i]:={} (*$T+*) END;
  Iterate(Board^.All,DrowChip);
  chX:=0; chY:=0; chR:=0;
  Vector(0,0,Board^.ctX DIV Scale,0);
  Vector(Board^.ctX DIV Scale,0,Board^.ctX DIV Scale,Board^.ctY DIV Scale);
  Vector(Board^.ctX DIV Scale,Board^.ctY DIV Scale,0,Board^.ctY DIV Scale);
  Vector(0,Board^.ctY DIV Scale,0,0);
  WriteWindow;
  Deallocate(Window,Wsz);
END DrowChips;

PROCEDURE Print(mdl: Object; s: CARDINAL; sg: ARRAY OF CHAR);
  VAR Xsize,Ysize,Cnt: CARDINAL; Name: ARRAY [0..79] OF CHAR;
BEGIN
  Board:=mdl; Scale:=s;
  Xsize:=mdl^.ctX DIV Scale;
  Ysize:=mdl^.ctY DIV Scale;
  WindowS:=0; WindowN:=Ysize-1;
  WindowW:=0; WindowE:=Xsize-1;
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  setPoint:=Point;
  Str1(patt,sg);
  Drow;
END Print;

PROCEDURE PrintPins(mdl: Object; s: INTEGER);
  VAR Xsize,Ysize,Cnt: CARDINAL; Name: ARRAY [0..79] OF CHAR;
BEGIN
  Board:=mdl; Scale:=s;
  Xsize:=mdl^.ctX DIV Scale;
  Ysize:=mdl^.ctY DIV Scale;
  WindowS:=0; WindowN:=Ysize-1;
  WindowW:=0; WindowE:=Xsize-1;
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  setPoint:=Point;
  DrowPins;
END PrintPins;

CONST font='FONT.fnt';

PROCEDURE PrintChips(mdl: Object; s: INTEGER);
  VAR Xsize,Ysize,Cnt: CARDINAL; Name: ARRAY [0..79] OF CHAR;
      f: File;
BEGIN
  checkHALT(OpenOnDir(CD(),f,font),font);
  checkHALT(bRead(f,0,ADR(Font),SIZE(Font)*4),font);
  checkHALT(Close(f),font);
  Board:=mdl; Scale:=s;
  Xsize:=mdl^.ctX DIV Scale +1;
  Ysize:=mdl^.ctY DIV Scale +1;
  WindowS:=0; WindowN:=Ysize-1;
  WindowW:=0; WindowE:=Xsize-1;
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  setPoint:=chPoint;
  DrowChips;
END PrintChips;

END bmpGrafic.
