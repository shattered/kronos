IMPLEMENTATION MODULE bmpGrafic; (* Sem 26-May-87. (c) KRONOS *)

FROM Pattern    IMPORT  Match;
FROM Strings    IMPORT  Str1;
FROM Terminal   IMPORT  print, ClearLine;
FROM Model      IMPORT  Objects, Iterate, Object, fixed, Lget;
FROM ModelMisc  IMPORT  StartConductor, NextConductor, Empty,
                        X1, X2, Y1, Y2, Size, Layer;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM FsPublic   IMPORT  FileName, File;
FROM BIO        IMPORT  bWrite, Create, checkHALT, CD, Link, SetEof, Close,
                        OpenOnDir, bRead;
FROM Image      IMPORT  image0;
IMPORT S: SYSTEM;
IMPORT T: Terminal;
IMPORT mcd: mCodeMnem;

TYPE

  ADDRESS = S.ADDRESS;
  WORD    = S.WORD;

  BMD =
  RECORD
    w,h : INTEGER;
    wpl : INTEGER;
    base: ADDRESS;
    patt: BITSET;
  END;

  CRF_CONTEXT =
        RECORD
          x,y,co,xn,yn: INTEGER;
          do: BOOLEAN;
        END;

  TRF_CONTEXT =
        RECORD
          x,y,co,xn,yn,Dx,Dy,dx,dy,xl: INTEGER;
          Gx,case: BOOLEAN;
        END;

  Pattern = POINTER TO ARRAY [0..32] OF POINTER TO ARRAY [0..0] OF BITSET;

VAR  Window     : POINTER TO ARRAY [0..0] OF BITSET;
     Wsz        : INTEGER;
     Wsz0       : INTEGER;
     WindowW    : INTEGER;
     WindowE    : INTEGER;
     WindowS    : INTEGER;
     WindowN    : INTEGER;
     WindowX    : INTEGER;
     WindowY    : INTEGER;
     WindowY0   : INTEGER;
     WindowY1   : INTEGER;
     Board      : Object;
     Font       : ARRAY [0c..377c] OF ARRAY [0..11] OF CHAR;
     setPoint   : PROCEDURE (INTEGER,INTEGER);
     Scale      : INTEGER;
     patt       : ARRAY [0..79] OF CHAR;
     bmd        : BMD;
     Patt       : Pattern;
     Patterns   : ARRAY [0..2] OF Pattern;
     pass       : INTEGER;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE hline(x,y,x1: INTEGER);

  PROCEDURE bitmove(mode: INTEGER;
                      to: ADDRESS;   to_ofs: INTEGER;
                    from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
  CODE 0F9h 02h END bitmove;

  VAR d: INTEGER;

BEGIN
  IF y < 0     THEN RETURN END;
  IF y >=bmd.h THEN RETURN END;
  IF x > x1    THEN d:=x1; x1:=x; x:=d END;  (* swap: x<x1 *)
  IF x < 0     THEN x :=0       END; (* clip *)
  IF x1>=bmd.w THEN x1:=bmd.w-1 END; (* clip *)
  d:=x1-x+1;
  IF d<=0 THEN RETURN END; (* out of screen *)
  bitmove(1,bmd.base+y*bmd.wpl,x,Patt^[y MOD 32],x,d);
END hline;

PROCEDURE round(X,Y,r: INTEGER);

  PROCEDURE crf(VAR crf_context: CRF_CONTEXT);
  CODE 0F9h 009h END crf;

  VAR crf_context: CRF_CONTEXT;

BEGIN
  IF ((X+r)<0) OR ((Y+r)<0) OR ((X-r)>=bmd.w) OR ((Y-r)>=bmd.h)
  THEN RETURN END;
  WITH crf_context DO
    x:=r; y:=0; co:=r DIV 2;
    hline(X-x,Y+y,X+x);
    LOOP
      crf(crf_context);
      hline(X-x,Y+y,X+x);
      hline(X-x,Y-y,X+x);
      IF do THEN
        IF (xn#y)&(yn#x) THEN
          hline(X-yn,Y+xn,X+yn);
          hline(X-yn,Y-xn,X+yn);
        END;
      END;
      IF y>=x THEN EXIT END;
    END;
  END;
END round;

PROCEDURE trian(x0,y0,x1,y1,x2,y2: INTEGER);

  PROCEDURE trf(VAR context: TRF_CONTEXT);
  CODE 0F9h 008h END trf;

  VAR con1,con2: TRF_CONTEXT;
      by,bx,yl: INTEGER;

BEGIN
  IF y1<y0 THEN
    by:=y0; bx:=x0; y0:=y1; x0:=x1; y1:=by; x1:=bx;
  END;
  IF y2<y0 THEN
    by:=y0; bx:=x0; y0:=y2; x0:=x2; y2:=by; x2:=bx;
  END;
  IF (x1-x0)*(y2-y0)>(x2-x0)*(y1-y0) THEN
    by:=y2; bx:=x2; y2:=y1; x2:=x1; y1:=by; x1:=bx;
  END;
  IF y1<y2 THEN yl:=y1 ELSE yl:=y2 END;
  WITH con1 DO
    x:=x0; y:=y0; xn:=x; yn:=y; xl:=x1;
    Dx:=x1-x0; Dy:=y1-y0;
    IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=TRUE ELSE dx:=1; case:=FALSE END;
    IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
    IF Dx>=Dy THEN
      Gx:=TRUE;  co:=Dx DIV 2
    ELSE
      Gx:=FALSE; co:=Dy DIV 2
    END;
  END;
  WITH con2 DO
    x:=x0; y:=y0; xn:=x; yn:=y; xl:=x2;
    Dx:=x2-x0; Dy:=y2-y0;
    IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=FALSE ELSE dx:=1; case:=TRUE END;
    IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
    IF Dx>=Dy THEN
      Gx:=TRUE;  co:=Dx DIV 2
    ELSE
      Gx:=FALSE; co:=Dy DIV 2
    END;
  END;
  LOOP
    IF con1.dx<0 THEN trf(con1); END;
    IF con2.dx>0 THEN trf(con2); END;
    hline(con1.xn,con1.yn,con2.xn);
    IF con1.yn=yl THEN EXIT END;
    IF con1.dx>0 THEN trf(con1); END;
    IF con2.dx<0 THEN trf(con2); END;
  END;
  IF y1=y2 THEN RETURN END;
  IF yl=y1 THEN
    yl:=y2;
    WITH con1 DO
      x:=x1; y:=y1; xn:=x; yn:=y; xl:=x2;
      Dx:=x2-x1; Dy:=y2-y1;
      IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=TRUE ELSE dx:=1; case:=FALSE END;
      IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
      IF Dx>=Dy THEN
        Gx:=TRUE;  co:=Dx DIV 2
      ELSE
        Gx:=FALSE; co:=Dy DIV 2
      END;
      IF dx<0 THEN trf(con1) END;
    END;
  ELSE
    yl:=y1;
    WITH con2 DO
      x:=x2; y:=y2; xn:=x; yn:=y; xl:=x1;
      Dx:=x1-x2; Dy:=y1-y2;
      IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=FALSE ELSE dx:=1; case:=TRUE END;
      IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
      IF Dx>=Dy THEN
        Gx:=TRUE;  co:=Dx DIV 2
      ELSE
        Gx:=FALSE; co:=Dy DIV 2
      END;
      IF dx>0 THEN trf(con2) END;
    END;
  END;
  REPEAT
    trf(con1); trf(con2);
      hline(con1.xn,con1.yn,con2.xn);
  UNTIL con1.yn=yl;
END trian;

PROCEDURE Sqrt(n: INTEGER): INTEGER;
  VAR r,l: INTEGER;
BEGIN
  IF n<2 THEN RETURN n END;
  l:=1; r:=n;
  REPEAT r:=(l+r) DIV 2; l:=n DIV r UNTIL l>=r;
  RETURN r
END Sqrt;

PROCEDURE DrowSeg(l: INTEGER);
  VAR x1,x2,y1,y2,sz,rx,ry: INTEGER;
      y1l,y2l,y1r,y2r     : INTEGER;
      x1l,x2l,x1r,x2r     : INTEGER;
      x,y                 : INTEGER;
BEGIN
  x1:=X1; x2:=X2; y1:=Y1; y2:=Y2; sz:=Size;

  IF y2<y1 THEN
    y:=y1; x:=x1; y1:=y2; x1:=x2; y2:=y; x2:=x  (* swap coordinates *)
  END;

  IF    x1=x2 THEN ry:=0; rx:=sz
  ELSIF y1=y2 THEN ry:=sz; rx:=0
  ELSE
    ry:=Sqrt(TRUNC(FLOAT(sz*sz)*FLOAT((x2-x1)*(x2-x1))/
            FLOAT((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)) ));
    rx:=Sqrt(TRUNC(FLOAT(sz*sz)*FLOAT((y2-y1)*(y2-y1))/
            FLOAT((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)) ));
  END;

  y1:=(y1 + Scale DIV 2) DIV Scale; y2:=(y2 + Scale DIV 2) DIV Scale;
  x1:=(x1 + Scale DIV 2) DIV Scale; x2:=(x2 + Scale DIV 2) DIV Scale;
  ry:=(ry + Scale DIV 2) DIV Scale;
  rx:=(rx + Scale DIV 2) DIV Scale;
  sz:=(sz + Scale DIV 2) DIV Scale;

  IF x2>x1 THEN y1l:=y1+ry; y2l:=y2+ry; y1r:=y1-ry; y2r:=y2-ry
  ELSE          y1l:=y1-ry; y2l:=y2-ry; y1r:=y1+ry; y2r:=y2+ry END;

  x1l:=x1-rx; x2l:=x2-rx; x1r:=x1+rx; x2r:=x2+rx;

IF NOT (l IN {0..2}) THEN RETURN END;

  Patt:=Patterns[l];
  y1:=  y1-WindowS;
  y2:=  y2-WindowS;
  y1l:=y1l-WindowS;
  y1r:=y1r-WindowS;
  y2l:=y2l-WindowS;
  y2r:=y2r-WindowS;
  round(x1,y1,sz);
  round(x2,y2,sz);
  trian(x2l,y2l,x2r,y2r,x1l,y1l);
  trian(x1r,y1r,x2r,y2r,x1l,y1l);

END DrowSeg;

PROCEDURE DrowObj(o,i: Object);
  VAR l: INTEGER;
BEGIN
  IF (Tag(o)=signal)& Match(patt,o^.Name) THEN
    print('%s\r',o^.Name);
    StartConductor(o,FALSE);
    WHILE NOT Empty DO
      DrowSeg(INTEGER(Layer)-1);
      NextConductor;
    END;
    ClearLine;
  END;
END DrowObj;

  VAR Name: FileName; fl: File;
      BufCnt, bNo: INTEGER;
      Buf: ARRAY [0..1023] OF S.WORD;

PROCEDURE WrBuf;
  VAR i: INTEGER;
BEGIN
  IF BufCnt>0 THEN
    checkHALT(bWrite(fl,1+bNo,S.ADR(Buf),4096),Name);
    BufCnt:=0; INC(bNo);
  END;
END WrBuf;

PROCEDURE PutW(W: S.WORD);
BEGIN
  IF BufCnt>=1024 THEN WrBuf END;
  Buf[BufCnt]:=W; INC(BufCnt);
END PutW;

PROCEDURE move(to,from: ADDRESS; sz: INTEGER);
CODE mcd.move END move;

PROCEDURE WriteWindow;
  VAR wnd: RECORD sz,w,e,s,n: INTEGER END;
      i,j: INTEGER;
BEGIN
  IF pass=0 THEN
    image0(Name,'%s.bmp',Board^.Name);
    WITH wnd DO
      sz:=Wsz;
      w:=WindowW; e:=WindowE;
      s:=WindowS; n:=WindowN;
    END;
    print('WindowN %d\n',WindowN);
    checkHALT(Create(fl),Name);
    checkHALT(bWrite(fl,0,S.ADR(wnd),SIZE(wnd)*4),Name);
    FOR i:=0 TO WindowY0-1 DO
      FOR j:=0 TO bmd.wpl-1 DO
        (*$T-*)
        PutW(Window^[i*bmd.wpl+j])
        (*$T+*);
      END;
    END;
  ELSIF pass=1 THEN
    FOR i:=0 TO WindowY1-1 DO
      FOR j:=0 TO bmd.wpl-1 DO
        (*$T-*) PutW(Window^[i*bmd.wpl+j]) (*$T+*);
      END;
    END;
    WrBuf;
    SetEof(fl,4096+Wsz*4);
    checkHALT(Link(CD(),Name,fl),Name);
    checkHALT(Close(fl),Name);
  ELSE
    image0(Name,'%s.bmp',Board^.Name);
    WITH wnd DO
      sz:=Wsz;
      w:=WindowW; e:=WindowE;
      s:=WindowS; n:=WindowN;
    END;
    print('WindowN %d\n',WindowN);
    checkHALT(Create(fl),Name);
    checkHALT(bWrite(fl,0,S.ADR(wnd),SIZE(wnd)*4),Name);
    FOR i:=0 TO WindowY-1 DO
      FOR j:=0 TO bmd.wpl-1 DO
        (*$T-*)
        PutW(Window^[i*bmd.wpl+j])
        (*$T+*);
      END;
    END;
    WrBuf;
    SetEof(fl,4096+Wsz*4);
    checkHALT(Link(CD(),Name,fl),Name);
    checkHALT(Close(fl),Name);
  END;
END WriteWindow;

PROCEDURE Drow;
  VAR i,j: INTEGER;
BEGIN
  Wsz:=(WindowX+31) DIV 32 * WindowY;
  T.print('Wsz %d\n',Wsz);
  WindowY0:=(WindowY+1) DIV 2;
  WindowY1:=WindowY-WindowY0;
  Wsz0:=(WindowX+31) DIV 32 * WindowY0;
  Allocate(Window,Wsz0);
  bmd.h   :=WindowY0;
  bmd.w   :=WindowX;
  bmd.wpl :=(bmd.w+31) DIV 32;
  bmd.base:=Window;
  bmd.patt:={0..31};
  FOR i:=0 TO HIGH(Patterns) DO
    Allocate(Patterns[i],SIZE(Patterns[i]^));
  END;
  FOR i:=0 TO HIGH(Patterns) DO
    FOR j:=0 TO 31 DO
      Allocate(Patterns[i]^[j],bmd.wpl);
    END;
  END;
  (*$T-*)
  Patterns[0]^[1]^[0]:={1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31};
  Patterns[0]^[1]^[0]:=Patterns[0]^[1]^[0]<<1;
  Patterns[0]^[0]^[0]:={};
  FOR i:=1 TO bmd.wpl-1 DO
    Patterns[0]^[0]^[i]:=Patterns[0]^[0]^[0];
    Patterns[0]^[1]^[i]:=Patterns[0]^[1]^[0];
  END;
  FOR i:=2 TO 31 DO
    FOR j:=0 TO bmd.wpl-1 DO
      Patterns[0]^[i]^[j]:=Patterns[0]^[i MOD 2]^[j];
    END;
  END;
  Patterns[1]^[0]^[0]:={0,4,8,12,16,20,24,28};
  Patterns[1]^[2]^[0]:=Patterns[1]^[0]^[0]>>2;
  Patterns[1]^[1]^[0]:=Patterns[0]^[1]^[0]>>1;
  Patterns[1]^[3]^[0]:=Patterns[1]^[1]^[0];
  FOR i:=1 TO bmd.wpl-1 DO
    Patterns[1]^[0]^[i]:=Patterns[1]^[0]^[0];
    Patterns[1]^[2]^[i]:=Patterns[1]^[2]^[0];
    Patterns[1]^[1]^[i]:=Patterns[1]^[1]^[0];
    Patterns[1]^[3]^[i]:=Patterns[1]^[3]^[0];
  END;
  FOR i:=4 TO 31 DO
    FOR j:=0 TO bmd.wpl-1 DO
      Patterns[1]^[i]^[j]:=Patterns[1]^[i MOD 4]^[j];
    END;
  END;
  FOR i:=0 TO 31 DO
    FOR j:=0 TO bmd.wpl-1 DO
      Patterns[2]^[i]^[j]:={0..31}; --Patterns[0]^[i]^[j]+Patterns[1]^[i]^[j];
    END;
  END;
  (*$T+*)
  FOR kk: INTEGER :=0 TO 1 DO
    pass:=kk;
    T.print('pass %d\n',pass);
    FOR i:=0 TO Wsz0-1 DO (*$T-*) Window^[i]:={} (*$T+*) END;
    IF pass=1 THEN bmd.h:=WindowY1; WindowS:=WindowY0 END;
    Iterate(Board^.All,DrowObj,0);
    WriteWindow;
  END;

  Deallocate(Window,Wsz0);
  FOR i:=0 TO HIGH(Patterns) DO
    FOR j:=0 TO 31 DO
      Deallocate(Patterns[i]^[j],(WindowX+31) DIV 32);
    END;
  END;
  FOR i:=0 TO HIGH(Patterns) DO
    Deallocate(Patterns[i],SIZE(Patterns[i]^));
  END;
END Drow;

PROCEDURE Print(mdl: Object; s: INTEGER; sg: ARRAY OF CHAR);
  VAR Xsize,Ysize,Cnt: INTEGER; Name: ARRAY [0..79] OF CHAR;
BEGIN
  Board:=mdl; Scale:=s;
  Xsize:=(mdl^.ctX + Scale DIV 2) DIV Scale;
  Ysize:=(mdl^.ctY + Scale DIV 2) DIV Scale;
  WindowS:=0; WindowN:=Ysize-1;
  WindowW:=0; WindowE:=Xsize-1;
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  Str1(patt,sg);
  Drow;
END Print;

PROCEDURE PrintPins(mdl: Object; Scale: INTEGER);
BEGIN END PrintPins;

PROCEDURE Vector(X,Y,x,y: INTEGER);
  VAR dx,dy,Xcnt,Ycnt,adx,ady,i: INTEGER;
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

PROCEDURE DrowChipPin(p,i: Object);
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

VAR chY, chX, chR: INTEGER;

PROCEDURE Point(x,y: INTEGER);
  TYPE b=BOOLEAN; s=BITSET; c=INTEGER;
  VAR i: INTEGER;
BEGIN
(*$T-*)
  IF b(s(x>=WindowW)*s(x<=WindowE)*s(y>=WindowS)*s(y<=WindowN)) THEN
    y:=y-WindowS;
    x:=x-WindowW;
    INCL(Window^[y*bmd.wpl+(x DIV 32)],x MOD 32);

    --i:=(x-WindowW)+(y-WindowS)*WindowX;
    --INCL(Window^[i DIV 32],c(s(i)*{0..4}));
  END;
(*$T+*)
END Point;

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

PROCEDURE DrowChip(o,i: Object);
  VAR x,y,xs,ys: INTEGER; t: Object;
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  T.print('%s\r',o^.Name);
  chX:=o^.XB DIV Scale; chY:=o^.YB DIV Scale; chR:=o^.RB;
  t:=o^.ChipType;
  Vector(0,0,t^.ctX DIV Scale,0);
  Vector(t^.ctX DIV Scale,0,t^.ctX DIV Scale,t^.ctY DIV Scale);
  Vector(t^.ctX DIV Scale,t^.ctY DIV Scale,0,t^.ctY DIV Scale);
  Vector(0,t^.ctY DIV Scale,0,0);
  Iterate(o^.Pins,DrowChipPin,0);
  Text(10,10,o^.Name);
END DrowChip;

PROCEDURE DrowChips;
  VAR i: INTEGER;
BEGIN
  Wsz:=(WindowX+31) DIV 32 * WindowY;
  Allocate(Window,Wsz);
  FOR i:=0 TO Wsz-1 DO (*$T-*) Window^[i]:={} (*$T+*) END;
  Iterate(Board^.All,DrowChip,0);
  chX:=0; chY:=0; chR:=0;
  Vector(0,0,Board^.ctX DIV Scale,0);
  Vector(Board^.ctX DIV Scale,0,Board^.ctX DIV Scale,Board^.ctY DIV Scale);
  Vector(Board^.ctX DIV Scale,Board^.ctY DIV Scale-1,0,Board^.ctY DIV Scale-1);
  Vector(0,Board^.ctY DIV Scale,0,0);
  WriteWindow;
  Deallocate(Window,Wsz);
END DrowChips;

CONST font='FONT.fnt';

PROCEDURE PrintChips(mdl: Object; s: INTEGER);
  VAR Xsize,Ysize,Cnt: INTEGER; Name: ARRAY [0..79] OF CHAR;
      f: File;
BEGIN
  checkHALT(OpenOnDir(CD(),f,font),font);
  checkHALT(bRead(f,0,S.ADR(Font),SIZE(Font)*4),font);
  checkHALT(Close(f),font);
  Board:=mdl; Scale:=s;
  Xsize:=mdl^.ctX DIV Scale +1;
  Ysize:=mdl^.ctY DIV Scale +1;
  WindowS:=0; WindowN:=Ysize-1;
  WindowW:=0; WindowE:=Xsize-1;
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  bmd.h   :=WindowY;
  bmd.w   :=WindowX;
  bmd.wpl :=(bmd.w+31) DIV 32;
  bmd.base:=Window;
  bmd.patt:={0..31};
  setPoint:=chPoint;
  pass:=-1;
  DrowChips;
END PrintChips;

BEGIN
  BufCnt:=0; bNo:=0;
END bmpGrafic.
