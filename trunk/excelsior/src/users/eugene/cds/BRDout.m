IMPLEMENTATION MODULE BRDout; (* Sem 03-Feb-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Model      IMPORT  Object, Objects, Iterate, Tag;
FROM ModelMisc  IMPORT  StartConductor, NextConductor, Empty,
                        X1, X2, Y1, Y2, Layer, Size, ViasSize;
FROM FsPublic   IMPORT  FileName, File;
FROM Image      IMPORT  image0, GetNum;
FROM Edit       IMPORT  ReadString;
FROM Terminal   IMPORT  print;
FROM BIO        IMPORT  Create, Link, Close, bWrite, checkHALT, SetEof, CD;

VAR x1,x2,y1,y2 : INTEGER;
    Buf         : ARRAY [0..4095] OF CHAR;
    BufCnt      : INTEGER;
    BufPtr      : INTEGER;
    Out         : File;
    OutName     : FileName;
    Masks       : ARRAY [0..255] OF INTEGER;
    Layers      : INTEGER;

PROCEDURE w16(w: INTEGER);
BEGIN
  IF BufPtr>HIGH(Buf) THEN
    checkHALT(bWrite(Out,BufCnt,ADR(Buf),4096),OutName);
    INC(BufCnt); BufPtr:=0;
  END;
  Buf[BufPtr]:=CHAR(w MOD 256); Buf[BufPtr+1]:=CHAR(w DIV 256);
  INC(BufPtr,2);
END w16;

PROCEDURE Unit(type,mask,layer: INTEGER);
BEGIN
  w16(mask+layer*256+type*4096);
  IF type=0 THEN RETURN END;
  w16(x1); w16(y1);
  IF type=1 THEN RETURN END;
  w16(x2); w16(y2);
END Unit;

PROCEDURE Sign(mask,x,y: INTEGER);
BEGIN
  x1:=x*25 DIV 24; x2:=x1;
  y1:=y*25 DIV 24; y2:=y1;
  Unit(1,mask,0);
END Sign;

PROCEDURE Line(mask: INTEGER; layer: BITSET; xs,ys,xe,ye: INTEGER);
  VAR i: INTEGER;
BEGIN
  x1:=xs*25 DIV 24; x2:=xe*25 DIV 24;
  y1:=ys*25 DIV 24; y2:=ye*25 DIV 24;
  FOR i:=0 TO Layers-1 DO
    IF i IN layer THEN Unit(2,mask,Layers-i) END;
  END;
END Line;

PROCEDURE Segment(size: INTEGER; layer: BITSET; xs,ys,xe,ye: INTEGER);
  VAR mask,i: INTEGER;
BEGIN
  mask:=0; size:=size*2;
  x1:=xs*25 DIV 24 + 600; x2:=xe*25 DIV 24 + 600;
  y1:=ys*25 DIV 24 + 600; y2:=ye*25 DIV 24 + 600;
  FOR i:=1 TO HIGH(Masks) DO
    IF ABS(Masks[i]-size)<ABS(Masks[mask]-size) THEN mask:=i END;
  END;
  FOR i:=0 TO Layers-1 DO
    IF i IN layer THEN Unit(2,mask,Layers-i) END;
  END;
END Segment;

PROCEDURE Vias(size,x,y: INTEGER);
  VAR mask,i: INTEGER;
BEGIN
  mask:=0; size:=size*2;
  x1:=x*25 DIV 24 + 600; x2:=x1;
  y1:=y*25 DIV 24 + 600; y2:=y1;
  FOR i:=1 TO HIGH(Masks) DO
    IF ABS(Masks[i]-size)<ABS(Masks[mask]-size) THEN mask:=i END;
  END;
  Unit(1,mask,0);
END Vias;

PROCEDURE Signal(s: Object);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  StartConductor(s);
  WHILE NOT Empty DO
    IF ViasSize#0 THEN Vias(Size,X1,Y1) END;
    IF (X1#X2)OR(Y1#Y2)OR(ViasSize=0) THEN Segment(Size,Layer,X1,Y1,X2,Y2) END;
    NextConductor;
  END;
END Signal;

PROCEDURE WriteBRD(mdl: Object);
  VAR ln: ARRAY [0..31] OF CHAR; m: BITSET; i: INTEGER;
BEGIN
  ASSERT(Tag(mdl)=chiptype);
  ln:='2';
  LOOP
    ReadString('Введите количество слоев в плате: ',ln); print('\n');
    IF GetNum(ln,Layers)>=0 THEN EXIT END;
    print('Не понял. ');
  END;
  BufPtr:=0; BufCnt:=0;
  image0(OutName,'%s.brd',mdl^.Name);
  checkHALT(Create(Out),OutName);
  Sign(24b,192,576);
  Sign(24b,1536,96);
  Sign(24b,10368,96);
  m:={}; FOR i:=0 TO Layers-1 DO INCL(m,i) END;
  Line(22b,m,3072,0,3552,0);
  Line(22b,m,3072,48,3552,48);
  Line(22b,m,3072,144,3552,144);
  Line(22b,m,3072,240,3552,240);
  Line(22b,m,8064,0,8544,0);
  Line(22b,m,8064,48,8544,48);
  Line(22b,m,8064,144,8544,144);
  Line(22b,m,8064,240,8544,240);
  Iterate(mdl^.All,Signal);
  Unit(0,0,0);
  IF BufPtr>0 THEN
    checkHALT(bWrite(Out,BufCnt,ADR(Buf),BufPtr),OutName);
  END;
  SetEof(Out,BufCnt*4096+BufPtr);
  checkHALT(Link(CD(),OutName,Out),OutName);
  checkHALT(Close(Out),OutName);
END WriteBRD;

PROCEDURE msk(d,n: INTEGER); BEGIN Masks[n]:=d*24 DIV 625 END msk;

VAR i: INTEGER;

BEGIN
  FOR i:=0 TO HIGH(Masks) DO Masks[i]:=0 END;
  msk(150,21b); msk(350,15b); msk(800,25b); msk(1000,35b);
  msk(1300,23b); msk(2000,26b); msk(2800,22b);
END BRDout.
