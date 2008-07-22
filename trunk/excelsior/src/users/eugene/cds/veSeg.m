IMPLEMENTATION MODULE veSeg; (* Sem 17-Feb-88. (c) KRONOS *)

IMPORT VIDEO;
FROM KRONOS     IMPORT  MOVE;
FROM vePbl      IMPORT  X, Y, CurPict, curpX, curpY, WndX, WndY, Scale;
FROM Model      IMPORT  Object, Objects, NewObject, Segment, setTag, Tag,
                        ObjectsNo;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM VIDEO      IMPORT  clipE, clipW, clipS, clipN;

PROCEDURE vect(x1,y1,x2,y2: INTEGER);
BEGIN
  VIDEO.vect((x1-WndX)*Scale DIV 4,(y1-WndY)*Scale DIV 8,
             (x2-WndX)*Scale DIV 4,(y2-WndY)*Scale DIV 8);
END vect;

PROCEDURE cursor(x,y,n: INTEGER);
BEGIN
  VIDEO.cursor((x-WndX)*Scale DIV 4,(y-WndY)*Scale DIV 8,n);
END cursor;

PROCEDURE New(): Object;
  VAR s: Object;
BEGIN
  s:=NewObject(picture);
  s^.pX1:=0; s^.pY1:=0;
  s^.pX2:=0; s^.pY2:=0;
  RETURN s;
END New;

PROCEDURE Tie(s: Object; VAR to: Object);
  VAR l,lto: Object;
BEGIN
  IF to=NIL THEN to:=s; RETURN END;
  IF s=NIL THEN RETURN END;
  l:=s^.pLeft; lto:=to^.pLeft;
  s^.pLeft:=lto; to^.pLeft:=l;
  lto^.pRight:=s; l^.pRight:=to;
  s^.pUp:=to^.pUp;
END Tie;

PROCEDURE Ls0(s: Object; ip: IterProc): BOOLEAN;
  VAR b,e: Object;
BEGIN
  IF (s^.pDown#NIL) THEN
    INC(Level);
    b:=s^.pDown; e:=b;
    REPEAT IF Ls0(b,ip) THEN RETURN TRUE END; b:=b^.pRight UNTIL b=e;
    DEC(Level);
  END;
  RETURN ip(s);
END Ls0;

PROCEDURE Ls(s: Object; ip: IterProc);
  VAR b,e: Object; r: BOOLEAN;
BEGIN
  Level:=0;
  IF s#NIL THEN
    b:=s; e:=s;
    REPEAT r:=Ls0(b,ip); b:=b^.pRight UNTIL (e=b)OR r;
  END;
END Ls;

PROCEDURE CopyTree(s: Object): Object;
  VAR p,b,e: Object;
BEGIN
  ASSERT(Tag(s)=picture);
  p:=New();
  p^.pX1:=s^.pX1;
  p^.pX2:=s^.pX2;
  p^.pY1:=s^.pY1;
  p^.pY2:=s^.pY2;
  IF s^.pLines#NIL THEN
    Allocate(p^.pLines,SIZE(Segment)*s^.pLines^.cLen+2);
    setTag(p^.pLines,conductor);
    INC(ObjectsNo);
    MOVE(p^.pLines,s^.pLines,2+SIZE(Segment)*s^.pLines^.cLen);
  END;
  IF s^.pDown#NIL THEN
    b:=s^.pDown; e:=b;
    REPEAT
      Tie(CopyTree(b),p^.pDown); b:=b^.pRight;
    UNTIL e=b;
  END;
  RETURN p;
END CopyTree;

PROCEDURE Min(x,y: INTEGER): INTEGER;
BEGIN
  IF x<y THEN RETURN x ELSE RETURN y END;
END Min;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN x ELSE RETURN y END;
END Max;

PROCEDURE Pack(VAR s: Segment; x1,y1,x2,y2: INTEGER);
BEGIN
  s.start:=x1+INTEGER(y1<<16);
  s.end  :=x2+INTEGER(y2<<16);
  s.size:=1;
END Pack;

PROCEDURE AppLine(x1,y1,x2,y2: INTEGER);
  VAR o,l,b,e: Object; i,d,x,y: INTEGER;
BEGIN
(*$T-*)
  IF (x1=x2)&(y1=y2) THEN RETURN END;
  DEC(x1,curpX-CurPict^.pX1); DEC(x2,curpX-CurPict^.pX1);
  DEC(y1,curpY-CurPict^.pY1); DEC(y2,curpY-CurPict^.pY1);
  IF CurPict^.pLines=NIL THEN
    Allocate(CurPict^.pLines,SIZE(Segment)+2);
    setTag(CurPict^.pLines,conductor);
    INC(ObjectsNo);
    CurPict^.pLines^.cLen:=1;
    CurPict^.pLines^.cType[0].size:=0;
    x:=CurPict^.pX1; y:=CurPict^.pY1;
    CurPict^.pX1:=Min(x1,x2); CurPict^.pX2:=Max(x1,x2);
    CurPict^.pY1:=Min(y1,y2); CurPict^.pY2:=Max(y1,y2);
    INC(curpX,CurPict^.pX1-x); INC(curpY,CurPict^.pY1-y);
  END;
  l:=CurPict^.pLines; ASSERT(Tag(l)=conductor);
  IF Max(x1,x2)>CurPict^.pX2 THEN CurPict^.pX2:=Max(x1,x2) END;
  IF Max(y1,y2)>CurPict^.pY2 THEN CurPict^.pY2:=Max(y1,y2) END;
  IF Min(x1,x2)<CurPict^.pX1 THEN
    d:=CurPict^.pX1-Min(x1,x2); CurPict^.pX1:=Min(x1,x2);
    DEC(curpX,d);
    IF CurPict^.pDown#NIL THEN
      b:=CurPict^.pDown; e:=b;
      REPEAT INC(b^.pX1,d); INC(b^.pX2,d); b:=b^.pRight UNTIL b=e;
    END;
    FOR i:=0 TO l^.cLen-1 DO
      IF l^.cType[i].size#0 THEN
        INC(l^.cType[i].start,d);
        INC(l^.cType[i].end  ,d);
      END;
    END;
  END;
  IF Min(y1,y2)<CurPict^.pY1 THEN
    d:=CurPict^.pY1-Min(y1,y2); CurPict^.pY1:=Min(y1,y2);
    DEC(curpY,d);
    IF CurPict^.pDown#NIL THEN
      b:=CurPict^.pDown; e:=b;
      REPEAT INC(b^.pY1,d); INC(b^.pY2,d); b:=b^.pRight UNTIL b=e;
    END;
    d:=d*10000h;
    FOR i:=0 TO l^.cLen-1 DO
      IF l^.cType[i].size#0 THEN
        INC(l^.cType[i].start,d);
        INC(l^.cType[i].end  ,d);
      END;
    END;
  END;
  DEC(x1,CurPict^.pX1); DEC(x2,CurPict^.pX1);
  DEC(y1,CurPict^.pY1); DEC(y2,CurPict^.pY1);
  FOR i:=0 TO l^.cLen-1 DO
    IF l^.cType[i].size=0 THEN
      Pack(l^.cType[i],x1,y1,x2,y2); RETURN;
    END;
  END;
  Allocate(o,2+SIZE(Segment)*(l^.cLen+1));
  MOVE(o,l,2+SIZE(Segment)*l^.cLen);
  INC(o^.cLen); CurPict^.pLines:=o;
  Deallocate(l,2+SIZE(Segment)*l^.cLen);
  Pack(o^.cType[o^.cLen-1],x1,y1,x2,y2); RETURN;
(*$T+*)
END AppLine;

PROCEDURE Drow(p: Object; x,y: INTEGER);
  VAR i,x1,x2,y1,y2: INTEGER; l: Object;
BEGIN
  l:=p^.pLines;
  IF l=NIL THEN RETURN END;
  IF (x-WndX)*Scale DIV 4 > clipE THEN RETURN END;
  IF (x-WndX+p^.pX2)*Scale DIV 4 < clipW THEN RETURN END;
  IF (y-WndY)*Scale DIV 8 > clipN THEN RETURN END;
  IF (y-WndY+p^.pY2)*Scale DIV 8 < clipS THEN RETURN END;
(*$T-*)
  FOR i:=0 TO l^.cLen-1 DO
    IF l^.cType[i].size#0 THEN
      x1:=INTEGER(BITSET(l^.cType[i].start)*{0..15});
      y1:=INTEGER(BITSET(l^.cType[i].start>>16)*{0..15});
      x2:=INTEGER(BITSET(l^.cType[i].end)*{0..15});
      y2:=INTEGER(BITSET(l^.cType[i].end>>16)*{0..15});
      VIDEO.vect((x+x1-WndX)*Scale DIV 4,(y+y1-WndY)*Scale DIV 8,
                 (x+x2-WndX)*Scale DIV 4,(y+y2-WndY)*Scale DIV 8);
    END;
  END;
(*$T+*)
END Drow;

PROCEDURE DrowRingTree(p,c: Object; x,y: INTEGER);
  VAR e: Object;
BEGIN
  IF p=NIL THEN RETURN END;
  e:=p;
  REPEAT
    IF p#c THEN
      Drow(p,x+p^.pX1,y+p^.pY1);
      DrowRingTree(p^.pDown,c,x+p^.pX1,y+p^.pY1);
    END;
    p:=p^.pRight;
  UNTIL p=e;
END DrowRingTree;

PROCEDURE DrowTree(p,c: Object; x,y: INTEGER);
BEGIN
  IF p=NIL THEN RETURN END;
  IF p#c THEN Drow(p,x,y); DrowRingTree(p^.pDown,c,x,y) END;
END DrowTree;

END veSeg.
