IMPLEMENTATION MODULE GPHout; (* Sem 19-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM FsPublic   IMPORT  FileName, File;
FROM BIO        IMPORT  bWrite, Create, Link, CD, checkHALT, Close, SetEof;
FROM Terminal   IMPORT  print;
FROM Edit       IMPORT  ReadString;
FROM Image      IMPORT  PeekNum;
FROM Strings    IMPORT  Str1;
FROM Model      IMPORT  Object, Tag, Iterate, Objects, setPoz, Poz, Lget,
                        NewObject, Tie, Lset;
FROM ModelMisc  IMPORT  Size, X1, X2, Y1, Y2, ViasSize, Fixed, Layer, Empty,
                        PinLocation, StartConductor, NextConductor, Signal;
FROM GPHtechnology IMPORT Tracks, Pins, Vias, RouterMode, RoutGrid, ViasGrid,
                          Clearance, Resist;

VAR rec          : ARRAY [0..127] OF CHAR;
    reclen       : ARRAY CHAR OF INTEGER;
    buf          : POINTER TO ARRAY [0..4095] OF CHAR;
    bufcnt,bufblk: INTEGER;
    Out          : File;
    OutName      : FileName;
    layer        : INTEGER;
    mdl          : Object;

PROCEDURE w16(a,w: INTEGER);
BEGIN
  rec[a]:=CHAR(w); w:=w>>8; rec[a+1]:=CHAR(w);
END w16;

PROCEDURE str8(a: INTEGER; s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<8)&(s[i]#0c) DO rec[a+i]:=s[i]; INC(i) END;
  WHILE (i<8) DO rec[a+i]:=' '; INC(i) END;
END str8;

PROCEDURE WriteRec;
  VAR i: INTEGER;
BEGIN
  rec[0]:=CHAR(reclen[rec[1]]);
  FOR i:=0 TO reclen[rec[1]]*2-1 DO
    IF bufcnt=4096 THEN
      checkHALT(bWrite(Out,bufblk,buf,bufcnt),OutName);
      bufcnt:=0; INC(bufblk);
    END;
    buf^[bufcnt]:=rec[i]; INC(bufcnt);
  END;
  FOR i:=0 TO 127 DO rec[i]:=0c END;
END WriteRec;

PROCEDURE WriteEof;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 3 DO
    IF bufcnt=4096 THEN
      checkHALT(bWrite(Out,bufblk,buf,bufcnt),OutName);
      bufcnt:=0; INC(bufblk);
    END;
    buf^[bufcnt]:=0c; INC(bufcnt);
  END;
  IF bufcnt>0 THEN
    checkHALT(bWrite(Out,bufblk,buf,bufcnt),OutName);
  END;
  SetEof(Out,bufblk*4096+bufcnt);
END WriteEof;

PROCEDURE Tool(sz: INTEGER): INTEGER;
  VAR i,j,s,sa: INTEGER;
BEGIN
  j:=-1; s:=100000;
  FOR i:=0 TO 15 DO
    IF Tracks[i].diameter>0 THEN
      sa:=ABS(Tracks[i].diameter-sz*2);
      IF s>sa THEN j:=i; s:=sa END;
    END;
  END;
  IF j>=0 THEN RETURN j END;
  print('Нет подходящей технологии проводников...\n'); HALT(1);
END Tool;

PROCEDURE X_seg;
BEGIN
  rec[1]:=41c;
  rec[2]:=CHAR(Tool(Size));
  rec[3]:=CHAR(layer+200b*ORD(Fixed));
  w16(4,Y1);
  IF X1<X2 THEN w16(6,X1); w16(8,X2) ELSE w16(6,X2); w16(8,X1) END;
  WriteRec;
END X_seg;

PROCEDURE Y_seg;
BEGIN
  rec[1]:=42c;
  rec[2]:=CHAR(Tool(Size));
  rec[3]:=CHAR(layer+200b*ORD(Fixed));
  w16(4,X1);
  IF Y1<Y2 THEN w16(6,Y1); w16(8,Y2) ELSE w16(6,Y2); w16(8,Y1) END;
  WriteRec;
END Y_seg;

PROCEDURE Vector;
BEGIN
  rec[1]:=47c;
  rec[2]:=CHAR(Tool(Size));
  rec[3]:=CHAR(layer+200b*ORD(Fixed));
  IF X1<X2 THEN
    w16(4,X1); w16(6,Y1);
    w16(8,X2); w16(10,Y2);
  ELSE
    w16(4,X2); w16(6,Y2);
    w16(8,X1); w16(10,Y1);
  END;
  WriteRec;
END Vector;

PROCEDURE FileHeader;
   VAR i,j: INTEGER; ln: ARRAY [0..11] OF CHAR;
BEGIN
  rec[1]:=46c;
  w16(2,1); w16(4,1);
  LOOP
    print('Количество слоев: ');
    ReadString('',ln); print('\n');
    j:=PeekNum(ln,0,i);
    IF (j>0)&(ln[j]=0c) THEN
      IF (i>=1)&(i<=7) THEN EXIT END;
      print('Количество слоев должно лежать в диапазоне 1..7.\n');
    ELSE
      print('Не понимаю: %s\n',ln);
    END;
  END;
  DEC(i);
  w16(6,8+INTEGER(i<<13));
  w16(14b,mdl^.ctX); w16(16b,mdl^.ctY);
  w16(20b,mdl^.ctX); w16(22b,mdl^.ctY);
  WriteRec;
END FileHeader;

PROCEDURE TechnologyHeader;
BEGIN
  rec[1]:=60c;
  w16(2,INTEGER(RouterMode));
  w16(4,RoutGrid[0]);
  w16(6,RoutGrid[1]);
  w16(8,ViasGrid);
  w16(10,Clearance);
  WriteRec;
END TechnologyHeader;

PROCEDURE TechnologyTracks;
  VAR i: INTEGER;
BEGIN
  rec[1]:=61c;
  FOR i:=0 TO 15 DO
    w16(2+i*2,Tracks[i].diameter);
    rec[42b+i*2]:=CHAR(Tracks[i].displaceX);
    rec[43b+i*2]:=CHAR(Tracks[i].displaceY);
  END;
  WriteRec;
END TechnologyTracks;

PROCEDURE TechnologyPins;
  VAR i: INTEGER;
BEGIN
  rec[1]:=62c;
  FOR i:=0 TO 15 DO
    w16(2+i*2,Pins[i].diameter0);
    w16(42b+i*2,Pins[i].diameter1);
  END;
  w16(102b,Resist);
  WriteRec;
END TechnologyPins;

PROCEDURE TechnologyVias;
  VAR i: INTEGER;
BEGIN
  rec[1]:=63c;
  FOR i:=0 TO 15 DO
    rec[2+i]:=CHAR(Vias[i].type+Vias[i].dril*16);
    rec[22b+i*2]:=CHAR(Vias[i].displaceX);
    rec[23b+i*2]:=CHAR(Vias[i].displaceY);
  END;
  WriteRec;
END TechnologyVias;

PROCEDURE Legend;
BEGIN
--       0 1 2 3 4 5 6 7 8
  rec:='  (c) Kronos      ';
  rec[1]:=64c;
  WriteRec;
END Legend;

PROCEDURE ToolVia(): INTEGER;
  VAR i,j,s,sa: INTEGER;
BEGIN
  j:=-1; s:=100000;
  FOR i:=0 TO 15 DO
    IF Vias[i].dril=ViasSize THEN
      sa:=ABS(Pins[Vias[i].type].diameter0-Size*2);
      IF s>sa THEN j:=i; s:=sa END;
    END;
  END;
  IF j>=0 THEN RETURN j END;
  print('Нет подходящей технологии переходных отверстий...\n');
  print('drill %d, size %d\n',ViasSize,Size);
  HALT(1);
END ToolVia;

VAR Pin?res: BOOLEAN;

PROCEDURE Pin?1(o: Object);
  VAR x,y: INTEGER;
BEGIN
  PinLocation(o,x,y);
  IF (x-X1)*(x-X1)+(y-Y1)*(y-Y1)<Size*Size THEN Pin?res:=TRUE END;
END Pin?1;

PROCEDURE Pin?(): BOOLEAN;
BEGIN
(* Является ли данный сегмент метализации площадкой пина? *)
  IF (ViasSize=0)OR(X1#X2)OR(Y1#Y2) THEN RETURN FALSE END;
  Pin?res:=FALSE;
  Iterate(Signal^.TiedPins,Pin?1);
  RETURN Pin?res;
END Pin?;

PROCEDURE WriteSeg;
BEGIN
  IF (X1#X2)OR(Y1#Y2)OR(ViasSize=0) THEN
    FOR layer:=0 TO 31 DO
      IF layer IN Layer THEN
        IF layer>7 THEN
          print(
          'Система CALAY не позволяет обрабатывать больше восьми слоев!\n');
          HALT(1);
        END;
        IF X1=X2 THEN Y_seg;
        ELSIF Y1=Y2 THEN X_seg;
        ELSE Vector;
        END;
      END;
    END;
  END;
  IF ViasSize#0 THEN
    IF NOT Pin?() THEN
      rec[1]:=43c;
      rec[2]:=CHAR(Layer);
      rec[3]:=CHAR(ToolVia()+ORD(Fixed)*200b);
      w16(4,X1); w16(6,Y1);
      WriteRec;
    END;
  END;
END WriteSeg;

PROCEDURE ToolPin(o: Object; x,y: INTEGER): INTEGER;
  VAR i,j,s,sa: INTEGER;
BEGIN
  StartConductor(o^.Signal);
  WHILE NOT Empty DO
    IF (X1=X2)&(Y1=Y2)&((X1-x)*(X1-x)+(Y1-y)*(Y1-y)<Size*Size)
       &(ViasSize#0) THEN
      j:=-1; s:=100000;
      FOR i:=0 TO 15 DO
        sa:=ABS(Pins[i].diameter0-Size*2);
        IF s>sa THEN j:=i; s:=sa END;
      END;
      IF j>=0 THEN RETURN j+ViasSize*16 END;
      print('Нет подходящей технологии для пинов...\n'); HALT(1);
    END;
    NextConductor;
  END;
  print('Нет метализации под пин %s %d...\n',o^.Chip^.Name,o^.No+1); HALT(1);
END ToolPin;

PROCEDURE pPin(o: Object);
  VAR x,y: INTEGER; e: Object;
BEGIN
  IF Tag(o)#pin THEN RETURN END;
  rec[1]:=44c;
  rec[5b]:=377c;
  PinLocation(o,x,y);
  rec[2]:=CHAR(ToolPin(o,x,y));
  rec[3]:=CHAR({0..1});
  w16(6b,x); w16(10b,y);
  e:=Lget(o^.Chip^.ChipType^.ExternalPins,o^.No);
ASSERT(Tag(e)=externalpin);
  x:=e^.TrackWidth;
  IF x=0 THEN rec[4]:=1c ELSE rec[4]:=CHAR(Tool(x)) END;
  str8(12b,o^.Chip^.Name);
  rec[22b]:=CHAR(o^.No+1);
  WriteRec;
END pPin;

PROCEDURE cPin(o: Object);
  VAR s: Object;
BEGIN
  IF Tag(o)#pin THEN RETURN END;
  s:=o^.Signal;
  IF (s=NIL)OR(s^.Name='..free..') THEN pPin(o) END;
END cPin;

VAR CheckedChip: Object;
    FreeSignal : Object;

PROCEDURE FreeSig(s: Object);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF s^.Name='..free..' THEN FreeSignal:=s END;
END FreeSig;

PROCEDURE CheckPin(o: Object);
  VAR i: INTEGER; p: Object;
BEGIN
  ASSERT(Tag(o)=externalpin);
  i:=o^.EPinNo;
  p:=Lget(CheckedChip^.Pins,i);
  IF p=NIL THEN
    print('create free pin in chip %s\n',CheckedChip^.Name);
    p:=NewObject(pin);
    IF FreeSignal=NIL THEN Iterate(mdl^.All,FreeSig) END;
    ASSERT(FreeSignal#NIL);
    p^.Signal:=FreeSignal;
    p^.Chip:=CheckedChip;
    Lset(CheckedChip^.Pins,i,p);
    Tie(mdl^.All,p);
  END;
  p^.No:=i;
END CheckPin;

PROCEDURE Component(o: Object);
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  rec[1]:=4c;
  str8(2,o^.Name);
  str8(12b,o^.ChipType^.Name);
  rec[22b]:=CHAR(o^.RB);
  w16(24b,o^.XB); w16(26b,o^.YB);
  w16(30b,o^.ChipType^.ctX); w16(32b,o^.ChipType^.ctY);
  WriteRec;
  CheckedChip:=o;
  Iterate(o^.ChipType^.ExternalPins,CheckPin);
  Iterate(o^.Pins,cPin);
END Component;

PROCEDURE Net(o: Object);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name='..free..' THEN RETURN END;
  rec[1]:=40c;
  str8(4,o^.Name);
  WriteRec;
  Iterate(o^.TiedPins,pPin);
  StartConductor(o);
  WHILE NOT Empty DO WriteSeg; NextConductor END;
END Net;

PROCEDURE WriteGPH(nm: ARRAY OF CHAR; m: Object);
  VAR b: ARRAY [0..4095] OF CHAR;
BEGIN
  bufblk:=0; bufcnt:=0; buf:=ADR(b);
  mdl:=m; FreeSignal:=NIL;
  Str1(OutName,nm);
  checkHALT(Create(Out),OutName);
  FileHeader;
  TechnologyHeader;
  TechnologyTracks;
  TechnologyPins;
  TechnologyVias;
  Legend;
  Iterate(mdl^.All,Component);
  Iterate(mdl^.All,Net);
  WriteEof;
  checkHALT(Link(CD(),OutName,Out),OutName);
  checkHALT(Close(Out),OutName);
END WriteGPH;

VAR c: CHAR;
    i: INTEGER;

BEGIN
  FOR i:=0 TO 127 DO rec[i]:=0c END;
  FOR c:=0c TO 377c DO reclen[c]:=64 END;
  reclen[41c]:=5; reclen[42c]:=5;
  reclen[43c]:=4; reclen[44c]:=10;
  reclen[47c]:=6; reclen[04c]:=26;
  reclen[64c]:=9; reclen[40c]:=6;
  reclen[45c]:=4; reclen[46c]:=18;
  reclen[60c]:=6; reclen[61c]:=104b DIV 2;
  reclen[63c]:=62b DIV 2;
END GPHout.
