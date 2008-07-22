IMPLEMENTATION MODULE ImageMisc; (* Sem 20-Mar-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR;
FROM KRONOS    IMPORT   MOVE;
FROM Images    IMPORT   Point, Vector, Circle, Text, Strob, New, Save,
                        Image, Img;
FROM Convert   IMPORT   AppNum;
FROM Strings   IMPORT   Str0, Str1;
FROM Model     IMPORT   Objects, NewObject, Tie, Lset, String, Lget;
FROM ModelIO   IMPORT   WriteModel;

PROCEDURE DIP14(Name: ARRAY OF CHAR);
  VAR p: Object; i: CARDINAL; Im: Image;
BEGIN
  mdl:=NewObject(chiptype);
  Str1(mdl^.Name,Name);
  mdl^.X1:=0; mdl^.Y1:=0;
  mdl^.X2:=6*2500; mdl^.Y2:=7500;
  FOR i:=0 TO 13 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<7 THEN
      p^.PinX:=i*2500; p^.PinY:=0;
    ELSE
      p^.PinX:=(13-i)*2500; p^.PinY:=7500;
    END;
  END;
  New(Im,Name);
  Img:=Im;
END DIP14;

PROCEDURE DIP16(Name: ARRAY OF CHAR);
  VAR p: Object; i: CARDINAL; Im: Image;
BEGIN
  mdl:=NewObject(chiptype);
  Str1(mdl^.Name,Name);
  mdl^.X1:=0; mdl^.Y1:=0;
  mdl^.X2:=7*2500; mdl^.Y2:=7500;
  FOR i:=0 TO 15 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<8 THEN
      p^.PinX:=i*2500; p^.PinY:=0;
    ELSE
      p^.PinX:=(15-i)*2500; p^.PinY:=7500;
    END;
  END;
  New(Im,Name);
  Img:=Im;
END DIP16;

PROCEDURE DIP20(Name: ARRAY OF CHAR);
  VAR p: Object; i: CARDINAL; Im: Image;
BEGIN
  mdl:=NewObject(chiptype);
  Str1(mdl^.Name,Name);
  mdl^.X1:=0; mdl^.Y1:=0;
  mdl^.X2:=9*2500; mdl^.Y2:=7500;
  FOR i:=0 TO 19 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<10 THEN
      p^.PinX:=i*2500; p^.PinY:=0;
    ELSE
      p^.PinX:=(19-i)*2500; p^.PinY:=7500;
    END;
  END;
  New(Im,Name);
  Img:=Im;
END DIP20;

PROCEDURE DIP24(Name: ARRAY OF CHAR);
  VAR p: Object; i: CARDINAL; Im: Image;
BEGIN
  mdl:=NewObject(chiptype);
  Str1(mdl^.Name,Name);
  mdl^.X1:=0; mdl^.Y1:=0;
  mdl^.X2:=11*2500; mdl^.Y2:=7500;
  FOR i:=0 TO 23 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<12 THEN
      p^.PinX:=i*2500; p^.PinY:=0;
    ELSE
      p^.PinX:=(23-i)*2500; p^.PinY:=7500;
    END;
  END;
  New(Im,Name);
  Img:=Im;
END DIP24;

PROCEDURE AppPin(n,x,y,r: CARDINAL; type: BITSET; Txt: ARRAY OF CHAR);
  VAR p: Object; s: ARRAY [0..79] OF CHAR;
BEGIN
  DEC(n); p:=Lget(mdl^.ExternalPins,n);
  ASSERT(p#NIL);
  p^.PinDX:=x;
  p^.PinDY:=y;
  p^.PinDR:=r;
  IF r=0 THEN
    Str0(s); AppNum(s,'%d2%',n+1);
    Point(x,y); Vector(x+30,y);
    IF neg IN type THEN Circle(x+30,y,5) END;
    Text(x+38,y-5,Txt);
    Text(x+5,y+5,s);
  ELSIF r=2 THEN
    Str0(s); AppNum(s,'%d*%',n+1);
    Point(x,y); Vector(x-30,y);
    IF neg IN type THEN Circle(x-30,y,5) END;
    Text(x-56,y-5,Txt);
    Text(x-25,y+5,s);
  END;
END AppPin;

PROCEDURE Finish;
BEGIN
  Save(Img);
  WriteModel(mdl);
END Finish;

END ImageMisc.
