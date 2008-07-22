MODULE genlib; (* 13-Feb-87. (c) KRONOS *)

IMPORT  str : Strings;

FROM StdIO      IMPORT  print;
FROM Model      IMPORT  Object, Objects, NewObject, Tie, Lset, String;
FROM ModelIO    IMPORT  WriteModel;
FROM pedTools   IMPORT  Tools;
FROM ModelMisc  IMPORT  StartConductor, AppConductor, X1, X2, Y1, Y2,
                        Size, ViasSize, Fixed, Layer;

VAR mdl,p: Object; i: INTEGER;
    Name: String;

PROCEDURE dip(nm: ARRAY OF CHAR; size: INTEGER; no: INTEGER);
  VAR i: INTEGER;
BEGIN
  print('%s\n',nm);
  str.print(Name,'%s',nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=(no DIV 2-1)*96; mdl^.ctY:=96*size;
  FOR i:=0 TO no-1 DO
    p:=NewObject(externalpin);
    Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<(no DIV 2) THEN
      p^.PinX:=i*96; p^.PinY:=0;
    ELSE
      p^.PinX:=(no-i-1)*96; p^.PinY:=96*size;
    END;
    StartConductor(p,TRUE);
    X1:=p^.PinX; X2:=p^.PinX; Y1:=p^.PinY; Y2:=p^.PinY;
    Size:=Tools[5].VSize; ViasSize:=Tools[5].DSize; Layer:={0..1};
    Fixed:=TRUE; AppConductor(TRUE);
    str.print(p^.Name,'ext pin %d',p^.EPinNo+1);
  END;
  WriteModel(mdl);
END dip;

PROCEDURE sip(nm: ARRAY OF CHAR; size: INTEGER; no: INTEGER);
  VAR i: INTEGER;
BEGIN
  print('%s\n',nm);
  str.print(Name,'%s',nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=(no-1)*96; mdl^.ctY:=96*size;
  FOR i:=0 TO no-1 DO
    p:=NewObject(externalpin);
    Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    p^.PinX:=i*96; p^.PinY:=0;
    StartConductor(p,TRUE);
    X1:=p^.PinX; X2:=p^.PinX; Y1:=p^.PinY; Y2:=p^.PinY;
    Size:=Tools[5].VSize; ViasSize:=Tools[5].DSize; Layer:={0..1};
    Fixed:=TRUE; AppConductor(TRUE);
    str.print(p^.Name,'ext pin %d',p^.EPinNo+1);
  END;
  WriteModel(mdl);
END sip;

PROCEDURE matrix(nm: ARRAY OF CHAR; pins,lines: INTEGER);
  VAR l,k: INTEGER;
BEGIN
  print('%s\n',nm);
  str.print(Name,'%s',nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctY:=(lines-1)*96; mdl^.ctX:=(pins-1)*96;
  FOR l:=0 TO lines-1 DO
    FOR k:=0 TO pins-1 DO
      p:=NewObject(externalpin);
      Lset(mdl^.ExternalPins,l*pins+k,p);
      p^.EPinNo:=l*pins+k; p^.Host:=mdl;
      p^.PinX:=k*96; p^.PinY:=(lines-1-l)*96;
      StartConductor(p,TRUE);
      X1:=p^.PinX; X2:=p^.PinX; Y1:=p^.PinY; Y2:=p^.PinY;
      Size:=Tools[5].VSize; ViasSize:=Tools[5].DSize; Layer:={0..1};
      Fixed:=TRUE; AppConductor(TRUE);
      str.print(p^.Name,'ext pin %d',p^.EPinNo+1);
    END;
  END;
  WriteModel(mdl);
END matrix;

PROCEDURE rh(nm: ARRAY OF CHAR; size: INTEGER);
  VAR i: INTEGER;
BEGIN
  print('%s\n',nm);
  str.print(Name,'%s',nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctY:=96; mdl^.ctX:=96*size;
  FOR i:=0 TO 1 DO
    p:=NewObject(externalpin);
    Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    p^.PinX:=i*size*96; p^.PinY:=0;
    StartConductor(p,TRUE);
    X1:=p^.PinX; X2:=p^.PinX; Y1:=p^.PinY; Y2:=p^.PinY;
    Size:=Tools[5].VSize; ViasSize:=Tools[5].DSize; Layer:={0..1};
    Fixed:=TRUE; AppConductor(TRUE);
    str.print(p^.Name,'ext pin %d',p^.EPinNo+1);
    WriteModel(mdl);
  END;
END rh;

PROCEDURE pln(nm: ARRAY OF CHAR; size: INTEGER; no: INTEGER);
  VAR i: INTEGER;
BEGIN
  print('%s\n',nm);
  str.print(Name,'%s',nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=((no DIV 2+1) DIV 2-1)*96;
  mdl^.ctY:=96*size;
  FOR i:=0 TO no-1 DO
    p:=NewObject(externalpin);
    Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<(no DIV 2) THEN
      p^.PinX:=i*48; p^.PinY:=((i+1) MOD 2)*96;
    ELSE
      p^.PinX:=(no-i-1)*48; p^.PinY:=96*(size-i MOD 2);
    END;
    StartConductor(p,TRUE);
    X1:=p^.PinX; X2:=p^.PinX; Y1:=p^.PinY; Y2:=p^.PinY;
    Size:=Tools[5].VSize; ViasSize:=Tools[5].DSize; Layer:={0..1};
    Fixed:=TRUE; AppConductor(TRUE);
    str.print(p^.Name,'ext pin %d',p^.EPinNo+1);
  END;
  WriteModel(mdl);
END pln;

PROCEDURE brd(nm: ARRAY OF CHAR);
BEGIN
  print('%s\n',nm);
  str.print(Name,'%s',nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=0; mdl^.ctY:=0;
  WriteModel(mdl);
END brd;

BEGIN
  matrix('SNP59',32,3);
  dip('DIP8',3,8);
  dip('DIP14',3,14);
  dip('DIP16',3,16);
  dip('DIP18',3,18);
  dip('DIP20',3,20);
  dip('DIP24',6,24);
  dip('DIP28',6,28);
  dip('DIP40',6,40);
  sip('SIP8',1,10);
  sip('SIP10',1,10);
  pln('PLN42',10,42);
  pln('PLN48',10,48);
  sip('MLT125',1,2);
  sip('STR3',1,3);
  sip('PIN',1,1);
  rh('RH',4);
  sip('STR32',1,32);
  dip('DIP64',9,64);
  dip('CON6',1,6);
  dip('CON16',1,16);
  dip('DIP18W',4,18);
END genlib.
