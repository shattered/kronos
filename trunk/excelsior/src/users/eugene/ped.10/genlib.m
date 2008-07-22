MODULE genlib; (* 13-Feb-87. (c) KRONOS *)

FROM StdIO     IMPORT   Show;
FROM Model     IMPORT   Object, Objects, NewObject, Tie, Lset, String;
FROM ModelIO   IMPORT   WriteModel;
FROM Args      IMPORT   TakeWord;
FROM Strings   IMPORT   Str1;

VAR mdl,p: Object; i: CARDINAL;
    Name: String;

PROCEDURE dip(nm: ARRAY OF CHAR; size: CARDINAL; no: CARDINAL);
BEGIN
  Show(nm);
  Str1(Name,nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=(no DIV 2-1)*96; mdl^.ctY:=96*size;
  FOR i:=0 TO no-1 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF i<(no DIV 2) THEN
      p^.PinX:=i*96; p^.PinY:=0;
    ELSE
      p^.PinX:=(no-i-1)*96; p^.PinY:=96*size;
    END;
  END;
  WriteModel(mdl);
END dip;

PROCEDURE sip(nm: ARRAY OF CHAR; size: CARDINAL; no: CARDINAL);
BEGIN
  Show(nm);
  Str1(Name,nm);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=(no-1)*96; mdl^.ctY:=96*size;
  FOR i:=0 TO no-1 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    p^.PinX:=i*96; p^.PinY:=0;
  END;
  WriteModel(mdl);
END sip;

PROCEDURE snp59;
BEGIN
  Str1(Name,'SNP59');
  Show(Name);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=31*96; mdl^.ctY:=96*2;
  FOR i:=0 TO 31 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i; p^.Host:=mdl;
    p^.PinX:=i*96; p^.PinY:=0;
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i+32,p);
    p^.EPinNo:=i+32; p^.Host:=mdl;
    p^.PinX:=i*96; p^.PinY:=96;
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,i+64,p);
    p^.EPinNo:=i+64; p^.Host:=mdl;
    p^.PinX:=i*96; p^.PinY:=96*2;
  END;
  WriteModel(mdl);
END snp59;

PROCEDURE plan18;
  PROCEDURE pn(n,x,y: INTEGER);
  BEGIN
    p:=NewObject(externalpin);
    Tie(mdl^.All,p); Lset(mdl^.ExternalPins,n,p);
    p^.EPinNo:=n; p^.Host:=mdl;
    p^.PinX:=x; p^.PinY:=y;
  END pn;
BEGIN
  Str1(Name,'PLAN18');
  Show(Name);
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  mdl^.ctX:=4*96; mdl^.ctY:=96*8;

  pn(0,0*96   ,  0);
  pn(1,1*96-48,144);
  pn(2,1*96   ,  0);
  pn(3,2*96-48,144);
  pn(4,2*96   ,  0);
  pn(5,3*96-48,144);
  pn(6,3*96   ,  0);
  pn(7,4*96-48,144);
  pn(8,4*96   ,  0);

  pn(17,0*96   ,   0+8*96);
  pn(16,1*96-48,-144+8*96);
  pn(15,1*96   ,   0+8*96);
  pn(14,2*96-48,-144+8*96);
  pn(13,2*96   ,   0+8*96);
  pn(12,3*96-48,-144+8*96);
  pn(11,3*96   ,   0+8*96);
  pn(10,4*96-48,-144+8*96);
  pn( 9,4*96   ,   0+8*96);

  WriteModel(mdl);
END plan18;

BEGIN
--dip('DIP8',3,8);
--dip('DIP14',3,14);
--dip('DIP16',3,16);
--dip('DIP20',3,20);
--dip('DIP24',6,24);
--dip('DIP28',6,28);
--dip('DIP40',6,40);
--sip('SIP10',1,10);
  snp59;
  plan18;
END genlib.
