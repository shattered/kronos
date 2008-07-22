IMPLEMENTATION MODULE BRDin; (* Sem 03-Feb-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM ModelPbl   IMPORT  Message, RaiseInMe, IOerror;
FROM Model      IMPORT  Object, Objects, NewObject, Tie;
FROM ModelMisc  IMPORT  AppConductor, Fixed, X1, X2, Y1, Y2,
                        Layer, Size, ViasSize, Signal;
FROM cdsSeq     IMPORT  DisConnect;
FROM FsPublic   IMPORT  FileName, File;
FROM Image      IMPORT  image0, GetNum;
FROM Edit       IMPORT  ReadString;
FROM Terminal   IMPORT  print;
FROM BIO        IMPORT  Close, bRead, checkHALT, GetEof, CD, OpenOnDir;
FROM Args       IMPORT  Flag?;
IMPORT  mcd: mCodeMnem;

VAR x1,x2,y1,y2 : INTEGER;
    Buf         : ARRAY [0..4095] OF CHAR;
    BufCnt      : INTEGER;
    BufPtr      : INTEGER;
    Inp         : File;
    InpName     : FileName;
    Masks       : ARRAY [0..255] OF INTEGER;
    Layers      : INTEGER;
    free        : Object;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE r16(): INTEGER;
  VAR n: INTEGER;
BEGIN
  IF BufPtr>HIGH(Buf) THEN
    INC(BufCnt);
    checkHALT(bRead(Inp,BufCnt,ADR(Buf),4096),InpName);
    BufPtr:=0;
  END;
  n:=ORD(Buf[BufPtr])+ORD(Buf[BufPtr+1])*256;
  INC(BufPtr,2);
  RETURN n;
END r16;

PROCEDURE Unit(VAR type,mask,layer: INTEGER);
  VAR n: INTEGER;
BEGIN
  n:=r16();
  mask:=n MOD 256;
  layer:=(n DIV 256) MOD 16;
  type:=n DIV 4096;
  IF type=0 THEN RETURN END;
  x1:=r16(); y1:=r16();
  IF type=1 THEN RETURN END;
  x2:=r16(); y2:=r16();
END Unit;

PROCEDURE Insert(type,mask,layer: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF Masks[mask]=0 THEN
    print('Undefined mask %bb. Ignored.\n',mask); RETURN
  END;
  IF type=1 THEN
    Layer:={};
    FOR i:=0 TO Layers-1 DO INCL(Layer,i) END;
    Fixed:=TRUE;
    X1:=x1*24 DIV 25; X1:=(X1+12) DIV 24*24; X2:=X1;
    Y1:=y1*24 DIV 25; Y1:=(Y1+12) DIV 24*24; Y2:=Y1;
    Size:=Masks[mask]; ViasSize:=1;
    Signal:=free;
    AppConductor(FALSE);
  ELSIF type=2 THEN
    Layer:={Layers-layer}; Fixed:=TRUE;
    X1:=x1*24 DIV 25; X1:=(X1+12) DIV 24*24;
    X2:=x2*24 DIV 25; X2:=(X2+12) DIV 24*24;
    Y1:=y1*24 DIV 25; Y1:=(Y1+12) DIV 24*24;
    Y2:=y2*24 DIV 25; Y2:=(Y2+12) DIV 24*24;
    Size:=Masks[mask]; ViasSize:=0;
    Signal:=free;
    AppConductor(FALSE);
  ELSE
    Message:='Illegal file structure.';
    RaiseInMe(IOerror);
  END;
END Insert;

PROCEDURE ReadBRD(mdl: Object);
  VAR
    ln: ARRAY [0..15] OF CHAR;
    type,mask,layer: INTEGER;
BEGIN
  ASSERT(Tag(mdl)=chiptype);
  free:=NewObject(signal);
  free^.Name:='..free..';
  Tie(mdl^.All,free);
  ln:='2';
  LOOP
    ReadString('Введите количество слоев в плате: ',ln); print('\n');
    IF GetNum(ln,Layers)>=0 THEN EXIT END;
    print('Не понял. ');
  END;
  BufPtr:=0; BufCnt:=0;
  image0(InpName,'%s.brd',mdl^.Name);
  checkHALT(OpenOnDir(CD(),Inp,InpName),InpName);
  IF GetEof(Inp)>0 THEN
    checkHALT(bRead(Inp,0,ADR(Buf),4096),InpName);
  END;
  LOOP
    Unit(type,mask,layer);
    IF type=0 THEN EXIT END;
    Insert(type,mask,layer);
  END;
  checkHALT(Close(Inp),InpName);
  IF Flag?('d') THEN DisConnect(mdl,free) END;
END ReadBRD;

PROCEDURE msk(d,n: INTEGER);
BEGIN
  Masks[n]:=(d*24+625) DIV 1250;
END msk;

VAR i: INTEGER;

BEGIN
  FOR i:=0 TO HIGH(Masks) DO Masks[i]:=0 END;
  msk(2200,13b);
  msk(1400,14b);
  msk( 350,15b);
  msk( 150,21b);
  msk(2800,22b);
  msk(1300,23b);
  msk( 800,25b);
  msk(2000,26b);
  msk(1000,35b);
  msk( 500,55b);
  msk( 250,56b);
  msk(  75,57b);
  msk(1400,60b);
  msk(2800,61b);
END BRDin.
