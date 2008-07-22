IMPLEMENTATION MODULE GPHin; (* Sem 09-May-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR;
FROM StdIO     IMPORT   Confirm, print;
FROM Model     IMPORT   Iterate, Tie, Lset, String, NewObject, Lget, Tag,
                        Object, Objects, SigTypes;
FROM ModelPbl  IMPORT   Reaction, Exception, KillReaction, Exception?,
                        RaiseInMe, IOerror, Message;
FROM ModelIO   IMPORT   ReadShortModel;
FROM ModelMisc IMPORT   StartConductor, X1, X2, Y1, Y2, Size, ViasSize, Layer,
                        AppConductor, Fixed, CreateConductor, TruncConductor;
FROM Image     IMPORT   image;
FROM Strings   IMPORT   Str0, Str1;
FROM FsPublic  IMPORT   File;
FROM BIO       IMPORT   bRead, OpenOnDir, Close, checkHALT, CD, GetEof;
IMPORT cdsHash;
IMPORT FROM GPHtechnology;

VAR Line: ARRAY [0..127] OF CHAR;
    mdl: Object;
    inp: File;
    poz: CARDINAL;
    EOF: BOOLEAN;
    Buf: ARRAY [0..127] OF CHAR;
    Block: ARRAY [0..4095] OF CHAR;
    BlockCnt: CARDINAL;
    Eof: CARDINAL;
    SigCnt,ChipCnt: INTEGER;

PROCEDURE Next;
  VAR i,l: CARDINAL;
BEGIN
  IF EOF THEN RETURN END;
  IF poz>4095 THEN
    print('blk=%d\r',BlockCnt);
    poz:=0;
    checkHALT(bRead(inp,BlockCnt,ADR(Block),4096),'GPH: ');
    INC(BlockCnt);
  END;
  IF Eof>0 THEN Buf[0]:=Block[poz]; INC(poz); DEC(Eof) ELSE Buf[0]:=0c END;
  IF Buf[0]=0c THEN EOF:=TRUE; RETURN END;
  FOR i:=1 TO CARDINAL(Buf[0])*2-1 DO
    IF poz>4095 THEN
      print('blk=%d\r',BlockCnt);
      poz:=0;
      checkHALT(bRead(inp,BlockCnt,ADR(Block),4096),'GPH: ');
      INC(BlockCnt);
    END;
    IF Eof>0 THEN Buf[i]:=Block[poz]; INC(poz); DEC(Eof) ELSE Buf[i]:=0c END;
  END;
END Next;

PROCEDURE Headers;
  VAR i,n: CARDINAL;
BEGIN
  LOOP
    CASE Buf[1] OF
       0c: (* Empty *)
     |46c: (* File header *)
        mdl^.ctX:=(CARDINAL(Buf[20b])+CARDINAL(Buf[21b])*256);
        mdl^.ctY:=(CARDINAL(Buf[22b])+CARDINAL(Buf[23b])*256);
     |60c: (* Technology header *)
        RouterMode:=BITSET(INTEGER(Buf[2b])+INTEGER(Buf[3b])*256);
        RoutGrid[0]:=INTEGER(Buf[4b])+INTEGER(Buf[5b])*256;
        RoutGrid[1]:=INTEGER(Buf[6b])+INTEGER(Buf[7b])*256;
        ViasGrid   :=INTEGER(Buf[10b])+INTEGER(Buf[11b])*256;
        Clearance  :=INTEGER(Buf[12b])+INTEGER(Buf[13b])*256;
     |61c: (* Technology tracks *)
        FOR i:=0 TO 15 DO
          Tracks[i].diameter:=
             CARDINAL(Buf[i*2+2])+CARDINAL(Buf[i*2+3])*256;
          n:=CARDINAL(Buf[i*2+42b]);
          IF n>=128 THEN n:=CARDINAL(BITSET(n)+{8..31}) END;
          Tracks[i].displaceX:=n;
          n:=CARDINAL(Buf[i*2+43b]);
          IF n>=128 THEN n:=CARDINAL(BITSET(n)+{8..31}) END;
          Tracks[i].displaceY:=n;
        END;
     |62c: (* Technology pins *)
        FOR i:=0 TO 15 DO
          Pins[i].diameter0:=CARDINAL(Buf[i*2+2])+CARDINAL(Buf[i*2+3])*256;
          Pins[i].diameter1:=CARDINAL(Buf[i*2+42b])+CARDINAL(Buf[i*2+43b])*256;
          Resist:=INTEGER(Buf[102b])+INTEGER(Buf[103b])*256;
        END;
     |63c: (* Technology vias *)
        FOR i:=0 TO 15 DO
          Vias[i].type:=CARDINAL(Buf[i+2]) MOD 16;
          Vias[i].dril:=CARDINAL(Buf[i+2]) DIV 16;
          n:=CARDINAL(Buf[i*2+22b]);
          IF n>=200b THEN n:=CARDINAL(BITSET(n)+{8..31}) END;
          Vias[i].displaceX:=n;
          n:=CARDINAL(Buf[i*2+23b]);
          IF n>=200b THEN n:=CARDINAL(BITSET(n)+{8..31}) END;
          Vias[i].displaceY:=n;
        END;
     |64c: (* Legend *)
     |04c: RETURN
     |40c: RETURN
    ELSE (* Unknown *)
    END;
    Next;
  END;
END Headers;

VAR ChipName: String;
    ChipTypeName: String;
    Chip: Object;
    ChipType: Object;

PROCEDURE LookUpSig(nm: String): Object;
  VAR Sig: Object;
BEGIN
  Sig:=cdsHash.LookUp(nm,signal);
  IF Sig=NIL THEN
    Sig:=NewObject(signal);
    Sig^.Name:=nm;
    Tie(mdl^.All,Sig);
    cdsHash.Insert(Sig);
    INC(SigCnt);
  END;
  RETURN Sig;
END LookUpSig;

PROCEDURE FindChipType(): Object;
  VAR c: Exception; e: Reaction;
BEGIN
  ChipType:=NIL;
  ChipType:=cdsHash.LookUp(ChipTypeName,chiptype);
  IF ChipType#NIL THEN RETURN ChipType END;
  print('Chip type %s',ChipTypeName);
  Confirm(' not found in model, enter file name: ',ChipTypeName);
  print('\n');
  LOOP
    c:=Exception(Exception?(e));
    IF BOOLEAN(c) THEN
      IF c#IOerror THEN RaiseInMe(c) END;
      print('%s\n',Message);
      Confirm('Enter file name: ',ChipTypeName);
      print('\n');
    ELSE
      ChipType:=ReadShortModel(ChipTypeName);
      cdsHash.Insert(ChipType);
      KillReaction(e); EXIT
    END;
  END;
  Tie(mdl^.All,ChipType);
  RETURN ChipType;
END FindChipType;

PROCEDURE ExternalPin?(p: Object);
  VAR x,y,i: INTEGER; e,c,t: Object;
BEGIN
  ASSERT(Buf[1]=44c);
  x:=INTEGER(Buf[06b])+INTEGER(Buf[07b])*256;
  y:=INTEGER(Buf[10b])+INTEGER(Buf[11b])*256;
  c:=p^.Chip;
  IF c#NIL THEN
    t:=c^.ChipType;
    IF t#NIL THEN
      e:=Lget(t^.ExternalPins,p^.No);
      IF e=NIL THEN
        e:=NewObject(externalpin);
        e^.Host:=t; e^.EPinNo:=p^.No;
        i:=INTEGER(Buf[4b]) MOD 16;
        IF i=0 THEN
          e^.TrackWidth:=0
        ELSE
          e^.TrackWidth:=Tracks[i].diameter DIV 2;
        END;
        Lset(t^.ExternalPins,e^.EPinNo,e);
      END;
      DEC(x,c^.XB); DEC(y,c^.YB);
      CASE c^.RB MOD 4 OF
        0:
       |1: i:=-y; y:=x;  x:=i;
       |2: x:=-x; y:=-y;
       |3: i:=y;  y:=-x; x:=i;
      END;
      e^.PinX:=x; e^.PinY:=y;
    END;
  END;
END ExternalPin?;

PROCEDURE ComponentSections;
  VAR i,l,t: CARDINAL; p,s: Object;
BEGIN
  LOOP
    WHILE (Buf[1]#4c)&(Buf[1]#40c) DO Next END;
    IF Buf[1]=40c THEN EXIT END;
    FOR i:=0 TO 7 DO ChipName[i]:=Buf[i+2b] END; ChipName[8]:=0c;
    FOR i:=0 TO 7 DO IF ChipName[i]<=' ' THEN ChipName[i]:=0c END END;
    IF ChipName[0]#0c THEN
      FOR i:=0 TO 7 DO ChipTypeName[i]:=Buf[i+12b] END; ChipTypeName[8]:=0c;
      FOR i:=0 TO 7 DO
        IF ChipTypeName[i]<=' ' THEN ChipTypeName[i]:=0c END
      END;
      Chip:=NewObject(chip);
      Chip^.Name:=ChipName;
      Chip^.ChipType:=FindChipType();
      Chip^.XB:=(CARDINAL(Buf[24b])+CARDINAL(Buf[25b])*256);
      Chip^.YB:=(CARDINAL(Buf[26b])+CARDINAL(Buf[27b])*256);
      Chip^.RB:=CARDINAL(Buf[22b]) MOD 4;
      Tie(mdl^.All,Chip);
      cdsHash.Insert(Chip);
      INC(ChipCnt);
      Next;
      LOOP
        WHILE (Buf[1]#44c)&(Buf[1]#4c)&(Buf[1]#40c) DO Next END;
        IF (Buf[1]=40c)OR(Buf[1]=4c) THEN EXIT END;
        p:=NewObject(pin);
        Tie(mdl^.All,p);
        p^.No:=CARDINAL(Buf[22b])-1;
        p^.Chip:=Chip; Lset(Chip^.Pins,p^.No,p);
        s:=LookUpSig('..free..');
        INCL(s^.sType,fixed);
        p^.Signal:=s;
        Tie(s^.TiedPins,p);
        StartConductor(s);
        t:=CARDINAL(Buf[2]) MOD 16;
        Size:=Pins[t].diameter0 DIV 2;
        ViasSize:=INTEGER(Buf[2]) DIV 16;
        Fixed:=TRUE;
        Layer:={0,1};
        X1  :=(CARDINAL(Buf[6])+CARDINAL(Buf[7])*256);
        X2  :=X1;
        Y1  :=(CARDINAL(Buf[8])+CARDINAL(Buf[9])*256);
        Y2  :=Y1;
        AppConductor;
        ExternalPin?(p);
        Next;
      END;
    ELSE
      Next;
    END;
  END;
END ComponentSections;

VAR SignalName: String;
    Signal: Object;

PROCEDURE Conductor;
  VAR l,t,i: CARDINAL; c: Object;
BEGIN
  CASE Buf[1] OF
    41c:
      t:=CARDINAL(Buf[2]) MOD 16;
      ViasSize:=0;
      Fixed:=7 IN BITSET(Buf[3]);
      Size:=Tracks[t].diameter DIV 2;
      Layer:={};
      INCL(Layer,CARDINAL(Buf[3]) MOD 8);
      Y1  :=(CARDINAL(Buf[4])+CARDINAL(Buf[5])*256);
      Y2  :=Y1;
      X1  :=(CARDINAL(Buf[6])+CARDINAL(Buf[7])*256);
      X2  :=(CARDINAL(Buf[8])+CARDINAL(Buf[9])*256);
   |42c:
      t:=CARDINAL(Buf[2]) MOD 16;
      ViasSize:=0;
      Fixed:=7 IN BITSET(Buf[3]);
      Size:=Tracks[t].diameter DIV 2;
      Layer:={};
      INCL(Layer,CARDINAL(Buf[3]) MOD 8);
      X1  :=(CARDINAL(Buf[4])+CARDINAL(Buf[5])*256);
      X2  :=X1;
      Y1  :=(CARDINAL(Buf[6])+CARDINAL(Buf[7])*256);
      Y2  :=(CARDINAL(Buf[8])+CARDINAL(Buf[9])*256);
   |47c:
      t:=CARDINAL(Buf[2]) MOD 16;
      ViasSize:=0;
      Fixed:=7 IN BITSET(Buf[3]);
      Size:=Tracks[t].diameter DIV 2;
      Layer:={};
      INCL(Layer,CARDINAL(Buf[3]) MOD 8);
      Y1  :=(CARDINAL(Buf[6])+CARDINAL(Buf[7])*256);
      Y2  :=(CARDINAL(Buf[10])+CARDINAL(Buf[11])*256);
      X1  :=(CARDINAL(Buf[4])+CARDINAL(Buf[5])*256);
      X2  :=(CARDINAL(Buf[8])+CARDINAL(Buf[9])*256);
   |43c:
      t:=CARDINAL(Buf[3]) MOD 16;
      X1  :=(CARDINAL(Buf[4])+CARDINAL(Buf[5])*256);
      Y1  :=(CARDINAL(Buf[6])+CARDINAL(Buf[7])*256);
      Fixed:=7 IN BITSET(Buf[3]);
      X2:=X1; Y2:=Y1;
      Size:=Pins[Vias[t].type].diameter0 DIV 2;
      ViasSize:=Vias[t].dril;
      Layer:={0,1};
  |44c:
      FOR i:=0 TO 7 DO ChipName[i]:=Buf[i+12b] END; ChipName[8]:=0c;
      FOR i:=0 TO 7 DO
        IF ChipName[i]<=' ' THEN ChipName[i]:=0c END
      END;
      Chip:=cdsHash.LookUp(ChipName,chip);
      IF Chip#NIL THEN
        i:=CARDINAL(Buf[22b])-1;
        c:=Lget(Chip^.Pins,i);
        IF c=NIL THEN
          c:=NewObject(pin);
          Lset(Chip^.Pins,i,c);
          c^.Chip:=Chip;
          c^.No:=i;
          Tie(mdl^.All,c);
        END;
        Tie(Signal^.TiedPins,c);
        c^.Signal:=Signal;
        ExternalPin?(c);
      END;
      t:=CARDINAL(Buf[2]) MOD 16;
      Size:=Pins[t].diameter0 DIV 2;
      ViasSize:=INTEGER(Buf[2]) DIV 16;
      Layer:={0,1};
      Fixed:=TRUE;
      X1  :=(CARDINAL(Buf[6])+CARDINAL(Buf[7])*256);
      X2  :=X1;
      Y1  :=(CARDINAL(Buf[8])+CARDINAL(Buf[9])*256);
      Y2  :=Y1;
  END;
END Conductor;

PROCEDURE SignalSections;
  VAR i: CARDINAL;
BEGIN
  LOOP
    WHILE NOT EOF &(Buf[1]#40c) DO Next END;
    IF EOF THEN EXIT END;
    FOR i:=0 TO 7 DO SignalName[i]:=Buf[i+4b] END; SignalName[8]:=0c;
    FOR i:=0 TO 7 DO IF SignalName[i]<=' ' THEN SignalName[i]:=0c END END;
    IF SignalName[0]=0c THEN SignalName:='..free..' END;
    Signal:=LookUpSig(SignalName);
    Next;
    StartConductor(Signal);
    CreateConductor(200);
    LOOP
      WHILE NOT EOF &(Buf[1]#40c)&(Buf[1]#41c)&(Buf[1]#42c)&(Buf[1]#43c)&
           (Buf[1]#44c)&(Buf[1]#47c) DO Next END;
      IF EOF OR (Buf[1]=40c) THEN EXIT END;
      Conductor;
      AppConductor;
      Next;
    END;
    TruncConductor;
  END;
END SignalSections;

PROCEDURE DoGPH(o: Object; Name: ARRAY OF CHAR);
BEGIN
  mdl:=o; SigCnt:=0; ChipCnt:=0;
  checkHALT(OpenOnDir(CD(),inp,Name),'GPH: ');
  EOF:=FALSE;
  Eof:=GetEof(inp);
  poz:=4096; BlockCnt:=0;
  Next;
  cdsHash.Init(1000);
  Headers;
  ComponentSections;
  SignalSections;
  checkHALT(Close(inp),'GPH: ');
  print('Обработано %d сигналов и %d чипов.\n',SigCnt,ChipCnt);
END DoGPH;

END GPHin.
