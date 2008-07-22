IMPLEMENTATION MODULE GPHvis; (* Sem 09-May-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR;
FROM StdIO     IMPORT   print;
FROM FsPublic  IMPORT   File;
FROM BIO       IMPORT   bRead, OpenOnDir, Close, checkHALT, CD, GetEof;
IMPORT FROM GPHtechnology;

TYPE String=ARRAY [0..15] OF CHAR;
VAR inp: File;
    poz: CARDINAL;
    EOF: BOOLEAN;
    Buf: ARRAY [0..127] OF CHAR;
    Block: ARRAY [0..4095] OF CHAR;
    BlockCnt: CARDINAL;
    Eof: CARDINAL;

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
       0c: print('..empty..\n');
     |46c: print('file header\n');
     |60c: print('technology header\n');
        RouterMode:=BITSET(INTEGER(Buf[2b])+INTEGER(Buf[3b])*256);
        RoutGrid[0]:=INTEGER(Buf[4b])+INTEGER(Buf[5b])*256;
        RoutGrid[1]:=INTEGER(Buf[6b])+INTEGER(Buf[7b])*256;
        ViasGrid   :=INTEGER(Buf[10b])+INTEGER(Buf[11b])*256;
        Clearance  :=INTEGER(Buf[12b])+INTEGER(Buf[13b])*256;
     |61c: print('technology tracks\n');
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
     |62c: print('technology pins\n');
        FOR i:=0 TO 15 DO
          Pins[i].diameter0:=CARDINAL(Buf[i*2+2])+CARDINAL(Buf[i*2+3])*256;
          Pins[i].diameter1:=CARDINAL(Buf[i*2+42b])+CARDINAL(Buf[i*2+43b])*256;
          Resist:=INTEGER(Buf[102b])+INTEGER(Buf[103b])*256;
        END;
     |63c: print('technology vias\n');
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

PROCEDURE PrintPin;
  VAR nm: String; i: INTEGER;
BEGIN
  FOR i:=0 TO 7 DO nm[i]:=Buf[i+12b] END; nm[8]:=0c;
  FOR i:=0 TO 7 DO IF nm[i]<=' ' THEN nm[i]:=0c END END;
  print('pin: %8s %2d (%4d,%4d)',nm,Buf[22b],
         CARDINAL(Buf[6])+CARDINAL(Buf[7])*256,
         CARDINAL(Buf[8])+CARDINAL(Buf[9])*256);
  print(' type %2d  dril %2d\n',INTEGER(Buf[2]) MOD 16,INTEGER(Buf[2]) DIV 16);
  print('     layers for pin %{}, for signal %{},',Buf[3],Buf[5]);
  print(' track type %2d\n',INTEGER(Buf[4]) MOD 16);
END PrintPin;

PROCEDURE ComponentSections;
  VAR i,l,t: CARDINAL;
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
      print('****** component section %8s (%4d,%4d) rot=%1d ******\n',
                ChipName,
                CARDINAL(Buf[24b])+CARDINAL(Buf[25b])*256,
                CARDINAL(Buf[26b])+CARDINAL(Buf[27b])*256,
                CARDINAL(Buf[22b]) MOD 4);
      print('chip type %8s\n',ChipTypeName);
      Next;
      LOOP
        WHILE (Buf[1]#44c)&(Buf[1]#4c)&(Buf[1]#40c) DO Next END;
        IF (Buf[1]=40c)OR(Buf[1]=4c) THEN EXIT END;
        PrintPin;
        Next;
      END;
    ELSE
      Next;
    END;
  END;
END ComponentSections;

VAR SignalName: String;

PROCEDURE Conductor;
  VAR l,t,i: CARDINAL;
BEGIN
  CASE Buf[1] OF
    41c:
      t:=CARDINAL(Buf[2]) MOD 16;
      print('X-segment %4d..%4d %4d, sz=%2d\n',
             CARDINAL(Buf[6])+CARDINAL(Buf[7])*256,
             CARDINAL(Buf[8])+CARDINAL(Buf[9])*256,
             CARDINAL(Buf[4])+CARDINAL(Buf[5])*256,
             Tracks[t].diameter);
   |42c:
      t:=CARDINAL(Buf[2]) MOD 16;
      print('Y-segment %4d..%4d %4d, sz=%2d\n',
             CARDINAL(Buf[6])+CARDINAL(Buf[7])*256,
             CARDINAL(Buf[8])+CARDINAL(Buf[9])*256,
             CARDINAL(Buf[4])+CARDINAL(Buf[5])*256,
             Tracks[t].diameter);
   |47c:
      t:=CARDINAL(Buf[2]) MOD 16;
      print('vector  (%4d,%4d)..(%4d,%4d), sz=%2d\n',
             CARDINAL(Buf[4])+CARDINAL(Buf[5])*256,
             CARDINAL(Buf[6])+CARDINAL(Buf[7])*256,
             CARDINAL(Buf[8])+CARDINAL(Buf[9])*256,
             CARDINAL(Buf[10])+CARDINAL(Buf[11])*256,
             Tracks[t].diameter);

   |43c:
      t:=CARDINAL(Buf[3]) MOD 16;
      print('vias (%4d,%4d) type=%2d\n',
             CARDINAL(Buf[4])+CARDINAL(Buf[5])*256,
             CARDINAL(Buf[6])+CARDINAL(Buf[7])*256,t);
   |44c: PrintPin;
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
    print('****** signal section %8s ******\n',SignalName);
    Next;
    LOOP
      WHILE NOT EOF &(Buf[1]#40c)&(Buf[1]#41c)&(Buf[1]#42c)&(Buf[1]#43c)&
           (Buf[1]#44c)&(Buf[1]#47c) DO Next END;
      IF EOF OR (Buf[1]=40c) THEN EXIT END;
      Conductor;
      Next;
    END;
  END;
END SignalSections;

PROCEDURE DoGPH(Name: ARRAY OF CHAR);
BEGIN
  checkHALT(OpenOnDir(CD(),inp,Name),'GPH: ');
  EOF:=FALSE;
  Eof:=GetEof(inp);
  poz:=4096; BlockCnt:=0;
  Next;
  Headers;
  ComponentSections;
  SignalSections;
  checkHALT(Close(inp),'GPH: ');
END DoGPH;

END GPHvis.
