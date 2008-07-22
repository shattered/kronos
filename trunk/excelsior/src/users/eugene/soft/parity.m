MODULE parity; (* Sem 02-Jun-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   WORD, ADR, ADDRESS;
FROM FsPublic  IMPORT   File, FileName;
FROM BIO       IMPORT   OpenOnDir, checkHALT, bRead, Close,
                        CD, GetEof, bWrite;
FROM Args      IMPORT   ScanFlags, TakeWord, NumFlag?;
FROM StdIO     IMPORT   print;

PROCEDURE MOVE(a,b: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

VAR Rom         : ARRAY [0..4095] OF ARRAY [0..7] OF CHAR;
    Name        : FileName;
    Inp         :  File;
    Eof         : INTEGER;
    i,j,k,cnt   : INTEGER;
    bit         : INTEGER;
    bitL,bitH   : INTEGER;
    bitS        : INTEGER;


BEGIN
  ScanFlags;
  TakeWord(Name);
  IF NumFlag?('b',bit)OR(Name[0]=0c) THEN
    print('parity <rom name> b=<bit no>\n');
    HALT;
  END;
  bitL:=bit MOD 8; bitH:=bit DIV 8;
  bitS:=INTEGER({bitL});
  Rom[0][0]:=0c; Rom[0][1]:=0c; Rom[0][2]:=0c; Rom[0][3]:=0c;
  MOVE(ADR(Rom)+1,ADR(Rom),SIZE(Rom)-1);
  checkHALT(OpenOnDir(CD(),Inp,Name),Name);
  Eof:=SIZE(Rom)*4;
  IF Eof>GetEof(Inp) THEN Eof:=GetEof(Inp) END;
  checkHALT(bRead(Inp,0,ADR(Rom),Eof),Name);
  FOR i:=0 TO Eof DIV 8 -1 DO
    cnt:=0;
    FOR j:=0 TO 7 DO
      FOR k:=0 TO 7 DO
        IF k IN BITSET(Rom[i][j]) THEN INC(cnt) END;
      END;
    END;
    IF (cnt MOD 2)#1 THEN
      IF bitL IN BITSET(Rom[i][bitH]) THEN
        Rom[i][bitH]:=CHAR(INTEGER(Rom[i][bitH])-bitS);
      ELSE
        Rom[i][bitH]:=CHAR(INTEGER(Rom[i][bitH])+bitS);
      END;
    END;
  END;
  checkHALT(bWrite(Inp,0,ADR(Rom),Eof),Name);
  checkHALT(Close(Inp),Name);
END parity.
