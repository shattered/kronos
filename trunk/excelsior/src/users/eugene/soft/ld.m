MODULE ld; (*$T- Sem 02-Jun-87. (c) KRONOS *)

                IMPORT  Labtam8086;
FROM SYSTEM     IMPORT  ADR;
FROM KRONOS     IMPORT  MOVE;
FROM FsPublic   IMPORT  File, FileName;
FROM BIO        IMPORT  OpenOnDir, checkHALT, bRead, Close, CD, GetEof;
FROM Args       IMPORT  ScanFlags, TakeWord;
FROM Terminal   IMPORT  print;

CONST
    iCSR=177520b DIV 2;
    iDTR=iCSR+1;
    oCSR=iCSR+2;
    oDTR=iCSR+3;

VAR Rom      : ARRAY [0..4095] OF ARRAY [0..7] OF CHAR;
    Name     : FileName;
    Inp      : File;
    Eof      : INTEGER;
    i,j,k,cnt: INTEGER;
    s        : ARRAY [0..3] OF CHAR;
    p,l      : INTEGER;

BEGIN
  Rom[0][0]:=0c; Rom[0][1]:=0c; Rom[0][2]:=0c; Rom[0][3]:=0c;
  MOVE(ADR(Rom)+1,ADR(Rom),SIZE(Rom)-1);
  ScanFlags; TakeWord(Name);
  checkHALT(OpenOnDir(CD(),Inp,Name),Name);
  Eof:=SIZE(Rom)*4;
  IF Eof>GetEof(Inp) THEN Eof:=GetEof(Inp) END;
  checkHALT(bRead(Inp,0,ADR(Rom),Eof),Name);
  checkHALT(Close(Inp),Name);
  Labtam8086.InitBCB(Labtam8086.Channel(3));
  Labtam8086.SISO     :=FALSE;
  Labtam8086.NewLine  :=FALSE;
  Labtam8086.EndString:=FALSE;
  FOR i:=0 TO HIGH(Rom) DO
    IF i MOD 100 =0 THEN print('%5d\r',i) END;
    FOR j:=0 TO 7 DO
      s[0]:=Rom[i][7-j]; p:=0; l:=1;
      REPEAT
        Labtam8086.puts(s,p,l);
      UNTIL l=0;
    END;
  END;
END ld.
