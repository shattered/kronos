MODULE roms; (* 10-May-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Args       IMPORT  TakeWord;
FROM Image      IMPORT  image0;
FROM StdIO      IMPORT  Show, print;
FROM BIO        IMPORT  checkHALT, LookUpFile, Create, CD, bRead, bWrite
                      , Close, Link, GetEof, SetEof, Open, dummyErr;


PROCEDURE Help;
BEGIN
  print("  roms <filename>\n");
END Help;

TYPE line=ARRAY [0..7] OF CHAR;

VAR rom: ARRAY [0..4095] OF line;
      f: INTEGER;
      N: INTEGER;
    i,j: INTEGER;
   name: ARRAY [0..127] OF CHAR;
   rt11: ARRAY [0..127] OF CHAR;
   dest: ARRAY [0..31] OF CHAR;
   buf : ARRAY [0..20000b+4095] OF CHAR;

PROCEDURE PutRom(i,j: INTEGER);
  VAR n: INTEGER;
BEGIN
  FOR n:=0 TO 2047 DO
    buf[20000b+n*2]:=rom[n+i*2048][j];
    buf[20000b+n*2+1]:=0c;
  END;
END PutRom;

BEGIN
  TakeWord(name); rt11:='rt11';
  IF name="" THEN Help; HALT END;
  checkHALT(LookUpFile(f,name,dummyErr),name);
  IF GetEof(f)>SIZE(rom)*4 THEN print('Too long file...\n'); HALT END;
  checkHALT(bRead(f,0,ADR(rom),GetEof(f)),name);
  N:=GetEof(f) DIV 8;
  checkHALT(Close(f),name);
  checkHALT(LookUpFile(f,rt11,Show),rt11);
  checkHALT(bRead(f,0,ADR(buf),512),rt11);
  checkHALT(Close(f),rt11);
  FOR i:=512 TO 20000b-1 DO buf[i]:=0c END;
  FOR i:=0 TO 1 DO
    FOR j:=0 TO 7 DO
      image0(dest,"%d%d.FIL",i,j);
      checkHALT(Create(f),dest);
      PutRom(i,j);
      checkHALT(bWrite(f,0,ADR(buf),20000b+4096),dest);
      SetEof(f,20000b+4096);
      checkHALT(Link(CD(),dest,f),dest);
      checkHALT(Close(f),dest);
    END;
  END;
END roms.
