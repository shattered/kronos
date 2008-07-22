MODULE fnt; (* 19-Nov-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM StdIO      IMPORT  Read, print;
FROM ASCII      IMPORT  NL;
FROM FsPublic   IMPORT  FileName, File;
IMPORT Terminal, BIO;

VAR nm: FileName;
    f: File;
    i: INTEGER;
    buf: ARRAY [0..10000] OF INTEGER;

BEGIN
  nm:='gt.fnt';
  BIO.checkHALT(BIO.OpenOnDir(BIO.CD(),f,nm),'');
  BIO.checkHALT(BIO.bRead(f,0,ADR(buf),BIO.GetEof(f)),'');
  print('%d, %d, %d,\n',buf[0],buf[1],buf[2]);
  FOR i:=3 TO BIO.GetEof(f) DIV 4 -1 DO
    print('%3$hh,',buf[i]);
    IF ((i-3) MOD 12)=11 THEN print('\n') END;
  END;
  print('\n');
  BIO.checkHALT(BIO.Close(f),'');
END fnt.
