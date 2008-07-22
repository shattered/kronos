IMPLEMENTATION MODULE ModelFS; (* Sem 08-Feb-87. (c) KRONOS *)

IMPORT BIO, FsPublic;
FROM Strings   IMPORT   Str1, AppStr, App, DelCh, Len;
FROM FsPublic  IMPORT   File, VisFSerr;
FROM ModelPbl  IMPORT   RaiseInMe, IOerror, Message;
FROM SYSTEM    IMPORT   WORD, ADR, ADDRESS;
FROM KRONOS    IMPORT   MOVE;
FROM cdsHeap   IMPORT   Allocate, Deallocate, HardAllocate;

TYPE Str80=ARRAY [0..80] OF CHAR;
VAR FileName: FsPublic.FileName;
    Mess: Str80;

PROCEDURE Err(s: ARRAY OF CHAR);
BEGIN
END Err;

PROCEDURE In(VAR s: ARRAY OF BITSET; n: INTEGER): BOOLEAN;
BEGIN
  RETURN (n MOD 32) IN s[n DIV 32];
END In;

PROCEDURE Incl(VAR s: ARRAY OF BITSET; n: INTEGER);
BEGIN
  INCL(s[n DIV 32],n MOD 32);
END Incl;

TYPE BufRec=RECORD
              Data: ARRAY [0..1023] OF WORD;
              Block: INTEGER;
            END;

VAR f: File;
    Blocks    : ARRAY [0..99] OF BITSET;
    Created   : BOOLEAN;
    WordPoz   : INTEGER;
    Block     : INTEGER;
    Bufs      : POINTER TO ARRAY [0..3] OF BufRec;
    BufCnt    : INTEGER;

PROCEDURE Check(b: BOOLEAN; n: INTEGER);
BEGIN
  IF NOT b THEN RETURN END;
  CASE n OF
     0: Str1(Message,'При чтении файла ');
    |1: Str1(Message,'При записи файла ');
    |2: Str1(Message,'При открытии файла ');
    |3: Str1(Message,'При создании файла ');
    |4: Str1(Message,'При закрытии файла ');
  END;
  VisFSerr(b,Mess); DelCh(Mess,Len(Mess)-1);
  AppStr(Message,FileName); AppStr(Message,' :');
  AppStr(Message,Mess); App(Message,'.');
  IF Bufs#NIL THEN Deallocate(Bufs,SIZE(Bufs^)) END;
  RaiseInMe(IOerror);
END Check;

PROCEDURE Open(Name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  Str1(FileName,Name);
  Check(BIO.Open(f,Name,Err),2);
  Created:=FALSE;
  IF Bufs=NIL THEN Allocate(Bufs,SIZE(Bufs^)) END;
  FOR i:=0 TO HIGH(Bufs^) DO Bufs^[i].Block:=-1 END;
  WordCnt:=BIO.GetEof(f) DIV 4;
  WordPoz:=0;
END Open;

PROCEDURE Create(Name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  Str1(FileName,Name);
  i:=50000;
  IF NOT BIO.Open(f,Name,Err) THEN i:=BIO.GetEof(f); Check(BIO.Close(f),3) END;
  Check(BIO.Create(f),3);
  Check(BIO.Extend(f,i),3);
  IF Bufs=NIL THEN HardAllocate(Bufs,SIZE(Bufs^)) END;
  FOR i:=0 TO HIGH(Bufs^) DO Bufs^[i].Block:=-1 END;
  Created:=TRUE;
  WordCnt:=0;
  WordPoz:=0;
  FOR i:=0 TO HIGH(Blocks) DO Blocks[i]:={} END;
END Create;

PROCEDURE sRead(a: ADDRESS; s: CARDINAL);
  VAR i,n,bf: CARDINAL;
BEGIN
  LOOP
    n:=WordPoz DIV 1024;
    bf:=0; WHILE (bf<=HIGH(Bufs^))&(Bufs^[bf].Block#n) DO INC(bf) END;
    IF bf>HIGH(Bufs^) THEN
      BufCnt:=(BufCnt+1) MOD (HIGH(Bufs^)+1); bf:=BufCnt; Bufs^[bf].Block:=n;
      Check(BIO.bRead(f,n,ADR(Bufs^[bf].Data),4096),0);
    END;
    i:=WordPoz MOD 1024;
    IF (i+s)>1024 THEN
      n:=1024-i;
      MOVE(a,ADR(Bufs^[bf].Data[i]),n); INC(WordPoz,n); INC(a,n); DEC(s,n);
    ELSE
      MOVE(a,ADR(Bufs^[bf].Data[i]),s); INC(WordPoz,s); RETURN
    END;
  END;
END sRead;

PROCEDURE sWrite(a: ADDRESS; s: CARDINAL);
  VAR i,n,bf: INTEGER;
BEGIN
  ASSERT(Created);
  LOOP
    n:=WordPoz DIV 1024;
    bf:=0; WHILE (bf<=HIGH(Bufs^))&(Bufs^[bf].Block#n) DO INC(bf) END;
    IF bf>HIGH(Bufs^) THEN
      BufCnt:=(BufCnt+1) MOD (HIGH(Bufs^)+1); bf:=BufCnt;
      IF Bufs^[bf].Block>=0 THEN
        Check(BIO.bWrite(f,Bufs^[bf].Block,ADR(Bufs^[bf].Data),4096),1);
        Incl(Blocks,Bufs^[bf].Block);
      END;
      Bufs^[bf].Block:=n;
      IF In(Blocks,n) THEN
        Check(BIO.bRead(f,n,ADR(Bufs^[bf].Data),4096),0);
      END;
    END;
    i:=WordPoz MOD 1024;
    IF (i+s)>1024 THEN
      n:=1024-i;
      MOVE(ADR(Bufs^[bf].Data[i]),a,n); INC(WordPoz,n); INC(a,n); DEC(s,n);
    ELSE
      MOVE(ADR(Bufs^[bf].Data[i]),a,s); INC(WordPoz,s);
      IF WordPoz>WordCnt THEN WordCnt:=WordPoz END;
      RETURN
    END;
  END;
END sWrite;

PROCEDURE Seek(n: CARDINAL);
BEGIN
  WordPoz:=n
END Seek;

PROCEDURE Poz(): CARDINAL;
BEGIN
  RETURN WordPoz
END Poz;

PROCEDURE Close();
  VAR Dir: File; Name: Str80; fName: FsPublic.FileName; i: INTEGER;
BEGIN
  IF Bufs=NIL THEN RETURN END;
  IF Created THEN
    Name:=FileName;
    FOR i:=0 TO HIGH(Bufs^) DO
      IF Bufs^[i].Block>=0 THEN
        Check(BIO.bWrite(f,Bufs^[i].Block,ADR(Bufs^[i].Data),4096),1);
      END;
    END;
    BIO.SetEof(f,WordCnt*4);
    Check(BIO.Go(BIO.CD(),Dir,Name,Err),4);
    Str1(fName,Name);
    Check(BIO.Link(Dir,fName,f),4);
    Check(BIO.Cut(f),4);
  END;
  Check(BIO.Close(f),4);
--  Deallocate(Bufs,SIZE(Bufs^));
END Close;

BEGIN
  BufCnt:=0;
  Bufs:=NIL;
  Allocate(Bufs,SIZE(Bufs^));
END ModelFS.
