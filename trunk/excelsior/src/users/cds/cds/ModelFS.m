IMPLEMENTATION MODULE ModelFS; (* Sem 08-Feb-87. (c) KRONOS *)

IMPORT  str : Strings;
IMPORT  mcd : defCodes;

IMPORT  bio : libBIO;
IMPORT  mem : libHeap;

FROM SYSTEM    IMPORT   WORD, ADR, ADDRESS;

WITH STORAGE : mem;

TYPE
  Str80=ARRAY [0..80] OF CHAR;

VAR
  FileName: Str80;
  Mess    : Str80;

PROCEDURE MOVE(from,to: ADDRESS; n: INTEGER); CODE mcd.move END MOVE;

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

VAR f         : bio.FILE;
    Blocks    : ARRAY [0..99] OF BITSET;
    Created   : BOOLEAN;
    WordPoz   : INTEGER;
    Block     : INTEGER;
    Bufs      : POINTER TO ARRAY [0..3] OF BufRec;
    BufCnt    : INTEGER;

PROCEDURE Open(Name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  str.print(FileName,'%s.mdl',Name);
  bio.open(f,FileName,'r');
  Created:=FALSE;
  IF Bufs=NIL THEN NEW(Bufs) END;
  FOR i:=0 TO HIGH(Bufs^) DO Bufs^[i].Block:=-1 END;
  WordCnt:=bio.eof(f) DIV 4;
  WordPoz:=0;
END Open;

PROCEDURE Create(Name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  str.print(FileName,'%s.mdl',Name);
  bio.create(f,FileName,'rw',50000);
  IF Bufs=NIL THEN NEW(Bufs) END;
  FOR i:=0 TO HIGH(Bufs^) DO Bufs^[i].Block:=-1 END;
  Created:=TRUE;
  WordCnt:=0; WordPoz:=0;
  FOR i:=0 TO HIGH(Blocks) DO Blocks[i]:={} END;
END Create;

PROCEDURE sRead(a: ADDRESS; s: INTEGER);
  VAR i,n,bf: INTEGER;
BEGIN
  LOOP
    n:=WordPoz DIV 1024;
    bf:=0; WHILE (bf<=HIGH(Bufs^))&(Bufs^[bf].Block#n) DO INC(bf) END;
    IF bf>HIGH(Bufs^) THEN
      BufCnt:=(BufCnt+1) MOD (HIGH(Bufs^)+1); bf:=BufCnt; Bufs^[bf].Block:=n;
      bio.seek(f,n*4096,0);
      i:=bio.eof(f)-n*4096;
      IF i>4096 THEN i:=4096 END;
      bio.read(f,ADR(Bufs^[bf].Data),i);
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

PROCEDURE sWrite(a: ADDRESS; s: INTEGER);
  VAR i,n,bf: INTEGER;
BEGIN
  ASSERT(Created);
  LOOP
    n:=WordPoz DIV 1024;
    bf:=0; WHILE (bf<=HIGH(Bufs^))&(Bufs^[bf].Block#n) DO INC(bf) END;
    IF bf>HIGH(Bufs^) THEN
      BufCnt:=(BufCnt+1) MOD (HIGH(Bufs^)+1); bf:=BufCnt;
      IF Bufs^[bf].Block>=0 THEN
        bio.seek(f,Bufs^[bf].Block*4096,0);
        bio.write(f,ADR(Bufs^[bf].Data),4096);
        Incl(Blocks,Bufs^[bf].Block);
      END;
      Bufs^[bf].Block:=n;
      IF In(Blocks,n) THEN
        bio.seek(f,n*4096,0);
        bio.read(f,ADR(Bufs^[bf].Data),4096);
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

PROCEDURE Seek(n: INTEGER);
BEGIN
  WordPoz:=n
END Seek;

PROCEDURE Poz(): INTEGER;
BEGIN
  RETURN WordPoz
END Poz;

PROCEDURE Close();
  VAR i: INTEGER;
BEGIN
  IF Bufs=NIL THEN RETURN END;
  IF Created THEN
    FOR i:=0 TO HIGH(Bufs^) DO
      IF Bufs^[i].Block>=0 THEN
        bio.seek(f,Bufs^[i].Block*4096,0);
        bio.write(f,ADR(Bufs^[i].Data),4096);
      END;
    END;
    bio.end(f,WordCnt*4);
  END;
  bio.close(f);
  DISPOSE(Bufs);
END Close;

BEGIN
  BufCnt:=0; Bufs:=NIL;
END ModelFS.
