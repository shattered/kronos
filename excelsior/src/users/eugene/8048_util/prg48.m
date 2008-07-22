MODULE prg48; (* Sem 20-Sep-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Terminal   IMPORT  print;
FROM FsPublic   IMPORT  File, FileName;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Edit       IMPORT  ReadString;
FROM BIO        IMPORT  SetEof, OpenOnDir, CD, Close, bRead, bWrite, Create,
                        Link, checkHALT, GetEof;

VAR rom: ARRAY [0..2047] OF ARRAY [0..7] OF CHAR;

PROCEDURE readROM;
  VAR inp: File; Name: FileName;
BEGIN
  TakeWord(Name);
  checkHALT(OpenOnDir(CD(),inp,Name),Name);
  checkHALT(bRead(inp,0,ADR(rom),SIZE(rom)*4),Name);
  checkHALT(Close(inp),Name);
END readROM;

PROCEDURE writeROM;
  VAR out: File; Name: FileName;
BEGIN
  Name:=''; ReadString('Output file: ',Name); print('\n');
  checkHALT(Create(out),Name);
  checkHALT(bWrite(out,0,ADR(rom),SIZE(rom)*4),Name);
  checkHALT(Link(CD(),Name,out),Name);
  SetEof(out,SIZE(rom)*4);
  checkHALT(Close(out),Name);
END writeROM;

PROCEDURE readABS;
  VAR buf   : ARRAY [0..4095] OF CHAR;
      blk   : INTEGER;
      cnt   : INTEGER;
      eof   : INTEGER;
      inp   : File;
      Name  : FileName;
      summ  : INTEGER;
  PROCEDURE get(): CHAR;
  BEGIN
    IF eof<=0 THEN RETURN 0c END;
    IF cnt>=4096 THEN
      INC(blk); cnt:=0;
      checkHALT(bRead(inp,blk,ADR(buf),4096),Name);
    END;
    INC(summ,ORD(buf[cnt])); DEC(eof); INC(cnt); RETURN buf[cnt-1]
  END get;
  VAR len,adr: INTEGER;
BEGIN
  FOR cnt:=0 TO HIGH(rom) DO rom[cnt][0]:=377c END;
  TakeWord(Name);
  checkHALT(OpenOnDir(CD(),inp,Name),Name);
  blk:=0; cnt:=0; eof:=GetEof(inp);
  IF eof>0 THEN checkHALT(bRead(inp,blk,ADR(buf),4096),Name) END;
  WHILE eof>0 DO
    summ:=0;
    IF get()=1c THEN
      IF get()#0c THEN print('Illegal record format.\n'); HALT END;
      len:=ORD(get())+ORD(get())*256;
      adr:=ORD(get())+ORD(get())*256;
      WHILE len>6 DO
        DEC(len); rom[adr][0]:=get(); INC(adr);
      END;
      IF get()=0c THEN END;
      IF summ MOD 256 # 0 THEN print('Check summ error.\n'); HALT END;
    END;
  END;
  checkHALT(Close(inp),Name);
END readABS;

PROCEDURE writeABS;
  VAR buf   : ARRAY [0..4095] OF CHAR;
      blk   : INTEGER;
      cnt   : INTEGER;
      out   : File;
      Name  : FileName;
      summ  : INTEGER;
  PROCEDURE put(c: CHAR);
  BEGIN
    IF cnt>=4096 THEN
      checkHALT(bWrite(out,blk,ADR(buf),4096),Name);
      INC(blk); cnt:=0;
    END;
    buf[cnt]:=c; INC(cnt);
    INC(summ,ORD(c));
  END put;
  VAR i,j: INTEGER;
BEGIN
  Name:=''; ReadString('Output name? ',Name); print('\n');
  checkHALT(Create(out),Name);
  blk:=0; cnt:=0; summ:=0;
  FOR j:=0 TO 2048 DIV 32 -1 DO
    put(1c); put(0c); put(46c); put(0c); put(CHAR(j*32)); put(CHAR(j DIV 8));
    FOR i:=0 TO 31 DO put(rom[i+j*32][0]) END;
    put(CHAR(-summ)); summ:=0;
  END;
  put(1c); put(0c); put(6c); put(0c); put(0c); put(0c);
  put(CHAR(-summ)); summ:=0;
  IF cnt>0 THEN checkHALT(bWrite(out,blk,ADR(buf),cnt),Name) END;
  SetEof(out,blk*4096+cnt);
  checkHALT(Link(CD(),Name,out),Name);
  checkHALT(Close(out),Name);
END writeABS;

PROCEDURE check;
  VAR i,j,b: INTEGER; r: BITSET; t: ARRAY [0..7] OF INTEGER;
BEGIN
  FOR i:=0 TO 2047 DO
    FOR b:=0 TO 7 DO t[b]:=0 END;
    FOR j:=0 TO 7 DO
      FOR b:=0 TO 7 DO IF b IN BITSET(rom[i][j]) THEN INC(t[b]) END END;
    END;
    r:={};
    FOR b:=0 TO 7 DO
      IF    t[b]>=7 THEN INCL(r,b)
      ELSIF t[b]>=2 THEN print('Bad %$3h: %d\n',i,t[b])
      END;
    END;
    rom[i][0]:=CHAR(r);
  END;
END check;

BEGIN
  ScanFlags;
  IF Flag?('w') THEN
    readROM; check; writeABS;
  ELSIF Flag?('r') THEN
    readABS; writeROM;
  END;
END prg48.
