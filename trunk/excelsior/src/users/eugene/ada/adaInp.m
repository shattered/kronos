IMPLEMENTATION MODULE adaInp; (* 03-Apr-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM BIO        IMPORT  CD, OpenOnDir, bRead, GetEof, checkHALT;
FROM FsPublic   IMPORT  File, FileName;
FROM ASCII      IMPORT  LF, NL;
FROM Args       IMPORT  TakeWord, ScanFlags;
FROM Image      IMPORT  image0;

VAR
  inp : File;
  buf : ARRAY [0..4095] OF CHAR;
  size: INTEGER;
  cnt : INTEGER;
  blk : INTEGER;

PROCEDURE get_ch(): CHAR;
  VAR c: CHAR;
BEGIN
  ch_pos:=blk*4096+cnt;
  IF size<=ch_pos THEN RETURN eof END;
  IF cnt>=4096 THEN
    INC(blk); DEC(cnt,4096);
    checkHALT(bRead(inp,blk,ADR(buf),4096),'');
  END;
  c:=buf[cnt]; INC(cnt);
  IF c=NL THEN c:=LF END;
  RETURN c;
END get_ch;

PROCEDURE set_pos(n: INTEGER);
  VAR b: INTEGER;
BEGIN
  b:=n DIV 4096; cnt:=n MOD 4096;
  IF (b#blk) & (n<size) THEN
    blk:=b;
    checkHALT(bRead(inp,blk,ADR(buf),4096),'');
  END;
END set_pos;

PROCEDURE reset;
BEGIN
  size:=GetEof(inp);
  blk:=0; cnt:=0; ch_pos:=-1;
  IF size>0 THEN
    checkHALT(bRead(inp,0,ADR(buf),4096),'');
  END;
END reset;

VAR
  w,w1: FileName;

BEGIN
  ScanFlags;
  TakeWord(w); image0(w1,'%s.a',w);
  checkHALT(OpenOnDir(CD(),inp,w1),'');
  reset;
END adaInp.
