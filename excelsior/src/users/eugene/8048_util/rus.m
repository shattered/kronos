MODULE rus; (* 28-May-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Args       IMPORT  ScanFlags, TakeWord, Flag?;
FROM ASCII      IMPORT  NL;
FROM FsPublic   IMPORT  File, FileName;
FROM BIO        IMPORT  Create, OpenOnDir, CD, bRead, bWrite, Close,
                        checkHALT, GetEof, SetEof, Link;

VAR inp,out: File;
    Name   : FileName;
    iblk,oblk: INTEGER;
    iptr,optr: INTEGER;
    ibuf,obuf: ARRAY [0..4095] OF CHAR;
    ieof     : INTEGER;
    pos      : INTEGER;
    ch       : CHAR;

PROCEDURE get;
BEGIN
  IF iptr>=4096 THEN
    INC(iblk); iptr:=0;
    checkHALT(bRead(inp,iblk,ADR(ibuf),4096),Name);
  END;
  ch:=ibuf[iptr]; INC(iptr);
END get;

PROCEDURE put;
BEGIN
  IF optr>=4096 THEN
    checkHALT(bWrite(out,oblk,ADR(obuf),4096),Name);
    INC(oblk); optr:=0;
  END;
  obuf[optr]:=ch; INC(optr);
END put;

BEGIN
  ScanFlags; TakeWord(Name);
  checkHALT(OpenOnDir(CD(),inp,Name),Name);
  checkHALT(Create(out),Name);
  iblk:=0; iptr:=0; ieof:=GetEof(inp);
  oblk:=0; optr:=0; pos:=0;
  IF ieof>0 THEN checkHALT(bRead(inp,0,ADR(ibuf),4096),Name) END;
  IF Flag?('w') THEN
    WHILE ieof>0 DO
      get; IF ch=NL THEN ch:=15c; put; ch:=12c; put; ELSE put END;
      DEC(ieof);
    END;
  ELSE
    WHILE ieof>0 DO
      get;
  --  IF (ch>=140c)&(ch<177c) THEN ch:=CHAR(ORD(ch)+140b) END;
      IF ch=11c THEN
        ch:=' '; REPEAT put; INC(pos) UNTIL (pos MOD 8) = 0;
      ELSIF ch=15c THEN
      ELSE
        put; IF ch=12c THEN pos:=0 ELSE INC(pos) END;
      END;
      DEC(ieof);
    END;
  END;
  IF optr>0 THEN checkHALT(bWrite(out,oblk,ADR(obuf),optr),Name) END;
  checkHALT(Close(inp),Name);
  SetEof(out,oblk*4096+optr);
  checkHALT(Link(CD(),Name,out),Name);
  checkHALT(Close(out),Name);
END rus.
