MODULE rom; (* 02-Dec-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR;
FROM FsPublic   IMPORT  FileName, File;
FROM Image      IMPORT  image0;
FROM Args       IMPORT  TakeWord;
IMPORT BIO;
IMPORT mcd : mCodeMnem;

VAR nm,w: FileName;
    buf: ARRAY [0..0FFFh] OF INTEGER;
    cod: ARRAY [0..2047] OF INTEGER;
    buf_pos: INTEGER;
    f: File;
    i,j: INTEGER;
    str,str_sz: INTEGER;
    code,code_sz: INTEGER;
    pc0: INTEGER;

PROCEDURE put(n: INTEGER);
BEGIN
  buf[buf_pos]:=-1; DEC(buf_pos);
  buf[buf_pos]:=INTEGER(BITSET(n)/{0..31}); DEC(buf_pos);
END put;

BEGIN
  TakeWord(w);
  image0(nm,'%s.cod',w);
  BIO.checkHALT(BIO.OpenOnDir(BIO.CD(),f,nm),'');
  ASSERT(BIO.GetEof(f)<=SIZE(cod)*4);
  BIO.checkHALT(BIO.bRead(f,0,ADR(cod),BIO.GetEof(f)),'');
  BIO.checkHALT(BIO.Close(f),'');
  str:=16; str_sz:=cod[3];
  code:=16+str_sz; code_sz:=cod[4];
  pc0:=cod[code];

  buf_pos:=HIGH(buf);
  put(mcd.jfl+((code_sz+str_sz)*4-3)<<8);
  i:=code+1; j:=code_sz-1;
  WHILE j>0 DO put(cod[i]); INC(i); DEC(j) END;
  i:=str; j:=str_sz;
  WHILE j>0 DO put(cod[i]); INC(i); DEC(j) END;
  put(mcd.lgw+(mcd.nop<<16)+(mcd.liw<<24));
  put(code_sz);
  put(mcd.add+(mcd.sgw<<8)+(1<<16)+(mcd.jbl<<24));
  put((code_sz+str_sz+3)*4+2-pc0);
  WHILE buf_pos>0 DO put(0) END;

  image0(nm,'%s.rom',w);
  BIO.checkHALT(BIO.Create(f),'');
  BIO.checkHALT(BIO.Link(BIO.CD(),nm,f),'');
  BIO.checkHALT(BIO.bWrite(f,0,ADR(buf),SIZE(buf)*4),'');
  BIO.SetEof(f,SIZE(buf)*4);
  BIO.checkHALT(BIO.Close(f),'');
END rom.
