MODULE dmp; (* 02-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM BIO        IMPORT  OpenOnDir, CD, checkHALT, bRead, Close;
FROM Terminal   IMPORT  print, Read, Write;
FROM FsPublic   IMPORT  File, FileName;

VAR mem: ARRAY [0..0FFFFh] OF INTEGER;
    fl : File;
    nm : FileName;

PROCEDURE pult;
  VAR v,i: INTEGER; a: INTEGER; ch: CHAR;
BEGIN
  a:=0;
  LOOP
    a:=INTEGER(BITSET(a)*{0..23});
    IF a<0 THEN a:=0 END;
    IF a>HIGH(mem) THEN a:=HIGH(mem) END;
    print('%$8h=%$8h ',a,mem[a]); i:=0; v:=0;
    LOOP
      ch:=Read();
      IF ch='q' THEN print('\n'); RETURN END;
      IF ch=12c THEN IF i>0 THEN mem[a]:=v END; INC(a); EXIT END;
      IF ch=15c THEN IF i>0 THEN mem[a]:=v END; DEC(a); EXIT END;
      IF ch='/' THEN
        IF i>0 THEN a:=v ELSE a:=mem[a] END; Write(ch);
        EXIT
      END;
      IF (ch>='0')&(ch<='9') THEN
        v:=INTEGER(BITSET(ch)*{0..3}+(BITSET(v<<4)-{0..3}));
        INC(i); Write(ch);
      END;
      IF (ch>='a')&(ch<='f') THEN
        v:=INTEGER(BITSET(ORD(ch)+9)*{0..3}+(BITSET(v<<4)-{0..3}));
        INC(i); Write(ch);
      END;
    END;
    print('\n');
  END;
END pult;

BEGIN
  nm:='DUMP';
  checkHALT(OpenOnDir(CD(),fl,nm),nm);
  checkHALT(bRead(fl,0,ADR(mem),SIZE(mem)*4),nm);
  checkHALT(Close(fl),nm);
  pult;
END dmp.
