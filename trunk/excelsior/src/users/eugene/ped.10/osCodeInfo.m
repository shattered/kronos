IMPLEMENTATION MODULE osCodeInfo; (* Leo 01-Feb-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM mCodeMnem  IMPORT  copt, ror, bic;
FROM Universe   IMPORT  IamUnic;

CONST  MAGIC  = 444F3424h; (* "$COD" *)

CONST current  = 2;
      currsize = 10;
      MAGofs   = 9;


PROCEDURE currentVersion(): INTEGER;
BEGIN RETURN current END currentVersion;

PROCEDURE version(c: CodePtr): INTEGER;
  VAR p: StrPtr;
BEGIN p:=c; RETURN ORD(p^[1]) END version;

PROCEDURE infoSize(c: CodePtr): INTEGER;
BEGIN
  IF version(c)=1 THEN RETURN 16 ELSE RETURN 9 END
END infoSize;

PROCEDURE getInfo(c: CodePtr; VAR info: Info);
  VAR w: POINTER TO ARRAY [0..15] OF INTEGER;
      b: POINTER TO ARRAY [0..63] OF INTEGER;
BEGIN w:=c; b:=c;
  info.version:=version(c);
  WITH info DO
    CASE version OF
    |1:
      defTime:=w^[1];           impTime:=w^[2];
      strOfs :=infoSize(c);     name   :=c+strOfs;
      codeOfs:=w^[3]+strOfs;    extOfs :=w^[4]+codeOfs;
      stack0 :=w^[5];           addstk :=w^[6];
      noglo  :=w^[7];           noext  :=w^[8];
      noproc :=w^[9];           compiler:=String4(ADR(w^[12]));
      size   :=w^[11];
    |current:
      ASSERT(w^[MAGofs]=MAGIC,4Fh);
      defTime:=w^[1];           impTime:=w^[2];
      codeOfs:=w^[3];           extOfs :=w^[4];
      stack0 :=w^[5];           addstk :=w^[6];
      size   :=w^[8];           compiler:=String4(ADR(w^[7]));
      strOfs :=infoSize(c);     name   :=c+strOfs;
      noproc :=ORD(b^[0]);      noglo  :=ORD(b^[2]);
      noext  :=ORD(b^[3]);
    ELSE ASSERT(FALSE,4Fh);
    END;
--print("getinfo: (version=%d, strofs=%d codofs=%d extofs=%d)\n"
--                ,version    ,strOfs   ,codeOfs  ,extOfs);
  END;
END getInfo;

PROCEDURE putInfo(c: CodePtr; VAR info: Info);
  VAR w: POINTER TO ARRAY [0..15] OF INTEGER;
      b: POINTER TO ARRAY [0..63] OF INTEGER;
BEGIN w:=c; b:=c;
  info.version:=version(c);
  WITH info DO
    CASE version OF
    |1:
      w^[1]:=defTime;           w^[2]:=impTime;
      w^[3]:=codeOfs-strOfs;    w^[4]:=extOfs-codeOfs;
      w^[5]:=stack0;            w^[6]:=addstk;
      w^[7]:=noglo;             w^[8]:=noext;
      w^[9]:=noproc;            String4(ADR(w^[12])):=compiler;
      w^[11]:=size;
    |current:
      w^[MAGofs]:=MAGIC;
      w^[1]:=defTime;           w^[2]:=impTime;
      w^[3]:=codeOfs;           w^[4]:=extOfs;
      w^[5]:=stack0;            w^[6]:=addstk;
      w^[8]:=size;
      b^[0]:=noproc;            b^[2]:=noglo;
      b^[3]:=noext;             String4(ADR(w^[7])):=compiler;
    ELSE ASSERT(FALSE,4Fh);
    END;
  END;
END putInfo;

PROCEDURE default(VAR info: Info);
BEGIN
  WITH info DO
    version:=current;
       name:=NIL;
   compiler:="M2";
    defTime:=0;
    impTime:=0;
     noproc:=0;
      noglo:=0;
      noext:=0;
     strOfs:=0;
    codeOfs:=0;
     extOfs:=0;
       size:=0;
     stack0:=0;
     addstk:=16*256;
  END;
END default;

(* ------------------------------------------------------ *)

PROCEDURE noExternals(C: ADDRESS): INTEGER;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN i.noext END noExternals;

PROCEDURE noGlobals(C: ADDRESS): INTEGER;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN i.noglo END noGlobals;

PROCEDURE defTime(C: ADDRESS): INTEGER;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN i.defTime END defTime;

PROCEDURE impTime(C: ADDRESS): INTEGER;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN i.impTime END impTime;

PROCEDURE address(w: WORD): ADDRESS;
CODE 1 copt ror bic END address; (* RETURN BITSET(w)-{31} *)

PROCEDURE Code(G: ADDRESS): ADDRESS;
  VAR a: ADDRESS;
BEGIN INC(G);
  a:=INTEGER(G^)-(currsize-MAGofs);
  IF a^=MAGIC THEN RETURN address( INTEGER(G^)-currsize )
  ELSE             RETURN address( INTEGER(G^)-16       )
  END;
END Code;

PROCEDURE FirstExternal(C: ADDRESS; VAR name: StrPtr; VAR deftime: INTEGER);
  VAR i: Info; a: ADDRESS;
BEGIN getInfo(C,i);
  IF i.version=current THEN
    a:=C+i.extOfs; name:=a+1; deftime:=a^;
  ELSE
    name:=C+i.extOfs;         deftime:=-1;
  END;
END FirstExternal;

PROCEDURE NextExternal(C: ADDRESS; VAR name: StrPtr; VAR deftime: INTEGER);
  VAR i: INTEGER; a: ADDRESS;
BEGIN i:=0;
  WHILE name^[i]#0c DO INC(i) END;
  i:=i+(4 - i MOD 4);
  IF version(C)=current THEN
    a:=ADDRESS(name) + i DIV 4; name:=a+1; deftime:=a^;
  ELSE
    name:=ADDRESS(name) + i DIV 4; deftime:=-1;
  END;
END NextExternal;

PROCEDURE ModuleName(G: ADDRESS): StrPtr;
BEGIN G:=G+1; RETURN address(G^) END ModuleName;

PROCEDURE Entry(G: ADDRESS): ADDRESS;
BEGIN G:=G-1; RETURN address(G^) END Entry;

PROCEDURE ProcTableAddress(C: ADDRESS): ADDRESS;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN address(C+i.codeOfs) END ProcTableAddress;

PROCEDURE StringsAddress(C: ADDRESS): ADDRESS;
BEGIN RETURN address(C+16) END StringsAddress;

PROCEDURE CodeName(C: ADDRESS): StrPtr;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN i.name END CodeName;

PROCEDURE getSize(C: ADDRESS): INTEGER;
  VAR i: Info;
BEGIN getInfo(C,i); RETURN i.size END getSize;

PROCEDURE putSize(C: ADDRESS; s: INTEGER);
  VAR i: Info;
BEGIN getInfo(C,i); i.size:=s; putInfo(C,i) END putSize;

BEGIN IamUnic END osCodeInfo.
