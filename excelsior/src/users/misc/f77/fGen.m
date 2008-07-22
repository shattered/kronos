IMPLEMENTATION MODULE fGen;

FROM SYSTEM    IMPORT   ADDRESS, ADR;
FROM KRONOS    IMPORT   MOVE;
FROM fScan     IMPORT   Error, Info, Unpack, Fault, SetMode, GenObj,
                        Idname, GetMode, Mode, IdStr, Sval, StrId;
FROM CodeFiles IMPORT   Attrs, CreateCode, CloseCode, WriteInfo,
                        WriteProcTable, WriteConsts, WriteCode, WriteExt;
FROM StdIO     IMPORT   Write, WriteString, CR, WriteLn,
                        print, ClearLine, Show;
FROM fDcl      IMPORT   global, unit, self, ModIdno;
FROM Clock     IMPORT   SystemClock;
FROM Config    IMPORT   SysInfo;

CONST CodeSize=9999; StpSize=2047; maxEstkDepth=7; eol=0c;

VAR cpSave,cpSave1,procno,
    depth,depthsave: INTEGER;
    sp   :INTEGER; (* указатель в STP *)
    COD  :ARRAY [0..CodeSize] OF CHAR;
    STP  :ARRAY [0.. StpSize] OF CHAR;
    pTab :ARRAY [0..31] OF INTEGER;
    Ext  :ARRAY [0..31] OF INTEGER; (* список внешних имен *)
    ext  :INTEGER; (* счетчик внешних,указывает на первый свободный *)
    Name :ARRAY [0..15] OF CHAR; (* имя текущего модуля *)
    I:Info; (* for help *)

PROCEDURE c(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  (*$T-*) COD[cp]:=CHAR(n); INC(cp); (*$T+*)
  IF cp>=HIGH(COD) THEN Fault(0); END;
END c;

PROCEDURE b2(n: INTEGER);
BEGIN
  c(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  c(INTEGER(BITSET(n)*{0..7}))
END b2;

PROCEDURE b4(n: INTEGER);
BEGIN
  c(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  c(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  c(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  c(INTEGER(BITSET(n)*{0..7}));
END b4;

CONST NOP=0CBh; copt=0B5h;

PROCEDURE c1(cmd: INTEGER; n1: INTEGER);
BEGIN c(cmd); c(n1); END c1;

PROCEDURE c2(cmd: INTEGER; n1,n2: INTEGER);
BEGIN c(cmd); c(n1); c(n2); END c2;

CONST JLFC=18h; JLF=19h; JSFC=1Ah; JSF=1Bh;
      JLBC=1Ch; JLB=1Dh; JSBC=1Eh; JSB=1Fh; INVLD=0FFh;
      RTN =0CAh;

PROCEDURE JB(cond: BOOLEAN; To: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=cp+2-To; ASSERT(i>0);
  IF i>0FFh THEN INC(i);
    IF cond THEN
      epop; c2(JLBC,i MOD 100h,i DIV 100h)
    ELSE    c2(JLB, i MOD 100h,i DIV 100h)
    END;
  ELSIF cond THEN epop; c1(JSBC,i) ELSE c1(JSB,i);
  END;
END JB;

PROCEDURE SetJLFC(): INTEGER;
BEGIN c2(JLFC,0,0); epop; RETURN cp-3 END SetJLFC;

PROCEDURE SetJLF(): INTEGER;
BEGIN c2(JLF,0,0); RETURN cp-3 END SetJLF;

PROCEDURE Jump(From, To: INTEGER);
  VAR i,cmd:INTEGER;
BEGIN
  ASSERT(To>=From+2); i:=To-(From+2);
  cmd:=ORD(COD[From]);
  ASSERT((cmd=JLF) OR (cmd=JLFC));
  IF cmd=JLF THEN  cmd:=JSF; ELSE cmd:=JSFC; END;
  IF i<=0FFh THEN
    COD[From]:=CHAR(cmd); COD[From+1]:=CHAR(i); COD[From+2]:=CHAR(NOP);
  ELSE DEC(i);
    COD[From+1]:=CHAR(i MOD 100h); COD[From+2]:=CHAR(i DIV 100h)
  END;
END Jump;

CONST move=0C0h; LXA =0EAh; drop=0B1h; trap=084h;

PROCEDURE Trap(no: INTEGER);
BEGIN li(no); c(trap); epop; END Trap;

PROCEDURE epush;
BEGIN
  INC(depth);
(*ShowInt("depth=",depth); WriteLn; *)
  IF depth>maxEstkDepth THEN depth:=0; Error(42); END;
END epush;

PROCEDURE epop;
BEGIN
  DEC(depth);
(*ShowInt("depth=",depth); WriteLn;*)
  IF depth<0 THEN Show("Empty stack;compiler");
    ASSERT(FALSE);
  END;
END epop;

CONST LIn=00h; LIB=10h; LID=11h; LIW=12h; LIN=13h; NEG=0A7h; ror=8Fh;

PROCEDURE li(n: INTEGER);
BEGIN
  IF n=INTEGER(NIL) THEN c(LIN)
  ELSIF n<0 THEN
    IF n=INTEGER({31}) THEN c(LIn+1); c(copt); c(ror);
    ELSIF -n>0FFFFh THEN c(LIW); b4(n)
    ELSE li(-n); c(NEG);
      RETURN (* RETURN to elimanate double -epush- *)
    END
  ELSIF n<=   0Fh THEN c (LIn+n)
  ELSIF n<=  0FFh THEN c1(LIB,n)
  ELSIF n<=0FFFFh THEN c2(LID,n MOD 100h,n DIV 100h)
  ELSE c(LIW); b4(n)
  END;
  epush;
END li;

CONST LLW=20h; LLWn=20h;  SLW=30h; SLWn=30h;
      LGW=21h; LGWn=40h;  SGW=31h; SGWn=50h;
      LSW=23h; LSWn=60h;
      LEW=22h; SEW =32h;  SSW=33h; SSWn=70h;

PROCEDURE slw(n: INTEGER);
BEGIN ASSERT((n>=5) & (n<=0FFh)); epop;
  IF n<=0Fh THEN c(SLWn+n) ELSE c1(SLW,n) END
END slw;

PROCEDURE sgw(n: INTEGER);
BEGIN ASSERT((n>=2) & (n<=0FFh)); epop;
  IF n<=0Fh THEN c(SGWn+n) ELSE c1(SGW,n) END
END sgw;

PROCEDURE ssw(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh)); epop; epop;
  IF n<=0Fh THEN c(SSWn+n) ELSE c1(SSW,n) END
END ssw;

PROCEDURE lgw(n: INTEGER);
BEGIN ASSERT((n>=2) & (n<=0FFh));
  IF n<=0Fh THEN c(LGWn+n) ELSE c1(LGW,n) END;
  epush;
END lgw;

PROCEDURE llw(n: INTEGER);
BEGIN ASSERT((n>=5) & (n<=0FFh));
  IF n<=0Fh THEN c(LLWn+n) ELSE c1(LLW,n) END;
  epush;
END llw;

PROCEDURE lsw(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  IF n<=0Fh THEN c(LSWn+n) ELSE c1(LSW,n) END
END lsw;

PROCEDURE sew(m,n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh)); ASSERT((m>=0) & (m<=0FFh));
  c2(SEW,m,n); epop;
END sew;

PROCEDURE lew(m,n: INTEGER);
BEGIN
  ASSERT((n>=0) & (n<=0FFh)); ASSERT((m>=0) & (m<=0FFh));
  c2(LEW,m,n); epush
END lew;

CONST LLAdr=14h; LGAdr=15h; LEAdr=17h;

PROCEDURE lla(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  c1(LLAdr,n);
  epush;
END lla;

PROCEDURE lga(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  c1(LGAdr,n);
  epush;
END lga;

PROCEDURE lea(m,n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh)); ASSERT((m>=0) & (m<=0FFh));
  c2(LEAdr,m,n);
  epush;
END lea;

PROCEDURE LoadVal(idno:INTEGER);
BEGIN
  I.name:=idno; Unpack(I);
  IF I.param THEN llw(I.offset); lsw(0);
  ELSE lgw(I.offset); END;
END LoadVal;

CONST LSA=16h;

PROCEDURE LoadAdr(idno:INTEGER);
BEGIN
  I.name:=idno; Unpack(I);
  IF I.param THEN llw(I.offset);
  ELSE lga(I.offset); END;
END LoadAdr;

PROCEDURE LoadLocVal(idno:INTEGER);
BEGIN
  I.name:=idno; Unpack(I);
  llw(I.offset);
END LoadLocVal;

CONST SXW=51h; SXB=50h;

PROCEDURE StoreInVar(idno:INTEGER);
BEGIN
  I.name:=idno; Unpack(I);
  IF I.param THEN ssw(0);
  ELSE sgw(I.offset); END;
END StoreInVar;

PROCEDURE MarkC;
BEGIN cpSave:=cp; END MarkC;

PROCEDURE BackC;
BEGIN cp:=cpSave; END BackC;

PROCEDURE MarkC1;
BEGIN cpSave1:=cp; END MarkC1;

PROCEDURE BackC1;
BEGIN cp:=cpSave1; END BackC1;

CONST ENTER=0C9h; STORE=0B3h; LODFV=0B2h;

PROCEDURE Getdepth():INTEGER;
BEGIN RETURN depth; END Getdepth;

PROCEDURE Setdepth(d:INTEGER);
BEGIN depth:=d; END Setdepth;

PROCEDURE Store;
BEGIN depthsave:=depth; c(STORE); depth:=0; END Store;

PROCEDURE Lodfv;
BEGIN c(LODFV); depth:=depthsave+1; END Lodfv;

CONST CLp=0D0h; CLoc=0CFh; CX=0CCh;

PROCEDURE CL(p:INTEGER);
BEGIN ASSERT((p>=0) & (p<=0FFh));
  IF p<=0Fh THEN c(CLp+p) ELSE c1(CLoc,p) END;
END CL;

PROCEDURE CallExt(m:INTEGER);
BEGIN ASSERT(m>0);
  c(CX); c(m); c(1);
END CallExt;

PROCEDURE InsertCode(bou,byte:INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=cp TO bou BY -1 DO COD[i]:=COD[i-1]; END;
  COD[bou]:=CHAR(byte);
  INC(cp); IF cp>HIGH(COD) THEN (* fatal error *); END;
END InsertCode;

PROCEDURE Enter(VAR s:INTEGER);
BEGIN c(ENTER); s:=cp; c(0); END Enter;

PROCEDURE PutCod(pos,cod:INTEGER);
BEGIN ASSERT(cod<=0FFh); COD[pos]:=CHAR(cod); END PutCod;

PROCEDURE FinishProc0;
BEGIN
  Alloc(10); sgw(2); (* разместили буфер для параметров-констант *)
  IF self=main THEN CL(1); END;
  li(MinPS+4); c(RTN);
  pTab[0]:=0;
END FinishProc0;

PROCEDURE StartProc1;
BEGIN depth:=0; pTab[1]:=cp; END StartProc1;

PROCEDURE StartFunc():INTEGER;
BEGIN depth:=0;
  pTab[procno]:=cp; INC(procno);
  c(STORE); RETURN (procno-1);
END StartFunc;

PROCEDURE error; BEGIN Fault(12) END error;

PROCEDURE WriteCodeFile;
  VAR a: Attrs; i,j: INTEGER;
      E:Idname; st:ARRAY [0..15] OF CHAR;
BEGIN
  WITH a DO
    CPU:=SysInfo.CPU;         Language:="f77+";
    DefTime:=0;               ImpTime:=SystemClock();
    ProcCo:=procno;           ExtsCo:=ext;
    MinProcStack:=MinPS;      OptimalStack:=MinPS;
    GlobalsSize:=global;      ConstsSize:=sp DIV 4;
    CodeSize:=cp DIV 4;
  END;
  Write(CR); ClearLine;
  print(" Код %s.cod %db\n",Name,a.CodeSize*4);
  CreateCode(Name,error); WriteInfo(a);
  WriteConsts(a,STP); WriteProcTable(a,pTab); WriteCode(a,COD);
  FOR i:=ext-1 TO 1 BY -1 DO
    IdStr(Ext[i],E); j:=0;
    REPEAT st[j]:=E[j]; INC(j); UNTIL E[j]=eol;
    st[j]:=eol; WriteExt(i,st);
  END; WriteExt(0,Name);
  CloseCode;
END WriteCodeFile;

CONST ALLOC=0C8h;

PROCEDURE Alloc(sz:INTEGER);
BEGIN li(sz); c(ALLOC); END Alloc;

PROCEDURE PutExt(idno:INTEGER):INTEGER;
BEGIN Ext[ext]:=idno; INC(ext); RETURN (ext-1); END PutExt;

PROCEDURE FinishCod;
BEGIN
  IF self=function THEN Trap(46h); (* Возврат из функции без значения *)
  ELSE c(RTN); END;
  WHILE (cp MOD 4)#0 DO c(NOP); END;
END FinishCod;

PROCEDURE InitGen(d:INTEGER);
  VAR i:INTEGER; I:Info;
BEGIN
  IF d<0 THEN (* main *) i:=0;
    WHILE codfile[i]#eol DO Name[i]:=codfile[i]; INC(i); END;
    Name[i]:=eol;
  ELSE IdStr(d,Name); END;
  FOR i:=0 TO 31 DO pTab[i]:=0; END;
  cp:=0; sp:=0; MinPS:=10; procno:=2; depth:=0; ext:=1;
  i:=StrId("StdIO"); SetMode(i,Subr); I.name:=i;
  I.parlist:=NIL; I.offset:=1; GenObj(I);
  i:=PutExt(i);
  REPEAT STP[sp]:=Name[sp]; INC(sp); UNTIL Name[sp]=eol;
  REPEAT STP[sp]:=eol;      INC(sp); UNTIL (sp MOD 4)=0;
END InitGen;

PROCEDURE SvalToPool(): INTEGER;
  VAR i,p: INTEGER;
BEGIN
  p:=sp DIV 4; i:=0;
  WHILE Sval[i]#eol DO STP[sp]:=Sval[i]; INC(sp); INC(i) END;
  STP[sp]:=eol; INC(sp);
  REPEAT STP[sp]:=eol; INC(sp) UNTIL (sp MOD 4)=0;
  RETURN p;
END SvalToPool;

BEGIN
END fGen.
