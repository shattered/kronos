IMPLEMENTATION MODULE fcProc; (* 03-Nov-88.  *)

FROM SYSTEM  IMPORT ADDRESS, ADR, WORD;
FROM fcScan  IMPORT Symbol, GetSy, sy, cType, Ival, Error, Expected,
                    KeyON;
FROM fcObj   IMPORT Idname, Types, Info, Pack, Unpack, Class,funcbit,
                    subrbit, IdStr, GetType;
FROM fcExpr  IMPORT Dexpr, Emode, Lexpr, aExpr, tExpr;
FROM fcStm   IMPORT Goto;
FROM fcGen   IMPORT cp, c, c1, cCall, llw, slw, lla , Getdepth, Setdepth,
                    CallExt, Store, Lodfv, lsta, InPoolConst, genLPC, li,
                    epop, epush, b2, sew, lew, lsw;
FROM fcInd   IMPORT LoadAdr, EvalAdr, LoadConst;
FROM fcTproc IMPORT AllocTemp, FreeTemp, AllocDesc, FreeDesc;
FROM fcDcl   IMPORT Mapoffs;
FROM fcDefs  IMPORT maxpar, maxEstkDepth, dplen;
FROM fcBugs  IMPORT AddStr ;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

MODULE Intrinsic;

IMPORT ADR, WORD, ADDRESS, MOVE, Idname, Types;
EXPORT InitIntr, LookIntr;

CONST NoIds = 211; NoIntr=85;

VAR Id:ARRAY [0..NoIds] OF ADDRESS;
    Ino:ARRAY [0..NoIds] OF CHAR;
    Ip: POINTER TO Idname;

VAR Col,Sum,Len,looks:INTEGER; C:CHAR;
    nameArray: ARRAY [0..NoIntr*2+4] OF WORD;
    iCh: INTEGER;

CONST Ilim=NoIds-30; eol=0c;

PROCEDURE Hash(VAR IdName:Idname);
BEGIN
  Sum:=0; Len:=0;
    C:=IdName[Len];
    WHILE C#eol DO
    Sum := INTEGER(BITSET(Sum)/BITSET(C<<Len));
    INC(Len);
    C:=IdName[Len];
    END; INC(Len);
  Sum := Sum MOD NoIds;
END Hash;

PROCEDURE Find(VAR IdName:Idname):INTEGER;
VAR R:INTEGER;
BEGIN
  Hash(IdName); R:=13;
  -- looks:=0;
  LOOP
   -- INC(looks);
    IF Id[Sum]=NIL THEN RETURN NoIds
    ELSE Ip:=Id[Sum];
      IF IdName=Ip^ THEN RETURN Sum END;
      Sum:=(Sum+R) MOD NoIds; R:=(R+Sum) MOD NoIds;
    END
  END
END Find;

PROCEDURE Give(VAR ad: ADDRESS; len: INTEGER);
BEGIN
  ad:=ADR(nameArray[iCh]);
  iCh:=iCh+2;
END Give;

PROCEDURE StrId(VAR IdName:Idname):INTEGER;
VAR R:INTEGER;
BEGIN (*$T-*)
  -- INC(looks);
  Hash(IdName); Len:=(Len+3) DIV 4; R:=13;
  LOOP
    IF Id[Sum]=NIL THEN
      Give(Id[Sum],Len);
      MOVE(Id[Sum],ADR(IdName),Len);
      -- INC(Ibusy);
      RETURN Sum;
    END;
    Ip:=Id[Sum];
    IF IdName=Ip^ THEN RETURN Sum; END;
    Sum := (Sum+R) MOD NoIds;  R := (R+Sum) MOD NoIds;
   -- INC(Col);
  END; (*$T+*)
END StrId;

PROCEDURE MakeIntr( nm:Idname; no:INTEGER);
VAR n:INTEGER;
BEGIN
  n:=StrId(nm); Ino[n]:=CHAR(no);
END MakeIntr;

PROCEDURE LookIntr(VAR nm:Idname):INTEGER;
VAR no,i:INTEGER;
BEGIN
  i:=Find(nm); IF i=NoIds THEN no:=0
               ELSE no:=INTEGER(Ino[i])
               END;
  RETURN no
END LookIntr;

PROCEDURE InitIntr;
VAR i:INTEGER;
BEGIN
  iCh:=0;
  FOR i:=0 TO HIGH(Id) DO Id[i]:=NIL  END;
  MakeIntr('INT',    1); MakeIntr('IFIX',   2);
  MakeIntr('IDINT',  3); MakeIntr('REAL',   4);
  MakeIntr('FLOAT',  5); MakeIntr('SNGL',   6);
  MakeIntr('DBLE',   7); MakeIntr('CMPLX',  8);
  MakeIntr('ICHAR',  9); MakeIntr('CHAR',  10);
  MakeIntr('AINT',  11); MakeIntr('DINT',  12);
  MakeIntr('ANINT', 13); MakeIntr('DNINT', 14);
  MakeIntr('NINT',  15); MakeIntr('IDNINT',16);
  MakeIntr('IABS',  17); MakeIntr('ABS',   18);
  MakeIntr('DABS',  19); MakeIntr('CABS',  20);
  MakeIntr('MOD',   21); MakeIntr('AMOD',  22);
  MakeIntr('DMOD',  23); MakeIntr('ISIGN', 24);
  MakeIntr('SIGN',  25); MakeIntr('DSIGN', 26);
  MakeIntr('IDIM',  27); MakeIntr('DIM',   28);
  MakeIntr('DDIM',  29); MakeIntr('DPROD', 30);
  MakeIntr('MAX',   31); MakeIntr('MAX0',  32);
  MakeIntr('AMAX1', 33); MakeIntr('DMAX1', 34);
  MakeIntr('AMAX0', 35); MakeIntr('MAX1',  36);
  MakeIntr('MIN',   37); MakeIntr('MIN0',  38);
  MakeIntr('AMIN1', 39); MakeIntr('DMIN1', 40);
  MakeIntr('AMIN0', 41); MakeIntr('MIN1',  42);
  MakeIntr('LEN',   43); MakeIntr('INDEX', 44);
  MakeIntr('LGE',   45); MakeIntr('LGT',   46);
  MakeIntr('LLE',   47); MakeIntr('LLT',   48);
  MakeIntr('AIMAG', 49); MakeIntr('CONJG', 50);
  MakeIntr('SQRT',  51); MakeIntr('DSQRT', 52);
  MakeIntr('CSQRT', 53); MakeIntr('EXP',   54);
  MakeIntr('DEXP',  55); MakeIntr('CEXP',  56);
  MakeIntr('LOG',   57); MakeIntr('ALOG',  58);
  MakeIntr('DLOG',  59); MakeIntr('CLOG',  60);
  MakeIntr('LOG10', 61); MakeIntr('ALOG10',62);
  MakeIntr('DLOG10',63); MakeIntr('SIN',   64);
  MakeIntr('DSIN',  65); MakeIntr('CSIN',  66);
  MakeIntr('COS',   67); MakeIntr('DCOS',  68);
  MakeIntr('CCOS',  69); MakeIntr('TAN',   70);
  MakeIntr('DTAN',  71); MakeIntr('ASIN',  72);
  MakeIntr('DASIN', 73); MakeIntr('ACOS',  74);
  MakeIntr('DACOS', 75); MakeIntr('ATAN',  76);
  MakeIntr('DATAN', 77); MakeIntr('ATAN2', 78);
  MakeIntr('DATAN2',79); MakeIntr('SINH',  80);
  MakeIntr('DSINH', 81); MakeIntr('COSH',  82);
  MakeIntr('DCOSH', 83); MakeIntr('TANH',  84);
  MakeIntr('DTANH', 85);
END InitIntr;

END Intrinsic;

TYPE arrob = ARRAY [0..20] OF INTEGER;

CONST FFCT=9Fh;  STOT=0E8h; CF=0CEh;  ENTRC=0BAh; DECS=0B0h;  ADD=88h;
      drop=0B1h; abs =0A6h; FABS=9Dh; FNEG=9Eh;   XIT=0BBh;   LXB=40h;
      GEQ= 0A3h; GTR= 0A2h; LEQ=0A1h; LSS=0A0h;   swap=0F0h;  SXB=50h;
      MoD= 0AFh;
VAR name:Idname;
    ParTemp: ARRAY [0..511] OF CHAR;
    ParTop : INTEGER;

CONST ForLib=2;      ChCmp=9;  ChTop=2;  ChBase=3;    ChIndex=3;
-- INT=  99; IFIX=  99; IDINT=99; ReAL= 99; FlOAT=99; SNGL=  99; DBLE=  99;
-- CMPLX=99; ICHAR= 99; ChAR= 99; AINT= 99; DINT= 99; ANINT= 99; DNINT= 99;
NINT= 11; IDNINT=99; IABS= 99; AbS=  99; DABS= 99; CABS=  26;
AMOD= 50; DMOD=  50; ISIGN=51; SIGN= 52; DSIGN=52; IDIM=  49; DIM=   48;
DDIM= 48; DPROD= 99; MaX=  99; MAX0= 43; AMAX1=44; DMAX1= 44; AMAX0= 99;
MAX1= 99; MiN=   99; MIN0= 45; AMIN1=46; DMIN1=46; AMIN0= 99; MIN1=  99;
LEN=  99; INDEX= 99;                                          AIMAG= 99;
CONJG=99; SQRT=  12; DSQRT=12; CSQRT=27; EXP=  13; DEXP=  13; CEXP=  29;
LOG=  99; ALOG=  14; DLOG= 14; CLOG= 28; LOG10=61; ALOG10=25; DLOG10=25;
SIN=  15; DSIN=  99; CSIN= 30; COS=  16; DCOS= 16; CCOS=  31; TAN=   21;
DTAN= 99; ASIN=  20; DASIN=20; ACOS= 19; DACOS=19; ATAN=  17; DATAN= 17;
ATAN2=18; DATAN2=18; SINH= 23; DSINH=23; COSH= 22; DCOSH= 22;
TANH= 24; DTANH= 24;

PROCEDURE loadfv(ds,mxs,n:INTEGER);
 VAR i: INTEGER;
BEGIN
  IF n=1 THEN Lodfv(ds,mxs,n);
  ELSE  AllocTemp(i,1); Lodfv(ds,mxs,i); FreeTemp(i,1);
  END;
END loadfv;

PROCEDURE tint(VAR par:Dexpr);
BEGIN
   IF par.tp=Int THEN

   ELSIF par.tp=Real THEN
         IF par.emd=cexpr THEN
            par.name:=TRUNC(REAL(par.name));
         ELSE c1(FFCT,1);
         END;
   ELSIF par.tp=Double THEN

   ELSIF par.tp=Complex THEN
         IF par.emd=cexpr THEN
           par.name:=TRUNC(REAL(par.name));
         ELSE c(drop); c1(FFCT,1); epop;
         END;
   ELSE Error(63);
   END; par.tp:=Int;
END tint;

PROCEDURE treal(VAR par:Dexpr);
BEGIN
  IF par.tp=Int THEN
         IF par.emd=cexpr THEN
            par.name:=INTEGER(FLOAT(par.name));
         ELSE c1(FFCT,0);
         END;
  ELSIF par.tp=Real THEN

  ELSIF par.tp=Double THEN

  ELSIF par.tp=Complex THEN
         IF par.emd#cexpr THEN c(drop); END;
  ELSE Error(63);
  END; par.tp:=Real;
END treal;

PROCEDURE tdouble(VAR par:Dexpr);
BEGIN
(*
  IF par.tp=Int THEN
     Error(64);
  ELSIF par.tp=Real THEN
     Error(64);
  ELSIF par.tp=Double THEN

  ELSIF par.tp=Complex THEN
     Error(64);
  ELSE Error(63);
  END;
*)
  treal(par);
  par.tp:=Double;
END tdouble;

PROCEDURE Intr17(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par:Dexpr; st:BOOLEAN;
BEGIN
  GetSy;
  st:=(Getdepth()#0);
  par.start:=cp; Lexpr(par);
  CASE noIntr OF
    1: (* INT,  1, Int..Complex, Int *)
       tint(par); ob:=par;
   |2: (* IFIX, 1, Real..Real, Int *)
       IF par.tp#Real THEN Error(63); par.tp:=Real; END;
       tint(par); ob:=par;
   |3: (* IDINT, 1, Double..Double, Int *)
       IF par.tp#Double THEN Error(63); par.tp:=Double; END;
       tint(par); ob:=par;
   |4: (* REAL, 1, Int..Complex,   Real *)
       treal(par); ob:=par;
   |5: (* FLOAT, 1, Int..Int, Real *)
       IF par.tp#Int THEN Error(63); par.tp:=Int; END;
       treal(par); ob:=par;
   |6: (* SNGL, 1, Double..Double, Real *)
       IF par.tp#Double THEN Error(63); par.tp:=Double; END;
       treal(par); ob:=par;
   |7: (* DBLE, 1, Int..Complex, Double *)
       tdouble(par); ob:=par;
   ELSE
  END;
END Intr17;

PROCEDURE Intr8(VAR ob:Dexpr; noIntr:INTEGER);
(* CMPLX, 1 or 2, Int..Complex, Complex *)
  VAR par1,par2:Dexpr;
BEGIN
  GetSy;
  ob.tp:=Complex;
  par1.start:=cp; aExpr(par1); treal(par1);
  IF par1.emd=cexpr THEN LoadConst(par1); END;
  IF sy=comma THEN
    GetSy; par2.start:=cp; aExpr(par2); treal(par2);
    IF par2.emd=cexpr THEN LoadConst(par2); END;
  ELSE
    li(0);
  END;
END Intr8;

PROCEDURE Intr920(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par:Dexpr; st:BOOLEAN;
      t, ds, mxs  : INTEGER;
BEGIN
  GetSy;
  st:=(Getdepth()#0);
  par.start:=cp;
  CASE noIntr OF
    9: (* ICHAR, 1, Char..Char, Int *)
       tExpr(par);
       IF par.emd=cexpr THEN LoadConst(par); END;
       ds:=INTEGER(par.wd); lsw(0); llw(ds+1); c(LXB); epop;
       IF par.emd=expr THEN llw(ds+1); sew(ForLib,ChTop); END;
       ob.tp:=Int; FreeDesc(ds);
   |10: (* CHAR, 1, Int..Int, Char *)
      AllocDesc(t); ob.wd:=t; ob.tp:=Char;
      lew(ForLib,ChBase); slw(t); lew(ForLib,ChTop); slw(t+1);
      li(1); slw(t+2);
      lew(ForLib,ChTop); li(1); c(ADD); epop;
      sew(ForLib,ChTop);
      llw(t); llw(t+1);
      Lexpr(par);
      IF par.tp#Int THEN Error(63); END;
      IF par.emd=cexpr THEN LoadConst(par); END;
      c(SXB); epop; epop; epop;
      lla(t);
   |11: (* AINT, 1, Real..Double *)
       Lexpr(par);
       IF par.tp=Real THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         c1(FFCT,1); c1(FFCT,0); ob.tp:=Real;
       ELSIF par.tp=Double THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         c1(FFCT,1); c1(FFCT,0);
         ob.tp:=Double;
       ELSE Error(63);
       END;
   |12: (* DINT, 1, Double..Double *)
       Lexpr(par);
       IF par.tp#Double THEN Error(63); END;
       IF par.emd=cexpr THEN LoadConst(par); END;
       c1(FFCT,1); c1(FFCT,0);
       ob.tp:=Double;
   |13: (* ANINT, 1, Real..Double *)
       IF st THEN Store(ds,mxs); END;
       Lexpr(par);
       IF par.tp=Real THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         CallExt(ForLib,NINT);  c1(FFCT,0);
         ob.tp:=Real;
       ELSIF par.tp=Double THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         CallExt(ForLib,NINT);  c1(FFCT,0);
         ob.tp:=Double;
       ELSE Error(63);
       END;
       IF st THEN
         IF ob.tp=Double THEN loadfv(ds,mxs,dplen);
         ELSE                 loadfv(ds,mxs,1);
         END;
       ELSE
         IF ob.tp=Double THEN Setdepth(dplen);
         ELSE                 Setdepth(1);
         END;
       END;
   |14: (* DNINT, 1, Double..Double *)
       IF st THEN Store(ds,mxs); END;
       Lexpr(par);
       IF par.tp#Double THEN Error(63); END;
       IF par.emd=cexpr THEN LoadConst(par); END;
       CallExt(ForLib,NINT);  c1(FFCT,0);
--       CallExt(ForLib,DNINT);
       ob.tp:=Double;
       IF st THEN loadfv(ds,mxs,dplen);
       ELSE       Setdepth(dplen);
       END;
   |15: (* NINT, 1, Real..Double, Int *)
       IF st THEN Store(ds,mxs); END;
       Lexpr(par);
       IF par.tp=Real THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         CallExt(ForLib,NINT);
         ob.tp:=Int;
       ELSIF par.tp=Double THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         CallExt(ForLib,NINT);
         ob.tp:=Int;
       ELSE Error(63);
       END;
       IF st THEN loadfv(ds,mxs,1);
       ELSE       Setdepth(1);
       END;
   |16: (* IDNINT, 1, Double..Double, Int *)
       IF st THEN Store(ds,mxs); END;
       Lexpr(par);
       IF par.tp=Double THEN
         IF par.emd=cexpr THEN LoadConst(par); END;
         CallExt(ForLib,NINT);
         ob.tp:=Int;
       ELSE Error(63);
       END;
       IF st THEN  loadfv(ds,mxs,1);
       ELSE        Setdepth(1);
       END;
   |17: (* IABS, 1, Int..Int *)
       Lexpr(par);
       IF par.tp#Int THEN Error(63); par.tp:=Int; END;
       IF par.emd=cexpr THEN
         ob.emd:=cexpr; ob.name:=ABS(par.name);
       ELSE c(abs);
       END; ob.tp:=Int;
   |18: (* ABS,  1, Int..Complex *)
       Lexpr(par);
       IF par.tp=Int THEN
         IF par.emd=cexpr THEN
           ob.emd:=cexpr; ob.name:=ABS(par.name);
         ELSE c(abs);
         END; ob.tp:=Int;
       ELSIF par.tp=Real THEN
         IF par.emd=cexpr THEN
           ob.emd:=cexpr; ob.name:=INTEGER(ABS(REAL(par.name)));
         ELSE c(FABS);
         END; ob.tp:=Real;
       ELSIF par.tp=Double THEN
         IF par.emd=cexpr THEN
           ob.emd:=cexpr; ob.name:=INTEGER(ABS(REAL(par.name)));
         ELSE c(FABS);
         END; ob.tp:=Double;
       ELSIF par.tp=Complex THEN
         IF st THEN
           IF par.emd=cexpr THEN
             Store(ds,mxs);
             LoadConst(par);
           ELSE
             AllocTemp(t,2); slw(t+1); slw(t); epop;epop;
             Store(ds,mxs);
             llw(t); llw(t+1); FreeTemp(t,2);
           END;
         ELSIF par.emd=cexpr THEN LoadConst(par);
         END;
         CallExt(ForLib,CABS);
         ob.tp:=Real;
         IF st THEN loadfv(ds,mxs,1);
         ELSE      Setdepth(1);
         END;
       ELSE Error(63);
       END;
   |19: (* DABS, 1, Double..Double *)
        Lexpr(par);
       IF par.tp#Double THEN Error(63); END;
       IF par.emd=cexpr THEN
         ob.emd:=cexpr; ob.name:=INTEGER(ABS(REAL(par.name)));
       ELSE c(FABS);
       END; ob.tp:=Double;
   |20: (* CABS, 1, Complex..Complex,   Real *)
       IF st THEN Store(ds,mxs); END;
       Lexpr(par);
       IF par.tp#Complex THEN Error(63); par.tp:=Complex; END;
       IF par.emd=cexpr THEN LoadConst(par); END;
       CallExt(ForLib,CABS);
       ob.tp:=Real;
       IF st THEN loadfv(ds,mxs,1);
       ELSE      Setdepth(1);
       END;
   ELSE
  END;
END Intr920;

PROCEDURE Intr2130(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par     :Dexpr;
      st      :BOOLEAN;
      t,ds,mxs:INTEGER;
      tpa     : Types;
      proc    :INTEGER;
BEGIN
  GetSy; st:=(Getdepth()#0);
  par.start:=cp;
  IF noIntr=21 THEN
    (* MoD,  2, Int..Double *)
    aExpr(par); IF sy#comma THEN Expected(comma); END;
    IF par.emd=cexpr THEN LoadConst(par); END;
    GetSy; par.start:=cp;
    IF par.tp=Int THEN
       ob.tp:=Int; aExpr(par); IF par.tp#Int THEN Error(63); END;
       IF par.emd=cexpr THEN LoadConst(par); END;
       c(MoD); epop;
       RETURN;
    ELSE
      IF st THEN
        AllocTemp(t,2);
        IF par.tp=Double THEN
 --       slw(t+1); slw(t); epop; epop;
 --       Store(ds,mxs); llw(t); llw(t+1);
        ELSE
          slw(t); epop;
          Store(ds,mxs); llw(t);
        END;
        FreeTemp(t,2);
      END;
      IF    par.tp=Real   THEN tpa:=Real;   ob.tp:=tpa; proc:=AMOD;
      ELSIF par.tp=Double THEN tpa:=Double; ob.tp:=tpa; proc:=DMOD;
      ELSE Error(63);
      END;
    END;
  ELSE
    IF st THEN Store(ds,mxs); END;
    aExpr(par); IF sy#comma THEN Expected(comma); END;
    IF par.emd=cexpr THEN LoadConst(par); END;
    GetSy; par.start:=cp;
    CASE noIntr OF
      22: (* AMOD, 2, Real..Real *)
        IF par.tp#Real THEN Error(63); END;
        tpa:=Real; ob.tp:=Real; proc:=AMOD;
     |23: (* DMOD, 2, Double..Double *)
        IF par.tp#Double THEN Error(63); END;
        tpa:=Double; ob.tp:=Double; proc:=DMOD;
     |24: (* ISIGN, 2, Int..Int *)
        IF par.tp#Int THEN Error(63); END;
        tpa:=Int; ob.tp:=Int; proc:=ISIGN;
     |25: (* SIGN, 2, Int..Double *)
        IF par.tp=Int THEN
          tpa:=Int; ob.tp:=Int; proc:=ISIGN;
        ELSIF par.tp=Real THEN
          tpa:=Real; ob.tp:=Real; proc:=SIGN;
        ELSIF par.tp=Double THEN
          tpa:=Double; ob.tp:=Double; proc:=DSIGN;
        ELSE  Error(63);
              tpa:=Int; ob.tp:=Int; proc:=ISIGN;
        END;
     |26: (* DSIGN, 2, Double..Double *)
        IF par.tp#Double THEN Error(63); END;
        tpa:=Double; ob.tp:=Double; proc:=DSIGN;
     |27: (* IDIM, 2, Int..Int *)
        IF par.tp#Int THEN Error(63); END;
        tpa:=Int; ob.tp:=Int; proc:=IDIM;
     |28: (* DIM,  2, Int..Double *)
        IF par.tp=Int THEN
          tpa:=Int; ob.tp:=Int; proc:=IDIM;
        ELSIF par.tp=Real THEN
          tpa:=Real; ob.tp:=Real; proc:=DIM;
        ELSIF par.tp=Double THEN
          tpa:=Double; ob.tp:=Double; proc:=DDIM;
        ELSE  Error(63);
              tpa:=Int; ob.tp:=Int; proc:=IDIM;
        END;
     |29: (* DDIM, 2, Double..Double *)
        IF par.tp#Double THEN Error(63); END;
        tpa:=Double; ob.tp:=Double; proc:=DDIM;
     |30: (* DPROD, 2, Real..Real, Double *)
        IF par.tp#Real THEN Error(63); END;
        tpa:=Real; ob.tp:=Double; proc:=DPROD;
     ELSE
    END;
  END;
  aExpr(par);
  IF par.emd=cexpr THEN LoadConst(par); END;
  IF par.tp#tpa THEN Error(63); END;
  CallExt(ForLib,proc);
  IF st THEN
    IF ob.tp=Double THEN loadfv(ds,mxs,dplen);
    ELSE                 loadfv(ds,mxs,1);
    END;
  ELSE
    IF ob.tp=Double THEN Setdepth(dplen);
    ELSE                 Setdepth(1);
    END;
  END;
END Intr2130;

PROCEDURE Intr3142(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par    :Dexpr;
      st     :BOOLEAN;
      ds,mxs :INTEGER;
      tpa    :Types;
      proc   :INTEGER;
BEGIN
  GetSy;
  st:=(Getdepth()#0);
  IF st THEN Store(ds,mxs); END;
  par.start:=cp; aExpr(par);
  IF sy#comma THEN Expected(comma); END;
  IF par.emd=cexpr THEN LoadConst(par); END;
  CASE noIntr OF
    31: (* MAX, >=2, Int..Double *)
        IF par.tp=Int THEN
          tpa:=Int; ob.tp:=Int; proc:=MAX0;
        ELSIF par.tp=Real THEN
          tpa:=Real; ob.tp:=Real; proc:=AMAX1;
        ELSIF par.tp=Double THEN
          tpa:=Double; ob.tp:=Double; proc:=DMAX1;
        ELSE  Error(63);
              tpa:=Int; ob.tp:=Int; proc:=MAX0;
        END;
   |32: (* MAX0, >=2, Int..Int *)
        IF par.tp#Int THEN Error(63); END;
        tpa:=Int; ob.tp:=Int; proc:=MAX0;
   |33: (* AMAX1, >=2, Real..Real *)
        IF par.tp#Real THEN Error(63); END;
        tpa:=Real; ob.tp:=Real; proc:=AMAX1;
   |34: (* DMAX1, >=2, Double..Double *)
        IF par.tp#Double THEN Error(63); END;
        tpa:=Double; ob.tp:=Double; proc:=DMAX1;
   |35: (* AMAX0, >=2, Int..Int,   Real *)
        IF par.tp#Int THEN Error(63); END;
        tpa:=Int; ob.tp:=Real; proc:=MAX0;
   |36: (* MAX1, >=2, Real..Real,  Int *)
        IF par.tp#Real THEN Error(63); END;
        tpa:=Real; ob.tp:=Int; proc:=AMAX1;
   |37: (* MIN,  >=2, Int..Double *)
        IF par.tp=Int THEN
          tpa:=Int; ob.tp:=Int; proc:=MIN0;
        ELSIF par.tp=Real THEN
          tpa:=Real; ob.tp:=Real; proc:=AMIN1;
        ELSIF par.tp=Double THEN
          tpa:=Double; ob.tp:=Double; proc:=DMIN1;
        ELSE  Error(63);
              tpa:=Int; ob.tp:=Int; proc:=MIN0;
        END;
   |38: (* MIN0, >=2, Int..Int *)
        IF par.tp#Int THEN Error(63); END;
        tpa:=Int; ob.tp:=Int; proc:=MIN0;
   |39: (* AMIN1, >=2, Real..Real *)
        IF par.tp#Real THEN Error(63); END;
        tpa:=Real; ob.tp:=Real; proc:=AMIN1;
   |40: (* DMIN1, >=2, Double..Double *)
        IF par.tp#Double THEN Error(63); END;
        tpa:=Double; ob.tp:=Double; proc:=DMIN1;
   |41: (* AMIN0, >=2, Int..Int,   Real *)
        IF par.tp#Int THEN Error(63); END;
        tpa:=Int; ob.tp:=Real; proc:=MIN0;
   |42: (* MIN1, >=2, Real..Real, Int *)
        IF par.tp#Real THEN Error(63); END;
        tpa:=Real; ob.tp:=Int; proc:=AMIN1;
   ELSE
  END;
  WHILE sy=comma  DO
     GetSy; par.start:=cp; aExpr(par);
     IF par.emd=cexpr THEN LoadConst(par); END;
     IF par.tp#tpa THEN Error(63); END;
     CallExt(ForLib,proc);
  END;
  IF ob.tp#tpa THEN
    IF ob.tp=Int THEN c1(FFCT,1);
    ELSE              c1(FFCT,0);
    END;
  END;
  IF st THEN
    IF ob.tp=Double THEN loadfv(ds,mxs,dplen);
    ELSE                 loadfv(ds,mxs,1);
    END;
  ELSE
    IF ob.tp=Double THEN Setdepth(dplen);
    ELSE                 Setdepth(1);
    END;
  END;
END Intr3142;

PROCEDURE Intr43(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par:Dexpr; st:BOOLEAN;
BEGIN
 (* LEN,  1, Char..Char, Int *)
  GetSy;
--  st:=(Getdepth()#0);
  par.start:=cp; tExpr(par);
  ob.tp:=Int;
  lsw(2); FreeDesc(par.wd);
END Intr43;

PROCEDURE Intr4448(VAR ob:Dexpr; noIntr:INTEGER);
  VAR L,R:Dexpr; st:BOOLEAN;
      ds,mxs:INTEGER;
BEGIN
  st:=(Getdepth()#0);
  IF st THEN  Store(ds,mxs); END;
  GetSy;
  L.start:=cp; tExpr(L);
  IF sy#comma THEN Error(63); RETURN END;
  GetSy;
  R.start:=cp; tExpr(R);
  IF noIntr=44 THEN
    (* INDEX, 2, Char..Char, Int *)
    ob.tp:=Int;
    CallExt(ForLib,ChIndex);
  ELSE
    ob.tp:=Logic;
    CallExt(ForLib,ChCmp);
    CASE noIntr OF
      45: (* LGE,  2, Char..Char, Logic *)
          c(GEQ);
     |46: (* LGT,  2, Char..Char, Logic *)
          c(GTR);
     |47: (* LLE,  2, Char..Char, Logic *)
         c(LEQ);
     |48: (* LLT,  2, Char..Char, Logic *)
         c(LSS);
     ELSE
    END;
  END;
  IF st THEN loadfv(ds,mxs,1);
  ELSE Setdepth(1);
  END;
  IF    L.emd=expr THEN llw(INTEGER(L.wd)+1); sew(ForLib,ChTop);
  ELSIF R.emd=expr THEN llw(INTEGER(R.wd)+1); sew(ForLib,ChTop);
  END; FreeDesc(L.wd); FreeDesc(R.wd);
END Intr4448;

PROCEDURE Intr4950(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par:Dexpr; st:BOOLEAN;
BEGIN
  GetSy;
  par.start:=cp; aExpr(par);
  IF par.tp#Complex THEN Error(63); END;
  IF par.emd=cexpr THEN LoadConst(par); END;
  CASE noIntr OF
    49: (* AIMAG, 1, Complex..Complex,  Real *)
       c(swap); c(drop); epop; ob.tp:=Real;
   |50: (* CONJG, 1, Complex..Complex *)
       c(FNEG); ob.tp:=Complex;
   ELSE
  END;
END Intr4950;

PROCEDURE Intr5177(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par:Dexpr; st:BOOLEAN;
      ds,mxs:INTEGER;
BEGIN
  GetSy;
  st:=(Getdepth()#0);
  IF st THEN  Store(ds,mxs); END;
  par.start:=cp;  aExpr(par);
  IF (par.tp=Real) THEN  ob.tp:=Real
  ELSIF (par.tp=Double)  THEN ob.tp:=Double
  ELSIF (par.tp=Complex) THEN ob.tp:=Complex
  ELSE Error(63); ob.tp:=Real;
  END;
  IF par.emd=cexpr THEN LoadConst(par) END;
  CASE noIntr OF
    51: (* SQRT, 1, Real..Complex  *)
       IF par.tp=Real THEN CallExt(ForLib,SQRT)
       ELSIF par.tp=Double THEN CallExt(ForLib,DSQRT)
       ELSE CallExt(ForLib,CSQRT)
       END;
   |52: (* DSQRT, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DSQRT) ELSE Error(63) END;
   |53: (* CSQRT, 1, Complex..Complex *)
       IF par.tp=Complex THEN CallExt(ForLib,CSQRT) ELSE Error(63) END;
   |54: (* EXP, 1, Real..Complex *)
       IF par.tp=Real THEN CallExt(ForLib,EXP)
       ELSIF par.tp=Double THEN CallExt(ForLib,DEXP)
       ELSE CallExt(ForLib,CEXP)
       END;
   |55: (* DEXP, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DEXP) ELSE Error(63) END;
   |56: (* CEXP, 1, Complex..Complex *)
       IF par.tp=Complex THEN CallExt(ForLib,CEXP) ELSE Error(63) END;
   |57: (* LOG,  1, Real..Complex *)
       IF par.tp=Real THEN CallExt(ForLib,ALOG)
       ELSIF par.tp=Double THEN CallExt(ForLib,DLOG)
       ELSE CallExt(ForLib,CLOG)
       END;
   |58: (* ALOG, 1, Real..Real *)
       IF par.tp=Real THEN CallExt(ForLib,ALOG) ELSE Error(63) END;
   |59: (* DLOG, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DLOG) ELSE Error(63) END;
   |60: (* CLOG, 1, Complex..Complex *)
       IF par.tp=Complex THEN CallExt(ForLib,CLOG) ELSE Error(63) END;
   |61: (* LOG10, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,ALOG10)
       ELSIF par.tp=Double THEN CallExt(ForLib,DLOG10)
       ELSE  Error(63);
       END;
   |62: (* ALOG10, 1, Real..Real *)
       IF par.tp=Real THEN CallExt(ForLib,ALOG10) ELSE Error(63) END;
   |63: (* DLOG10, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DLOG10) ELSE Error(63) END;
   |64: (* SIN, 1, Real..Complex *)
       IF par.tp=Real THEN CallExt(ForLib,SIN)
       ELSIF par.tp=Double THEN CallExt(ForLib,DSIN)
       ELSE CallExt(ForLib,CSIN)
       END;
   |65: (* DSIN, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DSIN) ELSE Error(63) END;
   |66: (* CSIN, 1, Complex..Complex *)
       IF par.tp=Complex THEN CallExt(ForLib,CSIN) ELSE Error(63) END;
   |67: (* COS,  1, Real..Complex *)
       IF par.tp=Real THEN CallExt(ForLib,COS)
       ELSIF par.tp=Double THEN CallExt(ForLib,DCOS)
       ELSE CallExt(ForLib,CCOS)
       END;
   |68: (* DCOS, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DCOS) ELSE Error(63) END;
   |69: (* CCOS, 1, Complex..Complex *)
       IF par.tp=Complex THEN CallExt(ForLib,CCOS) ELSE Error(63) END;
   |70: (* TAN,  1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,TAN)
       ELSIF par.tp=Double THEN CallExt(ForLib,DTAN)
       ELSE  Error(63);
       END;
   |71: (* DTAN, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DTAN) ELSE Error(63) END;
   |72: (* ASIN, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,ASIN)
       ELSIF par.tp=Double THEN CallExt(ForLib,DSIN)
       ELSE  Error(63);
       END;
   |73: (* DASIN, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DASIN) ELSE Error(63) END;
   |74: (* ACOS, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,ACOS)
       ELSIF par.tp=Double THEN CallExt(ForLib,DACOS)
       ELSE  Error(63);
       END;
   |75: (* DACOS, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DACOS) ELSE Error(63) END;
   |76: (* ATAN, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,ATAN)
       ELSIF par.tp=Double THEN CallExt(ForLib,DATAN)
       ELSE  Error(63);
       END;
   |77: (* DATAN, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DATAN) ELSE Error(63) END;
   ELSE
  END;
  IF st THEN
    IF ob.tp=Real THEN
        loadfv(ds,mxs,1);
    ELSIF ob.tp=Double THEN
        loadfv(ds,mxs,dplen);
    ELSE
        loadfv(ds,mxs,2);
    END;
  ELSIF ob.tp=Real THEN Setdepth(1);
  ELSIF ob.tp=Double THEN Setdepth(dplen);
  ELSE Setdepth(2);
  END;
END Intr5177;

PROCEDURE Intr7879(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par     :Dexpr;
      st      :BOOLEAN;
      t,ds,mxs:INTEGER;
      tpa     : Types;
      proc    :INTEGER;
BEGIN
  GetSy; st:=(Getdepth()#0);
  par.start:=cp;
  IF st THEN Store(ds,mxs); END;
  aExpr(par); IF sy#comma THEN Expected(comma); END;
  IF par.emd=cexpr THEN LoadConst(par); END;
  GetSy; par.start:=cp;
  CASE noIntr OF
    78: (* ATAN2, 2, Real..Double *)
      IF par.tp=Real THEN
        tpa:=Real; ob.tp:=Real; proc:=ATAN2;
      ELSIF par.tp=Double THEN
        tpa:=Double; ob.tp:=Double; proc:=DATAN2;
      ELSE  Error(63);
        tpa:=Real; ob.tp:=Real; proc:=ATAN2;
      END;
   |79: (* DATAN2, 2,Double..Double *)
      IF par.tp#Double THEN Error(63); END;
      tpa:=Double; ob.tp:=Double; proc:=DATAN2;
  ELSE
  END;
  aExpr(par);
  IF par.emd=cexpr THEN LoadConst(par); END;
  IF par.tp#tpa THEN Error(63); END;
  CallExt(ForLib,proc);
  IF st THEN
    IF ob.tp=Double THEN loadfv(ds,mxs,dplen);
    ELSE                 loadfv(ds,mxs,1);
    END;
  ELSE
    IF ob.tp=Double THEN Setdepth(dplen);
    ELSE                 Setdepth(1);
    END;
  END;
END Intr7879;

PROCEDURE Intr8085(VAR ob:Dexpr; noIntr:INTEGER);
  VAR par:Dexpr; st:BOOLEAN;
      ds,mxs:INTEGER;
BEGIN
  GetSy;
  st:=(Getdepth()#0); IF st THEN Store(ds,mxs) END;
  par.start:=cp; aExpr(par);
  IF par.emd=cexpr THEN LoadConst(par) END;
  CASE noIntr OF
    80: (* SINH, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,SINH); ob.tp:=Real;
       ELSIF par.tp=Double THEN CallExt(ForLib,DSINH); ob.tp:=Double;
       ELSE  Error(63); ob.tp:=Real;
       END;
   |81: (* DSINH, 1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DSINH); ob.tp:=Double;
       ELSE  Error(63); ob.tp:=Real;
       END;
   |82: (* COSH, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,COSH); ob.tp:=Real;
       ELSIF par.tp=Double THEN CallExt(ForLib,DCOSH); ob.tp:=Double;
       ELSE  Error(63); ob.tp:=Real;
       END;
   |83: (* DCOSH,1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DCOSH); ob.tp:=Double;
       ELSE  Error(63); ob.tp:=Real;
       END;
   |84: (* TANH, 1, Real..Double *)
       IF par.tp=Real THEN CallExt(ForLib,TANH); ob.tp:=Real;
       ELSIF par.tp=Double THEN CallExt(ForLib,DTANH); ob.tp:=Double;
       ELSE  Error(63); ob.tp:=Real;
       END;
   |85: (* DTANH,1, Double..Double *)
       IF par.tp=Double THEN CallExt(ForLib,DTANH); ob.tp:=Double;
       ELSE  Error(63); ob.tp:=Real;
       END;
  ELSE
  END;
  IF st THEN
    IF ob.tp=Real THEN
        loadfv(ds,mxs,1);
    ELSIF ob.tp=Double THEN
        loadfv(ds,mxs,dplen);
    ELSE
        loadfv(ds,mxs,2);
    END;
  ELSIF ob.tp=Real THEN Setdepth(1);
  ELSIF ob.tp=Double THEN Setdepth(dplen);
  ELSE Setdepth(2);
  END;
END Intr8085;

PROCEDURE CallIntr(VAR ob:Dexpr; noIntr:INTEGER);
BEGIN
  IF    noIntr < 8  THEN Intr17  (ob,noIntr)
  ELSIF noIntr = 8  THEN Intr8   (ob,noIntr)
  ELSIF noIntr < 21 THEN Intr920 (ob,noIntr)
  ELSIF noIntr < 31 THEN Intr2130(ob,noIntr)
  ELSIF noIntr < 43 THEN Intr3142(ob,noIntr)
  ELSIF noIntr = 43 THEN Intr43  (ob,noIntr)
  ELSIF noIntr < 49 THEN Intr4448(ob,noIntr)
  ELSIF noIntr < 51 THEN Intr4950(ob,noIntr)
  ELSIF noIntr < 78 THEN Intr5177(ob,noIntr)
  ELSIF noIntr < 80 THEN Intr7879(ob,noIntr)
  ELSE                   Intr8085(ob,noIntr)
  END;
  IF sy#rpar THEN Expected(rpar); END; GetSy;
END CallIntr;

PROCEDURE AddPtemp(tag,info:INTEGER);
BEGIN
  ParTemp[ParTop]:=CHAR(tag);  INC(ParTop);
  ParTemp[ParTop]:=CHAR(info); INC(ParTop);
END AddPtemp;

PROCEDURE FreeTempPar(top:INTEGER);
  VAR i,tag,info: INTEGER;
BEGIN
  i:=top;
  WHILE i<ParTop DO
    tag:=INTEGER(ParTemp[i]); INC(i);
    CASE tag OF
        1:  -- temp
           info:=INTEGER(ParTemp[i]); INC(i);
           FreeTemp(info,1);
       |2:  -- temp
           info:=INTEGER(ParTemp[i]); INC(i);
           FreeTemp(info,2);
       |3:  -- descriptor substring
           info:=INTEGER(ParTemp[i]); INC(i);
           FreeDesc(info);
       |4:  -- descriptor char expr.
           info:=INTEGER(ParTemp[i]); INC(i);
           -- free char stack --
           llw(info+1); sew(ForLib,ChTop);
           FreeDesc(info);
    ELSE
    END;
  END;
  ParTop:=top;
END FreeTempPar;

PROCEDURE LoadParam(VAR ob:Dexpr);
  VAR I:Info; tpar:INTEGER;
BEGIN
  CASE ob.emd OF
    expr,funcall:
              IF (ob.tp=Int) OR (ob.tp=Real) OR (ob.tp=Logic) THEN
                 AllocTemp(tpar,1); slw(tpar); lla(tpar);
                 AddPtemp(1,tpar);
              ELSIF ob.tp=Double  THEN
                 AllocTemp(tpar,1); slw(tpar); lla(tpar);
                 AddPtemp(1,tpar);
(*
                AllocTemp(tpar,2); slw(tpar+1); slw(tpar); lla(tpar);
                AddPtemp(2,tpar);
*)
              ELSIF ob.tp=Complex THEN
                AllocTemp(tpar,2); slw(tpar+1); slw(tpar); lla(tpar);
                AddPtemp(2,tpar);
              ELSIF ob.tp=Char    THEN
                AddPtemp(4,ob.wd);
              END;
   |cexpr   :
              IF (ob.tp=Int) OR (ob.tp=Real) OR (ob.tp=Logic) THEN
                 lsta(InPoolConst(ob.name));  -- load address const
              ELSIF ob.tp=Double  THEN
                 lsta(InPoolConst(ob.name));  -- load address const
--                 lsta(InPoolConst(ob.name));  -- load address const
--                 tpar:=InPoolConst(ob.wd);
              ELSIF ob.tp=Complex THEN
                 lsta(InPoolConst(ob.name));  -- load address const
                 tpar:=InPoolConst(ob.wd);
              ELSIF ob.tp=Char    THEN
                 LoadConst(ob);    AddPtemp(3,ob.wd);
              ELSIF ob.tp=Holl    THEN
                 LoadConst(ob);
              END;
   |var,arrel :IF ob.tp#Char THEN  EvalAdr(ob);
               ELSE AddPtemp(3,ob.wd);
               END;
   |substr  :   AddPtemp(3,ob.wd);
   |arr     : IF ob.tp=Char THEN
                AddPtemp(3,ob.wd);
              ELSE
              END;
   |ext     : I.name:=ob.name; Unpack(I);
              IF I.cl=Param THEN
                LoadAdr(I); -- ?? --
              ELSE
                 IdStr(I.name,name); genLPC(name);
              END;
   ELSE
  END; (* case *)
END LoadParam;

PROCEDURE genCall(VAR I:Info;nopar,noaltr:INTEGER);
VAR t:INTEGER;
BEGIN
  IF I.cl=Param THEN
    IF Getdepth()=maxEstkDepth THEN
      AllocTemp(t,1); slw(t);
      LoadAdr(I); c(STOT); epop;
      llw(t); FreeTemp(t,1);
    ELSE
      LoadAdr(I); c(STOT); epop;
    END;
    c(CF);
  ELSE
    IdStr(I.name,name);
    cCall(name,nopar,noaltr);
  END;
END genCall;

PROCEDURE CallLocal(VAR ob:Dexpr;VAR par:arrob; nopar:INTEGER);
BEGIN
END CallLocal;

PROCEDURE ParamList(VAR arr:arrob;VAR noaltr:INTEGER):INTEGER;
VAR ob:Dexpr; parno:INTEGER;
BEGIN
  parno:=0; noaltr:=0; GetSy;
  IF sy=rpar THEN RETURN parno END;
  LOOP
    IF sy=times THEN
      ob.emd:=star; GetSy;
      IF (sy#const) OR (cType#Int) OR (Ival<=0) OR (Ival>99999) THEN
         Error(4); ob.emd:=invmd;
      END;
      ob.tp:=Int; ob.name:=Ival;
      arr[noaltr]:=Ival;
      INC(noaltr);
      GetSy;
    ELSE
      ob.start:=cp;  Lexpr(ob);
      LoadParam(ob); INC(parno);
    END;
    -- IF parno > maxpar THEN Error(45); DEC(parno) END;
    IF sy#comma THEN EXIT
    ELSE GetSy
    END
  END;
  IF sy#rpar THEN Expected(rpar) END;
  RETURN parno
END ParamList;

PROCEDURE SubrCall(VAR I:Info);
  VAR arrpar:arrob; partop,nopar,noaltr:INTEGER;
      ob:Dexpr; offs, i:INTEGER;
      name: Idname;
BEGIN
  GetSy;
  IF funcbit IN I.bits THEN Error(48) END;
  INCL(I.bits,subrbit); Pack(I);
  IF sy=lpar THEN
    partop:=ParTop;
    nopar:=ParamList(arrpar,noaltr);
    genCall(I,nopar,noaltr);
    IF noaltr>0 THEN
      offs:=5*noaltr+1;
      c(ENTRC); b2(offs);
      FOR i:=0 TO noaltr-1 DO
        li(1); c(DECS); Goto(FALSE,TRUE,arrpar[i]);
      END;
      c(XIT);
      b2(1); b2(noaltr); b2(7); INC(offs,8);
      FOR i:=1 TO noaltr DO  b2(offs); DEC(offs,3); END;
    END;
    FreeTempPar(partop);
  ELSE
    genCall(I,0,0);
  END;
END SubrCall;

PROCEDURE FuncCall(VAR ob:Dexpr);
  VAR st:BOOLEAN;
      I:Info; arrpar:arrob; partop,nopar,noaltr:INTEGER;
      D,ds,maxs:INTEGER; tp:Types;
BEGIN
  I.name:=ob.name; Unpack(I);
  ob.start:=cp; ob.emd:=expr;
  IF I.cl=Intr  THEN  CallIntr (ob,I.offset); RETURN END;
  st:=(Getdepth()#0); IF st THEN  Store(ds,maxs); END;
  tp:=GetType(I.name);
    IF tp=Char THEN
      AllocDesc(D); ob.wd:=D;
      lew(ForLib,ChBase); slw(D); lew(ForLib,ChTop); slw(D+1);
      li(I.lenel); slw(D+2);
      lew(ForLib,ChTop); li(I.lenel); c(ADD); epop;
      sew(ForLib,ChTop);
    END;
  partop:=ParTop;
  nopar:=ParamList(arrpar,noaltr);
  IF noaltr#0 THEN Error(58) END;
  IF I.cl=Local THEN  CallLocal(ob,arrpar,nopar);
  ELSE
    IF subrbit IN I.bits THEN Error(48) END;
    INCL(I.bits,funcbit); Pack(I);
    IF tp=Char THEN
      IF Getdepth()=maxEstkDepth THEN  Store(ds,maxs); Error(45); END;
      lla(D);
    END;
    genCall(I,nopar,0);
  END;
  FreeTempPar(partop);
  IF st THEN
    IF tp=Complex THEN
        loadfv(ds,maxs,2);
    ELSIF tp=Double THEN
        loadfv(ds,maxs,dplen);
    ELSE
        loadfv(ds,maxs,1);
    END;
  ELSIF tp=Complex THEN Setdepth(2);
  ELSIF tp=Double THEN Setdepth(dplen);
  ELSE Setdepth(1);
  END;
  GetSy;
END FuncCall;

BEGIN
  InitIntr;
  ParTop:=0;
END fcProc.
