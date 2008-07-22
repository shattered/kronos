IMPLEMENTATION MODULE fcInOut;

FROM SYSTEM  IMPORT ADDRESS,WORD;
FROM fcScan  IMPORT Formsym,fsym,Getfsym,sy,GetSy,Symbol,Fault,
                    Error,Expected,cType,Ival,Psymbol,GetPosKW,
                    lookAhead, Label, GetHoll, Sval, Slen;
FROM fcObj   IMPORT Types,Mode,Class,GetType,SetType,Info,Unpack,
                    GetMode, GenLocal, GenObj;
FROM fcExpr  IMPORT Dexpr,Emode,Lexpr,lExpr,tExpr,iExpr,aExpr,
                    Expr;
FROM fcGen   IMPORT cp, c, c1, li, llw, slw, lla, CallExt, lew, ssw,lsw,
                    cJump,cLabel, InsertMode, InsertEnd, moveCode,
                    STP,stpPos, Setdepth, epop, epush;
FROM fcInd   IMPORT gDovarA, gDovarV, StoreInVar, LoadConst,
                    EvalAdr;
FROM fcStm   IMPORT GetFmt, SetFmt, Goto, AssTypeConv, gLoclab;
FROM fcTproc IMPORT AllocTemp, FreeTemp, FreeDesc, FreeDescs;
FROM fcDcl   IMPORT AllocVar, Mapoffs;
FROM objFile IMPORT pLabel;
-- FROM StdIO   IMPORT print;

TYPE Bits = SET OF Psymbol;
VAR ssym: Bits;
    obunit, obfmt, obrec, obiostat, ob: Dexpr;
    errlab,endlab:INTEGER;

CONST  ForIO=1;    ForOpen=1;    ForClose=2;  ForRewind=3;
     ForBackspace=4;             EndFile =5;
     InputIV  =6;  InputIA =7;   InputRA  =8;  InputRV  =9;
     InputLV =0Ah; InputLA=0Bh;  InputCV=0Ch;  InputCA =0Dh;
     OutputIV=0Eh; OutputIA=0Fh; OutputRA=10h; OutputRV=11h;
     OutputLA=12h; OutputLV=13h; OutputCV=14h; OutputCA=15h;
     ReadSF  =16h; ReadSFR =17h; ReadSU=18h;   ReadSL=19h;
     ReadDF=1Ah;   ReadDFR =1Bh; ReadDU=1Ch;   ReadDL=1Dh;
     ReadIF=1Eh;
     WriteSU =1Fh; WriteSF=20h;  WriteSFR=21h; WriteSL=22h;
     WriteDFR=23h; WriteDU=24h;  WriteDL=25h;
     WriteIF=26h;
     IOend=27h; IOerror=28h;
     WriteDF=29h;
     OutputDV=11h; OutputDA=10h; InputDV=9; InputDA=8;

CONST FFCT=9Fh;  RTN=0CAh;  JSFC=1Ah;  LEQ=0A1h; JSF=1Bh; ADD=88h;
      copt=0B5h; LSS=0A0h;  Not=0AEh;  LSA=16h;  GEQ=0A3h;
      SXW=51h;   LXW=41h;   LSTA=0C2h; CX=0CCh;  Drop=0B1h;FCMP=9Ch;
      ALLOC=0C8h;FADD=98h;  FSUB=99h;  STOT=0E8h;FDIV=9Bh; DECS=0B0h;
      GRT=0A2h;  Div=8Bh;   INC1=0E4h; DEC1=0E5h;Inc=0E6h; SUB=89h;

PROCEDURE GetLabel():BOOLEAN;
BEGIN
  GetSy; IF sy#const THEN Error(4);RETURN FALSE END;
  IF (cType#Int) OR (Ival<1) OR (Ival>99999) THEN
     Error(4); RETURN FALSE
  ELSE RETURN TRUE
  END
END GetLabel;

PROCEDURE idChanel(VAR ob:Dexpr):BOOLEAN;
(* TRUE if error *)
BEGIN
  INCL(ssym,unit);
  IF sy=times THEN
    ob.emd:=star; GetSy;
    li(-1);
  ELSE  ob.start:=cp;
    Lexpr(ob);
    IF ob.emd=invmd THEN RETURN TRUE END;
    IF ob.tp=Int THEN
      IF ORD(ob.emd)>ORD(arrel) THEN Error(14); RETURN TRUE END;
      IF ob.emd=cexpr THEN li(ob.name) END;
    ELSIF ob.tp=Char THEN
      IF (ORD(ob.emd)<ORD(var)) OR (ob.emd=funcall)
         OR (ORD(ob.emd)>ORD(arr)) THEN
            Error(53); RETURN TRUE
      END;
    ELSE Error(27); RETURN TRUE
    END;
  END;
  RETURN FALSE
END idChanel;

PROCEDURE idFormat(VAR ob:Dexpr):BOOLEAN;
(* TRUE if error  *)
BEGIN
  INCL(ssym,fmt);
  IF sy=times THEN ob.emd:=star; GetSy;
  ELSIF (sy=const) AND (cType#Char) THEN
    ob.emd:=cexpr; ob.name:=Ival; ob.tp:=Int;
    IF (cType#Int) OR (Ival<0) OR (Ival>99999) THEN
      Error(4); GetSy; RETURN TRUE
    END;
    GetSy;
  ELSE
    ob.start:=cp; Lexpr(ob);
    IF ob.emd=invmd THEN RETURN TRUE END;
    IF ob.tp=Int THEN
      IF ob.emd#var THEN Error(14); RETURN TRUE END;
      cp:=ob.start;
    ELSIF ob.tp=Char THEN
      IF ORD(ob.emd)>ORD(arr) THEN
        Error(15); RETURN TRUE
      END;
    ELSE Error(27); RETURN TRUE
    END;
  END;
  RETURN FALSE
END idFormat;

PROCEDURE Kunit;
BEGIN
  IF unit IN ssym THEN Error(47); INCL(ssym,invPkw) END;
  GetSy;
  IF (fmt IN ssym) AND (obfmt.tp=Char) THEN
     InsertMode(obfmt.start);
     IF idChanel(obunit) THEN INCL(ssym,invPkw); END;
     InsertEnd;
 ELSE
   IF idChanel(obunit) THEN INCL(ssym,invPkw); END;
 END;
END Kunit;

PROCEDURE Kfmt;
BEGIN
  IF fmt IN ssym THEN Error(47); INCL(ssym,invPkw); END;
  GetSy;
  IF idFormat(obfmt) THEN INCL(ssym,invPkw); END;
END Kfmt;

PROCEDURE Krec;
BEGIN
  IF rec IN ssym THEN Error(47); INCL(ssym,invPkw); END;
  INCL(ssym,rec); GetSy; iExpr(obrec);
  IF obrec.emd=invmd THEN INCL(ssym,invPkw);
  ELSIF ob.emd#cexpr THEN
    AllocTemp(obrec.name,1); slw(obrec.name);
  END;
END Krec;

PROCEDURE Kiostat;
BEGIN
  IF iostat IN ssym THEN Error(47); INCL(ssym,invPkw); END;
  INCL(ssym,iostat); GetSy; iExpr(obiostat);
  IF (obiostat.emd#var) AND (obiostat.emd#arrel) THEN
    Error(53); INCL(ssym,invPkw);
  ELSE
    EvalAdr(obiostat);
    AllocTemp(obiostat.name,1); slw(obiostat.name);
  END;
END Kiostat;

PROCEDURE Kerr;
BEGIN
  IF err IN ssym THEN Error(47); INCL(ssym,invPkw); END;
  INCL(ssym,err);
  IF GetLabel() THEN errlab:=Ival;
  ELSE INCL(ssym,invPkw);
  END; GetSy;
END Kerr;

PROCEDURE Kend;
BEGIN
  IF lend IN ssym THEN Error(47); INCL(ssym,invPkw); END;
  INCL(ssym,lend);
  IF GetLabel() THEN endlab:=Ival;
  ELSE INCL(ssym,invPkw);
  END; GetSy;
END Kend;

PROCEDURE CInfoList():BOOLEAN;
VAR pkw:Psymbol; i:INTEGER; posch,ret:BOOLEAN;
BEGIN
  i:=0; posch:=FALSE; ret:=FALSE;
  LOOP
    GetPosKW(pkw);
    CASE pkw OF
      invPkw: IF i=0 THEN posch:=TRUE;
                GetSy; IF idChanel(obunit) THEN ret:=TRUE END;
              ELSIF (i=1) AND posch THEN
                GetSy;
                IF idFormat(obfmt) THEN ret:=TRUE
                END;
              ELSE Error(41); RETURN TRUE;
              END;
     |unit:   Kunit;
     |fmt :   Kfmt;
     |rec :   Krec;
     |iostat: Kiostat;
     |err :   Kerr;
     |lend:   Kend;
     |Rpar :                    EXIT
     ELSE    Error(46); GetSy; INCL(ssym,invPkw);
    END; -- case
    INC(i);
    IF sy#comma THEN EXIT; END;
  END; -- loop
  IF NOT(unit IN ssym) THEN Error(40); INCL(ssym,invPkw) END;
  RETURN invPkw IN ssym;
END CInfoList;

VAR idolevel:INTEGER;
VAR tdovar:ARRAY [0..20] OF INTEGER;
VAR read:BOOLEAN;

PROCEDURE IOexpr(VAR ob:Dexpr);
VAR t: INTEGER;
BEGIN
  -- emd= expr or cexpr or funcall
  IF read THEN Error(53)
  ELSE
    IF ob.emd=cexpr THEN
      LoadConst(ob);
    END;
    CASE ob.tp OF
      Int:      CallExt(ForIO,OutputIV);
     |Real:     CallExt(ForIO,OutputRV);
     |Double:   CallExt(ForIO,OutputDV);
     |Complex:  AllocTemp(t,1); slw(t);   -- val val
                CallExt(ForIO,OutputRV);
                llw(t);
                CallExt(ForIO,OutputRV);
                FreeTemp(t,1);
     |Logic:    CallExt(ForIO,OutputLV);
     |Char:     CallExt(ForIO,OutputCV);
                IF ob.emd=expr THEN
                -- free chstack
                END;
                FreeDesc(INTEGER(ob.wd));
     ELSE
    END;
  END;
END IOexpr;

PROCEDURE IOvar(VAR ob:Dexpr);
  VAR I:Info;
BEGIN
  -- emd = var or arrel or substr
  IF read THEN
    IF ob.tp#Char THEN EvalAdr(ob); END;
    CASE ob.tp OF
      Int:      CallExt(ForIO,InputIV);
     |Real:     CallExt(ForIO,InputRV);
     |Double:   CallExt(ForIO,InputDV);
     |Complex:  li(2); CallExt(ForIO,InputRA); -- (val val)
     |Logic:    CallExt(ForIO,InputLV);
     |Char:     CallExt(ForIO,InputCV);
                FreeDesc(INTEGER(ob.wd));
    ELSE
    END;
  ELSE  -- write
    CASE ob.tp OF
      Int:      CallExt(ForIO,OutputIV);
     |Real:     CallExt(ForIO,OutputRV);
     |Double:   CallExt(ForIO,OutputDV);
     |Complex:  EvalAdr(ob);
                li(2); CallExt(ForIO,OutputRA); -- (val val)
     |Logic:    CallExt(ForIO,OutputLV);
     |Char:     CallExt(ForIO,OutputCV);
                FreeDesc(INTEGER(ob.wd));
     ELSE
    END;
  END;
END IOvar;

PROCEDURE IOarr(VAR ob:Dexpr);
  VAR I:Info; nel:INTEGER; p:ADDRESS;
      temp:INTEGER;
BEGIN
  I.name:=ob.name; Unpack(I);
  IF I.farray THEN
     nel:=INTEGER(I.desc^) + I.dim*3;
     llw(Mapoffs);
     IF nel<=255 THEN lsw(nel); ELSE li(nel); c(LXW); epop; END;
  ELSE
    p:=ADDRESS(INTEGER(I.desc) + I.dim*3);
    nel:=p^; li(nel);
  END;
  IF read THEN
    CASE ob.tp OF
      Int:      CallExt(ForIO,InputIA);
     |Real:     CallExt(ForIO,InputRA);
     |Double:   CallExt(ForIO,InputDA);
     |Complex:  c(copt); c(ADD); CallExt(ForIO,InputRA);
     |Logic:    CallExt(ForIO,InputLA);
     |Char:     CallExt(ForIO,InputCA);
                FreeDesc(INTEGER(ob.wd));
     ELSE
    END;
  ELSE
    CASE ob.tp OF
      Int:      CallExt(ForIO,OutputIA);
     |Real:     CallExt(ForIO,OutputRA);
     |Double:   CallExt(ForIO,OutputDA);
     |Complex:  c(copt); c(ADD); CallExt(ForIO,OutputRA);
     |Logic:    CallExt(ForIO,OutputLA);
     |Char:     CallExt(ForIO,OutputCA);
                FreeDesc(INTEGER(ob.wd));
     ELSE
    END;
  END;
END IOarr;

PROCEDURE IOelement(VAR ob:Dexpr);
BEGIN
  WITH ob DO
    IF (emd=var) OR (emd=arrel) OR (emd=substr) THEN
       IOvar(ob);
    ELSIF (emd=expr) OR (emd=cexpr) OR (emd=funcall) THEN
       IOexpr(ob);
    ELSIF emd=arr THEN
       IOarr(ob);
    ELSE Error(15)
    END;
  END;
  Setdepth(0);
END IOelement;

PROCEDURE ImplDo; FORWARD;

PROCEDURE iolist(VAR ob:Dexpr);
BEGIN
  LOOP
    IF sy=lpar THEN
      IF lookAhead('=') THEN
         ImplDo;
      ELSE
        Expr(ob);
        IOelement(ob);
      END;
    ELSE
      ob.start:=cp; Lexpr(ob);
      IF idolevel>0 THEN
        IF sy=becomes THEN
          cp:=ob.start; RETURN
        END;
      END;
      IOelement(ob);
    END;
    IF sy#comma THEN EXIT END;
    GetSy;
  END; -- loop
END iolist;

PROCEDURE ImplDo;
(* Генерация цикла   I=I0,N,By *)
  VAR cp0                       :INTEGER;
      dotype                    :Types;
      Dolab, ODlab              :INTEGER;
      Kir,IR0,Nir,Byir          :WORD;
      const, I0cons,Ncons,Bycons:BOOLEAN;
      dovar                     :INTEGER;
      tK,tBy                    :INTEGER;
      I:Info; ob,obDo:Dexpr;
BEGIN
  INC(idolevel); cp0:=cp;
  GetSy; iolist(ob);
  IF ob.emd#var THEN Error(14); END;
  dovar:=ob.name;
  I.name:=dovar;
  IF GetMode(dovar)=Empty THEN
    GenLocal(I); GenObj(I);
    AllocVar(I);
  ELSIF GetMode(dovar)=xVar THEN
    Unpack(I); AllocVar(I);
  ELSIF GetMode(dovar)=Var THEN
    Unpack(I);
  ELSE
    Error(14);
  END;
  dotype:=GetType(dovar); obDo.emd:=var; obDo.tp:=dotype;
  IF ORD(dotype)>=ORD(Double) THEN
    Error(49); RETURN
  END;
  IF sy#becomes THEN Expected(becomes); RETURN; END;
  InsertMode(cp0);  --? BPins cp0 BPins cp0 ?
  gDovarA(I);
  GetSy; aExpr(ob); I0cons:=ob.emd=cexpr;
  IF AssTypeConv(obDo,ob) THEN RETURN; END;
  AllocTemp(tK,1); tBy:=0;
  IF I0cons THEN
    IR0:=ob.name;
    li(IR0); const:=TRUE;
  ELSE
    -- store in temp. var. --
    epush; c(copt); slw(tK); const:=FALSE;
  END;
  StoreInVar(I);
  IF sy#comma THEN Expected(comma); RETURN; END;
  GetSy; aExpr(ob); Ncons:=ob.emd=cexpr;
  IF AssTypeConv(obDo,ob) THEN RETURN; END;
  IF Ncons THEN
    Nir:=ob.name;
    IF dotype=Int THEN
      IF const THEN Kir:=INTEGER(Nir)-INTEGER(IR0)
      ELSE  li(Nir); llw(tK); c(SUB); epop;
      END;
    ELSIF dotype=Real THEN
      IF const THEN Kir:=REAL(Nir)-REAL(IR0);
      ELSE  li(Nir); llw(tK); c(FSUB); epop;
      END;
    END;
  ELSE
    const:=FALSE;
    -- store in temp. var.
    IF I0cons THEN li(IR0) ELSE llw(tK) END;
    IF dotype=Int THEN c(SUB) ELSE c(FSUB) END; epop;
  END;
  IF sy=comma THEN (* By exist  *)
    GetSy; aExpr(ob); Bycons:=ob.emd=cexpr;
    IF AssTypeConv(obDo,ob) THEN RETURN; END;
    IF Bycons THEN
      Byir:=ob.name;
    ELSE
      -- store in temp. var. --
      AllocTemp(tBy,1); epush; c(copt); slw(tBy);
    END;
  ELSE Byir:=1; Bycons:=TRUE;
    IF dotype=Real THEN Byir:=1. END;
  END;
  Dolab:=gLoclab(); ODlab:=gLoclab();
  IF const THEN
    IF Bycons THEN
      IF dotype=Int THEN
        Kir:=(INTEGER(Nir)-INTEGER(IR0)+INTEGER(Byir))
                       DIV INTEGER(Byir);
        IF INTEGER(Kir)<0 THEN Kir:=0 END;
      ELSE
        Kir:=(REAL(Nir)-REAL(IR0)+REAL(Byir)) / REAL(Byir);
        Kir:=TRUNC(REAL(Kir));
        IF INTEGER(Kir)<0 THEN Kir:=0 END;
      END;
      li(Kir); slw(tK);
    ELSE
      li(Kir);
      IF dotype=Int THEN
        c(ADD); epop; llw(tBy); c(Div); epop;
      ELSE
        c(FADD); epop; llw(tBy); c(FDIV); c1(FFCT,1); epop;
      END;
      epush; c(copt); slw(tK);
      li(0); c(GRT); epop; epop;
      cJump(ODlab,TRUE,FALSE);
    END;
  ELSE -- not const
    IF Bycons THEN
      IF dotype=Int THEN
        IF INTEGER(Byir)=1 THEN c1(LSA,1);
        ELSE li(Byir);
          c(ADD); epop; li(Byir); c(Div); epop;
        END;
      ELSE
        li(Byir); c(FADD); epop; li(Byir);
        c(FDIV); epop; c1(FFCT,1);
      END;
    ELSE
      IF dotype=Int THEN
        c(ADD); epop; llw(tBy); c(Div); epop;
      ELSE
        c(FADD); epop; llw(tBy); c(FDIV); epop; c1(FFCT,1);
      END;
    END;
    epush; c(copt); slw(tK);
    li(0); c(GRT); epop; epop;
    cJump(ODlab,TRUE,FALSE);
  END;
  cLabel(Dolab); llw(tK); cJump(ODlab,TRUE,FALSE); epop;
  InsertEnd;
  gDovarA(I);
  IF dotype=Int THEN
    IF Bycons THEN
      IF INTEGER(Byir)=1 THEN c(INC1)
      ELSIF (INTEGER(Byir)=( -1) ) THEN c(DEC1)
      ELSE li(Byir); c(Inc); epop;
      END; epop;
    ELSE
      llw(tBy); c(Inc); epop; epop;
    END;
  ELSE  -- real do
    gDovarV(I);
    IF Bycons THEN
      li(Byir);
    ELSE
      llw(tBy);
    END;
    c(FADD); epop; StoreInVar(I);
  END;
  lla(tK); c(DEC1); epop;
  cJump(Dolab,FALSE,FALSE);
  cLabel(ODlab);
  DEC(idolevel);
  dovar:=idolevel*3;
  IF I.darea=3 THEN tdovar[dovar]:=I.offset; END;
  tdovar[dovar+1]:=tK; IF tBy>0 THEN tdovar[dovar+2]:=tBy END;
  IF idolevel=0 THEN
    FOR dovar:=0 TO 20 DO
      IF tdovar[dovar]>0 THEN FreeTemp(tdovar[dovar],1);
                              tdovar[dovar]:=0;
      END;
    END;
  END;
  IF sy#rpar THEN Expected(rpar); END;
  GetSy;
END ImplDo;

PROCEDURE IOlist;
VAR ob:Dexpr; i: INTEGER;
BEGIN
  IF sy=EOL THEN RETURN END;
  idolevel:=0; FOR i:=0 TO 20 DO tdovar[i]:=0 END;
  iolist(ob);
END IOlist;

PROCEDURE genFmt():INTEGER;
BEGIN
  WITH obfmt DO
    IF emd=star THEN
      RETURN 1
    ELSIF tp=Int THEN
      IF emd=cexpr THEN
        GetFmt(name);
        RETURN 2
      ELSE
      -- ASSIGN format TO var  не реализовано
        Error(64);
        RETURN 0
      END;
    ELSE
      --  descriptor of Run-Time format was loaded
        Error(64);
      RETURN 3
    END;
  END;
END genFmt;

PROCEDURE genRdWr;
VAR i,proc: INTEGER;
BEGIN
  IF obunit.emd=star THEN  --  system device
    IF rec IN ssym THEN Error(41); RETURN END;
    IF NOT ( fmt IN ssym) THEN Error(41); RETURN END;
    i:=genFmt();
    CASE i OF
      0:
     |1:   IF read THEN proc:=ReadSL  ELSE proc:=WriteSL  END;
     |2:   IF read THEN proc:=ReadSF  ELSE proc:=WriteSF  END;
     |3: --IF read THEN proc:=ReadSFR ELSE proc:=WriteSFR END;
     ELSE
    END;
  ELSIF obunit.tp=Int THEN --  file
  --  IF obunit.emd=cexpr THEN li(obunit.name); END;
    IF rec IN ssym THEN -- direct
      IF  fmt IN ssym THEN  -- direct formatted
        i:=genFmt();
        IF obrec.emd=cexpr THEN li(obrec.name)
        ELSE llw(obrec.name); FreeTemp(obrec.name,1);
        END;
        CASE i OF
         0:
         |1:   IF read THEN proc:=ReadDL  ELSE proc:=WriteDL  END;
         |2:   IF read THEN proc:=ReadDF  ELSE proc:=WriteDF  END;
         |3: --IF read THEN proc:=ReadDFR ELSE proc:=WriteDFR END;
        ELSE
        END;
      ELSE  -- direct unformatted
        IF obrec.emd=cexpr THEN li(obrec.name)
        ELSE llw(obrec.name); FreeTemp(obrec.name,1);
        END;
        IF read THEN proc:=ReadDU ELSE proc:=WriteDU END;
      END;
    ELSE
      IF fmt IN ssym THEN
        CASE genFmt() OF
         0:
         |1:   IF read THEN proc:=ReadSL  ELSE proc:=WriteSL  END;
         |2:   IF read THEN proc:=ReadSF  ELSE proc:=WriteSF  END;
         |3: --IF read THEN proc:=ReadSFR ELSE proc:=WriteSFR END;
        ELSE
        END;
      ELSE  -- unformatted
        IF read THEN proc:=ReadSU ELSE proc:=WriteSU END;
      END;
    END;
  ELSE  --  internal file
    i:=genFmt();
    IF read THEN proc:=ReadIF  ELSE proc:=WriteIF  END;
  END;
  CallExt(ForIO,proc);
  moveCode;
END genRdWr;

PROCEDURE genIOend;
VAR temp,local: INTEGER;
BEGIN
  IF iostat IN ssym THEN
    llw(obiostat.name); FreeTemp(obiostat.name,1);
  ELSE li(NIL);
  END;
  CallExt(ForIO,IOend);
  IF (err IN ssym) AND (lend IN ssym) THEN
    AllocTemp(temp,1); local:=gLoclab();
      c(copt); slw(temp); cJump(local,TRUE,FALSE);
      llw(temp); c(GRT);  Goto(TRUE,FALSE,endlab);
      Goto(FALSE,FALSE,errlab);
      moveCode; pLabel(local);
    FreeTemp(temp,1);
  ELSIF err IN ssym  THEN
    AllocTemp(temp,1); local:=gLoclab();
      c(copt); slw(temp); cJump(local,TRUE,FALSE);
      llw(temp); c(LSS);  Goto(TRUE,FALSE,errlab);
      llw(temp); CallExt(ForIO,IOerror);
      moveCode; pLabel(local);
    FreeTemp(temp,1);
  ELSIF lend IN ssym THEN
    AllocTemp(temp,1); local:=gLoclab();
      c(copt); slw(temp); cJump(local,TRUE,FALSE);
      llw(temp); c(GRT);  Goto(TRUE,FALSE,endlab);
      llw(temp); CallExt(ForIO,IOerror);
      moveCode; pLabel(local);
    FreeTemp(temp,1);
  ELSE
    CallExt(ForIO,IOerror);
    moveCode;
  END;
END genIOend;

PROCEDURE Write;
BEGIN  ssym:=Bits{};
  GetSy; IF sy#lpar THEN Expected(lpar) END;
  IF CInfoList() THEN RETURN END;
  IF sy#rpar THEN Expected(rpar) END;
  IF lend IN ssym THEN Error(46); END;
  GetSy;
  read:=FALSE; genRdWr;
  IOlist;
  genIOend;
END Write;

PROCEDURE Read;
BEGIN  ssym:=Bits{};
  GetSy;
  IF sy=lpar THEN
     IF CInfoList() THEN RETURN END;
     IF sy#rpar THEN Expected(rpar) END;
     GetSy;
   ELSE
     obunit.emd:=star; INCL(ssym,unit); li(-1);
     IF idFormat(obfmt) THEN RETURN END;
     IF sy=EOL THEN
     ELSIF sy#comma THEN Expected(comma)
     ELSE GetSy
     END;
  END;
  read:=TRUE; genRdWr;
  IOlist;
  genIOend;
END Read;

PROCEDURE Print;
BEGIN  ssym:=Bits{};
  obunit.emd:=star; INCL(ssym,unit); li(-1);
  GetSy;
  IF idFormat(obfmt) THEN RETURN END;
  IF sy=EOL THEN
  ELSIF sy#comma THEN Expected(comma)
  ELSE GetSy
  END;
  read:=FALSE; genRdWr;
  IOlist;
  genIOend;
END Print;

CONST exchRec=2;

PROCEDURE putexchPar(VAR ob:Dexpr; psw:Psymbol);
  CONST ioStat=0; iofile=1;  iostatus=2; ioaccess=3;
        ioform=4; ioblank=5; iorecl=6;
BEGIN
  IF ob.emd=invmd THEN RETURN END;
  IF ob.emd=cexpr THEN LoadConst(ob); END;
  CASE psw OF
     access: ssw(ioaccess);
    |blank:  ssw(ioblank);
    |form:   ssw(ioform);
    |file:   ssw(iofile);
    |iostat: ssw(ioStat);
    |recl:   ssw(iorecl);
    |status: ssw(iostatus);
    ELSE
  END;
END putexchPar;

PROCEDURE genIOcall(proc:INTEGER);
  VAR local: INTEGER;
BEGIN
  IF NOT (unit IN ssym) THEN  Error(40)
  ELSE
    CallExt(ForIO,proc);
    IF err IN ssym THEN
      local:=gLoclab();
      cJump(local,TRUE,FALSE);
      Goto(FALSE,FALSE,errlab);
      moveCode; pLabel(local);
    ELSE CallExt(ForIO,IOerror); moveCode;
    END;
  END;
END genIOcall;

PROCEDURE KOpen(pkw:Psymbol);
  VAR ob:Dexpr;
BEGIN
  IF pkw IN ssym THEN Error(47); END;
  INCL(ssym,pkw); GetSy;
  lew(ForIO,exchRec); tExpr(ob);
  putexchPar(ob,pkw);
END KOpen;

PROCEDURE Open;
VAR pkw:Psymbol; i,local:INTEGER;
BEGIN  ssym:=Bits{};
  i:=0; GetSy;
  IF sy#lpar THEN Expected(lpar) END;
  LOOP
    GetPosKW(pkw);
    CASE pkw OF
      invPkw: IF i=0 THEN
                GetSy; IF idChanel(obunit) THEN
                       ELSE
                       END;
              ELSE Error(41); GetSy;
              END;
     |unit:   Kunit;
     |form :  KOpen(form);
     |file :  KOpen(file);
     |status: KOpen(status);
     |access: KOpen(access);
     |blank : KOpen(blank);
     |recl:   IF recl IN ssym THEN Error(47) END;
              INCL(ssym,recl); GetSy;
              lew(ForIO,exchRec); iExpr(ob);
              putexchPar(ob,recl);
     |iostat: IF iostat IN ssym THEN Error(47);  END;
              INCL(ssym,iostat); GetSy;
              lew(ForIO,exchRec); iExpr(ob);
              IF (ob.emd#var) AND (ob.emd#arrel)
              THEN Error(53);
              ELSE EvalAdr(ob);
              END;
              putexchPar(ob,iostat);
     |err :   Kerr;
    --- ins in fcScan  |Rpar : sy:=rpar; EXIT
     ELSE   Error(46); GetSy;
    END; -- case
    IF sy#comma THEN EXIT; END;
    INC(i);
  END; -- loop
  IF sy#rpar THEN  Expected(rpar) END;
  genIOcall(ForOpen);
--  moveCode;
END Open;

PROCEDURE Close;
VAR pkw:Psymbol; i:INTEGER;
BEGIN ssym:=Bits{};
  i:=0; GetSy;
  IF sy#lpar THEN Expected(lpar) END;
  LOOP
    GetPosKW(pkw);
    CASE pkw OF
      invPkw: IF i=0 THEN
                GetSy; IF idChanel(obunit) THEN
                       ELSE
                       END;
              ELSE Error(41); GetSy;
              END;
     |unit:   Kunit;
     |status: KOpen(status);
     |iostat: Kiostat;
     |err :   Kerr;
     -- |Rpar : sy:=rpar; EXIT
     ELSE   Error(46); GetSy;
    END; -- case
    IF sy#comma THEN EXIT; END;
    INC(i);
  END; -- loop
  IF sy#rpar THEN  Expected(rpar) END;
  IF (iostat IN ssym) THEN
    llw(obiostat.name); FreeTemp(obiostat.name,1);
  ELSE li(NIL);
  END;
  genIOcall(ForClose);
END Close;

PROCEDURE KInqTE(pkw:Psymbol);
BEGIN
  IF pkw IN ssym THEN Error(47); END;
  INCL(ssym,pkw); GetSy;
  tExpr(ob);
END KInqTE;

PROCEDURE KInqTV(pkw:Psymbol);
BEGIN
  IF pkw IN ssym THEN Error(47); END;
  INCL(ssym,pkw); GetSy; tExpr(ob);
  IF (ob.emd#var) AND (ob.emd#arrel) THEN
    Error(53);
  END;
END KInqTV;

PROCEDURE KInqI(pkw:Psymbol);
BEGIN
  IF pkw IN ssym THEN Error(47); END;
  INCL(ssym,pkw); GetSy; iExpr(ob);
  IF (ob.emd#var) AND (ob.emd#arrel) THEN
    Error(53);
  END;
END KInqI;

PROCEDURE KInqL(pkw:Psymbol);
BEGIN
  IF pkw IN ssym THEN Error(47); END;
  INCL(ssym,pkw); GetSy; lExpr(ob);
  IF (ob.emd#var) AND (ob.emd#arrel) THEN
    Error(53);
  END;
END KInqL;

PROCEDURE Inquire;
VAR pkw:Psymbol; i:INTEGER;
BEGIN ssym:=Bits{};
  i:=0; GetSy;
  IF sy#lpar THEN Expected(lpar) END;
  LOOP
    GetPosKW(pkw);
    CASE pkw OF
      invPkw: IF i=0 THEN
                GetSy; IF idChanel(obunit) THEN
                       ELSE
                       END;
              ELSE Error(41); GetSy;
              END;
     |unit:   IF (file IN ssym) THEN Error(47); END;
              Kunit;
     |file :  IF (unit IN ssym) THEN Error(47); END;
                   KInqTE(pkw);
     |form :       KInqTV(pkw);
     |sequential:  KInqTV(pkw);
     |access:      KInqTV(pkw);
     |blank :      KInqTV(pkw);
     |recl:        KInqI(pkw);
     |iostat:      KInqI(pkw);
     |exist:       KInqL(pkw);
     |opened:      KInqL(pkw);
     |number:      KInqI(pkw);
     |name:        KInqTV(pkw);
     |direct:      KInqTV(pkw);
     |nextrec:     KInqI(pkw);
     |formatted:   KInqTV(pkw);
     |unformatted: KInqTV(pkw);
     |err :  Kerr;
    -- |Rpar : sy:=rpar; EXIT
     ELSE   Error(46); GetSy;
    END; -- case
    IF sy#comma THEN EXIT; END;
    INC(i);
  END; -- loop
  IF sy#rpar THEN  Expected(rpar) END;
  IF NOT((unit IN ssym) OR (file IN ssym)) THEN  Error(40) END;
  -- gen Inquire call
  Error(64);
END Inquire;

PROCEDURE Backspace;
VAR pkw:Psymbol; i:INTEGER;
BEGIN  ssym:=Bits{};
  i:=0; GetSy;
  IF sy=lpar THEN
    LOOP
      GetPosKW(pkw);
      CASE pkw OF
        invPkw: IF i=0 THEN
                  GetSy; IF idChanel(obunit) THEN
                  ELSE
                  END;
                ELSE Error(41); GetSy;
                END;
        |unit:  Kunit;
        |iostat:Kiostat;
        |err : Kerr;
       -- |Rpar : sy:=rpar; EXIT
      ELSE   Error(46); GetSy;
      END; -- case
      IF sy#comma THEN EXIT; END;
      INC(i);
    END; -- loop
    IF sy#rpar THEN  Expected(rpar) END;
    IF NOT (unit IN ssym) THEN  Error(40) END;
  ELSE
    IF idChanel(obunit) THEN
    ELSE
    END;
  END;
  IF (iostat IN ssym) THEN
    llw(obiostat.name); FreeTemp(obiostat.name,1);
  ELSE li(NIL);
  END;
  genIOcall(ForBackspace);
END Backspace;

PROCEDURE Endfile;
VAR pkw:Psymbol; i:INTEGER;
BEGIN ssym:=Bits{};
  i:=0; GetSy;
  IF sy=lpar THEN
    LOOP
      GetPosKW(pkw);
      CASE pkw OF
        invPkw: IF i=0 THEN
                  GetSy; IF idChanel(obunit) THEN
                  ELSE
                  END;
                ELSE Error(41); GetSy;
                END;
        |unit:  Kunit;
        |iostat:Kiostat;
        |err :  Kerr;
      --  |Rpar : sy:=rpar; EXIT
      ELSE   Error(46); GetSy;
      END; -- case
      IF sy#comma THEN EXIT; END;
      INC(i);
    END; -- loop
    IF sy#rpar THEN  Expected(rpar) END;
    IF NOT (unit IN ssym) THEN  Error(40) END;
  ELSE
    IF idChanel(obunit) THEN
    ELSE
    END;
  END;
  IF (iostat IN ssym) THEN
    llw(obiostat.name); FreeTemp(obiostat.name,1);
  ELSE li(NIL);
  END;
  genIOcall(EndFile);
END Endfile;

PROCEDURE Rewind;
VAR pkw:Psymbol; i:INTEGER;
BEGIN  ssym:=Bits{};
  i:=0; GetSy;
  IF sy=lpar THEN
    LOOP
      GetPosKW(pkw);
      CASE pkw OF
        invPkw: IF i=0 THEN
                  GetSy; IF idChanel(obunit) THEN
                  ELSE
                  END;
                ELSE Error(41); GetSy;
                END;
        |unit:  Kunit;
        |iostat:Kiostat;
        |err :  Kerr;
      --  |Rpar : sy:=rpar; EXIT
      ELSE   Error(46); GetSy;
      END; -- case
      IF sy#comma THEN EXIT; END;
      INC(i);
    END; -- loop
    IF sy#rpar THEN  Expected(rpar) END;
    IF NOT (unit IN ssym) THEN  Error(40) END;
  ELSE
    IF idChanel(obunit) THEN
    ELSE
    END;
  END;
  IF (iostat IN ssym) THEN
    llw(obiostat.name); FreeTemp(obiostat.name,1);
  ELSE li(NIL);
  END;
  genIOcall(ForRewind);
END Rewind;

VAR fmtofs, fmtpos :INTEGER;

PROCEDURE Putfc(ch:CHAR);
BEGIN
  STP[fmtpos]:=ch; INC(fmtpos);
  IF fmtpos>=HIGH(STP) THEN Fault(13); END;
END Putfc;

PROCEDURE Putfc0(sym:Formsym;rep:INTEGER);
(* repX , repP , Trep, TLrep, TSrep *)
BEGIN
    Putfc(CHAR(ORD(sym))); Putfc(CHAR(rep));
END Putfc0;

PROCEDURE Putfc1(rep:INTEGER;sym:Formsym;w:INTEGER);
(*  dL, dA, (   *)
VAR ch:CHAR;
BEGIN
  ch:=CHAR(ORD(sym));
  IF ((w>0) AND (sym=dA)) THEN ch:=CHAR(ORD(sym)+1) END;
  IF rep>1 THEN ch:=CHAR(BITSET(ch)+{7});
    Putfc(ch); Putfc(CHAR(rep));
  ELSE
    Putfc(ch);
  END;
  IF w>0 THEN Putfc(CHAR(w)); END;
END Putfc1;

PROCEDURE Putfc2(rep:INTEGER;sym:Formsym;w,d:INTEGER);
(*  dI   *)
VAR ch:CHAR;
BEGIN
  ch:=CHAR(ORD(sym));
  IF rep>1 THEN ch:=CHAR(BITSET(ch)+{7});
    Putfc(ch); Putfc(CHAR(rep));
  ELSE
    Putfc(ch);
  END;
  Putfc(CHAR(w));
  IF d>0 THEN Putfc(CHAR(d)); END;
END Putfc2;

PROCEDURE Putfc3(rep:INTEGER;sym:Formsym;w,d,e:INTEGER);
(* dF,dE,dD,dG   *)
VAR ch:CHAR;
BEGIN
  ch:=CHAR(ORD(sym));
  IF e>0 THEN ch:=CHAR(ORD(sym)+1) END;
  IF rep>1 THEN ch:=CHAR(BITSET(ch)+{7});
    Putfc(ch); Putfc(CHAR(rep));
  ELSE
    Putfc(ch);
  END;
  Putfc(CHAR(w));
  Putfc(CHAR(d));
  IF e>0 THEN Putfc(CHAR(e)); END;
END Putfc3;

PROCEDURE PutfcH;
(* Slen,Sval  *)
VAR i: INTEGER;
BEGIN
  Putfc(CHAR(ORD(dH))); Putfc(CHAR(Slen));
  FOR i:=0 TO Slen-1 DO Putfc(Sval[i]); END;
END PutfcH;

PROCEDURE PutfcS;
(* Slen,Sval  *)
BEGIN
  PutfcH;
END PutfcS;

PROCEDURE Getfrest(sym:Formsym; VAR w,d,e:INTEGER);
BEGIN
  Getfsym;
  IF fsym=dnum THEN
    w:=Ival; Getfsym;
    IF (sym=dA) OR (sym=dL) THEN RETURN END;
  ELSE
    IF sym=dA THEN w:=0;
    ELSE Error(11); w:=1;
    END;
    RETURN ;
  END;
  IF fsym=dot THEN
    Getfsym;
    IF fsym#dnum THEN Error(11); d:=1; RETURN END;
    d:=Ival; Getfsym;
    IF sym=dI THEN RETURN END;
  ELSE
    IF sym=dI THEN d:=0; RETURN END;
    Error(11); d:=1; RETURN
  END;
  e:=0;
  IF (sym=dE) OR (sym=dG) THEN
    IF fsym=dot THEN Getfsym;
      IF fsym#dnum THEN Error(11); e:=0
      ELSE e:=Ival; Getfsym;
      END;
    END;
  END;
END Getfrest;

PROCEDURE Gdescr(VAR level,nrep:INTEGER);
VAR rep,k,w,d,e:INTEGER; sym:Formsym;
BEGIN
  IF level=1 THEN Putfc(CHAR(ORD(startPos))); END;
  IF level>0 THEN Putfc1(nrep,dlpar,0); END;
  INC(level);
  LOOP
    Getfsym;
    IF fsym#dslash THEN EXIT END;
    Putfc(CHAR(ORD(dslash)));
  END;
  LOOP
    IF fsym=dnum THEN
      rep:=Ival; Getfsym;
      CASE fsym  OF
       dlpar: Gdescr(level,rep); Getfsym;
      |dI : Getfrest(dI,w,d,e); Putfc2(rep,dI,w,d);
      |dF : Getfrest(dF,w,d,e); Putfc3(rep,dF,w,d,e);
      |dE : Getfrest(dE,w,d,e); Putfc3(rep,dE,w,d,e);
      |dD : Getfrest(dD,w,d,e); Putfc3(rep,dD,w,d,e);
      |dG : Getfrest(dG,w,d,e); Putfc3(rep,dG,w,d,e);
      |dL : Getfrest(dL,w,d,e); Putfc1(rep,dL,w);
      |dA : Getfrest(dA,w,d,e); Putfc1(rep,dA,w);
      |dX : Putfc0(dX,rep); Getfsym;
      |dH : GetHoll(rep); PutfcH; Getfsym;
      |dP : Putfc0(dP,rep); Getfsym;
            IF fsym=dnum THEN
               rep:=Ival; Getfsym;
            ELSE rep:=1;
            END;
            CASE fsym OF
             dF : Getfrest(dF,w,d,e); Putfc3(rep,dF,w,d,e);
            |dE : Getfrest(dE,w,d,e); Putfc3(rep,dE,w,d,e);
            |dD : Getfrest(dD,w,d,e); Putfc3(rep,dD,w,d,e);
            |dG : Getfrest(dG,w,d,e); Putfc3(rep,dG,w,d,e);
            ELSE Error(11); fsym:=drpar;
            END;
      |drpar: DEC(level); RETURN ;  -- ? 10 ) ??
      ELSE Error(11); fsym:=drpar;
      END;
    ELSE -- not number
      rep:=1;
      CASE fsym  OF
       dlpar: Gdescr(level,rep); Getfsym;
      |dI : Getfrest(dI,w,d,e); Putfc2(rep,dI,w,d);
      |dF : Getfrest(dF,w,d,e); Putfc3(rep,dF,w,d,e);
      |dE : Getfrest(dE,w,d,e); Putfc3(rep,dE,w,d,e);
      |dD : Getfrest(dD,w,d,e); Putfc3(rep,dD,w,d,e);
      |dG : Getfrest(dG,w,d,e); Putfc3(rep,dG,w,d,e);
      |dL : Getfrest(dL,w,d,e); Putfc1(rep,dL,w);
      |dA : Getfrest(dA,w,d,e); Putfc1(rep,dA,w);
      |dstring : PutfcS; Getfsym;
      |dT,dTL,dTR: sym:=fsym; Getfsym;
          IF fsym=dnum THEN w:=Ival; Putfc0(sym,w);
                            Getfsym;
          ELSE Error(11); fsym:=drpar;
          END;
      |dcolon: Putfc(CHAR(ORD(dcolon))); Getfsym;
      |dS:     Putfc(CHAR(ORD( dS))); Getfsym;
      |dSP:    Putfc(CHAR(ORD(dSP))); Getfsym;
      |dSS:    Putfc(CHAR(ORD(dSS))); Getfsym;
      |dBN:    Putfc(CHAR(ORD(dBN))); Getfsym;
      |dBZ:    Putfc(CHAR(ORD(dBZ))); Getfsym;
      |drpar:  DEC(level);
               IF level=0 THEN Putfc(CHAR(ORD(eofFt)));
               ELSE            Putfc(CHAR(ORD(drpar  )));
               END;
               RETURN ;
      ELSE Error(11); fsym:=drpar;
      END;
    END; -- if
    IF fsym=drpar THEN DEC(level);
      IF level=0 THEN Putfc(CHAR(ORD(eofFt)));
      ELSE            Putfc(CHAR(ORD(drpar  )));
      END;
      RETURN
    END;
    IF fsym=dcomma THEN Getfsym;
    ELSE
      IF fsym#dslash THEN Expected(comma) END;

      WHILE fsym=dslash DO
          Putfc(CHAR(ORD(dslash))); Getfsym;
      END;

    END;
  END; -- global loop
END Gdescr;

PROCEDURE Format;
VAR level,nrep:INTEGER;
BEGIN
  fmtofs:=stpPos DIV 4; fmtpos:=fmtofs*4  ;
  GetSy;
  IF sy#lpar THEN Expected(lpar);
    IF sy=EOL THEN RETURN END;
  END;
  level:=0; nrep:=1;
  Gdescr(level,nrep);
  IF sy#rpar THEN Expected(rpar);
  ELSE
    GetSy; IF sy#EOL THEN Error(16) END;
  END;
  WHILE (fmtpos MOD 4)#0 DO
   STP[fmtpos]:=0c; INC(fmtpos);
  END;
  stpPos:=fmtpos;
  SetFmt(Label,fmtofs);
END Format;

END fcInOut.
