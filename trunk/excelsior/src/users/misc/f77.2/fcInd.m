IMPLEMENTATION MODULE fcInd;

FROM SYSTEM  IMPORT WORD, ADDRESS, ADR;
FROM fcGen   IMPORT epop, epush, cp, c, c1, c2, li, lla, llw, slw, lsa,
                    lsw, ssw, STP, CallExt,
                    Setdepth, Getdepth, InsertMode,InsertEnd, MarkC, BackC,
                    InPoolStr, cOrJump, cAndJump, cEndLab ,lsta;
FROM fcScan  IMPORT GetSy,Symbol,sy,Expected,
                    Error,MarkPos1,BackUp1, CxConst,
                    cType,Ival,Rval,Slen,Sval,lookAhead,
                    Warning, KeyON;
FROM fcObj   IMPORT SetType,GetType,Types, Mode,GetMode,SetMode,indbit,
                    Info,Class,idno,Unpack,Pack,GenObj,Ident, GenLocal;
FROM fcDcl   IMPORT AllocVar,AllocExt,AllocIntr, LHD, ALHD, Mapoffs;
FROM fcTproc IMPORT AllocDesc, AllocTemp, FreeTemp;
FROM fcExpr  IMPORT Dexpr, Emode, iExpr;
FROM fcDefs  IMPORT maxpar;
FROM fcProc  IMPORT FuncCall, LookIntr;
FROM StdIO   IMPORT Show;
FROM fcBugs  IMPORT AddStr;

CONST eol=0c;
      LSS =0A0h; LEQ =0A1h; GTR =0A2h; NEG =0A7h;
      GEQ =0A3h; EQU =0A4h; NEQ =0A5h;
      FFCT =9Fh; FADD =98h; FSUB =99h; FABS =9Dh;
      FMUL =9Ah; FDIV =9Bh; FNEG =9Eh; FCMP =9Ch;
      LXW  =41h; LSTA=0C2h;
      ADD  =88h; SUB  =89h; MUL  =8Ah; Div  =8Bh;
      And =0A9h; Or  =0A8h; Not =0AEh; LSA  =16h;
      CHKZ=0C7h; Drop=0B1h; Stot=0E8h; DECS=0B0h;
      SXW  =51h; Move=0C0h; copt=0B5h; swap=0F0h;

CONST ForLib=2; CxMul=32; CxDiv=33;
      ChAssign=10;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE ldSubstr(VAR I:Info; VAR ob:Dexpr);
  VAR lo,hi,D:INTEGER; ob1:Dexpr;
      loconst, deflen: BOOLEAN;
      cs:BITSET;
BEGIN
  AllocDesc(D); cs:={};
  IF (I.cl=Param) OR (I.cl=Func) THEN
    lla(D); LoadAdr(I); li(3); c(Move); epop; epop; epop;
    IF I.lenel=0 THEN
      deflen:=TRUE; INCL(cs,1);
    ELSE
      deflen:=FALSE; li(I.lenel); slw(D+2);
    END;
    IF sy#lpar THEN
      IF ob.emd=arrel THEN
        IF INTEGER(ob.wd)>0 THEN
          li(ob.wd); llw(D+1); c(ADD); epop; slw(D+1);
        ELSIF INTEGER(ob.wd)<0 THEN
          llw(D+1); c(ADD); epop; slw(D+1);
        END;
      END;
      lla(D); ob.wd:=D; RETURN
    END;
  ELSE
    llw(I.darea); slw(D);
    IF I.lenel=0 THEN Error(5); I.lenel:=1; Pack(I); END;
    deflen:=FALSE;
    IF sy#lpar THEN
      li(I.offset);
      IF ob.emd=arrel THEN
        IF INTEGER(ob.wd)>0 THEN
          li(ob.wd);  c(ADD); epop;
        ELSIF INTEGER(ob.wd)<0 THEN
          c(ADD); epop;
        END;
      END;
      slw(D+1); li(I.lenel); slw(D+2);
      lla(D); ob.wd:=D; RETURN
    END;
  END;
  --  sy=lpar --
  GetSy;
  IF sy=col THEN
    ob1.emd:=cexpr; ob1.name:=1;
    --  LC 1 --
  ELSE iExpr(ob1);
  END;
  IF ob1.emd=cexpr THEN
    lo:=ob1.name; INCL(cs,2);
    IF lo < 1 THEN Error(23); lo:=1
    ELSIF lo=1 THEN -- o.k.
    ELSIF deflen THEN
      -- li(lo); li(1); llw(D+2); c(CHK); c(drop);
    ELSE
      IF lo>I.lenel THEN Error(23); lo:=I.lenel; END;
    END;
    DEC(lo);  loconst:=TRUE;
  ELSE
    loconst:=FALSE; AllocTemp(lo,1);
    li(1); c(SUB); c(copt); slw(lo);
    IF deflen THEN
      --  llw(D+2); li(1); c(SUB); c(CHKZ);
    ELSE
      -- li(I.lenel-1); c(CHKZ);
    END;
  END;
-- offs
  IF ob.emd=arrel THEN INCL(cs,0); END;
  CASE INTEGER(cs) OF
    0: li(I.offset); c(ADD); epop;
   |1: IF INTEGER(ob.wd)>=0 THEN
             li(I.offset+INTEGER(ob.wd)); c(ADD); epop;
       ELSE  c(ADD); epop; li(I.offset); c(ADD); epop;
       END;
   |2: llw(D+1); c(ADD); epop;
   |3: IF INTEGER(ob.wd)>=0 THEN
             llw(D+1); c(ADD); epop; li(INTEGER(ob.wd)); c(ADD); epop;
       ELSE  c(ADD); epop; llw(D+1); c(ADD); epop;
       END;
   |4: li(lo+I.offset);
   |5: IF INTEGER(ob.wd)>=0 THEN
             li(lo+I.offset+INTEGER(ob.wd));
       ELSE  li(lo+I.offset); c(ADD); epop;
       END;
   |6: li(lo); llw(D+1); c(ADD); epop;
   |7: IF INTEGER(ob.wd)>=0 THEN
             li(lo+INTEGER(ob.wd)); llw(D+1); c(ADD); epop;
       ELSE  li(lo); c(ADD); epop; llw(D+1); c(ADD); epop;
       END;
  ELSE
  END; -- case --
  slw(D+1);
  IF sy#col THEN Expected(col); END;
  GetSy; EXCL(cs,0);
  IF sy=rpar THEN
    IF  deflen THEN
      llw(D+2); ob1.emd:=expr;
    ELSE ob1.emd:=cexpr; ob1.name:=I.lenel;
    END;
  ELSE iExpr(ob1);
  END;
  IF ob1.emd=cexpr THEN
     hi:=ob1.name; INCL(cs,0);
  END;
  CASE INTEGER(cs) OF
    0:
       -- llw(lo); lsa(1); li(I.lenel); c(CHK);
       llw(lo); c(SUB); epop;
   |1: li(hi);
       -- llw(lo); lsa(1); li(I.lenel); c(CHK);
       llw(lo); c(SUB); epop;
   |2:
       -- llw(lo); lsa(1); llw(D+2); c(CHK);
       llw(lo); c(SUB); epop;
   |3: li(hi);
       -- llw(lo); lsa(1); llw(D+2); c(CHK);
       llw(lo); c(SUB); epop;
   |4:
       -- li(lo+1); li(I.lenel); c(CHK);
       li(lo); c(SUB); epop;
   |5:
       IF (hi<=lo) OR (hi>I.lenel) THEN Error(23);  END;
       li(hi-lo);
   |6:
       -- li(lo+1); llw(D+2); c(CHK);
       li(lo); c(SUB); epop;
   |7:
       IF (hi<=lo) THEN Error(23);  END;
       -- li(hi); li(lo+1); llw(D+2); c(CHK); c(drop);
       li(hi-lo);
  ELSE
  END; -- case --
  slw(D+2); lla(D); ob.wd:=D;
  IF NOT loconst THEN FreeTemp(lo,1); END;
  IF sy#rpar THEN Expected(rpar) END;
  GetSy;
END ldSubstr;

PROCEDURE LoadInd(VAR I:Info):INTEGER;
  VAR da:ALHD; p:ADDRESS; cpart,lo,hi,d:INTEGER;
      ob:Dexpr; cpind,dim,offs:INTEGER;
      first,stack:BOOLEAN;
BEGIN
-- IF KeyON('e') THEN AddStr("LdInd");END;
  --IF NOT I.farray THEN
    p:=I.desc;
    cpart:=p^;
    p:=p+1; dim:=I.dim*3;
    MOVE(ADR(da),p,dim);
  --END;
  dim:=0; cpind:=0; d:=1;
  first:=TRUE; stack:=FALSE;
  sy:=comma;
  WHILE sy=comma DO
    IF NOT I.farray THEN
      IF dim<I.dim THEN
        lo:=da[dim].lo; hi:=da[dim].hi;
      END;
    END;
    GetSy; iExpr(ob);
    IF ob.emd=cexpr THEN
      IF I.farray THEN
        li( ob.name );
        IF first THEN
          first:=FALSE;
        ELSE
          c(MUL); c(ADD); epop; epop;
        END;
      ELSE
        IF (ob.name<lo) OR (ob.name>hi) THEN
          Error(33); ob.name:=lo
        END;
        cpind:=cpind+ob.name*d
      END;
    ELSE -- ind is expr.
      IF I.farray THEN
        IF first THEN
          first:=FALSE;
        ELSE
          c(MUL); c(ADD); epop; epop;
        END;
      ELSE
        IF first THEN
          stack:=TRUE; first:=FALSE;
        ELSE
          -- load d; mult; --
          li(d); c(MUL); epop;
          IF stack THEN
            -- add --
            c(ADD); epop;
          ELSE stack:=TRUE
          END;
        END;
      END;  -- end ind is expr.
    END;  -- pars ind
    IF I.farray THEN
      INC(dim);
      IF dim<I.dim THEN offs:=cpart+dim*3; llw(Mapoffs); -- LocalBase
        IF offs<=255 THEN lsw(offs) ELSE li(offs); c(LXW); epop; END;
      END;
    ELSE
      IF dim < I.dim THEN  d:=da[dim].d END;
      INC(dim);
    END;
  END; --while
  IF sy#rpar THEN Expected(rpar) END;
  IF dim#I.dim THEN Error(32) END;
  IF I.farray THEN llw(Mapoffs);
    --load cpart; sub; load lenel; mult --
    IF cpart<=255 THEN lsw(cpart) ELSE li(cpart); c(LXW); epop; END;
    c(SUB); epop;
    -- test 0<= offset < abs
    -- d:=da[I.dim-1];
    -- IF NOT ((d=0) OR (d=-1)) THEN
    --   llw(Mapofss); offs:=cpart+I.dim*3;
    --   IF offs<=255 THEN lsw(offs) ELSE li(offs); c(LXW); END;
    --   li(1); c(SUB); c(CHKZ);
    -- END;
    IF I.lenel=0 THEN
      IF I.cl=Param THEN
        llw(I.offset); lsw(2); c(MUL); epop;
      END;
    ELSIF I.lenel=1 THEN
    ELSE li(I.lenel); c(MUL); epop;
    END; RETURN -1;
  ELSE  -- not farray
    IF stack THEN
      cpart:=cpart-cpind;
      -- load cpart; sub; --
      li(cpart); c(SUB); epop;
      -- test 0<= offset < abs
      -- li(d-1); c(CHKZ);
      -- load lenel; mult; --
      IF I.lenel=0 THEN
        IF I.cl=Param THEN
          llw(I.offset); lsw(2); c(MUL); epop;
        END;
      ELSIF I.lenel=1 THEN
      ELSE li(I.lenel); c(MUL); epop;
      END; RETURN -1;
    ELSE  -- constant
      cpind:=cpind-cpart;
      -- test 0<= offset < abs
      IF ( cpind<0 ) OR ( d<=cpind ) THEN Error(33) END;
      IF I.lenel=0 THEN
        li(cpind);
        IF I.cl=Param THEN
          llw(I.offset); lsw(2); c(MUL); epop;
        END; RETURN -1;
      ELSE RETURN cpind*I.lenel;
      END;
    END;
  END;
END LoadInd;

PROCEDURE EvalAdr(VAR ob:Dexpr);
  VAR I:Info;
BEGIN BackC;
  IF ob.emd=var THEN
    I.name:=ob.name; Unpack(I); LoadAdr(I);
  ELSE -- arrel
    IF INTEGER(ob.wd) > 255 THEN
      li(ob.wd); c(ADD); epop;
    ELSIF INTEGER(ob.wd)=-1 THEN
      c(ADD); epop;
    ELSE
      c1(LSA,ob.wd);
    END;
  END;
END EvalAdr;

PROCEDURE EvalVal(VAR I:Info; VAR ob:Dexpr);
BEGIN  MarkC;
  IF INTEGER(ob.wd) > 255 THEN
    li(ob.wd); c(ADD); epop;
    IF I.lenel=1 THEN lsw(0);
    ELSE c(copt); epush; lsw(0); c(swap); lsw(1);
    END;
  ELSIF INTEGER(ob.wd)=-1 THEN
    IF I.lenel=1 THEN c(LXW); epop;
    ELSE c(ADD); c(copt); lsw(0); c(swap); lsw(1);
    END;
  ELSE
    IF I.lenel=1 THEN lsw(ob.wd);
    ELSE c1(LSA,ob.wd); c(copt); epush; lsw(0); c(swap); lsw(1);
    END;
  END;
END EvalVal;

PROCEDURE LeftAdr(VAR I:Info; VAR ob:Dexpr);
BEGIN
  IF ob.emd=var THEN
    IF I.darea=1    THEN  -- Param after L
      llw(I.offset); ob.wd:=-2;
    ELSIF I.darea=2 THEN  -- Param before L

    ELSIF I.darea=3 THEN  -- Temporary var.
      lla(I.offset); ob.wd:=-2;
    ELSIF I.darea>3 THEN  -- address of var.
      llw(I.darea);  -- base address
      IF I.offset>255 THEN   -- index addressing
        li(I.offset);
        IF I.lenel=1 THEN ob.wd:=-1;
        ELSE      c(ADD); ob.wd:=-2; epop;
        END;
      ELSE   -- offset<= 255
        IF I.lenel=1 THEN  ob.wd:=I.offset;
        ELSE
          lsa(I.offset);
          ob.wd:=-2;
        END;
      END;
    END;
  ELSIF ob.emd=arrel THEN
    IF INTEGER(ob.wd)>255 THEN   -- index addressing
        li(ob.wd);
        IF I.lenel=1 THEN ob.wd:=-1;
        ELSE      c(ADD); ob.wd:=-2; epop;
        END;
    ELSIF INTEGER(ob.wd)=-1 THEN
        IF I.lenel=1 THEN
        ELSE  c(ADD); ob.wd:=-2; epop;
        END;
    ELSE  -- 0<=ob.wd<=255
        IF I.lenel=1 THEN
        ELSE lsa(ob.wd); ob.wd:=-2;
        END;
    END;
  END;
END LeftAdr;

PROCEDURE StoreIn(VAR ob,obr:Dexpr);
 VAR t: INTEGER;
BEGIN
  IF ob.tp=Char THEN
     CallExt(ForLib,ChAssign); epop; epop;
  ELSIF obr.tp=Holl THEN
--    IF (ob.tp=Double) OR (ob.tp=Complex) THEN
    IF (ob.tp=Complex) THEN
      IF obr.name<=4 THEN
        AllocTemp(t,2); lsw(0); slw(t);
                        li(20202020h); slw(t+1);
                        lla(t); li(2); c(Move); epop; epop; epop;
        FreeTemp(t,2);
      ELSE li(2); c(Move); epop; epop; epop;
      END;
    ELSE  -- Int Real Logical  and temp Double
      lsw(0);
      IF INTEGER(ob.wd)>=0 THEN
        ssw(ob.wd);
      ELSIF INTEGER(ob.wd)=-1 THEN
        c(SXW); epop; epop; epop;
      ELSE
        ssw(0);
      END;
    END;
  ELSE
    IF INTEGER(ob.wd)>=0 THEN
      ssw(ob.wd);
    ELSIF INTEGER(ob.wd)=-1 THEN
      c(SXW); epop; epop; epop;
    ELSE
      IF ob.tp=Double THEN
         ssw(0);
(*
         AllocTemp(t,2); slw(t+1); slw(t);
         lla(t); li(2); c(Move); epop; epop; epop;
         FreeTemp(t,2);
*)
      ELSIF ob.tp=Complex THEN
         AllocTemp(t,2); slw(t+1); slw(t);
         lla(t); li(2); c(Move); epop; epop; epop;
         FreeTemp(t,2);
      ELSE
        ssw(0);
      END;
    END;
  END;
END StoreIn;

PROCEDURE LoadVal(VAR I:Info);
BEGIN MarkC;
  IF I.darea=1    THEN     -- Param after L
    llw(I.offset);
    IF I.lenel=1 THEN lsw(0);
    ELSE c(copt); epush; lsw(0); c(swap); lsw(1);
    END;
  ELSIF I.darea=2 THEN     -- Param before L

  ELSIF I.darea=3 THEN     -- Temporary var.
    IF I.lenel=1 THEN llw(I.offset);
    ELSE llw(I.offset); llw(I.offset+1);
    END;
  ELSIF I.darea>3 THEN     -- address of var.
    llw(I.darea);          -- base address
    IF I.offset>255 THEN   -- index addressing
      li(I.offset);
      IF I.lenel=1 THEN c(LXW); epop;
      ELSE c(ADD); c(copt); lsw(0); c(swap); lsw(1);
      END;
    ELSE                   -- stack addressing
      IF I.lenel=1 THEN lsw(I.offset);
      ELSE
        IF I.offset>0 THEN c1(LSA,I.offset); END;
        c(copt); epush; lsw(0); c(swap); lsw(1);
      END;
    END;
  END;
END LoadVal;

PROCEDURE LoadAdr(VAR I:Info);
 VAR  md:Mode;
BEGIN
  md:=GetMode(I.name);
  IF md=Var THEN
    IF I.darea=1    THEN  -- Param after L
      llw(I.offset);
    ELSIF I.darea=2 THEN  -- Param before L

    ELSIF I.darea=3 THEN  -- Temporary var.
      lla(I.offset);
    ELSIF I.darea>3 THEN  -- address of var.
      llw(I.darea);  -- base address
      IF I.offset>255 THEN   -- index addressing
        li(I.offset); c(ADD); epop;
      ELSIF I.offset>0 THEN
--        c1(LSA,I.offset);
        lsa(I.offset);
      END;
    END;
  ELSIF md=Array THEN
    IF I.darea=1    THEN  -- Param after L
      llw(I.offset);
    ELSIF I.darea=2 THEN  -- Param before L

    ELSIF I.darea=3 THEN  -- Temporary array
      lla(I.offset);
    ELSIF I.darea>3 THEN  -- address of array
      llw(I.darea);  -- base address
    END;
  ELSIF md=Proc THEN
    IF I.darea=1    THEN  -- Param after L
      llw(I.offset);
    ELSIF I.darea=2 THEN  -- Param before L

    END;
  END;
END LoadAdr;

PROCEDURE StoreInVar(VAR I:Info);
 VAR  md:Mode;
BEGIN
  md:=GetMode(I.name);
  IF I.darea=1    THEN  -- Param after L
    ssw(0);
  ELSIF I.darea=2 THEN  -- Param before L

  ELSIF I.darea=3 THEN  -- Temporary var.
    ssw(0);
  ELSIF I.darea>3 THEN  -- mapped var.
    IF I.offset>255 THEN   -- index addressing
      c(SXW); epop; epop; epop;
    ELSE ssw(I.offset);
    END;
  END;
END StoreInVar;

PROCEDURE gDovarA(VAR I:Info);
BEGIN
  IF I.darea=1    THEN  -- Param after L
    llw(I.offset);
  ELSIF I.darea=2 THEN  -- Param before L

  ELSIF I.darea=3 THEN  -- Temporary var.
      llw(I.offset);
  ELSIF I.darea>3 THEN  -- Local or Global var.
      llw(I.darea);  -- base address
      IF I.offset>255 THEN   -- index addressing
        li(I.offset); c(ADD); epop;
      ELSE  lsa(I.offset);
      END;
      AllocTemp(I.offset,1);
      c(copt); epush; slw(I.offset);
      I.darea:=3
  END;
END gDovarA;

PROCEDURE gDovarV(VAR I:Info);
BEGIN
  llw(I.offset); lsw(0);
END gDovarV;

PROCEDURE gDovarF(VAR I:Info);
BEGIN
  IF I.darea=3 THEN FreeTemp(I.offset,1); END;
END gDovarF;

PROCEDURE InsertC(VAR ob:Dexpr; cp0:INTEGER);
 VAR  insert:BOOLEAN; D:INTEGER;
BEGIN
  IF cp0=cp THEN insert:=FALSE; ELSE insert:=TRUE END;
  ob.start:=cp0;
  ob.emd:=expr;
  -- load constant --
  IF insert THEN InsertMode(cp0); END;
  CASE ob.tp OF
    Int,Real,Logic: li(ob.name);
   |Double:         li(ob.name); -- li(ob.wd);
   |Complex:        li(ob.name); li(ob.wd);
   |Char:           AllocDesc(D); li(ob.name); slw(D+2);
                    li(0); slw(D+1);
                    lsta(ob.wd);
                    slw(D); lla(D); ob.wd:=D;
   |Holl:           lsta(ob.wd);
  ELSE
  END;
  IF insert THEN InsertEnd; END;
END InsertC;

PROCEDURE LoadConst(VAR ob:Dexpr);
BEGIN
  InsertC(ob,cp);
END LoadConst;

PROCEDURE CompCstr(ofs0,ofs1:INTEGER):INTEGER;
VAR p0,p1: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  p0:=ADR(STP)+ofs0;
  p1:=ADR(STP)+ofs1;
  IF p0<p1 THEN RETURN -1
  ELSIF p0>p1 THEN RETURN +1
  ELSE RETURN 0
  END;
END CompCstr;

CONST xx=77;
PROCEDURE cCat(L,R:Dexpr);
VAR j,i:INTEGER;
BEGIN
  Slen:=L.name + R.name -1;
  IF Slen>= HIGH(Sval) THEN
    Error(xx); L.emd:=invmd; RETURN
  END;
  j:=INTEGER(L.wd);
  i:=0;
  WHILE i<L.name DO
    Sval[i]:=STP[j]; INC(i); INC(j);
  END;
  j:=INTEGER(R.wd);
  WHILE i<Slen  DO
    Sval[i]:=STP[j]; INC(i); INC(j);
  END;
  REPEAT Sval[i]:=eol; INC(i);
  UNTIL (i  MOD 4) = 0;
  L.name:=Slen;
  L.wd:=InPoolStr(Sval);
END cCat;

PROCEDURE genCxCmp(eq:BOOLEAN);
VAR t: INTEGER;
BEGIN
  AllocTemp(t,1);
   c(swap); slw(t);
   c(FCMP); c(EQU); epop;
   c(swap); llw(t);
   c(FCMP); c(EQU); epop;
   c(ADD);  epop; li(2);
   IF eq THEN c(EQU) ELSE c(NEQ) END; epop;
  FreeTemp(t,1);
END genCxCmp;

PROCEDURE genCxAdd(add:BOOLEAN);
VAR t: INTEGER;
BEGIN
  AllocTemp(t,1);
   c(swap); slw(t);
   IF add THEN c(FADD) ELSE c(FSUB) END; epop;
   c(swap); llw(t);
   IF add THEN c(FADD) ELSE c(FSUB) END; epop;
   c(swap);
  FreeTemp(t,1);
END genCxAdd;

PROCEDURE genCxMul(mul:BOOLEAN);
BEGIN
  IF mul THEN CallExt(ForLib,CxMul);
         ELSE CallExt(ForLib,CxDiv);
  END; epop; epop;
END genCxMul;

END fcInd.
