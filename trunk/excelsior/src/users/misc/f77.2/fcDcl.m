IMPLEMENTATION MODULE fcDcl;

FROM SYSTEM  IMPORT WORD,ADDRESS, ADR;
FROM fcScan  IMPORT Symbol,  Work, sy, Label,  GetSy, MarkPos,
                    Ival, BackUp, NewLine, SetWork, Error, Expected,
                    ErrorId, Fault, MarkPos1, GetLetter, cType, CxConst,
                    BackUp1,SkipTo, Rval, Sval, Slen, lookAhead;
FROM fcObj   IMPORT Default, GenObj, Unpack, DelObj, DelId, SetType,
                    GetType, SetMode, GetMode, Mode,
                    SaveO, RestoreO, Pack, idno,Info, Ident, IdStr,
                    GetDefType, Types, Class, PchImpl, Idname,
                    GenLocal,tpbit,indbit ,databit, SetImplType;
FROM fcHeap  IMPORT Give, Free;
FROM fcExpr  IMPORT Emode, Dexpr, icExpr, cExpr, bind, bIndex,
                    Expr, SetFN;
FROM fcProc  IMPORT LookIntr;
FROM fcStm   IMPORT AssTypeConv, PutLabel;
FROM fcDefs  IMPORT dplen,cxlen, maxpar, maxSlen;
FROM fcGen   IMPORT InPoolStr;
FROM fcInOut IMPORT Format;
FROM objFile IMPORT pProg, pBData, pSubr, pSubra, pFunc, pExternal;
FROM fcTproc IMPORT TableProc;

FROM StdIO   IMPORT print;


TYPE TempName = RECORD
                  name:INTEGER;
                  dim:INTEGER;
                  Index:ARRAY[1..7] OF INTEGER;
                  pos1,pos2:INTEGER;
                END;

VAR LenCh:INTEGER;
    Pimpl : PchImpl;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE size(VAR I:Info):INTEGER;
  VAR dp:POINTER TO ARRAY [0..21] OF INTEGER;
      len:INTEGER;
BEGIN
  IF GetMode(I.name)=Array THEN
    dp:=I.desc;
    len:= I.lenel*dp^[I.dim*3]
  ELSE len:=I.lenel
  END;
  RETURN len
END size;

PROCEDURE AllocVar(VAR I:Info);
BEGIN
  SetMode(I.name,Var);
  IF I.cl#Param THEN
    I.darea:=Mapoffs;
    IF GetType(I.name)=Char THEN
      I.offset:=Loffs*4;
      INC(Loffs,(size(I)+3) DIV 4);
    ELSE
      I.offset:=Loffs;
      INC(Loffs,size(I));
    END;
  END;
  Pack(I);
END AllocVar;

PROCEDURE AllocExt(VAR I:Info);
BEGIN
  SetMode(I.name,Proc);
  IF I.cl#Param THEN
    I.darea:=Mapoffs;
    I.cl:=Ext;
  END;
  IF GetType(I.name)=Char THEN
    IF I.lenel=0 THEN Error(5) END;
  END;
  Pack(I);
END AllocExt;

PROCEDURE AllocIntr(VAR I:Info);
BEGIN
  SetMode(I.name,Proc);
  I.cl:=Intr;
  Pack(I);
END AllocIntr;

PROCEDURE LenDescr(VAR len:INTEGER):BOOLEAN;
(*   n      --> len:=n
    (cexpr) --> len:=Ival
    ( star )    --> len:=0
    ELSE    --> LenDescr:=FALSE
*)
VAR res:BOOLEAN; tp:Types; ob:Dexpr;
BEGIN
  len:=0; res:=TRUE; GetSy;
  IF sy=const THEN
    IF cType=Int THEN len:=Ival ELSE res:=FALSE END;
  ELSIF sy=lpar THEN
    GetSy;
    IF sy=times THEN GetSy;
    ELSE icExpr(ob);
      IF ob.emd=cexpr THEN
        len:=ob.name
      ELSE res:=FALSE
      END;
    END;
    IF sy#rpar THEN Expected(rpar) END;
  ELSE res:=FALSE
  END;
  IF NOT res THEN Error(5)
  ELSIF (len<0) OR (len>maxSlen) THEN Error(20)
  END;
  RETURN res;
END LenDescr;

PROCEDURE External;
VAR I:Info; md:Mode; len:INTEGER; tp: Types;
BEGIN
  PutLabel(Label,-1);
  REPEAT
    GetSy; IF sy#id THEN Error(25); RETURN END;
    md:=GetMode(idno); I.name:=idno;
    IF md=Empty THEN
      GenLocal(I);
      SetMode(idno,Proc);
      I.cl:=Ext;
      GenObj(I);
    ELSIF md=xVar THEN
      Unpack(I);
      IF I.cl=Param THEN SetMode(idno,Proc)
      ELSIF I.cl=Local THEN
        SetMode(idno,Proc); I.cl:=Ext; Pack(I);
      ELSE ErrorId(1)
      END;
    ELSE ErrorId(1)
    END;
    GetSy
  UNTIL sy#comma;
  IF sy#EOL THEN Error(16) END
END External;

PROCEDURE Intrinsic;
VAR no:INTEGER; I:Info;
BEGIN
  PutLabel(Label,-1);
  REPEAT
    GetSy;
    IF sy=id THEN
      IF GetMode(idno)#Empty THEN
        ErrorId(1)
      ELSE
      -- поиск в таблице стандартных функций
        no:=LookIntr(Ident);
        IF no=0 THEN ErrorId(5)
        ELSE I.name:=idno; I.darea:=0; I.lenel:=1;
             I.cl:=Intr; I.bits:={}; I.offset:=no; I.commP:=0;
             SetMode(idno,Proc); SetType(idno,Undef);
             GenObj(I);
        END;
      END;
    ELSE Error(25); RETURN
    END;
    GetSy;
  UNTIL sy#comma;
  IF sy#EOL THEN Error(16); END
END Intrinsic;

PROCEDURE Save;
BEGIN
  PutLabel(Label,-1);
MarkPos1; GetSy;
IF sy=EOL THEN RETURN END;
BackUp1;
  REPEAT GetSy;
    IF sy=id THEN
      IF GetMode(idno)=Empty THEN ErrorId(9); DelId(idno) END;
    ELSIF sy=slash THEN
      GetSy; IF sy#id THEN Error(25); RETURN END;
      IF GetMode(idno)=Empty THEN DelId(idno) END;
      GetSy; IF sy#slash THEN Expected(slash) END;
    ELSE Error(25)
    END;
    GetSy;
  UNTIL sy#comma;
  IF sy#EOL THEN Error(16) END
END Save;

PROCEDURE ArrDcl(VAR I:Info):BOOLEAN;
  VAR lo,hi,d:INTEGER;  dim:INTEGER;
      cpart,abs:INTEGER; p:ADDRESS;
      bi:bind; bou:ALHD;
      md:Mode; param,last:BOOLEAN;
      i : INTEGER;
(* FALSE, если разбор был удачным. *)
BEGIN
  md:=GetMode(I.name);
  IF (md=Proc) OR (md=Const) OR (md=Array) THEN
     ErrorId(1); SkipTo(rpar); RETURN TRUE
  END;
  IF md=Empty THEN param:=FALSE
  ELSE param:=I.cl=Param;
       IF indbit IN I.bits THEN ErrorId(7) END;
       IF databit IN I.bits THEN Error(21) END;
       IF (I.cl=Func) OR (I.cl=Subr) THEN ErrorId(10) END;
  END;
  dim:=0; last:=FALSE; I.farray:=FALSE;
  REPEAT
    GetSy; IF last THEN Error(35) END;
    IF sy=times THEN
       IF param THEN lo:=1; d:=0;
       ELSE idno:=I.name; ErrorId(8); lo:=1; d:=0;
       END; last:=TRUE;
    ELSE
      IF bIndex(bi) THEN lo:=1; d:=1;
      ELSE
        IF bi.len=0 THEN lo:=bi.val; d:=1;
        ELSIF NOT param THEN lo:=1; d:=1;
          idno:=I.name; ErrorId(8);
        ELSE I.farray:=TRUE; d:=-1;
          Give(p,bi.len); MOVE(p,ADDRESS(bi.val),bi.len);
          lo:=INTEGER(p);
        END;
      END;
    END; -- lo index end
    IF sy#col THEN
      hi:=lo; lo:=1;
      IF d=-1 THEN d:=-2
      ELSIF d=1 THEN d:=hi-lo+1
      END;
    ELSE
      GetSy; IF last THEN Error(35) END;
      IF sy=times THEN
         IF NOT param THEN idno:=I.name; ErrorId(8) END;
         last:=TRUE; IF d=1 THEN d:=0 END;
      ELSE
        IF bIndex(bi) THEN hi:=lo;
           IF d=-1 THEN d:=-4 END;
        ELSE
          IF bi.len=0 THEN hi:=bi.val;
             IF d=-1 THEN d:=-3;
             ELSIF d=1 THEN d:=hi-lo+1;
             END;
          ELSIF NOT param THEN hi:=lo;
            idno:=I.name; ErrorId(8);
          ELSE I.farray:=TRUE;
            Give(p,bi.len); MOVE(p,ADDRESS(bi.val),bi.len);
            hi:=INTEGER(p);
            IF d=1 THEN d:=-2
            ELSIF d=0 THEN lo:=hi; hi:=1; d:=-1;
            ELSIF d=-1 THEN d:=-4;
            END;
          END;
        END;
      END;
    END;  -- hi index end
    INC(dim); IF dim>7 THEN Error(28); dim:=7; END;
    bou[dim-1].lo:=lo; bou[dim-1].hi:=hi; bou[dim-1].d:=d;
  UNTIL sy#comma;
  IF sy#rpar THEN Expected(rpar) END;
  abs:=1; cpart:=0;
  IF NOT I.farray THEN
    FOR i:=0 TO dim-1 DO
      d:=bou[i].d;
      cpart:=cpart+bou[i].lo*abs;
      abs:=abs*d; bou[i].d:=abs;
    END;
  END;
  lo:=dim*3+1; Give(p,lo);
  I.dim:=dim; I.desc:=p;
  p^:=cpart; p:=p+1;
  FOR i:=0 TO dim-1 DO
    p^:=bou[i].lo; p:=p+1;
    p^:=bou[i].hi; p:=p+1;
    p^:=bou[i].d;  p:=p+1;
  END;
  IF md#Empty THEN DelObj(I.name); END;
  SetMode(I.name,Array); GenObj(I);
  RETURN FALSE;
END ArrDcl;

PROCEDURE TypeDcl(tp:Types);
  VAR A,len:INTEGER; I:Info; md:Mode;
BEGIN
  PutLabel(Label,-1);
  REPEAT
    GetSy; A:=idno; I.name:=idno;
    IF sy#id THEN Error(25); RETURN END;
    md:=GetMode(A);
    SetType(A,tp);
    IF md=Empty THEN
      I.darea:=0; I.lenel:=1; I.cl:=Local; I.bits:={tpbit};
      I.offset:=0; I.commP:=0; I.equiP:=0;
    ELSE
      Unpack(I);
      IF tpbit IN I.bits THEN ErrorId(1) END;
      INCL(I.bits,tpbit);
      IF md=Proc THEN
        IF I.cl=Intr THEN Error(52) END;
      ELSIF md=Var THEN
        IF I.cl=Subr THEN Error(52) END;
      ELSIF md=Const THEN Error(21)
      END;
    END;
    GetSy;
    IF sy=lpar THEN  -- описатель массива
       IF ArrDcl(I) THEN  END;
       GetSy;
    ELSE
      IF md=Empty THEN SetMode(A,xVar); GenObj(I); END;
    END;
    len:=1;
    IF tp=Char THEN
       IF sy=times THEN
          IF NOT LenDescr(len) THEN RETURN END;
          GetSy;
       ELSE len:=LenCh
       END;
    ELSIF tp=Double THEN len:=dplen
    ELSIF tp=Complex THEN len:=cxlen
    END;
    I.lenel:=len; Pack(I);
  UNTIL sy#comma;
  IF sy#EOL THEN Error(16) END
END TypeDcl;

PROCEDURE FindBlock(name:Idname):PBlock;
VAR p:PBlock; nofind:BOOLEAN;
BEGIN
  p:=PlBlock; nofind:=TRUE;
  WHILE (p#NIL) AND nofind DO
     IF p^.name=name THEN nofind:=FALSE
     ELSE p:=p^.next
     END
  END;
  RETURN p
END FindBlock;

PROCEDURE Common;
VAR A,len: INTEGER; md: Mode; cl: Class; tp: Types;
    P:PBlock; I,I1: Info; Bname:Idname;
BEGIN
  PutLabel(Label,-1);
  GetSy;
  LOOP
    IF sy=slash THEN GetSy;
      IF sy=id THEN
         Bname:=Ident;
         IF GetMode(idno)=Empty THEN DelId(idno) END;
         GetSy;
      ELSE Bname:='BLANK!'
      END;
      IF sy#slash THEN Expected(slash) ELSE GetSy END;
    ELSE Bname:='BLANK!'
    END;
    P:=FindBlock(Bname);
    IF P=NIL THEN   -- создать блок
       Give(P,SIZE(Block));
       P^.name:=Bname; P^.da:=0;
       P^.FP:=-1; P^.LP:=-1; P^.len:=0;
       P^.next:=PlBlock; PlBlock:=P
    END;
    LOOP
      IF sy#id THEN Error(25); RETURN END;
      -- обработка элемента блока
      A:=idno; md:=GetMode(A);
      I.name:=idno;
      IF md=Empty THEN
         GenLocal(I);
         SetMode(A,Var);
         I.cl:=Global;
         GenObj(I);
      ELSIF (md=Proc) OR (md=Const) THEN
         ErrorId(1); RETURN
      ELSE Unpack(I);
        IF I.cl#Local THEN ErrorId(1); RETURN END;
        I.cl:=Global; Pack(I);
        IF md=xVar THEN SetMode(idno,Var); END;
      END;
      GetSy;
      IF sy=lpar THEN
        IF ArrDcl(I) THEN RETURN END;
        Unpack(I); GetSy;
      END;
      --  включить элемент в цепочку
      IF INTEGER(P^.FP)=-1 THEN  -- первый элемент
         P^.FP:=A;
      ELSE
        I1.name:=P^.LP; Unpack(I1);
        I1.commP:=A; Pack(I1)
      END;
      I.commP:=P^.FP; Pack(I); P^.LP:=A;
      -- анализ продолжения оператора
      IF sy=comma THEN
         GetSy; IF sy=slash THEN EXIT END;
      ELSIF sy=EOL THEN RETURN
      ELSIF sy=slash THEN EXIT
      ELSE  Expected(comma)
      END
    END; -- loop1
  END; -- loop
END Common;

PROCEDURE Parsname( VAR T: TempName):BOOLEAN;
(* разбор id | id(c:c) | id(c,c,..) | id(c,c..)(c:c) *)
VAR dim:INTEGER; ob:Dexpr; res:BOOLEAN;
BEGIN
  IF sy#id THEN Error(25); RETURN FALSE END;
  T.name:=idno; T.dim:=0; T.pos1:=0; T.pos2:=0; dim:=0;
  GetSy; res:=TRUE;
  IF sy=lpar THEN GetSy;
    IF sy=col THEN
      T.pos1:=1; GetSy;
      IF sy=rpar THEN T.pos2:=0;  -- id(:)
      ELSE
        icExpr(ob);
        IF ob.emd#cexpr THEN res:=FALSE;  -- error
        ELSE T.pos2:=ob.name;
        END;
        IF sy#rpar THEN Expected(rpar) END;
        GetSy;
      END;
    ELSE
      icExpr(ob);
      IF ob.emd#cexpr THEN res:=FALSE;  -- error
      ELSE T.pos1:=ob.name;
      END;
      IF sy=col THEN
        GetSy;
        IF sy=rpar THEN T.pos2:=0;  -- id(c:)
        ELSE icExpr(ob);
          IF ob.emd#cexpr THEN  res:=FALSE;
          ELSE T.pos2:=ob.name;
          END;
        END;
        IF sy#rpar THEN Expected(rpar) END;
        GetSy;   -- return id(c:c)
      ELSE  -- id(c,...
        INC(dim);
        T.Index[dim]:=T.pos1; T.pos1:=0;
        WHILE sy=comma DO
          GetSy; icExpr(ob);
          IF ob.emd#cexpr THEN res:=FALSE; ob.name:=1 END;
          INC(dim); T.Index[dim]:=ob.name;
        END;  -- while  id(c,c,...)
        T.dim:=dim;
        IF sy#rpar THEN Expected(rpar) END;
        GetSy;
        IF sy=lpar THEN  GetSy;
          IF sy=col THEN
            T.pos1:=1; GetSy;
            IF sy=rpar THEN T.pos2:=0;  -- id(c,c...)(:)
            ELSE
              icExpr(ob);
              IF ob.emd#cexpr THEN res:=FALSE; -- error
              ELSE T.pos2:=ob.name;
              END;
              IF sy#rpar THEN Expected(rpar) END;
              GetSy;
            END;                       -- id(c,c,...)(:c)
          ELSE
            icExpr(ob);
            IF ob.emd#cexpr THEN res:=FALSE;  -- error
            ELSE T.pos1:=ob.name;
            END;
            IF sy=col THEN
              GetSy;
              IF sy=rpar THEN T.pos2:=0;  -- id(c,...)(c:)
              ELSE icExpr(ob);
                IF ob.emd#cexpr THEN res:=FALSE;
                ELSE T.pos2:=ob.name;
                END;
              END;
              IF sy#rpar THEN Expected(rpar) END;
              GetSy;
            ELSE Expected(col); res:=FALSE;
            END;
          END;
        END;  -- id(c,c...,c)
      END;
    END;
  END;
  RETURN res;
END Parsname;

PROCEDURE Equivalence;
VAR PList,p:PEquiObj; T:TempName; n:INTEGER;
    AP:POINTER TO ARRAY[1..7] OF INTEGER; ps:PEquiStr;
    k:INTEGER; I:Info; md:Mode;
BEGIN
  PutLabel(Label,-1);
  REPEAT
    PList:=NIL; GetSy;
    IF sy#lpar THEN Expected(lpar) END;
    sy:=comma; n:=0;
    WHILE sy=comma DO
      INC(n); GetSy;
      IF Parsname(T) THEN
      -- проверить эл. таблицы
        I.name:=T.name; md:=GetMode(I.name); idno:=T.name;
        IF (md=Proc) OR (md=Const) THEN
            ErrorId(3);  RETURN
        ELSIF md=Empty THEN
          IdStr(I.name,Ident); GenLocal(I); md:=Var;
          SetMode(I.name,Var); GenObj(I);
        ELSE Unpack(I);
        END;
        IF (I.cl#Local) AND (I.cl#Global) THEN
          ErrorId(3); RETURN
        END;
        IF md=xVar THEN md:=Var; SetMode(I.name,Var); END;
        IF md=Array THEN
          IF (T.dim>0) AND (T.dim#I.dim) THEN Error(32) END;
        END;
        Give(p,SIZE(EquiObj));
        p^.name:=T.name; p^.dim:=T.dim; p^.pos:=T.pos1;
        p^.next:=PList; PList:=p;
        IF T.dim > 0 THEN
          Give(AP,T.dim);
          FOR k:=1 TO T.dim DO
            AP^[k]:=T.Index[k]
          END; p^.index:=AP;
        ELSE p^.index:=NIL;
        END;
      ELSE RETURN
      END; --if
    END; --while
    IF n<2 THEN Error(50) END;
    IF sy#rpar THEN Expected(rpar) END;
    Give(ps,2); ps^.list:=PList;
    ps^.next:=PEquiv; PEquiv:=ps;
    GetSy;
  UNTIL sy#comma;
  IF sy#EOL THEN Error(16) END
END Equivalence;

PROCEDURE implDo():BOOLEAN;
VAR I:Info; dim:INTEGER; md:Mode; notfind:BOOLEAN;
    Index: ARRAY  [0..7] OF INTEGER;
    mdInd: ARRAY  [0..7] OF Symbol;
    I0,Ni,Byi,i:INTEGER; ob:Dexpr;
BEGIN
  LOOP GetSy;
    IF sy=lpar THEN
      IF NOT implDo() THEN RETURN FALSE END;
    ELSE
      IF sy#id THEN Error(25); RETURN FALSE END;
      I.name:=idno; md:=GetMode(idno);
      IF md=Array THEN
        GetSy; IF sy#lpar THEN Expected(lpar); RETURN FALSE END;
        dim:=0; Unpack(I);
        REPEAT GetSy;
          IF sy=id THEN
            Index[dim]:=idno; mdInd[dim]:=id;
          ELSIF sy=minus THEN GetSy;
            IF sy#const THEN Error(14); RETURN FALSE END;
            IF cType#Int THEN Error(27); RETURN FALSE END;
            Index[dim]:=-Ival; mdInd[dim]:=const;
          ELSIF sy=const THEN
            IF cType#Int THEN Error(27); RETURN FALSE END;
            Index[dim]:=Ival; mdInd[dim]:=const;
          ELSE Error(14); RETURN FALSE
          END;
          GetSy; INC(dim); IF dim>6 THEN dim:=7 END;
        UNTIL sy#comma;
        IF sy#rpar THEN Expected(rpar); RETURN FALSE END;
        IF dim#I.dim THEN Error(32); RETURN FALSE END;
      ELSE
        GetSy; IF sy#becomes THEN Expected(becomes);RETURN FALSE END;
        EXIT;
      END;
    END;
    GetSy; IF sy#comma THEN Expected(comma); RETURN FALSE END;
  END; (* loop *)
  (* I.name=idno; sy="="; *)
  notfind:=TRUE;
--  WHILE notfind AND (dim>0) DO
--   DEC(dim); IF mdInd[dim]=id THEN notfind:=Index[dim]#I.name END;
--  END;
--  IF notfind THEN RETURN FALSE END;
  GetSy; icExpr(ob); I0:=ob.name;
  IF ob.emd=invmd THEN RETURN FALSE END;
  IF sy#comma THEN Expected(comma); RETURN FALSE; END;
  GetSy; icExpr(ob); Ni:=ob.name;
  IF ob.emd=invmd THEN RETURN FALSE END;
  IF sy=comma THEN (* By exist  *)
    GetSy; icExpr(ob); Byi:=ob.name;
    IF ob.emd=invmd THEN RETURN FALSE END;
  ELSE Byi:=1;
  END;
  IF sy#rpar THEN Expected(rpar);RETURN FALSE END;
  RETURN TRUE
END implDo;

PROCEDURE Data;
VAR names,consts:INTEGER; p:PDataStr;
    T:TempName; svsy:Symbol; offset,nel,rep:INTEGER;
    pn,Pn:PDataNames; pc,Pc:PDataConst;
    I:Info; md:Mode; op:Symbol;
    pp:ADDRESS; lo,hi,d,i,k:INTEGER;
BEGIN
  PutLabel(Label,-1);
  LOOP
    names:=0; Give(p,3);
    p^.next:=DataList; DataList:=p;
    p^.names:=NIL; p^.consts:=NIL;
    Pn:=ADDRESS(DataList)+1; Pc:=ADDRESS(DataList)+2;
  -- список имен
    REPEAT
      GetSy;
      IF sy=id THEN
        IF Parsname(T) THEN
          I.name:=T.name; md:=GetMode(I.name); idno:=T.name;
          IF (md=Proc) OR (md=Const) THEN
              ErrorId(3);  RETURN
          ELSIF md=Empty THEN
            IdStr(I.name,Ident); GenLocal(I); md:=xVar;
            GenObj(I);
          END;
          Unpack(I);
          IF (I.cl#Local) AND (I.cl#Global) THEN ErrorId(3) END;
          IF md=xVar THEN md:=Var; SetMode(I.name,Var); END;
          INCL(I.bits,databit); Pack(I);
          IF T.pos1#0 THEN
            IF GetType(I.name)#Char THEN
               ErrorId(6); T.pos1:=0;
            ELSE
              IF (T.pos1>I.lenel) OR (T.pos2>I.lenel) THEN
                Error(23)
              END;
            END;
          END;
          IF md=Var THEN
            IF T.dim#0 THEN ErrorId(11); END;
            nel:=1; offset:=T.pos1;
          ELSE (* Array *)
            pp:=I.desc; offset:=0;
            IF T.dim=0 THEN
               IF T.pos1#0 THEN Error(32) END;
               pp:=pp+I.dim*3; nel:=pp^;
            ELSIF T.dim#I.dim THEN Error(32)
            ELSE  d:=1; pp:=pp+1;
              FOR i:=1 TO T.dim DO
                lo:=pp^; pp:=pp+1; hi:=pp^; pp:=pp+1;
                k:=T.Index[i];
                IF (k<lo) OR (k>hi) THEN Error(23) END;
                offset:=offset+(k-lo)*d;
                d:=pp^; pp:=pp+1;
              END;
              nel:=1;
            END;
          END;
          -- include in list
          Give(pn,4); Pn^.next:=pn; Pn:=pn;
          Pn^.next:=NIL; Pn^.idno:=I.name; Pn^.nel:=nel;
          Pn^.offset:=offset;
        ELSE SkipTo(EOL); RETURN
        END;
      ELSIF sy=lpar THEN
        IF NOT implDo() THEN RETURN END;
        GetSy;
      ELSE Error(25); RETURN
      END;
      INC(names,nel);
    UNTIL sy#comma;
    IF sy#slash THEN Expected(slash) END;
  -- список констант
    consts:=0;
    REPEAT
      GetSy; rep:=1; svsy:=sy;
      IF sy=id THEN I.name:=idno;
      ELSIF sy=lpar THEN
        IF CxConst() THEN svsy:=sy; END;
      ELSIF sy=EOL THEN Error(39); RETURN;
      END;
      GetSy;
      IF sy=times THEN
        IF svsy=const THEN
          rep:=Ival;
          IF cType#Int THEN Error(27) END;
        ELSIF svsy=id THEN
          IF GetMode(I.name)#Const THEN ErrorId(12)
          ELSIF GetType(I.name)#Int THEN Error(27)
          END;
          Unpack(I); rep:=I.offset;
          IF rep<=0 THEN Error(38); rep:=1 END;
        ELSE Error(14);
        END;
        GetSy; svsy:=sy;
        IF sy=id THEN I.name:=idno;
        ELSIF sy=lpar THEN
          IF CxConst() THEN svsy:=sy; END;
        END;
        GetSy;
      END;
      IF (svsy=plus) OR (svsy=minus) THEN
         op:=svsy;
         svsy:=sy;
         IF sy=id THEN I.name:=idno;
         ELSIF sy=lpar THEN
           IF CxConst() THEN svsy:=sy; END;
         END;
         GetSy;
      ELSE op:=slash
      END;
      Give(pc,5); Pc^.next:=pc; Pc:=pc;
      Pc^.next:=NIL; Pc^.nrep:=rep;
      IF svsy=const THEN
        CASE cType OF
         Int:    IF op=minus THEN Ival:=-Ival END;
                 Pc^.val:=Ival;
        |Real:   IF op=minus THEN Rval:=-Rval END;
                 Pc^.val:=WORD(Rval);
        |Double: IF op=minus THEN Rval:=-Rval END;
                 Pc^.val:=WORD(Rval);
        |Complex: IF op=minus THEN
                    Rval:=-Rval; Ival:=INTEGER(-REAL(Ival))
                  END;
                  Pc^.val:=WORD(Rval); Pc^.val1:=WORD(Ival);
        |Logic :  IF op#slash THEN Error(13) END;
                  Pc^.val:=Ival;
        |Char,Holl: IF op#slash THEN Error(13) END;
                  Pc^.val:=Slen; Pc^.val1:=InPoolStr(Sval);
        ELSE
        END;
        Pc^.tp:=cType;
      ELSIF svsy=id THEN
        Unpack(I);
        CASE GetType(I.name) OF
         Int:    IF op=minus THEN Pc^.val:=-I.offset
                 ELSE Pc^.val:=Ival;
                 END;
        |Real:   IF op=minus THEN Pc^.val:=WORD(-REAL(I.offset))
                 ELSE Pc^.val:=WORD(I.offset);
                 END;
        |Double: IF op=minus THEN Pc^.val:=WORD(-REAL(I.offset))
                 ELSE Pc^.val:=WORD(I.offset);
                 END;
        |Complex: IF op=minus THEN
                    Pc^.val:=WORD(-REAL(I.offset));
                    Pc^.val1:=WORD(-REAL(I.commP));
                  ELSE
                    Pc^.val:=WORD(I.offset); Pc^.val1:=WORD(I.commP);
                  END;
        |Logic :  IF op#slash THEN Error(13) END;
                  Pc^.val:=I.offset;
        |Char,Holl: IF op#slash THEN Error(13) END;
                  Pc^.val:=I.lenel; Pc^.val1:=I.offset;
        ELSE
        END;
        Pc^.tp:=GetType(I.name);
      ELSE Error(14)
      END;
      INC(consts,rep);
    UNTIL sy#comma;
    IF sy#slash THEN Expected(slash) END;
    MarkPos1; GetSy;
    IF sy=EOL THEN EXIT
    ELSIF sy#comma THEN BackUp1;
    END;
  END
END Data;

PROCEDURE Implicit;
  VAR  c,c1,c2:CHAR; tp:Types; len:INTEGER; p:PchImpl;
BEGIN
  PutLabel(Label,-1);
  REPEAT
    LOOP
      SetWork(dcl); GetSy;
      IF ORD(sy)>ORD(char) THEN Error(22); SkipTo(comma);
         IF sy=EOL THEN RETURN END;
      ELSE tp:=Types(ORD(sy)); EXIT
      END;
    END;
    SetWork(ass); GetSy;
    IF tp=Char THEN len:=1;
       IF sy=times THEN
          IF LenDescr(len) THEN GetSy;
          ELSE RETURN
          END;
       END;
    END;
    IF sy#lpar THEN Expected(lpar); END;
    REPEAT
      IF GetLetter(c1) THEN GetSy;
        IF (sy=comma) OR (sy=rpar) THEN c2:=c1;
        ELSIF sy=minus THEN
          IF NOT GetLetter(c2) THEN RETURN  END;
          IF ORD(c1)>ORD(c2) THEN Error(23); END;
          GetSy;
        ELSE Error(23); RETURN
        END;
        FOR c:=c1 TO c2 DO
          IF (7 IN BITSET(Default[c])) THEN
            Error(23); RETURN
          END;
          Default[c]:=CHAR(BITSET(tp)+{7});
        END;
        IF tp=Char THEN Give(p,4);
           p^.next:=Pimpl; Pimpl:=p;
           p^.c1:=c1; p^.c2:=c2; p^.len:=len;
        END;
      ELSE RETURN
      END;
    UNTIL sy#comma;
    IF sy#rpar THEN Expected(rpar); END;
    GetSy;
  UNTIL sy#comma;
  SetImplType;
END Implicit;

PROCEDURE Parameter;
  VAR tp:Types; I:Info; obl,ob:Dexpr;
BEGIN
  PutLabel(Label,-1);
  GetSy;
  IF sy#lpar THEN Expected(lpar);
  ELSE
    LOOP GetSy;
      IF sy#id  THEN Error(25); EXIT; END;
      I.name:=idno;
      IF GetMode(idno)=Empty THEN
        GenLocal(I);
        SetMode(idno,Const);
        GenObj(I);
      ELSIF GetMode(idno)=xVar THEN
        Unpack(I);
        IF I.cl=Param THEN ErrorId(1); EXIT; END;
        SetMode(idno,Const);
      ELSE ErrorId(1); EXIT
      END;
      GetSy; IF sy#becomes THEN Expected(becomes); EXIT; END;
      GetSy;  cExpr(ob); obl.tp:=GetType(I.name);
                         obl.emd:=var;
      IF AssTypeConv(obl,ob) THEN RETURN END;
      IF ob.tp=Char THEN
        IF I.lenel=0 THEN I.lenel:=ob.name;
        ELSIF I.lenel> ob.name THEN
         -- add blanks
        END;
        I.offset:=ob.wd;
      ELSE I.offset:=ob.name; I.commP:=ob.wd;
      END;
      Pack(I);
      IF sy=rpar THEN GetSy;
        IF sy#EOL THEN Error(16); END;
        RETURN;
      ELSIF sy#comma THEN Expected(comma); RETURN
      END;
    END; (* loop *)
  END;
END Parameter;

PROCEDURE Dimension;
  VAR len:INTEGER; I:Info; md:Mode; tp:Types;
BEGIN
  PutLabel(Label,-1);
  REPEAT
    GetSy; IF sy#id THEN Error(25); RETURN  END;
    I.name:=idno;
    md:=GetMode(idno);
    IF md=Empty THEN GenLocal(I); SetMode(idno,Empty);
    ELSE             Unpack(I)
    END;
    GetSy; IF sy#lpar THEN Expected(lpar); RETURN; END;
    IF ArrDcl(I) THEN
    END;
    GetSy;
  UNTIL sy#comma;
  IF sy#EOL THEN Error(16) END;
END Dimension;

TYPE Pd=POINTER TO Del;
     Del=RECORD d:INTEGER; next:Pd; END;
VAR noFuncForm:INTEGER;

PROCEDURE LocFuncDcl():BOOLEAN;
  VAR F,i,no,parno:INTEGER; D,p:Pd;  pp:ADDRESS; I:Info;
      obF,ob:Dexpr; rtn:BOOLEAN; tp:Types;
      seq:ARRAY [0..maxpar] OF INTEGER;
(* Возвращает TRUE, если  описание внутренней функции *)
BEGIN
  IF NOT lookAhead("=") THEN RETURN FALSE END;
  GetSy; IF sy#id THEN RETURN FALSE END;
  F:=idno; I.name:=F;
  GetSy; IF sy#lpar THEN RETURN FALSE END;
  IF GetMode(F)=xVar THEN Unpack(I);
    IF I.cl#Local THEN RETURN FALSE END;
  ELSIF GetMode(F)#Empty THEN RETURN FALSE
  END;
  parno:=0; D:=NIL; rtn:=FALSE;
  WHILE sy#rpar DO
    GetSy;
    IF parno>maxpar THEN parno:=maxpar END;
    IF sy#id THEN rtn:=TRUE; sy:=rpar;
    ELSE
      IF GetMode(idno)#Empty THEN
          SaveO(idno);
      ELSE Give(p,SIZE(Del)); p^.d:=idno;
          p^.next:=D; D:=p;
      END;
      seq[parno]:=idno; INC(parno);
      GetSy; IF (sy#rpar) AND (sy#comma) THEN sy:=rpar; rtn:=TRUE; END;
    END;
  END;
  IF NOT rtn THEN GetSy;
     IF sy#becomes THEN rtn:=TRUE; END;
  END;
  IF rtn THEN p:=D;
    WHILE p#NIL DO
      DelId(p^.d);
      DelObj(p^.d); p:=p^.next;
    END;
    RestoreO; RETURN FALSE;
  END;

(* Проверка на единственность параметра *)
  FOR no:=0 TO parno-1 DO
    idno:=seq[no];
    IF idno=F THEN Error(38) END;
    FOR i:=no+1 TO parno-1 DO
      IF idno=seq[i] THEN Error(38); END;
    END;
  END;

  Give(pp,parno*2); i:=pp;
  FOR no:=0 TO parno-1 DO
    I.name:=seq[no];
    IF GetMode(I.name)#Empty THEN
      Unpack(I); tp:=GetType(I.name);
    ELSE IdStr(I.name,Ident);
         GetDefType(Ident[0],tp,I.lenel);
         SetType(I.name,tp);
    END;
    I.cl:=Local; I.bits:={}; I.darea:=0;
    SetMode(I.name,LocVar);
    I.offset:=parno-no+3;
    GenObj(I);
    pp^:=WORD(tp); pp:=pp+1; pp^:=I.lenel; pp:=pp+1;
  END;
  I.name:=F;
  IF GetMode(F)=Empty THEN
    IdStr(I.name,Ident);
    GetDefType(Ident[0],tp,I.lenel);
    SetType(I.name,tp);
  ELSE Unpack(I); DelObj(F);
  END;
  I.cl:=Local; I.bits:={};
  SetMode(F,Proc);
  I.darea:=parno;
  I.offset:=noFuncForm; INC(noFuncForm);
  I.commP:=pp; GenObj(I);
  SetFN(F);   GetSy; Expr(ob);    SetFN(100000);
  obF.emd:=var; obF.name:=F; obF.tp:=GetType(F);
  IF AssTypeConv(obF,ob) THEN
  END;
  IF ob.emd=cexpr THEN
  END;
  p:=D;
  WHILE p#NIL DO
    DelObj(p^.d);
    DelId(p^.d);
    p:=p^.next;
  END;
  PutLabel(Label,-1);
  RestoreO;
  Error(64);
  RETURN TRUE;
END LocFuncDcl;

PROCEDURE Entry;
VAR I:Info; md:Mode; tp:Types;
    Modidno,Parno,Noalt:INTEGER;
    ID:Idname;
BEGIN Parno:=0; Noalt:=0; INC(procno);
  PutLabel(Label,-1); GetSy;
  IF sy#id THEN Error(25); RETURN END;
  I.name:=idno; md:=GetMode(idno); ID:=Ident;
  CASE md OF
   Proc,Var,Const,Array: ErrorId(1); RETURN
   |xVar: Unpack(I); IF self#function THEN ErrorId(1); RETURN END;
   |Empty: GenLocal(I); GenObj(I);
  ELSE ErrorId(1);
  END;
  IF I.cl#Local THEN  ErrorId(1); RETURN END;
  IF self=function THEN
    IF GetType(idno)#GetType(ModIdno) THEN Error(37);RETURN END;
    I.cl:=Func;
  ELSE I.cl:=Subr; SetType(idno,Undef);
  END;
  SetMode(idno,Var); Modidno:=idno; Pack(I);
  GetSy;
  IF sy=EOL THEN
    IF ParNo#0 THEN Error(54) END;
--    pEntry(ID,procno,Parno,Noalt);
    RETURN
  END;
  IF sy#lpar THEN Expected(lpar); END;
  LOOP GetSy;
    IF sy=times THEN
      IF self#subrA THEN Error(56) END;
      INC(Noalt);
    ELSIF sy=id THEN
      INC(Parno); I.name:=idno; md:=GetMode(idno);
      IF md=Empty THEN
        GetDefType(Ident[0],tp,I.lenel);
        I.darea:=0; I.cl:=Param; I.bits:={};
        I.offset:=Parno; SetType(idno,tp);
        SetMode(idno,xVar); GenObj(I);
      ELSE Unpack(I);
        IF md=xVar THEN
          IF I.cl=Local THEN
            I.cl:=Param; I.offset:=Parno; Pack(I);
          ELSIF I.offset#Parno THEN ErrorId(15);
          END;
        ELSIF md=Proc THEN
          IF I.cl=Param THEN
            IF I.offset#Parno THEN ErrorId(15) END;
          ELSIF I.cl=Ext THEN
            I.cl:=Param; I.offset:=Parno; Pack(I)
          ELSE ErrorId(1)
          END;
        ELSIF md=Array THEN
          IF I.cl=Param THEN
            IF I.offset#Parno THEN ErrorId(15) END;
          ELSIF I.cl=Local THEN
            I.cl:=Param; I.offset:=Parno; Pack(I)
          ELSE ErrorId(1)
          END;
        ELSE  ErrorId(1)
        END;
      END;
    ELSE Error(25); RETURN
    END;
    GetSy; IF sy=rpar THEN EXIT; END;
    IF sy#comma THEN Expected(comma); RETURN; END;
  END;
--    pEntry(ID,procno,Parno,Noalt);
  GetSy; IF sy#EOL THEN Error(16); END;
  Error(64);
END Entry;

PROCEDURE GetParamDcl(VAR noalt:INTEGER);
  VAR I:Info; tp:Types;
BEGIN noalt:=0;
  LOOP
    GetSy;
    IF sy=times THEN
      IF self=function THEN
        Error(57)
      ELSE self:=subrA; INC(noalt);
      END;
    ELSIF sy=id THEN
      INC(ParNo); I.name:=idno;
      IF GetMode(idno)#Empty THEN ErrorId(1) END;
      GetDefType(Ident[0],tp,I.lenel);
      I.darea:=0; I.cl:=Param; I.bits:={};
      I.offset:=ParNo; SetType(idno,tp);
      SetMode(idno,xVar); GenObj(I);
    ELSIF (sy=rpar) AND (noalt=0) AND (ParNo=0) THEN EXIT
    ELSE Error(25); RETURN
    END;
    GetSy; IF sy=rpar THEN EXIT; END;
    IF sy#comma THEN Expected(comma); RETURN; END;
  END;
  GetSy; IF sy#EOL THEN Error(16); END;
END GetParamDcl;

PROCEDURE HeaderSubProg(T:Types);
  VAR I:Info; tp:Types; noalt:INTEGER;
BEGIN  PutLabel(Label,-1);
  SetWork(ass); GetSy; IF sy#id THEN Error(25); RETURN END;
  ModIdno:=idno; I.name:=idno; ProcId:=Ident;
  I.darea:=0; I.offset:=0; I.commP:=0; I.equiP:=0;
  IF T=Undef THEN
     GetDefType(Ident[0],tp,I.lenel);
     I.bits:={};
  ELSE
    tp:=T; I.bits:={tpbit}; I.lenel:=1;
    IF tp=Double THEN I.lenel:=dplen
    ELSIF tp=Complex THEN I.lenel:=cxlen
    ELSIF tp=Char THEN I.lenel:=LenCh
    END;
  END;
  IF self=function THEN
    I.cl:=Func; SetType(idno,tp);
  ELSE I.cl:=Subr; SetType(idno,Undef);
  END;
  SetMode(idno,Var);  GenObj(I);
  GetSy;
  IF sy=EOL THEN (* without parameter's *) noalt:=0;
  ELSE
    IF sy#lpar THEN Expected(lpar); RETURN; END;
    GetParamDcl(noalt);
  END;
  CASE self OF
    function:   pFunc(ProcId,procno,ParNo,ORD(tp));
    print(' function %s \n', ProcId);
   |subroutine: pSubr(ProcId,procno,ParNo);
    print(' subroutine %s \n', ProcId);
   |subrA:      pSubra(ProcId,procno,ParNo,noalt);
    print(' subroutineA %s \n', ProcId);
    ELSE
  END;
END HeaderSubProg;

PROCEDURE MainB(noname:BOOLEAN);
BEGIN
  IF noname THEN  Ident:='!Main' END;
    print(' program %s \n', Ident);
  pProg(Ident,procno); ProcId:=Ident;
END MainB;

PROCEDURE BDataB(noname:BOOLEAN);
BEGIN
  IF noname THEN Ident:='!BData' END;
    print(' blockdata %s \n', Ident);
  pBData(Ident,procno); ParNo:=6; -- for temp. vars.
  ProcId:=Ident;
END BDataB;

PROCEDURE InitDcl;
BEGIN
  PlBlock:=NIL; PEquiv:=NIL; DataList:=NIL;
  INC(ProcNo); procno:=ProcNo; ParNo:=0;
  noFuncForm:=1;
END InitDcl;

PROCEDURE declare;
  VAR tp:Types; I:Info;
BEGIN
  InitDcl;
(*  разбор 1-й строки     *)
  NewLine; SetWork(dcl);
    GetSy;
    CASE sy OF
      subr : self:=subroutine;
             HeaderSubProg(Undef); NewLine;
     |func : self:=function;
             HeaderSubProg(Undef); NewLine;
     |prog : self:=main; SetWork(ass); GetSy;
             IF sy=id THEN MainB(FALSE);
               GetSy; IF sy#EOL THEN Error(16) END;
             ELSE
               Error(25);
             END; NewLine;
     |bdata: self:=blockdata; SetWork(ass); GetSy;
             IF sy=id THEN BDataB(FALSE);
               GetSy; IF sy#EOL THEN Error(16) END;
             ELSE BDataB(TRUE);
               IF sy#EOL THEN Error(16); END;
             END; NewLine;
     |int,real,double,complex,log : tp:=Types(ORD(sy)); GetSy;
             IF sy=func THEN  self:=function;
                  HeaderSubProg(tp); NewLine
             ELSE BackUp; self:=main;
             END;
     |char :tp:=Char; GetSy;
            IF sy=times THEN
               IF NOT LenDescr(LenCh) THEN LenCh:=0 END; GetSy;
            ELSE LenCh:=1
            END;
            IF sy=func THEN self:=function;
               HeaderSubProg(tp); NewLine
            ELSE BackUp; self:=main; MainB(TRUE);
            END;
     ELSE BackUp; self:=main; MainB(TRUE);
     END;
(*  разбор операторов описания   *)
     LOOP  SetWork(dcl); GetSy; SetWork(ass);
       CASE sy OF
        int,real,double,complex,log : tp:=Types(ORD(sy)); TypeDcl(tp);
       |char : MarkPos1; GetSy; tp:=Char;
               IF sy=times THEN
                  IF LenDescr(LenCh) THEN MarkPos1; GetSy;
                     IF sy#comma THEN BackUp1; END;
                  ELSE LenCh:=0
                  END;
               ELSE LenCh:=1; BackUp1;
               END;
               TypeDcl(tp);
       |dim     : Dimension;
       |comm    : Common;
       |param   : Parameter;
       |external: External;
                  IF self=blockdata THEN Error(44); END;
       |equi    : Equivalence;
       |impl    : Implicit;
       |data    : Data;
       |intr    : Intrinsic;
                  IF self=blockdata THEN Error(44); END;
       |save    : Save;
       |entry   : Entry;
                  IF (self=blockdata) OR (self=main) THEN Error(44); END;
       |frmt    : Format;
                  IF self=blockdata THEN Error(44); END;
       ELSE BackUp;
            IF lookAhead('=') THEN  EXIT;
            ELSE SetWork(kw); GetSy;
              IF (ORD(sy)>=ORD(call)) AND (ORD(sy)<=ORD(inquire))THEN
                BackUp; SetWork(ass); EXIT;
              END;
            END;  Error(41);
       END; (* case *)
    NewLine;
  END; (* loop *)
  WHILE LocFuncDcl() DO
    IF self=blockdata THEN Error(44); END;
    NewLine;
  END;
  IF self=function THEN
     IF GetType(ModIdno)=Char THEN
       I.name:=ModIdno; Unpack(I);
       INC(ParNo); I.cl:=Param; I.offset:=ParNo;
       Pack(I);
     END;
  END;
(*  обработка таблицы идентификаторов  *)
  TableProc;
  BackUp;
END declare;

BEGIN
END fcDcl.
