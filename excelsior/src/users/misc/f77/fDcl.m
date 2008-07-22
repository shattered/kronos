IMPLEMENTATION MODULE fDcl; (* Max *)

FROM SYSTEM IMPORT WORD;
FROM fScan  IMPORT Symbol, Types, Work, sy, Label, idno, GetSy, MarkPos,
                   Ival, BackUp, NewLine, SetWork, Error, Expected, Info,
                   Default, GenObj, Unpack, DelObj, DelId, SetType, GetType,
                   SetMode, GetMode, ErrorId, Mode, Give, Free, Fault,
                   Filename, MarkPos1, GetLetter, WhatSy?, SaveO, RestoreO,
                   head, Descriptor, D, SetDefaultTp, ParList, PointP, IdStr,
                   Idname, StrId, PointPS;
FROM fGen   IMPORT FinishProc0, sgw, Alloc, MinPS, StartFunc,
                   InitGen, StartProc1, li, c1, c, PutExt;
FROM fExt   IMPORT Forward, PutSubProg, PutParam, WasRealised, ChangeType,
                   SetSubProgList, ExistSubProg;
FROM fExpr  IMPORT Expr, SetFN;

TYPE P =POINTER TO gw;
     gw=RECORD glo:INTEGER; next:P END;

VAR GW:P;

PROCEDURE GiveGW():INTEGER;
  VAR p:P; glo:INTEGER;
BEGIN
  IF GW=NIL THEN glo:=global; INC(global);
  ELSE p:=GW; GW:=p^.next;
    glo:=p^.glo; Free(p,TSIZE(gw));
  END;
  RETURN glo;
END GiveGW;

PROCEDURE FreeGW(glo:INTEGER);
  VAR p:P;
BEGIN
  IF glo=(global-1) THEN DEC(global);
  ELSE Give(p,TSIZE(gw)); p^.glo:=glo;
    p^.next:=GW; GW:=p;
  END;
END FreeGW;

PROCEDURE SubPrograms; (*Вносит подпрограммы в Таблицу Об'ектов*)
  VAR tp:Types; ID:Idname; P:PointPS;
      id:INTEGER; m:Mode; re:BOOLEAN; I:Info;
BEGIN
  SetSubProgList;
  WHILE ExistSubProg(ID,tp,P,re) DO
    id:=StrId(ID);
    IF id#ModIdno THEN
      SetType(id,tp); I.name:=id;
      IF tp=Undef THEN m:=Subr; ELSE m:=Func; END;
      SetMode(id,m);
      I.offset:=PutExt(id); I.parlist:=P; GenObj(I);
    ELSIF re THEN Fault(3);
    END;
  END;
END SubPrograms;

PROCEDURE ArrDcl(A:INTEGER; t:Types; IsParam:BOOLEAN; O:INTEGER):BOOLEAN;
  VAR lo,hi:INTEGER; tp:Types; w:WORD; dim:INTEGER;
      d:Descriptor; dd:D; abs,i:INTEGER; I:Info;
(* t-тип массива, равен Undef, если устанавливается по умолчанию; *)
(* FALSE, если разбор был удачным, A-имя массива *)
BEGIN
  ASSERT(sy=lpar); dim:=0;
  REPEAT
    MarkPos1; GetSy;
    IF NOT Expr(tp,w) THEN Error(24); RETURN TRUE; END;
    IF tp#Int THEN Error(27); RETURN TRUE; END;
    IF sy=col THEN
      lo:=INTEGER(w); MarkPos1; GetSy;
      IF NOT Expr(tp,w) THEN Error(24); RETURN TRUE; END;
      IF tp#Int THEN Error(27); RETURN TRUE; END;
      IF (sy=coma) OR (sy=rpar) THEN hi:=INTEGER(w);
        IF hi<=lo THEN Error(23); RETURN TRUE; END;
      ELSE Expected(rpar); RETURN TRUE; END;
    ELSIF (sy=coma) OR (sy=rpar) THEN lo:=1; hi:=INTEGER(w);
      IF INTEGER(w)<1 THEN Error(23); RETURN TRUE; END;
    ELSE Expected(rpar); RETURN TRUE; END;
    INC(dim); IF dim>3 THEN Error(28); RETURN TRUE; END;
    d[dim-1].lo:=lo; d[dim-1].hi:=hi;
  UNTIL sy=rpar;
  Give(dd,6); abs:=1;
  FOR i:=0 TO dim-1 DO
    dd^[i].lo:=d[i].lo; dd^[i].hi:=d[i].hi;
    abs:=abs*(d[i].hi-d[i].lo+1);
  END;
  IF t#Undef THEN SetType(A,t); END;
  SetMode(A,Array); I.name:=A;
  IF IsParam THEN I.offset:=O;
  ELSE I.offset:=GiveGW();
  END;
  INC(MinPS,abs); Alloc(abs); sgw(I.offset);
  I.dimension:=dim; I.param:=IsParam;
  I.desc:=dd; I.abs:=abs-1; GenObj(I);
  GetSy; RETURN FALSE;
END ArrDcl;

PROCEDURE TypeDcl(tp:Types);
  VAR A:INTEGER; I:Info; Offset:INTEGER;
BEGIN
  LOOP GetSy; A:=idno;
    IF sy#id THEN Error(25); EXIT; END;
    CASE GetMode(A) OF
      Array: SetType(A,tp); GetSy;
     |Var  : IF A=ModIdno THEN SetType(A,tp); ChangeType(tp); GetSy;
               IF self#function THEN Error(52); END;
             ELSE I.name:=A; Unpack(I);
               IF I.param THEN GetSy; SetType(A,tp);
                 IF sy=lpar THEN
                   Offset:=I.offset; DelObj(A);
                    IF ArrDcl(A,tp,TRUE,Offset) THEN EXIT; END;
                 END;
               ELSE ErrorId(1); RETURN; END;
             END;
     |Empty: SetType(A,tp); GetSy;
             IF sy#lpar THEN SetMode(A,Var);
               I.name:=A; I.param:=FALSE;
               I.offset:=GiveGW(); GenObj(I);
             ELSIF ArrDcl(A,tp,FALSE,Offset) THEN EXIT;
             END;
    ELSE (*#Empty*) ErrorId(1); EXIT;
    END; (*case*)
    IF sy=EOL THEN EXIT;
    ELSIF sy#coma THEN Expected(coma); EXIT; END;
  END; (*loop*)
END TypeDcl;

PROCEDURE Implicit;
  VAR b:BOOLEAN; c1,c2:CHAR; tp:Types;
BEGIN
  LOOP SetWork(kw); GetSy;
    IF ORD(sy)>ORD(char) THEN Error(22); EXIT;
    ELSE tp:=Types(ORD(sy)); END; SetWork(ass);
    GetSy; IF sy#lpar THEN Expected(lpar); EXIT; END;
    b:=FALSE;
    REPEAT
      IF sy=rpar THEN b:=TRUE; GetSy;
        IF sy=EOL THEN EXIT;
        ELSIF sy#coma THEN Expected(coma); EXIT;
        END;
      ELSIF GetLetter(c1) THEN GetSy;
        IF sy=coma THEN c2:=c1;
        ELSIF sy=minus THEN
          IF NOT GetLetter(c2) THEN EXIT; END;
          IF ORD(c1)>=ORD(c2) THEN Error(23); END;
          GetSy;
          IF (sy#coma) AND (sy#rpar) THEN Error(23); EXIT; END;
        ELSIF sy=rpar THEN c2:=c1;
        ELSE Error(23); EXIT; END;
        FOR c1:=c1 TO c2 DO Default[c1]:=CHAR(ORD(tp)); END;
        ELSE EXIT;
      END;
    UNTIL b;
  END;
END Implicit;

PROCEDURE Param;
  VAR tp:Types; w:WORD; I:Info;
BEGIN
  GetSy;
  IF sy#lpar THEN Expected(lpar);
  ELSE SetWork(ass);
    LOOP GetSy;
      IF sy#id  THEN Error(25); EXIT; END;
      IF GetMode(idno)=Empty THEN
        SetMode(idno,Const); I.name:=idno;
      ELSIF GetMode(idno)=Var THEN
        I.name:=idno; Unpack(I);
        IF I.param THEN Error(30); EXIT; END;
        FreeGW(I.offset); DelObj(idno);
        SetMode(idno,Const);
        IF idno=ModIdno THEN Fault(2); END;
      ELSE ErrorId(1); END;
      GetSy; IF sy#eqv THEN Expected(eqv); EXIT; END;
      MarkPos1; GetSy;
      IF Expr(tp,w) THEN
        IF GetType(I.name)#tp THEN Error(26); EXIT;
        ELSE I.offset:=INTEGER(w); GenObj(I); END;
      ELSE Error(24); EXIT; END;
      IF sy=rpar THEN GetSy;
        IF sy#EOL THEN Error(16); END;
        RETURN;
      ELSIF sy#coma THEN Expected(coma); RETURN; END;
    END; (* loop *)
    (* Необходимо уничтожить Об'ект *)
    SetMode(I.name,Empty); SetType(I.name,Undef);
    DelId(I.name);
  END;
END Param;

PROCEDURE Dimens;
  VAR A,Offset:INTEGER; I:Info; IsParam:BOOLEAN;
BEGIN
  LOOP
    IsParam:=FALSE;
    GetSy; IF sy#id THEN Error(25); EXIT; END; A:=idno;
    IF GetMode(A)=Var THEN I.name:=A; Unpack(I);
      IF A=ModIdno THEN Fault(1); END;
      IF I.param THEN IsParam:=TRUE;
       Offset:=I.offset; DelObj(A);
      ELSE ErrorId(1); RETURN; END;
    ELSIF GetMode(A)#Empty THEN ErrorId(1); RETURN;
    END;
    GetSy; IF sy#lpar THEN Expected(lpar); EXIT; END;
    IF ArrDcl(A,Undef,IsParam,Offset) THEN EXIT;
    ELSIF sy=EOL THEN EXIT;
    ELSIF sy#coma THEN Expected(coma); EXIT;
    END;
  END;
END Dimens;

TYPE Pd=POINTER TO Del;
     Del=RECORD d:INTEGER; next:Pd; END;

CONST FFCT=9Fh; RTN=0CAh;
VAR finish:BOOLEAN;    (* 0-ая процедура закончена *)
    first :BOOLEAN;    (* В текущем модуле разбирается первая строка *)
    MainExist:BOOLEAN; (* Во входном файле имеется main-program *)

PROCEDURE InitDcl;
BEGIN MainExist:=FALSE; END InitDcl;

PROCEDURE MainBegin;
BEGIN
  IF first THEN self:=main;
    IF MainExist THEN Fault(9); END;
    InitGen(-1); ModIdno:=1000000; first:=FALSE;
    SubPrograms;
    MainExist:=TRUE;
  END;
END MainBegin;

PROCEDURE LocFuncDcl():BOOLEAN;
  VAR F,no,parno:INTEGER; D,p:Pd;  pp:PointP; I:Info;
      tp,tpF:Types; w:WORD; rtn:BOOLEAN; P:ParList;
      seq:ARRAY [0..5] OF INTEGER;
(* Вовращает TRUE, если не описание внутренней функции *)
BEGIN
  F:=idno;
  IF GetMode(F)=Var THEN I.name:=F; Unpack(I);
    IF I.param THEN RETURN TRUE; END;
    IF F=ModIdno THEN RETURN TRUE; END;
    GetSy; IF sy#lpar THEN RETURN TRUE; END;
    FreeGW(I.offset); DelObj(F);
  ELSIF GetMode(F)#Empty THEN RETURN TRUE;
  ELSE SetDefaultTp(F);
    GetSy; IF sy#lpar THEN RETURN TRUE; END;
  END;
  tpF:=GetType(F);
  parno:=0; D:=NIL; rtn:=FALSE;
  WHILE sy#rpar DO
    IF parno=6 THEN Error(35); rtn:=TRUE; sy:=rpar;
    ELSE GetSy;
      IF sy#id THEN rtn:=TRUE; sy:=rpar;
      ELSE
        IF idno=F THEN Error(37); RETURN FALSE; END;
        FOR no:=0 TO parno-1 DO
          (* Проверка на единственность параметра *)
          IF idno=seq[no] THEN Error(38); RETURN FALSE; END;
        END;
        IF GetMode(idno)#Empty THEN
          SaveO(idno); SetDefaultTp(idno);
        ELSE Give(p,TSIZE(Del)); p^.d:=idno;
          p^.next:=D; D:=p;
        END;
        P[parno]:=GetType(idno); seq[parno]:=idno; INC(parno);
        GetSy; IF (sy#rpar) AND (sy#coma) THEN sy:=rpar; rtn:=TRUE; END;
      END;
    END;
  END;
  FOR no:=0 TO parno-1 DO
    I.name:=seq[no];
    SetMode(I.name,LocVar);
    I.offset:=parno-no+3; GenObj(I);
  END;
  IF NOT rtn THEN GetSy; IF sy#eqv THEN rtn:=TRUE; END; END;
  IF rtn THEN p:=D;
    WHILE p#NIL DO
      DelId(p^.d);
      DelObj(p^.d); p:=p^.next;
    END;
    RestoreO; RETURN TRUE;
  END;
  IF NOT finish THEN FinishProc0; finish:=TRUE; END;
  SetMode(F,LocFunc); SetFN(F);
  no:=StartFunc(); MarkPos1; GetSy;
  IF Expr(tp,w) THEN li(INTEGER(w)); END;
  SetFN(100000);
  IF tp#tpF THEN
    IF    (tp=Real) AND (tpF=Int) THEN c1(FFCT,1);
    ELSIF (tp=Int) AND (tpF=Real) THEN c1(FFCT,0);
    ELSIF tp#Undef THEN Error(36);
    END;
  END;
  c(RTN);
  I.name:=F; I.dimension:=parno; I.offset:=no;
  Give(pp,TSIZE(ParList));
  FOR no:=0 TO parno-1 DO pp^[no]:=P[no]; END;
  I.partypes:=pp; GenObj(I); p:=D;
  WHILE p#NIL DO
    DelId(p^.d);
    DelObj(p^.d); p:=p^.next;
  END;
  RestoreO; RETURN FALSE;
END LocFuncDcl;

VAR Params:ARRAY [0..99] OF INTEGER; (* lake fExt *)
    ForwardWas:BOOLEAN; (* для подпрограммы было описание *)

PROCEDURE InParamSet(id:INTEGER):BOOLEAN;
  VAR i:INTEGER;
BEGIN
  FOR i:=0 TO ParNo-1 DO
    IF Params[i]=id THEN RETURN TRUE; END;
  END;
  RETURN FALSE;
END InParamSet;

PROCEDURE GetParamDcl;
  VAR I:Info;
BEGIN ParNo:=0;
  LOOP
    GetSy; IF sy#id THEN Error(25); RETURN; END;
    IF InParamSet(idno) THEN Error(38);
    ELSIF idno=ModIdno THEN Error(49);
    ELSE
      Params[ParNo]:=idno;
      SetMode(idno,Var); I.name:=idno; I.param:=TRUE;
      I.offset:=5+ParNo; GenObj(I); INC(ParNo);
    END;
    GetSy; IF sy=rpar THEN EXIT; END;
    IF sy#coma THEN Expected(coma); RETURN; END;
  END;
  GetSy; IF sy#EOL THEN Error(16); END;
END GetParamDcl;

PROCEDURE HeaderSubProg(m:Mode;T:Types);
  VAR I:Info; tp:Types; ID:Idname;
BEGIN ParNo:=0;
  SetWork(ass); GetSy; IF sy#id THEN Fault(4); END;
  ModIdno:=idno; InitGen(ModIdno); SubPrograms;
  IF T#Undef THEN SetType(idno,T); END;
  IF m=Subr THEN tp:=Undef ELSE tp:=GetType(idno); END;
  IdStr(idno,ID); ForwardWas:=PutSubProg(ID,tp,TRUE);
  IF WasRealised() THEN Fault(3); END;
  IF m=Subr THEN SetMode(idno,Subr); (* Генерировать об'ект не надо *)
  ELSE SetMode(idno,Var);
    I.name:=idno; I.param:=FALSE;    (* Сгенерировали переменную *)
    I.offset:=GiveGW(); GenObj(I);   (* для значения функции     *)
  END;
  GetSy; IF sy=EOL THEN (* without parameter's *) RETURN; END;
  IF sy#lpar THEN Expected(lpar); RETURN; END;
  GetParamDcl;
END HeaderSubProg;

PROCEDURE FunctionDcl(T:Types);
BEGIN ModIdno:=1000000;
  IF first THEN self:=function; first:=FALSE;
    HeaderSubProg(Func,T);
  ELSE Error(29); END;
END FunctionDcl;

VAR spec:BOOLEAN; (* Разбор спецификаций разрешен *)

PROCEDURE TypeName;
  VAR tp:Types;
BEGIN
  tp:=Types(ORD(sy)); MarkPos; SetWork(dcl); GetSy;
  IF sy=func THEN FunctionDcl(tp);
  ELSE BackUp; SetWork(ass); MainBegin;
    IF spec THEN TypeDcl(tp);
    ELSE Error(21); END;
  END;
END TypeName;

PROCEDURE declare;
  VAR i,j:INTEGER;
BEGIN
  spec:=TRUE; global:=3; finish:=FALSE; first:=TRUE; GW:=NIL;
  LOOP NewLine; SetWork(dcl);
    GetSy; SetWork(ass);
    CASE sy OF
      int,real,log,char: TypeName;
     |subr : IF first THEN self:=subroutine; first:=FALSE;
               HeaderSubProg(Subr,Undef);
             ELSE Error(29); END;
     |func : FunctionDcl(Undef);
     |prog : MainBegin;
     |impl : MainBegin; IF spec THEN Implicit; ELSE Error(21); END;
     |comm,data,ext,equi : Fault(17);
     |param: MainBegin; IF spec THEN Param;    ELSE Error(21); END;
     |fwrd : IF first THEN GetSy;
               IF sy#EOL THEN Error(16); END;
               Forward;
             ELSE Error(51); END;
     |dim  : MainBegin; IF spec THEN Dimens;   ELSE Error(21); END;
     |frm  : Fault(17);
    ELSE BackUp; GetSy; MainBegin;
      IF sy#id THEN EXIT; END;
      IF LocFuncDcl() THEN EXIT;
      ELSE spec:=FALSE; END;
    END; (* case *)
  END; (* loop *)
  IF self=main THEN ParNo:=0; END;
  IF NOT ForwardWas THEN
    FOR i:=0 TO ParNo-1 DO j:=Params[i];
      PutParam(GetMode(j),GetType(j));
    END;
  END;
  BackUp; IF NOT finish THEN FinishProc0; END; StartProc1;
END declare;

BEGIN
END fDcl.
