MODULE fn; (* Max *)

IMPORT StdIO, fScan, fGen, fExpr, fDcl, fExt, KRONOS;
FROM SYSTEM IMPORT WORD;
FROM Args   IMPORT Flag?;

VAR level :INTEGER;  (* cp начала текущего тела IF, ELSE, ELSEIF или DO *)
                      (* равно 0, если находимся не в к.-л. теле *)
    iflevel:INTEGER; (* Вложенность структурных IF-ов *)
    dolevel:INTEGER; (* Вложенность операторов DO *)
    maxdolv:INTEGER; (* Максимальная вложенность операторов DO *)
    saventr:INTEGER; (* Количество локалов 1-ой процедуры *)
    EndUnit:BOOLEAN;  (* Флаг окончания единицы компиляции *)

(* ----------------------- L a b e l s ------------------------- *)
MODULE Labels;

(*FROM Main*) IMPORT level;
FROM fScan    IMPORT Error, ErrorCo, Give, Free;
FROM fGen     IMPORT JB, Jump, SetJLFC, SetJLF;
FROM StdIO    IMPORT print, Show;

EXPORT InitLabels, ExitBody, PutLabel, Goto, FinishCheck, GetFmt, SetFmt;

TYPE PT = POINTER TO Two;
     Two= RECORD from,level: INTEGER; next: PT; END;

     PF = POINTER TO F;
     F  = RECORD label:INTEGER;  next: PF;  from: PT; END;

     PE = POINTER TO E;
     E  = RECORD
            label:INTEGER;
            level:INTEGER; (*уровень видимости метки; =0,если видна всегда;*)
                            (* =FFh, если метка перестала быть видимой      *)
            cp   :INTEGER;
            next :PE;
          END;

VAR Forward: PF; (* список forward - меток,   *)
                 (* на которые имеются ссылки *)
    Exist  : PE; (* список описанных меток *)

PROCEDURE InitLabels;
BEGIN Forward:=NIL; Exist:=NIL; END InitLabels;

PROCEDURE PutLabel(label,cp:INTEGER);
  VAR t,p:PT; f,h:PF; e,i:PE;
(* Вносит метку в список описанных и разбирает все forward-ссылки на нее; *)
(* сообщает об ошибке,если нарушена видимость или метка об'яблена повторно*)
BEGIN
  IF label=0 THEN RETURN ; END;
  IF Exist=NIL THEN Give(Exist,TSIZE(E)); e:=Exist;
  ELSE e:=Exist;
    WHILE e#NIL DO
      IF e^.label=label THEN Error(9); RETURN; END;
      i:=e; e:=e^.next;
    END;
    Give(e,TSIZE(E)); i^.next:=e;
  END;
  e^.label:=label; e^.level:=level; e^.cp:=cp; e^.next:=NIL;
  f:=Forward;
  WHILE f#NIL DO
    IF f^.label=label THEN
      IF f=Forward THEN Forward:=f^.next; ELSE h^.next:=f^.next; END;
      t:=f^.from; ASSERT(t#NIL); Free(f,TSIZE(F));
      WHILE t#NIL DO
        IF t^.level<level THEN Error(8);
        ELSE Jump(t^.from,cp); END;
        p:=t; Free(p,TSIZE(Two)); t:=t^.next;
      END;
      RETURN;
    END;
    h:=f; f:=f^.next;
  END;
END PutLabel;

PROCEDURE FinishCheck;
  VAR f:PF; (* Проверяет, есть ли ненайденные метки *)
BEGIN
  IF Forward=NIL THEN RETURN; END;
  f:=Forward;
  Show("Не найдены метки:"); INC(ErrorCo);
  WHILE f#NIL DO
    print('   %d',f^.label);
    f:=f^.next;
  END; HALT;
END FinishCheck;

PROCEDURE ExitBody;
  VAR e:PE; f:PF; t:PT; (* Устанавливает об'ласти видимости *)
BEGIN e:=Exist;         (* всем меткам завершенного тела    *)
  WHILE e#NIL DO
    IF e^.level=level THEN e^.level:=0FFh; END;
    e:=e^.next;
  END;
  f:=Forward;
  WHILE f#NIL DO
    t:=f^.from;
    WHILE t#NIL DO
      IF t^.level=level THEN t^.level:=level-1; END;
      t:=t^.next;
    END;
    f:=f^.next;
  END;
END ExitBody;

PROCEDURE Goto(cond:BOOLEAN; tolabel:INTEGER);
  VAR t:PT; f,h:PF; e:PE;
(* Если label в списке Exist и она видима, то генерируется JB,      *)
(* в противном случае label заносится в список Forward с генерацией *)
(* SetJLFC или SetJLF, в соответствии с cond. *)
BEGIN
  e:=Exist;
  WHILE e#NIL DO
    IF e^.label#tolabel THEN e:=e^.next;
    ELSIF e^.level<=level THEN
      IF e^.cp<0 THEN Error(04) END;
      JB(cond,e^.cp); RETURN;
    ELSE Error(8); RETURN ;
    END;
  END;
  f:=Forward;
  WHILE f#NIL DO
    IF f^.label=tolabel THEN ASSERT(f^.from#NIL);
      Give(t,TSIZE(Two)); t^.level:=level;
      IF cond THEN t^.from:=SetJLFC();
      ELSE t^.from:=SetJLF();
      END;
      t^.next:=f^.from; f^.from:=t; RETURN;
    ELSE h:=f; f:=f^.next; END;
  END;
  Give(f,TSIZE(F));
  IF Forward=NIL THEN
    Forward:=f;
  ELSE h^.next:=f; END;
  f^.label:=tolabel; f^.next:=NIL;
  Give(t,TSIZE(Two));
  f^.from:=t; t^.level:=level; t^.next:=NIL;
  IF cond THEN t^.from:=SetJLFC();
  ELSE t^.from:=SetJLF();
  END;
END Goto;

PROCEDURE GetFmt(lab: INTEGER): INTEGER;
  VAR e: PE;
BEGIN
  e:=Exist;
  WHILE e#NIL DO
    IF e^.label#lab THEN e:=e^.next;
    ELSE
      IF e^.cp>0 THEN Error(04) END;
      RETURN -e^.cp
    END;
  END;
  Error(58); RETURN 0
END GetFmt;

PROCEDURE SetFmt(lab: INTEGER; strofs: INTEGER);
  VAR e: PE;
BEGIN
  e:=Exist;
  WHILE e#NIL DO
    IF e^.label#lab THEN e:=e^.next;
    ELSE
      IF e^.cp<0 THEN Error(04) END;
      e^.cp:=-strofs; RETURN
    END;
  END;
  Error(58); RETURN;
END SetFmt;

END Labels;

(* ------------------ E n d   L a b e l s ---------------------- *)

(* ----------------- S t a t e m e n t s ----------------------- *)

MODULE Stm;

FROM KRONOS     IMPORT  SETM, GETM;
FROM fScan IMPORT Symbol, Types, Work, sy, Label, idno, GetSy, MarkPos,
                  Ival, BackUp, NewLine, SetWork, Error, Expected, Info,
                  DelObj, DelId, SetType, GetType, SetMode, GetMode,
                  ErrorId, Mode, Give, Free, Fault, WhatSy?, MarkPos1,
                  Descriptor, D, head, GenObj, Unpack, IdStr, Idname,
                  PointPS;
FROM fGen  IMPORT cp, SetJLFC, SetJLF, Jump, epop, epush, Setdepth, LoadAdr,
                  JB, LoadVal, StoreInVar, MarkC, BackC, c, c1, lsw, PutExt,
                  li, lgw, llw, slw, Trap, MarkC1, BackC1, PutCod, SvalToPool;
FROM fExpr IMPORT Expr, LoadInd, ExtCall;
FROM fDcl  IMPORT GiveGW, ParNo, ModIdno, self, unit;
(* main *) IMPORT WORD, level, iflevel, dolevel, maxdolv,
                  Goto, PutLabel, ExitBody, EndUnit, GetFmt, SetFmt;

EXPORT Statement;

CONST FFCT=9Fh;  RTN=0CAh;  JSFC=1Ah; LEQ=0A1h; JSF=1Bh; ADD=88h;
      copt=0B5h; LSS=0A0h;  Not=0AEh; LSA=16h;  GEQ=0A3h; GTR=0A2h;
      SXW=51h;   LSTA=0C2h; CX=0CCh;
      ALLOC=0C8h;           STOT=0E8h;          DECS=0B0h;
      FCMP=09Ch; FADD=098h;
      INC1=0E4h; DEC1=0E5h; INCn=0E6h; DECn=0E7h;

PROCEDURE Assign():BOOLEAN; (* TRUE, if Assign-statement *)
  VAR tp,tpL:Types; w:WORD; cons:BOOLEAN; Left:INTEGER;
      del:BOOLEAN; mL:Mode; I:Info;
BEGIN
  SetWork(ass); MarkPos; MarkC;
  GetSy; IF sy#id THEN Error(25); RETURN TRUE; END;
  Left:=idno; GetSy; mL:=GetMode(Left); tpL:=GetType(Left);
  del:=(mL=Empty) AND (WhatSy?(Left)=invKW);
  IF sy=eqv  THEN
    IF mL=Var THEN I.name:=Left; Unpack(I);
      IF I.param THEN llw(I.offset); END;
    ELSIF mL#Empty THEN Error(14); RETURN TRUE; END;
    MarkPos1; GetSy; cons:=Expr(tp,w);
    IF   sy=coma THEN
      IF del THEN DelId(Left); END;
      BackUp; BackC; RETURN FALSE;
    ELSIF sy=EOL THEN
      IF cons THEN li(INTEGER(w)); END;
      IF mL=Empty THEN
        SetMode(Left,Var); I.offset:=GiveGW();
        I.param:=FALSE; I.name:=Left; GenObj(I);
      END;
      IF tpL#tp THEN
        IF    (tp=Real) AND (tpL=Int) THEN c1(FFCT,1);
        ELSIF (tp=Int) AND (tpL=Real) THEN c1(FFCT,0);
        ELSIF tp#Undef THEN Error(17); RETURN TRUE;
        END;
      END;
      StoreInVar(Left); RETURN TRUE;
    ELSE Error(16); RETURN TRUE;
    END;
  ELSIF sy=lpar THEN
    IF mL#Array THEN
      IF del THEN DelId(Left); END;
      BackUp; RETURN FALSE;
    ELSE
      I.name:=Left; Unpack(I);
      IF I.param THEN
        llw(I.offset); lsw(0);
      ELSE lgw(I.offset); END;
      IF LoadInd(I.desc^,I.dimension,I.abs) THEN
        GetSy; IF sy#eqv THEN BackUp; BackC; RETURN FALSE; END;
        MarkPos1; GetSy;
        IF Expr(tp,w) THEN li(INTEGER(w)); END;
        IF sy=EOL THEN
          IF tpL#tp THEN
            IF    (tp=Real) AND (tpL=Int) THEN c1(FFCT,1);
            ELSIF (tp=Int) AND (tpL=Real) THEN c1(FFCT,0);
            ELSIF tp#Undef THEN Error(17);
            END;
          END;
          c(SXW); epop; epop; epop;
        ELSE Error(16); END;
      END;
      RETURN TRUE;
    END;
  ELSE
    IF del THEN DelId(Left); END;
    BackUp; RETURN FALSE;
  END;
END Assign;

PROCEDURE GetLabel():BOOLEAN;
BEGIN
  GetSy; IF sy#const THEN Error(4); RETURN FALSE; END;
  IF (Ival<1) OR (Ival>99999) THEN Error(4); RETURN FALSE; END;
  RETURN TRUE;
END GetLabel;

VAR valexp:WORD; consexp:BOOLEAN;

PROCEDURE If3;
(* Разбор трехзубого IF-a; арифм. выражение на стеке *)
  VAR l1,l2,l3:INTEGER;
BEGIN
  IF GetLabel() THEN l1:=Ival; GetSy;
    IF sy#coma THEN Expected(coma); RETURN; END;
  ELSE RETURN; END;
  IF GetLabel() THEN l2:=Ival; GetSy;
    IF sy#coma THEN Expected(coma); RETURN; END;
  ELSE RETURN; END;
  IF GetLabel() THEN l3:=Ival; GetSy;
    IF sy#EOL THEN Error(16); RETURN; END;
  ELSE RETURN; END;
  IF consexp THEN  (* тип пока только INTEGER *)
    IF    INTEGER(valexp)=0 THEN Goto(FALSE,l2);
    ELSIF INTEGER(valexp)>0 THEN Goto(FALSE,l3);
    ELSE  Goto(FALSE,l1);
    END;
  ELSIF (l1=l2) AND (l2=l3) THEN BackC; Goto(FALSE,l1);
  ELSIF (l1=l2) THEN li(0); c(LEQ);epop; Goto(TRUE,l3); Goto(FALSE,l1);
  ELSIF (l3=l2) THEN li(0); c(LSS);epop; Goto(TRUE,l3); Goto(FALSE,l1);
  ELSE (* общий случай *)
    c(copt); epush; slw(ParNo+5); Goto(TRUE,l2); llw(ParNo+5); li(0);
    c(LEQ); epop; Goto(TRUE,l3); Goto(FALSE,l1);
  END;
END If3;

PROCEDURE SubrCall():BOOLEAN;
  VAR I:Info; Id:Idname; P:PointPS; tp:Types; (* TRUE if error *)
BEGIN
  SetWork(ass); GetSy; IF sy#id THEN Error(25); RETURN TRUE; END;
  IF GetMode(idno)=Empty THEN ErrorId(9); RETURN TRUE; END;
  ExtCall; RETURN FALSE;
END SubrCall;

PROCEDURE Return():BOOLEAN; (* TRUE if error *)
BEGIN
  IF self=main THEN Error(44); RETURN TRUE; END;
  IF self=function THEN LoadVal(ModIdno); END;
  c(RTN); RETURN FALSE;
END Return;

PROCEDURE Print; FORWARD;

PROCEDURE IfL():BOOLEAN;
   VAR offset:INTEGER;
(* Разбор логического IF-a; FALSE, если IF структурный; *)
(* на стеке логическое выражение *)
BEGIN
  MarkC1;
  IF consexp THEN
    IF BOOLEAN(valexp) THEN
      IF Assign() THEN RETURN TRUE; END;
    ELSIF Assign() THEN BackC; RETURN TRUE;
    ELSE SetWork(kw); GetSy; RETURN NOT (sy=then);
    END;
  ELSE c(JSFC); epop; offset:=cp; c(0);
    IF Assign() THEN
      PutCod(offset,cp-(offset+1));
      RETURN TRUE;
    END;
  END;
  SetWork(kw); GetSy;
  CASE sy OF
    call: IF SubrCall() THEN RETURN TRUE; END;
   |goto: IF GetLabel() THEN
            IF consexp THEN Goto(FALSE,Ival);
            ELSE BackC1; c(Not);
              (* "Положим" выражение вновь на стек *)
              epush; Goto(TRUE,Ival);
              GetSy; IF sy#EOL THEN Error(16); END;
            END;
          END; RETURN TRUE;
   |cnt : BackC1; RETURN TRUE;
   |rtn : IF Return() THEN RETURN TRUE; END;
   |stp : Trap(47h);
   |rd  :
   |wr  :
   |pr  : Print; RETURN TRUE;
   |then: GetSy; IF sy#EOL THEN Error(16); END;
          BackC1; epush; (*выражение осталось на стеке*) RETURN FALSE;
  ELSE IF sy#invKW THEN  Error(18); END; RETURN TRUE;
  END;
  IF NOT consexp THEN PutCod(offset,cp-(offset+1)); END;
  GetSy; IF sy#EOL THEN Error(16); END;
  RETURN TRUE;
END IfL;

PROCEDURE Statement; FORWARD;

PROCEDURE If;
  VAR i,savecp:INTEGER;
      ifjump: ARRAY [0..15] OF INTEGER;
      nojump: INTEGER;
      tp:Types;
      w:WORD;
BEGIN
  ASSERT(sy=if);
  GetSy; IF sy#lpar THEN Expected(lpar); RETURN; END;
  MarkPos1; SetWork(ass); GetSy; consexp:=Expr(tp,valexp);
  IF sy#rpar THEN Expected(rpar); RETURN; END;
  IF ORD(tp)<=ORD(Real) THEN If3; RETURN;
  ELSIF tp#Logic THEN Error(7);
  ELSIF IfL() THEN RETURN;
  END;
  (* Структурный IF *)
  IF consexp THEN li(INTEGER(valexp)); END;
  savecp:=SetJLFC(); INC(level); INC(iflevel); nojump:=-1;
  REPEAT NewLine; Statement; UNTIL (sy=else) OR (sy=elsif) OR (sy=endif);
  WHILE sy=elsif DO
    INC(nojump); ifjump[nojump]:=SetJLF();
    Jump(savecp,cp); ExitBody; DEC(level);
    GetSy; IF sy#lpar THEN Expected(lpar); DEC(iflevel); RETURN; END;
    MarkPos1; SetWork(ass); GetSy; IF Expr(tp,w) THEN li(INTEGER(w)); END;
    IF sy#rpar THEN Expected(rpar); DEC(iflevel); RETURN; END;
    IF tp#Logic THEN Error(7); DEC(iflevel); RETURN; END;
    SetWork(kw); GetSy;
    IF sy#then THEN Expected(then); DEC(iflevel); RETURN; END;
    GetSy; IF sy#EOL THEN Error(16); END;
    savecp:=SetJLFC(); INC(level);
    REPEAT NewLine; Statement; UNTIL (sy=else) OR (sy=elsif) OR (sy=endif);
  END;
  IF sy=else THEN INC(nojump); ifjump[nojump]:=SetJLF();
    Jump(savecp,cp); ExitBody;
    GetSy; IF sy#EOL THEN Error(16); END;
    REPEAT NewLine; Statement; UNTIL (sy=else) OR (sy=elsif) OR (sy=endif);
  ELSIF sy#endif THEN
    Expected(endif); DEC(level);
    DEC(iflevel); RETURN;
  ELSE Jump(savecp,cp); ExitBody; DEC(level);
    GetSy; IF sy#EOL THEN Error(16); END; DEC(iflevel);
    FOR i:=0 TO nojump DO Jump(ifjump[i],cp); END;
    RETURN;
  END;
  IF sy#endif THEN
    Expected(endif); DEC(level);
    DEC(iflevel); RETURN;
  END;
  ExitBody; DEC(level);
  GetSy; IF sy#EOL THEN Error(16); END; DEC(iflevel);
  FOR i:=0 TO nojump DO Jump(ifjump[i],cp); END;
END If;

PROCEDURE DoStatement(endlabel:INTEGER); FORWARD;

PROCEDURE Do;
(* Генерация цикла  DO <endlabel> I=I0,N,By *)
  VAR tp,vtp:Types;
  PROCEDURE Convert(w: WORD): INTEGER;
    VAR i: INTEGER;
  BEGIN
    SETM(GETM()-{31});
    IF tp=vtp THEN
      i:=INTEGER(w)
    ELSE
      IF vtp=Real THEN
        i:=INTEGER(FLOAT(INTEGER(w)))
      ELSE
        i:=TRUNC(REAL(w));
      END;
    END;
    -- ?????? проверить переполнение
    RETURN i;
  END Convert;
  VAR w:WORD;
      endlabel:INTEGER;
      I0,N,By,i:INTEGER;
      I0cons,Ncons,Bycons:BOOLEAN;
      var,entry,exit:INTEGER;
      I:Info;
BEGIN
  ASSERT(sy=do);
  IF GetLabel() THEN endlabel:=Ival;
  ELSE RETURN; END;
  SetWork(ass);

  GetSy; IF sy#id THEN Error(25); RETURN; END; var:=idno;
  I.name:=var;
  IF GetMode(var)=Empty THEN
    SetMode(var,Var); I.offset:=GiveGW();
    I.param:=FALSE; GenObj(I);
  ELSIF GetMode(var)#Var THEN Error(14);
    REPEAT NewLine; UNTIL Label=endlabel;
    RETURN;
  ELSE Unpack(I);
    IF I.param THEN Error(46);
      REPEAT NewLine; UNTIL Label=endlabel;
      RETURN;
    END;
  END;
  vtp:=GetType(var);
  IF ORD(vtp)>ORD(Real) THEN Error(19); RETURN END;
  GetSy; IF sy#eqv THEN Expected(eqv); RETURN; END;
  INC(dolevel); IF dolevel>maxdolv THEN maxdolv:=dolevel; END;

  MarkPos1; GetSy; I0cons:=Expr(tp,w);
  IF ORD(tp)>ORD(Real) THEN Error(19); RETURN; END;
  IF I0cons THEN
    I0:=Convert(w);
  ELSE
    IF vtp#tp THEN IF tp=Real THEN c1(FFCT,1) ELSE c1(FFCT,0) END END;
    slw(ParNo+5);
  END;
  IF sy#coma THEN Expected(coma); RETURN; END;

  MarkPos1; GetSy; Ncons:=Expr(tp,w);
  IF ORD(tp)>ORD(Real) THEN Error(19); RETURN; END;
  IF Ncons THEN
    N:=Convert(w);
  ELSE
    IF vtp#tp THEN IF tp=Real THEN c1(FFCT,1) ELSE c1(FFCT,0) END END;
    slw(ParNo+3*dolevel+3);
  END;

  IF sy=coma THEN (* exist By *)
    MarkPos1; GetSy; Bycons:=Expr(tp,w);
    IF ORD(tp)>ORD(Real) THEN Error(19); RETURN; END;
    IF Bycons THEN
      By:=Convert(w);
    ELSE
      IF vtp#tp THEN IF tp=Real THEN c1(FFCT,1) ELSE c1(FFCT,0) END END;
      c(copt); epush;
      i:=ParNo+4+3*dolevel; slw(i); INC(i);
      li(0); c(LSS); epop; -- верно и для Real --
      slw(ParNo+5+3*dolevel);
    END;
  ELSIF vtp=Real THEN By:=INTEGER(1.0); Bycons:=TRUE;
  ELSE By:=1; Bycons:=TRUE;
  END;
  IF sy#EOL THEN Error(16) END;
  IF I0cons THEN li(I0) ELSE llw(ParNo+5); END;
  IF Bycons & (vtp=Int) THEN
    StoreInVar(var);
    IF Ncons & I0cons & ((N>=I0)=(By>=0))
        THEN exit:=-1 ELSE exit:=SetJLF() END;
    entry:=cp; INC(level);
    REPEAT DoStatement(endlabel) UNTIL Label=endlabel; (* Body DO   *)
    IF By#0 THEN
      LoadAdr(var);
      IF    By=-1  THEN c(DEC1)
      ELSIF By= 1  THEN c(INC1)
      ELSIF By>=1  THEN li(By); c(INCn); epop;
      ELSIF By<=-1 THEN li(-By); c(DECn); epop;
      END;
      epop;
    END;
    IF exit>=0 THEN Jump(exit,cp) END;
    LoadVal(var);
    IF Ncons THEN li(N) ELSE llw(ParNo+3*dolevel+3) END;
    IF By>=0 THEN c(GTR) ELSE c(LSS) END; epop;
    JB(TRUE,entry);
  ELSE
    entry:=cp; c(copt); epush;
    StoreInVar(var);
    IF Ncons THEN li(N) ELSE llw(ParNo+3+3*dolevel) END;
    IF vtp=Real THEN c(FCMP) END;
    IF Bycons THEN
      IF By<0 THEN c(GEQ) ELSE c(LEQ) END; epop;
    ELSE
      llw(ParNo+5+3*dolevel);
      c1(JSFC,3); epop; c(GEQ); c1(JSF,1);  c(LEQ); epop;
    END;
    INC(level); exit:=SetJLFC();
    REPEAT DoStatement(endlabel) UNTIL Label=endlabel; (* Body DO   *)
    LoadVal(var);
    IF Bycons THEN
      IF vtp=Real THEN
        li(By); c(FADD); epop
      ELSIF (By>=1) AND (By<=0FFh) THEN
        c1(LSA,By);
      ELSE
        li(By); c(ADD); epop;
      END;
    ELSE
      llw(ParNo+4+3*dolevel);
      IF vtp=Real THEN c(FADD) ELSE c(ADD) END; epop;
    END;
    JB(FALSE,entry); epop; (*заочно*)
    Jump(exit,cp);
  END;
  ExitBody; DEC(level); DEC(dolevel);
END Do;

VAR svsy:Symbol;

PROCEDURE Print;
  VAR lab,i,n: INTEGER; cons: BOOLEAN;  tp: Types;
BEGIN
  IF NOT GetLabel() THEN RETURN END;
  lab:=Ival;
  c(LSTA); i:=GetFmt(lab); c(i MOD 100h); c(i DIV 100h); epush;
  li(255); epush;
  li(0); c(ALLOC);  epush;
  n:=0; SetWork(ass); GetSy;
  WHILE sy#EOL DO
    IF sy#coma THEN Expected(coma); RETURN END;
    MarkPos1; GetSy; cons:=Expr(tp,i);
    IF cons THEN li(i); epush END; INC(n); c(STOT); epop;
  END;
  Setdepth(0); li(n-1);
  c(CX); c(1); c(1); (* StdIO.print *)
  li(n); c(DECS);
END Print;

PROCEDURE Simple():BOOLEAN;
  VAR I:Info;
BEGIN
  IF Assign() THEN RETURN TRUE; END;
  SetWork(kw); GetSy; svsy:=sy;
  CASE sy OF
    call: IF SubrCall() THEN RETURN TRUE; END;
   |goto: IF GetLabel() THEN Goto(FALSE,Ival); END;
   |cnt : (* nothing *)
   |rtn : IF Return() THEN RETURN TRUE; END;
   |stp : Trap(47h);
   |rd,wr,asgn: Fault(17);
   |pr  : Print; RETURN TRUE;
  ELSE BackUp; RETURN FALSE;
  END;
  GetSy; IF sy#EOL THEN Error(16); END;
  RETURN TRUE;
END Simple;

PROCEDURE Format;
BEGIN
  IF Label=0 THEN Error(59); RETURN END;
  GetSy;
  IF sy#lpar THEN Expected(lpar); RETURN END;
  GetSy;
  IF sy#stringconst THEN Error(60); RETURN END;
  GetSy;
  IF sy#rpar THEN Expected(rpar); RETURN END;
  GetSy; IF sy#EOL THEN Error(16); END;
  SetFmt(Label,SvalToPool());
END Format;

PROCEDURE DoStatement(endlabel:INTEGER);
  VAR eND:BOOLEAN;
BEGIN Setdepth(0);
  NewLine;
  PutLabel(Label,cp); eND:=(endlabel=Label);
  IF Simple() THEN
    IF eND AND ((svsy=goto) OR (svsy=rtn) OR (svsy=stp))
    THEN Error(34); END;
    RETURN;
  END;
  IF svsy=invKW THEN RETURN; END;
  SetWork(kw); GetSy;
  CASE sy OF
    else,elsif,endif: IF iflevel=0 THEN ErrorId(3); RETURN; END;
                      IF Label#0 THEN ErrorId(4); END;
   |do : IF eND THEN Error(34); END; Do;
   |if : IF eND THEN Error(34); END; If;
   |end: Fault(7);
   |frm: Format;
  ELSE
  END;
END DoStatement;

PROCEDURE Statement;
BEGIN
  Setdepth(0); PutLabel(Label,cp);
  IF Simple() THEN RETURN; END;
  IF svsy=invKW THEN RETURN; END;
  SetWork(kw); GetSy;
  CASE sy OF
    else,elsif,endif: IF iflevel=0 THEN ErrorId(3); RETURN; END;
                      IF Label#0 THEN ErrorId(4); END;
   |do : Do;
   |if : If;
   |end: IF level#0 THEN Fault(8); END; EndUnit:=TRUE;
   |frm: (*Fault(17);*) Format;
  ELSE
  END;
END Statement;

END Stm;
(* -----------E n d   S t a t e m e n t s ---------------------- *)

PROCEDURE Parser;
BEGIN
  fScan.InitScan; InitLabels;
  iflevel:=0; dolevel:=0; maxdolv:=0;
  level:=0; fDcl.declare;
  fGen.Enter(saventr); EndUnit:=FALSE;
  LOOP
    Statement;
    IF EndUnit THEN EXIT; END;
    fScan.NewLine;
  END;
  fGen.FinishCod; FinishCheck;
  fGen.PutCod(saventr,fDcl.ParNo+2+maxdolv*3); (* отвели регистры *)
  IF fScan.ErrorCo=0 THEN fGen.WriteCodeFile; END;
(*IF Flag?("t") THEN fScan.VisIdTable; END; *)
END Parser;

BEGIN
  fScan.InitZeroScan; fDcl.InitDcl; fExt.InitForw;
  REPEAT
    Parser; fScan.ReleaseHeap;
  UNTIL (fScan.BuffIsEmpty OR (fScan.ErrorCo#0));
  IF fScan.ErrorCo#0 THEN
    StdIO.Show("*********************");
    StdIO.print("Число ошибок %d\n",fScan.ErrorCo);
  END;
END fn.
