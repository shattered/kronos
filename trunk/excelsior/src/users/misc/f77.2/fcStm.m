IMPLEMENTATION MODULE fcStm; (* 26-Oct-88.  *)

FROM StdIO  IMPORT Show,print;
FROM SYSTEM IMPORT WORD;
FROM fcScan  IMPORT Error,ErrorId,Expected, ErrorCo,
                    Fault,Warning,cType,Ival,Slen,Sval,Work,Label,
                    GetSy,Symbol,sy, lookAhead, SetWork, MarkPos,BackUp,
                    SkipTo, match, NewLine, MarkPos1, BackUp1, KeyON;
FROM fcObj   IMPORT Info, GetType, SetType, GetMode, SetMode,
                    Unpack, GenLocal, idno, Types, Mode, Class,Ident,
                    DelId, GenObj;
FROM fcHeap  IMPORT Give, Free;
FROM fcExpr  IMPORT Dexpr, Expr, Lexpr, aExpr, Emode, iExpr;
FROM fcDcl   IMPORT AllocVar, AllocExt, unit,self, Entry, ModIdno;
FROM fcInOut IMPORT Write,Read,Print,Rewind,Backspace,Endfile,
                    Open,Close,Inquire,Format;
FROM fcProc  IMPORT SubrCall;
FROM fcTproc IMPORT AllocTemp, FreeTemp, AllocDesc, FreeDesc, FreeDescs;
FROM fcGen   IMPORT cp,c,c1,cJump,cLdFormat,LdFormat, b2, lsta,
                    epop,epush, MarkC,MarkC1,BackC, BackC1, li, llw, slw,
                    sew, lla, moveCode, CallExt, Setdepth, Getdepth,
                    InPoolStr;
FROM fcInd   IMPORT LoadInd, ldSubstr, LoadAdr, LeftAdr, StoreIn, LoadVal,
                    StoreInVar, LoadConst, gDovarA, gDovarV;
FROM objFile IMPORT pLabel, pFormat;
FROM fcBugs  IMPORT AddStr;

VAR level :INTEGER;  (* cp начала текущего тела IF, ELSE, ELSEIF или DO *)
                      (* равно 0, если находимся не в к.-л. теле *)
    iflevel:INTEGER; (* Вложенность структурных IF-ов *)
    dolevel:INTEGER; (* Вложенность операторов DO *)
    maxdolv:INTEGER; (* Максимальная вложенность операторов DO *)
    Loclab :INTEGER;  (* локальная метка *)

(* ----------------------- L a b e l s ------------------------- *)
MODULE Labels;

(*FROM Main*) IMPORT level;
   IMPORT Error, ErrorCo, Loclab;
   IMPORT Give, Free;
   IMPORT cJump, LdFormat, cLdFormat, pLabel, pFormat;
   IMPORT Show, print;

EXPORT InitLabels, ExitBody, PutLabel, Goto, FinishCheck, GetFmt, SetFmt;

TYPE PT = POINTER TO Two;
     Two= RECORD level: INTEGER; next: PT; END;

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
BEGIN
  Forward:=NIL; Exist:=NIL; level:=0;
  Loclab:=100000;
END InitLabels;

PROCEDURE PutLabel(label,cp:INTEGER);
  VAR t,p:PT; f,h:PF; e,i:PE;
(* Вносит метку в список описанных и разбирает все forward-ссылки на нее; *)
(* сообщает об ошибке,если нарушена видимость или метка об'явлена повторно*)
BEGIN
  IF label=0 THEN RETURN ; END;
  IF Exist=NIL THEN Give(Exist,SIZE(E)); e:=Exist;
  ELSE e:=Exist;
    WHILE e#NIL DO
      IF e^.label=label THEN Error(9); RETURN; END;
      i:=e; e:=e^.next;
    END;
    Give(e,SIZE(E)); i^.next:=e;
  END;
  e^.label:=label; e^.level:=level; e^.cp:=cp; e^.next:=NIL;
  f:=Forward;
  WHILE f#NIL DO
    IF f^.label=label THEN
      IF f=Forward THEN Forward:=f^.next; ELSE h^.next:=f^.next; END;
      t:=f^.from; -- ASSERT(t#NIL);
      Free(f,SIZE(F));
      WHILE t#NIL DO
        IF t^.level<level THEN Error(8);
        ELSIF t^.level=0FFh THEN Error(4);
        END;
        p:=t; Free(p,SIZE(Two)); t:=t^.next;
      END;
      RETURN;
    END;
    h:=f; f:=f^.next;
  END;
END PutLabel;

PROCEDURE FinishCheck;
(* Проверяет, есть ли ненайденые метки *)
  VAR f:PF;
BEGIN
  IF Forward=NIL THEN RETURN; END;
  f:=Forward;
  Show("Не найдены метки:"); INC(ErrorCo);
  WHILE f#NIL DO
    print('   %d',f^.label);
    f:=f^.next;
  END;
  print("  \n");
END FinishCheck;

PROCEDURE ExitBody;
(* Устанавливает об'ласти видимости *)
(* всем меткам завершенного тела    *)
  VAR e:PE; f:PF; t:PT;
BEGIN e:=Exist;
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

PROCEDURE Goto(cond,fixed:BOOLEAN; tolabel:INTEGER);
  VAR t:PT; f,h:PF; e:PE;
(* Если label не в списке Exist то label заносится в список Forward *)
(* в соответствии с cond. генерируется JUMP  *)
BEGIN
  e:=Exist;
  WHILE e#NIL DO
    IF e^.label#tolabel THEN e:=e^.next;
    ELSIF e^.cp<0 THEN Error(26); RETURN;
    ELSIF e^.level<=level THEN
      cJump(tolabel,cond,fixed); RETURN;
    ELSE Error(8); RETURN ;
    END;
  END;
  f:=Forward;
  WHILE f#NIL DO
    IF f^.label=tolabel THEN -- ASSERT(f^.from#NIL);
      Give(t,SIZE(Two)); t^.level:=level;
      cJump(tolabel,cond,fixed);
      t^.next:=f^.from; f^.from:=t; RETURN;
    ELSE h:=f; f:=f^.next; END;
  END;
  Give(f,SIZE(F));
  IF Forward=NIL THEN
    Forward:=f;
  ELSE h^.next:=f; END;
  f^.label:=tolabel; f^.next:=NIL;
  Give(t,SIZE(Two));
  f^.from:=t; t^.level:=level; t^.next:=NIL;
  cJump(tolabel,cond,fixed);
END Goto;

PROCEDURE GetFmt(lab: INTEGER);
  VAR t:PT; f,h:PF; e:PE;
BEGIN
  e:=Exist;
  WHILE e#NIL DO
    IF e^.label#lab THEN e:=e^.next;
    ELSIF e^.level=0FFh THEN
      IF e^.cp>=0 THEN Error(04) END;
      LdFormat(-e^.cp); RETURN;
    ELSE Error(4); RETURN ;
    END;
  END;
  f:=Forward;
  WHILE f#NIL DO
    IF f^.label=lab THEN -- ASSERT(f^.from#NIL);
      Give(t,SIZE(Two)); t^.level:=0FFh;
      cLdFormat(lab);
      t^.next:=f^.from; f^.from:=t; RETURN;
    ELSE h:=f; f:=f^.next;
    END;
  END;
  Give(f,SIZE(F));
  IF Forward=NIL THEN Forward:=f; ELSE h^.next:=f; END;
  f^.label:=lab; f^.next:=NIL;
  Give(t,SIZE(Two));
  f^.from:=t; t^.level:=0FFh; t^.next:=NIL;
  cLdFormat(lab);
END GetFmt;

PROCEDURE SetFmt(lab: INTEGER; strofs: INTEGER);
  VAR t,p:PT; f,h:PF; e,i:PE;
(* Вносит метку в список описанных и разбирает все forward-ссылки на нее; *)
(* формирует метку формата в obj-файле *)
BEGIN
  IF lab=0 THEN Error(59); RETURN ; END;
  IF Exist=NIL THEN Give(Exist,SIZE(E)); e:=Exist;
  ELSE e:=Exist;
    WHILE e#NIL DO
      IF e^.label=lab THEN Error(9); RETURN; END;
      i:=e; e:=e^.next;
    END;
    Give(e,SIZE(E)); i^.next:=e;
  END;
  e^.label:=lab; e^.level:=0FFh; e^.cp:=-strofs; e^.next:=NIL;
  pFormat(lab,strofs);
  f:=Forward;
  WHILE f#NIL DO
    IF f^.label=lab THEN
      IF f=Forward THEN Forward:=f^.next; ELSE h^.next:=f^.next; END;
      t:=f^.from; -- ASSERT(t#NIL);
      Free(f,SIZE(F));
      WHILE t#NIL DO
        IF t^.level<0FFh THEN Error(26); END;
        p:=t; Free(p,SIZE(Two)); t:=t^.next;
      END;
      RETURN;
    END;
    h:=f; f:=f^.next;
  END;
END SetFmt;

END Labels;

(* ------------------ E n d   L a b e l s ---------------------- *)

(* ----------------- S t a t e m e n t s ----------------------- *)

CONST FFCT=9Fh;  RTN=0CAh;  JSFC=1Ah; LEQ=0A1h; JSF=1Bh; ADD=88h;
      copt=0B5h; LSS=0A0h;  Not=0AEh; LSA=16h;  GEQ=0A3h;EQ=0A4h;
      SXW=51h;   LSTA=0C2h; CX=0CCh;  Drop=0B1h;FCMP=9Ch;
      ALLOC=0C8h;FADD=98h;  FSUB=99h; STOT=0E8h;FDIV=9Bh; DECS=0B0h;
      GRT=0A2h;  Div=8Bh;   INC1=0E4h;DEC1=0E5h;Inc=0E6h; SUB=89h;
      XIT=0BBh;  ENTRC=0BAh;RET=0CAh;

CONST ForLib=2;  PAUSE= 2;  STOP=1; ChTop=2;

PROCEDURE AssToVar(VAR ob:Dexpr);
VAR I:Info;
BEGIN
  ob.tp:=GetType(ob.name); ob.emd:=var;
  GetSy;
  I.name:=ob.name; Unpack(I);
  IF ob.tp=Char THEN
    ldSubstr(I,ob);
  ELSE
    LeftAdr(I,ob);
  END;
END AssToVar;

PROCEDURE AssToArr(VAR ob:Dexpr);
VAR I:Info;
BEGIN
  ob.tp:=GetType(ob.name); ob.emd:=arr;
  I.name:=ob.name; Unpack(I);
  GetSy; IF sy#lpar THEN Expected(lpar); RETURN END;
  IF ob.tp#Char THEN LoadAdr(I); END;
  ob.emd:=arrel; ob.wd:=LoadInd(I);
  GetSy;
  IF ob.tp=Char THEN ldSubstr(I,ob);
  ELSE               LeftAdr (I,ob);
  END;
END AssToArr;

PROCEDURE LeftHand(VAR ob:Dexpr);
VAR I:Info;
BEGIN
  ob.name:=idno; I.name:=idno;
  CASE GetMode(idno) OF
   xVar  : Unpack(I); AllocVar(I); AssToVar(ob);
  |Var   : AssToVar(ob);
  |Array : AssToArr(ob);
  |Empty : GenLocal(I); GenObj(I);
           AllocVar(I); AssToVar(ob);
  ELSE  ErrorId(0); ob.emd:=invmd;
  END;
END LeftHand;

PROCEDURE AssTypeConv(VAR obl,obr:Dexpr):BOOLEAN;
(* TRUE if error *)
BEGIN
  IF (obl.emd=invmd) OR (obr.emd=invmd) THEN
     RETURN TRUE
  END;
  IF obl.tp=obr.tp THEN RETURN FALSE END;
  IF obl.tp=Int THEN
    CASE obr.tp OF
     Real:  IF obr.emd=cexpr THEN obr.name:=TRUNC(REAL(obr.name))
            ELSE c1(FFCT,1);
            END; obr.tp:=obl.tp;
    |Double:
            IF obr.emd=cexpr THEN obr.name:=TRUNC(REAL(obr.name))
            ELSE c1(FFCT,1);
            END; obr.tp:=obl.tp;
    |Complex:
            IF obr.emd=cexpr THEN obr.name:=TRUNC(REAL(obr.name))
            ELSE c(Drop); epop; c1(FFCT,1);
            END; obr.tp:=obl.tp;
    |Holl:
    ELSE Error(17); RETURN TRUE
    END;
  ELSIF obl.tp=Real THEN
    CASE obr.tp OF
      Int :  IF obr.emd=cexpr THEN obr.name:=WORD(FLOAT(obr.name))
             ELSE c1(FFCT,0);
             END; obr.tp:=obl.tp;
     |Double :
             obr.tp:=obl.tp;
     |Complex:
             IF obr.emd#cexpr THEN
               c(Drop); epop;
             END; obr.tp:=obl.tp;
     |Holl:
    ELSE Error(17); RETURN TRUE
    END;
  ELSIF obl.tp=Double THEN
    CASE obr.tp OF
      Int :  IF obr.emd=cexpr THEN obr.name:=WORD(FLOAT(obr.name))
             ELSE c1(FFCT,0);
             END; obr.tp:=obl.tp;
     |Real :
             obr.tp:=obl.tp;
     |Complex:
             IF obr.emd#cexpr THEN
               c(Drop); epop;
             END; obr.tp:=obl.tp;
     |Holl:
    ELSE Error(17); RETURN TRUE
    END;
  ELSIF obl.tp=Complex THEN
    CASE obr.tp OF
      Int :  IF obr.emd=cexpr THEN obr.name:=WORD(FLOAT(obr.name));
                                   obr.wd:=0;
             ELSE c1(FFCT,0); li(0);
             END; obr.tp:=obl.tp;
     |Real : IF obr.emd=cexpr THEN obr.wd:=0;
             ELSE li(0);
             END; obr.tp:=obl.tp;
     |Double :
             IF obr.emd=cexpr THEN obr.wd:=0;
             ELSE li(0);
             END; obr.tp:=obl.tp;
     |Holl:
    ELSE Error(17); RETURN TRUE
    END;
  ELSE Error(17); RETURN TRUE
  END;
  RETURN FALSE
END AssTypeConv;

PROCEDURE Assign():BOOLEAN; (* TRUE, if Assign-statement *)
  VAR obl,obr:Dexpr;
      Left:INTEGER;
      mL:Mode; I:Info;
BEGIN
  IF NOT lookAhead('=') THEN RETURN FALSE END;
  MarkPos; GetSy;
  IF sy#id THEN Error(25); RETURN TRUE END;
  mL:=GetMode(idno); Left:=idno;
  MarkPos1; GetSy;
  IF sy=becomes THEN
    IF (Ident[0]="D") AND (Ident[1]="O")
       AND (ORD(Ident[2])-ORD('0') IN {0..9} )
    THEN
      IF lookAhead(',') THEN
        IF mL=Empty THEN DelId(Left) END;
        BackUp; RETURN FALSE
      END;
    END;
  ELSIF sy=lpar THEN
    IF mL#Array THEN
      IF (Ident[0]="I") AND (Ident[1]="F") AND (Ident[2]=0c) THEN
        IF mL=Empty THEN DelId(Left) END;
        BackUp; RETURN FALSE
      ELSIF lookAhead(':') THEN
      ELSE ErrorId(0); RETURN TRUE
      END;
    END;
  ELSE BackUp; RETURN FALSE
  END;
  BackUp1; idno:=Left; sy:=id;
  LeftHand(obl);
  IF sy#becomes THEN Expected(becomes); SkipTo(becomes); END;
  GetSy; Expr(obr);
  IF sy#EOL THEN Error(16) END;
  IF AssTypeConv(obl,obr) THEN RETURN TRUE END;
  IF obr.emd=cexpr THEN
    LoadConst(obr);
  END;
  StoreIn(obl,obr);
  IF obl.tp=Char THEN
     IF obr.emd=expr THEN
       llw(INTEGER(obr.wd)+1); sew(ForLib,ChTop);
     END;
     FreeDescs;
  END;
  RETURN TRUE;
END Assign;

PROCEDURE GetLabel():BOOLEAN;
BEGIN
  GetSy; IF sy#const THEN Error(4); RETURN FALSE END;
  IF (cType#Int) OR (Ival<1) OR (Ival>99999) THEN
     Error(4); RETURN FALSE
  ELSE
     RETURN TRUE
  END
END GetLabel;

PROCEDURE gLoclab():INTEGER;
BEGIN
  INC(Loclab); RETURN Loclab-1
END gLoclab;

PROCEDURE Call;
  VAR I:Info;
BEGIN
  SetWork(ass); GetSy;
  IF sy#id THEN Error(25); RETURN  END;
  I.name:=idno;
  CASE GetMode(idno) OF
    Proc: Unpack(I); IF (I.cl#Ext) AND (I.cl#Param) THEN
                        ErrorId(4);
                     END;
   |xVar : Unpack(I); IF I.cl#Param THEN Warning(2) END;
           AllocExt(I);
   |Empty: GenLocal(I); GenObj(I); AllocExt(I);
   ELSE  ErrorId(4)
  END;
  SubrCall(I);
  GetSy; IF sy#EOL THEN Error(16); RETURN; END;
END Call;

PROCEDURE Return;
VAR ob:Dexpr; I:Info;
BEGIN
  IF (self=main) THEN Error(44); RETURN END;
  GetSy;
  IF sy=EOL THEN
    IF self=function THEN
       I.name:=ModIdno; Unpack(I);
       IF GetType(ModIdno)#Char THEN LoadVal(I);
       ELSE                          LoadAdr(I);
       END;
    ELSIF self=subrA THEN
       li(0); -- for alternate return
    END;
  ELSIF self#subrA THEN
    Error(56); RETURN
  ELSE
    iExpr(ob);
    IF ob.emd=cexpr THEN
      li(ob.name);
    END;
  END;
  c(RET);
END Return;

PROCEDURE End;
VAR  I:Info;
BEGIN
  GetSy;
  IF sy#EOL THEN Error(16) END;
  IF self=function THEN
    I.name:=ModIdno; Unpack(I);
    IF GetType(ModIdno)#Char THEN LoadVal(I);
    ELSE                          LoadAdr(I);
    END;
  ELSIF self=subrA THEN
    li(0); -- for alternate return
  END;
END End;

VAR svsy:Symbol;
PROCEDURE Simple():BOOLEAN;FORWARD;
VAR valexp:WORD; consexp:BOOLEAN;

PROCEDURE If3(VAR ob:Dexpr);
(* Разбор трехзубого IF-a; арифм. выражение на стеке *)
  VAR loclab, temp, l1, l2, l3 :INTEGER;
      tp:Types;
BEGIN
  IF GetLabel() THEN l1:=Ival; GetSy;
    IF sy#comma THEN Expected(comma); RETURN; END;
  ELSE RETURN;
  END;
  IF GetLabel() THEN l2:=Ival; GetSy;
    IF sy#comma THEN Expected(comma); RETURN; END;
  ELSE RETURN;
  END;
  IF GetLabel() THEN l3:=Ival; GetSy;
    IF sy#EOL THEN Error(16); RETURN; END;
  ELSE RETURN;
  END;
  tp:=ob.tp;
  IF consexp THEN
    IF (tp=Int) OR (tp=Real) THEN
      li(valexp);
    ELSE
    --  load dpconstant
      li(valexp);
    END;
  END;
  IF (l1=l2) AND (l2=l3) THEN
    IF (tp=Int) OR (tp=Real) THEN
      c(Drop); epop; Goto(FALSE,FALSE,l1);
    ELSE  -- Double
      c(Drop); epop; Goto(FALSE,FALSE,l1);
    END;
  ELSIF l1=l2 THEN
    IF    tp=Int  THEN
      li(0); c(LEQ); epop; epop;
      Goto(TRUE,FALSE,l3);
      Goto(FALSE,FALSE,l1);
    ELSIF tp=Real THEN
      li(0); c(FCMP); c(LEQ); epop; epop;
      Goto(TRUE,FALSE,l3);
      Goto(FALSE,FALSE,l1);
    ELSE  -- Double
      li(0); c(FCMP); c(LEQ); epop; epop;
      Goto(TRUE,FALSE,l3);
      Goto(FALSE,FALSE,l1);
    END;
  ELSIF l1=l3 THEN
    IF (tp=Int) OR (tp=Real) THEN
       epop; Goto(TRUE,FALSE,l2);
             Goto(FALSE,FALSE,l1);
    ELSE  -- Double
       epop; Goto(TRUE,FALSE,l2);
             Goto(FALSE,FALSE,l1);
    END;
  ELSIF l2=l3 THEN
    IF    tp=Int  THEN
      li(0); c(GEQ); epop; epop;
      Goto(TRUE,FALSE,l1);
      Goto(FALSE,FALSE,l2);
    ELSIF tp=Real THEN
      li(0); c(FCMP); c(GEQ); epop; epop;
      Goto(TRUE,FALSE,l1);
      Goto(FALSE,FALSE,l2);
    ELSE  -- Double
      li(0); c(FCMP); c(GEQ); epop; epop;
      Goto(TRUE,FALSE,l1);
      Goto(FALSE,FALSE,l2);
    END;
  ELSE   -- general case
    IF    tp=Int  THEN
      AllocTemp(temp,1);
      c(copt); epush; slw(temp);
      Goto(TRUE,FALSE,l2);
      llw(temp); li(0); c(LSS); epop; epop;
      Goto(TRUE,FALSE,l3);
      Goto(FALSE,FALSE,l1);
      FreeTemp(temp,1);
    ELSIF tp=Real THEN
      AllocTemp(temp,1);
      c(copt); epush; slw(temp);
      Goto(TRUE,FALSE,l2);
      llw(temp); li(0); c(FCMP); c(LSS); epop; epop;
      Goto(TRUE,FALSE,l3);
      Goto(FALSE,FALSE,l1);
      FreeTemp(temp,1);
    ELSE  -- Double
      AllocTemp(temp,1);
      c(copt); epush; slw(temp);
      Goto(TRUE,FALSE,l2);
      llw(temp); li(0); c(FCMP); c(LSS); epop; epop;
      Goto(TRUE,FALSE,l3);
      Goto(FALSE,FALSE,l1);
      FreeTemp(temp,1);
    END;
  END;
END If3;

PROCEDURE IfL;
   VAR loclab:INTEGER;
(* Разбор логического IF-a; *)
(* на стеке логическое выражение *)
BEGIN
  IF consexp THEN
    li(valexp);
  END;
  loclab:=gLoclab(); cJump(loclab,TRUE,FALSE); epop;
  IF Simple() THEN
    moveCode; pLabel(loclab);
    IF svsy=frmt THEN Error(18); END;
  ELSIF svsy=invKW THEN Error(41); RETURN
  ELSE                  Error(18); RETURN
  END;
  GetSy; IF sy#EOL THEN Error(16); END;
END IfL;

PROCEDURE If;
  VAR filab,nextlab:INTEGER;
      tp:Types;
      w:WORD;
      ob:Dexpr;
BEGIN
  SetWork(ass); GetSy;
  IF sy#lpar THEN Expected(lpar); RETURN; END;
  GetSy; Expr(ob);
  consexp:=ob.emd=cexpr; valexp:=ob.name; tp:=ob.tp;
  IF sy#rpar THEN Expected(rpar); RETURN; END;
  IF ORD(tp)<=ORD(Double) THEN If3(ob); RETURN;
  ELSIF tp#Logic THEN Error(7); END;
  MarkPos; SetWork(kw); GetSy; SetWork(ass);
  IF sy#then THEN BackUp; IfL; RETURN; END;
(* Структурный IF *)
  IF consexp THEN
    li(INTEGER(valexp));
  END;
  INC(level); INC(iflevel);
  filab:=gLoclab(); nextlab:=gLoclab();
  cJump(nextlab,TRUE,FALSE); moveCode;  epop;
  REPEAT
    NewLine; Statement;
  UNTIL (sy=else) OR (sy=elseif) OR (sy=endif) OR EndUnit;
  WHILE sy=elseif DO
    ExitBody; DEC(level);
    cJump(filab,FALSE,FALSE); moveCode;
    pLabel(nextlab);
    GetSy; IF sy#lpar THEN Expected(lpar); DEC(iflevel); RETURN; END;
    SetWork(ass); GetSy; Lexpr(ob);
    IF ob.tp#Logic THEN Error(7); END;
    IF ob.emd=cexpr THEN
      li(ob.name);
    END; epop;
    IF sy#rpar THEN Expected(rpar); DEC(iflevel); RETURN; END;
    SetWork(kw); GetSy; SetWork(ass);
    IF sy#then THEN Expected(then); DEC(iflevel); RETURN; END;
    GetSy; IF sy#EOL THEN Error(16); END;
    nextlab:=gLoclab(); cJump(nextlab,TRUE,FALSE); moveCode;
    INC(level);
    REPEAT
      NewLine; Statement;
    UNTIL (sy=else) OR (sy=elseif) OR (sy=endif) OR EndUnit;
  END;
  IF sy=else THEN
    cJump(filab,FALSE,FALSE); moveCode;
    pLabel(nextlab);
    ExitBody;
    GetSy; IF sy#EOL THEN Error(16); END;
    REPEAT
      NewLine; Statement;
    UNTIL (sy=endif) OR (sy=else) OR (sy=elseif) OR EndUnit;
  ELSIF sy#endif THEN
    Expected(endif); DEC(level);
    DEC(iflevel); RETURN;
  ELSE
    ExitBody; DEC(level);
    GetSy; IF sy#EOL THEN Error(16); END; DEC(iflevel);
    moveCode;
    pLabel(nextlab); pLabel(filab);
    RETURN;
  END;
  IF sy#endif THEN
    Expected(endif); DEC(level);
    DEC(iflevel); RETURN;
  END;
  ExitBody; DEC(level);
  GetSy; IF sy#EOL THEN Error(16); END; DEC(iflevel);
  moveCode; pLabel(filab);
END If;

PROCEDURE DoStatement(endlabel:INTEGER); FORWARD;

PROCEDURE Do;
(* Генерация цикла  DO <endlabel> I=I0,N,By *)
  VAR dotype                    :Types;
      endlabel                  :INTEGER;
      Dolab, ODlab              :INTEGER;
      Kir,IR0,Nir,Byir          :WORD;
      const, I0cons,Ncons,Bycons:BOOLEAN;
      dovar                     :INTEGER;
      tK,tBy                    :INTEGER;
      I:Info; ob,obDo:Dexpr;
BEGIN
  IF GetLabel() THEN endlabel:=Ival; ELSE RETURN; END;
  GetSy; IF sy#id THEN Error(25); RETURN; END; dovar:=idno;
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
  GetSy; IF sy#becomes THEN Expected(becomes); RETURN; END;
  gDovarA(I);
  GetSy; aExpr(ob); I0cons:=ob.emd=cexpr;
  IF AssTypeConv(obDo,ob) THEN RETURN; END;
  AllocTemp(tK,1); tBy:=0;
  IF I0cons THEN
    IR0:=ob.name;
    li(IR0); const:=TRUE;
  ELSE
    -- store in temp. var. --
    c(copt); epush; slw(tK); const:=FALSE;
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
      AllocTemp(tBy,1); c(copt); epush; slw(tBy);
    END;
  ELSE Byir:=1; Bycons:=TRUE;
    IF dotype=Real THEN Byir:=1. END;
  END;
  IF sy#EOL THEN Error(16); END;
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
        c(FADD);epop; llw(tBy); c(FDIV); c1(FFCT,1); epop;
      END;
      c(copt); epush; slw(tK);
      li(0); c(GRT); epop; epop;
      Goto(TRUE,FALSE,ODlab);
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
        c(FDIV); c1(FFCT,1); epop;
      END;
    ELSE
      IF dotype=Int THEN
        c(ADD); epop; llw(tBy); c(Div); epop;
      ELSE
        c(FADD); epop; llw(tBy); c(FDIV); c1(FFCT,1); epop;
      END;
    END;
    c(copt); epush; slw(tK);
    li(0); c(GRT);  epop; epop;
    cJump(ODlab,TRUE,FALSE);
  END;
  moveCode;
  pLabel(Dolab); llw(tK); cJump(ODlab,TRUE,FALSE); epop;
  moveCode;
  INC(level); INC(dolevel);
  REPEAT
    DoStatement(endlabel);
  UNTIL Label=endlabel;
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
  moveCode; pLabel(ODlab);
  ExitBody; DEC(level); DEC(dolevel);
  IF I.darea=3 THEN FreeTemp(I.offset,1); END;
  FreeTemp(tK,1); IF tBy>0 THEN FreeTemp(tBy,1) END;
END Do;

PROCEDURE DoStatement(endlabel:INTEGER);
  VAR eND:BOOLEAN;
BEGIN Setdepth(0);
  IF EndUnit THEN Label:=endlabel; RETURN END;
  NewLine;  eND:=(endlabel=Label);
  IF Simple() THEN
    PutLabel(Label,1); moveCode;
    IF eND AND ((svsy=goto) OR (svsy=return) OR (svsy=stop))
    THEN Error(34);
    END;
    RETURN;
  END;
  IF sy=frmt THEN  IF eND THEN Error(34); END;
      RETURN;
  END;
  IF sy=entry THEN Error(36); RETURN END;
  PutLabel(Label,1);
  CASE sy OF
    else,elseif: IF iflevel=0 THEN Error(2); RETURN; END;
                   IF Label#0 THEN Error(26); END;
   |endif:       IF iflevel=0 THEN Error(2); RETURN; END;
   |do :  IF eND THEN Error(34); END; Do;
   |if :  IF eND THEN Error(34); END; If;
   |end:  Error(29); EndUnit:=TRUE;
  ELSE    Error(41)
  END;
END DoStatement;

PROCEDURE AssignTO;
VAR I:Info; md:Mode;
    lab:INTEGER; obl,obr:Dexpr;
BEGIN
  IF GetLabel() THEN  lab:=Ival
  ELSE   RETURN
  END;
  SetWork(kw); GetSy;
  IF sy#to THEN Expected(to); RETURN END;
  SetWork(ass);
  GetSy; IF sy#id THEN Error(25); RETURN END;
  I.name:=idno; md:=GetMode(idno);
  IF md=Empty THEN
    GenLocal(I); GenObj(I); AllocVar(I);
  ELSIF md=xVar THEN
    Unpack(I); AllocVar(I);
  ELSIF md=Var THEN
  ELSE Error(14);  RETURN
  END;
  IF GetType(idno) # Int THEN
    Error(27);  RETURN
  END;
  GetSy; IF sy#EOL THEN Error(16) END;
  obl.name:=idno; obl.start:=cp; obl.tp:=Int; obl.emd:=var;
  obr.emd:=cexpr; obr.tp:=Int; obr.name:=lab;
  Unpack(I); LeftAdr(I,obl); li(lab); StoreIn(obl,obr);
END AssignTO;

PROCEDURE GoToAss;
VAR I:Info; md:Mode;
    i,offs,labs:INTEGER;
    Lab:ARRAY [0..255] OF INTEGER;
BEGIN
  I.name:=idno; md:=GetMode(idno);
  IF md=Empty THEN
    GenLocal(I); GenObj(I); AllocVar(I);
  ELSIF md=xVar THEN
    Unpack(I); AllocVar(I);
  ELSIF md=Var THEN
  ELSE Error(14);  RETURN
  END;
  IF GetType(idno) # Int THEN
    Error(27);  RETURN
  END;
  GetSy;
  IF sy=EOL THEN Error(66);  RETURN
  ELSIF sy=comma THEN GetSy
  END;
  IF sy#lpar THEN  Expected(lpar);  RETURN END;
  Unpack(I); LoadVal(I);
  labs:=0;
  LOOP
    IF GetLabel() THEN
      Lab[labs]:=Ival; INC(labs);
      IF labs>255 THEN Error(67); labs:=1; END;
    ELSE  RETURN
    END;
    GetSy;
    IF sy#comma THEN EXIT END;
  END;
  IF sy#rpar THEN Expected(rpar)
  ELSE GetSy; IF sy#EOL THEN Error(16) END;
  END;
  AllocTemp(offs,1); slw(offs);
  FOR i:=0 TO labs-1 DO
    llw(offs); li(Lab[i]); c(EQ); epop; epop;
    c1(JSFC,3); Goto(FALSE,TRUE,Lab[i]);
  END;
  -- li(77); CallExt(ForLib,ExError);
  FreeTemp(offs,1);
END GoToAss;

PROCEDURE GoToList;
VAR ob:Dexpr; i,offs,labs:INTEGER;
    Lab:ARRAY [0..255] OF INTEGER;
BEGIN
  labs:=0;
  LOOP
    IF GetLabel() THEN
      Lab[labs]:=Ival; INC(labs);
      IF labs>255 THEN Error(67); labs:=1; END;
    END;
    GetSy;
    IF sy#comma THEN EXIT END;
  END;
  IF sy#rpar THEN Expected(rpar) END;
  GetSy; IF sy=comma THEN GetSy END;
  iExpr(ob);
  IF ob.emd=cexpr THEN
    li(ob.name);
  END;
  IF sy#EOL THEN Error(16) END;
  offs:=5*labs+1;
  c(ENTRC); b2(offs);
  FOR i:=0 TO labs-1 DO
    li(1); c(DECS); Goto(FALSE,TRUE,Lab[i]); epop;
  END;
  c(XIT);
  b2(1); b2(labs); b2(7); INC(offs,8);
  FOR i:=1 TO labs DO  b2(offs); DEC(offs,3); END;
END GoToList;

PROCEDURE GoTo;
BEGIN
  GetSy;
  IF sy=const THEN
    IF (cType=Int) AND (Ival>0) AND (Ival<=99999) THEN
      Goto(FALSE,FALSE,Ival);
      GetSy; IF sy#EOL THEN Error(16) END;
    ELSE
      Error(4);  RETURN
    END
  ELSIF sy=id THEN GoToAss
  ELSIF sy=lpar THEN GoToList
  ELSE
    Error(4);
  END;
END GoTo;

PROCEDURE StopPause(proc:INTEGER);
 VAR D:INTEGER;
BEGIN
  GetSy;
  IF sy=EOL THEN
     li(0); li(0);
  ELSIF sy=const THEN
     IF cType=Int THEN
        li(1); li(Ival);
     ELSIF cType=Char THEN
        li(2); AllocDesc(D);
        li(Slen); slw(D+2); li(0); slw(D+1);
        lsta(InPoolStr(Sval)); slw(D); lla(D);
        FreeDesc(D);
     ELSE Error(60)
     END;
  ELSE Error(60);
  END;
  CallExt(ForLib,proc);
END StopPause;

PROCEDURE Stop;
BEGIN
  StopPause(STOP);
END Stop;

PROCEDURE Pause;
BEGIN
  StopPause(PAUSE);
END Pause;


PROCEDURE Simple():BOOLEAN;
  VAR I:Info;
BEGIN SetWork(ass);
  IF Assign() THEN  RETURN TRUE; END;
  SetWork(kw); GetSy; MarkPos;
  SetWork(ass); svsy:=sy;
  CASE sy OF
    call:    Call;
   |goto:    GoTo;
   |cont : (* nothing *)
   |return : Return;
   |stop :   Stop;
   |wr   :   Write;
   |rd   :   Read;
   |pr   :   Print;
   |asgn :   AssignTO;
   |pause:   Pause;
   |rewind:  Rewind;
   |backsp:  Backspace;
   |endfile: Endfile;
   |open   : Open;
   |close  : Close;
   |inquire: Inquire;
   |frmt   : Format; sy:=frmt; RETURN FALSE
  ELSE BackUp; sy:=svsy; RETURN FALSE;
  END;
  RETURN TRUE;
END Simple;

PROCEDURE Statement;
-- VAR cp0:INTEGER;
BEGIN
  Setdepth(0); -- cp0:=cp;
  IF Simple() THEN PutLabel(Label,1); moveCode; RETURN; END;
  IF sy=frmt THEN RETURN; END;
  IF sy=entry THEN
    IF level#0 THEN Error(36) END;
    Entry;  RETURN
  END;
  PutLabel(Label,1);
  CASE sy OF
    else,elseif: IF iflevel=0 THEN Error(2); RETURN; END;
                      IF Label#0 THEN Error(26); END;
   |endif:       IF iflevel=0 THEN Error(2); RETURN; END;
   |do : Do;
   |if : If;
   |end: IF level#0 THEN Error(29); END; EndUnit:=TRUE; End;
  ELSE Error(41)
  END;
  moveCode;
END Statement;

PROCEDURE InitStm;
BEGIN
  level:=0;
  dolevel:=0;
  iflevel:=0;
  maxdolv:=0;
  EndUnit:=FALSE;
END InitStm;

END fcStm.
