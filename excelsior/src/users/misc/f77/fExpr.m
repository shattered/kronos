IMPLEMENTATION MODULE fExpr; (* Max *)

FROM fGen   IMPORT lgw, epop, c, c1, li, Store, Lodfv, Setdepth, Alloc,
                   LoadLocVal, InsertCode, cp, LoadVal, CL, Getdepth,
                   llw, lsw, MarkC1, BackC1, LoadAdr, CallExt, epush;
FROM fScan  IMPORT GetType,GetSy,Symbol,Mode,GetMode,sy,ErrorId,Expected,
                   Error,idno,Ival,Rval,MarkPos1,BackUp1,Unpack,Info,head,
                   GenObj,SetMode,Warning,Idname,IdStr,PointPS,FAULT,SetType;
FROM fDcl   IMPORT GiveGW;
FROM fExt   IMPORT SetParList, GetParam;
FROM fScan  IMPORT Types, Descriptor;
FROM SYSTEM IMPORT WORD;


(*     Грамматика FORTRAN - Выражений :
  <expr>  = <expr1> {".OR." <expr1>}.
  <expr1> = <expr2> {".AND." <expr2>}.
  <expr2> = [".NOT."] <expr3>.
  <expr3> = .FALSE.|.TRUE.|<simple> [<relation> <simple>].
  <relation> = ".LT."|".GT."|".LE."|".GE."|".NE."|".EQ." .
  <simple> = <term> {("+"|"-")<term>}.
  <term>   = <factor> {("*"|"/")<factor>}.
  <factor> = [("+"|"-")]<factor1>
  <factor1>= <factor2>["**" <factor2>].
  <factor2>= "(" <expr> ")"|<constant>|<realconstant>|<var>
             |<parameter>|<array_elem>|<funcCall> .
  <array_elem> = <ident>"("<expr> {"," <expr>}")" .       *)

MODULE Constantexpresion ;

(*     Грамматика константных FORTRAN - Выражений :
  <cexpr>   = <cexpr1> {".OR." <cexpr1>}.
  <cexpr1>  = <cexpr2> {".AND." <cexpr2>}.
  <cexpr2>  = [".NOT."] <cexpr3>.
  <cexpr3>  = .FALSE.|.TRUE.|<csimple> [<relation> <csimple>].
  <csimple> = <cterm> {("+"|"-")<cterm>}.
  <cterm>   = <cfactor> {("*"|"/")<cfactor>}.
  <cfactor> = [("+"|"-")]<cfactor1>
  <cfactor1>= <cfactor2>["**" <cfactor2>].
  <cfactor2>= "(" <cexpr> ")"|<constant>|<realconstant>|<parameter>. *)

IMPORT GetType, GetSy, Symbol, Mode, Types, Expr, Expected,
       GetMode, sy, Error, idno, Ival, Rval, WORD, Info, Unpack;
EXPORT val, cExpr;

VAR val:WORD;

PROCEDURE cExpr(VAR tp:Types):BOOLEAN; FORWARD;

PROCEDURE cFactor2(VAR tp:Types):BOOLEAN;
  VAR I:Info;
BEGIN (* Возвращает тип cFactor2 *)
  CASE sy OF
   lpar     : GetSy;
              IF NOT cExpr(tp) THEN RETURN FALSE;
              ELSIF sy#rpar THEN Expected(rpar); RETURN TRUE; END;
  |const    : tp:=Int;  val:=Ival; RETURN TRUE;
  |realconst: tp:=Real; val:=Rval; RETURN TRUE;
  |id       : IF  GetMode(idno)=Const  THEN
                tp:=GetType(idno); I.name:=idno;
                Unpack(I); val:=I.offset; RETURN TRUE;
              ELSE RETURN FALSE;
              END;
  ELSE RETURN FALSE;
  END;
END cFactor2;

PROCEDURE cFactor1(VAR tp:Types):BOOLEAN;
  VAR tp1:Types;
BEGIN
  IF cFactor2(tp) THEN GetSy;
    IF sy=power THEN GetSy; (* i t d *)
    ELSE RETURN TRUE;
    END;
  ELSE RETURN FALSE; END;
END cFactor1;

PROCEDURE cFactor(VAR tp:Types):BOOLEAN;
BEGIN
  IF sy=plus THEN GetSy; RETURN cFactor1(tp);
  ELSIF sy=minus THEN GetSy;
    IF NOT cFactor1(tp) THEN RETURN FALSE;
    ELSE
      CASE tp OF
         Int: INTEGER(val):=-INTEGER(val);
       |Real: REAL(val):=-REAL(val);
      ELSE END;
      RETURN TRUE;
    END;
  ELSE RETURN cFactor1(tp);
  END;
END cFactor;

PROCEDURE cTerm(VAR tp:Types):BOOLEAN;
  VAR tp1:Types; op:Symbol; v:WORD;
BEGIN
  IF cFactor(tp) THEN v:=val;
    WHILE (sy=slash) OR (sy=times) DO
      op:=sy; GetSy; IF NOT cFactor(tp1) THEN RETURN FALSE; END;
      IF (tp=Real) OR (tp1=Real) THEN
        IF  tp=Int THEN REAL(v):=FLOAT(INTEGER(v)); END;
        IF tp1=Int THEN REAL(val):=FLOAT(INTEGER(val)); END;
        tp:=Real;
        IF  op=slash THEN REAL(v):=REAL(v)/REAL(val);
        ELSE REAL(v):=REAL(val)*REAL(v);
        END;
      ELSIF op=slash THEN INTEGER(v):=INTEGER(v) DIV INTEGER(val);
      ELSE INTEGER(v):=INTEGER(val)*INTEGER(v);
      END;
    END;
    val:=v; RETURN TRUE;
  ELSE RETURN FALSE; END;
END cTerm;

PROCEDURE cSimple(VAR tp:Types):BOOLEAN;
  VAR tp1:Types; op:Symbol; v:WORD;
BEGIN
  IF cTerm(tp) THEN v:=val;
    WHILE (sy=plus) OR (sy=minus) DO
      op:=sy; GetSy; IF NOT cTerm(tp1) THEN RETURN FALSE; END;
      IF (tp=Real) OR (tp1=Real) THEN
        IF  tp=Int THEN REAL(v):=FLOAT(INTEGER(v)); END;
        IF tp1=Int THEN REAL(val):=FLOAT(INTEGER(val)); END;
        tp:=Real;
        IF  op=plus THEN REAL(v):=REAL(v)+REAL(val);
        ELSE REAL(v):=REAL(v)-REAL(val);
        END;
      ELSIF op=plus THEN INTEGER(v):=INTEGER(v)+INTEGER(val);
      ELSE INTEGER(v):=INTEGER(v)-INTEGER(val);
      END;
    END;
    val:=v; RETURN TRUE;
  ELSE RETURN FALSE; END;
END cSimple;

PROCEDURE cExpr3(VAR tp:Types):BOOLEAN;
  VAR tp1:Types; op:Symbol; v:WORD;
BEGIN
  IF (sy=false) OR (sy=true) THEN
    INTEGER(val):=ORD(sy)-ORD(false);
    tp:=Logic; GetSy; RETURN TRUE;
  END;
  IF cSimple(tp) THEN v:=val;
    IF (ORD(sy)<=ORD(gt)) AND (ORD(sy)>=ORD(lt)) THEN
      op:=sy; GetSy; IF NOT cSimple(tp1) THEN RETURN FALSE; END;
      IF (tp=Int) AND (tp1=Int) THEN tp:=Logic;
        CASE op OF
          lt: BOOLEAN(val):=INTEGER(v) <  INTEGER(val);
        | le: BOOLEAN(val):=INTEGER(v) <= INTEGER(val);
        | eq: BOOLEAN(val):=INTEGER(v) =  INTEGER(val);
        | ne: BOOLEAN(val):=INTEGER(v) #  INTEGER(val);
        | ge: BOOLEAN(val):=INTEGER(v) >= INTEGER(val);
        | gt: BOOLEAN(val):=INTEGER(v) >  INTEGER(val);
        END;
      ELSE (* Не понятно *) END;
    END;
    RETURN TRUE;
  ELSE RETURN FALSE; END;
END cExpr3;

PROCEDURE cExpr2(VAR tp:Types):BOOLEAN;
BEGIN
  IF sy=not THEN GetSy;
    IF NOT cExpr3(tp) THEN RETURN FALSE; END;
    IF tp#Logic THEN Error(13); tp:=Logic;
    ELSE BOOLEAN(val):=NOT (BOOLEAN(val)); END;
  ELSIF NOT cExpr3(tp) THEN RETURN FALSE;
  END;
  RETURN TRUE;
END cExpr2;

PROCEDURE cExpr1(VAR tp:Types):BOOLEAN;
  VAR v:WORD;
BEGIN
  IF NOT cExpr2(tp) THEN RETURN FALSE; END;
  WHILE sy=and DO v:=val;
    IF tp#Logic THEN Error(13); tp:=Logic; END;
    GetSy; IF NOT cExpr2(tp) THEN RETURN FALSE; END;
    IF tp#Logic THEN Error(13); tp:=Logic; END;
    BOOLEAN(val):=BOOLEAN(v) AND BOOLEAN(val);
  END; RETURN TRUE;
END cExpr1;

PROCEDURE cExpr(VAR tp:Types):BOOLEAN;
  VAR v:WORD;
BEGIN
  IF NOT cExpr1(tp) THEN RETURN FALSE; END;
  WHILE sy=or DO v:=val;
    IF tp#Logic THEN Error(13); tp:=Logic; END;
    GetSy; IF NOT cExpr2(tp) THEN RETURN FALSE; END;
    IF tp#Logic THEN Error(13); tp:=Logic; END;
    BOOLEAN(val):=BOOLEAN(v) OR BOOLEAN(val);
  END; RETURN TRUE;
END cExpr;

END Constantexpresion ;

CONST LSS =0A0h; LEQ =0A1h; GTR =0A2h; NEG =0A7h;
      GEQ =0A3h; EQU =0A4h; NEQ =0A5h; LXW  =41h;
      FFCT =9Fh; FADD =98h; FSUB =99h; FABS =9Dh;
      FMUL =9Ah; FDIV =9Bh; FNEG =9Eh; FCMP =9Ch;
      ADD  =88h; SUB  =89h; MUL  =8Ah; Div  =8Bh;
      And =0A9h; Or  =0A8h; Not =0AEh; LSA  =16h;
      CHKZ=0C7h; Drop=0B1h; Stot=0E8h; DECS=0B0h;
      SXW  =51h; LGW2 =42h; LXA =0EAh;

MODULE CallExternal; (* Вызов подпрограмы *)

IMPORT Mode, Types, idno, sy, GetSy, Warning, Error, FAULT, GetMode, GetType,
       Info, Unpack, GenObj, MarkPos1, BackUp1, Expected, Symbol, lsw, LSA,
       Alloc, LGW2, DECS, Drop, WORD, SetType, MarkC1, BackC1, cp, li,
       LoadAdr, InsertCode, lgw, llw, epop, epush, CallExt, c, Stot,
       SXW, ADD, SetParList, GetParam, LoadInd, Expr, GiveGW, SetMode;

EXPORT ExtCall;

TYPE modpar=(var,array,arrayelem);
VAR Idno:INTEGER; BOU:INTEGER;

PROCEDURE ParamIsExpr?(VAR m:modpar):BOOLEAN;
  VAR I:Info; M:Mode;
(*Возвращает TRUE, если параметр есть выражение => не может иметь адрес*)
(*Иначе m-вид параметра,если m=arrayelem,то грузится его адрес *)
BEGIN
  MarkPos1; GetSy; IF sy#id THEN BackUp1; RETURN TRUE; END;
  Idno:=idno; GetSy; M:=GetMode(Idno);
  IF (sy=coma) OR (sy=rpar) THEN
    IF M=Var THEN m:=var;
    ELSIF M=Array THEN m:=array;
    ELSIF M=Empty THEN (* Генерируем новый об'ект-Var *)
      Warning(1); SetMode(idno,Var);
      I.name:=idno; I.param:=FALSE;
      I.offset:=GiveGW(); GenObj(I);
      m:=var;
    ELSE Error(53); FAULT; END;
    RETURN FALSE;
  ELSIF (sy=lpar) AND (M=Array) THEN
    MarkC1; I.name:=Idno; Unpack(I);
    BOU:=cp;
    IF I.param THEN llw(I.offset); lsw(0);
    ELSE lgw(I.offset); END;
    IF LoadInd(I.desc^,I.dimension,I.abs) THEN
      c(ADD); epop; (* на стеке адрес индексированного значения *)
      GetSy;
      IF (sy=coma) OR (sy=rpar) THEN m:=arrayelem; RETURN FALSE;
      ELSE BackC1; BackUp1; RETURN TRUE; END;
    ELSE FAULT;
    END;
  ELSE BackUp1; RETURN TRUE;
  END;
END ParamIsExpr?;

VAR parwa:INTEGER; (* параметры без адреса (ПБА)*)

PROCEDURE Standart;
(* Стандартные действия при загрузке параметров через буфер ПБА *)
BEGIN
  c(SXW); epop; epop; epop;
  lgw(2); IF parwa#0 THEN c(LSA); c(parwa); END; (*адрес в буфере ПБА*)
  IF parwa>9 THEN Error(45); END; INC(parwa);
END Standart;

PROCEDURE LoadParam(mp:modpar; m:Mode; tp:Types);
BEGIN
  IF GetType(Idno)#tp THEN Error(26); END;
  CASE mp OF
   var      : IF m#Var   THEN Error(56); END;
              LoadAdr(Idno);
  |array    : IF m#Array THEN Error(55); END;
              LoadAdr(Idno);
  |arrayelem: IF m=Array THEN (*Элемент массива подставляется как массив*)
              (*Создаем фиктивную базу. База и индекс буфера ПБА:*)
              InsertCode(BOU,LGW2);
              InsertCode(BOU+1,parwa); (* == li(parwa) *) epush; epush;
              Standart;
           (* ELSE m=Var , а адрес уже на стеке *)
              END;
  END; (* case *)
END LoadParam;

PROCEDURE ExtCall;
  VAR parno:INTEGER; m:Mode; T,tp:Types; mp:modpar;
      w:WORD; I:Info;
BEGIN
  I.name:=idno; Unpack(I); SetParList(I.parlist);
  MarkPos1; GetSy;
  IF sy#lpar THEN
    IF GetParam(m,tp) THEN Expected(lpar); FAULT END;
    Unpack(I); CallExt(I.offset); BackUp1; RETURN;
  END;
  parwa:=0; parno:=0; Alloc(5); c(Drop); epop;
  LOOP
    INC(parno); IF NOT GetParam(m,tp) THEN Error(54); FAULT; END;
    IF ParamIsExpr?(mp) THEN
      (* База и индекс буфера ПБА: *) lgw(2); li(parwa);
      MarkPos1; GetSy; IF Expr(T,w) THEN  li(INTEGER(w)); END;
      IF m=Array THEN Error(57); END;
      IF T#tp THEN Error(26); END;
      Standart;
    ELSE
      LoadParam(mp,m,tp);
    END;
    c(Stot); epop;
    IF sy=rpar THEN EXIT; END;
    IF sy#coma THEN Expected(coma); FAULT; END;
  END;
  IF GetParam(m,tp) THEN Warning(0) END;
  li(5+parno); c(DECS); epop; CallExt(I.offset);
END ExtCall;

END CallExternal;

PROCEDURE LoadInd(d:Descriptor; dim,abs:INTEGER):BOOLEAN;
  VAR dm,ind,term,lo:INTEGER; tp:Types;
      w:WORD; notstack:BOOLEAN;
BEGIN
  ASSERT(sy=lpar); dm:=0; notstack:=TRUE; term:=1; ind:=0;
  LOOP
    INC(dm); IF dm > dim THEN Error(32); RETURN FALSE; END;
    MarkPos1; GetSy; lo:=d[dm-1].lo;
    IF Expr(tp,w) THEN
      IF notstack THEN INC(ind,term*(INTEGER(w)-lo));
      ELSE ind:=term*(INTEGER(w)-lo);
        IF ind>255 THEN li(ind); c(ADD); epop;
        ELSIF ind>0 THEN c1(LSA,ind);
        END;
      END;
    ELSE
      IF lo#0 THEN li(lo); c(SUB); epop; END;
      IF term#1 THEN
        IF notstack THEN
          li(term); c(MUL); epop;
        ELSE
          li(term); c(LXA); epop; epop;
        END;
      END;
      IF notstack THEN
        notstack:=FALSE;
        IF (ind>255) OR (ind<0) THEN li(ind); c(ADD); epop;
        ELSIF ind>0 THEN c1(LSA,ind);
        END;
      END;
    END;
    IF tp#Int THEN Error(27); RETURN FALSE; END;
    term:=term*(d[dm-1].hi-lo+1);
    IF sy=rpar THEN EXIT; END;
    IF sy#coma THEN Expected(coma); RETURN FALSE; END;
  END;
  IF dm<dim THEN Error(32); RETURN FALSE; END;
  IF notstack THEN
    IF ind>abs THEN Error(33); RETURN FALSE; END;
    li(ind);
  ELSE
--  li(abs); c(CHKZ); epop;
  END;
  RETURN TRUE;
END LoadInd;

PROCEDURE Apply(VAR L:Types; R:Types; op:Symbol; bou:INTEGER);
BEGIN
  IF (L=Real) OR (R=Real) OR (op=power) THEN
    IF    NOT (L=Real) THEN InsertCode(bou,FFCT); InsertCode(bou+1,0);
    ELSIF NOT (R=Real) THEN c1(FFCT,0);
    ELSE (* nothing *)
    END; L:=Real;
    IF (ORD(op)<ORD(and)) AND (ORD(op)>ORD(minus)) THEN
      c(FCMP); L:=Logic;
    END;
    CASE op OF
      power: c1(FFCT,4);
    | slash: c(FDIV); | times: c(FMUL); | plus : c(FADD); | minus: c(FSUB);
    | lt   : c( LSS); | le   : c( LEQ); | eq   : c( EQU); | ne   : c( NEQ);
    | ge   : c( GEQ); | gt   : c( GTR);
    ELSE Error(12);
    END;
    epop;
  ELSIF (L=Int) AND (R=Int) THEN
    CASE op OF
       slash:c(Div); | times:c(MUL); | plus: c(ADD); | minus: c(SUB);
     | lt  : c(LSS); L:=Logic; | le  : c(LEQ); L:=Logic;
     | eq  : c(EQU); L:=Logic; | ne  : c(NEQ); L:=Logic;
     | ge  : c(GEQ); L:=Logic; | gt  : c(GTR); L:=Logic;
    ELSE Error(12);
    END;
    epop;
  ELSE Error(13); L:=Int; (*для борьбы с повторными сообщениями*) END;
END Apply;

PROCEDURE Locfunction;
  VAR I:Info; parno:INTEGER; tp:Types; w:WORD; st:BOOLEAN;
BEGIN
  I.name:=idno; Unpack(I);
  GetSy; IF sy#lpar THEN Expected(lpar); FAULT; END;
  st:=(Getdepth()#0);
  parno:=1; IF st THEN Store; END;
  LOOP
    MarkPos1; GetSy; IF Expr(tp,w) THEN li(INTEGER(w)); END;
    IF tp#I.partypes^[parno-1] THEN ErrorId(6); END;
    IF sy=rpar THEN EXIT;
    ELSIF sy=coma THEN
      INC(parno);
      IF parno>I.dimension THEN Error(40); FAULT; END;
    ELSE Expected(coma); FAULT; END;
  END;
  IF parno#I.dimension THEN Error(40); RETURN; END;
  CL(I.offset); IF st THEN Lodfv; ELSE Setdepth(1); END;
END Locfunction;

PROCEDURE array_elem;
  VAR I:Info;
BEGIN
  I.name:=idno;
  GetSy; IF sy#lpar THEN Expected(lpar); FAULT; END;
  Unpack(I);
  IF I.param THEN llw(I.offset); lsw(0);
  ELSE lgw(I.offset); END;
  IF LoadInd(I.desc^,I.dimension,I.abs) THEN c(LXW); END;
  epop;
END array_elem;

VAR FN:INTEGER;

PROCEDURE SetFN(fn:INTEGER);
BEGIN FN:=fn; END SetFN;

PROCEDURE FunctionCall;
  VAR st:BOOLEAN;
BEGIN
  st:=(Getdepth()#0); IF st THEN Store; END;
  ExtCall;
  IF st THEN Lodfv; ELSE Setdepth(1); END;
END FunctionCall;

PROCEDURE CONSTANT;
  VAR I:Info;
BEGIN
  I.name:=idno; Unpack(I); li(INTEGER(I.offset));
END CONSTANT;

PROCEDURE DoEmpty(VAR T:Types);
  VAR I:Info; saveid:INTEGER; Id:Idname; tp:Types; P:PointPS;
BEGIN
  saveid:=idno; MarkPos1; GetSy; BackUp1; idno:=saveid;
  IF sy#lpar THEN
    (* Генерируем новый об'ект Var с предупреждением *)
    Warning(1); SetMode(idno,Var);
    I.name:=idno; I.param:=FALSE;
    I.offset:=GiveGW(); GenObj(I);
    lgw(I.offset);
  ELSE ErrorId(9); FAULT;
  END;
END DoEmpty;

PROCEDURE Factor2(VAR tp:Types);
  VAR v:WORD;
BEGIN (* Возвращает тип Factor2 *)
  CASE sy OF
   lpar     : MarkPos1; GetSy;
              IF Expr(tp,v) THEN li(INTEGER(v)); END;
              IF sy#rpar THEN Expected(rpar); END;
  |id       : tp:=GetType(idno);
              CASE GetMode(idno) OF
                 Var   : LoadVal(idno);
               |Func   : FunctionCall;
               |Subr   : ErrorId(5); FAULT;
               |LocFunc: IF idno=FN THEN Error(39); RETURN; END;
                         Locfunction;
               |Const  : CONSTANT;
               |Array  : array_elem;
               |Empty  : DoEmpty(tp);
               |LocVar : LoadLocVal(idno);
              END;
  |const    : tp:=Int;  li(Ival);
  |realconst: tp:=Real; li(INTEGER(Rval));
  ELSE Error(15); FAULT;
  END;
END Factor2;

PROCEDURE Factor1(VAR tp:Types);
  VAR tp1:Types; bou:INTEGER;
BEGIN
  Factor2(tp); bou:=cp; GetSy;
  IF sy=power THEN GetSy; Factor2(tp1);
     Apply(tp,tp1,power,bou); GetSy;
  END;
END Factor1;

PROCEDURE Factor(VAR tp:Types);
BEGIN
  IF sy=plus THEN GetSy; Factor1(tp);
  ELSIF sy=minus THEN GetSy; Factor1(tp);
    CASE tp OF
      Real: c(FNEG);
     | Int: c( NEG);
    ELSE Error(13); tp:=Int;
    END;
  ELSE Factor1(tp);
  END;
END Factor;

PROCEDURE Term(VAR tp:Types);
  VAR tp1:Types; op:Symbol; bou:INTEGER;
BEGIN
  Factor(tp);
  WHILE (sy=slash) OR (sy=times) DO
    bou:=cp; op:=sy; GetSy; Factor(tp1);
    Apply(tp,tp1,op,bou);
  END;
END Term;

PROCEDURE Simple(VAR tp:Types);
  VAR tp1:Types; op:Symbol; bou:INTEGER;
BEGIN
  Term(tp);
  WHILE (sy=plus) OR (sy=minus) DO
    bou:=cp; op:=sy; GetSy; Term(tp1);
    Apply(tp,tp1,op,bou);
  END;
END Simple;

PROCEDURE Expr3(VAR tp:Types);
  VAR tp1:Types; op:Symbol; bou:INTEGER;
BEGIN
  IF (sy=false) OR (sy=true) THEN
    Error(31); tp:=Logic;
    GetSy; RETURN;
  END;
  Simple(tp); bou:=cp;
  IF (ORD(sy)<ORD(and)) AND (ORD(sy)>ORD(minus)) THEN
    op:=sy; GetSy; Simple(tp1);
    Apply(tp,tp1,op,bou);
  END;
END Expr3;

PROCEDURE Expr2(VAR tp:Types);
BEGIN
  IF sy=not THEN GetSy; Expr3(tp);
    IF tp#Logic THEN Error(13); tp:=Logic; END; c(Not);
  ELSE Expr3(tp); END;
END Expr2;

PROCEDURE Expr1(VAR tp:Types);
BEGIN
  Expr2(tp);
  WHILE sy=and DO
    GetSy;     IF tp#Logic THEN Error(13); tp:=Logic; END;
    Expr2(tp); IF tp#Logic THEN Error(13); tp:=Logic; END;
    c(And); epop;
  END;
END Expr1;

PROCEDURE Expr(VAR tp:Types;VAR value:WORD):BOOLEAN;
BEGIN
  IF cExpr(tp) THEN value:=val; RETURN TRUE; END;
  BackUp1; GetSy; Expr1(tp);
  WHILE sy=or DO
    GetSy;     IF tp#Logic THEN Error(13); tp:=Logic; END;
    Expr1(tp); IF tp#Logic THEN Error(13); tp:=Logic; END;
    c(Or); epop;
  END; RETURN FALSE;
END Expr;

BEGIN
END fExpr.
