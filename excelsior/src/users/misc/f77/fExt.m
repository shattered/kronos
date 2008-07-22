IMPLEMENTATION MODULE fExt; (* Max *)

FROM SYSTEM IMPORT ADDRESS;
FROM fScan  IMPORT Fault, GetSy, Symbol, sy, ParListS, SetWork, Error, idno,
                   Work, IdStr, NewLine, Expected, ErrorId, DelId, SetMode,
                   SetType, GetMode, GetType, BackUp;
FROM fScan  IMPORT Idname, Types, Mode, PointPS;
IMPORT fHeap;

(* ------------------ H E A P --------------------- *)
MODULE HeapSupport;

FROM fHeap IMPORT Allocate, SetAbort;
(*main*)  IMPORT Fault, ADDRESS;

EXPORT  Give, InitHeap;

CONST CurSz=100;

VAR heap:ADDRESS;
    Hfree:INTEGER;

PROCEDURE Give(VAR p:ADDRESS; sz:INTEGER);
BEGIN
  IF (CurSz-Hfree) < sz THEN
    Allocate(heap,CurSz); IF heap=NIL THEN Fault(13); END;
    Hfree:=0;
  END;
  INTEGER(p):=INTEGER(heap)+Hfree; INC(Hfree,sz);
END Give;

PROCEDURE InitHeap;
  VAR i: INTEGER;
BEGIN Hfree:=0; SetAbort(FALSE);
  Allocate(heap,CurSz); IF heap=NIL THEN Fault(13); END;
END InitHeap;

END HeapSupport;
(* ------------- E N D   H E A P ---------------------- *)

CONST eol=0c;

TYPE PointB=POINTER TO Body;
     Body  =RECORD
              ID:Idname;   (* имя подпрограммы *)
              tp:Types;    (* тип функции; Undef, если Subr *)
              re:BOOLEAN;  (* TRUE , если ID реализована    *)
              PL:PointPS;  (* указатель на список параметров*)
              next:PointB; (* указатель на следующую подпр-у*)
            END;

VAR FORward:PointB;    (* указатель на весь список подпрограмм *)
    CurSubProg:PointB; (* указатель на создаваемый элемент списка FORWARD *)
    wasrealised:BOOLEAN;

VAR SP:PointB;

PROCEDURE SetSubProgList;
BEGIN SP:=FORward; END SetSubProgList;

PROCEDURE ExistSubProg( VAR I:Idname;
                        VAR T:Types;
                        VAR P:PointPS;
                        VAR r:BOOLEAN  ):BOOLEAN;
BEGIN
  IF SP=NIL THEN RETURN FALSE; END;
  I:=SP^.ID; T:=SP^.tp; P:=SP^.PL;
  r:=SP^.re; SP:=SP^.next;
  RETURN TRUE;
END ExistSubProg;

PROCEDURE SkipToEnd;
BEGIN
  SetWork(kw);
  REPEAT NewLine; GetSy; UNTIL sy=end;
END SkipToEnd;

VAR Params:ARRAY [0..99] OF INTEGER;  (* список idno параметров *)
    parno:INTEGER; (* число параметров *)
    F:INTEGER;     (* idno функции *)
    SUBR:BOOLEAN;   (* разбираемая подпрограмма есть Subr *)

PROCEDURE InParamSet(id:INTEGER):BOOLEAN;
  VAR i:INTEGER;
BEGIN
  FOR i:=0 TO parno-1 DO
    IF Params[i]=id THEN RETURN TRUE; END;
  END;
  RETURN FALSE;
END InParamSet;

PROCEDURE GetParamDcl;
BEGIN parno:=0;
  LOOP
    GetSy; IF sy#id THEN Error(25); RETURN; END;
    IF InParamSet(idno) THEN Error(38);
    ELSIF idno=F THEN Error(49);
    ELSE
      Params[parno]:=idno;
      SetMode(idno,Var);
      INC(parno);
    END;
    GetSy; IF sy=rpar THEN EXIT; END;
    IF sy#coma THEN Expected(coma); RETURN; END;
  END;
  GetSy; IF sy#EOL THEN Error(16); END;
END GetParamDcl;

PROCEDURE Dimension;
BEGIN SetWork(ass);
  LOOP
    GetSy; IF sy#id THEN Error(25); RETURN; END;
    IF InParamSet(idno) THEN SetMode(idno,Array);
    ELSE ErrorId(8); END;
    GetSy; IF sy=EOL THEN RETURN; END;
    IF sy#coma THEN Expected(coma); RETURN; END;
  END;
END Dimension;

PROCEDURE TypeDcl;
  VAR tp:Types;
BEGIN SetWork(ass);
  tp:=Types(ORD(sy));
  LOOP
    GetSy; IF sy#id THEN Error(25); RETURN; END;
    IF idno=F THEN ChangeType(tp);
      IF SUBR THEN Error(50); END;
    ELSIF InParamSet(idno) THEN SetType(idno,tp);
    ELSE  ErrorId(8); END;
    GetSy; IF sy=EOL THEN RETURN; END;
    IF sy#coma THEN Expected(coma); RETURN; END;
  END;
END TypeDcl;

PROCEDURE AfterDcl;
BEGIN
  LOOP NewLine;
    SetWork(dcl); GetSy;
    IF sy=dim THEN Dimension;
    ELSIF ORD(sy)<=ORD(char) THEN TypeDcl;
    ELSE BackUp; SetWork(kw); GetSy;
      IF sy=end THEN GetSy;
        IF sy#EOL THEN Error(16); END;
        RETURN;
      ELSE Error(48); SkipToEnd; RETURN;
      END;
    END;
  END;
END AfterDcl;

PROCEDURE Forward;
  VAR tp:Types; i,j: INTEGER; IdName:Idname;
BEGIN NewLine;
  SetWork(dcl); GetSy; tp:=Undef;
  IF ORD(sy)<=ORD(char) THEN
    tp:=Types(ORD(sy)); GetSy;
    IF sy=subr THEN Error(50); END;
  END;
  SUBR:=(sy=subr);
  IF (sy#func) AND NOT SUBR THEN
    Error(47); SkipToEnd; RETURN;
  END;
  SetWork(ass); GetSy;
  IF sy#id THEN Error(25); SkipToEnd; RETURN; END;
  IF tp=Undef THEN tp:=GetType(idno); END;
  IF SUBR THEN tp:=Undef; END;
  IdStr(idno,IdName); F:=idno;
  IF PutSubProg(IdName,tp,FALSE) THEN
    ErrorId(7); SkipToEnd; RETURN;
  END;
  GetSy;
  IF sy=EOL THEN parno:=0;
  ELSE
    IF sy#lpar THEN Expected(lpar); SkipToEnd; RETURN; END;
    GetParamDcl;
  END; AfterDcl;
  FOR i:=0 TO parno-1 DO j:=Params[i];
    PutParam(GetMode(j),GetType(j));
    DelId(j); SetMode(j,Empty);
  END;
  DelId(F); SetMode(F,Empty);
END Forward;

PROCEDURE PutSubProg(VAR Id:Idname;tp:Types;Re:BOOLEAN):BOOLEAN;
  VAR f,n:PointB; i:INTEGER; c:CHAR;
BEGIN wasrealised:=FALSE;
  IF FORward=NIL THEN Give(f,TSIZE(Body)); FORward:=f;
  ELSE f:=FORward;
    WHILE f#NIL DO
      IF f^.ID=Id THEN
        wasrealised:=f^.re;
        f^.re:=Re; RETURN TRUE;
      END;
      n:=f; f:=f^.next;
    END;
    Give(f,TSIZE(Body)); n^.next:=f;
  END;
  CurSubProg:=f; i:=-1;
  REPEAT INC(i); c:=Id[i]; f^.ID[i]:=c; UNTIL c=eol;
  f^.tp:=tp; f^.re:=Re; f^.PL:=NIL; f^.next:=NIL;
  RETURN FALSE;
END PutSubProg;

PROCEDURE WasRealised():BOOLEAN;
BEGIN RETURN wasrealised; END WasRealised;

PROCEDURE ChangeType(tp:Types);
BEGIN CurSubProg^.tp:=tp; END ChangeType;

VAR LastParam:PointPS;

PROCEDURE PutParam(m:Mode;t:Types);
  VAR l:PointPS;
BEGIN
  IF CurSubProg^.PL=NIL THEN
    Give(LastParam,TSIZE(ParListS));
    CurSubProg^.PL:=LastParam;
    LastParam^.m:=m; LastParam^.t:=t;
    LastParam^.next:=NIL; RETURN;
  END;
  Give(l,TSIZE(ParListS));
  LastParam^.next:=l; LastParam:=l;
  l^.m:=m; l^.t:=t; l^.next:=NIL;
END PutParam;

VAR List:PointPS; (* список спецификаций параметров *)

PROCEDURE SetParList(P:PointPS);
BEGIN List:=P; END SetParList;

PROCEDURE GetParam(VAR m:Mode;VAR t:Types):BOOLEAN;
BEGIN
  IF List=NIL THEN RETURN FALSE; END;
  m:=List^.m; t:=List^.t;
  List:=List^.next; RETURN TRUE;
END GetParam;

PROCEDURE InitForw;
BEGIN InitHeap; FORward:=NIL; END InitForw;

END fExt.
