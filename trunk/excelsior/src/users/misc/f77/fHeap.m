IMPLEMENTATION MODULE fHeap; (* ww 03-Feb-87 *)

FROM SYSTEM   IMPORT ADR, PROCESS, TRANSFER, ADDRESS;
FROM KRONOS   IMPORT DOT,
                     TRAP, MySelf;
FROM Universe IMPORT NewPrs;

CONST StartSpase =1024;    (* Начальный зазор между кучей и стеком *)
      AddHeapSize= 256;    (* Минимальный размер отхватываемого от *)
                           (* стека места.                         *)
      IlParm     = 101h;   (* Прирывание при неверных аргументах   *)

TYPE  Point= POINTER TO Area;
      Area = RECORD
               Next: Point;             (* ПОЛЯ НЕ ПЕРЕСТАНОВОЧНЫ!!!! *)
               Size: INTEGER;          (* Very mobil software        *)
             END;

VAR   Free,P,Old,Left,Right,T: Point;

PROCEDURE Get(VAR A:ADDRESS; Sz:INTEGER);
(* Выдает кусок из списка свободных *)
BEGIN
  A:=NIL;
  IF Sz<1 THEN TRAP(IlParm); END;
  P:=Free; Old:=ADR(Free);
  WHILE P#NIL DO
    IF P^.Size>(Sz+1) THEN
       DEC(P^.Size,Sz);
       A:=ADDRESS(INTEGER(P)+P^.Size);
       RETURN;
    ELSIF P^.Size=Sz THEN
      Old^.Next:=P^.Next; A:=P; RETURN;
    END;
    Old:=P; P:=P^.Next;
  END;
END Get;

PROCEDURE DeAllocate(VAR A:ADDRESS; Sz:INTEGER);
(* Ввязывает память в список свободных, *)
(* список упорядочен по адресам.        *)
BEGIN
  IF Sz<1 THEN TRAP(IlParm); END;
  Left:=ADR(Free); Right:=Free; T:=A; A:=NIL;
  IF Sz<2 THEN RETURN; END;
  WHILE (Right#NIL) & (Right<T) DO
    Left:=Right; Right:=Right^.Next;
  END;
  IF (Left#ADR(Free)) & ((Left^.Size+INTEGER(Left))=INTEGER(T)) THEN
    INC(Left^.Size,Sz);
    IF (Left^.Size+INTEGER(Left))=INTEGER(Right) THEN
      INC(Left^.Size,Right^.Size);
      Left^.Next:=Right^.Next;
    END;
    RETURN;
  END;
  IF (Right#NIL) & ((INTEGER(T)+Sz)=INTEGER(Right)) THEN
    T^:=Right^; INC(T^.Size,Sz);
    Left^.Next:=T;
    RETURN;
  END;
  T^.Size:=Sz; T^.Next:=Right; Left^.Next:=T;
END DeAllocate;

CONST wsplim = 40;

TYPE Desc  = RECORD
               G: INTEGER;  L: INTEGER;
               PC:INTEGER;  M: BITSET;
               S: INTEGER;  H: INTEGER;
               T: INTEGER;
             END;

VAR Spase,  HDec : INTEGER;
    Main         : POINTER TO Desc;
    Pm, Pl       : PROCESS;
    NoSize, Abo  : BOOLEAN;
    wsp  : ARRAY [0..wsplim] OF INTEGER;

PROCEDURE SetSpase(C:INTEGER);
BEGIN Spase:=C; END SetSpase;

PROCEDURE SetAbort(B:BOOLEAN);
BEGIN Abo:=B; END SetAbort;

PROCEDURE DecH();
(* Уменьшитель H регистра *)
BEGIN
  LOOP
    NoSize:=((Main^.H-Main^.S)<(Spase+HDec));
    IF NOT NoSize THEN
      DEC(Main^.H,HDec);
    END;
    TRANSFER(Pl,Pm);
  END;
END DecH;

PROCEDURE Allocate(VAR A:ADDRESS; Sz:INTEGER);
BEGIN
  Get(A,Sz);
  IF A=NIL THEN
    HDec:=Sz+AddHeapSize;
    TRANSFER(Pm,Pl);
    IF NoSize & Abo THEN TRAP(40h); END;
    IF NOT NoSize THEN
      A:=Main^.H+1; DeAllocate(A,HDec); Get(A,Sz);
    END;
  END;
END Allocate;

PROCEDURE HeapSize():INTEGER;
BEGIN
  HDec:=0;
  TRANSFER(Pm,Pl);
  RETURN (Main^.H-Main^.S-Spase-AddHeapSize);
END HeapSize;

BEGIN
  Free:=NIL;
  Abo:=TRUE;
  Spase:=StartSpase;
  NewPrs(DecH,ADR(wsp),SIZE(wsp),Pl);
  Main:=MySelf();
  Pm:=PROCESS(Main);
END fHeap.
