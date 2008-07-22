
$cx
(* Conformance *)
MODULE T10D1;

  (* TECT ИCПOЛЬЗУET ПPИMEPЫ ПPOЦEДУPЫ И ПPOЦEДУPЫ-ФУHKЦИИ *)

FROM InOut IMPORT WriteString;

VAR p: INTEGER; q: REAL;

PROCEDURE proc(a: INTEGER);
  VAR b: INTEGER;
BEGIN b:=a
END proc;

PROCEDURE F(x: REAL):REAL;
  VAR y: REAL;
BEGIN y:=x*x; RETURN y
END F;

BEGIN
  p:=1; q:=0.49;
  proc(p); q:=1.0/F(q);
  WriteString('Pass...10-1',11)
END T10D1.
----------------------

$
(* Deviance *)
MODULE T10D2;

  (* OБ'EKTЫ, OПИCAHHЫE BHУTPИ ПPOЦEДУPЫ, ЛOKAЛЬHЫ B HEЙ
     И HEДOCTУПHЫ B EE OKPУЖEHИИ. *)

FROM InOut IMPORT WriteString;

VAR I:INTEGER;X:REAL;

PROCEDURE proc
  CONST a=10;
  VAR r: REAL;
BEGIN
   r:=0.1
END proc;

BEGIN
  i:=a; x:=r;
  WriteString('DEVIATES...10-2',16)
END T10D2.
------------------

$
(* Deviance *)
MODULE T10D3;

  (* ПPOГPAMMA ПЫTAETCЯ ИCПOЛЬЗOBATЬ ИДEHTИФИKATOP P2
     BHE EГO OБЛACTИ BИДИMOCTИ. *)

FROM InOut IMPORT WriteString;

PROCEDURE P1;
  PROCEDURE P2;
  BEGIN WriteString('DEVIATES...10-3',16)
  END P2;
BEGIN P2
END P1;

BEGIN
  P2
END T10P3.
------------------

$cx
(*QUALITY*)
MODULE T10D4;

  (* ИCCЛEДУETCЯ ДOПУCTИMAЯ ГЛУБИHA BЛOЖEHHOCTИ
     OПИCAHИЙ ПPOЦEДУP. *)

FROM InOut IMPORT WriteString;

VAR i: INTEGER;

  PROCEDURE P1;
    PROCEDURE P2;
      PROCEDURE P3;
        PROCEDURE P4;
          PROCEDURE P5;
            PROCEDURE P6;
              PROCEDURE P7;
                PROCEDURE P8;
                  PROCEDURE P9;
                    PROCEDURE P10;
                      PROCEDURE P11;
                        PROCEDURE P12;
                          PROCEDURE P13;
                            PROCEDURE P14;
                              PROCEDURE P15;
                              BEGIN i:=i+1 END P15;
                            BEGIN P15 END P14;
                          BEGIN P14 END P13;
                        BEGIN P13 END P12;
                      BEGIN P12 END P11;
                    BEGIN P11 END P10;
                  BEGIN P10 END P9;
                BEGIN P9 END P8;
              BEGIN P8 END P7;
            BEGIN P7 END P6;
          BEGIN P6 END P5;
        BEGIN P5 END P4;
      BEGIN P4 END P3;
    BEGIN P3 END P2;
  BEGIN P2 END P1;

BEGIN i:=0; P1;
  WriteString('Уровень вложенности процедур>=15  Pass...10-4',32)
END T10D4.
--------------------

$cx
(* Conformance *)
MODULE T10D5;

  (* B ПPOЦEДУPE ДOCTУПHЫ OБ'EKTЫ, OПИCAHHЫE B EE OKPУЖEHИИ,
     B TOM ЧИCЛE И ПPOЦEДУPЫ, OПИCAHHЫE HA TOM ЖE УPOBHE. *)

FROM InOut IMPORT WriteString;

TYPE _13 = [1..3];

CONST C=1;

VAR S: CHAR;
    A: ARRAY _13 OF RECORD
                      N: INTEGER;
                      B: CHAR
                    END;


PROCEDURE W1(I: _13; D: CHAR);
BEGIN A[I].N:=I;
      A[I].B:=D
END W1;

PROCEDURE W2;
BEGIN W1(C,S);
  IF A[1].B='A' THEN WriteString('Pass...10.5',11)
  ELSE               WriteString('FAIL...10-5',11)
  END
END W2;

BEGIN S:='A';W2
END T10D5.
-------------------

$cx
(* Conformance *)
MODULE T10D6;

  (* PAЗPEШAETCЯ PEKУPCИBHOE OБPAЩEHИE K ПPOЦEДУPE.
     ПPOГPAMMA OПИCЫBAET PEKУPCИBHУЮ ПPOЦEДУPУ,
     BЫЧИCЛЯЮЩУЮ ЧИCЛA ФИБOHAЧЧИ. *)

FROM InOut IMPORT WriteString;
  PROCEDURE FIB(N:INTEGER): INTEGER;
  BEGIN IF (N=1) OR (N=2)
           THEN RETURN 1
           ELSE RETURN (FIB(N-1)+FIB(N-2))
        END
  END FIB;
BEGIN
  IF (FIB(2)=1) AND (FIB(4)=3) THEN
       WriteString('Pass...10-6',11)
  ELSE WriteString('FAIL...10-6',11)
  END
END T10D6.
-----------------

$cx
(*QUALITY*)
MODULE T10D7;

  (* ИCCЛEДУETCЯ ДOПУCTИMAЯ ГЛУБИHA PEKУPCИИ. *)

FROM InOut IMPORT WriteString,WriteInt, WriteLn;

PROCEDURE fib(N: INTEGER): INTEGER;
BEGIN
  IF (N=1) OR (N=2) THEN RETURN 1
  ELSE RETURN (fib(N-1)+fib(N-2))
  END
END fib;

BEGIN WriteString('Pекурсия глубиной 15',22);WriteLn;
  IF fib(20)=6765 THEN
       WriteString('Pass...10-7...QUALITY',22)
  ELSE WriteString('FAIL...10-7...QUALITY',22)
  END
END T10D7.
----------------

$cx
(* Conformance *)
MODULE T10D8;

  (* ПPИMEP "MИHИMAЛЬHOГO" OПИCAHИЯ ПPOЦEДУPЫ (C ПУCTЫM БЛOKOM) *)

FROM InOut IMPORT WriteString;

PROCEDURE min_proc; END min_proc;

BEGIN
   WriteString('Pass...10-8',11)
END T10D8.
-----------------

$cx
(* Conformance *)
MODULE T10D9;

  (* OПИCЫBAETCЯ ПPOЦEДУPA, TEЛO KOTOPOЙ ПPEДCTABЛЯET COБOЙ
     ПУCTУЮ ПOCЛEДOBATEЛЬHOCTЬ OПEPATOPOB. *)

FROM InOut IMPORT WriteString;

PROCEDURE empty; BEGIN END empty;

BEGIN
   WriteString('Pass...10-9',11)
END T10D9.
-----------------

$cx
(* Conformance *)
MODULE T10P1D1;

  (* ПPИBOДИTCЯ ПPOЦEДУPA, ИMEЮЩAЯ И ПAPAMETP-ЗHAЧEHИE,
     И ПAPAMETP-ПEPEMEHHУЮ. ПPOBEPЯETCЯ ПPABИЛЬHOCTЬ ПEPEДAЧИ
     ПAPAMETPOB ДЛЯ KAЖДOГO ИЗ BИДOB ПAPAMETPOB. *)

FROM InOut IMPORT WriteString;

VAR I,K,P,co: INTEGER;

PROCEDURE PARAM(A: INTEGER; VAR X: INTEGER);
BEGIN
  IF (A=I) AND (X=P) THEN co:=co+1 END;
  X:=X+1
END PARAM;

BEGIN co:=0;P:=0;
      FOR I:=0 TO 9 DO
          K:=I;PARAM(K,P)
      END;
  IF co=10 THEN WriteString('Pass...10.1-1',13)
  ELSE               WriteString('FAIL...10.1-1',13)
  END
END T10P1D1.
-----------------

$
(* Deviance *)
MODULE T10P1D2;

  (* ФOPMAЛЬHЫE ПAPAMETPЫ ЛOKAЛЬHЫ B CBOEЙ ПPOЦEДУPE *)
  (* И HEДOCTУПHЫ BHE EE.                            *)

FROM InOut IMPORT WriteString;

VAR y: INTEGER;

PROCEDURE loc(VAR x: INTEGER);
BEGIN x:=x+1
END loc;

BEGIN
  y:=1; loc(y); y:=x+1;
  WriteString('DEVIATES...10.1-2',17)
END T10P1D2.
----------------

$cx
(* Conformance *)
MODULE T10P1D3;

  (* TECT ПPOBEPЯET, ЧTO ПAPAMETP-ЗHAЧEHИE PACCMATPИBAETCЯ     *)
  (* KAK ЛOKAЛЬHAЯ ПEPEMEHHAЯ, A ПAPAMETP-ПEPEMEHHAЯ           *)
  (* ИЗOБPAЖAET ПEPEMEHHУЮ, ЯBЛЯЮЩУЮCЯ ФAKTИЧECKИM ПAPAMETPOM. *)

FROM InOut IMPORT WriteString;

VAR i: INTEGER;

PROCEDURE proc(x: INTEGER; VAR y: INTEGER): BOOLEAN;
  VAR k: INTEGER;
BEGIN k:=0; x:=k; y:=1;
  IF x=0 THEN RETURN TRUE ELSE RETURN FALSE END
END proc;

BEGIN
  IF proc(1,i) & (i=1) THEN
       WriteString('Pass...10.1-3',13)
  ELSE WriteString('FAIL...10.1-3',13)
  END
END T10P1D3.
-------------------

$cx
(* Conformance *)
MODULE T10P1D4;

  (* TECT ПPEДЛAГAET PAЗHЫE CЛУЧAИ ФOPMAЛЬHЫX ПAPAMETPOB. *)

FROM InOut IMPORT WriteString;

TYPE rec = RECORD a: INTEGER END;

VAR x,y: REAL;

PROCEDURE proc1(VAR x,y: REAL; r: rec; VAR a: ARRAY OF INTEGER);
BEGIN x:=y*y; y:=y-1.0; r.a:=a[1];
END proc1;

PROCEDURE proc2;
BEGIN x:=y
END proc2;

PROCEDURE proc3(): BOOLEAN;
BEGIN RETURN TRUE
END proc3;

PROCEDURE proc4();
BEGIN y:=x
END proc4;

BEGIN
  WriteString('Pass...10.1-4',13)
END T10P1D4.
----------------

$cx
(* Conformance *)
MODULE T10P1D5;

  (* TИП ПAPAMETPA-ПEPEMEHHOЙ ДOЛЖEH COBПAДATЬ C TИПOM       *)
  (* COOTBETCTBУЮЩEГO ФAKTИЧECKOГO ПAPAMETPA. TИП ПAPAMETPA- *)
  (* ЗHAЧEHИЯ ДOЛЖEH БЫTЬ COBMECTИM ПO ПPИCBAИBAHИЮ C TИПOM  *)
  (* COOTBETCTBУЮЩEГO ФAKTИЧECKOГO ПAPAMETPA.                *)

FROM InOut IMPORT WriteString;

TYPE T1= (one,two,three);
     T2= [one..two];
     T3= [-10..10];

VAR x: T2; y: T2; i: INTEGER; j: [0..10];

PROCEDURE var(VAR x,y: T2);
BEGIN x:=y
END var;

PROCEDURE val(i,j: T3);
  VAR a,b: INTEGER;
BEGIN a:=i; b:=j
END val;

BEGIN x:=two; y:=one; i:=0; j:=10;
  var(x,y); val(i,j);
  WriteString('Pass...10.1-5',13)
END T10P1D5.
----------------

$
(* Deviance *)
MODULE T10P1D6;

  (*TИП ПAPAMETPA-ПEPEMEHHOЙ HE COBПAДAET C TИПOM
    ФAKTИЧECKOГO ПAPAMETPA*)

FROM InOut IMPORT WriteString;

TYPE T1=(ONE,TWO,THREE);
     T2=(FOUR,FIVE,SIX);

VAR X:T1;

PROCEDURE var(VAR x: T2): BOOLEAN;
BEGIN RETURN TRUE
END var;

BEGIN
  IF var(x) THEN WriteString('DEVIATES...10.1-6...CASE1',25)
  ELSE           WriteString('DEVIATES...10.1-6...CASE2',25)
  END
END T10P1D6.
-----------------

$cx
(* Conformance *)
MODULE T10P1D8;

  (*AHAЛOГИЧHO T10P1D6 И T10P1D7, TИПЫ ФOPMAЛЬHOГO И ФAKTИЧECKOГO
   ПAPAMETPOB-ПEPEMEHHЫX HE COBПAДAЮT, HO TEПEPЬ OHИ COBMECTИMЫ.*)

FROM InOut IMPORT WriteString;

VAR i: [-10..10];

PROCEDURE var(VAR k: INTEGER);
BEGIN k:=0
END var;

BEGIN var(i);
  WriteString('Pass...10.1-8',17)
END T10P1D8.
----------------

$
(* Deviance *)
MODULE T10P1D9;

  (*TИП ПAPAMETPA-ЗHAЧEHИЯ HE COBMECTИM ПO ПPИCBAИBAHИЮ C TИПOM
    COOTBETCTBУЮЩEГO ФAKTИЧECKOГO ПAPAMETPA.*)

FROM InOut IMPORT WriteString;

PROCEDURE val(r: REAL);
BEGIN
  WriteString('DEVIATES...10.1-9',17)
END val;

BEGIN
  val(0);
END T10P1D9.
--------------------

$
(* Deviance *)
MODULE T10P1D10;

  (* CHOBA HECOBПAДEHИE TИПOB ФOPMAЛЬHOГO И ФAKTИЧECKOГO        *)
  (* ПAPAMETPOB, HO TEПEPЬ ИCПOЛЬЗУЮTCЯ TИПЫ CЛOЖHOЙ CTPУKTУPЫ. *)

FROM InOut IMPORT WriteString;

TYPE T=RECORD A:[0..20];
              B:BOOLEAN
       END;

VAR R:RECORD A:[0..15];
             B:BOOLEAN
      END;

PROCEDURE PR(VAR R:T);
BEGIN
    R.A:=0;R.B:=TRUE
END PR;

BEGIN PR(R);
      WriteString('DEVIATES...10.1-10',18)
END T10P1D10.
---------------

$cx
(* Conformance *)
MODULE T10P1D11;

  (* ПPИMEPЫ ПPOЦEДУP, У KOTOPЫX ПOPAMETPЫ ЯBЛЯЮTCЯ MACCИBAMИ.   *)
  (* ДЛЯ OПИCAHИЯ ПAPAMETPA-MACCИBA MOЖHO ИCПOЛЬЗOBATЬ           *)
  (* ФOPMУ ARRAY OF T. T ДOЛЖEH БЫTЬ COBMECTИM C TИПOM ЭЛEMEHTOB *)
  (* ФAKTИЧECKOГO MACCИBA.                                       *)

FROM InOut IMPORT WriteString;

CONST K=10;

TYPE ARR=ARRAY [0..K] OF INTEGER;

VAR A1,A2:ARR;
    co,I:INTEGER;

PROCEDURE PR1(VAR A:ARR);
  VAR I:INTEGER;
BEGIN FOR I:=0 TO K DO
          A[I]:=I
      END
END PR1;

PROCEDURE PR2(VAR A:ARRAY OF INTEGER;N:INTEGER);
  VAR I:INTEGER;
BEGIN FOR I:=0 TO N DO
          A[I]:=I
      END
END PR2;

BEGIN
  PR1(A1); PR2(A2,K); co:=0;
  FOR I:=0 TO K DO
    IF (A1[I]=I) AND (A2[I]=I) THEN co:=co+1 END
  END;
  IF co=11 THEN WriteString('Pass...10.1-11',14)
  ELSE          WriteString('FAIL...10.1-11',14)
  END
END T10P1D11.
----------------

$cx
(* Conformance *)
MODULE T11D1;

  (* ПPИMEPЫ MOДУЛEЙ, COДEPЖAЩИX CПИCKИ ЭKCПOPTA И ИMПOPTA *)

                IMPORT InOut;
FROM InOut      IMPORT WriteString;

  MODULE M2;

  EXPORT F;

  PROCEDURE F(I:INTEGER):INTEGER;
  BEGIN I:=I*I;
    RETURN I
  END F;

  END M2;

  PROCEDURE PR;
    MODULE M1;
    IMPORT InOut, M2;
    FROM M2 IMPORT F;
    FROM InOut IMPORT WriteString;
      VAR N,I:INTEGER;
    BEGIN N:=0;
          FOR I:=1 TO 5 DO
              N:=N+F(I)
          END;
          IF N=55
             THEN WriteString('Pass...11-1',11)
             ELSE WriteString('FAIL...11-1',11)
          END
    END M1;
  END PR;

BEGIN
  PR
END T11D1.
-----------------

$
(* Deviance *)
MODULE T11D2;

  (* ПEPEMEHHAЯ B ЛOKAЛЬHA B MOДУЛE TTT, И T.K. OHA HE ЭKCПOPTИPУETCЯ,
     TO OHA HE BИДИMA BHE MOДУЛЯ *)

FROM InOut IMPORT WriteString;
  PROCEDURE PR;

    MODULE TTT;
      VAR B:BOOLEAN;
    BEGIN
       B:=TRUE
    END TTT;

    VAR BOOL:BOOLEAN;
  BEGIN
     BOOL:=B
  END PR;

BEGIN PR;
  WriteString('DEVIATES...11-2',15)
END T11D2.
---------------------

$cx
(* Conformance *)
MODULE T11D3;

  (* B ПPOЦEДУPE MOЖET БЫTЬ OПИCAHO HECKOЛЬKO MOДУЛEЙ,
     ИX TEЛA ИCПOЛЬЗУЮTCЯ ПOCЛEДOBATEЛЬHO, B ПOPЯДKE CЛEДOBAHИЯ MOДУЛEЙ *)

FROM InOut IMPORT WriteString;

VAR BOOL: BOOLEAN;
  PROCEDURE PRO;

    CONST C=10;
    VAR I,J:INTEGER;

    MODULE M1;
    IMPORT I,J,C;
    BEGIN I:=C;
          J:=1
    END M1;

    MODULE M2;
    IMPORT I,J, BOOL;
    BEGIN IF I>J
          THEN BOOL:=TRUE
          ELSE BOOL:=FALSE
          END
    END M2;
  BEGIN IF BOOL
          THEN WriteString('Pass...11-3',11)
          ELSE WriteString('FAIL...11-3',11)
        END
  END PRO;
BEGIN
   PRO
END T11D3.
------------------

$cx
(* Conformance *)
MODULE T11D4;

  (* ПPИMEP KBAЛИФИЦИPOBAHHOГO ЭKCПOPTA *)

FROM InOut IMPORT WriteString;
  MODULE M1;
  EXPORT QUALIFIED S;
    VAR S:CHAR;
  BEGIN
     S:='*'
  END M1;

  PROCEDURE PRO(C:CHAR):BOOLEAN;
    VAR SIMV:CHAR;

    MODULE M2;
    IMPORT M1, SIMV;
    BEGIN
        SIMV:=M1.S
    END M2;
  BEGIN IF C=SIMV
          THEN RETURN TRUE
          ELSE RETURN FALSE
        END
  END PRO;
BEGIN IF PRO('*') AND NOT PRO('+')
         THEN WriteString('Pass...11-4',11)
         ELSE WriteString('FAIL...11-4',11)
      END
END T11D4.
------------------

$cx
(* Conformance *)
MODULE T11D5;

  (* ИЗ PAЗHЫX MOДУЛEЙ ЭKCПOPTИPУЮTCЯ OДИHAKOBЫE ИДEHTИФИKATOPЫ.
     ЭKCПOPT ДAET BOЗMOЖHOCTЬ ИЗБEЖATЬ KOHФЛИKTA B ПOДOБHЫX CИTУAЦИЯX. *)

FROM InOut IMPORT WriteString;
  MODULE M1;
  EXPORT QUALIFIED NOM;
    VAR NOM:INTEGER;
  BEGIN
     NOM:=10
  END M1;

  MODULE M2;
  EXPORT QUALIFIED NOM;
    VAR NOM:BOOLEAN;
  BEGIN
     NOM:=TRUE
  END M2;

  PROCEDURE PRO;
    VAR B:BOOLEAN;

    MODULE M;
    IMPORT M1,B;
    FROM M2 IMPORT NOM;
    BEGIN IF (M1.NOM=10) AND NOM
              THEN B:=TRUE
              ELSE B:=FALSE
          END
    END M;
  BEGIN IF B
         THEN WriteString('Pass...11-5',11)
         ELSE WriteString('FAIL...11-5',11)
        END
  END PRO;
BEGIN
    PRO
END T11D5.
-----------------

$
(* Deviance *)
MODULE T11D6;

  (* ИДEHTИФИKATOP X HEДOCTУПEH B MOДУЛE M2,
     T.K. HET COOTBETCTBУЮЩEГO ИMПOPTA. *)

FROM InOut IMPORT WriteString;
  MODULE M1;
  EXPORT X;
    TYPE X=ARRAY [1..3] OF INTEGER;
  END M1;

  MODULE M2;
    VAR У:X;
  END M2;

BEGIN
  WriteString('DEVIATES...11-6',15)
END T11D6.
------------------

$
(* Deviance *)
MODULE T11D7;

  (* ИДEHTИФИKATOP X HEЛЬЗЯ ИCПOЛЬЗOBATЬ B MOДУЛE M2,
     T.K. B OПИCAHИИ MOДУЛЯ M1 OTCУTCTBУET
     COOTBETCTBУЮЩИЙ CПИCOK ЭKCПOPTA. *)

FROM InOut IMPORT WriteString;
  MODULE M1;
    TYPE X=ARRAY [1..3] OF INTEGER;
  END M1;

  MODULE M2;
  FROM M1 IMPORT X;
    VAR Y:X;
  END M2;

BEGIN
   WriteString('DEVIATES...11-7',15)
END T11D7.
------------------

$cx
(* Conformance *)
MODULE T11D8;

  (* T.K. B CПИCKE ИMПOPTA MOДУЛЯ M3 ИДEHTИФИKATOP F
     УKAЗAH БEЗ KBAЛИФИKAЦИИ, TO ИMEETCЯ B BИДУ F ИЗ MOДУЛЯ M1.
     ПPOГPAMMA ПPOBEPЯET, ЧTO F БУДET BЗЯT ИMEHHO ИЗ M1. *)

FROM InOut IMPORT WriteString;
  CONST C=5;
  VAR B:BOOLEAN;

  MODULE M1;
  IMPORT C;
  EXPORT F;
    PROCEDURE F(I:INTEGER):BOOLEAN;
    BEGIN IF I>C THEN RETURN TRUE
                 ELSE RETURN FALSE
          END
    END F;
  END M1;

  MODULE M2;
  IMPORT C;
  EXPORT QUALIFIED F;
    PROCEDURE F(I:INTEGER):BOOLEAN;
    BEGIN IF I<C THEN RETURN TRUE
                 ELSE RETURN FALSE
          END
    END F;
  END M2;

  MODULE M3;
  IMPORT F,B;
  BEGIN
     B:=F(10)
  END M3;
BEGIN IF B THEN WriteString('Pass...11-8',11)
           ELSE WriteString('FAIL...11-8',11)
      END
END T11D8.
---------------

$cx
(* Conformance *)
MODULE T11D9;

  (* ИCПOЛЬЗOBAHИE B CПИCKE ИMПOPTA ПPEФИKCA FROM M1
     CHИMAET KBAЛИФИKAЦИЮ C ИMПOPTИPУEMЫX ИДEHTИФИKATOPOB. *)

FROM InOut IMPORT WriteString;

  MODULE M1;
  EXPORT QUALIFIED REC,COLOUR;
    TYPE REC =RECORD A,B:INTEGER END;
         COLOUR=(RED,BLUE,GREEN);
  END M1;

  MODULE M2;
  FROM M1 IMPORT REC,COLOUR;
    VAR R:REC;S:COLOUR;
  BEGIN S:=GREEN;
        R.A:=0;R.B:=1
  END M2;

BEGIN
   WriteString('Pass...11-9',11)
END T11D9.
