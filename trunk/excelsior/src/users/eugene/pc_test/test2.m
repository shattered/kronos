-----------------

$cx
(*QUALITY*)
MODULE T7D1;

  (* TECT ПPOBEPЯET, PAЗPEШEHЫ ЛИ OЧEHЬ ДЛИHHЫE
     ГPУППOBЫE OПИCAHИЯ. B ДAHHOЙ ПPOГPAMME OДHИM
     OПИCAHИEM OБ'ЯBЛЯETCЯ 100 ПEPEMEHHЫX.        *)

FROM InOut IMPORT WriteString;

  VAR I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15,I16,
      I17,I18,I19,I20,I21,I22,I23,I24,I25,I26,I27,I28,I29,
      I30,I31,I32,I33,I34,I35,I36,I37,I38,I39,I40,I41,I42,
      I43,I44,I45,I46,I47,I48,I49,I50,I51,I52,I53,I54,I55,
      I56,I57,I58,I59,I60,I61,I62,I63,I64,I65,I66,I67,I68,
      I69,I70,I71,I72,I73,I74,I75,I76,I77,I78,I79,I80,I81,
      I82,I83,I84,I85,I86,I87,I88,I89,I90,I91,I92,I93,I94,
      I95,I96,I97,I98,I99 : INTEGER;
BEGIN I0:=0;I1:=1;I2:=2;I3:=3;I4:=4;I5:=5;I6:=6;I7:=7;I8:=8;I9:=9;
      I10:=I0+1;I11:=I1+1;I12:=I2+1;I13:=I3+1;I14:=I4+1;
      I15:=I5+1;I16:=I6+1;I17:=I7+1;I18:=I8+1;I19:=I9+1;
      I20:=I10+I0 ;I21:=I11+I1 ;I22:=I12+I2 ;I23:=I13+I3 ;I24:=I14+I4 ;
      I25:=I15+I5 ;I26:=I16+I6 ;I27:=I17+I7 ;I28:=I18+I8 ;I29:=I19+I9 ;
      I30:=I20+I10;I31:=I21+I11;I32:=I22+I12;I33:=I23+I13;I34:=I24+I14;
      I35:=I25+I15;I36:=I26+I16;I37:=I27+I17;I38:=I28+I18;I39:=I29+I19;
      I40:=I30+I20;I41:=I31+I21;I42:=I32+I22;I43:=I33+I23;I44:=I34+I24;
      I45:=I35+I25;I46:=I36+I26;I47:=I37+I27;I48:=I38+I28;I49:=I39+I29;
      I50:=I40+I30;I51:=I41+I31;I52:=I42+I32;I53:=I43+I33;I54:=I44+I34;
      I55:=I45+I35;I56:=I46+I36;I57:=I47+I37;I58:=I48+I38;I59:=I49+I39;
      I60:=I50+I40;I61:=I51+I41;I62:=I52+I42;I63:=I53+I43;I64:=I54+I44;
      I65:=I55+I45;I66:=I56+I46;I67:=I57+I47;I68:=I58+I48;I69:=I59+I49;
      I70:=I60+I50;I71:=I61+I51;I72:=I62+I52;I73:=I63+I53;I74:=I64+I54;
      I75:=I65+I55;I76:=I66+I56;I77:=I67+I57;I78:=I68+I58;I79:=I69+I59;
      I80:=I70+I60;I81:=I71+I61;I82:=I72+I62;I83:=I73+I63;I84:=I74+I64;
      I85:=I75+I65;I86:=I76+I66;I87:=I77+I67;I88:=I78+I68;I89:=I79+I69;
      I90:=I80+I70;I91:=I81+I71;I92:=I82+I72;I93:=I83+I73;I94:=I84+I74;
      I95:=I85+I75;I96:=I86+I76;I97:=I87+I77;I98:=I88+I78;I99:=I89+I79;
      I0:=I90+I91+I92+I93+I94+I95+I96+I97+I98+I99;
      IF (I0=2815)
      THEN WriteString('ГPУППOBOE OПИCAHИE 100 ПEPEMEHHЫX PAЗPEШEHO',45);
           WriteString(' ... 7-1 ... QUALITY',20)
      END
END T7D1.

----------------

$cx
(*CONFORMANCE*)
MODULE T8P1D2;

   (* ПPИMEPЫ ИЗOБPAЖEHИЙ KOMПOHEHT MACCИBA. *)

FROM InOut IMPORT WriteString;
  TYPE OTT = (ONE, TWO, THREE);
  CONST K=2;
  VAR A1:ARRAY [1..10] OF REAL;
      A2:ARRAY OTT OF BOOLEAN;
      A3:ARRAY BOOLEAN OF INTEGER;
      I:INTEGER; X:OTT; B:BOOLEAN;

BEGIN
  I:=1;X:=ONE; B:=TRUE;
  A1[I]:=0.01; A1[2*I+K]:=0.02;
  A2[ONE]:=TRUE; INC(X);A2[X]:=FALSE;
  A3[TRUE]:=1; A3[NOT B]:=0;
  IF (A1[4]=0.02) AND (A2[TWO]=FALSE) AND (A3[FALSE]=0)
     THEN WriteString('PASS...8.1-2',12)
     ELSE WriteString('FAIL...8.1-2',12)
  END
END T8P1D2.
-----------------

$cx
(*CONFORMANCE*)
MODULE T8P1D3;

  (* ИЗOБPAЖEHИE BИДA A[E1,...EN] ЯBЛЯETCЯ COKPAЩEHИEM
     ДЛЯ A[E1][E2]...[EN]. TECT ПPOBEPЯET ЭKBИBAЛEHTHOCTЬ
     ЭTИX ДBУX CПOCOБOB ИЗOБPAЖEHИЯ. *)

FROM InOut IMPORT WriteString;
  VAR A:ARRAY [2..5],[2..5] OF INTEGER;
      B:ARRAY [2..5] OF ARRAY [2..5] OF INTEGER;
      co,I,J:INTEGER;

BEGIN co:=0;
      FOR I:=2 TO 5 DO
          FOR J:=2 TO 5 DO
              A[I,J]:=J; B[I][J]:=J;
          END
      END;
      FOR I:=2 TO 5 DO
          FOR J:=2 TO 5 DO
              IF A[I][J]#A[I,J] THEN co:=co+1 END;
              IF B[I][J]#B[I,J] THEN co:=co+1 END
          END
      END;
      IF co=0
         THEN WriteString('PASS...8.1-3',12)
         ELSE WriteString('FAIL...8.1-3',12)
      END
END T8P1D3.
-------------------

$
(*DEVIANCE*)
MODULE T8P1D4;

  (* TИП ИHДEKCA MACCИBA HE COBMECTИM C TИПOM X. *)

FROM InOut IMPORT WriteString;

  TYPE T1=(ONE,TWO,THREE);
       T2=(FOUR,FIVE,SIX);
  VAR A:ARRAY [ONE..TWO] OF INTEGER;
      X:[FOUR..SIX];

BEGIN X:=FIVE;
      A[X]:=5;
      WriteString('DEVIATES...8.1-4',16)
END T8P1D4.
----------------

$
(*DEVIANCE*)
MODULE T8P1D5;

  (* ПPИ ИCПOЛHEHИИ ПPOГPAMMЫ BOЗБУЖДAETCЯ ИCKЛЮЧИTEЛЬHAЯ
     CИTУAЦИЯ: BЫXOД ЗA ГPAHИЦЫ MACCИBA.                  *)

FROM InOut IMPORT WriteString;
  VAR A:ARRAY [2..5] OF INTEGER;
BEGIN A[1]:=1;
      WriteString('ERROR IS NOT DETECTED...8.1-5',30)
END T8P1D5.
-------------------

$c
(*ERROR HANDLING*)
MODULE T8P1D5;

  (* ПPИ ИCПOЛHEHИИ ПPOГPAMMЫ BOЗБУЖДAETCЯ ИCKЛЮЧИTEЛЬHAЯ
     CИTУAЦИЯ: BЫXOД ЗA ГPAHИЦЫ MACCИBA.                  *)

FROM InOut IMPORT WriteString;
  VAR A:ARRAY [2..5] OF INTEGER;
      i: INTEGER;
BEGIN i:=1;
      A[i]:=1;
      WriteString('ERROR IS NOT DETECTED...8.1-5',30)
END T8P1D5.
-------------------

$
(*DEVIANCE*)
MODULE T8P1D6;

  (* AHAЛOГИЧHO T8P1D5, HO B БOЛEE ЗABУAЛИPOBAHHOM BИДE. *)

FROM InOut IMPORT WriteString;
  CONST I=2;K=5;
  VAR A:ARRAY [1..4],[2..5] OF INTEGER;
BEGIN A[I*3,K-4]:=1;
      WriteString('ERROR IS NOT DETECTED...8.1-6',30)
END T8P1D6.
------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D2;

  (* ПPOГPAMMA ПPOBEPЯET ПPИOPИTETHOCTЬ ЛOГИЧECKOЙ
     OПEPAЦИИ NOT ПO OTHOШEHИЮ K OПEPAЦИЯM AND И OR. *)

FROM InOut IMPORT WriteString;
VAR A,B,X,X1,Y,Y1:BOOLEAN;
    K:INTEGER;
BEGIN K:=0;
      FOR A:=FALSE TO TRUE DO
          FOR B:=FALSE TO TRUE DO
              X:=B AND (NOT A);X1:=NOT A AND B;
              Y:=B OR (NOT A); Y1:=NOT A OR B;
              IF (X1=X) AND (Y1=Y) THEN K:=K+1 END
          END
      END;
      IF K=4
         THEN WriteString('PASS...8.2-2',12)
         ELSE WriteString('FAIL...8.2-2',12)
      END
END T8P2D2.
--------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D3;

  (* TECT ПPOBEPЯET ДЛЯ ЛOГИЧECKИX OПEPAЦИЙ, ЧTO
     ПOCЛEДOBATEЛЬHOCTЬ OПEPAЦИЙ OДHOГO ПPИOPИTETA
     ИCПOЛHЯETCЯ CЛEBA HAПPABO. *)

FROM InOut IMPORT WriteString;
  VAR A,B,C,X,Y:BOOLEAN;
      K:INTEGER;
BEGIN K:=0;
      FOR A:=FALSE TO TRUE DO
          FOR B:=FALSE TO TRUE DO
              FOR C:=FALSE TO TRUE DO
                  X:=A OR B AND C;
                  Y:=A OR (B AND C);
                  IF X=Y THEN K:=K+1 END
              END
          END
      END;
      IF K=8
         THEN WriteString('PASS...8.2-3',12)
         ELSE WriteString('FAIL...8.2-3',12)
      END
END T8P2D3.
----------------

$
(*DEVIANCE*)
MODULE T8P2D5;

  (* ИCПOЛЬЗУEMAЯ ПOCЛEДOBATEЛЬHOCTЬ OПEPAЦИЙ ЯBЛЯETCЯ ЗAПPEЩEHHOЙ
     C TOЧKИ ЗPEHИЯ CИHTAKCИCA BЫPAЖEHИЙ. *)

FROM InOut IMPORT WriteString;
  VAR B:BOOLEAN;
BEGIN B:=NOT 1<2;
      IF B THEN WriteString('FAIL...8.2-5',12)
      ELSE WriteString('DEVIATES...8.2-5',12)
      END
END T8P2D5.
----------------

$cx
(*CONFORMANCE*)
MODULE T8P2D6;

(* ПPOГPAMMA ПPOBEPЯET, ЧTO BЫПOЛHЯЮTCЯ ПPABИЛA ПPИOPИTETA
   ДЛЯ OПEPAЦИЙ HAД MHOЖECTBAMИ. *)

FROM InOut IMPORT WriteString;

CONST A={1,2};
      B={3,4};
      C={5,6};

VAR K: INTEGER;

BEGIN
  K:=0;
  IF (A+B*C)#(A+(B*C)) THEN K:=K+1 END;
  IF (A-B/C)#(A-(B/C)) THEN K:=K+1 END;
  IF K=0 THEN WriteString('PASS...8.2-6',12)
  ELSE        WriteString('FAIL...8.2-6',12)
  END
END T8P2D6.
------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D7;

  (* ЗAПPEЩEHHAЯ ПOCЛEДOBATEЛЬHOCTЬ OПEPAЦИЙ. *)

FROM InOut IMPORT WriteString;
  CONST A=2;
        B=1;
        C=TRUE;
  VAR D,E:BOOLEAN;
BEGIN D:=A<B<C; E:=A#B=C;
      IF D AND E
         THEN WriteString('PASS...8.2-7',12)
         ELSE WriteString('FAIL...8.2-7',12)
     END
END T8P2D7.
-----------------

$
(*DEVIANCE*)
MODULE T8P2D8;

  (* ЗAПPEЩEHHAЯ ПOCЛEДOBATEЛЬHOCTЬ OПEPAЦИЙ. *)

FROM InOut IMPORT WriteString;
  CONST A=1;
        B=2;
        C=TRUE;
  VAR D:BOOLEAN;
BEGIN D:=C>A>B;
      WriteString('DEVIATES...8.2-8',16)
END T8P2D8.
-----------------

$cx
(*CONFORMANCE*)
MODULE T6P1D1;

  (* ПPИMEPЫ APИФMETИЧECKИX OПEPAЦИЙ HAД ЦEЛЫMИ И
     HATУPAЛЬHЫMИ OПEPAHДAMИ. *)

FROM InOut IMPORT WriteString;
  VAR X1,X2,X3:INTEGER;
      Y1,Y2,Y3:INTEGER;
      V1,V2:[0..10];
      W1,W2:[-5..0];
BEGIN X1:=4;X2:=2;X3:=X1*X2;
      Y1:=-1;Y2:=-2;Y3:=Y1+Y2;
      V1:=0;V2:=5;X3:=V2 DIV 2-1;
      W1:=-3;W2:=-2;Y3:=W1 MOD W2;
      WriteString('PASS...6.1-1',12)
END T6P1D1.
-------------------------

$cx
(*CONFORMANCE*)
MODULE T6P1D2;

  (* ПPИMEPЫ APИФMETИЧECKИX OПEPAЦИЙ HAД BEЩECTBEHHЫMИ
     ЧИCЛAMИ. *)

FROM InOut IMPORT WriteString;
  VAR X,Y,Z:REAL;
BEGIN X:=2.1;Y:=10.2;
      Z:=-X;Z:=+X;
      Z:=X+Y;Z:=X-Y;
      Z:=X*Y;Z:=X/Y;
      WriteString('PASS...6.1-2',12)
END T6P1D2.
----------------------

$cx
(*CONFORMANCE*)
MODULE T6P1D3;

  (* TECT ПPOBEPЯET, BЫПOЛHЯЮTCЯ ЛИ ПPABИЛA ПPИOPИTETA
     APИФMETИЧECKИX OПEPAЦИЙ. TAKЖE ПPOBEPЯETCЯ, ЧTO OПEPAЦИИ
     OДHOГO ПPИOPИTETA ИCПOЛHЯЮTCЯ CЛEBA HAПPABO. *)

FROM InOut IMPORT WriteString;
  VAR A,B,C,D,E,F,G:INTEGER;
      H,I,J,K,L,M,N:REAL;
BEGIN A:=1;
      B:=2;
      C:=3;
      D:=4;
      E:=5;
      F:=A-B+C-D;
      G:=E-D DIV B*C;
      H:=1.0;
      I:=2.0;
      J:=3.0;
      K:=4.0;
      L:=5.0;
      M:=H/I*J/K;
      N:=L+K/I-3.0*J;
      IF (F=-2) AND (G=-1) AND (N=-2.0) AND (M>0.37) AND (M<0.38)
         THEN WriteString('PASS...6.1-3',12)
         ELSE WriteString('FAIL...6.1-3',12)
      END
END T6P1D3.
----------------------

$c
(*ERROR HANDLING*)
MODULE T6P1D4;

  (* ПPИ ИCПOЛHEHИИ ДAHHOГO TECTA BOЗБУЖДAETCЯ
     ИCKЛЮЧИTEЛЬHAЯ CИTУAЦИЯ: ДEЛEHИE HA HOЛЬ.
     ЭTO ПPИMEP ДЛЯ OПEPAЦИИ DIV . *)

FROM InOut IMPORT WriteString;

VAR i,j,k:INTEGER;

BEGIN
  i:=6; j:=0;
  k:=i DIV j;
  WriteString('ERROR IS NOT DETECTED...6.1-4',30)
END T6P1D4.
---------------------

$c
(*ERROR HANDLING*)
MODULE T6P1D5;

  (* ПPИ ИCПOЛHEHИИ ДAHHOГO TECTA BOЗБУЖДAETCЯ
     ИCKЛЮЧИTEЛЬHAЯ CИTУAЦИЯ: ДEЛEHИE HA HOЛЬ.
     ЭTO ПPИMEP ДЛЯ OПEPAЦИИ MOD . *)

FROM InOut IMPORT WriteString;
  VAR I,J,K:INTEGER;
BEGIN
      I:=6;J:=0;
      K:=I MOD J;
      WriteString('ERROR IS NOT DETECTED...6.1-5',30)
END T6P1D5.
--------------------

$
(*DEVIANCE*)
MODULE T6P1D6;

  (* TECT ПPEДCTABЛЯET COБOЙ ПPИMEPЫ APИФMETИЧECKИX ДEЙCTBИЙ
     HA ГPAHИЧHЫX ЗHAЧEHИЯX: ИCПOЛЬЗУETCЯ MAXINT. *)

FROM InOut IMPORT WriteString;
  VAR I:INTEGER;
BEGIN I:=-(-MAX(INTEGER));
      I:=-MAX(INTEGER);
      IF ODD(MAX(INTEGER))
      THEN I:=(MAX(INTEGER)-((MAX(INTEGER) DIV 2)+1))*2;
      ELSE I:=(MAX(INTEGER)-(MAX(INTEGER) DIV 2))*2;  -------  here
      END;
      IF I<=MAX(INTEGER)
         THEN WriteString('PASS...6.1-6',12)
         ELSE WriteString('FAIL...6.1-6',12)
      END
END T6P1D6.
---------------------

$
(*DEVIANCE*)
MODULE T6P1D7;

  (* B PEЗУЛЬTATE ИCПOЛHEHИЯ ЭTOЙ ПPOГPAMMЫ ЦEЛOЙ
     ПEPEMEHHOЙ ПPИCBAИBAETCЯ ЗHAЧEHИE БOЛЬШE MAX(INTEGER). *)

FROM InOut IMPORT WriteString;
  VAR I:INTEGER;
BEGIN I:=( MAX(INTEGER)-(MAX(INTEGER) DIV 2))*2+2;
      WriteString('ERROR IS NOT DETECTED...6.1-7',30)
END T6P1D7.
---------------------

$
(*DEVIANCE*)
MODULE T6P1D8;

  (* B PEЗУЛЬTATE ИCПOЛHEHИЯ ЭTOЙ ПPOГPAMMЫ ЦEЛOЙ
     ПEPEMEHHOЙ ПPИCBAИBAETCЯ ЗHAЧEHИE MEHЬШE (-MAX(INTEGER)) . *)

FROM InOut IMPORT WriteString;
  VAR I:INTEGER;
BEGIN I:=(-MAX(INTEGER)+(MAX(INTEGER) DIV 2))*2-2;
      WriteString('ERROR IS NOT DETECTED...6.1-8',30)
END T6P1D8.
----------------------

$c
(*ERROR HANDLING*)
MODULE T6P1D9;

  (* PEЗУЛЬTAT BЫЧИTAHИЯ ДOЛЖEH ИMETЬ TИП INTEGER *)

FROM InOut IMPORT WriteString;

VAR X,Y,Z: [-10..10];

BEGIN
  X:=-5; Y:=10;
  Z:=X-Y;
  WriteString('DEVIATES...6.1-9',16)
END T6P1D9.
----------------------

$cx
(*CONFORMANCE*)
MODULE T6P1D10;

  (* TECT HA ПPOBEPKУ ПPABИЛ ИCПOЛЬЗOBAHИЯ
     APИФMETИЧECKИX OПEPAЦИЙ. *)

FROM InOut IMPORT WriteString;
  VAR Y,X: INTEGER; V,W: INTEGER;
      Z:[-10..10];  U:[0..10];
BEGIN
  X:=91; Z:=-5; Y:=X*Z;
  V:=4; U:=0; W:=V*U;
  WriteString('PASS...6.1-10',12)
END T6P1D10.
-------------------------
$
(*DEVIANCE*)
MODULE T6P1D12;

  (* OДИH ИЗ OПEPAHДOB HE COOTBETCTBУET OПEPAЦИИ  /  . *)

FROM InOut IMPORT WriteString;
  VAR X,Y:REAL;
BEGIN
  Y:=7.6; X:=Y/2;
  WriteString('DEVIATES...6.1-12',12)
END T6P1D12.
-----------------------

$
(*DEVIANCE*)
MODULE T6P1D13;

  (* PEЗУЛЬTAT OПEPAЦИИ HAД BEЩECTBEHHЫMИ OПEPAHДAMИ
     ИMEET TИП INTEGER . *)

FROM InOut IMPORT WriteString;
  VAR X,Y:REAL;Z:INTEGER;
BEGIN X:=5.1;Y:=3.1;
      Z:=X-Y;
      WriteString('DEVIATES...6.1-13',17)
END T6P1D13.
------------------------

$
(*DEVIANCE*)
MODULE T6P1D14;

  (* OПEPAЦИЯ ПPИMEHЯETCЯ K OПEPAHДAM PAЗHOГO TИПA. *)

FROM InOut IMPORT WriteString;
  CONST Z=2;
  VAR X,Y:REAL;
BEGIN Y:=1.0;
      X:=Y*Z;
      WriteString('DEVIATES...6.1-14',17)
END T6P1D14.
------------------------

$cx
(*CONFORMANCE*)
MODULE T6P1D15;

  (* ПPOBEPKA CEMAHTИKИ OПEPAЦИЙ  DIV  И  MOD  . *)

FROM InOut IMPORT WriteString;
  CONST X=13;Y=3;
  VAR B:BOOLEAN;
BEGIN B:=((X DIV Y)*Y+(X MOD Y))=X;
      IF B THEN WriteString('PASS...6.1-15',13)
           ELSE WriteString('FAIL...6.1-15',13)
      END
END T6P1D15.
-------------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D1;

(* ПPOГPAMMA ПPOBEPЯET PAБOTУ OПEPAЦИИ  OR HA PAЗHЫX APГУMEHTAX. *)

FROM InOut IMPORT WriteString;

CONST T=TRUE; F=FALSE;

BEGIN
  IF T OR T THEN WriteString('P',1) ELSE WriteString('F',1)  END;
  IF T OR F THEN WriteString('A',1) ELSE WriteString('A-',2) END;
  IF F OR T THEN WriteString('S',1) ELSE WriteString('I',1)  END;
  IF F OR F THEN WriteString('L',1) ELSE WriteString('S',1)  END;
  WriteString('...8.2-1',8)
END T8P2D1.
-------------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D2;

  (* ПPOBEPЯETCЯ PAБOTA OПEPAЦИИ AND HA PAЗЛИЧHЫX APГУMEHTAX. *)

FROM InOut IMPORT WriteString;

CONST T=TRUE;F=FALSE;

BEGIN
  IF T AND T THEN WriteString('P',1)  ELSE WriteString('F',1) END;
  IF F AND T THEN WriteString('A-',2) ELSE WriteString('A',1) END;
  IF T AND F THEN WriteString('I',1)  ELSE WriteString('S',1) END;
  IF F AND F THEN WriteString('L',1)  ELSE WriteString('S',1) END;
  WriteString('...8.2-2',8)
END T8P2D2.
----------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D3;

  (* ПPOBEPЯETCЯ PAБOTA OПEPAЦИИ NOT HA PAЗЛИЧHЫX APГУMEHTAX. *)

FROM InOut IMPORT WriteString;
  CONST T=TRUE;F=FALSE;
  VAR A:BOOLEAN;
BEGIN
  IF NOT T THEN WriteString('F',1)  ELSE WriteString('P',1)  END;
  IF NOT F THEN WriteString('AS',1) ELSE WriteString('AI',2) END;
  A:=NOT NOT T;
  IF A=T   THEN WriteString('S',1)  ELSE WriteString('L',1)  END;
  WriteString('...8.2-3',12)
END T8P2D3.
--------------------

$cx
(*CONFORMANCE*)
MODULE T8P2D4;

  (* ПPOBEPЯETCЯ BЫПOЛHEHИE HEKOTOPЫX TOЖДECTB *)

FROM InOut IMPORT WriteString;
  VAR A,B,C:BOOLEAN;co:INTEGER;
BEGIN co:=0;
  A:=TRUE;B:=FALSE;C:=TRUE;
  IF (A OR B)=(B OR A)
     THEN co:=co+1
     ELSE WriteString('FAIL...8.2-4...COMMUTATION',26)
  END;
  IF (A OR (B OR C))=((A OR B) OR C)
     THEN co:=co+1
     ELSE WriteString('FAIL...8.2-4...ASSOCIATATION',28)
  END;
  IF (A AND (B OR C))=((A AND B) OR (A AND C))
     THEN co:=co+1
     ELSE WriteString('FAIL...8.2-4...DISTRIBUTION',27)
  END;
  IF NOT (A OR B)=((NOT A) AND (NOT B))
     THEN co:=co+1
     ELSE WriteString('FAIL...8.2-4...NOT (A OR B)',27)
  END;
  IF NOT (A AND B)=((NOT A) OR (NOT B))
     THEN co:=co+1
     ELSE WriteString('FAIL...8.2-4...NOT (A AND B)',28)
  END;
  IF co=5 THEN WriteString('PASS...8.2-4',12) END
END T8P2D4.
---------------------

$cx
(*CONFORMANCE*)
MODULE T6P2D5;

  (* ПPOBEPKA TOГO, ЧTO ЗHAKИ OПEPAЦИЙ TИПA УMHOЖEHИЯ
     ИMEЮT ПPИOPИTET BЫШE, ЧEM ЗHAKИ OTHOШEHИЙ. *)

FROM InOut IMPORT WriteString;
  VAR A,B,C,X,W:BOOLEAN;
      co:INTEGER;
BEGIN co:=0;
      FOR A:=FALSE TO TRUE
      DO  FOR B:=FALSE TO TRUE
          DO FOR C:=FALSE TO TRUE
             DO W:=(A AND B)<C;
                X:=C>B AND A;
                IF W=X
                   THEN co:=co+1
                END
             END
          END
       END;
      IF co=8
         THEN WriteString('PASS...6.2-5',12)
         ELSE WriteString('FAIL...6.2-5',12)
      END
END T6P2D5.
---------------------

$cx
(*CONFORMANCE*)
MODULE T6P2D6;

  (* NOT ИMEET ПPИOPИTET BЫШE, ЧEM AND И OR *)

FROM InOut IMPORT WriteString;
  VAR A,B,X,Y:BOOLEAN;
BEGIN A:=FALSE;B:=A;X:=NOT A AND B;
      A:=TRUE;B:=A;Y:=NOT A OR B;
      IF ((NOT X) AND Y)
         THEN WriteString('PASS...6.2-6',12)
         ELSE WriteString('FAIL...6.2-6',12)
      END
END T6P2D6.
----------------------

$cx
(*CONFORMANCE*)
MODULE T6P2D7;

  (* B BЫPAЖEHИЯX, COДEPЖAЩИX OПEPAЦИЮ OR, CHAЧAЛA BЫЧИCЛЯETCЯ
     ЛEBЫЙ OПEPAHД, A ЗATEM ПPABЫЙ. TECT ПPOBEPЯET, ЧTO ЭTO TAK. *)

FROM InOut IMPORT WriteString;
  VAR A:BOOLEAN;
      K,L:INTEGER;

  PROCEDURE SIDEEFFECT(VAR I:INTEGER;B:BOOLEAN):BOOLEAN;
  BEGIN I:=I+1;
        RETURN B
  END SIDEEFFECT;

BEGIN K:=0;L:=0;
      A:=SIDEEFFECT(K,FALSE) AND SIDEEFFECT(L,FALSE);
      IF (K=0) AND (L=1)
         THEN WriteString('SECOND EXPRESSION EVALUATED...6.2-7',35)
      ELSIF (K=1) AND (L=0)
         THEN WriteString(' FIRST EXPRESSION EVALUATED...6.2-7',35)
      ELSIF (K=1) AND (L=1)
         THEN WriteString('  BOTH EXPRESSION EVALUATED...6.2-7',35)
      ELSE WriteString('FAIL...6.2-7',12)
      END
END T6P2D7.
----------------------

$cx
(*CONFORMANCE*)
MODULE T6P2D8;

  (* B BЫPAЖEHИЯX, COДEPЖAЩИX OПEPAЦИЮ AND, CHAЧAЛA BЫЧИCЛЯETCЯ
     ЛEBЫЙ OПEPAHД, A ЗATEM ПPABЫЙ. TECT ПPOBEPЯET, ЧTO ЭTO TAK. *)

FROM InOut IMPORT WriteString;
  VAR A:BOOLEAN;
      K,L:INTEGER;
  PROCEDURE SIDEEFFECT(VAR I:INTEGER;B:BOOLEAN):BOOLEAN;
  BEGIN I:=I+1;
        RETURN B
  END SIDEEFFECT;
BEGIN K:=0;L:=0;
      A:=SIDEEFFECT(K,TRUE) OR SIDEEFFECT(L,TRUE);
      IF (K=0) AND (L=1)
         THEN WriteString('SECOND EXPRESSION EVALUATED...6.2-8',35)
      ELSIF (K=1) AND (L=0)
         THEN WriteString(' FIRST EXPRESSION EVALUATED...6.2-8',35)
      ELSIF (K=1) AND (L=1)
         THEN WriteString('  BOTH EXPRESSION EVALUATED...6.2-8',35)
      ELSE WriteString('FAIL...6.2-8',12)
      END
END T6P2D8.
---------------------

$
(*DEVIANCE*)
MODULE T6P2D9;

  (* ЛOГИЧECKИE OПEPAЦИИ ПPИMEHИMЫ TOЛЬKO K OПEPAHДAM TИПA BOOLEAN
     И ДAЮT PEЗУЛЬTAT BOOLEAN. *)

FROM InOut IMPORT WriteString;
  VAR I,J:INTEGER;
BEGIN I:=1;J:=2;
      I:=I AND J;I:=I AND 1;
      I:=I OR J; I:=I OR 1;
      I:=NOT J;
      WriteString('DEVIATES...6.2-9',16)
END T6P2D9.
---------------------

$cx
(*CONFORMANCE*)
MODULE T8P2P3D1;

  (* HA HEKOTOPЫX ПPИMEPAX MHOЖECTB ПPOBEPЯETCЯ ПPABИЛЬHOCTЬ *)
  (* BЫПOЛHEHИЯ OПEPAЦИЙ HAД MHOЖECTBAMИ.                    *)

FROM InOut IMPORT WriteString;

  CONST A={0,2,4,6,8,10          };
        B={1,3,5,7,9             };
        C={0,3,6,9               };
        D={0,1,2,3,4,5,6,7,8,9,10};
        E={0,1,5,6,7             };

  VAR co:INTEGER;
BEGIN co:=0;
  IF A+B=D THEN co:=co+1 END;
  IF D-B=A THEN co:=co+1 END;
  IF D*A=A THEN co:=co+1 END;
  IF B/C=E THEN co:=co+1 END;
  IF co=4  THEN WriteString('PASS...8.2.3-1',12)
  ELSE          WriteString('FAIL...8.2.3-1',12)
  END
END T8P2P3D1.
--------------------

$cx
(*CONFORMANCE*)
MODULE T8P2P3D2;

  (* ПPOBEPKA ПPABИЛЬHOCTИ BЫПOЛHEHИЯ OПEPAЦИЙ HAД ПУCTЫM MHOЖECTBOM *)

FROM InOut IMPORT WriteString;
  CONST A={1,2,3};
  VAR co:INTEGER;
BEGIN co:=0;
      IF A+{}=A THEN co:=co+1 END;
      IF A-{}=A THEN co:=co+1 END;
      IF A/{}=A THEN co:=co+1 END;
      IF A*{}={} THEN co:=co+1 END;
      IF co=4
         THEN WriteString('PASS...8.2.3-2',14)
         ELSE WriteString('FAIL...8.2.3-2',14)
      END
END T8P2P3D2.
-----------------------

$cx
(*CONFORMANCE*)
MODULE T8P2P3D3;

  (* ПPOBEPKA BЫПOЛHEHИЯ HEKOTOPЫX TOЖДECTB HA OTДEЛЬHЫX ПPИMEPAX MHOЖECTB *)

FROM InOut IMPORT WriteString;
  CONST A={0,2,4,6,8,10          };
        B={1,3,5,7,9             };
        C={0,1,2,3,4,5,6,7,8,9,10};
  VAR co:INTEGER;
BEGIN co:=0;
      IF A*B={}  THEN co:=co+1 END;
      IF C-B=C/B THEN co:=co+1 END;
      IF A-B=A   THEN co:=co+1 END;
      IF A/B=A+B THEN co:=co+1 END;
      IF co=4
         THEN WriteString('PASS...8.2.3-3',14)
         ELSE WriteString('FAIL...8.2.3-3',14)
      END
END T8P2P3D3.
----------------------

$cx
(*CONFORMANCE*)
MODULE T8P2P3D4;

  (* ПPOBEPKA HEKOTOPЫX TOЖДECTB *)

FROM InOut IMPORT WriteString;
  CONST A={1,2,3};
        B={2,3,4};
        C={1,3,5};
VAR co:INTEGER;
BEGIN co:=0;
      IF A*(B+C)=(A*B)+(A*C) THEN co:=co+1 END;
      IF A+(B*C)=(A+B)*(A+C) THEN co:=co+1 END;
      IF A-(B*C)=(A-B)+(A-C) THEN co:=co+1 END;
      IF A-(B+C)=(A-B)*(A-C) THEN co:=co+1 END;
      IF  A/B  = (A+B)-(A*B) THEN co:=co+1 END;
      IF co=5
         THEN WriteString('PASS...8.2.3-4',14)
         ELSE WriteString('FAIL...8.2.3-4',14)
      END
END T8P2P3D4.
---------------------

