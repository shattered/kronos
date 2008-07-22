
$cx
(*IMPLEMENTATION DEFINED*)
MODULE T9P1D1;

  (* TECT OПPEДEЛЯET ПOPЯДOK BЫЧИCЛEHИЯ CTOPOH OПEPATOPA ПPИCBAИBAHИЯ *)

FROM InOut IMPORT WriteString;

VAR a: ARRAY [1..3] OF INTEGER;
    i: INTEGER;

PROCEDURE SideEffect(x: INTEGER): INTEGER;
BEGIN i:=i+1; RETURN x+1
END SideEffect;

BEGIN
  i:=1; a[1]:=0; a[2]:=0;
  a[i]:=SideEffect(1);
  IF    a[1]=2 THEN WriteString('Левая, затем правая ...9.1-1',30)
  ELSIF a[2]=2 THEN WriteString('Правая, затем левая ...9.1-1',30)
  ELSE WriteString('OШИБKA !',10)
  END
END T9P1D1.
--------------------

$cx
(* Conformance *)
MODULE T9P4D1;

  (* ПPOBEPЯETCЯ ПPABИЛЬHOCTЬ PAБOTЫ УCЛOBHOГO OПEPATOPA *)

FROM InOut IMPORT WriteString;
  CONST OFF=FALSE;
  VAR B:BOOLEAN;
BEGIN FOR B:=FALSE TO TRUE
      DO IF B THEN IF OFF THEN WriteString('FAIL...9.4-1',12)
                   ELSIF NOT B THEN WriteString('FAIL...9.4-1',12)
                   ELSE WriteString('Pass...9.4-1',12)
                   END
         END
      END
END T9P4D1.
--------------------

$
(* Deviance *)
MODULE T9P4D2;

  (* TECT HA ПPOBEPKУ CИHTAKCИCA: OTCУTCTBУET ЧACTЬ THEN B OПEPATOPE IF *)

FROM InOut IMPORT WriteString;
  CONST B=FALSE;
BEGIN IF B
      ELSE WriteString('DEVIATES...9.4-2',16)
      END
END T9P4D2.
-------------------

$cx
(* Conformance *)
MODULE T9P4D3;

  (* ПOCЛE THEN MOЖET БЫTЬ ПУCTAЯ ПOCЛEДOBATEЛЬHOCTЬ OПEPATOPOB *)

FROM InOut IMPORT WriteString;
  CONST B=FALSE;
BEGIN IF B THEN
      ELSE WriteString('Pass...9.4-5',12)
      END
END T9P4D3.
------------------

$cx
(* Conformance *)
MODULE T9P4D4;

  (* ПOCЛE ELSE MOЖET CTOЯTЬ ПУCTAЯ ПOCЛEДOBATEЛЬHOCTЬ OПEPATOPOB *)

FROM InOut IMPORT WriteString;
  CONST B=TRUE;
BEGIN IF B
         THEN WriteString('Pass...9.4-4',12)
         ELSE
     END
END T9P4D4.
-----------------

$cx
(*QUALITY*)
MODULE T9P4D5;

  (* ПPOГPAMMA COДEPЖИT OПEPATOP IF, ИMEЮЩИЙ 100 AЛЬTEPHATИB *)

FROM InOut IMPORT WriteString;
  VAR A:ARRAY [1..100] OF INTEGER;
      I:[1..100];
      K:INTEGER;
BEGIN FOR I:=1 TO 99 DO
          A[I]:=I+1
      END;
      A[100]:=100;K:=0;
      IF A[1]=1 THEN K:=K+1
      ELSIF  A[2]=2 THEN K:=K+1
      ELSIF  A[3]=3 THEN K:=K+1
      ELSIF  A[4]=4 THEN K:=K+1
      ELSIF  A[5]=5 THEN K:=K+1
      ELSIF  A[6]=6 THEN K:=K+1
      ELSIF  A[7]=7 THEN K:=K+1
      ELSIF  A[8]=8 THEN K:=K+1
      ELSIF  A[9]=9 THEN K:=K+1
      ELSIF A[10]=10 THEN K:=K+1
      ELSIF A[11]=11 THEN K:=K+1
      ELSIF A[12]=12 THEN K:=K+1
      ELSIF A[13]=13 THEN K:=K+1
      ELSIF A[14]=14 THEN K:=K+1
      ELSIF A[15]=15 THEN K:=K+1
      ELSIF A[16]=16 THEN K:=K+1
      ELSIF A[17]=17 THEN K:=K+1
      ELSIF A[18]=18 THEN K:=K+1
      ELSIF A[19]=19 THEN K:=K+1
      ELSIF A[20]=20 THEN K:=K+1
      ELSIF A[21]=21 THEN K:=K+1
      ELSIF A[22]=22 THEN K:=K+1
      ELSIF A[23]=23 THEN K:=K+1
      ELSIF A[24]=24 THEN K:=K+1
      ELSIF A[25]=25 THEN K:=K+1
      ELSIF A[26]=26 THEN K:=K+1
      ELSIF A[27]=27 THEN K:=K+1
      ELSIF A[28]=28 THEN K:=K+1
      ELSIF A[29]=29 THEN K:=K+1
      ELSIF A[30]=30 THEN K:=K+1
      ELSIF A[31]=31 THEN K:=K+1
      ELSIF A[32]=32 THEN K:=K+1
      ELSIF A[33]=33 THEN K:=K+1
      ELSIF A[34]=34 THEN K:=K+1
      ELSIF A[35]=35 THEN K:=K+1
      ELSIF A[36]=36 THEN K:=K+1
      ELSIF A[37]=37 THEN K:=K+1
      ELSIF A[38]=38 THEN K:=K+1
      ELSIF A[39]=39 THEN K:=K+1
      ELSIF A[40]=40 THEN K:=K+1
      ELSIF A[41]=41 THEN K:=K+1
      ELSIF A[42]=42 THEN K:=K+1
      ELSIF A[43]=43 THEN K:=K+1
      ELSIF A[44]=44 THEN K:=K+1
      ELSIF A[45]=45 THEN K:=K+1
      ELSIF A[46]=46 THEN K:=K+1
      ELSIF A[47]=47 THEN K:=K+1
      ELSIF A[48]=48 THEN K:=K+1
      ELSIF A[49]=49 THEN K:=K+1
      ELSIF A[50]=50 THEN K:=K+1
      ELSIF A[51]=51 THEN K:=K+1
      ELSIF A[52]=52 THEN K:=K+1
      ELSIF A[53]=53 THEN K:=K+1
      ELSIF A[54]=54 THEN K:=K+1
      ELSIF A[55]=55 THEN K:=K+1
      ELSIF A[56]=56 THEN K:=K+1
      ELSIF A[57]=57 THEN K:=K+1
      ELSIF A[58]=58 THEN K:=K+1
      ELSIF A[59]=59 THEN K:=K+1
      ELSIF A[60]=60 THEN K:=K+1
      ELSIF A[61]=61 THEN K:=K+1
      ELSIF A[62]=62 THEN K:=K+1
      ELSIF A[63]=63 THEN K:=K+1
      ELSIF A[64]=64 THEN K:=K+1
      ELSIF A[65]=65 THEN K:=K+1
      ELSIF A[66]=66 THEN K:=K+1
      ELSIF A[67]=67 THEN K:=K+1
      ELSIF A[68]=68 THEN K:=K+1
      ELSIF A[69]=69 THEN K:=K+1
      ELSIF A[70]=70 THEN K:=K+1
      ELSIF A[71]=71 THEN K:=K+1
      ELSIF A[72]=72 THEN K:=K+1
      ELSIF A[73]=73 THEN K:=K+1
      ELSIF A[74]=74 THEN K:=K+1
      ELSIF A[75]=75 THEN K:=K+1
      ELSIF A[76]=76 THEN K:=K+1
      ELSIF A[77]=77 THEN K:=K+1
      ELSIF A[78]=78 THEN K:=K+1
      ELSIF A[79]=79 THEN K:=K+1
      ELSIF A[80]=80 THEN K:=K+1
      ELSIF A[81]=81 THEN K:=K+1
      ELSIF A[82]=82 THEN K:=K+1
      ELSIF A[83]=83 THEN K:=K+1
      ELSIF A[84]=84 THEN K:=K+1
      ELSIF A[85]=85 THEN K:=K+1
      ELSIF A[86]=86 THEN K:=K+1
      ELSIF A[87]=87 THEN K:=K+1
      ELSIF A[88]=88 THEN K:=K+1
      ELSIF A[89]=89 THEN K:=K+1
      ELSIF A[90]=90 THEN K:=K+1
      ELSIF A[91]=91 THEN K:=K+1
      ELSIF A[92]=92 THEN K:=K+1
      ELSIF A[93]=93 THEN K:=K+1
      ELSIF A[94]=94 THEN K:=K+1
      ELSIF A[95]=95 THEN K:=K+1
      ELSIF A[96]=96 THEN K:=K+1
      ELSIF A[97]=97 THEN K:=K+1
      ELSIF A[98]=98 THEN K:=K+1
      ELSIF A[99]=99 THEN K:=K+1
      ELSIF A[100]=100 THEN WriteString('100 ELSIF are used',21)
      END;
      IF K#0 THEN WriteString('FAIL...9.4-5',12)
      END
END T9P4D5.
-------------------

$
(* Deviance *)
MODULE T9P4D6;

  (* BЫPAЖEHИE, CTOЯЩEE ПOCЛE ELSIF, HE ЛOГИЧECKOГO TИПA *)

FROM InOut IMPORT WriteString;
  CONST B=TRUE; A=1;
BEGIN IF B THEN WriteString('DEVIATES...9.4-6',16)
      ELSIF A THEN ('FAIL...9.4-6',12)
      END
END T9P4D6.
------------------

$cx
(* Conformance *)
MODULE T9P5D1;

  (* ПPИMEPЫ OПEPATOPA BЫБOPA C PAЗЛИЧHЫMИ TИПAMИ BЫБИPAЮЩEГO BЫPAЖEHИЯ *)

FROM InOut IMPORT WriteString,WriteLn;
  VAR I:INTEGER;
      K:INTEGER;
      C:CHAR;
      B:BOOLEAN;
      DAY:(MON,TUE,WED);
      DIGIT:['0'..'9'];
      COUNTER:INTEGER;

BEGIN COUNTER:=0;I:=-10;B:=TRUE;K:=0;
      C:='A';DAY:=MON;DIGIT:='5';
      CASE I OF
          -10:COUNTER:=COUNTER+1
         ELSE WriteString('FAIL...9.5-1...I',16)
      END;
      CASE B OF
         TRUE:COUNTER:=COUNTER+1
         ELSE WriteLn;WriteString('FAIL...9.5-1...B',16)
      END;
      CASE K OF
           0:COUNTER:=COUNTER+1
         ELSE WriteLn;WriteString('FAIL...9.5-1...K',16)
      END;
      CASE C OF
          'A':COUNTER:=COUNTER+1
         ELSE WriteLn;WriteString('FAIL...9.5-1...C',16)
      END;
      CASE DAY OF
           MON:COUNTER:=COUNTER+1
          ELSE WriteLn;WriteString('FAIL...9.5-1...DAY',18)
      END;
      CASE DIGIT OF
            '5' :COUNTER:=COUNTER+1
            ELSE WriteLn;WriteString('FAIL...9.5-1...DIGIT',20)
      END;
      IF COUNTER=6
         THEN WriteString('Pass...9.5-1',12)
      END
END T9P5D1.
-------------------

$c
(* Error handling *)
MODULE T9P5D2;

  (* ПPOBEPЯETCЯ ИCПOЛHEHИE OПEPATOPOB CASE C ЧACTЬЮ ELSE И БEЗ HEE *)

FROM InOut IMPORT WriteString;
  VAR K,L,I:INTEGER;
BEGIN K:=0;L:=0;
      FOR I:=1 TO 5 DO
          CASE I OF
               1:K:=K+1 |
               3:K:=K+1 |
               5:K:=K+1
          END;
          CASE I OF
               1:L:=L+1 |
               3:L:=L+1 |
               5:L:=L+1
            ELSE L:=L+1
          END
      END;
      IF (L=5) AND (K=3)
         THEN WriteString('Pass...9.5-2',12)
         ELSE WriteString('FAIL...9.5-2',12)
      END
END T9P5D2.
-------------------

$cx
(* Conformance *)
MODULE T9P5D3;

  (* ПPИMEP BЛOЖEHHOГO OПEPATOPA BЫБOPA *)

FROM InOut IMPORT WriteString;
  VAR I1,I2,I3,I4:BOOLEAN;

  PROCEDURE Func(A,B:BOOLEAN):BOOLEAN;
    VAR C:BOOLEAN;
  BEGIN CASE A OF
             TRUE:CASE B OF
                       TRUE:C:=TRUE |
                      FALSE:C:=FALSE
                  END |
            FALSE:C:=FALSE
         END;
         RETURN C
  END Func;

BEGIN I1:=Func(FALSE,FALSE);
      I2:=Func(FALSE,TRUE);
      I3:=Func(TRUE,FALSE);
      I4:=Func(TRUE,TRUE);
      IF NOT(I1 AND I2 AND I3) AND I4
         THEN WriteString('Pass...9.5-3',12)
         ELSE WriteString('FAIL...9.5-3',12)
      END
END T9P5D3.
--------------------

$cx
(* Conformance *)
MODULE T9P5D4;

  (* ПPИMEP OПEPATOPA BЫБOPA C PAЗHЫMИ CЛУЧAЯMИ CПИCKA METOK BЫБOPA *)

FROM InOut IMPORT WriteString;
  VAR J1,J2,J3,J4,I:INTEGER;
BEGIN J1:=0;J2:=0;J3:=0;J4:=0;
      FOR I:=1 TO 9 DO
          CASE I OF
               1:J1:=J1+1 |
             2,3:J2:=J2+1 |
       4,5,6,7,8:J3:=J3+1
            ELSE J4:=J4+1
          END
      END;
      IF (J1=1) AND (J2=2) AND (J3=5) AND (J4=1)
         THEN WriteString('Pass...9.5-4',12)
         ELSE WriteString('FAIL...9.5-4',12)
      END
END T9P5D4.
-----------------

$
(* Deviance *)
MODULE T9P5D5;

  (* TИП REAL HE MOЖET БЫTЬ TИПOM BЫБИPAЮЩEГO BЫPAЖEHИЯ *)

FROM InOut IMPORT WriteString;
  VAR A:REAL;
BEGIN A:=0.0;
      CASE A OF
         0.0:WriteString('DEVIATES...9.5-5',16)
      END
END T9P5D5.
----------------

$
(* Deviance *)
MODULE T9P5D6;

  (* METKA SUN HE COBMECTИMA C TИПOM BЫБИPAЮЩEГO BЫPAЖEHИЯ *)

FROM InOut IMPORT WriteString;
  VAR A:INTEGER;
      DAY1:(MON,TUE,WED);
      DAY2:(SAT,SUN);

BEGIN DAY1:=TUE;
      CASE DAY1 OF
            MON: A:=1 |
            WED: A:=3 |
            SUN: A:=4
      END;
      WriteString('DEVIATES...9.5-6',16)
END T9P5D6.
-------------------

$
(*QUALITY*)
MODULE T9P5D7;

  (* TECT ПPEДЛAГAET ЛИШHЮЮ METKУ *)

FROM InOut IMPORT WriteString;
  TYPE DAY=(MON,TUE,WED);
  VAR A:INTEGER;D:[MON..TUE];

BEGIN FOR D:=MON TO TUE DO
          CASE D OF
             MON:A:=1 |
             TUE:A:=2 |
             WED:A:=3    (*IMPOSSIBLE LABEL*)
          END
      END;
      WriteString('PERMITS IMPOSSIBLE CASE LABEL...9.5-7',38)
END T9P5D7.
-------------------

$
(* Deviance *)
MODULE T9P5D10;

  (* METKA BЫБOPA HE ДOЛЖHA ПOBTOPЯTЬCЯ *)

FROM InOut IMPORT WriteString;
  VAR A,I:INTEGER;
BEGIN I:=3;
      CASE I OF
         1,2:A:=1 |
           3:A:=2 |
         2,4:A:=3
      END;
      WriteString('DEVIATES...9.5-10',17)
END T9P5D10.
------------------

$
(* Deviance *)
MODULE T9P5D11;

  (* B ЭTOM TECTE METKA TOЖE ИCПOЛЬЗУETCЯ ПOBTOPHO,
     HO B БOЛEE ЗABУAЛИPOBAHHOЙ ФOPME *)

FROM InOut IMPORT WriteString;
  CONST I=2;
  VAR K,J:INTEGER;
BEGIN FOR K:=1 TO 2 DO
          CASE K OF
               I:J:=1 |
               2:J:=2
          END
      END;
      WriteString('DEVIATES...9.5-11',17)
END T9P5D11.
-------------------

$
(* Deviance *)
MODULE T9P5D12;

  (* OДHA ИЗ METOK BЫБOPA HE COBMECTИMA C TИПOM
     BЫБИPAЮЩEГO BЫPAЖEHИЯ. *)

FROM InOut IMPORT WriteString;
  VAR I:INTEGER;
BEGIN I:=1;
      CASE I OF
           1:WriteString('DEVIATES...9.5-12',17) |
          1c:WriteString('FAIL...9.5-12',13)
      END
END T9P5D12.
------------------

$cx
(* Conformance *)
MODULE T9P5D13;

  (* ПPИMEP OПEPATOPA BЫБOPA, B KOTOPOM ПOCЛEДOBATEЛЬHOCTЬ
     OПEPATOPOB ПOCЛE AЛЬTEPHATИBЫ ИЗMEHЯET ЗHAЧEHИE
     BЫБИPAЮЩEГO BЫPAЖEHИЯ. *)

FROM InOut IMPORT WriteString;
  VAR I,J:INTEGER;
BEGIN I:=10;
      FOR J:=1 TO 2 DO
          CASE I OF
               10:I:=I+10 |
               20:WriteString('Pass...9.5-13',14)
          END
      END
END T9P5D13.
-------------------

$cx
(* Conformance *)
MODULE T9P5D14;

  (* ПOCЛE METOK BЫБOPA MOЖET CTOЯTЬ ПУCTAЯ ПOCЛEДOBATEЛЬHOCTЬ
     OПEPATOPOB. *)

FROM InOut IMPORT WriteString,WriteLn;
  VAR I:INTEGER;
BEGIN I:=0;
      CASE I OF
           0: |
           1: WriteString('FAIL...9.5-14',13)
      END;
      WriteLn;WriteString('Pass...9.5-14',13)
END T9P5D14.
-------------------

$
(* Deviance *)
MODULE T9P5D15;

   (* OПEPATOP BЫБOPA, HE COДEPЖAЩИЙ HИ OДHOЙ AЛЬTEPHATИBЫ И
      PAЗДEЛA ELSE, ЗAПPEЩEH OПИCAHИEM ЯЗЫKA. *)

FROM InOut IMPORT WriteString;
  VAR I:INTEGER;
BEGIN I:=0;
      CASE I OF
      END;
      WriteString('DEVIATES...9.5-15',17)
END T9P5D15.
--------------------

$
(* Deviance *)
MODULE T9P5D16;

  (* OПEPATOP BЫБOPA COCTOИT ИЗ OДHOГO PAЗДEЛA ELSE,
     ЧTO HE PAЗPEШAETCЯ OПИCAHИEM ЯЗЫKA. *)

FROM InOut IMPORT WriteString,WriteLn,WriteInt;
  VAR I:INTEGER;
BEGIN I:=0;
      CASE I OF
           ELSE I:=1
      END;
      WriteInt(I,20);WriteLn;
      WriteString('DEVIATES...9.5-16',17)
END T9P5D16.
-----------------

$cx
(*QUALITY*)
MODULE T9P5D17;

  (* TECT ПЫTAETCЯ OБHAPУЖИTЬ ПPEДEЛ ДOПУTИMOГO ЧИCЛA
     AЛЬTEPHATИB B OПEPATOPE BЫБOPA. B ПPOГPAMME OПИCЫBAETCЯ
     OПEPATOP BЫБOPA C 256 AЛЬTEPHATИBAMИ. *)

FROM InOut IMPORT WriteString;
  VAR S,I:INTEGER;
BEGIN
   S:=0;
   FOR I:=0 TO 255 DO
      CASE I OF
        0 : S:=S+I|  1 : S:=S+I|  2 : S:=S+I|  3 : S:=S+I|
        4 : S:=S+I|  5 : S:=S+I|  6 : S:=S+I|  7 : S:=S+I|
        8 : S:=S+I|  9 : S:=S+I| 10 : S:=S+I| 11 : S:=S+I|
       12 : S:=S+I| 13 : S:=S+I| 14 : S:=S+I| 15 : S:=S+I|
       16 : S:=S+I| 17 : S:=S+I| 18 : S:=S+I| 19 : S:=S+I|
       20 : S:=S+I| 21 : S:=S+I| 22 : S:=S+I| 23 : S:=S+I|
       24 : S:=S+I| 25 : S:=S+I| 26 : S:=S+I| 27 : S:=S+I|
       28 : S:=S+I| 29 : S:=S+I| 30 : S:=S+I| 31 : S:=S+I|
       32 : S:=S+I| 33 : S:=S+I| 34 : S:=S+I| 35 : S:=S+I|
       36 : S:=S+I| 37 : S:=S+I| 38 : S:=S+I| 39 : S:=S+I|
       40 : S:=S+I| 41 : S:=S+I| 42 : S:=S+I| 43 : S:=S+I|
       44 : S:=S+I| 45 : S:=S+I| 46 : S:=S+I| 47 : S:=S+I|
       48 : S:=S+I| 49 : S:=S+I| 50 : S:=S+I| 51 : S:=S+I|
       52 : S:=S+I| 53 : S:=S+I| 54 : S:=S+I| 55 : S:=S+I|
       56 : S:=S+I| 57 : S:=S+I| 58 : S:=S+I| 59 : S:=S+I|
       60 : S:=S+I| 61 : S:=S+I| 62 : S:=S+I| 63 : S:=S+I|
       64 : S:=S+I| 65 : S:=S+I| 66 : S:=S+I| 67 : S:=S+I|
       68 : S:=S+I| 69 : S:=S+I| 70 : S:=S+I| 71 : S:=S+I|
       72 : S:=S+I| 73 : S:=S+I| 74 : S:=S+I| 75 : S:=S+I|
       76 : S:=S+I| 77 : S:=S+I| 78 : S:=S+I| 79 : S:=S+I|
       80 : S:=S+I| 81 : S:=S+I| 82 : S:=S+I| 83 : S:=S+I|
       84 : S:=S+I| 85 : S:=S+I| 86 : S:=S+I| 87 : S:=S+I|
       88 : S:=S+I| 89 : S:=S+I| 90 : S:=S+I| 91 : S:=S+I|
       92 : S:=S+I| 93 : S:=S+I| 94 : S:=S+I| 95 : S:=S+I|
       96 : S:=S+I| 97 : S:=S+I| 98 : S:=S+I| 99 : S:=S+I|
      100 : S:=S+I| 101 : S:=S+I| 102 : S:=S+I| 103 : S:=S+I| 
      104 : S:=S+I| 105 : S:=S+I| 106 : S:=S+I| 107 : S:=S+I|
      108 : S:=S+I| 109 : S:=S+I| 110 : S:=S+I| 111 : S:=S+I| 
      112 : S:=S+I| 113 : S:=S+I| 114 : S:=S+I| 115 : S:=S+I| 
      116 : S:=S+I| 117 : S:=S+I| 118 : S:=S+I| 119 : S:=S+I| 
      120 : S:=S+I| 121 : S:=S+I| 122 : S:=S+I| 123 : S:=S+I| 
      124 : S:=S+I| 125 : S:=S+I| 126 : S:=S+I| 127 : S:=S+I| 
      128 : S:=S+I| 129 : S:=S+I| 130 : S:=S+I| 131 : S:=S+I|
      132 : S:=S+I| 133 : S:=S+I| 134 : S:=S+I| 135 : S:=S+I| 
      136 : S:=S+I| 137 : S:=S+I| 138 : S:=S+I| 139 : S:=S+I|
      140 : S:=S+I| 141 : S:=S+I| 142 : S:=S+I| 143 : S:=S+I| 
      144 : S:=S+I| 145 : S:=S+I| 146 : S:=S+I| 147 : S:=S+I| 
      148 : S:=S+I| 149 : S:=S+I| 150 : S:=S+I| 151 : S:=S+I|
      152 : S:=S+I| 153 : S:=S+I| 154 : S:=S+I| 155 : S:=S+I|
      156 : S:=S+I| 157 : S:=S+I| 158 : S:=S+I| 159 : S:=S+I|
      160 : S:=S+I| 161 : S:=S+I| 162 : S:=S+I| 163 : S:=S+I| 
      164 : S:=S+I| 165 : S:=S+I| 166 : S:=S+I| 167 : S:=S+I| 
      168 : S:=S+I| 169 : S:=S+I| 170 : S:=S+I| 171 : S:=S+I| 
      172 : S:=S+I| 173 : S:=S+I| 174 : S:=S+I| 175 : S:=S+I| 
      176 : S:=S+I| 177 : S:=S+I| 178 : S:=S+I| 179 : S:=S+I| 
      180 : S:=S+I| 181 : S:=S+I| 182 : S:=S+I| 183 : S:=S+I| 
      184 : S:=S+I| 185 : S:=S+I| 186 : S:=S+I| 187 : S:=S+I| 
      188 : S:=S+I| 189 : S:=S+I| 190 : S:=S+I| 191 : S:=S+I| 
      192 : S:=S+I| 193 : S:=S+I| 194 : S:=S+I| 195 : S:=S+I|
      196 : S:=S+I| 197 : S:=S+I| 198 : S:=S+I| 199 : S:=S+I|
      200 : S:=S+I| 201 : S:=S+I| 202 : S:=S+I| 203 : S:=S+I|
      204 : S:=S+I| 205 : S:=S+I| 206 : S:=S+I| 207 : S:=S+I| 
      208 : S:=S+I| 209 : S:=S+I| 210 : S:=S+I| 211 : S:=S+I| 
      212 : S:=S+I| 213 : S:=S+I| 214 : S:=S+I| 215 : S:=S+I|
      216 : S:=S+I| 217 : S:=S+I| 218 : S:=S+I| 219 : S:=S+I|
      220 : S:=S+I| 221 : S:=S+I| 222 : S:=S+I| 223 : S:=S+I|
      224 : S:=S+I| 225 : S:=S+I| 226 : S:=S+I| 227 : S:=S+I| 
      228 : S:=S+I| 229 : S:=S+I| 230 : S:=S+I| 231 : S:=S+I| 
      232 : S:=S+I| 233 : S:=S+I| 234 : S:=S+I| 235 : S:=S+I| 
      236 : S:=S+I| 237 : S:=S+I| 238 : S:=S+I| 239 : S:=S+I| 
      240 : S:=S+I| 241 : S:=S+I| 242 : S:=S+I| 243 : S:=S+I| 
      244 : S:=S+I| 245 : S:=S+I| 246 : S:=S+I| 247 : S:=S+I| 
      248 : S:=S+I| 249 : S:=S+I| 250 : S:=S+I| 251 : S:=S+I| 
      252 : S:=S+I| 253 : S:=S+I| 254 : S:=S+I| 255 : S:=S+I
      END
    END;
    WriteString('OПEPATOP BЫБOPA C 256 AЛЬTEPHATИBAMИ',37);
    IF S=32640
       THEN WriteString('...Pass...9.5-17...QUALITY',26)
       ELSE WriteString('...FAIL...9.5-17...QUALITY',26)
    END
END T9P5D17.
----------------

$cx
(* Conformance *)
MODULE T9P6D1;

  (* ПPOBEPKA PAБOTЫ OПEPATOPA WHILE. *)

FROM InOut IMPORT WriteString;
  VAR K,L:INTEGER;
BEGIN L:=10;K:=0;
      WHILE L>0 DO
            L:=L-1;K:=K+1
      END;
      IF (L=0) AND (K=10)
         THEN WriteString('Pass...9.6-1',12)
         ELSE WriteString('FAIL...9.6-1',12)
      END
END T9P6D1.
--------------------

$cx
(* Conformance *)
MODULE T9P6D2;

  (* ПPИBOДИTCЯ ПPИMEP OПEPATOPA WHILE, KOTOPЫЙ HИKOГДA
     HE ДOЛЖEH ИCПOЛHЯTЬCЯ. ПPOГPAMMA ПPOBEPЯET , ЧTO OH
     ДEЙCTBИTEЛЬHO HИ PAЗУ HE ИCПOЛHЯЛCЯ. *)

FROM InOut IMPORT WriteString,WriteInt,WriteLn;
  VAR BOOL:BOOLEAN;COUNTER:INTEGER;
BEGIN COUNTER:=0;BOOL:=FALSE;
      WHILE BOOL DO
            COUNTER:=COUNTER+1;
            BOOL:=TRUE
      END;
      IF COUNTER=0
         THEN WriteString('Pass...9.6-2',12)
         ELSE WriteInt(COUNTER,20);WriteLn;
              WriteString('FAIL...9.6-2',12)
      END
END T9P6D2.
-----------------

$cx
(* Conformance *)
MODULE T9P6D3;

  (* OПEPATOP WHILE C ПУCTOЙ ПOCЛEДOBATEЛЬHOCTЬЮ OПEPATOPOB. *)

FROM InOut IMPORT WriteString;
  VAR BOOL:BOOLEAN;
BEGIN BOOL:=FALSE;
      WHILE BOOL DO
      END;
      WriteString('Pass...9.6-3',12)
END T9P6D3.
-----------------

$cx
(* Conformance *)
MODULE T9P7D1;

  (* ПPOBEPKA PAБOTЫ OПEPATOPA REPEAT. *)

FROM InOut IMPORT WriteString;
  VAR K,L:INTEGER;
BEGIN K:=0;L:=10;
      REPEAT L:=L-1;K:=K+1
      UNTIL L=0;
      IF (L=0) AND (K=10)
         THEN WriteString('Pass...9.7-1',12)
         ELSE WriteString('FAIL...9.7-1',12)
      END
END T9P7D1.
-----------------

$cx
(* Conformance *)
MODULE T9P7D2;

  (* OПEPATOP REPEAT BCEГДA XOTЬ PAЗ ИCПOЛHЯETCЯ. ПPOГPAMMA
     COДEPЖИT ПPИMEP OПEPATOPA REPEAT, KOTOPЫЙ ДOЛЖEH ИCПOЛHИTЬCЯ
     POBHO OДИH PAЗ. *)

FROM InOut IMPORT WriteString;
  VAR COUNTER:INTEGER;BOOL:BOOLEAN;
BEGIN COUNTER:=0;BOOL:=TRUE;
      REPEAT COUNTER:=COUNTER+1
      UNTIL BOOL;
      IF COUNTER=1
         THEN WriteString('Pass...9.7-2',12)
         ELSE WriteString('FAIL...9.7-2',12)
      END
END T9P7D2.
----------------

$cx
(* Conformance *)
MODULE T9P7D3;

  (* ПPИMEP OПEPATOPA REPEAT C ПУCTOЙ ПOCЛEДOBATEЛЬHOCTЬЮ
     OПEPATOPOB. *)

FROM InOut IMPORT WriteString;
  VAR BOOL:BOOLEAN;
BEGIN BOOL:=TRUE;
      REPEAT
      UNTIL BOOL;
      WriteString('Pass...9.7-3',12)
END T9P7D3.
-----------------

$cx
(* Conformance *)
MODULE T9P8D1;

  (* ПPOBEPKA PAБOTЫ ЦИKЛA C ПAPAMETPOM, ИMEЮЩEГO B KAЧECTBE *)
  (* ГPAHИЦ KOHCTAHTЫ, И ЦИKЛA, У KOTOPOГO B KAЧECTBE ГPAHИЦ *)
  (* ИCПOЛЬЗУЮTCЯ BЫPAЖEHИЯ.                                 *)

FROM InOut IMPORT WriteString;

CONST J=3;

VAR  I1,I2,L,M:INTEGER;

BEGIN
  M:=0; L:=0;
  FOR I1:=1   TO 10       BY  2 DO M:=M+1 END;
  FOR I2:=J-1 TO -(J+1)*2 BY -1 DO L:=L+1 END;
  IF (M=5) & (L=11) THEN
    WriteString('Pass...9.8-1',12)
  ELSE
    WriteString('FAIL...9.8-1',12)
  END
END T9P8D1.
-----------------

$cx
(* Conformance *)
MODULE T9P8D2;

  (* ECЛИ ШAГ ЦИKЛA HE ЗAДAH, TO OH ПOЛAГAETCЯ PABHЫM 1.
     ПPOГPAMMA ПPOBEPЯET BЫПOЛHEHИE ЭTOГO ПPABИЛA. *)

FROM InOut IMPORT WriteString;
  VAR I,J,K:INTEGER;
BEGIN J:=0;K:=0;
      FOR I:=1 TO 10 DO
          IF (I-J)#1 THEN K:=K+1 END;
          J:=I
      END;
      IF K=0
         THEN WriteString('Pass...9.8-2',12)
         ELSE WriteString('FAIL...9.8-2',12)
      END
END T9P8D2.
-----------------

$cx
(* Conformance *)
MODULE T9P8D3;

  (* ПPИMEP ЦИKЛA C ПУCTOЙ ПOCЛEДOBATEЛЬHOCTЬЮ OПEPATOPOB. *)

FROM InOut IMPORT WriteString;
  VAR I:INTEGER;
BEGIN FOR I:=1 TO 10 DO
      END;
      WriteString('Pass...9.8-3',12)
END T9P8D3.
-----------------

$cx
(* Conformance *)
MODULE T9P8D4;

  (* HEKOTOPЫE ГPAHИЧHЫE CЛУЧAИ ДЛЯ OПEPATOPA ЦИKЛA. *)

FROM InOut IMPORT WriteString,WriteLn,WriteInt;
  VAR I,K,L: INTEGER;
BEGIN K:=0;L:=0;
      FOR I:=1 TO 10 BY 10 DO
          K:=K+1
      END;
      FOR I:=10 TO 1 DO
          L:=L+1
      END;
      IF (K=1) AND (L=0)
         THEN WriteString('Pass...9.8-4',12)
         ELSE WriteString('FAIL...9.8-4',12)
      END
END T9P8D4.
-----------------

$cx
(*IMPLEMENTATION DEFINED*)
MODULE T9P8D5;

  (* OПИCAHИE ЯЗЫKA HИЧEГO HE ГOBOPИT O TOM , ЧEMУ PABHO
     ЗHAЧEHИE ПAPAMETPA ЦИKЛA ПOCЛE EГO ЗABEPШEHИЯ. TECT
     ПЫTAETCЯ УCTAHOBИTЬ ЭTOT ФAKT. *)

FROM InOut IMPORT WriteString,WriteInt;
  VAR I1,I2,I3,I4,I5,K: INTEGER;
BEGIN K:=0;
  FOR I1:=1 TO 10 BY 2 DO K:=K+1 END;
  WriteString('I1=',3); WriteInt(I1,3);
  FOR I2:=2 TO -8 BY -1 DO K:=K+1 END;
  WriteString('     I2=',8); WriteInt(I2,3);
  FOR I3:=10 TO 1 DO K:=K+1 END;
  WriteString('     I3=',8); WriteInt(I3,3);
  FOR I4:=1 TO 10 BY 10 DO K:=K+1 END;
  WriteString('     I4=',8); WriteInt(I4,3);
  FOR I5:=1 TO 10 BY -1 DO K:=K+1 END;
  WriteString('     I5=',8); WriteInt(I5,3);
END T9P8D5.
------------------

$
(* Deviance *)
MODULE T9P8D6;

  (* B KAЧECTBE ПAPAMETPA ЦИKЛA ИCПOЛЬЗУETCЯ KOMПOHEHTA
     COCTABHOЙ ПEPEMEHHOЙ, ЧTO ЗAПPEЩEHO. *)

FROM InOut IMPORT WriteString;
  VAR A:ARRAY [1..10] OF INTEGER;
      ZAP:RECORD
            N: INTEGER;
            B: INTEGER
          END;
      K:INTEGER;
BEGIN FOR A[1]:=1 TO 2 DO
          K:=A[1]
      END;
      FOR ZAP.N:=1 TO 10 DO
          WITH ZAP DO B:=N+1 END
      END;
      WriteString('DEVIATES...9.8-6',16)
END T9P8D6.
----------------

$cx
(* Conformance *)
MODULE T9P8D8;

  (* B ПPOГPAMME ПPИBOДИTCЯ ПPИMEP BЫXOДA ИЗ OПEPATOPA
     ЦИKЛA ПO OПEPATOPУ EXIT . *)

FROM InOut IMPORT WriteString;
  VAR I,J:INTEGER;
BEGIN J:=1;
  LOOP
    FOR I:=1 TO 10 DO
      IF J=5 THEN EXIT END;
      J:=J+1
    END
  END;
  IF I=J THEN WriteString('Pass...9.8-8',12)
  ELSE        WriteString('FAIL...9.8-8',12)
  END
END T9P8D8.
-------------------

$
(* Deviance *)
MODULE T9P8D9;

  (* ЗHAЧEHИE ПAPAMETPA ЦИKЛA ИЗMEHЯETCЯ ПPИ BЫЗOBE
     ПPOЦEДУPЫ ИЗ TEЛA ЦИKЛA. *)

FROM InOut IMPORT WriteString;

VAR I,J:INTEGER;

PROCEDURE PR(VAR N:INTEGER); BEGIN N:=N+1 END PR;

BEGIN J:=0;
  FOR I:=1 TO 10 DO J:=J+1; PR(I) END;
  WriteString('DEVIATES...9.8-9',16)
END T9P8D9.
---------------

$
(* Deviance *)
MODULE T9P8D10;

  (* ПAPAMETP ЦИKЛA HE MOЖET БЫTЬ ПAPAMETPOM-ПEPEMEHHOЙ *)

FROM InOut IMPORT WriteString,WriteInt,WriteLn;

VAR i,j:INTEGER;

PROCEDURE PR(VAR n,k: INTEGER); BEGIN k:=n+1 END PR;

BEGIN j:=0;
  FOR i:=1 TO 10 DO
    PR(i,j); WriteInt(j,4); WriteLn
  END;
  WriteString('DEVIATES...9.8-9',16)
END T9P8D10.
----------------

$
(* Deviance *)
MODULE T9P8D11;

  (* ЯBHOE ИЗMEHEHИE ЗHAЧEHИЯ ПAPAMETPA ЦИKЛA BHУTPИ TEЛA *)

FROM InOut IMPORT WriteString,WriteInt,WriteLn;

VAR i,j: INTEGER;

BEGIN j:=0;
  FOR i:=1 TO 10 DO
    j:=j+1; i:=i+1;
    WriteInt(i,5);  WriteLn
  END;
  WriteString('DEVIATES...9.8-11',17)
END T9P8D11.
-----------------

$
(* Deviance *)
MODULE T9P8D12;

  (* B ЭTOM TECTE HAЧAЛЬHOE ЗHAЧEHИE И ГPAHИЦA ЦИKЛA
     HE COBMECTИMЫ C TИПOM ПAPAMETPA ЦИKЛA *)

FROM InOut IMPORT WriteString;

VAR V,W:INTEGER;

BEGIN
  FOR V:=1.0 TO 2.0 DO W:=V END;
  WriteString('DEVIATES...9.8-12',17)
END T9P8D12.
-----------------

$
(* Deviance *)
MODULE T9P8D13;

  (* ЗAПPEЩEHHЫЙ TИП ПAPAMETPA ЦИKЛA *)

FROM InOut IMPORT WriteString;

VAR W,V: REAL;

BEGIN
  FOR V:=0 TO 5.0 DO W:=V END;
  WriteString('DEVIATES...9.8-13',17)
END T9P8D13.
----------------

$
(* Deviance *)
MODULE T9P8D14;

  (* ЗAПPEЩEHHЫЙ TИП ПAPAMETPA ЦИKЛA *)

FROM InOut IMPORT WriteString;

VAR V,W:REAL;

BEGIN
  FOR V:=0.0 TO 5.0 BY 1.0 DO W:=V/2 END;
  WriteString('DEVIATES...9.8-14',17)
END T9P8D14.
------------------

$
(* Deviance *)
MODULE T9P8D15;

  (* УKAЗATEЛЬ HE MOЖET БЫTЬ ИCПOЛЬЗOBAH
     B KAЧECTBE ПAPAMETPA ЦИKЛA. *)

FROM Heap IMPORT ALLOCATE;
FROM InOut IMPORT WriteString;

TYPE INT=POINTER TO INTEGER;

VAR ptr: INT; j: INTEGER;

BEGIN
  ALLOCATE(ptr,SIZE(ptr^));
  FOR ptr^ :=0 TO 10 DO j:=j+1 END;
  WriteString('DEVIATES...9.8-15',17)
END T9P8D15.
-----------------

$
(* Deviance *)
MODULE T9P8D16;

  (* ПAPAMETP ЦИKЛA HE MOЖET ЯBЛЯTЬCЯ ПAPAMETPOM-ПEPEMEHHOЙ *)

FROM InOut IMPORT WriteString;

PROCEDURE P;
  VAR i: INTEGER;

  PROCEDURE P0(VAR i: INTEGER);
    VAR j: INTEGER;
  BEGIN j:=0;
    FOR i:=1 TO 10 DO j:=j+1 END
  END P0;

BEGIN i:=10; P0(i)
END P;

BEGIN
  P;
  WriteString('DEVIATES...9.8-16',17)
END T9P8D16.
-----------------

$cx
(* Conformance *)
MODULE T9P8D17;

  (* HAЧAЛЬHOE ЗHAЧEHИE И ГPAHИЦA ЦИKЛA ДOЛЖHЫ BЫЧИCЛЯTЬCЯ
     OДИH PAЗ, ДO HAЧAЛA BЫПOЛHEHИЯ ЦИKЛA. *)

FROM InOut IMPORT WriteString,WriteLn,WriteInt;

VAR i,j,k: INTEGER;
  F: ARRAY [0..7] OF INTEGER;

BEGIN
  F[0]:=0; F[1]:=1; F[2]:=2; F[3]:=3;
  F[4]:=4; F[5]:=5; F[6]:=6; F[7]:=7;
  j:=0;
  FOR i:=F[j] TO F[j+7] DO
    j:=j+1;
    WriteString(  'i=',2 ); WriteInt(i,1);
    WriteString('; j=',4 ); WriteInt(j,1);
    WriteLn
  END;
  IF j=8 THEN WriteString('Pass...9.8-17',13)
  ELSE        WriteString('FAIL...9.8-17',13)
  END
END T9P8D17.
---------------

$cx
(* Conformance *)
MODULE T9P8D18;

FROM InOut IMPORT WriteString,WriteLn,WriteInt;

VAR i,j: INTEGER;

BEGIN j:=1;
  FOR i:=1 TO i+1 DO INC(j); WriteInt(i,10); WriteLn END;
  IF j=3 THEN WriteString('Pass...9.8-18',17)
  ELSE        WriteString('FAIL...9.8-18',17)
  END;
END T9P8D18.
-----------------

$
(* Deviance *)
MODULE T9P8D19;

  (* BЛOЖEHHЫE ЦИKЛЫ C OДИHAKOBЫMИ ИДEHTИФИKATOPAMИ *)
  (* B KAЧECTBE ПAPAMETPOB ЦИKЛA ЗAПPEЩEHЫ.         *)

FROM InOut IMPORT WriteString;

VAR i,j: INTEGER;

BEGIN j:=0;
  FOR i:=1 TO 10 DO
    FOR i:=1 TO 10 DO j:=j+1 END
  END;
  WriteString('DEVIATES...9.8-19',17)
END T9P8D19.
---------------

$
(* Deviance *)
MODULE T9P8D20;

  (* ПEPEMEHHAЯ HE MOЖET ИCПOЛЬЗOBATЬCЯ B KAЧECTBE ШAГA ЦИKЛA *)

FROM InOut IMPORT WriteString;

VAR i,j:INTEGER;

BEGIN j:=0;
  FOR i:=1 TO 10 BY j DO j:=j+1 END;
  WriteString('DEVIATES...9.8-20',17)
END T9P8D20.
----------------

$cx
(* Conformance *)
MODULE T9P8D21;

  (* ПPOBEPKA PAБOTЫ ЦИKЛOB C ЛOГИЧECKИMИ ПAPAMETPAMИ *)

FROM InOut IMPORT WriteString;

VAR B,C,D:BOOLEAN;
    K,L,M:INTEGER;

BEGIN K:=0;M:=0;L:=0;
  FOR B:=FALSE TO TRUE  DO K:=K+1 END;
  FOR C:=TRUE  TO FALSE DO L:=L+1 END;
  FOR D:=TRUE  TO TRUE  DO M:=M+1 END;
  IF (K=2) AND (M=1) AND (L=0) THEN
    WriteString('Pass...9.8-21',13)
  ELSE
    WriteString('FAIL...9.8-21',13)
  END
END T9P8D21.
----------------

$cx
(*QUALITY*)
MODULE T9P8D23;

  (* TECT ПЫTAETCЯ OПPEДEЛИTЬ, CУЩECTBУET ЛИ KAKOЙ-HИБУДЬ *)
  (* OБOЗPИMЫЙ ПPEДEЛ BЛOЖEHHOCTИ ЦИKЛA C ПAPAMETPOM.     *)

FROM InOut IMPORT WriteString, WriteInt, WriteLn;

VAR j,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10 : INTEGER;

BEGIN j:=0;
  FOR I1:=1 TO 2 DO
    FOR I2:=1 TO 2 DO
      FOR I3:=1 TO 2 DO
        FOR I4:=1 TO 2 DO
          FOR I5:=1 TO 2 DO
            FOR I6:=1 TO 2 DO
              FOR I7:=1 TO 2 DO
                FOR I8:=1 TO 2 DO
                  FOR I9:=1 TO 2 DO
                    FOR I10:=1 TO 2 DO
                      INC(j);
                    END
                  END
                END
              END
            END
          END
        END
      END
    END
  END;
  WriteString('Допустимая глубина вложенности ',34);
  WriteString('FOR-оператора >= 10 ...9.8-23 ',35); WriteLn;
  IF j=1024 THEN WriteString('Pass...9.8-23',12)
  ELSE           WriteString('FAIL...9.8-23',12)
  END;
END T9P8D23.
------------------

$cx
(* Conformance *)
MODULE T9P9D1;

  (* ПPOГPAMMA ПPOBEPЯET ПPABИЛЬHOCTЬ PAБOTЫ БEЗУCЛOBHOГO ЦИKЛA *)

FROM InOut IMPORT WriteString, WriteInt, WriteLn;

VAR n,k: INTEGER;

PROCEDURE proc;
BEGIN
  LOOP n:=n+1;
    LOOP k:=k-1;
      IF (n+k=10) THEN EXIT END
    END;
    RETURN
  END;
  n:=0; k:=0
END proc;

BEGIN n:=1; k:=10;
  proc;
  IF (n=2) AND (k=8) THEN WriteString('Pass...9.9-1',12)
  ELSE
    WriteString('n=',2); WriteInt(n,5); WriteLn;
    WriteString('k=',2); WriteInt(k,5); WriteLn;
    WriteString('FAIL...9.9-1',12)
  END
END T9P9D1.
-----------------

$cx
(* Conformance *)
MODULE T9P9D2;

  (* БEЗУCЛOBHЫЙ ЦИKЛ C НЕПУCTOЙ ПOCЛEДOBATEЛЬHOCTЬЮ *)
  (* OПEPATOPOB И С OПEPATOPОМ BЫXOДA.               *)
  (* ИCПOЛHEHИE TAKOЙ ПPOГPAMMЫ В КАЙФ ПОЙДЕТ. (Leg) *)

FROM InOut IMPORT WriteString;

BEGIN
  LOOP EXIT END;
  WriteString(' В кайф пошло ...9.9-2',12)
END T9P9D2.
---------------------

$cx
(* Conformance *)
MODULE T9P10D1;

  (* ПPOBEPЯETCЯ PAБOTA WITH-OПEPATOPA *)

FROM InOut IMPORT WriteString;

TYPE
  rec1 = RECORD i,j,k: INTEGER END;
  rec2 = RECORD i,j:   INTEGER END;

VAR
  r1: rec1;
  r2: rec2;

BEGIN
  WITH r1 DO i:=0; j:=0; k:=0 END;
  WITH r2 DO i:=1; j:=1       END;
  IF (r1.i=0) & (r1.j=0) & (r2.i=1) & (r2.j=1) THEN
    WriteString('Pass...9.10-1',14)
  ELSE
    WriteString('FAIL...9.10-1',14)
  END
END T9P10D1.
------------------

$cx
(* Conformance *)
MODULE T9P10D2;

  (* ПPИBOДЯTCЯ ПPИMEPЫ ЗAПИCEЙ C OДИHAKOBЫMИ ИДEHTИФИKATOPAMИ *)
  (* ПOЛEЙ. TECT ДOЛЖEH ДEMOHCTPИPOBATЬ, ЧTO OПEPATOPЫ WITH    *)
  (* ПPABИЛЬHO OПPEДEЛЯЮT OБЛACTИ BИДИMOCTИ.                   *)

FROM InOut IMPORT WriteString;

TYPE
  T1 = RECORD a,b: INTEGER END;
  T2 = RECORD a: INTEGER; b: BOOLEAN END;

VAR r1: T1; r2: T2;

BEGIN
  WITH r1 DO a:=0; b:=0; r1.a:=1  END;
  WITH r2 DO a:=r1.a; b:=(r1.a=1) END;
  IF (r1.a=1) AND (r2.a=1) AND r2.b THEN
    WriteString('Pass...9.10-2',14)
  ELSE
    WriteString('FAIL...9.10-2',14)
  END
END T9P10D2.
--------------------

$cx
(* Conformance *)
MODULE T9P10D3;

  (* BHУTPИ OПEPATOPA WITH ДOCTУПHЫ BHEШHИE OБ'EKTЫ *)

FROM InOut IMPORT WriteString;

TYPE rec = RECORD
             a: ARRAY [1..3] OF CHAR;
             n: INTEGER
           END;

VAR r: rec; k: INTEGER;

BEGIN
  r.a[1]:='A'; k:=1;
  WITH r DO
    a[k]:='B';
    k:=2;
    a[k]:='A'
  END;
  IF (r.a[1]='B') AND (r.a[2]='A') AND (k=2) THEN
    WriteString('Pass...9.10-3',14)
  ELSE
    WriteString('FAIL...9.10-3',14)
  END
END T9P10D3.
------------------

$
(* Deviance *)
MODULE T9P10D4;

  (* В OБЛACTИ BИДИMOCTИ, OTKPЫTOЙ OПEPATOPOM WITH, ИДEHTИФИKATOP
     ПOЛЯ A EЩE HE БУДET ДOCTУПEH. *)

FROM InOut IMPORT WriteString;
  TYPE REC1=RECORD A:BOOLEAN END;
       REC2=RECORD R2:REC1 END;
  VAR R1:REC2;
BEGIN R1.R2.A:=TRUE;
      WITH R1 DO
           A:=FALSE
      END;
      IF R1.R2.A
         THEN WriteString('DEVIATES...9.10-4...CASE1',25)
         ELSE WriteString('DEVIATES...9.10-4...CASE2',25)
      END
END T9P10D4.
-----------------

$cx
(* Conformance *)
MODULE T9P10D5;

  (* TECT ПPOBEPЯET, ЧTO B ЗABИCИMOCTИ OT OTKPЫTOЙ
     B HACTOЯЩИЙ MOMEHT OБЛACTИ BИДИMOCTИ БУДУT ПPABИЛЬHO
     BЫБИPATЬCЯ ИДEHTИФИKATOPЫ ПOЛEЙ C OДИHAKOBЫMИ ИMEHAMИ *)

FROM InOut IMPORT WriteString;
  VAR A:RECORD B:RECORD X:INTEGER;
                        Y:CHAR
                 END;
               X:INTEGER
        END;
BEGIN
  WITH A DO
    B.X:=1; X:=0;
    WITH B DO
      X:=-X
    END
  END;
  IF (A.X=0) AND (A.B.X=-1)
    THEN WriteString('Pass...9.10-5',14)
    ELSE WriteString('FAIL...9.10-5',14)
  END
END T9P10D5.
----------------

$cx
(* Conformance *)
MODULE T9P10D6;

  (* TECT ПPOBEPЯET, ЧTO BHУTPИ OПEPATOPA WITH HEДOCTУПHЫ
     BHEШHИE OБ'EKTЫ, ИMEЮЩИE TO ЖE ИMЯ, ЧTO И OДHO ИЗ ПOЛEЙ
     PACCMATPИBAEMOЙ ЗAПИCИ. *)

FROM InOut IMPORT WriteString;
  VAR R:RECORD I,J:INTEGER END;
      I:INTEGER;
BEGIN I:=10;
      WITH R DO
        I:=5
      END;
      IF (R.I=5) AND (I=10)
         THEN WriteString('Pass...9.10-6',14)
         ELSE WriteString('FAIL...9.10-6',14)
      END
END T9P10D6.
-----------------

$
(* Deviance *)
MODULE T9P10D7;

  (* ИДEHTИФИKATOP ЗAПИCИ R2 HEДOCTУПEH TAM,
     ГДE EГO ПЫTAЮTCЯ ИCПOЛЬЗOBATЬ. *)

FROM InOut IMPORT WriteString;
  VAR R1:RECORD R2:RECORD A:INTEGER;
                          B:CHAR
                   END
         END;
BEGIN WITH R2 DO A:=1 END;
      WriteString('DEVIATES...9.10-7',17)
END T9P10D7.
-------------------

$cx
(* Conformance *)
MODULE T9P10D8;

  (* TECT ПPOBEPЯET, ЧTO CEЛEKTOP ИЗOБPAЖEHИЯ ПOЛЯ ЗAПИCИ
     BЫЧИCЛЯETCЯ TOЛЬKO OДИH PAЗ, ПEPEД ПOCЛEДOBATEЛЬHOCTЬЮ
     OПEPATOPOB. *)

FROM InOut IMPORT WriteString;
  VAR A:ARRAY [1..2] OF RECORD I,J:INTEGER END;
      K:INTEGER;
BEGIN A[2].I:=5;K:=1;
      WITH A[K] DO
           J:=1;K:=2;I:=2
      END;
      IF (A[1].I=2) AND (A[2].I=5)
         THEN WriteString('Pass...9.10-8',14)
         ELSE WriteString('FAIL...9.10-8',14)
      END
END T9P10D8.
-----------------

$
(* Deviance *)
MODULE T9P10D9;

  (* ПPOГPAMMA ИCПOЛЬЗУET ИДEHTИФИKATOP
     BHE EГO OБЛACTИ BИДИMOCTИ. *)

FROM InOut IMPORT WriteString;
  TYPE T=RECORD X:INTEGER;
                Y:CHAR
          END;
  VAR R1:RECORD R2:T;
                Z:INTEGER
         END;
BEGIN WITH R1 DO
        WITH R2 DO X:=1 END;
        Z:=X
      END;
      IF Z=1
      THEN WriteString('DEVIATES...9.10-9...CASE1',25)
      ELSE WriteString('DEVIATES...9.10-9...CASE2',25)
      END
END T9P10D9.
-----------------

$cx
(* Conformance *)
MODULE T9P10D10;

  (* ПPOBEPKA CEMAHTИKИ PAБOTЫ OПEPATOPA WITH *)

FROM Heap IMPORT ALLOCATE;
FROM InOut IMPORT WriteString;

TYPE
  ptr = POINTER TO rec;
  rec = RECORD data: INTEGER; link: ptr END;

VAR co: INTEGER; p,q: ptr;

BEGIN co:=0;
  ALLOCATE(p,SIZE(p^)); p^.data:=0; p^.link:=NIL;
  ALLOCATE(q,SIZE(q^)); q^.data:=1; q^.link:=NIL;
  p^.link:=q;
  q:=p;
  WITH q^ DO
    q:=link;
    IF (data=0) AND (q^.data=1) THEN INC(co) END;
  END;
  WITH p^ DO
    p:=link;
    IF (data=0) AND (p^.data=1) THEN INC(co) END;
  END;
  IF co=2 THEN WriteString('Pass...9.10-10',14)
  ELSE         WriteString('FAIL...9.10-10',14)
  END
END T9P10D10.
-----------------

$cx
(*QUALITY*)
MODULE T9P10D11;

  (* ПPOГPAMMA ПЫTAETCЯ УCTAHOBИTЬ, CУЩECTBУET ЛИ
     KAKOЙ-HИБУДЬ ПPEДEЛ BЛOЖEHHOCTИ OПEPATOPA WITH.
     TECT COДEPЖИT 15 BЛOЖEHHЫX  WITH - OПEPATOPOB. *)

FROM InOut   IMPORT WriteString, WriteLn;
  TYPE Rec01=RECORD I:INTEGER END;
       Rec02=RECORD I:INTEGER END;
       Rec03=RECORD I:INTEGER END;
       Rec04=RECORD I:INTEGER END;
       Rec05=RECORD I:INTEGER END;
       Rec06=RECORD I:INTEGER END;
       Rec07=RECORD I:INTEGER END;
       Rec08=RECORD I:INTEGER END;
       Rec09=RECORD I:INTEGER END;
       Rec10=RECORD I:INTEGER END;
       Rec11=RECORD I:INTEGER END;
       Rec12=RECORD I:INTEGER END;
       Rec13=RECORD I:INTEGER END;
       Rec14=RECORD I:INTEGER END;
       Rec15=RECORD I:INTEGER END;
  VAR ptr01: Rec01;
      ptr02: Rec02;
      ptr03: Rec03;
      ptr04: Rec04;
      ptr05: Rec05;
      ptr06: Rec06;
      ptr07: Rec07;
      ptr08: Rec08;
      ptr09: Rec09;
      ptr10: Rec10;
      ptr11: Rec11;
      ptr12: Rec12;
      ptr13: Rec13;
      ptr14: Rec14;
      ptr15: Rec15;

BEGIN ptr15.I:=0;
      WITH ptr01  DO
        WITH ptr02  DO
          WITH ptr03  DO
            WITH ptr04  DO
              WITH ptr05  DO
                WITH ptr06  DO
                  WITH ptr07  DO
                    WITH ptr08  DO
                      WITH ptr09  DO
                        WITH ptr10  DO
                          WITH ptr11  DO
                            WITH ptr12  DO
                              WITH ptr13  DO
                                WITH ptr14  DO
                                  WITH ptr15  DO
                                       I:=5
                                  END
                                END
                              END
                            END
                          END
                        END
                      END
                    END
                  END
                END
              END
            END
          END
        END
      END;
      WriteString('15 BЛOЖEHHЫX WITH-OПEPATOPOB',30);
      WriteLn;
  IF ptr15.I=5
    THEN WriteString('Pass...9.10-11',30); WriteLn
    ELSE WriteString('FAIL...9.10-11',30); WriteLn
  END;

END T9P10D11.
-----------------

$cx
(* Conformance *)
MODULE T9P11D1;

  (* ПPOГPAMMA ИCПOЛЬЗУET OПEPATOP BOЗBPATA. *)

FROM InOut IMPORT WriteString;

VAR x: INTEGER;

PROCEDURE proc1;
  VAR a,b:INTEGER;
BEGIN
  a:=0; b:=0; RETURN
END proc1;

PROCEDURE proc2(VAR x: INTEGER);
BEGIN
  IF x=0 THEN RETURN
  ELSE x:=x*2
  END;
  x:=1
END proc2;

BEGIN
  proc1; x:=0; proc2(x);
  IF x=0 THEN WriteString('Pass...9.11-1',13)
  ELSE        WriteString('FAIL...9.11-1',13)
  END
END T9P11D1.
---------------------

$
(* Deviance *)
MODULE T9P11D2;

  (* BЫPAЖEHИE, CTOЯЩEE ПOCЛE RETURN, HECOBMECTИMO
     C TИПOM PEЗУЛЬTATA ПPOЦEДУPЫ - ФУHKЦИИ . *)

FROM InOut IMPORT WriteString;

PROCEDURE func(x:INTEGER): BOOLEAN;
  VAR b: BOOLEAN;
BEGIN
  IF    x>0 THEN RETURN TRUE
  ELSIF x<0 THEN RETURN FALSE
  ELSE RETURN 0
  END
END func;

BEGIN
  WriteString('DEVIATES...9.11-2',17)
END T9P11D2.
--------------------

$c
(* Error Handling *)
MODULE T9P11D3;

  (* ПOПЫTKA BOЗBPATИTЬ ЗHAЧEHИE, TИП KOTOPOГO
     OTЛИЧEH OT TИПA PEЗУЛЬTATA ПPOЦEДУPЫ-ФУHKЦИИ *)

FROM InOut IMPORT WriteString;

TYPE
  color = (red,yellow,green);
  T     = [red..yellow];

VAR y: color;

PROCEDURE func(x:INTEGER): T;
BEGIN
  IF x#0 THEN y:=red ELSE y:=green END;
  RETURN y
END func;

BEGIN
  y:=func(0);
  WriteString('ERROR IS NOT DETECTED...9.11-3',30)
END T9P11D3.
------------------

$c
(* Deviance *)
MODULE T9P11D4;

  (* ПPOЦEДУPA-ФУHKЦИЯ OБЯЗATEЛЬHO ДOЛЖHA ИMETЬ OПEPATOP *)
  (* BOЗBPATA. B ДAHHOM TECTE OПИCЫBAETCЯ ФУHKЦИЯ,       *)
  (* HE COДEPЖAЩAЯ OПEPATOP BOЗBPATA.                    *)

FROM InOut IMPORT WriteString;

PROCEDURE func(x: INTEGER): BOOLEAN;
  VAR b: BOOLEAN;
BEGIN
  IF x>0 THEN b:=TRUE ELSE b:=FALSE END
END func;

BEGIN
  IF func(0) THEN END;
  WriteString('DEVIATES...9.11-4',17)
END T9P11D4.
-------------------

$c
(* Error handling *)
MODULE T9P11D6;

  (* ЗHAЧEHИE ФУHKЦИИ B TOЧKE 0 HE OПPEДEЛEHO. *)

FROM InOut IMPORT WriteString;

VAR b: BOOLEAN;

PROCEDURE func(x: INTEGER): BOOLEAN;
BEGIN
  IF x>0 THEN RETURN TRUE END
END func;

BEGIN
  b:=func(0);
  WriteString('ERROR IS NOT DETECTED...9.11-6',30)
END T9P11D6.
------------------

$cx
(* Conformance *)
MODULE T9P11D7;

  (* ПPOBEPЯETCЯ ПPABИЛЬHOCTЬ PAБOTЫ OПEPATOPA EXIT. *)

FROM InOut IMPORT WriteString;

PROCEDURE F(x: INTEGER): BOOLEAN;
BEGIN
  LOOP x:=x-1;
    IF x=0 THEN EXIT; RETURN FALSE END;
    RETURN FALSE
  END;
  RETURN TRUE
END F;

BEGIN
  IF F(1) THEN WriteString('Pass...9.11-7',13)
  ELSE         WriteString('FAIL...9.11-7',13)
  END
END T9P11D7.
