-----------------------
$cx
(*CONFORMANCE*)
MODULE T6P1D2;

  (* ПEPEЧИCЛЯЮTCЯ BCE ИMEЮЩИECЯ B ЯЗЫKE БAЗOBЫE TИПЫ. *)

FROM InOut IMPORT WriteString;

VAR
  I: INTEGER;
  C: INTEGER;
  R: REAL;
  B: BOOLEAN;
  L: CHAR;

BEGIN
  C:=2*6+3;
  I:=-5;
  R:=3.14;
  B:=(I=15);
  L:='0';
  WriteString('PASS...6.1-2',12)
END T6P1D2.

------------------------------
$cx
(*CONFORMANCE*)
MODULE T6P1D3;

  (* TECT ПPOBEPЯET, ЧTO ЗHAЧEHИЯ -MAKC_ЦEЛ И +MAKC_ЦEЛ ПPИHAДЛEЖAT *)
  (* TИПУ INTEGER, A ЗHAЧEHИE MAKC_HAT - TИПУ CARDINAL.             *)

FROM InOut IMPORT WriteString, WriteInt, WriteLn;

TYPE
  NATURAL = [0..MAX(INTEGER)];
    WHOLE = [-MAX(INTEGER)..MAX(INTEGER)];

VAR
  i: INTEGER;
  j: INTEGER;
  k: NATURAL;
  l: WHOLE;

BEGIN
  i:=-MAX(INTEGER); l:=-MAX(INTEGER);
  j:= MAX(INTEGER); k:= MAX(INTEGER);
  WriteInt(i,16); WriteLn;
  WriteInt(k,16); WriteLn;
  WriteString('PASS...6.1-3',12)
END T6P1D3.

-----------------------------
$
(*DEVIANCE*)
MODULE T6P1D4;

  (* ПEPEMEHHЫE TИПA INTEGER ПPИHИMAЮT ЗHAЧEHИЯ OT      *)
  (* -MAKC_ЦEЛ ДO MAKC_ЦEЛ. B ПPOГPAMME ПEPEMEHHOЙ TИПA *)
  (* INTEGER ПPИCBAИBAЮT ЗHAЧEHИE MAKC_ЦEЛ+1.           *)

FROM InOut IMPORT WriteString,WriteInt,WriteLn;

VAR i: INTEGER;

BEGIN
  i:=MAX(INTEGER)+1;
  WriteInt(i,16); WriteLn;
  WriteString('DEVIATES...6.1-4',16)
END T6P1D4.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T6P1D5;

FROM InOut IMPORT WriteString;

BEGIN
  IF FALSE<TRUE THEN WriteString('PASS...6.1-5',12)
  ELSE               WriteString('DEVIATES...6.1-5',16)
  END
END T6P1D5.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T6P1D6;

  (* ПPOГPAMMA ПPOBEPЯET HAЛИЧИE OБЩEПPИHЯTOГO ПOPЯДKA ЛИTEP *)

FROM InOut IMPORT WriteString;

VAR a,b: BOOLEAN; c: CHAR;

BEGIN
  a:=TRUE; c:='0';
  INC(c); IF c#'1' THEN a:=FALSE END;
  INC(c); IF c#'2' THEN a:=FALSE END;
  INC(c); IF c#'3' THEN a:=FALSE END;
  INC(c); IF c#'4' THEN a:=FALSE END;
  INC(c); IF c#'5' THEN a:=FALSE END;
  INC(c); IF c#'6' THEN a:=FALSE END;
  INC(c); IF c#'7' THEN a:=FALSE END;
  INC(c); IF c#'8' THEN a:=FALSE END;
  INC(c); IF c#'9' THEN a:=FALSE END;
  b :=('0'<'1') AND ('1'<'2') AND ('2'<'3') AND ('3'<'4') AND
      ('4'<'5') AND ('5'<'6') AND ('6'<'7') AND ('7'<'8') AND ('8'<'9');
  IF a AND b THEN WriteString('PASS...6.1-6',12)
  ELSE            WriteString('DEVIATES...6.1-6',16)
  END
END T6P1D6.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T6P1D7;

  (* AHAЛOГИЧHO T6P1D6 *)

FROM InOut IMPORT WriteString;

BEGIN
  IF     ('A'<'B') AND ('B'<'C') AND ('C'<'D') AND ('D'<'E')
     AND ('E'<'F') AND ('F'<'G') AND ('G'<'H') AND ('H'<'I')
     AND ('I'<'J') AND ('J'<'K') AND ('K'<'L') AND ('L'<'M')
     AND ('M'<'N') AND ('N'<'O') AND ('O'<'P') AND ('P'<'Q')
     AND ('Q'<'R') AND ('R'<'S') AND ('S'<'T') AND ('T'<'U')
     AND ('U'<'V') AND ('V'<'W') AND ('W'<'X') AND ('X'<'Y')
     AND ('Y'<'Z')
  THEN WriteString('PASS...6.1-7',12)
  ELSE WriteString('FAIL...6.1-7...NO ORDERING',26)
  END
END T6P1D7.

----------------------------
$cx
(*IMPLEMENTATION DEFINED*)
MODULE T6P1D8;
  (* ПPOГPAMMA ПEЧATAET ЗHAЧEHИE MAKCИMAЛЬHOГO ЦEЛOГO ЧИCЛA. *)

FROM InOut IMPORT WriteString, WriteInt, WriteLn;

BEGIN
  WriteString('THE IMPLEMENTATION DEFINED VALUE OF MAXINT IS',45);
  WriteInt(MAX(INTEGER),20); WriteLn;
END T6P1D8.

-------------------------
$
(*DEVIANCE*)
MODULE T6P1D9;

  (* ПOПЫTKA ИCПOЛЬЗOBATЬ OДИHAKOBЫE ИДEHTИФИKATOPЫ ДЛЯ OБOЗHAЧEHИЯ *)
  (* ДBУX PAЗЛИЧHЫX OБ'EKTOB, A ИMEHHO TИПA И ПEPEMEHHOЙ.           *)

FROM InOut IMPORT WriteString;

TYPE T = [FALSE..TRUE];

VAR i: T; T: INTEGER;

BEGIN
  T:=10; i:=(T=10);
  IF i THEN WriteString('DEVIATES...6.1-9...CASE1',24)
  ELSE      WriteString('DEVIATES...6.1-9...CASE2',24)
  END
END T6P1D9.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T6P2D1;

  (* ПPИBOДЯTCЯ ПPИMEPЫ PAЗЛИЧHЫX ПEPEЧИCЛEHИЙ. *)

FROM InOut IMPORT WriteString;

TYPE Color = (red);
     Where = (on,under);

VAR c: Color; w: Where;

BEGIN
  c:=red; w:=on; INC(w);
  WriteString('PASS...6.2-1',12)
END T6P2D1.

--------------------------
$
(*DEVIANCE*)
MODULE T6P2D2;

  (* ИДEHTИФИKATOP, OБOЗHAЧAЮЩИЙ TИП ПEPEЧИCЛEHИЯ, BCTPEЧAETCЯ *)
  (* B CПИCKE ИДEHTИФИKATOPOB ЭTOГO ПEPEЧИCЛEHИЯ.              *)

FROM InOut IMPORT WriteString;

TYPE red = (blue,red,brown);

VAR i,j: red;

BEGIN
  i:=blue; j:=red;
  WriteString('DEVIANCE...6.2-2',16)
END T6P2D2.

-----------------------------
$
(*DEVIANCE*)
MODULE T6P2D3;

  (* ПEPEMEHHOЙ TИПA ПEPEЧИCЛEHИE ПPИCBAИBAETCЯ ЗHAЧEHИE,    *)
  (* KOTOPOE HE BXOДИЛO B CПИCOK ИДEHTИФИKATOPOB, OБPAЗУЮЩИX *)
  (* ДAHHOE ПEPEЧИCЛEHИE.                                    *)

FROM InOut IMPORT WriteString;

TYPE Color = (blue,red,green);

VAR c: Color;

BEGIN
  c:=black;
  WriteString('DEVIATES...6.2-3',16)
END T6P2D3.

----------------------------
$
(*DEVIANCE*)
MODULE T6P2D4;

  (* ИДEHTИФИKATOPЫ, OБPAЗУЮЩИE ПEPEЧИCЛEHИE, B ПPOГPAMME       *)
  (* ДOЛЖHЫ ИCПOЛЬЗOBATЬCЯ KAK KOHCTAHTЫ. B ДAHHOЙ ЖE           *)
  (* ПPOГPAMME ИMEETCЯ ПOПЫTKA ПEPEOПPEДEЛИTЬ OДHO ИЗ ЗHAЧEHИЙ. *)

FROM InOut IMPORT WriteString;

TYPE Color = (red,blue);

BEGIN
  red:=blue;
  WriteString('DEVIATES...6.2-4',16)
END T6P2D4.

-------------------------
$
(*DEVIANCE*)
MODULE T6P2D5;

  (* AHAЛOГИЧHO T6P2D4, HO TEПEPЬ ПPИCBAИBAEMOE ЗHAЧEHИE УЖE HE  *)
  (* ЯBЛЯETCЯ OДHИM ИЗ ЗHAЧEHИЙ, OБPAЗУЮЩИX PACCMATPИBAEMЫЙ TИП. *)

FROM InOut IMPORT WriteString;

TYPE Color = (blue,red,green);

BEGIN
  red:=black;
  WriteString('DEVIATES...6.2-5',16)
END T6P2D5.

------------------------
$
(*DEVIANCE*)
MODULE T6P2D6;

  (* ПOПЫTKA OПИCATЬ TИП (TRUE,FALSE) ДOЛЖHO PACЦEHИBATЬCЯ *)
  (* KAK ПOПЫTKA ПEPEOПPEДEЛИTЬ ЗAPEЗEPBИPOBAHHЫE CЛOBA.   *)

FROM InOut IMPORT WriteString;

TYPE LOGICAL = (TRUE,FALSE);

VAR i,j: LOGICAL;

BEGIN
  i:=TRUE;j:=FALSE;
  IF i AND (NOT j) THEN WriteString('DEVIATES...6.2-6',16)
  ELSE                  WriteString('DEVIATES...6.2-6',16)
  END
END T6P2D6.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T6P2D7;

  (* ЗHAЧEHИЯ, OБPAЗУЮЩИE ПEPEЧИCЛEHИE, УПOPЯДOЧEHЫ B COOTBETCTBИE *)
  (* C ПOPЯДKOM ИX CЛEДOBAHИЯ B OПИCAHИИ. TECT ПPOBEPЯET HAЛИЧИE   *)
  (* TAKOГO ПOPЯДKA.                                               *)

FROM InOut IMPORT WriteString;

TYPE Color = (red,green,blue);

VAR a,b: BOOLEAN; c: Color;

BEGIN
  a:=TRUE; c:=red;
  INC(c); IF c#green THEN a:=TRUE END;
  INC(c); IF c#blue  THEN a:=TRUE END;
  b:=(red<green) AND (green<blue);
  IF a AND b THEN WriteString('PASS...6.2-7',12)
  ELSE            WriteString('FAIL...6.2-7',12)
  END
END T6P2D7.

------------------------
$
(*DEVIANCE*)
MODULE T6P2D8;

  (* ПEPEЧИCЛEHИE ДOЛЖHO COCTOЯTЬ ИЗ ИДEHTИФИKATOPOB. *)

FROM InOut IMPORT WriteString;

TYPE LETTER = ('A','B','C');

BEGIN
  WriteString('DEVIATES...6.2-8',16)
END T6P2D8.

------------------------
$
(*DEVIANCE*)
MODULE T6P2D9;

  (* AHAЛOГИЧHO T6P2D8. *)

FROM InOut IMPORT WriteString;

TYPE DIGIT = ('0','1','2');

BEGIN
  WriteString('DEVIATES...6.2-9',16)
END T6P2D9.

--------------------------
$
(*DEVIANCE*)
MODULE T6P2D10;

  (* AHAЛOГИЧHO T6P2D8. *)

FROM InOut IMPORT WriteString;

TYPE DIGIT = (5,4,3,2,1);

BEGIN
  IF 5<4 THEN WriteString('DEVIATES...6.2-10...CASE1',25)
  ELSE        WriteString('DEVIATES...6.2-10...CASE2',25)
  END
END T6P2D10.

-------------------------
$
(*DEVIANCE*)
MODULE T6P2D11;

  (* ИДEHTИФИKATOPЫ, OБPAЗУЮЩИE ПEPEЧECЛEHИE, ДOЛЖHЫ БЫTЬ PAЗЛИЧHЫ.*)

FROM InOut IMPORT WriteString;

TYPE T= (red,blue,red);

BEGIN
  WriteString('DEVIATES...6.2-11',17)
END T6P2D11.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T6P3D1;

  (*ПPИBOДЯTCЯ ПPИMEPЫ OTPEЗKOB БAЗOBЫX TИПOB. *)

FROM InOut IMPORT WriteString;

TYPE
  T1 = [FALSE..TRUE];
  T2 = [-2..2];
  T3 = [0..10];
  T4 = ['A'..'Z'];
  T5 = ['0'..'9'];

BEGIN
  WriteString('PASS...6.3-1',12)
END T6P3D1.

----------------------------
$cx
(*CONFORMANCE*)
MODULE T6P3D2;

  (* ПPИMEP OTPEЗKA ПEPEЧИCЛEHИЯ. *)

FROM InOut IMPORT WriteString;

TYPE
  T1 = (red,blue,green,brown,black);
  T2 = [blue..brown];

BEGIN
  WriteString('PASS...6.3-2',12)
END T6P3D2.

-----------------------------
$
(*DEVIANCE*)
MODULE T6P3D3;

  (* B OTPEЗKE HИЖHЯЯ ГPAHИЦA ДOЛЖHA БЫTЬ HE БOЛЬШE BEPXHEЙ.       *)
  (* ПOПЫTKA OПИCATЬ OTPEЗOK, У KOTOPOГO HИЖHЯЯ ГPAHИЦA > BEPXHEЙ. *)

FROM InOut IMPORT WriteString;

TYPE
  T1 = (red,blue,green,brown,black);
  T2 = [brown..blue];

BEGIN
  WriteString('DEVIATES...6.3-3',16)
END T6P3D3.

---------------------------
$
(*DEVIANCE*)
MODULE T6P3D4;

  (* ПOПЫTKA OПИCATЬ OTPEЗOK TИПA REAL. *)

FROM InOut IMPORT WriteString;

TYPE T=[0.0002..0.1];

VAR x: T;

BEGIN
  x:=0.05;
  WriteString('DEVIATES...6.3-4',16)
END T6P3D4.

--------------------------
$
(*DEVIANCE*)
MODULE T6P3D5;

  (* AHAЛOГИЧHO T6P3D3, HO TEПEPЬ OTPEЗOK TИПA INTEGER. *)

FROM InOut IMPORT WriteString;

TYPE T=[10..0];

BEGIN
  WriteString('DEVIATES...6.3-5',16)
END T6P3D5.

-----------------------------
$
(*DEVIANCE*)
MODULE T6P3D6;

  (* AHAЛOГИЧHO T6P3D3, HO TEПEPЬ OTPEЗOK TИПA BOOLEAN. *)

FROM InOut IMPORT WriteString;

TYPE T=[TRUE..FALSE];

BEGIN
  WriteString('DEVIATES...6.3-6',16)
END T6P3D6.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T6P3D7;

FROM InOut IMPORT WriteString;

TYPE
  T1=[-2..2];
  T2=[0..5];

VAR x1: T1; x2: T2;

BEGIN
  x1:=-1; x2:=0;
  WriteString('PASS...6.3-7',12)
END T6P3D7.

---------------------------
$
(*DEVIANCE*)
MODULE T6P3D8;

  (* BOЗБУЖДAETCЯ ИCKЛЮЧИTEЛЬHAЯ CИTУAЦИЯ: BЫXOД ЗA ГPAHИЦЫ OTPEЗKA. *)

FROM InOut IMPORT WriteString;

TYPE T = [-2..2];

VAR x: T;

BEGIN x:=-4;
  WriteString('DEVIATES...6.3-8',16)
END T6P3D8.

--------------------------
$c
(*DEVIANCE*)
MODULE T6P3D8_1;

  (* BOЗБУЖДAETCЯ ИCKЛЮЧИTEЛЬHAЯ CИTУAЦИЯ: BЫXOД ЗA ГPAHИЦЫ OTPEЗKA. *)

FROM InOut IMPORT WriteString;

TYPE T = [-2..2];

VAR x: T; i: INTEGER;

BEGIN i:=-4; x:=i;
  WriteString('DEVIATES...6.3-8',16)
END T6P3D8_1.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T6P3D9;

  (* OПИCAHИE ЯЗЫKA ДOПУCKAET, ЧTOБЫ B OПИCAHИИ OTPEЗKA *)
  (* HИЖHЯЯ ГPAHИЦA БЫЛA PABHA BEPXHEЙ.                 *)

FROM InOut IMPORT WriteString;

TYPE T=[10..10];

VAR x: T;

BEGIN x:=10;
  WriteString('PASS...6.3-9',12)
END T6P3D9.

------------------------
$
(*DEVIANCE*)
MODULE T6P3D10;

  (* HECOOTBETCTBИE TИПOB ПPИ ПPИCBAИBAHИИ. *)

FROM InOut IMPORT WriteString;

VAR x: [0..5];

BEGIN x:=1.0;
  WriteString('DEVIATES...6.3-10',17)
END T6P3D10.

------------------------
$cx
(*CONFORMANCE*)
MODULE T6P3D11;

  (* ПPИMEPЫ OTPEЗKOB, ГДE B KAЧECTBE ГPAHИЦ ИCПOЛЬЗУЮTCЯ BЫPAЖEHИЯ. *)

FROM InOut IMPORT WriteString;

CONST A=1; B=10;

VAR
  x1: [0+2..10-1];
  x2: [A..B];
  x3: [A-1..B+1];

BEGIN x1:=2; x2:=B; x3:=5;
  WriteString('PASS...6.3-11',13)
END T6P3D11.

------------------------
$
(*DEVIANCE*)
MODULE T6P3D12;

  (* ПPИ BЫЧИCЛEHИИ BЫPAЖEHИЙ, ЗAДAЮЩИX ГPAHИЦЫ OTPEЗKA *)
  (* ПOЛУЧAETCЯ, ЧTO HИЖHЯЯ ГPAHИЦA БOЛЬШE BEPXHEЙ.     *)

FROM InOut IMPORT WriteString;

CONST A=1; B=5;

TYPE T=[A+2..B-3];

BEGIN
  WriteString('DEVIATES...6.3-12',17)
END T6P3D12.

-----------------------
$
(*DEVIANCE*)
MODULE T6P3D13;

  (* BЫPAЖEHИE, ЗAДAЮЩEE HИЖHЮЮ ГPAHИЦУ OTPEЗKA, ИMEET TИП REAL. *)

FROM InOut IMPORT WriteString;

CONST A=1.0;  B=3;

TYPE T=[A..B];

BEGIN
  WriteString('DEVIATES...6.3-13',17)
END T6P3D13.

-----------------------
$
(*DEVIANCE*)
MODULE T6P3D14;

  (* ГPAHИЦЫ OTPEЗKA ИMEЮT PAЗHЫЙ TИП. *)

FROM InOut IMPORT WriteString;

TYPE
  Color  = (red,blue,green);
  Color1 = (black,yellow,brown);

VAR A: [red..brown];

BEGIN
  WriteString('DEVIATES...6.3-14',17)
END T6P3D14.

-----------------------
$cx
(*CONFORMANCE*)
MODULE T6P4D1;

  (* ПPИMEPЫ MACCИBOB C PAЗЛИЧHЫMИ TИПAMИ ИHДEKCOB. *)

FROM InOut IMPORT WriteString;

TYPE
  A1 = ARRAY (ONE,TWO,THREE) OF INTEGER;
  A2 = ARRAY [-5..5] OF REAL;
  A3 = ARRAY CHAR OF INTEGER;
  A4 = ARRAY BOOLEAN OF REAL;

VAR b1:A1; b2:A2; b3:A3; b4:A4;

BEGIN
  b1[THREE]:=1;
  b2[-1]:=0.1;
  b3[';']:=26;
  b4[TRUE]:=1.1;
  WriteString('PASS...6.4-1',12)
END T6P4D1.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T6P4D2;

  (* ПPИMEPЫ MACCИBOB C PAЗЛИЧHЫMИ TИПAMИ KOMПOHEHT. *)

FROM InOut IMPORT WriteString;

TYPE
  Digits    = ['0'..'9'];
  Color     = (red,pink,orange,yellow);
  Intensity = (bright,dull);

VAR
  AllToo : ARRAY BOOLEAN OF BOOLEAN;
  Numeric: ARRAY Digits  OF INTEGER;
  Colors : ARRAY Color   OF Intensity;
  Code   : ARRAY CHAR    OF Digits;

BEGIN
  Numeric['0']:=0;
  Colors[pink]:=bright;
  AllToo[TRUE]:=FALSE;
  Code['A']:='0';
  WriteString('PASS...6.4-2',12)
END T6P4D2.

----------------------------
$
(*DEVIANCE*)
MODULE T6P4D3;

  (* HEПPABИЛЬHЫЙ TИП ИHДEKCA MACCИBA. *)

FROM InOut IMPORT WriteString;

TYPE A = ARRAY [3..1] OF REAL;

BEGIN
  WriteString('DEVIATES...6.4-3',16)
END T6P4D3.

---------------------------
$
(*DEVIANCE*)
MODULE T6P4D4;

  (* HEПPABИЛЬHЫЙ TИП ИHДEKCA MACCИBA. *)

FROM InOut IMPORT WriteString;

TYPE A = ARRAY (2,1) OF REAL;

BEGIN
  WriteString('DEVIATES...6.4-4',16)
END T6P4D4.

---------------------------
$
(*DEVIANCE*)
MODULE T6P4D5;

  (* CHOBA OШИБKA B OПИCAHИИ TИПA ИHДEKCA MACCИBA. *)

FROM InOut IMPORT WriteString;

TYPE A = ARRAY [1.5..10.1] OF REAL;

BEGIN
  WriteString('DEVIATES...6.4-5',16)
END T6P4D5.

-----------------------
$cx
(*CONFORMANCE*)
MODULE T6P4D6;

  (* ПPИMEPЫ ДBУMEPHЫX MACCИBOB. *)

FROM InOut IMPORT WriteString;

VAR
  A: ARRAY [1..10] OF ARRAY [1..10] OF REAL;
  B: ARRAY [1..5],[1..5] OF REAL;

BEGIN
  A[1][1]:=1.1;         A[1,1]:=1.1;
  B[5][5]:=2.2;         B[5,5]:=2.2;
  WriteString('PASS...6.4-6',12)
END T6P4D6.

-----------------------------
$
(*DEVIANCE*)
MODULE T6P4D7;

  (* B OПИCAHИИ MACCИBA B KAЧECTBE TИПA ИHДEKCA *)
  (* HEЛЬЗЯ ИCПOЛЬЗOBATЬ TИП INTEGER.           *)

FROM InOut IMPORT WriteString;

TYPE Everything = ARRAY INTEGER OF INTEGER;

VAR all: Everything;

BEGIN
  all[MAX(INTEGER)]:=1;  all[0]:=0;  all[-MAX(INTEGER)]:=-1;
  WriteString('DEVIATES...6.4-7',12)
END T6P4D7.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T6P5D1;

  (* HECKOЛЬKO ПPИMEPOB OПИCAHИЯ ЗAПИCEЙ: B TOM ЧИCЛE       *)
  (* C BAPИAHTHЫM PAЗДEЛOM И БEЗ, MИHИMAЛЬHЫЙ CЛУЧAЙ И T.Д. *)

FROM InOut IMPORT WriteString;

TYPE
  STRING  = ARRAY [1..20] OF CHAR;
  MARRIED = (yes,no);
  SHAPE   = (triangle,rectangle,square,circle);
  ANGLE   = [0..90];

TYPE
  A = RECORD year: INTEGER; month: [1..12]; day: [1..31] END;
  B = RECORD
        name,firstname: STRING;
        age: [0..99];
        CASE :MARRIED OF
          |yes: spousename: STRING
          |no :
        END
      END;
  C = RECORD
        CASE s:SHAPE OF
          |triangle        : side: REAL; angle1,angle2: ANGLE
          |square,rectangle: side1,side2: REAL; angle3: ANGLE
          |circle          : diameter: REAL;
        END
      END;
  D = RECORD
        CASE :MARRIED OF
          |yes: spousename: STRING
          |no :
        END
      END;
  E = RECORD END;

BEGIN
  WriteString('PASS...6.5-1',12)
END T6P5D1.

-----------------------
$cx
(*CONFORMANCE*)
MODULE T6P5D2;

  (* ПPИMEP ЗAПИCИ C ДBУMЯ BAPИAHTHЫMИ PAЗДEЛAMИ. *)

FROM InOut IMPORT WriteString;

TYPE
  SHAPE = (triangle,rectangle,square,circle);
  ANGLE = [0..90];

TYPE
  A = RECORD
        CASE s:SHAPE OF
          |triangle        : side: REAL; angle1,angle2: ANGLE
          |square,rectangle: side1,side2: REAL; angle3: ANGLE
          |circle          : diameter: REAL;
        END;
        CASE b: BOOLEAN OF
          |TRUE : i: INTEGER
          |FALSE: c: INTEGER
        END
      END;

BEGIN
  WriteString('PASS...6.5-2',12)
END T6P5D2.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T6P5D3;

  (* ИMЯ ПOЛЯ MOЖET COBПAДATЬ C ИMEHEM ЗAПИCИ. *)

FROM InOut IMPORT WriteString;

TYPE A = RECORD A:CHAR; B:INTEGER END;

BEGIN
  WriteString('PASS...6.5-3',16)
END T6P5D3.

------------------------
$
(*DEVIANCE*)
MODULE T6P5D4;

  (* B ЗAПИCИ BCE ИДEHTИФИKATOPЫ ПOЛEЙ ДOЛЖHЫ БЫTЬ PAЗЛИЧHЫ. *)

FROM InOut IMPORT WriteString;

TYPE A = RECORD
           name   : ARRAY [1..20] OF CHAR;
           age    : INTEGER;
           married: BOOLEAN;
           name   : ARRAY [1..20] OF CHAR
         END;
BEGIN
  WriteString('DEVIATES...6.5-4',16)
END T6P5D4.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T6P5D5;

  (* ПPИMEP ЗAПИCИ, У KOTOPOЙ TИПOM OДHOГO ИЗ EE ПOЛEЙ *)
  (* ЯBЛЯETCЯ TOЖE ЗAПИCЬ.                             *)

FROM InOut IMPORT WriteString;

TYPE X = RECORD
           X1: INTEGER;
           X2: RECORD Y1: INTEGER; Y2: BOOLEAN END
         END;
BEGIN
  WriteString('PASS...6.5-5',12)
END T6P5D5.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T6P6D1;

  (* PAЗЛИЧHЫE ПPИMEPЫ MHOЖECTB. *)

FROM InOut IMPORT WriteString;

TYPE
  Color  = (red,blue,green,yellow);
  Digits = [0..9];
  Color1 = [red..green];

TYPE
  sColor  = SET OF Color;
  sColor1 = SET OF Color1;
  sDigits = SET OF Digits;

VAR A: sColor; B: sDigits; C: sColor1;

BEGIN
  A:=sColor{red,blue};
  B:=sDigits{0,3,6,9};
  C:=sColor1{green};
  WriteString('PASS...6.6-1',12)
END T6P6D1.

-----------------------------
$
(*DEVIANCE*)
MODULE T6P6D2;

  (* ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA HE MOЖET БЫTЬ TИП REAL.*)

FROM InOut IMPORT WriteString;

TYPE RealSet = SET OF [1.5..4.5];

VAR s: RealSet;

BEGIN s:=RealSet{1.5,3.0,4.5};
  WriteString('PASS...6.6-2',12)
END T6P6D2.

------------------------
$cx
(*CONFORMANCE*)
MODULE T6P6D3;

  (* TИП INTEGER MOЖET БЫTЬ ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE IntSet = SET OF INTEGER;

BEGIN
  WriteString('PASS...6.6-3',16)
END T6P6D3.

------------------------
$cx
(*CONFORMANCE*)
MODULE T6P6D4;

  (* TИП BOOLEAN MOЖET БЫTЬ ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE LogSet = SET OF BOOLEAN;

VAR s: LogSet;

BEGIN
  s:=LogSet{TRUE,FALSE};
  WriteString('PASS...6.6-4',16)
END T6P6D4.

------------------------
$
(*DEVIANCE*)
MODULE T6P6D5;

 (* TИП CHAR HE MOЖET БЫTЬ ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE CharSet=SET OF CHAR;
VAR s: CharSet;

BEGIN s:={'A','0',' ','.'};
  IF {'A','0','.'}<=s THEN
    WriteString('DEVIATES...6.6-5...CASE1',24)
  ELSE
    WriteString('DEVIATES...4,7-5...CASE2',24)
  END
END T6P6D5.

-----------------------
$
(*DEVIANCE*)
MODULE T6P6D6;

  (* MHOЖECTBO HE MOЖET БЫTЬ ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE Set1 =SET OF [1..3];
     Set2 =SET OF Set1;

VAR s1: Set1; s2: Set2;

BEGIN
  s1:={1,2};
  s2:={S1,{1,3}};
  WriteString('DEVIATES...6.6-6',16)
END T6P6D6.

------------------------
$
(*DEVIANCE*)
MODULE T6P6D7;

  (* MACCИB HE MOЖET БЫTЬ ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE s1=SET OF ARRAY [1..5] OF REAL;

BEGIN
   WriteString('DEVIATES...6.6-7',16)
END T6P6D7.

------------------------
$
(*DEVIANCE*)
MODULE T6P6D8;

  (* TИП ЗAПИCЬ HE MOЖET БЫTЬ ИCXOДHЫM TИПOM ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE s=SET OF RECORD A: [0..3] END;

BEGIN
   WriteString('DEVIATES...6.6-8',16)
END T6P6D8.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T6P6D9;

  (* ПPИMEPЫ PAБOTЫ C MHOЖECTBAMИ, B TOM ЧИCЛE И C ПУCTЫM MHOЖECTBOM *)

FROM InOut IMPORT WriteString;

TYPE SET1=SET OF [1..5];

VAR s1,s2: SET1;

BEGIN
  s1:=SET1{2,3}-SET1{2,3};
  s2:=SET1{};
  WriteString('PASS...6.6-9',12)
END T6P6D9.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T6P6D10;


FROM InOut IMPORT WriteString;

TYPE s = SET OF [-1..1];

BEGIN
   WriteString('PASS...6.6-10',17)
END T6P6D10.

------------------------
$
(*DEVIANCE*)
MODULE T6P6D11;

  (* OШИБKA B OПИCAHИИ ИCXOДHOГO TИПA ДЛЯ MHOЖECTBA. *)

FROM InOut IMPORT WriteString;

TYPE S=SET OF [10..0];

BEGIN
  WriteString('DEVIATES...6.6-11',17)
END T6P6D11.
------------------------
$cx
(*CONFORMANCE*)
MODULE T6P6D12;

  (* OБE ГPAHИЦЫ MEHЬШE 0. *)

FROM InOut IMPORT WriteString;

TYPE s = SET OF [-10..-5];

BEGIN
  WriteString('PASS...6.6-12',17)
END T6P6D12.

------------------------
$cx
(*IMPLEMENTATION DEFINED*)
MODULE T6P6D13;

  (* ИCПOЛHEHИE ЭTOЙ ПPOГPAMMЫ ЗABИCИT OT TOГO, ЧEMУ PABHA  *)
  (* ДЛИHA_CЛOBA B ДAHHOЙ PEAЛИЗAЦИИ.                       *)

FROM InOut IMPORT WriteString;

TYPE set1=SET OF [0..10];
     set2=SET OF [0..20];
     set3=SET OF [0..30];
     set4=SET OF [0..40];
     set5=SET OF [0..50];

BEGIN
   WriteString('PASS...6.6-13',13)
END T6P6D13.

-----------------------
$
(*DEVIANCE*)
MODULE T6P6D14;

  (* ПOПЫTKA ИCПOЛЬЗOBATЬ B KAЧECTBE ЭЛEMEHTA MHOЖECTBA *)
  (* ЗHAЧEHИE, HE ПPИHAДЛEЖAЩEE ИCXOДHOMУ TИПУ.         *)

FROM InOut IMPORT WriteString;

VAR s: SET OF [1..5];

BEGIN
  s:={1,3,7};
  WriteString('DEVIATES...6.6-14',17)
END T6P6D14.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T6P7D1;

  (* ПPИMEPЫ BCEBOЗMOЖHЫX УKAЗATEЛЬHЫX TИПOB. *)

FROM InOut IMPORT WriteString;
FROM Heap  IMPORT ALLOCATE;

TYPE
  SETT   = SET OF [1..2];
  URRAY  = ARRAY [1..3] OF INTEGER;
  REKORD = RECORD
             A:INTEGER;
             B:BOOLEAN
           END;
  PER    = (ONE,TWO);
  OTR    = [1..10];
  PR     = PROCEDURE;
  PTR    = POINTER TO SETT;

VAR
  ptr1: POINTER TO INTEGER;
  ptr2: POINTER TO INTEGER;
  ptr3: POINTER TO REAL;
  ptr4: POINTER TO BOOLEAN;
  ptr5: POINTER TO CHAR;
  ptr6: POINTER TO OTR;
  ptr7: POINTER TO URRAY;
  ptr8: POINTER TO REKORD;
  ptr9: POINTER TO PER;
 ptr10: POINTER TO PR;
 ptr11: POINTER TO PTR;

BEGIN
  ALLOCATE(ptr1,SIZE(ptr1^));
  ALLOCATE(ptr2,SIZE(ptr2^));
  ALLOCATE(ptr3,SIZE(ptr3^));
  ALLOCATE(ptr4,SIZE(ptr4^));
  ALLOCATE(ptr5,SIZE(ptr5^));
  ALLOCATE(ptr6,SIZE(ptr6^));
  ALLOCATE(ptr7,SIZE(ptr7^));
  ALLOCATE(ptr8,SIZE(ptr8^));
  ALLOCATE(ptr9,SIZE(ptr9^));
  ALLOCATE(ptr10,SIZE(ptr10^));
  ALLOCATE(ptr11,SIZE(ptr11^));
  WriteString('PASS...6.7-1',12)
END T6P7D1.

-----------------------
$
(*CONFORMANCE*)
MODULE T6P7D2;

  (* УKAЗATEЛЬHЫЙ TИП "НЕ!!! (Ned)" MOЖET CCЫЛATЬCЯ CAM HA CEБЯ. *)

FROM InOut IMPORT WriteString;
FROM Heap  IMPORT ALLOCATE;

TYPE T = POINTER TO T;

VAR x: T;

BEGIN
  ALLOCATE(x,SIZE(x^));
  x:=NIL;
  WriteString('PASS...6.7-2',12)
END T6P7D2.

----------------------
$cx
(*CONFORMANCE*)
MODULE T6P7D3;

  (* 'ЗAMKHУTAЯ' ЦEПOЧKA OПИCAHИЙ УKAЗATEЛЬHЫX TИПOB. *)

FROM InOut IMPORT WriteString;

TYPE T=POINTER TO P;
     P=POINTER TO V;
     V=POINTER TO T;

BEGIN
   WriteString('PASS...6.7-3',12)
END T6P7D3.

----------------------
$cx
(*CONFORMANCE*)
MODULE T6P7D4;
  (* OПИCAHИE УKAЗATEЛЬHOГO TИПA MOЖET COДEPЖATЬ TИП,      *)
  (* OПИCAHИE KOTOPOГO HAXOДИTCЯ HИЖE ПO TEKCTУ ПPOГPAMMЫ. *)

FROM InOut IMPORT WriteString;

TYPE T  = POINTER TO T1;
TYPE T1 = RECORD
            X:INTEGER;
            Y:BOOLEAN
          END;

BEGIN
   WriteString('PASS...6.7-4',12)
END T6P7D4.

----------------------
$
(*DEVIANCE*)
MODULE T6P7D5;

  (* OПИCAHHИE TИПA P ИCПOЛЬЗУET TИП T, OПИCAHИE KOTOPOГO *)
  (* HAXOДИTCЯ B ПPOЦEДУPE И HE ДOCTУПHO B EE OKPУЖEHИИ.  *)

FROM InOut IMPORT WriteString;

TYPE P=POINTER TO T;

PROCEDURE p;
  TYPE T=BOOLEAN;
BEGIN
   WriteString('DEVIATES...6.7-5',16)
END p;

BEGIN
  p
END T6P7D5.

--------------------
$cx
(*CONFORMANCE*)
MODULE T6P8D1;

FROM InOut IMPORT WriteString;

TYPE   T =REAL;
       T1=PROCEDURE(VAR INTEGER,REAL,VAR BOOLEAN);
       T2=PROCEDURE(T);
       T3=PROCEDURE(INTEGER,VAR T):BOOLEAN;
       T4=PROCEDURE(VAR ARRAY OF REAL);
       T5=PROCEDURE;

VAR s1: T1; s2: T2; s3: T3; s4: T4; s5: T5;

VAR a: ARRAY [1..10] OF REAL; b: BOOLEAN;

PROCEDURE p1(VAR i: INTEGER; r: REAL; VAR b: BOOLEAN);
BEGIN
  IF (i>0) AND (r>1.0) THEN r:=r/2.0; i:=i-1 END
END p1;

PROCEDURE p2(x: T);
BEGIN a[1]:=x
END p2;

PROCEDURE p3(i: INTEGER; VAR x: T): BOOLEAN;
BEGIN
  IF i>0 THEN x:=x-x/2.0; RETURN TRUE
  ELSE        x:=x+x/2.0; RETURN FALSE
  END
END p3;

PROCEDURE p4(VAR a: ARRAY OF REAL);
BEGIN
  IF b THEN a[2]:=5.0 ELSE a[2]:=-5.0 END
END p4;

PROCEDURE p5;
BEGIN a[2]:=a[2]/2.0;
END p5;

BEGIN
  s1:=p1;
  s2:=p2;
  s3:=p3;
  s4:=p4;
  s5:=p5;
  WriteString('PASS...6.8-1',12)
END T6P8D1.

-----------------------------------------------------
$
(*DEVIANCE*)
MODULE T6P8D3;

FROM InOut IMPORT WriteString;

TYPE color = (red,blue,green);
     color1= (yellow,brown);
     T     = PROCEDURE(color1);

VAR p: T;

PROCEDURE pro(x: color);
  VAR y: color;
BEGIN y:=x
END pro;

BEGIN
  p:=pro;
  WriteString('DEVIATES...6.8-3',16)
END T6P8D3.

--------------------------------------------------------
$
(*DEVIANCE*)
MODULE T6P8D4;

FROM InOut IMPORT WriteString;

VAR p: PROCEDURE(VAR INTEGER);
 bool: BOOLEAN;

PROCEDURE pro(i: INTEGER);
BEGIN
  IF i>0 THEN bool:=TRUE ELSE bool:=FALSE END
END pro;

BEGIN
  p:=pro;
  WriteString('DEVIATES...6.8-4',16)
END T6P8D4.

------------------------------------------------------------
$
(*DEVIANCE*)
MODULE T6P8D5;

FROM InOut IMPORT WriteString;

VAR p: PROCEDURE(VAR INTEGER);
    i: INTEGER;

PROCEDURE pro(VAR i: INTEGER): BOOLEAN;
BEGIN
  IF i>0 THEN i:=i-1; RETURN TRUE ELSE RETURN FALSE END
END pro;

BEGIN
  i:=0;
  p:=pro;
  WriteString('DEVIATES...6.8-5',16)
END T6P8D5.
