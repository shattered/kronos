$
(*DEVIANCE*)
MODULE T3P1D1c;

  (* ИДEHTИФИKATOP ДOЛЖEH HAЧИHATЬCЯ C БУKBЫ *)

FROM IO IMPORT WriteString;

VAR 1IDEN: INTEGER;

BEGIN
  WriteString('DEVIATES...3.1-1')
END T3P1D1c.

----------------------------------
$cx
(*QUALITY*)
MODULE T3P1D2;

  (* TECT ПPOBEPЯET, БУДУT ЛИ PAЗЛИЧATЬCЯ ИДEHTИФИKATOPЫ *)

FROM IO IMPORT WriteString;

CONST
  ValueOfVeryLongIdentifier1=10;

PROCEDURE P;
  VAR ValueOfVeryLongIdentifier2: INTEGER;
BEGIN
  ValueOfVeryLongIdentifier2:=11;
  IF ValueOfVeryLongIdentifier1 <> ValueOfVeryLongIdentifier2 THEN
    WriteString('Identifiers distinguished...3.1-2')
  ELSE
    WriteString('Identifiers NOT distinguished...3.1-2')
  END
END P;

BEGIN
  P
END T3P1D2.

----------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P1D3;

  (* OПИCAHИE ЯЗЫKA HE HAKЛAДЫBAET OГPAHИЧEHИЙ HA ДЛИHУ  *)
  (* ИДEHTИФИKATOPA. ПPOГPAMMA ИCПOЛЬЗУET ИДEHTИФИKATOPЫ *)
  (* ДЛИHOЙ ДO 70 CИMBOЛOB.                              *)

FROM IO IMPORT WriteString;

CONST
  I10IIIIIII=10;
  I20IIIIIIIIIIIIIIIII=20;
  I30IIIIIIIIIIIIIIIIIIIIIIIIIII=30;
  I40IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=40;
  I50IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=50;
  I60IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=60;
  I70IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=70;

BEGIN
  IF I10IIIIIII+I20IIIIIIIIIIIIIIIII+
     I30IIIIIIIIIIIIIIIIIIIIIIIIIII+
     I40IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII+
     I50IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII+
     I60IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII+
     I70IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
     <> 280
  THEN
    WriteString('FAIL ... 1.1-3')
  ELSE
    WriteString('PASS ... 1.1-3')
  END
END T3P1D3.

----------------
$cx
(*QUALITY*)
MODULE T3P1D4;

  (* ЭTOT TECT ПЫTAETCЯ OБHAPУЖИTЬ ПPEДEЛ ЗHAЧИMOCTИ CИMBOЛOB     *)
  (* B ИДEHTИФИKATOPE. ECЛИ OH БУДET OБHAPУЖEH, TO BЫДAETCЯ       *)
  (* COOБЩEHИE, ЧEMУ OH PABEH.                                    *)

FROM IO IMPORT WriteString, WriteInt, WriteLn;

CONST
  I5III=5;
  I10IIIIIII=5;
  I15IIIIIIIIIIII=5;
  I20IIIIIIIIIIIIIIIII=5;
  I25IIIIIIIIIIIIIIIIIIIIII=5;
  I30IIIIIIIIIIIIIIIIIIIIIIIIIII=5;
  I35IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=5;
  I40IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=5;
  I45IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=5;
  I50IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII=5;

PROCEDURE SIGNIFICANCE;
  CONST
    I5IIJ=0;
    I10IIIIIIJ=0;
    I15IIIIIIIIIIIJ=0;
    I20IIIIIIIIIIIIIIIIJ=0;
    I25IIIIIIIIIIIIIIIIIIIIIJ=0;
    I30IIIIIIIIIIIIIIIIIIIIIIIIIIJ=0;
    I35IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIJ=0;
    I40IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIJ=0;
    I45IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIJ=0;
    I50IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIJ=0;
  VAR X:INTEGER;

BEGIN
  X:=I5III+I10IIIIIII+I15IIIIIIIIIIII+
     I20IIIIIIIIIIIIIIIII+I25IIIIIIIIIIIIIIIIIIIIII+
     I30IIIIIIIIIIIIIIIIIIIIIIIIIII+
     I35IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII+
     I40IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII+
     I45IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII+
     I50IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII;
  IF X=50 THEN
    WriteString('Max identifier lenght >= 50 ')
  ELSE
    WriteInt(X,4); X:=X+5;
    WriteString('<= Max identifier lenght <=');
    WriteInt(X,4)
  END;
  WriteLn;
  WriteString(' ...3.1-4...QUALITY')
END SIGNIFICANCE;

BEGIN
 SIGNIFICANCE
END T3P1D4.

-----------------------------------
$cx
(*CONFORIANCE*)
MODULE T3P2D1;

   (* TECT ПPOBEPЯET COOTBETCTBИE TPAHCЛЯTOPA CИHTAKCИCУ *)
   (* ПPEДCTABЛEHИЯ ЦEЛЫX ЧИCEЛ.                         *)

FROM IO IMPORT WriteString,WriteInt,WriteLn;

CONST
  A=1;
  B=123;
  C=0123;
  D=1B;
  E=123B;
  F=1H;
  G=123H;
  H=12AH;

BEGIN
  WriteInt(A,1); WriteLn;
  WriteInt(B,3); WriteLn;
  WriteInt(C,4); WriteLn;
  WriteInt(D,2); WriteLn;
  WriteInt(E,4); WriteLn;
  WriteInt(F,2); WriteLn;
  WriteInt(G,4); WriteLn;
  WriteInt(H,4); WriteLn;
  WriteString('PASS...3.2-1');
END T3P2D1.

-------------------------------------
$
(*DEVIANCE*)
MODULE T3P2D2c;

   (*ПPИMEP HEПPABИЛЬHOГO BOCMEPИЧHOГO ЧИCЛA. *)

FROM IO IMPORT WriteString;

CONST A=9B;
VAR   B:INTEGER;

BEGIN
  B:=9B;
  WriteString('DEVIATES...3.2-2')
END T3P2D2c.

--------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P2D3;

  (* TECT ПPOBEPЯET COOTBETCTBИE TPAHCЛЯTOPA CИHTAKCИCУ *)
  (* ПPEДCTABЛEHИЯ BEЩECTBEHHЫX ЧИCEЛ (БEЗ ПOPЯДKA).    *)

FROM IO IMPORT WriteString,WriteFix,WriteLn;

CONST
  A=123.123;
  B=123.0123;
  C=123.;
BEGIN
  WriteFix(A,6,3);WriteLn;
  WriteFix(B,7,4);WriteLn;
  WriteFix(C,3,0);WriteLn;
  WriteString('PASS...3.2-3')
END T3P2D3.

--------------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P2D4;

  (* TECT ПPOBEPЯET COOTBETCTBИE TPAHCЛЯTOPA CИHTAKCИCУ *)
  (* ПPEДCTABЛEHИЯ BEЩECTBEHHЫX ЧИCEЛ C ПOPЯДKOM.       *)

FROM IO IMPORT WriteString;

CONST
  A=123.123E+2;
  B=123.123E-2;
  C=123.123E2;
  D=123.E+2;
  E=12.3E0;
  F=123.0E+2;
BEGIN
  WriteString('PASS...3.2-4')
END T3P2D4.

-------------------------------------
$
(*DEVIANCE*)
MODULE T3P2D5c;

  (* B ЗAПИCИ BEЩECTBEHHOГO ЧИCЛA TOЧKE ДOЛЖHA ПPEДШECTBOBATЬ *)
  (* ПOCЛEДOBATEЛЬHOCTЬ ЦИФP. ECЛИ ЭTOT TECT 'ПPOXOДИT', TO   *)
  (* TPAHCЛЯTOP OTKЛOHЯETCЯ OT OПИCAHИЯ ЯЗЫKA.                *)

FROM IO IMPORT WriteString;

CONST j=.123;
VAR i:REAL;

BEGIN i:=0.123;
  IF i=j THEN WriteString('DEVIATES...3.2-5') END
END T3P2D5c.

---------------------------------
$
(*DEVIANCE*)
MODULE T3P2D6c;

  (* AHAЛOГИЧHO T3P2D5, HO TEПEPЬ BEЩECTBEHHЫE ЧИCЛA C ПOPЯДKOM. *)

FROM IO IMPORT WriteString;

CONST j=.123E+2;
VAR i:REAL;

BEGIN i:=0.123E+2;
  IF i=j THEN WriteString('DEVIATES...3.2-6') END
END T3P2D6c.

---------------------------------
$
(*DEVIANCE*)
MODULE T3P2D7c;

  (* ДECЯTИЧHЫЙ ПOPЯДOK MOГУT ИMETЬ TOЛЬKO BEЩECTBEHHЫE ЧИCЛA. *)

FROM IO IMPORT WriteString;

CONST j=123E+2;
VAR i: REAL;

BEGIN
  i:=123.0E+2;
  IF i=j THEN WriteString('DEVIATES...3.2-7') END
END T3P2D7c.

-----------------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P2D8;

  (* ЭTA ПPOГPAMMA ПPOBEPЯET, PAЗPEШAЮTCЯ ЛИ TPAHCЛЯTOPOM *)
  (* OЧEHЬ ДЛИHHЫE ЧИCЛA.                                 *)

FROM IO IMPORT WriteString, WriteFix, WriteLn;

CONST
  REEL = 123.456789012345678901234567890123456789;
BEGIN
  WriteFix(REEL,39,36); WriteLn;
  WriteString('PASS...3.2-8')
END T3P2D8.

--------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D1;

  (* ПO OПИCAHИЮ ЯЗЫKA, TEKCT, COCTOЯЩИЙ ИЗ OДHOЙ ЛИTEPЫ, ИMEET TИП CHAR. *)

FROM IO IMPORT WriteString;

CONST One='1';
      Two='2';

VAR TwoToo: CHAR;

BEGIN
  IF (One<>Two) AND (Two='2') THEN
    TwoToo:='2';
    IF TwoToo=Two THEN
      WriteString('PASS...3.3-1')
    ELSE
      WriteString('FAIL...3.3-1')
    END
  ELSE
    WriteString('FAIL...3.3-1')
  END
END T3P3D1.

-------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D2;

(* OПИCAHИE ЯЗЫKA HE HAKЛAДЫBAET OГPAHИЧEHИЙ HA ДЛИHУ TEKCTA. *)
(* TECT ПPOBEPЯET, PAЗPEШAЮTCЯ ЛИ TEKCTЫ ДЛИHOЙ ДO 66 ЛИTEP.  *)

FROM IO IMPORT WriteString;

TYPE
  String1 = ARRAY [0..65] OF CHAR;
  String2 = ARRAY [0..32] OF CHAR;

VAR
  ALPHA: String1;
  BETA : String2;

BEGIN
  ALPHA:='ABCDEFABCDEFABCDEFABCDEFABCDEFABCDEFABCDEFABCDEFABCDEFABCDEFABCDEF';
  BETA :='IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII';
  WriteString('PASS...3.3-2')
END T3P3D2.

------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D3;

  (* B KAЧECTBE KABЫЧEK ДЛЯ TEKCTOB MOЖHO ИCПOЛЬЗOBATЬ *)
  (* KAK ДBOЙHЫE KABЫЧKИ, TAK И AПOCTPOФ.              *)

FROM IO IMPORT WriteString;

CONST str1='ABCD';
VAR   str2: ARRAY [0..4] OF CHAR;

BEGIN
  str2:="ABCD";
  IF str1=str2 THEN WriteString("PASS...3.3-3") END
END T3P3D3.

----------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D4;

  (* TECT ПPOBEPЯET, ЧTO AПOCTPOФ MOЖET BCTPEЧATЬCЯ BHУTPИ *)
  (* TEKCTA, ЗAKЛЮЧEHHOГO B ДBOЙHЫE KABЫЧKИ, И HAOБOPOT.   *)

FROM IO IMPORT WriteString,WriteLn;

VAR String1,String2: ARRAY [0..4] OF CHAR;

BEGIN
  String1:="CAN'T"; WriteString(String1); WriteLn;
  String2:='CAN"T'; WriteString(String2); WriteLn;
  WriteString('PASS...3.3-4')
END T3P3D4.

--------------------------------
$
(*DEVIANCE*)
MODULE T3P3D5c;

   (* BHУTPИ TEKCTA HE ДOЛЖEH BCTPEЧATЬCЯ CИMBOЛ, OTKPЫBAЮЩИЙ *)
   (* И ЗAKPЫBAЮЩИЙ ДAHHЫЙ TEKCT.                             *)

FROM IO IMPORT WriteString;

CONST str1='A'A';
      str2="A"A";

BEGIN
  WriteString('DEVIATES...3.3-5')
END T3P3D5c.

----------------------------
$
(*DEVIANCE*)
MODULE T3P3D6c;

  (* TEKCT ДOЛЖEH OTKPЫBATЬCЯ И ЗAKPЫBATЬCЯ OДHИM И *)
  (* TEM ЖE CИMBOЛOM KABЫЧEK.                       *)

FROM IO IMPORT WriteString,WriteLn;

CONST str="ABCDEFG';
BEGIN
  WriteString(str); WriteLn;
  WriteString('ERROR IS NOT DETECTED...3.3-6')
END T3P3D6c.
@
---------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D7;
  (* TEKCT ДЛИHЫ N1 MOЖET БЫTЬ ПPИCBOEH ПEPEMEHHOЙ-TEKCTУ *)
  (* ДЛИHЫ N2, ECЛИ N2>N1.                                *)

FROM IO IMPORT WriteString,WriteLn;

VAR String1: ARRAY [0..9] OF CHAR;
    String2: ARRAY [0..5] OF CHAR;
BEGIN
  String2:='ABCDEF';
  String1:=String2;
  String2:='MMM';
  WriteString(String1); WriteLn;
  WriteString(String2);  WriteLn;
  WriteString('PASS...3.3-7')
END T3P3D7.

--------------------------------
$
(*DEVIANCE*)
MODULE T3P3D8c;

  (* TEKCT ДЛИHЫ N1 HEЛЬЗЯ ПPИCBOИTЬ ПEPEMEHHOЙ-TEKCTУ *)
  (* ДЛИHЫ N2, ECЛИ N2<N1.                             *)

FROM IO IMPORT WriteString,WriteLn;

VAR String1: ARRAY [0..9] OF CHAR;
    String2: ARRAY [0..5] OF CHAR;

BEGIN
  String1:='0123456789';
  String2:=String1;
  WriteString(String2); WriteLn;
  WriteString('DEVIATES...3.3-8')
END T3P3D8c.

------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D10;

  (* ПOCKOЛЬKУ TEKCTЫ OПPEДEЛЯЮTCЯ KAK MACCИBЫ, TO MOЖHO *)
  (* OБPAЩATЬCЯ K ИX KOMПOHEHTAM.                        *)

FROM IO IMPORT WriteString, WriteLn;

VAR str: ARRAY [0..9] OF CHAR;

BEGIN
  str:='1111111111';
  str[5]:='0';
  WriteString(str); WriteLn;
  WriteString('PASS...3.3-10')
END T3P3D10.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T3P3D13;

  (* CИHTAKCИCOM ЯЗЫKA PAЗPEШAЮTCЯ ПУCTЫE TEKCTЫ. *)

FROM IO IMPORT WriteString,WriteLn;

BEGIN
  WriteString(''); WriteLn;
  WriteString('PASS...3.3-13')
END T3P3D13.

---------------------------
$
(*DEVIANCE*)
MODULE T3P3D14;

  (* HECOBMECTИMOCTЬ TИПOB ПPИ ПPИCBAИBAHИИ. *)

FROM IO IMPORT WriteString;

CONST cStr='A';

VAR vStr: ARRAY [0..0] OF CHAR;
    char: CHAR;

BEGIN
  char:=cStr;
  vStr:=char;
  vStr:='A';
  WriteString('DEVIATES...3.3-14')
END T3P3D14.

-------------------------------
$
(*DEVIANCE*)
MODULE T3P4D1c;

  (* ЗAPEЗEPBИPOBAHHЫE CЛOBA ЗAПPEЩEHO ИCПOЛЬЗOBATЬ *)
  (* B KAЧECTBE ИДEHTИФИKATOPOB.                    *)

FROM IO IMPORT WriteString;

CONST ARRAY=10;

BEGIN
  WriteString('DEVIATES...3.4-1')
END T3P4D1c.

----------------------------
$
(*DEVIANCE*)
MODULE T3P4D2c;

  (* AHAЛOГИЧHO T3P4D1, HO ДPУГOЙ KOHTEKCT. *)

FROM IO IMPORT WriteString;

VAR T: (VAR,CAR);

BEGIN
  T:=VAR;
  WriteString('DEVIATES...3.4-2')
END T3P4D2c.

-------------------------------
$cx
(*CONFORMANCE*)
MODULE T3P4D3;

  (* BHУTPИ TEKCTA MOГУT BCTPEЧATЬCЯ ЗAPEЗEPBИPOBAHHЫE CЛOBA. *)

FROM IO IMPORT WriteString;

CONST Reserved='ARRAY';

BEGIN
  WriteString('PASS...3.4-3')
END T3P4D3.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T3P4D4;

  (* TECT ПPOBEPЯET, ЧTO ИCПOЛЬЗУEMЫE B ПPOГPAMME ИДEHTИФИKATOPЫ *)
  (* TPAHCЛЯTOP OTЛИЧAET OT ЗAPEЗEPBИPOBAHHЫX CЛOB.              *)

FROM IO IMPORT WriteString;

VAR
  IMPLEMENTATIONX,IMPLEMENTATIOM,IMPLEMENTATIO:INTEGER;
  PROCEDUREX,PROCEDURM,PROCEDUR:CHAR;
  IFF:BOOLEAN;
BEGIN
  IMPLEMENTATIONX:=0;IMPLEMENTATIOM:=1;IMPLEMENTATIO:=2;
  PROCEDUREX:='0';PROCEDURM:='1';PROCEDUR:='2';
  IFF:=TRUE;
  WriteString('PASS...3.4-4')
END T3P4D4.

-------------------------------
$
(*DEVIANCE*)
MODULE T3P4D5;

  (* TO ЖE, ЧTO И T3P4D1-D2, HO B ДPУГOM KOHTEKCTE. *)

FROM IO IMPORT WriteString;

PROCEDURE WHILE;
BEGIN WriteString('DEVIATES...3.4-5')
END WHILE;

BEGIN
  WHILE
END T3P4D5.
@
------------------------
$
(*DEVIANCE*)
MODULE T3P5D1;

  (* ПPOБEЛЫ BHУTPИ ЧИCEЛ ЗAПPEЩEHЫ. *)

FROM IO IMPORT WriteString;

CONST A=1 234;
BEGIN
  WriteString('DEVIATES...3.5-1')
END T3P5D1.

------------------------
$
(*DEVIANCE*)
MODULE T3P5D2;

  (* ПPOБEЛЫ BHУTPИ ЧИCEЛ ЗAПPEЩEHЫ. *)

FROM IO IMPORT WriteString;

CONST A=1.2 E+2;

BEGIN
  WriteString('DEVIATES...3.5-2')
END T3P5D2.

------------------------
$
(*DEVIANCE*)
MODULE T3P5D3c;

  (* ПPOБEЛЫ BHУTPИ ЧИCEЛ ЗAПPEЩEHЫ. *)

FROM IO IMPORT WriteString;

CONST A=1. 23;
BEGIN
  WriteString('DEVIATES...3.5-3')
END T3P5D3c.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T3P5D4;

  (* BHУTPИ TEKCTOB ПPOБEЛЫ PAЗPEШAЮTCЯ. *)

FROM IO IMPORT WriteString,WriteLn;

CONST str1='AA AA';
      str2=' AAAA';
      str3='AAAA ';

BEGIN
  WriteString(str1); WriteLn;
  WriteString(str2); WriteLn;
  WriteString(str3); WriteLn;
  WriteString('PASS...3.5-4')
END T3P5D4.

---------------------
$cx
(*CONFORMANCE*)
MODULE (*IS THIS PERMITED TO BE HERE*)T3P6D1(*OR HERE*);

  (* MEЖДУ ЛЮБЫMИ ЛEKCИЧECKИMИ CИMBOЛAMИ ПPOГPAMMЫ MOЖHO *)
  (* BCTAЛЯTЬ KOMMEHTAPИИ.                               *)

FROM IO IMPORT WriteString;

VAR I(*CONTROL VARIABLE*):(*COLON*)INTEGER(*TYPE*);

BEGIN
  FOR (*THIS IS A FOR LOOP*)I(*CONTROL VARIABLE*):=(*ASSINGMENT*)
    1(*INITIAL VALUE*) TO (*UNTIL*)1 (*LAST VALUE*)
    DO(*GO*) WriteString(*WRITE STATEMENT*)('PASS...3.6-1')
  END
END T3P6D1.

---------------------------------
$
(*DEVIANCE*)
MODULE T3P6D2c;

  (* HEЛЬЗЯ BCTABЛЯTЬ KOMMEHTAPИИ BHУTPЬ ЛEKCИЧECKOЙ EДИHИЦЫ. *)

FROM IO IMPORT WriteString;

BEGIN
  Write(*COMMENT*)String('DEVIATES...3.6-2',16)
END T3P6D2c.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T3P6D3;

  (* KOMMEHTAPИИ MOГУT БЫTЬ BЛOЖEHHЫMИ. *)

FROM IO IMPORT WriteString;
BEGIN
(*WriteString('RAN')
(*WriteString('RAN1')*)
  WriteString('FAIL...3.6-3');*)
  WriteString('PASS...3.6-3')
END T3P6D3.

---------------------------
$
(*DEVIANCE*)
MODULE T3P6D4c;

  (* BHУTPИ KOMMEHTAPИЯ HE MOЖET BCTPEЧATЬCЯ "(*". *)

FROM IO IMPORT WriteString;

BEGIN
  (*IS A (* PERMITED IN A COMMENT *)
  WriteString('DEVIATES...3.6-4')
END T3P6D4c.
@
-------------------------
$
(*QUALITY*)
MODULE T3P6D5c;

FROM IO IMPORT WriteString,WriteInt;

  VAR i: INTEGER;

BEGIN i:=10;
(*Now write out the value of -i-.
  WriteString('DEVIATES...3.6-4 The value of -i- is');
  WriteInt(i,2);
(*The value of -i- will not be printed
  because of the unclosed previos comment. *)
END T3P6D5c.
@
------------------
$cx
(*CONFORMANCE*)
MODULE T4D1;

  (* BHEШHИЙ OБ'EKT, ИMEЮЩИЙ OДИHAKOBOE ИMЯ C ЛOKAЛЬHЫM *)
  (* OБ'EKTOM, HEДOCTУПEH B ПPOЦEДУPE. TECT ПPOBEPЯET   *)
  (* BЫПOЛHEHИE ЭTOГO ПPABИЛA                           *)

FROM IO IMPORT WriteString;

CONST a=10;

TYPE T=BOOLEAN;

VAR b,c: T;

PROCEDURE Func(i: INTEGER): BOOLEAN;
  VAR b:BOOLEAN;
BEGIN
  IF i>0 THEN b:=TRUE ELSE b:=FALSE END;
  RETURN b
END Func;

BEGIN
  b:=FALSE; c:=Func(a);
  IF b#c THEN WriteString('PASS...4-1')
  ELSE        WriteString('FAIL...4-1')
  END
END T4D1.

----------------
$
(*DEVIANCE*)
MODULE T4D2;

  (* KAЖДЫЙ ИДEHTИФИKATOP, BCTPEЧAЮЩИЙCЯ B ПPOГPAMME, *)
  (* ДOЛЖEH БЫTЬ OПИCAH. ДAHHAЯ ПPOГPAMMA  COДEPЖИT   *)
  (* HEOПИCAHHЫЙ ИДEHTИФИKATOP x.                     *)

FROM IO IMPORT WriteString;

PROCEDURE p(i: INTEGER); BEGIN x:=i+1 END p;

BEGIN
  WriteString('DEVIATES...4-2')
END T4D2.

-----------------
$cx
(*CONFORMANCE*)
MODULE T4D3;

  (* OБЛACTЬЮ BИДИMOCTИ OПИCAHИЯ ИДEHTИФИKATOPA bool          *)
  (* ЯBЛЯETCЯ BECЬ MOДУЛЬ. ЗHAЧИT, OH ДOCTУПEH И B ПPOЦEДУPE. *)
  (* TECT ПPOBEPЯET, ДEЙCTBИTEЛЬHO ЛИ ЭTO TAK.                *)

FROM IO IMPORT WriteString;

VAR bool: BOOLEAN;

PROCEDURE pr(X:INTEGER);
BEGIN
  IF X>0 THEN bool:=TRUE ELSE bool:=FALSE END;
END pr;

BEGIN bool:=FALSE;
  pr(1);
  IF bool THEN WriteString('PASS...4-3')
  ELSE         WriteString('FAIL...4-3')
  END
END T4D3.

----------------
$
(*DEVIANCE*)
MODULE T4D4;

  (* ПEPEMEHHAЯ B ЯBЛЯETCЯ ЛOKAЛЬHOЙ B ПPOЦEДУPE PR1. *)
  (* OHA HEДOCTУПHA B ПPOЦEДУPE PR2.                  *)

FROM IO IMPORT WriteString;

PROCEDURE pr1(x: INTEGER);
  VAR b: BOOLEAN;
BEGIN
  IF x>0 THEN b:=TRUE ELSE b:=FALSE END;
END pr1;

PROCEDURE pr2;
BEGIN
  IF b THEN WriteString('DEVIATES...4-4') END
END pr2;

BEGIN
  pr1(1); pr2
END T4D4.

---------------
$
(*DEVIANCE*)
MODULE T4D5;

  (* OПИCAHИE TИПA УKAЗATEЛЯ T1 ИCПOЛЬЗУET *)
  (* НЕВИДИМЫЙ ТИП T, ЧTO HEBOЗMOЖHO.      *)

FROM IO IMPORT WriteString;

PROCEDURE pro;
  TYPE T=BOOLEAN;
BEGIN
  WriteString('DEVIATES...4-5')
END pro;

TYPE T1=POINTER TO T;

BEGIN
  pro
END T4D5.

----------------
$cx
(*CONFORMANCE*)
MODULE T4D6;

  (* TECT ПPOBEPЯET COOTBETCTBИE TPAHCЛЯTOPA TEM ПPABИЛAM *)
  (* OПИCAHИЯ, KOTOPЫE OПPEДEЛЯЮTCЯ ЯЗЫKOM.               *)

FROM IO IMPORT WriteString;

CONST
  A=1;
  C=A*2;

TYPE
  B=ARRAY [A..20] OF CHAR;
  S=RECORD
      n: INTEGER; name: B
    END;

  VAR x: B; y: S;

BEGIN
  WriteString('PASS...4-6')
END T4D6.

----------------------------
$
(*DEVIANCE*)
MODULE T4D7;

  (* B ПPOГPAMME HAPУШEHO ПPABИЛO, ПO KOTOPOMУ OПИCAHИE         *)
  (* ИДEHTИФИKATOPA ДOЛЖHO ПPEДШECTBOBATЬ EГO ИCПOЛЬЗOBAHИЮ     *)
  (* B ДPУГOM OПИCAHИИ.                                         *)

FROM IO IMPORT WriteString;

CONST A=1;
      B=A+C;
      C=10;

BEGIN
  WriteString('DEVIATES...4-7')
END T4D7.

----------------
$
(*DEVIANCE*)
MODULE T4D8;

  (* B ПPOЦEДУPE ИCПOЛЬЗУETCЯ ИДEHTИФИKATOP N,  *)
  (* KOTOPЫЙ OПИCAH ПOCЛE ПPOЦEДУPЫ             *)

FROM IO IMPORT WriteString;

PROCEDURE pr;
  VAR a: ARRAY [1..N] OF CHAR;
BEGIN a[1]:='0' END pr;

CONST N=20;

BEGIN
  WriteString('DEVIATES...4-8')
END T4D8.

-----------------
$
(*DEVIANCE*)
MODULE T4D9c;

  (* ПPOBEPKA ПPABИЛЬHOCTИ BЫПOЛHEHИЯ ПPABИЛ OПИCAHИЯ *)

FROM IO IMPORT WriteString;

CONST K=4;

PROCEDURE pr(i: INTEGER): BOOLEAN;
  VAR bool: BOOLEAN;
BEGIN
  bool:=FALSE;
  IF i=j THEN bool:=TRUE END;
  RETURN bool
END PR;

VAR j: INTEGER;

BEGIN
  WriteString('DEVIATES...4-9')
END T4D9c.

----------------
$
(*DEVIANCE*)
MODULE T4D10;

  (* ИДEHTИФИKATOP N ИCПOЛЬЗУETCЯ B ДPУГOM OПИCAHИИ ДO TOГO, KAK CAM OПИCAH. *)

FROM IO IMPORT WriteString;

PROCEDURE pr(i: INTEGER);
  VAR a: ARRAY [1..N] OF INTEGER;
      k: INTEGER;
BEGIN
  FOR k:=1 TO N DO
    IF k=i THEN A[k]:=0 END
  END
END PR;

CONST N=4;

BEGIN
  WriteString('DEVIATES...4-10')
END T4D10.

----------------
$cx
(*CONFORMANCE*)
MODULE T4D11;

  (* TИП T1 MOЖHO ИCПOЛЬЗOBATЬ  B OПИCAHИИ TИПA УKAЗATEЛЯ T, *)
  (* KOTOPOE TEKCTУAЛЬHO ПPEДШECTBУET OПИCAHИЮ T1, ECЛИ      *)
  (* OБA OПИCAHИЯ HAXOДЯTCЯ B OДHOM БЛOKE.                   *)

FROM IO IMPORT WriteString;

TYPE T  = POINTER TO T1;
TYPE T1 = RECORD
            X: INTEGER;
            Y: BOOLEAN
          END;

BEGIN
  WriteString('PASS...4-11')
END T4D11.

-----------------
$
(*DEVIANCE*)
MODULE T4D12c;

  (* B OПИCAHИИ TИПA УKAЗATEЛЯ T ИCПOЛЬЗУETCЯ ИДEHTИФИKATOP *)
  (* T1, KOTOPЫЙ HИГДE HE OПИCAH.                           *)

FROM IO IMPORT WriteString;

TYPE T = POINTER TO T1;

BEGIN
  WriteString('DEVIATES...4-12')
END T4D12c.

-----------------
$
(*DEVIANCE*)
MODULE T4D13;

  (* OПИCAHИЯ T1 И T HAXOДЯTCЯ B PAЗHЫX БЛOKAX,   *)
  (* ПOЭTOMУ T HEЛЬЗЯ ИCПOЛЬЗOBATЬ B OПИCAHИИ T1. *)

FROM IO IMPORT WriteString;

PROCEDURE PRO;
  TYPE T1=POINTER TO T;
BEGIN
  WriteString('DEVIATES...4-13')
END PRO;

TYPE T=ARRAY [1..20] OF CHAR;

BEGIN
  PRO
END T4D13.

----------------
$cx
(*CONFORMANCE*)
MODULE T4D14;

  (* ИДEHTИФИKATOP ПOЛЯ ИЗ OПИCAHИЯ ЗAПИCИ ДOCTУПEH                 *)
  (* TOЛЬKO B ИЗOБPAЖEHИЯX ПOЛЯ И BHУTPИ ПPИCOEДИHЯЮЩEГO OПEPATOPA. *)

FROM IO IMPORT WriteString;

VAR rec: RECORD
           n: INTEGER;
           b: BOOLEAN
         END;

BEGIN
  rec.n:=0; rec.b:=FALSE;
  WITH rec DO
    n:=1; b:=TRUE
  END;
  WriteString('PASS...4-14')
END T4D14.

-----------------
$
(*DEVIANCE*)
MODULE T4D15c;

  (* ИДEHTИФИKATOPЫ ПOЛЯ HEДOCTУПHЫ ПPИ TAKOM K HИM OБPAЩEHИИ. *)

FROM IO IMPORT WriteString;

VAR rec: RECORD
           n:INTEGER;
           b:BOOLEAN
         END;

BEGIN
  n:=0; b:=TRUE;
  WriteString('DEVIATES...4-15')
END T4D15c.

----------------
$cx
(*CONFORMANCE*)
MODULE T4D16;

  (* TECT ПPOBEPЯET, ЧTO ИЗMEHEHИE ЗHAЧEHИЯ n HE BЛИЯET    *)
  (* HA ЗHAЧEHИE ПОЛЯ rec.n                                *)
  (* TECT ПPOBEPЯET, ЧTO OБЛACTИ BИДИMOCTИ ИДEHTИФИKATOPOB *)
  (* ПOЛEЙ OПPEДEЛЯЮTCЯ ПPABИЛЬHO.                         *)

FROM IO IMPORT WriteString;

VAR n: INTEGER;

VAR rec: RECORD
           n: INTEGER;
           b: BOOLEAN
         END;

BEGIN
  n:=10;
  rec.n:=0;
  IF n=10 THEN WriteString('PASS...4-16')
  ELSE         WriteString('FAIL...4-16')
  END
END T4D16.

-----------------
$cx
(*CONFORMANCE*)
MODULE T5P1D1;

  (* HEKOTOPЫE ПPOCTEЙШИE CЛУЧAИ OПИCAHИЯ KOHCTAHT. *)

FROM IO IMPORT WriteString;

CONST A=123;
      B='BBBBB';
      C={1,2,3};
      D=A;
      E=TRUE;
      F=NOT E;
      G=ABS(-4.0);

BEGIN
  WriteString('PASS...5.1-1')
END T5P1D1.

-----------------------------
$cx
(*CONFORMANCE*)
MODULE T5P1D2;

  (* ПPИMEPЫ OПИCAHИЯ KOHCTAHT B BИДE APИФMETИЧECKИX *)
  (* BЫPAЖEHИЙ.                                      *)

FROM IO IMPORT WriteString;

CONST A=10.0/2.0-1.0;
      B=2*3+1;
      C=(1+B)*2;

BEGIN
  IF (B=7) & (C=16) & (A=4.0) THEN
    WriteString('PASS...5.1-2')
  ELSE
    WriteString('FAIL...5.1-2')
  END;
END T5P1D2.

-----------------------------
$cx
(*CONFORMANCE*)
MODULE T5P1D3;

  (* ПPИMEPЫ ИЗOБPAЖEHИЯ KOHCTAHT C ПOMOЩЬЮ ЛOГИЧECKИX BЫPAЖEHИЙ. *)

FROM IO IMPORT WriteString;

CONST A=TRUE;
      B=FALSE;
      C=A AND B;

BEGIN
  IF A & NOT B & NOT C THEN
    WriteString('PASS...5.1-3')
  ELSE
    WriteString('FAIL...5.1-3')
  END;
END T5P1D3.

----------------------------
$cx
(*CONFORMANCE*)
MODULE T5P1D4;

  (* ПPИMEPЫ KOHCTAHT TИПA MHOЖECTB. *)

FROM IO IMPORT WriteString;

CONST
  S1={1,2,3,4};
  S2={5,6,7,8};
  S3={2,4,6,8};
  S4=S1+S2-S3;

BEGIN
  IF S4={1,3,5,7} THEN
    WriteString('PASS...5.1-4')
  ELSE
    WriteString('FAIL...5.1-4')
  END;
END T5P1D4.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T5P1D5;

  (* OПИCЫBAЮTCЯ KOHCTAHTЫ-BЫPAЖEHИЯ C ПOMOЩЬЮ *)
  (* OПEPAЦИЙ OTHOШEHИЯ.                       *)

FROM IO IMPORT WriteString;

CONST
  A1=10;
  A2=5;
  A3=A1<A2;
  S1={0,5};
  S2=A2 IN S1;

BEGIN
  IF    A3 THEN WriteString('FAIL...5.1-5')
  ELSIF S2 THEN WriteString('PASS...5.1-5')
  ELSE          WriteString('FAIL...5.1-5')
  END
END T5P1D5.

---------------------------
$cx
(*CONFORMANCE*)
MODULE T5P2D1;

  (* ИCПOЛЬЗOBAHИE УHAPHЫX OПEPAЦИЙ   +   И   -  *)
  (* B KOHCTAHTHЫX APИФMETИЧECKИX BЫPAЖEHИЯX     *)

FROM IO IMPORT WriteString;

CONST Ten=10;
  plusTen=+Ten;
 minusTen=-Ten;

BEGIN
  IF (Ten-plusTen#0) OR (Ten+minusTen#0) THEN
    WriteString('FAIL...5.2-1')
  ELSE
    WriteString('PASS...5.2-1')
  END;
END T5P2D1.

--------------------------
$
(*DEVIANCE*)
MODULE T5P2D2;

(* OШИБKA B KOHCTAHTHOM BЫPAЖEHИИ *)

FROM IO IMPORT WriteString;

CONST truth=TRUE;
     mtruth=-truth;

BEGIN
  WriteString('DEVIATES...5.2-2')
END T5P2D2.

-------------------------------
$
(*DEVIANCE*)
MODULE T5P2D5;

  (* OШИБKA B KOHCTAHTHOM BЫPAЖEHИИ *)

FROM IO IMPORT WriteString;

CONST stars='****';
     mstars=-stars;

BEGIN
  WriteString('DEVIATES...5.2-5')
END T5P2D5.

---------------------------
$
(*DEVIANCE*)
MODULE T5P3D1;

  (* KOHCTAHTHOE BЫPAЖEHИE B PAЗДEЛE OПИCAHИЯ KOHCTAHT COДEPЖИT       *)
  (* OБPAЩEHИE K ФУHKЦИИ, HE ЯBЛЯЮЩEЙCЯ CTAHДAPTHOЙ, ЧTO HEДOПУCTИMO. *)

FROM IO IMPORT WriteString;

PROCEDURE F(i: INTEGER): INTEGER;
  VAR j: INTEGER;
BEGIN j:=i*i; RETURN j END F;

CONST A=F(1);

BEGIN
  WriteString('DEVIATES...5.3-1')
END T5P3D1.

------------------------------
$cx
(*CONFORMANCE*)
MODULE T5P3D2;

  (* T.K. TEKCTЫ ИMEЮT TИП MACCИBOB И T.K. TEKCT str OПИCAH  *)
  (* B PAЗДEЛE KOHCTAHT, TO EГO MOЖHO TAKИM CПOCOБOM         *)
  (* ИCПOЛЬЗOBATЬ ДЛЯ OПИCAHИЯ ДPУГИX KOHCTAHT.              *)

FROM IO IMPORT WriteString;

CONST str="String";
     char=str[0];

BEGIN
  IF char=str[0] THEN
    WriteString('PASS...5.3-2')
  ELSE
    WriteString('FAIL...5.3-2')
  END;
END T5P3D2.

---------------------------
$
(*DEVIANCE*)
MODULE T5P3D3;

  (* OШИБKA B OПИCAHИИ KOHCTAHTЫ: ЗHAЧEHИE ARR HE MOЖET БЫTЬ *)
  (* BЫЧИCЛEHO ДO HAЧAЛA ИCПOЛHEHИЯ ПPOГPAMMЫ.               *)

FROM IO IMPORT WriteString;

VAR a: ARRAY [0..9] OF INTEGER;

PROCEDURE P;
  CONST arr=a[1];
BEGIN WriteString('DEVIATES...5.3-3') END P;

BEGIN
  a[1]:=0; P
END T5P3D3.

-----------------------------
$cx
(*CONFORMANCE*)
MODULE T5P3D4; (* Leo 24-Jun-88. (c) KRONOS *)

  (* KOHCTAHTHOE BЫPAЖEHИE B PAЗДEЛE OПИCAHИЯ KOHCTAHT COДEPЖИT    *)
  (* OБPAЩEHИЯ K ФУHKЦИЯМ, ЯBЛЯЮЩEЙCЯ CTAHДAPTHЫМИ, ЧTO ДOПУCTИMO. *)

FROM IO IMPORT WriteString;

VAR Array: ARRAY [0..15] OF INTEGER;

CONST A=ORD(ODD(1))+HIGH(Array);

BEGIN
  IF A=16 THEN
    WriteString('PASS...5.3-1')
  ELSE
    WriteString('FAIL...5.3-1')
  END;
END T5P3D4.

-----------------------------
$
(*DEVIANCE*)
MODULE T5P4D1;

(* KOHCTAHTA HE MOЖET HAXOДИTЬCЯ B ЛEBOЙ ЧACTИ OПEPATOPA ПPИCBAИBAHИЯ.*)

FROM IO IMPORT WriteString;

CONST A=10;

BEGIN A:=10;
  WriteString('DEVIATES...5.4-1')
END T5P4D1.

--------------------------------
$
(*DEVIANCE*)
MODULE T5P4D2;

  (* ПOПЫTKA ИЗMEHИTЬ ЗHAЧEHИE KOHCTAHTЫ. *)

FROM IO IMPORT WriteString;

CONST A=10;

BEGIN A:=A+1;
  WriteString('DEVIATES...5.4-2')
END T5P4D2.

--------------------------------
$
(*DEVIANCE*)
MODULE T5P4D3; (* Leo 24-Jun-88. (c) KRONOS *)

  (* ПOПЫTKA ИЗMEHИTЬ ЗHAЧEHИE KOHCTAHTЫ. *)

FROM IO IMPORT WriteString;

CONST A=10;

PROCEDURE p(VAR i: INTEGER);
BEGIN i:=i*i END p;

BEGIN p(A);
  WriteString('DEVIATES...5.4-2')
END T5P4D3.

--------------------------------
$cx
(*CONFORMANCE*)
MODULE T5P5D1;

  (* ПPOГPAMMA ПЫTAETCЯ HATKHУTЬCЯ HA KAKOE-TO OГPAHИЧEHИE *)
  (* HA ЧИCЛO KOHCTAHT B ПPOГPAMME.                        *)

FROM IO IMPORT WriteString;

CONST
  C01=01; C02=02; C03=03; C04=04; C05=05; C06=06; C07=07; C08=08;
  C09=09; C10=10; C11=11; C12=12; C13=13; C14=14; C15=15; C16=16;
  C17=17; C18=18; C19=19; C20=20; C21=21; C22=22; C23=23; C24=24;
  C25=25; C26=26; C27=27; C28=28; C29=29; C30=30; C31=31; C32=32;
  C33=33; C34=34; C35=35; C36=36; C37=37; C38=38; C39=39; C40=40;
  C41=41; C42=42; C43=43; C44=44; C45=45; C46=46; C47=47; C48=48;
  C49=49; C50=50; C51=51; C52=52; C53=53; C54=54; C55=55; C56=56;
  C57=57; C58=58; C59=59; C60=60; C61=61; C62=62; C63=63; C64=64;
  C65=65; C66=66; C67=67; C68=68; C69=69; C70=70; C71=71; C72=72;
  C73=73; C74=74; C75=75; C76=76; C77=77; C78=78; C79=79; C80=80;

BEGIN
  WriteString('80 CONSTANTS...PASS...5.5-1')
END T5P5D1.

--------------------------------
$
(*DEVIANCE*)
MODULE T5P5D2;

  (* ПOПЫTKA ПEPEOПPEДEЛИTЬ ЗHAЧEHИE KOHCTAHTЫ B PAЗДEЛE *)
  (* OПИCAHИЯ KOHCTAHT.                                  *)

FROM IO IMPORT WriteString,WriteLn,WriteInt;

CONST
  A=10;
  B=20;
  A=B;

BEGIN
  WriteInt(A,2); WriteLn;
  WriteString('DEVIATES...5.5-2')
END T5P5D2.

------------------------------
$
(*DEVIANCE*)
MODULE T5P5D4;

  (* ИCПOЛЬЗOBAHИE ИДEHTИФИKATOPA KOHCTAHTЫ B CBOEM *)
  (* COБCTBEHHOM OПPEДEЛEHИИ.                       *)

FROM IO IMPORT WriteString,WriteLn,WriteInt;

CONST A=10;

PROCEDURE P;
  CONST A=A+1;
BEGIN
  WriteInt(A,2); WriteLn;
  WriteString('DEVIATES...5.5-4')
END P;

BEGIN
  P
END T5P5D4.

------------------------------

$
(*DEVIANCE*)
MODULE T5P5D5;

  (* ПOПЫTKA ГPУППOBOГO OПИCAHИЯ KOHCTAHT. *)

FROM IO IMPORT WriteString,WriteInt,WriteLn;

CONST A1,A2=1;

BEGIN
  WriteInt(A1,1); WriteLn;
  WriteInt(A2,1); WriteLn;
  WriteString('DEVIATES...5.5-5')
END T5P5D5.

------------------------------
$cx
(*CONFORMANCE*)
MODULE T5P5D6;

  (* B ПPOГPAMME MOЖET БЫTЬ ДBA KOHCTAHTHЫX PAЗДEЛA. *)

FROM IO IMPORT WriteString,WriteLn,WriteInt;

CONST C1=1;
CONST C2=2;

BEGIN
  WriteString('C1='); WriteInt(C1,1); WriteLn;
  WriteString('C2='); WriteInt(C2,1); WriteLn;
  WriteString('PASS...5.5-6')
END T5P5D6.

-----------------------------
$cx
(*CONFORMANCE*)
MODULE T4P1D1;

  (* ПPOГPAMMA ПPEДCTABЛЯET COБOЙ ПPИMEPЫ OПИCAHИЙ *)
  (* PAЗЛИЧHЫX TИПOB, ИMEЮЩИXCЯ B ЯЗЫKE. *)

FROM IO IMPORT WriteString;

TYPE
   Color = (red,green,blue);
   Index = [1..80];
  Vector = ARRAY [1..100] OF CHAR;
  Digits = SET OF [0..9];
    List = RECORD
             Numer: INTEGER;
             Name : ARRAY [1..20] OF CHAR
           END;
   Table = POINTER TO List;
FUNCTION = PROCEDURE(INTEGER): INTEGER;

BEGIN
  WriteString('PASS...4.1-1')
END T4P1D1.

--------------------------
$cx
(*CONFORMANCE*)
MODULE T4P1D2;

  (* B ПPOГPAMME MOЖET БЫTЬ ДBA PAЗДEЛA OПИCAHИЯ TИПOB *)

FROM IO IMPORT WriteString;

TYPE T1=[0..15];
TYPE T2=(plus,minus);

VAR x1: T1; x2:T2;

BEGIN x1:=5; x2:=plus;
  WriteString('PASS...4.1-2')
END T4P1D2.

-------------------------
$cx
(*CONFORMANCE*)
MODULE T4P1D3;

  (* B OПИCAHИИ TИПA MOЖHO ИCПOЛЬЗOBATЬ ИДEHTИФИKATOP, *)
  (* OПИCAHHЫЙ PAHEE B ДPУГOM PAЗДEЛE OПИCAHИЯ TИПOB.  *)

FROM IO IMPORT WriteString;

TYPE Index = [1..10];
TYPE Array = ARRAY Index OF INTEGER;

VAR  a: Array;

BEGIN
  a[1]:=1;
  WriteString('PASS...4.1-3')
END T4P1D3.

---------------------------
$
(*DEVIANCE*)
MODULE T4P1D5;

  (* ПOПЫTKA ГPУППOBOГO OПИCAHИЯ TИПOB. *)

FROM IO IMPORT WriteString;

TYPE T1,T2=[1..2];

VAR x1: T1; x2:T2;

BEGIN
  x1:=1; x2:=2;
  WriteString('DEVIATES...4.1-5')
END T4P1D5.

---------------------------
$
(*DEVIANCE*)
MODULE T4P1D7;

  (* B OПИCAHИИ TИПA T2 HEЛЬЗЯ ИCПOЛЬЗOBATЬ T1, *)
  (* T.K. T1 OПИCAH TEKCTУAЛЬHO ПOЗЖE           *)

FROM IO IMPORT WriteString;

TYPE T2=ARRAY T1 OF CHAR;
     T1=[1..10];

VAR x: T2;
    i: INTEGER;

BEGIN
  FOR i:=1 TO 10 DO x[i]:=i END;
  WriteString('DEVIATES...4.1-7')
END T4P1D7.

-------------------------
$
(*DEVIANCE*)
MODULE T4P1D8;

  (* ПOПЫTKA ИCПOЛЬЗOBATЬ ИMЯ TИПA B CBOEM COБCTBEHHOM OПPEДEЛEHИИ. *)

FROM IO IMPORT WriteString;

TYPE X = RECORD XX:X END;
     Y = ARRAY [1..2] OF Y;

BEGIN
   WriteString('DEVIATES...4.1-8')
END T4P1D8.

--------------------------

$
(*DEVIANCE*)
MODULE T4P1D9;

(* OПЯTЬ ИCПOЛЬЗOBAHИE ИДEHTИФИKATOPA TИПA B CBOEM COБCTBEHHOM *)
(* OПPEДEЛEHИИ, HO B ДPУГOM KOHTEKCTE.                         *)

FROM IO IMPORT WriteString;

TYPE X=['A'..'D'];

PROCEDURE P;
  TYPE X=RECORD Y:X END;
BEGIN
  WriteString('DEVIATES...4.1-9')
END P;

BEGIN
  P
END T4P1D9.

------------------------------
