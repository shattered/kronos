DEFINITION MODULE realMath; (* Roger 06-Jun-88.  (c) KRONOS *)
                            (* brd   05-aug-90.  (c) KRONOS *)
                            (* brd   19-oct-90.  (c) KRONOS *)
(*
      Das Teufel soll Das buseriern..
                                            (нем. площ. руг.)
                  Я.Гашек, "Похождения бравого солдата Швейка"
*)

CONST e   = 2.7182818285;
      pi  = 3.1415926535;
      pi2 = pi/2.;
      pi4 = pi/4.;
   minREAL=-REAL(7FFFFFFFh);
   maxREAL= REAL(7FFFFFFFh);

PROCEDURE ext(VAR min,max: REAL; x: ARRAY OF REAL);
  (* максимальное и минимальное из массива *)

PROCEDURE sqrt(x: REAL): REAL;

PROCEDURE sin(x: REAL): REAL;
PROCEDURE cos(x: REAL): REAL;

PROCEDURE tg(x: REAL): REAL;
PROCEDURE ctg(x: REAL): REAL;

PROCEDURE arctg(x: REAL): REAL;
PROCEDURE arcsin(x: REAL): REAL;
PROCEDURE arccos(x: REAL): REAL;

PROCEDURE max(x,y: REAL): REAL;
PROCEDURE min(x,y: REAL): REAL;

PROCEDURE rtod(x: REAL): REAL;
PROCEDURE dtor(x: REAL): REAL;

PROCEDURE round(x: REAL): INTEGER; (* округление до целого*)

PROCEDURE exp2(x: REAL): REAL;           (* 2^x *)
PROCEDURE exp (x: REAL): REAL;
PROCEDURE pow(x,y: REAL): REAL;           (* x^y  *)

PROCEDURE ln(x: REAL): REAL;
PROCEDURE lg(x: REAL): REAL;
PROCEDURE log(x,y: REAL):REAL;

END realMath.
