IMPLEMENTATION MODULE realMath; (* Roger 06-jun-88.  (c) KRONOS *)
                                (* brd   05-aug-90.  (c) KRONOS *)
                                (* brd   19-oct-90.  (c) KRONOS *)

  FROM  SYSTEM  IMPORT  WORD ;
  IMPORT mcd: defCodes;

CONST mul2 = INTEGER({23});
      mul4 = INTEGER({24});
      Eps  = 1.E-7;
      ipi  = 1./pi;
      ie   = 1./e;
     delta = REAL({23,29}); -- 2**-64

PROCEDURE SHL(x: WORD; sh: INTEGER): WORD;
CODE mcd.shl END SHL;

PROCEDURE SHR(x: WORD; sh: INTEGER): WORD;
CODE mcd.shr END SHR;

PROCEDURE ext(VAR min,max: REAL; VAL x: ARRAY OF REAL);
  VAR i: INTEGER;
BEGIN
  min:= maxREAL; max:=-minREAL;
  FOR i:=0 TO HIGH(x) DO
    IF x[i] >= max THEN max:=x[i] END;
    IF x[i] <= min THEN min:=x[i] END;
  END;
END ext;

PROCEDURE max(x,y: REAL): REAL;          (* максимальное из x,y *)
BEGIN IF x>y THEN RETURN x END; RETURN y END max;

PROCEDURE min(x,y: REAL): REAL;          (* минимальное из x,y *)
BEGIN IF x<y THEN RETURN x END; RETURN y END min;

PROCEDURE round(x: REAL): INTEGER;        (* округление до целого *)
  VAR i: INTEGER;
BEGIN
  i:=ABS(TRUNC(x));
  IF ABS(x)-FLOAT(i)>=0.5 THEN INC(i) END;
  IF x<0. THEN RETURN -i END;
  RETURN i
END round;

PROCEDURE rtod(x: REAL): REAL;            (* перевод из радиан в градусы *)
BEGIN RETURN x*180./pi END rtod;

PROCEDURE dtor(x: REAL): REAL;            (* перевод из градусов в радианы *)
BEGIN RETURN x*pi/180. END dtor;

PROCEDURE sqrt(x: REAL): REAL;
  CONST a0 = 0.9; b0 = 0.2749;
        a1 = 0.7; b1 = 0.3550;
        a2 = 0.6; b2 = 0.4148;
        a3 = 0.5; b3 = 0.5005;
        mask = {23..30};
        mneg = {1..22,31};
        modd = {23..29};
        mevn = {30};
        one  = 1.0-1.E-5;
   VAR b,sv: BITSET;
       sq, y: REAL;
       odd: BOOLEAN;
BEGIN
  b:=BITSET(x);
  IF x=0.0  THEN RETURN 0. END;
  ASSERT(x>=0.0,42h);
  odd:=23 IN b;
  sv:=mask*b; b:=b*mneg;
  IF odd THEN b:=b+modd ELSE b:=b+mevn END;
  y:=REAL(b);
  IF y>one THEN b:=BITSET(1.-(1.-y)/2.);
  ELSE
    IF y<0.39 THEN sq:=a0*y + b0
    ELSIF y<0.60 THEN sq:=a1*y + b1
    ELSIF y<0.84 THEN sq:=a2*y + b2
    ELSE sq:=a3*y + b3 END;
    sq:=REAL(INTEGER(sq + y/sq)-mul2);
    b:=BITSET(INTEGER(sq + y/sq)-mul2);
  END;
  IF 30 IN BITSET(x) THEN
    IF odd THEN sv:=BITSET(INTEGER(mevn)+INTEGER(SHR(BITSET(INTEGER(sv)-INTEGER(modd)),1)))
    ELSE sv:=BITSET(INTEGER(mevn)+INTEGER(SHR(BITSET(INTEGER(sv)-INTEGER(mevn)),1)))
    END
  ELSE
    IF odd THEN
      sv:=BITSET(INTEGER(mevn)-INTEGER(SHR(BITSET(INTEGER(modd)-INTEGER(sv)),1)))
    ELSE
      sv:=BITSET(INTEGER(mevn)-INTEGER(SHR(BITSET(INTEGER(mevn)-INTEGER(sv)),1)))
    END
  END;
  RETURN REAL(b*mneg+sv)
END sqrt;

PROCEDURE sin(x: REAL): REAL;
  CONST s1 = 1.57079631847;
        s3 =-6.4596371106e-1;
        s5 = 7.968967928e-2;
        s7 =-4.67376557e-3;
        s9 = 1.5148419e-4;
  VAR y,y2,s: REAL;
BEGIN
  IF x=0.  THEN RETURN 0. END;
  IF x=pi2 THEN RETURN 1. END;
  IF x=pi  THEN RETURN 0. END;
  y:=REAL(INTEGER(x/pi2-1.)-mul4);
  y:=ABS(REAL(INTEGER(ABS(y-FLOAT(TRUNC(y))))+mul4)-2.)-1.;
  y2:=y*y; s:=((y2*s9+s7)*y2+s5)*y2;
  RETURN ((s+s3)*y2+s1)*y
END sin;

PROCEDURE cos(x: REAL): REAL;
  CONST s1 = 1.57079631847;
        s3 =-6.4596371106e-1;
        s5 = 7.968967928e-2;
        s7 =-4.67376557e-3;
        s9 = 1.5148419e-4;
  VAR y,y2,s: REAL;
BEGIN
  IF x=0.  THEN RETURN  1. END;
  IF x=pi2 THEN RETURN  0. END;
  IF x=pi  THEN RETURN -1. END;
  x:=x+pi2;
  y:=REAL(INTEGER(x/pi2-1.)-mul4);
  y:=ABS(REAL(INTEGER(ABS(y-FLOAT(TRUNC(y))))+mul4)-2.)-1.;
  y2:=y*y; s:=((y2*s9+s7)*y2+s5)*y2;
  RETURN ((s+s3)*y2+s1)*y
END cos;

PROCEDURE xpi4(x: REAL): REAL;  -- x ^ 0..pi/4
  VAR yy,y: REAL;
  CONST k0= 0.7853980289;
        k2=-0.6545887679;
        k1= 6.1922344479;
        k3=1./491.0013934779;
BEGIN
  y:=x/pi4;
  IF y<=delta THEN RETURN x END;
  yy:=y*y; x:=k2+yy*k3;
  RETURN y*(k0+yy/(k1+yy/x))
END xpi4;

PROCEDURE tg(x: REAL): REAL;
  VAR minus: BOOLEAN; pos: INTEGER;
BEGIN
  minus:=(x<0.); x:=ABS(x);
  pos:=TRUNC(x/pi4); x:=x-FLOAT(pos)*pi4;
  pos:=INTEGER(BITSET(pos)*{0..1});
  IF ODD(pos) THEN x:=xpi4(pi4-x) ELSE x:=xpi4(x) END;
  IF pos IN {1,2} THEN x:=1./x END;
  IF (pos>1)#minus THEN RETURN -x END;
  RETURN x
END tg;

PROCEDURE exp2(x: REAL): REAL;            (* 2 в степени x *)
  CONST a1 = 8.6643396773e-2;
        a2 = 3.753591712e-3;
        a3 = 1.08419178e-4;
        a4 = 2.3481760e-5;
  VAR i: INTEGER;
      s,y: REAL;
      k: BITSET;
BEGIN
  y:=ABS(x);
  (* ASSERT(y<126.099,42h) *)
  IF y>126.099 THEN
    IF x<0.0 THEN
      RETURN 0.
    ELSE
      ASSERT(y<126.099,42h);
(* print(' слишком болшая степень двойки , не более  126.099 !!!'7c) *)
    END;
  ELSIF y<1.e-30 THEN RETURN 1. END;
  i:=TRUNC(y);
  y:=y-FLOAT(i);
  s:=(((a4*y+a3)*y+a2)*y+a1)*y+1.;
  s:=s*s; s:=s*s; s:=s*s;
  k:=SHL(BITSET(i),23);
  s:=REAL(INTEGER(s)+INTEGER(k));
  IF x<0. THEN RETURN 1./s END;
  RETURN s
END exp2;
(*
PROCEDURE exp(x: REAL): REAL;
  CONST k = 1.4426950409;
BEGIN RETURN exp2(x*k) END exp;
*)

PROCEDURE _exp(x: REAL): REAL;
  CONST k0=1.0000000020967;
        k1=0.0999743507186;
        k2=0.0166411490538;
  VAR xx: REAL;
BEGIN xx:=x*x;
  xx:=(k0+k1*xx)/(1.+k2*xx);
  RETURN 1.+ x/(xx-x*0.5)
END _exp;

PROCEDURE expn(n: INTEGER): REAL;
  VAR x: INTEGER;
BEGIN
  ASSERT(n>=0);
  CASE n OF
    |0 : RETURN 1.
    |1 : RETURN e
    |2 : RETURN e*e
    |3 : RETURN e*e*e
    |4 : RETURN e*e*e*e
    |5 : RETURN e*e*e*e*e
    |6 : RETURN e*e*e*e*e*e
    |7 : RETURN e*e*e*e*e*e*e
    |8 : RETURN e*e*e*e*e*e*e*e
    |9 : RETURN e*e*e*e*e*e*e*e*e
   ELSE  RETURN(e*e*e*e*e*e*e*e*e*e)*expn(n-10)
   END;
END expn;

PROCEDURE exp(x: REAL): REAL;
  VAR n: INTEGER;
BEGIN
  IF x<0. THEN RETURN 1./exp(-x) END;
  n:=TRUNC(x); x:=x-FLOAT(n);
  RETURN _exp(x)*expn(n)
END exp;

PROCEDURE ctg(x:REAL):REAL;
BEGIN  RETURN tg(pi2-x)  END ctg;

(*
PROCEDURE arctg(x: REAL): REAL;
  CONST
    A = 0.5550587037;
    B =-0.6576072985;
    C = 0.2488243799;
    D = 0.1750450062;
    E =-0.5861326182;
  VAR f,g,z: REAL; n: INTEGER; Avals: ARRAY [0..3] OF REAL;
BEGIN
   Avals[0]:=0.0;
   Avals[1]:=0.5235987755;
   Avals[2]:=1.5707963268;
   Avals[3]:=1.0471975511;
  f:=ABS(x); n:=0;
  IF f > 1.0 THEN f:=1.0/f; n:=2 END;
  IF f > 0.2679491924 THEN
    f := (((0.7320508075 * f - 0.5) - 0.5) + f) / (1.7320508075 + f);
    INC(n);
  END;
  IF ABS(f)<2.3e-10 THEN z:=f
  ELSE
    g:=f*f; z:=A*g+B; z:=z*z;
    z := f * ((z + C + g) * (z + D) - E)
  END;
  IF n>1 THEN z:=-z END;
  z := z + Avals[n];
  IF x<0.0 THEN z:=-z END;
  RETURN z
END arctg;
*)

PROCEDURE _arctg(x: REAL): REAL;  -- -1.<=x<=1.
  CONST k0=  0.99999752;
        k1=- 3.00064286;
        k2=- 0.55703890;
        k3=-17.03715998;
        k4=- 0.20556880;
  VAR xx,y: REAL;
BEGIN
  xx:=x*x;
  y:=       k2+xx/(k3+xx/k4);
  RETURN x*(k0+xx/(k1+xx/y))
END _arctg;

PROCEDURE arctg(x: REAL): REAL;
  VAR minus: BOOLEAN;
BEGIN
  minus:=(x<0.); x:=ABS(x);
  IF x>1. THEN x:=pi2-_arctg(1./x) ELSE x:=_arctg(x) END;
  IF minus THEN RETURN -x ELSE RETURN x END;
END arctg;

PROCEDURE arcsin(x: REAL): REAL;
BEGIN
  IF x=0. THEN RETURN 0.
  ELSIF x= 1. THEN RETURN pi2
  ELSIF x=-1. THEN RETURN -pi2
  ELSE
    RETURN arctg(x/sqrt(1.-x*x))
  END
END arcsin;

PROCEDURE arccos(x: REAL): REAL;
BEGIN
  IF x=0. THEN  RETURN pi2
  ELSIF x= 1. THEN  RETURN 0.
  ELSIF x=-1. THEN  RETURN pi
  ELSE
    RETURN pi2-arcsin(x)
  END
END arccos;

PROCEDURE Unpack(VAR m: REAL; VAR n: INTEGER; r:REAL);
BEGIN
  n:=INTEGER(BITSET(r<<9)*{0..7})-128;
  m:=REAL(BITSET(r)*{0..22,31}+{30});
END Unpack;

PROCEDURE ln(x: REAL): REAL;
  CONST
    P0 = 2.0000000;     P1 = 0.6666666;
    P2 = 0.4000059;     P3 = 0.2852538;
    P4 = 0.2376245;     C0 = 0.70710678;
    C1 = 0.69335937;    C2 =-2.12194440e-4;
  VAR f,z: REAL; n: INTEGER;
 BEGIN
  ASSERT(x>0.0,4Fh);
  Unpack(f,n,x);
  IF f>C0 THEN z := (f - 1.0) / (f + 1.0);
  ELSE  z := (f - 0.5) / (f + 0.5); DEC(n)
  END;
  f:=z*z; z := z* ((((P4*f+P3)*f+P2)*f+P1)*f+P0);
  f:=FLOAT(n);
  RETURN (f*C2+z) + f*C1
END ln;

PROCEDURE lg(x: REAL): REAL;
BEGIN
  RETURN ln(x)*0.434294481903;
END lg;

PROCEDURE pow(x,y: REAL): REAL;     (* x в степени y  *)
BEGIN
 IF x<=0.0 THEN
  IF y=0.0 THEN RETURN 1.0 END;
  ASSERT (y>0.0); RETURN 0.0
  END;
  RETURN exp(ln(x)*y)
END pow;

PROCEDURE log(x,y: REAL): REAL;
BEGIN RETURN ln(x)/ln(y)
END log;

END realMath.
