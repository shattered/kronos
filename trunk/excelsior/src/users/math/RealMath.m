IMPLEMENTATION MODULE RealMath; (* Dima 27-Mar-88. (c) KRONOS *)

FROM Clock      IMPORT  miliClock;
FROM StdIO      IMPORT  print;

TYPE re=REAL; bs=BITSET; in=INTEGER;

PROCEDURE sqrt(x:REAL): REAL; -- x IN 1/2..2
  VAR l,r:REAL;
BEGIN
  l:=1.; r:=x;
  REPEAT
    r:=re(in(l+r)-in({23}));
--  r:=(l+r)*0.5;
    l:=x/r;
--print('l=%.10f r=%.10f\n',l,r);
  UNTIL ABS(l-r)<eps;
  RETURN l
END sqrt;

PROCEDURE Sqrt(x:REAL): REAL;
  VAR ord: INTEGER; x1: REAL;
BEGIN
  IF x<0. THEN ASSERT(FALSE) END;
  IF x=0. THEN RETURN 0. END;
  x1:=re((bs(x)-{24..30})+{30});
--print('x1=%{}\n',x1);
  ord:=in( (bs(x)*{24..30}) >> 1 );
  RETURN re( in(bs(sqrt(x1))-{30})+ord+in({29}) )
END Sqrt;

CONST sqrt_delta = REAL({23,29}); -- 2**-64
      pi4 = PI/4.; pi2 =PI/2.;

      f23=1./2./3.; f45=1./4./5.; f67=1./6./7.; f89=1./8./9.;
      f21=1./2./1.; f43=1./4./3.; f65=1./6./5.; f87=1./8./7.;

PROCEDURE sin(x:REAL): REAL;  -- x IN 0..PI/4
  VAR xx,y:REAL;
BEGIN
  IF x<=sqrt_delta THEN RETURN x END;
  xx:=x*x;
  y:=+1.+ xx*f67*
    (-1.+ xx*f89);
  RETURN x*
    (+1.+ xx*f23*
    (-1.+ xx*f45*y))
END sin;

PROCEDURE cos(x:REAL): REAL;  -- x IN 0..PI/4
  VAR xx,y:REAL;
BEGIN
  IF x<=sqrt_delta THEN RETURN 1. END;
  xx:=x*x;
  y:=+1.+ xx*f65*
    (-1.+ xx*f87);
  RETURN
    (+1.+ xx*f21*
    (-1.+ xx*f43*y))
END cos;

PROCEDURE tg(x:REAL): REAL;  -- x IN 0..PI/4
  VAR yy,y: REAL;
  CONST k0= 0.7853980289;  k2=-0.6545887679;
        k1= 6.1922344479;  k3=1./491.0013934779;
BEGIN
  y:=x/pi4;
  IF y<=sqrt_delta THEN RETURN x END;
  yy:=y*y;
  x:=k2+yy*k3;
  RETURN y*(k0+yy/
           (k1+yy/x))
END tg;

PROCEDURE Cos(x:REAL): REAL;
  VAR pos: INTEGER;
BEGIN
  x:=ABS(x);
  pos:=TRUNC(x/pi4);
  x:=x-FLOAT(pos)*pi4;
  pos:=in(bs(pos)*{0..2});
  IF ODD(pos) THEN x:=pi4-x END;
  IF pos IN {1,2,5,6} THEN x:=sin(x) ELSE x:=cos(x) END;
  IF pos IN {2,3,4,5} THEN RETURN -x ELSE RETURN x  END;
--  RETURN re( bs(x)+bs( (pos IN {2..5}) >>1 ) )
END Cos;

PROCEDURE Sin(x:REAL): REAL; BEGIN
  RETURN Cos(x-pi2)
END Sin;

PROCEDURE Tg (x:REAL): REAL;
  VAR minus: BOOLEAN; pos: INTEGER;
BEGIN
  minus:=(x<0.); x:=ABS(x);
  pos:=TRUNC(x/pi4);
  x:=x-FLOAT(pos)*pi4;
  pos:=in(bs(pos)*{0..1});
  IF ODD(pos) THEN x:=tg(pi4-x) ELSE x:=tg(x) END;
  IF pos IN {1,2} THEN x:=1./x END;
  IF (pos>1)#minus THEN RETURN -x END;
  RETURN x
END Tg;

PROCEDURE Ctg(x:REAL): REAL; BEGIN
  RETURN Tg(pi2-x)
END Ctg;

PROCEDURE exp(x:REAL): REAL;
  CONST k0=1.0000000020967;
        k1=0.0999743507186;
        k2=0.0166411490538;
  VAR xx: REAL;
BEGIN xx:=x*x;
  xx:=(k0+k1*xx)/(1.+k2*xx);
  RETURN 1.+ x/(xx-x*0.5)
END exp;

PROCEDURE expn(n: INTEGER): REAL;
  VAR x: INTEGER;
BEGIN
  ASSERT(n>=0);
  CASE n OF
    |0 : RETURN 1.
    |1 : RETURN E
    |2 : RETURN E*E
    |3 : RETURN E*E*E
    |4 : RETURN E*E*E*E
    |5 : RETURN E*E*E*E*E
    |6 : RETURN E*E*E*E*E*E
    |7 : RETURN E*E*E*E*E*E*E
    |8 : RETURN E*E*E*E*E*E*E*E
    |9 : RETURN E*E*E*E*E*E*E*E*E
   ELSE  RETURN(E*E*E*E*E*E*E*E*E*E)*expn(n-10)
   END;
END expn;

PROCEDURE Exp(x:REAL): REAL;
  VAR n: INTEGER;
BEGIN
  IF x<0. THEN RETURN 1./Exp(-x) END;
  n:=TRUNC(x); x:=x-FLOAT(n);
  RETURN exp(x)*expn(n)
END Exp;

PROCEDURE Ln (x:REAL): REAL;
BEGIN
END Ln;
PROCEDURE Pow(x,y:REAL): REAL; -- x**y
BEGIN
END Pow;
PROCEDURE Log(x,y:REAL): REAL; -- log_x_(y)
BEGIN
END Log;

PROCEDURE dum(x:REAL): REAL; BEGIN RETURN x END dum;

TYPE func=PROCEDURE(REAL): REAL;

PROCEDURE Time(f:func; n:INTEGER):INTEGER;
  VAR r:REAL; i,time: INTEGER;
BEGIN
  time:=-miliClock();
  FOR i:=1 TO n DO r:=f(FLOAT(i)) END;
  time:=time+miliClock();
  RETURN time;
END Time;

CONST n=1000;

VAR d: INTEGER;

BEGIN
(*
  d:=Time(dum,n);
  print('Sin*%d=%d msec', n, Time(Sin,n)-d);
  print('Cos*%d=%d msec', n, Time(Cos,n)-d);
  print(' Tg*%d=%d msec', n, Time(Tg ,n)-d);
  print('Ctg*%d=%d msec', n, Time(Ctg,n)-d);
  print('Sqrt*%d=%d msec',n, Time(Sqrt,n)-d);
*)
END RealMath.
