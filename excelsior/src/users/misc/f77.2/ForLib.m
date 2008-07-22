IMPLEMENTATION MODULE ForLib; (* 05-Apr-89. (c) KRONOS *)

FROM StdIO      IMPORT print, Query;
FROM SYSTEM     IMPORT ADDRESS, ADR, WORD;
FROM mCodeMnem  IMPORT store, llw4, llw5, llw6, llw7, llw8,
                       copt,  slw4, slw5, slw6, slw7, slw8,
                       mul,   sub,  fmul, fdiv, fadd, fsub,
                       entr,  lla,  rtn,  jfsc, jbs,  lss,
                       cl5,   cl8,  alloc,li0,  li1,  li2,
                       lodt,  stot, cl,   dec1, lodfv;

CONST chsize=2047;
      OutRange=1; SquareNeg=2; ExpArg=3; LogArg=4;
      SineArg=5;  CosineArg=6; AsinArg=7;AcosArg=8;
      TanArg=9;   CharBuf=10;
VAR   chstack: ARRAY [0..chsize] OF CHAR;
--    chtop  : INTEGER;
--    chbase : ADDRESS;

PROCEDURE PushW(w: WORD); CODE END PushW;

PROCEDURE ExError(i:INTEGER);
BEGIN
 print(' Ошибка при исполнении программы: %d \n',i);
 CASE i OF
   OutRange:  print(' Переполнение целого \n');
  |SquareNeg: print(' Отрицательный аргумент в SQRT \n');
  |ExpArg:    print(' Переполнение экспоненты в EXP \n');
  |LogArg:    print(' Не положительный аргумент в LOG \n');
  |SineArg:   print(' При вычислении SIN \n');
  |CosineArg: print(' При вычислении COS \n');
  |AcosArg:   print(' При вычислении ACOS \n');
  |AsinArg:   print(' При вычислении ASIN \n');
  |TanArg:    print(' При вычислении TAN \n');
  |CharBuf:   print(' Переполнение текстового буфера(2047) \n');
 ELSE
 END;
 HALT;
END ExError;

PROCEDURE Stop(cs:INTEGER; wd:WORD);
VAR base:POINTER TO ARRAY [0..0] OF CHAR;
    offs,len,i:INTEGER;
    str: ptostr;
    txt: ARRAY [0..79] OF CHAR;
BEGIN
  print(' F77  STOP: ');
  CASE cs OF
    0:   print('00 \n');
   |1:   print(' %d \n',INTEGER(wd));
   |2: str:=ptostr(wd); i:=0;
       base:=str^.base; offs:=str^.offs; len:=str^.len;
       WHILE len>0 DO
      (*$T-*)
         txt[i]:=base^[offs]; INC(i); INC(offs); DEC(len);
      (*$T+*)
         IF i>79 THEN i:=79 END;
       END; txt[i]:=0c;
         print(' %s \n',txt);
   ELSE  print(' \n ');
  END;
  HALT;
END Stop;

PROCEDURE Pause(cs:INTEGER; wd:WORD);
VAR base:POINTER TO ARRAY [0..0] OF CHAR;
    offs,len,i:INTEGER;
    str: ptostr;
    txt: ARRAY [0..79] OF CHAR;
BEGIN
  print(' F77  PAUSE: ');
  CASE cs OF
    0:   print('00 \n');
   |1:   print(' %d \n',INTEGER(wd));
   |2: str:=ptostr(wd); i:=0;
       base:=str^.base; offs:=str^.offs; len:=str^.len;
       WHILE len>0 DO
      (*$T-*)
         txt[i]:=base^[offs]; INC(i); INC(offs); DEC(len);
      (*$T+*)
         IF i>79 THEN i:=79 END;
       END; txt[i]:=0c;
         print(' %s \n',txt);
   ELSE  print(' \n ');
  END;
  IF  Query(' Продолжать? (y/n):') THEN print(' \n');
  ELSE HALT;
  END;
END Pause;

PROCEDURE ChIndex(str1,str2:ptostr):INTEGER;
VAR base1:POINTER TO ARRAY [0..0] OF CHAR;
    base2:POINTER TO ARRAY [0..0] OF CHAR;
    offs1,len1:INTEGER;
    offs2,len2:INTEGER;
    ind,i1,i2,len:INTEGER;
BEGIN
  base1:=str1^.base; offs1:=str1^.offs; len1:=str1^.len;
  base2:=str2^.base; offs2:=str2^.offs; len2:=str2^.len;
  ind:=0;
  WHILE len1>=len2 DO
    i1:=offs1+ind; i2:=offs2; len:=len2;
    LOOP
      (*$T-*)
      IF base1^[i1]#base2^[i2] THEN EXIT; END;
      (*$T+*)
      DEC(len);
      IF (len<=0) THEN RETURN ind+1 END;
      INC(i1); INC(i2);
    END;
    DEC(len1); INC(ind);
  END;
  RETURN 0;
END ChIndex;

PROCEDURE FormDesc(VAR Map:ARRAY OF INTEGER);
 VAR i: INTEGER;
     d,cpart,abs:INTEGER;
BEGIN
  cpart:=0; abs:=1;
  FOR i:=1 TO HIGH(Map) BY 3 DO
      INC(cpart,Map[i]*abs);
      d:=Map[i+1]-Map[i]+1;
      IF d>0 THEN abs:=abs*d; Map[i+2]:=abs;
      ELSE        Map[i+2]:=0;
      END;
--  print(' i= %d lo= %d  hi= %d d= %d \n', i, Map[i], Map[i+1], Map[i+2]);
  END;
  Map[0]:=cpart;
--  print(' cpart= %d \n',Map[0]);
END FormDesc;

PROCEDURE StrMove(str:ptostr);
VAR base:POINTER TO ARRAY [0..0] OF CHAR;
    offs,len:INTEGER;
    i:INTEGER;
BEGIN
  base:=str^.base; offs:=str^.offs; len:=str^.len;
  IF (chtop+len)>chsize THEN ExError(CharBuf); END;
  str^.base:=chbase; str^.offs:=chtop;
-- print('StrMove: chbase=%12d chtop=%4d len=%4d \n',
--                 chbase,chtop,len);
  FOR i:=1 TO len DO
    (*$T-*) chstack[chtop]:=base^[offs]; (*$T+*)
    INC(chtop); INC(offs);
  END;
END StrMove;

PROCEDURE ChMoveC;
CODE
 store
  llw4 cl5 llw4 0 alloc 1 sub dec1 lodfv
END ChMoveC;

PROCEDURE ChMove;
BEGIN
  ChMoveC;
END ChMove;

PROCEDURE ChFree(top:INTEGER);
BEGIN  IF top<chtop THEN chtop:=top END;
END ChFree;

PROCEDURE ChComp(str1,str2:ptostr);
CONST blank=40c;
VAR base1:POINTER TO ARRAY [0..0] OF CHAR;
    base2:POINTER TO ARRAY [0..0] OF CHAR;
    offs1,len1:INTEGER;
    offs2,len2:INTEGER;
    ch1,ch2:CHAR;
BEGIN
  base1:=str1^.base; offs1:=str1^.offs; len1:=str1^.len;
  base2:=str2^.base; offs2:=str2^.offs; len2:=str2^.len;
  LOOP
    (*$T-*) ch1:=base1^[offs1];
            ch2:=base2^[offs2];
    (*$T+*)
    IF ch1#ch2 THEN PushW(INTEGER(ch2));
                    PushW(INTEGER(ch1)); RETURN
    END;
    INC(offs1); INC(offs2);
    DEC(len1);  DEC(len2);
    IF (len1<=0) OR (len2<=0) THEN EXIT END;
  END;
  IF len1<=0 THEN
     IF len2<=0 THEN PushW(0); PushW(0);
     ELSE -- len1=0 and len2>0
       ch1:=blank;
       WHILE len2>0 DO
         (*$T-*) ch2:=base2^[offs2]; (*$T+*)
         IF ch1#ch2 THEN PushW(INTEGER(ch2));
                         PushW(INTEGER(ch1)); RETURN
         END;
         INC(offs2);
         DEC(len2);
       END;
       PushW(0); PushW(0);
     END;
  ELSE    -- len1>0 and len2=0
    ch2:=blank;
    WHILE len1>0 DO
      (*$T-*) ch1:=base1^[offs1]; (*$T+*)
      IF ch1#ch2 THEN PushW(INTEGER(ch2));
                      PushW(INTEGER(ch1)); RETURN
      END;
      INC(offs1);
      DEC(len1);
    END;
    PushW(0); PushW(0);
  END;
END ChComp;

PROCEDURE ChCompC;
CODE
  store
  llw5 llw4 cl8 slw5 0 alloc 1 sub dec1 lodfv
END ChCompC;

PROCEDURE ChCmp;
BEGIN
  ChCompC;
END ChCmp;

PROCEDURE ChAssign(str1,str2:ptostr);
CONST blank=40c;
VAR base1:POINTER TO ARRAY [0..0] OF CHAR;
    base2:POINTER TO ARRAY [0..0] OF CHAR;
    offs1,len1:INTEGER;
    offs2,len2:INTEGER;
BEGIN
  base1:=str1^.base; offs1:=str1^.offs; len1:=str1^.len;
  base2:=str2^.base; offs2:=str2^.offs; len2:=str2^.len;
--   print('ch=ch %-10d %-4d %-4d %-10d %-4d %-4d \n',
--              base1,offs1,len1,base2,offs2,len2);
  LOOP
    (*$T-*) base1^[offs1]:=base2^[offs2]; (*$T+*)
    INC(offs1); INC(offs2);
    DEC(len1);  DEC(len2);
    IF (len1<=0) OR (len2<=0) THEN EXIT END;
  END;
  WHILE len1>0 DO
    (*$T-*) base1^[offs1]:=blank; (*$T+*)
    INC(offs1);
    DEC(len1);
  END;
END ChAssign;

VAR
    degreestoradians: REAL;
CONST
    MaxCard = 7FFFFFFFh;  MinInt=80000000h;
    MaxInt  = 7FFFFFFFh;  realMaxInt = REAL(7FFFFFFFh);
                          realMinInt = REAL(80000000h);
CONST
    ExpBias  = 80h;
    MaxExp   = 127;
    MinExp   =-128;

(*
   31     23        00
   |s|<exp>|<mantise>|
*)
    TYPE int=INTEGER;

    PROCEDURE extractexp(value: REAL) : INTEGER;
      VAR s:BITSET;
    BEGIN
      s:= BITSET(value)<<9;
      RETURN int(s*{0..7}) - ExpBias
    END extractexp;

    PROCEDURE setexp(VAR result:REAL; value: INTEGER);
      VAR val, s:BITSET;
    BEGIN
      s:= BITSET(result)<<9;
      val:=BITSET(value+ExpBias);
      result:=REAL((s*{8..31}+val)>>9);
    END setexp;

    PROCEDURE addexp(x0 : REAL; add : INTEGER) : REAL;
      VAR val, s:BITSET;
    BEGIN
      s:= BITSET(x0)<<9;
      val:=BITSET(int(s*{0..7})+add); val:=val*{0..7};
      RETURN REAL((s*{8..31}+val)>>9);
    END addexp;

    PROCEDURE float(x: INTEGER): REAL;
    BEGIN
      RETURN FLOAT(INTEGER(x));
    END float;

    PROCEDURE fix(x: REAL): INTEGER;
    BEGIN
      RETURN INTEGER(TRUNC(x));
    END fix;

    PROCEDURE real(i: INTEGER): REAL;
    BEGIN
      IF i >= 0 THEN
        RETURN float(i);
      ELSIF i= INTEGER(MinInt) THEN
        RETURN realMinInt;
      ELSE
        RETURN -float(-i);
      END
    END real;

    PROCEDURE integer(x: REAL): INTEGER;
    (* conversion from real to integer with rounding *)
    BEGIN
      IF ABS(x) > FLOAT(MaxInt) THEN
        ExError(OutRange)
      END;
      IF x >= 0.0 THEN
        RETURN fix(x+0.5)
      ELSE
        RETURN -fix(-x+0.5)
      END
    END integer;

     PROCEDURE entier(x: REAL): INTEGER;
      VAR k: INTEGER;
    BEGIN
      IF x >= 0.0 THEN RETURN fix(x)
      ELSE k := fix(-x);
        IF float(k) = -x THEN RETURN -k ELSE RETURN -k-1 END
      END
    END entier;

    PROCEDURE sqrt(x: REAL): REAL;
      VAR e  : INTEGER;
          s  : REAL;
    BEGIN
      IF x = 0.0 THEN RETURN x END ;
      IF x < 0.0 THEN
        ExError(SquareNeg);
      END ;
      e := extractexp(x);
      IF ODD(e) THEN
        INC(e);
        setexp(x,-1);
      ELSE
        setexp(x,0);
      END ;
      s := 3.4321975E-01*x + 8.9969074E-01
           -3.6404085E-01/(5.0000083E-01 + x);(*Waldvogel/N.Wirth estim.*)
      s := (s + x/s)*0.5;      (* portability: precision is made here ! *)
      s := (s + x/s)*0.5;
      RETURN addexp(s, e DIV 2);
    END sqrt;

  CONST

 (* from J.F.HART, "Computer Approximations", J.Wiley & Sons 1968 *)
    sqr2     = 1.4142135623730950488016887; (* SQRT(2) *)
    loge2    = 0.6931471805599453094172321; (* LOGE(2) *)
    logesqr2 = 0.3465735902799726547086160; (* LOGE(SQRT(2)) *)

 (* HART-index LOGE 2662 *)
    cl0 = 0.19999999937438e1;     (* constants for LOGE *)
    cl1 = 0.666669484507;
    cl2 = 0.39965794919;
    cl3 = 0.301003281;

 (* HART-index EXPBC 1321 *)
    p00 = 0.7215048037358433e1;   (* constants for EXP *)
    p01 = 0.57699581512902e-1;
    q00 = 0.208182280589720e+2;
    (* q01 = 1.0 *)

    PROCEDURE exp(x: REAL): REAL;
    (* algorithm (taken from J.Waldvogel / N.Wirth ) is HART-Nr. 1321;
       constants extended to more digits                  (* 840621 *)
       accuracy: precision-index=9.22                               *)

    VAR n    : INTEGER;
        x2, y: REAL;

    BEGIN x := x / loge2;  (* should be a mult. if rezip.-val. is known *)
      n := entier(x+0.5);
      x := x - real(n);
      IF n < MinExp THEN
        RETURN 0.0
      ELSIF n >= MaxExp THEN
        ExError(ExpArg);
      END ;
      x2 := x*x;
      y  := (p00 + p01*x2)*x;
      RETURN addexp(y/(q00 + x2 - y) + 0.5, n+1);            (* 840510 *)
    END exp;

    PROCEDURE ln(x: REAL): REAL;
    (* alogrithm for ln(x) from HART: Nr.2662 [1/sqrt(2)<= x <= sqrt(2)]*)
    (* accuracy: precision-index=9.92  *)

    VAR e        : INTEGER;
        z2,z,lg  : REAL;

    BEGIN
      IF x <= 0.0 THEN
        ExError(LogArg);
      END ;
      e := extractexp(x);
      setexp(x,0);
      x  := sqr2 * x;
      z  := (x - 1.0)/(x + 1.0);
      z2 := z * z;
      lg := cl0+z2*(cl1+z2*(cl2+z2*cl3));
      lg := z*lg;
      RETURN loge2 * real(e) - logesqr2 + lg;
    END ln;

MODULE SinCos;

    IMPORT ExError, SineArg, CosineArg;
    EXPORT sin,cos;

    CONST twoopi    =  0.63661977236758134308;
    CONST p0        =  0.1357884097877375669092680E8;
    CONST p1        = -0.4942908100902844161158627E7;
    CONST p2        =  0.4401030535375266501944918E6;
    CONST p3        = -0.1384727249982452873054457E5;
    CONST p4        =  0.1459688406665768722226959E3;
    CONST q0        =  0.8644558652922534429915149E7;
    CONST q1        =  0.4081792252343299749395779E6;
    CONST q2        =  0.9463096101538208180571257E4;
    CONST q3        =  0.1326534908786136358911494E3;

    PROCEDURE sinus(arg: REAL; quad: INTEGER): REAL;
        VAR e, f, ysq, x, y, temp1, temp2: REAL;
        VAR k: INTEGER;
    BEGIN
        x := arg;
        IF x < 0.0 THEN
            x := -x;
            quad := quad + 2;
        END;
        x := x * twoopi;    (* underflow? *)
        IF ABS (x - FLOAT (TRUNC (x))) > 1.0 THEN
          IF (quad MOD 2) =0 THEN
            ExError(SineArg)
          ELSE
            ExError(CosineArg)
          END;
        END;
        k := TRUNC (x);
        y := x - FLOAT (k);
        quad := (quad + k) MOD 4;
        IF ODD (quad) THEN  y := 1.0 - y; END;
        IF quad > 1 THEN y := 0.0 - y; END;

        ysq := y*y;
        temp1 := ((((p4*ysq+p3)*ysq+p2)*ysq+p1)*ysq+p0)*y;
        temp2 := ((((ysq+q3)*ysq+q2)*ysq+q1)*ysq+q0);
        RETURN temp1/temp2;
    END sinus;

    PROCEDURE cos (arg: REAL): REAL;
    BEGIN
        IF arg < 0.0 THEN arg := 0.0 - arg END;
        RETURN sinus (arg, 1);
    END cos;

    PROCEDURE sin (arg: REAL): REAL;
    BEGIN
        RETURN sinus (arg, 0);
    END sin;

END SinCos;
(*
    PROCEDURE radians (degrees: REAL): REAL;
    BEGIN
        RETURN degrees * degreestoradians;
    END radians;

    PROCEDURE degrees (radians: REAL): REAL;
    BEGIN
        RETURN radians / degreestoradians;
    END degrees;
*)

MODULE ArcTan;

EXPORT atan,atan2;

    (* atan coefficients are #5077 from Hart & Cheney. (19.56D) *)

    CONST sq2p1  = 2.414213562373095048802e0;
    CONST sq2m1  = 0.414213562373095048802e0;
    CONST pio2   = 1.570796326794896619231e0;
    CONST pio4   = 0.785398163397448309615e0;
    CONST p4     = 0.161536412982230228262e2;
    CONST p3     = 0.26842548195503973794141e3;
    CONST p2     = 0.11530293515404850115428136e4;
    CONST p1     = 0.178040631643319697105464587e4;
    CONST p0     = 0.89678597403663861959987488e3;
    CONST q4     = 0.5895697050844462222791e2;
    CONST q3     = 0.536265374031215315104235e3;
    CONST q2     = 0.16667838148816337184521798e4;
    CONST q1     = 0.207933497444540981287275926e4;
    CONST q0     = 0.89678597403663861962481162e3;

    (* xatan evaluates a series valid in the range [-0.414...,+0.414...]. *)

    PROCEDURE xatan(arg: REAL): REAL;
        VAR argsq, value: REAL;
    BEGIN
        argsq := arg * arg;
        value := ((((p4 * argsq + p3) * argsq + p2) * argsq + p1) *
            argsq + p0);
        value := value / (((((argsq + q4) * argsq + q3) *
            argsq + q2) * argsq + q1) * argsq + q0);
        RETURN value*arg;
    END xatan;

    (* satan reduces its positive argument to the range [0,0.414...]
       and calls xatan.
    *)

    PROCEDURE satan (arg: REAL): REAL;
    BEGIN
        IF arg < sq2m1 THEN
            RETURN xatan(arg);
        ELSIF arg > sq2p1 THEN
            RETURN pio2 - xatan(1.0 / arg);
        ELSE RETURN pio4 + xatan ((arg - 1.0) / (arg + 1.0));
        END;
    END satan;

    (* atan makes its argument positive and calls satan. *)

    PROCEDURE atan (arg: REAL): REAL;
    BEGIN
        IF arg > 0.0 THEN
            RETURN satan (arg);
        ELSE RETURN 0.0 - satan (0.0 - arg);
        END;
    END atan;

    (* atan2 discovers what quadrant the angle is in and calls atan. *)

    PROCEDURE atan2 (arg1, arg2: REAL): REAL;
    BEGIN
        IF arg2 = 0.0  THEN
            IF arg1 >= 0.0 THEN
                 RETURN pio2;
            ELSE RETURN 0.0 - pio2;
            END;
        ELSIF arg2 < 0.0 THEN
            IF arg1 >= 0.0 THEN
                RETURN pio2 + pio2 - satan(0.0 - arg1/arg2);
            ELSE RETURN 0.0 - pio2 - pio2 + satan(arg1/arg2);
            END;
        ELSIF arg1 > 0.0 THEN
            RETURN satan(arg1/arg2);
        ELSE RETURN 0.0 - satan(0.0 - arg1/arg2);
        END;
    END atan2;
END ArcTan;

    CONST pio2   = 1.570796326794896619231e0;
         mpio2   =-1.570796326794896619231e0;

PROCEDURE acos(x:REAL):REAL;
BEGIN
  IF x=0.     THEN RETURN pio2;
  ELSIF x=1.  THEN RETURN 0.;
  ELSIF x=-1. THEN RETURN pi;
  ELSIF ABS(x)<1. THEN
    IF x>0. THEN   RETURN atan(sqrt(1.-x*x)/x);
    ELSE           RETURN atan(sqrt(1.-x*x)/x) + pi;
    END;
  ELSE ExError(AcosArg); RETURN 0.
  END;
END acos;

PROCEDURE asin(x:REAL):REAL;
BEGIN
  IF    x=0.      THEN RETURN 0.;
  ELSIF x=1.      THEN RETURN pio2;
  ELSIF x=-1.     THEN RETURN mpio2;
  ELSIF ABS(x)<1. THEN RETURN atan(x/sqrt(1.-x*x));
  ELSE ExError(AsinArg);    RETURN 0.
  END;
END asin;

PROCEDURE tan(x:REAL):REAL;
  VAR t:REAL;
BEGIN
  t:=cos(x);
  IF t=0.  THEN ExError(TanArg); RETURN 0.
  ELSE                       RETURN sin(x)/t;
  END;
END tan;

PROCEDURE cosh(x:REAL):REAL;
BEGIN
  RETURN (exp(x)+exp(-x))*0.5
END cosh;

PROCEDURE sinh(x:REAL):REAL;
BEGIN
  RETURN (exp(x)-exp(-x))*0.5
END sinh;

PROCEDURE tanh(x:REAL):REAL;
  VAR t:REAL;
BEGIN
  t:=exp(2.0*x);
  RETURN (t-1.0)/(t+1.0)
END tanh;

PROCEDURE log10(x:REAL):REAL;
CONST log10e=0.434294481;
BEGIN
  RETURN log10e*ln(x);
END log10;

-- Complex functions --

PROCEDURE Cxabs(re,im:REAL):REAL;
BEGIN
  RETURN sqrt(re*re+im*im);
END Cxabs;

PROCEDURE Cxsqrt(re,im:REAL);
  VAR root,q:REAL;
BEGIN
  root:=sqrt((ABS(re)+Cxabs(re,im))*0.5);
  q:=im/(2.0*root);
  IF    re>=0. THEN PushW(root); PushW(q);
  ELSIF im>=0. THEN PushW(q);    PushW(root);
  ELSE              PushW(-q);   PushW(-root);
  END;
END Cxsqrt;

PROCEDURE Cxlog(re,im:REAL);
  VAR r,i:REAL;
BEGIN
  r:=ln(Cxabs(re,im));
  i:=atan2(re,im);
  PushW(r); PushW(i);
END Cxlog;

PROCEDURE Cxexp(re,im:REAL);
  VAR r,i,Exp:REAL;
BEGIN
  Exp:=exp(re);
  r:=Exp*cos(im);
  i:=Exp*sin(im);
  PushW(r); PushW(i);
END Cxexp;

PROCEDURE Cxsin(re,im:REAL);
  VAR r,i,Exp,Exp1:REAL;
BEGIN
  Exp:=exp(im); Exp1:=1./Exp;
  r:=sin(re)*(Exp+Exp1)*0.5;
  i:=cos(re)*(Exp-Exp1)*0.5;
  PushW(r); PushW(i);
END Cxsin;

PROCEDURE Cxcos(re,im:REAL);
  VAR r,i,Exp,Exp1:REAL;
BEGIN
  Exp:=exp(im); Exp1:=1./Exp;
  r:= cos(re)*(Exp+Exp1)/2.;
  i:=-sin(re)*(Exp-Exp1)/2.;
  PushW(r); PushW(i);
END Cxcos;

PROCEDURE Cxmul;
CODE
 entr 4
 slw7 slw6 slw5 copt slw4
 llw6 fmul
 llw5 llw7 fmul fsub
 llw4 llw7 fmul
 llw5 llw6 fmul fadd
END Cxmul;

PROCEDURE Cxdiv;
CODE
 entr 5
 slw7 slw6 slw5 slw4
 llw6 copt fmul
 llw7 copt fmul fadd slw8
 llw4 llw6 fmul
 llw5 llw7 fmul fadd llw8 fdiv
 llw5 llw6 fmul
 llw4 llw7 fmul fsub llw8 fdiv
END Cxdiv;

PROCEDURE CxMul;
BEGIN
 Cxmul;
END CxMul;

PROCEDURE CxDiv;
BEGIN
 Cxdiv;
END CxDiv;

PROCEDURE Powii;
CODE
 entr 2 slw5 slw4
 llw5   li0  lss
 jfsc 2
 li0    rtn
 li1
 llw5   jfsc 7
 llw4   mul
 lla 5  dec1
 jbs 10
END Powii;

PROCEDURE Powerii;
BEGIN
 Powii;
END Powerii;

PROCEDURE powri(x:REAL; n:INTEGER):REAL;
 VAR i: INTEGER;  neg:BOOLEAN; res:REAL;
BEGIN
   IF n<0 THEN neg:=TRUE; n:=-n ELSE neg:=FALSE END;
   res:=1.;
   WHILE n>0 DO res:=res*x; DEC(n) END;
   IF neg THEN RETURN 1./res
   ELSE        RETURN res
   END;
END powri;

PROCEDURE Powri;
CODE
 store llw5 llw4 cl 41
 lodt li2 sub stot lodfv
END Powri;

PROCEDURE Powerri;
BEGIN
 Powri;
END Powerri;

PROCEDURE Powerdi;
BEGIN
  Powri;
END Powerdi;

PROCEDURE Powerci;
BEGIN
END Powerci;

PROCEDURE powrr(x,r:REAL):REAL;
BEGIN
 RETURN exp(r*ln(x));
END powrr;

PROCEDURE Powrr;
CODE
 store llw5 llw4 cl 42
 lodt li2 sub stot lodfv
END Powrr;

PROCEDURE Powerrr;
BEGIN
 Powrr;
END Powerrr;

PROCEDURE Powerdd;
BEGIN
  Powrr;
END Powerdd;

PROCEDURE Powercc;
BEGIN
END Powercc;

PROCEDURE Max0(i,j:INTEGER):INTEGER;
BEGIN
  IF i>=j THEN RETURN i
  ELSE         RETURN j
  END;
END Max0;

PROCEDURE Amax1(i,j:REAL):REAL;
BEGIN
  IF i>=j THEN RETURN i
  ELSE         RETURN j
  END;
END Amax1;

PROCEDURE Min0(i,j:INTEGER):INTEGER;
BEGIN
  IF i<=j THEN RETURN i
  ELSE         RETURN j
  END;
END Min0;

PROCEDURE Amin1(i,j:REAL):REAL;
BEGIN
  IF i<=j THEN RETURN i
  ELSE         RETURN j
  END;
END Amin1;

PROCEDURE Missing;
BEGIN
  print(' вызов отсутствующей процедуры!!! \n');
  HALT;
END Missing;

PROCEDURE Dim(a,b:REAL):REAL;
BEGIN
 IF a>b THEN RETURN (a-b);
 ELSE        RETURN 0.;
 END
END Dim;

PROCEDURE iDim(a,b:INTEGER):INTEGER;
BEGIN
 IF a>b THEN RETURN (a-b);
 ELSE        RETURN 0;
 END
END iDim;

PROCEDURE AMod(a,b:REAL):REAL;
BEGIN
 RETURN (a-FLOAT(TRUNC(a/b))*b);
END AMod;

PROCEDURE iSign(a,b:INTEGER):INTEGER;
BEGIN
 IF b>=0 THEN RETURN ABS(a);
 ELSE         RETURN -ABS(a);
 END
END iSign;

PROCEDURE Sign(a,b:REAL):REAL;
BEGIN
 IF b>=0. THEN RETURN ABS(a);
 ELSE          RETURN -ABS(a);
 END
END Sign;

BEGIN
 chtop:=0; chbase:=ADR(chstack);
 degreestoradians := (2.0 * pi) / 360.0;

END ForLib.
