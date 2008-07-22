IMPLEMENTATION MODULE Poly; (* Dima 18-Jun-88. (c) KRONOS *)

                IMPORT  Heap, Strings, Image;
                IMPORT  Resource, Terminal;

TYPE poly  = POINTER TO monom;
     monom = RECORD next: poly;
               CASE :BOOLEAN OF
                 |TRUE : deg: INTEGER; coef: REAL;
                 |FALSE: len: INTEGER; tag : INTEGER;
               END;
             END;

CONST temp=123456789; cons=-temp; -- poly^.tag = +-temp !

PROCEDURE check(p: poly);
BEGIN ASSERT((p#NIL) & (ABS(p^.tag)=temp));
END check;

MODULE Memory;
                IMPORT  poly, temp, check;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;

EXPORT newm, newp, dropm, dropp, alloc;

VAR free: poly;
   alloc: INTEGER;

PROCEDURE newm(VAR m: poly);
BEGIN
  IF free=NIL THEN ALLOCATE(m, SIZE(m^)); ASSERT(m#NIL); INC(alloc);
  ELSE m:=free; free:=free^.next;
  END;
  m^.next:=NIL;
END newm;

PROCEDURE newp(VAR p: poly);
BEGIN
  IF free=NIL THEN ALLOCATE(p, SIZE(p^)); ASSERT(p#NIL); INC(alloc);
  ELSE p:=free; free:=free^.next;
  END;
  p^.next:=NIL; p^.tag:=temp;
END newp;

PROCEDURE dropm(VAR m: poly);
  VAR f: poly;
BEGIN IF m=NIL THEN RETURN END;
  f:=m;
  WHILE f^.next#NIL DO f:=f^.next END;
  f^.next:=free; free:=m; m:=NIL
END dropm;

PROCEDURE dropp(VAR m: poly);
  VAR f: poly;
BEGIN IF m^.tag<0 THEN RETURN END;
  f:=m;
  WHILE f^.next#NIL DO f:=f^.next END;
  f^.next:=free; free:=m; m:=NIL
END dropp;

BEGIN free:=NIL; alloc:=0;
END Memory;

PROCEDURE drop(VAR p: poly);
BEGIN check(p); dropm(p)
END drop;

PROCEDURE tmp(p: poly);
BEGIN check(p); p^.tag:=temp;
END tmp;

PROCEDURE !(SEQ coef: REAL): poly;
  VAR p: poly; d, i: INTEGER; m: poly;
BEGIN newp(p);
  m:=p; d:=HIGH(coef); i:=0;
  WHILE d>=0 DO
    IF coef[i]#0. THEN
      newm(m^.next); m:=m^.next;  m^.deg:=d; m^.coef:=coef[i];
    END;
    DEC(d); INC(i);
  END;
  RETURN p
END !;

PROCEDURE mono(deg: INTEGER; coef: REAL): poly;
  VAR p: poly;
BEGIN newp(p);
  IF (coef#0.) & (deg>=0) THEN
    newm(p^.next);
    p^.next^.deg:=deg; p^.next^.coef:=coef;
  END;
  RETURN p
END mono;

PROCEDURE asg(VAR x: poly; y: poly);
  VAR mx, my: poly;
BEGIN check(x); check(y);
  IF y^.tag=temp THEN
    dropm(x); x:=y; x^.tag:=cons; RETURN
  END;
  x^.tag:=cons;
  mx:=x; my:=y^.next;
  WHILE my#NIL DO
    IF mx^.next=NIL THEN newm(mx^.next) END;  mx:=mx^.next;
    mx^.deg:=my^.deg; mx^.coef:=my^.coef;
    my:=my^.next;
  END;
  dropm(mx^.next); dropp(y);
END asg;

PROCEDURE ini(VAR p: poly; SEQ coef: REAL); -- ini(p, 1.,0.,3.) p=x^2+3
BEGIN newp(p); asg(p, !(coef))
END ini;

PROCEDURE add(p1,p2: poly): poly;
  VAR p: poly; mp, m1, m2: poly; d, d1, d2: INTEGER; c: REAL;
BEGIN check(p1); check(p2); newp(p);
  m1:=p1^.next; m2:=p2^.next; mp:=p;
  WHILE (m1#NIL) OR (m2#NIL) DO
    IF m1#NIL THEN d1:=m1^.deg ELSE d1:=MIN(INTEGER) END;
    IF m2#NIL THEN d2:=m2^.deg ELSE d2:=MIN(INTEGER) END;
    c:=0.;
    IF d1>=d2 THEN c:=m1^.coef; m1:=m1^.next; d:=d1; END;
    IF d2>=d1 THEN c:=c+m2^.coef; m2:=m2^.next; d:=d2; END;
    IF c#0. THEN
      newm(mp^.next); mp:=mp^.next;
      mp^.deg:=d; mp^.coef:=c;
    END;
  END;
  dropp(p1); dropp(p2); RETURN p
END add;

PROCEDURE sub(p1,p2: poly): poly;
  VAR p: poly; mp, m1, m2: poly; d, d1, d2: INTEGER; c: REAL;
BEGIN check(p1); check(p2); newp(p);
  m1:=p1^.next; m2:=p2^.next; mp:=p;
  WHILE (m1#NIL) OR (m2#NIL) DO
    IF m1#NIL THEN d1:=m1^.deg ELSE d1:=MIN(INTEGER) END;
    IF m2#NIL THEN d2:=m2^.deg ELSE d2:=MIN(INTEGER) END;
    c:=0.;
    IF d1>=d2 THEN c:=m1^.coef; m1:=m1^.next; d:=d1; END;
    IF d2>=d1 THEN c:=c-m2^.coef; m2:=m2^.next; d:=d2; END;
    IF c#0. THEN
      newm(mp^.next); mp:=mp^.next;
      mp^.deg:=d; mp^.coef:=c;
    END;
  END;
  dropp(p1); dropp(p2); RETURN p
END sub;

PROCEDURE mul1(y: poly; m: poly): poly;
  VAR x: poly; mx, my: poly;
BEGIN newp(x);
  mx:=x; my:=y^.next;
  WHILE my#NIL DO
    newm(mx^.next); mx:=mx^.next;
    mx^.deg:=my^.deg+m^.deg; mx^.coef:=my^.coef*m^.coef;
    my:=my^.next;
  END;
  RETURN x
END mul1;

PROCEDURE mul(p1,p2: poly): poly;
  VAR p: poly; m: poly;
BEGIN check(p1); check(p2); newp(p);
 p^.tag:=cons;
  IF (p1^.next#NIL) & (p2^.next#NIL) THEN
    m:=p2^.next;
    REPEAT asg(p, add(p, mul1(p1, m)));
      m:=m^.next;
    UNTIL m=NIL;
  END;
 p^.tag:=temp;
  dropp(p1); dropp(p2); RETURN p
END mul;

PROCEDURE eq(p1,p2: poly): BOOLEAN;
  VAR true: BOOLEAN; m1, m2: poly;
BEGIN check(p1); check(p2);
  m1:=p1^.next; m2:=p2^.next;
  LOOP
    IF (m1=NIL) OR (m2=NIL) THEN true:=(m1=m2); EXIT END;
    IF (m1^.deg#m2^.deg) OR (m1^.coef#m2^.coef) THEN true:=FALSE; EXIT END;
    m1:=m1^.next; m2:=m2^.next;
  END;
  dropp(p1); dropp(p2); RETURN true
END eq;

PROCEDURE eval(p: poly; x: REAL): REAL;
  VAR d,b: INTEGER; v: REAL; m: poly;
BEGIN check(p);
  m:=p^.next;
  IF m=NIL THEN dropp(p); RETURN 0. END;
  d:=m^.deg; v:=0.;
  REPEAT
    v:=v+m^.coef; m:=m^.next;
    IF m=NIL THEN b:=0 ELSE b:=m^.deg END;
    WHILE d>b DO v:=v*x; DEC(d) END;
  UNTIL m=NIL;
  dropp(p); RETURN v
END eval;

PROCEDURE deg?(z: poly): INTEGER;
  VAR d: INTEGER;
BEGIN check(z);
  IF z^.next=NIL THEN d:=-1 ELSE d:=z^.next^.deg END;
  dropp(z); RETURN d
END deg?;

PROCEDURE coef?(z: poly; deg: INTEGER): REAL;
  VAR m: poly; c: REAL;
BEGIN check(z);
  m:=z^.next;
  WHILE (m#NIL) & (deg<m^.deg) DO m:=m^.next END;
  IF (m=NIL) OR (deg#m^.deg) THEN c:=0. ELSE c:=m^.coef END;
  dropp(z); RETURN c
END coef?;

MODULE Print;
                IMPORT  poly, check, dropp;
FROM Strings    IMPORT  Str2, App;
FROM Image      IMPORT  image;

EXPORT appoly;

PROCEDURE append(VAR s: ARRAY OF CHAR; vari,sign: CHAR; p: poly);

  PROCEDURE body(m: poly);
  BEGIN
    IF (ABS(m^.coef)#1.)OR(m^.deg=0) THEN image(s,'%g',ABS(m^.coef)) END;
    IF m^.deg#0 THEN App(s, vari) END;
    IF m^.deg>1 THEN
      IF sign#' ' THEN App(s, sign) END;
      image(s, '%d', m^.deg);
    END;
  END body;

  VAR m: poly;
BEGIN check(p);
  Str2(s); m:=p^.next;
  IF    m=NIL THEN App(s, '0');
  ELSIF m^.coef<0. THEN App(s, '-');
  END;
  WHILE m#NIL DO
    body(m); m:=m^.next;
    IF m#NIL THEN
      IF m^.coef>0. THEN App(s,'+') ELSE App(s,'-') END;
    END;
  END;
  dropp(p);
END append;

PROCEDURE appoly(VAR s: ARRAY OF CHAR; fmt: ARRAY OF CHAR; SEQ arg: poly);
  VAR fc, ac   : INTEGER;
      vari,sign: CHAR;

  VAR ch: CHAR;
BEGIN fc:=0; ac:=0; Str2(s);
  WHILE (fc<=HIGH(fmt)) & (fmt[fc]#0c) DO ch:=fmt[fc];
    IF ch='%' THEN
      INC(fc); ch:=fmt[fc];
      IF ch='%' THEN App(s, '%'); INC(fc);
      ELSE
        IF ch='p' THEN vari:='x'; sign:='^'; INC(fc);
        ELSE sign:=ch; vari:=fmt[fc+1]; INC(fc,3)
        END;
        append(s, vari,sign, arg[ac]); INC(ac)
      END;
    ELSE App(s, ch); INC(fc)
    END;
  END;
END appoly;

END Print;

PROCEDURE finish;
BEGIN
  Terminal.print('\n%d allocations\n', alloc);
END finish;

BEGIN Resource.Final(finish);
END Poly.

VAR p,q: poly;
    i  : INTEGER;
    s,f: ARRAY [0..255] OF CHAR;

BEGIN
  ini(p, 1., 2., 3., -4.);
  f:='F(z)=% zp; F(%%g)=%%g'; s:='';
  appoly(s, f, p);
  print(s, 0.6, eval(p, 0.6));
