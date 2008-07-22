MODULE xxx;
                IMPORT  Poly;
FROM Poly       IMPORT  poly, ini, !, asg, tmp, drop
                      , add, sub, mul, eval, appoly;
FROM StdIO      IMPORT  print;
FROM Image      IMPORT  image, image0;
FROM SYSTEM     IMPORT  ADR;

CONST level=4;

TYPE frac = RECORD up, dw: poly END;

VAR elp: ARRAY [0..3],[0..1] OF poly;
    eq : ARRAY [0..INTEGER({level-1})-1],[0..INTEGER({level})-1] OF poly;

PROCEDURE initelp;
BEGIN
  ini(elp[0,0],  1.    );     ini(elp[0,1], 0.    );
  ini(elp[1,0], -1., 1.);     ini(elp[1,1], 1., 0.);
  ini(elp[2,0], -1., 1.);     ini(elp[2,1], 1., 0.);
  ini(elp[3,0],  1.,-2., 1.); ini(elp[3,1],-1., 2., 0.);
END initelp;

PROCEDURE highbit(x: INTEGER): INTEGER;
  VAR n: INTEGER;
BEGIN
  n:=0; WHILE x>=INTEGER({n}) DO INC(n) END; RETURN n-1
END highbit;

PROCEDURE prob(from,to: INTEGER): poly;
  VAR i,n: INTEGER; p: poly;
BEGIN ASSERT(ODD(from));
  n:=highbit(from)+1;
  ini(p, 1.);
  from:=from*2;
  FOR i:=0 TO n DO
    asg(p, mul(p, elp[from MOD 4, to MOD 2]));
    from:=from DIV 2; to:=to DIV 2;
  END;
  tmp(p); RETURN p
END prob;

PROCEDURE canon(n: INTEGER): INTEGER;
  VAR i,j: INTEGER;
BEGIN ASSERT(n#0);
  j:=0;  WHILE NOT ODD(n) DO n:=n DIV 2 END;
  i:=highbit(n);
  WHILE (i>j) & ((i IN BITSET(n))=(j IN BITSET(n))) DO INC(j); DEC(i) END;
  IF    (i>j) &  (i IN BITSET(n)) THEN
    WHILE (i>j) DO
      IF (i IN BITSET(n))#(j IN BITSET(n)) THEN
        n:=INTEGER(BITSET(n)/{i,j});
      END;
      INC(j); DEC(i);
    END;
  END;
  RETURN n
END canon;

PROCEDURE initeq;
  VAR i,j,n: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(eq) DO FOR j:=0 TO HIGH(eq[0]) DO ini(eq[i,j]) END END;
  FOR i:=1 TO HIGH(eq) BY 2 DO
    FOR j:=1 TO INTEGER({highbit(i)+2})-1 DO n:=canon(j);
      asg(eq[i,n], add(eq[i,n], prob(i,j)));
    END;
  END;
END initeq;



VAR x: REAL;
    s: ARRAY [0..255] OF CHAR;

BEGIN
  initelp;
  initeq;
  s:=''; appoly(s, 'P1=(%p)P1+(%p)P11', eq[1,1],eq[1,3]);
  print('%s\n',s);
  s:=''; appoly(s, 'P11=(%p)P1+(%p)P11+(%p)P101+(%p)P111',
                       eq[3,1],eq[3,3], eq[3,5], eq[3,7]);
  print(s);
--  solvesystem;
--  findzero;
END xxx.
