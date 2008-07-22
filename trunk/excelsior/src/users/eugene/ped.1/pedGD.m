MODULE pedGD; (* Sem 08-Dec-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;
--FROM LabtamVDU  IMPORT  layer, Line;

FROM MathLib0   IMPORT  sqrt;
FROM Terminal   IMPORT  print;

PROCEDURE SQRT(n:INTEGER): INTEGER;
  VAR l,r: INTEGER;
BEGIN
  IF n<2 THEN RETURN n END;
  l:=1; r:=n; REPEAT r:=(l+r) DIV 2; l:=n DIV r UNTIL l>=r;
  RETURN r
END SQRT;


PROCEDURE seg(x1,y1,x2,y2,s: INTEGER);
BEGIN


END seg;

VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 20 DO
    print('%4d %4d %10g\n',i,SQRT(i),sqrt(FLOAT(i)));
  END;
END pedGD.
