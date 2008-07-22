IMPLEMENTATION MODULE Random; (* Ned 15-Jan-90. (c) KRONOS *)

CONST
  max=32767;
  _a =30000;
  _b =17000;

VAR a,b,c: INTEGER;

PROCEDURE next(): INTEGER;
BEGIN
  c:=a+b;
  IF c>max THEN DEC(c,max+1) END;
  c:=c*2;
  IF c>max THEN DEC(c,max) END;
  a:=b;
  b:=c;
  RETURN c
END next;

PROCEDURE reset;
BEGIN a:=_a; b:=_b;
END reset;

BEGIN a:=_a; b:=_b;
END Random.
