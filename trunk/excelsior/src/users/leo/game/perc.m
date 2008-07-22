MODULE perc; (* Dima 14-Jun-88. (c) KRONOS *)

FROM Random     IMPORT  RanGe;
FROM Args       IMPORT  Num?;
FROM bitmap     IMPORT  fillL, fillH, oneL, oneH, revealL, revealH, wait;
--              IMPORT  ConsolIO;
                IMPORT  Resource;

VAR p   : INTEGER;
    show: ARRAY [0..24] OF BITSET;
    a   : ARRAY [0..799] OF BOOLEAN;

CONST one=1000000;

PROCEDURE go?(): BOOLEAN;
BEGIN
  RETURN RanGe(0,one-1)<p;
END go?;

PROCEDURE incl(VAR row: ARRAY OF BITSET; i: INTEGER);
BEGIN INCL(row[i DIV 32], i MOD 32);
END incl;

PROCEDURE step(VAR start: INTEGER): BOOLEAN;
  VAR i: INTEGER; emp: BOOLEAN;
BEGIN
  emp:=TRUE;
  FOR i:=0 TO HIGH(show) DO show[i]:={} END;
  FOR i:=start TO HIGH(a)-1 BY 2 DO
    IF (a[i-1] & go?()) OR (a[i+1] & go?()) THEN
      a[i]:=TRUE; incl(show, i); emp:=FALSE;
    ELSE a[i]:=FALSE;
    END;
  END;
  IF a[start] & (start>1) THEN DEC(start) ELSE INC(start) END;
  RETURN emp
END step;

PROCEDURE out(n: INTEGER);
  VAR i: INTEGER;
BEGIN   FOR i:=0 TO HIGH(show) DO oneH(n,i,show[i]) END;
END out;


VAR P, n, n10, i,start: INTEGER;

BEGIN
--Resource.Final(ConsolIO.clear);
  IF Num?(P) OR (P<0) OR (P>10000) THEN P:=6350 END;
  p:=P*(one DIV 10000);
  start:=250;
  FOR i:=0 TO HIGH(show) DO show[i]:={} END;
  FOR i:=0 TO HIGH(a) DO a[i]:=FALSE END;
  FOR i:=start TO 800-start BY 2 DO a[i]:=TRUE; incl(show, i) END;
  DEC(start);
    fillL({}); fillH({});
  LOOP
    n:=0; n10:=0;
    REPEAT
      out(n); INC(n);
      IF n10=10 THEN n10:=0; revealH; wait; ELSE INC(n10) END;
    UNTIL step(start) OR (n>=300);
    IF n<300 THEN EXIT END;
  END;
END perc.
