IMPLEMENTATION MODULE VIDEO; (* Leo 06-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM Strings    IMPORT  Len, DelStr, Truncate;
FROM Image      IMPORT  image0;
FROM Misc       IMPORT  Min, Max;

PROCEDURE setclip(x0,y0,x1,y1: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
  IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
  clipW:=Max(0,Min(Dots-1,x0));
  clipS:=Max(0,Min(Lins-1,y0));
  clipE:=Max(0,Min(Dots-1,x1));
  clipN:=Max(0,Min(Lins-1,y1));
END setclip;

PROCEDURE color(i: INTEGER);
BEGIN
  IF (i>=0) & (i<Colors) THEN Color:=i END;
END color;

PROCEDURE mode(m: INTEGER);
BEGIN
  Mode:=m;
END mode;

PROCEDURE patt(p: INTEGER);
BEGIN
END patt;

PROCEDURE vect(x0,y0,x1,y1: INTEGER);
BEGIN

END vect;

PROCEDURE dot(x,y: INTEGER);
BEGIN

END dot;

PROCEDURE frame(x0,y0,x1,y1: INTEGER);
BEGIN
  IF ABS(x0-x1)>=2 THEN
    vect(x1-1,y1,x0+1,y1);
    IF y0#y1 THEN vect(x0+1,y0,x1-1,y0) END;
  END;
  vect(x1  ,y0,x1  ,y1);
  IF x0#x1 THEN vect(x0  ,y1,x0  ,y0) END;
END frame;

PROCEDURE rect(x0,y0,x1,y1: INTEGER);
BEGIN

END rect;

PROCEDURE print(ln,cl: INTEGER; format: ARRAY OF CHAR; SEQ args: WORD);
  VAR s: ARRAY [0..83] OF CHAR;
BEGIN
  IF (ln<0) OR (ln>3) THEN RETURN END;
  image0(s,format,args);
  IF Len(s)>Dots DIV charszX - cl THEN
    Truncate(s,Dots DIV charszX - cl);
  END;
  TV.WriteString(ln,cl,s);
END print;

PROCEDURE circ(x,y,r: INTEGER);
BEGIN

END circ;

PROCEDURE fill(i: INTEGER);
  VAR p: ADDRESS; j: INTEGER;
BEGIN
  IF (i<0) & (i>=Colors) THEN i:=0 END;
  p:=base;
  FOR j:=0 TO 359 DO
    IF 0 IN BITSET(j) THEN p^:=-1 ELSE p^:=0 END;
    MOVE(p+1,p,14); INC(p,16*512);
    IF 1 IN BITSET(j) THEN p^:=-1 ELSE p^:=0 END;
    MOVE(p+1,p,14); INC(p,16*512);
    IF 2 IN BITSET(j) THEN p^:=-1 ELSE p^:=0 END;
    MOVE(p+1,p,14); INC(p,16*512);
    IF 3 IN BITSET(j) THEN p^:=-1 ELSE p^:=0 END;
    MOVE(p+1,p,14); DEC(p,(3*512-1)*16);
  END;
END fill;

PROCEDURE rollN(n: INTEGER);
BEGIN

END rollN;

PROCEDURE rollE(n: INTEGER);
BEGIN

END rollE;

PROCEDURE rollS(n: INTEGER);
BEGIN

END rollS;

PROCEDURE rollW(n: INTEGER);
BEGIN

END rollW;

PROCEDURE cursor(x,y,no: INTEGER);
BEGIN
  IF (x<clipW) OR (x>clipE) OR (y<clipS) OR (y>clipN) THEN RETURN END;
  TV.pos(x,y); TV.cursor(no MOD Cursors)
END cursor;

PROCEDURE start;
BEGIN
  IF Activ THEN RETURN END;
  Colors:=16;
  Lines :=360; Dots :=480;
  dotszY:=1;   Color:=4;
  dotszX:=1;
  charszY:=12;
  charszX:=10;  Mode:=rep;
  rollX  :=10; rollY:=12;
  Cursors:=1;
  Activ:=TRUE;
  setclip(0,0,Dots-1,Lines-1-4*charszY);
END start;

PROCEDURE finish;
BEGIN
  Activ:=FALSE;
END finish;

BEGIN
  Activ:=FALSE;
END VIDEO.
