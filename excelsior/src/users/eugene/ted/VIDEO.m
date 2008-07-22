IMPLEMENTATION MODULE VIDEO; (* Leo 06-Nov-87. (c) KRONOS *)

IMPORT TV;
FROM SYSTEM     IMPORT  WORD;
FROM Strings    IMPORT  Len, DelStr, Truncate;
FROM Image      IMPORT  image0;
FROM Misc       IMPORT  Min, Max;

PROCEDURE setclip(x0,y0,x1,y1: INTEGER);
  VAR i,Lins: INTEGER;
BEGIN
  Lins:=Lines-4*charszY;
  IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
  IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
  clipW:=Max(0,Min(Dots-1,x0));
  clipS:=Max(0,Min(Lins-1,y0));
  clipE:=Max(0,Min(Dots-1,x1));
  clipN:=Max(0,Min(Lins-1,y1));
END setclip;

PROCEDURE color(i: INTEGER);
BEGIN
  IF (i>=0) & (i<Colors) THEN
    TV.color(i);
  END;
END color;

PROCEDURE mode(m: INTEGER); BEGIN TV.mode(m) END mode;

PROCEDURE patt(p: INTEGER); BEGIN TV.patt(p) END patt;

PROCEDURE vect(x0,y0,x1,y1: INTEGER);
  VAR N,E: INTEGER;
      S,W: INTEGER;     x0lssW,x0gtrE,x1lssW,x1gtrE: BITSET;
    a,b,c: INTEGER;     y0lssS,y0gtrN,y1lssS,y1gtrN: BITSET;
      x,y: INTEGER;                             out: BOOLEAN;

(*
  PROCEDURE clip(VAR x0,y0: INTEGER): BOOLEAN;
    VAR x,y: INTEGER;
  BEGIN
    IF BOOLEAN(x0lssW) THEN x:=W; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    IF BOOLEAN(y0lssS) THEN y:=S; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    IF BOOLEAN(x0gtrE) THEN x:=E; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    IF BOOLEAN(y0gtrN) THEN y:=N; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    RETURN TRUE
  END clip;
*)

TYPE !=BITSET;

BEGIN N:=clipN; E:=clipE; S:=clipS; W:=clipW;

  x0lssW:=!(x0<W);      x0gtrE:=!(x0>E);
  x1lssW:=!(x1<W);      x1gtrE:=!(x1>E);
  y0lssS:=!(y0<S);      y0gtrN:=!(y0>N);
  y1lssS:=!(y1<S);      y1gtrN:=!(y1>N);

--IF (x0<W) & (x1<W) OR (y0<S) & (y1<S) OR (x0>E) & (x1>E) OR (y0>N) & (y1>N)
--THEN RETURN END;

  IF (x0lssW*x1lssW)+(y0lssS*y1lssS)+(x0gtrE*x1gtrE)+(y0gtrN*y1gtrN)#{} THEN
    RETURN
  END;

  IF NOT BOOLEAN(x0lssW+x0gtrE+x1lssW+x1gtrE+y0lssS+y0gtrN+y1lssS+y1gtrN) THEN
    TV.pos(x0,y0); TV.vect(x1,y1); RETURN
  END;

--IF (W<=x0) & (x0<=E) & (W<=x1) & (x1<=E)
-- & (S<=y0) & (y0<=N) & (S<=y1) & (y1<=N) THEN -- in room
--  TV.pos(x0,y0); TV.vect(x1,y1); RETURN
--END;

  c:=(x0*y1-x1*y0);   -- a/c*x+b/c*y=1;
  a:=(y1-y0);         -- a*x+b*y=c;
  b:=(x0-x1);

--IF ((x0<W) OR (x0>E) OR (y0<S) OR (y0>N)) & clip(x0,y0) THEN RETURN END;
--IF ((x1<W) OR (x1>E) OR (y1<S) OR (y1>N)) & clip(x1,y1) THEN RETURN END;

  IF BOOLEAN(x0lssW+x0gtrE+y0lssS+y0gtrN) THEN out:=TRUE;
    IF BOOLEAN(x0lssW) THEN x:=W; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x0:=x; y0:=y; out:=FALSE END;
    END;
    IF BOOLEAN(y0lssS) THEN y:=S; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x0:=x; y0:=y; out:=FALSE END;
    END;
    IF BOOLEAN(x0gtrE) THEN x:=E; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x0:=x; y0:=y; out:=FALSE END;
    END;
    IF BOOLEAN(y0gtrN) THEN y:=N; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x0:=x; y0:=y; out:=FALSE END;
    END;
    IF out THEN RETURN END;
  END;
  IF BOOLEAN(x1lssW+x1gtrE+y1lssS+y1gtrN) THEN out:=TRUE;
    IF BOOLEAN(x1lssW) THEN x:=W; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x1:=x; y1:=y; out:=FALSE END;
    END;
    IF BOOLEAN(y1lssS) THEN y:=S; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x1:=x; y1:=y; out:=FALSE END;
    END;
    IF BOOLEAN(x1gtrE) THEN x:=E; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x1:=x; y1:=y; out:=FALSE END;
    END;
    IF BOOLEAN(y1gtrN) THEN y:=N; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x1:=x; y1:=y; out:=FALSE END;
    END;
    IF out THEN RETURN END;
  END;
(*
  ASSERT((W<=x0) & (x0<=E));    ASSERT((S<=y0) & (y0<=N));
  ASSERT((W<=x1) & (x1<=E));    ASSERT((S<=y1) & (y1<=N));
*)

  TV.pos (x0,y0);
  TV.vect(x1,y1);

END vect;

PROCEDURE dot(x,y: INTEGER);
BEGIN
  IF (clipW<=x) & (x<=clipE) & (clipS<=y) & (y<=clipN) THEN
    TV.pos(x,y); TV.vect(x,y);
  END;
END dot;

PROCEDURE frame(x0,y0,x1,y1: INTEGER);
BEGIN
  vect(x0+1,y0,x1-1,y0);
  vect(x1  ,y0,x1  ,y1);
  vect(x1-1,y1,x0+1,y1);
  vect(x0  ,y1,x0  ,y0);
END frame;

PROCEDURE rect(x0,y0,x1,y1: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
  IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
  ASSERT((y0<=y1) & (x0<=x1));
  IF (y0=y1) OR (x0=x1) THEN vect(x0,y0,x1,y1); RETURN END;
  IF (y0>clipN) OR (x0>clipE) OR (y1<clipS) OR (x1<clipW) THEN RETURN END;
  IF y0<clipS THEN y0:=clipS END;
  IF x0<clipW THEN x0:=clipW END;
  IF y1>clipN THEN y1:=clipN END;
  IF x1>clipE THEN x1:=clipE END;
  TV.pos(x0,y0);
  TV.rect(x1,y1);
END rect;

PROCEDURE trapez(x0,x1,x2,x3,y0,y1: INTEGER);
  VAR y,x: INTEGER;
BEGIN
  IF (y0>clipN) OR (y1<clipS) THEN RETURN END;
  IF (x0>clipE) & (x1>clipE) THEN RETURN END;
  IF (x2<clipW) & (x3<clipW) THEN RETURN END;
  IF y1>clipN THEN
    x1:=x0+(x1-x0)*(clipN-y0) DIV (y1-y0);
    x2:=x3+(x2-x3)*(clipN-y0) DIV (y1-y0);
    y1:=clipN;
  END;
  IF y0<clipS THEN
    x0:=x0+(x1-x0)*(clipS-y0) DIV (y1-y0);
    x3:=x3+(x2-x3)*(clipS-y0) DIV (y1-y0);
    y0:=clipS;
  END;
  IF (x0<clipW) & (x1<clipW) THEN
    x0:=clipW; x1:=clipW;
  ELSIF x0<clipW THEN
    y:=y0+(clipW-x0)*(y1-y0) DIV (x1-x0);
    x:=x3+(x2-x3)*(y-y0) DIV (y1-y0);
    trapez(clipW,x1,x2,x,y,y1);
    x:=x3+(x2-x3)*(y-1-y0) DIV (y1-y0);
    trapez(clipW,clipW,x,x3,y0,y-1);
    RETURN
  ELSIF x1<clipW THEN
    y:=y0+(clipW-x0)*(y1-y0) DIV (x1-x0);
    x:=x3+(x2-x3)*(y-y0) DIV (y1-y0);
    trapez(x0,clipW,x,x3,y0,y);
    x:=x3+(x2-x3)*(y+1-y0) DIV (y1-y0);
    trapez(clipW,clipW,x2,x,y+1,y1);
    RETURN
  END;
  IF (x2>clipE) & (x3>clipE) THEN
    x2:=clipE; x3:=clipE;
  ELSIF x3>clipE THEN
    y:=y0+(clipW-x3)*(y1-y0) DIV (x2-x3);
    x:=x0+(x1-x0)*(y-y0) DIV (y1-y0);
    trapez(x,x1,x2,clipE,y,y1);
    x:=x0+(x1-x0)*(y-1-y0) DIV (y1-y0);
    trapez(x0,x,clipE,clipE,y0,y-1);
    RETURN
  ELSIF x2>clipE THEN
    y:=y0+(clipW-x3)*(y1-y0) DIV (x2-x3);
    x:=x0+(x1-x0)*(y-y0) DIV (y1-y0);
    trapez(x0,x,clipE,x3,y0,y);
    x:=x0+(x1-x0)*(y+1-y0) DIV (y1-y0);
    trapez(x,x1,clipE,clipE,y+1,y1);
    RETURN
  END;
  IF (x0=x1) & (x2=x3) THEN TV.pos(x0,y0); TV.rect(x2,y1); RETURN END;
  TV.pos(x0,y0); TV.rect_s(x1,y1,x2,y1,x3,y0);
END trapez;

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
  VAR rx,ry,S,N,E,W,X,Y: INTEGER;
BEGIN
  IF r<=0 THEN RETURN END;
  IF r<=2 THEN dot(x,y); RETURN END;
  N:=clipN*1024; E:=clipE*1024;
  S:=clipS*1024; W:=clipW*1024;
  rx:=r*1024; ry:=rx*dotszX DIV dotszY;
  X :=x*1024;  Y:=y*1024;
  IF (W<=X-rx) & (X+rx<=E) & (S<=Y-ry) & (Y+ry<=N) THEN
    TV.pos(x,y);
    TV.circ(r);
  END;
END circ;

PROCEDURE fill(i: INTEGER);
BEGIN
  IF (i>=0) & (i<Colors) THEN
    TV.fill(i)
  ELSE
    TV.fill(0)
  END;
END fill;

PROCEDURE rollN(n: INTEGER); BEGIN TV.roll(0,n) END rollN;
PROCEDURE rollE(n: INTEGER); BEGIN TV.roll(1,n) END rollE;
PROCEDURE rollS(n: INTEGER); BEGIN TV.roll(2,n) END rollS;
PROCEDURE rollW(n: INTEGER); BEGIN TV.roll(3,n) END rollW;

PROCEDURE cursor(x,y,no: INTEGER);
BEGIN
  IF (x<clipW) OR (x>clipE) OR (y<clipS) OR (y>clipN) THEN RETURN END;
  TV.pos(x,y); TV.cursor(no MOD Cursors)
END cursor;

PROCEDURE start;
BEGIN
  TV.INIT;
  setclip(0,0,Dots-1,Lines-1-4*charszY);
END start;

PROCEDURE finish;
BEGIN
  TV.FINISH;
END finish;

END VIDEO.
