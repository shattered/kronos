MODULE orbit; (* Dima 30-Jul-88. (c) KRONOS *)

FROM RealMath   IMPORT  E, PI, Sin, Cos, Exp;
                IMPORT  LabtamVDU;
                IMPORT  Clock, Terminal, Keyboard, SYSTEM;

PROCEDURE sin(VAR x,y: REAL);
  VAR e, e_: REAL;
BEGIN
  e:=Exp(y); e_:=1./e;
  y:=Cos(x)*(e-e_)*0.5;
  x:=Sin(x)*(e+e_)*0.5;
END sin;

MODULE out;
                IMPORT  PI;
FROM LabtamVDU  IMPORT  layer, Line, Lay, auto, block;
FROM SYSTEM     IMPORT  ADR;

EXPORT QUALIFIED screen, point, cursor, on, off, clear;

VAR lay: Lay;
    x0, y0, x1, y1, onex, oney: REAL;

CONST lines=300-16; cols=800-32;

PROCEDURE screen(x,y,dx,dy: REAL);
BEGIN
  x0:=x-dx; y0:=y-dy;
  x1:=x+dx; y1:=y+dy;
  onex:=(x1-x0)/FLOAT(cols);
  oney:=(y1-y0)/FLOAT(lines);
END screen;

PROCEDURE lc(x,y: REAL; VAR l,c: INTEGER): BOOLEAN;
BEGIN
  IF (x<x0) OR (y<y0) OR (x>x1) OR (y>y1) THEN RETURN FALSE END;
  c:=TRUNC((x-x0)/onex);
  l:=lines-TRUNC((y-y0)/oney);
  RETURN TRUE
END lc;

PROCEDURE point(x,y: REAL);
  VAR l,c: INTEGER;
BEGIN
  IF lc(x,y,l,c) THEN INCL(lay^[l, c DIV 32], c MOD 32) END;
END point;

PROCEDURE cursor(x,y: REAL);
  VAR l, c, cdiv, cmod: INTEGER; p: POINTER TO BITSET; mask: BITSET;
BEGIN
  IF lc(x,y,l,c) THEN
    cdiv:=c DIV 32; cmod:=c MOD 32;
    p:=ADR(lay^[l,cdiv]);
    IF cmod=31 THEN mask:={0..30} ELSE mask:=BITSET(INTEGER({cmod})-1) END;
    p^:=p^/mask/{0..31}; INC(INTEGER(p)); p^:=p^/mask;
    FOR l:=l TO l+15 DO
      p:=ADR(lay^[l,cdiv]); p^:=p^/{cmod};
    END;
  END;
END cursor;

PROCEDURE on ; BEGIN auto(TRUE)  END on;
PROCEDURE off; BEGIN auto(FALSE) END off;

PROCEDURE clear;
  VAR i: INTEGER; row: Line;
BEGIN
  FOR i:=0 TO HIGH(row)  DO row[i]:={}   END;
  FOR i:=0 TO HIGH(lay^) DO lay^[i]:=row END;
END clear;

BEGIN lay:=layer[1];
  screen(0., 0., PI, PI*0.75);
END out;

PROCEDURE orbit(x,y: REAL);
BEGIN
  REPEAT
    out.point(x,y); sin(x,y);
  UNTIL (Terminal.BusyRead()#0c) OR (ABS(y)>10.);
END orbit;

PROCEDURE conv?(x,y: REAL): BOOLEAN;
  CONST iter=50;
  VAR i: INTEGER;
BEGIN
  i:=0;
  REPEAT sin(x,y); INC(i);
  UNTIL (i>iter) OR (ABS(y)>10.) OR (ABS(y)<0.5);
  RETURN (i>iter) OR (ABS(y)<0.5)
END conv?;

PROCEDURE play;
  VAR x, y, dx, dy, x0, y0, x1, y1: REAL;

  PROCEDURE start;
  BEGIN
    out.cursor(x, y);
    LOOP
      CASE Keyboard.ReadKey() OF
        |Keyboard.up   : out.cursor(x,y); y:=y+dy; out.cursor(x,y);
        |Keyboard.dw   : out.cursor(x,y); y:=y-dy; out.cursor(x,y);
        |Keyboard.left : out.cursor(x,y); x:=x-dx; out.cursor(x,y);
        |Keyboard.right: out.cursor(x,y); x:=x+dx; out.cursor(x,y);
        |' '           : EXIT
        |'c'           : out.clear; out.cursor(x, y);
      ELSE END;
    END;
    out.cursor(x, y);
  END start;

  VAR xx,yy: REAL;
BEGIN
  x:=0.; y:=1.5;
  dx:=1./100.; dy:=dx*0.75;
  x0:=x-dx*100.; y0:=y-dy*100.;
  x1:=x+dx*100.; y1:=y+dy*100.;
  out.screen(x, y, dx*100., dy*100.);
  xx:=x0;
  WHILE xx<=x1 DO yy:=y0;
    WHILE yy<=y1 DO
      IF conv?(xx,yy) THEN out.point(xx,yy) END;
      yy:=yy+dy;
    END;
    xx:=xx+dx;
  END;
END play;


BEGIN
  play;
  IF Terminal.Read()=0c THEN END;
END orbit.
