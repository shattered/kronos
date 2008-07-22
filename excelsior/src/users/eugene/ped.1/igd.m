PROCEDURE rep_dot(x,y: INTEGER);
  TYPE !=BITSET;
  VAR adr: POINTER TO BITSET;
BEGIN
  WITH bitmap^ DO
    IF !(x<clipW) + !(x>clipE) # {} THEN RETURN END;
    IF !(y<clipS) + !(y>clipN) # {} THEN RETURN END;
    (*$T-*)
    adr:=ADR(base^[(sizey-1-y)*step]) + (x DIV 32); INCL(adr^,x MOD 32);
    (*$T+*)
  END;
END rep_dot;

PROCEDURE bic_dot(x,y: INTEGER);
  TYPE !=BITSET;
  VAR adr: POINTER TO BITSET;
BEGIN
  WITH bitmap^ DO
    IF !(x<clipW) + !(x>clipE) # {} THEN RETURN END;
    IF !(y<clipS) + !(y>clipN) # {} THEN RETURN END;
    (*$T-*)
    adr:=ADR(base^[(sizey-1-y)*step]) + (x DIV 32); EXCL(adr^,x MOD 32);
    (*$T+*)
  END;
END bic_dot;

PROCEDURE xor_dot(x,y: INTEGER);
  TYPE !=BITSET;
  VAR adr: POINTER TO BITSET;
BEGIN
  WITH bitmap^ DO
    IF !(x<clipW) + !(x>clipE) # {} THEN RETURN END;
    IF !(y<clipS) + !(y>clipN) # {} THEN RETURN END;
    (*$T-*)
    adr:=ADR(base^[(sizey-1-y)*step]) + (x DIV 32);
    (*$T+*)
    adr^:=adr^/{x MOD 32};
  END;
END xor_dot;

PROCEDURE dot(x,y: INTEGER);
BEGIN bitmap^.dot(x,y) END dot;

VAR dump: Line;

PROCEDURE poly(x0,y0,x1,y1: INTEGER);

  PROCEDURE rep_poly(x0,y0,x1,y1: INTEGER);
    VAR adr: ADDRESS;
        end: ADDRESS;
     n,x,sz: INTEGER;
  BEGIN sz:=(x1-x0); x:=x0 MOD 32;
    WITH bitmap^ DO
      n:=step; adr:=ADR(base^[y0*n]);
      end:=adr+(y1-y0)*n;
    END;
    REPEAT
      BBLT(adr,x0,ADR(p11),x,sz); adr:=adr+n
    UNTIL adr>=end;
  END rep_poly;

  PROCEDURE bic_poly(x0,y0,x1,y1: INTEGER);
    VAR adr: ADDRESS;
        end: ADDRESS;
     n,x,sz: INTEGER;
  BEGIN sz:=(x1-x0); x:=x0 MOD 32;
    WITH bitmap^ DO
      n:=step; adr:=ADR(base^[y0*n]);
      end:=adr+(y1-y0)*n;
    END;
    REPEAT
      BBLT(adr,x0,ADR(p00),x,sz); adr:=adr+n
    UNTIL adr>=end;
  END bic_poly;

  PROCEDURE xor_poly(x0,y0,x1,y1: INTEGER);
    VAR adr: ADDRESS;
        end: ADDRESS;
     n,x,sz: INTEGER;
          a: ADDRESS;
          e: ADDRESS;
  BEGIN sz:=(x1-x0); x:=x0 MOD 32;
    WITH bitmap^ DO
      n:=step; adr:=ADR(base^[y0*n]);
      end:=adr+(y1-y0)*n;
    END;
    IF (sz+31) DIV 32 + 1 > HIGH(dump) THEN
      e:=ADR(dump[HIGH(dump)])
    ELSE
      e:=ADR(dump[(sz+31) DIV 32 + 1])
    END;
    REPEAT
      BBLT(ADR(dump),x,adr,x0,sz);
      a:=ADR(dump);
      REPEAT a^:=BITSET(a^)/{0..31}; a:=a+1 UNTIL a>e;
      BBLT(adr,x0,ADR(dump),x,sz); adr:=adr+n
    UNTIL adr>=end;
  END xor_poly;

  VAR   t: INTEGER;
  N,S,W,E: INTEGER;

BEGIN
  WITH bitmap^ DO
    N:=clipN; S:=clipS; W:=clipW; E:=clipE;
    IF (x0<W) & (x1<W) OR (x0>E) & (x1>E) THEN RETURN END;
    IF (y0<S) & (y1<S) OR (y0>N) & (y1>N) THEN RETURN END;
    IF x0>x1 THEN t:=x0; x0:=x1; x1:=t END;
    IF y0<y1 THEN t:=y0; y0:=y1; y1:=t END;
    IF x0<W THEN x0:=W END;
    IF x1>E THEN x1:=E END;
    IF y0>N THEN y0:=N END;
    IF y1<S THEN y1:=S END;
    y0:=sizey-1-y0;
    y1:=sizey-1-y1;
    CASE tool OF
    |rep,
     or : rep_poly(x0,y0,x1,y1);
    |xor: xor_poly(x0,y0,x1,y1);
    |bic: bic_poly(x0,y0,x1,y1);
    END;
  END;
END poly;

PROCEDURE dot_circ(X,Y: INTEGER; r: INTEGER);
  VAR x,y,co: INTEGER;
BEGIN
  IF r<1 THEN dot(X,Y); RETURN END;
  x:=r; y:=0; co:=r;
  REPEAT
    dot(X+x,Y+y);
    dot(X-y,Y+x);
    dot(X-x,Y-y);
    dot(X+y,Y-x);
    INC(y); DEC(co,y);
    IF co<0 THEN INC(co,y); INC(co,x); DEC(y); DEC(x) END;
  UNTIL x=0;
END dot_circ;

PROCEDURE rep_circ(X,Y: INTEGER; r: INTEGER);
  VAR x,y,co: INTEGER;
         map: MAP;
 a0,a1,a2,a3: ADDRESS;
           n: INTEGER;
BEGIN
  x:=r; y:=0; co:=r; map:=bitmap^.base;
  Y:=bitmap^.sizey-1-Y;
  n:=bitmap^.step;
  (*$T-*)
  a0:=ADR(map^[(Y+y)*n]);
  a1:=ADR(map^[(Y+x)*n]);
  a2:=ADR(map^[(Y-y)*n]);
  a3:=ADR(map^[(Y-x)*n]);
  (*$T+*)
  REPEAT
    BBP(a0,X+x,1,1);  BBP(a1,X-y,1,1);  BBP(a2,X-x,1,1);  BBP(a3,X+y,1,1);
    IF co-y-1>0 THEN
      y:=y+1; co:=co-y; a0:=a0+n; a2:=a2-n;
    ELSE
      co:=co+x; x:=x-1; a3:=a3+n; a1:=a1-n;
    END;
  UNTIL x=0;
END rep_circ;

PROCEDURE bic_circ(X,Y: INTEGER; r: INTEGER);
  VAR x,y,co: INTEGER;
         map: MAP;
 a0,a1,a2,a3: ADDRESS;
           n: INTEGER;
BEGIN
  x:=r; y:=0; co:=r; map:=bitmap^.base;
  Y:=bitmap^.sizey-1-Y;
  n:=bitmap^.step;
  (*$T-*)
  a0:=ADR(map^[(Y+y)*n]);
  a1:=ADR(map^[(Y+x)*n]);
  a2:=ADR(map^[(Y-y)*n]);
  a3:=ADR(map^[(Y-x)*n]);
  (*$T+*)
  REPEAT
    BBP(a0,X+x,1,0);  BBP(a1,X-y,1,0);  BBP(a2,X-x,1,0);  BBP(a3,X+y,1,0);
    IF co-y-1>0 THEN
      y:=y+1; co:=co-y; a0:=a0+n; a2:=a2-n;
    ELSE
      co:=co+x; x:=x-1; a3:=a3+n; a1:=a1-n;
    END;
  UNTIL x=0;
END bic_circ;

PROCEDURE xor_circ(X,Y: INTEGER; r: INTEGER);
  VAR x,y,co: INTEGER;
         map: MAP;
 a0,a1,a2,a3: ADDRESS;
 x0,x1,x2,x3: INTEGER;
           a: ADDRESS;
           n: INTEGER;
BEGIN
  x:=r; y:=0; co:=r; map:=bitmap^.base;
  Y:=bitmap^.sizey-1-Y;
  n:=bitmap^.step;
  (*$T-*)
  a0:=ADR(map^[(Y+y)*n]);   x0:=X+x;
  a1:=ADR(map^[(Y+x)*n]);   x1:=X-y;
  a2:=ADR(map^[(Y-y)*n]);   x2:=X-x;
  a3:=ADR(map^[(Y-x)*n]);   x3:=X+y;
  (*$T+*)
  REPEAT
    a:=a0 + x0 DIV 32;  a^:=BITSET(a^)/{x0 MOD 32};
    a:=a1 + x1 DIV 32;  a^:=BITSET(a^)/{x1 MOD 32};
    a:=a2 + x2 DIV 32;  a^:=BITSET(a^)/{x2 MOD 32};
    a:=a3 + x3 DIV 32;  a^:=BITSET(a^)/{x3 MOD 32};
    IF co-y-1>0 THEN
      y:=y+1; co:=co-y; a0:=a0+n; a2:=a2-n; x1:=x1-1; x3:=x3+1;
    ELSE
      co:=co+x; x:=x-1; a3:=a3+n; a1:=a1-n; x0:=x0-1; x2:=x2+1;
    END;
  UNTIL x=0;
END xor_circ;

PROCEDURE circ(x,y: INTEGER; r: INTEGER);
  TYPE !=BITSET;
BEGIN
  IF r<1 THEN dot(x,y); RETURN END;
  WITH bitmap^ DO
    IF !(x<clipW-r)+!(x>clipE+r)+!(y<clipS-r)+!(y>clipN+r)#{} THEN RETURN END;
    IF !(clipS<=y-r)*!(y+r<=clipN)*!(clipW<=x-r)*!(x+r<=clipE)#{} THEN
      CASE tool OF
      |or
      ,rep: rep_circ(x,y,r)
      |xor: xor_circ(x,y,r)
      |bic: bic_circ(x,y,r)
      END;
    ELSE
      dot_circ(x,y,r);
    END;
  END;
END circ;

PROCEDURE dot_arc(X,Y,x0,y0,x1,y1,r: INTEGER);
  VAR x,y,co: INTEGER; case: BOOLEAN;
         xy0: INTEGER;  xx0: INTEGER;
         xy1: INTEGER;  xx1: INTEGER;
         yx0: INTEGER;  yy0: INTEGER;
         yx1: INTEGER;  yy1: INTEGER;

BEGIN
  x:=r; y:=0;    co:=r;
  xy0:=x*y0;    xx0:=x*x0;      yx0:=0;       yy0:=0;
  xy1:=x*y1;    xx1:=x*x1;      yx1:=0;       yy1:=0;
  case:=(y1*x0>x1*y0);
  REPEAT
    IF case THEN
      IF (+yx0<=+xy0) OR (+yx1>=+xy1) THEN dot(X+x,Y+y) END;
      IF (+yx0>=+xy0) OR (+yx1<=+xy1) THEN dot(X-x,Y-y) END;
      IF (-xx0<=+yy0) OR (-xx1>=+yy1) THEN dot(X+y,Y-x) END;
      IF (-xx0>=+yy0) OR (-xx1<=+yy1) THEN dot(X-y,Y+x) END;
    ELSE
      IF (+yx0<=+xy0)  & (+yx1>=+xy1) THEN dot(X+x,Y+y) END;
      IF (+yx0>=+xy0)  & (+yx1<=+xy1) THEN dot(X-x,Y-y) END;
      IF (-xx0<=+yy0)  & (-xx1>=+yy1) THEN dot(X+y,Y-x) END;
      IF (-xx0>=+yy0)  & (-xx1<=+yy1) THEN dot(X-y,Y+x) END;
    END;
    IF co-y-1>0 THEN
        y:=y+1;          co:=co-y;
      yx0:=yx0+x0;      yy0:=yy0+y0;
      yx1:=yx1+x1;      yy1:=yy1+y1;
    ELSE
       co:=co+x;          x:=x-1;
      xy0:=xy0-y0;      xx0:=xx0-x0;
      xy1:=xy1-y1;      xx1:=xx1-x1;
    END;
  UNTIL x=0;
END dot_arc;

PROCEDURE bit(i: INTEGER): BITSET;  CODE 10h 1Fh 0A9h 0ADh END bit;
PROCEDURE ofs(i: INTEGER): ADDRESS; CODE 5 8Dh END ofs;

PROCEDURE rep_arc(X,Y,x0,y0,x1,y1,r: INTEGER);
  VAR x,y,co: INTEGER;                  map: MAP;
         xy0: INTEGER;  xx0: INTEGER;   a0,a1,a2,a3: ADDRESS;
         xy1: INTEGER;  xx1: INTEGER;   c0,c1,c2,c3: INTEGER;
         yx0: INTEGER;  yy0: INTEGER;   b0,b1,b2,b3: BOOLEAN;
         yx1: INTEGER;  yy1: INTEGER;   a: POINTER TO BITSET;
           n: INTEGER; case: BOOLEAN;

BEGIN
  x:=r; y:=0;    co:=r;
  xy0:=x*y0;    xx0:=x*x0;      yx0:=0;       yy0:=0;
  xy1:=x*y1;    xx1:=x*x1;      yx1:=0;       yy1:=0;
  case:=(y1*x0>x1*y0);
  WITH bitmap^ DO map:=base; Y:=sizey-1-Y; n:=step END;
  (*$T-*)
  a0:=ADR(map^[(Y+y)*n]);   c0:=X+x;
  a1:=ADR(map^[(Y-y)*n]);   c1:=X-x;
  a2:=ADR(map^[(Y+x)*n]);   c2:=X-y;
  a3:=ADR(map^[(Y-x)*n]);   c3:=X+y;
  (*$T+*)
  REPEAT
    IF case THEN
      IF (+yx0<=+xy0) OR (+yx1>=+xy1) THEN a:=a0+ofs(c0); a^:=a^+bit(c0) END;
      IF (+yx0>=+xy0) OR (+yx1<=+xy1) THEN a:=a1+ofs(c1); a^:=a^+bit(c1) END;
      IF (-xx0<=+yy0) OR (-xx1>=+yy1) THEN a:=a2+ofs(c2); a^:=a^+bit(c2) END;
      IF (-xx0>=+yy0) OR (-xx1<=+yy1) THEN a:=a3+ofs(c3); a^:=a^+bit(c3) END;
    ELSE
      IF (+yx0<=+xy0)  & (+yx1>=+xy1) THEN a:=a0+ofs(c0); a^:=a^+bit(c0) END;
      IF (+yx0>=+xy0)  & (+yx1<=+xy1) THEN a:=a1+ofs(c1); a^:=a^+bit(c1) END;
      IF (-xx0<=+yy0)  & (-xx1>=+yy1) THEN a:=a2+ofs(c2); a^:=a^+bit(c2) END;
      IF (-xx0>=+yy0)  & (-xx1<=+yy1) THEN a:=a3+ofs(c3); a^:=a^+bit(c3) END;
    END;
    IF co-y-1>0 THEN
        y:=y+1;          co:=co-y;
      yx0:=yx0+x0;      yy0:=yy0+y0;    a0:=a0-n;       a1:=a1+n;
      yx1:=yx1+x1;      yy1:=yy1+y1;    c2:=c2+1;       c3:=c3-1;
    ELSE
       co:=co+x;          x:=x-1;
      xy0:=xy0-y0;      xx0:=xx0-x0;    c0:=c0-1;       c1:=c1+1;
      xy1:=xy1-y1;      xx1:=xx1-x1;    a2:=a2-n;       a3:=a3+n;
    END;
  UNTIL x=0;
END rep_arc;

PROCEDURE bic_arc(X,Y,x0,y0,x1,y1,r: INTEGER);
  VAR x,y,co: INTEGER;                  map: MAP;
         xy0: INTEGER;  xx0: INTEGER;   a0,a1,a2,a3: ADDRESS;
         xy1: INTEGER;  xx1: INTEGER;   c0,c1,c2,c3: INTEGER;
         yx0: INTEGER;  yy0: INTEGER;   b0,b1,b2,b3: BOOLEAN;
         yx1: INTEGER;  yy1: INTEGER;   a: POINTER TO BITSET;
           n: INTEGER; case: BOOLEAN;

BEGIN
  x:=r; y:=0;    co:=r;
  xy0:=x*y0;    xx0:=x*x0;      yx0:=0;       yy0:=0;
  xy1:=x*y1;    xx1:=x*x1;      yx1:=0;       yy1:=0;
  case:=(y1*x0>x1*y0);
  WITH bitmap^ DO map:=base; Y:=sizey-1-Y; n:=step END;
  (*$T-*)
  a0:=ADR(map^[(Y+y)*n]);   c0:=X+x;
  a1:=ADR(map^[(Y-y)*n]);   c1:=X-x;
  a2:=ADR(map^[(Y+x)*n]);   c2:=X-y;
  a3:=ADR(map^[(Y-x)*n]);   c3:=X+y;
  (*$T+*)
  REPEAT
    IF case THEN
      IF (+yx0<=+xy0) OR (+yx1>=+xy1) THEN a:=a0+ofs(c0); a^:=a^-bit(c0) END;
      IF (+yx0>=+xy0) OR (+yx1<=+xy1) THEN a:=a1+ofs(c1); a^:=a^-bit(c1) END;
      IF (-xx0<=+yy0) OR (-xx1>=+yy1) THEN a:=a2+ofs(c2); a^:=a^-bit(c2) END;
      IF (-xx0>=+yy0) OR (-xx1<=+yy1) THEN a:=a3+ofs(c3); a^:=a^-bit(c3) END;
    ELSE
      IF (+yx0<=+xy0)  & (+yx1>=+xy1) THEN a:=a0+ofs(c0); a^:=a^-bit(c0) END;
      IF (+yx0>=+xy0)  & (+yx1<=+xy1) THEN a:=a1+ofs(c1); a^:=a^-bit(c1) END;
      IF (-xx0<=+yy0)  & (-xx1>=+yy1) THEN a:=a2+ofs(c2); a^:=a^-bit(c2) END;
      IF (-xx0>=+yy0)  & (-xx1<=+yy1) THEN a:=a3+ofs(c3); a^:=a^-bit(c3) END;
    END;
    IF co-y-1>0 THEN
        y:=y+1;          co:=co-y;
      yx0:=yx0+x0;      yy0:=yy0+y0;    a0:=a0-n;       a1:=a1+n;
      yx1:=yx1+x1;      yy1:=yy1+y1;    c2:=c2+1;       c3:=c3-1;
    ELSE
       co:=co+x;          x:=x-1;
      xy0:=xy0-y0;      xx0:=xx0-x0;    c0:=c0-1;       c1:=c1+1;
      xy1:=xy1-y1;      xx1:=xx1-x1;    a2:=a2-n;       a3:=a3+n;
    END;
  UNTIL x=0;
END bic_arc;

PROCEDURE xor_arc(X,Y,x0,y0,x1,y1,r: INTEGER);
  VAR x,y,co: INTEGER;                  map: MAP;
         xy0: INTEGER;  xx0: INTEGER;   a0,a1,a2,a3: ADDRESS;
         xy1: INTEGER;  xx1: INTEGER;   c0,c1,c2,c3: INTEGER;
         yx0: INTEGER;  yy0: INTEGER;   b0,b1,b2,b3: BOOLEAN;
         yx1: INTEGER;  yy1: INTEGER;   a: POINTER TO BITSET;
           n: INTEGER; case: BOOLEAN;

BEGIN
  x:=r; y:=0;    co:=r;
  xy0:=x*y0;    xx0:=x*x0;      yx0:=0;       yy0:=0;
  xy1:=x*y1;    xx1:=x*x1;      yx1:=0;       yy1:=0;
  case:=(y1*x0>x1*y0);
  WITH bitmap^ DO map:=base; Y:=sizey-1-Y; n:=step END;
  (*$T-*)
  a0:=ADR(map^[(Y+y)*n]);   c0:=X+x;
  a1:=ADR(map^[(Y-y)*n]);   c1:=X-x;
  a2:=ADR(map^[(Y+x)*n]);   c2:=X-y;
  a3:=ADR(map^[(Y-x)*n]);   c3:=X+y;
  (*$T+*)
  REPEAT
    IF case THEN
      IF (+yx0<=+xy0) OR (+yx1>=+xy1) THEN a:=a0+ofs(c0); a^:=a^/bit(c0) END;
      IF (+yx0>=+xy0) OR (+yx1<=+xy1) THEN a:=a1+ofs(c1); a^:=a^/bit(c1) END;
      IF (-xx0<=+yy0) OR (-xx1>=+yy1) THEN a:=a2+ofs(c2); a^:=a^/bit(c2) END;
      IF (-xx0>=+yy0) OR (-xx1<=+yy1) THEN a:=a3+ofs(c3); a^:=a^/bit(c3) END;
    ELSE
      IF (+yx0<=+xy0)  & (+yx1>=+xy1) THEN a:=a0+ofs(c0); a^:=a^/bit(c0) END;
      IF (+yx0>=+xy0)  & (+yx1<=+xy1) THEN a:=a1+ofs(c1); a^:=a^/bit(c1) END;
      IF (-xx0<=+yy0)  & (-xx1>=+yy1) THEN a:=a2+ofs(c2); a^:=a^/bit(c2) END;
      IF (-xx0>=+yy0)  & (-xx1<=+yy1) THEN a:=a3+ofs(c3); a^:=a^/bit(c3) END;
    END;
    IF co-y-1>0 THEN
        y:=y+1;          co:=co-y;
      yx0:=yx0+x0;      yy0:=yy0+y0;    a0:=a0-n;       a1:=a1+n;
      yx1:=yx1+x1;      yy1:=yy1+y1;    c2:=c2+1;       c3:=c3-1;
    ELSE
       co:=co+x;          x:=x-1;
      xy0:=xy0-y0;      xx0:=xx0-x0;    c0:=c0-1;       c1:=c1+1;
      xy1:=xy1-y1;      xx1:=xx1-x1;    a2:=a2-n;       a3:=a3+n;
    END;
  UNTIL x=0;
END xor_arc;


PROCEDURE arc(X,Y,x0,y0,x1,y1,r: INTEGER);
  TYPE !=BITSET;
  VAR case: INTEGER;
BEGIN
  IF r<1 THEN dot(X,Y); RETURN END;
  x0:=x0-X; y0:=y0-Y;
  x1:=x1-X; y1:=y1-Y;
  IF x0*y1=x1*y0 THEN circ(X,Y,r); RETURN END;
  case:=ORD( (x0>=0)#(x1>=0) )*2 + ORD( (y1*x0>x1*y0) );
  WITH bitmap^ DO
    IF !(X<clipW-r)+!(X>clipE+r)+!(Y<clipS-r)+!(Y>clipN+r)#{} THEN RETURN END;
    IF !(clipS<=Y-r)*!(Y+r<=clipN)*!(clipW<=X-r)*!(X+r<=clipE)#{} THEN
      CASE tool OF
      |or
      ,rep: rep_arc(X,Y,x0,y0,x1,y1,r);
      |bic: bic_arc(X,Y,x0,y0,x1,y1,r);
      |xor: xor_arc(X,Y,x0,y0,x1,y1,r);
      END;
    ELSE
      dot_arc(X,Y,x0,y0,x1,y1,r);
    END;
  END;
END arc;

MODULE pedGD; (* Sem 08-Dec-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;
FROM LabtamVDU  IMPORT  layer, Line;
FROM Terminal   IMPORT  Read;
FROM mCodeMnem  IMPORT  copt, lsw0, ssw0;

CONST clipW=0; clipE=799; clipS=0; clipN=279; lnsz=16;

TYPE tools=(rep,or,xor,bic);

VAR tool : tools;
    base : ARRAY [0..299] OF POINTER TO Line;
    patt : BITSET;
    patbt: INTEGER;


PROCEDURE BBP(to: ADDRESS; ofs: INTEGER; size: INTEGER; val: WORD);
CODE 0EDh END BBP;

PROCEDURE ASR(w: INTEGER; n: INTEGER): INTEGER;
CODE 08Dh END ASR;

PROCEDURE vectp(x,y,x1,y1: INTEGER);
  VAR co   : INTEGER;
      dy   : INTEGER;
      pat  : BITSET;
      xls  : BITSET;
      adr  : POINTER TO BITSET;
      Dx,Dy: INTEGER;
      xl,xh: INTEGER;

PROCEDURE c1(a: ADDRESS); CODE copt END c1;
PROCEDURE c2(): BITSET; CODE lsw0 END c2;
PROCEDURE c3(v: BITSET); CODE ssw0 END c3;

BEGIN
  Dx:=(x1-x); Dy:=(y1-y);
  xl:=x MOD 32; xh:=x DIV 32;
  IF Dy<0 THEN dy:=-1 ELSE dy:=+1 END;
  Dx:=ABS(Dx); Dy:=ABS(Dy);
  xls:={xl}; pat:=patt>>(patbt-xl);
  IF Dx>Dy THEN
    co:=Dx DIV 2; patbt:=(patbt+Dx+1) MOD 32;
    IF x1>=x THEN
      CASE tool OF
        |or :
          REPEAT
            c1(ADDRESS(base[y])+xh); c3(c2()+pat*xls);
            xls:=xls<<1; INC(x); INC(co,Dy);
            xh:=xh+INTEGER(xls={0});
            IF co>=Dx THEN INC(y,dy); DEC(co,Dx) END;
          UNTIL x>x1; RETURN;
        |bic:
          REPEAT
            c1(ADDRESS(base[y])+xh); c3(c2()-pat*xls);
            xls:=xls<<1; INC(x); INC(co,Dy);
            xh:=xh+INTEGER(xls={0});
            IF co>=Dx THEN INC(y,dy); DEC(co,Dx) END;
          UNTIL x>x1; RETURN;
        |xor:
          REPEAT
            c1(ADDRESS(base[y])+xh); c3(c2()/pat*xls);
            xls:=xls<<1; INC(x); INC(co,Dy);
            xh:=xh+INTEGER(xls={0});
            IF co>=Dx THEN INC(y,dy); DEC(co,Dx) END;
          UNTIL x>x1; RETURN;
      ELSE
      END;
    ELSE
      CASE tool OF
        |or :
          REPEAT
            c1(ADDRESS(base[y])+xh); c3(c2()+pat*xls);
            xls:=xls>>1; INC(x); INC(co,Dy);
            xh:=xh+INTEGER(xls={0});
            IF co>=Dx THEN INC(y,dy); DEC(co,Dx) END;
          UNTIL x>x1; RETURN;
        |bic:
          REPEAT
            c1(ADDRESS(base[y])+xh); c3(c2()-pat*xls);
            xls:=xls<<1; INC(x); INC(co,Dy);
            xh:=xh+INTEGER(xls={0});
            IF co>=Dx THEN INC(y,dy); DEC(co,Dx) END;
          UNTIL x>x1; RETURN;
        |xor:
          REPEAT
            c1(ADDRESS(base[y])+xh); c3(c2()/pat*xls);
            xls:=xls<<1; INC(x); INC(co,Dy);
            xh:=xh+INTEGER(xls={0});
            IF co>=Dx THEN INC(y,dy); DEC(co,Dx) END;
          UNTIL x>x1; RETURN;
      ELSE
      END;
    END;
  ELSE

  END;
END vectp;

PROCEDURE vect(x0,y0,x1,y1: INTEGER);
  VAR N,E  : INTEGER;
      S,W  : INTEGER;     x0lssW,x0gtrE,x1lssW,x1gtrE: BITSET;
      a,b,c: INTEGER;     y0lssS,y0gtrN,y1lssS,y1gtrN: BITSET;

  TYPE !=BITSET;

  PROCEDURE clip(VAR x0,y0: INTEGER): BOOLEAN;
    VAR x,y: INTEGER;
  BEGIN
    IF x0<W THEN x:=W; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    IF y0<S THEN y:=S; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    IF x0>E THEN x:=E; y:=(c-a*x) DIV b;
      IF (y>=S) & (y<=N) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    IF y0>N THEN y:=N; x:=(c-b*y) DIV a;
      IF (x>=W) & (x<=E) THEN x0:=x; y0:=y; RETURN FALSE END;
    END;
    RETURN TRUE
  END clip;

  VAR t: INTEGER;

BEGIN
  WITH bm^ DO
    W:=clipW; E:=clipE; S:=clipS; N:=clipN;
  END;
  x0lssW:=!(x0<W);      x0gtrE:=!(x0>E);
  x1lssW:=!(x1<W);      x1gtrE:=!(x1>E);
  y0lssS:=!(y0<S);      y0gtrN:=!(y0>N);
  y1lssS:=!(y1<S);      y1gtrN:=!(y1>N);
  IF x1lssW+x0gtrE+(y0lssS*y1lssS)+(y0gtrN*y1gtrN)#{} THEN
    RETURN
  END;
  IF NOT BOOLEAN(x0lssW+x0gtrE+x1lssW+x1gtrE+y0lssS+y0gtrN+y1lssS+y1gtrN) THEN
    (* simple case all vector in cliped room *)
    vectp(x0,y0,x1,y1); RETURN
  END;
  c:=(x0*y1-x1*y0);   -- a/c*x+b/c*y=1;
  a:=(y1-y0);         -- a*x+b*y=c;
  b:=(x0-x1);

  IF ((x0<W) OR (x0>E) OR (y0<S) OR (y0>N)) & clip(x0,y0) THEN RETURN END;
  IF ((x1<W) OR (x1>E) OR (y1<S) OR (y1>N)) & clip(x1,y1) THEN RETURN END;
  ASSERT((W<=x0) & (x0<=E) & (S<=y0) & (y0<=N));
  ASSERT((W<=x1) & (x1<=E) & (S<=y1) & (y1<=N));
  vectp(x0,y0,x1,y1);
END vect;

PROCEDURE arc (b: BMD; x,y,x0,y0,x1,y1: INTEGER);
END arc;

*)

VAR i: INTEGER;

BEGIN
  tool:=or; patt:={0,4,8,12,16,20,24,28};
  FOR i:=0 TO HIGH(base) DO base[i]:=ADR(layer[1]^[i]) END;
  FOR i:=0 TO 299 BY 10 DO vectp(0,150,799,i) END;
  IF Read()=0c THEN END;
END pedGD.
