IMPLEMENTATION MODULE bcMath; (*  31-Mar-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  mth: realMath;
IMPORT  cod: defCodes;
IMPORT  def: bcDef;
IMPORT  mem: Heap;
IMPORT  tim: Time;

WITH STORAGE (NEW    : mem.ALLOCATE;
              DISPOSE: mem.DEALLOCATE;
              RESIZE : mem.REALLOCATE);


----------------------------- CALC -----------------------------

PROCEDURE max(SEQ a: REAL): REAL;
  VAR b: REAL; i: INTEGER;
BEGIN
  i:=0; b:=a[0];
  WHILE i<=HIGH(a) DO
    IF a[i]>b THEN b:=a[i] END; INC(i)
  END;
  RETURN b
END max;

PROCEDURE min(SEQ a: REAL): REAL;
  VAR b: REAL; i: INTEGER;
BEGIN
  i:=0; b:=a[0];
  WHILE i<=HIGH(a) DO
    IF a[i]<b THEN b:=a[i] END; INC(i)
  END;
  RETURN b
END min;

PROCEDURE dist(x1,y1,x2,y2: REAL): REAL;
  VAR dx,dy: REAL;
BEGIN dx:=x1-x2; dy:=y1-y2; RETURN mth.sqrt(dx*dx+dy*dy) END dist;

PROCEDURE dist1(top1,top: def.VERTEX): REAL;
  VAR x1,y1,x2,y2: REAL;
BEGIN
  x1:=top[0]; x2:=top1[0];
  y1:=top[1]; y2:=top1[1];
  IF    x1=x2 THEN  RETURN ABS(y1-y2)
  ELSIF y1=y2 THEN  RETURN ABS(x1-x2)
  ELSE
    x1:=x1-x2; y1:=y1-y2;  RETURN mth.sqrt(x1*x1+y1*y1)
  END
END dist1;

PROCEDURE calc_arc(VAR xc,yc,r: REAL; top1,top2,top3:def.VERTEX): BOOLEAN;
  VAR a1,a2,b1,b2,c1,c2,det: REAL;
          x1,y1,x2,y2,x3,y3: REAL;
BEGIN
  x1:=top1[0]; y1:=top1[1];
  x2:=top2[0]; y2:=top2[1];
  x3:=top3[0]; y3:=top3[1];

  a1:=x1-x2; a2:=x1-x3;
  b1:=y1-y2; b2:=y1-y3;
  c1:=x1*x1-x2*x2 + y1*y1-y2*y2;
  c2:=x1*x1-x3*x3 + y1*y1-y3*y3;
  det:=2.*(a1*b2-a2*b1);
  IF det=0. THEN RETURN FALSE END;
  xc:=(c1*b2-c2*b1)/det;
  yc:=(a1*c2-a2*c1)/det;
  r:=dist(x1,y1,xc,yc);
  RETURN TRUE
END calc_arc;

PROCEDURE angle_1(top1,top2,top3: def.VERTEX): REAL;
 VAR ty1,ty2,tx1,
     tx2,tx3,ty3,
       t1,t2,t3,t: REAL;
BEGIN
  tx1:= (top2[0]-top1[0]);  tx1:=tx1*tx1;
  tx2:= (top3[0]-top1[0]);  tx2:=tx2*tx2;
  tx3:= (top3[0]-top2[0]);  tx3:=tx3*tx3;

  ty1:= (top2[1]-top1[1]);  ty1:=ty1*ty1;
  ty2:= (top3[1]-top1[1]);  ty2:=ty2*ty2;
  ty3:= (top3[1]-top2[1]);  ty3:=ty3*ty3;

  t1:=tx1+ty1;
  t2:=tx2+ty2;
  t3:=tx3+ty3;
  t:=t1*t2;
  IF t=0. THEN RETURN 0.
  ELSE
    t:=(t1+t2-t3)/(2.*mth.sqrt(t));
    IF t>=0.99999 THEN RETURN 0. END;
    RETURN mth.arccos(t)
  END
END angle_1;

PROCEDURE angle_0(top,top1: def.VERTEX): REAL;
  VAR top2: def.VERTEX;
BEGIN
  top2[0]:=top[0]+10.; top2[1]:=top[1];
  IF top1[1]>=top[1] THEN
    RETURN angle_1(top,top2,top1)
  ELSE
    RETURN mth.pi*2.-angle_1(top,top2,top1)
  END
END angle_0;

PROCEDURE angle0(top,top1: def.VERTEX): REAL; (* угол с гориз. в градусах *)
BEGIN RETURN mth.rtod(angle_0(top,top1)) END angle0;

PROCEDURE angle1(top,top1,top2: def.VERTEX): REAL;
BEGIN RETURN mth.rtod(angle_1(top,top1,top2)) END angle1;

PROCEDURE angle_(top,top1,top2: def.VERTEX): REAL;
  VAR a1,a2: REAL;
BEGIN
  a1:=angle_0(top,top1);
  a2:=angle_0(top,top2);
  IF a1< a2 THEN RETURN (a2-a1)
  ELSE RETURN mth.pi*2.-a1+a2 END
END angle_;

PROCEDURE angle(top,top1,top2: def.VERTEX): REAL;
BEGIN  RETURN mth.rtod(angle_(top,top1,top2)) END angle;

----------------------------- SPLIN ----------------------------

PROCEDURE move(s,d: SYSTEM.ADDRESS; s: INTEGER);
CODE cod.move END move;

PROCEDURE zero(VAR r: ARRAY OF REAL);
  VAR a: SYSTEM.ADDRESS;
BEGIN
  a:=SYSTEM.ADR(r); a^:=0; move(a+1,a,HIGH(r))
END zero;

----------------------------------------------------------------

PROCEDURE pro(VAL x,y: ARRAY OF REAL; VAR m:ARRAY OF REAL);
  VAR l,s,r: DYNARR OF REAL;
      d,e,f,h,p: REAL;
      k,n: INTEGER;
BEGIN
  n:=HIGH(x);
  NEW(l,n+1); NEW(s,n+1); NEW(r,n+1);
  zero(l);    zero(s);    zero(r);
  d:=x[1]-x[0]; e:=(y[1]-y[0])/d;
  FOR k:=1 TO n-1 DO
    IF x[k+1]#x[k] THEN
      h:=d; d:=x[k+1]-x[k];
      f:=e; e:=(y[k+1]-y[k])/d; l[k]:=d/(d+h);
      r[k]:=1.-l[k]; s[k]:=6.*(e-f)/(h+d)
    ELSE                                        --!!!
      l[k]:=l[k+1];
      r[k]:=1.-l[k];
      s[k]:=s[k+1]
    END
  END;
  FOR k:=1 TO n-1 DO
      p:=1./(r[k]*l[k-1]+2.); l[k]:=-l[k]*p;
      s[k]:=(s[k]-r[k]*s[k-1])*p
  END;
  m[n]:=0.; l[n-1]:=s[n-1]; m[n-1]:=l[n-1];
  FOR k:=n-2 TO 0 BY -1 DO
    l[k]:=l[k]*l[k+1]+s[k]; m[k]:=l[k]
  END
END pro;

PROCEDURE splin(VAL x,y,m:ARRAY OF REAL; X:REAL; VAR Y:REAL);
  VAR  n,i,j: INTEGER;
       h,p,r,d: REAL;
BEGIN
  ASSERT(HIGH(x)=HIGH(y));
  ASSERT(HIGH(x)=HIGH(m));
  n:=HIGH(x);
  IF X > x[n] THEN
    d:=x[n]-x[n-1];
    Y:= d*m[n-1]/6.+(y[n]-y[n-1])/d;
    Y:=Y*(X-x[n])+y[n];
    RETURN
  ELSIF X <= x[0] THEN
    d:=x[1]-x[0];
    Y:=-d*m[1]/6.+(y[1]-y[0])/d;
    Y:=Y*(X-x[0])+y[0];
    RETURN
  END;
  i:=0;
  WHILE X > x[i] DO INC(i); END;
  j:=i-1; d:=x[i]-x[j]; h:=X-x[j];
  r:=x[i]-X; p:=d*d/6.;
  Y:=(m[j]*r*r*r+m[i]*h*h*h)/(6.*d);
  Y:=Y+((y[j]-m[j]*p)*r+(y[i]-m[i]*p)*h)/d
END splin;

PROCEDURE long(x,y:ARRAY OF REAL; VAR l:ARRAY OF REAL);
  VAR    i: INTEGER;
       a,b: REAL;
BEGIN
  zero(l);
  FOR i:=1 TO HIGH(l) DO
   a:=x[i]-x[i-1];
   b:=y[i]-y[i-1];
   l[i]:=mth.sqrt(a*a+b*b)+l[i-1]
  END
END long;

PROCEDURE t_pro(x,y,t: ARRAY OF REAL; VAR pro_x,pro_y: ARRAY OF REAL);
BEGIN pro(t,x,pro_x); pro(t,y,pro_y) END t_pro;

PROCEDURE  t_splin(x,y,pro_x,pro_y,t: ARRAY OF REAL; T: REAL; VAR X,Y:REAL);
BEGIN splin(t,x,pro_x,T,X); splin(t,y,pro_y,T,Y) END t_splin;

------------------------- INTEGER CALC -------------------------

TYPE  process  = POINTER TO RECORD G,L,PC,M,S,H,T: INTEGER END;

PROCEDURE setm(m: BITSET); CODE cod.setm END setm;
PROCEDURE getm(): BITSET ; CODE cod.getm END getm;
PROCEDURE active(): process; CODE cod.activ END active;
PROCEDURE ca(VAL s: ARRAY OF INTEGER): SYSTEM.ADDRESS; CODE cod.drop END ca;

PROCEDURE muldiv(w: INTEGER; mul,div: INTEGER): INTEGER;
  VAR W: INTEGER; p: process;
BEGIN
  W:=w; p:=active(); p^.T:=0;
  setm(getm()-{31}); w:=(w*mul) DIV div; setm(getm()+{31});
  IF p^.T=41h THEN
    IF W>mul THEN w:=(W DIV div)*mul ELSE w:=(mul DIV div)*W END;
  END;
  RETURN w
END muldiv;

PROCEDURE sqrt(a: INTEGER): INTEGER;
  VAR s,b: INTEGER;
BEGIN
  ASSERT(a>=0);
  IF a IN {0,1} THEN RETURN a END;
  s:=2; b:=a DIV s;
  WHILE (ABS(s-b)>1) DO s:=(a DIV b + b) DIV 2; b:=a DIV s END;
  IF s>b THEN RETURN s ELSE RETURN b END
END sqrt;

---------------------------- Matrix ----------------------------

PROCEDURE MxM(VAR r: MATR; a,b: MATR);
  VAR i,j,k: INTEGER; s: REAL;
BEGIN
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      s:=0.;
      FOR k:=0 TO 2 DO s:=s+a[i,k]*b[k,j] END;
      r[i,j]:=s
    END
  END
END MxM;

PROCEDURE VxM(VAR r: VECT; v: VECT; VAL a: MATR);
  VAR j,k: INTEGER; s: REAL;
BEGIN
  FOR j:=0 TO 2 DO
    s:=0.;
    FOR k:=0 TO 2 DO s:=s+v[k]*a[k,j] END;
    r[j]:=s
  END
END VxM;

PROCEDURE MxV(VAR r: MATR; a: VECT; b: MATR);
  VAR i,j,k: INTEGER; s: REAL;
BEGIN
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      s:=0.;
      FOR k:=0 TO 2 DO s:=s+a[k]*b[k,j] END;   --!!!!!??
      r[i,j]:=s
    END
  END
END MxV;


BEGIN
END bcMath.
