IMPLEMENTATION MODULE adaNum; (* Sem 09-Sep-90. (c) KRONOS *)

IMPORT mcd : mCodeMnem;
FROM Terminal   IMPORT  print;

PROCEDURE inl(i: INTEGER; VAL n: long_int): BOOLEAN;
CODE mcd.lib bits_no mcd.inl END inl;

PROCEDURE incl(VAL n: long_int; i: INTEGER);
CODE mcd.lib bits_no-1 mcd.chkz mcd.incl END incl;

PROCEDURE excl(VAL n: long_int; i: INTEGER);
CODE mcd.lib bits_no-1 mcd.chkz mcd.excl END excl;

PROCEDURE bblt(VAR n1: long_int; i1: INTEGER;
               VAR n2: long_int; i2: INTEGER; s: INTEGER);
CODE mcd.bblt END bblt;

PROCEDURE long_zero(VAR v: long_int);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(v.i) DO v.i[i]:=0 END;
END long_zero;

PROCEDURE add_short(VAR v: long_int; n: INTEGER);
  TYPE int=INTEGER; VAR j,k: INTEGER; c,c1: BOOLEAN;
BEGIN
  c:=FALSE;
  FOR j:=0 TO 31 DO
    k:=int(inl(j,v))+int(j IN BITSET(n))+int(c);
    c:=k>=2;
    IF ODD(k) THEN incl(v,j) ELSE excl(v,j) END;
  END;
  FOR j:=32 TO bits_no-1 DO
    k:=int(inl(j,v))+int(n<0)+int(c);
    c1:=c; c:=k>=2;
    IF ODD(k) THEN incl(v,j) ELSE excl(v,j) END;
  END;
  ovr:=ovr OR (c#c1);
  IF ovr THEN long_zero(v) END;
END add_short;

PROCEDURE add(VAR v: long_int; n: long_int);
  TYPE int=INTEGER; VAR j,k: INTEGER; c,c1: BOOLEAN;
BEGIN
  c:=FALSE;
  FOR j:=0 TO bits_no-1 DO
    k:=int(inl(j,v))+int(inl(j,n))+int(c);
    c1:=c; c:=k>=2;
    IF ODD(k) THEN incl(v,j) ELSE excl(v,j) END;
  END;
  ovr:=ovr OR (c#c1);
  IF ovr THEN long_zero(v) END;
END add;

PROCEDURE sub(VAR v: long_int; n: long_int);
  TYPE int=INTEGER; VAR j,k: INTEGER; c,c1: BOOLEAN;
BEGIN
  c:=TRUE;
  FOR j:=0 TO bits_no-1 DO
    k:=int(inl(j,v))+int(NOT inl(j,n))+int(c);
    c1:=c; c:=k>=2;
    IF ODD(k) THEN incl(v,j) ELSE excl(v,j) END;
  END;
  ovr:=ovr OR (c#c1);
  IF ovr THEN long_zero(v) END;
END sub;

PROCEDURE neg(VAR v: long_int);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(v.s) DO v.s[i]:=v.s[i]/{0..31} END;
  add_short(v,1);
END neg;

PROCEDURE mul_short(VAR v: long_int; n: INTEGER);
  VAR t: long_int; i,j,m: INTEGER; sig: BOOLEAN;
BEGIN
  sig:=FALSE;
  IF n<0 THEN sig:=NOT sig; n:=-n END;
  IF 31 IN v.s[HIGH(v.s)] THEN sig:=NOT sig; neg(v) END;
  FOR i:=0 TO HIGH(t.i) DO t.i[i]:=0 END;
  FOR i:=0 TO HIGH(v.c) DO
    m:=ORD(v.c[i])*n; j:=0;
    WHILE m>0 DO
      IF i+j>HIGH(t.c) THEN ovr:=TRUE; RETURN END;
      m:=m+ORD(t.c[i+j]); t.c[i+j]:=CHAR(m);
      m:=m DIV 100h; INC(j);
    END;
  END;
  v:=t;
  IF sig THEN neg(v) END;
  IF ovr THEN long_zero(v) END;
END mul_short;

PROCEDURE normal(VAR f: long_float);
  VAR j: INTEGER; t: long_int;
BEGIN
  FOR j:=bits_no-1 TO 0 BY -1 DO
    IF inl(j,f.man) THEN
      IF j=bits_no-1 THEN RETURN END;
      long_zero(t);
      bblt(t,bits_no-j-1,f.man,0,j+1);
      f.man:=t; DEC(f.exp,bits_no-j-1);
      IF ABS(f.exp)>=1000000h THEN ovr:=TRUE; f.exp:=0 END;
      RETURN;
    END;
  END;
  f.sig:=FALSE; f.exp:=0;
END normal;

PROCEDURE int_float(n: long_int; VAR f: long_float);
BEGIN
  long_zero(f.man);
  IF 31 IN n.s[HIGH(n.s)] THEN neg(n); f.sig:=TRUE ELSE f.sig:=FALSE END;
  f.exp:=bits_no; f.man:=n; normal(f);
END int_float;

PROCEDURE fadd(VAR x,y: long_float);
  VAR z: long_float; i,k,dx,dy: INTEGER; c: BOOLEAN; TYPE int=INTEGER;
BEGIN
  IF x.exp<y.exp THEN
    dx:=y.exp-x.exp+2; dy:=2
  ELSE
    dy:=x.exp-y.exp+2; dx:=2
  END;
  long_zero(z.man); z.exp:=x.exp+dx;
  IF x.sig=y.sig THEN
    c:=FALSE;
    FOR i:=-4 TO bits_no-1 DO
      k:=int(inl(i+dx,x.man))+int(inl(i+dy,y.man))+int(c);
      c:=k>=2;
      IF i>=0 THEN
        IF ODD(k) THEN incl(z.man,i) ELSE excl(z.man,i) END;
      END;
    END;
    z.sig:=x.sig;
  ELSE
    c:=TRUE;
    FOR i:=-4 TO bits_no-1 DO
      k:=int(inl(i+dx,x.man))+int(NOT inl(i+dy,y.man))+int(c);
      c:=k>=2;
      IF i>=0 THEN
        IF ODD(k) THEN incl(z.man,i) ELSE excl(z.man,i) END;
      END;
    END;
    z.sig:=x.sig;
    IF inl(bits_no-1,z.man) THEN neg(z.man); z.sig:=NOT z.sig END;
  END;
  normal(z); x:=z;
END fadd;

PROCEDURE fsub(VAR x: long_float; y: long_float);
BEGIN
  y.sig:=NOT y.sig; fadd(x,y);
END fsub;

PROCEDURE fdiv(VAR x,y: long_float);
  TYPE int=INTEGER;
  VAR z: long_float; c: BOOLEAN; i,j,k: INTEGER; x1,x2: long_int;
BEGIN
  normal(y);
  IF NOT inl(bits_no-1,y.man) THEN ovr:=TRUE; RETURN END;
  long_zero(z.man);
  z.sig:=x.sig#y.sig;
  z.exp:=x.exp-y.exp+1;
  i:=bits_no;
  LOOP
    DEC(i);
    j:=bits_no;
    REPEAT DEC(j) UNTIL (j<0) OR inl(j,x.man);
    IF j<bits_no-1 THEN
      IF j<0 THEN EXIT END;
      long_zero(x1); bblt(x1,bits_no-1-j,x.man,0,j+1); x.man:=x1;
      DEC(i,bits_no-1-j);
    END;
    c:=TRUE;
    FOR j:=0 TO bits_no-1 DO
      k:=int(inl(j,x.man))+int(NOT inl(j,y.man))+int(c);
      c:=k>=2;
      IF j<bits_no-1 THEN
        IF ODD(k) THEN incl(x1,j+1) ELSE excl(x1,j+1) END;
        IF inl(j,x.man) THEN incl(x2,j+1) ELSE excl(x2,j+1) END;
      END;
    END;
    IF c THEN incl(z.man,i); x.man:=x1 ELSE x.man:=x2 END;
    excl(x.man,0);
    IF i=0 THEN EXIT END;
  END;
  x:=z; normal(x);
END fdiv;

PROCEDURE fmul(VAR x,y: long_float);
BEGIN

END fmul;

PROCEDURE print_float(VAL f: long_float);
  VAR i: INTEGER;
BEGIN
  IF f.sig THEN print('-') ELSE print('+') END;
  FOR i:=HIGH(f.man.i) TO 0 BY -1 DO print('%$8h',f.man.i[i]) END;
  print(' %d',f.exp);
END print_float;

PROCEDURE print_int(VAL n: long_int);
  VAR i: INTEGER;
BEGIN
  FOR i:=HIGH(n.i) TO 0 BY -1 DO print('%$8h',n.i[i]) END;
END print_int;

VAR
  i  : INTEGER;
  n  : long_int;
  f1 : long_float;
  f2 : long_float;

BEGIN
  ASSERT(bits_no<256); ovr:=FALSE;
  FOR i:=-9 TO 9 DO
    ovr:=FALSE;
    long_zero(n);
    add_short(n,i);
    int_float(n,f1);
    mul_short(n,10h);
    print_int(n);
    int_float(n,f2);
    print(' : ');
    fdiv(f2,f1);
    print_float(f2);
    IF ovr THEN print(' ovr') END;
    print('\n');
  END;
END adaNum.
