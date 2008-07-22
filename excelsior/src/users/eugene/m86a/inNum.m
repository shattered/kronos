IMPLEMENTATION MODULE inNum; (* Sem 01-Apr-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

IMPORT  pc : pcTab;

-- 1.0 = 3F800000h

TYPE
  real=RECORD
    sign: BOOLEAN;
    exp : INTEGER;
    val : INTEGER;
  END;
  types = (t_real,t_integer);

CONST div_by_zero = 'division by zero';

VAR text_pos: INTEGER;

PROCEDURE error(s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  pc.error(text_pos,FALSE,s,x);
END error;

PROCEDURE un_pack(n: WORD; VAR r: real);
BEGIN
  IF n=0 THEN r.sign:=FALSE; r.val:=0; r.exp:=0; RETURN END;
  r.sign:=31 IN BITSET(n);
  r.exp:=INTEGER(BITSET(n>>23)*{0..7});
  r.val:=INTEGER(BITSET(n)*{0..22}+{23})*4+1;
END un_pack;

PROCEDURE pack(VAR r: real; VAR n: WORD);
BEGIN
  IF 1 IN BITSET(r.val) THEN r.val:=r.val+4 END;
  r.val:=r.val DIV 4;
  IF r.val=0 THEN n:=0; RETURN END;
  IF r.exp<0 THEN error('real underflow'); n:=0; RETURN END;
  IF r.exp>0FFh THEN error('real overflow'); n:=0; RETURN END;
  n:=INTEGER(BITSET(r.val)*{0..22}+BITSET(r.exp<<23)*{23..30});
  IF r.sign THEN n:=INTEGER(BITSET(n)+{31}) END;
END pack;

PROCEDURE normal_pack(VAR r: real; VAR z: WORD);
BEGIN
  IF r.val<0 THEN r.sign:=NOT r.sign; r.val:=-r.val END;
  WHILE r.val>03FFFFFFh DO INC(r.exp); r.val:=r.val DIV 2 END;
  WHILE (r.val#0) & (r.val<2000000h) DO DEC(r.exp); r.val:=r.val*2 END;
  pack(r,z);
END normal_pack;

PROCEDURE shift(VAR r: real; e: INTEGER);
BEGIN
  LOOP
    IF r.val=0 THEN r.exp:=e; RETURN END;
    IF    r.exp<e THEN INC(r.exp); r.val:=r.val DIV 2
    ELSIF r.exp>e THEN
      IF (30 IN BITSET(r.val)) # (31 IN BITSET(r.val)) THEN
        error('integer overflow'); r.exp:=e; RETURN
      END;
      DEC(r.exp); r.val:=r.val * 2
    ELSE RETURN
    END;
  END;
END shift;

PROCEDURE real_plus(x,y: WORD; VAR z: WORD);
  VAR rx,ry: real;
BEGIN
  un_pack(x,rx); un_pack(y,ry);
  IF rx.exp>ry.exp THEN shift(ry,rx.exp) END;
  IF rx.exp<ry.exp THEN shift(rx,ry.exp) END;
  IF rx.sign#ry.sign THEN ry.val:=-ry.val END;
  rx.val:=rx.val+ry.val;
  normal_pack(rx,z);
END real_plus;

PROCEDURE real_minus(x,y: WORD; VAR z: WORD);
  VAR rx,ry: real;
BEGIN
  un_pack(x,rx); un_pack(y,ry);
  IF rx.exp>ry.exp THEN shift(ry,rx.exp) END;
  IF rx.exp<ry.exp THEN shift(rx,ry.exp) END;
  IF rx.sign=ry.sign THEN ry.val:=-ry.val END;
  rx.val:=rx.val+ry.val;
  normal_pack(rx,z);
END real_minus;

PROCEDURE real_star(x,y: WORD; VAR z: WORD);
  VAR rx,ry: real; x0,y0,x1,y1,z0,z1,z2: INTEGER;
BEGIN
  un_pack(x,rx); un_pack(y,ry);
  rx.sign:=rx.sign#ry.sign;
  rx.exp:=rx.exp+ry.exp-152;
  x0:=rx.val MOD 4000h; x1:=rx.val>>14 MOD 4000h;
  y0:=ry.val MOD 4000h; y1:=ry.val>>14 MOD 4000h;
  z1:=x0*y1+y0*x1;
  z0:=x0*y0+(z1 MOD 4000h)<<14;
  z2:=x1*y1+(z1 DIV 4000h);
  WHILE z2#0 DO
    IF ODD(z2) THEN z1:=z1+10000000h END;
    z2:=z2 DIV 2; z1:=z1 DIV 2; INC(rx.exp);
  END;
  rx.val:=z1;
  normal_pack(rx,z);
END real_star;

PROCEDURE real_slash(x,y: WORD; VAR z: WORD);
  VAR res: BITSET; rx,ry: real; i: INTEGER;
BEGIN
  IF y=0 THEN error(div_by_zero); z:=0; RETURN END;
  un_pack(x,rx); un_pack(y,ry);
  rx.sign:=rx.sign#ry.sign;
  rx.exp:=rx.exp-ry.exp+126;
  res:={};
  FOR i:=26 TO 0 BY -1 DO
    IF rx.val>=ry.val THEN rx.val:=rx.val-ry.val; res:=res+{i} END;
    rx.val:=rx.val<<1;
  END;
  rx.val:=INTEGER(res);
  normal_pack(rx,z);
END real_slash;

PROCEDURE float(x: INTEGER; VAR z: WORD);
  VAR r: real;
BEGIN
  r.exp:=152; r.sign:=x<0; r.val:=ABS(x);
  normal_pack(r,z);
END float;

PROCEDURE trunc(x: WORD; VAR z: INTEGER);
  VAR r: real;
BEGIN
  un_pack(x,r); shift(r,152);
  IF r.sign THEN z:=-r.val ELSE z:=r.val END;
END trunc;

PROCEDURE integer_plus(x,y: INTEGER; VAR z: INTEGER);
  VAR c: BOOLEAN; i,n: INTEGER;
BEGIN
  c:=FALSE; z:=0;
  FOR i:=0 TO 31 DO
    n:=ORD(i IN BITSET(x))+ORD(i IN BITSET(y))+ORD(c);
    c:=n>1;
    IF ODD(n) THEN z:=INTEGER(BITSET(z)+{i}) END;
  END;
  IF c#(x<0)#(y<0)#(z<0) THEN z:=0; error('integer overflow') END;
END integer_plus;

PROCEDURE integer_minus(x,y: INTEGER; VAR z: INTEGER);
  VAR c: BOOLEAN; i,n: INTEGER;
BEGIN
  c:=TRUE; z:=0;
  FOR i:=0 TO 31 DO
    n:=ORD(i IN BITSET(x))+ORD(NOT (i IN BITSET(y)))+ORD(c);
    c:=n>1;
    IF ODD(n) THEN z:=INTEGER(BITSET(z)+{i}) END;
  END;
  IF c#(x<0)#(y>=0)#(z<0) THEN z:=0; error('integer overflow') END;
END integer_minus;

PROCEDURE integer_star(x,y: INTEGER; VAR z: INTEGER);
BEGIN
  z:=x*y;
END integer_star;

PROCEDURE integer_slash(x,y: INTEGER; VAR z: INTEGER);
BEGIN
  IF y#0 THEN z:=x/y ELSE z:=0; error(div_by_zero) END;
END integer_slash;

PROCEDURE calc_plus(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  CASE type OF
    |pc.real   : real_plus(x,y,z);
    |pc.integer: integer_plus(x,y,z);
  END;
END calc_plus;

PROCEDURE calc_minus(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  CASE type OF
    |pc.real   : real_minus(x,y,z);
    |pc.integer: integer_minus(x,y,z);
  END;
END calc_minus;

PROCEDURE calc_star(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  CASE type OF
    |pc.real   : real_star(x,y,z);
    |pc.integer: integer_star(x,y,z);
  END;
END calc_star;

PROCEDURE calc_slash(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  CASE type OF
    |pc.real   : real_slash(x,y,z);
    |pc.integer: integer_slash(x,y,z);
  END;
END calc_slash;

PROCEDURE calc_float(type: pc.mode; x: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  float(x,z);
END calc_float;

PROCEDURE calc_trunc(type: pc.mode; x: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  trunc(z,x);
END calc_trunc;

PROCEDURE calc(pos: INTEGER; cop,type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  text_pos:=pos;
  CASE cop OF
    |pc.plus : calc_plus (type,x,y,z);
    |pc.minus: calc_minus(type,x,y,z);
    |pc.slash: calc_slash(type,x,y,z);
    |pc.star : calc_star (type,x,y,z);
    |pc.trunc: calc_trunc(type,x,z);
    |pc.float: calc_float(type,x,z);
    |pc.div  : IF y#0 THEN z:=x DIV y ELSE z:=0; error(div_by_zero) END;
    |pc.mod  : IF y#0 THEN z:=x MOD y ELSE z:=0; error(div_by_zero) END;
    |pc.rem  : IF y#0 THEN z:=x REM y ELSE z:=0; error(div_by_zero) END;
  END;
END calc;

BEGIN
  text_pos:=0;
  pc.gen_const:=calc;
END inNum.
