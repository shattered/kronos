IMPLEMENTATION MODULE inNum;

IMPORT inExp, pcK;

FROM pcM   IMPORT ALLOCATE;
FROM pcK   IMPORT SET32;

CONST
  exp ::= inExp;
  pc  ::= pcK;

TYPE
  real  = RECORD
    sign: BOOLEAN;
    exp : LONGINT;
    val : LONGINT;
  END;


CONST
  err_ill_op   = 0;
  err_neq_type = 0;
  err_num_ovr  = 0;

PROCEDURE err_ps(ps,n: LONGINT);
BEGIN
  HALT;
END err_ps;

PROCEDURE equ_str(x,y: pc.VALUE; VAR z: pc.VALUE);
BEGIN
  WHILE (x.r^.sc#0) & (x.r^.sc=y.r^.sc) DO
    x.r:=ADDRESS(LONGCARD(x)+1); y.r:=ADDRESS(LONGCARD(y)+1);
  END;
  z.n:=LONGINT(x.r^.sc=y.r^.sc);
END equ_str;

PROCEDURE equ_bin(x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  WHILE sz#0 DO
    IF x.r^.sc#y.r^.sc THEN z.n:=0; RETURN END;
    x.r:=ADDRESS(LONGCARD(x)+1); y.r:=ADDRESS(LONGCARD(y)+1);
    DEC(sz);
  END;
  z.n:=1;
END equ_bin;

PROCEDURE leq_real(x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF sz=4 THEN z.n:=LONGINT(x.r^.r<=y.r^.r)
  ELSE z.n:=LONGINT(x.r^.lr<=y.r^.lr)
  END;
END leq_real;

PROCEDURE leq_set(x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
  VAR i: CARDINAL; c: BOOLEAN;
BEGIN
  c:=FALSE;
  WHILE sz>0 DO
    FOR i:=0 TO 7 DO
      c:=c OR (i IN x.r^.s8) & NOT (i IN y.r^.s8);
      x.r:=ADDRESS(LONGCARD(x)+1); y.r:=ADDRESS(LONGCARD(y)+1);
    END;
    DEC(sz);
  END;
  z.n:=LONGINT(NOT c);
END leq_set;

PROCEDURE leq_str(x,y: pc.VALUE; VAR z: pc.VALUE);
BEGIN
  WHILE (x.r^.sc#0) & (x.r^.sc=y.r^.sc) DO
    x.r:=ADDRESS(LONGCARD(x)+1); y.r:=ADDRESS(LONGCARD(y)+1);
  END;
  z.n:=LONGINT(x.r^.sc<=y.r^.sc);
END leq_str;

PROCEDURE int_in(x,y: pc.VALUE; VAR z: pc.VALUE; a,b: LONGINT);
BEGIN
  IF (x.n<a) OR (x.n>b) THEN z.n:=0; RETURN END;
  x.n:=x.n-a;
  y.r:=ADDRESS(LONGINT(x)+x.n DIV 8);
  z.n:=LONGINT(CARDINAL(x.n MOD 8) IN y.r^.s8);
END int_in;

PROCEDURE int_check(ps: LONGINT; x: pc.VALUE; sz: LONGINT);
  VAR b: BOOLEAN;
BEGIN
  CASE CARDINAL(sz) OF
    |1: b:=(x.n>=-80H) & (x.n<=7FH);
    |2: b:=(x.n>=-8000H) & (x.n<=7FFFH);
    |4: b:=TRUE;
  ELSE err_ps(ps,err_ill_op); RETURN;
  END;
  IF NOT b THEN err_ps(ps,err_num_ovr) END;
END int_check;

PROCEDURE car_check(ps: LONGINT; x: pc.VALUE; sz: LONGINT);
  VAR b: BOOLEAN;
BEGIN
  CASE CARDINAL(sz) OF
    |1: b:=(x.n>=0H) & (x.n<=0FFH);
    |2: b:=(x.n>=0H) & (x.n<=0FFFFH);
    |4: b:=(x.n>=0H);
  ELSE err_ps(ps,err_ill_op); RETURN;
  END;
  IF NOT b THEN err_ps(ps,err_num_ovr) END;
END car_check;

PROCEDURE mul_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  z.n:=x.n*y.n;
  int_check(ps,z,sz);
END mul_int;

PROCEDURE mul_car(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  z.n:=x.n*y.n;
  car_check(ps,z,sz);
END mul_car;

PROCEDURE div_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF y.n=0 THEN err_ps(ps,err_num_ovr); RETURN END;
  z.n:=x.n DIV y.n;
  IF ((x.n<0)#(y.n<0))&(x.n*y.n#z.n) THEN INC(z.n) END;
  int_check(ps,z,sz);
END div_int;

PROCEDURE quo_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF y.n=0 THEN err_ps(ps,err_num_ovr); RETURN END;
  z.n:=x.n DIV y.n;
  int_check(ps,z,sz);
END quo_int;

PROCEDURE quo_car(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF y.n=0 THEN err_ps(ps,err_num_ovr); RETURN END;
  z.n:=x.n DIV y.n;
  car_check(ps,z,sz);
END quo_car;

PROCEDURE quo_real(ps: LONGINT; x,y,z: pc.VALUE; sz: LONGINT);
BEGIN
  CASE CARDINAL(sz) OF
    |4: IF y.r^.r=0.0 THEN err_ps(ps,err_num_ovr); RETURN END;
        z.r^.r:=x.r^.r/y.r^.r;
    |8: IF y.r^.lr=0.0 THEN err_ps(ps,err_num_ovr); RETURN END;
        z.r^.lr:=x.r^.lr/y.r^.lr;
  ELSE err_ps(ps,err_ill_op);
  END;
END quo_real;

PROCEDURE mod_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF y.n=0 THEN err_ps(ps,err_num_ovr); RETURN END;
  z.n:=x.n MOD y.n;
  int_check(ps,z,sz);
END mod_int;

PROCEDURE rem_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF y.n=0 THEN err_ps(ps,err_num_ovr); RETURN END;
  z.n:=x.n MOD y.n;
  int_check(ps,z,sz);
END rem_int;

PROCEDURE rem_car(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
BEGIN
  IF y.n=0 THEN err_ps(ps,err_num_ovr); RETURN END;
  z.n:=x.n MOD y.n;
  car_check(ps,z,sz);
END rem_car;

PROCEDURE plus_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
  VAR c: BOOLEAN; i,n: CARDINAL;
BEGIN
  c:=FALSE; z.n:=0;
  FOR i:=0 TO 31 DO
    n:=ORD(i IN SET32(x))+ORD(i IN SET32(y))+ORD(c);
    c:=n>1;
    IF ODD(n) THEN z.n:=LONGINT(SET32(z)+SET32{i}) END;
  END;
  IF ((c#(x.n<0))#(y.n<0))#(z.n<0) THEN z.n:=0; err_ps(ps,err_num_ovr) END;
  int_check(ps,z,sz);
END plus_int;

PROCEDURE plus_car(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
  VAR c: BOOLEAN; i,n: CARDINAL;
BEGIN
  c:=FALSE; z.n:=0;
  FOR i:=0 TO 31 DO
    n:=ORD(i IN SET32(x))+ORD(i IN SET32(y))+ORD(c);
    c:=n>1;
    IF ODD(n) THEN z.n:=LONGINT(SET32(z)+SET32{i}) END;
  END;
  IF ((c#(x.n<0))#(y.n<0))#(z.n<0) THEN z.n:=0; err_ps(ps,err_num_ovr) END;
  car_check(ps,z,sz);
END plus_car;

PROCEDURE minus_int(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
  VAR c: BOOLEAN; i,n: CARDINAL;
BEGIN
  c:=TRUE; z.n:=0;
  FOR i:=0 TO 31 DO
    n:=ORD(i IN SET32(x))+ORD(NOT (i IN SET32(y)))+ORD(c);
    c:=n>1;
    IF ODD(n) THEN z.n:=LONGINT(SET32(z)+SET32{i}) END;
  END;
  IF ((c#(x.n<0))#(y.n>=0))#(z.n<0) THEN z.n:=0; err_ps(ps,err_num_ovr) END;
  int_check(ps,z,sz);
END minus_int;

PROCEDURE minus_car(ps: LONGINT; x,y: pc.VALUE; VAR z: pc.VALUE; sz: LONGINT);
  VAR c: BOOLEAN; i,n: CARDINAL;
BEGIN
  c:=TRUE; z.n:=0;
  FOR i:=0 TO 31 DO
    n:=ORD(i IN SET32(x))+ORD(NOT (i IN SET32(y)))+ORD(c);
    c:=n>1;
    IF ODD(n) THEN z.n:=LONGINT(SET32(z)+SET32{i}) END;
  END;
  IF ((c#(x.n<0))#(y.n>=0))#(z.n<0) THEN z.n:=0; err_ps(ps,err_num_ovr) END;
  car_check(ps,z,sz);
END minus_car;

PROCEDURE un_pack_short(n: pc.VALUE; VAR r: real);
BEGIN
  IF n.r^.s32=SET32{} THEN r.sign:=FALSE; r.val:=0; r.exp:=0; RETURN END;
  r.sign:=31 IN n.r^.s32;
  r.exp:=LONGINT(SET32(LONGINT(n.r^.s32)>>23)*SET32{0..7});
  r.val:=LONGINT(SET32(n)*SET32{0..22}+SET32{23})*4+1;
END un_pack_short;

PROCEDURE un_pack_long(n: pc.VALUE; VAR r: real);
BEGIN
  n.r:=ADDRESS(LONGINT(n)+4);
  un_pack_short(n,r);
END un_pack_long;

PROCEDURE pack_short(ps: LONGINT; VAR r: real; n: pc.VALUE);
BEGIN
  IF 1 IN SET32(r.val) THEN r.val:=r.val+4 END;
  r.val:=r.val DIV 4;
  IF r.val=0 THEN n.r^.s32:=SET32{}; RETURN END;
  IF r.exp<0 THEN err_ps(ps,err_num_ovr); n.r^.s32:=SET32{}; RETURN END;
  IF r.exp>0FFH THEN err_ps(ps,err_num_ovr); n.r^.s32:=SET32{}; RETURN END;
  n.r^.s32:=SET32(r.val)*SET32{0..22}+SET32(r.exp<<23)*SET32{23..30};
  IF r.sign THEN n.r^.s32:=n.r^.s32+SET32{31} END;
END pack_short;

PROCEDURE pack_long(ps: LONGINT; VAR r: real; n: pc.VALUE);
BEGIN
  err_ps(ps,err_ill_op);
END pack_long;

PROCEDURE normal_pack(ps: LONGINT; VAR r: real; z: pc.VALUE; sz: LONGINT);
BEGIN
  IF r.val<0 THEN r.sign:=NOT r.sign; r.val:=-r.val END;
  WHILE r.val>03FFFFFFH DO INC(r.exp); r.val:=r.val DIV 2 END;
  WHILE (r.val#0) & (r.val<2000000H) DO DEC(r.exp); r.val:=r.val*2 END;
  CASE CARDINAL(sz) OF
    |4: pack_short(ps,r,z);
    |8: pack_long (ps,r,z);
  ELSE err_ps(ps,err_ill_op);
  END;
END normal_pack;

PROCEDURE shift_real(ps: LONGINT; VAR r: real; e: LONGINT);
BEGIN
  LOOP
    IF r.val=0 THEN r.exp:=e; RETURN END;
    IF    r.exp<e THEN INC(r.exp); r.val:=r.val DIV 2
    ELSIF r.exp>e THEN
      IF (30 IN SET32(r.val)) # (31 IN SET32(r.val)) THEN
        err_ps(ps,err_num_ovr); r.exp:=e; RETURN
      END;
      DEC(r.exp); r.val:=r.val * 2
    ELSE RETURN
    END;
  END;
END shift_real;

PROCEDURE un_pack(ps: LONGINT; x: pc.VALUE; VAR r: real; sz: LONGINT);
BEGIN
  CASE CARDINAL(sz) OF
    |4: un_pack_short(x,r);
    |8: un_pack_long(x,r);
  ELSE err_ps(ps,err_ill_op);
  END;
END un_pack;

PROCEDURE plus_real(ps: LONGINT; x,y,z: pc.VALUE; sz: LONGINT);
  VAR rx,ry: real;
BEGIN
  un_pack(ps,x,rx,sz); un_pack(ps,y,ry,sz);
  IF rx.exp>ry.exp THEN shift_real(ps,ry,rx.exp) END;
  IF rx.exp<ry.exp THEN shift_real(ps,rx,ry.exp) END;
  IF rx.sign#ry.sign THEN ry.val:=-ry.val END;
  rx.val:=rx.val+ry.val;
  normal_pack(ps,rx,z,sz);
END plus_real;

PROCEDURE minus_real(ps: LONGINT; x,y,z: pc.VALUE; sz: LONGINT);
  VAR rx,ry: real;
BEGIN
  un_pack(ps,x,rx,sz); un_pack(ps,y,ry,sz);
  IF rx.exp>ry.exp THEN shift_real(ps,ry,rx.exp) END;
  IF rx.exp<ry.exp THEN shift_real(ps,rx,ry.exp) END;
  IF rx.sign=ry.sign THEN ry.val:=-ry.val END;
  rx.val:=rx.val+ry.val;
  normal_pack(ps,rx,z,sz);
END minus_real;

PROCEDURE mul_real(ps: LONGINT; x,y,z: pc.VALUE; sz: LONGINT);
  VAR rx,ry: real; x0,y0,x1,y1,z0,z1,z2: LONGINT;
BEGIN
  un_pack(ps,x,rx,sz); un_pack(ps,y,ry,sz);
  rx.sign:=rx.sign#ry.sign;
  rx.exp:=rx.exp+ry.exp-152;
  x0:=rx.val MOD 4000H; x1:=rx.val>>14 MOD 4000H;
  y0:=ry.val MOD 4000H; y1:=ry.val>>14 MOD 4000H;
  z1:=x0*y1+y0*x1;
  z0:=x0*y0+(z1 MOD 4000H)<<14;
  z2:=x1*y1+(z1 DIV 4000H);
  WHILE z2#0 DO
    IF ODD(z2) THEN z1:=z1+10000000H END;
    z2:=z2 DIV 2; z1:=z1 DIV 2; INC(rx.exp);
  END;
  rx.val:=z1;
  normal_pack(ps,rx,z,sz);
END mul_real;

(*
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

PROCEDURE binary(n: pc.NODE; x,y: pc.VALUE; VAR z: pc.VALUE);
  VAR m: exp.val_mode; a,b: LONGINT;
BEGIN
  IF n^.type^.mode IN pc.SCALARs THEN z.n:=0;
  ELSE ALLOCATE(z.r,n^.type^.size);
  END;
  m:=exp.vm(n^.l^.type);
  CASE n^.sub OF
    |pc.equ,pc.neq:
      IF m=exp.vm_string THEN equ_str(x,y,z)
      ELSIF n^.l^.type^.size#n^.r^.type^.size THEN err_ps(n^.pos,err_neq_type)
      ELSE equ_bin(x,y,z,n^.l^.type^.size)
      END;
      IF n^.sub=pc.neq THEN z.n:=1-z.n END;
    |pc.leq,pc.gtr:
      IF (m#exp.vm_string) & (n^.l^.type^.size#n^.r^.type^.size) THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_cardinal,exp.vm_integer,exp.vm_boolean,exp.vm_address:
          z.n:=LONGINT(x.n<=y.n);
        |exp.vm_real    : leq_real(x,y,z,n^.l^.type^.size);
        |exp.vm_string  : leq_str (x,y,z);
        |exp.vm_set     : leq_set (x,y,z,n^.l^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
      IF n^.sub=pc.gtr THEN z.n:=1-z.n END;
    |pc.geq,pc.lss:
      IF (m#exp.vm_string) & (n^.l^.type^.size#n^.r^.type^.size) THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_cardinal,exp.vm_integer,exp.vm_boolean,exp.vm_address:
          z.n:=LONGINT(x.n>=y.n);
        |exp.vm_real    : leq_real(y,x,z,n^.l^.type^.size);
        |exp.vm_string  : leq_str (y,x,z);
        |exp.vm_set     : leq_set (y,x,z,n^.l^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
      IF n^.sub=pc.lss THEN z.n:=1-z.n END;
    |pc.in   :
      exp.min_max(n^.r^.type,a,b);
      int_in(x,y,z,a,b);
    |pc.mul  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : mul_int(n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: mul_car(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.div  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : div_int(n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: quo_car(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.slash:
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : quo_int (n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: quo_car (n^.pos,x,y,z,n^.type^.size);
        |exp.vm_real    : quo_real(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.mod  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : mod_int(n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: rem_car(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.rem  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : rem_int(n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: rem_car(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.plus :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : plus_int (n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: plus_car (n^.pos,x,y,z,n^.type^.size);
        |exp.vm_real    : plus_real(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.minus:
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      CASE m OF
        |exp.vm_integer : minus_int (n^.pos,x,y,z,n^.type^.size);
        |exp.vm_cardinal: minus_car (n^.pos,x,y,z,n^.type^.size);
        |exp.vm_real    : minus_real(n^.pos,x,y,z,n^.type^.size);
      ELSE err_ps(n^.pos,err_ill_op);
      END;
    |pc.and  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      IF m#exp.vm_set THEN err_ps(n^.pos,err_ill_op); RETURN END;
      and_set(x,y,z,n^.l^.type^.size);
    |pc.or   :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      IF m#exp.vm_set THEN err_ps(n^.pos,err_ill_op); RETURN END;
      or_set(x,y,z,n^.l^.type^.size);
    |pc.xor  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      IF m#exp.vm_set THEN err_ps(n^.pos,err_ill_op); RETURN END;
      xor_set(x,y,z,n^.l^.type^.size);
    |pc.bic  :
      IF n^.l^.type^.size#n^.r^.type^.size THEN
        err_ps(n^.pos,err_neq_type); RETURN;
      END;
      IF m#exp.vm_set THEN err_ps(n^.pos,err_ill_op); RETURN END;
      bic_set(x,y,z,n^.l^.size);
    |pc.cand :
    |pc.cor  :
    |pc.rol  :
    |pc.ror  :
    |pc.ash  :
  ELSE err_ps(n^.pos,err_ill_op);
  END;
END binary;

END inNum.