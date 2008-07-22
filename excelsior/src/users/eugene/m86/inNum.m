IMPLEMENTATION MODULE inNum; (* Sem 01-Apr-91. (c) KRONOS *)

IMPORT  pc : pcTab;

PROCEDURE calc_plus(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  z:=INTEGER(REAL(x)+REAL(y));
END calc_plus;

PROCEDURE calc_minus(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  z:=INTEGER(REAL(x)-REAL(y));
END calc_minus;

PROCEDURE calc_star(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  z:=INTEGER(REAL(x)*REAL(y)*4.);
END calc_star;

PROCEDURE calc_slash(type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  z:=INTEGER(REAL(x)/REAL(y)/4.);
END calc_slash;

PROCEDURE calc_float(type: pc.mode; x: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  z:=INTEGER(FLOAT(x)/4.);
END calc_float;

PROCEDURE calc_trunc(type: pc.mode; x: INTEGER; VAR z: INTEGER);
BEGIN
  ASSERT(type=pc.real);
  z:=TRUNC(REAL(x)*4.);
END calc_trunc;

PROCEDURE calc(cop,type: pc.mode; x,y: INTEGER; VAR z: INTEGER);
BEGIN
  CASE cop OF
    |pc.plus : calc_plus (type,x,y,z);
    |pc.minus: calc_minus(type,x,y,z);
    |pc.slash: calc_slash(type,x,y,z);
    |pc.star : calc_star (type,x,y,z);
    |pc.trunc: calc_trunc(type,x,z);
    |pc.float: calc_float(type,x,z);
  END;
END calc;

BEGIN
  pc.gen_const:=calc;
END inNum.
