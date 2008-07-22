$c
(* Conformance *)
DEFINITION MODULE T14D1_D;

TYPE hidden; (* Описание скрытого типа *)

PROCEDURE p(x: hidden): hidden;

END T14D1_D.
$c
IMPLEMENTATION MODULE T14D1_D;

TYPE
  ptr    = POINTER TO INTEGER;
  hidden = ptr;

PROCEDURE p(x: hidden): hidden;
  VAR y: hidden;
      z: ptr;
BEGIN
  y:=x;
  z:=x;
  y:=z;
  RETURN z
END p;

END T14D1_D.
$cx
MODULE T14D1;

FROM T14D1_D IMPORT hidden, p;
IMPORT  IO;

VAR x,y: hidden;

BEGIN
  x:=y;
  y:=p(x);
  ASSERT(INTEGER(y)=INTEGER(x));
END T14D1.
---------------------------------------------------------------
$cx
MODULE Tcase_in_loop;

IMPORT IO;

VAR i,co: INTEGER;

BEGIN
  i:=0; co:=0;
  LOOP
    INC(i,2);
    CASE i OF
      |2: INC(co);
      |3: ASSERT(FALSE,51h);
      |4: INC(co);
      |6: EXIT
    END;
  END;
  IF co=2 THEN IO.WriteString('Pass...case in loop')
  ELSE         IO.WriteString('FAIL...case in loop'); HALT(1);
  END;
END Tcase_in_loop.
--------------------------------------------------

$cx
MODULE Tsize; (*  22-Nov-90. (c) KRONOS *)

IMPORT  IO;

TYPE range = [1..13];
     ct=ARRAY range OF CHAR;
     at=ARRAY range OF INTEGER;

VAR a: at;
    c: ct;
    i: INTEGER;

BEGIN
  ASSERT(SIZE(a)=13,101h);
  ASSERT(LEN(a)=13,102h);
  ASSERT(BYTES(a)=13*4,103h);
  ASSERT(BITS(a)=32*13,104h);
--  ASSERT(LOW(a)=1,105h);
  ASSERT(HIGH(a)=13,106h);
--  ASSERT(LOW(a)=MIN(range),107h);
  ASSERT(HIGH(a)=MAX(range),108h);
  ASSERT(SIZE(range)=1,109h);
  ASSERT(SIZE(c)=(13+3) DIV 4,10Ah);
  ASSERT(LEN(c)=13,10Bh);
  ASSERT(BYTES(c)=SIZE(c)*4,10Ch);
  ASSERT(BITS(c)=SIZE(c)*32,10Dh);
--  ASSERT(LOW(c)=1,10Eh);
  ASSERT(HIGH(c)=13,10Fh);
--  ASSERT(LOW(c)=MIN(range),110h);
  ASSERT(HIGH(c)=MAX(range),111h);
  ASSERT(BYTES(ct)=13,112h);
  ASSERT(BITS(ct)=13*8,113h);
  IO.WriteString('Pass...size')
END Tsize.
--------------------------------------------------

$cx
MODULE T_MEM;

IMPORT  sys: SYSTEM;
IMPORT  IO;

VAR
  p  : POINTER TO RECORD a,b,c: INTEGER END;
  pco: INTEGER;
  d  : DYNARR OF INTEGER;
  dco: INTEGER;

PROCEDURE a(VAR a: sys.ADDRESS; n: INTEGER);
BEGIN
  ASSERT(sys.ADR(a)=sys.ADR(p));
  ASSERT(n=SIZE(p^));
  INC(pco);
END a;

PROCEDURE de(VAR a: sys.ADDRESS; n: INTEGER);
BEGIN
  ASSERT(sys.ADR(a)=sys.ADR(p));
  ASSERT(n=SIZE(p^));
  INC(pco);
END de;

PROCEDURE re(VAR a: sys.ADDRESS; VAR n: INTEGER; l,b: INTEGER);
BEGIN
  ASSERT(a=sys.ADR(d));
  ASSERT(n=HIGH(d));
  n:=l-1;
  INC(dco);
END re;

WITH STORAGE(NEW: a; DISPOSE: de; RESIZE: re);

BEGIN
  pco:=0; dco:=0;
  NEW(d); NEW(d,10); RESIZE(d,20); DISPOSE(d);
  NEW(p);                          DISPOSE(p);
  ASSERT(pco=2);
  ASSERT(dco=3);
END T_MEM.
--------------------------------------------------
