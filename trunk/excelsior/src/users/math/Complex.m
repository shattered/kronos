IMPLEMENTATION MODULE Complex; (* 28-Apr-88. (c) KRONOS *)

                IMPORT Heap;

CONST good = 123456789;

TYPE complex = POINTER TO
                 RECORD
                   re, im: REAL;
                   temp  : INTEGER; -- >0 if temporary
                 END;

PROCEDURE new(VAR z: complex);
BEGIN
  Heap.ALLOCATE(z, SIZE(z^));
  z^.temp:=good;
END new;

PROCEDURE drop(z: complex);
BEGIN
  IF (z#NIL) & (z^.temp=good) THEN Heap.DEALLOCATE(z, SIZE(z^)) END
END drop;

PROCEDURE !(re,im: REAL): complex;
  VAR z: complex;
BEGIN
  new(z); z^.re:=re; z^.im:=im; RETURN z
END !;

PROCEDURE re(z: complex): REAL;
  VAR r: REAL;
BEGIN
  r:=z^.re; drop(z); RETURN r
END re;

PROCEDURE im(z: complex): REAL;
  VAR r: REAL;
BEGIN
  r:=z^.im; drop(z); RETURN r
END im;

PROCEDURE asg(VAR z: complex; w: complex);
BEGIN
  IF (z=NIL) OR (ABS(z^.temp)#good) THEN new(z) END;
  z^:=w^; z^.temp:=-good; drop(w);
END asg;

PROCEDURE _(z: complex): complex;
  VAR z1: complex;
BEGIN
  IF z^.temp=good THEN z^.im:=-z^.im; RETURN z END;
  new(z1); z1^.re:=z^.re; z1^.im:=-z^.im;
  RETURN z1
END _;

PROCEDURE add(z1,z2: complex): complex;
  VAR z: complex;
BEGIN new(z);
  z^.re:=z1^.re+z2^.re;
  z^.im:=z1^.im+z2^.im;
  drop(z1); drop(z2);
  RETURN z
END add;

PROCEDURE sub(z1,z2: complex): complex;
  VAR z: complex;
BEGIN new(z);
  z^.re:=z1^.re-z2^.re;
  z^.im:=z1^.im-z2^.im;
  drop(z1); drop(z2);
  RETURN z
END sub;

PROCEDURE mul(z1,z2: complex): complex;
  VAR z: complex;
BEGIN new(z);
  z^.re:=z1^.re*z2^.re-z1^.im*z2^.im;
  z^.im:=z1^.re*z2^.im+z1^.im*z2^.re;
  drop(z1); drop(z2);
  RETURN z
END mul;

PROCEDURE div(z1,z2: complex): complex;
  VAR z: complex; d: REAL;
BEGIN new(z);
  d:=z2^.re*z2^.re+z2^.im*z2^.im;
  z^.re:=(z1^.re*z2^.re+z1^.im*z2^.im)/d;
  z^.im:=(z1^.re*z2^.im-z1^.im*z2^.re)/d;
  drop(z1); drop(z2);
  RETURN z
END div;

PROCEDURE eq(z1,z2: complex): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  r:=(z1^.re=z2^.re) & (z1^.im=z2^.im);
  drop(z1); drop(z2);
  RETURN r
END eq;

END Complex.
