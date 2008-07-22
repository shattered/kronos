DEFINITION MODULE Complex; (* 28-Apr-88. (c) KRONOS *)

TYPE complex;

PROCEDURE asg(VAR z: complex; w: complex);

PROCEDURE re(z: complex): REAL;
PROCEDURE im(z: complex): REAL;
PROCEDURE !(re,im: REAL): complex;

PROCEDURE _(z: complex): complex;
PROCEDURE add(z1,z2: complex): complex;
PROCEDURE sub(z1,z2: complex): complex;
PROCEDURE mul(z1,z2: complex): complex;
PROCEDURE div(z1,z2: complex): complex;

PROCEDURE eq(z1,z2: complex): BOOLEAN;

END Complex.
