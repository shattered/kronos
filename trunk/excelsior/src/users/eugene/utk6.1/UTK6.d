DEFINITION MODULE UTK6; (* 27-Dec-89. (c) KRONOS *)

PROCEDURE init;

PROCEDURE clear;

PROCEDURE power(on: BOOLEAN; val: INTEGER);
-- val = -2    -10%
-- val = -1    -5%
-- val =  0    normal
-- val =  1    +5%
-- val =  2    +10%

PROCEDURE output(SEQ n: INTEGER);

PROCEDURE input(SEQ n: INTEGER);

PROCEDURE a(n,val: INTEGER);

PROCEDURE b(n,val: INTEGER);

PROCEDURE b?(n,val: INTEGER): BOOLEAN;

END UTK6.
