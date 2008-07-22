DEFINITION MODULE cal; (* Dima 22-Mar-88. (c) KRONOS *)

TYPE Mode = (int2, int8, int10, int16, real, bitset);

PROCEDURE Mode?():Mode;
PROCEDURE Mode!(m:Mode);

PROCEDURE Kalk(VAL s:ARRAY OF CHAR; VAR out:ARRAY OF CHAR);

END cal.
