MODULE x;

IMPORT lowLevel;

TYPE big=ARRAY [0..255] OF ARRAY [0..63] OF INTEGER;

VAR a: big;
    b: big;
    c: big;


PROCEDURE zero(VAR a: big);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(a) DO
    FOR j:=0 TO HIGH(a[i]) DO a[i,j]:=0 END
  END
END zero;

BEGIN
  zero(a);
  zero(b);
  zero(c)
END x.
