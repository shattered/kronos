MODULE t; (*  25-Jan-01. (c) KRONOS *)

IMPORT Terminal;


  VAR s1: ARRAY [0..10] OF CHAR;
  VAR s2: ARRAY [0..10] OF CHAR;
BEGIN

  s1 := "ABC";
  s2 := "DEF";
  IF s1 > s2 THEN
    Terminal.print('"ABC" > "DEF"?!\n');
  END;
END t.
