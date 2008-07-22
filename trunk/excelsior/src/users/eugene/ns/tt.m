MODULE tt; (*  24-Jan-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

VAR
  i: INTEGER;

BEGIN
  CASE i OF
    |2: i:=2;
    |4: i:=4;
    |5: i:=5;
  ELSE
    i:=-1;
  END;
END tt.
