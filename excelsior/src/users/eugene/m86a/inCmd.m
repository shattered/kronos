IMPLEMENTATION MODULE inCmd; (* Sem 26-Feb-91. (c) KRONOS *)

IMPORT  mem : pcSystem;

WITH STORAGE : mem;

PROCEDURE b(n: INTEGER);
BEGIN
  IF cnt>HIGH(code) THEN RESIZE(code,BYTES(code)+4096) END;
  code[cnt]:=CHAR(n); INC(cnt);
END b;

PROCEDURE w(n: INTEGER);
BEGIN
  IF cnt+1>HIGH(code) THEN RESIZE(code,BYTES(code)+4096) END;
  code[cnt]:=CHAR(n); INC(cnt);
  code[cnt]:=CHAR(n>>8); INC(cnt);
END w;

PROCEDURE new_code(n: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  IF cnt+n-1>HIGH(code) THEN RESIZE(code,BYTES(code)+n+4096) END;
  i:=cnt; INC(cnt,n);
  RETURN i;
END new_code;

BEGIN
  cnt:=0; NEW(code);
END inCmd.
