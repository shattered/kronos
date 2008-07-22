MODULE zero;  -- makes things easier for WinZip to compress

IMPORT bio: BIO;
IMPORT stdio: StdIO;
IMPORT SYSTEM;


VAR buf: ARRAY [0..4095] OF CHAR;
      i: INTEGER;
      f: bio.FILE;
    s,u: INTEGER;
BEGIN
(*
  bio.du(bio.cd, s, u);
  i := s;
  REPEAT
    DEC(i, 4*1024);
    stdio.print("%5dK    \r", i / 1024);
    bio.create(f, "", "w", i);
  UNTIL bio.done OR (i = 0);
*)
  bio.create(f, "", "w", 0);
  IF NOT bio.done THEN HALT(bio.error) END;
  FOR i := 0 TO HIGH(buf) DO buf[i] := 0c END;
  i := 0;
  LOOP
    bio.write(f, SYSTEM.ADR(buf), BYTES(buf));
    IF NOT bio.done THEN EXIT END;
    INC(i, BYTES(buf));
    IF i MOD 16*BYTES(buf) = 0 THEN
       stdio.print("%5dK   \r", i DIV 1024);
    END
  END;
  stdio.print("\n");
  bio.close(f);
  HALT(bio.error);
END zero.
