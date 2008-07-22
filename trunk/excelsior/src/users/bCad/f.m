MODULE f; (*  11-Sep-91. (c) KRONOS *)

IMPORT  txt: bcText;
IMPORT  sys: SYSTEM;
IMPORT  mem: Heap;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  io : StdIO;
IMPORT  tty: Terminal;
IMPORT  arg: tskArgs;

CONST items = 7;

WITH STORAGE (NEW    : mem.ALLOCATE;
              DISPOSE: mem.DEALLOCATE;
              RESIZE : mem.REALLOCATE);

VAR main : txt.PFONT;
    ch   : CHAR;
    cnt,i: INTEGER;

PROCEDURE out(i: INTEGER);
BEGIN
  IF cnt=0 THEN io.print('\n  0%$8hh,',i)
  ELSE io.print('0%$8hh,',i) END;
  cnt:=(cnt+1) MOD items;
END out;

VAR a: sys.ADDRESS;

BEGIN
  IF HIGH(arg.words)<0 THEN HALT END;
  txt.load(main,arg.words[0]);
  io.print('CONST MAIN = ARRAY OF INTEGER {\n');
  cnt:=0;
  WITH main^ DO
    io.print('  0%8$hh,0%8$hh,',w,h);
--    FOR ch:=0c TO 377c DO out(propW[ch])  END;
    (*$U+*)
    FOR ch:=0c TO 377c DO out(ptr[ch]^.ADR); out(ptr[ch]^.HIGH) END;
    (*$U-*)
    cnt:=0;
    FOR ch:=0c TO 377c DO
      FOR i:=0 TO HIGH(ptr[ch]) DO out(ptr[ch][i]); END;
    END;
  END;
END f.
