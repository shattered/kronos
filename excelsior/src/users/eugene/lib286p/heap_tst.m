MODULE t; (*  07-May-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS;

IMPORT  tty : Terminal;
IMPORT  arg : tskArgs;
IMPORT  mem : Heap;

VAR s: STRING; i: INTEGER;

PROCEDURE test_heap;
  VAR
    a : ARRAY [0..999] OF ADDRESS;
    n : INTEGER;
    al,de: INTEGER;
    pass : INTEGER;
BEGIN
  tty.write('heap test'15c 12c,0,11);
  FOR n:=0 TO HIGH(a) DO a[n]:=NIL END;
  n:=0; al:=0; de:=0;
  FOR pass:=0 TO 9999 DO
    IF a[n]=NIL THEN
      mem.ALLOCATE(a[n],4); a[n]^:=n; INC(al);
    ELSE
      IF INTEGER(a[n]^)#n THEN tty.print('a[n]^#n\n') END;
      mem.DEALLOCATE(a[n],4); INC(de);
    END;
    n:=((n+13)*11) MOD LEN(a);
  END;
  tty.print('alloc %d, dealloc %d\n',al,de);
END test_heap;

BEGIN
--  test_heap;
  IF arg.number('num',i) THEN
    tty.print('num=%d\n',i);
  ELSE
    tty.print('num undef\n');
  END;
  IF arg.string('str',s) THEN
    tty.print('str=%s\n',s);
  ELSE
    tty.print('str undef\n');
  END;
  FOR i:=0 TO HIGH(arg.words) DO
    tty.print('%s\n',arg.words[i]);
  END;
END t.
