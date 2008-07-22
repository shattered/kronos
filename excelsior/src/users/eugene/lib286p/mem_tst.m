MODULE mem_tst; (*  07-May-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS;

IMPORT  tty : Terminal;
IMPORT  def : def_GDT;
IMPORT  low : lowLevel;

VAR a: ADDRESS; i,j: INTEGER;

BEGIN
  FOR i:=0 TO 3Fh BY 4  DO
    a:=def.mem_a+i*10000h;
    tty.print('%$2h0000: ',i+10h);
    FOR j:=0 TO 3 DO tty.print(' %$8h',a^); a:=a+4 END;
    tty.print('\n');
--    low._zero(a,10000h);
  END;
END mem_tst.
