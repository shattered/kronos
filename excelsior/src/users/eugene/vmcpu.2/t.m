MODULE t; (* 06-Jun-88. (c) KRONOS *)

FROM intRAM     IMPORT  SetAdr, CoreRD, CoreWR;

VAR i,j,k: INTEGER;

BEGIN
--FOR i:=0 TO 63  DO SetAdr(i*1024,i*1024,FALSE) END;
  FOR i:=0 TO 127 DO CoreWR(i,0) END;
  FOR i:=0 TO 127 DO j:=CoreRD(i) END;
END t.
