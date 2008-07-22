MODULE tt; (* 26-Sep-88. (c) KRONOS *)

FROM Terminal   IMPORT  print, Read, SetTermNo, TransparentIn,
                        TransparentOut, Write, BusyRead;

VAR ch: CHAR;
VAR i,j: INTEGER;

BEGIN
  i:=0;
  SetTermNo(3);
  TransparentIn(TRUE);
  TransparentOut(TRUE);
  LOOP
    Write(CHAR(i*30+1));
    FOR j:=0 TO 2000 DO END;
    ch:=BusyRead();
    WHILE ch#0c DO
      SetTermNo(0); print('%2$h  ',ch);
      IF ch=CHAR(24h) THEN print('\n') END; SetTermNo(3);
      ch:=BusyRead();
    END;
    Write(CHAR(0E0h+INTEGER({i})));
    FOR j:=0 TO 2000 DO END;
    i:=(i+1) MOD 5;
    ch:=BusyRead();
    IF ch#0c THEN
      SetTermNo(0); print('%2$h  ',ch);
      IF ch=CHAR(24h) THEN print('\n') END; SetTermNo(3);
    END;
  END;
END tt.
