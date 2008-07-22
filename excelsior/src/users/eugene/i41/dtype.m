MODULE dtype; (* 06-Oct-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM FsDrv      IMPORT  DoCFE, DevDrv;
FROM Args       IMPORT  TakeWord, NumFlag?;
FROM Terminal   IMPORT  print;

VAR i   : RECORD h,t,sec,secsize: INTEGER END;
    name: ARRAY [0..79] OF CHAR;
    d   : INTEGER;

BEGIN
  i.t:=0; i.h:=0; i.sec:=0; i.secsize:=0;
  IF NumFlag?('t',i.t)       THEN END;
  IF NumFlag?('h',i.h)       THEN END;
  IF NumFlag?('s',i.sec)     THEN END;
  IF NumFlag?('z',i.secsize) THEN END;
  TakeWord(name);
  IF name='' THEN
    print('  dtype <dev name> t=<tracks> h=<heads> s=<secno> z=<secsize>\n');
    HALT
  END;
  IF DevDrv(name,d) THEN print('No such drive.\n'); HALT END;
  IF DoCFE(d,6,ADR(i)) THEN print('No such CFE.\n'); HALT END;
END dtype.
