MODULE echo; (*  16-Aug-90. (c) KRONOS *)

IMPORT   io: StdIO;
IMPORT  env: tskEnv;
IMPORT  sci: ASCII;

VAR parm: STRING;
       i: INTEGER;

BEGIN
  env.get_str("ARGS",parm);
  IF env.done THEN
    i:=0;
    WHILE (i<HIGH(parm)) & (parm[i]#0c) DO
      IF (parm[i]="\") & (parm[i+1]="n") THEN
        parm[i]:=sci.CR; INC(i);
        parm[i]:=sci.LF
      END;
      INC(i)
    END;
    io.print("%s",parm)
  END
END echo.
