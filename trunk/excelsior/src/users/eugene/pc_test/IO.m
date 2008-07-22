IMPLEMENTATION MODULE IO; (* 02-Aug-88. (c) KRONOS *)

IMPORT      SYSTEM;
IMPORT  io: StdIO;
IMPORT  tm: Time;

PROCEDURE w(c: CHAR); BEGIN io.Write(c) END w;

PROCEDURE ws(VAL s: ARRAY OF CHAR); BEGIN io.WriteString(s) END ws;

PROCEDURE wi(n: INTEGER); BEGIN io.print('%d',n) END wi;

PROCEDURE time(): INTEGER; BEGIN RETURN tm.sys_time(tm.milisec) END time;

PROCEDURE print(VAL s: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN io.print(s,args);
END print;

PROCEDURE WriteString(VAL s: ARRAY OF CHAR);
BEGIN io.WriteString(s) END WriteString;

PROCEDURE WriteLn; BEGIN io.WriteLn END WriteLn;

PROCEDURE WriteInt(val: INTEGER; width: INTEGER);
BEGIN io.print("%*d",width,val) END WriteInt;

PROCEDURE WriteFix(val: REAL; width,dots: INTEGER);
BEGIN io.print("%*.*f",width,dots,val) END WriteFix;

END IO.
