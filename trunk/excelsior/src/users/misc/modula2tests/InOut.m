IMPLEMENTATION MODULE InOut; (* Leo 24-Jun-88. (c) KRONOS *)

IMPORT  io: StdIO;

PROCEDURE WriteString(VAL s: ARRAY OF CHAR; width: INTEGER);
BEGIN io.print("%-*s",width,s)
END WriteString;

PROCEDURE WriteInt(val: INTEGER; width: INTEGER);
BEGIN io.print("%*d",width,val) END WriteInt;

PROCEDURE WriteFix(val: REAL; width,dots: INTEGER);
BEGIN io.print("%*.*f",width,dots,val) END WriteFix;

PROCEDURE WriteLn;
BEGIN io.print("\n") END WriteLn;

END InOut.
