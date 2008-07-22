DEFINITION MODULE InOut; (* Leo 24-Jun-88. (c) KRONOS *)


PROCEDURE WriteString(s: ARRAY OF CHAR; width: INTEGER);
PROCEDURE WriteInt(val: INTEGER; width: INTEGER);
PROCEDURE WriteFix(val: REAL; width,dots: INTEGER);
PROCEDURE WriteLn;

END InOut.
