IMPLEMENTATION MODULE deBug; (* Leo 19-Dec-88. (c) KRONOS *)

IMPORT      SYSTEM;
IMPORT cod: defCodes;
IMPORT fmt: Formats;

TYPE WORD = SYSTEM.WORD;

PROCEDURE inp(adr: WORD): BITSET; CODE cod.inp END inp;
PROCEDURE out(adr, data: WORD);   CODE cod.out END out;
PROCEDURE di(): BITSET;  CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;


VAR on: BOOLEAN; (* Qbus present *)

PROCEDURE write_str(xxx: INTEGER; VAL s: ARRAY OF CHAR; i,len: INTEGER);
  VAR m: BITSET;
     ch: CHAR;
BEGIN
  m:=di();
  WHILE (i<=HIGH(s)) & (len>0) DO
    ch:=s[i];
    REPEAT UNTIL inp(177564b DIV 2)*{7}#{};
    IF ch#36c THEN
      out(177566b DIV 2,ch)
    ELSE
      out(177566b DIV 2,15c);
      REPEAT UNTIL inp(177564b DIV 2)*{7}#{};
      out(177566b DIV 2,12c)
    END;
    INC(i); DEC(len)
  END;
  ei(m)
END write_str;

PROCEDURE print(f: ARRAY OF CHAR; SEQ a: WORD);
BEGIN
  IF on THEN fmt.format(0,write_str,f,a) END
END print;

VAR i: INTEGER;

BEGIN
  out(177564b DIV 2,000b);
  i:=10000;
  REPEAT DEC(i) UNTIL (i=0) OR (inp(177564b DIV 2)*{7}#{});
  on:=(i#0)
END deBug.
