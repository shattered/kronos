MODULE her; (* 27-Dec-89. (c) KRONOS *)

IMPORT mcd : mCodeMnem;
FROM Terminal   IMPORT  print, Read;
FROM SYSTEM     IMPORT  WORD, ADR;
FROM Image      IMPORT  image0;

CONST
  rg_cmd  = 164002b DIV 2;
  rg_dwr  = 164004b DIV 2;
  rg_stat = 164006b DIV 2;
  rg_adr1 = 164010b DIV 2;
  rg_drd  = 164012b DIV 2;
  rg_adr2 = 164014b DIV 2;

TYPE
  word=ARRAY [0..36] OF CHAR;

PROCEDURE inp(n: INTEGER): BITSET; CODE mcd.inp END inp;
PROCEDURE out(n: INTEGER; v: WORD); CODE mcd.out END out;

PROCEDURE cmd_bit(VAR s: ARRAY OF CHAR; i: INTEGER);
BEGIN
  IF i<192 THEN
    image0(s,'dat%$3d',(31-i DIV 6)*6 + i MOD 6 +1);
  ELSE
    image0(s,'cmd???');
  END;
END cmd_bit;

PROCEDURE wr_word(adr: INTEGER; VAL w: word);
  VAR i: INTEGER;
BEGIN
  out(rg_cmd,10h);
  out(rg_adr2,adr);
  out(rg_cmd,18h);
  FOR i:=0 TO 35 DO out(rg_dwr,w[i]) END;
  out(rg_dwr,ORD(w[36]));
--  out(rg_dwr,ORD(w[36])+300b);
END wr_word;

PROCEDURE rd_word(adr: INTEGER; VAR w: word);
  VAR i: INTEGER;
BEGIN
  out(rg_cmd,0Dh);
  out(rg_adr2,adr);
  out(rg_cmd,19h);
  FOR i:=0 TO 36 DO w[i]:=CHAR(inp(rg_drd)) END;
END rd_word;

PROCEDURE zero_word(VAR w: word);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(w) DO w[i]:=0c END;
  w[33]:=077c;
END zero_word;

PROCEDURE incl(VAR w: word; n: INTEGER);
BEGIN
  IF (n MOD 6) IN BITSET(w[n DIV 6]) THEN RETURN END;
  w[n DIV 6]:=CHAR(BITSET(w[n DIV 6])+{n MOD 6});
  w[33]:=CHAR(BITSET(w[33])/{n MOD 6});
END incl;

PROCEDURE il(VAR w: word; x,y: INTEGER);
BEGIN
  w[31-x]:=CHAR(BITSET(w[31-x])+{5-y});
END il;

PROCEDURE encl(VAR w: word; n: INTEGER);
BEGIN
  IF NOT ((n MOD 6) IN BITSET(w[n DIV 6])) THEN RETURN END;
  w[n DIV 6]:=CHAR(BITSET(w[n DIV 6])-{n MOD 6});
  w[33]:=CHAR(BITSET(w[33])/{n MOD 6});
END encl;

PROCEDURE in(VAR w: word; n: INTEGER): BOOLEAN;
BEGIN
  RETURN (n MOD 6) IN BITSET(w[n DIV 6]);
END in;

PROCEDURE show_word(w: word);
  VAR i: INTEGER;
BEGIN
(*  print('[');
  FOR i:=0 TO 6*37-1 DO
    IF in(w,i) THEN print(' %d',i) END;
  END;
  print(']');
*)
  FOR i:=0 TO 69 DO
    IF in(w,i) THEN print('*') ELSE print('.') END;
  END;
END show_word;

PROCEDURE power_on;
BEGIN
  out(rg_cmd,4h);
END power_on;

PROCEDURE power_off;
BEGIN
  out(rg_cmd,7h);
END power_off;

VAR
  w: word;
  i: INTEGER;
  j: INTEGER;
  str: ARRAY [0..15] OF CHAR;
  ps : INTEGER;
  ch : CHAR;

BEGIN
  ps:=0;
  out(rg_cmd,0Eh); -- reset
  power_on;
  FOR i:=0 TO HIGH(w) DO w[i]:=0c END;
  il(w,0,0);
  il(w,1,1);
  il(w,2,2);
  il(w,3,3);
  il(w,4,4);
  il(w,5,5);
  il(w,5,0);
  il(w,4,1);
  il(w,3,2);
  il(w,2,3);
  il(w,1,4);
  il(w,0,5);

  il(w,7,0);
  il(w,8,1);
  il(w,9,2);
  il(w,10,3);
  il(w,11,4);
  il(w,12,5);

  il(w,9,3);
  il(w,8,4);
  il(w,7,5);

  il(w,14,5);
  il(w,14,4);
  il(w,14,3);
  il(w,14,2);
  il(w,14,1);
  il(w,14,0);
  il(w,15,1);
  il(w,16,2);
  il(w,17,3);
  il(w,18,4);
  il(w,19,5);
  il(w,19,4);
  il(w,19,3);
  il(w,19,2);
  il(w,19,1);
  il(w,19,0);
  il(w,16,5);
  il(w,17,5);

  LOOP
    wr_word(0,w);
    ch:=w[31];
    FOR i:=31 TO 1 BY -1 DO w[i]:=w[i-1] END;
    w[0]:=ch;
    FOR i:=0 TO 9999 DO END;
  END;
END her.
