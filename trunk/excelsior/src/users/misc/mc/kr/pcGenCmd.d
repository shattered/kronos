DEFINITION MODULE pcGenCmd; (* Sem 10-Nov-90. (c) KRONOS *)

IMPORT gen : krSym;

VAR
  code    : DYNARR OF CHAR;
  cnt     : INTEGER;
  stk     : INTEGER;
  stk_max : INTEGER;

PROCEDURE new_code(sz: INTEGER): INTEGER;

PROCEDURE b(n: INTEGER);
PROCEDURE c(n: INTEGER);

PROCEDURE li   (n: INTEGER);
PROCEDURE lsa  (n: INTEGER);
PROCEDURE sgw  (n: INTEGER);
PROCEDURE sgw1 (n: INTEGER);
PROCEDURE sgw2 (n: INTEGER);
PROCEDURE slw  (n: INTEGER);
PROCEDURE slw1 (n: INTEGER);
PROCEDURE slw2 (n: INTEGER);
PROCEDURE ssw  (n: INTEGER);
PROCEDURE ssw1 (n: INTEGER);
PROCEDURE ssw2 (n: INTEGER);
PROCEDURE lgw  (n: INTEGER);
PROCEDURE sew  (m,n: INTEGER);
PROCEDURE sew1 (m,n: INTEGER);
PROCEDURE sew2 (m,n: INTEGER);
PROCEDURE llw  (n: INTEGER);
PROCEDURE lsw  (n: INTEGER);
PROCEDURE lew  (m,n: INTEGER);
PROCEDURE lga  (n: INTEGER);
PROCEDURE lla  (n: INTEGER);
PROCEDURE lea  (m,n: INTEGER);
PROCEDURE gb   (n: INTEGER);
PROCEDURE enter(n: INTEGER);
PROCEDURE lsta (n: INTEGER);
PROCEDURE cmul (n: INTEGER);
PROCEDURE cdiv (n: INTEGER);
PROCEDURE load  (md: gen.access_mode; lvl,no,disp,sz: INTEGER);
PROCEDURE store1(md: gen.access_mode; lvl,no,disp,sz: INTEGER);
PROCEDURE store2(md: gen.access_mode; lvl,no,disp,sz: INTEGER);

END pcGenCmd.
