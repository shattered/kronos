DEFINITION MODULE lowLevel; (* Leo 29-Nov-89. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, WORD;

VAL
  cpu_type : INTEGER; -- 8 - 86, 1 - 186, 2 - 286
  cpu_mode : INTEGER; -- 0 - real mode, 1 - protected mode

PROCEDURE move(des,sou: ADDRESS; size: INTEGER);

PROCEDURE cmove(des: ADDRESS; des_ofs: INTEGER;
                sou: ADDRESS; sou_ofs: INTEGER; bytes: INTEGER);

PROCEDURE _zero(adr: ADDRESS; size: INTEGER);
PROCEDURE _fill(adr: ADDRESS; size: INTEGER; val: WORD);

PROCEDURE zero(VAR area: ARRAY OF WORD);
PROCEDURE fill(VAR area: ARRAY OF WORD; val: WORD);

PROCEDURE bblt_rep(t: ADDRESS; td: INTEGER; f: ADDRESS; td,sz: INTEGER);
PROCEDURE bblt_or (t: ADDRESS; td: INTEGER; f: ADDRESS; td,sz: INTEGER);
PROCEDURE bblt_xor(t: ADDRESS; td: INTEGER; f: ADDRESS; td,sz: INTEGER);
PROCEDURE bblt_bic(t: ADDRESS; td: INTEGER; f: ADDRESS; td,sz: INTEGER);

PROCEDURE QUIT;

END lowLevel.
