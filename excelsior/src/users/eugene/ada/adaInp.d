DEFINITION MODULE adaInp; (* 03-Apr-89. (c) KRONOS *)

CONST eof=3c;

VAR ch_pos: INTEGER;

PROCEDURE get_ch(): CHAR;

PROCEDURE set_pos(n: INTEGER);

PROCEDURE reset;

END adaInp.
