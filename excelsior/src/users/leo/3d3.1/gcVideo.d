DEFINITION MODULE gcVideo; (* Leo 23-Mar-90. (c) KRONOS *)

VAL
  C: INTEGER;
  W: INTEGER;
  H: INTEGER;

PROCEDURE picture(on: BOOLEAN);

PROCEDURE erase;

PROCEDURE color(c: INTEGER);
(* MOD 16 on IGD480 *)

PROCEDURE line(x,y,x1,y1: INTEGER);

PROCEDURE erase_line(y: INTEGER; mask: BITSET);
(* mask ignored at IGD480 *)

PROCEDURE plane(no: INTEGER);
(* 0|1 for gamma only *)

PROCEDURE flush;

END gcVideo.
