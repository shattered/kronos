DEFINITION MODULE videoGames; IMPORT SYSTEM; (* Leo 23-Aug-88. (c) KRONOS *)

(******************************************************************)
(*                                                                *)
(*         V i d e o   G a m e s    G r a p h i c                 *)
(*                                                                *)
(******************************************************************)


TYPE
  sprite =
    RECORD
      w,h : INTEGER;
      base: SYSTEM.ADDRESS;
      body: ARRAY [0..31] OF BITSET;
    END;

VAR  mode: INTEGER; CONST rep=0; or=1; xor=2; bic=3;

VAR   inv: INTEGER; CONST on=4; off=0;
VAL
   char_w: INTEGER;
   char_h: INTEGER;

PROCEDURE layer(n: INTEGER);

PROCEDURE erase(SEQ patterns: SYSTEM.WORD);

PROCEDURE dot  (x,y: INTEGER);
PROCEDURE vline(x,y,y1: INTEGER);
PROCEDURE hline(x,y,x1: INTEGER);

PROCEDURE line (x,y,x1,y1: INTEGER);
PROCEDURE rect (x,y,x1,y1: INTEGER);
PROCEDURE frame(x,y,x1,y1: INTEGER);
PROCEDURE circ (x,y,r: INTEGER);
PROCEDURE circf(x,y,r: INTEGER);

PROCEDURE write    (x,y: INTEGER;  ch: CHAR);
PROCEDURE write_str(x,y: INTEGER; str: ARRAY OF CHAR);
PROCEDURE print    (x,y: INTEGER; fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE bell;

PROCEDURE bit_move(mode: INTEGER;
                     to: SYSTEM.ADDRESS;   to_ofs: INTEGER;
                   from: SYSTEM.ADDRESS; from_ofs: INTEGER; nobits: INTEGER);

PROCEDURE bits(mode: INTEGER; x,y: INTEGER; adr: SYSTEM.ADDRESS; n: INTEGER);

PROCEDURE build_sprite(VAR sp: sprite; str: ARRAY OF CHAR);
(* string in format: "  ****  |"
                     "  ****  |"
                     "  ****  |"
*)

PROCEDURE show_sprite(x,y: INTEGER; VAR sp: sprite);

PROCEDURE  put_sprite(x,y: INTEGER; VAR sp: sprite); (* put but not show! *)

PROCEDURE refresh_around(x,y: INTEGER; VAR sp: sprite);

PROCEDURE refresh(x,y,w,h: INTEGER);

PROCEDURE bmd_adr(): SYSTEM.ADDRESS;

END videoGames.
