DEFINITION MODULE KTV480; (* Leo 12-Mar-90. (c) KRONOS *)

CONST
  ptrans = ARRAY OF INTEGER
           { 0Fh,07h,0Bh,03h,0Dh,05h,09h,01h,0Eh,06h,0Ah,02h,0Ch,04h,08h,00h };

TYPE PALETTE = ARRAY [0..15] OF INTEGER;

VAR awp: BOOLEAN; (* autowrap                     *)
   undl: BOOLEAN; (* underline                    *)
   cntl: BOOLEAN; (* FALSE => controls visible    *)
   lock: BOOLEAN; (* lock rolling *)
    shx: INTEGER;
    shy: INTEGER;
   scrl: INTEGER;
  palet: PALETTE;

VAL car: BOOLEAN;
  ln,cl: INTEGER;
  fg,bg: INTEGER;
   gmax: INTEGER; (* max fore/back ground         *)
   gmin: INTEGER; (* min fore/back ground (black) *)
  lines: INTEGER;
columns: INTEGER;

PROCEDURE RESET;

PROCEDURE toggle_carret;

PROCEDURE palette(p: PALETTE; from,len: INTEGER);

PROCEDURE set_lc(l,c: INTEGER);

PROCEDURE set_ground(f,b: INTEGER);

PROCEDURE write(str: ARRAY OF CHAR; pos,len: INTEGER);

PROCEDURE scrollup(no: INTEGER);
PROCEDURE scrolldw(no: INTEGER);

PROCEDURE eraln(dir: INTEGER);
PROCEDURE erach(no: INTEGER);
PROCEDURE erase(dir: INTEGER);

PROCEDURE delln(no: INTEGER);
PROCEDURE insln(no: INTEGER);

PROCEDURE delch(no: INTEGER);
PROCEDURE insch(no: INTEGER);

END KTV480.

