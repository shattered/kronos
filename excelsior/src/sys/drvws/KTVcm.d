DEFINITION MODULE KTVcm; (* Leo 12-Mar-90. (c) KRONOS *)


VAR awp: BOOLEAN; (* autowrap                     *)
   undl: BOOLEAN; (* underline                    *)
   cntl: BOOLEAN; (* FALSE => controls visible    *)

VAL car: BOOLEAN;
  ln,cl: INTEGER;
   gmax: INTEGER; (* max fore/back ground         *)
   gmin: INTEGER; (* min fore/back ground (black) *)
  lines: INTEGER;
columns: INTEGER;
reverse: BOOLEAN;

PROCEDURE RESET;

PROCEDURE toggle_carret;

PROCEDURE set_lc(l,c: INTEGER);

PROCEDURE set_color(c: INTEGER);

PROCEDURE set_reverse(on: INTEGER);

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

PROCEDURE final;

END KTVcm.

