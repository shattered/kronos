DEFINITION MODULE exScreen; (* Leo 05-Jun-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;

CONST maxcol   = 254;

(* Because some VT-52 like terminals not stopped at  *)
(* last column of screen and possible execute rollup *)
(* at last column of last line this module for such  *)
(* terminals must be used with width of screen       *)
(* decrized by 1.                                    *)

(* This module try use Hardware futhers IL,DL,IC,DC  *)
(* for current terminal and if them don't work emu-  *)
(* late them. So if you specified frame less than    *)
(* the realy screen you can't wait the correct in    *)
(* window operating, because IL, DL, IC, DC, RollUp  *)
(* & RollDw are full screen actions!                 *)


PROCEDURE frame(nolines,nocol: INTEGER);
(* Note -nolines- & -nocol- says the realy side of
   VT-52 window.
   Last line used for information.
   Edit window is nocol-1, nolines bounds
   Default 24x80
   Maximum frame 64x255
*)

VAL nolines, nocolumns: INTEGER;

VAR stop: PROCEDURE (): BOOLEAN;

(* When refresh, leftbound, write, il, dl, dup       *)
(* executing and something pressed on the keyboard   *)
(* -stop- calls to determine to stop screen output.  *)
(* All this proc firstly changes the surroundings    *)
(* of edited line and after that start refresh the   *)
(* screen. After thise procedures execution          *)
(* termination (normal or by -stop-) -stop-          *)
(* setted to dummy action:                           *)
(*                                                   *)
(*        PROCEDURE NotStop;                         *)
(*        BEGIN RETURN FALSE END NotStop;            *)
(*                                                   *)
(*        stop:=NotStop;                             *)
(* stop also setted to dummy action after call       *)
(* PleaseRefresh. PleaseRefresh returms TRUE if      *)
(* refreshing of screen stopped by stop and not      *)
(* finished.                                         *)

PROCEDURE stringsize(s: ARRAY OF CHAR): INTEGER;
(* calculate string size in bytes without tailed spaces *)

(***************************    O P T I O N S   *****************************)

PROCEDURE Bell;

PROCEDURE bell(on_off: BOOLEAN);        PROCEDURE bell?(): BOOLEAN;
(*   Bell at line overflow   *)         (* Defualt: TRUE  *)

PROCEDURE  ins(on_off: BOOLEAN);        PROCEDURE  ins?(): BOOLEAN;
(*   Insert/Replace mode     *)         (* Defualt: FALSE *)

PROCEDURE infomode(on_off: BOOLEAN);    PROCEDURE infomode?(): BOOLEAN;
(* on/off info showing *)               (* Default: FALSE *)

(*********************** W O R K   W I T H   L I N E S **********************)

PROCEDURE smode(l: INTEGER; s: BITSET);
(* not currently used *)
PROCEDURE gmode(l: INTEGER): BITSET;
(* not currently used *)
PROCEDURE sattr(l: INTEGER; val: INTEGER);
(* not currently used *)
PROCEDURE gattr(l: INTEGER): INTEGER;
(* not currently used *)

       (* screen  line information *)
PROCEDURE getline(ln: INTEGER; VAR s: ARRAY OF CHAR);
PROCEDURE adrline(ln: INTEGER): ADDRESS;
PROCEDURE linesize(ln: INTEGER): INTEGER;


                     (* current line information *)
PROCEDURE get(VAR s: ARRAY OF CHAR);    PROCEDURE size(): INTEGER;
PROCEDURE adr(): ADDRESS;

PROCEDURE setline(no: INTEGER; s: ARRAY OF CHAR; szB: INTEGER);
(* set line -no- to -s- but not refreshed it on the screen!!!    *)
(* USE CAREFULLY! this proc needed to fill screen or it part     *)
(* for later refreshing.                                         *)

PROCEDURE w(ch: CHAR);
(* write signle char *)

PROCEDURE ws(s: ARRAY OF CHAR; szB: INTEGER);
(* write whole line *)

(*  Hady. 03-Oct-90.

PROCEDURE reverse(on: INTEGER): INTEGER;
(* see Terminal *)
PROCEDURE cursor(on: INTEGER): INTEGER;
(* see Terminal *)
*)

PROCEDURE mark(line0,col0,line1,col1: INTEGER; on: INTEGER);
(* marked & unmarked rectangle & two lines:
              col0      col1
     line0: ---------------------
               |         |
               |         |
               |         |
               |         |
     line1: ---------------------
*)

(********************* CURSOR   P O S I T I O N I N G ***********************)

PROCEDURE pos?(VAR ln,col: INTEGER);

VAL posc?: INTEGER;             infoline_pos?: INTEGER;
    posl?: INTEGER;



PROCEDURE pos(ln,col: INTEGER);
PROCEDURE posc(col: INTEGER);
PROCEDURE posl( ln: INTEGER);

                     (* 32 pos stack *)
PROCEDURE push;                         PROCEDURE pop;
(* push current position into stack *)  (* execute pos to last pushed *)
PROCEDURE copt;                         PROCEDURE drop;
(* copy top of position stack *)        (* drop the top of position stack *)

(***************** L E F T   B O U N D   O F   W I N D O W ******************)

PROCEDURE leftbound(col: INTEGER);
PROCEDURE leftbound?(): INTEGER;

(**************** L I N E   &   S C R E E N   R E F R E S H *****************)

PROCEDURE refresh;

PROCEDURE refreshline;

PROCEDURE PleaseRefresh(): BOOLEAN;
(* Please refresh screen after editting of current line *)

PROCEDURE clearscr;                     PROCEDURE clearln;
                                        (* clear line tail *)

(**************** O N   S C R E E N   O P E R A T I O N S *******************)

(* Note!
   Procedures dl, il, swapup, swapdw, dup  not work at last line of
   the frame.
   It's recomended to rollup the screen when current line
   about 2 lines upper than frame bottom
*)

PROCEDURE ic(char: CHAR);               PROCEDURE dc;

PROCEDURE dl(s: ARRAY OF CHAR; szB: INTEGER);
PROCEDURE il;
(* s -- new last line of screen *)
(*       il   &   dl   cleans the info line and not refresh it !!!!      *)

PROCEDURE undel(VAR up,dw: ARRAY OF CHAR);
(* returns up & down lines that modified at last -dl-
   in state previos -dl-
*)

PROCEDURE delword0;             PROCEDURE delword1;
PROCEDURE ruboutword0;          PROCEDURE ruboutword1;
PROCEDURE skipwordleft0;        PROCEDURE skipwordleft1;
PROCEDURE skipwordright0;       PROCEDURE skipwordright1;

PROCEDURE swapup;                       PROCEDURE swapdw;

PROCEDURE rollup(s: ARRAY OF CHAR; szB: INTEGER);
(* s -- new last line of screen *)
PROCEDURE rolldw(s: ARRAY OF CHAR; szB: INTEGER);
(* s -- new first line of screen *)
(*     rollup & rolldw cleans the info line and not refresh it !!!!      *)

PROCEDURE pullleft;                     PROCEDURE pullright;

PROCEDURE dup;

PROCEDURE dupover;

PROCEDURE rubout;

(******************* I N F O   L I N E   S U P P O R T **********************)


VAL timepos: INTEGER;
      fnpos: INTEGER;

PROCEDURE setinfostr(col: INTEGER; s: ARRAY OF CHAR);

PROCEDURE set_on_off(col: INTEGER; on_off: BOOLEAN);

PROCEDURE fixpos(fileline: INTEGER);

PROCEDURE showinfo;

PROCEDURE pushandclearinfo;
(* cursor stays at the beginning of info line *)

PROCEDURE pushandposinfo;
(* cursor stays at the beginning of info line *)

END exScreen.
