DEFINITION MODULE VIDEO; (* Leo 06-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;

(*              G L O B A L   S T A T E   V A R I A B L E S     *)
(*                    shared  with  TV  driver                  *)

VAR Lines, Dots: INTEGER;
        Cursors: INTEGER; (* no of available cursors            *)
         Colors: INTEGER; (* no of available colors (0--black)  *)
          Color: INTEGER; (* current collor                     *)
           Mode: INTEGER; (* current mode (add,rep,com)         *)
  dotszX,dotszY: INTEGER; (* proportional sizes of a sigle dot     *)
charszX,charszY: INTEGER; (* sizes of a sigle char place in pixels *)
    rollX,rollY: INTEGER; (* roll's operation influence  in pixels *)

(* This GLOBAL state variables initialized in TV.INIT and       *)
(* management by TV module. Any assigment to them in other      *)
(* modules are ILLEGAL!                                         *)
(* Module VIDEO garantied to TV driver, that all calls of       *)
(* it procedures consist only on screen coordinates for drawing.*)
(* All clipping makes into VIDEO module.                        *)

(*              G L O B A L   S T A T E   V A R I A B L E S     *)
(*                    private for VIDEO                         *)

VAR clipW,clipE,clipN,clipS: INTEGER; (* clipping bounds *)

(*   All drawing enable omly in rectangle with
          clipW,clipE,clipN,clipS bounds:

                        N
                        |
                     W--|--E
                        |
                        S
*)

CONST com=0; -- complement (new xor old)
      rep=1; -- replace    (new independent of old)
      add=2; -- overlay    (new  or old)


PROCEDURE setclip(x0,y0,x1,y1: INTEGER);

PROCEDURE color(i: INTEGER);

PROCEDURE mode(m: INTEGER);

PROCEDURE patt(p: INTEGER);
-- 0 сплошная линия
-- 1 пунктир
-- 2 штрих пунктир
-- 3 отдельные точки

PROCEDURE fill(i: INTEGER);

PROCEDURE vect(x0,y0,x1,y1: INTEGER);

PROCEDURE dot(x,y: INTEGER);

PROCEDURE circ(x,y,rx: INTEGER);

PROCEDURE frame(x0,y0,x1,y1: INTEGER);

PROCEDURE rect (x0,y0,x1,y1: INTEGER);

PROCEDURE trapez(x0,x1,x2,x3,y0,y1: INTEGER);

PROCEDURE cursor(x,y,no: INTEGER);

PROCEDURE rollN(n: INTEGER); (*  Rolls whole screen at  specified   *)
PROCEDURE rollE(n: INTEGER); (*  direction.                         *)
PROCEDURE rollS(n: INTEGER); (*    SN -- rollY*n pixels             *)
PROCEDURE rollW(n: INTEGER); (*    EW -- rollX*n pixels             *)

PROCEDURE print(ln,col: INTEGER; VAL format: ARRAY OF CHAR; SEQ args: WORD);
(* first 4 lines on screen for text! (and only them)    *)
(* with numbers 0,1,2,3                                 *)

PROCEDURE start;

PROCEDURE finish;

END VIDEO.
