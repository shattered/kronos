DEFINITION MODULE Printer; (* Leg 10-Dec-89. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE BARS   = ARRAY [0..2] OF ARRAY [0..2] OF CHAR;
     WIDTH  = POINTER TO ARRAY CHAR OF INTEGER;
     STATUS =
          RECORD
            type     : INTEGER;
            fonts    : INTEGER;
            font     : INTEGER;
            hchar    : INTEGER;  (* ground box *)
            wchar    : WIDTH  ;  (* in  pixels *)
            something: INTEGER;
            underline: INTEGER;
            Wx2      : INTEGER;
            Hx2      : INTEGER;
            reverse  : INTEGER;
            raw      : INTEGER;
            awp      : INTEGER;
            grounds  : INTEGER;
            ground   : INTEGER;
            hbar     : CHAR   ;
            vbar     : CHAR   ;
            bars     : BARS   ;
            densities: INTEGER;  -- dot per inch
            density  : INTEGER;  -- number of density
          END;
     STATE = POINTER TO STATUS;

VAL    done: BOOLEAN;
      error: INTEGER;
      state: STATE;

--------------------------  STANDARD  --------------------------
                          ------------
PROCEDURE Write(ch: CHAR);

PROCEDURE WriteString(s: ARRAY OF CHAR);

PROCEDURE WriteLn;

--------------------------  EXTEND  ----------------------------
                          ----------

PROCEDURE write(buf: ARRAY OF CHAR; pos,len: INTEGER);

PROCEDURE repeat(ch: CHAR; no: INTEGER);

----------------  CHARACTER TYPEFACE CONTROL  ------------------
                ------------------------------

PROCEDURE set_font(no: INTEGER);

PROCEDURE set_something(on: INTEGER);

PROCEDURE set_reverse(on: INTEGER);

PROCEDURE set_underline(on: INTEGER);

PROCEDURE set_Wx2(on: INTEGER);

PROCEDURE set_Hx2(on: INTEGER);

PROCEDURE set_ground(c: INTEGER);


--------------------------- MOVEMENT ---------------------------
                           ----------
PROCEDURE fflf(n: INTEGER); (* Forward Full Line Feed *)
PROCEDURE fhlf(n: INTEGER); (* Forward Half Line Feed *)
PROCEDURE bflf(n: INTEGER); (* Back    Full Line Feed *)
PROCEDURE bhlf(n: INTEGER); (* Back    Half Line Feed *)

PROCEDURE fwd (pixels: INTEGER);
PROCEDURE back(pixels: INTEGER);

PROCEDURE right(pixels: INTEGER); (* move carrige left  *)
PROCEDURE left (pixels: INTEGER); (* move carrige right *)

PROCEDURE eject;
(* eject sheet or (for roll paper) form feed *)

--------------------------   GRAPHIC  --------------------------
                          ------------

PROCEDURE set_density(no: INTEGER);

PROCEDURE paint(map: ARRAY OF SYSTEM.WORD;
                w,h: INTEGER;
              dx,dy: INTEGER);
(* dx is horizontal margin, dy - vertical one.
   After successfull painting carrige will be lower
   by h+dy dots and at begining of line position *)


----------------------------- MISC -----------------------------
                             ------

PROCEDURE load_font(no: INTEGER; from,to: CHAR;
                    font: ARRAY OF SYSTEM.WORD);

PROCEDURE set_awp(on: INTEGER);

PROCEDURE set_raw(on: INTEGER);

PROCEDURE set_attr(no,val: INTEGER);

PROCEDURE get_attr(no: INTEGER): INTEGER;

PROCEDURE restore(status: STATUS);

PROCEDURE reset;

PROCEDURE nop;

PROCEDURE attach(name: ARRAY OF CHAR);

PROCEDURE ioctl(op: INTEGER; SEQ args: SYSTEM.WORD);

END Printer.

N O T E

     This  module  does  not  attach  driver  at  initialization.
Attaching  is  peformed  when  first  calling  of  driver occure.
Variable  -state- points to blank record before attaching. Hence,
if you want to use variable -state- before any calling to driver,
you have to call procedure -nop- to fulfil attaching.
