DEFINITION MODULE pedVG; (* Leo 06-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM Windows    IMPORT  window;

TYPE
  modes=(rep,or,xor,bic);
  inverse_modes=(off,on);

VAR rollX,rollY   : INTEGER; (* roll's operation influence  in pixels *)
    char_w, char_h: INTEGER;
    sign_w, sign_h: INTEGER;

PROCEDURE mode(w: window; m: modes);

PROCEDURE color(w: window; i: INTEGER);

PROCEDURE inverse(w: window; OnOff: inverse_modes);

PROCEDURE fill(w: window; i: INTEGER);

PROCEDURE clip(W,E,S,N: INTEGER; VAR x0,y0,x1,y1: INTEGER): BOOLEAN;

PROCEDURE vect(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE patt_vect(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE dot(w: window; x,y: INTEGER);

PROCEDURE circ(w: window; x,y,rx: INTEGER);

PROCEDURE arc(w: window; x,y,xa,ya,xb,yb,rx: INTEGER);

PROCEDURE frame(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE box(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE patt_box(w: window; x0,y0,x1,y1,patt: INTEGER);

PROCEDURE cursor(w: window; x,y,no: INTEGER);

PROCEDURE sign(w: window; x,y: INTEGER; sign: CHAR);

PROCEDURE greed(w: window; Scale,X,Y : INTEGER);

PROCEDURE rollN(w: window; n: INTEGER); (*  Rolls whole screen at  specified   *)
PROCEDURE rollE(w: window; n: INTEGER); (*  direction.                         *)
PROCEDURE rollS(w: window; n: INTEGER); (*    SN -- rollY*n pixels             *)
PROCEDURE rollW(w: window; n: INTEGER); (*    EW -- rollX*n pixels             *)


PROCEDURE prop_font(on: BOOLEAN);

PROCEDURE Write(w: window; x,y: INTEGER; c: CHAR);

PROCEDURE write_str(w: window; x,y: INTEGER; s: ARRAY OF CHAR);

PROCEDURE clip_write_str(w: window; W,E,S,N: INTEGER;
                         x,y: INTEGER; s: ARRAY OF CHAR);

PROCEDURE print(w: window;
                x,y   : INTEGER;
                format: ARRAY OF CHAR;
                SEQ args  : WORD);

PROCEDURE clip_print(w: window; W,E,S,N: INTEGER;
                     x,y   : INTEGER;
                     format: ARRAY OF CHAR;
                     SEQ args  : WORD);

END pedVG.
