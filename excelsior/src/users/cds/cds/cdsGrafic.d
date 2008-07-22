DEFINITION MODULE cdsGrafic; (* Sem 06-Jan-91. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM libWindows IMPORT  window;

TYPE
  modes=(rep,or,xor,bic);
  inverse_modes=(off,on);

CONST
  sg_resize =  0c;
  sg_move   =  4c;
  sg_close  = 10c;
  sg_up     = 14c;
  sg_down   = 20c;
  sg_right  = 24c;
  sg_left   = 30c;
  sg_yes    = 34c;
  sg_no     = 40c;
  sg_ok     = 44c;
  sg_cursor1= 50c;
  sg_cursor2= 54c;
  sg_in     = 60c;
  sg_out    = 64c;
  sg_empty  = 70c;
  sg_green  = 74c;
  sg_red    = 100c;

VAR
  char_w, char_h: INTEGER;
  sign_w, sign_h: INTEGER;

PROCEDURE mode      (w: window; m: modes);
PROCEDURE color     (w: window; c: INTEGER);
PROCEDURE inverse   (w: window; m: inverse_modes);
PROCEDURE prop_font (w: window; p: BOOLEAN);

PROCEDURE clip(W,E,S,N: INTEGER; VAR x0,y0,x1,y1: INTEGER): BOOLEAN;

PROCEDURE vect(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE dot(w: window; x,y: INTEGER);

PROCEDURE circ(w: window; x,y,rx: INTEGER);

PROCEDURE arc(w: window; x,y,xa,ya,xb,yb,rx: INTEGER);

PROCEDURE frame(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE box(w: window; x0,y0,x1,y1: INTEGER);

PROCEDURE patt_box(w: window; x0,y0,x1,y1,patt: INTEGER);

PROCEDURE sign(w: window; x,y: INTEGER; sign: CHAR);

PROCEDURE greed(w: window; scale,x,y : INTEGER);

PROCEDURE roll(w: window; dx,dy: INTEGER);

PROCEDURE write_char  (w: window; x,y: INTEGER; c: CHAR);
PROCEDURE write_string(w: window; x,y: INTEGER; s: ARRAY OF CHAR);
PROCEDURE write_pos   (w: window; x,y: INTEGER; s: ARRAY OF CHAR; p: INTEGER);
PROCEDURE print       (w: window; x,y: INTEGER; f: ARRAY OF CHAR; SEQ a: WORD);

PROCEDURE string_len(prp: BOOLEAN; s: ARRAY OF CHAR; p: INTEGER): INTEGER;

END cdsGrafic.
