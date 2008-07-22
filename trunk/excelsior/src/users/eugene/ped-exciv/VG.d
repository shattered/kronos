DEFINITION MODULE VG; (* Ilx 11-Jun-89. (c) KRONOS *)

IMPORT SYSTEM, VF;

TYPE BMD =
  RECORD
    w,h : INTEGER;
    wpl : INTEGER;
    base: SYSTEM.ADDRESS;
    patt: BITSET;
    mode: INTEGER;
  END;

CONST rep=0; or=1; xor=2; bic=3;
CONST on=4; off=0;
CONST layers=4;
CONST dots=480; lines=360;

VAL layer: ARRAY [0..layers-1] OF BMD;

VAL
   char_w : INTEGER;
   char_h : INTEGER;

PROCEDURE mode(VAR bmd: BMD; m: INTEGER);

PROCEDURE build_bmd(VAR bmd: BMD; w,h: INTEGER; adr: SYSTEM.ADDRESS);

PROCEDURE bmd_size(w,h: INTEGER): INTEGER; (* no of words needed for WxH *)

PROCEDURE erase(VAR bmd: BMD; SEQ patterns: SYSTEM.WORD);

PROCEDURE dot  (VAR bmd: BMD; x,y: INTEGER);
PROCEDURE vline(VAR bmd: BMD; x,y,y1: INTEGER);
PROCEDURE hline(VAR bmd: BMD; x,y,x1: INTEGER);
PROCEDURE line (VAR bmd: BMD; x,y,x1,y1: INTEGER);
PROCEDURE patt_line (VAR bmd: BMD; x,y,x1,y1: INTEGER);
PROCEDURE rect (VAR bmd: BMD; x,y,x1,y1: INTEGER);
PROCEDURE frame(VAR bmd: BMD; x,y,x1,y1: INTEGER);
PROCEDURE circ (VAR bmd: BMD; x,y,r: INTEGER);
PROCEDURE circf(VAR bmd: BMD; x,y,r: INTEGER);
PROCEDURE arc  (VAR bmd: BMD; X0,Y0,xa,ya,xb,yb,r: INTEGER);
PROCEDURE trif (VAR bmd: BMD; x0,y0,x1,y1,x2,y2: INTEGER);

PROCEDURE inverse(VAR bmd: BMD; OnOff: INTEGER);
PROCEDURE font     (f: VF.Font);
PROCEDURE write    (VAR bmd: BMD; x,y: INTEGER;  ch: CHAR);
PROCEDURE write_str(VAR bmd: BMD; x,y: INTEGER; str: ARRAY OF CHAR);
PROCEDURE print    (VAR bmd: BMD; x,y: INTEGER;
                    fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE clip_write_str(VAR bmd: BMD; W,E,S,N: INTEGER;
                         x,y: INTEGER; s: ARRAY OF CHAR);
PROCEDURE clip_print(VAR bmd: BMD; W,E,S,N: INTEGER;
                     x,y   : INTEGER;
                     format: ARRAY OF CHAR;
                     SEQ args  : SYSTEM.WORD);

PROCEDURE bit_move(mode: INTEGER;
                     to: SYSTEM.ADDRESS;   to_ofs: INTEGER;
                   from: SYSTEM.ADDRESS; from_ofs: INTEGER; nobits: INTEGER);

PROCEDURE paint(wait: BOOLEAN);
PROCEDURE color(no,R,G,B: INTEGER);
PROCEDURE init_palette;
PROCEDURE screen(on: BOOLEAN);

END VG.
