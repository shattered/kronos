DEFINITION MODULE Plotter; (* Leg 17-Jun-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  def: defPlotter;

TYPE FONT;

    STATUS = RECORD (* 36 BYTES *)
                type  : INTEGER;
                pens  : INTEGER;
                pen   : INTEGER;
                pen_dw: INTEGER;
                ox,oy : INTEGER;
                x,y   : INTEGER;
                speeds: INTEGER;
                speed : INTEGER;  --  mm*1000 per second
                resols: INTEGER;
                resol : INTEGER;  --  mm*1,000,000
              END;

     STATE  = POINTER TO STATUS;

VAL done  : BOOLEAN;
    error : INTEGER;

    state : STATE;

    font  : FONT;     -- current font
    W,H   : INTEGER;  -- symbols weight & height
    Sx,Sy : INTEGER;  -- slope  vector
    Ix,Iy : INTEGER;  -- italic vector

------------------------------ PEN -----------------------------
                              -----

PROCEDURE up;                   (* pen up   *)

PROCEDURE dw;                   (* pen down *)

PROCEDURE set_pen(no: INTEGER);  (* change pen *)


-------------------------- COORDINATES -------------------------
                          -------------

PROCEDURE origin(x,y: INTEGER);

PROCEDURE pos(x,y: INTEGER);


-------------------------- PRIMITIVES --------------------------
                          ------------

PROCEDURE line (x0,y0,x1,y1: INTEGER);

PROCEDURE frame(x0,y0,x1,y1: INTEGER);

PROCEDURE circ(x,y,r: INTEGER);

PROCEDURE arc (x,y,xl,yl,xr,yr,r: INTEGER);

PROCEDURE arc3(x0,y0,x1,y1,x2,y2: INTEGER);

---------------------------- SYMBOLS ---------------------------
                            ---------

PROCEDURE cwrite    (x,y: INTEGER; ch: CHAR);

PROCEDURE cwrite_str(x,y: INTEGER; s: ARRAY OF CHAR);

PROCEDURE cprint    (x,y: INTEGER; format: ARRAY OF CHAR; SEQ x: sys.WORD);

PROCEDURE write     (ch: CHAR);

PROCEDURE write_str (s: ARRAY OF CHAR);

PROCEDURE print     (format: ARRAY OF CHAR; SEQ x: sys.WORD);


----------------------------- FONTS ----------------------------
                             -------

PROCEDURE load_font(VAR fnt: FONT; name: ARRAY OF CHAR);

PROCEDURE dispose_font(VAR fnt: FONT);

PROCEDURE set_size(w,h: INTEGER);

PROCEDURE set_slope(x,y: INTEGER);

PROCEDURE set_ital (x,y: INTEGER);

PROCEDURE set_font(fnt: FONT);

---------------------------- SPECIAL ---------------------------
                            ---------

PROCEDURE set_speed(n: INTEGER);

PROCEDURE set_resol(n: INTEGER);

PROCEDURE reset;

PROCEDURE attach(name: ARRAY OF CHAR);

END Plotter.

Note:
       Plotter type                     Device

            1                     plotter emulator for bitmap
            2                     FOK GIEM (DMPL protocol)
