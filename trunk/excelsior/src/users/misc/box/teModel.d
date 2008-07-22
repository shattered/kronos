DEFINITION MODULE teModel; (* Igo & Leo 09-Feb-91. (c) KRONOS *)

IMPORT  bmg: BMG;
IMPORT  wnd: pmWnd;

TYPE VIEW;

     BLOCK  = bmg.BLOCK ;
     BITMAP = bmg.BITMAP;
     WINDOW = wnd.WINDOW;

     VIEW   = POINTER TO view;  (* READ ONLY TYPE *)
     view   = RECORD
                x: INTEGER; (* wc of ldc of vsa *)
                y: INTEGER;
                s: INTEGER;
                w: WINDOW
              END;

     XCROSS = ARRAY [0..31] OF ARRAY [0..31] OF INTEGER;
     TLAYER = ARRAY [0..31] OF INTEGER;
     CLASS  = RECORD
                xcross: XCROSS;
                  tlay: TLAYER;
                 layno: INTEGER
              END;

     BCOLOR = ARRAY [0..31] OF BITSET;

CONST   internal = 1;
        external = 2;

           GREED = 1;
             LAY = ARRAY OF INTEGER { 0,1 };
           LAYSM = {0,1};
           COLOR = ARRAY OF BITSET  {{},{0},{1},{0..1}};
            LAYS = 2;

VAR
    shape: BCOLOR;

VAL  done: BOOLEAN;
    error: INTEGER;
    class: CLASS;

PROCEDURE setxcross(type0,type1,result: INTEGER);
(* set type of overlap between type0 & type 1 *)
PROCEDURE setlayno (lno: INTEGER);
(* set number of lays *)
PROCEDURE settlay  (lno,type: INTEGER);
(* set refrens between type & lay where type will be *)

PROCEDURE new     (VAR s: VIEW; d: VIEW);
PROCEDURE dispose (VAR v: VIEW);

PROCEDURE mequal  (v0,v1: VIEW): BOOLEAN;

PROCEDURE getframe(VAR b: BLOCK; v: VIEW);

PROCEDURE setwindow(v: VIEW; w: WINDOW);

PROCEDURE setview  (v: VIEW; x,y,scale: INTEGER);
PROCEDURE zoom     (v: VIEW; n: INTEGER);
PROCEDURE scroll   (v: VIEW; sx,sy: INTEGER);
PROCEDURE pan      (v: VIEW; b: BLOCK);

PROCEDURE refreshm  (v: VIEW);
PROCEDURE refreshv  (v: VIEW);
PROCEDURE refreshbox(v: VIEW; x0,y0,x1,y1: INTEGER);

PROCEDURE setname(v: VIEW;     n: ARRAY OF CHAR);
PROCEDURE getname(v: VIEW; VAR n: ARRAY OF CHAR);

PROCEDURE write(    v: VIEW; fn: ARRAY OF CHAR);
PROCEDURE read (VAR v: VIEW; fn: ARRAY OF CHAR);
PROCEDURE readx(v: VIEW);
PROCEDURE readt(v: VIEW);

PROCEDURE setframe (v: VIEW; b: BLOCK);

PROCEDURE cut    (VAR d: VIEW; s: VIEW; b: BLOCK);
PROCEDURE paste  (    d: VIEW; s: VIEW; b: BLOCK);

PROCEDURE include(v: VIEW; v: VIEW; x,y: INTEGER);
PROCEDURE delete (v: VIEW; x,y: INTEGER);
PROCEDURE setshow(v: VIEW; x,y: INTEGER;  mode: INTEGER);

PROCEDURE scalebox  (v: VIEW; VAR d: BLOCK;  s: BLOCK);
PROCEDURE insbox    (v: VIEW; type: INTEGER; b: BLOCK);

PROCEDURE scalepoint(v: VIEW; VAR X,Y: INTEGER; x,y: INTEGER);
PROCEDURE instxt    (v: VIEW; x,y,cx,cy: INTEGER; type: INTEGER; s: ARRAY OF CHAR);
PROCEDURE deltxt    (v: VIEW; x,y: INTEGER);

PROCEDURE insport(v: VIEW; x,y: INTEGER; s: ARRAY OF CHAR;  b: BLOCK);
PROCEDURE delport(v: VIEW; x,y: INTEGER);

END teModel.
