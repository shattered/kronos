DEFINITION MODULE fedImage; (* nick 14-Dec-90. (c) KRONOS *)

IMPORT  defScreen, defBMG;

VAL main: defBMG.BITMAP;
   scalx: INTEGER;
   scaly: INTEGER;
   char : defScreen.BLOCK;    -- full image area
   work : defScreen.BLOCK;    -- working area in full
   map  : defScreen.BLOCK;

VAR mode: BOOLEAN;

PROCEDURE set_scale(w,h,m,n: INTEGER);          (* set size  "char area" *)
PROCEDURE work_area(x0,y0,x1,y1: INTEGER);      (* set size  "work area" *)

PROCEDURE refresh;                              (* refresh char          *)

(* ниже координаты x и y даются относительно main *)
----------------------------------------------------

PROCEDURE dot(x,y: INTEGER);    (* invert    dot  *)

  (* нижеследующие операции производятся *)
  (*     внутри прямоугольника work      *)

PROCEDURE mirror;                    (* зеркальное отражение *)
PROCEDURE invers;

PROCEDURE insdeldot (x,y: INTEGER);  (* insert    dot  *)
PROCEDURE insdelline(x,y: INTEGER);  (* insert    line *)

PROCEDURE dupl(x,from,to: INTEGER);  (* duplicate line *)

PROCEDURE swap(x,from,to: INTEGER);  (* swap line *)

PROCEDURE fill(x,y: INTEGER);        (* filling poligon *)

--------------------------- GRAPHICS ---------------------------
                           ----------
PROCEDURE line(x0,y0,x1,y1: INTEGER);
PROCEDURE circ(x0,y0,r: INTEGER);
PROCEDURE arc (x0,y0,x1,y1,x2,y2: INTEGER);

END fedImage.
