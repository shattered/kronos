DEFINITION MODULE tal; (* 26-Oct-89. (c) KRONOS *)

TYPE
  tile=POINTER TO tile_rec;
  tile_rec=RECORD
    up: tile;
    dw: tile;
    lf: tile;
    rt: tile;
    x : INTEGER;
    y : INTEGER;
    an: INTEGER;
    dt: INTEGER;
  END;
  iter_proc=PROCEDURE (tile);

VAR
  wsp: tile;

PROCEDURE app(x,y,sx,sy,dt: INTEGER);

PROCEDURE box(x,y,sx,sy: INTEGER; p: iter_proc);

PROCEDURE nab(t: tile; p: iter_proc);

PROCEDURE show(t: tile);

END tal.
