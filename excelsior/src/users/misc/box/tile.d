DEFINITION MODULE tile; (* 23-Jan-91 *)

FROM SYSTEM     IMPORT  WORD;

CONST wsize = 256*256;
    maxtype = MAX(INTEGER);
        spc =  0;
        bnd = -1;

TYPE TILE     = POINTER TO tile;
     tile     = RECORD
                  up,rg,lf,dw: TILE;
                  x,y,type   : INTEGER;
                END;

     LAY      = POINTER TO lay;
     lay      = RECORD
                  head: TILE
                END;

     iterator = PROCEDURE (TILE): BOOLEAN;
     tl_type  = PROCEDURE (INTEGER,INTEGER): INTEGER;

      ITERCTX = RECORD
                  x ,y : INTEGER;
                  x1,y1: INTEGER;
                     pt: TILE
                END;

VAL  done: BOOLEAN;
    error: INTEGER;

PROCEDURE new     (VAR l: LAY);
PROCEDURE dispose (VAR l: LAY);
PROCEDURE where   (l: LAY; x,y: INTEGER): TILE;
PROCEDURE iterate (l: LAY; x,y,sx,sy: INTEGER; i: iterator );
PROCEDURE insert  (l: LAY; t: WORD; x,y,sx,sy: INTEGER; f: tl_type);
PROCEDURE start   (VAR c: ITERCTX; l: LAY; x,y,sx,sy: INTEGER);
PROCEDURE next    (VAR c: ITERCTX; VAR t: TILE): BOOLEAN;
PROCEDURE free    (l: LAY; x,y,sx,sy: INTEGER): BOOLEAN;

END tile.
