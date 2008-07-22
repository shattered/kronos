DEFINITION MODULE defPlotter; (* Leg 17-Jun-90. (c) KRONOS *)

TYPE STATE = RECORD (* 36 BYTES *)
               type  : INTEGER;
               pens  : INTEGER;
               pen_dw: INTEGER;
               pen   : INTEGER;
               ox,oy : INTEGER;
               x,y   : INTEGER;
               speeds: INTEGER;
               speed : INTEGER;  --  mm*1000 per second
               resols: INTEGER;
               resol : INTEGER;  --  mm*1,000,000
             END;

CONST  (* M.S.B. of REQUEST.op when L.S.B. of it = CONTROL *)

              --- изменение состояния драйвера ---

  _info         = 00h;      -- (POINTER TO POINTER TO STATE)
  _reset        = 01h;

                        --- операции ---

  _up           = 10h;      --
  _down         = 11h;      --
  _set_pen      = 12h;      -- (no    : INTEGER);
  _set_pos      = 13h;      -- (line,col: INTEGER)
  _origin       = 14h;      -- (x,y: INTEGER);
  _circ         = 15h;      -- (x,y,r: INTEGER);
  _arc          = 16h;      -- (x,y,xl,yl,xr,yr,r: INTEGER);
  _set_speed    = 17h;      -- (n: INTEGER);
  _set_resol    = 18h;      -- (n: INTEGER);

END defPlotter.
