DEFINITION MODULE defPrinter; (* Leg 18-Jan-90. (c) KRONOS *)

TYPE
  BARS  = ARRAY [0..2],[0..2] OF CHAR;
  WIDTH = POINTER TO ARRAY CHAR OF INTEGER;
  STATE = RECORD
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

CONST  (* M.S.B. of REQUEST.op when L.S.B. of it = CONTROL *)

  _info      = 10h;  -- (POINTER TO STATE)
  _reset     = 11h;
  _restore   = 12h;  -- (STATE)
  _something = 20h;  -- (on_off: INTEGER);
  _underline = 21h;  -- (on_off: INTEGER);
  _Wx2       = 22h;  -- (on_off: INTEGER);
  _Hx2       = 23h;  -- (on_off: INTEGER);
  _autowrap  = 24h;  -- (on_off: INTEGER);
  _raw       = 25h;  -- (on_off: INTEGER);

  _reverse   = 36h;  -- (on_off: INTEGER);
  _ground    = 37h;  -- (no    : INTEGER);
  _density   = 38h;  -- (no    : INTEGER);
  _font      = 39h;  -- (no    : INTEGER);

                      --- opreations ---

  _back      = 50h;  -- (dots: INTEGER)
  _fwd       = 51h;  -- (dots: INTEGER)
  _left      = 52h;  -- (dots: INTEGER)
  _right     = 53h;  -- (dots: INTEGER)
  _bflf      = 54h;  -- (count: INTEGER)
  _bhlf      = 55h;  -- (count: INTEGER)
  _fhlf      = 56h;  -- (count: INTEGER)
  _fflf      = 57h;  -- (count: INTEGER)
  _write_ln  = 58h;
  _eject     = 59h;
  _repeat    = 5Ah;  -- (ch: CHAR; count: INTEGER);
  _paint     = 5Bh;  -- (buf: ADDRESS; sz,w,h,dx,dy: INTEGER);
  _load_font = 5Ch;  -- (no,from,to: INTEGER; font: ADDRESS; sz: INTEGER)
  _set_attr  = 5Dh;
  _get_attr  = 5Eh;

END defPrinter.
