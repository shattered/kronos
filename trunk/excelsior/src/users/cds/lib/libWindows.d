DEFINITION MODULE libWindows; (* Sem 31-Dec-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, WORD;

CONST
  r_on    = 226c;  m_on    = 227c;  l_on    = 215c;
  r_off   = 236c;  m_off   = 237c;  l_off   = 225c;

  k_up    = 200c;  k_dw    = 201c;  k_right = 202c;  k_left  = 203c;
  k_pgup  = 204c;  k_pgdw  = 205c;  k_home  = 206c;  k_end   = 207c;
  k_del   = 210c;  k_ins   = 211c;  k_bcktab= 212c;  k_newln = 213c;
  k_alt   = 214c;  k_ctrl  = 215c;  k_shft  = 216c;  k_alkb  = 217c;

  k_f1    = 220c;  k_f2    = 221c;  k_f3    = 222c;  k_f4    = 223c;
  k_f5    = 224c;  k_f6    = 225c;  k_f7    = 226c;  k_f8    = 227c;
  k_f9    = 230c;  k_f10   = 231c;  k_f11   = 232c;  k_f12   = 233c;
  k_f13   = 234c;  k_f14   = 235c;  k_f15   = 236c;  k_center= 237c;

TYPE
  rp_mode = (rp_vect,rp_move,rp_size,rp_free,rp_flow);
  window=POINTER TO wnd_rec;
  job_proc=PROCEDURE (window,INTEGER,INTEGER,CHAR);
  wnd_rec=RECORD
    sx      : INTEGER; -- window width in dots
    sy      : INTEGER; -- window high in dots
    wpl     : INTEGER; -- window width in word;
    base    : ADDRESS;
    patt    : BITSET;
    mode    : INTEGER;
    color   : INTEGER;
    wpp     : INTEGER; -- words per plane
    x,y     : INTEGER;
    W,E,S,N : INTEGER;
    open    : BOOLEAN;
    mem     : ADDRESS;
    fwd,bck : window;
    job     : job_proc;
    info    : ADDRESS;
    ident   : INTEGER;
  END;

VAR
  top   : window;
  screen: window;
  lsno  : INTEGER;
  mouseX: INTEGER;
  mouseY: INTEGER;

PROCEDURE create(sx,sy: INTEGER): window;
PROCEDURE remove(VAR w: window);

PROCEDURE ref_box(w: window; y,dy: INTEGER);
PROCEDURE refresh(w: window);

PROCEDURE open (w: window);
PROCEDURE close(w: window);

PROCEDURE ontop   (w: window);
PROCEDURE onbottom(w: window);
PROCEDURE ontop?  (w: window): BOOLEAN;

PROCEDURE job;

PROCEDURE read_point(m: rp_mode; VAR x,y,sx,sy: INTEGER; VAR ch: CHAR);

PROCEDURE del_crs;

PROCEDURE get_pal(n: INTEGER; VAR r,g,b: INTEGER);
PROCEDURE put_pal(n: INTEGER; r,g,b: INTEGER);

-- keyboard and mouse interface:
PROCEDURE wait;
PROCEDURE first(VAR x,y: INTEGER; VAR c: CHAR);
PROCEDURE drop;

END libWindows.
