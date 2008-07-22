DEFINITION MODULE Windows; (* Sem 31-Dec-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, WORD;
IMPORT  sch: osKernel;

TYPE
  window=POINTER TO wnd_rec;
  job_proc=PROCEDURE (window,INTEGER,INTEGER,CHAR);
  wnd_rec=RECORD
    dsx    : INTEGER; -- window width in dots
    sy     : INTEGER; -- window high in dots
    sx     : INTEGER; -- window width in word;
    mem    : ADDRESS;
    patt   : BITSET;
    mode   : INTEGER;
    color  : INTEGER;
    x,y    : INTEGER;
    W,E,S,N: INTEGER;
    open   : BOOLEAN;
    fwd,bck: window;
    cx,cy  : INTEGER;
    cl     : ADDRESS;
    job    : job_proc;
    info   : ADDRESS;
   ini_proc: PROCEDURE(WORD);
    lock   : sch.mutex_rec;
  END;


VAR top : window;
    lsno: INTEGER;

PROCEDURE create(sx,sy: INTEGER): window;

PROCEDURE remove(w: window);

PROCEDURE resize(w: window; sx,sy: INTEGER);

PROCEDURE refresh(w: window);

PROCEDURE ref_box(w: window; y,dy: INTEGER);

PROCEDURE open(w: window);

PROCEDURE close(w: window);

PROCEDURE ontop(w: window);

PROCEDURE job;

PROCEDURE read_point(
          mode: INTEGER; w: window; VAR nx,ny: INTEGER; VAR ch: CHAR);

PROCEDURE del_crs;

PROCEDURE search(): window;

PROCEDURE lock_window(w: window);

PROCEDURE release_window(w: window);

END Windows.
