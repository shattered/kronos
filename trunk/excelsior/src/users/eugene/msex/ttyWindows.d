DEFINITION MODULE ttyWindows; (* Leo 04-Jun-90. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE
  WINDOW;

VAL
  maxW: INTEGER;
  maxH: INTEGER;
  hbar: CHAR;
  vbar: CHAR;
  bars: ARRAY [0..2] OF ARRAY [0..2] OF CHAR;

  full: WINDOW; (* full screen viewport, closed *)
  null: WINDOW;

  done: BOOLEAN;
 error: INTEGER;

VAR freeze: BOOLEAN; (* no refresh when TRUE *)

PROCEDURE new    (VAR wnd: WINDOW);
PROCEDURE dispose(VAR wnd: WINDOW);

PROCEDURE open (wnd: WINDOW);
PROCEDURE close(wnd: WINDOW);

PROCEDURE closed(wnd: WINDOW): BOOLEAN;
PROCEDURE under (over,under: WINDOW): BOOLEAN;

PROCEDURE move  (wnd: WINDOW; l,c: INTEGER);
PROCEDURE resize(wnd: WINDOW; w,h: INTEGER);

PROCEDURE ontop   (wnd: WINDOW);
PROCEDURE onbottom(wnd: WINDOW);

PROCEDURE top(wnd: WINDOW): BOOLEAN;

PROCEDURE locate(l,c: INTEGER; VAR wnd: WINDOW);
(* returns "null" when not found *)

PROCEDURE setpos(wnd: WINDOW; l,c: INTEGER);
PROCEDURE push(wnd: WINDOW);
PROCEDURE pop (wnd: WINDOW);

PROCEDURE write(wnd: WINDOW;     str: ARRAY OF CHAR; pos,len: INTEGER);
PROCEDURE read (wnd: WINDOW; VAR str: ARRAY OF CHAR; pos,len: INTEGER);

PROCEDURE print (wnd: WINDOW;    fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
PROCEDURE perror(wnd: WINDOW;  ecode: INTEGER;
                                 fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE writech(wnd: WINDOW; ch: CHAR);

PROCEDURE il(wnd: WINDOW; n: INTEGER);
PROCEDURE dl(wnd: WINDOW; n: INTEGER);
PROCEDURE dc(wnd: WINDOW; n: INTEGER);
PROCEDURE ic(wnd: WINDOW; n: INTEGER);

PROCEDURE era  (wnd: WINDOW; n: INTEGER);
PROCEDURE eraln(wnd: WINDOW; n: INTEGER);

PROCEDURE roll(wnd: WINDOW; n: INTEGER); (* n>0 up; n<0 down *)

CONST
  off =   0;
  on  =   1;

PROCEDURE cursor    (wnd: WINDOW; on: INTEGER);
PROCEDURE frame     (wnd: WINDOW; on: INTEGER);
PROCEDURE autowrap  (wnd: WINDOW; on: INTEGER);
PROCEDURE manual    (wnd: WINDOW; on: INTEGER);

PROCEDURE reverse   (wnd: WINDOW; on   : INTEGER);
PROCEDURE underline (wnd: WINDOW; on   : INTEGER);
PROCEDURE foreground(wnd: WINDOW; color: INTEGER);
PROCEDURE background(wnd: WINDOW; color: INTEGER);

PROCEDURE frameprint(wnd: WINDOW; l,c: INTEGER;
                fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
(* l=0 top line, l=h+2 bottom line *)

PROCEDURE refreshall;
PROCEDURE refresharea(l,c,w,h: INTEGER);
PROCEDURE refreshbox(wnd: WINDOW; l,c,w,h: INTEGER);
PROCEDURE refresh   (wnd: WINDOW);

TYPE
  INFO = POINTER TO VIEW;
  VIEW = RECORD
           l,c      : INTEGER;
           wl,wc    : INTEGER;
           w,h      : INTEGER;
           frame    : INTEGER;
           awp      : INTEGER;
           cursor   : INTEGER;
           manual   : INTEGER;
           back     : INTEGER;
           fore     : INTEGER;
           underline: INTEGER;
           reverse  : INTEGER;
         END;

PROCEDURE get(wnd: WINDOW; VAR info: INFO);

END ttyWindows.
