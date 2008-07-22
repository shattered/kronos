DEFINITION MODULE pmWnd; (* Leo 09-Apr-91. (c) KRONOS *)

IMPORT  SYSTEM, defScreen, defFont;

TYPE
  TOOL   = defScreen.TOOL;
  BLOCK  = defScreen.BLOCK;
  FONT   = defFont.FONT;
  WINDOW = POINTER TO WNDESC;
  PAINT  = PROCEDURE (WINDOW,INTEGER,INTEGER,INTEGER,INTEGER);
  WNDESC = RECORD
             x,y   : INTEGER;
             w,h   : INTEGER;
             mode  : BITSET;
             fore  : BITSET;
             back  : BITSET;
             mask  : BITSET;
             image : BOOLEAN;
             closed: BOOLEAN;
             board : PAINT;
             paint : PAINT;
             mgr   : SYSTEM.WORD;
             obj   : SYSTEM.WORD;
             desc  : SYSTEM.WORD;
             inner : TOOL;
             full  : TOOL;
           END;

VAL done: BOOLEAN;
   error: INTEGER;

    scrW: INTEGER;  (* phisical screen W    *)
    scrH: INTEGER;  (* phisical screen H    *)
    scrM: BITSET;   (* phisical screen Mask *)

     top: WINDOW;
  bottom: WINDOW;
 desktop: WINDOW;

    kind: INTEGER;   CONST
            bitmap = 0;
            pelmap = 1;
            gamma  = 2;
            ega    = 3;
            vga    = 4;

CONST (* window modes: *)
  scr    = {0};
  img    = {1};
  deep   = {2};
  normal = scr+img;

  (* tool modes: *)

  xor    = defScreen.xor;
  or     = defScreen.or ;
  bic    = defScreen.bic;
  rep    = defScreen.rep;

PROCEDURE new    (VAR wnd: WINDOW);
PROCEDURE dispose(VAR wnd: WINDOW);

PROCEDURE open  (wnd: WINDOW);
PROCEDURE close (wnd: WINDOW);

PROCEDURE move  (wnd: WINDOW; x,y : INTEGER);
PROCEDURE resize(wnd: WINDOW; w,h : INTEGER);
PROCEDURE mask  (wnd: WINDOW; fore,back: BITSET);

PROCEDURE inner (wnd: WINDOW; x,y,w,h: INTEGER);

PROCEDURE upperthen(wnd: WINDOW; und: WINDOW): BOOLEAN;
(* TRUE iff window "wnd" upper then "und". (Note: upperthen(w,w)=FALSE!) *)

PROCEDURE ontop   (wnd: WINDOW);
PROCEDURE onbottom(wnd: WINDOW);

PROCEDURE putover (wnd: WINDOW; under: WINDOW);
PROCEDURE putunder(wnd: WINDOW; over : WINDOW);

PROCEDURE refreshbox  (wnd: WINDOW; x,y,w,h: INTEGER);
PROCEDURE refreshboard(wnd: WINDOW; x,y,w,h: INTEGER);
PROCEDURE refresh     (wnd: WINDOW);
PROCEDURE refreshall;

PROCEDURE savebox(wnd: WINDOW; x,y,w,h: INTEGER);

PROCEDURE locate(x,y: INTEGER): WINDOW;
PROCEDURE up    (wnd: WINDOW ): WINDOW;
PROCEDURE dw    (wnd: WINDOW ): WINDOW;

PROCEDURE image  (wnd: WINDOW; on   : BOOLEAN);
PROCEDURE painter(wnd: WINDOW; paint: PAINT);
PROCEDURE boarder(wnd: WINDOW; board: PAINT);

PROCEDURE object (wnd: WINDOW; obj  : SYSTEM.WORD);
PROCEDURE manager(wnd: WINDOW; mgr  : SYSTEM.WORD);

----------------------------------------------------------------

PROCEDURE mode(wnd: WINDOW; mode: BITSET);

----------------------------------------------------------------

PROCEDURE erase(wnd: WINDOW);
PROCEDURE fill (wnd: WINDOW; tool: TOOL;  block: BLOCK;
                  w: INTEGER; SEQ p: SYSTEM.WORD);

PROCEDURE bblt(des: WINDOW; dtool: TOOL;  x,y: INTEGER;
               sou: WINDOW; stool: TOOL;  blk: BLOCK);

PROCEDURE pattern(wnd: WINDOW;  t: TOOL; block: BLOCK;
                  w,h: INTEGER; p: ARRAY OF SYSTEM.WORD);

PROCEDURE grid  (wnd: WINDOW;  t: TOOL; block: BLOCK; xstep,ystep: INTEGER);

PROCEDURE dot   (wnd: WINDOW; tool: TOOL; x, y       : INTEGER);
PROCEDURE line  (wnd: WINDOW; tool: TOOL; x0,y0,x1,y1: INTEGER);
PROCEDURE dline (wnd: WINDOW; tool: TOOL; x0,y0,x1,y1: INTEGER; VAR r: SYSTEM.WORD);
PROCEDURE hline (wnd: WINDOW; tool: TOOL; x0,y0,x1   : INTEGER);
PROCEDURE vline (wnd: WINDOW; tool: TOOL; x0,y0,y1   : INTEGER);
PROCEDURE rect  (wnd: WINDOW; tool: TOOL; x0,y0,x1,y1: INTEGER);
PROCEDURE frame (wnd: WINDOW; tool: TOOL; x0,y0,x1,y1: INTEGER);
PROCEDURE scroll(wnd: WINDOW; tool: TOOL; x,y        : INTEGER);

PROCEDURE trif  (wnd: WINDOW; tool: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);

PROCEDURE arc   (wnd: WINDOW; tool: TOOL; xc,yc,xa,ya,xb,yb,r: INTEGER);
PROCEDURE arc3  (wnd: WINDOW; tool: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);

PROCEDURE ring   (wnd: WINDOW; tool: TOOL; xc,yc,r0,r1: INTEGER);
PROCEDURE circle (wnd: WINDOW; tool: TOOL; x,y,r: INTEGER);
PROCEDURE circlef(wnd: WINDOW; tool: TOOL; x,y,r: INTEGER);

PROCEDURE print (wnd: WINDOW; tool: TOOL; x,y: INTEGER;
                 fnt: defFont.FONT;
                 fmt: ARRAY OF CHAR;     SEQ  arg: SYSTEM.WORD);

PROCEDURE xprint(wnd: WINDOW; tool: TOOL; x,y: INTEGER;
                 fnt: defFont.FONT;
                 fmt: ARRAY OF CHAR; SEQ  arg: SYSTEM.WORD): INTEGER;

PROCEDURE write (wnd: WINDOW; tool: TOOL; x,y: INTEGER;
                 fnt: defFont.FONT;
                 str: ARRAY OF CHAR;  pos,len: INTEGER);

PROCEDURE xwrite(wnd: WINDOW; tool: TOOL; x,y: INTEGER;
                 fnt: defFont.FONT;
                 str: ARRAY OF CHAR;  pos,len: INTEGER): INTEGER;

PROCEDURE writech(wnd: WINDOW; tool: TOOL; x,y: INTEGER;
                  fnt: defFont.FONT;        ch: CHAR);

PROCEDURE lenght(fnt: defFont.FONT;
                 fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE width (fnt: defFont.FONT;
                 fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE margin(fnt: defFont.FONT;
                 fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;

END pmWnd.
