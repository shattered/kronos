DEFINITION MODULE pmWnd; (* Leo 09-Apr-91. (c) KRONOS *)

IMPORT  SYSTEM, defScreen, defFont, defBMG;

TYPE
  TOOL   = defScreen.TOOL;
  BLOCK  = defScreen.BLOCK;
  FONT   = defFont.FONT;

  WINDOW = POINTER TO WNDESC;

  PAINT  = PROCEDURE (WINDOW,INTEGER,INTEGER,INTEGER,INTEGER);
                           (* move *)
  RESIZE = PROCEDURE (WINDOW,BOOLEAN,INTEGER,INTEGER);

  WNDESC = RECORD (* READ ONLY! *)
             x,y    : INTEGER;  (* relative "desk"      *)
             w,h    : INTEGER;  (* pixels               *)
             sx,sy  : INTEGER;  (* relative "desktop"   *)
             mode   : BITSET;   (* method of drawing    *)
             fore   : BITSET;
             back   : BITSET;
             mask   : BITSET;
             image  : BOOLEAN;  (* has image ?          *)
             closed : BOOLEAN;  (* is  closed now?      *)
             visible: BOOLEAN;  (* is  visible now?     *)
             tool   : TOOL;     (* default tool         *)
             desk   : WINDOW;   (* own    desktop       *)
             top    : WINDOW;   (* upper  subwindow     *)
             bottom : WINDOW;   (* downer subwindow     *)

             resize : RESIZE;   (* pre resize action    *)
             corner : INTEGER;  (* resize will move it  *)
             minw   : INTEGER;  (* min sizes of window  *)
             minh   : INTEGER;
             maxw   : INTEGER;  (* max sizes of window  *)
             maxh   : INTEGER;
             refresh: PAINT;
           END;

CONST (* corners: *) ruc=0; rdc=1; ldc=2; luc=3; (* default "ruc" *)

VAL done: BOOLEAN;
   error: INTEGER;

    scrW: INTEGER;  (* phisical screen W    *)
    scrH: INTEGER;  (* phisical screen H    *)
    scrM: BITSET;   (* phisical screen Mask *)

 desktop: WINDOW;

       B: defBMG.BITMAP;

CONST (* window modes: *)
  scr    = {0};  (* drawing on screen in "fore" layers only *)
  img    = {1};  (* drawing in image  in "fore" layers only *)
  deep   = {2};  (* drawing in image  and on screen in all possible layers *)
  glass  = {3};  (* drawing over subwindows *)
  normal = scr+img; (* default window "mode" after create *)

  (* tool modes: *)

  xor    = defScreen.xor;
  or     = defScreen.or ;
  bic    = defScreen.bic;
  rep    = defScreen.rep;

PROCEDURE debug(on: BOOLEAN);

PROCEDURE create (VAR wnd: WINDOW; desk: WINDOW; x,y,w,h: INTEGER;
                fore,back: BITSET; refresh: PAINT);

PROCEDURE dispose(VAR wnd: WINDOW);

PROCEDURE image(wnd: WINDOW; x,y,w,h: INTEGER);
(* never be called. may by used only as "create" or "painter" parameter *)

PROCEDURE painter(wnd: WINDOW; refresh: PAINT);
(* set another refresh procedure for the window.
 * if "refresh" will stay to "image" then new image created
 *)

PROCEDURE open  (wnd: WINDOW);
PROCEDURE close (wnd: WINDOW);

PROCEDURE move  (wnd: WINDOW; x,y: INTEGER);

PROCEDURE resize(wnd: WINDOW; w,h: INTEGER);

PROCEDURE moveandresize(wnd: WINDOW; x,y,w,h: INTEGER);

PROCEDURE resizer(wnd: WINDOW; resize: RESIZE);
PROCEDURE minmax (wnd: WINDOW; minW,minH,maxW,maxH: INTEGER);
PROCEDURE corner (wnd: WINDOW; corner: INTEGER);
(* only "ruc", "rdc" are implemented yet *)

PROCEDURE upperthen(wnd: WINDOW; und: WINDOW): BOOLEAN;
(* TRUE iff window "wnd" upper then "und". (Note: upperthen(w,w)=FALSE!) *)

PROCEDURE ontop   (wnd: WINDOW);
PROCEDURE onbottom(wnd: WINDOW);

PROCEDURE putover (wnd: WINDOW; under: WINDOW);
PROCEDURE putunder(wnd: WINDOW; over : WINDOW);

PROCEDURE pass(wnd: WINDOW; desk: WINDOW);

PROCEDURE refreshbox(wnd: WINDOW; x,y,w,h: INTEGER);
PROCEDURE refresh   (wnd: WINDOW);
PROCEDURE refreshall(wnd: WINDOW);

PROCEDURE savebox(wnd: WINDOW; x,y,w,h: INTEGER);

PROCEDURE locate(x,y: INTEGER): WINDOW;
PROCEDURE up    (wnd: WINDOW ): WINDOW;
PROCEDURE dw    (wnd: WINDOW ): WINDOW;

PROCEDURE assign(wnd: WINDOW; name: ARRAY OF CHAR;     obj: SYSTEM.WORD);
PROCEDURE object(wnd: WINDOW; name: ARRAY OF CHAR; VAR obj: SYSTEM.WORD);
PROCEDURE delete(wnd: WINDOW; name: ARRAY OF CHAR);

PROCEDURE iterobjects(wnd: WINDOW);
PROCEDURE nextobject (wnd: WINDOW; VAR name: ARRAY OF CHAR;
                                   VAR obj : SYSTEM.WORD): BOOLEAN;

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

(*






PROCEDURE resize(wnd: WINDOW; w,h: INTEGER);
--------------------------------------------

      wnd^.resize(wnd,w,h)

   called (after possible image resize and) as resize notification
   before redraw.
   old coordinates and sizes are still in wnd^.(sx,sy,w,h).

      There are two kinds of action that may be done by wnd^.resize
   according to new window size

   For windows with IMAGE:
      moves and resize subwindows if need it.

      (image already resized when wnd^.resize called)

       set wnd^.mode(img) and redraw all extended rectangle of window
      (if exist). They will be refreshed later automaticly.

      set wnd^.mode(normal) and redraw all retained rectangles of window
      if them exist and it's needed. Mode normal need to refresh this
      rectangles because automaticly refresh of retained rectangle
      will NOT be executed.


   For windows WITHOUT image:
      moves and resize subwindows if need it.
      set wnd^.mode(normal) and redraw all retained rectangles of window
      if them exist and it's needed. Mode normal need to refresh this
      rectangles because automaticly refresh of retained rectangle
      will NOT be executed.

      be ready to redraw new parts of window when wnd^.refresh will
      be called.


     (0,0) coordinates still linked to left down corner of the window
           after resize.


     when corner =
       ruc: resize chage sizes moving right upper corner

       rdc: resize chage sizes moving right down  corner

            and so on



PROCEDURE moveandresize(wnd: WINDOW; x,y,w,h: INTEGER);
-------------------------------------------------------


      wnd^.resize(wnd,w,h)

   called (after possible image resize and) as resize notification
   before redraw.
   old coordinates and sizes are still in wnd^.(sx,sy,w,h).

   After moveandresize whole window will de redrown automaticly.

*)
