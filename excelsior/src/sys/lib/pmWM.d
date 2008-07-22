DEFINITION MODULE pmWM; (* Leo 23-Apr-91. (c) KRONOS *)

IMPORT  SYSTEM, pmWnd, defFont;

TYPE WINDOW = pmWnd.WINDOW;

VAL done: BOOLEAN;
   error: INTEGER;

  ssfont: defFont.FONT;  (* special signs font         *)

  active: WINDOW;        (* last activated window      *)
 abutton: INTEGER;       (* last switched button no    *)

  closed: BOOLEAN;       (* active window m.b. closed  *)

   moved: BOOLEAN;       (* active window m.b. moved   *)
   moveX: INTEGER;       (* new coordinates for        *)
   moveY: INTEGER;       (*        active  window      *)

 resized: BOOLEAN;       (* active window m.b. resized *)
 resizeW: INTEGER;       (* new sizes for              *)
 resizeH: INTEGER;       (*        active  window      *)

VAR
   black: BITSET;
  shadow: BITSET;
  normal: BITSET;
  bright: BITSET;

CONST (* ssfont chars *)
  ssempty   = 00c;
  ssmove    = 01c;
  ssresize  = 02c;
  ssclose   = 03c;
  ssontop   = 04c;
  ssonbot   = 05c;
  sszoomin  = 06c;
  sszoomout = 07c;
  ssleft    = 10c;
  ssright   = 11c;
  ssup      = 12c;
  ssdw      = 13c;
  sspan     = 14c;
  sseye     = 15c;
  sszoom    = 16c;
  ssgrid    = 17c;
  sscascade = 20c;
  ssfire    = 21c;
  ssicon    = 22c;
  sstool    = 23c;

PROCEDURE new    (VAR w: WINDOW);
PROCEDURE dispose(VAR w: WINDOW);

PROCEDURE enable (w: WINDOW); (* enable/disable WM control for        *)
PROCEDURE disable(w: WINDOW); (* move, resize, close (default enable) *)

PROCEDURE monitor;

CONST (* corners *)
  luc = 0;  ruc = 2;
  ldc = 1;  rdc = 3;

PROCEDURE button (w: WINDOW; no,corner,x,y,w,h: INTEGER); (* inner sizes *)
PROCEDURE pressed(w: WINDOW; button: INTEGER): BOOLEAN;
PROCEDURE toggle (w: WINDOW; button: INTEGER; pressed: BOOLEAN);

PROCEDURE buttoncolors(w: WINDOW; no: INTEGER; fore,pressed,back: BITSET);

PROCEDURE print  (w: WINDOW; button: INTEGER; font: defFont.FONT;
                             format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

END pmWM.
