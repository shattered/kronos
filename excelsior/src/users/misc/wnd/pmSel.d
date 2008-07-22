DEFINITION MODULE pmSel; (* Nick & Leo 03-Aug-91. (c) KRONOS *)

IMPORT  SYSTEM, pmWnd;

TYPE
  WINDOW = pmWnd.WINDOW;

CONST (* kind *)
  _panel     =  0;
  _block     =  1;
  _item      =  2;
  _box       =  3;
  _icon      =  4;
  _radio     =  5;
  _button    =  6;
  _switch    =  7;
  _viewer    =  9;
  _slider    =  8;
  _scrollbar = 10;
  _roller    = 11;
  _menu      = 12;
  _diabox    = 13;
  _direx     = 14;
  _filer     = 15;
  _line      = 16;
  _text      = 17;

VAR null: WINDOW;
    done: BOOLEAN;
   error: INTEGER;
   msize: INTEGER;

PROCEDURE stdprint(s: WINDOW);

PROCEDURE create (VAR s: WINDOW; kind: INTEGER; x,y,w,h: INTEGER);
PROCEDURE dispose(VAR s: WINDOW);
(* dispose also excludes references to the window out of it father *)

PROCEDURE open (s: WINDOW);
PROCEDURE close(s: WINDOW);

PROCEDURE copy(into: WINDOW; name: ARRAY OF CHAR; sub: WINDOW);
(* include makes a copy from subselector *)
(* after include subsel may be disposed  *)

PROCEDURE kind(s: WINDOW): INTEGER;

PROCEDURE subsel(sel: WINDOW; name: ARRAY OF CHAR; VAR sub: WINDOW);

PROCEDURE read  (VAR s: WINDOW; filename,selname: ARRAY OF CHAR);
PROCEDURE save  (    s: WINDOW; filename,selname: ARRAY OF CHAR);
PROCEDURE delete(               filename,selname: ARRAY OF CHAR);

PROCEDURE title(s: WINDOW; fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE moveandresize(s: WINDOW; x,y,w,h: INTEGER);
(* resize only for scroll bars, scrollers and rollermans! *)

PROCEDURE print  (sel: WINDOW; fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
PROCEDURE printxy(sel: WINDOW; x,y: INTEGER;
                               fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

CONST (* state *)
  disabled  = {0}; (* for any selector      *)
  pressed   = {1}; (* button, radio, switch *)
  fix       = {2}; (* button, radio, switch *)
  dirtree   = {3}; (* directory tree search *)
  dirs      = {4};
  hidden    = {5};
  underhot  = {6};

PROCEDURE setstate(sel: WINDOW;     state: BITSET);
PROCEDURE getstate(sel: WINDOW; VAR state: BITSET);

PROCEDURE ratio(sel: WINDOW; areasize,objectsize: INTEGER);
(* for scrollbar: areasize   - size [pixel] of controlled area
                  objectsize - size of scrolled object
   for slider   : areasize ignored
*)

PROCEDURE scroll(sel: WINDOW;  VAR oldrdcXY,newrdcXY: INTEGER);
(* for scrollbar with defined ratio depened of object size *)
(* for slider old and new value *)

PROCEDURE viewer(s: WINDOW; x,y,w,h: INTEGER);
(* for diabox only defined sizes of dialog fielde *)

PROCEDURE suppress(sel: WINDOW; pos: INTEGER; off: BOOLEAN);
(* only for menu and rollers *)

PROCEDURE setpos(sel: WINDOW; objectpos: INTEGER);
(* set position of slider/scrollbar aproximatly to according object position *)
(* for diabox set text cursor to according position of the text *)
(* for menu/roller/direx set position of alternative *)

PROCEDURE getpos(sel: WINDOW; VAR objectpos: INTEGER);
(* -- see setpos -- *)

PROCEDURE select(s: WINDOW);

PROCEDURE selected(s: WINDOW; VAR sub: WINDOW): BOOLEAN;

PROCEDURE putstr(s: WINDOW;     str: ARRAY OF CHAR);
PROCEDURE getstr(s: WINDOW; VAR str: ARRAY OF CHAR);
(* for diabox only *)

END pmSel.
