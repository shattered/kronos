DEFINITION MODULE bcPM; (* Leo 18-Jan-91. (c) KRONOS *)
                        (* brd 28-Jun-91. (c) KRONOS *)

IMPORT  SYSTEM;       (* It might be a Presentation Manager *)
IMPORT  pmWnd;
IMPORT  defFont;
IMPORT  BIO;

TYPE ROLLER;
     DIREX;
     TABLET;
     BAR;
     TEXT = DYNARR OF STRING;
     WINDOW= pmWnd.WINDOW;
      BLOCK= pmWnd.BLOCK;

VAL
    done: BOOLEAN;
   error: INTEGER;

   rnull: ROLLER;
   dnull: DIREX;
   tnull: TABLET;
   bnull: BAR;

   black: BITSET;  (* by default two last layers of display used *)
  shadow: BITSET;  (* {2,3} for 4 plane display, for example     *)
  normal: BITSET;
  bright: BITSET;

      ch: CHAR;    (* last key read from keyboard by PM *)
    time: INTEGER; (* timeout substract time of waiting *)
   mx,my: INTEGER; (* mouse coordinates driven by PM    *)
 timeout: INTEGER;

    font: defFont.FONT;
   sfont: defFont.FONT;  (* special signs font font^.H x font^.H *)

VAR ufont: defFont.FONT;  (* user defined font *)

CONST (* for sfont: (sfont generated automaticaly) *)
  empty=0c;  (* empty (black) char         *)
  utria=1c;  (* triangle directed to up    *)
  dtria=2c;  (* triangle directed to down  *)
  ltria=3c;  (* triangle directed to left  *)
  rtria=4c;  (* triangle directed to right *)

----------------------------------------------------------------

PROCEDURE block (w: WINDOW; b: BLOCK; fill,pressed: BOOLEAN);
PROCEDURE button(w: WINDOW; b: BLOCK; pressed: BOOLEAN);
PROCEDURE panel (W: WINDOW; b: BLOCK; VAR m: BLOCK; f,p: BOOLEAN);
PROCEDURE switch(W: WINDOW; b: BLOCK; p: BOOLEAN);


PROCEDURE inblock(x,y: INTEGER; w: WINDOW;  b: BLOCK): BOOLEAN;
PROCEDURE newstr(VAR s: STRING; fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

PROCEDURE blen(fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;
PROCEDURE slen(fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE setplanes(shadow,normal,bright: BITSET);
PROCEDURE setcolors;

PROCEDURE pushtimeout(milisec: INTEGER);
PROCEDURE poptimeout;

--------------------------- messages ---------------------------

PROCEDURE mwait(cpdkeys: BITSET; SEQ kbkeys: CHAR);
PROCEDURE wait (SEQ kbkeys: CHAR);

PROCEDURE message(   xc,yc: INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
PROCEDURE perror (er,xc,yc: INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);

--------------------------- dialogbox --------------------------

PROCEDURE diabox(xc,yc,w: INTEGER;
                 VAR str: ARRAY OF CHAR;
               promptfmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE confirm(xc,yc,w: INTEGER;
                  VAR   s: ARRAY OF CHAR;
                      fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

---------------------------- rollers ---------------------------

CONST
  xup = {0};    xlf = {2};
  xdw = {1};    xrg = {3};

PROCEDURE rnew    (VAR rol: ROLLER; x,y,w,h: INTEGER; exit: BITSET;
                    titfmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
PROCEDURE rdispose(VAR rol: ROLLER);
(* roller always maked with title *)

PROCEDURE rsettext(rol: ROLLER; text: TEXT; top,line: INTEGER);
(* if line<0 or top<0 (or both) automaticaly setted by internal AI *)

PROCEDURE rsetstr (rol: ROLLER;            alt: INTEGER;
                   fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

PROCEDURE rgettext(rol: ROLLER; VAR text: TEXT);

PROCEDURE ropen (rol: ROLLER);
PROCEDURE rclose(rol: ROLLER);

PROCEDURE rselect  (rol: ROLLER);
PROCEDURE rselected(rol: ROLLER): BOOLEAN;

PROCEDURE rchoose(rol: ROLLER; alt: INTEGER);

PROCEDURE ralt   (rol: ROLLER): INTEGER;

PROCEDURE rblocks(rol: ROLLER; VAR main,txt,tit,off,up,rl,dw: BLOCK);

PROCEDURE rwindow(rol: ROLLER; VAR wind: WINDOW);

----------------------------- direx ----------------------------

CONST (* items *)
  dirs     = {0};
  files    = {1};
  hidden   = {2};
  devices  = {3};
  all      = dirs + files + hidden + devices;
  standard = dirs + files;

PROCEDURE dnew    (VAR drx: DIREX;  x,y,w,h: INTEGER; exit: BITSET;
                    cdname: ARRAY OF CHAR;
                   pattern: ARRAY OF CHAR;
                     items: BITSET);
PROCEDURE ddispose(VAR drx: DIREX);

PROCEDURE dopen (drx: DIREX);
PROCEDURE dclose(drx: DIREX);

PROCEDURE dchoose(drx: DIREX; filename: ARRAY OF CHAR);
PROCEDURE dselect(drx: DIREX);

PROCEDURE dselected(drx: DIREX): BOOLEAN;

PROCEDURE dfilename(drx: DIREX; VAR fname: ARRAY OF CHAR);
PROCEDURE dfullname(drx: DIREX; VAR fname: ARRAY OF CHAR);
PROCEDURE dopenfile(drx: DIREX; VAR  f: BIO.FILE; biomode: ARRAY OF CHAR);
PROCEDURE dcd      (drx: DIREX; VAR cd: BIO.FILE);

PROCEDURE dblocks(drx: DIREX; VAR main,txt,tit,off,up,rl,dw: BLOCK);

PROCEDURE dwindow(d: DIREX; VAR wind: WINDOW);

---------------------------- Tablet ----------------------------

CONST
  xsel = {4};

PROCEDURE tnew    (VAR t: TABLET; col,lns,x,y,butw,buth: INTEGER; exit: BITSET;
                  titfmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

PROCEDURE tdispose(VAR t: TABLET);

PROCEDURE thotkey(t: TABLET; alt: INTEGER; hotkey: CHAR; capequ: BOOLEAN);

PROCEDURE tmove (t: TABLET; x,y: INTEGER);
PROCEDURE topen (t: TABLET);
PROCEDURE tclose(t: TABLET);

PROCEDURE tselect  (t: TABLET);
PROCEDURE tselected(t: TABLET): BOOLEAN;
PROCEDURE tbutton  (t: TABLET; x,y: INTEGER): INTEGER;

PROCEDURE tchoose(t: TABLET; alt: INTEGER);
PROCEDURE tunselect(t: TABLET);
PROCEDURE talt(t: TABLET): INTEGER;

PROCEDURE tdisable  (t: TABLET; alt: INTEGER);
PROCEDURE tundisable(t: TABLET; alt: INTEGER);

PROCEDURE tprint(t: TABLET; alt: INTEGER;
               fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD); (* print base font *)

PROCEDURE tsprint(t: TABLET; alt: INTEGER;
               fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD); (* print base font *)

PROCEDURE tontop    (t: TABLET);
PROCEDURE tonbottom (t: TABLET);

PROCEDURE twindow(t: TABLET; VAR w: WINDOW);
PROCEDURE tblocks(t: TABLET; but: INTEGER; VAR block: BLOCK);
PROCEDURE ttitle (t: TABLET; VAR title: BLOCK);

-------------------------- ACTION BAR --------------------------

PROCEDURE bnew(VAR b: BAR; balt: ARRAY OF STRING; x,y: INTEGER; exit: BITSET;
              titfmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);

PROCEDURE bdispose(VAR b: BAR);

PROCEDURE bhotkey(b: BAR; alt: INTEGER; hotkey: CHAR; capequ: BOOLEAN);

PROCEDURE bmove (b: BAR; x,y: INTEGER);
PROCEDURE bopen (b: BAR);
PROCEDURE bclose(b: BAR);

PROCEDURE bselect  (b: BAR);
PROCEDURE bselected(b: BAR): BOOLEAN;
PROCEDURE bbutton  (b: BAR; x,y: INTEGER): INTEGER;

PROCEDURE bchoose(b: BAR; alt: INTEGER);
PROCEDURE bunselect(b: BAR);
PROCEDURE balt(b: BAR): INTEGER;

PROCEDURE bdisable  (b: BAR; alt: INTEGER);
PROCEDURE bundisable(b: BAR; alt: INTEGER);

PROCEDURE  bontop   (b: BAR);
PROCEDURE  bonbottom(b: BAR);

PROCEDURE bwindow(b: BAR; VAR w: WINDOW);
PROCEDURE bblocks(b: BAR; but: INTEGER; VAR block: BLOCK);
PROCEDURE btitle (b: BAR; VAR title: BLOCK);

PROCEDURE bprint (b: BAR; alt: INTEGER;
                fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);  (* base font *)
PROCEDURE bsprint (b: BAR; alt: INTEGER;
                 fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD); (* user font *)

----------------------------- QUERY ----------------------------

PROCEDURE query(x,y: INTEGER; prompt,yes,no: ARRAY OF CHAR;
                   hoty,hotn: CHAR; cap,default: BOOLEAN): BOOLEAN;
           (* x,y -coord centr panel *)

-------------------------- DIALOG LINE -------------------------

PROCEDURE dia_lin(VAR str: ARRAY OF CHAR; prompt: ARRAY OF CHAR): BOOLEAN;
PROCEDURE dia_move(x,y: INTEGER);  (* default 0,0 *)

------------------------ mous & key read -----------------------

PROCEDURE x_read(VAR c: CHAR; VAR x,y: INTEGER;);
PROCEDURE clear_mou;

END bcPM.
