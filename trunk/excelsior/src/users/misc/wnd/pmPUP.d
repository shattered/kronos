DEFINITION MODULE pmPUP; (* Leo 18-Jan-91. (c) KRONOS *)

IMPORT  SYSTEM;       (* It might be a Presentation Manager *)
IMPORT  defScreen;
IMPORT  defFont;
IMPORT  BIO;

TYPE POPUP;
     MENU;
     DEBUG;
     ROLLER;
     DIREX;
     TEXT = DYNARR OF STRING;

VAL
    done: BOOLEAN;
   error: INTEGER;

   pnull: POPUP;
   mnull: MENU;
   rnull: ROLLER;
   dnull: DIREX;
   gnull: DEBUG;

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

CONST (* for sfont: (sfont generated automaticaly) *)
  empty=0c;  (* empty (black) char         *)
  utria=1c;  (* triangle directed to up    *)
  dtria=2c;  (* triangle directed to down  *)
  ltria=3c;  (* triangle directed to left  *)
  rtria=4c;  (* triangle directed to right *)

PROCEDURE setplanes(shadow,normal,bright: BITSET);
PROCEDURE setcolors;

PROCEDURE pushtimeout(milisec: INTEGER);
PROCEDURE poptimeout;

---------------------------- blocks ----------------------------
                            --------

PROCEDURE block(b: defScreen.BLOCK; fill,pressed: BOOLEAN);

PROCEDURE panel(b: defScreen.BLOCK; VAR inter: defScreen.BLOCK;
                      fill,pressed: BOOLEAN);

PROCEDURE switch(b: defScreen.BLOCK; pressed: BOOLEAN);

PROCEDURE button(b: defScreen.BLOCK; pressed: BOOLEAN);

PROCEDURE inblock(x,y: INTEGER; b: defScreen.BLOCK): BOOLEAN;

PROCEDURE inblocks(x,y: INTEGER; SEQ b: defScreen.BLOCK): INTEGER;

--------------------------- messages ---------------------------
                           ----------

PROCEDURE mwait(cpdkeys: BITSET; SEQ kbkeys: CHAR);
PROCEDURE wait (SEQ kbkeys: CHAR);

PROCEDURE message(   xc,yc: INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
PROCEDURE perror (er,xc,yc: INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);

---------------------------- popups ----------------------------
                            --------

PROCEDURE pnew    (VAR pup: POPUP; x,y,w,h: INTEGER);
PROCEDURE pdispose(VAR pup: POPUP);

PROCEDURE popen (pup: POPUP);
PROCEDURE pclose(pup: POPUP);

PROCEDURE pclosed(pup: POPUP): BOOLEAN;

PROCEDURE pblock (pup: POPUP; VAR block: defScreen.BLOCK);

--------------------------- dialogbox --------------------------
                           -----------

PROCEDURE diabox(xc,yc,w: INTEGER;
                 VAR str: ARRAY OF CHAR;
               promptfmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE confirm(xc,yc,w: INTEGER;
                  VAR str: ARRAY OF CHAR;
                promptfmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);


----------------------------- debug ----------------------------
                             -------

PROCEDURE gnew    (VAR debug: DEBUG; x,y,w,h: INTEGER);
PROCEDURE gdispose(VAR debug: DEBUG);

PROCEDURE gopen (debug: DEBUG);
PROCEDURE gclose(debug: DEBUG);

PROCEDURE gprint(debug: DEBUG; fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

---------------------------- rollers ---------------------------
                            ---------

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

PROCEDURE rblocks(rol: ROLLER; VAR main,txt,tit,off,up,rl,dw: defScreen.BLOCK);

----------------------------- menus ----------------------------
                             -------

PROCEDURE mnew    (VAR m: MENU; x,y,w,h: INTEGER; exit: BITSET;
                  titfmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
PROCEDURE mdispose(VAR m: MENU);

PROCEDURE mprint(m: MENU;                alt: INTEGER;
               fmt: ARRAY OF CHAR;  SEQ args: SYSTEM.WORD);

PROCEDURE mread(m: MENU; alt: INTEGER; VAR s: ARRAY OF CHAR);

PROCEDURE mhotkey(m: MENU; alt: INTEGER; hotkey: CHAR; capequ: BOOLEAN);

PROCEDURE mopen (m: MENU);
PROCEDURE mclose(m: MENU);

PROCEDURE mselect  (m: MENU);
PROCEDURE mselected(m: MENU): BOOLEAN;

PROCEDURE mchoose(m: MENU; alt: INTEGER);

PROCEDURE malt(m: MENU): INTEGER;

PROCEDURE mblocks(m: MENU; VAR main,txt,tit,off: defScreen.BLOCK);

----------------------------- direx ----------------------------
                             -------

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

PROCEDURE dblocks(drx: DIREX; VAR main,txt,tit,off,up,rl,dw: defScreen.BLOCK);

----------------------------------------------------------------

PROCEDURE zoom;

END pmPUP.

----------------------------------------------------------------

   NOTES:

1. "*dispose" never change "done" & "error" by itself!
2. "*blocks"  defined only after first "*open" or "*select"


ch    keeps value of last key readen from Keyboard by PM (000c if none)

mx,my keep  values of x- & y-coordinates of mouse cursor driven by PM.
      (any panel set cursor on it "off" block at open time)

time  keeps the value of the time not spended for waiting.
      Application may use the value timeout-time to
      calcuate the time spended by human for last input.



PROCEDURE setplanes(black,shadow,normal,bright: BITSET);
PROCEDURE setcolors;
-------------------

     By default PM used last (leftmost or higest) 2 planes on dispay.
Application may orevride this by calling "setplanes".
     PM not changed screen palette, till he is asked about it
by calling "setcolors". The last one set 4 gradations of grey
(if it possible) for current plane combination used for colors
black, shadow, normal & bright.


PROCEDURE pushfont(font: defFont.FONT);
PROCEDURE popfont;
-----------------

     Changes default font for printing text in PM.
     Be carefull! Current "font" linked with any "new"
popup, menu, roller, direx and will be used with it
as long as this object exist.
     Application may push and pop fonts freely, but it
will not dispose appropriate font before all object
referenced to it disposed!

PROCEDURE pushtimeout(milisec: INTEGER);
PROCEDURE poptimeout;
--------------------
     Changes current timeout time (in miliseconds).
     This timeout will be actual for all procedures
waiting mouse or keyboard input (such as "wait",
"message", "perror", "select" and so on) until next
push or pop call will be executed.
     To prevent timeout waiting execute call
"pushtimeout(-1)" or "pushtimeout(MAX(INTEGER))"
(realy it will be timeout for 24 days and nights).
     Aoolication may determine that "timeout" condition
was occured by testing PM.time<=0!

PROCEDURE inblock (x,y: INTEGER;     b: defScreen.BLOCK): BOOLEAN;
PROCEDURE inblocks(x,y: INTEGER; SEQ b: defScreen.BLOCK): INTEGER;
------------------
     "inblock"  returns TRUE when x,y IN block "b".
     "inblocks" returns the number of the block
                in wich x,y points otherwise -1.

PROCEDURE mwait(cpdkeys: BITSET; SEQ kbkeys: CHAR);
PROCEDURE wait (SEQ kbkeys: CHAR);
---------------

  "mwait" Waits for one of "kbkeys"  pressed on keyboard
         or one of "cpdkeys" pressed on mouse
         or timeout occured
    wait(....,0c)    waits any key pressed on keyboard
    wait(time,{})    waits for CR or ESC
    wait(time,{1,2}) waits for buttons 1 or 2 on mouse
If cpdkeys={} "mwait" do not touch mouse at all.
  "wait" is equal to mwait({0..CPD.state^.nokeys-1},kbkeys);

  "done" always TRUE.

PROCEDURE message(xc,yc   : INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
PROCEDURE perror (xc,yc,er: INTEGER; f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
-----------------

     "message" print message (format,args) in the appropriated box
with center xc,yc on the screen and waits, driving the mouse
cursor, until one of the next conditions will happen:
        - timeout occured (see timeout)
        - CR of ESC pressed on the Keyboard;
        - leftmost  mouse key pressed when cursor in "off" button of
          message panel;
        - rightmost mouse key pressed (cursor anywhere);
"perror"  is equal to sequence of calls:
          Lexicon.perror(bump,er,f,a);
          message(t,xc,yc,"%s",bump).
     Both procedures set "done"=FALSE only when it's impossible
to open appropriate window or parameters is bad. "done"=TRUE at least
human press ESC or timeout happens.

PROCEDURE pclosed(pup: POPUP): BOOLEAN;
-----------------

   Window opened or closed?
   returns TRUE of FALSE; never change "done", "error".
   returns TRUE for bad POPUP object.



NEWS


   psave
   prestore

   direx:  bright for directories.

   exit keys on CPD (diabox, message e.t.c)

   block for menu N'th alt
