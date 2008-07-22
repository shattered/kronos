DEFINITION MODULE Keyboard; (* Ned 21-Sep-89. (c) KRONOS *) IMPORT  SYSTEM;

TYPE BREAK  = PROCEDURE (INTEGER);
     STATE  = POINTER TO STATUS;
     STATUS = RECORD
                type : INTEGER;
                fkeys: INTEGER;
                buf  : CHAR;
                togs : BITSET;

                ubrk : BREAK;
                freq : INTEGER;
                dur  : INTEGER;

                breakon: INTEGER;
                foreign: INTEGER;
                caps   : INTEGER;
                shift  : INTEGER;
                raw    : INTEGER;
                nums   : INTEGER;
                scan   : INTEGER;
                autorep: INTEGER;
             END;

VAL
  done : BOOLEAN;       error: INTEGER;         state: STATE;

PROCEDURE read(VAR ch: CHAR);

PROCEDURE scan(VAR ch: CHAR; VAR togs: BITSET);
(* for keyboards that can produce "scan" codes, read key and    *)
(* state of "toggles" (see below). Every key will be read twice *)
(* first time with tog "press", second without it!              *)
(* When "scan" returns 0c as a "ch" it means that state of togs *)
(* changed.                                                     *)
(* Last read values also putted in "state^.buf" & "state^.togs" *)

PROCEDURE drop;
(* "read" and ignore a single key *)

PROCEDURE swallow;
(* read all ready keys *)

PROCEDURE ready(): INTEGER;

PROCEDURE wait(timeout: INTEGER);   (* until pressed or timeout (milisecs) *)

PROCEDURE bell(n: INTEGER);

PROCEDURE set_bell(freq,dura: INTEGER);

PROCEDURE set_raw(ON_OFF: INTEGER);

PROCEDURE user_break(new: BREAK);

PROCEDURE set_break  (ON_OFF: INTEGER);
PROCEDURE set_shift  (ON_OFF: INTEGER);
PROCEDURE set_nums   (ON_OFF: INTEGER);
PROCEDURE set_caps   (ON_OFF: INTEGER);
PROCEDURE set_foreign(ON_OFF: INTEGER);
PROCEDURE set_autorep(ON_OFF: INTEGER);

PROCEDURE ioctl(no: INTEGER; SEQ args: SYSTEM.WORD);

PROCEDURE attach(dev_name: ARRAY OF CHAR);

PROCEDURE reset; (* to initial state *)

PROCEDURE restore(status: STATUS);

PROCEDURE nop;

CONST
  lf    = 012c; (* ^J *)
  cr    = 015c; (* ^M *)
  break = 003c; (* ^C break proc(0) *)
  exit  = 005c; (* ^E *)
  back  = 010c; (* ^H *)
  tab   = 011c; (* ^I *)
  vt    = 013c; (* ^K *)
  rep   = 032c; (* ^Z *)
  nak   = 025c; (* ^U break proc(1) *)
  can   = 030c; (* ^X break proc(2) *)
  nl    = 036c; (* ^^ *)

  up    = 200c;  dw    = 201c;  right = 202c;  left  = 203c;
  pgup  = 204c;  pgdw  = 205c;  home  = 206c;  end   = 207c;
  del   = 210c;  ins   = 211c;  bcktab= 212c;  newln = 213c;

  f1    = 220c;  f2    = 221c;  f3    = 222c;  f4    = 223c;
  f5    = 224c;  f6    = 225c;  f7    = 226c;  f8    = 227c;
  f9    = 230c;  f10   = 231c;  f11   = 232c;  f12   = 233c;
  f13   = 234c;  f14   = 235c;  f15   = 236c;  center= 237c;

CONST (* scancode togs *)
  altL   = {0};
  altR   = {1};
  alt    = altL+altR;
  ctrlL  = {2};
  ctrlR  = {3};
  ctrl   = ctrlL+ctrlR;
  shiftL = {4};
  shiftR = {5};
  shift  = shiftL+shiftR;
  press  = {6};
  add    = {7}; (* arrow,pgup,pgdw,home,end,ins,del,center,+,-,*,enter
                   pressed on additional keyboard
                 *)

END Keyboard.

---------------------------  NOTES  ----------------------------
                           ---------

type -- unique keyboard identifier
        1-Фрящик
        2-Labtam XT
        4-Labtam HE
        7-Elorg

scan = TRUE iff keyboard driver generates ALT, CTRL, SHIFT pressed/unpressed
       for functional keys sequence
       more detailed scancodes may be taken in RAW mode!

f_range  number of keys f1,f2,...   min.10  max.15


        The bell frequency in Hz units for main octave:

             C          7643
             C#         7214
             D          6809
             D#         6427
             E          6066
             F          5726
             F#         5404
             G          5101
             G#         4815
             A          4545
             A#         4289
             B          4049
             C1         3822

        doubling or halving of frequence to other octaves.

        Bell duration in microseconds (for some
        implementation rounded upto multiple of 20)


user_break
=========

TYPE BREAK (n):

   n=0 -- break active      (etx 03c ^C)
   n=1 -- break sequence    (nak 25c ^U) (exa.: shell command file)
   n=2 -- break underground (can 30c ^X)

   new: new break reaction

   host reaction at any keyboard stored
   and restored after task finish or when keyboard reattached.

attach
======

  action(_init) executed:
          type,scan,fkeys -- changed

  if next keyboard attached and 'set_break' was executed for
  previos attached keyboard, 'old' break reaction proc for previos
  keyboard restored. So you can't redefine break action at
  more then one keyboard at a time!
  State of device (raw mode, break on/off e.t.c) saved
  at init or atach time and restored before halt or reattach

wait
====

  when timeout occured 'done=FALSE' 'error=time_out'

N O T E

     This  module  does  not  attach  driver  at  initialization.
Attaching  is  peformed  when  first  calling  of  driver occure.
Variable  -state- points to blank record before attaching. Hence,
if you want to use variable -state- before any calling to driver,
you have to call procedure -nop- to fulfil attaching.

                                              Leg, 15-Oct-90
