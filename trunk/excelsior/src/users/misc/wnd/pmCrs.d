DEFINITION MODULE pmCrs; (* Leo 16-Apr-91. (c) KRONOS *)

IMPORT  SYSTEM, defScreen;

VAL done: BOOLEAN;
   error: INTEGER;
      on: BOOLEAN;
     cur: INTEGER;
     x,y: INTEGER;
    clip: defScreen.BLOCK;
   color: BITSET;


CONST
  arrow    = -1;
  rarrow   = -2;
  clock    = -3;
  cross    = -4;
  xcross   = -5;

  mvarrows = -6;
  harrows  = -7;
  varrows  = -8;
  larrows  = -9;
  rarrows  = -10;


PROCEDURE read(VAR x,y: INTEGER; SEQ timer: INTEGER);
PROCEDURE monitor(SEQ timer: INTEGER);

PROCEDURE style (cur: INTEGER);
PROCEDURE move  (x,y: INTEGER);
PROCEDURE toggle(on : BOOLEAN);

PROCEDURE define  (cur,dx,dy,size: INTEGER; map: ARRAY OF SYSTEM.WORD);
PROCEDURE setcolor(cur: INTEGER; color: BITSET);
PROCEDURE setclip (cur: INTEGER; clip : defScreen.BLOCK);

END pmCrs.

(* USAGE:

     Cursor "saves" yhe screen image under it when it's toggeled on.
  So application can't draw at the screen any wait until
  cursor is "on". With one exclusion: pictures in "xor" mode
  may be drawwed and erased while cursor not "moved".


  pmCrs.toggle(TRUE);
  LOOP
    keys:=CPD.state^.keys;
    pmCrs.monitor;
    cha :=CPD.state^.keys/keys;
    ...... (x,y) coordinates of cursor also may be used here
  END;
  pmCrs.toggle(FALSE);

  If you need use special kind of addition cursors
  (rubber thread for example) next text may be used

  pmCrs.toggle(TRUE);
  LOOP
    "xor frame(x,y,X,Y)";
      keys:=CPD.state^.keys;
      pmCrs.read(x,y);
      cha :=CPD.state^.keys/keys;
    "xor frame(x,y,X,Y)";
    pmCrs.move(x,y);

    ...... (x,y) coordinates of cursor also may be used here
  END;
  pmCrs.toggle(FALSE);


PROCEDURE define(cur,dx,dy,size: INTEGER; map: ARRAY OF SYSTEM.WORD);

 "cur" must be in range [0..+15], "size"<=30.
 Standard cursors [-15..-1] can`t be redifined.
"map" is picture (2x2) ... (32x32) 1 word per line

*)
