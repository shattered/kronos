DEFINITION MODULE fedScreen; (* nick 13-Dec-90. (c) KRONOS *)

IMPORT  SYSTEM, defScreen, defBMG;

VAL bmd: defBMG.BITMAP;   -- screen bitmap
   full: defScreen.TOOL;  -- full screen block
   wblk: defScreen.BLOCK; -- working area
   mblk: defScreen.BLOCK; -- menus block
   tblk: defScreen.BLOCK; -- block for title
   sblk: defScreen.BLOCK; -- block for save char menu
   rblk: defScreen.BLOCK; -- block for refresh char menu
   quit: defScreen.BLOCK; -- block for quit


VAL version: ARRAY [0..31] OF CHAR;

----------------------- F u r n i t u r e ----------------------
                       -------------------

PROCEDURE block(b: defScreen.BLOCK; fill,pressed: BOOLEAN);
PROCEDURE print(b: defScreen.BLOCK; xpos: INTEGER;
                                     fmt: ARRAY OF CHAR;
                                 SEQ arg: SYSTEM.WORD);
PROCEDURE clear;


------------------ C u r s o r   S u p p o r t -----------------
                  -----------------------------

PROCEDURE inrect(x,y,w,h: INTEGER): BOOLEAN;

PROCEDURE inblock(b: defScreen.BLOCK): BOOLEAN;

END fedScreen.
