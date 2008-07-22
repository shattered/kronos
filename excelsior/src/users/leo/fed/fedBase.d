DEFINITION MODULE fedBase; (* nick 13-Dec-90. (c) KRONOS *)

IMPORT  SYSTEM, defScreen, defBMG, defFont;

VAL bmd: defBMG.BITMAP;   -- screen bitmap
   full: defScreen.TOOL;  -- full screen block
   wblk: defScreen.BLOCK; -- working area
   mblk: defScreen.BLOCK; -- menus block
   tblk: defScreen.BLOCK; -- block for title
   sblk: defScreen.BLOCK; -- block for save char menu
   rblk: defScreen.BLOCK; -- block for refresh char menu
   quit: defScreen.BLOCK; -- block for quit
   curs: defScreen.BLOCK; -- block for cursor
   dfnt: defFont.FONT;    -- defult font for messages

VAR efnt: defFont.FONT;   -- current font
   fW,fH: INTEGER;        -- width & height of current font

----------------------- F u r n i t u r e ----------------------
                       -------------------
PROCEDURE block(b: defScreen.BLOCK; fill,pressed: BOOLEAN);
PROCEDURE print(b: defScreen.BLOCK; xpos: INTEGER;
                                     fmt: ARRAY OF CHAR;
                                 SEQ arg: SYSTEM.WORD);
PROCEDURE clear;


------------------ C u r s o r   S u p p o r t -----------------
                  -----------------------------

PROCEDURE inrect (x,y,w,h: INTEGER): BOOLEAN;

PROCEDURE inblock(b: defScreen.BLOCK): BOOLEAN;

------------ F i l e  &  C h a r O p e r a t i o n s -----------
            -----------------------------------------

VAL done: BOOLEAN;
   error: INTEGER;        -- last error

PROCEDURE readchar(bmd: defBMG.BITMAP; ch: CHAR);
PROCEDURE savechar(bmd: defBMG.BITMAP; ch: CHAR);

PROCEDURE newfont(w,h: INTEGER; first,last: CHAR; state: BITSET);
PROCEDURE newsize(w,h: INTEGER;  cutw,cuth: INTEGER);
PROCEDURE disposefont;

PROCEDURE readfont (filename: ARRAY OF CHAR);
PROCEDURE writefont(filename: ARRAY OF CHAR);

PROCEDURE xwrite(tool: defScreen.TOOL; x,y: INTEGER; ch: CHAR): INTEGER;

END fedBase.
