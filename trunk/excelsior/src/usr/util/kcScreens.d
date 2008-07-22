DEFINITION MODULE kcScreens; (* kl 25-Oct-90. (c) KRONOS *)

IMPORT  SYSTEM,Terminal;

TYPE SCREEN;

VAL TTY: SCREEN; state: Terminal.STATE; done: BOOLEAN;

PROCEDURE set_reverse(i: INTEGER);
PROCEDURE set_color  (i: INTEGER);
PROCEDURE set_back   (i: INTEGER);
PROCEDURE set_cursor (i: INTEGER);

PROCEDURE create(VAR scr: SCREEN; line,col,h,w: INTEGER);
PROCEDURE on (scr: SCREEN);
PROCEDURE off(scr: SCREEN);

PROCEDURE on_top   (scr: SCREEN);
PROCEDURE on_bottom(scr: SCREEN);
PROCEDURE delete (scr: SCREEN);
PROCEDURE kill   (scr: SCREEN);
PROCEDURE refresh(scr: SCREEN);
PROCEDURE move   (scr: SCREEN; line,col: INTEGER);

PROCEDURE set_pos(scr: SCREEN; line,col: INTEGER);
PROCEDURE write (scr: SCREEN; s: ARRAY OF CHAR; pos,len: INTEGER);
PROCEDURE print (scr: SCREEN; s: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE repeat(scr: SCREEN; c: CHAR;         times:    INTEGER);
PROCEDURE right (scr: SCREEN; i: INTEGER);
PROCEDURE left  (scr: SCREEN; i: INTEGER);

PROCEDURE Write (scr: SCREEN; c: CHAR);
PROCEDURE WriteString(scr: SCREEN; s: ARRAY OF CHAR);
PROCEDURE WriteLn(scr: SCREEN);

PROCEDURE center(scr: SCREEN; s: ARRAY OF CHAR; line:    INTEGER);
PROCEDURE title (scr: SCREEN; s: ARRAY OF CHAR);
PROCEDURE frame (scr: SCREEN);
PROCEDURE refresh_all;

END kcScreens.
