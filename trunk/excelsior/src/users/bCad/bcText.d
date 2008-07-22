DEFINITION MODULE bcText; (* brd 21-Apr-91. (c) KRONOS *)

IMPORT  pmWnd;
IMPORT  SYSTEM;

TYPE Char  = DYNARR OF INTEGER;
     PFONT = POINTER TO RECORD
              w,h  : INTEGER;
              propW: ARRAY CHAR OF INTEGER;
              ptr  : ARRAY CHAR OF Char;
              W,H  : INTEGER;
              ss,sc: INTEGER; -- slope  sin/cos
              is,ic: INTEGER; -- italic sin/cos
              magic: INTEGER;
            END;
     WINDOW = pmWnd.WINDOW;
       TOOL = pmWnd.TOOL;

VAR  font: PFONT;
VAL error: INTEGER;
     done: BOOLEAN;

PROCEDURE write_ch (w: WINDOW; t: TOOL; f: PFONT; VAR x,y: INTEGER; ch: CHAR);
PROCEDURE write_str(w: WINDOW; t: TOOL; f: PFONT; VAR x,y: INTEGER;
                                                        s: ARRAY OF CHAR);
PROCEDURE print    (w: WINDOW; t: TOOL; f: PFONT; VAR x,y: INTEGER;
                                   fmt: ARRAY OF CHAR; SEQ  arg: SYSTEM.WORD);
     (*   по   завершениии   процедуры  в  переменных  x,y  лежат
координаты правого нижнего угла знакоместа *)

PROCEDURE len(f: PFONT; fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE font_size (VAR f: PFONT; w,h: INTEGER);
PROCEDURE font_ital (VAR f: PFONT; x,y: INTEGER);
PROCEDURE font_slop (VAR f: PFONT; x,y: INTEGER);

PROCEDURE load(VAR f: PFONT; name: ARRAY OF CHAR);

END bcText.
