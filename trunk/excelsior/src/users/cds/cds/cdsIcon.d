DEFINITION MODULE cdsIcon; (* Sem 25-Feb-91. (c) KRONOS *)

FROM libWindows  IMPORT window;

TYPE ICON;

CONST nil=ICON(NIL);

PROCEDURE close(VAR w: window; VAR i: ICON; x,y: INTEGER);
PROCEDURE move (w: window; VAR i: ICON; x,y: INTEGER);

PROCEDURE on_off(w: window; VAR i: ICON; x,y: INTEGER; VAR v: BOOLEAN);

PROCEDURE number -- окошко с целым числом
  (w: window; VAR i: ICON; x,y,sx,fr,to: INTEGER; VAR val: INTEGER);

PROCEDURE pcb_number
  (w: window; VAR i: ICON; x,y: INTEGER; VAR v: INTEGER);

PROCEDURE do_icon(i: ICON; x,y: INTEGER; c: CHAR): BOOLEAN;

PROCEDURE remove(VAR i: ICON);

END cdsIcon.
