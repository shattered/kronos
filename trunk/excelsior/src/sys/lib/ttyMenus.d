DEFINITION MODULE ttyMenus; (*$N+ Leo 25-Jun-90. (c) KRONOS *)

IMPORT SYSTEM;

TYPE KIND = (updown, barline, pop_up);

TYPE MENU;

VAL done: BOOLEAN;
   error: INTEGER;
    null: MENU;

PROCEDURE exit(m: MENU; c: CHAR);

PROCEDURE selector(m: MENU; c: CHAR);

PROCEDURE fill_bar(m: MENU);

PROCEDURE frame(m: MENU; on: INTEGER);

PROCEDURE title(m: MENU; fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE set_alt_pos(m: MENU; no: INTEGER);

PROCEDURE mark_alt_pos(m: MENU; mark: BOOLEAN);

PROCEDURE set_alt(m: MENU;
                 no: INTEGER;
             hotkey: CHAR;
            capital: BOOLEAN;
                fmt: ARRAY OF CHAR;
           SEQ args: SYSTEM.WORD);

PROCEDURE open(m: MENU; l,c: INTEGER);

PROCEDURE close(m: MENU);

PROCEDURE on_bottom(m: MENU);

PROCEDURE on_top(m: MENU);

PROCEDURE index(s: ARRAY OF CHAR; ch: CHAR; VAR inx: INTEGER): BOOLEAN;

PROCEDURE select(m: MENU);

PROCEDURE set_select(m: MENU; a: INTEGER);

PROCEDURE popup(m: MENU; l,c: INTEGER);

PROCEDURE alt(m: MENU): INTEGER;

PROCEDURE hotkey(m: MENU): CHAR;

PROCEDURE last(m: MENU): CHAR;

PROCEDURE new(VAR m: MENU; kind: KIND; w: INTEGER);

PROCEDURE dispose(VAR m: MENU);

END ttyMenus.
