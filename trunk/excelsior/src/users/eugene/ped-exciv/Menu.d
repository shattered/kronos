DEFINITION MODULE Menu; (* 13-Oct-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, WORD;
FROM Windows    IMPORT  window;

TYPE
  menu_proc=PROCEDURE (window, INTEGER, WORD);
  tty;

PROCEDURE message(x,y: INTEGER; s: ARRAY OF CHAR);

PROCEDURE menu (x,y: INTEGER; s: ARRAY OF CHAR; sel: menu_proc);

PROCEDURE panel(x,y: INTEGER; s: ARRAY OF CHAR; sel: menu_proc);

PROCEDURE cre_tmp_menu(s: ARRAY OF CHAR): window;

PROCEDURE tmp_menu(w: window; x,y: INTEGER; sel: menu_proc; info: WORD);

PROCEDURE readln(x,y: INTEGER; pmt: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);

PROCEDURE read_string(w: window; x,y,sz: INTEGER; VAR s: ARRAY OF CHAR);

PROCEDURE qwest(rx,ry: INTEGER; pmt: ARRAY OF CHAR): BOOLEAN;

PROCEDURE alarm(s: ARRAY OF CHAR);

PROCEDURE create_tty(sx,sy: INTEGER): tty;

PROCEDURE print(t: tty; fmt: ARRAY OF CHAR; SEQ arg: WORD);

PROCEDURE read_tty(t: tty): CHAR;

PROCEDURE remove_tty(t: tty);

PROCEDURE free_mem(x,y: INTEGER);

END Menu.
