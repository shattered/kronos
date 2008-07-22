DEFINITION MODULE fedMenus; (* nick 10-Jan-91. (c) KRONOS *)

IMPORT  defScreen;

(* for image editor *)
CONST
  _mirr = 0;  _fill = 1;  _swap = 2;  _dot = 3;
  _iddt = 4;  _idln = 5;  _dupl = 6;  _inv = 7;

CONST Read = 0;  Save = 1;

VAL coor: defScreen.BLOCK;
    refr: defScreen.BLOCK;
    save: defScreen.BLOCK;
    line: defScreen.BLOCK;
    mode: INTEGER;

PROCEDURE main_menus;

PROCEDURE readsave(mode: INTEGER);

PROCEDURE operation(VAR cmd: INTEGER);

PROCEDURE setlines;


END fedMenus.
