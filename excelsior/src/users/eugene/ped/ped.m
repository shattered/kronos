MODULE ped; (* 13-Feb-87. (c) KRONOS *)

FROM Model     IMPORT   Object, Objects, RemoveModel, Lget, NewObject;
FROM ModelIO   IMPORT   ReadModel, WriteModel;
FROM ModelPbl  IMPORT   Exception?, Reaction, Message, KillReaction;
FROM pedEditor IMPORT   Sheet, EdtContext, Editor;
FROM pedTools  IMPORT   ToolsMenu;

               IMPORT  vg  : pedVG;
               IMPORT  gg  : VG;
               IMPORT  wnd : Windows;
               IMPORT  mem : cdsHeap;
               IMPORT  men : Menu;

WITH STORAGE (NEW    : mem.Allocate;
              DISPOSE: mem.Deallocate;
              RESIZE : mem.Reallocate);


VAR
    Name   : ARRAY [0..15] OF CHAR;
    mdl    : Object;
    sht    : Sheet;

PROCEDURE main(w: wnd.window; n,m: INTEGER);
  VAR e: Reaction;
BEGIN
  IF Exception?(e) THEN men.alarm(Message); RETURN END;
  CASE n OF
  |0:
  |1: men.alarm('Ну что здесь не понятно?');
  |2: Name[0]:=0c;
      men.readln(5,100,'model name',Name);
      IF Name[0]#0c THEN
        mdl:=ReadModel(Name);
        IF mdl#NIL THEN
          NEW(sht);
          sht^.mdl:=mdl;
          sht^.wnd:=NIL;
          sht^.EditorContext:=EdtContext(NIL);
          sht^.PublicContext:=NIL;
          sht^.ScreenContext:=NIL;
          Editor(sht)
        END
      END
  |3: men.alarm('Не реализовано')
  |4: ToolsMenu
  |5: IF men.qwest(5,100,'Cansel !!!  Are you sure ?') THEN HALT END
  END;
  KillReaction(e)
END main;

PROCEDURE init_palette;
BEGIN
  gg.color(00,00,00,00);  --- 0 - solder;
  gg.color(01,11,07,00);  --- 1 - component
  gg.color(02,00,08,00);  --- 2 - intensive
  gg.color(03,08,08,08);  --- 3 - cursor

  gg.color(04,08,08,08);
  gg.color(05,12,00,08);
  gg.color(06,00,08,08);

  gg.color(07,12,12,00);  -- int vias

  gg.color(08,13,13,13);
  gg.color(09,13,11,11);
  gg.color(10,11,13,11);
  gg.color(11,13,13,13);

  gg.color(12,11,11,13);
  gg.color(13,13,11,13);
  gg.color(14,11,13,13);
  gg.color(15,13,13,13);

  gg.paint(FALSE);
END init_palette;

BEGIN
  init_palette;
  men.menu(4,100,'ped|help|read model|new model|tools menu|cansel',main);
  wnd.job;
END ped.
