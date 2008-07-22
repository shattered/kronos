MODULE cds; (*  09-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

IMPORT  mn : cdsMenu;
IMPORT  err: libCrash;
IMPORT  wnd: libWindows;
IMPORT  mdl: pedModel;
IMPORT  edt: pedEditor;
IMPORT  flx: pedFlex;

PROCEDURE cre_pcb;
  VAR nm: mdl.string; b: mdl.board;
BEGIN
  nm:='';
  mn.readln(-10000,-10000,'имя модели',nm);
  IF nm='' THEN RETURN END;
  mdl.cre_board(b); b^.name:=nm; b^.lays:={0..1}; edt.open_model(b);
END cre_pcb;

PROCEDURE read_pcb;
  VAR nm: ARRAY [0..79] OF CHAR; b: mdl.board;
BEGIN
  nm:='';
  mn.readln(-10000,-10000,'имя файла',nm);
  IF nm='' THEN RETURN END;
  b:=mdl.read_model(nm); edt.open_model(b);
END read_pcb;

PROCEDURE read_mdl;
  VAR nm: ARRAY [0..79] OF CHAR; b: mdl.board;
BEGIN
  nm:='';
  mn.readln(-10000,-10000,'имя файла',nm);
  IF nm='' THEN RETURN END;
  b:=flx.read_model(nm); edt.open_model(b);
END read_mdl;

PROCEDURE main_job(w: wnd.window; n: INTEGER; x: WORD);
  VAR e: err.trap;
BEGIN
  IF err.enter(e) THEN
    mn.message(-10000,-10000,1,4,'%s',e.txt);
  ELSE
    CASE n OF
      |1: read_pcb;
      |2: cre_pcb;
      |3: wnd.remove(w); HALT;
      |4: mn.zoom;
      |5: mn.rainbow;
      |6: read_mdl;
    END;
    err.exit(e);
  END;
END main_job;

BEGIN
  mn.menu(100,100,main_job,
    ' CDS\nпрочесть PCB\nсоздать PCB\n'
    'закончить работу\nлупа\nрадуга\nпрочесть MDL');
  wnd.job;
END cds.
