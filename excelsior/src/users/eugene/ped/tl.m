MODULE tl; (* 13-Feb-87. (c) KRONOS *)

IMPORT  defCodes;
IMPORT  arg: tskArgs;
IMPORT  pcb: Model;
IMPORT  mio: ModelIO;
IMPORT  pbl: ModelPbl;
IMPORT  scr: pedScreen;
IMPORT  tpl: pedTopology;
IMPORT  tls: pedTools;
IMPORT  Layout;
IMPORT  Points;
IMPORT  mem: cdsHeap;
IMPORT  ModelMisc;
IMPORT  tim: Time;
IMPORT  DWS;
IMPORT  cdsImp;
IMPORT  pedEditor;
IMPORT  pedMouse;
IMPORT  Windows;
IMPORT  tty: Terminal;
IMPORT  gg: VG;

WITH STORAGE (NEW    : mem.Allocate;
              DISPOSE: mem.Deallocate;
              RESIZE : mem.Reallocate);

CONST DefTool=4;

VAR e           : pbl.Reaction;
    Name        : ARRAY [0..80] OF CHAR;
    mdl         : pcb.Object;
    Tool        : INTEGER;
    WriteTime   : INTEGER;
    Abort       : BOOLEAN;
    sht         : pedEditor.Sheet;
    CrsX, CrsY  : INTEGER;

PROCEDURE Tag(o: pcb.Object): pcb.Objects;
CODE 0 defCodes.lxb END Tag;

PROCEDURE Calc(s: pcb.Object; d: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF Tag(s)#pcb.signal THEN RETURN END;
  IF (pcb.fixed IN s^.sType)OR(pcb.power IN s^.sType) THEN
    i:=1
  ELSE
    i:=tpl.Areas(s,sht)
  END;
  s^.sGang:=i;
  IF i>1 THEN INC(Layout.TotalGang,i-1) END;
  s^.sHard:=0;
END Calc;

PROCEDURE LayoutSignal(s: pcb.Object; d: INTEGER);
  VAR i,j: INTEGER; ln: ARRAY [0..79] OF CHAR;
      re: pbl.Reaction; r: BOOLEAN;
      x,y: INTEGER; c: CHAR;
BEGIN
  IF (Tag(s)#pcb.signal)OR Abort THEN RETURN END;
  IF s^.sGang<=1 THEN RETURN END;
  i:=tim.sys_time(tim.sec); j:=WriteTime;
  scr.Text(sht,0,'%2d:%2$d:%2$d -- %2d:%2$d:%2$d %3d %s',
           i DIV 3600 MOD 24,i DIV 60 MOD 60,i MOD 60,
           j DIV 3600 MOD 24,j DIV 60 MOD 60,j MOD 60,
           Layout.TotalGang,
           s^.Name);
  IF (i-j)>=30*60 THEN
    r:=pbl.Exception?(re);
    IF r THEN
      IF r=pbl.MemoryOverflow THEN
        Points.DeallocPoints; mio.WriteModel(mdl);
      ELSE
        pbl.RaiseInMe(r);
      END;
    ELSE
      mio.WriteModel(mdl);
      pbl.KillReaction(re);
    END;
    WriteTime:=tim.sys_time(tim.sec)
  END;
  Layout.Layouter(sht,s,Tool,CrsX,CrsY);
  IF pedMouse.empty?() THEN Abort:=FALSE
  ELSE pedMouse.first(x,y,c); pedMouse.drop; Abort:=(c='Ц') END;
END LayoutSignal;

PROCEDURE Clear(o: pcb.Object; d: INTEGER);
BEGIN
  IF Tag(o)#pcb.signal THEN RETURN END;
  ModelMisc.StartConductor(o,FALSE);
  WHILE NOT ModelMisc.Empty DO
    IF NOT ModelMisc.Fixed THEN ModelMisc.DelConductor END;
    ModelMisc.NextConductor;
  END;
END Clear;

PROCEDURE ClearO(o: pcb.Object; d: INTEGER);
BEGIN
  IF Tag(o)#pcb.signal THEN RETURN END;
  ModelMisc.StartConductor(o,FALSE);
  WHILE NOT ModelMisc.Empty DO
    IF NOT ModelMisc.Fixed THEN tpl.Del(sht) END;
    ModelMisc.NextConductor;
  END;
END ClearO;

PROCEDURE optim(s: pcb.Object; d: INTEGER);
BEGIN
  IF Tag(s)#pcb.signal THEN RETURN END;
  ClearO(s,0);
  Calc(s,0);
  --изменить константы
  LayoutSignal(s,0);
END optim;

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

VAR i: INTEGER;

BEGIN
  init_palette;
  Windows.del_crs;
  IF NOT arg.number('t',Tool)        THEN Tool:=DefTool END;
  IF NOT arg.number('v',Layout.maxV) THEN Layout.maxV:=1000000 END;
  IF HIGH(arg.words)<0 THEN
    tty.print('    tl <model name> [t=<tool>]\n'
            '  Автоматическая трассировка связей в модели печатной платы.\n'
            '  Ключи:\n'
            't=<tool>  выбор инструмента.\n');
    tty.print('-n        удалить результаты предидущих ытераций.\n'
            '-c        удалить результаты предидущих ытераций и кончить.\n'
            '-g        осуществить сглаживание.\n');
    HALT;
  END;
  Name:=arg.words[0];
  tty.print('Трассировка будет вестись инструментом #%d:\n\n'
          '   ширина трассы               : %5dmkm\n'
          '   диаметр переходных отверстий: %5dmkm\n'
          '   номер сверла                : %5d\n\n',
          Tool,
          tls.Tools[Tool].Size *625 DIV 12,
          tls.Tools[Tool].VSize*625 DIV 12,
          tls.Tools[Tool].DSize);
  tty.print('Вы согласны с выбором инструмента?');
  IF pedMouse.Read()#'y' THEN HALT END; tty.print('y\n');
  IF pbl.Exception?(e)   THEN tty.print('%s\n',pbl.Message); HALT END;
  mdl:=mio.ReadModel(Name);
  NEW(sht);
  sht^.mdl:=mdl;
  sht^.wnd:=NIL;
  sht^.EditorContext:=pedEditor.EdtContext(NIL);
  sht^.PublicContext:=NIL;
  sht^.ScreenContext:=NIL;
  pedEditor.Editor(sht);
  sht^.PublicContext^.ExtPin:=FALSE;
  WriteTime:=tim.sys_time(tim.sec);
  IF arg.flag('-','n') OR arg.flag('-','c') THEN
    pcb.Iterate(mdl^.All,Clear,0);
    mio.WriteModel(mdl); WriteTime:=tim.sys_time(tim.sec);
    IF arg.flag('-','c') THEN HALT END;
  END;
  IF arg.flag('-','g') THEN
    scr.SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    tim.delay(1,tim.sec); tim.delay(1,tim.sec);
    pcb.Iterate(mdl^.All,cdsImp.Bril,sht);
    mio.WriteModel(mdl); WriteTime:=tim.sys_time(tim.sec);
    HALT;
  END;
  IF arg.flag('-','o') THEN
    Layout.TotalGang:=0; pcb.Iterate(mdl^.All,Calc,0);
    DWS.InitDWS(mdl,Tool);
    scr.SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    CrsX:=mdl^.ctX DIV 2;
    CrsX:=mdl^.ctY DIV 2;
    Abort:=FALSE;
    scr.SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    pcb.Iterate(mdl^.All,optim,sht);
    mio.WriteModel(mdl);
    DWS.RemoveDWS;
    HALT;
  END;
  Layout.TotalGang:=0; pcb.Iterate(mdl^.All,Calc,0);
  IF Layout.TotalGang=0 THEN HALT END;
  DWS.InitDWS(mdl,Tool);
  scr.SetWindow(sht,32,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
  CrsX:=mdl^.ctX DIV 2;
  CrsX:=mdl^.ctY DIV 2;
  Abort:=FALSE;
  WHILE (Layout.TotalGang>0)& NOT Abort DO
    pcb.Iterate(mdl^.All,LayoutSignal,0);
  END;
  mio.WriteModel(mdl);
  DWS.RemoveDWS;
END tl.
