MODULE tl; (* 13-Feb-87. (c) KRONOS *)

IMPORT T: Terminal;
IMPORT  m: pedMouse;
FROM Args      IMPORT   TakeWord, ScanFlags, Flag?, NumFlag?;
FROM Model     IMPORT   Object, Objects, Iterate, SigTypes;
FROM ModelIO   IMPORT   ReadModel, WriteModel;
FROM ModelPbl  IMPORT   Exception?, Reaction, Message, MemoryOverflow,
                        RaiseInMe, KillReaction;
FROM pedScreen IMPORT   OpenWindow, SetWindow, CloseWindow, Text, Cursor;
FROM Scheduler  IMPORT  EnterGate, ExitGate, Wait, TimeSignal;
FROM pedTopology IMPORT Areas, Del;
FROM pedTools   IMPORT  Tools;
FROM Layout    IMPORT   Layouter, TotalGang, maxV;
FROM Points     IMPORT  DeallocPoints;
FROM cdsHeap   IMPORT   Allocate;
FROM ModelMisc IMPORT   StartConductor, NextConductor, DelConductor,
                        Fixed, Empty;
FROM Clock      IMPORT  SystemClock;
FROM Image      IMPORT  image0;
FROM DWS        IMPORT  InitDWS, RemoveDWS;
FROM cdsImp     IMPORT  Bril;
FROM pedEditor  IMPORT  Sheet, EdtContext, Editor;
IMPORT vg: pedVG;
IMPORT  mcd: mCodeMnem;
IMPORT  Windows;

CONST DefTool=4;

VAR e           : Reaction;
    Name        : ARRAY [0..80] OF CHAR;
    mdl         : Object;
    Tool        : INTEGER;
    WriteTime   : INTEGER;
    Abort       : BOOLEAN;
    sht         : Sheet;
    CrsX, CrsY  : INTEGER;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE Poz(o: Object): INTEGER;
CODE mcd.lsw0 mcd.lib 0FFh mcd.bic 8 mcd.shr END Poz;

PROCEDURE setPoz(o: Object; p: INTEGER);
CODE
  mcd.stot mcd.copt mcd.lsw0 mcd.lib 0FFh
  mcd.and mcd.lodt 8 mcd.shl mcd.add mcd.ssw0
END setPoz;

PROCEDURE Calc(s: Object; d: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF (fixed IN s^.sType)OR(power IN s^.sType) THEN
    i:=1
  ELSE
    i:=Areas(s,sht)
  END;
  s^.sGang:=i;
  IF i>1 THEN INC(TotalGang,i-1) END;
  s^.sHard:=0;
END Calc;

PROCEDURE LayoutSignal(s: Object; d: INTEGER);
  VAR i,j: INTEGER; ln: ARRAY [0..79] OF CHAR;
      re: Reaction; r: BOOLEAN;
      x,y: INTEGER; c: CHAR;
BEGIN
  IF (Tag(s)#signal)OR Abort THEN RETURN END;
  IF s^.sGang<=1 THEN RETURN END;
  i:=SystemClock(); j:=WriteTime;
  Text(sht,0,'%2d:%2$d:%2$d -- %2d:%2$d:%2$d %3d %s',
           i DIV 3600 MOD 24,i DIV 60 MOD 60,i MOD 60,
           j DIV 3600 MOD 24,j DIV 60 MOD 60,j MOD 60,
           TotalGang,
           s^.Name);
  IF (i-j)>=30*60 THEN
    r:=Exception?(re);
    IF r THEN
      IF r=MemoryOverflow THEN
        DeallocPoints; WriteModel(mdl);
      ELSE
        RaiseInMe(r);
      END;
    ELSE
      WriteModel(mdl);
      KillReaction(re);
    END;
    WriteTime:=SystemClock();
  END;
  Layouter(sht,s,Tool,CrsX,CrsY);
  IF m.empty?() THEN Abort:=FALSE
  ELSE m.first(x,y,c); m.drop; Abort:=(c='Ц') END;
END LayoutSignal;

PROCEDURE Clear(o: Object; d: INTEGER);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  StartConductor(o,FALSE);
  WHILE NOT Empty DO
    IF NOT Fixed THEN DelConductor END;
    NextConductor;
  END;
END Clear;

PROCEDURE ClearO(o: Object; d: INTEGER);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  StartConductor(o,FALSE);
  WHILE NOT Empty DO
    IF NOT Fixed THEN Del(sht) END;
    NextConductor;
  END;
END ClearO;

PROCEDURE optim(s: Object; d: INTEGER);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  ClearO(s,0);
  Calc(s,0);
  --изменить константы
  LayoutSignal(s,0);
END optim;

VAR i: INTEGER;

BEGIN
  Windows.del_crs;
  ScanFlags;
  IF NumFlag?('t',Tool) THEN Tool:=DefTool END;
  IF NumFlag?('v',maxV) THEN maxV:=1000000 END;
  TakeWord(Name);
  IF Name[0]=0c THEN
    T.print('    tl <model name> [t=<tool>]\n'
            '  Автоматическая трассировка связей в модели печатной платы.\n'
            '  Ключи:\n'
            't=<tool>  выбор инструмента.\n');
    T.print('-n        удалить результаты предидущих итераций.\n'
            '-c        удалить результаты предидущих итераций и кончить.\n'
            '-g        осуществить сглаживание.\n');
    HALT;
  END;
  T.print('Трассировка будет вестись инструментом #%d:\n\n'
          '   ширина трассы               : %5dmkm\n'
          '   диаметр переходных отверстий: %5dmkm\n'
          '   номер сверла                : %5d\n\n',
          Tool,
          Tools[Tool].Size*625 DIV 12,
          Tools[Tool].VSize*625 DIV 12,
          Tools[Tool].DSize);
  T.print('Вы согласны с выбором инструмента?');
  IF m.Read()#'y' THEN HALT END; T.print('y\n');
  IF Exception?(e) THEN T.Show(Message); HALT END;
  mdl:=ReadModel(Name);
  Allocate(sht,SIZE(sht^));
  sht^.mdl:=mdl;
  sht^.wnd:=NIL;
  sht^.EditorContext:=EdtContext(NIL);
  sht^.PublicContext:=NIL;
  sht^.ScreenContext:=NIL;
  Editor(sht);
  sht^.PublicContext^.ExtPin:=FALSE;
  WriteTime:=SystemClock();
  IF Flag?('n')OR Flag?('c') THEN
    Iterate(mdl^.All,Clear,0);
    WriteModel(mdl); WriteTime:=SystemClock();
    IF Flag?('c') THEN HALT END;
  END;
  IF Flag?('g') THEN
    SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    Wait(TimeSignal); Wait(TimeSignal);
    Iterate(mdl^.All,Bril,sht);
    WriteModel(mdl); WriteTime:=SystemClock();
    HALT;
  END;
  IF Flag?('o') THEN
    TotalGang:=0; Iterate(mdl^.All,Calc,0);
    InitDWS(mdl,Tool);
    SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    CrsX:=mdl^.ctX DIV 2;
    CrsX:=mdl^.ctY DIV 2;
    Abort:=FALSE;
    SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    Iterate(mdl^.All,optim,sht);
    WriteModel(mdl);
    RemoveDWS;
    HALT;
  END;
  TotalGang:=0; Iterate(mdl^.All,Calc,0);
  IF TotalGang=0 THEN HALT END;
  InitDWS(mdl,Tool);
  SetWindow(sht,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
  CrsX:=mdl^.ctX DIV 2;
  CrsX:=mdl^.ctY DIV 2;
  Abort:=FALSE;
  WHILE (TotalGang>0)& NOT Abort DO
    Iterate(mdl^.All,LayoutSignal,0);
  END;
  WriteModel(mdl);
  RemoveDWS;
END tl.
