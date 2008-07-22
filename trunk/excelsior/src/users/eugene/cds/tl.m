MODULE tl; (* 13-Feb-87. (c) KRONOS *)

FROM StdIO     IMPORT   Show, print, Query;
FROM Terminal   IMPORT  BusyRead, SetMode;
FROM Args      IMPORT   TakeWord, ScanFlags, Flag?, NumFlag?;
FROM Model     IMPORT   Object, Objects, Iterate, Tag, Poz, setPoz, SigTypes;
FROM ModelIO   IMPORT   ReadModel, WriteModel;
FROM ModelPbl  IMPORT   Exception?, Reaction, Message, MemoryOverflow,
                        RaiseInMe, KillReaction;
FROM pedScreen IMPORT   OpenWindow, SetWindow, CloseWindow, Text;
FROM Scheduler  IMPORT  EnterGate, ExitGate, Wait, TimeSignal;
FROM pedTopology IMPORT Areas;
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

CONST DefTool=4;

VAR e           : Reaction;
    Name        : ARRAY [0..80] OF CHAR;
    mdl         : Object;
    Tool        : INTEGER;
    WriteTime   : INTEGER;
    Abort       : BOOLEAN;

    CrsX, CrsY  : INTEGER;

PROCEDURE Calc(s: Object);
  VAR i: INTEGER;
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF (fixed IN s^.sType)OR(power IN s^.sType) THEN i:=1 ELSE i:=Areas(s) END;
  s^.sGang:=i; INC(TotalGang,i-1); s^.sHard:=0;
END Calc;

PROCEDURE LayoutSignal(s: Object);
  VAR i,j: INTEGER; ln: ARRAY [0..79] OF CHAR;
      re: Reaction; r: BOOLEAN;
BEGIN
  IF (Tag(s)#signal)OR Abort THEN RETURN END;
  IF s^.sGang=1 THEN RETURN END;
  i:=SystemClock(); j:=WriteTime;
  image0(ln,'%2d:%2$d:%2$d -- %2d:%2$d:%2$d %3d %s',
         i DIV 3600 MOD 24,i DIV 60 MOD 60,i MOD 60,
         j DIV 3600 MOD 24,j DIV 60 MOD 60,j MOD 60,
         TotalGang,
         s^.Name);
  Text(0,ln);
  IF (i-j)>=30*60 THEN
(*  r:=Exception?(re);
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
*)
    image0(ln,'Write model...'); Text(1,ln);
    DeallocPoints; WriteModel(mdl);
    WriteTime:=SystemClock();
  END;
  Layouter(mdl,s,Tool,CrsX,CrsY);
  Abort:=BusyRead()='Ц';
END LayoutSignal;

PROCEDURE Clear(o: Object);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  StartConductor(o);
  WHILE NOT Empty DO
    IF NOT Fixed THEN DelConductor END;
    NextConductor;
  END;
END Clear;

VAR i: INTEGER;

BEGIN
  ScanFlags;
--SetMode(FALSE);
  IF NumFlag?('t',Tool) THEN Tool:=DefTool END;
  IF NumFlag?('v',maxV) THEN maxV:=1000000 END;
  TakeWord(Name);
  IF Name[0]=0c THEN
    print('    tl <model name> [t=<tool>]\n'
          '  Автоматическая трассировка связей в модели печатной платы.\n'
          '  Ключи:\n'
          't=<tool>  выбор инструмента.\n');
    print('-n        удалить результаты предидущих итераций.\n'
          '-c        удалить результаты предидущих итераций и кончить.\n'
          '-g        осуществить сглаживание.\n');
    HALT;
  END;
  print('Трассировка будет вестись инструментом #%d:\n\n'
        '   ширина трассы               : %5dmkm\n'
        '   диаметр переходных отверстий: %5dmkm\n'
        '   номер сверла                : %5d\n\n',
        Tool,
        Tools[Tool].Size*625 DIV 12,
        Tools[Tool].VSize*625 DIV 12,
        Tools[Tool].DSize);
  IF NOT Query('Вы согласны с выбором инструмента?') THEN HALT END;
  print('\n');
  IF Exception?(e) THEN CloseWindow; Show(Message); HALT END;
  mdl:=ReadModel(Name);
  WriteTime:=SystemClock();
  IF Flag?('n')OR Flag?('c') THEN
    Iterate(mdl^.All,Clear);
    WriteModel(mdl); WriteTime:=SystemClock();
    IF Flag?('c') THEN HALT END;
  END;
  IF Flag?('g') THEN
    OpenWindow(mdl,12,mdl^.ctX DIV 2,mdl^.ctY DIV 2);
    Wait(TimeSignal); Wait(TimeSignal);
    Iterate(mdl^.All,Bril);
    WriteModel(mdl); WriteTime:=SystemClock();
    HALT;
  END;
  TotalGang:=0; Iterate(mdl^.All,Calc);
  IF TotalGang=0 THEN HALT END;
  InitDWS(mdl,Tool);
  OpenWindow(mdl,12,mdl^.ctX DIV 2,mdl^.ctY * 3 DIV 4);
  CrsX:=mdl^.ctX DIV 2;
  CrsX:=mdl^.ctY DIV 2;
  Abort:=FALSE;
  WHILE (TotalGang>0)& NOT Abort DO
    Iterate(mdl^.All,LayoutSignal);
  END;
  WriteModel(mdl);
  RemoveDWS;
  CloseWindow;
END tl.
