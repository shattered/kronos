IMPLEMENTATION MODULE pedEditor;(* Sem 06-Mar-87. (c) KRONOS *)

FROM pedPbl    IMPORT   CursorX, CursorY, Layer, Fixed, Signal, ShowStat,
                        LayersNo;
FROM pedScreen IMPORT   WindowX, WindowY, Text, OpenWindow, Cursor,
                        SetWindow, WindowE, WindowS, WindowN, WindowW,
                        CloseWindow, Drow, Delete, ScaleX;
FROM pedTopology IMPORT InsertRange, DeleteRange, LineXs, LineYs,
                        LineXe, LineYe, check_on,
                        InsertVias, DeleteVias, FindSignal, Del;
FROM pedMacro  IMPORT   DefineMacro, InsertMacro, InsertMetalMacro, DeleteBox;
FROM pedTools  IMPORT   Tools;
IMPORT ModelMisc, cdsImp, cdsSeq;
FROM Strings   IMPORT   GetWord;
FROM Terminal  IMPORT   Read, Write, ClearLine, print, Home, Clear;
FROM Edit      IMPORT   ReadString;
FROM Model     IMPORT   Iterate, NewObject, Tie, Tag, String, Object, Objects,
                        Lget;
FROM Image     IMPORT   image, image0, PeekNum;
FROM Keyboard  IMPORT   PeekKey, ReadKey, up, dw, left, right, delc, insc,
                        gold, silver, bronze, insln, dwpg, uppg,
                        empty, home, end;
FROM pedWindow  IMPORT  Command, Scales, InChain, InMacro, Speed, Zoom,
                        LastX, LastY;

PROCEDURE Error(s: ARRAY OF CHAR);
BEGIN
  Text(0,s);
END Error;

VAR Insert : BOOLEAN;
    Buf: ARRAY [0..199] OF CHAR;
    mdl: Object;
    TreckTool: CARDINAL;
    PinTool: CARDINAL;
    Name: ARRAY [0..79] OF CHAR;

PROCEDURE LookUpSignal;
BEGIN
  IF InChain THEN RETURN END;
  Text(1,'...... Working ......');
  FindSignal(CursorX,CursorY,Layer,0, mdl,NIL);
  Signal:=ModelMisc.Signal;
END LookUpSignal;

PROCEDURE FindName1(o: Object);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name#Name  THEN RETURN END;
  Signal:=o;
END FindName1;

PROCEDURE FindName;
BEGIN
  IF InChain THEN RETURN END;
  Text(1,'...... Working ......');
  Signal:=NIL;
  Iterate(mdl^.All,FindName1);
END FindName;

PROCEDURE ShowRange;
  VAR nm: String;
BEGIN
  FindSignal(CursorX,CursorY,Layer,0,mdl,NIL);
  IF ModelMisc.Signal=NIL THEN
    image0(Buf,"Can't find any conductor at this point.");
  ELSE
    nm:=ModelMisc.Signal^.Name;
    image0(Buf,'%s, size %dmkm(%d)',nm,ModelMisc.Size*2*25000 DIV 960,
           ModelMisc.Size);
    IF ModelMisc.Fixed THEN
      image(Buf,', fixed');
    ELSE
      image(Buf,', variable');
    END;
    IF ModelMisc.ViasSize#0 THEN
      image(Buf,', drilling tool #%d',ModelMisc.ViasSize);
    END;
    image(Buf,'  (%d,%d)-(%d,%d)',ModelMisc.X1,ModelMisc.Y1,
                                  ModelMisc.X2,ModelMisc.Y2);
  END;
  Text(1,Buf);
END ShowRange;

PROCEDURE Rename;
  VAR l,x1,x2,y1,y2,sz,vsz: CARDINAL; fx: BOOLEAN; ls: BITSET; sg: Object;
BEGIN
  IF InChain THEN RETURN END;
  FindSignal(CursorX,CursorY,Layer,0,mdl,NIL);
  sg:=ModelMisc.Signal;
  IF sg#NIL THEN
    x1:=ModelMisc.X1; x2:=ModelMisc.X2;
    y1:=ModelMisc.Y1; y2:=ModelMisc.Y2;
    sz:=ModelMisc.Size; vsz:=ModelMisc.ViasSize;
    fx:=ModelMisc.Fixed; ls:=ModelMisc.Layer;
    CloseWindow; ClearLine;
    ReadString('Signal name: ',Name); FindName;
    OpenWindow(mdl,ScaleX,WindowX,WindowY);
    IF (Signal#NIL)&(sg#Signal) THEN
      LineXs:=x1; LineXe:=x2; LineYs:=y1; LineYe:=y2;
      FOR l:=0 TO LayersNo-1 DO
        IF l IN ls THEN DeleteRange(l,sg) END;
      END;
      FOR l:=0 TO LayersNo-1 DO
        IF (l IN ls)& InsertRange(sz,l,fx,mdl,Signal) THEN
          Error('Shot curcut.');
          IF InsertRange(sz,l,fx,mdl,sg) THEN END;
        END;
      END;
    END;
  END;
END Rename;

PROCEDURE PlacePin(o: Object);
  VAR l,PinX,PinY: CARDINAL; s: Object;
BEGIN
  IF Tag(o)#pin THEN RETURN END;
  ModelMisc.PinLocation(o,PinX,PinY);
  s:=o^.Signal;
  IF s=NIL THEN
    s:=NewObject(signal);
    o^.Signal:=s;
    Tie(s^.TiedPins,o);
    Tie(mdl^.All,s);
    image0(s^.Name,'%s"%d',o^.Chip^.Name,o^.No+1);
  END;
  LineXs:=PinX+Tools[PinTool].VDefX;
  LineYs:=PinY+Tools[PinTool].VDefY;
  LineXe:=LineXs;
  LineYe:=LineYs;
  DeleteVias(s);
  IF InsertVias(Tools[PinTool].VSize,Tools[PinTool].DSize,TRUE,mdl,s) THEN
  END;
END PlacePin;

PROCEDURE StartChain;
BEGIN
  IF Signal=NIL THEN Error('Absent signal name.'); RETURN END;
  LastX:=CursorX; LastY:=CursorY;
  InChain:=TRUE; Insert:=TRUE;
END StartChain;

PROCEDURE MoveChain;
BEGIN
  IF NOT InChain THEN RETURN END;
  Cursor(CursorX,CursorY,CursorX,CursorY);
  LineXs:=LastX+Tools[TreckTool].DefX;
  LineYs:=LastY+Tools[TreckTool].DefY;
  LineXe:=CursorX+Tools[TreckTool].DefX;
  LineYe:=CursorY+Tools[TreckTool].DefY;
  IF Insert THEN
    IF Tools[TreckTool].Size=0 THEN
      Error('Illegal tool.'); InChain:=FALSE; RETURN
    END;
    IF InsertRange(Tools[TreckTool].Size,Layer,Fixed,mdl,Signal) THEN
      Error('Short curcut.'); RETURN
    END;
  ELSE DeleteRange(Layer,Signal)
  END;
  LastX:=CursorX; LastY:=CursorY;
END MoveChain;

PROCEDURE FinishChain;
BEGIN
  IF Insert THEN MoveChain END;
  InChain:=FALSE;
END FinishChain;

PROCEDURE PlaceVias;
BEGIN
  IF NOT InChain THEN RETURN END;
  MoveChain;
  IF (LastX#CursorX)OR(LastY#CursorY) THEN RETURN END;
  LineXs:=LastX+Tools[TreckTool].VDefX;
  LineYs:=LastY+Tools[TreckTool].VDefY;
  LineXe:=LineXs;
  LineYe:=LineYs;
  IF Insert THEN
    IF Tools[TreckTool].VSize=0 THEN
      Error('Illegal tool.'); InChain:=FALSE; RETURN
    END;
    IF InsertVias
      (Tools[TreckTool].VSize,Tools[TreckTool].DSize,Fixed,mdl,Signal) THEN
      Error('Short curcut.'); RETURN
    END;
  ELSE DeleteVias(Signal)
  END;
  Layer:=1-Layer;
END PlaceVias;

PROCEDURE DeleteTrack;
BEGIN
  IF Signal=NIL THEN Error('Absent signal name.'); RETURN END;
  LastX:=CursorX; LastY:=CursorY;
  InChain:=TRUE; Insert:=FALSE;
END DeleteTrack;

PROCEDURE DeleteSignal;
  VAR Ch: CHAR;
BEGIN
  IF Signal=NIL THEN
    LookUpSignal;
    IF Signal=NIL THEN Error('Absent signal name.'); RETURN END;
  END;
  CloseWindow; ClearLine; print('%s - delete all segments?',Signal^.Name);
  Ch:=Read();
  IF (Ch#'y')&(Ch#'Y') THEN RETURN END;
  ModelMisc.StartConductor(Signal);
  WHILE NOT ModelMisc.Empty DO
    Del; ModelMisc.NextConductor;
  END;
END DeleteSignal;

PROCEDURE AskInt(s: ARRAY OF CHAR; VAR i: CARDINAL);
  VAR ln,ln1: ARRAY [0..79] OF CHAR; k,j: CARDINAL; Ch: CHAR;
BEGIN
  Write(15c); ClearLine;
  print('%s',s);
  image0(ln,' [ %d ] ',i);
  image0(ln1,'%d',i);
  ReadString(ln,ln1);
  j:=0; k:=0;
  WHILE ln1[j]=' ' DO INC(j) END;
  IF ln1[j]=0c THEN RETURN END;
  WHILE (ln1[j]>='0')&(ln1[j]<='9') DO
    k:=k*10+ORD(ln1[j])-ORD('0'); INC(j);
  END;
  IF ln1[j]#0c THEN
    print('\rInvalid number.');
    ClearLine;
    Ch:=Read(); RETURN
  END;
  i:=k;
END AskInt;

VAR ChipNm: String; ChipFnd: Object;

PROCEDURE FindChip(c: Object);
BEGIN
  IF Tag(c)#chip THEN RETURN END;
  IF c^.Name=ChipNm THEN ChipFnd:=c END;
END FindChip;

PROCEDURE FindPin;
  VAR ln,wd: ARRAY [0..79] OF CHAR; n: INTEGER; ch: CHAR;
BEGIN
  CloseWindow;
  ReadString('Chip name & pin no: ',ln); print('\n');
  GetWord(ln,ChipNm);
  IF ChipNm[0]#0c THEN
    ChipFnd:=NIL;
    Iterate(mdl^.All,FindChip);
    IF ChipFnd#NIL THEN
      GetWord(ln,wd);
      IF (PeekNum(wd,0,n)>0)&(n>=1)&(Lget(ChipFnd^.Pins,n-1)#NIL) THEN
        ModelMisc.PinLocation(Lget(ChipFnd^.Pins,n-1),CursorX,CursorY);
        WindowX:=CursorX; WindowY:=CursorY;
      ELSE
        print('Invalid pin no: %s; pres any key...',wd); ch:=Read();
      END;
    ELSE
      print('Chip %s not found; pres any key...',ChipNm); ch:=Read();
    END;
  END;
  OpenWindow(mdl,ScaleX,WindowX,WindowY);
END FindPin;

PROCEDURE Help;
BEGIN
  Home;
  print('стрелки и цифры 1..4,6..9 - движение курсора\n');
  print('gold + стрелка            - движение окна\n');
  print(' +                        - увеличение шага курсора\n');
  print(' -                        - уменьшение шага курсора\n');
  print(' *                        - установка курсора в ближайший узел сетки\n');
  print('page down                 - переход на слой 0\n');
  print('page up                   - переход на слой 1\n');
  print('insert                    - начало трассы\n');
  print('delete                    - начало удаления трассы\n');
  print('CR                        - точка поворота\n');
  print('ins line                  - переходное отверстие\n');
  print('LF                        - конец трассы\n');
  print(' f                        - поиск сигнала по курсору\n');
  print(' n                        - поиск сигнала по имени\n');
  print(' q                        - переименование сегмента\n');
  print(' w                        - вывод информации о сегменте\n');
  print(' r                        - перемещение курсора на заданный пин\n');
  print('**** для продолжения нажмите любую клавишу *****');
  IF Read()=0c THEN END; Home; Clear;
  print(' i             - вставление макроса\n');
  print(' o             - вставление макроса с переименованием\n');
  print(' d             - начало определения макроса\n');
  print(' s             - конец определения макроса\n');
  print(' [             - начало удаляемой области\n');
  print(' ]             - конец удаляемой области\n');
  print(' p             - создание площадок под пины\n');
  print(' t             - смена иструмента\n');
  print(' k             - удаление всей метализации тек. сигнала\n');
  print(' e             - выход в командный режим\n');
  print(' u             - включение/выключение fixed\n');
  print(' g             - поиск неподключенных пинов\n');

END Help;

PROCEDURE DigitizeTracks;
  VAR Ch: CHAR; i: CARDINAL; k: CHAR;
BEGIN
  Layer:=0; InChain:=FALSE; Signal:=NIL; InMacro:=FALSE;
  LOOP
    ShowStat;
    IF InChain OR InMacro THEN
      Cursor(LastX,LastY,CursorX,CursorY)
    ELSE
      Cursor(CursorX,CursorY,CursorX,CursorY);
    END;
    k:=Command();
    CASE k OF
      insc : IF InChain THEN FinishChain END;
             IF NOT InMacro THEN StartChain END;
     |delc : IF InChain THEN FinishChain END;
             IF NOT InMacro THEN DeleteTrack END;
     |insln: IF InChain THEN MoveChain; PlaceVias END;
    ELSE
      IF (k>=0c)&(k<=176c)OR(k>=240c)&(k<=377c) THEN
        Ch:=CHAR(k);
        CASE Ch OF
          15c: IF InChain THEN MoveChain END;
         |12c: IF InChain THEN FinishChain END;
         |'c': check_on:=NOT check_on;
         |'f': IF InChain THEN FinishChain END; LookUpSignal;
         |'q': Rename;
         |'w': ShowRange;
         |'r': FindPin;
         |'i': InsertMacro(CursorX,CursorY,mdl);
         |'o': InsertMetalMacro(CursorX,CursorY,mdl);
         |'d': IF InChain THEN FinishChain END;
               LastX:=CursorX; LastY:=CursorY; InMacro:=TRUE;
         |'s': IF InMacro THEN
                 InMacro:=FALSE; DefineMacro(mdl,LastX,LastY,CursorX,CursorY);
               END;
         |'[': IF InChain THEN FinishChain END;
               LastX:=CursorX; LastY:=CursorY; InMacro:=TRUE;
         |']': IF InMacro THEN
                 InMacro:=FALSE;
                 Text(1,'Delete all conductors in box?');
                 Ch:=Read();
                 IF Ch='y' THEN
                   DeleteBox(mdl,LastX,LastY,CursorX,CursorY);
                 END;
                 Text(1,'');
               END;
         |'n': IF InChain THEN FinishChain END;
               CloseWindow; ClearLine; ReadString('Signal name: ',Name);
               FindName;
               OpenWindow(mdl,ScaleX,WindowX,WindowY);
         |'p': IF Tools[PinTool].VSize=0 THEN
                 Error('Illegal tool.');
               ELSE
                 Iterate(mdl^.All,PlacePin);
               END;
         |'t': CloseWindow; AskInt('Tool for trecks',TreckTool);
               AskInt('Tool for pins',PinTool);
               OpenWindow(mdl,ScaleX,WindowX,WindowY);
         |'k': DeleteSignal;
         |'e': IF InChain THEN FinishChain END; RETURN;
         |'u': Fixed:=NOT Fixed;
         |'a': IF Signal#NIL THEN cdsImp.Bril(Signal) END;
         |'g': IF Signal#NIL THEN
                 cdsSeq.Next(Signal,CursorX,CursorY);
                 SetWindow(Scales[Zoom],CursorX,CursorY);
               END;
         |'h': CloseWindow;
               Help; Ch:=Read();
               OpenWindow(mdl,ScaleX,WindowX,WindowY);
        ELSE
        END;
      END;
    END;
  END;
END DigitizeTracks;

PROCEDURE Editor(o: Object);
BEGIN
  IF o=NIL THEN print('No model for edit.\n'); RETURN END;
  IF mdl#o THEN
    Zoom:=2;
    CursorX:=o^.ctX DIV 2; CursorY:=o^.ctY DIV 2;
    CursorX:=CursorX-CursorX MOD 48;
    CursorY:=CursorY-CursorY MOD 48;
  END;
  mdl:=o;
  OpenWindow(mdl,Scales[Zoom],CursorX,CursorY);
  DigitizeTracks;
  CloseWindow;
END Editor;

BEGIN
  mdl:=NIL;
  Speed:=48;
  Zoom:=2;
  Scales[0]:=48;
  Scales[1]:=24;
  Scales[2]:=12;
  Scales[3]:=6;
  Scales[4]:=3;
  Scales[5]:=1;
  TreckTool:=4;
  PinTool:=5;
  Fixed:=TRUE;
END pedEditor.
