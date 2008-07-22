IMPLEMENTATION MODULE pedEditor;(* Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
--IMPORT cdsImp, cdsSeq;
IMPORT pedTopology;
IMPORT pedCU;
IMPORT FROM pedModel;
--FROM pedModel   IMPORT  board, signal_arr, string, signal, pin, ctype, chip;
FROM pedPbl    IMPORT   CursorX, CursorY, Layer, Fixed, Signal, ShowStat,
                        LayersNo;
FROM pedScreen IMPORT   WindowX, WindowY, Text, OpenWindow, Cursor,
                        SetWindow, WindowE, WindowS, WindowN, WindowW,
                        CloseWindow, Drow, Delete, ScaleX;
FROM pedTopology IMPORT InsertRange, DeleteRange, LineXs, LineYs,
                        LineXe, LineYe,
                        InsertVias, DeleteVias, FindSignal, Del;
FROM pedMacro  IMPORT   DefineMacro, InsertMacro, InsertMetalMacro, DeleteBox;
FROM pedTools  IMPORT   Tools;
FROM Strings   IMPORT   GetWord;
FROM Terminal  IMPORT   Read, Write, ClearLine, print, Home, Clear;
FROM Edit      IMPORT   ReadString;
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
    mdl: board;
    TreckTool: CARDINAL;
    PinTool: CARDINAL;
    Name: string;

PROCEDURE LookUpSignal;
  VAR n: INTEGER;
BEGIN
  IF InChain THEN RETURN END;
  Signal:=pedCU.FindSignal(mdl,NIL,CursorX,CursorY,Layer,0,n);
END LookUpSignal;

PROCEDURE FindName;
  VAR i: INTEGER; sa: signal_arr;
BEGIN
  IF InChain THEN RETURN END;
  Signal:=NIL; sa:=mdl^.sigs;
  FOR i:=0 TO mdl^.sno-1 DO
    IF signal(sa^[i])^.name=Name THEN Signal:=signal(sa^[i]) END;
  END;
END FindName;

PROCEDURE ShowRange;
  VAR nm: string; s: signal; n: INTEGER; sg: pedCU.segment;
BEGIN
  s:=pedCU.FindSignal(mdl,NIL,CursorX,CursorY,Layer,0,n);
  IF s=NIL THEN
    image0(Buf,"Can't find any conductor at this point.");
  ELSE
    nm:=s^.name; pedCU.unpack(s,n,sg);
    image0(Buf,'%s, size %dmkm(%d)',nm,sg.size*2*25000 DIV 960,sg.size);
    IF sg.fix THEN image(Buf,', fixed') ELSE image(Buf,', variable') END;
    IF sg.pipe#0 THEN image(Buf,', pipe #%dmkm',sg.pipe*50000 DIV 960) END;
    image(Buf,'  (%d,%d)-(%d,%d)',sg.x1,sg.y1,sg.x2,sg.y2);
  END;
  Text(1,Buf);
END ShowRange;

PROCEDURE Rename;
  VAR l,x1,x2,y1,y2,sz,vsz: CARDINAL; fx: BOOLEAN; ls: BITSET; sg: signal;
BEGIN
  IF InChain THEN RETURN END;
  FindSignal(CursorX,CursorY,Layer,0,mdl,NIL);
  sg:=pedTopology.Signal;
  IF sg#NIL THEN
    x1:=pedTopology.X1; x2:=pedTopology.X2;
    y1:=pedTopology.Y1; y2:=pedTopology.Y2;
    sz:=pedTopology.Size; vsz:=pedTopology.ViasSize;
    fx:=pedTopology.Fixed; ls:=pedTopology.Layer;
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

PROCEDURE PinLocation(p: pin; VAR x,y: INTEGER);
  VAR t: ctype; x1,y1: CARDINAL;
BEGIN
  x:=0; y:=0;
  IF p=NIL THEN RETURN END;
  ASSERT(p^.chp#NIL);
  t:=p^.chp^.type;
  x1:=p^.chp^.x;
  y1:=p^.chp^.y;
  CASE p^.chp^.r MOD 4 OF
   |0: x:=x1+t^.pins[p^.no].x; y:=y1+t^.pins[p^.no].y;
   |1: x:=x1+t^.pins[p^.no].x; y:=y1-t^.pins[p^.no].y;
   |2: x:=x1-t^.pins[p^.no].x; y:=y1-t^.pins[p^.no].y;
   |3: x:=x1-t^.pins[p^.no].x; y:=y1+t^.pins[p^.no].y;
  END;
END PinLocation;

PROCEDURE PlacePin(o: pin);
  VAR l,x,y,sz,vsz: INTEGER; s: signal; ls: BITSET;
BEGIN
  PinLocation(o,x,y);
  s:=o^.sig;
  ASSERT(s#NIL);
  x:=x+Tools[PinTool].VDefX;
  y:=y+Tools[PinTool].VDefY;
  sz:=Tools[PinTool].VSize;
  vsz:=Tools[PinTool].DSize;
  ls:={0..1};
  pedCU.DeleteVias(s,x,y);
  IF pedCU.InsertViasF(mdl,s,x,y,sz,vsz,ls) THEN END;
END PlacePin;

PROCEDURE StartChain;
BEGIN
  IF Signal=NIL THEN Error('Absent signal name.'); RETURN END;
  LastX:=CursorX; LastY:=CursorY;
  InChain:=TRUE; Insert:=TRUE;
END StartChain;

PROCEDURE MoveChain;
  VAR rn: pedCU.range;
BEGIN
  IF NOT InChain THEN RETURN END;
  Cursor(CursorX,CursorY,CursorX,CursorY);
  rn.x1:=LastX+Tools[TreckTool].DefX;
  rn.y1:=LastY+Tools[TreckTool].DefY;
  rn.x2:=CursorX+Tools[TreckTool].DefX;
  rn.y2:=CursorY+Tools[TreckTool].DefY;
  IF Insert THEN
    IF Tools[TreckTool].Size=0 THEN
      Error('Illegal tool.'); InChain:=FALSE; RETURN
    END;
    IF pedCU.InsertRange
             (mdl,Signal,rn,Tools[TreckTool].Size,Layer,Fixed) THEN
      Error('Short curcut.'); RETURN
    END;
  ELSE
    pedCU.DeleteRange(Signal,rn,Layer)
  END;
  LastX:=CursorX; LastY:=CursorY;
END MoveChain;

PROCEDURE FinishChain;
BEGIN
  IF Insert THEN MoveChain END;
  InChain:=FALSE;
END FinishChain;

PROCEDURE PlaceVias;
  VAR x,y,sz,vsz: INTEGER; ls: BITSET;
BEGIN
  IF NOT InChain THEN RETURN END;
  MoveChain;
  IF (LastX#CursorX)OR(LastY#CursorY) THEN RETURN END;
  x:=LastX+Tools[TreckTool].VDefX;
  y:=LastY+Tools[TreckTool].VDefY;
  ls:={0..1};
  IF Insert THEN
    IF Tools[TreckTool].VSize=0 THEN
      Error('Illegal tool.'); InChain:=FALSE; RETURN
    END;
    sz:=Tools[TreckTool].VSize;
    vsz:=Tools[TreckTool].DSize;
    IF Fixed THEN
      IF pedCU.InsertViasF(mdl,Signal,x,y,sz,vsz,ls) THEN
        Error('Short curcut.'); RETURN
      END;
    ELSE
      IF pedCU.InsertViasV(mdl,Signal,x,y,sz,vsz,ls) THEN
        Error('Short curcut.'); RETURN
      END;
    END;
  ELSE
    pedCU.DeleteVias(Signal,x,y);
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
  CloseWindow; ClearLine; print('%s - delete all segments?',Signal^.name);
  Ch:=Read();
  IF (Ch#'y')&(Ch#'Y') THEN RETURN END;
  pedTopology.StartConductor(Signal);
  WHILE NOT pedTopology.Empty DO
    Del; pedTopology.NextConductor;
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

PROCEDURE FindPin;
  VAR ln,wd: ARRAY [0..79] OF CHAR; n,i: INTEGER; ch: CHAR; chp: chip;
      nm: string;
BEGIN
  CloseWindow;
  ReadString('Chip name & pin no: ',ln); print('\n');
  GetWord(ln,nm);
  IF nm#'' THEN
    FOR i:=0 TO mdl^.cno-1 DO
      IF chip(mdl^.chps^[i])^.name=nm THEN chp:=chip(mdl^.chps^[i]) END;
    END;
    IF chp#NIL THEN
      GetWord(ln,wd);
      IF (PeekNum(wd,0,n)>0)&(n>=1)&(n<=chp^.pno) THEN
        PinLocation(ADR(chp^.pins[n-1]),CursorX,CursorY);
        WindowX:=CursorX; WindowY:=CursorY;
      ELSE
        print('Invalid pin no: %s; pres any key...',wd); ch:=Read();
      END;
    ELSE
      print('Chip %s not found; pres any key...',nm); ch:=Read();
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
  VAR Ch: CHAR; i,j: CARDINAL; k: CHAR;
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
                 FOR i:=0 TO mdl^.cno-1 DO
                   FOR j:=0 TO mdl^.chps^[i]^.pno-1 DO
                     PlacePin(ADR(mdl^.chps^[i]^.pins[j]));
                   END;
                 END;
               END;
         |'t': CloseWindow; AskInt('Tool for trecks',TreckTool);
               AskInt('Tool for pins',PinTool);
               OpenWindow(mdl,ScaleX,WindowX,WindowY);
         |'k': DeleteSignal;
         |'e': IF InChain THEN FinishChain END; RETURN;
         |'u': Fixed:=NOT Fixed;
--       |'a': IF Signal#NIL THEN cdsImp.Bril(Signal) END;
--       |'g': IF Signal#NIL THEN
--               cdsSeq.Next(Signal,CursorX,CursorY);
--               SetWindow(Scales[Zoom],CursorX,CursorY);
--             END;
         |'h': CloseWindow;
               Help; Ch:=Read();
               OpenWindow(mdl,ScaleX,WindowX,WindowY);
        ELSE
        END;
      END;
    END;
  END;
END DigitizeTracks;

PROCEDURE Editor(o: board);
BEGIN
  IF o=NIL THEN print('No model for edit.\n'); RETURN END;
  mdl:=o;
  OpenWindow(mdl,Scales[Zoom],CursorX,CursorY);
  DigitizeTracks;
  CloseWindow;
END Editor;

BEGIN
  mdl:=NIL;
  Speed:=48;
  Zoom:=3;
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
