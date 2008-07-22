IMPLEMENTATION MODULE pedEditor;(*$U+ Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS;
FROM pedTopology IMPORT InsertRange, DeleteRange, LineXs, LineYs,
                        LineXe, LineYe, SeekInBox, Len,
                        InsertVias, DeleteVias, FindSignal, Del;
FROM pedChips   IMPORT  LookUpChip, LookUpPin, RenamePin, InsertAllPins,
                        KillChip, MoveChip, FindChip, UpdatePin, NewChip,
                        DeleteMetal;
FROM pedMacro  IMPORT   DefineMacro, InsertMetalMacro, DeleteBox, InsertMacro,
                        DefineChipMacro, DoChipMacro;
FROM pedTools  IMPORT   Tools;
IMPORT ModelMisc, cdsImp, cdsSeq;
FROM ModelMisc  IMPORT  X1, X2, Y1, Y2;
FROM pedEdit   IMPORT   ReadString;
FROM Model     IMPORT   Iterate, NewObject, Tie, String, Object, Objects,
                        Lget, RemoveModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, KillReaction, Message,
                        IOerror, RaiseInMe, MemoryOverflow;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  io : ModelIO;
IMPORT  scr: pedScreen;
IMPORT  pct: pedChipType;
IMPORT  m  : pedMouse;
IMPORT  mcd: defCodes;
IMPORT  win: Windows;
IMPORT  VG;
IMPORT  vg : pedVG;
IMPORT  men: Menu;
IMPORT  mem: cdsHeap;

WITH STORAGE (NEW    : mem.Allocate;
              DISPOSE: mem.Deallocate;
              RESIZE : mem.Reallocate);

TYPE window=win.window;

TYPE EdtContext=POINTER TO RECORD
       InChain,InMacro: BOOLEAN;
       Speed, Zoom    : INTEGER;
       LastX,LastY    : INTEGER;
       Insert         : BOOLEAN;
       mode           : INTEGER;
       LastChip  : Object;
       CurPin    : Object;
       TreckTool : INTEGER;
       PinTool   : INTEGER;
       EditMode  : INTEGER;
       InDelBox  : BOOLEAN;
       show?     : INTEGER;
       chip_r    : INTEGER;
       CursorXm   , CursorYm    : INTEGER;
       OldCursorX , OldCursorY  : INTEGER;
     END;

CONST del    =  0c; move   =  1c; plus   =  2c; minus  =  3c; up     =  4c;
      down   =  5c; right  =  6c; left   =  7c; resize = 14c; fill   = 15c;
      ins    = 16c; delete = 17c; quit   = 20c;
      StartEdit = 0; StartMacro= 1; InsMacro  = 2; OvlMacro  = 3;
      DelBox    = 4; move_chip = 5; DelSg     = 6; dgt_trck  = 0;
      chp_plsm  = 1; cht_edit  = 2;

VAR Sheets    : DYNARR OF Sheet;
    Scales    : ARRAY [0..5] OF INTEGER;
    Name      : ARRAY [0..15] OF CHAR;
    EPinNo    : INTEGER;
    steps     : window;
    tools     : window;
    modes     : window;
    show_men  : window;
    edit_men  : window;
    general   : window;
    show_dgt_men: window;
    show_cht_men: window;
    edit_dgt_men: window;
    edit_cht_men: window;
    edit_chp_men: window;
    gner_dgt_men: window;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN x ELSE RETURN y END;
END Max;

PROCEDURE ShowPos(sht: Sheet);
  VAR xh,xl,yh,yl: INTEGER;
BEGIN
  WITH sht^ DO
    WITH PublicContext^ DO
      xh:=(ABS(CursorX)*625) DIV 24; xl:=xh MOD 1000; xh:=xh DIV 1000;
      yh:=(ABS(CursorY)*625) DIV 24; yl:=yh MOD 1000; yh:=yh DIV 1000;
      IF wnd^.lock.cou>0 THEN RETURN END;
      win.lock_window(wnd);
      vg.mode(wnd,vg.rep); vg.color(wnd,2);
      vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
               83+vg.char_w DIV 2,wnd^.S-vg.char_h-2,
               "%3d.%$3d %3d.%$3d (%5d %5d)",
                 xh,  xl, yh,  yl,CursorX,CursorY);
      win.release_window(wnd);
      win.ref_box(wnd,wnd^.sy-1-wnd^.S,wnd^.S);
    END;
  END;
END ShowPos;

PROCEDURE ShowStat(sht: Sheet);
BEGIN
  ShowPos(sht);
  WITH sht^.PublicContext^ DO
    WITH sht^ DO
      win.lock_window(wnd);
      vg.mode(wnd,vg.rep);
      vg.color(wnd,2);
      IF    sht^.EditorContext^.mode=dgt_trck THEN
        vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                      vg.sign_w+4+vg.char_w*10+1,wnd^.sy-1-3-vg.char_h*2,
                      ' Signal name');
        IF Signal#NIL THEN
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*23+1,wnd^.sy-1-3-vg.char_h*2,
                        '%17-s',Signal^.Name)
        ELSE
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*23+1,wnd^.sy-1-3-vg.char_h*2,
                        '%17-s','...')
        END;
        IF Fixed THEN
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*40+2,wnd^.sy-1-3-vg.char_h*2,
                        '  fixed  ')
        ELSE
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*40+2,wnd^.sy-1-3-vg.char_h*2,
                        ' variable')
        END;
      ELSIF sht^.EditorContext^.mode=chp_plsm THEN
        vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                      vg.sign_w+4+vg.char_w*10+1,wnd^.sy-1-3-vg.char_h*2,
                      '  Chip name ');
        IF Chip#NIL THEN
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*23+1,wnd^.sy-1-3-vg.char_h*2,
                        '%17-s',Chip^.Name);
          vg.print(wnd,vg.sign_w+4,wnd^.sy-vg.char_h*2-4,
                   ' rotate %d ',sht^.EditorContext^.chip_r MOD 4);
        ELSE
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*23+1,wnd^.sy-1-3-vg.char_h*2,
                        '%17-s','...');
          vg.print(wnd,vg.sign_w+4,wnd^.sy-vg.char_h*2-4,
                   '          ');
        END;
      ELSIF sht^.EditorContext^.mode=cht_edit THEN
        vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                      vg.sign_w+4+vg.char_w*10+1,wnd^.sy-1-3-vg.char_h*2,
                      'External pin');
        IF Signal#NIL THEN
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*23+1,wnd^.sy-1-3-vg.char_h*2,
                        '%17-d',Signal^.EPinNo)
        ELSE
          vg.clip_print(wnd,wnd^.W,wnd^.dsx-wnd^.E,0,wnd^.sy-1,
                        vg.sign_w+4+vg.char_w*23+1,wnd^.sy-1-3-vg.char_h*2,
                        '%17-s','...')
        END;
      END;
      IF (sht^.EditorContext^.mode=dgt_trck) OR
         (sht^.EditorContext^.mode=cht_edit) THEN
        vg.inverse(wnd,vg.on);
        IF Layer=0 THEN
          vg.color(wnd,1);
          vg.print(wnd,vg.sign_w+4,wnd^.sy-vg.char_h*2-4,'  solder  ');
          vg.color(wnd,2); vg.inverse(wnd,vg.off);
          vg.print(wnd,vg.sign_w+4,wnd^.sy-vg.char_h*2-4,'          ');
        ELSE
          vg.color(wnd,2);
          vg.print(wnd,vg.sign_w+4,wnd^.sy-vg.char_h*2-4,' component');
          vg.color(wnd,1); vg.inverse(wnd,vg.off);
          vg.print(wnd,vg.sign_w+4,wnd^.sy-vg.char_h*2-4,'          ');
        END;
      END;
      win.ref_box(wnd,vg.char_h+4,vg.char_h+1);
      vg.inverse(wnd,vg.off);
      win.release_window(wnd);
    END;
  END;
END ShowStat;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE mcd.move END MOVE;

PROCEDURE setm(m: BITSET); CODE mcd.setm END setm;
PROCEDURE getm(): BITSET ; CODE mcd.getm END getm;

PROCEDURE Error(s: ARRAY OF CHAR; sht: Sheet);
BEGIN scr.Text(sht,0,'%s',s) END Error;

PROCEDURE LookUpSignal(sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF InChain THEN RETURN END;
      scr.Text(sht,0,'...... Working ......');
      FindSignal(CursorX,CursorY,Layer,0,sht,NIL);
      Signal:=ModelMisc.Signal;
    END;
  END;
  scr.CursorOff(sht); scr.UpdSig(sht);
  scr.Text(sht,0,''); ShowStat(sht);
END LookUpSignal;

PROCEDURE FindName1(o: Object; sht: Sheet);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name#Name  THEN RETURN END;
  sht^.PublicContext^.Signal:=o;
END FindName1;

PROCEDURE FindName(sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF InChain THEN RETURN END;
      scr.Text(sht,0,'...... Working ......');
      Signal:=NIL;
      Iterate(sht^.mdl^.All,FindName1,sht);
      IF Name[0]#0c THEN
        IF Signal=NIL THEN
          IF men.qwest(4,150,' Create new signal ?  ') THEN
            Signal:=NewObject(signal);
            Signal^.Name:=Name; Tie(sht^.mdl^.All,Signal);
          END;
        END;
      END;
    END;
  END;
  ShowStat(sht); scr.UpdSig(sht); scr.Text(sht,0,'');
END FindName;

PROCEDURE FindEpinNo1(o: Object; sht: Sheet);
BEGIN
  IF Tag(o)#externalpin THEN RETURN END;
  IF o^.EPinNo#EPinNo THEN RETURN END;
  sht^.PublicContext^.Signal:=o;
END FindEpinNo1;

PROCEDURE FindEpinNo(sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF InChain THEN RETURN END;
      scr.Text(sht,0,'...... Working ......');
      Signal:=NIL;
      Iterate(sht^.mdl^.ExternalPins,FindEpinNo1,sht);
      IF Signal=NIL THEN
        IF men.qwest(4,150,' Create new external pin ?  ') THEN
          Signal:=NewObject(externalpin);
          Signal^.EPinNo:=EPinNo;
          Tie(sht^.mdl^.ExternalPins,Signal);
        END;
      END;
    END;
  END;
  ShowStat(sht); scr.UpdSig(sht); scr.Text(sht,0,'');
END FindEpinNo;

PROCEDURE ShowRange(sht: Sheet);
  VAR nm: String; Buf: ARRAY [0..79] OF CHAR;
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      FindSignal(CursorXm,CursorYm,Layer,0,sht,NIL);
    END;
  END;
  IF ModelMisc.Signal=NIL THEN
    Error("Can't find any conductor at this point.",sht);
    scr.Text(sht,1,'');
  ELSE
    Error("",sht);
    nm:=ModelMisc.Signal^.Name;
    str.print(Buf,'%s, size %dmkm(%d)',nm,ModelMisc.Size*2*25000 DIV 960,
          ModelMisc.Size);
    IF ModelMisc.Fixed THEN
      str.append(Buf,', fixed')
    ELSE
      str.append(Buf,', variable')
    END;
    IF ModelMisc.ViasSize#0 THEN
      str.append(Buf,', drilling tool #%d',ModelMisc.ViasSize);
    END;
    str.append(Buf,'  (%d,%d)-(%d,%d)',ModelMisc.X1,ModelMisc.Y1,
                                   ModelMisc.X2,ModelMisc.Y2);
    scr.Text(sht,1,'%s',Buf);
  END;
END ShowRange;

VAR CurLayer: BITSET;
    sig     : Object;
    delX1, delX2, delY1, delY2, delSize: INTEGER;
    delLayer: BITSET;

PROCEDURE DelSeg(): BOOLEAN;
BEGIN
  IF (ModelMisc.Layer*CurLayer#{})&
     (Len(X1,Y1,X2,Y2,LineXs,LineYs)<=ModelMisc.Size*ModelMisc.Size) THEN
    delX1:=X1; delX2:=X2; delY1:=Y1; delY2:=Y2; delSize:=ModelMisc.Size;
    delLayer:=ModelMisc.Layer;
    sig  :=ModelMisc.Signal;
    IF (X1=X2)&(Y1=Y2) THEN RETURN TRUE END;
  END;
  RETURN FALSE;
END DelSeg;

PROCEDURE DelSegment(sht: Sheet);
BEGIN
  LineXs:=sht^.PublicContext^.CursorX; LineXe:=LineXs;
  LineYs:=sht^.PublicContext^.CursorY; LineYe:=LineYs;
  CurLayer:={}; INCL(CurLayer,sht^.PublicContext^.Layer);
  delSize:=-1;
  SeekInBox(sht,DelSeg);
  IF delSize>=0 THEN
    ModelMisc.StartConductor(sig,sht^.PublicContext^.ExtPin);
    WHILE NOT ModelMisc.Empty DO
      IF (delX1=X1)&(delX2=X2)&(delY1=Y1)&(delY2=Y2)&
         (delSize=ModelMisc.Size)&(delLayer=ModelMisc.Layer) THEN
        IF (X1=X2)&(Y1=Y2) THEN
          sht^.PublicContext^.Layer:=(sht^.PublicContext^.Layer+1) MOD 2;
          ShowStat(sht);
        END;
        Del(sht)
      END;
      ModelMisc.NextConductor;
    END;
  END;
END DelSegment;

(*
PROCEDURE Rename;
  VAR l,x1,x2,y1,y2,sz,vsz: INTEGER; fx: BOOLEAN; ls: BITSET; sg: Object;
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
    ReadString('Signal name: ',Name);
    T.print('\r'); T.ClearLine;
    FindName;
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
*)

PROCEDURE StartChain(sht: Sheet);
  VAR LastSignal: Object;
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      LastSignal:=Signal;
      LookUpSignal(sht);
      IF Signal=NIL THEN
        Signal:=LastSignal;
        scr.CursorOff(sht); scr.UpdSig(sht);
        scr.Text(sht,0,''); ShowStat(sht);
      END;
      IF (Signal=NIL) OR (Signal#LastSignal) THEN RETURN END;
      LastX:=CursorX; LastY:=CursorY;
      InChain:=TRUE;
      IF Insert THEN scr.Text(sht,2,'INSERT            ')
      ELSE           scr.Text(sht,2,'DELETE            ') END;
    END;
  END;
END StartChain;

PROCEDURE PlaceVias(sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF NOT InChain THEN RETURN END;
      IF (LastX#CursorX)OR(LastY#CursorY) THEN RETURN END;
      LineXs:=LastX+Tools[TreckTool].VDefX;
      LineYs:=LastY+Tools[TreckTool].VDefY;
      LineXe:=LineXs;
      LineYe:=LineYs;
      IF Insert THEN
        IF Tools[TreckTool].VSize=0 THEN
          Error('Illegal tool.',sht); InChain:=FALSE; RETURN
        END;
        IF InsertVias
          (Tools[TreckTool].VSize,Tools[TreckTool].DSize,Fixed,
           sht,Signal,TRUE) THEN
          Error('short curcut.',sht); RETURN
        END;
      ELSE DeleteVias(Signal,sht)
      END;
      Layer:=1-Layer;
    END;
  END;
END PlaceVias;

PROCEDURE MoveChain(sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF NOT InChain THEN RETURN END;
      IF (CursorX=LastX) & (CursorY=LastY) THEN PlaceVias(sht); RETURN  END;
      LineXs:=LastX+Tools[TreckTool].DefX;
      LineYs:=LastY+Tools[TreckTool].DefY;
      LineXe:=CursorX+Tools[TreckTool].DefX;
      LineYe:=CursorY+Tools[TreckTool].DefY;
      IF Insert THEN
        IF Tools[TreckTool].Size=0 THEN
          Error('Illegal tool.',sht); InChain:=FALSE; RETURN
        END;
        IF InsertRange(Tools[TreckTool].Size,Layer,Fixed,sht,Signal,TRUE) THEN
          Error('short curcut.',sht); RETURN
        END;
      ELSE DeleteRange(Layer,Signal,sht)
      END;
      LastX:=CursorX; LastY:=CursorY;
    END;
  END;
END MoveChain;

PROCEDURE FinishChain(sht: Sheet);
BEGIN
  sht^.EditorContext^.InChain:=FALSE; scr.Text(sht,2,'       ')
END FinishChain;

PROCEDURE DeleteSignal(sht: Sheet);
  VAR Ch: CHAR;
BEGIN
  WITH sht^.PublicContext^ DO
    IF Signal=NIL THEN
      LookUpSignal(sht);
      IF Signal=NIL THEN Error('Absent signal name.',sht); RETURN END;
    END;
    scr.CloseWindow(sht);
    tty.erase_line(0); tty.print('%s - delete all segments?',Signal^.Name);
    Ch:=m.Read();
    IF (Ch#'y')&(Ch#'Y') THEN RETURN END;
    ModelMisc.StartConductor(Signal,sht^.PublicContext^.ExtPin);
  END;
  WHILE NOT ModelMisc.Empty DO
    Del(sht); ModelMisc.NextConductor;
  END;
END DeleteSignal;

PROCEDURE Help;
BEGIN
  tty.home; tty.erase(0);
  tty.print('стрелки и цифры 1..4,6..9 - движение курсора\n');
  tty.print('gold + стрелка            - движение окна\n');
  tty.print(' +                        - увеличение шага курсора\n');
  tty.print(' -                        - уменьшение шага курсора\n');
  tty.print(' *                        - установка курсора в ближайший узел сетки\n');
  tty.print('page down                 - переход на слой 0\n');
  tty.print('page up                   - переход на слой 1\n');
  tty.print('insert                    - начало трассы\n');
  tty.print('delete                    - начало удаления трассы\n');
  tty.print('CR                        - точка поворота\n');
  tty.print('ins line                  - переходное отверстие\n');
  tty.print('LF                        - конец трассы\n');
  tty.print(' f                        - поиск сигнала по курсору\n');
  tty.print(' n                        - поиск сигнала по имени\n');
  tty.print(' q                        - переименование сегмента\n');
  tty.print(' w                        - вывод информации о сегменте\n');
  tty.print(' r                        - перемещение курсора на заданный пин\n');
  tty.print('**** для продолжения нажмите любую клавишу *****');
  IF m.Read()=0c THEN END; tty.home; tty.erase(0);
  tty.print(' i             - вставление макроса\n');
  tty.print(' o             - вставление макроса с переименованием\n');
  tty.print(' d             - начало определения макроса\n');
  tty.print(' s             - конец определения макроса\n');
  tty.print(' [             - начало удаляемой области\n');
  tty.print(' ]             - конец удаляемой области\n');
  tty.print(' p             - создание площадок под пины\n');
  tty.print(' t             - смена иструмента\n');
  tty.print(' k             - удаление всей метализации тек. сигнала\n');
  tty.print(' e             - выход в командный режим\n');
  tty.print(' u             - включение/выключение fixed\n');
  tty.print(' g             - поиск неподключенных пинов\n');

END Help;

CONST marginX=64;
      marginY=30;

PROCEDURE Correct(sht: Sheet);
BEGIN
  WITH sht^.PublicContext^ DO
    CursorX:=(CursorX+24) DIV 48 * 48;
    CursorY:=(CursorY+24) DIV 48 * 48;
  END;
END Correct;

PROCEDURE recalc(x,y: INTEGER; sht: Sheet);
  VAR s,siz: INTEGER;
BEGIN
  WITH sht^ DO
    siz:=wnd^.dsx-(wnd^.W+wnd^.E);
    x:=x-wnd^.W;
    y:=y-wnd^.S;
  END;
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF (WindowE-WindowW)<(siz DIV 2) THEN
        s:=siz DIV (WindowE-WindowW);
        CursorXm:=x DIV s + WindowW; CursorX:=CursorXm;
        CursorYm:=y DIV s + WindowS; CursorY:=CursorYm;
      ELSE
        s:=(WindowE-WindowW+siz-1) DIV siz;
        CursorXm:=x*s+WindowW; CursorX:=CursorXm;
        CursorYm:=y*s+WindowS; CursorY:=CursorYm;
      END;
    END;
  END;
END recalc;

VAR shts: Sheet;

PROCEDURE DelChips(sht: Sheet; c: Object; x,y: INTEGER);
BEGIN
  IF c=sht^.PublicContext^.Chip THEN
    scr.Chip(sht,c,c^.XB,c^.YB,c^.RB,5);
  END;
  scr.Chip(sht,c,c^.XB,c^.YB,c^.RB,1);
  IF DeleteMetal(sht,c) THEN END;
END DelChips;

PROCEDURE MoveChips(sht: Sheet; c: Object; x,y: INTEGER);
BEGIN
  IF MoveChip(sht,c,c^.XB+x,c^.YB+y,c^.RB) THEN END;
  scr.Chip(sht,c,c^.XB,c^.YB,c^.RB,0);
  IF c=sht^.PublicContext^.Chip THEN
    scr.Chip(sht,c,c^.XB,c^.YB,c^.RB,4);
  END;
END MoveChips;

PROCEDURE DigitizeTracks(x,y: INTEGER; sht: Sheet);
  VAR  i,j: INTEGER; k,Ch: CHAR; Sp2: INTEGER;
BEGIN
  k:=0c;
  win.read_point(3,sht^.wnd,x,y,Ch);
  LOOP
    ShowStat(sht);
    WITH sht^.EditorContext^ DO
      WITH sht^.PublicContext^ DO
        LOOP
          i:=ScaleX*marginX; j:=ScaleY*marginY;
          x:=x-sht^.wnd^.x*32;
          y:=y-sht^.wnd^.y;
          y:=sht^.wnd^.sy-y-1;
          IF (win.search()#sht^.wnd) OR
             (x<sht^.wnd^.W)OR(y<sht^.wnd^.S) OR
             (x>=(sht^.wnd^.dsx-sht^.wnd^.E)) OR
             (y>=(sht^.wnd^.sy-sht^.wnd^.N)) THEN
            scr.CursorOff(sht);
            RETURN;
          END;
          recalc(x,y,sht);
          Sp2:=Speed DIV 2;
          scr.Cursord(sht,CursorXm,CursorYm,CursorXm,CursorYm);
          CursorX:=CursorXm+Sp2 - (CursorXm+Sp2) MOD Speed;
          CursorY:=CursorYm+Sp2 - (CursorYm+Sp2) MOD Speed;
          IF CursorX<WindowW THEN
            CursorX:=WindowW; CursorXm:=CursorX;
            scr.Cursord(sht,CursorXm,CursorYm,CursorXm,CursorYm);
          END;
          IF CursorX>WindowE THEN
            CursorX:=WindowE; CursorXm:=CursorX;
            scr.Cursord(sht,CursorXm,CursorYm,CursorXm,CursorYm);
          END;
          IF CursorY<WindowS THEN
            CursorY:=WindowS; CursorYm:=CursorY;
            scr.Cursord(sht,CursorXm,CursorYm,CursorXm,CursorYm);
          END;
          IF CursorY>WindowN THEN
            CursorY:=WindowN; CursorYm:=CursorY;
            scr.Cursord(sht,CursorXm,CursorYm,CursorXm,CursorYm);
          END;
          IF (CursorX  # OldCursorX )OR(CursorY  # OldCursorY ) THEN
            ShowPos(sht);
            OldCursorX :=CursorX ; OldCursorY :=CursorY;
          END;
          IF InChain OR InMacro THEN
            scr.Cursor(sht,LastX,LastY,CursorX,CursorY)
          ELSE
            scr.Cursor(sht,CursorX,CursorY,CursorX,CursorY);
          END;
          IF EditMode=move_chip THEN
            scr.Chip(sht,Chip,CursorX,CursorY,chip_r,2)
          END;
          win.read_point(3,sht^.wnd,x,y,k);
          IF k#0c THEN EXIT END;
        END;
        CASE k OF
         |15c,266c: CASE EditMode OF
                       StartEdit:
                         StartChain(sht);
                         IF InChain THEN EditMode:=-1 END;
                      |StartMacro:
                         LastX:=CursorX; LastY:=CursorY; InMacro:=TRUE;
                         InDelBox:=FALSE; EditMode:=-1;
                      |InsMacro:
                         scr.CursorOff(sht);
                         IF    sht^.EditorContext^.mode=dgt_trck THEN
                           InsertMacro(CursorX,CursorY,sht,shts);
                         ELSIF sht^.EditorContext^.mode=chp_plsm THEN
                           DoChipMacro(sht,
                                       CursorX-LastX,CursorY-LastY,DelChips);
                           DoChipMacro(sht,
                                       CursorX-LastX,CursorY-LastY,MoveChips);
                         END;
                         EditMode:=-1; RETURN;
                      |OvlMacro:
                         scr.CursorOff(sht);
                         InsertMetalMacro(CursorX,CursorY,sht,shts);
                         EditMode:=-1; RETURN;
                      |DelBox:
                         LastX:=CursorX; LastY:=CursorY; InMacro:=TRUE;
                         InDelBox:=TRUE; EditMode:=-1;
                      |DelSg : DelSegment(sht);
                      |move_chip:
                        scr.CursorOff(sht);
                        IF Chip^.RB>=0 THEN
                          scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,5);
                          scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,1);
                        END;
                        scr.Chip(sht,Chip,CursorX,CursorY,chip_r,0);
                        scr.Chip(sht,Chip,CursorX,CursorY,chip_r,4);
                        IF MoveChip(sht,Chip,CursorX,CursorY,chip_r)
                        THEN END;
                        EditMode:=-1; RETURN
                    ELSE
                      IF InMacro THEN
                        IF InDelBox THEN
                          InMacro:=FALSE;
                          InDelBox:=FALSE;
                          scr.CursorOff(sht);
                          IF men.qwest(5,100,'Delete all conductors in box?') THEN
                            DeleteBox(sht,LastX,LastY,CursorX,CursorY);
                          END;
                          scr.Text(sht,1,'');
                        ELSE
                          InMacro:=FALSE;
                          IF    sht^.EditorContext^.mode=dgt_trck THEN
                            DefineMacro(sht,LastX,LastY,CursorX,CursorY);
                          ELSIF sht^.EditorContext^.mode=chp_plsm THEN
                            DefineChipMacro(sht,LastX,LastY,CursorX,CursorY);
                          END;
                        END;
                        scr.CursorOff(sht); RETURN;
                      ELSIF InChain THEN
                        MoveChain(sht)
                      END;
                    END;
         |12c,33c:
                 FinishChain(sht); InMacro:=FALSE; InDelBox:=FALSE;
                 EditMode:=-1;
                 scr.CursorOff(sht);
                 RETURN
         |264c: IF mode#chp_plsm THEN
                  InChain:=FALSE; EditMode:=StartEdit; Insert:=NOT Insert;
                  IF Insert THEN  scr.Text(sht,2,'Search for INSERT')
                  ELSE            scr.Text(sht,2,'Search for DELETE') END;
                END;
         |'q': --Rename;
         |'m': --Iterate(mdl^.All,InsPin,0);
         |'k': DeleteSignal(sht);
         |'a': IF Signal#NIL THEN cdsImp.Bril(Signal,sht) END;

         |'h': scr.CloseWindow(sht);
               Help; Ch:=m.Read();
               scr.OpenWindow(sht,ScaleX,WindowX,WindowY);
        ELSE
        END;
      END;
    END;
  END;
END DigitizeTracks;

PROCEDURE ini(map: window);
  VAR x,y: INTEGER;
BEGIN
  x:=map^.sx*32-1; y:=map^.sy-1;
  win.lock_window(map);
  vg.mode(map,vg.rep); vg.color(map,4); vg.inverse(map,vg.off);
  vg.vect(map,0,y,x,y); vg.vect(map,x,y,x,0);
  vg.vect(map,x,0,0,0); vg.vect(map,0,0,0,y);
  DEC(x,2); DEC(y,2);
  vg.mode(map,vg.rep); vg.color(map,4);
  vg.vect(map,2,y,x,y); vg.vect(map,x,y,x,2);
  vg.vect(map,x,2,2,2); vg.vect(map,2,2,2,y);
  y:=map^.sy-14;
  vg.color(map,4); vg.mode(map,vg.rep);
  vg.box(map,2,2,map^.dsx-1-2,2+Max(vg.sign_h,vg.char_h)+1);
  vg.sign(map,3,3,move);
  vg.sign(map,map^.sx*32-vg.sign_w-3,3,resize);
  win.release_window(map);
END ini;

PROCEDURE ini_main(sht: Sheet);
  VAR m0, m1: INTEGER;
      x0,y0,x1,y1: INTEGER;
BEGIN
  WITH sht^ DO
    ini(wnd);
    win.lock_window(wnd);
    vg.color(wnd,4); vg.mode(wnd,vg.rep);
    vg.box(wnd,2,2+vg.sign_h+1,2+vg.sign_w+1,wnd^.sy-1-2);
    vg.sign(wnd,23,3,del);
    vg.sign(wnd,3,wnd^.sy-vg.sign_h-23,up);
    vg.sign(wnd,3,wnd^.sy-vg.sign_h-43,down);
    vg.sign(wnd,3,wnd^.sy-vg.sign_h-63,right);
    vg.sign(wnd,3,wnd^.sy-vg.sign_h-83,left);
    vg.sign(wnd,3,wnd^.sy-vg.sign_h-103,quit);
    IF sht^.EditorContext^.mode#chp_plsm THEN
      vg.sign(wnd,3,wnd^.sy-vg.sign_h-123,ins);
      vg.sign(wnd,3,wnd^.sy-vg.sign_h-143,delete);
    END;
    vg.box(wnd,wnd^.dsx-1-2,2+vg.sign_h+1,
                wnd^.dsx-1-(2+vg.sign_w+1),wnd^.sy-1-2);
    vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-23,up);
    vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-43,down);
    vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-63,right);
    vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-83,left);
    vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-103,quit);
    IF sht^.EditorContext^.mode#chp_plsm THEN
      vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-123,ins);
      vg.sign(wnd,wnd^.dsx-3-vg.sign_w,wnd^.sy-vg.sign_h-143,delete);
    END;
    IF sht^.EditorContext^.Zoom#HIGH(Scales) THEN
      vg.sign(wnd,43,3,plus)
    END;
    IF sht^.EditorContext^.Zoom#0 THEN
      vg.sign(wnd,63,3,minus)
    END;
    vg.box (wnd,2,wnd^.sy-1-2,wnd^.dsx-1-2,wnd^.sy-1-(2+vg.char_h+1));
    vg.vect(wnd,4+vg.sign_w,         wnd^.sy-vg.char_h*2-5,
                wnd^.dsx-4-vg.sign_w,wnd^.sy-vg.char_h*2-5);
    vg.print(wnd,23,wnd^.sy-1-(vg.char_h+2),'Mode');
    vg.print(wnd,23+5*vg.char_w,wnd^.sy-1-(vg.char_h+2),'Step');
    vg.print(wnd,23+10*vg.char_w,wnd^.sy-1-(vg.char_h+2),'Show');
    vg.print(wnd,23+15*vg.char_w,wnd^.sy-1-(vg.char_h+2),'Edit');
    vg.print(wnd,23+20*vg.char_w,wnd^.sy-1-(vg.char_h+2),'Tool');
    vg.print(wnd,23+25*vg.char_w,wnd^.sy-1-(vg.char_h+2),'General');
    vg.vect(wnd,vg.sign_w+4+vg.char_w*10,wnd^.sy-vg.char_h*2-5,
                vg.sign_w+4+vg.char_w*10,wnd^.sy-vg.char_h  -5);
    vg.vect(wnd,vg.sign_w+4+vg.char_w*40+1,wnd^.sy-vg.char_h*2-5,
                vg.sign_w+4+vg.char_w*40+1,wnd^.sy-vg.char_h  -5);
    vg.color(wnd,5);
    CASE sht^.EditorContext^.mode OF
      |dgt_trck: vg.print(wnd,23+33*vg.char_w,wnd^.sy-1-(vg.char_h+2),
                          ' Digitize Tracks ');
                 vg.color(wnd,4);
                 vg.vect(wnd,vg.sign_w+4+vg.char_w*49+2,wnd^.sy-vg.char_h*2-5,
                             vg.sign_w+4+vg.char_w*49+2,wnd^.sy-vg.char_h  -5);
                 vg.color(wnd,5);
      |chp_plsm: vg.print(wnd,23+33*vg.char_w,wnd^.sy-1-(vg.char_h+2),
                          ' Chips Placement ');
      |cht_edit: vg.print(wnd,23+33*vg.char_w,wnd^.sy-1-(vg.char_h+2),
                          ' Chip type editor');
    END;
    IF sht^.PublicContext^.check_on THEN
      vg.print(wnd,23+51*vg.char_w,wnd^.sy-1-(vg.char_h+2),'on ');
    ELSE
      vg.print(wnd,23+51*vg.char_w,wnd^.sy-1-(vg.char_h+2),'off');
    END;
    wnd^.W:=vg.sign_w+4;             wnd^.E:=vg.sign_w+4;
    wnd^.N:=vg.char_h+4+vg.char_h+1; wnd^.S:=Max(vg.sign_h,vg.char_h)+4;
    x0:=83;              y0:=sht^.wnd^.S-Max(vg.char_h,vg.sign_h)-1;
    x1:=83+32*vg.char_w; y1:=sht^.wnd^.S-2;
    vg.mode(wnd,vg.bic);
    IF x1>(wnd^.dsx-1-wnd^.E) THEN x1:=wnd^.dsx-1-wnd^.E END;
    vg.box(wnd,x0,y0,x1,y1);
    win.release_window(wnd);
    ShowStat(sht);
  END;
END ini_main;

PROCEDURE ini_stat(w: WORD);
BEGIN
  ini(scr.stat);
  scr.stat^.W:=3; scr.stat^.E:=3;
  scr.stat^.N:=3; scr.stat^.S:=14;
END ini_stat;

PROCEDURE set_tool(w: window; n: INTEGER; sht: Sheet);
BEGIN
  IF n<=0  THEN RETURN END;
  sht^.EditorContext^.TreckTool:=n-1;
END set_tool;

PROCEDURE edit_chip_plsm(w: window; n: INTEGER; sht: Sheet);
  VAR pmt: ARRAY [0..15] OF CHAR; x,y: INTEGER; c: CHAR;
BEGIN
  WITH sht^.EditorContext^ DO
    CASE n OF
     |0:
     |1: IF InsertAllPins(sht,TRUE) THEN END;
     |2: EditMode:=StartMacro;
     |3: EditMode:=InsMacro;
     |4: IF sht^.PublicContext^.Chip#NIL THEN
           WITH sht^.PublicContext^ DO
             str.print(pmt,'Kill chip %s ?',Chip^.Name);
             IF men.qwest(5,100,pmt) THEN
               IF Chip^.RB>=0 THEN
                 scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,5);
                 scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,1);
               END;
               IF KillChip(sht,Chip) THEN END;
             END;
             Chip:=NIL; EditMode:=-1;
             scr.Text(sht,1,'');
           END;
         END;
    END;
  END;
END edit_chip_plsm;

PROCEDURE edit_dgt_trck(w: window; n: INTEGER; sht: Sheet);
  VAR wnds,mw: window;
      x,y: INTEGER; c: CHAR;
BEGIN
  WITH sht^.EditorContext^ DO
    CASE n OF
     |0:
     |1: InChain:=FALSE; EditMode:=StartMacro;
     |2: InChain:=FALSE;
         mw:=win.create((vg.char_w*30+31) DIV 32,vg.char_h*3);
         mw^.x:=((VG.dots+31) DIV 32-mw^.sx) DIV 2;
         mw^.y:=( VG.lines          -mw^.sy) DIV 2;
         vg.inverse(mw,vg.off); vg.mode(mw,vg.rep); vg.color(mw,2);
         vg.box(mw,0,0,mw^.dsx-1,mw^.sy-1);
         vg.mode(mw,vg.bic);
         vg.print(mw,vg.char_w*2,vg.char_h,'    select model to insert    ');
         vg.color(mw,8); vg.mode(mw,vg.rep);
         vg.print(mw,vg.char_w*2,vg.char_h,'    select model to insert    ');
         --'укажи модель, которую вставить'
         win.open(mw);
         LOOP
           win.read_point(2,w,x,y,c);
           IF (c=33c) OR (c=12c) THEN EXIT END;
           IF c=266c THEN
             wnds:=win.search();
             IF wnds#NIL THEN
               shts:=NIL;
               FOR x:=0 TO HIGH(Sheets) DO
                 IF wnds^.info=Sheets[x] THEN shts:=Sheets[x] END;
               END;
               IF shts#NIL THEN
                 IF shts#sht THEN EditMode:=InsMacro; EXIT
                 ELSE men.alarm('Do not use own model') END;
               ELSE
                 men.alarm('This is not model')
               END;
             END;
           END;
         END;
         win.remove(mw);
         --EditMode:=InsMacro;
     |3: InChain:=FALSE;
         mw:=win.create((vg.char_w*30+31) DIV 32,vg.char_h*3);
         mw^.x:=((VG.dots+31) DIV 32-mw^.sx) DIV 2;
         mw^.y:=( VG.lines          -mw^.sy) DIV 2;
         vg.inverse(mw,vg.off); vg.mode(mw,vg.rep); vg.color(mw,2);
         vg.box(mw,0,0,mw^.dsx-1,mw^.sy-1);
         vg.mode(mw,vg.bic);
         vg.print(mw,vg.char_w*2,vg.char_h,'    select model to overlap   ');
         vg.color(mw,8); vg.mode(mw,vg.rep);
         vg.print(mw,vg.char_w*2,vg.char_h,'    select model to overlap   ');
         --'укажи модель, которую наложить'
         win.open(mw);
         LOOP
           win.read_point(2,w,x,y,c);
           IF (c=33c) OR (c=12c) THEN EXIT END;
           IF c=266c THEN
             wnds:=win.search();
             IF wnds#NIL THEN
               shts:=NIL;
               FOR x:=0 TO HIGH(Sheets) DO
                 IF wnds^.info=Sheets[x] THEN shts:=Sheets[x] END;
               END;
               IF shts#NIL THEN
                 IF shts#sht THEN EditMode:=OvlMacro; EXIT
                 ELSE men.alarm('Do not use own model') END;
               ELSE
                 men.alarm('This is not model')
               END;
             END;
           END;
         END;
         win.remove(mw);
         --EditMode:=OvlMacro;
     |4: InChain:=FALSE; EditMode:=DelBox;
     |5: InChain:=FALSE; EditMode:=DelSg;
    END;
  END;
END edit_dgt_trck;

PROCEDURE edit_cht_edit(w: window; n: INTEGER; sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    CASE n OF
     |0:
     |1: InChain:=FALSE; EditMode:=DelSg;
    END;
  END;
END edit_cht_edit;

PROCEDURE show_dgt_trck(w: window; n: INTEGER; sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      CASE n OF
       |0:
       |1: show?:=1;
       |2: InChain:=FALSE;
           Name:=''; men.readln(100 DIV 32,130,'Signal name: ',Name);
           FindName(sht);
       |3: show?:=0;
       |4: Name:=''; men.readln(100 DIV 32,130,'Chip name: ',Name);
           Chip:=FindChip(sht^.mdl,Name);
           IF Chip=NIL THEN Error('Chip not found',sht); RETURN END;
           scr.SetWindow(sht,Scales[Zoom],Chip^.XB,Chip^.YB);
       |5: IF Signal#NIL THEN
             cdsSeq.Next(Signal,CursorX,CursorY,sht^.PublicContext^.ExtPin);
             scr.SetWindow(sht,Scales[Zoom],CursorX,CursorY);
           END;
       |6: show?:=2;
      END;
    END;
  END;
END show_dgt_trck;

PROCEDURE show_cht_edit(w: window; n: INTEGER; sht: Sheet);
  VAR ok: BOOLEAN; i: INTEGER;
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      CASE n OF
       |0:
       |1: show?:=1;
       |2: InChain:=FALSE;
           Name:='';
           men.readln(100 DIV 32,130,'External pin No: ',Name);
           i:=0;
           str.iscan(EPinNo,Name,i,ok);
           FindEpinNo(sht);
       |3: show?:=0;
      END;
    END;
  END;
END show_cht_edit;

PROCEDURE exit_from_edit(sht: Sheet; cansel: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (Sheets[i]#sht)&(i<HIGH(Sheets)) DO INC(i) END;
  IF NOT cansel THEN io.WriteModel(sht^.mdl) END;
  RemoveModel(sht^.mdl,0);
  scr.CloseWindow(sht);
  DISPOSE(sht^.PublicContext);
  DISPOSE(sht^.EditorContext);
  sht^.wnd^.info:=NIL;
  win.remove(sht^.wnd);
  DISPOSE(sht);
  Sheets[i]:=Sheets[HIGH(Sheets)];
  RESIZE(Sheets,HIGH(Sheets))
END exit_from_edit;

PROCEDURE gner_dgt_trck(w: window; n: INTEGER; sht: Sheet);
  VAR name: ARRAY [0..15] OF CHAR;
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^ DO
      CASE n OF
       |0:
       |1: name:=mdl^.Name;
           men.readln(5,100,'Write to file',mdl^.Name);
           IF mdl^.Name[0]#0c THEN io.WriteModel(mdl) END;
           mdl^.Name:=name;
       |2: exit_from_edit(sht,FALSE);
       |3: IF men.qwest(2,100,'All data in model will be lost !!!  '
                              'Are you sure ?') THEN
             exit_from_edit(sht,TRUE);
           END;
      END;
    END;
  END;
END gner_dgt_trck;

VAR edit,show,gner: men.menu_proc;

PROCEDURE set_mode(w: window; n: INTEGER; sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    WITH sht^.PublicContext^ DO
      IF (mode=(n-1)) OR (n=0) THEN RETURN END;
      FinishChain(sht); InMacro:=FALSE; InDelBox:=FALSE;
      EditMode:=-1; scr.CursorOff(sht);
      Signal:=NIL; ModelMisc.Signal:=NIL; Chip:=NIL;
      CASE n OF
        |1: mode:=dgt_trck; ExtPin:=FALSE;
            edit:=edit_dgt_trck; edit_men:=edit_dgt_men;
            show:=show_dgt_trck; show_men:=show_dgt_men;
        |2: mode:=chp_plsm; ExtPin:=FALSE;
            edit:=edit_chip_plsm; edit_men:=edit_chp_men;
            show:=show_dgt_trck ; show_men:=show_dgt_men;
        |3: mode:=cht_edit; ExtPin:=TRUE;
            edit:=edit_cht_edit ; edit_men:=edit_cht_men;
            show:=show_cht_edit ; show_men:=show_cht_men;
      END;
      scr.CloseWindow(sht);
      scr.OpenWindow(sht,Scales[Zoom],WindowX,WindowY);
    END;
  END;
END set_mode;

PROCEDURE set_step(w: window; n: INTEGER; sht: Sheet);
BEGIN
  WITH sht^.EditorContext^ DO
    CASE n OF
      |0:
      |1: Speed:=48; Correct(sht);
      |2: Speed:=24; Correct(sht);
      |3: Speed:=16; Correct(sht);
    END;
  END;
END set_step;

PROCEDURE main_job(w: window; x,y: INTEGER; c: CHAR);
  VAR wx,wy,fx,fy,sx,sy,ox,oy,pos: INTEGER;
      sht: Sheet;
      ch_tp: Object;
      pmt: ARRAY [0..79] OF CHAR;
      PinSigName, chip_name: ARRAY [0..15] OF CHAR;
      e,e1: Reaction; r,ok: BOOLEAN;
BEGIN
  ASSERT(w^.info#NIL);
  IF Exception?(e1) THEN men.alarm(Message); RETURN END;
  sht:=w^.info;
  WITH sht^ DO
    WITH sht^.EditorContext^ DO
      WITH sht^.PublicContext^ DO
        wx:=x-w^.x*32; wy:=y-w^.y;
        fx:=x-wnd^.x*32; fy:=y-wnd^.y; fy:=wnd^.sy-fy-1;
        recalc(fx,fy,sht); ShowPos(sht);
        IF c=266c THEN
          IF (wy>w^.sy-1-(vg.sign_h+3)) & (wy<=w^.sy-1-3) THEN
            IF (wx>=3) & (wx<3+vg.sign_w) THEN
              REPEAT win.read_point(1,w,x,y,c) UNTIL c=267c;
              win.close(w);
              w^.x:=(x-18+16) / 32; w^.y:=y-w^.sy+9;
              win.open(w);
            ELSIF (wx>=23) & (wx<23+vg.sign_w) THEN
              IF men.qwest(2,100,'All data in model will be lost !!!  '
                                 'Are you sure ?') THEN
                exit_from_edit(sht,TRUE);
              END;
            ELSIF (wx>=43) & (wx<43+vg.sign_w) THEN
              IF Zoom<HIGH(Scales) THEN
                win.lock_window(sht^.wnd);
                vg.color(sht^.wnd,4); vg.mode(sht^.wnd,vg.rep);
                IF Zoom=0 THEN
                  vg.sign(sht^.wnd,63,3,minus);
                END;
                INC(Zoom);
                IF Zoom=HIGH(Scales) THEN
                  vg.sign(sht^.wnd,43,3,fill);
                END;
                win.release_window(sht^.wnd);
                win.refresh(sht^.wnd);
                scr.SetWindow(sht,Scales[Zoom],WindowX,WindowY);
              END;
            ELSIF (wx>=63) & (wx<63+vg.sign_w) THEN
              IF Zoom>0 THEN
                win.lock_window(sht^.wnd);
                vg.color(sht^.wnd,4); vg.mode(sht^.wnd,vg.rep);
                IF Zoom=HIGH(Scales) THEN
                  vg.sign(sht^.wnd,43,3,plus);
                END;
                DEC(Zoom);
                IF Zoom=HIGH(Scales) THEN
                  vg.sign(sht^.wnd,63,3,fill);
                END;
                win.release_window(sht^.wnd);
                win.refresh(sht^.wnd);
                scr.SetWindow(sht,Scales[Zoom],WindowX,WindowY);
              END;
            ELSIF (wx<w^.sx*32-1-3) & (wx>=w^.sx*32-1-3-vg.sign_w) THEN
              -- resize
              REPEAT win.read_point(4,w,x,y,c) UNTIL c=267c;
              fx:=w^.x; fy:=w^.y;
              sx:=(x+31) DIV 32 - fx; sy:=y+8-fy;
              IF sx<4 THEN sx:=4 END;
              IF sy<20*9 THEN sy:=20*9 END;
              IF (sx#w^.sx) OR (sy#w^.sy) THEN
                ox:=w^.dsx; oy:=w^.sy;
                win.resize(w,sx,sy);
                wnd^.ini_proc(sht);
                win.open(wnd); w:=wnd;
                scr.HardSetWindow(sht,Scales[Zoom],
                                  WindowX+(wnd^.dsx-ox)*ScaleX DIV 2,
                                  WindowY-(wnd^.sy -oy)*ScaleY DIV 2);
              END;
            ELSE
              win.ontop(w)
            END;
            KillReaction(e1); RETURN
          ELSIF (wy<(vg.char_h+3)) & (wy>=3) THEN
            IF (wx>=23)&(wx<(23+vg.char_w*4)) THEN
              men.tmp_menu(modes,w^.x+23 DIV 32,w^.y+3,set_mode,sht);
              ini_main(sht); win.ref_box(w,0,vg.char_h+3);
            ELSIF (wx>=(23+vg.char_w*5))&(wx<(23+vg.char_w*9)) THEN
              men.tmp_menu(steps,w^.x+(23+vg.char_w*5) DIV 32,w^.y+3,
                           set_step,sht);
            ELSIF (wx>=(23+vg.char_w*10))&(wx<(23+vg.char_w*14)) THEN
              men.tmp_menu(show_men,w^.x+(23+vg.char_w*10) DIV 32,w^.y+3,
                           show,sht);
            ELSIF (wx>=(23+vg.char_w*15))&(wx<(23+vg.char_w*19)) THEN
              men.tmp_menu(edit_men,w^.x+(23+vg.char_w*15) DIV 32,w^.y+3,
                           edit,sht);
            ELSIF (wx>=(23+vg.char_w*20))&(wx<(23+vg.char_w*24)) THEN
              men.tmp_menu(tools,w^.x+(23+vg.char_w*20) DIV 32,w^.y+3,
                           set_tool,sht);
            ELSIF (wx>=(23+vg.char_w*25))&(wx<(23+vg.char_w*32)) THEN
              men.tmp_menu(general,w^.x+(23+vg.char_w*20) DIV 32,w^.y+3,
                           gner,sht);
            ELSIF (wx>=(23+vg.char_w*51))&(wx<(23+vg.char_w*54)) THEN
              sht^.PublicContext^.check_on:=NOT sht^.PublicContext^.check_on;
              win.lock_window(wnd);
              vg.color(sht^.wnd,5); vg.mode(sht^.wnd,vg.rep);
              IF sht^.PublicContext^.check_on THEN
                vg.print(wnd,23+51*vg.char_w,wnd^.sy-1-(vg.char_h+2),'on ');
              ELSE
                vg.print(wnd,23+51*vg.char_w,wnd^.sy-1-(vg.char_h+2),'off');
              END;
              win.ref_box(wnd,4,vg.char_h+5);
              win.release_window(wnd);
            ELSE
              win.ontop(w)
            END;
            KillReaction(e1); RETURN
          ELSIF (wy<(vg.char_h*2+4)) & (wy>=vg.char_h+4) THEN
            IF (wx<(vg.sign_w+4+vg.char_w*10)) & (wx>=vg.sign_w+4) THEN
              IF (sht^.EditorContext^.mode=dgt_trck) OR
                 (sht^.EditorContext^.mode=cht_edit) THEN
                IF NOT InChain THEN Layer:=(Layer+1) MOD 2; ShowStat(sht) END;
              ELSIF (sht^.EditorContext^.mode=chp_plsm) &
                    (sht^.PublicContext^.Chip#NIL) THEN
                WITH sht^.PublicContext^ DO
                  IF Chip^.RB>=0 THEN
                    scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,5);
                    scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,1);
                  END;
                  chip_r:=(chip_r+1) MOD 4;
                  IF Chip^.RB>=0 THEN
                    scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,0);
                    scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,chip_r,4);
                  END;
                  ShowStat(sht);
                END;
              END;
            ELSIF (wx<(vg.sign_w+4+vg.char_w*33+1)) &
                  (wx>=vg.sign_w+4+vg.char_w*10+1) THEN
              IF (sht^.EditorContext^.mode=dgt_trck) THEN
                InChain:=FALSE;
                IF Signal#NIL THEN Name:=Signal^.Name
                ELSE               Name:='' END;
                men.read_string(w,vg.sign_w+4+vg.char_w*23+1,
                                w^.sy-1-3-vg.char_h*2,17,Name);
                FindName(sht);
              ELSIF (sht^.EditorContext^.mode=cht_edit) THEN
                InChain:=FALSE;
                IF Signal#NIL THEN str.print(Name,'%d',Signal^.EPinNo)
                ELSE               Name:='' END;
                men.read_string(w,vg.sign_w+4+vg.char_w*23+1,
                                w^.sy-1-3-vg.char_h*2,10,Name);
                pos:=0;
                str.iscan(EPinNo,Name,pos,ok);
                FindEpinNo(sht);
              ELSIF (sht^.EditorContext^.mode=chp_plsm) THEN
                EditMode:=-1;
                IF Chip#NIL THEN Name:=Chip^.Name
                ELSE             Name:='' END;
                men.read_string(w,vg.sign_w+4+vg.char_w*23+1,
                                w^.sy-1-3-vg.char_h*2,10,Name);
                LastChip:=Chip;
                Chip:=FindChip(sht^.mdl,Name);
                IF (Chip#NIL)&(Chip^.RB>=0) THEN
                  scr.SetWindow(sht,Scales[Zoom],Chip^.XB,Chip^.YB)
                END;
                IF LastChip^.RB>=0 THEN
                  scr.Chip(sht,LastChip,LastChip^.XB,LastChip^.YB,chip_r,5);
                  scr.Chip(sht,LastChip,LastChip^.XB,LastChip^.YB,chip_r,1);
                  scr.Chip(sht,LastChip,LastChip^.XB,LastChip^.YB,
                           LastChip^.RB,0);
                END;
                IF Chip#NIL THEN
                  scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,Chip^.RB,4);
                  chip_r:=Chip^.RB;
                  IF chip_r<0 THEN EditMode:=move_chip; chip_r:=0 END;
                END;
                IF (Chip=NIL)&(Name[0]#0c)&
                   (men.qwest(3,100,'Create new chip ?')) THEN
                  chip_name:=Name; Name:='';
                  LOOP
                    men.readln(3,100,'Enter chip type',Name);
                    ch_tp:=NIL;
                    ch_tp:=pct.FindChipType(Name,sht^.mdl);
                    IF ch_tp=NIL THEN
                      r:=Exception?(e);
                      IF r THEN
                        IF r#IOerror THEN RaiseInMe(r) END;
                        men.alarm(Message)
                      ELSE
                        str.print(pmt,
                           'Chip type %s not in model. Enter file name',Name);
                        men.readln(3,100,pmt,Name);
                        IF Name[0]=0c THEN KillReaction(e); EXIT END;
                        ch_tp:=io.ReadModel(Name);
                        KillReaction(e); EXIT
                      END;
                    ELSE
                      EXIT
                    END;
                  END;
                  IF ch_tp#NIL THEN
                    IF pct.LinkToModel(ch_tp,sht^.mdl) THEN END;
                    IF NewChip(sht,ch_tp,chip_name,Chip) THEN END;
                    Chip^.RB:=-1; chip_r:=0;
                    EditMode:=move_chip;
                  END;
                END;
                ShowStat(sht);
              END;
            ELSIF (wx<(vg.sign_w+4+vg.char_w*49+2)) &
                  (wx>=vg.sign_w+4+vg.char_w*40+2) THEN
              IF (sht^.EditorContext^.mode=dgt_trck) THEN
                Fixed:=NOT Fixed; ShowStat(sht);
              END;
            END;
            KillReaction(e1); RETURN
          ELSIF (wx>=3) & (wx<3+vg.sign_w) OR
                (wx<=wnd^.dsx-1-3) & (wx>wnd^.dsx-1-(3+vg.sign_w)) THEN
            IF    (wy>=23) & (wy<23+vg.sign_h) THEN
              scr.SetWindow(sht,Scales[Zoom],WindowX,WindowY+2*ScaleY*vg.rollY);
            ELSIF (wy>=43) & (wy<43+vg.sign_h) THEN
              scr.SetWindow(sht,Scales[Zoom],WindowX,WindowY-2*ScaleY*vg.rollY);
            ELSIF (wy>=63) & (wy<63+vg.sign_h) THEN
              scr.SetWindow(sht,Scales[Zoom],WindowX+2*ScaleX*vg.rollX,WindowY);
            ELSIF (wy>=83) & (wy<83+vg.sign_h) THEN
              scr.SetWindow(sht,Scales[Zoom],WindowX-2*ScaleX*vg.rollX,WindowY);
            ELSIF (wy>=103) & (wy<103+vg.sign_h) THEN
              FinishChain(sht); InMacro:=FALSE; InDelBox:=FALSE;
              EditMode:=-1; scr.CursorOff(sht);
            ELSIF (wy>=123) & (wy<123+vg.sign_h) THEN
              IF mode#chp_plsm THEN
                InChain:=FALSE; EditMode:=StartEdit; Insert:=TRUE;
                scr.Text(sht,2,'Search for INSERT');
              END;
            ELSIF (wy>=143) & (wy<143+vg.sign_h) THEN
              IF mode#chp_plsm THEN
                InChain:=FALSE; EditMode:=StartEdit; Insert:=FALSE;
                scr.Text(sht,2,'Search for DELETE');
              END;
            ELSE
              win.ontop(w);
            END;
            KillReaction(e1); RETURN
          END;
          IF (wx>=0) & (wx<3) OR (wx<=wnd^.dsx-1) & (wx>wnd^.dsx-1-3) OR
             (wy>=0) & (wy<3) OR (wy<=wnd^.sy -1) & (wy>wnd^.sy -1-3) OR
             (wy=3+vg.char_h) OR (wx=3+vg.sign_w) OR
             (wy=wnd^.sy-1-(3+vg.sign_h)) OR (wx=wnd^.dsx-1-(3+vg.sign_w)) OR
             (wy=vg.char_h*2+4)
          THEN
            win.ontop(w);
            KillReaction(e1); RETURN
          END;

          IF mode=chp_plsm THEN
            recalc(wx,w^.sy-wy-1,sht);
            IF (Chip#NIL)&(Chip^.RB>=0) THEN
              LastPin:=CurPin;
              CurPin:=LookUpPin(Chip,CursorXm,CursorYm);
              UpdatePin(sht,CurPin);
              IF CurPin#NIL THEN
                IF CurPin#LastPin THEN
                  scr.Text(sht,2,'Pin %d  Signal %s',
                           CurPin^.No+1,CurPin^.Signal^.Name);
                  KillReaction(e1); RETURN
                ELSE
                  LOOP
                    str.print(pmt,'%s  Pin %d  Signal %s  Enter new signal name',
                           Chip^.Name,CurPin^.No+1,CurPin^.Signal^.Name);
                    PinSigName:=CurPin^.Signal^.Name;
                    men.readln(3,150,pmt,PinSigName);
                    IF PinSigName#CurPin^.Signal^.Name THEN
                      IF PinSigName[0]#0c THEN
                        Name:=PinSigName; FindName(sht);
                        IF Signal#NIL THEN
                          IF RenamePin(sht,CurPin,Signal,TRUE) THEN
                            IF men.qwest(4,150,
                               ' short surcut  !!!  Are you sure ? ') THEN
                              IF RenamePin(sht,CurPin,Signal,FALSE) THEN END;
                            END;
                          END;
                          EXIT
                        END;
                      ELSE
                        men.alarm('Empty signal name is impossible')
                      END;
                    ELSE
                      EXIT
                    END;
                  END;
                  KillReaction(e1); RETURN
                END;
              END;
            END;
            LastChip:=Chip;
            Chip:=LookUpChip(mdl,CursorXm,CursorYm);
            IF Chip#LastChip THEN
              IF LastChip^.RB>=0 THEN
                scr.Chip(sht,LastChip,LastChip^.XB,LastChip^.YB,chip_r,5);
                scr.Chip(sht,LastChip,LastChip^.XB,LastChip^.YB,chip_r,1);
                scr.Chip(sht,LastChip,LastChip^.XB,LastChip^.YB,LastChip^.RB,0);
              END;
              IF Chip#NIL THEN
                scr.Text(sht,1,'%s  %s x=%d y=%d r=%d\n',
                         Chip^.Name, Chip^.ChipType^.Name,
                         Chip^.XB, Chip^.YB, Chip^.RB);
                scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,Chip^.RB,4);
                chip_r:=Chip^.RB; IF chip_r<0 THEN chip_r:=0 END;
              ELSE
                scr.Text(sht,1,'');
              END;
              ShowStat(sht);
            ELSIF Chip#NIL THEN
              EditMode:=move_chip;
            END;
            KillReaction(e1); RETURN
          END;
          IF ((mode=dgt_trck) OR (mode=cht_edit)) & (show?>=0) THEN
             CASE show? OF
                0: recalc(wx,w^.sy-wy-1,sht); ShowRange(sht);
               |1: recalc(wx,w^.sy-wy-1,sht); InChain:=FALSE; LookUpSignal(sht);
               |2: recalc(wx,w^.sy-wy-1,sht);
                   IF Chip^.RB>=0 THEN
                     LastChip:=Chip;
                     scr.Chip(sht,LastChip,
                              LastChip^.XB,LastChip^.YB,LastChip^.RB,5);
                   END;
                   Chip:=LookUpChip(mdl,CursorXm,CursorYm);
                   IF Chip#NIL THEN
                     Error("",sht);
                     scr.Text(sht,1,'%s  %s x=%d y=%d r=%d\n',
                              Chip^.Name, Chip^.ChipType^.Name,
                              Chip^.XB, Chip^.YB, Chip^.RB);
                     scr.Chip(sht,Chip,Chip^.XB,Chip^.YB,Chip^.RB,4);
                   ELSE
                     scr.Text(sht,1,'');
                   END;
             END;
             KillReaction(e1); RETURN
          END;
        END;
        IF (InChain OR InMacro OR (EditMode>=0))&(wx>wnd^.W)&(wy>wnd^.N)&
           (wx<=(wnd^.dsx-wnd^.E))&(wy<=(wnd^.sy-wnd^.S)) THEN
          DigitizeTracks(x,y,sht);
        END;
      END;
    END;
  END;
END main_job;

PROCEDURE stat_job(w: window; x,y: INTEGER; c: CHAR);
  VAR wx,wy,fx,fy,sx,sy,ox,oy: INTEGER;
BEGIN
  ASSERT(w=scr.stat);
  wx:=x-w^.x*32; wy:=y-w^.y;
  IF (c=226c) OR (c=227c) OR (c=215c) OR (c=266c) THEN
    IF (wy>=w^.sy-(2+vg.sign_h)) & (wy<w^.sy-2) THEN
      IF (wx>=3) & (wx<3+vg.sign_w) THEN
        REPEAT win.read_point(1,w,x,y,c)
        UNTIL (c=236c) OR (c=237c) OR (c=225c) OR (c=267c);
        win.close(w);
        w^.x:=(x-18+16) / 32; w^.y:=y-w^.sy+9;
        win.open(w);
      ELSIF (wx<w^.sx*32-1-3) & (wx>=w^.sx*32-1-3-vg.sign_w) THEN
        -- resize
        REPEAT win.read_point(4,w,x,y,c)
        UNTIL (c=236c) OR (c=237c) OR (c=225c) OR (c=267c);
        fx:=w^.x; fy:=w^.y;
        sx:=(x+31) DIV 32 - fx; sy:=y+8-fy;
        IF sx<4 THEN sx:=4 END;
        IF sy<64 THEN sy:=64 END;
        IF (sx#w^.sx) OR (sy#w^.sy) THEN
          win.remove(w);
          scr.stat:=win.create(sx,sy);
          scr.stat^.x:=fx; scr.stat^.y:=fy; scr.stat^.job:=stat_job;
          scr.stat^.ini_proc:=ini_stat;
          scr.stat^.ini_proc(0);
          win.open(scr.stat);
        END;
      ELSE
        win.ontop(w);
      END;
    END;
    RETURN
  END;
END stat_job;

PROCEDURE open_window(s: Sheet);
BEGIN
  WITH s^ DO
    IF wnd=NIL THEN
      IF HIGH(Sheets)=-1 THEN
        wnd:=win.create(15,298);
        wnd^.x:=0; wnd^.y:=62;
      ELSE
        wnd:=win.create(4,20*9);
        wnd^.x:=0; wnd^.y:=0;
      END;
      wnd^.job:=main_job;
      wnd^.ini_proc:=ini_main;
      wnd^.ini_proc(s);
    END;
    win.open(wnd);
  END;
  IF scr.stat=NIL THEN
    scr.stat:=win.create(15,62);
    scr.stat^.job:=stat_job;
    scr.stat^.ini_proc:=ini_stat;
    scr.stat^.ini_proc(0);
  END;
  win.open(scr.stat);
END open_window;

PROCEDURE Editor(s: Sheet);
  VAR e: Reaction; r: BOOLEAN;
BEGIN
  r:=Exception?(e);
  IF r THEN
    scr.CloseWindow(s);
    IF r#MemoryOverflow THEN RaiseInMe(r) END;
    IF s^.wnd#NIL THEN s^.wnd^.info:=NIL; win.remove(s^.wnd) END;
    IF s^.EditorContext#NIL THEN DISPOSE(s^.EditorContext) END;
    IF s^.PublicContext#NIL THEN DISPOSE(s^.PublicContext) END;
    RaiseInMe(r);
  END;
  NEW(s^.EditorContext);
  NEW(s^.PublicContext);
  WITH s^.EditorContext^ DO
    WITH s^.PublicContext^ DO
      Chip:=NIL; LastChip:=NIL;
      CurPin:=NIL;
      LastPin:=NIL;
      mode:=dgt_trck; edit:=edit_dgt_trck ; edit_men:=edit_dgt_men;
                      show:=show_dgt_trck ; show_men:=show_dgt_men;
                      gner:=gner_dgt_trck ; general :=gner_dgt_men;
      check_on:=TRUE;
      show?:=-1;
      Speed:=48; Zoom:=3;
      TreckTool:=4; PinTool:=5;
      Fixed:=TRUE;
      CursorXm:=0; CursorYm:=0;
      Signal:=NIL;
      ExtPin:=FALSE;
      Boxed:=FALSE;
      CursorX:=0; CursorY:=0;
      Layer:=0; InChain:=FALSE; InMacro:=FALSE;
      EditMode:=-1;
      open_window(s); s^.wnd^.info:=s;
      scr.OpenWindow(s,Scales[Zoom],CursorX,CursorY);
    END;
  END;
  RESIZE(Sheets,HIGH(Sheets)+2);
  KillReaction(e);
  Sheets[HIGH(Sheets)]:=s;
END Editor;

BEGIN
  NEW(Sheets);
  Scales[0]:=48;
  Scales[1]:=24;
  Scales[2]:=12;
  Scales[3]:=6;
  Scales[4]:=3;
  Scales[5]:=1;
  steps:=men.cre_tmp_menu('Step|1/20"|1/40"|1/60"');
  modes:=men.cre_tmp_menu('Mode|Digitize tracks|Chips placement|'
                          'Chip type editor');
  tools:=men.cre_tmp_menu('Tool|  0  |  1  |  2  |  3  |  4  |'
         '  5  |  6  |  7  |  8  |  9  | 10  | 11  | 12  | 13  |'
         ' 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  ');
  show_dgt_men:=men.cre_tmp_menu('   Show|signal by cursor|'
                                 'signal by name|segment info|'
                                 'chip by name|next pin|'
                                 'chip by cursor');
  show_cht_men:=men.cre_tmp_menu('   Show|external pin by cursor|'
                                 'external pin by name|segment info');
  edit_dgt_men:=men.cre_tmp_menu('Edit|Define Macro|'
                                 'Insert Macro|Overlap Macro|Delete Box|'
                                 'Delete Segment');
  edit_cht_men:=men.cre_tmp_menu('Edit|Delete Segment');
  edit_chp_men:=men.cre_tmp_menu('Edit|Insert all pins|Define Macro|'
                                 'Move Macro|Kill Chip');
  gner_dgt_men:=men.cre_tmp_menu('General|Write model|'
                                 'Write model & close window|Cansel');
END pedEditor.
