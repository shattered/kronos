MODULE tn; (* 13-Feb-87. (c) KRONOS *)

IMPORT Terminal;
FROM StdIO      IMPORT  Show, WriteString, print, Write;
FROM Strings    IMPORT  Str1;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Pattern    IMPORT  Match;
FROM Model      IMPORT  Object, Objects, Iterate, Tag, String, Segment,
                        NewObject, Tie, KillObject, KillList, UnTie, SigTypes;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM pedTopology IMPORT Areas, Shoted?, ShotedSignal, ShotedIdent,
                        LineXs, LineXe, LineYs, LineYe;
FROM Edit       IMPORT  ReadString;
IMPORT FROM ModelMisc;

VAR e           : Reaction;
    Name        : ARRAY [0..79] OF CHAR;
    mdl         : Object;
    Patt        : ARRAY [0..79] OF CHAR;
    FindName    : String;
    FindSig     : Object;
    MayRout     : INTEGER;
    FixCnt      : INTEGER;
    PowerCnt    : INTEGER;

PROCEDURE CheckChain(s: Object);
  VAR i,j: INTEGER; nm: String;
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF NOT Match(Patt,s^.Name) THEN RETURN END;
  i:=Areas(s); nm:=s^.Name;
  IF i<=1 THEN RETURN END;
  DEC(i); j:=i MOD 10;
  IF (j IN {0,5..9})OR(i>=10)&(i<=20) THEN
    print('В сигнале %s найдено %d разрывов\n',nm,i)
  ELSIF j=1 THEN
    print('В сигнале %s найден  %d разрыв\n',nm,i)
  ELSIF j IN {2..4} THEN
    print('В сигнале %s найдено %d разрыва\n',nm,i)
  END;
  IF    power IN s^.sType THEN INC(PowerCnt,i)
  ELSIF fixed IN s^.sType THEN INC(FixCnt,i)
  ELSE INC(MayRout,i)
  END;
END CheckChain;

PROCEDURE MovePin(o: Object);
BEGIN
  IF Tag(o)#pin THEN RETURN END;
  IF (o^.Signal#NIL)&(Tag(o^.Signal)=signal) THEN
    UnTie(o^.Signal^.TiedPins,o)
  END;
  o^.Signal:=FindSig;
  Tie(FindSig^.TiedPins,o);
END MovePin;

PROCEDURE FindSignal(o: Object);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name#FindName THEN RETURN END;
  FindSig:=o;
END FindSignal;

PROCEDURE Connect(s: Object; nm: String);
  VAR sg: ARRAY [0..9999] OF Segment; cnt,i: INTEGER;
BEGIN
  FindName:=nm; FindSig:=NIL;
  Iterate(mdl^.All,FindSignal);
  IF FindSig=NIL THEN
    FindSig:=NewObject(signal);
    FindSig^.Name:=nm;
    Tie(mdl^.All,FindSig);
  END;
  IF s=FindSig THEN RETURN END;
  Iterate(s^.TiedPins,MovePin);
  cnt:=0;
  StartConductor(s);
  WHILE NOT Empty DO
    IF cnt<=HIGH(sg) THEN PackSeg(sg[cnt]); INC(cnt) END;
    NextConductor;
  END;
  StartConductor(FindSig);
  FOR i:=0 TO cnt-1 DO UnPackSeg(sg[i]); AppConductor END;
  KillList(s^.TiedPins);
  KillObject(s^.ChainB);
  KillObject(s);
END Connect;

TYPE Range=RECORD
       x1,x2,y1,y2: INTEGER;
       size       : INTEGER;
       layer      : BITSET;
     END;

PROCEDURE CheckShoted(s: Object);
  VAR i,j,l,e,p: INTEGER; nm,Name1,Name2: String;
      bf: ARRAY [0..1000] OF Range;
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF NOT Match(Patt,s^.Name) THEN RETURN END;
  StartConductor(s); i:=0;
  WHILE NOT Empty DO
    WITH bf[i] DO
      x1:=X1; x2:=X2; y1:=Y1; y2:=Y2; size:=Size; layer:=Layer;
    END;
    INC(i);
    NextConductor;
  END;
  Terminal.ClearLine;
  Terminal.WriteString(s^.Name);
  Terminal.Write(15c);
  FOR j:=0 TO i-1 DO
    WITH bf[j] DO
      FOR l:=0 TO 7 DO
        IF l IN layer THEN
          LineXs:=x1; LineYs:=y1; LineXe:=x2; LineYe:=y2;
          IF Shoted?(size,l,mdl,s) THEN
            print('Сегмент (%d,%d-%d,%d) %s замыкает на сигнал %s\n',
                   x1,y1,x2,y2,s^.Name,ShotedSignal^.Name);
            IF Flag?('m') THEN
              print('Введите новое имя для цепи %s:',s^.Name);
              Str1(Name1,s^.Name); ReadString('',Name1);
              print('\n');
              print('Введите новое имя для цепи %s:',ShotedSignal^.Name);
              Str1(Name2,ShotedSignal^.Name); ReadString('',Name2);
              print('\n');
              Connect(s,Name1);
              Connect(ShotedSignal,Name2);
            END;
          END;
        END;
      END;
    END;
  END;
END CheckShoted;

PROCEDURE CheckPins(p: Object);
BEGIN
END CheckPins;

BEGIN
  ScanFlags;
  TakeWord(Name); TakeWord(Patt);
  IF Patt[0]=0c THEN Patt:='*' END;
  IF (Name[0]=0c)OR Flag?('h') THEN
    Show('tn <model name> [<pattern for signal names>]');
    Show('   Проверка технологических норм в модели печатной платы.');
    Show('   Ключи:');
    Show('-h    подсказка');
    Show('-s    отмена поиска коротких замыканий');
    Show('-m    переименование (слияние) замкнувших сигналов');
    Show('-c    отмена поиска отсутствующих связей');
    Show('-p    выдать список неподключенных выводов компонентов');
    HALT;
  END;
  IF Exception?(e) THEN
    Show(Message); HALT
  END;
  mdl:=ReadModel(Name);
  IF NOT Flag?('c') THEN
    MayRout:=0; FixCnt:=0; PowerCnt:=0;
    Iterate(mdl^.All,CheckChain);
    print('%4d разрывов пригодных к автотрассировке.\n',MayRout);
    print('%4d разрывов в цепях питания.\n',PowerCnt);
    print('%4d разрывов трассировщику обрабатывать запрещено.\n',FixCnt);
  END;
  IF NOT Flag?('s') THEN Iterate(mdl^.All,CheckShoted) END;
  IF     Flag?('p') THEN Iterate(mdl^.All,CheckPins) END;
END tn.
