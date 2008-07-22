MODULE chp; (* 13-Feb-87. (c) KRONOS *)

FROM Terminal   IMPORT  print, Read;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Pattern    IMPORT  Match;
FROM Model      IMPORT  Object, Objects, Iterate, Tag, String, Segment,
                        NewObject, Tie, KillObject, KillList, UnTie, SigTypes;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM Edit       IMPORT  ReadString;
FROM Image      IMPORT  image0, GetNum;
IMPORT FROM ModelMisc;

VAR e           : Reaction;
    Name        : ARRAY [0..79] OF CHAR;
    mdl         : Object;
    Patt        : ARRAY [0..79] OF CHAR;
    FindName    : String;
    FindCp      : Object;
    cmds        : ARRAY [0..5] OF ARRAY [0..15] OF CHAR;

PROCEDURE FindCmd(cmd: ARRAY OF CHAR): INTEGER;
  VAR i,j,k: INTEGER;
BEGIN
  j:=-1;
  FOR i:=0 TO HIGH(cmds) DO
    k:=0;
    LOOP
      IF cmds[i,k]=0c THEN
        IF cmd[k]#0c THEN EXIT END;
        IF j>=0 THEN RETURN -2 END;
        j:=i; EXIT;
      ELSE
        IF cmd[k]=0c THEN IF j>=0 THEN RETURN -2 ELSE j:=i; EXIT END END;
        IF cmd[k]=cmds[i,k] THEN INC(k) ELSE EXIT END;
      END;
    END;
  END;
  RETURN j;
END FindCmd;

PROCEDURE DelPin(o: Object);
  VAR x,y: INTEGER;
BEGIN
  PinLocation(o,x,y);
  IF o^.Signal=NIL THEN RETURN END;
  StartConductor(o^.Signal);
  WHILE NOT Empty DO
    IF (X1=x)&(X2=x)&(Y1=y)&(Y2=y) THEN DelConductor END;
    NextConductor;
  END;
END DelPin;

PROCEDURE CrePin(o: Object);
  VAR x,y: INTEGER;
BEGIN
  PinLocation(o,x,y);
  IF o^.Signal=NIL THEN RETURN END;
  StartConductor(o^.Signal);
  X1:=x; X2:=x; Y1:=y; Y2:=y;
  Size:=26; ViasSize:=1; Fixed:=TRUE; Layer:={0..1};
  AppConductor;
END CrePin;

PROCEDURE KillPin(p: Object);
BEGIN
  UnTie(p^.Signal^.TiedPins,p);
  UnTie(mdl^.All,p);
  KillObject(p);
END KillPin;

PROCEDURE EditChip;
  VAR cmd: ARRAY [0..79] OF CHAR; i,x,y,r: INTEGER;
BEGIN
  cmds[0]:='move';
  cmds[1]:='delete';
  cmds[2]:='insert';
  cmds[3]:='position';
  cmds[4]:='bye';
  cmds[5]:='kill';
  LOOP
    print('Компонент %s\n',FindName);
    print('Координаты: %d %d; ориентация: %d.\n',
           FindCp^.XB,FindCp^.YB,FindCp^.RB);
    ReadString('>',cmd); print('\n');
    i:=FindCmd(cmd);
    CASE i OF
      0: ReadString('X=',cmd); print('\n');
         i:=GetNum(cmd,x);
         ReadString('Y=',cmd); print('\n');
         i:=GetNum(cmd,y);
         ReadString('R=',cmd); print('\n');
         i:=GetNum(cmd,r);
         print('Переместить в %d %d %d?',x,y,r);
         IF Read()='y' THEN
           Iterate(FindCp^.Pins,DelPin);
           FindCp^.XB:=x; FindCp^.YB:=y; FindCp^.RB:=r;
           Iterate(FindCp^.Pins,CrePin);
         END;
         print('\n');
     |1: Iterate(FindCp^.Pins,DelPin);
     |2: Iterate(FindCp^.Pins,CrePin);
     |3: ReadString('X=',cmd); print('\n');
         i:=GetNum(cmd,x);
         ReadString('Y=',cmd); print('\n');
         i:=GetNum(cmd,y);
         ReadString('R=',cmd); print('\n');
         i:=GetNum(cmd,r);
         print('Переместить в %d %d %d?',x,y,r);
         IF Read()='y' THEN
           FindCp^.XB:=x; FindCp^.YB:=y; FindCp^.RB:=r;
         END;
         print('\n');
     |4: RETURN
     |5: Iterate(FindCp^.Pins,DelPin);
         Iterate(FindCp^.Pins,KillPin);
         UnTie(mdl^.All,FindCp);
         KillObject(FindCp);
         RETURN
    ELSE
      print('Непонятная команда.\n');
    END;
  END;
END EditChip;

PROCEDURE FindChip(o: Object);
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  IF o^.Name#FindName THEN RETURN END;
  FindCp:=o;
END FindChip;

BEGIN
  ScanFlags;
  TakeWord(Name);
  IF Name[0]=0c THEN HALT END;
  IF Exception?(e) THEN print('%s',Message); HALT END;
  mdl:=ReadModel(Name);
  image0(FindName,'D1');
  LOOP
    ReadString('Имя компонента: ',FindName); print('\n');
    IF FindName[0]=0c THEN EXIT END;
    FindCp:=NIL;
    Iterate(mdl^.All,FindChip);
    IF FindCp#NIL THEN
      EditChip
    ELSE
      print('Нет компонента %s\n',FindName);
    END;
  END;
  WriteModel(mdl);
END chp.
