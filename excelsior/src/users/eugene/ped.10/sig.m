MODULE sig; (* 13-Feb-87. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Pattern    IMPORT  Match;
FROM Model      IMPORT  Object, Objects, Iterate, Tag, String, Segment,
                        NewObject, Tie, KillObject, KillList, UnTie, SigTypes;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM Edit       IMPORT  ReadString;
FROM Image      IMPORT  image0;
IMPORT FROM ModelMisc;

VAR e           : Reaction;
    Name        : ARRAY [0..79] OF CHAR;
    mdl         : Object;
    Patt        : ARRAY [0..79] OF CHAR;
    FindName    : String;
    FindSig     : Object;
    cmds        : ARRAY [0..4] OF ARRAY [0..15] OF CHAR;

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

PROCEDURE EditSignal;
  VAR cmd: ARRAY [0..79] OF CHAR; i: INTEGER;
BEGIN
  cmds[0]:='fixed';
  cmds[1]:='fantom';
  cmds[2]:='power';
  cmds[3]:='bye';
  cmds[4]:='name';
  LOOP
    print('Сигнал %s\n',FindSig^.Name);
    print('Атрибуты :');
    IF fixed  IN FindSig^.sType THEN print(' fixed') END;
    IF fantom IN FindSig^.sType THEN print(' fantom') END;
    IF power  IN FindSig^.sType THEN print(' power') END;
    print('\n');
    ReadString('>',cmd); print('\n');
    i:=FindCmd(cmd);
    CASE i OF
      0: IF fixed IN FindSig^.sType THEN
           EXCL(FindSig^.sType,fixed)
         ELSE
           INCL(FindSig^.sType,fixed)
         END;
     |1: IF fantom IN FindSig^.sType THEN
           EXCL(FindSig^.sType,fantom)
         ELSE
           INCL(FindSig^.sType,fantom)
         END;
     |2: IF power IN FindSig^.sType THEN
           EXCL(FindSig^.sType,power)
         ELSE
           INCL(FindSig^.sType,power)
         END;
     |3: RETURN
     |4: ReadString('New name:',FindSig^.Name); print('\n');
    ELSE
      print('Непонятная команда.\n');
    END;
  END;
END EditSignal;

PROCEDURE FindSignal(o: Object);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name#FindName THEN RETURN END;
  FindSig:=o;
END FindSignal;

BEGIN
  ScanFlags;
  TakeWord(Name);
  IF Name[0]=0c THEN HALT END;
  IF Exception?(e) THEN print('%s',Message); HALT END;
  mdl:=ReadModel(Name);
  image0(FindName,'GND');
  LOOP
    ReadString('Имя сигнала: ',FindName); print('\n');
    IF FindName[0]=0c THEN EXIT END;
    FindSig:=NIL;
    Iterate(mdl^.All,FindSignal);
    IF FindSig#NIL THEN
      EditSignal
    ELSE
      print('Нет сигнала %s\n',FindName);
    END;
  END;
  WriteModel(mdl);
END sig.
