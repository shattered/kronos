MODULE chain; (* 08-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS;
FROM StdIO     IMPORT   WriteLn, QueryLn, Show, print, Write, WriteString;
FROM Model     IMPORT   String, Object, NewObject, Objects, Iterate;
FROM ModelIO   IMPORT   WriteModel, ReadModel;
FROM Chain     IMPORT   Execute;
FROM Args      IMPORT   ScanFlags, Flag?, TakeWord, ArgC;
FROM Strings   IMPORT   AppStr;
FROM ModelPbl  IMPORT   Exception?, Reaction, KillReaction, Message;
IMPORT  mcd: mCodeMnem;

VAR Name1,Name2: String;
    model: Object;
    Poz: INTEGER;
    Cnt: INTEGER;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE mk(n: INTEGER): INTEGER;
BEGIN
  RETURN n*25000 DIV 960;
END mk;

PROCEDURE ShowPins(o: Object; info: INTEGER);
BEGIN
  IF o^.Signal=NIL THEN RETURN END;
  IF Poz>3 THEN Poz:=0; WriteLn END;
  print('%3d-%-15s',o^.No+1,o^.Signal^.Name);
  INC(Poz);
END ShowPins;

PROCEDURE VisChips(o: Object; info: INTEGER);
BEGIN
  IF Tag(o)=chip THEN
    print('%15s',o^.Name);
    IF o^.ChipType#NIL THEN
      print(' - %-15s',o^.ChipType^.Name);
    END;
    print(' : %d %d %d = %d\n',mk(o^.XB),mk(o^.YB),o^.RB,o^.cValue);
    Poz:=0; Iterate(o^.Pins,ShowPins,0);
    IF Poz>0 THEN print('\n') END;
    print(';\n');
  END;
END VisChips;

PROCEDURE ShowPins1(o: Object; info: INTEGER);
BEGIN
  IF o^.Chip=NIL THEN RETURN END;
  IF Poz>4 THEN Poz:=0; WriteLn END;
  IF Poz=0 THEN INC(Poz); WriteString('                 ') END;
  print('%3d %-8s',o^.No+1,o^.Chip^.Name);
  INC(Poz);
END ShowPins1;

PROCEDURE VisTrees(o: Object; info: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF Tag(o)=signal THEN
    print('%-16s ',o^.Name);
    Poz:=1;
    Iterate(o^.TiedPins,ShowPins1,0);
    print('\n');
  END;
END VisTrees;

PROCEDURE Help;
BEGIN
  WriteLn;
  Show('chain <file name> [-cvit]'); HALT
END Help;

VAR e: Reaction;
    create, vis, insert, trees: BOOLEAN;

BEGIN
  ScanFlags;
  create:=Flag?('c'); vis:=Flag?('v'); insert:=Flag?('i'); trees:=Flag?('t');
  IF ArgC()#1 THEN Help; HALT END;
  TakeWord(Name1); Name2:=Name1; AppStr(Name1,'.con');
  IF Exception?(e) THEN
    Show(Message); HALT(1);
  ELSE
    IF create THEN
      IF NOT QueryLn('Create new model ?') THEN HALT END;
      model:=NewObject(chiptype);
      model^.Name:=Name2;
    ELSE
      model:=ReadModel(Name2);
    END;
    IF create OR insert OR NOT (vis OR trees) THEN
      IF Execute(model,Name1)=0 THEN
        IF QueryLn('Write modified model ?') THEN WriteModel(model) END;
      END;
    END;
    IF vis   THEN
      print(': %d %d\n',mk(model^.ctX),mk(model^.ctY));
      Iterate(model^.All,VisChips,0); Show('$')
    END;
    IF trees THEN Iterate(model^.All,VisTrees,0) END;
    KillReaction(e);
  END;
END chain.
