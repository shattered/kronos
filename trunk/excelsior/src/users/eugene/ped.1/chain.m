MODULE chain; (* 08-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS, ADR;
FROM StdIO     IMPORT   WriteLn, QueryLn, Show, print, Write, WriteString;
FROM Args      IMPORT   ScanFlags, Flag?, TakeWord, ArgC;
FROM Strings   IMPORT   AppStr;

FROM pedChain  IMPORT   Execute;
FROM pedModel   IMPORT  string, board, ReadModel, WriteModel,
                        pin, signal, chip, cre_board;

VAR Name1: string;
    Name2: string;
    model: board;

PROCEDURE VisChip(c: chip);
  VAR i,p: INTEGER; pn: pin;
BEGIN
  print('%15s',c^.name);
  IF c^.type#NIL THEN print(' - %-15s',c^.type^.name) END;
  print(' : %d %d %d = %d\n',c^.x,c^.y,c^.r,0);
  p:=0;
  FOR i:=0 TO c^.pno-1 DO
    pn:=ADR(c^.pins[i]);
    IF p>3 THEN p:=0; print('\n') END;
    print('%3d-%-15s',pn^.no+1,pn^.sig^.name);
    INC(p);
  END;
  IF p>0 THEN print('\n') END;
  print(';\n');
END VisChip;

PROCEDURE VisTree(s: signal);
  VAR i,p: INTEGER; pn: pin;
BEGIN
  print('%s\n',s^.name);
  p:=0;
  FOR i:=0 TO s^.pno-1 DO
    IF p>3 THEN p:=0; print('\n') END;
    IF p=0 THEN INC(p); print('        ') END;
    pn:=s^.pins^[i];
    print('%4d %-14s',pn^.no+1,pn^.chp^.name);
    INC(p);
  END;
  IF p>0 THEN print('\n') END;
  FOR i:=0 TO 67 DO print('-') END; print('\n');
END VisTree;

PROCEDURE Help;
BEGIN
  WriteLn;
  Show('chain <file name> [-cvit]'); HALT
END Help;

VAR create, vis, insert, trees: BOOLEAN; i: INTEGER;

BEGIN
  ScanFlags;
  create:=Flag?('c'); vis:=Flag?('v'); insert:=Flag?('i'); trees:=Flag?('t');
  IF ArgC()#1 THEN Help; HALT END;
  TakeWord(Name1); Name2:=Name1; AppStr(Name1,'.con');
  IF create THEN
    IF NOT QueryLn('Create new model ?') THEN HALT END;
    cre_board(model);
    model^.name:=Name2;
  ELSE
    model:=ReadModel(Name2);
  END;
  IF create OR insert OR NOT (vis OR trees) THEN
    Execute(model,Name1);
    IF QueryLn('Write modified model ?') THEN WriteModel(Name2,model) END;
  END;
  IF vis   THEN
    FOR i:=0 TO model^.cno-1 DO VisChip(model^.chps^[i]) END;
    print('$\n');
  END;
  IF trees THEN
    FOR i:=0 TO model^.sno-1 DO VisTree(model^.sigs^[i]) END;
  END;
END chain.
