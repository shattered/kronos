MODULE ped; (* 13-Feb-87. (c) KRONOS *)

FROM Edit      IMPORT   ReadString;
FROM Terminal  IMPORT   SetMode, print, Write, ClearLine, WriteLn, CR, Read;
FROM Args      IMPORT   TakeWord;
FROM ModelPbl  IMPORT   Exception?, Reaction, Message;
FROM pedScreen  IMPORT  CloseWindow;
FROM pedEditor IMPORT   Editor;
FROM pedTools  IMPORT   ToolsMenu;
FROM pedModel   IMPORT  ReadModel, board, WriteModel, string;
FROM Strings   IMPORT   GetWord, Str1;

PROCEDURE Query(s: ARRAY OF CHAR): BOOLEAN;
  VAR Ch: CHAR;
BEGIN
  print('%s ',s);
  REPEAT Ch:=Read() UNTIL (Ch='y')OR(Ch='Y')OR(Ch='n')OR(Ch='N');
  print('%c\n',Ch);
  RETURN (Ch='y')OR(Ch='Y');
END Query;

PROCEDURE Help;
BEGIN
  print('v        - edit board\nc        - cansel\n'
        'r <name> - read model\nw <name> - write model\n'
        'e        - write model & exit\nn        - remove model from memory\n'
        't        - tools menu\nh        - help\n');
END Help;

VAR e      : Reaction;
    Name   : string;
    mdl    : board;
    Cmd    : ARRAY [0..70] OF CHAR;
    CmdName: string;
    MustBeRead: BOOLEAN;

BEGIN
--SetMode(FALSE);
  TakeWord(Name); MustBeRead:=Name[0]#0c; mdl:=NIL;
  LOOP
    IF Exception?(e) THEN
      CloseWindow; print('%s\n',Message);
    ELSE
      LOOP
        IF MustBeRead THEN
          MustBeRead:=FALSE;
--        IF mdl#NIL THEN RemoveModel(mdl) END;
          mdl:=ReadModel(Name);
        END;
        ReadString('ped>',Cmd); print('\n');
        GetWord(Cmd,CmdName);
        IF CmdName[0]#0c THEN
          CASE CmdName[0] OF
            'v': Editor(mdl);
           |'c': IF Query('Cansel?') THEN HALT END;
           |'r': GetWord(Cmd,Name); MustBeRead:=TRUE;
           |'w': IF mdl#NIL THEN
                   GetWord(Cmd,CmdName); Str1(mdl^.name,CmdName);
                   print('Write to file %s',mdl^.name);
                   IF Query('?') THEN WriteModel(mdl^.name,mdl) END;
                 ELSE
                   print('No model for write.\n');
                 END;
           |'e': IF mdl#NIL THEN WriteModel(mdl^.name,mdl) END; HALT;
           |'n': IF (mdl#NIL)&Query('Remove model?') THEN mdl:=NIL         END;
--         |'n': IF (mdl#NIL)&Query('Remove model?') THEN RemoveModel(mdl) END;
           |'t': ToolsMenu;
           |'h': Help;
          ELSE print('Illegal command.\n');
          END;
        END;
      END;
    END;
  END;
END ped.
