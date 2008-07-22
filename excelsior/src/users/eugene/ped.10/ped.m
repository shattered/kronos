MODULE ped; (* 13-Feb-87. (c) KRONOS *)

FROM Edit      IMPORT   ReadString;
FROM Terminal  IMPORT   SetMode, print, Write, ClearLine, WriteLn, CR, Read;
FROM Args      IMPORT   TakeWord;
FROM Model     IMPORT   Object, Objects, RemoveModel;
FROM ModelIO   IMPORT   ReadModel, WriteModel;
FROM ModelPbl  IMPORT   Exception?, Reaction, Message;
FROM pedScreen  IMPORT  CloseWindow;
FROM pedEditor IMPORT   Editor;
FROM pedTools  IMPORT   ToolsMenu;
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
    Name   : ARRAY [0..80] OF CHAR;
    mdl    : Object;
    Cmd    : ARRAY [0..70] OF CHAR;
    CmdName: ARRAY [0..70] OF CHAR;
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
          IF mdl#NIL THEN RemoveModel(mdl) END;
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
                   GetWord(Cmd,CmdName);
                   IF CmdName[0]#0c THEN Str1(mdl^.Name,CmdName) END;
                   print('Write to file %s',mdl^.Name);
                   IF Query('?') THEN WriteModel(mdl) END;
                 ELSE
                   print('No model for write.\n');
                 END;
           |'e': IF mdl#NIL THEN WriteModel(mdl) END; HALT;
           |'n': IF (mdl#NIL)&Query('Remove model?') THEN RemoveModel(mdl) END;
           |'t': ToolsMenu;
           |'h': Help;
          ELSE print('Illegal command.\n');
          END;
        END;
      END;
    END;
  END;
END ped.
