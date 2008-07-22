MODULE brd; (* 13-Feb-87. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Model      IMPORT  Object, Objects, Iterate, NewObject, Tie, String;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM BRDout     IMPORT  WriteBRD;

VAR e: Reaction;
    Name: String;
    mdl: Object;

BEGIN
  ScanFlags;
  TakeWord(Name);
  IF Exception?(e) THEN print('%s\n',Message); HALT END;
  IF Flag?('r') THEN
    mdl:=NewObject(chiptype);
    mdl^.Name:=Name;
HALT;
  ELSIF Flag?('w') THEN
    mdl:=ReadModel(Name);
    WriteBRD(mdl);
  ELSE
    print('brd <name> -r\n');
    print('  чтение brd файла\n');
    print('brd <name> -w\n');
    print('  запись brd файла\n');
  END;
END brd.
