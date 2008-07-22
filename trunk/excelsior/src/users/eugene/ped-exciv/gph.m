MODULE gph; (* 13-Feb-87. (c) KRONOS *)

IMPORT GPHtechnology, GPHvis;
FROM Terminal   IMPORT  print;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Model      IMPORT  Object, Objects, Iterate, NewObject, Tie, String;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM GPHin      IMPORT  DoGPH;
FROM GPHout     IMPORT  WriteGPH;
FROM Strings    IMPORT  AppStr, Str1;
FROM Edit       IMPORT  ReadString;

VAR e: Reaction;
    Name,GPHName,tName: String;
    mdl: Object;

BEGIN
  ScanFlags;
  TakeWord(Name);
  IF Exception?(e) THEN print('%s\n',Message); HALT END;
  IF Flag?('r') THEN
    mdl:=NewObject(chiptype);
    mdl^.Name:=Name;
    GPHName:=Name; AppStr(GPHName,'.gph');
    DoGPH(mdl,GPHName);
    WriteModel(mdl);
    IF Flag?('t') THEN GPHtechnology.Save(Name) END;
  ELSIF Flag?('w') THEN
    IF Flag?('t') THEN
      Str1(tName,Name);
      ReadString('Имя файла с описанием технологии: ',tName);
      print('\n');
      GPHtechnology.Restore(tName)
    END;
    mdl:=ReadModel(Name);
    GPHName:=Name; AppStr(GPHName,'.gph');
    WriteGPH(GPHName,mdl);
  ELSIF Flag?('v') THEN
    GPHName:=Name; AppStr(GPHName,'.gph');
    GPHvis.DoGPH(GPHName); HALT;
  ELSE
    print('gph <name> -rt\n');
    print('  чтение gph файла\n');
    print('gph <name> -wt\n');
    print('  запись gph файла\n');
  END;
END gph.
