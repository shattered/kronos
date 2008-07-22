MODULE tm; (* 13-Feb-87. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Model      IMPORT  Object, String;
FROM ModelIO    IMPORT  ReadModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM cdsEmul    IMPORT  Emulate, CalculateState;

VAR e           : Reaction;
    Name        : String;
    mdl         : Object;

BEGIN
  ScanFlags; TakeWord(Name);
  IF (Name[0]=0c)OR Flag?('h') THEN print('tm <model name>\n'); HALT END;
  IF Exception?(e) THEN print('%s\n',Message); HALT END;
  mdl:=ReadModel(Name);
  CalculateState(mdl); Emulate(mdl);
END tm.
