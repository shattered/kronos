MODULE vs; (* Sem 10-Feb-87. (c) KRONOS *)

FROM StdIO     IMPORT   AskStr, StdOut, WriteLn;
FROM Model     IMPORT   Iterate, Object, String;
FROM ModelIO   IMPORT   ReadModel;
FROM VisModel  IMPORT   VisObject, Output;

VAR Name: String;
    m   : Object;

BEGIN
  AskStr('File name: ',Name); WriteLn;
  m:=ReadModel(Name);
  Output:=StdOut;
  VisObject(m);
  Iterate(m^.All,VisObject);
END vs.
