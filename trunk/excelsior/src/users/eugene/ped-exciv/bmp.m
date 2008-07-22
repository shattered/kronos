MODULE bmp; (* Sem 15-Nov-87. (c) KRONOS *)

FROM Terminal   IMPORT  Show;
FROM bmpGrafic  IMPORT  Print, PrintPins, PrintChips;
FROM Toshiba    IMPORT  PrintWindow;
FROM Model      IMPORT  Object;
FROM ModelIO    IMPORT  ReadModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM Args       IMPORT  ScanFlags, TakeWord, Flag?;

VAR e: Reaction;
    Name,Sig: ARRAY [0..79] OF CHAR;
    mdl: Object;

BEGIN
  ScanFlags; TakeWord(Name); TakeWord(Sig);
  IF Exception?(e) THEN Show(Message); HALT END;
  IF (Name[0]=0c)OR Flag?('h') THEN
    Show('bmp <model name> -p  печать отрисованного файла.');
    Show('bmp <model name> -c  отрисовка оотсутствующих связей.');
    Show('bmp <model name> -d  отрисовка сборочного чертежа.');
    Show('bmp <model name>     отрисовка метализации.');
    Show('bmp <model name> <signal> отрисовка метализации сигнала.');
    HALT;
  ELSIF Flag?('p') THEN PrintWindow(Name);
  ELSIF Flag?('c') THEN mdl:=ReadModel(Name); PrintPins(mdl,4);
  ELSIF Flag?('d') THEN mdl:=ReadModel(Name); PrintChips(mdl,4);
  ELSE mdl:=ReadModel(Name); IF Sig='' THEN Sig:='*' END; Print(mdl,4,Sig);
  END;
END bmp.
