IMPLEMENTATION MODULE abcDefs; (* Hady. 24-Apr-88. (c) KRONOS *)

FROM Streams    IMPORT  Why?;
FROM Strings    IMPORT  AppStr, Str0;

PROCEDURE VisError(err: INTEGER; VAR s: ARRAY OF CHAR);
  VAR t: ARRAY [0..79] OF CHAR;
BEGIN
  IF err<0 THEN
    IF err=NoSuchWord THEN AppStr(s,'Неизвестное слово')
    ELSIF err=NoMemory THEN AppStr(s,'Нет памяти в буффере')
    ELSIF err=EmptyBuf THEN AppStr(s,'Буффер пуст')
    ELSIF err=NotOpened THEN AppStr(s,'Обращение к неоткрытому словарю')
    ELSIF err=ExistOpened THEN AppStr(s,'Словарь уже открыт')
    ELSE Str0(t); Why?(err,s); AppStr(s,t);
    END;
  END;
END VisError;

END abcDefs.
