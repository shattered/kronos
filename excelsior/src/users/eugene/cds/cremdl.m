MODULE cremdl; (* 20-Apr-88. (c) KRONOS *)

FROM Model     IMPORT   Object, Objects, NewObject, Tie, Lset, String;
FROM ModelIO   IMPORT   WriteModel;
FROM Args      IMPORT   TakeWord, NumFlag?, ScanFlags, Flag?;
FROM Terminal  IMPORT   print, Read;

PROCEDURE Help;
BEGIN
  print('Ключи:\n');
  print('p=n  количество выводов;\n');
  print('d=n  двурядный корпус, расстояние между рядами n*2.5mm;\n');
  print('s=n  однорядный корпус, расстояние между выводами n*1.25mm;\n');
  print('x=n  длина корпуса n*1.25mm;\n');
  print('y=n  ширина корпуса n*1.25mm;\n');
  print('-q   отмена запроса подтверждения.\n');
END Help;

VAR mdl,p: Object;
    i,n,s: INTEGER;
    Name : String;
    x,y,d: INTEGER;
    ch   : CHAR;

BEGIN
  ScanFlags;
  IF Flag?('h') THEN Help; HALT END;
  IF NumFlag?('p',n) THEN
    print('Укажите количество выводов: p=...\n'); HALT
  END;
  IF NumFlag?('x',x) THEN x:=-1 END;
  IF NumFlag?('y',y) THEN y:=-1 END;
  IF NumFlag?('d',d) THEN d:=-1 END;
  IF NumFlag?('s',s) THEN s:=-1 END;
  TakeWord(Name);
  IF Name[0]=0c THEN
    print('Укажите имя модели.\n'); HALT
  END;
  IF Flag?('q') THEN
    print('%s\n',Name)
  ELSE
    print('Создать модель %s? ',Name);
    LOOP
      ch:=Read();
      IF (ch='y')OR(ch='Y') THEN EXIT END;
      IF (ch='n')OR(ch='N') THEN HALT END;
      print('%c',7c);
    END;
    print('%c\n',ch);
  END;
  mdl:=NewObject(chiptype);
  mdl^.Name:=Name;
  IF x>0 THEN mdl^.ctX:=x*48 END;
  IF y>0 THEN mdl^.ctY:=y*48 END;
  FOR i:=0 TO n-1 DO
    p:=NewObject(externalpin);
    Tie(mdl^.All,p);
    Lset(mdl^.ExternalPins,i,p);
    p^.EPinNo:=i;
    p^.Host:=mdl;
    IF d>0 THEN
      IF i<n DIV 2 THEN
        p^.PinX:=i*96; p^.PinY:=0;
      ELSE
        p^.PinX:=(n-i-1)*96; p^.PinY:=d*96;
      END;
    ELSIF s>0 THEN
      p^.PinX:=i*48*s; p^.PinY:=0;
    END;
  END;
  WriteModel(mdl);
END cremdl.
