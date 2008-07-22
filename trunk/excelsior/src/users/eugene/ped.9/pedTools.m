IMPLEMENTATION MODULE pedTools; (* 13-May-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR;
FROM FsPublic  IMPORT   File, FileName;
FROM BIO       IMPORT   LookUpFile, bRead, Close, Create, Link, check,
                        bWrite, CD, SetEof;
FROM Terminal  IMPORT   WriteString, Show, SetCrs, Home, Clear, Read,
                        IL, DL, Write, Up, Dw, Left, Right, ClearLine;
FROM Edit      IMPORT   ReadString;
FROM Image     IMPORT   image;
FROM Strings   IMPORT   Str0, Str1, DelCh, AppStr;
FROM Keyboard  IMPORT   ReadKey, up, dw, left, right, gold;

CONST Header=
'Tool |  Size  | Deflect (X,Y) | Vias size | Vias deflect  | Drilling tool';

VAR Line: ARRAY [0..99] OF CHAR;
    Screen, CursorX, CursorY: CARDINAL;
    Ch: CHAR;
    cKey: CHAR;

PROCEDURE PackLine(n: CARDINAL);
  VAR i: CARDINAL; ch: CHAR;
BEGIN
  WITH Tools[n] DO
    Str0(Line);
    image(Line,' %3d |',n);
    i:=Size*2*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    image(Line,'%c%2d.%$3d |',ch,i DIV 1000,i MOD 1000);
    i:=DefX*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    image(Line,'%c%2d.%$3d ',ch,i DIV 1000,i MOD 1000);
    i:=DefY*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    image(Line,'%c%2d.%$3d|',ch,i DIV 1000,i MOD 1000);
    i:=VSize*2*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    image(Line,' %c%2d.%$3d   |',ch,i DIV 1000,i MOD 1000);
    i:=VDefX*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    image(Line,'%c%2d.%$3d ',ch,i DIV 1000,i MOD 1000);
    i:=VDefY*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    image(Line,'%c%2d.%$3d|',ch,i DIV 1000,i MOD 1000);
    image(Line,'  %4d',DSize);
  END;
END PackLine;

PROCEDURE WriteLine(n: CARDINAL);
  VAR i: CARDINAL;
BEGIN
  i:=n-Screen+1;
  IF (i<1)OR(i>23) THEN RETURN END;
  SetCrs(i,0); Write(15c); ClearLine;
  IF (n<0)OR(n>HIGH(Tools)) THEN
  ELSE
    PackLine(n); WriteString(Line);
  END;
END WriteLine;

PROCEDURE InsertLine;
  VAR i: CARDINAL;
BEGIN
  SetCrs(1,0);
  IF IL() THEN FOR i:=Screen+1 TO Screen+22 DO WriteLine(i) END END;
END InsertLine;

PROCEDURE DeleteLine;
  VAR i: CARDINAL;
BEGIN
  SetCrs(1,0);
  IF DL() THEN FOR i:=Screen+1 TO Screen+22 DO WriteLine(i) END END;
END DeleteLine;

PROCEDURE SetCursor;
  VAR i: CARDINAL;
BEGIN
  i:=CursorY-Screen+1;
  CASE CursorX OF
    0: SetCrs(i,7);
   |1: SetCrs(i,16);
   |2: SetCrs(i,23);
   |3: SetCrs(i,33);
   |4: SetCrs(i,44);
   |5: SetCrs(i,51);
   |6: SetCrs(i,59);
  ELSE
    SetCrs(i,0);
  END;
END SetCursor;

PROCEDURE mUp;
BEGIN
  IF CursorY=Screen THEN
    DEC(CursorY); DEC(Screen);
    InsertLine;
    WriteLine(Screen);
    SetCursor;
  ELSE
    DEC(CursorY); Up;
  END;
END mUp;

PROCEDURE Down;
BEGIN
  IF CursorY=Screen+22 THEN
    INC(CursorY); INC(Screen);
    DeleteLine;
    WriteLine(Screen+22);
    SetCursor;
  ELSE
    INC(CursorY); Dw;
  END;
END Down;

PROCEDURE Refresh;
  VAR i: CARDINAL;
BEGIN
  Home;
  WriteString(Header);
  FOR i:=Screen TO Screen+22 DO WriteLine(i) END;
  SetCursor;
END Refresh;

PROCEDURE Next;
BEGIN
  cKey:=ReadKey(); Ch:=CHAR(cKey);
END Next;

PROCEDURE char?(k: CHAR): BOOLEAN;
BEGIN
  RETURN (k>=40c)&(k<=176c)OR(k>=240c)&(k<=377c);
END char?;

PROCEDURE GetNumber;
  VAR s: ARRAY [0..15] OF CHAR; Cnt,Cnt1,i,j: CARDINAL;
      Negativ: BOOLEAN;
BEGIN
  IF (CursorY<0)OR(CursorY>HIGH(Tools)) THEN Next; RETURN END;
  Cnt:=0;
  Str0(s);
  WITH Tools[CursorY] DO
    CASE CursorX OF
      0: i:=Size*25000*2 DIV 960;
     |1: i:=DefX*25000 DIV 960;
     |2: i:=DefY*25000 DIV 960;
     |3: i:=VSize*25000*2 DIV 960;
     |4: i:=VDefX*25000 DIV 960;
     |5: i:=VDefY*25000 DIV 960;
     |6: i:=DSize;
    ELSE
    END;
    IF CursorX=6 THEN
      image(s,'  %4d',i);
    ELSE
      image(s,'%2d.%$3d',i DIV 1000,i MOD 1000);
    END;
    WHILE char?(cKey) DO
      IF ((Ch>='0')&(Ch<='9'))OR(Ch=' ')OR(Ch='.')OR(Ch='-') THEN
        IF Cnt<6 THEN Write(Ch); s[Cnt]:=Ch; INC(Cnt) END;
      END;
      Next;
    END;
    SetCursor;
    i:=0; j:=0; Cnt:=0;
    WHILE s[Cnt]=' ' DO INC(Cnt) END;
    Negativ:=s[Cnt]='-'; IF Negativ THEN INC(Cnt) END;
    WHILE (s[Cnt]>='0')&(s[Cnt]<='9') DO
      i:=i*10+ORD(s[Cnt])-ORD('0');
      INC(Cnt);
    END;
    IF s[Cnt]='.' THEN
      INC(Cnt); Cnt1:=0;
      WHILE (s[Cnt]>='0')&(s[Cnt]<='9') DO
        j:=j*10+ORD(s[Cnt])-ORD('0');
        INC(Cnt); INC(Cnt1);
      END;
      WHILE Cnt1>3 DO j:=j DIV 10; DEC(Cnt1) END;
      WHILE Cnt1<3 DO j:=j *   10; INC(Cnt1) END;
    END;
    FOR Cnt:=Cnt TO 5 DO
      IF s[Cnt]#' ' THEN
        WriteLine(CursorY);
        RETURN;
      END
    END;
    i:=i MOD 100;
    i:=i*1000+j; j:=(i*960+12500) DIV 25000;
    IF Negativ THEN j:=-j END;
    CASE CursorX OF
      0: Size:=j DIV 2;
     |1: DefX:=j;
     |2: DefY:=j;
     |3: VSize:=j DIV 2;
     |4: VDefX:=j;
     |5: VDefY:=j;
     |6: DSize:=i DIV 1000;
    ELSE
    END;
    WriteLine(CursorY);
    SetCursor;
  END;
END GetNumber;

PROCEDURE Dummy(s: ARRAY OF CHAR);
BEGIN END Dummy;

PROCEDURE Eof(f: File): BOOLEAN;
BEGIN
  SetEof(f,SIZE(Tools)*4);
  RETURN FALSE;
END Eof;

PROCEDURE ToolsMenu;
  VAR Done,r: BOOLEAN; cmd,mes: FileName;
      f: File;
BEGIN
  Home; Clear; Refresh; Str0(cmd);
  Next;
  LOOP
    IF cKey=gold THEN
      Next;
      IF cKey=gold THEN
        Done:=FALSE;
        SetCrs(CursorY-Screen+1,0); ClearLine;
        ReadString('ped tools> ',cmd); Write(15c);
        Ch:=cmd[0]; DelCh(cmd,0); AppStr(cmd,'.tools');
        r:=FALSE;
        IF Ch='>' THEN
          r:=Create(f)OR bWrite(f,0,ADR(Tools),SIZE(Tools)*4)
            OR Link(CD(),cmd,f)OR Eof(f) OR Close(f);
        ELSIF Ch='<' THEN
          r:=LookUpFile(f,cmd,Dummy)OR bRead(f,0,ADR(Tools),SIZE(Tools)*4)
            OR Close(f);
        ELSIF Ch='e' THEN
          Done:=TRUE;
        END;
        IF r THEN r:=check(r,mes); WriteString(mes); Ch:=Read() END;
        WriteLine(CursorY);
        SetCursor;
        IF Done THEN SetCrs(23,0); Clear; RETURN END;
      END;
      Next;
    ELSE
      CASE cKey OF
        up   : mUp; Next;
       |dw   : Down; Next;
       |left : IF CursorX>0 THEN DEC(CursorX); SetCursor END; Next;
       |right: IF CursorX<6 THEN INC(CursorX); SetCursor END; Next;
      ELSE
        IF char?(cKey) THEN
          GetNumber;
        ELSE
          Next;
        END;
      END;
    END;
  END;
END ToolsMenu;

PROCEDURE ClearTools;
  VAR i: INTEGER;
BEGIN
  Show("Warning: can't read tools from file ped.tools ...");
  FOR i:=0 TO HIGH(Tools) DO
    WITH Tools[i] DO
      Size:=0; VSize:=0; DSize:=0;
      DefX:=0; DefY:=0;
      VDefX:=0; VDefY:=0;
      Magic:=i+0ABCDh;
    END;
  END;
END ClearTools;

VAR nm: ARRAY [0..15] OF CHAR;
    f: File;
    i: CARDINAL;

BEGIN
  Str1(nm,'ped.tools');
  IF LookUpFile(f,nm,Dummy)OR bRead(f,0,ADR(Tools),SIZE(Tools)*4)OR
    Close(f) THEN
    ClearTools;
  END;
  FOR i:=0 TO HIGH(Tools) DO
    IF Tools[i].Magic#i+0ABCDh THEN ClearTools END;
  END;
  Screen:=0; CursorX:=0; CursorY:=0;
END pedTools.
