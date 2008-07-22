IMPLEMENTATION MODULE pedTools; (* 13-May-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR;
FROM pedEdit   IMPORT   ReadString;
FROM Keyboard  IMPORT   up, dw, left, right, f1;

IMPORT  m: pedMouse;
IMPORT  str: Strings;
IMPORT  lex: Lexicon;
IMPORT  tty: Terminal;
IMPORT  bio: BIO;

CONST Header=
'Tool |  Size  | Deflect (X,Y) | Vias size | Vias deflect  | Drilling tool';

VAR Line: ARRAY [0..99] OF CHAR;
    Screen, CursorX, CursorY: INTEGER;
    Ch: CHAR;
    cKey: CHAR;

PROCEDURE PackLine(n: INTEGER);
  VAR i: INTEGER; ch: CHAR;
BEGIN
  WITH Tools[n] DO
    i:=Size*2*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    str.print(Line,' %3d |%c%2d.%$3d |',n,ch,i DIV 1000,i MOD 1000);
    i:=DefX*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    str.append(Line,'%c%2d.%$3d ',ch,i DIV 1000,i MOD 1000);
    i:=DefY*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    str.append(Line,'%c%2d.%$3d|',ch,i DIV 1000,i MOD 1000);
    i:=VSize*2*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    str.append(Line,' %c%2d.%$3d   |',ch,i DIV 1000,i MOD 1000);
    i:=VDefX*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    str.append(Line,'%c%2d.%$3d ',ch,i DIV 1000,i MOD 1000);
    i:=VDefY*25000 DIV 960;
    IF i<0 THEN i:=ABS(i); ch:='-' ELSE ch:=' ' END;
    str.append(Line,'%c%2d.%$3d|',ch,i DIV 1000,i MOD 1000);
    str.append(Line,'  %4d',DSize);
  END;
END PackLine;

PROCEDURE WriteLine(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=n-Screen+1;
  IF (i<1)OR(i>23) THEN RETURN END;
  tty.set_pos(i,0); tty.Write(15c); tty.erase_line(0);
  IF (n<0)OR(n>HIGH(Tools)) THEN
  ELSE
    PackLine(n); tty.WriteString(Line);
  END;
END WriteLine;

PROCEDURE InsertLine;
  VAR i: INTEGER;
BEGIN
  tty.set_pos(1,0);
  --IF IL() THEN FOR i:=Screen+1 TO Screen+22 DO WriteLine(i) END END;
  tty.ins_line(1);
END InsertLine;

PROCEDURE DeleteLine;
  VAR i: INTEGER;
BEGIN
  tty.set_pos(1,0);
--  IF DL() THEN FOR i:=Screen+1 TO Screen+22 DO WriteLine(i) END END;
  tty.del_line(1);
END DeleteLine;

PROCEDURE SetCursor;
  VAR i: INTEGER;
BEGIN
  i:=CursorY-Screen+1;
  CASE CursorX OF
    0: tty.set_pos(i,7);
   |1: tty.set_pos(i,16);
   |2: tty.set_pos(i,23);
   |3: tty.set_pos(i,33);
   |4: tty.set_pos(i,44);
   |5: tty.set_pos(i,51);
   |6: tty.set_pos(i,59);
  ELSE
    tty.set_pos(i,0);
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
    DEC(CursorY); tty.up(1);
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
    INC(CursorY); tty.down(1);
  END;
END Down;

PROCEDURE Refresh;
  VAR i: INTEGER;
BEGIN
  tty.set_pos(0,0);
  tty.WriteString(Header);
  FOR i:=Screen TO Screen+22 DO WriteLine(i) END;
  SetCursor;
END Refresh;

PROCEDURE Next;
BEGIN
  cKey:=m.Read(); Ch:=CHAR(cKey);
END Next;

PROCEDURE char?(k: CHAR): BOOLEAN;
BEGIN
  RETURN (k>=40c)&(k<=176c)OR(k>=240c)&(k<=377c);
END char?;

PROCEDURE GetNumber;
  VAR s: ARRAY [0..15] OF CHAR; Cnt,Cnt1,i,j: INTEGER;
      Negativ: BOOLEAN;
BEGIN
  IF (CursorY<0)OR(CursorY>HIGH(Tools)) THEN Next; RETURN END;
  Cnt:=0;
  str.print(s,'');
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
      str.append(s,'  %4d',i);
    ELSE
      str.append(s,'%2d.%$3d',i DIV 1000,i MOD 1000);
    END;
    WHILE char?(cKey) DO
      IF ((Ch>='0')&(Ch<='9'))OR(Ch=' ')OR(Ch='.')OR(Ch='-') THEN
        IF Cnt<6 THEN tty.Write(Ch); s[Cnt]:=Ch; INC(Cnt) END;
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

PROCEDURE ToolsMenu;
  VAR Done: BOOLEAN; cmd,mes: ARRAY [0..79] OF CHAR;
      f: bio.FILE;
BEGIN
  tty.set_pos(0,0); tty.erase(0); Refresh; str.print(cmd,'');
  Next;
  LOOP
    IF cKey=f1 THEN
      Next;
      IF cKey=f1 THEN
        Done:=FALSE;
        tty.set_pos(CursorY-Screen+1,0); tty.erase_line(0);
        ReadString('ped tools> ',cmd); tty.Write(15c);
        Ch:=cmd[0]; str.delete(cmd,0,1); str.append(cmd,'.tools');
        IF Ch='>' THEN
          bio.create(f,cmd,'hw',0);
          IF bio.done THEN bio.write(f,ADR(Tools),SIZE(Tools)*4) END;
          IF bio.done THEN bio.close(f) END;
        ELSIF Ch='<' THEN
          bio.open(f,cmd,'r');
          IF bio.done THEN bio.read(f,ADR(Tools),SIZE(Tools)*4) END;
          IF bio.done THEN bio.close(f) END;
        ELSIF Ch='e' THEN
          Done:=TRUE;
        END;
        IF NOT bio.done THEN
          lex.perror(mes,bio.error,'IO error: %%s.');
          tty.WriteString(mes); Ch:=m.Read()
        END;
        WriteLine(CursorY);
        SetCursor;
        IF Done THEN tty.set_pos(23,0); tty.erase(0); RETURN END;
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
  tty.print("Warning: can't read tools from file ped.tools ...\n");
  FOR i:=0 TO HIGH(Tools) DO
    WITH Tools[i] DO
      Size:=0; VSize:=0; DSize:=0;
      DefX:=0; DefY:=0;
      VDefX:=0; VDefY:=0;
      Magic:=i+0ABCDh;
    END;
  END;
END ClearTools;

VAR
  f : bio.FILE;
  i : INTEGER;

BEGIN
  bio.open(f,'ped.tools','r');
  IF bio.done THEN bio.read(f,ADR(Tools),SIZE(Tools)*4) END;
  IF bio.done THEN bio.close(f) END;
  IF NOT bio.done THEN ClearTools END;
  FOR i:=0 TO HIGH(Tools) DO
    IF Tools[i].Magic#i+0ABCDh THEN ClearTools END;
  END;
  Screen:=0; CursorX:=0; CursorY:=0;
END pedTools.
