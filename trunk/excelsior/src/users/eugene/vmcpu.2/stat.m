MODULE stat; (* 06-Jul-88. (c) KRONOS *)

IMPORT Terminal;
FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM StdIO      IMPORT  print;
FROM mCode      IMPORT  CmdLen, VisCommand;
FROM Code       IMPORT  CodeAtr, GetCatr;
FROM BIO        IMPORT  dOpen, bRead, checkHALT, Close, GetEof, Ls, CD;
FROM FsPublic   IMPORT  File, FileName, dNode;
FROM Pattern    IMPORT  Match;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;

VAR cmd : ARRAY [0..255] OF INTEGER;
    jmp : ARRAY [0..3] OF ARRAY [0..15] OF INTEGER;
    patt: FileName;

PROCEDURE one(d: dNode);
  VAR a    : POINTER TO ARRAY [0..99999] OF CHAR;
      buf  : ARRAY [0..4095] OF INTEGER;
      e,i,n: INTEGER;
      f    : File;
BEGIN
  IF d.Name[0]=0c  THEN RETURN END;
  IF d.Name[0]='.' THEN RETURN END;
  IF NOT Match(patt,d.Name) THEN RETURN END;
  Terminal.print('%s\r',d.Name);
  checkHALT(dOpen(d,f),d.Name);
  e:=GetEof(f);
  IF e>SIZE(buf)*4 THEN Terminal.print('Too long: %s\n',d.Name); HALT END;
  checkHALT(bRead(f,0,ADR(buf),e),d.Name);
  checkHALT(Close(f),d.Name);
  a:=ADDRESS(GetCatr(ADR(buf),CodeAdr))+ADDRESS(GetCatr(ADR(buf),NoPrc));
  e:=INTEGER(GetCatr(ADR(buf),CodeSz ))-INTEGER(GetCatr(ADR(buf),NoPrc));
  i:=0; e:=e*4;
  WHILE i<e DO
    INC(cmd[ORD(a^[i])]);
    n:=CmdLen[ORD(a^[i])];
    IF n>=0 THEN INC(i,n+1);
    ELSIF n=-1 THEN INC(i,ORD(a^[i+1])+1);
    ELSE Terminal.print('Illegal cmd: %s[%d]=%2$h\n',d.Name,i,a^[i]); HALT;
    END;
  END;
  Terminal.ClearLine;
END one;

PROCEDURE vis;
  VAR i: INTEGER;
      mnem: ARRAY [0..7] OF CHAR;
BEGIN
  FOR i:=0 TO 255 DO
    IF cmd[i]>0 THEN
      VisCommand(i,mnem);
      print('%s %4d\n',mnem,cmd[i]);
    END;
  END;
END vis;

VAR i,j: INTEGER;

BEGIN
  ScanFlags;
  TakeWord(patt);
  IF (patt='') OR Flag?('h') THEN
    print('stat <имя кодофайла>\n');
    print('  выдает на стандартный вывод статистику использования\n');
    print('  команд для указанных кодофайлов.\n');
    HALT;
  END;
  FOR i:=0 TO HIGH(cmd) DO cmd[i]:=0 END;
  FOR i:=0 TO HIGH(jmp) DO
    FOR j:=0 TO HIGH(jmp[i]) DO jmp[i][j]:=0 END;
  END;
  checkHALT(Ls(CD(),one,FALSE),'Ls CD');
  vis;
END stat.
