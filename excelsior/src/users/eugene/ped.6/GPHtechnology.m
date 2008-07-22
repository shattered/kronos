IMPLEMENTATION MODULE GPHtechnology; (* 19-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM FsPublic   IMPORT  FileName, File;
FROM ASCII      IMPORT  NL;
FROM BIO        IMPORT  bWrite, Create, Link, CD, checkHALT, Close, SetEof,
                        bRead, OpenOnDir, GetEof;
FROM Terminal   IMPORT  print;
FROM Image      IMPORT  PeekNum, image0;

VAR buf          : ARRAY [0..4095] OF CHAR;
    bufcnt,bufblk: INTEGER;
    Out          : File;
    OutName      : FileName;
    eof          : INTEGER;

PROCEDURE w(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE s[i]#0c DO
    IF bufcnt=4096 THEN
      checkHALT(bWrite(Out,bufblk,ADR(buf),bufcnt),OutName);
      INC(bufblk); bufcnt:=0;
    END;
    buf[bufcnt]:=s[i]; INC(bufcnt); INC(i);
  END;
END w;

PROCEDURE Save(nm: ARRAY OF CHAR);
  VAR ln: ARRAY [0..79] OF CHAR; i: INTEGER;
BEGIN
  bufblk:=0; bufcnt:=0;
  image0(OutName,'%s.technology',nm);
  checkHALT(Create(Out),OutName);
  bufblk:=0; bufcnt:=0;
  image0(ln,'Pins\n'); w(ln);
  image0(ln,'-- diameter0 diameter1\n'); w(ln);
  FOR i:=0 TO 15 DO
    image0(ln,'%2d %9d %9d\n',i,Pins[i].diameter0,Pins[i].diameter1);
    w(ln);
  END;
  image0(ln,'\nTracks\n'); w(ln);
  image0(ln,'-- diameter displaceX displaceY\n'); w(ln);
  FOR i:=0 TO 15 DO
    image0(ln,'%2d %8d %9d %9d\n',i,Tracks[i].diameter,
           Tracks[i].displaceX, Tracks[i].displaceY);
    w(ln);
  END;
  image0(ln,'\nVias\n'); w(ln);
  image0(ln,'-- type dril displaceX displaceY\n'); w(ln);
  FOR i:=0 TO 15 DO
    image0(ln,'%2d %4d %4d %9d %9d\n',i,Vias[i].type, Vias[i].dril,
           Vias[i].displaceX, Vias[i].displaceY);
    w(ln);
  END;
  image0(ln,'\nRouterMode %5d\n',RouterMode); w(ln);
  image0(ln,'RoutGrid   %5d %5d\n',RoutGrid[0],RoutGrid[1]); w(ln);
  image0(ln,'ViasGrid   %5d\n',ViasGrid); w(ln);
  image0(ln,'Clearance  %5d\n',Clearance); w(ln);
  image0(ln,'Resist     %5d\n',Resist); w(ln);
  IF bufcnt>0 THEN
    checkHALT(bWrite(Out,bufblk,ADR(buf),bufcnt),OutName);
  END;
  SetEof(Out,bufblk*4096+bufcnt);
  checkHALT(Link(CD(),OutName,Out),OutName);
  checkHALT(Close(Out),OutName);
END Save;

PROCEDURE error;
BEGIN
  print('Error in technology file.\n'); HALT(1);
END error;

PROCEDURE GetCh(): CHAR;
  VAR ch: CHAR;
BEGIN
  IF bufblk*4096+bufcnt=eof THEN RETURN NL END;
  IF bufcnt=4096 THEN
    INC(bufblk); bufcnt:=0;
    checkHALT(bRead(Out,bufblk,ADR(buf),4096),OutName);
  END;
  ch:=buf[bufcnt]; INC(bufcnt);
  RETURN ch;
END GetCh;

PROCEDURE NextLn;
BEGIN
  REPEAT UNTIL GetCh()=NL;
END NextLn;

PROCEDURE GetWord(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER; ch: CHAR;
BEGIN
  LOOP
    REPEAT ch:=GetCh() UNTIL (ch#' ')&(ch#NL);
    i:=0;
    REPEAT s[i]:=ch; ch:=GetCh(); INC(i) UNTIL (i>=HIGH(s))OR(ch=' ')OR(ch=NL);
    s[i]:=0c;
    IF s#'--' THEN RETURN END;
    NextLn;
  END;
END GetWord;

PROCEDURE GetNum(): INTEGER;
  VAR ln: ARRAY [0..79] OF CHAR; i,n: INTEGER;
BEGIN
  GetWord(ln);
  i:=PeekNum(ln,0,n);
  IF (i<0)OR(ln[i]#0c) THEN error END;
  RETURN n;
END GetNum;

PROCEDURE Restore(nm: ARRAY OF CHAR);
  VAR ln: ARRAY [0..79] OF CHAR; i: INTEGER;
BEGIN
  bufblk:=0; bufcnt:=0;
  image0(OutName,'%s.technology',nm);
  checkHALT(OpenOnDir(CD(),Out,OutName),OutName);
  bufblk:=-1; bufcnt:=4096;
  eof:=GetEof(Out);
  WHILE bufblk*4096+bufcnt#eof DO
    GetWord(ln);
    IF ln='Tracks' THEN
      FOR i:=0 TO 15 DO
        IF i#GetNum() THEN error END;
        Tracks[i].diameter:=GetNum();
        Tracks[i].displaceX:=GetNum();
        Tracks[i].displaceY:=GetNum();
      END;
    ELSIF ln='Pins' THEN
      FOR i:=0 TO 15 DO
        IF i#GetNum() THEN error END;
        Pins[i].diameter0:=GetNum();
        Pins[i].diameter1:=GetNum();
      END;
    ELSIF ln='Vias' THEN
      FOR i:=0 TO 15 DO
        IF i#GetNum() THEN error END;
        Vias[i].type:=GetNum();
        Vias[i].dril:=GetNum();
        Vias[i].displaceX:=GetNum();
        Vias[i].displaceY:=GetNum();
      END;
    ELSIF ln='RouterMode' THEN
      RouterMode:=BITSET(GetNum());
    ELSIF ln='RoutGrid' THEN
      RoutGrid[0]:=GetNum(); RoutGrid[1]:=GetNum();
    ELSIF ln='ViasGrid' THEN
      ViasGrid:=GetNum();
    ELSIF ln='Clearance' THEN
      Clearance:=GetNum();
    ELSIF ln='Resist' THEN
      Resist:=GetNum();
    ELSE
      error;
    END;
  END;
  checkHALT(Close(Out),OutName);
END Restore;

VAR i: INTEGER;

BEGIN
  RouterMode:={};
  Resist:=0;
  Clearance:=0;
  ViasGrid:=0;
  FOR i:=0 TO 15 DO
    WITH Tracks[i] DO
      diameter:=0; displaceX:=0; displaceY:=0;
    END;
    WITH Pins  [i] DO
      diameter0:=0; diameter1:=0;
    END;
    WITH Vias  [i] DO
      type:=0; dril:=0; displaceX:=0; displaceY:=0;
    END;
  END;
END GPHtechnology.
