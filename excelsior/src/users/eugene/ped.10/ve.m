MODULE ve; (* 19-Jan-87. (c) KRONOS *)

IMPORT Model;
FROM Terminal   IMPORT  print, SetMode, Write, Read;
FROM Misc       IMPORT  Bold, Control?;
FROM Args       IMPORT  ScanFlags, Flag?, TakeWord, ArgC;
FROM SYSTEM     IMPORT  ADDRESS;
FROM VIDEO      IMPORT  start, finish, fill, mode, color, com, add, rep,
                        clipE, clipW, clipN, clipS, dot;
FROM vePbl      IMPORT  X, Y, Root, CurPict, Mdl, curpX, curpY, WndX, WndY,
                        Scale;
FROM veSeg      IMPORT  New, Tie, Level, Ls, AppLine, Drow, DrowRingTree,
                        DrowTree, vect, cursor, CopyTree;
FROM Model      IMPORT  Object, String, NewObject, Objects, Iterate, Tag;
FROM ModelPbl   IMPORT  Reaction, Exception?, Message;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM Resource   IMPORT  Final;

FROM Keyboard  IMPORT   ReadKey, PeekKey, cr, lf, space, empty,
                        up, dw, left, right, delc, insc, delln, insln,
                        uppg, dwpg, newln, dupln, rtab, ltab, upswp, dwswp,
                        lpull, rpull, erase, eraln, end, home,
                        break, del, ins_rep,  exit, gold , silver, bronze;

VAR step  : INTEGER;
    Gold  : BOOLEAN;

PROCEDURE Refresh;
  VAR x,y,s: INTEGER;
BEGIN
  fill(0); mode(rep);
--color(1);
--s:=800 DIV Scale;
--FOR y:=WndY DIV 100 TO (WndY+(clipN+1-clipS)*8 DIV Scale) DIV 100 DO
--  FOR x:=WndX DIV 100 TO (WndX+(clipE+1-clipW)*4 DIV Scale) DIV 100 DO
--    dot((x*100-WndX)*Scale DIV 4,(y*100-WndY)*Scale DIV 8);
--  END;
--END;
  color(1); DrowTree(Root,CurPict,Root^.pX1,Root^.pY1);
  color(3); Drow(CurPict,curpX,curpY);
  vect(0,0,2399,0); vect(2399,0,2399,1799);
  vect(2399,1799,0,1799); vect(0,1799,0,0);
  color(2); DrowRingTree(CurPict^.pDown,NIL,curpX,curpY);
END Refresh;

PROCEDURE Center;
BEGIN
  WndX:=X-(clipE-clipW) * 2 DIV Scale;
  WndY:=Y-(clipN-clipS) * 4 DIV Scale;
  Refresh;
END Center;

PROCEDURE MoveCrs(c: CHAR): BOOLEAN;
BEGIN
  CASE c OF
     |left : DEC(X,step);
     |right: INC(X,step);
     |up   : INC(Y,step);
     |dw   : DEC(Y,step);
     |rtab : IF step=10 THEN step:=1 ELSE
               step:=10;
               X:=(X+5) DIV 10 * 10;
               Y:=(Y+5) DIV 10 * 10;
             END;
     |silver: Center;
     |dupln: IF Scale<64 THEN Scale:=Scale*2; Center END;
     |eraln: IF Scale>=2 THEN Scale:=Scale DIV 2; Center END;
  ELSE
    IF c='r' THEN
      Refresh
    ELSE
      RETURN TRUE
    END;
  END;
  RETURN FALSE;
END MoveCrs;

PROCEDURE StartLine(c: CHAR): BOOLEAN;
  VAR x,y: INTEGER;
BEGIN
  IF c#insln THEN RETURN TRUE END;
  x:=X; y:=Y;
  LOOP
    mode(com); color(3);
    vect(x,y,X,Y); cursor(X,Y,2);
    c:=ReadKey();
    cursor(X,Y,2); vect(x,y,X,Y);
    IF MoveCrs(c) THEN
      IF (c=lf)OR(c=cr) THEN
        AppLine(x,y,X,Y);
        mode(add); color(3); vect(x,y,X,Y);
        x:=X; y:=Y;
        IF c=lf THEN RETURN FALSE END;
      ELSE
        Write(7c)
      END;
    END;
  END;
  RETURN FALSE;
END StartLine;

PROCEDURE MovePict(c: CHAR): BOOLEAN;
  VAR x,y: INTEGER;
BEGIN
  IF c#delln THEN RETURN TRUE END;
  X:=curpX; Y:=curpY; x:=X; y:=Y;
  LOOP
    mode(com); color(3);
    vect(x,y,X,Y); cursor(X,Y,3);
    c:=ReadKey();
    cursor(X,Y,3); vect(x,y,X,Y);
    IF MoveCrs(c) THEN
      IF c=lf THEN
        mode(rep); color(0); DrowTree(CurPict,NIL,curpX,curpY);
        INC(CurPict^.pX1,X-x); INC(CurPict^.pX2,X-x);
        INC(CurPict^.pY1,Y-y); INC(CurPict^.pY2,Y-y);
        curpX:=X; curpY:=Y;
        color(3); Drow(CurPict,curpX,curpY);
        color(2); DrowRingTree(CurPict^.pDown,NIL,curpX,curpY);
        RETURN FALSE;
      ELSE
        Write(7c);
      END;
    END;
  END;
END MovePict;

PROCEDURE Find(c: CHAR): BOOLEAN;
  VAR s: Object;
BEGIN
  IF Gold & (c=uppg) THEN
    IF CurPict^.pUp#NIL THEN
      mode(rep); color(2);
      DrowRingTree(CurPict,CurPict^.pDown,
        curpX-CurPict^.pX1,curpY-CurPict^.pY1);
      DEC(curpX,CurPict^.pX1); DEC(curpY,CurPict^.pY1);
      CurPict^.pUp^.pDown:=CurPict; CurPict:=CurPict^.pUp;
      mode(rep); color(3); Drow(CurPict,curpX,curpY);
    END;
  ELSIF Gold & (c=dwpg) THEN
    IF CurPict^.pDown#NIL THEN
      mode(rep); color(1); DrowTree(CurPict,CurPict^.pDown,curpX,curpY);
      CurPict:=CurPict^.pDown;
      INC(curpX,CurPict^.pX1); INC(curpY,CurPict^.pY1);
      mode(rep); color(3); Drow(CurPict,curpX,curpY);
    END;
  ELSIF c=upswp THEN
    IF CurPict^.pRight#CurPict THEN
      mode(rep); color(1); DrowTree(CurPict,NIL,curpX,curpY);
      DEC(curpX,CurPict^.pX1); DEC(curpY,CurPict^.pY1);
      CurPict:=CurPict^.pRight;
      INC(curpX,CurPict^.pX1); INC(curpY,CurPict^.pY1);
      mode(rep); color(3); Drow(CurPict,curpX,curpY);
      s:=CurPict^.pDown;
      color(2); DrowRingTree(s,NIL,curpX,curpY);
    END;
  ELSIF c=dwswp THEN
    IF CurPict^.pLeft#CurPict THEN
      mode(rep); color(1); DrowTree(CurPict,NIL,curpX,curpY);
      DEC(curpX,CurPict^.pX1); DEC(curpY,CurPict^.pY1);
      CurPict:=CurPict^.pLeft;
      INC(curpX,CurPict^.pX1); INC(curpY,CurPict^.pY1);
      mode(rep); color(3); Drow(CurPict,curpX,curpY);
      s:=CurPict^.pDown;
      color(2); DrowRingTree(s,NIL,curpX,curpY);
    END;
  ELSIF c='*' THEN
    mode(rep); color(1); DrowTree(CurPict,NIL,curpX,curpY);
    s:=New(); s^.pUp:=CurPict; Tie(s,CurPict^.pDown);
    CurPict:=s;
    INC(curpX,s^.pX1); INC(curpY,s^.pY1);
  ELSIF c='+' THEN
    IF CurPict^.pUp#NIL THEN
      mode(rep); color(1); DrowTree(CurPict,NIL,curpX,curpY);
      s:=CopyTree(CurPict); Tie(s,CurPict^.pUp^.pDown);
      INC(s^.pX1,32); INC(s^.pY1,32);
      DEC(curpX,CurPict^.pX1); DEC(curpY,CurPict^.pY1);
      CurPict:=s;
      INC(curpX,CurPict^.pX1); INC(curpY,CurPict^.pY1);
      mode(rep); color(3); Drow(CurPict,curpX,curpY);
      s:=CurPict^.pDown;
      color(2); DrowRingTree(s,NIL,curpX,curpY);
    END;
  ELSE
    RETURN TRUE;
  END;
  RETURN FALSE;
END Find;

PROCEDURE Finish(c: CHAR): BOOLEAN;
BEGIN
  IF    c='q'  THEN
  ELSIF c=exit THEN WriteModel(Mdl);
  ELSE
    RETURN TRUE;
  END;
  RETURN FALSE;
END Finish;

PROCEDURE Monitor;
  VAR c: CHAR;
BEGIN
  X:=0; Y:=0; step:=10;
  Refresh;
  LOOP
    mode(com); color(3);
    cursor(X,Y,1);
    c:=ReadKey();
    IF c=gold THEN Gold:=TRUE; c:=ReadKey() ELSE Gold:=FALSE END;
    cursor(X,Y,1);
    IF MoveCrs(c)&StartLine(c)&MovePict(c)&Find(c) THEN
      IF Finish(c) THEN Write(7c) ELSE RETURN END;
    END;
  END;
END Monitor;

PROCEDURE runHelp;
BEGIN
  print(' ve [ filename  ] [-n]\n');
END runHelp;

PROCEDURE FindRoot(o: Object);
  VAR ch: CHAR;
BEGIN
  IF Tag(o)=picture THEN
    print('%s ? ',o^.Name); ch:=Read();
    IF ch='y' THEN Root:=o END;
    print('%c\n',ch);
  END;
END FindRoot;

VAR e   : Reaction;
    Name: String;

BEGIN
  IF Exception?(e) THEN
    finish; print('%s\n',Message); HALT(1);
  END;
  ScanFlags; TakeWord(Name);
  IF Flag?('h')OR(Name[0]=0c) THEN runHelp; HALT END;
  Final(finish);
  IF Flag?('n') THEN
    Mdl:=NewObject(chiptype); Mdl^.Name:=Name;
    Root:=New(); Root^.Name:=Name;
    Model.Tie(Mdl^.All,Root);
  ELSE
    Mdl:=ReadModel(Name);
    Iterate(Mdl^.All,FindRoot);
    IF Root=NIL THEN HALT END;
  END;
  start;
  CurPict:=Root; curpX:=Root^.pX1; curpY:=Root^.pY1;
--SetMode(FALSE);
  Monitor;
END ve.
