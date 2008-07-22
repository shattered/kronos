IMPLEMENTATION MODULE pedWindow; (* 03-Dec-87. (c) KRONOS *)

FROM pedPbl    IMPORT   CursorX, CursorY, Layer, Fixed, Signal, ShowStat,
                        LayersNo;
FROM pedScreen IMPORT   WindowX, WindowY, Text, OpenWindow, Cursor,
                        SetWindow, WindowE, WindowS, WindowN, WindowW,
                        CloseWindow, Drow, Delete, ScaleX, ScaleY;
FROM Keyboard  IMPORT   PeekKey, ReadKey, up, dw, left, right, delc, insc,
                        gold, silver, bronze, insln, dwpg, uppg,
                        empty, home, end;

CONST marginX=150;
      marginY=60;

PROCEDURE GetKey(): CHAR;
BEGIN
  RETURN ReadKey();
END GetKey;

PROCEDURE Correct;
BEGIN
  CursorX:=(CursorX+24)DIV 48 * 48;
  CursorY:=(CursorY+24)DIV 48 * 48;
END Correct;


PROCEDURE DoKey(): CHAR;
  VAR Ch: CHAR; i,j: CARDINAL; k: CHAR;
  PROCEDURE UP;
  BEGIN
    INC(CursorY,Speed);
    IF CursorY>WindowN-j DIV 2 THEN
      SetWindow(Scales[Zoom],WindowX,CursorY+j+WindowY-WindowN);
    END;
  END UP;
  PROCEDURE DOWN;
  BEGIN
    DEC(CursorY,Speed);
    IF CursorY<WindowS+j DIV 2 THEN
      SetWindow(Scales[Zoom],WindowX,CursorY-j+WindowY-WindowS);
    END;
  END DOWN;
  PROCEDURE RIGHT;
  BEGIN
    INC(CursorX,Speed);
    IF CursorX>WindowE-i DIV 2 THEN
      SetWindow(Scales[Zoom],CursorX+i+WindowX-WindowE,WindowY);
    END;
  END RIGHT;
  PROCEDURE LEFT;
  BEGIN
    DEC(CursorX,Speed);
    IF CursorX<WindowW+i DIV 2 THEN
      SetWindow(Scales[Zoom],CursorX-i+WindowX-WindowW,WindowY);
    END;
  END LEFT;
BEGIN
  LOOP
    ShowStat;
    IF InChain OR InMacro THEN
      Cursor(LastX,LastY,CursorX,CursorY)
    ELSE
      Cursor(CursorX,CursorY,CursorX,CursorY);
    END;
    k:=GetKey();
    i:=ScaleX*marginX; j:=ScaleY*marginY;
    CASE k OF
      left,'4'  : LEFT;
     |right,'6' : RIGHT;
     |dw,'2'    : DOWN;
     |up,'8'    : UP;
     |'7'       : LEFT; UP;
     |'9'       : RIGHT; UP;
     |'3'       : RIGHT; DOWN;
     |'1'       : LEFT; DOWN;
     |dwpg : IF NOT InChain THEN Layer:=0 END;
     |uppg : IF NOT InChain THEN Layer:=1 END;
    ELSE
      IF (k>=0c)&(k<=176c)OR(k>=240c)&(k<=377c) THEN
        Ch:=CHAR(k);
        CASE Ch OF
          'z': IF Zoom<HIGH(Scales) THEN
                 INC(Zoom); SetWindow(Scales[Zoom],CursorX,CursorY);
               END;
         |'x': IF Zoom>0 THEN
                 DEC(Zoom); SetWindow(Scales[Zoom],CursorX,CursorY);
               END;
         |'-': IF Speed>1       THEN Speed:=Speed DIV 2 END;
         |'+': IF Speed<480     THEN Speed:=Speed*2 END;
         |'*': Speed:=48; Correct;
        ELSE RETURN k;
        END;
      ELSIF k=gold THEN
        k:=ReadKey();
        i:=Scales[Zoom];
        CASE k OF
          left : DEC(CursorX,WindowX-WindowW); Correct;
                 SetWindow(i,WindowW,WindowY);
         |right: INC(CursorX,WindowE-WindowX); Correct;
                 SetWindow(i,WindowE,WindowY);
         |dw   : DEC(CursorY,WindowY-WindowS); Correct;
                 SetWindow(i,WindowX,WindowS);
         |up   : INC(CursorY,WindowN-WindowY); Correct;
                 SetWindow(i,WindowX,WindowN);
        ELSE
        END;
      ELSE RETURN k;
      END;
    END;
  END;
END DoKey;

PROCEDURE Command(): CHAR;
  VAR k: CHAR;
BEGIN
  k:=DoKey();
  RETURN k;
END Command;

END pedWindow.
