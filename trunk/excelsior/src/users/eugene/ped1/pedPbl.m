IMPLEMENTATION MODULE pedPbl; (* Sem 06-Mar-87. (c) KRONOS *)

FROM Image     IMPORT   image;
FROM Strings   IMPORT   Str0, AppStr;
FROM pedScreen IMPORT   Text;

VAR Line: ARRAY [0..81] OF CHAR;

PROCEDURE ShowStat;
  VAR xh,xl,yh,yl: INTEGER;
BEGIN
  Str0(Line);
  AppStr(Line,'Cursor: ');
  IF Layer=0 THEN AppStr(Line,'  solder ') ELSE AppStr(Line,'component') END;
  xh:=(ABS(CursorX)*625) DIV 24; xl:=xh MOD 1000; xh:=xh DIV 1000;
  yh:=(ABS(CursorY)*625) DIV 24; yl:=yh MOD 1000; yh:=yh DIV 1000;
  image(Line,' %3d.%$3d %3d.%$3d (%5d %5d) ',xh,xl,yh,yl,CursorX,CursorY);
  IF Signal#NIL THEN
    AppStr(Line,Signal^.name);
  ELSE
    AppStr(Line,'...');
  END;
  IF Boxed THEN AppStr(Line,' boxed') END;
  IF Fixed THEN AppStr(Line,' fixed') END;
  Text(3,Line);
END ShowStat;

BEGIN
  Signal:=NIL;
  Boxed:=FALSE;
  CursorX:=0; CursorY:=0;
END pedPbl.
