IMPLEMENTATION MODULE teMS; (*  03-Jul-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  wn : pmWnd;
IMPORT  sc : Screen;
IMPORT  bmg: BMG;
IMPORT  cod: defCodes;

PROCEDURE query(x,y: INTEGER; VAL f: ARRAY OF CHAR; SEQ p: SYSTEM.WORD): BOOLEAN;
BEGIN
  RETURN TRUE
END query;

VAR B: bmg.BITMAP;

PROCEDURE bmv(t,to,f,fo,s: INTEGER); CODE cod.bmv END bmv;

PROCEDURE paint(wnd: wn.WINDOW; cx,cy,cw,ch: INTEGER);
  VAR                           dx,dy,dh,dw: INTEGER;
    bmd: bmg.BITMAP;            so,do      : INTEGER;
   mask: BITSET;                sb,db      : SYSTEM.ADDRESS;
BEGIN
  bmd:=wnd^.desc;
  IF bmd=NIL THEN RETURN END;
  dw:=cw;  dx:=cx+wnd^.x;
  dh:=ch;  dy:=cy+wnd^.y;
  IF dx<0           THEN dw:=dw+dx; cx:=cx-dx; dx:=0 END;
  IF dy<0           THEN dh:=dh+dy; cy:=cy-dy; dy:=0 END;
  IF dx+dw>wn.scrW  THEN dw:=wn.scrW-dx END;
  IF dy+dh>wn.scrH  THEN dh:=wn.scrH-dy END;
  IF (dw<=0) OR (dh<=0) THEN RETURN END;
  so:=(bmd^.H-cy-dh)*bmd^.WPL;
  do:=(B^  .H-dy-dh)*B^.  WPL;
  REPEAT
    bmv(B^.layers[0]+do,dx,bmd^.layers[0]+so,cx,dw);
    bmv(B^.layers[1]+do,dx,bmd^.layers[1]+so,cx,dw);
    INC(so,bmd^.WPL); INC(do,B^.WPL); DEC(dh)
  UNTIL dh=0
END paint;

BEGIN
  sc.loophole(sc.bitmap,B);
  IF NOT sc.done THEN HALT(sc.error) END
END teMS.
