MODULE t; (* Leo 19-Jan-2001. (c) KRONOS *)

sadasd

IMPORT  defBMG;
IMPORT  BMG, SYSTEM;
IMPORT  scr: Screen;
IMPORT  cod: defCodes;
IMPORT  low: lowLevel;

IMPORT  tty: Terminal;
IMPORT  kbd: Keyboard;

VAR tool: BMG.TOOL;
VAR bmd: defBMG.BITMAP;

PROCEDURE erase;
BEGIN
  BMG.erase(bmd)
END erase;

PROCEDURE test();
  VAR c,x,y: INTEGER;
BEGIN
   c := 0;
   tool.mode := BMG.or;
   FOR y := 0 TO 359 DO
       c := (c + 1) MOD 16;
       tool.color := BITSET(c);
       BMG.line(bmd, tool,   0,   y, 479,   0);
       BMG.line(bmd, tool, 479,   y,   0,   0);
       BMG.line(bmd, tool, 479, 359,   0,   y);
       BMG.line(bmd, tool,   0, 359, 479,   y);
   END;
   c := 0;
   tool.mode := BMG.rep;
   FOR y := 0 TO 359 DO
      IF y MOD 22 = 21 THEN -- 360 /16 = 22.5
         c := (c + 1) MOD 16;
         tty.print("y=%d color=%d\n", y, c);
      END;
      tool.color := BITSET(c);
--      BMG.line(bmd, tool, 0, y, 479, y);
      FOR x := 0 TO 479 DO
        BMG.dot(bmd, tool, x, y);
      END;
   END;
END test;

VAR ch: CHAR;

BEGIN
  scr.loophole(scr.bitmap,bmd); ASSERT(scr.done,scr.error);
  scr.set_ldcy(0);              ASSERT(scr.done,scr.error);
  scr.set_ldcx(0);              ASSERT(scr.done,scr.error);
  WITH tool DO
    zX:=0; clip.x:=0; clip.w:=480; mode:=BMG.rep;    back :={    };
    zY:=0; clip.y:=0; clip.h:=360; mask:=bmd^.mask;  color:={0..3}
  END;
  erase;
  test;
  kbd.read(ch);
END t.
