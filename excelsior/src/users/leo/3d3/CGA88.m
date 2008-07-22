IMPLEMENTATION MODULE CGA88; (* nick 12-Mar-90. (c) KRONOS *)

IMPORT  BMG, SYSTEM;
IMPORT  scr: Screen;
IMPORT  cod: defCodes;
IMPORT  low: lowLevel;

IMPORT  tty: Terminal;

VAR tool: BMG.TOOL;

PROCEDURE color(c: INTEGER);
BEGIN
  C:=INTEGER(BITSET(c)*{0..2});
  CASE C OF
    3: C:=6 |  5: C:=7 |
    6: C:=3 |  7: C:=5 |
  ELSE
  END;
  tool.mask :=BITSET(C);
  tool.color:=BITSET(C);
END color;

PROCEDURE erase; BEGIN BMG.erase(bmd) END erase;

PROCEDURE line(c,x,y,x1,y1: INTEGER);
BEGIN
  tool.mask :={0..3};
  tool.color:=BITSET(c);
  BMG.line(bmd,tool,x*2,359-y*2,x1*2,359-y1*2)
END line;

TYPE  ADDRESS = SYSTEM.ADDRESS;


PROCEDURE bitrepl(d: ADDRESS; do: INTEGER;
                  s: ADDRESS; so: INTEGER; sz: INTEGER);
CODE cod.bblt END bitrepl;

VAR line0: ADDRESS; ln0: ARRAY [0..15] OF BITSET;
    line1: ADDRESS; ln1: ARRAY [0..15] OF BITSET;
    line2: ADDRESS; ln2: ARRAY [0..15] OF BITSET;
    line3: ADDRESS; ln3: ARRAY [0..15] OF BITSET;

PROCEDURE hline(c,int,xb,yb,xe: INTEGER);
  CONST
    tr=ARRAY OF INTEGER {0,1,2,6,4,7,3,5};

  VAR l0,l1,l2,l3: ADDRESS;
      len,WL     : INTEGER;
      msk        : BITSET;
BEGIN
  c:=tr[c MOD 8];
  xb:=xb*2; xe:=xe*2; yb:=359-yb*2;
  WITH tool.clip DO
    IF (yb<0) OR (yb>=358) THEN RETURN END;
    IF xb> xe  THEN len:=xe; xe:=xb; xb:=len END;
    IF xb< 0   THEN xb:=0     END;
    IF xe>=480 THEN xe:=480-1 END;
    yb:=bmd^.H-1-yb-y;
    xb:=x+xb;
  END;
  len:=xe-xb+2;
  msk:=BITSET(c);
  WITH bmd^ DO
    yb:=yb*WPL;
    l0:=layers[0]+yb;
    l1:=layers[1]+yb;
    l2:=layers[2]+yb;
    l3:=layers[3]+yb;
    CASE int OF
      | 0 :
      |1,5: IF msk*{0}#{} THEN bitrepl(l0,xb,line1,xb,len) END;
            IF msk*{1}#{} THEN bitrepl(l1,xb,line1,xb,len) END;
            IF msk*{2}#{} THEN bitrepl(l2,xb,line1,xb,len) END;
            IF msk*{3}#{} THEN bitrepl(l3,xb,line1,xb,len) END;

      | 2 : IF msk*{0}#{} THEN bitrepl(l0    ,xb,line2,xb,len);
                               bitrepl(l0-WPL,xb,line1,xb,len) END;
            IF msk*{1}#{} THEN bitrepl(l1    ,xb,line2,xb,len);
                               bitrepl(l1-WPL,xb,line1,xb,len) END;
            IF msk*{2}#{} THEN bitrepl(l2    ,xb,line2,xb,len);
                               bitrepl(l2-WPL,xb,line1,xb,len) END;
            IF msk*{3}#{} THEN bitrepl(l3    ,xb,line2,xb,len);
                               bitrepl(l3-WPL,xb,line1,xb,len) END;

      |3,6: IF msk*{0}#{} THEN bitrepl(l0    ,xb,line3,xb,len);
                               bitrepl(l0-WPL,xb,line1,xb,len) END;
            IF msk*{1}#{} THEN bitrepl(l1    ,xb,line3,xb,len);
                               bitrepl(l1-WPL,xb,line1,xb,len) END;
            IF msk*{2}#{} THEN bitrepl(l2    ,xb,line3,xb,len);
                               bitrepl(l2-WPL,xb,line1,xb,len) END;
            IF msk*{3}#{} THEN bitrepl(l3    ,xb,line3,xb,len);
                               bitrepl(l3-WPL,xb,line1,xb,len) END;

      |4,7,8,9,
        10: IF msk*{0}#{} THEN bitrepl(l0    ,xb,line3,xb,len);
                               bitrepl(l0-WPL,xb,line3,xb,len) END;
            IF msk*{1}#{} THEN bitrepl(l1    ,xb,line3,xb,len);
                               bitrepl(l1-WPL,xb,line3,xb,len) END;
            IF msk*{2}#{} THEN bitrepl(l2    ,xb,line3,xb,len);
                               bitrepl(l2-WPL,xb,line3,xb,len) END;
            IF msk*{3}#{} THEN bitrepl(l3    ,xb,line3,xb,len);
                               bitrepl(l3-WPL,xb,line3,xb,len) END;
            CASE int OF
            |5,6,7: bitrepl(l3    ,xb,line1,xb,len)

            |  8  : bitrepl(l3    ,xb,line2,xb,len);
                    bitrepl(l3-WPL,xb,line1,xb,len)

            |  9  : bitrepl(l3    ,xb,line3,xb,len);
                    bitrepl(l3-WPL,xb,line1,xb,len)

            | 10  : bitrepl(l3    ,xb,line3,xb,len);
                    bitrepl(l3-WPL,xb,line3,xb,len)
            ELSE
            END
    ELSE
      ASSERT(FALSE)
    END;
    CASE int OF
      |5,6,7: bitrepl(l3    ,xb,line1,xb,len)

      |  8  : bitrepl(l3    ,xb,line2,xb,len);
              bitrepl(l3-WPL,xb,line1,xb,len)

      |  9  : bitrepl(l3    ,xb,line3,xb,len);
              bitrepl(l3-WPL,xb,line1,xb,len)

      | 10  : bitrepl(l3    ,xb,line3,xb,len);
              bitrepl(l3-WPL,xb,line3,xb,len)
    ELSE
    END
  END
END hline;

PROCEDURE init;
BEGIN
  line1:=SYSTEM.ADR(ln1); low.fill(ln1,055555555h);
  line2:=SYSTEM.ADR(ln2); low.fill(ln2,0AAAAAAAAh);
  line0:=SYSTEM.ADR(ln0); low.fill(ln0, 0);
  line3:=SYSTEM.ADR(ln3); low.fill(ln3,-1);
END init;

VAR i,j,k: INTEGER;

PROCEDURE palette;
  VAR i,j,k: INTEGER;
BEGIN
  WITH scr.state^ DO
    FOR i:=0 TO 15 DO
      WITH pal[i] DO
        IF 3 IN BITSET(i) THEN k:=3 ELSE k:=1 END;
        IF 2 IN BITSET(i) THEN b:=k ELSE b:=0 END;
        IF 1 IN BITSET(i) THEN g:=k ELSE g:=0 END;
        IF 0 IN BITSET(i) THEN r:=k ELSE r:=0 END;
      END
    END;
    WITH pal[15] DO r:=3; g:=3; b:=3 END;
  END;
  scr.set_palette(scr.state^.pal,0,16);
  ASSERT(scr.done,scr.error)
END palette;

BEGIN
  --scr.attach("/dev/scr0");
  scr.loophole(scr.bitmap,bmd); ASSERT(scr.done,scr.error);
  scr.set_ldcy(0);              ASSERT(scr.done,scr.error);
  scr.set_ldcx(0);              ASSERT(scr.done,scr.error);
  palette;
  WITH tool DO
    zX:=0; clip.x:=0; clip.w:=480; mode:=BMG.rep;    back :={    };
    zY:=0; clip.y:=0; clip.h:=360; mask:=bmd^.mask;  color:={0..3}
  END;
  line1:=SYSTEM.ADR(ln1); low.fill(ln1,055555555h);
  line2:=SYSTEM.ADR(ln2); low.fill(ln2,0AAAAAAAAh);
  line0:=SYSTEM.ADR(ln0); low.fill(ln0, 0);
  line3:=SYSTEM.ADR(ln3); low.fill(ln3,-1);
  erase
END CGA88.
