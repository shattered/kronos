MODULE fed; (* nick 12-Feb-91. (c) KRONOS / V0.00 / *)

IMPORT  bmg: BMG;               IMPORT  scr: fedBase;
IMPORT  men: fedMenus;          IMPORT  pup: pmPUP;
IMPORT  cpd: CPD;               IMPORT  crs: pmCrs;
IMPORT  img: fedImage;          IMPORT  key: Keyboard;
IMPORT  tim: Time;

VAR s: BITSET;
  i,j: INTEGER;
 oper: INTEGER;

PROCEDURE cursor(VAR s: BITSET);
BEGIN
  crs.toggle(TRUE);     crs.monitor;
  s:=cpd.state^.keys;   crs.toggle(FALSE)
END cursor;

PROCEDURE editchar;
  VAR c: CHAR;       x,y,xo,yo: INTEGER;
   s,so: BITSET;       pressed: BOOLEAN;
BEGIN
  IF scr.efnt=NIL THEN RETURN END;
  xo:=-1;  yo:=-1;  so:={};
  LOOP
    cursor(s);
    IF NOT scr.inblock(scr.wblk) THEN men.setlines; RETURN END;
    IF (s*{2}#{}) OR (c=33c)     THEN men.operation(oper)  END;
    IF scr.inblock(img.map) THEN
      x:=(crs.x-img.map.x) DIV img.scalx;
      y:=(crs.y-img.map.y) DIV img.scaly;
      scr.print(men.coor,9,'%03d:%03d',x,y);
      pressed:=(s*{0,1}#{});
      IF pressed & (s#so) OR (x#xo) OR (y#yo)  THEN
        img.mode:=(s*{0}#{});
        CASE oper OF
          |men._fill: IF pressed & (so={}) THEN img.fill(x,y) END
          |men._inv : IF pressed & (so={}) THEN img.invers    END
          |men._swap: IF pressed THEN img.swap(x,yo,y)        END
          |men._dot : IF pressed THEN img.dot(x,y)            END
          |men._iddt: IF pressed THEN img.insdeldot(x,y)      END
          |men._idln: IF pressed THEN img.insdelline(x,y)     END
          |men._dupl: IF pressed THEN img.dupl(x,yo,y)        END
        ELSE
        END;
      END;
      xo:=x;  yo:=y;  so:=s
    ELSE
    END
  END
END editchar;

PROCEDURE showtime;
BEGIN END showtime;

VAR c: CHAR;

BEGIN
  oper:=men._dot;
  LOOP
    cursor(s);
    IF    scr.inblock(scr.mblk) THEN men.main_menus
    ELSIF scr.inblock(scr.sblk) THEN men.readsave(men.Save)
    ELSIF scr.inblock(scr.rblk) THEN men.readsave(men.Read)
    ELSIF scr.inblock(scr.wblk) THEN editchar
    ELSIF scr.inblock(scr.quit) THEN
      IF s*{0}#{} THEN
        pup.switch(scr.quit,TRUE);
        REPEAT cpd.read(i,j,s) UNTIL s*{0}={};
        pup.switch(scr.quit,FALSE); tim.delay(200,tim.milisec);
        EXIT
      END;
    ELSIF c=33c THEN
      pup.switch(scr.quit,TRUE);  tim.delay(200,tim.milisec);
      pup.switch(scr.quit,FALSE); tim.delay(200,tim.milisec);
      EXIT
    END
  END
END fed.
