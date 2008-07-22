MODULE te; (*  02-Jul-91. (c) KRONOS *)

IMPORT  ed: teED;
IMPORT  mn: teMN;
IMPORT  wm: pmWM;

VAR type: POINTER TO INTEGER;

BEGIN
  ASSERT(ed.type#mn.type);
  LOOP
    wm.monitor;
    mn.ontop;
    IF wm.active#NIL THEN
      type:=wm.active^.obj;
      IF    type^=ed.type THEN
        IF    wm.moved    THEN ed.move
        ELSIF wm.resized  THEN ed.resize
        ELSIF wm.closed   THEN ed.close
        ELSE                   ed.switch
        END
      ELSIF type^=mn.type THEN
        IF    wm.moved    THEN mn.move
        ELSIF wm.resized  THEN mn.resize
        ELSIF wm.closed   THEN mn.close
        ELSE                   mn.switch
        END
      ELSE
        HALT(1)
      END
    END
  END
END te.
