IMPLEMENTATION MODULE VF; (* Leo 23-Aug-88. (c) KRONOS *)

IMPORT SYSTEM, cdsHeap, BIO, tty : Terminal;

TYPE ADDRESS=SYSTEM.ADDRESS;  WORD=SYSTEM.WORD;

CONST
  default = 'ped_c.bmf';

PROCEDURE load_font(VAL font_name: ARRAY OF CHAR): BOOLEAN;
  VAR f: BIO.FILE;
   size: INTEGER;
BEGIN
  font:=NIL;
  BIO.open(f,font_name,'r');
  IF NOT BIO.done THEN RETURN TRUE END;
  size:=(BIO.eof(f)+3) DIV 4;
  cdsHeap.ALLOCATE(font,size);
  IF font=NIL THEN BIO.close(f); RETURN TRUE END;
  BIO.read(f,font,size*4);
  BIO.close(f);
  IF NOT BIO.done THEN
    cdsHeap.DEALLOCATE(font,size);
    RETURN TRUE;
  END;
  font^.base:=ADDRESS(font)+SIZE(font^);
  font^.p_w :=font^.base + font^.h*256;
  RETURN FALSE;
END load_font;

PROCEDURE make_font(VAL name: ARRAY OF CHAR);
BEGIN
  IF load_font(name) THEN
    tty.print('Can not read font from file "%s".\n',name); HALT;
  END;
END make_font;

BEGIN
  make_font(default);
  main:=font;
  micro:=font;
END VF.
