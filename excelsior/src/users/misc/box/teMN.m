IMPLEMENTATION MODULE teMN; (*  02-Jul-91. (c) KRONOS *)

IMPORT  ms: teMS;       IMPORT  bio: BIO;
IMPORT  ed: teED;       IMPORT  str: Strings;
IMPORT  mm: Heap;
IMPORT  wm: pmWM;
IMPORT  wn: pmWnd;
IMPORT  pm: pmPUP;

TYPE MMENU = POINTER TO Mmenu;
     Mmenu = RECORD
              type: INTEGER;
                 o: wn.WINDOW
             END;

CONST _zoom    = 0;
      _cascade = 1;
      _new     = 2;
      _open    = 3;
      _quit    = 4;

VAR main: wn.WINDOW;

PROCEDURE create(x,y: INTEGER);
  VAR o: wn.WINDOW;
      m: MMENU;
BEGIN
  wm.new    (o);         IF NOT wm.done THEN HALT(1) END;
  wn.move   (o,x,y    ); IF NOT wm.done THEN HALT(1) END;
  wn.resize (o,45 ,64 ); IF NOT wm.done THEN HALT(1) END;
  wn.inner  (o,0,0,0,0);
  wm.disable(o);
  wm.button (o,_zoom   ,wm.luc,16,-14,13,13); IF NOT wm.done THEN HALT(1) END;
  wm.button (o,_cascade,wm.luc,31,-14,13,13); IF NOT wm.done THEN HALT(1) END;
  wm.button (o,_new    ,wm.luc,02,-30,41,13); IF NOT wm.done THEN HALT(1) END;
  wm.button (o,_open   ,wm.luc,02,-46,41,13); IF NOT wm.done THEN HALT(1) END;
  wm.button (o,_quit   ,wm.luc,02,-62,41,13); IF NOT wm.done THEN HALT(1) END;
  wm.print  (o,_zoom   ,wm.ssfont,"%c",wm.sszoom   );
  wm.print  (o,_cascade,wm.ssfont,"%c",wm.sscascade);
  wm.print  (o,_new    ,pm.font  ,"new ");
  wm.print  (o,_open   ,pm.font  ,"open");
  wm.print  (o,_quit   ,pm.font  ,"quit");

  mm.allocate(m,SIZE(m^)); IF NOT mm.done THEN HALT(1) END;
  m^.type:=type;
  m^.o   :=o;
  wn.object(o,m);

  main:=o;
  wn.open(o)
END create;

PROCEDURE read;

  PROCEDURE readwnd(f: bio.FILE);
    VAR x,y,w,h: INTEGER;
        in     : ed.INFO;
        s      : ARRAY [0..79] OF CHAR;
        pos,i  : INTEGER;
        done   : BOOLEAN;
  BEGIN
    bio.getstr(f,s,0); IF NOT bio.done THEN RETURN END;
    bio.getstr(f,s,0); IF NOT bio.done THEN RETURN END;
    pos:=0;
    str.iscan(x,s,pos,done); IF NOT done THEN RETURN END;
    str.iscan(y,s,pos,done); IF NOT done THEN RETURN END;
    str.iscan(w,s,pos,done); IF NOT done THEN RETURN END;
    str.iscan(h,s,pos,done); IF NOT done THEN RETURN END;
    FOR i:=0 TO HIGH(in.i) DO
      str.iscan(in.i[i],s,pos,done); IF NOT done THEN RETURN END;
    END;
    bio.getstr(f,in.s,0); IF NOT bio.done THEN RETURN END;
    ed.restore(x,y,w,h,in)
  END readwnd;

  VAR f     : bio.FILE;
      done  : BOOLEAN;
      x,y,no: INTEGER;
      pos,i : INTEGER;
      s     : ARRAY [0..79] OF CHAR;

  PROCEDURE c; BEGIN create(400,100); bio.close(f) END c;

BEGIN
  bio.open(f,'te.setup','r'); IF NOT bio.done THEN c; RETURN END;
  bio.getstr(f,s,0);          IF NOT bio.done THEN c; RETURN END;
  pos:=0;
  str.iscan(no,s,pos,done);
  IF (NOT done) OR (no<0) THEN c; RETURN END;
  bio.getstr(f,s,0); IF NOT bio.done THEN c; RETURN END;
  bio.getstr(f,s,0); IF NOT bio.done THEN c; RETURN END;
  pos:=0;
  str.iscan(x,s,pos,done); IF NOT done THEN c; RETURN END;
  str.iscan(y,s,pos,done); IF NOT done THEN c; RETURN END;
  create(x,y);
  FOR i:=0 TO no-1 DO
    readwnd(f); IF NOT bio.done THEN bio.close(f); RETURN END
  END;
  bio.close(f)
END read;

PROCEDURE write;

  PROCEDURE writeno(f: bio.FILE);
    VAR o : wn.WINDOW;
        no: INTEGER;
        t : POINTER TO INTEGER;
  BEGIN
    no:=0;
    o:=wn.top;
    WHILE o#NIL DO
      t:=o^.obj;
      IF (t#NIL)&(t^=ed.type) THEN INC(no) END;
      o:=wn.dw(o)
    END;
    bio.print(f,'%d\n\n',no)
  END writeno;

  PROCEDURE writewnds(f: bio.FILE);
    VAR o : wn.WINDOW;
        t : POINTER TO INTEGER;
        in: ed.INFO;
        i : INTEGER;
  BEGIN
    o:=wn.top;
    WHILE o#NIL DO
      t:=o^.obj;
      IF (t#NIL)&(t^=ed.type) THEN
        bio.print(f,'%d %d %d %d ',o^.x,o^.y,o^.w,o^.h); IF NOT bio.done THEN RETURN END;
        ed.info(o^.obj,in);
        FOR i:=0 TO HIGH(in.i) DO
          bio.print(f,'%d ',in.i[i]); IF NOT bio.done THEN RETURN END;
        END;
        bio.print(f,'\n%s\n\n',in.s); IF NOT bio.done THEN RETURN END
      END;
      o:=wn.dw(o)
    END
  END writewnds;

  VAR f: bio.FILE;

  PROCEDURE c; BEGIN bio.close(f) END c;

BEGIN
  bio.create(f,'te.setup','w',0);          IF NOT bio.done THEN c; RETURN END;
  writeno(f);                              IF NOT bio.done THEN c; RETURN END;
  bio.print (f,'%d %d\n\n',main^.x,main^.y); IF NOT bio.done THEN c; RETURN END;
  writewnds(f);                            IF NOT bio.done THEN c; RETURN END;
  bio.close (f);
END write;

PROCEDURE quit(m: MMENU);
BEGIN
  IF ms.query(100,100,"Are you sure?") THEN write; HALT END
END quit;

PROCEDURE typecheck(VAR m: MMENU; o: wn.WINDOW);
BEGIN
  ASSERT(o#NIL);
  m:=o^.obj;
  ASSERT(m^.type=type)
END typecheck;

PROCEDURE move;
BEGIN
  wn.move(wm.active,wm.moveX,wm.moveY)
END move;

PROCEDURE resize; BEGIN END resize;
PROCEDURE close ; BEGIN END close;

PROCEDURE switch;
  VAR m: MMENU;
BEGIN
  typecheck(m,wm.active);
  CASE wm.abutton OF
    |_zoom   : pm.zoom
    |_cascade:
    |_new    : ed.new(0,100,100,100)
    |_open   :
    |_quit   : quit(m)
  ELSE
  END;
  wm.toggle(wm.active,wm.abutton,FALSE)
END switch;

PROCEDURE ontop;
BEGIN
  wn.ontop(main)
END ontop;

BEGIN
  read
END teMN.
