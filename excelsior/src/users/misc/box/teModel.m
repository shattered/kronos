IMPLEMENTATION MODULE teModel; (*$U+$X+$N+ Igo & Leo 09-Feb-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  mem: Heap;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  bmg: BMG;
IMPORT  fnt: Fonts;
IMPORT  low: lowLevel;
IMPORT  tile;
IMPORT  pm : pmPUP;
IMPORT  wnd: pmWnd;

IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  tim: Time;

CONST
      DWSDEPTH = 5;
      CELLSIZE = 1<<(16-DWSDEPTH*2);

TYPE ADDRESS = SYSTEM.ADDRESS;
     TOOL    = bmg.TOOL;

     PLAN  = POINTER TO plan;
     plan  = DYNARR OF tile.LAY;

     WSP   = POINTER TO wsp;
     wsp   = RECORD
               box: plan;
              text: plan;
              port: plan;
              elem: plan
             END;

     MODEL = POINTER TO Model;
     FVIEW = POINTER TO fview;

     fview = RECORD
               f    : view;
               magic: INTEGER;
               m    : MODEL;
               ws   : WSP;
               tool : TOOL;
               greed: ARRAY [0..31] OF BITSET
             END;

     Model = RECORD
               magic: INTEGER;
               open : INTEGER;
               round: BLOCK;
               iws  : wsp;
               xws  : wsp;
               name : ARRAY [0..31] OF CHAR;
               views: DYNARR OF FVIEW
             END;

     TEXT  = POINTER TO text;
     text  = RECORD
               x0,y0: INTEGER;
               x1,y1: INTEGER;
                 x,y: INTEGER;
                   s: STRING;
                   c: INTEGER;
                next: TEXT
             END;

VAR MAGIC: INTEGER;
      fFF: ARRAY [0..31] OF BITSET;
      f00: ARRAY [0..31] OF BITSET;
   fGREED: ARRAY [0..31] OF BITSET;

PROCEDURE bad_desc; BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE bad_parm; BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;

PROCEDURE min(x,y: INTEGER): INTEGER;
BEGIN
  IF x<y THEN RETURN x ELSE RETURN y END
END min;

PROCEDURE max(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN x ELSE RETURN y END
END max;

PROCEDURE bmv (t,to,f,fo,len: INTEGER); CODE cod.bmv  END bmv;
PROCEDURE move(t,f,len: INTEGER);       CODE cod.move END move;
PROCEDURE wmv (t,f,len: INTEGER);       CODE cod.wmv  END wmv;

PROCEDURE allocate(VAR a: ADDRESS; size: INTEGER);
BEGIN
  mem.allocate(a,size);  done:=mem.done;
  IF NOT done THEN error:=mem.error END
END allocate;

PROCEDURE deallocate(VAR a: ADDRESS; size: INTEGER);
BEGIN
  mem.deallocate(a,size);
  IF NOT mem.done THEN HALT(mem.error) END
END deallocate;

PROCEDURE reallocate(VAR a: ADDRESS; VAR h: INTEGER; s,b: INTEGER);
BEGIN
  mem.reallocate(a,h,s,b); done:=mem.done;
  IF NOT done THEN error:=mem.error END
END reallocate;

WITH STORAGE (NEW: allocate; DISPOSE: deallocate; RESIZE: reallocate);

PROCEDURE tdispose(VAR t: TEXT);
BEGIN
  DISPOSE(t^.s); DISPOSE(t)
END tdispose;

PROCEDURE _disposetext(t: tile.TILE): BOOLEAN;
  VAR p,pn: TEXT;
BEGIN
  IF t^.type=tile.spc THEN RETURN FALSE END;
  p:=ADDRESS(t^.type);
  WHILE p#NIL DO
    pn:=p^.next;
    tdispose(p);
    p:=pn
  END;
  RETURN FALSE
END _disposetext;

PROCEDURE mdispose(VAR m: MODEL); FORWARD;

PROCEDURE _disposeport(t: tile.TILE): BOOLEAN;
BEGIN
  IF t^.type=tile.spc THEN RETURN FALSE END;
  ASSERT(FALSE);
END _disposeport;

PROCEDURE _disposeelem(t: tile.TILE): BOOLEAN;
  VAR p: POINTER TO MODEL;
BEGIN
  IF t^.type=tile.spc THEN RETURN FALSE END;

ASSERT(FALSE);
  p:=ADDRESS(t^.type);
  mdispose(p^);
  RETURN FALSE
END _disposeelem;

PROCEDURE disposews(VAR ws: wsp);

PROCEDURE disposep(VAR p: plan; it: tile.iterator);
  VAR i: INTEGER;
BEGIN
  IF HIGH(p)<0 THEN RETURN END;
  FOR i:=0 TO HIGH(p) DO
    tile.iterate(p[i],0,0,tile.wsize-1,tile.wsize-1,it);
    tile.dispose(p[i])
  END
END disposep;

  VAR i: INTEGER;
BEGIN
  disposep(ws.text,_disposetext);
  disposep(ws.port,_disposeport);
  disposep(ws.elem,_disposeelem);
  FOR i:=0 TO HIGH(ws.box) DO tile.dispose(ws.box[i]) END; DISPOSE(ws.box)
END disposews;

PROCEDURE newws(VAR ws: wsp);

PROCEDURE newp(VAR p: plan; sz: INTEGER);
  VAR i: INTEGER;
BEGIN
  NEW(p,sz);
  IF NOT done THEN RETURN END;
  FOR i:=0 TO HIGH(p) DO p[i]:=NIL END;
  FOR i:=0 TO HIGH(p) DO
    tile.new(p[i]); done:=tile.done;
    IF NOT done THEN error:=tile.error; RETURN END
  END
END newp;

BEGIN
  WITH ws DO
    NEW(box ); NEW(text);
    NEW(port); NEW(elem)
  END;
  newp(ws.box ,class.layno); IF NOT done THEN disposews(ws); RETURN END;
  newp(ws.text,DWSDEPTH   ); IF NOT done THEN disposews(ws); RETURN END;
  newp(ws.port,DWSDEPTH   ); IF NOT done THEN disposews(ws); RETURN END;
  newp(ws.elem,DWSDEPTH   ); IF NOT done THEN disposews(ws); RETURN END
END newws;

PROCEDURE mnew(VAR m: MODEL);
BEGIN
  NEW(m);
  IF NOT done THEN RETURN END;
  WITH m^ DO
    magic:=MAGIC;
    low.zero(round);
    name    :="";
    open    :=0;
    newws(xws);
    IF NOT done THEN mdispose(m); RETURN END;
    newws(iws);
    IF NOT done THEN mdispose(m); RETURN END;
    NEW(views)
  END
END mnew;

PROCEDURE mdispose(VAR m: MODEL);
  VAR i: INTEGER;
BEGIN
  IF m=NIL THEN RETURN END;
  IF m^.magic#MAGIC THEN bad_desc; RETURN END;
  WITH m^ DO
    DEC(open);
    IF open>0 THEN m:=NIL; RETURN END;
    disposews(iws);
    disposews(xws);
    FOR i:=0 TO HIGH(views) DO views[i]^.m:=NIL; views[i]^.ws:=NIL END;
    DISPOSE(views);
    magic:=0;  DISPOSE(m)
  END
END mdispose;

PROCEDURE badmodel(m: MODEL): BOOLEAN;
BEGIN
  RETURN (m=NIL) OR (m^.magic#MAGIC)
END badmodel;

PROCEDURE badview(VAR s: FVIEW; d: VIEW): BOOLEAN;
BEGIN
  IF d=NIL THEN bad_desc; RETURN TRUE END;
  s:=FVIEW(d);
  IF s^.magic#MAGIC THEN s:=NIL; bad_desc; RETURN TRUE END;
  RETURN FALSE
END badview;

PROCEDURE vnew(VAR s: FVIEW; d: FVIEW);
  VAR m: MODEL;
BEGIN
  s:=NIL;
  IF d=NIL THEN
    mnew(m); IF NOT done THEN RETURN END
  ELSE
    m:=d^.m; IF badmodel(m) THEN RETURN END
  END;
  INC(m^.open);
  NEW(s);
  IF NOT done THEN mdispose(m); RETURN END;
  s^.m:=m;
  s^.ws:=SYSTEM.ADR(m^.iws);
  WITH s^ DO
    magic:=MAGIC;
    f.s:=1;
    f.x:=0;
    f.y:=0;
    f.w:=NIL;
    low.zero(greed);
    low.zero(tool)
  END;
  RESIZE(m^.views,HIGH(m^.views)+2);
  IF NOT done THEN s^.magic:=0; DISPOSE(s); mdispose(m); RETURN END;
  m^.views[HIGH(m^.views)]:=s
END vnew;

PROCEDURE new(VAR vs: VIEW; vd: VIEW);
  VAR s,d: FVIEW;
BEGIN
  IF    vd=NIL        THEN d:=NIL
  ELSIF badview(d,vd) THEN RETURN
  END;
  vnew(s,d);
  vs:=VIEW(s)
END new;

PROCEDURE vdispose(VAR v: FVIEW);
  VAR i,j: INTEGER; m: MODEL;
BEGIN
  done:=TRUE;
  IF v=NIL          THEN           RETURN END;
  IF v^.magic#MAGIC THEN bad_desc; RETURN END;
  IF v^.m#NIL THEN
    WITH v^.m^ DO
      IF magic#MAGIC THEN bad_desc; RETURN END;
      FOR i:=0 TO HIGH(views) DO
        IF views[i]=v THEN
          FOR j:=i TO HIGH(views)-1 DO views[j]:=views[j+1] END;
          RESIZE(views,HIGH(views));
          mdispose(v^.m);
          v^.ws:=NIL; v^.magic:=0;
          DISPOSE(v);
          RETURN
        END
      END
    END;
    bad_parm
  ELSE
    v^.ws:=NIL; v^.magic:=0;
    DISPOSE(v)
  END
END vdispose;

PROCEDURE dispose(VAR vv: VIEW);
  VAR v: FVIEW;
BEGIN
  IF vv=NIL THEN RETURN END;
  v:=FVIEW(vv);
  vdispose(v);
  IF done THEN vv:=NIL END
END dispose;

PROCEDURE mequal(vv0,vv1: VIEW): BOOLEAN;
  VAR v0,v1: FVIEW;
BEGIN
  IF badview (v0,vv0) THEN RETURN FALSE END; IF badmodel(v0^.m) THEN RETURN FALSE END;
  IF badview (v1,vv1) THEN RETURN FALSE END; IF badmodel(v1^.m) THEN RETURN FALSE END;
  RETURN v0^.m=v1^.m
END mequal;

PROCEDURE setframe(vv: VIEW; VAL b: BLOCK);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  v^.m^.round:=b; done:=TRUE
END setframe;

PROCEDURE getframe(VAR b: BLOCK; vv: VIEW);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  b:=v^.m^.round; done:=TRUE
END getframe;

PROCEDURE setname(vv: VIEW; VAL n: ARRAY OF CHAR);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  str.copy(v^.m^.name,n); done:=TRUE
END setname;

PROCEDURE getname(vv: VIEW; VAR n: ARRAY OF CHAR);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  str.copy(n,v^.m^.name); done:=TRUE
END getname;

PROCEDURE write(vv: VIEW; VAL fn: ARRAY OF CHAR);
  VAR lens: DYNARR OF INTEGER;
      lcnt: INTEGER;
     _file: bio.FILE;
     _cnt : INTEGER;
     _bcnt: INTEGER;
     _buf : DYNARR OF INTEGER;
         m: MODEL;
         v: FVIEW;

PROCEDURE _wrerror;
BEGIN
  IF NOT bio.done THEN
    done:=bio.done; error:=bio.error;
    pm.perror(bio.error,200,200, 'write : %%s\n')
  END
END _wrerror;

PROCEDURE bwrite;
BEGIN
  bio.put(_file,_buf,_cnt*4); _cnt:=0; _wrerror
END bwrite;

PROCEDURE put(i: SYSTEM.WORD);
BEGIN _buf[_cnt]:=i; INC(_cnt); IF _cnt>HIGH(_buf) THEN bwrite END
END put;

PROCEDURE flush; BEGIN IF _cnt>0 THEN bwrite END END flush;

PROCEDURE d;
BEGIN bio.purge(_file); DISPOSE(lens); DISPOSE(_buf)
END d;

PROCEDURE writewsp(VAR ws: wsp);

  PROCEDURE putstring(VAL s: ARRAY OF CHAR);
    VAR p: ADDRESS;
        i: INTEGER;
      len: INTEGER;
  BEGIN
    p:=SYSTEM.ADR(s);
    len:=(HIGH(s)+4) DIV 4;
    put(HIGH(s)+1);  IF NOT done THEN RETURN END;
    FOR i:=0 TO len-1 DO
      put(p^); IF NOT done THEN RETURN END;
      INC(p)
    END
  END putstring;

  PROCEDURE puttile(t: tile.TILE);
  BEGIN
    put(t^.x);       IF NOT done THEN RETURN END;
    put(t^.y);       IF NOT done THEN RETURN END;
    put(t^.rg^.x-1); IF NOT done THEN RETURN END;
    put(t^.up^.y-1); IF NOT done THEN RETURN END
  END puttile;

  PROCEDURE writebox(l: tile.LAY);
    VAR stop: BOOLEAN;
           c: tile.ITERCTX;
           t: tile.TILE;
  BEGIN
    tile.start(c,l,0,0,tile.wsize,tile.wsize);
    _bcnt:=0;
    REPEAT
      stop:=tile.next(c,t);
      IF t^.type#tile.spc THEN
        put(t^.type);    IF NOT done THEN RETURN END;
        puttile(t);      IF NOT done THEN RETURN END;
        INC(_bcnt)
      END;
    UNTIL stop;
    lens[lcnt]:=_bcnt; INC(lcnt)
  END writebox;

  PROCEDURE writetext(p: plan);
    VAR stop: BOOLEAN;
           c: tile.ITERCTX;
           t: tile.TILE;
         txt: TEXT;
           i: INTEGER;
  BEGIN
    _bcnt:=0;
    FOR i:=0 TO HIGH(p) DO
      tile.start(c,p[i],0,0,tile.wsize,tile.wsize);
      REPEAT
        stop:=tile.next(c,t);
        IF t^.type#tile.spc THEN
          txt:=ADDRESS(t^.type);
          WHILE txt#NIL DO
            put      (txt^.x0); IF NOT done THEN RETURN END;
            put      (txt^.y0); IF NOT done THEN RETURN END;
            put      (txt^.x1); IF NOT done THEN RETURN END;
            put      (txt^.y1); IF NOT done THEN RETURN END;
            putstring(txt^.s ); IF NOT done THEN RETURN END;
            put      (txt^.x ); IF NOT done THEN RETURN END;
            put      (txt^.y ); IF NOT done THEN RETURN END;
            put      (txt^.c ); IF NOT done THEN RETURN END;
            txt:=txt^.next;
            INC(_bcnt)
          END
        END
      UNTIL stop
    END;
    lens[lcnt]:=_bcnt; INC(lcnt)
  END writetext;
  PROCEDURE writeport(p: plan);
    VAR stop: BOOLEAN;
           c: tile.ITERCTX;
           t: tile.TILE;
           i: INTEGER;
  BEGIN
    _bcnt:=0;
    FOR i:=0 TO HIGH(p) DO
      tile.start(c,p[i],0,0,tile.wsize,tile.wsize);
      REPEAT
        stop:=tile.next(c,t);
        IF t^.type#tile.spc THEN ASSERT(FALSE) END;
      UNTIL stop
    END;
    lens[lcnt]:=_bcnt; INC(lcnt)
  END writeport;
  PROCEDURE writeelem(p: plan);
    VAR stop: BOOLEAN;
           c: tile.ITERCTX;
           t: tile.TILE;
           i: INTEGER;
  BEGIN
    _bcnt:=0;
    FOR i:=0 TO HIGH(p) DO
      tile.start(c,p[i],0,0,tile.wsize,tile.wsize);
      REPEAT
        stop:=tile.next(c,t);
        IF t^.type#tile.spc THEN ASSERT(FALSE) END;
      UNTIL stop
    END;
    lens[lcnt]:=_bcnt; INC(lcnt)
  END writeelem;

  VAR i: INTEGER; l: INTEGER;
BEGIN
  l:=lcnt;
  FOR i:=0 TO HIGH(ws.box) DO
    writebox(ws.box[i]);
    IF NOT done THEN RETURN END
  END;
  writetext(ws.text);
  IF NOT done THEN RETURN END;
  writeport(ws.port);
  IF NOT done THEN RETURN END;
  writeelem(ws.elem);
  IF NOT done THEN RETURN END;
  lens[lcnt]:=lcnt-l+1; INC(lcnt)
END writewsp;

PROCEDURE writeframe(m: MODEL);
BEGIN
  put(m^.round.x); IF NOT done THEN RETURN END;
  put(m^.round.y); IF NOT done THEN RETURN END;
  put(m^.round.w); IF NOT done THEN RETURN END;
  put(m^.round.h); IF NOT done THEN RETURN END
END writeframe;

BEGIN
  IF badview(v,vv) THEN RETURN END;
  m:=v^.m;
  IF badmodel(m)   THEN RETURN END;
  NEW(_buf); NEW(lens);
  bio.create(_file,fn,'w',4096); done:=bio.done;
  IF NOT done THEN _wrerror; RETURN END;
  NEW(_buf,1024);
  IF NOT done THEN d; RETURN END;
  NEW(lens,HIGH(m^.iws.box)+1+4+HIGH(m^.xws.box)+1+4);
  IF NOT done THEN d; RETURN END;
  _cnt:=0;
  lcnt:=HIGH(m^.xws.box)+1+4;
  writeframe(m);
  IF NOT done THEN d; RETURN END;
  writewsp(m^.iws);
  IF NOT done THEN d; RETURN END;
  lcnt:=0;
  writewsp(m^.xws);
  IF NOT done THEN d; RETURN END;
  flush;
  bio.put(_file,lens,(HIGH(lens)+1)*4); _wrerror;
  IF NOT done THEN d; RETURN END;
  bio.close(_file);                     _wrerror;
  DISPOSE(lens); DISPOSE(_buf)
END write;

PROCEDURE _tiletype(t0,t1: INTEGER): INTEGER;
BEGIN

--; tty.print('%d %d -> %d\n',t0,t1,class.xcross[t0,t1]);

  RETURN class.xcross[t0,t1]
END _tiletype;

PROCEDURE _texttype(t0,t1: INTEGER): INTEGER;
BEGIN
  IF    t0=tile.spc THEN ASSERT(t1#tile.spc); RETURN t1
  ELSIF t1=tile.spc THEN                      RETURN t0
  ELSE                   ASSERT(FALSE)
  END
END _texttype;

PROCEDURE inserttext(p: plan; txt: TEXT);
  VAR x0,y0,x1,y1,i: INTEGER; t: tile.TILE;
BEGIN
  IF    txt^.x0>=tile.wsize THEN x0:=(tile.wsize-1) DIV CELLSIZE
  ELSIF txt^.x0< 0          THEN x0:=0
  ELSE                           x0:=txt^.x0 DIV CELLSIZE;
  END;
  IF    txt^.y0>=tile.wsize THEN y0:=(tile.wsize-1) DIV CELLSIZE
  ELSIF txt^.y0< 0          THEN y0:=0
  ELSE                           y0:=txt^.y0 DIV CELLSIZE;
  END;
  IF    txt^.x1>=tile.wsize THEN x1:=(tile.wsize-1) DIV CELLSIZE
  ELSIF txt^.x1< 0          THEN x1:=0
  ELSE                           x1:=txt^.x1 DIV CELLSIZE;
  END;
  IF    txt^.y1>=tile.wsize THEN y1:=(tile.wsize-1) DIV CELLSIZE
  ELSIF txt^.y1< 0          THEN y1:=0
  ELSE                           y1:=txt^.y1 DIV CELLSIZE;
  END;
  i:=0;
  WHILE (i<HIGH(p))&(x0#x1)&(y0#y1) DO
    x0:=x0 DIV 4; y0:=y0 DIV 4;
    x1:=x1 DIV 4; y1:=y1 DIV 4;
    INC(i)
  END;
  IF i=HIGH(p) THEN x0:=0; y0:=0 END;
  x0:=INTEGER((x0*CELLSIZE)<<(i*2));
  y0:=INTEGER((y0*CELLSIZE)<<(i*2));
  t:=tile.where(p[i],x0,y0);
  IF t^.type=tile.spc THEN
    tile.insert(p[i],INTEGER(NIL),x0,y0
                ,INTEGER(CELLSIZE<<(i*2)),INTEGER(CELLSIZE<<(i*2)),_texttype);
    done:=tile.done;
    IF NOT done THEN error:=tile.error; RETURN END;
    t:=tile.where(p[i],x0,y0)
  END;
  txt^.next:=ADDRESS(t^.type);
  t^.type:=INTEGER(SYSTEM.ADR(txt^))
END inserttext;

PROCEDURE read (VAR vv: VIEW; fn: ARRAY OF CHAR);

  VAR v: FVIEW;

  PROCEDURE rerror;
  BEGIN
    IF NOT bio.done THEN
      done:=bio.done; error:=bio.error;
      pm.perror(bio.error,200,200, 'read : %%s\n')
    END
  END rerror;

  VAR file: bio.FILE;
      fcnt: INTEGER;
       cnt: INTEGER;
       buf: DYNARR OF INTEGER;

  PROCEDURE get(VAR i: SYSTEM.WORD);
    VAR len: INTEGER;
  BEGIN
    IF fcnt>HIGH(buf) THEN
      len:=(HIGH(buf)+1)*4;
      IF (bio.eof(file)-bio.pos(file))<len THEN len:=bio.eof(file)-bio.pos(file) END;
      bio.get(file,buf,len); rerror;
      IF NOT done THEN RETURN END;
      fcnt:=0
    END;
    i:=buf[fcnt]; INC(fcnt)
  END get;

  PROCEDURE d;
  BEGIN bio.close(file); vdispose(v); DISPOSE(buf)
  END d;

  PROCEDURE readwsp(VAR ws: wsp);

    TYPE reader = PROCEDURE (tile.TILE);

    VAR lcnt: INTEGER;
        lens: DYNARR OF INTEGER;
         i,j: INTEGER;

    PROCEDURE getstring(VAR s: STRING);
      VAR len,i: INTEGER; p: ADDRESS;
    BEGIN
      get(len);   IF NOT done THEN RETURN END;
      NEW(s,len); IF NOT done THEN RETURN END;
      len:=(len+3) DIV 4;
      p:=SYSTEM.ADR(s);
      FOR i:=0 TO len-1 DO
        get(p^); IF NOT done THEN DISPOSE(s); RETURN END;
        INC(p)
      END
    END getstring;

    PROCEDURE readbox(l: tile.LAY; len: INTEGER);
      VAR x0,y0,x1,y1,type,i: INTEGER;
    BEGIN
      FOR i:=0 TO len-1 DO
        get(type); IF NOT done THEN RETURN END;
        get(x0);   IF NOT done THEN RETURN END;
        get(y0);   IF NOT done THEN RETURN END;
        get(x1);   IF NOT done THEN RETURN END;
        get(y1);   IF NOT done THEN RETURN END;
        tile.insert(l,type,x0,y0,x1-x0+1,y1-y0+1,_tiletype);
        IF NOT tile.done THEN done:=FALSE; error:=tile.error; RETURN END
      END
    END readbox;

    PROCEDURE readtext(p: plan; len: INTEGER);
      VAR i: INTEGER;
        txt: TEXT;
    BEGIN
      FOR i:=0 TO len-1 DO
        NEW(txt);          IF NOT done THEN                RETURN END;
        NEW(txt^.s);
        get(txt^.x0);      IF NOT done THEN tdispose(txt); RETURN END;
        get(txt^.y0);      IF NOT done THEN tdispose(txt); RETURN END;
        get(txt^.x1);      IF NOT done THEN tdispose(txt); RETURN END;
        get(txt^.y1);      IF NOT done THEN tdispose(txt); RETURN END;
        getstring(txt^.s); IF NOT done THEN DISPOSE (txt); RETURN END;
        get(txt^.x);       IF NOT done THEN tdispose(txt); RETURN END;
        get(txt^.y);       IF NOT done THEN tdispose(txt); RETURN END;
        get(txt^.c);       IF NOT done THEN tdispose(txt); RETURN END;
        inserttext(p,txt); IF NOT done THEN tdispose(txt); RETURN END
      END
    END readtext;

    PROCEDURE readport(p: plan; len: INTEGER);
    BEGIN ASSERT(len=0)
    END readport;
    PROCEDURE readelem(p: plan; len: INTEGER);
    BEGIN ASSERT(len=0)
    END readelem;

  BEGIN

    i:=bio.pos(file);
    bio.seek(file,bio.eof(file)-4-cnt*4,0); rerror;
    IF NOT done THEN RETURN END;
    bio.read(file,SYSTEM.ADR(lcnt),4);   rerror;
    IF NOT done THEN RETURN END;
    NEW(lens,lcnt-1); cnt:=cnt+lcnt;
    IF NOT done THEN RETURN END;
    bio.seek(file,bio.eof(file)-cnt*4,0); rerror;
    IF NOT done THEN DISPOSE(lens); RETURN END;
    bio.read(file,SYSTEM.ADR(lens),(lcnt-1)*4); rerror;
    IF NOT done THEN DISPOSE(lens); RETURN END;
    bio.seek(file,i,0); rerror;
    IF NOT done THEN RETURN END;
    FOR i:=0 TO HIGH(lens)-3 DO
      readbox(ws.box[i],lens[i]);
      IF NOT done THEN DISPOSE(lens); RETURN END
    END;
    readtext(ws.text,lens[HIGH(lens)-2]);
    IF NOT done THEN DISPOSE(lens); RETURN END;
    readport(ws.port,lens[HIGH(lens)-1]);
    IF NOT done THEN DISPOSE(lens); RETURN END;
    readelem(ws.elem,lens[HIGH(lens)-0]);
    IF NOT done THEN DISPOSE(lens); RETURN END;
    DISPOSE(lens)
  END readwsp;

  PROCEDURE readframe(m: MODEL);
  BEGIN
    get(m^.round.x); IF NOT done THEN RETURN END;
    get(m^.round.y); IF NOT done THEN RETURN END;
    get(m^.round.w); IF NOT done THEN RETURN END;
    get(m^.round.h); IF NOT done THEN RETURN END
  END readframe;

BEGIN
  vv:=NIL;
  bio.open(file,fn,'r'); done:=bio.done;
  IF NOT done THEN rerror; RETURN END;
  vnew(v,NIL);
  IF NOT done THEN bio.close(file); RETURN END;
  NEW(buf,1024);
  fcnt:=HIGH(buf)+1; cnt:=0;
  readframe(v^.m);
  IF NOT done THEN d; RETURN END;
  readwsp(v^.m^.iws);
  IF NOT done THEN d; RETURN END;
  readwsp(v^.m^.xws);
  IF NOT done THEN d; RETURN END;
  bio.close(file);
  vv:=VIEW(v)
END read;

PROCEDURE readx(v: VIEW);
BEGIN
END readx;

PROCEDURE readt(v: VIEW);
BEGIN
END readt;

----------------------------- DRAW -----------------------------
                             ------

PROCEDURE creategreed(v: FVIEW);
  VAR i,high: INTEGER;
BEGIN
  WITH v^ DO
    low.zero(greed); IF f.s<4 THEN RETURN END;
    i:=(f.s-f.x MOD f.s) MOD f.s + v^.f.w^.inner.zX;
    high:=(HIGH(greed)+1)*32-1;
    WHILE i<=high DO INCL(greed[i DIV 32],i MOD 32); i:=i+f.s END
  END
END creategreed;

PROCEDURE erase(v: FVIEW; x0,y0,bits,y1: INTEGER);

  PROCEDURE lsw (a: INTEGER): BITSET; CODE cod.copt          cod.lsw0 END lsw;
  PROCEDURE lswc(a: INTEGER): BITSET; CODE cod.copt cod.copt cod.lsw0 END lswc;
  PROCEDURE ssw (m: BITSET );         CODE                   cod.ssw0 END ssw;
  PROCEDURE add1(): INTEGER;          CODE cod.li1  cod.add           END add1;

  VAR grd,to,fo,_f00,_gre,wpl,wto: INTEGER;
      l: ARRAY [0..3] OF INTEGER;
      b: BITMAP;
      m0,m1: BITSET;
BEGIN
  WITH v^.f.w^.inner.clip DO
    IF (bits<x)OR(x0>=x+w)OR(y1<y)OR(y0>=y+h) THEN RETURN END;
    IF x0  <x    THEN x0  :=x     END; IF y0<y    THEN y0:=y     END;
    IF bits>=x+w THEN bits:=x+w-1 END; IF y1>=y+h THEN y1:=y+h-1 END
  END;
  b   :=v^.f.w^.desc;
  bits:=bits-x0+1;
  WITH v^.f.w^.inner DO
    to:=(b^.H-(zY+y0)-1)*b^.WPL*32+zX+x0;
    x0:=x0+zX
  END;
  fo :=to MOD 32;
  grd:=(v^.f.s-(y0+v^.f.y) MOD v^.f.s) MOD v^.f.s +y0;
  IF v^.f.s<4 THEN grd:=MAX(INTEGER) END;
  _f00:=0;
  FOR _gre:=0 TO LAYS-1 DO
    IF GREED#LAY[_gre] THEN l[_f00  ]:=b^.layers[LAY[_gre]]; INC(_f00)
    ELSE                    l[LAYS-1]:=b^.layers[LAY[_gre]]
    END
  END;
  _f00:=SYSTEM.ADR(f00);
  _gre:=SYSTEM.ADR(v^.greed);
  wpl:=b^.WPL;
  IF (fo=0) & (bits MOD 32=0) THEN
    to:=to DIV 32; bits:=bits DIV 32; x0:=x0 DIV 32;
    WHILE y0<=y1 DO
      IF LAYS>1 THEN move(l[0]+to,_f00,bits) END;
      IF LAYS>2 THEN move(l[1]+to,_f00,bits) END;
      IF LAYS>3 THEN move(l[2]+to,_f00,bits) END;
      IF LAYS>0 THEN
        IF y0#grd THEN        move(l[LAYS-1]+to,_f00,bits)
        ELSE INC(grd,v^.f.s); move(l[LAYS-1]+to,_gre+x0,bits)
        END;
      END;
      INC(y0); DEC(to,wpl)
    END
  ELSE
    IF fo + bits <= 32 THEN
      wto:=to DIV 32;
      to :=to MOD 32;
      INC(l[LAYS-1],wto);
      m0:={fo,fo+bits-1};
      WHILE y0<=y1 DO
        IF LAYS>1 THEN ssw(lsw(l[0]+wto)-m0) END;
        IF LAYS>2 THEN ssw(lsw(l[1]+wto)-m0) END;
        IF LAYS>3 THEN ssw(lsw(l[2]+wto)-m0) END;
        IF LAYS>0 THEN
          IF y0#grd THEN        ssw(lsw(l[LAYS-1])-m0)
          ELSE INC(grd,v^.f.s); bmv(l[LAYS-1],to,_gre,x0,bits)
          END;
        END;
        INC(y0);  DEC(wto,wpl); DEC(l[LAYS-1],wpl)
      END
    ELSIF fo + bits <= 64 THEN
      wto:=to DIV 32;
      to :=to MOD 32;
      INC(l[LAYS-1],wto);
      m0:={fo..31};
      IF (fo+bits) MOD 32 # 0 THEN m1:={0..(fo+bits) MOD 32-1} ELSE m1:={0..31} END;
      WHILE y0<=y1 DO
        IF LAYS>1 THEN   ssw(lswc(l[0]+wto)-m0); ssw(lsw(add1())-m1) END;
        IF LAYS>2 THEN   ssw(lswc(l[1]+wto)-m0); ssw(lsw(add1())-m1) END;
        IF LAYS>3 THEN   ssw(lswc(l[2]+wto)-m0); ssw(lsw(add1())-m1) END;
        IF LAYS>0 THEN
          IF y0#grd THEN ssw(lswc(l[LAYS-1])-m0); ssw(lsw(add1())-m1)
          ELSE INC(grd,v^.f.s); bmv(l[LAYS-1],to,_gre,x0,bits)
          END;
        END;
        INC(y0);  DEC(wto,wpl); DEC(l[LAYS-1],wpl)
      END
    ELSE
      wpl:=wpl*32;
      WHILE y0<=y1 DO
        IF LAYS>1 THEN bmv(l[0],to,_f00,fo,bits) END;
        IF LAYS>2 THEN bmv(l[1],to,_f00,fo,bits) END;
        IF LAYS>3 THEN bmv(l[2],to,_f00,fo,bits) END;
        IF LAYS>0 THEN
          IF y0#grd THEN        bmv(l[LAYS-1],to,_f00,fo,bits)
          ELSE INC(grd,v^.f.s); bmv(l[LAYS-1],to,_gre,x0,bits)
          END;
        END;
        INC(y0);  DEC(to,wpl);
      END
    END
  END
END erase;

PROCEDURE _rectgreed(bmd: bmg.BITMAP; VAL t: TOOL; c: BITSET; x0,y0,x1,y1: INTEGER; dfo: INTEGER);
  VAR wpl,fo,to: INTEGER; m: BITSET; lay: ADDRESS;
BEGIN
  WITH t.clip DO
    IF x0<x THEN x0:=x   END; IF x1>=x+w THEN x1:=x+w-1 END;
    IF y0<y THEN y0:=y   END; IF y1>=y+h THEN y1:=y+h-1 END;
    WITH bmd^ DO
      wpl :=WPL*32;
      to:=(H-(y0+t.zY)-1)*wpl+x0+t.zX
    END;
    IF dfo<0 THEN fo:=(x0+t.zX) MOD 4 + 3 - (y0+t.zY) MOD 4
    ELSE          fo:=(x0+t.zX) MOD 4     + (y0+t.zY) MOD 4
    END
  END;
  x1:=x1-x0+1;
  x0:=SYSTEM.ADR(fGREED);
  y0:=to-(y1-y0)*wpl;
  y1:=SYSTEM.ADR(bmd^.layers[0]);
  WHILE c*{0}={} DO INC(y1); c:=c>>1 END;
  IF c-{0}={} THEN
    lay:=y1; lay:=lay^;
    REPEAT bmv(lay,to,x0,fo,x1); fo:=(fo+dfo) MOD 4; to:=to-wpl UNTIL to<y0
  ELSE
    REPEAT
      lay:=y1; m:=c;
      REPEAT
        IF m*{0}#{} THEN bmv(lay^,to,x0,fo,x1) END;
        INC(lay); m:=(m-{0})>>1
      UNTIL m={};
      fo:=(fo+dfo) MOD 4;
      to:=to-wpl
    UNTIL to<y0
  END
END _rectgreed;

PROCEDURE _rect(bmd: bmg.BITMAP; VAL t: TOOL; c: BITSET; x0,y0,x1,y1: INTEGER);
  VAR wpl,fo,to: INTEGER; m: BITSET; lay: ADDRESS;
BEGIN
  WITH t.clip DO
    IF x0<x THEN x0:=x END; IF x1>=x+w THEN x1:=x+w-1 END;
    IF y0<y THEN y0:=y END; IF y1>=y+h THEN y1:=y+h-1 END;
    WITH bmd^ DO
      wpl :=WPL*32;
      to:=(H-(y0+t.zY)-1)*wpl+x0+t.zX;
      fo:=to MOD 32
    END
  END;
  x1:=x1-x0+1;
  x0:=SYSTEM.ADR(fFF);
  y0:=to-(y1-y0)*wpl;
  y1:=SYSTEM.ADR(bmd^.layers[0]);
  WHILE c*{0}={} DO INC(y1); c:=c>>1 END;
  IF c-{0}={} THEN
    lay:=y1; lay:=lay^; REPEAT bmv(lay,to,x0,fo,x1); to:=to-wpl UNTIL to<y0
  ELSE
    REPEAT
      lay:=y1; m:=c;
      REPEAT
        IF m*{0}#{} THEN bmv(lay^,to,x0,fo,x1) END;
        INC(lay); m:=(m-{0})>>1
      UNTIL m={};
      to:=to-wpl
    UNTIL to<y0
  END
END _rect;

PROCEDURE dvl(m: INTEGER; VAL bm: bmg.BMD; x,y,len: INTEGER);
CODE cod.bmg cod.bmg_dvl END dvl;

PROCEDURE _hline(bmd: bmg.BITMAP; VAL t: TOOL; c: BITSET; x0,y0,x1: INTEGER);
  VAR to,lay,len: INTEGER;
BEGIN
  WITH t.clip DO
    IF (y0<y) OR (y0>=y+h) THEN RETURN END;
    IF x0< x   THEN x0:=x     END;
    IF x1>=x+w THEN x1:=x+w-1 END;
    len:=x1-x0+1;
    IF len<1 THEN RETURN END;
    lay:=0;
    WITH bmd^ DO
      to:=(H-(y0+t.zY)-1)*WPL*32+x0+t.zX;
      REPEAT
        IF c*{0}#{} THEN
          bmv(layers[lay],to,SYSTEM.ADR(fFF),to MOD 32,len)
        END;
        INC(lay); c:=(c-{0})>>1
      UNTIL c={}
    END
  END
END _hline;

PROCEDURE _vline(bmd: bmg.BITMAP; VAL t: TOOL; c: BITSET; x0,y0,len: INTEGER);
  VAR to,y1,lay: INTEGER;
BEGIN
  WITH t.clip DO
    y1:=y0+len-1;
    IF (x0<x) OR (x0>=x+w) OR (y1<y) OR (y0>=y+h) THEN RETURN END;
    IF y0< y   THEN y0:=y     END;
    IF y1>=y+h THEN y1:=y+h-1 END;
    lay:=0;
    WITH bmd^ DO
      REPEAT
        IF c*{0}#{} THEN
          BASE:=layers[lay]; dvl(bmg.or,bmd^,x0+t.zX,H-(y1+t.zY)-1,y1-y0);
        END;
        INC(lay); c:=(c-{0})>>1
      UNTIL c={}
    END
  END
END _vline;

PROCEDURE _vline2(bmd: bmg.BITMAP; VAL t: TOOL; c: BITSET; x0,x1,y0,y1: INTEGER);
  TYPE b = BITSET;
  VAR to,lay: INTEGER;
BEGIN
  WITH t.clip DO
    IF (b(x1<x)+b(x0>=x+w))#{} THEN RETURN END;
    IF y0< y   THEN y0:=y     END;
    IF y1>=y+h THEN y1:=y+h-1 END;
    lay:=0;
    WITH bmd^ DO
      IF x0<x THEN
        IF x1>=x+w THEN RETURN END;
        REPEAT
          IF c*{0}#{} THEN
            BASE:=layers[lay]; dvl(bmg.or,bmd^,x1+t.zX,H-(y1+t.zY)-1,y1-y0+1);
          END;
          INC(lay); c:=(c-{0})>>1
        UNTIL c={}
      ELSE
        IF x1>=x+w THEN
          REPEAT
            IF c*{0}#{} THEN
              BASE:=layers[lay]; dvl(bmg.or,bmd^,x0+t.zX,H-(y1+t.zY)-1,y1-y0+1);
            END;
            INC(lay); c:=(c-{0})>>1
          UNTIL c={}
        ELSE
          REPEAT
            IF c*{0}#{} THEN
              BASE:=layers[lay]; dvl(bmg.or,bmd^,x0+t.zX,H-(y1+t.zY)-1,y1-y0+1);
                                 dvl(bmg.or,bmd^,x1+t.zX,H-(y1+t.zY)-1,y1-y0+1)
            END;
            INC(lay); c:=(c-{0})>>1
          UNTIL c={}
        END;
      END
    END
  END
END _vline2;

VAR V: FVIEW;

PROCEDURE _displaybox(t: tile.TILE): BOOLEAN;
  VAR  x0,y0,x1,y1: INTEGER;
      n: tile.TILE;      nx,px: INTEGER;
      c: BITSET;
      m: INTEGER;
      d: BOOLEAN;
      b: BITMAP;
BEGIN
  IF t^.type=tile.spc THEN RETURN FALSE END;
  c:=shape[t^.type];
  m:=INTEGER((c>>8)*{0..1});
  c:=COLOR[INTEGER(c*{0..LAYS-1})]*LAYSM;
  IF c={} THEN RETURN FALSE END;
  b:=V^.f.w^.desc;
  x0:=t^.x*V^.f.s-V^.f.x;
  y0:=t^.y*V^.f.s-V^.f.y;
  x1:=t^.rg^.x*V^.f.s-V^.f.x-1;
  y1:=t^.up^.y*V^.f.s-V^.f.y-1;
  CASE m OF
    |0: _rect     (b,V^.tool,c,x0,y0,x1,y1); RETURN FALSE
    |1: _rectgreed(b,V^.tool,c,x0,y0,x1,y1,-1)
    |2: _rectgreed(b,V^.tool,c,x0,y0,x1,y1, 1)
    |3:
  END;
  _vline2(b,V^.tool,c,x0,x1,y0,y1);
  n:=t^.up;
  IF n^.x>t^.x THEN
    IF n^.type=t^.type THEN px:=n^.x*V^.f.s-V^.f.x ELSE px:=x1 END;
    n:=n^.lf;
    WHILE n^.x>t^.x DO
      IF n^.type=t^.type THEN
        nx:=n^.rg^.x*V^.f.s-V^.f.x-1;
        _hline(b,V^.tool,c,nx,y1,px);
        px:=n^.x*V^.f.s-V^.f.x
      END;
      n:=n^.lf
    END;
    IF n^.type=t^.type THEN nx:=n^.rg^.x*V^.f.s-V^.f.x-1 ELSE nx:=x0 END;
    _hline(b,V^.tool,c,nx,y1,px)
  ELSIF n^.type#t^.type THEN
    _hline(b,V^.tool,c,x0,y1,x1)
  END;
  n:=t^.dw;
  IF n^.rg^.x<t^.rg^.x THEN
    IF n^.type=t^.type THEN px:=n^.rg^.x*V^.f.s-V^.f.x-1 ELSE px:=x0 END;
    n:=n^.rg;
    WHILE n^.rg^.x<t^.rg^.x DO
      IF n^.type=t^.type THEN
        nx:=n^.x*V^.f.s-V^.f.x-1;
        _hline(b,V^.tool,c,px,y0,nx);
        px:=n^.rg^.x*V^.f.s-V^.f.x-1;
      END;
      n:=n^.rg
    END;
    IF n^.type=t^.type THEN nx:=n^.x*V^.f.s-V^.f.x-1 ELSE nx:=x1 END;
    _hline(b,V^.tool,c,px,y0,nx)
  ELSIF n^.type#t^.type THEN
    _hline(b,V^.tool,c,x0,y0,x1)
  END;
  RETURN FALSE
END _displaybox;

PROCEDURE _displaytext(v: FVIEW; p: TEXT);
  VAR x0,y0: INTEGER;
          b: BITMAP;
BEGIN
  WITH v^.tool DO
    color:=COLOR[INTEGER(shape[p^.c]*{0..LAYS-1})]*LAYSM;
    IF color={} THEN RETURN END;
    mask :=color;
    back :={};
    mode :=bmg.or
  END;
  b:=v^.f.w^.desc;
  x0:=p^.x*v^.f.s-V^.f.x-(p^.x-p^.x0);
  y0:=p^.y*v^.f.s-V^.f.y-(p^.y-p^.y0);

  bmg.write(b,v^.tool,x0,y0,pm.font,p^.s,0,HIGH(p^.s)+1)
END _displaytext;

PROCEDURE refreshtext(V: FVIEW; VAL vsa: BLOCK);
  VAR t: tile.TILE;
      p: TEXT;
      i: INTEGER;
      c: tile.ITERCTX;
      x0,y0,x1,y1: INTEGER;
   stop: BOOLEAN;
BEGIN
  x0:=vsa.x; x1:=vsa.x+vsa.w-1;
  y0:=vsa.y; y1:=vsa.y+vsa.h-1;
  FOR i:=0 TO HIGH(V^.ws^.text) DO
    tile.start(c,V^.ws^.text[i],vsa.x,vsa.y,vsa.w,vsa.h);
    REPEAT
      stop:=tile.next(c,t);
      IF t^.type#tile.spc THEN
        p:=ADDRESS(t^.type);
        WHILE p#NIL DO
          IF (p^.x1>=x0)&(p^.y1>=y0)&(p^.x0<=x1)&(p^.y0<=y1) THEN
            _displaytext(V,p)
          END;
          p:=p^.next
        END
      END
    UNTIL stop
  END;
END refreshtext;

PROCEDURE refreshview(v: FVIEW; x0,y0,x1,y1: INTEGER);
  VAR      vb: BLOCK;
            i: INTEGER;
            w: WINDOW;
BEGIN
--;tty.print('refresh view (%d,%d) (%d,%d)\n',x0,y0,x1,y1);
--;tty.print('%d,%d,%d\n',v^.f.x,v^.f.y,v^.f.s);
  w:=v^.f.w;
  IF w=NIL THEN RETURN END;
  WITH w^.inner.clip DO

--;tty.print(' (%d,%d) (%d,%d) (%d,%d)\n',x,y,w,h,v^.f.w^.inner.zX,v^.f.w^.inner.zY);

    IF (x0>=x+w) OR (y0>=y+h) OR (x1<x) OR (y1<x) THEN RETURN END;
    IF x0<x THEN x0:=x END; IF x1>=x+w THEN x1:=x+w-1 END;
    IF y0<y THEN y0:=y END; IF y1>=y+h THEN y1:=y+h-1 END
  END;
  erase(v,x0,y0,x1,y1);
  v^.tool:=w^.inner;
  WITH v^.tool.clip DO
    x      :=x +x0  ; y     :=y +y0  ;
    w      :=x1-x0+1; h     :=y1-y0+1
  END;
  WITH v^ DO
    x0:=f.x+x0;
    y0:=f.y+y0;
    vb.x:=x0 DIV f.s;
    vb.y:=y0 DIV f.s;
    vb.w:=(tool.clip.w+x0 MOD f.s + f.s -1) DIV f.s;
    vb.h:=(tool.clip.h+y0 MOD f.s + f.s -1) DIV f.s
  END;
  V:=SYSTEM.ADR(v^);

--;tty.print('vb (%d,%d) (%d,%d)\n',vb.x,vb.y,vb.w,vb.h);

  FOR i:=0 TO HIGH(v^.ws^.box) DO
    tile.iterate(v^.ws^.box[i],vb.x,vb.y,vb.w,vb.h,_displaybox)
  END;

  IF v^.f.s>3 THEN refreshtext(V,vb) END;

END refreshview;

PROCEDURE wrefresh(o: WINDOW; cx,cy,cw,ch: INTEGER);
BEGIN
  WITH o^.inner.clip DO
    IF cx<0    THEN cw:=cw+cx; cx:=0 END;
    IF cy<0    THEN ch:=ch+cy; cy:=0 END;
    IF cx+cw>w THEN cw:=w-cx    END;
    IF cy+ch>h THEN ch:=h-cy    END;
    IF (cw>0) & (ch>0) THEN
      wnd.refreshbox(o,o^.inner.zX+cx,o^.inner.zY+cy,cw,ch)
    END
  END
END wrefresh;

PROCEDURE refreshblock(v: FVIEW; VAL b: BLOCK);
  VAR      lv: FVIEW;
           vn: INTEGER;
        x0,y0: INTEGER;
        x1,y1: INTEGER;

t: INTEGER;

BEGIN

t:=tim.sys_time(tim.milisec);

  FOR vn:=0 TO HIGH(v^.m^.views) DO
    lv:=v^.m^.views[vn];
    IF lv^.ws=v^.ws THEN
      x0:=b.x; y0:=b.y; x1:=b.x+b.w-1; y1:=b.y+b.h-1;
      x0:=x0*lv^.f.s-lv^.f.x; x1:=(x1+1)*lv^.f.s-lv^.f.x-1;
      y0:=y0*lv^.f.s-lv^.f.y; y1:=(y1+1)*lv^.f.s-lv^.f.y-1;
      refreshview(lv,x0,y0,x1,y1);
      WITH lv^.f.w^.inner DO
        wrefresh(lv^.f.w,x0,y0,x1-x0+1,y1-y0+1)
      END
    END
  END

;t:=tim.sys_time(tim.milisec)-t;
;tty.print('%d msec\n',t);

END refreshblock;

PROCEDURE refreshround(v: FVIEW; b: BLOCK);
BEGIN
  IF b.x+b.w<tile.wsize THEN INC(b.w) END;
  IF b.y+b.h<tile.wsize THEN INC(b.h) END;
  IF b.x>0 THEN DEC(b.x); INC(b.w) END;
  IF b.y>0 THEN DEC(b.y); INC(b.h) END;
  refreshblock(v,b)
END refreshround;

PROCEDURE refreshm(vv: VIEW);
  VAR vn: INTEGER; v: FVIEW;
BEGIN done:=TRUE;
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  FOR vn:=0 TO HIGH(v^.m^.views) DO refreshv(VIEW(v^.m^.views[vn])) END
END refreshm;

PROCEDURE refreshv(vv: VIEW);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  WITH v^.f.w^.inner.clip DO refreshview(v,x,y,w-1,h-1) END;
  WITH v^.f.w^.inner DO
    wrefresh(v^.f.w,0,0,clip.w,clip.h)
  END
END refreshv;

PROCEDURE refreshbox(vv: VIEW; x0,y0,x1,y1: INTEGER);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  refreshview(v,x0,y0,x1,y1);
  WITH v^.f.w^.inner DO
    wrefresh(v^.f.w,x0,y0,x1-x0+1,y1-y0+1)
  END
END refreshbox;

----------------------------------------------------------------

PROCEDURE setwindow(v: VIEW; w: WINDOW);
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  done:=TRUE;
  v^.w :=w;
  IF w=NIL THEN RETURN END;
  refreshv(v)
END setwindow;

PROCEDURE clearblock(v: FVIEW; VAL b: BLOCK);
  VAR x0,y0,x1,y1: INTEGER;
BEGIN
  x0:= b.x     *v^.f.s-v^.f.x;
  y0:= b.y     *v^.f.s-v^.f.y;
  x1:=(b.x+b.w)*v^.f.s-v^.f.x-1;
  y1:=(b.y+b.h)*v^.f.s-v^.f.y-1;
  erase(v,x0,y0,x1,y1)
END clearblock;

PROCEDURE move_scr(v: FVIEW; xs,ys: INTEGER);

  VAR  b: BITMAP;

  PROCEDURE loop(to,fo,words,len,wpl: INTEGER);
    VAR t0,t1,t2,t3: INTEGER;
        f0,f1,f2   : INTEGER;
  BEGIN
    len:=to+len*wpl;
    WITH b^ DO
      fo:=fo-to;
      IF 0 IN LAYSM THEN t0:=layers[0]+len; f0:=t0+fo END;
      IF 1 IN LAYSM THEN t1:=layers[1]+len; f1:=t1+fo END;
      IF 2 IN LAYSM THEN t2:=layers[2]+len; f2:=t2+fo END;
      IF 3 IN LAYSM THEN t3:=layers[3]+len; fo:=t3+fo END
    END;
    to:=to-len;
    REPEAT
      IF 0 IN LAYSM THEN wmv(t0+to,f0+to,words) END;
      IF 1 IN LAYSM THEN wmv(t1+to,f1+to,words) END;
      IF 2 IN LAYSM THEN wmv(t2+to,f2+to,words) END;
      IF 3 IN LAYSM THEN wmv(t3+to,fo+to,words) END;
      to:=to+wpl
    UNTIL to=0
  END loop;
  VAR t,f,wpl,xsw,H,W: INTEGER; w: WINDOW;
BEGIN
  ASSERT(xs MOD 32 = 0); xsw:=xs DIV 32;
  w:=v^.f.w;
  b:=w^.desc;
  WITH w^ DO
    IF (xs=0) & (ys=0) THEN RETURN END;
    H:=inner.clip.h;
    W:=inner.clip.w+inner.zX MOD 32;
    IF (ABS(ys)<H) & (ABS(xs)<W) THEN
      wpl:=b^.WPL;
      IF ys>=0 THEN t:=(b^.H  -inner.zY-H)*wpl+inner.zX DIV 32;
      ELSE          t:=(b^.H-1-inner.zY  )*wpl+inner.zX DIV 32; wpl:=-wpl
      END;
      f:=t+ys*ABS(wpl);
      IF xs>0 THEN INC(t,xsw) ELSE DEC(f,xsw) END;
      W:=(W+31) DIV 32 - ABS(xsw);
      H:=H-ABS(ys);
      loop (t,f,W,H,wpl)
    END
  END
END move_scr;

PROCEDURE check_ofs(v: FVIEW);
  VAR i: INTEGER;
BEGIN
  WITH v^ DO
    WITH v^.f.w^.inner.clip DO
      f.x:=(f.x+16) DIV 32*32;
      f.y:=(f.y+16) DIV 32*32;
      i:=tile.wsize*f.s;
      IF f.x<0 THEN f.x:=0 ELSIF f.x+w>=i THEN f.x:=(i-w) DIV 32*32 END;
      IF f.y<0 THEN f.y:=0 ELSIF f.y+h>=i THEN f.y:=(i-h) DIV 32*32 END
    END
  END
END check_ofs;

PROCEDURE calc_scroll(v: FVIEW; VAR sx,sy: INTEGER);
  VAR lx,ly: INTEGER;
BEGIN
  WITH v^ DO
    ASSERT(f.x MOD 32 = 0); ASSERT(f.y MOD 32 = 0);
    lx:=f.x; ly:=f.y;
    f.x:=f.x+sx; f.y:=ly+sy;
    check_ofs(v); creategreed(v);
    sx:=lx-f.x; sy:=ly-f.y
  END
END calc_scroll;

PROCEDURE scroll(vv: VIEW; sx,sy: INTEGER);
  VAR dy: INTEGER; v: FVIEW;
BEGIN
  IF badview(v,vv) THEN RETURN END;
  calc_scroll(v,sx,sy);
  IF (sx=0)&(sy=0) THEN RETURN END;
  WITH v^.f.w^.inner.clip DO
    IF (ABS(sx)>w) OR (ABS(sy)>h) THEN refreshv(vv); RETURN END;
    move_scr(v,sx,sy);
    dy:=0;
    IF    sy>0 THEN refreshview(v,x,y     ,x+w-1,y+sy-1)
    ELSIF sy<0 THEN refreshview(v,x,y+h+sy,x+w-1,h -1); dy:=ABS(sy); sy:=0
    END;
    IF    sx>0 THEN refreshview(v,x     ,y+sy,x+sx-1,y+h-dy-1)
    ELSIF sx<0 THEN refreshview(v,x+w+sx,y+sy,x+w -1,y+h-dy-1)
    END;
    wrefresh(v^.f.w,0,0,w,h)
  END
END scroll;

PROCEDURE zoom(vv: VIEW; n: INTEGER);
  VAR lx,ly,sc: INTEGER; v: FVIEW;
BEGIN
  IF badview(v,vv) THEN RETURN END;
  WITH v^ DO
    IF n=0 THEN RETURN END;
    IF f.s+n<1 THEN n:=f.s-1 END;
    WITH f.w^.inner.clip DO
      lx:=f.x; ly:=f.y; sc:=f.s;
      f.x:=(lx+ w DIV 2+sc DIV 2) DIV sc;
      f.y:=(ly+ h DIV 2+sc DIV 2) DIV sc;
      f.s:=f.s+n;
      f.x:=(f.x*f.s-w DIV 2+16)DIV 32*32;
      f.y:=(f.y*f.s-h DIV 2+16)DIV 32*32;
      IF n<0 THEN check_ofs(v) END;
      creategreed(v)
    END
  END;
  refreshv(vv)
END zoom;

PROCEDURE setview(vv: VIEW; x,y,scale: INTEGER);
  VAR v: FVIEW;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  v^.f.s:=max(scale,1);
  v^.f.x:=x DIV 32*32;
  v^.f.y:=y DIV 32*32;
  check_ofs(v); creategreed(v);
  refreshv(vv)
END setview;

PROCEDURE pan(vv: VIEW; b: BLOCK);
  VAR v: FVIEW; w,h: INTEGER;
BEGIN
  IF badview (v,vv) THEN RETURN END;
  IF badmodel(v^.m) THEN RETURN END;
  w:=v^.f.w^.inner.clip.w;
  h:=v^.f.w^.inner.clip.h;
  v^.f.s:=min(w DIV b.w,h DIV b.h);
  v^.f.s:=max(v^.f.s,1);
  v^.f.x:=b.x*v^.f.s DIV 32*32;
  v^.f.y:=b.y*v^.f.s DIV 32*32;
  check_ofs(v); creategreed(v);
  refreshv(vv)
END pan;

VAR _x0,_y0,_x1,_y1: INTEGER;
    _ws            : WSP;

PROCEDURE _cut(t: tile.TILE): BOOLEAN;
  VAR x0,y0,x1,y1: INTEGER;
BEGIN
  IF t^.x    <_x0 THEN x0:=_x0 ELSE x0:=t^.x END;
  IF t^.y    <_y0 THEN y0:=_y0 ELSE y0:=t^.y END;
  IF t^.up^.y<_y1 THEN y1:=t^.up^.y ELSE y1:=_y1 END;
  IF t^.rg^.x<_x1 THEN x1:=t^.rg^.x ELSE x1:=_x1 END;
  tile.insert(_ws^.box[class.tlay[t^.type]],t^.type,x0-_x0,y0-_y0,x1-x0,y1-y0,_tiletype);
  RETURN FALSE
END _cut;

PROCEDURE cut(VAR vd: VIEW; vs: VIEW; VAL b: BLOCK);
  VAR i: INTEGER; s,d: FVIEW;
BEGIN
  d:=NIL;
  IF badview(s,vs) THEN RETURN END;
  vnew(d,NIL);

;tty.print('cut (%d,%d) (%d,%d)\n',b.x,b.y,b.w,b.h);

  IF NOT done THEN RETURN END;
  _x0:=b.x; _x1:=b.x+b.w;
  _y0:=b.y; _y1:=b.y+b.h;
  d^.m^.round:=b;
  WITH d^.m^.round DO x:=0; y:=0 END;
  IF s^.ws=SYSTEM.ADR(s^.m^.iws) THEN
    _ws:=SYSTEM.ADR(d^.m^.iws)
  ELSE
    _ws:=SYSTEM.ADR(d^.m^.xws)
  END;
  FOR i:=0 TO HIGH(s^.ws^.box) DO
    tile.iterate(s^.ws^.box[i],b.x,b.y,b.w,b.h,_cut)
  END;
--  tile.iterate(s^.ws^.text,b.x,b.y,b.w,b.h,_cuttext)
  vd:=VIEW(d)
END cut;

PROCEDURE _paste(t: tile.TILE): BOOLEAN;
  VAR x0,y0,sx,sy: INTEGER;
BEGIN
  IF t^.type=tile.spc THEN RETURN FALSE END;
  x0:=t^.x+_x0; sx:=t^.rg^.x-t^.x;
  y0:=t^.y+_y0; sy:=t^.up^.y-t^.y;
  tile.insert(_ws^.box[class.tlay[t^.type]],t^.type,x0,y0,sx,sy,_tiletype);
  RETURN FALSE
END _paste;

PROCEDURE paste(vd: VIEW; vs: VIEW; b: BLOCK);
  VAR i: INTEGER;
     ws: WSP;
    d,s: FVIEW;
BEGIN
  IF badview(s,vs) OR badmodel(s^.m) THEN RETURN END;
  IF badview(d,vd) OR badmodel(d^.m) THEN RETURN END;
  _x0:=b.x; _y0:=b.y;
  ws:=SYSTEM.ADR(s^.m^.iws);
  IF d^.ws#SYSTEM.ADR(d^.m^.iws) THEN ws:=SYSTEM.ADR(s^.m^.xws) END;
  _ws:=d^.ws;
  FOR i:=0 TO HIGH(ws^.box) DO
    tile.iterate(ws^.box[i],0,0,tile.wsize,tile.wsize,_paste)
  END;
  b.w:=s^.m^.round.w; b.h:=s^.m^.round.h;
  refreshblock(d,b)
END paste;

PROCEDURE include(d,s: VIEW; x,y: INTEGER);
BEGIN
  ASSERT(FALSE);
END include;

PROCEDURE delete(v: VIEW; x,y: INTEGER);
BEGIN
  ASSERT(FALSE);
END delete;

PROCEDURE setshow(v: VIEW; x,y: INTEGER;  show: INTEGER);
BEGIN
  ASSERT(FALSE);
END setshow;

PROCEDURE scalebox(v: VIEW; VAR db: BLOCK; sb: BLOCK);
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  WITH v^ DO
    sb.x:=sb.x+x; sb.y:=sb.y+y;
    db.x:=sb.x DIV s;  db.w:=(sb.x+sb.w-1) DIV s-db.x+1;
    db.y:=sb.y DIV s;  db.h:=(sb.y+sb.h-1) DIV s-db.y+1
  END
END scalebox;

PROCEDURE scalepoint(v: VIEW; VAR X,Y: INTEGER; x,y: INTEGER);
BEGIN
  IF v=NIL THEN bad_desc; RETURN END;
  WITH v^ DO
    X:=(x+x) DIV s;
    Y:=(y+y) DIV s
  END
END scalepoint;

PROCEDURE insbox(vv: VIEW; type: INTEGER; b: BLOCK);
  VAR v: FVIEW;
BEGIN
  IF badview(v,vv) OR badmodel(v^.m) THEN RETURN END;
  tile.insert(v^.ws^.box[class.tlay[type]],type,b.x,b.y,b.w,b.h,_tiletype);
  refreshround(v,b)
END insbox;

PROCEDURE instxt(vv: VIEW; x,y,cx,cy,type: INTEGER; VAL s: ARRAY OF CHAR);
  VAR len: INTEGER;
      txt: TEXT;
        b: BLOCK;
        v: FVIEW;
    x0,y0: INTEGER;
BEGIN
  IF badview(v,vv) OR badmodel(v^.m) THEN RETURN END;

  len:=bmg.lenght(pm.font,'%s',s);
  x0:=x+(len+v^.f.s-1) DIV (v^.f.s*2);
  y0:=y+(pm.font^.H+v^.f.s-1) DIV (v^.f.s*2);
  len:=(len+3) DIV 4;

  b.x:=x0-len DIV 2; b.w:=(len+3) DIV 4;
  b.y:=y0-(pm.font^.H+3) DIV 8; b.h:=(pm.font^.H+3) DIV 4;

  NEW(txt);
  IF NOT done THEN RETURN END;
  txt^.x0:=b.x; txt^.x1:=b.x+b.w-1;
  txt^.y0:=b.y; txt^.y1:=b.y+b.h-1;
  txt^.x:=cx;
  txt^.y:=cy;
  txt^.c:=type;
  NEW(txt^.s,HIGH(s)+1);
  IF NOT done THEN DISPOSE(txt); RETURN END;
  txt^.s:=s;
  inserttext(v^.ws^.text,txt);

  --tile.insert(v^.ws^.text,txt,b.x,b.y,b.w,b.h,_texttype);

  IF NOT done THEN
    done:=FALSE; error:=tile.error;
    DISPOSE(txt^.s); DISPOSE(txt); RETURN
  END;
  refreshround(v,b)
END instxt;

PROCEDURE _killtile(t0,t1: INTEGER): INTEGER;
BEGIN RETURN tile.spc
END _killtile;

PROCEDURE deltxt(vv: VIEW; x,y: INTEGER);
  VAR t: tile.TILE; p,pp: TEXT; b: BLOCK; i: INTEGER; v: FVIEW;
BEGIN
  IF badview(v,vv) OR badmodel(v^.m) THEN RETURN END;
  done:=TRUE;
  FOR i:=0 TO HIGH(v^.ws^.text) DO
    t:=tile.where(v^.ws^.text[i],x,y);
    IF t^.type#tile.spc THEN
      p:=ADDRESS(t^.type); pp:=NIL;
      WHILE p#NIL DO
        IF (p^.x1>=x)&(p^.y1>=y)&(p^.x0<=x)&(p^.y0<=y) THEN
          IF pp=NIL THEN t^.type:=INTEGER(p^.next) ELSE pp^.next:=p^.next END;
          tdispose(p);
          b.x:=t^.x; b.w:=t^.rg^.x-t^.x;
          b.y:=t^.y; b.h:=t^.up^.y-t^.y;
          IF t^.type=INTEGER(NIL) THEN
            tile.insert(v^.ws^.text[i],tile.spc,b.x,b.y,b.w,b.h,_killtile);
            done:=tile.done;
            IF NOT done THEN error:=tile.error; RETURN END
          END;
          refreshround(v,b);
          RETURN
        END;
        pp:=p; p:=p^.next
      END
    END
  END
END deltxt;

PROCEDURE insport(v: VIEW; x,y: INTEGER; s: ARRAY OF CHAR;  b: BLOCK);
BEGIN
  ASSERT(FALSE);
END insport;

PROCEDURE delport(v: VIEW; x,y: INTEGER);
BEGIN
  ASSERT(FALSE);
END delport;

PROCEDURE setxcross(t0,t1,r: INTEGER);
BEGIN
  IF NOT (t0 IN {0..31}) THEN bad_parm; RETURN END;
  IF NOT (t1 IN {0..31}) THEN bad_parm; RETURN END;
  class.xcross[t0,t1]:=r; class.xcross[t1,t0]:=r
END setxcross;

PROCEDURE settlay(lno,type: INTEGER);
BEGIN
  class.tlay[type]:=lno
END settlay;

PROCEDURE setlayno(lno: INTEGER);
BEGIN
  class.layno:=lno
END setlayno;

  VAR i: INTEGER;

BEGIN
  done:=TRUE; error:=err.ok; MAGIC:=123h;
  low.fill(class,0);
  low.fill(shape,{0    });
  low.fill(fFF  ,{0..31});
  low.zero(f00);
  low.fill(fGREED,{});
  FOR i:=0 TO (HIGH(fGREED)+1)*32-1 BY 4 DO INCL(fGREED[i DIV 32],i MOD 32) END
END teModel.
