IMPLEMENTATION MODULE Foto; (* Leo 05-Sep-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  scr: Screen;
IMPORT  bmg: BMG;
IMPORT  lzw: LZW12;
IMPORT  mem: Heap;
IMPORT  bio: BIO;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  low: lowLevel;
IMPORT  str: Strings;


IMPORT  std: StdIO;

TYPE
  PALETTE = ARRAY [0..15] OF CHAR;

  PICTURE =
    RECORD
      x,y    : INTEGER;
      w,h    : INTEGER;
      palette: PALETTE;
      size   : INTEGER;
      layer  : ARRAY [0..3] OF SYSTEM.ADDRESS;
    END;

  CATALOG = ARRAY [0..255] OF
    RECORD
      pos : INTEGER;
      len : INTEGER;
      name: ARRAY [0..7] OF CHAR;
    END;

VAR p: PICTURE;
    f: bio.FILE;
    o: bio.FILE;
    T: ARRAY CHAR OF CHAR;

PROCEDURE bmv(a,b,c,d,e: SYSTEM.ADDRESS); CODE cod.bmv END bmv;


PROCEDURE bits(a: SYSTEM.ADDRESS; bytes: INTEGER);
  VAR i: INTEGER;
      p: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
BEGIN
  p:=a; i:=0;
  REPEAT p^[i]:=T[p^[i]]; INC(i); bytes:=bytes-1 UNTIL bytes=0
END bits;

PROCEDURE undo;
  VAR i: INTEGER;
BEGIN
  bio.close(f);
  bio.purge(o);
  FOR i:=0 TO 3 DO mem.deallocate(p.layer[i],p.size) END
;IF NOT done THEN ;HALT(error) END;
END undo;

PROCEDURE bio_error;
BEGIN done:=FALSE; error:=bio.error; undo END bio_error;

PROCEDURE mem_error;
BEGIN done:=FALSE; error:=mem.error; undo END mem_error;

PROCEDURE no_entry;
BEGIN done:=FALSE; error:=err.no_entry; undo END no_entry;

PROCEDURE not_enough;
BEGIN done:=FALSE; error:=err.not_enough; undo END not_enough;

PROCEDURE setpalette(VAL p: PALETTE);
  VAR i,r,g,b: INTEGER;
BEGIN
  FOR i:=0 TO 15 DO
    r:=ORD(p[i]) DIV 16;
    g:=ORD(p[i]) DIV 4 MOD 4;
    b:=ORD(p[i]) MOD 4;
    scr.state^.pal[i].r:=r;
    scr.state^.pal[i].g:=g;
    scr.state^.pal[i].b:=b;
  END;
  scr.set_palette(scr.state^.pal,0,16)
END setpalette;

PROCEDURE getpalette(VAR p: PALETTE);
  VAR i,r,g,b: INTEGER;
BEGIN
  FOR i:=0 TO 15 DO
    r:=scr.state^.pal[i].r;
    g:=scr.state^.pal[i].g;
    b:=scr.state^.pal[i].b;
    p[i]:=CHAR(r*16+g*4+b);
  END
END getpalette;

PROCEDURE opencat(VAL filename: ARRAY OF CHAR);
  VAR c: CATALOG;
BEGIN
  bio.open(f,filename,'rw');
  IF bio.done THEN RETURN END;
  bio.create(f,filename,'rw',BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  low.zero(c);
  bio.put(f,c,BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.seek(f,0,0);
  IF NOT bio.done THEN bio_error; RETURN END;
END opencat;

VAR buf: ARRAY [0..1024*8-1] OF CHAR;

PROCEDURE copy(len: INTEGER);
  VAR i: INTEGER;
BEGIN
  WHILE len>0 DO
    i:=BYTES(buf);
    IF i>len THEN i:=len END;
    bio.get(f,buf,i);
    IF NOT bio.done THEN bio_error; RETURN END;
    bio.put(o,buf,i);
    IF NOT bio.done THEN bio_error; RETURN END;
    DEC(len,i)
  END
END copy;

PROCEDURE compress(VAR c: CATALOG; filename: ARRAY OF CHAR);
  VAR i,sum: INTEGER;
BEGIN
  sum:=0;
  FOR i:=0 TO HIGH(c) DO
    IF c[i].pos#0 THEN INC(sum,c[i].len) ELSE c[i].len:=0 END
  END;
  bio.create(o,filename,'rw',BYTES(c)+sum);
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.put(o,c,BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  FOR i:=0 TO HIGH(c) DO
    IF (c[i].pos#0) & (c[i].name#"") & (c[i].len#0) THEN
      bio.seek(f,c[i].pos,0);
      IF NOT bio.done THEN bio_error; RETURN END;
      c[i].pos:=bio.eof(o);
      bio.seek(o,c[i].pos,0);
      IF NOT bio.done THEN bio_error; RETURN END;
      copy(c[i].len);
      IF NOT done THEN RETURN END
    ELSE
      c[i].pos:=0;
      c[i].len:=0;
      c[i].name:="";
    END
  END;
  bio.seek(o,0,0);
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.put(o,c,BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.close(o);
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.purge(f)
END compress;

PROCEDURE foto(B: bmg.BITMAP; x,y,w,h: INTEGER; VAL filename,picname: ARRAY OF CHAR);
  VAR c: CATALOG;
    i,j: INTEGER;
    a,d: SYSTEM.ADDRESS;
    ent: INTEGER;
   free: INTEGER;
   xywh: RECORD xy: INTEGER; wh: INTEGER END;
BEGIN
  done:=TRUE;
  undo;
  p.x:=x;  p.w:=w;
  p.y:=y;  p.h:=h;
  p.size:=(p.w+31) DIV 32 * p.h;
std.print("%d %d %d %d\n",p.x,p.y,p.w,p.h);

  opencat(filename);
  IF NOT done THEN RETURN END;
  bio.get(f,c,BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  i:=0; free:=-1;
  WHILE (i<=HIGH(c)) & (c[i].name#picname) DO
    IF (free<0) & (c[i].pos=0) THEN free:=i END;
    INC(i)
  END;
  IF i>HIGH(c) THEN
    IF free<0 THEN not_enough ELSE i:=free END;
  END;
  ent:=i;
  c[ent].pos:=bio.eof(f);
  c[ent].len:=0;

  FOR i:=0 TO 3 DO
    mem.allocate(p.layer[i],p.size);
    IF NOT mem.done THEN mem_error; RETURN END
  END;
  FOR i:=0 TO 3 DO
    a:=p.layer[i];
    bmg.offset(B,p.x,p.y+p.h-1,i,d,x);
    FOR j:=0 TO p.h-1 DO
      low._zero(a,(p.w+31) DIV 32);
      bmv(a,0,d,x,p.w);
      bits(a,(p.w+7) DIV 8);
      INC(a,(p.w+31) DIV 32);
      INC(d,B^.WPL)
    END
  END;

  bio.seek(f,c[ent].pos,0);
  IF NOT bio.done THEN bio_error; RETURN END;


  xywh.xy:=p.x+(scr.state^.H-(p.y+p.h-1))*10000h;
  xywh.wh:=p.w+p.h*10000h;
  bio.put(f,xywh,BYTES(xywh));
  IF NOT bio.done THEN bio_error; RETURN END;

  getpalette(p.palette);
  bio.put(f,p.palette,BYTES(p.palette));
  IF NOT bio.done THEN bio_error; RETURN END;

  FOR i:=0 TO 3 DO
    bio.create(o,"",'rw',p.size*4);
    IF NOT bio.done THEN bio_error; RETURN END;
    bio.write(o,p.layer[i],p.size*4);
    IF NOT bio.done THEN bio_error; RETURN END;
    bio.seek(o,0,0);
    IF NOT bio.done THEN bio_error; RETURN END;
    lzw.pack(o,f,p.size*4,j,FALSE);
    bio.purge(o);
    IF NOT bio.done THEN bio_error; RETURN END
  END;

  c[ent].len:=bio.eof(f)-c[ent].pos;
  str.copy(c[ent].name,picname);
  bio.seek(f,0,0);
  IF NOT bio.done THEN bio_error; RETURN END;

  bio.put(f,c,BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  compress(c,filename);
  undo
END foto;

PROCEDURE show(B: bmg.BITMAP; VAL filename,picname: ARRAY OF CHAR);
  VAR c: CATALOG;
      p: PICTURE;
    a,d: SYSTEM.ADDRESS;
   xywh: RECORD xy: INTEGER; wh: INTEGER END;
  i,j,x: INTEGER;
BEGIN
  undo;
  o:=bio.null;
  bio.open(f,filename,'r');
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.get(f,c,BYTES(c));
  IF NOT bio.done THEN bio_error; RETURN END;
  i:=0;
  WHILE (i<=HIGH(c)) & (c[i].name#picname) DO INC(i) END;
  IF i>HIGH(c) THEN no_entry; RETURN END;
  bio.seek(f,c[i].pos,0);
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.get(f,xywh,BYTES(xywh));
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.get(f,p.palette,BYTES(p.palette));
  IF NOT bio.done THEN bio_error; RETURN END;
  p.x:=xywh.xy MOD 10000h;
  p.y:=xywh.xy DIV 10000h;
  p.w:=xywh.wh MOD 10000h;
  p.h:=xywh.wh DIV 10000h;
std.print("show: %d %d %d %d\n",p.x,p.y,p.w,p.h);
  p.y:=scr.state^.H-p.y-p.h+1;
std.print("show: %d %d %d %d\n",p.x,p.y,p.w,p.h);
  p.size:=(p.w+31) DIV 32 * p.h;

  FOR i:=0 TO 3 DO
    mem.allocate(p.layer[i],p.size);
    IF NOT mem.done THEN mem_error; RETURN END
  END;
  FOR i:=0 TO 3 DO
    bio.create(o,"",'rw',p.size*4);
    IF NOT bio.done THEN bio_error; RETURN END;
    lzw.unpack(f,o,j);
    ASSERT(j=p.size*4);
    bio.seek(o,0,0);
    IF NOT bio.done THEN bio_error; RETURN END;
    bio.read(o,p.layer[i],p.size*4);
    IF NOT bio.done THEN bio_error; RETURN END;
    bio.purge(o);
    IF NOT bio.done THEN bio_error; RETURN END
  END;
  setpalette(p.palette);
  FOR i:=0 TO 3 DO
    a:=p.layer[i];
    bmg.offset(B,p.x,p.y+p.h-1,i,d,x);
    FOR j:=0 TO p.h-1 DO
      bits(a,(p.w+7) DIV 8);
      bmv(d,x,a,0,p.w);
      INC(a,(p.w+31) DIV 32);
      INC(d,B^.WPL)
    END
  END;
  undo
END show;

VAR i,j: INTEGER;  s: BITSET;

BEGIN
  FOR i:=0 TO 255 DO
    s:={};
    FOR j:=0 TO 7 DO
     IF j IN BITSET(i) THEN INCL(s,7-j) END
    END;
    T[CHAR(i)]:=CHAR(s)
  END;
  done:=TRUE;
  error:=0;
  FOR i:=0 TO 3 DO p.layer[i]:=NIL END;  p.size:=0; f:=bio.null
END Foto.
