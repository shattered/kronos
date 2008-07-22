MODULE pf; (* $X+brd  07-Mar-91. (c) KRONOS *)
           (*    brd  11-Jun-91. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  key: Keyboard;
IMPORT  mou: CPD;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  wnd: pmWnd;
IMPORT  bpm: bcPM;
IMPORT  mem : Heap;
IMPORT  tty: Terminal;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

TYPE BUFFER = DYNARR OF INTEGER;
     Char   = DYNARR OF BITSET;
     FONT   = RECORD
                w,h  : INTEGER;
                patt : ARRAY CHAR OF Char;
                propW: ARRAY CHAR OF INTEGER;
                magic: INTEGER;
              END;

CONST step=8;

VAR   bFONT: BUFFER;
       edit: FONT;
        sou: INTEGER;
      fname: ARRAY [0..31]  OF CHAR;
       name: ARRAY [0..255] OF CHAR;
     emagic: INTEGER;
         km: INTEGER;

      tsel: bpm.TABLET;
      tctr: bpm.TABLET;
      ewnd: wnd.WINDOW;
      desk: wnd.WINDOW;

  VAR state: BITSET;

PROCEDURE clear_mou;
 VAR i,x,y: INTEGER; b: BITSET;
BEGIN
  FOR i:=0 TO mou.ready()-1 DO mou.read(x,y,b) END
END clear_mou;

PROCEDURE x_read(VAR c: CHAR; VAR x,y: INTEGER;);
VAR s: BITSET;
    S: INTEGER;
BEGIN
  LOOP
    IF key.ready()>0 THEN key.read(c); x:=0; y:=0; EXIT END;
    IF mou.ready()>0 THEN
      mou.read(x,y,s);
      IF s = state THEN c:=0c; RETURN  END;
      state:=s; S:=INTEGER(s);
      CASE S OF
         INTEGER({0})  : c:=1c; clear_mou; RETURN
        |INTEGER({1})  : c:=2c; clear_mou; RETURN
        |INTEGER({2})  : c:=3c; clear_mou; RETURN
      ELSE  c:=0c; RETURN END
    END
  END
END x_read;

PROCEDURE pack(x,y,X,Y: INTEGER): BITSET;
  VAR data: BITSET;
BEGIN data:={};
  data:=data + BITSET(Y)*{0..7}; data:=data << 8;
  data:=data + BITSET(X)*{0..7}; data:=data << 8;
  data:=data + BITSET(y)*{0..7}; data:=data << 8;
  data:=data + BITSET(x)*{0..7}; RETURN data
END pack;

PROCEDURE un_pack(data: BITSET; VAR x,y,X,Y: INTEGER);
BEGIN
  x:=INTEGER(data*{0..7}); data:=data >> 8;
  y:=INTEGER(data*{0..7}); data:=data >> 8;
  X:=INTEGER(data*{0..7}); data:=data >> 8;
  Y:=INTEGER(data*{0..7});
END un_pack;

PROCEDURE ins(VAR wrk: Char; inx: INTEGER; data: BITSET);
  VAR i: INTEGER;
BEGIN
  IF (inx>HIGH(wrk)+1) THEN RETURN END;
  RESIZE(wrk,HIGH(wrk)+2);
  IF (inx=HIGH(wrk)) THEN wrk[inx]:=data; RETURN END;
  FOR i:=HIGH(wrk)-1 TO inx BY -1 DO wrk[i+1]:=wrk[i] END;
  wrk[inx]:=data;
END ins;

PROCEDURE del(VAR wrk: Char; inx: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF inx>HIGH(wrk) THEN RETURN END;
  FOR i:=inx TO HIGH(wrk)-1 DO wrk[i]:=wrk[i+1] END;
  RESIZE(wrk,HIGH(wrk));
END del;

PROCEDURE rect(W: wnd.WINDOW; T: wnd.TOOL; b: wnd.BLOCK);
BEGIN WITH b DO wnd.rect(W,T,x,y,x+w,y+h) END END rect;

PROCEDURE init_desk;
  VAR b: wnd.BLOCK;
      T: wnd.TOOL;
BEGIN
  wnd.resize(desk,480,360);
  T:= desk^.inner;
  WITH T.clip DO x:=0; y:=0; h:=358; w:=478 END;
  WITH b      DO x:=3; y:=3; h:=334; w:=472 END;
  bpm.panel(desk,T.clip,b,TRUE,FALSE);
  T.color:=bpm.shadow;
  rect(desk,T,b);
  WITH b DO x:=3; h:=bpm.font^.H+6; y:=337; w:=472 END;
  bpm.block(desk,b,TRUE,FALSE);
  T.color:=bpm.normal;
  T.back:={};
  T.mode:=wnd.bic;
  wnd.print(desk,T,b.x+15,b.y+4,bpm.font,
   '  Plotter Font Editor V1.41  %c KRONOS 1991 ',260c);
  T.color:=bpm.black;
  T.back:= bpm.shadow;
  T.mode:= wnd.rep;
  wnd.print(desk,T,325,130,bpm.font,'   USSR, Novosibirsk ');
  wnd.print(desk,T,325,115,bpm.font,'   p. Lavrentjeva 6');
  wnd.print(desk,T,325,100 ,bpm.font,'       Institut of ');
  wnd.print(desk,T,325,85 ,bpm.font,' Informatics System,');
  wnd.print(desk,T,322,70 ,bpm.font,'Kronos Research Group,');
  wnd.print(desk,T,325,55 ,bpm.font,' tel. 355067, box 503,');
  wnd.print(desk,T,325,40 ,bpm.font,'  W. Malukh  "Beard"');
END init_desk;

PROCEDURE init_work;
  VAR  b,b1,b2: wnd.BLOCK;
     bg1,bg2: wnd.TOOL;
     X0,Y0,i,j: INTEGER;
           s: ARRAY [0..13] OF CHAR;
           T: wnd.TOOL;
BEGIN
  wnd.close(ewnd);
  wnd.inner(ewnd,0,0,ewnd^.w,ewnd^.h);
  IF edit.w<12 THEN
    wnd.resize(ewnd,wnd.lenght(bpm.font,'char= 000c [Щ]')+12,
                               (edit.h+1)*km+bpm.font^.H*2+36);
  ELSE
    wnd.resize(ewnd,edit.w*km+36,(edit.h+1)*km+bpm.font^.H*2+36)
  END;
  X0:=22; Y0:=bpm.font^.H+24;
  WITH ewnd^.full.clip DO b.x:=x+3; b.w:=w-6; b.y:=x+3; b.h:=h-6 END;
  bpm.panel(ewnd,ewnd^.inner.clip,b,TRUE,FALSE);
  wnd.move(ewnd,(desk^.w-ewnd^.w) DIV 2,(desk^.h-ewnd^.h) DIV 2);
  T:=ewnd^.full;
  WITH b2 DO
    x:=X0-km-3; y:=Y0-km-3;
    w:=(edit.w+1)*km+6;
    h:=(edit.h+1)*km+6
  END;
  WITH b1 DO x:=b2.x-4; y:=b2.y-4; w:=b2.w+8; h:=b2.h+8 END;
  bpm.panel(ewnd,b1,b2,FALSE,FALSE);
  T.color:= bpm.normal;
  T.back:={};
  T.mode:=wnd.bic;
  wnd.rect(ewnd,T,b2.x-1,b2.y,b2.x+b2.w,b2.y+b2.h);
  wnd.inner(ewnd,X0,Y0,km*edit.w,km*edit.h)
END init_work;

PROCEDURE show_name;
  VAR w: wnd.WINDOW;
      T: wnd.TOOL;
      b: wnd.BLOCK;
BEGIN
   bpm.twindow(tsel,w);
   bpm.ttitle(tsel,b);
   T:= w^.inner;
   T.mode:=wnd.rep;
   T.clip:=b;
   T.color:=bpm.normal;
   rect(w,T,b);
   T.mode:=wnd.bic;
   T.color:=bpm.normal;
   T.back :={};
   wnd.print(w,T,b.x+4,b.y+1,bpm.font,'%s  w=%d  h=%d',fname,edit.w,edit.h)
END show_name;

VAR whelp: wnd.WINDOW;

PROCEDURE _help;
  VAR c: CHAR;
BEGIN
  wnd.ontop(whelp); wnd.open(whelp);
  key.read(c);  wnd.close(whelp)
END _help;

PROCEDURE _edit(ch: CHAR);
VAR X,Y,x,y: INTEGER;
       MAIN: wnd.TOOL;
        pen: BOOLEAN;
        wrk: Char;

PROCEDURE _coord;
  VAR s1,s2: ARRAY [0..5] OF CHAR;
      T: wnd.TOOL;
BEGIN
  T:= ewnd^.full;
  T.color:=bpm.black;
  T.back:= bpm.normal;
  T.mode:=wnd.rep;
  wnd.mode(ewnd,wnd.scr);
  wnd.print(ewnd,T,10,8,bpm.font,'x=%$3d y=%$3d  ',x,y);
  wnd.mode(ewnd,wnd.normal)
END _coord;

PROCEDURE in(x,y,X,Y,xa,ya: INTEGER): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  IF (x=X) THEN
    RETURN (xa=X) & ((y<=ya) & (ya<=Y) OR (Y<=ya) & (ya<=y))
  ELSIF (y=Y) THEN
    RETURN (ya=Y) & ((x<=xa) & (xa<=X) OR (X<=xa) & (xa<=x))
  ELSE
    x:=x-X; y:=y-Y; xa:=xa-X; ya:=ya-Y;
    RETURN (xa*y=ya*x)&(ABS(xa)<=ABS(x))&(ABS(ya)<=ABS(y))
  END;
END in;

PROCEDURE put(x,y,X,Y: INTEGER);
  VAR i,xo,yo,Xo,Yo,sx,sy: INTEGER; s: BOOLEAN;
      xy_in_o,XY_in_o: BOOLEAN;
      o_in_xy,o_in_XY: BOOLEAN;

  PROCEDURE put0(x,y,X,Y,Xo,Yo,xo,yo: INTEGER): BOOLEAN;
  BEGIN
    IF (X=Xo) & (Y=Yo) THEN
      IF in(x,y,xo,yo,X,Y) THEN del(wrk,i); put(x,y,xo,yo);
      ELSE RETURN FALSE
      END;
    ELSE
      wrk[i]:=pack(X,Y,xo,yo); put(x,y,Xo,Yo);
    END;
    RETURN TRUE
  END put0;

BEGIN i:=0;
  WHILE (i<=HIGH(wrk)) DO
    un_pack(wrk[i],xo,yo,Xo,Yo);
    xy_in_o:=in(xo,yo,Xo,Yo,x,y);  XY_in_o:=in(xo,yo,Xo,Yo,X,Y);
    o_in_xy:=in(x,y,X,Y,xo,yo);    o_in_XY:=in(x,y,X,Y,Xo,Yo);
    IF xy_in_o & XY_in_o THEN
      IF in(xo,yo,x,y,X,Y) THEN
        sx:=X; X:=x; x:=sx; sy:=Y; Y:=y; y:=sy
      END;
      IF o_in_xy & o_in_XY THEN del(wrk,i); RETURN END;
      IF o_in_xy THEN wrk[i]:=pack(X,Y,Xo,Yo); RETURN END;
      IF o_in_XY THEN wrk[i]:=pack(x,y,xo,yo); RETURN END;
      wrk[i]:=pack(xo,yo,x,y); ins(wrk,HIGH(wrk)+1,pack(X,Y,Xo,Yo));
      RETURN
    END;
    IF o_in_xy & o_in_XY THEN del(wrk,i);
      IF in(x,y,xo,yo,Xo,Yo) THEN
        sx:=Xo; Xo:=xo; xo:=sx; sy:=Yo; Yo:=yo; yo:=sy
      END;
      IF xy_in_o & XY_in_o THEN ASSERT(FALSE); END;
      IF xy_in_o THEN put(X,Y,Xo,Yo); RETURN END;
      IF XY_in_o THEN put(x,y,xo,yo); RETURN END;
      put(x,y,xo,yo); put(Xo,Yo,X,Y);
      RETURN
    END;
    IF xy_in_o & o_in_xy & put0(X,Y,x,y,xo,yo,Xo,Yo) THEN RETURN END;
    IF XY_in_o & o_in_XY & put0(x,y,X,Y,Xo,Yo,xo,yo) THEN RETURN END;
    IF xy_in_o & o_in_XY & put0(X,Y,x,y,Xo,Yo,xo,yo) THEN RETURN END;
    IF XY_in_o & o_in_xy & put0(x,y,X,Y,xo,yo,Xo,Yo) THEN RETURN END;
    INC(i);
  END;
  ins(wrk,HIGH(wrk)+1,pack(x,y,X,Y));
END put;

PROCEDURE line; BEGIN wnd.line(ewnd,MAIN,x*km, y*km,X*km,Y*km) END line;

PROCEDURE make;
  VAR i: INTEGER;
      T: wnd.TOOL;
      b: wnd.BLOCK;
      X0,Y0: INTEGER;
BEGIN
  T:=ewnd^.full;
  T.color:=bpm.black;
  T.back :=bpm.normal;
  T.mode :=wnd.rep;
  wnd.print(ewnd,T,12,ewnd^.h-bpm.font^.H-8,bpm.font,'char=%$3oc[%c] ',ch,ch);
  MAIN:= ewnd^.inner;
  MAIN.mode:= wnd.rep;
  MAIN.color:= bpm.black;
  WITH MAIN.clip DO wnd.rect(ewnd,MAIN,x,y,x+w,y+h) END;
  MAIN.mode:= wnd.rep;
  MAIN.color:= bpm.shadow;
  wnd.grid(ewnd,MAIN,MAIN.clip,km,km);
  X0:= ewnd^.inner.zX;
  Y0:= ewnd^.inner.zY;
  T.color:=bpm.shadow;
  FOR i:=0 TO edit.w-1 BY 5 DO
    wnd.line(ewnd,T,X0 + i*km,Y0-5,X0 + i*km,Y0 + edit.h*km-1)
  END;
  FOR i:=0 TO edit.h-1 BY 5 DO
    wnd.line(ewnd,T,X0-5,Y0 + i*km,X0+ edit.w*km-1,Y0+i*km)
  END;
  MAIN.color:= bpm.normal;
  FOR i:=0 TO HIGH(wrk) DO
    un_pack(wrk[i],x,y,X,Y); line
  END;
  MAIN.mode:= wnd.xor;
  MAIN.color:={0};
  WITH edit DO wnd.line(ewnd,MAIN,propW[ch]*km,0,propW[ch]*km,h*km) END;
  MAIN.color:= bpm.normal
END make;

PROCEDURE cursor;
  VAR l,c: INTEGER;
BEGIN
  IF pen THEN line ELSE X:=x; Y:=y END;
  l:=y*km; c:=x*km;
  wnd.frame(ewnd,MAIN,c-2,l-2,c+2,l+2)
END cursor;

PROCEDURE up_prop;
 VAR T: wnd.TOOL;
BEGIN
  T:= ewnd^.inner;
  T.color:={0};
  T.mode:=wnd.xor;
  WITH edit DO
    IF propW[ch]< w THEN
      wnd.line(ewnd,T,propW[ch]*km,0,propW[ch]*km,h*km);
      INC(propW[ch]);
      wnd.line(ewnd,T,propW[ch]*km,0,propW[ch]*km,h*km)
    END
  END
END up_prop;

PROCEDURE dw_prop;
 VAR T: wnd.TOOL;
BEGIN
  T:= ewnd^.inner;
  T.color:={0};
  T.mode:=wnd.xor;
  WITH edit DO
    IF propW[ch]>0 THEN
      wnd.line(ewnd,T,propW[ch]*km,0,propW[ch]*km,h*km);
      DEC(propW[ch]);
      wnd.line(ewnd,T,propW[ch]*km,0,propW[ch]*km,h*km)
    END
  END
END dw_prop;

VAR mx,my: INTEGER;
       kb: CHAR;
BEGIN
  wnd.ontop(ewnd);
  NEW(wrk,HIGH(edit.patt[ch])+1);
  wrk:=edit.patt[ch];
  make;
  wnd.open(ewnd);
  x:=0; y:=0; X:=0; Y:=0; pen:=FALSE;
  mx:=0; my:=0;
  LOOP
    _coord;
    cursor;
    x_read(kb,mx,my);
    cursor;
    CASE kb OF
      key.left    : x:=(x-1+edit.w) MOD edit.w
     |key.right   : x:=(x+1+edit.w) MOD edit.w
     |key.up      : y:=(y+1+edit.h) MOD edit.h
     |key.dw      : y:=(y-1+edit.h) MOD edit.h
     |key.end     : x:=(x-1+edit.w) MOD edit.w; y:=(y-1+edit.h) MOD edit.h
     |key.home    : y:=(y+1+edit.h) MOD edit.h; x:=(x-1+edit.w) MOD edit.w
     |key.pgup    : x:=(x+1+edit.w) MOD edit.w; y:=(y+1+edit.h) MOD edit.h
     |key.pgdw    : x:=(x+1+edit.w) MOD edit.w; y:=(y-1+edit.h) MOD edit.h
     |3c,key.del  : pen:=FALSE;
     |2c,key.ins  : pen:=TRUE;
     |key.cr      : IF pen THEN pen:=FALSE ELSE pen:=TRUE END;
     |1c,' '      : IF pen THEN
                      line; put(x,y,X,Y);
                    END;
                    X:=x; Y:=y
     |'r','R'    : make
     |'+'        : up_prop;
     |'-'        : dw_prop;
     |'w','W'    : RESIZE(edit.patt[ch],HIGH(wrk)+1);
                   edit.patt[ch]:=wrk;
     |'e','E'    : EXIT;
     |'q','Q',33c: wnd.close(ewnd); DISPOSE(wrk); RETURN
     |'h','H'    : _help
    ELSE
      INC(x,mx DIV 2); x:=x MOD edit.w;
      INC(y,my DIV 2); y:=y MOD edit.h
    END
  END;
  RESIZE(edit.patt[ch],HIGH(wrk)+1);
  edit.patt[ch]:=wrk;
  wnd.close(ewnd);
  DISPOSE(wrk)
END _edit;

PROCEDURE _select(VAR ch: CHAR): BOOLEAN;
  VAR w: wnd.WINDOW;
      b: wnd.BLOCK;
      i: INTEGER;
BEGIN
  bpm.twindow(tsel,w);
  FOR i:=0 TO 255 DO
    bpm.tblocks(tsel,i,b);
    IF bpm.inblock(bpm.mx,bpm.my,w,b) THEN
      ch:=CHAR(i);
      bpm.tchoose(tsel,i);
      RETURN TRUE
    END
  END;
  RETURN FALSE
END _select;

----------------------------- files ----------------------------

PROCEDURE _quit;
BEGIN
  IF bpm.query(340,180,'Quit without writting?','Yes','No','y','n',TRUE,FALSE) THEN
    HALT END
END _quit;

PROCEDURE ext(s: ARRAY OF CHAR): BOOLEAN;
  VAR h: INTEGER;
     dn: BOOLEAN;
BEGIN
  h:=str.len(s)-4;
  IF h<0 THEN RETURN FALSE END;
  str.scan(s,h,'.plf',dn);
  RETURN dn
END ext;

  VAR dbx: bpm.DIREX;

PROCEDURE font?(VAR file,font: ARRAY OF CHAR): BOOLEAN;
BEGIN
  bpm.dopen(dbx);
  bpm.dselect(dbx);
  IF bpm.dselected(dbx) THEN
    bpm.dfullname(dbx,file);
    bpm.dfilename(dbx,font)
  END;
  bpm.dclose(dbx);
  RETURN ext(font)
END font?;

PROCEDURE e;
BEGIN
  IF NOT bio.done THEN
    bpm.perror(bio.error,240,180,'error in file "%s"\n',name)
  END
END e;

PROCEDURE _write;
  VAR fn,nm: ARRAY [0..255] OF CHAR;
      fsize,pos,i: INTEGER;
                f: bio.FILE;
               ch: CHAR;
BEGIN
  IF NOT font?(fname,name) THEN  RETURN  END;
  fsize:=0;
  FOR ch:=0c TO 377c DO fsize:=fsize + HIGH(edit.patt[ch]) + 3 END;
  INC(fsize,2);
  RESIZE(bFONT,fsize);
  fsize:=fsize*4;
  bFONT[0]:=edit.w; bFONT[1]:=edit.h; pos:=2;
  FOR ch:=0c TO 377c DO
    bFONT[pos]:=HIGH(edit.patt[ch]); INC(pos);
    FOR i:=0 TO HIGH(edit.patt[ch]) DO
      bFONT[pos]:=INTEGER(edit.patt[ch][i]); INC(pos)
    END
  END;
  FOR ch:=0c TO 377c DO bFONT[pos]:= edit.propW[ch]; INC(pos) END;
  bio.create(f,name,'w',fsize); e;
  bio.put(f,bFONT,fsize); e;
  bio.close(f); e;
END _write;

PROCEDURE _load;
  VAR nm,font: ARRAY [0..31] OF CHAR;
      ch: CHAR;
       f: bio.FILE;
      fsize,pos,i,j: INTEGER;
BEGIN
  IF font?(nm,font) THEN
    FOR ch:=0c TO 377c DO NEW(edit.patt[ch]) END
  ELSE RETURN END;
  bio.open(f,nm,'r');
  IF NOT bio.done THEN
    bpm.message(240,180,'Files are not found: %s \n',nm); RETURN
  END;
  str.print(name,'%s',nm);
  str.print(fname,'%s',font);
  fsize:=bio.eof(f);
  NEW(bFONT,(fsize+3) DIV 4 + 1);
  bio.get(f,bFONT,fsize); e;
  bio.purge(f);
  edit.w:=bFONT[0]; edit.h:=bFONT[1];
  IF (edit.w > 72) OR (edit.h>48) THEN km:=4 ELSE km:=6 END;
  show_name;
  pos:=2;
  WITH edit DO
    FOR ch:=0c TO 377c DO
      RESIZE(patt[ch],bFONT[pos]+1); INC(pos);
      FOR i:=0 TO HIGH(patt[ch]) DO
        patt[ch][i]:=BITSET(bFONT[pos]); INC(pos);
      END
    END;
    FOR ch:=0c TO 377c DO
      propW[ch]:= bFONT[pos]; INC(pos);
    END
  END
END _load;

---------------------------- create ----------------------------

PROCEDURE _create;

PROCEDURE rd_numb(prompt: ARRAY OF CHAR; VAR x:INTEGER): BOOLEAN;
  VAR s: ARRAY [0..31] OF CHAR;
      p: INTEGER;
     dn: BOOLEAN;
BEGIN
  p:=0; s:=''; dn:=TRUE;
  bpm.diabox(245,12,70,s,prompt);
  IF (s='') THEN  RETURN FALSE END;
  str.iscan(x,s,p,dn);
  IF x<1 THEN x:=1 END;
  IF dn THEN RETURN TRUE END;
  RETURN FALSE;
END rd_numb;

  VAR ch: CHAR;
BEGIN
  IF NOT rd_numb(' W grid:',edit.w) THEN RETURN END;
  IF NOT rd_numb(' H grid:',edit.h) THEN RETURN END;
  name:='';
  str.print(fname,'%s',name);
  IF (edit.w > 72) OR (edit.h>48) THEN km:=4 ELSE km:=6 END;
  FOR ch:=0c TO 377c DO
    NEW(edit.patt[ch]);
    edit.propW[ch]:=edit.w
  END;
  show_name
END _create;

--------------------------- optimize ---------------------------

PROCEDURE optimize(ch: CHAR);
  VAR s,d: Char;

PROCEDURE make_net(VAR nt: Char);
  VAR ox,oy,oX,oY: INTEGER; x,y,X,Y: INTEGER; i,h: INTEGER; done: BOOLEAN;
BEGIN ASSERT(HIGH(s)>=0);
  ins(nt,0,s[0]);
  un_pack(s[0],ox,oy,oX,oY);
  del(s,0);
  REPEAT i:=0; done:=TRUE;
    WHILE (i<=HIGH(s)) DO h:=HIGH(nt)+1;
      un_pack(s[i],x,y,X,Y);
      IF    (x=ox) & (y=oy) THEN
        done:=FALSE; ins(nt,0,pack(X,Y,x,y)); del(s,i); ox:=X; oy:=Y;
      ELSIF (x=oX) & (y=oY) THEN
        done:=FALSE; ins(nt,h,pack(x,y,X,Y)); del(s,i); oX:=X; oY:=Y;
      ELSIF (X=ox) & (Y=oy) THEN
        done:=FALSE; ins(nt,0,pack(x,y,X,Y)); del(s,i); ox:=x; oy:=y;
      ELSIF (X=oX) & (Y=oY) THEN
        done:=FALSE; ins(nt,h,pack(X,Y,x,y)); del(s,i); oX:=x; oY:=y;
      ELSE INC(i);
      END;
    END;
  UNTIL done;
END make_net;

VAR sx,sy,x,y,X,Y,l0,l1,i,min,len: INTEGER;
                             nets: DYNARR OF Char;
                                h: INTEGER;
                              dim: BOOLEAN;
BEGIN
  (*$U+*) s^:=edit.patt[ch]^; (*$U-*)
  IF HIGH(s)<0 THEN RETURN END;
  h:=0;
  WHILE h<=HIGH(s) DO
    un_pack(s[h],x,y,X,Y);
    IF (x=X) & (y=Y) THEN s[h]:=s[HIGH(s)]; RESIZE(s,HIGH(s));
    ELSE INC(h)
    END;
  END;
  NEW(d); NEW(nets);
  WHILE (HIGH(s)>=0) DO
    RESIZE(nets,HIGH(nets)+2);
    NEW(nets[HIGH(nets)]);
    make_net(nets[HIGH(nets)]);
  END;
  sx:=0; sy:=0;
  REPEAT i:=0; min:=0; len:=MAX(INTEGER);
    WHILE (i<=HIGH(nets)) DO
      un_pack(nets[i][0],x,y,X,Y);
      l0:=(x-sx)*(x-sx) + (y-sy)*(y-sy);
      un_pack(nets[i][HIGH(nets[i])],x,y,X,Y);
      l1:=(X-sx)*(X-sx) + (Y-sy)*(Y-sy);
      IF l0<len THEN dim:=TRUE ; len:=l0; min:=i END;
      IF l1<len THEN dim:=FALSE; len:=l1; min:=i END;
      INC(i);
    END;
    h:=HIGH(nets[min]);
    IF dim THEN
      FOR i:=0 TO h DO ins(d,HIGH(d)+1,nets[min][i]) END;
      un_pack(d[HIGH(d)],x,y,sx,sy);
    ELSE
      FOR i:=0 TO h DO ins(d,HIGH(d)+1,(nets[min][h-i]) >> 16) END;
      un_pack(d[HIGH(d)],x,y,sx,sy);
    END;
    DISPOSE(nets[min]);
    FOR i:=min TO HIGH(nets)-1 DO
      (*$U+*) nets[i]^:=nets[i+1]^ (*$U-*)
    END;
    RESIZE(nets,HIGH(nets));
  UNTIL (HIGH(nets)<0);
  (*$U+*) edit.patt[ch]^:=d^; (*$U-*)
END optimize;

PROCEDURE _optimize;
  VAR c: CHAR;
      w: wnd.WINDOW;
      t: wnd.TOOL;
      b: wnd.BLOCK;
BEGIN
  bpm.twindow(tsel,w);
  t:= w^.inner;
  t.mode:=wnd.xor;
  t.color:= bpm.shadow;
  FOR c:=0c TO 377c DO
    bpm.tblocks(tsel,INTEGER(c),b);
    rect(w,t,b);
    optimize(c);
    rect(w,t,b)
  END
END _optimize;

PROCEDURE _source;
  VAR b: wnd.BLOCK;
      W: wnd.WINDOW;
      T: wnd.TOOL;
BEGIN
  bpm.tselect(tsel);
  IF NOT bpm.tselected(tsel) OR  (sou = bpm.talt(tsel)) THEN RETURN END;
  bpm.twindow(tsel,W);
  T:= W^.full;
  T.color:= bpm.bright;
  T.mode := wnd.xor;
  IF sou # -1 THEN
    bpm.tblocks(tsel,sou,b);
    wnd.rect(W,T,b.x+1,b.y+1,b.x+b.w-1,b.y+b.h-1)
  END;
  sou:= bpm.talt(tsel);
  bpm.tblocks(tsel,sou,b);
  wnd.rect(W,T,b.x+1,b.y+1,b.x+b.w-1,b.y+b.h-1);
  bpm.tblocks(tctr,4,b);
  bpm.twindow(tctr,W);
  T:= W^.full;
  T.color:= bpm.black;
  T.back := bpm.normal;
  T.mode := wnd.rep;
  wnd.print(W,T,b.x+3,b.y+2,bpm.font,'Source= %$3oc [%c] ',CHAR(sou),CHAR(sou));
  bpm.tunselect(tsel)
END _source;

PROCEDURE _duplicate;
  VAR s,d: CHAR;
        t: ARRAY [0..63] OF CHAR;
BEGIN
  IF sou=-1 THEN RETURN END;
  bpm.tselect(tsel);
  IF NOT bpm.tselected(tsel) THEN RETURN END;
  s:= CHAR(sou);
  d:= CHAR(bpm.talt(tsel));
  str.print(t,'Copy %$3os[%c] in %$3os[%c] ?',s,s,d,d);
  IF bpm.query(320,200,t,'Yes','No','y','n',TRUE,FALSE) THEN
    WITH edit DO
      RESIZE(patt[d],HIGH(patt[s])+1);
      propW[d]:=propW[s];
      patt[d]:=patt[s]
    END
  END;
  bpm.tunselect(tsel)
END _duplicate;

PROCEDURE monitor;
  VAR ch: CHAR;
BEGIN
  LOOP
    bpm.tselect(tctr);
    IF NOT bpm.tselected(tctr) THEN
      IF _select(ch) THEN _edit(ch); bpm.tunselect(tsel) END
    ELSE
      CASE bpm.talt(tctr) OF
         0: _write;       bpm.tunselect(tctr)
        |1: _load;        bpm.tunselect(tctr); init_work
        |2: _create;      bpm.tunselect(tctr); init_work
        |3: _optimize;    bpm.tunselect(tctr)
        |4: _source;      bpm.tunselect(tctr)
        |5: _duplicate;   bpm.tunselect(tctr)
        |6: _quit;        bpm.tunselect(tctr)
      ELSE
      END
    END
  END
END monitor;

PROCEDURE init_select;
  VAR i: INTEGER;
      T: wnd.TOOL;
      W: wnd.WINDOW;
      b: wnd.BLOCK;
BEGIN
  bpm.tnew(tsel,16,16,20,20,18,18,bpm.xsel,'  %s  w=%d  h=%d ',name,edit.w,edit.h);
  bpm.twindow(tsel,W);
  T:=W^.inner;
  T.color:=bpm.normal;
  T.back:={};
  T.mode:=wnd.bic;
  FOR i:=0 TO 255 DO
    bpm.tblocks(tsel,i,b);
    wnd.writech(W,T,b.x+3,b.y+2,bpm.font,CHAR(i))
  END
END init_select;

PROCEDURE init_control;
  VAR i: INTEGER;
      T: wnd.TOOL;
      W: wnd.WINDOW;
      b: wnd.BLOCK;
BEGIN
  bpm.tnew(tctr,1,7,330,182,120,18,bpm.xsel+bpm.xlf,'   Control');
  bpm.twindow(tctr,W);
  T:=W^.inner;
  T.color:=bpm.normal;
  T.back:={};
  T.mode:=wnd.bic;
  bpm.tblocks(tctr,0,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Write');
  bpm.tblocks(tctr,1,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Load ');
  bpm.tblocks(tctr,2,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Create');
  bpm.tblocks(tctr,3,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Optimize');
  bpm.tblocks(tctr,4,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Source');
  bpm.tblocks(tctr,5,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Duplicate');
  bpm.tblocks(tctr,6,b); wnd.print(W,T,b.x+3,b.y+2,bpm.font,' Quit');
END init_control;

PROCEDURE init_help;
 VAR b: wnd.BLOCK;
     t: wnd.TOOL;
BEGIN
  wnd.new(whelp);            ASSERT(wnd.done);
  wnd.resize(whelp,320,180); ASSERT(wnd.done);
  wnd.inner(whelp,0,0,whelp^.w,whelp^.h);
  wnd.move(whelp,80,100);
  b.x:= whelp^.inner.clip.x+2;
  b.y:= whelp^.inner.clip.y+2;
  b.w:= whelp^.inner.clip.w-4;
  b.h:= whelp^.inner.clip.h-4;
  bpm.panel(whelp,whelp^.inner.clip,b,TRUE,FALSE);
  t:=whelp^.inner;
  t.clip:=b;
  t.mode:=wnd.rep;
  t.back := bpm.normal;
  t.mask := {0..3};
  t.color:= {2};
  wnd.print(whelp,t,10,145,bpm.font,"e/E");
  wnd.print(whelp,t,10,130,bpm.font,'esc/q/Q ');
  wnd.print(whelp,t,10,115,bpm.font,'space/ 1 mou button ');
  wnd.print(whelp,t,10,100,bpm.font,'ins/ 2 mou button ');
  wnd.print(whelp,t,10, 85,bpm.font,'del/ 3 mou button ');
  wnd.print(whelp,t,10, 70,bpm.font,'+');
  wnd.print(whelp,t,10, 55,bpm.font,'-');
  wnd.print(whelp,t,10, 40,bpm.font,'r/R');
  wnd.print(whelp,t,10, 25,bpm.font,'w/W');

  t.color:= bpm.black;
  wnd.print(whelp,t,150,160,bpm.font,'HELP');
  wnd.print(whelp,t,140,145,bpm.font,"exit & save char ");
  wnd.print(whelp,t,140,130,bpm.font,'exit without save ');
  wnd.print(whelp,t,140,115,bpm.font,'vertex in drawning');
  wnd.print(whelp,t,140,100,bpm.font,'begin drawning');
  wnd.print(whelp,t,140, 85,bpm.font,'end drawning');
  wnd.print(whelp,t,140, 70,bpm.font,'increase prop Width');
  wnd.print(whelp,t,140, 55,bpm.font,'decrease prop Width');
  wnd.print(whelp,t,140, 40,bpm.font,'refresh ');
  wnd.print(whelp,t,140, 25,bpm.font,'write char ');

  wnd.print(whelp,t,50, 10,bpm.font,'Press any key to continue');

END init_help;

VAR ch: CHAR;
     i: INTEGER;

BEGIN
  FOR ch:=0c TO 377c DO NEW(edit.patt[ch]); edit.propW[ch]:=0; END;
  clear_mou;
  sou:=-1;
  edit.w:=1; edit.h:=1; km:=6;
  fname:=''; name:='';
  wnd.new(desk); init_desk;
  wnd.new(ewnd); init_work;
  init_select;
  init_help;
  init_control;
  bpm.dnew(dbx,340,130,100,120,{},'.','*.plf',bpm.standard);
  wnd.open(desk);
  bpm.topen(tctr);
  bpm.topen(tsel);
  wnd.open(ewnd);
  emagic:=65646974h;
  monitor
END pf.
