MODULE pfe; (* brd  07-Mar-91. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  kb : Keyboard;
IMPORT  mou: CPD;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  gph: pmWnd;
IMPORT  pup: pmPUP;
IMPORT  fnt: Fonts;
IMPORT  tim: Time;
IMPORT  scr: Screen;
IMPORT  hp : Heap;

WITH STORAGE: hp;

TYPE BUFFER = DYNARR OF INTEGER;
     Char   = DYNARR OF BITSET;
     FONT   = RECORD
                w,h  : INTEGER;
                patt : ARRAY CHAR OF Char;
                propW: ARRAY CHAR OF INTEGER;
              END;

CONST step=8;

VAR   bFONT: BUFFER;
       edit: FONT;
      fsize: INTEGER;
      X0,Y0: INTEGER;
      X,  Y: INTEGER;
 fname,name: ARRAY [0..31] OF CHAR;
       MAIN: gph.TOOL;
         km: INTEGER;

PROCEDURE move(VAR ch: CHAR): BOOLEAN;
  VAR d: INTEGER;
BEGIN
  d:=0;
  IF ABS(X)>step THEN INC(d,2);
    IF X<0 THEN INC(X,step); INC(d)
    ELSE DEC(X,step);
    END;
  END;
  IF ABS(Y)>step THEN INC(d,8);
    IF Y<0 THEN INC(Y,step); INC(d,4);
    ELSE DEC(Y,step);
    END
  END;
  CASE d OF
    | 0: RETURN FALSE
    | 2: ch:=kb.right; RETURN TRUE;
    | 3: ch:=kb.left ; RETURN TRUE;
    | 8: ch:=kb.up   ; RETURN TRUE;
    |10: ch:=kb.pgup ; RETURN TRUE;
    |11: ch:=kb.home ; RETURN TRUE;
    |12: ch:=kb.dw   ; RETURN TRUE;
    |14: ch:=kb.pgdw ; RETURN TRUE;
    |15: ch:=kb.end  ; RETURN TRUE;
  ELSE pup.message(240,180,'direction is %d !!!\n\n',d); ASSERT(FALSE);
  END;
END move;

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
    IF kb.ready()>0 THEN kb.read(c); x:=0; y:=0; EXIT END;
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

PROCEDURE read(VAR ch: CHAR);
  VAR x,y: INTEGER;
       st: BITSET;
BEGIN
  LOOP
    IF (kb.ready()#0) OR (mou.ready()#0) THEN
      x_read(ch,x,y);
      IF move(ch) OR (ch#0c) THEN  clear_mou; RETURN END;
      IF (x#0) OR (y#0) THEN INC(X,x); INC(Y,y) END
    END
  END;
END read;

PROCEDURE query(prompt: ARRAY OF CHAR): BOOLEAN;
 VAR m: pup.MENU;
       l: INTEGER;
BEGIN
  l:=gph.lenght(pup.font,prompt)+32;
  IF l<= 45 THEN l:=45 END;
  pup.mnew(m,360-l DIV 2,30,l,60,{},prompt);
  pup.mprint(m,0,"  Yes");  pup.mhotkey(m,0,"y",TRUE);
  pup.mprint(m,1,"  No ");  pup.mhotkey(m,1,"n",TRUE);
  pup.mopen(m);
  LOOP
    pup.mselect(m);
    IF NOT pup.mselected(m) THEN EXIT END;
    CASE pup.malt(m) OF
       0: pup.mclose(m); RETURN TRUE
      |1: pup.mclose(m); RETURN FALSE
    ELSE
    END;
  END;
  pup.mdispose(m);
  RETURN FALSE
END query;

VAR BC: ARRAY [0..8] OF gph.BLOCK;

PROCEDURE show_control;
  VAR b1,b2: gph.BLOCK;
          i: INTEGER;
BEGIN
  WITH b1 DO x:=310; y:=34; w:=150; h:=284 END;
  WITH b2 DO x:=314; y:=38; w:=142; h:=276 END;
  pup.panel(b1,b2,TRUE,FALSE);
  FOR i:=0 TO 8 DO
    WITH BC[i] DO
      x:=319; y:=270-22*i;
      w:=132; h:=18
    END;
    pup.button(BC[i],FALSE)
  END;
  gph.print(gph.desktop,MAIN,BC[0].x+31,BC[0].y+25,pup.font,' CONTROL  ');
  gph.print(gph.desktop,MAIN,BC[0].x+31,BC[0].y+3,pup.font,'   Write   ');
  gph.print(gph.desktop,MAIN,BC[1].x+31,BC[1].y+3,pup.font,'   Exit    ');
  gph.print(gph.desktop,MAIN,BC[2].x+31,BC[2].y+3,pup.font,'   Quit    ');
  gph.print(gph.desktop,MAIN,BC[3].x+31,BC[3].y+3,pup.font,'   Load    ');
  gph.print(gph.desktop,MAIN,BC[4].x+31,BC[4].y+3,pup.font,'   Name    ');
  gph.print(gph.desktop,MAIN,BC[5].x+31,BC[5].y+3,pup.font,'Source=    ');
  gph.print(gph.desktop,MAIN,BC[6].x+31,BC[6].y+3,pup.font,'  Duplicate');
  gph.print(gph.desktop,MAIN,BC[7].x+31,BC[7].y+3,pup.font,'  Optimize ');
  gph.print(gph.desktop,MAIN,BC[8].x+31,BC[8].y+3,pup.font,'   Create  ');
--  scr.paint(1,0,1,0);
--  scr.paint(INTEGER(pup.shadow),1,1,1);
--  scr.paint(INTEGER(pup.normal),2,2,2);
--  scr.paint(INTEGER(pup.bright),3,3,3);
END show_control;

PROCEDURE showfont;
  VAR b1,b2: gph.BLOCK;
   x,y,ch,n: INTEGER;
         st: ARRAY [0..48] OF CHAR;
          c: CHAR;
BEGIN
  WITH b1 DO x:=0; y:=20; w:=480; h:=321 END;
  WITH b2 DO x:=4; y:=24; w:=476; h:=317 END;
  MAIN.mode:= gph.rep;
  MAIN.color:= BITSET(7);
  gph.rect(gph.desktop,MAIN,0,0,480,361);
  pup.panel(b1,b2,TRUE,FALSE);
  WITH b2 DO x:=X0-2; y:=Y0-290; w:=276; h:=276 END;
  WITH b1 DO x:=b2.x-4; y:=b2.y-4; w:=b2.w+8; h:=b2.h+8 END;
  pup.panel(b1,b2,TRUE,FALSE);
  WITH b1 DO x:=2; y:=342; w:=477; h:=16 END; --title
  pup.button(b1,FALSE);
  b1.y:=2; b1.h:=16; --name
  pup.button(b1,FALSE);
  MAIN.color:= BITSET(0);
  MAIN.back:= BITSET(7);
  FOR n:=2 TO 16 DO gph.line(gph.desktop,MAIN,X0,Y0-n*17,X0+271,Y0-n*17) END;
  FOR n:=1 TO 15 DO gph.line(gph.desktop,MAIN,X0+n*17,Y0-17,X0+n*17,Y0-288) END;
  FOR n:=0 TO 255 DO
    gph.writech(gph.desktop,MAIN,X0 + (n MOD 16)*17 + 5,
                             Y0 - (n DIV 16 + 2)*17 +2,pup.font,CHAR(n));
  END;
  show_control;
  gph.print(gph.desktop,MAIN,24,344,pup.font,'  Plotter Font Editor V1.31  %c KRONOS 1991 ',260c);
  gph.write(gph.desktop,MAIN,6,4,pup.font,'Font:',0,5)
END showfont;

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
  (*$U+*)
  s^:=edit.patt[ch]^;
  (*$U-*)
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

PROCEDURE rd_numb(prompt: ARRAY OF CHAR; VAR x:INTEGER): BOOLEAN;
  VAR s: ARRAY [0..31] OF CHAR;
      p: INTEGER;
     dn: BOOLEAN;
BEGIN
  p:=0; s:=''; dn:=TRUE;
  pup.diabox(245,12,70,s,prompt);
  IF (s='') THEN  RETURN FALSE END;
  str.iscan(x,s,p,dn);
  IF x<1 THEN x:=1 END;
  IF dn THEN RETURN TRUE END;
  RETURN FALSE;
END rd_numb;

PROCEDURE ext(s: ARRAY OF CHAR): BOOLEAN;
  VAR h: INTEGER;
     dn: BOOLEAN;
BEGIN
  h:=str.len(s)-4;
  IF h<0 THEN RETURN FALSE END;
  str.scan(s,h,'.plf',dn);
  RETURN dn
END ext;

PROCEDURE rd_name(VAR n: ARRAY OF CHAR): BOOLEAN;
  VAR s: ARRAY [0..31] OF CHAR;
      d: BOOLEAN;
      p: INTEGER;
BEGIN
  s:='';
  pup.diabox(245,12,150,s,'Name font  <*.plf>:');
  IF s#'' THEN
    IF NOT ext(s) THEN str.print(n,'%s.plf',s) ELSE str.print(n,'%s',s) END;
    RETURN TRUE
  END;
  RETURN FALSE
END rd_name;

PROCEDURE show_name;
BEGIN
  MAIN.color:= pup.normal;
  gph.rect(gph.desktop,MAIN,4,4,476,15);
  MAIN.color:= BITSET(0);
  gph.print(gph.desktop,MAIN,6,4,pup.font,' Font: %s w=%d h=%d',fname,edit.w,edit.h)
END show_name;

PROCEDURE create_font;
  VAR ch: CHAR;
BEGIN
  IF NOT rd_numb(' W grid:',edit.w) THEN RETURN END;
  IF NOT rd_numb(' H grid:',edit.h) THEN RETURN END;
  IF NOT rd_name(name) THEN  RETURN END;
  fname:=name;
  IF (edit.w > 72) OR (edit.h>48) THEN km:=4 ELSE km:=6 END;
  FOR ch:=0c TO 377c DO NEW(edit.patt[ch]) END;
  show_name
END create_font;

PROCEDURE font?(VAR file,font: ARRAY OF CHAR; X,Y: INTEGER): BOOLEAN;
  VAR dbx: pup.DIREX;
BEGIN
  pup.dnew(dbx,X,Y-120,100,120,{},'.','^*.*|*.plf',pup.standard);
  pup.dselect(dbx);
  IF pup.dselected(dbx) THEN
    pup.dfullname(dbx,file);
    pup.dfilename(dbx,font)
  END;
  pup.ddispose(dbx);
  RETURN ext(font)
END font?;

PROCEDURE e;
BEGIN
  IF NOT bio.done THEN
    pup.perror (bio.error,240,180,'File "%s": %%s\n',name)
  END
END e;

PROCEDURE load_font;
  VAR nm,font: ARRAY [0..31] OF CHAR;
      ch: CHAR;
       f: bio.FILE;
      fsize,pos,i,j: INTEGER;
BEGIN
  IF font?(nm,font,286,198) THEN
    FOR ch:=0c TO 377c DO NEW(edit.patt[ch]) END
  ELSE RETURN END;
  bio.open(f,nm,'r');
  IF NOT bio.done THEN
    pup.message(240,180,'Files are not found: %s \n',nm); RETURN
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
END load_font;

PROCEDURE write_font;
  VAR fsize,pos,i: INTEGER;
                f: bio.FILE;
               ch: CHAR;
BEGIN

  FOR ch:=0c TO 377c DO edit.propW[ch]:= edit.w END;

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
END write_font;

VAR sou: INTEGER;

PROCEDURE select(VAR ch: CHAR): BOOLEAN;
  VAR i,j: INTEGER;
      x,y: INTEGER;

  PROCEDURE cursor;
  BEGIN
   MAIN.mode:= gph.xor;
   MAIN.color:= pup.bright;
   gph.rect(gph.desktop,MAIN,x+2,y-1,x+15,y-14);
   MAIN.color:= BITSET(0);
   MAIN.mode:= gph.rep;
  END cursor;

  PROCEDURE ShowStat(c: CHAR);
  BEGIN
    MAIN.mode:= gph.rep;
    MAIN.color:= BITSET(0);
    MAIN.back:= BITSET(7);
    gph.print(gph.desktop,MAIN,20,320,pup.font,'char=%$3bc [%c]  ',c,c)
  END ShowStat;

  PROCEDURE copy(d,s: INTEGER);
  BEGIN
    WITH edit DO
      RESIZE(patt[CHAR(d)],HIGH(patt[CHAR(s)])+1);
      (*$X+*) patt[CHAR(d)]:=patt[CHAR(s)]; (*$X-*)
    END;
  END copy;

  VAR ssou,scop: ARRAY [0..23] OF CHAR;
            key: CHAR;
              d: BOOLEAN;
BEGIN
  clear_mou;
  i:=ORD(ch); key:=0c;
  LOOP
    x:=X0+(i MOD 16)*17;
    y:=Y0-1-(16+1)*(i DIV 16 + 1);
    cursor;
    ShowStat(ch);
    read(key);
    cursor;
    CASE key OF
       kb.right: i:=(i+01+256) MOD 256;
      |kb.left : i:=(i-01+256) MOD 256;
      |kb.up   : i:=(i-16+256) MOD 256;
      |kb.dw   : i:=(i+16+256) MOD 256;
      |kb.home : i:=(i-01+256) MOD 256; i:=(i-16+256) MOD 256;
      |kb.end  : i:=(i+16+256) MOD 256; i:=(i-01+256) MOD 256;
      |kb.pgup : i:=(i+01+256) MOD 256; i:=(i-16+256) MOD 256;
      |kb.pgdw : i:=(i+01+256) MOD 256; i:=(i+16+256) MOD 256;
      |15c,1c  : ch:=CHAR(i); RETURN TRUE
      |'w','W' : pup.button(BC[0],TRUE);
                 IF query('Write?') THEN write_font END;
                 pup.button(BC[0],FALSE)
      |'e','E' : pup.button(BC[1],TRUE);
                 IF query('Exit?') THEN write_font; HALT END;
                 pup.button(BC[1],FALSE);
      |'q','Q' : pup.button(BC[2],TRUE);
                 IF query('Quit?') THEN tim.delay(500,tim.milisec); HALT
                 END;
                 pup.button(BC[2],FALSE)
      |'z':pup.zoom
      |'s','S' : sou:=i;
                 pup.button(BC[5],TRUE);
                 str.print(ssou,'Source= %$3bc [%c] ',CHAR(i),CHAR(i));
                 gph.write(gph.desktop,MAIN,BC[5].x+24,BC[5].y+3,pup.font,ssou,0,17);
                 tim.delay(100,tim.milisec);
                 pup.button(BC[5],FALSE);
      |'D','d' : str.print(scop,'Copy sourse to %$3bc [%c]',CHAR(i),CHAR(i));
                 pup.button(BC[6],TRUE);
                 IF query(scop) THEN copy(i,sou) END;
                 tim.delay(100,tim.milisec); pup.button(BC[6],FALSE)
      |'c','C' : pup.button(BC[8],TRUE);
                 IF query('Crate new font?') THEN create_font END;
                 pup.button(BC[8],FALSE)
      |'l','L' : pup.button(BC[3],TRUE); load_font;
                 pup.button(BC[3],FALSE);
      |'n','N' : pup.button(BC[4],TRUE);
                 IF (name='') THEN
                   pup.message(240,12,'NOT CREATE  OR LOAD FONT!!!')
                 ELSE d:=rd_name(fname); show_name END;
                 pup.button(BC[4],FALSE)
      |'o','O' : pup.button(BC[7],TRUE);
                 FOR j:=0 TO 377b DO
                   cursor; optimize(CHAR(i)); cursor;
                   i:=(i+1) MOD 400b;
                   x:=X0+(i MOD 16)*17;
                   y:=Y0-1-(16+1)*(i DIV 16 + 1);
                 END;
                 pup.button(BC[7],FALSE)
    ELSE END;
    ch:=CHAR(i);
  END;
  RETURN FALSE;
END select;

PROCEDURE edit_char(ch: CHAR);
VAR wrk: Char; X0,Y0,k: INTEGER;

PROCEDURE clear;
BEGIN
  MAIN.mode:= gph.bic;
  gph.rect(gph.desktop,MAIN,X0+km,Y0+km,X0+edit.w*km,Y0+edit.h*km)
END clear;

PROCEDURE ShowCoord(x,y: INTEGER);
  VAR s1,s2: ARRAY [0..5] OF CHAR;
      old_mod: INTEGER;
BEGIN
  MAIN.mode:= gph.rep;
  MAIN.color:= BITSET(0);
  str.print(s1,'x=%$3d [%d] ',x,x);
  str.print(s2,'y=%$3d [%d] ',y,y);
  gph.write(gph.desktop,MAIN,X0,   Y0-24,pup.font,s1,0,HIGH(s1));
  gph.write(gph.desktop,MAIN,X0+35,Y0-24,pup.font,s2,0,HIGH(s2));
  MAIN.color:= BITSET(7);
  MAIN.mode:= gph.xor
END ShowCoord;

VAR window: pup.POPUP;

PROCEDURE grid;
  VAR  b1,b2: gph.BLOCK;
     bg1,bg2: gph.TOOL;
         i,j: INTEGER;
           s: ARRAY [0..13] OF CHAR;
BEGIN
  IF edit.w < 12 THEN
    pup.pnew(window,X0-km-16,Y0-km-39,120,(edit.h+1)*km+73);
    WITH b1 DO
      x:=X0-km-14; y:=Y0-km-34;
      w:=106; h:=(edit.h+1)*km+61
    END
  ELSE
    pup.pnew(window,X0-km-16,Y0-km-39,edit.w*km+38,(edit.h+1)*km+73);
    WITH b1 DO
      x:=X0-km-14; y:=Y0-km-34;
      w:=edit.w*km+36; h:=(edit.h+1)*km+61
    END
  END;
  pup.pclose(window);
  pup.popen(window);
  MAIN.mode:= gph.rep;
  WITH b2 DO x:=b1.x+4; y:=b1.y+4; w:=b1.w-8; h:=b1.h-8 END;
  pup.panel(b1,b2,TRUE,FALSE);
  WITH b2 DO
    x:=X0-km-2; y:=Y0-km-2;
    w:=(edit.w+1)*km+5;
    h:=(edit.h+1)*km+4
  END;
  WITH b1 DO x:=b2.x-4; y:=b2.y-4; w:=b2.w+8; h:=b2.h+8 END;
  pup.panel(b1,b2,TRUE,FALSE);
  MAIN.color:= pup.black;
  MAIN.mode:= gph.rep;
  gph.rect(gph.desktop,MAIN,b2.x-1,b2.y,b2.x+b2.w,b2.y+b2.h);
  str.print(s,'char=%$3oc [%c] ',ch,ch);
  gph.write(gph.desktop,MAIN,X0-5,Y0+(edit.h+1)*km+2,pup.font,s,0,HIGH(s));
  MAIN.color:= BITSET(1);
  bg1:=MAIN;
  bg2:=MAIN;
  WITH bg1.clip DO x:=X0; y:=Y0;    w:=edit.w*km; h:=edit.h*km END;
  WITH bg2.clip DO x:=X0; y:=Y0+km; w:=edit.w*km; h:=1 END;
--  FOR i:=0 TO edit.w DO gph.dot(gph.desktop,bg1,X0+i*km,Y0) END;
  gph.grid(gph.desktop,MAIN,bg1.clip,km,km);
--  FOR j:=1 TO edit.h DO
 --   gph.bblt(gph.desktop,bg2,gph.desktop,bg1); bg2.clip.y:= Y0+j*km
--  END;
  gph.frame(gph.desktop,MAIN,b2.x-1,b2.y,b2.x+b2.w,b2.y+b2.h);
  FOR i:=0 TO edit.w-1 BY 5 DO
    gph.line(gph.desktop,MAIN,X0 + i*km,Y0-5,X0 + i*km,Y0 + edit.h*km-1)
  END;
  FOR j:=0 TO edit.h-1 BY 5 DO
    gph.line(gph.desktop,MAIN,X0-5,Y0 + j*km,X0+ edit.w*km-1,Y0+j*km)
  END
END grid;

VAR X,Y,x,y: INTEGER;
    pen    : BOOLEAN;

PROCEDURE in(x,y,X,Y,xa,ya: INTEGER): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  IF (x=X) THEN
    RETURN (xa=X) & ((y<=ya) & (ya<=Y) OR (Y<=ya) & (ya<=y))
  ELSIF (y=Y) THEN
    RETURN (ya=Y) & ((x<=xa) & (xa<=X) OR (X<=xa) & (xa<=x))
  ELSE
    x:=x-X; y:=y-Y; xa:=xa-X; ya:=ya-Y;
    RETURN (xa*y=ya*x)&(ABS(xa)<=ABS(x))&(ABS(ya)<=ABS(y));
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

PROCEDURE line;
BEGIN gph.line(gph.desktop,MAIN,X0+x*km, Y0+y*km, X0+X*km, Y0+Y*km) END line;

PROCEDURE make;
  VAR i: INTEGER;
BEGIN
  MAIN.mode:= gph.rep;
  MAIN.color:= BITSET(7);
  FOR i:=0 TO HIGH(wrk) DO
    un_pack(wrk[i],x,y,X,Y); line
  END;
  MAIN.mode:= gph.xor
END make;

PROCEDURE cursor;
  VAR l,c: INTEGER;
BEGIN
  IF pen THEN line ELSE X:=x; Y:=y END;
  l:=Y0+y*km; c:=X0+x*km;
  gph.line(gph.desktop,MAIN,c+2,l+2,c+2,l-2);
  gph.line(gph.desktop,MAIN,c-2,l+2,c-2,l-2);
  gph.line(gph.desktop,MAIN,c+2,l-2,c-2,l-2);
  gph.line(gph.desktop,MAIN,c+2,l+2,c-2,l+2)
END cursor;

PROCEDURE sh;
  VAR i: INTEGER; x,y,X,Y: INTEGER;
BEGIN i:=0;
  WHILE i<=HIGH(wrk) DO un_pack(wrk[i],x,y,X,Y); INC(i) END
END sh;

PROCEDURE up_prop;
 VAR m: INTEGER;
BEGIN
  --IF edit.propW[ch]< w THEN INC(propW[ch]) END;
END up_prop;

PROCEDURE dw_prop;
BEGIN

END dw_prop;


VAR key: CHAR;
    d  : BITSET;

BEGIN
  NEW(wrk,HIGH(edit.patt[ch])+1);
  (*$X+*) wrk:=edit.patt[ch]; (*$X-*)
  X0:=(480-(edit.w*km)) DIV 2;
  Y0:=(360-(edit.h*km)) DIV 2;
  grid; make;
  pup.popen(window);
  x:=0; y:=0; X:=0; Y:=0; pen:=FALSE;
  LOOP
    ShowCoord(x,y);
    cursor;
    read(key);
    cursor;
    CASE key OF
    |kb.left    : x:=(x-1+edit.w) MOD edit.w
    |kb.right   : x:=(x+1+edit.w) MOD edit.w
    |kb.up      : y:=(y+1+edit.h) MOD edit.h
    |kb.dw      : y:=(y-1+edit.h) MOD edit.h
    |kb.end     : x:=(x-1+edit.w) MOD edit.w; y:=(y-1+edit.h) MOD edit.h
    |kb.home    : y:=(y+1+edit.h) MOD edit.h; x:=(x-1+edit.w) MOD edit.w
    |kb.pgup    : x:=(x+1+edit.w) MOD edit.w; y:=(y+1+edit.h) MOD edit.h
    |kb.pgdw    : x:=(x+1+edit.w) MOD edit.w; y:=(y-1+edit.h) MOD edit.h
    |3c,kb.del  : pen:=FALSE;
    |2c,kb.ins  : pen:=TRUE;
    |'z':pup.zoom
    |kb.cr      : IF pen THEN pen:=FALSE ELSE pen:=TRUE END;
    |1c,' '     : IF pen THEN line; put(x,y,X,Y); END;
                 X:=x; Y:=y
(*
    |'o'        : pup.pclose(window); pup.pdispose(window);
                  (*$U+*) edit.patt[ch]^:=wrk^; optimize(ch);
                  wrk^:=edit.patt[ch]^; (*$U-*)
                  grid; make
*)
    |'r','R'    : make
    |'+'        : up_prop;
    |'-'        : dw_prop;
    |'w','W'    : RESIZE(edit.patt[ch],HIGH(wrk)+1);
                  (*$X+*) edit.patt[ch]:=wrk; (*$X-*)
    |'e','E'    : EXIT;
    |'q','Q',33c: pup.pclose(window); pup.pdispose(window);
                  DISPOSE(wrk); RETURN
    ELSE END
  END;
  RESIZE(edit.patt[ch],HIGH(wrk)+1);
  (*$X+*) edit.patt[ch]:=wrk; (*$X-*)
  pup.pclose(window); pup.pdispose(window);
  DISPOSE(wrk)
END edit_char;

PROCEDURE sh(w: Char);
  VAR i: INTEGER; x,y,X,Y: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(w) DO un_pack(w[i],x,y,X,Y) END
END sh;

PROCEDURE Edit;
  VAR ch: CHAR;
BEGIN
  X0:=24; Y0:=328;
  ch:=100c;
  showfont;
  LOOP
    IF NOT select(ch) THEN EXIT END;
    edit_char(ch); optimize(ch);
    ch:=CHAR((ORD(ch)) MOD 256);
  END;
END Edit;

VAR ch: CHAR;
     i: INTEGER;

BEGIN
  WITH MAIN DO
    zX:=0; zY:=0;
    mode := gph.rep;
    mask := {0..3};
    color:= pup.black;
    back := pup.normal;
    WITH clip DO  x:=0; y:=0; w:=479; h:=359 END
  END;
  X:=0; Y:=0;
  edit.w:=1; edit.h:=1; km:=6;
  fnt.unpack(pup.font);
  FOR ch:=0c TO 377c DO NEW(edit.patt[ch]); edit.propW[ch]:=0; END;
  name:=''; fname:='';
  clear_mou;
  Edit
END pfe.
