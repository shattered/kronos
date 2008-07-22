IMPLEMENTATION MODULE videoGames; (* Leo 23-Aug-88. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  lab: LabtamVDU;
IMPORT  tty: Terminal;
IMPORT  mcd: defCodes;
IMPORT  env: tskEnv;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT heap: Heap;

VAR lay: INTEGER;

TYPE ADDRESS = sys.ADDRESS;
        WORD = sys.WORD;

         BMD = RECORD
                  w,h: INTEGER;
                  wpl: INTEGER;
                 base: ADDRESS;
                  pat: BITSET;
               END;

       FONT = POINTER TO
              RECORD
                w,h : INTEGER;
                base: sys.ADDRESS;
              END;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE mcd.move END move;

PROCEDURE bitmove(mode: INTEGER;
                    to: ADDRESS;   to_ofs: INTEGER;
                  from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bitmove;

PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
CODE 0F9h 00h END in_rect;

PROCEDURE in_range0(i,hi: INTEGER): BOOLEAN;
CODE 0F6h END in_range0;

PROCEDURE check(i,lo,hi: INTEGER);
CODE 0C6h 0B1h END check;

PROCEDURE check0(i,hi: INTEGER);
CODE 0C7h 0B1h END check0;

PROCEDURE bbp(to: ADDRESS; ofs: INTEGER; size: INTEGER; val: WORD);
CODE mcd.bbp END bbp;

PROCEDURE bbu(to: ADDRESS; ofs: INTEGER; size: INTEGER): BITSET;
CODE mcd.bbu END bbu;

PROCEDURE dch(m: INTEGER; VAR bmd: BMD; x,y: INTEGER; f: FONT; ch: CHAR);
CODE  0F9h 003h END dch;

PROCEDURE ddt(m: INTEGER; VAR bmd: BMD; x,y: INTEGER);
CODE 1 0F9h 001h END ddt;

PROCEDURE dvl(m: INTEGER; VAR bmd: BMD; x,y,len: INTEGER);
CODE 0F9h 001h END dvl;

PROCEDURE bit_move(mode: INTEGER;
                     to: ADDRESS;   to_ofs: INTEGER;
                   from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
BEGIN bitmove(mode,to,to_ofs,from,from_ofs,nobits) END bit_move;

VAR bump: ARRAY [0..63] OF BITSET; (* 2048 pixels *)
    lnFF: ARRAY [0..63] OF BITSET; (* 2048 111111 *)
    ln00: ARRAY [0..63] OF BITSET; (* 2048 000000 *)
    font: FONT;
     bmd: BMD;

PROCEDURE build_bmd(VAR bmd: BMD; w,h: INTEGER; adr: ADDRESS);
BEGIN
  ASSERT((h>0) & (w>0),4Ah);
  bmd.w:=w; bmd.h:=h; bmd.wpl:=(w+31) DIV 32; bmd.base:=adr
END build_bmd;

PROCEDURE bmd_size(w,h: INTEGER): INTEGER; (* no of words needed for WxH *)
BEGIN RETURN (w+31) DIV 32 * h END bmd_size;

PROCEDURE erase(SEQ pat: WORD);
  VAR N,n: INTEGER;
      ptr: ADDRESS;
BEGIN
  IF bmd.h<=0 THEN RETURN END;
  ptr:=bmd.base+bmd.wpl*bmd.h;
  n:=0; N:=HIGH(pat)+1;
  REPEAT
    ptr:=ptr-bmd.wpl;
    IF N=0 THEN ptr^:={} ELSE ptr^:=pat[n]; n:=(n+1) MOD N END;
    move(ptr+1,ptr,bmd.wpl-1);
  UNTIL ptr=bmd.base;
  lab.block(lay,0,0,bmd.w,bmd.h);
END erase;

PROCEDURE bits(mode,x,y: INTEGER; adr: ADDRESS; n: INTEGER);
BEGIN
  y:=bmd.h-1-y;
  bitmove(mode,bmd.base+y*bmd.wpl,x,adr,0,n);
END bits;

PROCEDURE refresh(x,y,w,h: INTEGER);
BEGIN
  y:=bmd.h-1-y;
  lab.block(lay,x,y-h,w,h);
END refresh;

PROCEDURE dot(x,y: INTEGER);
BEGIN
  y:=bmd.h-1-y;
  ddt(mode MOD 4,bmd,x,y); lab.block(lay,x,y,1,1);
END dot;

PROCEDURE sdt(x,y: INTEGER);
BEGIN
  ddt(mode MOD 4,bmd,x,y); lab.block(lay,x,y,1,1);
END sdt;

PROCEDURE vline(x,y,y1: INTEGER);
  VAR i: INTEGER;
BEGIN
  y:=bmd.h-1-y; y1:=bmd.h-1-y1;
  mode:=mode MOD 4;
  IF x < 0     THEN RETURN END;
  IF x >=bmd.w THEN RETURN END;
  IF y > y1    THEN i:=y1; y1:=y; y:=i END;  (* swap: y<y1 *)
  IF y < 0     THEN y :=0       END; (* clip *)
  IF y1>=bmd.h THEN y1:=bmd.h-1 END; (* clip *)
  IF y = y1    THEN ddt(mode MOD 4,bmd,x,y); RETURN END;
  i:=y1-y+1;
  IF i<=0 THEN RETURN END; (* out of screen *)
  dvl(mode,bmd,x,y,i);
  lab.block(lay,x,y,1,i);
END vline;

PROCEDURE hline(x,y,x1: INTEGER);
  VAR d: INTEGER;
    ptr: ADDRESS;
  w,pat: BITSET;
    ofs: INTEGER;
    len: INTEGER;
BEGIN
  y:=bmd.h-1-y;
  mode:=mode MOD 4;
  IF y < 0     THEN RETURN END;
  IF y >=bmd.h THEN RETURN END;
  IF x > x1    THEN d:=x1; x1:=x; x:=d END;  (* swap: x<x1 *)
  IF x < 0     THEN x :=0       END; (* clip *)
  IF x1>=bmd.w THEN x1:=bmd.w-1 END; (* clip *)
  IF x = x1    THEN ddt(mode MOD 4,bmd,x,y); RETURN END;
  d:=x1-x+1;
  IF d<=0 THEN RETURN END; (* out of screen *)
  bitmove(mode,bmd.base+y*bmd.wpl,x,sys.ADR(lnFF),x,d);
  lab.block(lay,x,y,d,1);
END hline;

PROCEDURE line (x,y,x1,y1: INTEGER);

  PROCEDURE draw(x,y,x1,y1: INTEGER);
    VAR i,j,k: INTEGER;
        dx,dy: INTEGER;
        p,q,d: INTEGER;
  BEGIN
    IF x<=x1 THEN i:=1 ELSE i:=-1 END;
    IF y<=y1 THEN j:=1 ELSE j:=-1 END;
    dx:=ABS(x1-x); dy:=ABS(y1-y);
    IF    dx>dy THEN p:=2*dy; q:=2*(dy-dx); d:=dy-dx; k:=dx;
      REPEAT sdt(x,y);
        IF d <= dy THEN d:=d+p ELSE d:=d+q; y:=y+j END;
        x:=x+i; k:=k-1
      UNTIL k<0;
    ELSIF dx<dy THEN p:=2*dx; q:=2*(dx-dy); d:=dx-dy; k:=dy;
      REPEAT sdt(x,y);
        IF d <= dx THEN d:=d+p ELSE d:=d+q; x:=x+i END;
        y:=y+j; k:=k-1
      UNTIL k<0;
    ELSE (* dx=dy *) k:=dx;
      REPEAT sdt(x,y); y:=y+j; x:=x+i; k:=k-1 UNTIL k<0;
    END;
  END draw;

  PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
  CODE 0F9h 00 END in_rect;

  PROCEDURE clip(x0,y0,x1,y1,w,h: INTEGER);
    VAR x2,y2,x,y,xC,yC: INTEGER;
  BEGIN
    IF in_rect(x1,y1,w,h) THEN
      x2:=x1; y2:=y1;
    ELSE
      x2:=x0; y2:=y0;
    END;
    IF NOT in_rect(x2,y2,w,h) THEN
      LOOP
        x2:=(x2+x1) DIV 2; y2:=(y2+y1) DIV 2;
        IF in_rect(x2,y2,w,h) THEN EXIT END;
        (* check out of clipping area: *)
        IF (x0=x2) & (y0=y2) OR (x1=x2) & (y1=y2) THEN RETURN END;
        IF (x2<0)=(x0<0) OR (y2<0)=(y0<0) THEN   (* !!!!!! ERROR *)
          x0:=x2; y0:=y2
        ELSE
          x1:=x2; y1:=y2
        END;
      END;
    END;
    (* x2,y2 in rectangle! *)
    IF NOT in_rect(x0,y0,w,h) THEN
      x:=x2; y:=y2;
      LOOP
        xC:=(x0+x) DIV 2; yC:=(y0+y) DIV 2;
        IF  (xC=x0) & (yC=y0) OR (xC=x) & (yC=y) THEN EXIT END;
        IF in_rect(xC,yC,w,h) THEN x:=xC; y:=yC ELSE x0:=xC; y0:=yC END;
      END;
      x0:=x; y0:=y;
    END;
    IF NOT in_rect(x1,y1,w,h) THEN
      x:=x2; y:=y2;
      LOOP
        xC:=(x1+x) DIV 2; yC:=(y1+y) DIV 2;
        IF  (xC=x1) & (yC=y1) OR (xC=x) & (yC=y) THEN EXIT END;
        IF in_rect(xC,yC,w,h) THEN x:=xC; y:=yC ELSE x1:=xC; y1:=yC END;
      END;
      x1:=x; y1:=y;
    END;
    draw(x0,y0,x1,y1);
  END clip;

  VAR i: INTEGER;
    w,h: INTEGER;
BEGIN
  h:=bmd.h;     w :=bmd.w;
  mode:=mode MOD 4;
  IF x=x1 THEN vline(x,y,y1); RETURN END;
  IF y=y1 THEN hline(x,y,x1); RETURN END;
  y:=h-1-y;     y1:=h-1-y1;
  IF x1<x THEN i:=x1; x1:=x; x:=i;  i:=y1; y1:=y; y:=i END;
  IF in_rect(x,y,w,h) & in_rect(x1,y1,w,h) THEN
    draw(x,y,x1,y1);
  ELSE
    clip(x,y,x1,y1,w,h);
  END;
END line;

PROCEDURE rect (x,y,x1,y1: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF y>y1 THEN i:=y; y:=y1; y1:=i END;
  IF y<0       THEN y :=0       END;
  IF y1>=bmd.h THEN y1:=bmd.h-1 END;
  i:=y1-y+1;
  WHILE i>0 DO hline(x,y,x1); INC(y); DEC(i) END;
END rect;

PROCEDURE frame(x,y,x1,y1: INTEGER);
BEGIN
  hline(x,y ,x1);
  hline(x,y1,x1);
  IF y<y1 THEN
    vline(x ,y+1,y1-1);
    vline(x1,y+1,y1-1);
  ELSE
    vline(x ,y1+1,y-1);
    vline(x1,y1+1,y-1);
  END;
END frame;

PROCEDURE circ(X,Y: INTEGER; r: INTEGER);
  VAR x,y,q,s: INTEGER;
BEGIN
  IF (X+r<0) OR (Y+r<0) OR (X-r>=bmd.w) OR (Y-r>=bmd.h) THEN RETURN END;
  Y:=bmd.h-1-Y;
  mode:=mode MOD 4;
  IF r<1 THEN sdt(X,Y); RETURN END;
  IF r=1 THEN
    sdt(X-1,Y); sdt(X+1,Y);
    sdt(X,Y+1); sdt(X,Y-1); RETURN
  END;
  IF ODD(r) THEN INC(r) END;
  x:=r; y:=0; q:=r;
  IF mode=xor THEN
    sdt(X+r,Y); sdt(X-r,Y);
  END;
  REPEAT
    sdt(X+x,Y+y);
    sdt(X+x,Y-y);
    sdt(X-x,Y+y);
    sdt(X-x,Y-y);
    x:=x-1; s:=q; q:=q-(x*2+1);
    IF q<1 THEN
      q:=s; x:=x+1; y:=y+1; q:=q+y*8-4;
    END;
  UNTIL x<0;
  IF mode=xor THEN
    sdt(X,Y+y); sdt(X,Y-y);
  END;
END circ;

PROCEDURE circf(X,Y: INTEGER; r: INTEGER);
  VAR x,y,Y0,q,s: INTEGER;
BEGIN
  IF (X+r<0) OR (Y+r<0) OR (X-r>=bmd.w) OR (Y-r>=bmd.h) THEN RETURN END;
  Y0:=Y; Y:=bmd.h-1-Y;
  mode:=mode MOD 4;
  IF r<1 THEN sdt(X,Y); RETURN END;
  IF r=1 THEN
    sdt(X-1,Y); sdt(X+1,Y);
    sdt(X,Y+1); sdt(X,Y-1); RETURN
  END;
  IF ODD(r) THEN INC(r) END;
  x:=r; y:=0; q:=r;
  IF mode=xor THEN
    hline(X+r,Y0,X-r);
  END;
  REPEAT
    hline(X+x,Y0+y,X-x);
    hline(X+x,Y0-y,X-x);
    x:=x-1; s:=q; q:=q-(x*2+1);
    IF q<1 THEN
      q:=s; x:=x+1; y:=y+1; q:=q+y*8-4;
    END;
  UNTIL x<0;
  IF mode=xor THEN
    sdt(X,Y+y); sdt(X,Y-y);
  END;
END circf;


--------------------------  Sprites  ---------------------------
                          -----------

PROCEDURE build_sprite(VAR sp: sprite; VAL str: ARRAY OF CHAR);
  VAR i,h,w,max: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(sp.body)) DO sp.body[i]:={}; INC(i) END;
  i:=0; h:=0; w:=0; max:=0;
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO
    IF (str[i]="|") OR (w=32) THEN
      IF max<w THEN max:=w END;
      IF (i<=HIGH(str)) & (str[i]#0c) THEN INC(h); w:=0 END;
    ELSIF str[i]#' ' THEN
      INCL(sp.body[h],w); INC(w)
    ELSE
      INC(w);
    END;
    INC(i);
  END;
  sp.base:=sys.ADR(sp.body); sp.w:=max; sp.h:=h;
END build_sprite;

PROCEDURE show_sprite(x,y: INTEGER; VAR  sp: sprite);
  VAR w,h: INTEGER;
BEGIN y:=bmd.h-(y+sp.h);
  w:=sp.w; h:=sp.h; sp.base:=sys.ADR(sp.body);
  IF NOT in_rect(x,y,bmd.w-w,bmd.h-h) THEN RETURN END;
  dch(mode + inv,bmd,x,y,sys.ADR(sp),0c);
  lab.block(lay,x,y,w,h);
END show_sprite;

PROCEDURE put_sprite(x,y: INTEGER; VAR  sp: sprite);
  VAR w,h: INTEGER;
BEGIN y:=bmd.h-(y+sp.h);
  w:=sp.w; h:=sp.h; sp.base:=sys.ADR(sp.body);
  IF NOT in_rect(x,y,bmd.w-w,bmd.h-h) THEN RETURN END;
  dch(mode + inv,bmd,x,y,sys.ADR(sp),0c);
  lab.block(lay,x,y,w,h);
END put_sprite;

PROCEDURE refresh_around(x,y: INTEGER; VAR sp: sprite);
BEGIN
  y:=bmd.h-(y+sp.h);
  lab.block(lay,x-2,y-2,sp.w+2,sp.h+2);
END refresh_around;

----------------------------  Text  ----------------------------
                            --------

PROCEDURE write(x,y: INTEGER; ch: CHAR);
BEGIN y:=bmd.h-(y+char_h);
  IF NOT in_rect(x,y,bmd.w-char_w,bmd.h-char_h) THEN RETURN END;
  inv :=INTEGER(BITSET(inv)*{2});
  mode:=mode MOD 4;
  dch(mode + inv,bmd,x,y,font,ch);
  lab.block(lay,x,y,char_w,char_h);
END write;

PROCEDURE write_str(x,y: INTEGER; VAL str: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO
    write(x,y,str[i]);
    x:=x+char_w; i:=i+1;
  END;
END write_str;

PROCEDURE print(x,y: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  str.print(s,fmt,args); write_str(x,y,s);
END print;

PROCEDURE load_font(VAL font_name: ARRAY OF CHAR): BOOLEAN;
  VAR f: bio.FILE;
      p: bio.PATHs;
     io: BOOLEAN;
   size: INTEGER;
BEGIN
  bio.get_paths(p,env.etc);
  IF NOT bio.done THEN p:=bio.here END;
  bio.lookup(p,f,font_name,'r');
  IF NOT bio.done THEN RETURN TRUE END;
  size:=(bio.eof(f)+3) DIV 4;
  heap.ALLOCATE(font,size);
  IF font=NIL THEN bio.close(f); RETURN TRUE END;
  bio.read(f,font,size*4);
  bio.close(f);
  font^.base:=ADDRESS(font)+SIZE(font^);
  RETURN FALSE
END load_font;

PROCEDURE layer(n: INTEGER);
BEGIN
  ASSERT(n IN {0,1});
  lay:=n;
  build_bmd(bmd,800,300,lab.layer[lay]);
END layer;

PROCEDURE bmd_adr(): ADDRESS;
BEGIN RETURN bmd.base END bmd_adr;

PROCEDURE bell;
BEGIN tty.print(""7c) END bell;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  lab.auto(FALSE);
  build_bmd(bmd,800,300,lab.layer[lay]);
  mode:=rep; inv:=off;
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={     } END;
  FOR i:=0 TO HIGH(lnFF) DO lnFF[i]:={0..31} END;
  IF load_font("FONT10.fnt") THEN ASSERT(FALSE,4Bh) END;
--  IF load_font("default.bmf") THEN ASSERT(FALSE,4Bh) END;
  char_w:=font^.w;
  char_h:=font^.h;
END init;

BEGIN
  lay:=0;
  init
END videoGames.
