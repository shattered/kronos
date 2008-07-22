IMPLEMENTATION MODULE KTV480; (* Leo 12-Mar-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;

TYPE
  WORD      = SYSTEM.WORD;
  ADDRESS   = SYSTEM.ADDRESS;

----------------------------------------------------------------

CONST
   H = 360;   W = 480;  L=1;  G=INTEGER(1<<L);
  fH =  14;  fW =   6;
  bH = 512;  bW = 512;  WPL = 16;  bL=4;  lSIZE=WPL*bH;

  SHIFT   = ADDRESS( 1F0000h );
  PLT     = ADDRESS( 1F0010h );
  PAUSE   = ADDRESS( 1F0020h );

  BASE    = ADDRESS( 1F8000h );


----------------------------------------------------------------

TYPE

  FONT      = POINTER TO FONT_DESC;
  FONT_DESC = RECORD
                w,h : INTEGER;
                base: ADDRESS;
                xxxx: ADDRESS;
              END;

   BMDPTR = POINTER TO BMD;
   BMD    = RECORD
              w,h : INTEGER;
              wpl : INTEGER;
              base: ADDRESS;
              patt: BITSET;
            END;

CONST rep=0; or=1; xor=2; bic=3; inv=rep+4;


VAR font: FONT;
  carret: FONT;
   white: FONT;
   clip0: FONT;
   clip1: FONT;

VAR

    plt: ADDRESS;
  pause: POINTER TO BITSET;
pauseok: BOOLEAN;
  shift: ADDRESS;

    crs: BMDPTR;
    bmd: BMDPTR;
   bbmd: BMD;
   bcrs: BMD;

 filler: ARRAY [0..1] OF ARRAY [0..WPL-1] OF INTEGER;

     px: ARRAY [0..W DIV fW-1] OF INTEGER;
     py: ARRAY [0..H DIV fH-1] OF INTEGER;


---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------


PROCEDURE di(): BITSET; CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;

PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE dch(mode: WORD; bmd: ADDRESS; x,y: INTEGER; font: ADDRESS; ch: WORD);
CODE 0F9h 003h END dch;

PROCEDURE bblt(to: ADDRESS;   to_ofs: INTEGER;
             from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE cod.bblt END bblt;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE cod.move END move;

PROCEDURE croll(dest,sou: ADDRESS; size: INTEGER);
CODE 96h END croll;

PROCEDURE zero_arr(VAR a: ARRAY OF WORD);
BEGIN a[0]:=0; move(SYSTEM.ADR(a)+1,SYSTEM.ADR(a),HIGH(a)) END zero_arr;

PROCEDURE fill_arr(VAR a: ARRAY OF WORD; p: WORD);
BEGIN a[0]:=p; move(SYSTEM.ADR(a)+1,SYSTEM.ADR(a),HIGH(a)) END fill_arr;

PROCEDURE zero(a: ADDRESS; size: INTEGER);
BEGIN a^:=0; move(a+1,a,size-1) END zero;

PROCEDURE fill(a: ADDRESS; size: INTEGER; p: WORD);
BEGIN a^:=p; move(a+1,a,size-1) END fill;


----------------------  INIT HARDWARE  -------------------------
                      -----------------

VAR plt0: PALETTE;
    plt1: PALETTE;

PROCEDURE palette(p: PALETTE; from,len: INTEGER);
  VAR m: BITSET; s,d,sh: INTEGER;
BEGIN
  d:=plt+from;
  s:=SYSTEM.ADR(p)+from;   sh:=(shy*200h+shx)*400h;
  IF pauseok THEN m:=di();
    REPEAT ei(m); m:=di() UNTIL pause^*{0}#{}; move(d,s,len); shift^:=sh; ei(m)
  ELSE
    move(d,s,len); shift^:=sh
  END;
  move(SYSTEM.ADR(palet)+from,s,len)
END palette;

PROCEDURE init_plt(VAR p: ARRAY OF INTEGER; zero: BOOLEAN);

  PROCEDURE p_set(color: INTEGER; r,g,b: INTEGER);
  BEGIN
    p[color]:=ptrans[r*4]+ptrans[g*4]*16+ptrans[b*4]*256
  END p_set;

  VAR i: INTEGER;

BEGIN
  FOR i:=0 TO 15 DO p_set(i,0,0,0) END;
  IF zero THEN RETURN END;
  CASE L OF
    |3:
        p_set(0,00,00,00);  p_set( 8,02,02,02);
        p_set(1,00,01,01);  p_set( 9,00,00,03);
        p_set(2,01,03,01);  p_set(10,00,01,01);
        p_set(3,03,02,00);  p_set(11,03,00,00);
        p_set(4,00,02,00);  p_set(12,03,00,03);
        p_set(5,00,00,02);  p_set(13,00,03,00);
        p_set(6,02,00,00);  p_set(14,01,03,01);
        p_set(7,02,02,02);  p_set(15,00,00,00);
   |2:
        p_set(0,00,00,00);  p_set( 8,02,02,02);
        p_set(1,00,01,00);  p_set( 9,03,03,03);
        p_set(2,00,02,00);  p_set(10,00,01,02);
        p_set(3,00,03,00);  p_set(11,01,01,01);

   |1:  p_set(0,00,00,00);  p_set( 8,02,02,02);
        p_set(1,00,02,00);  p_set( 9,00,01,00);
  ELSE
    ASSERT(FALSE)
  END;
END init_plt;

VAR
  FDESC: FONT_DESC;

 _WHITE: FONT_DESC;
_CARRET: FONT_DESC;
 _CLIP0: FONT_DESC;
 _CLIP1: FONT_DESC;

  WHITE: ARRAY [0..fH] OF INTEGER;
 CARRET: ARRAY [0..fH] OF INTEGER;

PROCEDURE init_hw(VAL FNT: ARRAY OF WORD);
  VAR i,f: INTEGER;
BEGIN
  shx:=0010h;
  shy:=01FFh; scrl:=0;

  font :=SYSTEM.ADR(FDESC);
  font^.w:=fW;  font^.h:=fH;  font^.base:=SYSTEM.ADR(FNT)+4;

  fill_arr(WHITE,-1); _WHITE:=FDESC;
  white:=SYSTEM.ADR(_WHITE);
  white^.base:=SYSTEM.ADR(WHITE);

  fill_arr(CARRET,-1); _CARRET:=FDESC;
  carret:=SYSTEM.ADR(_CARRET);
  carret^.base:=SYSTEM.ADR(CARRET);
  carret^.h:=fH;    (*3 *)
  carret^.w:=fW+1;  (*fW*)

  clip0 :=SYSTEM.ADR(_CLIP0);           clip1 :=SYSTEM.ADR(_CLIP1);
  clip0^:=font^;                        clip1^:=font^;

  shift :=SHIFT;
  plt   :=PLT;
  pause :=PAUSE;

  i:=50000;
  REPEAT DEC(i) UNTIL (pause^*{0}={}) OR (i=0);
  IF i#0 THEN i:=50000;
    REPEAT DEC(i) UNTIL (pause^*{0}#{}) OR (i=0)
  END;
  pauseok:=(i#0);

  bmd:=SYSTEM.ADR(bbmd);
  crs:=SYSTEM.ADR(bcrs);
  WITH bmd^ DO w:=W;  h:=H;  wpl:=WPL;  patt:={0..31}; base:=BASE  END;
  crs^:=bmd^;
  WITH crs^ DO base:=BASE+(bL-1)*lSIZE END;

  f:=0;
  FOR i:=0 TO HIGH(px) DO px[i]:=f; INC(f,fW) END;
  f:=0;
  FOR i:=0 TO HIGH(py) DO py[i]:=f; INC(f,fH) END;
  init_plt(plt0,TRUE);
  init_plt(plt1,FALSE);
  palette (plt0,0,16);
  fill_arr(filler[0],0);  fill_arr(filler[1],-1)
END init_hw;

-----------------------  TTY EMULATOR  -------------------------
                       ----------------

VAR BGND: INTEGER;
    FGND: INTEGER;

PROCEDURE set_lc(l,c: INTEGER);
  VAR CAR: BOOLEAN;
BEGIN
  CAR:=car;
  IF CAR THEN toggle_carret END;
  IF l>HIGH(py) THEN ln:=HIGH(py)
  ELSIF  l<0    THEN ln:=0
  ELSE               ln:=l
  END;
  IF c>HIGH(px) THEN cl:=HIGH(px)
  ELSIF  c<0    THEN cl:=0
  ELSE               cl:=c
  END;
  IF CAR THEN toggle_carret END
END set_lc;

PROCEDURE set_ground(f,b: INTEGER);
BEGIN
  IF (gmin<=f) & (f<=gmax) & (gmin<=b) & (b<=gmax) THEN
    fg:=f; FGND:=fg + G DIV 2;   bg:=b; BGND:=bg + G DIV 2
  END
END set_ground;

PROCEDURE CLEAR;
  VAR l,h: INTEGER;
      CAR: BOOLEAN;
     base: ADDRESS;
     back: INTEGER;
BEGIN
  palette(plt0,0,16);
  CAR:=car;
  IF CAR THEN toggle_carret END;
  shx:=0010h;
  shy:=01FFh; scrl:=0;
  back:=BGND;
  base:=BASE;
  h:=(H DIV fH * fH)*WPL;
  l:=0;
  REPEAT
    fill(base,h,0-(back MOD 2)); INC(base,h);
    zero(base,lSIZE-h);          INC(base,lSIZE-h);
    back:=back DIV 2;            l:=l+1
  UNTIL l=L;
  FOR l:=L TO bL-1 DO zero(base,lSIZE); INC(base,lSIZE) END;
  IF CAR THEN toggle_carret END;
  palette(plt1,0,16)
  (* shift^:=(shy*200h+shx)*400h;   done by pallete! *)
END CLEAR;

PROCEDURE RESET;
BEGIN
  awp :=TRUE;
  undl:=FALSE;
  cntl:=TRUE;
  lock:=FALSE;
  set_ground(0,gmin);
  IF car=FALSE THEN toggle_carret END
END RESET;

PROCEDURE toggle_carret;
  VAR f: ADDRESS;
    F,B: INTEGER;
    x,y: INTEGER;
    h,b: INTEGER;
BEGIN
  h:=carret^.h;
  x:= px[cl]+shx-(carret^.w-fW);
  y:=(py[ln]+scrl+fH-h) MOD bH;
  car:=NOT car;
  IF y+h<=bH THEN
    dch(xor,crs,x,y,carret,0c); RETURN
  ELSE
    b:=bH-y;
    carret^.h:=b;   dch(xor,crs,x,y,carret,0c);
    carret^.h:=h-b; dch(xor,crs,x,0,carret,0c);
    carret^.h:=h
  END
END toggle_carret;

PROCEDURE write(VAL str: ARRAY OF CHAR; pos,len: INTEGER);

  VAR b: INTEGER;
     ch: CHAR;
     do: BOOLEAN;
    l,u: INTEGER;
    x,y: INTEGER;
    CAR: BOOLEAN;
   fore: INTEGER;
   back: INTEGER;

  PROCEDURE underline(x: INTEGER);
    VAR l,w: INTEGER;
  BEGIN
    w:=x-u;
    IF w<=0 THEN RETURN END;
    fore:=FGND;
    bmd^.base:=BASE + (y+fH-2) MOD bH * WPL;
    l:=0;
    REPEAT
      bblt(bmd^.base,u,SYSTEM.ADR(filler[fore MOD 2]),u,w);
      INC(bmd^.base,lSIZE); fore:=fore DIV 2; l:=l+1
    UNTIL l=L
  END underline;

  PROCEDURE lf;
  BEGIN
    IF undl THEN underline(x) END;   u:=x;
    IF ln=HIGH(py) THEN
      IF NOT lock THEN scrollup(1) ELSE ln:=0 END;
      y:=(py[ln]+scrl) MOD bH
    ELSE
      INC(ln); y:=(y+fH) MOD bH
    END
  END lf;

BEGIN
  x:= px[cl]+shx;  u:=x;
  y:=(py[ln]+scrl) MOD bH;
  CAR:=car;
  IF CAR THEN toggle_carret END;
  LOOP
    IF len=0 THEN EXIT END;
    ch:=str[pos]; INC(pos); DEC(len);
    do:=cntl & (ch<40c);
    IF do THEN
      CASE ch OF
        |15c: IF undl THEN underline(x) END; x:=shx; cl:=0; u:=x;
        |12c: lf
        |36c: lf; x:=shx; cl:=0; u:=x
      ELSE    do:=FALSE
      END
    END;
    IF NOT do THEN
      fore:=FGND; back:=BGND;
      bmd^.base:=BASE;
      IF y+fH<=bH THEN
        l:=0;
        LOOP
          CASE fore MOD 2 + (back MOD 2)*2 OF
            |0: dch(inv,bmd,x,y,white,0c)
            |1: dch(rep,bmd,x,y,font,ch)
            |2: dch(inv,bmd,x,y,font,ch)
            |3: dch(rep,bmd,x,y,white,0c)
          END;
          l:=l+1;
          IF l=L THEN EXIT END;
          INC(bmd^.base,lSIZE);   back:=back DIV 2;   fore:=fore DIV 2
        END
      ELSE
        b:=bH-y;
        clip0^.base:=font^.base+ORD(ch)*fH;  clip1^.base:=clip0^.base+b;
        clip0^.h   :=b;                      clip1^.h   :=fH-b;
        l:=0;
        LOOP
          CASE fore MOD 2 + (back MOD 2)*2 OF
            |0: white^.h:=b;    dch(inv,bmd,x,y,white,0c);
                white^.h:=fH-b; dch(inv,bmd,x,0,white,0c)
            |1: dch(rep,bmd,x,y,clip0,0c); dch(rep,bmd,x,0,clip1,0c)
            |2: dch(inv,bmd,x,y,clip0,0c); dch(inv,bmd,x,0,clip1,0c)
            |3: white^.h:=b;    dch(rep,bmd,x,y,white,0c);
                white^.h:=fH-b; dch(rep,bmd,x,0,white,0c)
          END;
          l:=l+1;
          IF l=L THEN EXIT END;
          INC(bmd^.base,lSIZE);   back:=back DIV 2;   fore:=fore DIV 2
        END;
        white^.h:=fH
      END;
      IF (ch=007c) & cntl THEN (* bell: nothing *)
      ELSIF cl<HIGH(px)   THEN     x:=x+fW; cl:=cl+1
      ELSIF awp           THEN lf; x:=shx;  cl:=0; u:=x
      ELSIF undl          THEN underline(x+fW);    u:=x
      END
    END
  END;
  IF undl THEN underline(x)  END;
  IF CAR  THEN toggle_carret END
END write;

PROCEDURE scrollup(no: INTEGER);
  VAR adr: ADDRESS;
      CAR: BOOLEAN;
     back: INTEGER;
  l,h,y,n: INTEGER;
BEGIN
  IF lock THEN RETURN END;
  IF no<=0       THEN RETURN END;
  IF no>HIGH(py) THEN CLEAR; RETURN END;
  CAR:=car;
  IF CAR THEN toggle_carret END;
  y:=(H - H MOD fH + scrl) MOD bH;
  no:=no*fH; n:=no; (* number of scan lines that must be cleared *)
  REPEAT
    h:=bH-y;
    IF h>n THEN h:=n END;
    l:=0;  adr:=BASE+y*WPL; back:=BGND;
    REPEAT
      fill(adr,h*WPL,0-back MOD 2); INC(adr,lSIZE); back:=back DIV 2; l:=l+1
    UNTIL l=L;
    y:=(y+h) MOD bH; n:=n-h
  UNTIL n=0;
  shy :=(shy +no) MOD bH;  shift^:=(shy*200h+shx)*400h;
  scrl:=(scrl+no) MOD bH;
  n:=no;
  IF n>bH - H +H MOD fH THEN n:=bH - H +H MOD fH END;
  y:=scrl;
  REPEAT
    h:=n;
    IF y<h THEN h:=y END;
    IF h=0 THEN h:=1 END;
    l:=0;  y:=(y-h+bH) MOD bH;  adr:=BASE+y*WPL;
    REPEAT
      adr^:=0; move(adr+1,adr,h*WPL-1); INC(adr,lSIZE);  l:=l+1
    UNTIL l=L;
    n:=n-h
  UNTIL n=0;
  IF NOT CAR  THEN RETURN END;
--IF pauseok  THEN REPEAT UNTIL pause^*{0}#{} END;
  toggle_carret
END scrollup;

PROCEDURE scrolldw(no: INTEGER);
  VAR adr: ADDRESS;
      CAR: BOOLEAN;
     back: INTEGER;
  l,h,y,n: INTEGER;
BEGIN
  IF lock THEN RETURN END;
  IF no<=0       THEN RETURN END;
  IF no>HIGH(py) THEN CLEAR; RETURN END;
  CAR:=car;
  IF CAR THEN toggle_carret END;
  no:=no*fH; n:=no; (* number of scan lines that must be cleared *)
  y:=(scrl - n + bH) MOD bH;
  REPEAT
    h:=bH-y;
    IF h>n THEN h:=n END;
    l:=0;  adr:=BASE+y*WPL;  back:=BGND;
    REPEAT
      fill(adr,h*WPL,0-back MOD 2); INC(adr,lSIZE); back:=back DIV 2;  l:=l+1
    UNTIL l=L;
    y:=(y+h) MOD bH; n:=n-h
  UNTIL n=0;
  shy :=(shy -no+bH) MOD bH;  shift^:=(shy*200h+shx)*400h;
  scrl:=(scrl-no+bH) MOD bH;
  n:=no;
  y:=(scrl + H - H MOD fH) MOD bH;
  IF n > bH - H + H MOD fH THEN n:=bH - H + H MOD fH END;
  REPEAT
    h:=bH-y;
    IF h>n THEN h:=n END;
    l:=0;  adr:=BASE+y*WPL;
    REPEAT
      adr^:=0; move(adr+1,adr,h*WPL-1); INC(adr,lSIZE);  l:=l+1
    UNTIL l=L;
    y:=(y+h) MOD bH; n:=n-h
  UNTIL n=0;
  IF NOT CAR  THEN RETURN END;
--IF pauseok  THEN REPEAT UNTIL pause^*{0}#{} END;
  toggle_carret
END scrolldw;

PROCEDURE erarng(x,y,w: INTEGER);
  VAR  adr: ADDRESS;
      back: INTEGER;
      base: ADDRESS;
      fill: ADDRESS;
     d,n,l: INTEGER;
BEGIN
  back:=BGND;
  base:=BASE;
  l:=0;
  REPEAT
    d:=y;
    n:=fH; fill:=SYSTEM.ADR(filler[back MOD 2]);
    REPEAT
      adr:=base+d*WPL; bblt(adr,x,fill,x,w);
      d:=(d+1) MOD bH; DEC(n)
    UNTIL n=0;
    INC(base,lSIZE); back:=back DIV 2;  l:=l+1
  UNTIL l=L
END erarng;

PROCEDURE eraln(dir: INTEGER);
  VAR adr: ADDRESS;
      i,l: INTEGER;
      x,y: INTEGER;
     fill: INTEGER;
     base: ADDRESS;
     back: INTEGER;
    d,w,n: INTEGER;
BEGIN
  x:= px[cl]+shx;
  y:=(py[ln]+scrl) MOD bH;
  CASE dir OF
    |1: i:=INTEGER(BITSET(x)-BITSET(31));
        w:=x+fW-i; erarng(i,y,w); w:=i DIV 32; x:=0
    |2: w:=(W+shx+31) DIV 32; x:=0
  ELSE (* 0 *)
        w:=x MOD 32;
        IF w#0 THEN
          w:=32-w; erarng(x,y,w); x:=x DIV 32 + 1
        ELSE
          x:=x DIV 32
        END;
        w:=(W+shx+31) DIV 32 - x
  END;
  IF w=0 THEN RETURN END;
  w:=w-1; (* for filling move *)
  base:=BASE+x; back:=BGND;
  l:=0;
  REPEAT
    n:=fH; d:=y; fill:=0-back MOD 2;
    REPEAT
      adr:=base+d*WPL;  adr^:=fill;  move(adr+1,adr,w);
      d:=(d+1) MOD bH;  DEC(n)
    UNTIL n=0;
    INC(base,lSIZE); back:=back DIV 2;  l:=l+1
  UNTIL l=L
END eraln;

PROCEDURE erach(no: INTEGER);
BEGIN
  IF no<=0          THEN           RETURN END;
  IF cl+no>HIGH(px) THEN eraln(0); RETURN END;
  erarng(px[cl]+shx,(py[ln]+scrl) MOD bH,no*fW)
END erach;

PROCEDURE erase(dir: INTEGER);
  VAR base: ADDRESS;
      back: INTEGER;
   y,l,n,h: INTEGER;
BEGIN
  IF dir=2 THEN CLEAR; RETURN END;
  l:=ln;
  IF dir=1 THEN
    (* to screen top *)
    IF cl<HIGH(px) THEN eraln(dir) END;
    IF ln=0        THEN RETURN END;
    n:=py[ln]; y:=scrl
  ELSE
    IF (ln=0) & (cl=0) THEN CLEAR; RETURN END;
    (* to screen bottom *)
    IF cl>0       THEN eraln(dir); INC(l) END;
    IF l>HIGH(py) THEN RETURN END;
    n:=py[l];
    y:=(scrl+n) MOD bH; n:=H - H MOD fH - n
  END;
  REPEAT
    h:=bH-y;
    IF h>n THEN h:=n END;
    base:=BASE+y*WPL; back:=BGND;
    l:=0;
    REPEAT
      fill(base,h*WPL,0-back MOD 2); INC(base,lSIZE); back:=back DIV 2;  l:=l+1
    UNTIL l=L;
    y:=(y+h) MOD bH; n:=n-h
  UNTIL n=0
END erase;

PROCEDURE delln(no: INTEGER);
  VAR des: ADDRESS;
      sou: ADDRESS;
      s,d: INTEGER;
     back: INTEGER;
    h,l,n: INTEGER;
BEGIN
  IF no<=0          THEN RETURN END;
  IF ln+no>HIGH(py) THEN n:=cl; cl:=0; erase(0); cl:=n; RETURN END;
  n:=HIGH(py)+1-(ln+no);
  d:=(scrl+py[ln]) MOD bH;
  n:=n*fH;  no:=no*fH;
  s:=(d+no) MOD bH;
  REPEAT
    h:=fH;
    IF h>n    THEN h:=n    END;
    IF s+h>bH THEN h:=bH-s END;
    IF d+h>bH THEN h:=bH-d END;
    des:=BASE+d*WPL; sou:=BASE+s*WPL;
    l:=0;
    REPEAT
      move(des,sou,h*WPL); INC(des,lSIZE); INC(sou,lSIZE); l:=l+1
    UNTIL l=L;
    d:=(d+h) MOD bH;  s:=(s+h) MOD bH;  n:=n-h
  UNTIL n=0;
  REPEAT
    h:=bH-d;
    IF h>no THEN h:=no END;
    des:=BASE+d*WPL; back:=BGND;
    l:=0;
    REPEAT
      fill(des,h*WPL,0-back MOD 2); INC(des,lSIZE); back:=back DIV 2;  l:=l+1
    UNTIL l=L;
    d:=(d+h) MOD bH;  no:=no-h
  UNTIL no=0
END delln;

PROCEDURE insln(no: INTEGER);
  VAR des: ADDRESS;
      sou: ADDRESS;
      s,d: INTEGER;
     back: INTEGER;
    h,l,n: INTEGER;
BEGIN
  IF no<=0          THEN RETURN END;
  IF ln+no>HIGH(py) THEN n:=cl; cl:=0; erase(0); cl:=n; RETURN END;
  n:=HIGH(py)+1-ln-no;
  d:=(scrl + H - H MOD fH) MOD bH;
  n:=n*fH;  no:=no*fH;
  s:=(d-no) MOD bH;
  REPEAT
    h:=fH;
    IF h>n THEN h:=n END;
    IF s<h THEN h:=s END;
    IF d<h THEN h:=d END;
    IF h=0 THEN h:=1 END;
    d:=(d-h) MOD bH; s:=(s-h) MOD bH;
    des:=BASE+d*WPL; sou:=BASE+s*WPL;
    l:=0;
    REPEAT
      croll(des,sou,h*WPL); INC(des,lSIZE); INC(sou,lSIZE);  l:=l+1
    UNTIL l=L;
    n:=n-h
  UNTIL n=0;
  d:=(scrl+py[ln]) MOD bH;
  REPEAT
    h:=bH-d;
    IF h>no THEN h:=no END;
    des:=BASE+d*WPL; back:=BGND;
    l:=0;
    REPEAT
      fill(des,h*WPL,0-back MOD 2); INC(des,lSIZE); back:=back DIV 2;  l:=l+1
    UNTIL l=L;
    d:=(d+h) MOD bH;
    no:=no-h
  UNTIL no=0
END insln;

VAR LINE: ARRAY [0..L-1]  OF
          ARRAY [0..fH-1] OF ARRAY [0..WPL-1] OF INTEGER;

PROCEDURE putLINE(y: INTEGER);
  VAR sou: ADDRESS;
      des: ADDRESS;
      l,d: INTEGER;
      n,h: INTEGER;
     base: ADDRESS;
BEGIN
  base:=BASE;
  sou :=SYSTEM.ADR(LINE);
  l:=0;
  IF pauseok THEN REPEAT UNTIL pause^*{0}#{} END;
  REPEAT
    n:=fH;
    d:=y;
    REPEAT
      h:=fH;
      IF h>n    THEN h:=n    END;
      IF d+h>bH THEN h:=bH-d END;
      des:=base+d*WPL; move(des,sou,h*WPL); INC(sou,h*WPL);
      d:=(d+h) MOD bH; DEC(n,h)
    UNTIL n=0;
    INC(base,lSIZE);  l:=l+1
  UNTIL l=L
END putLINE;

PROCEDURE delch(no: INTEGER);
  VAR des: ADDRESS;
      sou: ADDRESS;
      n,d: INTEGER;
     back: INTEGER;
     fill: ADDRESS;
     base: ADDRESS;
    l,x,y: INTEGER;
    m,w,b: INTEGER;
BEGIN
  IF no<=0          THEN RETURN END;
  IF cl+no>HIGH(px) THEN eraln(0); RETURN END;
  x:= px[cl]+shx;
  y:=(py[ln]+scrl) MOD bH;
  m:=(x+31) DIV 32;
  w:=no*fW;
  b:=W+shx-x-w;
  base:=BASE; back:=BGND;
  des :=SYSTEM.ADR(LINE);
  l:=0;
  REPEAT
    n:=fH;
    d:=y;  fill:=SYSTEM.ADR(filler[back MOD 2]);
    REPEAT
      sou:=base+d*WPL;
      move(des,sou,m); bblt(des,x,sou,x+w,b); bblt(des,x+b,fill,x+b,w);
      INC (des,WPL);   d:=(d+1) MOD bH;       DEC(n)
    UNTIL n=0;
    INC(base,lSIZE); back:=back DIV 2;  l:=l+1
  UNTIL l=L;
  putLINE(y)
END delch;

PROCEDURE insch(no: INTEGER);
  VAR des: ADDRESS;
      sou: ADDRESS;
      n,d: INTEGER;
     back: INTEGER;
     fill: ADDRESS;
     base: ADDRESS;
    l,x,y: INTEGER;
    m,w,b: INTEGER;
BEGIN
  IF no<=0          THEN RETURN END;
  IF cl+no>HIGH(px) THEN eraln(0); RETURN END;
  x:= px[cl]+shx;
  y:=(py[ln]+scrl) MOD bH;
  m:=(x+31) DIV 32;
  w:=no*fW;
  b:=W+shx-x-w;
  base:=BASE; back:=BGND;
  des :=SYSTEM.ADR(LINE);
  l:=0;
  REPEAT
    n:=fH;
    d:=y;  fill:=SYSTEM.ADR(filler[back MOD 2]);
    REPEAT
      sou:=base+d*WPL;
      move(des,sou,m); bblt(des,x,fill,x,w);  bblt(des,x+w,sou,x,b);
      INC (des,WPL);   d:=(d+1) MOD bH;       DEC(n)
    UNTIL n=0;
    INC(base,lSIZE); back:=back DIV 2;  l:=l+1
  UNTIL l=L;
  putLINE(y)
END insch;

-------------------------------------------------------------

CONST
  FONT0 = ARRAY OF INTEGER
  {
    00h,0Eh,00h,00h,06h,09h,09h,09h,06h,00h,00h,0Ch,12h,12h,12h,0Ch,00h,00h,06h,09h,09h,09h,06h,00h,00h,08h,0Ch,08h,08h,1Ch,00h,00h,06h,09h,09h,09h,06h,00h,00h,0Ch,12h,08h,04h,1Eh,00h,00h,06h,09h,
    09h,09h,06h,00h,00h,0Ch,12h,08h,12h,0Ch,00h,00h,06h,09h,09h,09h,06h,00h,00h,12h,12h,1Eh,10h,10h,00h,00h,06h,09h,09h,09h,06h,00h,00h,1Eh,02h,0Eh,10h,0Eh,00h,00h,06h,09h,09h,09h,06h,00h,00h,1Ch,
    02h,0Eh,12h,0Ch,00h,00h,04h,04h,0Eh,0Ah,0Ah,0Ah,1Bh,1Fh,04h,02h,07h,02h,00h,00h,02h,03h,02h,02h,07h,00h,0Ch,12h,12h,12h,0Ch,00h,00h,00h,02h,03h,02h,02h,07h,00h,08h,0Ch,08h,08h,1Ch,00h,00h,00h,
    02h,03h,02h,02h,07h,00h,00h,0Ch,12h,08h,04h,1Eh,00h,00h,02h,03h,02h,02h,07h,00h,00h,0Ch,12h,08h,12h,0Ch,00h,00h,02h,03h,02h,02h,07h,00h,00h,12h,12h,1Eh,10h,10h,00h,00h,02h,03h,02h,02h,07h,00h,
    00h,1Eh,02h,0Eh,10h,0Eh,00h,00h,02h,03h,02h,02h,07h,00h,00h,1Ch,02h,0Eh,12h,0Ch,00h,00h,02h,03h,02h,02h,07h,00h,1Eh,10h,08h,04h,04h,00h,00h,00h,06h,09h,04h,02h,0Fh,00h,00h,0Ch,12h,12h,12h,0Ch,
    00h,00h,06h,09h,04h,02h,0Fh,00h,00h,08h,0Ch,08h,08h,1Ch,00h,00h,06h,09h,04h,02h,0Fh,00h,00h,0Ch,12h,08h,04h,1Eh,00h,00h,06h,09h,04h,02h,0Fh,00h,00h,0Eh,10h,0Ch,10h,0Eh,00h,00h,06h,09h,04h,02h,
    0Fh,00h,00h,12h,12h,1Eh,10h,10h,00h,00h,06h,09h,04h,02h,0Fh,00h,00h,1Eh,02h,0Eh,10h,0Eh,00h,00h,06h,09h,04h,02h,0Fh,00h,00h,1Ch,02h,0Eh,12h,0Ch,00h,00h,06h,09h,04h,02h,0Fh,00h,1Eh,10h,08h,04h,
    04h,00h,00h,00h,06h,09h,04h,09h,06h,00h,0Ch,12h,12h,12h,0Ch,00h,00h,00h,06h,09h,04h,09h,06h,00h,08h,0Ch,08h,08h,1Ch,00h,00h,00h,06h,09h,04h,09h,06h,00h,00h,0Ch,12h,08h,04h,1Eh,00h,00h,06h,09h,
    04h,09h,06h,00h,00h,0Ch,12h,08h,12h,0Ch,00h,00h,06h,09h,04h,09h,06h,00h,00h,12h,12h,1Eh,10h,10h,00h,00h,06h,09h,04h,09h,06h,00h,00h,1Eh,02h,0Eh,10h,0Eh,00h,00h,06h,09h,04h,09h,06h,00h,00h,1Ch,
    02h,0Eh,12h,0Ch,00h,00h,06h,09h,04h,09h,06h,00h,1Eh,10h,08h,04h,04h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,04h,04h,04h,04h,04h,04h,00h,00h,04h,00h,00h,00h,
    00h,00h,0Ah,0Ah,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,0Ah,0Ah,1Fh,0Ah,0Ah,1Fh,0Ah,0Ah,00h,00h,00h,00h,00h,0Eh,15h,05h,05h,0Eh,14h,14h,15h,0Eh,00h,00h,00h,00h,00h,00h,12h,12h,08h,
    08h,04h,04h,12h,12h,00h,00h,00h,00h,00h,04h,0Ah,0Ah,0Ah,04h,0Ah,09h,09h,15h,16h,00h,00h,00h,00h,04h,04h,02h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,08h,04h,02h,02h,02h,02h,02h,04h,08h,00h,
    00h,00h,00h,00h,02h,04h,08h,08h,08h,08h,08h,04h,02h,00h,00h,00h,00h,00h,00h,00h,0Ah,04h,1Fh,04h,0Ah,00h,00h,00h,00h,00h,00h,00h,00h,04h,04h,04h,1Fh,04h,04h,04h,00h,00h,00h,00h,00h,00h,00h,00h,
    00h,00h,00h,00h,00h,04h,04h,02h,00h,00h,00h,00h,00h,00h,00h,00h,1Fh,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,0Ch,0Ch,00h,00h,00h,00h,00h,00h,08h,08h,04h,04h,02h,02h,01h,
    01h,00h,00h,00h,00h,00h,0Eh,11h,11h,19h,15h,13h,11h,11h,0Eh,00h,00h,00h,00h,00h,04h,06h,04h,04h,04h,04h,04h,04h,0Eh,00h,00h,00h,00h,00h,0Eh,11h,10h,10h,0Ch,02h,01h,01h,1Fh,00h,00h,00h,00h,00h,
    0Eh,11h,10h,10h,0Ch,10h,10h,11h,0Eh,00h,00h,00h,00h,00h,0Ch,0Ah,09h,09h,09h,1Fh,08h,08h,08h,00h,00h,00h,00h,00h,1Fh,01h,01h,0Fh,10h,10h,10h,11h,0Eh,00h,00h,00h,00h,00h,0Eh,11h,01h,0Fh,11h,11h,
    11h,11h,0Eh,00h,00h,00h,00h,00h,1Fh,10h,10h,08h,08h,04h,04h,02h,02h,00h,00h,00h,00h,00h,0Eh,11h,11h,11h,0Eh,11h,11h,11h,0Eh,00h,00h,00h,00h,00h,0Eh,11h,11h,11h,1Eh,10h,10h,11h,0Eh,00h,00h,00h,
    00h,00h,00h,00h,04h,04h,00h,00h,04h,04h,00h,00h,00h,00h,00h,00h,00h,00h,08h,08h,00h,00h,00h,08h,08h,04h,00h,00h,00h,00h,00h,08h,04h,02h,01h,02h,04h,08h,00h,00h,00h,00h,00h,00h,00h,00h,00h,0Fh,
    00h,00h,0Fh,00h,00h,00h,00h,00h,00h,00h,00h,02h,04h,08h,10h,08h,04h,02h,00h,00h,00h,00h,00h,00h,0Eh,11h,10h,10h,08h,04h,00h,00h,04h,00h,00h,00h,00h,00h,0Eh,11h,1Dh,15h,15h,1Dh,01h,01h,1Eh,00h,
    00h,00h,00h,00h,0Eh,11h,11h,11h,1Fh,11h,11h,11h,11h,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,0Fh,11h,11h,11h,0Fh,00h,00h,00h,00h,00h,0Eh,11h,01h,01h,01h,01h,01h,11h,0Eh,00h,00h,00h,00h,00h,0Fh,11h,
    11h,11h,11h,11h,11h,11h,0Fh,00h,00h,00h,00h,00h,1Fh,01h,01h,01h,0Fh,01h,01h,01h,1Fh,00h,00h,00h,00h,00h,1Fh,01h,01h,01h,0Fh,01h,01h,01h,01h,00h,00h,00h,00h,00h,0Eh,11h,01h,01h,01h,1Dh,11h,11h,
    0Eh,00h,00h,00h,00h,00h,11h,11h,11h,11h,1Fh,11h,11h,11h,11h,00h,00h,00h,00h,00h,1Fh,04h,04h,04h,04h,04h,04h,04h,1Fh,00h,00h,00h,00h,00h,1Eh,08h,08h,08h,08h,08h,08h,09h,06h,00h,00h,00h,00h,00h,
    11h,11h,09h,05h,03h,05h,09h,11h,11h,00h,00h,00h,00h,00h,01h,01h,01h,01h,01h,01h,01h,01h,1Fh,00h,00h,00h,00h,00h,11h,1Bh,1Bh,15h,15h,11h,11h,11h,11h,00h,00h,00h,00h,00h,11h,11h,13h,15h,15h,15h,
    19h,11h,11h,00h,00h,00h,00h,00h,0Eh,11h,51h,51h,51h,51h,51h,11h,0Eh,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,0Fh,01h,01h,01h,01h,00h,00h,00h,00h,00h,0Eh,11h,11h,11h,11h,11h,15h,09h,16h,10h,00h,00h,
    00h,00h,0Fh,11h,11h,11h,0Fh,05h,09h,11h,11h,00h,00h,00h,00h,00h,0Eh,11h,01h,01h,0Eh,10h,10h,11h,0Eh,00h,00h,00h,00h,00h,1Fh,04h,04h,04h,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,11h,11h,11h,11h,
    11h,11h,11h,11h,0Eh,00h,00h,00h,00h,00h,11h,11h,11h,11h,11h,0Ah,0Ah,04h,04h,00h,00h,00h,00h,00h,11h,15h,15h,15h,15h,15h,15h,15h,0Ah,00h,00h,00h,00h,00h,51h,11h,0Ah,0Ah,04h,0Ah,0Ah,11h,51h,00h,
    00h,00h,00h,00h,11h,11h,0Ah,0Ah,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,1Fh,10h,08h,08h,04h,02h,02h,01h,1Fh,00h,00h,00h,00h,00h,0Ch,04h,04h,04h,04h,04h,04h,04h,0Ch,00h,00h,00h,00h,00h,01h,01h,
    02h,02h,04h,08h,08h,10h,10h,00h,00h,00h,00h,00h,0Ch,08h,08h,08h,08h,08h,08h,08h,0Ch,00h,00h,00h,00h,00h,04h,0Ah,11h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,
    00h,5Fh,00h,00h,00h,00h,04h,04h,08h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,0Eh,10h,10h,1Eh,11h,11h,2Eh,00h,00h,00h,00h,00h,01h,01h,0Fh,11h,11h,11h,11h,11h,0Fh,00h,00h,00h,00h,00h,
    00h,00h,0Eh,11h,01h,01h,01h,11h,0Eh,00h,00h,00h,00h,00h,10h,10h,1Eh,11h,11h,11h,11h,11h,1Eh,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,11h,1Fh,01h,01h,0Eh,00h,00h,00h,00h,00h,18h,04h,04h,1Eh,04h,04h,
    04h,04h,04h,00h,00h,00h,00h,00h,00h,10h,0Eh,11h,11h,11h,0Eh,10h,10h,11h,0Eh,00h,00h,00h,01h,01h,0Fh,11h,11h,11h,11h,11h,11h,00h,00h,00h,00h,04h,04h,00h,06h,04h,04h,04h,04h,04h,0Eh,00h,00h,00h,
    00h,08h,08h,00h,0Eh,08h,08h,08h,08h,08h,08h,09h,06h,00h,00h,00h,01h,01h,11h,09h,05h,03h,05h,09h,11h,00h,00h,00h,00h,00h,06h,04h,04h,04h,04h,04h,04h,04h,0Eh,00h,00h,00h,00h,00h,00h,00h,0Fh,15h,
    15h,15h,15h,15h,15h,00h,00h,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,11h,11h,11h,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,11h,11h,11h,11h,0Eh,00h,00h,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,11h,11h,0Fh,01h,
    01h,00h,00h,00h,00h,00h,1Eh,11h,11h,11h,11h,11h,1Eh,10h,10h,00h,00h,00h,00h,00h,0Dh,13h,01h,01h,01h,01h,01h,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,01h,0Eh,10h,11h,0Eh,00h,00h,00h,00h,00h,04h,04h,
    1Eh,04h,04h,04h,04h,04h,18h,00h,00h,00h,00h,00h,00h,00h,11h,11h,11h,11h,11h,11h,0Eh,00h,00h,00h,00h,00h,00h,00h,11h,11h,11h,0Ah,0Ah,04h,04h,00h,00h,00h,00h,00h,00h,00h,11h,15h,15h,15h,15h,15h,
    0Ah,00h,00h,00h,00h,00h,00h,00h,11h,11h,0Ah,04h,0Ah,11h,11h,00h,00h,00h,00h,00h,00h,00h,11h,11h,11h,11h,1Eh,10h,10h,11h,0Eh,00h,00h,00h,00h,00h,1Fh,10h,08h,04h,02h,01h,1Fh,00h,00h,00h,00h,00h,
    08h,04h,04h,04h,02h,04h,04h,04h,08h,00h,00h,00h,00h,00h,04h,04h,04h,04h,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,02h,04h,04h,04h,08h,04h,04h,04h,02h,00h,00h,00h,00h,00h,02h,15h,08h,00h,00h,00h,
    00h,00h,00h,00h,00h,00h,00h,5Fh,5Fh,5Fh,5Fh,5Fh,5Fh,5Fh,5Fh,5Fh,5Fh,5Fh,00h,00h,10h,12h,12h,12h,12h,12h,12h,12h,12h,12h,12h,12h,12h,02h,05h,14h,05h,14h,05h,14h,05h,14h,05h,14h,05h,14h,05h,14h,
    12h,09h,12h,09h,12h,09h,12h,09h,12h,09h,12h,09h,12h,09h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,07h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,07h,
    04h,07h,04h,04h,04h,04h,04h,04h,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Bh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,00h,00h,00h,00h,00h,00h,0Fh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,00h,00h,00h,00h,00h,07h,04h,07h,04h,04h,04h,04h,
    04h,04h,0Ah,0Ah,0Ah,0Ah,0Ah,0Bh,08h,0Bh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,00h,00h,00h,00h,00h,0Fh,08h,0Bh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,
    0Ah,0Bh,08h,0Fh,00h,00h,00h,00h,00h,00h,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Fh,00h,00h,00h,00h,00h,00h,00h,04h,04h,04h,04h,04h,07h,04h,07h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,07h,04h,04h,04h,
    04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,3Ch,00h,00h,00h,00h,00h,00h,00h,04h,04h,04h,04h,04h,04h,3Fh,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,3Fh,04h,04h,04h,04h,04h,04h,04h,04h,04h,
    04h,04h,04h,04h,3Ch,04h,04h,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,00h,3Fh,00h,00h,00h,00h,00h,00h,00h,04h,04h,04h,04h,04h,04h,3Fh,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,3Ch,04h,3Ch,
    04h,04h,04h,04h,04h,04h,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,3Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,3Ah,02h,3Eh,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,3Eh,02h,3Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,
    0Ah,0Ah,0Ah,0Ah,0Ah,3Bh,00h,3Fh,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,3Fh,00h,3Bh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,3Ah,02h,3Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,00h,00h,00h,00h,00h,3Fh,
    00h,3Fh,00h,00h,00h,00h,00h,00h,0Ah,0Ah,0Ah,0Ah,0Ah,3Bh,00h,3Bh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,04h,04h,04h,04h,04h,3Fh,00h,3Fh,00h,00h,00h,00h,00h,00h,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,3Fh,00h,00h,00h,00h,00h,
    00h,00h,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,3Eh,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,3Fh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,00h,00h,00h,00h,00h,3Fh,00h,3Fh,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,
    04h,3Ch,04h,3Ch,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,3Ch,04h,3Ch,04h,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,00h,3Eh,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,3Fh,0Ah,0Ah,0Ah,
    0Ah,0Ah,0Ah,0Ah,04h,04h,04h,04h,04h,3Fh,04h,3Fh,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,04h,07h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,3Ch,04h,04h,04h,04h,04h,04h,04h,3Fh,3Fh,
    3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,00h,00h,00h,00h,00h,00h,00h,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,38h,38h,38h,38h,38h,38h,38h,38h,
    38h,38h,38h,38h,38h,38h,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,3Fh,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,1Eh,21h,2Dh,25h,25h,2Dh,21h,1Eh,00h,00h,00h,00h,00h,00h,1Eh,21h,2Dh,2Dh,25h,35h,21h,1Eh,00h,00h,
    00h,00h,00h,00h,00h,00h,00h,07h,07h,07h,07h,07h,07h,07h,38h,38h,38h,38h,38h,38h,38h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,38h,38h,38h,38h,38h,38h,38h,07h,07h,07h,07h,07h,07h,
    07h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,04h,0Eh,1Fh,1Bh,04h,0Eh,00h,00h,00h,00h,00h,00h,00h,04h,0Eh,04h,1Fh,1Bh,04h,0Eh,00h,00h,00h,00h,00h,00h,00h,00h,04h,0Eh,1Fh,0Eh,04h,00h,00h,
    00h,00h,00h,00h,00h,00h,00h,00h,1Bh,1Fh,0Eh,04h,00h,00h,00h,00h,00h,00h,00h,00h,04h,08h,1Fh,08h,04h,00h,00h,00h,00h,00h,00h,00h,00h,00h,04h,02h,1Fh,02h,04h,00h,00h,00h,00h,00h,00h,00h,04h,04h,
    0Eh,0Eh,1Fh,15h,04h,04h,04h,04h,00h,00h,00h,00h,00h,04h,04h,04h,15h,1Fh,0Eh,0Eh,04h,04h,00h,00h,00h,00h,00h,00h,00h,04h,02h,1Fh,12h,14h,10h,10h,10h,00h,15h,2Ah,15h,2Ah,15h,2Ah,15h,2Ah,15h,2Ah,
    15h,2Ah,15h,2Ah,00h,00h,00h,00h,09h,15h,15h,17h,15h,15h,09h,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,18h,16h,11h,19h,26h,00h,00h,00h,00h,00h,00h,00h,0Fh,01h,01h,0Fh,11h,11h,0Fh,00h,00h,00h,00h,00h,
    00h,00h,09h,09h,09h,09h,09h,09h,1Fh,10h,08h,00h,00h,00h,00h,00h,0Ch,0Ah,0Ah,0Ah,0Ah,0Ah,1Fh,11h,00h,00h,00h,00h,00h,00h,0Eh,11h,11h,0Fh,01h,11h,0Eh,00h,00h,00h,00h,00h,00h,04h,0Eh,15h,15h,15h,
    15h,0Eh,04h,04h,00h,00h,00h,00h,00h,00h,1Fh,01h,01h,01h,01h,01h,01h,00h,00h,00h,00h,00h,00h,00h,11h,11h,0Ah,04h,0Ah,11h,11h,00h,00h,00h,00h,00h,00h,00h,11h,19h,19h,15h,13h,13h,11h,00h,00h,00h,
    00h,00h,04h,04h,11h,19h,19h,15h,13h,13h,11h,00h,00h,00h,00h,00h,00h,00h,11h,11h,09h,07h,09h,11h,11h,00h,00h,00h,00h,00h,00h,00h,1Eh,12h,12h,12h,12h,12h,11h,00h,00h,00h,00h,00h,00h,00h,11h,1Bh,
    15h,15h,11h,11h,11h,00h,00h,00h,00h,00h,00h,00h,11h,11h,11h,1Fh,11h,11h,11h,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,11h,11h,11h,11h,0Eh,00h,00h,00h,00h,00h,00h,00h,1Fh,11h,11h,11h,11h,11h,11h,00h,
    00h,00h,00h,00h,00h,00h,1Eh,11h,11h,1Eh,14h,12h,11h,00h,00h,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,11h,0Fh,01h,01h,00h,00h,00h,00h,00h,00h,1Eh,01h,01h,01h,01h,01h,1Eh,00h,00h,00h,00h,00h,00h,00h,
    1Fh,04h,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,00h,00h,11h,11h,11h,11h,1Eh,10h,10h,09h,06h,00h,00h,00h,00h,00h,15h,15h,15h,0Eh,15h,15h,15h,00h,00h,00h,00h,00h,00h,00h,0Fh,11h,11h,0Fh,11h,11h,
    0Fh,00h,00h,00h,00h,00h,00h,00h,01h,01h,01h,0Fh,11h,11h,0Fh,00h,00h,00h,00h,00h,00h,00h,11h,11h,11h,13h,15h,15h,13h,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,10h,0Ch,10h,11h,0Eh,00h,00h,00h,00h,00h,
    00h,00h,11h,15h,15h,15h,15h,15h,1Fh,00h,00h,00h,00h,00h,00h,00h,0Eh,11h,10h,1Ch,10h,11h,0Eh,00h,00h,00h,00h,00h,00h,00h,11h,15h,15h,15h,15h,15h,1Fh,10h,08h,00h,00h,00h,00h,00h,11h,11h,11h,1Eh,
    10h,10h,10h,00h,00h,00h,00h,00h,00h,00h,03h,02h,02h,0Eh,12h,12h,0Eh,00h,00h,00h,00h,00h,09h,15h,15h,15h,17h,15h,15h,15h,09h,00h,00h,00h,00h,00h,1Ch,12h,11h,11h,11h,11h,1Fh,11h,11h,00h,00h,00h,
    00h,00h,1Fh,01h,01h,01h,0Fh,11h,11h,11h,0Fh,00h,00h,00h,00h,00h,11h,11h,11h,11h,11h,11h,11h,11h,1Fh,10h,08h,00h,00h,00h,0Ch,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,0Ah,1Fh,11h,00h,00h,00h,00h,1Fh,01h,01h,01h,
    0Fh,01h,01h,01h,1Fh,00h,00h,00h,00h,00h,0Eh,15h,15h,15h,15h,15h,15h,15h,0Eh,04h,04h,00h,00h,00h,1Fh,12h,02h,02h,02h,02h,02h,02h,07h,00h,00h,00h,00h,00h,51h,11h,11h,0Ah,04h,0Ah,11h,11h,11h,00h,
    00h,00h,00h,00h,11h,11h,19h,19h,15h,13h,13h,11h,11h,00h,00h,00h,00h,00h,15h,11h,19h,19h,15h,13h,13h,11h,11h,00h,00h,00h,00h,00h,11h,11h,09h,05h,03h,05h,09h,11h,11h,00h,00h,00h,00h,00h,1Ch,12h,
    12h,12h,12h,12h,12h,12h,11h,00h,00h,00h,00h,00h,11h,1Bh,1Bh,15h,15h,11h,11h,11h,11h,00h,00h,00h,00h,00h,11h,11h,11h,1Fh,11h,11h,11h,11h,11h,00h,00h,00h,00h,00h,0Eh,11h,11h,11h,11h,11h,11h,11h,
    0Eh,00h,00h,00h,00h,00h,1Fh,11h,11h,11h,11h,11h,11h,11h,11h,00h,00h,00h,00h,00h,1Eh,11h,11h,11h,1Eh,14h,12h,11h,11h,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,0Fh,01h,01h,01h,01h,00h,00h,00h,00h,00h,
    0Eh,11h,01h,01h,01h,01h,01h,11h,0Eh,00h,00h,00h,00h,00h,1Fh,04h,04h,04h,04h,04h,04h,04h,04h,00h,00h,00h,00h,00h,11h,11h,11h,11h,1Eh,10h,10h,11h,0Eh,00h,00h,00h,00h,00h,15h,15h,15h,15h,0Eh,15h,
    15h,15h,15h,00h,00h,00h,00h,00h,0Fh,11h,11h,11h,0Fh,11h,11h,11h,0Fh,00h,00h,00h,00h,00h,01h,01h,01h,01h,0Fh,11h,11h,11h,0Fh,00h,00h,00h,00h,00h,11h,11h,11h,13h,15h,15h,15h,15h,13h,00h,00h,00h,
    00h,00h,0Eh,11h,10h,08h,06h,08h,10h,11h,0Eh,00h,00h,00h,00h,00h,11h,15h,15h,15h,15h,15h,15h,15h,1Fh,00h,00h,00h,00h,00h,0Eh,11h,10h,10h,1Eh,10h,10h,11h,0Eh,00h,00h,00h,00h,00h,11h,15h,15h,15h,
    15h,15h,15h,15h,1Fh,10h,08h,00h,00h,00h,11h,11h,11h,11h,1Eh,10h,10h,10h,10h,00h,00h,00h,00h,00h,03h,02h,02h,02h,0Eh,12h,12h,12h,0Eh,00h,00h,00h
  };

-------------------------------------------------------------

BEGIN
  init_hw(FONT0);
  lines:=HIGH(py)+1;    columns:=HIGH(px)+1;
  gmin:=- G DIV 2;      gmax   :=G DIV 2 - 1;
  BGND:=0;              FGND   :=G-1;
  ln  :=0;              cl     :=0;
  car :=FALSE;          CLEAR;
  set_lc(lines-1,0);    RESET;
END KTV480.
