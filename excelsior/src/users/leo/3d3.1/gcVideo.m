IMPLEMENTATION MODULE gcVideo; (* Leo 12-Mar-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  tsk: tskEnv;
IMPORT  key: Keyboard;

TYPE
  WORD      = SYSTEM.WORD;
  ADDRESS   = SYSTEM.ADDRESS;

----------------------------------------------------------------

CONST
  GREEN = FALSE;
  hH = 360;  hW = 480;
  fH =  14;  fW =   6;  KB  = 256;
  bH = 512;  bW = 512;  WPL = 16;  bL=4;  lSIZE=WPL*bH;

  SHIFT   = ADDRESS( 1F0000h );
  PLT     = ADDRESS( 1F0010h );
  PAUSE   = ADDRESS( 1F0020h );

  BASE    = ADDRESS( 1F8000h );

  p_trans = ARRAY OF INTEGER
              {0Fh,07,0Bh,03,0Dh,05,09h,01,0Eh,06,0Ah,02,0Ch,04,08h,00};


----------------------------------------------------------------

TYPE
   BMDPTR = POINTER TO BMD;
   BMD    = RECORD
              w,h : INTEGER;
              wpl : INTEGER;
              base: ADDRESS;
              patt: BITSET;
            END;

  CLIP   = RECORD
            xa,ya: INTEGER;
            xb,yb: INTEGER;
           END;


CONST rep=0; or=1; xor=2; bic=3;

VAR
    plt: ADDRESS;
  shift: ADDRESS;
  pause: POINTER TO BITSET;
pauseok: BOOLEAN;

    bmd: BMDPTR;
   bbmd: BMD;
   cset: BITSET;
   lineFF: ARRAY [0..15] OF INTEGER;
   lineAA: ARRAY [0..15] OF INTEGER;
   line55: ARRAY [0..15] OF INTEGER;


---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------


PROCEDURE bblt(to: ADDRESS;   to_ofs: INTEGER;
             from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE cod.bblt END bblt;

PROCEDURE bitmove(mode: INTEGER;
                    to: ADDRESS;   to_ofs: INTEGER;
                  from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bitmove;

PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
--CODE 0F9h 00h END in_rect;
  BEGIN RETURN NOT ((x<0) OR (y<0) OR (x>w) OR (y>h)) END in_rect;

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

PROCEDURE _clip(VAR clip: CLIP; w,h:INTEGER): BOOLEAN;
CODE 0F9h 004h END _clip;

PROCEDURE _line(mode: INTEGER; bmd: BMDPTR; x,y,x1,y1: INTEGER);
CODE 0F9h 005h END _line;

PROCEDURE _dot(m: INTEGER; bmd: BMDPTR; x,y: INTEGER);
CODE 1 0F9h 001h END _dot;

PROCEDURE _vline(m: INTEGER; bmd: BMDPTR; x,y,len: INTEGER);
CODE 0F9h 001h END _vline;


----------------------  INIT HARDWARE  -------------------------
                      -----------------

VAR plt0: ARRAY [0..15] OF INTEGER;
    plt1: ARRAY [0..15] OF INTEGER;

PROCEDURE palette(VAL p: ARRAY OF INTEGER);
BEGIN
  IF pauseok THEN REPEAT UNTIL pause^*{0}#{} END;
  move(plt,SYSTEM.ADR(p),SIZE(p))
END palette;

PROCEDURE picture(on: BOOLEAN);
BEGIN
  IF on THEN palette(plt1) ELSE palette(plt0) END
END picture;

VAR odd: BOOLEAN;

PROCEDURE color(c: INTEGER);
BEGIN
--ASSERT((c>=0) & (c<C));
  odd:=ODD(c);
  cset:=(BITSET(c)*{1..4})>>1;
END color;

PROCEDURE erase;
BEGIN
  picture(FALSE);
  fill(BASE,128*KB,0);
  picture(TRUE);
END erase;

PROCEDURE dot(x,y: INTEGER);
  VAR c: BITSET;
BEGIN
  bmd^.base:=BASE; c:=cset;
  WHILE c#{} DO
    IF c*{0}#{} THEN _dot(rep,bmd,x,y) END;
    INC(bmd^.base,lSIZE);
    c:=(c-{0})>>1
  END
END dot;

PROCEDURE vline(x,y,y1: INTEGER);
  VAR i,d: INTEGER; c: BITSET;
BEGIN
  IF x < 0     THEN RETURN END;
  IF x >=W     THEN RETURN END;
  IF y > y1    THEN i:=y1; y1:=y; y:=i END;  (* swap: y<y1 *)
  IF y < 0     THEN y :=0   END; (* clip *)
  IF y1>=H     THEN y1:=H-1 END; (* clip *)
  IF y = y1    THEN dot(x,y); RETURN END;
  d:=y1-y+1;
  IF i<=0 THEN RETURN END; (* out of screen *)
  bmd^.base:=BASE; c:=cset;
  FOR i:=0 TO 3 DO
    IF c*{0}#{} THEN _vline(rep,bmd,x,y,d) ELSE _vline(bic,bmd,x,y,d) END;
    INC(bmd^.base,lSIZE);
    c:=(c-{0})>>1
  END
END vline;

PROCEDURE hline(x,y,x1: INTEGER);
  VAR d,i: INTEGER; c: BITSET;
      adr: ADDRESS;
BEGIN
  IF y < 0     THEN RETURN END;
  IF y >=H     THEN RETURN END;
  IF x = x1    THEN dot(x,y); RETURN END;
  IF x > x1    THEN i:=x1; x1:=x; x:=i END;  (* swap: x<x1 *)
  IF x < 0     THEN x :=0   END; (* clip *)
  IF x1>=W     THEN x1:=W-1 END; (* clip *)
  d:=x1-x+1;
  IF d<=0 THEN RETURN END; (* out of screen *)
  bmd^.base:=BASE+y*WPL; c:=cset;
  IF NOT odd THEN
    IF ODD(y) THEN adr:=SYSTEM.ADR(lineAA)
    ELSE           adr:=SYSTEM.ADR(line55)
    END;
  ELSE             adr:=SYSTEM.ADR(lineFF)
  END;
  FOR i:=0 TO 3 DO
    IF c*{0}#{} THEN
      bitmove(or,bmd^.base,x,adr,x,d)
    ELSE
      bitmove(bic,bmd^.base,x,SYSTEM.ADR(lineFF),x,d)
    END;
    INC(bmd^.base,lSIZE);
    c:=(c-{0})>>1
  END
END hline;

PROCEDURE hline1(x,y,x1: INTEGER);
  VAR d,i: INTEGER; c: BITSET;
      adr: ADDRESS;
BEGIN
  IF y < 0     THEN RETURN END;
  IF y >=H     THEN RETURN END;
  IF x = x1    THEN dot(x,y); RETURN END;
  IF x > x1    THEN i:=x1; x1:=x; x:=i END;  (* swap: x<x1 *)
  IF x < 0     THEN x :=0   END; (* clip *)
  IF x1>=W     THEN x1:=W-1 END; (* clip *)
  d:=x1-x+1;
  IF d<=0 THEN RETURN END; (* out of screen *)
  bmd^.base:=BASE+y*WPL; c:=cset;
  IF NOT odd THEN
    IF ODD(y) THEN adr:=SYSTEM.ADR(line55)
    ELSE           adr:=SYSTEM.ADR(lineAA)
    END;
  ELSE             adr:=SYSTEM.ADR(lineFF)
  END;
  FOR i:=0 TO 3 DO
    IF c*{0}#{} THEN
      bitmove(or,bmd^.base,x,adr,x,d)
    ELSE
  --    bitmove(bic,bmd^.base,x,SYSTEM.ADR(lineFF),x,d)
    END;
    INC(bmd^.base,lSIZE);
    c:=(c-{0})>>1
  END
END hline1;

PROCEDURE erase_line(y: INTEGER; mask: BITSET);
  VAR i: INTEGER;
BEGIN
  IF y < 0     THEN RETURN END;
  IF y >=H     THEN RETURN END;
  bmd^.base:=BASE+y*WPL;
  FOR i:=0 TO 3 DO
    bitmove(bic,bmd^.base,0,SYSTEM.ADR(lineFF),0,W);
    INC(bmd^.base,lSIZE);
  END
END erase_line;

PROCEDURE line (x,y,x1,y1: INTEGER);
  VAR i: INTEGER;
      c: BITSET;
   clip: CLIP;
BEGIN
  IF x=x1 THEN vline(x,y,y1); RETURN END;
  IF y=y1 THEN hline(x,y,x1); RETURN END;
  IF x1<x THEN i:=x1; x1:=x; x:=i;  i:=y1; y1:=y; y:=i END;
  IF NOT in_rect(x,y,W,H) OR NOT in_rect(x1,y1,W,H) THEN
    WITH clip DO
      xa:=x; xb:=x1; ya:=y; yb:=y1;
      IF NOT _clip(clip,W,H) THEN RETURN END;
      x:=xa; x1:=xb; y:=ya; y1:=yb;
    END;
  END;
  bmd^.base:=BASE; c:=cset;
  FOR i:=0 TO 3 DO
    IF c*{0}#{} THEN _line(rep,bmd,x,y,x1,y1)
    ELSE             _line(bic,bmd,x,y,x1,y1)
    END;
    INC(bmd^.base,lSIZE);
    c:=(c-{0})>>1
  END
END line;

PROCEDURE init_plt(VAR p: ARRAY OF INTEGER; zero: BOOLEAN);

  PROCEDURE p_set(color: INTEGER; r,g,b: INTEGER);
  BEGIN
    p[color]:=p_trans[r*4]+p_trans[g*4]*16+p_trans[b*4]*256
  END p_set;

  VAR i,j,k: INTEGER;
      r,g,b: INTEGER;
BEGIN
  FOR i:=0 TO 15 DO p_set(i,0,0,0) END;
  IF zero  THEN RETURN END;
  IF GREEN THEN
    p_set( 0,10,00,00);
    p_set( 1,02,00,00);
    p_set( 2,00,02,00);
    p_set( 3,02,02,00);
    p_set( 4,00,00,01);
    p_set( 5,02,00,01);
    p_set( 6,00,02,01);
    p_set( 7,02,02,01);

    p_set( 8,03,03,03);
    p_set( 9,03,00,00);
    p_set(10,00,03,00);
    p_set(11,03,03,00);
    p_set(12,00,00,02);
    p_set(13,03,00,02);
    p_set(14,00,03,02);
    p_set(15,03,03,02);
  ELSE
    p_set(   0,  0,  0,  0);
    p_set(   1,  1,  0,  0);
    p_set(   2,  2,  0,  0);
    p_set(   3,  3,  0,  0);
    p_set(   4,  0,  1,  0);
    p_set(   5,  0,  2,  0);
    p_set(   6,  0,  3,  0);
    p_set(   7,  0,  1,  1);

    p_set(   8,  0,  2,  2);
    p_set(   9,  0,  3,  3);
    p_set(  10,  0,  0,  1);
    p_set(  11,  0,  0,  2);
    p_set(  12,  0,  0,  3);
    p_set(  13,  1,  1,  1);
    p_set(  14,  2,  2,  2);
    p_set(  15,  3,  3,  3);
  END
END init_plt;

PROCEDURE init_hw;
  VAR i,f: INTEGER;
BEGIN
  H:=hH;
  W:=hW;
  C:=16;

  plt   :=PLT;
  pause :=PAUSE;
  shift :=SHIFT;

  i:=20000;
  REPEAT DEC(i) UNTIL (pause^*{0}={}) OR (i=0);
  IF i#0 THEN i:=20000;
    REPEAT DEC(i) UNTIL (pause^*{0}#{}) OR (i=0)
  END;
  pauseok:=(i#0);

  bmd:=SYSTEM.ADR(bbmd);
  WITH bmd^ DO w:=W;  h:=H;  wpl:=WPL;  base:=BASE; patt:={0..31} END;

  init_plt(plt0,TRUE);
  init_plt(plt1,FALSE);
  palette (plt0);
  erase;
  shift^:=(1FFh*200h+000)*400h;
  fill_arr(lineFF,0FFFFFFFFh);
  fill_arr(lineAA,0AAAAAAAAh);
  fill_arr(line55,055555555h);
END init_hw;

PROCEDURE plane(n: INTEGER);
BEGIN
END plane;

PROCEDURE flush;
BEGIN
END flush;

VAR c1,i1: INTEGER; --color,intencity #1
    c2,i2: INTEGER; --color,intencity #2

CONST sz = 10;

BEGIN
  init_hw;
  color(C-1);
  erase;
  picture(TRUE);
END gcVideo.
