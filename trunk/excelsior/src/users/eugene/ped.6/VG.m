IMPLEMENTATION MODULE VG; (* Ilx 11-Jun-89. (c) KRONOS *)

IMPORT SYSTEM, VF, defCodes, Strings, clc: Time;

IMPORT StdIO;

TYPE ADDRESS=SYSTEM.ADDRESS;
        WORD=SYSTEM.WORD;

    CLP_PARM =
          RECORD
            xa,ya,xb,yb: INTEGER;
          END;
     Palette = ARRAY [0..15] OF RECORD red,green,blue: INTEGER; END;

  ARC_PARM = RECORD
               X,Y,x0,y0,x1,y1,r: INTEGER;
               x,y,co: INTEGER;
               xy0: INTEGER;  xx0: INTEGER;
               xy1: INTEGER;  xx1: INTEGER;
               yx0: INTEGER;  yy0: INTEGER;
               yx1: INTEGER;  yy1: INTEGER;
               case: BOOLEAN;
             END;

  CRC_CONTEXT =
  RECORD
    x,y,co: INTEGER;
  END;

  CRF_CONTEXT =
        RECORD
          x,y,co,xn,yn: INTEGER;
          do: BOOLEAN;
        END;
  TRF_CONTEXT =
        RECORD
          x,y,co,xn,yn,Dx,Dy,dx,dy,xl: INTEGER;
          Gx,case: BOOLEAN;
        END;

VAR palette : Palette;
    p_trans : ARRAY [0..15] OF INTEGER;

VAR shift : POINTER TO INTEGER;
    palet_ptr : POINTER TO ARRAY [0..15] OF INTEGER;
    back  : POINTER TO BITSET;
    not_back: BOOLEAN;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE defCodes.move END move;

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
CODE defCodes.bbp END bbp;

PROCEDURE bbu(to: ADDRESS; ofs: INTEGER; size: INTEGER): BITSET;
CODE defCodes.bbu END bbu;

PROCEDURE dch(m: INTEGER; VAR bmd: BMD; x,y: INTEGER; f: VF.Font; ch: CHAR);
CODE  0F9h 003h END dch;

PROCEDURE ddt(m: INTEGER; VAR bmd: BMD; x,y: INTEGER);
CODE 1 0F9h 001h END ddt;

PROCEDURE dvl(m: INTEGER; VAR bmd: BMD; x,y,len: INTEGER);
CODE 0F9h 001h END dvl;

PROCEDURE bit_move(mode: INTEGER;
                     to: ADDRESS;   to_ofs: INTEGER;
                   from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
BEGIN bitmove(mode,to,to_ofs,from,from_ofs,nobits) END bit_move;

PROCEDURE crc(mode: INTEGER; VAR bmd: BMD; VAR context: CRC_CONTEXT;
              X,Y: INTEGER);
CODE 0F9h 006h END crc;

PROCEDURE ARC(mode:INTEGER; VAR bmd: BMD; VAR arc_parm: ARC_PARM);
CODE 0F9h 007h END ARC;

PROCEDURE crf(VAR crf_context: CRF_CONTEXT);
CODE 0F9h 009h END crf;

PROCEDURE trf(VAR context: TRF_CONTEXT);
CODE 0F9h 008h END trf;

VAR bump: ARRAY [0..63] OF BITSET; (* 2048 pixels *)
    lnFF: ARRAY [0..63] OF BITSET; (* 2048 111111 *)
    ln00: ARRAY [0..63] OF BITSET; (* 2048 000000 *)

     fnt: VF.Font;

PROCEDURE build_bmd(VAR bmd: BMD; w,h: INTEGER; adr: ADDRESS);
BEGIN
  ASSERT((h>0) & (w>0),4Ah);
  bmd.w:=w; bmd.h:=h; bmd.wpl:=16; bmd.base:=adr
END build_bmd;

PROCEDURE bmd_size(w,h: INTEGER): INTEGER; (* no of words needed for WxH *)
BEGIN RETURN (w+31) DIV 32 * h END bmd_size;

PROCEDURE mode(VAR bmd: BMD; m: INTEGER);
BEGIN
  bmd.mode:=INTEGER(BITSET(bmd.mode)*{2} + BITSET(m MOD 4));
END mode;

PROCEDURE erase(VAR bmd: BMD; SEQ pat: WORD);
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
END erase;

PROCEDURE dot(VAR bmd: BMD; x,y: INTEGER);
BEGIN ddt(bmd.mode MOD 4,bmd,x,bmd.h-1-y) END dot;

PROCEDURE vline(VAR bmd: BMD; x,y,y1: INTEGER);
  VAR i: INTEGER;
BEGIN
  y:=bmd.h-1-y; y1:=bmd.h-1-y1;
  IF x < 0     THEN RETURN END;
  IF x >=bmd.w THEN RETURN END;
  IF y = y1    THEN ddt(bmd.mode MOD 4,bmd,x,y); RETURN END;
  IF y > y1    THEN i:=y1; y1:=y; y:=i END;  (* swap: y<y1 *)
  IF y < 0     THEN y :=0       END; (* clip *)
  IF y1>=bmd.h THEN y1:=bmd.h-1 END; (* clip *)
  i:=y1-y+1;
  IF i<=0 THEN RETURN END; (* out of screen *)
  dvl(bmd.mode MOD 4,bmd,x,y,i);
END vline;

PROCEDURE hline(VAR bmd: BMD; x,y,x1: INTEGER);
  VAR d: INTEGER;
BEGIN
  y:=bmd.h-1-y;
  IF y < 0     THEN RETURN END;
  IF y >=bmd.h THEN RETURN END;
  IF x = x1    THEN ddt(bmd.mode MOD 4,bmd,x,y); RETURN END;
  IF x > x1    THEN d:=x1; x1:=x; x:=d END;  (* swap: x<x1 *)
  IF x < 0     THEN x :=0       END; (* clip *)
  IF x1>=bmd.w THEN x1:=bmd.w-1 END; (* clip *)
  d:=x1-x+1;
  IF d<=0 THEN RETURN END; (* out of screen *)
  bitmove(bmd.mode MOD 4,bmd.base+y*bmd.wpl,x,SYSTEM.ADR(lnFF),x,d);
END hline;

PROCEDURE clp(VAR clp_parm: CLP_PARM; w,h:INTEGER): BOOLEAN;
CODE 0F9h 004h END clp;

PROCEDURE dln(mode: INTEGER; VAR bmd: BMD; x,y,x1,y1: INTEGER);
CODE 0F9h 005h END dln;

PROCEDURE line (VAR bmd: BMD; x,y,x1,y1: INTEGER);

  PROCEDURE draw(x,y,x1,y1: INTEGER);
  BEGIN dln(bmd.mode MOD 4,bmd,x,y,x1,y1) END draw;

  PROCEDURE clip(x0,y0,x1,y1,w,h: INTEGER);
    VAR clp_parm: CLP_PARM;
    VAR t: INTEGER;
  BEGIN
    WITH clp_parm DO xa:=x0; xb:=x1; ya:=y0; yb:=y1;
      WITH bmd DO
        IF NOT clp(clp_parm,w,h) THEN RETURN END;
      END;
      draw(xa,ya,xb,yb);
    END;
  END clip;

  VAR i: INTEGER;
    w,h: INTEGER;
BEGIN
  h:=bmd.h;     w :=bmd.w;
  IF x=x1 THEN vline(bmd,x,y,y1); RETURN END;
  IF y=y1 THEN hline(bmd,x,y,x1); RETURN END;
  y:=h-1-y;     y1:=h-1-y1;
  IF x1<x THEN i:=x1; x1:=x; x:=i;  i:=y1; y1:=y; y:=i END;
  IF in_rect(x,y,w,h) & in_rect(x1,y1,w,h) THEN
    draw(x,y,x1,y1);
  ELSE
    clip(x,y,x1,y1,w,h);
  END;
END line;

PROCEDURE patt_line (VAR bmd: BMD; x,y,x1,y1: INTEGER);

  PROCEDURE draw(x,y,x1,y1: INTEGER);
  BEGIN dln(bmd.mode MOD 4,bmd,x,y,x1,y1) END draw;

  PROCEDURE clip(x0,y0,x1,y1,w,h: INTEGER);
    VAR clp_parm: CLP_PARM;
    VAR t: INTEGER;
  BEGIN
    WITH clp_parm DO xa:=x0; xb:=x1; ya:=y0; yb:=y1;
      WITH bmd DO
        IF NOT clp(clp_parm,w,h) THEN RETURN END;
      END;
      draw(xa,ya,xb,yb);
    END;
  END clip;

  VAR i: INTEGER;
    w,h: INTEGER;
BEGIN
  h:=bmd.h;     w :=bmd.w;
  y:=h-1-y;     y1:=h-1-y1;
  IF x1<x THEN i:=x1; x1:=x; x:=i;  i:=y1; y1:=y; y:=i END;
  IF in_rect(x,y,w,h) & in_rect(x1,y1,w,h) THEN
    draw(x,y,x1,y1);
  ELSE
    clip(x,y,x1,y1,w,h);
  END;
END patt_line;

PROCEDURE rect(VAR bmd: BMD; x0,y0,x1,y1: INTEGER);

  PROCEDURE poly1(x0,y0,x1,y1: INTEGER);
    VAR adr: ADDRESS;
        end: ADDRESS;
     n,x,sz: INTEGER;
  BEGIN sz:=x1-x0+1; x:=x0 MOD 32;
    WITH bmd DO
      n:=wpl; adr:=base+(y0*n);
      end:=adr+(y1-y0+1)*n;
    END;
    REPEAT
      bitmove(bmd.mode MOD 4,adr,x0,SYSTEM.ADR(lnFF),x,sz); adr:=adr+n
    UNTIL adr>=end;
  END poly1;

  VAR   t: INTEGER;
BEGIN
  WITH bmd DO
    IF (x0<0) & (x1<0) OR (x0>=w) & (x1>=w) THEN RETURN END;
    IF (y0<0) & (y1<0) OR (y0>=h) & (y1>=h) THEN RETURN END;
    IF x0>x1 THEN t:=x0; x0:=x1; x1:=t END;
    IF y0<y1 THEN t:=y0; y0:=y1; y1:=t END;
    IF x0< 0 THEN x0:=0   END;
    IF x1>=w THEN x1:=w-1 END;
    IF y0>=h THEN y0:=h-1 END;
    IF y1< 0 THEN y1:=0   END;
    y0:=h-1-y0;
    y1:=h-1-y1;
    poly1(x0,y0,x1,y1);
  END;
END rect;

PROCEDURE frame(VAR bmd: BMD; x,y,x1,y1: INTEGER);
BEGIN
  hline(bmd,x,y ,x1);
  hline(bmd,x,y1,x1);
  IF y<y1 THEN
    vline(bmd,x ,y+1,y1-1);
    vline(bmd,x1,y+1,y1-1);
  ELSE
    vline(bmd,x ,y1+1,y-1);
    vline(bmd,x1,y1+1,y-1);
  END;
END frame;


PROCEDURE circ(VAR bmd: BMD; X,Y: INTEGER; r: INTEGER);
  VAR context: CRC_CONTEXT;
BEGIN
  IF (X+r<0) OR (Y+r<0) OR (X-r>=bmd.w) OR (Y-r>=bmd.h) THEN RETURN END;
  Y:=bmd.h-1-Y;
  IF r<1 THEN ddt(bmd.mode MOD 4,bmd,X,Y); RETURN END;
  WITH context DO
    x:=r; y:=0; co:=r DIV 2;
    REPEAT crc(bmd.mode MOD 4,bmd,context,X,Y) UNTIL y>x;
  END;
END circ;

PROCEDURE arc(VAR bmd: BMD; X0,Y0,xa,ya,xb,yb,R: INTEGER);
  VAR arc_parm: ARC_PARM;
BEGIN
  IF (X0+R<0) OR (Y0+R<0) OR (X0-R>=bmd.w) OR (Y0-R>=bmd.h) THEN RETURN END;
  Y0:=bmd.h-1-Y0;
  ya:=bmd.h-1-ya;
  yb:=bmd.h-1-yb;
  IF R<1 THEN ddt(bmd.mode MOD 4,bmd,X0,Y0) END;
  WITH arc_parm DO
    x0:=xa-X0; x1:=xb-X0; X:=X0;
    y0:=ya-Y0; y1:=yb-Y0; Y:=Y0;
    x:=R; y:=0; co:=R DIV 2; r:=R;
    xy0:=x*y0;    xx0:=x*x0;      yx0:=0;       yy0:=0;
    xy1:=x*y1;    xx1:=x*x1;      yx1:=0;       yy1:=0;
    case:=(y1*x0>=x1*y0);
    REPEAT      ARC(bmd.mode MOD 4,bmd,arc_parm) UNTIL y>x;
  END;
END arc;

PROCEDURE circf(VAR bmd: BMD; X,Y,r: INTEGER);
  VAR crf_context: CRF_CONTEXT;
BEGIN
  IF ((X+r)<0) OR ((Y+r)<0) OR ((X-r)>=bmd.w) OR ((Y-r)>=bmd.h) THEN RETURN END;
  WITH crf_context DO
    x:=r; y:=0; co:=r DIV 2;
    hline(bmd,X-x,Y+y,X+x);
    LOOP
      crf(crf_context);

      hline(bmd,X-x,Y+y,X+x);
      hline(bmd,X-x,Y-y,X+x);
      IF do THEN
        IF (xn#y)&(yn#x) THEN
          hline(bmd,X-yn,Y+xn,X+yn);
          hline(bmd,X-yn,Y-xn,X+yn);
        END;
      END;
      IF y>=x THEN EXIT END;
    END;
  END;
END circf;

PROCEDURE trif(VAR bmd: BMD; x0,y0,x1,y1,x2,y2: INTEGER);
  VAR con1,con2: TRF_CONTEXT;
      by,bx,yl: INTEGER;
BEGIN
  IF y1<y0 THEN
    by:=y0; bx:=x0; y0:=y1; x0:=x1; y1:=by; x1:=bx;
  END;
  IF y2<y0 THEN
    by:=y0; bx:=x0; y0:=y2; x0:=x2; y2:=by; x2:=bx;
  END;
  IF (x1-x0)*(y2-y0)>(x2-x0)*(y1-y0) THEN
    by:=y2; bx:=x2; y2:=y1; x2:=x1; y1:=by; x1:=bx;
  END;
  IF y1<y2 THEN yl:=y1 ELSE yl:=y2 END;
  WITH con1 DO
    x:=x0; y:=y0; xn:=x; yn:=y; xl:=x1;
    Dx:=x1-x0; Dy:=y1-y0;
    IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=TRUE ELSE dx:=1; case:=FALSE END;
    IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
    IF Dx>=Dy THEN
      Gx:=TRUE;  co:=Dx DIV 2
    ELSE
      Gx:=FALSE; co:=Dy DIV 2
    END;
  END;
  WITH con2 DO
    x:=x0; y:=y0; xn:=x; yn:=y; xl:=x2;
    Dx:=x2-x0; Dy:=y2-y0;
    IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=FALSE ELSE dx:=1; case:=TRUE END;
    IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
    IF Dx>=Dy THEN
      Gx:=TRUE;  co:=Dx DIV 2
    ELSE
      Gx:=FALSE; co:=Dy DIV 2
    END;
  END;
  LOOP
    IF con1.dx<0 THEN trf(con1); END;
    IF con2.dx>0 THEN trf(con2); END;
    hline(bmd,con1.xn,con1.yn,con2.xn);
    IF con1.yn=yl THEN EXIT END;
    IF con1.dx>0 THEN trf(con1); END;
    IF con2.dx<0 THEN trf(con2); END;
  END;
  IF y1=y2 THEN RETURN END;
  IF yl=y1 THEN
    yl:=y2;
    WITH con1 DO
      x:=x1; y:=y1; xn:=x; yn:=y; xl:=x2;
      Dx:=x2-x1; Dy:=y2-y1;
      IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=TRUE ELSE dx:=1; case:=FALSE END;
      IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
      IF Dx>=Dy THEN
        Gx:=TRUE;  co:=Dx DIV 2
      ELSE
        Gx:=FALSE; co:=Dy DIV 2
      END;
      IF dx<0 THEN trf(con1) END;
    END;
  ELSE
    yl:=y1;
    WITH con2 DO
      x:=x2; y:=y2; xn:=x; yn:=y; xl:=x1;
      Dx:=x1-x2; Dy:=y1-y2;
      IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=FALSE ELSE dx:=1; case:=TRUE END;
      IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
      IF Dx>=Dy THEN
        Gx:=TRUE;  co:=Dx DIV 2
      ELSE
        Gx:=FALSE; co:=Dy DIV 2
      END;
      IF dx>0 THEN trf(con2) END;
    END;
  END;
  REPEAT
    trf(con1); trf(con2);
      hline(bmd,con1.xn,con1.yn,con2.xn);
  UNTIL con1.yn=yl;
END trif;

----------------------------  Text  ----------------------------
                            --------

PROCEDURE font(f: VF.Font);
BEGIN
  fnt:=f; char_w:=f^.w; char_h:=f^.h
END font;

PROCEDURE inverse(VAR bmd: BMD; OnOff: INTEGER);
BEGIN
  bmd.mode:=INTEGER(BITSET(bmd.mode)-{2}+BITSET(OnOff)*{2});
END inverse;

-----------     For Proportional Mode    ---------------

PROCEDURE write(VAR bmd: BMD; x,y: INTEGER; ch: CHAR);
  VAR old_w: INTEGER;
BEGIN
  old_w:=fnt^.w; fnt^.w:=fnt^.p_w^[ch];
  y:=bmd.h-1-(y+char_h-1);
  IF NOT in_rect(x,y,bmd.w-fnt^.w,bmd.h-char_h) THEN RETURN END;
  dch(bmd.mode,bmd,x,y,fnt,ch); fnt^.w:=old_w;
END write;

PROCEDURE write_str(VAR bmd: BMD; x,y: INTEGER; VAL str: ARRAY OF CHAR);
  VAR i,w,h: INTEGER;
      old_w: INTEGER;
         ch: CHAR;
BEGIN
  old_w:=fnt^.w;
  y:=bmd.h-1-(y+char_h-1); w:=bmd.w-char_w; h:=bmd.h-char_h;
  IF NOT in_range0(y,h) THEN RETURN END;
  (*$T-*)
  i:=0; ch:=str[i];
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO
    fnt^.w:=fnt^.p_w^[ch];
    IF in_rect(x,y,w,h) THEN dch(bmd.mode,bmd,x,y,fnt,ch) END;
    x:=x+fnt^.w; i:=i+1; ch:=str[i];
  END;
  (*$T+*)
  fnt^.w:=old_w;
END write_str;

PROCEDURE clip_write_str(VAR bmd: BMD; W,E,S,N: INTEGER;
                         x,y: INTEGER; VAL str: ARRAY OF CHAR);
  VAR i  : INTEGER;  old_w: INTEGER;
      w,h: INTEGER;  ch: CHAR;
BEGIN
  y:=bmd.h-1-(y+char_h-1); w:=E-W-char_w; h:=N-S-char_h;
                         --w:=bmd.w-char_w-(W+E); h:=bmd.h-char_h-(S+N);
  IF NOT in_range0(y,h) THEN RETURN END;
  old_w:=fnt^.w;
  (*$T-*)
  i:=0; ch:=str[i];
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO
    fnt^.w:=fnt^.p_w^[ch];
    IF in_rect(x-W,y-S,w,h) THEN dch(bmd.mode,bmd,x,y,fnt,ch) END;
    x:=x+fnt^.w; i:=i+1; ch:=str[i];
  END;
  (*$T+*)
  fnt^.w:=old_w;
END clip_write_str;

(*
-------        FOR NONPROPORTIONAL MODE        -------------

PROCEDURE write(VAR bmd: BMD; x,y: INTEGER; ch: CHAR);
BEGIN y:=bmd.h-1-(y+char_h-1);
  IF NOT in_rect(x,y,bmd.w-char_w,bmd.h-char_h) THEN RETURN END;
  dch(bmd.mode,bmd,x,y,fnt,ch);
END write;

PROCEDURE write_str(VAR bmd: BMD; x,y: INTEGER; VAL str: ARRAY OF CHAR);
  VAR i  : INTEGER;
      w,h: INTEGER;  ch: CHAR;
BEGIN
  y:=bmd.h-1-(y+char_h-1);  w:=bmd.w-char_w;  h:=bmd.h-char_h;
  IF NOT in_range0(y,h) THEN RETURN END;
  (*$T-*)
  i:=0; ch:=str[i];
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO
    IF in_rect(x,y,w,h) THEN dch(bmd.mode,bmd,x,y,fnt,ch) END;
    x:=x+char_w; i:=i+1; ch:=str[i];
  END;
  (*$T+*)
END write_str;

PROCEDURE clip_write_str(VAR bmd: BMD; W,E,S,N: INTEGER;
                         x,y: INTEGER; VAL str: ARRAY OF CHAR);
  VAR i  : INTEGER;
      w,h: INTEGER;  ch: CHAR;
BEGIN
  y:=bmd.h-1-(y+char_h-1); w:=E-W-char_w; h:=N-S-char_h;
                         --w:=bmd.w-char_w-(W+E); h:=bmd.h-char_h-(S+N);
  IF NOT in_range0(y,h) THEN RETURN END;
  (*$T-*)
  i:=0; ch:=str[i];
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO
    IF in_rect(x-W,y-S,w,h) THEN dch(bmd.mode,bmd,x,y,fnt,ch) END;
    x:=x+char_w; i:=i+1; ch:=str[i];
  END;
  (*$T+*)
END clip_write_str;
-----------------------------------------------------------------------
*)

PROCEDURE print(VAR bmd: BMD; x,y: INTEGER;
                VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
  VAR str: ARRAY [0..255] OF CHAR;
BEGIN
  Strings.print(str,fmt,args); write_str(bmd,x,y,str);
END print;

PROCEDURE clip_print(VAR bmd: BMD; W,E,S,N: INTEGER;
                     x,y   : INTEGER;
                     VAL fmt: ARRAY OF CHAR;
                     SEQ args  : WORD);
  VAR str: ARRAY [0..255] OF CHAR;
BEGIN
  Strings.print(str,fmt,args); clip_write_str(bmd,W,E,S,N,x,y,str);
END clip_print;

PROCEDURE paint(wait: BOOLEAN);
  VAR m: BITSET;
  r,g,b: INTEGER;
      i: INTEGER;
      p: ARRAY [0..15] OF INTEGER;

BEGIN
  FOR i:=0 TO HIGH(palette) DO
    WITH palette[i] DO
      r:=INTEGER( BITSET(red  )*{0..3} );
      g:=INTEGER( BITSET(green)*{0..3} );
      b:=INTEGER( BITSET(blue )*{0..3} );
      r:=p_trans[r];
      g:=p_trans[g];
      b:=p_trans[b];
    END;
    p[i]:=r+g*16+b*256;
  END;
  IF wait & NOT not_back THEN
    REPEAT  UNTIL back^*{0}={};
    REPEAT  UNTIL back^*{0}#{};
  END;
  move(palet_ptr,SYSTEM.ADR(p),SIZE(palet_ptr^));
END paint;

PROCEDURE color(no: INTEGER; r,g,b: INTEGER);
BEGIN
  WITH palette[no] DO red:=r; green:=g; blue:=b END
END color;

PROCEDURE screen(on: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  IF on THEN paint(TRUE)
  ELSE FOR i:=0 TO HIGH(palet_ptr^) DO palet_ptr^[i]:=0FFFh END;
  END;
END screen;

PROCEDURE init_palette;
BEGIN
  p_trans[ 0]:=0Fh; p_trans[ 1]:=07h;
  p_trans[ 2]:=0Bh; p_trans[ 3]:=03h;
  p_trans[ 4]:=0Dh; p_trans[ 5]:=05h;
  p_trans[ 6]:=09h; p_trans[ 7]:=01h;
  p_trans[ 8]:=0Eh; p_trans[ 9]:=06h;
  p_trans[10]:=0Ah; p_trans[11]:=02h;
  p_trans[12]:=0Ch; p_trans[13]:=04h;
  p_trans[14]:=08h; p_trans[15]:=00h;
          (* R  G  B *)
  color(00h, 0, 0, 0  );   -- черный
  color(01h,14, 0, 0  );   -- красный
  color(02h,13, 6, 0  );   --
  color(03h,12, 7, 0  );   -- оранжевый
  color(04h,11, 8, 0  );   --
  color(05h,11,10, 0  );   -- желтый
  color(06h, 3,10, 0  );   --
  color(07h, 0,14, 0  );   -- зеленый
  color(08h, 0,10, 7  );   --
  color(09h, 0, 7,10  );   -- голубой
  color(0Ah, 0, 4,11  );   --
  color(0Bh, 0, 0,14  );   -- синий
  color(0Ch, 7, 0,10  );   --
  color(0Dh, 9, 7, 9  );   -- фиолетовый
  color(0Eh, 8, 0, 8  );   --
  color(0Fh, 9, 9, 9  );   -- белый
  paint(TRUE);
END init_palette;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  i:=clc.sys_time(clc.milisec);
  REPEAT  UNTIL (back^*{0}={}) OR (clc.sys_time(clc.milisec)-i > 1000);
  IF clc.sys_time(clc.milisec)-i > 1000 THEN
    not_back:=TRUE
  ELSE
    REPEAT  UNTIL (back^*{0}#{}) OR (clc.sys_time(clc.milisec)-i > 1000);
    IF clc.sys_time(clc.milisec)-i > 1000 THEN
      not_back:=TRUE
    ELSE
      not_back:=FALSE
    END;
  END;
  init_palette;
  build_bmd(layer[0],480,360,ADDRESS(1F8000h));
  build_bmd(layer[1],480,360,ADDRESS(1FA000h));
  build_bmd(layer[2],480,360,ADDRESS(1FC000h));
  build_bmd(layer[3],480,360,ADDRESS(1FE000h));
  FOR i:=0 TO 3 DO
    layer[i].wpl:=16;  layer[i].patt:={0..31};
    layer[i].mode:=rep;
  END;
  font(VF.main);
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={     } END;
  FOR i:=0 TO HIGH(lnFF) DO lnFF[i]:={0..31} END;
  shift^  :=01FFh*200h*400h;
END init;

BEGIN
  shift    :=ADDRESS(1F0000h);
  back     :=ADDRESS(1F0020h);
  palet_ptr:=ADDRESS(1F0010h);
  init;
END VG.
