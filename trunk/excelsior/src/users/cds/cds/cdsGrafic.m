IMPLEMENTATION MODULE cdsGrafic; (*$N+ Sem 14-Jan-91. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM libWindows IMPORT  window;

IMPORT  mcd : defCodes;
IMPORT  wnd : libWindows;
IMPORT  str : Strings;
IMPORT  mem : libHeap;
IMPORT  bio : libBIO;
IMPORT  tty : Terminal;

TYPE
  CLP_PARM = RECORD xa,ya,xb,yb: INTEGER; END;
  bpt  = ARRAY [0..7] OF BITSET;
  font_rec = RECORD
    w,h : INTEGER;
    base: ADDRESS;
    p_w : POINTER TO ARRAY CHAR OF INTEGER;
  END;
  font = POINTER TO font_rec;
  ARC_PARM = RECORD
    X,Y,x0,y0,x1,y1,r: INTEGER;
    x,y,co: INTEGER;
    xy0 : INTEGER;  xx0: INTEGER;
    xy1 : INTEGER;  xx1: INTEGER;
    yx0 : INTEGER;  yy0: INTEGER;
    yx1 : INTEGER;  yy1: INTEGER;
    case: BOOLEAN;
  END;
  CRC_CONTEXT = RECORD
    x,y,co: INTEGER;
  END;

CONST
  Colors = 16;
  max    = 4;
  lsno   = 4;

VAR
  ln00  : ARRAY [0..63] OF BITSET; (* 2048 000000 *)
  lnFF  : ARRAY [0..63] OF BITSET; (* 2048 000000 *)
  xsz   : ARRAY [0..max] OF INTEGER;
  ysz   : ARRAY [0..max] OF INTEGER;
  pat   : ARRAY [0..max] OF bpt;
  main_c: font;
  main_p: font;
  signs : font;

PROCEDURE MOVE(to,from: ADDRESS; n: INTEGER); CODE mcd.move END MOVE;
PROCEDURE bblt(t: ADDRESS; ot: INTEGER;
               f: ADDRESS; of,s: INTEGER);    CODE mcd.bblt END bblt;
PROCEDURE bitmove(mode: INTEGER;
                    to: ADDRESS;   to_ofs: INTEGER;
                  from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bitmove;
PROCEDURE dln(mode: INTEGER; bmd: window; x,y,x1,y1: INTEGER);
CODE 0F9h 005h END dln;
PROCEDURE clp(VAR clp_parm: CLP_PARM; w,h:INTEGER): BOOLEAN;
CODE 0F9h 004h END clp;
PROCEDURE ddt(m: INTEGER; bmd: window; x,y: INTEGER);
CODE 1 0F9h 001h END ddt;
PROCEDURE dvl(m: INTEGER; bmd: window; x,y,len: INTEGER);
CODE 0F9h 001h END dvl;
PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
CODE 0F9h 00h END in_rect;
PROCEDURE dch(m: INTEGER; bmd: window; x,y: INTEGER; f: font; ch: CHAR);
CODE  0F9h 003h END dch;
PROCEDURE in_range0(i,hi: INTEGER): BOOLEAN;
CODE 0F6h END in_range0;
PROCEDURE crc(mode: INTEGER; bmd: window; VAR context: CRC_CONTEXT;
              X,Y: INTEGER);
CODE 0F9h 006h END crc;
PROCEDURE ARC(mode:INTEGER; bmd: window; VAR arc_parm: ARC_PARM);
CODE 0F9h 007h END ARC;

PROCEDURE color(w: window; i: INTEGER);
BEGIN
  w^.color:=i MOD Colors;
END color;

PROCEDURE mode(w: window; m: modes);
BEGIN
  w^.mode:=INTEGER(BITSET(w^.mode)-{0..1}+BITSET(INTEGER(m) MOD 4));
END mode;

PROCEDURE inverse(w: window; m: inverse_modes);
BEGIN
  IF m=on THEN
    w^.mode:=INTEGER(BITSET(w^.mode)+{2});
  ELSE
    w^.mode:=INTEGER(BITSET(w^.mode)-{2});
  END;
END inverse;

PROCEDURE prop_font(w: window; on: BOOLEAN);
BEGIN
  IF on THEN
    w^.mode:=INTEGER(BITSET(w^.mode)+{3});
  ELSE
    w^.mode:=INTEGER(BITSET(w^.mode)-{3});
  END;
END prop_font;

PROCEDURE clip(W,E,S,N: INTEGER; VAR x0,y0,x1,y1: INTEGER): BOOLEAN;
  VAR cp: CLP_PARM;
BEGIN
  WITH cp DO
    xa:=x0-W; xb:=x1-W; ya:=y0-S; yb:=y1-S;
    IF clp(cp,E-W,N-S) THEN
      x0:=xa+W; x1:=xb+W; y0:=ya+S; y1:=yb+S; RETURN TRUE
    END;
    RETURN FALSE;
  END;
END clip;

PROCEDURE vline(w: window; x,y,y1: INTEGER);
  VAR d,i: INTEGER;
BEGIN
  IF x<w^.W THEN RETURN END;
  IF x>w^.E THEN RETURN END;
  IF y>y1 THEN i:=y1; y1:=y; y:=i END;
  IF y<w^.S THEN y:=w^.S END;
  IF y1>w^.N THEN y1:=w^.N END;
  d:=y1-y+1;
  IF d<=0 THEN RETURN END;
  y:=w^.sy-1-y1;
  w^.base:=w^.mem;
  FOR i:=0 TO lsno-1 DO
    IF i IN BITSET(w^.color) THEN dvl(w^.mode MOD 4,w,x,y,d) END;
    INC(w^.base,w^.wpp);
  END;
END vline;

PROCEDURE hline(w: window; x,y,x1: INTEGER);
  VAR d,i: INTEGER; a: ADDRESS;
BEGIN
  IF y<w^.S THEN RETURN END;
  IF y>w^.N THEN RETURN END;
  IF x>x1 THEN d:=x1; x1:=x; x:=d END;
  IF x<w^.W THEN x:=w^.W END;
  IF x1>w^.E THEN x1:=w^.E END;
  d:=x1-x+1;
  IF d<=0 THEN RETURN END;
  a:=w^.mem+(w^.sy-1-y)*w^.wpl;
  FOR i:=0 TO lsno-1 DO
    IF i IN BITSET(w^.color) THEN
      bitmove(w^.mode MOD 4,a,x,ADR(lnFF),x,d);
    END;
    INC(a,w^.wpp);
  END;
END hline;

PROCEDURE vect(w: window; x,y,x1,y1: INTEGER);
  VAR i: INTEGER; cp: CLP_PARM; p: BITSET;
BEGIN
  WITH cp DO
    IF (x=x1) & (w^.patt={0..31})  THEN vline(w,x,y,y1); RETURN END;
    IF (y=y1) & (w^.patt={0..31})  THEN hline(w,x,y,x1); RETURN END;
    IF x1<x THEN
      xa:=x1-w^.W; xb:=x-w^.W; ya:=y1-w^.S; yb:=y-w^.S;
    ELSE
      xa:=x-w^.W; xb:=x1-w^.W; ya:=y-w^.S; yb:=y1-w^.S;
    END;
    IF NOT clp(cp,w^.E-w^.W,w^.N-w^.S) THEN RETURN END;
    y:=w^.sy-1-ya-w^.S; y1:=w^.sy-1-yb-w^.S;
    x:=xa+w^.W; x1:=xb+w^.W;
    w^.base:=w^.mem;
    p:=w^.patt;
    FOR i:=0 TO lsno-1 DO
      IF i IN BITSET(w^.color) THEN
        dln(w^.mode MOD 4,w,x,y,x1,y1); w^.patt:=p;
      END;
      INC(w^.base,w^.wpp);
    END;
  END;
END vect;

PROCEDURE frame(w: window; x0,y0,x1,y1: INTEGER);
BEGIN
  IF ABS(x0-x1)>=2 THEN
    vect(w,x1-1,y1,x0+1,y1);
    IF y0#y1 THEN vect(w,x0+1,y0,x1-1,y0) END;
  END;
  vect(w,x1,y0,x1,y1);
  IF x0#x1 THEN vect(w,x0  ,y1,x0  ,y0) END;
END frame;

PROCEDURE box(w: window; x0,y0,x1,y1: INTEGER);
  PROCEDURE poly1(x0,y0,x1,y1: INTEGER);
    VAR
      adr,a : ADDRESS;
      end   : ADDRESS;
      n,x,sz: INTEGER;
      i     : INTEGER;
  BEGIN
    sz:=x1-x0+1;
    x:=x0 MOD 32;
    n:=w^.wpl;
    adr:=w^.mem+(w^.sy-1-y1)*n;
    end:=w^.mem+(w^.sy-1-y0)*n;
    FOR i:=0 TO lsno-1 DO
      IF i IN BITSET(w^.color) THEN
        a:=adr;
        REPEAT
          bitmove(w^.mode MOD 4,a,x0,ADR(lnFF),x,sz); a:=a+n;
        UNTIL a>end;
      END;
      INC(adr,w^.wpp); INC(end,w^.wpp);
    END;
  END poly1;
  VAR i: INTEGER;
BEGIN
  IF x1<x0 THEN i:=x1; x1:=x0; x0:=i END;
  IF y1<y0 THEN i:=y1; y1:=y0; y0:=i END;
  WITH w^ DO
    IF x0<W THEN IF x1<W THEN RETURN END; x0:=W; END;
    IF y0<S THEN IF y1<S THEN RETURN END; y0:=S; END;
    IF x1>E THEN IF x0>E THEN RETURN END; x1:=E; END;
    IF y1>N THEN IF y0>N THEN RETURN END; y1:=N; END;
  END;
  poly1(x0,y0,x1,y1);
END box;

PROCEDURE patt_box(w: window; x0,y0,x1,y1,p: INTEGER);
  VAR px,py,i,n,x,l: INTEGER; pp: POINTER TO bpt; a: ADDRESS;
BEGIN
  IF x1<x0 THEN i:=x1; x1:=x0; x0:=i END;
  IF y1<y0 THEN i:=y1; y1:=y0; y0:=i END;
  WITH w^ DO
    IF x0<W THEN IF x1<W THEN RETURN END; x0:=W; END;
    IF y0<S THEN IF y1<S THEN RETURN END; y0:=S; END;
    IF x1>E THEN IF x0>E THEN RETURN END; x1:=E; END;
    IF y1>N THEN IF y0>N THEN RETURN END; y1:=N; END;
    FOR l:=0 TO 3 DO
      IF l IN BITSET(color) THEN
        a:=mem+(sy-y0-1)*wpl+l*wpp;
        pp:=ADR(pat[p]);
        px:=xsz[p]; py:=ysz[p];
        FOR i:=0 TO y1-y0 DO
          n:=x1-x0+1; x:=x0;
          WHILE n>px DO
            bitmove(mode MOD 4,a,x,ADR(pp^[i MOD py]),0,px);
            INC(x,px); DEC(n,px);
          END;
          IF n>0 THEN bitmove(mode MOD 4,a,x,ADR(pp^[i MOD py]),0,n) END;
          DEC(a,wpl);
        END;
      END;
    END;
  END;
END patt_box;

PROCEDURE write_char_clp(w: window; x,y: INTEGER; ch: CHAR; f: font);
  VAR cx,csz,i,j,l: INTEGER; a,b,c: ADDRESS;
BEGIN
  IF x<w^.W THEN cx:=w^.W-x; IF cx>=f^.w THEN RETURN END
  ELSE cx:=0;
  END;
  IF x+f^.w-1>w^.E THEN csz:=w^.E-x-cx+1; IF csz<=0 THEN RETURN END
  ELSE csz:=f^.w-cx;
  END;
  a:=w^.mem+(w^.sy-1-y)*w^.wpl;
  b:=f^.base+f^.h*(ORD(ch)+1)-1;
  l:=f^.h; j:=w^.wpl*w^.sy;
  REPEAT
    IF (y>=w^.S) & (y<=w^.N) THEN
      c:=a;
      FOR i:=0 TO lsno-1 DO
        IF i IN BITSET(w^.color) THEN
          bitmove(w^.mode MOD 4,c,x+cx,b,cx,csz);
        END;
        INC(c,j);
      END;
    END;
    DEC(a,w^.wpl); DEC(l); INC(y); DEC(b);
  UNTIL l=0;
END write_char_clp;

PROCEDURE write_str(w: window; x,y,ps: INTEGER; VAL s: ARRAY OF CHAR; f: font);
  VAR i,j,k,n,m: INTEGER; ch: CHAR; fr: font_rec; cc: BITSET;
BEGIN
  IF y>w^.N THEN RETURN END;
  IF y+f^.h<=w^.S THEN RETURN END;
  fr:=f^; f:=ADR(fr); n:=ps; INC(x);
  IF (y<w^.S) OR (y+f^.h-1>w^.N) THEN
    WHILE s[n]#0c DO
      f^.w:=f^.p_w^[s[n]];
      write_char_clp(w,x,y,s[n],f);
      INC(n); INC(x,f^.w+2);
    END;
  ELSE
    k:=w^.sy-y-f^.h; j:=w^.wpl*w^.sy; cc:=BITSET(w^.color); m:=w^.mode MOD 4;
    WHILE s[n]#0c DO
      ch:=s[n]; f^.w:=f^.p_w^[ch];
      IF (x<w^.W) OR (x+f^.w-1>w^.E) THEN write_char_clp(w,x,y,ch,f);
      ELSE
        w^.base:=w^.mem;
        IF cc*{0}#{} THEN dch(m,w,x,k,f,ch) END; INC(w^.base,j);
        IF cc*{1}#{} THEN dch(m,w,x,k,f,ch) END; INC(w^.base,j);
        IF cc*{2}#{} THEN dch(m,w,x,k,f,ch) END; INC(w^.base,j);
        IF cc*{3}#{} THEN dch(m,w,x,k,f,ch) END;
      END;
      INC(n); INC(x,f^.w+2);
    END;
  END;
END write_str;

PROCEDURE write_char(w: window; x,y: INTEGER; ch: CHAR);
  VAR s: ARRAY [0..3] OF CHAR;
BEGIN
  s[0]:=ch; s[1]:=0c;
  IF 3 IN BITSET(w^.mode) THEN write_str(w,x-1,y,0,s,main_p);
  ELSE write_str(w,x-1,y,0,s,main_c);
  END;
END write_char;

PROCEDURE string_len(prp: BOOLEAN; VAL s: ARRAY OF CHAR; p: INTEGER): INTEGER;
  VAR l,i: INTEGER; f: font;
BEGIN
  IF prp THEN f:=main_p ELSE f:=main_c END;
  l:=0;
  FOR i:=p TO HIGH(s) DO
    IF s[i]=0c THEN RETURN l END;
    l:=l+f^.p_w^[s[i]]+2;
  END;
  RETURN l;
END string_len;

PROCEDURE write_string(w: window; x,y: INTEGER; VAL s: ARRAY OF CHAR);
BEGIN
  IF 3 IN BITSET(w^.mode) THEN write_str(w,x,y,0,s,main_p);
  ELSE write_str(w,x,y,0,s,main_c);
  END;
END write_string;

PROCEDURE write_pos(w: window; x,y: INTEGER; VAL s: ARRAY OF CHAR; p: INTEGER);
BEGIN
  IF 3 IN BITSET(w^.mode) THEN write_str(w,x,y,p,s,main_p);
  ELSE write_str(w,x,y,p,s,main_c);
  END;
END write_pos;

PROCEDURE print(w: window; x,y : INTEGER;
                VAL fmt : ARRAY OF CHAR; SEQ args: WORD);
  VAR buf: ARRAY [0..255] OF CHAR;
BEGIN
  str.print(buf,fmt,args);
  IF 3 IN BITSET(w^.mode) THEN write_str(w,x,y,0,buf,main_p);
  ELSE write_str(w,x,y,0,buf,main_c);
  END;
END print;

PROCEDURE arc_em(w: window; X0,Y0,xa,ya,xb,yb,R: INTEGER);
  VAR or: BOOLEAN;
  PROCEDURE d(x,y: INTEGER);
  BEGIN
    IF or THEN
      IF (xa*y<ya*x) OR (xb*y>yb*x) THEN RETURN END;
    ELSE
      IF (xa*y<ya*x) & (xb*y>yb*x) THEN RETURN END;
    END;
    dot(w,X0+x,Y0+y);
  END d;
  VAR x,y,xl: INTEGER;
BEGIN
  IF R<1 THEN dot(w,X0,Y0); RETURN END;
  xl:=R DIV 2;
  IF (w^.W>X0-xl) & (w^.E<X0+xl) & (w^.S>Y0-xl) & (w^.N<Y0+xl) THEN RETURN END;
  xa:=xa-X0; xb:=xb-X0; ya:=ya-Y0; yb:=yb-Y0;
  or:=yb*xa>xb*ya; x:=R; y:=0;
  REPEAT
    d(x,y); d(y,x);
    IF y#0 THEN d(x,-y); d(-y,x) END;
    IF x#0 THEN
      d(-x,y); d(y,-x);
      IF y#0 THEN d(-x,-y); d(-y,-x) END;
    END;
    xl:=xl+y; y:=y+1;
    IF xl>=x THEN xl:=xl-x; x:=x-1 END;
  UNTIL x<y;
END arc_em;

PROCEDURE arc(w: window; X0,Y0,xa,ya,xb,yb,R: INTEGER);
  VAR i: INTEGER; arc_parm: ARC_PARM;
BEGIN
  IF (X0+R<w^.W) OR (X0-R>w^.E) THEN RETURN END;
  IF (Y0+R<w^.S) OR (Y0-R>w^.N) THEN RETURN END;
  IF (X0-R<w^.W) OR (Y0-R<w^.S) OR (X0+R>w^.E) OR (Y0+R>w^.N) THEN
    arc_em(w,X0,Y0,xa,ya,xb,yb,R);
  ELSE
    w^.base:=w^.mem; Y0:=w^.sy-1-Y0; ya:=w^.sy-1-ya; yb:=w^.sy-1-yb;
    FOR i:=0 TO 3 DO
      IF i IN BITSET(w^.color) THEN
        IF R<1 THEN
          ddt(w^.mode MOD 4,w,X0,Y0)
        ELSE
          WITH arc_parm DO
            x0:=xa-X0; x1:=xb-X0; X:=X0;
            y0:=ya-Y0; y1:=yb-Y0; Y:=Y0;
            x:=R; y:=0; co:=R DIV 2; r:=R;
            xy0:=x*y0; xx0:=x*x0; yx0:=0; yy0:=0;
            xy1:=x*y1; xx1:=x*x1; yx1:=0; yy1:=0;
            case:=(y1*x0>=x1*y0);
            REPEAT ARC(w^.mode MOD 4,w,arc_parm) UNTIL y>x;
          END;
        END;
      END;
      INC(w^.base,w^.wpp);
    END;
  END;
END arc;

PROCEDURE circ(w: window; X,Y: INTEGER; r: INTEGER);
  VAR i: INTEGER; context: CRC_CONTEXT;
BEGIN
  IF (X+r<w^.W) OR (Y+r<w^.S) THEN RETURN END;
  IF (X-r>w^.E) OR (Y-r>w^.N) THEN RETURN END;
  IF (X-r<w^.W) OR (Y-r<w^.S) OR (X+r>w^.E) OR (Y+r>w^.N) THEN
    arc_em(w,X,Y,X+r,Y,X+r,Y,r);
  ELSE
    Y:=w^.sy-1-Y; w^.base:=w^.mem;
    FOR i:=0 TO 3 DO
      IF i IN BITSET(w^.color) THEN
        IF r<1 THEN
          ddt(w^.mode MOD 4,w,X,Y)
        ELSE
          WITH context DO
            x:=r; y:=0; co:=r DIV 2;
            REPEAT crc(w^.mode MOD 4,w,context,X,Y) UNTIL y>x;
          END;
        END;
      END;
      INC(w^.base,w^.wpp);
    END;
  END;
END circ;

PROCEDURE dot(w: window; x,y: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (x<w^.W) OR (x>w^.E) THEN RETURN END;
  IF (y<w^.S) OR (y>w^.N) THEN RETURN END;
  w^.base:=w^.mem; y:=w^.sy-1-y;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN ddt(w^.mode MOD 4,w,x,y) END;
    INC(w^.base,w^.wpp);
  END;
END dot;

VAR Lnpt: ARRAY [0..15] OF INTEGER;

PROCEDURE greed(w: window; Scale,x,y: INTEGER);
  VAR k,l,i,j: INTEGER;
            p: ADDRESS;
BEGIN
  color(w,8); mode(w,rep);
  x:=x MOD 48; y:=y MOD 48;
(*
  k:=(w^.sx-(w^.W+w^.E)) DIV (48 DIV Scale);
  l:=(w^.sy -(w^.N+w^.S)) DIV (48 DIV Scale);
  IF (k < (w^.sx-(w^.W+w^.E)) DIV 2) & (l < (w^.sy-(w^.N+w^.S)) DIV 2) THEN
    b.base:=ADR(Lnpt); vg.w.w:=w^.sx;
    b.h:=1;
    Lnpt[0]:=0;
    MOVE(ADR(Lnpt)+1,ADR(Lnpt),w^.wpl-1);
    vg.mode:=vg.rep;
    FOR i:=0 TO k DO
      vg.dot((i*48-x) DIV Scale,0);
    END;
    i:=(48 DIV Scale)*w^.wpl;
    p:=w^.mem+(w^.sy*3+w^.sx-1)*w^.wpl + y DIV Scale*w^.wpl;
    IF (p-i*l)<(w^.mem+w^.sy*w^.wpl*3) THEN INC(p,i) END;
    FOR j:=0 TO l DO
      vg.bit_move(vg.or,p,0,ADR(Lnpt),0,w^.wpl*32); DEC(p,i)
--    MOVE(p,ADR(Lnpt),15);
    END;
  END;
*)
END greed;

PROCEDURE roll(w: window; dx,dy: INTEGER);
  VAR
    buf: ARRAY [0..31] OF INTEGER;
    f_x,t_x,f_sz,z_x,z_sz,e_x,e_sz: INTEGER;
    wpl,l1,l2,l3,i: INTEGER;
    t_a,f_a,ln0: ADDRESS;
BEGIN
  ASSERT(lsno=4);
  wpl:=w^.wpl; l1:=wpl*w^.sy; l2:=l1+l1; l3:=l2+l1;
  e_sz:=w^.E-w^.W+1; e_x:=w^.W; ln0:=ADR(ln00);
  IF dx>0 THEN
    f_x:=w^.W; f_sz:=w^.E-f_x+1-dx; t_x:=w^.W+dx; z_x:=f_x; z_sz:=dx;
  ELSE
    f_x:=w^.W-dx; f_sz:=w^.E-f_x+1; t_x:=w^.W; z_x:=w^.E+dx+1; z_sz:=-dx;
  END;
  IF dy>0 THEN
    t_a:=w^.mem+(w^.sy-1-w^.N)*wpl; f_a:=t_a+dy*wpl;
    FOR i:=w^.S TO w^.N-dy DO
      bblt(t_a   ,t_x,f_a   ,f_x,f_sz); bblt(t_a   ,z_x,ln0,z_x,z_sz);
      bblt(t_a+l1,t_x,f_a+l1,f_x,f_sz); bblt(t_a+l1,z_x,ln0,z_x,z_sz);
      bblt(t_a+l2,t_x,f_a+l2,f_x,f_sz); bblt(t_a+l2,z_x,ln0,z_x,z_sz);
      bblt(t_a+l3,t_x,f_a+l3,f_x,f_sz); bblt(t_a+l3,z_x,ln0,z_x,z_sz);
      INC(t_a,wpl); INC(f_a,wpl);
    END;
    FOR i:=0 TO dy-1 DO
      bblt(t_a   ,e_x,ln0,e_x,e_sz);
      bblt(t_a+l1,e_x,ln0,e_x,e_sz);
      bblt(t_a+l2,e_x,ln0,e_x,e_sz);
      bblt(t_a+l3,e_x,ln0,e_x,e_sz);
      INC(t_a,wpl)
    END;
  ELSIF dy=0 THEN
    IF dx=0 THEN RETURN END;
    t_a:=w^.mem+(w^.sy-1-w^.N)*wpl; f_a:=ADR(buf);
    FOR i:=w^.S TO w^.N DO
      bblt(f_a,f_x,t_a   ,f_x,f_sz); bblt(t_a   ,t_x,f_a,f_x,f_sz);
      bblt(f_a,f_x,t_a+l1,f_x,f_sz); bblt(t_a+l1,t_x,f_a,f_x,f_sz);
      bblt(f_a,f_x,t_a+l2,f_x,f_sz); bblt(t_a+l2,t_x,f_a,f_x,f_sz);
      bblt(f_a,f_x,t_a+l3,f_x,f_sz); bblt(t_a+l3,t_x,f_a,f_x,f_sz);
      bblt(t_a   ,z_x,ln0,z_x,z_sz);
      bblt(t_a+l1,z_x,ln0,z_x,z_sz);
      bblt(t_a+l2,z_x,ln0,z_x,z_sz);
      bblt(t_a+l3,z_x,ln0,z_x,z_sz);
      INC(t_a,wpl);
    END;
  ELSE
    t_a:=w^.mem+(w^.sy-1-w^.S)*wpl; f_a:=t_a+dy*wpl;
    FOR i:=w^.S TO w^.N+dy DO
      bblt(t_a   ,t_x,f_a   ,f_x,f_sz); bblt(t_a   ,z_x,ln0,z_x,z_sz);
      bblt(t_a+l1,t_x,f_a+l1,f_x,f_sz); bblt(t_a+l1,z_x,ln0,z_x,z_sz);
      bblt(t_a+l2,t_x,f_a+l2,f_x,f_sz); bblt(t_a+l2,z_x,ln0,z_x,z_sz);
      bblt(t_a+l3,t_x,f_a+l3,f_x,f_sz); bblt(t_a+l3,z_x,ln0,z_x,z_sz);
      DEC(t_a,wpl); DEC(f_a,wpl);
    END;
    FOR i:=0 TO -dy-1 DO
      bblt(t_a   ,e_x,ln0,e_x,e_sz);
      bblt(t_a+l1,e_x,ln0,e_x,e_sz);
      bblt(t_a+l2,e_x,ln0,e_x,e_sz);
      bblt(t_a+l3,e_x,ln0,e_x,e_sz);
      DEC(t_a,wpl)
    END;
  END;
END roll;

PROCEDURE sign(w: window; x,y: INTEGER; sn: CHAR);
  VAR cx,csz,i,j,l,yy: INTEGER; a,b,c: ADDRESS;
BEGIN
  IF x<w^.W THEN cx:=w^.W-x; IF cx>=signs^.w THEN RETURN END
  ELSE cx:=0;
  END;
  IF x+signs^.w-1>w^.E THEN csz:=w^.E-x-cx+1; IF csz<=0 THEN RETURN END
  ELSE csz:=signs^.w-cx;
  END;
  a:=w^.mem+(w^.sy-1-y)*w^.wpl;
  j:=w^.wpl*w^.sy;
  FOR i:=0 TO lsno-1 DO
    IF i IN BITSET(w^.color) THEN
      b:=signs^.base+signs^.h*ORD(sn)+signs^.h-1; c:=a; yy:=y;
      FOR l:=1 TO signs^.h DO
        IF (yy>=w^.S) & (yy<=w^.N) THEN
          bitmove(w^.mode MOD 4,c,x+cx,b,cx,csz);
        END;
        DEC(c,w^.wpl); INC(yy); DEC(b);
      END;
      INC(sn);
    END;
    INC(a,j);
  END;
END sign;

PROCEDURE load_font(VAL nm: ARRAY OF CHAR): font;
  VAR f: bio.FILE; size: INTEGER; fnt: font;
BEGIN
  fnt:=NIL;
  bio.open(f,nm,'r');
  size:=(bio.eof(f)+3) DIV 4;
  mem.ALLOCATE(fnt,size);
  bio.read(f,fnt,bio.eof(f));
  bio.close(f);
  fnt^.base:=ADDRESS(fnt)+SIZE(fnt^);
  fnt^.p_w :=fnt^.base + fnt^.h*256;
  RETURN fnt;
END load_font;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={}; lnFF[i]:={0..31} END;

  main_c:=load_font('ped_c.bmf');
  main_p:=load_font('ped_p.bmf');
  signs :=load_font('ped_signs.bmf');

  char_w:=main_c^.w;
  char_h:=main_c^.h;

  sign_w:=signs^.w;
  sign_h:=signs^.h;
END init;

VAR r: INTEGER;

BEGIN
  ASSERT(wnd.lsno=lsno);
  init;
  xsz[0]:=32; ysz[0]:=1;
  pat[0][0]:={};
  xsz[1]:=32; ysz[1]:=1;
  pat[1][0]:={0..31};
  xsz[2]:=16; ysz[2]:=4;
  pat[2][0]:={2..3,6..7,10..11,14..15};
  pat[2][1]:={1..2,5..6, 9..10,13..14};
  pat[2][2]:={0..1,4..5, 8.. 9,12..13};
  pat[2][3]:={   0,3..4, 7.. 8,11..12,15};
  xsz[3]:=16; ysz[3]:=4;
  pat[3][0]:={2..3,6..7,10..11,14..15};
  pat[3][1]:={0,3..4,7..8,11..12,15};
  pat[3][2]:={0..1,4..5,8..9,12..13};
  pat[3][3]:={1..2,5..6,9..10,13..14};
  xsz[4]:=16; ysz[4]:=2;
  pat[4][0]:={0,4,8,12};
  pat[4][1]:={};
END cdsGrafic.
