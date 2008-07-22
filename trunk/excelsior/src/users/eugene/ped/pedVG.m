IMPLEMENTATION MODULE pedVG; (* Ilx 14-Apr-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
IMPORT s: SYSTEM;
IMPORT  VF;
IMPORT  vg: VG;
IMPORT  mcd: defCodes;
IMPORT  wnd: Windows;

TYPE ADDRESS  = s.ADDRESS;
     WORD     = s.WORD;
     window   = wnd.window;
     CLP_PARM = RECORD xa,ya,xb,yb: INTEGER; END;

CONST Colors= 16;

VAR
  ln00     : ARRAY [0..63] OF BITSET; (* 2048 000000 *)

CONST
  max=4;

TYPE
  bpt=ARRAY [0..7] OF BITSET;

VAR
  xsz: ARRAY [0..max] OF INTEGER;
  ysz: ARRAY [0..max] OF INTEGER;
  pat: ARRAY [0..max] OF bpt;

PROCEDURE MOVE(to,from: ADDRESS; n: INTEGER); CODE mcd.move END MOVE;
PROCEDURE bblt(t: ADDRESS; ot: INTEGER;
               f: ADDRESS; of,s: INTEGER);    CODE mcd.bblt END bblt;
PROCEDURE bitmove(mode: INTEGER;
                    to: ADDRESS;   to_ofs: INTEGER;
                  from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE 0F9h 02h END bitmove;

VAR main_c,main_p,signs: VF.Font;
                   prop: BOOLEAN;

PROCEDURE Min(X,Y: INTEGER): INTEGER;
BEGIN IF X<Y THEN RETURN X ELSE RETURN Y END END Min;

PROCEDURE Max(X,Y: INTEGER): INTEGER;
BEGIN IF X>Y THEN RETURN X ELSE RETURN Y END END Max;

PROCEDURE color(w: window; i: INTEGER);
BEGIN
  w^.color:=i MOD 16;
END color;

PROCEDURE mode(w: window; m: modes);
  VAR b: POINTER TO vg.BMD;
BEGIN
  b:=ADDRESS(w); vg.mode(b^,INTEGER(m));
END mode;

PROCEDURE inverse(w: window; OnOff: inverse_modes);
  VAR b: POINTER TO vg.BMD;
BEGIN
  b:=ADDRESS(w);
  IF OnOff=on THEN vg.inverse(b^,4) ELSE vg.inverse(b^,0) END;
END inverse;

PROCEDURE clip(W,E,S,N: INTEGER; VAR x0,y0,x1,y1: INTEGER): BOOLEAN;

  PROCEDURE clp(VAR clp_parm: CLP_PARM; w,h:INTEGER): BOOLEAN;
  CODE 0F9h 004h END clp;

  VAR clp_parm: CLP_PARM;
      t       : BOOLEAN;

BEGIN
  WITH clp_parm DO
    xa:=x0-W; xb:=x1-W; ya:=y0-S; yb:=y1-S;
    t:=clp(clp_parm,E-W-1,N-S-1);
    IF t THEN x0:=xa+W; x1:=xb+W; y0:=ya+S; y1:=yb+S; RETURN TRUE END;
    RETURN t;
  END;
END clip;

PROCEDURE vect(w: window; x,y,x1,y1: INTEGER);
  VAR j: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR j:=0 TO 3 DO
    IF j IN BITSET(w^.color) THEN vg.line(bmd,x,y,x1,y1) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END vect;

PROCEDURE patt_vect(w: window; x,y,x1,y1: INTEGER);
  VAR j: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR j:=0 TO 3 DO
    IF j IN BITSET(w^.color) THEN vg.patt_line(bmd,x,y,x1,y1) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END patt_vect;

PROCEDURE frame(w: window; x0,y0,x1,y1: INTEGER);
BEGIN
  IF ABS(x0-x1)>=2 THEN
    vect(w,x1-1,y1,x0+1,y1);
    IF y0#y1 THEN vect(w,x0+1,y0,x1-1,y0) END;
  END;
  vect(w,x1  ,y0,x1  ,y1);
  IF x0#x1 THEN vect(w,x0  ,y1,x0  ,y0) END;
END frame;

PROCEDURE box(w: window; x0,y0,x1,y1: INTEGER);
  VAR j: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR j:=0 TO 3 DO
    IF j IN BITSET(w^.color) THEN vg.rect(bmd,x0,y0,x1,y1) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END box;

PROCEDURE patt_box(w: window; x0,y0,x1,y1,p: INTEGER);
  VAR px,py,i,n,x,l: INTEGER; pp: POINTER TO bpt; a: ADDRESS;
BEGIN
  IF x1<x0 THEN i:=x1; x1:=x0; x0:=i END;
  IF y1<y0 THEN i:=y1; y1:=y0; y0:=i END;
  IF x0<0 THEN
    IF x1<0 THEN RETURN END;
    x0:=0;
  END;
  IF y0<0 THEN
    IF y1<0 THEN RETURN END;
    y0:=0;
  END;
  IF x1>=w^.dsx THEN
    IF x0>=w^.dsx THEN RETURN END;
    x1:=w^.dsx-1;
  END;
  IF y1>=w^.sy THEN
    IF y0>=w^.sy THEN RETURN END;
    y1:=w^.sy-1;
  END;
  FOR l:=0 TO 3 DO
    IF l IN BITSET(w^.color) THEN
      a:=w^.mem+l*w^.sx*w^.sy+(w^.sy-y0-1)*w^.sx;
      pp:=ADR(pat[p]);
      px:=xsz[p]; py:=ysz[p];
      FOR i:=0 TO y1-y0 DO
        n:=x1-x0+1; x:=x0;
        WHILE n>px DO
          bitmove(w^.mode,a,x,ADR(pp^[i MOD py]),0,px);
          INC(x,px); DEC(n,px);
        END;
        IF n>0 THEN bitmove(w^.mode,a,x,ADR(pp^[i MOD py]),0,n) END;
        DEC(a,w^.sx);
      END;
    END;
  END;
END patt_box;

PROCEDURE rect(w: window; x0,y0,x1,y1: INTEGER);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.rect(bmd,x0,y0,x1,y1) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END rect;

PROCEDURE prop_font(on: BOOLEAN);
BEGIN
  prop:=on;
  IF prop THEN char_w:=main_p^.w; char_h:=main_p^.h
  ELSE char_w:=main_c^.w; char_h:=main_c^.h
  END;
END prop_font;

PROCEDURE Write(w: window; x,y: INTEGER; ch: CHAR);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  IF prop THEN vg.font(main_p) ELSE vg.font(main_c) END;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.write(bmd,x,y,ch) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END Write;

PROCEDURE write_str(w: window; x,y: INTEGER; VAL str: ARRAY OF CHAR);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  IF prop THEN vg.font(main_p) ELSE vg.font(main_c) END;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.write_str(bmd,x,y,str) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END write_str;

PROCEDURE clip_write_str(w: window; W,E,S,N: INTEGER;
                         x,y: INTEGER; VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  IF prop THEN vg.font(main_p) ELSE vg.font(main_c) END;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN
      vg.clip_write_str(bmd,W,E,S,N,x,y,s)
    END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END clip_write_str;

PROCEDURE print(w: window; x,y : INTEGER; VAL fmt : ARRAY OF CHAR; SEQ args: WORD);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  IF prop THEN vg.font(main_p) ELSE vg.font(main_c) END;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.print(bmd,x,y,fmt,args) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END print;

PROCEDURE clip_print(w: window; W,E,S,N: INTEGER;
                     x,y   : INTEGER;
                     VAL fmt: ARRAY OF CHAR;
                     SEQ args  : WORD);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  IF prop THEN vg.font(main_p) ELSE vg.font(main_c) END;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN
      vg.clip_print(bmd,W,E,S,N,x,y,fmt,args)
    END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END clip_print;

PROCEDURE circ(w: window; X,Y: INTEGER; r: INTEGER);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.circ(bmd,X,Y,r) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END circ;

PROCEDURE arc(w: window; X0,Y0,xa,ya,xb,yb,R: INTEGER);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.arc(bmd,X0,Y0,xa,ya,xb,yb,R) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END arc;

PROCEDURE dot(w: window; x,y: INTEGER);
  VAR i: INTEGER;
      b: POINTER TO vg.BMD;
      bmd: vg.BMD;
BEGIN
  b:=ADDRESS(w); bmd:=b^;
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN vg.dot(bmd,x,y) END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END dot;

PROCEDURE fill(w: window; i: INTEGER);
  VAR p: ADDRESS; j: INTEGER;
BEGIN
  IF (i<0) & (i>=Colors) THEN i:=0 END;
  p:=w^.mem;
  FOR j:=0 TO w^.sy-1 DO
    IF 0 IN BITSET(i) THEN
      p^:=0; MOVE(p+1,p,w^.sx-1);
    END;
    INC(p,w^.sx*w^.sy);
    IF 1 IN BITSET(i) THEN
      p^:=0; MOVE(p+1,p,w^.sx-1);
    END;
    INC(p,w^.sx*w^.sy);
    IF 2 IN BITSET(i) THEN
      p^:=0; MOVE(p+1,p,w^.sx-1);
    END;
    INC(p,w^.sx*w^.sy);
    IF 3 IN BITSET(i) THEN
      p^:=0; MOVE(p+1,p,w^.sx-1);
    END;
    DEC(p,(3*w^.sy-1)*w^.sx);
  END;
  w^.ini_proc(w^.info);
  wnd.refresh(w);
END fill;

  VAR Lnpt: ARRAY [0..15] OF INTEGER;

PROCEDURE greed(w: window; Scale,x,y: INTEGER);
  VAR k,l,i,j: INTEGER;
            p: ADDRESS;
BEGIN
  color(w,8); mode(w,rep);
  x:=x MOD 48; y:=y MOD 48;
(*
  k:=(w^.dsx-(w^.W+w^.E)) DIV (48 DIV Scale);
  l:=(w^.sy -(w^.N+w^.S)) DIV (48 DIV Scale);
  IF (k < (w^.dsx-(w^.W+w^.E)) DIV 2) & (l < (w^.sy-(w^.N+w^.S)) DIV 2) THEN
    b.base:=ADR(Lnpt); vg.bmd.w:=w^.dsx;
    b.h:=1;
    Lnpt[0]:=0;
    MOVE(ADR(Lnpt)+1,ADR(Lnpt),w^.sx-1);
    vg.mode:=vg.rep;
    FOR i:=0 TO k DO
      vg.dot((i*48-x) DIV Scale,0);
    END;
    i:=(48 DIV Scale)*w^.sx;
    p:=w^.mem+(w^.sy*3+w^.dsx-1)*w^.sx + y DIV Scale*w^.sx;
    IF (p-i*l)<(w^.mem+w^.sy*w^.sx*3) THEN INC(p,i) END;
    FOR j:=0 TO l DO
      vg.bit_move(vg.or,p,0,ADR(Lnpt),0,w^.sx*32); DEC(p,i)
--    MOVE(p,ADR(Lnpt),15);
    END;
  END;
*)
END greed;

PROCEDURE rollN(w: window; n: INTEGER);
  VAR f0,f1,t0,t1: ADDRESS;
      f2,t2,f3,t3: ADDRESS;
      y: INTEGER;
BEGIN
  IF n=0 THEN RETURN END;
  ASSERT(n>0);
  t0:=w^.mem+(0*w^.sy+w^.N)*w^.sx; f0:=t0+w^.sx*rollY*n;
  t1:=w^.mem+(1*w^.sy+w^.N)*w^.sx; f1:=t1+w^.sx*rollY*n;
  t2:=w^.mem+(2*w^.sy+w^.N)*w^.sx; f2:=t2+w^.sx*rollY*n;
  t3:=w^.mem+(3*w^.sy+w^.N)*w^.sx; f3:=t3+w^.sx*rollY*n;
  FOR y:=w^.S+1 TO w^.sy-w^.N-rollY*n DO
    bblt(t0,w^.W,f0,w^.W,w^.dsx-(w^.W+w^.E)); INC(t0,w^.sx); INC(f0,w^.sx);
    bblt(t1,w^.W,f1,w^.W,w^.dsx-(w^.W+w^.E)); INC(t1,w^.sx); INC(f1,w^.sx);
    bblt(t2,w^.W,f2,w^.W,w^.dsx-(w^.W+w^.E)); INC(t2,w^.sx); INC(f2,w^.sx);
    bblt(t3,w^.W,f3,w^.W,w^.dsx-(w^.W+w^.E)); INC(t3,w^.sx); INC(f3,w^.sx);
  END;
  n:=rollY*n; IF n>w^.sy THEN n:=w^.sy END;
  FOR y:=1 TO n DO
    bblt(t0,w^.W,ADR(ln00),0,w^.dsx-(w^.W+w^.E)); INC(t0,w^.sx);
    bblt(t1,w^.W,ADR(ln00),0,w^.dsx-(w^.W+w^.E)); INC(t1,w^.sx);
    bblt(t2,w^.W,ADR(ln00),0,w^.dsx-(w^.W+w^.E)); INC(t2,w^.sx);
    bblt(t3,w^.W,ADR(ln00),0,w^.dsx-(w^.W+w^.E)); INC(t3,w^.sx);
  END;
  w^.ini_proc(w^.info);
  wnd.refresh(w);
END rollN;

PROCEDURE rollE(w: window; n: INTEGER);
  VAR t0,t1,t2,t3: ADDRESS;
      bf: ARRAY [0..31] OF INTEGER;
      y,sz: INTEGER;
BEGIN
  IF n<=0 THEN RETURN END;
  ASSERT(n>0);
  sz:=w^.dsx-n*32-(w^.W+w^.E);
  IF sz<0 THEN sz:=0; n:=w^.sx END;
  t0:=w^.mem+(0*w^.sy+w^.N)*w^.sx;
  t1:=w^.mem+(1*w^.sy+w^.N)*w^.sx;
  t2:=w^.mem+(2*w^.sy+w^.N)*w^.sx;
  t3:=w^.mem+(3*w^.sy+w^.N)*w^.sx;
  FOR y:=w^.S+1 TO w^.sy-w^.N DO
    bblt(ADR(bf),w^.W,t0,w^.W,sz); bblt(t0+n,w^.W,ADR(bf),w^.W,sz);
    bblt(t0,w^.W,ADR(ln00),w^.W,n*32); INC(t0,w^.sx);
    bblt(ADR(bf),w^.W,t1,w^.W,sz); bblt(t1+n,w^.W,ADR(bf),w^.W,sz);
    bblt(t1,w^.W,ADR(ln00),w^.W,n*32); INC(t1,w^.sx);
    bblt(ADR(bf),w^.W,t2,w^.W,sz); bblt(t2+n,w^.W,ADR(bf),w^.W,sz);
    bblt(t2,w^.W,ADR(ln00),w^.W,n*32); INC(t2,w^.sx);
    bblt(ADR(bf),w^.W,t3,w^.W,sz); bblt(t3+n,w^.W,ADR(bf),w^.W,sz);
    bblt(t3,w^.W,ADR(ln00),w^.W,n*32); INC(t3,w^.sx);
  END;
  w^.ini_proc(w^.info);
  wnd.refresh(w);
END rollE;

PROCEDURE rollS(w: window; n: INTEGER);
  VAR f0,f1,t0,t1,f2,t2,f3,t3: ADDRESS; y: INTEGER;
BEGIN
  IF n=0 THEN RETURN END;
  ASSERT(n>0);
  t0:=w^.mem+(0*w^.sy+w^.sy-w^.S-1)*w^.sx; f0:=t0-w^.sx*rollY*n;
  t1:=w^.mem+(1*w^.sy+w^.sy-w^.S-1)*w^.sx; f1:=t1-w^.sx*rollY*n;
  t2:=w^.mem+(2*w^.sy+w^.sy-w^.S-1)*w^.sx; f2:=t2-w^.sx*rollY*n;
  t3:=w^.mem+(3*w^.sy+w^.sy-w^.S-1)*w^.sx; f3:=t3-w^.sx*rollY*n;
  FOR y:=w^.S+1 TO w^.sy-w^.N-rollY*n DO
    bblt(t0,w^.W,f0,w^.W,w^.dsx-(w^.W+w^.E)); DEC(t0,w^.sx); DEC(f0,w^.sx);
    bblt(t1,w^.W,f1,w^.W,w^.dsx-(w^.W+w^.E)); DEC(t1,w^.sx); DEC(f1,w^.sx);
    bblt(t2,w^.W,f2,w^.W,w^.dsx-(w^.W+w^.E)); DEC(t2,w^.sx); DEC(f2,w^.sx);
    bblt(t3,w^.W,f3,w^.W,w^.dsx-(w^.W+w^.E)); DEC(t3,w^.sx); DEC(f3,w^.sx);
  END;
  n:=rollY*n; IF n>w^.sy THEN n:=w^.sy END;
  FOR y:=1 TO n DO
    bblt(t0,w^.W,ADR(ln00),w^.W,w^.dsx-(w^.W+w^.E)); DEC(t0,w^.sx);
    bblt(t1,w^.W,ADR(ln00),w^.W,w^.dsx-(w^.W+w^.E)); DEC(t1,w^.sx);
    bblt(t2,w^.W,ADR(ln00),w^.W,w^.dsx-(w^.W+w^.E)); DEC(t2,w^.sx);
    bblt(t3,w^.W,ADR(ln00),w^.W,w^.dsx-(w^.W+w^.E)); DEC(t3,w^.sx);
  END;
  w^.ini_proc(w^.info);
  wnd.refresh(w);
END rollS;

PROCEDURE rollW(w: window; n: INTEGER);
  VAR f0,f1,t0,t1,t2,f2,t3,f3: ADDRESS; y,sz: INTEGER;
BEGIN
  IF n<=0 THEN RETURN END;
  t0:=w^.mem+(0*w^.sy+w^.N)*w^.sx; f0:=t0+n;
  t1:=w^.mem+(1*w^.sy+w^.N)*w^.sx; f1:=t1+n;
  t2:=w^.mem+(2*w^.sy+w^.N)*w^.sx; f2:=t2+n;
  t3:=w^.mem+(3*w^.sy+w^.N)*w^.sx; f3:=t3+n;
  sz:=w^.dsx-n*32-(w^.W+w^.E);
  IF sz<0 THEN sz:=0; n:=w^.sx END;
  FOR y:=w^.S+1 TO w^.sy-w^.N DO
    bblt(t0,w^.W,f0,w^.W,sz); bblt(t0,sz+w^.W,ADR(ln00),0,n*32);
    INC(t0,w^.sx); INC(f0,w^.sx);
    bblt(t1,w^.W,f1,w^.W,sz); bblt(t1,sz+w^.W,ADR(ln00),0,n*32);
    INC(t1,w^.sx); INC(f1,w^.sx);
    bblt(t2,w^.W,f2,w^.W,sz); bblt(t2,sz+w^.W,ADR(ln00),0,n*32);
    INC(t2,w^.sx); INC(f2,w^.sx);
    bblt(t3,w^.W,f3,w^.W,sz); bblt(t3,sz+w^.W,ADR(ln00),0,n*32);
    INC(t3,w^.sx); INC(f3,w^.sx);
  END;
  w^.ini_proc(w^.info);
  wnd.refresh(w);
END rollW;

PROCEDURE cursor(w: window; x,y,no: INTEGER);
BEGIN
  IF no=4 THEN
    vect(w,x-8,y-8,x-4,y-4);
    vect(w,x+8,y-8,x+4,y-4);
    vect(w,x+8,y+8,x+4,y+4);
    vect(w,x-8,y+8,x-4,y+4);
    dot(w,x,y);
  ELSE
    vect(w,x+4,y,x+3,y);
    vect(w,x-4,y,x-3,y);
    vect(w,x,y+4,x,y+3);
    vect(w,x,y-4,x,y-3);
(*
    circ(x,y,3);
    dot(x,y);
*)
  END;
END cursor;

PROCEDURE sign(w: window; x,y: INTEGER; sn: CHAR);
  VAR a: POINTER TO vg.BMD;
      bmd: vg.BMD; i: INTEGER;
BEGIN
  a:=ADDRESS(w); bmd:=a^;
  vg.font(signs);
  FOR i:=0 TO 3 DO
    IF i IN BITSET(w^.color) THEN
      vg.write(bmd,x,y,sn)
    END;
    INC(bmd.base,w^.sx*w^.sy);
  END;
END sign;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  rollX  :=32; rollY:=16;
  FOR i:=0 TO HIGH(ln00) DO ln00[i]:={     } END;
  prop:=FALSE;
  VF.make_font('ped_c.bmf'); main_c:=VF.font;
  IF main_c=NIL THEN HALT(1) END;
  char_w:=main_c^.w;
  char_h:=main_c^.h;
  VF.make_font('ped_signs.bmf');
  signs:=VF.font;
  IF signs=NIL THEN HALT(1) END;
  sign_w:=signs^.w;
  sign_h:=signs^.h;
END init;

BEGIN
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
END pedVG.
