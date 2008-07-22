IMPLEMENTATION MODULE ttyWindows; (*$N+$U+$X+ Leo 04-Jun-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  tty: Terminal;
IMPORT  str: Strings;
IMPORT  low: lowLevel;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  fmt: Formats;
IMPORT  lex: Lexicon;
IMPORT  mem: Heap;

FROM SYSTEM  IMPORT ADR;


WITH STORAGE (NEW:     mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE:  mem.reallocate);


TYPE
  ATTRS    = DYNARR OF BITSET;
  LINE     = RECORD txt: STRING; atr: ATTRS END;
  SCREEN   = DYNARR OF LINE;
  VIEWLIST = POINTER TO VIEWNODE;

  WINDOW   = POINTER TO WDESC;
  WDESC    = RECORD  magic: INTEGER;
               scr : SCREEN;
               view: VIEW;
               open: BOOLEAN;
               free: BOOLEAN;
               stk : VIEWLIST;
               attr: BITSET;
               dw  : WINDOW;
               up  : WINDOW
             END;

  VIEWNODE = RECORD
               view: VIEW;
               next: VIEWLIST;
             END;

VAR all: WINDOW; (* set of windows *)
    TOP: WINDOW; (* top    window  *)
  MAGIC: INTEGER;

CONST
  spaces = "                                          "
           "                                          ";
  SPACES = 20202020h;

VAR
  HBAR: INTEGER;

PROCEDURE adr(VAL s: ARRAY OF CHAR): SYSTEM.ADDRESS; CODE cod.drop END adr;

PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;
PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;

PROCEDURE min(x,y: INTEGER): INTEGER;
BEGIN
  IF x<y THEN RETURN x ELSE RETURN y END
END min;

PROCEDURE max(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN x ELSE RETURN y END
END max;

PROCEDURE cursor_to_top(crs: INTEGER);
  VAR W: INTEGER;
BEGIN
  IF freeze THEN RETURN END;
  IF TOP=NIL THEN
    IF (crs>=0) & (crs#tty.state^.cursor) THEN tty.set_cursor(crs) END; RETURN
  END;
  WITH TOP^.view DO
    IF cursor=off THEN
      IF tty.state^.cursor#0 THEN tty.set_cursor(0) END; RETURN
    END;
    tty.set_pos(l+wl+frame,c+wc+frame); tty.set_cursor(1)
  END
END cursor_to_top;

PROCEDURE cursor_off(): INTEGER;
  VAR i,c: INTEGER;
BEGIN
  IF freeze THEN RETURN tty.state^.cursor END;
  c:=tty.state^.cursor;
  IF c#0 THEN tty.set_cursor(0) END;
  RETURN c
END cursor_off;

PROCEDURE setpos(vp: WINDOW; _l,_c: INTEGER);
  VAR crs: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    IF (_l=wl) & (_c=wc) THEN RETURN END;
    IF (_l<0) OR (_l>=h-frame*2) OR (_c<0) OR (_c>=w-frame*2) THEN
      bad_parm; RETURN
    END;
    IF (TOP=vp) & (cursor#off) THEN crs:=cursor_off()  END;
    wl:=_l; wc:=_c;
    IF (TOP=vp) & (cursor#off) THEN cursor_to_top(crs) END
  END
END setpos;

PROCEDURE hoverlap(vp: WINDOW);
  VAR v: WINDOW;
  L0,L1: INTEGER;
BEGIN
  ASSERT(all#NIL);
  WITH vp^.view DO L0:=l; L1:=L0+h END;
  vp^.free:=TRUE;
  v:=all;
  REPEAT
    IF (v#vp) & (v^.open) THEN
      WITH v^.view DO
        IF (l>=L1) OR (L0>=l+h) THEN (* not overlaped *)
        ELSE
          vp^.free:=FALSE; v^.free:=FALSE
        END
      END
    END;
    v:=v^.dw
  UNTIL v=all
END hoverlap;

PROCEDURE set_tty_attrs(a: BITSET);
  VAR fg,bg,rv,un: INTEGER;
BEGIN
  bg:=INTEGER(a*{0..7})-128;  a:=a>>8;
  fg:=INTEGER(a*{0..7})-128;  a:=a>>8;
  rv:=INTEGER(a*{0});         a:=a>>1;
  un:=INTEGER(a*{0});
  IF (fg>=tty.state^.min_color) & (fg<=tty.state^.max_color) THEN
    IF fg#tty.state^.color   THEN tty.set_color (fg)    END
  END;
  IF (bg>=tty.state^.min_color) & (bg<=tty.state^.max_color) THEN
    IF bg#tty.state^.back    THEN tty.set_back(bg)      END
  END;
  IF un#tty.state^.underline THEN tty.set_underline(un) END;
  IF rv#tty.state^.reverse   THEN tty.set_reverse  (rv) END;
END set_tty_attrs;

PROCEDURE packed_attrs(w: WINDOW): BITSET;
BEGIN
  WITH w^.view DO
    RETURN BITSET((back+128)+(fore+128)<<8
                 +(reverse MOD 2)<<16+(underline MOD 2)<<17)
  END
END packed_attrs;

PROCEDURE write_line(VAL l: LINE; pos,len: INTEGER);
  VAR i: INTEGER;
BEGIN
  WHILE len>0 DO
    i:=pos+1;
    WHILE (i<pos+len) & (l.atr[i]=l.atr[pos]) DO INC(i) END;
    set_tty_attrs(l.atr[pos]);
    tty.write(l.txt,pos,i-pos);
    DEC(len,i-pos);  pos:=i
  END;
END write_line;

PROCEDURE long_write(l,c: INTEGER; VAL ln: LINE; pos,len: INTEGER);
  VAR s: STRING;
      a: ATTRS;   fgzero: BITSET;
    i,j: INTEGER;   mask: BITSET;
BEGIN
  s^:=ln.txt^;  fgzero:=BITSET(128)<<8;
  a^:=ln.atr^;  mask:={8..15};
  i:=pos+len-1;
  IF (s[i]#' ') OR (a[i]*mask#fgzero) THEN DEC(i) END;
  j:=i;
  IF (s[j]=' ') & (a[j]*mask=fgzero) THEN
    REPEAT j:=j-1 UNTIL (j=pos) OR (s[j]#' ') OR (a[j]*mask#fgzero)
  END;
  IF i-j<=12 THEN write_line(ln,pos,len); RETURN END;
  IF (s[j]#' ') OR (a[j]*mask#fgzero) THEN INC(j) END;
  IF j-pos>0  THEN write_line(ln,pos,j-pos) END;
  IF (s[pos+len-1]=' ') & (a[pos+len-1]*mask=fgzero) THEN
    IF c+len>=maxW THEN tty.erase_line(0) ELSE tty.erase_chars(i-j+1) END;
    RETURN
  END;
  tty.erase_chars(i-j+1);
  tty.set_pos(l,c+len-1);
  write_line(ln,pos+len-1,1)
END long_write;

PROCEDURE redraw(vp: WINDOW; _l,_c,_w,_h: INTEGER);
  VAR i,j,f,b: INTEGER;  F,B: BOOLEAN;
BEGIN
  IF freeze THEN RETURN END;
  ASSERT(_l>=0); ASSERT(_c>=0);
  WITH vp^.view DO
    IF _l+_h>h THEN _h:=h-_l END;
    IF _c+_w>w THEN _w:=w-_c END;
    IF (_h<=0) OR (_w<=0) THEN RETURN END;
    FOR i:=_l TO _l+_h-1 DO
      tty.set_pos(l+i,c+_c);
      IF _w<20 THEN
        write_line(vp^.scr[i],_c,_w)
      ELSE
        long_write(l+i,c+_c,vp^.scr[i],_c,_w)
      END
    END
  END
END redraw;

PROCEDURE _refresh(vp,over: WINDOW; L0,C0,L1,C1: INTEGER);
  VAR l,c: INTEGER;
    l0,c0: INTEGER;
    l1,c1: INTEGER;
BEGIN
  IF (L1<=L0) OR (C1<=C0) THEN RETURN END;
  IF over=all THEN
    WITH vp^.view DO redraw(vp,L0-l,C0-c,C1-C0,L1-L0) END; RETURN
  END;
  over:=over^.up;
  IF NOT over^.open THEN _refresh(vp,over,L0,C0,L1,C1); RETURN END;
  WITH over^.view DO l0:=l; c0:=c; l1:=l0+h; c1:=c0+w END;
  IF (L1<=l0) OR (C1<=c0) OR (C0>=c1) OR (L0>=l1) THEN
    (* not crossed windows, skip over *)
    _refresh(vp,over,L0,C0,L1,C1); RETURN
  END;

  IF L0<l0 THEN
    l:=l0;
    IF L1<l THEN l:=L1 END;
    _refresh(vp,over,L0,C0,l,C1); L0:=l
  END;
  IF l1<L1 THEN
    l:=l1;
    IF l<L0 THEN l:=L0 END;
    _refresh(vp,over,l,C0,L1,C1); L1:=l
  END;
  IF L1<=L0 THEN RETURN END;
  IF C0<c0 THEN
    c:=c0;
    IF C1<c THEN c:=C1 END;
    _refresh(vp,over,L0,C0,L1,c)
  END;
  IF c1<C1 THEN
    c:=c1;
    IF c<C0 THEN c:=C0 END;
    _refresh(vp,over,L0,c,L1,C1)
  END;
END _refresh;

PROCEDURE REFRESH(vp: WINDOW; _l,_c,_w,_h: INTEGER);
  VAR crs: INTEGER;
BEGIN
  ASSERT(all#NIL);
  crs:=cursor_off();
  WITH vp^.view DO
    _refresh(vp,vp,l+_l,c+_c,l+_l+_h,c+_c+_w)
  END;
  cursor_to_top(crs)
END REFRESH;

PROCEDURE _erase(vp: WINDOW; _l,_c,_w,_h: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF freeze THEN RETURN END;
  ASSERT(_l>=0); ASSERT(_c>=0);
  WITH vp^.view DO
    IF _l+_h>h THEN _h:=h-_l END;
    IF _c+_w>w THEN _w:=w-_c END;
    IF (_h<=0) OR (_w<=0) THEN RETURN END;
    IF (_c=0) & (_w>=maxW) & (_l=0) & (_h>=maxH) THEN
      set_tty_attrs({});
      tty.erase(2);
      IF NOT tty.done THEN tty.home; tty.erase(0) END;
      RETURN
    END;
    set_tty_attrs({});
    FOR i:=_l TO _l+_h-1 DO
      tty.set_pos(l+i,c+_c);
      IF c+_c+_w>=maxW THEN
        tty.erase_line(0)
      ELSE
        tty.erase_chars(_w)
      END
    END
  END
END _erase;

PROCEDURE _clear(vp,top: WINDOW; era,under: BOOLEAN; L0,C0,L1,C1: INTEGER);
  VAR L,C: INTEGER;
      W,H: INTEGER;
     next: WINDOW;
    l0,c0: INTEGER;
    l1,c1: INTEGER;
BEGIN
  IF (L1<=L0) OR (C1<=C0) THEN RETURN END;
  next:=top^.dw;
  IF under & (top=all) THEN
    IF era THEN
      WITH vp^.view DO _erase(vp,L0-l,C0-c,C1-C0,L1-L0) END
    END;
    RETURN
  END;
  IF  vp=top       THEN _clear(vp,next,era,TRUE, L0,C0,L1,C1); RETURN END;
  IF NOT top^.open THEN _clear(vp,next,era,under,L0,C0,L1,C1); RETURN END;

  WITH top^.view DO l0:=l; c0:=c; l1:=l0+h; c1:=c0+w END;
  IF (L1<=l0) OR (C1<=c0) OR (C0>=c1) OR (L0>=l1) THEN
    (* not crossed windows, skip *)
    _clear(vp,next,era,under,L0,C0,L1,C1); RETURN
  END;

  IF L0<l0 THEN
    L:=l0;
    IF L1<L THEN L:=L1 END;
    _clear(vp,next,era,under,L0,C0,L,C1); L0:=L
  END;
  IF l1<L1 THEN
    L:=l1;
    IF L<L0 THEN L:=L0 END;
    _clear(vp,next,era,under,L,C0,L1,C1); L1:=L
  END;
  IF L1>L0 THEN
    IF C0<c0 THEN
      C:=c0;
      IF C1<C THEN C:=C1 END;
      _clear(vp,next,era,under,L0,C0,L1,C)
    END;
    IF c1<C1 THEN
      C:=c1;
      IF C<C0 THEN C:=C0 END;
      _clear(vp,next,era,under,L0,C,L1,C1)
    END
  END;
  IF under THEN
    L:=l0;
    IF L<L0 THEN L:=L0 END;
    H:=L1-L;
    C:=c0;
    IF C<C0 THEN C:=C0 END;
    W:=C1-C;
    WITH top^.view DO
      IF H>h THEN H:=h END;
      IF W>w THEN W:=w END;
      IF (W>0) & (H>0) THEN _refresh(top,top,L,C,L+H,C+W) END
    END
  END;
  hoverlap(top)
END _clear;

PROCEDURE CLEAR(vp: WINDOW; _l,_c,_w,_h: INTEGER);
  VAR crs: INTEGER;
BEGIN
  ASSERT(all#NIL);
  crs:=cursor_off();
  WITH vp^.view DO
    _clear(vp,all,TRUE,FALSE,l+_l,c+_c,l+_l+h,c+_c+w)
  END;
  cursor_to_top(crs)
END CLEAR;

PROCEDURE dispose_screen(VAR s: SCREEN);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(s) DO DISPOSE(s[i].txt); DISPOSE(s[i].atr) END;
  DISPOSE(s)
END dispose_screen;

PROCEDURE new_screen(VAR s: SCREEN; w,h: INTEGER; attr: BITSET);
  VAR i: INTEGER;
BEGIN
  NEW(s,h);
  IF NOT mem.done THEN mem_error; RETURN END;
  FOR i:=0 TO h-1 DO NEW(s[i].txt); NEW(s[i].atr) END;
  FOR i:=0 TO h-1 DO
    NEW(s[i].txt,w);
    IF NOT mem.done THEN mem_error; dispose_screen(s); RETURN END;
    NEW(s[i].atr,w);
    IF NOT mem.done THEN mem_error; dispose_screen(s); RETURN END
  END;
  FOR i:=0 TO HIGH(s) DO
    low.fill(s[i].txt,SPACES);  low.fill(s[i].atr,attr)
  END;
  done:=TRUE
END new_screen;

PROCEDURE _frame(scr: SCREEN; tog: INTEGER);
  VAR li: STRING;
      l0: STRING;
      lh: STRING;
     w,h: INTEGER;
     i,j: INTEGER;
BEGIN
  h:=HIGH(scr)+1;
  w:=HIGH(scr[0].txt)+1;
  l0^:=scr[0].txt^;   lh^:=scr[h-1].txt^;   j:=w-1;
  IF tog#off THEN
    low.fill(l0,HBAR); low.fill(lh,HBAR);
    FOR i:=1 TO h-2 DO li^:=scr[i].txt^; li[0]:=vbar; li[j]:=vbar END;
    l0[0]:=bars[0,0];    l0[j]:=bars[0,2];
    lh[0]:=bars[2,0];    lh[j]:=bars[2,2]
  ELSE
    low.fill(l0,SPACES);
    low.fill(lh,SPACES);
    FOR i:=1 TO h-2 DO li^:=scr[i].txt^; li[0]:=' '; li[j]:=' ' END;
    l0[0]:=' ';    l0[j]:=' ';
    lh[0]:=' ';    lh[j]:=' '
  END
END _frame;

PROCEDURE copy_screen(VAR d: SCREEN; VAL s: SCREEN; frame: INTEGER);
  VAR i: INTEGER;
    w,h: INTEGER;
BEGIN
  h:=HIGH(s);
  IF h>HIGH(d) THEN h:=HIGH(d) END;
  IF h<0 THEN RETURN END;
  w:=HIGH(s[0].txt);
  IF w>HIGH(d[0].txt) THEN w:=HIGH(d[0].txt) END;
  IF w<0 THEN RETURN END;
  w:=w+1-frame*2;
  FOR i:=frame TO h-frame DO
    low.cmove(ADR(d[i].txt),frame,ADR(s[i].txt),frame,w);
    low.move (ADR(d[i].atr)+frame,ADR(s[i].atr)+frame,w)
  END;
  IF frame=off THEN RETURN END;

  FOR i:=1 TO h-1 DO
    d[i].txt[0]:=s[i].txt[0]; d[i].txt[HIGH(d[i].txt)]:=s[i].txt[HIGH(s[i].txt)];
    d[i].atr[0]:=s[i].atr[0]; d[i].atr[HIGH(d[i].atr)]:=s[i].atr[HIGH(s[i].atr)]
  END;
  low.cmove(ADR(d[0].txt)      ,1,ADR(s[0].txt)      ,1,w-1);
  low. move(ADR(d[0].atr)      +1,ADR(s[0].atr)      +1,w-1);
  low.cmove(ADR(d[HIGH(d)].txt),1,ADR(s[HIGH(s)].txt),1,w-1);
  low. move(ADR(d[HIGH(d)].atr)+1,ADR(s[HIGH(s)].atr)+1,w-1);
  d[0].txt[HIGH(d[0].txt)]:=s[0].txt[HIGH(s[0].txt)];
  d[0].atr[HIGH(d[0].atr)]:=s[0].atr[HIGH(s[0].atr)];
  d[HIGH(d)].txt[HIGH(d[0].txt)]:=s[HIGH(s)].txt[HIGH(s[0].txt)];
  d[HIGH(d)].atr[HIGH(d[0].atr)]:=s[HIGH(s)].atr[HIGH(s[0].atr)];
  d[HIGH(d)].txt[0]:=s[HIGH(s)].txt[0];
  d[HIGH(d)].atr[0]:=s[HIGH(s)].atr[0]
END copy_screen;

PROCEDURE _new(VAR vp: WINDOW; under: WINDOW);
BEGIN
  NEW(vp);
  IF NOT mem.done THEN mem_error; RETURN END;
  WITH vp^ DO
    NEW(scr,0); ASSERT(mem.done);
    dw :=NIL;     open:=FALSE;
    up :=NIL;     free:=FALSE;
    stk:=NIL;
    WITH view DO
      l:=0;  h:=0;   wl:=0;
      c:=0;  w:=0;   wc:=0;
      awp   :=0;
      frame :=0;
      cursor:=1;   back:=tty.state^.min_color;
      manual:=0;   fore:=0;
      underline:=0;
      reverse  :=0;
      attr:=packed_attrs(vp);
    END
  END;
  IF all=NIL THEN
    vp^.dw:=vp;          vp^.up:=vp;        all:=vp
  ELSE
    vp^.up:=under^.up;   under^.up :=vp;
    vp^.dw:=under;       vp^.up^.dw:=vp
  END;
  vp^.magic:=MAGIC;
  done:=TRUE
END _new;

PROCEDURE new(VAR vp: WINDOW);
BEGIN _new(vp,all) END new;

PROCEDURE dispose(VAR vp: WINDOW);
BEGIN
  IF vp=NIL THEN done:=TRUE; RETURN END;
  ASSERT(vp^.magic=MAGIC,4Fh);
  IF vp=full  THEN bad_parm; RETURN END;
  IF vp^.open THEN close(vp) END;
  IF all=vp THEN all:=all^.dw END;
  IF all=vp THEN all:=NIL
  ELSE vp^.up^.dw:=vp^.dw;  vp^.dw^.up:=vp^.up
  END;
  dispose_screen(vp^.scr);
  vp^.magic:=0;             DISPOSE(vp);        done:=TRUE
END dispose;

PROCEDURE set_TOP;
BEGIN
  TOP:=all;
  IF all=NIL THEN RETURN END;
  REPEAT
    IF TOP^.open THEN RETURN END;
    TOP:=TOP^.dw
  UNTIL TOP=all;
  TOP:=NIL
END set_TOP;

PROCEDURE open(vp: WINDOW);
  VAR i: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  IF vp^.open THEN RETURN END;
  vp^.open:=TRUE;
  set_TOP;
  REFRESH(vp,0,0,vp^.view.w,vp^.view.h);
  hoverlap(vp);
  done:=TRUE
END open;

PROCEDURE close(vp: WINDOW);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  IF NOT vp^.open THEN RETURN END;
  vp^.open:=FALSE;
  set_TOP;
  CLEAR(vp,0,0,vp^.view.w,vp^.view.h);
  done:=TRUE
END close;

PROCEDURE closed(vp: WINDOW): BOOLEAN;
BEGIN (* NOT changed done *)
  RETURN (vp=NIL) OR NOT vp^.open
END closed;

PROCEDURE under(t,b: WINDOW): BOOLEAN;
BEGIN (* NOT changed done *)
  IF (t=NIL) OR (b=NIL) THEN RETURN FALSE END;
  ASSERT(t^.magic=MAGIC,4Fh);
  ASSERT(b^.magic=MAGIC,4Fh);
  ASSERT(all#NIL);
  IF t=all THEN RETURN b#all END;
  LOOP
    t:=t^.dw;
    IF t=all THEN RETURN FALSE END;
    IF b=t   THEN RETURN TRUE  END
  END
END under;

PROCEDURE top(vp: WINDOW): BOOLEAN;
BEGIN (* NOT changed done *)
  RETURN (vp=TOP) & (TOP#NIL)
END top;


PROCEDURE resize(vp: WINDOW; _w,_h: INTEGER);
  VAR s: SCREEN;
      i: INTEGER;
     l0: STRING;
     lh: STRING;
     li: STRING;
    crs: INTEGER;
    W,H: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  WITH vp^.view DO
    IF (_h=h) & (_w=w)       THEN done:=TRUE; RETURN END;
    IF (_h<0) OR (l+_h>maxH) THEN bad_parm; RETURN END;
    IF (_w<0) OR (c+_w>maxW) THEN bad_parm; RETURN END;
    IF (frame#off) & ((_h<3) OR (_w<3)) THEN bad_parm; RETURN END;

    IF vp^.open THEN
      crs:=cursor_off();
      H:=h;
      vp^.open:=FALSE; (* to force refresh underlaing windows *)
      IF _h<H THEN _clear(vp,all,TRUE,FALSE,l+_h,c,l+h,c+w); H:=_h END;
      IF _w<w THEN _clear(vp,all,TRUE,FALSE,l,c+_w,l+H,c+w)        END;
      vp^.open:=TRUE
    END;

    NEW(s);
    new_screen(s,_w,_h,vp^.attr);
    IF NOT done THEN RETURN END;

    W:=w;
    IF W>_w THEN W:=_w END;

    IF frame#off THEN _frame(s,frame) END;
    copy_screen(s,vp^.scr,frame);

    dispose_screen(vp^.scr);
    W:= w; H:= h;
    w:=_w; h:=_h;
    vp^.scr^:=s^;

    IF vp^.open THEN
      IF    H<h       THEN _refresh(vp,vp,l+H-frame,c,l+h,c+w)
      ELSIF frame#off THEN _refresh(vp,vp,l+h-1,c,l+h,c+w)
      END;
      IF    W<w       THEN _refresh(vp,vp,l,c+W-frame,l+H,c+w)
      ELSIF frame#off THEN _refresh(vp,vp,l,c+w-1,l+h-1,c+w)
      END;
      cursor_to_top(crs)
    END
  END;
  done:=TRUE
END resize;

PROCEDURE move(vp: WINDOW; _l,_c: INTEGER);
  VAR u: WINDOW;
    crs: INTEGER;
    sav: VIEW;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    IF (_l=l) & (_c=c)       THEN done:=TRUE;   RETURN END;
    IF (_l<0) OR (_l+h>maxH) THEN bad_parm;     RETURN END;
    IF (_c<0) OR (_c+w>maxW) THEN bad_parm;     RETURN END;
    IF NOT vp^.open          THEN l:=_l; c:=_c; RETURN END;
    _new(u,vp);
    IF NOT done THEN RETURN END;
    IF vp=all   THEN all:=u END;
    crs:=cursor_off();
    sav:=u^.view;   u^.view:=vp^.view;
    l:=_l;  c:=_c;  _refresh(vp,vp,l,c,l+h,c+w);
    WITH u^.view DO _clear(u,all,TRUE,FALSE,l,c,l+h,c+w) END;
    u^.view:=sav;   dispose(u);
    set_TOP;        cursor_to_top(crs)
  END
END move;

PROCEDURE atr_fill(VAR a: ARRAY OF BITSET; pos,len: INTEGER; filler: BITSET);
BEGIN
  low._fill(ADR(a[pos]),len,filler)
--FOR i:=pos TO pos+len-1 DO a[i]:=filler END
END atr_fill;

PROCEDURE xor_reverse(VAR a: ARRAY OF BITSET; pos,len: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=pos TO pos+len-1 DO a[i]:=a[i]/{16} END
END xor_reverse;

PROCEDURE frameprint(vp: WINDOW;
                  _l,_c: INTEGER;
                VAL frm: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR len: INTEGER;
      bump: ARRAY [0..127] OF CHAR;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    IF frame=off THEN RETURN END;
    IF frm="" THEN
      _frame(vp^.scr,on);
      IF NOT vp^.open THEN RETURN END;
      REFRESH(vp,0,0,w-1,1);   REFRESH(vp,0,0,1,h-1);
      REFRESH(vp,0,h-1,w-1,1); REFRESH(vp,w-1,0,1,h-1);  RETURN
    END;
    str.print(bump,frm,args);
    IF (_l<0) OR (_l>=h) THEN bad_parm; RETURN END;
    IF (_c<0) OR (_c>=w) THEN bad_parm; RETURN END;
    IF (_l>0) & (_l<h-1) THEN
      IF _c#0 THEN _c:=w-1 END;
      len:=1
    ELSE
      len:=str.len(bump)
    END;
    str.move(vp^.scr[_l].txt,_c,bump,0,len);
    atr_fill(vp^.scr[_l].atr,_c,len,vp^.attr);
    IF vp^.open THEN REFRESH(vp,_l,_c,len,1) END
  END
END frameprint;

PROCEDURE frame(vp: WINDOW; tog: INTEGER);
  VAR i: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    IF  frame=tog THEN RETURN  END;
    IF  tog#off   THEN tog:=on END;
    IF (tog#off) & ((w<3) OR (h<3)) THEN bad_parm; RETURN END;
    frame:=tog;
    IF tog#off THEN
      FOR i:=h-2 TO 1 BY -1 DO
        low.cmove(ADR(vp^.scr[i].txt),1,ADR(vp^.scr[i-1].txt),0,w-2);
        low. move(ADR(vp^.scr[i].atr)+1,ADR(vp^.scr[i-1].atr)+0,w-2)
      END;
      IF wl>h-3 THEN wl:=h-3 END;
      IF wc>w-3 THEN wc:=w-3 END;
      _frame(vp^.scr,on)
    ELSE
      _frame(vp^.scr,off);
      FOR i:=1 TO h-2 DO
        low.cmove(ADR(vp^.scr[i-1].txt),0,ADR(vp^.scr[i].txt),1,w-1);
        low. move(ADR(vp^.scr[i-1].atr)+0,ADR(vp^.scr[i].atr)+1,w-1)
      END;
      low.fill(vp^.scr[h-2].txt,SPACES); low.fill(vp^.scr[h-2].atr,vp^.attr);
      low.fill(vp^.scr[h-1].txt,SPACES); low.fill(vp^.scr[h-1].atr,vp^.attr)
    END;
    IF vp^.open THEN REFRESH(vp,0,0,w,h) END
  END
END frame;

PROCEDURE refresh(vp: WINDOW);
  VAR f: BOOLEAN;
BEGIN
  f:=freeze; freeze:=FALSE;
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  WITH vp^.view DO REFRESH(vp,0,0,w,h) END;
  freeze:=f;
  done:=TRUE
END refresh;

PROCEDURE refreshbox(vp: WINDOW; _l,_c,_w,_h: INTEGER);
  VAR f: BOOLEAN;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF (_w<=0) OR (_h<=0) THEN RETURN END;
  WITH vp^.view DO
    IF (_l<0) OR (_l+_h>h) THEN bad_parm; RETURN END;
    IF (_c<0) OR (_c+_w>w) THEN bad_parm; RETURN END;
    f:=freeze; freeze:=FALSE;
    REFRESH(vp,_l,_c,_w,_h);
    freeze:=f
  END
END refreshbox;

PROCEDURE refreshall;
  VAR v: WINDOW;
      f: BOOLEAN;
    crs: INTEGER;
BEGIN
  f:=freeze; freeze:=FALSE;
  crs:=cursor_off();
  done:=TRUE;
  v:=all;
  WHILE v#NIL DO
    IF v^.open THEN
      WITH v^.view DO REFRESH(v,0,0,w,h) END
    END;
    v:=v^.dw;
    IF v=all THEN cursor_to_top(crs); freeze:=f; RETURN END
  END;
  freeze:=f
END refreshall;

PROCEDURE cross(l0,c0,w0,h0,l1,c1,w1,h1: INTEGER;
                        VAR l2,c2,w2,h2: INTEGER): BOOLEAN;
BEGIN
  l2:=l0; (* l2:=max(l0,l1) *)
  IF l2<l1 THEN l2:=l1 END;
  c2:=c0; (* c2:=max(c0,c1) *)
  IF c2<c1 THEN c2:=c1 END;
  h2:=l0+h0-l2; (* l2+h2=min(l0+h0,l1+h1) *)
  IF h2>l1+h1-l2 THEN h2:=l1+h1-l2 END;
  w2:=c0+w0-c2; (* c2+w2=min(c0+w0,c1+w1) *)
  IF w2>c1+c1-c2 THEN w2:=c1+w1-c2 END;
  RETURN (h2>0) & (w2>0)
END cross;

PROCEDURE refresharea(_l,_c,_w,_h: INTEGER);
  VAR v: WINDOW;   l1,c1: INTEGER;
      f: BOOLEAN;  w1,h1: INTEGER;
    crs: INTEGER;
BEGIN
  f:=freeze; freeze:=FALSE;
  crs:=cursor_off();
  done:=TRUE;
  v:=all;
  WHILE v#NIL DO
    IF v^.open THEN
      WITH v^.view DO
        IF cross(l,c,w,h,_l,_c,_w,_h,l1,c1,w1,h1) THEN
          REFRESH(v,l1-l,c1-c,w1,h1)
        END
      END
    END;
    v:=v^.dw;
    IF v=all THEN cursor_to_top(crs); freeze:=f; RETURN END
  END;
  freeze:=f
END refresharea;

PROCEDURE ontop(vp: WINDOW);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  ASSERT(all#NIL);
  IF all=vp THEN done:=TRUE; RETURN END;
  (* untie *)
  vp^.up^.dw:=vp^.dw;   vp^.dw^.up:=vp^.up;
  (* retie *)
  vp^.dw:=all;          vp^.up:=all^.up;
  all^.up:=vp;          vp^.up^.dw:=vp;
  all:=vp;
  IF vp^.open THEN set_TOP; REFRESH(vp,0,0,vp^.view.w,vp^.view.h) END;
  hoverlap(vp);
  done:=TRUE
END ontop;

PROCEDURE onbottom(vp: WINDOW);
  VAR opn: BOOLEAN;
      crs: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  ASSERT(all#NIL);
  IF all^.dw=all THEN RETURN END;
  opn:=vp^.open;
  IF opn THEN
    vp^.open:=FALSE; set_TOP;
    crs:=cursor_off();
    WITH vp^.view DO
      _clear(vp,vp,FALSE,FALSE,l,c,l+h,c+w)  (* vp,all ??? *)
    END;
    cursor_to_top(crs)
  END;
  IF all=vp   THEN all:=all^.dw END;
  (* untie *)
  vp^.up^.dw:=vp^.dw;   vp^.dw^.up:=vp^.up;
  (* retie *)
  vp^.up:=all^.up;   all^.up:=vp;
  vp^.dw:=all;       vp^.up^.dw:=vp;
  IF opn THEN
    vp^.open:=TRUE; set_TOP
    (*; REFRESH(vp,0,0,vp^.view.w,vp^.view.h)??? deleted 15-Oct-90 *)
  END;
  hoverlap(vp);
  done:=TRUE
END onbottom;

PROCEDURE get(vp: WINDOW; VAR info: INFO);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  info:=ADR(vp^.view);
  done:=TRUE
END get;

PROCEDURE locate(_l,_c: INTEGER; VAR vp: WINDOW);
BEGIN
  done:=TRUE;
  vp  :=TOP;
  IF vp#NIL THEN
    REPEAT
      IF vp^.open THEN
        WITH vp^.view DO
          IF (_l>=l) & (_l<l+h) & (_c>=c) & (_c<c+w) THEN RETURN END
        END
      END;
      vp:=vp^.dw
    UNTIL vp=all
  END;
  vp:=NIL; done:=FALSE; error:=err.no_entry
END locate;

PROCEDURE push(vp: WINDOW);
  VAR vl: VIEWLIST;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  NEW(vl);
  IF NOT mem.done THEN mem_error; RETURN END;
  vl^.view:=vp^.view;
  vl^.next:=vp^.stk;
  vp^.stk :=vl^.next;
  done:=TRUE
END push;

PROCEDURE pop(vp: WINDOW);
  VAR v: VIEWLIST;
    crs: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  v:=vp^.stk;
  IF v=NIL THEN done:=FALSE; error:=err.inv_op; RETURN END;
  vp^.stk:=v^.next;
  WITH vp^.view DO
    IF vp=TOP THEN crs:=cursor_off() END;
    wl :=v^.view.wl;    fore:=v^.view.fore;
    wc :=v^.view.wc;    back:=v^.view.back;
    awp:=v^.view.awp;
    cursor:=v^.view.cursor;
    IF wl<h-frame*2 THEN wl:=h-frame*2-1 END;
    IF wc<w-frame*2 THEN wc:=w-frame*2-1 END;
    IF vp=TOP THEN cursor_to_top(cursor) END
  END;
  DISPOSE(v)
END pop;

PROCEDURE era(vp: WINDOW; n: INTEGER);
  VAR i,l0,l1,p,len,W: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    W:=w-frame*2;
    CASE n OF
    |1:  l0:=frame;      l1:=wl-1+frame;  p:=frame;    len:=wc+1
    |2:  l0:=frame;      l1:=h-1-frame;   p:=1;        len:=0
    ELSE l0:=wl+1+frame; l1:=h-1-frame;   p:=wc+frame; len:=w-frame*2-wc
    END;
    FOR i:=l0 TO l1 DO
      low.cmove(ADR(vp^.scr[i].txt),frame,adr(spaces),frame,W);
      atr_fill(vp^.scr[i].atr,frame,W,vp^.attr)
    END;
    low.cmove(ADR(vp^.scr[wl+frame].txt),p,adr(spaces),p,len);
    atr_fill(vp^.scr[wl+frame].atr,p,len,vp^.attr);
    IF NOT vp^.open OR (manual#off) THEN RETURN END;

    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      tty.erase(n);
      IF tty.done THEN RETURN END
    END;
    IF vp^.free THEN
      _erase(vp,l0,0,w-frame*2,l1-l0+1);
      _erase(vp,wl+frame,p,len,1);  RETURN
    END;
    IF wl+frame<l0 THEN l0:=wl+frame END;
    IF wl+frame>l1 THEN l1:=wl+frame END;
    REFRESH(vp,l0,frame,w-frame*2,l1-l0+1)
  END
END era;

PROCEDURE eraln(vp: WINDOW; n: INTEGER);
  VAR p,len: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    CASE n OF
    |1:  p:=frame;      len:=wc+1
    |2:  p:=frame;      len:=w-frame*2
    ELSE p:=wc+frame;   len:=w-frame*2-wc
    END;
    low.cmove(ADR(vp^.scr[wl+frame].txt),p,adr(spaces),p,len);
    atr_fill(vp^.scr[wl+frame].atr,p,len,vp^.attr);
    IF NOT vp^.open OR (manual#off) THEN RETURN END;

    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      tty.erase_line(n);
      IF tty.done THEN RETURN END
    END;
    IF vp^.free THEN _erase(vp,wl+frame,p,len,1); RETURN END;
    REFRESH(vp,wl+frame,p,len,1)
  END
END eraln;

PROCEDURE _il(vp: WINDOW; ln,n: INTEGER);
  VAR i,l0,l1: INTEGER;
BEGIN
  WITH vp^.view DO
    IF ln+n>=h-frame*2 THEN n:=h-frame*2-ln END;
    l0:=ln+frame;
    l1:=h-1-frame;
    FOR i:=l1 TO l0+n BY -1 DO
      vp^.scr[i].txt:=vp^.scr[i-n].txt;
      vp^.scr[i].atr:=vp^.scr[i-n].atr
    END;
    FOR i:=l0 TO l0+n-1 DO
      low.cmove(ADR(vp^.scr[i].txt),frame,adr(spaces),frame,w-frame*2);
      atr_fill(vp^.scr[i].atr,frame,w-frame*2,vp^.attr)
    END
  END
END _il;

PROCEDURE il(vp: WINDOW; n: INTEGER);
  VAR i: INTEGER;  MARK: BOOLEAN;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    IF n=0 THEN RETURN END;
    IF n<0 THEN bad_parm;  RETURN END;
    IF wl+n>=h-frame*2 THEN
      i:=wc; wc:=0; era(vp,0); wc:=i; RETURN
    END;
    _il(vp,wl,n);
    IF NOT vp^.open OR (manual#off) THEN RETURN END;

    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      i:=cursor_off();
      set_tty_attrs(vp^.attr);
      tty.ins_line(n);
      cursor_to_top(i);
      RETURN
    END;
    IF NOT freeze & vp^.free THEN
      set_tty_attrs(vp^.attr);
      i:=cursor_off();
      tty.set_pos(l+h-frame*2-(n-1),0);  tty.del_line(n);
      tty.set_pos(l+wl+frame,0);         tty.ins_line(n);
      REFRESH(vp,wl+frame,0,w,n);    (* frame refreshed too *)
      RETURN
    END;
    REFRESH(vp,wl+frame,frame,w-frame*2,h-frame*2-wl)
  END
END il;

PROCEDURE _dl(vp: WINDOW; ln,n: INTEGER);
  VAR i,l0,l1: INTEGER;
BEGIN
  WITH vp^.view DO
    IF ln+n>=h-frame*2 THEN n:=h-frame*2-ln END;
    l0:=ln+frame;
    l1:=h-1-frame;
    FOR i:=l0 TO l1-n DO
      vp^.scr[i].txt:=vp^.scr[i+n].txt;
      vp^.scr[i].atr:=vp^.scr[i+n].atr
    END;
    FOR i:=l1-n+1 TO l1 DO
      low.cmove(ADR(vp^.scr[i].txt),frame,adr(spaces),frame,w-frame*2);
      atr_fill(vp^.scr[i].atr,frame,w-frame*2,vp^.attr)
    END
  END
END _dl;

PROCEDURE dl(vp: WINDOW; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  WITH vp^.view DO
    IF n=0 THEN RETURN END;
    IF n<0 THEN bad_parm;  RETURN END;
    IF wl+n>=h-frame*2 THEN
      i:=wc; wc:=0; era(vp,0); wc:=i; RETURN
    END;
    _dl(vp,wl,n);
    IF NOT vp^.open OR (manual#off) THEN RETURN END;

    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      i:=cursor_off();
      tty.set_pos(l+wl+frame,0);  tty.del_line(n);
      cursor_to_top(i);
      RETURN
    END;
    IF NOT freeze & vp^.free THEN
      set_tty_attrs(vp^.attr);
      i:=cursor_off();
      tty.set_pos(l+wl+frame,0);         tty.del_line(n);
      tty.set_pos(l+h-frame*2-(n-1),0);  tty.ins_line(n);
      REFRESH(vp,h-frame*2-(n-1),0,w,n); (* frame refreshed too *)
      RETURN
    END;
    REFRESH(vp,wl+frame,frame,w-frame*2,h-frame*2-wl)
  END
END dl;

PROCEDURE dc(vp: WINDOW; n: INTEGER);
  VAR W: INTEGER;
    crs: INTEGER;
    ptr: SYSTEM.ADDRESS;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF n<=0 THEN RETURN END;
  WITH vp^.view DO
    W:=w-frame*2;
    IF wc+n>=W THEN eraln(vp,0); RETURN END;
    ptr:=ADR(vp^.scr[wl+frame].txt);
    low.cmove(ptr,wc+frame,ptr,wc+frame+n,W-wc-n);
    low.cmove(ptr,W-n+frame,adr(spaces),0,n);
    atr_fill(vp^.scr[wl+frame].atr,W-n+frame,n,vp^.attr);
    IF NOT vp^.open OR (manual#off) THEN RETURN END;

    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();      tty.set_pos(l+wl+frame,c+wc+frame);
      tty.del_char(n);        cursor_to_top(crs);
      RETURN
    END;

    IF NOT freeze & (vp^.free OR (vp=TOP) & (w=maxW)) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();
      tty.set_pos(l+wl+frame,c+wc+frame);     tty.del_char(n);
      IF frame#0 THEN
        tty.set_pos(l+wl+frame,c+w-frame-n);  tty.ins_char(n)
      END;
      IF (tty.state^.type=92) & (n>1) THEN (* ins_char has hardware error *)
        REFRESH(vp,wl+frame,w-frame-n,1,1)
      END;
      cursor_to_top(crs);                  RETURN
    END;

    REFRESH(vp,wl+frame,wc+frame,W-wc,1)
  END
END dc;

PROCEDURE ic(vp: WINDOW; n: INTEGER);
  VAR W: INTEGER;
    crs: INTEGER;
    ptr: SYSTEM.ADDRESS;
    bmp: SYSTEM.ADDRESS;
   bump: ARRAY [0..127] OF CHAR;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF n<=0 THEN RETURN END;
  WITH vp^.view DO
    W:=w-frame*2;
    IF wc+n>=W THEN eraln(vp,0); RETURN END;
    ptr:=ADR(vp^.scr[wl+frame].txt);
    bmp:=ADR(bump);
    low.cmove(bmp,0,adr(spaces),0,n);
    low.cmove(bmp,n,ptr,wc+frame,W-wc-n);
    low.cmove(ptr,wc+frame,bmp,0,W-wc);
    atr_fill(vp^.scr[wl+frame].atr,wc+frame,n,vp^.attr);
    IF NOT vp^.open OR (manual#off) THEN RETURN END;

    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();      tty.set_pos(l+wl+frame,c+wc+frame);
      tty.ins_char(n);
      IF (tty.state^.type=92) & (n>1) THEN (* ins_char has hardware error *)
        REFRESH(vp,wl,wc+n,1,1)
      END;
      cursor_to_top(crs);
      RETURN
    END;


    IF NOT freeze & (vp^.free OR (vp=TOP) & (w=maxW)) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();
      IF frame=0 THEN
        tty.set_pos(l+wl+frame,c+wc+frame);   tty.ins_char(n);
        IF (tty.state^.type=92) & (n>1) THEN (* ins_char has hardware error *)
          REFRESH(vp,wl,wc+n,1,1)
        END
      ELSE
        tty.set_pos(l+wl+frame,c+w-frame-n);  tty.del_char(n);
        tty.set_pos(l+wl+frame,c+wc+frame);   tty.ins_char(n);
        IF (tty.state^.type=92) & (n>1) THEN (* ins_char has hardware error *)
          REFRESH(vp,wl+frame,wc+frame+n,1,1)
        END
      END;
      cursor_to_top(crs);                   RETURN
    END;

    REFRESH(vp,wl+frame,wc+frame,W-wc,1)
  END
END ic;

PROCEDURE roll(vp: WINDOW; n: INTEGER); (* n>0 up; n<0 down *)
  VAR crs: INTEGER;
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF n=0 THEN RETURN END;
  IF n<0 THEN _il(vp,0,-n) ELSE _dl(vp,0,n) END;
  WITH vp^.view DO
    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();
      IF n<0 THEN tty.scroll_down(-n) ELSE tty.scroll_up(+n) END;
      IF tty.done THEN cursor_to_top(crs); RETURN END;
      IF n<0 THEN tty.roll_down(-n)   ELSE tty.roll_up(+n) END;
      cursor_to_top(crs);  RETURN
    END;
    IF NOT freeze & (vp^.free OR (vp=TOP) & (w=maxW)) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();
      IF n>0 THEN
        tty.set_pos(l+frame,0);            tty.del_line(n);
        tty.set_pos(l+h-frame*2-(n-1),0);  tty.ins_line(n);
        REFRESH(vp,h-frame*2-(n-1),0,w,n)
      ELSE
        n:=-n;
        tty.set_pos(l+h-frame*2-(n-1),0);  tty.del_line(n);
        tty.set_pos(l+frame,0);            tty.ins_line(n);
        REFRESH(vp,frame,0,w,n)
      END;
      RETURN
    END;
    REFRESH(vp,frame,frame,w-frame*2,h-frame*2)
  END
END roll;

PROCEDURE writech(vp: WINDOW; ch: CHAR);
  VAR s: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN s:=ADR(ch); write(vp,s^,0,1) END writech;

PROCEDURE write(vp: WINDOW; VAL str: ARRAY OF CHAR; pos,len: INTEGER);
  VAR ch: CHAR;
     crs: INTEGER;
     lfs: INTEGER;
   U,L,R: INTEGER;

  PROCEDURE lf;
  BEGIN
    WITH vp^.view DO
      IF wl<h-frame*2-1 THEN INC(wl); RETURN END;
      _dl(vp,0,1); INC(lfs)
    END
  END lf;

  PROCEDURE cr;
  BEGIN
    WITH vp^.view DO
      IF R<wc THEN R:=wc END; wc:=0
    END
  END cr;

BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  lfs :=0;
  WITH vp^.view DO
    U:=wl;
    L:=wc;
    R:=wc;
    LOOP
      IF (pos>HIGH(str)) OR (len=0) THEN EXIT END;
      ch:=str[pos];
      IF ch>=40c THEN
        IF L>wc THEN L:=wc END;
        vp^.scr[wl+frame].txt[wc+frame]:=ch;
        vp^.scr[wl+frame].atr[wc+frame]:=vp^.attr;
        INC(wc);
        IF R<wc THEN R:=wc END;
        IF wc>=w-frame*2 THEN
          IF awp=0 THEN wc:=w-frame*2-1
          ELSE cr; lf
          END
        END
      ELSIF ch=36c  THEN cr; lf
      ELSIF ch=15c  THEN cr
      ELSIF ch=12c  THEN lf
      END;
      INC(pos); DEC(len)
    END;
    IF NOT vp^.open OR (manual#off) THEN RETURN END;
    IF lfs=0 THEN REFRESH(vp,U+frame,L+frame,R-L,wl-U+1); RETURN END;
    IF NOT freeze & (vp=TOP) & (w=maxW) & (h=maxH) & (frame=0) THEN
      set_tty_attrs(vp^.attr);
      crs:=cursor_off();
      tty.scroll_up(lfs);
      IF NOT tty.done THEN tty.roll_up(lfs) END;
      U:=h-lfs-1;
      IF U<0 THEN U:=0 END;
      REFRESH(vp,U,L,R-L,wl-U+1);
      RETURN
    END;
    IF NOT freeze & vp^.free & (lfs<h*2 DIV 3) THEN
      crs:=cursor_off();
      set_tty_attrs(vp^.attr);
      tty.set_pos(l+frame,0);          tty.del_line(lfs);
      tty.set_pos(l+h-1-frame,0);      tty.ins_line(lfs);
      U:=U-lfs;
      REFRESH(vp,U+frame,0,w,wl-U+1);  RETURN
    END;
    U:=0; L:=0; R:=w-frame*2;
    REFRESH(vp,U+frame,L+frame,R-L,wl-U+1)
  END
END write;

PROCEDURE pwrite(vp: SYSTEM.WORD; VAL str: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  write(vp,str,pos,len)
END pwrite;

PROCEDURE print(vp: WINDOW; VAL format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  fmt.format(vp,pwrite,format,args)
END print;

PROCEDURE perror(vp: WINDOW; code: INTEGER;
              VAL f: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR s: ARRAY [0..127] OF CHAR;
BEGIN
  s:="";
  lex.perror(s,code,f,args);
  print(vp,"%s",s);
  IF NOT done THEN RETURN END;
  done:=lex.done;
  IF NOT done THEN error:=lex.error END
END perror;

PROCEDURE read(vp: WINDOW; VAR str: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  WITH vp^.view DO
    IF pos+len>BYTES(str) THEN len:=BYTES(str)-pos END;
    IF len>w-frame*2-wc   THEN len:=w-frame*2-wc   END;
    low.cmove(ADR(str),pos,ADR(vp^.scr[wl].txt),wc,len)
  END;
  done:=TRUE
END read;

PROCEDURE manual(vp: WINDOW; tog: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF tog#off THEN vp^.view.manual:=on ELSE vp^.view.manual:=off END
END manual;

PROCEDURE autowrap(vp: WINDOW; tog: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF tog#off THEN vp^.view.awp:=on ELSE vp^.view.awp:=off END
END autowrap;


PROCEDURE foreground(vp: WINDOW; color: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  vp^.view.fore:=color; vp^.attr:=packed_attrs(vp)
END foreground;

PROCEDURE background(vp: WINDOW; color: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  vp^.view.back:=color; vp^.attr:=packed_attrs(vp)
END background;

PROCEDURE reverse(vp: WINDOW; on: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  vp^.view.reverse:=on MOD 2; vp^.attr:=packed_attrs(vp)
END reverse;

PROCEDURE underline(vp: WINDOW; on: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  done:=TRUE;
  vp^.view.underline:=on MOD 2; vp^.attr:=packed_attrs(vp)
END underline;

PROCEDURE cursor(vp: WINDOW; tog: INTEGER);
BEGIN
  ASSERT((vp#NIL) & (vp^.magic=MAGIC),4Fh);
  IF vp#TOP THEN vp^.view.cursor:=tog; RETURN END;
  vp^.view.cursor:=tog;  tty.set_cursor(tog);
  done:=TRUE
END cursor;

PROCEDURE init;
  PROCEDURE check;
  BEGIN
    IF NOT tty.done THEN HALT(err.unsuitable) END
  END check;
BEGIN
  tty.set_awp   (0); ASSERT(tty.done,err.unsuitable);
  tty.set_cursor(0); ASSERT(tty.done,err.unsuitable);
  tty.erase(2);      ASSERT(tty.done,err.unsuitable);
  MAGIC:=57777474h;
  done:=TRUE;  error:=err.ok;
  full:=NIL;
  null:=NIL;
  all :=NIL;
  TOP :=NIL;
  maxW:=tty.state^.columns;  tty.set_pos(maxW,0);
  maxH:=tty.state^.lines;    bars[0]:=tty.state^.bars[0];
  hbar:=tty.state^.hbar;     bars[1]:=tty.state^.bars[1];
  vbar:=tty.state^.vbar;     bars[2]:=tty.state^.bars[2];
  HBAR:=INTEGER(BITSET(hbar<<8)+BITSET(hbar<<16)+BITSET(hbar>>8)+BITSET(hbar));
  freeze:=FALSE;

  tty.erase_chars(2);  check;
  tty.ins_line(1);     check;
  tty.del_line(1);     check;
  tty.ins_char(1);     check;
  tty.del_char(1);     check;
  tty.set_reverse(1);  check;
  tty.set_reverse(0);  check;

  new(full);
  IF NOT done THEN RETURN END;
  resize(full,maxW,maxH)
END init;

BEGIN
  init
END ttyWindows.
