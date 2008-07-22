IMPLEMENTATION MODULE kcScreens; (*  25-Oct-90. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  tty: Terminal,
        str: Strings,
        mem: Heap;

WITH STORAGE (NEW    : mem.ALLOCATE;
              DISPOSE: mem.DEALLOCATE;
              RESIZE : mem.REALLOCATE);




TYPE SCREEN = POINTER TO SCR;
     SCR   = RECORD
                  x,y,w,h,
                    x1,y1: INTEGER;
                prev,next: SCREEN;
                 line,col: INTEGER;
                   off,in: BOOLEAN;
                    liter: DYNARR OF STRING;
                    attrs: DYNARR OF STRING
              END;


VAR top_scr,bot_scr: SCREEN;

VAR tmp: ARRAY [0..83] OF CHAR;
    Line,Col,lines,columns,color,back,reverse,mic,mac: INTEGER;

VAR hb,vb,ul,ur,dl,dr: CHAR;

PROCEDURE min(a,b: INTEGER): INTEGER;
BEGIN
  IF a<b THEN RETURN a ELSE RETURN b END
END min;

PROCEDURE max(a,b: INTEGER): INTEGER;
BEGIN
  IF a>b THEN RETURN a ELSE RETURN b END
END max;

PROCEDURE diap(x,a,b: INTEGER): INTEGER;
BEGIN
  RETURN min(max(x,a),b)
END diap;

PROCEDURE tty_repeat(ch: CHAR; t: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF t>0 THEN
    FOR i:=0 TO t-1 DO tmp[i]:=ch END;
    tmp[t]:=0c;
    tty.write(tmp,0,t)
  END;
END tty_repeat;

PROCEDURE sr(i: INTEGER);
BEGIN
  IF i#state^.reverse THEN tty.set_reverse(i) END
END sr;

PROCEDURE sc(i: INTEGER);
  VAR j: INTEGER;
BEGIN
  j:=state^.reverse;
  IF i#state^.color THEN tty.set_color(i) END;
  IF j#0 THEN tty.set_reverse(1) END
END sc;

PROCEDURE sb(i: INTEGER);
BEGIN
  IF mac-mic=1 THEN
    IF i#mic THEN i:=1 ELSE i:=0 END;
    IF i#state^.underline THEN tty.set_underline(i) END;
  ELSIF i#state^.back THEN tty.set_back(i)
  END;
END sb;

PROCEDURE set_reverse(i: INTEGER); BEGIN reverse:=i END set_reverse;
PROCEDURE set_color  (i: INTEGER); BEGIN color  :=i END set_color;
PROCEDURE set_back   (i: INTEGER); BEGIN back   :=i END set_back;

PROCEDURE set_cursor(i: INTEGER);
BEGIN
  tty.set_cursor(i)
END set_cursor;

PROCEDURE tty_state(): CHAR;
  VAR s: INTEGER;
BEGIN
   s:=color-mic+(back-mic)*16+reverse*8;
  RETURN CHAR(s)
END tty_state;

PROCEDURE hidden(scr: SCREEN; x,y: INTEGER; VAR x1: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF scr=NIL THEN x1:=columns; RETURN FALSE END;
  x1:=scr^.x1; scr:=scr^.prev;
  WHILE scr#NIL DO
    IF (y>=scr^.y)&(y<scr^.y1) THEN
      IF x>=scr^.x THEN
        IF x<scr^.x1 THEN x1:=min(scr^.x1,x1); RETURN TRUE
        END;
      ELSE x1:=min(x1,scr^.x)
      END;
    END;
    scr:=scr^.prev
  END;
  RETURN FALSE
END hidden;

PROCEDURE first_under(scr: SCREEN; x,y: INTEGER; VAR x1: INTEGER): SCREEN;
  VAR i,n: INTEGER;
BEGIN
  IF scr#NIL THEN
    x1:=scr^.x1; scr:=scr^.next
  ELSE x1:=columns; scr:=top_scr
  END;
  WHILE scr#NIL DO
    IF (y>=scr^.y)&(y<scr^.y1) THEN
      IF x>=scr^.x THEN
        IF x<scr^.x1 THEN x1:=min(x1,scr^.x1); RETURN scr END
      ELSE x1:=min(x1,scr^.x)
      END;
    END;
    scr:=scr^.next
  END;
END first_under;

PROCEDURE set_tty(scr: SCREEN; ln,cl: INTEGER);
  VAR a: BITSET; f,b,t: INTEGER;
BEGIN
  a:=BITSET(ORD(scr^.attrs[ln][cl]));
  f:=(INTEGER(a) MOD 8)+mic;
  b:=(INTEGER(a) DIV 16)+mic;
  IF 3 IN a THEN sr(1) ELSE sr(0) END;
  sc(f); sb(b);
END set_tty;

PROCEDURE tty_pos(L,C: INTEGER);
BEGIN
  tty.set_pos(L,C);
(*
  IF Line#L THEN tty.set_pos(L,C)
  ELSIF Col#C THEN
    IF Col<C THEN tty.right(C-Col) ELSE tty.left(C-Col) END;
  END;
*)
  Col:=C; Line:=L
END tty_pos;

PROCEDURE show(scr: SCREEN; c,l,len: INTEGER);
  VAR c0,c1: INTEGER; a: CHAR;

  PROCEDURE wr;
    VAR L,C: INTEGER;
  BEGIN
    WITH scr^ DO
      set_tty(scr,l,c0); L:=y+l; C:=x+c0;
      tty_pos(L,C); tty.write(scr^.liter[l],c0,c-c0);
      INC(Col,c-c0)
    END;
  END wr;

BEGIN
  WITH scr^ DO
    c0:=c;
    c1:=c+len;
    INC(c);
    a:=attrs[l][c0];
    WHILE c<c1 DO
      IF a#attrs[l][c] THEN
        wr; a:=attrs[l][c]; c0:=c
      END;
      INC(c)
    END;
    IF c0#c THEN wr END;
  END;
END show;

PROCEDURE restore_line(scr: SCREEN; i: INTEGER);
  VAR x,y,xv,xn,x1,xf,yf: INTEGER;
                 f_scr: SCREEN;
BEGIN
  IF scr#NIL THEN
    x:=scr^.x; x1:=scr^.x1; y:=scr^.y+i
  ELSE
    x:=0; x1:=columns; y:=i
  END;
  LOOP
    WHILE (x<x1) & hidden(scr,x,y,xv) DO x:=xv END;
    IF x=x1 THEN RETURN
    ELSE
      WHILE x<xv DO
        f_scr:=first_under(scr,x,y,xn);
        xf:=x-f_scr^.x;
        yf:=y-f_scr^.y;
        xn:=min(xn,xv);
        show(f_scr,xf,yf,xn-x);
        x:=xn
      END;
    END;
  END;
END restore_line;

PROCEDURE delete(scr: SCREEN);
  VAR i: INTEGER;
BEGIN
  IF scr#TTY THEN
    WITH scr^ DO
      IF off THEN RETURN END; off:=TRUE; in:=FALSE;
      FOR i:=0 TO h-1 DO restore_line(scr,i) END;
      IF prev#NIL THEN prev^.next:=next ELSE top_scr:=next END;
      IF next#NIL THEN next^.prev:=prev ELSE bot_scr:=prev END;
      next:=NIL; prev:=NIL
    END;
  END;
END delete;

PROCEDURE refresh_line(scr: SCREEN; l: INTEGER);
  VAR x,y,xv,x1: INTEGER;
BEGIN
  x:=scr^.x; x1:=scr^.x1; y:=scr^.y+l;
  LOOP
    WHILE (x<x1) & hidden(scr,x,y,xv) DO x:=xv END;
    IF x=x1 THEN RETURN
    ELSE
      show(scr,x-scr^.x,l,xv-x); x:=xv
    END;
  END;
END refresh_line;

PROCEDURE refresh(scr: SCREEN);
  VAR i: INTEGER;
BEGIN
  WITH scr^ DO
    IF off THEN RETURN
    ELSE
      FOR i:=0 TO h-1 DO refresh_line(scr,i) END
    END;
  END;
END refresh;

PROCEDURE create(VAR scr: SCREEN; yy,xx,hh,ww: INTEGER);
  VAR i,j: INTEGER; st: CHAR;
BEGIN
  NEW(scr); st:=tty_state();
  WITH scr^ DO
    x:=xx; y:=yy; x1:=xx+ww; NEW(liter,hh);
    w:=ww; h:=hh; y1:=yy+hh; NEW(attrs,hh);
    next:=NIL; line:=0;
    prev:=NIL; col :=0;
    off:=TRUE; in:=FALSE;
    FOR i:=0 TO hh-1 DO
      NEW(liter[i],ww);
      NEW(attrs[i],ww);
      FOR j:=0 TO ww-1 DO
        liter[i][j]:=' ';
        attrs[i][j]:=st
      END;
    END;
  END;
END create;

PROCEDURE ontop_line(scr: SCREEN; l: INTEGER);
  VAR x,y,xv,x1: INTEGER;
BEGIN
  x:=scr^.x; x1:=scr^.x1; y:=scr^.y+l;
  WHILE x<x1 DO
    IF hidden(scr,x,y,xv) THEN
      show(scr,x-scr^.x,l,min(xv,x1)-x)
    END;
    x:=xv
  END;
END ontop_line;

PROCEDURE on_top(scr: SCREEN);
  VAR i: INTEGER;
BEGIN
  IF scr=TTY THEN RETURN END;
  WITH scr^ DO
    IF in THEN
      IF off OR (scr=top_scr) THEN RETURN
      ELSE
        FOR i:=0 TO h-1 DO ontop_line(scr,i) END;
      END;
      IF    next#NIL THEN    next^.prev:=prev END;
      IF top_scr#NIL THEN top_scr^.prev:=scr  END;
      prev^.next:=next; prev:=NIL; next:=top_scr;
      top_scr:=scr
    ELSE
      next:=top_scr; next^.prev:=scr;
      prev:=NIL; top_scr:=scr;
      IF bot_scr=NIL THEN bot_scr:=scr END;
      off:=FALSE; in:=TRUE;
      refresh(scr)
    END;
  END;
END on_top;

PROCEDURE onbot_line(scr: SCREEN; l: INTEGER);
  VAR x,y,xv,xh,x1: INTEGER; usc: SCREEN;
BEGIN
  x:=scr^.x; x1:=scr^.x1; y:=scr^.y+l;
  WHILE x<x1 DO
    IF NOT hidden(scr,x,y,xh) THEN
      usc:=first_under(scr,x,y,xv); xv:=min(xv,xh);
      IF usc#TTY THEN
        show(usc,x-usc^.x,y-usc^.y,min(xv,x1)-x)
      END;
      x:=xv
    ELSE x:=xh
    END;
  END;
END onbot_line;

PROCEDURE on_bottom(scr: SCREEN);
  VAR i: INTEGER;
BEGIN
  IF scr=TTY THEN RETURN END;
  WITH scr^ DO
    IF in THEN
      IF off OR (scr^.next=TTY) THEN RETURN
      ELSE
        FOR i:=0 TO h-1 DO onbot_line(scr,i) END;
      END;
      IF scr=top_scr THEN top_scr:=next END;
      IF prev#NIL THEN prev^.next:=next END;
      next^.prev:=prev;
      prev:=TTY^.prev; prev^.next:=scr;
      TTY^.prev:=scr;  next:=TTY;
    ELSE
      IF top_scr=TTY THEN top_scr:=scr END;
      next:=TTY; prev:=TTY^.prev;
      IF prev#NIL THEN prev^.next:=scr END;
      TTY^.prev:=scr;
      off:=FALSE; in:=TRUE;
      refresh(scr)
    END;
  END;
END on_bottom;

PROCEDURE on(s: SCREEN);
BEGIN
  IF s^.in THEN s^.off:=FALSE END
END on;

PROCEDURE off(s: SCREEN); BEGIN s^.off:=TRUE END off;

PROCEDURE kill(scr: SCREEN);
  VAR i: INTEGER;
BEGIN
  delete(scr);
  WITH scr^ DO
    FOR i:=0 TO h-1 DO
      DISPOSE(liter[i]);
      DISPOSE(attrs[i])
    END;
    DISPOSE(liter);
    DISPOSE(attrs);
  END;
  DISPOSE(scr);
END kill;

PROCEDURE move(scr: SCREEN; l,c: INTEGER);
  VAR p,n: SCREEN;
BEGIN
  WITH scr^ DO
    p:=prev;
    n:=next;
    delete(scr);
    x:=c; x1:=c+w;
    y:=l; y1:=l+h;
    prev:=p;
    next:=n;
    in:=TRUE; off:=FALSE;
  END;
  IF p#NIL THEN p^.next:=scr END; n^.prev:=scr;
  refresh(scr)
END move;

PROCEDURE set_pos(scr: SCREEN; l,c: INTEGER);
BEGIN
  WITH scr^ DO
    line:=diap(l,0,h-1);
    col :=diap(c,0,w-1)
  END;
END set_pos;

PROCEDURE write(scr: SCREEN; s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR i,x0,x2,xv: INTEGER; ch: CHAR;
BEGIN
  WITH scr^ DO
    len:=min(len,w-col);
    ch:=tty_state();
    FOR i:=0 TO len-1 DO
      liter[line][col+i]:=s[pos+i];
      attrs[line][col+i]:=ch
    END;
    IF off THEN col:=min(w-1,col+len); RETURN END;
    x0:=x+col;
    x2:=x0+len;
    WHILE x0<x2 DO
      IF hidden(scr,x0,y+line,xv) THEN x0:=xv
      ELSE
        xv:=min(x2,xv);
        show(scr,x0-x,line,xv-x0);
        x0:=xv
      END;
    END;
    col:=min(w-1,col+len)
  END;
END write;

PROCEDURE print(scr: SCREEN; s: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
BEGIN
  str.print(tmp,s,a); write(scr,tmp,0,str.len(tmp));
END print;

PROCEDURE Write(scr: SCREEN; ch: CHAR);
  VAR i: INTEGER;
BEGIN
  WITH scr^ DO
    liter[line][col]:=ch;
    attrs[line][col]:=tty_state();
    IF off THEN col:=min(col+1,w-1); RETURN END;
    IF NOT hidden(scr,x+col,y+line,i) THEN
      tty_pos(y+line,x+col); set_tty(scr,line,col); tty.Write(ch); INC(Col)
    END;
    col:=min(col+1,w-1)
  END;
END Write;

PROCEDURE frame(scr: SCREEN);
  VAR i: INTEGER;
BEGIN
  WITH scr^ DO
    set_pos(scr,0,0);
    Write(scr,ul); repeat(scr,hb,w-2); Write(scr,ur);
    FOR i:=1 TO h-1 DO
      set_pos(scr,i,0);
      Write(scr,vb); right(scr,w-2); Write(scr,vb)
    END;
    set_pos(scr,h+1,0);
    Write(scr,dl); repeat(scr,hb,w-2); Write(scr,dr)
  END;
END frame;

PROCEDURE title(scr: SCREEN; s: ARRAY OF CHAR);
BEGIN
  center(scr,s,0)
END title;

PROCEDURE repeat(scr: SCREEN; ch: CHAR; t: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF t<=0 THEN RETURN
  ELSE
    t:=min(t,HIGH(tmp)+1);
    FOR i:=0 TO t-1 DO
      tmp[i]:=ch
    END;
    write(scr,tmp,0,t)
  END;
END repeat;

PROCEDURE right(scr: SCREEN; i: INTEGER);
BEGIN
  set_pos(scr,scr^.line,scr^.col+i)
END right;

PROCEDURE left(scr: SCREEN; i: INTEGER);
BEGIN
  set_pos(scr,scr^.line,scr^.col-i)
END left;

PROCEDURE center(scr: SCREEN; s: ARRAY OF CHAR; line: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  i:=str.len(s); j:=(scr^.w-i) DIV 2;
  set_pos(scr,line,j); write(scr,s,0,i)
END center;

PROCEDURE WriteString(scr: SCREEN; s: ARRAY OF CHAR);
BEGIN
  write(scr,s,0,str.len(s))
END WriteString;

PROCEDURE WriteLn(scr: SCREEN);
BEGIN
  set_pos(scr,scr^.line+1,0)
END WriteLn;

PROCEDURE refresh_all;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO lines-1 DO restore_line(NIL,i); tty.erase_line(0) END
END refresh_all;                                 (* last column *)

VAR ch: CHAR;

BEGIN
  tty.nop; tty.set_cursor(0); tty.erase(2); reverse:=0;

  state:=tty.state;   color  :=state^.color;
  back :=state^.back; reverse:=state^.reverse;
  mic:=state^.min_color;
  mac:=state^.max_color;
  WITH state^ DO
    ul:=bars[0,0]; ur:=bars[0,2]; hb:=hbar;
    dl:=bars[2,0]; dr:=bars[2,2]; vb:=vbar
  END;
  lines:=state^.lines; columns:=state^.columns-1;

  Line:=0; Col:=0;
  create(TTY,0,0,lines,columns); tty.home;
  TTY^.in :=TRUE;  top_scr:=TTY;
  TTY^.off:=FALSE; bot_scr:=TTY
END kcScreens.
