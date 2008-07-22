IMPLEMENTATION MODULE ttyMenus; (*$N+ Leo 25-Jun-90. (c) KRONOS *)
                                (*    Vik 17-Aug-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  wnd: ttyWindows;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  key: Keyboard;
IMPORT  mem: Heap;
IMPORT  tty: Terminal;
IMPORT       ASCII;

WITH STORAGE (NEW:     mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE:  mem.reallocate);

TYPE
  MENU = POINTER TO
         RECORD
           win: wnd.WINDOW;
           alt: INTEGER;
           frm: INTEGER;
           hot: CHAR;
           ch : CHAR;
           typ: KIND;
           W,A: INTEGER;
           htk: STRING;
           cap: STRING;
           xit: STRING;
           sel: STRING;
         END;
VAR bmp: ARRAY [0..79] OF CHAR;


PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;
PROCEDURE wnd_error; BEGIN done:=FALSE; error:=wnd.error    END wnd_error;
PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;

PROCEDURE exit(m: MENU; c: CHAR);
  VAR i: INTEGER;
BEGIN
  done:=TRUE;
  WITH m^ DO
    FOR i:=0 TO HIGH(xit) DO
      IF xit[i]=c THEN RETURN END
    END;
    RESIZE(xit,BYTES(xit)+1);
    IF NOT mem.done THEN mem_error; RETURN END;
    xit[HIGH(xit)]:=c
  END
END exit;

PROCEDURE selector(m: MENU; c: CHAR);
  VAR i: INTEGER;
BEGIN
  done:=TRUE;
  FOR i:=0 TO HIGH(m^.sel) DO
    IF m^.sel[i]=c THEN RETURN END
  END;
  RESIZE(m^.sel,BYTES(m^.sel)+1);
  IF NOT mem.done THEN mem_error; RETURN END;
  m^.sel[HIGH(m^.sel)]:=c
END selector;

PROCEDURE new(VAR m: MENU; kind: KIND; w: INTEGER);
BEGIN
  NEW(m);
  IF NOT mem.done  THEN mem_error; RETURN END;
  WITH m^ DO
    NEW(htk);      NEW(cap);
    NEW(xit);      NEW(sel);
    exit(m, ASCII.ESC);
    selector(m,key.newln); selector(m,key.lf);
    selector(m,key.nl);    selector(m,key.cr);
    A:=1;          hot:=0c;
    W:=w;          alt:=0;
    frm:=1;
    win:=wnd.null; typ:=kind;
    IF NOT mem.done THEN dispose(m); mem_error; RETURN END;
    wnd.new(win);
    IF NOT wnd.done THEN dispose(m); wnd_error; RETURN END;
    wnd.autowrap(win,wnd.off);
    wnd.cursor  (win,wnd.off);
    wnd.resize(win,W+2,3);
    IF NOT wnd.done THEN dispose(m); wnd_error; RETURN END;
    wnd.frame(win,wnd.on)
  END;
  done:=TRUE
END new;

PROCEDURE fill_bar(m: MENU);
  VAR c: CHAR;
    i,j: INTEGER;
BEGIN
  WITH m^ DO
    IF typ#barline THEN RETURN END;
    IF frm#0 THEN c:=wnd.vbar ELSE c:=' ' END;
    j:=W;
    FOR i:=1 TO A-1 DO
      wnd.setpos(win,0,j); wnd.writech(win,c); INC(j,W+1)
    END
  END
END fill_bar;

PROCEDURE frame(m: MENU; on: INTEGER);
BEGIN
  WITH m^ DO
    IF on#0 THEN on:=1 END;
    frm:=on;
    wnd.frame(m^.win,on);
    IF typ=updown THEN wnd.resize(win,W+frm*2,A+frm*2)
    ELSE               wnd.resize(win,(W+1)*A+frm*2,1+frm*2)
    END;
    fill_bar(m)
  END
END frame;

PROCEDURE title(m: MENU; VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN wnd.frameprint(m^.win,0,1,fmt,args) END title;

PROCEDURE dispose(VAR m: MENU);
BEGIN
  wnd.dispose(m^.win);
  DISPOSE(m^.htk);  DISPOSE(m^.cap);
  DISPOSE(m^.xit);  DISPOSE(m^.sel);
  DISPOSE(m);
  done:=TRUE
END dispose;

PROCEDURE set_alt_pos(m: MENU; no: INTEGER);
BEGIN
  IF m^.typ=updown THEN wnd.setpos(m^.win,no,0)
  ELSE                  wnd.setpos(m^.win,0,(m^.W+1)*no)
  END
END set_alt_pos;

PROCEDURE mark_alt_pos(m: MENU; mark: BOOLEAN);

  PROCEDURE _mark(l,c: INTEGER);
  BEGIN
    IF mark THEN wnd.reverse(m^.win,1) END;
    wnd.setpos(m^.win, l,c);
    wnd.read(m^.win,bmp,0,m^.W);
    wnd.setpos(m^.win, l,c);
    wnd.write(m^.win, bmp, 0, m^.W);
    done:=wnd.done;
    wnd.reverse(m^.win,0);
  END _mark;

BEGIN
  IF m^.typ=updown THEN _mark(m^.alt,0)
  ELSE                  _mark(m^.frm,(m^.W+1)*m^.alt)
  END;
END mark_alt_pos;

PROCEDURE set_alt(m: MENU;
                 no: INTEGER;
             hotkey: CHAR;
            capital: BOOLEAN;
                fmt: ARRAY OF CHAR;
           SEQ args: SYSTEM.WORD);
  VAR i,h: INTEGER;
BEGIN
  WITH m^ DO
    IF no>HIGH(htk) THEN
      h:=HIGH(htk)+1;
      RESIZE(htk,no+1);
      IF NOT mem.done THEN mem_error; RETURN END;
      FOR i:=h TO HIGH(htk) DO htk[i]:=0c END;
      RESIZE(cap,no+1);
      IF NOT mem.done THEN mem_error; RETURN END;
      FOR i:=h TO HIGH(cap) DO cap[i]:=0c END;
    END;
    IF no>=A THEN
      IF typ=updown THEN wnd.resize(win, W+frm*2, (no+1)+frm*2)
      ELSE               wnd.resize(win,(W+1)*(no+1)-1+2*frm*2,1+frm*2)
      END;
      IF NOT wnd.done THEN wnd_error; RETURN ELSE A:=no+1 END
    END;
    str.print(bmp,fmt,args);
    set_alt_pos(m,no);
    wnd.print(win,"%-*.*s",W,W,bmp);
    fill_bar(m);
    htk[no]:=hotkey;
    IF capital THEN
      htk[no]:=ASCII.SMALL  (hotkey);
      cap[no]:=ASCII.CAPITAL(hotkey)
    END
  END;
  done:=TRUE
END set_alt;

PROCEDURE open(m: MENU; l,c: INTEGER);
BEGIN
  wnd.move(m^.win,l,c);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  wnd.open(m^.win);   done:=TRUE
END open;

PROCEDURE close(m: MENU);
BEGIN wnd.close(m^.win);  done:=TRUE END close;

PROCEDURE on_top(m: MENU);
BEGIN wnd.ontop(m^.win);  done:=TRUE END on_top;

PROCEDURE on_bottom(m: MENU);
BEGIN wnd.onbottom(m^.win);  done:=TRUE END on_bottom;

PROCEDURE index(VAL s: ARRAY OF CHAR; ch: CHAR; VAR inx: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(s) DO
    IF s[i]=ch THEN inx:=i; RETURN TRUE END
  END;
  RETURN FALSE
END index;

PROCEDURE _select(m: MENU);
  VAR i: INTEGER;
     up: CHAR;
     dw: CHAR;
    clo: BOOLEAN;
BEGIN done:=FALSE;
  WITH m^ DO
    wnd.cursor(win,wnd.off);
    clo:=wnd.closed(win);
    IF clo THEN wnd.open(win) END;
    mark_alt_pos(m,TRUE);
    IF typ=updown THEN up:=key.up;   dw:=key.dw
    ELSE               up:=key.left; dw:=key.right
    END;
    LOOP
      key.read(ch);
      mark_alt_pos(m,FALSE);
      IF index(htk,ch,i) OR index(cap,ch,i) THEN
        done:=TRUE; hot:=htk[i]; alt:=i; EXIT
      ELSIF index(xit,ch,i) THEN done:=FALSE; EXIT
      ELSIF index(sel,ch,i) THEN
         done:=TRUE;
         IF (alt >= 0) & (alt <= HIGH(htk)) THEN hot:=htk[alt];
         ELSE hot:=0c;
         END;
         EXIT
      ELSIF ch=up           THEN alt:=(alt-1) MOD A;
      ELSIF ch=dw           THEN alt:=(alt+1) MOD A;
      END;
      mark_alt_pos(m,TRUE)
    END;
    IF clo THEN wnd.close(win) END
  END
END _select;

PROCEDURE select(m: MENU);
BEGIN
  IF wnd.closed(m^.win) THEN done:=FALSE; error:=err.inv_op; RETURN END;
  _select(m)
END select;

PROCEDURE set_select(m: MENU; a: INTEGER);
BEGIN
  IF (a>=0) & (a<m^.A) THEN done:=TRUE; m^.alt:=a ELSE bad_parm END
END set_select;

PROCEDURE popup(m: MENU; l,c: INTEGER);
BEGIN
  IF NOT wnd.closed(m^.win) THEN done:=FALSE; error:=err.inv_op; RETURN END;
  wnd.move(m^.win,l,c);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  _select(m)
END popup;

PROCEDURE alt(m: MENU): INTEGER;
BEGIN RETURN m^.alt END alt;

PROCEDURE hotkey(m: MENU): CHAR;
BEGIN RETURN m^.hot END hotkey;

PROCEDURE last(m: MENU): CHAR;
BEGIN RETURN m^.ch END last;

BEGIN
  null:=NIL; done:=TRUE; error:=err.ok
END ttyMenus.
