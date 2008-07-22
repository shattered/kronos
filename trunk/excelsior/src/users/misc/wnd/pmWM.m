IMPLEMENTATION MODULE pmWM; (*$N+ Leo 23-Apr-91. (c) KRONOS *)

IMPORT  wnd: pmWnd;
IMPORT  crs: pmCrs;
IMPORT  cpd: CPD;
IMPORT  key: Keyboard;

CONST MAGIC = 6E774D57h;

PROCEDURE my(w: WINDOW): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  wnd.object(w,"WM",i);
  RETURN wnd.done & (i#MAGIC)
END my;

PROCEDURE nop(VAL e: EVENT); END nop;

PROCEDURE incl(w: WINDOW);
  VAR i: INTEGER;
BEGIN
  wnd.object(w,"WM",i);
  IF wnd.done & (i=MAGIC) THEN RETURN END;
  wnd.assign(w,"WM",MAGIC);
  wnd.assign(w,"WM.TRK",nop);
END incl;

PROCEDURE excl(w: WINDOW);
BEGIN
  wnd.assign(w,"WM",0);
  wnd.delete(w,"WM");
  wnd.delete(w,"WM.TRK");
END excl;

PROCEDURE monitoring(w: WINDOW; p: MONITOR);
BEGIN
  wnd.assign(w,"WM.TRK",p);
END monitoring;

PROCEDURE monitor(w: WINDOW): MONITOR;
  VAR p: MONITOR;
BEGIN
  wnd.object(w,"WM.TRK",p);
  IF wnd.done THEN RETURN p ELSE RETURN nop END
END monitor;

VAR
  READ: BOOLEAN;
  BACK: BOOLEAN;
  EV  : EVENT;

PROCEDURE listen(VAR event: EVENT);
  VAR c,l: WINDOW;
      trk: MONITOR;
BEGIN
  READ:=FALSE;
  IF BACK THEN
    BACK:=FALSE;
    wnd.object(EV.win,"WM.TRK",trk);
    IF wnd.done THEN trk(EV) END;
    RETURN
  END;
  c:=wnd.locate(crs.x,crs.y);
  IF NOT my(c) THEN
    crs.monitor(20)
  ELSE
    EV.cha:=cpd.state^.keys;
    crs.monitor(20);
    EV.but:=cpd.state^.keys;
    EV.cha:=EV.cha/EV.but;
    EV.x  :=crs.x;
    EV.y  :=crs.y;
    IF key.ready()>0 THEN key.read(EV.key) ELSE EV.key:=0c END;
    EV.win:=c;
    l:=wnd.locate(crs.x,crs.y);
    READ:=FALSE; BACK:=FALSE;
    wnd.object(c,"WM.TRK",trk);
    IF wnd.done THEN trk(EV) END;
    IF NOT READ & (l#c) & my(l) THEN
      EV.win:=l;
      wnd.object(c,"WM.TRK",trk);
      IF wnd.done THEN trk(EV) END
    END
  END;
  event:=EV
END listen;

PROCEDURE read(VAR event: EVENT);
BEGIN
  READ:=TRUE;
  IF BACK THEN event:=EV; BACK:=FALSE; RETURN END;
  event.cha:=cpd.state^.keys;
  crs.monitor(20);
  event.but:=cpd.state^.keys;
  event.cha:=event.cha/event.but;
  event.x  :=crs.x;
  event.y  :=crs.y;
  IF key.ready()>0 THEN key.read(event.key) ELSE event.key:=0c END;
  event.win:=wnd.locate(crs.x,crs.y)
END read;

PROCEDURE back;
BEGIN
  IF READ THEN BACK:=TRUE END
END back;

BEGIN
  READ:=FALSE; BACK:=FALSE
END pmWM.
