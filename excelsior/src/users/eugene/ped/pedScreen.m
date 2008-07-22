IMPLEMENTATION MODULE pedScreen; (* Sem 05-Mar-87. (c) KRONOS *)
                                 (* Ilx 24-Oct-89. (c) KRONOS *)

                IMPORT  s  : SYSTEM;
FROM SYSTEM     IMPORT  ADR;
                IMPORT  Model;
FROM Model      IMPORT  Objects;
FROM pedEditor  IMPORT  Sheet;
                IMPORT  win: Windows;
                IMPORT  vg : pedVG;
                IMPORT  VG, ModelMisc;
                IMPORT  heap: cdsHeap;
                IMPORT  mcd: defCodes;
                IMPORT  sch: osKernel;
                IMPORT  ex : ModelPbl;
                IMPORT  Strings;

TYPE ADDRESS  = s.ADDRESS;
     WORD     = s.WORD;
     window   = win.window;
     txtb     = ARRAY [0..3] OF ARRAY [0..79] OF CHAR;
     requests = (wnd,crs,dcrs,app,del,stop,txt,txtr,sig,coff,chp);
     rs       = SET OF requests;

CONST crsType=4;

VAR crs_bmd: VG.BMD;

TYPE seg=RECORD
       X1,X2,Y1,Y2   : INTEGER;
       Size          : INTEGER;
       Layer         : BITSET;
     END;

TYPE ScrContext=POINTER TO RECORD
       TextBuf       : txtb;
       TextSet       : BITSET;
       textBuf       : ARRAY [0..79] OF CHAR;
       lin,colomn    : INTEGER;
       color         : INTEGER;
       wait?         : BOOLEAN;
       textOK        : sch.signal_rec;
       CurSig        : Model.Object;
       Outer,Ref     : sch.process;
       cur_chip      : Model.Object;
       Stop          : BOOLEAN;
       OutRequest    : sch.signal_rec;
       RefRequest    : sch.signal_rec;
       OutHalted     : sch.signal_rec;
       SegOK         : sch.signal_rec;
       SigOK         : sch.signal_rec;
       crsOK         : sch.signal_rec;
       chipOK        : sch.signal_rec;
       Lock          : sch.mutex_rec;
       crsX1,crsX2   : INTEGER;
       crsY1,crsY2   : INTEGER;
     dcrsX1,dcrsX2   : INTEGER;
     dcrsY1,dcrsY2   : INTEGER;
       wndX1,wndY1   : INTEGER;
       wndX2,wndY2   : INTEGER;
       wndsX,wndsY   : INTEGER;
       scrX,scrY     : INTEGER;
       refcrsX1,refcrsX2: INTEGER;
       refcrsY1,refcrsY2: INTEGER;
       drefcrsX1,drefcrsX2: INTEGER;
       drefcrsY1,drefcrsY2: INTEGER;
       chip_x,chip_y: INTEGER;
       chip_r,chip_m: INTEGER;
       NoCursor     : BOOLEAN;
       NoCursord    : BOOLEAN;
       Req,Bad      : rs;
       WndAborted   : BOOLEAN;
       x_o,xx,y_o,yy: INTEGER;
       Ch           : Model.Object;
       chip_on      : BOOLEAN;
       x,y,r,m      : INTEGER;
       sw,sh        : INTEGER;
       c_line       : PROCEDURE(INTEGER,INTEGER,INTEGER,INTEGER,Sheet);
       Chain        : Model.Object;
       LineXs,LineYs,LineXe,LineYe: INTEGER;
       InDo         : BOOLEAN;
       LastSig      : Model.Object;
     END;

PROCEDURE rem_process(VAR p: sch.process);
  VAR i: INTEGER;
BEGIN
  p^.halt:=NIL;
  sch.stop(p,TRUE);
  i:=sch.rem_process(p);
  IF i#0 THEN HALT(i) END;
END rem_process;

PROCEDURE Tag(o: Model.Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE setm(m: BITSET); CODE mcd.setm END setm;
PROCEDURE getm(): BITSET;  CODE mcd.getm END getm;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;

PROCEDURE cursor(x,y,no: INTEGER);
BEGIN
  IF no=4 THEN
    VG.line(crs_bmd,x-8,y-8,x-4,y-4);
    VG.line(crs_bmd,x+8,y-8,x+4,y-4);
    VG.line(crs_bmd,x+8,y+8,x+4,y+4);
    VG.line(crs_bmd,x-8,y+8,x-4,y+4);
    VG.dot(crs_bmd,x,y);
  ELSE
    VG.line(crs_bmd,x+4,y,x+3,y);
    VG.line(crs_bmd,x-4,y,x-3,y);
    VG.line(crs_bmd,x,y+4,x,y+3);
    VG.line(crs_bmd,x,y-4,x,y-3);
  END;
END cursor;

PROCEDURE CursorOFF(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    IF NoCursor THEN RETURN END;
    VG.mode(crs_bmd,VG.xor);
    WITH sht^ DO
      IF (refcrsX1#refcrsX2)OR(refcrsY1#refcrsY2) THEN
        VG.line(crs_bmd,refcrsX1+wnd^.W+wnd^.x*32,
                        refcrsY1+wnd^.S+VG.lines-wnd^.y-wnd^.sy,
                        refcrsX2+wnd^.W+wnd^.x*32,
                        refcrsY2+wnd^.S+VG.lines-wnd^.y-wnd^.sy);
      END;
      cursor(refcrsX2+wnd^.W+wnd^.x*32,
             refcrsY2+wnd^.S+VG.lines-wnd^.y-wnd^.sy,crsType);
    END;
    NoCursor:=TRUE;
  END;
END CursorOFF;

PROCEDURE RefCursor(sht: Sheet);
  VAR x1,y1,x2,y2: INTEGER;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock);
    x1:=(crsX1-scrX) DIV wndsX;
    x2:=(crsX2-scrX) DIV wndsX;
    y1:=(crsY1-scrY) DIV wndsY;
    y2:=(crsY2-scrY) DIV wndsY;
    EXCL(Req,crs);
    sch.release(Lock);
    CursorOFF(sht);
    refcrsX1:=x1; refcrsX2:=x2; refcrsY1:=y1; refcrsY2:=y2;
    VG.mode(crs_bmd,VG.xor);
    WITH sht^ DO
      IF (refcrsX1#refcrsX2)OR(refcrsY1#refcrsY2) THEN
        VG.line(crs_bmd,refcrsX1+wnd^.W+wnd^.x*32,
                        refcrsY1+wnd^.S+VG.lines-wnd^.y-wnd^.sy,
                        refcrsX2+wnd^.W+wnd^.x*32,
                        refcrsY2+wnd^.S+VG.lines-wnd^.y-wnd^.sy);
      END;
      cursor(refcrsX2+wnd^.W+wnd^.x*32,
             refcrsY2+wnd^.S+VG.lines-wnd^.y-wnd^.sy,crsType);
    END;
  NoCursor:=FALSE;
  END;
END RefCursor;

PROCEDURE CursordOFF(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    IF NoCursord THEN RETURN END;
    VG.mode(crs_bmd,VG.xor);
    cursor(drefcrsX2+sht^.wnd^.W+sht^.wnd^.x*32,
           drefcrsY2+sht^.wnd^.S+VG.lines-sht^.wnd^.y-sht^.wnd^.sy,0);
    NoCursord:=TRUE;
  END;
END CursordOFF;

PROCEDURE RefCursord(sht: Sheet);
  VAR x1,y1,x2,y2: INTEGER;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock);
    x1:=(dcrsX1-scrX) DIV wndsX;
    x2:=(dcrsX2-scrX) DIV wndsX;
    y1:=(dcrsY1-scrY) DIV wndsY;
    y2:=(dcrsY2-scrY) DIV wndsY;
    EXCL(Req,dcrs);
    sch.release(Lock);
    CursordOFF(sht);
    IF (rs{dcrs,wnd,stop}*Req)#rs{} THEN RETURN END;
    drefcrsX1:=x1; drefcrsX2:=x2; drefcrsY1:=y1; drefcrsY2:=y2;
    VG.mode(crs_bmd,VG.xor);
    cursor(drefcrsX2+sht^.wnd^.W+sht^.wnd^.x*32,
           drefcrsY2+sht^.wnd^.S+VG.lines-sht^.wnd^.y-sht^.wnd^.sy,0);
    NoCursord:=FALSE;
  END;
END RefCursord;

PROCEDURE recalc(VAR xa,ya,xb,yb: INTEGER; sht: Sheet): BOOLEAN;
  VAR x0,x1: INTEGER;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    CASE r OF
       0: xa:=(x+xa-scrX) DIV wndsX; xb:=(x+xb-scrX) DIV wndsX;
          ya:=(y+ya-scrY) DIV wndsY; yb:=(y+yb-scrY) DIV wndsY;
      |1: x0:=xa; x1:=xb;
          xa:=(x+ya-scrX) DIV wndsX; xb:=(x+yb-scrX) DIV wndsX;
          ya:=(y-x0-scrY) DIV wndsY; yb:=(y-x1-scrY) DIV wndsY;
      |2: xa:=(x-xa-scrX) DIV wndsX; xb:=(x-xb-scrX) DIV wndsX;
          ya:=(y-ya-scrY) DIV wndsY; yb:=(y-yb-scrY) DIV wndsY;
      |3: x0:=xa; x1:=xb;
          xa:=(x-ya-scrX) DIV wndsX; xb:=(x-yb-scrX) DIV wndsX;
          ya:=(y+x0-scrY) DIV wndsY; yb:=(y+x1-scrY) DIV wndsY;
    END;
    RETURN vg.clip(0,sw,0,sh,xa,ya,xb,yb);
  END;
END recalc;

PROCEDURE chip_line(xa,ya,xb,yb: INTEGER; sht: Sheet);
BEGIN
  IF recalc(xa,ya,xb,yb,sht) THEN
    WITH sht^ DO vg.vect(wnd,xa+wnd^.W,ya+wnd^.S,xb+wnd^.W,yb+wnd^.S) END;
  END;
END chip_line;

PROCEDURE ch_line(xa,ya,xb,yb: INTEGER; sht: Sheet);
BEGIN
  IF recalc(xa,ya,xb,yb,sht) THEN
    WITH sht^ DO
      VG.line(crs_bmd,
              xa+wnd^.W+wnd^.x*32,ya+wnd^.S+VG.lines-wnd^.y-wnd^.sy,
              xb+wnd^.W+wnd^.x*32,yb+wnd^.S+VG.lines-wnd^.y-wnd^.sy);
    END;
  END;
END ch_line;

PROCEDURE chip_rect(xa,ya,xb,yb: INTEGER; sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    c_line(xa,ya,xb,ya,sht); c_line(xb,ya,xb,yb,sht);
    c_line(xb,yb,xa,yb,sht); c_line(xa,yb,xa,ya,sht);
  END;
END chip_rect;

PROCEDURE drow_chip(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    c_line:=chip_line;
    chip_rect(0,0,Ch^.ChipType^.ctX,Ch^.ChipType^.ctY,sht);
    chip_rect(33,(Ch^.ChipType^.ctY+1) DIV 2 - 15,
              63,(Ch^.ChipType^.ctY+1) DIV 2 + 15,sht);
  END;
END drow_chip;

PROCEDURE dr_chip(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    c_line:=ch_line;
    chip_rect(0,0,Ch^.ChipType^.ctX,Ch^.ChipType^.ctY,sht);
    chip_rect(33,(Ch^.ChipType^.ctY+1) DIV 2 - 15,
              63,(Ch^.ChipType^.ctY+1) DIV 2 + 15,sht);
  END;
END dr_chip;

PROCEDURE RefCh(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH sht^ DO
    WITH c^ DO
      sw:=wnd^.dsx-(wnd^.W+wnd^.E);
      sh:=wnd^.sy -(wnd^.S+wnd^.N);
      CASE m OF
        |0: vg.color(wnd,12); vg.mode(wnd,vg.rep); drow_chip(sht);
        |1: vg.color(wnd,12); vg.mode(wnd,vg.bic); drow_chip(sht);
        |2: VG.mode(crs_bmd,VG.xor);
            IF NOT chip_on THEN
              dr_chip(sht); x_o:=x; y_o:=y; chip_on:=TRUE;
            ELSE
              xx:=x; yy:=y; x:=x_o; y:=y_o; dr_chip(sht);
              x:=xx; y:=yy; x_o:=x; y_o:=y; dr_chip(sht);
            END;
        |3: IF chip_on THEN
              x:=x_o; y:=y_o; dr_chip(sht); chip_on:=FALSE;
            END;
        |4: vg.color(wnd,1); vg.mode(wnd,vg.rep); drow_chip(sht);
            vg.color(wnd,8); vg.mode(wnd,vg.bic); drow_chip(sht);
        |5: vg.color(wnd,1); vg.mode(wnd,vg.bic); drow_chip(sht);
            vg.color(wnd,8); vg.mode(wnd,vg.rep); drow_chip(sht);
      END;
    END;
  END;
END RefCh;

PROCEDURE RefChip(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock);
    Ch:=cur_chip;
    x:=chip_x; y:=chip_y; r:=chip_r MOD 4; m:=chip_m;
    EXCL(Req,chp);
    sch.release(Lock);
    win.lock_window(sht^.wnd); RefCh(sht); win.release_window(sht^.wnd);
    IF m IN {0,1,4,5} THEN win.refresh(sht^.wnd) END;
    sch.send(chipOK);
  END;
END RefChip;

PROCEDURE CrsOff(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock); EXCL(Req,coff); sch.release(Lock);
    CursorOFF(sht); CursordOFF(sht);
    m:=3; RefCh(sht);
    sch.send(crsOK);
    NoCursor:=TRUE; NoCursord:=TRUE;
  END;
END CrsOff;

PROCEDURE line(x0,y0,x1,y1: INTEGER; w: INTEGER; sht: Sheet);
  VAR h,v,l: INTEGER;
      dx,dy: INTEGER;         r: INTEGER;
    x0u,x0d: INTEGER;   x1u,x1d: INTEGER;
    y0u,y0d: INTEGER;   y1u,y1d: INTEGER;

  PROCEDURE SQRT(n:INTEGER): INTEGER;
    VAR l,r: INTEGER;
  BEGIN
    IF n<0 THEN RETURN SQRT(-n) END;
    IF n<2 THEN RETURN n END;
    l:=1; r:=n;
    REPEAT r:=(l+r) DIV 2; l:=n DIV r UNTIL l>=r;
    RETURN r
  END SQRT;

BEGIN
  WITH sht^ DO
    IF w<=2 THEN
      IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
                 0,wnd^.sy -(wnd^.S+wnd^.N),
                                   x0,y0,x1,y1) THEN
        vg.vect(wnd,x0+wnd^.W,y0+wnd^.S,x1+wnd^.W,y1+wnd^.S)
      END;
      IF w=2 THEN
        IF    x0=x1 THEN
          x0:=x0+1; x1:=x1+1;
          IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
                     0,wnd^.sy -(wnd^.S+wnd^.N),
                                       x0,y0,x1,y1) THEN
            vg.vect(wnd,x0+wnd^.W,y0+wnd^.S,x1+wnd^.W,y1+wnd^.S)
          END;
        ELSIF y0=y1 THEN
          y0:=y0+1; y1:=y1+1;
          IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
                     0,wnd^.sy -(wnd^.S+wnd^.N),
                                       x0,y0,x1,y1) THEN
            vg.vect(wnd,x0+wnd^.W,y0+wnd^.S,x1+wnd^.W,y1+wnd^.S)
          END;
        ELSE
          x0:=x0+1; x1:=x1+1;
          IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
                     0,wnd^.sy -(wnd^.S+wnd^.N),
                                       x0,y0,x1,y1) THEN
            vg.vect(wnd,x0+wnd^.W,y0+wnd^.S,x1+wnd^.W,y1+wnd^.S)
          END;
        --vg.vect(wnd,x0+1,y0+1,x1+1,y1+1)
        END;
      END;
      RETURN
    END;
    IF ODD(w) THEN ELSE INC(w) END;
    h:=x1-x0;   v:=y1-y0;
    IF    h=0 THEN l:=ABS(v)
    ELSIF v=0 THEN l:=ABS(h)
    ELSE
      l:=SQRT(h*h+v*v);
    END;
    IF l=0 THEN RETURN END;
    dx:=v*w / l / 2;
    dy:=h*w / l / 2;
    IF (dx=0) & (dy=0) THEN
      IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
                 0,wnd^.sy -(wnd^.S+wnd^.N),
                                   x0,y0,x1,y1) THEN
        vg.vect(wnd,x0+wnd^.W,y0+wnd^.S,x1+wnd^.W,y1+wnd^.S)
      END;
      RETURN
    END;
    x0u:=x0-dx; x0d:=x0+dx;
    y0u:=y0+dy; y0d:=y0-dy;
    x1u:=x1-dx; x1d:=x1+dx;
    y1u:=y1+dy; y1d:=y1-dy;
    r:=w / 2;
    IF ((x1-r)>=0)&((x1+r)<(wnd^.dsx-(wnd^.W+wnd^.E)))&
       ((y1-r)>=0)&((y1+r)<(wnd^.sy -(wnd^.S+wnd^.N))) THEN
      vg.arc(wnd,x1+wnd^.W,y1+wnd^.S,x1d+wnd^.W,y1d+wnd^.S,
                                        x1u+wnd^.W,y1u+wnd^.S,r);
    END;
    IF ((x0-r)>=0)&((x0+r)<(wnd^.dsx-(wnd^.W+wnd^.E)))&
       ((y0-r)>=0)&((y0+r)<(wnd^.sy -(wnd^.S+wnd^.N))) THEN
      vg.arc(wnd,x0+wnd^.W,y0+wnd^.S,x0u+wnd^.W,y0u+wnd^.S,
                                        x0d+wnd^.W,y0d+wnd^.S,r);
    END;
    IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
               0,wnd^.sy -(wnd^.S+wnd^.N),
                            x0u,y0u,x1u,y1u) THEN
      vg.vect(wnd,x0u+wnd^.W,y0u+wnd^.S,x1u+wnd^.W,y1u+wnd^.S);
    END;
    IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),
               0,wnd^.sy -(wnd^.S+wnd^.N),
                            x0d,y0d,x1d,y1d) THEN
      vg.vect(wnd,x1d+wnd^.W,y1d+wnd^.S,x0d+wnd^.W,y0d+wnd^.S);
    END;
  END;
END line;

PROCEDURE DoConductor(sht: Sheet; VAL s: seg; mod: vg.modes);
  VAR X,Y,R: INTEGER;
      c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  CursorOFF(sht); CursordOFF(sht);
  sch.acquire(sht^.wnd^.lock);
  vg.color(sht^.wnd,INTEGER(s.Layer)); vg.mode(sht^.wnd,mod);
  IF (s.X1=s.X2) & (s.Y1=s.Y2) THEN
    X:=(s.X1-c^.scrX) / c^.wndsX; Y:=(s.Y1-c^.scrY) / c^.wndsY;
    R:=s.Size / c^.wndsY;
    IF ((X-R)>=0)&((X+R)<(sht^.wnd^.dsx-(sht^.wnd^.W+sht^.wnd^.E)))&
       ((Y-R)>=0)&((Y+R)<(sht^.wnd^.sy -(sht^.wnd^.S+sht^.wnd^.N))) THEN
      vg.circ(sht^.wnd,X+sht^.wnd^.W,Y+sht^.wnd^.S,R);
    END;
  ELSE
    line((s.X1-c^.scrX) / c^.wndsX,(s.Y1-c^.scrY) / c^.wndsY,
         (s.X2-c^.scrX) / c^.wndsX,(s.Y2-c^.scrY) / c^.wndsY,
         s.Size*2 / c^.wndsX,sht);
  END;
  sch.release(sht^.wnd^.lock);
  c^.WndAborted:=c^.WndAborted OR((rs{wnd,stop}*c^.Req)#rs{});
END DoConductor;

VAR mask: BITSET;

(*$T-*)

PROCEDURE UnPackSeg(VAL s: Model.Segment; VAR c: seg);
BEGIN
  WITH c DO
    WITH s DO
      Size:=INTEGER(BITSET(size<<12)*{0..11});
      IF 1 IN BITSET(size) THEN
        X1:=INTEGER(BITSET(start)*mask)+Size;
        Y1:=INTEGER(BITSET(start>>16)*mask)+Size;
        X2:=INTEGER(BITSET(end)*mask)-Size;
        Y2:=INTEGER(BITSET(end>>16)*mask)-Size;
      ELSE
        X1:=INTEGER(BITSET(start)*mask)+Size;
        Y2:=INTEGER(BITSET(start>>16)*mask)+Size;
        X2:=INTEGER(BITSET(end)*mask)-Size;
        Y1:=INTEGER(BITSET(end>>16)*mask)-Size;
      END;
      DEC(X1,8000h); DEC(X2,8000h); DEC(Y1,8000h); DEC(Y2,8000h);
      Layer:=BITSET(size>>12)*{0..7};
    END;
  END;
END UnPackSeg;

(*$T+*)

PROCEDURE DoInBox0(s: Model.Object; sht: Sheet);
  TYPE pSegment=POINTER TO Model.Segment;

  PROCEDURE chk_box(p1,p2: pSegment): BOOLEAN; CODE 0F8h END chk_box;

  VAR  i,j: INTEGER; c: Model.Object; p,e,b: pSegment; l,ch: Model.Segment;
  VAR co: ScrContext; sg: seg;
BEGIN
  co:=sht^.ScreenContext;
  WITH co^ DO
    IF ((Tag(s)#signal)      OR (    sht^.PublicContext^.ExtPin) OR (s^.ChainB =NIL))  &
       ((Tag(s)#chip)        OR (    sht^.PublicContext^.ExtPin)) &
       ((Tag(s)#externalpin) OR (NOT sht^.PublicContext^.ExtPin) OR (s^.PinType=NIL))
       OR WndAborted THEN RETURN END;
    l.start:=LineXs+8000h+INTEGER((LineYs+8000h)<<16);
    l.end  :=LineXe+8000h+INTEGER((LineYe+8000h)<<16);
    IF (Tag(s)=signal) OR (Tag(s)=externalpin) THEN
      IF Tag(s)=signal THEN c:=s^.ChainB ELSE c:=s^.PinType END;
      IF c^.cFree=0 THEN RETURN END;
  (*$T-*)
      b:=ADR(c^.cType[0]);
      p:=ADR(c^.cType[0]);
      e:=ADR(c^.cType[c^.cFree]);
      co^.CurSig:=s;
      REPEAT
        IF chk_box(p,ADR(l)) THEN
          UnPackSeg(p^,sg);
          IF (s=sht^.PublicContext^.Signal) THEN sg.Layer:=sg.Layer+{2} END;
          DoConductor(sht,sg,vg.rep);
          IF WndAborted THEN InDo:=FALSE; win.refresh(sht^.wnd); RETURN END;
        END;
        p:=ADDRESS(INTEGER(p)+SIZE(Model.Segment));
      UNTIL p=e;
  (*$T+*)
    ELSE
      CASE s^.RB OF
        -1: RETURN ;
        |0: ch.start:=s^.XB+8000h+INTEGER((s^.YB+8000h)<<16);
            ch.end  :=s^.XB+s^.ChipType^.ctX+8000h+
                      INTEGER((s^.YB+s^.ChipType^.ctY+8000h)<<16);
        |1: ch.start:=s^.XB+8000h+INTEGER((s^.YB-s^.ChipType^.ctX+8000h)<<16);
            ch.end  :=s^.XB+s^.ChipType^.ctY+8000h+INTEGER((s^.YB+8000h)<<16);
        |2: ch.start:=s^.XB-s^.ChipType^.ctX+8000h+
                      INTEGER((s^.YB-s^.ChipType^.ctY+8000h)<<16);
            ch.end  :=s^.XB+8000h+INTEGER((s^.YB+8000h)<<16);
        |3: ch.start:=s^.XB-s^.ChipType^.ctY+8000h+INTEGER((s^.YB+8000h)<<16);
            ch.end  :=s^.XB+8000h+INTEGER((s^.YB+s^.ChipType^.ctX+8000h)<<16);
      END;
      IF chk_box(ADR(ch),ADR(l)) THEN
        sch.acquire(sht^.wnd^.lock);
        x:=s^.XB; y:=s^.YB; r:=s^.RB; m:=0; Ch:=s; RefCh(sht);
        IF Ch=sht^.PublicContext^.Chip THEN m:=4; RefCh(sht) END;
        sch.release(sht^.wnd^.lock);
      END;
    END;
  END;
END DoInBox0;

PROCEDURE DoInBox(sht: Sheet);
  VAR i: INTEGER;
  VAR co: ScrContext;
BEGIN
  co:=sht^.ScreenContext;
  WITH co^ DO
    IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
    IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
    IF LineXs<0 THEN LineXs:=0 END;
    IF LineXe<0 THEN LineXe:=0 END;
    IF LineYs<0 THEN LineYs:=0 END;
    IF LineYe<0 THEN LineYe:=0 END;
    InDo:=TRUE; sch.send(RefRequest);
    Model.Iterate(sht^.mdl^.All,DoInBox0,sht);
    Model.Iterate(sht^.mdl^.ExternalPins,DoInBox0,sht);
    InDo:=FALSE;
    win.refresh(sht^.wnd);
  END;
END DoInBox;

PROCEDURE RefSig0(s: Model.Object; sht: Sheet);
  TYPE pSegment=POINTER TO Model.Segment;
  PROCEDURE chk_box(p1,p2: pSegment): BOOLEAN; CODE 0F8h END chk_box;
  VAR  i,j: INTEGER; c: Model.Object; p,e,b: pSegment; l: Model.Segment;
  VAR co: ScrContext; sg: seg;
BEGIN
  co:=sht^.ScreenContext;
  WITH co^ DO
    EXCL(Req,sig);
    IF s=NIL THEN RETURN END;
    IF sht^.PublicContext^.ExtPin THEN
      IF (Tag(s)#externalpin) OR (s^.PinType=NIL) THEN RETURN END;
      c:=s^.PinType;
    ELSE
      IF (Tag(s)#signal)      OR (s^.ChainB =NIL) THEN RETURN END;
      c:=s^.ChainB;
    END;
    IF c^.cFree=0 THEN RETURN END;
  (*$T-*)
    b:=ADR(c^.cType[0]);
    p:=ADR(c^.cType[0]);
    e:=ADR(c^.cType[c^.cFree]);
    l.start:=LineXs+8000h+INTEGER((LineYs+8000h)<<16);
    l.end  :=LineXe+8000h+INTEGER((LineYe+8000h)<<16);
    co^.CurSig:=s;
    REPEAT
      IF chk_box(p,ADR(l)) THEN
        UnPackSeg(p^,sg);
        IF s=sht^.PublicContext^.Signal THEN
          sg.Layer:=sg.Layer+{2}; DoConductor(sht,sg,vg.rep);
        ELSE
          sg.Layer:={2};          DoConductor(sht,sg,vg.bic);
        END;
      END;
      p:=ADDRESS(INTEGER(p)+SIZE(Model.Segment));
    UNTIL p=e;
  (*$T+*)
  END;
END RefSig0;

PROCEDURE RefSig(sht: Sheet);
  VAR i: INTEGER;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    LineXs:=wndX1; LineYs:=wndY1; LineXe:=wndX2; LineYe:=wndY2;
    IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
    IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
    IF LineXs<0 THEN LineXs:=0 END;
    IF LineXe<0 THEN LineXe:=0 END;
    IF LineYs<0 THEN LineYs:=0 END;
    IF LineYe<0 THEN LineYe:=0 END;
    RefSig0(LastSig,sht);
    RefSig0(sht^.PublicContext^.Signal,sht);
    LastSig:=sht^.PublicContext^.Signal;
    win.refresh(sht^.wnd);
    sch.send(SigOK);
  END;
END RefSig;

PROCEDURE RefWindow(sht: Sheet);

  VAR sx,sy,x1,x2,y1,y2,i,rollXs,rollYs: INTEGER;
      x,y,j,k,l: INTEGER;

  PROCEDURE MoveScreen;
    VAR i: INTEGER;
    VAR c: ScrContext;
  BEGIN
    c:=sht^.ScreenContext;
    WITH c^ DO
      LOOP
        IF (Req*rs{wnd,stop})#rs{} THEN RETURN END;
        IF scrX-rollXs>x1 THEN
          i:=(scrX-x1) / rollXs;
          vg.rollE(sht^.wnd,i);
          DEC(scrX,rollXs*i);
          DEC(wndX2,rollXs*i);
        ELSIF scrX+rollXs<x1 THEN
          i:=(x1-scrX) / rollXs;
          vg.rollW(sht^.wnd,i);
          INC(scrX,rollXs*i);
          INC(wndX1,rollXs*i);
        ELSIF scrY-rollYs>y1 THEN
          i:=(scrY-y1) / rollYs;
          vg.rollN(sht^.wnd,i);
          DEC(scrY,rollYs*i);
          DEC(wndY2,rollYs*i);
        ELSIF scrY+rollYs<y1 THEN
          i:=(y1-scrY) / rollYs;
          vg.rollS(sht^.wnd,i);
          INC(scrY,rollYs*i);
          INC(wndY1,rollYs*i);
        ELSE RETURN
        END;
      END;
    END;
  END MoveScreen;

  PROCEDURE MoveWindow;
    VAR c: ScrContext;
  BEGIN
    c:=sht^.ScreenContext;
    WITH c^ DO
      LOOP
        WndAborted:=FALSE;
        IF wndX1>scrX THEN
          LineXs:=scrX; LineXe:=wndX1; LineYs:=wndY1; LineYe:=wndY2;
          DoInBox(sht);
          IF NOT WndAborted THEN wndX1:=scrX ELSE RETURN END;
        ELSIF (scrX+(x2-x1))>wndX2 THEN
          LineXs:=wndX2; LineXe:=scrX+(x2-x1);
          LineYs:=wndY1; LineYe:=wndY2;
          DoInBox(sht);
          IF NOT WndAborted THEN wndX2:=scrX+(x2-x1) ELSE RETURN END;
        ELSIF wndY1>scrY THEN
          LineXs:=wndX1; LineXe:=wndX2; LineYs:=scrY; LineYe:=wndY1;
          DoInBox(sht);
          IF NOT WndAborted THEN wndY1:=scrY ELSE RETURN END;
        ELSIF scrY+(y2-y1)>wndY2 THEN
          LineXs:=wndX1; LineXe:=wndX2;
          LineYs:=wndY2; LineYe:=scrY+(y2-y1);
          DoInBox(sht);
          IF NOT WndAborted THEN wndY2:=scrY+(y2-y1) ELSE RETURN END;
        ELSE RETURN
        END;
      END;
    END;
  END MoveWindow;

  VAR xa, ya, xb, yb: INTEGER;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock);
    WITH sht^.PublicContext^ DO
      sx:=ScaleX; sy:=ScaleY; rollXs:=vg.rollX*sx; rollYs:=vg.rollY*sy;
      x1:=WindowW; x2:=WindowE; y1:=WindowS; y2:=WindowN;
    END;
    EXCL(Req,wnd);
    sch.release(Lock);
    IF NOT(wnd IN Bad)&(sx=wndsX)&(sy=wndsY)&
      (ABS(x1-wndX1)<rollXs)&(ABS(y1-wndY1)<rollYs)
    THEN RETURN END;
    IF (wnd IN Bad)OR(sx#wndsX)OR(sy#wndsY)OR
      (ABS(wndX1-x1)>((x2-x1) / 2))OR
      (ABS(wndY1-y1)>((y2-y1) / 2))OR
      (ABS(wndX2-x2)>((x2-x1) / 2))OR
      (ABS(wndY2-y2)>((y2-y1) / 2)) THEN
      vg.fill(sht^.wnd,15);
      Bad:=Bad+rs{wnd}; TextSet:={0..3};
      WndAborted:=FALSE;
      scrX:=x1; scrY:=y1;
      wndX1:=x1; wndY1:=y1; wndX2:=x2; wndY2:=y2; wndsX:=sx; wndsY:=sy;
      LineXs:=x1; LineXe:=x2; LineYs:=y1; LineYe:=y2;
      DoInBox(sht);
      IF NOT WndAborted THEN EXCL(Bad,wnd) END;
    ELSE
      CursorOFF(sht);
      CursordOFF(sht);
      MoveScreen;
      IF (Req*rs{wnd,stop})#rs{} THEN RETURN END;
      MoveWindow;
    END;
    WITH sht^ DO
      win.lock_window(sht^.wnd);
      --vg.greed(wnd,wndsX,scrX,scrY);
      vg.mode(wnd,vg.or); vg.color(wnd,4);
      wnd^.patt:={3..9,13..15,19..25,29..31};
      x1:=(        -scrX) / wndsX; y1:=(        -scrY) / wndsY;
      x2:=(mdl^.ctX-scrX) / wndsX; y2:=(mdl^.ctY-scrY) / wndsY;
      xa:=x1; ya:=y1; xb:=x2; yb:=y1;
      IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),0,wnd^.sy -(wnd^.S+wnd^.N),
                 xa,ya,xb,yb) THEN
        vg.patt_vect(wnd,xa+wnd^.W,ya+wnd^.S,xb+wnd^.W,yb+wnd^.S);
      END;
      xa:=x2; ya:=y1; xb:=x2; yb:=y2;
      IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),0,wnd^.sy -(wnd^.S+wnd^.N),
                 xa,ya,xb,yb) THEN
        vg.patt_vect(wnd,xa+wnd^.W,ya+wnd^.S,xb+wnd^.W,yb+wnd^.S);
      END;
      xa:=x2; ya:=y2; xb:=x1; yb:=y2;
      IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),0,wnd^.sy -(wnd^.S+wnd^.N),
                 xa,ya,xb,yb) THEN
        vg.patt_vect(wnd,xa+wnd^.W,ya+wnd^.S,xb+wnd^.W,yb+wnd^.S);
      END;
      xa:=x1; ya:=y2; xb:=x1; yb:=y1;
      IF vg.clip(0,wnd^.dsx-(wnd^.W+wnd^.E),0,wnd^.sy -(wnd^.S+wnd^.N),
                 xa,ya,xb,yb) THEN
        vg.patt_vect(wnd,xa+wnd^.W,ya+wnd^.S,xb+wnd^.W,yb+wnd^.S);
      END;
      wnd^.patt:={0..31};
      win.refresh(wnd);
      IF NOT WndAborted THEN
        sch.acquire(Lock);
        WITH PublicContext^ DO
          WindowW:=wndX1; WindowE:=wndX2; WindowS:=wndY1; WindowN:=wndY2;
          WindowX:=WindowW+((wnd^.dsx-(wnd^.W+wnd^.E)) / 2) * sx;
          WindowY:=WindowS+((wnd^. sy-(wnd^.S+wnd^.N)) / 2) * sy;
        END;
        sch.release(Lock);
      END;
      win.release_window(sht^.wnd);
    END;
  END;
END RefWindow;

PROCEDURE RefText(sht: Sheet);
  VAR i,j: INTEGER; ts: BITSET;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock);
    ts:=TextSet; TextSet:={}; EXCL(Req,txt);
    sch.release(Lock);
    vg.color(stat,2); vg.mode(stat,vg.rep);
    FOR i:=0 TO 3 DO
      IF i IN ts THEN
        j:=0;
        WHILE (TextBuf[i,j]#0c)&(j<=79) DO
          vg.Write(stat,j*vg.char_w+4,stat^.sy-1-(i+1)*vg.char_h-4,TextBuf[i,j]);
          INC(j)
        END;
        WHILE j<=79 DO
          vg.Write(stat,j*vg.char_w+4,stat^.sy-1-(i+1)*vg.char_h-4,' ');
          INC(j);
        END;
      END
    END;
    win.refresh(stat);
    EXCL(Bad,txt);
  END;
END RefText;

PROCEDURE Reftext(sht: Sheet);
  VAR ln,cl,col: INTEGER;
      s: ARRAY [0..79] OF CHAR;
      wt?: BOOLEAN;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH c^ DO
    sch.acquire(Lock);
    ln:=lin; cl:=colomn;
    col:=color; s:=textBuf;
    wt?:=wait?;
    EXCL(Req,txtr);
    sch.release(Lock);
    vg.color(stat,col); vg.mode(stat,vg.rep);
    vg.write_str(stat,cl*vg.char_w+4,stat^.sy-1-(ln+1)*vg.char_h-4,s);
    win.refresh(stat);
    IF wt? THEN sch.send(textOK) END;
  END;
END Reftext;

PROCEDURE AppSeg(sht: Sheet);
  VAR c: ScrContext; sg: seg;
BEGIN
  c:=sht^.ScreenContext;
  EXCL(c^.Req,app);
  sg.X1:=ModelMisc.X1; sg.X2:=ModelMisc.X2;
  sg.Y1:=ModelMisc.Y1; sg.Y2:=ModelMisc.Y2;
  sg.Size:=ModelMisc.Size; sg.Layer:=ModelMisc.Layer;
  IF ModelMisc.Signal=sht^.PublicContext^.Signal THEN
    sg.Layer:=sg.Layer+{2}
  END;
  DoConductor(sht,sg,vg.rep);
  win.ref_box(sht^.wnd,
      sht^.wnd^.sy-1-(Max(sg.Y1,sg.Y2)-c^.scrY+sg.Size) / c^.wndsY-sht^.wnd^.S,
      (ABS(sg.Y1-sg.Y2)+sg.Size*2) / c^.wndsY+2);
  sch.send(c^.SegOK);
END AppSeg;

PROCEDURE DelSeg(sht: Sheet);
  VAR c: ScrContext; sg: seg;
BEGIN
  c:=sht^.ScreenContext;
  EXCL(c^.Req,del);
  sg.X1:=ModelMisc.X1;     sg.X2:=ModelMisc.X2;
  sg.Y1:=ModelMisc.Y1;     sg.Y2:=ModelMisc.Y2;
  sg.Size:=ModelMisc.Size; sg.Layer:=ModelMisc.Layer;
  IF ModelMisc.Signal=sht^.PublicContext^.Signal THEN sg.Layer:=sg.Layer+{2} END;
  DoConductor(sht,sg,vg.bic);
  win.ref_box(sht^.wnd,
      sht^.wnd^.sy-1-(Max(sg.Y1,sg.Y2)-c^.scrY+sg.Size) / c^.wndsY-sht^.wnd^.S,
      (ABS(sg.Y1-sg.Y2)+sg.Size*2) / c^.wndsY+2);
  sch.send(c^.SegOK);
END DelSeg;

VAR SH: Sheet;

PROCEDURE ref;
  VAR sht: Sheet; c: ScrContext;
BEGIN
  sht:=SH; c:=sht^.ScreenContext;
  LOOP
    sch.wait(c^.RefRequest);
    LOOP
      sch.delay(50);
      IF c^.InDo THEN win.refresh(sht^.wnd) ELSE EXIT END;
    END;
  END;
END ref;

PROCEDURE Out;
  VAR sht: Sheet; c: ScrContext;
BEGIN
  sht:=SH;
  c:=sht^.ScreenContext;
  WITH c^ DO
    LOOP
      sch.wait(OutRequest);
      WHILE BITSET(Req+Bad)#{} DO
        IF    wnd  IN (Req+Bad) THEN RefWindow(sht);
        ELSIF crs  IN  Req      THEN RefCursor(sht);
        ELSIF dcrs IN  Req      THEN RefCursord(sht);
        ELSIF coff IN  Req      THEN CrsOff(sht);
        ELSIF txt  IN  Req      THEN RefText(sht);
        ELSIF txtr IN  Req      THEN Reftext(sht);
        ELSIF app  IN  Req      THEN AppSeg(sht);
        ELSIF del  IN  Req      THEN DelSeg(sht);
        ELSIF sig  IN  Req      THEN RefSig(sht);
        ELSIF chp  IN  Req      THEN RefChip(sht);
        END;
      END;
    END;
  END;
END Out;

PROCEDURE OpenWindow(sht: Sheet; scale,x,y: INTEGER);
  VAR i : INTEGER;
  VAR c: ScrContext;
      e: ex.Reaction; r: BOOLEAN;
BEGIN
  IF sht^.ScreenContext=NIL THEN
    r:=ex.Exception?(e);
    IF r THEN
      IF r#ex.MemoryOverflow THEN ex.RaiseInMe(r) END;
      IF sht^.ScreenContext#NIL THEN
        c:=sht^.ScreenContext;
        IF c^.Ref#NIL THEN rem_process(c^.Ref) END;
        IF c^.Outer#NIL THEN rem_process(c^.Outer) END;
        heap.Deallocate(sht^.ScreenContext,SIZE(c^));
      END;
      ex.Message:='Переполнена динамическая память, очень жаль...';
      ex.RaiseInMe(ex.MemoryOverflow);
    END;
    heap.Allocate(sht^.ScreenContext,SIZE(c^));
    c:=sht^.ScreenContext;
    WITH c^ DO
      Outer:=NIL; Ref:=NIL;
      Req:=rs{}; Bad:=rs{wnd}; TextSet:={0..3};
      IF sch.make_process(Outer,Out,2000)#0 THEN
        ex.Message:='Переполнена динамическая память, очень жаль...';
        ex.RaiseInMe(ex.MemoryOverflow);
      END;
      sch.ini_signal(OutHalted,{},0);
      sch.ini_signal(OutRequest,{},0);
      sch.ini_signal(RefRequest,{},0);
      sch.ini_signal(SegOK,{},0);
      sch.ini_signal(SigOK,{},0);
      sch.ini_signal(textOK,{},0);
      sch.ini_signal(crsOK,{},0);
      sch.ini_signal(chipOK,{},0);
      Outer^.halt:=ADR(OutHalted);
      sch.ini_mutex(Lock);
      NoCursor:=TRUE; NoCursord:=TRUE;
      chip_on:=FALSE;
      TextSet:={};
      FOR i:=0 TO 3 DO Strings.print(TextBuf[i],'') END;
      CurSig:=NIL; LastSig:=NIL;
      Chain:=NIL;
      crsX1:=0;  crsX2:=0;  crsY1:=0;  crsY2:=0;
      dcrsX1:=0; dcrsX2:=0; dcrsY1:=0; dcrsY2:=0;
      IF sch.make_process(Ref,ref,1000)#0 THEN
        ex.Message:='Переполнена динамическая память, очень жаль...';
        ex.RaiseInMe(ex.MemoryOverflow);
      END;
      SH:=sht; InDo:=FALSE;
      sch.start(Outer);
      sch.start(Ref);
    END;
    ex.KillReaction(e);
  END;
  SetWindow(sht,scale,x,y);
END OpenWindow;

PROCEDURE CloseWindow(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  rem_process(c^.Ref);
  rem_process(c^.Outer);
  heap.Deallocate(sht^.ScreenContext,SIZE(c^));
END CloseWindow;

PROCEDURE SetWindow0(sht: Sheet; s,x,y: INTEGER);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  WITH sht^.PublicContext^ DO
    ScaleX:=s; ScaleY:=ScaleX;
    WindowX:=x; WindowY:=y;
    WITH sht^ DO
      WindowW:=WindowX-((wnd^.dsx-(wnd^.W+wnd^.E)) / 2) * ScaleX;
      WindowE:=WindowX+((wnd^.dsx-(wnd^.W+wnd^.E)) / 2) * ScaleX;
      WindowS:=WindowY-((wnd^.sy -(wnd^.N+wnd^.S)) / 2) * ScaleY;
      WindowN:=WindowY+((wnd^.sy -(wnd^.N+wnd^.S)) / 2) * ScaleY;
    END;
  END;
END SetWindow0;

PROCEDURE SetWindow(sht: Sheet; s,x,y: INTEGER);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  sch.acquire(c^.Lock);
  SetWindow0(sht,s,x,y);
  INCL(c^.Req,wnd);
  sch.send(c^.OutRequest);
  sch.release(c^.Lock);
END SetWindow;

PROCEDURE HardSetWindow(sht: Sheet; s,x,y: INTEGER);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  sch.acquire(c^.Lock);
  SetWindow0(sht,s,x,y);
  INCL(c^.Req,wnd); INCL(c^.Bad,wnd);
  sch.send(c^.OutRequest);
  sch.release(c^.Lock);
END HardSetWindow;

PROCEDURE Cursor(sht: Sheet; x1,y1,x2,y2: INTEGER);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO
    sch.acquire(Lock);
    crsX1:=x1; crsY1:=y1; crsX2:=x2; crsY2:=y2;
    INCL(Req,crs);
    sch.send(OutRequest);
    sch.release(Lock);
  END;
END Cursor;

PROCEDURE Cursord(sht: Sheet; x1,y1,x2,y2: INTEGER);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO
    sch.acquire(Lock);
    dcrsX1:=x1; dcrsY1:=y1; dcrsX2:=x2; dcrsY2:=y2;
    INCL(Req,dcrs);
    sch.send(OutRequest);
    sch.release(Lock);
  END;
END Cursord;

PROCEDURE CursorOff(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO
    sch.acquire(Lock); INCL(Req,coff); sch.send(OutRequest);
    sch.release(Lock);
    sch.wait(crsOK);
  END;
END CursorOff;

PROCEDURE Text(sht: Sheet; ln: INTEGER; fmt:  ARRAY OF CHAR; SEQ a: WORD);
  VAR i: INTEGER;
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO
    sch.acquire(Lock);
    Strings.print(TextBuf[ln],fmt,a);
    INCL(TextSet,ln);
    INCL(Req,txt);
    sch.send(OutRequest);
    sch.release(Lock);
  END;
END Text;

PROCEDURE text(sht: Sheet; cl,ln,col: INTEGER; s:  ARRAY OF CHAR; wt: BOOLEAN);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO
    sch.acquire(Lock);
    Strings.print(textBuf,'%s',s);
    lin:=ln; colomn:=cl; color:=col;
    wait?:=wt;
    INCL(Req,txtr);
    sch.send(OutRequest);
    sch.release(Lock);
    IF wt THEN sch.wait(textOK) END;
  END;
END text;

PROCEDURE Chip(sht: Sheet; chip: Model.Object; X,Y,R,M: INTEGER);
  VAR c: ScrContext;
BEGIN
  IF R<0 THEN RETURN END;
  c:=sht^.ScreenContext;
  IF (c=NIL) OR (chip=NIL) THEN RETURN END;
  WITH c^ DO
    sch.acquire(Lock);
    INCL(Req,chp);
    cur_chip:=chip;
    chip_x:=X; chip_y:=Y;
    chip_r:=R; chip_m:=M;
    sch.send(OutRequest);
    sch.release(Lock);
    sch.wait(chipOK);
  END;
END Chip;

PROCEDURE Drow(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO INCL(Req,app); sch.send(OutRequest); sch.wait(SegOK) END;
END Drow;

PROCEDURE Delete(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO INCL(Req,del); sch.send(OutRequest); sch.wait(SegOK) END;
END Delete;

PROCEDURE UpdSig(sht: Sheet);
  VAR c: ScrContext;
BEGIN
  c:=sht^.ScreenContext;
  IF c=NIL THEN RETURN END;
  WITH c^ DO INCL(Req,sig); sch.send(OutRequest); sch.wait(SigOK) END;
END UpdSig;

BEGIN
  crs_bmd:=VG.layer[VG.layers-1]; mask:={0..15};
END pedScreen.
