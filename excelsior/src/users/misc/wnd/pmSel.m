IMPLEMENTATION MODULE pmSel; (*$X+N+ Nick & Leo 10-Aug-91. (c) KRONOS *)

FROM SYSTEM  IMPORT ADR,ADDRESS,WORD;

IMPORT  low: lowLevel;          IMPORT  tim: Time;
IMPORT  err: defErrors;         IMPORT  fnt: Fonts;
IMPORT  for: Formats;           IMPORT  str: Strings;
IMPORT  scr: Screen;            IMPORT  mem: Heap;
IMPORT  bio: BIO;               IMPORT  wnd: pmWnd;
IMPORT  cpd: CPD;               IMPORT  crs: pmCrs;
IMPORT  key: Keyboard;          IMPORT       ASCII;

IMPORT  tty: StdIO;
IMPORT  pm: pmPUP;

TYPE
  str32 = ARRAY [0..31] OF CHAR;
  SEL   = POINTER TO sbody;
  sbody = RECORD
            magic: INTEGER;
            state: BITSET;
            selec: BOOLEAN;
            CASE kind: INTEGER OF
              |_box,_button,
               _item,_radio  : name : str32;
              |_switch       : --???
              |_icon         : --???
              |_viewer       : vrol: WINDOW;
              |_scrollbar,
               _slider       : cpos: INTEGER;
                               clen: INTEGER;
                               ctop: INTEGER;
                               ptop: INTEGER;
                               osz : INTEGER;
                               asz : INTEGER;
                               lift: wnd.BLOCK;
                               ibut: wnd.BLOCK;
                               dbut: wnd.BLOCK;
                               srol: WINDOW;
              |_roller       : rpos: INTEGER;
                               rsel: INTEGER;
                               scrl: SEL;       --"RCSRL"
                               view: SEL;       --"RVIEW"
                               text: DYNARR OF STRING;
              |_menu         : mpos: INTEGER;
                               alts: ARRAY [0..31] OF STRING;
                               wdth: ARRAY [0..31] OF INTEGER;
                               caps: BITSET;
                               able: BITSET;
                               lalt: INTEGER;
                               salt: INTEGER;
              |_diabox       : dname: str32;
                               dmain: wnd.BLOCK;
                               dtool: wnd.TOOL;
                               dstr : STRING;
                               save : STRING;
                               cx,cp: INTEGER;
                               sp   : INTEGER;
              |_direx        : --???
              |_filer        : --???
            ELSE
            END
          END;

VAR nfont: fnt.FONT;
    bfont: fnt.FONT;
    ifont: fnt.FONT;
     char: CHAR;
       ch: CHAR;
      ank: BITSET;
      sum: BITSET;
    black: BITSET;
   shadow: BITSET;
   normal: BITSET;
   bright: BITSET;
   Smagic: INTEGER;

PROCEDURE strcopy(VAR d: ARRAY OF CHAR; s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;  low.zero(d);
  WHILE (i<=HIGH(s)) & (i<=HIGH(d)) & (s[i]#0c) DO
    d[i]:=s[i]; INC(i)
  END
END strcopy;

PROCEDURE checkname(VAL s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN (s[0]="S") & (s[1]="E") & (s[2]="L") & (s[3]=".")
END checkname;

PROCEDURE index(VAL s: ARRAY OF CHAR; ch: CHAR; VAR inx: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF ch=0c THEN RETURN FALSE END;
  FOR i:=0 TO HIGH(s) DO
    IF s[i]=ch THEN inx:=i; RETURN TRUE END
  END;
  RETURN FALSE
END index;

PROCEDURE stdprint(VAL s: WINDOW);
  VAR nm: str32;
      ss: ARRAY [0..127] OF CHAR;

  PROCEDURE _print(VAL w: WINDOW; i,k: INTEGER);
    VAR j: INTEGER;
       s0: WINDOW;
  BEGIN
    FOR j:=0 TO i-1 DO ss[j]:=" " END;
    WHILE (k<=HIGH(nm)) & (nm[k]#0c) DO ss[j]:=nm[k]; INC(k); INC(j) END;
    ss[j]:=0c;
    tty.print("%s\n",ss);
    wnd.iterobjects(w);
    WHILE wnd.nextobject(w,nm,s0) DO
      IF checkname(nm) THEN
        _print(s0,i+1,4);
        IF NOT done THEN RETURN END
      END
    END
  END _print;

BEGIN
  _print(s,0,0)
END stdprint;

PROCEDURE clearcpd;
  VAR s: BITSET; x,y: INTEGER;
BEGIN
  WHILE cpd.ready()>0 DO cpd.read(x,y,s) END;
END clearcpd;

VAR crssave: BOOLEAN;
    timeout: INTEGER;
     _start: INTEGER;
       time: INTEGER;

PROCEDURE con; BEGIN crs.toggle(crssave) END con;

PROCEDURE coff;
BEGIN
  crssave:=crs.on;
  IF crssave THEN crs.toggle(FALSE) END
END coff;

PROCEDURE startwait;
BEGIN
  _start:=tim.sys_time(tim.milisec); time:=timeout
END startwait;

PROCEDURE awaited(): BOOLEAN;
BEGIN
  time:=timeout-(tim.sys_time(tim.milisec)-_start);
  IF time<=0 THEN time:=0 END;
  RETURN time>0
END awaited;

PROCEDURE inblock(x,y: INTEGER; VAL b: wnd.BLOCK): BOOLEAN;
BEGIN
  RETURN (b.x<=x) & (x<=b.x+b.w-1) & (b.y<=y) & (y<=b.y+b.h-1)
END inblock;

PROCEDURE inwindow(v: WINDOW; VAL b: wnd.BLOCK): BOOLEAN;
  VAR x,y: INTEGER;
BEGIN
  x:=crs.x-v^.sx-b.x;
  y:=crs.y-v^.sy-b.y;
  RETURN (wnd.locate(crs.x,crs.y)=v) & (0<=x) & (x<b.w) & (0<=y) & (y<b.h)
END inwindow;

PROCEDURE inblocks(X,Y: INTEGER; SEQ b: wnd.BLOCK): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(b) DO
    WITH b[i] DO
      IF (x<=X) & (X<=x+w-1) & (y<=Y) & (Y<=y+h-1) THEN RETURN i END
    END
  END;
  RETURN -1
END inblocks;

PROCEDURE oncemore(b: wnd.BLOCK; VAR s: BITSET): BOOLEAN;
  VAR x,y,time: INTEGER;
BEGIN
  time:=tim.sys_time(tim.milisec)+0;
  REPEAT
    IF cpd.ready()>0 THEN
      crs.monitor;
      s:=cpd.state^.keys;
      IF NOT inblock(crs.x,crs.y,b) OR (s*{0}={}) THEN RETURN FALSE END
    END;
  UNTIL (time-tim.sys_time(tim.milisec))<=0;
  RETURN TRUE
END oncemore;

----------------------- keyboard & mouse -----------------------
                       ------------------

PROCEDURE _read(delay: INTEGER; VAR p,s: BITSET);
  VAR dx,dy: INTEGER;
BEGIN
  ch:=0c;
  IF (delay>0) & (key.ready()=0) THEN cpd.wait(delay) END;
  IF cpd.ready()>0 THEN
    p:=cpd.state^.keys;  cpd.read(dx,dy,s);
    IF p#s THEN RETURN END;
    IF  ABS(dx)+ABS(dy)#0      THEN crs.move(crs.x+dx,crs.y+dy) END
  ELSIF cpd.error#err.time_out THEN key.wait(20)
  END;
  IF key.ready()>0 THEN key.read(ch) END
END _read;

PROCEDURE _kread(): BOOLEAN;
  VAR s,p: BITSET;
    dx,dy: INTEGER;
BEGIN
  ch:=0c;
  IF key.ready()=0 THEN cpd.wait(300) END;
  IF cpd.ready()>0 THEN
    p:=cpd.state^.keys;  cpd.read(dx,dy,s);
    IF p#s THEN RETURN TRUE END;
    IF  ABS(dx)+ABS(dy)#0      THEN crs.move(crs.x+dx,crs.y+dy) END
  ELSIF cpd.error#err.time_out THEN key.wait(20)
  END;
  IF key.ready()>0 THEN key.read(ch); RETURN TRUE END;
  RETURN FALSE
END _kread;

PROCEDURE _mread(timeout: INTEGER): BOOLEAN;
  VAR keys: BITSET;
     dx,dy: INTEGER;
BEGIN
  ch:=0c;
  IF key.ready()>0 THEN key.read(ch); RETURN FALSE END;
  WHILE cpd.ready()>0 DO      cpd.read(dx,dy,keys);
    IF ABS(dx)+ABS(dy)#0 THEN crs.move(crs.x+dx,crs.y+dy) END
  END;
  cpd.wait(timeout);
  RETURN cpd.ready()=0
END _mread;

PROCEDURE mwait(keys: BITSET; SEQ k: CHAR);
  VAR i: INTEGER;
    x,y: INTEGER;
    p,s: BITSET;
BEGIN
  startwait;
  done:=TRUE;  ch:=0c;  p:={};  s:={};
  LOOP
    _read(20,p,s);
    IF ch#0c THEN
      IF ((ch=15c) OR (ch=33c)) & (HIGH(k)<0) THEN EXIT END;
      IF (HIGH(k)=0) & (k[0]=0c)              THEN EXIT END;
      IF index(k,ch,i)                        THEN EXIT END
    END;
    IF (p*keys={}) & (s*keys#{}) THEN EXIT END;
    IF NOT awaited()             THEN EXIT END
  END
END mwait;

PROCEDURE wait(SEQ k: CHAR);
BEGIN mwait({0..cpd.state^.nokeys-1},k) END wait;

-------------------------- E r r o r s -------------------------
                          -------------

PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm  END bad_parm;
PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc  END bad_desc;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error     END mem_error;
PROCEDURE bio_error; BEGIN done:=FALSE; error:=bio.error     END bio_error;
PROCEDURE wnd_error; BEGIN done:=FALSE; error:=wnd.error     END wnd_error;
PROCEDURE duplicate; BEGIN done:=FALSE; error:=err.duplicate END duplicate;
PROCEDURE no_entry;  BEGIN done:=FALSE; error:=err.no_entry  END no_entry;
PROCEDURE undef;     BEGIN done:=FALSE; error:=err.undef     END undef;

-------------------------- M e m o r y -------------------------
                          -------------

PROCEDURE allocate(VAR a: ADDRESS; size: INTEGER);
BEGIN
  mem.allocate(a,size); done:=mem.done;
  IF NOT done THEN error:=mem.error END
END allocate;

PROCEDURE reallocate(VAR    a: ADDRESS;
                     VAR high: INTEGER; len,bytesperelem: INTEGER);
BEGIN
  mem.reallocate(a,high,len,bytesperelem); done:=mem.done;
  IF NOT done THEN error:=mem.error END
END reallocate;

WITH STORAGE (NEW    : allocate;
              DISPOSE: mem.deallocate;
              RESIZE : reallocate);

PROCEDURE _len(a: ADDRESS; str: ARRAY OF CHAR; p,l: INTEGER);
  VAR ptr: POINTER TO INTEGER;
BEGIN
  ptr:=a; ptr^:=ptr^+l
END _len;

PROCEDURE sprint(VAR s: STRING; fmt: ARRAY OF CHAR; SEQ arg: WORD);
  VAR i: INTEGER;
BEGIN
  i:=1;
  for.format(ADR(i),_len,fmt,arg);
  RESIZE(s,i);
  IF NOT done THEN RETURN END;
  str.print(s,fmt,arg)
END sprint;

-------------- D i s p l a y  P r o c e d u r e s --------------
              -------------------------------------

PROCEDURE shbox(s: WINDOW; name: ARRAY OF CHAR);
  VAR i,j: INTEGER;
    t0,t1: INTEGER;
    x1,y1: INTEGER;
BEGIN
  i:=wnd.width(nfont,name)+bfont^.W;
  x1:=s^.w-1;
  y1:=s^.h-1-nfont^.H DIV 2;
  IF s^.w>i THEN i:=(s^.w-i) DIV 2;  t0:=i; t1:=x1-i
  ELSE                               t0:=3; t1:=x1
  END;
  WITH s^ DO
    tool.color:=normal; wnd.rect(s,tool,0   ,0   ,x1  ,h -1);
    tool.color:=shadow; wnd.line(s,tool,0   ,0   ,0   ,y1  );
                        wnd.line(s,tool,0   ,y1  ,t0  ,y1  );
                        wnd.line(s,tool,t1  ,y1  ,x1  ,y1  );
                        wnd.line(s,tool,x1-1,y1-2,x1-1,1   );
                        wnd.line(s,tool,2   ,1   ,x1-1,1   );
    tool.color:=bright; wnd.line(s,tool,1   ,0   ,1   ,y1-1);
                        wnd.line(s,tool,1   ,y1-1,t0  ,y1-1);
                        wnd.line(s,tool,t1  ,y1-1,x1  ,y1-1);
                        wnd.line(s,tool,x1  ,y1-1,x1  ,0   );
                        wnd.line(s,tool,1   ,0   ,x1  ,0   );
    tool.color:=black;  i:=s^.h-bfont^.H+bfont^.bline;
    tool.back :=normal; wnd.print(s,tool,t0+bfont^.W DIV 2,i,bfont,name)
  END
END shbox;

PROCEDURE bframe(s: WINDOW; p: BOOLEAN; x0,y0,x1,y1: INTEGER);
  VAR t: wnd.TOOL;
BEGIN
  t:=s^.tool;
  WITH s^ DO
    IF p THEN t.color:=shadow ELSE t.color:=bright END;
    wnd.line(s,t,x0,y0,x0,y1);
    wnd.line(s,t,x0,y1,x1,y1);
    IF p THEN t.color:=bright ELSE t.color:=black  END;
    wnd.line(s,t,x0,y0,x1,y0);
    wnd.line(s,t,x1,y0,x1,y1);
    INC(x0);  DEC(x1);
    INC(y0);  DEC(y1);
    IF p THEN t.color:=black  ELSE t.color:=normal END;
    wnd.line(s,t,x0,y0,x0,y1);
    wnd.line(s,t,x0,y1,x1,y1);
    IF p THEN t.color:=normal ELSE t.color:=shadow END;
    wnd.line(s,t,x0,y0,x1,y0);
    wnd.line(s,t,x1,y0,x1,y1)
  END
END bframe;

PROCEDURE shblock(s: WINDOW; p: BOOLEAN; c: BITSET; name: ARRAY OF CHAR);
  VAR t: wnd.TOOL;
    i,j: INTEGER;
BEGIN
  WITH s^ DO
    t:=tool;     bframe(s,p,0,0,w-1,h-1);
    t.color:=c;  wnd.rect(s,t,2,2,w-3,h-3);
    IF name="" THEN RETURN END;
    t.color:=black;
    t.back :=c;
    i:=(w-wnd.width(nfont,name)) DIV 2;
    j:=nfont^.bline+(h-nfont^.H) DIV 2;
    wnd.print(s,t,i,j,nfont,name)
  END
END shblock;

PROCEDURE shsicon(s: WINDOW; X,Y: INTEGER; i: INTEGER);
  VAR t: wnd.TOOL;
BEGIN
  t:=s^.tool;
  t.color:=normal; wnd.print(s,t,X,Y,ifont,"%c",i*2+1);  t.mode:=wnd.or;
  t.color:=shadow; wnd.print(s,t,X,Y,ifont,"%c",i*2  );  t.mode:=wnd.rep
END shsicon;

PROCEDURE shswitch(s: WINDOW; p: BOOLEAN; i: INTEGER);
BEGIN
  bframe(s,p,0,0,msize-1,msize-1);
  shsicon(s,2,2,i)
END shswitch;

PROCEDURE shradio(s: WINDOW; p: BOOLEAN; n: ARRAY OF CHAR);
  VAR tl: wnd.TOOL;
   r,x,y: INTEGER;
   ax,ay: INTEGER;
   bx,by: INTEGER;
BEGIN
  WITH s^ DO
    r:=nfont^.H DIV 2-1;  x:=r+1;      ax:=x-r;    bx:=x+r+2;
    tl:=tool;             y:=h DIV 2;  ay:=y-r-2;  by:=y+r;
    tl.color:=normal;     wnd.rect(s,tl,0,0,w-1 ,h-1 );
    tl.color:=shadow;     wnd.arc (s,tl,x,y,bx-2,by  ,ax  ,ay+2,r+1);
    tl.color:=bright;     wnd.arc (s,tl,x,y,ax  ,ay-2,bx+2,by  ,r+1);
    IF NOT p THEN tl.color:=normal END;
    wnd.circlef(s,tl,x,y,r-1);
    tl.back :=normal;
    tl.color:=black;
    wnd.print(s,tl,r*4,(h-nfont^.H) DIV 2-nfont^.bline,nfont,n)
  END
END shradio;

PROCEDURE shfield(s: WINDOW; b: wnd.BLOCK);
BEGIN
  s^.tool.color:=shadow;
  WITH b DO
    wnd.line(s,s^.tool,x    ,y+1  ,x    ,y+h-1);
    wnd.line(s,s^.tool,x    ,y+h-1,x+w-1,y+h-1);  s^.tool.color:=bright;
    wnd.line(s,s^.tool,x+w-1,y    ,x+w-1,y+h-2);
    wnd.line(s,s^.tool,x    ,y    ,x+w-1,y    );  s^.tool.color:=shadow;
    wnd.line(s,s^.tool,x+1  ,y+2  ,x+1  ,y+h-2);
    wnd.line(s,s^.tool,x+1  ,y+h-2,x+w-2,y+h-2);  s^.tool.color:=normal;
    wnd.line(s,s^.tool,x+w-2,y+1  ,x+w-2,y+h-2);
    wnd.line(s,s^.tool,x+1  ,y+1  ,x+w-2,y+1  );  s^.tool.color:=bright;
    INC(x,2); INC(y,2); DEC(w,4); DEC(h,4);
    wnd.rect(s,s^.tool,x,y,x+w-1,y+h-1)
  END
END shfield;

------------------ R a d i o  &  B u t t t o n -----------------
                  -----------------------------

PROCEDURE aselect(VAL s: WINDOW; VAL ss: SEL);
  VAR p: BITSET;
      b: wnd.BLOCK;
     tl: wnd.TOOL;
     rc: INTEGER;
  x0,y0: INTEGER;
BEGIN
  WITH s^ DO
    rc:=nfont^.H DIV 2-1;  x0:=rc+1;     b.x:=sx;        b.w:=rc*2;
    tl:=tool;              y0:=h DIV 2;  b.y:=sy+y0-rc;  b.h:=b.w;
    REPEAT
      p:=cpd.state^.keys;
      crs.monitor;
      IF wnd.locate(crs.x,crs.y)#s THEN RETURN END
    UNTIL (p*{0}#{}) & (cpd.state^.keys={}) & inblock(crs.x,crs.y,b);
    ss^.state:=ss^.state/pressed;
    IF ss^.state*pressed={} THEN tl.color:=normal ELSE tl.color:=bright END;
    coff; wnd.circlef(s,tl,x0,y0,rc-1); con
  END
END aselect;

PROCEDURE bselect(VAL s: WINDOW; VAL ss: SEL);
  VAR p: BITSET;
BEGIN
  REPEAT
    p:=cpd.state^.keys;
    crs.monitor;
    IF wnd.locate(crs.x,crs.y)#s THEN
    RETURN END
  UNTIL (p*{0}#{}) & (cpd.state^.keys={});
  ss^.state:=ss^.state/pressed;
  coff; bframe(s,ss^.state*pressed#{},0,0,s^.w-1,s^.h-1); con
END bselect;

-------------------------- D i a b o x -------------------------
                          -------------

PROCEDURE dxwrite(s: WINDOW; ss: SEL; x: INTEGER; ch: CHAR): INTEGER;
BEGIN
  WITH ss^.dtool DO
    RETURN wnd.xprint(s,ss^.dtool,x+clip.x,clip.y,nfont,"%c",ch)-clip.x
  END
END dxwrite;

PROCEDURE droll(s: WINDOW; ss: SEL; left: BOOLEAN;);
  VAR r: wnd.TOOL;
    j,w: INTEGER;
BEGIN
  WITH ss^ DO
    r:=dtool; r.color:=bright; r.mode:=wnd.rep;
    IF left THEN
      j:=0; w:=0;
      WHILE cx+nfont^.W-w>r.clip.w DO
        w:=w+wnd.lenght(nfont,"%c",dstr[sp+j]); INC(j)
      END;
      wnd.scroll(s,r,w,0); INC(sp,j); DEC(cx,w)
    ELSE
      DEC(sp);
      w:=wnd.lenght(nfont,"%c",dstr[sp]);
      wnd.scroll(s,r,-w,0);   INC(cx,w);
      j:=dxwrite(s,ss,0,dstr[sp])
    END
  END
END droll;

PROCEDURE dputstr(s: WINDOW; ss: SEL);
  VAR t: wnd.TOOL;
BEGIN
  WITH ss^ DO
    t:=dtool;
    t.color:=bright; t.mode:=wnd.rep;  wnd.fill(s,t,t.clip,32,-1);
    cx:=0;  cp:=0;  sp:=0;
    IF (HIGH(save)>0) & (save#"") THEN
      strcopy(dstr,save);
      WHILE (cp<HIGH(save)) & (save[cp]#0c) DO
        cx:=dxwrite(s,ss,cx,save[cp]);
        INC(cp);
        IF cx+nfont^.W>dtool.clip.w THEN droll(s,ss,TRUE) END
      END
    END
  END
END dputstr;

PROCEDURE dselect(s: WINDOW; ss: SEL);
  VAR i: INTEGER;  erase: BOOLEAN;
     sh: INTEGER;    r,k: BITSET;

  PROCEDURE marker;
    VAR c: wnd.TOOL; x,y: INTEGER;
  BEGIN
    x:=ss^.dmain.x+2+ss^.cx;       c:=ss^.dtool;
    y:=ss^.dmain.y+2;              c.mode:=wnd.rep;
    c.color:=bright;  wnd.rect(s,c,x,y,x+nfont^.W,y+nfont^.H-1);
    IF erase THEN erase:=FALSE; RETURN ELSE erase:=TRUE END;
    c.color:=black;   wnd.line(s,c,x,y,x,y+nfont^.H-1)
  END marker;

BEGIN
  sh:=nfont^.H;   done:=TRUE;   ss^.selec:=FALSE;
  startwait;     erase:=FALSE;
  WHILE cpd.state^.keys*{0}={} DO
   crs.monitor;
   IF wnd.locate(crs.x,crs.y)#s THEN RETURN END
  END;
  coff;
  LOOP
    REPEAT marker UNTIL _kread(); erase:=TRUE; marker;
    WITH ss^ DO
      r:=cpd.state^.keys;
      IF    r*{0}#{} THEN selec:=TRUE;  EXIT
      ELSIF r*ank#{} THEN selec:=FALSE; EXIT
      ELSIF (cp>0) & ((ch=key.back) OR (ch=key.left)) THEN
        DEC(cp);
        DEC(cx,wnd.lenght(nfont,"%c",dstr[cp]));
        WHILE (sp>0) & (cx+nfont^.W<dtool.clip.w) DO droll(s,ss,FALSE) END
      ELSIF (ORD(ch) MOD 128 >= 32) & (cp<HIGH(dstr)) THEN
        dstr[cp]:=ch;
        cx:=dxwrite(s,ss,cx,dstr[cp]);
        INC(cp);
        IF cx+nfont^.W*2>dtool.clip.w THEN droll(s,ss,TRUE) END;
      ELSIF ch=15c THEN selec:=TRUE;  EXIT
      ELSIF ch=33c THEN selec:=FALSE; EXIT
      END;
      dstr[cp]:=0c
    END
  END;
  IF ss^.selec THEN strcopy(ss^.save,ss^.dstr) ELSE dputstr(s,ss) END;
  con
END dselect;

PROCEDURE shdiabox(VAL s: WINDOW; VAL ss: SEL);
  VAR i: INTEGER;
      t: wnd.TOOL;
BEGIN
  WITH ss^ DO
    dtool:=s^.tool;
    WITH dmain DO                                     dtool.color:=normal;
      wnd.rect(s,dtool,0,0,w-1,h-1);                  dtool.color:=shadow;
      wnd.line(s,dtool,x    ,y+1  ,x    ,y+h-1);
      wnd.line(s,dtool,x    ,y+h-1,x+w-1,y+h-1);      dtool.color:=bright;
      wnd.line(s,dtool,x+w-1,y    ,x+w-1,y+h-2);
      wnd.line(s,dtool,x    ,y    ,x+w-1,y    );      dtool.color:=shadow;
      wnd.line(s,dtool,x+1  ,y+2  ,x+1  ,y+h-2);
      wnd.line(s,dtool,x+1  ,y+h-2,x+w-2,y+h-2);      dtool.color:=normal;
      wnd.line(s,dtool,x+w-2,y+1  ,x+w-2,y+h-2);
      wnd.line(s,dtool,x+1  ,y+1  ,x+w-2,y+1  );      dtool.color:=bright;
      wnd.rect(s,dtool,x+2  ,y+2  ,x+w-3,y+h-3);      dtool.color:=black;
      i:=x-nfont^.W DIV 2-wnd.width(nfont,dname);     dtool.back :=normal;
      wnd.print(s,dtool,i,2+nfont^.bline,nfont,dname)
    END;
    WITH dtool DO
      mode :=wnd.xor;   clip.x:=ss^.dmain.x+2;  clip.w:=ss^.dmain.w-4;
      back :={};        clip.y:=ss^.dmain.y+2;  clip.h:=ss^.dmain.h-4;
      color:=bright/black
    END
  END;
  dputstr(s,ss)
END shdiabox;

PROCEDURE _dnew(VAL s: WINDOW; VAL ss: SEL);
BEGIN
  WITH ss^ DO
    cx:=0;  sp   :=0;   NEW(dstr);
    cp:=0;  dname:="";  NEW(save);
    WITH dmain DO
      w:=s^.w*2 DIV 3;  x:=s^.w-w;
      h:=msize;         y:=0
    END
  END
END _dnew;

--------------- S c r o l l b a r  &  S l i d e r --------------
               -----------------------------------

PROCEDURE bswap(VAR b: wnd.BLOCK);
  VAR i: INTEGER;
BEGIN
  i:=b.x; b.x:=b.y; b.y:=i;
  i:=b.w; b.w:=b.h; b.h:=i
END bswap;

PROCEDURE _snew(s: WINDOW; ss: SEL; a,o: INTEGER);
  VAR i: INTEGER;
BEGIN
  WITH ss^ DO
    IF s^.w<s^.h THEN i:=s^.h ELSE i:=s^.w END;
    lift.x:=1; lift.h:=msize; dbut.x:=0; dbut.w:=0; cpos:=0; ptop:=0; osz:=o;
    lift.y:=1; lift.w:=i-2;   dbut.y:=0; dbut.h:=0; clen:=0; ctop:=0; asz:=a;
    ibut:=dbut
  END
END _snew;

PROCEDURE _smake(ss: SEL);
  VAR i: INTEGER;
BEGIN
  WITH ss^ DO
    IF kind=_slider THEN clen:=msize*3 DIV 2+1; RETURN END;
    IF (ss^.state*disabled#{}) OR (osz<=asz) THEN RETURN END;
    dbut:=lift;  dbut.w:=msize;     dbut.h:=msize;  i:=msize+1;
    ibut:=dbut;  ibut.x:=ibut.x+i;                  i:=i*2+1;
    cpos:=0;     lift.x:=lift.x+i;
    ctop:=0;     lift.w:=lift.w-i;    ptop:=0;      i:=msize*3 DIV 2;
    clen:=lift.w*asz DIV osz;
    IF clen<i THEN
      IF lift.w>i THEN clen:=i ELSE clen:=0; RETURN  END
    END;
    IF ss^.state*{31}#{} THEN cpos:=lift.w-clen END
  END
END _smake;

PROCEDURE chess0(s: WINDOW; VAL b: wnd.BLOCK);
  VAR t: wnd.TOOL;
BEGIN
  t:=s^.tool;
  t.mode :=wnd.xor;
  t.color:=normal/shadow;
  wnd.fill(s,t,b,32,0AAAAAAAAh,055555555h)
END chess0;

PROCEDURE chess1(s: WINDOW; VAL b: wnd.BLOCK);
  VAR t: wnd.TOOL;
BEGIN
  t:=s^.tool;
  WITH t DO
    color:=normal;       wnd.rect(s,t,b.x,b.y,b.x+b.w-1,b.y+b.h-1);
    mode:=wnd.xor;
    color:=color/shadow; wnd.fill(s,t,b,32,0AAAAAAAAh,055555555h)
  END
END chess1;

PROCEDURE _slifter(s: WINDOW; b: wnd.BLOCK; m: BOOLEAN);
  VAR tl: wnd.TOOL;
   x0,x1: INTEGER;
 j,y0,y1: INTEGER;
BEGIN
  x0:=b.x; x1:=b.x+b.w-1;
  y0:=b.y; y1:=b.y+b.h-1;
  bframe(s,FALSE,x0,y0,x1,y1);
  tl:=s^.tool;
  tl.color:=normal; wnd.rect(s,tl,x0+2,y0+2,x1-2,y1-2);
  tl.color:=sum;
  IF m THEN
    x0:=x0+(b.w-ifont^.W) DIV 2;
    y0:=y0+(b.h-ifont^.H) DIV 2;
    tl.mask:=shadow;  wnd.writech(s,tl,x0,y0,ifont,26c);
    tl.mask:=normal;  wnd.writech(s,tl,x0,y0,ifont,27c)
  ELSIF b.w>b.h THEN
    x0:=x0+b.w DIV 2-1;
    INC(y0); DEC(y1);
    tl.color:=shadow;  wnd.line(s,tl,x0,y0,x0,y1); INC(x0);
    tl.color:=bright;  wnd.line(s,tl,x0,y0,x0,y1)
  ELSE
    y0:=y0+b.h DIV 2;
    INC(x0); DEC(x1);
    tl.color:=bright;  wnd.line(s,tl,x0,y0,x1,y0); INC(y0);
    tl.color:=shadow;  wnd.line(s,tl,x0,y0,x1,y0)
  END
END _slifter;

PROCEDURE _sshow(s: WINDOW; ss: SEL);
  VAR t: wnd.TOOL;
      c: wnd.BLOCK;
      i: INTEGER;
BEGIN
  t:=s^.tool;       i:=9;
  t.color:=normal;  wnd.rect(s,t,0,0,s^.w-1,s^.h-1);
  WITH ss^ DO
    t.color:=black;
    IF kind=_slider THEN
      wnd.line(s,t,0,     0,     0,s^.h-1);
      wnd.line(s,t,0,s^.h-1,s^.w-1,s^.h-1);  t.color:=bright;
      wnd.line(s,t,     1,0,s^.w-1,     0);
      wnd.line(s,t,s^.w-1,0,s^.w-1,s^.h-2)
    ELSIF s^.w<s^.h THEN
      wnd.line(s,t,s^.w-1,0,s^.w-1,s^.h-1)
    ELSE
      wnd.line(s,t,0,s^.h-1,s^.w-1,s^.h-1)
    END;
    c  :=lift;
    c.x:=c.x+cpos;
    c.w:=clen;
    IF s^.w<s^.h THEN
      bswap(dbut);  bswap(ibut);  bswap(lift);  bswap(c);  DEC(i,2)
    END;
    t.mode:=wnd.xor;  t.color:=normal/shadow;
    wnd.fill(s,t,lift,32,0AAAAAAAAh,055555555h);
    IF kind=_slider THEN _slifter(s,c,FALSE)
    ELSIF dbut.w>0  THEN
      bframe(s,FALSE,dbut.x,dbut.y,dbut.x+dbut.w-1,dbut.y+dbut.h-1);
      bframe(s,FALSE,ibut.x,ibut.y,ibut.x+ibut.w-1,ibut.y+dbut.h-1);
      shsicon(s,dbut.x+2,dbut.y+2,i  );
      shsicon(s,ibut.x+2,ibut.y+2,i+1);
      _slifter(s,c,TRUE)
    END;
    IF s^.w<s^.h THEN bswap(dbut); bswap(ibut); bswap(lift); bswap(c) END
  END
END _sshow;

PROCEDURE _smove(VAL s: WINDOW; VAL ss: SEL; np: INTEGER);
  VAR d,m: INTEGER;
      l,b: wnd.BLOCK;
BEGIN
  WITH ss^ DO
    IF np+clen>lift.w THEN np:=lift.w-clen ELSIF np<0 THEN np:=0 END;
    d:=np-cpos;
    IF d=0 THEN RETURN END;
    b:=lift;  b.x:=b.x+cpos;  b.w:=clen;
    l:=b;
    IF ABS(d)<clen THEN
      l.w:=ABS(d);
      IF d<0 THEN l.x:=b.x+b.w+d END
    END;
    IF s^.w<s^.h THEN
      bswap(b);  bswap(l);
      wnd.bblt(s,s^.tool,b.x,b.y+d,s,s^.tool,b)
    ELSE
      wnd.bblt(s,s^.tool,b.x+d,b.y,s,s^.tool,b)
    END;
    chess1(s,l);
    cpos:=np
  END
END _smove;

PROCEDURE _rscroll(s: WINDOW; sc: SEL);  FORWARD;

PROCEDURE _spos(VAL s: WINDOW; VAL ss: SEL; t: INTEGER);
  VAR i,j,p: INTEGER;
BEGIN
  WITH ss^ DO
    i:=osz-asz;
    j:=lift.w-clen;
    IF ctop=t THEN RETURN
    ELSIF t<0 THEN t:=0; p:=0;
    ELSIF t>i THEN t:=i; p:=j
    ELSE                 p:=j*t DIV i
    END;
    IF state*{31}#{} THEN p:=lift.w-clen-p END;
    _smove(s,ss,p); ptop:=ctop; ctop:=t;
    IF (state*{31}#{}) & (ctop#ptop) THEN _rscroll(srol,ss) END
  END
END _spos;

PROCEDURE sselect(VAL s: WINDOW; VAL ss: SEL);
  VAR i: INTEGER; cb,cn: wnd.BLOCK;
      v: BOOLEAN; ib,db: wnd.BLOCK;

   PROCEDURE pushbutton(b: wnd.BLOCK; d: INTEGER);
   BEGIN
     IF ss^.state*{31}#{} THEN d:=-d END;
     coff;
     bframe(s,TRUE ,b.x,b.y,b.x+b.w-1,b.y+b.h-1);
     _spos(s,ss,ss^.ctop+d);
     REPEAT
       IF _mread(20) THEN _spos(s,ss,ss^.ctop+d) END
     UNTIL NOT inwindow(s,b) OR (cpd.state^.keys*{0}={});
     bframe(s,FALSE,b.x,b.y,b.x+b.w-1,b.y+b.h-1);
     con
   END pushbutton;

BEGIN
  WITH ss^ DO
    v:=s^.w<s^.h;        ib:=ibut;      ptop:=ctop;
    db:=dbut;            cn:=lift;
    IF v THEN bswap(ib); bswap(db); bswap(cn) END;
    IF cpd.state^.keys*{0}#{} THEN
      IF    inwindow(s,db) THEN pushbutton(db,-1)
      ELSIF inwindow(s,ib) THEN pushbutton(ib,+1)
      ELSIF inwindow(s,cn) THEN coff;
        cb:=crs.clip;
        crs.setclip(crs.cur,cn);
        REPEAT
          IF v THEN i:=crs.y-s^.sy ELSE i:=crs.x-s^.sx END;
          i:=i-lift.x-clen DIV 2;
          IF state*{31}#{} THEN i:=lift.w-clen-i END;
          i:=(osz-asz)*i DIV (lift.w-clen);
          _spos(s,ss,i);
          crs.monitor;
        UNTIL cpd.state^.keys*{0}={};
        con;
(*
        REPEAT
          IF v THEN i:=crs.y-s^.sy ELSE i:=crs.x-s^.sx END;
         _smove(s,ss,i-lift.x-clen DIV 2);
          crs.monitor;
        UNTIL cpd.state^.keys*{0}={};
        con;
        i:=cpos;
        IF state*{31}#{} THEN i:=lift.w-clen-i END;
        i:=(osz-asz)*i DIV (lift.w-clen);
        _spos(s,ss,i);
*)
        crs.setclip(crs.cur,cb)
      END
    END;
  END
END sselect;

-------------------------- R o l l e r -------------------------
                          -------------

PROCEDURE _create(VAR s: WINDOW; k: INTEGER;
             x,y,w,h: INTEGER; d,r: WINDOW); FORWARD;

PROCEDURE _rnew(VAR s: WINDOW; VAL ss: SEL);

  PROCEDURE rnew;
    VAR t: wnd.TOOL;
        i: INTEGER;
    v0,v1: WINDOW;
  BEGIN
    i:=msize+3;
    _create(v0,_scrollbar,2,2,i,s^.h-4,s,s);       IF NOT done THEN RETURN END;
    _create(v1,_viewer,i+2,2,s^.w-i-4,s^.h-4,s,s); IF NOT done THEN RETURN END;
    wnd.assign(s,"SEL.RSCRL",v0);   IF NOT wnd.done THEN wnd_error; RETURN END;
    wnd.assign(s,"SEL.RVIEW",v1);   IF NOT wnd.done THEN wnd_error; RETURN END;
    t:=v1^.tool; t.color:=normal;
    wnd.fill(v1,t,t.clip,32,-1);
    ss^.rpos:= 0; NEW(ss^.text);
    ss^.rsel:=-1; ss^.state:=ss^.state+pressed
  END rnew;

BEGIN
  rnew;
  IF NOT done THEN dispose(s) END
END _rnew;

PROCEDURE _refstr(s,sv: WINDOW; ss,ssc: SEL; pos,len: INTEGER; clear: BOOLEAN);
  VAR t: wnd.TOOL;
    i,j: INTEGER;
BEGIN
  j:=pos-ssc^.ctop;
  IF (j<0) OR (j>ssc^.asz-1) THEN RETURN END;
  t:=sv^.tool;
  i:=sv^.h-(j+1)*nfont^.H;
  IF clear THEN t.color:=normal; wnd.rect(sv,t,0,i,sv^.w,i+len*nfont^.H-1) END;
  t.color:=black;  INC(i,nfont^.bline);
  FOR pos:=pos TO pos+len-1 DO
    IF pos#ss^.rsel THEN t.back:=normal ELSE t.back:=bright END;
    IF pos>HIGH(ss^.text) THEN RETURN END;
    wnd.print(sv,t,4,i,nfont,ss^.text[pos]);
    DEC(i,nfont^.H)
  END
END _refstr;

PROCEDURE _rprint(s: WINDOW; ss: SEL; f: ARRAY OF CHAR; SEQ a: WORD);
  VAR sc: WINDOW; ssc: SEL;
      sv: WINDOW; ssv: SEL;
BEGIN
 wnd.object(s,"SEL.RSCRL",sc);  IF NOT wnd.done THEN wnd_error; RETURN END;
 wnd.object(s,"SEL.RVIEW",sv);  IF NOT wnd.done THEN wnd_error; RETURN END;
 wnd.object(sc,"SEL",ssc);      IF NOT wnd.done THEN wnd_error; RETURN END;
 wnd.object(sv,"SEL",ssv);      IF NOT wnd.done THEN wnd_error; RETURN END;
  WITH ss^ DO
    IF rpos>HIGH(text) THEN
      RESIZE(text,rpos+1);
      IF NOT done THEN RETURN END;
      NEW(text[rpos]);
      _snew(sc,ssc,(sv^.h+nfont^.H-1) DIV nfont^.H,rpos+2);
      _smake(ssc);
      _sshow(sc,ssc)
    END;
    sprint(text[rpos],f,a);
    IF NOT done THEN RETURN END;
    _refstr(s,sv,ss,ssc,rpos,1,TRUE)
  END
END _rprint;

PROCEDURE _rscroll(VAL s: WINDOW; VAL sc: SEL);
  VAR v: WINDOW;
     ss: SEL;
    i,j: INTEGER;
BEGIN
  ASSERT(s#NIL,err.bad_desc);
  wnd.object(s,"SEL.RVIEW",v);     ASSERT(wnd.done,wnd.error);
  wnd.object(s,"SEL",ss);          ASSERT(wnd.done,wnd.error);
  v^.tool.color:=normal;
  wnd.mode(v,wnd.img);
  WITH sc^ DO
    j:=ptop-ctop;
    IF j=0 THEN RETURN END;
    wnd.scroll(v,v^.tool,0,j*nfont^.H);
    IF ABS(j)>=asz THEN  _refstr(s,v,ss,sc,ctop      ,asz,FALSE)
    ELSIF  j>0     THEN  _refstr(s,v,ss,sc,ctop      , j ,FALSE)
    ELSE   j:=-j+1;      _refstr(s,v,ss,sc,ptop+asz-1, j ,FALSE)
    END
  END;
  wnd.mode(v,wnd.normal);
  wnd.refresh(v)
END _rscroll;

----------------------------- menus ----------------------------
                             -------

PROCEDURE _mblock(s: WINDOW; ss: SEL; a: INTEGER);
  VAR t: wnd.TOOL;
      f: wnd.FONT;
      c: BITSET;
     ch: CHAR;
    x,y: INTEGER;
BEGIN
  t:=s^.tool;
  y:=s^.h-msize*(a+1)-1;
  c:=black;
  f:=nfont;
  IF         a=0            THEN  t.color:=black;  c:=bright; f:=bfont
  ELSIF NOT (a IN ss^.able) THEN  t.color:=shadow
  ELSIF      a=ss^.salt     THEN  t.color:=bright
  ELSE                            t.color:=normal
  END;
  bframe(s,FALSE,1,y,s^.w-1,y+msize-1);
  wnd.rect(s,t,3,y+2,s^.w-3,y+msize-3);
  t.back:=t.color; t.color:=c;
  wnd.print(s,t,f^.W DIV 2,y+2,f,ss^.alts[0]);
  IF a>0 THEN
    t.color:=black;
    wnd.print(s,t,nfont^.W DIV 2,y+2,nfont,ss^.alts[a]);
    x:=HIGH(ss^.alts[a]);
    IF (x>0) & ((ss^.alts[a][x]="^") OR (ss^.alts[a][x]="~")) THEN
      ch:=ss^.alts[a][x-1];
      IF ch=0c  THEN RETURN END;
      IF ch>37c THEN
        x:=s^.w-wnd.width(f,"%c",ch)-f^.W DIV 2-4;
        wnd.print(s,t,x,y+2,nfont,"%c",ch)
      ELSE
        x:=s^.w-wnd.width(f,"^%c",ch)-f^.W DIV 2-4;
        wnd.print(s,t,x,y+2,nfont,"^%c",ch)
      END
    END
  END
END _mblock;

PROCEDURE shmenu(s: WINDOW; ss: SEL);
  VAR i: INTEGER;
BEGIN
  wnd.mode(s,wnd.img);
  FOR i:=0 TO ss^.lalt DO _mblock(s,ss,i) END;
  wnd.mode(s,wnd.normal);
  wnd.refresh(s)
END shmenu;

PROCEDURE _mprint(s: WINDOW; ss: SEL;  VAL fmt: ARRAY OF CHAR; SEQ arg: WORD);
  VAR f: wnd.FONT;
     ch: CHAR;
    i,j: INTEGER;
BEGIN
  done:=TRUE;
  WITH ss^ DO
    sprint(alts[mpos],fmt,arg);
    IF NOT done THEN  RETURN  END;
    IF  mpos=0  THEN f:=bfont ELSE f:=nfont END;
    wdth[mpos]:=wnd.width(f,alts[mpos]);
    IF mpos>lalt THEN lalt:=mpos END;
    j:=mpos;
    i:=HIGH(alts[mpos])-2;
    ch:=alts[mpos][i];
    IF    ch="~" THEN caps:=caps-{j}; alts[j][i]:=0c; alts[j][i+2]:=ch
    ELSIF ch="^" THEN caps:=caps+{j}; alts[j][i]:=0c; alts[j][i+2]:=ch;
      alts[j][i]:=ASCII.CAPITAL(alts[j][i])
    END;
    FOR j:=lalt TO 0 BY -1 DO
      IF (j=lalt) & (wdth[j]=0) THEN lalt:=j-1 END;
      IF i<wdth[j] THEN i:=wdth[j] END
    END;
    WITH s^ DO
      wnd.corner(s,wnd.rdc);
wnd.open(s);
pm.zoom;
      wnd.resize(s,i+f^.W*3+1,(lalt+1)*msize+1);
      coff; shmenu(s,ss); con
;pm.zoom;
    END
  END
END _mprint;

PROCEDURE _mnew(VAL s: WINDOW; VAL ss: SEL);
  VAR i,l: INTEGER;
BEGIN
  ss^.mpos:=0;
  ss^.lalt:=00;     ss^.able:={1..31};
  ss^.salt:=00;     ss^.caps:={0..31};
  FOR i:=0 TO 31 DO ss^.wdth[i]:=00; NEW(ss^.alts[i],0) END;
  _mprint(s,ss,"Menu")
END _mnew;

PROCEDURE mselect(s: WINDOW; ss: SEL);
(*
  VAR a: INTEGER;
   _mou: BOOLEAN;

  PROCEDURE _select(a: INTEGER);
  BEGIN
    crs.toggle(FALSE); _mchoose(m,a);
    crs.toggle(TRUE);  m^.out:=NOT (m^.salt IN m^.able)
  END _select;

  PROCEDURE _move(VAR a: INTEGER);
  BEGIN
    WITH m^ DO
      IF a<1 THEN a:=1 ELSIF a>lalt THEN a:=lalt END;
      mx:=block.x+block.w DIV 2;
      my:=block.y+block.h-bh*(a+1)+bh DIV 2
    END;
    crs.move(mx,my)
  END _move;

  PROCEDURE _place(alt: INTEGER);
  BEGIN
    WITH m^ DO
      a:=alt;
      mx:=block.x+block.w DIV 2;
      IF (a>0) & (a<=lalt) THEN my:=block.y+block.h-bh*(a+1)+bh DIV 2
      ELSE                      my:=block.y+block.h-bh*2+bh DIV 2
      END
    END;
    crs.move(mx,my)
  END _place;

  PROCEDURE _kselect(): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    WITH m^ DO
      char:=0c;
      out:=(ch=key.left) OR (ch=key.right) OR (ch=33c) OR ((ch=15c) & NOT in());
      IF out THEN char:=ch; RETURN TRUE END;
      IF index(hot,ch,i)                THEN _select(i); RETURN TRUE END;
      IF index(cap,ASCII.CAPITAL(ch),i) THEN _select(i); RETURN TRUE END;
      IF _mou THEN _place(salt); _mou:=FALSE END;
      CASE ch OF
        | 15c    : _select(a); RETURN TRUE
        |key.up  : DEC(a); _move(a)
        |key.dw  : INC(a); _move(a)
        |key.pgup: _place(1)
        |key.pgdw: _place(lalt)
        |key.home: _place(1)
        |key.end : _place(lalt)
      ELSE
      END
    END;
    RETURN FALSE
  END _kselect;

  VAR pop: BOOLEAN;   lbut: BOOLEAN;   ox,oy: INTEGER;
      p,s: BITSET;    rbut: BOOLEAN;       i: INTEGER;
BEGIN
  IF (m=NIL) OR (m^.magic#mmagic) THEN bad_desc; RETURN END;
  IF m^.lalt<1                    THEN undef;    RETURN END;
  done:=TRUE; coff;
  WITH m^ DO
    pop:=pup^.close;
    IF pop THEN
      popen(pup);
      IF NOT done THEN RETURN END;
      _mrefresh(m)
    END;
    _place(salt);  crs.toggle(TRUE);
    p:={};  s:={};  ch:=0c;  keys:={};  out:=FALSE;
    LOOP
      ox:=mx; oy:=my; p:=s; startwait;
      REPEAT
        _read(20,p,s,TRUE)
      UNTIL (ox#mx) OR (oy#my) OR (p#s) OR (p#s) OR (ch#0c) OR NOT awaited();
      IF  NOT  awaited() THEN out:=TRUE; EXIT END;
      lbut:=(p*{0}={}) & (s*{0}#{});
      rbut:=(p*ank={}) & (s*ank#{});
      IF rbut OR (lbut & NOT in()) THEN out:=TRUE; EXIT
      ELSIF lbut THEN
        i:=(block.h-1-(my-block.y)) DIV bh;
        _select(i);
        IF i=0 THEN s:=s-{0} END;
        EXIT
      ELSIF (ox#mx) OR (oy#my) THEN crs.move(mx,my); _mou:=TRUE
      END;
      IF (ch#0c) & _kselect() THEN EXIT END
    END;
    keys:=s;
    WHILE s*keys#{} DO _read(20,p,s,FALSE) END;
    IF pop THEN con; pclose(pup) END;
  END
*)
END mselect;


-------------- R e f r e s h  P r o c e d u r e s --------------
              ------------------------------------

PROCEDURE emptyrefr(s: WINDOW; x,y,w,h: INTEGER); BEGIN END emptyrefr;

PROCEDURE refbox(s: WINDOW; X,Y,W,H: INTEGER);
  VAR ss: SEL;
BEGIN
  wnd.object(s,"SEL",ss);
  s^.tool.clip.x:=X;  s^.tool.clip.w:=W;
  s^.tool.clip.y:=Y;  s^.tool.clip.h:=H;
  shbox(s,ss^.name)
END refbox;

PROCEDURE refpanel(s: WINDOW; X,Y,W,H: INTEGER);
  VAR ss: SEL;
BEGIN
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  WITH s^ DO
    tool.clip.x:=X;  tool.clip.w:=W;
    tool.clip.y:=Y;  tool.clip.h:=H;
    tool.color:=normal; wnd.rect (s,tool,0,0,w-1,h-1);
    tool.color:=black;  wnd.frame(s,tool,0,0,w-1,h-1)
  END
END refpanel;

PROCEDURE refradio(s: WINDOW; X,Y,W,H: INTEGER);
  VAR ss: SEL;
BEGIN
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  WITH s^ DO
    tool.clip.x:=X;  tool.clip.w:=W;
    tool.clip.y:=Y;  tool.clip.h:=H
  END;
  shradio(s,ss^.state*pressed#{},ss^.name)
END refradio;

PROCEDURE refbutton(s: WINDOW; X,Y,W,H: INTEGER);
  VAR ss: SEL;
BEGIN
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  WITH s^ DO
    tool.clip.x:=X;  tool.clip.w:=W;
    tool.clip.y:=Y;  tool.clip.h:=H
  END;
  shblock(s,ss^.state*pressed#{},normal,ss^.name)
END refbutton;

PROCEDURE refblock(s: WINDOW; X,Y,W,H: INTEGER);
  VAR ss: SEL;
BEGIN
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  WITH s^ DO
    tool.clip.x:=X;  tool.clip.w:=W;
    tool.clip.y:=Y;  tool.clip.h:=H
  END;
  IF ss^.state*pressed#{} THEN shblock(s,TRUE ,normal,"")
  ELSE                         shblock(s,FALSE,normal,"")
  END
END refblock;

PROCEDURE refitem(s: WINDOW; X,Y,W,H: INTEGER);
  VAR ss: SEL;
BEGIN
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done THEN wnd_error; RETURN END;
  WITH s^ DO
    tool.clip.x:=X;  tool.clip.w:=W;
    tool.clip.y:=Y;  tool.clip.h:=H;
    shblock(s,FALSE,normal,ss^.name);
    tool.back:={};
    shsicon(s,w-msize,2,15)
  END
END refitem;

PROCEDURE refline(s: WINDOW; X,Y,W,H: INTEGER);
BEGIN
  WITH s^ DO
    tool.clip.x:=X;  tool.clip.w:=W;
    tool.clip.y:=Y;  tool.clip.h:=H;
    IF w=2 THEN
      tool.color:=bright; wnd.line(s,tool,1,0,1,h-1);
      tool.color:=shadow; wnd.line(s,tool,0,0,0,h-1)
    ELSE
      tool.color:=bright; wnd.line(s,tool,0,0,w-1,0);
      tool.color:=shadow; wnd.line(s,tool,0,1,w-1,1)
    END
  END
END refline;

----------------------------------------------------------------

PROCEDURE _ref(kind: INTEGER): wnd.PAINT;
BEGIN
  CASE kind OF
    |_panel    : RETURN refpanel
    |_box      : RETURN refbox
    |_block    : RETURN refblock
    |_radio    : RETURN refradio
    |_button   : RETURN refbutton
    |_item     : RETURN refitem
    |_line     : RETURN refline
    |_slider   : RETURN wnd.image
    |_scrollbar: RETURN wnd.image
    |_switch   : RETURN wnd.image
    |_roller   : RETURN refblock
    |_viewer   : RETURN wnd.image
    |_diabox   : RETURN wnd.image
    |_menu     : RETURN wnd.image
  ELSE           RETURN emptyrefr
  END
END _ref;

PROCEDURE _create(VAR s: WINDOW; kind: INTEGER; x,y,w,h: INTEGER;
                   desk: WINDOW; roll: WINDOW);
  VAR ss: SEL;
BEGIN
  NEW(ss);
  IF NOT done THEN RETURN END;
  ss^.magic:=Smagic;
  ss^.selec:=FALSE;
  ss^.kind :=kind;
  IF roll#NIL THEN ss^.state:={31} ELSE ss^.state:={} END;
;IF kind IN {_menu} THEN w:=1; h:=1 END;
  IF kind IN {_menu} THEN w:=0; h:=0 END;
  wnd.create (s,wnd.desktop,x,y,w,h,sum,{},_ref(kind));
  IF NOT done THEN DISPOSE(ss); RETURN END;
  wnd.assign(s,"SEL",ss);
  IF NOT done THEN DISPOSE(ss); RETURN END;
  CASE ss^.kind OF
    |_box,_button,
     _radio,_item: ss^.name:=""
    |_menu       : _mnew(s,ss);   shmenu(s,ss)
    |_diabox     : _dnew(s,ss);   shdiabox(s,ss)
    |_roller     : _rnew(s,ss)
    |_scrollbar  : ss^.srol:=roll;  _snew(s,ss,0,0); _sshow(s,ss)
    |_viewer     : ss^.vrol:=roll;
  ELSE
  END
END _create;

PROCEDURE create(VAR s: WINDOW; kind: INTEGER; x,y,w,h: INTEGER);
BEGIN
  _create(s,kind,x,y,w,h,wnd.desktop,NIL)
END create;

PROCEDURE disposesel(VAR ss: SEL);
  VAR i: INTEGER;
BEGIN
  WITH ss^ DO
    CASE kind OF
      |_diabox: DISPOSE(dstr); DISPOSE(save)
      |_roller: i:=0;
                WHILE (i<=HIGH(text)) DO DISPOSE(text[i]); INC(i) END;
                DISPOSE(text)
      |_menu  : FOR i:=0 TO 31 DO DISPOSE(ss^.alts[i]) END
    ELSE
    END;
    magic:=0
  END;
  DISPOSE(ss)
END disposesel;

PROCEDURE dispose(VAR s: WINDOW);
  VAR S0: SEL;
      nm: str32;

  PROCEDURE _dispose(s: WINDOW);
    VAR s0: WINDOW;
  BEGIN
    wnd.object(s,"SEL",S0);
    IF NOT wnd.done THEN wnd_error; RETURN END;
    disposesel(S0);
    wnd.iterobjects(s);
    WHILE wnd.nextobject(s,nm,s0) DO
      IF checkname(nm) THEN
        _dispose(s0);
        IF NOT done THEN RETURN END
      END
    END
  END _dispose;

BEGIN
  IF s=NIL THEN RETURN END;
  wnd.object(s,"SEL",S0);
  IF NOT wnd.done OR (S0^.magic#Smagic) THEN bad_desc;  RETURN END;
  _dispose(s);
  wnd.dispose(s);
  IF NOT wnd.done THEN wnd_error END
END dispose;

PROCEDURE open(s: WINDOW);
  VAR S0: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",S0);
  IF NOT wnd.done OR (S0^.magic#Smagic) THEN bad_desc; RETURN END;
  wnd.open(s)
END open;

PROCEDURE close(s: WINDOW);
  VAR S0: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",S0);
  IF NOT wnd.done OR (S0^.magic#Smagic) THEN bad_desc; RETURN END;
  wnd.close(s)
END close;

PROCEDURE copy(into: WINDOW; name: ARRAY OF CHAR; sub: WINDOW);
  VAR S0: SEL;
      nm: str32;

  PROCEDURE _copy(VAR d: WINDOW; on,s: WINDOW);

    PROCEDURE copywindow;
    BEGIN
      d :=NIL;
      S0:=NIL;
      wnd.object(s,"SEL",S0);
      IF NOT wnd.done THEN wnd_error; RETURN END;
      WITH s^ DO
        wnd.create(d,on,x,y,w,h,fore,back,_ref(S0^.kind));
        IF NOT wnd.done THEN wnd_error; RETURN END;
      END;
      IF s^.image THEN
        wnd.bblt(d,d^.tool,0,0,s,s^.tool,s^.tool.clip);
        IF NOT wnd.done THEN wnd_error; wnd.dispose(d); RETURN END
      END;
      wnd.assign(on,nm,d);
      IF NOT wnd.done THEN wnd_error; wnd.dispose(d) END
    END copywindow;

    PROCEDURE copyselector;
      VAR i: INTEGER;
         s1: SEL;
    BEGIN
      NEW(s1);
      ASSERT(done);
      IF NOT done THEN RETURN END;
      s1^:=S0^;
      WITH s1^ DO
        CASE kind OF
          |_scrollbar: IF state*{31}#{} THEN srol:=on ELSE srol:=NIL END
          |_viewer   : IF state*{31}#{} THEN vrol:=on ELSE vrol:=NIL END
          |_diabox   : NEW(s1^.save,HIGH(S0^.save)+1);
                       IF NOT done THEN disposesel(s1); RETURN END;
                       NEW(s1^.dstr,HIGH(S0^.dstr)+1);
                       IF NOT done THEN disposesel(s1); RETURN END;
                       strcopy(s1^.save,S0^.save);
                       strcopy(s1^.dstr,S0^.dstr)
          |_roller   : NEW(s1^.text,HIGH(S0^.text)+1);
                       IF NOT done THEN disposesel(s1); RETURN END;
                       IF HIGH(s1^.text)>=0 THEN
                         i:=0;
                         WHILE i<=HIGH(S0^.text) DO
                           sprint(s1^.text[i],S0^.text[i]); INC(i);
                           IF NOT done THEN disposesel(s1); RETURN END
                         END
                       END
          |_menu    :  FOR i:=0 TO S0^.lalt DO
                         NEW(s1^.alts[i],HIGH(S0^.alts[i])+1);
                         IF NOT done THEN disposesel(s1); RETURN END;
                         strcopy(s1^.alts[i],S0^.alts[i]);
                       END
        ELSE
        END
      END;
      wnd.assign(d,"SEL",s1);
      IF NOT wnd.done THEN wnd_error; disposesel(s1) END
    END copyselector;

    VAR s0,s1: WINDOW;
  BEGIN
    copywindow;   IF NOT done THEN RETURN END;
    copyselector; IF NOT done THEN wnd.dispose(d); RETURN END;
    wnd.iterobjects(s);
    WHILE wnd.nextobject(s,nm,s0) DO
      IF checkname(nm) THEN
        _copy(s1,d,s0);
        IF NOT done THEN RETURN END
      END
    END;
    wnd.open(d)
  END _copy;

  VAR cop: WINDOW;
BEGIN
  IF sub=NIL THEN bad_parm; RETURN END;
  wnd.object(sub,"SEL",S0);
  IF NOT wnd.done     THEN wnd_error; RETURN END;
  IF S0^.magic#Smagic THEN bad_desc;  RETURN END;
  str.print(nm,"SEL.%s",name);
  _copy(cop,into,sub);
  IF NOT done THEN dispose(cop) END
END copy;

PROCEDURE kind(s: WINDOW): INTEGER;
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN -1 END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN -1 END;
  RETURN ss^.kind
END kind;

PROCEDURE subsel(s: WINDOW; name: ARRAY OF CHAR; VAR sub: WINDOW);
  VAR ss: SEL;
      nm: str32;
BEGIN
  sub:=NIL;
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  str.print(nm,"SEL.%s",name);
  wnd.object(s,nm,sub);
  IF NOT wnd.done THEN wnd_error END
END subsel;

PROCEDURE read(VAR s: WINDOW; selname,filename: ARRAY OF CHAR);
BEGIN END read;

PROCEDURE save(s: WINDOW; selname,filename: ARRAY OF CHAR);
BEGIN END save;

PROCEDURE delete(selname,filename: ARRAY OF CHAR);
BEGIN END delete;

PROCEDURE title(s: WINDOW; f: ARRAY OF CHAR; SEQ a: WORD);
  VAR ss: SEL;  i: INTEGER;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_menu       : i:=ss^.mpos; ss^.mpos:=0; _mprint(s,ss,f,a);
                   ss^.mpos:=i;              shmenu(s,ss)
    |_diabox     : str.print(ss^.dname,f,a); shdiabox(s,ss)
    |_box,_button,
     _item,_radio: str.print(ss^.name,f,a)
  ELSE
  END;
  coff; wnd.refresh(s); con
END title;

PROCEDURE moveandresize(s: WINDOW; x,y,w,h: INTEGER);
BEGIN END moveandresize;

PROCEDURE print(s: WINDOW; f: ARRAY OF CHAR; SEQ a: WORD);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_menu  : _mprint(s,ss,f,a)
    |_roller: _rprint(s,ss,f,a)
  ELSE
  END
END print;

PROCEDURE printxy(sel: WINDOW; x,y: INTEGER; f: ARRAY OF CHAR; SEQ a: WORD);
BEGIN END printxy;

PROCEDURE setstate(s: WINDOW; state: BITSET);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  ss^.state:=state-{31};
  CASE ss^.kind OF
    |_slider,_scrollbar: _snew(s,ss,ss^.asz,ss^.osz); _smake(ss); _sshow(s,ss)
  ELSE
  END
END setstate;

PROCEDURE getstate(s: WINDOW; VAR state: BITSET);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  state:=ss^.state
END getstate;

PROCEDURE viewer(s: WINDOW; x,y,w,h: INTEGER);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_diabox: ss^.dmain.x:=x;  ss^.dmain.w:=w;  shdiabox(s,ss)
  ELSE
  END
END viewer;

PROCEDURE ratio(s: WINDOW; asize,osize: INTEGER);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  IF _scrollbar=ss^.kind THEN _snew(s,ss,asize,osize)
  ELSIF _slider=ss^.kind THEN _snew(s,ss,  0  ,osize)
  ELSE RETURN
  END;
  _smake(ss);
  _sshow(s,ss)
END ratio;

PROCEDURE setpos(s: WINDOW; opos: INTEGER);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_menu     : IF (opos<0) OR (opos>30) THEN bad_parm; RETURN END;
                 ss^.mpos:=opos+1
    |_roller   : ss^.rpos:=opos
    |_scrollbar,
     _slider   : coff; _spos(s,ss,opos); con
  ELSE
  END
END setpos;

PROCEDURE getpos(s: WINDOW; VAR objectpos: INTEGER);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_scrollbar,
     _slider   : objectpos:=ss^.ctop
    |_roller   : objectpos:=ss^.rsel
    |_menu     : objectpos:=ss^.salt
  ELSE
  END
END getpos;

PROCEDURE suppress(s: WINDOW; pos: INTEGER; off: BOOLEAN);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  IF ss^.kind=_menu THEN
    IF off THEN ss^.able:=ss^.able-{ss^.mpos}
    ELSE        ss^.able:=ss^.able+{ss^.mpos}
    END
  END
END suppress;

PROCEDURE select(VAL s: WINDOW);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  IF ss^.state*disabled#{} THEN
  END;
  CASE ss^.kind OF
    |_scrollbar: sselect(s,ss)
    |_slider   : sselect(s,ss)
    |_radio    : aselect(s,ss)
    |_button   : bselect(s,ss)
    |_diabox   : dselect(s,ss)
(*
    |_roller   : rselect(s,ss)
    |_switch   : sselect(s,ss)
    |_viewer   : vselect(s,ss)
*)
  ELSE
  END
END select;

PROCEDURE selected(s: WINDOW; VAR sub: WINDOW): BOOLEAN;
BEGIN

END selected;

PROCEDURE putstr(s: WINDOW; sstr: ARRAY OF CHAR);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_diabox: RESIZE(ss^.save,HIGH(sstr)+1);  IF NOT done THEN RETURN END;
              RESIZE(ss^.dstr,HIGH(sstr)+1);  IF NOT done THEN RETURN END;
              strcopy(ss^.save,sstr);         dputstr(s,ss)
  ELSE
  END
END putstr;

PROCEDURE getstr(s: WINDOW; VAR sstr: ARRAY OF CHAR);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  CASE ss^.kind OF
    |_diabox: strcopy(sstr,ss^.save)
  ELSE
    sstr[0]:=0c
  END
END getstr;

PROCEDURE scroll(s: WINDOW; VAR old,new: INTEGER);
  VAR ss: SEL;
BEGIN
  IF s=NIL THEN bad_parm; RETURN END;
  wnd.object(s,"SEL",ss);
  IF NOT wnd.done OR (ss^.magic#Smagic) THEN bad_desc; RETURN END;
  IF (ss^.kind=_slider) OR (ss^.kind=_scrollbar) THEN
    old:=ss^.ptop; new:=ss^.ctop
  ELSE
    old:=-1;       new:=-1
  END
END scroll;

PROCEDURE max(a: INTEGER; SEQ c: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(c) DO
    IF a<c[i] THEN a:=c[i] END
  END;
  RETURN a
END max;

PROCEDURE setcolors;
  VAR i: INTEGER;
BEGIN
  i:=scr.state^.RGB DIV 4;
  IF i<=0 THEN bad_parm; RETURN END;
  WITH scr.state^.pal[INTEGER(black )] DO r:=i*0; g:=i*0; b:=i*0 END;
  WITH scr.state^.pal[INTEGER(shadow)] DO r:=i*1; g:=i*1; b:=i*1 END;
  WITH scr.state^.pal[INTEGER(normal)] DO r:=i*2; g:=i*2; b:=i*2 END;
  WITH scr.state^.pal[INTEGER(bright)] DO r:=i*3; g:=i*3; b:=i*3 END;
  scr.set_palette(scr.state^.pal,0,HIGH(scr.state^.pal)+1);
  done:=scr.done;
  IF NOT done THEN error:=scr.error END
END setcolors;

BEGIN
  null:=NIL;;
  done:=TRUE;
  error:=err.ok;
  Smagic:=4C455340h;
  crssave:=crs.on;
(*
  bfont:=fnt.font;
  nfont:=fnt.font;
  ifont:=fnt.font;
*)
  fnt.read(nfont,'PNn.fnt');  IF NOT fnt.done THEN nfont:=fnt.font END;
  fnt.unpack(nfont);
  fnt.read(bfont,'PNb.fnt');  IF NOT fnt.done THEN bfont:=nfont END;
  fnt.unpack(nfont);
  fnt.read(ifont,'PN.icn');   IF NOT fnt.done THEN ifont:=nfont END;
  ASSERT(fnt.done,fnt.error);
  fnt.unpack(nfont);
  msize:=max(ifont^.H,ifont^.W,nfont^.H,nfont^.W,bfont^.H,bfont^.W)+4;
  black :={   };    normal:={ 1 };
  shadow:={ 0 };    bright:={0,1};
  sum:=bright+normal+shadow+black;
  timeout:=0;
  cpd.nop;
  IF cpd.state^.nokeys>1 THEN ank:={cpd.state^.nokeys-1} END;
  setcolors
END pmSel.
