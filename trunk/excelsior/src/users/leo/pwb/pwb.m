MODULE pwb; (*$N- Leo 05-Jun-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT       ASCII;
IMPORT  err: defErrors;
IMPORT  wld: pwbWorld;
IMPORT  cpd: CPD;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  key: Keyboard;
IMPORT  mem: Heap;
IMPORT  tim: Time;

IMPORT  wm : pmWM;
IMPORT  wnd: pmWnd;
IMPORT  pup: pmPUP;
IMPORT  crs: pmCrs;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

VAR bug: CHAR;

CONST VERSION = "PWB v0.091";
CONST DEBUG = TRUE;
CONST FOE   = FALSE;

CONST
  languages= 2;
  English  = 0;
  Russian  = 1;


TYPE
  STRING16  = ARRAY [0.. 15] OF CHAR;
  STRING32  = ARRAY [0.. 31] OF CHAR;
  STRING64  = ARRAY [0.. 63] OF CHAR;
  STRING128 = ARRAY [0..127] OF CHAR;

VAR
 language  : INTEGER;

  YES      : STRING16;
  NO       : STRING16;

  EDIT     : STRING16;
    oPEN   : STRING16;
    qUIT   : STRING16;
    nEW    : STRING16;
    LAN    : STRING16;

  SETUP    : STRING16;
   lSCROLL : STRING16;
   rSCROLL : STRING16;
   uSCROLL : STRING16;
   dSCROLL : STRING16;
     CRS   : STRING16;
     LOC   : STRING16;
   PANO    : STRING16;
     FRAME : STRING16;
     LINE  : STRING16;


  FILES    : STRING16;
    OPEN   : STRING16;
    SAVE   : STRING16;
    SAVEAS : STRING32;
    RENAME : STRING32;
    mNEW   : STRING16;
    CANCEL : STRING32;
    QUIT   : STRING16;
  NEWONE   : STRING16;
  UNTIT    : STRING32;

  ILGSETUP : STRING64;
  CLOSE    : STRING16;
  SHARED   : STRING64;
  ENTNAME  : STRING64;
  NONAME   : STRING32;
  TOOPEN   : STRING64;
  NOTHING  : STRING64;
  MBSAVED  : STRING64;
    SAVED  : STRING16;
    NOSAVE : STRING16;
    ESC    : STRING16;
  CHLOST   : STRING64;
  QTLOST   : STRING64;
  NOCHNG   : STRING64;
  EXIST    : STRING64;
  CORNER0  : STRING64;
  CORNER1  : STRING64;


PROCEDURE texts;
   PROCEDURE set(VAR d: ARRAY OF CHAR; e,r: ARRAY OF CHAR);
   BEGIN
     IF language=English THEN str.copy(d,e) ELSE str.copy(d,r) END
   END set;
BEGIN
  set(oPEN,"open","откр");
  set(qUIT,"exit","стоп");
  set(nEW ,"new" ,"нов");
  set(FILES,"Files","Файлы F");
    set(OPEN  ,"Open"   ,"Открыть [O]");
    set(SAVE  ,"Save"   ,"Сохранить [S]");
    set(SAVEAS,"Save As","Сохранить как  [A]");

    set(RENAME,"Rename" ,"Переименовать [R]");
    set(mNEW  ,"New"    ,"Новый [N]");
    set(CANCEL,"Clear"  ,"Почистить [C]");
    set(QUIT  ,"Quit"   ,"Выход [Q]");
  set(EDIT ,"Edit" ,"Ред E");
  set(SETUP,"Setup","Уст S");

  set(NEWONE,"N e w"    ,"Н о в ы й");
  set(UNTIT ,"U n t i t l e d","Н е  н а з в а н");

  set(YES    ,"yYes","yДа");
  set(NO     ,"nNo","nНет");
  set(SAVED  ,""15c"Save",""15c"Сохранить");
  set(NOSAVE ,"nDon`t Save","nНе Сохранять");
  set(ESC    ,""33c"Escape",""33c"Отменить");

  set(CLOSE  ,"Close","Закрыть");


  set(SHARED ,'%s [%s] shared with other window'
             ,'%s [%s] распределено между окнами');
  set(ENTNAME,'Enter new name: ','Введите новое имя: ');

  set(ILGSETUP,'"pwb.setup" illegal version or structure'
              ,'"pwb.setup" неправильная версия или структура файла');

  set(TOOPEN,'Press "open" or "F9" when You need to open closed windows',
             'Нажмите "откр" или F9 для открытия закрытых окон');
  set(NOTHING,"You have nothing here"
             ,"Здесь ничего нет");
  set(MBSAVED,"Current model was changed. Save it?."
             ,"Нужно сохранить?");
  set(CHLOST ,'All changes in "%s" will be lost! Are your sure?'
             ,'Все изменения в "%s" будут потеряны! Вы уверены?');
  set(QTLOST ,'All changes will be lost! Are your sure?'
             ,'Все изменения будут потеряны! Вы уверены?');
  set(NOCHNG ,'No changes! Write "%s"?'
             ,'Нет изменений! Все равно записать "%s"?');
  set(EXIST  ,'File "%s" already exist. Overwrite?'
             ,'Файл "%s" уже существует. Все равно записать?');

  set(NONAME,"U N N A M E D","А н о н и м");
  set(CORNER0,"Select a corner.","Отметьте угол.");
  set(CORNER1,"Select another corner.","Отметьте другой угол.");

  set(LAN,"рус","eng");
  set(lSCROLL,"scroll L=%02d","ролик Л=%02d");
  set(rSCROLL,"scroll R=%02d","ролик П=%02d");
  set(uSCROLL,"scroll U=%02d","ролик В=%02d");
  set(dSCROLL,"scroll D=%02d","ролик Н=%02d");
  set(PANO   ,"pan %s","пан %s");
  set(FRAME  ,"FRAME","РАМКА");
  set(LINE   ,"LINE" ,"ЛИНИЯ");
  set(CRS    ,"CURSOR" ,"КУРСОР");
  set(LOC    ,"LOCATOR","ЛОКАТОР");
END texts;

VAR done: BOOLEAN;
   error: INTEGER;
   MAGIC: INTEGER;
  wcount: INTEGER;

TYPE
  PROJECT   = POINTER TO Project;
  Project   =
    RECORD

      magic : INTEGER;
      next  : PROJECT;
      x,y   : INTEGER;
      w,h   : INTEGER;
      icon  : BOOLEAN;
      fire  : BOOLEAN;
      grid  : BOOLEAN;
      panfrm: BOOLEAN;
      lctr  : BOOLEAN;

      main  : wld.MODEL;
      int   : BOOLEAN;

      ldcx  : INTEGER;
      ldcy  : INTEGER;
      scale : INTEGER;

      Smenu : pup.MENU;
      Fmenu : pup.MENU;
      Fdrx  : pup.DIREX;
      fname : STRING128;

      wname : wnd.WINDOW;
      xnml  : INTEGER;
      xnmr  : INTEGER;
      wcoor : wnd.WINDOW;
      winfo : wnd.WINDOW;

      over  : wnd.WINDOW;

      Lscrl : INTEGER;   (* cpd scroll control *)
      Rscrl : INTEGER;
      Uscrl : INTEGER;
      Dscrl : INTEGER;
      Xadd  : INTEGER;
      Yadd  : INTEGER;


      ex,ey : INTEGER;  (* edit X,Y in model coordinates          *)
      gstep : INTEGER;  (* grid step (see: grid and setscale)     *)
      step  : INTEGER;  (* number of model pixels per 1 grid step *)
                        (* 960/10, 960/20, 960/40, 960/60         *)
    END;

VAR projs: PROJECT;
     cntX: INTEGER;
     cntY: INTEGER;
     minW: INTEGER;
     minH: INTEGER;
      act: wnd.WINDOW;
     main: wnd.WINDOW;
     bfnt: wnd.FONT;

      red: BITSET;      firstclose: BOOLEAN;

CONST
  btnfiles   = 0;       btnactive  = 1;
  btnedit    = 2;       btntools   = 3;
  btnsetup   = 4;       btnzoomin  = 5;
  btnzoomout = 6;       btnpan     = 7;
  btnfire    = 8;       btneye     = 9;
  btnzoom    =10;       btnright   =11;
  btnleft    =12;       btnup      =13;
  btndw      =14;       btncascade =15;
  btngrid    =16;       btnicon    =17;
  btnnew     =19;       btnopen    =20;
  btnquit    =21;       btnlang    =22;

PROCEDURE isprj(wn: wnd.WINDOW): BOOLEAN;
  VAR p: PROJECT;
BEGIN
  IF (wn=NIL) OR (wn^.obj=NIL) THEN RETURN FALSE END;
  p:=wn^.obj;
  RETURN p^.magic=MAGIC
END isprj;

PROCEDURE coff;
BEGIN crs.toggle(FALSE) END coff;

PROCEDURE con;
BEGIN crs.toggle(TRUE) END con;

PROCEDURE putover(wn: wnd.WINDOW); FORWARD;

PROCEDURE activate(wn: wnd.WINDOW);
BEGIN
  IF wn=act  THEN RETURN END;
  IF act#NIL THEN wm.buttoncolors(act,btnactive,wm.black,wm.shadow,wm.normal) END;
  act:=wn;
  IF act#NIL THEN wm.buttoncolors(act,btnactive,red,wm.shadow,wm.normal) END;
  coff;
  wnd.putunder(act,main);
  putover(wn);
END activate;

PROCEDURE activatetop;
  VAR wn: wnd.WINDOW;
BEGIN
  wn:=wnd.top;
  WHILE wn#NIL DO
    IF isprj(wn) & NOT wn^.closed THEN activate(wn); RETURN END;
    wn:=wnd.dw(wn)
  END
END activatetop;

PROCEDURE new(VAR p: PROJECT);
BEGIN
  NEW(p);
  IF NOT mem.done THEN
    done:=FALSE; error:=mem.error; pup.perror(error,cntX,cntY,"%%s"); RETURN
  END;
  done:=TRUE;
  p^.fname:="";                 p^.Lscrl:=8;    p^.Xadd:=16;
  p^.scale:=0;                  p^.Rscrl:=8;
  p^.ldcx :=0;                  p^.Uscrl:=4;    p^.Yadd:=8;
  p^.ldcy :=0;                  p^.Dscrl:=4;
  p^.main :=wld.mnull;          p^.int  :=TRUE;

  p^.Smenu:=pup.mnull;          p^.x:=0;        p^.icon:=FALSE;
  p^.Fmenu:=pup.mnull;          p^.y:=0;        p^.fire:=FALSE;
  p^.Fdrx :=pup.dnull;          p^.w:=0;        p^.grid:=FALSE;
  p^.next :=projs;              p^.h:=0;

  p^.step :=960/40;             p^.ex:=0;
  p^.gstep:=-1;                 p^.ey:=0;
  p^.magic:=MAGIC;
  projs:=p;

  p^.panfrm:=FALSE;
  p^.lctr  :=FALSE;
END new;

PROCEDURE wld_error(VAL s: ARRAY OF CHAR);
BEGIN
  done:=FALSE; error:=wld.error; pup.perror(error,cntX,cntY,"%s: %%s",s)
END wld_error;

PROCEDURE rem_mdl(VAR m: wld.MODEL; int: BOOLEAN);
BEGIN
  IF wld.opencount(m)>0 THEN
    wld.close(m,int);
    IF NOT wld.done THEN wld_error(CLOSE) END
  END;
  IF wld.opencount(m)=0 THEN wld.rem_mdl(m) END;
  m:=wld.mnull
END rem_mdl;

PROCEDURE dispose(VAR p: PROJECT);
  VAR l,n: PROJECT;
BEGIN
  IF p=NIL THEN RETURN END;
  ASSERT(p^.magic=MAGIC);
  IF p^.main#wld.mnull THEN rem_mdl(p^.main,p^.int) END;
  pup.ddispose(p^.Fdrx);
  pup.mdispose(p^.Fmenu);
  pup.mdispose(p^.Smenu);
  l:=projs; n:=NIL;
  WHILE l#p DO n:=l; l:=l^.next END;
  IF n=NIL THEN projs:=p^.next ELSE n^.next:=p^.next END;
  p^.magic:=0;
  DISPOSE(p)
END dispose;





PROCEDURE releaselastpressed;
BEGIN
  IF (wm.active=NIL) OR (wm.abutton<0) OR NOT wm.pressed(wm.active,wm.abutton)
  THEN
    RETURN
  END;
  IF crs.on THEN coff END;
  wm.toggle(wm.active,wm.abutton,FALSE)
END releaselastpressed;

PROCEDURE releaseallcpdbuts;
BEGIN
  WHILE cpd.state^.keys#{} DO crs.monitor END
END releaseallcpdbuts;

PROCEDURE sure(VAL fmt: ARRAY OF CHAR; SEQ answers: STRING16): INTEGER;
  VAR wn: wnd.WINDOW;
      ch: CHAR;           i,j,l: INTEGER;
    tool: wnd.TOOL;     x,y,w,h: INTEGER;
BEGIN
  coff;
  wm.new(wn);
  wm.disable(wn);
  l:=wnd.lenght(bfnt,fmt);
  j:=0;
  FOR i:=0 TO HIGH(answers) DO j:=j+wnd.lenght(bfnt,answers[i])+8+bfnt^.W END;
  IF l<j THEN w:=j ELSE w:=l END;
  INC(w,bfnt^.W*6);
  h:=bfnt^.H*5;
  wnd.resize(wn,w,h);

  wnd.inner(wn,(wn^.w-l) DIV 2,bfnt^.H*3,l+8,bfnt^.H);
  tool:=wn^.inner;
  tool.color:=wm.bright;  wnd.rect(wn,tool,0,0,tool.clip.w-1,tool.clip.h-1);
  tool.color:=wm.black;
  tool.back :=wm.bright;  wnd.print(wn,tool,4,0,bfnt,fmt);

  wnd.move(wn,(wnd.scrW-w) DIV 2,(wnd.scrH-h) DIV 2);
  x:=bfnt^.W*3;
  y:=bfnt^.H;
  FOR i:=0 TO HIGH(answers) DO
    w:=wnd.lenght(bfnt,answers[i])+8;
    wm.button(wn,i,wm.ldc,x,y,w,bfnt^.H+2);
    wm.print(wn,i,bfnt,"%..1s",answers[i]);
    x:=x+w+8
  END;

  wnd.open(wn);
  i:=-1;
  LOOP
    wnd.ontop(wn);
    wm.monitor;
    IF cpd.state^.keys*{2}#{} THEN EXIT END;
    IF key.ready()>0 THEN
      key.read(ch);
      IF ch=33c THEN EXIT END;
      FOR j:=0 TO HIGH(answers) DO
        IF CAP(ch)=CAP(answers[j][0]) THEN i:=j; EXIT END
      END
    END;
    IF (wm.active=wn) & (wm.abutton>=0) THEN i:=wm.abutton; EXIT END;
    releaselastpressed
  END;
  WHILE cpd.state^.keys*{2}#{} DO
    wm.monitor; releaselastpressed
  END;
  coff;
  wm.dispose(wn);
  RETURN i
END sure;

PROCEDURE query(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD): BOOLEAN;
  VAR s: STRING128;
BEGIN
  str.print(s,fmt,args); RETURN sure(s,YES,NO)=0
END query;

PROCEDURE perror(e: INTEGER);
BEGIN
  done:=FALSE; error:=e;
  pup.perror(e,cntX,cntY,"%%s")
END perror;

PROCEDURE mkm(x: INTEGER): INTEGER;
BEGIN
  RETURN 2500*x DIV 96 (* 25000*x DIV 960 *)
END mkm;

PROCEDURE coord(wn: wnd.WINDOW);
  VAR p: PROJECT;
    x,y: INTEGER;
    w,i: INTEGER;
    fmt: STRING32;
   tool: wnd.TOOL;
  xl,yl: INTEGER;
  xh,yh: INTEGER;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  tool:=p^.wcoor^.inner;
  tool.mode:=wnd.rep;
  tool.color:=wm.normal;
  tool.back :=wm.normal;
  wnd.mode(p^.wcoor,wnd.img);
  wnd.rect(p^.wcoor,tool,0,0,tool.clip.w-1,tool.clip.h-1);
  IF p^.main=wld.mnull THEN wnd.refresh(p^.wcoor); RETURN END;
  fmt:="%d.%03d %d.%03d [%d %d]";
  tool.color:=wm.black;
  x:=mkm(p^.ex);
  xh:=x DIV 1000;  xl:=ABS(x) MOD 1000;  x:=p^.ex DIV p^.step;
  y:=mkm(p^.ey);
  yh:=y DIV 1000;  yl:=ABS(y) MOD 1000;  y:=p^.ey DIV p^.step;
  w:=wnd.lenght(bfnt,fmt,xh,xl,yh,yl,x,y);
  IF tool.clip.w<w THEN
    fmt:="%d.%03d %d.%03d";
    w:=wnd.lenght(bfnt,fmt,xh,xl,yh,yl,x,y)
  END;
  IF tool.clip.w>w THEN i:=(tool.clip.w-w) DIV 2 ELSE i:=0 END;
  wnd.print  (p^.wcoor,tool,i,0,bfnt,fmt,xh,xl,yh,yl,x,y);
  wnd.refresh(p^.wcoor)
END coord;

PROCEDURE info(wn: wnd.WINDOW; VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR p: PROJECT;
   tool: wnd.TOOL;
    w,x: INTEGER;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  tool:=p^.winfo^.inner;
  tool.mode:=wnd.rep;
  tool.color:=wm.normal;
  tool.back :=wm.normal;
  wnd.mode(p^.winfo,wnd.scr+wnd.img);
  wnd.rect(p^.winfo,tool,0,0,tool.clip.w-1,tool.clip.h-1);
  IF fmt="" THEN RETURN END;
  tool.color:=red;
  w:=wnd.lenght(bfnt,fmt,args);
  IF tool.clip.w>w THEN x:=(tool.clip.w-w) DIV 2 ELSE x:=0 END;
  wnd.print(p^.winfo,tool,x,0,bfnt,fmt,args)
END info;

----------------------------------------------------------------


PROCEDURE makemenus(wn: wnd.WINDOW);

  VAR p: PROJECT;
      i: INTEGER;
    x,y: INTEGER;
    w,h: INTEGER;
    drx: pup.DIREX;
   menu: pup.MENU;
   name: STRING32;
   bump: STRING32;
  nx,ny: INTEGER;

BEGIN

  ----------------------------- FILES ----------------------------
                               -------
  done:=TRUE;
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  w:=wnd.lenght(bfnt,SAVEAS)+bfnt^.W*2;
  h:=(bfnt^.H+1)*8+5;
  x:=wn^.x+wm.ssfont^.W+4;        nx:=x+wnd.lenght(bfnt,FILES);
  y:=wn^.y+wn^.h-1-bfnt^.H;       ny:=y;
  pup.mnew(menu,x,y-h,w,h,pup.xlf+pup.xrg+pup.xup+pup.xdw,FILES);
  IF NOT pup.done THEN coff; perror(pup.error); RETURN END;

  pup.mprint(menu,0,OPEN);     pup.mhotkey(menu,0,OPEN  [0],TRUE);
  pup.mprint(menu,1,SAVE);     pup.mhotkey(menu,1,SAVE  [0],TRUE);
  pup.mprint(menu,2,SAVEAS);   pup.mhotkey(menu,2,"A"      ,TRUE);
  pup.mprint(menu,3,RENAME);   pup.mhotkey(menu,3,RENAME[0],TRUE);
  pup.mprint(menu,4,mNEW);     pup.mhotkey(menu,4,mNEW  [0],TRUE);
  pup.mprint(menu,5,CANCEL);   pup.mhotkey(menu,5,CANCEL[0],TRUE);
  pup.mprint(menu,6,QUIT);     pup.mhotkey(menu,6,QUIT  [0],TRUE);

  w:=w*2;
  IF w>wnd.scrW DIV 4 THEN w:=wnd.scrW DIV 4 END;
  pup.dnew(drx,x+w-8,y-h*2+8,w,h*2,{},".","*.pwb",pup.standard);
  IF NOT pup.done THEN
    coff;  perror(pup.error);  pup.mdispose(menu);  RETURN
  END;

  IF p^.Fmenu#pup.mnull THEN pup.mchoose(menu,pup.malt(p^.Fmenu)) END;
  IF p^.Fdrx #pup.dnull THEN
    pup.dfilename(p^.Fdrx,name);
    IF pup.done THEN pup.dchoose(drx,name) END
  END;

  pup.mdispose(p^.Fmenu);  p^.Fmenu:=menu;
  pup.ddispose(p^.Fdrx);   p^.Fdrx :=drx;

  ----------------------------- SETUP ----------------------------
                               -------

  w:=wnd.lenght(bfnt,lSCROLL,0)+bfnt^.W*2;
  x:=nx;        nx:=x+wnd.lenght(bfnt,SETUP);
  y:=ny;
  h:=(bfnt^.H+1)*8+5;
  pup.mnew(menu,x,y-h,w,h,pup.xlf+pup.xrg+pup.xup+pup.xdw,SETUP);
  IF NOT pup.done THEN coff; perror(pup.error); RETURN END;

  pup.mprint(menu,0,lSCROLL,p^.Lscrl);      pup.mhotkey(menu,0,"L",TRUE);
  pup.mprint(menu,1,rSCROLL,p^.Rscrl);      pup.mhotkey(menu,1,"R",TRUE);
  pup.mprint(menu,2,uSCROLL,p^.Uscrl);      pup.mhotkey(menu,2,"U",TRUE);
  pup.mprint(menu,3,dSCROLL,p^.Dscrl);      pup.mhotkey(menu,3,"D",TRUE);

  IF p^.panfrm THEN str.copy(bump,FRAME) ELSE str.copy(bump,LINE) END;
  pup.mprint(menu,4,PANO,bump);             pup.mhotkey(menu,4,"P",TRUE);
  IF p^.lctr   THEN str.copy(bump,LOC)   ELSE str.copy(bump,CRS)  END;
  pup.mprint(menu,5,"%s",bump);             pup.mhotkey(menu,4,"C",TRUE);
  pup.mprint(menu,6,SAVE);                  pup.mhotkey(menu,6,"S",TRUE);

  IF p^.Smenu#pup.mnull THEN pup.mchoose(menu,pup.malt(p^.Smenu)) END;
  pup.mdispose(p^.Smenu);  p^.Smenu:=menu;

END makemenus;

----------------------------------------------------------------

PROCEDURE gprint0(wn: wnd.WINDOW; VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR p: PROJECT;
BEGIN
  IF DEBUG THEN
    ASSERT(isprj(wn));
    p:=wn^.obj;
    wnd.mode(p^.winfo,wnd.scr+wnd.img);
    wnd.print(p^.winfo,p^.winfo^.inner,0,0,bfnt,"%80s",'  ');
    wnd.print(p^.winfo,p^.winfo^.inner,0,0,bfnt,fmt,args);
    wnd.open(p^.winfo)
  END
END gprint0;

PROCEDURE gprint1(wn: wnd.WINDOW; VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR p: PROJECT;
BEGIN
  IF DEBUG THEN
    ASSERT(isprj(wn));
    p:=wn^.obj;
    wnd.mode(p^.wcoor,wnd.scr+wnd.img);
    wnd.print(p^.wcoor,p^.wcoor^.inner,0,0,bfnt,'%80s','  ');
    wnd.print(p^.wcoor,p^.wcoor^.inner,0,0,bfnt,fmt,args);
    wnd.open(p^.wcoor)
  END
END gprint1;


----------------------------------------------------------------

PROCEDURE setscale(wn: wnd.WINDOW; s: INTEGER);
  VAR p: PROJECT;  i: INTEGER;
BEGIN
  ASSERT(s>=1);
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.scale=s THEN RETURN END;
  p^.gstep:=p^.step;
  LOOP
    i:=p^.gstep DIV s;
    IF i>4 THEN EXIT END;
    p^.gstep:=p^.gstep*2;
  END;
  IF s=1 THEN
    wm.buttoncolors(wn,btnzoomout,wm.normal,wm.shadow,wm.normal)
  END;
  IF p^.scale=1 THEN
    wm.buttoncolors(wn,btnzoomout,wm.black,wm.shadow,wm.normal)
  END;
  p^.scale:=s
END setscale;

----------------------------------------------------------------

PROCEDURE line(wn: wnd.WINDOW; VAL tool: wnd.TOOL; x0,y0,x1,y1,w: INTEGER);

  VAR h,v,l: INTEGER;
      dx,dy: INTEGER;
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
  IF w<=1 THEN
    wnd.line(wn,tool,x0,y0,x1,y1);
    IF w=1 THEN
      IF    x0=x1 THEN wnd.line(wn,tool,x0+1,y0,x1+1,y1)
      ELSIF y0=y1 THEN wnd.line(wn,tool,x0,y0+1,x1,y1+1)
      ELSE             wnd.line(wn,tool,x0+1,y0,x1+1,y1)
      END
    END;
    RETURN
  END;
  h:=x1-x0;   v:=y1-y0;
  IF    h=0           THEN
    IF v=0 THEN RETURN END;
    dy:=0;
    IF v>=0 THEN dx:=w ELSE dx:=-w END;
  ELSIF v=0           THEN
    IF h=0 THEN RETURN END;
    dx:=0;
    IF h>=0 THEN dy:=w ELSE dy:=-w END;
  ELSIF ABS(h)=ABS(v) THEN
    IF h=0 THEN RETURN END;
    IF v>=0 THEN
      dx:=  (w * 100000h + 741455) DIV 1482911;
      IF h>=0 THEN dy:= dx ELSE dy:=-dx END
    ELSE
      dx:=-((w * 100000h + 741455) DIV 1482911);
      IF h>=0 THEN dy:=-dx ELSE dy:= dx END
    END
  ELSE
    l:=SQRT(h*h+v*v);
    IF l=0 THEN RETURN END;
    dx:=v*w / l;
    dy:=h*w / l
  END;

  IF (dx=0) & (dy=0) THEN wnd.line(wn,tool,x0,y0,x1,y1); RETURN END;

  x0u:=x0-dx; x0d:=x0+dx;
  y0u:=y0+dy; y0d:=y0-dy;
  x1u:=x1-dx; x1d:=x1+dx;
  y1u:=y1+dy; y1d:=y1-dy;

  wnd.arc(wn,tool,x1,y1,x1d,y1d,x1u,y1u,w);
  wnd.arc(wn,tool,x0,y0,x0u,y0u,x0d,y0d,w);
  wnd.line(wn,tool,x0u,y0u,x1u,y1u);
  wnd.line(wn,tool,x1d,y1d,x0d,y0d)
END line;

PROCEDURE drawsegment(wn: wnd.WINDOW;      p: PROJECT;
                VAR tool: wnd.TOOL;  VAL seg: wld.SEGMENT);
  VAR X1,Y1,X2,Y2,R: INTEGER;    s: wld.TOPOLOGY;
BEGIN
  WITH s DO
    wld.unpack(seg,s);
    tool.color:=layer;  tool.mask:=tool.color;
    X1:=(x1-p^.ldcx) DIV p^.scale;
    Y1:=(y1-p^.ldcy) DIV p^.scale;
    R :=size DIV p^.scale;
    IF (x1=x2) & (y1=y2) THEN
      wnd.circle(wn,tool,X1,Y1,R)
    ELSE
      X2:=(x2-p^.ldcx) DIV p^.scale;
      Y2:=(y2-p^.ldcy) DIV p^.scale;
      line(wn,tool,X1,Y1,X2,Y2,R)
    END
  END
END drawsegment;

PROCEDURE blk2seg(p: PROJECT; VAR box: wld.SEGMENT; VAL b: wnd.BLOCK);
  VAR bx: wld.TOPOLOGY;
BEGIN
  bx.x1:=p^.ldcx + b.x*p^.scale;
  bx.y1:=p^.ldcy + b.y*p^.scale;
  bx.x2:=bx.x1   + b.w*p^.scale - 1;
  bx.y2:=bx.y1   + b.h*p^.scale - 1;
  bx.vsize:=0;     bx.size:=0;
  wld.pack(box,bx)
END blk2seg;

PROCEDURE grid(wn: wnd.WINDOW; b: wnd.BLOCK);
  VAR p: PROJECT;
      t: wnd.TOOL;
      s: INTEGER;
BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.main=wld.mnull THEN RETURN END;

  t:=wn^.inner;
  t.clip:=b;
  wnd.mode(wn,wnd.img);
  b:=wn^.inner.clip;

  s:=p^.gstep DIV p^.scale;
  b.x:=p^.ldcx DIV p^.scale;
  b.y:=p^.ldcy DIV p^.scale;

  b.x:=(ABS(b.x) MOD s);
  IF (b.x#0) & (p^.ldcx>0) THEN b.x:=s-b.x END;
  b.y:=(ABS(b.y) MOD s);
  IF (b.y#0) & (p^.ldcy>0) THEN b.y:=s-b.y END;

(*
gprint0(wn,"gstep=%d s=%d b.x=%d b.y=%d  ",p^.gstep,s,b.x,b.y);
gprint1(wn,"%d %d",p^.ldcx DIV p^.scale,p^.ldcy DIV p^.scale);
*)
  t.mode:=wnd.rep;   t.color:={2,3};
  wnd.grid(wn,t,b,s,s)
END grid;

PROCEDURE refresh(wn: wnd.WINDOW; VAL b: wnd.BLOCK);

  CONST N=32; M=64;

  VAR p: PROJECT;       ibox: wld.ITERBOX;    sav: INTEGER;
    n,m: INTEGER;    seg,box: wld.SEGMENT;   tool: wnd.TOOL;
    sig: wld.SIGNAL;

  PROCEDURE fresh;
  BEGIN
    coff;
    WITH wn^.inner DO wnd.refreshbox(wn,b.x+zX,b.y+zY,b.w,b.h) END
  END fresh;

BEGIN
  tool:=wn^.inner;
  tool.clip:=b;
  wnd.mode(wn,wnd.img);
  tool.mode:=wnd.bic;   tool.color:=tool.mask;
  wnd.rect(wn,tool,b.x,b.y,b.x+b.w-1,b.y+b.h-1);

  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.main=wld.mnull THEN fresh; RETURN END;

  IF p^.grid THEN grid(wn,b) END;
  blk2seg(p,box,b);

  IF NOT wld.first_in_box(seg,box,sig,ibox,p^.main,TRUE) THEN fresh; RETURN END;
  n:=N;
  m:=M;
  sav:=crs.cur;
  crs.style(crs.clock); con;
  tool.mode:=wnd.or;
  REPEAT
    drawsegment(wn,p,tool,seg);
    DEC(n);
    IF n=0 THEN
      n:=N; DEC(m);
      IF m=0 THEN fresh; m:=M END;
      IF cpd.ready()>0 THEN crs.monitor END;
      con
    END
  UNTIL NOT wld.next_in_box(seg,box,sig,ibox);
  coff; crs.style(sav);  fresh
END refresh;

PROCEDURE scroll(wn: wnd.WINDOW; dx,dy: INTEGER); (* screen coordinates *)

  VAR p: PROJECT;
   tool: wnd.TOOL;

  PROCEDURE fresh(VAL b: wnd.BLOCK);
    VAR sig: wld.SIGNAL;
       ibox: wld.ITERBOX;
    seg,box: wld.SEGMENT;
  BEGIN
    IF (b.w=0) OR (b.h=0) THEN RETURN END;
    IF p^.grid THEN grid(wn,b) END;
    blk2seg(p,box,b);
    IF NOT wld.first_in_box(seg,box,sig,ibox,p^.main,TRUE) THEN RETURN END;
    tool.mode:=wnd.or;
    REPEAT
      drawsegment(wn,p,tool,seg);
    UNTIL NOT wld.next_in_box(seg,box,sig,ibox)
  END fresh;

  CONST
    addx = 16;
    addy =  8;

  PROCEDURE cpdscroll;  (* cpd driven scroll *)
    VAR x,y: INTEGER;
  BEGIN
    dx:=0;  dy:=0;
    REPEAT
      WHILE cpd.ready()>0 DO crs.monitor END;
      x:=crs.x-wn^.x-tool.zX;
      y:=crs.y-wn^.y-tool.zY;
      IF    x<p^.Lscrl              THEN x:=x-p^.Lscrl-p^.Xadd
      ELSIF x>=tool.clip.w-p^.Rscrl THEN x:=x-tool.clip.w+p^.Rscrl+p^.Xadd
      ELSE                               x:=0
      END;
      IF    y<0            THEN y:=y-addy
      ELSIF y>=tool.clip.h THEN y:=y-tool.clip.h+addy
      ELSE                      y:=0
      END;
      INC(dx,x); INC(dy,y);
      IF (dx=0) & (dy=0) THEN RETURN END;
      coff;
      crs.move(crs.x-dx,crs.y-dy);
      cpd.wait(40)
    UNTIL cpd.ready()=0
  END cpdscroll;

  VAR vb,hb: wnd.BLOCK;  x,y: INTEGER;

BEGIN
  p:=wn^.obj;
  IF p^.main=wld.mnull THEN RETURN END;
  ASSERT(p^.magic=MAGIC);
  tool:=wn^.inner;
  IF (dx=0) & (dy=0) THEN cpdscroll END;
  IF (dx=0) & (dy=0) THEN RETURN    END;
  wnd.mode(wn,wnd.img);
  tool.mode :=wnd.rep;
  tool.color:=wn^.back;
  wnd.scroll(wn,tool,dx,dy);  (* left: dx>0,  down: dy>0 *)
  IF dx#0 THEN INC(p^.ldcx,dx*p^.scale) END;
  IF dy#0 THEN INC(p^.ldcy,dy*p^.scale) END;
  vb:=tool.clip; (* vertical block *)
  IF    dx>0 THEN vb.x:=vb.w-dx; vb.w:= dx
  ELSIF dx<0 THEN vb.x:=0;       vb.w:=-dx
  ELSE                           vb.w:=0;
  END;
  hb:=tool.clip; (* horizontal block *)
  IF    dy>0 THEN hb.y:=hb.h-dy; hb.h:= dy
  ELSIF dy<0 THEN hb.y:=0;       hb.h:=-dy
  ELSE                           hb.h:=0
  END;
  IF hb.h#0 THEN (* VB := VB - VB x HB *)
    DEC(vb.h,hb.h);
    IF hb.y=0 THEN INC(vb.y,hb.h) END
  END;
  coff;
  fresh(hb);
  fresh(vb);
  WITH wn^.inner DO wnd.refreshbox(wn,zX,zY,clip.w,clip.h) END
END scroll;

----------------------------------------------------------------



PROCEDURE overview(wn: wnd.WINDOW);
  VAR p: PROJECT;
  s,x,y: INTEGER;
  i,w,h: INTEGER;
  sw,sh: INTEGER;
  iw,ih: INTEGER;
BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.main=wld.mnull THEN RETURN END;
  wm.toggle(wn,btneye,TRUE);
  wld.mdl_get_size(p^.main,w,h);
  iw:=wn^.inner.clip.w;
  IF w<iw  THEN sw:=1 ELSE sw:=(w+iw-1) DIV iw END;
  ih:=wn^.inner.clip.h;
  IF h<ih  THEN sh:=1 ELSE sh:=(h+ih-1) DIV ih END;
  IF sw>sh THEN s:=sw ELSE s:=sh END;
  i:=(iw*s-w) DIV 2;
  IF i>0 THEN x:=-i ELSE x:=0 END;
  i:=(ih*s-h) DIV 2;
  IF i>0 THEN y:=-i ELSE y:=0 END;
  IF (p^.ldcx#x) OR (p^.ldcy#y) OR (p^.scale#s) THEN
    p^.ldcx:=x;      p^.ldcy:=y;    setscale(wn,s);
    refresh(wn,wn^.inner.clip)
  END;
  wm.toggle(wn,btneye,FALSE)
END overview;


PROCEDURE zoomscale(wn: wnd.WINDOW; s: INTEGER);
  VAR p: PROJECT;
BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  INC(p^.ldcx, wn^.inner.clip.w*(p^.scale-s) DIV 2);
  INC(p^.ldcy, wn^.inner.clip.h*(p^.scale-s) DIV 2);
  setscale(wn,s);
  refresh(wn,wn^.inner.clip)
END zoomscale;

PROCEDURE zoomin(wn: wnd.WINDOW);
  VAR p: PROJECT;
BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.main#wld.mnull THEN
    wm.toggle(wn,btnzoomin,TRUE);
    zoomscale(wn,p^.scale+1);
  END;
  wm.toggle(wn,btnzoomin,FALSE)
END zoomin;

PROCEDURE zoomout(wn: wnd.WINDOW);
  VAR p: PROJECT;
BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.main#wld.mnull THEN
    wm.toggle(wn,btnzoomout,TRUE);
    IF p^.scale>1 THEN zoomscale(wn,p^.scale-1) END
  END;
  wm.toggle(wn,btnzoomout,FALSE)
END zoomout;

PROCEDURE zoomview(wn: wnd.WINDOW; x,y,w,h: INTEGER);
  VAR p: PROJECT;
      i: INTEGER;
  sw,sh: INTEGER;
  iw,ih: INTEGER;
BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);

  iw:=wn^.inner.clip.w;
  IF w<iw  THEN sw:=1 ELSE sw:=(w+iw-1) DIV iw END;
  ih:=wn^.inner.clip.h;
  IF h<ih  THEN sh:=1 ELSE sh:=(h+ih-1) DIV ih END;
  IF sw>sh THEN setscale(wn,sw) ELSE setscale(wn,sh) END;

  i:=(iw*p^.scale - w) DIV 2;
  IF i>0 THEN p^.ldcx:=x-i ELSE p^.ldcx:=x END;
  i:=(ih*p^.scale - h) DIV 2;
  IF i>0 THEN p^.ldcy:=y-i ELSE p^.ldcy:=y END;
  refresh(wn,wn^.inner.clip)
END zoomview;

PROCEDURE pano(wn: wnd.WINDOW);

  VAR p: PROJECT;
      v: wnd.WINDOW;
     pv: PROJECT;
    sav: INTEGER;
   tool: wnd.TOOL;
  bx,by: INTEGER;
  ex,ey: INTEGER;

  PROCEDURE frame;
    VAR x0,y0,x1,y1: INTEGER;
  BEGIN
    x0:=(bx-pv^.ldcx) DIV pv^.scale;
    y0:=(by-pv^.ldcy) DIV pv^.scale;
    x1:=(ex-pv^.ldcx) DIV pv^.scale;
    y1:=(ey-pv^.ldcy) DIV pv^.scale;
    IF pv^.panfrm THEN wnd.frame(v,tool,x0,y0,x1,y1)
    ELSE               wnd.line (v,tool,x0,y0,x1,y1)
    END
  END frame;

BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  IF p^.main=wld.mnull THEN RETURN END;

  wm.toggle(wn,btnpan,TRUE);
  info(wn,CORNER0);
  sav:=crs.cur;
  crs.style(crs.xcross);  con;
  releaseallcpdbuts;
  LOOP
    REPEAT crs.monitor UNTIL cpd.state^.keys*{0,2}#{};
    v:=wnd.locate(crs.x,crs.y);
    IF (v=wn)   THEN pv:=p; EXIT END;
    IF (v#NIL) & isprj(v) THEN
      pv:=v^.obj;
      IF pv^.main=p^.main THEN EXIT END
    END
  END;
  coff;  crs.style(sav);
  info(wn,"");
  IF cpd.state^.keys*{2}#{} THEN RETURN END;

  tool:=v^.inner;
  tool.mode:=wnd.xor; tool.color:=wnd.scrM/wn^.back;

  bx:=pv^.ldcx+(crs.x-v^.x-tool.zX)*pv^.scale;  ex:=bx;
  by:=pv^.ldcy+(crs.y-v^.y-tool.zY)*pv^.scale;  ey:=by;
  info(wn,CORNER1);
  releaseallcpdbuts;
  REPEAT
    IF cpd.ready()=0 THEN
      wnd.mode(v,wnd.scr+wnd.deep);
      frame;  crs.monitor;  frame;
    ELSE
      crs.monitor
    END;
    scroll(v,0,0);
    ex:=pv^.ldcx+(crs.x-v^.x-tool.zX)*pv^.scale;
    ey:=pv^.ldcy+(crs.y-v^.y-tool.zY)*pv^.scale;
  UNTIL cpd.state^.keys*{0,2}#{};
  info(wn,"");
  IF cpd.state^.keys*{2}#{} THEN RETURN END;

  wnd.mode(v,wnd.normal);
  IF bx>ex THEN sav:=bx; bx:=ex; ex:=sav END;
  IF by>ey THEN sav:=by; by:=ey; ey:=sav END;
  zoomview(wn,bx,by,ex-bx+1,ey-by+1);
  wm.toggle(wn,btnpan,FALSE)
END pano;

----------------------------------------------------------------

PROCEDURE updatename(wn: wnd.WINDOW);
  VAR p: PROJECT;  v: wnd.WINDOW; t: wnd.TOOL;
    x,w: INTEGER;  pname,name,fname: STRING64;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  str.copy(name,p^.fname);
  bio.splitpathname(name,pname);
  str.copy(fname,pname);
  IF p^.main#wld.mnull THEN
    wld.mdl_name(p^.main,name);
    IF wld.done THEN str.print(fname,"%s [%s]",pname,name) END
  END;

  IF p^.icon THEN v:=wn;       t:=v^.inner
  ELSE            v:=p^.wname; t:=v^.full
  END;
  t.color:=wm.normal; t.back:={};
  wnd.rect (v,t,0,0,t.clip.w-1,t.clip.h-1);
  t.color:={};  t.back:=wm.normal;  DEC(t.clip.w,2);
  w:=wnd.lenght(bfnt,"%s",fname);
  IF w<t.clip.w THEN
    x:=(t.clip.w-wnd.lenght(bfnt,"%s",fname)) DIV 2 + 1;
  ELSE
    x:=1
  END;
  wnd.print(v,t,x,1,bfnt,"%s",fname)
END updatename;


PROCEDURE shared(cur: wnd.WINDOW; VAL fname: ARRAY OF CHAR;
                 int: BOOLEAN; VAR mdl: wld.MODEL;
                 msg: BOOLEAN): BOOLEAN;
  VAR v: wnd.WINDOW;  name: STRING64;  p: PROJECT;
BEGIN
  v:=wnd.top;
  WHILE v#NIL DO
    IF (v#cur) & isprj(v) THEN
      p:=v^.obj;
      IF (p^.main#wld.mnull) & (p^.int=int) & (p^.fname=fname) THEN
        mdl:=p^.main;
        wld.mdl_name(mdl,name);
        IF NOT wld.done THEN name:="" END;
        IF NOT msg THEN RETURN TRUE END;
        pup.pushtimeout(10000);
        pup.message(cntX,cntY,SHARED,fname,name);
        pup.poptimeout;
        RETURN TRUE
      END
    END;
    v:=wnd.dw(v)
  END;
  mdl:=wld.mnull;
  RETURN FALSE
END shared;

PROCEDURE _read(wn: wnd.WINDOW; VAL fname: ARRAY OF CHAR; msg: BOOLEAN);
  VAR p: PROJECT;
    mdl: wld.MODEL;
BEGIN
  p:=wn^.obj;
  IF NOT shared(wn,fname,p^.int,mdl,msg) THEN
    wld.read_mdl(mdl,fname);
    IF NOT wld.done THEN wld_error(OPEN); done:=FALSE; RETURN END;
  END;
  wld.open(mdl,p^.int);
  IF NOT wld.done THEN
    wld_error(OPEN);
    IF wld.opencount(mdl)=0 THEN rem_mdl(mdl,p^.int); done:=FALSE; RETURN END
  END;
  IF p^.main#wld.mnull THEN rem_mdl(p^.main,p^.int) END;
  p^.main:=mdl;
  str.copy(p^.fname,fname);
  done:=TRUE
END _read;

PROCEDURE filesmenu(wn: wnd.WINDOW; VAR _quit: BOOLEAN);

  VAR p: PROJECT;  x,y: INTEGER;
      _refresh : BOOLEAN;
      _overview: BOOLEAN;
      _rename  : BOOLEAN;

  PROCEDURE extention(VAR fname: ARRAY OF CHAR; VAL ext: ARRAY OF CHAR);
    VAR i,j,k: INTEGER;
  BEGIN
    i:=str.len(fname);
    j:=str.len(ext);
    IF i>j THEN
      k:=0; DEC(i,j);
      WHILE (j>0) & (fname[i]=ext[k]) DO DEC(j); INC(i); INC(k) END;
      IF j=0 THEN RETURN END
    END;
    str.append(fname,ext)
  END extention;

  PROCEDURE save(as: BOOLEAN);
    VAR tst: bio.FILE;
      fname: STRING128;
  BEGIN
    IF p^.main=wld.mnull THEN
      pup.message(cntX,cntY,NOTHING); done:=FALSE; RETURN
    END;
    IF as THEN
      pup.dselect(p^.Fdrx);
      IF NOT pup.dselected(p^.Fdrx) THEN done:=FALSE; RETURN END;
      pup.dfullname(p^.Fdrx,fname);
      extention(fname,".pwb");
      bio.open(tst,fname,'rw');
      IF (bio.done) OR (NOT bio.done & (bio.error=err.sec_vio)) THEN
        bio.close(tst);
        pup.dclose(p^.Fdrx);
        pup.mclose(p^.Fmenu);
        IF NOT query(EXIST,fname) THEN done:=FALSE; RETURN END
      END;
      str.copy(p^.fname,fname); _rename:=TRUE
    ELSE
      str.copy(fname,p^.fname);
      IF NOT wld.mdl_changed(p^.main) THEN
        pup.dclose(p^.Fdrx);
        pup.mclose(p^.Fmenu);
        IF NOT query(NOCHNG,fname) THEN done:=FALSE; RETURN END
      END
    END;
    wld.write_mdl(p^.main,fname);
    IF NOT wld.done THEN wld_error(SAVE) END
  END save;

  PROCEDURE read;
    VAR i: INTEGER;
      tst: bio.FILE;
     mnew: wld.MODEL;
    fname: STRING128;
  BEGIN
    IF wld.mdl_changed(p^.main) THEN
      i:=sure(MBSAVED,SAVED,NOSAVE,ESC);
      IF (i<0) OR (i=2) THEN RETURN END;
      IF i=0 THEN
        save(FALSE);
        IF NOT done THEN RETURN END
      END
    END;
    pup.dselect(p^.Fdrx);
    IF NOT pup.dselected(p^.Fdrx) THEN done:=FALSE; RETURN END;
    pup.dopenfile(p^.Fdrx,tst,'r');
    IF NOT pup.done THEN done:=FALSE; RETURN END;
    bio.close(tst);
    pup.dfullname(p^.Fdrx,fname);
    _read(wn,fname,TRUE);
    pup.dclose(p^.Fdrx);
    IF NOT done THEN RETURN END;
    _overview:=TRUE; _rename:=TRUE
  END read;

  PROCEDURE rename;
    VAR name: STRING128;
  BEGIN
    done:=TRUE;
    IF p^.main=wld.mnull THEN
      pup.message(cntX,cntY,NOTHING); done:=FALSE; RETURN
    END;
    wld.mdl_name(p^.main,name);
    IF NOT wld.done THEN
      wld_error(RENAME); done:=FALSE; RETURN
    END;
    pup.confirm(cntX,cntY,wnd.scrW DIV 2,name,ENTNAME);
    wld.mdl_rename(p^.main,name);
    IF NOT wld.done THEN
      wld_error(RENAME); done:=FALSE; RETURN
    END;
    _rename:=TRUE
  END rename;

  PROCEDURE clear;
    VAR mname: STRING128;
  BEGIN
    done:=TRUE;
    IF p^.main=wld.mnull THEN RETURN END;
    wld.mdl_name(p^.main,mname);
    IF wld.mdl_changed(p^.main) THEN
      pup.dclose(p^.Fdrx);
      pup.mclose(p^.Fmenu);
      IF NOT query(CHLOST,mname) THEN done:=FALSE; RETURN END
    END;
    rem_mdl(p^.main,p^.int);
    p^.main:=wld.mnull;  p^.scale:=0; p^.fname:="";
    _refresh:=TRUE; _rename:=TRUE
  END clear;

  PROCEDURE quit;
  BEGIN
    clear;
    IF NOT done THEN RETURN END;
    _quit:=TRUE
  END quit;

  PROCEDURE newone;
  BEGIN
    clear;
    IF NOT done THEN RETURN END;
    wld.new_mdl(p^.main,NEWONE);
    IF NOT wld.done THEN
      wld_error(mNEW);  p^.main:=wld.mnull
    ELSE
      str.copy(p^.fname,UNTIT); p^.scale:=1
    END;
    _rename:=TRUE
  END newone;

  PROCEDURE select;
  BEGIN
    LOOP
      done:=TRUE;
      pup.mselect(p^.Fmenu);
      IF NOT pup.mselected(p^.Fmenu) THEN RETURN END;
      coff;
      CASE pup.malt(p^.Fmenu) OF
      |0: read
      |1: save(FALSE)
      |2: save(TRUE)
      |3: rename
      |4: newone
      |5: clear
      |6: quit
      ELSE
      END;
      pup.dclose(p^.Fdrx);
      IF done THEN EXIT END
    END
  END select;

BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  x:=wn^.x+wn^.w DIV 2;
  y:=wn^.y+wn^.h DIV 2;
  _rename   :=FALSE;
  _refresh  :=FALSE;
  _overview :=FALSE;
  wm.toggle(wn,btnfiles,TRUE);
  pup.mopen(p^.Fmenu);
  select;
  coff;
  pup.mclose(p^.Fmenu);
  wm.toggle(wn,btnfiles,FALSE);
  IF _rename    THEN updatename(wn)             END;
  IF _overview  THEN overview(wn)               END;
  IF _refresh   THEN refresh(wn,wn^.inner.clip) END;
END filesmenu;

PROCEDURE savesetup; FORWARD;

PROCEDURE setupmenu(wn: wnd.WINDOW);

  VAR p: PROJECT;
    x,y: INTEGER;
   bump: STRING16;

  PROCEDURE scrollzone(VAR z: INTEGER; alt: INTEGER; VAL name: ARRAY OF CHAR);
    VAR i,pos: INTEGER;  fmt: STRING32;
  BEGIN
    str.print(bump,"%02d",z);
    str.print(fmt,"%s := ",name);
    pup.diabox(x,y,bfnt^.W*3,bump,fmt,z);
    i:=z;  pos:=0;
    str.iscan(i,bump,pos,done);
    IF NOT done THEN RETURN END;
    done:=FALSE;
    IF NOT (i IN {1..23}) THEN
      str.print(fmt,"%s [1..23]",name);  pup.message(x,y,fmt,i);
      RETURN
    END;
    z:=i;
    pup.mprint(p^.Smenu,alt,name,z);
  END scrollzone;

  PROCEDURE select;
  BEGIN
    LOOP
      done:=FALSE;
      pup.mselect(p^.Smenu);
      IF NOT pup.mselected(p^.Smenu) THEN RETURN END;
      coff;
      CASE pup.malt(p^.Smenu) OF
      |0: scrollzone(p^.Lscrl,0,lSCROLL);
      |1: scrollzone(p^.Rscrl,1,rSCROLL);
      |2: scrollzone(p^.Uscrl,2,uSCROLL);
      |3: scrollzone(p^.Dscrl,3,dSCROLL);
      |4: p^.panfrm:=NOT p^.panfrm;
          IF p^.panfrm THEN str.copy(bump,FRAME) ELSE str.copy(bump,LINE) END;
          pup.mprint(p^.Smenu,4,PANO,bump)
      |5: p^.lctr:=NOT p^.lctr;
          IF p^.lctr THEN str.copy(bump,LOC) ELSE str.copy(bump,CRS) END;
          pup.mprint(p^.Smenu,5,"%s",bump)
      |6: savesetup; done:=TRUE;
      ELSE
      END;
      IF done THEN EXIT END
    END
  END select;

BEGIN
  p:=wn^.obj;
  ASSERT(p^.magic=MAGIC);
  x:=wn^.x+wn^.w DIV 2;
  y:=wn^.y+wn^.h DIV 2;
  wm.toggle(wn,btnsetup,TRUE);
  pup.mopen(p^.Smenu);
  select;
  coff;
  pup.mclose(p^.Smenu);
  wm.toggle(wn,btnsetup,FALSE);
END setupmenu;

PROCEDURE hbuttons(wn: wnd.WINDOW);

  VAR prj: PROJECT;
      fmt: ARRAY [0..7] OF CHAR;
  x,y,w,h: INTEGER;

  PROCEDURE p(btn: INTEGER; VAL s: ARRAY OF CHAR);
  BEGIN
    w:=wnd.lenght(bfnt,fmt,s)+6;
    wm.button(wn,btn,wm.luc,x,-h-1,w,h);
    wm.print(wn,btn,bfnt,fmt,s);
    INC(x,w+2)
  END p;

  PROCEDURE v(btn: INTEGER; x,y: INTEGER; ch: CHAR);
  BEGIN
    wm.button(wn,btn,wm.ruc,x,y,wm.ssfont^.W+2,wm.ssfont^.H+2);
    wm.print (wn,btn,wm.ssfont ,"%c",ch);
    DEC(y,h+2)
  END v;

  PROCEDURE off(btn: INTEGER);
  BEGIN
    wm.button(wn,btn,wm.ldc,0,0,0,0);
  END off;

BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  prj:=wn^.obj;
  coff;
  IF prj^.icon THEN
    off(btnleft);     off(btnright);
    off(btnactive);   off(btnfiles);
    off(btnsetup);    off(btnedit);
    RETURN
  END;
  IF wn^.inner.clip.w>=minW THEN fmt:="%s" ELSE fmt:="%1.1s" END;
  h:=wm.ssfont^.H+2;
  v(btnright,-(h+2)+1  ,-h-1,wm.ssright);
  v(btnleft ,-(h+2)*2+1,-h-1,wm.ssleft);

  h:=bfnt^.H+2;
  x:=wm.ssfont^.W+5;
  p(btnactive,"*");
  p(btnfiles,FILES);
  p(btnsetup,SETUP);
  p(btnedit ,EDIT);
  prj^.xnml:=x+1;
  prj^.xnmr:=wn^.w-(h+2)*2-1;
  IF wn=act THEN wm.buttoncolors(wn,btnactive,red,wm.shadow,wm.normal) END;

  y:=wn^.inner.zY+wn^.inner.clip.h;
  wnd.refreshboard(wn,0,y,wn^.full.clip.w,wn^.full.clip.h-y)
END hbuttons;

PROCEDURE vbuttons(wn: wnd.WINDOW);

  VAR x,y,w,h,c: INTEGER;

  PROCEDURE v(btn: INTEGER; ch: CHAR);
  BEGIN
    wm.button(wn,btn,c,x,y,wm.ssfont^.W+2,wm.ssfont^.H+2);
    wm.print (wn,btn,wm.ssfont ,"%c",ch);
    DEC(y,h+2)
  END v;

  PROCEDURE off(btn: INTEGER);
  BEGIN
    wm.button(wn,btn,wm.ldc,0,0,0,0);
  END off;

  VAR prj: PROJECT;

BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  prj:=wn^.obj;
  IF prj^.icon THEN RETURN END;
  h:=wm.ssfont^.H+2;
  IF prj^.icon THEN
    off(btnup);     off(btndw);
    off(btnfire);   off(btneye);
    off(btnpan);    off(btnzoomin);
    off(btntools);  off(btnzoomout);
    off(btngrid);   off(btnicon);
    RETURN
  END;

  coff;

  c:=wm.ldc;
  x:=1;
  y:=wn^.inner.zY+(h+2);
  v(btnup     ,wm.ssup);
  v(btndw     ,wm.ssdw);

  c:=wm.luc;
  x:=1;
  y:=-bfnt^.H-4-h-1;

  v(btnfire   ,wm.ssfire);
  v(btneye    ,wm.sseye);
  v(btnpan    ,wm.sspan);
  v(btnzoomin ,wm.sszoomin);
  v(btnzoomout,wm.sszoomout);
  v(btntools  ,wm.sstool);
  v(btngrid   ,wm.ssgrid);
  v(btnicon   ,wm.ssicon);

END vbuttons;

PROCEDURE putover(wn: wnd.WINDOW);
  VAR p: PROJECT; w,h: INTEGER;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  IF p^.icon THEN RETURN END;
  coff;
  wnd.putover(p^.wname,wn);
  wnd.putover(p^.wcoor,p^.wname);
  wnd.putover(p^.winfo,p^.wcoor);

  h:=bfnt^.H+2;
  w:=p^.xnmr-p^.xnml+1;
  IF w<=0 THEN w:=8 END;
  IF (p^.wname^.w#w) THEN wnd.resize(p^.wname,w,h);  updatename(wn) END;
  w:=(wn^.w-4) DIV 2;
  IF (p^.wcoor^.w#w) THEN wnd.resize(p^.wcoor,w,h);  coord(wn)      END;
  IF (p^.winfo^.w#w) THEN wnd.resize(p^.winfo,w,h);  info(wn,"")    END;
  wnd.move(p^.wname,wn^.x+p^.xnml,wn^.y+wn^.h-p^.wname^.h-1);
  wnd.move(p^.winfo,wn^.x+1,wn^.y+1);
  wnd.move(p^.wcoor,wn^.x+1+p^.wcoor^.w+1,wn^.y+1);
END putover;

PROCEDURE newwindow(VAR wn: wnd.WINDOW; toopen: BOOLEAN);

  VAR  p: PROJECT;  x,y,w,h: INTEGER;

  PROCEDURE undo(e: INTEGER);
  BEGIN
    perror(e); wm.dispose(wn); dispose(p)
  END undo;

BEGIN
  p:=NIL;     wn:=NIL;
  new(p);
  IF NOT done THEN RETURN END;
  wm.new(wn);
  IF NOT wm.done  THEN undo(wm.error); RETURN END;
  wm.disable(wn);
  wnd.mask (wn,wnd.scrM,{});
  wnd.image(wn,TRUE);
  IF NOT wnd.done THEN undo(wnd.error); RETURN END;
  wnd.mode  (wn,wnd.normal);
  IF NOT wnd.done THEN undo(wnd.error); RETURN END;
  IF wcount=0 THEN
    w:=wnd.scrW*3 DIV 4;
    h:=wnd.scrH*3 DIV 4
  ELSE
    w:=minW;
    h:=minH;
  END;
  wnd.resize(wn,w,h);
  IF NOT wnd.done THEN undo(wnd.error); RETURN END;
  y:=wm.ssfont^.W+4;
  wnd.inner(wn,y,4+bfnt^.H,wn^.w-y-4,wn^.h-(bfnt^.H*2+4+4));
  IF NOT wnd.done THEN undo(wnd.error); RETURN END;
  IF wcount=0 THEN
    x:=wnd.scrW DIV 8;  y:=wnd.scrH DIV 8;
  ELSE
    CASE wcount MOD 4 OF
    |0: x:=0;           y:=0
    |1: x:=wnd.scrW-w;  y:=0
    |2: y:=wnd.scrH-h;  x:=0
    |3: y:=wnd.scrH-h;  x:=wnd.scrW-w
    END
  END;
  wnd.move(wn,x,y);
  IF NOT wnd.done THEN undo(wnd.error); RETURN END;
  wnd.object(wn,p);

  p^.x:=x;  p^.y:=y;
  p^.w:=w;  p^.h:=h;

  wnd.new(p^.wname);         wnd.image(p^.wname,TRUE);
  wnd.new(p^.wcoor);         wnd.image(p^.wcoor,TRUE);
  wnd.new(p^.winfo);         wnd.image(p^.winfo,TRUE);
  p^.over :=NIL;

  vbuttons(wn);
  hbuttons(wn);

  putover(wn);

  makemenus(wn);
  wnd.putunder(wn,main);

  INC(wcount);
  IF NOT toopen THEN RETURN END;
  wnd.open(wn);
  wnd.open(p^.wname);
  wnd.open(p^.wcoor);
  wnd.open(p^.winfo);
  activate(wn);
END newwindow;

PROCEDURE disposewindow(wn: wnd.WINDOW);
  VAR p: PROJECT;
     na: BOOLEAN;
BEGIN
  IF wn=NIL    THEN RETURN END;
  IF isprj(wn) THEN
    p:=wn^.obj;
    wnd.dispose(p^.wname);
    wnd.dispose(p^.wcoor);
    wnd.dispose(p^.winfo);
    dispose(p)
  END;
  na:=(act#wn);
  wm.dispose(wn);
  IF na THEN RETURN END;
  act:=NIL;
  activatetop
END disposewindow;

PROCEDURE openall;
  VAR wn: wnd.WINDOW;  p: PROJECT;
BEGIN
  wn:=wnd.top;
  WHILE wn#NIL DO
    IF wn^.closed & isprj(wn) THEN
      p:=wn^.obj;
      wnd.open(wn);
      IF NOT p^.icon THEN
        putover(wn); wnd.open(p^.wname); wnd.open(p^.wcoor); wnd.open(p^.winfo)
      END
    END;
    wn:=wnd.dw(wn)
  END;
  IF act=NIL THEN activatetop END
END openall;


PROCEDURE savesetup;

  VAR f: bio.FILE;
      v: wnd.WINDOW;
      p: PROJECT;

  PROCEDURE put(w: SYSTEM.WORD);
  BEGIN
    bio.print(f,"%04d\n",w);
  END put;

BEGIN
  openall;
  bio.create(f,"pwb.setup",'wh',4096);
  bio.buffers(f,1,4096);
  v:=wnd.bottom;
  bio.print(f,"%s\n",VERSION);
  put(language);
  put(main^.x);
  put(main^.y);
  WHILE v#NIL DO
    IF isprj(v) THEN
      p:=v^.obj;
      IF (p^.main#wld.mnull) THEN
        bio.print(f,"PROJECT\n");
        put(p^.x);               put(p^.y);
        put(p^.w);               put(p^.h);
        put(p^.icon);            put(p^.fire);
        put(p^.grid);            put(p^.panfrm);
        put(p^.lctr);            put(p^.int);
        put(p^.ldcx);            put(p^.ldcy);
        put(p^.scale);
        put(p^.Lscrl);           put(p^.Rscrl);
        put(p^.Dscrl);           put(p^.Uscrl);
        put(p^.Xadd);            put(p^.Yadd);
        put(p^.ex);              put(p^.ey);
        put(p^.gstep);           put(p^.step);
        bio.print(f,"%s\n",p^.fname)
      END
    END;
    v:=wnd.up(v)
  END;
  bio.close(f)
END savesetup;

PROCEDURE _icon(wn: wnd.WINDOW; on: BOOLEAN); FORWARD;
PROCEDURE _fire(wn: wnd.WINDOW; on,chk: BOOLEAN); FORWARD;
PROCEDURE makemain; FORWARD;

PROCEDURE restoresetup;

  VAR f: bio.FILE;
      p: PROJECT;
     wn: wnd.WINDOW;
    bad: BOOLEAN;
   bump: STRING128;

  PROCEDURE get(VAR w: SYSTEM.WORD);
    VAR i: INTEGER;
  BEGIN
    bio.getstr(f,bump,0);
    bad:=bad OR NOT bio.done;
    i:=0;
    str.iscan(w,bump,i,done);
    bad:=bad OR NOT done
  END get;

  PROCEDURE makewindow;
    VAR y: INTEGER;  icon,fire: BOOLEAN;
  BEGIN
    _read(wn,p^.fname,FALSE);
    IF NOT done THEN bad:=TRUE; RETURN END;
    IF (p^.x<-wnd.scrW) OR (p^.x>wnd.scrW  ) THEN bad:=TRUE; RETURN END;
    IF (p^.w<0        ) OR (p^.w>wnd.scrW*2) THEN bad:=TRUE; RETURN END;
    IF (p^.y<-wnd.scrH) OR (p^.y>wnd.scrH  ) THEN bad:=TRUE; RETURN END;
    IF (p^.h<0        ) OR (p^.h>wnd.scrH*2) THEN bad:=TRUE; RETURN END;
    icon:=p^.icon;  p^.icon:=FALSE;
    fire:=p^.fire;  p^.fire:=FALSE;
    wnd.move  (wn,p^.x,p^.y);
    wnd.resize(wn,p^.w,p^.h);
    y:=wm.ssfont^.W+4;
    wnd.inner(wn,y,4+bfnt^.H,wn^.w-y-4,wn^.h-(bfnt^.H*2+4+4));
    hbuttons(wn);
    IF NOT icon THEN refresh(wn,wn^.inner.clip) END;
    wnd.putunder(wn,main);
    putover(wn);
    IF icon THEN _icon(wn,TRUE);       RETURN END;
    IF fire THEN _fire(wn,TRUE,FALSE); RETURN END;
    coff;
    wnd.open(wn);
    wnd.open(p^.wname);  updatename(wn);
    wnd.open(p^.wcoor);  coord(wn);
    wnd.open(p^.winfo);  info(wn,"");
    makemenus(wn);
    done:=TRUE
  END makewindow;

  PROCEDURE illegal;
  BEGIN
    disposewindow(wn);
    pup.pushtimeout(10000);
    pup.message(cntX,cntY,ILGSETUP);
    pup.poptimeout;
    bio.close(f);
    bio.unlink("pwb.setup")
  END illegal;

  VAR x,y: INTEGER;

BEGIN
  wn:=NIL; p:=NIL;
  bio.open(f,"pwb.setup",'r');
  IF NOT bio.done THEN RETURN END;
  bio.buffers(f,1,4096);
  bio.getstr(f,bump,0);
  IF NOT bio.done OR (bump#VERSION) THEN illegal; RETURN END;
  bad:=FALSE;
  get(language);
  IF bad OR (language<0) OR (language>=languages) THEN illegal; RETURN END;
  get(x);
  get(y);
  wnd.move(main,x,y);
  texts;
  makemain;
  LOOP
    IF bio.pos(f)>=bio.eof(f) THEN EXIT END;
    crs.move (cntX,cntY);
    crs.style(crs.clock); con;
    newwindow(wn,FALSE);
    IF wn#NIL THEN
      p:=wn^.obj;
      bio.getstr(f,bump,0);
      IF NOT bio.done OR (bump#"PROJECT") THEN illegal; RETURN END;
      bad:=FALSE;
      get(p^.x);               get(p^.y);
      get(p^.w);               get(p^.h);
      get(p^.icon);            get(p^.fire);
      get(p^.grid);            get(p^.panfrm);
      get(p^.lctr);            get(p^.int);
      get(p^.ldcx);            get(p^.ldcy);
      get(p^.scale);
      get(p^.Lscrl);           get(p^.Rscrl);
      get(p^.Dscrl);           get(p^.Uscrl);
      get(p^.Xadd);            get(p^.Yadd);
      get(p^.ex);              get(p^.ey);
      get(p^.gstep);           get(p^.step);
      bio.getstr(f,p^.fname,0);
      bad:=bad OR NOT bio.done;
      IF bad THEN illegal; EXIT END;
      makewindow;
      IF bad THEN illegal; EXIT END;
    END
  END;
  IF wn#NIL THEN activate(wn) END;
  bio.close(f)
END restoresetup;



PROCEDURE cascade;
  VAR wn: wnd.WINDOW; x,y: INTEGER;  p: PROJECT;
BEGIN
  wn:=wnd.bottom;  x:=0;  y:=wnd.scrH-8;
  WHILE wn#NIL DO
    IF (NOT wn^.closed) & isprj(wn) THEN
      p:=wn^.obj;
      wnd.close(p^.wname);
      wnd.close(p^.wcoor);
      wnd.close(p^.winfo);
      p^.x:=x;  p^.y:=y-wn^.h;
      wnd.move(wn,p^.x,p^.y);
      IF NOT p^.icon THEN
        putover(wn);
        wnd.open(p^.wname); wnd.open(p^.wcoor); wnd.open(p^.winfo);
        makemenus(wn)
      END;
      DEC(y,bfnt^.H+4);  INC(x,bfnt^.H+4)
    END;
    wn:=wnd.up(wn)
  END
END cascade;

PROCEDURE changelanguage;
  VAR wn: wnd.WINDOW;
BEGIN
  releaselastpressed;
  language:=(language+1) MOD languages;
  texts;
  makemain;
  wn:=wnd.top;
  WHILE wn#NIL DO
    IF isprj(wn) THEN hbuttons(wn); putover(wn); makemenus(wn) END;
    wn:=wnd.dw(wn)
  END
END changelanguage;

PROCEDURE inneredit(wn: wnd.WINDOW); -- !!!
  VAR p: PROJECT;
     ch: CHAR;
  ox,oy: INTEGER;
  cx,cy: INTEGER;
    xor: wnd.TOOL;

  PROCEDURE cross;
  BEGIN
    IF NOT p^.lctr THEN RETURN END;
    coff;
    cx:=(ox-p^.ldcx) DIV p^.scale;
    cy:=(oy-p^.ldcy) DIV p^.scale;
    wnd.mode(wn,wnd.deep+wnd.scr);
    wnd.line(wn,xor,cx,0,cx,wn^.h-1);
    wnd.line(wn,xor,0,cy,wn^.w-1,cy);
  END cross;

  PROCEDURE xystump;
  BEGIN
    ox:=p^.ex; oy:=p^.ey;
  END xystump;

BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  IF p^.main=wld.mnull THEN RETURN END;
  crs.style(crs.xcross);
  xystump;
  cx:=(ox-p^.ldcx) DIV p^.scale + wn^.x + wn^.inner.zX;
  cy:=(oy-p^.ldcy) DIV p^.scale + wn^.y + wn^.inner.zY;
  crs.move(cx,cy);
  xor:=wn^.inner;
  xor.mode:=wnd.xor;
  xor.color:=wnd.scrM;
  IF p^.lctr THEN coff END;
  REPEAT
    IF NOT p^.lctr THEN con END;
    IF ((ox#p^.ex) OR (oy#p^.ey)) & (cpd.ready()+key.ready()<=0) THEN
      coord(wn); xystump
    END;
    cross; crs.monitor; cross;
    p^.ex:=(crs.x-wn^.x-wn^.inner.zX)*p^.scale+p^.ldcx;
    p^.ey:=(crs.y-wn^.y-wn^.inner.zY)*p^.scale+p^.ldcy;
    scroll(wn,0,0);
    IF key.ready()>0 THEN
      key.read(ch);
      IF CAP(ch)='R' THEN refresh(wn,wn^.inner.clip) END
    END
  UNTIL cpd.state^.keys*{2}#{};
END inneredit;


PROCEDURE _resize(wn: wnd.WINDOW; ow,oh,w,h: INTEGER);
  VAR p: PROJECT;
     vb: wnd.BLOCK;
     hb: wnd.BLOCK;
BEGIN
  p:=wn^.obj;

  wnd.resize(wn,w,h);
  hb:=wn^.inner.clip;
  vb:=wn^.inner.clip;
  IF ow<vb.w THEN vb.x:=ow; vb.w:=vb.w-ow ELSE vb.w:=0 END;
  IF oh<hb.h THEN hb.y:=oh; hb.h:=hb.h-oh ELSE hb.h:=0 END;
  IF hb.h>0 THEN DEC(vb.h,hb.h) END; (* VB := VB - VB x HB *)
  IF hb.h>0 THEN refresh(wn,hb) END;
  IF vb.w>0 THEN refresh(wn,vb) END;
END _resize;

PROCEDURE _fire(wn: wnd.WINDOW; on,chk: BOOLEAN);
  VAR p: PROJECT;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  IF chk & (p^.w=wnd.scrW) & (p^.h=wnd.scrH) & (p^.x=0) & (p^.y=0) THEN
    RETURN
  END;
  wnd.close(wn);
  wnd.close(p^.wname);  wnd.close(p^.wcoor);  wnd.close(p^.winfo);
  p^.fire:=on;
  IF p^.fire THEN
    p^.over:=wnd.up(wn);
    WHILE (p^.over#NIL) & NOT isprj(p^.over) DO p^.over:=wnd.up(p^.over) END;
    wnd.putunder(wn,main);
    activate(wn);
    wnd.move(wn,0,0);
    _resize(wn,wn^.inner.clip.w,wn^.inner.clip.h,wnd.scrW,wnd.scrH);
    wm.buttoncolors(wn,btnfire,red,wm.shadow,wm.normal)
  ELSE
    wnd.putunder(wn,p^.over);
    _resize(wn,wnd.scrW,wnd.scrH,p^.w,p^.h);
    wnd.move(wn,p^.x,p^.y);
    wm.buttoncolors(wn,btnfire,wm.black,wm.shadow,wm.normal)
  END;
  hbuttons(wn);
  putover(wn);
  wnd.open(wn);
  wnd.open(p^.wname);  wnd.open(p^.wcoor);  wnd.open(p^.winfo);
  makemenus(wn)
END _fire;

PROCEDURE fire(wn: wnd.WINDOW);
  VAR p: PROJECT;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  p^.fire:=NOT p^.fire;
  _fire(wn,p^.fire,TRUE);
END fire;

PROCEDURE _icon(wn: wnd.WINDOW; icon: BOOLEAN);
  VAR p: PROJECT;  x,y: INTEGER;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  IF p^.icon=icon THEN RETURN END;
  p^.icon:=icon;
  IF icon THEN
    wnd.close(p^.wname);
    wnd.close(p^.wcoor);
    wnd.close(p^.winfo);
    wnd.close(wn);
    hbuttons(wn);
    wnd.move(wn,wn^.x,wn^.y+wn^.h-(wm.ssfont^.H+4));
    x:=(wm.ssfont^.W+4)*2+4;
    wnd.resize(wn,x + p^.wname^.w+4,wm.ssfont^.H+4);
    wnd.inner (wn,x,2,p^.wname^.w,wm.ssfont^.H);
    updatename(wn);
    vbuttons(wn);
    wm.button(wn,btnicon,wm.ldc,wm.ssfont^.W+5,1,wm.ssfont^.W+2,wm.ssfont^.H+2);
    wm.print (wn,btnicon,wm.ssfont,'%c',wm.ssicon);
    wnd.open(wn)
  ELSE
    wnd.close(p^.wname);
    wnd.close(p^.wcoor);
    wnd.close(p^.winfo);
    wnd.close(wn);
    IF NOT p^.fire THEN
      wnd.resize(wn,p^.w,p^.h);
      wnd.move  (wn,p^.x,p^.y);
      y:=wm.ssfont^.W+4;
      wnd.inner(wn,y,4+bfnt^.H,wn^.w-y-4,wn^.h-(bfnt^.H*2+4+4));
      hbuttons(wn);
      vbuttons(wn);
      putover(wn);
      wnd.open(wn);
      wnd.open(p^.wname);
      wnd.open(p^.wcoor);
      wnd.open(p^.winfo);
      refresh(wn,wn^.inner.clip);
      makemenus(wn)
    ELSE
      wnd.resize(wn,minW,minH);
      y:=wm.ssfont^.W+4;
      wnd.inner(wn,y,4+bfnt^.H,wn^.w-y-4,wn^.h-(bfnt^.H*2+4+4));
      refresh(wn,wn^.inner.clip);
      vbuttons(wn);
      _fire(wn,TRUE,FALSE)
    END
  END
END _icon;

PROCEDURE icon(wn: wnd.WINDOW);
  VAR p: PROJECT;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  _icon(wn,NOT p^.icon)
END icon;

PROCEDURE resize(wn: wnd.WINDOW);
  VAR p: PROJECT;
     ow: INTEGER;
     oh: INTEGER;
BEGIN
  IF wn=main THEN RETURN END;
  p:=wn^.obj;
  IF p=NIL THEN RETURN END;
  coff;
  IF (wm.resizeW<32) & (wm.resizeH<32) THEN _icon(wn,TRUE); RETURN END;
  IF p^.icon THEN _icon(wn,FALSE); RETURN END;
  IF p^.fire THEN fire(wn);        RETURN END;
  ow:=wn^.inner.clip.w;
  oh:=wn^.inner.clip.h;
  p^.w:=wm.resizeW;
  IF p^.w<minW THEN p^.w:=minW END;
  p^.h:=wm.resizeH;
  IF p^.h<minH THEN p^.h:=minH END;

  wnd.close(p^.wname); wnd.close(p^.wcoor); wnd.close(p^.winfo);
  _resize(wn,ow,oh,p^.w,p^.h);
  hbuttons(wn);
  putover(wn);
  wnd.open(p^.wname);  wnd.open(p^.wcoor);  wnd.open(p^.winfo);
  makemenus(wn)
END resize;

PROCEDURE move(wn: wnd.WINDOW);
  VAR p: PROJECT; x,y: INTEGER;
BEGIN
  p:=wn^.obj;
  IF p=NIL THEN wnd.move(wn,wm.moveX,wm.moveY); RETURN END;
  coff;
  x:=wm.moveX;
  y:=wm.moveY;
  IF NOT p^.icon THEN p^.x:=x; p^.y:=y END;
  IF p^.fire THEN RETURN END;
  wnd.close(p^.wname);
  wnd.move(wn,x,y);
  IF p^.icon THEN RETURN END;
  putover(wn);
  wnd.open(p^.wname);
  makemenus(wn)
END move;

PROCEDURE close(wn: wnd.WINDOW);
  VAR p: PROJECT;
BEGIN
  IF wn=main THEN RETURN END;
  IF NOT isprj(wm.active) THEN RETURN END;
  coff;
  p:=wm.active^.obj;
  wnd.close(p^.wname);
  wnd.close(p^.wcoor);
  wnd.close(p^.winfo);
  wnd.close(wm.active);
  IF wm.active=act  THEN activate(NIL); activatetop END;
  IF NOT firstclose THEN RETURN END;
  pup.pushtimeout(5000);
  pup.message(cntX,cntY,TOOPEN);
  pup.poptimeout;  firstclose:=FALSE
END close;


PROCEDURE _grid(wn: wnd.WINDOW);
  VAR p: PROJECT;
BEGIN
  IF NOT isprj(wn) THEN RETURN END;
  p:=wn^.obj;
  p^.grid:=NOT p^.grid;
  refresh(wn,wn^.inner.clip)
END _grid;

PROCEDURE abort;
BEGIN
  IF NOT query(QTLOST) THEN wm.toggle(main,btnquit,FALSE); RETURN END;
  savesetup;
  HALT
END abort;

PROCEDURE help;
BEGIN

END help;

PROCEDURE editor;
  VAR ch: CHAR; loc: wnd.WINDOW;  drop: wnd.WINDOW; quit: BOOLEAN;
BEGIN
  restoresetup;
  IF projs=NIL THEN wcount:=0; newwindow(drop,TRUE) END;
  LOOP
    IF main^.closed THEN coff; wnd.open (main) END;
    IF main#wnd.top THEN coff; wnd.ontop(main) END;
    quit:=FALSE;
    crs.style(crs.arrow); con;
    wm.monitor;
    IF key.ready()>0 THEN key.read(ch) ELSE ch:=0c END;
    loc:=wnd.locate(crs.x,crs.y);
    CASE ch OF
    |key.break: coff; abort
    |key.f1   : coff; help
    |key.f10  : coff; cascade
    |key.f9   : coff; openall
    |key.f3   : coff; newwindow(drop,TRUE);
    ELSE
    END;
    IF (loc=main) & (wm.abutton>=0) THEN
      CASE wm.abutton OF
      |btncascade: coff; cascade
      |btnzoom   : coff; pup.zoom
      |btnopen   : coff; openall
      |btnnew    : coff; newwindow(drop,TRUE)
      |btnquit   : coff; abort
      |btnlang   : coff; changelanguage
      ELSE
      END
    END;
    IF (ch#0c) & isprj(loc) THEN
      CASE ASCII.CAPITAL(ch) OF
      |'X'     : coff; zoomin  (loc)
      |'Z'     : coff; zoomout (loc)
      |'O'     : coff; overview(loc)
      |'R'     : coff; refresh(loc,loc^.inner.clip)
      |'G'     : coff; _grid(loc)
      |'I'     : coff; icon(loc)
      |key.f4  : coff; close(loc)
      |key.f5  : coff; fire(loc)
      |key.end : coff; scroll(loc,+loc^.inner.clip.w DIV 8,0)
      |key.home: coff; scroll(loc,-loc^.inner.clip.w DIV 8,0)
      |key.pgup: coff; scroll(loc,0,+loc^.inner.clip.h DIV 8)
      |key.pgdw: coff; scroll(loc,0,-loc^.inner.clip.h DIV 8)
      ELSE
      END
    END;
    IF (ch#0c) & (loc=act) THEN
      CASE ASCII.CAPITAL(ch) OF
      |'F': coff; filesmenu(act,quit)
      |'S': coff; setupmenu(act)
      |'P': coff; pano(act)
      ELSE
      END
    END;
    IF isprj(loc) & (wm.active=loc) & (ch=0c) & (wm.abutton>=0) THEN
      CASE wm.abutton OF
      |btnzoomin : coff; zoomin(loc)
      |btnzoomout: coff; zoomout(loc)
      |btneye    : coff; overview(loc)
      |btnright  : coff; scroll(loc,+loc^.inner.clip.w DIV 8,0)
      |btnleft   : coff; scroll(loc,-loc^.inner.clip.w DIV 8,0)
      |btnup     : coff; scroll(loc,0,+loc^.inner.clip.h DIV 8)
      |btndw     : coff; scroll(loc,0,-loc^.inner.clip.h DIV 8)
      |btnactive : coff; activate(loc)
      |btnicon   : coff; icon(loc)
      |btngrid   : coff; _grid(loc)
      |btnfire   : coff; fire(loc)
      ELSE
      END
    END;
    IF isprj(act) & (wm.active=act) & (ch=0c) & (wm.abutton>=0) THEN
      CASE wm.abutton OF
      |btnfiles  : coff; filesmenu(act,quit)
      |btnsetup  : coff; setupmenu(act)
      |btnpan    : coff; pano(act)
      |btnedit   : coff; inneredit(act)
      ELSE
      END
    ELSE
    END;
    releaselastpressed;
    IF    quit          THEN disposewindow(act)
    ELSIF wm.closed     THEN close  (wm.active)
    ELSIF wm.resized    THEN resize (wm.active)
    ELSIF wm.moved      THEN move   (wm.active)
    ELSIF wm.active#NIL THEN putover(wm.active)
    END;
    con;
    releaseallcpdbuts
  END
END editor;

PROCEDURE makemain;

  VAR x,y,w,h,W: INTEGER;

  PROCEDURE p(btn: INTEGER; f: wnd.FONT; VAL s: ARRAY OF CHAR);
  BEGIN
    w:=wnd.lenght(f,s)+2;
    wm.button(main,btn,wm.luc,x,-wm.ssfont^.H-2-1,w,h);
    wm.print (main,btn,f,s);
    INC(x,w+2)
  END p;

  PROCEDURE v(btn: INTEGER; f: wnd.FONT; VAL s: ARRAY OF CHAR);
  BEGIN
    wm.button(main,btn,wm.ldc,x,y,W-2,h);
    wm.print (main,btn,f,s);
    INC(y,bfnt^.H+4)
  END v;

  VAR old: wnd.WINDOW;

BEGIN
  old:=main;
  h:=wm.ssfont^.H+2;
  wm .new    (main);
  IF NOT wm.done THEN perror(wm.error); RETURN END;
  wm .disable(main);
  wnd.image  (main,FALSE);
  wnd.mask   (main,wnd.scrM,{});
  wnd.ontop  (main);
  W:=(wm.ssfont^.W+4)*3;
  wnd.resize (main,W,h+2+(bfnt^.H+4)*4);
  wnd.inner  (main,0,0,0,0);
  wnd.move   (main,wnd.scrW-1-W-2,2);
  x:=1+h+2;
  p(btncascade,wm.ssfont,wm.sscascade);
  p(btnzoom   ,wm.ssfont,wm.sszoom);
  x:=1; y:=1;
  v(btnlang   ,bfnt,LAN);
  v(btnquit   ,bfnt,qUIT);
  v(btnopen   ,bfnt,oPEN);
  v(btnnew    ,bfnt,nEW);
  IF old#NIL THEN wnd.move(main,old^.x,old^.y); wnd.open(main) END;
  wm.dispose(old)
END makemain;

VAR starttime: INTEGER;  h0,h1,h2,h3,h4: STRING32;

PROCEDURE header;

  PROCEDURE _h(VAR s: ARRAY OF CHAR; SEQ c: CHAR);
    VAR i,j: INTEGER;
  BEGIN
    j:=0;
    FOR i:=0 TO HIGH(c) BY 2 DO s[j]:=c[i]; INC(j) END;
    s[j]:=0c
  END _h;

BEGIN
  IF FOE THEN
    _h(h0,'(','1','c','+',')','(',' ','i','K','P','R','N','O','B','N','|','O','K','S','Q',' ','F','1','8','9','7','9','6','1');
    _h(h1,'h','y','a','h','c','t','k','1','e','k','d','r',' ','y','b','v','y','o',' ','w','L','N','e','g','o','t','p','w','o','j','l','g','d');
    _h(h2,'a','h','t','g',' ','d','t','i','h','h','e','e',' ','t','b','c','e','k','s','s','t','k',' ','b','t','r','i','s','m','w','e','q','s');
    _h(h3,'s','g','t','y','e','v','a','j','l','g','e','w','d','r',' ','n','f','m','r','x','o','i','m','h',' ','d','m','x','a','r','i','a','n');
    _h(h4,'j','b','o','r','b','i',' ','m','o','f','f','q',' ','e','h','r','i','t','s','p',' ','j','l','u','i','c','f','e','e','f','.','z','.',':','.');
  END
END header;

PROCEDURE ground(wn: wnd.WINDOW; x,y,w,h: INTEGER);

  CONST P0={0,4,8,12,16,20,24,28}/{0..31};
        P1={0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
        P2=P0<<2;
        P3=P1;

  VAR b: wnd.BLOCK;  t: wnd.TOOL;

  PROCEDURE p;
  BEGIN
    wnd.print(wn,t,x,y+44,bfnt,h0);
    wnd.print(wn,t,x,y+33,bfnt,h1);
    wnd.print(wn,t,x,y+22,bfnt,h2);
    wnd.print(wn,t,x,y+11,bfnt,h3);
    wnd.print(wn,t,x,y+00,bfnt,h4);
  END p;

BEGIN
  t:=wn^.full;         b:=t.clip;
  t.mode :=wnd.rep;    t.clip.x:=x;  t.clip.w:=w;
  t.color:=wm.shadow;  t.clip.y:=y;  t.clip.h:=h;
  wnd.fill(wn,t,b,32,P0,P1,P2,P3);

  IF FOE THEN
    IF tim.time()-starttime<15*60 THEN RETURN END;
    IF h0="" THEN header END;
    x:=cntX-40;
    y:=cntY-27;
    DEC(x); DEC(y);
    t.mode :=wnd.or;
    t.color:=wm.shadow;
    t.back :={};
    p;
    INC(x,2); INC(y,2);
    t.mode :=wnd.or;
    t.color:=wm.shadow;
    p;
    DEC(x); DEC(y);
    t.mode :=wnd.bic;
    t.color:=wnd.scrM;
    p;
  END
END ground;

BEGIN
  starttime:=tim.time();
  IF NOT DEBUG THEN key.set_break(0) END;
  wcount:=0;
  MAGIC:=4A4F5250h;
  bfnt :=pup.font;
  IF NOT FOE THEN language:=English ELSE language:=Russian END;
  red:={0,3};
  texts;
  projs:=NIL;
  act  :=NIL;
  cntX :=wnd.scrW DIV 2;
  cntY :=wnd.scrH DIV 2;
  minW :=((bfnt^.H+4)*4+(wm.ssfont^.H+4)*3)*2;
  minH := (bfnt^.H+4)*2+(wm.ssfont^.H+4)*10  ;
  main :=NIL;
  h0   :="";
  wnd.boarder(wnd.desktop,ground);
  wnd.painter(wnd.desktop,ground);
  wnd.refresh(wnd.desktop);
  makemain;
  firstclose:=TRUE;
  pup.pushtimeout(60000);
  editor
END pwb.
