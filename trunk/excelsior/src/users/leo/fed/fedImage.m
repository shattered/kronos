IMPLEMENTATION MODULE fedImage; (* nick 14-Dec-90. (c) KRONOS *)

FROM  SYSTEM IMPORT ADR, ADDRESS, WORD;
IMPORT  cod: defCodes;          IMPORT  crs: pmCrs;
IMPORT  scr: fedBase;           IMPORT  low: lowLevel;
IMPORT  mem: Heap;              IMPORT  bmg: BMG;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

VAR tool: bmg.TOOL;
    pBUF: ADDRESS;  BUF: ARRAY [0..31] OF WORD;
    pSCL: ADDRESS;  SCL: ARRAY [0..15] OF ARRAY [0..1] OF WORD;
    pMIR: ADDRESS;  MIR: ARRAY [0..255] OF CHAR;
    pLN0: ADDRESS;  LN0: ARRAY [0..4] OF INTEGER;
    pLNF: ADDRESS;  LNF: ARRAY [0..4] OF INTEGER;
    pIND: ADDRESS;  IND: INTEGER;
    pDOT: ADDRESS;  DOT: ARRAY [0..1] OF BITSET;
    full: bmg.BLOCK;
        (* scal - масштаб увеличения точек образа символа.         *)
        (* в BUF создается увеличенная в scal раз строка из образа *)
        (* символа, pBUF - указатель на этот массив. в SCL хранятся*)
        (* образы кусков строк по четыре бита, pSCL - указатель на *)
        (* этот массив. DOT - вспомогательное слово, используемое  *)
        (* для заполнения массива SCL                              *)

----------------------------- INIT -----------------------------
                             ------
PROCEDURE init_misc;
  VAR i,j: INTEGER;
    f,b,c: BITSET;
BEGIN
  NEW(main);
  IF NOT mem.done THEN HALT(mem.error) END;
  WITH main^ DO
    mask:={0};
    W   :=256;
    H   :=256;
    WPL :=(W+31) DIV 32;
    PATTERN:={0..31};
    mem.allocate(BASE,H*WPL);
    IF NOT mem.done THEN HALT(mem.error) END;
    low.fill(layers,NIL);
    layers[0]:=BASE;
    low._zero(BASE,H*WPL)
  END;
  WITH work DO x:=0; y:=0; w:=main^.W; h:=main^.H END;
  low.fill(LN0, 0);  pLN0:=ADR(LN0);  full:=work;
  low.fill(LNF,-1);  pLNF:=ADR(LNF);  pBUF:=ADR(BUF);  pSCL:=ADR(SCL);
  low.fill(DOT,-1);  pDOT:=ADR(DOT);  pMIR:=ADR(MIR);  pIND:=ADR(IND);
  tool.zX:=0;  tool.mode:=bmg.rep;  tool.back:={ };
  tool.zY:=0;  tool.clip:=work;     tool.mask:={0};  tool.color:={0};
  FOR i:=0b TO 377b DO
    f:={0}; b:={7}; c:={};
    FOR j:=0 TO 7 DO
      IF f*BITSET(i)#{} THEN c:=c+b END;
      f:=f<<1; b:=b>>1
    END;
    MIR[i]:=CHAR(c)
  END
END init_misc;

------------------------ CODE PROCEDURES -----------------------
                        -----------------

PROCEDURE bitrepl(des: ADDRESS; off_des: INTEGER;
                  sou: ADDRESS; off_sou: INTEGER; no: INTEGER);
CODE cod.bmv END bitrepl;

PROCEDURE bitmove(mod: INTEGER;
                  des: ADDRESS; off_des: INTEGER;
                  sou: ADDRESS; off_sou: INTEGER; no: INTEGER);
CODE cod.bmg cod.bmg_bblt END bitmove;

TYPE CLP_PARM = RECORD xa,ya,xc,yc: INTEGER END;

PROCEDURE clp(VAR clp_parm: CLP_PARM; w,h:INTEGER): BOOLEAN;
CODE cod.bmg cod.bmg_clip END clp;

PROCEDURE dln(mode: INTEGER; VAL dsc: bmg.BMD; x,y,x1,y1: INTEGER);
CODE cod.bmg cod.bmg_line END dln;

--------------------------- S C A L E --------------------------
                           -----------
VAR lineW,sW: INTEGER;

PROCEDURE min(x,y: INTEGER): INTEGER;
BEGIN
  IF x>y THEN RETURN y ELSE RETURN x END
END min;

PROCEDURE scale_greed;
  VAR i: INTEGER;
   tool: bmg.TOOL;
BEGIN
  WITH tool DO
    mode:=bmg.rep;   back :={};
    mask:={0..3};    color:={};
    zX:=map.x;      clip.x:=0;   clip.w:=map.w;
    zY:=map.y;      clip.y:=0;   clip.h:=map.h
  END;
  FOR i:=0 TO char.w DO
    bmg.line(scr.bmd,tool,i*scalx,0,i*scalx,char.h*scaly);
  END;
  FOR i:=0 TO char.h DO
    bmg.line(scr.bmd,tool,0,i*scaly,char.w*scalx,i*scaly);
  END;
END scale_greed;

PROCEDURE set_scale(w,h,m,n: INTEGER);
  VAR j: BITSET;
      s: INTEGER;
     b1: bmg.BLOCK;
    i,k: INTEGER;
BEGIN
  char.x:=0; char.w:=w;
  char.y:=0; char.h:=h;
  bmg.cross(char,char,full);
  IF (char.w<0) OR (char.h<0) THEN RETURN END;
  work:=char;
  s:=min((scr.wblk.w-1) DIV (char.w*m),(scr.wblk.h-1) DIV (char.h*n));
  IF s*m>16 THEN s:=16 DIV m END;
  IF s<1 THEN s:=1 END;
  scalx:=s*m;
  scaly:=s*n;
  IF scalx>2 THEN EXCL(DOT[0],0) END;
  sW:=scalx*4;
  low.zero(SCL);
  FOR i:=0 TO 15 DO
    k:=0;  j:=BITSET(i);
    WHILE j#{} DO
      IF j*{0}#{} THEN bitrepl(pSCL+i*2,k,pDOT,0,scalx) END;
      INC(k,scalx); j:=(j-{0})>>1
    END
  END;
  map.w:=scalx*w+1; map.x:=scr.wblk.x+(scr.wblk.w-map.w) DIV 2;
  map.h:=scaly*h+1; map.y:=scr.wblk.y+(scr.wblk.h-map.h) DIV 2;
  bmg.cross(map,map,scr.wblk);
  scale_greed
END set_scale;

PROCEDURE work_area(x0,y0,x1,y1: INTEGER);
  VAR b0: bmg.BLOCK;
    tool: bmg.TOOL;

  PROCEDURE frame(x,y,w,h: INTEGER; p: WORD);
    VAR i,x0,y0,x1,y1: INTEGER;
  BEGIN
    x0:=x*scalx;  x1:=(x+w)*scalx;
    y0:=y*scaly;  y1:=(y+h)*scaly;
    IF x0=x1 THEN bmg.dline(scr.bmd,tool,x0,y0,x0,y1,p); RETURN END;
    IF y0=y1 THEN bmg.dline(scr.bmd,tool,x0,y0,x1,y1,p); RETURN END;
    bmg.dline(scr.bmd,tool,x0+1,y0,x1,y0,p);
    bmg.dline(scr.bmd,tool,x1,y0+1,x1,y1,p);
    bmg.dline(scr.bmd,tool,x1-1,y1,x0,y1,p);
    bmg.dline(scr.bmd,tool,x0,y1-1,x0,y0,p)
  END frame;

BEGIN
  b0.x:=x0;  b0.w:=ABS(x0-x1)+1;
  b0.y:=y0;  b0.h:=ABS(y0-y1)+1;
  bmg.cross(b0,char,b0);
  IF (b0.w<=0) OR (b0.h<=0) THEN RETURN END;
  WITH tool DO
    mask:={3};       back:={ };
    mode:=bmg.xor;   color:=mask;
    zX:=scr.wblk.x;  clip.x:=0;   clip.w:=scr.wblk.w;
    zY:=scr.wblk.y;  clip.y:=0;   clip.h:=scr.wblk.h
  END;
  frame(work.x,work.y,work.w,work.h,055555555h);
  work:=b0;
  frame(work.x,work.y,work.w,work.h,055555555h);
END work_area;

PROCEDURE packline(pat: ADDRESS; pos,len: INTEGER);
  VAR l0,l1: INTEGER;
BEGIN
  lineW:=0;
  WHILE len>0 DO
    IND:=0;
    IF len<4 THEN  l0:=len; l1:=len*scalx  ELSE  l0:=4; l1:=sW  END;
    bitrepl(pIND,0,pat,pos,l0);
    bitrepl(pBUF,lineW,pSCL+IND*2,0,sW);
    INC(lineW,l1);
    INC(pos,4);
    DEC(len,4)
  END
END packline;

PROCEDURE showline(xb,yb: INTEGER);
  VAR i,k,len,sta: INTEGER;
          lay,end: INTEGER;
BEGIN
  xb:=xb*scalx;    ASSERT((xb>=0) & (xb<map.x+map.w));
  yb:=yb*scaly+1;
  k:=xb+lineW-1;
  end:=scaly-1;
  WITH map DO
    IF k >=w THEN k :=w-1 END;
    IF (yb>=h) OR (yb<0) THEN RETURN END;
    IF yb+scaly>=h THEN end:=h-yb-1 END;
  END;
  len:=k-xb+1;
  IF len<=0 THEN RETURN END;
  xb:=xb+map.x;
  IF scaly>2 THEN sta:=1 ELSE sta:=0 END;
  yb:=(scr.bmd^.H-1-(yb+map.y))*scr.bmd^.WPL+scr.bmd^.layers[1];
  FOR i:=sta TO end DO
    bitrepl(yb,xb,pBUF,0,len); yb:=yb-scr.bmd^.WPL
  END
END showline;

PROCEDURE refresh;
  VAR yo: INTEGER;
     adr: ADDRESS;
BEGIN
  adr:=main^.BASE+(main^.H-1)*main^.WPL;
  FOR yo:=0 TO scr.fH-1 DO
    packline(adr,0,scr.fW);  showline(0,yo);  DEC(adr,main^.WPL)
  END
END refresh;

PROCEDURE refreshwork;
  VAR yo: INTEGER;
     adr: ADDRESS;
BEGIN
  WITH work DO
    adr:=main^.BASE+(main^.H-1-y)*main^.WPL;
    FOR yo:=y TO y+h-1 DO
      packline(adr,x,w);  showline(x,yo);  DEC(adr,main^.WPL)
    END
  END
END refreshwork;

PROCEDURE refline(x,y,w: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  IF x+w>=128 THEN w:=128-x END;
  packline(main^.BASE+(main^.H-1-y)*main^.WPL,x,w);
  showline(x,y)
END refline;

PROCEDURE mirror;
  VAR l0,l1,patt: ADDRESS;
     yo,x,w,offs: INTEGER;
BEGIN
  IF mode THEN
    patt:=main^.BASE+(main^.H-1-work.y)*main^.WPL;
    FOR yo:=work.y TO work.y+work.h-1 DO
      w:=work.w; x:=work.x; offs:=w;
      bitrepl(pBUF,0,patt,x,w);
      WHILE w>0 DO
        IND:=0;
        IF w>=8 THEN
          DEC(offs,8);  bitrepl(pIND,0,pBUF,offs ,8);
                        bitrepl(patt,x,pMIR,IND*8,8)
        ELSE
          DEC(offs,w);  bitrepl(pIND, 8-w, pBUF, offs ,w);
                        bitrepl(patt,  x , pMIR, IND*8,w)
        END;
        INC(x,8); DEC(w,8)
      END;
      DEC(patt,main^.WPL);
    END
  ELSE
    WITH work DO
      l0:=main^.BASE+(main^.H-1-y)*main^.WPL;
      l1:=main^.BASE+(main^.H-h-y)*main^.WPL;
      FOR yo:=0 TO h DIV 2 - 1 DO
        bitrepl(pBUF,0, l0 ,x,w);
        bitrepl( l0 ,x, l1 ,x,w);
        bitrepl( l1 ,x,pBUF,0,w);
        INC(l1,main^.WPL);
        DEC(l0,main^.WPL)
      END
    END
  END;
  refreshwork
END mirror;

PROCEDURE swap(x,y0,y1: INTEGER);
  VAR w: INTEGER;  l0,l1: ADDRESS;
BEGIN
  IF (y1<work.y) OR (y1>=work.y+work.h) OR
     (y0<work.y) OR (y0>=work.y+work.h) OR
     (x <work.x) OR (x >=work.x+work.w) OR (y0=y1) THEN RETURN END;
  w:=work.w+work.x-x;
  WITH main^ DO
    l0:=BASE+(H-1-y0)*WPL;
    l1:=BASE+(H-1-y1)*WPL;
    bitrepl(pBUF,0, l0 ,x,w);
    bitrepl( l0 ,x, l1 ,x,w);
    bitrepl( l1 ,x,pBUF,0,w);
  END;
  refline(x,y0,w);
  refline(x,y1,w)
END swap;

PROCEDURE dupl(x,yo,yn: INTEGER);
  VAR w,y0,y1: INTEGER;
BEGIN
  IF (yn<work.y) OR (yn>=work.y+work.h) OR
     (x <work.x) OR (x >=work.x+work.w) THEN RETURN END;
  w:=work.w+work.x-x;
  WITH main^ DO
    y0:=BASE+(H-1-yo)*WPL;
    y1:=BASE+(H-1-yn)*WPL;
  END;
  bitrepl(y1,x,y0,x,w);
  refline(x,yn,w)
END dupl;

PROCEDURE insdeldot(x,y: INTEGER);
  VAR w: INTEGER;
     y0: ADDRESS;
BEGIN
  IF (y<work.y) OR (y>=work.y+work.h) OR
     (x<work.x) OR (x>=work.x+work.w) THEN RETURN END;
  w:=work.w+work.x-x-1;
  WITH main^ DO
    y0:=BASE+(H-1-y)*WPL;
    IF w=0 THEN bitrepl(y0,x,pLN0,0,1); refline(x,y,1); RETURN END;
    IF mode THEN
      bitrepl( y0 ,x+1, y0 ,x,w);
      bitrepl( y0 , x ,pLN0,0,1)
    ELSE
      bitrepl( y0 , x ,y0 ,x+1,w);
      bitrepl( y0 ,x+w,pLN0, 0 ,1)
    END
  END;
  refline(x,y,w+1)
END insdeldot;

PROCEDURE insdelline(x,y: INTEGER);
  VAR  y0: ADDRESS;
     tool: bmg.TOOL;
cx,cy,w,i: INTEGER;

BEGIN
  IF (y<work.y) OR (y>=work.y+work.h) OR
     (x<work.x) OR (x>=work.x+work.w) THEN RETURN END;
  w:=work.w+work.x-x;
  cx:=(crs.x-map.x) DIV scalx;
  cy:=(crs.y-map.y) DIV scaly;
  WITH tool.clip DO
    x:=map.x+cx*scalx;  w:=map.x+map.w-x;
    y:=map.y;           h:=(cy+1)*scaly
  END;
  bmg.cross(tool.clip,tool.clip,scr.wblk);
  tool.zX:=tool.clip.x;  tool.clip.x:=0;  tool.mask:={1};
  tool.zY:=tool.clip.y;  tool.clip.y:=0;  tool.mode:=bmg.rep;
  tool.color:={};
  WITH main^ DO
    IF mode THEN
      y0:=BASE+(H-1)*WPL;
      FOR i:=1 TO y DO
        bitrepl(y0,x,y0-WPL,x,w); DEC(y0,WPL)
      END;
      bitrepl(y0,x,pLN0,0,w);
      bmg.scroll(scr.bmd,tool,0,+scaly);
      refline(cx,cy,work.w-cx)
    ELSE
      y0:=BASE+(H-1-y)*WPL;
      FOR i:=1 TO y DO
        bitrepl(y0,x,y0+WPL,x,w); INC(y0,WPL)
      END;
      bitrepl(y0,x,pLN0,0,w);
      bmg.scroll(scr.bmd,tool,0,-scaly);
      refline(cx,0,work.w-cx)
    END
  END;
END insdelline;

PROCEDURE dot(x,y: INTEGER);
  VAR adr: ADDRESS;
        v: INTEGER;
BEGIN
  IF mode THEN v:=-1 ELSE v:=0 END;
  IF (x>=char.x+char.w) OR (y>=char.y+char.h) THEN RETURN END;
  adr:=main^.layers[0]+(main^.H-1-y)*main^.WPL;
  bitrepl(adr,x,ADR(v),0,1);
  packline(adr,x,1);
  showline(x,y)
END dot;

PROCEDURE invers;
  VAR i: INTEGER;
      a: ADDRESS;
BEGIN
  a:=main^.layers[0];
  FOR i:=0 TO main^.H-1 DO
    bitmove(bmg.xor,a,0,pLNF,0,main^.W); INC(a,main^.WPL)
  END;
  refresh
END invers;
---------------------------- F I L L ---------------------------

CONST
  DEAPTH = 256;         BYTE0 = {0..7 };
  MASK   = {0..5};      BYTE1 = {8..15};

VAR head,tail,deap: INTEGER;
            buffer: ARRAY [0..DEAPTH-1] OF WORD;

PROCEDURE push(xp,yp: INTEGER);
BEGIN
  buffer[head]:=INTEGER((BITSET(xp)*BYTE0)<<8)+yp;
  head:=INTEGER(BITSET(head+1)*MASK);
  INC(deap)
END push;

PROCEDURE pop(VAR xp,yp: INTEGER);
  VAR n: INTEGER;
BEGIN
  n:=buffer[tail];
  tail:=INTEGER(BITSET(tail+1)*MASK);
  xp:=INTEGER((BITSET(n)*BYTE1)>>8);
  yp:=INTEGER((BITSET(n)*BYTE0));
  DEC(deap)
END pop;

PROCEDURE init_stack;
BEGIN
  low.zero(buffer); deap:=0; head:=0; tail:=0;
END init_stack;

(* заливка 4-связных областей *)

PROCEDURE fill(X,Y: INTEGER);
    VAR xe,ye,xb,yb: INTEGER; old: INTEGER;
        x,y,t,r,l  : INTEGER; new: INTEGER;

  PROCEDURE pix(x,y: INTEGER; a: ADDRESS): INTEGER;
    VAR c: INTEGER;
  BEGIN
    c:=0;  bitrepl(ADR(c),0,a,x,1);  RETURN c
  END pix;

  PROCEDURE check_line(x,y,xr: INTEGER; a: ADDRESS);
  BEGIN
    IF (y<yb) OR (y>ye) THEN RETURN END;
    WHILE x<=xr DO
      IF pix(x,y,a)=old THEN
        REPEAT x:=x+1 UNTIL (pix(x,y,a)#old) OR (x>xr);
        push(x-1,y);
      END;
      WHILE (pix(x,y,a)#old) & (x<=xr) DO INC(x) END;
    END
  END check_line;

VAR a,p: ADDRESS;

BEGIN
  a:=(main^.H-1-Y)*main^.WPL+main^.BASE;
  new:=INTEGER(mode);
  old:=pix(X,Y,a);
  IF (Y<work.y) OR (Y>=work.y+work.h) OR
     (X<work.x) OR (X>=work.x+work.w) OR (new=old) THEN RETURN END;
  IF mode THEN p:=pLNF ELSE p:=pLN0 END;
  xb:=work.x;  xe:=work.x+work.w-1;
  yb:=work.y;  ye:=work.y+work.h-1;
  init_stack;
  push(X,Y);
  WHILE deap>0 DO
    pop(x,y);
    a:=(main^.H-1-y)*main^.WPL+main^.BASE;
    t:=x; INC(x);
    WHILE (pix(x,y,a)=old) & (x>=xb) & (x<=xe) DO INC(x) END;
    r:=x-1; x:=t; DEC(x);
    WHILE (pix(x,y,a)=old) & (x>=xb) & (x<=xe) DO DEC(x) END;
    l:=x+1;
    bitrepl(a,l,p,0,r-l+1);
    packline(a,l,r-l+1);
    showline(l,y);
    check_line(l,y+1,r,a-main^.WPL);
    check_line(l,y-1,r,a+main^.WPL)
  END
END fill;

------------------------ G R A P H I C S -----------------------
                        -----------------

PROCEDURE line (xb,yb,xe,ye: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
  t.mask:={0};   t.color:={0};
  t.clip:=work;  t.back :={ };
  IF mode THEN t.mode:=bmg.rep ELSE t.mode:=bmg.bic END;
  bmg.line(main,t,xb,yb,xe,ye);
  refreshwork
END line;

PROCEDURE circ(x,y,r: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
  t.mask:={0};   t.color:={0};
  t.clip:=work;  t.back :={ };
  IF mode THEN t.mode:=bmg.rep ELSE t.mode:=bmg.bic END;
  bmg.circle(main,t,x,y,r);
  refreshwork
END circ;

PROCEDURE arc (x0,y0,x1,y1,x2,y2: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
  t.mask:={0};   t.color:={0};
  t.clip:=work;  t.back :={ };
  IF mode THEN t.mode:=bmg.rep ELSE t.mode:=bmg.bic END;
  bmg.arc3(main,t,x0,y0,x1,y1,x2,y2);
  refreshwork
END arc;

BEGIN
  mode:=TRUE; init_misc
END fedImage.
