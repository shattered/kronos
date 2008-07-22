IMPLEMENTATION MODULE fedBase; (* nick 13-Dec-90. (c) KRONOS *)

FROM SYSTEM  IMPORT ADR, ADDRESS,WORD;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;          IMPORT  low: lowLevel;
IMPORT  dfn: defFont;           IMPORT  fnt: Fonts;
IMPORT  scr: Screen;            IMPORT  bmg: BMG;
IMPORT  pup: pmPUP;             IMPORT  cpd: CPD;
IMPORT  mem: Heap;              IMPORT  bio: BIO;
IMPORT  env: tskEnv;            IMPORT  crs: pmCrs;
IMPORT tty: Terminal;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

PROCEDURE move(         s,d,s: INTEGER); CODE cod.move             END move;
PROCEDURE gbblt(m,d,do,s,so,n: INTEGER); CODE cod.bmg cod.bmg_bblt END gbblt;
PROCEDURE bmv(    d,do,s,so,n: INTEGER); CODE cod.bmv              END bmv;

----------------------- V a r i a b l e s ----------------------
                       -------------------

VAR ichar: ARRAY CHAR OF ADDRESS; (* array of pointers to images *)
    wchar: ARRAY CHAR OF INTEGER; (* array of image's leghts     *)

----------------------- F u r n i t u r e ----------------------
                       -------------------

PROCEDURE block0(fill,pressed: BOOLEAN; x0,y0,x1,y1: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
 t:=full;
 IF fill THEN
   t.color:=pup.normal; bmg.rect(bmd,t,x0+1,y0+1,x1-1,y1-1)
 END;
 IF pressed THEN t.color:=pup.shadow ELSE t.color:=pup.bright END;
 bmg.line(bmd,t,x0,y0+1,x0,y1);
 bmg.line(bmd,t,x0,y1  ,x1,y1);
 IF pressed THEN t.color:=pup.bright ELSE t.color:=pup.shadow END;
 bmg.line(bmd,t,x1,y0,x1,y1-1);
 bmg.line(bmd,t,x1,y0,x0,y0  );
END block0;

PROCEDURE block(b: bmg.BLOCK; fill,pressed: BOOLEAN);
BEGIN
  block0(fill,pressed,b.x,b.y,b.x+b.w-1,b.y+b.h-1)
END block;

PROCEDURE print(b: bmg.BLOCK; x: INTEGER; f: ARRAY OF CHAR; SEQ a: WORD);
  VAR t: bmg.TOOL;
BEGIN
  INC(b.x); INC(b.y); INC(b.w,2); INC(b.w,2);
  t:=full;            t.color:=pup.black;
  t.clip:=b;          t.back :=pup.normal;
tty.print("(%d %d) ", b.x+x,b.y+1+pup.font^.bline);
tty.print(f, a);
tty.print("\n");
  bmg.print(bmd,t,b.x+x,b.y+1+pup.font^.bline,pup.font,f,a)
END print;

PROCEDURE clear;
  VAR t: bmg.TOOL;
BEGIN
  t:=full;
  t.color:=pup.shadow;
  bmg.rect(bmd,t,wblk.x,wblk.y,wblk.x+wblk.w-1,wblk.y+wblk.h-1);
  t.color:=pup.black;
  bmg.frame(bmd,t,wblk.x,wblk.y,wblk.x+wblk.w-1,wblk.y+wblk.h-1)
END clear;

------------------ C u r s o r   S u p p o r t -----------------
                  -----------------------------

PROCEDURE _inrect(x,y,h,w: INTEGER): BOOLEAN;
CODE
  cod.li1 cod.sub cod.swap
  cod.li1 cod.sub cod.bmg
  cod.bmg_inrect
END _inrect;

PROCEDURE inrect(x,y,w,h: INTEGER): BOOLEAN;
BEGIN
  RETURN _inrect(x,y,h,w)
END inrect;

PROCEDURE inblock(VAL b: bmg.BLOCK): BOOLEAN;
BEGIN
  RETURN _inrect(crs.x-b.x,crs.y-b.y,b.h,b.w)
END inblock;

------------------------- New & Dispose ------------------------
                         ---------------

PROCEDURE fnt_error; BEGIN done:=FALSE; error:=fnt.error END fnt_error;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error END mem_error;

-------------------------- Characters --------------------------
                          ------------

PROCEDURE disposechar(c: CHAR);
BEGIN
  ASSERT( (wchar[c]=0) & (ichar[c]=NIL) OR (wchar[c]>0) & (ichar[c]#NIL) );
  IF wchar[c]=0 THEN RETURN END;
  mem.deallocate(ichar[c],wchar[c]);
  efnt^.cellY^[c]:=0c;  wchar[c]:=0;
  efnt^.cellW^[c]:=0c;  ichar[c]:=NIL;
  efnt^.cellH^[c]:=0c
END disposechar;

PROCEDURE newchar(c: CHAR; x,y,w,h: INTEGER);
BEGIN
  disposechar(c);
  wchar[c]:=(w*h+31) DIV 32;
  mem.allocate(ichar[c],wchar[c]);
  IF NOT mem.done THEN mem_error; wchar[c]:=0; ichar[c]:=NIL; RETURN END;
  low._zero(ichar[c],wchar[c]);
  efnt^.cellX^[c]:=CHAR(x);
  efnt^.cellY^[c]:=CHAR(y);
  efnt^.cellW^[c]:=CHAR(w);
  efnt^.cellH^[c]:=CHAR(h)
END newchar;

----------------------------- Fonts ----------------------------
                             -------

PROCEDURE newfont(w,h: INTEGER; f,l: CHAR; s: BITSET);
BEGIN
  fnt.new(efnt,w,h,f,l,s+fnt.packed);  fW:=w;  fH:=h
END newfont;

PROCEDURE disposefont;
  VAR c: CHAR;
BEGIN
  FOR c:=0c TO 377c DO disposechar(c) END;
  IF efnt=NIL THEN RETURN END;
  WITH efnt^ DO
    IF fnt.prop*state#{} THEN DISPOSE(propW) END;
    IF BASE#NIL THEN mem.deallocate(BASE,size) END;
    DISPOSE(cellH);   DISPOSE(cellW);
    DISPOSE(cellY);   DISPOSE(bases);
    magic:=0
  END;
  DISPOSE(efnt)
END disposefont;

----------------------- Read & Write font ----------------------
                       -------------------

PROCEDURE readfont(file_name: ARRAY OF CHAR);
  VAR c: CHAR; w,cx,px: INTEGER; p: BOOLEAN;
BEGIN
  done:=TRUE;
  fnt.read(efnt,file_name);  IF NOT fnt.done THEN fnt_error; RETURN END;
  fnt.pack(efnt);            IF NOT fnt.done THEN fnt_error; RETURN END;
  low.zero(wchar);  low.fill(ichar,NIL);
  WITH efnt^ DO
    p:=state*fnt.prop#{};
    FOR c:=fchar TO lchar DO
      IF bases^[c]>=0 THEN
        IF p THEN
          cx:=ORD(cellX^[c]);
          px:=ORD(propX^[c]);
          IF cx<px THEN px:=px-cx; cx:=0 ELSE cx:=cx-px; px:=0 END;
          cellX^[c]:=CHAR(cx);
          propX^[c]:=CHAR(px);
        END;
        w:=ORD(cellW^[c])*ORD(cellH^[c]);
        mem.allocate(ichar[c],(w+31) DIV 32);
        IF NOT mem.done THEN mem_error; disposefont; RETURN END;
        wchar[c]:=(w+31) DIV 32;
        low._zero(ichar[c],wchar[c]);
        bmv(ichar[c],0,BASE,bases^[c],w)
      END
    END;
    mem.deallocate(BASE,size);
    BASE:=NIL;
    size:=0
  END;
  fW:=efnt^.W;
  fH:=efnt^.H
END readfont;

PROCEDURE writefont(file_name: ARRAY OF CHAR);

  PROCEDURE eq(c0,c1: CHAR): BOOLEAN;
    VAR a,b: ADDRESS;
        i,w: INTEGER;
  BEGIN
    a:=ichar[c0];
    b:=ichar[c1];
    i:=0;
    w:=(ORD(efnt^.cellW^[c0])*ORD(efnt^.cellH^[c0])+31) DIV 32;
    WHILE (i<w) & (a^=b^) DO INC(i); INC(a); INC(b) END;
    RETURN i=w
  END eq;

  VAR offs: ARRAY CHAR OF CHAR;
     ch,cc: CHAR;
    w,sz,o: INTEGER;
BEGIN
  done:=TRUE;
  sz:=0;
  low.fill(offs,0);
  WITH efnt^ DO
    FOR ch:=fchar TO lchar DO
      IF ichar[ch]#NIL THEN
        cc:=fchar;
        LOOP
          IF cc=ch THEN
            offs[ch]:=ch; sz:=sz+ORD(cellW^[ch])*ORD(cellH^[ch]); EXIT
          END;
          IF (ichar[cc]#NIL) &
             (cellW^[ch]=cellW^[cc]) &
             (cellH^[ch]=cellH^[cc]) & eq(ch,cc)
          THEN
            offs[ch]:=cc; EXIT
          END;
          INC(cc)
        END
      END
    END;
    mem.allocate(BASE,(sz+31) DIV 32);
    IF NOT mem.done THEN mem_error; RETURN END;
    size:=(sz+31) DIV 32;
    low.fill(bases^,-1);
    o:=0;
    FOR ch:=fchar TO lchar DO
      IF ichar[ch]#NIL THEN
        IF offs[ch]=ch THEN
          sz:=ORD(cellW^[ch])*ORD(cellH^[ch]);
          bmv(BASE,o,ichar[ch],0,sz);
          bases^[ch]:=o;
          o:=o+sz
        ELSE
          bases^[ch]:=bases^[offs[ch]]
        END
      END
    END;
    fnt.write(efnt,file_name);
    IF NOT fnt.done THEN fnt_error END;
    mem.deallocate(BASE,size);
    BASE:=NIL;
    size:=0
  END
END writefont;

----------------------------------------------------------------

PROCEDURE readchar(main: bmg.BITMAP; ch: CHAR);
  VAR i,Y,W,H: INTEGER;  coff,boff: INTEGER;
          bpl: INTEGER;  base,char: ADDRESS;

BEGIN
  base:=main^.BASE;
  bpl :=main^.WPL*BITS(WORD);
  low._zero(base,main^.H*main^.WPL);
  IF ichar[ch]=NIL THEN RETURN END;
  Y:=ORD(efnt^.cellY^[ch]);  char:=ichar[ch];
  W:=ORD(efnt^.cellW^[ch]);  coff:=0;
  H:=ORD(efnt^.cellH^[ch]);
  boff:=(main^.H-Y-H)*bpl + ORD(efnt^.cellX^[ch]);
  FOR i:=0 TO H-1 DO
    bmv(base,boff,char,coff,W);  coff:=coff+W;  boff:=boff+bpl
  END
END readchar;

PROCEDURE savechar(bmd: bmg.BITMAP; char: CHAR);
  CONST bpw = BITS(WORD);
  VAR   wpl: INTEGER;

  PROCEDURE empty(ln: ADDRESS): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO wpl-1 DO
      IF ln^#0 THEN RETURN FALSE END; INC(ln)
    END;
    RETURN TRUE
  END empty;

  VAR i,j,W,H: INTEGER;
        y0,y1: INTEGER;
        x0,x1: INTEGER;
        m0,m1: BITSET;
      ln,base: ADDRESS;
          sum: ARRAY [0..7] OF BITSET;
BEGIN
  wpl:=bmd^.WPL;                  y0:=-1;
  ln :=bmd^.BASE+(bmd^.H-1)*wpl;  y1:=-1;
  low.zero(sum);
  FOR i:=0 TO fH-1 DO
    IF NOT empty(ln) THEN
      gbblt(1,ADR(sum),0,ln,0,fW);
      IF y0<0 THEN y0:=i END;
      IF y1<i THEN y1:=i END
    END;
    DEC(ln,wpl)
  END;
  IF (y0<0) OR (y1<0) THEN disposechar(char); RETURN END;
  i:=0;
  WHILE sum[i]={} DO INC(i) END;
  x0:=i*bpw;
  m0:={0};
  m1:=sum[i];
  WHILE m0*m1={} DO m0:=m0<<1; INC(x0) END;
  i:=fW DIV bpw;
  WHILE sum[i]={} DO DEC(i) END;
  x1:=(i+1)*bpw-1;
  m0:={bpw-1};
  m1:=sum[i];
  WHILE m0*m1={} DO m0:=m0>>1; DEC(x1) END;
  ln:=bmd^.BASE+(bmd^.H-y1-1)*wpl;
  x1:=x1-x0+1;
  y1:=y1-y0+1;
  newchar(char,x0,y0,x1,y1);
  IF NOT done THEN RETURN END;
  j:=0;
  FOR i:=0 TO y1-1 DO
    bmv(ichar[char],j,ln,x0,x1); INC(j,x1); INC(ln,wpl)
  END
END savechar;

VAR line00: ADDRESS;  bump00: ARRAY [0..7] OF WORD;
    lineFF: ADDRESS;  bumpFF: ARRAY [0..7] OF WORD;

PROCEDURE newW(W: INTEGER; cut: INTEGER);
  VAR c: CHAR;           a,ptr: ADDRESS;
    w,h: INTEGER;        n,o,s: INTEGER;
BEGIN
  IF W>=efnt^.W THEN efnt^.W:=W; RETURN END;
  FOR c:=0c TO 377c DO
    w:=ORD(efnt^.cellW^[c]);
    h:=ORD(efnt^.cellH^[c]);
    IF (ichar[c]#NIL) & (h>1) & (w>W) THEN
      s:=(W*h+31) DIV 32;
      ptr:=ichar[c];
      IF wchar[c]#s THEN
        mem.allocate(a,s);  IF NOT mem.done THEN mem_error; RETURN END;
        low._zero(a,s);     o:=0;  n:=0;
        REPEAT
          bmv(a,n,ptr,o,W); n:=n+W; o:=o+w; h:=h-1
        UNTIL h=0;
        mem.deallocate(ichar[c],wchar[c]);  ichar[c]:=a;  wchar[c]:=s
      ELSE
        n:=W;  o:=w;  h:=h-1;
        REPEAT
          bmv(ptr,n,ptr,o,W); n:=n+W; o:=o+w; h:=h-1
        UNTIL h=0;
      END;
      efnt^.cellW^[c]:=CHAR(W);
      IF (efnt^.propW#NIL) & (ORD(efnt^.propW^[c])>W) THEN
        efnt^.propW^[c]:=CHAR(W)
      END
    END
  END;
  efnt^.W:=W; fW:=W
END newW;

PROCEDURE newH(nh: INTEGER; ch: INTEGER);
  VAR c: CHAR;  y,w,h,o,d: INTEGER;

  PROCEDURE change;
    VAR a: ADDRESS;
      s,n: INTEGER;
  BEGIN
    IF h<1 THEN disposechar(c); RETURN END;
    n:=h*w;  o:=o*w;  s:=(n+31) DIV 32;
    IF wchar[c]#s THEN
      mem.allocate(a,s);
      IF NOT mem.done THEN mem_error; RETURN END;
      low._zero(a,s);
      bmv(a,0,ichar[c],o,n);
      mem.deallocate(ichar[c],wchar[c]);
      ichar[c]:=a;
      wchar[c]:=s
    ELSE
      IF o#0 THEN bmv(ichar[c],0,ichar[c],o,n) END
    END;
    efnt^.cellH^[c]:=CHAR(h)
  END change;

BEGIN
  IF nh > fH THEN
    d:=nh-fH;
    IF ch=0 THEN efnt^.H:=nh; fH:=nh; RETURN END;  d:=d DIV ch;
    IF  d=0 THEN efnt^.H:=nh; fH:=nh; RETURN END;
    FOR c:=0c TO 377c DO
      IF ichar[c]#NIL THEN
        efnt^.cellY^[c]:=CHAR(ORD(efnt^.cellY^[c])+d)
      END
    END;
    efnt^.H:=nh; fH:=nh; RETURN
  END;
  d:=fH-nh;
  FOR c:=0c TO 377c DO
    IF ichar[c]#NIL THEN
      w:=ORD(efnt^.cellW^[c]);
      h:=ORD(efnt^.cellH^[c]);  ASSERT(h>0);
      y:=ORD(efnt^.cellY^[c]);
      CASE ch OF
        |0:  IF y+h>nh THEN
               o:=y+h-nh; h:=nh-y;  change;
               IF NOT done THEN RETURN END;
             END;
        |1:  IF   d>y THEN
               h:=h+y-d; o:=0;  change;
               IF NOT done THEN RETURN END;
               efnt^.cellY^[c]:=0c
             ELSE
               efnt^.cellY^[c]:=CHAR(y-d)
             END;
        |2:  IF y+h>nh THEN
               IF h>nh THEN o:=(h-nh) DIV 2; h:=nh;
                 change;
                 IF NOT done THEN RETURN END;
                 efnt^.cellY^[c]:=0c
               ELSE
                 efnt^.cellY^[c]:=CHAR(nh-h)
               END;
             END
      END
    END
  END;
  efnt^.H:=nh; fH:=nh
END newH;

PROCEDURE newsize(w,h,cw,ch: INTEGER);
BEGIN
  done:=(w>0) & (w<=256) & (h>0) & (h<=256);
  IF NOT done THEN error:=err.bad_parm; RETURN END;
  IF (cw>=0) & (w#efnt^.W) THEN newW(w,cw) END;
  IF (ch>=0) & (h#efnt^.H) THEN newH(h,ch) END
END newsize;

PROCEDURE xwrite(T: bmg.TOOL; x,y: INTEGER; ch: CHAR): INTEGER;

  PROCEDURE pchar(p0,p1,p2,p3,cw,cW,maxw,bpl,C: INTEGER); (*$<$W+*)
     VAR lay: ADDRESS;  coff,ln,k,mode: INTEGER;
       m,f,b: BITSET;             bump: ARRAY [0..7] OF INTEGER;
  BEGIN
    lay:=ADR(bmd^.layers);
    coff:=0;
    mode:=T.mode; m:=T.mask*bmd^.mask;  b:=T.back;  f:=T.color<<1;
    REPEAT
     IF m*{0}#{} THEN
       m:=m-{0};
       ln:=p0;
       CASE INTEGER(f*{1})+INTEGER(b*{0}) OF
       |0:
       |1: WHILE ln<p1 DO gbblt(mode,lay^,ln,lineFF,0,maxw); ln:=ln+bpl END;
           IF (cw>0) & (ln#p2) THEN
             k:=coff;
             REPEAT
               move(ADR(bump),lineFF,4);
               gbblt(bmg.xor,ADR(bump),0,C,k,cw);    k :=k+cW;
               gbblt(mode,lay^,ln,ADR(bump),0,maxw); ln:=ln+bpl
             UNTIL ln=p2
           END;
           WHILE ln#p3 DO gbblt(mode,lay^,ln,lineFF,0,maxw); ln:=ln+bpl END
       |2: IF (cw>0) & (p1#p2) THEN
             ln:=p1;    k:=coff;
             REPEAT gbblt(mode,lay^,ln,C,k,cw); k:=k+cW; ln:=ln+bpl
             UNTIL ln=p2
           END;
       |3: REPEAT gbblt(mode,lay^,ln,lineFF,0,maxw); ln:=ln+bpl UNTIL ln=p3
       END
     END;
     INC(lay); b:=b>>1; f:=f>>1; m:=m>>1
   UNTIL m={};
 END pchar;                                                    (*$>*)

 PROCEDURE rchar(p0,p1,p2,p3,cw,cW,maxw,bpl,C: INTEGER); (*$<$W+*)
    VAR lay: ADDRESS;     ln,k,coff: INTEGER;
      m,f,b: BITSET;           bump: ARRAY [0..7] OF INTEGER;
 BEGIN
   lay:=ADR(bmd^.layers);
   m:=T.mask*bmd^.mask;        b:=T.back;      f:=T.color<<1;
   coff:=0;
   REPEAT
     IF m*{0}#{} THEN
       m:=m-{0};
       ln:=p0;
       CASE INTEGER(f*{1})+INTEGER(b*{0}) OF
       |0: REPEAT bmv(lay^,ln,line00,0,maxw); ln:=ln+bpl UNTIL ln=p3
       |1: WHILE ln<p1 DO bmv(lay^,ln,lineFF,0,maxw); ln:=ln+bpl END;
           IF (cw>0) & (ln#p2) THEN
             k:=coff;
             REPEAT
               move(ADR(bump),lineFF,4);
               gbblt(bmg.xor,ADR(bump),0,C,k,cw);  k :=k+cW;
               bmv(lay^,ln,ADR(bump),0,maxw);      ln:=ln+bpl
             UNTIL ln=p2
           END;
           WHILE ln#p3 DO bmv(lay^,ln,lineFF,0,maxw); ln:=ln+bpl END
       |2: WHILE ln#p1 DO bmv(lay^,ln,line00,0,maxw); ln:=ln+bpl END;
           IF (cw>0) & (p1#p2) THEN
             ln:=p1;    k:=coff;
             REPEAT
               bmv(lay^,ln,C,k,cw);              k :=k+cW;
               bmv(lay^,ln+cw,line00,0,maxw-cw); ln:=ln+bpl
             UNTIL ln=p2
           END;
           WHILE ln#p3 DO bmv(lay^,ln,line00,0,maxw); ln:=ln+bpl END
       |3: REPEAT bmv(lay^,ln,lineFF,0,maxw); ln:=ln+bpl UNTIL ln=p3
       END
     END;
     INC(lay); b:=b>>1; f:=f>>1; m:=m>>1
   UNTIL m={};
 END rchar;                                                    (*$>*)

 VAR p0y,p3y: INTEGER;          P: BOOLEAN;
    bpl,maxw: INTEGER;          C: INTEGER;
   coff,xoff: INTEGER;         pw: dfn.BTPTR;
 i,j,bH,tH,w: INTEGER;
 p0,p1,p2,p3: INTEGER;
 cW,cw,cY,cH: INTEGER;

BEGIN
  pw:=efnt^.propW;  P:=(pw#NIL);  C:=ichar[ch];
  IF C=NIL THEN
    IF P THEN RETURN efnt^.W DIV 2+x ELSE RETURN efnt^.W+x END
  END;

  (* bH, tH  bottom, top over Height of Full Char Place *)

  IF y<0 THEN bH:=-y ELSE bH:=0 END;
  IF y+efnt^.H>T.clip.h THEN tH:=y+efnt^.H-T.clip.h ELSE tH:=0 END;
  bpl:=bmd^.WPL*BITS(WORD);

  (********  NOTE! Now y will be in LUC coordinate system! ******)

  xoff:=x+T.clip.x+T.zX + ORD(efnt^.cellX^[ch]) - ORD(efnt^.propX^[ch]);
  y   :=bmd^.H-1-(y+T.clip.y+T.zY);

  p0y:=(y-(efnt^.H-1)+tH);    p0:=xoff+p0y*bpl;
  p3y:=y+1-bH;                p3:=xoff+p3y*bpl;
  cY:=ORD(efnt^.cellY^[ch]);  cW:=ORD(efnt^.cellW^[ch]);
  cH:=ORD(efnt^.cellH^[ch]);  cw:=cW;
  w :=efnt^.W;
  IF P   THEN w:=ORD(pw^[ch]) END;
  IF w=0 THEN w:=w DIV 2 END;
  IF x<0 THEN i:=ABS(x);
    DEC(cw,i); DEC(w,i); INC(coff,i); INC(xoff,i); INC(p0,i); INC(p3,i); x:=0
  END;
  maxw:=w;
  IF cw>maxw    THEN maxw:=cw END;
  i:=y-(cY+cH-1);
  j:=y-(cY-1);
  IF (j>=p0y) & (i<=p3y) THEN
    IF i>p0y THEN p1:=i*bpl+xoff ELSE p1:=p0; coff:=coff+(p0y-i)*cW END;
    IF j<p3y THEN p2:=j*bpl+xoff ELSE p2:=p3  END
  ELSE
    p1:=p0; p2:=p0
  END;
  IF x+maxw>T.clip.w THEN maxw:=T.clip.w-x END;
  IF x+cw  >T.clip.w THEN cw  :=T.clip.w-x END;
  IF T.mode=bmg.rep THEN rchar(p0,p1,p2,p3,cw,cW,maxw,bpl,C)
  ELSE                   pchar(p0,p1,p2,p3,cw,cW,maxw,bpl,C)
  END;
  RETURN x+w
END xwrite;

----------------------------------------------------------------
CONST dp= ARRAY OF INTEGER
-- black  shadow normal light
  {0,0,0, 1,1,1, 2,2,2, 3,3,3,  2,2,2, 2,2,0, 0,2,0, 0,0,2
  ,3,3,3, 3,3,3, 3,3,3, 3,3,3,  3,2,2, 3,3,2, 2,3,2, 2,2,3};

PROCEDURE start_init;
  VAR x,y,i: INTEGER; s: BITSET;
BEGIN
  scr.loophole(scr.bitmap,bmd);
  IF NOT scr.done THEN HALT(scr.error) END;
  FOR i:=0 TO cpd.ready()-1 DO cpd.read(x,y,s) END;
  WITH scr.state^ DO
    ASSERT((W>= 480)&(H>= 360)&(bpp>=4),err.not_enough);
    ASSERT((W<=1024)&(H<=1024)&(bpp<=8),err.too_large);
  END;
  FOR i:=0 TO HIGH(dp) DIV 3 DO
    WITH scr.state^.pal[i] DO r:=dp[i*3]; g:=dp[i*3+1]; b:=dp[i*3+2] END
  END;
  scr.set_palette(scr.state^.pal,0,16);
  IF NOT scr.done THEN HALT(scr.error) END;
  WITH full DO
    zX:=0; clip.x:=0; clip.w:=scr.state^.W; mode:=bmg.rep; back :={ };
    zY:=0; clip.y:=0; clip.h:=scr.state^.H; mask:={0..3};  color:=mask
  END;
  pup.setplanes({0},{1},{0,1});
  bmg.erase(bmd)
END start_init;

PROCEDURE main_board;
  VAR s: INTEGER;
BEGIN
  s:=pup.sfont^.H+3;
  wblk:=full.clip;
  WITH wblk DO
    block0(TRUE,FALSE,x+1,y+1,x+w-2,y+h-2);
    block0(FALSE,TRUE,x+3,y+3,x+w-4,y+h-4);
  END;
  INC(wblk.x,4); DEC(wblk.w,8);
  INC(wblk.y,4); DEC(wblk.h,8);
  rblk:=wblk;  rblk.h:=s;
  sblk:=rblk;  sblk.y:=sblk.y+s;
  tblk:=rblk;  tblk.y:=wblk.y+wblk.h-s;
  mblk:=tblk;  mblk.y:=tblk.y-s;         tblk.w:=tblk.w-s;
  quit:=tblk;  quit.w:=s;                tblk.x:=tblk.x+s;

  wblk.h:=wblk.h-s*4;
  wblk.y:=wblk.y+s*2;

  block(quit,FALSE,FALSE);  pup.switch(quit,FALSE);
  block(tblk,FALSE,FALSE);  print(tblk,10,'Font Editor V00.32');
  block(mblk,FALSE,FALSE);
  block(rblk,FALSE,FALSE);  block(sblk,FALSE,FALSE);
  clear
END main_board;

BEGIN
  start_init;
  main_board;
  efnt:=NIL;
  fW:=32;
  fH:=32;
  low.fill(ichar,NIL);
  low.zero(wchar);
  line00:=ADR(bump00);  low.fill(bump00,00);
  lineFF:=ADR(bumpFF);  low.fill(bumpFF,-1);
  crs.setcolor(crs.cur,{3})
END fedBase.
