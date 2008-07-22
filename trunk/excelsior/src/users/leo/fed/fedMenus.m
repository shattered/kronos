IMPLEMENTATION MODULE fedMenus; (* nick 10-Jan-91. (c) KRONOS *)

FROM  SYSTEM IMPORT ADR, ADDRESS, WORD;
IMPORT  lex: Lexicon;           IMPORT  cod: defCodes;
IMPORT  bmg: BMG;               IMPORT  str: Strings;
IMPORT  cpd: CPD;               IMPORT  pup: pmPUP;
IMPORT  fnt: defFont;           IMPORT  crs: pmCrs;
IMPORT  img: fedImage;          IMPORT  scr: fedBase;
IMPORT  tim: Time;              IMPORT  key: Keyboard;

IMPORT  tty: Terminal;

CONST  xall = pup.xup+pup.xdw+pup.xrg+pup.xlf;

CONST  up = 0;  dw = 1;  sN = 15;  wB = 67;
       lf = 2;  rg = 3;  sW = 22;  sB = CHAR(255-sN);

TYPE rsdesc = RECORD
                schar: CHAR;
                first: CHAR;
                lfbut: bmg.BLOCK;
                rgbut: bmg.BLOCK;
                inner: bmg.BLOCK;
                base : bmg.BLOCK;
                but  : bmg.BLOCK;
                scrl : bmg.TOOL;
              END;

VAR  M,N: INTEGER;
    rsmv: ARRAY [Read..Save] OF rsdesc;

VAR     desc: bmg.BLOCK;               -- block for descmenu title
     tln,but: bmg.TOOL;                -- tool for main lines & text
    fontname: ARRAY [0..127] OF CHAR;  -- current font file name
   uprg,dwlf: bmg.BLOCK;               -- u/r & d/l buttons
cpw,cpx,Line: INTEGER;                 -- current p width & mode u/b/p line


VAR mOper: pup.MENU;   mFile: pup.MENU;  file: bmg.BLOCK;
    mDesc: pup.MENU;   mFont: pup.MENU;  font: bmg.BLOCK;
    direx: pup.DIREX;  mOpts: pup.MENU;  opts: bmg.BLOCK;
                       mScal: pup.MENU;  scal: bmg.BLOCK;

---------------------------- M i s c ---------------------------
                            ---------

PROCEDURE cursor(VAR s: BITSET);
BEGIN
  crs.toggle(TRUE);     crs.monitor;
  s:=cpd.state^.keys;   crs.toggle(FALSE)
END cursor;

CONST timeout = 50;

PROCEDURE oncemore(VAR s: BITSET; blk: bmg.BLOCK): BOOLEAN;
  VAR x,y,time: INTEGER;
BEGIN
  time:=tim.sys_time(tim.milisec)+timeout;
  REPEAT
    IF cpd.ready()>0 THEN  cursor(s);
      IF NOT scr.inblock(blk) OR (s*{0}={}) THEN RETURN FALSE END
    END;
  UNTIL (time-tim.sys_time(tim.milisec))<=0;
  RETURN TRUE
END oncemore;

PROCEDURE newvalue(m: pup.MENU; VAR val: INTEGER;
                            min,max,alt: INTEGER;
                            ask,err,fmt: ARRAY OF CHAR);
  VAR i,j: INTEGER;
      num: ARRAY [0..7] OF CHAR;
     done: BOOLEAN;
BEGIN
  LOOP
    pup.diabox(220,180,100,num,ask,val);
    IF num='' THEN EXIT END;
    j:=0;
    str.iscan(i,num,j,done);
    IF done & (min<=i) & (i<=max) THEN
      val:=i; pup.mprint(m,alt,fmt,val); EXIT
    END;
    pup.message(220,180,err)
  END
END newvalue;

PROCEDURE perr(e: INTEGER; fmt: ARRAY OF CHAR; SEQ arg: WORD);
BEGIN pup.perror(e,240,180,fmt,arg) END perr;

-------------------- Set Base & Under Lines --------------------
                    ------------------------

CONST pbline = 252645135;  puline = 872363007;
      ppline = 286331153;  pxline = 858993459;

PROCEDURE arrow(button: bmg.BLOCK; direction: INTEGER);

  PROCEDURE markbutton(b: bmg.BLOCK; arrow: CHAR);
    VAR t: bmg.TOOL; x,y: INTEGER;
  BEGIN
    x:=b.x+(b.w-pup.sfont^.W) DIV 2;
    y:=b.y+(b.h-pup.sfont^.H) DIV 2+pup.sfont^.bline;
    INC(b.x);  INC(b.w,2);  t:=scr.full;  t.color:=pup.black;
    INC(b.y);  INC(b.w,2);  t.clip:=b;    t.back :=pup.normal;
    bmg.writech(scr.bmd,t,x,y,pup.sfont,arrow)
  END markbutton;

BEGIN
  scr.block(button,FALSE,FALSE);
  CASE direction OF
    |rg: markbutton(button,pup.rtria)
    |lf: markbutton(button,pup.ltria)
    |up: markbutton(button,pup.utria)
    |dw: markbutton(button,pup.dtria)
  END
END arrow;

PROCEDURE printlines;
BEGIN
  scr.block(line,TRUE,FALSE);
  IF Line=0 THEN
    IF scr.efnt^.propW#NIL THEN
      scr.print(line,6,'propWidth');  arrow(dwlf,lf);  arrow(uprg,rg)
    ELSE
      scr.print(line,5,'Non proportional')
    END;
    RETURN
  END;
  IF Line=3 THEN
    IF scr.efnt^.propX#NIL THEN
      scr.print(line,6,'X offset');  arrow(dwlf,lf);  arrow(uprg,rg)
    ELSE
      scr.print(line,5,'Non proportional')
    END;
    RETURN
  END;
  scr.block(dwlf,FALSE,FALSE);
  IF    Line=1 THEN  scr.print(line,8,'Baseline')
  ELSIF Line=2 THEN  scr.print(line,8,'Underline')
  END;
  arrow(dwlf,dw);  arrow(uprg,up)
END printlines;

PROCEDURE showprop;
  VAR t: bmg.TOOL; w,p,x,y: INTEGER;
BEGIN
  t:=tln;
  t.clip:=img.map;     x:=t.clip.x;
  t.mode:=bmg.bic;     y:=t.clip.y;
  w:=x+cpw*img.scalx;  p:=ppline;  bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p);
  w:=x+cpx*img.scalx;  p:=pxline;  bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p);
  IF scr.efnt^.propW#NIL THEN
    cpx:=ORD(scr.efnt^.propX^[rsmv[Read].schar]);
    cpw:=ORD(scr.efnt^.propW^[rsmv[Read].schar])+cpx
  ELSE
    cpw:=scr.efnt^.W;  cpx:=0
  END;
  t.mode:=bmg.xor;
  w:=x+cpw*img.scalx;  p:=ppline;  bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p);
  w:=x+cpx*img.scalx;  p:=pxline;  bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p)
END showprop;

PROCEDURE showline;
  VAR p: WORD;
      t: bmg.TOOL;
    x,y: INTEGER;
BEGIN
  t:=tln;  t.clip:=img.map;
  x:=t.clip.x;
  y:=t.clip.y+scr.efnt^.bline*img.scaly;      p:=pbline;
  bmg.dline(scr.bmd,t,x,y,x+t.clip.w-1,y,p);
  y:=t.clip.y+scr.efnt^.uline*img.scaly;      p:=puline;
  bmg.dline(scr.bmd,t,x,y,x+t.clip.w-1,y,p);
END showline;

PROCEDURE mline(inc: BOOLEAN);
  VAR m,x,y,w,p: INTEGER;  t: bmg.TOOL;
BEGIN
  t:=tln;  t.clip:=img.map;
  x:=t.clip.x;
  y:=t.clip.x;
  WITH scr.efnt^ DO
    CASE Line OF
      |0: m:=W;
          IF propW#NIL THEN w:=ORD(propW^[rsmv[Read].schar]) ELSE w:=m END;
          w:=x+cpw*img.scalx;
          p:=ppline;   bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p);
          IF inc THEN  IF cpw<m THEN INC(cpw); INC(w,img.scalx) END
          ELSE         IF cpw>0 THEN DEC(cpw); DEC(w,img.scalx) END
          END;
          p:=ppline;   bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p)
      |3: m:=W;
          IF propX#NIL THEN w:=ORD(propX^[rsmv[Read].schar]) ELSE w:=m END;
          w:=x+cpx*img.scalx;
          p:=pxline;   bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p);
          IF inc THEN  IF cpx<m THEN INC(cpx); INC(w,img.scalx) END
          ELSE         IF cpx>0 THEN DEC(cpx); DEC(w,img.scalx) END
          END;
          p:=pxline;  bmg.dline(scr.bmd,t,w,y,w,y+t.clip.h-1,p)
      |1: m:=scr.efnt^.H-1;
          y:=y+bline*img.scaly;
          p:=pbline;   bmg.dline(scr.bmd,t,x,y,x+t.clip.w-1,y,p);
          IF inc THEN  IF bline<m THEN INC(bline); INC(y,img.scaly) END
          ELSE         IF bline>0 THEN DEC(bline); DEC(y,img.scaly) END
          END;
          p:=pbline;   bmg.dline(scr.bmd,t,x,y,x+t.clip.w-1,y,p)
      |2: m:=scr.efnt^.H-1;
          y:=y+uline*img.scaly;
          p:=puline;   bmg.dline(scr.bmd,t,x,y,x+t.clip.w-1,y,p);
          IF inc THEN  IF uline<m THEN INC(uline); INC(y,img.scaly) END
          ELSE         IF uline>0 THEN DEC(uline); DEC(y,img.scaly) END
          END;
          p:=puline;   bmg.dline(scr.bmd,t,x,y,x+t.clip.w-1,y,p)
    ELSE
    END
  END
END mline;

PROCEDURE moveline(VAR s: BITSET; blk: bmg.BLOCK; up: BOOLEAN);
BEGIN
  IF ((Line=0) OR (Line=3)) & (scr.efnt^.propW=NIL) THEN RETURN END;
  scr.block(blk,FALSE,TRUE);
  mline(up);
  cpd.wait(500);
  WHILE oncemore(s,blk) & (s*{0}#{}) DO mline(up) END;
  scr.block(blk,FALSE,FALSE)
END moveline;

PROCEDURE setlines;
  VAR c: CHAR;    p,y,yo: INTEGER;
   s,so: BITSET; pressed: BOOLEAN;
BEGIN
  IF scr.efnt=NIL THEN RETURN END;
  so:={};
  LOOP
    cursor(s);
    IF NOT scr.inblock(line) THEN RETURN END;
    IF (so*{0}={}) & (s*{0}#{}) THEN
      IF    scr.inblock(dwlf) THEN moveline(s,dwlf,FALSE)
      ELSIF scr.inblock(uprg) THEN moveline(s,uprg,TRUE)
      ELSE  Line:=(Line+1) MOD 4;  printlines
      END
    END;
    so:=s
  END
END setlines;

-------------------------- Select char -------------------------
                          -------------
PROCEDURE markchar(p,gmode,op: INTEGER);
  VAR t: bmg.TOOL;
BEGIN
  t:=rsmv[op].scrl; t.color:=pup.normal; t.mode:=gmode;
  WITH t.clip DO
    bmg.rect(scr.bmd,t,x+p*sW+1,y,x+(p+1)*sW-1,y+h-1)
  END
END markchar;

PROCEDURE print(pos,op: INTEGER);
  VAR x: INTEGER;
     ch: CHAR;
BEGIN
  x:=pos*22;
  markchar(pos,bmg.rep,op);
  ch:=CHAR(ORD(rsmv[op].first)+pos);
  CASE ch OF
    |00c..37c: x:=x+(sW-bmg.lenght(pup.font,'%b',ch)) DIV 2;
               scr.print(rsmv[op].inner,x,'%b',ch)
    |"0".."9": x:=x+(sW-bmg.lenght(pup.font,'"%c"',ch)) DIV 2;
               scr.print(rsmv[op].inner,x,'"%c"',ch)
  ELSE         x:=x+(sW-bmg.lenght(pup.font,'%c',ch)) DIV 2;
               scr.print(rsmv[op].inner,x,'%c',ch)
  END;
  IF ch=rsmv[op].schar THEN markchar(pos,bmg.xor,op) END
END print;

PROCEDURE printsch(op: INTEGER);
  VAR c: CHAR;
BEGIN
  c:=rsmv[op].schar;
  scr.block(rsmv[op].but,TRUE,FALSE);
  CASE op OF
    |Read: scr.print(rsmv[op].but,4,"Read->%03bc %c",c,c)
    |Save: scr.print(rsmv[op].but,4,"Save->%03bc %c",c,c)
  END;
END printsch;

PROCEDURE moveselect(VAR s: BITSET; op: INTEGER; rg: BOOLEAN);

  PROCEDURE move(): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    IF s*{0}={} THEN RETURN FALSE END;
    WITH rsmv[op] DO
      IF rg THEN
        IF first=0c THEN RETURN FALSE END;  DEC(first);
        bmg.scroll(scr.bmd,scrl,-sW,0);     print(00,op)
      ELSE
        IF first=sB THEN RETURN FALSE END;  INC(first);
        bmg.scroll(scr.bmd,scrl, sW,0);     print(sN,op)
      END;
      RETURN (first>0c) & (first<sB)
    END
  END move;

  VAR b: bmg.BLOCK;
BEGIN
  WITH rsmv[op] DO
    IF rg THEN  IF first=0c THEN RETURN END;  b:=lfbut
    ELSE        IF first=sB THEN RETURN END;  b:=rgbut
    END
  END;
  scr.block(b,FALSE,TRUE);
  IF move() THEN END;
  cpd.wait(500);
  WHILE oncemore(s,b) & move() DO END;
  scr.block(b,FALSE,FALSE)
END moveselect;

PROCEDURE readsave(op: INTEGER);
  VAR c: CHAR;  x,cx,px,pw: INTEGER;  s,o: BITSET;
BEGIN
  o:={};
  LOOP
    cursor(s);
    WITH rsmv[op] DO
      IF NOT scr.inblock(base) THEN RETURN END;
      IF s*{0}#{} THEN
        IF    scr.inblock(lfbut) THEN  moveselect(s,op,TRUE)
        ELSIF scr.inblock(rgbut) THEN  moveselect(s,op,FALSE)
        ELSIF scr.inblock(inner) & (o*{0}={}) THEN
          x:=(crs.x-inner.x) DIV 22;
          IF (first<=schar) & (schar<=CHAR(ORD(first)+16)) THEN
            markchar(ORD(schar)-ORD(first),bmg.xor,op)
          END;
          schar:=CHAR(ORD(first)+x);
          markchar(x,bmg.xor,op);
          printsch(op)
        ELSIF scr.inblock(but) & (o*{0}={}) & (scr.efnt#NIL) THEN
          scr.block(but,FALSE,TRUE);
          CASE op OF
            |Save:  scr.savechar(img.main,schar);
                    IF NOT scr.done THEN
                      perr(scr.error,'Save char: %%s')
                    ELSE
                      WITH scr.efnt^ DO
                        IF propW#NIL THEN
                          px:=cpx;
                          cx:=ORD(cellX^[schar]);
                          IF cx>px THEN cx:=cx-px;  px:=0
                          ELSE          cx:=px-cx;  cx:=0
                          END;
                          cellX^[schar]:=CHAR(cx);
                          propX^[schar]:=CHAR(px);
                          propW^[schar]:=CHAR(cpw-cpx)
                        END
                      END
                    END
             |Read: scr.readchar(img.main,schar);
                    img.refresh;
                    showprop
          END;
          tim.delay(300,tim.milisec);
          scr.block(but,FALSE,FALSE)
        END
      END
    END;
    o:=s
  END
END readsave;

PROCEDURE operation(VAR o: INTEGER);

  PROCEDURE setop;
  BEGIN
    IF o=_mirr THEN pup.mprint(mOper,_mirr,"* mirror")
    ELSE            pup.mprint(mOper,_mirr,"  mirror")
    END;
    IF o=_fill THEN pup.mprint(mOper,_fill,"* fill")
    ELSE            pup.mprint(mOper,_fill,"  fill")
    END;
    IF o=_swap THEN pup.mprint(mOper,_swap,"* swap")
    ELSE            pup.mprint(mOper,_swap,"  swap")
    END;
    IF o=_dot  THEN pup.mprint(mOper,_dot ,"* dot")
    ELSE            pup.mprint(mOper,_dot ,"  dot")
    END;
    IF o=_iddt THEN pup.mprint(mOper,_iddt,"* ins/del dot")
    ELSE            pup.mprint(mOper,_iddt,"  ins/del dot")
    END;
    IF o=_idln THEN pup.mprint(mOper,_idln,"* ins/del line")
    ELSE            pup.mprint(mOper,_idln,"  ins/del line")
    END;
    IF o=_dupl THEN pup.mprint(mOper,_dupl,"* duplicate")
    ELSE            pup.mprint(mOper,_dupl,"  duplicate")
    END;
    IF o=_inv  THEN pup.mprint(mOper,_inv ,"* invers")
    ELSE            pup.mprint(mOper,_inv ,"  invers")
    END;
  END setop;

  CONST wm=100; hm=116;

  VAR x,y: INTEGER;
BEGIN
  IF scr.efnt=NIL THEN RETURN END;
  x:=crs.x;
  IF    x+wm DIV 2>=scr.wblk.w THEN x:=scr.wblk.x+scr.wblk.w-wm
  ELSIF x-wm DIV 2<=0          THEN x:=scr.wblk.x
  ELSE x:=crs.x-wm DIV 2
  END;
  y:=crs.y;
  IF    y+hm DIV 2>=scr.wblk.h THEN y:=scr.wblk.y+scr.wblk.h-hm
  ELSIF y-hm DIV 2<=0          THEN y:=scr.wblk.y
  ELSE y:=crs.y-hm DIV 2
  END;
  pup.mnew(mOper,x,y,wm,hm,xall,' Operation');
  IF NOT pup.done THEN RETURN END;
  setop;
  pup.mhotkey(mOper,0,'m',TRUE);  pup.mhotkey(mOper,1,'f',TRUE);
  pup.mhotkey(mOper,2,'w',TRUE);  pup.mhotkey(mOper,3,'o',TRUE);
  pup.mhotkey(mOper,4,'d',TRUE);  pup.mhotkey(mOper,5,'l',TRUE);
  pup.mhotkey(mOper,6,'u',TRUE);  pup.mhotkey(mOper,7,'i',TRUE);
  pup.mselect(mOper);
  IF pup.mselected(mOper) THEN o:=pup.malt(mOper) END;
  pup.mclose(mOper);              pup.mdispose(mOper)
END operation;

--------------------- READ & WRITE & CREATE --------------------
                     -----------------------
PROCEDURE readfont;
BEGIN
  IF scr.efnt#NIL THEN
    pup.message(220,180,'Just exist font "%s"',fontname); RETURN
  END;
  pup.dselect(direx);
  pup.dclose(direx);
  IF NOT pup.dselected(direx) THEN RETURN END;
  pup.dfullname(direx,fontname);
  scr.readfont(fontname);
  IF NOT scr.done THEN perr(scr.error,'Read "%s": %%s',fontname); RETURN END;
  scr.block(scr.tblk,TRUE,FALSE);  pup.mclose(mFont);
  scr.print(scr.tblk,4,fontname);  scr.clear;
  img.set_scale(scr.fW,scr.fH,M,N);
  scr.readchar(img.main,rsmv[Read].schar);
  img.refresh;        printlines;
  showline;           showprop
END readfont;

PROCEDURE savefont;
BEGIN
  IF scr.efnt=NIL THEN
    pup.message(220,180,'There is NO current font ...'); RETURN
  END;
  scr.writefont(fontname);
  IF NOT scr.done THEN perr(scr.error,'Write "%s": %%s',fontname) END
END savefont;

PROCEDURE savefontas;
BEGIN
  IF scr.efnt=NIL THEN
    pup.message(220,180,'There is NO current font ...'); RETURN
  END;
  pup.dselect(direx);
  pup.dclose(direx);
  IF NOT pup.dselected(direx) THEN RETURN END;
  pup.dfullname(direx,fontname);
  scr.writefont(fontname);
  IF scr.done THEN
    scr.block(scr.tblk,TRUE,FALSE);
    scr.print(scr.tblk,4,fontname);  RETURN
  END;
  perr(scr.error,'Write "%s": %%s',fontname)
END savefontas;

PROCEDURE setfontstate(VAR s: BITSET; fc,lc,fw,fh: INTEGER);
BEGIN
  IF s*fnt.prop#{} THEN pup.mprint(mDesc,0,"Proportional")
  ELSE                  pup.mprint(mDesc,0,"Constant")
  END;
  IF s*fnt.italic#{} THEN pup.mprint(mDesc,1,"Italic")
  ELSE                    pup.mprint(mDesc,1,"Normal")
  END;
  pup.mprint(mDesc,2,"%03bc__First" ,fc);
  pup.mprint(mDesc,3,"%03bc__Last"  ,lc);
  pup.mprint(mDesc,4,"%03d___Width" ,fw);
  pup.mprint(mDesc,5,"%03d___Height",fh)
END setfontstate;

PROCEDURE editdesc(create: BOOLEAN);
  VAR s: BITSET;        i,fc,fw,cw: INTEGER;
      b: bmg.BLOCK;     j,lc,fh,ch: INTEGER;
     hf: pup.MENU;
BEGIN
  IF create THEN
    IF scr.efnt#NIL THEN
      pup.message(220,180,'Just exist font "%s"',fontname); RETURN
    END;
    s:=fnt.prop+fnt.packed;  fc:=000b;  lc:=377b;  fw:=scr.fW;  fh:=scr.fH
  ELSE
    IF scr.efnt=NIL THEN
      pup.message(220,180,"There's no current font"); RETURN
    END;
    WITH scr.efnt^ DO
      s:=state;  fc:=ORD(fchar);  lc:=ORD(lchar);  fw:=W;  fh:=H
    END
  END;
  setfontstate(s,fc,lc,fw,fh);
  pup.mopen(mDesc);
  pup.mblocks(mDesc,b,b,desc,b);
  DEC(desc.y,2);
  IF create THEN scr.print(desc,6,'Create')
  ELSE           scr.print(desc,6,'Change')
  END;
  LOOP
    pup.mselect(mDesc);
    IF NOT pup.mselected(mDesc) THEN pup.mclose(mDesc); RETURN END;
    CASE pup.malt(mDesc) OF
      |0:  s:=s/fnt.prop;
           IF s*fnt.prop#{} THEN pup.mprint(mDesc,0,"Proportional")
           ELSE                  pup.mprint(mDesc,0,"Constant")
           END;
      |1:  s:=s/fnt.italic;
           IF s*fnt.italic#{} THEN pup.mprint(mDesc,1,"Italic")
           ELSE                    pup.mprint(mDesc,1,"Normal")
           END;
      |2:  newvalue(mDesc,fc, 0,255,2,'First char [%3bc] =',
                                     '0c <= First char <= 377c',
                                     "%03bc__First");
      |3:  newvalue(mDesc,lc,fc,255,3,'Last char [%3bc] =',
                                     'First char <= Last char <=377c',
                                     "%03bc__Last");
      |4:  newvalue(mDesc,fw, 1,256,4,"Maximum char's width in font [%3d] =",
                                     " 1 <= Font Width <= 256",
                                     "%03d___Width");
      |5:  newvalue(mDesc,fh, 1,256,5,"Maximum char's height in font [%3d] =",
                                     " 1 <= Font Height <= 256 ",
                                     "%03d___Height");
           IF NOT create THEN
             IF scr.fH<fh THEN
               pup.mnew(hf,70,90,72,72,xall,'  Add');
               pup.mprint(hf,0,'Up');       pup.mhotkey(hf,0,'u',TRUE);
               pup.mprint(hf,1,'Down');     pup.mhotkey(hf,1,'d',TRUE);
               pup.mprint(hf,2,'Up&Down');  pup.mhotkey(hf,2,'b',TRUE)
             ELSE
               pup.mnew(hf,70,90,72,72,xall,'  Cut');
               pup.mprint(hf,0,'Up');       pup.mhotkey(hf,0,'u',TRUE);
               pup.mprint(hf,1,'Down');     pup.mhotkey(hf,1,'d',TRUE);
               pup.mprint(hf,2,'Minimum');  pup.mhotkey(hf,2,'m',TRUE)
             END;
             pup.mselect(hf); cw:= 0;
             pup.mclose(hf);
             IF pup.mselected(hf) THEN ch:=pup.malt(hf) ELSE ch:=-1 END;
             pup.mdispose(hf)
           END
      |6:  IF create THEN
             scr.newfont(fw,fh,CHAR(fc),CHAR(lc),s);
             IF scr.done THEN EXIT END;
             perr(scr.error,'Create: %%s')
           ELSE
             scr.efnt^.fchar:=CHAR(fc);  scr.efnt^.state:=s;
             scr.efnt^.lchar:=CHAR(lc);  scr.newsize(fw,fh,cw,ch);
             IF scr.done THEN EXIT END;
             perr(scr.error,'Change size: %%s')
           END
    ELSE
    END
  END;
  pup.mclose(mDesc);
  pup.mclose(mFont);  scr.clear;
  img.set_scale(scr.fW,scr.fH,M,N);
  scr.readchar(img.main,rsmv[Read].schar);
  img.refresh;        printlines;
  showline;           showprop
END editdesc;

PROCEDURE showfont;
  VAR i: CHAR;
      t: bmg.TOOL;
    x,y: INTEGER;
BEGIN
  IF scr.efnt=NIL THEN RETURN END;
  pup.mclose(mFont);    scr.clear;
  t.mask:=pup.bright;  t.color :=t.mask;
  t.mode:=bmg.xor;     t.back  :={};
  t.zX:=scr.wblk.x;    t.clip.x:=0; t.clip.w:=scr.wblk.w;
  t.zY:=scr.wblk.y;    t.clip.y:=0; t.clip.h:=scr.wblk.h;
  x:=3;
  y:=t.clip.h-scr.fH-1;
  FOR i:=scr.efnt^.fchar TO scr.efnt^.lchar DO
    x:=scr.xwrite(t,x,y,i);
    IF x>=(t.clip.w-scr.fW-3) THEN DEC(y,scr.fH+1); x:=3 END;
    IF y<0 THEN
      pup.message(220,20,'... press any key to continue');
      y:=t.clip.h-scr.fH-1; x:=3;
      scr.clear
    END
  END;
  pup.message(220,20,'... press any key to continue');
  scr.clear;    img.set_scale(scr.fW,scr.fH,M,N);
  img.refresh;  showline;
  printlines;   showprop
END showfont;

PROCEDURE testtype;
  VAR c: CHAR;          str: ARRAY [0..127] OF CHAR;
    s,t: bmg.TOOL;      x,y: INTEGER;
     st: BITSET;
BEGIN
  IF scr.efnt=NIL THEN RETURN END;
  pup.mclose(mFont);    scr.clear;
  t.mask:=pup.normal;  t.color:=t.mask;
  t.mode:=bmg.xor;     t.back :={};
  t.zX:=scr.wblk.x;    t.clip.x:=0; t.clip.w:=scr.wblk.w;
  t.zY:=scr.wblk.y;    t.clip.y:=0; t.clip.h:=scr.wblk.h;
  s.mask:={3};         s.color:=s.mask;
  s.mode:=bmg.xor;     s.back:={};
  s.zX:=scr.wblk.x;    s.clip.x:=0; s.clip.w:=scr.wblk.w;
  s.zY:=scr.wblk.y;    s.clip.y:=0; s.clip.h:=scr.wblk.h;
  x:=5;
  y:=t.clip.h-scr.fH-5;
  LOOP
    IF cpd.ready()>0 THEN
      cursor(st);
      IF st*{2}#{} THEN EXIT END
    END;
    bmg.line(scr.bmd,s,x,y,x,y+scr.efnt^.H);
    key.read(c);
    bmg.line(scr.bmd,s,x,y,x,y+scr.efnt^.H);

    IF c=33c THEN EXIT
    ELSIF (c=key.cr) OR (c=key.lf) THEN  DEC(y,scr.efnt^.H); x:=5
    ELSE
      x:=scr.xwrite(t,x,y,c);
      IF x>=(t.clip.w-scr.fW-5) THEN DEC(y,scr.fH); x:=5 END;
    END;
    IF y<0 THEN
      pup.message(220,20,'New page! Press any key to continue');
      y:=t.clip.h-scr.fH-5; x:=5;
      scr.clear
    END
  END;
  pup.message(220,20,'... press any key to continue');
  scr.clear;    img.set_scale(scr.fW,scr.fH,M,N);
  img.refresh;  showline;
  printlines;   showprop
END testtype;

PROCEDURE packsave;
  VAR c: CHAR;
BEGIN
  WITH scr.efnt^ DO
    FOR c:=fchar TO lchar DO
      scr.readchar(img.main,c); ASSERT(scr.done,scr.error);
      scr.savechar(img.main,c); ASSERT(scr.done,scr.error);
    END
  END;
  scr.writefont(fontname);  ASSERT(scr.done,scr.error);
END packsave;

--------------------------- Main Menu --------------------------
                           -----------
PROCEDURE main_menus;
  VAR c: CHAR;
      s: BITSET;
    m,n: INTEGER;
BEGIN
  LOOP
    cursor(s);
    IF NOT scr.inblock(scr.mblk) THEN RETURN END;
    IF    scr.inblock(file) & (s*{0}#{}) THEN
      scr.block(file,FALSE,TRUE); pup.mopen(mFile);
      pup.mselect(mFile);
      IF pup.mselected(mFile) THEN
        CASE pup.malt(mFile) OF
         |0: readfont
         |1: savefont
         |2: savefontas
        ELSE
        END
      END;
      scr.block(file,FALSE,FALSE);  pup.mclose(mFile);
      crs.move(pup.mx,pup.my);      s:=cpd.state^.keys
    ELSIF scr.inblock(font) & (s*{0}#{}) THEN
      scr.block(font,FALSE,TRUE);  pup.mopen(mFont);
      pup.mselect(mFont);
      IF pup.mselected(mFont) THEN
        CASE pup.malt(mFont) OF
         |0: editdesc(TRUE)               -- create
         |1: editdesc(FALSE)              -- atributes
         |2: scr.disposefont;             pup.mclose(mFont);
             scr.block(coor,TRUE,FALSE);  scr.block(scr.tblk,TRUE,FALSE);
             scr.block(line,TRUE,FALSE);  scr.clear
        ELSE
        END
      END;
      scr.block(font,FALSE,FALSE);  pup.mclose(mFont);
      crs.move(pup.mx,pup.my);      s:=cpd.state^.keys
    ELSIF scr.inblock(opts) & (s*{0}#{}) THEN
      scr.block(opts,FALSE,TRUE);   pup.mselect(mOpts);
      scr.block(opts,FALSE,FALSE);  s:=cpd.state^.keys;
      crs.move(pup.mx,pup.my);
      IF pup.mselected(mOpts) THEN
        CASE pup.malt(mOpts) OF
         |0: showfont
         |1: testtype
         |3: packsave
        ELSE
        END
      END
    ELSIF scr.inblock(scal) & (s*{0}#{}) THEN
      scr.block(scal,FALSE,TRUE);   pup.mselect(mScal);
      scr.block(scal,FALSE,FALSE);  s:=cpd.state^.keys;
      crs.move(pup.mx,pup.my);
      IF pup.mselected(mScal) THEN
        CASE pup.malt(mScal) OF
          |0: m:=1; n:=1
          |1: m:=1; n:=2
          |2: m:=2; n:=1
          |3: m:=2; n:=3
          |4: m:=3; n:=2
        ELSE
        END;
        IF (m#M) OR (n#N) THEN
          scr.clear;  M:=m;  N:=n;
          img.set_scale(scr.fW,scr.fH,M,N);
          showline;  img.refresh;  showprop
        END
      END
    ELSIF scr.inblock(line) & (s*{0}#{}) THEN setlines
    END;
  END
END main_menus;

----------------------- I n i t   P a r t ----------------------
                       -------------------

PROCEDURE initerror(done: BOOLEAN; error: INTEGER);
BEGIN IF done THEN RETURN END; HALT(error) END initerror;

PROCEDURE initinfo;
  VAR s: INTEGER;
BEGIN
  cpw:=0;       cpx:=0;  Line:=0;
  WITH tln DO
    mask:=pup.bright;  color:=mask;  zX:=0;
    mode:=bmg.xor;     back :={  };  zY:=0;
  END;
  s:=pup.sfont^.H+3;
  uprg:=line;  uprg.x:=line.x+line.w-s;  uprg.w:=s;
  dwlf:=uprg;  dwlf.x:=uprg.x-s;
END initinfo;

PROCEDURE initselectchar;
  VAR op,i,x,y: INTEGER; bs: bmg.BLOCK;
BEGIN
  FOR op:=Read TO Save DO
    WITH rsmv[op] DO
      CASE op OF
        |Read: bs:=scr.rblk; refr:=bs; refr.w:=sW*(19-sN)+2; but:=refr
        |Save: bs:=scr.sblk; save:=bs; save.w:=sW*(19-sN)+2; but:=save
      END;
      base:=bs;
      schar:="A";  bs.w:= bs.w-but.w;
      first:="8";  bs.x:=but.x+but.w;
      printsch(op);
      lfbut:=bs;     lfbut.w:=lfbut.h;           arrow(lfbut,lf);
      rgbut:=lfbut;  rgbut.x:=bs.x+bs.w-rgbut.w; arrow(rgbut,rg);
      inner:=bs;     inner.x:=inner.x+lfbut.w;
      inner.w:=inner.w-lfbut.w-rgbut.w;
      scrl:=scr.full;   scrl.color:={};
      WITH scrl.clip DO
        x:=inner.x+1;  y:=inner.y+1;  w:=inner.w-2; h:=inner.h-2
      END;
      x:=scrl.clip.x+sW;
      y:=scrl.clip.y;
      FOR i:=0 TO sN DO
        bmg.line(scr.bmd,scrl,x,y,x,y+scrl.clip.h-1); print(i,op); x:=x+sW
      END
    END;
  END
END initselectchar;

PROCEDURE initbarline;
  VAR i: INTEGER;
BEGIN
  but.mask:=pup.normal;  but.color:=but.mask;
  but.mode:=bmg.bic;     but.back :={};
  file:=scr.mblk;  file.w:=wB;
  font:=file;      font.x:=font.x+font.w;  scr.block(font,FALSE,FALSE);
  opts:=font;      opts.x:=opts.x+opts.w;  scr.block(opts,FALSE,FALSE);
  coor:=opts;      coor.x:=coor.x+coor.w;  scr.block(coor,FALSE,FALSE);
  scal:=coor;      scal.x:=scal.x+scal.w;  scr.block(scal,FALSE,FALSE);
  line:=scal;      line.x:=line.x+line.w;
  scr.print(file,(wB-bmg.lenght(pup.font,"File")   ) DIV 2,"File");
  scr.print(font,(wB-bmg.lenght(pup.font,"Font")   ) DIV 2,"Font");
  scr.print(opts,(wB-bmg.lenght(pup.font,"Options")) DIV 2,"Options");
  scr.print(scal,(wB-bmg.lenght(pup.font,"Scale")  ) DIV 2,"Scale");
  line.w:=scr.mblk.x+scr.mblk.w-line.x
END initbarline;

PROCEDURE initmenus;
  VAR x,y: INTEGER;
BEGIN
  y:=scr.mblk.y-60;
  x:=scr.mblk.x;
  pup.mnew(mFile,x,y,80,60,xall,' Files');
  initerror(pup.done,pup.error);
  pup.mprint(mFile,0,"Read");           pup.mhotkey(mFile,0,'r',TRUE);
  pup.mprint(mFile,1,"Save");           pup.mhotkey(mFile,1,'s',TRUE);
  pup.mprint(mFile,2,"save As...");     pup.mhotkey(mFile,2,'a',TRUE);
  x:=x+wB;
  pup.mnew(mFont,x,y,80,60,xall,' Fonts');
  initerror(pup.done,pup.error);
  pup.mprint(mFont,0,"Create");         pup.mhotkey(mFont,0,'c',TRUE);
  pup.mprint(mFont,1,"Atributes");      pup.mhotkey(mFont,1,'a',TRUE);
  pup.mprint(mFont,2,"Remove");         pup.mhotkey(mFont,2,'d',TRUE);
  x:=x+wB;
  pup.mnew(mOpts,x,y,80,60,xall,' Options');
  initerror(pup.done,pup.error);
  pup.mprint(mOpts,0,"Show");           pup.mhotkey(mOpts,0,'s',TRUE);
  pup.mprint(mOpts,1,"Test type");      pup.mhotkey(mOpts,1,'t',TRUE);
  pup.mprint(mOpts,2,"Pack & save");    pup.mhotkey(mOpts,2,'p',TRUE);
  y:=scr.mblk.y-80;
  x:=x+wB*2;
  pup.mnew(mScal,x,y,56,80,xall,' x/y');
  initerror(pup.done,pup.error);
  pup.mprint(mScal,0,"  1/1");          pup.mhotkey(mScal,0,'0',FALSE);
  pup.mprint(mScal,1,"  1/2");          pup.mhotkey(mScal,1,'1',FALSE);
  pup.mprint(mScal,2,"  2/1");          pup.mhotkey(mScal,2,'2',FALSE);
  pup.mprint(mScal,3,"  2/3");          pup.mhotkey(mScal,3,'3',FALSE);
  pup.mprint(mScal,4,"  3/2");          pup.mhotkey(mScal,4,'4',FALSE);
  y:=scr.mblk.y-80-200;
  x:=scr.mblk.x+32;
  pup.dnew(direx,x,y,150,200,xall,'.','^*.*|*.fnt',pup.dirs+pup.files);
  initerror(pup.done,pup.error);
  y:=scr.mblk.y-150;
  x:=scr.mblk.x+wB+32;
  pup.mnew(mDesc,x,y,103,105,xall,"");
  ASSERT(pup.done,pup.error);
  pup.mprint(mDesc,0,"Proportional");        pup.mhotkey(mDesc,0,'p',TRUE);
  pup.mprint(mDesc,1,"Normal");              pup.mhotkey(mDesc,1,'n',TRUE);
  pup.mprint(mDesc,2,"%03bc__First" ,  0 );  pup.mhotkey(mDesc,2,'f',TRUE);
  pup.mprint(mDesc,3,"%03bc__Last"  ,377b);  pup.mhotkey(mDesc,3,'l',TRUE);
  pup.mprint(mDesc,4,"%03d___Width" , 32 );  pup.mhotkey(mDesc,4,'w',TRUE);
  pup.mprint(mDesc,5,"%03d___Height", 32 );  pup.mhotkey(mDesc,5,'h',TRUE);
  pup.mprint(mDesc,6,"  Execute");           pup.mhotkey(mDesc,6,'e',TRUE);
END initmenus;

BEGIN
  M:=1;
  N:=1;
  fontname:='';
  initbarline;
  initinfo;
  initmenus;
  initselectchar
END fedMenus.
