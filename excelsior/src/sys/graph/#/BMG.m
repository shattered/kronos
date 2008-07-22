IMPLEMENTATION MODULE BMG; (*$N+ Leo & Nick 05-Oct-90. (c) KRONOS *)

FROM SYSTEM  IMPORT ADR, ADDRESS, WORD;
IMPORT  def: defScreen;                 IMPORT  cod: defCodes;
IMPORT  dfn: defFont;                   IMPORT  fmt: Formats;

TYPE
  int     = INTEGER;
  FONT    = dfn.FONT;
  CLIP    = RECORD x0,y0,x1,y1: INTEGER END;

  ARC     = RECORD
              X,Y    : INTEGER;
              x0,y0  : INTEGER;
              x1,y1,r: INTEGER;
              x,y,co : INTEGER;
              xy0,xx0: INTEGER;
              xy1,xx1: INTEGER;
              yx0,yy0: INTEGER;
              yx1,yy1: INTEGER;
              case   : BOOLEAN
            END;

  CIRC    = RECORD x,y,co: INTEGER END;
  CIRCF   = RECORD x,y,co,xn,yn: INTEGER; do: BOOLEAN END;

  ELIPSE  = RECORD
              x,a2,d,g  : INTEGER;
              y,b2,h,t  : INTEGER;
              Da2,ab2,C0: INTEGER;
              Db2,ba2,C1: INTEGER;
            END;

  TRIF    = RECORD
              x,y,co,xn,yn,Dx,Dy,dx,dy,xl: INTEGER;  Gx,case: BOOLEAN
            END;

CONST
  maxW = 4096;

--------------------------- LOW LEVEL --------------------------
                           -----------

VAR lineFF,line00: ADDRESS;
    bumpFF,bump00: ARRAY [0..maxW DIV 32-1] OF WORD;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE cod.move END move;

PROCEDURE gbblt(mode,des,des_ofs,sou,sou_ofs,nobits: INTEGER);
CODE cod.bmg cod.bmg_bblt END gbblt;

PROCEDURE _bblt(des: ADDRESS; des_ofs: INTEGER;
                  sou: ADDRESS; sou_ofs: INTEGER; nobits: INTEGER);
CODE cod.bblt END _bblt;

PROCEDURE bmv(des,des_ofs,sou,sou_ofs,nobits: INTEGER);
CODE cod.bblt END bmv;

PROCEDURE dch(m: INTEGER; bmd: BITMAP; x,y: INTEGER; f: FONT; ch: CHAR);
CODE cod.bmg cod.bmg_dch END dch;

PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
CODE
  cod.li1 cod.sub cod.swap
  cod.li1 cod.sub cod.swap
  cod.bmg cod.bmg_inrect
(*RETURN NOT ((x<0) OR (y<0) OR (x>=w) OR (y>=h))*)
END in_rect;

PROCEDURE rchkz(x,hi: INTEGER): BOOLEAN;
BEGIN RETURN (x>=0) & (x<=hi) END rchkz;
--CODE cod.rchkz END rchkz;

PROCEDURE chk16(x: INTEGER): BOOLEAN;
CODE cod.lid 00h 80h cod.neg cod.lid 0FFh 7Fh cod.rchk END chk16;

PROCEDURE _vline(m: INTEGER; bmd: BITMAP; x,y,len: INTEGER);
CODE cod.bmg cod.bmg_dvl END _vline;

PROCEDURE _line(mode: INTEGER;  bmd: BITMAP; x,y,x1,y1: INTEGER);
CODE cod.bmg cod.bmg_line END _line;

PROCEDURE _clip(VAR clip: CLIP; w,h:INTEGER): BOOLEAN;
CODE
  cod.li1 cod.sub cod.swap
  cod.li1 cod.sub cod.swap
  cod.bmg cod.bmg_clip
END _clip;

PROCEDURE _circ(mode: INTEGER; bm: BITMAP; VAR context: CIRC; X,Y: INTEGER);
CODE cod.bmg cod.bmg_circ END _circ;

PROCEDURE _arc(mode:INTEGER; bm: BITMAP; VAR arc: ARC);
CODE cod.bmg cod.bmg_arc END _arc;

PROCEDURE _circf(VAR crf_context: CIRCF);
CODE cod.bmg cod.bmg_fcirc END _circf;

PROCEDURE _trif(VAR context: TRIF);
CODE cod.bmg cod.bmg_ftri END _trif;


PROCEDURE _sqrt(n: INTEGER): INTEGER;
  VAR l,r: INTEGER;
BEGIN
  ASSERT(n>=0,41h);
  IF n<2 THEN RETURN n END;
  IF n=2 THEN RETURN 1 END;
  l:=1; r:=n DIV 2 + 1;
  REPEAT
    r:=(l+r) DIV 2;
    l:=n     DIV r;
  UNTIL l>=r;
  l:=(l+r+1) DIV 2;
  IF (l>7FFFh) OR (r>7FFFh) THEN
    IF ABS(n DIV l-l)<ABS(n DIV r-r)  THEN RETURN l ELSE RETURN r END;
  ELSE
    IF ABS(n-l*l)<ABS(n-r*r)  THEN RETURN l ELSE RETURN r END
  END
END _sqrt;

PROCEDURE _muldiv(a,b,c: INTEGER): INTEGER;  (* a*b/c *)
  VAR i: INTEGER;
BEGIN
  IF a=MIN(INTEGER) THEN RETURN a DIV c * b END;
  IF b=MIN(INTEGER) THEN RETURN b DIV c * a END;
  IF ABS(a)>ABS(b)  THEN i:=a; a:=b; b:=i   END;
  IF a>MAX(INTEGER) DIV b THEN RETURN a DIV c * b END;
  RETURN a *b DIV c
END _muldiv;


---------------------- BitBlock Procedures ---------------------
                      ---------------------

PROCEDURE cross(VAR dest: BLOCK; blk0,blk1: BLOCK);
  VAR x0,y0,x1,y1: INTEGER;
BEGIN
  WITH blk0 DO x0:=x+w-1; y0:=y+h-1 END;
  WITH blk1 DO x1:=x+w-1; y1:=y+h-1 END;
  IF x1<x0 THEN x0:=x1 END;
  IF y1<y0 THEN y0:=y1 END;
  WITH dest DO
    IF blk0.x>blk1.x THEN x:=blk0.x ELSE x:=blk1.x END;
    IF blk0.y>blk1.y THEN y:=blk0.y ELSE y:=blk1.y END;
    w:=x0-x+1;
    h:=y0-y+1;
    IF (h<=0) OR (w<=0) THEN x:=0; y:=0; w:=0; h:=0 END
  END
END cross;

(*
PROCEDURE overlap(dbmd: BITMAP; VAR dblk: BLOCK;
                  sbmd: BITMAP; VAR sblk: BLOCK);
  VAR W,H: INTEGER;
BEGIN
  W:=sbmd^.W; H:=sbmd^.H;
  WITH sblk DO
    IF x<0 THEN INC(w,x); x:=0 END;
    IF y<0 THEN INC(h,y); y:=0 END;
    IF (w<1) OR (h<1) THEN w:=0; h:=0; RETURN END;
    IF x+w>W THEN w:=W-x END;
    IF y+h>H THEN h:=H-y END;
    IF (w<1) OR (h<1) THEN w:=0; h:=0; RETURN END;
  END;
  W:=dbmd^.W; H:=dbmd^.H;
  WITH dblk DO
    IF x<0 THEN INC(w,x); sblk.w:=w; DEC(sblk.x,x); x:=0 END;
    IF y<0 THEN INC(h,y); sblk.h:=h; DEC(sblk.y,y); y:=0 END;
    IF (w<1) OR (h<1) THEN w:=0; h:=0; RETURN END;
    IF x+w>W THEN w:=W-x END;
    IF y+h>H THEN h:=H-y END;
    IF (w<1) OR (h<1) THEN w:=0; h:=0; RETURN END
  END;
  IF sblk.w<dblk.w THEN dblk.w:=sblk.w ELSE sblk.w:=dblk.w END;
  IF sblk.h<dblk.h THEN dblk.h:=sblk.h ELSE sblk.h:=dblk.h END
END overlap;
*)

PROCEDURE bblt(dbmd: BITMAP; VAL dtool: TOOL;
               x,y : INTEGER;
               sbmd: BITMAP; VAL stool: TOOL;  b: BLOCK);
  VAR   dlr,slr: ADDRESS;  bump: ADDRESS;    su,du: INTEGER;
        ww,woff: INTEGER;  wump: ADDRESS;
      sadr,dadr: ADDRESS;  BUMP: ARRAY [0..maxW DIV 32-1] OF WORD;
      soff,doff: INTEGER; i,w,h: INTEGER;
      swpl,dwpl: INTEGER; dmask: BITSET;
      sblk,dblk: BLOCK;   smask: BITSET;
BEGIN
  dmask:=dbmd^.mask*dtool.mask*dtool.color;
  smask:=sbmd^.mask*stool.mask*stool.color;
  IF (smask={}) OR (dmask={}) THEN RETURN END;
  bump:=ADR(BUMP);

  sblk:=stool.clip;  INC(sblk.x,stool.zX);  INC(sblk.y,stool.zY);
  dblk:=dtool.clip;  INC(dblk.x,dtool.zX);  INC(dblk.y,dtool.zY);
  INC(x,dtool.zX);   INC(b.x,stool.zX);
  INC(y,dtool.zY);   INC(b.y,stool.zY);

  (* clip "b" with "stool.clip": *)
  IF b.x<sblk.x THEN INC(x,sblk.x-x);   DEC(b.w,sblk.x-x); b.x:=sblk.x END;
  IF b.y<sblk.y THEN INC(y,sblk.y-y);   DEC(b.h,sblk.y-y); b.y:=sblk.y END;
  IF b.x+b.w>sblk.x+sblk.w THEN b.w:=sblk.x+sblk.w-b.x END;
  IF b.y+b.h>sblk.y+sblk.h THEN b.h:=sblk.y+sblk.h-b.y END;

  (* clip "b" with "sbmd.WH": *)
  IF b.x<0 THEN DEC(x,b.x); INC(b.w,b.x); b.x:=0 END;
  IF b.y<0 THEN DEC(y,b.y); INC(b.h,b.y); b.y:=0 END;
  IF b.x+b.w>sbmd^.W THEN b.w:=sbmd^.W-b.x END;
  IF b.y+b.h>sbmd^.H THEN b.h:=sbmd^.H-b.y END;

  (* clip "b" with "dtool.clip": *)
  IF x<dblk.x   THEN INC(b.x,dblk.x-x); DEC(b.w,dblk.x-x); x:=dblk.x END;
  IF y<dblk.y   THEN INC(b.y,dblk.y-y); DEC(b.h,dblk.y-y); y:=dblk.y END;
  IF x+b.w>dblk.x+dblk.w THEN b.w:=dblk.x+dblk.w-x END;
  IF y+b.h>dblk.y+dblk.h THEN b.h:=dblk.y+dblk.h-y END;

  (* clip "b" with "dbmd.WH": *)
  IF x<0 THEN DEC(b.x,x); INC(b.w,x); x:=0 END;
  IF y<0 THEN DEC(b.y,y); INC(b.h,y); y:=0 END;
  IF x+b.w>dbmd^.W THEN b.w:=dbmd^.W-x END;
  IF y+b.h>dbmd^.H THEN b.h:=dbmd^.H-y END;

  IF (b.w<=0) OR (b.h<=0) THEN RETURN END;

  soff:=b.x; w:=b.w;
  doff:=  x; h:=b.h;

  woff:= soff DIV 32;
  ww  :=(soff MOD 32+w+31) DIV 32;
  wump:= bump+woff;
  swpl:=sbmd^.WPL;
  dwpl:=dbmd^.WPL;
  b.y:=(sbmd^.H-b.y-1)*swpl;  su:=(h-1)*swpl;
  y  :=(dbmd^.H-  y-1)*dwpl;  du:=(h-1)*dwpl;

  slr:=ADR(sbmd^.layers[0]);
  dlr:=ADR(dbmd^.layers[0]);
  IF dtool.mode#rep THEN
    REPEAT
      WHILE smask*{0}={} DO smask:=smask>>1; INC(slr) END; smask:=smask-{0};
      WHILE dmask*{0}={} DO dmask:=dmask>>1; INC(dlr) END; dmask:=dmask-{0};
      sadr:=INTEGER(slr^)+b.y+woff;
      dadr:=INTEGER(dlr^)+  y;
      i:=h;
      IF dadr<=sadr THEN
        sadr:=sadr-su;  dadr:=dadr-du;
        REPEAT move (wump,sadr,ww); gbblt(dtool.mode,dadr,doff,bump,soff,w);
               INC(sadr,swpl);      INC(dadr,dwpl);  DEC(i)
        UNTIL i=0
      ELSE
        REPEAT move (wump,sadr,ww); gbblt(dtool.mode,dadr,doff,bump,soff,w);
               DEC(sadr,swpl);      DEC(dadr,dwpl);  DEC(i)
        UNTIL i=0
      END
    UNTIL (smask={}) OR (dmask={})
  ELSE
    REPEAT
      WHILE smask*{0}={} DO smask:=smask>>1; INC(slr) END; smask:=smask-{0};
      WHILE dmask*{0}={} DO dmask:=dmask>>1; INC(dlr) END; dmask:=dmask-{0};
      sadr:=INTEGER(slr^)+b.y+woff;
      dadr:=INTEGER(dlr^)+  y;
      i:=h;
      IF ABS(sadr-dadr)<ww THEN (* overlaps *)
        IF dadr<=sadr THEN
          sadr:=sadr-su;  dadr:=dadr-du;
          REPEAT move(wump,sadr,ww); bmv(dadr,doff,bump,soff,w);
                 INC(sadr,swpl);     INC(dadr,dwpl);     DEC(i)
          UNTIL i=0
        ELSE
          REPEAT move(wump,sadr,ww); bmv(dadr,doff,bump,soff,w);
                 DEC(sadr,swpl);     DEC(dadr,dwpl);     DEC(i)
          UNTIL i=0
        END
      ELSE
        IF dadr<=sadr THEN
          sadr:=sadr-su;  dadr:=dadr-du;
          REPEAT bmv(dadr,doff,sadr,soff MOD 32,w);
                 INC(sadr,swpl); INC(dadr,dwpl); DEC(i)
          UNTIL i=0
        ELSE
          REPEAT bmv(dadr,doff,sadr,soff MOD 32,w);
                 DEC(sadr,swpl); DEC(dadr,dwpl); DEC(i)
          UNTIL i=0
        END
      END
    UNTIL (smask={}) OR (dmask={})
  END
END bblt;

---------------  Graphic Primitive Procedures  -----------------
               --------------------------------

PROCEDURE erase(bmd: BITMAP);
  VAR msk: BITSET;  size: INTEGER;  lay,adr: ADDRESS;
BEGIN
  msk:=bmd^.mask;
  lay:=ADR(bmd^.layers[0]);   size:=bmd^.WPL*bmd^.H-1;
  WHILE msk#{} DO
    IF msk*{0}#{} THEN adr:=lay^; adr^:={}; move(adr+1,adr,size) END;
    INC(lay);  msk:=(msk-{0})>>1
  END
END erase;

PROCEDURE fill(bmd: BITMAP; VAL t: TOOL; VAL b: BLOCK; w: INTEGER; SEQ p: WORD);
BEGIN
  pattern(bmd,t,b,w,HIGH(p)+1,p)
END fill;

---------------------- Graphic Primitives ----------------------
                      --------------------

PROCEDURE dot(bmd: BITMAP; VAL tool: TOOL; x,y: INTEGER);
  VAR   m: INTEGER;
      msk: BITSET;      ofs: INTEGER;     b: POINTER TO ADDRESS;
      col: BITSET;      bit: BITSET;      a: POINTER TO BITSET;
BEGIN
  IF NOT in_rect(x-tool.clip.x,y-tool.clip.y,tool.clip.w,tool.clip.h) THEN
    RETURN
  END;
  x  :=x+tool.zX;
  y  :=y+tool.zY;
  m  :=tool.mode;
  b  :=ADR(bmd^.layers[0]);
  msk:=tool.mask*bmd^.mask;
  ofs:=(bmd^.H-1-y)*bmd^.WPL+x DIV 32;
  bit:={x MOD 32};
  CASE m OF
  |rep: col:=tool.color;
        REPEAT
          IF msk*{0}#{} THEN
            msk:=msk-{0}; a:=b^+ofs;
            IF col*{0}={} THEN a^:=a^-bit ELSE a^:=a^+bit END
          END;
          b:=ADDRESS(b)+1;  col:=col>>1;  msk:=msk>>1
        UNTIL msk={}
  |xor: msk:=msk*tool.color;
        REPEAT
          IF msk*{0}#{} THEN a:=b^+ofs; a^:=a^/bit; msk:=msk-{0} END;
          b:=ADDRESS(b)+1;   msk:=msk>>1
        UNTIL msk={}
  |bic: msk:=msk*tool.color;
        REPEAT
          IF msk*{0}#{} THEN a:=b^+ofs; a^:=a^-bit; msk:=msk-{0} END;
          b:=ADDRESS(b)+1;   msk:=msk>>1
        UNTIL msk={}
  | or: msk:=msk*tool.color;
        REPEAT
          IF msk*{0}#{} THEN a:=b^+ofs; a^:=a^+bit; msk:=msk-{0} END;
          b:=ADDRESS(b)+1;   msk:=msk>>1
        UNTIL msk={}
  ELSE
  END
END dot;

PROCEDURE dline(bmd: BITMAP; VAL tool: TOOL; xb,yb,xe,ye: INTEGER; VAR r: WORD);
  VAR b: ADDRESS;
      i: INTEGER;
    msk: BITSET;
    col: BITSET;
    BMD: BMD;
 cliper: CLIP;
BEGIN
  IF xe<xb THEN i:=xe; xe:=xb; xb:=i; i:=ye; ye:=yb; yb:=i END;
  IF in_rect(xb-tool.clip.x,yb-tool.clip.y,tool.clip.w,tool.clip.h)
   & in_rect(xe-tool.clip.x,ye-tool.clip.y,tool.clip.w,tool.clip.h)
  THEN
    xb:=xb+tool.zX;   yb:=bmd^.H-1-(yb+tool.zY);
    xe:=xe+tool.zX;   ye:=bmd^.H-1-(ye+tool.zY)
  ELSE
    cliper.x0:=xb-tool.clip.x;      cliper.y0:=yb-tool.clip.y;
    cliper.x1:=xe-tool.clip.x;      cliper.y1:=ye-tool.clip.y;
    IF NOT _clip(cliper,tool.clip.w,tool.clip.h) THEN RETURN END;
    i :=tool.clip.x+tool.zX;
    xb:=cliper.x0+i;
    xe:=cliper.x1+i;
    i :=tool.clip.y+tool.zY;
    yb:=bmd^.H-1-(cliper.y0+i);
    ye:=bmd^.H-1-(cliper.y1+i)
  END;
  BMD:=bmd^;
  bmd:=ADR(BMD);
  msk:=bmd^.mask*tool.mask;
  b  :=ADR(bmd^.layers[0]);
  IF tool.mode=rep THEN
    col:=tool.color;
    REPEAT
      bmd^.PATTERN:=r;
      IF msk*{0}#{} THEN
        msk:=msk-{0};  bmd^.BASE:=b^;
        IF col*{0}#{} THEN _line(rep,bmd,xb,yb,xe,ye)
        ELSE               _line(bic,bmd,xb,yb,xe,ye)
        END
      END;
      INC(b);  col:=col>>1;  msk:=msk>>1
    UNTIL msk={}
  ELSE
    msk:=msk*tool.color;
    REPEAT
      bmd^.PATTERN:=r;
      IF msk*{0}#{} THEN
        msk:=msk-{0};  bmd^.BASE:=b^;  _line(tool.mode,bmd,xb,yb,xe,ye)
      END;
      INC(b);  msk:=msk>>1
    UNTIL msk={}
  END;
  r:=bmd^.PATTERN
END dline;

PROCEDURE line(bmd: BITMAP; VAL tool: TOOL; xb,yb,xe,ye: INTEGER);
  VAR b: ADDRESS;
      i: INTEGER;
    len: INTEGER;
    msk: BITSET;
    col: BITSET;
   roll: BITSET;
   clip: CLIP;
BEGIN
  IF xb=xe THEN (* vline *)
    IF NOT rchkz(xb-tool.clip.x,tool.clip.w-1) THEN RETURN END;
    xb:=xb+tool.zX;
    IF yb>ye THEN i:=ye; ye:=yb; yb:=i END;
    IF yb<tool.clip.y THEN yb:=tool.clip.y END;
    len:=ye-yb+1;
    IF yb+len>tool.clip.y+tool.clip.h THEN len:=tool.clip.y+tool.clip.h-yb END;
    IF len<=0 THEN RETURN END;
    yb:=bmd^.H-(yb+tool.zY+len);
    b :=ADR(bmd^.layers[0]);    msk:=tool.mask*bmd^.mask;
    IF tool.mode=rep THEN
      col:=tool.color;
      REPEAT
        IF msk*{0}#{} THEN
          msk:=msk-{0};  bmd^.BASE:=b^;
          IF col*{0}#{} THEN _vline(rep,bmd,xb,yb,len)
          ELSE               _vline(bic,bmd,xb,yb,len)
          END
        END;
        INC(b);  col:=col>>1;  msk:=msk>>1
      UNTIL msk={}
    ELSE
      msk:=msk*tool.color;
      REPEAT
        IF msk*{0}#{} THEN
          msk:=msk-{0};  bmd^.BASE:=b^;  _vline(tool.mode,bmd,xb,yb,len)
        END;
        INC(b);  msk:=msk>>1
      UNTIL msk={}
    END;
    RETURN
  END; (* vline *)

  IF yb=ye THEN (* hline *)
    IF NOT rchkz(yb-tool.clip.y,tool.clip.h-1) THEN RETURN END;
    yb:=yb+tool.zY;
    IF xb>xe THEN i:=xe; xe:=xb; xb:=i END;
    IF xb<tool.clip.x THEN xb:=tool.clip.x END;
    len:=xe-xb+1;
    IF xb+len>tool.clip.x+tool.clip.w THEN len:=tool.clip.x+tool.clip.w-xb END;
    IF len<=0 THEN RETURN END;
    xb:=xb+tool.zX;
    yb:=(bmd^.H-1-yb)*bmd^.WPL;      msk:=tool.mask*bmd^.mask;
    b :=ADR(bmd^.layers[0]);
    CASE tool.mode OF
      |rep: col:=tool.color;
            REPEAT
              IF msk*{0}#{} THEN
                msk:=msk-{0};
                IF col*{0}#{} THEN bmv(int(b^)+yb,xb,lineFF,xb,len)
                ELSE               bmv(int(b^)+yb,xb,line00,xb,len)
                END
              END;
              INC(b);  col:=col>>1;  msk:=msk>>1
            UNTIL msk={}
      |or : msk:=msk*tool.color;
            REPEAT
              IF msk*{0}#{} THEN
                msk:=msk-{0}; bmv(int(b^)+yb,xb,lineFF,xb,len)
              END;
              INC(b);  msk:=msk>>1
            UNTIL msk={}
      |bic: msk:=msk*tool.color;
            REPEAT
              IF msk*{0}#{} THEN
                msk:=msk-{0}; bmv(int(b^)+yb,xb,line00,xb,len)
              END;
              INC(b);  msk:=msk>>1
            UNTIL msk={}
      |xor: msk:=msk*tool.color;
            REPEAT
              IF msk*{0}#{} THEN
                msk:=msk-{0}; gbblt(xor,int(b^)+yb,xb,lineFF,xb,len)
              END;
              INC(b);  msk:=msk>>1
            UNTIL msk={}
    ELSE

    END; (* CASE *)
    RETURN
  END; (* hline *)

  roll:={0..31};
  dline(bmd,tool,xb,yb,xe,ye,roll)
END line;

CONST
  power =
    ARRAY OF BITSET {
      {},
      {0..00}, {0..01}, {0..02}, {0..03}, {0..04}, {0..05}, {0..06}, {0..07},
      {0..08}, {0..09}, {0..10}, {0..11}, {0..12}, {0..13}, {0..14}, {0..15},
      {0..16}, {0..17}, {0..18}, {0..19}, {0..20}, {0..21}, {0..22}, {0..23},
      {0..24}, {0..25}, {0..26}, {0..27}, {0..28}, {0..29}, {0..30}, {0..31} };

PROCEDURE rect(bmd: BITMAP; VAL tool: TOOL; xb,yb,xe,ye: INTEGER);
  VAR msk: BITSET;                m0,m1: BITSET;
      col: BITSET;                x0,x1: INTEGER;
     b,b0: ADDRESS;             a,a0,a1: POINTER TO BITSET;
    i,w,h: INTEGER;                 MSK: BITSET;
      wpl: INTEGER;
BEGIN
  IF xb>xe THEN i:=xb; xb:=xe; xe:=i END;
  IF yb>ye THEN i:=yb; yb:=ye; ye:=i END;
  (* yb<ye & xb<xe ==> xb,yb left down corner *)
  IF xb<tool.clip.x THEN xb:=tool.clip.x END;
  IF yb<tool.clip.y THEN yb:=tool.clip.y END;
  w:=xe-xb+1;
  h:=ye-yb+1;
  i:=tool.clip.x+tool.clip.w;
  IF xb+w>i THEN w:=i-xb END;
  i:=tool.clip.y+tool.clip.h;
  IF yb+h>i THEN h:=i-yb END;
  IF (w<=0) OR (h<=0) THEN RETURN END;
  yb:=(bmd^.H-(yb+tool.zY+h))*bmd^.WPL;
  xb:=xb+tool.zX;  (* m0:={x0..31} *)
  wpl:=bmd^.WPL;
  IF tool.mode=rep THEN
    xe:=xb+w-1;
    x0:=xb MOD 32; (* m0:={x0..31} *) m0:=power[x0]/{0..31}; xb:=xb-x0;
    x1:=xe MOD 32; (* m1:={00..x1} *) m1:=power[x1+1];       xe:=xe-x1;
    b0:=ADR(bmd^.layers[0]);
    MSK:=tool.mask*bmd^.mask;
    IF xb=xe THEN (* whole rectangle into single word width *)
      m0:=m0*m1;
      yb:=yb+xb DIV 32;
      REPEAT
        b:=b0;  msk:=MSK;  col:=tool.color;
        REPEAT
          IF msk*{0}#{} THEN
            msk:=msk-{0};      a :=ADDRESS(b^)+yb;
            IF col*{0}#{} THEN a^:=a^+m0 ELSE a^:=a^-m0 END
          END;
          INC(b);  col:=col>>1;  msk:=msk>>1
        UNTIL msk={};
        INC(yb,wpl);  DEC(h)
      UNTIL h=0
    ELSE
      x0:=yb+xb DIV 32;  w :=(xe-xb-1) DIV 32;
      x1:=yb+xe DIV 32;  yb:=x0+1;
      REPEAT
        b:=b0;  msk:=MSK;  col:=tool.color;
        REPEAT
          IF msk*{0}#{} THEN
            a0:=ADDRESS(b^)+x0;   msk:=msk-{0};
            a1:=ADDRESS(b^)+x1;   a  :=ADDRESS(b^)+yb;
            IF col*{0}#{} THEN
              a0^:=a0^+m0; move(a,lineFF,w); a1^:=a1^+m1
            ELSE
              a0^:=a0^-m0; move(a,line00,w); a1^:=a1^-m1
            END
          END;
          INC(b);  col:=col>>1;  msk:=msk>>1
        UNTIL msk={};
        INC(yb,wpl); INC(x0,wpl); INC(x1,wpl); DEC(h)
      UNTIL h=0
    END
  ELSE
    REPEAT
      b  :=ADR(bmd^.layers[0]);
      msk:=tool.mask*tool.color*bmd^.mask;
      col:=tool.color;
      REPEAT
        IF msk*{0}#{} THEN
          msk:=msk-{0};  gbblt(tool.mode,int(b^)+yb,xb,lineFF,xb,w)
        END;
        INC(b);  msk:=msk>>1;  col:=col>>1
      UNTIL msk={};
      INC(yb,wpl);  DEC(h)
    UNTIL h=0
  END
END rect;

PROCEDURE frame(bmd: BITMAP; VAL t: TOOL; x0,y0,x1,y1: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF y0=y1 THEN line(bmd,t,x0,y0,x1,y0); RETURN END;
  IF x0=x1 THEN line(bmd,t,x0,y0,x0,y1); RETURN END;
  IF x0>x1 THEN i:=x0; x0:=x1; x1:=i END;
  IF y0>y1 THEN i:=y0; y0:=y1; y1:=i END;
  line(bmd,t,x0+1,y0,x1,y0);   line(bmd,t,x1,y0+1,x1,y1);
  line(bmd,t,x1-1,y1,x0,y1);   line(bmd,t,x0,y1-1,x0,y0)
END frame;

PROCEDURE arc(bmd: BITMAP; VAL tool: TOOL; X0,Y0,xa,ya,xb,yb,R: INTEGER);
(* !!! bad release *)

  PROCEDURE inerarc;
    VAR b: ADDRESS;
     arc0: ARC;
     arc1: ARC;
     col : BITSET;
     msk : BITSET;
  BEGIN
    X0:=X0+tool.zX;   xa:=xa+tool.zX;   xb:=xb+tool.zX;
    Y0:=Y0+tool.zY;   ya:=ya+tool.zY;   yb:=yb+tool.zY;
    Y0:=bmd^.H-1-Y0;  ya:=bmd^.H-1-ya;  yb:=bmd^.H-1-yb;

    arc0.x0:=xa-X0;         arc0.x1:=xb-X0;
    arc0.y0:=ya-Y0;         arc0.y1:=yb-Y0;
    arc0.x :=R;             arc0.y :=0;
    arc0.co:=R DIV 2;       arc0.X  :=X0;
    arc0.r :=R;             arc0.Y  :=Y0;
    arc0.xy0:=R*arc0.y0;    arc0.yx0:=0;
    arc0.xy1:=R*arc0.y1;    arc0.yx1:=0;
    arc0.xx0:=R*arc0.x0;    arc0.yy0:=0;
    arc0.xx1:=R*arc0.x1;    arc0.yy1:=0;
    arc0.case:=(arc0.y1*arc0.x0>=arc0.x1*arc0.y0);

    b:=ADR(bmd^.layers[0]);
    IF tool.mode=rep THEN
      msk:=tool.mask*bmd^.mask;
      col:=tool.color;
      REPEAT
        IF msk*{0}#{} THEN
          bmd^.BASE:=b^;  arc1:=arc0;  msk:=msk-{0};
          IF col*{0}#{} THEN  REPEAT _arc(rep,bmd,arc1) UNTIL arc1.y>arc1.x
          ELSE                REPEAT _arc(bic,bmd,arc1) UNTIL arc1.y>arc1.x
          END
        END;
        INC(b);  col:=col>>1;  msk:=msk>>1
      UNTIL msk={}
    ELSE
      msk:=tool.mask*tool.color*bmd^.mask;
      REPEAT
        IF msk*{0}#{} THEN
          bmd^.BASE:=b^;  arc1:=arc0;  msk:=msk-{0};
          REPEAT _arc(tool.mode,bmd,arc1) UNTIL arc1.y>arc1.x
        END;
        INC(b);  msk:=msk>>1
      UNTIL msk={}
    END
  END inerarc;

  VAR X,Y,x0,y0,x1,y1,r: INTEGER;
      x,y,co: INTEGER;
      xy0,xx0: INTEGER;
      xy1,xx1: INTEGER;
      yx0,yy0: INTEGER;
      yx1,yy1: INTEGER;
      case: BOOLEAN;

BEGIN
  IF R<1 THEN dot(bmd,tool,X0,Y0) END;
  IF X0+R< tool.clip.x             THEN RETURN END;
  IF X0-R>=tool.clip.x+tool.clip.w THEN RETURN END;
  IF Y0+R< tool.clip.y             THEN RETURN END;
  IF Y0-R>=tool.clip.y+tool.clip.h THEN RETURN END;

  IF (X0-R>=tool.clip.x) & (X0+R<=tool.clip.x+tool.clip.w)
   & (Y0-R>=tool.clip.y) & (Y0+R<=tool.clip.y+tool.clip.h)
  THEN
    inerarc; RETURN
  END;

  x:=R;
  y:=0;
  r:=R;
  co:=R DIV 2;
  x0 :=xb-X0; x1 :=xa-X0; X  :=X0;
  y0 :=yb-Y0; y1 :=ya-Y0; Y  :=Y0;
  xy0:=x*y0;  xx0:=x*x0;  yx0:=0;  yy0:=0;
  xy1:=x*y1;  xx1:=x*x1;  yx1:=0;  yy1:=0;
  case:=(y1*x0>=x1*y0);
  REPEAT
    IF case THEN
      IF (+yx0<=+xy0) OR (+yx1>=+xy1) THEN dot(bmd,tool,X+x,Y+y) END;
      IF (-xx0>=+yy0) OR (-xx1<=+yy1) THEN dot(bmd,tool,X-y,Y+x) END;
      IF (+yx0>=+xy0) OR (+yx1<=+xy1) THEN dot(bmd,tool,X-x,Y-y) END;
      IF (-xx0<=+yy0) OR (-xx1>=+yy1) THEN dot(bmd,tool,X+y,Y-x) END;
      IF (y#x)&(y#0) THEN
        IF (+xx0<=+yy0) OR (+xx1>=+yy1) THEN dot(bmd,tool,X+y,Y+x) END;
        IF (-yx0>=+xy0) OR (-yx1<=+xy1) THEN dot(bmd,tool,X-x,Y+y) END;
        IF (+xx0>=+yy0) OR (+xx1<=+yy1) THEN dot(bmd,tool,X-y,Y-x) END;
        IF (-yx0<=+xy0) OR (-yx1>=+xy1) THEN dot(bmd,tool,X+x,Y-y) END
      END;
    ELSE
      IF (+yx0<=+xy0) & (+yx1>=+xy1) THEN dot(bmd,tool,X+x,Y+y) END;
      IF (-xx0>=+yy0) & (-xx1<=+yy1) THEN dot(bmd,tool,X-y,Y+x) END;
      IF (+yx0>=+xy0) & (+yx1<=+xy1) THEN dot(bmd,tool,X-x,Y-y) END;
      IF (-xx0<=+yy0) & (-xx1>=+yy1) THEN dot(bmd,tool,X+y,Y-x) END;
      IF (y#x) & (y#0) THEN
        IF (+xx0<=+yy0) & (+xx1>=+yy1) THEN dot(bmd,tool,X+y,Y+x) END;
        IF (-yx0>=+xy0) & (-yx1<=+xy1) THEN dot(bmd,tool,X-x,Y+y) END;
        IF (+xx0>=+yy0) & (+xx1<=+yy1) THEN dot(bmd,tool,X-y,Y-x) END;
        IF (-yx0<=+xy0) & (-yx1>=+xy1) THEN dot(bmd,tool,X+x,Y-y) END;
      END
    END;
    INC(co,y);   INC(y);
    INC(yx0,x0); INC(yy0,y0);
    INC(yx1,x1); INC(yy1,y1);
    IF co>=x THEN
      DEC(co,x);   DEC(x);
      DEC(xy0,y0); DEC(xx0,x0);
      DEC(xy1,y1); DEC(xx1,x1);
    END
  UNTIL y>x
END arc;

PROCEDURE arc3(bmd: BITMAP; VAL tool: TOOL; xl,yl,xm,ym,xr,yr: INTEGER);

  PROCEDURE equ(x0,y0,x1,y1: INTEGER): BOOLEAN;
  BEGIN RETURN (x0=x1) & (y0=y1) END equ;

  PROCEDURE arc0; BEGIN line(bmd,tool,xl,yl,xr,yr) END arc0;

  VAR a1,a2,b1,b2,c1,c2,t,xc,yc,r: INTEGER;

BEGIN
  IF equ(xl,yl,xr,yr) THEN line(bmd,tool,xl,yl,xm,ym); RETURN END;
  IF equ(xl,yl,xm,ym) OR equ(xm,ym,xr,yr) THEN  arc0;  RETURN END;
  IF xl=xr THEN
    IF xm=xl THEN arc0; RETURN END
  ELSIF chk16(xl)&chk16(xr)&chk16(xm)&chk16(yl)&chk16(yr)&chk16(ym) THEN
    IF ym=_muldiv((xm-xl),(yr-yl),(xr-xl)) + yl THEN arc0; RETURN END
  ELSE
    arc0; RETURN
  END;
  a1:=xl-xm; a2:=xl-xr; b1:=yl-ym; b2:=yl-yr;
  c1:=xl*xl-xm*xm + yl*yl-ym*ym;
  c2:=xl*xl-xr*xr + yl*yl-yr*yr;
  t:=2*(a1*b2-a2*b1);
  IF t=0 THEN arc0; RETURN END;
  xc:=(c1*b2-c2*b1) DIV t;
  yc:=(a1*c2-a2*c1) DIV t;
  r:=_sqrt((xl-xc)*(xl-xc)+(yl-yc)*(yl-yc));
  IF ((xl-xr)*(ym-yr)-(xm-xr)*(yl-yr))<0 THEN
    t:=xl; xl:=xr; xr:=t; t:=yl; yl:=yr; yr:=t
  END;
  arc(bmd,tool,xc,yc,xl,yl,xr,yr,r)
END arc3;

PROCEDURE circle(bmd: BITMAP; VAL tool: TOOL; xc,yc,r: INTEGER);

  VAR lay,mod: INTEGER;

  PROCEDURE inercirc;
    VAR b: ADDRESS;
      col: BITSET;
      msk: BITSET;
     circ: CIRC;
  BEGIN
    b :=ADR(bmd^.layers[0]);
    xc:=xc+tool.zX;
    yc:=bmd^.H-1-(yc+tool.zY);
    IF tool.mode=rep THEN
      msk:=tool.mask*bmd^.mask; col:=tool.color;
      REPEAT
        IF msk*{0}#{} THEN
          bmd^.BASE:=b^; msk:=msk-{0};
          circ.x:=r;  circ.y:=0;  circ.co:=(r+1) DIV 2;
          IF col*{0}#{} THEN
            REPEAT _circ(rep,bmd,circ,xc,yc) UNTIL circ.y>circ.x
          ELSE
            REPEAT _circ(bic,bmd,circ,xc,yc) UNTIL circ.y>circ.x
          END
        END;
        INC(b);  col:=col>>1;  msk:=msk>>1
      UNTIL msk={}
    ELSE
      msk:=tool.mask*tool.color*bmd^.mask;
      REPEAT
        IF msk*{0}#{} THEN
          bmd^.BASE:=b^; msk:=msk-{0};
          circ.x:=r;  circ.y:=0;  circ.co:=(r+1) DIV 2;
          REPEAT _circ(tool.mode,bmd,circ,xc,yc) UNTIL circ.y>circ.x
        END;
        INC(b);  msk:=msk>>1
      UNTIL msk={}
    END
  END inercirc;

  VAR x,y,co: INTEGER;

BEGIN
  IF r<1 THEN dot(bmd,tool,xc,yc); RETURN END;

  IF xc+r< tool.clip.x             THEN RETURN END;
  IF xc-r>=tool.clip.x+tool.clip.w THEN RETURN END;
  IF yc+r< tool.clip.y             THEN RETURN END;
  IF yc-r>=tool.clip.y+tool.clip.h THEN RETURN END;

  IF (xc-r>=tool.clip.x) & (xc+r<=tool.clip.x+tool.clip.w)
   & (yc-r>=tool.clip.y) & (yc+r<=tool.clip.y+tool.clip.h)
  THEN
    inercirc; RETURN
  END;

  x:=r; y:=1; co:=r DIV 2;
  dot(bmd,tool,xc+0,yc+r);  dot(bmd,tool,xc+0,yc-r);
  dot(bmd,tool,xc-r,yc+0);  dot(bmd,tool,xc+r,yc+0);
  REPEAT
    IF co>=x THEN DEC(co,x); DEC(x) END;
    IF x#y THEN
      dot(bmd,tool,xc+x,yc+y);  dot(bmd,tool,xc+x,yc-y);
      dot(bmd,tool,xc-x,yc-y);  dot(bmd,tool,xc-x,yc+y);
      dot(bmd,tool,xc+y,yc+x);  dot(bmd,tool,xc+y,yc-x);
      dot(bmd,tool,xc-y,yc-x);  dot(bmd,tool,xc-y,yc+x)
    ELSE
      dot(bmd,tool,xc+x,yc+y);  dot(bmd,tool,xc+x,yc-y);
      dot(bmd,tool,xc-x,yc-y);  dot(bmd,tool,xc-x,yc+y)
    END;
    INC(co,y); INC(y)
  UNTIL y>=x
END circle;

PROCEDURE circlef(bmd: BITMAP; VAL tool: TOOL; X,Y,r: INTEGER);
  VAR circf: CIRCF;
BEGIN
  IF r<1 THEN dot(bmd,tool,X,Y); RETURN END;
  IF X+r< tool.clip.x             THEN RETURN END;
  IF X-r>=tool.clip.x+tool.clip.w THEN RETURN END;
  IF Y+r< tool.clip.y             THEN RETURN END;
  IF Y-r>=tool.clip.y+tool.clip.h THEN RETURN END;
  circf.x:=r; circf.y:=0; circf.co:=r DIV 2;
  line(bmd,tool,X-circf.x,Y+circf.y,X+circf.x,Y+circf.y);
  LOOP
    _circf(circf);
    line(bmd,tool,X-circf.x,Y+circf.y,X+circf.x,Y+circf.y);
    line(bmd,tool,X-circf.x,Y-circf.y,X+circf.x,Y-circf.y);
    IF circf.do & (circf.xn#circf.y) & (circf.yn#circf.x) THEN
      line(bmd,tool,X-circf.yn,Y+circf.xn,X+circf.yn,Y+circf.xn);
      line(bmd,tool,X-circf.yn,Y-circf.xn,X+circf.yn,Y-circf.xn)
    END;
    IF circf.y>=circf.x THEN EXIT END
  END
END circlef;

PROCEDURE CRCF4(VAR x,y,d: INTEGER);
BEGIN
  IF y<x  THEN
    IF d>=x THEN  DEC(d,x); DEC(x) END;
    INC(y); INC(d,y)
  ELSE
    WHILE x>0 DO
      IF d<x THEN INC(y); INC(d,y); RETURN END;
      DEC(d,x); DEC(x)
    END
  END;
END CRCF4;

PROCEDURE ring(bmd: BITMAP; VAL tool: TOOL; xc,yc,r1,r2: INTEGER);
  VAR x1,y1,d1: INTEGER;
      x2,y2,d2: INTEGER;
BEGIN
  IF r1>r2 THEN x1:=r2; r2:=r1; r1:=x1 END;
  IF r2=r1 THEN circle (bmd,tool,xc,yc,r2); RETURN END;
  IF r1<0  THEN circlef(bmd,tool,xc,yc,r2); RETURN END;
  x1:=r1;  y1:=0;  d1:=r1 DIV 2;
  x2:=r2;  y2:=0;  d2:=r2 DIV 2;
  line(bmd,tool,xc-r2,yc,xc-r1-1,yc);
  line(bmd,tool,xc+r2,yc,xc+r1+1,yc);
  REPEAT
    CRCF4(x2,y2,d2);
    IF y1<r1 THEN
      CRCF4(x1,y1,d1);
      line(bmd,tool,xc-x2,yc+y2,xc-x1-1,yc+y2);
      line(bmd,tool,xc+x2,yc+y2,xc+x1+1,yc+y2);
      line(bmd,tool,xc-x2,yc-y2,xc-x1-1,yc-y2);
      line(bmd,tool,xc+x2,yc-y2,xc+x1+1,yc-y2)
    ELSE
      line(bmd,tool,xc-x2,yc+y2,xc+x2,yc+y2);
      line(bmd,tool,xc-x2,yc-y2,xc+x2,yc-y2)
    END
  UNTIL y2>=r2
END ring;

PROCEDURE _ellipse(VAR elipse: ELIPSE);
BEGIN
  WITH elipse DO
    IF g<0 THEN
      d:=C0;
      IF h < 0 THEN INC(g,C0);
      ELSE  DEC(y); DEC(C1,Da2); DEC(d,C1); INC(g,d+Da2)
      END;
      INC(C0,Db2);  INC(h,d); INC(x);
    ELSE
      INC(h,C0);
      IF h>=0 THEN DEC(x); DEC(C1,Db2); DEC(h,C1) END;
      INC(C0,Da2); INC(y)
    END
  END
END _ellipse;

PROCEDURE ellipse0(bmd: BITMAP; VAL tool: TOOL; xc,yc,a,b: INTEGER);
  VAR elipse: ELIPSE;
BEGIN
  IF (a<0) OR (b<0) THEN dot(bmd,tool,xc,yc); RETURN END;
  dot(bmd,tool,xc,yc+b); dot(bmd,tool,xc,yc-b);
  WITH elipse DO
    a2:=a*a;  ba2:=b*a2;  Da2:=2*a2;  x:=0;  C0:=0;
    b2:=b*b;  ab2:=a*b2;  Db2:=2*b2;  y:=b;  C1:=b*Da2;
    h:=  a2 DIV 4 - ba2   + b2;
    g:=9*a2 DIV 4 - ba2*3 + b2;
    d:=Db2+b2;
    WHILE g<0 DO
      dot(bmd,tool,xc+x,yc+y); dot(bmd,tool,xc+x,yc-y);
      dot(bmd,tool,xc-x,yc+y); dot(bmd,tool,xc-x,yc-y);
      _ellipse(elipse)
    END;
    dot(bmd,tool,xc+a,yc); dot(bmd,tool,xc-a,yc);
    x:=a; t:=y; y:=0;
    h:=b2 DIV 4-ab2+Da2;
    C0:=0; C1:=a*Db2;
    WHILE y <= t DO
      dot(bmd,tool,xc+x,yc+y); dot(bmd,tool,xc+x,yc-y);
      dot(bmd,tool,xc-x,yc+y); dot(bmd,tool,xc-x,yc-y);
      _ellipse(elipse)
    END
  END
END ellipse0;

PROCEDURE ellipse0f(bmd: BITMAP; VAL tool: TOOL; xc,yc,a,b: INTEGER);
  VAR yo: INTEGER;
  elipse: ELIPSE;
BEGIN
  line(bmd,tool,xc-a,yc,xc+a,yc);
  WITH elipse DO
    a2:=a*a;  ba2:=b*a2;  Da2:=2*a2;  x:=0;  C0:=0;
    b2:=b*b;  ab2:=a*b2;  Db2:=2*b2;  y:=b;  C1:=b*Da2;
    h:=  a2 DIV 4 - ba2   + b2;
    g:=9*a2 DIV 4 - ba2*3 + b2;
    d:=Db2+b2;
    yo:=y;
    WHILE g<0 DO
      line(bmd,tool,xc+x,yc+y,xc-x,yc+y);
      line(bmd,tool,xc+x,yc-y,xc-x,yc-y);
      WHILE yo=y DO _ellipse(elipse) END;
      yo:=y
    END;
    x:=a; t:=y; y:=0;
    h:=b2 DIV 4-ab2+Da2;
    C0:=0; C1:=a*Db2;
    WHILE y <= t DO
      line(bmd,tool,xc+x,yc+y,xc-x,yc+y);
      line(bmd,tool,xc+x,yc-y,xc-x,yc-y);
      _ellipse(elipse)
    END
  END
END ellipse0f;

PROCEDURE polyline0(bmd: BITMAP; VAL tool: TOOL; SEQ xy: INTEGER);
BEGIN polyline1(bmd,tool,xy) END polyline0;

PROCEDURE polyline1(bmd: BITMAP; VAL tool: TOOL; xy: ARRAY OF INTEGER);
  VAR i,no: INTEGER;
BEGIN
  no:=(HIGH(xy)+1) DIV 2; i:=0;
  IF no=0 THEN dot(bmd,tool,xy[0],xy[1]); RETURN END;
  WHILE i <= no-2 DO
    line(bmd,tool,xy[i*2],xy[i*2+1],xy[i*2+2],xy[i*2+3]); INC(i)
  END;
END polyline1;

PROCEDURE trif(bmd: BITMAP; VAL tool: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);
  VAR trif1,trif2: TRIF;
       by,bx,yL: INTEGER;
BEGIN
  IF y1<y0 THEN
    by:=y0; bx:=x0; y0:=y1; x0:=x1; y1:=by; x1:=bx;
  END;
  IF y2<y0 THEN
    by:=y0; bx:=x0; y0:=y2; x0:=x2; y2:=by; x2:=bx;
  END;
  IF (x1-x0)*(y2-y0)>(x2-x0)*(y1-y0) THEN
    by:=y2; bx:=x2; y2:=y1; x2:=x1; y1:=by; x1:=bx;
  END;
  IF y1<y2 THEN yL:=y1 ELSE yL:=y2 END;
  WITH trif1 DO
    x:=x0; y:=y0; xn:=x; yn:=y; xl:=x1;
    Dx:=x1-x0; Dy:=y1-y0;
    IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=TRUE ELSE dx:=1; case:=FALSE END;
    IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
    IF Dx>=Dy THEN
      Gx:=TRUE;  co:=Dx DIV 2
    ELSE
      Gx:=FALSE; co:=Dy DIV 2
    END;
  END;
  WITH trif2 DO
    x:=x0; y:=y0; xn:=x; yn:=y; xl:=x2;
    Dx:=x2-x0; Dy:=y2-y0;
    IF Dx<0   THEN Dx:=-Dx; dx:=-1; case:=FALSE ELSE dx:=1; case:=TRUE END;
    IF Dy<0   THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
    IF Dx>=Dy THEN Gx:=TRUE;  co:=Dx DIV 2
    ELSE           Gx:=FALSE; co:=Dy DIV 2
    END
  END;
  LOOP
    IF trif1.dx<0 THEN _trif(trif1); END;
    IF trif2.dx>0 THEN _trif(trif2); END;
    line(bmd,tool,trif1.xn,trif1.yn,trif2.xn,trif1.yn);
    IF trif1.yn=yL THEN EXIT END;
    IF trif1.dx>0 THEN _trif(trif1); END;
    IF trif2.dx<0 THEN _trif(trif2); END;
  END;
  IF y1=y2 THEN RETURN END;
  IF yL=y1 THEN
    yL:=y2;
    WITH trif1 DO
      x:=x1; y:=y1; xn:=x; yn:=y; xl:=x2;
      Dx:=x2-x1; Dy:=y2-y1;
      IF Dx<0 THEN Dx:=-Dx; dx:=-1; case:=TRUE ELSE dx:=1; case:=FALSE END;
      IF Dy<0 THEN Dy:=-Dy; dy:=-1 ELSE dy:=1 END;
      IF Dx>=Dy THEN Gx:=TRUE;  co:=Dx DIV 2
      ELSE           Gx:=FALSE; co:=Dy DIV 2
      END;
      IF dx<0 THEN _trif(trif1) END;
    END;
  ELSE
    yL:=y1;
    WITH trif2 DO
      x:=x2; y:=y2; xn:=x; yn:=y; xl:=x1;
      Dx:=x1-x2; Dy:=y1-y2;
      IF Dx<0   THEN Dx:=-Dx; dx:=-1; case:=FALSE ELSE dx:=1; case:=TRUE END;
      IF Dy<0   THEN Dy:=-Dy; dy:=-1  ELSE dy:=1 END;
      IF Dx>=Dy THEN Gx:=TRUE;  co:=Dx DIV 2
      ELSE           Gx:=FALSE; co:=Dy DIV 2
      END;
      IF dx>0 THEN _trif(trif2) END
    END
  END;
  REPEAT
    _trif(trif1); _trif(trif2); line(bmd,tool,trif1.xn,trif1.yn,trif2.xn,trif1.yn)
  UNTIL trif1.yn=yL
END trif;

---------------------------- Pattern ---------------------------
                            ---------

PROCEDURE grid(bmd: BITMAP; VAL t: TOOL; b: BLOCK; xstep,ystep: INTEGER);
  VAR msk: BITSET;                      col: BITSET;
      X,Y: INTEGER;                  shiftX: INTEGER;
      i,j: INTEGER;                  shiftY: INTEGER;
      ptr: POINTER TO BITSET;     end,adr,l: ADDRESS;
     line: ADDRESS;                    bump: ARRAY [0..maxW DIV 32-1] OF WORD;
BEGIN
  IF (xstep<=0) OR (ystep<=0) THEN RETURN END;

  shiftX:=0;
  IF b.x<t.clip.x THEN
    shiftX:=(t.clip.x-b.x) MOD xstep;
    IF shiftX#0 THEN shiftX:=xstep-shiftX END;
    DEC(b.w,t.clip.x-b.x); b.x:=t.clip.x
  END;

  shiftY:=0;
  IF b.y<t.clip.y THEN
    shiftY:=(t.clip.y-b.y) MOD ystep;
    IF shiftY#0 THEN shiftY:=ystep-shiftY END;
    DEC(b.h,t.clip.y-b.y); b.y:=t.clip.y
  END;

  IF b.x+b.w>t.clip.x+t.clip.w THEN b.w:=t.clip.x+t.clip.w-b.x END;
  IF b.y+b.h>t.clip.y+t.clip.h THEN b.h:=t.clip.y+t.clip.h-b.y END;
  IF (b.w<=0) OR (b.h<=0) THEN RETURN END;
  line:=ADR(bump);
  line^:=0;
  i:=b.x+t.zX;
  j:=i+b.w;
  move(line+1,line,(j+31) DIV 32 - 1);
  i:=i+shiftX;
  msk:={i MOD 32};
  WHILE i<j DO
    ptr:=line+i DIV 32; ptr^:=ptr^+msk; msk:=msk<<(xstep MOD 32); i:=i+xstep
  END;

  Y  :=bmd^.WPL*(bmd^.H-1-(b.y+t.zY+shiftY));
  end:=bmd^.WPL*(bmd^.H  -(b.h+b.y+t.zY));
  X  :=b.x+t.zX;
  IF t.mode#rep THEN
    adr:=Y;
    WHILE adr>=end DO
      l:=ADR(bmd^.layers[0]);  msk:=t.mask*bmd^.mask*t.color;
      REPEAT
        IF msk*{0}#{} THEN
          msk:=msk-{0}; gbblt(t.mode,int(l^)+adr,X,line,X,b.w)
        END;
        INC(l); msk:=msk>>1
      UNTIL msk={};
      DEC(adr,bmd^.WPL*ystep)
    END
  ELSE
    adr:=Y;
    WHILE adr>=end DO
      l:=ADR(bmd^.layers[0]);
      msk:=t.mask*bmd^.mask;  col:=t.color;
      REPEAT
        IF msk*{0}#{} THEN msk:=msk-{0};
          gbblt(bic,int(l^)+adr,X,line,X,b.w);
          IF col*{0}#{} THEN gbblt(or,int(l^)+adr,X,line,X,b.w) END
        END;
        INC(l); msk:=msk>>1; col:=col>>1
      UNTIL msk={};
      DEC(adr,bmd^.WPL*ystep)
    END
  END
END grid;

PROCEDURE pattern(bmd: BITMAP;  VAL t: TOOL;  b: BLOCK;
                wp,hp: INTEGER; VAL p: ARRAY OF WORD);

  VAR wpl,X,Y,i: INTEGER;  msk, col: BITSET;
      l,wph,adr: ADDRESS;  end,line: ADDRESS;
         shiftX: INTEGER;    shiftY: INTEGER;
           bump: ARRAY [0..maxW DIV 32-1] OF WORD;

  PROCEDURE prepare_line_buffer;
    VAR i,j: INTEGER;  sou: ADDRESS;
  BEGIN
    sou:=ADR(p)+shiftY*wpl;  shiftY:=(shiftY+1) MOD hp;
    IF (wp MOD 32 = 0) & (shiftX MOD 32 = 0) THEN
      i:=shiftX DIV 32;
      j:=b.x DIV 32;
      move(line+j,sou+i,wpl-i);
      move(line+j+wpl-i,sou,i);
      move(line+j+wpl,line+j,(b.w+31) DIV 32 - wpl + 1)
    ELSE
      i:=b.x;
      WHILE i<X+b.w DO
        bmv(line,i,sou,shiftX,wp-shiftX); INC(i,wp-shiftX);
        bmv(line,i,sou,0,shiftX);         INC(i,shiftX)
      END
    END
  END prepare_line_buffer;

BEGIN
  line:=ADR(bump);
  wpl:=(wp+31) DIV 32;
  i  :=SIZE(p) DIV wpl;
  IF hp>i THEN hp:=i END;
  IF (hp<=0) OR (wpl<=0) THEN RETURN END;
  IF b.x<t.clip.x THEN  DEC(b.w,t.clip.x-b.x);  b.x:=t.clip.x  END;
  IF b.y<t.clip.y THEN  DEC(b.h,t.clip.y-b.y);  b.y:=t.clip.y  END;
  shiftX:=b.x MOD wp;
  shiftY:=b.y MOD hp;

  IF b.x+b.w>t.clip.x+t.clip.w THEN b.w:=t.clip.x+t.clip.w-b.x END;
  IF b.y+b.h>t.clip.y+t.clip.h THEN b.h:=t.clip.y+t.clip.h-b.y END;
  IF (b.w<=0) OR (b.h<=0) THEN RETURN END;

  wph:=bmd^.WPL*hp;

  Y  :=bmd^.WPL*(bmd^.H-1-(b.y+t.zY));
  end:=bmd^.WPL*(bmd^.H  -(b.h+b.y+t.zY));
  X  :=b.x+t.zX;

  IF t.mode=rep THEN
    FOR i:=0 TO hp-1 DO
      prepare_line_buffer;
      adr:=Y;
      WHILE adr>=end DO
        l:=ADR(bmd^.layers[0]);  msk:=t.mask*bmd^.mask;  col:=t.color;
        REPEAT
          IF msk*{0}#{}   THEN msk:=msk-{0};
            IF col*{0}#{} THEN bmv(int(l^)+adr,X,line  ,X,b.w)
            ELSE               bmv(int(l^)+adr,X,line00,X,b.w)
            END
          END;
          INC(l);  col:=col>>1;  msk:=msk>>1
        UNTIL msk={};
        DEC(adr,wph)
      END;
      Y:=Y-bmd^.WPL
    END
  ELSE
    FOR i:=0 TO hp-1 DO
      prepare_line_buffer;
      adr:=Y;
      WHILE adr>=end DO
        l:=ADR(bmd^.layers[0]);  msk:=t.mask*bmd^.mask*t.color;
        REPEAT
          IF msk*{0}#{} THEN
            msk:=msk-{0}; gbblt(t.mode,int(l^)+adr,X,line,X,b.w)
          END;
          INC(l); msk:=msk>>1
        UNTIL msk={};
        DEC(adr,wph)
      END;
      Y:=Y-bmd^.WPL
    END
  END
END pattern;

PROCEDURE offset(VAL b: BITMAP; x,y,l: INTEGER; VAR a: ADDRESS; VAR o: INTEGER);
BEGIN
  a:=b^.layers[l]+(b^.H-1-y)*b^.WPL+x DIV 32;  o:=x MOD 32
END offset;

---------------------------- Scrolls ---------------------------
                            ---------

PROCEDURE scroll(b: BITMAP; tool: TOOL; xs,ys: INTEGER);
  VAR t,f,l,a,i,row,bits: INTEGER; m: BITSET;
BEGIN
  xs:=-xs;  ys:=-ys;
  IF (xs=0) & (ys=0) OR (tool.mask*b^.mask={}) THEN RETURN END;
  IF (ABS(ys)<tool.clip.h) & (ABS(xs)<tool.clip.w) THEN
    l:=tool.clip.h-ABS(ys);  row:=b^.WPL*BITS(WORD);
    IF ys>=0 THEN
      t:=(b^.H-tool.zY-tool.clip.y-tool.clip.h)*row+tool.zX+tool.clip.x;
    ELSE
      t:=(b^.H-1-tool.clip.y-tool.zY)*row+tool.zX+tool.clip.x;  row:=-row
    END;
    f:=t+ys*ABS(row);
    bits:=tool.clip.w-ABS(xs);
    IF xs>0 THEN INC(t,xs) ELSE DEC(f,xs) END;
    REPEAT
      m:=tool.mask*b^.mask; i:=0;
      REPEAT
        IF m*{0}#{} THEN a:=b^.layers[i]; bmv(a,t,a,f,bits); m:=m-{0} END;
        INC(i); m:=m>>1
      UNTIL m={};
      INC(t,row);  INC(f,row);  l:=l-1
    UNTIL l=0
  ELSE
    xs:=tool.clip.w; ys:=tool.clip.h
  END;
  WITH tool.clip DO
    IF    ys>0  THEN rect(b,tool,x,y,x+w-1,y+ys-1);  INC(y,ys)
    ELSIF ys<0  THEN rect(b,tool,x,y+h-1,x+w-1,y+h+ys)
    END;
    DEC(h,ABS(ys));
    IF    xs>0  THEN rect(b,tool,x,y,x+xs-1,y+h-1)
    ELSIF xs<0  THEN rect(b,tool,x+w-1,y,x+w+xs,y+h-1)
    END
  END
END scroll;

----------------------------------------------------------------

PROCEDURE pwrite(B: BITMAP;       VAL T: TOOL;
               x,y: INTEGER;          F: FONT;
             VAL s: ARRAY OF CHAR;  p,l: INTEGER): INTEGER;

  PROCEDURE pchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff: INTEGER);    (*$<$W+*)
     VAR lay,C: ADDRESS;       ln,k,mode: INTEGER;
         m,f,b: BITSET;             bump: ARRAY [0..7] OF INTEGER;
  BEGIN
    lay:=ADR(B^.layers);       C:=F^.BASE;
    mode:=T.mode; m:=T.mask*B^.mask;  b:=T.back;  f:=T.color<<1;
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
                gbblt(xor,ADR(bump),0,C,k,cw);        k :=k+cW;
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
    UNTIL m={}
  END pchar;                                                    (*$>*)

  PROCEDURE rchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff: INTEGER);    (*$<$W+*)
     VAR lay,C: ADDRESS;          ln,k: INTEGER;
         m,f,b: BITSET;           bump: ARRAY [0..7] OF INTEGER;
  BEGIN
    lay:=ADR(B^.layers);  C:=F^.BASE;
    m:=T.mask*B^.mask;           b:=T.back;      f:=T.color<<1;
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
                bmv(lay^,ln,lineFF,0,maxw);
                gbblt(xor,lay^,ln,C,k,cw);    ln:=ln+bpl;  k:=k+cW
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
    UNTIL m={}
  END rchar;                                                    (*$>*)

  PROCEDURE cellX(p0,p3,cX,bpl: INTEGER);
     VAR lay,a: ADDRESS;
         m,f,b: BITSET;
     ln,k,mode: INTEGER;
  BEGIN
    IF cX<1 THEN RETURN END;
    lay:=ADR(B^.layers);
    m:=T.mask*B^.mask;  b:=T.back;  f:=T.color<<1;
    IF T.mode#rep THEN
      mode:=T.mode;
      REPEAT
        IF m*{0}#{} THEN
          m:=m-{0};
          ln:=p0;
          k:=INTEGER(f*{1})+INTEGER(b*{0});
          IF (k=1) OR (k=3) THEN
            REPEAT gbblt(mode,lay^,ln,lineFF,0,cX); ln:=ln+bpl UNTIL ln=p3
          END
        END;
        INC(lay); b:=b>>1; f:=f>>1; m:=m>>1
      UNTIL m={}
    ELSE
      REPEAT
        IF m*{0}#{} THEN
          m:=m-{0};
          ln:=p0;
          k:=INTEGER(f*{1})+INTEGER(b*{0});
          IF (k=1) OR (k=3) THEN a:=lineFF ELSE a:=line00 END;
          REPEAT bmv(lay^,ln,a,0,cX); ln:=ln+bpl UNTIL ln=p3
        END;
        INC(lay); b:=b>>1; f:=f>>1; m:=m>>1
      UNTIL m={}
    END
  END cellX;

  VAR p0y,p3y: INTEGER;          P: BOOLEAN;
     bpl,maxw: INTEGER;         pw: dfn.BTPTR;
    coff,xoff: INTEGER;         px: dfn.BTPTR;
    i,j,bH,tH: INTEGER;         ch: CHAR;
   w,cw,cX,cW: INTEGER;         tw: INTEGER;
  p0,p1,p2,p3: INTEGER;

BEGIN
  IF l=0  THEN RETURN x END;
  pw:=F^.propW;  px:=F^.propX;  P:=(pw#NIL);
  (* bH, tH  bottom, top over Height of Full Char Place *)
  IF y<T.clip.y THEN bH:=T.clip.y-y ELSE bH:=0 END;
  IF y+F^.H>T.clip.y+T.clip.h THEN tH:=y+F^.H-T.clip.y-T.clip.h ELSE tH:=0 END;
  bpl:=B^.WPL*BITS(WORD);
  tw :=T.clip.x+T.clip.w;
  (********  NOTE! Now y will be in LUC coordinate system! ******)

  y   :=B^.H-1-(y+T.zY);
  xoff:=x+T.zX;
  p0y :=(y-(F^.H-1)+tH);    p0:=xoff+p0y*bpl;
  p3y :=y+1-bH;             p3:=xoff+p3y*bpl;
  REPEAT
    ch:=s[p];  cW:=ORD(F^.cellW^[ch]);  coff:=F^.bases^[ch];
    cw:=cW;    cX:=ORD(F^.cellX^[ch]);
    w :=F^.W;
    IF P THEN
      i:=ORD(px^[ch]);    DEC(p0,i);  DEC(p3,i);
      w:=ORD(pw^[ch])+i;  DEC(x,i);   DEC(xoff,i)
    END;
    IF w=0 THEN w:=F^.space END;
    IF x<T.clip.x THEN   i:=T.clip.x-x;
      INC(x,i);  INC(p0,i);  DEC(cX,i);
      DEC(w,i);  INC(p3,i);  INC(xoff,i)
    END;
    IF    cX<0 THEN INC(cw,cX); DEC(coff,cX)
    ELSIF cX>0 THEN i:=cX;
      IF x+i>tw THEN i:=tw-x END;
      cellX(p0,p3,i,bpl);
      INC(x,cX);  INC(p0,cX);
      DEC(w,cX);  INC(p3,cX);  INC(xoff,cX)
    END;
    maxw:=w;
    IF cw>maxw THEN maxw:=cw END;
    j:=ORD(F^.cellY^[ch]);
    i:=y-j-ORD(F^.cellH^[ch])+1;
    j:=y-j+1;
    IF (j>=p0y) & (i<=p3y) THEN
      IF i>p0y THEN p1:=i*bpl+xoff ELSE p1:=p0; coff:=coff+(p0y-i)*cW END;
      IF j<p3y THEN p2:=j*bpl+xoff ELSE p2:=p3  END
    ELSE
      p1:=p0; p2:=p0
    END;
    IF x+maxw>tw THEN maxw:=tw-x END;
    IF x+cw  >tw THEN cw  :=tw-x END;
    IF T.mode=rep THEN  rchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff)
    ELSE                pchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff)
    END;
    INC(x,w); INC(xoff,w); INC(p0,w); INC(p3,w); INC(p); DEC(l)
  UNTIL (l=0) OR (x>tw);
  RETURN x
END pwrite;

PROCEDURE outoffclip(T: TOOL;  VAR   x: INTEGER; y: INTEGER;
                     F: FONT;  VAL   S: ARRAY OF CHAR;
                               VAR P,L: INTEGER): BOOLEAN;
  VAR pw,px,cx,cw: dfn.BTPTR;    const: BOOLEAN;
           i,w,cp: INTEGER;        W,H: INTEGER;
               ch: CHAR;
BEGIN
  px:=F^.propX;  pw:=F^.propW;
  cx:=F^.cellX;  cw:=F^.cellW;
  const:=(F^.state*dfn.prop={}) OR (pw=NIL);
  W:=T.clip.x+T.clip.w;
  H:=T.clip.y+T.clip.h;
  IF (x >= W) OR (y+F^.H <= T.clip.y) OR (y >= H) THEN
    IF const THEN x:=x+L*F^.W; RETURN TRUE END;
    FOR i:=P TO P+L-1 DO
      w:=ORD(pw^[S[i]]);
      IF w=0 THEN x:=x+F^.space ELSE x:=x+w END
    END;
    RETURN TRUE
  END;
  IF x>=T.clip.x THEN RETURN FALSE END;
  IF const THEN
    cp:=(T.clip.x-x) DIV F^.W;
    P :=P+cp;
    IF L<=cp THEN x:=x+L*F^.W;  L:=0;    RETURN TRUE
    ELSE          x:=x+cp*F^.W; L:=L-cp; RETURN FALSE
    END
  END;
  i:=x;
  REPEAT
    ch:=S[P];
    w:=ORD(pw^[ch]);
    IF w=0 THEN
      i:=x+F^.space;
      IF i>T.clip.x THEN RETURN FALSE END
    ELSE
      IF (i+ORD(pw^[ch])>T.clip.x) OR
         (i-ORD(px^[ch])+ORD(cx^[ch])+ORD(cw^[ch])>T.clip.x)
      THEN
        RETURN FALSE
      END;
      i:=x+ORD(pw^[ch])
    END;
    INC(P);  DEC(L);  x:=i
  UNTIL L=0;
  RETURN TRUE
END outoffclip;

VAR lineXX: ADDRESS;
    bumpXX: ARRAY [0..31] OF WORD;

PROCEDURE xwrite(B: BITMAP;  VAL t: TOOL;          x,y: INTEGER;
                 f: FONT;    VAL s: ARRAY OF CHAR; p,l: INTEGER): INTEGER;

  VAR ch: CHAR;               P: BOOLEAN;
     lay: ADDRESS;        px,pw: dfn.BTPTR;
   X,F,G: FONT;        XD,FD,GD: dfn.FNTD;
bH,tH,x1: INTEGER;   bump,m,c,b: BITSET;
    dx,w: INTEGER;   ln,beg,end: INTEGER;

BEGIN
  DEC(y,f^.bline);
  IF (l=0) OR (f^.BASE=NIL) OR outoffclip(t,x,y,f,s,p,l) THEN RETURN x END;
  IF f^.state*dfn.packed#{} THEN RETURN pwrite(B,t,x,y,f,s,p,l) END;

  F:=ADR(FD);   F^.W:=f^.W;        P:=f^.state*dfn.prop#{};
  G:=ADR(GD);   G^.BASE:=line00;  pw:=f^.propW;
  X:=ADR(XD);   X^.BASE:=lineXX;  px:=f^.propX;

  (* bH, tH  bottom, top over Height of Full Char Place *)

  IF y<t.clip.y THEN bH:=t.clip.y-y ELSE bH:=0 END;
  IF y+f^.H>t.clip.y+t.clip.h THEN tH:=y+f^.H-t.clip.y-t.clip.h ELSE tH:=0 END;

  (********  NOTE! Now y will be in LUC coordinate system! ******)

  y:=B^.H-1-(y+t.zY+f^.H-1-tH);

  F^.H:=f^.H-tH-bH;
  G^.H:=F^.H;
  X^.H:=F^.H;

  WHILE (l>0) & (x<t.clip.x+t.clip.w) DO
    ch:=s[p];
    IF  P   THEN dx:=ORD(pw^[ch]); w:=f^.W ELSE dx:=f^.W; w:=dx END;
    IF dx=0 THEN dx:=f^.space; w:=dx END;
    IF (ch<f^.fchar) OR (ch>f^.lchar) THEN F^.BASE:=G^.BASE;
    ELSE F^.BASE:=f^.BASE+(ORD(ch)-ORD(f^.fchar))*f^.H+tH
    END;
    IF x+w>t.clip.x+t.clip.w THEN w:=t.clip.x+t.clip.w-x END;
    F^.W:=w;   G^.W:=F^.W;
    m:=t.mask*B^.mask;     c:=t.color<<1;   b:=t.back;
    lay:=ADR(B^.layers);
    IF P THEN x1:=x-ORD(px^[ch]) ELSE x1:=x END;
    x:=x+dx;
    IF x1<t.clip.x THEN
      dx:=t.clip.x-x1;  X^.W:=F^.W-dx;
      x1:=t.clip.x;     G^.W:=G^.W-dx;
      bmv(X^.BASE,0,F^.BASE,dx,F^.H*32);
      x1:=x1+t.zX;
      REPEAT
        IF m*{0}#{} THEN
          m:=m-{0}; B^.BASE:=lay^;
          CASE INTEGER(c*{1})+INTEGER(b*{0}) OF
          |0: dch(t.mode  ,B,x1,y,G,0c)
          |1: dch(t.mode+4,B,x1,y,X,0c)
          |2: dch(t.mode  ,B,x1,y,X,0c)
          |3: dch(t.mode+4,B,x1,y,G,0c)
          END
        END;
        b:=b>>1; c:=c>>1; INC(lay); m:=m>>1
      UNTIL m={}
    ELSE
      x1:=x1+t.zX;
      REPEAT
        IF m*{0}#{} THEN
          m:=m-{0}; B^.BASE:=lay^;
          CASE INTEGER(c*{1})+INTEGER(b*{0}) OF
          |0: dch(t.mode  ,B,x1,y,G,0c)
          |1: dch(t.mode+4,B,x1,y,F,0c)
          |2: dch(t.mode  ,B,x1,y,F,0c)
          |3: dch(t.mode+4,B,x1,y,G,0c)
          END
        END;
        b:=b>>1; c:=c>>1; INC(lay); m:=m>>1
      UNTIL m={}
    END;
    INC(p); DEC(l)
  END;
  RETURN x
END xwrite;

PROCEDURE write(bmd: BITMAP; VAL t: TOOL;  x,y: INTEGER;
                fnt: FONT;   VAL s: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  IF xwrite(bmd,t,x,y,fnt,s,pos,len)#0 THEN END
END write;

TYPE
  WDTPTR  = POINTER TO WDTMRG;
  WDTMRG  = RECORD
              font : FONT;
              first: BOOLEAN;
              x,l  : INTEGER;
            END;

PROCEDURE _width(link: WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR ptr: WDTPTR; i: INTEGER; ch: CHAR;
BEGIN
  IF len<=0 THEN RETURN END;
  ptr:=link;
  WITH ptr^ DO
    IF first THEN  first:=FALSE; l:=0;  x:=ORD(font^.propX^[s[pos]])  END;
    REPEAT
      ch:=s[pos];
      i :=x+ORD(font^.cellX^[ch])+ORD(font^.cellW^[ch])-ORD(font^.propX^[ch]);
      IF i>l THEN l:=i END;
      i:=ORD(font^.propW^[ch]);
      IF i=0 THEN INC(x,font^.space) ELSE INC(x,i) END;
      INC(pos); DEC(len)
    UNTIL len=0
  END
END _width;

PROCEDURE width(font: FONT; f: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
  VAR str: WDTMRG;
BEGIN
  IF font^.state*dfn.prop={} THEN RETURN lenght(font,f,a) END;
  str.font :=font;  str.l:=0;
  str.first:=TRUE;  str.x:=0;
  fmt.format(ADR(str),_width,f,a);
  RETURN str.l
END width;

PROCEDURE _margin(link: WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR ptr: WDTPTR; pw,px,cw: INTEGER; ch: CHAR;
BEGIN
  ptr:=link;
  IF (NOT ptr^.first) OR (len<=0) THEN RETURN END;
  ptr^.l:=ORD(ptr^.font^.propX^[s[pos]]); ptr^.first:=FALSE
END _margin;

PROCEDURE margin(font: FONT; f: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
  VAR str: WDTMRG;
BEGIN
  IF font^.state*dfn.prop={} THEN RETURN 0 END;
  str.font :=font;  str.l:=0;
  str.first:=TRUE;  str.x:=0;
  fmt.format(ADR(str),_margin,f,a);
  RETURN str.l
END margin;

TYPE
  STRPTR  = POINTER TO STRREC;
  STRREC  = RECORD
              bmd : BITMAP;
              x,y : INTEGER;
              fnt : FONT;
              tool: POINTER TO TOOL;
            END;

PROCEDURE _len(link: WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR ptr: STRPTR; w: INTEGER;
BEGIN
  IF len<=0 THEN RETURN END;
  ptr:=link;
  WITH ptr^ DO
    IF fnt^.state*dfn.prop={} THEN INC(x,len*fnt^.W); RETURN END;
    REPEAT
      w:=ORD(fnt^.propW^[s[pos]]);
      IF w=0 THEN x:=x+fnt^.space ELSE x:=x+w END;
      INC(pos); DEC(len)
    UNTIL len=0
  END
END _len;

PROCEDURE lenght(VAL font: FONT; f: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
  VAR str: STRREC;
BEGIN
  str.x:=0;  str.fnt:=font;  fmt.format(ADR(str),_len,f,a);  RETURN str.x
END lenght;


PROCEDURE ws(link: WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR p: STRPTR;
BEGIN
  p:=link;  p^.x:=xwrite(p^.bmd,p^.tool^,p^.x,p^.y,p^.fnt,s,pos,len)
END ws;

PROCEDURE xprint(bmd: BITMAP; VAL tool: TOOL; x,y: INTEGER; fnt: FONT;

                VAL f: ARRAY OF CHAR; SEQ arg: WORD): INTEGER;
  VAR str: STRREC;
BEGIN
  str.bmd:=bmd; str.x:=x; str.y:=y; str.fnt:=fnt; str.tool:=ADR(tool);
  fmt.format(ADR(str),ws,f,arg);
  RETURN str.x
END xprint;

PROCEDURE print(bmd: BITMAP; VAL t: TOOL; x,y: INTEGER; fnt: FONT;
                VAL f: ARRAY OF CHAR; SEQ arg: WORD);
BEGIN
  x:=xprint(bmd,t,x,y,fnt,f,arg)
END print;

PROCEDURE writech(bmd: BITMAP; VAL t: TOOL; x,y: INTEGER; fnt: FONT; ch: CHAR);
  VAR p: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  p:=ADR(ch);
  x:=xwrite(bmd,t,x,y,fnt,p^,0,1)
END writech;

CONST h=HIGH(bump00);

BEGIN
  lineFF:=ADR(bumpFF);   bumpFF[0]:=-1;  move(lineFF+1,lineFF,h);
  line00:=ADR(bump00);   bump00[0]:= 0;  move(line00+1,line00,h);
  lineXX:=ADR(bumpXX)
END BMG.
