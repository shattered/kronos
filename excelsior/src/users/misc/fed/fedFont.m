IMPLEMENTATION MODULE fedFont; (* nick 13-Dec-90. (c) KRONOS *)

FROM SYSTEM  IMPORT ADR, ADDRESS,WORD;
IMPORT  defBMG,defScreen;       IMPORT  err: defErrors;
IMPORT  cod: defCodes;          IMPORT  low: lowLevel;
IMPORT  dfn: defFont;           IMPORT  fnt: Fonts;
IMPORT  mem: Heap;

IMPORT  tty: Terminal;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

CONST
  BPW=BITS(WORD);  fmagic=24666E74h;  (* "$fnt" *)

----------------- C o d e   P r o c e d u r e s ----------------
                 -------------------------------

PROCEDURE move(         s,d,s: INTEGER); CODE cod.move             END move;
PROCEDURE gbblt(m,d,do,s,so,n: INTEGER); CODE cod.bmg cod.bmg_bblt END gbblt;
PROCEDURE bmv(    d,do,s,so,n: INTEGER); CODE cod.bmv              END bmv;

-------------------------- E r r o r s -------------------------
                          -------------
PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;
PROCEDURE bad_desc;  BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;
PROCEDURE fnt_error; BEGIN done:=FALSE; error:=fnt.error    END fnt_error;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;

----------------- F o n t   D e f i n i t i o n ----------------
                 -------------------------------
TYPE
  Font = POINTER TO RECORD
                      magic: INTEGER;
                      font : fnt.FONT;
                      sizes: ARRAY CHAR OF INTEGER;
                      image: ARRAY CHAR OF ADDRESS;
                    END;

--------------------------- F o n t s --------------------------
                           -----------

PROCEDURE dispose(VAR f: Font);
  VAR c: CHAR;
BEGIN
  IF (f=null) OR (f^.magic#fmagic) THEN RETURN END;
  FOR c:=0c TO 377c DO
    IF f^.image[c]#NIL THEN mem.deallocate(f^.image[c],f^.sizes[c]) END
  END;
  fnt.dispose(f^.font);
  f^.magic:=0;
  DISPOSE(f)
END dispose;

PROCEDURE _new(VAR f: Font);
BEGIN
  NEW(f);
  IF NOT mem.done THEN mem_error; RETURN END;
  f^.magic:=fmagic;  low.zero(f^.sizes);
  f^.font :=NIL;     low.fill(f^.image,NIL);
END _new;

PROCEDURE new(VAR f: Font; w,h: INTEGER; fc,lc: CHAR; st: BITSET);
BEGIN
  IF (w*h<=0) OR (w>256) OR (h>256) OR (fc>lc) THEN bad_parm;  RETURN END;
  _new(f);
  IF NOT done THEN RETURN END;
  fnt.new(f^.font,w,h,fc,lc,st);
  IF NOT fnt.done THEN fnt_error; dispose(f) END
END new;

PROCEDURE read(VAR f: Font; name: ARRAY OF CHAR);
  VAR w,cx,px: INTEGER;
         prop: BOOLEAN;
            c: CHAR;
BEGIN
  done:=TRUE;
  _new(f);                IF NOT done THEN RETURN END;
  fnt.read(f^.font,name); IF NOT fnt.done THEN fnt_error; dispose(f); RETURN END;
  fnt.pack(f^.font);      IF NOT fnt.done THEN fnt_error; dispose(f); RETURN END;
  WITH f^.font^ DO
    prop:=state*fnt.prop#{};
    FOR c:=fchar TO lchar DO
      IF bases^[c]>=0 THEN
        IF prop THEN
          cx:=ORD(cellX^[c]);
          px:=ORD(propX^[c]);
          IF cx>px THEN cx:=cx-px; px:=0 ELSE px:=px-cx; cx:=0 END;
          cellX^[c]:=CHAR(cx);
          propX^[c]:=CHAR(px)
        END;
        w:=ORD(cellW^[c])*ORD(cellH^[c]);
        f^.sizes[c]:=(w+BPW-1) DIV BPW;
        mem.allocate(f^.image[c],f^.sizes[c]);
        IF NOT mem.done THEN mem_error; f^.sizes[c]:=0; dispose(f); RETURN END;
        low._zero(f^.image[c],f^.sizes[c]);
        bmv(f^.image[c],0,BASE,bases^[c],w)
      END
    END;
    low.fill(bases^,-1);
    mem.deallocate(BASE,size);  size:=0
  END
END read;

PROCEDURE save(f: Font; file_name: ARRAY OF CHAR);

  PROCEDURE eq(c0,c1: CHAR): BOOLEAN;
    VAR a,b: ADDRESS;
        i,w: INTEGER;
  BEGIN
    a:=f^.image[c0];   b:=f^.image[c1];
    w:=f^.sizes[c0];   i:=0;
    WHILE (i<w) & (a^=b^) DO INC(i); INC(a); INC(b) END;
    RETURN i=w
  END eq;

  VAR offs: ARRAY CHAR OF CHAR;
     ch,cc: CHAR;
    w,sz,o: INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  sz:=0;
  low.fill(offs,0);
  WITH f^.font^ DO
    FOR ch:=fchar TO lchar DO
      IF f^.image[ch]#NIL THEN
        cc:=fchar;
        LOOP
          IF cc=ch THEN
            offs[ch]:=ch; sz:=sz+ORD(cellW^[ch])*ORD(cellH^[ch]); EXIT
          END;
          IF (f^.image[cc]#NIL) &
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
      IF f^.image[ch]#NIL THEN
        IF offs[ch]=ch THEN
          sz:=ORD(cellW^[ch])*ORD(cellH^[ch]);
          bmv(BASE,o,f^.image[ch],0,sz);
          bases^[ch]:=o;
          o:=o+sz
        ELSE
          bases^[ch]:=bases^[offs[ch]]
        END
      END
    END;
    fnt.write(f^.font,file_name);  IF NOT fnt.done THEN fnt_error END;
    mem.deallocate(BASE,size);     size:=0
  END
END save;

PROCEDURE fontW!(f: Font; w: INTEGER; mode: BITSET);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
END fontW!;

PROCEDURE fontH!(f: Font; h: INTEGER; mode: BITSET);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
END fontH!;

PROCEDURE bline!(f: Font; y: INTEGER);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  done:=(y>0) & (y<f^.font^.H);
  IF NOT done THEN bad_parm; RETURN END;
  f^.font^.bline:=y
END bline!;

PROCEDURE uline!(f: Font; y: INTEGER);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  done:=(y>0) & (y<f^.font^.H);
  IF NOT done THEN bad_parm; RETURN END;
  f^.font^.uline:=y
END uline!;

PROCEDURE state!(f: Font; s: BITSET);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  f^.font^.state:=s+packed
END state!;

PROCEDURE fchar!(f: Font; c: CHAR);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  done:=(c<=f^.font^.lchar);
  IF NOT done THEN bad_parm; RETURN END;
  f^.font^.fchar:=c
END fchar!;

PROCEDURE lchar!(f: Font; c: CHAR);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  done:=(c>=f^.font^.fchar);
  IF NOT done THEN bad_parm; RETURN END;
  f^.font^.lchar:=c
END lchar!;

PROCEDURE fontW?(f: Font): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  RETURN f^.font^.W
END fontW?;

PROCEDURE fontH?(f: Font): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  RETURN f^.font^.H
END fontH?;

PROCEDURE bline?(f: Font): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  RETURN f^.font^.bline
END bline?;

PROCEDURE uline?(f: Font): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  RETURN f^.font^.uline
END uline?;

PROCEDURE state?(f: Font): BITSET;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN {} END;
  RETURN f^.font^.state
END state?;

PROCEDURE fchar?(f: Font): CHAR;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0c END;
  RETURN f^.font^.fchar
END fchar?;

PROCEDURE lchar?(f: Font): CHAR;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0c END;
  RETURN f^.font^.lchar
END lchar?;

PROCEDURE proportional(f: Font): BOOLEAN;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN FALSE END;
  RETURN f^.font^.state*prop#{}
END proportional;

-------------------------- Characters --------------------------
                          ------------

PROCEDURE readchar(f: Font; bmd: defBMG.BITMAP; c: CHAR);
  VAR i,w,h: INTEGER;   co,bo: INTEGER;
     cw,bpl: INTEGER;   bs,ch: ADDRESS;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  IF bmd=NIL THEN RETURN END;
  bs :=bmd^.BASE;
  bpl:=bmd^.WPL*BPW;
  low._zero(bs,bmd^.H*bmd^.WPL);
  IF f^.image[c]=NIL THEN RETURN END;
  ch:=f^.image[c];  co:=0;
  WITH f^.font^ DO
    w:=ORD(cellW^[c]);  bo:=ORD(cellX^[c]);
    h:=ORD(cellH^[c]);  cw:=w;
    IF bo   >=bmd^.W THEN RETURN END;
    IF bo+cw>=bmd^.W THEN cw:=bmd^.W-bo-cw END;
    IF H    > bmd^.H THEN co:=(H-bmd^.H)*w;
    ELSE      co:=0;      bo:=bo+(bmd^.H-ORD(cellY^[c])-h)*bpl
    END
  END;
  FOR i:=0 TO h-1 DO
    bmv(bs,bo,ch,co,w); INC(co,w); INC(bo,bpl)
  END
END readchar;

PROCEDURE savechar(f: Font; bmd: defBMG.BITMAP; ch: CHAR);
  VAR wpl: INTEGER;

  PROCEDURE str(ln: ADDRESS): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO wpl-1 DO
      IF ln^#0 THEN RETURN FALSE END;
      INC(ln)
    END;
    RETURN TRUE
  END str;

  VAR i,j,W,H: INTEGER;
        w,h,s: INTEGER;
        y0,y1: INTEGER;
        x0,x1: INTEGER;
        m0,m1: BITSET;
      ln,base: ADDRESS;
          sum: ARRAY [0..(256+BPW-1) DIV BPW] OF BITSET;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  IF bmd=NIL THEN RETURN END;
  W:=f^.font^.W;  wpl:=bmd^.WPL;
  H:=f^.font^.H;  ln :=bmd^.BASE+(bmd^.H-H)*wpl;
  low.zero(sum);
  y0:=-1;
  y1:=-1;
  FOR i:=0 TO H-1 DO
    IF NOT str(ln) THEN
      gbblt(defScreen.or,ADR(sum),0,ln,0,W);
      IF y0<0 THEN y0:=i END;
      IF y1<i THEN y1:=i END;
    END;
    DEC(ln,wpl)
  END;
  IF (y0<0) OR (y1<0) THEN
    IF f^.image[ch]=0 THEN RETURN END;
    mem.deallocate(f^.image[ch],f^.sizes[ch]);  f^.sizes[ch]:=0;
    WITH f^.font^ DO
      cellX^[ch]:=0c;  cellY^[ch]:=0c;
      cellW^[ch]:=0c;  cellH^[ch]:=0c
    END;
    RETURN
  END;
  i:=0;            x0:=0;       WHILE sum[i]={} DO            INC(i)  END;
  m0:={0};         m1:=sum[i];  WHILE m1*m0 ={} DO m0:=m0<<1; INC(x0) END;
  i:=W DIV BPW;    x1:=W;       WHILE sum[i]={} DO            DEC(i)  END;
  m0:={W MOD 32};  m1:=sum[i];  WHILE m1*m0 ={} DO m0:=m0>>1; DEC(x1) END;
  w:=x1-x0;
  h:=y1-y0;
  s:=(w*h+BPW-1) DIV BPW;
  IF s#f^.sizes[ch] THEN
    mem.deallocate(f^.image[ch],f^.sizes[ch]);  f^.sizes[ch]:=s;
    mem.allocate(f^.image[ch],s);
    IF NOT mem.done THEN mem_error; f^.sizes[ch]:=0; RETURN END
  END;
  low._zero(f^.image[ch],f^.sizes[ch]);
  WITH f^.font^ DO
    cellX^[ch]:=CHAR(x0);  cellW^[ch]:=CHAR(w);
    cellY^[ch]:=CHAR(y0);  cellH^[ch]:=CHAR(h)
  END;
  ln:=bmd^.BASE+(bmd^.H-H-y1-1)*wpl;
  j:=0;
  FOR i:=0 TO h-1 DO
    bmv(f^.image[ch],j,ln,x0,w); INC(j,w); INC(ln,wpl)
  END
END savechar;

VAR line00: ADDRESS;  bump00: ARRAY [0..7] OF WORD;
    lineFF: ADDRESS;  bumpFF: ARRAY [0..7] OF WORD;

PROCEDURE cellx!(f: Font; c: CHAR; x: INTEGER);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  f^.font^.cellX^[c]:=CHAR(x)
END cellx!;

PROCEDURE propx!(f: Font; c: CHAR; px: INTEGER);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  IF f^.font^.state*prop={} THEN RETURN END;
  f^.font^.propX^[c]:=CHAR(px)
END propx!;

PROCEDURE propw!(f: Font; c: CHAR; pw: INTEGER);
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN END;
  IF f^.font^.state*prop={} THEN RETURN END;
  f^.font^.propW^[c]:=CHAR(pw)
END propw!;

PROCEDURE cellx?(f: Font; c: CHAR): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  RETURN ORD(f^.font^.cellX^[c])
END cellx?;

PROCEDURE propx?(f: Font; c: CHAR): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  IF f^.font^.state*prop={} THEN RETURN 0 END;
  RETURN ORD(f^.font^.propX^[c])
END propx?;

PROCEDURE propw?(f: Font; c: CHAR): INTEGER;
BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN 0 END;
  IF f^.font^.state*prop={} THEN RETURN f^.font^.W END;
  RETURN ORD(f^.font^.propW^[c])
END propw?;


PROCEDURE outoffclip(T: defScreen.TOOL;  VAR   x: INTEGER; y: INTEGER;
                     F: fnt.FONT;        VAL   S: ARRAY OF CHAR;
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

PROCEDURE xwrite(B: defBMG.BITMAP; VAL T: defScreen.TOOL;
               x,y: INTEGER;           f: Font;
             VAL s: ARRAY OF CHAR;   p,l: INTEGER): INTEGER;

  VAR cadr: ADDRESS;

  PROCEDURE pchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff: INTEGER);    (*$<$W+*)
     VAR lay,C: ADDRESS;       ln,k,mode: INTEGER;
         m,f,b: BITSET;             bump: ARRAY [0..7] OF INTEGER;
  BEGIN
    lay:=ADR(B^.layers);              C:=cadr;
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
                gbblt(defScreen.xor,ADR(bump),0,C,k,cw);  k :=k+cW;
                gbblt(mode,lay^,ln,ADR(bump),0,maxw);     ln:=ln+bpl
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
    lay:=ADR(B^.layers);  C:=cadr;
    m:=T.mask*B^.mask;    b:=T.back;      f:=T.color<<1;
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
                gbblt(defScreen.xor,lay^,ln,C,k,cw); ln:=ln+bpl; k:=k+cW
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
    IF T.mode#defScreen.rep THEN
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
     bpl,maxw: INTEGER;          F: dfn.FONT;
    coff,xoff: INTEGER;         pw: dfn.BTPTR;
    i,j,bH,tH: INTEGER;         px: dfn.BTPTR;
   w,cw,cX,cW: INTEGER;         ch: CHAR;
  p0,p1,p2,p3: INTEGER;

BEGIN
  done:=(f#null) & (f^.magic=fmagic) & (f^.font#NIL);
  IF NOT done THEN bad_desc; RETURN x END;
  F:=f^.font;
  DEC(y,F^.bline);
  IF (l=0) OR outoffclip(T,x,y,f^.font,s,p,l) THEN RETURN x END;
  pw:=F^.propW;
  px:=F^.propX;
  P:=(pw#NIL);
  (* bH, tH  bottom, top over Height of Full Char Place *)
  IF y<T.clip.y THEN bH:=T.clip.y-y ELSE bH:=0 END;
  IF y+F^.H>T.clip.y+T.clip.h THEN tH:=y+F^.H-T.clip.y-T.clip.h ELSE tH:=0 END;
  bpl:=B^.WPL*BITS(WORD);

  (********  NOTE! Now y will be in LUC coordinate system! ******)

  y   :=B^.H-1-(y+T.zY);
  xoff:=x+T.zX;
  p0y :=(y-(F^.H-1)+tH);    p0:=xoff+p0y*bpl;
  p3y :=y+1-bH;             p3:=xoff+p3y*bpl;
  REPEAT
    ch:=s[p];  cW:=ORD(F^.cellW^[ch]);  coff:=0;
    cw:=cW;    cX:=ORD(F^.cellX^[ch]);  cadr:=f^.image[ch];
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
      IF x+i>T.clip.x+T.clip.w THEN i:=T.clip.x+T.clip.w-x END;
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
    IF x+maxw>T.clip.x+T.clip.w THEN maxw:=T.clip.x+T.clip.w-x END;
    IF x+cw  >T.clip.x+T.clip.w THEN cw  :=T.clip.x+T.clip.w-x END;
    IF T.mode=defScreen.rep THEN  rchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff)
    ELSE                          pchar(p0,p1,p2,p3,cw,cW,maxw,bpl,coff)
    END;
    INC(x,w); INC(xoff,w); INC(p0,w); INC(p3,w); INC(p); DEC(l)
  UNTIL (l=0) OR (x>T.clip.x+T.clip.w);
  RETURN x
END xwrite;

BEGIN
  null  :=NIL;
  line00:=ADR(bump00); low.zero(bump00);
  lineFF:=ADR(bumpFF); low.fill(bumpFF,-1)
END fedFont.
