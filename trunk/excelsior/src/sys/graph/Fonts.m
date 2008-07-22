IMPLEMENTATION MODULE Fonts;  (*$N+ Leo 27-Jan-91 *)
                              (*   nick 22-May-91 *)

FROM  SYSTEM IMPORT  ADR, ADDRESS, WORD;
IMPORT  def: defFont;           IMPORT  low: lowLevel;
IMPORT  err: defErrors;         IMPORT  bio: BIO;
IMPORT  cod: defCodes;          IMPORT  mem: Heap;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

PROCEDURE bmv(t,tofs,f,fofs,bits: INTEGER); CODE cod.bmv END bmv;

CONST
  MAGIC = 746E6F66h;  (* "font" *)
 fMAGIC = 544E4F46h;  (* "FONT" *)

PROCEDURE bio_error; BEGIN done:=FALSE; error:=bio.error    END bio_error;
PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;
PROCEDURE bad_parm ; BEGIN done:=FALSE; error:=err.bad_parm END bad_parm;
PROCEDURE bad_desc ; BEGIN done:=FALSE; error:=err.bad_desc END bad_desc;

PROCEDURE new(VAR fnt: FONT; w,h: INTEGER; f,l: CHAR; s: BITSET);

  PROCEDURE undo;
  BEGIN
    WITH fnt^ DO
      mem.deallocate(BASE,size);  DISPOSE(bases);
      DISPOSE(propW);             DISPOSE(cellX);
      DISPOSE(propX);             DISPOSE(cellY);
      DISPOSE(cellW);             DISPOSE(cellH)
    END;
    DISPOSE(fnt)
  END undo;

  VAR i: INTEGER;  p: BITSET;

BEGIN
  fnt:=NIL;
  IF (w<1) OR (h<1)                     THEN bad_parm; RETURN END;
  IF (w>256) OR (h>256) OR (f>l)        THEN bad_parm; RETURN END;
  IF ((w>32) OR (h>32)) & (s*packed={}) THEN bad_parm; RETURN END;
  done:=TRUE;
  NEW(fnt);
  IF NOT mem.done THEN mem_error; RETURN END;
  WITH fnt^ DO
    W    :=w;    H    :=h;
    state:=s;    bline:=0;
    size :=0;    uline:=0;
    BASE :=NIL;  bases:=NIL;
    propW:=NIL;  cellY:=NIL;
    propX:=NIL;  cellX:=NIL;
    cellH:=NIL;  cellW:=NIL;
    IF s*prop#{} THEN
      NEW(propW);
      IF NOT mem.done THEN mem_error; undo; RETURN END;
      low.zero(propW^);
      NEW(propX);
      IF NOT mem.done THEN mem_error; undo; RETURN END;
      low.zero(propX^)
    END;
    IF s*packed={} THEN
      size:=((w+31) DIV 32)*h*(ORD(l)-ORD(f)+1);
      mem.allocate(BASE,size);
      IF NOT mem.done THEN mem_error; undo; RETURN END;
      low._zero(BASE,size)
    ELSE (* packed *)
      NEW(cellW);        IF NOT mem.done THEN mem_error; undo; RETURN END;
      NEW(cellH);        IF NOT mem.done THEN mem_error; undo; RETURN END;
      NEW(cellY);        IF NOT mem.done THEN mem_error; undo; RETURN END;
      NEW(cellX);        IF NOT mem.done THEN mem_error; undo; RETURN END;
      NEW(bases);        IF NOT mem.done THEN mem_error; undo; RETURN END;
      low.zero(cellW^);  low.zero(cellH^);
      low.zero(cellY^);  low.zero(cellX^);
      low.fill(bases^,-1)
    END;
    fchar:=f;   state:=s;
    lchar:=l;   magic:=MAGIC;
    IF prop*state#{} THEN space:=W DIV 2 ELSE space:=W END
  END
END new;

PROCEDURE dispose(VAR fnt: FONT);
BEGIN
  IF (fnt=NIL) OR (fnt=font) OR (fnt^.magic#MAGIC) THEN fnt:=NIL; RETURN END;
  WITH fnt^ DO
    IF prop*state#{} THEN DISPOSE(propW); DISPOSE(propX) END;
    mem.deallocate(BASE,size);
    IF packed*state#{} THEN
      DISPOSE(cellH);   DISPOSE(cellW);
      DISPOSE(cellY);   DISPOSE(cellX);
      DISPOSE(bases)
    END;
    magic:=0
  END;  DISPOSE(fnt)
END dispose;

PROCEDURE read(VAR fnt: FONT; file_name: ARRAY OF CHAR);

  VAR f: bio.FILE;
      d: bio.FILE;
   path: bio.PATHs;

  PROCEDURE undo;
  BEGIN
    dispose(fnt);  bio.close(f); bio.close(d); bio.close_paths(path)
  END undo;

  PROCEDURE _read;

    PROCEDURE geth(VAR w: WORD);
    BEGIN
      bio.read(f,ADR(w),4);
      IF NOT bio.done THEN bio_error END
    END geth;
  
    PROCEDURE get(VAR ptr: ADDRESS; ofs,len,size: INTEGER; fill: INTEGER);
    BEGIN
      mem.allocate(ptr,size);
      IF NOT mem.done THEN mem_error; RETURN END;
      low._fill(ptr,size,fill);
      len:=(len+3) DIV 4*4;
      bio.fread(f,ptr,ofs,len);
      IF NOT bio.done THEN bio_error; RETURN END
    END get;

    VAR i,ofs,len: INTEGER;

  BEGIN
    bio.get_paths(path,"FNT");
    IF NOT bio.done THEN path:=bio.here END;
    bio.lookup(path,f,file_name,"r");
    IF NOT bio.done  THEN bio_error; RETURN END;
    bio.close_paths(path);
    IF NOT bio.done THEN bio_error;  RETURN END;
    bio.buffers(f,1,4096);
    NEW(fnt);
    IF NOT mem.done THEN mem_error;  RETURN END;
    WITH fnt^ DO
      W    :=0;    H    :=0;
      state:={};   bline:=0;
      size :=0;    uline:=0;
      BASE :=NIL;  bases:=NIL;
      propW:=NIL;  cellY:=NIL;
      propX:=NIL;  cellX:=NIL;
      cellH:=NIL;  cellW:=NIL;
      magic:=MAGIC;
      geth(i);     IF NOT done THEN RETURN END;
      IF i#fMAGIC  THEN bad_desc;   RETURN END;
      geth(state); IF NOT done THEN RETURN END;
      geth(W);     IF NOT done THEN RETURN END;
      geth(H);     IF NOT done THEN RETURN END;
      geth(fchar); IF NOT done THEN RETURN END;
      geth(lchar); IF NOT done THEN RETURN END;
      geth(bline); IF NOT done THEN RETURN END;
      geth(uline); IF NOT done THEN RETURN END;
      geth(size);  IF NOT done THEN RETURN END;
      geth(space); IF NOT done THEN RETURN END;
      ofs:=ORD(fchar);
      len:=ORD(lchar)-ofs+1;
      IF state*packed#{} THEN
        get(cellW,ofs,len,SIZE(cellW^),0);      IF NOT done THEN RETURN END;
        get(cellH,ofs,len,SIZE(cellH^),0);      IF NOT done THEN RETURN END;
        get(cellY,ofs,len,SIZE(cellY^),0);      IF NOT done THEN RETURN END;
        get(cellX,ofs,len,SIZE(cellX^),0);      IF NOT done THEN RETURN END;
        get(bases,ofs*4,len*4,SIZE(bases^),-1); IF NOT done THEN undo; RETURN END;
      END;
      get(BASE,0,size*4,size,0);                IF NOT done THEN RETURN END;
      IF state*prop#{} THEN
        get(propW,ofs,len,SIZE(propW^),0);      IF NOT done THEN RETURN END;
        get(propX,ofs,len,SIZE(propW^),0);      IF NOT done THEN RETURN END;
      END;
      IF prop*state#{} THEN space:=W DIV 2 ELSE space:=W END
    END;
    bio.close(f)
  END _read;

BEGIN
  done:=TRUE;
  fnt:=NIL; f:=bio.null; d:=bio.null; path:=bio.empty;
  _read;
  IF NOT done THEN undo END;
END read;

PROCEDURE write(fnt: FONT; file_name: ARRAY OF CHAR);

  VAR f: bio.FILE;

  PROCEDURE puth(SEQ w: WORD);
  BEGIN
    bio.put(f,w,SIZE(w)*4);
    IF bio.done THEN RETURN END;
    bio_error; bio.purge(f)
  END puth;

  PROCEDURE put(ptr: ADDRESS; ofs,len: INTEGER);
  BEGIN
    len:=(len+3) DIV 4 * 4;
    bio.fwrite(f,ptr,ofs,len);
    IF bio.done THEN RETURN END;
    bio_error; bio.purge(f)
  END put;

  VAR len,ofs: INTEGER;

BEGIN
  IF (fnt=NIL) OR (fnt^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  bio.create(f,file_name,"w",fnt^.size*4);
  IF NOT bio.done THEN bio_error; RETURN END;
  bio.buffers(f,1,4096);
  WITH fnt^ DO
    puth(fMAGIC,state,W,H,fchar,lchar,bline,uline,size,space);
    IF NOT done THEN RETURN END;
    ofs:=ORD(fchar);
    len:=ORD(lchar)-ofs+1;
    IF state*packed#{} THEN
      put(cellW,ofs,len);       IF NOT done THEN RETURN END;
      put(cellH,ofs,len);       IF NOT done THEN RETURN END;
      put(cellY,ofs,len);       IF NOT done THEN RETURN END;
      put(cellX,ofs,len);       IF NOT done THEN RETURN END;
      put(bases,ofs*4,len*4);   IF NOT done THEN RETURN END;
    END;
    put(BASE,0,size*4);         IF NOT done THEN RETURN END;
    IF state*prop#{} THEN
      put(propW,ofs,len);       IF NOT done THEN RETURN END;
      put(propX,ofs,len);       IF NOT done THEN RETURN END
    END
  END;
  bio.close(f);
  IF NOT bio.done THEN bio_error; bio.purge(f) END
END write;

PROCEDURE pack(f: FONT);

  VAR  i,k: INTEGER;
       a,b: ADDRESS;
       equ: BOOLEAN;
      m,mw: BITSET;
      body: DYNARR OF INTEGER;
      ch,c: CHAR;
     fc,lc: CHAR;
     h0,h1: INTEGER;
   p,x,w,h: INTEGER;
     b0,b1: ARRAY [0..256 DIV 8] OF CHAR;

  PROCEDURE undo;
  BEGIN
    DISPOSE(f^.bases);  DISPOSE(f^.cellH);   DISPOSE(f^.cellX);
    DISPOSE(f^.cellY);  DISPOSE(f^.cellW);   DISPOSE(body)
  END undo;

BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF f^.state*packed#{}          THEN           RETURN END;
  IF (f^.W>32) OR (f^.H>32)      THEN bad_parm; RETURN END;
  p:=0;
  NEW(body);            f^.cellX:=NIL;
  f^.bases:=NIL;        f^.cellH:=NIL;
  f^.cellY:=NIL;        f^.cellW:=NIL;
  NEW(f^.bases);        IF NOT mem.done THEN mem_error; undo; RETURN END;
  NEW(f^.cellY);        IF NOT mem.done THEN mem_error; undo; RETURN END;
  NEW(f^.cellX);        IF NOT mem.done THEN mem_error; undo; RETURN END;
  NEW(f^.cellH);        IF NOT mem.done THEN mem_error; undo; RETURN END;
  NEW(f^.cellW);        IF NOT mem.done THEN mem_error; undo; RETURN END;
  RESIZE(body,f^.size); IF NOT mem.done THEN mem_error; undo; RETURN END;
  low.zero(f^.cellW^);  low.zero(f^.cellY^);
  low.zero(f^.cellH^);  low.zero(f^.cellX^);
  low.zero(body);       low.fill(f^.bases^,-1);
  fc:=f^.lchar;   lc:=f^.fchar;
  FOR ch:=f^.fchar TO f^.lchar DO
    b:=f^.BASE+(ORD(ch)-ORD(f^.fchar))*f^.H;
    IF (prop+italic)*f^.state=prop THEN
      w:=ORD(f^.propW^[ch]);
      IF w>32 THEN m:={0..31} ELSIF w>0 THEN m:={0..w-1} ELSE m:={} END
    ELSE
      m:={0..f^.W-1}
    END;
    mw:={}; h0:=-1; a:=b;
    FOR i:=0 TO f^.H-1 DO
      IF (h0<0) & (BITSET(a^)*m#{}) THEN h0:=i END;
      mw:=mw+BITSET(a^)*m; INC(a)
    END;
    IF h0<0 THEN
      IF prop*f^.state#{} THEN f^.propW^[ch]:=0c END
    ELSE
      h1:=f^.H; a:=b+f^.H;
      REPEAT DEC(a); DEC(h1) UNTIL BITSET(a^)*m#{};
      h:=h1-h0+1;
      w:=f^.W;
      x:=0;
      WHILE (x<w) & (mw*{ x }={}) DO INC(x) END;
      WHILE (w>0) & (mw*{w-1}={}) DO DEC(w) END;
      IF ch<fc THEN fc:=ch END;      DEC(w,x);
      f^.cellX^[ch]:=CHAR(x);           f^.cellW^[ch]:=CHAR(w);
      f^.cellY^[ch]:=CHAR(f^.H-1-h1);   f^.cellH^[ch]:=CHAR(h);
      c:=f^.fchar;   lc:=ch;
      LOOP
        IF c>=ch THEN equ:=FALSE; EXIT END;
        IF (f^.cellW^[c]=CHAR(w)) & (f^.cellH^[c]=CHAR(h)) THEN
          k:=f^.bases^[c];
          i:=h0;  equ:=TRUE;  a:=b+h0;
          WHILE (i<=h1) & equ DO
            low.zero(b0);  bmv(ADR(b0),0,ADR(body),k,w);
            low.zero(b1);  bmv(ADR(b1),0,a,0,w);
            equ:=(b0=b1);  INC(a);  INC(k,w);  INC(i)
          END;
          IF equ THEN EXIT END
        END;
        INC(c)
      END;
      IF equ THEN
        f^.bases^[ch]:=f^.bases^[c]
      ELSE
        IF p+w*h>SIZE(body)*32 THEN
          RESIZE(body,(p+w*h+31) DIV 32 + 16);
          IF NOT mem.done THEN mem_error; undo; RETURN END
        END;
        f^.bases^[ch]:=p;  a:=b+h0; i:=a+h;
        REPEAT bmv(ADR(body),p,a,x,w); INC(p,w); a:=a+1 UNTIL a=i
      END
    END
  END;
  RESIZE(body,(p+31) DIV 32);
  IF NOT mem.done THEN mem_error; undo; RETURN END;
  mem.deallocate(f^.BASE,f^.size);
  ASSERT(fc<=lc);
  f^.fchar:=fc;  f^.size:=SIZE(body);
  f^.lchar:=lc;  f^.BASE:=ADR(body); f^.state:=f^.state+packed
END pack;

PROCEDURE unpack(f: FONT);
  VAR   ch: CHAR;
    x,size: INTEGER;
   w,h,y,b: INTEGER;
  a,e,base: ADDRESS;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN bad_desc; RETURN END;
  done:=TRUE;
  IF f^.state*packed={}     THEN RETURN END;
  IF (f^.W>32) OR (f^.H>32) THEN RETURN END;
  size:=(ORD(f^.lchar)-ORD(f^.fchar)+1)*f^.H;
  mem.allocate(base,size);
  IF NOT mem.done THEN mem_error; RETURN END;
  low._zero(base,size);
  a:=base;
  FOR ch:=f^.fchar TO f^.lchar DO
    x:=ORD(f^.cellX^[ch]);  w:=ORD(f^.cellW^[ch]); b:=f^.bases^[ch];
    y:=ORD(f^.cellY^[ch]);  h:=ORD(f^.cellH^[ch]); INC(a,f^.H-h-y);
    IF h>0 THEN e:=a+h;
      REPEAT bmv(a,x,f^.BASE,b,w); INC(b,w); a:=a+1 UNTIL a=e
    ELSIF (prop*f^.state#{}) & (w=0) THEN f^.propW^[ch]:=0c (* space *)
    END;
    INC(a,y)
  END;
  ASSERT(a=base+size);
  IF f#font THEN DISPOSE(f^.bases); mem.deallocate(f^.BASE,f^.size) END;
  f^.state:=f^.state-packed;
  f^.BASE :=base;    f^.size :=size
END unpack;

PROCEDURE copy(f: FONT; to: CHAR; s: FONT; from: CHAR);
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN bad_desc; RETURN END;
  IF (s=NIL) OR (s^.magic#MAGIC) THEN bad_desc; RETURN END;
  ASSERT(FALSE);
END copy;

VAR DEFAULT: def.FNTD;

CONST _BASE = ARRAY OF INTEGER {
  00064A526h,0664A5260h,000064A52h,026E210C4h,0600064A5h,0526F1112h,02600064Ah,
  0A5266491h,0D2900064h,04A526843h,01C2F0006h,064A52674h,049C2E000h,04A538846h,
  011C44FEDh,000710862h,0264A5260h,000071086h,062E210C4h,060007108h,0862F1112h,
  026000710h,008626491h,0D2900071h,010862843h,01C2F0007h,071086274h,049C2E000h,
  007108626h,021110F00h,000F11126h,0664A5260h,0000F1112h,026E210C4h,06000F111h,
  0126F1112h,007000F11h,011267419h,0D29000F1h,011126843h,01C2F000Fh,0F1112674h,
  049C2E000h,00F111266h,021110F00h,000649126h,0664A5260h,000064912h,026E210C4h,
  060006491h,0126F1112h,026000649h,091266491h,0D2900064h,049126843h,01C2F0006h,
  064912674h,049C2E000h,006491266h,021110F00h,054024924h,0F52BEA55h,014B57295h,
  052756947h,094884422h,054452944h,04292D54Ah,042111112h,024444421h,04513E451h,
  09084F908h,024488CCFh,0CC62E112h,0C8E8C675h,0DC888888h,008990845h,02108BBE1h,
  0A63A3083h,0211F4A52h,0107843F4h,00C5CE8C2h,0DD18C62Fh,042211087h,046317084h,
  02E746317h,08C21E8C6h,04448048Eh,024244000h,0807C2109h,044410417h,022108B84h,
  0B1710002h,0F043DAD7h,063F8C62Eh,018C5F18Ch,0B9F18C5Fh,021084218h,08C6317BAh,
  043F7C631h,0F0842F08h,085E1087Fh,0218B8210h,08BA31E84h,0631FC631h,042109F8Ch,
  023DF2108h,0C9421084h,028CA98C4h,04210C629h,01F842108h,0C635AEF7h,0B59C6318h,
  08BA31CD6h,0A318C631h,017C6317Bh,0C62E0842h,0164D6318h,02BE318BEh,043174629h,
  0F7461070h,010842109h,0318C6242h,08C5D18C6h,008A54631h,05AD6B589h,02A31556Bh,
  0318A9445h,02108A546h,010887C84h,0267C2211h,00E222222h,010820841h,088888C84h,
  024544C88h,0BC820742h,078437228h,09F18C631h,0E884218Bh,0C631F420h,0F18BBD18h,
  02130E087h,00842109Eh,007463174h,0BC217461h,0918C6318h,088888C08h,0111C111Ch,
  0842D3111h,02928CA98h,01111111Ah,0D6B5ABF9h,0318C5F5Ah,08C62E8C6h,06317BA31h,
  0E085F18Ch,07A318C63h,0210CDB08h,07062E084h,03C423A30h,01C108421h,0BA318C63h,
  08452A318h,0AD6B5AC4h,05445462Ah,0E8C6318Ch,043EE8C21h,048F84444h,048444244h,
  021492492h,011222422h,0FFFFFD15h,0FFFFFFFFh,04A5294A1h,05294A529h,02D0B428Ah,
  00B42D0B4h,09926542Dh,026499264h,049226499h,092492492h,02493C924h,079E49249h,
  055552492h,055555755h,055555F55h,02493CF55h,08BAAAAA9h,0AAAAAAABh,0AAAAAAAAh,
  0B8FAAAAAh,0AAAAAAAAh,0AAF8BAAAh,0924FAAAAh,024927F3Ch,041041049h,010413C10h,
  03FFC4104h,041041041h,010410410h,00413C104h,0F1041041h,010410413h,004104FC4h,
  041041041h,013C10410h,00410413Ch,0A28A2841h,028AE8A28h,08A28A28Ah,0A28A28A2h,
  0E82FBE0Bh,08A28A28Ah,0A28A28A2h,003FFC0ECh,08A28A2BBh,0A28A28A2h,02BA0BA28h,
  0CA28A28Ah,0A28AFC0Fh,0EC0ECA28h,08A28A28Ah,041041042h,028AFC0FCh,0BF28A28Ah,
  0A28A28A2h,028A2BFF8h,03F28A28Ah,0410413F0h,010410410h,03C13C104h,04104F04Fh,
  02BE10410h,08A28A28Ah,0A28A28A2h,028A2BF28h,00428A28Ah,04FC41041h,0104104FCh,
  0E4924104h,008208279h,0FFFE2082h,0FFFFFFFFh,0FFFFFFFFh,0FFFFFFFFh,0FFFFFFFFh,
  071FFFFFFh,01C71C71Ch,0C71C71C7h,0DB0BDC71h,0CF436CB2h,0ACB6DB0Bh,0FFFFEF43h,
  0E38E38E3h,0DFDC4E38h,0FE4711C4h,0EFB88E26h,0108EFEC8h,0E22111F4h,07388420Bh,
  0084212BFh,0E77EA421h,04BE22108h,0D4AC210Ah,04AD4AD4Ah,0AD4AD4ADh,0ADEB54D4h,
  066113935h,042F99945h,094BE3178h,043E94A52h,04A5294C4h,07C62E8FDh,06AE23A21h,
  0F211D5ADh,084210843h,0339D7398h,0AE731212h,0D318C673h,0297A3149h,0EE3194A5h,
  0318C635Ah,0FC631FC6h,0A318C631h,01953D18Fh,0BE318C5Fh,021087C10h,02109FF04h,
  063189084h,0532610F4h,0D6B5756Bh,0F18BE317h,08C5E1085h,06B38C62Fh,006422E9Dh,
  0D6B58BA3h,0908BBF5Ah,0AD62E8C3h,0510FD6B5h,00843D18Ch,04A4E1087h,0B7AD6A97h,
  08CB89AD6h,0E31FC631h,018BC210Fh,0C6318BE3h,010FC6318h,05294A531h,06AE8FD4Ah,
  0EAD6B5ADh,00852F908h,031388421h,08C544546h,0EB9CC631h,098D6319Ch,046339D73h,
  05294A52Eh,0FC6318CAh,07F18C631h,018C6318Ch,07A318FA3h,0B5AC632Ah,0AD6B5756h,
  062F08421h,038C62F8Ch,0BA75AD6Bh,030419108h,0AD6B58BAh,022EFD6B5h,0E8C21E84h,
  0D6B5AD62h,031443F5Ah,08421E8C6h,09C210870h,00001D294h
  };

CONST _cellW = ARRAY OF CHAR {
   5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 0c, 3c, 4c, 5c,
   5c, 5c, 5c, 3c, 4c, 4c, 5c, 5c, 3c, 5c, 4c, 4c, 5c, 4c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c, 3c, 4c, 4c, 4c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c,
   5c, 4c, 5c, 4c, 5c, 5c, 4c, 6c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 4c, 4c, 5c,
   4c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 4c, 3c, 4c,
   5c, 5c, 5c, 5c, 5c, 3c, 3c, 3c, 4c, 4c, 3c, 4c, 4c, 4c, 4c, 4c, 3c, 3c,
   6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c, 6c,
   6c, 6c, 6c, 6c, 6c, 6c, 6c, 3c, 6c, 6c, 6c, 3c, 6c, 6c, 6c, 6c, 3c, 6c,
   6c, 3c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 6c, 5c, 6c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c, 5c,
   5c, 5c, 5c, 5c
  };

CONST _cellX = ARRAY OF CHAR {
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
   0c, 0c, 0c, 0c
  };

CONST _cellH = ARRAY OF CHAR {
  14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,
  14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c,14c, 0c,11c, 2c,10c,
  11c,10c,12c, 3c,11c,11c, 5c, 7c, 3c, 1c, 2c,10c,11c,11c,11c,11c,11c,11c,
  11c,11c,11c,11c, 6c,10c, 7c, 4c, 7c,11c,11c,11c,11c,11c,11c,11c,11c,11c,
  11c,11c,11c,11c,11c,11c,11c,11c,11c,12c,11c,11c,11c,11c,11c,11c,11c,11c,
  11c,11c,11c,11c, 3c, 1c, 3c, 7c,11c, 7c,11c, 7c,11c,12c,11c,12c,14c,11c,
  11c, 7c, 7c, 7c,11c,11c, 7c, 7c,11c, 7c, 7c, 7c, 7c,11c, 7c,11c,11c,11c,
   3c,13c,16c,16c,16c,16c,16c,16c,16c,10c,11c,16c,16c,11c,10c, 7c,10c,10c,
   7c, 7c,10c,16c, 1c,16c,16c,16c,10c,11c,10c,11c,16c, 3c,16c,10c, 7c, 7c,
  10c,11c,10c,11c,10c,16c,16c, 7c,10c,16c, 7c,16c,16c, 7c,10c,10c, 7c, 7c,
   7c, 7c, 6c, 7c, 5c, 4c, 5c, 5c,12c,11c,10c,16c, 7c, 7c, 7c,11c,10c, 7c,
  11c, 7c, 7c, 7c,11c, 7c, 7c, 7c, 7c, 7c, 7c, 7c,10c, 7c, 7c,11c, 7c, 7c,
   7c, 7c, 7c, 7c, 7c,11c, 7c, 7c,11c,11c,11c,13c,12c,11c,13c,11c,11c,11c,
  11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,11c,
  11c,13c,11c,11c
  };

CONST _cellY = ARRAY OF CHAR {
   2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c,
   2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 2c, 0c, 3c,12c, 3c,
   3c, 3c, 2c,11c, 3c, 3c, 5c, 4c, 2c, 7c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c,
   3c, 3c, 3c, 3c, 4c, 2c, 4c, 5c, 4c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c,
   3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 2c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c,
   3c, 3c, 3c, 3c,11c, 2c,11c, 3c, 3c, 3c, 3c, 3c, 3c, 1c, 3c, 3c, 1c, 3c,
   3c, 3c, 3c, 3c, 1c, 1c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 1c, 3c, 3c, 3c, 3c,
  11c, 2c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 6c, 7c, 6c, 0c,
   7c, 7c, 0c, 0c, 7c, 0c, 0c, 0c, 6c, 0c, 6c, 0c, 0c, 6c, 0c, 6c, 7c, 7c,
   0c, 0c, 6c, 0c, 0c, 0c, 0c, 7c, 0c, 0c, 0c, 0c, 0c, 7c, 2c, 2c, 0c, 7c,
   0c, 7c, 3c, 3c, 4c, 4c, 5c, 5c, 2c, 2c, 1c, 0c, 3c, 3c, 3c, 1c, 2c, 3c,
   2c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 2c, 3c, 3c, 1c, 3c, 3c,
   3c, 3c, 3c, 3c, 3c, 1c, 3c, 3c, 3c, 3c, 3c, 1c, 2c, 3c, 1c, 3c, 3c, 3c,
   3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c, 3c,
   3c, 1c, 3c, 3c
  };
CONST _bases = ARRAY OF INTEGER {
  0000h,003Ch,0078h,00B4h,00F0h,012Ch,0168h,01A4h,01E0h,021Ch,0258h,0294h,
  02D0h,030Ch,0348h,0384h,03C0h,03FCh,0438h,0474h,04B0h,04ECh,0528h,0564h,
  05A0h,05DCh,0618h,0654h,0690h,06CCh,0708h,0744h,-0001,0780h,079Bh,07A3h,
  07CBh,07F8h,0820h,0852h,085Bh,087Fh,08A3h,08BCh,0852h,08DFh,08E4h,08ECh,
  090Ch,0939h,095Dh,098Ah,09B7h,09E4h,0A11h,0A3Eh,0A6Bh,0A98h,0AC5h,0AD7h,
  0AF7h,0B13h,0B23h,0B46h,0B73h,0BA0h,0BCDh,0BFAh,0C27h,0C54h,0C81h,0CAEh,
  0CDBh,0D08h,0D35h,0D62h,0D8Fh,0DBCh,0DE9h,0E16h,0E43h,0E70h,0EA2h,0ECFh,
  0EFCh,0F29h,0F56h,0F83h,0FB0h,0FDDh,100Ah,1037h,105Bh,1088h,10ACh,08DFh,
  10BBh,10C7h,10F1h,111Eh,1141h,116Eh,1191h,11BEh,11F0h,121Dh,1245h,1275h,
  12A2h,12C6h,12E9h,130Ch,132Fh,135Ch,1389h,13ACh,13CFh,13FCh,141Fh,1442h,
  1465h,1488h,14B5h,14D8h,14FCh,1517h,153Bh,154Ah,1581h,15C7h,160Dh,1653h,
  167Dh,16A7h,16D1h,1709h,1729h,1744h,177Ch,17B4h,17D8h,17F8h,1814h,182Ch,
  1844h,186Eh,1898h,18C8h,191Ch,1922h,1976h,19CAh,1A1Eh,1A4Eh,1A84h,1AB4h,
  1AEAh,1B3Eh,1B50h,1BA4h,1BD4h,1BFEh,1C28h,1C58h,1C8Eh,1CBEh,1CF4h,1D24h,
  1D78h,1DCCh,1DE1h,1E11h,1E65h,1E8Fh,1EB9h,1E65h,1F0Dh,1F3Dh,1F6Dh,1F82h,
  1F82h,1F6Dh,1FACh,1FCAh,1FEDh,2006h,201Ah,2033h,204Ch,207Eh,20ABh,20D3h,
  2127h,214Ah,2174h,2197h,21C4h,21ECh,220Fh,223Ch,1465h,225Fh,2282h,22AFh,
  22D2h,22F5h,2318h,130Ch,233Bh,235Eh,2381h,23A9h,23CCh,23EFh,241Ch,243Fh,
  2462h,2485h,24A8h,24CBh,24EEh,2511h,253Eh,2561h,2584h,25B1h,25DEh,260Bh,
  2642h,0C54h,2674h,26ABh,26D8h,2705h,2732h,0D62h,275Fh,0DBCh,278Ch,0E16h,
  27B9h,27E6h,0E43h,0BFAh,0EFCh,1488h,2813h,0BCDh,2840h,286Dh,289Ah,28C7h,
  28F4h,2921h,2958h,2985h
  };

PROCEDURE adr(VAL xx: ARRAY OF WORD): ADDRESS;
BEGIN RETURN ADR(xx) END adr;

BEGIN
  done:=TRUE;  error:=err.ok;
  font:=ADR(DEFAULT);
  WITH DEFAULT DO
    state:=packed;       W:=6; H:=14;
    magic:=MAGIC;        BASE :=adr(_BASE);
    cellW:=adr(_cellW);  cellH:=adr(_cellH);
    cellY:=adr(_cellY);  bases:=adr(_bases);
    cellX:=adr(_cellX);  propW:=NIL;     propX:=NIL;
    size :=SIZE(_BASE);  space:=W;
    uline:=0;            bline:=0;
    fchar:=0c;           lchar:=377c
  END
END Fonts.
