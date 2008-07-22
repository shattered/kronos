IMPLEMENTATION MODULE pcGenCmd; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT mcd : defCodes;
IMPORT mem : pcSystem;
IMPORT gen : krSym;
IMPORT exp : pcGenExpr;

WITH STORAGE : mem;

CONST
  stk_inc = ARRAY OF INTEGER {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 0, 1,-1, 0,-1, 0,-1, 0,-1, 0,
    1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   -1,-1,-1,-2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   -3,-3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   -2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,
    0, 0, 1,-1,-1,-2, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,
    0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0,
   -1,-1,-1,-1,-1,-1, 0, 0,-1,-1,-1,-1,-1, 0, 0,-1,
   -1,-1, 0, 0, 0, 1,-1,-2,-3, 0,-1, 0, 0,-1, 0, 0,
   -3, 0, 1, 0, 1, 1,-2,-1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   -2,-2,-2,-1,-1,-1,-2,-2,-1, 1,-2, 1,-2,-4,-5, 0,
    0, 1, 1,-1,-1,-2,-1, 0,-2, 0, 1, 0, 0, 0,-1, 0
  };

PROCEDURE new_code(n: INTEGER): INTEGER;
BEGIN
  IF cnt+n>HIGH(code)+1 THEN RESIZE(code,HIGH(code)+n+4000) END;
  INC(cnt,n);
  RETURN cnt-n;
END new_code;

PROCEDURE b(n: INTEGER);
BEGIN
  IF cnt>HIGH(code) THEN RESIZE(code,HIGH(code)+4000) END;
  code[cnt]:=CHAR(n); INC(cnt);
END b;

PROCEDURE c(n: INTEGER);
BEGIN
  b(n);
  INC(stk,stk_inc[n]);
  IF stk>stk_max THEN stk_max:=stk END;
END c;

PROCEDURE li(n: INTEGER);
BEGIN
  IF n>=0 THEN
    IF n<10h THEN c(mcd.li0+n)
    ELSIF n<100h THEN c(mcd.lib); b(n);
    ELSIF n<10000h THEN c(mcd.lid); b(n); b(n>>8);
    ELSIF n=7FFFFF80h THEN c(mcd.lin);
    ELSE c(mcd.liw); b(n); b(n>>8); b(n>>16); b(n>>24);
    END;
  ELSIF n=MIN(INTEGER) THEN c(mcd.li1); c(mcd.copt); c(mcd.ror);
  ELSIF -n<10000h THEN li(-n); c(mcd.neg)
  ELSE c(mcd.liw); b(n); b(n>>8); b(n>>16); b(n>>24);
  END;
END li;

PROCEDURE lsa(n: INTEGER);
BEGIN
  IF n=0 THEN
  ELSIF (n>=10h) & (n<100h) THEN c(mcd.lsa); b(n);
  ELSIF n>0 THEN li(n); c(mcd.add);
  ELSE li(-n); c(mcd.sub);
  END;
END lsa;

PROCEDURE power2(n: INTEGER; VAR i: INTEGER): BOOLEAN;
BEGIN
  i:=0;
  LOOP
    IF n<INTEGER({i}) THEN RETURN FALSE END;
    IF n=INTEGER({i}) THEN RETURN TRUE  END;
    IF i=30 THEN RETURN FALSE END;
    INC(i);
  END;
END power2;

PROCEDURE cmul(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF n=0 THEN c(mcd.drop); c(mcd.li0);
  ELSIF n=1 THEN
  ELSIF n=-1 THEN c(mcd.neg);
  ELSIF n=2 THEN c(mcd.copt); c(mcd.add);
  ELSIF power2(n,i) THEN li(i); c(mcd.shl);
  ELSE li(n); c(mcd.mul)
  END;
END cmul;

PROCEDURE cdiv(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF n=1 THEN
  ELSIF n=-1 THEN c(mcd.neg);
  ELSIF power2(n,i) THEN li(i); c(mcd.shr);
  ELSE li(n); c(mcd.div);
  END;
END cdiv;

PROCEDURE sgw(n: INTEGER);
BEGIN
  IF (n>=2) & (n<10h) THEN c(mcd.sgw2-2+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.sgw); b(n);
  ELSE
    c(mcd.lga); b(0); li(n); c(mcd.add);
    c(mcd.swap); c(mcd.ssw0);
  END;
END sgw;

PROCEDURE sgw1(n: INTEGER);
BEGIN
  IF n<0 THEN c(mcd.lga); b(0); lsa(n);
  ELSIF n>=100h THEN c(mcd.lga); b(0FFh); lsa(n-0FFh);
  END;
END sgw1;

PROCEDURE sgw2(n: INTEGER);
BEGIN
  IF (n>=0) & (n<100h) THEN sgw(n) ELSE c(mcd.ssw0) END;
END sgw2;

PROCEDURE slw(n: INTEGER);
BEGIN
  IF (n>=4) & (n<10h) THEN c(mcd.slw4-4+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.slw); b(n);
  ELSIF (n>=-100h) & (n<0) THEN c(mcd.spw); b(-n-1);
  ELSE
    c(mcd.lla); b(0); li(n); c(mcd.add);
    c(mcd.swap); c(mcd.ssw0);
  END;
END slw;

PROCEDURE slw1(n: INTEGER);
BEGIN
  IF n<-100h THEN c(mcd.lla); b(0); lsa(n);
  ELSIF n>=100h THEN c(mcd.lla); b(0FFh); lsa(n-0FFh);
  END;
END slw1;

PROCEDURE slw2(n: INTEGER);
BEGIN
  IF (n>=-100h) & (n<100h) THEN slw(n) ELSE c(mcd.ssw0) END;
END slw2;

PROCEDURE ssw(n: INTEGER);
BEGIN
  IF (n>=0) & (n<10h) THEN c(mcd.ssw0+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.ssw); b(n);
  ELSIF n>0 THEN
    c(mcd.swap); li(n-0Fh); c(mcd.add);
    c(mcd.swap); c(mcd.ssw0F);
  ELSIF n<0 THEN
    c(mcd.swap); li(-n); c(mcd.sub);
    c(mcd.swap); c(mcd.ssw0);
  END;
END ssw;

PROCEDURE ssw1(n: INTEGER);
BEGIN
  IF n>=100h THEN li(n-0Fh); c(mcd.add);
  ELSIF n<0 THEN li(-n); c(mcd.sub);
  END;
END ssw1;

PROCEDURE ssw2(n: INTEGER);
BEGIN
  IF (n>=0) & (n<10h) THEN c(mcd.ssw0+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.ssw); b(n);
  ELSIF n>0 THEN c(mcd.ssw0F);
  ELSIF n<0 THEN c(mcd.ssw0);
  END;
END ssw2;

PROCEDURE lgw(n: INTEGER);
BEGIN
  IF (n>=2) & (n<10h) THEN c(mcd.lgw2-2+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.lgw); b(n);
  ELSIF n>0 THEN c(mcd.lga); b(0FFh); lsa(n-0FFh); c(mcd.lsw0);
  ELSE c(mcd.lga); b(0); lsa(n); c(mcd.lsw0);
  END;
END lgw;

PROCEDURE sew(m,n: INTEGER);
BEGIN
  IF (m>=0) & (m<100h) & (n>=0) & (n<100h) THEN c(mcd.sew); b(m); b(n);
  ELSE lgw(-m-1); c(mcd.lsw0); c(mcd.swap); ssw(n);
  END;
END sew;

PROCEDURE sew1(m,n: INTEGER);
BEGIN
  IF (m<0) OR (m>=100h) OR (n<0) OR (n>=100h) THEN
    lgw(-m-1); c(mcd.lsw0); ssw1(n);
  END;
END sew1;

PROCEDURE sew2(m,n: INTEGER);
BEGIN
  IF (m>=0) & (m<100h) & (n>=0) & (n<100h) THEN c(mcd.sew); b(m); b(n);
  ELSE ssw2(n);
  END;
END sew2;

PROCEDURE llw(n: INTEGER);
BEGIN
  IF (n>=4) & (n<10h) THEN c(mcd.llw4-4+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.llw); b(n);
  ELSIF (n>=-100h) & (n<0) THEN c(mcd.lpw); b(-n-1);
  ELSIF n>0 THEN c(mcd.lla); b(0FFh); lsa(n-0FFh); c(mcd.lsw0);
  ELSE c(mcd.lla); b(0); lsa(n); c(mcd.lsw0);
  END;
END llw;

PROCEDURE lsw(n: INTEGER);
BEGIN
  IF (n>=0) & (n<10h) THEN c(mcd.lsw0+n)
  ELSIF (n>=0) & (n<100h) THEN c(mcd.lsw); b(n);
  ELSIF n>0 THEN li(n-0Fh); c(mcd.add); c(mcd.lsw0F);
  ELSIF n<0 THEN li(-n); c(mcd.sub);  c(mcd.ssw0);
  END;
END lsw;

PROCEDURE lew(m,n: INTEGER);
BEGIN
  IF (m>=0) & (m<100h) & (n>=0) & (n<100h) THEN c(mcd.lew); b(m); b(n);
  ELSE lgw(-m-1); c(mcd.lsw0); lsw(n);
  END;
END lew;

PROCEDURE lga(n: INTEGER);
BEGIN
  IF (n>=0) & (n<100h) THEN c(mcd.lga); b(n);
  ELSIF n>=0 THEN c(mcd.lga); b(0FFh); li(n-0FFh); c(mcd.add);
  ELSE c(mcd.lga); b(0); li(-n); c(mcd.sub);
  END;
END lga;

PROCEDURE lla(n: INTEGER);
BEGIN
  IF (n>=0) & (n<100h) THEN c(mcd.lla); b(n);
  ELSIF (n>=-100h) & (n<0) THEN c(mcd.lpa); b(-n-1);
  ELSIF n>=0 THEN c(mcd.lla); b(0FFh); li(n-0FFh); c(mcd.add);
  ELSE c(mcd.lla); b(0); li(-n); c(mcd.sub);
  END;
END lla;

PROCEDURE lea(m,n: INTEGER);
BEGIN
  IF (m>=0) & (m<100h) & (n>=0) & (n<100h) THEN c(mcd.lea); b(m); b(n);
  ELSE lgw(-m-1); c(mcd.lsw0); lsa(n);
  END;
END lea;

PROCEDURE gb(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  ASSERT(n>=0);
  IF n=0 THEN lla(0)
  ELSIF n=1 THEN c(mcd.gb1)
  ELSIF n<100h THEN c(mcd.gb); b(n)
  ELSE
    c(mcd.gb); b(0FFh);
    FOR i:=100h TO n-1 DO c(mcd.lsw0) END;
  END;
END gb;

PROCEDURE enter(n: INTEGER);
BEGIN
  IF n=0 THEN
  ELSIF (n>=0) & (n<100h) THEN c(mcd.entr); b(n)
  ELSE li(-n); c(mcd.decs);
  END;
END enter;

PROCEDURE lsta(n: INTEGER);
BEGIN
  IF (n>=0) & (n<=0FFFFh) THEN c(mcd.lsta); b(n); b(n>>8);
  ELSIF n<0 THEN lgw(1); lsa(n);
  ELSE c(mcd.lsta); b(0FFh); b(0FFh); lsa(n-0FFFFh);
  END;
END lsta;

PROCEDURE load(md: gen.access_mode; lvl,no,disp,sz: INTEGER);
BEGIN
  ASSERT((sz>0) & (sz<=32));
  CASE md OF
    |gen.am_L  :
      IF lvl#exp.level THEN
        gb(exp.level-lvl); load(gen.am_adr,0,0,disp,sz);
      ELSIF (disp MOD 32 = 0) & (sz=32) THEN llw(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN
        lla(disp DIV 32); li(disp MOD 32 DIV 8); c(mcd.lxb);
      ELSE
        lla(disp DIV 32); li(disp MOD 32); li(sz); c(mcd.bbu);
      END;
    |gen.am_aL :
      IF lvl#exp.level THEN gb(exp.level-lvl); lsw(no) ELSE llw(no) END;
      load(gen.am_adr,0,0,disp,sz);
    |gen.am_G  :
      IF lvl=0 THEN
        IF (disp MOD 32 = 0) & (sz=32) THEN lgw(disp DIV 32);
        ELSIF (disp MOD 8 = 0) & (sz=8) THEN
          lga(disp DIV 32); li(disp MOD 32 DIV 8); c(mcd.lxb);
        ELSE
          lga(disp DIV 32); li(disp MOD 32); li(sz); c(mcd.bbu);
        END;
      ELSE
        IF (disp MOD 32 = 0) & (sz=32) THEN lew(lvl,disp DIV 32);
        ELSIF (disp MOD 8 = 0) & (sz=8) THEN
          lea(lvl,disp DIV 32); li(disp MOD 32 DIV 8); c(mcd.lxb);
        ELSE
          lea(lvl,disp DIV 32); li(disp MOD 32); li(sz); c(mcd.bbu);
        END;
      END;
    |gen.am_aG :
      IF lvl=0 THEN lgw(no) ELSE lew(lvl,no) END;
      load(gen.am_adr,0,0,disp,sz);
    |gen.am_STR:
      IF lvl=0 THEN lgw(1) ELSE lew(lvl,1) END;
      load(gen.am_adr,0,0,disp,sz);
    |gen.am_xw :
      IF (disp MOD 32 =0) & (sz=32) THEN lsa(disp DIV 32); c(mcd.lxw);
      ELSE
        c(mcd.add);
        IF (disp MOD 8=0) & (sz=8) THEN li(disp DIV 8); c(mcd.lxb);
        ELSE li(disp); li(sz); c(mcd.bbu);
        END;
      END;
    |gen.am_xb :
      IF (disp MOD 8=0) & (sz=8) THEN lsa(disp DIV 8); c(mcd.lxb);
      ELSE c(mcd.li3); c(mcd.shl); lsa(disp); li(sz); c(mcd.bbu);
      END;
    |gen.am_xt : lsa(disp); li(sz); c(mcd.bbu);
    |gen.am_adr:
      IF (disp MOD 32 = 0) & (sz=32) THEN lsw(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN li(disp DIV 8); c(mcd.lxb);
      ELSE li(disp); li(sz); c(mcd.bbu);
      END;
    |gen.am_abs:
      li(no);
      IF (disp MOD 32 = 0) & (sz=32) THEN lsw(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN li(disp DIV 8); c(mcd.lxb);
      ELSE li(disp); li(sz); c(mcd.bbu);
      END;
    |gen.am_stk:
    |gen.am_imm: li(no);
  END;
END load;

PROCEDURE store1(md: gen.access_mode; lvl,no,disp,sz: INTEGER);
BEGIN
  ASSERT((sz>0) & (sz<=32));
  CASE md OF
    |gen.am_L  :
      IF lvl#exp.level THEN
        gb(exp.level-lvl); store1(gen.am_adr,0,0,disp,sz);
      ELSIF (disp MOD 32 = 0) & (sz=32) THEN slw1(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN
        lla(disp DIV 32); li(disp MOD 32 DIV 8);
      ELSE
        lla(disp DIV 32); li(disp MOD 32); li(sz);
      END;
    |gen.am_aL :
      IF lvl#exp.level THEN gb(exp.level-lvl); lsw(no) ELSE llw(no) END;
      store1(gen.am_adr,0,0,disp,sz);
    |gen.am_G  :
      IF lvl=0 THEN
        IF (disp MOD 32 = 0) & (sz=32) THEN sgw1(disp DIV 32);
        ELSIF (disp MOD 8 = 0) & (sz=8) THEN
          lga(disp DIV 32); li(disp MOD 32 DIV 8);
        ELSE
          lga(disp DIV 32); li(disp MOD 32); li(sz);
        END;
      ELSE
        IF (disp MOD 32 = 0) & (sz=32) THEN sew1(lvl,disp DIV 32);
        ELSIF (disp MOD 8 = 0) & (sz=8) THEN
          lea(lvl,disp DIV 32); li(disp MOD 32 DIV 8);
        ELSE
          lea(lvl,disp DIV 32); li(disp MOD 32); li(sz);
        END;
      END;
    |gen.am_aG :
      IF lvl=0 THEN lgw(no) ELSE lew(lvl,no) END;
      store1(gen.am_adr,0,0,disp,sz);
    |gen.am_xw :
      IF (disp MOD 32 =0) & (sz=32) THEN lsa(disp DIV 32);
      ELSE
        c(mcd.add);
        IF (disp MOD 8=0) & (sz=8) THEN li(disp DIV 8);
        ELSE li(disp); li(sz);
        END;
      END;
    |gen.am_xb :
      IF (disp MOD 8=0) & (sz=8) THEN lsa(disp DIV 8);
      ELSE c(mcd.li3); c(mcd.shl); lsa(disp); li(sz);
      END;
    |gen.am_xt : lsa(disp); li(sz);
    |gen.am_adr:
      IF (disp MOD 32 = 0) & (sz=32) THEN ssw1(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN li(disp DIV 8);
      ELSE li(disp); li(sz);
      END;
    |gen.am_abs:
      li(no);
      IF (disp MOD 32 = 0) & (sz=32) THEN ssw1(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN li(disp DIV 8);
      ELSE li(disp); li(sz);
      END;
  ELSE exp.error(NIL,'store1: illegal mode %d',md);
  END;
END store1;

PROCEDURE store2(md: gen.access_mode; lvl,no,disp,sz: INTEGER);
BEGIN
  ASSERT((sz>0) & (sz<=32));
  CASE md OF
    |gen.am_L  :
      IF lvl#exp.level THEN store2(gen.am_adr,0,0,disp,sz);
      ELSIF (disp MOD 32 = 0) & (sz=32) THEN slw2(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN c(mcd.sxb);
      ELSE c(mcd.bbp);
      END;
    |gen.am_G  :
      IF lvl=0 THEN
        IF (disp MOD 32 = 0) & (sz=32) THEN sgw2(disp DIV 32);
        ELSIF (disp MOD 8 = 0) & (sz=8) THEN c(mcd.sxb);
        ELSE c(mcd.bbp);
        END;
      ELSE
        IF (disp MOD 32 = 0) & (sz=32) THEN sew2(lvl,disp DIV 32);
        ELSIF (disp MOD 8 = 0) & (sz=8) THEN c(mcd.sxb);
        ELSE c(mcd.bbp);
        END;
      END;
    |gen.am_xw :
      IF (disp MOD 32 =0) & (sz=32) THEN c(mcd.sxw);
      ELSE
        IF (disp MOD 8=0) & (sz=8) THEN c(mcd.sxb);
        ELSE c(mcd.bbp);
        END;
      END;
    |gen.am_xb :
      IF (disp MOD 8=0) & (sz=8) THEN c(mcd.sxb);
      ELSE c(mcd.bbp);
      END;
    |gen.am_xt : c(mcd.bbp);
    |gen.am_adr,gen.am_aL,gen.am_aG,gen.am_abs:
      IF (disp MOD 32 = 0) & (sz=32) THEN ssw2(disp DIV 32);
      ELSIF (disp MOD 8 = 0) & (sz=8) THEN c(mcd.sxb);
      ELSE c(mcd.bbp);
      END;
  END;
END store2;

END pcGenCmd.
