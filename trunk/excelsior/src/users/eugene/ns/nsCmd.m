IMPLEMENTATION MODULE nsCmd; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
IMPORT mem : pcSystem;
IMPORT sym : nsSym;

FROM nsSym       IMPORT adr_mode, index_mode;

WITH STORAGE : mem;

TYPE
  access_class = (a_read,a_write,a_rmw,a_addr,a_regaddr);

PROCEDURE new_code(n: INTEGER): INTEGER;
BEGIN
  IF cnt+n>HIGH(code) THEN RESIZE(code,cnt+(cnt+n)) END;
  RETURN cnt;
END new_code;

PROCEDURE put(n: WORD);
BEGIN
  IF cnt>HIGH(code) THEN RESIZE(code,cnt*2) END;
  code[cnt]:=CHAR(n); INC(cnt);
END put;

PROCEDURE put2(n: WORD);
BEGIN
  IF cnt>=HIGH(code) THEN RESIZE(code,cnt*2) END;
  code[cnt]:=CHAR(n); INC(cnt);
  code[cnt]:=CHAR(n>>8); INC(cnt);
END put2;

PROCEDURE md(VAL a: sym.access; pos: INTEGER): BITSET;
  VAR n: INTEGER;
BEGIN
  CASE a.xm OF
    |xm_off:
      CASE a.am OF
        |am_RG    : n:=a.rg;
        |am_FPP4  : n:=a.rg;
        |am_FPP8  : n:=a.rg;
        |am_aRG   : n:=a.rg+8;
        |am_aaFP  : n:=10h;
        |am_aaSP  : n:=11h;
        |am_aaSB  : n:=12h;
        |am_imm   : n:=14h;
        |am_abs   : n:=15h;
        |am_EXT   : n:=16h;
        |am_TOS   : n:=17h;
        |am_aFP   : n:=18h;
        |am_aFPimm: n:=18h;
        |am_aSP   : n:=19h;
        |am_aSB   : n:=1Ah;
        |am_aSBimm: n:=1Ah;
        |am_aPC   : n:=1Bh;
      END;
    |xm_b: n:=1Ch;
    |xm_w: n:=1Dh;
    |xm_d: n:=1Eh;
    |xm_q: n:=1Fh;
  END;
  RETURN BITSET(n<<pos);
END md;

PROCEDURE isz(sz: INTEGER): BITSET;
  VAR n: INTEGER;
BEGIN
  CASE sz OF
    |1: n:=0;
    |2: n:=1;
    |4: n:=3;
  END;
  RETURN BITSET(n);
END isz;

PROCEDURE disp(n: INTEGER);
BEGIN
  IF (n>=-64) & (n<=63) THEN
    put(BITSET(n)-{7});
  ELSIF (n>=-8192) & (n<=8191) THEN
    put(BITSET(n>>8)-{6}+{7}); put(n);
  ELSE
    put(BITSET(n>>24)+{6,7}); put(n>>16); put(n>>8); put(n);
  END;
END disp;

PROCEDURE value(n,sz: INTEGER);
BEGIN
  CASE sz OF
    |1: put(n);
    |2: put(n>>8); put(n);
    |4: put(n>>24); put(n>>16); put(n>>8); put(n);
  END;
END value;

PROCEDURE put_disp(VAL a: sym.access; sz: INTEGER);
BEGIN
  CASE a.am OF
    |am_RG    :
    |am_FPP4  :
    |am_FPP8  :
    |am_aRG   : disp(a.disp);
    |am_aaFP  : disp(a.n); disp(a.disp);
    |am_aaSP  : disp(a.n); disp(a.disp);
    |am_aaSB  : disp(a.n); disp(a.disp);
    |am_abs   : disp(a.disp);
    |am_EXT   : disp(a.n); disp(a.disp);
    |am_TOS   :
    |am_aFP   : disp(a.disp);
    |am_aFPimm: disp(a.disp);
    |am_aSP   : disp(a.disp);
    |am_aSB   : disp(a.disp);
    |am_aSBimm: disp(a.disp);
    |am_aPC   : disp(a.disp);
    |am_imm   : value(a.n,sz);
  END;
END put_disp;

PROCEDURE put_index(VAL a: sym.access);
  VAR n: INTEGER;
BEGIN
  IF a.xm=xm_off THEN RETURN END;
  CASE a.am OF
    |am_aRG : n:=a.rg+8;
    |am_aaFP: n:=10h;       |am_aaSP  : n:=11h;
    |am_aaSB: n:=12h;
    |am_abs : n:=15h;       |am_EXT   : n:=16h;
    |am_TOS : n:=17h;       |am_aFP   : n:=18h;
                            |am_aFPimm: n:=18h;
    |am_aSP : n:=19h;       |am_aSB   : n:=1Ah;
    |am_aPC : n:=1Bh;       |am_aSBimm: n:=1Ah;
  END;
  put(BITSET(n<<3)+BITSET(a.rg_x)*{0..2});
END put_index;

PROCEDURE chk(VAL a: sym.access; class: access_class);
BEGIN
  IF a.xm=xm_off THEN ASSERT((a.am#am_imm) OR (class=a_read));
  ELSE ASSERT(a.am#am_imm);
  END;
END chk;

PROCEDURE fpp(c: cop; VAL x,y: sym.access; sz: INTEGER);
  VAR s: BITSET;
BEGIN
  chk(x,a_read);
  CASE c OF
    |absf: s:={2,4,5};  chk(y,a_write);
    |negf: s:={2,4};    chk(y,a_write);
    |addf: s:={};       chk(y,a_rmw);
    |subf: s:={4};      chk(y,a_rmw);
    |mulf: s:={4,5};    chk(y,a_rmw);
    |divf: s:={5};      chk(y,a_rmw);
    |cmpf: s:={3};      chk(y,a_read);
    |movf: s:={2};      chk(y,a_write);
  END;
  CASE sz OF
    |4: s:=s+{0}
    |8:
  END;
  put(0BEh);
  put2(s+md(y,6)+md(x,11));
  put_index(x); put_index(y);
  put_disp(x,sz); put_disp(y,sz);
END fpp;

PROCEDURE int(c: cop; VAL x,y: sym.access; sz: INTEGER);
  VAR s: BITSET;
  PROCEDURE quick(): BOOLEAN;
  BEGIN
    IF (x.am#am_imm) OR (x.n<-8) OR (x.n>7) THEN RETURN FALSE END;
    CASE c OF
      |add : s:={2,3};     chk(y,a_rmw);   IF x.n=0 THEN RETURN TRUE END;
      |cmp : s:={2,3,4};   chk(y,a_read);
      |mov : s:={2,3,4,6}; chk(y,a_write);
    ELSE RETURN FALSE;
    END;
    put2(s+isz(sz)+md(y,11)+BITSET(x.n<<7)*{7..10});
    put_index(y); put_disp(y,sz);
    RETURN TRUE;
  END quick;
BEGIN
  IF quick() THEN RETURN END;
  chk(x,a_read);
  CASE c OF
    |add : s:={};       chk(y,a_rmw);
    |addc: s:={4};      chk(y,a_rmw);
    |addr: s:={2,5};    chk(y,a_write); ASSERT(sz=4);
    |and : s:={3,5};    chk(y,a_rmw);
    |bic : s:={3};      chk(y,a_rmw);
    |cmp : s:={2};      chk(y,a_read);
    |mov : s:={2,4};    chk(y,a_write);
    |or  : s:={3,4};    chk(y,a_rmw);
    |sub : s:={5};      chk(y,a_rmw);
    |subc: s:={4,5};    chk(y,a_rmw);
    |tbit: s:={2,4,5};  chk(y,a_regaddr);
    |xor : s:={3,4,5};  chk(y,a_rmw);
  END;
  put2(s+isz(sz)+md(y,6)+md(x,11));
  put_index(x); put_index(y);
  put_disp(x,sz); put_disp(y,sz);
END int;

PROCEDURE int_4E(c: cop; VAL x,y: sym.access; sz: INTEGER);
  VAR s: BITSET;
BEGIN
  chk(x,a_read);
  CASE c OF
    |abs  : s:={4,5};   chk(y,a_write);
    |addp : s:={2..5};  chk(y,a_rmw);
    |subp : s:={2,3,5}; chk(y,a_rmw);
    |ash  : s:={2};     chk(y,a_rmw);
    |cbit : s:={3};     chk(y,a_regaddr);
    |cbiti: s:={2,3};   chk(y,a_regaddr);
    |sbit : s:={3,4};   chk(y,a_regaddr);
    |sbiti: s:={2,3,4}; chk(y,a_regaddr);
    |com  : s:={2,4,5}; chk(y,a_write);
    |ibit : s:={3,4,5}; chk(y,a_regaddr);
    |lsh  : s:={2,4};   chk(y,a_rmw);
    |neg  : s:={5};     chk(y,a_write);
    |not  : s:={2,5};   chk(y,a_write);
    |rot  : s:={};      chk(y,a_rmw);
  END;
  put(4Eh);
  put2(s+isz(sz)+md(y,6)+md(x,11));
  put_index(x); put_index(y);
  IF (c=ash) OR (c=lsh) OR (c=rot) THEN put_disp(x,1);
  ELSE put_disp(x,sz);
  END;
  put_disp(y,sz);
END int_4E;

PROCEDURE int_6E(c: cop; VAL x,y: sym.access; sz: INTEGER);
  VAR s: BITSET;
BEGIN
  chk(x,a_read);
  CASE c OF
    |ffs  : s:={2};   chk(y,a_rmw);
  END;
  put(6Eh);
  put2(s+isz(sz)+md(y,6)+md(x,11));
  put_index(x); put_index(y);
  put_disp(x,sz); put_disp(y,0);
END int_6E;

PROCEDURE int_CE(c: cop; VAL x,y: sym.access; sz: INTEGER);
  VAR s: BITSET;
BEGIN
  chk(x,a_read);
  CASE c OF
    |dei : s:={2,3,5}; chk(y,a_rmw);
    |div : s:={2..5};  chk(y,a_rmw);
      IF (x.am=am_imm) & (x.n=1) THEN RETURN END;
    |mei : s:={2,5};   chk(y,a_rmw);
    |mod : s:={3,4,5}; chk(y,a_rmw);
    |mul : s:={5};     chk(y,a_rmw);
      IF (x.am=am_imm) & (x.n=1) THEN RETURN END;
    |quo : s:={4,5};   chk(y,a_rmw);
      IF (x.am=am_imm) & (x.n=1) THEN RETURN END;
    |rem : s:={2,4,5}; chk(y,a_rmw);
  END;
  put(0CEh);
  put2(s+isz(sz)+md(y,6)+md(x,11));
  put_index(x); put_index(y);
  put_disp(x,sz); put_disp(y,sz);
END int_CE;

PROCEDURE cmd(c: cop; VAL x,y: sym.access; sz: INTEGER);
BEGIN
  CASE c OF
    |absf,addf,cmpf,divf,movf,mulf,negf,subf           : fpp(c,x,y,sz);
    |add,addc,addr,and,bic,cmp,mov,or,sub,subc,tbit,xor: int(c,x,y,sz);
    |abs,addp,ash,cbit,cbiti,sbit,sbiti,
                       com,ibit,lsh,neg,not,rot        : int_4E(c,x,y,sz);
    |dei,div,mei,mod,mul,quo,rem                       : int_CE(c,x,y,sz);
    |ffs                                               : int_6E(c,x,y,sz);
  END;
END cmd;

PROCEDURE flag;
BEGIN
  put({1,4,6,7});
END flag;

PROCEDURE svc;
BEGIN
  put({1,5,6,7});
END svc;

PROCEDURE enter(rg: BITSET; loc_sz: INTEGER);
BEGIN
  put({1,7}); put(rg); disp(loc_sz);
END enter;

PROCEDURE exit(rg: BITSET);
  VAR i: INTEGER; m: BITSET;
BEGIN
  m:={};
  FOR i:=0 TO 7 DO IF i IN rg THEN INCL(m,7-i) END END;
  put({1,4,7}); put(m);
END exit;

PROCEDURE restore(rg: BITSET);
  VAR i: INTEGER; m: BITSET;
BEGIN
  IF rg={} THEN RETURN END;
  m:={};
  FOR i:=0 TO 7 DO IF i IN rg THEN INCL(m,7-i) END END;
  put({1,4,5,6}); put(m);
END restore;

PROCEDURE save(rg: BITSET);
BEGIN
  IF rg={} THEN RETURN END;
  put({1,5,6}); put(rg);
END save;

PROCEDURE ret(n: INTEGER);
BEGIN
  put({1,4}); disp(n);
END ret;

PROCEDURE rxp(n: INTEGER);
BEGIN
  put({1,4,5}); disp(n);
END rxp;

PROCEDURE floor(VAL x,y: sym.access; xsz,ysz: INTEGER);
BEGIN HALT(1);
END floor;

PROCEDURE round(VAL x,y: sym.access; xsz,ysz: INTEGER);
BEGIN HALT(1);
END round;

PROCEDURE trunc(VAL x,y: sym.access; xsz,ysz: INTEGER);
BEGIN
  chk(x,a_read); chk(y,a_write);
  put(3Eh);
  IF xsz=8 THEN
    put2(isz(ysz)+{3,5}+md(y,6)+md(x,11));
  ELSE
    put2(isz(ysz)+{2,3,5}+md(y,6)+md(x,11));
  END;
  put_index(x); put_index(y);
  put_disp(x,xsz); put_disp(y,ysz);
END trunc;

PROCEDURE movif(VAL x,y: sym.access; xsz,ysz: INTEGER);
BEGIN
  chk(x,a_read); chk(y,a_write);
  put(3Eh);
  IF ysz=8 THEN
    put2(isz(xsz)+md(y,6)+md(x,11));
  ELSE
    put2(isz(xsz)+{2}+md(y,6)+md(x,11));
  END;
  put_index(x); put_index(y);
  put_disp(x,xsz); put_disp(y,ysz);
END movif;

PROCEDURE movfl(VAL x,y: sym.access);
BEGIN HALT(1);
END movfl;

PROCEDURE movlf(VAL x,y: sym.access);
BEGIN HALT(1);
END movlf;

PROCEDURE movx(VAL x,y: sym.access; xsz,ysz: INTEGER);
BEGIN
  chk(x,a_read);
  chk(y,a_write);
  IF (xsz=1) & (ysz=4) THEN
    put(0CEh);
    put2({2,3,4}+md(y,6)+md(x,11));
    put_index(x); put_index(y);
    put_disp(x,xsz); put_disp(y,ysz);
  ELSIF (xsz=2) & (ysz=4) THEN
    put(0CEh);
    put2({0,2,3,4}+md(y,6)+md(x,11));
    put_index(x); put_index(y);
    put_disp(x,xsz); put_disp(y,ysz);
  ELSIF (xsz=1) & (ysz=2) THEN
    put(0CEh);
    put2({4}+md(y,6)+md(x,11));
    put_index(x); put_index(y);
    put_disp(x,xsz); put_disp(y,ysz);
  ELSE
    int(mov,x,y,ysz);
  END;
END movx;

PROCEDURE movz(VAL x,y: sym.access; xsz,ysz: INTEGER);
BEGIN
  chk(x,a_read);
  chk(y,a_write);
  IF (xsz=1) & (ysz=4) THEN
    put(0CEh);
    put2({3,4}+md(y,6)+md(x,11));
    put_index(x); put_index(y);
    put_disp(x,xsz); put_disp(y,ysz);
  ELSIF (xsz=2) & (ysz=4) THEN
    put(0CEh);
    put2({0,3,4}+md(y,6)+md(x,11));
    put_index(x); put_index(y);
    put_disp(x,xsz); put_disp(y,ysz);
  ELSIF (xsz=1) & (ysz=2) THEN
    put(0CEh);
    put2({2,4}+md(y,6)+md(x,11));
    put_index(x); put_index(y);
    put_disp(x,xsz); put_disp(y,ysz);
  ELSE
    int(mov,x,y,ysz);
  END;
END movz;

PROCEDURE lfsr(VAL x: sym.access);
BEGIN HALT(1);
END lfsr;

PROCEDURE sfsr(VAL x: sym.access);
BEGIN HALT(1);
END sfsr;

PROCEDURE lpr(VAL x: sym.access; rg: cpu_reg; sz: INTEGER);
BEGIN
  chk(x,a_read);
  put2(isz(sz)+{2,3,5,6}+BITSET(rg<<7)*{7..10}+md(x,11));
  put_index(x); put_disp(x,sz);
END lpr;

PROCEDURE spr(VAL x: sym.access; rg: cpu_reg; sz: INTEGER);
BEGIN
  chk(x,a_write);
  put2(isz(sz)+{2,3,5}+BITSET(rg<<7)*{7..10}+md(x,11));
  put_index(x); put_disp(x,sz);
END spr;

PROCEDURE adjsp(VAL x: sym.access; sz: INTEGER);
BEGIN
  chk(x,a_read);
  IF (x.am=am_imm) & (x.n=0) THEN RETURN END;
  put2(isz(sz)+{2..6,8,10}+md(x,11));
  put_index(x);
  put_disp(x,sz);
END adjsp;

PROCEDURE s(cc: condition; VAL y: sym.access; sz: INTEGER);
BEGIN
  chk(y,a_write);
  put2(isz(sz)+{2,3,4,5}+BITSET(cc<<7)*{7..10}+md(y,11));
  put_index(y); put_disp(y,sz);
END s;

PROCEDURE index(VAL length,index: sym.access; accum,sz: INTEGER);
BEGIN
  put(02Eh);
  chk(length,a_read);
  chk(index,a_read);
  put2(isz(sz)+{2}+BITSET(accum<<3)*{3..5}+md(index,6)+md(length,11));
  put_index(length); put_index(index);
  put_disp(length,sz); put_disp(index,sz);
END index;

PROCEDURE check(VAL bounds,src: sym.access; dest,sz: INTEGER);
BEGIN
  put(0EEh);
  chk(bounds,a_addr);
  chk(src,a_read);
  put2(isz(sz)+BITSET(dest<<3)*{3..5}+md(src,6)+md(bounds,11));
  put_index(bounds); put_index(src);
  put_disp(bounds,0); put_disp(src,sz);
END check;

PROCEDURE cmpm(x,y: sym.access; sz,no: INTEGER);
BEGIN HALT(1);
END cmpm;

PROCEDURE movm(VAL x,y: sym.access; sz,no: INTEGER);
BEGIN
  chk(x,a_addr);
  chk(y,a_addr);
  put(0CEh);
  put2(isz(sz)+md(y,6)+md(x,11));
  put_index(x); put_index(y);
  put_disp(x,sz); put_disp(y,sz);
  disp((no-1)*sz);
END movm;

PROCEDURE ext(base,dest: sym.access; sz,offset,length: INTEGER);
BEGIN
  chk(base,a_regaddr);
  chk(dest,a_write);
  put(02Eh);
  put2(isz(sz)+BITSET(offset<<3)*{3..5}+md(dest,6)+md(base,11));
  put_index(base); put_index(dest);
  put_disp(base,0); put_disp(dest,sz);
  disp(length);
END ext;

PROCEDURE exts(base,dest: sym.access; sz,offset,length: INTEGER);
BEGIN
  chk(base,a_regaddr);
  chk(dest,a_write);
  put(0CEh);
  put2(isz(sz)+{2,3}+md(dest,6)+md(base,11));
  put_index(base); put_index(dest);
  put_disp(base,0); put_disp(dest,sz);
  put(BITSET(offset<<5)*{5..7}+BITSET(length-1)*{0..4});
END exts;

PROCEDURE ins(src,base : sym.access; sz,offset,length: INTEGER);
BEGIN
  chk(base,a_regaddr);
  chk(src,a_read);
  put(0AEh);
  put2(isz(sz)+BITSET(offset<<3)*{3..5}+md(base,6)+md(src,11));
  put_index(src); put_index(base);
  put_disp(src,sz); put_disp(base,0);
  disp(length);
END ins;

PROCEDURE inss (src,base : sym.access; sz,offset,length: INTEGER);
BEGIN
  chk(src,a_read);
  chk(base,a_regaddr);
  put(0CEh);
  put2(isz(sz)+{3}+md(base,6)+md(src,11));
  put_index(src); put_index(base);
  put_disp(src,sz); put_disp(base,0);
  put(BITSET(offset<<5)*{5..7}+BITSET(length-1)*{0..4});
END inss;

PROCEDURE cxpd(VAL x: sym.access);
BEGIN
  chk(x,a_addr);
  put2({0,1,2,3,4,5,6}+md(x,11));
  put_index(x); put_disp(x,4);
END cxpd;

PROCEDURE movs(sz: INTEGER; back,while,until: BOOLEAN);
  VAR s: BITSET;
BEGIN
  put(0Eh);
  put(isz(sz));
  IF back THEN s:={0} ELSE s:={} END;
  IF while THEN s:=s+{1} END;
  IF until THEN s:=s+{1,2} END;
  put(s);
END movs;

PROCEDURE movst(back,while,until: BOOLEAN);
  VAR s: BITSET;
BEGIN
  put(0Eh);
  put({7});
  IF back THEN s:={0} ELSE s:={} END;
  IF while THEN s:=s+{1} END;
  IF until THEN s:=s+{1,2} END;
  put(s);
END movst;

PROCEDURE cmps(sz: INTEGER; back,while,until: BOOLEAN);
  VAR s: BITSET;
BEGIN
  put(0Eh);
  put(isz(sz)+{2});
  IF back THEN s:={0} ELSE s:={} END;
  IF while THEN s:=s+{1} END;
  IF until THEN s:=s+{1,2} END;
  put(s);
END cmps;

PROCEDURE cmpst(back,while,until: BOOLEAN);
  VAR s: BITSET;
BEGIN
  put(0Eh);
  put({2,7});
  IF back THEN s:={0} ELSE s:={} END;
  IF while THEN s:=s+{1} END;
  IF until THEN s:=s+{1,2} END;
  put(s);
END cmpst;

PROCEDURE skps (sz: INTEGER; back,while,until: BOOLEAN);
  VAR s: BITSET;
BEGIN
  put(0Eh);
  put(isz(sz)+{2,3});
  IF back THEN s:={0} ELSE s:={} END;
  IF while THEN s:=s+{1} END;
  IF until THEN s:=s+{1,2} END;
  put(s);
END skps;

PROCEDURE skpst(back,while,until: BOOLEAN);
  VAR s: BITSET;
BEGIN
  put(0Eh);
  put({2,3,7});
  IF back THEN s:={0} ELSE s:={} END;
  IF while THEN s:=s+{1} END;
  IF until THEN s:=s+{1,2} END;
  put(s);
END skpst;

PROCEDURE case(VAL src: sym.access; sz: INTEGER);
BEGIN
  chk(src,a_read);
  put2(isz(sz)+{2..6,8,9,10}+md(src,11));
  put_index(src); put_disp(src,sz);
END case;

PROCEDURE jump(VAL x: sym.access);
BEGIN
  chk(x,a_addr);
  put2({0..6,9}+md(x,11));
  put_index(x); put_disp(x,4);
END jump;

PROCEDURE acb(inc: INTEGER; index: sym.access; dest,sz: INTEGER);
BEGIN
  chk(index,a_rmw);
  put2(isz(sz)+{2,3,6}+BITSET(inc<<7)*{7..10}+md(index,11));
  put_index(index); put_disp(index,sz); disp(dest);
END acb;

PROCEDURE jsr(x: sym.access);
BEGIN
  chk(x,a_addr);
  put2({0..6,9,10}+md(x,11));
  put_index(x); put_disp(x,4);
END jsr;

PROCEDURE lmr(x: sym.access; rg: mmu_reg);
BEGIN
  chk(x,a_read);
  put(1Eh);
  put2({0,1,3}+BITSET(rg<<7)*{7..10}+md(x,11));
  put_index(x); put_disp(x,4);
END lmr;

PROCEDURE smr(x: sym.access; rg: mmu_reg);
BEGIN
  chk(x,a_read);
  put(1Eh);
  put2({0,1,2,3}+BITSET(rg<<7)*{7..10}+md(x,11));
  put_index(x); put_disp(x,4);
END smr;

PROCEDURE bispsr(x: sym.access; sz: INTEGER);
BEGIN
  chk(x,a_read);
  ASSERT(sz IN {1,2});
  IF sz=1 THEN
    put2({2..6,8,9}+md(x,11));
  ELSE
    put2({0,2..6,8,9}+md(x,11));
  END;
  put_index(x); put_disp(x,sz);
END bispsr;

PROCEDURE bicpsr(x: sym.access; sz: INTEGER);
BEGIN
  chk(x,a_read);
  ASSERT(sz IN {1,2});
  IF sz=1 THEN
    put2({2..6,8}+md(x,11));
  ELSE
    put2({0,2..6,8}+md(x,11));
  END;
  put_index(x); put_disp(x,sz);
END bicpsr;


END nsCmd.
