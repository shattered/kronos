IMPLEMENTATION  MODULE pedModel; (* Sem 13-Sep-86. (c) KRONOS *)

IMPORT BIO;
FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM KRONOS     IMPORT  MOVE;
FROM cdsHeap    IMPORT  Allocate, Deallocate, HardAllocate;
FROM FsPublic   IMPORT  File, FileName;

CONST magic1=334;
      magic2=145;
      magic3=789;
      magic4=666;
      empty_nm='.....';

VAR f : File;
    nm: FileName;
    bf: ARRAY [0..1023] OF WORD;
    bc: INTEGER;
    bl: INTEGER;

--CONST chk=BIO.checkHALT;

PROCEDURE chk(b: BOOLEAN; n: ARRAY OF CHAR);
BEGIN
  IF b THEN HALT(1) END;
END chk;

PROCEDURE get(a: ADDRESS; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO n-1 DO
    IF bc=1024 THEN
      bc:=0; INC(bl);
      chk(BIO.bRead(f,bl,ADR(bf),4096),nm);
    END;
    a^:=bf[bc]; INC(bc); INC(a);
  END;
END get;

PROCEDURE skip(n: INTEGER);
BEGIN
  INC(bc,n);
  IF bc>1024 THEN
    REPEAT DEC(bc,1024); INC(bl) UNTIL bc<=1024;
    IF bc<1024 THEN
      chk(BIO.bRead(f,bl,ADR(bf),4096),nm);
    END;
  END;
END skip;

PROCEDURE put(a: ADDRESS; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO n-1 DO
    IF bc=1024 THEN
      chk(BIO.bWrite(f,bl,ADR(bf),4096),nm);
      bc:=0; INC(bl);
    END;
    bf[bc]:=a^; INC(bc); INC(a);
  END;
END put;

PROCEDURE r(VAR n: WORD);
BEGIN
  IF bc=1024 THEN
    bc:=0; INC(bl);
    chk(BIO.bRead(f,bl,ADR(bf),4096),nm);
  END;
  n:=bf[bc]; INC(bc);
END r;

PROCEDURE w(n: WORD);
BEGIN
  IF bc=1024 THEN
    chk(BIO.bWrite(f,bl,ADR(bf),4096),nm);
    bc:=0; INC(bl);
  END;
  bf[bc]:=n; INC(bc);
END w;

PROCEDURE fail;
BEGIN HALT(1)
END fail;

PROCEDURE ReadModel(name: ARRAY OF CHAR): board;
  VAR b: board;
  PROCEDURE FindSig(n: INTEGER): signal;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO b^.sno-1 DO
      IF b^.sigs^[i]^.gang=n THEN RETURN b^.sigs^[i] END;
    END;
    fail;
  END FindSig;
  PROCEDURE FindTyp(n: INTEGER): ctype;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO b^.tno-1 DO
      IF b^.typs^[i]^.id=n THEN RETURN b^.typs^[i] END;
    END;
    fail;
  END FindTyp;
  VAR pt: POINTER TO tpin_rec;
      pp: pin;
      t : ctype;
      s : signal;
      c : chip;
      i,j,n,k: INTEGER;
BEGIN
  nm:=name;
  nm[HIGH(nm)]:=0c;
  chk(BIO.OpenOnDir(BIO.CD(),f,nm),nm);
  bc:=1024; bl:=-1;
  r(n); IF n#magic1 THEN fail END;
  Allocate(b,SIZE(b^));
  get(ADR(b^.name),4);
  r(b^.x);
  r(b^.y);
  r(b^.lays);
  ---- chip types library ----
  r(n); IF n#magic2 THEN fail END;
  r(b^.tno);
  Allocate(b^.typs,b^.tno);
  FOR i:=0 TO b^.tno-1 DO
    r(n); Allocate(t,n);
    b^.typs^[i]:=t;
    get(ADR(t^.name),4);
    r(t^.id);
    r(t^.x);
    r(t^.y);
    r(t^.xnm);
    r(t^.ynm);
    ---- pin types ----
    r(t^.pno); IF t^.pno*5+10#n THEN fail END;
    pt:=ADR(t^.pins);
    FOR j:=0 TO t^.pno-1 DO
      r(pt^.x);
      r(pt^.y);
      r(pt^.tool);
      r(pt^.cno); Allocate(pt^.cu,pt^.cno*3);
      get(pt^.cu,pt^.cno*3);
      INC(ADDRESS(pt),5);
    END;
  END;
  ---- signals ----
  r(n); IF n#magic3 THEN fail END;
  r(b^.sno); Allocate(b^.sigs,b^.sno);
  FOR i:=0 TO b^.sno-1 DO
    Allocate(s,SIZE(s^));
    b^.sigs^[i]:=s;
    get(ADR(s^.name),4);
    r(s^.type);
    r(s^.gang);
    r(s^.pno);
    r(s^.hard);
    r(s^.cno); Allocate(s^.cu,s^.cno*3);
    get(s^.cu,s^.cno*3);
  END;
  ---- chips ----
  r(n); IF n#magic4 THEN fail END;
  r(b^.cno); Allocate(b^.chps,b^.cno);
  FOR i:=0 TO b^.cno-1 DO
    r(n); Allocate(c,n);
    b^.chps^[i]:=c;
    get(ADR(c^.name),4);
    r(k); c^.type:=FindTyp(k);
    r(c^.x);
    r(c^.y);
    r(c^.r);
    ---- pins ----
    r(c^.pno);
    IF c^.pno*3+9#n THEN fail END;
    pp:=ADR(c^.pins);
    FOR j:=0 TO c^.pno-1 DO
      pp^.chp:=c;
      pp^.no :=j;
      r(k); pp^.sig:=FindSig(k);
      INC(pp^.sig^.pno);
      INC(ADDRESS(pp),3);
    END;
  END;
  chk(BIO.Close(f),nm);
  FOR i:=0 TO b^.sno-1 DO
    Allocate(b^.sigs^[i]^.pins,b^.sigs^[i]^.pno); b^.sigs^[i]^.pno:=0;
  END;
  FOR i:=0 TO b^.cno-1 DO
    c:=b^.chps^[i];
    FOR j:=0 TO c^.pno-1 DO
      s:=c^.pins[j].sig; s^.pins^[s^.pno]:=ADR(c^.pins[j]); INC(s^.pno);
    END;
  END;
  RETURN b;
END ReadModel;

PROCEDURE WriteModel(name: ARRAY OF CHAR; b: board);
  VAR pt: POINTER TO tpin_rec;
      pp: pin;
      t : ctype;
      s : signal;
      c : chip;
      i,j,n: INTEGER;
BEGIN
  nm:=name;
  nm[HIGH(nm)]:=0c;
  chk(BIO.Create(f),nm);
  bc:=0; bl:=0;
  w(magic1);
  put(ADR(b^.name),4);
  w(b^.x);
  w(b^.y);
  w(b^.lays);
  ---- chip types library ----
  w(magic2);
  w(b^.tno);
  FOR i:=0 TO b^.tno-1 DO
    t:=b^.typs^[i];
    w(t^.pno*5+10);
    put(ADR(t^.name),4);
    w(t);
    w(t^.x);
    w(t^.y);
    w(t^.xnm);
    w(t^.ynm);
    ---- pin types ----
    w(t^.pno);
    pt:=ADR(t^.pins);
    FOR j:=0 TO t^.pno-1 DO
      w(pt^.x);
      w(pt^.y);
      w(pt^.tool);
      w(pt^.cno);
      put(pt^.cu,pt^.cno*3);
      INC(ADDRESS(pt),5);
    END;
  END;
  ---- signals ----
  w(magic3);
  w(b^.sno);
  FOR i:=0 TO b^.sno-1 DO
    s:=b^.sigs^[i];
    put(ADR(s^.name),4);
    w(s^.type);
    w(s);
    w(s^.pno);
    w(s^.hard);
    w(s^.cno);
    put(s^.cu,s^.cno*3);
  END;
  ---- chips ----
  w(magic4);
  w(b^.cno);
  FOR i:=0 TO b^.cno-1 DO
    c:=b^.chps^[i];
    w(c^.pno*3+9);
    put(ADR(c^.name),4);
    w(c^.type);
    w(c^.x);
    w(c^.y);
    w(c^.r);
    ---- pins ----
    w(c^.pno);
    pp:=ADR(c^.pins);
    FOR j:=0 TO c^.pno-1 DO w(pp^.sig); INC(ADDRESS(pp),3) END;
  END;
  IF bc>0 THEN chk(BIO.bWrite(f,bl,ADR(bf),4096),nm) END;
  BIO.SetEof(f,bl*4096+bc*4);
  chk(BIO.Link(BIO.CD(),nm,f),nm);
  chk(BIO.Close(f),nm);
END WriteModel;

PROCEDURE ReadType(fnm: ARRAY OF CHAR; tnm: string; host: board): ctype;
  VAR pt: POINTER TO tpin_rec;
      t : ctype;
      ta: ctype_arr;
      i,j,n,k,tno: INTEGER;
BEGIN
  nm:=fnm;
  nm[HIGH(nm)]:=0c;
  chk(BIO.OpenOnDir(BIO.CD(),f,nm),nm);
  bc:=1024; bl:=-1;
  r(n); IF n#magic1 THEN fail END;
  skip(7);
  ---- chip types library ----
  r(n); IF n#magic2 THEN fail END;
  r(tno);
  FOR i:=0 TO tno-1 DO
    r(n); Allocate(t,n);
    get(ADR(t^.name),4);
    r(t^.id);
    r(t^.x);
    r(t^.y);
    r(t^.xnm);
    r(t^.ynm);
    IF t^.name=tnm THEN
      ---- pin types ----
      r(t^.pno); IF t^.pno*5+10#n THEN fail END;
      pt:=ADR(t^.pins);
      FOR j:=0 TO t^.pno-1 DO
        r(pt^.x);
        r(pt^.y);
        r(pt^.tool);
        r(pt^.cno); Allocate(pt^.cu,pt^.cno*3);
        get(pt^.cu,pt^.cno*3);
        INC(ADDRESS(pt),5);
      END;
      chk(BIO.Close(f),nm);
      Allocate(ta,host^.tno+1);
      MOVE(ta,host^.typs,host^.tno); ta^[host^.tno]:=t;
      Deallocate(host^.typs,host^.tno);
      host^.typs:=ta; INC(host^.tno);
      RETURN t;
    ELSE
      r(t^.pno); IF t^.pno*5+10#n THEN fail END;
      pt:=ADR(t^.pins);
      FOR j:=0 TO t^.pno-1 DO
        r(pt^.x);
        r(pt^.y);
        r(pt^.tool);
        r(pt^.cno);
        skip(pt^.cno*3);
        INC(ADDRESS(pt),5);
      END;
      Deallocate(t,n);
    END;
  END;
  chk(BIO.Close(f),nm);
  RETURN NIL;
END ReadType;

PROCEDURE cre_board(VAR b: board);
BEGIN
  Allocate(b,SIZE(b^));
  b^.sno:=0; b^.sigs:=NIL;
  b^.cno:=0; b^.chps:=NIL;
  b^.tno:=0; b^.typs:=NIL;
  b^.x:=0; b^.y:=0; b^.lays:={};
  b^.name:=empty_nm;
END cre_board;

PROCEDURE cre_signal(VAR s: signal; host: board);
  VAR sa: signal_arr;
BEGIN
  Allocate(s,SIZE(s^));
  s^.pno:=0; s^.pins:=NIL;
  s^.cno:=0; s^.cu  :=NIL;
  s^.type:=signal_type{};
  s^.gang:=0; s^.hard:=0;
  s^.name:=empty_nm;
  Allocate(sa,host^.sno+1);
  MOVE(sa,host^.sigs,host^.sno); sa^[host^.sno]:=s;
  Deallocate(host^.sigs,host^.sno);
  host^.sigs:=sa; INC(host^.sno);
END cre_signal;

PROCEDURE del_signal(VAR s: signal; host: board);
  VAR sa: signal_arr; i,j: INTEGER; p: pin;
BEGIN
  Allocate(sa,host^.sno-1);
  Deallocate(s^.cu,s^.cno*3); s^.cno:=0;
  FOR i:=0 TO s^.pno-1 DO
    p:=s^.pins^[i];
    ASSERT(p^.sig=s);
    cre_signal(p^.sig,host);
    Allocate(p^.sig^.pins,1);
    p^.sig^.pno:=1;
    p^.sig^.pins^[0]:=p;
  END;
  Deallocate(s^.pins,s^.pno); s^.pno:=0;
  j:=0;
  FOR i:=0 TO host^.sno-1 DO
    IF host^.sigs^[i]#s THEN
      ASSERT(j<host^.sno-1);
      sa^[j]:=host^.sigs^[i]; INC(j);
    END;
  END;
  Deallocate(s,SIZE(s^));
  ASSERT(j=host^.sno-1);
  Deallocate(host^.sigs,host^.sno);
  host^.sigs:=sa; DEC(host^.sno);
END del_signal;

PROCEDURE cre_chip(VAR c: chip; host: board; t: ctype);
  VAR ca: chip_arr; s: signal; i,n: INTEGER;
BEGIN
  n:=9+3*t^.pno;
  Allocate(c,n);
  c^.type:=t; c^.x:=0; c^.y:=0; c^.r:=0;
  c^.name:=empty_nm; c^.pno:=t^.pno;
  FOR i:=0 TO c^.pno-1 DO
    c^.pins[i].no:=i;
    c^.pins[i].chp:=c;
    c^.pins[i].sig:=NIL;
    cre_signal(s,host);
    tie(s,c,i,host);
  END;
  Allocate(ca,host^.cno+1);
  MOVE(ca,host^.chps,host^.cno); ca^[host^.cno]:=c;
  Deallocate(host^.chps,host^.cno);
  host^.chps:=ca; INC(host^.cno);
END cre_chip;

PROCEDURE cre_ctype(VAR t: ctype; no: INTEGER; host: board);
  VAR ta: ctype_arr; i: INTEGER;
BEGIN
  Allocate(t,10+no*SIZE(tpin_rec));
  Allocate(ta,host^.tno+1);
  t^.x:=0; t^.y:=0;
  t^.name:=empty_nm;
  t^.xnm:=0; t^.ynm:=0;
  t^.pno:=no;
  MOVE(ta,host^.typs,host^.tno); ta^[host^.tno]:=t;
  Deallocate(host^.typs,host^.tno);
  host^.typs:=ta; INC(host^.tno);
  FOR i:=0 TO no-1 DO
    t^.pins[i].x:=0; t^.pins[i].y:=0;
    t^.pins[i].cu:=NIL; t^.pins[i].cno:=0;
    t^.pins[i].tool:=0;
  END;
END cre_ctype;

PROCEDURE tie(s: signal; c: chip; n: INTEGER; host: board);
  VAR pa,na: pin_arr; p: pin; os: signal; i,j: INTEGER;
BEGIN
  ASSERT(n>=0);
  ASSERT(n<c^.pno);
  p:=ADR(c^.pins[n]);
  ASSERT(p^.no =n);
  ASSERT(p^.chp=c);
  Allocate(na,s^.pno+1);
  os:=p^.sig;
  IF os#NIL THEN
    IF s=os THEN RETURN END;
    ASSERT(os^.pno>0);
    Allocate(pa,os^.pno-1);
    j:=0;
    FOR i:=0 TO os^.pno-1 DO
      IF os^.pins^[i]#p THEN
        ASSERT(j<os^.pno-1);
        pa^[j]:=os^.pins^[i]; INC(j)
      END;
    END;
    Deallocate(os^.pins,os^.pno);
    os^.pins:=pa; DEC(os^.pno);
    IF (os^.pno=0)&(os^.name=empty_nm) THEN del_signal(os,host) END;
  END;
  p^.sig:=s;
  MOVE(na,s^.pins,s^.pno); na^[s^.pno]:=p;
  Deallocate(s^.pins,s^.pno);
  s^.pins:=na; INC(s^.pno);
END tie;

END pedModel.
