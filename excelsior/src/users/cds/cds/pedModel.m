IMPLEMENTATION  MODULE pedModel; (*$U+ Sem 13-Sep-86. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT  bio : libBIO;
IMPORT  mem : libHeap;
IMPORT  err : libCrash;

IMPORT  tty : Terminal;

CONST
  magic1=334;
  magic2=145;
  magic3=789;
  magic4=666;
  empty_nm='.....';
  file_ext='.pcb';

VAR
  f   : bio.FILE;
  nm  : ARRAY [0..79] OF CHAR;
  bf  : ARRAY [0..1023] OF WORD;
  bc  : INTEGER;
  tag : mem.AREA;
  trap: err.trap;

PROCEDURE mem_fail(n: INTEGER);
BEGIN
  err.raise('Не хватило памяти.\nЗапрос %d b',n*4);
END mem_fail;

PROCEDURE new(VAR a: ADDRESS; n: INTEGER);
BEGIN
  ASSERT(n>=0);
  mem.alloc_tag(a,n,tag);
  IF (n>0) & (a=NIL) THEN mem_fail(n) END;
END new;

PROCEDURE resize(VAR a: ADDRESS; VAR h: INTEGER; l,b: INTEGER);
  VAR s: INTEGER;
BEGIN
  s:=(l*b+3) DIV 4;
  ASSERT(s>=0);
  IF h<0 THEN
    mem.alloc_tag(a,s,tag);
    IF (s>0) & (a=NIL) THEN mem_fail(s) END;
  ELSE
    IF NOT mem.realloc(a,s) THEN mem_fail(s) END;
  END;
  h:=l-1;
END resize;

PROCEDURE dispose(VAR a: ADDRESS; n: INTEGER);
BEGIN
  mem.dealloc_adr(a);
END dispose;

WITH STORAGE (NEW: new; RESIZE: resize; DISPOSE: dispose);

PROCEDURE setName(n: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(n)) & (n[i]>' ') & (n[i]#'.') & (i<=HIGH(nm)-5) DO
    nm[i]:=n[i]; INC(i);
  END;
  IF i=0 THEN nm:='tmp'; i:=3 END;
  j:=0;
  WHILE file_ext[j]#0c DO nm[i]:=file_ext[j]; INC(i); INC(j) END;
  FOR j:=i TO HIGH(nm) DO nm[j]:=0c END;
END setName;

PROCEDURE get(a: ADDRESS; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO n-1 DO
    IF bc=1024 THEN
      bc:=0;
      bio.read(f,ADR(bf),4096);
    END;
    a^:=bf[bc]; INC(bc); INC(a);
  END;
END get;

PROCEDURE skip(n: INTEGER);
BEGIN
  INC(bc,n);
  IF bc>1024 THEN
    REPEAT
      DEC(bc,1024); bio.seek(f,4096,1);
    UNTIL bc<=1024;
    IF bc<1024 THEN
      bio.read(f,ADR(bf),4096);
    END;
  END;
END skip;

PROCEDURE put(a: ADDRESS; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO n-1 DO
    IF bc=1024 THEN
      bio.write(f,ADR(bf),4096);
      bc:=0;
    END;
    bf[bc]:=a^; INC(bc); INC(a);
  END;
END put;

PROCEDURE r(VAR n: WORD);
BEGIN
  IF bc=1024 THEN
    bc:=0;
    bio.read(f,ADR(bf),4096);
  END;
  n:=bf[bc]; INC(bc);
END r;

PROCEDURE w(n: WORD);
BEGIN
  IF bc=1024 THEN
    bio.write(f,ADR(bf),4096);
    bc:=0;
  END;
  bf[bc]:=n; INC(bc);
END w;

PROCEDURE fail;
BEGIN
  err.raise('Не могу прочесть модель из файла "%s".\n'
            'Файл содержит некорректные данные.',nm);
END fail;

PROCEDURE RdModel(VAL name: ARRAY OF CHAR; VAR b: board);
  PROCEDURE FindSig(n: INTEGER): signal;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(b^.sigs) DO
      IF b^.sigs[i]^.gang=n THEN RETURN b^.sigs[i] END;
    END;
    fail;
  END FindSig;
  PROCEDURE FindTyp(n: INTEGER): ctype;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(b^.typs) DO
      IF b^.typs[i]^.id=n THEN RETURN b^.typs[i] END;
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
  setName(name);
  bio.open(f,nm,'r');
  bc:=1024;
  r(n); IF n#magic1 THEN fail END;
  mem.ALLOCATE(b,SIZE(b^));
  b^.mem:=mem.FREE;
  get(ADR(b^.name),4);
  r(b^.x);
  r(b^.y);
  r(b^.lays);
  ---- chip types library ----
  r(n); IF n#magic2 THEN fail END;
  r(k); NEW(b^.typs,k);
tty.print('read %d types\n',k);
  FOR i:=0 TO HIGH(b^.typs) DO
    NEW(t);
    b^.typs[i]:=t;
    get(ADR(t^.name),4);
    r(t^.id);
    r(t^.x);
    r(t^.y);
    r(t^.xnm);
    r(t^.ynm);
    ---- pin types ----
    r(k); NEW(t^.pins,k);
    pt:=ADR(t^.pins);
    FOR j:=0 TO HIGH(t^.pins) DO
      r(pt^.x);
      r(pt^.y);
      r(pt^.tool);
      r(k); NEW(pt^.cu,k);
      get(ADR(pt^.cu),SIZE(pt^.cu));
      pt:=ADDRESS(pt)+SIZE(tpin_rec);
    END;
  END;
  ---- signals ----
  r(n); IF n#magic3 THEN fail END;
  r(k); NEW(b^.sigs,k);
tty.print('read %d signals\n',k);
  FOR i:=0 TO HIGH(b^.sigs) DO
    NEW(s);
    b^.sigs[i]:=s;
    get(ADR(s^.name),4);
    r(s^.type);
    r(s^.gang);
    s^.pins^.HIGH:=0;
    r(s^.hard);
    r(k); NEW(s^.cu,k);
    get(ADR(s^.cu),SIZE(s^.cu));
  END;
  ---- chips ----
  r(n); IF n#magic4 THEN fail END;
  r(k); NEW(b^.chps,k);
tty.print('read %d chips\n',k);
  FOR i:=0 TO HIGH(b^.chps) DO
    NEW(c);
    b^.chps[i]:=c;
    get(ADR(c^.name),4);
    r(k); c^.type:=FindTyp(k);
    r(c^.x);
    r(c^.y);
    r(c^.r);
    ---- pins ----
    r(k); NEW(c^.pins,k);
    pp:=ADR(c^.pins);
    FOR j:=0 TO HIGH(c^.pins) DO
      pp^.chp:=c; pp^.no :=j;
      r(k); pp^.sig:=FindSig(k);
      INC(pp^.sig^.pins^.HIGH);
      pp:=ADDRESS(pp)+SIZE(pin_rec);
    END;
  END;
  bio.close(f);
  FOR i:=0 TO HIGH(b^.sigs) DO
    WITH b^.sigs[i]^ DO n:=pins^.HIGH; NEW(pins,n); gang:=0 END;
  END;
  FOR i:=0 TO HIGH(b^.chps) DO
    c:=b^.chps[i];
    FOR j:=0 TO HIGH(c^.pins) DO
      s:=c^.pins[j].sig;
      s^.pins[s^.gang]:=ADR(c^.pins[j]);
      INC(s^.gang);
    END;
  END;
END RdModel;

PROCEDURE read_model(VAL name: ARRAY OF CHAR): board;
  VAR b: board;
BEGIN
  ASSERT(f=bio.FILE(NIL));
  ASSERT(tag=mem.FREE);
  b:=NIL;
  IF err.enter(trap) THEN
    IF f#bio.FILE(NIL) THEN bio.close(f) END;
    mem.dealloc_all(tag); mem.dealloc_adr(b);
    err.re_raise(trap);
  END;
  RdModel(name,b);
  mem.change_all(tag,b^.mem);
  err.exit(trap);
  RETURN b;
END read_model;

PROCEDURE WrModel(VAL name: ARRAY OF CHAR; b: board);
  VAR pt: POINTER TO tpin_rec;
      pp: pin;
      t : ctype;
      s : signal;
      c : chip;
      i,j,n: INTEGER;
BEGIN
  setName(name);
  bio.create(f,nm,'w',0);
  bc:=0;
  w(magic1);
  put(ADR(b^.name),4);
  w(b^.x);
  w(b^.y);
  w(b^.lays);
  ---- chip types library ----
  w(magic2);
  w(HIGH(b^.typs)+1);
  FOR i:=0 TO HIGH(b^.typs) DO
    t:=b^.typs[i];
    put(ADR(t^.name),4);
    w(t);
    w(t^.x);
    w(t^.y);
    w(t^.xnm);
    w(t^.ynm);
    ---- pin types ----
    w(HIGH(t^.pins)+1);
    pt:=ADR(t^.pins);
    FOR j:=0 TO HIGH(t^.pins) DO
      w(pt^.x);
      w(pt^.y);
      w(pt^.tool);
      w(HIGH(pt^.cu)+1);
      put(ADR(pt^.cu),SIZE(pt^.cu));
      pt:=ADDRESS(pt)+SIZE(tpin_rec);
    END;
  END;
  ---- signals ----
  w(magic3);
  w(HIGH(b^.sigs)+1);
  FOR i:=0 TO HIGH(b^.sigs) DO
    s:=b^.sigs[i];
    put(ADR(s^.name),4);
    w(s^.type);
    w(s);
    w(s^.hard);
    w(HIGH(s^.cu)+1);
    put(ADR(s^.cu),SIZE(s^.cu));
  END;
  ---- chips ----
  w(magic4);
  w(HIGH(b^.chps)+1);
  FOR i:=0 TO HIGH(b^.chps) DO
    c:=b^.chps[i];
    put(ADR(c^.name),4);
    w(c^.type);
    w(c^.x);
    w(c^.y);
    w(c^.r);
    ---- pins ----
    w(HIGH(c^.pins)+1);
    pp:=ADR(c^.pins);
    FOR j:=0 TO HIGH(c^.pins) DO
      w(pp^.sig); pp:=ADDRESS(pp)+SIZE(pin_rec);
    END;
  END;
  IF bc>0 THEN bio.write(f,ADR(bf),4096) END;
  bio.close(f);
END WrModel;

PROCEDURE write_model(VAL name: ARRAY OF CHAR; b: board);
BEGIN
  f:=bio.FILE(NIL);
  IF err.enter(trap) THEN
    IF f#bio.FILE(NIL) THEN bio.close(f) END;
    err.re_raise(trap);
  END;
  WrModel(name,b);
  err.exit(trap);
END write_model;

PROCEDURE RdType(fnm: ARRAY OF CHAR; tnm: string; host: board; VAR t: ctype);
  VAR pt: POINTER TO tpin_rec;
      i,j,n,k,tno: INTEGER;
BEGIN
  setName(fnm);
  bio.open(f,nm,'r');
  bc:=1024;
  NEW(t);
  r(n); IF n#magic1 THEN fail END;
  skip(7);
  ---- chip types library ----
  r(n); IF n#magic2 THEN fail END;
  r(tno);
  FOR i:=0 TO tno-1 DO
    get(ADR(t^.name),4);
    IF t^.name=tnm THEN
      r(t^.id);
      r(t^.x);
      r(t^.y);
      r(t^.xnm);
      r(t^.ynm);
      ---- pin types ----
      r(k); NEW(t^.pins,k);
      pt:=ADR(t^.pins);
      FOR j:=0 TO HIGH(t^.pins) DO
        r(pt^.x);
        r(pt^.y);
        r(pt^.tool);
        r(k); NEW(pt^.cu,k);
        get(ADR(pt^.cu),SIZE(pt^.cu));
        pt:=ADDRESS(pt)+SIZE(tpin_rec);
      END;
      bio.close(f);
      RESIZE(host^.typs,HIGH(host^.typs)+2);
      host^.typs[HIGH(host^.typs)]:=t;
      RETURN
    ELSE
      skip(5); r(n);
      FOR j:=0 TO n-1 DO
        skip(3); r(k); skip(k*SIZE(seg_rec));
      END;
    END;
  END;
  bio.close(f);
  err.raise('В файле "%s" не найден элемент "%s".',fnm,tnm);
END RdType;

PROCEDURE read_type(fnm: ARRAY OF CHAR; tnm: string; host: board): ctype;
  VAR t: ctype;
BEGIN
  t:=NIL; f:=bio.FILE(NIL);
  IF err.enter(trap) THEN
    IF f#bio.FILE(NIL) THEN bio.close(f) END;
    mem.dealloc_all(tag); err.re_raise(trap);
  END;
  RdType(fnm,tnm,host,t);
  mem.change_all(tag,host^.mem);
  err.exit(trap);
  RETURN t;
END read_type;

PROCEDURE cre_board(VAR b: board);
BEGIN
  mem.ALLOCATE(b,SIZE(b^));
  NEW(b^.sigs);
  NEW(b^.chps);
  NEW(b^.typs);
  b^.x:=0; b^.y:=0; b^.lays:={};
  b^.name:=empty_nm; b^.mem:=mem.FREE;
END cre_board;

PROCEDURE del_board(VAR b: board);
BEGIN
  IF b=NIL THEN RETURN END;
  mem.dealloc_all(b^.mem);
  mem.DEALLOCATE(b,SIZE(b^));
END del_board;

PROCEDURE cre_signal(VAR s: signal; h: board);
BEGIN
  NEW(s);
  NEW(s^.pins);
  NEW(s^.cu);
  s^.type:=signal_type{};
  s^.gang:=0;
  s^.hard:=0;
  s^.name:=empty_nm;
  RESIZE(h^.sigs,HIGH(h^.sigs)+2);
  h^.sigs[HIGH(h^.sigs)]:=s;
  mem.change_all(tag,h^.mem);
END cre_signal;

PROCEDURE del_signal(VAR s: signal; host: board);
  VAR i,j: INTEGER; p: pin;
BEGIN
  DISPOSE(s^.cu);
  FOR i:=0 TO HIGH(s^.pins) DO
    p:=s^.pins[i];
    ASSERT(p^.sig=s);
    cre_signal(p^.sig,host);
    NEW(p^.sig^.pins,1);
    p^.sig^.pins[0]:=p;
  END;
  DISPOSE(s^.pins);
  i:=0;
  WHILE host^.sigs[i]#s DO INC(i) END;
  host^.sigs[i]:=host^.sigs[HIGH(host^.sigs)];
  RESIZE(host^.sigs,HIGH(host^.sigs));
  mem.change_all(tag,host^.mem);
END del_signal;

PROCEDURE cre_chip(VAR cc: chip; host: board; t: ctype);
  VAR s: signal; i,n: INTEGER; c: chip;
BEGIN
  NEW(c);
  c^.type:=t; c^.x:=0; c^.y:=0; c^.r:=0; c^.name:=empty_nm;
  NEW(c^.pins,HIGH(t^.pins)+1);
  FOR i:=0 TO HIGH(c^.pins) DO
    c^.pins[i].no:=i;
    c^.pins[i].chp:=c;
    c^.pins[i].sig:=NIL;
    cre_signal(s,host);
    tie(s,c,i,host);
  END;
  RESIZE(host^.chps,HIGH(host^.chps)+2);
  host^.chps[HIGH(host^.chps)]:=c;
  mem.change_all(tag,host^.mem);
  cc:=c;
END cre_chip;

PROCEDURE del_chip(VAR c: chip; host: board);
END del_chip;

PROCEDURE cre_ctype(VAR tt: ctype; no: INTEGER; host: board);
  VAR i: INTEGER; t: ctype;
BEGIN
  NEW(t);
  t^.x:=0; t^.y:=0;
  t^.name:=empty_nm;
  t^.xnm:=0; t^.ynm:=0;
  NEW(t^.pins,no);
  RESIZE(host^.typs,HIGH(host^.typs)+2);
  host^.typs[HIGH(host^.typs)]:=t;
  tt:=t;
  mem.change_all(tag,host^.mem);
  FOR i:=0 TO no-1 DO
    t^.pins[i].x:=0; t^.pins[i].y:=0;
    NEW(t^.pins[i].cu);
    t^.pins[i].tool:=0;
  END;
END cre_ctype;

PROCEDURE tie(s: signal; c: chip; n: INTEGER; host: board);
  VAR p: pin; os: signal; i,j: INTEGER;
BEGIN
  ASSERT(n>=0);
  ASSERT(n<=HIGH(c^.pins));
  p:=ADR(c^.pins[n]);
  ASSERT(p^.no =n);
  ASSERT(p^.chp=c);
  os:=p^.sig;
  IF os#NIL THEN
    IF s=os THEN RETURN END;
    i:=0; WHILE os^.pins[i]#p DO INC(i) END;
    os^.pins[i]:=os^.pins[HIGH(os^.pins)];
    RESIZE(os^.pins,HIGH(os^.pins));
    IF (HIGH(os^.pins)<0) & (os^.name=empty_nm) THEN del_signal(os,host) END;
  END;
  p^.sig:=s;
  RESIZE(s^.pins,HIGH(s^.pins)+2);
  s^.pins[HIGH(s^.pins)]:=p;
  mem.change_all(tag,host^.mem);
END tie;

BEGIN
  tag:=mem.FREE; f:=bio.FILE(NIL);
END pedModel.
