IMPLEMENTATION MODULE cdsEmulLib; (* 15-Mar-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, PROCESS, TRANSFER, ADR;
FROM KRONOS     IMPORT  SETM, GETM;
FROM Universe   IMPORT  NewPrs, ProcessDesc;
FROM Terminal   IMPORT  print;
FROM Model      IMPORT  Object, Objects, Iterate, Tag;
FROM cdsLoader  IMPORT  Load, allocProc, DFT, SYS, TSK, lookupModule, Call;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM FsPublic   IMPORT  File, FileName, VisFSerr;
FROM BIO        IMPORT  OpenOnDir, bRead, GetEof, Close, CD, checkHALT;
FROM Image      IMPORT  image0;

VAR dft     : ARRAY [0..999] OF DFT;
    cnt     : INTEGER;
    wsp     : ARRAY [0..999] OF INTEGER;
    free    : ADDRESS;
    main,set: PROCESS;
    S,H     : ADDRESS;
    Op      : INTEGER;
    lib     : File;

PROCEDURE setter;
  VAR s,h: ADDRESS; m: ProcessDesc;
BEGIN
  SETM(GETM()-{0..1});
  LOOP
    m:=ProcessDesc(main);
    IF Op=0 THEN
      s:=m^.S_reg; h:=m^.H_reg;
      m^.S_reg:=S+1; m^.H_reg:=H;
      S^:=0;
    ELSE
      m^.S_reg:=s; m^.H_reg:=h;
      DEC(s); s^:=0;
    END;
    TRANSFER(set,main);
  END;
END setter;

PROCEDURE alloc(VAR a: ADDRESS; s: INTEGER);
BEGIN
  IF s>0 THEN Allocate(a,s) ELSE Deallocate(a,-s) END;
END alloc;

PROCEDURE get(Name: ARRAY OF CHAR; VAR code: ADDRESS; VAR sz: INTEGER;
              p: allocProc; VAR err: ARRAY OF CHAR): BOOLEAN;
  VAR fName: FileName; f: File;
      r: BOOLEAN;
BEGIN
  image0(fName,'%s.cod',Name);
  print('Read file %s\n',fName);
  r:=OpenOnDir(lib,f,fName);
  IF NOT r THEN
    sz:=(GetEof(f)+3) DIV 4;
    p(code,sz);
    r:=bRead(f,0,code,sz*4);
    r:=Close(f) OR r;
  END;
  IF r THEN
    p(code,-sz); VisFSerr(r,err);
  END;
  RETURN NOT r;
END get;

PROCEDURE lookup(Name: ARRAY OF CHAR; VAR g: ADDRESS): BOOLEAN;
  VAR i: INTEGER; a: ADDRESS;
BEGIN
  IF lookupModule(TSK,Name,g) THEN RETURN TRUE END;
  IF lookupModule(SYS,Name,g) THEN RETURN TRUE END;
  FOR i:=0 TO cnt-1 DO
    IF lookupModule(dft[i],Name,g) THEN RETURN TRUE END;
  END;
  RETURN FALSE;
END lookup;

PROCEDURE CreEmulation(c: Object);
  VAR nm: ARRAY [0..79] OF CHAR; g: ADDRESS;
BEGIN
  IF Tag(c)#chip THEN RETURN END;
  IF cnt>HIGH(dft) THEN
    print('Too many chips in model, aborted...\n'); HALT(1);
  END;
  image0(nm,'%s',c^.ChipType^.Name);
  IF  Load(dft[cnt],nm,lookup,get,alloc,10)#0 THEN
    print('Loading %s error: %s\n',c^.ChipType^.Name,nm); HALT(1);
  END;
  S:=free; H:=ADR(wsp[HIGH(wsp)]); Op:=0;
  TRANSFER(main,set);
  free:=Call(dft[cnt]);
  Op:=1;
  TRANSFER(main,set);
  image0(nm,'%s',c^.ChipType^.Name);
  ASSERT(lookupModule(dft[cnt],nm,g));
  g:=ADDRESS(g-1)^;
  c^.cBefor:=INTEGER(BITSET(g)*{0..23}+{24});
  c^.cInit :=INTEGER(BITSET(g)*{0..23}+{25});
  c^.cAfter:=INTEGER(BITSET(g)*{0..23}+{24,25});
  INC(cnt);
END CreEmulation;

PROCEDURE InitEmulation(m: Object);
  VAR f: File; nm: FileName;
BEGIN
  IF cnt>0 THEN
    print('Не реализована отгрузка модулей ...\n'); HALT(1);
  END;
  nm:='..';
  checkHALT(OpenOnDir(CD(),f,nm),'../lib');
  nm:='lib';
  checkHALT(OpenOnDir(f,lib,nm),'../lib');
  checkHALT(Close(f),'../lib');
  free:=ADR(wsp);
  Iterate(m^.All,CreEmulation);
  print('Free memory %d bytes.\n',(ADR(wsp[HIGH(wsp)])-free+1)*4);
  checkHALT(Close(lib),'../lib');
END InitEmulation;

VAR setwsp: ARRAY [0..99] OF INTEGER;

BEGIN
  NewPrs(setter,ADR(setwsp),SIZE(setwsp),set);
  cnt:=0;
END cdsEmulLib.
