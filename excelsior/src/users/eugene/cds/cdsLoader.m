IMPLEMENTATION MODULE cdsLoader; (* Leo 29-Jan-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;
FROM KRONOS     IMPORT  MOVE;
FROM Image      IMPORT  image0;
FROM Config     IMPORT  SysInfo;
FROM Scheduler  IMPORT  ProcessId, MyTask;
FROM Tasks      IMPORT  LP;
FROM Universe   IMPORT  IamUnic;
FROM osCodeInfo IMPORT  String32, StrPtr
                      , noExternals, noGlobals, Code, Entry
                      , putSize, getSize
                      , ProcTableAddress, StringsAddress, defTime, impTime
                      , FirstExternal, NextExternal, ModuleName, CodeName;

FROM Terminal   IMPORT  print;

CONST MAGIC=54464424; (* "$DFT" *)

TYPE DFTinfo=
    RECORD    magic: INTEGER;
      size: INTEGER;    (* максимальное число модулей в DFT *)
      body: ADDRESS;    (* указатель на DFT                 *)
      init: ADDRESS;    (* указатель на массив 0-процедур   *)
      bcou: ADDRESS;    (* размер body                      *)
      icou: ADDRESS;    (* размер init                      *)
     alloc: allocProc;  (* манаджер памяти                  *)
    END;

TYPE DFT=POINTER TO DFTinfo;

VAR SYSinfo: DFTinfo;
    TSKinfo: DFTinfo;

PROCEDURE isUnic(DFTentry: ADDRESS): BOOLEAN;
BEGIN RETURN 31 IN BITSET(DFTentry^) END isUnic;

(* -------------- DFT MANAGEMENT PROCEDURE ------------------------- *)


PROCEDURE newDFT(VAR dft: DFT; size: INTEGER; alloc: allocProc): BOOLEAN;
(* Если удалось создать DFT размером в -size- элементов   *)
(* возвращает TRUE, иначе FALSE. Причиной неудачи может   *)
(* служить отсутствие памяти 2*size+SIZE(DFTinfo) слов.   *)
  VAR a: ADDRESS;  space: ADDRESS;
BEGIN ASSERT(size>0);
  ASSERT((dft=NIL) OR (dft^.magic#MAGIC),4Fh);
  alloc(space,size*2+SIZE(DFTinfo));
  IF space=NIL THEN RETURN FALSE END;
  dft:=space;                   INC(space,SIZE(DFTinfo));
  dft^.magic:=MAGIC;            dft^.size:=size;
  dft^.body:=space;             dft^.bcou:=0;
  dft^.init:=space+size;        dft^.icou:=0;
  dft^.alloc:=alloc;
  FOR a:=space TO space+size*2 - 1 DO a^:=NIL END;
  RETURN TRUE;
END newDFT;

PROCEDURE remDFT(VAR dft: DFT);
BEGIN
  ASSERT(dft^.magic=MAGIC,4Fh); dft^.magic:=0;
  dft^.alloc(dft, - ( dft^.size*2+SIZE(DFTinfo) ) );
  dft:=NIL;
END remDFT;

PROCEDURE appDFTbody(VAR dft: DFT; G: ADDRESS): BOOLEAN;
  VAR a: ADDRESS;
BEGIN
  IF dft^.bcou>=dft^.size THEN RETURN FALSE END;
  a:=dft^.body+dft^.bcou; INC(dft^.bcou);
  a^:=G; DEC(G); G^:=a;
  RETURN TRUE
END appDFTbody;

PROCEDURE appDFTinit(VAR dft: DFT; X: ADDRESS): BOOLEAN;
  VAR a: ADDRESS;
BEGIN
  IF dft^.icou>=dft^.size THEN RETURN FALSE END;
  a:=dft^.init+dft^.icou; INC(dft^.icou); a^:=X;
  RETURN TRUE
END appDFTinit;

PROCEDURE lookupModule(VAL d: DFT; VAL m: ARRAY OF CHAR;
                       VAR G: ADDRESS
                      ): BOOLEAN;
  VAR a: ADDRESS;
BEGIN
  ASSERT(d^.magic=MAGIC,4Fh);
  FOR a:=d^.body TO d^.body+d^.bcou-1 DO
    IF a^#NIL THEN
      IF ModuleName(a^)^=m THEN G:=a^; RETURN TRUE END;
    END;
  END;
  RETURN FALSE
END lookupModule;

(* ----------------------------- LOADER ---------------------------- *)

PROCEDURE releaseG(alloc: allocProc; G: ADDRESS);
  VAR C,lDFT: ADDRESS; noE,noG: INTEGER;
BEGIN C:=Code(G);
  noE:=noExternals(C); noG:=noGlobals(C); lDFT:=G-noE; alloc(lDFT,-(noG+noE));
END releaseG;

(* cм. -loadCodes- & -copyGlo- на предмет 31 бита *)
PROCEDURE releaseC(alloc: allocProc; G: ADDRESS);
  VAR C,GW1: ADDRESS;
BEGIN C:=Code(G); GW1:=G+1;
  IF 31 IN BITSET(GW1^) THEN alloc(C,-getSize(C)) END;
END releaseC;

PROCEDURE releaseGC(alloc: allocProc; G: ADDRESS);
  VAR C,GW1,lDFT: ADDRESS;
         noE,noG: INTEGER;
BEGIN C:=Code(G); GW1:=G+1;
  noE:=noExternals(C); noG:=noGlobals(C); lDFT:=G-noE; alloc(lDFT,-(noG+noE));
  IF 31 IN BITSET(GW1^) THEN alloc(C,-getSize(C)) END;
END releaseGC;

PROCEDURE Load(VAR  dft: DFT;
               VAR Name: ARRAY OF CHAR;
                 lookup: lookupProc;
                    get: getProc;
                  alloc: allocProc;
                dftsize: INTEGER
              ): INTEGER;

  CONST Fover ="%s нет места в DFT";
        Fnomem="%s нет памяти для глобалов";
        Ftime ="%s конфликт версий";

  PROCEDURE error(VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
  BEGIN image0(Name,fmt,args) END error;

  VAR getResult: ARRAY [0..79] OF CHAR;

  PROCEDURE loaded(VAL name: ARRAY OF CHAR; VAR C: ADDRESS): BOOLEAN;
    VAR i: INTEGER;    a: ADDRESS;
  BEGIN i:=dft^.icou-1; a:=dft^.init+dft^.icou-1;
    WHILE (i>=0) DO
      IF CodeName(a^)^=name THEN C:=a^; RETURN TRUE END;
      DEC(i); DEC(a);
    END;
    RETURN FALSE;
  END loaded;

  (* глобалы выделенные для зачитанных модулей будут промаркированы *)
  (* 31 битом в адресе строкового пула (см. -load- ниже).           *)
  PROCEDURE loadCodes(VAL name: ARRAY OF CHAR; time: INTEGER): INTEGER;
    VAR G,C: ADDRESS;
       pExt: StrPtr;
      n,res: INTEGER;
       size: INTEGER;
  BEGIN
    IF lookup(name,G) OR loaded(name,C) THEN RETURN ok END;
    IF NOT get(name,C,size,alloc,getResult) THEN
      error("%s %s",name,getResult); RETURN geterror
    END;
    putSize(C,size);
    IF (time>=0) & (time#defTime(C)) THEN
      error(Ftime,name); RETURN wrongtime
    END;
    IF NOT appDFTinit(dft,C) THEN alloc(C,-size);
      error(Fover,name); RETURN dftoverflow
    END;
    n:=noExternals(C);  FirstExternal(C,pExt,time);
    WHILE (n>1) DO
      res:=loadCodes(pExt^,time);
      IF res#ok THEN RETURN res END;
      NextExternal(C,pExt,time);
      DEC(n);
    END;
    RETURN ok
  END loadCodes;

  PROCEDURE remCodes;
    VAR a,C: ADDRESS;
  BEGIN
     FOR a:=dft^.init TO dft^.init+dft^.icou-1 DO C:=a^;
       alloc(C,-getSize(C));
     END;
  END remCodes;

  PROCEDURE my(G: ADDRESS): BOOLEAN; (* -G- уже загружен в мою -dft- ? *)
    VAR my: ADDRESS;
  BEGIN my:=Entry(G);
    RETURN (my>=ADDRESS(dft^.body)) & (my<ADDRESS(dft^.body)+dft^.bcou);
  END my;

  PROCEDURE load(VAL name: ARRAY OF CHAR; VAR G: ADDRESS; deftime: INTEGER
                ): INTEGER;

    PROCEDURE copyGlo(VAR G: ADDRESS): INTEGER;
      VAR lDFT, newG, Cod, GW1: ADDRESS;
             noExt, noGlo, res: INTEGER;

      PROCEDURE copyExts(): INTEGER;
        VAR res: INTEGER;
         l,extG: ADDRESS;
          entry: ADDRESS;
      BEGIN
        FOR l:=lDFT TO lDFT+noExt-2 DO entry:=l^; extG:=entry^;
          IF NOT isUnic(entry) & NOT my(extG) THEN
            IF NOT lookupModule(dft,ModuleName(extG)^,extG) THEN
              res:=copyGlo(extG);
              IF res#ok THEN RETURN res END;
            END;
            l^:=Entry(extG);
          END;
        END;
        RETURN ok;
      END copyExts;

    (* глобалы выделенные для скопированных модулей демаркированы *)
    (* 31 битом из адреса строкового пула.                        *)
    BEGIN
      IF my(G) OR isUnic(Entry(G)) THEN RETURN ok END;
      Cod:=Code(G);
      noExt:=noExternals(Cod); noGlo:=noGlobals(Cod);
      alloc(lDFT,noExt+noGlo);
      IF lDFT=NIL THEN error(Fnomem,ModuleName(G)^); RETURN nomemory END;
      newG:=lDFT+noExt;        MOVE(lDFT,G-noExt,noExt+noGlo);
      GW1:=newG+1;             GW1^:=BITSET(GW1^)-{31};
      IF NOT appDFTbody(dft,newG) THEN alloc(lDFT, - (noExt+noGlo));
        error(Fover,ModuleName(G)^); RETURN dftoverflow
      END;
      res:=copyExts();
      IF res#ok THEN RETURN res END;
      G:=newG;
      RETURN ok
    END copyGlo;

    VAR
        lDFT: ADDRESS;   C,GW1: ADDRESS;
        extC: ADDRESS;   noExt: INTEGER;

    PROCEDURE loadExts(): INTEGER;
      VAR i,res: INTEGER;
        entry,E: ADDRESS;
           pExt: StrPtr;
    BEGIN
      FirstExternal(C,pExt,deftime);
      i:=noExt;  entry:=lDFT;
      WHILE (i>1) DO
        IF NOT lookupModule(dft,pExt^,E) THEN
          res:=load(pExt^,E,deftime);
          IF res#ok THEN RETURN res END;
          IF impTime(C)<defTime(Code(E)) THEN
            error(Ftime,ModuleName(E)); RETURN wrongtime
          END;
        END;
        entry^:=Entry(E);   NextExternal(C,pExt,deftime);
        INC(entry); DEC(i);
      END;
      ASSERT(entry=G-1);
      RETURN ok
    END loadExts;

  (* глобалы выделенные для зачитанных модулей промаркированы *)
  (* 31 битом в адресе строкового пула.                       *)
  BEGIN
    IF lookup(name,G)           THEN RETURN copyGlo(G) END;
    IF lookupModule(dft,name,G) THEN RETURN ok         END;
    IF NOT loaded(name,C)       THEN ASSERT(FALSE)     END;
    IF (deftime>=0) & (deftime#defTime(C)) THEN
      error(Ftime,name); RETURN wrongtime
    END;
    noExt:=noExternals(C);
    alloc(lDFT,noExt+noGlobals(C));
    IF lDFT=NIL THEN error(Fnomem,name); RETURN nomemory END;
    G   :=lDFT+noExt;           GW1 :=G+1;
    G^  :=ProcTableAddress(C);  GW1^:=BITSET(StringsAddress(C))+{31};
    IF NOT appDFTbody(dft,G) THEN alloc(lDFT,- (noExt+noGlobals(C)));
      error(Fover,name); RETURN dftoverflow
    END;
    RETURN loadExts();
  END load;

  PROCEDURE passed(G: ADDRESS): BOOLEAN;    (* пометка для обнаружения  *)
  BEGIN RETURN 31 IN BITSET(G^) END passed; (* циклов в инициализации   *)
  PROCEDURE   pass(G: ADDRESS); BEGIN INCL(BITSET(G^),31) END   pass;
  PROCEDURE unpass(G: ADDRESS); BEGIN EXCL(BITSET(G^),31) END unpass;

  PROCEDURE marked(E: ADDRESS): BOOLEAN;    (* пометка о том, что модуль *)
  BEGIN RETURN 31 IN BITSET(E^) END marked; (* уже проинициализирован    *)
  PROCEDURE markinit(E: ADDRESS); BEGIN INCL(BITSET(E^),31) END markinit;
  PROCEDURE unmark  (E: ADDRESS); BEGIN EXCL(BITSET(E^),31) END unmark;

  PROCEDURE makeInit(VAL name: ARRAY OF CHAR): INTEGER;

    VAR res: INTEGER;

    PROCEDURE initmodule(E: ADDRESS): INTEGER;
      VAR n: INTEGER; l,e,G: ADDRESS;
    BEGIN G:=E^;
      IF marked(E) OR passed(G) THEN RETURN ok END;
      pass(G);
      n:=noExternals(Code(G));  l:=G-2;
      WHILE (n>1) DO e:=l^; DEC(n); DEC(l);
        IF my(e^) THEN  res:=initmodule(e);
          IF res#ok THEN RETURN res END;
        END;
      END;
      markinit(E);
      IF NOT appDFTinit(dft,G) THEN
        error(Fover,ModuleName(G)); RETURN dftoverflow
      END;
      unpass(G); RETURN ok
    END initmodule;

    VAR G,E: ADDRESS;

  BEGIN
    IF ADR(dft)=ADR(SYS) THEN RETURN ok END;
    IF ADR(dft)=ADR(TSK) THEN RETURN ok END;
    IF NOT lookupModule(dft,name,G) THEN ASSERT(FALSE) END;
    res:=initmodule(Entry(G));
    FOR E:=dft^.body TO dft^.body+dft^.bcou-1 DO unmark(E) END;
    RETURN res
  END makeInit;

  PROCEDURE remGlobs;
    VAR a: ADDRESS;
  BEGIN
    FOR a:=dft^.body TO dft^.body+dft^.bcou-1 DO releaseG(alloc,a^) END;
  END remGlobs;

  PROCEDURE remAll;
    VAR a: ADDRESS;
  BEGIN
    FOR a:=dft^.body TO dft^.body+dft^.bcou-1 DO releaseGC(alloc,a^) END;
  END remAll;

  VAR r: INTEGER;
    G,C: ADDRESS;
   name: String32;

BEGIN
  image0(name,"%s",Name);
  IF NOT newDFT(dft,dftsize,alloc) THEN
    error("%s нет %d слов памяти для DFT в %d элементов"
          ,name,dftsize,dftsize*2+SIZE(DFTinfo)); RETURN nomemory
  END;
  r:=loadCodes(name,-1);
  IF r#ok THEN remCodes; remDFT(dft); RETURN r END;
  r:=load(name,G,-1);
  IF r#ok THEN remGlobs;  remCodes; remDFT(dft); RETURN r END;

(*НЕ ПОНЯЛ САМ СЕБЯ!:
  IF isUnic(Entry(G)) THEN RETURN ok END; (* Unic модуль не может инициа- *)
                                          (* лизироваться дважды!         *)
*)

  dft^.icou:=0;
  r:=makeInit(name);
  IF r#ok THEN remAll; remDFT(dft); END;
  RETURN r
END Load;

PROCEDURE UnLoad(dft: DFT);
  VAR a: ADDRESS; alloc: allocProc;
BEGIN
  ASSERT(dft^.magic=MAGIC,4Fh);
  alloc:=dft^.alloc;
  FOR a:=dft^.body TO dft^.body+dft^.bcou-1 DO releaseGC(alloc,a^) END;
  FOR a:=dft^.body TO dft^.body+dft^.size-1 DO a^:=NIL END;
  FOR a:=dft^.init TO dft^.init+dft^.size-1 DO a^:=NIL END;
  dft^.icou:=0; dft^.bcou:=0;
  remDFT(dft);
END UnLoad;

PROCEDURE Call(dft: DFT): ADDRESS;
  TYPE pp=PROCEDURE (): INTEGER;
  VAR a,h,f: ADDRESS;
      p  : pp;
      n,s: INTEGER;
  PROCEDURE ALLOC(n: INTEGER): ADDRESS; CODE 0C8h END ALLOC;
  PROCEDURE lla0(): ADDRESS; CODE 14h 0h END lla0;
BEGIN
  ASSERT(dft^.magic=MAGIC,4Fh);
  a:=dft^.init; n:=dft^.icou;
  h:=ALLOC(0); f:=h;
  WHILE n>0 DO
    p:=pp(a); s:=p();
    h:=ALLOC(s)+s; DEC(n); INC(a);
  END;
  IF f=h THEN RETURN lla0() ELSE RETURN h END;
END Call;

PROCEDURE Order(dft: DFT; ip: iterProc);
  VAR a: ADDRESS; p: PROC; n: INTEGER;
BEGIN
  ASSERT(dft^.magic=MAGIC,4Fh);
  a:=dft^.init; n:=dft^.icou;
  WHILE n>0 DO
    ip(PROC(a)); DEC(n); INC(a);
  END;
END Order;

VAR SYSinit: ARRAY [0..127] OF INTEGER;
    TSKinit: ARRAY [0..127] OF INTEGER;
(* 3Fh (64) среднефонарная оценка размера догружаемых к   *)
(* системе утилит.                                        *)

PROCEDURE initSYS;
  VAR a: ADDRESS;
BEGIN
  SYS:=ADR(SYSinfo);
  SYS^.magic:=MAGIC;
  SYS^.size:=SysInfo.dftsz;
  SYS^.init:=ADR(SYSinit);
  SYS^.icou:=0;
  SYS^.body:=SysInfo.dft;
  SYS^.bcou:=0;
  a:=SYS^.body;
  WHILE (SYS^.bcou<SYS^.size) & (a^#NIL) DO INC(a); INC(SYS^.bcou) END;
END initSYS;

PROCEDURE initTSK;
  VAR a: ADDRESS; im: ProcessId; lp: POINTER TO LP;
      p: POINTER TO ARRAY [0..79] OF CHAR;
BEGIN
  im:=MyTask(); lp:=im^.InfoLink;
  TSK:=ADR(TSKinfo);
  TSK^.magic:=MAGIC;
  TSK^.size:=(lp^.DFTLim-lp^.tDFT+1);
  TSK^.init:=ADR(TSKinit);
  TSK^.icou:=0;
  TSK^.body:=lp^.tDFT;
  TSK^.bcou:=0;
  a:=TSK^.body;
  WHILE (TSK^.bcou<TSK^.size) & (a^#NIL) DO
    INCL(BITSET(a^),31);
    INC(a); INC(TSK^.bcou)
  END;
END initTSK;

BEGIN
  initSYS;
  initTSK;
END cdsLoader.
