IMPLEMENTATION MODULE lkPass; (* 21-Feb-89.  *)

IMPORT StdIO;

FROM SYSTEM     IMPORT ADDRESS, ADR;
FROM objFile    IMPORT FILE,  SPOOL, EOF,
                       PROG,  FUNC,  SUBR,    SUBRA, BDATA, ENTRY, ENDP,
                       aRRAY, COMMON,EXTERNAL,CALL,  DCODE, PCODE, BCODE,
                       LABEL, ORJUMP,ANDJUMP, eNDLAB,JUMP,  JUMPC,
                       LDFORM,FORMAT,LPC,
                       gTag,    gxTag,  gName,
                       gProg,   gBData, gFunc, gSubr, gSubra,
                       gCommon, gArray, gCall, gLabel,gFormat,
                       gEndproc,
                       OpenObj, CloseObj;
FROM fcHeap     IMPORT Give, Free, ReleaseHeap, InitHeap, BackHeap,
                       MarkHeap;
FROM fcGen      IMPORT COD, cp, MinPS, STP, stpPos, proTab, procno,
                       global, BpCode, ProcCode, InitGen, FinishGen,
                       Alloc,  sgw, StartProc, FinishProc, genCbase,
                       genAbase, b, genSjump,  genSJlab, CallExt,
                       lgw, li, c;

FROM lkOpt      IMPORT InitLabs, setFormat, setldFormat, ldFormat,
                       setLabel, setJump,   optJump,     procJump;

FROM CodeFiles  IMPORT Attrs, CreateCode, CloseCode, WriteInfo,
                       WriteProcTable, WriteConsts, WriteCode, WriteExt;

FROM Clock      IMPORT SystemClock;

FROM Config     IMPORT SysInfo;

TYPE Mode  = (main, bdata, subr, subra, func, missing);
     Ppelem= POINTER TO Pelem;
     Pelem = RECORD
               modno  : INTEGER;
               procno : INTEGER;
               procmd : Mode;
               nopars : INTEGER;
               proctp : INTEGER;
               locals : INTEGER;
               maplen : INTEGER;
               templen: INTEGER;
             END;
     Pcelem= POINTER TO Celem;
     Celem = RECORD
               modno : INTEGER;
               comno : INTEGER;
               length: INTEGER;
             END;
     Plelem= POINTER TO Lelem;
     Lelem = RECORD
               pname : ADDRESS;
               pinfo : ADDRESS;
               next  : Plelem;
             END;

     Idname = ARRAY [0..35] OF CHAR;

     Pmodule= POINTER TO Module;
     Module= RECORD
               name:Idname;
               modno:INTEGER;
               next: Pmodule;
             END;

     Tpos = INTEGER;

VAR
    name: Idname;  Name: Idname;
    proc, parno, noalt, tp, moffs, length :INTEGER;
    label : INTEGER;
    fixed,cond : BOOLEAN;
    bmoffs, offset, maplen, locals, templen : INTEGER;
    pe : Pelem;  ce : Celem; ppos, cpos : Tpos;
    Modno :INTEGER;
    ProcNo: INTEGER; ProcPos: Tpos;
    mainmod, mainno : INTEGER;
    ProgFile, first, skip : BOOLEAN;
    modList, curMod: Pmodule;
    block:INTEGER; Block:ARRAY [0..35] OF CHAR;


CONST NoIds = 256;
      Pesize= 8;
      Cesize= 3;
      Lesize= 3;
      eol   = 0c;
      ORJP  = 0BEh;
      ANDJP = 0BFh;
      lpc   = 0EBh;

VAR Ip: POINTER TO Idname;
    HP: ARRAY [0..NoIds] OF ADDRESS;
    HC: ARRAY [0..NoIds] OF ADDRESS;
    LenId :INTEGER;
    hi: INTEGER;
    pel, pne : Plelem;
    locDft: ARRAY [0..31] OF INTEGER;
    dftpos: INTEGER;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE InitDft(modno:INTEGER);
BEGIN
 locDft[0]:=modno; dftpos:=1;
END InitDft;

PROCEDURE addDft(modno:INTEGER);
BEGIN
  locDft[dftpos]:=modno; INC(dftpos);
  IF dftpos>HIGH(locDft) THEN StdIO.print(' locDft!!! '); END;
END addDft;

PROCEDURE noImport(modno:INTEGER):INTEGER;
  VAR i:INTEGER;
BEGIN
  FOR i:=0 TO dftpos-1 DO
    IF locDft[i]=modno THEN RETURN i END;
  END;
  addDft(modno); RETURN dftpos-1;
END noImport;

PROCEDURE Hash(VAR Name:Idname):INTEGER;
  VAR Sum:INTEGER; C:CHAR;
BEGIN
  Sum:=0; LenId:=0;
  REPEAT  C:=Name[LenId];
    Sum := INTEGER(BITSET(Sum)/BITSET(C<<LenId));
    INC(LenId);
  UNTIL C = eol;
  RETURN Sum MOD NoIds;
END Hash;

PROCEDURE pr(txt:ARRAY OF CHAR; VAR nm:ARRAY OF CHAR);
BEGIN
  StdIO.print('  %s %s \n', txt, nm);
END pr;

PROCEDURE Inittable(VAR HA: ARRAY OF ADDRESS);
  VAR h: INTEGER;
BEGIN
  FOR h:=0 TO HIGH(HA) DO  HA[h]:=NIL; END;
END Inittable;

PROCEDURE InitLink;
BEGIN
   InitHeap;
   Inittable(HP);
   Inittable(HC);
   Give(modList,SIZE(Module));
   modList^.name:='ForIO'; modList^.modno:=1;
   Give(curMod,SIZE(Module));
   curMod^.name:='ForLib'; curMod^.modno:=2; curMod^.next:=NIL;
   modList^.next:=curMod; Modno:=2; mainmod:=0;
END InitLink;

PROCEDURE GiveModName(modno:INTEGER);
  VAR cmp:Pmodule;
BEGIN
  cmp:=modList;
  WHILE cmp#NIL  DO
    IF cmp^.modno=modno THEN
       name:=cmp^.name; RETURN ;
    END;
    cmp:=cmp^.next;
  END;
END GiveModName;

PROCEDURE error;
BEGIN
  StdIO.print(' Ошибка при записи кодофайла: %s \n',name);
END error;

PROCEDURE WriteCodeFile;
  VAR a: Attrs; i,j: INTEGER;
BEGIN
  WITH a DO
    CPU:=SysInfo.CPU;         Language:="Mod0";
    DefTime:=0;               ImpTime:=SystemClock();
    ProcCo:=procno;           ExtsCo:=dftpos;
    MinProcStack:=MinPS;      OptimalStack:=MinPS;
    GlobalsSize:=global;      ConstsSize:=stpPos DIV 4;
    CodeSize:=cp DIV 4;
  END;
  GiveModName(locDft[0]);
  StdIO.print(" Код %s.cod %dw\n",name,a.CodeSize);
  CreateCode(name,error); WriteInfo(a);
  WriteConsts(a,STP); WriteProcTable(a,proTab); WriteCode(a,COD);
  FOR i:=dftpos-1 TO 1 BY -1 DO
    GiveModName(locDft[i]);
    WriteExt(i,name);
  END;
  CloseCode;
END WriteCodeFile;

PROCEDURE NextEl(VAR HA:ARRAY OF ADDRESS):BOOLEAN;
BEGIN
  WHILE pne=NIL  DO
    INC(hi); IF hi>HIGH(HA) THEN RETURN FALSE END;
    pne:=HA[hi];
  END;
  pel:=pne; pne:=pel^.next;
  RETURN TRUE
END NextEl;

PROCEDURE fEtable(VAR HA:ARRAY OF ADDRESS):Tpos;
VAR h:INTEGER;
    p,p3: Plelem;
BEGIN
  h:=Hash(name);
  p:=HA[h]; p3:=ADR(HA[h])-2;
  LOOP
    IF p=NIL THEN RETURN -(INTEGER(p3)+2)
    ELSE
      Ip:=p^.pname;
      IF name=Ip^ THEN RETURN INTEGER(p);
      ELSE  p3:=p; p:=p3^.next;
      END;
    END;
  END;
END fEtable;

PROCEDURE pPtable(VAR pos:Tpos; VAR pe:Pelem);
  VAR p,p1,p2 : ADDRESS;
      pel, p3 :  Plelem;
      Len: INTEGER;
BEGIN
  IF pos<0 THEN p:=ADDRESS(-pos);
    Len:=(LenId+3) DIV 4;
    Give(p1,Len);
    MOVE(p1,ADR(name),Len);
    Give(p2,Pesize);
    Give(p3,Lesize);
    p3^.pname:=p1;  p3^.pinfo:=p2; p3^.next:=NIL;
    p^:=p3; pel:=p3; pos:=INTEGER(pel);
  ELSE pel:= ADDRESS(pos);
  END;
  p:=pel^.pinfo;
  MOVE(p,ADR(pe),8);
END pPtable;

PROCEDURE gPtable(pos:Tpos; VAR pe:Pelem);
  VAR p:ADDRESS; pel:Plelem;
BEGIN
  pel:= ADDRESS(pos); p:=pel^.pinfo;
  MOVE(ADR(pe),p,8);
END gPtable;

PROCEDURE pCtable(VAR pos:Tpos; VAR ce:Celem);
  VAR p,p1,p2 : ADDRESS;
      pel, p3 : Plelem;
      Len: INTEGER;
BEGIN
  IF pos<0 THEN p:=ADDRESS(-pos);
    Len:=(LenId+3) DIV 4;
    Give(p1,Len);
    MOVE(p1,ADR(name),Len);
    Give(p2,Cesize);
    Give(p3,Lesize);
    p3^.pname:=p1;  p3^.pinfo:=p2; p3^.next:=NIL;
    p^:=p3; pel:=p3; pos:=INTEGER(pel);
  ELSE pel:= ADDRESS(pos);
  END;
  p:=pel^.pinfo;
  MOVE(p,ADR(ce),SIZE(Celem));
END pCtable;

PROCEDURE gCtable(pos:Tpos; VAR ce:Celem);
  VAR p:ADDRESS; pel:Plelem;
BEGIN
  pel:= ADDRESS(pos); p:=pel^.pinfo;
  MOVE(ADR(ce),p,SIZE(Celem));
END gCtable;

PROCEDURE gEname(pl:Plelem);
BEGIN
  Ip:=pl^.pname;
  name:=Ip^;
END gEname;

PROCEDURE addModule;
  VAR cmp:Pmodule;
BEGIN
  INC(Modno); cmp:=modList;
-- StdIO.Show(' Modules List: ');
  WHILE cmp#NIL  DO
    IF cmp^.name=name THEN
      pr(' Два одинаковых модуля ',name);
    END;
--  WITH cmp^ DO
--    StdIO.print(' %-10s %-6d \n', name, modno);
--  END;
    cmp:=cmp^.next;
  END;
  Give(cmp,SIZE(Module));
  cmp^.name:=name; cmp^.modno:=Modno; cmp^.next:=NIL;
  curMod^.next:=cmp; curMod:=cmp;
--  WITH cmp^ DO
--      StdIO.print(' %-10s %-6d \n', name, modno);
--  END;
END addModule;

PROCEDURE prPe;
BEGIN
--  StdIO.print(' %-10s %-4d %-4d %-3d %-3d %-3d \n',
--              name,pe.modno,pe.procno,
--              ORD(pe.procmd),pe.nopars,
--              pe.proctp);
END prPe;

PROCEDURE prCe;
BEGIN
--  StdIO.print(' %-10s %-4d %-5d %-8d  \n',
--               name,ce.modno,ce.comno,ce.length);
  BpCode;
  Alloc(ce.length); sgw(ce.comno);
  -- debug --
  -- lgw(ce.comno); c(0FEh); li(ce.comno); c(0FEh);
  -- debug --
  INC(MinPS,ce.length);
END prCe;

PROCEDURE prPtable;
BEGIN
--  StdIO.Show(' Таблица процедур');
  hi:=-1; pne:=NIL;
  WHILE NextEl(HP) DO
    ppos:=INTEGER(pel); gPtable(ppos,pe);
    IF pe.procmd=missing THEN
    gEname(pel); pr(' Нет процедуры:', name);
    END;
  END;
END prPtable;

PROCEDURE CBlockMN(num:INTEGER);
VAR i: INTEGER; ch:CHAR;
BEGIN
  name:=filelist^.fname; i:=0; ch:=name[i];
  WHILE (ch#0c) AND (ch#'.') DO
    INC(i); ch:=name[i];
  END;
  name[i]:='C'; INC(i);
  name[i]:='b'; INC(i);
  name[i]:='L'; INC(i);
  name[i]:=CHAR(num+30h); INC(i);
  name[i]:=0c;
END CBlockMN;

PROCEDURE prCtable;
  VAR cbmno,comno:INTEGER; wrcodefile:BOOLEAN;
BEGIN
--  StdIO.Show(' Таблица common-блоков');
  hi:=-1; pne:=NIL; comno:=256; wrcodefile:=FALSE; cbmno:=0;
  WHILE NextEl(HC) DO
    IF comno=256 THEN
      INC(cbmno);
      IF wrcodefile THEN
        FinishGen; global:=comno; procno:=1;
        WriteCodeFile;
      END;
      CBlockMN(cbmno);  -- name:='filename + CbLi';
      addModule; comno:=2;
      InitGen(name);    InitDft(Modno);  wrcodefile:=TRUE;
      BpCode;
    END;
    cpos:=INTEGER(pel); gCtable(cpos,ce);
    ce.modno:=Modno; ce.comno:=comno; pCtable(cpos,ce);
    gEname(pel); prCe;
    INC(comno);
  END;
  IF wrcodefile THEN
    FinishGen; global:=comno; procno:=1;
    WriteCodeFile;
  END;
END prCtable;

PROCEDURE prH(VAR HA:ARRAY OF ADDRESS);
   VAR n,h:INTEGER;
BEGIN
  FOR h:=0 TO HIGH(HA) DO
    pel:=HA[h]; n:=0;
    WHILE pel#NIL DO
      INC(n); gEname(pel); pr('Entry: ',name);
      pel:=pel^.next;
    END;
    IF n>0 THEN
      StdIO.print(' [%-4d] len=%-3d \n',h,n);
    END;
  END;
END prH;

PROCEDURE tFile;
  VAR cmp:Pmodule;
BEGIN
  IF first THEN
    addModule;
  ELSE
    cmp:=modList;
    WHILE cmp#NIL  DO
      IF cmp^.name=name THEN
        Modno:=cmp^.modno; curMod:=cmp;
        InitGen(name); InitDft(Modno); addDft(1); addDft(2);
        RETURN
      END;
      cmp:=cmp^.next;
    END;
  END;
END tFile;

PROCEDURE tProg;
BEGIN
  IF first THEN
     ppos:=fEtable(HP);
     IF ppos<0 THEN
       pe.modno:=Modno;
       pe.procno:=proc;
       pe.procmd:=main;
       pe.nopars:=0;
       pe.proctp:=0;
       pe.locals:=0;
       pe.maplen:=0;
       pe.templen:=0;
       pPtable(ppos,pe);
       ProcNo:=proc; ProcPos:=ppos;
       IF mainmod#0 THEN
         pr(' Программа не единственная: ',name);
         skip:=TRUE;
       ELSE
         mainmod:=Modno; mainno:=proc;
       END;
     ELSE
       pr(' Программа не единственная: ',name);
       skip:=TRUE;
     END;
  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    IF (pe.modno#Modno) OR (pe.procno#proc) THEN
      skip:=TRUE; RETURN
    END;
    ProcNo:=proc; ProcPos:=ppos;
-- StdIO.print(' PROG: %d %d %d %d \n',proc,pe.maplen,pe.locals,pe.templen);
    StartProc(0,proc,pe.maplen,pe.locals,pe.templen);
    InitLabs;
  END;
END tProg;

PROCEDURE tBData;
BEGIN
  IF first THEN
     ppos:=fEtable(HP);
     IF ppos<0 THEN
       pe.modno:=Modno;
       pe.procno:=proc;
       pe.procmd:=bdata;
       pe.nopars:=0;
       pe.proctp:=0;
       pe.locals:=0;
       pe.maplen:=0;
       pe.templen:=0;
       pPtable(ppos,pe);
       ProcNo:=proc; ProcPos:=ppos;
     ELSE
       pr(' Программа не единственная: ',name);
       skip:=TRUE;
     END;

  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    IF (pe.modno#Modno) OR (pe.procno#proc) THEN
      skip:=TRUE; RETURN
    END;
    ProcNo:=proc; ProcPos:=ppos; Block[block]:=CHAR(proc); INC(block);
    StartProc(-7,proc,pe.maplen,pe.locals,pe.templen);
    InitLabs;
  END;
END tBData;

PROCEDURE tFunc;
  VAR pE: Pelem;
BEGIN
  IF first THEN
     ppos:=fEtable(HP);
     pe.modno:=Modno;
     pe.procno:=proc;
     pe.procmd:=func;
     pe.nopars:=parno;
     pe.proctp:=tp;
       pe.locals:=0;
       pe.maplen:=0;
       pe.templen:=0;
     IF ppos<0 THEN
       pPtable(ppos,pe);
       ProcNo:=proc; ProcPos:=ppos;
     ELSE
       gPtable(ppos,pE);
       IF pE.procmd=missing THEN
         pPtable(ppos,pe);
         ProcNo:=proc; ProcPos:=ppos;
       ELSE
         skip:=TRUE;
       END;
     END;
  ELSE  -- second pass
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    IF (pe.modno#Modno) OR (pe.procno#proc) THEN
      skip:=TRUE; RETURN
    END;
    ProcNo:=proc; ProcPos:=ppos;
-- StdIO.print(' Func: %d %d %d %d \n',proc,pe.maplen,pe.locals,pe.templen);
    StartProc(parno,proc,pe.maplen,pe.locals,pe.templen);
    InitLabs;
  END;
END tFunc;

PROCEDURE tSubr;
  VAR pE: Pelem;
BEGIN
  IF first THEN
     ppos:=fEtable(HP);
     pe.modno:=Modno;
     pe.procno:=proc;
     pe.procmd:=subr;
     pe.nopars:=parno;
     pe.proctp:=0;
       pe.locals:=0;
       pe.maplen:=0;
       pe.templen:=0;
     IF ppos<0 THEN
       pPtable(ppos,pe);
       ProcNo:=proc; ProcPos:=ppos;
     ELSE
       gPtable(ppos,pE);
       IF pE.procmd=missing THEN
         pPtable(ppos,pe);
         ProcNo:=proc; ProcPos:=ppos;
       ELSE
         skip:=TRUE;
       END;
     END;
  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    IF (pe.modno#Modno) OR (pe.procno#proc) THEN
      skip:=TRUE; RETURN
    END;
    ProcNo:=proc; ProcPos:=ppos;
-- StdIO.print(' Subr: %d %d %d %d \n',proc,pe.maplen,pe.locals,pe.templen);
    StartProc(parno,proc,pe.maplen,pe.locals,pe.templen);
    InitLabs;
  END;
END tSubr;

PROCEDURE tSubra;
  VAR pE: Pelem;
BEGIN
  IF first THEN
     ppos:=fEtable(HP);
     pe.modno:=Modno;
     pe.procno:=proc;
     pe.procmd:=subra;
     pe.nopars:=parno;
     pe.proctp:=noalt;
       pe.locals:=0;
       pe.maplen:=0;
       pe.templen:=0;
     IF ppos<0 THEN
       pPtable(ppos,pe);
       ProcNo:=proc; ProcPos:=ppos;
     ELSE
       gPtable(ppos,pE);
       IF pE.procmd=missing THEN
         pPtable(ppos,pe);
         ProcNo:=proc; ProcPos:=ppos;
       ELSE
         skip:=TRUE;
       END;
     END;
  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    IF (pe.modno#Modno) OR (pe.procno#proc) THEN
      skip:=TRUE; RETURN
    END;
    ProcNo:=proc; ProcPos:=ppos;
    StartProc(parno,proc,pe.maplen,pe.locals,pe.templen);
    InitLabs;
  END;
END tSubra;

PROCEDURE tCommon;
BEGIN
  IF skip THEN RETURN END;
  IF first THEN
    cpos:=fEtable(HC);
    IF cpos<0 THEN
      ce.length:=length;
      ce.modno:=0; ce.comno:=0;
      pCtable(cpos,ce);
    ELSE
      gCtable(cpos,ce);
      IF ce.length<length THEN
        ce.length:=length;
        pCtable(cpos,ce);
      END;
    END;
  ELSE  -- second pass
    cpos:=fEtable(HC);
    gCtable(cpos,ce);
-- StdIO.print(' Common: %-8s %-3d %-4d %-4d \n',
--             name, noImport(ce.modno), ce.comno,moffs);
    genCbase(ProcNo,noImport(ce.modno),ce.comno,moffs);
  END;
END tCommon;

PROCEDURE tExternal;
  VAR i:INTEGER;
BEGIN
  IF skip THEN RETURN END;
  IF first THEN
     ppos:=fEtable(HP);
     IF ppos<0 THEN
       pe.modno :=0;
       pe.procno:=0;
       pe.procmd:=missing;
       pe.nopars:=0;
       pe.proctp:=0;
       pPtable(ppos,pe);
     END;
  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    i:=noImport(pe.modno);
  END;
END tExternal;

PROCEDURE tLpc;
  VAR i:INTEGER;
BEGIN
  IF skip THEN RETURN END;
  IF first THEN
  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    i:=noImport(pe.modno);
    ProcCode;
    c(lpc); b(i); b(pe.procno);
  END;
END tLpc;

PROCEDURE tArray;
BEGIN
  IF skip  THEN RETURN END;
  IF first THEN RETURN
  ELSE
    genAbase(ProcNo,bmoffs,moffs,offset);
  END;
END tArray;

PROCEDURE tPCode;
  VAR i,len,j:INTEGER;
BEGIN
  IF first OR skip THEN
    len:=gxTag();
    FOR i:=1 TO len DO
      j:=gTag();
    END;
  ELSE
    ProcCode;
    len:=gxTag();
    FOR i:=1 TO len DO
      b(gTag());
    END;
  END;
END tPCode;

PROCEDURE tDCode;
  VAR i,len,j:INTEGER;
BEGIN
  IF first OR skip THEN
    len:=gxTag();
    FOR i:=1 TO len DO
      j:=gTag();
    END;
  ELSE
    BpCode;
    len:=gxTag();
    FOR i:=1 TO len DO
      b(gTag());
    END;
  END;
END tDCode;

CONST ForLib=2; Missing=47;

PROCEDURE tCall;
  VAR i:INTEGER;
BEGIN
  IF skip THEN RETURN END;
  IF first THEN
     ppos:=fEtable(HP);
     IF ppos<0 THEN
       pe.modno :=0;
       pe.procno:=0;
       pe.procmd:=missing;
       pe.nopars:=parno;
       pe.proctp:=noalt;
       pPtable(ppos,pe);
     END;
  ELSE
    ppos:=fEtable(HP);
    gPtable(ppos,pe);
    IF pe.procmd#missing THEN
      i:=noImport(pe.modno);
     -- test pe.nopars=parno; & pe.proctp=noalt
      CallExt(i,pe.procno);
    ELSE
     -- call ForLib.Missing
      CallExt(ForLib,Missing);
    END;
  END;
END tCall;

PROCEDURE tSjump(code:INTEGER);
BEGIN
  IF first OR skip THEN RETURN
  ELSE
   genSjump(code);
  END;
END tSjump;

PROCEDURE tEndlab;
BEGIN
  IF first OR skip THEN RETURN
  ELSE
   genSJlab;
  END;
END tEndlab;

PROCEDURE tLabel;
BEGIN
  IF first OR skip THEN RETURN
  ELSE
   setLabel(label);
  END;
END tLabel;

PROCEDURE tJump;
BEGIN
  IF first OR skip THEN RETURN
  ELSE
   setJump(label,fixed,cond);
  END;
END tJump;

PROCEDURE tLdform;
BEGIN
  IF first OR skip THEN RETURN
  ELSE
   setldFormat(label);
  END;
END tLdform;

PROCEDURE tFormat;
BEGIN
  IF first OR skip THEN RETURN
  ELSE
   setFormat(label,offset);
  END;
END tFormat;

PROCEDURE tEndp;
BEGIN
  IF skip THEN skip:=FALSE; RETURN END;
  IF first THEN
     ppos:=ProcPos;
--   ppos:=fEtable(HP);
     IF ppos<0 THEN StdIO.print(' linker error !!! ');
     ELSE
       gPtable(ppos,pe);
       pe.locals:=locals; pe.maplen:=maplen; pe.templen:=templen;
       pPtable(ppos,pe);
     END;
  ELSE
    ldFormat;
    optJump;
    procJump;
    FinishProc;
  END;
END tEndp;

PROCEDURE tSpool;
  VAR i,len,j:INTEGER;
BEGIN
  IF first THEN
    len:=gxTag();
    FOR i:=0 TO len*4-1 DO
      j:=gTag();
    END;
  ELSE
    len:=gxTag();
    FOR i:=0 TO len*4-1 DO
      STP[i]:=CHAR(gTag());
    END;
    stpPos:=len*4;
  END;
END tSpool;

PROCEDURE tEndfile;
  VAR i: INTEGER;
BEGIN
  IF first THEN RETURN
  ELSE  BpCode;
    FOR i:=1 TO block DO
-- StdIO.print(' Linker: bdata=%-4d  \n',INTEGER(Block[i-1]));
      CallExt(0,INTEGER(Block[i-1]));
    END;
    IF ProgFile THEN
      IF mainmod#0 THEN
        CallExt(noImport(mainmod),mainno);
      ELSE
        StdIO.print(' Отсутствует Главная программа  \n');
      END;
    END;
    FinishGen;
    procno:=proc+1; global:=proc+1;
--StdIO.print(' Codefile: proc=%-4d  globals=%-4d \n',procno-2,global-2);
    WriteCodeFile;
  END;
END tEndfile;

PROCEDURE scan;
  VAR byte:INTEGER;
BEGIN
  LOOP
    byte:=gTag();
    CASE byte  OF
      FILE:     gName(name);                          tFile;
     |PROG:     gProg(name,proc);                     tProg;
     |BDATA:    gBData(name,proc);                    tBData;
     |FUNC:     gFunc(name,proc,parno,tp);            tFunc;
     |SUBR:     gSubr(name,proc,parno);               tSubr;
     |SUBRA:    gSubra(name,proc,parno,noalt);        tSubra;
     |ENTRY:    gSubra(name,proc,parno,noalt);    --  tEntry;
     |COMMON:   gCommon(name,moffs,length);           tCommon;
     |EXTERNAL: gName(name);                          tExternal;
     |LPC     : gName(name);                          tLpc;
     |aRRAY :   gArray(bmoffs,moffs,offset);          tArray;
     |PCODE:                                          tPCode;
     |DCODE:                                          tDCode;
     |CALL:     gCall(name,parno,noalt);              tCall;
     |ORJUMP:                                         tSjump(ORJP);
     |ANDJUMP:                                        tSjump(ANDJP);
     |eNDLAB:                                         tEndlab;
     |LABEL:    gLabel(label);                        tLabel;
     |JUMP:     gLabel(label); fixed:=BOOLEAN(gTag());
                cond:=FALSE;                          tJump;
     |JUMPC:    gLabel(label); fixed:=BOOLEAN(gTag());
                cond:=TRUE;                           tJump;
     |LDFORM:   gLabel(label);                        tLdform;
     |FORMAT:   gFormat(label,offset);                tFormat;
     |ENDP :    gEndproc(name,maplen,locals,templen); tEndp;
     |SPOOL :                                         tSpool;
    ELSE
       IF byte#EOF THEN
         StdIO.print('Некорректный тег: %#h',byte);
       ELSE
         tEndfile;
       END; RETURN
    END; -- case
  END;   -- loop
END scan;

PROCEDURE Pass1;
BEGIN  first:=TRUE;
  curfile:=filelist;
  WHILE curfile#NIL DO
    OpenObj(curfile^.fname);
       skip:=FALSE;  scan;
    CloseObj;
    curfile:=curfile^.fnext;
  END;
END Pass1;

PROCEDURE AfterPass1;
BEGIN
--  prH(HP);
  prPtable;
  prCtable;
--  prH(HC);
END AfterPass1;

PROCEDURE BeforePass2;
BEGIN
  MarkHeap;
END BeforePass2;

PROCEDURE Pass2;
BEGIN   first:=FALSE;
  curfile:=filelist; ProgFile:=TRUE;
  WHILE curfile#NIL DO
    OpenObj(curfile^.fname);
    skip:=FALSE; block:=0;      scan;
    CloseObj; ProgFile:=FALSE;
    curfile:=curfile^.fnext;
  END;
END Pass2;

PROCEDURE AfterPass2;
BEGIN
END AfterPass2;

END lkPass.
