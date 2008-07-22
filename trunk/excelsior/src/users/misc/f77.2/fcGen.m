IMPLEMENTATION MODULE fcGen;

FROM SYSTEM    IMPORT   ADDRESS,WORD,ADR;
FROM fcScan    IMPORT   Fault, Error;
--FROM fcObj     IMPORT   Info, Unpack,  SetMode, GenObj,
--                        Idname, GetMode, Mode, IdStr, StrId;
FROM StdIO     IMPORT   Write, WriteString, CR, WriteLn,
                        print, ClearLine, Show;
IMPORT FileNames;
--FROM fcExpr    IMPORT   Dexpr, Emode;
FROM fcDefs    IMPORT   CodeSize, StpSize, maxEstkDepth;
FROM objFile   IMPORT   SPOOL, PCODE, DCODE, pCall,pLabel,pJump,pJumpC,
                        pORJump,pANDJump,pENDLab,pLDForm,
                        pTag, pxTag, pLPC;
--FROM fcTproc   IMPORT   AllocTemp, FreeTemp;


TYPE BPcode= ( BPcall,  BPlab,    BPjump,  BPjumpcond, BPldformat,
               BPorjmp, BPandjmp, BPendlab,BPstart,    BPend, BPlpc  );

CONST eol=0c;

VAR cpSave,cpSave1:INTEGER;
VAR fcp0, cp0, lcp0, depth, maxdepth: INTEGER;
    depthsave, maxdepthsave, depthsave1, maxdepthsave1:INTEGER;
    procode: BOOLEAN;
    lastc  : INTEGER;
    PDcode : INTEGER;
--    I:Info; (* for help *)

VAR BPoint: ARRAY [0..91] OF INTEGER;
    BPpos : INTEGER;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE ModuleName(VAR fnm:ARRAY OF CHAR);
  VAR fn:ARRAY [0..255] OF CHAR;
BEGIN
  fn:=fnm;
  FileNames.LastPart(fn,ModName);
  FileNames.DelExt(ModName);
END ModuleName;

PROCEDURE movecode(cp0,cp1:INTEGER);
  VAR len,i:INTEGER;
BEGIN  len:=cp1-cp0;
  IF len>0 THEN
    pTag(PDcode); pxTag(len);
    FOR i:=cp0 TO cp1-1 DO pTag(INTEGER(COD[i])) END;
  END;
END movecode;

PROCEDURE glab(VAR lab,cp0:INTEGER);
BEGIN
  lab:= INTEGER( BITSET(COD[cp0])+BITSET((COD[cp0+1])<<8) +
         BITSET((COD[cp0+2])<<16)+BITSET((COD[cp0+3])<<24));
  INC(cp0,4);
END glab;

PROCEDURE skiptoend(VAR i,cp0:INTEGER);
  VAR j,level: INTEGER;
BEGIN
  level:=0;
  REPEAT
    IF    BPoint[i+1]=ORD(BPstart) THEN INC(level)
    ELSIF BPoint[i+1]=ORD(BPend)   THEN DEC(level)
    END;
    INC(i,2);
  UNTIL level=0;
  DEC(i,2); cp0:=BPoint[i];
-- print(' skip:i,cp0= %d %d \n',i,cp0);
END skiptoend;

PROCEDURE findRg(VAR i,j,cpstart,cpend:INTEGER);
BEGIN
  REPEAT INC(i,2); UNTIL  BPoint[i]=cpstart;
  j:=i; skiptoend(j,cpend);
  INC(i,2);
END findRg;

PROCEDURE MoveCode(i,j,cp0,cpe:INTEGER);
  VAR cdi,cp1:INTEGER;
      i1,j1,cpstart,cpend:INTEGER;
      cd:BPcode;
      name:ARRAY [0..31] OF CHAR;  ch:CHAR;
      label,parno,noaltr:INTEGER;
      fixed:BOOLEAN;
BEGIN
  WHILE  i<j  DO
    cp1:=BPoint[i];
    movecode(cp0,cp1);
    --  moveBPoint
    cdi:=BPoint[i+1];
    cp0:=cp1;
    IF cdi>=0 THEN
      cd:=BPcode(cdi);
      CASE cd  OF
         BPcall:      parno:=0; ch:=COD[cp0];
                      WHILE ch#0c DO
                        name[parno]:=ch; INC(parno);
                        INC(cp0); ch:=COD[cp0];
                      END;
                      name[parno]:=ch; INC(cp0);
                      parno :=INTEGER(COD[cp0]); INC(cp0);
                      noaltr:=INTEGER(COD[cp0]); INC(cp0);
                      pCall(name,parno,noaltr);
        |BPlab:       glab(label,cp0);
                      pLabel(label);
        |BPjump:      glab(label,cp0);
                      fixed:=BOOLEAN(COD[cp0]); INC(cp0);
                      pJump(label,fixed);
        |BPjumpcond:  glab(label,cp0);
                      fixed:=BOOLEAN(COD[cp0]); INC(cp0);
                      pJumpC(label,fixed);
        |BPldformat:  glab(label,cp0);
                      pLDForm(label);
        |BPorjmp:     pORJump;
        |BPandjmp:    pANDJump;
        |BPendlab:    pENDLab;
        |BPstart:     skiptoend(i,cp0);
        |BPend:
        |BPlpc:       parno:=0; ch:=COD[cp0];
                      WHILE ch#0c DO
                        name[parno]:=ch; INC(parno);
                        INC(cp0); ch:=COD[cp0];
                      END;
                      name[parno]:=ch; INC(cp0);
                      pLPC(name);
        ELSE
      END;
    ELSE -- cdi<0 BPinsert
      i1:=i; cpstart:= -cdi; findRg(i1,j1,cpstart,cpend);
-- print('MoveCode: i,j,cps,cpe= %d %d %d %d \n',i1,j1,cpstart,cpend);
      MoveCode(i1,j1,cpstart,cpend);
    END;
    INC(i,2);
  END;
  movecode(cp0,cpe);
END MoveCode;

PROCEDURE moveCode;
 (* VAR i,j,k: INTEGER; *)
BEGIN
(*
   IF cp>100 THEN
print('moveCode: BPpos ,cp= %d %d  \n',BPpos,cp);
     j:=(cp DIV 10); k:=0;
     FOR i:=0 TO j DO
print(' %h %h %h %h %h %h %h %h %h %h \n',
COD[k],COD[k+1],COD[k+2],COD[k+3],COD[k+4],COD[k+5],COD[k+6],
COD[k+7],COD[k+8],COD[k+9]);
      INC(k,10);
     END;
     j:=(BPpos DIV 6); k:=0;
     FOR i:=0 TO j DO
print(' %d %d %d %d %d %d \n',
BPoint[k],   BPoint[k+1], BPoint[k+2],
BPoint[k+3], BPoint[k+4], BPoint[k+5]);
      INC(k,6);
     END;
   END;
*)
  PDcode:=PCODE; MoveCode(0,BPpos,0,cp);
  BPpos:=0; cp:=0;
END moveCode;

PROCEDURE DataCode;
 (* VAR i,j,k: INTEGER; *)
BEGIN
(*
   IF cp>100 THEN
print('moveCode: BPpos ,cp= %d %d  \n',BPpos,cp);
     j:=(cp DIV 10); k:=0;
     FOR i:=0 TO j DO
print(' %h %h %h %h %h %h %h %h %h %h \n',
COD[k],COD[k+1],COD[k+2],COD[k+3],COD[k+4],COD[k+5],COD[k+6],
COD[k+7],COD[k+8],COD[k+9]);
      INC(k,10);
     END;
     j:=(BPpos DIV 6); k:=0;
     FOR i:=0 TO j DO
print(' %d %d %d %d %d %d \n',
BPoint[k],   BPoint[k+1], BPoint[k+2],
BPoint[k+3], BPoint[k+4], BPoint[k+5]);
      INC(k,6);
     END;
   END;
*)
  PDcode:=DCODE; MoveCode(0,BPpos,0,cp);
  BPpos:=0; cp:=0;
END DataCode;

PROCEDURE pSpool;
  VAR len,i:INTEGER;
BEGIN
  pTag(SPOOL); len:= stpPos DIV 4; pxTag(len);
  FOR i:=0 TO len*4-1 DO pTag(INTEGER(STP[i])) END;
END pSpool;

PROCEDURE addBP(bpcode:INTEGER);
BEGIN
  BPoint[BPpos]:=cp;     INC(BPpos);
  BPoint[BPpos]:=bpcode; INC(BPpos);
END addBP;

PROCEDURE InsertMode(cp0:INTEGER);
VAR i,j: INTEGER;  grt:BOOLEAN;
BEGIN
  i:=0; grt:=TRUE;
  WHILE (i<BPpos) AND grt  DO
    IF BPoint[i]>=cp0 THEN
--    IF BPoint[i+1]<0 THEN INC(i,2); ELSE grt:=FALSE; END; --!?
      grt:=FALSE;
    ELSE INC(i,2);
    END;
  END;
  IF grt THEN BPoint[BPpos]:=cp0; BPoint[BPpos+1]:=-cp
  ELSE
    FOR j:=BPpos-1 TO i BY -1 DO
      BPoint[j+2]:=BPoint[j];
    END;
    BPoint[i]:=cp0; BPoint[i+1]:=-cp;
  END;
  INC(BPpos,2);
  addBP(ORD(BPstart)) ;
END InsertMode;

PROCEDURE InsertEnd;
BEGIN
  addBP(ORD(BPend));
END InsertEnd;

PROCEDURE ProcCode;
BEGIN
  procode:=TRUE;
END ProcCode;

PROCEDURE BpCode;
BEGIN
  procode:=FALSE;
END BpCode;

PROCEDURE b(n: INTEGER);
  VAR free,len, i, j : INTEGER;
BEGIN
  ASSERT((n>=0) & (n<=0FFh));
  IF procode THEN
    (*$T-*) COD[cp]:=CHAR(n); INC(cp); (*$T+*)
    IF cp>=fcp0 THEN
      free:=lcp0-cp0;
      IF free<8 THEN
print(' free= %d cp0=%d lcp0=%d \n',free,cp0,lcp0);
      Fault(0); END;
      free:=free DIV 2; len:=cp0-fcp0;
      fcp0:=fcp0+free;  j:=cp0;
      cp0:= cp0+free;   i:=cp0;
      WHILE len>0 DO
        DEC(i); DEC(j); DEC(len);
        COD[i]:=COD[j];
      END;
    END;
  ELSE
    (*$T-*) COD[cp0]:=CHAR(n); INC(cp0); (*$T+*)
    IF cp0>=lcp0 THEN
      free:=fcp0-cp;
      IF free<8 THEN
print(' free= %d cp=%d fcp0=%d \n',free,cp,fcp0);
      Fault(0); END;
      free:=free DIV 2; len:=cp0-fcp0;
      i:=fcp0-free;     j:=fcp0;
      cp0:= cp0-free;   fcp0:=i;
      WHILE len>0 DO
        COD[i]:=COD[j];
        INC(i); INC(j); DEC(len);
      END;
    END;
  END;
END b;

PROCEDURE b2(n: INTEGER);
BEGIN
  b(INTEGER(BITSET(n)*{0..7})); n:=(n>>8);
  b(INTEGER(BITSET(n)*{0..7}))
END b2;

PROCEDURE b4(n: INTEGER);
BEGIN
  b(INTEGER(BITSET(n)*{0..7})); n:=(n>>8);
  b(INTEGER(BITSET(n)*{0..7})); n:=(n>>8);
  b(INTEGER(BITSET(n)*{0..7})); n:=(n>>8);
  b(INTEGER(BITSET(n)*{0..7}));
END b4;

PROCEDURE c(n: INTEGER);
BEGIN
  lastc:=n; b(n);
END c;

PROCEDURE c1(cmd: INTEGER; n1: INTEGER);
BEGIN c(cmd); b(n1); END c1;

PROCEDURE c2(cmd: INTEGER; n1,n2: INTEGER);
BEGIN c(cmd); b(n1); b(n2);
END c2;

PROCEDURE cCall(VAR name:ARRAY OF CHAR; nopar,noaltr:INTEGER);
  VAR i:INTEGER; ch:CHAR;
BEGIN
  addBP(ORD(BPcall));
  i:=0;
  REPEAT
    ch:=name[i];
    b(INTEGER(ch));
    INC(i);
  UNTIL (ch=0c) OR (i>HIGH(name));
  IF ch#0c THEN b(0); END;
  b(nopar); b(noaltr);
END cCall;

PROCEDURE genLPC(VAR name:ARRAY OF CHAR);
  VAR i:INTEGER; ch:CHAR;
BEGIN
  addBP(ORD(BPlpc));
  i:=0;
  REPEAT
    ch:=name[i];
    b(INTEGER(ch));
    INC(i);
  UNTIL (ch=0c) OR (i>HIGH(name));
  IF ch#0c THEN b(0); END;
END genLPC;

PROCEDURE cLabel(label:INTEGER);
BEGIN
  addBP(ORD(BPlab));
  -- put label
  b4(label);
END cLabel;

PROCEDURE cJump(label:INTEGER; cond,fixed:BOOLEAN);
BEGIN
  IF cond THEN addBP(ORD(BPjumpcond));
  ELSE         addBP(ORD(BPjump));
  END;
  -- put label; put fixed;
  b4(label); b(INTEGER(fixed));
END cJump;

PROCEDURE cOrJump;
BEGIN
  addBP(ORD(BPorjmp));
END cOrJump;

PROCEDURE cAndJump;
BEGIN
  addBP(ORD(BPandjmp));
END cAndJump;

PROCEDURE cEndLab;
BEGIN
  addBP(ORD(BPendlab));
END cEndLab;

CONST JLFC=18h; JLF=19h; JSFC=1Ah; JSF=1Bh;
      JLBC=1Ch; JLB=1Dh; JSBC=1Eh; JSB=1Fh; INVLD=0FFh;
      RTN =0CAh;NOP=0CBh;SXW =51h; LXW=41h;

PROCEDURE epush;
BEGIN
--(*
  INC(depth);
  IF depth>maxdepth THEN maxdepth:=depth; END;
--  print("depth= %-4d maxdepth= %-4d \n",depth,maxdepth);
  IF depth>maxEstkDepth THEN depth:=0; Error(42); END;
--*)
END epush;

PROCEDURE epop;
BEGIN
--(*
  DEC(depth);
--  print("depth= %-4d \n",depth);
  IF depth<0 THEN Show("Empty stack;compiler");
--    ASSERT(FALSE);
  END;
--*)
END epop;

CONST LIn=00h; LIB=10h; LID=11h; LIW=12h; LIN=13h; NEG=0A7h; ror=8Fh;
      copt=0B5h; ADD=88h;

PROCEDURE li(n: INTEGER);
BEGIN
  IF n=INTEGER(NIL) THEN c(LIN)
  ELSIF n<0 THEN
    IF n=INTEGER({31}) THEN c(LIn+1); c(copt); c(ror);
    ELSIF -n>0FFFFh THEN c(LIW); b4(n)
    ELSE li(-n); c(NEG);
      RETURN (* RETURN to elimanate double -epush- *)
    END
  ELSIF n<=   0Fh THEN c (LIn+n)
  ELSIF n<=  0FFh THEN c1(LIB,n)
  ELSIF n<=0FFFFh THEN c2(LID,n MOD 100h,n DIV 100h)
  ELSE c(LIW); b4(n)
  END;
  epush;
END li;

CONST LLW=20h; LLWn=20h;  SLW=30h; SLWn=30h;
      LGW=21h; LGWn=40h;  SGW=31h; SGWn=50h;
      LSW=23h; LSWn=60h;
      LEW=22h; SEW =32h;  SSW=33h; SSWn=70h;

PROCEDURE slw(n: INTEGER);
BEGIN ASSERT((n>=4) & (n<=0FFh)); epop;
  IF n<=0Fh THEN c(SLWn+n) ELSE c1(SLW,n) END
END slw;

PROCEDURE sgw(n: INTEGER);
BEGIN ASSERT((n>=2) & (n<=0FFh)); epop;
  IF n<=0Fh THEN c(SGWn+n) ELSE c1(SGW,n) END
END sgw;

PROCEDURE ssw(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh)); epop; epop;
  IF n<=0Fh THEN c(SSWn+n) ELSE c1(SSW,n) END
END ssw;

PROCEDURE lgw(n: INTEGER);
BEGIN ASSERT((n>=2) & (n<=0FFh));
  IF n<=0Fh THEN c(LGWn+n) ELSE c1(LGW,n) END;
  epush;
END lgw;

PROCEDURE llw(n: INTEGER);
BEGIN ASSERT((n>=4) & (n<=0FFh));
  IF n<=0Fh THEN c(LLWn+n) ELSE c1(LLW,n) END;
  epush;
END llw;

PROCEDURE lsw(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  IF n<=0Fh THEN c(LSWn+n) ELSE c1(LSW,n) END
END lsw;

PROCEDURE sew(m,n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh)); ASSERT((m>=0) & (m<=0FFh));
  c2(SEW,m,n); epop;
END sew;

PROCEDURE lew(m,n: INTEGER);
BEGIN
  ASSERT((n>=0) & (n<=0FFh)); ASSERT((m>=0) & (m<=0FFh));
  c2(LEW,m,n); epush
END lew;

CONST LLAdr=14h; LGAdr=15h; LSAdr=16h; LEAdr=17h;

PROCEDURE lla(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  c1(LLAdr,n);
  epush;
END lla;

PROCEDURE lga(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  c1(LGAdr,n);
  epush;
END lga;

PROCEDURE lsa(n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh));
  IF n>0 THEN c1(LSAdr,n); END;
END lsa;

PROCEDURE lea(m,n: INTEGER);
BEGIN ASSERT((n>=0) & (n<=0FFh)); ASSERT((m>=0) & (m<=0FFh));
  c2(LEAdr,m,n);
  epush;
END lea;

PROCEDURE MarkC;
BEGIN cpSave:=cp;
      depthsave:=depth; maxdepthsave:=maxdepth;
END MarkC;

PROCEDURE BackC;
BEGIN cp:=cpSave;
      depth:=depthsave; maxdepth:=maxdepthsave;
END BackC;

PROCEDURE MarkC1;
BEGIN cpSave1:=cp;
      depthsave1:=depth; maxdepthsave1:=maxdepth;
END MarkC1;

PROCEDURE BackC1;
BEGIN cp:=cpSave1;
      depth:=depthsave1; maxdepth:=maxdepthsave1;
END BackC1;

PROCEDURE error; BEGIN Fault(12) END error;

PROCEDURE InPoolStr(VAR s:ARRAY OF CHAR): INTEGER;
  VAR i,j: INTEGER;
BEGIN
  j:=stpPos DIV 4; i:=0;
  WHILE (i<=HIGH(s)) AND (s[i]#eol) DO
   STP[stpPos]:=s[i]; INC(stpPos); INC(i);
   IF stpPos=HIGH(STP) THEN Fault(13) END;
  END;
  REPEAT STP[stpPos]:=eol; INC(stpPos);
  UNTIL (stpPos MOD 4)=0;
  RETURN j;
END InPoolStr;

PROCEDURE InPoolConst(wd:WORD):INTEGER;
VAR i: INTEGER; k: WORD;
    p: POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
  i:=stpPos DIV 4; k:=wd; p:=ADR(k);
  IF (stpPos+4) > HIGH(STP) THEN Fault(13); END;
  STP[stpPos]:=p^[0]; INC(stpPos);
  STP[stpPos]:=p^[1]; INC(stpPos);
  STP[stpPos]:=p^[2]; INC(stpPos);
  STP[stpPos]:=p^[3]; INC(stpPos);
  RETURN i;
END InPoolConst;

PROCEDURE InitPool(VAR fname:ARRAY OF CHAR);
  VAR i:INTEGER;
BEGIN
  stpPos:=0; ModuleName(fname);
  i:=InPoolStr(ModName);
END InitPool;

CONST ENTER=0C9h; STORE=0B3h; LODFV=0B2h;

PROCEDURE Getdepth():INTEGER;
BEGIN RETURN depth;
END Getdepth;

PROCEDURE Setdepth(d:INTEGER);
BEGIN depth:=d; maxdepth:=d;
END Setdepth;

PROCEDURE Store(VAR ds,mxs:INTEGER);
BEGIN
  ds:=depth; mxs:=maxdepth; c(STORE); depth:=0; maxdepth:=0;
END Store;

PROCEDURE Lodfv(ds,mxs,n:INTEGER);
BEGIN
  IF n=1 THEN
    c(LODFV); depth:=ds+1; maxdepth:=mxs+1;
  ELSE
    slw(n);
    c(LODFV); depth:=ds+1; maxdepth:=mxs+1;
    llw(n);
  END;
END Lodfv;

CONST CLp=0D0h; CLoc=0CFh; CX=0CCh;

PROCEDURE CL(p:INTEGER);
BEGIN ASSERT((p>=0) & (p<=0FFh));
  IF p<=0Fh THEN c(CLp+p) ELSE c1(CLoc,p) END;
END CL;

PROCEDURE CallExt(mod,pro:INTEGER);
BEGIN ASSERT(mod>=0);
  IF mod=0 THEN  CL(pro);
  ELSE
    c(CX); b(mod); b(pro);
  END;
END CallExt;

CONST ALLOC=0C8h; DROP=0B1h; Move=0C0h;

PROCEDURE Alloc(sz:INTEGER);
BEGIN li(sz); c(ALLOC);
END Alloc;

PROCEDURE Enter( s:INTEGER);
BEGIN
  IF s<= 255 THEN
   c(ENTER);  b(s);
 ELSE Alloc(s); c(DROP); epop;
 END;
END Enter;

PROCEDURE genCbase(procno,import,comno,moffs:INTEGER);
BEGIN  BpCode;
  lgw(procno); lew(import,comno); ssw(moffs);
END genCbase;

PROCEDURE genAbase(procno,bmoffs,moffs,offset:INTEGER);
BEGIN  BpCode;
  lgw(procno); c(copt); lsw(bmoffs);
  IF offset>0 THEN li(offset);  c(ADD); END;
  ssw(moffs);
END genAbase;

PROCEDURE genSjump(code:INTEGER);
BEGIN  ProcCode;
  c1(code,0); BPoint[BPpos]:=cp-1; INC(BPpos);
END genSjump;

PROCEDURE genSJlab;
  VAR cp0,offs:INTEGER;
BEGIN
  DEC(BPpos); cp0:=BPoint[BPpos];
  offs:=cp-cp0-1; COD[cp0]:=CHAR(offs);
END genSJlab;

CONST LSTA = 0C2h;

PROCEDURE LdFormat(soffs:INTEGER);
BEGIN  ProcCode;
  c(LSTA); b2(soffs); epush;
END LdFormat;

PROCEDURE lsta(soffs:INTEGER);
BEGIN
  c(LSTA); b2(soffs); epush;
END lsta;

PROCEDURE cLdFormat(label:INTEGER);
BEGIN
  addBP(ORD(BPldformat)); b4(label);
END cLdFormat;

PROCEDURE FinishProc;
BEGIN  ProcCode;
  IF lastc#RTN THEN c(RTN) END;
END FinishProc;

PROCEDURE StartProc(ParNo,procno,maplen,Locals,templen:INTEGER);
BEGIN
  ProcCode;
  depth:=0;
  proTab[procno]:=cp;
  IF ParNo>0 THEN c(STORE);
  ELSIF ParNo<0 THEN Enter(-ParNo);
  END;
  Alloc(maplen);
  lgw(procno); li(maplen); c(Move);
  IF templen>0 THEN Enter(templen); END;
       BpCode;
  Alloc(maplen); c(copt); sgw(procno);
  Alloc(Locals); ssw(0);
  INC(MinPS,maplen); INC(MinPS,Locals);
  ProcCode;
END StartProc;

PROCEDURE FinishGen;
 VAR  len:INTEGER;
BEGIN
  proTab[0]:=cp;
  len:=cp0-fcp0;
  WHILE len>0  DO
    COD[cp]:=COD[fcp0];
    INC(cp); INC(fcp0); DEC(len);
  END; ProcCode;
  li(MinPS); c(RTN);
  IF (cp MOD 4)#0 THEN
    REPEAT  c(NOP) UNTIL (cp MOD 4)=0;
  END;
END FinishGen;

PROCEDURE InitGen(VAR fname:ARRAY OF CHAR);
  VAR i:INTEGER;
BEGIN
  InitPool(fname);
  FOR i:=0 TO HIGH(proTab) DO proTab[i]:=0; END;
  cp:=0; fcp0:=CodeSize - (CodeSize DIV 10);
  cp0:=fcp0; lcp0:=CodeSize;
  MinPS:=4;  depth:=0; maxdepth:=0; BPpos:=0;
  procode:=FALSE;
     Enter(7); INC(MinPS,7);
  procode:=TRUE;
END InitGen;


BEGIN
END fcGen.
