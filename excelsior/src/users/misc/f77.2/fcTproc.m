IMPLEMENTATION MODULE fcTproc; (* 07-Feb-89.  *)

FROM SYSTEM  IMPORT ADDRESS, ADR;
FROM fcScan  IMPORT Error, ErrorId, ShowLine, ShowErrors, NoStmErr;
FROM fcObj   IMPORT GetMode, GetType, Types, Info, Mode, Class,
                   Id, Ident,idno, IdStr, Unpack, Pack;
FROM fcDcl   IMPORT PEquiStr, PEquiObj, PEquiv, PlBlock, PBlock,
                   PDataStr, PDataNames, PDataConst, DataStr,
                   DataNames, DataConst, DataList, self, unit, ProcNo,
                   Mapoffs, Maplength, Loffs, ParNo, Toffs, size,
                   ALHD, LHD;
FROM Misc    IMPORT Max, Min;
FROM objFile IMPORT pCommon, pExternal, pArray;
FROM fcGen   IMPORT llw, li, lsw, ssw, c, ProcCode, CallExt, moveCode,
               lla, c1, slw, lgw, lsa ,lsta, DataCode, epop, Setdepth;
FROM fcInd   IMPORT LoadVal;
FROM fcExpr  IMPORT Dexpr, Emode;
FROM fcStm   IMPORT AssTypeConv;

FROM StdIO      IMPORT print;


VAR PS,Q:INTEGER;
    ps,q:Info;
    offs,l,l1,ad:INTEGER;

VAR TempMap:ARRAY [0..255] OF CHAR;
    Tbegin, Tend : INTEGER;
    CharDescr:ARRAY [0..255] OF CHAR;
    Dpos:INTEGER;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE FreeTemp(offs,len:INTEGER);
  VAR i:INTEGER;
BEGIN
  FOR i:=offs TO offs+len-1 DO TempMap[i]:=0c END;
END FreeTemp;

PROCEDURE AllocTemp(VAR offs:INTEGER; len:INTEGER);
  VAR i,l,k:INTEGER; freearea:BOOLEAN;
BEGIN
  i:=Tbegin; l:=len; freearea:=FALSE;
  REPEAT
    k:=INTEGER(TempMap[i]);
    IF k=0 THEN
      IF NOT freearea THEN
         freearea:=TRUE; offs:=i; l:=len;
      END;
      DEC(l);
    ELSE freearea:=FALSE;
    END; INC(i);
  UNTIL (l=0) OR (i>HIGH(TempMap));
  IF l=0 THEN
    k:=offs+len-1;
    FOR i:=offs TO k DO TempMap[i]:=1c END;
    IF k>=Toffs THEN Toffs:=k+1 END;
  ELSE Error(68); offs:=255;
  END;
END AllocTemp;

PROCEDURE AllocDesc(VAR D:INTEGER);
VAR j,i: INTEGER;
BEGIN
   FOR i:=0 TO Dpos-1 DO
     j:=INTEGER(CharDescr[i]);
     IF j=0 THEN
       AllocTemp(D,3);
       CharDescr[i]:=CHAR(D);
       RETURN
     END;
   END;
   AllocTemp(D,3);
   CharDescr[Dpos]:=CHAR(D); INC(Dpos);
END AllocDesc;

PROCEDURE FreeDesc(D:INTEGER);
VAR j,i: INTEGER;
BEGIN
   FOR i:=0 TO Dpos-1 DO
     j:=INTEGER(CharDescr[i]);
     IF j=D THEN
       FreeTemp(j,3); CharDescr[i]:=0c; RETURN
     END;
   END;
END FreeDesc;

PROCEDURE FreeDescs;
VAR j,i: INTEGER;
BEGIN
   FOR i:=0 TO Dpos-1 DO
     j:=INTEGER(CharDescr[i]);
     IF j>0 THEN FreeTemp(j,3); END;
   END;
   Dpos:=0;
END FreeDescs;

CONST SXW=51h; NEG=0A7h; ADD=88h; SUB=89h; MUL=8Ah; DiV=8Bh; copt=0B5h;
      JFSC=1Ah; JBS=1Fh; Move=0C0h; Inc1=0E4h; Dec1=0E5h; Inc=0E6h;
      ForLib=2; FormDesc=4; ChAssign=10;

VAR block:BOOLEAN;

PROCEDURE ldata(offs,offs1,val:INTEGER);
BEGIN
  IF block THEN
    llw(offs);
    IF offs1<=255 THEN li(val); ssw(offs1);
    ELSE               li(offs1); li(val); c(SXW); epop;epop;epop;
    END;
  ELSE
    lgw(ProcNo);
    IF offs<=255 THEN      li(val); ssw(offs);
    ELSIF offs1<=255 THEN  lsw(0); li(val); ssw(offs1);
    ELSE                   li(offs); li(val); c(SXW); epop;epop;epop;
    END;
  END;
END ldata;

PROCEDURE laddress(offs,offs1:INTEGER);
BEGIN
  IF block THEN
    llw(offs);
    IF offs1<=255 THEN     lsa(offs1);
    ELSE                   li(offs1); c(ADD); epop;
    END;
  ELSE
    lgw(ProcNo);
    IF offs<=255 THEN      lsa(offs);
    ELSIF offs1<=255 THEN  lsw(0); lsa(offs1);
    ELSE                   li(offs); c(ADD); epop;
    END;
  END;
END laddress;

PROCEDURE ProcData;
  VAR cs, offs, offs1  :INTEGER;
      n, nel, nrep, nb :INTEGER;
      Dlist:PDataStr; pn:PDataNames;  pc:PDataConst;
                      dnm:DataNames;  cnst:DataConst;
      I:Info; obl,obr:Dexpr;
BEGIN
  Dlist:=DataList;
  WHILE Dlist#NIL  DO
    pn:=Dlist^.names; pc:=Dlist^.consts; nrep:=0;
    WHILE (pn#NIL) DO
      dnm:=pn^; nel:=dnm.nel;
      I.name:=dnm.idno; Unpack(I);
      obl.name:=I.name; obl.emd:=var; obl.tp:=GetType(I.name);
      offs1:=I.offset+dnm.offset;
      IF block THEN offs:=I.darea;
        IF (GetMode(I.name)=Array) AND (obl.tp#Char) THEN
          offs1:=dnm.offset;
        END;
        IF I.cl#Global THEN idno:=I.name; ErrorId(15); END;
      ELSE
        offs:=Maplength+offs1;
        IF I.cl#Local THEN idno:=I.name; ErrorId(15); END;
      END;
      IF obl.tp=Char THEN cs:=3;
--      ELSIF (obl.tp=Double) THEN cs:=2;
      ELSIF (obl.tp=Complex) THEN cs:=2;
      ELSE cs:=1;
      END;
      IF nel=1 THEN
        IF nrep=0 THEN
          IF pc=NIL THEN Error(54); RETURN ;
          ELSE cnst:=pc^; pc:=cnst.next;
          END; nrep:=cnst.nrep;
        END;
        obr.emd:=cexpr; obr.tp:=cnst.tp;
        obr.name:=cnst.val; obr.wd:=cnst.val1;
        IF AssTypeConv(obl,obr) THEN
        END;
        CASE cs OF
          1: IF obr.tp=Holl THEN
               laddress(offs,offs1);
               lsta(obr.wd); li(1); c(Move); epop; epop; epop;
             ELSE  ldata(offs,offs1,obr.name);
             END;
         |2: laddress(offs,offs1);
             IF obr.tp=Holl THEN
               lsta(obr.wd);
               IF obr.name<=4 THEN
                 lsw(0); slw(4);
                 li(20202020h); slw(5); lla(4);
               END;
             ELSE
               li(obr.name); slw(4);
               li(obr.wd);  slw(5);  lla(4);
             END;
             li(2); c(Move); epop;epop;epop;
         |3: IF block THEN
               llw(offs);
             ELSE
               lgw(ProcNo); lsw(0);
             END;                 slw(4);
             li(offs1);           slw(5);
             li(I.lenel);         slw(6);  -- ?? id(i:j) ??
             lsta(obr.wd);        slw(7);
             li(0);               slw(8);
             li(obr.name);        slw(9);
             lla(4);  lla(7); CallExt(ForLib,ChAssign); epop;epop;
        ELSE
        END;  -- case
        DEC(nrep);
      ELSE  -- nel>1
        IF cs=3 THEN
          IF block THEN
            llw(offs);
          ELSE
            lgw(ProcNo); lsw(0);
          END;                 slw(4);
          li(offs1);           slw(5);
          li(I.lenel);         slw(6);
          li(0);               slw(8);
        ELSE laddress(offs,offs1); slw(4);
        END;
        WHILE nel>0 DO
          IF nrep=0 THEN
            IF pc=NIL THEN Error(54);
            ELSE cnst:=pc^; pc:=cnst.next;
            END; nrep:=cnst.nrep;
          END;
          obr.emd:=cexpr; obr.tp:=cnst.tp;
          obr.name:=cnst.val; obr.wd:=cnst.val1;
          IF AssTypeConv(obl,obr) THEN
          END;
          CASE cs OF
            1: IF obr.tp=Holl THEN
                 lsta(obr.wd); lsw(0);
               ELSE  li(obr.name);
               END;  slw(6);
           |2: IF obr.tp=Holl THEN
                 IF obr.name<=4 THEN
                   lsta(obr.wd);
                   lsw(0); slw(6);
                   li(20202020h); slw(7);
                 ELSE
                   lla(6); lsta(obr.wd); li(2); c(Move); epop; epop; epop;
                 END;
               ELSE
                 li(obr.name); slw(6);
                 li(obr.wd);  slw(7);
               END;
           |3: lsta(obr.wd);        slw(7);
               -- li(0);               slw(8); --
               li(obr.name);        slw(9);
          ELSE
          END;  -- case
          IF (nel>1) AND (nrep>1) THEN
            n:=Min(nel,nrep);
            CASE cs OF
              1: li(n); slw(5);
                 llw(5); c1(JFSC,11); epop;
                   llw(4); llw(6); ssw(0);
                   lla(4); c(Inc1); lla(5); c(Dec1); epop; epop;
                 c1(JBS,14);
             |2: li(n); slw(5);
                 llw(5); c1(JFSC,14);
                   llw(4);  lla(6); li(2); c(Move); epop;epop; epop;
                   lla(4);  li(2); c(Inc); lla(5); c(Dec1); epop;epop;
                 c1(JBS,17);
             |3: li(n); slw(10);
                 IF    I.lenel<=0Fh    THEN nb:=16;
                 ELSIF I.lenel<=0FFh   THEN nb:=17;
                 ELSIF I.lenel<=0FFFFh THEN nb:=18;
                 END;
                 llw(10); c1(JFSC,nb); epop;
                   lla(4);  lla(7); CallExt(ForLib,ChAssign); epop;epop;
                   lla(5);  li(I.lenel); c(Inc); lla(10); c(Dec1); epop; epop;
                 c1(JBS,nb+3);
            ELSE
            END;
            DEC(nel,n);  DEC(nrep,n);
          ELSE  -- nel=1 or nrep=1
            CASE cs OF
              1: llw(4); llw(6); ssw(0);
                 IF nel>1 THEN lla(4); c(Inc1); epop; END;
             |2: llw(4);  lla(6); li(2); c(Move); epop;epop;epop;
                 IF nel>1 THEN lla(4); li(2); c(Inc); epop; epop;END;
             |3: lla(4);  lla(7); CallExt(ForLib,ChAssign);epop;epop;
                 IF nel>1 THEN lla(5); li(I.lenel); c(Inc);epop;epop; END;
            ELSE
            END;
            DEC(nel); DEC(nrep);
          END;
        END;
      END;
      pn:=dnm.next;
    END;
    IF block THEN moveCode ELSE  DataCode; END;
    IF pc#NIL THEN Error(54); END;
    Dlist:=Dlist^.next;
-- print(' Dlist= %d \n', Dlist);
  END;
  IF NoStmErr#0 THEN ShowErrors; END;
END ProcData;

PROCEDURE DataProc;
BEGIN
  block:= self=blockdata;
  ProcData;
END DataProc;

PROCEDURE loadc(offs,val:INTEGER);
BEGIN
  llw(Mapoffs);
  IF offs<=255 THEN li(val); ssw(offs);
  ELSE  li(offs); li(val); c(SXW); epop; epop; epop;
  END;
END loadc;

PROCEDURE loade(offs:INTEGER; p:ADDRESS);
VAR pa:POINTER TO ARRAY [0..255] OF INTEGER;
    I:Info; i:INTEGER;
BEGIN
  pa:=p; i:=0;
  llw(Mapoffs); li(offs);
  LOOP
    CASE pa^[i]  OF
      0: EXIT;
     |1: INC(i); li(pa^[i]);
     |2: INC(i); I.name:=pa^[i];
         Unpack(I);
         IF GetType(I.name)#Int THEN idno:=I.name; ErrorId(13); END;
         IF (I.cl#Param) AND (I.cl#Global) THEN
           idno:=I.name; ErrorId(14);
         END;
         LoadVal(I);
     |3: c(NEG);
     |4: c(ADD); epop;
     |5: c(SUB); epop;
     |6: c(MUL); epop;
     |7: c(DiV); epop;
    ELSE
    END;
    INC(i);
  END;
  c(SXW); epop; epop; epop;
END loade;

PROCEDURE formDesc(VAR I:Info);
VAR da  :ALHD;
    p   :ADDRESS;
    offs:INTEGER;
    desc:INTEGER;
    i   :INTEGER;
    d   :INTEGER;
BEGIN
  p:=I.desc; offs:=p^; INC(p); desc:=offs;
  MOVE(ADR(da),p,I.dim*3);
  FOR i:=0 TO I.dim-1 DO
    d:=da[i].d; INC(offs);
    IF d>0 THEN
      loadc(offs,da[i].lo); INC(offs);
      loadc(offs,da[i].hi); INC(offs);
    ELSIF d=0 THEN
      loadc(offs,da[i].lo); INC(offs);
      loadc(offs,da[i].lo-1); INC(offs);
    ELSIF d=-1 THEN
      loade(offs,ADDRESS(da[i].lo)); INC(offs);
      llw(Mapoffs); li(offs-1); c(ADD); c(copt);
      lsw(0); li(1); c(SUB); epop; ssw(1); INC(offs);
    ELSIF d=-2 THEN
      loadc(offs,da[i].lo); INC(offs);
      loade(offs,ADDRESS(da[i].hi)); INC(offs);
    ELSIF d=-3 THEN
      loade(offs,ADDRESS(da[i].lo)); INC(offs);
      loadc(offs,da[i].hi); INC(offs);
    ELSIF d=-4 THEN
      loade(offs,ADDRESS(da[i].lo)); INC(offs);
      loade(offs,ADDRESS(da[i].hi)); INC(offs);
    END;
  END;
  llw(Mapoffs); li(desc); c(ADD); epop; li(I.dim*3);
  CallExt(ForLib,FormDesc); Setdepth(0);
  moveCode;
  IF NoStmErr#0 THEN ShowErrors; END;
END formDesc;

PROCEDURE CreateMap;
   VAR bmoffs,k,PS:INTEGER;
       md:   Mode;
BEGIN
  FOR PS:=0 TO HIGH(Id) DO
    IF Id[PS]#NIL THEN
      ps.name:=PS; md:=GetMode(PS);
      IF md#Empty THEN Unpack(ps); END;
      IF (md=Array) THEN
        IF ps.cl#Param THEN
          IF GetType(PS)# Char THEN
            bmoffs:=ps.darea-Mapoffs; k:=ps.offset;
            ps.darea:=Mapoffs+Maplength;
            pArray(bmoffs,Maplength,k);
            INC(Maplength);
            Pack(ps);
          END;
        ELSE
          IF ps.farray THEN
            k:=ps.dim*3+1;
            ps.desc^:=Loffs; INC(Loffs,k);
            formDesc(ps);
          END;
        END;
      END;  -- array processing
    END;  -- ps#nil
  END;  -- for
END CreateMap;

PROCEDURE ExtProc;
BEGIN
END ExtProc;

PROCEDURE check(id:INTEGER; charvar:BOOLEAN);
VAR tp:Types;
BEGIN
  tp:=GetType(id);
  IF charvar THEN
    IF tp#Char THEN Error(55); END;
  ELSE
    IF tp=Char THEN Error(55); END;
  END;
END check;

PROCEDURE LocalProc;
   VAR DAl,k,PS:INTEGER;
       md:   Mode;
       charvar:BOOLEAN;
BEGIN
  DAl:=Mapoffs; Loffs:=0;
  FOR PS:=0 TO HIGH(Id) DO
    IF Id[PS]#NIL THEN
      ps.name:=PS; md:=GetMode(PS);
      IF md#Empty THEN Unpack(ps); END;
      IF (md=Var) OR (md=Array) THEN
        IF ps.cl=Param THEN
          ps.darea:=1; ps.offset:=Mapoffs-ps.offset-1; Pack(ps);
        END;
        IF ps.darea=0 THEN
          charvar:=GetType(PS)=Char;
          IF ps.equiP=0 THEN
            l:=size(ps);
            ps.darea:=DAl;
            IF charvar THEN ps.offset:=Loffs*4; l:=(l+3) DIV 4;
                       ELSE ps.offset:=Loffs;
            END;
            Pack(ps); INC(Loffs,l);
          ELSE
            k:=ps.offset; Q:=ps.equiP;
            WHILE Q#PS DO
              q.name:=Q; Unpack(q);
              k:=Min(k,q.offset);
              Q:=q.equiP;
            END;  -- while
            IF charvar THEN k:=Loffs*4-k ELSE k:=Loffs-k; END;
            REPEAT
              q.name:=Q; Unpack(q);  q.darea:=DAl;
              q.offset:=q.offset+k;  l:=q.offset+size(q);
              IF charvar THEN l:=(l+3) DIV 4; END;
              Loffs:=Max(Loffs,l);
              Q:=q.equiP;  Pack(q);
            UNTIL Q=PS;
          END;   -- equi chain
        END;     -- set address to local
      ELSIF md=Proc THEN
        IF ps.cl=Ext THEN
          IdStr(PS,Ident); pExternal(Ident);
          ps.darea:=Mapoffs;
        ELSIF ps.cl=Param THEN
          ps.darea:=1; ps.offset:=Mapoffs-ps.offset-1;
        END;
        IF GetType(ps.name)= Char THEN
          IF ps.lenel=0 THEN Error(5); END; -- idno=ps.name ErrorId(?)
        END;
        Pack(ps);
      ELSIF md=xVar THEN
        IF ps.cl=Param THEN
          ps.darea:=1; ps.offset:=Mapoffs-ps.offset-1;
        END; Pack(ps);
      END;  -- element processing
    END;  -- ps#nil
  END;  -- for
  IF NoStmErr#0 THEN ShowErrors; END;
END LocalProc;

PROCEDURE SetCBaddr(VAR P:PBlock; DA:INTEGER);
  VAR FP,l1,ad:INTEGER;
      charcom:BOOLEAN;
BEGIN
  PS:=P^.FP; IF PS=-1 THEN RETURN END;
  FP:=PS; offs:=0; P^.da:=DA; P^.len:=0;
  charcom:=GetType(PS)=Char;
  REPEAT      -- until PS=FP
    check(PS,charcom);
    ps.name:=PS; Unpack(ps);
    l:=size(ps);
    IF ps.darea=0 THEN    -- адрес еще не присвоен
      IF ps.equiP=0 THEN  -- в эквив.  не участвует
        ps.darea:=DA; ps.offset:=offs;
        Pack(ps);
      ELSE      -- эквивалентность
        ad:=offs-ps.offset; Q:=PS;
        REPEAT
          check(Q,charcom);
          q.name:=Q; Unpack(q);  q.cl:=Global;
          q.offset:=q.offset+ad; q.darea:=DA;
          IF q.offset<0 THEN Error(50) END;
          l1:=size(q);
          P^.len:=Max(P^.len,q.offset+l1);
          Q:=q.equiP; Pack(q);
        UNTIL Q=PS;
      END;
    ELSE   --  адрес уже присвоен
      IF (ps.darea#DA) OR (ps.offset#offs) THEN Error(37) END;
    END;
    offs:=offs+l; PS:=ps.commP;
  UNTIL PS=FP ;
  P^.len:=Max(P^.len,offs);
  IF charcom THEN P^.len:=(P^.len+3) DIV 4; END;
  -- for linker "COMMON P^.name  DAoffs P^.len "
  pCommon(P^.name,  DA-Mapoffs, P^.len);
  IF NoStmErr#0 THEN ShowErrors; END;
END SetCBaddr;

PROCEDURE CommProc;
  VAR P:PBlock; DA:INTEGER;
BEGIN
  P:=PlBlock; DA:=Mapoffs;
  WHILE P#NIL  DO
    INC(DA); SetCBaddr(P,DA);
    INC(Maplength); P:=P^.next;
  END;
END CommProc;

PROCEDURE EquiProc;
VAR P,  PS,  Q,  Q1:INTEGER;
    I, I1:Info; md:Mode;
    Ps:PEquiStr; Po:PEquiObj;
    Boffset,z,diff:INTEGER;
    d, i, noerr:INTEGER;
    IP:POINTER TO ARRAY [1..7] OF INTEGER;
    DP:POINTER TO ARRAY [0..21] OF INTEGER;
BEGIN
  noerr:=0; Ps:=PEquiv;
  WHILE Ps#NIL DO
    P:=-1; Boffset:=0;
    Po:=Ps^.list;
    WHILE Po#NIL DO
      PS:=Po^.name; I.name:=PS; Unpack(I);
      md:=GetMode(PS);
      IF Po^.dim=0 THEN z:=Boffset;
         IF Po^.pos#0 THEN z:=z-Po^.pos+1; END;
      ELSE
        IF md#Array THEN INC(noerr); z:=Boffset;
        ELSE  -- compute offset
          DP:=I.desc; IP:=Po^.index; z:=0; d:=1;
          FOR i:=1 TO Po^.dim DO
            z:=z+IP^[i]*d; d:=DP^[i*3];
          END;
          z:=Boffset+(DP^[0]-z)*I.lenel;
          IF Po^.pos#0 THEN z:=z-Po^.pos+1; END;
        END;
      END;
      IF I.equiP = 0 THEN
        IF P=-1 THEN P:=PS; I.equiP:=PS; I.offset:=z; Pack(I);
        ELSE    I1.name:=P; Unpack(I1); I.equiP:=I1.equiP;
                I1.equiP:=PS; I.offset:=z;
                Pack(I); Pack(I1);
        END;
      ELSE
        diff:=z-I.offset; Boffset:=Boffset-diff; Q:=P;
        IF P=-1 THEN  P:=PS;
        ELSE
          LOOP
            REPEAT
              IF Q=PS THEN
                IF diff#0 THEN INC(noerr); END;
                EXIT;
              END;
              I1.name:=Q; Unpack(I1);
              I1.offset:=I1.offset-diff; Q1:=Q;
              Q:=I1.equiP; Pack(I1);
            UNTIL Q=P ;
            I1.name:=Q1; Unpack(I1);
            I1.equiP:=I.equiP; I.equiP:=P;
            Pack(I); Pack(I1); EXIT
          END;
        END;
      END;
      Po:=Po^.next;
    END;
    Ps:=Ps^.next;
  END;
  IF noerr#0 THEN Error(50); ShowErrors; END;
END EquiProc;

PROCEDURE TableProc;
VAR i: INTEGER;
BEGIN
  ShowLine(FALSE);
  Mapoffs:=4; IF ParNo>0 THEN Mapoffs:=Mapoffs+ParNo+1; END;
  Maplength:=1;
  EquiProc;
  CommProc;
  LocalProc;
  CreateMap;
  DataProc;
  Toffs:=Mapoffs+Maplength ;
  Tbegin:= Toffs; Tend:=Tbegin; Dpos:=0;
  FOR i:=Tbegin TO HIGH(TempMap) DO TempMap[i]:=0c END;
  ShowLine(TRUE);
END TableProc;

BEGIN
END fcTproc.
