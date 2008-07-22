IMPLEMENTATION MODULE fcExpr;

FROM SYSTEM  IMPORT WORD, ADDRESS, ADR;
FROM fcGen   IMPORT epop, epush, cp, c, c1, li, CallExt, llw, slw, sew,
                    Setdepth, Getdepth, InsertMode,InsertEnd,
                    InPoolStr, cOrJump, cAndJump, cEndLab;
FROM fcInd   IMPORT ldSubstr, LoadInd, EvalVal, InsertC, LoadConst,
                    LoadVal, LoadAdr, CompCstr, cCat,
                    genCxAdd, genCxMul,genCxCmp;
FROM fcScan  IMPORT GetSy,Symbol,sy,Expected,
                    Error,MarkPos1,BackUp1, CxConst,
                    cType,Ival,Rval,Slen,Sval,lookAhead,
                    Warning, KeyON;
FROM fcObj   IMPORT SetType,GetType,Types, Mode,GetMode,SetMode,indbit,
                    Info,Class,idno,Unpack,Pack,GenObj,Ident, GenLocal;
FROM fcDcl   IMPORT AllocVar,AllocExt,AllocIntr, LHD, ALHD;
FROM fcDefs  IMPORT maxpar;
FROM fcTproc IMPORT AllocTemp, FreeTemp, FreeDesc;
FROM fcProc  IMPORT FuncCall, LookIntr;
FROM StdIO   IMPORT Show, print;
FROM fcBugs  IMPORT AddStr;

TYPE trops=(topt, IopR, IopD, IopC, RopD, RopC, DopC,
                  RopI, DopI, CopI, DopR, CopR, CopD );

(*     Грамматика FORTRAN - Выражений :
  <Lexpr> = <Lsum>  {(".EQV."|".NEQV.") <Lsum>.
  <Lsum>  = <Lterm> {".OR." <Lterm>}.
  <Lterm> = <Lfactor> {".AND." <Lfactor>}.
  <Lfactor> = [".NOT."] <Lsimple>.
  <Lsimple> = <CAexpr> [<relation> <CAexpr>].
  <relation> = ".LT."|".GT."|".LE."|".GE."|".NE."|".EQ." .
  <CAexpr> = <Aexpr> {"//" <Aexpr>}.
  <Aexpr> = [("+"|"-")]<Term>{("+"|"-")<Term>}.
  <Term>   = <Factor> {("*"|"/")<Factor>}.
  <Factor> = <Simple>{"**"<Factor>
  <Simple>= "(" <Lexpr> ")"|<const>|<var>|<arrel>|<substr>|<funcall> .
*)

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

MODULE AbIndex;
(* FROM Main *) IMPORT  Symbol, sy, GetSy,
    Ival, Error, Expected,  cType, GenLocal,
    bind, MOVE, ADR, indbit, GenObj, Unpack, Pack, SetType,
    GetType, SetMode, GetMode, Mode, idno,Info, Ident,
    Types, Class;

EXPORT bIndex;
CONST maxlen=40; blen=maxlen-2;
VAR code:ARRAY [0..maxlen] OF INTEGER;
    cp:INTEGER;
    err:BOOLEAN;
CONST LC=1; (* load constant ; LC value *)
      LV=2; (* load variable;  LV idno  *)
      NEG=3; ADD=4; SUB=5; MULT=6; DiV=7;
      End=0;

PROCEDURE bapply(VAR b,b1:bind; op:Symbol);
VAR c,l:INTEGER;
BEGIN
  IF err THEN RETURN END;
  IF b.len=0 THEN
     IF b1.len=0 THEN
        CASE op OF
          slash: b.val:=b.val DIV b1.val;
         |times: b.val:=b.val * b1.val;
         |plus : b.val:=b.val + b1.val;
         |minus: b.val:=b.val - b1.val;
        END;
     ELSE c:=b1.val; l:=cp-c;
        MOVE(ADR(code[c+2]),ADR(code[c]),l);
        code[c]:=LC; code[c+1]:=b.val;
        b.len:=1; b.val:=c; INC(cp,2);
        IF cp > blen THEN Error(42); err:=TRUE END;
     END;
  ELSE
    IF b1.len=0 THEN
       code[cp]:=LC; INC(cp); code[cp]:=b1.val; INC(cp);
       IF cp > blen THEN Error(42); err:=TRUE END;
    END;
    CASE op OF
      slash: code[cp]:=DiV ;
     |times: code[cp]:=MULT ;
     |plus : code[cp]:=ADD;
     |minus: code[cp]:=SUB;
    END;
    INC(cp);
    IF cp > blen THEN Error(42); err:=TRUE END;
  END;
END bapply;

PROCEDURE biexpr(VAR b:bind); FORWARD;

PROCEDURE bifactor(VAR b:bind);
VAR I:Info; tp:Types; len:INTEGER;
BEGIN
  IF sy=const THEN
    IF cType=Int THEN b.len:=0; b.val:=Ival
    ELSE Error(27); err:=TRUE
    END;
  ELSIF sy=lpar THEN
    GetSy; biexpr(b);
    IF sy#rpar THEN Expected(rpar); err:=TRUE END;
  ELSIF sy=id THEN
    CASE GetMode(idno) OF
      Proc,Array : Error(14); err:=TRUE;
     |Var,xVar : I.name:=idno; Unpack(I);
                 INCL(I.bits,indbit); Pack(I);
                 b.len:=1; b.val:=cp; code[cp]:=LV; INC(cp);
                 code[cp]:=idno; INC(cp);
                 IF cp>blen THEN Error(42); err:=TRUE END;
     |Const : I.name:=idno; Unpack(I); b.len:=0; b.val:=I.offset;
              IF GetType(idno)#Int THEN Error(27); err:=TRUE END;
     |Empty : I.name:=idno; GenLocal(I);
              I.bits:={indbit};
              GenObj(I);
              b.len:=1; b.val:=cp; code[cp]:=LV; INC(cp);
              code[cp]:=idno; INC(cp);
              IF cp>blen THEN Error(42); err:=TRUE END;
    END;
  ELSE Error(14); err:=TRUE
  END;
  GetSy;
END bifactor;

PROCEDURE biterm(VAR b:bind);
VAR b1:bind; op:Symbol;
BEGIN
  bifactor(b);
  WHILE (sy=times) OR (sy=slash) DO
    op:=sy; GetSy; bifactor(b1);
    bapply(b,b1,op);
  END;
END biterm;

PROCEDURE biexpr(VAR b:bind);
VAR b1:bind; op:Symbol;
BEGIN
  IF sy=plus THEN op:=sy; GetSy;
  ELSIF sy=minus THEN op:=sy; GetSy;
  ELSE op:=plus
  END;
  biterm(b);
  IF op=minus THEN
     IF b.len=0 THEN b.val:=-b.val
     ELSE  code[cp]:=NEG; INC(cp);
           IF cp>blen THEN Error(42); err:=TRUE END;
     END;
  END;
  WHILE (sy=plus) OR (sy=minus) DO
    op:=sy; GetSy; biterm(b1);
    bapply(b,b1,op);
  END;
END biexpr;

PROCEDURE bIndex(VAR bi:bind):BOOLEAN;
VAR b:bind;
BEGIN
  err:=FALSE; cp:=0; biexpr(b);
  IF b.len=0 THEN bi.len:=0; bi.val:=b.val;
  ELSE code[cp]:=End; INC(cp);
    bi.len:=cp; bi.val:=ADR(code[0]);
  END;
  RETURN err
END bIndex;

END AbIndex;

CONST LSS =0A0h; LEQ =0A1h; GTR =0A2h; NEG =0A7h;
      GEQ =0A3h; EQU =0A4h; NEQ =0A5h; LXW  =41h;
      FFCT =9Fh; FADD =98h; FSUB =99h; FABS =9Dh;
      FMUL =9Ah; FDIV =9Bh; FNEG =9Eh; FCMP =9Ch;
      ADD  =88h; SUB  =89h; MUL  =8Ah; Div  =8Bh;
      And =0A9h; Or  =0A8h; Not =0AEh; LSA  =16h;
      CHKZ=0C7h; Drop=0B1h; Stot=0E8h; DECS=0B0h;
      SXW  =51h; LGW2 =42h; copt=0B5h; swap=0F0h;

CONST ForLib=2; Powii=34; Powri=35; Powdi=36; Powci=37;
                Powrr=38; Powdd=39; Powcc=40;
     ChMove=6;  ChCmp=9;  ChTop=2;

PROCEDURE genChMove(VAR ob:Dexpr);
BEGIN
  CallExt(ForLib,ChMove);
END genChMove;

PROCEDURE genChCmp;
BEGIN
  CallExt(ForLib,ChCmp);
END genChCmp;

PROCEDURE AddTcode( ops:trops);
BEGIN
  CASE ops OF
    RopI: c1(FFCT,0);
   |DopI: c1(FFCT,0);  -- ItoDp;
   |CopI: c1(FFCT,0); li(0);
   |DopR:              -- RtoDp;
   |CopR: li(0);
   |CopD: li(0);       -- DtoCx;
  ELSE
  END;
END AddTcode;

PROCEDURE InsertTcode(VAR ob:Dexpr; ops:trops);
  VAR insert:BOOLEAN;
BEGIN
  IF ob.emd=cexpr THEN insert:=FALSE ELSE insert:=TRUE END;
  IF insert THEN InsertMode(ob.start); END;
  CASE ops OF
    IopR:  c1(FFCT,0);
   |IopD:  c1(FFCT,0);  -- ItoDp;
   |IopC:  c1(FFCT,0); li(0);
   |RopD:               -- RtoDp;
   |RopC:  li(0);
   |DopC:  li(0);       -- DtoCx;
    ELSE
  END;
  IF insert THEN InsertEnd; END;
END InsertTcode;

PROCEDURE TransC(VAR ob:Dexpr; ops:trops);
VAR r:REAL;
BEGIN
  CASE ops  OF
    IopR,RopI: r:=FLOAT(ob.name); ob.name:=WORD(r);
               ob.tp:=Real;
   |IopD,DopI: -- cItoDp(ob);
               r:=FLOAT(ob.name); ob.name:=WORD(r);
               r:=0.; ob.wd:=WORD(r);
               ob.tp:=Double;
   |IopC,CopI: r:=FLOAT(ob.name); ob.name:=WORD(r);
               r:=0.; ob.wd:=WORD(r);
               ob.tp:=Complex;
   |RopD,DopR: -- cRtoDp(ob);
               r:=REAL(ob.name);  ob.name:=WORD(r);
               r:=0.; ob.wd:=WORD(r);
               ob.tp:=Double;
   |RopC,CopR: r:=REAL(ob.name);  ob.name:=WORD(r);
               r:=0.; ob.wd:=WORD(r);
               ob.tp:=Complex;
   |DopC,CopD: r:=REAL(ob.name);  ob.name:=WORD(r);
               r:=0.; ob.wd:=WORD(r);
               ob.tp:=Complex;
  END;
END TransC;

PROCEDURE TypeComp(VAR L,R:Dexpr;
                       op:Symbol;
                       ):BOOLEAN;  -- TRUE if error
VAR ops:trops; left:BOOLEAN; cp0: INTEGER;
BEGIN
  IF L.emd=invmd THEN RETURN TRUE END;
  IF R.emd=invmd THEN L.emd:=invmd; RETURN TRUE END;
  IF op=power    THEN RETURN FALSE END;
  IF L.tp=R.tp   THEN ops:=topt; RETURN FALSE
  ELSIF L.tp=Int THEN
    CASE R.tp OF
      Real :    ops:=IopR; L.tp:=Real;
     |Double :  ops:=IopD; L.tp:=Double;
     |Complex : ops:=IopC; L.tp:=Complex;
    ELSE RETURN TRUE
    END;
  ELSIF L.tp=Real THEN
    CASE R.tp OF
      Int :     ops:=RopI;
     |Double :  ops:=RopD; L.tp:=Double;
     |Complex : ops:=RopC; L.tp:=Complex;
    ELSE RETURN TRUE
    END;
  ELSIF L.tp=Double THEN
    CASE R.tp OF
      Int :     ops:=DopI;
     |Real :    ops:=DopR;
     |Complex : ops:=DopC; L.tp:=Complex;
    ELSE RETURN TRUE
    END;
  ELSIF L.tp=Complex THEN
    CASE R.tp OF
      Int :     ops:=CopI;
     |Real :    ops:=CopR;
     |Double :  ops:=CopD;
    ELSE RETURN TRUE
    END;
  ELSE
    L.emd:=invmd; RETURN  TRUE
  END;
  IF ORD(ops)<ORD(RopI) THEN
    IF L.emd=cexpr THEN TransC(L,ops);
    ELSE                InsertTcode(R,ops);
    END;
  ELSE
    IF R.emd=cexpr THEN TransC(R,ops);
    ELSE                AddTcode(ops);
    END;
  END;
  RETURN FALSE
END TypeComp;

PROCEDURE powercc(VAR L,R:Dexpr);
BEGIN
  LoadConst(L); LoadConst(R);
  CallExt(ForLib,Powcc); L.emd:=expr; epop; -- epop;
 -- not implemented yet!!!
   Error(64);
END powercc;

PROCEDURE powerdd(VAR L,R:Dexpr);
BEGIN
  LoadConst(L); LoadConst(R);
  CallExt(ForLib,Powdd); L.emd:=expr; epop; -- epop;
END powerdd;

PROCEDURE powercr(VAR L,R:Dexpr);
BEGIN
 TransC(R,CopR); powercc(L,R);
END powercr;

PROCEDURE powerci(VAR L,R:Dexpr);
BEGIN
 -- not implemented yet!!!
   Error(64);
END powerci;

PROCEDURE powerdr(VAR L,R:Dexpr);
BEGIN
 TransC(R,DopR); powerdd(L,R);
END powerdr;

PROCEDURE powerrr(VAR L,R:Dexpr);
BEGIN
  LoadConst(L); LoadConst(R);
  CallExt(ForLib,Powrr); L.emd:=expr; epop;
END powerrr;

PROCEDURE powerir(VAR L,R:Dexpr);
BEGIN
 TransC(L,RopI); powerrr(L,R);
END powerir;

PROCEDURE powerid(VAR L,R:Dexpr);
BEGIN
 TransC(L,DopI); powerdd(L,R);
END powerid;

PROCEDURE poweric(VAR L,R:Dexpr);
BEGIN
 TransC(L,CopI); powercc(L,R);
END poweric;

PROCEDURE powerrd(VAR L,R:Dexpr);
BEGIN
 TransC(L,DopR); powerdd(L,R);
END powerrd;

PROCEDURE powerrc(VAR L,R:Dexpr);
BEGIN
 TransC(L,CopR); powercc(L,R);
 -- not implemented yet!!!
   Error(64);
END powerrc;

PROCEDURE powerii(VAR l,r:Dexpr);
VAR i,j,k:INTEGER;
BEGIN
  i:=l.name; j:=r.name;
  IF j<0 THEN k:=0
  ELSE k:=1;
    WHILE j> 0 DO  k:=k*i; DEC(j) END;
  END;
  l.name:=k;
END powerii;

PROCEDURE powerri(VAR l,r:Dexpr);
VAR j:INTEGER; i,k:REAL; neg:BOOLEAN;
BEGIN
  i:=REAL(l.name); j:=r.name;
  IF j<0 THEN neg:=TRUE; j:=-j;
  ELSE neg:=FALSE;
  END;
  k:=1.;
  WHILE j> 0 DO  k:=k*i; DEC(j) END;
  IF neg THEN k:=1./k END;
  l.name:=INTEGER(k);
END powerri;

PROCEDURE powerdi(VAR l,r:Dexpr);
VAR j:INTEGER; i,k:REAL; neg:BOOLEAN;
BEGIN
  i:=REAL(l.name); j:=r.name;
  IF j<0 THEN neg:=TRUE; j:=-j;
  ELSE neg:=FALSE;
  END;
  k:=1.;
  WHILE j> 0 DO  k:=k*i; DEC(j) END;
  IF neg THEN k:=1./k END;
  l.name:=INTEGER(k);
END powerdi;

PROCEDURE powerCC(VAR L,R:Dexpr);
BEGIN
  CASE L.tp OF
    Int:
       IF    R.tp=Int     THEN  powerii(L,R)
       ELSIF R.tp=Real    THEN  powerir(L,R)
       ELSIF R.tp=Double  THEN  powerid(L,R)
       ELSIF R.tp=Complex THEN  poweric(L,R)
       ELSE  Error(13)
       END;
   |Real:
       IF    R.tp=Int     THEN  powerri(L,R)
       ELSIF R.tp=Real    THEN  powerrr(L,R)
       ELSIF R.tp=Double  THEN  powerrd(L,R)
       ELSIF R.tp=Complex THEN  powerrc(L,R)
       ELSE  Error(13)
       END;
   |Double:
       IF    R.tp=Int     THEN  powerdi(L,R)
       ELSIF R.tp=Real    THEN  powerdr(L,R)
       ELSIF R.tp=Double  THEN  powerdd(L,R)
       ELSIF R.tp=Complex THEN  Error(13)
       ELSE  Error(13)
       END;
   |Complex:
       IF    R.tp=Int     THEN  powerci(L,R)
       ELSIF R.tp=Real    THEN  powercr(L,R)
       ELSIF R.tp=Double  THEN  Error(13)
       ELSIF R.tp=Complex THEN  powercc(L,R)
       ELSE  Error(13)
       END;
  ELSE Error(13)
  END;
END powerCC;

PROCEDURE Powercc(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN  LoadConst(R);
  END;
  CallExt(ForLib,Powcc); epop; -- epop;
 -- not implemented yet!!!
   Error(64);
END Powercc;

PROCEDURE Powercr(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN InsertC(L,R.start); L.emd:=expr; END;
  IF R.emd=cexpr THEN TransC(R,CopR); LoadConst(R);
  ELSE AddTcode(CopR);
  END; L.tp:=Complex;
  CallExt(ForLib,Powcc); epop; -- epop;
 -- not implemented yet!!!
   Error(64);
END Powercr;

PROCEDURE Powerci(VAR L,R:Dexpr);
BEGIN
  IF    L.emd=cexpr THEN InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN LoadConst(R);
  END;
  CallExt(ForLib,Powci); epop;
 -- not implemented yet!!!
   Error(64);
END Powerci;

PROCEDURE Powerdd(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN  LoadConst(R);
  END;
  CallExt(ForLib,Powdd); epop; -- epop;
END Powerdd;

PROCEDURE Powerdr(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN InsertC(L,R.start); L.emd:=expr; END;
  IF R.emd=cexpr THEN TransC(R,DopR); LoadConst(R);
  ELSE AddTcode(DopR);
  END; L.tp:=Double;
  CallExt(ForLib,Powdd); epop; -- epop;
END Powerdr;

PROCEDURE Powerdi(VAR L,R:Dexpr);
BEGIN
  IF    L.emd=cexpr THEN InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN LoadConst(R);
  END;
  CallExt(ForLib,Powdi); epop;
END Powerdi;

PROCEDURE Powerrc(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN TransC(L,RopC);
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN AddTcode(CopR); LoadConst(R);
  ELSE InsertTcode(R,RopC);
  END; L.tp:=Complex;
  CallExt(ForLib,Powcc); epop; -- epop;
 -- not implemented yet!!!
   Error(64);
END Powerrc;

PROCEDURE Powerrd(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN TransC(L,RopD);
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN AddTcode(DopR); LoadConst(R);
  ELSE InsertTcode(R,RopD);
  END; L.tp:=Double;
  CallExt(ForLib,Powdd); epop; -- epop;
END Powerrd;

PROCEDURE Powerrr(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN  LoadConst(R);
  END;
  CallExt(ForLib,Powrr); epop;
END Powerrr;

PROCEDURE Powerri(VAR L,R:Dexpr);
VAR w:WORD; t:INTEGER;
BEGIN
  IF L.emd=cexpr THEN InsertC(L,R.start); L.emd:=expr;
    CallExt(ForLib,Powri); epop;
  ELSIF R.emd=cexpr THEN
      CASE R.name OF
       0:  c(Drop); epop; w:=1.; li(w);
       |1:
       |2: epush; c(copt); c(FMUL); epop;
       |3: IF Getdepth()<6 THEN
             c(copt); c(copt); epush; epush;
             c(FMUL); c(FMUL); epop;  epop;
           ELSE
             AllocTemp(t,1); epush;  c(copt);
             slw(t); c(copt); epush; c(FMUL); epop;
             llw(t); c(FMUL); epop;
             FreeTemp(t,1);
           END;
       |4: epush; c(copt); c(FMUL);
                  c(copt); c(FMUL); epop;
       |5: IF Getdepth()<6 THEN
             c(copt); c(copt); epush; epush;
             c(FMUL); c(copt); c(FMUL); c(FMUL); epop; epop;
           ELSE
             AllocTemp(t,1); epush; c(copt);
             slw(t); epush; c(copt); c(FMUL);
             c(copt);c(FMUL); epop;
             llw(t); c(FMUL); epop;
             FreeTemp(t,1);
           END;
       |6: IF Getdepth()<6 THEN
             epush; c(copt); c(FMUL);
             c(copt); epush; c(copt); c(FMUL); c(FMUL); epop; epop;
           ELSE
             AllocTemp(t,1); epush; c(copt); c(FMUL); c(copt);
             slw(t); epush; c(copt); c(FMUL); epop;
             llw(t); c(FMUL); epop;
             FreeTemp(t,1);
           END;
       |8: epush; c(copt); c(FMUL);
                  c(copt); c(FMUL);
                  c(copt); c(FMUL); epop;
      ELSE LoadConst(R);
           CallExt(ForLib,Powri); epop;
      END;
  ELSE  CallExt(ForLib,Powri); epop;
  END;
END Powerri;

PROCEDURE Poweric(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN TransC(L,IopC);
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN AddTcode(CopI); LoadConst(R);
  ELSE InsertTcode(R,IopC);
  END; L.tp:=Complex;
  CallExt(ForLib,Powcc); epop; -- epop;
 -- not implemented yet!!!
   Error(64);
END Poweric;

PROCEDURE Powerid(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN TransC(L,IopD);
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN AddTcode(DopI); LoadConst(R);
  ELSE InsertTcode(R,IopD);
  END; L.tp:=Double;
  CallExt(ForLib,Powdd); epop; -- epop;
END Powerid;

PROCEDURE Powerir(VAR L,R:Dexpr);
BEGIN
  IF L.emd=cexpr THEN TransC(L,IopR);
    InsertC(L,R.start); L.emd:=expr;
  ELSIF R.emd=cexpr THEN AddTcode(RopI); LoadConst(R);
  ELSE InsertTcode(R,IopR);
  END; L.tp:=Real;
  CallExt(ForLib,Powrr); epop;
END Powerir;

PROCEDURE Powerii(VAR L,R:Dexpr);
  VAR t:INTEGER;
BEGIN
  IF L.emd=cexpr THEN InsertC(L,R.start); L.emd:=expr;
    CallExt(ForLib,Powii); epop;
  ELSIF R.emd=cexpr THEN
    IF R.name<0 THEN c(Drop); li(0);
    ELSE
      CASE R.name OF
       0:  c(Drop); epop; li(1);
       |1:
       |2: epush; c(copt); c(MUL); epop;
       |3: IF Getdepth()<6 THEN epush; epush;
             c(copt); c(copt);
             c(MUL); c(MUL); epop; epop;
           ELSE
             AllocTemp(t,1); epush; c(copt);
             slw(t); epush; c(copt); c(MUL); epop;
             llw(t); c(MUL); epop;
             FreeTemp(t,1);
           END;
       |4: epush; c(copt); c(MUL);
                  c(copt); c(MUL); epop;
       |5: IF Getdepth()<6 THEN epush; epush;
             c(copt); c(copt);
             c(MUL); c(copt); c(MUL); c(MUL); epop; epop;
           ELSE
             AllocTemp(t,1); epush; c(copt);
             slw(t); epush; c(copt); c(MUL);
             c(copt);c(MUL); epop;
             llw(t); c(MUL); epop;
             FreeTemp(t,1);
           END;
       |6: IF Getdepth()<6 THEN epush;
             c(copt); c(MUL);
             c(copt); epush; c(copt); c(MUL); c(MUL); epop; epop;
           ELSE
             AllocTemp(t,1); epush; c(copt); c(MUL); c(copt);
             slw(t); epush; c(copt); c(MUL); epop;
             llw(t); c(MUL); epop;
             FreeTemp(t,1);
           END;
       |8: epush; c(copt); c(MUL);
                  c(copt); c(MUL);
                  c(copt); c(MUL); epop;
      ELSE LoadConst(R);
           CallExt(ForLib,Powii); epop;
      END;
    END;
  ELSE  CallExt(ForLib,Powii); epop;
  END;
END Powerii;

PROCEDURE Powerxx(VAR L,R:Dexpr);
BEGIN
  CASE L.tp OF
    Int:
       IF    R.tp=Int     THEN  Powerii(L,R)
       ELSIF R.tp=Real    THEN  Powerir(L,R)
       ELSIF R.tp=Double  THEN  Powerid(L,R)
       ELSIF R.tp=Complex THEN  Poweric(L,R)
       ELSE  Error(13)
       END;
   |Real:
       IF    R.tp=Int     THEN  Powerri(L,R)
       ELSIF R.tp=Real    THEN  Powerrr(L,R)
       ELSIF R.tp=Double  THEN  Powerrd(L,R)
       ELSIF R.tp=Complex THEN  Powerrc(L,R)
       ELSE  Error(13)
       END;
   |Double:
       IF    R.tp=Int     THEN  Powerdi(L,R)
       ELSIF R.tp=Real    THEN  Powerdr(L,R)
       ELSIF R.tp=Double  THEN  Powerdd(L,R)
       ELSIF R.tp=Complex THEN  Error(13)
       ELSE  Error(13)
       END;
   |Complex:
       IF    R.tp=Int     THEN  Powerci(L,R)
       ELSIF R.tp=Real    THEN  Powercr(L,R)
       ELSIF R.tp=Double  THEN  Error(13)
       ELSIF R.tp=Complex THEN  Powercc(L,R)
       ELSE  Error(13)
       END;
  ELSE Error(13)
  END;
END Powerxx;

PROCEDURE CApply(VAR L,R:Dexpr; op:Symbol);
VAR r1,r2,rr:REAL; im1,im2:REAL;
    r:WORD;
    b1,b2:BOOLEAN; x:INTEGER;
BEGIN
  IF L.tp= Int THEN
    CASE op OF
       slash: L.name:=L.name DIV R.name;
     | times: L.name:=L.name  *  R.name;
     | plus : L.name:=L.name  +  R.name;
     | minus: L.name:=L.name  -  R.name;
     | lt  : L.name:=INTEGER(L.name <  R.name);  L.tp:=Logic;
     | le  : L.name:=INTEGER(L.name <= R.name);  L.tp:=Logic;
     | eq  : L.name:=INTEGER(L.name =  R.name);  L.tp:=Logic;
     | ne  : L.name:=INTEGER(L.name #  R.name);  L.tp:=Logic;
     | ge  : L.name:=INTEGER(L.name >= R.name);  L.tp:=Logic;
     | gt  : L.name:=INTEGER(L.name >  R.name);  L.tp:=Logic;
    ELSE Error(12); L.emd:=invmd;
    END;
  ELSIF L.tp=Real THEN
    r1:=REAL(L.name); r2:=REAL(R.name);
    CASE op OF
      slash:   r:= r1 /  r2;  L.name:=WORD(r);
      | times: r:= r1 *  r2;  L.name:=WORD(r);
      | plus:  r:= r1 +  r2;  L.name:=WORD(r);
      | minus: r:= r1 -  r2;  L.name:=WORD(r);
      | lt   : r:= r1 <  r2;  L.name:=r; L.tp:=Logic;
      | le   : r:= r1 <= r2;  L.name:=r; L.tp:=Logic;
      | eq   : r:= r1 =  r2;  L.name:=r; L.tp:=Logic;
      | ne   : r:= r1 #  r2;  L.name:=r; L.tp:=Logic;
      | ge   : r:= r1 >= r2;  L.name:=r; L.tp:=Logic;
      | gt   : r:= r1 >  r2;  L.name:=r; L.tp:=Logic;
    ELSE Error(12); L.emd:=invmd;
    END;
  ELSIF L.tp=Double THEN
(* same as real  *)
    r1:=REAL(L.name); r2:=REAL(R.name);
    CASE op OF
      slash:   r:= r1 /  r2; L.name:=WORD(r);
      | times: r:= r1 *  r2; L.name:=WORD(r);
      | plus:  r:= r1 +  r2; L.name:=WORD(r);
      | minus: r:= r1 -  r2; L.name:=WORD(r);
      | lt   : r:= r1 <  r2; L.name:=r; L.tp:=Logic;
      | le   : r:= r1 <= r2; L.name:=r; L.tp:=Logic;
      | eq   : r:= r1 =  r2; L.name:=r; L.tp:=Logic;
      | ne   : r:= r1 #  r2; L.name:=r; L.tp:=Logic;
      | ge   : r:= r1 >= r2; L.name:=r; L.tp:=Logic;
      | gt   : r:= r1 >  r2; L.name:=r; L.tp:=Logic;
    ELSE Error(12); L.emd:=invmd;
    END;
  ELSIF L.tp=Complex THEN
    r1:= REAL(L.name); r2:=REAL(R.name);
    im1:=REAL(L.wd);  im2:=REAL(R.wd);
    CASE op OF
      slash:  rr:=r2*r2 + im2*im2;
              r:=(r1*r2 + im1*im2) / rr;
              L.name:=WORD(r);
              r:=(r2*im1 - r1*im2) / rr;
              L.wd:=r;
      |times: r:=r1 * r2 - im1 * im2;
              L.name:=WORD(r);
              r:= r1 * im2 + im1 * r2;
              L.wd:=r;
      |plus:  r:= r1 + r2;   L.name:=WORD(r);
              r:=im1 + im2;  L.wd:=r;
      |minus: r:= r1 - r2;   L.name:=WORD(r);
              r:=im1 - im2;  L.wd:=r;
      |eq   : r:=(r1 = r2) AND (im1=im2) ; L.name:=r; L.tp:=Logic;
      |ne   : r:=(r1 # r2) AND (im1#im2) ; L.name:=r; L.tp:=Logic;
    ELSE Error(12); L.emd:=invmd;
    END;
  ELSIF L.tp=Logic THEN
    b1:=BOOLEAN(L.name); b2:=BOOLEAN(R.name);
    CASE op  OF
      and: IF b1 AND b2 THEN L.name:=INTEGER(TRUE)
           ELSE L.name:=INTEGER(FALSE)
           END;  -- remove cAndJump !!!!
     |or : IF b1 OR b2 THEN L.name:=INTEGER(TRUE)
           ELSE L.name:=INTEGER(FALSE)
           END;  -- remove cOrJump  !!!!
     |eqv: IF b1 THEN
              IF b2 THEN L.name:=INTEGER(TRUE)
              ELSE L.name:=INTEGER(FALSE)
              END;
            ELSE
              IF b2 THEN L.name:=INTEGER(FALSE)
              ELSE L.name:=INTEGER(TRUE)
              END;
            END;
     |neqv: IF b1 THEN
              IF b2 THEN L.name:=INTEGER(FALSE)
              ELSE L.name:=INTEGER(TRUE)
              END;
            ELSE
              IF b2 THEN L.name:=INTEGER(TRUE)
              ELSE L.name:=INTEGER(FALSE)
              END;
            END;
    ELSE Error(12); L.emd:=invmd;
    END;
  ELSIF L.tp=Char THEN
    IF op=cat THEN  cCat(L,R)
    ELSE
      x:=CompCstr(L.wd,R.wd);
      CASE op OF
        lt   : r:= x < 0 ; L.name:=r; L.tp:=Logic;
      | le   : r:= x <=0 ; L.name:=r; L.tp:=Logic;
      | eq   : r:= x = 0 ; L.name:=r; L.tp:=Logic;
      | ne   : r:= x # 0 ; L.name:=r; L.tp:=Logic;
      | ge   : r:= x >=0 ; L.name:=r; L.tp:=Logic;
      | gt   : r:= x > 0 ; L.name:=r; L.tp:=Logic;
      ELSE Error(12); L.emd:=invmd;
      END;
    END;
  ELSE Error(13); L.emd:=invmd;
  END;
END CApply;

PROCEDURE Power(VAR L,R:Dexpr);
BEGIN
  IF (L.emd=cexpr) AND (R.emd=cexpr) THEN
    powerCC(L,R);
  ELSE
    Powerxx(L,R);
  END;
END Power;

PROCEDURE f(cmd:INTEGER);
BEGIN
   c(FCMP); c(cmd); -- epop;
END f;

PROCEDURE dpcmp(cmd:INTEGER);
BEGIN
 f(cmd); -- epop;
END dpcmp;

PROCEDURE dpadd(add:BOOLEAN);
BEGIN
  IF add THEN c(FADD);
  ELSE        c(FSUB);
  END;
END dpadd;

PROCEDURE dpmul(mul:BOOLEAN);
BEGIN
  IF mul THEN c(FMUL);
  ELSE        c(FDIV);
  END;
END dpmul;

PROCEDURE Apply(VAR L,R:Dexpr; op:Symbol);
BEGIN
  IF op=power  THEN Power(L,R); RETURN END;
  IF TypeComp(L,R,op) THEN RETURN END;
  IF (L.emd=cexpr)  THEN
    IF (R.emd=cexpr) THEN CApply(L,R,op); RETURN
    ELSE                  InsertC(L,R.start);
    END;
  ELSE
    IF (R.emd=cexpr) THEN LoadConst(R); END;
  END;
  L.emd:=expr;
  IF L.tp= Int THEN
    CASE op OF
       slash:c(Div); | times:c(MUL); | plus: c(ADD);
     | minus: c(SUB);
     | lt  : c(LSS); L.tp:=Logic; | le  : c(LEQ); L.tp:=Logic;
     | eq  : c(EQU); L.tp:=Logic; | ne  : c(NEQ); L.tp:=Logic;
     | ge  : c(GEQ); L.tp:=Logic; | gt  : c(GTR); L.tp:=Logic;
    ELSE Error(12);
    END;
     epop;
  ELSIF L.tp=Real THEN
    CASE op OF
      slash: c(FDIV); | times: c(FMUL); | plus : c(FADD);
    | minus: c(FSUB);
    | lt   : f(LSS); L.tp:=Logic | le   : f(LEQ); L.tp:=Logic;
    | eq   : f(EQU); L.tp:=Logic | ne   : f(NEQ); L.tp:=Logic;
    | ge   : f(GEQ); L.tp:=Logic | gt   : f(GTR); L.tp:=Logic
    ELSE Error(12);
    END;
     epop;
  ELSIF L.tp=Double THEN
    CASE op OF
      slash: dpmul(FALSE); | times: dpmul(TRUE);
    | plus : dpadd(TRUE);  | minus: dpadd(FALSE);
    | lt   : dpcmp(LSS); L.tp:=Logic | le   : dpcmp(LEQ); L.tp:=Logic;
    | eq   : dpcmp(EQU); L.tp:=Logic | ne   : dpcmp(NEQ); L.tp:=Logic;
    | ge   : dpcmp(GEQ); L.tp:=Logic | gt   : dpcmp(GTR); L.tp:=Logic
    ELSE Error(12);
    END;
     epop;
  ELSIF L.tp=Complex THEN
    CASE op OF
      slash:   genCxMul(FALSE);
      |times:  genCxMul(TRUE);
      |plus:   genCxAdd(TRUE);
      |minus:  genCxAdd(FALSE);
      |eq   : L.tp:=Logic;  genCxCmp(TRUE);
      |ne   : L.tp:=Logic;  genCxCmp(FALSE);
    ELSE Error(12); L.emd:=invmd;
    END;
  ELSIF L.tp=Logic THEN
    CASE op  OF
      and,or: cEndLab;
     |eqv:    c(EQU); epop; epop;
     |neqv:   c(NEQ); epop; epop;
    ELSE Error(12); L.emd:=invmd;
    END;

  ELSIF L.tp=Char THEN
      genChCmp;
      IF L.emd=expr THEN    llw(INTEGER(L.wd)+1); sew(ForLib,ChTop);
      ELSIF R.emd=expr THEN llw(INTEGER(R.wd)+1); sew(ForLib,ChTop);
      END; FreeDesc(L.wd); FreeDesc(R.wd);
      CASE op OF
        lt  : c(LSS); L.tp:=Logic; | le  : c(LEQ); L.tp:=Logic;
      | eq  : c(EQU); L.tp:=Logic; | ne  : c(NEQ); L.tp:=Logic;
      | ge  : c(GEQ); L.tp:=Logic; | gt  : c(GTR); L.tp:=Logic;
      ELSE Error(12); L.emd:=invmd;
      END;
      epop;
  ELSE Error(13); L.emd:=invmd;
  END;
END Apply;

PROCEDURE arrayelem(VAR ob:Dexpr);
  VAR I:Info;
BEGIN
  I.name:=idno; Unpack(I);
  ob.name:=idno; ob.emd:=arr;
  GetSy;
  IF sy#lpar THEN
    IF ob.tp=Char THEN ldSubstr(I,ob)
    ELSE LoadAdr(I);
    END; RETURN
  END;
  IF ob.tp#Char THEN LoadAdr(I) END;
  ob.emd:=arrel; ob.wd:=LoadInd(I);
  GetSy;
  IF ob.tp=Char THEN ldSubstr(I,ob)
  ELSE               EvalVal (I,ob)
  END;
END arrayelem;

PROCEDURE Function(VAR ob:Dexpr);
BEGIN
  ob.name:=idno; GetSy;
  IF sy=lpar THEN  FuncCall(ob)
  ELSE ob.emd:=ext
  END
END Function;

PROCEDURE CONSTANT(VAR ob:Dexpr);
  VAR I:Info;
BEGIN
  I.name:=idno; Unpack(I);
  ob.emd:=cexpr; ob.name:=I.offset; ob.wd:=I.commP;
  GetSy
END CONSTANT;

PROCEDURE ldConst(VAR ob:Dexpr);
BEGIN
  ob.emd:=cexpr;
  CASE cType OF
    Int,Logic : ob.name:=Ival;
   |Real      : ob.name:=WORD(Rval);
   |Double    : ob.name:= WORD(Rval); ob.wd:=WORD(Ival);
   |Complex   : ob.name:= WORD(Rval); ob.wd:=WORD(Ival);
   |Char,Holl : ob.name:=Slen;
                ob.wd:=InPoolStr(Sval);
  END;
  GetSy;
END ldConst;

PROCEDURE LoadVar(VAR ob:Dexpr);
VAR I:Info;
BEGIN
   I.name:=idno; Unpack(I);
   ob.name:=idno; ob.emd:=var;
   GetSy;
   IF ob.tp=Char THEN ldSubstr(I,ob);
   ELSE               LoadVal(I);
   END;
END LoadVar;

PROCEDURE LoadLocVar(VAR ob:Dexpr);
VAR I:Info;
BEGIN
   I.name:=idno; Unpack(I);
   ob.name:=idno; ob.emd:=var;
   GetSy;
   IF ob.tp=Char THEN ldSubstr(I,ob);
   ELSE            -- LoadLocVal(I);
   END;
END LoadLocVar;

VAR FN:INTEGER;
PROCEDURE SetFN(i:INTEGER);
BEGIN FN:=i; END SetFN;

PROCEDURE Fused(VAR ob:Dexpr; md:Mode);
  VAR I:Info; tp:Types; no:INTEGER;
BEGIN
  I.name:=idno;
  IF md=Empty THEN
   GenLocal(I); GenObj(I); ob.tp:=GetType(I.name);
  ELSE Unpack(I)
  END;
  MarkPos1; GetSy; BackUp1; idno:=I.name;
  IF sy#lpar THEN
    (* Генерируем новый об'ект Var с предупреждением *)
    (*  Warning(1); *)
    AllocVar(I);  LoadVar(ob);
  ELSE
    IF GetType(idno) = Char THEN
      IF lookAhead(':') THEN
        (* Warning(1);  *)
        AllocVar(I);  LoadVar(ob);
        RETURN
      END;
    END;
    IF md=Empty THEN
      no:=LookIntr(Ident);
      IF no#0 THEN
        I.offset:=no; AllocIntr(I);
      ELSE
        AllocExt(I);
      END;
    ELSE
        AllocExt(I);
    END;
    Function(ob);
  END;
END Fused;

PROCEDURE Simple(VAR ob:Dexpr);
BEGIN
  CASE sy OF
   lpar     : IF CxConst() THEN ob.tp:=cType; ldConst(ob);
              ELSE
                GetSy; Lexpr(ob);
                IF sy#rpar THEN Expected(rpar); END;
                GetSy;
              END;
  |id       : ob.tp:=GetType(idno);
              CASE GetMode(idno) OF
                Proc   : Function(ob);
               |xVar   : Fused(ob,xVar);
               | Var   : LoadVar(ob);
               |Const  : CONSTANT(ob);
               |Array  : arrayelem(ob);
               |Empty  : Fused(ob,Empty);
               |LocVar : LoadLocVar(ob);
              END;
  |const    :ob.tp:=cType;  ldConst(ob);
  ELSE Error(14);
  END;
END Simple;

PROCEDURE Factor(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
   Simple(ob);
  WHILE sy=power DO
    op:=sy; ob1.start:=cp; GetSy;
    Factor(ob1); Apply(ob,ob1,op);
  END;
END Factor;

PROCEDURE Term(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
  Factor(ob);
  WHILE (sy=slash) OR (sy=times) DO
    op:=sy; ob1.start:=cp; GetSy;
    Factor(ob1); Apply(ob,ob1,op);
  END;
END Term;

PROCEDURE Aexpr(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
  IF sy=minus THEN op:=sy; GetSy;
  ELSIF sy=plus THEN op:=sy; GetSy;
  ELSE op:=plus
  END;
  Term(ob);
  IF op=minus THEN
    IF ob.emd=cexpr THEN
      CASE ob.tp OF
       Int     : ob.name:=-ob.name;
      |Real    : ob.name:= WORD(-REAL(ob.name));
      |Double  : ob.name:= WORD(-REAL(ob.name));     -- cDpNeg(ob);
      |Complex : ob.name:= WORD(-REAL(ob.name));
                   ob.wd:= WORD(-REAL(ob.wd));
      ELSE Error(12); ob.emd:=invmd;
      END;
    ELSIF ORD(ob.emd) >= ORD(arr) THEN Error(15)
    ELSE ob.emd:=expr;
      CASE ob.tp OF
       Int     : c(NEG);
      |Real    : c(FNEG);
      |Double  : c(FNEG);            -- DpNeg;
      |Complex : c(FNEG); c(swap);
                 c(FNEG); c(swap);
      ELSE Error(12); ob.emd:=invmd;
      END;
    END;
  END;
  WHILE (sy=plus) OR (sy=minus) DO
   ob1.start:=cp; op:=sy; GetSy; Term(ob1);
    Apply(ob,ob1,op);
  END;
END Aexpr;

PROCEDURE CAexpr(VAR ob:Dexpr);
VAR ob1:Dexpr; D,D1:INTEGER;
BEGIN
 Aexpr(ob);
 IF sy=cat THEN
   IF ob.tp#Char THEN Error(13);
   ELSE
     IF ob.emd=cexpr THEN LoadConst(ob); genChMove(ob);
     ELSIF ob.emd#expr  THEN genChMove(ob);
     END;
     D:=INTEGER(ob.wd);
   END;
   WHILE sy=cat DO
     ob1.start:=cp; GetSy;
     Aexpr(ob1);
     IF ob1.tp#Char THEN Error(13);
     ELSE
       IF ob1.emd=cexpr THEN LoadConst(ob1); genChMove(ob1);
       ELSIF ob1.emd#expr  THEN genChMove(ob1);
       END;
       D1:=INTEGER(ob1.wd);
       llw(D+2); llw(D1+2); c(ADD); epop;
       slw(D+2); ob.emd:=expr;
       FreeDesc(D1); c(Drop); epop;
     END;
   END; -- while
 END;  -- if
END CAexpr;

PROCEDURE Lsimple(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
  CAexpr(ob);
  IF (ORD(sy)<ORD(and)) AND (ORD(sy)>ORD(minus)) THEN
    op:=sy; GetSy; ob1.start:=cp; CAexpr(ob1);
    Apply(ob,ob1,op);
  END;
END Lsimple;

PROCEDURE Lfactor(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
  IF sy=not THEN GetSy; Lsimple(ob);
    IF ob.tp#Logic THEN Error(13); ob.emd:=invmd; END;
    IF ob.emd=cexpr THEN
      IF BOOLEAN(ob.name) THEN ob.name:=INTEGER(FALSE)
      ELSE ob.name:=INTEGER(TRUE)
      END;
    ELSE
      c(Not);
    END;
  ELSE Lsimple(ob);
  END;
END Lfactor;

PROCEDURE Lterm(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
 Lfactor(ob);
  WHILE sy=and DO   cAndJump;
    op:=sy; ob1.start:=cp; GetSy;
    Lfactor(ob1); Apply(ob,ob1,op);
  END;
END Lterm;

PROCEDURE Lsum(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
 Lterm(ob);
  WHILE sy=or DO  cOrJump;
    op:=sy; ob1.start:=cp; GetSy;
    Lterm(ob1); Apply(ob,ob1,op);
  END;
END Lsum;

PROCEDURE Lexpr(VAR ob:Dexpr);
VAR ob1:Dexpr; op: Symbol;
BEGIN
   Lsum(ob);
   WHILE (sy=eqv) OR (sy=neqv) DO
     op:=sy; ob1.start:=cp; GetSy;
     Lsum(ob1); Apply(ob,ob1,op);
   END;
END Lexpr;

PROCEDURE Expr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ORD(ob.emd) >= ORD(arr) THEN Error(15) END;
END Expr;

PROCEDURE lExpr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ob.tp#Logic THEN Error(7)
  ELSIF ORD(ob.emd) >= ORD(arr) THEN Error(15)
  END;
END lExpr;

PROCEDURE tExpr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ob.tp#Char THEN Error(30)
  ELSIF ORD(ob.emd) >= ORD(arr) THEN Error(15)
  END;
END tExpr;

PROCEDURE aExpr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ORD(ob.tp) >= ORD(Logic) THEN Error(31)
  ELSIF ORD(ob.emd) >= ORD(arr) THEN Error(15)
  END;
END aExpr;

PROCEDURE cExpr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ob.emd#cexpr THEN Error(24)
  END;
END cExpr;

PROCEDURE icExpr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ob.emd#cexpr THEN Error(24)
  ELSIF ob.tp # Int THEN Error(27)
  END;
END icExpr;

PROCEDURE iExpr(VAR ob:Dexpr);
BEGIN
  ob.start:=cp; Lexpr(ob);
  IF ob.tp#Int THEN Error(27)
  ELSIF ORD(ob.emd) >= ORD(arr) THEN Error(15)
  END;
END iExpr;

BEGIN
END fcExpr.
