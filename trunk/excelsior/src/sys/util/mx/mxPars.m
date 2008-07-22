IMPLEMENTATION MODULE mxPars; (* Ned 10-Nov-87. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT  comp: coolDefs;
IMPORT inter: coolSystem;
IMPORT  scan: mxScan;
IMPORT   obs: mxObj;
IMPORT   sym: mxSym;
IMPORT   gen: mxGen;

FROM mxScan IMPORT  sy, GetSy;

WITH STORAGE: inter;

CONST
  _origin = -10;    _dcopy = -11;   _ash = 16;
  _short_long = 17;

VAR  CU: obs.ObjPtr;  -- compilation unit
  modNo: INTEGER;
   cpu0: INTEGER;

---------------------------------------------------------------

PROCEDURE CheckGet(S: INTEGER);
BEGIN
  IF sy#S THEN scan.expc(S) END; GetSy
END CheckGet;

PROCEDURE SkipTo(SEQ s: INTEGER);
  VAR i: INTEGER;
BEGIN
  LOOP
    IF sy=scan.end THEN RETURN END;
    i:=0;
    WHILE (i<=HIGH(s)) & (s[i]#sy) DO INC(i) END;
    IF i<=HIGH(s) THEN RETURN END;
    GetSy;
  END;
END SkipTo;

-------------------------  DESIGNATOR  ------------------------
                         --------------

VAR withs: obs.ObjPtr;

PROCEDURE Expr(VAR o: gen.ObjPtr); FORWARD;

PROCEDURE qualident(VAR v: obs.ObjPtr);
BEGIN
  IF sy#scan.ident THEN scan.expc(scan.ident); v:=obs.Ilg; RETURN END;
  v:=obs.Vis(scan.Id); GetSy;
  WHILE (sy=scan.period) & (v^.mode=obs.modul) DO
    GetSy;
    IF sy#scan.ident THEN scan.expc(scan.ident); RETURN END;
    v:=obs.VisInScope(v^.head,scan.Id); GetSy;
  END;
END qualident;

PROCEDURE vis_in_with?(VAR v: obs.ObjPtr): BOOLEAN;
  VAR l: obs.ObjPtr;
BEGIN l:=withs;
  WHILE l#NIL DO
    IF (l^.type^.mode=obs.rec) & obs.VisInScope?(l^.type^.head,scan.Id,v) THEN
      v:=l; RETURN TRUE
    END;
    l:=l^.next;
  END; RETURN FALSE
END vis_in_with?;

PROCEDURE designator(VAR o: gen.ObjPtr);
  VAR ex: gen.ObjPtr; v: obs.ObjPtr;
BEGIN
  IF sy#scan.ident THEN
    scan.err(27); Expr(o); gen.genObj(o,obs.Ilg); RETURN
  END;
  IF (withs#NIL) & vis_in_with?(v) THEN
    gen.genObj(o,v); gen.access(o,scan.Id); GetSy;
  ELSE qualident(v); gen.genObj(o,v)
  END;
  LOOP
    IF    sy=scan.bar    THEN gen.deref(o); GetSy
    ELSIF sy=scan.period THEN GetSy;
      IF sy#scan.ident   THEN scan.expc(scan.ident); RETURN END;
      gen.access(o,scan.Id);
      GetSy;
    ELSIF sy=scan.lbr THEN GetSy;
      gen.index0(o); Expr(ex); gen.index1(o,ex);
      IF sy=scan.coma THEN sy:=scan.lbr (* "," ==  "][" *)
      ELSE CheckGet(scan.rbr)
      END
    ELSE RETURN
    END;
  END;
END designator;

--------------------  STANDARD PROC CALLs  --------------------
                    -----------------------

PROCEDURE sProcCall(no: INTEGER; o: gen.ObjPtr);
BEGIN
  gen.enterStandardCall(o);
  IF no=gen._halt THEN
    IF sy=scan.lpar THEN GetSy;
      IF sy#scan.rpar THEN Expr(o); gen.standard_param(o) END;
      CheckGet(scan.rpar);
    END;
  ELSE CheckGet(scan.lpar);
    CASE no OF
      |gen._inc,gen._dec,gen._new:
         designator(o); gen.standard_param(o);
         IF sy=scan.coma THEN GetSy; Expr(o); gen.standard_param(o) END;
      |gen._incl,gen._excl,gen._resize:
         designator(o); gen.standard_param(o);
         CheckGet(scan.coma); Expr(o); gen.standard_param(o);
      |gen._assert:
         Expr(o); gen.standard_param(o);
         IF sy=scan.coma THEN GetSy; Expr(o); gen.standard_param(o) END;
      |gen._dispose:
         designator(o); gen.standard_param(o);
      |_origin:
         designator(o); gen.standard_param(o);
         CheckGet(scan.coma); Expr(o); gen.standard_param(o);
         CheckGet(scan.coma); Expr(o); gen.standard_param(o);
      |_dcopy:
         Expr(o); gen.standard_param(o); CheckGet(scan.coma);
         designator(o); gen.standard_param(o);
    ELSE ASSERT(FALSE);
    END;
    CheckGet(scan.rpar);
  END;
  gen.exitStandardCall(o);
END sProcCall;

PROCEDURE sFuncCall(no: INTEGER; VAR o: gen.ObjPtr);
  CONST
    quals = {gen._min,gen._max};
    desis = {gen._adr,gen._size,gen._bytes,gen._bits,gen._high,gen._len};
    exprs = {gen._odd,gen._ord,gen._abs,gen._cap,gen._chr
            ,gen._float,gen._trunc};

  VAR p: gen.ObjPtr; v: obs.ObjPtr;
BEGIN
  CheckGet(scan.lpar);
  IF no=_short_long THEN Expr(o);
  ELSE
    gen.enterStandardCall(o);
    IF    no IN exprs THEN Expr(p);       gen.standard_param(p);
    ELSIF no IN desis THEN designator(p); gen.standard_param(p);
    ELSIF no IN quals THEN qualident(v);
      gen.genObj(p,v); gen.standard_param(p);
    ELSIF no=gen._ref THEN
      designator(p);   gen.standard_param(p); CheckGet(scan.coma);
      qualident(v);
      gen.genObj(p,v); gen.standard_param(p);
    ELSIF no=_ash THEN
      Expr(p);   gen.standard_param(p); CheckGet(scan.coma);
      Expr(p);   gen.standard_param(p);
    ELSE ASSERT(FALSE);
    END;
    gen.exitStandardCall(o);
  END;
  CheckGet(scan.rpar);
END sFuncCall;

-------------------------  PROC CALLs  ------------------------
                         --------------
PROCEDURE parmlist(parm: obs.ParmPtr);

  PROCEDURE sequence(parm: obs.ParmPtr);
    VAR o: gen.ObjPtr;
  BEGIN
    gen.enter_seq;
    LOOP
      IF sy=scan.rpar THEN EXIT END;
      IF obs.varpar IN parm^.kind THEN designator(o) ELSE Expr(o) END;
      gen.seq_param(o,parm);
      IF sy=scan.coma THEN GetSy ELSE EXIT END;
    END;
    gen.end_seq;
  END sequence;

  PROCEDURE empty_list;
  BEGIN
    IF parm=NIL THEN RETURN END;
    IF obs.seqpar IN parm^.kind THEN gen.enter_seq; gen.end_seq
    ELSE scan.err(49)
    END;
  END empty_list;

  VAR ok: BOOLEAN; o: gen.ObjPtr;
BEGIN
  IF sy#scan.lpar THEN empty_list; RETURN
  ELSE GetSy;
    IF sy=scan.rpar THEN empty_list; GetSy; RETURN END;
  END;
  ok:=TRUE;
  LOOP
    IF ok & (parm=NIL) THEN scan.err(49); ok:=FALSE END;
    IF NOT ok THEN Expr(o);
    ELSIF obs.seqpar IN parm^.kind THEN
      sequence(parm); CheckGet(scan.rpar); RETURN
    ELSE
      IF obs.varpar IN parm^.kind THEN designator(o) ELSE Expr(o) END;
      gen.param(o,parm);
    END;
    IF ok THEN parm:=parm^.next END;
    IF sy=scan.coma THEN GetSy ELSE EXIT END;
  END;
  IF ok THEN empty_list END;
  CheckGet(scan.rpar);
END parmlist;

PROCEDURE skip_parms;
  VAR o: gen.ObjPtr;
BEGIN
  IF sy#scan.lpar THEN RETURN END;
  GetSy;
  LOOP
    IF sy=scan.rpar THEN EXIT END;
    Expr(o);
    IF sy=scan.coma THEN GetSy ELSE EXIT END;
  END;
  GetSy;
END skip_parms;

PROCEDURE ProcCall(o: gen.ObjPtr);
  VAR t: obs.TypePtr; no: INTEGER;
BEGIN
  IF gen.standard_proc(o,no) THEN
    IF no<0 THEN sProcCall(no,o) ELSE scan.err(46); sFuncCall(no,o) END;
    RETURN
  END;
  gen.get_type(o,t);
  IF t^.mode#obs.proctype THEN
    IF    t^.mode=obs.functype THEN scan.err(46);
    ELSIF t^.mode#obs.invtype  THEN scan.err(35);
    END;
    skip_parms; RETURN
  END;
  gen.enterProcCall(o);
  parmlist(t^.plist);
  gen.exitProcCall(o);
END ProcCall;

PROCEDURE FuncCall(VAR o: gen.ObjPtr);
  VAR t: obs.TypePtr; no: INTEGER;
BEGIN
  IF gen.standard_proc(o,no) THEN
    IF no>=0 THEN sFuncCall(no,o);
    ELSE scan.err(39); sProcCall(no,o); gen.genObj(o,obs.Ilg);
    END; RETURN
  END;
  gen.get_type(o,t);
  IF t^.mode#obs.functype THEN
    IF    t^.mode=obs.proctype THEN scan.err(39);
    ELSIF t^.mode#obs.invtype  THEN scan.err(50);
    END;
    skip_parms; RETURN
  END;
  gen.enterProcCall(o);
  parmlist(t^.plist);
  gen.exitProcCall(o);
END FuncCall;

------------------------  EXPRESSIONs  ------------------------
                        ---------------

PROCEDURE Factor(VAR o: gen.ObjPtr);

  PROCEDURE set;
    VAR e1,e2: gen.ObjPtr;
  BEGIN
    gen.enterset(o);
    GetSy;
    IF sy=scan.rbrace THEN
      GetSy; gen.exitset(o); RETURN
    END;
    LOOP
      Expr(e1);
      IF sy=scan.range THEN GetSy; Expr(e2);
           gen.setrange(o,e1,e2);
      ELSE gen.setelem (o,e1);
      END;
      IF sy=scan.rbrace THEN GetSy; EXIT END;
      IF sy=scan.coma   THEN GetSy
      ELSE scan.expc(scan.rbrace); EXIT
      END
    END;
    gen.exitset(o);
  END set;

  PROCEDURE open_array(base: obs.TypePtr);
    VAR type: obs.TypePtr; expr: gen.ObjPtr; n: INTEGER;
  BEGIN
    IF sy#scan.lbrace THEN
      scan.expc(scan.lbrace); gen.genObj(o,obs.Ilg); RETURN
    END; GetSy;
    gen.create_open_array(o,base);
    n:=0;
    LOOP
      Expr(expr); INC(n);
      gen.app_elem(o,expr);
      IF sy=scan.rbrace THEN GetSy; EXIT END;
      IF sy=scan.coma   THEN GetSy
      ELSE scan.expc(scan.rbrace); EXIT
      END;
    END;
    type:=obs.MakeRange(obs.intp,0,n-1);
    type:=obs.MakeArr(type,base);
    gen.close_open_array(o,type);
  END open_array;

  PROCEDURE string;
    VAR t: obs.TypePtr;
  BEGIN
    t:=obs.MakeRange(obs.intp,0,scan.sLen-1);
    t:=obs.MakeArr(t,obs.charp);
    gen.gen_string(o,t,scan.sVal,scan.sLen);
  END string;

  PROCEDURE iden;
    VAR ex: gen.ObjPtr;
  BEGIN designator(o);
    IF sy=scan.lpar THEN
      IF gen.type?(o) THEN
        GetSy; Expr(ex); CheckGet(scan.rpar);
        gen.typetransfer(o,ex);
      ELSE FuncCall(o)
      END;
    ELSIF sy=scan.lbrace THEN set;
    ELSE gen.load_value(o);
    END;
  END iden;

  VAR v: obs.ObjPtr;
BEGIN
  CASE sy OF
    |scan.intval : gen.gen_const(o,obs.intp ,scan.cVal); GetSy;
    |scan.charval: gen.gen_const(o,obs.charp,scan.cVal); GetSy;
    |scan.realval: gen.gen_const(o,obs.realp,scan.cVal); GetSy;
    |scan.string : string;  GetSy;
    |scan.lpar   : GetSy; Expr(o); CheckGet(scan.rpar);
    |scan.ident  : iden;
    |scan.lbrace : gen.genObj(o,obs.bitsetp^.obj); set
    |scan.not    : GetSy; Factor(o); gen.applyNOT(o);
    |scan.array  : GetSy; CheckGet(scan.of);
                   qualident(v); obs.chkType(v); open_array(v^.type);
  ELSE scan.err(26); SkipTo(scan.rpar,scan.semic);
    gen.genObj(o,obs.Ilg);
  END;
END Factor;

TYPE subExpr = PROCEDURE (VAR gen.ObjPtr);

PROCEDURE Cond(VAR o: gen.ObjPtr; andor: INTEGER; E: subExpr);
  VAR o1: gen.ObjPtr;
BEGIN
  gen.entercond(o,andor=scan.and);
  GetSy;
  E(o1);
  IF sy=andor THEN Cond(o1,andor,E) END;
  o:=gen.exitcond(o1);
END Cond;

PROCEDURE Term(VAR o: gen.ObjPtr);
  VAR o1: gen.ObjPtr; op: INTEGER;
BEGIN Factor(o);
  LOOP
    op:=sy;
    IF op IN scan.Multiplop THEN
      GetSy; Factor(o1); gen.apply(o,o1,op)
    ELSIF op=scan.and THEN Cond(o,scan.and,Factor)
    ELSE RETURN
    END
  END;
END Term;

PROCEDURE Simple(VAR o: gen.ObjPtr);
  VAR o1: gen.ObjPtr; op: INTEGER;
BEGIN
  IF    sy=scan.plus  THEN GetSy; Term(o);
  ELSIF sy=scan.minus THEN GetSy; Term(o); gen.applyNEG(o);
  ELSE  Term(o)
  END;
  LOOP
    IF sy=scan.or THEN Cond(o,scan.or,Term)
    ELSE op:=sy;
      IF (op=scan.plus) OR (op=scan.minus) THEN
        GetSy; Term(o1); gen.apply(o,o1,op)
      ELSE RETURN
      END
    END;
  END
END Simple;

PROCEDURE Expr(VAR o: gen.ObjPtr);
  VAR o1: gen.ObjPtr; op: INTEGER;
BEGIN Simple(o);
  LOOP
    IF sy IN scan.Relation THEN
      op:=sy; GetSy; Simple(o1); gen.apply(o,o1,op)
    ELSE RETURN
    END
  END;
END Expr;

PROCEDURE ConstExpr(VAR val: INTEGER; VAR t: obs.TypePtr);
  VAR o: gen.ObjPtr;
BEGIN Expr(o); gen.constexpr(o,val,t);
END ConstExpr;

-------------------------  STATMENTs  -------------------------
                         -------------

PROCEDURE StatSeq;

  PROCEDURE ident;
    VAR o,o1: gen.ObjPtr;
  BEGIN designator(o);
    IF sy=scan.becomes THEN GetSy;
      gen.assign0(o); Expr(o1); gen.assign1(o,o1);
    ELSE ProcCall(o)
    END;
  END ident;

  PROCEDURE if;
    VAR o: gen.ObjPtr;
  BEGIN
    Expr(o); gen.if(o); CheckGet(scan.then);
    StatSeq;
    IF    sy=scan.else  THEN gen.else; GetSy; StatSeq;
    ELSIF sy=scan.elsif THEN gen.else; GetSy; if;
    END;
    gen.endif;
  END if;

  PROCEDURE with;
    VAR v: obs.ObjPtr; o: gen.ObjPtr;
  BEGIN GetSy;
    designator(o);
    obs.NewObj(v,obs.vari); v^.next:=withs; withs:=v;
    gen.enterwith(o,v);
    CheckGet(scan.do); StatSeq; CheckGet(scan.end);
    gen.endwith(v);
    ASSERT(v=withs);
    withs:=withs^.next;
    DISPOSE(v);
  END with;

  PROCEDURE return;
    VAR o: gen.ObjPtr;
  BEGIN GetSy;
    IF obs.CurBlock^.mode#obs.proc THEN scan.err(53); RETURN END;
    IF obs.CurBlock^.type^.mode#obs.functype THEN gen.return;
    ELSE Expr(o); gen.freturn(o,obs.CurBlock^.type^.base)
    END;
  END return;

  PROCEDURE for;
    VAR o: gen.ObjPtr; v,t: obs.ObjPtr; id: INTEGER;
  BEGIN
    GetSy; id:=scan.Id;
    IF sy#scan.ident THEN scan.expc(scan.ident); id:=scan.DmId
    ELSIF vis_in_with?(v) THEN scan.err(27)
    END;
    v:=obs.Vis(id);
    IF v^.mode#obs.vari THEN
      IF v^.mode#obs.inv THEN scan.err(27) END;
    ELSE
      IF obs.RO     IN v^.tags THEN scan.err(76) END;
      IF obs.varpar IN v^.tags THEN scan.err(75) END;
    END;
    INCL(v^.tags,obs.RO);
    gen.enterfor(v);  GetSy;  CheckGet(scan.becomes);  Expr(o);
    gen.for_from(o);          CheckGet(scan.to);       Expr(o);
    gen.for_to(o);
    IF sy=scan.by THEN GetSy; Expr(o); gen.for_step(o) END;
    gen.for_do; CheckGet(scan.do);
    StatSeq;
    gen.endfor;
    CheckGet(scan.end);
    EXCL(v^.tags,obs.RO);
  END for;

  PROCEDURE case;
    VAR o,o1: gen.ObjPtr;

    PROCEDURE Variant;
    BEGIN
      LOOP
        Expr(o);
        IF sy=scan.range THEN GetSy; Expr(o1); gen.caserange(o,o1);
        ELSE gen.caselabel(o)
        END;
        IF sy=scan.coma THEN GetSy
        ELSE
          IF sy#scan.colon THEN scan.expc(scan.colon) END; EXIT
        END;
      END; GetSy;
      StatSeq;
      IF (sy#scan.else) & (sy#scan.end) THEN CheckGet(scan.sep) END;
      gen.exitvariant;
    END Variant;

  BEGIN GetSy;
    Expr(o); CheckGet(scan.of); gen.entercase(o);
    WHILE (sy#scan.end) & (sy#scan.else) DO
      IF sy=scan.sep THEN GetSy ELSE Variant END;
    END;
    IF sy=scan.else THEN GetSy; gen.elsecase; StatSeq END;
    CheckGet(scan.end); gen.endcase;
  END case;

  VAR o: gen.ObjPtr;
BEGIN
  LOOP
    IF sy IN scan.StatTerm THEN RETURN END;
    CASE sy OF
     |scan.ident : ident;
     |scan.if    : GetSy; if; CheckGet(scan.end);
     |scan.while : gen.enterwhile; GetSy; Expr(o);
                   gen.do(o);      CheckGet(scan.do); StatSeq;
                   gen.endwhile;   CheckGet(scan.end);
     |scan.repeat: gen.repeat; GetSy; StatSeq;
                   CheckGet(scan.until); Expr(o);
                   gen.until(o);
     |scan.loop  : gen.enterloop; GetSy; StatSeq;
                   gen.endloop;   CheckGet(scan.end);
     |scan.exit  : GetSy; gen.exitloop;
     |scan.return: return;
     |scan.for   : for
     |scan.with  : with
     |scan.case  : case
     |scan.rol   : GetSy;
                   IF sy=scan.ident THEN gen.mark(scan.Id); GetSy
                   ELSE scan.expc(scan.ident)
                   END;
     |scan.ror   : GetSy;
                   IF sy=scan.ident THEN gen.goto(scan.Id); GetSy
                   ELSE scan.expc(scan.ident)
                   END;
     |scan.semic :
    ELSE scan.err(35);
      SkipTo(scan.semic,scan.else,scan.elsif,scan.until,scan.sep);
    END;
    IF sy IN scan.StatTerm THEN RETURN ELSE CheckGet(scan.semic) END;
  END
END StatSeq;

---------------------------  TYPEs  ---------------------------
                           ---------

VAR FWD: inter.QUEUE; -- очередь указателей вперед

PROCEDURE checkFWD;
  VAR t: obs.TypePtr; o: obs.ObjPtr;
BEGIN
  WHILE inter.pop(FWD,t) DO
    IF t^.mode=obs.ptr THEN ASSERT(t^.tid>=0);
      o:=obs.Vis(t^.tid); obs.chkType(o);
      t^.base:=o^.type; t^.tid:=-1;
    ELSE ASSERT(t^.mode=obs.invtype);
    END;
  END;
  inter.clear(FWD);
END checkFWD;

PROCEDURE IdList(VAR q: inter.QUEUE; term: INTEGER);
BEGIN
  IF sy=term THEN
    scan.expc(scan.ident); GetSy; RETURN
  END;
  LOOP
    IF sy=scan.ident THEN inter.push(q,scan.Id) END;
    CheckGet(scan.ident);
    IF    sy=scan.coma  THEN GetSy
    ELSIF sy=term         THEN EXIT
    ELSIF sy=scan.ident THEN scan.expc(scan.coma)
    ELSE EXIT
    END
  END;
  IF sy#term THEN scan.expc(term) END;
END IdList;

PROCEDURE FormalType(): obs.TypePtr;
  VAR o: obs.ObjPtr; f: BOOLEAN;
BEGIN
  f:=(sy=scan.array);
  IF f THEN GetSy; CheckGet(scan.of) END;
  qualident(o); obs.chkType(o);
  IF f THEN RETURN obs.MakeFlx(o^.type) ELSE RETURN o^.type END;
END FormalType;

PROCEDURE function?(proc: obs.TypePtr);
  VAR o: obs.ObjPtr;
BEGIN
  IF sy=scan.colon THEN GetSy;
    qualident(o); obs.chkType(o); obs.chkSimple(o^.type);
    obs.FuncType(proc,o^.type);
  ELSE obs.ProcType(proc);
  END;
END function?;

PROCEDURE TypeConstr(): obs.TypePtr;

  PROCEDURE Range(): obs.TypePtr;
    VAR t1,t2: obs.TypePtr; c1,c2: INTEGER;
  BEGIN
    ConstExpr(c1,t1); CheckGet(scan.range); ConstExpr(c2,t2);
    obs.TypeCmp(t1,t2); CheckGet(scan.rbr);
    RETURN obs.MakeRange(t1,c1,c2);
  END Range;

  PROCEDURE Enum(): obs.TypePtr;
    VAR t: obs.TypePtr; val: INTEGER;
  BEGIN
    t:=obs.MakeEnum(); val:=0;
    IF sy=scan.rpar THEN scan.expc(scan.ident); GetSy; RETURN t END;
    LOOP
      IF sy=scan.ident THEN obs.Dcl(scan.Id,obs.AppEnum(t,val)); INC(val) END;
      CheckGet(scan.ident);
      IF    sy=scan.coma  THEN GetSy
      ELSIF sy=scan.rpar  THEN EXIT
      ELSIF sy=scan.ident THEN scan.expc(scan.coma)
      ELSE EXIT END
    END; CheckGet(scan.rpar); RETURN t
  END Enum;

  PROCEDURE Array(): obs.TypePtr;
    VAR t: obs.TypePtr;
  BEGIN
    IF sy=scan.of THEN
      scan.err(38); GetSy; t:=TypeConstr(); RETURN obs.Any
    END;
    t:=TypeConstr();
    IF sy=scan.coma THEN sy:=scan.array ELSE CheckGet(scan.of) END;
    RETURN obs.MakeArr(t,TypeConstr())
  END Array;

  PROCEDURE Pointer(): obs.TypePtr;
    VAR t: obs.TypePtr; o: obs.ObjPtr;
  BEGIN
    CheckGet(scan.to);
    IF sy#scan.ident THEN RETURN obs.MakePtr(TypeConstr()) END;
    IF obs.Vis?(scan.Id,o) THEN qualident(o); obs.chkType(o); t:=o^.type;
      IF o^.type^.mode=obs.farr THEN scan.err(38); t:=obs.Any END;
      RETURN obs.MakePtr(t)
    ELSE t:=obs.MakeFwdPtr(scan.Id); GetSy; inter.push(FWD,t); RETURN t
    END
  END Pointer;

  PROCEDURE Record(): obs.TypePtr;

    VAR r: obs.TypePtr; ofs: INTEGER;

    PROCEDURE FieldList; FORWARD;

    PROCEDURE SimpleFields;
      VAR t: obs.TypePtr; id,sz: INTEGER; que: inter.QUEUE;
    BEGIN
      inter.fifo(que);
      IdList(que,scan.colon); GetSy;
      t:=TypeConstr(); sz:=obs.tsize(t);
      WHILE inter.pop(que,id) DO
        obs.AppField(r,id,t,ofs); INC(ofs,sz)
      END;
      inter.clear(que);
    END SimpleFields;

    PROCEDURE VariantPart;

      VAR labTyp: obs.TypePtr;

      PROCEDURE Variant;
        VAR t: obs.TypePtr; c: INTEGER;
      BEGIN
        IF sy=scan.else THEN GetSy
        ELSE
          WHILE sy#scan.colon DO
            ConstExpr(c,t); obs.TypeCmp(t,labTyp);
            IF sy=scan.range THEN GetSy;
              ConstExpr(c,t); obs.TypeCmp(t,labTyp)
            END;
            IF sy#scan.colon THEN CheckGet(scan.coma) END
          END; GetSy
        END;
        FieldList;
        IF NOT (sy IN {scan.else,scan.end}) THEN CheckGet(scan.sep) END;
      END Variant;

      VAR lOfs,max: INTEGER; id: INTEGER; o: obs.ObjPtr;
    BEGIN
      CheckGet(scan.case);
      IF sy=scan.colon THEN (* unnamed tag *) id:=scan.DmId
      ELSIF sy#scan.ident THEN
        scan.expc(scan.ident); SkipTo(scan.end); RETURN
      ELSE id:=scan.Id; GetSy;
      END; CheckGet(scan.colon);
      qualident(o); obs.chkType(o);
      labTyp:=o^.type; obs.chkScalar(labTyp);
      IF id#scan.DmId THEN
        obs.AppField(r,id,labTyp,ofs); INC(ofs(*,tsize(labTyp)=1*));
      END;
      lOfs:=ofs; max:=ofs; CheckGet(scan.of);
      WHILE sy#scan.end DO
        IF sy=scan.sep THEN GetSy
        ELSE Variant;
          IF ofs>max THEN max:=ofs END; ofs:=lOfs;
        END;
      END;
      ofs:=max; GetSy;
    END VariantPart;

    PROCEDURE FieldList;
    BEGIN
      LOOP
        IF    sy=scan.ident THEN SimpleFields
        ELSIF sy=scan.case  THEN VariantPart
        ELSIF sy IN {scan.end,scan.sep,scan.else} THEN EXIT
        ELSIF sy#scan.semic THEN
          scan.expc(scan.end); SkipTo(scan.end); GetSy; EXIT
        END;
        IF NOT (sy IN {scan.end,scan.sep,scan.else}) THEN
          CheckGet(scan.semic)
        END;
      END;
    END FieldList;

  BEGIN
    r:=obs.MakeRec(); ofs:=0;
    FieldList;
    CheckGet(scan.end);
    RETURN r
  END Record;

  PROCEDURE procType(): obs.TypePtr;
    VAR proc,parm: obs.TypePtr; kind: BITSET; o: obs.ObjPtr; p: obs.ParmPtr;
  BEGIN
    proc:=obs.MakeProcType();
    IF sy#scan.lpar THEN obs.ProcType(proc); RETURN proc END;
    GetSy;
    LOOP
      kind:={};
      IF    sy=scan.var THEN GetSy; INCL(kind,obs.varpar);
      ELSIF sy=scan.val THEN GetSy; scan.err(70);
      ELSIF (sy#scan.ident) & (sy#scan.array) THEN EXIT
      END;
      parm:=FormalType();
      p:=obs.AppParm(proc,scan.DmId,parm,kind);
      IF sy#scan.rpar THEN CheckGet(scan.coma) END
    END;
    IF sy=scan.seq THEN GetSy; kind:={obs.seqpar,obs.RO};
      IF sy=scan.var THEN GetSy; kind:=kind/{obs.varpar,obs.RO} END;
      qualident(o); obs.chkType(o);
      p:=obs.AppParm(proc,scan.DmId,obs.MakeFlx(o^.type),kind);
    END;
    CheckGet(scan.rpar);
    function?(proc);
    gen.scanParms(proc^.plist);
    RETURN proc
  END procType;

  VAR o: obs.ObjPtr; lsy: INTEGER;
BEGIN
  IF sy=scan.ident THEN qualident(o); obs.chkType(o); RETURN o^.type END;
  lsy:=sy; GetSy;
  CASE lsy OF
   |scan.lbr      : RETURN Range()
   |scan.lpar     : RETURN Enum()
   |scan.set      : CheckGet(scan.of); RETURN obs.MakeSet(TypeConstr())
   |scan.procedure: RETURN procType()
   |scan.record   : RETURN Record();
   |scan.array    : RETURN Array()
   |scan.dynarr   : CheckGet(scan.of); RETURN obs.MakeDynArr(TypeConstr(),1)
   |scan.pointer  : RETURN Pointer()
  ELSE scan.err(19)
  END; RETURN obs.Any
END TypeConstr;

---------------------------  IMPORT  --------------------------
                           ----------

TYPE visout = PROCEDURE (INTEGER): obs.ObjPtr;

PROCEDURE importlist(outside: visout);
  VAR id: INTEGER; o : obs.ObjPtr; from?: BOOLEAN;
BEGIN
  WHILE (sy=scan.import) OR (sy=scan.from) DO
    from?:=(sy=scan.from);
    IF from? THEN GetSy;
      IF sy#scan.ident THEN scan.expc(scan.ident); o:=obs.Ilg;
      ELSE o:=outside(scan.Id); GetSy;
        IF o^.mode#obs.modul THEN scan.err_id(42,o^.id); o:=obs.Ilg END;
      END;
    END;
    CheckGet(scan.import);
    LOOP
      IF sy=scan.ident THEN id:=scan.Id; GetSy;
        IF   from? THEN obs.FromImport(o,id)
        ELSIF (sy=scan.colon) OR (sy=scan.becomes) THEN
          GetSy;
          IF sy=scan.ident THEN obs.Import(outside(scan.Id),id) END;
          CheckGet(scan.ident);
        ELSE obs.Import(outside(id),id);
        END;
      ELSE CheckGet(scan.ident);
      END;
      IF    sy=scan.coma  THEN GetSy
      ELSIF sy=scan.semic THEN EXIT
      ELSIF sy=scan.ident THEN scan.expc(scan.coma)
      ELSE EXIT
      END
    END;
    CheckGet(scan.semic);
  END;
END importlist;

PROCEDURE conflict(out,in: obs.ObjPtr);
  VAR n: INTEGER;
BEGIN
  IF (out^.mode=obs.proc) & (obs.forward IN out^.tags)
   & (in ^.mode=obs.proc) & NOT (obs.duplicate IN in^.tags)
  THEN
    n:=obs.ProcCmp?(in^.type,out^.type);
    IF n#0 THEN scan.err_id(n,out^.id) END;
    EXCL(out^.tags,obs.forward);
    gen.ProcEqu(in,out);
  ELSIF (in^.mode=obs.typ) & (out^.mode=obs.typ)
                           & (out^.type^.mode=obs.hidden) THEN
    IF obs.CurBlock#CU THEN scan.err(68); RETURN END;
    obs.set_hidden(out^.type,in^.type)
  ELSE scan.err_id(47,in^.id)
  END;
END conflict;

----------------------------  USE  ----------------------------
                            -------

PROCEDURE IniSTORAGE;

  PROCEDURE new(no: INTEGER; t: obs.TypePtr; VAL s: ARRAY OF CHAR);
    VAR o: obs.ObjPtr;
  BEGIN
    obs.NewObj(o,obs.std_proc); o^.no:=no;
    o^.type:=t; o^.tags:={obs.penetrate,obs.forward};
    obs.DclInScope(obs.SuperProc^.head,scan.str_id(s),o);
  END new;

  VAR t: obs.TypePtr; p: obs.ParmPtr;
BEGIN
  t:=obs.MakeProcType();
  p:=obs.AppParm(t,scan.DmId,obs.addrp,{obs.varpar});
  p:=obs.AppParm(t,scan.DmId,obs.intp,{});
  obs.ProcType(t);
  gen.scanParms(t^.plist);
  new(gen._new,t,'NEW');
  new(gen._dispose,t,'DISPOSE');
  t:=obs.MakeProcType();
  p:=obs.AppParm(t,scan.DmId,obs.addrp,{obs.varpar});
  p:=obs.AppParm(t,scan.DmId,obs.intp ,{obs.varpar});
  p:=obs.AppParm(t,scan.DmId,obs.intp ,{});
  p:=obs.AppParm(t,scan.DmId,obs.intp ,{});
  obs.ProcType(t);
  gen.scanParms(t^.plist);
  new(gen._resize,t,'RESIZE');
END IniSTORAGE;

PROCEDURE compare(rts,o: obs.ObjPtr);
  VAR err: INTEGER;
BEGIN
  IF (rts^.mode=obs.inv) OR (o^.mode=obs.inv) THEN RETURN END;
  IF (o^.mode#obs.proc)&((o^.mode#obs.vari)OR(o^.type^.mode#obs.proctype)) THEN
    scan.err_id(50,o^.id); RETURN
  END;
  err:=obs.ProcCmp?(rts^.type,o^.type);
  IF err#0 THEN scan.err_id(err,o^.id)
  ELSE
    IF NOT (obs.forward IN rts^.tags) THEN scan.err_id(15,rts^.id) END;
    EXCL(rts^.tags,obs.forward);
    IF o^.mode=obs.vari THEN INCL(rts^.tags,obs.varpar) END;
    rts^.scope:=o^.scope; rts^.addr:=o^.addr;
  END;
END compare;

PROCEDURE compare_storage(m: obs.ObjPtr);

  PROCEDURE check(VAL s1,s2: ARRAY OF CHAR);
    VAR p1,p2: obs.ObjPtr;
  BEGIN
    IF NOT obs.VisInScope?(obs.SuperProc^.head,scan.str_id(s1),p1) THEN
      ASSERT(FALSE)
    END;
    IF NOT obs.VisInScope?(m^.head,scan.str_id(s2),p2) THEN RETURN END;
    compare(p1,p2);
  END check;

BEGIN
  IF m^.mode#obs.modul THEN scan.err(42); RETURN END;
  check('NEW'    ,'ALLOCATE');
  check('DISPOSE','DEALLOCATE');
  check('RESIZE' ,'REALLOCATE');
END compare_storage;

PROCEDURE rts_list;
  VAR o,rts: obs.ObjPtr;
BEGIN ASSERT(sy=scan.with);
  GetSy;
  IF sy#scan.ident THEN scan.expc(scan.ident); RETURN END;
  IF scan.str_id('STORAGE')#scan.Id THEN scan.err_id(14,scan.Id) END;
  GetSy;
  IF sy=scan.lpar THEN GetSy;
    LOOP
      IF sy#scan.ident THEN scan.expc(scan.ident); EXIT END;
      rts:=obs.VisInScope(obs.SuperProc^.head,scan.Id);
      GetSy;
      IF sy#scan.colon THEN scan.expc(scan.colon); EXIT END;
      GetSy;
      qualident(o);
      compare(rts,o);
      IF sy=scan.semic THEN GetSy ELSE EXIT END;
    END;
    CheckGet(scan.rpar);
  ELSIF sy=scan.colon THEN GetSy;
    IF sy#scan.ident THEN scan.expc(scan.ident);
    ELSE
      o:=obs.Vis(scan.Id); GetSy;
      compare_storage(o);
    END;
  ELSE scan.err(36);
  END;
  CheckGet(scan.semic);
END rts_list;

----------------------------  DCLs  ---------------------------
                            --------

PROCEDURE Block;

  PROCEDURE ConstDcl;
    VAR o: obs.ObjPtr; x: gen.ObjPtr;
  BEGIN ASSERT(sy=scan.const); GetSy;
    WHILE sy=scan.ident DO
      obs.NewObj(o,obs.cons); obs.Dcl(scan.Id,o); GetSy;
      IF sy#scan.equ THEN scan.expc(scan.equ); RETURN END;
      GetSy;
      INCL(o^.tags,obs.undefined);
      Expr(x); gen.const_dcl(x,o);
      EXCL(o^.tags,obs.undefined);
      IF    sy=scan.semic THEN GetSy
      ELSIF sy=scan.ident THEN scan.expc(scan.semic)
      ELSE CheckGet(scan.semic); RETURN
      END;
    END;
  END ConstDcl;

  PROCEDURE VarDcl(val?: BOOLEAN);

    PROCEDURE VarDclList;
      VAR f: inter.QUEUE; id: INTEGER;
          o: obs.ObjPtr; tags: BITSET; t: obs.TypePtr;
    BEGIN ASSERT(sy=scan.ident);
      inter.fifo(f); IdList(f,scan.colon); GetSy;
      IF val? THEN tags:={obs.RO} ELSE tags:={} END;
      t:=TypeConstr();
--    IF t^.mode=farr THEN scan.err(38); t:=obs.Any END;
      WHILE inter.pop(f,id) DO
        obs.NewObj(o,obs.vari); o^.type:=t; o^.tags:=tags;
        obs.Dcl(id,o);
        gen.dclVar(o);
      END;
      inter.clear(f);
    END VarDclList;

  BEGIN GetSy;
    WHILE sy=scan.ident DO
      VarDclList;
      CheckGet(scan.semic);
    END;
  END VarDcl;

  PROCEDURE TypeDcl;
    VAR id: INTEGER; o,hid: obs.ObjPtr; t: obs.TypePtr;
  BEGIN ASSERT(sy=scan.type); GetSy;
    WHILE sy=scan.ident DO id:=scan.Id; GetSy;
      IF sy=scan.equ THEN GetSy;
        IF obs.VisInScope?(obs.CurBlock^.head,id,hid) THEN
          IF (hid^.mode=obs.typ) & (hid^.type^.mode=obs.hidden) THEN
            IF obs.CurBlock#CU THEN scan.err(68) END;
            obs.set_hidden(hid^.type,TypeConstr());
          ELSE scan.err_id(15,id); t:=TypeConstr();
          END;
        ELSE
          obs.NewObj(o,obs.typ); obs.Dcl(id,o); o^.scope:=0;
          INCL(o^.tags,obs.undefined);
            o^.type:=TypeConstr();
          EXCL(o^.tags,obs.undefined);
          IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
        END;
      ELSIF (sy=scan.semic) & (unit=comp.def) THEN
        obs.NewObj(o,obs.typ); o^.type:=obs.MakeHidden(1); o^.scope:=0;
        obs.Dcl(id,o);
        IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
      ELSE scan.expc(scan.equ)
      END; CheckGet(scan.semic);
    END;
  END TypeDcl;

  PROCEDURE ModuleDcl;
    VAR id,mid: INTEGER; exps: inter.QUEUE; o: obs.ObjPtr;
          save: BITSET;
  BEGIN
    ASSERT(sy=scan.module); GetSy;
    IF sy#scan.ident THEN scan.Fault(8,'') END;
    mid:=scan.Id; GetSy; CheckGet(scan.semic);
    obs.NewObj(o,obs.modul); o^.addr:=modNo; INC(modNo);
    obs.Dcl(mid,o);
    obs.EnterBlock(o);
    importlist(obs.VisOut);
    inter.fifo(exps);
    IF sy=scan.export THEN GetSy;
      IF sy=scan.qualified THEN INCL(o^.tags,obs.qualified); GetSy END;
      IdList(exps,scan.semic); GetSy;
    END;
    Block;
    save:=scan.opts;
      INCL(scan.opts,ORD('A')-ORD('A'));
      WHILE inter.pop(exps,id) DO obs.Export(id) END;
      inter.clear(exps);
      obs.ExitModule(conflict);
    scan.opts:=save;
    IF sy=scan.ident THEN
      IF scan.Id#mid THEN scan.err(8) END; GetSy
    ELSE scan.expc(scan.ident)
    END;
    CheckGet(scan.semic);
  END ModuleDcl;

  PROCEDURE proc_comp(proc: obs.TypePtr);
    VAR o : obs.ObjPtr;   f   : BOOLEAN;
      parm: obs.ParmPtr;  kind: BITSET;  id,xid: INTEGER;
       que: inter.QUEUE;  val : BOOLEAN;
  BEGIN
    IF sy#scan.lpar THEN
      IF proc^.plist#NIL THEN scan.err(49) END; RETURN
    END;
    parm:=proc^.plist;
    GetSy;
    inter.fifo(que);
    LOOP
      kind:={}; val:=FALSE;
      IF    sy=scan.var   THEN GetSy; INCL(kind,obs.varpar);
      ELSIF sy=scan.val   THEN GetSy; val:=TRUE;
      ELSIF sy#scan.ident THEN EXIT
      END;
      LOOP
        IF sy=scan.ident THEN
          id:=scan.Id; GetSy;
          IF sy=scan.minus THEN
            IF obs.varpar IN kind THEN scan.err(36)
            ELSE INC(id,scan.noIdents);
            END;
            GetSy;
          END;
          inter.push(que,id)
        ELSE CheckGet(scan.ident);
        END;
        IF    sy=scan.coma  THEN GetSy
        ELSIF sy=scan.colon THEN EXIT
        ELSIF sy=scan.ident THEN scan.expc(scan.coma)
        ELSE EXIT
        END
      END;
      CheckGet(scan.colon);
      f:=(sy=scan.array);
      IF f THEN GetSy; CheckGet(scan.of) END;
      qualident(o); obs.chkType(o);
      WHILE (parm#NIL) & inter.pop(que,xid) DO
        IF xid>scan.noIdents THEN id:=xid-scan.noIdents ELSE id:=xid END;
        IF f THEN
          IF parm^.type^.mode#obs.farr THEN scan.err_id(21,id)
          ELSE obs.TypeCmp(parm^.type^.base,o^.type);
            parm^.type^.base:=o^.type;
          END;
        ELSE obs.TypeCmp(parm^.type,o^.type); parm^.type:=o^.type;
        END;
        IF kind#parm^.kind*{obs.varpar} THEN scan.err_id(21,id) END;
        parm^.id:=id;
        IF val OR (xid#id) THEN INCL(parm^.kind,obs.RO) END;
        parm:=parm^.next;
      END;
      IF (parm=NIL) & inter.pop(que,id) THEN scan.err_id(49,id) END;
      IF sy#scan.rpar THEN CheckGet(scan.semic) END;
    END;
    inter.clear(que);
    IF sy=scan.seq THEN GetSy; kind:={obs.seqpar,obs.RO};
      IF sy=scan.var THEN GetSy; kind:=kind/{obs.varpar,obs.RO} END;
      IF sy=scan.ident THEN parm^.id:=scan.Id ELSE scan.expc(scan.ident) END;
      GetSy; CheckGet(scan.colon);
      qualident(o); obs.chkType(o);
      IF parm=NIL THEN scan.err(49)
      ELSE
        IF parm^.type^.mode#obs.farr THEN scan.err_id(21,parm^.id)
        ELSE obs.TypeCmp(parm^.type^.base,o^.type);
          parm^.type^.base:=o^.type;
        END;
        IF parm^.kind#kind THEN scan.err_id(21,parm^.id) END;
        parm:=parm^.next;
      END;
    END;
    CheckGet(scan.rpar);
    IF parm#NIL THEN scan.err(49) END;
    IF sy=scan.colon THEN GetSy;
      qualident(o); obs.chkType(o); obs.chkSimple(o^.type);
      IF proc^.mode=obs.functype THEN
        obs.TypeCmp(proc^.base,o^.type); proc^.base:=o^.type;
      ELSE scan.err(21)
      END;
    ELSIF proc^.mode#obs.proctype THEN scan.err(21)
    END;
  END proc_comp;

  PROCEDURE headType(): obs.TypePtr;
    VAR proc,parm: obs.TypePtr; kind: BITSET; o: obs.ObjPtr; p: obs.ParmPtr;
        que: inter.QUEUE; id: INTEGER;
  BEGIN
    proc:=obs.MakeProcType();
    IF sy#scan.lpar THEN obs.ProcType(proc); RETURN proc END;
    GetSy;
    inter.fifo(que);
    LOOP
      kind:={};
      IF    sy=scan.var THEN GetSy; INCL(kind,obs.varpar);
      ELSIF sy=scan.val THEN GetSy;
        IF unit=comp.def THEN scan.err(37) ELSE INCL(kind,obs.RO) END;
      ELSIF sy#scan.ident THEN EXIT
      END;
      LOOP
        IF sy=scan.ident THEN
          id:=scan.Id; GetSy;
          IF sy=scan.minus THEN
            IF obs.varpar IN kind THEN scan.err(36)
            ELSE INC(id,scan.noIdents);
            END;
            GetSy;
          END;
          inter.push(que,id)
        ELSE CheckGet(scan.ident);
        END;
        IF    sy=scan.coma  THEN GetSy
        ELSIF sy=scan.colon THEN EXIT
        ELSIF sy=scan.ident THEN scan.expc(scan.coma)
        ELSE EXIT
        END
      END;
      CheckGet(scan.colon);
      parm:=FormalType();
      WHILE inter.pop(que,id) DO
        IF id>=scan.noIdents THEN
          p:=obs.AppParm(proc,id-scan.noIdents,parm,kind+{obs.RO})
        ELSE
          p:=obs.AppParm(proc,id,parm,kind)
        END;
      END;
      IF sy#scan.rpar THEN CheckGet(scan.semic) END
    END;
    inter.clear(que);
    IF sy=scan.seq THEN GetSy; kind:={obs.seqpar,obs.RO};
      IF sy=scan.var THEN GetSy; kind:=kind/{obs.varpar,obs.RO} END;
      IF sy#scan.ident THEN scan.expc(scan.ident); id:=scan.DmId
      ELSE id:=scan.Id
      END;
      GetSy; CheckGet(scan.colon);
      qualident(o); obs.chkType(o);
      p:=obs.AppParm(proc,id,obs.MakeFlx(o^.type),kind);
    END;
    CheckGet(scan.rpar);
    function?(proc);
    gen.scanParms(proc^.plist);
    RETURN proc
  END headType;

  PROCEDURE CodeProc(p: obs.ObjPtr);
    VAR o: gen.ObjPtr;
  BEGIN
    INCL(p^.tags,obs.code_proc);
    gen.enterCodeProc(p);
    WHILE sy#scan.end DO
      Expr(o);
      IF (sy=scan.semic) OR (sy=scan.rpar) THEN scan.err(26); GetSy END;
      gen.code_expr(p,o);
    END; GetSy;
    gen.exitCodeProc(p);
  END CodeProc;

  PROCEDURE newCodeProc(p: obs.ObjPtr);
    VAR o: gen.ObjPtr;
  BEGIN
    IF unit=comp.def THEN scan.err(37) END;
    IF obs.forward IN p^.tags THEN scan.err(15) END;
    INCL(p^.tags,obs.code_proc);
    gen.enterCodeProc(p);
    IF sy#scan.semic THEN
      LOOP
        Expr(o);
        gen.code_expr(p,o);
        IF sy=scan.semic THEN EXIT  END;
        IF sy=scan.coma  THEN GetSy END;
      END;
    END;
    gen.exitCodeProc(p);
    GetSy;
  END newCodeProc;

  PROCEDURE ProcHead(VAR p: obs.ObjPtr; VAR new_code: BOOLEAN);
    VAR pre?: BOOLEAN; pid: INTEGER;
  BEGIN
    ASSERT(sy=scan.procedure); GetSy;
    new_code:=(sy=scan.minus);
    IF new_code THEN GetSy END;
    IF sy#scan.ident THEN scan.expc(scan.ident); pid:=scan.DmId
    ELSE pid:=scan.Id
    END;
    GetSy;
    pre?:=obs.VisInScope?(obs.CurBlock^.head,pid,p)&(obs.forward IN p^.tags);
    IF pre? THEN proc_comp(p^.type);
    ELSE obs.NewObj(p,obs.proc); p^.type:=headType(); obs.Dcl(pid,p);
    END;
    IF new_code THEN newCodeProc(p)
    ELSE
      CheckGet(scan.semic);
      IF NOT pre? & (sy#scan.code) THEN gen.dclProc(p) END;
    END;
  END ProcHead;

  PROCEDURE dclParms(p: obs.ParmPtr);
    VAR o: obs.ObjPtr;
  BEGIN
    WHILE (p#NIL) & (p^.id>=0) DO
      obs.NewObj(o,obs.vari);
      o^.type:=p^.type; o^.tags:=p^.kind;
      gen.dclParm(o,p); obs.Dcl(p^.id,o);
      p:=p^.next;
    END;
  END dclParms;

  PROCEDURE ProcDcl;
    VAR p: obs.ObjPtr; new_code: BOOLEAN;
  BEGIN
    ProcHead(p,new_code);
    IF (unit=comp.def) OR (new_code) THEN RETURN END;
    IF sy=scan.forward THEN
      GetSy; CheckGet(scan.semic);
      IF obs.forward IN p^.tags THEN scan.err(63) END;
      INCL(p^.tags,obs.forward); RETURN
    ELSIF sy=scan.code THEN
      IF obs.forward IN p^.tags THEN scan.err(15) END;
      GetSy;
      CodeProc(p)
    ELSE
      EXCL(p^.tags,obs.forward);
      obs.EnterBlock(p);
        gen.enterProc(p);
          dclParms(p^.type^.plist);
          Block;
        gen.exitProc(p,sym.put_xpos);
      obs.ExitProc;
    END;
    IF sy=scan.ident THEN
      IF (p^.id#scan.DmId) & (p^.id#scan.Id) THEN scan.err_id(8,p^.id) END;
      GetSy
    ELSE scan.expc(scan.ident)
    END;
    CheckGet(scan.semic);
  END ProcDcl;

  PROCEDURE checkFWDproc;
    VAR l: obs.ObjPtr;
  BEGIN l:=obs.locals();
    WHILE l#NIL DO
      IF (l^.mode=obs.proc) & (obs.forward IN l^.tags)
                            & NOT (obs.duplicate IN l^.tags)
      THEN scan.err_id(58,l^.id)
      END;
      l:=l^.next;
    END;
  END checkFWDproc;

  VAR save: BITSET;
BEGIN   (* Block *)
  inter.fifo(FWD);
  LOOP
    CASE sy OF
     |scan.const    : ConstDcl
     |scan.var      : VarDcl(FALSE);
     |scan.val      : IF unit#comp.def THEN scan.err(77) END;
                      VarDcl(TRUE);
     |scan.type     : TypeDcl
     |scan.module   : checkFWD; ModuleDcl; inter.fifo(FWD);
     |scan.procedure: checkFWD; ProcDcl;   inter.fifo(FWD);
     |scan.with     : IF unit=comp.def THEN scan.err(37) END;
                      IF obs.CurBlock#CU THEN scan.err(68) END;
                      rts_list;
     |scan.begin,scan.end: checkFWD; EXIT
    ELSE scan.err(36); GetSy
    END;
  END;
  IF (sy=scan.begin) THEN GetSy;
    IF unit=comp.def THEN scan.err(37) END;
    IF sy#scan.end THEN StatSeq END;
  END;
  CheckGet(scan.end);
  IF unit#comp.def THEN
    save:=scan.opts;
      INCL(scan.opts,ORD('A')-ORD('A'));
      checkFWDproc;
    scan.opts:=save;
  END;
  IF scan.noErrors=0 THEN sym.outscope END;
--IF ORD('U')-ORD('A') IN scan.opts THEN mxBug.VisUnit END;
END Block;

----------------------------------------------------------------

PROCEDURE external(id: INTEGER): obs.ObjPtr;
  VAR o: obs.ObjPtr;
BEGIN
  IF obs.VisInScope?(obs.SuperProc^.head,id,o) & (obs.complete IN o^.tags) THEN
    RETURN o
  END;
  sym.getsym(id);
  RETURN obs.VisInScope(obs.SuperProc^.head,id);
END external;

PROCEDURE compilationUnit;
  VAR def_time,imp_time,tag: INTEGER;
BEGIN
  IF    sy=scan.definition     THEN GetSy; unit:=comp.def;
  ELSIF sy=scan.implementation THEN GetSy; unit:=comp.imp;
  ELSE unit:=comp.main;
  END;
  IF sy#scan.module THEN scan.Fault(6,''); RETURN END;
  GetSy;
  IF sy#scan.ident  THEN scan.Fault(6,''); RETURN END;
  obs.NewObj(CU,obs.modul); CU^.addr:=0; modNo:=1;
  obs.Dcl(scan.Id,CU); scan.id_str(scan.Id,name);
sym.Ini; gen.Ini(name,cpu);
  GetSy;
  tag:=0;
  IF sy=scan.lbr THEN GetSy;
    IF sy=scan.intval THEN tag:=scan.cVal; GetSy END;
    CheckGet(scan.rbr);
  END;

  CheckGet(scan.semic);
  obs.EnterBlock(CU);
  def_time:=inter.time(); imp_time:=-1;
  IF    unit=comp.imp THEN
    CU^.head^.ctime:=-1; sym.getsym(CU^.id);
    imp_time:=def_time; def_time:=CU^.head^.ctime;
  ELSIF unit=comp.main THEN CU^.head^.ctime:=def_time; imp_time:=def_time;
  END;

  importlist(external);
  sym.createsym(name,unit,def_time,imp_time);

  Block;
  obs.ExitModule(conflict);

  IF (sy#scan.ident) OR (scan.Id#CU^.id) THEN scan.err(08)
  ELSE GetSy;
    IF sy#scan.period THEN scan.expc(scan.period) END
  END;
  IF NOT scan.fault & (scan.noErrors=0) & (unit#comp.def) THEN
    gen.GenCode(name,sym.put_xpos,def_time,imp_time,tag,code)
  END;
  sym.closesym;
gen.Exi; sym.Exi;
END compilationUnit;

--------------------------  COMPILE  --------------------------
                          -----------

VAR end?: BOOLEAN;

PROCEDURE IniSTANDARD;

  VAR stnd?: BOOLEAN;

  PROCEDURE type0(t: obs.TypePtr; VAL s: ARRAY OF CHAR);
    VAR o: obs.ObjPtr;
  BEGIN
    obs.NewObj(o,obs.typ); o^.type:=t; o^.scope:=0;
    obs.Dcl(scan.str_id(s),o);
    IF o^.type^.obj=NIL THEN o^.type^.obj:=o END;
    IF stnd? THEN INCL(o^.tags,obs.penetrate) END;
  END type0;

  PROCEDURE type(VAR t: obs.TypePtr; m: obs.TypeMode; VAL s: ARRAY OF CHAR);
  BEGIN
    obs.NewType(t,m);
    type0(t,s);
  END type;

  PROCEDURE proc(VAL s: ARRAY OF CHAR; no: INTEGER);
    VAR o: obs.ObjPtr;
  BEGIN
    obs.NewObj(o,obs.std_proc); o^.no:=no;
    o^.type:=obs.Any; -- NIL;
    IF stnd? THEN INCL(o^.tags,obs.penetrate) END;
    obs.Dcl(scan.str_id(s),o);
  END proc;

  PROCEDURE const(s: ARRAY OF CHAR; val: INTEGER; t: obs.TypePtr);
    VAR o: obs.ObjPtr;
  BEGIN
    obs.NewObj(o,obs.cons);
    o^.type:=t; o^.addr:=val; o^.tags:={obs.penetrate};
    obs.Dcl(scan.str_id(s),o);
  END const;

  PROCEDURE IniSYSTEM;
    VAR system: obs.ObjPtr; o: obs.ObjPtr;
  BEGIN
    obs.NewObj(system,obs.modul);
    system^.tags:={obs.qualified,obs.complete};
    obs.Dcl(scan.str_id("SYSTEM"),system);
    obs.EnterBlock(system);
      type(obs.wordp,obs.word,"WORD");
      type(obs.addrp,obs.addr,"ADDRESS");
      proc("ADR",gen._adr);
      proc("ORIGIN",_origin);
      o:=system^.head^.locals;
      WHILE o#NIL DO obs.Export(o^.id); o:=o^.next END;
    obs.ExitModule(conflict);
  END IniSYSTEM;

BEGIN
  stnd?:=TRUE;
  type(obs.intp,obs.int,"INTEGER");
    type0(obs.intp,"LONGINT");
    type0(obs.intp,"SHORTINT");
  type(obs.charp,obs.char,"CHAR");
  type(obs.procp,obs.proctype,"PROC");
  obs.procp^.base:=NIL; obs.procp^.plist:=NIL;
  type(obs.realp,obs.real,"REAL");
    type0(obs.realp,"LONGREAL");
  type(obs.bitsetp,obs.bitset,"BITSET"); obs.bitsetp^.base:=obs.intp;
  type(obs.boolp,obs.bool,"BOOLEAN");
  const("FALSE",0,obs.boolp);
  const("TRUE" ,1,obs.boolp);

  stnd?:=FALSE; IniSYSTEM; stnd?:=TRUE;

  obs.stringp:=obs.MakeDynArr(obs.charp,1); type0(obs.stringp,"STRING");

  const("NIL",INTEGER(NIL),obs.addrp);
----------------------------------------------------------------
  proc("ABS"  ,gen._abs);    proc("ODD"  ,gen._odd);
  proc("ORD"  ,gen._ord);    proc("SIZE" ,gen._size);
  proc("BYTES",gen._bytes);  proc("BITS" ,gen._bits);
  proc("FLOAT",gen._float);  proc("TRUNC",gen._trunc);
  proc("CAP"  ,gen._cap);    proc("CHR"  ,gen._chr);
  proc("MIN"  ,gen._min);    proc("MAX"  ,gen._max);
  proc("HIGH" ,gen._high);   proc("REF"  ,gen._ref);
  proc("LEN"  ,gen._len);
  proc("ASH"  ,_ash);
  proc("SHORT",_short_long);
  proc("LONG",_short_long);
----------------------------------------------------------------
  proc("INC" ,gen._inc);     proc("DEC"   ,gen._dec);
  proc("INCL",gen._incl);    proc("EXCL"  ,gen._excl);
  proc("HALT",gen._halt);    proc("ASSERT",gen._assert);
  proc("DCOPY",_dcopy);
END IniSTANDARD;

PROCEDURE compile(text : comp.io_ptr;
                   ini  : comp.INI;
                   exi  : comp.EXI;
                   error: comp.ERROR;
                   print: comp.PRINT;
                   opts : BITSET;
                   _cpu : INTEGER);
  CONST debug = { ORD('F')-ORD('A'), ORD('D')-ORD('A') };
BEGIN
  inter.ini  :=ini;     inter.exi  :=exi;
  inter.error:=error;   inter.print:=print;
  scan.opts:=opts;
  name[0]:=0c;
  code:=0;
  IF _cpu IN {0..2} THEN cpu:=_cpu ELSE cpu:=cpu0 END;
  scan.Ini(text);
  obs.Ini; IniSTANDARD; IniSTORAGE;
  withs:=NIL;
end?:=(debug*scan.opts={});
  compilationUnit;
end?:=TRUE;
  obs.Exi;
  scan.Exi;
END compile;

PROCEDURE get_import(text  : comp.io_ptr;
                     error : comp.ERROR;
                     print : comp.PRINT;
                     import: ONE_MODULE;
                    );
  VAR name: ARRAY [0..255] OF CHAR;
     from?: BOOLEAN; id: INTEGER;
BEGIN
  inter.error:=error;   inter.print:=print;
  scan.opts:=opts;
  name[0]:=0c;
  code:=0;
  scan.Ini(text);

  IF    sy=scan.definition     THEN unit:=comp.def; GetSy
  ELSIF sy=scan.implementation THEN unit:=comp.imp; GetSy
  ELSE                              unit:=comp.main;
  END;
  IF sy#scan.module THEN scan.Fault(6,''); RETURN END;
  GetSy;
  IF sy#scan.ident  THEN scan.Fault(6,''); RETURN END;
  scan.id_str(scan.Id,name);
  GetSy;
  IF sy=scan.lbr THEN GetSy;
    IF sy=scan.intval THEN GetSy END;
    CheckGet(scan.rbr);
  END;
  CheckGet(scan.semic);
  WHILE (sy=scan.import) OR (sy=scan.from) DO
    from?:=(sy=scan.from);
    IF from? THEN GetSy;
      IF sy#scan.ident THEN scan.expc(scan.ident);
      ELSE scan.id_str(scan.Id,name); import(name); GetSy;
      END;
    END;
    CheckGet(scan.import);
    LOOP
      IF sy=scan.ident THEN id:=scan.Id; GetSy;
        IF   from? THEN
        ELSIF sy=scan.colon THEN GetSy;
          IF sy=scan.ident THEN scan.id_str(scan.Id,name); import(name) END;
          CheckGet(scan.ident);
        ELSE scan.id_str(id,name); import(name);
        END;
      ELSE CheckGet(scan.ident);
      END;
      IF sy#scan.coma THEN EXIT END;
      GetSy;
    END;
    CheckGet(scan.semic);
  END;
  scan.Exi;
END get_import;

PROCEDURE fault(VAL format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  scan.Fault(0,format,args);
END fault;

PROCEDURE final;
  VAR s: ARRAY [0..63] OF CHAR;
BEGIN
  IF end? THEN RETURN END;
  inter.xprint('*****************************\n');
  inter.xprint('textpos=%d.%d    ',scan.line,scan.col);
  IF scan.sy=scan.ident THEN scan.id_str(scan.Id,s);
    inter.xprint('sy="%s"',s);
  ELSE scan.vis_sym(sy,s); inter.xprint('sy=%s [%d]',s,sy);
  END;
  inter.xprint('\n*****************************\n');
END final;

BEGIN
  vers:='Modula X  v0.993 /03-Nov-91/';
  gen_opts:={
         ORD('W')-ORD('A')
        ,ORD('N')-ORD('A')
        ,ORD('T')-ORD('A')
        ,ORD('R')-ORD('A')
        ,ORD('I')-ORD('A')
        };
  cpu0:=gen.cpu();
  cpu :=cpu0;
  opts:={
         ORD('T')-ORD('A')
        ,ORD('R')-ORD('A')
        ,ORD('I')-ORD('A')
        };
  unit:=-1;
  code:=0;
  inter.final(final); end?:=TRUE;
END mxPars.
