IMPLEMENTATION MODULE pcVis; (* Ned 04-Apr-91. (c) KRONOS *)

IMPORT  pcK, pcM;

PROCEDURE type_mode(t: pcK.STRUCT; VAR s: ARRAY OF CHAR);
BEGIN
  s[0]:=0C;
  CASE t^.mode OF
    |pcK.invtype  : pcM.app(s,"*INV*");
    |pcK.undef    : pcM.app(s,"*UND*");
    |pcK.shortint : pcM.app(s,"SINT");
    |pcK.shortcard: pcM.app(s,"SCARD");
    |pcK.shortIC  : pcM.app(s,"SIC");
    |pcK.IC       : pcM.app(s,"IC");
    |pcK.integer  : pcM.app(s,"INT");
    |pcK.cardinal : pcM.app(s,"CARD");
    |pcK.longint  : pcM.app(s,"LINT");
    |pcK.real     : pcM.app(s,"REAL");
    |pcK.longreal : pcM.app(s,"LREAL");
    |pcK.boolean  : pcM.app(s,"BOOL");
    |pcK.char     : pcM.app(s,"CHAR");
    |pcK.bitset   : pcM.app(s,"BITSET");
    |pcK.byte     : pcM.app(s,"BYTE");
    |pcK.word     : pcM.app(s,"WORD");
    |pcK.addr     : pcM.app(s,"ADDRESS");
    |pcK.niltype  : pcM.app(s,"NILTYPE");
    |pcK.range    : pcM.app(s,"RANGE");
    |pcK.enum     : pcM.app(s,"ENUM");
    |pcK.opaque   : pcM.app(s,"OPAQUE");
    |pcK.pointer  : pcM.app(s,"PTR");
    |pcK.set      : pcM.app(s,"SET");
    |pcK.proctype : pcM.app(s,"PROCTYPE");
    |pcK.array    : pcM.app(s,"ARRAY");
    |pcK.vector   : pcM.app(s,"VECTOR");
    |pcK.array_of : pcM.app(s,"ARR_OF");
    |pcK.dynarr   : pcM.app(s,"DYNARR");
    |pcK.record   : pcM.app(s,"RECORD");
  ELSE pcM.abort
  END;
END type_mode;

PROCEDURE ws(name,s: ARRAY OF CHAR);
BEGIN
  pcM.ws(name); pcM.wc('='); pcM.ws(s); pcM.wc(' ');
END ws;

PROCEDURE wi(name: ARRAY OF CHAR; n: LONGINT);
BEGIN
  pcM.ws(name); pcM.wc('='); pcM.wi(n,0); pcM.wc(' ');
END wi;

PROCEDURE pair(a,b: LONGINT);
BEGIN
  pcM.wc('['); pcM.wi(a,0); pcM.ws('..'); pcM.wi(b,0); pcM.ws('] ');
END pair;

PROCEDURE num(a: LONGINT);
BEGIN
  pcM.wc('['); pcM.wi(a,0); pcM.ws('] ');
END num;

PROCEDURE type(t: pcK.STRUCT);
  VAR s: ARRAY [0..15] OF CHAR;
BEGIN
  IF t=NIL THEN pcM.ws('type=NIL ');
  ELSE
    type_mode(t,s);
    ws('type',s); num(t^.size);
    CASE t^.mode OF
      |pcK.range : type_mode(t^.base,s);
                   pcM.ws(s); pair(t^.n,t^.m);
      |pcK.enum  : pair(t^.n,t^.m);
      |pcK.record: wi('n',t^.n); wi('m',t^.m);
      |pcK.array_of,pcK.dynarr,pcK.vector: wi('n',t^.n);
    ELSE
    END;
  END;
END type;

PROCEDURE locs(o: pcK.OBJECT);
BEGIN
  IF o=NIL THEN RETURN END;
  locs(o^.l);
  obj(o);
  locs(o^.r);
END locs;

PROCEDURE struct(t: pcK.STRUCT);
BEGIN
  pcM.ws('T: ');
  type(t);
  pcM.wl;
END struct;

PROCEDURE mode(s: ARRAY OF CHAR);
BEGIN
  pcM.ws(s);
END mode;

PROCEDURE adr(o: pcK.OBJECT);
BEGIN
  pcM.wc('['); pcM.wi(LONGINT(o^.scope),0); pcM.wc(',');
  pcM.wi(o^.adr,0); pcM.ws('] ');
END adr;

PROCEDURE obj(o: pcK.OBJECT);
BEGIN
  pcM.ws('O: '); (* tty.print('O:%05h:  ',o);*)
  CASE o^.mode OF
    |pcK.cons   : mode('CONS   ');  type(o^.type);
    |pcK.var    : mode('VAR    ');  type(o^.type);  adr(o);
    |pcK.varpar : mode('VPAR   ');  type(o^.type);  adr(o);
    |pcK.varseq : mode('VSEQ   ');  type(o^.type);  adr(o);
    |pcK.seq    : mode('SEQ    ');  type(o^.type);  adr(o);
    |pcK.field  : mode('FLD    ');  type(o^.type);  adr(o);
    |pcK.proc   : mode('PROC   ');  type(o^.type);  adr(o);
    |pcK.xproc  : mode('XPROC  ');  type(o^.type);  adr(o);
    |pcK.cproc  : mode('CPROC  ');  type(o^.type);
    |pcK.type   : mode('TYPE   ');  type(o^.type);
    |pcK.method : mode('METHOD ');  type(o^.type);  wi('adr',o^.adr);
    |pcK.module : mode('MODULE ');
  ELSE wi('mode',LONGINT(o^.mode)); type(o^.type);
  END;
  pcM.wc('"'); pcM.ws(o^.name); pcM.wc('"'); pcM.wl;
END obj;

PROCEDURE node(n: pcK.NODE);

  PROCEDURE nobj;
  BEGIN
    IF n^.obj=NIL THEN pcM.ws('obj=NIL ');
    ELSE
      pcM.ws('   '); adr(n^.obj);
      pcM.wc('"'); pcM.ws(n^.obj^.name); pcM.wc('"');
    END;
  END nobj;

  PROCEDURE sub;
  BEGIN
    CASE n^.sub OF
      |pcK.bits : pcM.ws('bits  ')
      |pcK.bytes: pcM.ws('bytes ')
      |pcK.size : pcM.ws('size  ')
      |pcK.high : pcM.ws('high  ')
      |pcK.len  : pcM.ws('len   ')
      |pcK.max  : pcM.ws('max   ')
      |pcK.min  : pcM.ws('min   ')
      |pcK.abs  : pcM.ws('abs   ')
      |pcK.adr  : pcM.ws('adr   ')
      |pcK.ash  : pcM.ws('ash   ')
      |pcK.cap  : pcM.ws('cap   ')
      |pcK.conv : pcM.ws('conv  ')
      |pcK.typetran: pcM.ws('typetran ')
      |pcK.odd  : pcM.ws('odd   ')
      |pcK.not  : pcM.ws('not   ')
      |pcK.equ  : pcM.ws('equ   ')
      |pcK.neq  : pcM.ws('neq   ')
      |pcK.lss  : pcM.ws('lss   ')
      |pcK.leq  : pcM.ws('leq   ')
      |pcK.gtr  : pcM.ws('gtr   ')
      |pcK.geq  : pcM.ws('geq   ')
      |pcK.in   : pcM.ws('in    ')
      |pcK.is   : pcM.ws('is    ')
      |pcK.mul  : pcM.ws('mul   ')
      |pcK.div  : pcM.ws('div   ')
      |pcK.slash: pcM.ws('slash ')
      |pcK.mod  : pcM.ws('mod   ')
      |pcK.rem  : pcM.ws('rem   ')
      |pcK.plus : pcM.ws('plus  ')
      |pcK.minus: pcM.ws('minus ')
      |pcK.and  : pcM.ws('and   ')
      |pcK.or   : pcM.ws('or    ')
      |pcK.xor  : pcM.ws('xor   ')
      |pcK.bic  : pcM.ws('bic   ')
      |pcK.cand : pcM.ws('cand  ')
      |pcK.cor  : pcM.ws('cor   ')
      |pcK.rol  : pcM.ws('rol   ')
      |pcK.ror  : pcM.ws('ror   ')
      |pcK.rcheck: pcM.ws('rcheck ')
      |pcK.concat: pcM.ws('concat ');
    ELSE pcM.abort
    END
  END sub;

  PROCEDURE rchk;
  BEGIN
    IF    n^.sub=0 THEN pcM.ws('(no check) ')
    ELSIF n^.sub=pcK.rcheck THEN pcM.ws('(check) ')
    ELSE  pcM.abort;
    END;
  END rchk;

BEGIN
  CASE n^.mode OF
    |pcK.inv      : pcM.ws('*INV*  ');  nobj
    |pcK.header   : pcM.ws('HEADER ');  nobj
    |pcK.cons     : pcM.ws('CONST  ');  type(n^.type); nobj
    |pcK.var      : pcM.ws('VAR    ');  type(n^.type); nobj
    |pcK.varpar   : pcM.ws('VARPAR ');  type(n^.type); nobj
    |pcK.seq      : pcM.ws('SEQ    ');  type(n^.type); nobj
    |pcK.varseq   : pcM.ws('VARSEQ ');  type(n^.type); nobj
    |pcK.field    : pcM.ws('FIELD  ');  type(n^.type); nobj
    |pcK.method   : pcM.ws('METHOD ');  type(n^.type); nobj
    |pcK.proc     : pcM.ws('PROC   ');  type(n^.type); nobj
    |pcK.cproc    : pcM.ws('CPROC  ');  type(n^.type); nobj
    |pcK.sproc    : pcM.ws('SPROC  ');  nobj
    |pcK.sfunc    : pcM.ws('SFUNC  ');  nobj
    |pcK.type     : pcM.ws('TYPE   ');  type(n^.type); nobj
    |pcK.module   : pcM.ws('MODULE ');  nobj
    |pcK.index    : pcM.ws('INDEX  ');  rchk; type(n^.type);
    |pcK.binary   : pcM.ws('BINARY ');  sub; type(n^.type);
    |pcK.unary    : pcM.ws('UNARY  ');  sub; type(n^.type);
    |pcK.deref    : pcM.ws('DEREF  ');  type(n^.type);
    |pcK.guard    : pcM.ws('GUARD  ');  type(n^.type); nobj;
    |pcK.eguard   : pcM.ws('EGUARD ');  type(n^.type); nobj;
    |pcK.value    : pcM.ws('VALUE  ');  type(n^.type);
                   IF n^.type^.mode IN pcK.SCALARs THEN
                     wi('val',n^.val)
                   END;
    |pcK.aggregate: pcM.ws('AGGREG ');  rchk; type(n^.type);
    |pcK.sequence : pcM.ws('SEQU   ');  type(n^.type);
    |pcK.pair     : pcM.ws('PAIR   ');  pair(n^.a,n^.b);
    |pcK.node     : pcM.ws('NODE   ');
    |pcK.char2str : pcM.ws('CH2STR ');  type(n^.type);
    |pcK.while    : pcM.ws('WHILE  ');
    |pcK.repeat   : pcM.ws('REPEAT ');
    |pcK.loop     : pcM.ws('LOOP   ');
    |pcK.exit     : pcM.ws('EXIT   ');
    |pcK.return   : pcM.ws('RETURN ');
    |pcK.for      : pcM.ws('FOR    ');  nobj
    |pcK.with     : pcM.ws('WITH   ');  nobj
    |pcK.wguard   : pcM.ws('WGUARD ');  nobj
    |pcK.if       : pcM.ws('IF     ');
    |pcK.ifelse   : pcM.ws('IFELSE ');
    |pcK.case     : pcM.ws('CASE   ');
    |pcK.caselse  : pcM.ws('CASELS ');
    |pcK.casedo   : pcM.ws('CASEDO ');
    |pcK.assign   : pcM.ws(':=     ');  nobj;
    |pcK.call     : pcM.ws('CALL   ');  type(n^.type); nobj;
    |pcK.inc      : pcM.ws('INC    ');
    |pcK.dec      : pcM.ws('DEC    ');
    |pcK.incl     : pcM.ws('INCL   ');  rchk
    |pcK.excl     : pcM.ws('EXCL   ');  rchk
    |pcK.new      : pcM.ws('NEW    ');
    |pcK.dispose  : pcM.ws('DISPOSE ');
    |pcK.resize   : pcM.ws('RESIZE ');
    |pcK.halt     : pcM.ws('HALT   ');
    |pcK.copy     : pcM.ws('COPY   ');
    |pcK.dcopy    : pcM.ws('DCOPY  ');
    |pcK.move     : pcM.ws('MOVE   ');
    |pcK.assert   : pcM.ws('ASSERT ');
    |pcK.origin   : pcM.ws('ORIGIN ');
    |pcK.inittd   : pcM.ws('initTD '); type(n^.type);
                    wi('adr',n^.type^.adr);
    |pcK.free     : pcM.ws('FREE'  );
  ELSE wi('mode',LONGINT(n^.mode)); type(n^.type);
  END;
  pcM.wl;
END node;

VAR ndepth: INTEGER;

PROCEDURE vis(n: pcK.NODE);
  VAR i: INTEGER;
BEGIN
  WHILE n#NIL DO
    FOR i:=1 TO ndepth DO pcM.wc(' ') END;
    node(n);
    IF (n^.mode#pcK.pair) &
       (n^.mode#pcK.value) &
       (n^.mode#pcK.exit) &
       (n^.mode#pcK.cons) THEN
      INC(ndepth,2); vis(n^.l);
      INC(ndepth,2); vis(n^.r)
    END;
    n:=n^.next;
  END;
  DEC(ndepth,2)
END vis;

(*----------------------------------------------------------------*)
(*
VAR NO,ASSIGN,IDENT,REF: INTEGER;

PROCEDURE expr(n: pcK.NODE; VAR no,ref,assign,ident: INTEGER);
BEGIN
  WHILE n#NIL DO
    IF n^.mode IN pcK.VARs+pcK.PROCs+{pcK.cons} THEN INC(ref);
    ELSIF (n^.mode=pcK.pair) OR (n^.mode=pcK.value) OR (n^.mode=pcK.exit) THEN
    ELSE
      IF (n^.mode=pcK.assign) OR (n^.mode=pcK.call) THEN
        IF n^.l=NIL THEN INC(assign) END;
      ELSIF (n^.mode=pcK.index) OR (n^.mode=pcK.binary) THEN
        IF n^.l^.mode IN pcK.VARs+pcK.PROCs+{pcK.cons} THEN INC(ident) END;
      ELSIF (n^.mode=pcK.unary) OR (n^.mode=pcK.deref) THEN
        IF n^.r^.mode IN pcK.VARs+pcK.PROCs+{pcK.cons} THEN INC(ident) END;
      END;
      expr(n^.l,no,ref,assign,ident);
      expr(n^.r,no,ref,assign,ident);
    END;
    INC(no);
    n:=n^.next;
  END;
END expr;

PROCEDURE trav(n: pcK.NODE);
  VAR assign,ident,no,ref: INTEGER;
BEGIN
  assign:=0; ident:=0; no:=0; ref:=0;
  expr(n^.r,no,ref,assign,ident);
  tty.print('%-15s nodes=%4d  assign=%3d  ident=%3d  ref=%3d\n'
            ,n^.obj^.name,no,assign,ident,ref);
  INC(NO,no); INC(ASSIGN,assign); INC(IDENT,ident); INC(REF,ref);
END trav;

PROCEDURE down(n: pcK.NODE);
BEGIN
  WHILE n#NIL DO
    down(n^.l);
    trav(n);
    n:=n^.next;
  END;
END down;
*)

PROCEDURE stat(root: pcK.NODE);
BEGIN
(*
  NO:=0; ASSIGN:=0; IDENT:=0; REF:=0;
  tty.print('%.60c\n','-');
  down(root^.l);
  trav(root);
  tty.print('%-15s nodes=%4d  assign=%3d  ident=%3d  ref=%3d\n'
            ,'TOTAL',NO,ASSIGN,IDENT,REF);
  tty.print('%.60c\n','-');
*)
END stat;

BEGIN
  ndepth:=0;
END pcVis.
