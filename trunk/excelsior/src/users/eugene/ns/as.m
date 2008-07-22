MODULE as; (*  29-Dec-90. (c) KRONOS *)

IMPORT mcd : defCodes;
IMPORT mem : pcSystem;
IMPORT tty : Terminal;
IMPORT std : StdIO;
IMPORT str : Strings;
IMPORT sym : nsSym;
IMPORT bio : BIO;
IMPORT arg : tskArgs;
IMPORT put : nsCmd;

FROM SYSTEM      IMPORT ADR, WORD;

WITH STORAGE : mem;

TYPE
  name     = ARRAY [0..15] OF CHAR;
  node     = POINTER TO node_rec;
  mode     = (command,label,symbol);
  node_rec = RECORD
    md : mode;
    l,r: node;
    nm : name;
    val: INTEGER;
  END;

VAR
  tbl      : node;
  src      : DYNARR OF CHAR;
  pos      : INTEGER;
  pass     : INTEGER;
  fail_no  : INTEGER;
  code_base: INTEGER;
  line     : INTEGER;
  line_pos : INTEGER;
  file_name: ARRAY [0..255] OF CHAR;
  err_cnt  : INTEGER;

PROCEDURE error(fmt: ARRAY OF CHAR; SEQ x: WORD);
  VAR txt: ARRAY [0..255] OF CHAR; fr,to,sz: INTEGER;
BEGIN
  fr:=line_pos;
  LOOP
    to:=fr; sz:=0;
    WHILE (sz<40) & (src[to]#36c) DO
      IF to=pos THEN txt[sz]:='$'; INC(sz) END;
      txt[sz]:=src[to]; INC(sz); INC(to);
    END;
    IF (src[to]=36c) OR (to>=pos+20) THEN EXIT END;
    INC(fr);
  END;
  txt[sz]:=0c;
  std.print('*** "%s" %4d.%$2d: %s\n',file_name,line,pos-line_pos,txt);
  std.print(fmt,x);
  std.print('.\n\n');
  INC(err_cnt);
  IF err_cnt>=7 THEN HALT END;
END error;

PROCEDURE fault(fmt: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  error(fmt,x); HALT(1);
END fault;

PROCEDURE cmp_name(VAL t,s: name): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=0;
  LOOP
    IF t[i]='%' THEN
      IF (s[i]='b') OR (s[i]='w') OR (s[i]='d') OR (s[i]='%') THEN INC(i);
      ELSE RETURN -1;
      END;
    ELSIF t[i]='*' THEN
      CASE s[i] OF
        |'e': IF s[i+1]='q' THEN INC(i,2) ELSE RETURN -1 END;
        |'n': IF s[i+1]='e' THEN INC(i,2) ELSE RETURN -1 END;
        |'c': IF (s[i+1]='s') OR (s[i+1]='c') THEN INC(i,2)
              ELSE RETURN -1
              END;
        |'h': IF (s[i+1]='s') OR (s[i+1]='i') THEN INC(i,2)
              ELSE RETURN -1
              END;
        |'l': IF (s[i+1]='s') OR (s[i+1]='e') OR
                 (s[i+1]='o') OR (s[i+1]='t') THEN INC(i,2)
              ELSE RETURN -1
              END;
        |'g': IF (s[i+1]='t') OR (s[i+1]='e') THEN INC(i,2)
              ELSE RETURN -1
              END;
        |'f': IF (s[i+1]='s') OR (s[i+1]='c') THEN INC(i,2)
              ELSE RETURN -1
              END;
      ELSE RETURN -1
      END;
    ELSIF t[i]<s[i] THEN RETURN -1
    ELSIF t[i]>s[i] THEN RETURN 1
    ELSIF s[i]=0c THEN RETURN 0
    ELSE INC(i);
    END;
  END;
END cmp_name;

PROCEDURE cre_node(VAL s: name; md: mode; val: INTEGER);
  VAR t: POINTER TO node; i: INTEGER;
BEGIN
  t:=ADR(tbl);
  LOOP
    IF t^=NIL THEN
      NEW(t^);
      t^^.l:=NIL;
      t^^.r:=NIL;
      t^^.nm:=s;
      t^^.md:=md;
      t^^.val:=val;
      RETURN
    END;
    i:=cmp_name(t^^.nm,s);
    IF    i<0 THEN t:=ADR(t^^.r)
    ELSIF i>0 THEN t:=ADR(t^^.l)
    ELSE error('double declaration: %s',s); EXIT;
    END;
  END;
END cre_node;

PROCEDURE find(VAL s: name): node;
  VAR t: node; i: INTEGER;
BEGIN
  t:=tbl;
  LOOP
    IF t=NIL THEN RETURN NIL END;
    i:=cmp_name(t^.nm,s);
    IF    i<0 THEN t:=t^.r
    ELSIF i>0 THEN t:=t^.l
    ELSE RETURN t;
    END;
  END;
END find;

PROCEDURE print_names;
  VAR cnt: INTEGER;
  PROCEDURE one(t: node);
  BEGIN
    IF t=NIL THEN RETURN END;
    one(t^.l);
    IF t^.md=label THEN
      IF cnt>=2 THEN std.print('\n'); cnt:=0 END;
      std.print('%-16s  %$8hh     ',t^.nm,t^.val);
      INC(cnt);
    END;
    one(t^.r);
  END one;
BEGIN
  cnt:=0;
  one(tbl);
  IF cnt>=2 THEN std.print('\n'); cnt:=0 END;
END print_names;

PROCEDURE number(VAR n: INTEGER);
  VAR ok: BOOLEAN;
BEGIN
  str.iscan(n,src,pos,ok);
  IF NOT ok THEN error('number expected'); n:=0 END;
END number;

PROCEDURE skip;
BEGIN
  WHILE (pos<=HIGH(src)) & (src[pos]=' ') DO INC(pos) END;
END skip;

PROCEDURE ident(VAR nm: name);
  VAR i: INTEGER;
BEGIN
  IF (src[pos]>='A') & (src[pos]<='Z') OR
     (src[pos]>='a') & (src[pos]<='z') THEN
    i:=0;
    REPEAT
      nm[i]:=src[pos]; INC(i); INC(pos);
    UNTIL NOT (
      (src[pos]>='A') & (src[pos]<='Z') OR
      (src[pos]>='a') & (src[pos]<='z') OR
      (src[pos]>='0') & (src[pos]<='9') OR
      (src[pos]='_') );
    nm[i]:=0c;
  ELSE
    nm:=''
  END;
END ident;

PROCEDURE prime(VAR n: INTEGER);
  VAR nm: name; t: node;
BEGIN
  skip;
  IF (src[pos]>='0') & (src[pos]<='9') THEN
    number(n)
  ELSIF (src[pos]>='A') & (src[pos]<='Z') OR
        (src[pos]>='a') & (src[pos]<='z') THEN
    ident(nm);
    IF pass=1 THEN
      n:=put.cnt;
    ELSE
      t:=find(nm);
      IF t=NIL THEN error('not found: %s',nm); n:=0 END;
      IF t^.md#label THEN error('must be label: %s',nm); n:=0 END;
      n:=t^.val;
    END;
  ELSE error('invalid expression'); n:=0;
  END;
END prime;

PROCEDURE factor(VAR n: INTEGER);
BEGIN
  skip;
  IF    src[pos]='+' THEN INC(pos); prime(n);
  ELSIF src[pos]='-' THEN INC(pos); prime(n); n:=-n;
  ELSE prime(n);
  END;
END factor;

PROCEDURE expression(VAR n: INTEGER);
  VAR y: INTEGER;
BEGIN
  factor(n);
  LOOP
    skip;
    IF src[pos]='+' THEN
      INC(pos); factor(y); n:=n+y;
    ELSIF src[pos]='-' THEN
      INC(pos); factor(y); n:=n-y;
    ELSE RETURN
    END;
  END;
END expression;

PROCEDURE check_get(c: CHAR);
BEGIN
  skip;
  IF src[pos]=c THEN INC(pos); RETURN END;
  error('"%c" expected',c);
END check_get;

PROCEDURE access(VAR a: sym.access);
  VAR id: name; p,i,j: INTEGER;
BEGIN
  skip;
  p:=pos; ident(id);
  IF ( (id[0]='r') OR (id[0]='f') ) &
     (id[1]>='0') & (id[1]<='7') & (id[2]=0c) THEN
    WITH a DO
      am:=sym.am_RG;
      xm:=sym.xm_off;
      rg:=ORD(id[1])-ORD('0');
      rg_x:=0;
      n:=0;
      disp:=0;
      level:=0;
    END;
  ELSIF id='tos' THEN
    WITH a DO
      am:=sym.am_TOS;
      xm:=sym.xm_off;
      rg:=0;
      rg_x:=0;
      n:=0;
      disp:=0;
      level:=0;
    END;
  ELSE
    pos:=p;
    IF src[pos]='$' THEN
      INC(pos);
      expression(a.n);
      WITH a DO
        am:=sym.am_imm;
        xm:=sym.xm_off;
        rg:=0;
        rg_x:=0;
        disp:=0;
        level:=0;
      END;
    ELSE
      expression(i);
      skip;
      IF src[pos]='(' THEN
        INC(pos); p:=pos; ident(id);
        IF (id[0]='r') & (id[1]>='0') & (id[1]<='7') & (id[2]=0c) THEN
          WITH a DO
            am:=sym.am_aRG;
            xm:=sym.xm_off;
            rg:=ORD(id[1])-ORD('0');
            rg_x:=0;
            n:=0;
            disp:=i;
            level:=0;
          END;
          check_get(')');
        ELSIF id='fp' THEN
          WITH a DO
            am:=sym.am_aFP;
            xm:=sym.xm_off;
            rg:=0;
            rg_x:=0;
            n:=0;
            disp:=i;
            level:=0;
          END;
          check_get(')');
        ELSIF id='sp' THEN
          WITH a DO
            am:=sym.am_aSP;
            xm:=sym.xm_off;
            rg:=0;
            rg_x:=0;
            n:=0;
            disp:=i;
            level:=0;
          END;
          check_get(')');
        ELSIF id='sb' THEN
          WITH a DO
            am:=sym.am_aSB;
            xm:=sym.xm_off;
            rg:=0;
            rg_x:=0;
            n:=0;
            disp:=i;
            level:=0;
          END;
          check_get(')');
        ELSE
          pos:=p;
          expression(j);
          check_get('(');
          ident(id);
          WITH a DO
            xm:=sym.xm_off;
            rg:=0;
            rg_x:=0;
            n:=j;
            disp:=i;
            level:=0;
          END;
          IF    id='fp' THEN a.am:=sym.am_aaFP;
          ELSIF id='sp' THEN a.am:=sym.am_aaSP;
          ELSIF id='sb' THEN a.am:=sym.am_aaSB;
          ELSE fault('invalid register: %s',id);
          END;
          check_get(')');
        END;
      ELSE
        WITH a DO
          am:=sym.am_abs;
          xm:=sym.xm_off;
          rg:=0;
          rg_x:=0;
          n:=0;
          disp:=i;
          level:=0;
        END;
      END;
    END;
  END;
END access;

PROCEDURE access_index(VAR a: sym.access);
BEGIN
  access(a);
  skip;
  IF src[pos]='[' THEN
    INC(pos); skip;
    IF (src[pos]#'r') OR (src[pos+1]<'0') OR (src[pos+1]>'7') THEN
      error('register name expected'); a.rg_x:=0;
    ELSE
      a.rg_x:=ORD(src[pos+1])-ORD('0'); INC(pos,2);
    END;
    check_get(':'); skip;
    IF    src[pos]='b' THEN a.xm:=sym.xm_b
    ELSIF src[pos]='w' THEN a.xm:=sym.xm_w
    ELSIF src[pos]='d' THEN a.xm:=sym.xm_d
    ELSIF src[pos]='q' THEN a.xm:=sym.xm_q
    ELSE error('illegal scale'); a.xm:=sym.xm_b;
    END;
    INC(pos);
  END;
END access_index;

PROCEDURE register(VAR r: INTEGER);
BEGIN
  skip;
  IF (src[pos]='r') & (src[pos+1]>='0') & (src[pos+1]<='7') THEN
    r:=ORD(src[pos+1])-ORD('0'); INC(pos,2);
  ELSE error('register name expected'); r:=0;
  END;
END register;

PROCEDURE cpu_register(VAR i: put.cpu_reg);
  VAR cmd: name;
BEGIN
  skip;
  ident(cmd);
  IF    cmd='upsr'    THEN i:=put.UPSR;
  ELSIF cmd='fp'      THEN i:=put.FP;
  ELSIF cmd='sp'      THEN i:=put.SP;
  ELSIF cmd='sb'      THEN i:=put.SB;
  ELSIF cmd='psr'     THEN i:=put.PSR;
  ELSIF cmd='intbase' THEN i:=put.INTBASE;
  ELSIF cmd='mod'     THEN i:=put.MD;
  ELSE error('cpu register name expected'); i:=put.UPSR;
  END;
END cpu_register;

PROCEDURE mmu_register(VAR i: put.mmu_reg);
  VAR cmd: name;
BEGIN
  skip;
  ident(cmd);
  IF    cmd='bpr0'    THEN i:=put.BPR0;
  ELSIF cmd='bpr1'    THEN i:=put.BPR1;
  ELSIF cmd='pf0'     THEN i:=put.PF0;
  ELSIF cmd='pf1'     THEN i:=put.PF1;
  ELSIF cmd='sc'      THEN i:=put.SC;
  ELSIF cmd='msr'     THEN i:=put.MSR;
  ELSIF cmd='bcnt'    THEN i:=put.BCNT;
  ELSIF cmd='ptb0'    THEN i:=put.PTB0;
  ELSIF cmd='ptb1'    THEN i:=put.PTB1;
  ELSIF cmd='eia'     THEN i:=put.EIA;
  ELSE error('mmu register name expected');
  END;
END mmu_register;

PROCEDURE command_case(no,isz1,isz2,fsz,cc: INTEGER);
  VAR
    x,y,z   : sym.access;
    cnt     : INTEGER;
    b1,b2,b3: BOOLEAN;
    s       : BITSET;
  PROCEDURE p;
  BEGIN
    IF cnt>0 THEN check_get(',') END;
    CASE cnt OF
      |0: access_index(x);
      |1: access_index(y);
      |2: access_index(z);
    END;
    INC(cnt);
  END p;
  PROCEDURE movs_opts;
  BEGIN
    skip; b1:=FALSE; b2:=FALSE; b3:=FALSE;
    LOOP
      IF    (src[pos]='b') & NOT b1 THEN b1:=TRUE; INC(pos)
      ELSIF (src[pos]='w') & NOT b2 THEN b2:=TRUE; INC(pos)
      ELSIF (src[pos]='u') & NOT b3 THEN b3:=TRUE; INC(pos)
      ELSE RETURN
      END;
    END;
  END movs_opts;
  PROCEDURE reg_list;
    VAR i: INTEGER;
  BEGIN
    s:={}; check_get('['); skip;
    IF src[pos]=']' THEN INC(pos); RETURN END;
    LOOP
      register(i); INCL(s,i); skip;
      IF src[pos]=']' THEN INC(pos); RETURN END;
      check_get(',');
    END;
  END reg_list;
  VAR i,j: INTEGER; crg: put.cpu_reg; mrg: put.mmu_reg;
BEGIN
  cnt:=0;
  CASE no OF
    | 0: p; p; put.cmd(put.mov ,x,y,isz1);
    | 1: p; p; put.cmd(put.add ,x,y,isz1);
    | 2: p; p; put.cmd(put.addc,x,y,isz1);
    | 3: p; p; put.cmd(put.sub ,x,y,isz1);
    | 4: p; p; put.cmd(put.subc,x,y,isz1);
    | 5: p; p; put.cmd(put.cmp ,x,y,isz1);
    | 6: p; p; put.cmd(put.neg ,x,y,isz1);
    | 7: p; p; put.cmd(put.abs ,x,y,isz1);
    | 8: p; p; put.cmd(put.mul ,x,y,isz1);
    | 9: p; p; put.cmd(put.div ,x,y,isz1);
    |10: p; p; put.cmd(put.mod ,x,y,isz1);
    |11: p; p; put.cmd(put.quo ,x,y,isz1);
    |12: p; p; put.cmd(put.rem ,x,y,isz1);
    |13: p; p; put.cmd(put.and ,x,y,isz1);
    |14: p; p; put.cmd(put.or  ,x,y,isz1);
    |15: p; p; put.cmd(put.bic ,x,y,isz1);
    |16: p; p; put.cmd(put.xor ,x,y,isz1);
    |17: p; p; put.cmd(put.com ,x,y,isz1);
    |18: p; p; put.cmd(put.ash ,x,y,isz1);
    |19: p; p; put.cmd(put.lsh ,x,y,isz1);
    |20: p; p; put.cmd(put.rot ,x,y,isz1);
    |21: p; p; put.movx(x,y,isz1,isz2);
    |22: p; p; put.movz(x,y,isz1,isz2);
    |23: -- movq
    |24: -- addq
    |25: -- cmpq
    |26: p; p; put.cmd(put.mei ,x,y,isz1);
    |27: p; p; put.cmd(put.dei ,x,y,isz1);
    |28: p; p; put.cmd(put.not ,x,y,isz1);
    |29: p; put.s(put.condition(cc),x,isz1);
    |30: p; p; put.cmd(put.tbit,x,y,isz1);
    |31: p; p; put.cmd(put.sbit,x,y,isz1);
    |32: p; p; put.cmd(put.cbit,x,y,isz1);
    |33: p; p; put.cmd(put.sbiti,x,y,isz1);
    |34: p; p; put.cmd(put.cbiti,x,y,isz1);
    |35: p; p; put.cmd(put.ibit,x,y,isz1);
    |36: HALT(1); -- cvtp
    |37: p; p; put.cmd(put.ffs,x,y,isz1);
    |38: register(i); check_get(','); p; p; check_get(','); expression(j);
         put.ext(x,y,isz1,i,j);
    |39: register(i); check_get(','); p; p; check_get(','); expression(j);
         put.ins(x,y,isz1,i,j);
    |40: p; p; check_get(','); expression(i); check_get(','); expression(j);
         put.exts(x,y,isz1,i,j);
    |41: p; p; check_get(','); expression(i); check_get(','); expression(j);
         put.inss(x,y,isz1,i,j);
    |42: movs_opts; put.movs (isz1,b1,b2,b3);
    |43: movs_opts; put.movst(b1,b2,b3);
    |44: movs_opts; put.cmps (isz1,b1,b2,b3);
    |45: movs_opts; put.cmpst(b1,b2,b3);
    |46: movs_opts; put.skps (isz1,b1,b2,b3);
    |47: movs_opts; put.skpst(b1,b2,b3);
    |48: p; p; put.cmd(put.addp,x,y,isz1);
    |49: p; p; put.cmd(put.subp,x,y,isz1);
    |50: register(i); check_get(','); p; p; put.index(x,y,i,isz1);
    |51: register(i); check_get(','); p; p; put.check(x,y,i,isz1);
    |52: p; p; check_get(','); expression(i); put.movm(x,y,isz1,i);
    |53: p; p; check_get(','); expression(i); put.cmpm(x,y,isz1,i);
    |54: p; put.jump(x);
    |55: expression(i); DEC(i,code_base+put.cnt);
         put.put(0EAh); put.disp(i);
    |56: expression(i); DEC(i,code_base+put.cnt);
         put.put(BITSET(cc<<4)+{1,3}); put.disp(i);
    |57: p; put.case(x,isz1);
    |58: expression(i); check_get(','); p; check_get(','); expression(j);
         DEC(j,code_base+put.cnt); put.acb(i,x,j,isz1);
    |59: p; put.jsr(x);
    |60: expression(i); DEC(i,code_base+put.cnt);
         put.put(002h); put.disp(i);
    |61: expression(i); put.put(12h); put.disp(i);
    |62: put.put(52h);
    |63: expression(i); put.put(42h); put.disp(i);
    |64: reg_list; put.save(s);
    |65: reg_list; put.restore(s);
    |66: reg_list; check_get(','); expression(i); put.enter(s,i);
    |67: reg_list; put.exit(s);
    |68: cpu_register(crg); check_get(','); p; put.lpr(x,crg,isz1);
    |69: cpu_register(crg); check_get(','); p; put.spr(x,crg,isz1);
    |70: p; put.adjsp(x,isz1);
    |71: p; put.bispsr(x,isz1);
    |72: p; put.bicpsr(x,isz1);
    |73:
      s:={0,1,3}; skip;
      LOOP
        IF    (src[pos]='i') & NOT ( 7 IN s) THEN INC(pos); INCL(s, 7);
        ELSIF (src[pos]='f') & NOT ( 8 IN s) THEN INC(pos); INCL(s, 8);
        ELSIF (src[pos]='m') & NOT ( 9 IN s) THEN INC(pos); INCL(s, 9);
        ELSIF (src[pos]='c') & NOT (10 IN s) THEN INC(pos); INCL(s,10);
        ELSE EXIT
        END;
      END;
      put.put(0Eh); put.put2(s);
    |74: put.put(0A2h);
    |75: put.put(0B2h);
    |76: p; p; put.cmd(put.addr,x,y,isz1);
    |77: put.put(0E2h);
    |78: put.flag;
    |79: put.put(0F2h);
    |80: put.put(0C2h);
(*
  cre_node('mov$'  ,command,81);
  cre_node('movlf' ,command,82);
  cre_node('movfl' ,command,83);
  cre_node('mov%$' ,command,84);
  cre_node('cmp$'  ,command,85);
  cre_node('add$'  ,command,86);
  cre_node('sub$'  ,command,87);
  cre_node('mul$'  ,command,88);
  cre_node('div$'  ,command,89);
  cre_node('neg$'  ,command,90);
  cre_node('abs$'  ,command,91);
  cre_node('round$%',command,92);
  cre_node('trunc$%',command,93);
  cre_node('floor$%',command,94);
*)
    |95: p; put.lfsr(x);
    |96: p; put.sfsr(x);
    |97: mmu_register(mrg); check_get(','); p; put.lmr(x,mrg);
    |98: mmu_register(mrg); check_get(','); p; put.smr(x,mrg);
(*
  cre_node('rdval' ,command,99);
  cre_node('wrval' ,command,100);
  cre_node('movsu%',command,101);
  cre_node('movus%',command,102);
*)
  ELSE error('illegal command %d',no);
  END;
END command_case;

PROCEDURE compile_command(VAL cmd: name);
  CONST
    cc_mnem='eqnecscchilsgtlefsfclohsltge';
  VAR
    cc  : INTEGER;
    fsz : INTEGER;
    isz1: INTEGER;
    isz2: INTEGER;
    dst : BOOLEAN;
    t   : node;
    i   : INTEGER;
BEGIN
  t:=find(cmd);
  IF (t=NIL) OR (t^.md#command) THEN
    error('illegal command: %s',cmd); RETURN;
  END;
  i:=0; cc:=-1; fsz:=-1; isz1:=-1; isz2:=-1; dst:=FALSE;
  WHILE t^.nm[i]#0c DO
    IF    (t^.nm[i]='%') & NOT dst THEN
      IF    cmd[i]='b' THEN isz1:=1
      ELSIF cmd[i]='w' THEN isz1:=2
      ELSE                  isz1:=4
      END;
      dst:=TRUE; INC(i);
    ELSIF (t^.nm[i]='%') & dst THEN
      IF    cmd[i]='b' THEN isz2:=1
      ELSIF cmd[i]='w' THEN isz2:=2
      ELSE                  isz2:=4
      END;
      dst:=FALSE; INC(i);
    ELSIF t^.nm[i]='$' THEN
      IF cmd[i]='l' THEN fsz:=8 ELSE fsz:=4 END; INC(i);
    ELSIF t^.nm[i]='*' THEN
      cc:=-1;
      REPEAT
        REPEAT INC(cc) UNTIL cc_mnem[cc*2]=cmd[i];
      UNTIL cc_mnem[cc*2+1]=cmd[i+1];
      INC(i,2);
    ELSE INC(i)
    END;
  END;
  command_case(t^.val,isz1,isz2,fsz,cc);
END compile_command;

PROCEDURE compile;
  VAR cmd: name; t: node; i: INTEGER;
BEGIN
  WHILE pos<BYTES(src) DO
    skip;
    ident(cmd);
    IF (cmd#'') & (src[pos]=':') THEN
      IF pass=1 THEN
        cre_node(cmd,label,code_base+put.cnt)
      ELSE
        t:=find(cmd);
        IF t=NIL THEN fault('must be defined: %s',cmd) END;
        IF t^.md#label THEN fault('must be label: %s',cmd) END;
        IF t^.val#code_base+put.cnt THEN
          t^.val:=code_base+put.cnt; INC(fail_no);
        END;
      END;
      INC(pos); skip; ident(cmd);
    END;
    skip;
    IF (cmd#'') & (src[pos]='=') THEN
      INC(pos); expression(i);
      IF pass=1 THEN cre_node(cmd,label,i) END;
    ELSIF cmd#'' THEN
      compile_command(cmd)
    END;
    skip;
    IF src[pos]=';' THEN str.search(src,pos,36c) END;
    IF src[pos]=36c THEN INC(pos);
    ELSE error('illegal text, ignored'); str.search(src,pos,36c); INC(pos);
    END;
    line_pos:=pos; INC(line);
  END;
END compile;

PROCEDURE init_mnem;
BEGIN
  cre_node('mov%'  ,command,0);
  cre_node('add%'  ,command,1);
  cre_node('addc%' ,command,2);
  cre_node('sub%'  ,command,3);
  cre_node('subc%' ,command,4);
  cre_node('cmp%'  ,command,5);
  cre_node('neg%'  ,command,6);
  cre_node('abs%'  ,command,7);
  cre_node('mul%'  ,command,8);
  cre_node('div%'  ,command,9);
  cre_node('mod%'  ,command,10);
  cre_node('quo%'  ,command,11);
  cre_node('rem%'  ,command,12);
  cre_node('and%'  ,command,13);
  cre_node('or%'   ,command,14);
  cre_node('bic%'  ,command,15);
  cre_node('xor%'  ,command,16);
  cre_node('com%'  ,command,17);
  cre_node('ash%'  ,command,18);
  cre_node('lsh%'  ,command,19);
  cre_node('rot%'  ,command,20);
  cre_node('movx%%',command,21);
  cre_node('movz%%',command,22);
  cre_node('movq%' ,command,23);
  cre_node('addq%' ,command,24);
  cre_node('cmpq%' ,command,25);
  cre_node('mei%'  ,command,26);
  cre_node('dei%'  ,command,27);
  cre_node('not%'  ,command,28);
  cre_node('s**%'  ,command,29);
  cre_node('tbit%' ,command,30);
  cre_node('sbit%' ,command,31);
  cre_node('cbit%' ,command,32);
  cre_node('sbiti%',command,33);
  cre_node('cbiti%',command,34);
  cre_node('ibit%' ,command,35);
  cre_node('cvtp'  ,command,36);
  cre_node('ffs%'  ,command,37);
  cre_node('ext%'  ,command,38);
  cre_node('ins%'  ,command,39);
  cre_node('exts%' ,command,40);
  cre_node('inss%' ,command,41);
  cre_node('movs%' ,command,42);
  cre_node('movst' ,command,43);
  cre_node('cmps%' ,command,44);
  cre_node('cmpst' ,command,45);
  cre_node('skps%' ,command,46);
  cre_node('skpst' ,command,47);
  cre_node('addp%' ,command,48);
  cre_node('subp%' ,command,49);
  cre_node('index%',command,50);
  cre_node('check%',command,51);
  cre_node('movm%' ,command,52);
  cre_node('cmpm%' ,command,53);
  cre_node('jump'  ,command,54);
  cre_node('br'    ,command,55);
  cre_node('b**'   ,command,56);
  cre_node('case%' ,command,57);
  cre_node('acb%'  ,command,58);
  cre_node('jsr'   ,command,59);
  cre_node('bsr'   ,command,60);
  cre_node('ret'   ,command,61);
  cre_node('reti'  ,command,62);
  cre_node('rett'  ,command,63);
  cre_node('save'  ,command,64);
  cre_node('restore',command,65);
  cre_node('enter' ,command,66);
  cre_node('exit'  ,command,67);
  cre_node('lpr%'  ,command,68);
  cre_node('spr%'  ,command,69);
  cre_node('adjsp%',command,70);
  cre_node('bispsr%',command,71);
  cre_node('bicpsr%',command,72);
  cre_node('setcfg',command,73);
  cre_node('nop'   ,command,74);
  cre_node('wait'  ,command,75);
  cre_node('addr'  ,command,76);
  cre_node('svc'   ,command,77);
  cre_node('flag'  ,command,78);
  cre_node('bpt'   ,command,79);
  cre_node('dia'   ,command,80);
  cre_node('mov$'  ,command,81);
  cre_node('movlf' ,command,82);
  cre_node('movfl' ,command,83);
  cre_node('mov%$' ,command,84);
  cre_node('cmp$'  ,command,85);
  cre_node('add$'  ,command,86);
  cre_node('sub$'  ,command,87);
  cre_node('mul$'  ,command,88);
  cre_node('div$'  ,command,89);
  cre_node('neg$'  ,command,90);
  cre_node('abs$'  ,command,91);
  cre_node('round$%',command,92);
  cre_node('trunc$%',command,93);
  cre_node('floor$%',command,94);
  cre_node('lfsr'  ,command,95);
  cre_node('sfsr'  ,command,96);
  cre_node('lmr'   ,command,97);
  cre_node('smr'   ,command,98);
  cre_node('rdval' ,command,99);
  cre_node('wrval' ,command,100);
  cre_node('movsu%',command,101);
  cre_node('movus%',command,102);
END init_mnem;

PROCEDURE chk_io;
BEGIN
  IF NOT bio.done THEN
    std.perror(bio.error,'"%s": %%s.\n',file_name); HALT(1)
  END;
END chk_io;

VAR
  i  : INTEGER;
  inp: bio.FILE;

BEGIN
  NEW(src); NEW(put.code,100);
  IF HIGH(arg.words)<0 THEN
    tty.print('as {<file name>}\n'); HALT;
  END;
  init_mnem;
  IF NOT arg.number('org',code_base) THEN code_base:=0 END;
  pass:=0; err_cnt:=0;
  REPEAT
    INC(pass);
    tty.print('--- pass %d ---\n',pass);
    i:=0; put.cnt:=0; fail_no:=0;
    REPEAT
      str.print(file_name,'%s.a32',arg.words[i]);
      bio.open(inp,file_name,'r'); chk_io;
      RESIZE(src,bio.eof(inp)+1);
      bio.read(inp,ADR(src),BYTES(src)-1); chk_io;
      src[HIGH(src)]:=36c;
      bio.close(inp); chk_io;
      pos:=0; line:=0; line_pos:=0;
      compile;
      INC(i);
    UNTIL i>HIGH(arg.words);
    IF err_cnt>0 THEN HALT(1) END;
  UNTIL (pass>=2) & (fail_no=0);
  str.print(file_name,'%s.exe',arg.words[0]);
  bio.create(inp,file_name,'w',put.cnt); chk_io;
  bio.write(inp,ADR(put.code),put.cnt); chk_io;
  bio.close(inp); chk_io;
  print_names;
END as.
