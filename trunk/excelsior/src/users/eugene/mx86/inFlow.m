IMPLEMENTATION MODULE inFlow; (*  28-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADR;

IMPORT  mem : pcSystem;
IMPORT  put : inCmd;
IMPORT  vrs : inVars;
IMPORT  tty : Terminal;

WITH STORAGE : mem;

VAR last_line: INTEGER;

PROCEDURE new(): node;
  VAR r: node;
BEGIN
  NEW(r);
  WITH r^ DO
    goto :=NIL;
    md   :=nm_goto;
    else :=NIL;
    NEW(line);
    pos  :=0;
    len  :=0;
    tpos :=0;
    tlen :=0;
    adr  :=0;
    cnt  :=0;
    next :=NIL;
  END;
  RETURN r;
END new;

PROCEDURE finish;
BEGIN
  IF block#NIL THEN
    block^.len:=put.cnt-block^.pos;
    block:=NIL; last_line:=-1;
  END;
END finish;

PROCEDURE start(n: node);
BEGIN
  finish; block:=n; block^.pos:=put.cnt;
END start;

PROCEDURE gen_jp(f: node): INTEGER;
  PROCEDURE jump(to: INTEGER);
  BEGIN
    to:=to-f^.adr-f^.len-put.cnt+f^.tpos;
    IF (to<=129) & (to>=-126) THEN put.b(put.jmp_s); put.b(to-2);
    ELSE put.b(put.jmp); put.w(to-3);
    END;
  END jump;
  VAR p: node; c,n,i,j: INTEGER;
BEGIN
  f^.tpos:=put.cnt;
  CASE f^.md OF
    |nm_call:
      IF vrs.prcs[f^.proc].mod#0 THEN
        i:=vrs.prcs[f^.proc].ofs; ASSERT(i<=0FFFCh);
        put.b(put.grp2_w); put.b(put.g2_call_ii+put.md_abs); put.w(i);
      ELSIF vrs.prcs[f^.proc].export THEN
        i:=vrs.prcs[f^.proc].no;
        n:=proc_flow[i]^.adr-f^.adr-f^.len;
        put.b(put.push_cs); put.b(put.call); put.w(n-4);
      ELSE
        i:=vrs.prcs[f^.proc].no;
        n:=proc_flow[i]^.adr-f^.adr-f^.len;
        put.b(put.call); put.w(n-3);
      END;
      IF (f^.goto#NIL) & (f^.next#f^.goto) THEN jump(f^.goto^.adr) END;
    |nm_case:
      IF ODD(vrs.scnt) THEN i:=vrs.new_str(1) END;
      i:=vrs.new_str(LEN(f^.ctbl)*2);
      ASSERT(vrs.scnt<=10000h);
      put.b(put.shift_wm1); put.b(put.md_reg+put.SI+put.s_shl);
      put.b(put.grp2_w); put.b(put.md_w+put.g2_jmp_i+put.rm_si); put.w(i);
      FOR n:=0 TO HIGH(f^.ctbl) DO
        c:=f^.ctbl[n]^.adr;
        vrs.scode[i]:=CHAR(c);    INC(i);
        vrs.scode[i]:=CHAR(c>>8); INC(i);
      END;
    |nm_cond:
      ASSERT(f^.goto#NIL); ASSERT(f^.else#NIL);
      IF f^.else=f^.goto THEN
        IF f^.next#f^.goto THEN jump(f^.goto^.adr) END;
      ELSIF f^.else=f^.next THEN
        n:=f^.goto^.adr-f^.adr-f^.len;
        IF (n<=129) & (n>=-126) THEN
          put.b(put.jo+INTEGER(f^.flag)); put.b(n-2);
        ELSE
          put.b(put.jo+INTEGER(BITSET(f^.flag)/{0})); put.b(3);
          put.b(put.jmp); put.w(n-5);
        END;
      ELSIF f^.goto=f^.next THEN
        n:=f^.else^.adr-f^.adr-f^.len;
        IF (n<=129) & (n>=-126) THEN
          put.b(put.jo+INTEGER(BITSET(f^.flag)/{0})); put.b(n-2);
        ELSE
          put.b(put.jo+INTEGER(f^.flag)); put.b(3);
          put.b(put.jmp); put.w(n-5);
        END;
      ELSE
        n:=f^.goto^.adr-f^.adr-f^.len;
        IF (n<=129) & (n>=-126) THEN
          put.b(put.jo+INTEGER(f^.flag)); put.b(n-2);
          jump(f^.else^.adr);
        ELSE
          n:=f^.else^.adr-f^.adr-f^.len;
          IF (n<=129) & (n>=-126) THEN
            put.b(put.jo+INTEGER(BITSET(f^.flag)/{0})); put.b(n-2);
            jump(f^.goto^.adr);
          ELSE
            -- both jump are long
            put.b(put.jo+INTEGER(f^.flag)); put.b(3);
            put.b(put.jmp); put.w(n-5);
            jump(f^.goto^.adr);
          END;
        END;
      END;
    |nm_goto:
      IF (f^.goto#NIL) & (f^.next#f^.goto) THEN jump(f^.goto^.adr) END;
    |nm_ext_call:
      put.b(9Ah); put.w(0); put.w(0);
      IF (f^.goto#NIL) & (f^.next#f^.goto) THEN jump(f^.goto^.adr) END;
  END;
  RETURN put.cnt-f^.tpos;
END gen_jp;

PROCEDURE mark(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=n DIV 1000h+1;
  IF i=last_line THEN RETURN END;
  last_line:=i;
  RESIZE(block^.line,LEN(block^.line)+1);
  IF LEN(block^.line)>1 THEN i:=i+(put.cnt-block^.pos)<<16 END;
  block^.line[HIGH(block^.line)]:=i;
END mark;

PROCEDURE external_call(pos: INTEGER; VAL nm: ARRAY OF CHAR);
  VAR t: POINTER TO ext_name; i: INTEGER;
BEGIN
  block^.md:=nm_ext_call;
  block^.goto:=new();
  t:=ADR(enm_tree);
  LOOP
    IF t^=NIL THEN
      NEW(t^);
      t^^.up:=NIL;
      t^^.dw:=NIL;
      t^^.index:=0;
      i:=0;
      WHILE (i<=HIGH(nm)) & (nm[i]#0c) DO INC(i) END;
      NEW(t^^.nm,i+1);
      i:=0;
      WHILE (i<=HIGH(nm)) & (nm[i]#0c) DO t^^.nm[i]:=nm[i]; INC(i) END;
      t^^.nm[i]:=0c;
      EXIT;
    END;
    IF t^^.nm=nm THEN EXIT END;
    IF nm>t^^.nm THEN t:=ADR(t^^.up) ELSE t:=ADR(t^^.dw) END;
  END;
  block^.name:=t^;
  start(block^.goto);
  mark(pos);
END external_call;

BEGIN block:=NIL; last_line:=-1; enm_tree:=NIL;
END inFlow.
