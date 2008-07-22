IMPLEMENTATION MODULE inFlow; (*  28-Feb-91. (c) KRONOS *)

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
    true :=NIL;
    false:=NIL;
    md   :=nm_goto;
    proc :=0;
    NEW(alts);
    NEW(ctbl);
    NEW(line);
    pos  :=0;
    len  :=0;
    tpos :=0;
    tlen :=0;
    adr  :=0;
    cnt  :=0;
    next :=NIL;
    mark :=FALSE;
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
      IF (f^.true#NIL) & (f^.next#f^.true) THEN jump(f^.true^.adr) END;
    |nm_case:
      IF ODD(vrs.scnt) THEN i:=vrs.new_str(1) END;
      i:=vrs.new_str(LEN(f^.ctbl)*2);
      ASSERT(vrs.scnt<=10000h);
      put.b(put.shift_wm1); put.b(put.md_reg+put.SI+put.s_shl);
      put.b(put.grp2_w); put.b(put.md_w+put.g2_jmp_i+put.rm_si); put.w(i);
      FOR n:=0 TO HIGH(f^.ctbl) DO
        j:=f^.ctbl[n];
        IF j<0 THEN c:=f^.false^.adr ELSE c:=f^.alts[j]^.adr END;
        vrs.scode[i]:=CHAR(c);    INC(i);
        vrs.scode[i]:=CHAR(c>>8); INC(i);
      END;
    |nm_cond:
      ASSERT(f^.true#NIL); ASSERT(f^.false#NIL);
      IF f^.false=f^.true THEN
        IF f^.next#f^.true THEN jump(f^.true^.adr) END;
      ELSIF f^.false=f^.next THEN
        n:=f^.true^.adr-f^.adr-f^.len;
        IF (n<=129) & (n>=-126) THEN
          put.b(put.jo+INTEGER(f^.flag)); put.b(n-2);
        ELSE
          put.b(put.jo+INTEGER(BITSET(f^.flag)/{0})); put.b(3);
          put.b(put.jmp); put.w(n-5);
        END;
      ELSIF f^.true=f^.next THEN
        n:=f^.false^.adr-f^.adr-f^.len;
        IF (n<=129) & (n>=-126) THEN
          put.b(put.jo+INTEGER(BITSET(f^.flag)/{0})); put.b(n-2);
        ELSE
          put.b(put.jo+INTEGER(f^.flag)); put.b(3);
          put.b(put.jmp); put.w(n-5);
        END;
      ELSE
        n:=f^.true^.adr-f^.adr-f^.len;
        IF (n<=129) & (n>=-126) THEN
          put.b(put.jo+INTEGER(f^.flag)); put.b(n-2);
          jump(f^.false^.adr);
        ELSE
          n:=f^.false^.adr-f^.adr-f^.len;
          IF (n<=129) & (n>=-126) THEN
            put.b(put.jo+INTEGER(BITSET(f^.flag)/{0})); put.b(n-2);
            jump(f^.true^.adr);
          ELSE
            -- both jump are long
            put.b(put.jo+INTEGER(f^.flag)); put.b(3);
            put.b(put.jmp); put.w(n-5);
            jump(f^.true^.adr);
          END;
        END;
      END;
    |nm_goto:
      IF (f^.true#NIL) & (f^.next#f^.true) THEN jump(f^.true^.adr) END;
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

BEGIN block:=NIL; last_line:=-1;
END inFlow.
