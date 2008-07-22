IMPLEMENTATION MODULE regExpr; (* Leo 07-Oct-89. (c) KRONOS *)

IMPORT  mem: Heap;
IMPORT  def: defErrors;


WITH STORAGE  ( NEW    : mem.  allocate;
                DISPOSE: mem.deallocate;
                RESIZE : mem.reallocate );


CONST
  str = 0;
  any = 1; -- "?"
  set = 2; -- "[]"
  bra = 3; -- "{}"
  seq = 4; -- "*"
  and = 5; -- "&"
  or  = 6; -- "|"
  not = 7; -- "^"
  par = 8; -- "()"

  ok = def.ok;

TYPE
  EXPR      = POINTER TO reg_node;

  char_set  = POINTER TO ARRAY [0..7] OF BITSET;
  RESULT    = RECORD pos,len: ARRAY [0..9] OF INTEGER END;
  reg_node  = RECORD
                next: EXPR;
                nres: INTEGER;
                res : POINTER TO RESULT;
                CASE kind: INTEGER OF
                |any,seq,not,par: (* next=arg for not,bra,par *)
                |and,or :           left: EXPR; (* right=next *)
                |str    :           pat : STRING;
                |set,bra:           leg : char_set;
                END
              END;

PROCEDURE dispose(VAR reg: EXPR);
  VAR n: EXPR;
BEGIN
  IF (reg#NIL) & (reg^.res#NIL) THEN DISPOSE(reg^.res) END;
  WHILE reg#NIL DO
    n:=reg^.next;
    IF     reg^.kind IN {and,or}               THEN dispose(reg^.left)
    ELSIF (reg^.kind=str) & (SIZE(reg^.pat)>0) THEN DISPOSE(reg^.pat )
    ELSIF (reg^.kind=set) & (reg^.leg#NIL)     THEN DISPOSE(reg^.leg )
    END;
    DISPOSE(reg);
    reg:=n
  END
END dispose;

PROCEDURE pars_reg(VAL expr: ARRAY OF CHAR; VAR reg: EXPR;
                   VAR    i: INTEGER;
                   VAR  err: INTEGER);

  PROCEDURE app(VAR reg: EXPR; r: EXPR);
    VAR t: EXPR;
  BEGIN
    IF reg=NIL THEN reg:=r; RETURN END;
    t:=reg;
    WHILE t^.next#NIL DO t:=t^.next END;
    IF (t^.kind=seq) & (r^.kind=seq) THEN
      epos:=i; err:=def.bad_parm; RETURN
    ELSE
      t^.next:=r
    END;
  END app;

  PROCEDURE new(kind: INTEGER): EXPR;
    VAR n: EXPR;
  BEGIN
    NEW(n);
    IF n=NIL THEN err:=def.no_memory; epos:=i; RETURN NIL END;
    n^.kind:=kind;
    n^.next:=NIL;
    n^.res :=NIL;
    n^.nres:=-1;
    (* .leg, .pat and .left are overlay and so, can't have legal init value *)
    RETURN n
  END new;

  PROCEDURE app_new(VAR reg: EXPR; kind: INTEGER): EXPR;
    VAR n: EXPR;
  BEGIN
    n:=new(kind);
    IF err#ok THEN RETURN NIL END;
    app(reg,n);
    IF err#ok THEN dispose(n); RETURN NIL END;
    RETURN n
  END app_new;

  PROCEDURE dollar(n: EXPR);
    VAR ch: CHAR;
  BEGIN
    IF err#ok THEN RETURN END;
    IF (i>=HIGH(expr)) OR (expr[i]#'$') THEN RETURN END;
    ch:=expr[i+1];
    IF ("0"<=ch) & (ch<="9") THEN
      n^.nres:=ORD(ch)-ORD("0"); INC(i,2)
    ELSE
      epos:=i; err:=def.bad_parm
    END
  END dollar;

  PROCEDURE char_code(c: CHAR; VAR j: INTEGER): CHAR;
    VAR n: INTEGER;
  BEGIN
    c:=CHAR(ORD(c)-ORD('0'));
    FOR n:=0 TO 1 DO
      IF (j<=HIGH(expr)) & ('0'<=expr[j]) & (expr[j]<='7') THEN
        c:=CHAR( ORD(c)*8+ORD(expr[j])-ORD('0') );
        INC(j)
      ELSE
        epos:=j; err:=def.bad_parm; RETURN c
      END;
    END;
    RETURN c
  END char_code;

  PROCEDURE esc(VAR j: INTEGER): CHAR;
    VAR c: CHAR;
  BEGIN
    INC(j); c:=expr[j]; INC(j);
    CASE c OF
    |'n': RETURN 36c    |'e': RETURN 33c
    |'r': RETURN 15c    |'g': RETURN 07c
    |'l': RETURN 12c
    ELSE
      IF ('0'<=c) & (c<='7') THEN RETURN char_code(c,j)
      ELSE                        RETURN c
      END;
    END;
  END esc;

  PROCEDURE fill_set(n: EXPR);

    VAR from: CHAR;
       range: BOOLEAN;

    PROCEDURE incl(ch: CHAR);
      VAR c: CHAR;
    BEGIN
      IF NOT range THEN
        INCL(n^.leg^[ORD(ch) DIV 32],ORD(ch) MOD 32); RETURN
      END;
      IF from>ch THEN epos:=i; err:=def.bad_parm; RETURN END;
      FOR c:=from TO ch DO
        INCL(n^.leg^[ORD(c) DIV 32],ORD(c) MOD 32);
      END;
      range:=FALSE;
    END incl;

    VAR j: INTEGER;
     q,ch: CHAR;
      inv: BOOLEAN;

  BEGIN
    IF err#ok THEN RETURN END;
    NEW(n^.leg);
    IF n^.leg=NIL THEN err:=def.no_memory; RETURN END;
    FOR j:=0 TO HIGH(n^.leg^) DO n^.leg^[j]:={} END;
    range:=FALSE;
    IF expr[i]='[' THEN q:=']' ELSE q:='}'; n^.kind:=bra END;
    INC(i);
    inv:=(i<=HIGH(expr)) & (expr[i]="^");
    IF inv THEN INC(i) END;
    IF (i<=HIGH(expr)) & (expr[i]=q) THEN
      epos:=i; err:=def.bad_parm; RETURN
    END;
    WHILE (i<=HIGH(expr)) & (expr[i]#q) & (expr[i]#0c) & (err=ok) DO
      IF (expr[i]='\') & (i<HIGH(expr)) & (expr[i+1]#0c) THEN
        ch:=esc(i);
        incl(ch);
      ELSIF (expr[i]='-') & (i<HIGH(expr)) & (expr[i+1]#q) THEN
        from:=ch;    (* save pred char *)
        range:=TRUE; (* next char will be right bound of the range *)
        INC(i);
      ELSE
        ch:=expr[i];
        incl(ch);
        INC(i);
      END;
    END;
    IF err#ok THEN RETURN END;
    IF (i>HIGH(expr)) OR (expr[i]#q) OR range THEN
      epos:=i; err:=def.bad_parm
    ELSE
      INC(i);
      IF NOT inv THEN RETURN END;
      FOR j:=0 TO HIGH(n^.leg^) DO n^.leg^[j]:=n^.leg^[j]/{0..31} END;
    END;
  END fill_set;

  PROCEDURE fill_str(n: EXPR);

    PROCEDURE scan(put: BOOLEAN): INTEGER;
      VAR ch: CHAR;
         c,j: INTEGER;
    BEGIN
      j:=i; c:=0;
      WHILE j<=HIGH(expr) DO
        ch:=expr[j];
        IF (ch= 0c) OR (ch='[') OR (ch='*') OR (ch='?') OR (ch='{')
        OR (ch=')') OR (ch='&') OR (ch='|') OR (ch='^') OR (ch='$')
        THEN
          IF put THEN i:=j END;
          RETURN c
        ELSIF (ch='\') & (j<HIGH(expr)) & (expr[j+1]#0c) THEN
          ch:=esc(j);
          IF put THEN n^.pat[c]:=ch END;
          INC(c);
        ELSE
          IF put THEN n^.pat[c]:=expr[j] END;
          INC(c);
          INC(j);
        END;
      END;
      IF put THEN i:=j END;
      RETURN c
    END scan;

  BEGIN
    IF err#ok THEN RETURN END;
    NEW(n^.pat,scan(FALSE)+1);
    IF NOT mem.done THEN err:=def.no_memory; RETURN END;
    n^.pat[scan(TRUE)]:=0c;
  END fill_str;

  PROCEDURE simple(VAR reg: EXPR): INTEGER;
    VAR n: EXPR;
       ch: CHAR;
  BEGIN
    IF err#ok THEN RETURN err END;
    IF (i>HIGH(expr)) OR (expr[i]=0c) THEN epos:=i; RETURN def.bad_parm END;
    LOOP
      IF (err#ok) OR (i>HIGH(expr)) THEN EXIT END;
      ch:=expr[i];
      IF (ch= 0c) OR (ch=')') OR (ch='(')
      OR (ch='|') OR (ch='&') OR (ch='^') THEN EXIT END;
      IF    ch='*' THEN n:=app_new(reg,seq); INC(i); dollar(n)
      ELSIF ch='?' THEN n:=app_new(reg,any); INC(i); dollar(n)
      ELSIF ch='{' THEN n:=app_new(reg,set); fill_set(n); dollar(n)
      ELSIF ch='[' THEN n:=app_new(reg,set); fill_set(n); dollar(n)
      ELSE
        n:=app_new(reg,str); fill_str(n); dollar(n)
      END
    END;
    IF err#ok THEN dispose(reg) END;
    RETURN err
  END simple;

  PROCEDURE re(VAR reg: EXPR): INTEGER; FORWARD;

  PROCEDURE factor(VAR reg: EXPR): INTEGER;
  BEGIN
    reg:=NIL;
    IF err#ok THEN RETURN err END;
    IF (i>HIGH(expr)) OR (expr[i]=0c) THEN epos:=i; RETURN def.bad_parm END;
    IF expr[i]='(' THEN INC(i);
      reg:=new(par);
      IF err#ok THEN RETURN err END;
      err:=re(reg^.next);
      IF err#ok THEN RETURN err END;
      IF expr[i]=')' THEN INC(i); dollar(reg)
      ELSE epos:=i; err:=def.bad_parm
      END;
    ELSIF expr[i]='^' THEN
      INC(i);
      reg:=new(not);
      IF err#ok THEN RETURN err END;
      err:=factor(reg^.next);
      IF err#ok THEN RETURN err END;
      IF reg^.next^.nres>=0 THEN
        reg^.nres:=reg^.next^.nres; reg^.next^.nres:=-1
      END
    ELSE
      err:=simple(reg)
    END;
    IF err#ok THEN dispose(reg) END;
    RETURN err
  END factor;

  PROCEDURE term(VAR reg: EXPR): INTEGER;
    VAR t: EXPR;
  BEGIN
    reg:=NIL;
    IF err#ok THEN RETURN err END;
    IF (i>HIGH(expr)) OR (expr[i]=0c) THEN epos:=i; RETURN def.bad_parm END;
    err:=factor(reg);
    IF (i<HIGH(expr)) & (expr[i]='&') & (err=ok) THEN
      t:=new(and);      INC(i);
      IF err=ok THEN t^.left:=reg; reg:=t; err:=term(t^.next) END
    END;
    IF err#ok THEN dispose(reg) END;
    RETURN err
  END term;

  PROCEDURE re(VAR reg: EXPR): INTEGER;
    VAR t: EXPR;
  BEGIN
    reg:=NIL;
    IF err#ok THEN RETURN err END;
    IF (i>HIGH(expr)) OR (expr[i]=0c) THEN epos:=i; RETURN def.bad_parm END;
    err:=term(reg);
    IF (i<HIGH(expr)) & (expr[i]='|') & (err=ok) THEN
      t:=new(or);       INC(i);
      IF err=ok THEN t^.left:=reg; reg:=t; err:=re(t^.next) END
    END;
    IF err#ok THEN dispose(reg) END;
    RETURN err
  END re;

BEGIN
  reg:=NIL;
  IF (i>HIGH(expr)) OR (expr[i]=0c) THEN
    epos:=i; err:=def.bad_parm; RETURN
  END;
  err:=re(reg);
  IF err#ok THEN dispose(reg) END;
  RETURN
END pars_reg;

PROCEDURE compile(VAL expr: ARRAY OF CHAR; VAR reg: EXPR);
  VAR i,err: INTEGER;
BEGIN
  done:=TRUE; err:=ok;
  i:=0;
  pars_reg(expr,reg,i,err);
  IF err#ok THEN done:=FALSE; error:=err; RETURN END;
  IF (i<=HIGH(expr)) & (expr[i]#0c) THEN
    epos:=i; err:=def.bad_parm; dispose(reg)
  ELSE
    NEW(reg^.res);
    IF reg^.res#NIL THEN
      FOR i:=0 TO HIGH(reg^.res^.len) DO reg^.res^.len[i]:=0 END;
      reg^.res^.pos:=reg^.res^.len
    ELSE
      err:=def.no_memory; dispose(reg)
    END
  END;
  IF err#ok THEN done:=FALSE; error:=err END
END compile;

PROCEDURE const(re: EXPR): BOOLEAN;
BEGIN
  ASSERT(re#NIL,4Fh);
  WHILE re^.kind=par DO re:=re^.next END;
  RETURN (re^.kind=str) & (re^.next=NIL)
END const;

PROCEDURE match0(reg: EXPR;     VAL    s: ARRAY OF CHAR;
                   p: INTEGER;  VAR stop: INTEGER; VAR rs: RESULT
                ): BOOLEAN;

  VAR n: INTEGER; (* reg^.nres *)

  PROCEDURE bra_seq_end(): BOOLEAN;
  BEGIN
    WHILE (reg#NIL) & (reg^.kind IN {bra,seq}) DO reg:=reg^.next END;
    IF reg#NIL THEN RETURN FALSE END;
    IF n>=0 THEN rs.len[n]:=p-rs.pos[n] END; stop:=p;
    RETURN TRUE
  END bra_seq_end;

  VAR i: INTEGER;

BEGIN
  stop:=p;
  IF reg=NIL THEN RETURN (p>HIGH(s)) OR (s[p]=0c) END;
  n:=reg^.nres;
  IF (p>HIGH(s)) OR (s[p]=0c) THEN
    RETURN (reg=NIL) OR ((reg^.kind IN {seq,bra}) & (reg^.next=NIL))
  END;
  IF reg^.kind=any THEN
    IF n>=0 THEN rs.pos[n]:=p; rs.len[n]:=1 END;
    RETURN match0(reg^.next,s,p+1,stop,rs);
  ELSIF reg^.kind=seq THEN
    IF n>=0 THEN rs.pos[n]:=p END;
    WHILE (p<=HIGH(s)) & (s[p]#0c) DO
      IF match0(reg^.next,s,p,stop,rs) THEN
        IF n>=0 THEN rs.len[n]:=p-rs.pos[n] END;
        RETURN TRUE
      END;
      p:=p+1
    END;
    RETURN bra_seq_end()
  ELSIF reg^.kind=set THEN
    i:=ORD(s[p]);
    IF NOT (i MOD 32 IN reg^.leg^[i DIV 32]) THEN RETURN FALSE END;
    IF n>=0 THEN rs.pos[n]:=p; rs.len[n]:=1 END;
    RETURN match0(reg^.next,s,p+1,stop,rs);
  ELSIF reg^.kind=bra THEN
    IF n>=0 THEN rs.pos[n]:=p END;
    WHILE (p<HIGH(s)) & (s[p]#0c) DO
      IF match0(reg^.next,s,p,stop,rs) THEN
        IF n>=0 THEN rs.len[n]:=p-rs.pos[n] END;
        RETURN TRUE
      END;
      i:=ORD(s[p]);
      IF NOT (i MOD 32 IN reg^.leg^[i DIV 32]) THEN RETURN FALSE END;
      p:=p+1
    END;
    RETURN bra_seq_end()
  ELSIF reg^.kind=str THEN
    IF n>=0 THEN rs.pos[n]:=p END;
    FOR i:=0 TO HIGH(reg^.pat)-1 DO
      IF s[p]#reg^.pat[i] THEN RETURN FALSE END;
      INC(p)
    END;
    IF n>=0 THEN rs.len[n]:=p-rs.pos[n] END;
    RETURN match0(reg^.next,s,p,stop,rs);
  ELSIF reg^.kind=and THEN
    RETURN match0(reg^.left,s,p,stop,rs)
         & match0(reg^.next,s,p,stop,rs)
  ELSIF reg^.kind=or THEN
    RETURN match0(reg^.left,s,p,stop,rs)
        OR match0(reg^.next,s,p,stop,rs)
  ELSIF reg^.kind=not THEN
    IF n>=0 THEN rs.pos[n]:=p END;
    IF match0(reg^.next,s,p,stop,rs) THEN
      RETURN FALSE
    ELSE
      WHILE (p<HIGH(s)) & (s[p]#0c) DO INC(p) END; stop:=p;
      IF n>=0 THEN rs.len[n]:=stop-rs.pos[n] END;
      RETURN TRUE
    END
  ELSIF reg^.kind=par THEN
    IF n>=0 THEN rs.pos[n]:=p END;
    IF match0(reg^.next,s,p,stop,rs) THEN
      IF n>=0 THEN rs.len[n]:=stop-rs.pos[n] END;
      RETURN TRUE
    END;
    RETURN FALSE
  ELSE
    ASSERT(FALSE,4Fh)
  END
END match0;

PROCEDURE match(re: EXPR; VAL s: ARRAY OF CHAR; pos: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  ASSERT(re#NIL,4Fh);
  done:=TRUE;
  FOR i:=0 TO HIGH(re^.res^.len) DO re^.res^.len[i]:=0 END;
  re^.res^.pos:=re^.res^.len;
  RETURN match0(re,s,pos,i,re^.res^)
END match;

PROCEDURE substitute(re   : EXPR;
                     VAL s: ARRAY OF CHAR;
                         m: ARRAY OF CHAR;
                     VAR d: ARRAY OF CHAR);
(* NB: 'm' cant be VAL because 'm' & 'd' may be the same variable *)
  VAR i,j,k,n,l: INTEGER;
BEGIN
  ASSERT(re#NIL,4Fh);
  done:=TRUE;
  i:=0; j:=0;
  IF j>HIGH(d) THEN RETURN END;
  WHILE (i<=HIGH(m)) & (m[i]#0c) DO
    IF j=HIGH(d) THEN d[j]:=0c; RETURN END;
    IF (m[i]='\') & (i<HIGH(m)) & (m[i+1]='$') THEN
      d[j]:='$'; INC(i,2); INC(j)
    ELSIF (m[i]='$') & (i<HIGH(m)) & (ORD(m[i+1])-ORD("0") IN {0..9}) THEN
      n:=ORD(m[i+1])-ORD("0");
      k:=re^.res^.pos[n];
      l:=re^.res^.len[n];
      IF j+l  >HIGH(d) THEN l:=HIGH(d)-j   END;
      IF k+l-1>HIGH(s) THEN l:=HIGH(s)-k+1 END;
      FOR k:=k TO k+re^.res^.len[n]-1 DO d[j]:=s[k]; INC(j) END;
      INC(i,2)
    ELSE
      d[j]:=m[i]; INC(i); INC(j)
    END
  END;
  IF j<=HIGH(d) THEN d[j]:=0c END
END substitute;

PROCEDURE len(re: EXPR; n: INTEGER): INTEGER;
BEGIN
  ASSERT(re#NIL,4Fh);
  done:=TRUE;
  RETURN re^.res^.len[n]
END len;

PROCEDURE pos(re: EXPR; n: INTEGER): INTEGER;
BEGIN
  ASSERT(re#NIL,4Fh);
  done:=TRUE;
  RETURN re^.res^.pos[n]
END pos;

BEGIN
  null:=NIL; error:=0; done:=TRUE; epos:=0
END regExpr.

----------------------------- DEBUG ----------------------------
                             -------

PROCEDURE show_reg(reg: EXPR);

  PROCEDURE show(VAL s: ARRAY OF CHAR; SEQ d: INTEGER);
  BEGIN
    tty.set_something(1);
    tty.print("%s",s);
    IF (HIGH(d)>=0) & (d[0]>=0) THEN tty.print("$%d",d[0]) END;
    tty.set_something(0);
  END show;

  VAR i: INTEGER;
BEGIN
  ASSERT(reg#NIL,4Fh);
  done:=TRUE;
  tty.set_something(0);
  WHILE reg#NIL DO
    IF    reg^.kind=any THEN show("?",reg^.nres)
    ELSIF reg^.kind=seq THEN show("*",reg^.nres);
    ELSIF reg^.kind IN {set,bra} THEN
      IF reg^.kind=set THEN show("[")
      ELSE                  show("{")
      END;
      FOR i:=0 TO 255 DO
        IF i MOD 32 IN reg^.leg^[i DIV 32] THEN
          tty.print("%c",i);
        END;
      END;
      IF reg^.kind=set THEN show("]",reg^.nres)
      ELSE                  show("}",reg^.nres)
      END;
    ELSIF reg^.kind=par THEN
      show("("); show_reg(reg^.next); show(")",reg^.nres); RETURN
    ELSIF reg^.kind=not THEN
      show("^("); show_reg(reg^.next); show(")",reg^.nres); RETURN
    ELSIF reg^.kind=and THEN
      show("("); show_reg(reg^.left); show(")&(");
      show_reg(reg^.next); show(")",reg^.nres); RETURN
    ELSIF reg^.kind=or THEN
      show("("); show_reg(reg^.left); show(")|(");
      show_reg(reg^.next); show(")",reg^.nres); RETURN
    ELSIF reg^.kind=str THEN
      IF reg^.nres>=0 THEN
        show("(");
        tty.print("%s",reg^.pat);
        show(")",reg^.nres);
      ELSE
        tty.print("%s",reg^.pat);
      END;
    ELSE
      ASSERT(FALSE)
    END;
    reg:=reg^.next;
  END
END show_reg;
