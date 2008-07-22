MODULE lrk; (* 19-Sep-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR;
FROM StdIO      IMPORT  print, GetS, StdIn;
FROM Strings    IMPORT  GetWord, Str2;
FROM Image      IMPORT  image0;
FROM Heap       IMPORT  ALLOCATE;
IMPORT tty : Terminal;
IMPORT mcd : mCodeMnem;

CONST
  char_limit   = 320;
  rule_limit   = 500;
  bits_no_low  = (char_limit-1) MOD 100h;
  bits_no_high = (char_limit-1) DIV 100h;

TYPE
  string = ARRAY [0..63] OF CHAR;
  set    = ARRAY [0..(char_limit+31) DIV 32 -1] OF BITSET;
  char   = [0..char_limit-1];
  chain  = ARRAY [0..11] OF char;
  rule   = RECORD
    l : char;
    r : chain;
  END;
  rule_ref  = [0..rule_limit-1];
  situation = RECORD
    rl : rule_ref;
    pt : INTEGER; -- позиция точки
    u  : set;
  END;
  st_arr=ARRAY [0..599] OF situation;

CONST
  char_null = 0;
  rule_null = 0;

VAR
  char_name : ARRAY char OF string;
  char_rule : ARRAY char OF rule_ref;
  term_no   : char;
  char_no   : char;
  P         : ARRAY rule_ref OF rule; -- правила грамматики
  P_nxt     : ARRAY rule_ref OF rule_ref;
  P_sz      : INTEGER; -- кол-во правил в грамматике
  S         : char; -- аксиома
  sys       : ARRAY [0..3999] OF POINTER TO st_arr;
  sys_sz    : ARRAY [0..3999] OF INTEGER;
  sys_no    : INTEGER;

PROCEDURE move(a,b: ADDRESS; c: INTEGER); CODE mcd.move END move;

PROCEDURE inl(i: INTEGER; VAL n: set): BOOLEAN;
CODE mcd.lid bits_no_low bits_no_high mcd.inl END inl;

PROCEDURE incl(VAR n: set; i: char);
CODE
  mcd.lid bits_no_low bits_no_high
  mcd.chkz mcd.incl
END incl;

PROCEDURE excl(VAR n: set; i: char);
CODE
  mcd.lid bits_no_low bits_no_high
  mcd.chkz mcd.excl
END excl;

PROCEDURE set_zero(VAR x: set);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(x) DO x[i]:={} END;
END set_zero;

PROCEDURE and_not(VAR x,y: set): BOOLEAN;
  VAR i: char;
BEGIN
  FOR i:=0 TO char_no-1 DO
    IF inl(i,x) & inl(i,y) THEN RETURN FALSE END;
  END;
  RETURN TRUE;
END and_not;

PROCEDURE print_set(VAR s: set);
  VAR i: char;
BEGIN
  print('------------- set ----------\n');
  FOR i:=0 TO char_no-1 DO
    IF inl(i,s) THEN print('  %s\n',char_name[i]) END;
  END;
  print('----------- end set --------\n');
END print_set;

PROCEDURE find(s: ARRAY OF CHAR): char;
  VAR p,e: POINTER TO string;
BEGIN
  p:=ADR(char_name); e:=ADR(char_name[char_no]);
  IF p=e THEN RETURN char_null END;
  REPEAT
    IF p^=s THEN RETURN INTEGER(p-ADR(char_name)) DIV SIZE(string) END;
    p:=ADDRESS(p)+SIZE(string);
  UNTIL p=e;
  RETURN char_null;
END find;

PROCEDURE print_rule(n: rule_ref);
  VAR j: INTEGER;
BEGIN
  print('%s ::=',char_name[P[n].l]);
  j:=0;
  WHILE P[n].r[j]#char_null DO print(' %s',char_name[P[n].r[j]]) END;
  print('\n');
END print_rule;

PROCEDURE read;
  VAR w: string; ln: ARRAY [0..255] OF CHAR; l,c,k: INTEGER;
BEGIN
  l:=-1;
  LOOP
    ASSERT(GetS(StdIn,ln)>0); INC(l); Str2(ln);
    GetWord(ln,w);
    IF w='$'0c THEN EXIT
    ELSIF w='*'0c THEN term_no:=char_no;
    ELSE
      REPEAT
        IF w='' THEN print('error line %d\n',l); HALT END;
        IF find(w)#char_null THEN
          print('line %d, double def %s\n',l,w)
        ELSE
          char_name[char_no]:=w;
          char_rule[char_no]:=rule_null;
          INC(char_no);
        END;
        GetWord(ln,w);
      UNTIL w='';
    END;
  END;
  LOOP
    ASSERT(GetS(StdIn,ln)>0); INC(l); Str2(ln);
    GetWord(ln,w);
    IF w='$'0c THEN EXIT END;
    k:=0;
    REPEAT
      IF w='char_null' THEN
        c:=char_null
      ELSE
        c:=find(w);
        IF c=char_null THEN
          print('%-40s is not def\n',w);
          char_name[char_no]:=w;
          char_rule[char_no]:=rule_null;
          c:=char_no; INC(char_no);
        END;
      END;
      IF k=0 THEN
        IF c<term_no THEN
          print('line %d, rule for terminal char\n',l);
        END;
        P_nxt[P_sz]:=char_rule[c];
        char_rule[c]:=P_sz;
        P[P_sz].l:=c;
      ELSE
        P[P_sz].r[k-1]:=c;
      END;
      INC(k); GetWord(ln,w);
    UNTIL w='';
    IF k<2 THEN print('error rule, line %d\n',l); HALT END;
    FOR l:=k-1 TO HIGH(P[P_sz].r) DO P[P_sz].r[l]:=char_null END;
    INC(P_sz);
  END;
  ASSERT(GetS(StdIn,ln)>0); INC(l); Str2(ln);
  GetWord(ln,w);
  S:=find(w); -- аксиома
  FOR c:=term_no TO char_no-1 DO
    IF char_rule[c]=rule_null THEN
      print('%-40s no rule for this char\n',char_name[c])
    END;
  END;
  tty.print('read: rules %d, chars %d\n',P_sz,char_no);
END read;

PROCEDURE FIRST(a: char; VAR x: set);
  VAR p: set;
  PROCEDURE one(c: char);
    VAR r: rule_ref;
  BEGIN
    IF c<term_no THEN
      incl(x,c);
    ELSE
      IF inl(c,p) THEN RETURN END;
      incl(p,c);
      r:=char_rule[c];
      REPEAT
        ASSERT(P[r].l=c);
        one(P[r].r[0]); r:=P_nxt[r];
      UNTIL r=rule_null;
    END;
  END one;
BEGIN
  set_zero(p); set_zero(x); one(a);
END FIRST;

PROCEDURE EFF(a: char; VAR x: set);
BEGIN
  FIRST(a,x);
END EFF;

PROCEDURE GOTO(VAR st: ARRAY OF situation; VAR st_no: INTEGER; xx: char);
  PROCEDURE insert(r: rule_ref; VAL x: set);
    VAR i,j: INTEGER;
  BEGIN
    FOR i:=0 TO st_no-1 DO
      IF (st[i].rl=r) & (st[i].pt=0) THEN
        FOR j:=0 TO HIGH(x) DO st[i].u[j]:=st[i].u[j]+x[j] END;
        RETURN;
      END;
    END;
    st[st_no].rl:=r; st[st_no].pt:=0; st[st_no].u:=x; INC(st_no);
  END insert;
  VAR i: INTEGER; x,c: char; st1_no: INTEGER; xs: set; r: rule_ref;
BEGIN
  st1_no:=st_no; st_no:=0;
  FOR i:=0 TO st1_no-1 DO
    IF P[st[i].rl].r[st[i].pt]=xx THEN
      st[st_no]:=st[i]; INC(st[st_no].pt); INC(st_no);
    END;
  END;
  i:=0;
  WHILE i<st_no DO
    c:=P[st[i].rl].r[st[i].pt];
    IF c>=term_no THEN
      x:=P[st[i].rl].r[st[i].pt+1];
      IF x=char_null THEN xs:=st[i].u ELSE FIRST(x,xs) END;
      r:=char_rule[c];
      REPEAT
        ASSERT(P[r].l=c);
        insert(r,xs); r:=P_nxt[r];
      UNTIL r=rule_null;
    END;
    INC(i);
  END;
END GOTO;

PROCEDURE print_situation(VAL s: situation);
  VAR j: INTEGER; x: char;
BEGIN
  print('[ %s ::=',char_name[P[s.rl].l]);
  j:=0;
  WHILE P[s.rl].r[j]#char_null DO
    IF j=s.pt THEN print(' .') END;
    print(' %s',char_name[P[s.rl].r[j]]);
    INC(j);
  END;
  IF j=s.pt THEN print(' .') END;
  print(';');
  FOR x:=0 TO char_no-1 DO
    IF inl(x,s.u) THEN print(' %s',char_name[x]) END;
  END;
  print(' ]\n');
END print_situation;

PROCEDURE situations_for_chain(VAL s: chain; l: INTEGER;
                              VAR st: ARRAY OF situation;
                              VAR st_no: INTEGER);
-- алгоритм 5.8 ахоульмана

  PROCEDURE insert(r: rule_ref; VAL x: set);
    VAR i,j: INTEGER;
  BEGIN
    FOR i:=0 TO st_no-1 DO
      IF (st[i].rl=r) & (st[i].pt=0) THEN
        FOR j:=0 TO HIGH(x) DO st[i].u[j]:=st[i].u[j]+x[j] END;
        RETURN;
      END;
    END;
    st[st_no].rl:=r; st[st_no].pt:=0; st[st_no].u:=x; INC(st_no);
  END insert;
  VAR
    r     : rule_ref;
    c,x   : char;
    xs    : set;
    i,j   : INTEGER;
    st1_no: INTEGER;
BEGIN
  st_no:=0;
  IF l=0 THEN
    r:=char_rule[S];
    WHILE r#rule_null DO
      WITH st[st_no] DO rl:=r; pt:=0; set_zero(u); u[0]:={0} END; INC(st_no);
      r:=P_nxt[r];
    END;
    i:=0;
    WHILE i<st_no DO
      c:=P[st[i].rl].r[0];
      IF c>=term_no THEN
        x:=P[st[i].rl].r[1];
        IF x=char_null THEN xs:=st[i].u ELSE FIRST(x,xs) END;
        r:=char_rule[c];
        REPEAT
          ASSERT(P[r].l=c);
          insert(r,xs); r:=P_nxt[r];
        UNTIL r=rule_null;
      END;
      INC(i);
    END;
  ELSE
    situations_for_chain(s,l-1,st,st_no);
    GOTO(st,st_no,s[l-1]);
  END;
END situations_for_chain;

PROCEDURE check_situation_set(VAR st: ARRAY OF situation; st_no: INTEGER);
  VAR i,j,k: INTEGER; xs: set;
BEGIN
  FOR i:=0 TO st_no-1 DO
    FOR j:=i+1 TO st_no-1 DO
      IF P[st[i].rl].r[st[i].pt]=char_null THEN
        IF P[st[j].rl].r[st[j].pt]=char_null THEN
          xs:=st[j].u
        ELSE
          FIRST(P[st[j].rl].r[st[j].pt],xs)
        END;
        IF NOT and_not(st[i].u,xs) THEN
          print('----- NOT LR(1) -----\n');
          print_situation(st[i]);
          print_situation(st[j]);
          RETURN;
        END;
      ELSIF P[st[j].rl].r[st[j].pt]=char_null THEN
        IF P[st[i].rl].r[st[i].pt]=char_null THEN
          xs:=st[i].u
        ELSE
          FIRST(P[st[i].rl].r[st[i].pt],xs)
        END;
        IF NOT and_not(st[j].u,xs) THEN
          print('----- NOT LR(1) -----\n');
          print_situation(st[j]);
          print_situation(st[i]);
          RETURN;
        END;
      END;
    END;
  END;
END check_situation_set;

PROCEDURE situation_system;
  VAR
    st   : st_arr;
    st_no: INTEGER;
  PROCEDURE set_equ(VAL x,y: set): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(x) DO
      IF x[i]#y[i] THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END set_equ;
  PROCEDURE st_in(VAR s: situation): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO st_no-1 DO
      IF (s.rl=st[i].rl)&(s.pt=st[i].pt) THEN RETURN set_equ(s.u,st[i].u) END;
    END;
    RETURN FALSE;
  END st_in;
  PROCEDURE equ(n: INTEGER): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO sys_sz[n]-1 DO
      IF NOT st_in(sys[n]^[i]) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END equ;
  PROCEDURE exist(): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO sys_no-1 DO
      IF (sys_sz[i]=st_no) & equ(i) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END exist;
  VAR
    x    : char;
    s    : chain;
    i,k  : INTEGER;
BEGIN
  situations_for_chain(s,0,st,st_no);
  check_situation_set(st,st_no);
  ALLOCATE(sys[0],SIZE(situation)*st_no);
  move(sys[0],ADR(st),SIZE(situation)*st_no);
  sys_sz[0]:=st_no; sys_no:=1;
  print('situation set %4d (%4d)\n',0,st_no);
  i:=0;
  WHILE i<sys_no DO
--print('----- %5d ------\n',i);
--FOR k:=0 TO sys_sz[i]-1 DO print_situation(sys[i]^[k]) END;
    FOR x:=1 TO char_no-1 DO
      move(ADR(st),sys[i],SIZE(situation)*sys_sz[i]);
      st_no:=sys_sz[i];
      GOTO(st,st_no,x);
      IF (st_no>0) & NOT exist() THEN
        check_situation_set(st,st_no);
        ALLOCATE(sys[sys_no],SIZE(situation)*st_no);
        move(sys[sys_no],ADR(st),SIZE(situation)*st_no);
        sys_sz[sys_no]:=st_no;
        print('situation set %4d (%4d), mark %d\n',sys_no,st_no,i);
        INC(sys_no);
      END;
    END;
    INC(i);
  END;
END situation_system;

PROCEDURE print_situations;
  VAR i,j: INTEGER;
BEGIN
  FOR j:=0 TO sys_no-1 DO
    print('----- %5d ------\n',j);
    FOR i:=0 TO sys_sz[j]-1 DO print_situation(sys[j]^[i]) END;
  END;
END print_situations;

VAR
  c : char;
  i : INTEGER;
  st: ARRAY [0..999] OF situation;
  sl: INTEGER;
  x : chain;

BEGIN
  char_no:=1; term_no:=1; P_sz:=1;
  char_name[char_null]:='char_null';
  FOR c:=1 TO HIGH(char_name) DO char_name[c]:='undef_char' END;
  P[0].l:=char_null;
  FOR i:=0 TO HIGH(P[0].r) DO P[0].r[i]:=char_null END;
  read;

  situation_system;
  print_situations;

END lrk.
