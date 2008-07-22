(*# debug(vid=>full) *)
IMPLEMENTATION MODULE Strings; (* Leo  25-Jul-91. (c) KRONOS *)

IMPORT lowLevel;

CONST
  low   ::= lowLevel;
  BYTES ::= SIZE;

(*# save, call(o_a_copy=>off,o_a_size=>on) *)

PROCEDURE len(str: ARRAY OF CHAR): CARDINAL;
BEGIN
  RETURN low.scasb(ADR(str),BYTES(str),0C)
END len;

PROCEDURE move(VAR d: ARRAY OF CHAR; dp: CARDINAL;
                   s: ARRAY OF CHAR; sp: CARDINAL; len: CARDINAL);
BEGIN
  IF (sp>BYTES(s)) OR (dp>BYTES(d)) THEN RETURN END;
  IF sp+len>BYTES(s) THEN len:=BYTES(s)-sp END;
  IF dp+len>BYTES(d) THEN len:=BYTES(d)-dp END;
  IF len>0 THEN
    low.cmove(SYSTEM.ADR(d),dp,SYSTEM.ADR(s),sp,len)
  END
END move;


PROCEDURE app(VAR d: ARRAY OF CHAR; s: ARRAY OF CHAR);
BEGIN
  move(d,len(d),s,0,len(s))
END app;

PROCEDURE copy(VAR d: ARRAY OF CHAR; s: ARRAY OF CHAR);
BEGIN
  move(d,0,s,0,len(s))
END copy;

PROCEDURE delete(VAR s: ARRAY OF CHAR; pos,size: CARDINAL);
  VAR i,j: CARDINAL;
BEGIN
  IF size=0 THEN RETURN END;
  j:=len(s);
  IF pos>=j THEN RETURN END;
  IF pos+size>j THEN size:=j-pos END;
  move(s,pos,s,pos+size,j-(pos+size)+1)
END delete;

PROCEDURE insert(VAR s: ARRAY OF CHAR; pos,size: CARDINAL;  ch: CHAR);
  VAR i,n: CARDINAL; (* ??? *)
BEGIN
  IF size<=0 THEN RETURN END;
  n:=len(s)-1;
  i:=n+size;
  IF i<HIGH(s) THEN s[i+1]:=0C ELSE i:=HIGH(s); n:=i-size END;
  WHILE n>=pos DO s[i]:=s[n]; DEC(i); DEC(n) END;
  WHILE pos<=i DO s[pos]:=' '; INC(pos) END
END insert;

PROCEDURE substr(VAR sub: ARRAY OF CHAR;
                     str: ARRAY OF CHAR; pos,size: CARDINAL);
  VAR i,l: CARDINAL;
BEGIN
  l:=len(str);
  IF size+pos>l     THEN size:=l-pos     END;
  IF size>HIGH(sub) THEN size:=HIGH(sub) END;
  i:=0;
  WHILE i<size DO sub[i]:=str[pos+i]; INC(i) END;
  IF i<=HIGH(sub) THEN sub[i]:=0C END
END substr;

PROCEDURE subarr(VAR sub: ARRAY OF CHAR;
                     str: ARRAY OF CHAR; pos,size: CARDINAL);
  VAR i: CARDINAL;
BEGIN
  IF pos+size>HIGH(str) THEN size:=HIGH(str)-pos END;
  IF     size>HIGH(sub) THEN size:=HIGH(sub)     END;
  i:=0;
  WHILE i<size DO sub[i]:=str[pos+i]; INC(i) END;
  IF i<=HIGH(sub) THEN sub[i]:=0C END
END subarr;

PROCEDURE replace(VAR d: ARRAY OF CHAR;
                      s: ARRAY OF CHAR;
                  pos,size: CARDINAL);
  VAR i,lim: CARDINAL;
BEGIN
  IF size>HIGH(s) THEN size:=HIGH(s)+1 END;
  lim:=pos+size-1;
  IF lim>HIGH(d) THEN lim:=HIGH(d) END;
  i:=0;
  WHILE (pos<=lim) & (s[i]#0C) DO
    d[pos]:=s[i]; INC(i); INC(pos);
  END;
END replace;

PROCEDURE skip(s: ARRAY OF CHAR; VAR pos: CARDINAL; ch: CHAR);
BEGIN
  WHILE (pos<=HIGH(s)) & (s[pos]=ch) DO INC(pos) END
END skip;

PROCEDURE search(s: ARRAY OF CHAR; VAR pos: CARDINAL; ch: CHAR);
BEGIN
(*  pos:=low.scasb(ADR(str),BYTES(str)-pos,ch); ?? *)
  WHILE (pos<=HIGH(s)) & (s[pos]#ch) & (s[pos]#0C) DO INC(pos) END
END search;

PROCEDURE scan(    s   : ARRAY OF CHAR;
               VAR pos : CARDINAL;
                   p   : ARRAY OF CHAR;
               VAR done: BOOLEAN);
  VAR i: CARDINAL;
BEGIN
  i:=0;
  WHILE (pos<=HIGH(s)) & (i<=HIGH(p)) & (p[i]#0C) & (s[pos]=p[i]) DO
    INC(pos); INC(i)
  END;
  done:=(i>HIGH(p)) OR (p[i]=0C);
END scan;

(*-----------------------  NUMBERs  ---------------------------*)

PROCEDURE iscan(VAR num : LONGWORD;
                    str : ARRAY OF CHAR;
                VAR pos : CARDINAL;
                VAR done: BOOLEAN);


  CONST
    O=ORD('0');         A=ORD('A');
    B=ORD('B');         C=ORD('C');
    H=ORD('H');     Dovfl=MAX(LONGINT) DIV 10;

  VAR Plus: BOOLEAN;

  PROCEDURE Fin(n: CARDINAL): BOOLEAN;
  BEGIN
    IF n>HIGH(str) THEN RETURN TRUE END;
    n:=ORD(str[n]);
    RETURN NOT(
      ((n-O) IN {0..9}) OR
      ((n-A) IN {0..5,7}) OR
      ((n-ORD('a')) IN {0..5,7}) )
  END Fin;

  VAR c: CHAR;            Oval,Dval,Hval: LONGCARD;
 ch,dig: CARDINAL;         Oct,Dec, o,d,h: BOOLEAN;

BEGIN
  done:=FALSE;
  IF (pos<0) OR (pos>HIGH(str)) THEN RETURN (*illegal*) END;
  WHILE (pos<=HIGH(str)) & (str[pos]=' ') DO INC(pos) END;
  IF (pos>HIGH(str)) OR (str[pos]=0C) THEN RETURN (*illegal*) END;
  Plus:=(str[pos]#'-');
  IF Plus THEN INC(pos, INTEGER(str[pos]='+')) ELSE INC(pos) END;
  IF Fin(pos) OR NOT((ORD(str[pos])-O) IN {0..9}) & Fin(pos+1) THEN
    RETURN (*illegal*) (* no number *)
  END;
  Oval:=0; Dval:=0; Hval:=0;
  Oct:=TRUE; Dec:=TRUE;
  o:=TRUE; d:=o; h:=d;
  LOOP
    IF pos<=HIGH(str) THEN c:=str[pos];
      IF ('a'<=c) & (c<='z') THEN c:=CAP(c) END; ch:=ORD(c);
    ELSE ch:=0
    END;
    IF    (ch-O) IN {0..7} THEN dig:=ch-O;
    ELSIF (ch-O) IN {8..9} THEN dig:=ch-O;
      Oct:=FALSE;
    ELSIF ((ch=B) OR (ch=C)) & Fin(pos+1) THEN
      INC(pos);
      IF Plus THEN num:=Oval
      ELSIF LONGWORD(Hval)=LONGWORD(MIN(LONGINT)) THEN RETURN
      END;
      done:=o & Oct;
      RETURN
    ELSIF (ch-A) IN {0..5} THEN dig:=10+ch-A;
      Oct:=FALSE; Dec:=FALSE;
    ELSIF ch=H THEN
      INC(pos);
      IF Plus THEN num:=Hval
      ELSIF LONGWORD(Hval)=LONGWORD(MIN(LONGINT)) THEN RETURN
      END;
      done:=h;
      RETURN
    ELSE
      IF Plus THEN num:=Dval
      ELSIF LONGWORD(Dval)=LONGWORD(MIN(LONGINT)) THEN RETURN
      END;
      done:=d & Dec;
      RETURN
    END;
    IF h THEN
      h:=(Hval<=0FFFFFFFH);
      IF h THEN Hval:=LONGCARD(low.rol32(Hval,4))+LONGCARD(dig) END
    END;
    IF d & Dec THEN
      IF    (Dval=Dovfl) & (dig=8)        THEN Dval:=LONGWORD(MIN(LONGINT))
      ELSIF (Dval+LONGCARD(dig=9)) > Dovfl THEN d:=FALSE
      ELSE   Dval:=Dval*10+LONGCARD(dig)
      END
    END;
    IF o & Oct THEN
      o:=(Oval<=1FFFFFFFH);
      IF o THEN Oval:=LONGCARD(low.rol32(Oval,3))+LONGCARD(dig) END
    END;
    INC(pos)
  END
END iscan;

PROCEDURE rscan(VAR r   : LONGREAL;
                    s   : ARRAY OF CHAR;
                VAR pos : CARDINAL;
                VAR done: BOOLEAN);

(*
  CONST
    MaxReal=REAL(7FFFFFFFH);  (* TYPE REAL=[-MaxReal..MaxReal] *)
    Delta  =REAL(00800000H);  (* Delta / 2.0 = 0.0             *)
                              (* 0.0 = REAL(00000000h)         *)
  VAR x,p,d,F: REAL;
     sign : REAL;
     signE: INTEGER;
     E,i: INTEGER;
BEGIN
  done:=FALSE;
  x:=0.; sign:=+1.;
  i:=pos;
  WHILE (i<=HIGH(s)) & (s[i]=' ') DO INC(i) END;
  IF i>HIGH(s) THEN RETURN (*illegal*) END;
  IF    s[i]='+' THEN sign:=+1.; INC(i)
  ELSIF s[i]='-' THEN sign:=-1.; INC(i)
  END;
  IF (i>HIGH(s)) OR NOT (ORD(s[i])-ORD('0') IN {0..9}) THEN
    RETURN (*illegal*)
  END;
  WHILE (i<=HIGH(s)) & (ORD(s[i])-ORD('0') IN {0..9}) DO
    d:=FLOAT(ORD(s[i])-ORD('0'));
    IF x>=(MaxReal-d)/10. THEN RETURN (*overflow*) END;
    x:=10.*x+d;
    INC(i);
  END;
  IF (i<=HIGH(s)) & (s[i]='.') THEN INC(i); p:=0.1;
    WHILE (i<=HIGH(s)) & (ORD(s[i])-ORD('0') IN {0..9}) DO
      x:=FLOAT(ORD(s[i])-ORD('0'))*p+x;
      INC(i); p:=p/10.;
    END;
  END;
  IF (i<=HIGH(s)) & (CAP(s[i])='E') THEN INC(i);
    signE:=+1;
    IF    s[i]='-' THEN signE:=-1; INC(i);
    ELSIF s[i]='+' THEN signE:=+1; INC(i);
    END;  E:=0;
    IF (i>HIGH(s)) OR NOT (ORD(s[i])-ORD('0') IN {0..9}) THEN
      RETURN (*illegal*)
    END;
    WHILE (i<=HIGH(s)) & (ORD(s[i])-ORD('0') IN {0..9}) & (E<=38) DO
      E:=E*10+ORD(s[i])-ORD('0'); INC(i);
    END;
    WHILE E>0 DO
      IF signE=-1 THEN
        IF x<=Delta*10.   THEN r:=x; RETURN (*underflow*) END;
        x:=x/10.
      ELSE
        IF x>=MaxReal/10. THEN r:=x; RETURN (*overflow*) END;
        x:=x*10.
      END;
      DEC(E);
    END
  END;
  r:=x*sign;
  pos:=i;
  done:=TRUE
*)
END rscan;



(****************** FORMATTED PRINT **************************)


PROCEDURE _format(VAR str,fmt: ARRAY OF CHAR; seg,ofs: CARDINAL); FORWARD;

PROCEDURE format(VAR str: ARRAY OF CHAR; fmt: ARRAY OF CHAR; adr: ADDRESS);
  VAR seg,ofs: CARDINAL;
BEGIN
  seg:=Seg(adr^);
  ofs:=Ofs(adr^);
  _format(str,fmt,seg,ofs)
END format;

(*# save, call(var_arg=>on,c_conv=>on,reg_param=>()) *)
PROCEDURE print(VAR str: ARRAY OF CHAR; fmt: ARRAY OF CHAR);
  VAR seg,ofs: CARDINAL;
BEGIN
  seg:=low.SS();  ofs:=low.BP()+20;
  _format(str,fmt,seg,ofs)

END print;

PROCEDURE append(VAR str: ARRAY OF CHAR; fmt: ARRAY OF CHAR);
  VAR seg,ofs: CARDINAL;  bump: ARRAY [0..1023] OF CHAR;
BEGIN
  seg:=low.SS();  ofs:=low.BP()+26;
  _format(bump,fmt,seg,ofs);
  app(str,bump)
END append;

PROCEDURE image(VAR s: ARRAY OF CHAR; VAR pos: CARDINAL; f: ARRAY OF CHAR);
  VAR seg,ofs,i: CARDINAL;  bump: ARRAY [0..1023] OF CHAR;
BEGIN
  seg:=low.SS();  ofs:=low.BP()+30;
  _format(bump,f,seg,ofs);
  i:=len(bump);
  move(s,pos,bump,0,i); INC(pos,i)
END image;
(*# restore *)






PROCEDURE _format(VAR str,fmt: ARRAY OF CHAR; seg,ofs: CARDINAL);

  VAR inp,out,high: CARDINAL;  ch: CHAR;

  PROCEDURE put(ch: BYTE);
  BEGIN
    IF out<HIGH(str) THEN str[out]:=ch; INC(out) END
  END put;

  (*# save *)  (*# call(o_a_copy=>off) *)
  PROCEDURE puts(s: ARRAY OF CHAR);
    VAR i: CARDINAL;
  BEGIN
    i:=0;
    WHILE s[i]#0C DO put(s[i]); INC(i) END
  END puts;    (*# restore *)

  PROCEDURE special;
  BEGIN
    INC(inp);
    IF inp>high THEN put(ch); RETURN END;
    ch:=fmt[inp];
    INC(inp);
    IF ch='n' THEN put(15C); put(12C); RETURN END;
    IF ch='r' THEN put(15C); RETURN END;
    IF ch='l' THEN put(12C); RETURN END;
    put(ch)
  END special;

  VAR FLAGS: BITSET;
      RADIX: CHAR;
      PARMS: ARRAY [0..3] OF CARDINAL;  pcou: CARDINAL;

  CONST
    sign   = {0};
    space  = {1};
    leftj  = {2};
    long   = {3};
    short  = {4};
    base   = {5};
    center = {6};
    zero   = {7};
    caps   = {8};

  PROCEDURE repeat(ch: CHAR; cou: CARDINAL);
  BEGIN
    WHILE cou>0 DO put(ch); DEC(cou) END
  END repeat;

  PROCEDURE outnum(neg: BOOLEAN; VAR bump: ARRAY OF CHAR; len: CARDINAL);
    VAR i: CARDINAL;
      j,t: CARDINAL;
      sgn: BOOLEAN;
     fill: CHAR;

    PROCEDURE putsgn;
    BEGIN
      IF NOT sgn THEN RETURN END;
      IF neg THEN put('-') ELSIF sign*FLAGS#{} THEN put('+') ELSE put(' ') END
    END putsgn;

    PROCEDURE putbump;
    BEGIN
      WHILE len>0 DO put(bump[i]); INC(i); DEC(len) END
    END putbump;

  BEGIN
    IF (PARMS[1]>0) & (len>PARMS[1]) THEN i:=len-PARMS[1]; len:=PARMS[1]
    ELSE i:=0
    END;
    sgn:=neg OR (space*FLAGS#{}) OR (sign*FLAGS#{});
    IF PARMS[0]<=len+ORD(sgn) THEN
      putsgn; putbump
    ELSE
      j:=PARMS[0]-len-ORD(sgn);
      IF leftj*FLAGS#{} THEN
        putsgn; putbump; t:=j
      ELSE
        IF zero*FLAGS#{} THEN fill:='0'; putsgn ELSE fill:=' ' END;
        t:=0;
        IF (zero+center)*FLAGS=center THEN t:=j DIV 2; j:=j-t END;
        repeat(fill,j);
        IF zero*FLAGS={} THEN putsgn END;
        putbump
      END;
      repeat(' ',t)
    END
  END outnum;

  PROCEDURE getint(VAR val: LONGINT);
    VAR p: POINTER TO INTEGER;
       lp: POINTER TO LONGINT;
       sp: POINTER TO SHORTINT;
  BEGIN
    IF    long *FLAGS#{} THEN lp:=[seg:ofs];  INC(ofs,4); val:=lp^
    ELSIF short*FLAGS#{} THEN sp:=[seg:ofs];  INC(ofs,4); val:=LONGINT(sp^)
    ELSE                       p:=[seg:ofs];  INC(ofs,2); val:=LONGINT(p^)
    END
  END getint;

  PROCEDURE justify(VAR bump: ARRAY OF CHAR; i: CARDINAL; VAR len: CARDINAL);
  BEGIN
    len:=0; INC(i);
    WHILE i<=HIGH(bump) DO bump[len]:=bump[i]; INC(len); INC(i) END;
    bump[len]:=0C
  END justify;

  PROCEDURE signed(val: LONGINT);
    VAR i,j: CARDINAL;
        neg: BOOLEAN;
       bump: ARRAY [0..15] OF CHAR;
  BEGIN
    IF (long*FLAGS#{}) & (val=MIN(LONGINT)) THEN
      neg:=TRUE; j:=10; bump:="2147483648"
    ELSE
      neg:=(val<0);
      val:=ABS(val);
      i:=HIGH(bump);
      REPEAT
        bump[i]:=CHAR(ORD('0')+val MOD 10);  DEC(i);  val:=val DIV 10
      UNTIL val=0;
      justify(bump,i,j)
    END;
    outnum(neg,bump,j)
  END signed;

  PROCEDURE getword(VAR val: LONGWORD);
    VAR p: POINTER TO WORD;
       lp: POINTER TO LONGWORD;
       sp: POINTER TO BYTE;
  BEGIN
    IF     long*FLAGS#{} THEN lp:=[seg:ofs];  INC(ofs,4); val:=lp^
    ELSIF short*FLAGS#{} THEN sp:=[seg:ofs];  INC(ofs,4); val:=LONGWORD(sp^)
    ELSE                       p:=[seg:ofs];  INC(ofs,2); val:=LONGWORD(p^)
    END
  END getword;

  PROCEDURE unsigned;
    VAR i,j: CARDINAL;
        val: LONGCARD;
       bump: ARRAY [0..15] OF CHAR;
  BEGIN
    getword(val);
    i:=HIGH(bump);
    REPEAT
      bump[i]:=CHAR(ORD('0')+val MOD 10);  DEC(i);  val:=val DIV 10
    UNTIL val=0;
    justify(bump,i,j);
    outnum(FALSE,bump,j)
  END unsigned;

  PROCEDURE hex;
    VAR i,j: CARDINAL;
        val: LONGCARD;
        dig: CARDINAL;
        DIG: ARRAY [0..15] OF CHAR;
       bump: ARRAY [0..15] OF CHAR;
  BEGIN
    IF caps*FLAGS#{} THEN DIG:="0123456789ABCDEF"
    ELSE                  DIG:="0123456789abcdef"
    END;
    getword(val);
    i:=HIGH(bump);
    REPEAT
      bump[i]:=DIG[CARDINAL(val MOD 16)];  val:=val DIV 16;  DEC(i)
    UNTIL val=0;
    justify(bump,i,j);
    outnum(FALSE,bump,j)
  END hex;

  PROCEDURE bin;
    VAR i,j: CARDINAL;
        val: LONGCARD;
        dig: CARDINAL;
       bump: ARRAY [0..15] OF CHAR;
  BEGIN
    getword(val);
    i:=HIGH(bump);
    REPEAT
      bump[i]:=CHAR(ORD('0')+CARDINAL(val MOD 2));  val:=val DIV 2;  DEC(i)
    UNTIL val=0;
    justify(bump,i,j); outnum(FALSE,bump,j)
  END bin;

  PROCEDURE oct;
    VAR i,j: CARDINAL;
        val: LONGCARD;
        dig: CARDINAL;
       bump: ARRAY [0..15] OF CHAR;
  BEGIN
    getword(val);
    i:=HIGH(bump);
    REPEAT
      bump[i]:=CHAR(ORD('0')+CARDINAL(val MOD 8));  val:=val DIV 8;  DEC(i)
    UNTIL val=0;
    justify(bump,i,j);  outnum(FALSE,bump,j)
  END oct;

  PROCEDURE outbump(VAR bump: ARRAY OF CHAR; len: CARDINAL);
    VAR i,j: CARDINAL;
  BEGIN
    i:=0; j:=0;
    IF PARMS[0]>len THEN
      i:=PARMS[0]-len;
      IF   center*FLAGS#{} THEN j:=i DIV 2; i:=i-j
      ELSIF leftj*FLAGS#{} THEN j:=i;       i:=0
      END
    END;
    repeat(' ',i);  puts(bump);  repeat(' ',j)
  END outbump;


  PROCEDURE bitset;

    VAR i,b: CARDINAL;
        cou: CARDINAL;
        val: SET OF [0..31];
        emp: BOOLEAN;
       bump: ARRAY [0..63] OF CHAR;

    PROCEDURE one(n: CARDINAL);
    BEGIN
      bump[b]:=CHAR(n DIV 10 + ORD('0')); INC(b);
      bump[b]:=CHAR(n MOD 10 + ORD('0')); INC(b)
    END one;

  BEGIN
    getword(val);
    cou:=0;  emp:=TRUE;   b:=1;   bump[0]:='{';
    i:=0;
    REPEAT
      IF i IN val THEN
        INC(cou)
      ELSIF cou>0 THEN
        IF NOT emp THEN bump[b]:=','; INC(b) END;
        IF cou=1 THEN
          one(i-1);
        ELSE
          one(i-cou);  bump[b]:='.'; INC(b);  bump[b]:='.'; INC(b);  one(i-1)
        END;
        cou:=0; emp:=FALSE;
      END;
      i:=i+1;
    UNTIL i>32;
    bump[b]:='}'; INC(b);  bump[b]:=0C;
    outbump(bump,b)
  END bitset;

  PROCEDURE string;
    VAR i,j: CARDINAL;
        len: CARDINAL;
        ptr: POINTER TO ARRAY [0..0FFFEH] OF CHAR;
   w0,w1,w2: CARDINAL;
  BEGIN
    FLAGS:=FLAGS+long;
    getword(ptr);
    IF ptr=NIL THEN RETURN END;
    w0:=PARMS[0];
    w1:=PARMS[1];
    w2:=PARMS[2];
    i:=0;
    IF w0+w1+w2=0 THEN
      IF ptr=NIL THEN RETURN END;
      WHILE (ptr^[i]#0C) & (out<HIGH(str)) DO
        put(ptr^[i]); INC(i)
      END;
      RETURN
    END;
    IF w2#0 THEN
      IF ptr=NIL THEN RETURN END;
      WHILE (ptr^[i]#0C) & (w2>0) DO INC(i); DEC(w2) END;
      IF ptr^[i]=0C THEN RETURN END
    END;
    len:=0;
    IF w1=0 THEN w1:=0FFFEH END;
    IF ptr=NIL THEN
      len:=0
    ELSE
      WHILE (len<w1) & (ptr^[len]#0C) DO INC(len) END;
    END;
    IF (w0=0) OR (len>w0) THEN
      WHILE len>0 DO put(ptr^[i]); INC(i); DEC(len) END; RETURN
    END;
    w1:=0; w2:=0;
    IF w0>len THEN
      w1:=w0-len;
      IF   center*FLAGS#{} THEN w2:=w1 DIV 2; w1:=w1-w2
      ELSIF leftj*FLAGS#{} THEN w2:=w1;       w1:=0
      END
    END;
    repeat(' ',w1);
    WHILE len>0 DO put(ptr^[i]); INC(i); DEC(len) END;
    repeat(' ',w2)
  END string;

  PROCEDURE char;
    VAR i,j,r: CARDINAL;
        n: LONGCARD;
  BEGIN
    getword(n);
    n:=n MOD 256;
    IF PARMS[1]>0 THEN r:=PARMS[1] ELSE r:=1 END;
    IF (PARMS[0]>1) & (PARMS[0]>r) THEN
      IF leftj*FLAGS#{} THEN repeat(CHAR(n),r) END;
      i:=PARMS[0]-r; j:=0;
      IF center*FLAGS#{} THEN j:=i DIV 2; i:=i-j END;
      repeat(' ',i);
      IF leftj*FLAGS={} THEN repeat(CHAR(n),r) END;
      repeat(' ',j);
    ELSE
      repeat(CHAR(n),r)
    END
  END char;

  PROCEDURE getreal(VAR val: LONGREAL);
    VAR p: POINTER TO REAL;
       lp: POINTER TO LONGREAL;
  BEGIN
    IF long*FLAGS#{} THEN lp:=[seg:ofs];  INC(ofs,8); val:=lp^
    ELSE                   p:=[seg:ofs];  INC(ofs,4); val:=LONGREAL(p^)
    END
  END getreal;

  PROCEDURE floatF(val: LONGREAL);
    VAR r: LONGREAL;
        e: CARDINAL;
     digs: CARDINAL;
     bump: ARRAY [0..127] OF CHAR;
    w0,w1: CARDINAL;
  sgn,neg: BOOLEAN;

    PROCEDURE outsgn;
    BEGIN
      IF NOT sgn THEN RETURN END;
      IF neg THEN put('-'); RETURN END;
      IF sign*FLAGS#{} THEN put('+'); RETURN END;
      put(' ')
    END outsgn;

    PROCEDURE outdig;
      VAR j: CARDINAL;
    BEGIN
      IF digs=0 THEN put('0'); RETURN END;
      j:=TRUNC(r);  put(CHAR(ORD('0')+j));
      r:=(r-VAL(LONGREAL,j))*10.0;  DEC(digs)
    END outdig;


  BEGIN
    neg:=(val<0.0);
    IF neg THEN val:=ABS(val) END;
    sgn:=neg OR ((sign+space)*FLAGS#{});
    IF PARMS[0]=0 THEN w0:=6 ELSE w0:=PARMS[0] END;
    IF PARMS[1]=0 THEN w1:=3 ELSE w1:=PARMS[1] END;
    w0:=w0-ORD(sgn);
    r:=val;
    e:=1;
    WHILE r>10.0 DO r:=r/10.0; INC(e) END;
    IF e+w1+1>w0 THEN w0:=e+w1+1 END;
    IF zero*FLAGS={} THEN repeat(' ',w0-(e+w1+1));  outsgn
    ELSE    outsgn;       repeat('0',w0-(e+w1+1))
    END;
    IF long*FLAGS#{} THEN digs:=17 ELSE digs:=8 END;
    WHILE e>=1 DO outdig; DEC(e) END;
    put('.');
    WHILE w1>0 DO outdig; DEC(w1) END
  END floatF;

  PROCEDURE floatE(val: LONGREAL);
    VAR r: LONGREAL;
      b,e: CARDINAL;
      exp: CARDINAL;
     digs: CARDINAL;
     bump: ARRAY [0..127] OF CHAR;
    w0,w1: CARDINAL;
  sgn,neg: BOOLEAN;

    PROCEDURE outsgn;
    BEGIN
      IF NOT sgn THEN RETURN END;
      IF neg THEN put('-'); RETURN END;
      IF sign*FLAGS#{} THEN put('+'); RETURN END;
      put(' ')
    END outsgn;

    PROCEDURE outdig;
      VAR j: CARDINAL;
    BEGIN
      IF digs=0 THEN put('0'); RETURN END;
      j:=TRUNC(r);  put(CHAR(ORD('0')+j));
      r:=(r-VAL(LONGREAL,j))*10.0;  DEC(digs)
    END outdig;


  BEGIN
    neg:=(val<0.0);
    IF neg THEN val:=ABS(val) END;
    sgn:=neg OR ((sign+space)*FLAGS#{});
    IF PARMS[0]=0 THEN w0:=6 ELSE w0:=PARMS[0] END;
    IF PARMS[1]=0 THEN w1:=3 ELSE w1:=PARMS[1] END;
    IF long*FLAGS#{} THEN digs:=17; exp:=3; b:=100
    ELSE                  digs:= 8; exp:=2; b:=10
    END;
    w0:=w0-ORD(sgn);
    r:=val;
    e:=0;
    WHILE r>10.0 DO r:=r/10.0; INC(e) END;
    IF r>1.0E-306 THEN
      WHILE r<1.0 DO r:=r*10.0; DEC(e) END;
    ELSE
      r:=0.0
    END;
    IF w1+1+exp+2>w0 THEN w0:=w1+1+exp+2 END;
    IF zero*FLAGS={} THEN repeat(' ',w0-(exp+3+w1+1)); outsgn
    ELSE    outsgn;       repeat('0',w0-(exp+3+w1+1))
    END;
    outdig;
    put('.');
    WHILE w1>0 DO outdig; DEC(w1) END;
    IF caps*FLAGS#{} THEN put('E') ELSE put('e') END;
    IF e>=0 THEN put('+') ELSE put('-') END;
    e:=ABS(e);
    WHILE b>0 DO
      put(CHAR(ORD('0')+(e DIV b) MOD 10)); b:=b DIV 10
    END
  END floatE;

  PROCEDURE floatG(val: LONGREAL);
    VAR e: CARDINAL;
        r: LONGREAL;
    w0,w1: CARDINAL;
  sgn,neg: BOOLEAN;
  BEGIN
    neg:=(val<0.0);
    sgn:=neg OR ((sign+space)*FLAGS#{});
    IF PARMS[0]=0 THEN w0:=6 ELSE w0:=PARMS[0] END;
    IF PARMS[1]=0 THEN w1:=3 ELSE w1:=PARMS[1] END;
    w0:=w0-ORD(sgn);
    r:=ABS(val);
    e:=0;
    WHILE r>10.0 DO r:=r/10.0; INC(e) END;
    IF (e<8) & (VAL(LONGREAL,VAL(LONGINT,val))=val) THEN
      PARMS[1]:=0; signed(LONGINT(val))
    ELSIF e<w0-(w1+1) THEN
      floatF(val)
    ELSE
      floatE(val)
    END
  END floatG;

  PROCEDURE item;
    CONST ilgrdx = "*** ilg radix ***";
    VAR n: CARDINAL;
        p: POINTER TO WORD;
       lp: POINTER TO LONGWORD;
      lrp: POINTER TO LONGREAL;
      int: LONGINT;
     real: LONGREAL;
  BEGIN
    FLAGS:={};
    pcou:=0;
    PARMS[0]:=0;
    PARMS[1]:=0;
    PARMS[2]:=0;
    LOOP
      IF inp>high THEN RETURN END;
      ch:=fmt[inp];
      CASE ch OF
      |'+': FLAGS:=FLAGS+sign
      |'-': FLAGS:=FLAGS+leftj
      |' ': FLAGS:=FLAGS+space
      |'#': FLAGS:=FLAGS+base
      |'l': FLAGS:=FLAGS+long
      |'@': FLAGS:=FLAGS+short
      |'.': IF pcou<2 THEN INC(pcou) ELSE puts("*** %w.p.o err ***") END
      |'|': FLAGS:=FLAGS+center
      |'*': p:=[seg:ofs]; PARMS[pcou]:=CARDINAL(p^) MOD 256;
            INC(ofs,2)
      |'0'..'9':
            IF (ch='0') & (pcou=0) THEN FLAGS:=FLAGS+zero END;
            n:=0;
            WHILE (inp<=high) & ('0'<=ch) & (ch<='9') DO
              n:=(n*10+ORD(ch)-ORD('0')) MOD 256;   INC(inp);
              IF inp<=high THEN ch:=fmt[inp] ELSE ch:=0C END
            END;
            PARMS[pcou]:=n; DEC(inp)
      ELSE
        RADIX:=ch; INC(inp); EXIT
      END;
      INC(inp)
    END;
    IF ('A'<=RADIX) AND (RADIX<='Z') THEN
      INC(RADIX,40B); FLAGS:=FLAGS+caps
    END;
    CASE RADIX OF
    |'d': getint(int); signed(int)
    |'u': unsigned
    |'c': char
    |'s': string
    |'{': IF (inp<=high) & (fmt[inp]='}') THEN INC(inp) ELSE puts(ilgrdx) END;
              bitset
    |'e': getreal(real); floatE(real)
    |'f': getreal(real); floatF(real)
    |'g': getreal(real); floatG(real)
    |'i': bin
    |'o','b': oct
    |'x','h': hex
    ELSE
      puts(ilgrdx)
    END
  END item;

BEGIN
  inp:=0;  out:=0;  high:=HIGH(fmt);
  WHILE (inp<=high) & (fmt[inp]#0C) DO
    ch:=fmt[inp];
    IF ch='\' THEN
      special
    ELSIF ch#'%' THEN
      put(ch); INC(inp)
    ELSIF (inp+1<=high) & (fmt[inp+1]='%') THEN
      put(ch); INC(inp,2)
    ELSE
      INC(inp); item
    END
  END;
  str[out]:=0C
END _format;

END Strings.
