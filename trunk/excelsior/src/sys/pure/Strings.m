IMPLEMENTATION MODULE Strings[1]; (*    Ned 20-Jun-89. (c) KRONOS *)
                                  (*$T- Leo 12-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  out: Formats;
IMPORT  low: lowLevel;

---------------------------  FORMAT  --------------------------
                           ----------

TYPE
  WORD     = SYSTEM.WORD;

  desc_rec = RECORD
               str: STRING;
               pos: INTEGER;
             END;
  desc_ptr = POINTER TO desc_rec;

PROCEDURE format_app(x: WORD; VAL s: ARRAY OF CHAR; i,l: INTEGER);
  VAR ptr: desc_ptr;
        d: STRING;
      h,p: INTEGER;
BEGIN
  ptr:=x;       p:=ptr^.pos;
  h:=p+l;
(*$<U+*)
  d^:=ptr^.str^;
(*$>*)
  IF h>HIGH(d) THEN h:=HIGH(d) END;
  IF p<h THEN
    REPEAT d[p]:=s[i]; i:=i+1; p:=p+1 UNTIL p=h
  END;
  ptr^.pos:=p
END format_app;

PROCEDURE print(VAR s: ARRAY OF CHAR; VAL f: ARRAY OF CHAR;
                                      SEQ x: WORD);
  VAR r: desc_rec;
BEGIN
  r.pos:=0;
(*$<U+*)
  r.str^.ADR:=SYSTEM.ADR(s); r.str^.HIGH:=HIGH(s);
(*$>*)
  out.format(SYSTEM.ADR(r),format_app,f,x);
  IF r.pos<=HIGH(s) THEN s[r.pos]:=0c END;
END print;

PROCEDURE append(VAR s: ARRAY OF CHAR; VAL f: ARRAY OF CHAR;
                                       SEQ x: WORD);
  VAR i: INTEGER; r: desc_rec;
BEGIN i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO INC(i) END;
  r.pos:=i;
(*$<U+*)
  r.str^.ADR:=SYSTEM.ADR(s); r.str^.HIGH:=HIGH(s);
(*$>*)
  out.format(SYSTEM.ADR(r),format_app,f,x);
  IF r.pos<=HIGH(s) THEN s[r.pos]:=0c END;
END append;

PROCEDURE image(VAR s: ARRAY OF CHAR; VAR pos: INTEGER;
                VAL f: ARRAY OF CHAR; SEQ x  : WORD);
  VAR r: desc_rec;
BEGIN
  ASSERT(pos>=0,4Fh);
  r.pos:=pos;
(*$<U+*)
  r.str^.ADR:=SYSTEM.ADR(s); r.str^.HIGH:=HIGH(s);
(*$>*)
  out.format(SYSTEM.ADR(r),format_app,f,x);
  IF r.pos<=HIGH(s) THEN s[r.pos]:=0c END;
  pos:=r.pos;
END image;

----------------------------------------------------------------

PROCEDURE len(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO INC(i) END;
  RETURN i
END len;

PROCEDURE app(VAR d: ARRAY OF CHAR; VAL s: ARRAY OF CHAR);
  VAR i,j,h: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(d)) & (d[i]#0c) DO INC(i) END;
  h:=HIGH(s)+1;
  IF i+h>HIGH(d) THEN h:=HIGH(d)-i END;
  j:=0;
  WHILE (j<h) & (s[j]#0c) DO d[i]:=s[j]; INC(j); INC(i) END;
  IF i<=HIGH(d) THEN d[i]:=0c END;
END app;

PROCEDURE copy(VAR d: ARRAY OF CHAR; VAL s: ARRAY OF CHAR);
  VAR i,h: INTEGER;
BEGIN
  h:=HIGH(s)+1;
  IF h>HIGH(d) THEN h:=HIGH(d) END;
  i:=0;
  WHILE (i<h) & (s[i]#0c) DO d[i]:=s[i]; INC(i) END;
  IF i<=HIGH(d) THEN d[i]:=0c END;
END copy;

PROCEDURE delete(VAR s: ARRAY OF CHAR; pos,size: INTEGER);
  VAR i,hi: INTEGER;
BEGIN
  IF size<=0 THEN RETURN END;
  ASSERT(pos>=0,4Fh);
  i:=pos+size;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO
    s[pos]:=s[i]; INC(pos); INC(i)
  END;
  IF pos<=HIGH(s) THEN s[pos]:=0c END
END delete;

PROCEDURE insert(VAR s: ARRAY OF CHAR; pos,size: INTEGER);
  VAR i,n: INTEGER;
BEGIN
  IF size<=0 THEN RETURN END;
  ASSERT(pos>=0,4Fh);
  n:=len(s)-1;
  i:=n+size;
  IF i<HIGH(s) THEN s[i+1]:=0c ELSE i:=HIGH(s); n:=i-size END;
  WHILE n>=pos DO s[i]:=s[n]; DEC(i); DEC(n) END;
  WHILE pos<=i DO s[pos]:=' '; INC(pos) END
END insert;

PROCEDURE sub_str(VAR sub: ARRAY OF CHAR;
                  VAL str: ARRAY OF CHAR; pos,size: INTEGER);
  VAR i,l: INTEGER;
BEGIN
  ASSERT((pos>=0) & (size>=0),4Fh);
  l:=len(str);
  IF size+pos>l     THEN size:=l-pos     END;
  IF size>HIGH(sub) THEN size:=HIGH(sub) END;
  i:=0;
  WHILE i<size DO sub[i]:=str[pos+i]; INC(i) END;
  IF i<=HIGH(sub) THEN sub[i]:=0c END
END sub_str;

PROCEDURE sub_arr(VAR sub: ARRAY OF CHAR;
                  VAL str: ARRAY OF CHAR; pos,size: INTEGER);
  VAR i: INTEGER;
BEGIN
  ASSERT((pos>=0) & (size>=0),4Fh);
  IF pos+size>HIGH(str) THEN size:=HIGH(str)-pos END;
  IF     size>HIGH(sub) THEN size:=HIGH(sub)     END;
  i:=0;
  WHILE i<size DO sub[i]:=str[pos+i]; INC(i) END;
  IF i<=HIGH(sub) THEN sub[i]:=0c END
END sub_arr;

PROCEDURE replace(VAR d: ARRAY OF CHAR;
                  VAL s: ARRAY OF CHAR;
                  pos,size: INTEGER);
  VAR i,lim: INTEGER;
BEGIN
  IF size>HIGH(s) THEN size:=HIGH(s)+1 END;
  lim:=pos+size-1;
  IF lim>HIGH(d) THEN lim:=HIGH(d) END;
  i:=0;
  WHILE (pos<=lim) & (s[i]#0c) DO
    d[pos]:=s[i]; INC(i); INC(pos);
  END;
END replace;

PROCEDURE skip(VAL s: ARRAY OF CHAR; VAR pos: INTEGER; ch: CHAR);
BEGIN
  ASSERT(pos>=0);
  WHILE (pos<=HIGH(s)) & (s[pos]=ch) DO INC(pos) END
END skip;

PROCEDURE search(VAL s: ARRAY OF CHAR; VAR pos: INTEGER; ch: CHAR);
BEGIN
  ASSERT(pos>=0);
  WHILE (pos<=HIGH(s)) & (s[pos]#ch) & (s[pos]#0c) DO INC(pos) END
END search;

PROCEDURE scan(VAL s   : ARRAY OF CHAR;
               VAR pos : INTEGER;
               VAL p   : ARRAY OF CHAR;
               VAR done: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  ASSERT(pos>=0);
  i:=0;
  WHILE (pos<=HIGH(s)) & (i<=HIGH(p)) & (p[i]#0c) & (s[pos]=p[i]) DO
    INC(pos); INC(i)
  END;
  done:=(i>HIGH(p)) OR (p[i]=0c);
END scan;

-------------------------  NUMBERs  ---------------------------
                         -----------

PROCEDURE iscan(VAR num : WORD;
                VAL str : ARRAY OF CHAR;
                VAR pos : INTEGER;
                VAR done: BOOLEAN);

  CONST
    O=ORD('0');         A=ORD('A');
    B=ORD('B');         C=ORD('C');
    H=ORD('H');     Dovfl=MAX(INTEGER) DIV 10;

  VAR Plus: BOOLEAN;

  PROCEDURE Fin(n: INTEGER): BOOLEAN;
  BEGIN
    IF n>HIGH(str) THEN RETURN TRUE END;
    n:=ORD(str[n]);
    RETURN NOT(
      ((n-O) IN {0..9}) OR
      ((n-A) IN {0..5,7}) OR
      ((n-ORD('a')) IN {0..5,7}) )
  END Fin;

  PROCEDURE Sgn(x: INTEGER): INTEGER;
  BEGIN
    IF (x=MIN(INTEGER)) OR Plus THEN RETURN x ELSE RETURN -x END
  END Sgn;

  VAR c: CHAR;    ch, dig: INTEGER;
      Oval, Dval, Hval   : INTEGER;
      Oct?, Dec?, o, d, h: BOOLEAN;
BEGIN
  done:=FALSE;
  IF (pos<0) OR (pos>HIGH(str)) THEN RETURN (*illegal*) END;
  WHILE (pos<=HIGH(str)) & (str[pos]=' ') DO INC(pos) END;
  IF (pos>HIGH(str)) OR (str[pos]=0c) THEN RETURN (*illegal*) END;
  Plus:=(str[pos]#'-');
  IF Plus THEN INC(pos, INTEGER(str[pos]='+')) ELSE INC(pos) END;
  IF Fin(pos) OR NOT((ORD(str[pos])-O) IN {0..9}) & Fin(pos+1) THEN
    RETURN (*illegal*) (* no number *)
  END;
  Oval:=0; Dval:=0; Hval:=0;
  Oct?:=TRUE; Dec?:=TRUE;
  o:=TRUE; d:=o; h:=d;
  LOOP
    IF pos<=HIGH(str) THEN c:=str[pos];
      IF ('a'<=c) & (c<='z') THEN c:=CAP(c) END; ch:=ORD(c);
    ELSE ch:=0
    END;
    IF    (ch-O) IN {0..7} THEN dig:=ch-O;
    ELSIF (ch-O) IN {8..9} THEN dig:=ch-O;
      Oct?:=FALSE;
    ELSIF ((ch=B) OR (ch=C)) & Fin(pos+1) THEN
      num:=Sgn(Oval);
      IF o & Oct? THEN INC(pos); done:=TRUE; RETURN
      ELSIF  Oct? THEN RETURN (*overflow   *)
      ELSE             RETURN (*invalidbase*)
      END;
    ELSIF (ch-A) IN {0..5} THEN dig:=10+ch-A;
      Oct?:=FALSE; Dec?:=FALSE;
    ELSIF ch=H THEN
      num:=Sgn(Hval);
      IF h THEN INC(pos); done:=TRUE; RETURN ELSE RETURN (*overflow*) END;
    ELSE
      num:=Sgn(Dval);
      IF d & Dec? THEN done:=TRUE; RETURN
      ELSIF  Dec? THEN RETURN (*overflow   *)
      ELSE             RETURN (*invalidbase*)
      END;
    END;
    IF h THEN
      h:=(BITSET(Hval)*{28..31} = {});
      IF h THEN Hval:=INTEGER(BITSET(Hval<<4)+BITSET(dig)) END
    END;
    IF d & Dec? THEN
      IF    (Dval=Dovfl) & (dig=8)        THEN Dval:=MIN(INTEGER);
      ELSIF (Dval+INTEGER(dig=9)) > Dovfl THEN d:=FALSE
      ELSE   Dval:=Dval*10+dig
      END
    END;
    IF o & Oct? THEN
      o:=(BITSET(Oval)*{29..31} = {});
      IF o THEN Oval:=INTEGER(BITSET(Oval<<3)+BITSET(dig)) END
    END;
    INC(pos);
  END;
END iscan;

PROCEDURE rscan(VAR r   : REAL;
                VAL s   : ARRAY OF CHAR;
                VAR pos : INTEGER;
                VAR done: BOOLEAN);

  CONST
    MaxReal=REAL(7FFFFFFFh);  (* TYPE REAL=[-MaxReal..MaxReal] *)
    Delta  =REAL(00800000h);  (* Delta / 2.0 = 0.0             *)
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
  done:=TRUE;
END rscan;

PROCEDURE move(VAR d: ARRAY OF CHAR; dp: INTEGER;
               VAL s: ARRAY OF CHAR; sp: INTEGER; len: INTEGER);
BEGIN
  IF sp+len>BYTES(s) THEN len:=BYTES(s)-sp END;
  IF dp+len>BYTES(d) THEN len:=BYTES(d)-dp END;
  IF len>0 THEN
    low.cmove(SYSTEM.ADR(d),dp,SYSTEM.ADR(s),sp,len)
  END
END move;

END Strings.
