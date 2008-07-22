IMPLEMENTATION MODULE tskArgs; (* Ned   27-Sep-89. (c) KRONOS *)
                               (* Hady. 14-Oct-89. (c) KRONOS *)

(*$U+*)

IMPORT  str: Strings;
IMPORT  mem: Heap;
IMPORT  env: tskEnv;

WITH STORAGE: mem;

TYPE
  CHARs = ARRAY [0..7] OF BITSET;
  f_ptr = POINTER TO f_rec;
  f_rec = RECORD
            pre : CHAR;
            set : CHARs;
            next: f_ptr;
          END;
  e_ptr = POINTER TO e_rec;
  e_rec = RECORD
            name: STRING;
            body: STRING;
            next: e_ptr;
          END;

VAR equ: e_ptr;
  flags: f_ptr;

PROCEDURE scan_string(VAL s  : ARRAY OF CHAR;
                         equ?: BOOLEAN;
                          wds: BOOLEAN;
                         esc?: BOOLEAN;
                      VAL pre: ARRAY OF CHAR;
                      VAL sep: ARRAY OF CHAR);

  PROCEDURE find_flags(ch: CHAR; VAR f: f_ptr);
    VAR i: INTEGER;
  BEGIN
    f:=flags;
    WHILE (f#NIL) & (f^.pre#ch) DO f:=f^.next END;
    IF f#NIL THEN RETURN END;
    NEW(f);
    FOR i:=0 TO HIGH(f^.set) DO f^.set[i]:={} END;
    f^.pre:=ch;
    f^.next:=flags; flags:=f;
  END find_flags;

  PROCEDURE new_equ(name: ARRAY OF CHAR; ln: INTEGER; VAL x: STRING);
    VAR l,e: e_ptr; i: INTEGER;
  BEGIN
    NEW(e);
    NEW(e^.name,ln);
    FOR i:=0 TO ln-1 DO e^.name[i]:=name[i] END;
    e^.body^:=x^;
    e^.next:=NIL;
    IF equ=NIL THEN equ:=e
    ELSE l:=equ;
      WHILE l^.next#NIL DO l:=l^.next END;
      l^.next:=e;
    END;
  END new_equ;

  VAR   set: CHARs;
      x,W,w: STRING;
         wp: INTEGER;
 chs,chx,ch: CHAR;
         wc: INTEGER;
      i,k,n: INTEGER;
          F: f_ptr;
        esc: CHAR;

  PROCEDURE put_s(VAR x: STRING; c: CHAR; VAR cc: INTEGER);
  BEGIN
    IF cc>HIGH(x) THEN RESIZE(x,HIGH(x)+17) END;
    x[cc]:=c; INC(cc);
  END put_s;

  PROCEDURE put(c: CHAR); BEGIN put_s(w,c,wc) END put;

  PROCEDURE new_word(VAR s: STRING; wc: INTEGER);
    VAR n, k: INTEGER;
  BEGIN
    IF wds THEN
      RESIZE(w,wc);
      IF wp>HIGH(words) THEN RESIZE(words,HIGH(words)+9) END;
      words[wp]^:=w^; INC(wp);
      NEW(w,16)
    ELSE k:=0; n:=HIGH(W);
      IF n<0 THEN n:=0 END;
      RESIZE(W, n+wc+1); W[n]:=' '; INC(n);
      WHILE (k<wc) DO W[n]:=w[k]; INC(n); INC(k) END;
    END;
  END new_word;

BEGIN
  dispose;
  IF esc? THEN esc:="\" ELSE esc:=CHAR(-1) END;
  wp:=0; NEW(w,16);
  IF wds THEN NEW(words,8) ELSE NEW(W) END;
  FOR i:=0 TO HIGH(set) DO set[i]:={} END;
  FOR i:=0 TO HIGH(pre) DO
    IF pre[i]#esc THEN n:=ORD(pre[i]); INCL(set[n DIV 32],n MOD 32) END;
  END;
  i:=0; wc:=0;
  LOOP                                                 -- main parsering
    IF (i>HIGH(s)) OR (s[i]=0c) THEN EXIT END;
    ch:=s[i];
    IF ch=' ' THEN                                     -- remove spaces
      WHILE (i<HIGH(s)) & (s[i]=' ') DO INC(i) END;
    ELSE                                               -- pars term
      IF (ORD(ch) MOD 32) IN set[ORD(ch) DIV 32] THEN  -- pars flags
        find_flags(ch,F); INC(i);
        WHILE (i<=HIGH(s)) & (s[i]#0c) & (s[i]#' ') DO
          IF (s[i]=esc) & (i<HIGH(s)) THEN
            chx:=s[i+1];
            IF (chx=' ') OR (chx=esc) THEN INC(i) END;
          END;
          n:=ORD(s[i]); INCL(F^.set[n DIV 32], (n MOD 32));
          INC(i);
        END;
      ELSE wc:=0;                                      -- word, equ...
        IF (ch=esc) & (i<HIGH(s)) THEN
          chx:=s[i+1]; n:=ORD(chx);
          IF (chx=' ') OR (chx=esc) OR ((n MOD 32) IN set[n DIV 32]) THEN
            INC(i)
          END
        END;
        LOOP                                           -- name or word
          put_s(w,s[i],wc); INC(i);
          IF (i>HIGH(s)) OR (s[i]=0c) OR (s[i]=' ') OR (equ? & (s[i]="="))
            THEN EXIT
          END;
          IF (s[i]=esc) & (i<HIGH(s)) THEN chx:=s[i+1];
            IF (chx=' ') OR (chx=esc) OR (chx="=") THEN INC(i) END
          END
        END;
        IF (i>HIGH(s)) OR (s[i]=0c) THEN
          put_s(w,0c,wc); RESIZE(w,wc);
          new_word(w,wc); EXIT
        END;
        IF s[i]="=" THEN
          NEW(x,16); n:=0; INC(i);      -- equation
          IF (i>HIGH(s)) OR (s[i]=0c) OR (s[i]=" ") THEN  -- empty body
            put_s(x,0c,n); RESIZE(x,n); put_s(w,0c,wc);
            new_equ(w,wc,x)
          ELSE                                            -- find separator
            IF (s[i]=esc) & (i<HIGH(s)) & (s[i+1]#0c) THEN
              chs:=s[i+1]; k:=0;                          -- "\" separator
              WHILE (k<HIGH(sep)) & (sep[k]#chs) DO INC(k,2) END;
              IF (k<HIGH(sep)) & (sep[k]=chs) THEN INC(i) END;
              chs:=" "
            ELSE
              chs:=s[i]; k:=0;
              WHILE (k<HIGH(sep)) & (sep[k]#chs) DO INC(k,2) END;
              IF (k<HIGH(sep)) & (sep[k]=chs) THEN
                IF k#HIGH(sep)-1 THEN chs:=sep[k+1] END;
                INC(i)
              ELSE chs:=" "
              END
            END;
            WHILE (i<HIGH(s)) & (s[i]#0c) & (s[i]#chs) DO  -- pars body
              IF (s[i]=esc) & (i<HIGH(s))
               & ((s[i+1]=chs) OR (s[i+1]=esc)) THEN INC(i)
              END;
              put_s(x,s[i],n); INC(i)
            END -- pars body
          END; -- equation
          IF s[i]=chs THEN INC(i) END; -- separator
          put_s(x,0c,n); RESIZE(x,n); put_s(w,0c,wc); -- put equation
          new_equ(w,wc,x);
        ELSE  -- it was word
          put_s(w,0c,wc); new_word(w,wc); -- put word
        END;  -- pars word or equ...
      END; -- pars term
    END;
  END; -- main parsering
  IF wds THEN
    IF wp>0 THEN RESIZE(words,wp) ELSE DISPOSE(words) END;
  ELSE
    NEW(words,1); words[0]^:=W^
  END;
END scan_string;

PROCEDURE del_word(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF (n<0) OR (n>HIGH(words)) THEN ASSERT(FALSE,4Fh) END;
  DISPOSE(words[n]);
  FOR i:=n TO HIGH(words)-1 DO
    words[i]^:=words[i+1]^;
  END;
  RESIZE(words,HIGH(words));
END del_word;

PROCEDURE pack_words(from,to: INTEGER);
  VAR i,j,n: INTEGER;
BEGIN
  IF from>to THEN i:=from; from:=to; to:=i END;
  IF (from<0) OR (to>HIGH(words)) OR (from=to) THEN RETURN END;
  n:=0;
  FOR i:=from TO to DO j:=j+HIGH(words[i])+1 END;
  n:=HIGH(words[from]);
  RESIZE(words[from],j+1);
  FOR i:=from+1 TO to DO
    words[from,n]:=' '; INC(n); j:=0;
    WHILE (j<HIGH(words[i])) DO
      words[from,n]:=words[i,j]; INC(j); INC(n);
    END;
  END;
  words[from,n]:=0c;
  n:=from+1; j:=to+1;
  WHILE (j<=HIGH(words)) DO words[n]^:=words[j]^; INC(j); INC(n) END;
  RESIZE(words,n);
END pack_words;

PROCEDURE flag(prefix,c: CHAR): BOOLEAN;
  VAR f: f_ptr;
BEGIN f:=flags;
  WHILE (f#NIL) & (f^.pre#prefix) DO f:=f^.next END;
  RETURN (f#NIL) & (ORD(c) MOD 32 IN f^.set[ORD(c) DIV 32])
END flag;

PROCEDURE string(VAL name: ARRAY OF CHAR; VAR s: STRING): BOOLEAN;
  VAR l,p: e_ptr;
BEGIN l:=equ; p:=NIL;
  WHILE (l#NIL) & (l^.name#name) DO p:=l; l:=l^.next END;
  IF l#NIL THEN s^:=l^.body^; RETURN TRUE END;
  env.get_str(name,s);
  RETURN env.done
END string;

PROCEDURE number(VAL name: ARRAY OF CHAR; VAR n: INTEGER): BOOLEAN;
  VAR s: STRING; p: INTEGER; done: BOOLEAN;
BEGIN
  IF NOT string(name,s) THEN RETURN FALSE END;
  p:=0;
  str.iscan(n,s,p,done);
  RETURN done
END number;

PROCEDURE dispose;
  VAR i: INTEGER; f: f_ptr; e: e_ptr;
BEGIN
  FOR i:=0 TO HIGH(words) DO DISPOSE(words[i]) END;
  DISPOSE(words);
  WHILE flags#NIL DO f:=flags; flags:=flags^.next; DISPOSE(f) END;
  WHILE equ#NIL DO
    e:=equ; equ:=equ^.next;
    DISPOSE(e^.body);
    DISPOSE(e^.name);
    DISPOSE(e)
  END;
END dispose;

PROCEDURE scan_parm;
  VAR s: STRING;
BEGIN
  env.get_str(env.args,s);
  IF env.done THEN
    scan_string(s,TRUE,TRUE,TRUE,'-+','""' "''")
  END
END scan_parm;

VAR c: INTEGER;

BEGIN
  c:=mem.credit;
  mem.set_credit(2);
  flags:=NIL; equ:=NIL; NEW(words);
  scan_parm;
  mem.set_credit(c)
END tskArgs.
