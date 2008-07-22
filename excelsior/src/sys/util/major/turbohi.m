MODULE turbohi; (* Leo        31-Jul-86. (c) KRONOS *)
                (* Ned        29-Nov-88. (c) KRONOS *)
                (* Ned        16-May-89. (c) KRONOS *)
                (* Hady       07-Oct-89. (c) KRONOS *)
                (* Ned        18-Oct-90. (c) KRONOS *)
                (* Hady & Leg 26-Nov-90. (c) KRONOS *)

IMPORT   tsk: tskEnv;
IMPORT   ref: xRef;
IMPORT   str: Strings;
IMPORT    ex: myEditor;
IMPORT   lex: Lexicon;

TYPE str1024=ARRAY [0..1023] OF CHAR;
     str256 =ARRAY [0.. 255] OF CHAR;
     str32  =ARRAY [0..  31] OF CHAR;

VAR h,name: str1024;
     marks: INTEGER;

PROCEDURE GetWord(VAR from,to: ARRAY OF CHAR);
  VAR i,j,l,m: INTEGER;
BEGIN i:=0; l:=str.len(from);
  WHILE (i<l) & (from[i]=' ') DO INC(i) END;
  j:=0; m:=i+HIGH(to);
  IF m>l THEN m:=l END;
  WHILE (i<m) & (from[i]#0c) & (from[i]#' ') DO
    to[j]:=from[i]; INC(i); INC(j);
  END; to[j]:=0c;
  IF i>0 THEN str.delete(from,0,i) END;
END GetWord;

PROCEDURE GW(VAR W: ARRAY OF CHAR);
  VAR l: INTEGER; c: CHAR;
BEGIN
  GetWord(h,W);
  FOR l:=str.len(W)-1 TO 0 BY -1 DO
    c:=W[l];
    IF ORD(c) MOD 200b<=37b THEN str.delete(W,l,1) END
  END;
END GW;

PROCEDURE ScanModule(): BOOLEAN;
  VAR l: INTEGER;       line: INTEGER;
      n: INTEGER;
     pc: INTEGER;      N,W,M: str32;
    pos: INTEGER;       done: BOOLEAN;
     cu: ref.cu_ptr;    proc: ref.obj_ptr;

  VAR i: INTEGER;
BEGIN
  REPEAT GW(M); l:=str.len(M) UNTIL (l=0) OR (M[l-1]=':');
  IF l=0 THEN RETURN TRUE END;
  M[l-1]:=0c;
  ex.message(FALSE,'...%s',M);
  IF M#name THEN RETURN FALSE END;
  ref.read_cu(ref.main,cu,M,TRUE);
  IF NOT ref.done THEN
    RETURN TRUE
  END;
  LOOP
    GW(W);
    IF (W[0]='{') OR (W[0]=0c) THEN EXIT END;
    GW(N);
    IF (N[0]='[') & (N[str.len(N)-1]=']') THEN
      str.delete(N,0,1); N[str.len(N)-1]:=0c;
      i:=0;
      str.iscan(pc,N,i,done);
      IF NOT done THEN pc:=-1; END;
    ELSE pc:=-1
    END;
    i:=0;
    str.iscan(n,W,i,done);
    IF done THEN
      proc:=cu^.proc_tab[n];
      IF n=0 THEN str.copy(W,"BEGIN")
      ELSE ref.id_str(cu^.names,proc^.id,W);
      END;
      IF pc>=0 THEN line:=-1;
        IF proc^.locs#NIL THEN
          ref.text_pos(proc^.locs^.xpos,pc,line,pos);
        END;
        IF line>0 THEN str.append(W,":%d.%d",line,pos);
        ELSE           str.append(W,"[%$4#h]",pc)
        END
      END;
      ex.mark(line,pos,W); INC(marks);
    END;
  END;
--ref.exit_cu(cu);
  RETURN M[0]=0c;
END ScanModule;

PROCEDURE SubStr(VAR from: ARRAY OF CHAR;
                  pos,len: INTEGER;
                 VAR   to: ARRAY OF CHAR);
BEGIN
  str.sub_str(to,from,pos,len);
  str.delete(from,pos,len);
END SubStr;

PROCEDURE get_name(VAR name: ARRAY OF CHAR);
  VAR x: str256;
   line: INTEGER;
   last: INTEGER;
    pos: INTEGER;
    hig: INTEGER;

  CONST imp = 'IMPLEMENTATION';
        def = 'DEFINITION';
        mod = 'MODULE';

  PROCEDURE err;
  BEGIN ex.message(FALSE,'illegal module header %d.%d',line,pos); HALT END err;

  PROCEDURE next_line;
  BEGIN
    pos:=0;
    REPEAT
      INC(line);
      IF line>last THEN err END;
      ex.jump(line);
      ex.get(x,hig); DEC(hig)
    UNTIL hig>=0
  END next_line;

  PROCEDURE skip_comment;
    VAR ch: CHAR;
  BEGIN
    LOOP
      IF pos>hig THEN next_line END;
      ch:=x[pos];
      IF    ch='*' THEN
        IF (pos<hig) & (x[pos+1]=')') THEN INC(pos,2); RETURN END;
      ELSIF ch='(' THEN
        IF (pos<hig) & (x[pos+1]='*') THEN
          INC(pos,2); skip_comment
        END;
      END;
      INC(pos);
    END;
  END skip_comment;

  PROCEDURE get_sy(VAR t: ARRAY OF CHAR);
    VAR i: INTEGER; ch: CHAR;
  BEGIN
    t:=''; i:=0;
    LOOP
      str.skip(x,pos,' ');
      IF pos>hig THEN next_line
      ELSE
        LOOP
          ch:=x[pos];
          IF    ch='-' THEN
            INC(pos); IF pos>hig THEN err END;
            IF x[pos]#'-' THEN err ELSE next_line; EXIT END;
          ELSIF (ch=';') OR (ch='[') THEN
            IF i>0 THEN EXIT ELSE err END;
          ELSIF ch='(' THEN
            INC(pos); IF pos>hig THEN err END;
            IF x[pos]='*' THEN INC(pos); skip_comment; EXIT ELSE err END;
          ELSIF ch=' ' THEN EXIT
          ELSE t[i]:=ch; INC(i);
          END;
          INC(pos);
          IF pos>hig THEN next_line; EXIT END;
        END;
        IF i>0 THEN t[i]:=0c; RETURN END;
      END;
    END;
  END get_sy;

VAR s: str256;

BEGIN
  line:=-1; pos:=0; hig:=-1; last:=ex.last();
  get_sy(s);
  IF (s=def) OR (s=imp) THEN get_sy(s) END;
  IF s#mod THEN err END;
  get_sy(name);
END get_name;

PROCEDURE cause(VAR s: ARRAY OF CHAR);
  VAR i,err: INTEGER; done: BOOLEAN;
      cau: ARRAY [0..63] OF CHAR;
BEGIN
  i:=str.len(s)-1;
  REPEAT DEC(i) UNTIL (i<0) OR (s[i]='['); INC(i);
  IF i=0 THEN RETURN END;
  str.iscan(err,s,i,done);
  IF NOT done THEN RETURN END;
  lex.perror(cau,err,' "%%s"');
  str.app(s,cau)
END cause;

VAR c: str1024;
    i: INTEGER;
    j: INTEGER;
    s: STRING;

BEGIN
  tsk.get_str(tsk.history,s);
  IF NOT tsk.done THEN ex.message(TRUE,"No history"); HALT END;
  str.copy(h,s);
  j:=str.len(h); i:=1;
  WHILE (i<j) & (h[i]#"#") DO INC(i) END;
  SubStr(h,0,i,c);
  cause(c);
  str.delete(h,0,1);
  h[str.len(h)-1]:=0c;
  get_name(name);  marks:=0;
  REPEAT UNTIL ScanModule();
  IF marks=0 THEN
    ex.message(FALSE,'no entries of module "%s" in history',name)
  END
END turbohi.
