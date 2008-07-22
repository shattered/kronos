MODULE hi; (* Leo 31-Jul-86. (c) KRONOS *)
           (* Ned 29-Nov-88. (c) KRONOS *)
        (*$N+ Ned 16-May-89. (c) KRONOS *)
         (* Hady. 07-Oct-89. (c) KRONOS *)

IMPORT   ref: xRef;
IMPORT  args: tskArgs;
IMPORT   tsk: Tasks;
IMPORT   str: Strings;
IMPORT   std: StdIO;
IMPORT   env: tskEnv;
IMPORT   lex: Lexicon;

IMPORT  thr: Threads;
IMPORT  sig: Signals;
IMPORT  sys: SYSTEM;
IMPORT  key: Keyboard;

TYPE str1024=ARRAY [0..1023] OF CHAR;
     str32  =ARRAY [0..  31] OF CHAR;

VAR h: str1024;

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
    IF (c<=37c) OR (c>177c) & (c<=237c) THEN str.delete(W,l,1) END
  END;
END GW;

PROCEDURE ScanModule(): BOOLEAN;
  VAR l: INTEGER;       line: INTEGER;
      n: INTEGER;        col: INTEGER;
     pc: INTEGER;   N,W,M,fn: str32;
    pos: INTEGER;       done: BOOLEAN;
     cu: ref.cu_ptr;    proc: ref.obj_ptr;

  VAR i: INTEGER;
BEGIN
  REPEAT GW(M); l:=str.len(M) UNTIL (l=0) OR (M[l-1]=':');
  IF l=0 THEN RETURN TRUE END;
  M[l-1]:=0c;
  std.print("%-16.16s| ",M);
  col:=18;
  ref.read_cu(ref.main,cu,M,TRUE);
  IF NOT ref.done THEN
    std.perror(ref.error,"%s %%s\n",ref.note);
    std.print("%-16s| ","   "); col:=18;
    LOOP
      GW(W);
      IF (W[0]='{') OR (W[0]=0c) THEN EXIT END;
      GW(N);
      INC(col,str.len(W)+str.len(N)+4);
      IF col>=78 THEN std.print("\n");
        std.print("%-16s| ","   "); col:=18;
        INC(col,str.len(W)+str.len(N)+4);
      END;
      std.print("%s %s <- ",W,N);
    END; std.print("\n"); RETURN W[0]=0c
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
    END;
    INC(col,str.len(W)+3);
    IF col>=77 THEN std.print("\n%-16s| ","  "); col:=18; INC(col,str.len(W)+3) END;
    std.print("%s <- ",W);
  END; std.print("\n");
  ref.exit_cu(cu);
  RETURN M[0]=0c;
END ScanModule;

PROCEDURE SubStr(VAR from: ARRAY OF CHAR;
                  pos,len: INTEGER;
                 VAR   to: ARRAY OF CHAR);
BEGIN
  str.sub_str(to,from,pos,len);
  str.delete(from,pos,len);
END SubStr;

PROCEDURE cause(VAR s: ARRAY OF CHAR);
  VAR i,err: INTEGER; done: BOOLEAN;
      cau: ARRAY [0..63] OF CHAR;
BEGIN
  i:=str.len(s)-1;
  REPEAT DEC(i) UNTIL (i<0) OR (s[i]=']');
  IF i=0 THEN RETURN END;
  s[i+1]:=0c;
  REPEAT DEC(i) UNTIL (i<0) OR (s[i]='['); INC(i);
  IF i=0 THEN RETURN END;
  str.iscan(err,s,i,done);
  IF NOT done THEN RETURN END;
  lex.perror(cau,err,' "%%s"');
  str.app(s,cau)
END cause;

PROCEDURE history(SEQ args: sys.WORD);
  VAR i: INTEGER; c: ARRAY [0..255] OF CHAR;
BEGIN
  i:=1;
  WHILE (i<str.len(h)) & (h[i]#'#') DO INC(i) END;
  SubStr(h,0,i,c);
  cause(c);
  std.print("%s\n",c);
  str.delete(h,0,1);
  h[str.len(h)-1]:=0c;
  REPEAT UNTIL ScanModule();
END history;

VAR break: key.BREAK;
     halt: sig.SIGNAL;
     hprs: thr.THREAD;

PROCEDURE holder(n: INTEGER);
BEGIN
  CASE n OF
    |0: sig.send(halt);
    |2:
  ELSE
    IF break#holder THEN break(n) END
  END
END holder;

PROCEDURE final;
BEGIN
  IF break#holder THEN
    key.user_break(break)
  END
END final;

PROCEDURE start;
  VAR done: BOOLEAN;
     i,stk: INTEGER;
         s: STRING;
BEGIN
  sig.new_signal(halt,0,done);
  IF NOT done THEN
    std.print('#no memory'); HALT(1)
  END;
  env.get_str(env.stk,s);
  IF NOT env.done THEN stk:=2
  ELSE
    i:=0; str.iscan(stk,s,i,done);
    IF NOT done THEN stk:=2 END
  END;
  IF stk<2 THEN stk:=2 END; stk:=stk*256; -- stack size
  key.nop;
  break:=key.state^.ubrk;
  env.final(final);
  key.user_break(holder);
  thr.xfork(hprs,history,stk,halt,done);
  IF NOT done THEN
    std.print('#no memory'); HALT(1)
  END
END start;

PROCEDURE task_history(id: INTEGER);
  VAR t: tsk.TASK; n: INTEGER;
BEGIN
  tsk.open(t,tsk.task0,id);
  IF NOT tsk.done THEN
    std.print('no such task %d\n',id); HALT
  END;
  tsk.history(t,n,h);
  IF NOT tsk.done THEN std.print('illegal state\n'); HALT END;
  tsk.close(t);
END task_history;

PROCEDURE Help;
BEGIN
  std.print(
      '   "hi"   history utility program    (c) KRONOS\n'
      'usage:\n'
      '    hi [task_no] [-h]\n')
END Help;

VAR done: BOOLEAN;
     i,j: INTEGER;
       s: STRING;

BEGIN
  IF args.flag('-','h') THEN Help; HALT END;
  IF HIGH(args.words)<0 THEN
    env.get_str("HISTORY",s);
    IF env.done THEN str.copy(h,s)
    ELSE std.print("No history"); HALT
    END;
  ELSE
    j:=0;
    str.iscan(i,args.words[0],j,done);
    IF done THEN task_history(i) ELSE Help; HALT END;
  END;
  start; sig.wait(halt);
  thr.cause(hprs,i);
  IF (i#47h) & (i#0) THEN HALT(i) END
END hi.
