IMPLEMENTATION MODULE Shell; (* Hady. 20-Aug-90. (c) KRONOS *)
                             (* Hady. 28-Nov-91. (c) KRONOS *)

IMPORT  sys: SYSTEM;     IMPORT  err: defErrors;
IMPORT   os: osKernel;   IMPORT  exe: Execute;
IMPORT  tsk: Tasks;      IMPORT  def: defTasks;
IMPORT  usr: Users;      IMPORT  env: tskEnv;
IMPORT  lex: Lexicon;    IMPORT  fmt: Formats;
IMPORT  str: Strings;    IMPORT  sta: Statistics;
IMPORT  reg: regExpr;    IMPORT  sci: ASCII;
IMPORT  clk: Time;       IMPORT  mem: Heap;
IMPORT  bio: BIO;

IMPORT  Terminal, Keyboard;

TYPE str80 = ARRAY [0..79] OF CHAR;
     str8  = ARRAY [0..7 ] OF CHAR;

---------------------------- LIBRARY ---------------------------
                            ---------

PROCEDURE comp(VAL patt: ARRAY OF CHAR;
               VAL sour: ARRAY OF CHAR;
                  sf,st: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF sf>st THEN RETURN FALSE END;
  i:=0;
  WHILE (i<=HIGH(patt)) & (patt[i]#0c) & (sf<=st) DO
    IF sour[sf]#patt[i] THEN RETURN FALSE END;
    sf:=sf+1; i:=i+1
  END;
  RETURN (sf>st) & ((i>HIGH(patt)) OR (patt[i]=0c))
END comp;

PROCEDURE skip(VAL s: ARRAY OF CHAR; VAR p: INTEGER);
BEGIN
  WHILE (p<=HIGH(s)) & (s[p]=" ") DO p:=p+1 END
END skip;

PROCEDURE skip_name(VAL s: ARRAY OF CHAR; fr: INTEGER): INTEGER;
BEGIN
  WHILE (fr<=HIGH(s)) & (s[fr]#" ") & (s[fr]#0c) DO
    fr:=fr+1
  END;
  RETURN fr
END skip_name;

CONST NULL=CHAR(-1);

PROCEDURE equation(VAL cmd: ARRAY OF CHAR;
                 VAR ns,nf: INTEGER;  -- name start & finish
                 VAR bs,bf: INTEGER;  -- body start & finish
                 VAR   del: CHAR;     -- delimiter
                 VAR fault: BOOLEAN): BOOLEAN;

BEGIN
  IF ns<0 THEN ns:=0 END; skip(cmd,ns);
  IF (ns>=HIGH(cmd)) OR (cmd[ns]=0c) THEN RETURN FALSE END;

  nf:=ns;
  WHILE (nf<=HIGH(cmd)) &
        (cmd[nf]#0c ) & (cmd[nf]#"}") &
        (cmd[nf]#"=") & (cmd[nf]#" ") DO nf:=nf+1
  END;
  IF (nf-ns<=0)                      THEN RETURN FALSE END;
  IF (nf>HIGH(cmd)) OR (cmd[nf]#"=") THEN RETURN FALSE END;
  IF (nf>0) & (cmd[nf-1]="\")        THEN RETURN FALSE END;
  bs:=nf+1; bf:=bs;
  IF (bf>HIGH(cmd)) OR (cmd[bf]=0c)  THEN RETURN TRUE END;
  IF (cmd[bf]="'") OR (cmd[bf]='"')  THEN
    del:=cmd[bf]; bf:=bf+1; bs:=bf;
    str.search(cmd,bf,del);
    IF (bf>HIGH(cmd)) OR (cmd[bf]#del) THEN
      print('not pared delimeter\n'); fault:=TRUE; RETURN TRUE
    END;
  ELSE
    WHILE (bf<=HIGH(cmd)) & (cmd[bf]#0c ) &
          (cmd[bf]#"}")   & (cmd[bf]#" ") DO bf:=bf+1
    END;
    del:=NULL
  END;
  RETURN TRUE
END equation;

CONST _trap = {0}; _break = {1}; _ipr = {2}; _his = {3}; _cfi = {4} ;

PROCEDURE pack_echo(VAL s: ARRAY OF CHAR): BITSET;
  VAR e: BITSET; i,no: INTEGER; ch: CHAR;
BEGIN
  e:={}; i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO
    ch:=sci.CAPITAL(s[i]);
    IF    ch="0" THEN e:={}
    ELSIF ch="1" THEN e:=BITSET(-1)
    ELSIF ch='T' THEN e:=e+_trap
    ELSIF ch='B' THEN e:=e+_break
    ELSIF ch='I' THEN e:=e+_ipr
    ELSIF ch='H' THEN e:=e+_his
    ELSIF ch='@' THEN e:=e+_cfi
    END;
    INC(i)
  END;
  RETURN e
END pack_echo;

PROCEDURE _get_echo(): BITSET;
  VAR e: str8;
BEGIN
  get_echo(e); RETURN pack_echo(e)
END _get_echo;

WITH STORAGE (NEW: mem.allocate;
          DISPOSE: mem.deallocate;
           RESIZE: mem.reallocate);

PROCEDURE no_mem;
BEGIN
  print('#no memory\n')
END no_mem;

PROCEDURE mem_error(): BOOLEAN;
BEGIN
  IF mem.done THEN RETURN FALSE END;
  no_mem; RETURN TRUE
END mem_error;

TYPE pr_ptr = POINTER TO pr_rec;
     pr_rec = RECORD
                 buff: STRING;
                 bpos: INTEGER;
                fault: BOOLEAN
               END;

PROCEDURE swrite(  dir: pr_ptr;
                 VAL s: ARRAY OF CHAR;
                   pos: INTEGER;
                   len: INTEGER);
BEGIN
  WITH dir^ DO
    IF fault THEN RETURN END;
    IF bpos+len>=HIGH(buff) THEN
      RESIZE(buff,bpos+len+1);
      fault:=NOT mem.done;
      IF fault THEN RETURN END
    END;
    WHILE len>0 DO
      buff[bpos]:=s[pos]; INC(pos); INC(bpos); DEC(len)
    END
  END
END swrite;

PROCEDURE sprint(VAR s: STRING;
              VAR done: BOOLEAN;
                 VAL f: ARRAY OF CHAR;
                 SEQ a: sys.WORD);
  VAR io: pr_rec;
BEGIN
(*$<$U+*)
  WITH io DO
    buff^:=s^; bpos:=0; fault:=FALSE
  END;
  fmt.format(sys.ADR(io),swrite,f,a);
  s^:=io.buff^;
  done:=NOT io.fault;
  IF done & (HIGH(s)>=0) THEN s[HIGH(s)]:=0c END
(*$>*)
END sprint;

PROCEDURE simage(VAR s: STRING;
              VAR done: BOOLEAN;
              VAR  pos: INTEGER;
                 VAL f: ARRAY OF CHAR;
                 SEQ a: sys.WORD);
  VAR io: pr_rec;
BEGIN
(*$<$U+*)
  WITH io DO
    buff^:=s^; fault:=NOT done; bpos:=pos
  END;
  fmt.format(sys.ADR(io),swrite,f,a);
  s^:=io.buff^;
  done:=NOT io.fault; pos:=io.bpos;
  IF done & (HIGH(s)>=0) THEN s[HIGH(s)]:=0c END
(*$>*)
END simage;

------------------------- ERROR REPORTS ------------------------
                         ---------------

PROCEDURE perror(code: INTEGER; VAL f: ARRAY OF CHAR; SEQ arg: sys.WORD);
  VAR e,s0: STRING; done: BOOLEAN;
BEGIN
  lex.get(lex.sysmsg,code,e);
  IF NOT lex.done THEN
    print("%s\n",e); RETURN
  END;
  NEW(s0); sprint(s0,done,f,arg);
  IF NOT done THEN no_mem; RETURN END;
  print(s0,e); DISPOSE(s0)
END perror;

PROCEDURE bio_error(VAL op: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF bio.done THEN RETURN FALSE END;
  perror(bio.error,'#%s(%s): %%s\n',op,bio.ename);
  RETURN TRUE
END bio_error;

CONST _exe_run = 0;
      _exe_cd  = 1;
      _exe_env = 2;

PROCEDURE exe_error(op: INTEGER): BOOLEAN;
  VAR opn: str8;
BEGIN
  IF exe.done THEN RETURN FALSE END;
  CASE op OF
    |_exe_run: opn:='run'
    |_exe_cd : opn:='go to'
    |_exe_env: opn:='env'
  ELSE
    opn:='';
  END;
  IF exe.note#"" THEN
    perror(exe.error,'#%s %s: %%s\n',opn,exe.note)
  ELSE
    perror(exe.error,'#%s: %%s\n',opn)
  END;
  RETURN TRUE
END exe_error;

----------------------------- SHELL ----------------------------
                             -------

VAR cmd_buff, cfi_buff: STRING;

PROCEDURE show_history;
  VAR his: STRING;
BEGIN
  env.get_str("HISTORY",his);
  IF env.done THEN
    print('%s\n',his)
  ELSE
    print('#no history\n')
  END;
END show_history;

PROCEDURE run_task(VAL tname: ARRAY OF CHAR;
                   VAL  args: ARRAY OF CHAR;
                   VAL alias: ARRAY OF CHAR;
                       chenv: exe.ENV;
                   VAR  wait: BOOLEAN;
                   VAR  kcfi: BOOLEAN;
                        echo: BITSET): INTEGER;

  VAR res: INTEGER;

BEGIN
  res:=0; kcfi:=FALSE;
  exe.run_task(tname, chenv, alias, args, wait);
  IF exe_error(_exe_run) THEN RETURN exe.error END;
  IF exe.cause=exe.continue THEN
    wait:=TRUE; RETURN res
  END;
  wait:=FALSE;
  IF    exe.cause*exe.awake#{} THEN
    IF (echo*_ipr#{}) & NOT env.ipr() THEN print('#AWAKE %d\n',exe.task) END
  ELSIF exe.cause*exe.break#{} THEN
    IF echo*_break#{} THEN
      IF exe.error=50h THEN
        print('\n#BREAK\n');
        kcfi:=(exe.cause*exe.cfi#{})
      ELSE
        perror(exe.error,'\n#TASK %d:  HALT(%hh) %%s\n',exe.task,exe.error)
      END
    END;
    IF echo*_his #{} THEN show_history END;
    res:=exe.error
  ELSIF exe.cause*exe.abort#{} THEN
    IF echo*_trap#{} THEN
      perror(exe.error,"\n#TASK %d:  TRAP(%hh) %%s\n",exe.task,exe.error)
    END;
    IF echo*_his #{} THEN show_history END;
    res:=exe.error
  END;
  RETURN res
END run_task;

PROCEDURE show_task(t: tsk.TASK; use,mem,tt: BOOLEAN);
  VAR s: STRING;
      p: INTEGER;
    pri: BOOLEAN;
   done: BOOLEAN;
      w: STRING;
      U: usr.USER;
    val: INTEGER;
    u,g: ARRAY [0..7] OF CHAR;
   user: INTEGER;
BEGIN
  p:=0; NEW(s); done:=TRUE;
  tsk.get_attr(t,tsk.a_id,val);
  simage(s,done,p,'%04d  ',val);
  tsk.get_attr(t,tsk.a_user,user);
  usr.unpack(user,U.usr,U.gro,U.priv);
  IF U.priv THEN
       simage(s,done,p,'+ ')
  ELSE simage(s,done,p,'  ')
  END;
  tsk.get_attr(t,tsk.a_status,val);
  CASE val OF
    |def.new    : simage(s,done,p,'%-4s ','new');
    |def.loaded : simage(s,done,p,'%-4s ','load');
    |def.loaderr,
     def.ready  : simage(s,done,p,'%-4s ','    ');
    |def.running: simage(s,done,p,'%-4s ','run');
    |def.stopped: simage(s,done,p,'%-4s ','stop');
    |def.killed : simage(s,done,p,'%-4s ','kill')
  ELSE
                  simage(s,done,p,'**%d** ',val)
  END;
  IF use THEN
    usr.get_user(U);
    IF U.done THEN simage(s,done,p,'%-5s',  U.name)
    ELSE           simage(s,done,p,' %03d ',U.usr )
    END;
    IF NOT done THEN DISPOSE(s); no_mem; RETURN END;
    usr.get_group(U);
    IF U.done THEN simage(s,done,p,' %-5s  ',  U.name)
    ELSE           simage(s,done,p,'  %03d   ',U.gro )
    END;
  END;
  IF mem THEN
    tsk.get_attr(t,tsk.a_mem,val);
    simage(s,done,p,'%3dK+%04d  ',val DIV 256,val*4 MOD 1024);
  END;
  IF tt THEN
    tsk.get_env(t,env.tty,0,w,pri);
    IF tsk.done THEN simage(s,done,p,'%-10s  ',w)
    ELSE             simage(s,done,p,'%-10s  ','')
    END
  END;
  tsk.get_env(t,env.name,0,w,pri);
  IF tsk.done THEN simage(s,done,p,'%-12s  ',w)
  ELSE             simage(s,done,p,'%-12s  ','')
  END;
  tsk.get_env(t,env.info,0,w,pri);
  IF tsk.done THEN simage(s,done,p,'%s',w) END;
  IF done       THEN print("%s",s) ELSE no_mem END;
  IF HIGH(s)>=0 THEN DISPOSE(s)    END
END show_task;

PROCEDURE show_tasks(all, usr, mem, tt: BOOLEAN);

  VAR my: INTEGER;

  PROCEDURE iter(papa: tsk.TASK; id: INTEGER);
    VAR user: INTEGER; t: tsk.TASK;
  BEGIN
    WHILE id>=0 DO
      tsk.open(t,papa,id);
      IF NOT tsk.done THEN RETURN END;
      tsk.son(t,id);
      IF tsk.done THEN iter(t,id) END;
      tsk.brother(t,id);
      IF NOT tsk.done THEN id:=-1 END;
      tsk.get_attr(t,tsk.a_user,user);
      IF all OR (my DIV 100h = user DIV 100h) THEN
        show_task(t, usr, mem, tt); print("\n");
      END;
      tsk.close(t);
    END;
  END iter;

  VAR id: INTEGER; t: tsk.TASK;

BEGIN
  IF all THEN print('...TASKS...\n') END;
  tsk.get_attr(tsk.self,tsk.a_user,my);
  id:=0;
  IF tsk.done THEN iter(tsk.task0,id) END;
  IF all THEN print('\n') END
END show_tasks;

PROCEDURE shell(VAL cmd,args: ARRAY OF CHAR): BOOLEAN;

  VAR s,f: INTEGER;

  PROCEDURE ps;
    VAR use,mem,all,tty: BOOLEAN;
          no: INTEGER;
        task: tsk.TASK;
  BEGIN
    use:=FALSE; mem:=FALSE;
    tty:=FALSE; all:=FALSE;
    WHILE (s<=HIGH(args)) & (args[s]="-") DO
      f:=skip_name(args,s);
      IF    comp("-usr",args,s,f-1) THEN use:=TRUE
      ELSIF comp("-tty",args,s,f-1) THEN tty:=TRUE
      ELSIF comp("-mem",args,s,f-1) THEN mem:=TRUE
      ELSIF comp("-all",args,s,f-1) THEN all:=TRUE
      ELSIF comp("-l"  ,args,s,f-1) THEN
        s:=f; use:=TRUE; tty:=TRUE; mem:=TRUE
      END;
      s:=f; skip(args,s)
    END;
    IF (s<HIGH(args)) & NOT all THEN
      str.iscan(no,args,s,all);
      IF all THEN
        REPEAT
          tsk.open(task,(tsk.task0),no);
          IF tsk.done THEN
            show_task(task,use,mem,tty);
            print('\n');
            tsk.close(task)
          END;
          str.iscan(no,args,s,all);
        UNTIL NOT all;
        RETURN
      END
    END;
    show_tasks(all,use,mem,tty)
  END ps;

  PROCEDURE cd;
    VAR cdn: STRING; done: BOOLEAN;
  BEGIN
    IF (s>HIGH(args)) OR (args[s]=0c) THEN
      print('#current directory: ');
      env.get_str(env.cd,cdn);
      IF env.done THEN print('%s\n',cdn)
      ELSE             print('/\n')
      END
    ELSE
      NEW(cdn); sprint(cdn,done,'%..*s',s,args);
      IF done THEN
        exe.change_env(-1,env.cd,cdn,FALSE);
        DISPOSE(cdn);
        IF exe_error(_exe_cd) THEN END
      END
    END
  END cd;

  PROCEDURE his;
    VAR no: INTEGER;
     cause: INTEGER;
      done: BOOLEAN;
      task: tsk.TASK;
      bump: STRING;
  BEGIN
    IF (s<HIGH(args)) & (args[s]#0c) THEN
      str.iscan(no,args,s,done);
      IF done THEN
        tsk.open(task,(tsk.task0),no);
        IF tsk.done THEN
          NEW(bump,512); IF mem_error() THEN RETURN END;
          tsk.history(task,cause,bump);
          IF tsk.done THEN
            print('#TASK %d: cause %d\n%s\n',no,cause,bump)
          ELSE
            perror(tsk.error,'#history: %%s\n')
          END;
          DISPOSE(bump);
          RETURN
        ELSE
          perror(tsk.error,"#[%d]: %%s\n",no)
        END
      END
    END;
    show_history
  END his;

  PROCEDURE readpwd(VAR s: ARRAY OF CHAR);
    VAR i: INTEGER;
       ch: CHAR;
  BEGIN
    print("password:");
    i:=0;
    REPEAT
      read(ch);
      IF (ch>40c) & (ch<177c) THEN s[i]:=ch; INC(i) END;
    UNTIL (ch=15c) OR (i=HIGH(s));
    s[i]:=0c;
    print("\n")
  END readpwd;

  PROCEDURE su(on: BOOLEAN);
    VAR t: os.task_ptr;
        u: usr.USER;

    PROCEDURE ask_pwd(): BOOLEAN;
      VAR pwd: ARRAY [0..7] OF CHAR;
    BEGIN
      u.usr:=0;
      usr.get_user(u);
      readpwd(pwd);
      RETURN u.pass=pwd
    END ask_pwd;

    VAR save: INTEGER;

  BEGIN
    t:=os.self();
    IF on THEN
      save:=t^.user;
      t^.user:=INTEGER(BITSET(t^.user)+{7});
      usr.unpack(t^.user,u.usr,u.gro,u.priv);
      usr.get_user(u);
      IF NOT u.priv & NOT ask_pwd() THEN
        t^.user:=save; print("illegal password\n")
      END
    ELSE
      t^.user:=INTEGER(BITSET(t^.user)-{7})
    END;
  END su;

  PROCEDURE mem;

    PROCEDURE p(VAL s: ARRAY OF CHAR; i: INTEGER);
    BEGIN
      i:=i*4;
      print("   %s  %4dK+%04d\n",
                 s,i DIV 1024,i MOD 1024)
    END p;

    VAR c,t,f: INTEGER;

  BEGIN
    print('\n');
    print('...MEMORY TOTAL...\n');
    sta.get(sta.mem_core,c);
    sta.get(sta.mem_total,t);
    sta.get(sta.mem_free,f);
    p("available",c+t);
    p("core     ",c);
    p("free     ",f);
    p("busy     ",t-f);
    print('\n')
  END mem;

  PROCEDURE set;

    CONST fmt = '%c%-16.16s  "%s"\n';

    VAR body: ARRAY [0..59] OF CHAR;
         pri: BOOLEAN;
         pre: CHAR;

    PROCEDURE copy(VAL d: ARRAY OF CHAR);
      VAR i: INTEGER;
         ch: CHAR;
    BEGIN
      i:=0;
      WHILE (i<HIGH(body)) & (i<HIGH(d)) & (d[i]#0c) DO
        ch:=d[i];
        IF {sci.control}*sci.KIND(ch)#{} THEN ch:="?" END;
        body[i]:=ch; INC(i)
      END;
      body[i]:=0c
    END copy;

    PROCEDURE _set;

      PROCEDURE p(VAL name,def: ARRAY OF CHAR);
        VAR s: STRING;
      BEGIN
        pri:=FALSE;
        env.get_env(name,-1,s,pri);
        IF env.done  THEN copy(s)
        ELSIF def="" THEN RETURN
        ELSE copy(def)
        END;
        IF pri THEN pre:='.' ELSE pre:=' ' END;
        print(fmt,pre,name,body)
      END p;

    BEGIN
      p("USER" ,"") ;  p("HOME" ,"") ; p("NAME",""); p("SHELL","");
      p("TASK" ,"") ;  p("FATHER",""); p("BASE",""); p("SON"  ,"");
      p("STK"  ,"4");  p("CEMETRY","1");
      p("ALIAS","") ;  p("CD" ,"");
      p("BIN","");  p("SYM","");  p("ETC","");  p("REF","");
      p("TTY","");  p("KEY","");
      p("PROMPT",""); p("INSERT","1"); p("ECHO","0"); p("BELL","1")
    END _set;

    VAR i,j: INTEGER;
          e: reg.EXPR;
         s0: STRING;
        n,b: STRING;

  BEGIN
    i:=s;
    IF (i>=HIGH(args)) OR (args[i]=0c) THEN _set; RETURN END;
    j:=skip_name(args,i);
    NEW(s0,j-i+1); IF mem_error() THEN RETURN END;
    str.sub_str(s0,args,i,j-i);
    reg.compile(s0,e);
    IF NOT reg.done THEN
      perror(reg.error,"#%%s\n");
      i:=reg.epos;
      IF i<0 THEN i:=0 ELSIF i>80 THEN i:=80 END;
      print('#"%s"\n#%.*c*^*\n',s0,i," ");
      DISPOSE(s0); RETURN
    END;
    DISPOSE(s0);
    i:=0;
    env.env_entry(i,-1,n,b,pri);
    WHILE env.done DO
      IF reg.match(e,n,0) THEN
        copy(b);
        IF pri THEN pre:='.' ELSE pre:=' ' END;
        print(fmt,pre,n,body)
      END;
      INC(i);
      env.env_entry(i,-1,n,b,pri)
    END
  END set;

  PROCEDURE delay;
    VAR sec: INTEGER;
       done: BOOLEAN;
  BEGIN
    IF (s>HIGH(args)) OR (args[s]=0c) THEN sec:=1
    ELSE
      str.iscan(sec,args,f,done);
      IF NOT done THEN sec:=1 END;
    END;
    clk.delay(sec,clk.sec)
  END delay;

  PROCEDURE get_word(VAR word: ARRAY OF CHAR);
  BEGIN
    f:=skip_name(args,s);
    str.sub_str(word,args,s,f-s);
    skip(args,f); s:=f
  END get_word;

  PROCEDURE mount;
    VAR ro: BOOLEAN;
      only: ARRAY [0..3] OF CHAR;
       dev: str80;
       lab: str80;
      home: str80;
  BEGIN
    get_word(home); get_word(dev); get_word(only);
    ro:=(only#"-ro");
    bio.mount(home,dev,'',lab,ro);
    IF bio.done THEN
      print('#"%s" [%s] mounted at "%s"\n',dev,lab,home);
    ELSE
      print("#can't"' mount "%s" at "%s":',home,dev);
      perror(bio.error,"%%s\n")
    END
  END mount;

  PROCEDURE unmount;
    VAR home: str80;
  BEGIN
    get_word(home); bio.unmount(home,1);
    IF NOT bio.done THEN
      print("can't"' unmount "%s":',home);
      perror(bio.error,"%%s\n")
    END
  END unmount;

  PROCEDURE task_controll(op: INTEGER);
    VAR c,i: INTEGER;
      tnums: ARRAY [0..7] OF INTEGER;
         ok: BOOLEAN;
  BEGIN
    FOR c:=0 TO HIGH(tnums) DO tnums[c]:=-1 END;
    c:=0; i:=s;
    REPEAT
      str.iscan(tnums[c],args,i,ok); INC(c)
    UNTIL (c>HIGH(tnums)) OR NOT ok;
    exe.task_controll(op,tnums)
  END task_controll;

BEGIN
  s:=0; str.skip(args,s," ");
  IF    cmd="ps"  THEN ps
  ELSIF cmd="cd"  THEN cd
  ELSIF cmd="his" THEN his
  ELSIF cmd="su"  THEN su(TRUE)
  ELSIF cmd="us"  THEN su(FALSE)
  ELSIF cmd="mem" THEN mem
  ELSIF cmd="set" THEN set
  ELSIF cmd="delay" THEN delay
  ELSIF cmd="mount" THEN mount
  ELSIF cmd="unmount" THEN unmount
  ELSIF cmd="stop"  THEN task_controll(exe._stop)
  ELSIF cmd="kill"  THEN task_controll(exe._kill)
  ELSIF cmd="wait"  THEN task_controll(exe._wait)
  ELSIF cmd="version" THEN sta.get(sta.os_vers,s);
    print('Excelsior iV  v%d.%03d  (c) 1990 KRONOS',s DIV 1000,s MOD 1000)
  ELSE RETURN FALSE
  END;
  RETURN TRUE
END shell;

CONST delta = 16;

TYPE CSET = ARRAY [0..7] OF BITSET;

PROCEDURE new_cset(VAR c: CSET);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(c) DO c[i]:={} END
END new_cset;

PROCEDURE add_cset(VAR c: CSET; ch: CHAR);
BEGIN
  INCL( c[ORD(ch) DIV 32], (ORD(ch) MOD 32) )
END add_cset;

PROCEDURE in_cset?(VAL c: CSET; ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ORD(ch) MOD 32) IN c[ORD(ch) DIV 32]
END in_cset?;

PROCEDURE substitute(VAR dest: STRING;
                     VAL  cmd: ARRAY OF CHAR;
                  VAR pos,len: INTEGER;
                     VAR  sep: CHAR;
                         seps: CSET;
                     VAR fail: BOOLEAN);


  VAR ch: CHAR;

  PROCEDURE put(ch: CHAR);
  BEGIN
    IF len>HIGH(dest) THEN
      RESIZE(dest,BYTES(dest)+delta);
      fail:=mem_error()
    END;
    IF fail THEN RETURN END;
    dest[len]:=ch; INC(len)
  END put;

  PROCEDURE change_env_name;
    VAR i,j: INTEGER;
        yes: BOOLEAN;
        n,b: STRING;
      ns,bs: STRING;
  BEGIN
    NEW(ns); NEW(bs); j:=0;
    env.env_entry(j,-1,n,b,yes);
    WHILE env.done DO
      i:=pos+1; str.scan(cmd,i,n,yes);
      IF yes THEN
        IF (HIGH(ns)<0) OR (ns<n) THEN
(*$<$U+*) ns^:=n^; bs^:=b^ (*$>*)
        END
      END;
      j:=j+1; env.env_entry(j,-1,n,b,yes)
    END;
    IF HIGH(ns)<0 THEN put("$"); INC(pos); RETURN END;
    IF HIGH(dest)<len+HIGH(bs) THEN
      RESIZE(dest,len+HIGH(bs)); fail:=mem_error();
    END;
    IF fail THEN RETURN END;
    i:=0;
    WHILE (i<=HIGH(bs)) & (bs[i]#0c) DO
      dest[len]:=bs[i]; INC(i); INC(len)
    END;
    pos:=pos+1+HIGH(ns)
  END change_env_name;

BEGIN
  len:=0; fail:=FALSE; sep:=0c;
  LOOP
    IF fail OR (pos>HIGH(cmd)) THEN EXIT END;
    IF cmd[pos]=" " THEN
      WHILE (pos<=HIGH(cmd)) & (cmd[pos]=" ") DO put(' '); INC(pos) END;
      IF pos>HIGH(cmd) THEN EXIT END;
      IF in_cset?(seps,cmd[pos]) THEN
        IF (pos=HIGH(cmd) ) OR
           (cmd[pos+1]=" ") OR (cmd[pos+1]=0c) THEN
          sep:=cmd[pos]; INC(pos); EXIT
        END
      ELSIF cmd[pos]="\"        THEN
        IF (pos<HIGH(cmd)) & in_cset?(seps,cmd[pos+1]) &
           ( (pos=HIGH(cmd)-1) OR (cmd[pos+2]=" ") OR (cmd[pos+2]=0c) )
        THEN
          INC(pos); put(cmd[pos]); INC(pos);
        END;
      END
    ELSE
      ch:=cmd[pos];
      IF ch=0c THEN EXIT END;
      IF ch="$" THEN
        IF (pos<HIGH(cmd)) & (cmd[pos+1]="$") THEN
          put("$"); INC(pos,2)
        ELSE
          change_env_name; IF fail THEN EXIT END
        END
      ELSE put(ch); INC(pos)
      END;
    END
  END;
  IF NOT fail THEN
    put(0c);
    IF fail THEN RETURN END;
    DEC(len);
    REPEAT dest[len]:=0c; DEC(len) UNTIL (len<0) OR (dest[len]#" "); INC(len);
    IF pos<=HIGH(cmd) THEN str.skip(cmd,pos," ") END
  END;
END substitute;

TYPE cf_ptr = POINTER TO cf_rec;        (* command file interpretation *)
     cf_rec = RECORD
                args: DYNARR OF STRING; (* arguments of command file   *)
                file: bio.FILE;         (* command file                *)
                line: INTEGER;          (* number of last readen line  *)
                next: cf_ptr
              END;

VAR  cfi: cf_ptr; (* command file interpretting   *)
     cfs: STRING; (* i/o buffer for command files *)

PROCEDURE take_word(VAR    d: STRING;
                    VAL    s: ARRAY OF CHAR;
                    VAR  pos: INTEGER;
                    VAR fail: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  NEW(d);
  str.skip(s,pos,' ');
  IF (pos>HIGH(s)) OR (s[pos]=0c) THEN RETURN END;
  i:=pos; str.search(s,i,' ');
  IF i>HIGH(s) THEN i:=HIGH(s)+1 END;
  NEW(d,i-pos+1);
  fail:=mem_error();
  IF NOT fail THEN
    str.sub_str(d,s,pos,i-pos); pos:=i
  END
END take_word;

PROCEDURE dis_cf(VAR F: cf_ptr);
  VAR i: INTEGER;
BEGIN
  WITH F^ DO
    FOR i:=0 TO HIGH(args) DO DISPOSE(args[i]) END;
    DISPOSE(args);
    IF file#bio.null THEN bio.close(file) END
  END;
  DISPOSE(F); F:=NIL
END dis_cf;

PROCEDURE new_cf(VAR    F: cf_ptr;
                 VAR fail: BOOLEAN);
BEGIN
  NEW(F); fail:=mem_error();
  IF fail THEN RETURN END;
  WITH F^ DO
    file:=bio.null;
    line:=-1 ;
    next:=NIL;
    NEW(args)
  END;
END new_cf;

CONST (* masks *)
  cfi_mask = {0..2}; default_mask={0, 2};

PROCEDURE pop_cfi;
  VAR t: cf_ptr;
BEGIN
  ASSERT(cfi#NIL);
  t:=cfi; cfi:=t^.next; dis_cf(t);
  IF cfi=NIL THEN
    exe.break_mask(default_mask)
  END;
END pop_cfi;

PROCEDURE cf_args(       F: cf_ptr;
                  VAL name: ARRAY OF CHAR;
                  VAL args: ARRAY OF CHAR;
                  VAR fail: BOOLEAN);
  VAR i,n: INTEGER; s: STRING;
BEGIN
  n:=0;
  IF name#"" THEN
    NEW(F^.args,1);
    fail:=mem_error(); IF fail THEN RETURN END;
    i:=str.len(name);
    NEW(F^.args[0],i+2);
    fail:=mem_error(); IF fail THEN RETURN END;
    str.copy(F^.args[0],name);
    n:=1;
  END;
  i:=0;
  LOOP
    take_word(s,args,i,fail);
    IF fail OR (HIGH(s)<0) THEN EXIT END;
    IF n>HIGH(F^.args) THEN
      RESIZE(F^.args,n+1);
      fail:=mem_error(); IF fail THEN EXIT END
    END;
(*$<$U+*) F^.args[n]^:=s^; n:=n+1 (*$>*)
  END
END cf_args;

(*
PROCEDURE check_equ(F: cf_ptr): BOOLEAN;
  VAR i: INTEGER; t: cf_ptr;
BEGIN
  t:=cfi;
  WHILE (t#NIL) & NOT bio.equal(t^.file,F^.file) DO t:=t^.next END;
  IF t#NIL THEN
    print('#recursive call of "%s.@": ',F^.args[0]);
    t:=cfi;
    LOOP
      print('"%s"[%d]',t^.args[0],t^.line);
      IF (t=NIL) OR bio.equal(t^.file,F^.file) THEN EXIT END;
      print(' <- '); t:=t^.next
    END;
    print('\n'); RETURN TRUE
  END;
  RETURN FALSE
END check_equ;
*)

PROCEDURE kill_cfi;
  VAR t: cf_ptr;
BEGIN
  WHILE cfi#NIL DO
    t:=cfi; cfi:=t^.next;
    dis_cf(t)
  END;
  exe.break_mask(default_mask)
END kill_cfi;

PROCEDURE ch_access(VAR f: bio.FILE): BOOLEAN;
  VAR save: INTEGER;
      self: os.task_ptr;
       res: BOOLEAN;
BEGIN
  self:=os.self();
  save:=self^.user;
  self^.user:=INTEGER(BITSET(save)+{7});
  bio.chmode(f,'r');
  res:=bio_error('chmode');
  self^.user:=save;
  RETURN res
END ch_access;

PROCEDURE com_file(VAL tname,args: ARRAY OF CHAR; VAR fail: BOOLEAN): BOOLEAN;

  VAR cf: cf_ptr;
    file: bio.FILE;
    name: STRING;
     ext: BOOLEAN;
     res: BOOLEAN;

  PROCEDURE close;
  BEGIN
    IF file#bio.null THEN bio.close(file) END
  END close;

  VAR i,j,l: INTEGER;

BEGIN
  file:=bio.null; fail:=FALSE;
  i:=str.len(tname)-1; j:=i;   (* "take_word" causes len()=HIGH() *)
  NEW(name,i+2); fail:=mem_error(); IF fail THEN RETURN TRUE END;
  str.copy(name,tname);
  REPEAT
    DEC(i)
  UNTIL (i<0) OR (name[i]='.') OR (name[i]='/');
  ext:=FALSE;
  IF (i>=0) & (name[i]#'/') THEN
    IF NOT comp('.@',name,i,j) THEN RETURN FALSE END;
    ext:=TRUE
  ELSE
    RESIZE(name,j+4);
    fail:=mem_error(); IF fail THEN RETURN TRUE END;
    name[j+1]:="."; name[j+2]:="@"; name[j+3]:=00c;
  END;
  bio.lookup(exe.etc,file,name,'x');
  IF    NOT bio.done THEN
    res:=ext;
    IF   (bio.error=err.no_entry) THEN
      IF ext THEN
        fail:=bio_error('lookup')
      END
    ELSIF bio.error=err.sec_vio  THEN
      print('#"%s": non executable\n',name);
      fail:=TRUE
    ELSE fail:=bio_error('lookup')
    END;
    DISPOSE(name); RETURN res
  END;
  fail:=ch_access(file) OR (bio.eof(file)<=0);
  IF fail THEN close; DISPOSE(name); RETURN TRUE END;
  new_cf(cf,fail);
  IF fail THEN close; DISPOSE(name); RETURN TRUE END;
  cf_args(cf,name,args,fail);  DISPOSE(name);
  IF fail THEN close; dis_cf(cf); RETURN TRUE END;
  cf^.file:=file;
(*
  fail:=check_equ(cf);
  IF fail THEN close; dis_cf(cf); RETURN TRUE END;
*)
  cf^.next:=cfi; cfi:=cf;
  exe.break_mask(cfi_mask);
  RETURN TRUE
END com_file;

PROCEDURE get_cfi(VAR cmd: STRING; VAR fail: BOOLEAN);

  VAR d,n,i,p: INTEGER;
            t: cf_ptr;
           ch: CHAR;

  PROCEDURE get_s(): BOOLEAN;
    VAR pos,len: INTEGER;
  BEGIN
    pos:=bio.pos(cfi^.file);
    LOOP
      bio.getstr(cfi^.file,cfs,0); len:=bio.iolen;
      fail:=bio_error('read');
      IF fail THEN RETURN FALSE END;
      IF len<BYTES(cfs) THEN EXIT END;
      RESIZE(cfs,BYTES(cfs)+delta); fail:=mem_error();
      IF fail THEN RETURN FALSE END;
      bio.seek(cfi^.file,pos,0);
      fail:=bio_error('rewind');
      IF fail THEN RETURN FALSE END
    END;
    RETURN TRUE
  END get_s;

  PROCEDURE put(c: CHAR);
  BEGIN
    IF d>HIGH(cmd) THEN
      RESIZE(cmd,d+1+delta);
      fail:=mem_error(); IF fail THEN RETURN END
    END;
    cmd[d]:=c; INC(d)
  END put;

  PROCEDURE puts(VAL s: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN
    IF HIGH(cmd)<d+HIGH(s) THEN
      RESIZE(cmd,d+HIGH(s)+1);
      fail:=mem_error(); IF fail THEN RETURN END
    END;
    i:=0;
    WHILE i<HIGH(s) DO cmd[d]:=s[i]; INC(i); INC(d) END
  END puts;

  PROCEDURE substitute;
    VAR p,n,i: INTEGER;
  BEGIN
    d:=0;
    WITH cfi^ DO
      p:=0;
      WHILE NOT fail & (p<=HIGH(cfs)) & (cfs[p]#0c) DO
        ch:=cfs[p];
        IF (ch='$') & (p<HIGH(cfs)) THEN
          n:=ORD(cfs[p+1])-ORD('0');
          IF n IN {1..9} THEN
            IF n<=HIGH(args) THEN puts(args[n]) END;
            INC(p)
          ELSE put('$')
          END
        ELSIF ch='\' THEN
          IF (p<HIGH(cfs)) &
             ((cfs[p+1]='$') OR (cfs[p+1]='\')) THEN
               INC(p); put(cfs[p])
          ELSE put('\')
          END
        ELSE put(ch)
        END;
        INC(p)
      END;
      put(0c)
    END
  END substitute;

BEGIN
  fail:=FALSE; ASSERT(cfi#NIL);
  IF HIGH(cmd)>=0 THEN cmd[0]:=0c END;
  IF bio.pos(cfi^.file)>=bio.eof(cfi^.file) THEN pop_cfi END;
  IF cfi=NIL THEN RETURN END;
  IF HIGH(cfs)<0 THEN
    NEW(cfs,delta);
    fail:=mem_error(); IF fail THEN RETURN END
  END;
  IF NOT get_s() THEN RETURN END;
  INC(cfi^.line); substitute
END get_cfi;

PROCEDURE user_info(VAR  cmd: ARRAY OF CHAR;
                    VAR exec: BOOLEAN;
                        echo: BITSET);

  PROCEDURE _echo;
  BEGIN
    IF (echo*_cfi#{}) & NOT env.ipr() THEN
      print('%s\n',cmd)
    END
  END _echo;

  PROCEDURE _query();
    VAR ch: CHAR;
  BEGIN
    IF env.ipr() THEN RETURN END;
    print('%s ? ',cmd);
    REPEAT
      read(ch); ch:=sci.CAPITAL(ch)
    UNTIL (ch="Y") OR (ch="N") OR (ch=sci.NAK);
    exec:=(ch="Y");
    IF ch=sci.NAK THEN
      kill_cfi; print('^U\n')
    ELSE
      print('%c\n',ch)
    END
  END _query;

  VAR i: INTEGER;

BEGIN
  i:=0; exec:=TRUE;
  str.skip(cmd,i,' ');
  IF    cmd[i]='?' THEN cmd[i]:=' '; _query; RETURN END;
  IF    cmd[i]='^' THEN cmd[i]:=' '; echo:=echo-_cfi
  ELSIF cmd[i]='%' THEN exec:=FALSE
  END;
  _echo
END user_info;

PROCEDURE env_controll(VAL cmd: ARRAY OF CHAR): BOOLEAN;

  VAR ns, nf: INTEGER;
      bs, bf: INTEGER;
    priv,yes: BOOLEAN;
     tno,i,j: INTEGER;
      s1, s2: STRING;
           T: tsk.TASK;
         del: CHAR;
       fault: BOOLEAN;

  PROCEDURE new(): BOOLEAN;
  BEGIN
    NEW(s1); NEW(s2); RETURN mem_error()
  END new;

  PROCEDURE dispose;
  BEGIN
    IF HIGH(s1)>=0 THEN DISPOSE(s1) END;
    IF HIGH(s2)>=0 THEN DISPOSE(s2) END
  END dispose;

BEGIN
  IF new() THEN RETURN TRUE END;
  ns:=0; fault:=FALSE;
  str.skip(cmd,ns," ");
  IF (ns>HIGH(cmd)) OR (cmd[ns]=0c) THEN RETURN TRUE
  ELSIF (cmd[ns]="{")               THEN RETURN FALSE
  END;
  IF equation(cmd,ns,nf,bs,bf,del,fault) THEN
    IF fault THEN RETURN TRUE END;
    priv:=(ns<nf) & (cmd[ns]=".");
    IF priv OR
       ((cmd[ns]="\") & (ns<nf) & (cmd[ns+1]=".")) THEN INC(ns)
    END;
    yes:=FALSE; i:=ns;
    str.iscan(tno,cmd,i,yes);
    IF yes & (i<nf) & (cmd[i]=".") THEN ns:=i+1
    ELSE tno:=-1
    END;
    NEW(s1,nf-ns+1); IF mem_error() THEN RETURN TRUE END;
    NEW(s2,bf-bs+1); IF mem_error() THEN dispose; RETURN TRUE END;
    str.sub_arr(s1,cmd,ns,nf-ns);
    str.sub_arr(s2,cmd,bs,bf-bs);
    exe.change_env(tno,s1,s2,priv); dispose;
    IF exe_error(_exe_env) THEN END;
    RETURN TRUE
  END;
  RETURN FALSE
END env_controll;

PROCEDURE scan_pref(VAL   cmd: ARRAY OF CHAR;
                    VAR   pos: INTEGER;
                    VAR chenv: exe.ENV;
                    VAR alias: STRING;
                    VAR  fail: BOOLEAN);

  VAR fin: INTEGER;

  PROCEDURE scan_chenv(): INTEGER;
    VAR np,nl,vp,vl,p: INTEGER;
                  del: CHAR;
                priv: BOOLEAN;
  BEGIN
    p:=pos; np:=p;
    WHILE (np<fin) & equation(cmd,np,nl,vp,vl,del,fail) DO
      IF fail THEN RETURN 0 END;
      p:=vl; nl:=nl-np; vl:=vl-vp;
      priv:=cmd[np]=".";
      IF priv OR (cmd[np]="\") THEN INC(np) END;
      exe.fappend(chenv,cmd,np,nl,vp,vl,priv);
      fail:=exe_error(_exe_env);
      IF fail THEN RETURN  0  END;
      IF del#NULL THEN p:=p+1 END;
      IF p>fin    THEN p:=fin END;
      np:=p
    END;
    RETURN p
  END scan_chenv;

  VAR i,end: INTEGER;
        ali: STRING;

BEGIN
  ASSERT(cmd[0]="{"); fail:=FALSE;
  pos:=001; str.skip(cmd,pos," ");
  fin:=pos; str.search(cmd,fin,"}");
  IF (fin>HIGH(cmd)) OR (cmd[fin]#"}") THEN
    print('bracket "}" missed\n'); fail:=TRUE; RETURN
  END;
  end:=fin+1;
  REPEAT fin:=fin-1 UNTIL cmd[fin]#" "; fin:=fin+1;
  IF pos<fin THEN
    pos:=scan_chenv();
    IF fail THEN RETURN END;
    IF pos<fin THEN
      NEW(alias,fin-pos+1); fail:=mem_error();
      IF fail THEN RETURN END;
      str.sub_str(alias,cmd,pos,i)
    END
  END;
  skip(cmd,end); pos:=end
END scan_pref;

PROCEDURE scan_name(VAL  cmd: ARRAY OF CHAR;
                    VAR  pos: INTEGER;
                    VAR name: STRING;
                    VAR fail: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  i:=skip_name(cmd,pos);
  IF i<=pos THEN
    fail:=TRUE; print("empty name\n"); RETURN
  END;
  IF HIGH(name)<i-pos+1 THEN
    RESIZE(name,i-pos+1); fail:=mem_error();
    IF fail THEN RETURN END
  END;
  str.sub_arr(name,cmd,pos,i-pos);
  pos:=i
END scan_name;

TYPE cmd_ptr = POINTER TO cmd_rec;
     cmd_rec = RECORD
                 cmd: STRING;
                 pos: INTEGER;
                 len: INTEGER;
                 chenv: exe.ENV;
                 alias: STRING;
                 next: cmd_ptr
               END;

VAR cmd_stk: cmd_ptr;

PROCEDURE cmd_push(VAL cmd: ARRAY OF CHAR; VAR fail: BOOLEAN);
  VAR t: cmd_ptr;
    len: INTEGER;
BEGIN
  len:=str.len(cmd);
  NEW(t);
  fail:=mem_error(); IF fail THEN RETURN END;
  NEW(t^.cmd,len+1);
  fail:=mem_error(); IF fail THEN RETURN END;
  str.copy(t^.cmd,cmd);
  t^.len:=len; t^.pos:=0;
  exe.new(t^.chenv);
  NEW(t^.alias); t^.next:=cmd_stk; cmd_stk:=t
END cmd_push;

PROCEDURE cmd_pop;
  VAR t: cmd_ptr;
BEGIN
  ASSERT(cmd_stk#NIL);
  t:=cmd_stk; cmd_stk:=cmd_stk^.next;
  WITH t^ DO
    DISPOSE(cmd); DISPOSE(alias);
    exe.dispose(chenv)
  END;
  DISPOSE(t)
END cmd_pop;

VAR seps: CSET;

PROCEDURE pars_string(echo: BITSET);

  VAR fail,exec: BOOLEAN;

  PROCEDURE clear;
  BEGIN
    WHILE cmd_stk#NIL DO cmd_pop END
  END clear;

  VAR pos,len,i: INTEGER;
            sep: CHAR;
          tname: STRING;

  PROCEDURE execute(VAR cmd: ARRAY OF CHAR;
                        len: INTEGER; echo: BITSET);
    VAR yes,chain_wait: BOOLEAN;
  BEGIN
    ASSERT(cmd_stk#NIL);
    IF env_controll(cmd) THEN
    ELSE
      pos:=0; skip(cmd,pos); IF pos>=len THEN RETURN END;
      str.delete(cmd,0,pos); pos:=0;
      IF cmd[pos]="{" THEN
        DISPOSE(cmd_stk^.alias); NEW(cmd_stk^.alias);
        exe.dispose(cmd_stk^.chenv); exe.new(cmd_stk^.chenv);
        scan_pref(cmd,pos,cmd_stk^.chenv,cmd_stk^.alias,fail);
        IF fail THEN RETURN END
      END;
      IF pos>=len THEN
        print("empty name\n"); fail:=TRUE; RETURN
      END;
      scan_name(cmd,pos,tname,fail);
      IF fail THEN RETURN END;
      str.delete(cmd,0,pos);
      IF    (tname[0]#"\") & shell(tname,cmd) THEN
      ELSE
        IF (tname[0]="\") THEN str.delete(tname,0,1) END;
        IF com_file(tname,cmd,yes) THEN
          IF NOT yes THEN
            get_cfi(cfi_buff,fail);
            IF NOT fail THEN
              echo:=_get_echo();
              user_info(cfi_buff,exec,echo);
              IF exec THEN
                cmd_push(cfi_buff,fail)
              ELSE
                cmd_push("",fail)
              END
            END
          END
        ELSE
          chain_wait:=sep#'&';
          result:=run_task(tname,cmd,cmd_stk^.alias,
                           cmd_stk^.chenv,chain_wait,yes,echo);
          IF (result=err.ok) & chain_wait THEN
            cmd_push(exe.chain,fail)
          ELSIF yes THEN
            kill_cfi; cmd_pop
          END
        END
      END
    END
  END execute;

  PROCEDURE dispose;
  BEGIN
    clear; DISPOSE(tname)
  END dispose;

BEGIN
  (* ASSERT(cmd_stk#NIL); *)
  NEW(tname); fail:=FALSE;
  WHILE NOT fail & (cmd_stk#NIL) DO
    IF cmd_stk^.pos<cmd_stk^.len THEN
      substitute(cmd_buff,cmd_stk^.cmd,cmd_stk^.pos,len,sep,seps,fail);
      IF NOT fail & (len>0) THEN
        execute(cmd_buff,len,echo)
      END
    ELSE
      cmd_pop;
      IF cfi#NIL THEN
        IF bio.pos(cfi^.file)<bio.eof(cfi^.file) THEN
          get_cfi(cfi_buff,fail);
          IF NOT fail THEN
            echo:=_get_echo();
            user_info(cfi_buff,exec,echo);
            IF exec THEN
              cmd_push(cfi_buff,fail)
            ELSE
              cmd_push("",fail)
            END
          ELSE pop_cfi
          END
        ELSE pop_cfi
        END
      END
    END
  END;
  dispose
END pars_string;

PROCEDURE system(VAL cmd, echo: ARRAY OF CHAR);
  VAR fail: BOOLEAN; e: BITSET;
BEGIN
  e:=pack_echo(echo);
  cmd_push(cmd,fail);
  IF fail THEN RETURN END;
  pars_string(e)
END system;

PROCEDURE submit(VAL cmd: ARRAY OF CHAR; silent: BOOLEAN);
  VAR cf: cf_ptr;
    file: bio.FILE;
    fail: BOOLEAN;
    echo: BITSET;
BEGIN
  new_cf(cf,fail); IF fail THEN RETURN END;
  cf_args(cf,"",cmd,fail);
  IF fail THEN dis_cf(cf); RETURN END;
  IF HIGH(cf^.args)<0 THEN dis_cf(cf); RETURN END;
  bio.lookup(exe.etc,file,cf^.args[0],'x');
  IF NOT bio.done THEN
    IF    bio.error=err.no_entry THEN
      IF NOT silent THEN
        perror(bio.error,'#"%s": %%s\n',cf^.args[0])
      END;
    ELSIF bio.error=err.sec_vio THEN
      IF NOT silent THEN
        print('#"%s": non executable\n',cf^.args[0])
      END
    ELSIF bio_error('open') THEN
    END;
    dis_cf(cf); RETURN
  END;
  IF ch_access(file) THEN bio.purge(file); dis_cf(cf); RETURN END;
  cf^.file:=file;
(*
  IF check_equ(cf)   THEN bio.purge(file); dis_cf(cf); RETURN END;
*)
  cf^.next:=cfi; cfi:=cf;
  cmd_push("",fail);
  IF fail THEN RETURN END;
  echo:=_get_echo();
  pars_string(echo)
END submit;

PROCEDURE hold_break(on: BOOLEAN);
BEGIN exe.hold_break(on) END hold_break;

PROCEDURE get_prompt(VAR pmt: ARRAY OF CHAR; VAR ins,bell: BOOLEAN);

  VAR p: INTEGER;

  PROCEDURE app_str(VAL new: ARRAY OF CHAR; len: INTEGER);
    VAR i: INTEGER;
  BEGIN
    i:=0;
    IF p+len>HIGH(pmt) THEN len:=HIGH(pmt)-p END;
    WHILE i<len DO
      pmt[p]:=new[i]; INC(i); INC(p)
    END
  END app_str;

  PROCEDURE scan_time(all: BOOLEAN);
    VAR t: ARRAY [0..9] OF CHAR;
        h,m,s,i,len: INTEGER;
  BEGIN
    i:=clk.time();
    clk.unpack((i),i,i,i,h,m,s);
    IF all THEN str.print(t,"%02d:%02d.%02d",h,m,s); len:=8
    ELSE        str.print(t,"%02d:%02d",h,m);        len:=5
    END;
    app_str(t,len)
  END scan_time;

  PROCEDURE scan_user;
    VAR s: STRING;
  BEGIN
    env.get_str("USER",s);
    IF env.done THEN app_str(s,str.len(s)) END
  END scan_user;

  PROCEDURE scan_dir(all: BOOLEAN);
    VAR s: STRING;
        e: ARRAY [0..31] OF CHAR;
      i,j: INTEGER;
  BEGIN
    env.get_str(env.cd,s);
    IF env.done THEN
      IF all THEN app_str(s,str.len(s)); RETURN END;
      IF s="/"0c THEN e:="/"; j:=0; i:=1
      ELSE
        i:=0; j:=0;
        WHILE (i<=HIGH(s)) & (s[i]#0c) DO
          IF s[i]="/" THEN j:=i END;
          i:=i+1
        END;
        IF s[j]="/"    THEN j:=j+1       END;
        IF i-j>HIGH(e) THEN j:=i-HIGH(e) END;
        str.sub_str(e,s,j,i-j)
      END;
      app_str(e,i-j)
    END;
  END scan_dir;

  VAR i: INTEGER; s: STRING;

BEGIN
  pmt:="";
  env.get_str("PROMPT",s);
  IF env.done THEN
    i:=0; p:=0;
    WHILE (p<HIGH(pmt)) & (i<=HIGH(s)) & (s[i]#0c) DO
      IF (s[i]="%") & (i<HIGH(s)) THEN
        INC(i);
        IF    s[i]="t" THEN INC(i); scan_time(FALSE)
        ELSIF s[i]="T" THEN INC(i); scan_time(TRUE)
        ELSIF s[i]="/" THEN INC(i); scan_dir(FALSE)
        ELSIF s[i]="p" THEN INC(i); scan_dir(TRUE)
        ELSIF s[i]="?" THEN INC(i); scan_user
        ELSE
          pmt[p]:="%"; INC(p);
          IF s[i]="%" THEN INC(i) END
        END
      ELSE
        pmt[p]:=s[i]; INC(i); INC(p)
      END
    END;
    IF p<=HIGH(pmt) THEN pmt[p]:=0c END;
  END;

  env.get_str("INSERT",s);
  ins:=NOT env.done OR (s#"0"0c);

  env.get_str("BELL",s);
  bell:=NOT env.done OR (s#"0"0c)

END get_prompt;

PROCEDURE get_echo(VAR echo: ARRAY OF CHAR);
  VAR s: STRING;
BEGIN
  echo:="";
  env.get_str(env.echo,s);
  IF NOT env.done THEN RETURN END;
  str.copy(echo,s)
END get_echo;

----------------------------- INIT -----------------------------
                             ------
BEGIN
  result:=0;
  NEW(cfs,delta); NEW(cmd_buff); NEW(cfi_buff);
  cfi:=NIL; cmd_stk:=NIL;
  print:=Terminal.print;
  read:=Keyboard.read;
  new_cset(seps);
  add_cset(seps,"&");
  add_cset(seps,";");
--  add_cset(seps,"|");
END Shell.
