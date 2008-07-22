IMPLEMENTATION MODULE Execute; (* Hady. 13-May-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  bio: BIO,         env: tskEnv,
        syn: Signals,     tsk: Tasks,
        err: defErrors,   str: Strings,
        key: Keyboard,    lex: Lexicon,
        tty: Terminal;

IMPORT  Heap;

TYPE str8 = ARRAY [0..7] OF CHAR;

---------------------------- STORAGE ---------------------------
                            ---------

WITH STORAGE (NEW: Heap.allocate;
          DISPOSE: Heap.deallocate;
           RESIZE: Heap.reallocate);

PROCEDURE check_mem;
BEGIN
  done:=Heap.done;
  IF NOT done THEN error:=Heap.error END
END check_mem;

------------------------- ERRORS CHECK -------------------------
                         --------------

PROCEDURE check_tsk(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  done:=tsk.done;
  IF done THEN RETURN END;
  error:=tsk.error; str.print(note,fmt,arg)
END check_tsk;

PROCEDURE check_bio(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  done:=bio.done;
  IF done THEN RETURN END;
  error:=bio.error; str.print(note,fmt,arg)
END check_bio;

PROCEDURE chk_bio;
BEGIN
  done:=bio.done;
  IF done THEN RETURN END;
  error:=bio.error; str.print(note,'"%s"',bio.ename)
END chk_bio;

---------------------- BREAK HOLD KITCHEN ----------------------
                      --------------------

VAR sbreak, sawake: syn.SIGNAL;
    old_break: key.BREAK;

PROCEDURE kbnull(no: INTEGER); BEGIN END kbnull;

PROCEDURE on_break(no: INTEGER);
BEGIN
  IF no IN mask THEN
    CASE no OF
      |0: syn.broadcast(sbreak)
      |1: syn.broadcast(sbreak); cause:=cause+cfi
      |2: syn.broadcast(sawake)
    ELSE
    END
  END;
--  old_break(no)
END on_break;

PROCEDURE change_brk;
BEGIN
  IF old_break#kbnull THEN
    key.user_break(on_break)
  END
END change_brk;

PROCEDURE restore_brk;
BEGIN
  IF old_break#kbnull THEN
    key.user_break(old_break); old_break:=kbnull
  END
END restore_brk;

PROCEDURE hold_break(on: BOOLEAN);
BEGIN
  done:=TRUE; note:="";
  IF on THEN
    IF old_break=kbnull THEN
      key.nop; old_break:=key.state^.ubrk
    END;
    key.user_break(on_break)
  ELSE restore_brk
  END
END hold_break;

PROCEDURE break_mask(m: BITSET);
BEGIN
  done:=TRUE; note:=""; mask:=m
END break_mask;

----------------------------- TASKS ----------------------------
                             -------

VAR buffer: STRING;

PROCEDURE wait(task: tsk.TASK; VAR info,t: INTEGER;
                         cemetery, action: BOOLEAN);

  PROCEDURE result;
  BEGIN
    tsk.history(task,t,buffer);
    tsk.put_str(tsk.self,env.history,buffer,FALSE);
    IF NOT tsk.done THEN
      str.print(note,'"%s"',env.history);
    END;
    IF t=4Dh THEN tsk.get_attr(task,tsk.a_res,info);
    ELSE info:=0
    END
  END result;

TYPE SS = (_stop,_kill,_ipr,_awake,_break);

  VAR n: INTEGER;
     ss: ARRAY SS OF syn.SIGNAL;
   done: BOOLEAN;
   stop: syn.SIGNAL;

(* NOTE! break and awake are global variables so we    *)
(* must to provide correct values of them at any time! *)
(* Here assumed that := for syn.SIGNALS is none        *)
(* interruptable action, and broadcast in onstkbrk     *)
(* procedure finished before waiting process resumed!  *)

BEGIN
  tsk.get_signal(stop ,task,tsk.stop);  ss[_stop]:=stop;
  tsk.get_signal(ss[_kill],task,tsk.kill);
  tsk.get_signal(ss[_ipr ],task,tsk.ipr);
  t:=0; info:=0;
  ss[_break]:=sbreak;   ss[_awake]:=sawake;
  LOOP
    ss[_break]:=sbreak;   ss[_awake]:=sawake;
    syn.alts(n,-1,ss);
    IF action THEN
      CASE SS(n) OF
        |_stop : result; ss[_stop]:=syn.null;
                 IF cemetery THEN EXIT
                 ELSE tsk.signal(task,tsk.kill)
                 END
        |_kill : EXIT
        |_break: tsk.signal(task,tsk.stop);
        |_awake: tsk.signal(task,tsk.ipr); cause:=cause+awake; EXIT
        |_ipr  : tsk.signal(task,tsk.ipr); cause:=cause+awake; EXIT
      END
    ELSIF SS(n)#_ipr THEN
      IF (SS(n)=_break) OR (SS(n)=_awake) THEN
--        str.print(note,"wait is broken")  ---???
      END;
      EXIT
    END
  END;
  tsk.free_signal(stop     ,task,tsk.stop);
  tsk.free_signal(ss[_kill],task,tsk.kill);
  tsk.free_signal(ss[_ipr ],task,tsk.ipr);
END wait;

PROCEDURE task_controll(op: INTEGER; VAL tasks: ARRAY OF INTEGER);

  PROCEDURE iterate();
    VAR t: tsk.TASK;
        c: INTEGER;
       tt: INTEGER;
       no: INTEGER;
  BEGIN
    FOR c:=0 TO HIGH(tasks) DO
      no:=tasks[c];
      IF no>=0 THEN
        tsk.open(t,tsk.task0,no);
        IF tsk.done THEN
          CASE op OF
            |_kill: tsk.signal(t,tsk.kill)
            |_stop: tsk.signal(t,tsk.stop)
            |_wait: wait(t,tt,tt,FALSE,FALSE)
          END;
          tsk.close(t); check_tsk('[%d]',no);
          IF NOT done THEN RETURN END;
        END
      END
    END
  END iterate;

BEGIN
  done:=TRUE; note:="";
  IF (op>=0) & (op IN {_stop, _kill, _wait}) THEN
    iterate
  ELSE
    done:=FALSE; error:=err.bad_parm
  END
END task_controll;

--------------------- ENVIRONMENT CONTROLL ---------------------
                     ----------------------

PROCEDURE change_dir(task: tsk.TASK; VAL new: ARRAY OF CHAR);
  VAR cd_name: ARRAY [0..255] OF CHAR;
         test: bio.FILE;
BEGIN
  IF task=tsk.self THEN
    bio.chdir(new); chk_bio;
    IF done THEN bio.fname(bio.cd,cd_name) END
  ELSE
    bio.open(test,new,"r"); chk_bio;
    IF done THEN bio.fname(test,cd_name)   END;
    bio.close(test)
  END;
  IF done THEN
    tsk.put_str(task,env.cd,cd_name,FALSE);
    check_tsk('"%s"',env.cd)
  END
END change_dir;

PROCEDURE chpaths_bin(VAL s: ARRAY OF CHAR);
BEGIN
  tsk.chpaths;
  IF tsk.done THEN RETURN END;
  done:=FALSE; error:=tsk.error;
  str.print(note,'"%s"',env.bin)
END chpaths_bin;

PROCEDURE chpaths_etc(VAL s: ARRAY OF CHAR);
  VAR new: bio.PATHs;
BEGIN
  bio.open_paths(new,s); chk_bio;
  IF done THEN
    IF etc#bio.here THEN
      bio.close_paths(etc); chk_bio
    END;
    etc:=new
  END;
END chpaths_etc;

PROCEDURE ch_sysmsg(VAL name: ARRAY OF CHAR);
  VAR l: lex.LEX;
BEGIN
  lex.open(l,name);
  IF NOT lex.done THEN
    done:=FALSE; error:=lex.error;
    str.print(note,'"%s"',name);
    RETURN
  END;
  lex.change_sysmsg(l)
END ch_sysmsg;

PROCEDURE ch_tty(VAL name: ARRAY OF CHAR);
  VAR s: STRING;
BEGIN
  tsk.get_str((tsk.self),env.tty,s);
  IF NOT tsk.done OR (s="") THEN tty.attach(name) END;
END ch_tty;

PROCEDURE ch_env(T: tsk.TASK; tno: INTEGER;
                 VAL name: ARRAY OF CHAR;
                 VAL body: ARRAY OF CHAR;
                     priv: BOOLEAN);
BEGIN
  IF name=env.cd  THEN change_dir(T,body); RETURN END;

  IF (T=tsk.self) & (name=env.tty) THEN ch_tty(body) END;

  IF body#"" THEN tsk.put_str(T,name,body,priv)
  ELSE            tsk.del_env(T,name)
  END;

  check_tsk('"%s"',name);
  IF NOT done THEN RETURN END;

  IF T=tsk.self THEN
    IF    name=env.bin THEN chpaths_bin(body)
    ELSIF name=env.etc THEN chpaths_etc(body)
    ELSIF name=env.msg THEN ch_sysmsg  (body)
    END
  ELSE
    tsk.close(T); check_tsk('[%d]',tno)
  END
END ch_env;

PROCEDURE change_env( task_no: INTEGER;
                     VAL name: ARRAY OF CHAR;
                     VAL body: ARRAY OF CHAR;
                         priv: BOOLEAN);
  VAR T: tsk.TASK;

BEGIN
  done:=TRUE; note:="";
  IF task_no>=0 THEN
    tsk.open(T,tsk.task0,task_no); check_tsk('[%d]',task_no);
    IF NOT done THEN RETURN END
  ELSE
    T:=tsk.self; task_no:=env.id
  END;
  ch_env(T,task_no,name,body,priv);
  IF T#tsk.self THEN tsk.close(T) END
END change_env;

---------------------- INSTANT ENVIRONMENT ---------------------
                      ---------------------

TYPE ENV = POINTER TO ec_rec;
  ec_rec = RECORD
                tag: INTEGER;
               name: STRING;
              value: STRING;
               priv: BOOLEAN;
               next: ENV
           END;

CONST ec_tag = 454E56h; (* "ENV" *)

PROCEDURE new(VAR ec: ENV);
BEGIN
  done:=TRUE; note:=""; ec:=NIL
END new;

PROCEDURE dis_ec(VAR ec: ENV);
BEGIN
  WITH ec^ DO
    ASSERT(tag=ec_tag,err.bad_parm);
    DISPOSE(name); DISPOSE(value);
    tag:=0
  END;
  DISPOSE(ec); ec:=NIL
END dis_ec;

PROCEDURE _dispose(VAR ec: ENV);
  VAR t: ENV;
BEGIN
  WHILE ec#NIL DO
    ASSERT(ec^.tag=ec_tag);
    t:=ec; ec:=ec^.next;
    dis_ec(t)
  END;
END _dispose;

PROCEDURE dispose(VAR ec: ENV);
  VAR t: ENV;
BEGIN
  done:=TRUE; note:="";
  WHILE ec#NIL DO
    IF ec^.tag#ec_tag THEN
      done:=FALSE; error:=err.bad_parm; RETURN
    END;
    t:=ec; ec:=ec^.next;
    dis_ec(t)
  END;
END dispose;

PROCEDURE new_ec(VAR  ec: ENV;
                 VAL n,v: INTEGER);
BEGIN
  NEW(ec); check_mem;
  IF NOT done THEN ec:=NIL; RETURN END;
  WITH ec^ DO
    tag:=ec_tag;
    NEW(name,n+1); check_mem;
    IF NOT done THEN
      tag:=0; DISPOSE(ec); ec:=NIL; RETURN
    END;
    NEW(value,v+1); check_mem;
    IF NOT done THEN
      tag:=0; DISPOSE(name); DISPOSE(ec); ec:=NIL
    ELSE
      next:=NIL
    END;
  END
END new_ec;

PROCEDURE append(VAR ec: ENV; VAL n,v: ARRAY OF CHAR; priv: BOOLEAN);
  VAR t: ENV;
BEGIN
  done:=TRUE; note:="";
  new_ec(t,str.len(n),str.len(v));
  IF NOT done THEN RETURN END;
  str.copy(t^.name,n); str.copy(t^.value,v);
  t^.priv:=priv; t^.next:=ec; ec:=t
END append;

PROCEDURE fappend(VAR ec: ENV;
                  VAL  s: ARRAY OF CHAR;
                   np,nl: INTEGER;
                   vp,vl: INTEGER;
                    priv: BOOLEAN);
  VAR t: ENV;
BEGIN
  done:=TRUE; note:="";
  IF (np+nl>HIGH(s)+1) THEN nl:=HIGH(s)-np+1 END;
  IF (vp+vl>HIGH(s)+1) THEN vl:=HIGH(s)-vp+1 END;
  new_ec(t,nl,vl);
  IF NOT done THEN RETURN END;
  str.sub_arr(t^.name,s,np,nl); str.sub_arr(t^.value,s,vp,vl);
  t^.priv:=priv; t^.next:=ec; ec:=t
END fappend;

PROCEDURE check_ec(ec: ENV): BOOLEAN;
BEGIN
  WHILE ec#NIL DO
    IF ec^.tag#ec_tag THEN
      done:=FALSE; error:=err.bad_parm;
      RETURN TRUE
    END;
    ec:=ec^.next
  END;
  RETURN FALSE
END check_ec;

--------------------------- RUN TASKS --------------------------
                           -----------

PROCEDURE create(VAR ntask: tsk.TASK;
                      name: ARRAY OF CHAR;
                     chenv: ENV;
                 VAL alias: ARRAY OF CHAR;
                 VAL parms: ARRAY OF CHAR);

  VAR remote: BOOLEAN;
      cdname: STRING;
      _parms: STRING;
      _alias: STRING;

  PROCEDURE return;
  BEGIN
    IF remote THEN
      ASSERT(HIGH(cdname)>=0);
      bio.chdir(cdname); DISPOSE(cdname);
      remote:=FALSE
    END
  END return;

  PROCEDURE choose(VAR   cmd: ARRAY OF CHAR;
                   VAR tname: ARRAY OF CHAR);

    VAR ext: ARRAY [0..31] OF CHAR;
       name: ARRAY [0..31] OF CHAR;
        cdn: STRING;

    PROCEDURE remote_dir(VAR path,name: ARRAY OF CHAR);
      VAR i: INTEGER;
    BEGIN
      i:=0;
      WHILE (i<HIGH(path)) &
             (path[i]#00c) &
             (path[i]#'/') DO INC(i)
      END;
      IF (i>HIGH(path)) OR (path[i]#'/') THEN
        str.copy(name,path); path:=""; remote:=FALSE; RETURN
      END;
      bio.splitpathname(path,name); chk_bio;
      IF NOT done THEN RETURN END;
      tsk.get_str(tsk.self,env.cd,cdn);
      check_tsk('"%s"',env.cd);
      IF done THEN i:=LEN(cdn); ELSE NEW(cdn); i:=1 END;
      NEW(cdname,i); check_mem;
      IF NOT done THEN RETURN END;
      IF HIGH(cdn)<0 THEN
           str.copy(cdname,"/")
      ELSE str.copy(cdname,cdn)
      END;
      bio.chdir(path); chk_bio;
      IF NOT done THEN RETURN END;
      remote:=TRUE
    END remote_dir;

    PROCEDURE get_ext(VAR name,ext: ARRAY OF CHAR);
      VAR i,j,e: INTEGER;
    BEGIN
      ext:=""; i:=0; j:=0;
      WHILE (i<HIGH(name)) & (name[i]#0c) DO
      IF name[i]="." THEN j:=i END; i:=i+1
      END;
      IF name[j]#"." THEN RETURN END;
      IF i-j>HIGH(ext) THEN i:=j+HIGH(ext) END;
      e:=0;
      WHILE j<i DO
        ext[e]:=name[j]; e:=e+1; j:=j+1
      END;
      IF e<=HIGH(ext) THEN ext[e]:=0c END;
      name[j-e]:=0c
    END get_ext;

    PROCEDURE profile;
      VAR s: STRING;
    BEGIN
      tsk.get_str(tsk.self,env.shell,s);
      IF NOT tsk.done OR (HIGH(s)>HIGH(tname)) THEN
        done:=FALSE; error:=err.unsuitable;
        str.print(note,'%s="%s"',env.shell,s);
        RETURN
      END;
      str.copy(tname,s);
      IF NOT remote OR (cmd="") THEN str.copy(cmd,name)
      ELSE str.append(cmd,"/%s",name);
      END;
      IF remote THEN return END;
      NEW(_parms,str.len(cmd)+5+str.len(parms)+1); check_mem;
      IF NOT done THEN RETURN END;
      str.print(_parms,"<%s.sh %s",cmd,parms)
    END profile;

    PROCEDURE copy_parms;
    BEGIN
      NEW(_parms,str.len(parms)+1); check_mem;
      IF done THEN
        str.copy(_parms,parms)
      END
    END copy_parms;

    VAR test: bio.FILE;

  BEGIN
    remote:=FALSE; NEW(cdname);
    remote_dir(cmd,name);
    IF NOT done THEN RETURN END;
    get_ext(name,ext);
    IF    ext=".sh"  THEN profile
    ELSIF ext=".cod" THEN str.copy(tname,name); copy_parms
    ELSIF ext=""     THEN
      tsk.find(tsk.task0,name);
      IF tsk.done THEN
        str.copy(tname,name);
        copy_parms; RETURN
      END;
      IF tsk.error#err.no_entry THEN
        check_tsk('"%s.cod"',name); RETURN
      ELSE
        str.print(tname,"%s.sh",name);
        bio.lookup(etc,test,tname,"x");
        IF bio.done THEN
          bio.close(test); profile
        ELSE
          str.print(note,
                    '"%s.cod","%s.@","%s.sh"',
                    name,name,name);
          done:=FALSE; error:=err.no_entry
        END
      END
    ELSE
      error:=err.unsuitable; done:=FALSE;
      str.print(note,'"%s"',ext);
      RETURN
    END
  END choose;

  PROCEDURE sync_envs(T: tsk.TASK;
                   papa: INTEGER);

    VAR  task: INTEGER;
         bump: str8;
          tst: STRING;
          del: CHAR;
          pri: BOOLEAN;

  BEGIN
    tsk.del_env(T,env.chain);
    tsk.get_attr(T,tsk.a_id,task);
    str.print(bump,"%d",task);
    tsk.put_str(T,env.task,bump,TRUE);
    check_tsk('"%s"',env.task); IF NOT done THEN RETURN END;
    tsk.get_str(T,env.base,tst);
    IF NOT tsk.done THEN
      tsk.put_str(T,env.base,bump,TRUE);
      check_tsk('"%s"',env.base); IF NOT done THEN RETURN END
    END;
    str.print(bump,"%d",papa);
    tsk.put_str(T,env.father,bump,TRUE);
    check_tsk('"%s"',env.father)
  END sync_envs;

  VAR direct: ENV;
     instant: ENV;
     restore: ENV;

  PROCEDURE divide;
    VAR c,t: ENV;

    PROCEDURE save(VAL name: ARRAY OF CHAR);
      VAR val: STRING;
         priv: BOOLEAN;
    BEGIN
      tsk.get_env(tsk.self,name,-1,val,priv);
      IF tsk.done THEN
        append(restore,name,val,priv)
      ELSIF tsk.error=err.no_entry THEN
        append(restore,name,"",FALSE)
      ELSE
        check_tsk('"%s"',name)
      END
    END save;

  BEGIN
    c:=chenv;
    WHILE done & (c#NIL) DO
      IF c^.priv THEN
        append(direct,c^.name,c^.value,c^.priv)
      ELSE
        save(c^.name);
        IF done THEN
          append(instant,c^.name,c^.value,c^.priv)
        END
      END;
      c:=c^.next
    END
  END divide;

  PROCEDURE sync_env(T: tsk.TASK; tno: INTEGER; ec: ENV);
  BEGIN
    WHILE done & (ec#NIL) DO
      ch_env(T,tno,ec^.name,ec^.value,ec^.priv);
      ec:=ec^.next
    END
  END sync_env;

  VAR tname: ARRAY [0..31] OF CHAR;

  PROCEDURE load_task();
    VAR stk: INTEGER;
       dady: INTEGER;
       base: tsk.TASK;

    PROCEDURE release;
    BEGIN
      IF base#tsk.null THEN tsk.close(base) END;
    END release;

    PROCEDURE get_num(VAL name: ARRAY OF CHAR; VAR n: INTEGER): BOOLEAN;
      VAR s: STRING; yes: BOOLEAN; i: INTEGER;
    BEGIN
      tsk.get_str((tsk.self),name,s);
      IF tsk.done THEN
        i:=0;
        str.iscan(n,s,i,yes);
        RETURN yes
      END;
      RETURN FALSE
    END get_num;

  BEGIN
    divide;
    IF NOT done THEN release; RETURN END;
    sync_env(tsk.self,env.id,instant);
    IF NOT done THEN release; RETURN END;

    IF NOT get_num(env.base,dady) THEN dady:=env.id END;
    tsk.open(base,tsk.task0,dady); check_tsk('[%d]',dady);
    IF NOT done THEN RETURN END;
    IF NOT get_num(env.stk,stk) THEN stk:=4 END;
    tsk.create(ntask,(base),tname,_alias,stk*256,_parms);
    IF NOT tsk.done THEN
      error:=tsk.error;
      IF tsk.note#"" THEN
        str.print(note,'"%s"(%s)',tname,tsk.note)
      ELSE
        str.print(note,'"%s"',tname)
      END;
      sync_env(tsk.self,env.id,restore); release;
      done:=FALSE;
      RETURN
    END;
    sync_env(tsk.self,env.id,restore);

    tsk.get_attr(ntask,tsk.a_id,task);
    sync_env(ntask,task,direct);
    IF done THEN sync_envs(ntask,dady) END
  END load_task;

  PROCEDURE copy_alias;
    VAR s: STRING;
      len: INTEGER;
  BEGIN
    tsk.get_str(tsk.self,env.alias,s);
    IF tsk.done THEN
      len:=str.len(s)+str.len(alias)+1;
      NEW(_alias,len+1); check_mem;
      IF done THEN
        str.print(_alias,'%s %s',s,alias)
      END
    ELSE
      NEW(_alias,str.len(alias)+1); check_mem;
      IF done THEN
        str.copy(_alias,alias)
      END
    END
  END copy_alias;

  PROCEDURE release;
  BEGIN
    return; -- DISPOSEs cdname
    DISPOSE(_parms); DISPOSE(_alias);
    _dispose(direct);
    _dispose(instant);
    _dispose(restore)
  END release;

BEGIN
  IF name="" THEN
    error:=err.unsuitable; done:=FALSE; RETURN
  END;
  remote:=FALSE; NEW(cdname); NEW(_parms); NEW(_alias);
  direct:=NIL; instant:=NIL; restore:=NIL;
  copy_alias;
  IF NOT done THEN RETURN END;
  choose(name,tname);
  IF done THEN load_task END;
  release
END create;

PROCEDURE run_task(VAL name: ARRAY OF CHAR;
                      chenv: ENV;
                  VAL alias: ARRAY OF CHAR;
                  VAL  args: ARRAY OF CHAR;
                      _wait: BOOLEAN);

   VAR   ntsk: tsk.TASK;
   trap, info: INTEGER;
        cemet: BOOLEAN;
         buff: str8;
            s: STRING;

BEGIN
  done:=TRUE; note:="";
  create(ntsk,name,chenv,alias,args);
  IF NOT done THEN RETURN END;

  tsk.get_str(tsk.self,env.cemetry,s); cemet:=tsk.done & (s="0"0c);

  tsk.get_attr(ntsk,tsk.a_id,task);
  check_tsk('[???]',env.id); IF NOT done THEN RETURN END;
  str.print(buff,"%d",task);
  tsk.put_str(tsk.self,env.son,buff,TRUE);
  check_tsk('[%d]',task); IF NOT done THEN RETURN END;

  tsk.set_attr(ntsk,tsk.a_ipr,NOT _wait OR env.ipr());
  check_tsk('[%d]',task); IF NOT done THEN RETURN END;
  IF _wait THEN change_brk END;

  tsk.run(ntsk);
  check_tsk('[%d]',task); IF NOT done THEN RETURN END;
  cause:={};
  IF NOT _wait THEN cause:=cause+awake; tsk.close(ntsk); RETURN END;

  wait(ntsk,info,trap,cemet,TRUE);
  IF    (trap=00h) OR (trap=47h) THEN
    tsk.get_str(ntsk,env.chain,s);
    IF tsk.done THEN
      IF HIGH(chain)<HIGH(s) THEN
        RESIZE(chain, HIGH(s)+1); check_mem;
        IF NOT done THEN RETURN END
      END;
      str.copy(chain,s);
      cause:=cause+continue
    END
  ELSIF trap>0   THEN
    IF    trap=4Dh THEN
      cause:=cause+break; error:=info
    ELSIF trap=50h THEN
      cause:=cause+break; error:=trap
    ELSE
      cause:=cause+abort; error:=trap
    END
  END;
  tsk.close(ntsk)
END run_task;

PROCEDURE final; BEGIN restore_brk END final;

PROCEDURE make_environment;
  VAR bump: str8;
      done: BOOLEAN;
       i,n: INTEGER;
         s: STRING;
BEGIN
  tsk.caller(n);
  str.print(bump,"%d",n);
  tsk.put_str(tsk.self,env.task,bump,TRUE);
  tsk.get_str((tsk.self),env.base,s);
  IF tsk.done THEN
    i:=0; str.iscan(n,s,i,done)
  ELSE done:=FALSE
  END;
  IF NOT done THEN
    tsk.put_str(tsk.self,env.base,bump,TRUE)
  END;
  tsk.papa((tsk.self),n);
  IF NOT tsk.done THEN n:=0 END;
  str.print(bump,"%d",n);
  tsk.put_str(tsk.self,env.father,bump,TRUE)
END make_environment;

PROCEDURE make_buff;
  CONST max=256; min=80; step=16;
  VAR sz: INTEGER;
BEGIN
  sz:=max+step;
  REPEAT
    sz:=sz-step;
    NEW(buffer,sz); check_mem;
  UNTIL done OR (sz<min);
  IF sz<=min THEN HALT(err.heap_abort) END
END make_buff;

PROCEDURE init;
  VAR done: BOOLEAN;
BEGIN
  make_buff;   make_environment;
  syn.new_signal(sbreak,0,done);
  IF NOT done THEN HALT(err.no_memory) END;
  syn.new_signal(sawake,0,done);
  IF NOT done THEN HALT(err.no_memory) END;

  bio.get_paths(etc,env.etc);
  IF NOT bio.done THEN
    bio.open_paths(etc,".");
    IF NOT bio.done THEN HALT(bio.error) END
  END;

  old_break:=kbnull;
  env.final(final);

  mask :={0,2};
  cause:={};
  done :=TRUE;
  error:=0;
  note :="";
  task :=-1;
  NEW(chain,1); check_mem; IF NOT done THEN HALT(error) END;
  chain[0]:=0c
END init;

BEGIN
  done:=TRUE; note:=""; init
END Execute.
