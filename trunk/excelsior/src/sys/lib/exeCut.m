IMPLEMENTATION MODULE exeCut; (* Hady. 16-Aug-90. (c) KRONOS *)

IMPORT  str: Strings;
IMPORT  lex: Lexicon;
IMPORT  env: tskEnv;
IMPORT  sys: SYSTEM;
IMPORT  tsk: Tasks;
IMPORT  key: Keyboard;
IMPORT  err: defErrors;
IMPORT  usr: Users;
IMPORT  syn: Signals;
IMPORT  bio: BIO;

IMPORT  Heap;

TYPE str80= ARRAY [0..79] OF CHAR;
     str8 = ARRAY [0.. 7] OF CHAR;

---------------------------- STORAGE ---------------------------
                            ---------
PROCEDURE check_mem;
BEGIN
  IF NOT Heap.done THEN done:=FALSE; error:=Heap.error END
END check_mem;

PROCEDURE alloc(VAR a: sys.ADDRESS; sz: INTEGER);
BEGIN Heap.allocate(a,sz); check_mem   END alloc;

PROCEDURE dealloc(VAR a: sys.ADDRESS; sz: INTEGER);
BEGIN Heap.deallocate(a,sz); check_mem END dealloc;

PROCEDURE realloc(VAR a: sys.ADDRESS; VAR high: INTEGER;
                                 len,el_byte_size: INTEGER);
BEGIN
  Heap.reallocate(a,high,len,el_byte_size); check_mem
END realloc;

WITH STORAGE (NEW    :   alloc;
              DISPOSE: dealloc;
              RESIZE : realloc);


---------------------------- STRINGS ---------------------------
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

--------------------------- INTERFACE --------------------------
                           -----------

PROCEDURE check_tsk(VAL fmt: ARRAY OF CHAR;
                    SEQ arg: sys.WORD): BOOLEAN;
BEGIN
  IF tsk.done THEN RETURN FALSE END;
  done:=FALSE; error:=tsk.error;
  str.print(note,fmt,arg);
  IF tsk.note#"" THEN str.append(note,".\n#%s",tsk.note) END;
  RETURN TRUE
END check_tsk;

PROCEDURE check_bio(VAL fmt: ARRAY OF CHAR; SEQ arg: sys.WORD): BOOLEAN;
BEGIN
  IF bio.done THEN RETURN FALSE END;
  done:=FALSE; error:=bio.error;
  str.print(note,fmt,arg);
  RETURN TRUE
END check_bio;

CONST (* syntax errors *)
       ok=0;
  bramiss=1; -- brackets "}" missed
  notpare=2; -- not pared delimiter
  illgext=3; -- illegal extension
  illbase=4; -- base number absent
  longnam=5; -- too long name
  emptynm=6; -- empty name
  noshell=7; -- absent or illegal "SHELL"

PROCEDURE vis_err(er: INTEGER);
BEGIN
  done:=FALSE; error:=err.inv_op;
  CASE er OF
    |bramiss: note:='bracket "}" missed'
    |notpare: note:='not pared delimiter'
    |illgext: note:='illegal extension'
    |illbase: note:='base number absent or illegal'
    |longnam: note:='too long name'
    |emptynm: note:='empty task name'
    |noshell: note:='absent or illegal "SHELL" string'
  ELSE note:=''
  END
END vis_err;

PROCEDURE no_memory;
BEGIN
  done:=FALSE; error:=err.no_memory
END no_memory;

---------------------- BREAK HOLD KITCHEN ----------------------
                      --------------------

VAR sbreak, sawake: syn.SIGNAL;
    old_break: key.BREAK;

PROCEDURE null(no: INTEGER); BEGIN END null;

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
  IF old_break#null THEN
    key.user_break(on_break)
  END
END change_brk;

PROCEDURE restore_brk;
BEGIN
  IF old_break#null THEN
    key.user_break(old_break); old_break:=null
  END
END restore_brk;

PROCEDURE hold_break(on: BOOLEAN);
BEGIN
  IF on THEN
    IF old_break=null THEN
      key.nop; old_break:=key.state^.ubrk
    END;
    key.user_break(on_break)
  ELSE restore_brk
  END
END hold_break;

PROCEDURE break_mask(m: BITSET);
BEGIN mask:=m END break_mask;

----------------------------------------------------------------

CONST NULL=CHAR(-1);

PROCEDURE equation(VAL cmd: ARRAY OF CHAR;
                 VAR ns,nf: INTEGER;  -- name start & finish
                 VAR bs,bf: INTEGER;  -- body start & finish
                 VAR   del: CHAR      -- delimiter
                   ): BOOLEAN;

BEGIN
  IF ns<0 THEN ns:=0 END; skip(cmd,ns);
  IF (ns>=HIGH(cmd)) OR (cmd[ns]=0c) THEN RETURN FALSE END;

  nf:=ns;
  WHILE (nf<=HIGH(cmd)) &
        (cmd[nf]#0c ) & (cmd[nf]#"{") &
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
      vis_err(notpare); RETURN TRUE
    END;
  ELSE
    bf:=skip_name(cmd,bf); del:=NULL
  END;
  RETURN TRUE
END equation;

PROCEDURE change_dir(task: tsk.TASK; VAL new: ARRAY OF CHAR);
  VAR cd_name: ARRAY [0..255] OF CHAR;
         test: bio.FILE;
     self,res: BOOLEAN;
BEGIN
  self:=(task=tsk.self);
  IF self THEN bio.chdir(new);
    res:=check_bio('change_dir("%s")',bio.ename);
    IF NOT res THEN bio.fname(bio.cd,cd_name) END
  ELSE
    bio.open(test,new,"r");
    res:=check_bio('open("%s")',bio.ename);
    IF NOT res THEN bio.fname(test,cd_name)   END;
    bio.purge(test)
  END;
  IF NOT res THEN
    tsk.put_str(task,env.cd,cd_name,FALSE);
    res:=check_tsk("put CD name to environment")
  END
END change_dir;

PROCEDURE chpaths_bin(VAL s: ARRAY OF CHAR);
BEGIN
  tsk.chpaths;
  IF check_tsk("can't"' update "%s" in TASKS',s) THEN END
END chpaths_bin;

PROCEDURE chpaths_etc(VAL s: ARRAY OF CHAR);
  VAR new: bio.PATHs;
BEGIN
  bio.open_paths(new,s);
  IF check_bio("can't change paths: %s",s) THEN RETURN END;
  bio.close_paths(etc);
  IF check_bio("can't close paths ETC") THEN END;
  etc:=new
END chpaths_etc;

PROCEDURE ch_sysmsg(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR l: lex.LEX;
BEGIN
  lex.open(l,name);
  IF NOT lex.done THEN
    done:=FALSE; error:=lex.error;
    str.print(note,'open_msg("%s")',name);
    RETURN TRUE
  END;
  lex.change_sysmsg(l);
  RETURN FALSE
END ch_sysmsg;

PROCEDURE change_env(       T: tsk.TASK;
                     VAL name: ARRAY OF CHAR;
                     VAL body: ARRAY OF CHAR;
                         priv: BOOLEAN);
BEGIN
  IF name=env.cd  THEN change_dir(T,body); RETURN END;

  IF (name=env.msg) & ch_sysmsg(body) THEN RETURN END;

  IF body#"" THEN tsk.put_str(T,name,body,priv)
  ELSE            tsk.del_env(T,name)
  END;
  done:=tsk.done;
  IF NOT done THEN error:=tsk.error;
    str.print(note,"can't"' put in environment "%s"',name);
    RETURN
  END;

  IF T=tsk.self THEN
    IF    name=env.bin THEN chpaths_bin(body)
    ELSIF name=env.etc THEN chpaths_etc(body)
    END
  END
END change_env;

PROCEDURE ch_env(      T: tsk.TASK;
                 VAL cmd: ARRAY OF CHAR;
                   ns,nf: INTEGER;
                   bs,bf: INTEGER;
                    priv: BOOLEAN);

  VAR len: INTEGER;
    s1,s2: STRING;

  PROCEDURE new(): BOOLEAN;
  BEGIN
    NEW(s1); NEW(s2); RETURN NOT done
  END new;

  PROCEDURE dispose;
  BEGIN
    IF HIGH(s1)>=0 THEN DISPOSE(s1) END;
    IF HIGH(s2)>=0 THEN DISPOSE(s2) END
  END dispose;

BEGIN
  IF new() THEN RETURN END;

  len:=nf-ns;
  NEW(s1,len+1);
  IF NOT done THEN dispose; RETURN END;
  str.sub_str(s1,cmd,ns,nf-ns);
  len:=bf-bs;
  NEW(s2,len+1);
  IF NOT done THEN dispose; RETURN END;
  str.sub_str(s2,cmd,bs,bf-bs);
  change_env(T,s1,s2,priv);
  dispose

END ch_env;

PROCEDURE env_controll(VAL cmd: ARRAY OF CHAR): BOOLEAN;

  VAR ns, nf: INTEGER;
      bs, bf: INTEGER;
    priv,yes: BOOLEAN;
     tno,i,j: INTEGER;
           s: STRING;
           T: tsk.TASK;
         del: CHAR;

BEGIN
  ns:=0;
  IF equation(cmd,ns,nf,bs,bf,del) THEN
    IF NOT done THEN RETURN TRUE END;
    priv:=(ns<nf) & (cmd[ns]=".");
    IF priv OR
       ((cmd[ns]="\") & (ns<nf) & (cmd[ns+1]=".")) THEN INC(ns)
    END;
    yes:=FALSE; i:=ns;
    str.search(cmd,i,".");
    IF (i<nf) & (cmd[i]=".") THEN
      del:=cmd[ns];
      IF (del>="0") & (del<="9") THEN
        j:=ns; str.iscan(tno,cmd,j,yes);
        yes:=yes & (j=i)
      ELSIF del="$" THEN
        NEW(s,i-ns); IF NOT done THEN RETURN TRUE END;
        str.sub_str(s,cmd,ns+1,i-ns-1);
        yes:=get_num(s,tno);
        DISPOSE(s)
      ELSE yes:=FALSE
      END;
      IF yes THEN
        tsk.open(T,tsk.task0,tno);
        yes:=tsk.done
      END;
      IF yes THEN ns:=i+1 END
    END;
    IF NOT yes THEN T:=tsk.self END;
    ch_env(T,cmd,ns,nf,bs,bf,priv);
    IF yes THEN
      tsk.close(T); yes:=check_tsk('close(%d)',tno)
    END;
    RETURN TRUE
  END;
  RETURN NOT done
END env_controll;

(*
   task_control ::=
                "stop"  task_number { task_number }
             |  "kill"  task_number { task_number }
             |  "wait"  task_number { task_number } .
*)

VAR buffer: STRING;

PROCEDURE wait(task: tsk.TASK; VAR info,t: INTEGER;
                         cemetery, action: BOOLEAN);

  PROCEDURE result;
    VAR chain: STRING;
  BEGIN
    tsk.history(task,t,buffer);
    tsk.put_str(tsk.self,env.history,buffer,FALSE);
    IF NOT tsk.done THEN
      str.print(note,"can't put history")
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
        str.print(note,"wait is broken")
      END;
      EXIT
    END
  END;
  tsk.free_signal(stop     ,task,tsk.stop);
  tsk.free_signal(ss[_kill],task,tsk.kill);
  tsk.free_signal(ss[_ipr ],task,tsk.ipr);
END wait;

PROCEDURE task_controll(VAL cmd: ARRAY OF CHAR): BOOLEAN;

  VAR nf,nt,no,i: INTEGER; ok: BOOLEAN;

  CONST _kill=0; _stop=1; _wait=2;

  PROCEDURE iterate(act: INTEGER);
    VAR t: tsk.TASK;
     j,tt: INTEGER;
  BEGIN
    j:=i;
    str.iscan(no,cmd,j,ok);
    IF NOT ok THEN RETURN END;
    REPEAT
      i:=j;
      tsk.open(t,tsk.task0,no);
      IF tsk.done THEN
        CASE act OF
          |_kill: tsk.signal(t,tsk.kill)
          |_stop: tsk.signal(t,tsk.stop)
          |_wait: wait(t,tt,tt,FALSE,FALSE)
        END;
        tsk.close(t);
        IF check_tsk("task_close(%d)",no) THEN RETURN END;
      END;
      str.iscan(no,cmd,j,ok)
    UNTIL NOT ok
  END iterate;

BEGIN
  nf:=0; skip(cmd,nf);
  IF (nf>=HIGH(cmd)) OR (cmd[nf]=0c) THEN RETURN FALSE END;
  nt:=skip_name(cmd,nf);
  i:=nt; nt:=nt-1;
  IF    comp("stop",cmd,nf,nt) THEN iterate(_stop)
  ELSIF comp("kill",cmd,nf,nt) THEN iterate(_kill)
  ELSIF comp("wait",cmd,nf,nt) THEN iterate(_wait)
  ELSE
    RETURN FALSE
  END;
  RETURN TRUE
END task_controll;

PROCEDURE dev_controll(VAR cmd: ARRAY OF CHAR): BOOLEAN;
  VAR ns,nf: INTEGER;

  PROCEDURE get_word(VAR word: ARRAY OF CHAR);
  BEGIN
    ns:=nf; IF ns<0 THEN ns:=0 END;
    skip(cmd,ns); nf:=skip_name(cmd,ns);
    str.sub_str(word,cmd,ns,nf-ns)
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
      str.print(note,'"%s" [%s] mounted at "%s"\n',dev,lab,home);
    ELSE
      done:=FALSE; error:=bio.error;
      str.print(note,"can't"' mount "%s" at "%s"',home,dev)
    END
  END mount;

  PROCEDURE unmount;
    VAR home: str80;
  BEGIN
    get_word(home);
    bio.unmount(home,1);
    IF NOT bio.done THEN
      done:=FALSE; error:=bio.error;
      str.print(note,"can't"' unmount "%s"',home)
    END
  END unmount;

BEGIN
  ns:=0; skip(cmd,ns);
  IF (ns>HIGH(cmd)) OR (cmd[ns]=0c) THEN RETURN FALSE END;
  nf:=skip_name(cmd,(ns));
  IF nf<=ns THEN RETURN FALSE END;
  IF    comp("mount"  ,cmd,ns,nf-1) THEN mount
  ELSIF comp("unmount",cmd,ns,nf-1) THEN unmount
  ELSE RETURN FALSE
  END;
  RETURN TRUE
END dev_controll;

PROCEDURE create(VAR ntsk: tsk.TASK;
                 VAR  cmd: ARRAY OF CHAR;
                 VAR  ipr: BOOLEAN);

  VAR chenv: STRING;
      alias: STRING;
      parms: STRING;
     remote: BOOLEAN;

  PROCEDURE scan_pref(VAR   cmd: ARRAY OF CHAR;
                      VAR chenv: STRING;
                      VAR alias: STRING): BOOLEAN;

    VAR fin,pos: INTEGER;

    PROCEDURE scan_chenv(): INTEGER;
      VAR s,tt,f,p: INTEGER; del: CHAR;
    BEGIN
      p:=pos;
      s:=p;
      WHILE (s<fin) & equation(cmd,s,tt,tt,f,del) DO
        IF NOT done THEN RETURN 0 END;
        IF del#NULL THEN f:=f+1 END;
        IF f>fin    THEN f:=fin END;
        p:=f; s:=p
      END;
      RETURN p
    END scan_chenv;

    VAR i,end: INTEGER;
          ali: STRING;

  BEGIN
    pos:=0; skip(cmd,pos);
    IF (pos>HIGH(cmd)) OR (cmd[pos] # "{") THEN
      tsk.get_str(tsk.self,env.alias,ali);
      IF tsk.done THEN
        NEW(alias,BYTES(ali));
        IF NOT done THEN RETURN TRUE END;
        str.copy(alias,ali)
      END;
      str.delete(cmd,0,pos); RETURN FALSE
    END;
    pos:=pos+1; skip(cmd,pos);
    fin:=pos;
    str.search(cmd,fin,"}");
    IF (fin>HIGH(cmd)) OR (cmd[fin]#"}") THEN
      vis_err(bramiss); RETURN TRUE
    END;
    end:=fin+1;
    REPEAT fin:=fin-1 UNTIL cmd[fin]#" "; fin:=fin+1;
    IF pos<fin THEN
      i:=scan_chenv();
      IF i>pos THEN
        i:=i-pos;
        NEW(chenv,i+1);
        IF NOT done THEN RETURN TRUE END;
        str.sub_str(chenv,cmd,pos,i);
        pos:=pos+i; skip(cmd,pos)
      END;
      IF pos<fin THEN
        i:=fin-pos;
        tsk.get_str(tsk.self,env.alias,ali);
        IF tsk.done THEN i:=i+HIGH(ali)+1 ELSE NEW(ali) END;
        NEW(alias,i+1);
        IF NOT done THEN RETURN TRUE END;
        str.sub_str(alias,cmd,pos,fin-pos);
        IF HIGH(ali)>=0 THEN
          str.append(alias," %s",ali)
        END
      ELSE
        tsk.get_str(tsk.self,env.alias,ali);
        IF tsk.done THEN
          NEW(alias,HIGH(ali)+1);
          IF NOT done THEN RETURN TRUE END;
          str.copy(alias,ali)
        END
      END
    END;
    skip(cmd,end); str.delete(cmd,0,end);
    RETURN FALSE
  END scan_pref;

  PROCEDURE take_ipr(VAR cmd: ARRAY OF CHAR;
                     VAR ipr: BOOLEAN;
                     VAR len: INTEGER);
  BEGIN
    len:=str.len(cmd);
    REPEAT len:=len-1 UNTIL (len<0) OR (cmd[len]#" ");
    IF (len>0) & (cmd[len]="&") THEN
      ipr:=(len=0) OR (cmd[len-1]#"\");
      IF NOT ipr THEN cmd[len-1]:="&"
      ELSE
        REPEAT len:=len-1 UNTIL (len<0) OR (cmd[len]#" ");
        len:=len+1
      END
    ELSE len:=len+1; ipr:=FALSE
    END;
    cmd[len]:=0c
  END take_ipr;

  PROCEDURE take_parm(VAR cmd: ARRAY OF CHAR;
                          len: INTEGER;
                    VAR parms: STRING): BOOLEAN;
    VAR l,i,plen: INTEGER;
  BEGIN
    l:=0;
    l:=skip_name(cmd,l);
    i:=l+1;                       -- skip(cmd,i);
    IF i<len THEN
      NEW(parms,len-i+1);       IF NOT done THEN RETURN TRUE END;
      str.sub_str(parms,cmd,i,len-i)
    END;
    cmd[l]:=0c; RETURN NOT done
  END take_parm;

  PROCEDURE return;
    VAR str: STRING;
  BEGIN
    IF remote THEN
      tsk.get_str(tsk.self,env.cd,str);
      IF tsk.done THEN bio.chdir(str)
      ELSE             bio.chdir("/")
      END;
      remote:=FALSE
    END
  END return;

  PROCEDURE choose(VAR   cmd: ARRAY OF CHAR;
                   VAR tname: ARRAY OF CHAR;
                   VAR chenv: STRING): BOOLEAN;

    VAR ext: ARRAY [0..31] OF CHAR;
       name: ARRAY [0..31] OF CHAR;

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
      bio.splitpathname(path,name);
      IF check_bio(path) THEN RETURN END;
      bio.chdir(path);
      IF check_bio('change_dir("%s")',path) THEN RETURN END;
      remote:=TRUE
    END remote_dir;

    PROCEDURE get_ext;
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
        vis_err(noshell); RETURN
      END;
      tname:=s;
      IF NOT remote OR (cmd="") THEN str.copy(cmd,name)
      ELSE str.append(cmd,"/%s",name);
      END;
      IF remote THEN return END;
(*$<U+*)
      s^:=parms^;
(*$>*)
      NEW(parms,HIGH(s)+str.len(cmd)+6);
      IF NOT done THEN RETURN END;
      str.print(parms,"<%s.sh %s",cmd,s);
      DISPOSE(s)
    END profile;

    VAR test: bio.FILE;

  BEGIN
    remote_dir(cmd,name); IF NOT done THEN RETURN TRUE END;
    get_ext;
    IF    ext=".sh"  THEN profile
    ELSIF ext=".cod" THEN str.copy(tname,name)
    ELSIF ext=""     THEN
      tsk.find(tsk.task0,name);
      IF tsk.done THEN str.copy(tname,name); RETURN FALSE END;
      IF tsk.error#err.no_entry THEN
        RETURN check_tsk('find("%s.cod")',name)
      ELSE
        str.print(tname,"%s.sh",name);
        bio.lookup(etc,test,tname,"x");
        IF bio.done THEN
          bio.close(test); profile
        ELSE
          str.print(note,'"%s.cod" or "%s.@" or "%s.sh"',name,name,name);
          done:=FALSE; error:=err.no_entry
        END;
      END;
    ELSE vis_err(illgext)
    END;
    RETURN NOT done
  END choose;

  PROCEDURE sync_env(T: tsk.TASK;
             VAL chenv: ARRAY OF CHAR;
                  papa: INTEGER);

    VAR ns,nf: INTEGER;
        bs,bf: INTEGER;
         task: INTEGER;
         bump: str8;
          tst: STRING;
          del: CHAR;
          pri: BOOLEAN;

  BEGIN
    tsk.del_env(T,env.chain);

    tsk.get_attr(T,tsk.a_id,task);
    str.print(bump,"%d",task);
    tsk.put_str(T,env.task,bump,TRUE);
    IF check_tsk('put_env("%s")',env.task) THEN RETURN END;

    tsk.get_str(T,env.base,tst);
    IF NOT tsk.done THEN
      tsk.put_str(T,env.base,bump,TRUE);
      IF check_tsk('put_env("%s")',env.base) THEN RETURN END
    END;

    str.print(bump,"%d",papa);
    tsk.put_str(T,env.father,bump,TRUE);
    IF check_tsk('put_env("%s")',env.father) THEN RETURN END;

    ns:=0;
    WHILE (ns<HIGH(chenv)) & equation(chenv,ns,nf,bs,bf,del) DO
      IF NOT done THEN RETURN END;
      pri:=(ns<nf) & (chenv[ns]=".");
      IF pri OR (chenv[ns]="\") THEN INC(ns) END;
      ch_env(T,chenv,ns,nf,bs,bf,pri);
      IF NOT done THEN RETURN END;
      IF (bf<HIGH(chenv)) & (chenv[bf]=del) THEN bf:=bf+1 END;
      ns:=bf
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

  BEGIN
    IF NOT get_num(env.base,dady) THEN vis_err(illbase); RETURN END;
    tsk.open(base,tsk.task0,dady);
    IF check_tsk("open_base(%d)",dady) THEN RETURN END;
    IF NOT get_num(env.stk,stk) THEN stk:=4 END;
    tsk.create(ntsk,(base),tname,alias,stk*256,parms);
    IF check_tsk('create("%s")',tname) THEN release; RETURN END;
    tsk.get_attr(ntsk,tsk.a_id,task);
    sync_env(ntsk,chenv,dady)
  END load_task;

  PROCEDURE release;
  BEGIN
    IF HIGH(alias)>=0 THEN DISPOSE(alias) END;
    IF HIGH(chenv)>=0 THEN DISPOSE(chenv) END;
    IF HIGH(parms)>=0 THEN DISPOSE(parms) END;
    return
  END release;

  PROCEDURE new(): BOOLEAN;
  BEGIN
    NEW(alias); IF NOT done THEN RETURN TRUE END;
    NEW(chenv); IF NOT done THEN RETURN TRUE END;
    NEW(parms);
    RETURN NOT done
  END new;

  VAR len: INTEGER;

BEGIN
  remote:=FALSE;
  IF new() OR scan_pref(cmd,chenv,alias) THEN release; RETURN END;
  take_ipr(cmd,ipr,len);
  IF len=0 THEN vis_err(emptynm);  release; RETURN END;
  IF take_parm(cmd,len,parms) THEN release; RETURN END;
  IF choose(cmd,tname,parms)  THEN release; RETURN END;
  load_task; release
END create;

PROCEDURE command(VAR cmd: ARRAY OF CHAR): BOOLEAN;
   VAR    ntsk: tsk.TASK;
   trap, info: INTEGER;
   cemet, ipr: BOOLEAN;
        chain: STRING;
         exit: BOOLEAN;
         buff: str8;

BEGIN
  tsk.get_str(tsk.self,env.cemetry,chain);
  cemet:=tsk.done & (chain="0"0c);
  exit:=TRUE;
  create(ntsk,cmd,ipr);
  IF NOT done THEN RETURN TRUE END;
  tsk.get_attr(ntsk,tsk.a_id,task);
  str.print(buff,"%d",task);
  tsk.put_str(tsk.self,env.son,buff,TRUE);
  tsk.set_attr(ntsk,tsk.a_ipr,ipr OR env.ipr());
  IF NOT ipr THEN change_brk END;

  tsk.run(ntsk);
  IF check_tsk('run(%d)',task) THEN                RETURN TRUE END;
  IF ipr THEN cause:=cause+awake; tsk.close(ntsk); RETURN TRUE END;

  wait(ntsk,info,trap,cemet,TRUE);
  IF    (trap=00h) OR (trap=47h) THEN
    tsk.get_str(ntsk,env.chain,chain);
    IF tsk.done THEN
      exit:=FALSE; str.copy(buffer,chain)
    END
  ELSIF trap>0   THEN
    IF trap=4Dh THEN
      error:=info; cause:=cause+break
    ELSIF trap=50h THEN cause:=cause+break; error:=trap
    ELSE
      cause:=cause+abort; error:=trap
    END
  END;
  tsk.close(ntsk);
  RETURN exit
END command;

PROCEDURE _system(): BOOLEAN;
  VAR s: INTEGER;
BEGIN
  s:=0; skip(buffer,s);
  IF (s>HIGH(buffer)) OR (buffer[s]=0c) THEN RETURN TRUE END;
  IF dev_controll(buffer) THEN RETURN TRUE END;
  IF env_controll(buffer) THEN RETURN TRUE END;
  IF buffer[s]#"\" THEN
    IF task_controll(buffer) THEN RETURN TRUE END;
  ELSIF (s<HIGH(buffer)) & (buffer[s]#' ') & (buffer[s]#0c) THEN
    buffer[s]:=' '
  END;
  RETURN command(buffer)
END _system;

PROCEDURE system(VAL cmd: ARRAY OF CHAR);
BEGIN
  change_brk;
  cause:={}; done:=TRUE; note:="";
  str.copy(buffer,cmd);
  REPEAT UNTIL _system();
END system;

VAR save_break: INTEGER;

PROCEDURE final; BEGIN restore_brk END final;

PROCEDURE make_environment;
  VAR bump: str80;
      done: BOOLEAN;
       i,n: INTEGER;
         s: STRING;
BEGIN
(*
initialisation:
  TASK=current task number
  FATHER=father of task
  IF BASE="" THEN BASE=$TASK END
*)
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
  CONST max=512; min=80; step=16;
  VAR sz: INTEGER;
BEGIN
(*$<*) (*$U+*)
  sz:=max+step;
  REPEAT sz:=sz-step;
    Heap.allocate(buffer^.ADR,(sz+3) DIV 4)
  UNTIL Heap.done OR (sz<min);
  IF sz<=min THEN HALT(err.heap_abort) END;
  buffer^.HIGH:=(sz+3) DIV 4 * 4;
(*$>*)
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

  old_break:=null;
  env.final(final);

  mask :={0,2};
  cause:={};
  done :=TRUE;
  error:=0;
  note :="";
  task :=-1
END init;

BEGIN
  init
END exeCut.
