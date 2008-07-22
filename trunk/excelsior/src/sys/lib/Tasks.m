IMPLEMENTATION MODULE Tasks; (* Ned 19-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  dcf: defCode;
IMPORT  err: defErrors;
IMPORT  def: defTasks;

IMPORT  os : osKernel;
IMPORT  ldr: osLoader;

IMPORT  str: Strings;
IMPORT  env: tskEnv;
IMPORT  bio: BIO;
IMPORT  syn: Signals;
IMPORT  usr: Users;
IMPORT  mem: Heap;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

CONST ok = err.ok;

---------------------------  TASKs  ----------------------------
                           ---------

VAR MAGIC: INTEGER;

CONST max_sig = 7;

TYPE
  TASK   = POINTER TO T_DESC;
  T_DESC = RECORD
             magic: INTEGER;
             task : os.task_ptr;
             sigs : ARRAY [0..max_sig] OF INTEGER;
             fwd  : TASK;
             bck  : TASK;
           END;

VAR tasks: TASK;
      BIN: bio.PATHs;

PROCEDURE tie(T: TASK);
  VAR i: INTEGER;
BEGIN
  IF tasks=NIL THEN
    tasks:=T; T^.fwd:=T; T^.bck:=T
  ELSE
    T^.fwd:=tasks;
    T^.bck:=tasks^.bck;
    T^.bck^.fwd:=T;
    T^.fwd^.bck:=T
  END;
  FOR i:=0 TO HIGH(T^.sigs) DO T^.sigs[i]:=0 END;
  T^.magic:=MAGIC
END tie;

PROCEDURE untie(T: TASK);
BEGIN
  IF (T=NIL) OR (T=self) OR (T=task0) THEN RETURN END;
  IF tasks=T THEN tasks:=tasks^.fwd END;
  IF tasks=T THEN tasks:=NIL END;
  T^.fwd^.bck:=T^.bck;
  T^.bck^.fwd:=T^.fwd;
  T^.magic:=0
END untie;


----------------------------  BIN  -----------------------------
                            -------

PROCEDURE chpaths;
  VAR new: bio.PATHs;
BEGIN
  bio.get_paths(new,env.bin);
  IF NOT bio.done THEN
    done:=FALSE; error:=bio.error;
    str.print(note,'chpaths "%s"',env.bin);
    RETURN
  END;
  bio.close_paths(BIN);
  BIN:=new
END chpaths;

----------------------------  LOAD  ----------------------------
                            --------

CONST str_size = 32; table_size = 64;

CONST -- modes
  _normal    = 0;
  _unic      = 1;
  _new_code  = 2;
  _alias     = 3;

TYPE
  str32   = ARRAY [0..str_size-1] OF CHAR;
  mod_ptr = POINTER TO mod_rec;
  mod_rec = RECORD
              mode: INTEGER;
              name: str32;
              fn  : STRING;
              code: dcf.code_ptr;
              glo : SYSTEM.ADDRESS;
              next: mod_ptr;
            END;

VAR table: ARRAY [0..table_size-1] OF mod_ptr;

PROCEDURE sum(VAL name: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER; sum: BITSET;
BEGIN
  sum:={}; i:=0;
  WHILE (i<=HIGH(name)) & (name[i]#0c) DO
    sum:=BITSET(sum<<3) / BITSET(ORD(name[i])*16+i);
    INC(i);
  END;
  RETURN INTEGER( sum/(sum>>8)/(sum<<8) ) MOD table_size
END sum;

PROCEDURE append(VAR c: dcf.code_ptr): INTEGER;
  VAR pname: POINTER TO str32; n: INTEGER; m: mod_ptr;
BEGIN
  IF c^.vers*dcf.vers_mask#dcf.cur_vers THEN RETURN err.ill_vers END;
  pname:=SYSTEM.ADDRESS(c)+dcf.info_size;
  n:=sum(pname^); m:=table[n];
  WHILE m#NIL DO
    IF m^.name=pname^ THEN
      IF m^.mode=_new_code THEN
        m^.mode:=_normal; m^.code:=c;
        n:=c^.size; c^.size:=0;
        c:=SYSTEM.ADDRESS(c)+n;
        RETURN ok
      ELSE
        str.print(note,'unpack("%s")',m^.name);
        RETURN err.duplicate
      END;
    END;
    m:=m^.next;
  END;
  NEW(m);
  IF m=NIL THEN RETURN err.no_memory END;
  str.copy(m^.name,pname^);
  m^.next:=table[n]; table[n]:=m;
  m^.mode:=_normal;
  m^.code:=c; m^.glo:=NIL;
  n:=c^.size; c^.size:=0;
  c:=SYSTEM.ADDRESS(c)+n;
  RETURN ok
END append;

PROCEDURE get(task: os.task_ptr; m: mod_ptr): INTEGER;

  PROCEDURE read(f: bio.FILE; VAR c: dcf.code_ptr; VAR s: INTEGER): INTEGER;
    VAR eof: INTEGER;
  BEGIN
    bio.chmode(f,'r');
    IF NOT bio.done THEN RETURN bio.error END;
    eof:=bio.eof(f); s:=(eof+3) DIV 4;
    os.ALLOCATE(task^.area,c,s);
    IF c=NIL THEN RETURN err.no_memory END;
    bio.read(f,c,eof);
    IF NOT bio.done THEN RETURN bio.error END;
    RETURN ok
  END read;

  PROCEDURE pug(f: bio.FILE; c: dcf.code_ptr): INTEGER;
    VAR p: BITSET; u,g: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_pro,p);
    IF NOT bio.done THEN RETURN bio.error END;
    bio.get_attr(f,bio.a_uid,u);
    IF NOT bio.done THEN RETURN bio.error END;
    bio.get_attr(f,bio.a_gid,g);
    IF NOT bio.done THEN RETURN bio.error END;
    u:=usr.pack(u,g,FALSE);
    c^.usercode:=BITSET(u<<16)+p;
    RETURN ok
  END pug;

  PROCEDURE er;
  BEGIN
    IF note='' THEN
      str.print(note,'get("%s")',m^.name)
    END
  END er;

  VAR c: dcf.code_ptr;
      f: bio.FILE;
     fn: ARRAY [0..40] OF CHAR;
    res: INTEGER;
   save: INTEGER;
 size,n: INTEGER;
  first: dcf.code_ptr;

BEGIN
  IF m^.mode=_alias THEN
    bio.lookup(BIN,f,m^.fn,'x');
    m^.mode:=_new_code; DISPOSE(m^.fn)
  ELSE
    str.print(fn,"%s.cod",m^.name);
    bio.lookup(BIN,f,fn,'x')
  END;
  IF NOT bio.done THEN er; RETURN bio.error END;
  save:=self^.task^.user;
  self^.task^.user:=INTEGER(BITSET(save)+{7});
    res:=read(f,c,size);
  self^.task^.user:=save;
  IF res=ok THEN res:=pug(f,c) END;
  bio.close(f);
  IF (res=ok) & NOT bio.done THEN res:=bio.error END;
  IF res#ok THEN er; RETURN res END;
  IF c^.size=0 THEN m^.mode:=_normal; m^.code:=c; c^.size:=size;
  ELSE
    first:=c; n:=size;
    REPEAT
      DEC(n,c^.size);
      res:=append(c);
    UNTIL (res#ok) OR (n<=0);
    first^.size:=size;
  END;
  IF (res=ok) & (m^.code=NIL) THEN er; RETURN err.no_entry END;
  IF res#ok THEN er END;
  RETURN res
END get;

PROCEDURE find(task: TASK; VAL name: ARRAY OF CHAR);
  VAR f: bio.FILE;
     fn: ARRAY [0..39] OF CHAR;
BEGIN
  lookup_module(task,name);
  IF done THEN RETURN END;
  str.print(note,'find("%s")',name);
  str.print(fn,"%s.cod",name);
  bio.lookup(BIN,f,fn,'x');
  done:=bio.done;
  IF done THEN bio.close(f) ELSE error:=bio.error END
END find;

PROCEDURE lookup(task: os.task_ptr;
             VAL name: ARRAY OF CHAR;
             VAR code: dcf.code_ptr;
             VAR glo : SYSTEM.ADDRESS;
             VAR tags: BITSET): INTEGER;
  VAR n,res: INTEGER; m: mod_ptr;
BEGIN
  tags:={};
  n:=sum(name);
  m:=table[n];
  WHILE m#NIL DO
    IF m^.name=name THEN
      IF m^.mode IN {_new_code,_alias} THEN
        res:=get(task,m); glo:=NIL;
        IF res=ok THEN code:=m^.code; tags:=tags+ldr.new END;
        RETURN res
      ELSE
        code:=m^.code; glo:=m^.glo;
        IF m^.mode=_unic THEN tags:=tags+ldr.unic END;
        RETURN ok
      END;
    END;
    m:=m^.next;
  END;
  NEW(m);
  IF m=NIL THEN RETURN err.no_memory END;
  str.copy(m^.name,name);
  m^.next:=table[n]; table[n]:=m;
  m^.mode:=_normal;
  ldr.lookupModule(task,name,m^.code,m^.glo);
  IF m^.code#NIL THEN res:=ok
  ELSE m^.mode:=_new_code; res:=get(task,m); tags:=tags+ldr.new
  END;
  IF res=ok THEN code:=m^.code; glo:=m^.glo END;
  RETURN res
END lookup;

PROCEDURE set_alias(papa: os.task_ptr; VAL s: ARRAY OF CHAR): INTEGER;

  PROCEDURE check_unic(glo: SYSTEM.ADDRESS): INTEGER;
    VAR u,a: ldr.UNPACKED; i: INTEGER;
  BEGIN
    ldr.unpack(glo,u);
    FOR i:=0 TO HIGH(u.exts) DO
      ldr.unpack(u.exts[i]^,a);
      IF a.code^.tag#1 THEN RETURN err.unsuitable END;
    END;
    RETURN ok
  END check_unic;

  PROCEDURE insert(m: mod_ptr): INTEGER;
    VAR n: INTEGER; l: mod_ptr;
  BEGIN
    n:=sum(m^.name); l:=table[n];
    WHILE l#NIL DO
      IF l^.name=m^.name THEN
        IF (l^.mode=m^.mode) & (l^.mode#_alias) THEN RETURN ok
        ELSE RETURN err.duplicate
        END;
      END;
      l:=l^.next;
    END;
    m^.next:=table[n]; table[n]:=m;
    IF m^.mode=_unic THEN
      ldr.lookupModule(papa,m^.name,m^.code,m^.glo);
      IF m^.glo=NIL THEN RETURN err.no_entry END;
      RETURN check_unic(m^.glo)
    ELSE
      ldr.lookupModule(papa,m^.name,m^.code,m^.glo);
      IF (m^.glo#NIL) & (m^.code^.tag=1) THEN RETURN err.busy END;
      m^.code:=NIL; m^.glo:=NIL;
    END;
    RETURN ok
  END insert;

  PROCEDURE word(VAL s: ARRAY OF CHAR; size: INTEGER; VAR pos,len: INTEGER);
    VAR i: INTEGER;
  BEGIN i:=pos; len:=0;
    WHILE (i<size) & (s[i]=' ') DO INC(i) END;
    IF i>=size THEN RETURN END;
    pos:=i;
    WHILE (i<size) & (s[i]#' ') DO INC(i) END;
    len:=i-pos;
  END word;

  VAR size,len,i,res: INTEGER; c: CHAR;
    m: mod_ptr; mode: INTEGER;

BEGIN
  size:=str.len(s);
  IF size<=0 THEN RETURN ok END;
  i:=0;
  LOOP
    word(s,size,i,len);
    IF len=0 THEN RETURN ok END;
    c:=s[i];
    IF    c='-' THEN mode:=_new_code
    ELSIF c='+' THEN mode:=_unic
    ELSIF c='=' THEN mode:=_alias
    ELSE str.print(note,'alias: illegal prefix "%c"',c); RETURN err.bad_parm
    END;
    IF (len=1) OR (len>=str_size)  THEN RETURN err.bad_parm END;
    NEW(m);
    IF m=NIL THEN RETURN err.no_memory END;
    m^.mode:=mode; m^.code:=NIL; m^.glo:=NIL; NEW(m^.fn);
    str.sub_arr(m^.name,s,i+1,len-1);
    res:=insert(m);
    IF res#ok THEN
      str.print(note,'alias: "%c%s"',c,m^.name); RETURN res
    END;
    INC(i,len);
    IF mode=_alias THEN
      word(s,size,i,len);
      IF len=0 THEN RETURN err.bad_parm END;
      NEW(m^.fn,len+1);
      IF LEN(m^.fn)<=0 THEN RETURN err.no_memory END;
      str.sub_arr(m^.fn,s,i,len);
      INC(i,len);
    END;
  END;
END set_alias;

PROCEDURE dispose_table;
  VAR i: INTEGER; m,x: mod_ptr;
BEGIN
  FOR i:=0 TO HIGH(table) DO
    m:=table[i];
    WHILE m#NIL DO
      x:=m; m:=m^.next;
      IF x^.mode=_alias THEN DISPOSE(x^.fn) END;
      DISPOSE(x)
    END
  END;
END dispose_table;

PROCEDURE load(T: TASK; VAL name,alias: ARRAY OF CHAR): INTEGER;
  VAR papa: os.task_ptr; i,res: INTEGER;
BEGIN
  IF (T=NIL) OR (T^.magic#MAGIC) THEN RETURN err.bad_desc END;
  os.papa(T^.task,i);
  res:=os.open(papa,os.task0,i);
  IF res#ok THEN RETURN res END;
  FOR i:=0 TO HIGH(table) DO table[i]:=NIL END;
  res:=set_alias(papa,alias);
  os.close(papa);
  IF res=ok THEN res:=ldr.load(T^.task,name,lookup,note) END;
  dispose_table;
  RETURN res
END load;

PROCEDURE load_codes(T: TASK; VAL name,alias: ARRAY OF CHAR);
  VAR task: os.task_ptr; res,i: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  task:=T^.task;
  os.reopen(task);
  FOR i:=0 TO HIGH(table) DO table[i]:=NIL END;
  res:=set_alias(task,alias);
  IF res=ok THEN
    res:=ldr.load_codes(task,name,lookup,note);
    dispose_table;
    IF res#ok THEN error:=res ELSE done:=TRUE END
  ELSE
    error:=res
  END;
  os.close(task)
END load_codes;

PROCEDURE _run(task: os.task_ptr;
             stack: INTEGER;
          VAL name: ARRAY OF CHAR;
          VAL parm: ARRAY OF CHAR): INTEGER;
  VAR res: INTEGER;
BEGIN
  res:=os.copy_env(os.self(),task);
  IF res#ok THEN RETURN res END;
  res:=os.put_str(task,env.name,name,TRUE);
  IF res#ok THEN RETURN res END;
  IF HIGH(parm)>=0 THEN
    res:=os.put_str(task,env.args,parm,TRUE);
    IF res#ok THEN RETURN res END
  END;
  IF BITSET(task^.mucode)*bio.run_uid#{} THEN
    task^.user:=INTEGER(BITSET(task^.mucode>>16)*{0..6,8..14})
  END;
  IF BITSET(task^.mucode)*bio.run_priv#{} THEN
    task^.user:=INTEGER(BITSET(task^.user)+{7})
  END;
  res:=ldr.run(task,stack);
  IF res#ok THEN RETURN res END;
  RETURN ok
END _run;

PROCEDURE create(VAR     T: TASK;
                    father: TASK;
                 VAL  name: ARRAY OF CHAR;
                 VAL alias: ARRAY OF CHAR;
                     stack: INTEGER;
                 VAL  parm: ARRAY OF CHAR);
  VAR res: INTEGER; f: os.task_ptr;
BEGIN
  done:=FALSE; note[0]:=0c;
  IF (father=NIL) OR (father^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  f:=father^.task;
  NEW(T);
  IF T=NIL THEN error:=err.no_memory; RETURN END;
  os.lock;
    res:=os.create(T^.task,f);
    IF res=ok THEN tie(T) END;
  os.unlock;
  IF res#ok THEN error:=res; DISPOSE(T); RETURN END;
  res:=load(T,name,alias);
  IF res=ok THEN res:=_run(T^.task,stack,name,parm) END;
  done:=(res=ok);
  IF res#ok THEN
    error:=res;
    os.lock;
      os.send(T^.task^.inp[def.kill]^);
      os.close(T^.task);
      untie(T); DISPOSE(T);
    os.unlock;
  END;
END create;

PROCEDURE run(T: TASK);
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  IF T^.task^.status#def.ready   THEN error:=err.ill_desc; RETURN END;
  os.send(T^.task^.inp[def.start]^);
  done:=TRUE;
END run;

PROCEDURE history(T: TASK; VAR cause: INTEGER; VAR s: ARRAY OF CHAR);
  VAR p: os.process;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  os.acquire(T^.task^.lock);
    p:=T^.task^.main;
    IF p=NIL THEN error:=err.ill_desc;
    ELSE
      cause:=p^.pp^.T;
      IF cause#47h THEN os.extract(s,p^.pp) END;
      done:=TRUE
    END;
  os.release(T^.task^.lock)
END history;

----------------------------------------------------------------

PROCEDURE signal(T: TASK; no: INTEGER);
  VAR s: os.signal_ptr;
BEGIN
  done:=FALSE;
  IF NOT (no IN {1..max_sig})    THEN error:=err.bad_parm; RETURN END;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  os.send(T^.task^.inp[no]^);
  done:=TRUE;
END signal;

PROCEDURE get_signal(VAR s: syn.SIGNAL; T: TASK; no: INTEGER);
BEGIN
  done:=FALSE;
  IF NOT (no IN {1..max_sig})    THEN error:=err.bad_parm; RETURN END;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  os.lock;
    INC(T^.sigs[no]);
    s:=syn.SIGNAL(T^.task^.out[no]);
  os.unlock;
  done:=TRUE;
END get_signal;

PROCEDURE free_signal(VAR s: syn.SIGNAL; T: TASK; no: INTEGER);
BEGIN
  done:=FALSE;
  IF NOT (no IN {1..max_sig})    THEN error:=err.bad_parm; RETURN END;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  IF T^.sigs[no]>0 THEN DEC(T^.sigs[no]) ELSE T^.sigs[no]:=0 END;
  s:=syn.null;
  done:=TRUE
END free_signal;

----------------------------------------------------------------

PROCEDURE _tie(res: INTEGER; T: TASK);
BEGIN
  done:=(res=ok);
  IF done THEN tie(T) ELSE DISPOSE(T); error:=res END
END _tie;

PROCEDURE open(VAR T: TASK; papa: TASK; id: INTEGER);
  VAR task: os.task_ptr;
BEGIN
  done:=FALSE;
  IF (papa=NIL) OR (papa^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  task:=papa^.task;
  NEW(T);
  IF T=NIL THEN error:=err.no_memory; RETURN END;
  os.lock;
    _tie(os.open(T^.task,task,id),T);
  os.unlock;
END open;

PROCEDURE papa(T: TASK; VAR id: INTEGER);
BEGIN
  done:=FALSE;  id:=-1;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  os.papa(T^.task,id);
  done:=(id>=0);
  IF NOT done THEN error:=err.no_entry END;
END papa;

PROCEDURE son(T: TASK; VAR id: INTEGER);
BEGIN
  done:=FALSE; id:=-1;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  os.son(T^.task,id);
  done:=(id>=0);
  IF NOT done THEN error:=err.no_entry END;
END son;

PROCEDURE brother(T: TASK; VAR id: INTEGER);
BEGIN
  done:=FALSE;  id:=-1;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  os.brother(T^.task,id);
  done:=(id>=0);
  IF NOT done THEN error:=err.no_entry END;
END brother;

PROCEDURE close(VAR T: TASK);
  VAR res,i: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  FOR i:=0 TO HIGH(T^.sigs) DO
    IF T^.sigs[i]>0 THEN error:=err.busy; RETURN END;
  END;
  os.lock;
    os.close(T^.task);
    done:=TRUE;
    untie(T); DISPOSE(T);
  os.unlock;
END close;

PROCEDURE get_attr(T: TASK; no: INTEGER; VAR val: SYSTEM.WORD);
  VAR x: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  CASE no OF
    |a_status : val:=T^.task^.status;
    |a_mem    : os.info(T^.task^.area,val,x);
    |a_user   : val:=T^.task^.user;
    |a_id     : val:=T^.task^.id;
    |a_ipr    : val:=T^.task^.ipr;
    |a_res    : val:=T^.task^.res;
  ELSE error:=err.inv_op; RETURN
  END;
  done:=TRUE;
END get_attr;

PROCEDURE set_attr(T: TASK; no: INTEGER; val: SYSTEM.WORD);
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  CASE no OF
  |a_user: done:=usr.su(usr.user());
           IF done THEN T^.task^.user:=val ELSE error:=err.su_only END
  |a_ipr : done:=TRUE; T^.task^.ipr:=val#0
  ELSE
    error:=err.inv_op
  END
END set_attr;

PROCEDURE caller(VAR id: INTEGER);
  VAR t: os.task_ptr;
BEGIN
  t:=os.self(); id:=t^.id; done:=TRUE
END caller;

PROCEDURE lookup_module(T: TASK; VAL name: ARRAY OF CHAR);
  VAR c: dcf.code_ptr; G: SYSTEM.ADDRESS;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  ldr.lookupModule(T^.task,name,c,G);
  done:=(c#NIL);
  IF NOT done THEN error:=err.no_entry END
END lookup_module;

------------------------  ENVIRONMENT  -------------------------
                        ---------------

PROCEDURE get_str(T: TASK; VAL name: ARRAY OF CHAR; VAR s: STRING);
  VAR res: INTEGER; priv: BOOLEAN;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  res:=os.get_env(T^.task,-1,name,s,priv);
  done:=(res=ok);
  IF NOT done THEN
    error:=res; NEW(s);
  END;
END get_str;

PROCEDURE get_env(T   : TASK;
                  name: ARRAY OF CHAR;
               milisec: INTEGER;
              VAR data: STRING;
              VAR priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  res:=os.get_env(T^.task,(milisec+os.tick-1) DIV os.tick,name,data,priv);
  done:=(res=ok);
  IF NOT done THEN
    error:=res; NEW(data);
  END;
END get_env;

PROCEDURE put_str(T: TASK; VAL name,str: ARRAY OF CHAR; priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  res:=os.put_str(T^.task,name,str,priv);
  done:=(res=ok);
  IF NOT done THEN error:=res END;
END put_str;

PROCEDURE put_env(   T: TASK;
              VAL name: ARRAY OF CHAR;
              VAL data: ARRAY OF SYSTEM.WORD;
                  priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  res:=os.put_env(T^.task,name,data,priv);
  done:=(res=ok);
  IF res#ok THEN error:=res END;
END put_env;

PROCEDURE env_entry(   T: TASK;
                      en: INTEGER;
                 milisec: INTEGER;
                VAR name: STRING;
                VAR data: STRING;
                VAR priv: BOOLEAN);
  VAR res: INTEGER;
BEGIN
  done:=FALSE;
  NEW(data); NEW(name);
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  res:=os.show_env(T^.task,en,(milisec+os.tick-1) DIV os.tick,name,data,priv);
  done:=(res=ok);
  IF NOT done THEN error:=res END
END env_entry;

PROCEDURE del_env(T: TASK; VAL name: ARRAY OF CHAR);
  VAR res: INTEGER;
BEGIN
  done:=FALSE;
  IF (T=NIL) OR (T^.magic#MAGIC) THEN error:=err.bad_desc; RETURN END;
  res:=os.del_env(T^.task,name);
  done:=(res=ok);
  IF res#ok THEN error:=res END;
END del_env;

PROCEDURE xole(T: TASK; VAR x: SYSTEM.WORD);
BEGIN
  done:=(T#NIL) & (T^.magic=MAGIC);
  IF done THEN x:=T^.task ELSE x:=NIL; error:=err.bad_desc END;
END xole;

----------------------------------------------------------------

PROCEDURE finish;
  VAR l: TASK;
BEGIN
  l:=tasks;
  IF l=NIL THEN RETURN END;
  REPEAT
    IF l^.magic=MAGIC THEN os.close(l^.task) END;
    l:=l^.fwd;
  UNTIL l=tasks
END finish;

VAR c: INTEGER;

BEGIN
  MAGIC:=6B737424h; (* $tsk *)
  c:=mem.credit;
  mem.set_credit(0);
  null:=NIL; done:=TRUE; error:=ok; note[0]:=0c;

  NEW(self);
  IF self=NIL THEN HALT(err.heap_abort) END;
  tasks:=NIL;
  NEW(task0);
  IF task0=NIL THEN HALT(err.heap_abort) END;

  task0^.task:=os.task0;
  self^ .task:=os.self();
  IF os.final(self^.task,finish)#ok THEN HALT(err.no_resource) END;
  os.lock;
    os.reopen(task0^.task);
    os.reopen(self ^.task);
    self ^.magic:=MAGIC;
    task0^.magic:=MAGIC;
    task0^.fwd:=self;
    task0^.bck:=self;
    self ^.fwd:=task0;
    self ^.bck:=task0;
    tasks:=task0;
  os.unlock;

  bio.open_paths(BIN,".");
  chpaths;
  mem.set_credit(c)
END Tasks.
