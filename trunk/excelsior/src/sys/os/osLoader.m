IMPLEMENTATION MODULE osLoader[1]; (* Ned 29-Apr-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  dcf: defCode;
IMPORT  def: defTasks;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  low: lowLevel;

IMPORT  os : osKernel;

(*$U+*)

CONST ok = err.ok;

CONST MAGIC = 4F4Ch; (* LO *)

TYPE
  ADDRESS  = sys.ADDRESS;
  str32    = ARRAY [0..31] OF CHAR;
  str_ptr  = POINTER TO str32;

TYPE
  entry_ptr = POINTER TO entry_rec;
  entry_rec = RECORD
                G   : sys.ADDRESS;
                task: os.task_ptr;
                init: BOOLEAN;
                next: entry_ptr;
              END;
  node_ptr  = POINTER TO node_rec;
  node_rec  = RECORD
                code: dcf.code_ptr;
                next: node_ptr;
              END;
  PROCs     = DYNARR OF PROC;
  load_ptr  = POINTER TO load_rec;
  load_rec  = RECORD
                magic: INTEGER;
                entry: entry_ptr;
                procs: PROCs;
                nodes: node_ptr;
                stack: INTEGER;
              END;

PROCEDURE module_name(G: ADDRESS): str_ptr; CODE cod.lsw1 END module_name;
PROCEDURE code_name(C: ADDRESS): str_ptr;
CODE cod.lsa dcf.info_size END code_name;

PROCEDURE Code(G: ADDRESS): dcf.code_ptr;
CODE cod.lsw1 cod.lib dcf.info_size cod.sub END Code;
--BEGIN G:=G+1; RETURN ADDRESS(G^)-dcf.info_size END Code;

PROCEDURE Entry(G: ADDRESS): ADDRESS;
CODE cod.li1 cod.sub cod.lsw0      END Entry;
--BEGIN G:=G-1; RETURN ADDRESS(G^) END Entry;

PROCEDURE lookupSystem(VAL name: ARRAY OF CHAR;
                       VAR c   : dcf.code_ptr;
                       VAR G   : ADDRESS);
  VAR p: POINTER TO RECORD glo: ADDRESS; next: ADDRESS END;
      a: ADDRESS; n: str_ptr;
BEGIN
  a:=84h; p:=a^;
  WHILE ADDRESS(p)#0 DO
    n:=module_name(p^.glo);
    IF n^=name THEN G:=p^.glo; c:=Code(G); RETURN END;
    p:=p^.next
  END;
  a:=87h; a:=a^;
  WHILE a#0 DO
    n:=a+1+dcf.info_size;
    IF n^=name THEN G:=NIL; c:=a+1; RETURN END;
    a:=a^
  END;
  c:=NIL; G:=NIL
END lookupSystem;

PROCEDURE _lookup(task: os.task_ptr;
             VAL name: ARRAY OF CHAR;
             VAR    c: dcf.code_ptr;
             VAR    G: sys.ADDRESS): BOOLEAN;
  VAR l: load_ptr; e: entry_ptr; n: node_ptr; mname: str_ptr;
BEGIN
  l:=task^.loader;
  IF l=NIL THEN RETURN FALSE END;
  ASSERT(l^.magic=MAGIC);
  n:=l^.nodes;
  WHILE n#NIL DO
    mname:=code_name(n^.code);
    IF mname^=name THEN c:=n^.code; G:=NIL; RETURN TRUE END;
    n:=n^.next;
  END;
  e:=l^.entry;
  WHILE e#NIL DO
    mname:=module_name(e^.G);
    IF mname^=name THEN G:=e^.G; c:=Code(G); RETURN TRUE END;
    e:=e^.next
  END;
  c:=NIL; G:=NIL;
  RETURN FALSE
END _lookup;

PROCEDURE lookupModule(task: os.task_ptr;
                   VAL name: ARRAY OF CHAR;
                   VAR    c: dcf.code_ptr;
                   VAR    G: sys.ADDRESS);
BEGIN
  c:=NIL; G:=NIL;
  IF (task=NIL) OR (task^.magic#os.MAGIC) THEN RETURN END;
  LOOP
    IF _lookup(task,name,c,G) THEN RETURN END;
    IF task^.node^.papa=NIL   THEN EXIT   END;
    task:=task^.node^.papa^.task
  END;
  lookupSystem(name,c,G)
END lookupModule;

PROCEDURE first_ext(c: dcf.code_ptr; VAR name: str_ptr; VAR time: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  adr:=ADDRESS(c)+dcf.info_size+c^.str_size+c^.code_size+c^.no_mg*2;
  time:=adr^; name:=adr+1;
END first_ext;

PROCEDURE next_ext(c: dcf.code_ptr; VAR name: str_ptr; VAR time: INTEGER);
  VAR i: INTEGER; adr: ADDRESS;
BEGIN
  i:=0;
  WHILE name^[i]#0c DO INC(i) END;
  i:=i+(4 - i MOD 4);
  adr:=ADDRESS(name) + i DIV 4;
  time:=adr^; name:=adr+1
END next_ext;

PROCEDURE check_code(c: dcf.code_ptr;
          VAL glo,name: ARRAY OF CHAR;
                 dtime: INTEGER;
               VAR msg: ARRAY OF CHAR
                 ): INTEGER;
  CONST BAD_TIME ='recompile "%s": time conflict with "%s"';

  VAR inter_name: str_ptr;
BEGIN
  IF c^.vers*dcf.vers_mask#dcf.cur_vers THEN
    str.print(msg,'illegal version "%s"',name); RETURN err.ill_vers
  END;
  IF INTEGER(c^.vers*dcf.cpu_mask<<8)>low.cpu_vers THEN
    str.print(msg,'illegal cpu version "%s"',name); RETURN err.ill_vers
  END;
  inter_name:=code_name(c);
  IF inter_name^#name THEN
    str.print(msg,'"%s"#"%s"',name,inter_name^); RETURN err.inconsistency
  END;
  IF (c^.vers*dcf.check_time#{}) & (dtime>=0) & (dtime#c^.def_time) THEN
    IF c^.imp_time<dtime THEN
      str.print(msg,BAD_TIME,name,glo)
    ELSE
      str.print(msg,BAD_TIME,glo,name)
    END;
    RETURN err.inconsistency
  ELSE
    RETURN ok
  END
END check_code;

PROCEDURE load(task: os.task_ptr;
               name: ARRAY OF CHAR;
             lookup: LOOKUP;
           VAR  msg: ARRAY OF CHAR;
                  ): INTEGER;

  VAR
    gstk : INTEGER;
    pstk : INTEGER;

  PROCEDURE link(VAL glo,name: ARRAY OF CHAR;
                        dtime: INTEGER;
                        VAR G: ADDRESS): INTEGER;

    PROCEDURE load_exts(c: dcf.code_ptr; ldft: ADDRESS): INTEGER;
      VAR pExt: str_ptr;   imp: dcf.code_ptr; G: ADDRESS;
         n,res: INTEGER;  time: INTEGER;
    BEGIN
      n:=c^.no_exts;
      IF n<=1 THEN RETURN ok END;
      first_ext(c,pExt,time);
      LOOP
        res:=link(name,pExt^,time,G);
        IF res#ok THEN RETURN res END;
        ldft^:=Entry(G);
        INC(ldft); DEC(n);
        IF n=1 THEN RETURN ok END;
        next_ext(c,pExt,time)
      END;
    END load_exts;

    PROCEDURE alloc_multi_glo(c: dcf.code_ptr; G: ADDRESS): INTEGER;
      VAR mg,mul: ADDRESS; nomg,ofs,size: INTEGER;
    BEGIN
      mg:=ADDRESS(c)+dcf.info_size+c^.str_size+c^.code_size;
      nomg:=c^.no_mg;
      WHILE nomg>0 DO
        ofs :=mg^; mg:=mg+1;
        size:=mg^; mg:=mg+1;
        mul:=G+ofs;
        os.ALLOCATE(task^.area,mul^,size);
        IF mul^=NIL THEN RETURN err.no_memory END;
        DEC(nomg)
      END;
      RETURN ok
    END alloc_multi_glo;

    VAR c: dcf.code_ptr;
        e: entry_ptr;
        l: load_ptr;
      res: INTEGER;
     tags: BITSET;
   a,lDFT: ADDRESS;

  BEGIN
    IF _lookup(task,name,c,G) & (G#NIL) THEN
      RETURN check_code(Code(G),glo,name,dtime,msg)
    END;
    res:=lookup(task,name,c,G,tags);
    IF res#ok THEN RETURN res END;
    res:=check_code(c,glo,name,dtime,msg);
    IF res#ok THEN RETURN res END;
    IF G#NIL THEN
      e:=Entry(G);
      IF (c^.tag=1) OR (unic*tags#{}) OR (e^.task=task) THEN  -- ?? E^.task
        IF pstk<c^.add_stack THEN pstk:=c^.add_stack END;
        RETURN ok
      END
    END;
    INC(gstk,c^.min_stack);
    IF pstk<c^.add_stack THEN pstk:=c^.add_stack END;

    os.ALLOCATE(task^.area,lDFT,c^.glo_size);
    IF lDFT=NIL THEN RETURN err.no_memory END;
    G :=lDFT+c^.no_exts;
    G^:=ADDRESS(c)+dcf.info_size+c^.str_size;
    a :=G+1;
    a^:=ADDRESS(c)+dcf.info_size;

    os.ALLOCATE(task^.area,e,SIZE(e^));
    IF e=NIL THEN RETURN err.no_memory END;
    a:=G-1; a^:=e;
    e^.G:=G; e^.init:=FALSE; e^.task:=task;
    l:=task^.loader;
    e^.next:=l^.entry; l^.entry:=e;

    res:=alloc_multi_glo(c,G);
    IF res#ok THEN RETURN res END;

    RETURN load_exts(c,lDFT)
  END link;

  PROCEDURE make_init(G: ADDRESS): INTEGER;

    VAR
      procs: PROCs;
      cp   : INTEGER;

    PROCEDURE initmodule(E: entry_ptr): INTEGER;
      VAR n,res: INTEGER; l: ADDRESS;
          e: entry_ptr; c: dcf.code_ptr;
    BEGIN
      E^.init:=TRUE;    l:=E^.G-2;
      c:=Code(E^.G);    n:=c^.no_exts;
      WHILE n>1 DO
        e:=l^; DEC(n); DEC(l);
        IF (e^.task=task) & NOT e^.init THEN
          res:=initmodule(e);
          IF res#ok THEN RETURN res END
        END
      END;
      procs[cp]:=PROC(E); INC(cp);
      RETURN ok
    END initmodule;

    VAR e: entry_ptr;  l: load_ptr;  c: dcf.code_ptr;

  BEGIN
    l:=task^.loader;
    e:=l^.entry; cp:=0;
    WHILE e#NIL DO
      IF e^.task=task THEN INC(cp) END;
      e:=e^.next
    END;
    procs^.HIGH:=cp-1;
    os.ALLOCATE(task^.area,procs^.ADR,SIZE(procs));
    IF procs^.ADR=NIL THEN RETURN err.no_memory END;
    l^.procs^:=procs^; cp:=0;
    c:=Code(G);
    task^.mucode:=INTEGER(c^.usercode);
    RETURN initmodule(Entry(G))
  END make_init;

  PROCEDURE _load(): INTEGER;
    VAR res: INTEGER; G: ADDRESS; c: dcf.code_ptr; tags: BITSET;
  BEGIN
    res:=lookup(task,name,c,G,tags);
    IF res#ok THEN RETURN res END;
    IF G#NIL THEN
      IF (tags*unic#{}) OR (c^.tag=1) THEN RETURN err.busy END;
    END;
    res:=link('',name,-1,G);
    IF res#ok THEN RETURN res END;
    RETURN make_init(G)
  END _load;

  VAR res: INTEGER; l: load_ptr;
BEGIN
  msg:="";
  IF (task=NIL) OR (task^.magic#os.MAGIC) THEN RETURN err.bad_desc END;
  os.acquire(task^.lock);
    IF task^.status#def.new THEN res:=err.ill_desc
    ELSE
      os.ALLOCATE(task^.area,l,SIZE(l^));
      IF l=NIL THEN res:=err.no_memory
      ELSE
        l^.magic:=MAGIC;
        l^.entry:=NIL;
        l^.nodes:=NIL;
        l^.procs^.ADR :=NIL;
        l^.procs^.HIGH:=-1;
        task^.status:=def.loaderr;
        task^.loader:=l;
        gstk:=0; pstk:=0;
        res:=_load();
        IF res=ok THEN task^.status:=def.loaded; l^.stack:=pstk*2+gstk END
      END
    END;
  os.release(task^.lock);
  RETURN res
END load;

PROCEDURE load_codes(task: os.task_ptr;
                     name: ARRAY OF CHAR;
                   lookup: LOOKUP;
                 VAR  msg: ARRAY OF CHAR;
                     ): INTEGER;

  VAR loader: load_ptr;

  PROCEDURE link(VAL glo,name: ARRAY OF CHAR; dtime: INTEGER): INTEGER;

    PROCEDURE load_exts(c: dcf.code_ptr): INTEGER;
      VAR pExt: str_ptr; n,res,time: INTEGER;
    BEGIN
      n:=c^.no_exts;
      IF n<=1 THEN RETURN ok END;
      first_ext(c,pExt,time);
      LOOP
        res:=link(name,pExt^,time);
        IF res#ok THEN RETURN res END;
        DEC(n);
        IF n=1 THEN RETURN ok END;
        next_ext(c,pExt,time)
      END
    END load_exts;

    VAR c: dcf.code_ptr;
        n: node_ptr;
        G: ADDRESS;
      res: INTEGER;
     tags: BITSET;

  BEGIN
    res:=lookup(task,name,c,G,tags);
    IF res#ok THEN RETURN res END;
    res:=check_code(c,glo,name,dtime,msg);
    IF res#ok THEN RETURN res END;
    IF tags*new={} THEN RETURN ok END;
    os.ALLOCATE(task^.area,n,SIZE(n^));
    n^.code:=c; n^.next:=loader^.nodes; loader^.nodes:=n;
    RETURN load_exts(c)
  END link;

  PROCEDURE dispose(bound: node_ptr);
    VAR l,x: node_ptr;
  BEGIN
    l:=loader^.nodes;
    WHILE l#bound DO
      x:=l; l:=x^.next;
      IF x^.code^.size>0 THEN
        os.DEALLOCATE(task^.area,x^.code,x^.code^.size);
      END;
      os.DEALLOCATE(task^.area,x,SIZE(x^))
    END;
    loader^.nodes:=bound
  END dispose;

  VAR res: INTEGER; bound,x: node_ptr;

BEGIN
  msg:="";
  IF (task=NIL) OR (task^.magic#os.MAGIC) THEN RETURN err.bad_desc END;
  os.acquire(task^.lock);
    IF NOT (task^.status IN {def.loaded,def.ready,def.running}) THEN
      res:=err.ill_desc
    ELSE
      loader:=task^.loader;
      ASSERT(loader^.magic=MAGIC);
      bound :=loader^.nodes;
      res:=link('',name,-1);
      IF res#ok THEN dispose(bound) END
    END;
  os.release(task^.lock);
  RETURN res
END load_codes;

PROCEDURE task_starter;

  PROCEDURE alloc; CODE cod.alloc END alloc;
  PROCEDURE drop;  CODE cod.drop  END drop;

  VAR t: os.task_ptr; l: load_ptr; ps: PROCs; n: INTEGER;
BEGIN
  t:=os.self();
  l:=t^.loader;
  ASSERT(l^.magic=MAGIC);
  ps^:=l^.procs^; ASSERT(ps^.ADR#NIL);
  n:=0;
  WHILE n<=HIGH(ps) DO
    ps[n]; alloc; drop; INC(n);
  END;
  HALT
END task_starter;

PROCEDURE run(task: os.task_ptr; stack: INTEGER): INTEGER;
  VAR l: load_ptr;
BEGIN
  IF (task=NIL) OR (task^.magic#os.MAGIC) THEN RETURN err.bad_desc END;
  os.acquire(task^.lock);
    l:=task^.loader;
    ASSERT(l^.magic=MAGIC);
    INC(stack,l^.stack);
  os.release(task^.lock);
  RETURN os.run(task,task_starter,stack)
END run;

----------------------------------------------------------------

PROCEDURE unpack(glo: ADDRESS; VAR u: UNPACKED);
  VAR c: dcf.code_ptr;
      g: POINTER TO RECORD F,S: sys.ADDRESS END;
BEGIN
  ASSERT(glo#NIL);
  g:=glo;
  c:=Code(glo);
  u.cmds^.ADR :=g^.F;
  u.cmds^.HIGH:=c^.code_size*4-1;
  u.proc^.ADR :=g^.F;
  u.proc^.HIGH:=c^.no_proc-1;
  u.pool^.ADR :=g^.S;
  u.pool^.HIGH:=c^.str_size*4-1;
  u.code:=c;
  u.exts^.ADR :=glo-c^.no_exts;
  u.exts^.HIGH:=c^.no_exts-2;
END unpack;

----------------------------------------------------------------

VAR l: load_ptr;  x: os.task_ptr;

BEGIN
  os.ALLOCATE(os.task0^.area,l,SIZE(l^));
  ASSERT(l#NIL);
  l^.magic:=MAGIC;
  l^.entry:=NIL;
  l^.nodes:=NIL;
  l^.stack:=0;
  NEW(l^.procs);
  x:=os.task0;
  x^.loader:=l
END osLoader.
