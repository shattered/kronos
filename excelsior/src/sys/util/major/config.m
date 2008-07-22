MODULE config; (* Leo 01-Dec-89. (c) KRONOS *) IMPORT  SYSTEM;

(*$U+*)

IMPORT  bio: BIO;               IMPORT  mem: Heap;
IMPORT  str: Strings;           IMPORT  std: StdIO;
IMPORT  err: defErrors;         IMPORT  asc: ASCII;
IMPORT  arg: tskArgs;           IMPORT  cod: defCodes;
IMPORT  low: lowLevel;          IMPORT  tty: Terminal;
IMPORT  cdf: defCode;           IMPORT  key: Keyboard;
IMPORT  tim: Time;              IMPORT  env: tskEnv;

WITH STORAGE: mem;

PROCEDURE pusage; FORWARD;

CONST  EXT = "boot";

(* RESERVED LOCATIONS: *)

CONST (*          contence of this location is:       *)
  IPR   = 83h; (* start of ipr run specification list *)
  INIT  = 84h; (* start of init list of the system    *)
  FREE  = 85h; (* first free word after multi globals *)
  UTIL  = 86h; (* start of utility partition          *)
  CODES = 87h; (* start of code file list             *)
  MEMTOP= 88h; (* set by booter                       *)
  BTDEV = 89h; (* set by booter                       *)
  RESSZ = 8Ah; (* RES size                            *)
  PROC0 = 90h; (* pointer to boot process             *)

--------80------90------A0-------------UTIL------ ------------STK0-MG.. --FREE
--      | HEAD  | PROC0 |  s y s t e m |  i p r  |  c o d e s |   |       |
-------------------------------------------------------------------- .. --


TYPE
  STR32   = ARRAY [0..31] OF CHAR;
  ADDRESS = SYSTEM.ADDRESS;
  BUFFER  = DYNARR OF INTEGER;

  list    = POINTER TO node;
  ulist   = POINTER TO unode;

  unode   = RECORD
              next: ulist;
              stk : INTEGER;
              util: STRING;
              args: STRING
            END;

  node    = RECORD
              next: list;
              name: STR32;
              init: BOOLEAN;
              ofs : INTEGER;
              glo : BUFFER;
              cod : cdf.code_ptr;
              csz : INTEGER;
              time: INTEGER;
           END;
  MG     = RECORD  ofs: INTEGER; size: INTEGER END;

  str_ptr= POINTER TO STR32;

CONST info_size = SIZE(cdf.code_rec);

      name_fmt  = "\r%s                                \r";
      erase_fmt = "\r                                  \r";

VAR kernel: list;
      util: list;
     nodes: ulist;
     kinit: INTEGER;  (* in file list *)
      kend: list;

   verbose: BOOLEAN;
    nobufs: INTEGER;
       sys: bio.FILE;
       OFS: INTEGER;
     sname: STR32;
       RES: INTEGER;
       CPU: INTEGER;

CONST
  OPEN    = "open";
  CREATE  = "create";
  BUFFERS = "BUFFERS";
  WRITE   = "write";
  CLOSE   = "close";
  READ    = "read";
  SEEK    = "seek";

PROCEDURE noisy(): BOOLEAN;
BEGIN
  IF env.ipr() THEN verbose:=FALSE END; RETURN verbose
END noisy;

PROCEDURE op_error(erc: INTEGER; VAL op,name: ARRAY OF CHAR);
BEGIN
  IF op#"" THEN std.perror(erc,'%s("%s"): %%s\n',op,name)
  ELSE          std.perror(erc,'"%s": %%s\n',name)
  END;
  HALT(erc);
END op_error;

PROCEDURE chk(VAL op: ARRAY OF CHAR);
BEGIN
  IF NOT bio.done THEN op_error(bio.error,op,sname) END;
END chk;

PROCEDURE first_external(c: cdf.code_ptr; VAR name: str_ptr; VAR time: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  IF c^.vers*{0..7} = {1} THEN
    adr:=ADDRESS(c)+info_size+c^.str_size+c^.code_size+c^.no_mg*2;
    time:=adr^; name:=adr+1;
  ELSE
    name:=ADDRESS(c)+info_size+c^.str_size+c^.code_size; time:=-1
  END;
END first_external;

PROCEDURE next_external(c: cdf.code_ptr; VAR name: str_ptr; VAR time: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  adr:=name;
  WHILE BITSET(adr^<<8)*{0..7}#{} DO adr:=adr+1 END;
  adr:=adr+1;
  IF c^.vers*{0..7} = {1} THEN
    time:=adr^; name:=adr+1
  ELSE
    time:=-1;   name:=adr
  END;
END next_external;

PROCEDURE find(l: list; VAL name: ARRAY OF CHAR): list;
BEGIN
  WHILE (l#NIL) & (l^.name#name) DO l:=l^.next END;
  RETURN l
END find;

TYPE FIND = PROCEDURE (list, ARRAY OF CHAR): list;

PROCEDURE app(VAR to: list; ext: list; system: BOOLEAN; found: FIND); FORWARD;

VAR BIN: bio.PATHs;

PROCEDURE collect(VAR to: list; l: list; system: BOOLEAN; found: FIND);
  VAR i: INTEGER;
     cf: bio.FILE;
    eof: INTEGER;
t,n,ext: list;
  noext: INTEGER;
   name: str_ptr;
  cname: STR32;
   time: INTEGER;
BEGIN
  t:=found(to,l^.name);
  IF t#NIL THEN RETURN END;
  str.print(cname,"%s.cod",l^.name);
  IF noisy() THEN std.print(name_fmt,cname) END;
  bio.lookup(BIN,cf,cname,'r');
  IF NOT bio.done THEN op_error(bio.error,'search',cname) END;
  eof:=bio.eof(cf);
  l^.csz:=(eof+3) DIV 4;
  mem.ALLOCATE(l^.cod,l^.csz);
  low._zero(l^.cod,l^.csz);
  bio.read(cf,l^.cod,eof);
  IF NOT bio.done THEN op_error(bio.error,READ,cname) END;
  bio.get_attr(cf,bio.a_pro,l^.cod^.usercode);
  IF NOT bio.done THEN op_error(bio.error,"get_pro",cname) END;
  IF (l^.time>=0) & (l^.time#l^.cod^.def_time) THEN
    op_error(err.inconsistency,'timecheck',l^.name)
  ELSIF INTEGER(l^.cod^.vers*cdf.cpu_mask<<8)>CPU THEN
    op_error(err.inconsistency,'cpu_check',l^.name)
  END;

  IF system THEN
    NEW(l^.glo,l^.cod^.glo_size+2);
    low.zero(l^.glo);
    --low.fill(l^.glo,0AAAAAAAAh);
    --low.fill(l^.glo,MAX(INTEGER));
    --low.fill(l^.glo,NIL);
    --low.fill(l^.glo,MIN(INTEGER));
    l^.glo[0]:=OFS+2+l^.cod^.no_exts;
    l^.ofs:=OFS; INC(OFS,SIZE(l^.glo));
  ELSE
    l^.glo^.ADR:=NIL; l^.glo^.HIGH:=-1;
  END;

  bio.close(cf);
  IF NOT bio.done THEN op_error(bio.error,'close',cname) END;

  noext:=l^.cod^.no_exts;
  ext:=NIL;
  first_external(l^.cod,name,time);
  FOR i:=1 TO noext-1 DO
    t:=find(l,name^);
    IF t=NIL THEN t:=found(to,name^) END;
    IF t=NIL THEN
      NEW(n);
      n^.next:=ext;  ext:=n;  str.copy(n^.name,name^);  n^.init:=FALSE;
      n^.time:=time
    ELSE
      IF (t^.time>=0) & (t^.time#time) THEN
        std.perror(err.inconsistency,'timecheck("%s.%s"): %%s',l^.name,t^.name);
        HALT(1)
      END
    END;
    next_external(l^.cod,name,time)
  END;
  l^.next:=to;     to:=l;
  app(to,ext,system,found)
END collect;

PROCEDURE app(VAR to: list; ext: list; system: BOOLEAN; found: FIND);
  VAR next: list;
BEGIN
  WHILE ext#NIL DO
    next:=ext^.next;  collect(to,ext,system,found);  ext:=next
  END
END app;

PROCEDURE init(l: list);
  VAR x: list;
  i,G,E: INTEGER;
   time: INTEGER;
   name: str_ptr;
BEGIN
  IF l^.init THEN RETURN END;
  l^.init:=TRUE;
  first_external(l^.cod,name,time);
  G:=2+l^.cod^.no_exts;
  l^.glo[G]:=l^.ofs;
  E:=2;
  FOR i:=1 TO l^.cod^.no_exts-1 DO
    x:=find(kernel,name^);
    ASSERT(x#NIL);
    l^.glo[E]:=x^.ofs; INC(E);
    IF NOT x^.init THEN init(x) END;
    next_external(l^.cod,name,time)
  END;
  l^.glo[E]:=l^.ofs;
  IF kend=NIL THEN kinit:=l^.ofs ELSE kend^.glo[1]:=l^.ofs END;
  l^.glo[1]:=0; kend:=l;
END init;

VAR HEAD: ARRAY [0..9Fh] OF INTEGER;
    TAIL: ARRAY [0..11 ] OF INTEGER;

PROCEDURE reverse(VAR k: list): INTEGER;
  VAR l,n,p: list; i: INTEGER;
BEGIN
  i:=SIZE(HEAD)+RES;
  l:=k; p:=NIL;
  WHILE l#NIL DO
    INC(i,SIZE(l^.glo)+l^.csz);
    n:=l^.next; l^.next:=p; p:=l; l:=n
  END;
  k:=p;
  RETURN i
END reverse;

VAR STK: INTEGER;

PROCEDURE makeinitcode(size: INTEGER);

  PROCEDURE code(SEQ x: INTEGER): INTEGER;
    VAR i: INTEGER;
        w: BITSET;
  BEGIN
    w:={};
    FOR i:=3 TO 0 BY -1 DO w:=BITSET(w<<8)+BITSET(x[i]) END;
    RETURN INTEGER(w)
  END code;

BEGIN
  low.zero(TAIL);
  HEAD[PROC0+0]:=size;     (* G  *)
  HEAD[PROC0+1]:=size+6;   (* L  *)
  HEAD[PROC0+2]:=0;        (* PC *)
  HEAD[PROC0+3]:=0;        (* M  *)
  HEAD[PROC0+4]:=size+11;  (* S  *)
  HEAD[PROC0+5]:=size+STK-SIZE(TAIL); (* H *)

  TAIL[0]:=size+1;         (* F  *)
  TAIL[1]:=code(cod.lib,  084h,     cod.lsw0, cod.copt );
  TAIL[2]:=code(cod.li0,  cod.equ,  cod.jfsc, 01       );
  TAIL[3]:=code(cod.quit, cod.copt, cod.stot, cod.stot );
  TAIL[4]:=code(cod.cf,   cod.drop, cod.lodt, cod.li1  );
  TAIL[5]:=code(cod.add,  cod.jbs,  011h    , cod.invld);
END makeinitcode;

PROCEDURE initsys(main: list);

  PROCEDURE mgalloc(l: list; VAR ofs: INTEGER);
    VAR mg: DYNARR OF MG;
       i,G: INTEGER;
  BEGIN
    mg^.HIGH:=l^.cod^.no_mg-1;
    mg^.ADR :=ADDRESS(l^.cod)+info_size+l^.cod^.str_size+l^.cod^.code_size;
    G:=2+l^.cod^.no_exts;
    FOR i:=0 TO l^.cod^.no_mg-1 DO
      l^.glo[G+mg[i].ofs]:=ofs; INC(ofs,mg[i].size)
    END
  END mgalloc;

  VAR l: list;
   mofs: INTEGER;
   size: INTEGER;

BEGIN
  init(main);
  size:=reverse(kernel);
  low.zero(HEAD);
  HEAD[00h] :=PROC0;
  HEAD[01h] :=PROC0;
  HEAD[INIT]:=kinit;
  makeinitcode(size);
  mofs:=size+STK;
  l:=kernel;
  WHILE l#NIL DO mgalloc(l,mofs); l:=l^.next END;
  HEAD[FREE]:=mofs;

  bio.write(sys,SYSTEM.ADR(HEAD),BYTES(HEAD)); chk(WRITE);
  l:=kernel;
  WHILE l#NIL DO
    l^.glo[2+l^.cod^.no_exts+0]:=OFS+info_size+l^.cod^.str_size;
    l^.glo[2+l^.cod^.no_exts+1]:=OFS+info_size;
    ASSERT(bio.pos(sys)=l^.ofs*4);
    l^.ofs:=OFS; INC(OFS,l^.csz);
    bio.write(sys,l^.glo^.ADR,BYTES(l^.glo)); chk(WRITE);
    DISPOSE(l^.glo);
    l:=l^.next;
  END;
  l:=kernel;
  WHILE l#NIL DO
    ASSERT(bio.pos(sys)=l^.ofs*4);
    bio.write(sys,l^.cod,l^.csz*4); chk(WRITE);
    mem.DEALLOCATE(l^.cod,l^.csz);
    l:=l^.next;
  END;
  ASSERT(bio.pos(sys)=size*4);

  bio.write(sys,SYSTEM.ADR(TAIL),BYTES(TAIL)); chk(WRITE);
END initsys;

PROCEDURE makesys(VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
   main: list;
BEGIN
  str.print(sname,"%s.%s",name,EXT);

  bio.create (sys,sname,"hm",32*1024-1); chk(CREATE);
  bio.buffers(sys,nobufs,4096);          chk(BUFFERS);

  OFS:=SIZE(HEAD);
  IF arg.number("RES",RES) THEN OFS:=OFS+RES ELSE RES:=0  END;
  IF RES<0 THEN std.print("bad RES parameter (less then 0)\n"); HALT(err.bad_parm) END;

  NEW(main);
  main^.next:=NIL;     str.copy(main^.name,name);
  main^.init:=FALSE;   main^.time:=-1;
  app(kernel,main,TRUE,find);
  IF noisy() THEN std.print(erase_fmt) END;

  initsys(main);

  i:=bio.pos(sys);
  bio.cut(sys,i);

  IF NOT bio.done THEN
    std.perror(bio.error,'cut("%s",%d): %%s\n',sname,i); HALT(1)
  END;
  IF noisy() THEN std.print('"%s" %dK+%04d\n',sname,i DIV 1024,i MOD 1024) END;
  IF NOT arg.flag('-','c') THEN
    bio.close(sys); chk(CLOSE)
  END
END makesys;

---------------------------  CONFIG  ---------------------------
                           ----------
PROCEDURE seekw(wp: INTEGER);
BEGIN
  bio.seek(sys,wp*4,0); chk(SEEK)
END seekw;

PROCEDURE readw(adr: INTEGER; VAR data: SYSTEM.WORD);
BEGIN
  seekw(adr);
  bio.read(sys,SYSTEM.ADR(data),4);
  chk(READ)
END readw;

PROCEDURE writew(adr: INTEGER; data: SYSTEM.WORD);
BEGIN
  seekw(adr);
  bio.write(sys,SYSTEM.ADR(data),4);
  chk(WRITE)
END writew;

PROCEDURE mg_correct(G,C,add: INTEGER);
  VAR c: cdf.code_rec;
    i,o: INTEGER;
    a,m: INTEGER;
BEGIN
  seekw(C);
  bio.read(sys,SYSTEM.ADR(c),BYTES(c));
  chk(READ);
  m:=C+info_size+c.str_size+c.code_size;
  FOR i:=0 TO c.no_mg-1 DO
    readw(m+i*2,o);  readw(G+o,a);  INC(a,add);  writew(G+o,a)
  END
END mg_correct;

PROCEDURE prs_correct(add: INTEGER);
BEGIN
  INC(HEAD[FREE],add);

  INC(HEAD[PROC0+0],add);  (* G  *)
  INC(HEAD[PROC0+1],add);  (* L  *)

  INC(HEAD[PROC0+4],add);  (* S  *)
  INC(HEAD[PROC0+5],add);  (* H  *)

  INC(TAIL[0  ],add);  (* F  *)
END prs_correct;

PROCEDURE extract_kernel(add: INTEGER);
  VAR l: list;
      G: INTEGER;
    STR: INTEGER;
   next: INTEGER;
BEGIN
  kernel:=NIL;
  OFS:=HEAD[INIT];
  WHILE OFS#0 DO
    NEW(l);
    l^.cod:=NIL; l^.glo^.ADR:=NIL; l^.glo^.HIGH:=-1;
    l^.ofs:=OFS; l^.next:=kernel; kernel:=l;

    readw(OFS,G);   readw(OFS+1,next);
    readw(G+1,STR);
    readw(STR-15,l^.time); -- deftime
    seekw(STR);
    bio.read(sys,SYSTEM.ADR(l^.name),BYTES(l^.name));
    chk(READ);

    IF add#0 THEN mg_correct(G,STR-info_size,add) END;
    OFS:=next;
  END;
  IF add#0 THEN prs_correct(add) END;
END extract_kernel;

PROCEDURE find_in_kernel(l: list; VAL name: ARRAY OF CHAR): list;
  VAR n: list;
BEGIN
  n:=find(kernel,name);
  IF n#NIL THEN RETURN n END;
  RETURN find(l,name);
END find_in_kernel;

PROCEDURE skip(VAL a: ARRAY OF CHAR; VAR i: INTEGER);
BEGIN
  WHILE (i<=HIGH(a)) & (a[i]=' ') DO INC(i) END
END skip;

PROCEDURE stack(VAL a: ARRAY OF CHAR; VAR i,stk: INTEGER);
  VAR j: INTEGER; done: BOOLEAN;
BEGIN
  IF (i>HIGH(a)) OR (a[i]#'[') THEN stk:=1; RETURN END;
  j:=i+1;
  str.iscan(stk,a,j,done);
  IF NOT done OR (j>HIGH(a)) OR (a[j]#']') THEN
    IF NOT done THEN j:=i END;
    std.print('illegal [stack] specification:\n%s\n%*c*\n',a,i,' ');
    HALT(1)
  ELSIF (stk<0) OR (stk>64) THEN
    std.print('stack out of range: %dKB [1..64]\n',stk); HALT(1)
  END;
  i:=j+1;
  skip(a,i);
END stack;

PROCEDURE take_util_name(VAL a: ARRAY OF CHAR; VAR i: INTEGER;
                         VAR u: ARRAY OF CHAR; VAR j: INTEGER);
BEGIN
  j:=0;
  WHILE (i<=HIGH(a)) & (j<=HIGH(u)) & (a[i]#0c) & (a[i]#' ') & (a[i]#'&') DO
    u[j]:=a[i]; INC(j); INC(i)
  END;
  IF (j>0) & (j<=HIGH(u)) THEN u[j]:=0c; RETURN END;
  std.print('too long or empty name: "%s"\n',a); HALT(1)
END take_util_name;

PROCEDURE append(a: ARRAY OF CHAR);
  VAR u: STR32;
      n: ulist;     main: list;
    stk: INTEGER;  i,j,l: INTEGER;
BEGIN
  i:=0;
  skip(a,i); stack(a,i,stk);
  take_util_name(a,i,u,j);
  skip(a,i);
  str.delete(a,0,i);
  l:=str.len(a);
  IF (l>0) & (a[l-1]='&') THEN
    DEC(l);
    REPEAT DEC(l) UNTIL (l<0) OR (a[l]#' ');
    INC(l); a[l]:=0c;
    NEW(n);            n^.stk:=stk*256;
    NEW(n^.util,j+1);  low.zero(n^.util); str.copy(n^.util,u);
    NEW(n^.args,l+1);  low.zero(n^.args); str.copy(n^.args,a);
    n^.next:=nodes; nodes:=n
  END;

  IF find_in_kernel(util,u)#NIL THEN RETURN END;
  NEW(main);
  main^.next:=NIL; main^.init:=FALSE; main^.time:=-1;
  str.copy(main^.name,u);
  app(util,main,FALSE,find_in_kernel);
  IF noisy() THEN std.print(erase_fmt) END
END append;

PROCEDURE insert_utils;
  VAR i: INTEGER;
      p: INTEGER;
      l:  list;
      u: ulist;
   size: INTEGER;
BEGIN
  IF util=NIL THEN RETURN END;
  size:=0;
  l:=util;
  WHILE l#NIL DO INC(size,l^.csz+1); l:=l^.next END;
  u:=nodes;
  WHILE u#NIL DO
    INC(size,1+SIZE(u^.stk)+SIZE(u^.util)+1+SIZE(u^.args)+1); u:=u^.next
  END;
  i:=HEAD[PROC0];
  bio.extend(sys,(i+size)*4+BYTES(TAIL)); chk("extend");

  WHILE kernel#NIL DO l:=kernel; kernel:=kernel^.next; DISPOSE(l) END;
  extract_kernel(size);

  OFS:=i;
  HEAD[UTIL]:=OFS;
  u:=nodes;
  p:=0;
  WHILE u#NIL DO
    writew(OFS,p);                 p:=OFS;        INC(OFS);
    writew(OFS,u^.stk);                           INC(OFS);
    writew(OFS,HIGH(u^.util));                    INC(OFS);
    bio.write(sys,u^.util^.ADR,SIZE(u^.util)*4);  INC(OFS,SIZE(u^.util));
    chk(WRITE);
    writew(OFS,HIGH(u^.args));                    INC(OFS);
    bio.write(sys,u^.args^.ADR,SIZE(u^.args)*4);  INC(OFS,SIZE(u^.args));
    chk(WRITE);
    u:=u^.next
  END;
  HEAD[IPR]:=p;

  IF noisy() THEN
    i:=HEAD[PROC0]*4+BYTES(TAIL);
    std.print('"%s" %dK+%04d\n',sname,i DIV 1024,i MOD 1024)
  END;
  l:=util; p:=0;
  WHILE l#NIL DO
    writew(OFS,p); p:=OFS;
    bio.write(sys,l^.cod,l^.csz*4); chk(WRITE);
    INC(OFS,l^.csz+1);
    mem.DEALLOCATE(l^.cod,l^.csz);
    l:=l^.next
  END;
  IF noisy() THEN std.print(erase_fmt) END;
  HEAD[CODES]:=p;
  ASSERT(OFS=HEAD[PROC0]);
END insert_utils;

PROCEDURE copy_sys;
  VAR f: bio.FILE;
    i,l: INTEGER;
    buf: STRING;
    eof: INTEGER;
BEGIN
  bio.open(f,sname,"r"); chk(OPEN);
  bio.buffers(f,nobufs,4096);          chk(BUFFERS);

  bio.read(f,SYSTEM.ADR(HEAD),BYTES(HEAD)); chk(READ);
  bio.seek(f,HEAD[PROC0]*4,0); chk(SEEK);
  bio.read(f,SYSTEM.ADR(TAIL),BYTES(TAIL)); chk(READ);

  IF HEAD[UTIL]=0 THEN eof:=HEAD[PROC0]*4 ELSE eof:=HEAD[UTIL]*4 END;
  IF eof>32*1024 THEN l:=32*1024 ELSE l:=eof END;

  bio.seek(f,0,0); chk(SEEK);
  NEW(buf,l);
  bio.create (sys,sname,"hm",eof); chk(CREATE);
  bio.buffers(sys,nobufs,4096);    chk(BUFFERS);
  WHILE eof>0 DO
    IF eof>BYTES(buf) THEN l:=BYTES(buf) ELSE l:=eof END;
    bio.read(f,buf^.ADR,l);    chk(READ);
    bio.write(sys,buf^.ADR,l); chk(WRITE);
    DEC(eof,l);
  END;
  DISPOSE(buf);
  bio.close(f); chk(CLOSE);
  seekw(0);
END copy_sys;

PROCEDURE insert(a: ARRAY OF CHAR);
  VAR u: STR32;
      n: ulist;     main: list;
    stk: INTEGER;  i,j,l: INTEGER;
BEGIN
  l:=str.len(a);
  NEW(n);            n^.stk:=0;
  NEW(n^.util,l+1);  low.zero(n^.util);  str.copy(n^.util,a);
  NEW(n^.args,1);    low.zero(n^.args);
  n^.next:=nodes; nodes:=n
END insert;

PROCEDURE codes_and_iprs(VAL name: ARRAY OF CHAR);
  VAR f: bio.FILE;
    buf: STRING;
    one: STRING;
  i,j,l: INTEGER;
  cname: STR32;
BEGIN
  str.print(cname,"%s.cnf",name);
  bio.open(f,cname,"r");
  IF NOT bio.done THEN op_error(bio.error,OPEN,cname) END;
  NEW(buf,bio.eof(f));
  bio.read(f,buf^.ADR,bio.eof(f));
  IF NOT bio.done THEN op_error(bio.error,READ,cname) END;
  bio.close(f);
  IF NOT bio.done THEN op_error(bio.error,CLOSE,cname) END;
  NEW(one,0);
  i:=0;
  REPEAT
    j:=i;
    WHILE (j<=HIGH(buf)) & (buf[j]#asc.NL) DO INC(j) END;
    l:=j-i+1;
    RESIZE(one,l);
    low.cmove(one^.ADR,0,buf^.ADR,i,l);
    one[l-1]:=0c;
    IF one#"" THEN
      IF noisy() THEN std.print("%s> %s\n",cname,one) END;
      IF one[0]#'$' THEN append(one) ELSE insert(one) END
    END;
    i:=j+1
  UNTIL i>HIGH(buf);
  DISPOSE(one);
  DISPOSE(buf);
END codes_and_iprs;

PROCEDURE configsys(VAL name,cname: ARRAY OF CHAR; app: BOOLEAN);
  VAR i: INTEGER;      uname: STRING;
BEGIN
  str.print(sname,"%s.%s",name,EXT);
  IF NOT arg.flag('-','m') THEN copy_sys END;

  IF HEAD[UTIL]#0 THEN i:=HEAD[UTIL]-HEAD[PROC0] ELSE i:=0 END;
  extract_kernel(i);
  util :=NIL;
  nodes:=NIL;
  HEAD[UTIL ]:=0;
  HEAD[IPR  ]:=0;
  HEAD[CODES]:=0;
  IF app THEN
    codes_and_iprs(cname);
    FOR i:=1 TO HIGH(arg.words) DO
      IF noisy() THEN std.print(". . . .cnf> %s\n",arg.words[i]) END;
      append(arg.words[i])
    END;
  END;
  insert_utils;

  seekw(0);
  bio.write(sys,SYSTEM.ADR(HEAD),BYTES(HEAD)); chk(WRITE);

  seekw(HEAD[PROC0]);
  bio.write(sys,SYSTEM.ADR(TAIL),BYTES(TAIL)); chk(WRITE);

  bio.cut(sys,HEAD[90h]*4+BYTES(TAIL)); chk("cut");
  bio.close(sys); chk(CLOSE);
END configsys;

PROCEDURE listsys(VAL name: ARRAY OF CHAR);

  VAR mnm: STR32;
      crc: cdf.code_rec;

  PROCEDURE print;
    VAR d,mn,y,h,m,s: INTEGER;
  BEGIN
    tim.unpack(crc.def_time,y,mn,d,h,m,s);
    std.print('d%02d%02d%02d %2d:%02d.%02d  ',d,mn,y-1900,h,m,s);
    tim.unpack(crc.imp_time,y,mn,d,h,m,s);
    std.print('i%02d%02d%02d %2d:%02d.%02d   ',d,mn,y-1900,h,m,s);
    std.print('"%s"\n',mnm)
  END print;

  VAR i,x: INTEGER;
      g,s: INTEGER;
      u,a: STRING;

BEGIN
  str.print  (sname,"%s.%s",name,EXT);
  bio.open   (sys,sname,'r');       chk(OPEN);
  bio.buffers(sys,nobufs,4096);     chk(BUFFERS);

  i:=INIT; readw(i,i);
  std.print("%|48s\n",sname);
  std.print("%|48s\n","*** KERNEL ***");
  WHILE i#0 DO
    readw(i,g);   readw(i+1,i);
    readw(g+1,s);
    seekw(s-cdf.info_size);
    bio.read(sys,SYSTEM.ADR(crc),BYTES(crc)); chk(READ);
    seekw(s);
    bio.read(sys,SYSTEM.ADR(mnm),BYTES(mnm)); chk(READ);
    print
  END;
  std.print("\n");

  std.print("%|48s\n","*** IPRs ***");
  NEW(u,0);
  NEW(a,0);
  i:=IPR; readw(i,i);
  WHILE i#0 DO
    readw(i,s); INC(i);
    readw(i,g); INC(i);
    readw(i,x); INC(i); RESIZE(u,x+1);
    bio.read(sys,u^.ADR,SIZE(u)*4); chk(READ); INC(i,SIZE(u));
    readw(i,x); INC(i); RESIZE(a,x+1);
    bio.read(sys,a^.ADR,SIZE(a)*4); chk(READ); INC(i,SIZE(a));
    g:=g*4;
    std.print('%s -} "[%2dK+%04d]%s %s &"\n',sname,g DIV 1024,g MOD 1024,u,a);
    i:=s;
  END;
  DISPOSE(a);
  DISPOSE(u);

  std.print("%|48s\n","*** CODEs ***");
  i:=CODES; readw(i,i);
  WHILE i#0 DO
    readw(i,i);
    bio.read(sys,SYSTEM.ADR(crc),BYTES(crc)); chk(READ);
    bio.read(sys,SYSTEM.ADR(mnm),BYTES(mnm)); chk(READ);
    print
  END;
  std.print("\n");
END listsys;

----------------------  MAKE SYSTEM.BOOT -----------------------
                      -------------------

PROCEDURE query;
  VAR ch: CHAR;
BEGIN
  REPEAT key.read(ch) UNTIL (CAP(ch)='Y') OR (CAP(ch)='N');
  tty.print("%c\n",ch);
  IF CAP(ch)#'Y' THEN HALT END
END query;

PROCEDURE mkboot(VAL where,name: ARRAY OF CHAR);
  VAR i,l: INTEGER;
      buf: STRING;
      eof: INTEGER;
      boo: bio.FILE;
    sboot: ARRAY [0..255] OF CHAR;
BEGIN
  str.print(sname,"%s.%s",name,EXT);
  bio.open(sys,sname,"r"); chk(OPEN);
  i:=str.len(where)-1;
  IF i<0 THEN pusage; HALT END;
  IF where[i]='/' THEN
    str.print(sboot,"%sSYSTEM.BOOT",where);
  ELSE
    str.print(sboot,"%s/SYSTEM.BOOT",where);
  END;
  bio.open(boo,sboot,"w");
  IF NOT bio.done THEN op_error(bio.error,OPEN,sboot) END;
  bio.get_attr(boo,bio.a_inode,i);
  IF NOT bio.done THEN op_error(bio.error,"get_attrs",sboot) END;
  IF i#1 THEN
    std.print('none boot file "%s" inode#1\n',sboot); HALT(1)
  END;
  eof:=bio.eof(sys);
  IF eof>64*1024 THEN l:=64*1024 ELSE l:=eof END;
  tty.print('copy bootable system to "%s"? ',sboot);
  query;
  NEW(buf,l);
  WHILE eof>0 DO
    IF eof>BYTES(buf) THEN l:=BYTES(buf) ELSE l:=eof END;
    bio.read (sys,buf^.ADR,l); chk(READ);
    bio.write(boo,buf^.ADR,l);
    IF NOT bio.done THEN op_error(bio.error,WRITE,sboot) END;
    DEC(eof,l)
  END;
  DISPOSE(buf);
  bio.cut(boo,bio.pos(boo));
  IF NOT bio.done THEN op_error(bio.error,'cut',sboot) END;
  bio.close(boo);
  IF NOT bio.done THEN op_error(bio.error,CLOSE,sboot) END
END mkboot;

PROCEDURE mkcold(VAL dev,name: ARRAY OF CHAR);
  VAR i,l: INTEGER;
      buf: STRING;
      lab: ARRAY [0..15] OF CHAR;
      eof: INTEGER;
      boo: bio.FILE;
BEGIN
  str.print(sname,"%s.%s",name,EXT);
  bio.open(sys,sname,"r"); chk(OPEN);
  bio.open(boo,dev,"rw");
  IF NOT bio.done THEN op_error(bio.error,OPEN,dev) END;
  IF bio.is_disk*bio.kind(boo)={} THEN
    std.print('file "%s" is not legal device\n',dev); HALT(1)
  END;
  eof:=bio.eof(sys);
  IF eof>4*1024 THEN
    std.print('file "%s" longer then 4KB\n',sname); HALT(1)
  END;
  NEW(buf,4096);
  tty.print('copy cold booter "%s" to device "%s"? ',sname,dev);
  query;
  low.zero(buf);
  bio.read (sys,buf^.ADR,eof);     chk(READ);
  bio.seek (boo,8,0);
  IF NOT bio.done THEN op_error(bio.error,SEEK,dev) END;
  bio.get  (boo,lab,BYTES(lab));
  IF NOT bio.done THEN op_error(bio.error,READ,dev) END;
  bio.seek (boo,0,0);
  IF NOT bio.done THEN op_error(bio.error,SEEK,dev) END;
  low.move(buf^.ADR+2,SYSTEM.ADR(lab),SIZE(lab));
  bio.write(boo,buf^.ADR,4096);
  IF NOT bio.done THEN op_error(bio.error,WRITE,dev) END;
  DISPOSE(buf);
  bio.close(boo);
  IF NOT bio.done THEN op_error(bio.error,CLOSE,dev) END;
END mkcold;

PROCEDURE mkice(VAL dev,name: ARRAY OF CHAR);

  VAR i,l: INTEGER;
      buf: STRING;
      lab: ARRAY [0..15] OF CHAR;
      eof: INTEGER;
      boo: bio.FILE;
      pos: INTEGER;
     boot: DYNARR OF INTEGER;
    bname: ARRAY [0..127] OF CHAR;

  PROCEDURE putw(n: INTEGER);
  BEGIN
    IF pos>HIGH(buf) THEN
      std.print('ice booter "%s" longer then 4KB\n',name); HALT(err.too_large)
    END;
    boot[pos]:=n; INC(pos)
  END putw;

  PROCEDURE icelink;
    VAR i: INTEGER;
        a: ADDRESS;
        p: cdf.code_ptr;
       pc: POINTER TO INTEGER;
      g,l: INTEGER;
  BEGIN
    p:=SYSTEM.ADR(buf); pos:=0;
    i:=p^.str_size+p^.code_size;
    g:=i+9+2+4;
    l:=g+p^.glo_size-p^.no_exts;
    pc:=ADDRESS(p)+cdf.info_size+p^.str_size;
    putw(6);
    putw(6);
    putw(0); putw(0); putw(0); putw(0); (* LABEL *)
    putw(g);
    putw(l);
    putw(pc^);
    putw(0);
    putw(l+5);
    putw(0FFFFh);
    putw(0);
    a:=ADDRESS(p)+cdf.info_size;
    WHILE i>0 DO putw(a^); INC(a); DEC(i) END;
    putw(g);
    putw(g-2);
    putw(9+4+p^.str_size);
    putw(9+4);
    FOR i:=2 TO p^.glo_size-p^.no_exts-1 DO putw(0) END;
    IF (p^.no_exts>2) OR (p^.no_mg>0) THEN
      tty.print('WARNING! No global data or import in ICE booter!\n\n');
    END;
    FOR i:=0 TO 4 DO putw(0) END;
    RESIZE(boot,pos);
    std.print('ice booter "%s.boot" %d,%03d bytes\n',name,BYTES(boot) DIV 1000,
                                                          BYTES(boot) MOD 1000);
  END icelink;

BEGIN
  str.print(sname,"%s.%s",name,"cod");
  bio.open(sys,sname,"r"); chk(OPEN);

  IF dev#"" THEN
    bio.open(boo,dev,"rw");
    IF NOT bio.done THEN op_error(bio.error,OPEN,dev) END;
    IF bio.is_disk*bio.kind(boo)={} THEN
      std.print('file "%s" is not legal device\n',dev); HALT(1)
    END;
    str.print(bname,"%s",dev);
  ELSE
    str.print(bname,"%s.%s",name,EXT);
    bio.create(boo,bname,"w",1);         chk(CREATE);
  END;
  eof:=bio.eof(sys);
  IF eof>4*1024 THEN
    std.print('file "%s" longer then 4KB\n'); HALT(err.too_large)
  END;
  NEW(buf,eof);
  bio.get(sys,buf,eof);           chk(READ);
  NEW(boot,4096);
  icelink;
  IF dev#"" THEN
    tty.print('copy ice booter "%s.boot" to device "%s"? ',name,dev);
    query;
    bio.seek(boo,8,0);
    IF NOT bio.done THEN op_error(bio.error,SEEK,dev) END;
    bio.get (boo,lab,BYTES(lab));
    IF NOT bio.done THEN op_error(bio.error,READ,dev) END;
    bio.seek(boo,0,0);
    IF NOT bio.done THEN op_error(bio.error,SEEK,dev) END;
    low.move(boot^.ADR+2,SYSTEM.ADR(lab),SIZE(lab))
  END;
  bio.put(boo,boot,BYTES(boot));
  IF NOT bio.done THEN op_error(bio.error,WRITE,bname) END;
  DISPOSE(buf);
  DISPOSE(boot);
  bio.close(boo);
  IF NOT bio.done THEN op_error(bio.error,CLOSE,bname) END
END mkice;

PROCEDURE pusage;
BEGIN
  std.print('  "config"  system configuration utility program   (c) KRONOS\n');
  std.print("usage:\n");
  std.print("   config -m [-cv] system_name {codes} [cpu=N] [stack=N] [RES=N]\n");
  std.print("   config -c [-v]  system_name {codes} [cpu=N]\n");
  std.print("   config -r [-v]  system_name\n");
  std.print("   config -l       system_name\n");
  std.print("\n");
  std.print("   config -b  boot_device_root_directory system_name\n");
  std.print("   config -B  boot_device  cold_booter_name\n");
  std.print("   config -I [boot_device] ice_booter_name\n");
  std.print("\n");
  std.print("   -c configurate system.cnf file with following syntax:\n");
  std.print('     { [ "["stk"]" ] util [ args ] [ "&" ] }\n\n');
  std.print("   cpu - version of instruction set (0,1,2)\n\n");
  std.print("   RES - reserves N words from address 0A0h\n");
  std.print("                                           Leopold, Mar 06 91\n")
END pusage;

VAR cname: STRING;

BEGIN
  (* If you want to change flag name, do it globaly! *)
  IF (HIGH(arg.words)<0) OR arg.flag('-','h') THEN pusage; HALT END;
  IF NOT arg.number('stack',STK) THEN STK:=2*256 END;
  IF NOT arg.number('cpu',CPU) THEN CPU:=low.cpu_vers END;
  verbose:=NOT arg.flag('-','v');
  kernel:=NIL;
  kinit :=0;
  kend  :=NIL;
  nobufs:=4;
  IF     arg.flag('-','0')   THEN nobufs :=0     END;
  IF NOT std.is_tty(std.out) THEN verbose:=FALSE END;

  bio.get_paths(BIN,env.bin);
  IF NOT bio.done THEN
    std.perror(bio.error,
              'get_paths("%s"): %%s; "." will be used instead it\n'
              ,env.bin);
    BIN:=bio.here
  END;
  IF arg.flag('-','b') THEN
    IF HIGH(arg.words)<1 THEN pusage
    ELSE mkboot(arg.words[0],arg.words[1])
    END;
    HALT
  ELSIF arg.flag('-','B') THEN
    IF HIGH(arg.words)<1 THEN pusage
    ELSE mkcold(arg.words[0],arg.words[1])
    END;
    HALT
  ELSIF arg.flag('-','I') THEN
    IF HIGH(arg.words)<0 THEN pusage
    ELSIF HIGH(arg.words)<1 THEN mkice("",          arg.words[0])
    ELSE                         mkice(arg.words[0],arg.words[1])
    END;
    HALT
  END;

  IF    arg.flag('-','m') THEN   makesys(arg.words[0]) END;

  IF    arg.flag('-','c') THEN
    IF arg.string('cnf',cname) THEN
      configsys(arg.words[0],cname,TRUE)
    ELSE
      configsys(arg.words[0],arg.words[0],TRUE)
    END;
  ELSIF arg.flag('-','r') THEN configsys(arg.words[0],'',FALSE)
  ELSIF arg.flag('-','l') THEN
    listsys(arg.words[0])
  ELSIF arg.flag('-','h') THEN
    pusage
  END
END config.
