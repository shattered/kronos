MODULE config; (* Leo   01-Dec-89. (c) KRONOS *) IMPORT  SYSTEM;
               (* Hady. 28-Aug-91. (c) KRONOS *)

(*$U+*)

IMPORT  bio: BIO;               IMPORT  mem: Heap;
IMPORT  str: Strings;           IMPORT  std: StdIO;
IMPORT  err: defErrors;         IMPORT  asc: ASCII;
IMPORT  arg: tskArgs;           IMPORT  cod: defCodes;
IMPORT  low: lowLevel;          IMPORT  tty: Terminal;
IMPORT  cdf: defCode;           IMPORT  key: Keyboard;
IMPORT  tim: Time;              IMPORT  env: tskEnv;
IMPORT  sle: strEditor;         IMPORT  lex: Lexicon;

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
  UTAIL = 8Bh; (* tile of utils list (Hady)           *)
  ATIME = 8Ch; (* assembling time of kernel           *)
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
              args: STRING;
              wait: BOOLEAN
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
              link: INTEGER
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
  IF t#NIL THEN INC(t^.link); RETURN END;
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
      n^.time:=time; n^.link:=1
    ELSE
      IF (t^.time>=0) & (t^.time#time) THEN
        std.perror(err.inconsistency,'timecheck("%s.%s"): %%s',l^.name,t^.name);
        HALT(1)
      END;
      INC(t^.link)
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
    i:=i+SIZE(l^.glo)+l^.csz;
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
  HEAD[00h]  :=PROC0;
  HEAD[01h]  :=PROC0;
  HEAD[INIT] :=kinit;
  HEAD[ATIME]:=tim.time();
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
    l^.ofs:=OFS;  INC(OFS,l^.csz);
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

  bio.write(sys,SYSTEM.ADR(TAIL),BYTES(TAIL)); chk(WRITE)
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
  IF RES<0 THEN
    std.print("bad RES parameter (less then 0)\n"); HALT(err.bad_parm)
  END;

  NEW(main);
  main^.next:=NIL;     str.copy(main^.name,name);
  main^.init:=FALSE;   main^.time:=-1; main^.link:=0;
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
  bio.read(sys,SYSTEM.ADR(c),BYTES(c)); chk(READ);
  m:=C+info_size+c.str_size+c.code_size;
  FOR i:=0 TO c.no_mg-1 DO
    readw(m+i*2,o);  readw(G+o,a);  a:=a+add;  writew(G+o,a)
  END
END mg_correct;

PROCEDURE prs_correct(add: INTEGER);
BEGIN
  HEAD[FREE]:=HEAD[FREE]+add;

  HEAD[PROC0+0]:=HEAD[PROC0+0]+add;  (* G  *)
  HEAD[PROC0+1]:=HEAD[PROC0+1]+add;  (* L  *)

  HEAD[PROC0+4]:=HEAD[PROC0+4]+add;  (* S  *)
  HEAD[PROC0+5]:=HEAD[PROC0+5]+add;  (* H  *)

  TAIL[0  ]:=TAIL[0]+add;            (* F  *)
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
    l^.ofs:=OFS; l^.next:=kernel ; kernel:=l;

    readw(OFS,G);   readw(OFS+1,next);
    readw(G+1,STR);
    readw(STR-15,l^.time); -- deftime
    seekw(STR);
    bio.read(sys,SYSTEM.ADR(l^.name),BYTES(l^.name));
    chk(READ);

    IF add#0 THEN mg_correct(G,STR-info_size,add) END;
    OFS:=next
  END;
  IF add#0 THEN prs_correct(add) END;
END extract_kernel;

PROCEDURE find_in_kernel(l: list; VAL name: ARRAY OF CHAR): list;
  VAR n: list;
BEGIN
  n:=find(kernel,name);
  IF n#NIL THEN RETURN n END;
  RETURN find(l,name)
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

PROCEDURE _append(u: ARRAY OF CHAR);
  VAR main: list;
BEGIN
  IF find_in_kernel(util,u)#NIL THEN RETURN END;
  NEW(main);
  main^.next:=NIL; main^.init:=FALSE; main^.time:=-1;
  str.copy(main^.name,u); main^.link:=0;
  app(util,main,FALSE,find_in_kernel)
END _append;

PROCEDURE append(a: ARRAY OF CHAR);
  VAR u: STR32;      ipr: BOOLEAN;
      n: ulist;     main: list;
    stk: INTEGER;  i,j,l: INTEGER;
BEGIN
  i:=0;
  skip(a,i); stack(a,i,stk);
  take_util_name(a,i,u,j);
  skip(a,i);
  str.delete(a,0,i);
  l:=str.len(a);
  ipr:=(l>0) & (a[l-1]='&');
  IF ipr THEN DEC(l) END;
  WHILE (l>0) & (a[l]=" ") DO a[l]:=0c; DEC(l) END;

  NEW(n);            n^.stk:=stk*256;
  NEW(n^.util,j+1);  low.zero(n^.util);  str.copy(n^.util,u);
  NEW(n^.args,l+1);  low.zero(n^.args);  str.copy(n^.args,a);
  n^.wait:=NOT ipr;  n^.next:=nodes;     nodes:=n;

  _append(u);
  main:=find(util,u); ASSERT(main#NIL);
  INC(main^.link);
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
  WHILE l#NIL DO size:=size+l^.csz+1+1; l:=l^.next END;
  u:=nodes;
  WHILE u#NIL DO
    size:=size+1+SIZE(u^.stk)+SIZE(u^.util)+1+SIZE(u^.args)+1+SIZE(u^.wait);
    u:=u^.next
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
    writew(OFS,u^.wait);                          INC(OFS);
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
    bio.write(sys,l^.cod,l^.csz*4); chk(WRITE); INC(OFS,l^.csz+1);
    writew(OFS,l^.link);                        INC(OFS);
    mem.DEALLOCATE(l^.cod,l^.csz);
    l:=l^.next
  END;
  IF noisy() THEN std.print(erase_fmt) END;
  HEAD[CODES]:=p;
  HEAD[UTAIL]:=OFS;
  ASSERT(OFS=HEAD[PROC0]);
END insert_utils;

PROCEDURE copy_sys(unpack: BOOLEAN);
  VAR
      f: bio.FILE;
    i,l: INTEGER;
    s,x: INTEGER;
    buf: STRING;
    eof: INTEGER;
    ofs: INTEGER;

      n: ulist;
   main:  list;
    crc: cdf.code_rec;

BEGIN
  bio.open(sys,sname,"r"); chk(OPEN);
  bio.buffers(sys,nobufs,4096);            chk(BUFFERS);

  bio.read(sys,SYSTEM.ADR(HEAD),BYTES(HEAD)); chk(READ);
  bio.seek(sys,HEAD[PROC0]*4,0);              chk(SEEK);
  bio.read(sys,SYSTEM.ADR(TAIL),BYTES(TAIL)); chk(READ);

  IF HEAD[UTIL]=0 THEN eof:=HEAD[PROC0]*4 ELSE eof:=HEAD[UTIL]*4 END;
  IF eof>32*1024  THEN l:=32*1024         ELSE l:=eof            END;
  bio.seek(sys,0,0); chk(SEEK);
  NEW(buf,l);
  bio.create (f,sname,"hm",eof); chk(CREATE );
  bio.buffers(f,nobufs,4096)   ; chk(BUFFERS);
  WHILE eof>0 DO
    IF eof>BYTES(buf) THEN l:=BYTES(buf) ELSE l:=eof END;
    bio.read(sys,buf^.ADR,l); chk(READ );
    bio.write(f ,buf^.ADR,l); chk(WRITE);
    DEC(eof,l)
  END;
  DISPOSE(buf);
  IF NOT unpack THEN
    bio.close(sys); chk(CLOSE);
    sys:=f; seekw(0); RETURN
  END;

  IF HEAD[UTIL]#0 THEN ofs:=HEAD[UTIL]-HEAD[PROC0] ELSE ofs:=0 END;

  nodes:=NIL;
  i:=IPR; readw(i,i);
  WHILE i#0 DO
    NEW(n);
    readw(i,s);      INC(i);
    readw(i,n^.stk); INC(i);
    readw(i,x);      INC(i); NEW(n^.util,x+1);
    bio.read(sys,n^.util^.ADR,SIZE(n^.util)*4); chk(READ); i:=i+SIZE(n^.util);
    readw(i,x);      INC(i); NEW(n^.args,x+1);
    bio.read(sys,n^.args^.ADR,SIZE(n^.args)*4); chk(READ); i:=i+SIZE(n^.args);
    readw(i,n^.wait); INC(i);
    n^.next:=nodes; nodes:=n;
    i:=s
  END;

  util:=NIL;
  i:=CODES; readw(i,i);
  l:=UTAIL; readw(l,l);
  WHILE i#0 DO
    readw(i,s);
    bio.read(sys,SYSTEM.ADR(crc),BYTES(crc)); chk(READ);

    NEW(main);
    bio.read(sys,SYSTEM.ADR(main^.name),BYTES(main^.name)); chk(READ);
    main^.csz:=l-i-2;
    seekw(i+1);
    mem.ALLOCATE(main^.cod,main^.csz);
    bio.read(sys,main^.cod,main^.csz*4);    chk(READ);
    bio.read(sys,SYSTEM.ADR(main^.link),4); chk(READ);

    NEW(main^.glo); main^.init:=FALSE; main^.ofs:=0;
    main^.time:=crc.def_time;
    main^.next:=util; util:=main;

--    IF noisy() THEN std.print("%s\r",main^.name) END;
    l:=i; i:=s
  END;
--  IF noisy() THEN std.print(erase_fmt) END;

  bio.close(sys); chk(CLOSE);
  sys:=f;
  extract_kernel(ofs);

END copy_sys;

PROCEDURE first_word_neq(VAL s0,s1: ARRAY OF CHAR): BOOLEAN;
  VAR i,j: INTEGER; c0,c1: CHAR;
BEGIN
  i:=0; str.skip(s0,i," ");
  j:=0; str.skip(s1,j," ");
  IF i<=HIGH(s0) THEN c0:=s0[i] ELSE c0:=0c END;
  IF j<=HIGH(s1) THEN c1:=s1[j] ELSE c1:=0c END;
  WHILE (c0=c1) & (c0#0c) DO
    INC(i); IF i>HIGH(s0) THEN c0:=0c ELSE c0:=s0[i] END;
    INC(j); IF j>HIGH(s1) THEN c1:=0c ELSE c1:=s1[j] END;
    IF c0=" " THEN c0:=0c END; IF c1=" " THEN c1:=0c END;
  END;
  RETURN c0#c1
END first_word_neq;

PROCEDURE insert(a: ARRAY OF CHAR);
  VAR u: STR32;
      n: ulist;     main: list;
    stk: INTEGER;  i,j,l: INTEGER;
BEGIN
  l:=str.len(a);
  NEW(n);            n^.stk:=0;          n^.wait:=TRUE;
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
  IF NOT arg.flag('-','m') THEN copy_sys(FALSE) END;

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
      _append(arg.words[i])
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
     link: INTEGER;

  PROCEDURE print;
    VAR d,mn,y,h,m,s: INTEGER;
  BEGIN
    tim.unpack(crc.def_time,y,mn,d,h,m,s);
    std.print('d%02d%02d%02d %2d:%02d.%02d  ',d,mn,y-1900,h,m,s);
    tim.unpack(crc.imp_time,y,mn,d,h,m,s);
    std.print('i%02d%02d%02d %2d:%02d.%02d  ',d,mn,y-1900,h,m,s);
    std.print('%03d "%s"\n',link,mnm)
  END print;

  PROCEDURE kernel_head(time: INTEGER);
    VAR y,m,d,h,mi,se: INTEGER;
        buf: ARRAY [0..47] OF CHAR;
  BEGIN
    tim.unpack(time, y,m,d,h,mi,se); y:=y MOD 100;
    str.print(buf,'*** KERNEL %02d/%02d/%02d %2d:%02d.%02d ***',
              d,m,y,h,mi,se);
    std.print('%|48s\n',buf);
  END kernel_head;

  CONST ipr_ch = ARRAY OF CHAR { "&", " " };

  VAR i,x,t: INTEGER;
      g,s: INTEGER;
      u,a: STRING;

BEGIN
  str.print  (sname,"%s.%s",name,EXT);
  bio.open   (sys,sname,'r');       chk(OPEN);
  bio.buffers(sys,nobufs,4096);     chk(BUFFERS);

  std.print("%|48s\n",sname);
  i:=ATIME; readw(i,i);
  kernel_head(i);
--  std.print("%|48s\n","*** KERNEL ***");
  i:=INIT ; readw(i,i);
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
  i:=IPR  ; readw(i,i);
  WHILE i#0 DO
    readw(i,s); INC(i);
    readw(i,g); INC(i);
    readw(i,x); INC(i); RESIZE(u,x+1);
    bio.read(sys,u^.ADR,SIZE(u)*4); chk(READ); INC(i,SIZE(u));
    readw(i,x); INC(i); RESIZE(a,x+1);
    bio.read(sys,a^.ADR,SIZE(a)*4); chk(READ); INC(i,SIZE(a));
    readw(i,x); INC(i);
    g:=g*4;
    std.print('%s -} "[%2dK+%04d]%s %s%c"\n',sname,g DIV 1024,g MOD 1024,u,a,ipr_ch[x]);
    i:=s;
  END;
  DISPOSE(a);
  DISPOSE(u);

  std.print("%|48s\n","*** CODEs ***");
  i:=CODES; readw(i,i);
  t:=UTAIL; readw(t,t);
  WHILE i#0 DO
    seekw(i+1);
    bio.read(sys,SYSTEM.ADR(crc),BYTES(crc)); chk(READ);
    bio.read(sys,SYSTEM.ADR(mnm),BYTES(mnm)); chk(READ);
    readw(t-1, link);
    print;
    t:=i; readw(i,i)
  END;
  std.print("\n");
END listsys;


---------------------- EDIT CONFIGURATION ----------------------
                      --------------------

CONST ue_cols = 3;

TYPE ue_str_ptr = POINTER TO ue_str_rec;
     ue_str_rec = RECORD
                    fwd: ue_str_ptr;
                    bck: ue_str_ptr;
                    val: ARRAY [0..ue_cols-1] OF list;
                  END;

VAR nil_list: list;

PROCEDURE new_nil_list;
BEGIN
  NEW(nil_list);
  WITH nil_list^ DO
    name:=""; next:=NIL; cod:=NIL; time:=-1; link:=1
  END;
END new_nil_list;

PROCEDURE dis_ue(VAR e: ue_str_ptr);
  VAR t: ue_str_ptr;
BEGIN
  WHILE e#NIL DO
    t:=e; e:=e^.fwd; DISPOSE(t)
  END
END dis_ue;

PROCEDURE repack_codes(VAR e: ue_str_ptr);

  VAR tail: ue_str_ptr;

  PROCEDURE new(VAR t: ue_str_ptr);
  BEGIN
    NEW(t);
    t^.fwd:=NIL;
    t^.bck:=NIL;
    low.fill(t^.val,NIL);
  END new;

  PROCEDURE tie(t: ue_str_ptr);
  BEGIN
    IF e=NIL THEN
      t^.fwd:=t; t^.bck:=t; e:=t
    ELSE
      t^.bck:=e^.bck;
      t^.bck^.fwd:=t;
      t^.fwd:=e;
      e^.bck:=t
    END
  END tie;

  VAR l: list;
      t: ue_str_ptr;
      i: INTEGER;

  PROCEDURE link(l: list);
  BEGIN
    IF i>=ue_cols THEN
      new(t); tie(t); i:=0
    END;
    t^.val[i]:=l; INC(i)
  END link;

BEGIN
  dis_ue(e);
--  IF util=NIL THEN RETURN END;
  t:=NIL; i:=ue_cols;
  l:=util;
  WHILE l#NIL DO
    link(l); l:=l^.next
  END;
  link(nil_list);
  WHILE i<ue_cols DO link(nil_list) END;
  e^.bck^.fwd:=NIL; e^.bck:=NIL
END repack_codes;

VAR (* util list frame parameters *)
  u_lucx: INTEGER;
  u_lucy: INTEGER;
  u_cols: INTEGER;
  u_cwdh: INTEGER;

VAR (* configuration editor parameter *)
  c_lucx: INTEGER;
  c_lucy: INTEGER;
  c_stkx: INTEGER;
  c_utlx: INTEGER;
  c_iprx: INTEGER;
  c_utlw: INTEGER;

VAR (* global state of screen *)
   lines: INTEGER;
messline: INTEGER;

PROCEDURE message(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR ch: CHAR;
BEGIN
  tty.set_pos(messline,0); tty.erase_line(0);
  tty.print(fmt,args);
  tty.set_pos(messline,tty.state^.columns-10);
  tty.print('HIT KEY ');
  key.read(ch);
  tty.set_pos(messline,0); tty.erase_line(0)
END message;

PROCEDURE prepare_terminal;
  VAR l,i,c: INTEGER;

  PROCEDURE header;
    VAR buf: ARRAY [0..79] OF CHAR;
        y,m,d,h,mi,se: INTEGER;
  BEGIN
    tim.unpack(HEAD[ATIME],y,m,d,h,mi,se);
    y:=y MOD 100;
    str.print(buf,'"%s" %d/%02d/%02d %d:%02d.%02d',
                  sname,d,m,y,h,mi,se);
    tty.set_reverse(1);
    tty.print('%|*s',c,buf);
    tty.set_reverse(0);
  END header;

BEGIN
  tty.set_cursor(0);
  tty.home; tty.erase(0);
  c:=tty.state^.columns-1; header;

  u_lucx:=c DIV 2+1; u_lucy:=2;
  messline:=tty.state^.lines-1;
  lines:=tty.state^.lines-u_lucy-2(*frame*)-2; ASSERT(lines>0);
  u_cols:=3;
  u_cwdh:=(c DIV 2-u_cols-1-2) DIV u_cols;

  c_lucx:=0; c_lucy:=u_lucy;
  c_stkx:=1; c_utlx:=c_stkx+6;
  c_iprx:=c DIV 2-2;
  c_utlw:=c_iprx-1-c_utlx;

  (***************** util list frame *******************)
  tty.set_pos(u_lucy,u_lucx);
  tty.print('%c%.*c',tty.state^.bars[0,0],u_cwdh,tty.state^.hbar);
  FOR i:=1 TO u_cols-1 DO
    tty.print('%c%.*c',tty.state^.bars[0,1],u_cwdh,tty.state^.hbar)
  END;
  tty.print('%c',tty.state^.bars[0,2]);
  l:=1;
  WHILE l<=lines DO
    tty.set_pos(u_lucy+l,u_lucx);
    tty.print('%c',tty.state^.vbar);
    FOR i:=1 TO u_cols DO
      tty.set_pos(u_lucy+l,i*(u_cwdh+1)+u_lucx);
      tty.print('%c',tty.state^.vbar)
    END;
    INC(l)
  END;
  tty.set_pos(u_lucy+1+lines,u_lucx);
  tty.print('%c%.*c',tty.state^.bars[2,0],u_cwdh,tty.state^.hbar);
  FOR i:=2 TO u_cols DO
    tty.print('%c%.*c',tty.state^.bars[2,1],u_cwdh,tty.state^.hbar)
  END;
  tty.print('%c',tty.state^.bars[2,2]);

  (***************** configuration frame *******************)
  tty.set_pos(c_lucy,c_lucx);
  tty.print('%c%.*c%c%.*c%c%c%c',
             tty.state^.bars[0,0],
             5     ,tty.state^.hbar,
             tty.state^.bars[0,1],
             c_utlw,tty.state^.hbar,
             tty.state^.bars[0,1],
                    tty.state^.hbar,
             tty.state^.bars[0,2]);
  l:=1;
  WHILE l<=lines DO
    tty.set_pos(c_lucy+l,c_lucx);
    tty.print('%c',tty.state^.vbar);
    tty.set_pos(c_lucy+l,c_utlx-1);
    tty.print('%c',tty.state^.vbar);
    tty.set_pos(c_lucy+l,c_iprx-1);
    tty.print('%c %c',tty.state^.vbar,tty.state^.vbar);
    INC(l)
  END;
  tty.set_pos(c_lucy+1+lines,c_lucx);
  tty.print('%c%.*c%c%.*c%c%c%c',
             tty.state^.bars[2,0],
             5     ,tty.state^.hbar,
             tty.state^.bars[2,1],
             c_utlw,tty.state^.hbar,
             tty.state^.bars[2,1],
                    tty.state^.hbar,
             tty.state^.bars[2,2]);
END prepare_terminal;

VAR nodes_list: DYNARR OF ulist;

PROCEDURE pack_nodes;
  VAR i: INTEGER; l,h: ulist;
BEGIN
  NEW(nodes_list,lines);
  FOR i:=0 TO HIGH(nodes_list) DO nodes_list[i]:=NIL END;
  h:=nodes; nodes:=NIL;
  WHILE h#NIL DO
    l:=h; h:=h^.next;
    l^.next:=nodes;
    nodes:=l
  END;
  l:=nodes; i:=0;
  WHILE l#NIL DO
    nodes_list[i]:=l; l:=l^.next; i:=i+1
  END
END pack_nodes;

PROCEDURE unpack_nodes;
  VAR i: INTEGER; l: ulist;
BEGIN
  nodes:=NIL;
--  FOR i:=HIGH(nodes_list) TO 0 BY -1 DO
  FOR i:=0 TO HIGH(nodes_list) DO
    l:=nodes_list[i];
    IF l#NIL THEN l^.next:=nodes; nodes:=l END
  END;
  DISPOSE(nodes_list)
END unpack_nodes;

PROCEDURE ref_node(l,col: INTEGER; mark: BOOLEAN; n: ulist);
  VAR buf: ARRAY [0..78] OF CHAR;
BEGIN
  IF mark THEN tty.set_reverse(1) END;
  l:=l+c_lucy+1;
  IF n=NIL THEN
    tty.set_pos(l,c_utlx); tty.print('%*s',c_utlw,'');
    IF mark THEN tty.set_reverse(0)
    ELSE
      tty.set_pos(l,c_stkx); tty.print('%5s','');
      tty.set_pos(l,c_iprx); tty.print(' ');
    END;
    RETURN
  END;
  CASE col OF
    |0: tty.set_pos(l,c_stkx);
        tty.print('%5d',n^.stk)
    |1: str.print(buf,'%s %s',n^.util,n^.args);
        tty.set_pos(l,c_utlx);
        tty.print('%-*.*s',c_utlw,c_utlw,buf);
    |2: tty.set_pos(l,c_iprx);
        IF n^.wait THEN tty.print(' ') ELSE tty.print('&') END
  END;
  IF mark THEN tty.set_reverse(0) END
END ref_node;

PROCEDURE ref_nodes;
  VAR i: INTEGER; n: ulist;
BEGIN
  FOR i:=0 TO HIGH(nodes_list) DO
    n:=nodes_list[i];
    IF n=NIL THEN
      ref_node(i,0,FALSE,nodes_list[i]);
    ELSE
      ref_node(i,0,FALSE,nodes_list[i]);
      ref_node(i,1,FALSE,nodes_list[i]);
      ref_node(i,2,FALSE,nodes_list[i])
    END
  END
END ref_nodes;

PROCEDURE ref_util(l,c: INTEGER; mark: BOOLEAN; t: list);
BEGIN
  tty.set_pos(l+u_lucy+1,c*(u_cwdh+1)+u_lucx+1);
  IF t#NIL THEN
    IF t^.link=0 THEN tty.set_underline(1) END;
    IF mark      THEN tty.set_reverse  (1) END;
    tty.print('%-*.*s',u_cwdh,u_cwdh, t^.name);
    IF mark      THEN tty.set_reverse  (0) END;
    IF t^.link=0 THEN tty.set_underline(0) END;
--tty.set_pos(messline,0); tty.print('%d    ',t^.link);
  ELSE
    tty.print('%-*.*s',u_cwdh,u_cwdh,"")
  END
END ref_util;

PROCEDURE refresh_util(e: ue_str_ptr);
  VAR t: ue_str_ptr; i,c: INTEGER;
BEGIN
  t:=e; i:=0;
  WHILE i<lines DO
    IF t#NIL THEN
      FOR c:=0 TO HIGH(t^.val) DO
        ref_util(i,c,FALSE,t^.val[c])
      END;
      t:=t^.fwd
    ELSE
      FOR c:=0 TO ue_cols-1 DO
        ref_util(i,c,FALSE,NIL)
      END
    END;
    INC(i)
  END
END refresh_util;

PROCEDURE edit_system;
  VAR u: ue_str_ptr;
     ch: CHAR;
    l,c: INTEGER;
    cur: ue_str_ptr;
   head: ue_str_ptr;

  PROCEDURE up;
  BEGIN
    IF cur^.bck#NIL THEN
      cur:=cur^.bck;
      IF l>0 THEN DEC(l)
      ELSE
        head:=head^.bck;
        refresh_util(head)
      END
    END
  END up;

  PROCEDURE pgup;
    VAR i: INTEGER;
  BEGIN
    i:=8;
    WHILE (i>0) & (cur^.bck#NIL) DO
      cur:=cur^.bck; DEC(l); DEC(i)
    END;
    IF l>=0 THEN RETURN END;
    REPEAT head:=head^.bck; INC(l) UNTIL l=0;
    ASSERT(head=cur);
    refresh_util(head)
  END pgup;

  PROCEDURE home;
  BEGIN
    IF head^.bck=NIL THEN
      cur:=head; l:=0
    ELSE
      REPEAT head:=head^.bck UNTIL head^.bck=NIL;
      cur:=head; l:=0; refresh_util(head)
    END
  END home;

  PROCEDURE down;
  BEGIN
    IF cur^.fwd#NIL THEN
      cur:=cur^.fwd;
      IF l<lines-1 THEN INC(l)
      ELSE
        head:=head^.fwd;
        refresh_util(head)
      END
    END
  END down;

  PROCEDURE pgdown;
    VAR i: INTEGER;
  BEGIN
    i:=8;
    WHILE (cur^.fwd#NIL) & (i>0) DO
      cur:=cur^.fwd; INC(l); DEC(i)
    END;
    IF l<lines THEN RETURN END;
    REPEAT
      head:=head^.fwd; DEC(l); ASSERT(head#NIL);
    UNTIL l<lines;
    refresh_util(head);
  END pgdown;

  PROCEDURE end;
  BEGIN
    IF cur^.fwd=NIL THEN RETURN END;
    REPEAT cur:=cur^.fwd; INC(l) UNTIL cur^.fwd=NIL;
    WHILE cur^.val[c]#nil_list DO
      c:=c+1 MOD u_cols
    END;
    IF l<lines THEN RETURN END;
    REPEAT
      ASSERT(head#NIL);
      head:=head^.fwd; DEC(l)
    UNTIL l<lines;
    refresh_util(head)
  END end;

  VAR desc: sle.descriptor;
      help: ARRAY [0..79] OF CHAR;

  CONST
    nodes_help =
'TAB-Util ARROWS-Move F4-DelLn F5-SwapUp F6-SwapDw CR-Edit ESC-Quit CTRL_E-Exit';
     util_help =
'TAB-Config ARROWS-Move CR-Del/AppModule F4-DelUtil ESC-Quit CTRL_E-Exit';

  PROCEDURE era_messline;
    VAR c: INTEGER;
  BEGIN
    c:=tty.state^.columns-1;
    tty.set_pos(messline,0);
    tty.print('%|*.*s',c,c,help)
  END era_messline;

  PROCEDURE message(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
    VAR ch: CHAR;
  BEGIN
    tty.set_pos(messline,0); tty.erase_line(0);
    tty.print(fmt,args);
    tty.set_pos(messline,tty.state^.columns-10);
    tty.print('HIT KEY ');
    key.read(ch);
    era_messline
  END message;

  PROCEDURE conf_str(VAL pmt: ARRAY OF CHAR;
                     VAR inp: ARRAY OF CHAR;
                     l,c0,c1: INTEGER): BOOLEAN;
  BEGIN
    desc^.how:=sle.confirm;
    tty.set_pos(messline,0); tty.erase_line(0); tty.print('%s',pmt);
    tty.set_cursor(1);
    sle.edit_str("",inp, l, c0, c1,desc,033c);
    tty.set_cursor(0);
    era_messline;
    RETURN (desc^.last=033c) OR (inp='')
  END conf_str;

  PROCEDURE get_str(VAL pmt: ARRAY OF CHAR;
                    VAR inp: ARRAY OF CHAR;
                        ch0: CHAR;
                    l,c0,c1: INTEGER): BOOLEAN;
  BEGIN
    low.fill(inp,0);
    inp[0]:=ch0;
    RETURN conf_str(pmt,inp,l,c0,c1)
  END get_str;

  PROCEDURE get_util_name(VAR inp: ARRAY OF CHAR; ch0: CHAR): BOOLEAN;
    VAR ln,c0,c1: INTEGER;
  BEGIN
    ln:=l+u_lucy+1;
    c0:=c*(u_cwdh+1)+u_lucx+1;
    c1:=c0+u_cwdh-1;
    RETURN get_str("enter module name to append",inp,ch0,ln,c0,c1)
  END get_util_name;

  PROCEDURE check_code(VAL name: ARRAY OF CHAR): BOOLEAN;
    VAR mname: STR32; main: list; test: bio.FILE;
  BEGIN
    main:=find(kernel,name);
    IF main=NIL THEN main:=find(util,name) END;
    IF main#NIL THEN RETURN TRUE END;
    str.print(mname,'%s.cod',name);
    bio.lookup(BIN,test,mname,'');
    IF NOT bio.done THEN
      lex.perror(mname,bio.error,'"%s": %%s',bio.ename);
      message('%s',mname);
      RETURN FALSE
    END;
    bio.close(test);
    RETURN TRUE
  END check_code;

  PROCEDURE include(ch0: CHAR): BOOLEAN;
    VAR inp: STR32;
  BEGIN
    IF get_util_name(inp,ch0) THEN RETURN FALSE END;
    IF check_code(inp) THEN
      _append(inp); RETURN TRUE
    END;
    RETURN FALSE
  END include;

  PROCEDURE untie(l: list);
    VAR t,p: list;
  BEGIN
    t:=util; p:=NIL;
    WHILE (t#NIL) & (t#l) DO
      p:=t; t:=t^.next
    END;
    ASSERT(t#NIL);
    IF p=NIL THEN util:=util^.next ELSE p^.next:=t^.next END
  END untie;

  PROCEDURE dec_links(l: list);
    VAR    s: str_ptr;
      time,i: INTEGER;
           t: list;
  BEGIN
    IF l^.link>0 THEN RETURN END;
    first_external(l^.cod,s,time);
    FOR i:=1 TO l^.cod^.no_exts-1 DO
      IF find(kernel,s^)=NIL THEN
        t:=find(util,s^);
        IF (t#NIL) & (t^.link>0) THEN DEC(t^.link) END
      END;
      next_external(l^.cod,s,time)
    END
  END dec_links;

  PROCEDURE exclude(l: list; tree: BOOLEAN): BOOLEAN;

    PROCEDURE del_subs(l: list);
      VAR    s: str_ptr;
        time,i: INTEGER;
             t: list;
    BEGIN
      IF l^.link>0 THEN RETURN END;
      first_external(l^.cod,s,time);
      FOR i:=1 TO l^.cod^.no_exts-1 DO
        IF find(kernel,s^)=NIL THEN
          t:=find(util,s^);
          IF (t#NIL) & (t^.link>0) THEN
            DEC(t^.link);
            IF t^.link=0 THEN
              untie(t);
              del_subs(t);
              mem.DEALLOCATE(t^.cod,t^.csz);
              DISPOSE(t)
            END;
          END
        END;
        next_external(l^.cod,s,time)
      END
    END del_subs;

  BEGIN
    IF l^.link#0 THEN
      message('"%s": busy, can not be excluded',l^.name);
      RETURN FALSE
    END;
    untie(l);
    IF tree THEN
      del_subs(l)
    ELSE
      dec_links(l)
    END;
    mem.DEALLOCATE(l^.cod,l^.csz);
    DISPOSE(l);
    RETURN TRUE
  END exclude;

  PROCEDURE edit_util(tree: BOOLEAN; ch0: CHAR);
    VAR t: list; ref: BOOLEAN;
  BEGIN
    t:=cur^.val[c];
    IF ((t=nil_list) & include(ch0)) OR
       ((t#nil_list) & exclude(t,tree)) THEN
      repack_codes(u); refresh_util(u); era_messline;
      cur:=u; head:=u; l:=0; c:=0
    END
  END edit_util;

  VAR UTIL : BOOLEAN;
      nl,nc: INTEGER;

  PROCEDURE unmark_ulist(n: ulist);
    VAR main: list;
  BEGIN
    IF n^.util[0]="$" THEN RETURN END;
    main:=find(util,n^.util);
    IF main#NIL THEN
      DEC(main^.link);
      IF main^.link<=0 THEN refresh_util(head) END
    END
  END unmark_ulist;

  PROCEDURE dis_ulist(VAR n: ulist);
  BEGIN
    IF n=NIL THEN RETURN END;
    DISPOSE(n^.util); DISPOSE(n^.args);
    DISPOSE(n); n:=NIL
  END dis_ulist;

  PROCEDURE edit_node(ch: CHAR);

    VAR refresh: BOOLEAN;

    PROCEDURE new_ulist(VAR n: ulist);
    BEGIN
      NEW(n);
      NEW(n^.util); n^.wait:=TRUE;
      NEW(n^.args); n^.stk:=0;
      n^.next:=NIL
    END new_ulist;

    PROCEDURE read_stk(VAR stk: INTEGER; ch: CHAR): BOOLEAN;
      VAR buf: STR32; i: INTEGER; yes: BOOLEAN;
        c0,c1: INTEGER;
    BEGIN
      low.fill(buf,0); buf[0]:=ch;
      IF buf="" THEN
        str.print(buf,'%d',stk)
      END;
      i:=nl+c_lucy+1; c0:=c_lucx+1; c1:=c0+4;
      IF conf_str('enter stack word size',buf,i,c0,c1) THEN RETURN FALSE END;
      i:=0;
      str.iscan(stk,buf,i,yes);
      IF yes THEN
        IF stk<0 THEN stk:=0 END;
        IF stk>9999 THEN stk:=99999 END;
      END;
      RETURN yes
    END read_stk;

    PROCEDURE read_util_args(VAR n: ulist; ch: CHAR): BOOLEAN;
      VAR buf: ARRAY [0..79] OF CHAR;
          s,f: INTEGER;
         main: list;
      l,c0,c1: INTEGER;
    BEGIN
      low.fill(buf,0);
      buf[0]:=ch;
      IF (buf="") & (n#NIL) THEN
        str.print(buf,'%s %s',n^.util,n^.args);
      END;
      l:=nl+c_lucy+1; c0:=c_lucx+c_utlx; c1:=c0+c_utlw-1;
      IF conf_str('enter util name & arguments',buf,l,c0,c1) THEN RETURN FALSE END;
      new_ulist(n);
      s:=0; WHILE (s<=HIGH(buf)) & (buf[s]=" ") DO INC(s) END;
      IF s>HIGH(buf) THEN dis_ulist(n); RETURN FALSE END;
      IF buf[s]="$" THEN
        f:=str.len(buf); NEW(n^.util,f-s+1);
        str.sub_arr(n^.util,buf,s,f-s);
        RETURN TRUE
      END;
      f:=s; WHILE (f<=HIGH(buf)) & (buf[f]#" ") & (buf[f]#0c) DO INC(f) END;
      NEW(n^.util,f-s+1);
      str.sub_arr(n^.util,buf,s,f-s);
      s:=f+1;
      f:=str.len(buf);
      REPEAT DEC(f) UNTIL (f<=s) OR (buf[f]#" "); INC(f);
      IF f>=s THEN
        NEW(n^.args,f-s+1); str.sub_arr(n^.args,buf,s,f-s)
      END;
      IF check_code(n^.util) THEN
        _append(n^.util);
        main:=find(util,n^.util); ASSERT(main#NIL); INC(main^.link);
        n^.stk:=256;
        repack_codes(u);
        l:=0; c:=0; cur:=u; head:=u;
        refresh_util(head);
        RETURN TRUE
      ELSE dis_ulist(n); RETURN FALSE
      END;
      RETURN FALSE
    END read_util_args;

    PROCEDURE input;
      VAR n: ulist;
          i: INTEGER;
    BEGIN
      n:=NIL;
      IF asc.control IN asc.KIND(ch) THEN ch:=0c END;
      IF read_util_args(n,ch) THEN
        nodes_list[nl]:=n; refresh:=TRUE
--      ELSE dis_ulist(n)
      END
    END input;

    PROCEDURE edit;
      VAR n: ulist; i: INTEGER;
    BEGIN
      n:=nodes_list[nl];
      CASE nc OF
        |0: IF asc.KIND(ch)*{asc.digit}#{} THEN
              IF read_stk(i,ch) THEN nodes_list[nl]^.stk:=i END
            ELSIF (ch=0c) OR (ch=key.cr) THEN
              i:=nodes_list[nl]^.stk;
              IF read_stk(i,0c) THEN nodes_list[nl]^.stk:=i END
            END;
        |1: IF asc.KIND(ch)*{asc.control}={} THEN
              IF read_util_args(n,ch) THEN
                unmark_ulist(nodes_list[nl]);
                dis_ulist(nodes_list[nl]);
                nodes_list[nl]:=n;
                refresh:=TRUE
              END
            ELSE
              IF read_util_args(n,0c) THEN
                unmark_ulist(nodes_list[nl]);
                dis_ulist(nodes_list[nl]);
                nodes_list[nl]:=n;
                refresh:=TRUE
              END
            END
        |2: IF ch=asc.CR THEN
              nodes_list[nl]^.wait:=NOT nodes_list[nl]^.wait
            ELSIF ch=" " THEN
              nodes_list[nl]^.wait:=TRUE
            ELSIF ch="&" THEN
              nodes_list[nl]^.wait:=FALSE
            END;
      END
    END edit;

  BEGIN
    refresh:=FALSE;
    IF nodes_list[nl]=NIL THEN input
    ELSE edit
    END;
    IF refresh THEN ref_nodes; era_messline END
  END edit_node;

  PROCEDURE swap_up;
    VAR n: ulist; i: INTEGER;
  BEGIN
    IF nl=0 THEN RETURN END;
    n:=nodes_list[nl-1];
    nodes_list[nl-1]:=nodes_list[nl];
    nodes_list[nl]:=n;
    FOR i:=0 TO 2 DO
      ref_node(nl-1,i,FALSE,nodes_list[nl-1]);
      ref_node(nl  ,i,FALSE,nodes_list[nl  ])
    END;
    DEC(nl)
  END swap_up;

  PROCEDURE swap_dw;
    VAR n: ulist; i: INTEGER;
  BEGIN
    IF nl=HIGH(nodes_list) THEN RETURN END;
    n:=nodes_list[nl+1];
    nodes_list[nl+1]:=nodes_list[nl];
    nodes_list[nl]:=n;
    FOR i:=0 TO 2 DO
      ref_node(nl+1,i,FALSE,nodes_list[nl+1]);
      ref_node(nl  ,i,FALSE,nodes_list[nl  ])
    END;
    INC(nl)
  END swap_dw;

  PROCEDURE del_ln;
    VAR i,c: INTEGER; n: ulist;
  BEGIN
    n:=nodes_list[nl];
    IF n#NIL THEN
      unmark_ulist(n); dis_ulist(n);
    END;
    i:=nl;
    WHILE i<HIGH(nodes_list) DO
      nodes_list[i]:=nodes_list[i+1]; INC(i)
    END;
    nodes_list[i]:=NIL;
    ref_nodes;
  END del_ln;

  VAR i: INTEGER;

BEGIN
  new_nil_list;
  sle.new(desc,8);
  u:=NIL;
  repack_codes(u);
  prepare_terminal;
  refresh_util(u);
  pack_nodes; ref_nodes;
  UTIL:=FALSE; help:=nodes_help;
  era_messline;
  l:=0; c:=0; cur:=u; head:=u; nl:=0; nc:=0;
  LOOP
    IF UTIL THEN
      ref_util(l,c,TRUE ,cur^.val[c]);
      key.read(ch);
      ref_util(l,c,FALSE,cur^.val[c]);
      CASE ch OF
        |033c     : HALT
        |key.exit : EXIT
        |key.up   : up
        |key.pgup : pgup
        |key.dw   : down
        |key.home : home
        |key.end  : end
        |key.pgdw : pgdown
        |key.left : c:=(c+u_cols-1) MOD u_cols
        |key.right: c:=(c+1) MOD u_cols
        |key.tab  : UTIL:=FALSE; help:=nodes_help; era_messline;
        |" ",key.cr: edit_util(FALSE,0c)
        |key.f4   : edit_util(TRUE,0c)
      ELSE
        IF (asc.KIND(ch)*{asc.control}={}) & (cur^.val[c]=nil_list) THEN
          edit_util(FALSE,ch)
        END;
      END
    ELSE
      ref_node(nl,nc,TRUE ,nodes_list[nl]);
      key.read(ch);
      ref_node(nl,nc,FALSE,nodes_list[nl]);
      CASE ch OF
        |033c     : HALT
        |key.exit : EXIT
        |key.up   : IF nl>0 THEN DEC(nl) END
        |key.pgup : DEC(nl,8); IF nl<0 THEN nl:=0 END
        |key.dw   : IF nl<HIGH(nodes_list) THEN INC(nl) END
        |key.pgdw : INC(nl,8);
                    IF nl>HIGH(nodes_list) THEN
                      nl:=HIGH(nodes_list)
                    END
        |key.home : nl:=0
        |key.end  : nl:=HIGH(nodes_list)
        |key.left : nc:=(nc+2) MOD 3
        |key.right: nc:=(nc+1) MOD 3
        |key.tab  : UTIL:=TRUE; help:=util_help; era_messline;
        |" ",key.cr: edit_node(ch)
        |key.f4   : del_ln;
        |key.f5   : swap_up;
        |key.f6   : swap_dw;
      ELSE
        IF asc.KIND(ch)*{asc.control}={} THEN
          edit_node(ch)
        END;
      END
    END
  END;
  sle.dispose(desc);
  unpack_nodes;
  tty.set_cursor(1);
  tty.set_pos(messline,0); tty.erase_line(0)
END edit_system;

PROCEDURE dis_nodes;
  VAR n: ulist; i: INTEGER; main: list;
BEGIN
  WHILE nodes#NIL DO
    n:=nodes; nodes:=n^.next;
    i:=0;
    str.skip(n^.util,i,' ');
    IF (i<=HIGH(n^.util)) & (n^.util[i]#"$") THEN
      main:=find(util,n^.util);
      IF main#NIL THEN DEC(main^.link) END
    END;
    DISPOSE(n^.util);
    DISPOSE(n^.args);
    DISPOSE(n)
  END;
END dis_nodes;

PROCEDURE reconfigsys(VAL name,cname: ARRAY OF CHAR);
  VAR size: INTEGER;
BEGIN
  str.print(sname,'%s.%s',name,EXT);
  copy_sys(TRUE);
  size:=bio.eof(sys) DIV 4; ASSERT(size*4=bio.eof(sys));

  IF cname#"" THEN
    dis_nodes; codes_and_iprs(cname)
  ELSE
    edit_system;
  END;

  HEAD[UTIL ]:=0;
  HEAD[IPR  ]:=0;
  HEAD[CODES]:=0;

  makeinitcode(size);
  insert_utils;

  seekw(0);
  bio.write(sys,SYSTEM.ADR(HEAD),BYTES(HEAD)); chk(WRITE);

  seekw(HEAD[PROC0]);
  bio.write(sys,SYSTEM.ADR(TAIL),BYTES(TAIL)); chk(WRITE);

  bio.cut(sys,HEAD[90h]*4+BYTES(TAIL)); chk("cut");
  bio.close(sys); chk(CLOSE);

END reconfigsys;

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
  bio.read (sys,buf^.ADR,eof);        chk(READ);
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
  std.print("   config -R [-f]  system_name [cnf=cnf_name]\n");
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
  ELSIF arg.flag('-','r') THEN   configsys(arg.words[0],'',FALSE)
  ELSIF arg.flag('-','R') THEN
    IF arg.flag('-','f') THEN
      IF arg.string('cnf',cname) THEN
        reconfigsys(arg.words[0],cname)
      ELSE
        reconfigsys(arg.words[0],arg.words[0])
      END
    ELSE
      reconfigsys(arg.words[0],"")
    END
  ELSIF arg.flag('-','e') THEN
    configsys(arg.words[0],arg.words[0],TRUE)
  ELSIF arg.flag('-','l') THEN
    listsys(arg.words[0])
  ELSIF arg.flag('-','h') THEN
    pusage
  END
END config.


ADDITIONAL KEYS:

   -R   -- Reconfigurate system with smth. like screen editor
   -Rf  -- Reconfigurate system using *.cnf file

                                        Hady. 30-Jan-92.
