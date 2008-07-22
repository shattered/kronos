IMPLEMENTATION MODULE BIO; (* Leo 04-Sep-89. (c) KRONOS *)
                           (* Leo 26-Jun-90. (c) KRONOS *)
                           (* Igo 02-Mar-92. (c) KRONOS *)

(*$U+*)

IMPORT       SYSTEM;            IMPORT   os: osKernel;
IMPORT       ASCII;             IMPORT   fs: osFiles;

IMPORT  cod: defCodes;          IMPORT  str: Strings;
IMPORT  req: defRequest;        IMPORT  env: tskEnv;
IMPORT  err: defErrors;         IMPORT  low: lowLevel;
IMPORT  mem: Heap;              IMPORT  fmt: Formats;

WITH STORAGE (NEW: mem.allocate;
          DISPOSE: mem.deallocate;
           RESIZE: mem.reallocate);

VAR MAGIC: INTEGER;

CONST ok = err.ok;
 _MAGIC  = 656C6966h; (* "file" *)

CONST
  CD   = env.cd;
  SU   =    0 ;    (* superuser *)

TYPE
  ADDRESS = SYSTEM.ADDRESS;
  WORD    = SYSTEM.WORD;
  str32   = ARRAY [0..31] OF CHAR;

  BUFFER  = RECORD
              pos: INTEGER;
              dty: BOOLEAN;
              buf: DYNARR OF CHAR;
            END;

  BUFFERs = DYNARR OF BUFFER;

  DNODEs  = DYNARR OF fs.dnode;
  FILE    = POINTER TO F_DESC;
  F_DESC  =
  RECORD
    magic: INTEGER; (* offset 0 see: ini *)
    file : fs.FILE; (* offset 1 see: ini *)
    dir  : fs.FILE;
    name : str32;
    fname: STRING;
    mode : BITSET;
    fpos : INTEGER;
    buf  : DNODEs;
    order: DYNARR OF INTEGER;
    pbuf : INTEGER;            -- позиция в buf
    next : FILE;
    buffs: BUFFERs;
  END;

  PATHs   = POINTER TO pNODE;
  pNODE   = RECORD
              dir : FILE;
              name: DYNARR OF CHAR;
              next: PATHs
            END;


CONST -- additional file modes
  append  = {31};
  hidden  = {30};
  r_pro   = {29};
  w_pro   = {28};
  l_pro   = {27};
  x_pro   = {26};
  _avail =  {25};
  to_link = {24};



CONST
  OPEN_MODEs  = r_pro + w_pro + l_pro+x_pro + append + _avail
              + fs.nocash + fs.wait_wr + fs.nodelay;
  CREATE_MODEs= w_pro + l_pro + x_pro + hidden + to_link + fs.wait_wr;
  LINK_MODEs  = l_pro + x_pro + hidden;

CONST
  UNKNOWN ="U N K N O W N";

VAR fset: FILE;
    self: os.task_ptr; (* never be changed durring program execution *)
  ignore: INTEGER;


PROCEDURE check_p_stack(n: INTEGER);
CODE cod.copt cod.alloc cod.drop cod.decs END check_p_stack;

PROCEDURE su(user: INTEGER): BOOLEAN;
(* (user=0) OR (user*{7}#{}) *)
CODE cod.copt cod.not cod.swap cod.lib 80h cod.and cod.or END su;

PROCEDURE gro(user: INTEGER): INTEGER;
(* user MOD 80h *)
CODE cod.lib 7Fh cod.and END gro;

PROCEDURE usr(user: INTEGER): INTEGER;
(* user DIV 100h MOD 80h *)
CODE cod.li8 cod.shr cod.lib 7Fh cod.and END usr;

(*
PROCEDURE illegal_desc(f: FILE): BOOLEAN;
CODE
  cod.copt cod.lin cod.equ
    cod.orjp 14             (* M A G I C *)
  cod.copt cod.lsw0 cod.liw 46h 49h 4Ch 45h cod.neq
    cod.orjp 04
  cod.copt cod.lsw1 cod.lin cod.equ
  cod.swap cod.drop
END illegal_desc;
*)

PROCEDURE bad_fd(magic: INTEGER; f: FILE): BOOLEAN;
CODE
  cod.copt          cod.lin  cod.equ  cod.orjp 10
  cod.copt cod.lsw1 cod.lin  cod.equ  cod.orjp  4
           cod.lsw0 cod.neq           cod.jfs   2
  cod.drop cod.drop (* magic#0 used as TRUE result! *)
END bad_fd;

VAR   halt: INTEGER;
  io_check: BOOLEAN;

PROCEDURE check_io(on_off: BOOLEAN);
BEGIN
  io_check:=on_off
END check_io;

PROCEDURE errorf(f: FILE; code: INTEGER);
BEGIN
  done:=FALSE; error:=code;
  IF (f=NIL) OR (f^.magic#MAGIC) THEN ename:=UNKNOWN ELSE ename:=f^.name END
END errorf;

PROCEDURE errorn(VAL name: ARRAY OF CHAR; code: INTEGER);
BEGIN done:=FALSE; error:=code; str.copy(ename,name) END errorn;


PROCEDURE enter;
BEGIN check_p_stack(200); os.lock END enter;

PROCEDURE ioerror(f: FILE; code: INTEGER);
(* ioerror may and MUST be called in parentheses 'enter' 'exit' ! *)
BEGIN
  IF io_check THEN halt:=code ELSE errorf(f,code) END
END ioerror;

PROCEDURE exit;
BEGIN
  os.unlock;
  IF halt#0 THEN HALT(halt) END
END exit;


PROCEDURE alloc_file(VAR f: FILE);
BEGIN
  NEW(f);
  IF f=NIL THEN error:=err.no_memory; done:=FALSE; RETURN END;
  f^.magic:=MAGIC;
  f^.file:=fs.null;      f^.dir :=fs.null;        NEW(f^.buffs);
  f^.name:="";           f^.mode:={};             NEW(f^.fname);
  f^.pbuf:=-1;                                    NEW(f^.buf);
  f^.next:=fset;         f^.fpos:=0;              NEW(f^.order);
  fset:=f;
END alloc_file;

PROCEDURE disposewbuf(VAR wb: DNODEs);
BEGIN
  IF wb^.ADR=NIL THEN RETURN END;
  os.DEALLOCATE(self^.area,wb^.ADR,(wb^.HIGH+1)*SIZE(fs.dnode))
END disposewbuf;

PROCEDURE release_file(VAR f: FILE);
  VAR i: INTEGER;
    l,p: FILE;
BEGIN
  l:=fset; p:=NIL;
  WHILE (l#NIL) & (l#f) DO p:=l; l:=l^.next END;
  IF l=NIL THEN f:=NIL; RETURN END;
  IF p=NIL THEN fset:=f^.next ELSE p^.next:=f^.next END;
  f^.magic:=0;
  disposewbuf(f^.buf);
  DISPOSE(f^.order);
  DISPOSE(f^.fname);
  FOR i:=0 TO HIGH(f^.buffs) DO DISPOSE(f^.buffs[i].buf) END;
  DISPOSE(f^.buffs);
  DISPOSE(f)
END release_file;

PROCEDURE pars_modes(VAL s: ARRAY OF CHAR; VAR mode: BITSET);
  VAR i: INTEGER;
BEGIN
  mode:=r_pro+w_pro+x_pro;
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO
    CASE s[i] OF
      |'r': mode:=mode-r_pro;
      |'w': mode:=mode-w_pro;
      |'x': mode:=mode-x_pro;
      |'m': mode:=mode-r_pro-w_pro;
      |'c': mode:=mode+fs.nocash;
      |'d': mode:=mode+fs.wait_wr;
      |'n': mode:=mode+fs.nodelay;
      |'h': mode:=mode+hidden;
      |'a': mode:=mode+append-w_pro;
      |'X': mode:=mode+_avail
    ELSE -- nothing
    END;
    INC(i);
  END;
END pars_modes;


PROCEDURE splitpathname(VAR path: ARRAY OF CHAR; VAR name: ARRAY OF CHAR);
  VAR i,l,k,n: INTEGER;   ch: CHAR;
BEGIN
  (*$<*) (*$T+*)
  done:=TRUE;
  name:="";
  IF HIGH(path)<0 THEN errorn(UNKNOWN,err.bad_name); RETURN END;
  IF path="/"0c THEN path:=""; name:="/"; RETURN END;
  l:=-1;
  i:=0; ch:=path[i];
  LOOP
    IF ch=0c THEN EXIT END;
    IF ch='/' THEN
      IF (l>=0) & (i-l=1) THEN EXIT
      ELSIF i-l<=32       THEN l:=i
      ELSE                     EXIT
      END;
    END;
    i:=i+1;
    IF i>HIGH(path) THEN EXIT END;
    ch:=path[i]
  END;
  IF (i-l>32) OR (l>=0) & (i-l=1) THEN
    done:=FALSE; error:=err.bad_name;
    n:=0; l:=l+1;
    WHILE (l<=i) & (n<HIGH(ename)) DO ename[n]:=path[l]; INC(l); INC(n) END;
    ename[n]:=0c;
    RETURN
  END;
  n:=0;
  FOR k:=l+1 TO i-1 DO name[n]:=path[k]; INC(n) END;
  name[n]:=0c;
  IF l<=0 THEN path[l+1]:=0c ELSE path[l]:=0c END;
  (*$>*)
END splitpathname;

PROCEDURE _mask(f: FILE): BITSET;
  VAR p,m: BITSET;
     u,fo: INTEGER;
BEGIN
  u:=self^.user;
  fs.getattr(f^.file,fs.a_pro,p);  -- p:=BITSET(f^.file^.pro);
  fo:=INTEGER(p-{0..15})>>16;
  IF    su(u)          THEN m:={}
  ELSIF usr(u)=usr(fo) THEN m:=p
  ELSIF gro(u)=gro(fo) THEN m:=p>>4
  ELSE                      m:=p>>8
  END;
  RETURN m*{0..2}+p*(run_priv+run_uid);
END _mask;

PROCEDURE _enable(f: fs.FILE; mask: BITSET);
  VAR u: INTEGER;
     fo: INTEGER;
  m,r,p: BITSET;
BEGIN
  u:=self^.user;
  fs.getattr(f,fs.a_pro,p);  -- p:=BITSET(f^.pro);
  fo:=INTEGER(p-{0..15})>>16;
  IF     su(u)         THEN m:={}
  ELSIF usr(u)=usr(fo) THEN m:=p
  ELSIF gro(u)=gro(fo) THEN m:=p>>4
  ELSE                      m:=p>>8
  END;
  done:=(mask*m*{0..2}={});
  IF NOT done THEN errorn(UNKNOWN,err.sec_vio) END
END _enable;

PROCEDURE _set_mode(f: FILE; mode: BITSET);
  VAR u: INTEGER;
     fo: INTEGER;
  p,m,r: BITSET;
BEGIN
  u:=self^.user;
  fs.getattr(f^.file,fs.a_pro,p);  -- p:=BITSET(f^.file^.pro);
  fo:=INTEGER(p-{0..15})>>16;
  IF     su(u)         THEN m:={}
  ELSIF usr(u)=usr(fo) THEN m:=p
  ELSIF gro(u)=gro(fo) THEN m:=p>>4
  ELSE                      m:=p>>8
  END;
  mode:=mode*OPEN_MODEs;
  IF _avail*mode#{} THEN
    IF own_write*m={} THEN mode:=mode-w_pro ELSE mode:=mode+w_pro END;
    IF own_read *m={} THEN mode:=mode-r_pro ELSE mode:=mode+r_pro END;
    IF own_exec *m={} THEN mode:=mode-x_pro ELSE mode:=mode+x_pro END;
    mode:=mode-_avail
  END;
  IF (own_write*m#{}) & (w_pro*mode={})
  OR (own_read *m#{}) & (r_pro*mode={})
  OR (own_exec *m#{}) & (x_pro*mode={})
  THEN
    errorf(f,err.sec_vio); RETURN
  END;
  f^.mode:=mode
END _set_mode;

PROCEDURE access(f: FILE): BITSET;
  VAR p: BITSET;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN {0..2} END;
  fs.getattr(f^.file,fs.a_pro,p);  -- p:=BITSET(f^.file^.pro);
  RETURN p*{0..15}
END access;

PROCEDURE owner(f: FILE; VAR owner,group: INTEGER);
  VAR o: BITSET;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  fs.getattr(f^.file,fs.a_pro,o);  -- o:=(BITSET(f^.file^.pro)-{0..15})>>16;
  o:=(o-{0..15})>>16;
  owner:=INTEGER((o>>8)*{0..6});
  group:=INTEGER( o    *{0..6});
END owner;

PROCEDURE chmode(f: FILE; VAL mode: ARRAY OF CHAR);
  VAR m: BITSET;
BEGIN
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  done:=TRUE;
  pars_modes(mode,m);
  _set_mode(f,m);
  IF NOT done THEN errorf(f,error) END
END chmode;

PROCEDURE equal(f0,f1: FILE): BOOLEAN;
BEGIN
  IF bad_fd(MAGIC,f0) THEN errorf(f0,err.bad_desc); RETURN FALSE END;
  IF bad_fd(MAGIC,f1) THEN errorf(f1,err.bad_desc); RETURN FALSE END;
  RETURN f0^.file=f1^.file
END equal;



(*                          THROUGH                           *)
(* walk through 'pathname' and returns openned directory 'sub'*)
(* at which 'path' points.                                    *)
(* 'through' may be called only after 'splitpath'!            *)
(* 'sub' always closed when 'through' returns bad result      *)

PROCEDURE through(dir: FILE; VAL path: ARRAY OF CHAR;
                             VAR  sub: fs.FILE;  access: BITSET);

  VAR p: INTEGER;
    cur: fs.FILE;
    res: INTEGER;       prename: str32;
    clo: INTEGER;       subname: str32;

  PROCEDURE one; (* set "done" to FALSE when error occurs *)
    VAR i: INTEGER;
  BEGIN i:=0;
    REPEAT
      subname[i]:=path[p]; INC(i); INC(p)
    UNTIL (path[p]=0c) OR (path[p]='/');
    subname[i]:=0c;
    IF subname="."0c THEN
      sub:=cur;
      res:=fs.reopen(sub)
    ELSE
      res:=fs.open((cur),sub,subname,fs.fsys)
    END;
    IF res#ok THEN errorn(subname,res) END
  END one;

  PROCEDURE close;
  BEGIN
    done:=FALSE; clo:=fs.close(sub); sub:=fs.null
  END close;

BEGIN
  done:=TRUE;
  IF path[0]='/' THEN
    p:=1; subname:="/";
    IF fs.root()=fs.null THEN errorn("/",err.bad_desc); RETURN END;
    cur:=fs.root()
  ELSE
    p:=0;
    IF bad_fd(MAGIC,dir) THEN errorf(NIL,err.bad_desc); RETURN END;
    cur:=dir^.file;  subname:=dir^.name
  END;
  IF NOT fs.isdir(cur) THEN errorn(subname,err.not_dir); RETURN END;
  _enable(cur,own_read);
  IF NOT done THEN ename:=subname; RETURN END;
  IF (path="/"0c) OR (path="") THEN (* simple case *)
    IF access#own_read THEN (* otherwise already checked *)
      _enable(cur,access);
      IF NOT done THEN ename:=subname; RETURN END
    END;
    sub:=cur; res:=fs.reopen(sub);
    IF res#ok THEN done:=FALSE; error:=res; ename:=subname END;
    RETURN
  END;
  one;
  IF NOT done THEN RETURN END;
  WHILE path[p]#0c DO
    INC(p); (* skip '/' *)
    cur:=sub; prename:=subname;
    one;
    clo:=fs.close(cur);
    IF res#ok THEN (* 'cur' & 'sub' both closed here *) sub:=fs.null; RETURN END;
    IF clo#ok THEN close; errorn(prename,clo); RETURN END;
    IF NOT fs.isdir(sub) THEN
      close; errorn(subname,err.not_dir); RETURN
    END;
    _enable(sub,own_read);
    IF NOT done THEN ename:=subname; close; RETURN END;
  END;
  IF access=own_read THEN (* already checked *) RETURN END;
  _enable(sub,access);
  IF NOT done THEN close; ename:=subname END
END through;

PROCEDURE chcmask(m: BITSET);
BEGIN
  done:=TRUE;
  IF su(self^.user) THEN cmask:=m*{0..11,14..15} ELSE cmask:=m*{0..11} END
END chcmask;

PROCEDURE chowner(f: FILE; own,gro: INTEGER);
  VAR fo: INTEGER;  p,o,g: BITSET; pu: BOOLEAN;
BEGIN
  done:=TRUE;
  o:=BITSET(own)*{0..6};
  g:=BITSET(gro)*{0..6};
  IF bad_fd(MAGIC,f)  THEN errorf(f,err.bad_desc); RETURN END;
  o:=o<<8; o:=(o+g)<<16;
  (* only superuser can chowner to 0000: *)
  pu:=su(self^.user);
  IF (o=BITSET(SU)) & NOT pu THEN errorf(f,err.su_only); RETURN END;
  enter;
   fs.getattr(f^.file,fs.a_pro,p);  -- p:=BITSET(f^.file^.pro);
   fo:=INTEGER(p-{0..15})>>16;
   IF pu OR (usr(fo)=usr(self^.user)) THEN
     fs.setattr(f^.file,fs.a_pro,p*{0..15}+o);
     (*
      * f^.file^.pro:=INTEGER(p*{0..15}+o);
      * f^.file^.state:=f^.file^.state+fs.dirty;  (*!!!*)
      *)
   ELSE
     errorf(f,err.sec_vio)
  END;
  exit;
  IF done THEN flush(f) END (* !!! *)
END chowner;

PROCEDURE chaccess(f: FILE; m: BITSET);
  VAR p: BITSET; fo: INTEGER; pu: BOOLEAN;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  pu:=su(self^.user);
  enter;
   fs.getattr(f^.file,fs.a_pro,p);  -- p:=BITSET(f^.file^.pro);
   fo:=INTEGER(p-{0..15})>>16;
   IF pu THEN m:=m*{0..11,14..15}; p:=p-{0..15}
   ELSE       m:=m*{0..11};        p:=p-{0..11}
   END;
   IF pu OR (usr(fo)=usr(self^.user)) THEN
     fs.setattr(f^.file,fs.a_pro,p+m);
     (*
      * f^.file^.pro:=INTEGER(p+m);
      * f^.file^.state:=f^.file^.state+fs.dirty; (*!!!*)
      *)
   ELSE
     errorf(f,err.su_only)
   END;
  exit;
  IF done THEN flush(f) END (*!!!*)
END chaccess;

PROCEDURE fname(f: FILE; VAR name: ARRAY OF CHAR);
BEGIN
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  done:=TRUE;
  IF HIGH(f^.fname)<0 THEN str.copy(name,UNKNOWN)
  ELSE str.copy(name,f^.fname)
  END
END fname;

PROCEDURE full_name(f,d: FILE; path,name: ARRAY OF CHAR);

  VAR i,p: INTEGER; subname: str32;

  PROCEDURE append(s: ARRAY OF CHAR; l: INTEGER; slash: BOOLEAN);
  BEGIN
    IF i+l+2>=BYTES(f^.fname) THEN RESIZE(f^.fname,i+l+8) END;
    IF mem.done THEN
      str.move(f^.fname,i,s,0,l); i:=i+l;
      IF slash THEN f^.fname[i]:="/"; INC(i) END;
      f^.fname[i]:=0c
    END
  END append;

  PROCEDURE father;
  BEGIN
    IF i<=1 THEN RETURN END;
    DEC(i);
    REPEAT DEC(i) UNTIL (i=0) OR (f^.fname[i]="/");
    INC(i); f^.fname[i]:=0c
  END father;

  PROCEDURE one;
    VAR j: INTEGER;
  BEGIN
    j:=0;
    REPEAT
      subname[j]:=path[p]; INC(j); INC(p)
    UNTIL (path[p]=0c) OR (path[p]='/');
    subname[j]:=0c;
    IF    subname="."0c THEN (* nothing *)
    ELSIF subname=".."  THEN father
    ELSE
      append(subname,j,TRUE)
    END;
    IF path[p]='/' THEN INC(p) END
  END one;

BEGIN
  IF (path="") & (name="/"0c) THEN
    RESIZE(f^.fname,2);
    IF mem.done THEN f^.fname[0]:="/"; f^.fname[1]:=0c END;
    RETURN
  END;
  i:=0;
  p:=0;
  IF path[0]#"/" THEN
    IF (d#NIL) & (BYTES(d^.fname)>0) THEN
      IF d^.fname="/"0c THEN append("/",1,FALSE)
      ELSE                   append(d^.fname,BYTES(d^.fname)-1,TRUE)
      END
    END
  ELSE
    append("/",1,FALSE); p:=1
  END;
  WHILE (p<=HIGH(path)) & (path[p]#0c) DO
    one
  END;
  IF name="."0c THEN
    IF i>1 THEN DEC(i); f^.fname[i]:=0c END
  ELSIF name=".." THEN father;
    IF i>1 THEN DEC(i); f^.fname[i]:=0c END
  ELSE
    append(name,str.len(name),FALSE)
  END;
  RESIZE(f^.fname,i+1)
END full_name;

PROCEDURE fopen(d: FILE; VAR f: FILE; path: ARRAY OF CHAR;
                                  VAL mode: ARRAY OF CHAR);

  VAR m: BITSET;
   name: str32;

  PROCEDURE _root;
    VAR res: INTEGER;
  BEGIN
    IF fs.root()=fs.null       THEN errorf(f,err.bad_desc); RETURN END;
    IF NOT fs.isdir(fs.root()) THEN errorf(f,err.not_dir);  RETURN END;
    _enable(fs.root(),m);
    IF NOT done THEN errorf(f,error); RETURN END;
    res:=fs.reopen(fs.root());
    IF NOT done THEN errorf(f,res);   RETURN END;
    f^.file:=fs.root()
  END _root;

  PROCEDURE _open;
    VAR sub: fs.FILE;
    clo,res: INTEGER;
  BEGIN
    through(d,path,sub,m);
    IF NOT done THEN RETURN END;
    IF name="."0c THEN f^.file:=sub; RETURN END;
    res:=fs.open((sub),f^.file,name,m);
    clo:=fs.close(sub);
    IF res=ok THEN res:=clo END;
    done:=(res=ok);
    IF NOT done THEN errorf(f,res) END
  END _open;

  PROCEDURE _spec;
    VAR i: INTEGER;
      res: INTEGER;
  BEGIN
    IF NOT su(self^.user) THEN errorf(f,err.su_only); RETURN END;
    str.delete(name,0,1);
    res:=fs.open_dev(f^.file,name,m);
    IF res#ok THEN errorf(f,res); RETURN END;
    m:=m+fs.fsys;
    IF (fs.fsys*m={}) OR (fs.state(f^.file)*fs.disk={}) THEN RETURN END;
    res:=fs.close(f^.file);
    IF res#ok THEN errorf(f,res); RETURN END;
    res:=fs.open(fs.null,f^.file,name,m);
    IF res#ok THEN errorf(f,res) END
  END _spec;

  VAR clo: INTEGER; -- Hady. 26-Jul-90

BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  pars_modes(mode,m);
  enter;
   alloc_file(f);
   IF NOT done THEN exit; RETURN END;
   f^.name:=name;         ------
   IF (path="") & (name="/"0c) THEN
     _root
   ELSIF (path="") & (name[0]=':') THEN
     _spec
   ELSE
     _open
   END;
   IF done & (m*append#{}) THEN seek(f,0,2) END;
   IF done THEN _set_mode(f,m) END;
   IF NOT done THEN
     clo:=fs.close(f^.file); release_file(f)
   ELSE
     full_name(f,d,path,name);
     IF fs.state(f^.file)*fs.hidden#{} THEN f^.mode:=f^.mode+hidden END
   END;
  exit
END fopen;

PROCEDURE fcreate(d: FILE; VAR  f: FILE;
                             path: ARRAY OF CHAR;
                        VAL  mode: ARRAY OF CHAR;
                             size: INTEGER);

  VAR usr: BITSET;

  PROCEDURE _temporary;
    VAR res: INTEGER;
  BEGIN
    (* it is no need to check access rigths here *)
    done:=NOT bad_fd(MAGIC,d);
    IF NOT done THEN errorf(d,err.bad_desc); RETURN END;
    done:=fs.isdir(d^.file);
    IF NOT done THEN errorf(d,err.not_dir);  RETURN END;
    res :=fs.create((d^.file),f^.file,size,f^.mode);
    IF res#ok THEN errorf(f,res); RETURN END;
    fs.setattr(f^.file,fs.a_pro,usr+cmask);  -- f^.file^.pro:=INTEGER(usr+cmask);
    (* it is no need to mark file as "dirty" here *)
  END _temporary;

  PROCEDURE _permanent;
    VAR res: INTEGER;
       name: str32;
  BEGIN
    splitpathname(path,name);
    IF NOT done THEN RETURN END;
    res:=fs.copy_fname(f^.name,name); done:=(res=ok);
    IF NOT done THEN errorf(f,res); ename:=name; RETURN END;
    IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
      errorf(f,err.bad_name); RETURN
    END;
    through(d,path,f^.dir,own_write);
    IF NOT done THEN f^.dir:=fs.null; RETURN END;
    res:=fs.create((f^.dir),f^.file,size,f^.mode);
    IF res#ok THEN errorf(f,res); RETURN END;
    f^.mode:=f^.mode+to_link;

    fs.setattr(f^.file,fs.a_pro,usr+cmask);  -- f^.file^.pro:=INTEGER(usr+cmask);
    (*
     * f^.file^.pro:=INTEGER(usr+cmask);
     * f^.file^.state:=f^.file^.state+fs.dirty;    (*!!!*)
     *)
    full_name(f,d,path,name)
  END _permanent;

  PROCEDURE undo;
  BEGIN
    IF f^.dir #fs.null THEN ignore:=fs.close(f^.dir);  f^.dir :=fs.null END;
    IF f^.file#fs.null THEN ignore:=fs.close(f^.file); f^.file:=fs.null END;
    release_file(f)
  END undo;

  VAR m: BITSET;

BEGIN
  done:=TRUE;
  pars_modes(mode,m); (* not changed "done", "error" *)
  usr:=(BITSET(self^.user)*{0..6,8..14}) << 16;
  enter;
   alloc_file(f);
   IF done THEN
     f^.mode:=m*CREATE_MODEs;
     IF (HIGH(path)<0) OR (path="") THEN _temporary ELSE _permanent END;
     IF NOT done THEN undo END
   END;
  exit
END fcreate;

PROCEDURE dup(VAR new: FILE; f: FILE);
  VAR res: INTEGER;
BEGIN
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  done:=TRUE;
  enter;
    alloc_file(new);
    IF done THEN
      res:=fs.reopen(f^.file);
      IF res=ok THEN
        new^.file:=f^.file;
        new^.mode:=f^.mode-to_link;
        new^.fpos:=f^.fpos;
        RESIZE(new^.fname,BYTES(f^.fname));
        str.copy(new^.fname,f^.fname);
      ELSE
        release_file(new);
        errorf(f,res)
      END
    END;
  exit
END dup;

PROCEDURE flink(d: FILE; f: FILE; path,mode: ARRAY OF CHAR);
  VAR res: INTEGER;
      clo: INTEGER;
      hid: BOOLEAN;      m: BITSET;
      sub: fs.FILE;
     name: str32;
BEGIN
  done:=TRUE;
  IF HIGH(path)<0 THEN errorn("",err.bad_name); RETURN END;
  splitpathname(path,name);
  IF NOT done          THEN RETURN END;
  IF bad_fd(MAGIC,f)   THEN errorf(NIL,err.bad_desc); RETURN END;
  IF fs.isdir(f^.file) THEN errorn(name,err.is_dir);  RETURN END;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  IF (f^.name="") & (f^.mode*to_link={}) THEN f^.name:=name END;
  pars_modes(mode,m); (* not changed "done", "error" *)
  hid:=(m*hidden#{});
  enter;
   through(d,path,sub,own_write);
   IF NOT done THEN exit; RETURN END;
   res:=fs.link(sub,f^.file,name,hid);
   clo:=fs.close(sub);
   IF res=ok THEN res:=clo END;
  exit;
  IF res#ok THEN errorf(f,res) END
END flink;

PROCEDURE funlink(d: FILE; path: ARRAY OF CHAR);
  VAR res: INTEGER;
      clo: INTEGER;
      sub: fs.FILE;
     name: str32;
BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  enter;
   through(d,path,sub,own_write);
   IF NOT done THEN exit; RETURN END;
   res:=fs.unlink(sub,name);
   clo:=fs.close (sub);
   IF res=ok THEN res:=clo END;
  exit;
  IF res#ok THEN errorn(name,res) END
END funlink;

PROCEDURE fmkdir(d: FILE; path: ARRAY OF CHAR; hid: BOOLEAN);
  VAR res: INTEGER;
      clo: INTEGER;
      usr: BITSET;
      sub: fs.FILE;
      dir: fs.FILE;
     name: str32;
BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  done:=FALSE;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  enter;
   through(d,path,sub,own_write);
   IF NOT done THEN exit; RETURN END;
   _enable(sub,own_write);
   IF NOT done THEN clo:=fs.close(sub); ename:=name; exit; RETURN END;
   res:=fs.make_dir(dir,(sub));
   IF res#ok THEN errorn(name,res); exit; RETURN END;
   usr:=(BITSET(self^.user)*{0..6,8..14}) << 16;
   fs.setattr(dir,fs.a_pro,usr+cmask);
   res:=fs.link(sub,dir,name,hid);
   clo:=fs.close(sub);
   IF res=ok THEN res:=clo END;
   clo:=fs.close(dir);
   IF res=ok THEN res:=clo END;
  exit;
  done:=(res=ok);
  IF NOT done THEN error:=res; ename:=name END
END fmkdir;

PROCEDURE fmvdir(from,to: FILE; path: ARRAY OF CHAR;
                         VAL newname: ARRAY OF CHAR; hid: BOOLEAN);
  VAR sub: fs.FILE;
      res: INTEGER;
      clo: INTEGER;
      mov: INTEGER;
     name: str32;
BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  done:=NOT bad_fd(MAGIC,to);
  IF NOT done THEN errorf(to,err.bad_desc); RETURN END;
  done:=fs.isdir(to^.file);
  IF NOT done THEN errorf(to,err.not_dir);  RETURN END;
  done:=(to^.mode*w_pro={});
  IF NOT done THEN errorf(to,err.sec_vio);  RETURN END;
  enter;
   through(from,path,sub,own_read+own_write);
   IF NOT done THEN exit; RETURN END;
   res:=fs.move_dir(sub,name,to^.file,newname,hid);
   clo:=fs.close(sub);
   IF res=ok THEN res:=clo END;
  exit;
  IF res#ok THEN errorn(newname,res) END
END fmvdir;

PROCEDURE fmknode(d: FILE; path: ARRAY OF CHAR;
                       VAL node: ARRAY OF CHAR);
  VAR f: fs.FILE;
    sub: fs.FILE;
    res: INTEGER;
    clo: INTEGER;
    mov: INTEGER;
    usr: BITSET;
   name: str32;
BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  done:=TRUE;
  enter;
   through(d,path,sub,own_write);
   IF NOT done THEN exit; RETURN END;
   res:=fs.make_node(sub,f,node);
   IF res=ok THEN
     usr:=(BITSET(self^.user)*{0..6,8..14}) << 16;
     fs.setattr(f,fs.a_pro,usr+cmask);
     res:=fs.link(sub,f,name,FALSE);
     clo:=fs.close(f);
     IF res=ok THEN res:=clo END
   END;
   clo:=fs.close(sub);
   IF res=ok THEN res:=clo END;
  exit;
  IF res#ok THEN errorn(name,res) END
END fmknode;

PROCEDURE fmount(d: FILE; path: ARRAY OF CHAR;
               VAL device,info: ARRAY OF CHAR;
               VAR       label: ARRAY OF CHAR; ro: BOOLEAN);
  VAR res: INTEGER;
      clo: INTEGER;
      sub: fs.FILE;
      dev: FILE;
     name: str32;
BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  dev:=NIL;
  IF ro       THEN open(dev,device,'r') ELSE open(dev,device,'wr') END;
  IF NOT done THEN RETURN END;
  enter;
   through(d,path,sub,own_read); --++ 26-Jun-90
   IF NOT done THEN res:=error
   ELSE
     res:=fs.mount(sub,name,dev^.file,label,info,label,ro);
     clo:=fs.close(sub);
     IF res=ok THEN res:=clo END
   END;
  exit;
  close(dev);
  IF res#ok THEN errorn(name,res); RETURN END
END fmount;

PROCEDURE funmount(d: FILE; path: ARRAY OF CHAR; method: INTEGER);
  VAR res: INTEGER;
      clo: INTEGER;
      sub: fs.FILE;
     name: str32;
BEGIN
  done:=TRUE;
  splitpathname(path,name);
  IF NOT done THEN RETURN END;
  IF ((path="") & (name="/"0c)) OR (name="..") OR (name="."0c) THEN
    errorn(name,err.bad_name); RETURN
  END;
  done:=TRUE;
  enter;
   through(d,path,sub,own_read);
   IF NOT done THEN exit; RETURN END;
   res:=fs.unmount(sub,name,method);
   clo:=fs.close(sub);
   IF res=ok THEN res:=clo END;
  exit;
  done:=(res=ok);
  IF NOT done THEN errorn(name,res) END
END funmount;

PROCEDURE mount(VAL path,device,info: ARRAY OF CHAR;
                VAR            label: ARRAY OF CHAR; ro: BOOLEAN);
BEGIN fmount(cd,path,device,info,label,ro) END mount;

PROCEDURE unmount(VAL path: ARRAY OF CHAR; method: INTEGER);
BEGIN funmount(cd,path,method) END unmount;

PROCEDURE open(VAR file: FILE; VAL path,mode: ARRAY OF CHAR);
BEGIN
  fopen((cd),file,path,mode)
END open;

PROCEDURE create(VAR f: FILE; VAL path,mode: ARRAY OF CHAR; size: INTEGER);
BEGIN
  fcreate(cd,f,path,mode,size)
END create;

PROCEDURE link(file: FILE; VAL path,mode: ARRAY OF CHAR);
BEGIN
  flink(cd,file,path,mode)
END link;

PROCEDURE unlink(VAL path: ARRAY OF CHAR);
BEGIN
  funlink(cd,path)
END unlink;

PROCEDURE update_buffers(f: FILE); FORWARD;

PROCEDURE flush(f: FILE);
  VAR res: INTEGER;
BEGIN
  IF bad_fd(MAGIC,f)   THEN errorf(f,err.bad_desc); RETURN END;
  enter;
    done:=TRUE;
    IF HIGH(f^.buffs)>=0 THEN update_buffers(f) END;
    IF done THEN
      res:=fs.flush(f^.file);
      IF res#ok THEN errorf(f,res) END
    END;
  exit
END flush;

PROCEDURE _close(VAR f: FILE);
  VAR res: INTEGER;
      clo: INTEGER;
BEGIN
  done:=TRUE;
  IF HIGH(f^.buffs)>=0 THEN update_buffers(f) END;
  IF NOT done THEN RETURN END;
  res:=ok;         ------
  IF f^.dir#fs.null THEN
    IF f^.mode*to_link#{} THEN
      res:=fs.link(f^.dir,f^.file,f^.name,f^.mode*hidden#{});
      done:=(res=ok);
      IF res#ok THEN errorf(f,res); RETURN END;
    END;                            ------
    clo:=fs.close(f^.dir);
    IF res=ok THEN res:=clo END
  END;
  IF f^.file#fs.null THEN
    clo:=fs.close(f^.file);
    IF res=ok THEN res:=clo END
  END;
  IF res#ok THEN errorf(f,res) END
END _close;

PROCEDURE close(VAR f: FILE);
BEGIN
  done:=TRUE;
  IF f=NIL           THEN RETURN END;
  IF bad_fd(MAGIC,f) THEN error:=err.bad_desc; RETURN END;
  enter;
   _close(f);
   IF done THEN release_file(f); f:=NIL END;
  exit
END close;

PROCEDURE purge(VAR f: FILE);
  VAR i: INTEGER;
BEGIN
  done:=TRUE;
  IF f=NIL           THEN RETURN END;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  f^.mode:=f^.mode-to_link;
  FOR i:=0 TO HIGH(f^.buffs) DO DISPOSE(f^.buffs[i].buf) END;
  DISPOSE(f^.buffs);
  close(f)
END purge;

PROCEDURE mkdir(VAL path: ARRAY OF CHAR; hid: BOOLEAN);
BEGIN fmkdir(cd,path,hid) END mkdir;

PROCEDURE mknode(VAL path,node: ARRAY OF CHAR);
BEGIN fmknode(cd,path,node) END mknode;

PROCEDURE mkfs(f: FILE; type,blocksize: INTEGER;
       VAL label: ARRAY OF CHAR; VAL bad: ARRAY OF INTEGER);
  VAR res: INTEGER;
BEGIN
  done:=TRUE;
  IF kind(f)#is_disk THEN errorf(f,err.unsuitable); RETURN END;
  enter;
    IF    type=1 THEN
      res:=fs.make_fs(f^.file,"EXC",label,bad)
    ELSIF type=2 THEN
      res:=fs.make_fs(f^.file,"MSDOS",label,bad)
    ELSE
      res:=err.bad_parm
    END;
  exit;
  IF res#ok THEN errorf(f,res) END
END mkfs;

PROCEDURE seek(f: FILE; offset,origin: INTEGER);
  VAR i: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  IF    origin=0 THEN i:=offset
  ELSIF origin=1 THEN i:=f^.fpos+offset
  ELSIF origin=2 THEN fs.getattr(f^.file,fs.a_eof,i);  i:=i+offset
  ELSE                i:=-1
  END;
  IF i<0 THEN errorf(f,err.bad_parm); RETURN END;
  f^.fpos:=i
END seek;

PROCEDURE pos(f: FILE): INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN 0 END;
  RETURN f^.fpos
END pos;

PROCEDURE eof(f: FILE): INTEGER;
  VAR r: req.REQUEST;
    res: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f)                 THEN errorf(f,err.bad_desc); RETURN 0 END;
  IF fs.state(f^.file)*(fs.special-fs.disk)={} THEN
    fs.getattr(f^.file,fs.a_eof,res);
    RETURN res
  END;
  enter;
    r.op:=req.READY;  r.buf:=NIL;  r.len:=0;  r.pos:=0;  r.ofs:=0;  r.res:=0;
    res:=fs.doio(f^.file,r);
  exit;
  IF res#ok THEN errorf(f,res); RETURN 0 ELSE RETURN r.len END
END eof;

PROCEDURE cut(f: FILE; size: INTEGER);
  VAR res,eof: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f)                 THEN errorf(f,err.bad_desc); RETURN END;
  IF f^.mode*w_pro#{}                THEN errorf(f,err.sec_vio);  RETURN END;
  IF fs.state(f^.file)*fs.special#{} THEN (* empty op *)          RETURN END;
  IF fs.isdir(f^.file)               THEN errorf(f,err.is_dir);   RETURN END;
  enter;
   res:=fs.cut(f^.file,size);
   IF res=ok THEN
     fs.getattr(f^.file,fs.a_eof,eof);
     IF size<eof THEN
       fs.setattr(f^.file,fs.a_eof,size);
       (*
        * f^.file^.eof  :=size;
        * f^.file^.state:=f^.file^.state+fs.dirty (*!!!*)
        *)
     END
   ELSE
     errorf(f,res)
   END;
  exit;
  IF NOT done THEN RETURN END;
  IF fs.state(f^.file)*fs.wait_wr#{} THEN flush(f) END (*!!!*)
END cut;

PROCEDURE extend(f: FILE; size: INTEGER);
  VAR res: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f)                 THEN errorf(f,err.bad_desc); RETURN END;
  IF f^.mode*w_pro#{}                THEN errorf(f,err.sec_vio);  RETURN END;
  IF fs.state(f^.file)*fs.special#{} THEN (* empty op *)          RETURN END;
  IF fs.isdir(f^.file)               THEN errorf(f,err.is_dir);   RETURN END;
  enter;
   res:=fs.extend(f^.file,size);
   IF res#ok THEN errorf(f,res) END;
  exit;
  IF fs.state(f^.file)*fs.wait_wr#{} THEN flush(f) END (*!!!*)
END extend;

PROCEDURE end(f: FILE; size: INTEGER);
  VAR eof: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f)                 THEN errorf(f,err.bad_desc); RETURN END;
  IF f^.mode*w_pro#{}                THEN errorf(f,err.sec_vio);  RETURN END;
  IF fs.state(f^.file)*fs.special#{} THEN (* empty op *)          RETURN END;
  IF fs.isdir(f^.file)               THEN errorf(f,err.is_dir);   RETURN END;
  fs.getattr(f^.file,fs.a_eof,eof);
  IF eof=size THEN RETURN END;
  enter;
   fs.setattr(f^.file,fs.a_eof,size);
  exit;
  IF fs.state(f^.file)*fs.wait_wr#{} THEN flush(f) END (*!!!*)
END end;

PROCEDURE du(d: FILE; VAR free,used: INTEGER);
  VAR res: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,d)       THEN errorf(d,err.bad_desc); RETURN END;
  IF NOT fs.isdir(d^.file) THEN errorf(d,err.not_dir);  RETURN END;
  enter;
   res:=fs.du(d^.file,free,used);
  exit;
  IF res#ok THEN errorf(d,res) END
END du;

PROCEDURE fstype(f: FILE; VAR fstype,blocksize: INTEGER);
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  fstype:=1; blocksize:=4096
END fstype;

PROCEDURE get_attr(f: FILE; a: INTEGER; VAR val: WORD);
  VAR i: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  CASE a OF
    |a_ctime: fs.getattr(f^.file,fs.a_ctime,val)
    |a_wtime: fs.getattr(f^.file,fs.a_wtime,val)
    |a_links: fs.getattr(f^.file,fs.a_links,val)
    |a_inode: fs.getattr(f^.file,fs.a_inode,val)
    |a_gid  : fs.getattr(f^.file,fs.a_pro  ,i); val:=i DIV 10000h   MOD 80h
    |a_uid  : fs.getattr(f^.file,fs.a_pro  ,i); val:=i DIV 1000000h MOD 80h
    |a_pro  : fs.getattr(f^.file,fs.a_pro  ,i); val:=i MOD 10000h
  ELSE
    val:=0; errorf(f,err.bad_parm)
  END
END get_attr;

PROCEDURE set_attr(f: FILE; a: INTEGER; val: WORD);
  VAR pu: BOOLEAN; u,fo: INTEGER;  p: BITSET;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;

  u :=self^.user;
  fs.getattr(f^.file,fs.a_pro,p);
  fo:=INTEGER(p-{0..15})>>16;
  pu:=su(u);
  IF a IN {a_ctime,a_wtime} THEN
    IF NOT pu & (usr(u)#usr(fo)) THEN errorf(f,err.sec_vio); RETURN END
  ELSE
    IF NOT pu THEN errorf(f,err.su_only); RETURN END
  END;
  enter;
   CASE a OF
     |a_ctime: fs.setattr(f^.file,fs.a_ctime,val)
     |a_wtime: fs.setattr(f^.file,fs.a_wtime,val)
   ELSE
     error:=err.bad_parm; done:=FALSE
   END;
--   IF done THEN f^.file^.state:=f^.file^.state+fs.dirty END; (*!!!*)
  exit;
  IF fs.state(f^.file)*fs.wait_wr#{} THEN flush(f) END (* !!! *)
END set_attr;

PROCEDURE kind(f: FILE): BITSET;
  VAR k,m: BITSET;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN {} END;
  k:=fs.state(f^.file)*fs.special;
  IF k=fs.tty  THEN RETURN is_tty  END;
  IF k=fs.disk THEN RETURN is_disk END;
  IF k=fs.spec THEN RETURN is_spec END;
  IF NOT fs.isdir(f^.file)   THEN k:=is_file ELSE k:=is_dir END;
  fs.getattr(f^.file,fs.a_mode,m);
  IF m*fs.sys#{} THEN k:=k+is_sys END;
  RETURN k
END kind;

PROCEDURE is_hidd(f: FILE): BOOLEAN;
  VAR m: BITSET;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN FALSE END;
  fs.getattr(f^.file,fs.a_mode,m);
  RETURN m*hidden#{}
END is_hidd;


PROCEDURE update_buffers(f: FILE);
  VAR i,l: INTEGER;
      eof: INTEGER;
      res: INTEGER;
BEGIN
  done:=TRUE;
  fs.getattr(f^.file,fs.a_eof,eof);
  FOR i:=0 TO HIGH(f^.buffs) DO
    WITH f^.buffs[i] DO
      IF dty THEN
        l :=eof-pos;
        IF l>BYTES(buf) THEN l:=BYTES(buf) END;
        IF l>0 THEN
          res:=fs.write(f^.file,pos,buf^.ADR,0,l);
          IF res#ok THEN ioerror(f,res); RETURN END
        END;
        dty:=FALSE
      END
    END
  END
END update_buffers;

PROCEDURE buffers(f: FILE; no,bytes: INTEGER);

  PROCEDURE undo(VAR b: BUFFERs);
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(b) DO DISPOSE(b[i].buf) END; DISPOSE(b)
  END undo;

  VAR i: INTEGER;
     bs: INTEGER;
    new: BUFFERs;

BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f)     THEN errorf(f,err.bad_desc); RETURN END;
  IF (no<0) OR (bytes<0) THEN errorf(f,err.bad_parm); RETURN END;
  IF fs.state(f^.file)*(fs.tty+fs.spec)#{} THEN RETURN END;

  IF HIGH(f^.buffs)>=0 THEN
    enter; update_buffers(f); exit;
    IF NOT done THEN RETURN END
  END;
  IF bytes=0 THEN no:=0  END;
  fstype(f,i,bs);
  IF NOT done THEN RETURN END;
  bytes:=(bytes+bs-1) DIV bs * bs;
  NEW(new);
  IF no<=0 THEN
    undo(f^.buffs);  f^.buffs^:=new^;  done:=TRUE;  RETURN
  END;
  RESIZE(new,no);
  IF HIGH(new)<no-1 THEN errorf(f,err.no_memory); RETURN END;
  FOR i:=0 TO HIGH(new) DO NEW(new[i].buf) END;
  FOR i:=0 TO HIGH(new) DO
    WITH new[i] DO
      RESIZE(buf,bytes);
      IF BYTES(buf)<bytes THEN
        undo(new); errorf(f,err.no_memory); RETURN
      END;
      pos:=-bytes-1; dty:=FALSE
    END
  END;
  undo(f^.buffs);  f^.buffs^:=new^
END buffers;

PROCEDURE _bfread(f: FILE; adr: ADDRESS; bpos,len: INTEGER);
  VAR i,p,fp,bs,x,max,res,l,eof: INTEGER;        swap: BUFFER;
BEGIN
  x  :=-1;
  fp :=f^.fpos;
  max:=0;
  bs :=BYTES(f^.buffs[0].buf);
  ASSERT((len>0) & (len<=bs));
  FOR i:=0 TO HIGH(f^.buffs) DO
    WITH f^.buffs[i] DO
      IF (fp>=pos) & (fp<pos+bs) THEN
        p:=fp-pos;
        ASSERT(p+len<=bs);
        low.cmove(adr,bpos,buf^.ADR,p,len);
        INC(iolen,len);
        f^.fpos:=fp+len;
        IF i#0 THEN
          swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[i]; f^.buffs[i]:=swap
        END;
        RETURN
      END;
      IF NOT dty & (x<0) OR (ABS((pos+bs DIV 2)-fp)>max) THEN
        x:=i; max:=ABS((pos+bs DIV 2)-fp)
      END
    END
  END;
  IF x<0 THEN x:=0 END;
  WITH f^.buffs[x] DO
    IF dty THEN
      l:=bs;
      fs.getattr(f^.file,fs.a_eof,eof);
      IF l>eof-pos THEN l:=eof-pos END;
      IF l>0 THEN (* l<=0 if file was cuted *)
        res:=fs.write(f^.file,pos,buf^.ADR,0,l);
        IF res#ok THEN ioerror(f,res); RETURN END
      END;
      dty:=FALSE
    END;
    pos:=INTEGER(BITSET(fp)-BITSET(bs-1)); (* fp DIV bs * bs *)
    l  :=bs;
    fs.getattr(f^.file,fs.a_eof,eof);
    IF l>eof-pos THEN l:=eof-pos END;
    res:=fs.read(f^.file,pos,buf^.ADR,0,l);
    IF res#ok THEN ioerror(f,res); RETURN END;
    p:=fp-pos;
    ASSERT(p+len<=bs);
    low.cmove(adr,bpos,buf^.ADR,p,len);
    INC(iolen,len);
    f^.fpos:=fp+len;
    IF x#0 THEN
      swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[x]; f^.buffs[x]:=swap
    END
  END
END _bfread;

PROCEDURE bfread(f: FILE; buf: ADDRESS; bpos,len: INTEGER);
  VAR l,bs: INTEGER;
BEGIN
  bs:=BYTES(f^.buffs[0].buf);
  WHILE len>0 DO
    l:=bs - INTEGER(BITSET(f^.fpos)*BITSET(bs-1)); (* l:=bs-f^.fpos MOD bs; *)
    IF l>len THEN l:=len END;
    _bfread(f,buf,bpos,l);
    IF NOT done THEN RETURN END;
    DEC(len,l); INC(bpos,l)
  END
END bfread;

PROCEDURE _bfwrite(f: FILE; adr: ADDRESS; bpos,len: INTEGER);
  VAR i,p,fp,bs,x,max,res,l,eof: INTEGER;         swap: BUFFER;
BEGIN
  x  :=-1;
  fp :=f^.fpos;
  max:=0;
  bs :=BYTES(f^.buffs[0].buf);
  ASSERT((len>0) & (len<=bs));
  FOR i:=0 TO HIGH(f^.buffs) DO
    WITH f^.buffs[i] DO
      IF (fp>=pos) & (fp<pos+bs) THEN
        p:=fp-pos;
        ASSERT(p+len<=bs);
        low.cmove(buf^.ADR,p,adr,bpos,len);
        INC(iolen,len);
        f^.fpos:=fp+len;  dty:=TRUE;
        IF i#0 THEN
          swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[i]; f^.buffs[i]:=swap
        END;
        RETURN
      END;
      IF NOT dty & (x<0) OR (ABS((pos+bs DIV 2)-fp)>max) THEN
        x:=i; max:=ABS((pos+bs DIV 2)-fp)
      END
    END
  END;
  IF x<0 THEN x:=0 END;

  WITH f^.buffs[x] DO
    IF dty THEN
      l  :=bs;
      fs.getattr(f^.file,fs.a_eof,eof);
      IF l>eof-pos THEN l:=eof-pos END;
      IF l>0 THEN (* l<=0 if file was cuted *)
        res:=fs.write(f^.file,pos,buf^.ADR,0,l);
        IF res#ok THEN ioerror(f,res); RETURN END
      END;
      dty:=FALSE
    END;
    pos:=INTEGER(BITSET(fp)-BITSET(bs-1)); (* fp DIV bs * bs *)
    fs.getattr(f^.file,fs.a_eof,eof);
    IF pos<eof THEN
      l:=bs;
      IF l>eof-pos THEN l:=eof-pos END;
      res:=fs.read(f^.file,pos,buf^.ADR,0,l);
      IF (res#ok) & (res#err.no_data) THEN ioerror(f,res); RETURN END
    END;
    p:=fp-pos;
    ASSERT(p+len<=bs);
    low.cmove(buf^.ADR,p,adr,bpos,len);
    INC(iolen,len);
    dty:=TRUE;
    f^.fpos:=fp+len;
    IF x#0 THEN
      swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[x]; f^.buffs[x]:=swap
    END
  END
END _bfwrite;

PROCEDURE bfwrite(f: FILE; buf: ADDRESS; bpos,len: INTEGER);
  VAR l,bs,eof: INTEGER;
BEGIN
  bs:=BYTES(f^.buffs[0].buf);
  WHILE len>0 DO
    l:=bs - INTEGER(BITSET(f^.fpos)*BITSET(bs-1)); (* l:=bs-f^.fpos MOD bs; *)
    IF l>len THEN l:=len END;
    _bfwrite(f,buf,bpos,l);
    IF NOT done THEN RETURN END;

    fs.getattr(f^.file,fs.a_eof,eof);
    IF eof<f^.fpos THEN fs.setattr(f^.file,fs.a_eof,f^.fpos) END;
    DEC(len,l); INC(bpos,l)
  END
END bfwrite;

PROCEDURE fread(f: FILE; buf: ADDRESS; bpos,len: INTEGER);
  VAR res: INTEGER;
BEGIN
  done:=TRUE; iolen:=0;
  IF bad_fd(MAGIC,f)     THEN errorf(f,err.bad_desc); RETURN END;
  IF f^.mode*r_pro#{}    THEN errorf(f,err.sec_vio);  RETURN END;
  IF bpos<0              THEN errorf(f,err.bad_parm); RETURN END;
  IF len<=0              THEN                         RETURN END;
  enter;
    IF HIGH(f^.buffs)>=0 THEN bfread(f,buf,bpos,len); exit; RETURN END;
    res:=fs.read(f^.file,f^.fpos,buf,bpos,len);
    IF res=ok THEN INC(f^.fpos,len) ELSE ioerror(f,res) END;
  exit;
  iolen:=len
END fread;

PROCEDURE fwrite(f: FILE; buf: ADDRESS; bpos,len: INTEGER);
  VAR res: INTEGER;
BEGIN
  done:=TRUE; iolen:=0;
  IF bad_fd(MAGIC,f)   THEN errorf(f,err.bad_desc);  RETURN END;
  IF f^.mode*w_pro#{}  THEN errorf(f,err.sec_vio);   RETURN END;
  IF bpos<0            THEN errorf(f,err.bad_parm);  RETURN END;
  IF len<=0            THEN                          RETURN END;
  enter;
    IF HIGH(f^.buffs)>=0     THEN bfwrite(f,buf,bpos,len); exit; RETURN END;
    res:=fs.write(f^.file,f^.fpos,buf,bpos,len);
    IF res=ok THEN INC(f^.fpos,len) ELSE ioerror(f,res) END;
  exit;
  iolen:=len
END fwrite;

PROCEDURE read(f: FILE; buf: ADDRESS; len: INTEGER);
BEGIN
  fread(f,buf,0,len);
  IF done & (iolen#len) THEN
    IF io_check THEN HALT(err.no_data) ELSE errorf(f,err.no_data) END
  END
END read;

PROCEDURE write(f: FILE; buf: ADDRESS; len: INTEGER);
BEGIN
  fwrite(f,buf,0,len);
  IF done & (iolen#len) THEN
    IF io_check THEN HALT(err.no_data) ELSE errorf(f,err.no_data) END
  END
END write;

PROCEDURE get(f: FILE; VAR str: ARRAY OF WORD; len: INTEGER);
BEGIN read(f,SYSTEM.ADR(str),len) END get;

PROCEDURE put(f: FILE; VAL str: ARRAY OF WORD; len: INTEGER);
BEGIN write(f,SYSTEM.ADR(str),len) END put;

----------------------------------------------------------------

PROCEDURE getch(f: FILE; VAR ch: CHAR);
  VAR i,fp,bs,eof: INTEGER;    swap: BUFFER;
BEGIN
  done:=TRUE;  iolen:=0;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  IF HIGH(f^.buffs)>=0  THEN
    IF f^.mode*r_pro#{} THEN errorf(f,err.sec_vio); RETURN END;
    fs.getattr(f^.file,fs.a_eof,eof);
    IF f^.fpos>=eof THEN
      IF io_check THEN HALT(err.no_data) ELSE errorf(f,err.no_data) END
    END;
    bs:=BYTES(f^.buffs[0]);
    fp:=f^.fpos;
    FOR i:=0 TO HIGH(f^.buffs) DO
      WITH f^.buffs[i] DO
        IF (fp>=pos) & (fp<pos+bs) THEN
          ch:=buf[fp-pos];  iolen:=1;  f^.fpos:=fp+1;
          IF i#0 THEN
            swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[i]; f^.buffs[i]:=swap
          END;
          RETURN
        END
      END
    END
  END;
  ch:=0c;
  fread(f,SYSTEM.ADR(ch),0,1);
  IF done & (iolen#1) THEN
    IF io_check THEN HALT(err.no_data) ELSE errorf(f,err.no_data) END
  END
END getch;

PROCEDURE putch(f: FILE;  ch: CHAR);
  VAR i,fp,bs,eof: INTEGER; swap: BUFFER;
BEGIN
  done:=TRUE;  iolen:=0;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  IF HIGH(f^.buffs)>=0 THEN
    IF f^.mode*w_pro#{}        THEN errorf(f,err.sec_vio); RETURN END;
    bs:=BYTES(f^.buffs[0]);
    fp:=f^.fpos;
    FOR i:=0 TO HIGH(f^.buffs) DO
      WITH f^.buffs[i] DO
        IF (fp>=pos) & (fp<pos+bs) THEN
          buf[fp-pos]:=ch;  iolen:=1;  f^.fpos:=fp+1;  dty:=TRUE;
          fs.getattr(f^.file,fs.a_eof,eof);
          IF eof<=fp THEN fs.setattr(f^.file,fs.a_eof,fp+1) END;
          IF i#0 THEN
            swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[i]; f^.buffs[i]:=swap
          END;
          RETURN
        END
      END
    END
  END;
  fwrite(f,SYSTEM.ADR(ch),0,1);
  IF done & (iolen#1) THEN
    IF io_check THEN HALT(err.no_data) ELSE errorf(f,err.no_data) END
  END
END putch;

PROCEDURE bfgetstr(f: FILE; VAR str: ARRAY OF CHAR; sp: INTEGER);
  VAR find,stop: BOOLEAN;        ch: CHAR;
    max,res,end: INTEGER;       ptr: POINTER TO ARRAY [0..0] OF CHAR;
  i,p,x,l,fp,bs: INTEGER;      swap: BUFFER;       eof: INTEGER;
BEGIN
  bs  :=BYTES(f^.buffs[0].buf);
  stop:=FALSE;
  REPEAT
    fp:=f^.fpos;
    x:=-1;       max :=0;
    i:=0;        find:=FALSE;
    WHILE NOT find & (i<=HIGH(f^.buffs)) DO
      WITH f^.buffs[i] DO
        find:=(fp>=pos) & (fp<pos+bs);
        IF find THEN
          x:=i
        ELSIF NOT dty & (x<0) OR (ABS((pos+bs DIV 2)-fp)>max) THEN
          x:=i; max:=ABS((pos+bs DIV 2)-fp)
        END
      END;
      INC(i)
    END;
    IF x<0 THEN x:=0 END;
    WITH f^.buffs[x] DO
      IF NOT find THEN
        IF dty THEN
          l:=bs;
          fs.getattr(f^.file,fs.a_eof,eof);
          IF l>eof-pos THEN l:=eof-pos END;
          IF l>0 THEN (* l<=0 if file was cuted *)
            res:=fs.write(f^.file,pos,buf^.ADR,0,l);
            IF res#ok THEN ioerror(f,res); RETURN END
          END;
          dty:=FALSE
        END;
        pos:=INTEGER(BITSET(fp)-BITSET(bs-1)); (* fp DIV bs * bs *)
        l  :=bs;
        fs.getattr(f^.file,fs.a_eof,eof);
        IF l>eof-pos THEN l:=eof-pos END;
        IF l<=0 THEN str[sp]:=0c; RETURN END; (* eof *)
        res:=fs.read(f^.file,pos,buf^.ADR,0,l);
        IF res#ok THEN ioerror(f,res); RETURN END
      END;
    END;
    (* now block in buf: swap it with 0 *)
    IF x#0 THEN
      swap:=f^.buffs[0]; f^.buffs[0]:=f^.buffs[x]; f^.buffs[x]:=swap
    END;
    WITH f^.buffs[0] DO
      p:=fp-pos;
      l:=bs-p;
      fs.getattr(f^.file,fs.a_eof,eof);
      IF l>eof-fp THEN l:=eof-fp END;
      IF sp+l>BYTES(str)   THEN l:=BYTES(str)-sp   END;
      IF l<=0 THEN (* eof or str space exhaused *)
        IF sp<=HIGH(str) THEN (*$<$T-*) str[sp]:=0c (*$>*) END; RETURN
      END;

      i  :=p;
      ptr:=SYSTEM.ADR(buf);
      end:=p+l;
      LOOP (*$<$T- this loop determines the speed of getstr *)
        ch:=ptr^[p];
        IF BITSET(ch=ASCII.NL)+BITSET(ch=ASCII.LF) # {} THEN EXIT END;
        p:=p+1;
        IF p=end THEN EXIT END
      END; (*$>*)
      l:=p-i;
      IF l>0 THEN low.cmove(SYSTEM.ADR(str),sp,ptr,i,l); INC(sp,l) END;
      stop:=(p#end);
      l:=ORD(stop)+l;  INC(f^.fpos,l);  INC(iolen,l)

    END (* WITH *)
  UNTIL stop OR (sp>HIGH(str));
  IF sp<=HIGH(str) THEN (*$<$T-*) str[sp]:=0c (*$>*) END
END bfgetstr;

PROCEDURE getstr(f: FILE; VAR str: ARRAY OF CHAR; sp: INTEGER);
  VAR i,l,L,res,eof: INTEGER;    stop: BOOLEAN;       ch: CHAR;
BEGIN
  done:=TRUE;  iolen:=0;
  IF bad_fd(MAGIC,f)   THEN errorf(f,err.bad_desc);   RETURN END;
  IF f^.mode*r_pro#{}  THEN errorf(f,err.sec_vio);    RETURN END;
  IF sp<0              THEN errorf(f,err.bad_parm);   RETURN END;
  IF sp>=BYTES(str)    THEN                           RETURN END;
  IF fs.state(f^.file)*(fs.tty+fs.spec)#{} THEN L:=1 ELSE L:=64 END;
  enter;
   IF HIGH(f^.buffs)>=0 THEN bfgetstr(f,str,sp); exit; RETURN END;
   stop:=FALSE;                                  ----  ------
   REPEAT
     l:=L;
     fs.getattr(f^.file,fs.a_eof,eof);
     IF l>eof-f^.fpos          THEN l:=eof-f^.fpos    END;
     IF l>BYTES(str)-sp        THEN l:=BYTES(str)-sp  END;
     IF l=0  THEN str[sp]:=0c; exit; RETURN END;
                               ----  ------
     res:=fs.read(f^.file,f^.fpos,SYSTEM.ADR(str),sp,l);
     IF res#ok THEN str[sp]:=0c; ioerror(f,res); exit; RETURN END;
     i:=0;                                       ----  ------
     LOOP
       IF l<=0 THEN EXIT END;
       ch:=str[sp];
       IF (ch=ASCII.NL) OR (ch=ASCII.LF) THEN
         stop:=TRUE; INC(i); (* to skip NL *) EXIT
       END;
       DEC(l); INC(i); INC(sp)
     END;
     INC(iolen,i);  INC(f^.fpos,i);
     stop:=stop OR (sp>=BYTES(str))
   UNTIL stop;
   IF sp<=HIGH(str) THEN str[sp]:=0c END;
  exit
END getstr;

PROCEDURE putstr(f: FILE; VAL str: ARRAY OF CHAR; pos: INTEGER);
  VAR i: INTEGER;
BEGIN
  done:=TRUE; iolen:=0;
  IF pos<0 THEN errorf(f,err.bad_parm); RETURN END;
  i:=pos;                                           (*$<$T-*)
  WHILE (i<=HIGH(str)) & (str[i]#0c) DO INC(i) END; (*$>*)
  IF i=pos THEN RETURN END;
  fwrite(f,SYSTEM.ADR(str),pos,i-pos)
END putstr;

PROCEDURE fmtwrite(f: WORD; VAL str: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  fwrite(f,SYSTEM.ADR(str),pos,len)
END fmtwrite;

PROCEDURE print(f: FILE; VAL format: ARRAY OF CHAR; SEQ args: WORD);
BEGIN
  done:=TRUE;
  fmt.format(f,fmtwrite,format,args)
END print;

----------------------------------------------------------------

PROCEDURE lock(milisec: INTEGER; f: FILE);
  VAR res: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  res:=fs.usrlock(f^.file,milisec);
  IF res#ok THEN errorf(f,res) END
END lock;

PROCEDURE unlock(f: FILE);
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f) THEN errorf(f,err.bad_desc); RETURN END;
  fs.usrunlock(f^.file)
END unlock;

----------------------------------------------------------------

PROCEDURE sort(d: FILE; kind: INTEGER);

  VAR dfwd: BOOLEAN;
      rvrs: BOOLEAN;

  PROCEDURE namesort(L,R: INTEGER);
    VAR t: INTEGER;
        x: POINTER TO fs.dnode;
     swap: BOOLEAN;
    de,dx: BOOLEAN;
    i,j,m: INTEGER;
  BEGIN
    i:=L; j:=R; m:=d^.order[(i+j) DIV 2];
    REPEAT
      x:=SYSTEM.ADR(d^.buf[m]);
      LOOP
        WITH d^.buf[d^.order[i]] DO
          IF dfwd THEN
            de:=(   kind*fs.d_dir#{});
            dx:=(x^.kind*fs.d_dir#{});
            IF de=dx THEN
              IF rvrs THEN swap:=(name<=x^.name) ELSE swap:=(name>=x^.name) END
            ELSE
              swap:=de<dx
            END
          ELSE
            IF rvrs THEN swap:=(name<=x^.name) ELSE swap:=(name>=x^.name) END
          END;
          IF swap THEN EXIT END
        END;
        i:=i+1
      END;
      LOOP
        WITH d^.buf[d^.order[j]] DO
          IF dfwd THEN
            de:=(   kind*fs.d_dir#{});
            dx:=(x^.kind*fs.d_dir#{});
            IF de=dx THEN
              IF rvrs THEN swap:=(x^.name<=name) ELSE swap:=(x^.name>=name) END
            ELSE
              swap:=dx<de
            END
          ELSE
            IF rvrs THEN swap:=(x^.name<=name) ELSE swap:=(x^.name>=name) END
          END;
          IF swap THEN EXIT END
        END;
        j:=j-1
      END;
      IF i<=j THEN
        t:=d^.order[i]; d^.order[i]:=d^.order[j]; d^.order[j]:=t;
        i:=i+1; j:=j-1
      END
    UNTIL i>j;
    IF L<j THEN namesort(L,j) END;
    IF i<R THEN namesort(i,R) END
  END namesort;

BEGIN
  IF BITSET(kind)*BITSET(s_name)={} THEN RETURN END;
  dfwd:=BITSET(kind)*BITSET(s_dirfwd )#{};
  rvrs:=BITSET(kind)*BITSET(s_reverse)#{};
  IF HIGH(d^.buf)<1 THEN RETURN END;
  namesort(0,HIGH(d^.buf))
END sort;

PROCEDURE dir_walk(d: FILE; sort_kind: INTEGER);
  VAR i: INTEGER;
    len: INTEGER;
    res: INTEGER;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,d)       THEN errorf(d,err.bad_desc); RETURN END;
  IF NOT fs.isdir(d^.file) THEN errorf(d,err.not_dir);  RETURN END;
  IF d^.mode*x_pro#{}      THEN errorf(d,err.sec_vio);  RETURN END;
  IF d^.pbuf>=0            THEN errorf(d,err.busy);     RETURN END;
  enter;
    res:=fs.read_dir(d^.file,d^.buf^.ADR,len);
    IF res#ok THEN ioerror(d,res) END;
  exit;
  IF NOT done THEN RETURN END;
  d^.buf^.HIGH:=len-1;
  NEW(d^.order,len);
  IF SYSTEM.ADR(d^.buf)=NIL THEN
    errorf(d,err.no_memory); disposewbuf(d^.buf); RETURN
  END;
  d^.pbuf:=0;
  FOR i:=0 TO HIGH(d^.order) DO d^.order[i]:=i END;
  IF done THEN sort(d,sort_kind) END;
  IF NOT done THEN disposewbuf(d^.buf); DISPOSE(d^.order); d^.pbuf:=-1 END
END dir_walk;

PROCEDURE restart_walk(d: FILE);
BEGIN
  IF bad_fd(MAGIC,d)       THEN errorf(d,err.bad_desc); RETURN END;
  IF NOT fs.isdir(d^.file) THEN errorf(d,err.not_dir);  RETURN END;
  IF d^.pbuf<0             THEN errorf(d,err.inv_op);   RETURN END;
  d^.pbuf:=0; done:=TRUE
END restart_walk;

PROCEDURE get_entry(d: FILE; VAR s   : ARRAY OF CHAR;
                             VAR mode: BITSET): BOOLEAN;
  VAR k: BITSET;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,d) THEN errorf(d,err.bad_desc); RETURN FALSE END;
  IF d^.pbuf<0       THEN errorf(d,err.inv_op);   RETURN FALSE END;
  WHILE d^.pbuf<=HIGH(d^.buf) DO
    WITH d^.buf[d^.order[d^.pbuf]] DO
      INC(d^.pbuf);
      IF (kind*fs.d_entry#{}) & (kind*fs.d_del={}) THEN
        mode:={};
        IF kind*fs.d_dir#{}    THEN mode:=mode+e_dir    END;
        IF kind*fs.d_esc#{}    THEN mode:=mode+e_esc    END;
        IF kind*fs.d_sys#{}    THEN mode:=mode+e_sys    END;
        IF kind*fs.d_hidden#{} THEN mode:=mode+e_hidden END;
        str.copy(s,name);
        RETURN TRUE
      END
    END
  END;
  RETURN FALSE
END get_entry;

PROCEDURE end_walk (d: FILE);
BEGIN
  IF bad_fd(MAGIC,d) THEN errorf(d,err.bad_desc); RETURN END;
  IF d^.pbuf<0       THEN errorf(d,err.inv_op);   RETURN END;
  done:=TRUE;
  disposewbuf(d^.buf);
  DISPOSE(d^.order);
  d^.pbuf:=-1
END end_walk;

--------------------------  PATHs  -----------------------------
                          ---------

PROCEDURE close_paths(VAR pth: PATHs);
  VAR res: INTEGER;
     next: PATHs;
BEGIN
  res:=ok;
  WHILE pth#NIL DO
    next:=pth^.next;
    IF pth^.dir#NIL THEN
      close(pth^.dir);
      IF NOT done THEN res:=error END;
    END;
    DISPOSE(pth^.name); DISPOSE(pth);
    pth:=next
  END;
  IF res#ok THEN errorn(UNKNOWN,res) END
END close_paths;

PROCEDURE open_paths(VAR p: PATHs; VAL paths: ARRAY OF CHAR);
  VAR i,j,s: INTEGER; n,end: PATHs;
BEGIN
  done:=TRUE;
  p:=NIL; end:=NIL;
  i:=0;
  LOOP
    WHILE (i<=HIGH(paths)) & (paths[i]=' ') DO INC(i) END;
    IF (i>HIGH(paths)) OR (paths[i]=0c) THEN EXIT END;
    s:=i;
    WHILE (i<=HIGH(paths)) & (paths[i]#' ') & (paths[i]#0c) DO INC(i) END;
    NEW(n);
    IF n=NIL THEN errorn(paths,err.no_memory); close_paths(p); RETURN END;
    IF end=NIL THEN p:=n ELSE end^.next:=n END;
    NEW(n^.name,i-s+1);
    n^.dir :=NIL;  end:=n;
    n^.next:=NIL;  n^.name[i-s]:=0c;
    FOR j:=0 TO i-s-1 DO n^.name[j]:=paths[s]; INC(s) END
  END
END open_paths;

PROCEDURE get_paths(VAR dirs: PATHs; VAL name: ARRAY OF CHAR);
  VAR paths: STRING;
BEGIN
  done:=TRUE;
  dirs:=NIL;
  env.get_str(name,paths);
  IF NOT env.done THEN errorn(name,env.error); RETURN END;
  open_paths(dirs,paths)
END get_paths;


PROCEDURE lookup(p: PATHs; VAR f: FILE; VAL path,mode: ARRAY OF CHAR);
  VAR d: FILE;
BEGIN
  IF (HIGH(path)>=0) & (path[0]='/') THEN open(f,path,mode); RETURN END;
  done:=TRUE;
  WHILE p#NIL DO
    IF    p^.dir #NIL   THEN d:=p^.dir
    ELSIF p^.name='.'0c THEN d:=cd
    ELSE
      open(p^.dir,p^.name,'r');
      IF NOT done THEN
        IF (error#err.no_entry) & (error#err.sec_vio) THEN RETURN END;
        done:=TRUE; p^.dir:=NIL
      END;
      d:=p^.dir
    END;
    IF d#NIL THEN
      fopen((d),f,path,mode);
      IF done  OR  (error#err.no_entry) & (error#err.sec_vio) THEN RETURN END
    END;
    p:=p^.next
  END;
  IF done THEN errorn(UNKNOWN,err.no_entry) END
END lookup;

----------------------------------------------------------------

PROCEDURE chdir(VAL path: ARRAY OF CHAR);
  VAR f: FILE;
BEGIN
  IF (cd#NIL) & (cd^.pbuf>=0) THEN errorf(cd,err.busy); RETURN END;
  enter;
   open(f,path,"X");
   IF NOT done THEN exit; RETURN END;
   IF NOT fs.isdir(f^.file) THEN
     close(f); errorf(f,err.not_dir); exit; RETURN
   END;
   IF cd#NIL THEN close(cd) END;
   cd:=f;
  exit
END chdir;

PROCEDURE chroot(VAL path: ARRAY OF CHAR);
BEGIN
  done:=FALSE; error:=err.inv_op
END chroot;

PROCEDURE doio(f: FILE; VAR x: ARRAY OF WORD);
  VAR res: INTEGER;
        r: POINTER TO req.REQUEST;
BEGIN
  done:=TRUE;
  IF bad_fd(MAGIC,f)                 THEN errorf(f,err.bad_desc);   RETURN END;
  IF fs.state(f^.file)*fs.special={} THEN errorf(f,err.unsuitable); RETURN END;
  r:=SYSTEM.ADR(x);
  IF    (r^.op=req.READ ) & (f^.mode*r_pro#{}) THEN
    errorf(f,err.sec_vio); RETURN            (*???*)
  ELSIF (r^.op=req.WRITE) & (f^.mode*w_pro#{}) THEN
    errorf(f,err.sec_vio); RETURN            (*???*)
  END;
  r^.res:=ok;
  res:=fs.doio(f^.file,r^);
  IF res#ok THEN errorf(f,res) END
END doio;

PROCEDURE pars_mask(VAL s: ARRAY OF CHAR; VAR set: BITSET): BOOLEAN;

  PROCEDURE tog(pos: INTEGER; bit: BITSET): BOOLEAN;
  BEGIN
    IF    s[pos]='+' THEN set:=set+bit
    ELSIF s[pos]='-' THEN set:=set-bit
    ELSE  RETURN FALSE
    END;
    RETURN TRUE
  END tog;

  VAR i,j: INTEGER;
      bit: BITSET;
BEGIN
  IF HIGH(s)<10 THEN RETURN FALSE END;
  set:={};
  IF NOT tog(0,run_uid)  THEN RETURN FALSE END;
  IF NOT tog(1,run_priv) THEN RETURN FALSE END;
  bit:={0};
  FOR i:=0 TO 2 DO
    FOR j:=2 TO 0 BY -1 DO
      IF NOT tog(i*3+j+2,bit) THEN RETURN FALSE END; bit:=bit<<1
    END;
    bit:=bit<<1
  END;
  set:=set/{0..11};
  RETURN TRUE
END pars_mask;

PROCEDURE init;
  VAR s: STRING;
    res: INTEGER;
      f: fs.FILE;
    tst: F_DESC;
   mask: BITSET;
BEGIN
  (* check that .magic is the first field of FILE descriptor: *)
  ASSERT(SYSTEM.ADR(tst.magic)-SYSTEM.ADR(tst)=0,4Bh);
  ASSERT(SYSTEM.ADR(tst.file )-SYSTEM.ADR(tst)=1,4Bh);
  cmask:=({0..3}+gro_read+gro_search)/{0..11};
  (* nothing for others, {read,search,exec} for group, all for owner *)
  check_p_stack(256);
  IF fs.root()=fs.null THEN res:=fs.open(fs.null,f,"/",{}) END;
  env.get_str(CD,s);
  IF env.done THEN
    done:=(HIGH(s)>=0) & (s[0]='/');
    IF done     THEN open(cd,s,"X") END;
    IF NOT done THEN cd:=NIL        END
  END;
  env.get_str(env.cmask,s);
  IF env.done & pars_mask(s,mask) THEN chcmask(mask) END;
  IF cd=NIL THEN open(cd,'/',"X") END;
  open_paths(here,".")
END init;

PROCEDURE finish;
  VAR i: INTEGER;
    f,n: FILE;
BEGIN
  f:=fset;
  WHILE f#NIL DO
    n:=f^.next;
    IF NOT bad_fd(MAGIC,f) THEN
      (* purge created but not closed files *)
      f^.mode:=f^.mode-to_link;
      FOR i:=0 TO HIGH(f^.buffs) DO DISPOSE(f^.buffs[i].buf) END;
      DISPOSE(f^.buffs);
      _close(f);
      release_file(f)
    END;
    f:=n
  END
END finish;

VAR c: INTEGER;

BEGIN
  done :=TRUE;
  error:=ok;
  null :=NIL;
  cd   :=NIL;
  fset :=NIL;
  ename:="";
  empty:=NIL;
  halt :=0;
  self :=os.self();
  MAGIC:=_MAGIC;
  io_check:=FALSE;
  env.final(finish);
  c:=mem.credit;
  mem.set_credit(2);
    init;
  mem.set_credit(c)
END BIO.
