IMPLEMENTATION MODULE fsWalk; (* Leo 13-Oct-89. (c) KRONOS *)

(*$X+U+*)
(*$N+*)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT  bio: BIO;
IMPORT  mem: Heap;
IMPORT stng: Strings;
IMPORT   re: regExpr;


CONST ok = err.ok;
   MAGIC = 4B4C4157h;

TYPE
  STR32 = ARRAY [0..31] OF CHAR;

  TREE = POINTER TO fHEAD;

  PATT = RECORD
           rexp: re.EXPR;
           str : STRING;
         END;

  DIR =  RECORD
            dir : bio.FILE;
            name: STR32;
            pat : INTEGER;
            pass: INTEGER;
          END;

  PATTs = DYNARR OF PATT;
  DIRs  = DYNARR OF DIR;

  fHEAD = RECORD
            magic: INTEGER;
            root : BOOLEAN;
            only : BOOLEAN;
            tree : BOOLEAN;
            cur  : INTEGER;
            pats : INTEGER;
            patt : PATTs;
            top  : INTEGER;
            stk  : DIRs;
            fn   : STR32;
            fm   : BITSET;
          END;

PROCEDURE extend_patt(VAR pat: PATTs);
  VAR i: INTEGER;
    new: PATTs;
BEGIN
  new^.HIGH:=pat^.HIGH+8;
  mem.ALLOCATE(new^.ADR,SIZE(new));
  IF new^.ADR=NIL THEN
    error:=err.no_memory; done:=FALSE; RETURN
  END;
  new^.HIGH:=pat^.HIGH;
  new:=pat;
  new^.HIGH:=pat^.HIGH+8;
  FOR i:=HIGH(pat)+1 TO HIGH(new) DO
    WITH new[i] DO NEW(str); rexp:=re.null END
  END;
  IF pat^.ADR#NIL THEN mem.DEALLOCATE(pat^.ADR,SIZE(pat)) END;
  pat^:=new^
END extend_patt;

PROCEDURE walk(VAR fs: TREE; VAL p: ARRAY OF CHAR; files: BOOLEAN);

  VAR i: INTEGER;

  PROCEDURE final_slash;
  BEGIN
    IF fs^.pats>HIGH(fs^.patt)  THEN
      extend_patt(fs^.patt);
      IF NOT done THEN RETURN END
    END;
    INC(fs^.pats);
    WITH fs^.patt[fs^.pats-1] DO
      rexp:=re.null;
      str^.ADR:=NIL; str^.HIGH:=-1;
      str^.HIGH:=1;
      mem.ALLOCATE(str^.ADR,SIZE(str));
      IF str^.ADR=NIL THEN
        error:=err.no_memory; done:=FALSE; RETURN
      END;
      str[0]:='*'; str[1]:=0c;
      re.compile(str,rexp);
      IF NOT re.done THEN
        pos:=i+re.epos; error:=re.error; done:=FALSE;
        RETURN
      END
    END
  END final_slash;

  PROCEDURE next_one(): BOOLEAN;
    VAR j,l: INTEGER;
  BEGIN
    IF (i>HIGH(p)) OR (p[i]=0c) THEN RETURN FALSE END;
    IF fs^.pats>HIGH(fs^.patt)  THEN
      extend_patt(fs^.patt);
      IF NOT done THEN RETURN FALSE END
    END;
    INC(fs^.pats);
    WITH fs^.patt[fs^.pats-1] DO
      rexp:=re.null;
      str^.ADR:=NIL; str^.HIGH:=-1;
      IF p[i]="/" THEN -- tree node
        INC(i); pos:=i;  fs^.tree:=TRUE;
        IF (i>HIGH(p)) OR (p[i]=0c) THEN final_slash; RETURN done END;
        RETURN TRUE
      END;
      j:=i;
      WHILE (i<=HIGH(p)) & (p[i]#0c) & (p[i]#"/") DO INC(i) END;
      IF i=j THEN
        error:=err.bad_parm; done:=FALSE; pos:=i; RETURN FALSE
      END;
      str^.HIGH:=i-j;
      mem.ALLOCATE(str^.ADR,SIZE(str));
      IF str^.ADR=NIL THEN error:=err.no_memory; done:=FALSE; RETURN FALSE END;
      l:=0;
      REPEAT str[l]:=p[j]; INC(j); INC(l) UNTIL j=i;
      str[l]:=0c;
      re.compile(str,rexp);
      IF NOT re.done THEN
        pos:=i+re.epos; error:=re.error; done:=FALSE;
        RETURN FALSE
      END;
      IF (i<=HIGH(p)) & (p[i]='/') THEN
        INC(i);
        IF (i>HIGH(p)) OR (p[i]=0c) THEN final_slash; RETURN done END;
        RETURN TRUE
      END;
      RETURN FALSE
    END;
  END next_one;

  PROCEDURE remove;
    VAR e: INTEGER;
  BEGIN
    e:=error; dispose(fs); done:=FALSE; error:=e
  END remove;

  VAR slash: BOOLEAN;

BEGIN
  done:=FALSE;
  i:=0;
  IF (i>HIGH(p)) OR (p[i]=0c) THEN error:=err.bad_parm; pos:=0; RETURN END;
  slash:=(p[i]='/');
  IF slash THEN
    INC(i)
  ELSIF (i+1<=HIGH(p)) & (p[i]='.') & (p[i+1]='/') THEN
    INC(i,2)
  END;
  mem.ALLOCATE(fs,SIZE(fs^));
  IF fs=NIL THEN error:=err.no_memory; RETURN END;
  done:=TRUE;
  fs^.stk^.ADR:=NIL;    fs^.stk^.HIGH:=-1;
  fs^.patt^.ADR:=NIL;   fs^.patt^.HIGH:=-1;
  fs^.magic:=MAGIC;     fs^.only:=files;
  fs^.pats:=0;          fs^.cur :=-1;
  fs^.root:=slash;      fs^.top :=0;
  fs^.fn  :="";         fs^.fm  :={};
  fs^.tree:=FALSE;
  IF (i>0) & (p[i-1]='/') & ((i>HIGH(p)) OR (p[i]=0c)) THEN
    final_slash
  ELSIF (i>HIGH(p)) OR (p[i]=0c)  THEN
    error:=err.bad_parm; pos:=0; done:=FALSE
  ELSE
    WHILE next_one() DO END;
  END;
  IF NOT done THEN remove; RETURN END;
  FOR i:=0 TO fs^.pats-3 DO
    IF HIGH(fs^.patt[i].str)<0 THEN done:=FALSE; error:=err.bad_parm END
  END;
  IF NOT done THEN remove ELSE pos:=0 END
END walk;

PROCEDURE match(fs: TREE; fullpathname: ARRAY OF CHAR; pos: INTEGER): BOOLEAN;
BEGIN  HALT(100h) END match;

PROCEDURE matched_len(fs: TREE; n: CHAR): INTEGER;
BEGIN  HALT(100h) END matched_len;


PROCEDURE matched_pos(fs: TREE; n: CHAR): INTEGER;
BEGIN  HALT(100h) END matched_pos;



PROCEDURE close(fs: TREE);
BEGIN
  WITH fs^.stk[fs^.top-1] DO
    IF dir=bio.null THEN RETURN END;
    bio.close(dir);
    IF bio.done THEN RETURN END;
    error:=bio.error; done:=FALSE
  END
END close;

PROCEDURE bio_error(): BOOLEAN;
BEGIN
  IF bio.done THEN RETURN FALSE END;
  error:=bio.error; done:=FALSE; RETURN TRUE
END bio_error;

PROCEDURE extend_stk(VAR stk: DIRs);
  VAR new: DIRs;
BEGIN
  new^.HIGH:=stk^.HIGH+8;
  mem.ALLOCATE(new^.ADR,SIZE(new));
  IF new^.ADR=NIL THEN
    error:=err.no_memory; done:=FALSE; RETURN
  END;
  new^.HIGH:=stk^.HIGH;
  new:=stk;
  new^.HIGH:=stk^.HIGH+8;
  IF stk^.ADR#NIL THEN mem.DEALLOCATE(stk^.ADR,SIZE(stk)) END;
  stk^:=new^;
END extend_stk;

PROCEDURE push(fs: TREE);
  VAR p: INTEGER;
BEGIN
  WITH fs^ DO
    IF top>HIGH(stk) THEN
      extend_stk(stk);
      IF NOT done THEN RETURN END;
    END;
    IF top>0 THEN
      p:=stk[top-1].pat;
      IF HIGH(patt[p].str)>0 THEN INC(cur) END
    END;
    INC(top);
    WITH stk[top-1] DO dir:=bio.null; name:=""; pat:=cur; pass:=0 END;
    RETURN
  END
END push;

PROCEDURE pop(fs: TREE);
  VAR p: INTEGER;
BEGIN
  WITH fs^ DO
    close(fs);
    DEC(top);
    IF NOT done THEN RETURN END;
    IF top=0 THEN cur:=-1; RETURN END;
    p:=stk[top-1].pat;
    IF HIGH(patt[p].str)>0 THEN DEC(cur) END;
    WITH stk[top] DO dir:=bio.null; name:=""; pat:=-1 END;
  END
END pop;

CONST _exist = 0; _absent=1; _skip=2;

PROCEDURE start(fs: TREE): BOOLEAN;
BEGIN
  fs^.cur:=0;
  push(fs);
  IF NOT done THEN RETURN FALSE END;
  WITH fs^.stk[fs^.top-1] DO
    IF fs^.root THEN bio.open(dir,"/",'X');  name:="/"
    ELSE             bio.dup(dir,bio.cd);    name:="."
    END;
    IF bio_error() THEN RETURN FALSE END;
    bio.dir_walk(dir,bio.s_name+bio.s_dirfwd);
    IF bio_error() THEN RETURN FALSE END;
    IF fs^.cur>=fs^.pats-1 THEN RETURN TRUE END;
    IF NOT next_dir(fs) THEN
      error:=err.no_entry; done:=FALSE; RETURN FALSE
    END;
    RETURN TRUE
  END
END start;

PROCEDURE open_son(fs: TREE; VAL fname: STR32; tree: BOOLEAN): INTEGER;

  (* always execute "push(fs)", never "pop(fs)" *)

  PROCEDURE toskip(): BOOLEAN;
  BEGIN
    RETURN NOT bio.done & ((bio.error=err.no_entry) OR (bio.error=err.sec_vio))
  END toskip;

  VAR cd: bio.FILE;

BEGIN
  cd:=fs^.stk[fs^.top-1].dir;
  push(fs);
  IF NOT done THEN RETURN _absent END;
  WITH fs^.stk[fs^.top-1] DO
    bio.fopen(cd,dir,fname,"X");
    IF toskip()    THEN RETURN _skip   END;
    IF bio_error() THEN RETURN _absent END;
    name:=fname;
    bio.dir_walk(dir,bio.s_name+bio.s_dirfwd);
    IF toskip()    THEN RETURN _skip   END;
    IF bio_error() THEN RETURN _absent END;
    IF tree OR (fs^.cur<fs^.pats-1) THEN
      IF next_dir(fs) THEN RETURN _exist ELSE RETURN _absent END
    END;
    RETURN _exist
  END
END open_son;

PROCEDURE next_son(fs: TREE): INTEGER;
  VAR son: INTEGER;
     mode: BITSET;
    fname: STR32;
BEGIN
  WHILE bio.get_entry(fs^.stk[fs^.top-1].dir,fname,mode) DO
    WITH fs^.stk[fs^.top-1] DO
    WITH fs^.patt[fs^.cur]  DO
      IF bio.e_dir*mode#{} THEN
        IF  (fname="..") & (HIGH(str)>0) & (fname=str)
        OR  (fname#"..") & ((rexp=re.null) OR re.match(rexp,fname,0)) THEN
          son:=open_son(fs,fname,FALSE);
          IF son=_skip THEN pop(fs) ELSE RETURN son END;
        END
      END
    END
    END
  END;
  IF bio_error() THEN RETURN _absent END;
  pop(fs);
  IF NOT done OR (fs^.top=0) THEN RETURN _absent END;
  IF next_dir(fs) THEN RETURN _exist ELSE RETURN _absent END
END next_son;

PROCEDURE next_descendant(fs: TREE): INTEGER;
  VAR son: INTEGER;
     mode: BITSET;
    fname: STR32;
BEGIN
  IF fs^.stk[fs^.top-1].pass=0 THEN
    WHILE bio.get_entry(fs^.stk[fs^.top-1].dir,fname,mode) DO
      IF (bio.e_dir*mode#{}) & (fname#"..") THEN
        son:=open_son(fs,fname,TRUE);
        IF son=_skip THEN pop(fs) ELSE RETURN son END
      END
    END;
    IF bio_error() THEN RETURN _absent END;
    fs^.stk[fs^.top-1].pass:=1;
    bio.restart_walk(fs^.stk[fs^.top-1].dir);
    IF bio_error() THEN RETURN _absent ELSE RETURN _exist END
  END;
  pop(fs);
  IF NOT done OR (fs^.top=0) THEN RETURN _absent END;
  IF next_dir(fs) THEN RETURN _exist ELSE RETURN _absent END
END next_descendant;

PROCEDURE next_dir(fs: TREE): BOOLEAN;
  VAR next: INTEGER;
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF fs^.cur<0 THEN RETURN start(fs) END;
  IF fs^.cur<fs^.pats-1 THEN
    IF HIGH(fs^.patt[fs^.cur].str)>0 THEN
      REPEAT next:=next_son(fs) UNTIL next#_skip
    ELSE
      REPEAT next:=next_descendant(fs) UNTIL next#_skip
    END;
    RETURN (next=_exist)
  ELSE
    pop(fs);
    IF NOT done OR (fs^.top=0) THEN RETURN FALSE END;
    REPEAT next:=next_son(fs)  UNTIL next#_skip;
    RETURN (next=_exist)
  END
END next_dir;

PROCEDURE dir(fs: TREE): bio.FILE;
BEGIN
  IF fs^.top=0 THEN RETURN bio.null END;
  RETURN fs^.stk[fs^.top-1].dir
END dir;

PROCEDURE dpath0(fs: TREE; VAR path: ARRAY OF CHAR; VAR i: INTEGER);
  VAR l,j: INTEGER;
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  ASSERT(HIGH(path)>=1,4Fh);
  i:=0; l:=0;  path:="";
  IF fs^.top=0 THEN RETURN END;
  l:=0;
  LOOP
    WITH fs^.stk[l] DO
      j:=0;
      WHILE (i<HIGH(path)) & (name[j]#0c) DO
        path[i]:=name[j]; INC(i); INC(j);
      END;
      IF (l>=fs^.top-1) OR (i=HIGH(path)) THEN EXIT END;
      IF (name#"/"0c) & (name#"") THEN path[i]:='/'; INC(i) END;
    END;
    l:=l+1;
  END;
  path[i]:=0c;
END dpath0;

PROCEDURE dpath(fs: TREE; VAR path: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN dpath0(fs,path,i) END dpath;

PROCEDURE dname(fs: TREE; VAR fname: ARRAY OF CHAR);
BEGIN
  ASSERT(HIGH(fname)>=1,4Fh);
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  IF fs^.top=0 THEN fname:=""; RETURN END;
  stng.copy(fname,fs^.stk[fs^.top-1].name)
END dname;

PROCEDURE fpath(fs: TREE; VAR path: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  ASSERT(HIGH(path)>=1,4Fh);
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  IF fs^.top=0 THEN path:=""; RETURN END;
  dpath0(fs,path,i);
  IF i>HIGH(path)-1 THEN RETURN END;
  IF    (i=1) & (path[0]='.')   THEN i:=0
  ELSIF (i>0) & (path[i-1]#'/') THEN
    path[i]:='/'; INC(i)
  END;
  WITH fs^ DO
    j:=0;
    WHILE (i<HIGH(path)) & (j<=HIGH(fn)) & (fn[j]#0c) DO
      path[i]:=fn[j]; INC(i); INC(j)
    END;
    path[i]:=0c
  END
END fpath;

PROCEDURE fname(fs: TREE; VAR fname: ARRAY OF CHAR);
BEGIN
  ASSERT(HIGH(fname)>=1,4Fh);
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  IF fs^.top=0 THEN fname:=""; RETURN END;
  stng.copy(fname,fs^.fn)
END fname;

PROCEDURE fmode(fs: TREE; VAR mode: BITSET);
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  mode:=fs^.fm
END fmode;

PROCEDURE next_entry(fs: TREE;
              VAR fname: ARRAY OF CHAR; VAR mode: BITSET): BOOLEAN;
  VAR d: bio.FILE;
    reg: re.EXPR;
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  done:=TRUE;
  IF fs^.top=0 THEN done:=FALSE; error:=err.not_dir; RETURN FALSE END;
  reg:=fs^.patt[fs^.pats-1].rexp;
  ASSERT(reg#re.null,4Fh);
  d:=fs^.stk[fs^.top-1].dir;
  LOOP
    IF NOT bio.get_entry(d,fs^.fn,fs^.fm) THEN
      IF bio_error() THEN END;
      bio.end_walk(d); fs^.fn:=""; fs^.fm:={};
      RETURN FALSE
    ELSIF (fs^.fm*bio.e_dir={}) OR NOT fs^.only THEN
      IF re.match(reg,fs^.fn,0) THEN
        stng.copy(fname,fs^.fn); mode:=fs^.fm; RETURN TRUE
      END
    ELSE
      (* try next one *)
    END
  END
END next_entry;

PROCEDURE substitute(fs: TREE; VAL m: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);

  PROCEDURE sub_find(n: INTEGER; VAR s: STRING; VAR p,l: INTEGER);
    VAR i,j: INTEGER;  add: INTEGER;
  BEGIN
    l:=0; p:=0;
    WITH fs^.patt[fs^.pats-1] DO
      IF fs^.fn#"" THEN
        l:=re.len(rexp,n);
        IF l>0 THEN
          p:=re.pos(rexp,n);
          s^.ADR :=SYSTEM.ADR(fs^.fn);
          s^.HIGH:=HIGH(fs^.fn);
          RETURN
        END
      END
    END;

    l:=0; p:=0;
    FOR i:=1 TO fs^.top-1 DO
      j:=fs^.stk[i].pat-1;
      IF (j>=0) & (j<=HIGH(fs^.patt)) THEN
        WITH fs^.patt[j] DO
          IF rexp#re.null THEN
            l:=re.len(rexp,n);
            IF l>0 THEN
              p:=re.pos(rexp,n);
              s^.ADR :=SYSTEM.ADR(fs^.stk[i].name);
              s^.HIGH:=HIGH(fs^.stk[i].name);
              RETURN
            END
          END
        END
      END
    END;
    l:=0; p:=0
  END sub_find;

  VAR j: INTEGER; first: BOOLEAN;

  PROCEDURE from_stk(t: INTEGER);
    VAR k: INTEGER;
  BEGIN
    WITH fs^.stk[t] DO
      k:=0;  first:=FALSE;
      WHILE (k<=HIGH(name)) & (name[k]#0c) & (j<HIGH(d)) DO
        d[j]:=name[k]; INC(j); INC(k)
      END
    END
  END from_stk;

  VAR i,k,l,p,t: INTEGER;  s: STRING;

BEGIN
  i:=0; j:=0;
  IF j>HIGH(d) THEN RETURN END;
  WHILE (i<=HIGH(m)) & (m[i]#0c) DO
    IF j=HIGH(d) THEN d[j]:=0c; RETURN END;
    IF (m[i]='\') & (i<HIGH(m)) & (m[i+1]='$') THEN
      d[j]:='$'; INC(i,2); INC(j)
    ELSIF (m[i]='$') & (i<HIGH(m)) & (ORD(m[i+1])-ORD("0") IN {0..9}) THEN
      sub_find(ORD(m[i+1])-ORD("0"),s,p,l);
      IF l>0 THEN
        IF j+l> HIGH(d) THEN l:=HIGH(d)-j END;
        IF p+l>=HIGH(s) THEN l:=HIGH(s)-p END;
        FOR k:=p TO p+l-1 DO d[j]:=s[k]; INC(j) END
      END;
      INC(i,2)
    ELSIF (m[i]='$') & (i<HIGH(m)) & (m[i+1]='@') THEN
      first:=TRUE;
      t:=0;
      WHILE (t<=fs^.top-1) & (HIGH(fs^.patt[fs^.stk[t].pat].str)>0) DO
        INC(t)
      END;
      INC(t);
      WHILE (t<=fs^.top-1) & (HIGH(fs^.patt[fs^.stk[t].pat].str)<0) DO
        IF NOT first & (j<HIGH(d)) THEN d[j]:='/'; INC(j) END;
        from_stk(t);
        INC(t)
      END;
      IF first & (j<HIGH(d)) THEN d[j]:='.'; INC(j) END;
      INC(i,2)
    ELSIF (m[i]='$') & (i<HIGH(m)) & (m[i+1]='.') THEN
      WITH fs^ DO
        k:=0;
        WHILE (k<=HIGH(fn)) & (fn[k]#0c) & (j<HIGH(d)) DO
          d[j]:=fn[k]; INC(j); INC(k)
        END
      END; INC(i,2)
    ELSE
      d[j]:=m[i]; INC(i); INC(j)
    END
  END;
  IF j<=HIGH(d) THEN d[j]:=0c END
END substitute;

PROCEDURE restart_walk(fs: TREE);
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  fs^.fn:=""; fs^.fm:={};
  done:=TRUE;
  WHILE fs^.top>0 DO
    pop(fs);
    IF NOT done THEN RETURN END;
  END;
END restart_walk;

PROCEDURE restart_dir(fs: TREE);
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  IF fs^.top=0 THEN done:=FALSE; error:=err.not_dir END;
  bio.restart_walk(fs^.stk[fs^.top-1].dir);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END;
  fs^.fn:=""; fs^.fm:={}
END restart_dir;

PROCEDURE dispose(VAR fs: TREE);
  VAR i: INTEGER;
BEGIN
  ASSERT((fs#NIL) & (fs^.magic=MAGIC),4Fh);
  done:=TRUE;
  restart_walk(fs);
  FOR i:=0 TO fs^.pats-1 DO
    WITH fs^.patt[i] DO
      IF str^.ADR#NIL  THEN mem.DEALLOCATE(str^.ADR,SIZE(str)) END;
      IF rexp#re.null THEN re.dispose(rexp) END
    END
  END;
  fs^.magic:=0;
  mem.DEALLOCATE(fs^.stk^.ADR,SIZE(fs^.stk ));
  mem.DEALLOCATE(fs^.patt^.ADR,SIZE(fs^.patt));
  mem.DEALLOCATE(fs,SIZE(fs^));
  fs:=NIL;
  done:=TRUE;
END dispose;

BEGIN
  null:=NIL; pos:=0; done:=TRUE; error:=ok
END fsWalk.
