MODULE cp; (*$U+$2000 Leo 07-Nov-89. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT   key: Keyboard;
IMPORT    fw: fsWalk;
IMPORT   bio: BIO;
IMPORT   err: defErrors;
IMPORT   tty: Terminal;
IMPORT   std: StdIO;
IMPORT   str: Strings;
IMPORT  args: tskArgs;
IMPORT   mem: Heap;
IMPORT   env: tskEnv;
IMPORT   dnv: tskEnv;
IMPORT   tim: Time;
IMPORT   usr: Users;

WITH STORAGE (NEW: mem.allocate;
           RESIZE: mem.reallocate;
          DISPOSE: mem.deallocate);

TYPE
  STR4   = ARRAY [0..  3] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

VAR (* flags *)
  vfy: BOOLEAN; (* verify read after write       *)
  cre: BOOLEAN; (* create new files              *)
  qry: BOOLEAN; (* query when copy               *)
  rm : BOOLEAN; (* remove source after copy      *)
  exi: BOOLEAN; (* only existing at dest dir     *)
  exc: BOOLEAN; (* only not existing there       *)
  ign: BOOLEAN; (* ignore i/o errors             *)
  ctm: BOOLEAN; (* set the same creation time    *)
  wtm: BOOLEAN; (* set the same last write time  *)
  mkd: BOOLEAN; (* make path to dest directory   *)
  mdf: BOOLEAN; (* modified files only           *)
  hid: BOOLEAN; (* make files as hidden          *)
  vis: BOOLEAN; (* make files as visible         *)
  sys: BOOLEAN; (* copy system files too         *)
  dev: BOOLEAN; (* copy devices                  *)
  old: BOOLEAN; (* older versions only           *)
  erl: BOOLEAN; (* early versions only           *)
  own: BOOLEAN; (* retain owner                  *)

  w_aft,w_bef: INTEGER;    user : INTEGER;
  c_aft,c_bef: INTEGER;    group: INTEGER;

VAR (* global state variables *)
      tt: BOOLEAN;
    tree: fw.TREE;
    patt: STR32;
    home: STR256;
   todir: bio.FILE;
  toname: STR256;

     all: BOOLEAN; (* all files at this dir *)
    skip: BOOLEAN; (* skip this directory   *)

   ioerc: INTEGER; (* i/o error counter *)

CONST
  OPEN   = 'open';
  CLOSE  = 'close';
  REWIND = 'seek(0,0)';
  READ   = 'read';
  WRITE  = 'write';

PROCEDURE _error(ec: INTEGER; f: bio.FILE; VAL lname: ARRAY OF CHAR);
  VAR name: ARRAY [0..127] OF CHAR;
BEGIN
  IF f=bio.null THEN tty.perror(ec,'"%s" %%s\n',lname); HALT END;
  bio.fname(f,name);
  IF NOT bio.done THEN str.copy(name,lname) END;
  tty.perror(ec,'"%s" ("%s") %%s\n',name,lname); HALT(ec)
END _error;

PROCEDURE _warning(ec: INTEGER; f: bio.FILE; VAL lname: ARRAY OF CHAR);
  VAR name: ARRAY [0..127] OF CHAR;
BEGIN
  bio.fname(f,name);
  IF NOT bio.done THEN str.copy(name,lname) END;
  tty.perror(ec,'"%s" ("%s") %%s\n',name,lname)
END _warning;

PROCEDURE fw_check(tree: fw.TREE; VAL name: ARRAY OF CHAR);
  VAR ec: INTEGER;
BEGIN
  IF fw.done THEN RETURN END;
  ec:=fw.error;
  IF tree#fw.null THEN _error(ec,fw.dir(tree),name)
  ELSE tty.perror(ec,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END;
  HALT
END fw_check;

PROCEDURE check(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,fmt,args);  HALT(bio.error)
END check;

PROCEDURE checkat(d: bio.FILE; VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR name: ARRAY [0..127] OF CHAR;
BEGIN
  IF bio.done THEN RETURN END;
  bio.fname(d,name);
  IF bio.done THEN tty.print("%s ",name) END;
  tty.perror(bio.error,fmt,args);  HALT
END checkat;

PROCEDURE check_isdir(d: bio.FILE; VAL name: ARRAY OF CHAR);
BEGIN
  IF bio.is_dir*bio.kind(d)={} THEN _error(err.not_dir,d,name) END
END check_isdir;

PROCEDURE _print_df(len: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  IF    toname='/'0c THEN tty.print("/%-*s",len,name)
  ELSIF toname='.'0c THEN tty.print("%-*s",len,name)
  ELSE                    tty.print("%s/%-*s",toname,len,name)
  END;
END _print_df;

VAR info: ARRAY [0..79] OF CHAR;  info_len: INTEGER;

PROCEDURE push_info(VAL name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF    toname='/'0c THEN str.print(info,"/%s",name)
  ELSIF toname='.'0c THEN str.print(info,"%s",name)
  ELSE                    str.print(info,"%s/%s",toname,name)
  END;
  info_len:=str.len(info)
END push_info;

PROCEDURE query(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  IF NOT qry OR NOT tt THEN RETURN TRUE END;
  IF str.len(name)<16 THEN _print_df(16,name) ELSE _print_df(32,name) END;
  tty.print("?");
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR (ch=key.can) OR (c='A') OR (c='I') THEN
      EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can THEN tty.print("^X\n") ELSE tty.print("%c\n",ch) END;
  IF c ='Q'    THEN HALT        END;
  IF c ='I'    THEN skip:=TRUE  END;
  IF ch='A'    THEN qry :=FALSE END;
  IF ch='a'    THEN all :=TRUE  END;
  IF ch=key.can THEN qry:=FALSE; tt:=FALSE END;
  RETURN (c='Y') OR (c='A') OR (ch=key.can);
END query;

CONST op_err = '%s("%s") %%s\n';
     op2_err = '%s("%s") "%s" %%s\n';

PROCEDURE make_through(at: bio.FILE; VAR dir : bio.FILE;
                        i: INTEGER;  VAL path: ARRAY OF CHAR);
  VAR j,h: INTEGER;
     last: BOOLEAN;
     name: STR32;
BEGIN
  j:=0;
  h:=HIGH(path)+1;
  IF i+HIGH(name)<h THEN h:=i+HIGH(name) END;
  WHILE (i<h) & (path[i]#0c) & (path[i]#'/') DO
    name[j]:=path[i]; INC(i); INC(j)
  END;
  name[j]:=0c;
  last:=(i>HIGH(path)) OR (path[i]=0c);
  IF NOT last THEN INC(i) (* skip '/' *) END;
  bio.fopen((at),dir,name,'m');
  IF NOT bio.done & (bio.error#err.no_entry) THEN
    checkat(at,op_err,OPEN,name)
  END;
  IF bio.done THEN
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,dir,name) END;
  ELSE
    bio.fmkdir(at,name,FALSE);     checkat(at,op_err,'mkdir',name);
    bio.fopen((at),dir,name,'m');  checkat(at,op_err,OPEN,name);
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,dir,name) END
  END;
  bio.close(at);  check(op2_err,CLOSE,path,name);
  IF NOT last THEN
    at:=dir; dir:=bio.null; make_through(at,dir,i,path)
  END
END make_through;

PROCEDURE open_or_make(VAR  dir: bio.FILE;
                       VAL path: ARRAY OF CHAR;
                       make,chk: BOOLEAN);
  VAR i: INTEGER;
     at: bio.FILE;
BEGIN
  bio.open(dir,path,'m');
  IF NOT bio.done & NOT chk THEN dir:=bio.null; RETURN END;
  IF NOT make THEN
    check(op2_err,OPEN,path,bio.ename);
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,dir,path) END;
    RETURN
  END;
  IF bio.done THEN check_isdir(dir,path) END;
  dir:=bio.null;
  IF NOT make THEN RETURN END;
  IF path[0]='/' THEN
    bio.open(at,"/",'');  check(op_err,'open',"/");  i:=1
  ELSE
    bio.dup(at,(bio.cd)); checkat(at,op_err,'dup',".");  i:=0
  END;
  make_through(at,dir,i,path)
END open_or_make;

PROCEDURE open_to(VAR to: bio.FILE; make,chk: BOOLEAN);
BEGIN
  IF todir#bio.null THEN to:=todir; toname:=home; RETURN END;
  fw.substitute(tree,home,toname);
  IF toname="" THEN _error(err.bad_name,to,home) END;
  open_or_make(to,toname,make,chk);
END open_to;

PROCEDURE close_to(VAR to: bio.FILE);
BEGIN
  IF todir#bio.null THEN to:=bio.null; RETURN END;
  IF to=bio.null THEN RETURN END;
  bio.close(to); check(op_err,CLOSE ,toname);
END close_to;

VAR BUF: STRING;
    VBF: STRING;

PROCEDURE resize(VAR buf: STRING; size: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF size>128*1024 THEN size:=128*1024 END;
  IF size=0        THEN RETURN         END;
  IF (buf^.HIGH+1>=size) & (buf^.HIGH+1<=size*2) THEN RETURN END;
  RESIZE(buf,size);
  IF mem.done THEN RETURN END;
  (* take_it_easy: *)
  i:=(size+4095) DIV 4096 * 4096;
  REPEAT
    i:=i DIV 2;
    NEW(buf,i);
  UNTIL mem.done OR (i<=4096);
  IF mem.done THEN RETURN END;
  tty.perror(err.no_memory,'%%s\n'); HALT
END resize;

PROCEDURE compare(len: INTEGER): BOOLEAN;
  VAR i: INTEGER;
      b: SYSTEM.ADDRESS;
      v: SYSTEM.ADDRESS;
BEGIN
  i:=len DIV 4;
  b:=BUF^.ADR-1;
  v:=VBF^.ADR-1;
  IF i>0 THEN
    REPEAT INC(b); INC(v); i:=i-1 UNTIL (i=0) OR (b^#v^);
    IF b^#v^ THEN RETURN FALSE END;
  END;
  FOR i:=i*4 TO len-1 DO
    IF BUF[i]#VBF[i] THEN RETURN FALSE END
  END;
  RETURN TRUE
END compare;

PROCEDURE copy(from: bio.FILE; VAR to: bio.FILE; mode: BITSET;
                VAL sname,dname,cmask: ARRAY OF CHAR);

  VAR sf,df: bio.FILE;
     copied: BOOLEAN;
      ct,wt: INTEGER;
        eof: INTEGER;

  PROCEDURE chk(d: bio.FILE; VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
    VAR ec: INTEGER;
      name: ARRAY [0..127] OF CHAR;
  BEGIN
    IF bio.done THEN RETURN END;
    ec:=bio.error;
    bio.fname(d,name);
    IF bio.done THEN tty.print("%s ",name) END;
    tty.perror(ec,fmt,args);
    IF NOT ign THEN HALT END;
    INC(ioerc)
  END chk;

  PROCEDURE chk_len(d: bio.FILE; i,l: INTEGER; VAL op,name: ARRAY OF CHAR);
    VAR done: BOOLEAN;
       error: INTEGER;
       dname: ARRAY [0..127] OF CHAR;
  BEGIN
    IF bio.done & (l=i) THEN RETURN END;
    done:=bio.done;  error:=bio.error;
    IF i=l THEN RETURN END;
    bio.fname(d,dname);
    IF bio.done THEN tty.print("%s ",dname) END;
    IF done THEN tty.perror(err.no_data,op_err,op,name)
    ELSE         tty.perror(error      ,op_err,op,name)
    END;
    IF NOT ign THEN HALT END;
    copied:=FALSE;  INC(ioerc)
  END chk_len;

  PROCEDURE _try_open;
  BEGIN
    IF to=bio.null THEN RETURN END;
    bio.fopen((to),df,dname,cmask);
    IF (NOT bio.done) & (bio.error#err.no_entry) THEN
      checkat(to,op_err,OPEN,dname)
    END;
    IF NOT bio.done THEN df:=bio.null; RETURN END;
    IF bio.is_dir*bio.kind(df)#{} THEN _error(err.is_dir,df,dname) END
  END _try_open;


  PROCEDURE ctim(f: bio.FILE; VAL name: ARRAY OF CHAR): INTEGER;
    VAR t: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_ctime,t); checkat(f,op_err,'get_ctime',name); RETURN t
  END ctim;

  PROCEDURE wtim(f: bio.FILE; VAL name: ARRAY OF CHAR): INTEGER;
    VAR t: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_wtime,t); checkat(f,op_err,'get_wtime',name); RETURN t
  END wtim;

  PROCEDURE _close(unlink,ctime,wtime,own: BOOLEAN; user: INTEGER);
    VAR u,g: INTEGER; p: BITSET;
  BEGIN
    IF ctime THEN
      bio.set_attr(df,bio.a_ctime,ct); checkat(to,op_err,'set_ctime',dname)
    END;
    IF wtime THEN
      bio.set_attr(df,bio.a_wtime,wt); checkat(to,op_err,'set_wtime',dname)
    END;
    IF user>=0 THEN
      bio.chowner(df,user,group); checkat(to,op_err,'chowner',dname)
    END;
    IF own THEN (* retain owner *)
      bio.get_attr(sf,bio.a_uid,u);
      IF NOT bio.done THEN u:=-1; checkat(from,op_err,'get_uid',sname) END;
      bio.get_attr(sf,bio.a_gid,g);
      IF NOT bio.done THEN g:=-1; checkat(from,op_err,'get_uid',sname) END;
      bio.get_attr(sf,bio.a_pro,p);
      IF NOT bio.done THEN p:={0..31}; checkat(from,op_err,'get_pro',sname) END;
      IF (u>=0) & (g>=0) & (p#{0..31}) THEN
        bio.chowner (df,u,g); checkat(to,op_err,'chowner' ,dname);
        bio.chaccess(df,p);   checkat(to,op_err,'chaccess',dname)
      END
    END;
    IF sf#bio.null THEN bio.close(sf); checkat(from,op_err,CLOSE,sname) END;
    IF df#bio.null THEN bio.close(df); checkat(to  ,op_err,CLOSE,dname) END;
    IF unlink & copied THEN
      bio.funlink(from,sname); checkat(from,op_err,'unlink',sname)
    END
  END _close;

  PROCEDURE _purge; BEGIN _close(FALSE,FALSE,FALSE,FALSE,-1) END _purge;

  VAR i,e,len,free,used,prc,prc1: INTEGER;

BEGIN
  bio.fopen((from),sf,sname,'r');
  IF NOT bio.done & ((bio.error=err.no_entry) OR (bio.error=err.sec_vio)) THEN
    _warning(bio.error,sf,sname); RETURN
  END;
  checkat(from,op_err,OPEN,sname);
  IF bio.is_dir*bio.kind(sf)#{} THEN
    _warning(err.is_dir,sf,sname); RETURN
  END;
  copied:=TRUE;

  eof:=bio.eof(sf);
  df :=bio.null;

  IF (exi OR exc ) & (to=bio.null) THEN
    open_to(to,FALSE,FALSE);
    IF exi & (to=bio.null) THEN _purge; RETURN END
  ELSIF NOT cre & (to=bio.null) THEN
    open_to(to,FALSE,FALSE)
  ELSE
    fw.substitute(tree,home,toname);
  END;

  IF exi OR exc OR mdf OR old OR erl OR NOT cre THEN _try_open END;
  IF exc & (df#bio.null)  OR  exi & (df=bio.null) THEN
    _purge; RETURN
  ELSIF exi & cre & NOT mdf THEN
    bio.close(df); df:=bio.null; checkat(to,op_err,CLOSE ,dname)
  END;

  wt:=wtim(sf,sname);
  ct:=ctim(sf,sname);
  IF mdf & (df#bio.null) & (wt=wtim(df,dname)) THEN
    _purge; RETURN
  END;
  IF old & (df#bio.null) & (wt<=wtim(df,dname)) THEN
    _purge; RETURN
  END;
  IF erl & (df#bio.null) & (wt>=wtim(df,dname)) THEN
    _purge; RETURN
  END;
  IF (wt<w_aft) OR (wt>w_bef) OR (ct<c_aft) OR (ct>c_bef) THEN
    _purge; RETURN
  END;
  IF env.ipr() THEN qry:=FALSE; tt:=FALSE END;
  IF NOT all THEN
    IF qry & NOT query(dname) THEN _purge; RETURN END
  END;


  IF tt & (all OR NOT qry) THEN _print_df(0,dname); tty.print("\n") END;

  IF to=bio.null THEN open_to(to,mkd,TRUE) END;

  bio.du(to,free,used);
  IF NOT bio.done THEN _error(bio.error,df,toname) END;

  IF df#bio.null THEN free:=free+bio.eof(df)        END;
  IF eof>free    THEN _error(err.no_space,df,dname) END;
  IF df#bio.null THEN
    IF bio.eof(df)<eof THEN
      bio.extend(df,eof);                  checkat(to,op_err,'extend',dname)
    ELSIF bio.eof(df)>eof THEN
      bio.cut(df,eof);                     checkat(to,op_err,'cut',dname)
    END
  ELSE
    bio.fcreate((to),df,dname,cmask,eof);  checkat(to,op_err,'create',dname)
  END;

  push_info(dname); env.put_str(dnv.info,info,TRUE);
  resize(BUF,eof);
  e:=eof;  prc1:=eof DIV 100;  prc:=0;
  WHILE e>0 DO
    len:=BYTES(BUF);
    IF e<len THEN len:=e END;
    bio.get(sf,BUF,len);  chk_len(from,len,bio.iolen,READ ,sname);
    bio.put(df,BUF,len);  chk_len(to  ,len,bio.iolen,WRITE,dname);
    e:=e-len;
    IF eof>128*1024 THEN
      IF e DIV prc1 # prc THEN prc:=e DIV prc1;
        info[info_len]:=0c; str.append(info," %2d%%",prc);
        env.put_str(dnv.info,info,TRUE)
      END
    END
  END;
  bio.cut(df,bio.pos(df));  checkat(to,op_err,'cut',dname);
  IF NOT vfy OR NOT copied THEN _close(rm,ctm,wtm,own,user); RETURN END;

  IF eof>128*1024 THEN
    info[info_len]:=0c; str.append(info," verify");
    env.put_str(dnv.info,info,TRUE)
  END;
  bio.seek(sf,0,0); chk(from,op_err,REWIND,sname);
  bio.seek(df,0,0); chk(to  ,op_err,REWIND,dname);
  resize(VBF,BUF^.HIGH+1);
  WHILE eof>0 DO
    len:=BYTES(BUF);
    IF len>BYTES(VBF) THEN len:=BYTES(VBF) END;
    IF len>eof        THEN len:=eof END;
    bio.get(sf,BUF,len);   chk_len(from,len,bio.iolen,READ,sname);
    bio.get(df,VBF,len);   chk_len(to  ,len,bio.iolen,READ,dname);
    IF NOT compare(len) THEN
      _error(err.chk_err,to,dname);
      IF ign THEN copied:=FALSE ELSE HALT END
    END;
    eof:=eof-len
  END;
  _close(rm,ctm,wtm,own,user); env.put_str(dnv.info,"",TRUE)
END copy;

PROCEDURE copy_tree(VAL pat: ARRAY OF CHAR);
  VAR to: bio.FILE;      do: BOOLEAN;
    mode: BITSET;     cmask: STR4;
   sname: STR32;      dname: STR32;
BEGIN
  fw.walk(tree,pat,TRUE);
  fw_check(tree,pat);
  WHILE fw.next_dir(tree) DO
    fw_check(tree,pat);
    to:=bio.null;
    all :=NOT qry;
    skip:=FALSE;

    fw.substitute(tree,home,toname);
    IF toname="" THEN
      to:=bio.null
    ELSE
      bio.open(to,toname,'m');
      IF NOT bio.done THEN to:=bio.null END
    END;
    WHILE NOT skip & fw.next_entry(tree,sname,mode) DO
      fw_check(tree,sname);
      do :=  (mode*(bio.e_dir+bio.e_sys+bio.e_esc)={}) OR
       dev & (mode* bio.e_esc=bio.e_esc) OR
       sys & (mode*(bio.e_sys+bio.e_dir)=bio.e_sys);
      IF do THEN
        IF vfy THEN cmask:='mc' ELSE cmask:='w' END;
        IF NOT vis & (hid OR (mode*bio.e_hidden#{})) THEN
          str.app(cmask,'h')
        END;
        IF patt="" THEN
          str.copy(dname,sname)
        ELSE
          fw.substitute(tree,patt,dname);
          IF dname="" THEN _error(err.bad_name,bio.null,patt) END;
        END;
        copy(fw.dir(tree),to,mode,sname,dname,cmask)
      END
    END;
    fw_check(tree,pat);
    close_to(to)
  END;
  fw_check(tree,pat);
  fw.dispose(tree);
END copy_tree;

PROCEDURE isconst(VAL s: ARRAY OF CHAR): BOOLEAN;

  PROCEDURE err; BEGIN std.print("illegal use of $ sign\n"); HALT END err;

  VAR i: INTEGER;    ch,c: CHAR;

BEGIN
  FOR i:=0 TO HIGH(s) DO
    ch:=s[i];
    IF ch=0c  THEN RETURN TRUE END;
    IF ch='$' THEN
      IF (i=0) OR (s[i-1]#'\') THEN
        IF i+1>HIGH(s) THEN err END;
        c:=s[i+1];
        IF (c#'@') & (c#'.') & NOT (ORD(c)-ORD("0") IN {0..9}) THEN
          err
        END;
        RETURN FALSE
      END
    END
  END;
  RETURN TRUE;
END isconst;

PROCEDURE set_home(VAL path: ARRAY OF CHAR);
  VAR d: bio.FILE;
      l: INTEGER;
    app: BOOLEAN;
   hisc: BOOLEAN;
   pisc: BOOLEAN;
   prim: STR256;
BEGIN
  l:=str.len(path);
  IF l+3>=HIGH(prim) THEN std.print("too long arg\n"); HALT END;
  str.copy(prim,path);
  app:=(l>0) & (prim[l-1]='/');
  IF app THEN str.app(prim,"$.") END;
  str.copy(home,prim);
  bio.splitpathname(home,patt); check(op_err,'splitpath',home);
  IF home="" THEN home:="." END;
  hisc:=isconst(home);
  pisc:=isconst(patt);
  todir:=bio.null;
  IF mkd THEN
    IF hisc & pisc THEN
      str.copy(home,prim); patt:=""; toname:=home;
      open_or_make(todir,home,FALSE,FALSE)
    END;
    RETURN
  END;
  IF NOT hisc THEN RETURN END;
  toname:=home;
  open_or_make(todir,home,FALSE,FALSE);
  IF (todir=bio.null) OR NOT pisc THEN RETURN END;
  bio.fopen((todir),d,patt,'m');
  IF NOT bio.done THEN RETURN END;
  IF bio.is_dir*bio.kind(d)#{} THEN
    bio.close(todir); todir:=d;
    home:=prim; patt:=""; toname:=home; RETURN
  END;
  bio.close(d)
END set_home;

CONST MAGIC = "cpRv0.0";

TYPE
  HEADER = RECORD
             magic: ARRAY [0..7]  OF CHAR;
             name : ARRAY [0..31] OF CHAR;
             no   : INTEGER;
             pos  : INTEGER;
             len  : INTEGER;
             eof  : INTEGER;
           END;

CONST
  spacer =
    "\r                                                                    \r";

PROCEDURE rollout(fi,fo: bio.FILE; VAL iname,oname: ARRAY OF CHAR);

  VAR  i: INTEGER;
      no: INTEGER;
     len: INTEGER;
     LEN: INTEGER;
    next: INTEGER;
    tail: INTEGER;
  header: HEADER;

  PROCEDURE ichk;
  BEGIN
    IF bio.done THEN RETURN END;
    INC(ioerc);
    IF ign THEN _warning(bio.error,fi,iname)
    ELSE        _error  (bio.error,fi,iname)
    END
  END ichk;

  PROCEDURE back;
  BEGIN
    bio.seek(fi,header.pos,0);  check('"%s" seek error\n',iname)
  END back;

BEGIN
  resize(BUF,16*1024);
  IF vfy THEN
    resize(VBF,BYTES(BUF));
    IF BYTES(VBF)#BYTES(BUF) THEN
      tty.print("no memory for verification buffer\n"); vfy:=FALSE
    END
  END;
  header.magic:=MAGIC;
  header.eof  :=bio.eof(fi);
  str.copy(BUF,iname);
  bio.splitpathname(BUF,header.name);
  check('bad name "%s"\n',iname);
  no:=0;  tail:=bio.eof(fi);
  LOOP
    LOOP
      IF tail=0 THEN RETURN END;
      tty.print("insert disk #%d and strike any key when ready...",no);
      key.swallow;
      key.drop;
      tty.print("\n");
      LEN:=bio.eof(fo)-4096;
      IF tail<LEN THEN LEN:=tail END;
      len:=LEN;
      bio.seek(fo,2048,0);
      IF NOT bio.done THEN _warning(bio.error,fo,oname); EXIT END;
      header.pos:=bio.pos(fi);
      header.len:=LEN;
      header.no :=no;
      bio.put(fo,header,BYTES(header));
      IF NOT bio.done THEN _warning(bio.error,fo,oname); EXIT END;
      bio.seek(fo,4096,0);
      IF NOT bio.done THEN _warning(bio.error,fo,oname); EXIT END;
      next:=0;
      WHILE len>0 DO
        IF len>BYTES(BUF) THEN i:=BYTES(BUF) ELSE i:=len END;
        bio.get(fi,BUF,i); ichk;
        bio.put(fo,BUF,i);
        IF NOT bio.done THEN
          INC(ioerc);
          IF ign THEN _warning(bio.error,fo,oname)
          ELSE        back; EXIT
          END
        END;
        IF bio.pos(fo)>next THEN
          tty.set_reverse(1); tty.print('w'); tty.set_reverse(0);
          next:=next+LEN DIV 50
        END;
        DEC(len,i)
      END;
      IF vfy THEN
        tty.print(spacer);
        bio.seek(fo,4096,0);
        IF NOT bio.done THEN _warning(bio.error,fo,oname); back; EXIT END;
        back;
        len:=LEN;
        next:=0;
        WHILE len>0 DO
          IF len>BYTES(BUF) THEN i:=BYTES(BUF) ELSE i:=len END;
          bio.get(fi,BUF,i); ichk;
          bio.get(fo,VBF,i);
          IF NOT bio.done THEN
            INC(ioerc);
            IF ign THEN _warning(bio.error,fo,oname)
            ELSE        back; EXIT
            END
          END;
          IF NOT compare(i) THEN
            tty.print('verify error\n');
            IF NOT ign THEN back; EXIT END
          END;
          IF bio.pos(fo)>next THEN
            tty.set_reverse(1); tty.print('v'); tty.set_reverse(0);
            next:=next+LEN DIV 50
          END;
          DEC(len,i)
        END
      END;
      tty.print(spacer);
      INC(no);  DEC(tail,LEN);  EXIT
    END
  END
END rollout;

PROCEDURE rollin(fi,fo: bio.FILE; VAL iname,oname: ARRAY OF CHAR);

  VAR  i: INTEGER;
      no: INTEGER;
     len: INTEGER;
     LEN: INTEGER;
    opos: INTEGER;
    next: INTEGER;
   first: BOOLEAN;
  header: HEADER;
  main  : HEADER;

  PROCEDURE ichk;
  BEGIN
    IF bio.done THEN RETURN END;
    INC(ioerc);
    IF ign THEN _warning(bio.error,fi,iname)
    ELSE        _error  (bio.error,fi,iname)
    END
  END ichk;

  PROCEDURE back;
  BEGIN
    bio.seek(fo,opos,0); check('"%s" seek error\n',oname)
  END back;

BEGIN
  resize(BUF,16*1024);
  IF vfy THEN
    resize(VBF,BYTES(BUF));
    IF BYTES(VBF)#BYTES(BUF) THEN
      tty.print("no memory for verification buffer\n"); vfy:=FALSE
    END
  END;
  no:=-1;
  LOOP
    LOOP
      IF (no>=0) & (main.eof=0) THEN RETURN END;
      IF no>=0  THEN
        tty.print('insert disk "%s" #%d and strike any key when ready...'
                  ,main.name,no);
      ELSE
        tty.print("insert disk and strike any key when ready...");
      END;
      key.swallow;
      key.drop;
      tty.print(spacer);
      bio.seek(fi,2048,0);
      IF NOT bio.done THEN _warning(bio.error,fi,iname); EXIT END;
      bio.get(fi,header,BYTES(header));
      IF NOT bio.done THEN _warning(bio.error,fi,iname); EXIT END;
      IF header.magic#MAGIC THEN
        tty.print("illegal header\n"); EXIT
      END;
      IF no<0 THEN
        main:=header; no:=0;
        bio.extend(fo,header.eof);
        check('can`t extend file "%s"\n',oname);
      END;
      IF header.name#main.name THEN
        tty.print('illegal name "%s" ("%s" expected)\n',header.name,main.name);
        EXIT
      END;
      IF header.no#no THEN EXIT END;
      tty.print("\n");
      LEN:=header.len;
      len:=LEN;
      bio.seek(fi,4096,0);
      IF NOT bio.done THEN _warning(bio.error,fi,iname); EXIT END;
      opos:=bio.pos(fo);
      next:=0;
      WHILE len>0 DO
        IF len>BYTES(BUF) THEN i:=BYTES(BUF) ELSE i:=len END;
        bio.get(fi,BUF,i); ichk;
        bio.put(fo,BUF,i);
        IF NOT bio.done THEN INC(ioerc);
          IF ign THEN _warning(bio.error,fo,oname)
          ELSE        back; EXIT
          END
        END;
        IF bio.pos(fi)>next THEN
          tty.set_reverse(1); tty.print('r'); tty.set_reverse(0);
          next:=next+main.len DIV 50
        END;
        DEC(len,i)
      END;
      IF vfy THEN
        tty.print(spacer);
        bio.seek(fi,4096,0);
        IF NOT bio.done THEN _warning(bio.error,fi,iname); back; EXIT END;
        back;
        len:=LEN;
        next:=0;
        WHILE len>0 DO
          IF len>BYTES(BUF) THEN i:=BYTES(BUF) ELSE i:=len END;
          bio.get(fi,BUF,i); ichk;
          bio.get(fo,VBF,i);
          IF NOT bio.done THEN
            INC(ioerc);
            IF ign THEN _warning(bio.error,fo,oname)
            ELSE        back; EXIT
            END
          END;
          IF NOT compare(i) THEN
            tty.print('verify error\n');
            IF NOT ign THEN back; EXIT END
          END;
          IF bio.pos(fi)>next THEN
            tty.set_reverse(1); tty.print('v'); tty.set_reverse(0);
            next:=next+main.len DIV 50
          END;
          DEC(len,i)
        END
      END;
      tty.print(spacer);
      INC(no);  DEC(main.eof,LEN);  EXIT
    END
  END
END rollin;

PROCEDURE rollcopy;
  VAR fi,fo: bio.FILE;  mask: ARRAY [0..7] OF CHAR;
BEGIN
  tty.set_cursor(0);
  IF HIGH(args.words)#1 THEN
    tty.print("cp -R file device\n"); HALT(err.bad_parm)
  END;
  bio.open(fi,args.words[0],'rc');
  check('can`t open "%s" for read');
  IF vfy THEN mask:='wr' ELSE mask:='wrcd' END;
  IF bio.kind(fi)*bio.is_disk#{} THEN
    bio.create(fo,args.words[1],mask,0);
    rollin (fi,fo,args.words[0],args.words[1])
  ELSE
    bio.open(fo,args.words[1],mask);
    check('can`t open "%s" for read/write');
    IF bio.kind(fo)*bio.is_disk#{} THEN
      rollout(fi,fo,args.words[0],args.words[1])
    ELSE
      tty.print("cp -R file device\n"); HALT(err.bad_parm)
    END
  END;
  bio.close(fo);
  check('can`t close "%s"\n',args.words[1]);
  bio.close(fi)
END rollcopy;

PROCEDURE pusage;
BEGIN
  std.print(
      '   "cp"   files copy utility program    (c) KRONOS\n'
      'usage:\n'
      '    cp [-qvicmrdoxUbAByzlWCR] [+H|-H] { source_tree } dest_tree [times] [owner]\n'
      'to ".":\n'
      '    cp [-qvicmrdoxUbAByzlWCR] [+H|-H]   source_tree [times] [owner]\n'
           );
  std.print(
      'owner:\n'
      '    [owner=user_name]\n'
      'times:\n'
      '    [after=time] [before=time] [cafter=time] [cbefore=time]\n'
      '    time = [dd/mm/yy,][hh:mm[.s]]  (Europian style)\n'
      '         | [yy#dd#mm,][hh:mm[.s]]  (American style)\n'
           );
  std.print(
      'query:\n'
      '       (y|n|q|a|A|i|^X) = (Yes|No|Quit|all|All|skIp|IPR)\n\n'
      '                                     Leopold, Mar 1 90\n'
           )
END pusage;

PROCEDURE flags;

  CONST max=MAX(INTEGER);

  VAR s: STRING;        bump: ARRAY [0..7] OF CHAR;
      u: usr.USER;

  PROCEDURE date(VAR t: INTEGER; night: BOOLEAN);
  BEGIN
    tim.scan_date(t,s,night);
    IF t>=0 THEN RETURN END;
    std.print("illegal time specification\n"); HALT
  END date;

BEGIN
  cre:=NOT args.flag('-','c');
  vfy:=    args.flag('-','v');
  qry:=NOT args.flag('-','q');
  rm :=    args.flag('-','r');
  mkd:=    args.flag('-','d');
  exi:=    args.flag('-','o');  (* only existing at dest directory *)
  exc:=    args.flag('-','x');  (* exclusevly (only not existing)  *)
  ign:=    args.flag('-','i');  (* ignore errors                   *)
  mdf:=    args.flag('-','b');  (* modified only (backup)          *)
  ctm:=NOT args.flag('-','C');
  wtm:=NOT args.flag('-','W');
  hid:=    args.flag('+','H');
  vis:=    args.flag('-','H');
  sys:=    args.flag('-','y');
  dev:=    args.flag('-','z');
  old:=    args.flag('-','A');
  erl:=    args.flag('-','B');
  own:=    args.flag('-','U');
  IF dev        THEN cre:=FALSE END;
  IF exc & exi  THEN
    std.print("invalid flag's combination -o -x\n"); HALT
  END;
  IF hid & vis  THEN
    std.print("invalid flag's combination -H +H\n"); HALT
  END;
  IF args.flag('-','m')       THEN cre:=FALSE; rm:=TRUE END;
  IF args.flag('-','l')       THEN tt :=qry             END;

  user:=-1; group:=-1;
  IF args.string("owner",s) THEN
    str.copy(u.name,s); usr.find(u);
    IF u.done THEN
      usr.get_user(u);
      IF u.done THEN user:=u.usr; group:=u.gro END
    END
  END;
  IF (user>=0) & own THEN
    std.print("invalid flag's combination -U owner=\n"); HALT
  END;
  IF args.string("after" ,s)  THEN date(w_aft,FALSE) ELSE w_aft:= 0  END;
  IF args.string("before",s)  THEN date(w_bef,TRUE)  ELSE w_bef:=max END;
  IF args.string("cafter" ,s) THEN date(c_aft,FALSE) ELSE c_aft:= 0  END;
  IF args.string("cbefore",s) THEN date(c_bef,TRUE)  ELSE c_bef:=max END;
  IF (c_bef<c_aft) OR (w_bef<w_aft) THEN
    std.print("invalid time range"); HALT
  END
END flags;

PROCEDURE init;
BEGIN
  flags;
  NEW(BUF); (* resize to 0 *)
  NEW(VBF); (* resize to 0 *)
  IF env.ipr() THEN qry:=FALSE; tt:=FALSE END
END init;

VAR i: INTEGER;

BEGIN
  tt:=std.is_tty(std.out);
  init;
  IF (HIGH(args.words)<0) OR (args.flag('-','h')) THEN
    pusage; HALT
  END;
  ioerc:=0;
  IF args.flag('-','R') THEN
    rollcopy
  ELSIF HIGH(args.words)<1 THEN
    set_home(".");
    copy_tree(args.words[0])
  ELSE
    set_home(args.words[HIGH(args.words)]);
    FOR i:=0 TO HIGH(args.words)-1 DO copy_tree(args.words[i]) END
  END;
  IF ioerc#0 THEN std.print("copy: i/o errors total %d\n",ioerc) END
END cp.

(* FLAGS comments *)
  -c    [not create] if file with this name exist on destination
        directory, don't create new file, write a copy into
        existing one.
        (warning: if i/o errors happen some part of file
                  will be "new", part "old", and part "who knows")

  -v    [verify] switch cashe off, for reading and writting files,
        and compare source and destinator after copying.

  -q    [query] don't ask any questions durring copying.

  -r    [remove] unlink source file after successfull copying.
        (note: if flag -i (see) used every copying is "successfull")

  -d    [directory] if destination directory don't exist,
        automaticaly creates new one.

  -o    [only] make copy only if file with equal name exists at
        destination directory. (-o can't be used together with -x)

  -x    [exclusively] make copy only if file with equal name don't exists
        at destination directory. (-x can't be used together with -o)

  -i    [ignore] ignore read & write errors.

  -b    [backup] copy only the files that don't exist at destination
        directory or have different (not equal) write-time.

  -A    [backup time after] copy only the files that don't exist
        at destination directory or was writen later (write-time great)
        then existing ones.

  -B    [backup time before] copy only the files that don't exist
        at destination directory or was writen earlier (write-time less)
        then existing ones.

  +H    [hidden]     makes copied files hidden.
  -H    [not hidden] makes copied files visible.

  -z    [device] copy devices (-c flags added automaticaly).
  -y    [system] copy system files.

  -m    [move]    equal to -r -c combination.
  -l    [silence] don't inform about files copying.

  -C    [create-time] set creation time "now", otherwise it retains
        old value.

  -W    [write-time]  set modification time "now", otherwise it retains
        old value.

  -U    [user] retain owner and protection from source, otherwise
        set CMASK (see BIO) and owner=user.

  -R    [roll] roll copy to device
