MODULE ln; (* Leo 07-Nov-89. (c) KRONOS *)
           (* Ned 17-Jan-90. (c) KRONOS *)
           (* Leg 01-Apr-90. (c) KRONOS *)

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
IMPORT   tim: Time;

WITH STORAGE: mem;

TYPE
  STR4   = ARRAY [0..  3] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

VAR (* flags *)
  qry: BOOLEAN; (* query when copy               *)
  exi: BOOLEAN; (* only existing at dest dir     *)
  exc: BOOLEAN; (* only not existing there       *)
  ign: BOOLEAN; (* ignore i/o errors             *)
  hid: BOOLEAN; (* make files as hidden          *)
  vis: BOOLEAN; (* make files as visible         *)
  sys: BOOLEAN; (* link system files too         *)

  w_aft,w_bef: INTEGER;
  c_aft,c_bef: INTEGER;

VAR (* global state variables *)
      tt: BOOLEAN;
    tree: fw.TREE;
    patt: STR32;
    home: STR256;
   todir: bio.FILE;
  toname: STR256;

     all: BOOLEAN; (* all files at this dir *)
    skip: BOOLEAN; (* skip this directory   *)

CONST
  OPEN   = 'open';
  CLOSE  = 'close';

PROCEDURE _error(ec: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(ec,'"%s" %%s\n',name); HALT
END _error;

PROCEDURE fw_check(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.done THEN RETURN END;
  IF fw.error#err.bad_parm THEN
    _error(fw.error,name)
  ELSE
    tty.perror(fw.error,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END;
  HALT
END fw_check;

PROCEDURE check(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,fmt,args); HALT
END check;

PROCEDURE _print_df(len: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  IF    toname='/'0c THEN tty.print("/%-*s",len,name)
  ELSIF toname='.'0c THEN tty.print("%-*s",len,name)
  ELSE                    tty.print("%s/%-*s",toname,len,name)
  END;
END _print_df;

PROCEDURE push_info(VAL name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF    toname='/'0c THEN str.print(s,"/%s",name)
  ELSIF toname='.'0c THEN str.print(s,"%s",name)
  ELSE                    str.print(s,"%s/%s",toname,name)
  END;
  env.put_str(env.info,s,TRUE);
END push_info;

PROCEDURE query(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  IF NOT qry OR NOT tt THEN RETURN TRUE END;
  IF str.len(name)<16 THEN _print_df(16,name) ELSE _print_df(32,name) END;
  tty.print("?");
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR
       (c='A') OR (c='I') OR (ch=key.can) THEN
      EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can THEN tty.print("^X\n") ELSE tty.print("%c\n",ch) END;
  IF c ='Q'    THEN HALT END;
  IF c ='I'    THEN skip:=TRUE  END;
  IF ch='A'    THEN qry :=FALSE END;
  IF ch=key.can THEN qry :=FALSE; tt:=FALSE END;
  IF ch='a'    THEN all :=TRUE  END;
  RETURN (c='Y') OR (c='A') OR (ch=key.can);
END query;

CONST op_err = '%s("%s") %%s\n';

PROCEDURE open_dir(VAR dir: bio.FILE; VAL path: ARRAY OF CHAR);
BEGIN
  bio.open(dir,path,'m');
  check(op_err,OPEN,path);
  IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,path) END;
END open_dir;

PROCEDURE open_to(VAR to: bio.FILE; make: BOOLEAN);
BEGIN
  IF todir#bio.null THEN to:=todir; toname:=home; RETURN END;
  fw.substitute(tree,home,toname);
  IF toname="" THEN _error(err.bad_name,home) END;
  open_dir(to,toname);
END open_to;

PROCEDURE close_to(VAR to: bio.FILE);
BEGIN
  IF todir#bio.null THEN to:=bio.null; RETURN END;
  IF to=bio.null THEN RETURN END;
  bio.close(to); check(op_err,CLOSE ,toname);
END close_to;

PROCEDURE link(from: bio.FILE; VAR to: bio.FILE;
               VAL sname,dname,cmas: ARRAY OF CHAR;
               o_mode: BITSET);

  VAR sf,df: bio.FILE;

  PROCEDURE _try_open;
  BEGIN
    IF to=bio.null THEN RETURN END;
    bio.fopen((to),df,dname,'');
    IF (NOT bio.done) & (bio.error#err.no_entry) THEN
      check(op_err,OPEN,dname)
    END;
    IF NOT bio.done THEN df:=bio.null; RETURN END;
    IF bio.is_dir*bio.kind(df)#{} THEN _error(err.is_dir,dname) END;
  END _try_open;

  PROCEDURE _close;
  BEGIN
    IF sf#bio.null THEN bio.close(sf); check(op_err,CLOSE,sname) END;
  END _close;

  VAR ct,wt: INTEGER;
BEGIN
  bio.fopen((from),sf,sname,''); check(op_err,OPEN,sname);
  IF (bio.is_tty+bio.is_spec)*bio.kind(sf)#{} THEN
    _error(err.unsuitable,sname)
  END;
  IF bio.is_dir*bio.kind(sf)#{} THEN RETURN END;
  IF (exi OR exc) & (to=bio.null) THEN open_to(to,FALSE)
  ELSE
    fw.substitute(tree,home,toname);
  END;
  IF exi OR exc THEN
    _try_open;
    IF exc & (df#bio.null)  OR  exi & (df=bio.null) THEN
      _close; RETURN
    END;
    IF df#bio.null THEN
      bio.close(df); df:=bio.null; check(op_err,CLOSE,dname);
    END;
  END;
  bio.get_attr(sf,bio.a_wtime,wt); check(op_err,'get_wtime',sname);
  bio.get_attr(sf,bio.a_ctime,ct); check(op_err,'get_ctime',sname);
  IF (wt<w_aft) OR (wt>w_bef) OR (ct<c_aft) OR (ct>c_bef) THEN
    _close; RETURN
  END;

  IF env.ipr() THEN qry:=FALSE; tt:=FALSE END;
  IF NOT all THEN
    IF qry & NOT query(dname) THEN _close; RETURN END;
  END;
  IF tt & (all OR NOT qry) THEN _print_df(0,dname); tty.print("\n") END;
  push_info(dname);
  IF to=bio.null THEN open_to(to,TRUE) END;
  bio.flink(to,sf,dname,cmas); check(op_err,'link',sname);
  _close;
END link;

PROCEDURE link_tree(VAL pat: ARRAY OF CHAR);
  VAR to: bio.FILE;
    mode: BITSET;     cmask: STR4;
   sname: STR32;      dname: STR32;
BEGIN
  fw.walk(tree,pat,FALSE);
  fw_check(pat);
  WHILE fw.next_dir(tree) DO
    fw_check(pat);
    to:=bio.null;
    all :=NOT qry;
    skip:=FALSE;

    fw.substitute(tree,home,toname);
    IF toname="" THEN _error(err.bad_name,home) END;
    bio.open(to,toname,'m');
    IF NOT bio.done THEN to:=bio.null END;

    WHILE NOT skip & fw.next_entry(tree,sname,mode) DO
      IF (sname#'..') & (bio.e_esc*mode={}) & (sys=(bio.e_sys*mode#{})) THEN
        fw_check(sname);
        cmask:='';
        IF NOT vis & (hid OR (mode*bio.e_hidden#{})) THEN cmask:='h' END;
        IF patt="" THEN
          str.copy(dname,sname)
        ELSE
          fw.substitute(tree,patt,dname);
          IF dname="" THEN _error(err.bad_name,patt) END;
        END;
        link(fw.dir(tree),to,sname,dname,cmask,mode)
      END
    END;
    fw_check(pat);
    close_to(to)
  END;
  fw_check(pat);
  fw.dispose(tree);
END link_tree;

PROCEDURE isconst(VAL s: ARRAY OF CHAR): BOOLEAN;

  PROCEDURE err; BEGIN std.print("illegal use of $ sign\n"); HALT(1) END err;

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
   hisc: BOOLEAN;
   pisc: BOOLEAN;
   prim: STR256;
BEGIN
  l:=str.len(path);
  IF l+3>=HIGH(prim) THEN std.print("too long arg\n"); HALT(1) END;
  str.copy(prim,path);
  IF (l>0) & (prim[l-1]='/') THEN str.app(prim,"$.") END;
  str.copy(home,prim);
  bio.splitpathname(home,patt); check(op_err,'splitpath',home);
  IF home="" THEN home:="." END;
  hisc:=isconst(home);
  pisc:=isconst(patt);
  todir:=bio.null;
  IF NOT hisc THEN RETURN END;
  toname:=home;
  open_dir(todir,home);
  IF (todir=bio.null) OR NOT pisc THEN RETURN END;
  bio.fopen((todir),d,patt,'m');
  IF NOT bio.done THEN RETURN END;
  IF bio.is_dir*bio.kind(d)#{} THEN
    bio.close(todir); todir:=d;
    home:=prim; patt:=""; toname:=home; RETURN
  END;
  bio.close(d)
END set_home;

PROCEDURE pusage;
BEGIN
  std.print('   "ln"   files link utility program    (c) KRONOS\n'
            'usage:\n'
            '    ln [-qoxy] [+H|-H] { source_tree } dest_tree [times]\n'
            '    ln [-qoxy] [+H|-H]   source_tree [times]\n'
            'times:\n');
  std.print('    [after=time] [before=time] [cafter=time] [cbefore=time]\n'
            '    time = [dd/mm/yy,][hh:mm[.s]]\n'
            '         | [yy#dd#mm,][hh:mm[.s]]\n');
  std.print('query:\n'
            '       (y|n|q|a|A|i|^X) = (Yes|No|Quit|all|All|skIp|IPR)\n\n'
            '                                         Ned, Jan 17 90\n')
END pusage;

PROCEDURE flags;
  VAR s: STRING;      CONST max=MAX(INTEGER);
BEGIN
  qry:=NOT args.flag('-','q');
  exi:=    args.flag('-','o');  (* only existing at dest directory *)
  exc:=    args.flag('-','x');  (* exclusevly (only not existing)  *)
  hid:=    args.flag('+','H');  (* make files as hidden            *)
  vis:=    args.flag('-','H');  (* make files as visible           *)
  sys:=    args.flag('-','y');  (* system files too                *)
  IF exc & exi THEN
    std.print("invalid flag's combination -o -x\n"); HALT(1)
  END;
  IF vis & hid THEN
    std.print("invalid flag's combination +H -H\n"); HALT(1)
  END;
  IF args.string("after" ,s)  THEN
    tim.scan_date(w_aft,s,FALSE)
  ELSE w_aft :=0
  END;
  IF args.string("before",s)  THEN
    tim.scan_date(w_bef,s,TRUE)
  ELSE w_bef:=max
  END;
  IF args.string("cafter" ,s) THEN
    tim.scan_date(c_aft,s,FALSE)
  ELSE c_aft :=0
  END;
  IF args.string("cbefore",s) THEN
    tim.scan_date(c_bef,s,TRUE)
  ELSE c_bef:=max
  END;
  IF (c_bef<0) OR (c_aft<0) OR (w_bef<0) OR (w_aft<0) THEN
    std.print("illegal time specification\n"); HALT(1)
  END;
  IF (c_bef<c_aft) OR (w_bef<w_aft) THEN
    std.print("invalid time range"); HALT(1)
  END;
  IF env.ipr() THEN qry:=FALSE; tt:=FALSE END;
END flags;

VAR i: INTEGER;

BEGIN
  tt  :=std.is_tty(std.out);
  IF HIGH(args.words)<0 THEN pusage; HALT END;
  flags;
  IF HIGH(args.words)<1 THEN
    set_home(".");
    link_tree(args.words[0])
  ELSE
    set_home(args.words[HIGH(args.words)]);
    FOR i:=0 TO HIGH(args.words)-1 DO link_tree(args.words[i]) END
  END
END ln.
