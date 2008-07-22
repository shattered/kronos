MODULE rm; (* Leo 07-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;        IMPORT  std: StdIO;
IMPORT  fw : fsWalk;           IMPORT  tim: Time;
IMPORT  bio: BIO;              IMPORT  tty: Terminal;
IMPORT  key: Keyboard;         IMPORT  tsk: tskEnv;
IMPORT  arg: tskArgs;          IMPORT  str: Strings;

VAR (* flags *)
  dirs: BOOLEAN;
  qry : BOOLEAN;
  vrbs: BOOLEAN;
  TREE: BOOLEAN;

  all : BOOLEAN;
  skip: BOOLEAN;
  tchk: BOOLEAN;
  sys : BOOLEAN;
  dev : BOOLEAN;

 w_aft: INTEGER;
 w_bef: INTEGER;
 c_aft: INTEGER;
 c_bef: INTEGER;

TYPE
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

VAR dname: STR256;

CONST op_err = '%s("%s") %%s\n';

PROCEDURE fw_error(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.error#err.bad_parm THEN
    tty.perror(fw.error,'"%s" %%s\n',name)
  ELSE
    tty.perror(fw.error,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END
END fw_error;

PROCEDURE bio_error(VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'"%s" %%s\n',name)
END bio_error;

PROCEDURE check(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,fmt,args); HALT(bio.error)
END check;

PROCEDURE print(len: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  IF    dname='/'0c THEN tty.print("/%-*s",len,name)
  ELSIF dname='.'0c THEN tty.print("%-*s",len,name)
  ELSE                   tty.print("%s/%-*s",dname,len,name)
  END;
END print;

PROCEDURE ALL;
  VAR ch,c: CHAR;
BEGIN
  tty.print("All files at All directories; are you realy sure?");
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='N') THEN qry:=TRUE ; EXIT END;
    IF (c='Y') THEN qry:=FALSE; EXIT END;
    key.bell(1)
  END;
  tty.print("%c\n",ch);
END ALL;

PROCEDURE query(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  IF all THEN
    IF vrbs THEN print(str.len(name),name); tty.print("\n") END;
    RETURN TRUE
  END;
  IF str.len(name)<16 THEN print(16,name) ELSE print(32,name) END;
  tty.print("?");
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR
       (c='A') OR (c='I') OR (ch=key.can) THEN
      EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can  THEN tty.print("^X\n") ELSE tty.print("%c\n",ch) END;
  IF   c='Q'    THEN HALT       END;
  IF   c='I'    THEN skip:=TRUE END;
  IF  ch='A'    THEN ALL        END;
  IF (ch=key.can) & qry THEN
    tty.print("remove stopped; available only with -q flag\n"); HALT
  END;
  IF ch=key.can  THEN vrbs:=FALSE END;
  IF  ch='a'    THEN all :=TRUE  END;
  RETURN (c='Y') OR (c='A') OR (ch=key.can);
END query;

PROCEDURE out_of_time(f: bio.FILE; VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR t: INTEGER;
BEGIN
  bio.get_attr(f,bio.a_wtime,t); check(op_err,'get_wtime',name);
  IF (t<w_aft) OR (t>w_bef) THEN RETURN TRUE END;
  bio.get_attr(f,bio.a_ctime,t); check(op_err,'get_ctime',name);
  IF (t<c_aft) OR (t>c_bef) THEN RETURN TRUE END;
  RETURN FALSE
END out_of_time;

PROCEDURE rm_one(dir: bio.FILE; VAL name: ARRAY OF CHAR);
  VAR f: bio.FILE; pass: BOOLEAN;
BEGIN
  IF dirs THEN
    bio.fopen(dir,f,name,'');
    IF NOT bio.done         THEN RETURN END;
    pass:=(bio.is_dir*bio.kind(f)={}) OR (bio.eof(f)#64);
    IF pass                 THEN bio.close(f); RETURN END;
    IF out_of_time(f,name)  THEN bio.close(f); RETURN END;
    bio.close(f);
    IF NOT bio.done         THEN RETURN END
  ELSIF tchk THEN
    bio.fopen(dir,f,name,'');
    IF NOT bio.done        THEN RETURN END;
    IF out_of_time(f,name) THEN RETURN END;
    bio.close(f);
    IF NOT bio.done        THEN RETURN END
  END;
  IF tsk.ipr() THEN qry:=FALSE; vrbs:=FALSE END;
  IF qry THEN
    IF NOT query(name) THEN RETURN END
  ELSIF vrbs THEN
    print(str.len(name),name); tty.print("\n")
  END;
  bio.funlink(dir,name);
  IF NOT bio.done THEN bio_error(name) END
END rm_one;

PROCEDURE rm_tree(VAL patt: ARRAY OF CHAR);
  VAR dir: bio.FILE;  mode: BITSET;
     tree: fw.TREE;   name: STR32;
      len: INTEGER;
BEGIN
  len:=str.len(patt);
  fw.walk(tree,patt,FALSE);
  IF NOT fw.done THEN fw_error(patt); HALT(fw.error) END;
  WHILE fw.next_dir(tree) DO
    IF NOT fw.done THEN fw_error(patt) END;
    fw.dpath(tree,dname);
    all:=FALSE; skip:=FALSE;
    dir:=fw.dir(tree);
    WHILE fw.next_entry(tree,name,mode) & NOT skip DO
      IF NOT fw.done THEN fw_error(name) END;
      IF (name#"..") & ( TREE OR ( dirs=(bio.e_dir*mode#{}) ) )
       & (sys=(bio.e_sys*mode#{})) & (dev=(bio.e_esc*mode#{}))
      THEN
        rm_one(dir,name)
      END
    END;
    IF NOT fw.done THEN fw_error(patt) END
  END;
  IF NOT fw.done THEN fw_error(patt) END;
  fw.dispose(tree)
END rm_tree;

PROCEDURE flags;
  CONST max = MAX(INTEGER);
  VAR s: STRING;
BEGIN
  dirs:=    arg.flag('-','d');
  qry :=NOT arg.flag('-','q');
  vrbs:=NOT arg.flag('-','l');
  sys :=    arg.flag('-','y');
  dev :=    arg.flag('-','z');
  TREE:=    arg.flag('-','T');
  IF arg.string("after" , s) THEN
    tim.scan_date(w_aft,s,FALSE)
  ELSE w_aft:=0
  END;
  IF arg.string("before", s) THEN
    tim.scan_date(w_bef,s,TRUE)
  ELSE w_bef:=max
  END;
  IF arg.string("cafter" ,s) THEN
    tim.scan_date(c_aft,s,FALSE)
  ELSE c_aft:=0
  END;
  IF arg.string("cbefore",s) THEN
    tim.scan_date(c_bef,s,TRUE)
  ELSE c_bef:=max
  END;
  IF (c_bef<0) OR (c_aft<0) OR (w_bef<0) OR (w_aft<0) THEN
    std.print("illegal time specification\n"); HALT(err.bad_parm)
  END;
  tchk:=(w_aft#0) OR (c_aft#0) OR (w_bef#max) OR (c_bef#max);
  IF (c_bef<c_aft) OR (w_bef<w_aft) THEN
    std.print("invalid time range"); HALT(err.bad_parm)
  END;
  IF tsk.ipr() & qry THEN
    std.print("\nrm: -q key can not be ommited in independent run mode\n");
    HALT;
  END;
  IF tsk.ipr() THEN vrbs:=FALSE END
END flags;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "rm"  remove files tree utility program (c) KRONOS\n'
      'usage:\n'
      '   rm [-qTdyzl] {file_tree} [times]\n'
      'times:\n'
      '    [after=time] [before=time] [cafter=time] [cbefore=time]\n'
      '    time = [dd/mm/yy,][hh:mm[.s]]\n'
      '         | [yy#dd#mm,][hh:mm[.s]]\n');
  std.print(
      'query:\n'
      '       (y|n|q|a|A|i|^X) = (Yes|No|Quit|all|All|skIp|IPR)\n\n'
      '                                    Leopold, Dec 18 89\n')
END pusage;

VAR  i: INTEGER;

BEGIN
  IF (HIGH(arg.words)<0) OR arg.flag('-','h') THEN pusage; HALT END;
  flags;
  FOR i:=0 TO HIGH(arg.words) DO rm_tree(arg.words[i]) END
END rm.
