MODULE find; (* Leo 07-Nov-89. (c) KRONOS *)
             (* Leg 21-Nov-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  key: Keyboard;
IMPORT   fw: fsWalk;
IMPORT  bio: BIO;
IMPORT  err: defErrors;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT  str: Strings;
IMPORT  arg: tskArgs;
IMPORT  env: tskEnv;
IMPORT  mem: Heap;
IMPORT  tim: Time;
IMPORT  shl: Shell;

WITH STORAGE: mem;

TYPE
  STR4   = ARRAY [0..  3] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

VAR (* flags *)
  dev: BOOLEAN;
  sys: BOOLEAN;
  dir: BOOLEAN;
  qry: BOOLEAN;
  vrb: ARRAY [0..15] OF CHAR;
  com: BOOLEAN;
  tms: BOOLEAN;
  fls: BOOLEAN;
  w_aft,w_bef: INTEGER;    user : INTEGER;
  c_aft,c_bef: INTEGER;    group: INTEGER;

VAR (* global state variables *)
     cmd: DYNARR OF STRING;
    tree: fw.TREE;
     all: BOOLEAN; (* all files at this dir *)
    skip: BOOLEAN; (* skip this directory   *)

CONST
  OPEN   = 'open';
  CLOSE  = 'close';

PROCEDURE _error(ec: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(ec,'"%s" %%s\n',name); HALT;
END _error;

PROCEDURE _warning(ec: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(ec,'"%s" %%s\n',name)
END _warning;

PROCEDURE fw_check(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.done THEN RETURN END;
  IF fw.error#err.bad_parm THEN
    _error(fw.error,name)
  ELSE
    tty.perror(fw.error,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END;
  HALT;
END fw_check;

PROCEDURE check(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,fmt,args);
  HALT;
END check;

PROCEDURE push_info(VAL name: ARRAY OF CHAR);
BEGIN
  env.put_str(env.info,name,TRUE);
END push_info;

PROCEDURE query(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  IF NOT qry THEN
    RETURN TRUE
  END;
  IF str.len(name)<16 THEN tty.print('%-16s',name)
  ELSE tty.print('%-32s',name)
  END;
  tty.print("?");
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR (ch=key.can) OR (c='A') OR (c='I') THEN
      EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can THEN tty.print("^X\n") ELSE tty.print("%c\n",ch) END;
  IF c ='Q'     THEN HALT END;
  IF c ='I'     THEN skip:=TRUE  END;
  IF ch='A'     THEN qry :=FALSE END;
  IF ch='a'     THEN all :=TRUE  END;
  IF ch=key.can THEN qry:=FALSE  END;
  RETURN (c='Y') OR (c='A') OR (ch=key.can);
END query;

CONST op_err = '%s("%s") %%s\n';

PROCEDURE process(VAL nm: ARRAY OF CHAR);
  VAR  cm: STRING;
  j,i,len: INTEGER;

  PROCEDURE app(VAL s: ARRAY OF CHAR; l: INTEGER);
    VAR i,h: INTEGER;
  BEGIN
    h:=HIGH(cm)+1;
    RESIZE(cm,h+l);
    FOR i:=0 TO l-1 DO cm[h+i]:=s[i] END;
  END app;

BEGIN
  len:=str.len(nm);
  NEW(cm);
  FOR i:=0 TO HIGH(cmd) DO
    IF HIGH(cmd[i])<0 THEN app(nm,len) ELSE app(cmd[i],HIGH(cmd[i])+1) END
  END;
  app(''0c,1); push_info(cm);
  IF NOT all THEN
    IF NOT query(cm) THEN DISPOSE(cm); RETURN END
  END;
  IF (vrb # "") AND (all OR NOT qry) THEN tty.print('%s\n',cm) END;
  shl.system(cm,vrb);
  DISPOSE(cm);
END process;

PROCEDURE find;
  VAR ct,wt: INTEGER;
          f: bio.FILE;
       name: ARRAY [0..255] OF CHAR;

  PROCEDURE ctim(f: bio.FILE; VAL name: ARRAY OF CHAR): INTEGER;
    VAR t: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_ctime,t); check(op_err,'get_ctime',name); RETURN t
  END ctim;

  PROCEDURE wtim(f: bio.FILE; VAL name: ARRAY OF CHAR): INTEGER;
    VAR t: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_wtime,t); check(op_err,'get_wtime',name); RETURN t
  END wtim;

BEGIN
  fw.fpath(tree,name);
  IF tms THEN
    bio.open(f,name,'');
    check(op_err,OPEN,name);
    wt:=wtim(f,name);
    ct:=ctim(f,name);
    bio.purge(f);
    IF (wt<w_aft) OR (wt>w_bef) OR (ct<c_aft) OR (ct>c_bef) THEN RETURN END
  END;
  IF env.ipr() THEN IF NOT com THEN HALT END; qry:=FALSE; vrb:="" END;
  IF com THEN process(name) ELSE std.print('%s\n',name) END
END find;

PROCEDURE find_tree(VAL pat: ARRAY OF CHAR);
  VAR do: BOOLEAN;
    name: STR32;
    mode: BITSET;
BEGIN
  fw.walk(tree,pat,FALSE);
  fw_check(pat);
  WHILE fw.next_dir(tree) DO
    all :=NOT qry;
    skip:=FALSE;
    WHILE NOT skip & fw.next_entry(tree,name,mode) DO
      fw_check(name);
      do := (name#'..') & (
            fls & (mode*(bio.e_dir+bio.e_sys+bio.e_esc)={}) OR
            dir & (mode*bio.e_dir#{}) OR
            dev & (mode*bio.e_esc#{}) OR
            sys & (mode*bio.e_sys#{}));
      IF do THEN find END
    END;
  END;
  fw_check(pat);
  fw.dispose(tree);
END find_tree;

PROCEDURE help;
BEGIN
  std.print(
      '  "find"  find & processing files utility program (c) KRONOS\n'
      'usage:\n'
      '   find [-qvfdzy] {file_tree} [times] [cmd=command]\n');
  std.print(
      '  times:\n'
      '      [after=time] [before=time] [cafter=time] [cbefore=time]\n'
      '      time = [dd/mm/yy,][hh:mm[.s]]\n'
      '           | [yy#dd#mm,][hh:mm[.s]]\n');
  std.print(
      '  command:\n'
      '      any string containing some times "%s"\n\n');
  std.print(
      '   -q  no query                -v  no messages & query\n'
      '   -f  no conventional files   -d  no directories\n'
      '   -z  no nodes                -y  no system files\n\n'
      '                                            Leg, Nov 22 90\n')
END help;

PROCEDURE pars(VAL s: ARRAY OF CHAR);
  VAR f,t: INTEGER;
        c: BOOLEAN;

  PROCEDURE app(f,t: INTEGER);
    VAR i,j: INTEGER;
  BEGIN
    i:=HIGH(cmd)+1; RESIZE(cmd,i+1);
    NEW(cmd[i]);
    IF t<f THEN RETURN END;
    NEW(cmd[i],t-f+1);
    FOR j:=f TO t DO cmd[i,j-f]:=s[j] END;
  END app;

BEGIN
  c:=FALSE; f:=0; t:=0;
  NEW(cmd);
  WHILE (t<=HIGH(s)) & (s[t]#0c) DO
    IF s[t]='%' THEN
      IF (t<HIGH(s)) & (s[t+1]='s') THEN
        IF t>f THEN app(f,t-1) END;
        app(1,0); c:=TRUE; INC(t); f:=t+1
      END
    END;
    INC(t)
  END;
  IF t>f THEN app(f,t-1) END;
  IF NOT c THEN
    tty.print('Command string without "%%s"\n'); HALT(1)
  END;
END pars;

PROCEDURE flags; CONST max=MAX(INTEGER);
  VAR s: STRING;

  PROCEDURE date(VAR t: INTEGER; night: BOOLEAN);
  BEGIN
    tim.scan_date(t,s,night);
    IF t>=0 THEN tms:=TRUE; RETURN END;
    std.print("illegal time specification\n"); HALT(1)
  END date;

BEGIN
  qry:=NOT arg.flag('-','q');
  fls:=NOT arg.flag('-','f');
  dir:=NOT arg.flag('-','d');
  sys:=NOT arg.flag('-','y');
  dev:=NOT arg.flag('-','z');
  shl.get_echo(vrb);
  IF arg.flag('-','v') THEN vrb:="" END;
  tms:=FALSE;
  IF arg.string("after"  ,s) THEN date(w_aft,FALSE) ELSE w_aft:= 0  END;
  IF arg.string("before" ,s) THEN date(w_bef,TRUE)  ELSE w_bef:=max END;
  IF arg.string("cafter" ,s) THEN date(c_aft,FALSE) ELSE c_aft:= 0  END;
  IF arg.string("cbefore",s) THEN date(c_bef,TRUE)  ELSE c_bef:=max END;
  IF (c_bef<c_aft) OR (w_bef<w_aft) THEN
    std.print("invalid time range"); HALT
  END;
  com:=arg.string('cmd',s);
  IF com THEN pars(s) END;
END flags;

PROCEDURE init;
BEGIN
  flags;
  IF NOT env.ipr() THEN RETURN END;
  IF NOT com THEN HALT END;
  qry:=FALSE; vrb:="";
END init;

VAR i: INTEGER;

BEGIN
  init;
  IF (HIGH(arg.words)<0) OR arg.flag('-','h') THEN help; HALT END;
  FOR i:=0 TO HIGH(arg.words) DO find_tree(arg.words[i]) END;
END find.
