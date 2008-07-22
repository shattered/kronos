MODULE chmode; (* Leo 27-Dec-1989 (c) KRONOS *)
               (* Leg 28-Mar-1990 (c) KRONOS *)

IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT  bio: BIO;
IMPORT  fw : fsWalk;
IMPORT  arg: tskArgs;
IMPORT  usr: Users;
IMPORT  str: Strings;
IMPORT  err: defErrors;
IMPORT  key: Keyboard;
IMPORT  env: tskEnv;
IMPORT  dnv: tskEnv;

PROCEDURE help;
BEGIN
  std.print('  "chmode"  change file protection utility program  (c) KRONOS\n');
  std.print('run\n');
  std.print('   chmode  tree ["pro="pro0 | {pro1}] [-dqvT] [ "owner="user ]\n');
  std.print('\n');
  std.print('   pro0 = P U R W X R W X R W X \n');
  std.print('   P    = ("P" | "p" | "-" | "+")\n');
  std.print('   U    = ("U" | "u" | "-" | "+")\n');
  std.print('   R    = ("R" | "r" | "-" | "+")\n');
  std.print('   W    = ("W" | "w" | "-" | "+")\n');
  std.print('   X    = ("X" | "x" | "-" | "+")\n');
  std.print('   pro1 = ("o"|"a"|"g") {("+"|"-") ("r"|"w"|"x"|"p"|"u")}\n');
  std.print('\n');
  std.print(
      'query:\n'
      '       (y|n|q|a|A|i|^X) = (Yes|No|Quit|all|All|skIp|IPR)\n\n');
  std.print('                                         Leg, 28-Mar-90\n');
  HALT
END help;

TYPE
  STR16  = ARRAY [0.. 15] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

VAR PRO  : BITSET;
    user : usr.USER;
    dname: STR256;
    chu  : BOOLEAN;
    chpro: BOOLEAN;
    incl : BITSET;
    excl : BITSET;

VAR (* flags *)
  dirs: BOOLEAN;
  qry : BOOLEAN;
  vrbs: BOOLEAN;
  TREE: BOOLEAN;
  all : BOOLEAN;
  skip: BOOLEAN;
  setp: BOOLEAN;

PROCEDURE cap(ch: CHAR): CHAR;
BEGIN
  IF (ch>=141c) & (ch<=172c) THEN RETURN CHAR(ORD(ch)-40b) END;
  IF (ch>=300c) & (ch<=337c) THEN RETURN CHAR(ORD(ch)+40b) END;
  RETURN ch
END cap;

PROCEDURE chk(VAL op,name: ARRAY OF CHAR);
BEGIN
  IF NOT bio.done THEN
    tty.perror(bio.error,'%s("%s"): %%s\n',op,name); HALT(bio.error)
  END;
END chk;

PROCEDURE fw_error(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.error#err.bad_parm THEN
    tty.perror(fw.error,'"%s" %%s\n',name)
  ELSE
    tty.perror(fw.error,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END
END fw_error;

PROCEDURE print(len: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  IF    dname='/'0c THEN tty.print("/%-*s",len,name)
  ELSIF dname='.'0c THEN tty.print("%-*s",len,name)
  ELSE                   tty.print("%s/%-*s",dname,len,name)
  END;
END print;

PROCEDURE push_info(VAL name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF    dname='/'0c THEN str.print(s,"/%s",name)
  ELSIF dname='.'0c THEN str.print(s,"%s",name)
  ELSE                   str.print(s,"%s/%s",dname,name)
  END;
  env.put_str(dnv.info,s,TRUE);
END push_info;

PROCEDURE query(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  IF str.len(name)<16 THEN print(16,name) ELSE print(32,name) END;
  tty.print("?");
  LOOP
    key.read(ch); c:=cap(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR
       (c='A') OR (c='I') OR (ch=key.can) THEN
      EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can THEN tty.print('^X\n') ELSE tty.Write(ch) END;
  IF c ='Q'    THEN HALT END;
  IF c ='I'    THEN skip:=TRUE; RETURN FALSE END;
  IF c ='N'    THEN RETURN FALSE END;
  IF ch='A'    THEN qry:=FALSE END;
  IF ch=key.can THEN qry:=FALSE; vrbs:=FALSE END;
  IF ch='a'    THEN all:=TRUE END;
  RETURN TRUE
END query;

PROCEDURE unpack_pro(VAR s: ARRAY OF CHAR; p: BITSET);
  VAR i,j: INTEGER;
BEGIN
  s:="purwxrwxrwx";
  IF p*bio.run_priv={} THEN s[0]:='-' END;
  IF p*bio.run_uid ={} THEN s[1]:='-' END;
  FOR j:=0 TO 2 DO
    FOR i:=0 TO 2 DO
      IF i IN p THEN s[j*3+2+2-i]:='-' END
    END;
    p:=p>>4
  END
END unpack_pro;

CONST PATT = 'purwxrwxrwx';

PROCEDURE pars_I(VAR pro: BITSET; w: ARRAY OF CHAR);
  VAR i,j: INTEGER;
      set: BITSET;
      run: BITSET;
      len: INTEGER;
      pos: INTEGER;

  PROCEDURE ilg;
  BEGIN
    tty.print('  Illegal specification           : "%s"\n'
              '  must be "%c" or "%c" or "-" or "+":  %*c\n',
                 w,PATT[pos],cap(PATT[pos]),pos+1,'^');
    HALT(err.bad_parm)
  END ilg;

BEGIN
  len:=0; WHILE (len<=HIGH(w)) & (w[len]#0c) DO INC(len) END;
  IF len<11 THEN pos:=len; ilg END;
  set:={0..11}; run:={}; pos:=0;
  IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
    run:=run+bio.run_priv
  ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
  IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+')  THEN
    run:=run+bio.run_uid
  ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      CASE j OF
        |0: IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
              set:=set-bio.own_read
            ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
        |1: IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
              set:=set-bio.own_write
            ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
        |2: IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
              set:=set-bio.own_exec
            ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
      END;
    END;
    set:=set>>4;
  END;
  set:=set<<12;
  pro:=run+set;
END pars_I;

PROCEDURE pars_II(w: ARRAY OF CHAR): BOOLEAN;
  VAR shift,i: INTEGER;
      len    : INTEGER;
      x      : BOOLEAN;

  PROCEDURE set(y: BITSET);
  BEGIN IF x THEN excl:=excl-y; incl:=incl+y
        ELSE      excl:=excl+y; incl:=incl-y END;
  END set;

BEGIN
  len:=str.len(w);
  IF len<3 THEN RETURN FALSE END;
  IF (w[1]#'-') & (w[1]#'+') THEN RETURN FALSE END;
  CASE cap(w[0]) OF
    |'A': shift:=2
    |'G': shift:=1
    |'O': shift:=0
  ELSE RETURN FALSE
  END;
  i:=1;
  WHILE i<len DO
    CASE cap(w[i]) OF
      |'+': x:=TRUE
      |'-': x:=FALSE
      |'R': set(bio.own_read <<(shift*4))
      |'W': set(bio.own_write<<(shift*4))
      |'X': set(bio.own_exec <<(shift*4))
      |'P': set(bio.run_priv)
      |'U': set(bio.run_uid)
    ELSE
      RETURN FALSE
    END;
    INC(i);
  END;
  RETURN TRUE
END pars_II;

PROCEDURE chmode_one(dir: bio.FILE; name: ARRAY OF CHAR);
  VAR f   : bio.FILE; i   : INTEGER;
      ous : INTEGER;  ogr : INTEGER;
      nus : INTEGER;  ngr : INTEGER;
      old : BITSET;   new : BITSET;
      gnew: STR16;    unew: STR16;
      n   : STR16;    U   : usr.USER;

  PROCEDURE get(VAR x: STR16; i: INTEGER);
  BEGIN
    IF U.done THEN str.print(x,"%-5s",U.name)
    ELSE           str.print(x," %03d ",i) END
  END get;

BEGIN
  bio.fopen(dir,f,name,'');       chk('open',name);
  bio.get_attr(f,bio.a_uid,ous);  chk('get_uid',name);
  bio.get_attr(f,bio.a_gid,ogr);  chk('get_gid',name);
  bio.get_attr(f,bio.a_pro,old);
  IF chpro THEN
    IF setp THEN new:=PRO
    ELSE new:=((bio.run_priv+bio.run_uid)        )*(old+incl-excl)+
              ((bio.run_priv+bio.run_uid)/{0..31})*(old-incl+excl);
    END;
  ELSE new:=old
  END;
  IF chu THEN nus:=user.usr; ngr:=user.gro;
  ELSE        nus:=ous;      ngr:=ogr;
  END;
  IF (old=new) & (ous=nus) & (ogr=ngr) THEN
    bio.close(f); chk('close',name); RETURN
  END;
  IF env.ipr() THEN qry:=FALSE; vrbs:=FALSE END;
  IF NOT all & qry THEN
    IF query(name) THEN
      tty.print('\r'); tty.erase_line(0);
    ELSE
      tty.print('\n'); bio.purge(f); chk('purge',name); RETURN
    END;
  END;
  push_info(name);
  IF old#new THEN
    bio.chaccess(f,new);  chk('change_mode',name);
  END;
  IF (ous#nus) OR (ogr#ngr) THEN
    bio.chowner(f,nus,ngr); chk('change_owner',name);
  END;
  bio.close(f); chk('close',name);
  IF env.ipr() THEN RETURN END;
  unpack_pro(n,new);
  U.usr:=nus;  usr.get_user (U); get(unew,U.usr);
  U.gro:=ngr;  usr.get_group(U); get(gnew,U.gro);
  IF env.ipr() THEN RETURN END;
  IF vrbs THEN
    tty.print("|%s|  %s %s  ",n,unew,gnew);
    print(1,name); tty.print('\n');
  END;
END chmode_one;

PROCEDURE chmode_tree(VAL patt: ARRAY OF CHAR);
  VAR dir: bio.FILE;  mode: BITSET;
     tree: fw.TREE;   name: STR32;
      len: INTEGER;
BEGIN
  IF patt='/'0c THEN
    IF dirs THEN dname:='.'; chmode_one(bio.null,patt) END;
    RETURN
  END;
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
      IF (bio.e_esc*mode={}) & (name#"..") &
         (TREE OR ( dirs=(bio.e_dir*mode#{}) ) ) THEN
        chmode_one(dir,name)
      END
    END;
    IF NOT fw.done THEN fw_error(patt) END
  END;
  IF NOT fw.done THEN fw_error(patt) END;
  fw.dispose(tree)
END chmode_tree;

PROCEDURE flags;
  VAR s: STRING;
BEGIN
  dirs:=    arg.flag('-','d');
  qry :=NOT arg.flag('-','q');
  vrbs:=NOT arg.flag('-','v');
  TREE:=    arg.flag('-','T');
  IF env.ipr() & qry THEN
    tty.print("\nrm: -q key can not be ommited in independent run mode\n");
    HALT(err.bad_parm);
  END;
  IF env.ipr() THEN vrbs:=FALSE END
END flags;

VAR x: STRING; i: INTEGER;

BEGIN
  IF (HIGH(arg.words)<0) OR (arg.flag('-','h')) THEN help END;
  incl:={}; excl:={}; PRO:={0..11};
  chpro:=arg.string('pro',x);
  IF chpro THEN setp:=TRUE; pars_I(PRO,x)
  ELSE
    setp:=FALSE; i:=0;
    FOR i:=1 TO HIGH(arg.words) DO
      IF pars_II(arg.words[i]) THEN chpro:=TRUE END;
    END;
  END;
  user.done:=FALSE; chu:=FALSE;
  IF arg.string('owner',x) THEN
    str.copy(user.name,x);
    usr.find(user);
    IF user.done THEN usr.get_user(user); chu:=TRUE;
    ELSE tty.print('No such user: "%s"\n',x); HALT(err.no_entry)
    END;
  END;
  flags;
  IF chpro OR chu THEN
    chmode_tree(arg.words[0])
  ELSE help
  END;
END chmode.
