MODULE ls; (* Leo 07-Nov-89. (c) KRONOS *)

IMPORT   fw: fsWalk;           IMPORT  tim: Time;
IMPORT  bio: BIO;              IMPORT  tty: Terminal;
IMPORT  err: defErrors;        IMPORT  std: StdIO;
IMPORT  lex: Lexicon;          IMPORT  str: Strings;
IMPORT  mem: Heap;             IMPORT  usr: Users;
IMPORT  low: lowLevel;         IMPORT  env: tskEnv;
IMPORT args: tskArgs;

WITH STORAGE: mem;

CONST ok = err.ok;

TYPE
  STR4   = ARRAY [0..  3] OF CHAR;
  STR8   = ARRAY [0..  7] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR128 = ARRAY [0..127] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;


VAR (* flags *)
  long: BOOLEAN;
  all : BOOLEAN;
  inv : BOOLEAN;  (* invisible only      *)
  rodr: BOOLEAN;  (* reverse order       *)
  tt  : BOOLEAN;  (* output to terminal  *)
  full: BOOLEAN;  (* full names 1 column *)
  one : BOOLEAN;  (* one column list     *)
  time: BOOLEAN;  (* time sort           *)
  ino : BOOLEAN;  (* inodes              *)
  cre : BOOLEAN;  (* creation time sort  *)
  ltim: BOOLEAN;  (* long time           *)
  user: BOOLEAN;  (* info about user?    *)
  norm: BOOLEAN;  (* normal intensity    *)
  dirs: BOOLEAN;  (* directories only    *)
  fls : BOOLEAN;  (* files only  Leg, 21-Nov-90 *)
  nsrt: BOOLEAN;  (* not sorted          *)

  after,before: INTEGER;

TYPE

  ENTRY = POINTER TO BODY;

  BODY  = RECORD (* 20*4 = 80 bytes *)
            next: ENTRY;
            mode: BITSET;
            name: STR32;
            nlen: INTEGER;
            err : INTEGER;
            uid : INTEGER;
            gid : INTEGER;
            eof : INTEGER;
            pro : BITSET;
            time: INTEGER;
            link: INTEGER;
            inod: INTEGER;
            type: INTEGER;
          END;

  ENTRYs = DYNARR OF ENTRY;

CONST
  MONTHs = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ";

VAR
  dirco: INTEGER;
  filco: INTEGER;
  smvol: INTEGER;

  cyear: INTEGER;

VAR (* this variables declared globaly to decrise P-stack *)
     lng: STR128;
   fname: STR128;
   dname: STR256;
  prefix: STR128;

PROCEDURE fw_error(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.error#err.bad_parm THEN
    tty.perror(fw.error,'"%s" %%s "%s"\n',name,bio.ename)
  ELSE
    tty.perror(fw.error,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END
END fw_error;

PROCEDURE bio_error(VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'"%s" %%s "%s"\n',name,bio.ename)
END bio_error;

PROCEDURE get_attrs(dir: bio.FILE; entry: ENTRY);

  PROCEDURE check_error;
  BEGIN
    IF NOT bio.done THEN entry^.err:=bio.error END;
  END check_error;

  VAR f: bio.FILE;
  xname: STR32;

BEGIN
  WITH entry^ DO
    err:=0;    time:=tim.time();
    uid:=0;    link:=0;
    gid:=0;    inod:=0;
    eof:=0;    pro :={};

    bio.fopen((dir),f,name,"");
   (*
    IF bio.e_esc*mode={} THEN
    ELSE
      str.print(xname,"%s:",name);
      bio.fopen((dir),f,xname,"");
    END;
   *)
    IF NOT bio.done THEN err:=bio.error; RETURN END;
    IF    bio.is_tty *bio.kind(f)#{} THEN type:=1
    ELSIF bio.is_disk*bio.kind(f)#{} THEN type:=2
    ELSIF bio.is_spec*bio.kind(f)#{} THEN type:=3
    ELSE                                  type:=0
    END;
    bio.get_attr(f,bio.a_links,link);  check_error;
    bio.get_attr(f,bio.a_inode,inod);  check_error;
    bio.get_attr(f,bio.a_gid  ,gid );  check_error;
    bio.get_attr(f,bio.a_uid  ,uid );  check_error;
    bio.get_attr(f,bio.a_pro,  pro );  check_error;
    IF cre THEN
      bio.get_attr(f,bio.a_ctime,time)
    ELSE
      bio.get_attr(f,bio.a_wtime,time)
    END;
    check_error;
    IF type IN {0,2} THEN
      eof:=bio.eof(f);                check_error
    END;
    bio.close(f);                     check_error
  END
END get_attrs;

PROCEDURE sort(VAR ents: ENTRYs);

  PROCEDURE names(L,R: INTEGER);
    VAR t,x: ENTRY;
        i,j: INTEGER;
      de,dx: BOOLEAN;
  BEGIN
    i:=L; j:=R; x:=ents[(i+j) DIV 2];
    REPEAT
      LOOP
        WITH ents[i]^ DO
          de:=(bio.e_dir*   mode={});
          dx:=(bio.e_dir*x^.mode={});
          IF NOT ((de<dx) OR (de=dx) & (name<x^.name)) THEN EXIT END
        END;
        i:=i+1
      END;
      LOOP
        WITH ents[j]^ DO
          de:=(bio.e_dir*   mode={});
          dx:=(bio.e_dir*x^.mode={});
          IF NOT ((dx<de) OR (dx=de) & (x^.name<name)) THEN EXIT END
        END;
        j:=j-1
      END;
      IF i<=j THEN
        t:=ents[i]; ents[i]:=ents[j]; ents[j]:=t; i:=i+1; j:=j-1
      END
    UNTIL i>j;
    IF L<j THEN names(L,j) END;
    IF i<R THEN names(i,R) END
  END names;

  PROCEDURE times(L,R: INTEGER);
    VAR t: ENTRY;
    i,j,x: INTEGER;
  BEGIN
    i:=L; j:=R; x:=ents[(i+j) DIV 2]^.time;
    REPEAT
      WHILE ents[i]^.time>x DO i:=i+1 END;
      WHILE x>ents[j]^.time DO j:=j-1 END;
      IF i<=j THEN
        t:=ents[i]; ents[i]:=ents[j]; ents[j]:=t; i:=i+1; j:=j-1
      END
    UNTIL i>j;
    IF L<j THEN times(L,j) END;
    IF i<R THEN times(i,R) END
  END times;

BEGIN
  IF time THEN times(0,HIGH(ents)) ELSE names(0,HIGH(ents)) END
END sort;

PROCEDURE unpack_size(size: INTEGER; ext: BOOLEAN);
  VAR s: ARRAY [0..15] OF CHAR;
BEGIN
  IF ext THEN s:="        " ELSE s:="" END;
  IF size>=1000000 THEN
    str.append(lng,"%2d,%03d,%03d "
               ,size DIV 1000000,size DIV 1000 MOD 1000,size MOD 1000)
  ELSIF size>=1000 THEN
    str.append(lng,"%.*s%3d,%03d ",3,s,size DIV 1000,size MOD 1000)
  ELSE
    str.append(lng,"%.*s%03d ",7,s,size)
  END
END unpack_size;

PROCEDURE ss(n: INTEGER; VAR ss: STR4);
BEGIN
  IF (n=0) OR (n MOD 100=11) THEN ss:="s"; RETURN END;
  IF n MOD 10 = 1 THEN ss:="" ELSE ss:="s" END
END ss;

PROCEDURE print_out(VAL dname: ARRAY OF CHAR; ents: ENTRYs; hco,max: INTEGER);

  VAR i: INTEGER;         len: INTEGER;
    pos: INTEGER;         vol: INTEGER;
    ofs: INTEGER;         fno: INTEGER;
    h,d: BOOLEAN; ssu,ss0,ss1: STR4;
    wdt: INTEGER;        plen: INTEGER;
    mic: INTEGER; -- min color

  PROCEDURE tty_on;
  BEGIN
    IF norm OR NOT tt THEN RETURN END;
    h:=h & (mic<0);
    IF d THEN tty.set_color(+1) END;
    IF h THEN tty.set_color(-1) END
  END tty_on;

  PROCEDURE tty_off;
  BEGIN
    IF norm OR NOT tt THEN RETURN END;
    h:=(h & (mic<0)) OR d;
    IF h THEN tty.set_color(0) END
  END tty_off;

  PROCEDURE f_one;
  BEGIN
    tty_on;
    IF str.len(fname)>=wdt THEN fname[wdt-1]:=0c END;
    std.print("%s",fname);
    tty_off;
    std.print("\n");
  END f_one;

  PROCEDURE unpack_pro(type: INTEGER; p: BITSET);
    VAR i,j: INTEGER;
  BEGIN
    lng:="purwxrwxrwx ";
    CASE type OF
      |1: lng[0]:='T'; lng[1]:='T'
      |2: lng[0]:='D'; lng[1]:='K'
      |3: lng[0]:='S'; lng[1]:='P'
    ELSE
      IF p*bio.run_priv={} THEN lng[0]:='-' END;
      IF p*bio.run_uid ={} THEN lng[1]:='-' END
    END;
    FOR j:=0 TO 2 DO
      FOR i:=0 TO 2 DO
        IF i IN p THEN lng[j*3+2+2-i]:='-' END
      END;
      p:=p>>4
    END
  END unpack_pro;

  PROCEDURE l_one;
    VAR mon: ARRAY [0..3] OF CHAR;
      y,m,d: INTEGER;
     h,mn,s: INTEGER;
          u: usr.USER;
  BEGIN
    WITH ents[i]^ DO
      IF err#ok THEN
        lex.perror(lng,err," *** (%%|32.32s) ***   ");
        std.print("%s",lng);
        tty_on; std.print("%s",fname); tty_off; std.print("\n"); RETURN
      END;
      unpack_pro(type,pro);
      str.append(lng,"%2d ",link);
      IF user THEN
        u.usr:=uid; usr.get_user(u);
        IF u.done THEN str.append(lng,"%-5s ",u.name)
        ELSE           str.append(lng," %03d  ",uid)
        END;
        u.gro:=gid; usr.get_group(u);
        IF u.done THEN str.append(lng,"%-5s ",u.name)
        ELSE           str.append(lng," %03d  ",gid)
        END
      END;
      IF name#".." THEN
        INC(fno);
        IF mode*bio.e_esc={} THEN vol:=vol+eof END
      END;
      unpack_size(eof,TRUE);
      tim.unpack(time,y,m,d,h,mn,s);
      str.sub_arr(mon,MONTHs,(m-1)*4,3);
      IF ltim THEN
        str.append(lng," %s %2d %02d  %2d:%02d.%02d ",mon,d,y-1900,h,mn,s)
      ELSIF y#cyear THEN
        str.append(lng," %s %2d   %4d ",mon,d,y)
      ELSE
        str.append(lng," %s %2d  %2d:%02d ",mon,d,h,mn)
      END;
      IF ino & (inod>=0) THEN str.append(lng," i%-5d ",inod) END
    END;
    std.print("%s  ",lng);
    tty_on; std.print("%s",fname); tty_off; std.print("\n");
  END l_one;

  PROCEDURE s_one;
  BEGIN
    IF tt & (pos>0) & (pos+max>=tty.state^.columns) THEN
      pos:=0; std.print("\n")
    END;
    IF (pos=0) & (ofs>0) THEN std.print("%*c",ofs,' '); pos:=ofs END;
    IF len>=max THEN len:=max-1 END;
    tty_on;
    std.print("%-*.*s",len,len,fname);
    tty_off;
    std.print("%*c",max-len,' ');
    pos:=pos+max;
    IF pos+16>=wdt THEN std.print("\n"); pos:=0 END
  END s_one;

  VAR co: INTEGER;

BEGIN
  IF    dname="."0c THEN prefix:=""
  ELSIF dname="/"0c THEN prefix:="/"
  ELSE  str.print(prefix,"%s/",dname)
  END;
  plen:=str.len(prefix);
  IF NOT (long OR full) THEN
    ofs:=plen; std.print("%s\n",prefix); prefix:=""; plen:=0
  ELSE
    ofs:=0
  END;
  wdt:=80;
  mic:=tty.state^.min_color+1;
  IF tt & (tty.state^.columns<=wdt) THEN wdt:=tty.state^.columns-1 END;
  pos:=0;
  IF max<16 THEN max:=17 ELSE max:=33 END;
  vol:=0; fno:=0; co:=HIGH(ents)+1;
  IF rodr THEN i:=HIGH(ents) ELSE i:=0 END;
  WHILE co>0 DO
    WITH ents[i]^ DO
      d:=FALSE;  len:=nlen;
      h:=tt & (bio.e_hidden*mode#{});
      IF bio.e_dir*mode#{} THEN d:=tt;
        str.print(fname,"%s%s/",prefix,name); INC(len,plen+1)
      ELSE
        str.print(fname,"%s%s",prefix,name);  INC(len,plen)
      END;
      IF    long        THEN l_one
      ELSIF full OR one THEN f_one
      ELSE                   s_one
      END
    END;
    DEC(co);
    IF rodr THEN DEC(i) ELSE INC(i) END;
  END;
  smvol:=smvol+vol; filco:=filco+fno;
  IF pos>0   THEN std.print("\n") END;
  IF long THEN
    lng:="";
    unpack_size(vol,FALSE);

    ss(vol,ss0);  ss(fno,ss1);
    std.print("%sbyte%s in %d file%s on ",lng,ss0,fno,ss1);

    d:=tt; h:=FALSE;
    tty_on;
    IF dname#'/'0c THEN std.print('%s/',dname) ELSE std.print('/') END;
    tty_off;

    IF hco>0 THEN
      IF hco=1 THEN ss0:='' ELSE ss0:='s'  END;
      IF inv   THEN ssu:='' ELSE ssu:='in' END;
      std.print("  (%d %svisible file%s)\n\n",hco,ssu,ss0,ss1);
    ELSE
      std.print("\n\n")
    END;
  END;
END print_out;

PROCEDURE print_dir(tree: fw.TREE;
                     dir: bio.FILE;
               VAL dname: ARRAY OF CHAR;
                    list: ENTRY;
                  co,hco: INTEGER);
  VAR i,max: INTEGER;
    entries: ENTRYs;
BEGIN
  NEW(entries,co); max:=0;
  IF list#NIL THEN
    INC(dirco); i:=0;
    REPEAT
      co:=co-1;
      IF long THEN get_attrs(dir,list) ELSE list^.time:=after END;
      IF (after<=list^.time) & (list^.time<=before) THEN
        entries[i]:=list; INC(i);
        list^.nlen:=str.len(list^.name);
        IF list^.nlen>max THEN max:=list^.nlen END;
      END;
      list:=list^.next
    UNTIL co=0;
    ASSERT(list=NIL);
    RESIZE(entries,i);
    IF (i>0) & NOT nsrt THEN sort(entries) END
  END;
  print_out(dname,entries,hco,max);
  DISPOSE(entries)
END print_dir;

PROCEDURE list_tree(VAL patt: ARRAY OF CHAR);
  VAR co: INTEGER;       dir: bio.FILE;
     ss0: STR4;      ss1,ss2: STR4;
     hco: INTEGER;      mode: BITSET;
    tree: fw.TREE;      name: STR32;
    list: ENTRY;       entry: ENTRY;
    next: ENTRY;     visible: BOOLEAN;
     pat: STR256;        len: INTEGER;
BEGIN
  str.copy(pat,patt);
  len:=str.len(pat);
  fw.walk(tree,pat,FALSE);
  IF NOT fw.done THEN fw_error(pat); HALT(fw.error) END;
  dirco:=0;
  smvol:=0;
  filco:=0;
  WHILE fw.next_dir(tree) DO
    IF NOT fw.done THEN fw_error(pat) END;
    fw.dpath(tree,dname);
    IF NOT fw.done THEN fw_error(pat) END;
    dir:=fw.dir(tree);
    IF fw.done THEN
      list:=NIL; co:=0; hco:=0;
      WHILE fw.next_entry(tree,name,mode) DO
        IF NOT fw.done THEN fw_error(name) END;
        IF NOT (dirs OR fls) OR
               (dirs & (bio.e_dir*mode#{})) OR
               (fls  & (bio.e_dir*mode={})) THEN
          IF NOT inv THEN
            visible:=all OR (bio.e_hidden*mode={})
          ELSE
            visible:=(bio.e_hidden*mode#{})
          END;
          IF visible THEN
            NEW(entry);         INC(co);
            entry^.next:=list;  list:=entry;
            entry^.name:=name;  entry^.mode:=mode;
            entry^.err :=0;
          ELSIF name#".." THEN
            INC(hco)
          END
        END
      END;
      IF NOT fw.done THEN fw_error(pat) END;
      IF co+hco#0 THEN print_dir(tree,dir,dname,list,co,hco) END;
      WHILE list#NIL DO next:=list^.next; DISPOSE(list); list:=next END
    ELSE
      fw_error(dname)
    END
  END;
  IF NOT fw.done THEN fw_error(pat) END;
  fw.dispose(tree);
  IF long & (dirco>1) & (smvol>0) THEN
    ss(smvol,ss0);
    ss(filco,ss1);
    ss(dirco,ss2);
    IF ss2='' THEN ss2:='y' ELSE ss2:='ies' END;
    lng:="";
    unpack_size(smvol,FALSE);
    std.print("TOTAL %sbyte%s in %d file%s on %d director%s\n"
                    ,lng,ss0,filco,ss1,dirco,ss2);
  END;
END list_tree;

PROCEDURE flags;

  CONST max=MAX(INTEGER);

  VAR s: STRING; mn,d,h,m,sc: INTEGER;

  PROCEDURE date(VAR t: INTEGER; night: BOOLEAN);
  BEGIN
    tim.scan_date(t,s,night); long:=TRUE;
    IF t>=0 THEN RETURN END;
    std.print("illegal time specification\n"); HALT(err.bad_parm)
  END date;

BEGIN
  long:=args.flag('-','l');
  all :=args.flag('-','a');
  rodr:=args.flag('-','r');
  time:=args.flag('-','t');
  one :=args.flag('-','1');
  full:=args.flag('-','f');
  cre :=args.flag('-','c');
  ino :=args.flag('-','i');
  user:=args.flag('-','u');
  inv :=args.flag('-','v'); (* invisible only   *)
  ltim:=args.flag('-','y'); (* year & seconds   *)
  norm:=args.flag('-','n'); (* normal intensity *)
  dirs:=args.flag('-','D'); (* directories only *)
  fls :=args.flag('-','F'); (* files only  Leg, 21-Nov-90 *)
  nsrt:=args.flag('-','s'); (* not sorted       *)
  IF dirs & fls THEN
    std.print('illegal flag combination -D & -F\n'); HALT(err.bad_parm)
  END;
  IF time THEN long:=TRUE  END;
  IF user THEN long:=TRUE  END;
  IF all  THEN inv :=FALSE END;

  IF args.string("after" ,s) THEN date(after,FALSE) ELSE after := 0  END;
  IF args.string("before",s) THEN date(before,TRUE) ELSE before:=max END;
  IF before<after THEN
    std.print("degenerated time range"); HALT(err.bad_parm)
  END;

  IF long THEN tim.unpack(tim.time(),cyear,mn,d,h,m,sc) END
END flags;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "ls"  list files tree utility program (c) KRONOS\n'
      'usage:\n'
      '   ls [-lart1fciuvynDF] {file_tree} [times]\n');
  std.print(
      'times:\n'
      '    [after=time] [before=time]\n'
      '    time = [dd/mm/yy,][hh:mm[.s]]\n'
      '         | [yy#dd#mm,][hh:mm[.s]]\n\n'
      '                                   Leopold, Mar 1 90\n')
END pusage;

VAR  i: INTEGER;

BEGIN
  IF args.flag('-','h') THEN pusage; HALT END;
  tt:=std.is_tty(std.out);
  flags;
  IF tt THEN tty.set_cursor(0); tty.set_awp(1) END;
  IF HIGH(args.words)<0 THEN
    list_tree("*"0c)
  ELSE
    FOR i:=0 TO HIGH(args.words) DO
      list_tree(args.words[i])
    END
  END
END ls.
