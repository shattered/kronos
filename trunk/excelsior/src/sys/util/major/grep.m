MODULE grep; (*$U+    Leo 07-Nov-89.  (c) KRONOS *)
             (*       Leg 01-Apr-90.  (c) KRONOS *)
             (*       Hady 23-Nov-90. (c) KRONOS *)
             (*$+2048 words add on stack         *)

IMPORT  sys: SYSTEM;
IMPORT  key: Keyboard;
IMPORT   fw: fsWalk;
IMPORT  bio: BIO;
IMPORT  err: defErrors;
IMPORT  tty: Terminal;
IMPORT  sci: ASCII;
IMPORT  std: StdIO;
IMPORT  str: Strings;
IMPORT  arg: tskArgs;
IMPORT  mem: Heap;
IMPORT  env: tskEnv;
IMPORT  dnv: tskEnv;
IMPORT  tim: Time;
IMPORT  usr: Users;

WITH STORAGE: mem;

TYPE
  STR4   = ARRAY [0..  3] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

VAR (* flags *)
  qry: BOOLEAN; (* query for files               *)
  qrr: BOOLEAN; (* query for replace             *)
  exi: BOOLEAN; (* only existing at dest dir     *)
  exc: BOOLEAN; (* only not existing there       *)
  mkd: BOOLEAN; (* make path to dest directory   *)
  mdf: BOOLEAN; (* modified files only           *)
  txt: BOOLEAN; (* consider file as text         *)
  rep: BOOLEAN; (* replace pattern and copy      *)
  shf: BOOLEAN; (* show entry account in file(s) *)
  shr: BOOLEAN; (* show replaces      in file(s) *)
  ale: BOOLEAN; (* show all entries              *)

  w_aft,w_bef: INTEGER;
  c_aft,c_bef: INTEGER;

VAR (* global state variables *)
      tt: BOOLEAN;
     ipr: BOOLEAN;
    tree: fw.TREE;
    patt: STR32;
    home: STR256;
   todir: bio.FILE;
  toname: STR256;
  frname: STR256;

     all: BOOLEAN; (* all files at this dir  *)
    skip: BOOLEAN; (* skip this directory    *)

   entc, entcs: INTEGER; (* local/global entries  account *)
   repc, repcs: INTEGER; (* local/global replaces account *)
         files: INTEGER; (* scanned files account         *)

---------------------------- ERRORS ----------------------------
                            --------

CONST
  OPEN   = 'open';
  CLOSE  = 'close';
  REWIND = 'seek(0,0)';
  READ   = 'read';
  WRITE  = 'write';

PROCEDURE _error(ec: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(ec,'"%s" %%s\n',name); HALT(1);
END _error;

PROCEDURE fw_check(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.done THEN RETURN END;
  IF fw.error#err.bad_parm THEN
    _error(fw.error,name)
  ELSE
    tty.perror(fw.error,'"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END;
  HALT(1);
END fw_check;

PROCEDURE check(VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,fmt,args);
  HALT(1)
END check;

----------------------------- QUERY ----------------------------
                             -------

CONST vis_char = ARRAY OF CHAR {
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  " ", "!", '"', "#", "$", "%", "&", "'",
  "(", ")", "*", "+", ",", "-", ".", "/",
  "0", "1", "2", "3", "4", "5", "6", "7",
  "8", "9", ":", ";", "<", "=", ">", "?",
    "@", "A", "B", "C", "D", "E", "F", "G",
    "H", "I", "J", "K", "L", "M", "N", "O",
    "P", "Q", "R", "S", "T", "U", "V", "W",
    "X", "Y", "Z", "[", "\", "]", "^", "_",
    "`", "a", "b", "c", "d", "e", "f", "g",
    "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w",
    "x", "y", "z", "{", "|", "}", "~", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
  "_", "_", "_", "_", "_", "_", "_", "_",
    "ю", "а", "б", "ц", "д", "е", "ф", "г",
    "х", "и", "й", "к", "л", "м", "н", "о",
    "п", "я", "р", "с", "т", "у", "ж", "в",
    "ь", "ы", "з", "ш", "э", "щ", "ч", "ъ",
    "Ю", "А", "Б", "Ц", "Д", "Е", "Ф", "Г",
    "Х", "И", "Й", "К", "Л", "М", "Н", "О",
    "П", "Я", "Р", "С", "Т", "У", "Ж", "В",
    "Ь", "Ы", "З", "Ш", "Э", "Щ", "Ч", "Ъ"};

PROCEDURE _print_df(len: INTEGER; VAL name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF    toname='/'0c THEN str.print(s,"/%-*s",len,name)
  ELSIF toname='.'0c THEN str.print(s,"%-*s",len,name)
  ELSE                    str.print(s,"%s/%-*s",toname,len,name)
  END;
  IF NOT std.is_tty(std.in) THEN tty.print("%s",s) END;
  std.print("%s",s)
END _print_df;

PROCEDURE _print_sf(len: INTEGER; VAL name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF    frname='/'0c THEN str.print(s,"/%-*s",len,name)
  ELSIF frname='.'0c THEN str.print(s,"%-*s",len,name)
  ELSE                    str.print(s,"%s/%-*s",frname,len,name)
  END;
  IF NOT std.is_tty(std.in) THEN tty.print("%s",s) END;
  std.print("%s",s)
END _print_sf;

PROCEDURE push_info(VAL name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF    toname='/'0c THEN str.print(s,"/%s",name)
  ELSIF toname='.'0c THEN str.print(s,"%s",name)
  ELSE                    str.print(s,"%s/%s",toname,name)
  END;
  env.put_str(dnv.info,s,TRUE);
END push_info;

PROCEDURE query(VAL sname,dname: ARRAY OF CHAR): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  ipr:=env.ipr();
  IF  NOT qry OR NOT tt OR ipr THEN RETURN TRUE END;
  IF str.len(sname)<16 THEN _print_sf(16,sname) ELSE _print_sf(32,sname) END;
  IF rep THEN tty.print(" >> ");
    IF str.len(dname)<16 THEN _print_df(16,dname) ELSE _print_df(32,dname) END
  END;
  tty.print('?');
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR
       (c='A') OR (c='I') OR (ch=key.can) THEN EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can THEN tty.print("^X\r") ELSE tty.print("%c\r",ch) END;
  tty.erase_line(0);
  IF c ='Q'    THEN HALT END;
  IF c ='I'    THEN skip:=TRUE  END;
  IF ch='A'    THEN qry :=FALSE END;
  IF ch='a'    THEN all :=TRUE  END;
  IF ch=key.can THEN qry:=FALSE; ipr:=TRUE END;
  RETURN (c='Y') OR (c='A') OR (ch=key.can)
END query;

CONST op_err = '%s("%s") %%s\n';

---------------------------- SEARCH ----------------------------
                            --------
VAR  pattern: STRING;
     replace: STRING;
       table: ARRAY CHAR OF INTEGER;

PROCEDURE prepare(VAR s: STRING);
  VAR ch: CHAR; i: INTEGER;
BEGIN
  pattern^:=s^;
  FOR ch:=MIN(CHAR) TO MAX(CHAR) DO table[ch]:=-1        END;
  FOR i:=0 TO HIGH(pattern)-1    DO table[pattern[i]]:=i END
END prepare;

PROCEDURE search(B: sys.ADDRESS; pos,fin: INTEGER): INTEGER;
  VAR   C: POINTER TO ARRAY [0..8196] OF CHAR;
      i,c: INTEGER;
BEGIN
  ASSERT(fin<HIGH(C^));
  C:=B;
  IF (HIGH(pattern)<=0) OR (pos>=fin) THEN RETURN -1 END;
  i:=HIGH(pattern)-1; c:=pos+i;
(*$<*) (*$T-*)
  WHILE (c<=fin) DO
    WHILE (i>=0) & (C^[c]=pattern[i]) DO DEC(i); DEC(c) END;
    IF i<0 THEN RETURN pos
    ELSE c:=table[C^[c]];
      IF c>i THEN pos:=pos-c+HIGH(pattern)
      ELSE        pos:=pos-c+i
      END;
      i:=HIGH(pattern)-1; c:=pos+i
    END
  END;
(*$>*)
  RETURN -1
END search;

----------------------------- FILES ----------------------------
                             -------

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
  IF NOT bio.done & (bio.error#err.no_entry) THEN check(op_err,OPEN,name) END;
  IF bio.done THEN
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,name) END;
  ELSE
    bio.fmkdir(at,name,FALSE);    check(op_err,'make_dir',name);
    bio.fopen((at),dir,name,'m'); check(op_err,OPEN,name);
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,name) END;
  END;
  bio.close(at); check(op_err,CLOSE,name);
  IF NOT last THEN
    at:=dir; dir:=bio.null; make_through(at,dir,i,path)
  END
END make_through;

PROCEDURE open_or_make(VAR dir: bio.FILE;
                      VAL path: ARRAY OF CHAR;
                          make: BOOLEAN);
  VAR i: INTEGER;
     at: bio.FILE;
BEGIN
  bio.open(dir,path,'m');
  IF NOT mkd THEN
    check(op_err,OPEN,path);
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,path) END;
    RETURN
  END;
  IF bio.done THEN
    IF bio.is_dir*bio.kind(dir)={} THEN _error(err.not_dir,path) END;
    RETURN
  END;
  dir:=bio.null;
  IF NOT make THEN RETURN END;
  IF path[0]='/' THEN
    bio.open(at,"/",'');
    check(op_err,'open',"/"); i:=1
  ELSE
    at:=bio.cd; i:=0
  END;
  bio.dup(at,(at)); check(op_err,'dup',path);
  make_through(at,dir,i,path)
END open_or_make;

PROCEDURE open_to(VAR to: bio.FILE; make: BOOLEAN);
BEGIN
  IF todir#bio.null THEN to:=todir; toname:=home; RETURN END;
  fw.substitute(tree,home,toname);
  IF toname="" THEN _error(err.bad_name,home) END;
  open_or_make(to,toname,make);
END open_to;

PROCEDURE close_to(VAR to: bio.FILE);
BEGIN
  IF todir#bio.null THEN to:=bio.null; RETURN END;
  IF to=bio.null THEN RETURN END;
  bio.close(to); check(op_err,CLOSE ,toname);
END close_to;

----------------------------- GREP -----------------------------
                             ------

VAR BUF: STRING;
    sep: CHAR;

PROCEDURE grep(F, T: bio.FILE; VAL dname, sname: ARRAY OF CHAR);

  VAR pos: INTEGER; -- позиция в файле первого символа буфера
      eof: INTEGER; -- просто eof
      len: INTEGER; -- размер буффера реальный (<=HIGH)
      sta: INTEGER; -- откуда начинать поиск
      fnd: INTEGER; -- где нашелся образец
    lines: INTEGER; -- lines account in current file
        i: INTEGER; -- на всякий случай

  VAR lnl, fnl: INTEGER;

  PROCEDURE acc_strs(fnd: INTEGER);
    VAR i: INTEGER;
  BEGIN
    ASSERT(len<=BYTES(BUF));
    IF lnl>fnd THEN RETURN END;
    IF fnd>len THEN fnd:=len END;
    WHILE lnl<=fnd DO
      fnl:=lnl;
      REPEAT lnl:=lnl+1
(*$<*) (*$T-*)
      UNTIL (lnl>=len) OR (BUF[lnl]=sep);
(*$>*)
      IF lnl<len THEN lines:=lines+1 END
    END
  END acc_strs;

  PROCEDURE first_buf;
  BEGIN
    len:=BYTES(BUF); pos:=0;
    IF len>eof THEN len:=eof END;
    bio.read(F, sys.ADR(BUF), len);
    check(op_err,READ,sname)
  END first_buf;

  PROCEDURE next_buf(): BOOLEAN;
  BEGIN
    pos:=pos+4096;
    IF pos>=eof THEN RETURN TRUE
    ELSE
      bio.seek(F,pos,0);
      check(op_err,REWIND,sname);
      len:=BYTES(BUF);
      IF len>eof-pos THEN len:=eof-pos END;
      bio.read(F, sys.ADR(BUF), len);
      check(op_err,READ,sname);
      sta:=sta-4096; ASSERT(sta>=0);
      lnl:=lnl-4096; IF lnl<0 THEN lnl:=-1 END;
      RETURN FALSE
    END;
  END next_buf;

  PROCEDURE out(s: ARRAY OF CHAR; f,t: INTEGER);
    VAR l: INTEGER;
  BEGIN
    l:=t-f; IF l<=0 THEN RETURN END;
    bio.fwrite(T, sys.ADR(s), f, l);
    check(op_err,WRITE,dname)
  END out;

  CONST str_size = 64; -- maximal string length on screen

  PROCEDURE tty_show(   p: INTEGER;
                 VAL patt: ARRAY OF CHAR;
                    p_len: INTEGER;
                     line: BOOLEAN);
    VAR f,t,l: INTEGER;
  BEGIN
    f:=fnl+1; IF f<0 THEN f:=0 END;
    IF txt THEN
      IF tt THEN tty.set_underline(1) END;
      IF line THEN std.print('line %4d',lines)
      ELSE         std.print('  replace')
      END;
      IF tt THEN tty.set_underline(0) END;
      std.WriteString(": "); t:=lnl
    ELSE std.WriteString("    "); t:=len
    END;
    l:=str_size;
    IF p-f>(l DIV 2) THEN
      f:=p-(l DIV 2)+3; std.WriteString("..."); l:=l-3
    END;
    WHILE f<p DO
      std.Write(vis_char[ORD(BUF[f])]); f:=f+1; l:=l-1
    END;
    f:=0;
    IF tt THEN tty.set_reverse(1) END;
    WHILE (f<p_len) & (l>3) DO
      std.Write(vis_char[ORD(patt[f])]); f:=f+1; l:=l-1
    END;
    IF tt THEN tty.set_reverse(0) END;
    IF l<=3 THEN std.WriteString("..."); RETURN END;
    f:=p+HIGH(pattern);
    WHILE (f<t) & (l>3) DO
      std.Write(vis_char[ORD(BUF[f])]); f:=f+1; l:=l-1
    END;
    IF l<=3 THEN std.WriteString("...") END;
  END tty_show;

  VAR shows: INTEGER;

  PROCEDURE show(       p: INTEGER;
                 VAL patt: ARRAY OF CHAR;
                    p_len: INTEGER);
  BEGIN
    IF ipr & tt THEN RETURN END;
    IF shows MOD 8=0 THEN
      IF rep THEN _print_df(32,dname)
      ELSE        _print_sf(32,dname)
      END;
      std.WriteLn
    END;
    tty_show(p,patt,p_len,TRUE); std.WriteLn;
    IF NOT tt THEN shows:=      1
    ELSE           shows:=shows+1
    END
  END show;

  VAR rall: BOOLEAN;
      skip: BOOLEAN;

  PROCEDURE query(p: INTEGER): BOOLEAN;
    VAR ch: CHAR; r: BOOLEAN;
  BEGIN
    IF skip THEN RETURN FALSE END;
    ipr:=env.ipr();
    r:=rall OR NOT tt OR ipr;
    IF r THEN
      show(p,replace,HIGH(replace));
      RETURN TRUE
    ELSE
      show(p,pattern,HIGH(pattern));
      tty_show(p,replace,HIGH(replace),FALSE);
      tty.print(' ?');
      LOOP key.read(ch);
        IF    ch="q" THEN HALT
        ELSIF ch="y" THEN r:=TRUE ; EXIT
        ELSIF ch="n" THEN r:=FALSE; EXIT
        ELSIF ch="a" THEN r:=TRUE ; rall:=TRUE; EXIT
        ELSIF ch="A" THEN r:=TRUE ; rall:=TRUE; qrr:=FALSE; EXIT
        ELSIF ch="i" THEN r:=FALSE; skip:=TRUE; EXIT
        ELSIF ch=key.can THEN qry:=FALSE; ipr:=TRUE; r:=TRUE; EXIT
        ELSE key.bell(1)
        END
      END;
      tty.print('\r'); tty.erase_line(0)
    END;
    IF r & tt THEN
      tty.up(1); tty.erase_line(0);
      tty_show(p,replace,HIGH(replace),TRUE);
      tty.print('\n')
    END;
    RETURN r
  END query;

BEGIN
  rall:=NOT (qrr & tt); skip:=FALSE;
  repc:=0; entc:=0;
  eof:=bio.eof(F);
  IF eof<=0 THEN RETURN END;
  first_buf; sta:=0; lines:=-1;
  shows:=0;
  fnl:=-1; lnl:=-1;
  REPEAT
    REPEAT
      fnd:=search(sys.ADR(BUF),sta,len);
      IF fnd>=0 THEN entc:=entc+1;
        IF txt THEN acc_strs(fnd) END;
        IF rep THEN
          out(BUF,sta,fnd);
          IF query(fnd) THEN
            out(replace,0,HIGH(replace));
            repc:=repc+1
          ELSE out(pattern,0,HIGH(pattern))
          END
        ELSE
          show(fnd, pattern, HIGH(pattern));
        END;
        IF NOT ale THEN RETURN END;
        sta:=fnd+HIGH(pattern)
      ELSE
        IF txt THEN acc_strs(4096) END;
        IF rep THEN
          i:=len;
          IF i>4096 THEN i:=4096 END;
          out(BUF,sta,i)
        END;
        IF sta<4096 THEN sta:=4096 END
      END;
    UNTIL (fnd<0) OR (len-sta<HIGH(pattern));
  UNTIL next_buf();
--  IF rep THEN out(BUF,sta,len) END;
  entcs:=entcs+entc; repcs:=repcs+repc; files:=files+1
END grep;

PROCEDURE grep_file(from: bio.FILE; VAR to: bio.FILE;
                     VAL sname,dname,mask: ARRAY OF CHAR);

  VAR sf,df: bio.FILE;
      ct,wt: INTEGER;
        eof: INTEGER;

  PROCEDURE _try_open;
  BEGIN
    IF to=bio.null THEN RETURN END;
    bio.fopen((to),df,dname,"w");
    IF (NOT bio.done) & (bio.error#err.no_entry) THEN
      check(op_err,OPEN,dname)
    END;
    IF NOT bio.done THEN df:=bio.null; RETURN END;
    IF bio.is_dir*bio.kind(df)#{} THEN
      _error(err.is_dir,dname)
    END;
  END _try_open;

  PROCEDURE ctim(f: bio.FILE; VAL name: ARRAY OF CHAR): INTEGER;
    VAR t: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_ctime,t) ;
    check(op_err,'get_ctime',name);
    RETURN t
  END ctim;

  PROCEDURE wtim(f: bio.FILE; VAL name: ARRAY OF CHAR): INTEGER;
    VAR t: INTEGER;
  BEGIN
    bio.get_attr(f,bio.a_wtime,t) ;
    check(op_err,'get_wtime',name);
    RETURN t
  END wtim;

  PROCEDURE _close;
  BEGIN
    IF sf#bio.null THEN bio.close(sf); check(op_err,CLOSE,sname) END;
    IF df#bio.null THEN bio.close(df); check(op_err,CLOSE,dname) END
  END _close;

  VAR us,gr: INTEGER;

BEGIN
  bio.fopen((from),sf,sname,'r'); check(op_err,OPEN,sname);
  IF (bio.is_spec+bio.is_dir)*bio.kind(sf)#{} THEN
    _error(err.unsuitable,sname)
  END;
  wt:=wtim(sf,sname);
  ct:=ctim(sf,sname);
  IF (wt<w_aft) OR (wt>w_bef) OR (ct<c_aft) OR (ct>c_bef) THEN
    _close; RETURN
  END;
  push_info(sname);

  df:=bio.null;
  IF rep THEN
    IF (exi OR exc) & (to=bio.null) THEN open_to(to,FALSE)
    ELSE fw.substitute(tree,home,toname)
    END;

    IF exi OR exc OR mdf THEN _try_open END;
    IF exc & (df#bio.null)  OR  exi & (df=bio.null) THEN
      _close; RETURN
    ELSIF exi THEN
      bio.close(df); df:=bio.null; check(op_err,CLOSE ,dname)
    END;
    IF mdf & (df#bio.null) & (wt<=wtim(df,dname)) THEN
      _close; RETURN
    END;
    IF env.ipr() THEN qry:=FALSE; ipr:=TRUE END;
  END;
  IF NOT all THEN
    IF qry & NOT query(sname,dname) THEN _close; RETURN END;
  END;
  IF rep THEN
    IF to=bio.null THEN open_to(to,TRUE) END;
    IF df=bio.null THEN
      bio.fcreate((to),df,dname,mask,bio.eof(sf));
      check(op_err,'create',dname)
    END;
  END;
  grep(sf,df,sname,dname);
  _close;
  IF (shf OR shr) & NOT (ipr & tt) THEN
    std.print('       ');
    IF shf THEN std.print(' entries %d',entc) END;
    IF shr THEN std.print(' replaces %d',repc) END;
    std.WriteLn
  END
END grep_file;

PROCEDURE grep_tree(VAL pat: ARRAY OF CHAR);
  VAR to: bio.FILE;
    mode: BITSET;     cmask: STR4;
   sname: STR32;      dname: STR32;
BEGIN
  fw.walk(tree,pat,TRUE);
  fw_check(pat);
  WHILE fw.next_dir(tree) DO
    fw_check(pat);
    fw.dpath(tree,frname);
    to:=bio.null;
    all :=NOT qry;
    skip:=FALSE;

    fw.substitute(tree,home,toname);
    IF toname="" THEN _error(err.bad_name,home) END;
    bio.open(to,toname,'m');
    IF NOT bio.done THEN to:=bio.null END;

    WHILE NOT skip & fw.next_entry(tree,sname,mode) DO
      fw_check(sname);
      cmask:="w"0c;
      IF mode*bio.e_hidden#{} THEN str.app(cmask,"h"0c) END;
      IF patt="" THEN str.copy(dname,sname)
      ELSE
        fw.substitute(tree,patt,dname);
        IF dname="" THEN _error(err.bad_name,patt) END;
      END;
      grep_file(fw.dir(tree),to,sname,dname,cmask)
    END;
    fw_check(pat);
    close_to(to)
  END;
  fw_check(pat);
  fw.dispose(tree)
END grep_tree;

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
  RETURN TRUE
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
  IF l+3>=HIGH(prim) THEN std.print("too long arg\n"); HALT(1) END;
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
      open_or_make(todir,home,FALSE)
    END;
    RETURN
  END;
  IF NOT hisc THEN RETURN END;
  toname:=home;
  open_or_make(todir,home,FALSE);
  IF (todir=bio.null) OR NOT pisc THEN RETURN END;
  bio.fopen((todir),d,patt,'m');
  IF NOT bio.done THEN RETURN END;
  IF bio.is_dir*bio.kind(d)#{} THEN
    bio.close(todir); todir:=d;
    home:=prim; patt:=""; toname:=home; RETURN
  END;
  bio.close(d)
END set_home;

PROCEDURE usage;
BEGIN
 std.print(
  ' "grep"   Global REPlace pattern utility program    (c) KRONOS\n'
  'search:\n'
  '  grep [-qdoxmbap] {source_tree} p=SEARCH_PATT [times] [sep=number]\n'
  'replace:\n'
  '  grep [-qdoxmbaprc] {source_tree} [dest_tree] PATTs [times] [sep=number]\n'
          );
 std.print(
  'PATTs = p=SEARCH_PATT r=REPLACE_PATT\n\n'
  'times:\n'
  '  [after=time] [before=time] [cafter=time] [cbefore=time]\n'
  '  time = [dd/mm/yy,][hh:mm[.s]]  (Europian style)\n'
  '       | [yy#dd#mm,][hh:mm[.s]]  (American style)\n'
          );
 std.print(
  'query:\n'
  '      (y|n|q|a|A|i|^X) = (Yes|No|Quit|all|All|skIp|IPR)\n\n'
  '                                    Hady, Nov 23 1990\n'
          )
END usage;

PROCEDURE flags;

  CONST max=MAX(INTEGER);

  VAR s: STRING;        bump: ARRAY [0..7] OF CHAR;
      u: usr.USER;

  PROCEDURE date(VAR t: INTEGER; night: BOOLEAN);
  BEGIN
    tim.scan_date(t,s,night);
    IF t>=0 THEN RETURN END;
    std.print("illegal time specification\n"); HALT(1)
  END date;

BEGIN
  qry:=NOT arg.flag('-','q');
  qrr:=NOT arg.flag('-','r');
  mkd:=    arg.flag('-','d');
  exi:=    arg.flag('-','o');  (* only existing at dest directory *)
  exc:=    arg.flag('-','x');  (* exclusevly (only not existing)  *)
  mdf:=    arg.flag('-','m');  (* modified only (backup)          *)
  txt:=NOT arg.flag('-','b');
  ale:=    arg.flag('-','a');
  shf:=    arg.flag('-','p');  (* Patterns found *)
  shr:=    arg.flag('-','c');  (* changes made   *)
  IF exc & exi THEN
    std.print("invalid flag's combination -o -x\n"); HALT(1)
  END;
  IF arg.string("after" ,s)  THEN date(w_aft,FALSE) ELSE w_aft:= 0  END;
  IF arg.string("before",s)  THEN date(w_bef,TRUE)  ELSE w_bef:=max END;
  IF arg.string("cafter" ,s) THEN date(c_aft,FALSE) ELSE c_aft:= 0  END;
  IF arg.string("cbefore",s) THEN date(c_bef,TRUE)  ELSE c_bef:=max END;
  IF (c_bef<c_aft) OR (w_bef<w_aft) THEN
    std.print("invalid time range"); HALT(1)
  END
END flags;

PROCEDURE init;
  VAR s: STRING; i: INTEGER;
BEGIN
  flags;
  files:=0; entcs:=0; repcs:=0;
  IF NOT arg.string("p",s) THEN usage; HALT ELSE prepare(s) END;
  NEW(BUF,4096+HIGH(pattern));
  IF NOT arg.number("sep",i) THEN sep:=sci.NL
  ELSE sep:=CHAR(i MOD 256)
  END;
  rep:=arg.string("r",replace); shr:=shr & rep;
  IF rep THEN ale:=TRUE ELSE NEW(replace) END;
  ipr:=env.ipr();
  IF ipr THEN qry:=FALSE END;
END init;

VAR i: INTEGER;

BEGIN
  tt:=std.is_tty(std.out);
  init;
  IF HIGH(arg.words)<0 THEN
    usage; HALT
  END;
  IF HIGH(arg.words)<1 THEN
    set_home("."); grep_tree(arg.words[0])
  ELSE
    set_home(arg.words[HIGH(arg.words)]);
    FOR i:=0 TO HIGH(arg.words)-1 DO grep_tree(arg.words[i]) END
  END;
  IF (shf OR shr) & NOT (ipr & tt) THEN
    IF tt THEN tty.repeat(tty.state^.hbar,60); tty.WriteLn END;
    std.print('Total :');
    IF shf THEN std.print(' entries %d' ,entcs) END;
    IF shr THEN std.print(' replaces %d',repcs) END;
    std.print(' in %d files\n',files)
  END
END grep.
