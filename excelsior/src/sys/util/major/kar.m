MODULE kar; (*$U+$X+$N+  02-Mar-91. (c) KRONOS *)

IMPORT       SYSTEM;            IMPORT  err: defErrors;
IMPORT  bio: BIO;               IMPORT  std: StdIO;
IMPORT  tim: Time;              IMPORT  reg: regExpr;
IMPORT  mem: Heap;              IMPORT  str: Strings;
IMPORT  env: tskEnv;            IMPORT  tty: Terminal;
IMPORT  arg: tskArgs;           IMPORT  key: Keyboard;

WITH STORAGE: mem;

TYPE STR256 = ARRAY [0..255] OF CHAR;

CONST
  MAGIC  = "iVkar" 000c 002c;
  HIDDEN = {0};

VAR
  ALLIGN: INTEGER;
  bump  : STR256;
  QUERY : BOOLEAN;
  CLEAN : BOOLEAN;
  TIME  : BOOLEAN;
  DEL   : BOOLEAN;
  LIST  : BOOLEAN;
  IGNORE: BOOLEAN;
  PACK  : BOOLEAN;
  CD    : BOOLEAN;
  ROLL  : BOOLEAN;
  PRO   : BOOLEAN;

TYPE
  ITEM = RECORD
           name: ARRAY [0..31] OF CHAR;
           tag : BITSET;
           eof : INTEGER;
           wtim: INTEGER;
           pro : BITSET;
           fpos: INTEGER;
           rfe0: INTEGER;
           rfe1: INTEGER;
           rfe2: INTEGER;
           rfe3: INTEGER;
         END;

  DIR  = RECORD
           tag  : BITSET;
           pro  : BITSET;
           sum  : INTEGER;
           rfe0 : INTEGER;
           rfe1 : INTEGER;
           rfe2 : INTEGER;
           rfe3 : INTEGER;
           fname: STRING;
           patt : STRING;
           cont : DYNARR OF ITEM;
           ctlg : INTEGER;
         END;

  BACK = DYNARR OF DIR;


VAR all: BOOLEAN;
    ALL: BOOLEAN;
   skip: BOOLEAN;
   show: BOOLEAN;

PROCEDURE query(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD): BOOLEAN;
  VAR ch,cch: CHAR;
BEGIN
  show:=(NOT QUERY OR all OR ALL);
  IF NOT QUERY  THEN RETURN TRUE  END;
  IF env.ipr()  THEN RETURN TRUE  END;
  IF all OR ALL THEN RETURN TRUE  END;
  IF skip       THEN RETURN FALSE END;
  tty.print(fmt,args);
  REPEAT
    tty.set_cursor(1);
    key.read(ch);  cch:=CAP(ch);
    tty.set_cursor(0)
  UNTIL (cch='Y')OR(cch='N')OR(cch='A')OR(cch='I')OR(cch='Q') OR env.ipr();
  IF env.ipr() THEN tty.print("^X\n"); RETURN TRUE END;
  tty.print("%c\n",ch);
  IF cch='Q' THEN HALT(err.break) END;
  IF ch ='A' THEN ALL :=TRUE END;
  IF ch ='a' THEN all :=TRUE END;
  IF cch='I' THEN skip:=TRUE END;
  RETURN (cch='Y') OR (cch='A')
END query;

PROCEDURE allign(n: INTEGER): INTEGER;
BEGIN
  IF ALLIGN<=1 THEN RETURN n END;
  RETURN (n+ALLIGN-1) DIV ALLIGN*ALLIGN
END allign;

PROCEDURE bio_error(VAL op,name: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'"%s" %%s %s("%s")\n',name,op,bio.ename)
END bio_error;

PROCEDURE bio_fatal(VAL op,name: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'"%s" %%s %s("%s")\n',name,op,bio.ename); HALT(bio.error)
END bio_fatal;

PROCEDURE bio_check(VAL op,name: ARRAY OF CHAR);
BEGIN
  IF NOT bio.done THEN bio_error(op,name) END
END bio_check;

PROCEDURE bio_assert(VAL op,name: ARRAY OF CHAR);
BEGIN
  IF NOT bio.done THEN bio_fatal(op,name) END
END bio_assert;

PROCEDURE bio_pro(f: bio.FILE; VAL name: ARRAY OF CHAR): BITSET;
  VAR u,g,p: BITSET;
BEGIN
  bio.get_attr(f,bio.a_uid,u); bio_check('get_attr',name);
  bio.get_attr(f,bio.a_gid,g); bio_check('get_attr',name);
  bio.get_attr(f,bio.a_pro,p); bio_check('get_attr',name);
  RETURN (g>>8)+(u<<16)+p
END bio_pro;

PROCEDURE ext_is_kar(VAL fn: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=str.len(fn);
  IF i<=4 THEN RETURN FALSE END;
  RETURN (fn[i-4]='.') & (fn[i-3]='k') & (fn[i-2]='a') & (fn[i-1]='r')
END ext_is_kar;

PROCEDURE print(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR s: ARRAY [0..63] OF CHAR;
BEGIN
  IF NOT env.ipr() THEN tty.print(fmt,args); tty.print("\n")
  ELSE str.print(s,fmt,args); env.put_str(env.info,s,TRUE)
  END
END print;

PROCEDURE cut_word(VAR w,s: ARRAY OF CHAR);
  VAR i,j,k: INTEGER;
BEGIN
  i:=0;
  str.skip(s,i,' ');
  j:=i;
  str.search(s,j,' ');
  str.sub_str(w,s,i,j-i);
  str.skip(s,j,' ');
  str.delete(s,0,j);
END cut_word;

PROCEDURE times(VAR s: ARRAY OF CHAR; VAR from,to: INTEGER);
BEGIN
  IF s="" THEN from:=0; to:=MAX(INTEGER); RETURN END;
  cut_word(bump,s);
  tim.scan_date(from,bump,FALSE);
  IF from<0 THEN
    tty.print("ILLEGAL time specification\n"); HALT(err.bad_parm)
  END;
  IF s="" THEN to:=MAX(INTEGER); RETURN END;
  cut_word(bump,s);
  tim.scan_date(to,bump,TRUE);
  IF to<0 THEN
    tty.print("ILLEGAL time specification\n"); HALT(err.bad_parm)
  END
END times;

PROCEDURE swap(VAR back: BACK; i,j: INTEGER);
  VAR t: DIR;
BEGIN
  t:=back[i]; back[i]:=back[j]; back[j]:=t
END swap;

PROCEDURE scan_dirs(VAR back: BACK; after,before,size: INTEGER);
  VAR d,f: bio.FILE;
     name: ARRAY [0..31] OF CHAR;
     mode: BITSET;
     rexp: reg.EXPR;
    first: BOOLEAN;
    i,j,t: INTEGER;
    magic: ARRAY [0..7] OF CHAR;
BEGIN
  i:=0;
  WHILE i<=HIGH(back) DO
    first:=TRUE;
    WITH back[i] DO
      sum:=0;
      reg.compile(patt,rexp);
      IF NOT reg.done THEN
        tty.perror(reg.error,'"%s" %%s "%s"\n',fname,patt); HALT(reg.error)
      END;
      d:=bio.null;
      bio.open(d,fname,'rx');
      IF NOT bio.done THEN
        bio_error('open',fname)
      ELSIF bio.kind(d)*bio.is_dir={} THEN
        tty.print('"%s" is not directory\n')
      ELSE
        bio.fname(d,bump);              bio_assert('fname',fname);
        RESIZE(fname,str.len(bump)+1);
        str.copy(fname,bump);
        pro:=bio_pro(d,fname);
        tag:={};
        IF bio.is_hidd(d) THEN tag:=tag+HIDDEN END;
        rfe0:=0;  rfe1:=0; rfe2:=0; rfe3:=0;
        bio.dir_walk(d,bio.s_name+bio.s_dirfwd);  bio_assert('dir_walk',fname);
        all:=FALSE; skip:=FALSE;
        WHILE bio.get_entry(d,name,mode) DO
          IF (mode*(bio.e_dir+bio.e_esc)={}) & reg.match(rexp,name,0) THEN
            f:=bio.null;
            bio.fopen((d),f,name,'r');
            IF NOT bio.done THEN
              bio_error('open',name)
            ELSIF bio.eof(f)<size-256-BYTES(cont) THEN
              IF bio.eof(f)<BYTES(magic) THEN magic:=""
              ELSE
                bio.get(f,magic,BYTES(magic)); bio_assert('read',name)
              END;
              bio.get_attr(f,bio.a_wtime,t);     bio_assert('get_attr',name);
              IF (t>=after) & (t<=before) & (magic#MAGIC) THEN
                IF first & QUERY THEN print("%s",fname); first:=FALSE END;
                IF query('save  %-32s?',name) THEN
                  IF first THEN print("%s",fname); first:=FALSE END;
                  IF show  THEN print('save  %s',name) END;
                  j:=HIGH(cont)+1;      INC(sum,allign(bio.eof(f)));
                  RESIZE(cont,j+1);     cont[j].tag:={};
                  cont[j].wtim:=t;      cont[j].eof:=bio.eof(f);
                  cont[j].name:=name;   cont[j].pro:=bio_pro(f,name);
                  cont[j].rfe0:=0;      cont[j].rfe1:=0;
                  cont[j].rfe2:=0;      cont[j].rfe3:=0;
                  IF bio.is_hidd(f) THEN cont[j].tag:=cont[j].tag+HIDDEN END
                END
              END
            ELSE
              print('"%s" %d bytes is too large',name,bio.eof(f)); HALT(err.too_large)
            END;
            bio.close(f)
          END
        END;
        bio.end_walk(d);        bio_assert('end_walk',fname);
        bio.close(d)
      END
    END;
    IF back[i].sum=0 THEN
      swap(back,i,HIGH(back)); RESIZE(back,HIGH(back))
    ELSE
      INC(i)
    END
  END
END scan_dirs;

PROCEDURE bio_putw(f: bio.FILE; w: SYSTEM.WORD);
BEGIN bio.write(f,SYSTEM.ADR(w),4) END bio_putw;

PROCEDURE bio_getw(f: bio.FILE; VAR w: SYSTEM.WORD);
BEGIN bio.read(f,SYSTEM.ADR(w),4) END bio_getw;

VAR buf: ARRAY [0..4096*4] OF CHAR;

PROCEDURE open_device(VAR dst: bio.FILE; VAR  out: bio.FILE;
                           no: INTEGER;  VAL dstn: ARRAY OF CHAR);
  VAR time,t,n,num: INTEGER;
BEGIN
  IF no#0 THEN
    bio.seek(dst,8+16,0);  bio_assert("seek",dstn);
    bio_getw(dst,time);    bio_assert("read",dstn);
    bio_getw(dst,num);     bio_assert("read",dstn);
    bio.close(dst);        tim.delay(6,tim.sec)
  END;
  bio.check_io(FALSE);
  REPEAT
    LOOP
      IF NOT env.ipr() THEN
        tty.set_color(+1);  tty.print("\r *** insert DISK #%d ***\r",no)
      END;
      bio.open(dst,dstn,'rc');
      IF bio.done THEN bio.seek(dst,8+16,0) END;
      IF bio.done THEN bio_getw(dst,t)   END;
      IF bio.done THEN bio_getw(dst,n)   END;
      IF bio.done & ((no=0) OR (t#time) OR (n#num)) THEN
        bio.close(dst); EXIT
      END;
      bio.close(dst);
      tim.delay(3,tim.sec);
      IF NOT env.ipr() THEN tty.print("\r                         \r") END;
      tim.delay(3,tim.sec);
    END;
    bio.open(dst,dstn,'wr')
  UNTIL bio.done;
  IF NOT env.ipr() THEN tty.set_color(0) END;
  bio.dup(out,(dst)); bio_assert("dup",dstn)
END open_device;

PROCEDURE write_dirs(VAR dst: bio.FILE; VAL dstn,name: ARRAY OF CHAR;
                          no: INTEGER;       VAR back: BACK;
           from,finx,to,tinx: INTEGER);

  VAR
    cprcnt: INTEGER;
    writen: INTEGER;
   percent: INTEGER;

  PROCEDURE copy(out,inp: bio.FILE);
    VAR len,eof,j: INTEGER;
  BEGIN
    eof:=bio.eof(inp);
    WHILE eof>0 DO
      IF BYTES(buf)<=eof THEN len:=BYTES(buf) ELSE len:=eof END;
      bio.get(inp,buf,len);
      bio.put(out,buf,len);
      INC(writen,len);
      IF NOT env.ipr() THEN
        j:=writen DIV percent;
        WHILE j>cprcnt DO
          tty.set_reverse(1); tty.print(" "); tty.set_reverse(0); INC(cprcnt)
        END;
      END;
      DEC(eof,len)
    END
  END copy;

  VAR i,x: INTEGER;
      inp: bio.FILE;
      out: bio.FILE;
      dir: bio.FILE;
      d,n: INTEGER;
     size: INTEGER;
    l,h,s: INTEGER;

  PROCEDURE result(i: INTEGER; VAL s0,s1,s2: ARRAY OF CHAR);
  BEGIN
    IF i<1000 THEN print('%d "%s%s%s" #%d',i,s0,s1,s2,no)
    ELSE print('%d,%03d "%s%s%s" #%d',i DIV 1000,i MOD 1000,s0,s1,s2,no)
    END
  END result;

BEGIN
  size:=allign(256);
  FOR d:=from TO to DO
    IF d=from THEN l:=finx   ELSE l:=0                  END;
    IF d=to   THEN h:=tinx-1 ELSE h:=HIGH(back[d].cont) END;
    FOR i:=l TO h DO
      INC(size,allign(back[d].cont[i].eof)+9*4+32)
    END;
  END;
  IF bio.kind(dst)*bio.is_disk={} THEN
    bio.fcreate((dst),out,name,'w',size); bio_assert('create',name);
    bio.extend(out,size);                 bio_assert('extend',name)
  ELSE
    open_device(dst,out,no,dstn)
  END;

  bio.buffers(out,4,4096*2);
  bio.check_io(TRUE);
  bio.put(out,MAGIC,8);
  bio.seek(out,16,1); bio_assert('seek',name); (* skip space for DISK label *)
  bio_putw(out,tim.time());
  bio_putw(out,no);
  bio_putw(out,0);  (* RFE *)
  bio_putw(out,0);  (* RFE *)
  bio_putw(out,0);  (* RFE *)
  bio_putw(out,0);  (* RFE *)
  bio_putw(out,to-from+1);
  bio_putw(out,ALLIGN);
  writen:=0;
  cprcnt:=0;
  percent:=size DIV 50;
  FOR d:=from TO to DO
    WITH back[d] DO
      IF d=from THEN l:=finx   ELSE l:=0          END;
      IF d=to   THEN h:=tinx-1 ELSE h:=HIGH(cont) END;
      bio_putw(out,pro);
      bio_putw(out,h-l+1);
      bio_putw(out,rfe0);
      bio_putw(out,rfe1);
      bio_putw(out,rfe2);
      bio_putw(out,rfe3);
      bio.putch(out,CHAR(BYTES(fname)));
      bio.put(out,fname,BYTES(fname));
      FOR i:=l TO h DO
        bio_putw(out,cont[i].tag);
        bio_putw(out,cont[i].pro);
        bio_putw(out,cont[i].wtim);
        bio_putw(out,cont[i].eof);
        bio_putw(out,cont[i].rfe0);
        bio_putw(out,cont[i].rfe1);
        bio_putw(out,cont[i].rfe2);
        bio_putw(out,cont[i].rfe3);
        cont[i].fpos:=bio.pos(out);
        bio_putw(out,0);
        n:=str.len(cont[i].name)+1;
        bio.putch(out,CHAR(n));
        bio.put(out,cont[i].name,n)
      END
    END
  END;
  IF NOT env.ipr() THEN
    FOR i:=0 TO 48 DO bump[i]:=tty.state^.hbar END; bump[49]:=0c;
    tty.print("%49s%c\r",bump,tty.state^.bars[1,2])
  END;
  FOR d:=from TO to DO
    WITH back[d] DO
      bio.open(dir,fname,'r');                 bio_assert('open',fname);
      IF d=from THEN l:=finx   ELSE l:=0          END;
      IF d=to   THEN h:=tinx-1 ELSE h:=HIGH(cont) END;
      FOR i:=l TO h DO
        n:=allign(bio.pos(out));
        bio.seek(out,n,0);                     bio_assert('seek',dstn);
        bio.fopen((dir),inp,cont[i].name,'r'); bio_assert('open',cont[i].name);
        copy(out,inp);
        bio.close(inp);                        bio_assert('close',cont[i].name);
        x:=allign(bio.pos(out));
        bio.seek(out,cont[i].fpos,0);          bio_assert('seek',dstn);
        bio_putw(out,n);
        bio.seek(out,x,0);                     bio_assert('seek',dstn)
      END;
      bio.close(dir);                          bio_assert('close',fname)
    END
  END;
  IF NOT env.ipr() THEN tty.print("\r"); tty.erase_line(0) END;
  IF bio.kind(out)*bio.is_disk#{} THEN
    result(bio.pos(out),dstn,"","")
  ELSE
    result(bio.eof(out),dstn,"/",name)
  END;
  bio.close(out);       bio_assert('close',name)
END write_dirs;

PROCEDURE findlessandswap(VAR cont: ARRAY OF ITEM; f,size: INTEGER): BOOLEAN;
  VAR i: INTEGER;
      t: ITEM;
BEGIN
  i:=f+1;
  WHILE (i<=HIGH(cont)) & (cont[i].eof+64+str.len(cont[i].name)+1>size) DO
    INC(i)
  END;
  IF i>HIGH(cont) THEN RETURN FALSE END;
  t:=cont[i]; cont[i]:=cont[f]; cont[f]:=t; RETURN TRUE
END findlessandswap;

PROCEDURE pack_dirs(VAR dst: bio.FILE; VAL dstn,last: ARRAY OF CHAR;
                                       VAR back: BACK; size: INTEGER);
  VAR  dir: bio.FILE;
     fname: STR256;
       add: INTEGER;
      full: BOOLEAN;
        to: INTEGER;
      from: INTEGER;
     toinx: INTEGER;
   frominx: INTEGER;
 i,j,s,n,k: INTEGER;
BEGIN
  IF HIGH(back)<0 THEN RETURN END;
  n:=0;
  from:=0; frominx:=0;
  i:=from;
  j:=0;
  LOOP
    IF from>HIGH(back) THEN EXIT END;
    s:=64; (* header *)
    full:=FALSE;
    WHILE NOT full & (i<=HIGH(back)) DO
      s:=s+64+str.len(back[i].fname)+1;
      LOOP
        IF i>HIGH(back)         THEN full:=TRUE;   EXIT END;
        IF j>HIGH(back[i].cont) THEN j:=0; INC(i); EXIT END;
        add:=9*4+str.len(back[i].cont[j].name)+1+allign(back[i].cont[j].eof);
        IF s+add>size-allign(64)-1024 THEN
          full:=NOT findlessandswap(back[i].cont,j,size-s-allign(64)-1024);
          IF full THEN EXIT END
        ELSE
          INC(j);  s:=s+add
        END
      END
    END;
    to:=i; toinx:=j;
    IF j=0 THEN to:=i-1; toinx:=HIGH(back[i-1].cont)+1 END;
    IF bio.kind(dst)*bio.is_disk#{} THEN str.print(fname,"%s",dstn)
    ELSIF (i>HIGH(back)) & (n=0)    THEN str.print(fname,"%s.kar",last)
    ELSE                                 str.print(fname,"%s%02d.kar",last,n)
    END;
    write_dirs(dst,dstn,fname,n,back,from,frominx,to,toinx); INC(n);
    from:=i; frominx:=j
  END;

  IF NOT DEL THEN RETURN END;
  all:=FALSE;  skip:=FALSE;
  FOR i:=0 TO HIGH(back) DO
    WITH back[i] DO
      bio.open(dir,fname,'rw');
      IF NOT bio.done THEN bio_error('open',fname)
      ELSE
        FOR j:=0 TO HIGH(cont) DO
          IF query('remove  %s/%-32s?',fname,cont[j].name) THEN
            IF show THEN print('remove  %s/%s',fname,cont[j].name) END;
            bio.funlink(dir,cont[j].name);  bio_check('unlink',cont[j].name)
          END
        END
      END;
      bio.close(dir);      bio_check('close',fname)
    END
  END
END pack_dirs;

PROCEDURE save(spc: bio.FILE; VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
    s,n: STR256;
    dst: bio.FILE;
   dstn: STR256;
   last: STR256;
   done: BOOLEAN;
   size: INTEGER;
   back: BACK;
  after: INTEGER;
 before: INTEGER;
BEGIN
  size  :=MAX(INTEGER);
  after :=0;
  before:=MAX(INTEGER);
  ALLIGN:=0;
  IF spc#bio.null THEN
    dst:=bio.null;
    NEW(back);
    WHILE bio.pos(spc)<bio.eof(spc) DO
      bio.getstr(spc,s,0);     bio_assert('getstr',name);
      cut_word(n,s);
      IF n[0]='%' THEN (* it's comment *)
      ELSIF n="TIME" THEN
        times(s,after,before);
      ELSIF n="ALLIGN" THEN
        i:=0; str.iscan(ALLIGN,s,i,done);
        IF NOT done OR (ALLIGN<0) OR (ALLIGN>4096) THEN
          tty.print('"ALLIGN" out off [0..4096]\n'); HALT(err.bad_parm)
        END
      ELSIF n="DESTINATOR" THEN
        IF dst#bio.null THEN
          tty.print('"DESTINATOR" specified twice\n'); HALT(err.bad_parm)
        END;
        cut_word(dstn,s);
        bio.open(dst,dstn,'rw');
        IF bio.done & (bio.kind(dst)*bio.is_disk#{}) THEN
          size:=bio.eof(dst); last:=""
        ELSE
          bio.close(dst);
          bio.splitpathname(dstn,last);   bio_assert('splitpath',dstn);
          bio.open(dst,dstn,'rw');        bio_assert('open',dstn);
          IF bio.kind(dst)*bio.is_dir={} THEN
            tty.print('DESTINATOR "%s" isn`t directory\n',dstn); HALT(err.not_dir)
          END;
          size:=-1;
          i:=0; str.iscan(size,s,i,done);
          IF NOT done THEN tty.print('size expected in "%s"\n',name); HALT END;
          size:=size*1024;
          IF size<64*1024 THEN tty.print('size must be > 64 (KB)!\n'); HALT END
        END
      ELSE
        i:=HIGH(back)+1;
        RESIZE(back,i+1);
        NEW(back[i].fname,str.len(n)+1); str.copy(back[i].fname,n);
        NEW(back[i].cont);
        back[i].tag:={};
        back[i].pro:={};
        back[i].sum:=0;
        back[i].rfe0:=0;
        back[i].rfe1:=0;
        back[i].rfe2:=0;
        back[i].rfe3:=0;
        back[i].ctlg:=0;
        IF s="" THEN
          NEW(back[i].patt,2); str.copy(back[i].patt,"*")
        ELSE
          NEW(back[i].patt,str.len(s)+1); str.copy(back[i].patt,s)
        END
      END
    END;
    bio.close(spc);
    IF dst=bio.null THEN
      tty.print('DESTINATOR don`t specified\n',name); HALT(err.bad_parm)
    END;
    bio.fname(dst,dstn);        bio_assert('fname',dstn)
  ELSE
    NEW(back,1);
    WITH back[0] DO
      IF HIGH(arg.words)>0 THEN
        str.copy(dstn,arg.words[1]);
        bio.splitpathname(dstn,last);   bio_assert("splitpath",dstn);
        IF dstn="" THEN dstn:="." END
      ELSE
        dstn:=".";  last:=""
      END;
      bio.open(dst,dstn,'r');     bio_assert('open',dstn);
      bio.fname(dst,dstn);        bio_assert('fname',dstn);
      NEW(fname,str.len(dstn)+1); str.copy(fname,dstn);
      NEW(cont);  tag:={};  pro:={};  sum:=0;  ctlg:=0;
      rfe0:=0;  rfe1:=0; rfe2:=0; rfe3:=0;
      NEW(patt,BYTES(arg.words[0]));  str.copy(patt,arg.words[0]);
      IF last="" THEN
        bump:=dstn; bio.splitpathname(bump,last); bio_assert("splitpath",dstn)
      END
    END
  END;
  IF bio.kind(dst)*bio.is_disk#{} THEN
    tty.print('SAVE to "%s"\n',dstn)
  ELSE
    tty.print('SAVE to "%s/%s.kar"\n',dstn,last);
  END;
  scan_dirs(back,after,before,size);
  print("");
  pack_dirs(dst,dstn,last,back,size);
  bio.close(dst);
  bio_assert('close',dstn)
END save;

PROCEDURE make_through(VAL e: DIR;
                       at: bio.FILE; VAR dir : bio.FILE;  i: INTEGER;
                       VAL path: ARRAY OF CHAR; VAR done: BOOLEAN);
  VAR j,h: INTEGER;
     last: BOOLEAN;
     name: ARRAY [0..31] OF CHAR;
     mode: ARRAY [0..3] OF CHAR;
BEGIN
  IF at=bio.null THEN
    bio.open(at,"/",'rx');
    IF NOT bio.done THEN bio_error('open',"/"); done:=FALSE; RETURN END
  END;
  done:=TRUE;
  j:=0;
  h:=HIGH(path)+1;
  IF i+HIGH(name)<h THEN h:=i+HIGH(name) END;
  WHILE (i<h) & (path[i]#0c) & (path[i]#'/') DO
    name[j]:=path[i]; INC(i); INC(j)
  END;
  name[j]:=0c;
  last:=(i>HIGH(path)) OR (path[i]=0c);
  IF NOT last THEN INC(i) (* skip '/' *) END;
  IF last THEN mode:='rwx' ELSE mode:='rx' END;
  bio.fopen((at),dir,name,mode);
  IF NOT bio.done & (bio.error#err.no_entry) THEN
    bio_error('open',name); done:=FALSE; RETURN
  END;
  IF NOT bio.done THEN
    bio.fmkdir(at,name,FALSE);
    IF NOT bio.done THEN bio_error('mkdir',name); done:=FALSE; RETURN END;
    bio.fopen((at),dir,name,'rwx');
    IF NOT bio.done THEN bio_error('open',name); done:=FALSE; RETURN END
  END;
  IF bio.is_dir*bio.kind(dir)={} THEN
    print('is not directory "%s"',name); done:=FALSE; RETURN
  END;
  IF PRO & (e.pro#bio_pro(dir,name)) THEN
    bio.chowner(dir,INTEGER((e.pro>>16)*{0..6}),INTEGER((e.pro<<8)*{0..6}));
    bio_check('chowner',e.fname);
    bio.chaccess(dir,e.pro*{0..15});
    bio_check('chaccess',e.fname)
  END;
  bio.close(at);
  IF NOT bio.done THEN bio_error('close',''); done:=FALSE; RETURN END;
  IF NOT last THEN
    at:=dir; dir:=bio.null; make_through(e,at,dir,i,path,done)
  END
END make_through;

PROCEDURE mkdir(VAR dir: bio.FILE; VAL e: DIR; VAR done: BOOLEAN);
  VAR mode: BITSET;
      name: ARRAY [0..31] OF CHAR;
BEGIN
  IF CD THEN
    bio.dup(dir,(bio.cd)); done:=bio.done; bio_check("dup","."); RETURN
  END;
  done:=FALSE;
  bio.open(dir,e.fname,'rwx');
  IF NOT bio.done & (bio.error#err.no_entry) THEN
    bio_error('open',e.fname); RETURN
  END;
  IF NOT bio.done THEN
    IF NOT query('make directory "%s"?',e.fname) THEN RETURN END;
    make_through(e,bio.null,dir,1,e.fname,done);
    IF NOT done THEN RETURN END
  END;
  IF PRO & (e.pro#bio_pro(dir,e.fname)) THEN
    bio.chowner(dir,INTEGER((e.pro>>16)*{0..6}),INTEGER((e.pro<<8)*{0..6}));
    bio_check('chowner',e.fname);
    bio.chaccess(dir,e.pro*{0..15});
    bio_check('chaccess',e.fname)
  END;
  done:=TRUE;
  IF NOT CLEAN THEN RETURN END;
  bio.dir_walk(dir,0);
  IF NOT bio.done THEN bio_error('dir_walk',e.fname); RETURN END;
  WHILE bio.get_entry(dir,name,mode) DO
    IF mode*(bio.e_esc+bio.e_dir)={} THEN
      IF query('remove  %-32s?',name) THEN
        IF show THEN print('remove  %s',name) END;
        bio.funlink(dir,name); bio_check('unlink',name)
      END
    END
  END;
  IF NOT bio.done THEN bio_error('get_entry',e.fname); RETURN END;
  bio.end_walk(dir);
  IF NOT bio.done THEN bio_error('end_walk',e.fname) END
END mkdir;


PROCEDURE read(inp,dir: bio.FILE; VAL iname: ARRAY OF CHAR;
               VAL f: ITEM; VAL e: DIR);

  VAR t: INTEGER;
     io: BOOLEAN;
    out: bio.FILE;
    ask: ARRAY [0..7] OF CHAR;
   mode: ARRAY [0..3] OF CHAR;

  PROCEDURE copy;
    VAR eof,len: INTEGER;
  BEGIN
    eof:=f.eof;
    WHILE eof>0 DO
      IF eof>BYTES(buf) THEN len:=BYTES(buf) ELSE len:=eof END;
      bio.get(inp,buf,len); io:=io OR NOT bio.done; bio_check('read',iname);
      bio.put(out,buf,len); io:=io OR NOT bio.done; bio_check('write',f.name);
      DEC(eof,len)
    END
  END copy;

BEGIN
  bio.fopen((dir),out,f.name,'w');
  IF NOT bio.done & (bio.error#err.no_entry) THEN
    bio_error('open',f.name); RETURN
  END;
  IF bio.done THEN
    bio.get_attr(out,bio.a_wtime,t);
    bio.close(out);
    IF TIME & (t>=f.wtim) THEN RETURN END;
    ask:="rewrite"
  ELSE
    ask:="create "
  END;
  IF CD THEN str.print(bump,'%s/ -> .',e.fname) ELSE str.print(bump,"%s",e.fname) END;
  IF NOT query('%s  %s/%-32s?',ask,bump,f.name) THEN RETURN END;
  IF show THEN print('%s  %s/%s',ask,bump,f.name) END;

  IF HIDDEN*f.tag={} THEN mode:="w" ELSE mode:="wh" END;
  bio.fcreate((dir),out,f.name,mode,f.eof);
  IF NOT bio.done THEN bio_error('create',f.name); RETURN END;
  IF PRO THEN
    bio.chowner(out,INTEGER((f.pro>>16)*{0..6}),INTEGER((f.pro<<8)*{0..6}));
                                           bio_check('chowner' ,f.name);
    bio.chaccess(out,f.pro*{0..15});       bio_check('chaccess',f.name);
    bio.seek(inp,f.fpos,0);                bio_check('seek',f.name);
  END;
  io:=FALSE;
  copy;
  bio.set_attr(out,bio.a_wtime,f.wtim);  bio_check('set_attr',f.name);
  IF IGNORE OR NOT io THEN bio.close(out) ELSE bio.purge(out) END;
  bio_check('close',f.name)
END read;

PROCEDURE restorearch(inp: bio.FILE; VAL iname: ARRAY OF CHAR);
  VAR e: DIR;                            f: ITEM;
      c: CHAR;                         rfe: INTEGER;
    dir: bio.FILE;                   i,d,p: INTEGER;
   time: INTEGER;                    h,m,s: INTEGER;
   made: BOOLEAN;                  y,mn,dy: INTEGER;
  magic: ARRAY [0..7] OF CHAR;    nd,nf,no: INTEGER;
BEGIN
  bio.check_io(FALSE); bio.get(inp,magic,BYTES(magic));
  IF magic#MAGIC THEN tty.print('illegal magic in "%s"\n'); RETURN END;
  bio.seek(inp,16,1); bio_check('seek',iname); (* DISK label skip *)
  bio_getw(inp,time);
  bio_getw(inp,no);
  tim.unpack(time,y,mn,dy,h,m,s);
  str.print(bump,"%02d/%02d/%02d  %02d:%02d.%02d",dy,mn,y MOD 100,h,m,s);
  IF LIST THEN
    print('LIST "%s" #%d (%s)',iname,no,bump);
  ELSE
    print('RESTORE from "%s" #%d (%s)',iname,no,bump);
  END;
  bio.buffers(inp,2,4096*2);
  bio_getw(inp,rfe);
  bio_getw(inp,rfe);
  bio_getw(inp,rfe);
  bio_getw(inp,rfe);
  bio_getw(inp,nd);
  bio_getw(inp,ALLIGN);
  NEW(e.fname);
  FOR d:=0 TO nd-1 DO
    bio_getw(inp,e.pro);
    bio_getw(inp,nf);
    bio_getw(inp,e.rfe0);
    bio_getw(inp,e.rfe1);
    bio_getw(inp,e.rfe2);
    bio_getw(inp,e.rfe3);
    bio.getch(inp,c);
    RESIZE(e.fname,ORD(c));
    bio.get(inp,e.fname,ORD(c));
    dir:=bio.null;
    all:=FALSE; skip:=FALSE;
    IF LIST THEN print("%s/",e.fname) ELSE mkdir(dir,e,made) END;
    FOR i:=0 TO nf-1 DO
      bio_getw(inp,f.tag);
      bio_getw(inp,f.pro);
      bio_getw(inp,f.wtim);
      bio_getw(inp,f.eof);
      bio_getw(inp,f.rfe0);
      bio_getw(inp,f.rfe1);
      bio_getw(inp,f.rfe2);
      bio_getw(inp,f.rfe3);
      bio_getw(inp,f.fpos);
      bio.getch(inp,c);
      IF ORD(c)>HIGH(f.name) THEN c:=CHAR(HIGH(f.name));
        tty.print('bad archive structure\n')
      END;
      bio.get(inp,f.name,ORD(c));
      IF (ALLIGN#0) & (f.fpos MOD ALLIGN#0) OR (f.fpos=0) THEN
        tty.print('bad archive structure for file "%s"\n',f.name)
      END;
      IF LIST THEN
        str.print(bump,"  %-32s",f.name);
        IF f.eof<1000 THEN str.append(bump,"    %03d",f.eof);
        ELSE               str.append(bump,"%3d,%03d",f.eof DIV 1000,f.eof MOD 1000)
        END;
        tim.unpack(f.wtim,y,mn,dy,h,m,s);
        str.append(bump,"  %02d/%02d/%02d  %02d:%02d.%02d",dy,mn,y MOD 100,h,m,s);
        print(bump);
      ELSIF made THEN
        p:=bio.pos(inp); read(inp,dir,iname,f,e); bio.seek(inp,p,0)
      END
    END;
    bio.close(dir);
    IF made & NOT bio.done THEN bio_error('close',e.fname) END
  END
END restorearch;

PROCEDURE restore(VAR inp: bio.FILE; VAL iname: ARRAY OF CHAR);
  VAR time,t,no,n: INTEGER;  magic: ARRAY [0..7] OF CHAR;
BEGIN
  LOOP
    restorearch(inp,iname);
    IF NOT ROLL OR (bio.kind(inp)*bio.is_disk={}) THEN RETURN END;
    bio.seek(inp,8+16,0);
    IF NOT bio.done THEN bio_error('seek',iname);  RETURN END;
    bio_getw(inp,time);
    IF NOT bio.done THEN bio_error('read',iname);  RETURN END;
    bio_getw(inp,no);
    IF NOT bio.done THEN bio_error('read',iname);  RETURN END;
    bio.close(inp);
    IF NOT bio.done THEN bio_error('close',iname); RETURN END;
    tty.print("\n");
    REPEAT
      LOOP
        IF NOT env.ipr() THEN tty.set_color(+1);
          tty.print("\r *** insert ANOTHER disk ***  \r")
        END;
        tim.delay(3,tim.sec);
        IF NOT env.ipr() THEN
          tty.print("\r                              \r")
        END;
        tim.delay(3,tim.sec);
        bio.open(inp,iname,'rc'); magic:="" 0c 0c 0c 0c 0c 0c 0c;
        IF bio.done THEN bio.get(inp,magic,8) END;
        IF bio.done & (magic=MAGIC) THEN
          IF bio.done THEN bio.seek(inp,8+16,0)  END;
          IF bio.done THEN bio_getw(inp,t)       END;
          IF bio.done THEN bio_getw(inp,n)       END;
          IF bio.done & ((t#time) OR (n#no)) THEN bio.close(inp); EXIT END
        END;
        bio.close(inp)
      END;
      bio.open(inp,iname,'r');
    UNTIL bio.done;
    IF NOT env.ipr() THEN tty.set_color(0) END
  END
END restore;

PROCEDURE pusage;
BEGIN
  std.print(
      '   "kar"  archives backup and restore utility program (c) KRONOS\n'
      'usage:\n'
      '    kar  [-qrp] pattern [ destinator ]  [allign=N]\n'
      '    kar  [-qrp] @source_tree_def_file\n'
      '    kar  [-qltcRu] archive_file\n'
      '\n');
  std.print(
      '     -h help\n'
      '     -q save/restore all files without query\n'
      '     -r remove source files after archiving\n'
      '\n'
      '     -l list archive\n'
      '     -R rolling restore\n'
      '     -d restore to current directory\n'
      '     -c clean directory BEFORE restore\n');
  std.print(
      '     -t restore without time check\n'
      '     -u restore without user & protection bits\n'
      '     -i ignore i/o errors\n'
      '\n'
      '                                     Leopold, Mar 5 91\n')
END pusage;

VAR   f: bio.FILE;
   name: STRING;
  magic: ARRAY [0..7] OF CHAR;

PROCEDURE check_magic(): BOOLEAN;
BEGIN
  bio.get(f,magic,BYTES(magic)); bio_assert('read',name);
  bio.seek(f,0,0);               bio_assert('seek',name);
  RETURN magic=MAGIC
END check_magic;

BEGIN
  tty.set_cursor(0);
  IF arg.flag('-','h') OR (HIGH(arg.words)<0) THEN pusage; HALT END;
  all:=FALSE;  ALL:=FALSE; skip:=FALSE;
  f:=bio.null;
  IF NOT arg.number('allign',ALLIGN) THEN ALLIGN:=0 END;
  IGNORE:=   arg.flag('-','i');
  QUERY:=NOT arg.flag('-','q');
  CLEAN:=    arg.flag('-','c');
  TIME :=NOT arg.flag('-','t') & NOT CLEAN;
  DEL  :=    arg.flag('-','r');
  LIST :=    arg.flag('-','l');
  PACK :=NOT arg.flag('-','p');
  CD   :=    arg.flag('-','d');
  ROLL :=    arg.flag('-','R');
  PRO  :=NOT arg.flag('-','u');
  NEW(name,BYTES(arg.words[0])+6);
  IF arg.words[0][0]='@' THEN
    str.sub_str(name,(arg.words[0]),1,BYTES(name));
    bio.open(f,name,'r'); bio_assert('open',arg.words[0]);
    save(f,name)
  ELSE
    str.print(name,"%s",arg.words[0]);
    IF ext_is_kar(name) THEN
      bio.open(f,name,'r'); bio_assert('open',name);
      IF NOT check_magic() THEN
        tty.print('illegal magic in "%s"\n',name); HALT(err.unsuitable)
      ELSE
        restore(f,name)
      END;
      HALT
    END;
    bio.open(f,name,'r');
    IF bio.done & check_magic() THEN restore(f,name); HALT END;
    bio.close(f);
    str.print(name,"%s.kar",arg.words[0]);
    bio.open(f,name,'r');
    IF bio.done & check_magic() THEN restore(f,name); HALT END;
    str.print(name,"%s",arg.words[0]);
    bio.close(f); save(bio.null,"")
  END
END kar.
