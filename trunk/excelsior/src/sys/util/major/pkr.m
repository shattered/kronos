MODULE pkr; (*$N+ Leo 07-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;           IMPORT  req: defRequest;
IMPORT   fw: fsWalk;           IMPORT  tim: Time;
IMPORT  bio: BIO;              IMPORT  tty: Terminal;
IMPORT  err: defErrors;        IMPORT  std: StdIO;
IMPORT  mem: Heap;             IMPORT  env: tskEnv;
IMPORT  lzw: LZW0;             IMPORT  str: Strings;
IMPORT  arg: tskArgs;          IMPORT  key: Keyboard;


WITH STORAGE: mem;

CONST ok = err.ok;

TYPE
  STR4   = ARRAY [0..  3] OF CHAR;
  STR8   = ARRAY [0..  7] OF CHAR;
  STR32  = ARRAY [0.. 31] OF CHAR;
  STR64  = ARRAY [0.. 63] OF CHAR;
  STR128 = ARRAY [0..127] OF CHAR;
  STR256 = ARRAY [0..255] OF CHAR;

CONST MAGIC0=574C6B70h;
      FNIL = -1;
VAR
  DSK     : bio.FILE;
  ARCH,DIR: bio.FILE;
  DIRname : STR32;
  ARCHname: STR128;

VAR (* flags *)
  after,before: INTEGER;
  DISK: BOOLEAN;
  xmem: BOOLEAN;
  qry : BOOLEAN;
  skip: BOOLEAN;
  all : BOOLEAN;
  path: BOOLEAN;
  bcup: BOOLEAN;
  relp: BOOLEAN;

TYPE
  ENTRY = POINTER TO BODY;
  BODY  = RECORD (* 20*4 = 80 bytes *)
            next: ENTRY;
            name: STR32;
            mode: BITSET;
            kind: BITSET;
            uid : INTEGER;
            gid : INTEGER;
            eof : INTEGER;
            pro : BITSET;
            ctim: INTEGER;
            wtim: INTEGER;
            rfe0: INTEGER;
            rfe1: INTEGER;
            rfe2: INTEGER;
            rfe3: INTEGER;
            apos: INTEGER;
            err : INTEGER;
          END;

  ENTRYs = DYNARR OF ENTRY;


PROCEDURE query(): BOOLEAN;
  VAR ch,c: CHAR;
BEGIN
  IF NOT qry OR env.ipr() THEN RETURN TRUE END;
  tty.set_cursor(1);
  LOOP
    key.read(ch); c:=CAP(ch);
    IF (c='Y') OR (c='N') OR (c='Q') OR (ch=key.can) OR (c='A') OR (c='I') THEN
      EXIT
    END;
    key.bell(1)
  END;
  IF ch=key.can THEN tty.print("^X") ELSE tty.print("%c",ch) END;
  IF c ='Q'     THEN HALT        END;
  IF c ='I'     THEN skip:=TRUE  END;
  IF ch='A'     THEN qry :=FALSE END;
  IF ch='a'     THEN all :=TRUE  END;
  IF ch=key.can THEN qry:=FALSE  END;
  tty.set_cursor(0);
  RETURN (c='Y') OR (c='A') OR (ch=key.can)
END query;

CONST
  MONTHs = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ";

VAR (* this variables declared globaly to decrise P-stack *)
   fname: STR256;
   dname: STR256;

PROCEDURE fw_error(VAL name: ARRAY OF CHAR);
BEGIN
  IF fw.error#err.bad_parm THEN
    tty.perror(fw.error,'\n"%s" %%s "%s"\n',name,bio.ename)
  ELSE
    tty.perror(fw.error,'\n"%s"\n%*c^ %%s\n',name,fw.pos+1,' ')
  END
END fw_error;

PROCEDURE bio_error(VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'\n"%s" %%s "%s"\n',name,bio.ename)
END bio_error;

PROCEDURE fatal(i: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(i,'\n%%s "%s"\n',name); HALT(i)
END fatal;

PROCEDURE error(i: INTEGER; VAL name: ARRAY OF CHAR);
BEGIN
  tty.perror(i,'\n%%s "%s"\n',name)
END error;

PROCEDURE bio_fatal(VAL name: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=bio.error; tty.perror(i,'\n"%s" %%s "%s"\n',name,bio.ename); HALT(i)
END bio_fatal;

PROCEDURE rprint(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR msg: STR64;
BEGIN
  str.print(msg,fmt,args);
  IF NOT env.ipr() THEN tty.print  ("%s\r",msg)
  ELSE                  env.put_str(env.info,msg,TRUE)
  END;
END rprint;

CONST
  spacer =
  "                                                                          ";
  OFS  = 4096;    (* don't use first block *)
  TAIL = 1024*5;  (* don't use last  track *)

PROCEDURE reopen(mode: ARRAY OF CHAR);
BEGIN
  bio.close(DSK);
  bio.open(DSK,ARCHname,mode);
  IF NOT bio.done THEN bio_error(ARCHname) END;
END reopen;

VAR spc: req.REQUEST;

PROCEDURE readspec;
  VAR buf: ARRAY [0..4095] OF CHAR;
BEGIN
  spc.op :=req.MOUNT;
  spc.buf:=SYSTEM.ADR(buf);
  spc.len:=BYTES(buf);
  bio.doio(DSK,spc);

  spc.op :=req.GET_SPEC;
  bio.doio(DSK,spc);
  IF NOT bio.done THEN spc.secsize:=1024 END
END readspec;

TYPE LABEL = ARRAY [0..3] OF INTEGER;

PROCEDURE insertfirstdisk;
  VAR done: BOOLEAN;
BEGIN
  LOOP
    tim.delay(1,tim.sec);
    rprint("Insert archive disk");
    reopen("rwc");
    done:=bio.done;
    IF done THEN readspec; done:=bio.done END;
    IF done THEN EXIT END;
    tim.delay(4,tim.sec);
    rprint(spacer)
  END;
  reopen("rw")
END insertfirstdisk;

PROCEDURE insertanotherdisk(c,n: INTEGER; lab: LABEL);
  VAR msg: STR64;
      lbl: LABEL;
      eof: INTEGER;
     done: BOOLEAN;
BEGIN
  eof:=bio.eof(DSK);
  LOOP
    IF n<=1 THEN   msg:="Insert archive disk"
    ELSE str.print(msg, "Insert archive disk #%d of [0..%d] disks",c,n-1)
    END;
    tim.delay(1,tim.sec);
    rprint(msg);
    reopen("rwc");
    done:=bio.done;
    IF done THEN readspec; done:=bio.done END;
    IF done & (eof#bio.eof(DSK)) THEN
      rprint(msg,"Unsiutable disk size, must be %d",eof);  done:=FALSE
    END;
    IF done THEN bio.seek(DSK,OFS-BYTES(lab),0);  done:=bio.done END;
    IF done THEN bio.get (DSK,lbl,BYTES(lbl));    done:=bio.done END;
    IF done & (lbl[0]#lab[0]) OR (lbl[1]#lab[1]) OR (lbl[2]#lab[2]) THEN
      EXIT
    END;
    tim.delay(4,tim.sec);
    rprint(spacer)
  END;
  reopen("rw")
END insertanotherdisk;

PROCEDURE rewritearchive;

  VAR buf: POINTER TO ARRAY [0..16*1024-1] OF CHAR;
      lab: LABEL;
     done: BOOLEAN;
      nodisks,diskno,volsize,disksize,eof,len,l,pos: INTEGER;

  PROCEDURE percent(VAL op: ARRAY OF CHAR);
  BEGIN
    IF nodisks>1 THEN
      rprint('%s "%s" %d of [0..%d] %d%%   ',op,ARCHname,diskno,nodisks-1
             ,(bio.pos(DSK)-OFS)*100 DIV volsize)
    ELSE
      rprint('%s "%s" %d%%   ',op,ARCHname
             ,(bio.pos(DSK)-OFS)*100 DIV volsize)
    END
  END percent;

BEGIN
  bio.open(DSK,ARCHname,'rw');
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  bio.buffers(ARCH,0,0);
  bio.seek(ARCH,0,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  insertfirstdisk;
  disksize:=(bio.eof(DSK)-OFS-BYTES(lab)-TAIL);
  nodisks :=(bio.eof(ARCH)+disksize-1) DIV disksize;
  diskno:=0;
  NEW(buf);
  eof:=bio.eof(ARCH);
  pos:=0;
  LOOP
    len:=disksize;
    IF eof<len THEN len:=eof END;
    volsize:=len;

    lab[0]:=volsize;
    lab[1]:=diskno+nodisks<<8+0ABh<<16+07h>>8;
    lab[2]:=tim.time();
    lab[3]:=0;   (* rfe *)

    bio.seek(ARCH,pos,0);
    IF NOT bio.done THEN bio_fatal(ARCHname) END;

    bio.seek(DSK,OFS-BYTES(lab),0);
    done:=bio.done;
    IF NOT done THEN
      bio_error(ARCHname)
    ELSE
      bio.put(DSK,lab,BYTES(lab));  done:=bio.done;
      IF NOT done THEN bio_error(ARCHname) END
    END;
    WHILE (len>0) & done DO
      IF len>BYTES(buf^) THEN l:=BYTES(buf^) ELSE l:=len END;
      bio.get(ARCH,buf^,l);
      IF NOT bio.done THEN bio_fatal(ARCHname) END;
      bio.put(DSK,buf^,l);  done:=bio.done;
      IF NOT done THEN bio_error(ARCHname) END;
      percent('writting');
      DEC(len,l)
    END;
    IF done THEN
      (* verify *)
      bio.seek(DSK,0,0);
      IF NOT bio.done THEN bio_error(ARCHname); done:=FALSE END;
      len:=volsize+OFS;
      WHILE (len>0) & done DO
        IF len>BYTES(buf^) THEN l:=BYTES(buf^) ELSE l:=len END;
        bio.get(DSK,buf^,l);
        IF NOT bio.done THEN bio_error(ARCHname); done:=FALSE END;
        percent('verifing');
        DEC(len,l)
      END
    END;
    IF done THEN
      IF NOT env.ipr() & (nodisks>1) THEN
        rprint(spacer);
        rprint("Disk #%d of [0..%d] updated\n",diskno,nodisks-1)
      END;
      DEC(eof,volsize); INC(pos,volsize); INC(diskno)
    ELSE
      tty.print(spacer);
      tty.print("\nThis disk can`t be updated. Try another one\n")
    END;
    IF diskno=nodisks THEN EXIT END;
    insertanotherdisk(diskno,nodisks,lab)
  END;
  rprint(spacer);
  bio.close(DSK);
  bio.purge(ARCH);
  DISPOSE(buf)
END rewritearchive;

PROCEDURE insertnext(c: INTEGER; VAR n: INTEGER; VAR lab: LABEL);
  VAR msg: STR64;  done: BOOLEAN;
BEGIN
  LOOP
    IF n<=1 THEN   msg:="Insert archive disk"
    ELSE str.print(msg, "Insert archive disk #%d of [0..%d] disks",c,n-1)
    END;
    tim.delay(1,tim.sec);
    rprint(msg);
    reopen("rwc");
    done:=bio.done;
    IF done THEN readspec; done:=bio.done END;
    IF done THEN bio.seek(DSK,OFS-BYTES(lab),0); done:=bio.done END;
    IF done THEN bio.get (DSK,lab,BYTES(lab)  ); done:=bio.done END;
    IF done & (lab[1] DIV 10000h=7ABh) THEN
      n:=(lab[1] MOD 10000h) DIV 256;
      IF lab[1] MOD 256 = c THEN EXIT END
    END;
    tim.delay(4,tim.sec);
    rprint(spacer)
  END;
  rprint(spacer);
  reopen("rw");
  bio.seek(DSK,OFS,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END
END insertnext;

PROCEDURE rereadarchive;

  VAR buf: POINTER TO ARRAY [0..16*1024-1] OF CHAR;
      lab: LABEL;
      nodisks,diskno,volsize,len,l: INTEGER;

  PROCEDURE percent(VAL op: ARRAY OF CHAR);
  BEGIN
    IF nodisks>1 THEN
      rprint('%s "%s" %d of [0..%d] %d%%   ',op,ARCHname,diskno,nodisks-1
             ,(bio.pos(DSK)-OFS)*100 DIV volsize)
    ELSE
      rprint('%s "%s" %d%%   ',op,ARCHname
             ,(bio.pos(DSK)-OFS)*100 DIV volsize)
    END
  END percent;

BEGIN
  NEW(buf);
  bio.close(ARCH);
  bio.open(DSK,ARCHname,'rw');
  IF NOT bio.done THEN bio_fatal(ARCHname) END;

  bio.create(ARCH,"",'rw',4096);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;

  diskno := 0;
  nodisks:=-1;
  REPEAT
    insertnext(diskno,nodisks,lab);
    len:=lab[0];
    volsize:=len;
    bio.extend(ARCH,bio.pos(ARCH)+volsize);
    IF NOT bio.done THEN bio_fatal(ARCHname) END;
    WHILE len>0 DO
      IF len>BYTES(buf^) THEN l:=BYTES(buf^) ELSE l:=len END;
      bio.get(DSK,buf^,l);
      IF NOT bio.done THEN bio_fatal(ARCHname) END;
      bio.put(ARCH,buf^,l);
      IF NOT bio.done THEN bio_fatal(ARCHname) END;
      percent('reading');
      DEC(len,l)
    END;
    rprint(spacer);
    IF nodisks>1 THEN
      rprint("Disk #%d of [0..%d] restored\n",diskno,nodisks-1)
    END;
    INC(diskno)
  UNTIL diskno=nodisks;
  rprint(spacer);
  bio.close(DSK);
  DISPOSE(buf)
END rereadarchive;

PROCEDURE checkname;
  VAR i: INTEGER;
     ch: CHAR;
BEGIN
  i:=0;
  WHILE (i<=HIGH(ARCHname)) & (ARCHname[i]#0c) DO
    ch:=ARCHname[i];
    IF (ch='*')OR(ch='?')OR(ch='[')OR(ch=']')OR(ch='{')OR(ch='}') THEN
      tty.print('illegal character "%c" in arcive name "%s"\n',ch,ARCHname);
      HALT(err.bad_name)
    END;
    INC(i)
  END;
END checkname;

PROCEDURE openarchive(mode: ARRAY OF CHAR);
BEGIN
  checkname;
  bio.open(ARCH,ARCHname,mode);
  IF bio.done & (bio.kind(ARCH)*bio.is_disk#{}) THEN
    DISK:=TRUE; bio.buffers(ARCH,2,4096); RETURN
  END;
  DISK:=FALSE;
  str.print(fname,"%s.pkr",ARCHname);
  str.copy (ARCHname,fname);
  bio.open(ARCH,ARCHname,mode);
  IF bio.done THEN bio.buffers(ARCH,2,4096); RETURN  END;
  IF bio.error#err.no_entry THEN bio_fatal(ARCHname) END;
  ARCH:=bio.null
END openarchive;

PROCEDURE createarchive;
  VAR i: INTEGER;
BEGIN
  checkname;
  openarchive('w'); (* to check permission rights *)
  IF NOT DISK & (ARCH#bio.null) THEN
    bio.close(ARCH);
    IF qry & NOT env.ipr() THEN
      tty.print('archive "%s" already exist. Rewrite? ',ARCHname);
      IF NOT query() THEN tty.print("\n"); HALT END;
      tty.print("\n")
    END
  END;
  IF DISK THEN
    bio.create(ARCH,"",'rw',4096)
  ELSE
    bio.create(ARCH,ARCHname,'rw',4096)
  END;
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  bio.buffers(ARCH,2,4096);
  bio.create(DIR,"",'rw',4096);
  DIRname:="D*I*R";
  IF NOT bio.done THEN bio_fatal(DIRname) END;
  bio.buffers(DIR,1,4096);
  i:=FNIL;
  bio.write(ARCH,SYSTEM.ADR(i),4); (* reference to directory *)
  IF NOT bio.done THEN bio_fatal(ARCHname) END
END createarchive;

PROCEDURE unpackattrs(f: bio.FILE; VAR b: BODY);
  PROCEDURE check_error;
  BEGIN
    IF NOT bio.done THEN b.err:=bio.error; bio_error(b.name) END
  END check_error;
BEGIN
  WITH b DO
    err:=0;    wtim:=tim.time();
    uid:=0;    ctim:=wtim;
    gid:=0;    apos:=FNIL;
    eof:=0;    pro :={};
    rfe0:=0;   rfe2:=0;
    rfe1:=0;   rfe3:=0;

    bio.get_attr(f,bio.a_gid  ,gid );  check_error;
    bio.get_attr(f,bio.a_uid  ,uid );  check_error;
    bio.get_attr(f,bio.a_pro,  pro );  check_error;
    bio.get_attr(f,bio.a_ctime,ctim);  check_error;
    bio.get_attr(f,bio.a_wtime,wtim);  check_error;
    kind:=bio.kind(f);                 check_error;
    eof :=bio.eof(f);                  check_error;
  END
END unpackattrs;

PROCEDURE getattrs(dir: bio.FILE; entry: ENTRY);

  PROCEDURE check_error;
  BEGIN
    IF NOT bio.done THEN entry^.err:=bio.error; bio_error(entry^.name) END
  END check_error;

  VAR f: bio.FILE;

BEGIN
  bio.fopen((dir),f,entry^.name,"r");
  IF NOT bio.done THEN
    entry^.err:=bio.error; bio_error(entry^.name); RETURN
  END;
  unpackattrs(f,entry^);
  bio.close(f)
END getattrs;

PROCEDURE size2str(size: INTEGER; VAR s: ARRAY OF CHAR);
  CONST spaces="        ";
BEGIN
  IF size>=1000000 THEN
    str.print(s,"%2d,%03d,%03d "
               ,size DIV 1000000,size DIV 1000 MOD 1000,size MOD 1000)
  ELSIF size>=1000 THEN
    str.print(s,"%.*s%3d,%03d ",3,spaces,size DIV 1000,size MOD 1000)
  ELSE
    str.print(s,"%.*s%03d ",7,spaces,size)
  END
END size2str;

--------------------- DIRECTORY --------------------------------


PROCEDURE seek(pos: INTEGER);
BEGIN
  bio.seek(DIR,pos,0);
  IF NOT bio.done THEN bio_fatal(DIRname) END
END seek;

PROCEDURE putc(ch: SYSTEM.WORD);
BEGIN
  bio.putch(DIR,CHAR(ch));
  IF NOT bio.done THEN bio_fatal(DIRname) END
END putc;

PROCEDURE putw(w: SYSTEM.WORD);
BEGIN
  bio.write(DIR,SYSTEM.ADR(w),4);
  IF NOT bio.done THEN bio_fatal(DIRname) END
END putw;

PROCEDURE puts(VAL s: ARRAY OF CHAR);
BEGIN
  bio.putstr(DIR,s,0);
  IF NOT bio.done THEN bio_fatal(DIRname) END
END puts;

PROCEDURE getc(VAR w: SYSTEM.WORD);
  VAR ch: CHAR;
BEGIN
  ch:=0c;
  bio.getch(DIR,ch);
  IF NOT bio.done THEN bio_fatal(DIRname) ELSE w:=ch END
END getc;

PROCEDURE getw(VAR w: SYSTEM.WORD);
BEGIN
  bio.read(DIR,SYSTEM.ADR(w),4);
  IF NOT bio.done THEN bio_fatal(DIRname) END
END getw;

PROCEDURE gets(VAR s: ARRAY OF CHAR; len: INTEGER);
BEGIN
  bio.read(DIR,SYSTEM.ADR(s),len);
  IF NOT bio.done THEN bio_fatal(DIRname) ELSE s[len]:=0c END
END gets;

----------------------------------------------------------------

VAR
  filco : INTEGER;
  dirco : INTEGER;
  direof: INTEGER;

PROCEDURE putentry(VAL b: BODY; VAL fname: ARRAY OF CHAR);
  VAR len: INTEGER;
BEGIN
  len:=str.len(fname);
  IF len>255 THEN
    tty.print('too long path name\n"%s"\n',fname); HALT(err.too_large)
  END;
  WITH b DO
    putw(MAGIC0);
    putw(apos);  (* position of file body *)
    putw(eof );
    putw(mode);
    putw(kind);
    putw(uid );
    putw(gid );
    putw(pro );
    putw(ctim);
    putw(wtim);
    putw(rfe0);
    putw(rfe1);
    putw(rfe2);
    putw(rfe3);
    putc(len );
    puts(fname);
  END
END putentry;

PROCEDURE packfiles(dir: bio.FILE; VAL ents: ENTRYs; maxnamelen: INTEGER);
  VAR   f: bio.FILE;
        b: BODY;
      msg: STR64;
     bump: STR64;
    lname: STR256;
  proceed: BOOLEAN;
  i,p,len: INTEGER;
BEGIN
  INC(maxnamelen,str.len(dname)+1);
  skip:=FALSE;
  FOR i:=0 TO HIGH(ents) DO
    IF dname="/"0c THEN str.print(fname,"/%s",ents[i]^.name)
    ELSE        str.print(fname,"%s/%s",dname,ents[i]^.name)
    END;
    proceed:=NOT qry OR all OR env.ipr();
    IF NOT proceed THEN
      tty.print("%-*s ?",maxnamelen,fname);
      proceed:=query();
      IF skip           THEN tty.print("\n"); RETURN
      ELSIF NOT proceed THEN tty.print("\n");
      ELSE                   tty.print("\r")
      END
    END;
    IF proceed THEN
      IF dirco=0 THEN
        str.copy(lname,dname);
        bio.splitpathname(lname,b.name);
        IF NOT bio.done THEN bio_fatal(dname) END;
        b.mode:=bio.e_dir;
        unpackattrs(dir,b);
        b.eof :=maxnamelen;
        putentry(b,dname)
      END;
      INC(dirco);
      bio.fopen((dir),f,ents[i]^.name,'r');
      IF NOT bio.done THEN bio_fatal(fname) END;
      ents[i]^.apos:=bio.pos(ARCH);
      putentry(ents[i]^,fname);

      size2str(ents[i]^.eof,bump);
      IF NOT env.ipr() THEN
        str.print(msg,"");
        tty.print("%-*s %s",maxnamelen,fname,bump);
      ELSE
        str.print(msg,"%-*s %s",maxnamelen,fname,bump)
      END;

      lzw.pack(f,ARCH,bio.eof(f),len,xmem);
      IF NOT lzw.done THEN tty.print("\n"); fatal(lzw.error,fname) END;
      INC(direof,len);

      IF len>=bio.eof(f) THEN   str.append(msg," stored")
      ELSE str.append(msg," %d%%",len*100 DIV bio.eof(f))
      END;
      IF env.ipr() THEN env.put_str(env.info,msg,TRUE)
      ELSE              tty.print("%s\n",msg)
      END;
      bio.close(f)
    END
  END
END packfiles;

PROCEDURE packdir(dir: bio.FILE; list: ENTRY; co: INTEGER);
  VAR   msg,bump: STR64;
         entries: ENTRYs;
      i,max,nlen: INTEGER;
BEGIN
  NEW(entries,co);
  ASSERT(list#NIL);
  i:=0; max:=0;
  REPEAT
    co:=co-1;
    getattrs(dir,list);
    IF (list^.err=err.ok) & (after<=list^.wtim) & (list^.wtim<=before) THEN
      entries[i]:=list; INC(i);
      nlen:=str.len(list^.name);
      IF nlen>max THEN max:=nlen END;
    END;
    list:=list^.next
  UNTIL co=0;
  ASSERT(list=NIL);
  RESIZE(entries,i);
  IF i=0 THEN RETURN END;
  dirco:=0;  direof:=0;
  packfiles(dir,entries,max);
  INC(filco,dirco);
  IF dirco>0 THEN
    size2str(direof,bump);
    str.print(msg,'%s/  T O T A L %s bytes in %d files',dname,bump,dirco);
    IF env.ipr() THEN env.put_str(env.info,msg,TRUE)
    ELSE              tty.print("%s\n",msg)
    END;
  END;
  DISPOSE(entries)
END packdir;


PROCEDURE reverse(VAR list: ENTRY);
  VAR r,e: ENTRY;
BEGIN
  IF list=NIL THEN RETURN END;
  r:=NIL;
  WHILE list#NIL DO e:=list;  list:=e^.next;  e^.next:=r;  r:=e END;
  list:=r
END reverse;


PROCEDURE packtree(VAL patt: ARRAY OF CHAR);
  VAR co: INTEGER;       dir: bio.FILE;
    tree: fw.TREE;      list: ENTRY;
    name: STR32;        mode: BITSET;
   entry: ENTRY;        next: ENTRY;
BEGIN
  fw.walk(tree,patt,FALSE);
  IF NOT fw.done THEN fw_error(patt); HALT(fw.error) END;
  WHILE fw.next_dir(tree) DO
    dir:=fw.dir(tree);
    IF NOT fw.done THEN fw_error(patt) END;
    IF relp THEN
      fw.dpath(tree,dname)
    ELSE
      bio.fname(dir,dname);
      IF NOT bio.done THEN bio_error(patt) END
    END;
    IF fw.done THEN
      list:=NIL; co:=0;
      WHILE fw.next_entry(tree,name,mode) DO
        IF NOT fw.done THEN fw_error(name) END;
        IF (bio.e_dir+bio.e_esc+bio.e_sys)*mode={} THEN
          NEW(entry);
          entry^.next:=list;
          entry^.name:=name;
          entry^.err :=0;
          entry^.mode:=mode;
          list:=entry;
          INC(co)
        END
      END;
      IF NOT fw.done THEN fw_error(patt) END;
      IF co#0 THEN
        reverse(list);
        packdir(dir,list,co)
      END;
      WHILE list#NIL DO
        next:=list^.next; DISPOSE(list); list:=next
      END
    ELSE
      fw_error(dname)
    END
  END;
  IF NOT fw.done THEN fw_error(patt) END;
  fw.dispose(tree)
END packtree;

PROCEDURE verifyandclose;
  VAR eof: INTEGER;
      len: INTEGER;
      buf: POINTER TO ARRAY [0..16*1024-1] OF CHAR;
      msg: STR64;
     bump: STR64;
BEGIN
  IF filco=0 THEN bio.purge(ARCH); bio.purge(DIR); RETURN END;
  bio.seek (ARCH,0,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  len:=bio.eof(ARCH);
  bio.write(ARCH,SYSTEM.ADR(len),4);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  bio.seek (ARCH,len,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;

  seek(0);
  lzw.pack(DIR,ARCH,bio.eof(DIR),len,xmem);
  IF NOT lzw.done THEN fatal(lzw.error,DIRname) END;
  bio.purge(DIR);

  bio.flush(ARCH);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  bio.buffers(ARCH,0,0);

  size2str(bio.eof(ARCH),bump);
  str.print(msg,'Archive "%s" %s bytes in %d files',ARCHname,bump,filco);
  IF env.ipr() THEN env.put_str(env.info,msg,TRUE)
  ELSE              tty.print("\n%s\n",msg)
  END;

  IF DISK THEN
    rewritearchive
  ELSE
    NEW(buf);
    eof:=bio.eof(ARCH);
    bio.seek(ARCH,0,0);
    IF NOT bio.done THEN bio_fatal(ARCHname) END;
    WHILE eof>0 DO
      IF eof>BYTES(buf^) THEN len:=BYTES(buf^) ELSE len:=eof END;
      bio.get(ARCH,buf^,len);
      IF NOT bio.done    THEN bio_fatal(ARCHname) END;
      DEC(eof,len)
    END;
    bio.cut(ARCH,bio.eof(ARCH));
    IF NOT bio.done THEN bio_fatal(ARCHname) END;
    bio.close(ARCH);
    IF NOT bio.done THEN bio_fatal(ARCHname) END;
    DISPOSE(buf)
  END
END verifyandclose;

----------------------------------------------------------------

PROCEDURE opendir;
  VAR pos,len: INTEGER;
BEGIN
  IF ARCH=bio.null THEN
    tty.print('no such archive "%s"\n',ARCHname); HALT(err.no_entry)
  END;
  IF DISK THEN rereadarchive END;
  bio.create(DIR,"",'rw',4096);
  DIRname:="D*I*R";
  IF NOT bio.done THEN bio_fatal(DIRname) END;
  bio.buffers(DIR,1,4096);
  bio.seek(ARCH,0,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  bio.read(ARCH,SYSTEM.ADR(pos),4);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  IF pos=FNIL THEN fatal(err.bad_desc,ARCHname) END;
  bio.seek(ARCH,pos,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END;
  lzw.unpack(ARCH,DIR,len);
  IF NOT lzw.done THEN fatal(lzw.error,ARCHname) END;
  bio.seek(ARCH,0,0);
  IF NOT bio.done THEN bio_fatal(ARCHname) END
END opendir;

VAR back: INTEGER;

PROCEDURE nextentry(VAR b: BODY): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  back:=bio.pos(DIR);
  IF bio.pos(DIR)>=bio.eof(DIR) THEN RETURN FALSE END;
  getw(i);
  IF i#MAGIC0 THEN fatal(err.bad_desc,ARCHname) END;
  WITH b DO
    getw(apos);  (* position of file body *)
    getw(eof );
    getw(mode);
    getw(kind);
    getw(uid );
    getw(gid );
    getw(pro );
    getw(ctim);
    getw(wtim);
    getw(rfe0);
    getw(rfe1);
    getw(rfe2);
    getw(rfe3);
    getc(i);
    gets(fname,i)
  END;
  RETURN TRUE
END nextentry;

PROCEDURE listarchive;
  VAR e: BODY;
      s: STR64;
BEGIN
  opendir;
  seek(0);
  WHILE nextentry(e) DO
    IF e.kind*bio.is_dir#{} THEN s:="       dir "
    ELSE size2str(e.eof,s)
    END;
    std.print("%s %s\n",s,fname)
  END
END listarchive;


PROCEDURE mkdir(fname: ARRAY OF CHAR);
  VAR i,j: INTEGER;
      d,f: bio.FILE;
    dname: STR256;
BEGIN
--tty.print("makedir  (%s)\n",fname);
  i:=str.len(fname);
  LOOP
    str.sub_str(dname,fname,0,i); dname[i]:=0c;
    bio.open(d,dname,'wr');
    IF bio.done & (bio.kind(d)*bio.is_dir#{}) THEN EXIT END;
    IF i=1 THEN
      tty.print('\ncan`t open root "/" for r/w\n'); HALT(err.sec_vio)
    END;
    REPEAT DEC(i) UNTIL (i=1) OR (fname[i]='/')
  END;
  IF (i=1) & (dname="/"0c) THEN DEC(i) END;
  LOOP
    j:=i+1;
    REPEAT INC(i) UNTIL (i=HIGH(fname)) OR (fname[i]='/') OR (fname[i]=0c);
    str.sub_str(dname,fname,j,i-j); dname[i-j]:=0c;
    bio.fmkdir(d,dname,FALSE);
    IF NOT bio.done THEN bio_error(fname); EXIT END;
    bio.fopen((d),f,dname,'rw');
    IF NOT bio.done THEN bio_error(fname); EXIT END;
    bio.close(d);
    d:=f; f:=bio.null;
    IF (i=HIGH(fname)) OR (fname[i]=0c) THEN EXIT END
  END;
  bio.close(d)
END mkdir;

PROCEDURE createfile(VAR f: bio.FILE; VAL e: BODY; fullname: ARRAY OF CHAR);
  VAR root,dir: bio.FILE;
BEGIN
  f:=bio.null;
  IF path & (e.kind*bio.is_dir#{}) THEN
    bio.open(f,fname,'wr');
    IF NOT bio.done THEN
      mkdir(fname);
      IF NOT bio.done THEN bio_error(fname); RETURN END;
      bio.open(f,fname,'wr');
      IF NOT bio.done THEN bio_error(fname); RETURN END;
    END;
    (* set attrs *)
    bio.chowner(f,e.uid,e.gid);
    IF NOT bio.done THEN bio_error(fname); RETURN END;
    bio.chaccess(f,e.pro*{0..15});
    IF NOT bio.done THEN bio_error(fname); RETURN END;
    bio.close(f);
    f:=bio.null;
    RETURN
  END;
  IF e.mode*bio.e_hidden#{} THEN
    bio.create(f,fname,'wh',e.eof)
  ELSE
    bio.create(f,fname,'w',e.eof)
  END;
  IF NOT bio.done THEN bio_error(fname); RETURN END;
END createfile;

PROCEDURE setattrs(f: bio.FILE; VAL e: BODY; VAL fname: ARRAY OF CHAR);
BEGIN
  bio.chowner(f,e.uid,e.gid);
  IF NOT bio.done THEN bio_error(fname); RETURN END;
  bio.chaccess(f,e.pro*{0..15});
  IF NOT bio.done THEN bio_error(fname); RETURN END;
  bio.set_attr(f,bio.a_ctime,e.ctim);
  IF NOT bio.done THEN bio_error(fname); RETURN END;
  bio.set_attr(f,bio.a_wtime,e.wtim);
  IF NOT bio.done THEN bio_error(fname); RETURN END;
END setattrs;

PROCEDURE unpacktree;
  VAR   e: BODY;
        t: BODY;
        f: bio.FILE;
      msg: STR64;
      len: INTEGER;
     bump: STR64;
    isdir: BOOLEAN;
   maxlen: INTEGER;
  proceed: BOOLEAN;
BEGIN
  opendir;
  seek(0);
  WHILE nextentry(e) DO
    isdir:=e.kind*bio.is_dir#{};
    IF isdir THEN
      maxlen:=e.eof;
      IF NOT path THEN maxlen:=maxlen-str.len(fname) END
    END;

    proceed:=TRUE;
    IF NOT isdir THEN
      bio.open(f,fname,'rw');
      IF bio.done & bcup THEN
        unpackattrs(f,t);
        proceed:=(t.err=0) & ((e.ctim#t.ctim) OR (e.wtim#t.wtim));
        bio.close(f)
      ELSIF NOT bio.done & (bio.error#err.no_entry) THEN
        bio_error(fname); proceed:=FALSE
      END
    END;
    IF NOT path & isdir THEN
      tty.print("%s/\n",fname);
      proceed:=FALSE
    ELSIF proceed THEN
      IF NOT path THEN
        bio.splitpathname(fname,e.name); str.copy(fname,e.name)
      END;
      proceed:=NOT qry OR all OR env.ipr();
      IF NOT proceed THEN
        IF isdir THEN
          tty.print("%s/     ?",fname)
        ELSE
          tty.print("%s %*s?",fname,maxlen-str.len(fname),'')
        END;
        proceed:=query();
        IF skip           THEN tty.print("\n"); RETURN
        ELSIF NOT proceed THEN tty.print("\n")
        ELSE                   tty.print("\r")
        END
      END;
      IF path & NOT proceed & isdir THEN
        REPEAT UNTIL NOT nextentry(e) OR (e.kind*bio.is_dir#{});
        seek(back)
      END
    END;

    IF proceed THEN
      createfile(f,e,fname);
      IF f#bio.null THEN
        IF NOT env.ipr() THEN
          str.print(msg,"");
          tty.print("%-*s",maxlen,fname)
        ELSE
          str.print(msg,"%-*s",maxlen,fname)
        END;
        bio.seek(ARCH,e.apos,0);
        IF NOT bio.done THEN bio_fatal(ARCHname) END;
        lzw.unpack(ARCH,f,len);
        IF NOT lzw.done THEN
          error(lzw.error,fname); bio.purge(f)
        ELSIF len#e.eof THEN
          tty.print('\nbad archive structure "%s"\n',fname); bio.purge(f)
        ELSE
          setattrs(f,e,fname);
          bio.close(f);
          IF NOT bio.done THEN bio_error(fname)
          ELSE
            size2str(len,bump); str.append(msg,"%s",bump);
            IF NOT env.ipr() THEN tty.print("%s\n",msg)
            ELSE                  env.put_str(env.info,msg,TRUE)
            END
          END
        END
      END
    END
  END
END unpacktree;

PROCEDURE flags;

  CONST max=MAX(INTEGER);

  VAR s: STRING;

  PROCEDURE date(VAR t: INTEGER; night: BOOLEAN);
  BEGIN
    tim.scan_date(t,s,night);

    IF t>=0 THEN RETURN END;
    std.print("illegal time specification\n"); HALT(err.bad_parm)
  END date;

BEGIN
  all :=FALSE;
  skip:=FALSE;
  xmem:=NOT arg.flag('-','m');
  qry :=NOT arg.flag('-','q');
  path:=    arg.flag('-','T');
  bcup:=    arg.flag('-','b');
  relp:=    arg.flag('-','r');
  IF arg.string("after" ,s) THEN date(after,FALSE) ELSE after := 0  END;
  IF arg.string("before",s) THEN date(before,TRUE) ELSE before:=max END;
  IF before<after THEN
    std.print("degenerated time range"); HALT(err.bad_parm)
  END
END flags;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "pkr"  packing & archiving utility program (c) KRONOS\n'
      'usage:\n'
      '   pkr [-hmlrTq] archive_name {[@]files_tree_spec} .... [times]\n\n');
  std.print(
      'flags:\n'
      '   -h   print this text\n'
      '   -l   list  existing archive\n'
      '   -m   small memory model compression\n'
      '   -T   restore with tree structure\n'
      '   -r   relational paths storing\n'
      '   -q   not   query\n');
  std.print(
      'times:\n'
      '    [after=time] [before=time]\n'
      '    time = [dd/mm/yy,][hh:mm[.s]]\n'
      '         | [yy#dd#mm,][hh:mm[.s]]\n');
  std.print(
      'files:\n'
      '    @file  notes that files_tree_spec will be read\n'
      '           from redirection file (one speciment per line)\n'
      '    if your file_tree_spec starts "@" repeat it twice "@@"\n\n'
      '                                   Leopold, Jun 21 91\n')
END pusage;

VAR  i: INTEGER;
   cmd: STR256;
   sub: bio.FILE;
  snam: STR256;

BEGIN
  IF arg.flag('-','h') THEN pusage; HALT END;
  tty.set_cursor(0);
  flags;
  IF HIGH(arg.words)<0 THEN pusage; HALT END;
  str.copy(ARCHname,arg.words[0]);
  IF HIGH(arg.words)=0 THEN
    openarchive('r');
    IF arg.flag('-','l') THEN listarchive
    ELSE                       unpacktree
    END
  ELSE
    filco:=0;
    createarchive;
    FOR i:=1 TO HIGH(arg.words) DO
      IF (HIGH(arg.words[i])>0) & (arg.words[i][0]='@') & (arg.words[i][1]='@')
      THEN
        str.sub_str(cmd,arg.words[i],1,str.len(arg.words[i])-1);
        packtree(cmd)
      ELSIF (HIGH(arg.words[i])>0) & (arg.words[i][0]='@') THEN
        str.sub_str(snam,arg.words[i],1,str.len(arg.words[i])-1);
        bio.open(sub,snam,'r');
        IF NOT bio.done THEN bio_fatal(snam) END;
        WHILE bio.pos(sub)<bio.eof(sub) DO
          bio.getstr(sub,cmd,0);
          IF NOT bio.done THEN bio_fatal(snam) END;
          IF cmd#"" THEN packtree(cmd) END
        END;
        bio.close(sub)
      ELSE
        packtree(arg.words[i])
      END
    END;
    verifyandclose
  END
END pkr.
