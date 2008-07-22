IMPLEMENTATION MODULE osFiles[1]; (* Igo 16-Feb-92. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  fs : osFileSystem;

TYPE FILE = fs._FILE;

VAR MAGIC: INTEGER;

PROCEDURE getattr(f: FILE; atr: INTEGER; VAR val: SYSTEM.WORD);
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN END;
  f^.fsid^.getattr(f,atr,val)
END getattr;

PROCEDURE setattr(f: FILE; atr: INTEGER;     val: SYSTEM.WORD);
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN END;
  f^.fsid^.setattr(f,atr,val)
END setattr;

PROCEDURE isdir(f: FILE): BOOLEAN;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN FALSE END;
  RETURN f^.fsid^.isdir(f)
END isdir;

PROCEDURE state(f: FILE): BITSET;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN {} END;
  RETURN f^.fsid^.state(f)
END state;

PROCEDURE usrlock  (f: FILE; t: INTEGER): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.usrlock(f,t)
END usrlock;

PROCEDURE usrunlock(f: FILE);
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN END;
  f^.fsid^.usrunlock(f)
END usrunlock;

PROCEDURE root(): FILE;
BEGIN
  RETURN fs.getroot()
END root;

PROCEDURE mount  (dir: FILE; VAL name: ARRAY OF CHAR;   dev: FILE;
                             VAL fsname: ARRAY OF CHAR;
                             VAL info: ARRAY OF CHAR;
                             VAR lab: ARRAY OF CHAR;    ro: BOOLEAN): INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN dir^.fsid^.mount(dir,name,dev,fsname,info,lab,ro)
END mount;

PROCEDURE unmount(dir: FILE; name: ARRAY OF CHAR; flush: INTEGER): INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN dir^.fsid^.unmount(dir,name,flush)
END unmount;

(*
PROCEDURE copy_fname(VAR name: ARRAY OF CHAR; VAL fname: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER; ch: CHAR;  TYPE b=BITSET;
BEGIN
  IF (HIGH(name)<31) OR (HIGH(fname)<0) THEN RETURN err.bad_name END;

  IF fname=".."    THEN name:='..'; RETURN err.ok END;
  i:=0;
  WHILE (i<=HIGH(fname)) & (fname[i]#0c) & (i<32) DO
    ch:=fname[i];
    IF b(ch=' ')+b(ch='/')+b(ch=':')+b(ch='\')#{} THEN
      RETURN err.bad_name
    END;
    name[i]:=ch;
    i:=i+1
  END;
  IF (i=0) OR (i>31) THEN RETURN err.bad_name END;
  name[i]:=0c;
  RETURN err.ok
END copy_fname;
*)

PROCEDURE hash_name(VAR  name: ARRAY OF CHAR;
                    VAL fname: ARRAY OF CHAR): INTEGER; (*$<*) (*$T-*)
  VAR i,h: INTEGER; ch: CHAR;  TYPE b=BITSET;
BEGIN
  IF HIGH(fname)<0 THEN RETURN -1 END;
  IF fname=".."    THEN name:='..'; RETURN 0 END;
  i:=0; h:=0;
  WHILE (i<=HIGH(fname)) & (fname[i]#0c) & (i<32) DO
    ch:=fname[i];
    IF b(ch=' ')+b(ch='/')+b(ch=':')+b(ch='\')#{} THEN
      RETURN -1
    END;
    name[i]:=ch;
    h:=h+ORD(ch)-40b+i; i:=i+1
  END;
  IF (i=0) OR (i>31) THEN RETURN -1 END;
  name[i]:=0c;
  RETURN (ABS(h) MOD 64 + ABS(h) DIV 32) MOD 64;
END hash_name;                                         (*$>*)

PROCEDURE copy_fname(VAR name: ARRAY OF CHAR; VAL str: ARRAY OF CHAR): INTEGER;
BEGIN
  IF HIGH(name)<31          THEN RETURN err.bad_name END;
  IF hash_name(name,str)<0  THEN RETURN err.bad_name END;
  RETURN err.ok
END copy_fname;

PROCEDURE open  (dir: FILE;          VAR f: FILE;
                VAL name: ARRAY OF CHAR; state: BITSET): INTEGER;
BEGIN
  RETURN fs.openfile(dir,f,name,state)
END open;

PROCEDURE create(dir: FILE;          VAR f: FILE;
                size: INTEGER;       state: BITSET): INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN dir^.fsid^.create(dir,f,size,state)
END create;

PROCEDURE open_dev(VAR f: FILE; device: ARRAY OF CHAR; state: BITSET): INTEGER;
(* state <= nodelay+nocash+wait_wr *)
BEGIN
  RETURN fs.opendev(f,device,state)
END open_dev;

PROCEDURE close (f: FILE): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.close(f)
END close;

PROCEDURE reopen(f: FILE): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.reopen(f)
END reopen;

PROCEDURE flush (f: FILE): INTEGER;
(* close (write_inode) + open for dir & file
   erase cash for blocked
*)
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.flush(f)
END flush;

PROCEDURE make_dir(VAR d: FILE; where: FILE): INTEGER;
BEGIN
  IF (where=NIL) OR (where^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN where^.fsid^.makedir(d,where)
END make_dir;

PROCEDURE move_dir(from: FILE; VAL fname: ARRAY OF CHAR;
                     to: FILE; VAL tname: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;
BEGIN
  IF (from=NIL) OR (from^.fmagic#MAGIC)  THEN RETURN err.bad_desc END;
  IF (to  =NIL) OR (to  ^.fmagic#MAGIC)  THEN RETURN err.bad_desc END;
  IF from^.fsid#to^.fsid THEN RETURN err.xcross   END;
  RETURN from^.fsid^.movedir(from,fname,to,tname,hid)
END move_dir;

PROCEDURE read_dir(d: FILE; VAR dir: SYSTEM.ADDRESS; VAR sz: INTEGER): INTEGER;
(* read directory:
   allocate memory for directory & read it
   dir point to array of dnode
   memory allocated by os.ALLOCATE from task^.area
*)
BEGIN
  IF (d=NIL) OR (d^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN d^.fsid^.readdir(d,dir,sz)
END read_dir;

PROCEDURE read (f: FILE; fpos: INTEGER;     buf: SYSTEM.ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.read(f,fpos,buf,bpos,len)
END read;

PROCEDURE write(f: FILE; fpos: INTEGER;     buf: SYSTEM.ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.write(f,fpos,buf,bpos,len)
END write;

PROCEDURE doio(f: FILE; VAR r: req.REQUEST): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.doio(f,r)
END doio;

PROCEDURE link  (dir,file: FILE; VAL name: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;
BEGIN
  IF (dir =NIL) OR (dir ^.fmagic#MAGIC)   THEN RETURN err.bad_desc END;
  IF (file=NIL) OR (file^.fmagic#MAGIC)   THEN RETURN err.bad_desc END;
  IF dir^.fsid#file^.fsid THEN RETURN err.xcross   END;
  RETURN dir^.fsid^.link(dir,file,name,hid)
END link;

PROCEDURE unlink(dir: FILE; VAL name: ARRAY OF CHAR): INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN dir^.fsid^.unlink(dir,name)
END unlink;

PROCEDURE du(dir: FILE; VAR free,used: INTEGER): INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN dir^.fsid^.du(dir,free,used)
END du;

PROCEDURE cut(f: FILE; size: INTEGER): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.cut(f,size)
END cut;

PROCEDURE extend(f: FILE; size: INTEGER): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN f^.fsid^.extend(f,size)
END extend;

PROCEDURE make_fs(disk: FILE;
                  VAL fsname: ARRAY OF CHAR;
                  VAL label : ARRAY OF CHAR;
                  VAL bads  : ARRAY OF INTEGER): INTEGER;
BEGIN
  RETURN fs.makefs(disk,fsname,label,bads)
END make_fs;

PROCEDURE make_node(d: FILE; VAR f: FILE; VAL ref: ARRAY OF CHAR): INTEGER;
BEGIN
  IF (d=NIL) OR (d^.fmagic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN d^.fsid^.makenode(d,f,ref)
END make_node;

PROCEDURE getfhandle(f: FILE; VAR fh: FHANDLE);
BEGIN
  IF (f=NIL)OR(f^.fmagic#MAGIC) THEN RETURN END;
  f^.fsid^.getfh(f,fh)
END getfhandle;

PROCEDURE openfh(VAR f: FILE; VAR fh: FHANDLE): INTEGER;
BEGIN
  RETURN fs.openfhandle(f,fh)
END openfh;

PROCEDURE statistic(VAR chsize,dkwrite,dkread,chread: INTEGER);
BEGIN
  chsize :=fs.chsize;
  dkwrite:=fs.dkwrite;
  dkread :=fs.dkread;
  chread :=fs.chread
END statistic;

---------------------------- DEVICES ---------------------------
                            ---------

PROCEDURE define_driver(VAL name,host: ARRAY OF CHAR; drn: INTEGER;
                                 mode: BITSET;       doio: DOIO): INTEGER;
BEGIN
  RETURN fs.define_driver(name,host,drn,mode,doio)
END define_driver;

PROCEDURE remove_driver(VAL name: ARRAY OF CHAR): INTEGER;
BEGIN
  RETURN fs.remove_driver(name)
END remove_driver;

BEGIN
  MAGIC:=fs.FMAGIC;
  null :=NIL
END osFiles.
