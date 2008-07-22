IMPLEMENTATION MODULE nfs[1]; (* Igo 07-Jan-92. (c) KRONOS *)

(*$U+*)
(*$N+*)

IMPORT  SYSTEM;
IMPORT  fs : osFiles;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  str: Strings;
IMPORT  rpc;

--IMPORT  tty: Terminal;

WITH STORAGE (NEW    : nos.allocate;
              DISPOSE: nos.deallocate;
              RESIZE : nos.reallocate);

TYPE WORD    = SYSTEM.WORD;

CONST NFSPORT = 2049; NFSPROG = 10;
      MNTPORT = 2048; MNTPROG = 11;

TYPE
     NCTX = POINTER TO nctx;
     nctx = RECORD
              port: rpc.PORT;
            END;

VAR     nfscontext: nctx;

PROCEDURE check_p_stack(n: INTEGER);
CODE cod.copt cod.alloc cod.drop cod.decs END check_p_stack;

PROCEDURE enter;
BEGIN check_p_stack(200); os.lock END enter;

PROCEDURE exit;
BEGIN
  os.unlock
END exit;

PROCEDURE getfh(f: fs.FILE; VAR fh: fhandle);
  VAR fh0: POINTER TO fs.FHANDLE;
BEGIN
  fh0:=SYSTEM.ADR(fh);
  fs.getfhandle(f,fh0^)
END getfh;

PROCEDURE open(VAR f: fs.FILE; VAL fh: fhandle): INTEGER;
  VAR res: INTEGER;
      fh0: POINTER TO fs.FHANDLE;
BEGIN
  fh0:=SYSTEM.ADR(fh);
  enter;
    res:=fs.openfh(f,fh0^);
  exit;
  IF    res=err.ok        THEN RETURN NFS_OK
  ELSIF res=err.no_memory THEN RETURN NFSERR_IO
  ELSIF res=err.no_entry  THEN RETURN NFSERR_STALE
  ELSE                         RETURN NFSERR_IO
  END
END open;

PROCEDURE close(VAR f: fs.FILE);
  VAR res: INTEGER;
BEGIN
  enter;
    res:=fs.close(f);
  exit
END close;

PROCEDURE fserror(e: INTEGER): INTEGER;
  VAR ne: INTEGER;
BEGIN
  CASE e OF
  |err.ok      : ne:=NFS_OK
  |err.no_entry: ne:=NFSERR_NOENT
  |err.is_dir  : ne:=NFSERR_ISDIR
  |err.not_dir : ne:=NFSERR_NOTDIR
  ELSE
    ne:=NFSERR_IO
  END;
  RETURN ne
END fserror;

PROCEDURE fcreate(VAR dfh: fhandle; VAL name: ARRAY OF CHAR;
                  VAR rf: fs.FILE; VAR a: sattr; d: BOOLEAN): INTEGER;
  VAR dir: fs.FILE;
      eof: INTEGER;
      res: INTEGER;
BEGIN
  res:=open(dir,dfh);
  IF res#NFS_OK THEN RETURN res END;
  eof:=0;
  IF a.size>0 THEN eof:=a.size END;
  enter;
    IF d THEN
      res:=fs.make_dir(rf,dir)
    ELSE
      res:=fs.create(dir,rf,eof,{});
    END;
    IF res#err.ok THEN exit; close(dir); RETURN fserror(res) END;
    res:=fs.link(dir,rf,name,FALSE);
    IF res#err.ok THEN
      eof:=fs.close(rf); exit; close(dir);
      RETURN fserror(res)
    END;
  exit;
  RETURN NFS_OK
END fcreate;

PROCEDURE funlink(VAR d: fhandle; VAL name: ARRAY OF CHAR): INTEGER;
  VAR res: INTEGER;
      dir: fs.FILE;
BEGIN
  enter;
    res:=open(dir,d);
    IF res#NFS_OK THEN exit; RETURN res END;
    res:=fserror(fs.unlink(dir,name));
    close(dir);
  exit;
  RETURN res
END funlink;

PROCEDURE fillna(VAR a: rpc.rpcadr; host,proc,tout: INTEGER);
BEGIN
  a.iadr:=host;
  a.port:=NFSPORT;
  a.prog:=NFSPROG;
  a.proc:=proc;
  a.vers:=2;
  a.tout:=tout;
  a.try :=10;
END fillna;

PROCEDURE putfhandle(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR fh: fhandle);
BEGIN
  ASSERT(p+7<=HIGH(buf));
  nos.move(SYSTEM.ADR(buf),p*4,SYSTEM.ADR(fh),0,BYTES(fh));
  INC(p,8)
END putfhandle;

PROCEDURE getfhandle(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR fh: fhandle): BOOLEAN;
BEGIN
  IF p+7>HIGH(buf) THEN RETURN FALSE END;
  nos.move(SYSTEM.ADR(fh),0,SYSTEM.ADR(buf),p*4,BYTES(fh));
  INC(p,8);
  RETURN TRUE
END getfhandle;

PROCEDURE putfixbarray(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR a: ARRAY OF CHAR);
  VAR wsz: INTEGER;
BEGIN
  wsz:=(BYTES(a)+3) DIV 4;
  ASSERT(p+wsz-1<=HIGH(buf));
  nos.move(SYSTEM.ADR(buf),p*4,SYSTEM.ADR(a),0,wsz*4);
  INC(p,wsz)
END putfixbarray;

PROCEDURE getfixbarray(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR a: ARRAY OF CHAR): BOOLEAN;
  VAR wsz: INTEGER;
BEGIN
  wsz:=(BYTES(a)+3) DIV 4;
  IF p+wsz-1>HIGH(buf) THEN RETURN FALSE END;
  nos.move(SYSTEM.ADR(a),0,SYSTEM.ADR(buf),p*4,wsz*4);
  INC(p,wsz);
  RETURN TRUE
END getfixbarray;

PROCEDURE putfixiarray(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR a: ARRAY OF WORD);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(a) DO rpc.writenum(buf,p,a[i]) END
END putfixiarray;

PROCEDURE getfixiarray(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR a: ARRAY OF WORD): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(a) DO
    IF NOT rpc.readnum(buf,p,a[i]) THEN RETURN FALSE END
  END;
  RETURN TRUE
END getfixiarray;

PROCEDURE getstring(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR l,wsz: INTEGER;
BEGIN
  IF NOT rpc.readnum(buf,p,l) THEN RETURN FALSE END;
  IF l<0 THEN RETURN FALSE END;
  wsz:=(l+3) DIV 4;
  IF p+wsz-1>HIGH(buf) THEN RETURN FALSE END;
  IF l=0 THEN s[0]:=0c; RETURN TRUE END;
  IF l-1>HIGH(s) THEN l:=HIGH(s)+1 END;
  nos.move(SYSTEM.ADR(s),0,SYSTEM.ADR(buf),p*4,l);
  IF l<=HIGH(s) THEN s[l]:=0c END;
  INC(p,wsz);
  RETURN TRUE
END getstring;

PROCEDURE getstring0(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR s: ARRAY OF CHAR; VAR len: INTEGER): BOOLEAN;
  VAR l,wsz: INTEGER;
BEGIN
  IF NOT rpc.readnum(buf,p,l) THEN RETURN FALSE END;
  len:=l;
  IF l<0 THEN RETURN FALSE END;
  wsz:=(l+3) DIV 4;
  IF p+wsz-1>HIGH(buf) THEN RETURN FALSE END;
  IF l=0 THEN s[0]:=0c; RETURN TRUE END;
  IF l-1>HIGH(s) THEN l:=HIGH(s)+1 END;
  nos.move(SYSTEM.ADR(s),0,SYSTEM.ADR(buf),p*4,l);
  IF l<=HIGH(s) THEN s[l]:=0c END;
  INC(p,wsz);
  RETURN TRUE
END getstring0;

PROCEDURE putstring(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR s: ARRAY OF CHAR);
  VAR wsz: INTEGER;
BEGIN
  wsz:=(BYTES(s)+3) DIV 4;
  rpc.writenum(buf,p,BYTES(s));
  ASSERT(p+wsz-1<=HIGH(buf));
  nos.move(SYSTEM.ADR(buf),p*4,SYSTEM.ADR(s),0,BYTES(s));
  INC(p,wsz)
END putstring;

PROCEDURE readnfsdata(VAR buf: ARRAY OF WORD; VAR p: INTEGER; a: ADDRESS; bpos,len: INTEGER): BOOLEAN;
  VAR l,wsz: INTEGER;
BEGIN
  ASSERT(l>=0);
  IF (NOT rpc.readnum(buf,p,l))OR(l#len) THEN RETURN FALSE END;
  wsz:=(l+3) DIV 4;
  IF l=0 THEN RETURN TRUE END;
  IF p+wsz-1>HIGH(buf) THEN RETURN FALSE END;
  nos.move(a,bpos,SYSTEM.ADR(buf),p*4,l);
  INC(p,wsz);
  RETURN TRUE
END readnfsdata;

PROCEDURE u(VAR rp: rpc.buffer);
BEGIN
  nos.deallocate(rp.adr,rp.sz)
END u;

(* 00 *)
PROCEDURE null(): INTEGER;
BEGIN
END null;

PROCEDURE getfattr(f: fs.FILE; VAR a: fattr);

  CONST
    dir     = {1};
    long    = {2};
    escape  = {3};

  VAR m: BITSET;

BEGIN
  WITH a DO
    fs.getattr(f,fs.a_eof,size);
    fs.getattr(f,fs.a_wtime,atime.seconds);
    atime.useconds:=0;
    mtime:=atime;
    fs.getattr(f,fs.a_ctime,ctime.seconds);
    ctime.useconds:=0;

    fs.getattr(f,fs.a_links,nlink );
    fs.getattr(f,fs.a_mode ,m     );
    fs.getattr(f,fs.a_inode,fileid);
    rdev:=0;
    fsid:=0;
    blocks:=(size+4095) DIV 4096;
    blocksize:=4096;
    IF    m*dir#{}    THEN type:=NFDIR; mode:=40000b
    ELSIF m*escape#{} THEN type:=NFBLK; mode:=10000b
    ELSE                   type:=NFREG; mode:=100000b
    END;

    fs.getattr(f,fs.a_pro,m);

    gid:=INTEGER((m>>24)*{0..6});
    uid:=INTEGER((m>>16)*{0..7});

    mode:=INTEGER(BITSET(mode)+
                  ((m*{0..2} )<<6)+
                  ((m*{4..6} )>>1)+
                  ((m*{8..10})>>8));
  END
END getfattr;

PROCEDURE __getattr(VAR fh: fhandle; VAR a: attrstat);
  VAR f: fs.FILE;
BEGIN
  a.state:=open(f,fh);
  IF a.state#NFS_OK THEN RETURN END;
  getfattr(f,a.attributes);
  close(f)
END __getattr;

PROCEDURE replyattrstat(nc: NCTX; VAR a: attrstat);
  VAR buf: DYNARR OF WORD;
      pos: INTEGER;
      pa : POINTER TO ARRAY [0..16] OF INTEGER;
      rp : rpc.buffer;
BEGIN
  IF a.state=NFS_OK THEN
    NEW(buf,18);
  ELSE
    NEW(buf,1)
  END;
  IF buf^.ADR=NIL THEN RETURN END;
  pos:=0;
  rpc.writenum(buf,pos,a.state);
  IF a.state=NFS_OK THEN
    pa:=SYSTEM.ADR(a.attributes);
    putfixiarray(buf,pos,pa^)
  END;
  rp.adr:=buf^.ADR;
  rp.sz :=buf^.HIGH+1;
  ASSERT(pos=rp.sz);
  rpc.reply(nc^.port,rp)
END replyattrstat;

PROCEDURE _getattr(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR fh : fhandle;
      pos: INTEGER;
      a  : attrstat;
BEGIN
  pos:=0;
  IF NOT getfhandle(parm,pos,fh) THEN rpc.garbageargs(nc^.port); RETURN END;
  __getattr(fh,a);
  replyattrstat(nc,a)
END _getattr;

PROCEDURE readfattr(VAR buf: ARRAY OF WORD; VAR p: INTEGER; VAR a: fattr): BOOLEAN;
  VAR i: INTEGER;
      b: POINTER TO ARRAY [0..16] OF INTEGER;
BEGIN
  ASSERT(SIZE(a)=17);
  b:=SYSTEM.ADR(a);
  FOR i:=0 TO HIGH(b^) DO
    IF NOT rpc.readnum(buf,p,b^[i]) THEN RETURN FALSE END
  END;
  RETURN TRUE
END readfattr;

PROCEDURE getattrstat(VAR attr: attrstat; VAR rp: rpc.buffer): INTEGER;
  VAR buf: DYNARR OF WORD;
      pos: INTEGER;
BEGIN
  buf^.ADR :=rp.adr;
  buf^.HIGH:=rp.sz-1;
  pos:=0;
  IF NOT rpc.readnum(buf,pos,attr.state) THEN RETURN err.inconsistency END;
  IF attr.state#NFS_OK THEN RETURN err.ok END;
  IF NOT readfattr(buf,pos,attr.attributes) THEN RETURN err.inconsistency END;
  RETURN err.ok
END getattrstat;

(* 01 *)
PROCEDURE getattr(VAR ia: nfa; VAR f: fhandle;   VAR attr: attrstat): INTEGER;
  VAR adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,01,ia.tout);
  b.buf :=SYSTEM.ADR(f);
  b.pos :=0;
  b.len :=8*4;
  b.next:=NIL;
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getattrstat(attr,rp);
  u(rp);
  RETURN res
END getattr;

PROCEDURE lookuperror(VAR ne: INTEGER; xe: INTEGER);
BEGIN
  CASE xe OF
  |err.ok      : ne:=NFS_OK
  |err.no_entry: ne:=NFSERR_NOENT
  |err.not_dir : ne:=NFSERR_NOTDIR
  |err.is_dir  : ne:=NFSERR_ISDIR
  ELSE
    ne:=NFSERR_IO
  END
END lookuperror;

PROCEDURE __setattr(VAR fh: fhandle; VAL s: sattr; VAR a: attrstat);
  VAR f: fs.FILE;
      i: INTEGER;
      p,p0: BITSET;
BEGIN
  a.state:=open(f,fh);
  IF a.state#NFS_OK THEN RETURN END;
  IF s.size>=0 THEN
    IF fs.isdir(f) THEN
      a.state:=NFSERR_ISDIR; RETURN
    ELSE
      IF s.size>0 THEN fs.setattr(f,fs.a_eof,s.size) END;
      IF s.size=0 THEN
        fs.getattr(f,fs.a_eof,i);
        lookuperror(a.state,fs.cut(f,i))
      END
    END
  END;
  fs.getattr(f,fs.a_pro,p); p0:=p;
  IF s.uid#-1  THEN p:=BITSET((s.uid MOD 256)<<16)+p END;
  IF s.gid#-1  THEN p:=BITSET((s.gid MOD 128)<<24)+p END;
  IF s.mode#-1 THEN
     p:=BITSET((s.mode MOD 8) <<8 )+
        BITSET((s.mode DIV 8  MOD 8)<<4)+
        BITSET((s.mode DIV 64 MOD 8)   )+p
  END;
  IF p#p0 THEN fs.setattr(f,fs.a_pro,p) END;
  getfattr(f,a.attributes);
  close(f)
END __setattr;

PROCEDURE _setattr(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR fh : fhandle;
      pos: INTEGER;
      s  : sattr;
      pa : POINTER TO ARRAY [0..7] OF INTEGER;
      a  : attrstat;
BEGIN
  pos:=0;
  pa :=SYSTEM.ADR(s);
  IF (NOT getfhandle  (parm,pos,fh )) OR
     (NOT getfixiarray(parm,pos,pa^)) OR
     (pos#SIZE(parm))
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  __setattr(fh,s,a);
  replyattrstat(nc,a)
END _setattr;

PROCEDURE putsetattrargs(VAR buf: ARRAY OF WORD; VAR a: sattrargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
      pa : POINTER TO ARRAY [0..7] OF INTEGER;
BEGIN
  pos:=0;
  putfhandle  (buf,pos,a.file);
  pa:=SYSTEM.ADR(a.attributes);
  putfixiarray(buf,pos,pa^);
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putsetattrargs;

(* 02 *)
PROCEDURE setattr(VAR ia: nfa; VAR a: sattrargs; VAR attr: attrstat): INTEGER;
  VAR buf: ARRAY [0..15] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,02,ia.tout);
  putsetattrargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getattrstat(attr,rp);
  u(rp);
  RETURN res
END setattr;

(* 03 *)
PROCEDURE root; (* empty *)
BEGIN
  ASSERT(FALSE)
END root;

PROCEDURE putdiropargs(VAR buf: ARRAY OF WORD; VAR a: diropargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
BEGIN
  pos:=0;
  putfhandle(buf,pos,a.dir);
  putstring (buf,pos,a.name);
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putdiropargs;

PROCEDURE getdiropres(VAR r: diropres; VAR rp: rpc.buffer): INTEGER;
  VAR buf: DYNARR OF WORD;
      pos: INTEGER;
BEGIN
  buf^.ADR :=rp.adr;
  buf^.HIGH:=rp.sz-1;
  pos:=0;
  IF NOT rpc.readnum(buf,pos,r.state) THEN RETURN err.inconsistency END;
  IF r.state#NFS_OK THEN RETURN err.ok END;
  IF NOT getfhandle(buf,pos,r.file)       THEN RETURN err.inconsistency END;
  IF NOT readfattr (buf,pos,r.attributes) THEN RETURN err.inconsistency END;
  IF pos#rp.sz THEN RETURN err.inconsistency END;
  RETURN err.ok
END getdiropres;

(* 04 *)
PROCEDURE lookup(VAR ia: nfa; VAR a: diropargs; VAR r: diropres): INTEGER;
  VAR buf: ARRAY [0..16] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,04,ia.tout);
  putdiropargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getdiropres(r,rp);
  u(rp);
  RETURN res
END lookup;

PROCEDURE __lookup(VAR dir: fhandle; VAR name: ARRAY OF CHAR; VAR r: diropres);
  VAR fdir: fs.FILE;
      file: fs.FILE;
BEGIN
  r.state:=open(fdir,dir);
  IF r.state#NFS_OK THEN RETURN END;
  lookuperror(r.state,fs.open(fdir,file,name,fs.wait_wr));
  close(fdir);
  IF r.state#NFS_OK THEN RETURN END;
  getfh   (file,r.file);
  getfattr(file,r.attributes);
  close(file)
END __lookup;

PROCEDURE replydiropres(nc: NCTX; VAR r: diropres);
  VAR pos: INTEGER;
      rp : rpc.buffer;
      buf: DYNARR OF WORD;
      pa : POINTER TO ARRAY [0..16] OF INTEGER;
BEGIN
  IF r.state=NFS_OK THEN
    NEW(buf,17+1+8);
  ELSE
    NEW(buf,1)
  END;
  IF buf^.ADR=NIL THEN RETURN END;
  pos:=0;
  rpc.writenum(buf,pos,r.state);
  IF r.state=NFS_OK THEN
    putfhandle(buf,pos,r.file);
    pa:=SYSTEM.ADR(r.attributes);
    putfixiarray(buf,pos,pa^)
  END;
  rp.adr:=buf^.ADR;
  rp.sz :=buf^.HIGH+1;
  ASSERT(pos=rp.sz);
  rpc.reply(nc^.port,rp)
END replydiropres;

PROCEDURE _lookup(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR pos: INTEGER;
      dir: fhandle;
      fn : ARRAY [0..31] OF CHAR;
      r  : diropres;
      i  : INTEGER;
BEGIN
  pos:=0;
  IF NOT getfhandle(parm,pos,dir)  THEN rpc.garbageargs(nc^.port); RETURN END;
  IF NOT getstring0(parm,pos,fn,i) THEN rpc.garbageargs(nc^.port); RETURN END;
  IF i>32 THEN
    r.state:=NFSERR_NAMETOOLONG
  ELSE
    __lookup(dir,fn,r);
  END;
  replydiropres(nc,r)
END _lookup;

(* 05 *)
PROCEDURE readlink(VAR ia: nfa; VAR f: fhandle; VAR r: readlinkres): INTEGER;
BEGIN
  RETURN err.inv_op
END readlink;

PROCEDURE putreadargs(VAR buf: ARRAY OF WORD; VAR a: readargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
BEGIN
  pos:=0;
  putfhandle(buf,pos,a.file);
  rpc.writenum(buf,pos,a.offset);
  rpc.writenum(buf,pos,a.count );
  rpc.writenum(buf,pos,a.totalcount);
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putreadargs;

PROCEDURE getreadres(VAR r: readres; VAR rp: rpc.buffer; len: INTEGER): INTEGER;
  VAR buf: DYNARR OF WORD;
      pos: INTEGER;
BEGIN
  buf^.ADR :=rp.adr;
  buf^.HIGH:=rp.sz-1;
  pos:=0;
  IF NOT rpc.readnum(buf,pos,r.state) THEN RETURN err.inconsistency END;
  IF r.state#NFS_OK THEN RETURN err.ok END;
  IF NOT readfattr  (buf,pos,r.attributes) THEN RETURN err.inconsistency END;
  IF NOT readnfsdata(buf,pos,r.buf,r.bpos,len) THEN RETURN err.inconsistency END;
  IF pos#rp.sz THEN RETURN err.inconsistency END;
  RETURN err.ok
END getreadres;

(* 06 *)
PROCEDURE read(VAR ia: nfa; VAR a: readargs; VAR r: readres): INTEGER;
  VAR buf: ARRAY [0..10] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF (a.count<0) OR (a.count>(1024*8)) THEN RETURN err.bad_parm END;
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,06,ia.tout);
  putreadargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getreadres(r,rp,a.count);
  u(rp);
  RETURN res
END read;

PROCEDURE readerror(VAR ne: INTEGER; xe: INTEGER);
BEGIN
  CASE xe OF
  |err.ok      : ne:=NFS_OK
  |err.no_entry: ne:=NFSERR_NOENT
  |err.not_dir : ne:=NFSERR_NOTDIR
  ELSE
    ne:=NFSERR_IO
  END
END readerror;

PROCEDURE __read(VAR fh: fhandle; ofs,len: INTEGER; VAR r: readres);
  VAR f  : fs.FILE;
      res: INTEGER;
BEGIN
  r.state:=open(f,fh);
  IF r.state#NFS_OK THEN RETURN END;
  getfattr(f,r.attributes);
  res:=fs.read(f,ofs,r.buf,0,len);
  close(f);
  readerror(r.state,res)
END __read;

PROCEDURE _read(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR pos   : INTEGER;
      buf   : DYNARR OF WORD;
      pa    : POINTER TO ARRAY [0..16] OF INTEGER;
      i     : INTEGER;
      offset: INTEGER;
      count : INTEGER;
      file  : fhandle;
      r     : readres;
      rp    : rpc.buffer;
BEGIN
  pos:=0;
  IF    (NOT getfhandle (parm,pos,file))
     OR (NOT rpc.readnum(parm,pos,offset))
     OR (NOT rpc.readnum(parm,pos,count ))
     OR (count>(1024*8)) OR (count<0     )
     OR (NOT rpc.readnum(parm,pos,i     ))
     OR (pos#SIZE(parm)                  )
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  NEW(buf,17+1+1+ (count+3) DIV 4);
  IF buf^.ADR=NIL THEN RETURN END;
  r.buf:=buf^.ADR+17+1+1;
  __read(file,offset,count,r);
  IF r.state#NFS_OK THEN
    DISPOSE(buf);
    NEW(buf,1);
    IF buf^.ADR=NIL THEN RETURN END
  END;
  pos:=0;
  rpc.writenum(buf,pos,r.state);
  IF r.state=NFS_OK THEN
    pa:=SYSTEM.ADR(r.attributes);
    putfixiarray(buf,pos,pa^);
    rpc.writenum(buf,pos,count)
  END;
  rp.adr:=buf^.ADR;
  rp.sz :=buf^.HIGH+1;
  rpc.reply(nc^.port,rp)
END _read;

(* 07 *)
PROCEDURE writecash; (* empty *)
BEGIN
  ASSERT(FALSE)
END writecash;

PROCEDURE putwriteargs(VAR buf: ARRAY OF WORD; VAR a: writeargs; VAR b: ARRAY OF nos.buffer);
  VAR pos: INTEGER;
BEGIN
  pos:=0;
  putfhandle  (buf,pos,a.file);
  rpc.writenum(buf,pos,a.beginoffset);
  rpc.writenum(buf,pos,a.offset    );
  rpc.writenum(buf,pos,a.totalcount);
  rpc.writenum(buf,pos,a.bsz       );
  b[0].buf :=SYSTEM.ADR(buf);
  b[0].pos :=0;
  b[0].len :=pos*4;
  b[0].next:=SYSTEM.ADR(b[1]);
  b[1].buf :=a.buf;
  b[1].pos :=a.bpos;
  b[1].len :=(a.bsz+3) DIV 4*4;
  b[1].next:=NIL
END putwriteargs;

(* 08 *)
PROCEDURE write(VAR ia: nfa; VAR a: writeargs; VAR attr: attrstat): INTEGER;
  VAR buf: ARRAY [0..11] OF WORD;
      adr: rpc.rpcadr;
      b  : ARRAY [0..1] OF nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF (a.bsz<0) OR (a.bsz>(1024*8)) THEN RETURN err.bad_parm END;
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,08,ia.tout);
  putwriteargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b[0],b[0].len+b[1].len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getattrstat(attr,rp);
  u(rp);
  RETURN res
END write;

PROCEDURE writeerror(VAR ne: INTEGER; xe: INTEGER);
BEGIN
  CASE xe OF
  |err.ok       : ne:=NFS_OK
  |err.no_entry : ne:=NFSERR_NOENT
  |err.not_dir  : ne:=NFSERR_NOTDIR
  |err.too_large: ne:=NFSERR_FBIG
  |err.no_space : ne:=NFSERR_DQUOT
  ELSE
    ne:=NFSERR_IO
  END
END writeerror;

PROCEDURE __write(VAR fh: fhandle; buf: ADDRESS; ofs,len: INTEGER; VAR r: attrstat);
  VAR f  : fs.FILE;
      res: INTEGER;
BEGIN
  r.state:=open(f,fh);
  IF r.state#NFS_OK THEN RETURN END;
  getfattr(f,r.attributes);
  enter;
    res:=fs.write(f,ofs,buf,0,len);
  exit;
  getfattr(f,r.attributes);
  close(f);
  writeerror(r.state,res)
END __write;

PROCEDURE _write(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR pos   : INTEGER;
      offset: INTEGER;
      count : INTEGER;
      file  : fhandle;
      r     : attrstat;
      data  : ADDRESS;
BEGIN
  pos:=0;
  IF    (NOT getfhandle (parm,pos,file  ))
     OR (NOT rpc.readnum(parm,pos,count ))
     OR (NOT rpc.readnum(parm,pos,offset))
     OR (NOT rpc.readnum(parm,pos,count ))
     OR (NOT rpc.readnum(parm,pos,count ))
     OR (count>(1024*8)) OR (count<0     )
     OR (pos+(count+3) DIV 4#SIZE(parm)  )
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  data:=SYSTEM.ADR(parm)+pos;
  __write(file,data,offset,count,r);
  replyattrstat(nc,r)
END _write;

PROCEDURE putcreateargs(VAR buf: ARRAY OF WORD; VAR a: createargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
      pa : POINTER TO ARRAY [0..7] OF WORD;
BEGIN
  pos:=0;
  putfhandle(buf,pos,a.where.dir);
  putstring (buf,pos,a.where.name);
  pa:=SYSTEM.ADR(a.attributes);
  putfixiarray(buf,pos,pa^);
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putcreateargs;

PROCEDURE create0(VAR ia: nfa; VAR a: createargs; VAR r: diropres; d: BOOLEAN): INTEGER;
  VAR buf: ARRAY [0..24] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  IF d THEN
    fillna(adr,ia.iadr,14,ia.tout)
  ELSE
    fillna(adr,ia.iadr,09,ia.tout)
  END;
  putcreateargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getdiropres(r,rp);
  u(rp);
  RETURN res
END create0;

(* 09 *)
PROCEDURE create(VAR ia: nfa; VAR a: createargs ; VAR r: diropres): INTEGER;
BEGIN
  RETURN create0(ia,a,r,FALSE)
END create;

PROCEDURE __create(VAR a: createargs; VAR r: diropres; d: BOOLEAN);
  VAR f  : fs.FILE;
BEGIN
  r.state:=fcreate(a.where.dir,a.where.name,f,a.attributes,d);
  IF r.state#NFS_OK THEN RETURN END;
  getfh   (f,r.file);
  getfattr(f,r.attributes);
  close(f)
END __create;

PROCEDURE _create(nc: NCTX; VAR parm: ARRAY OF WORD; d: BOOLEAN);
  VAR pos   : INTEGER;
      r     : diropres;
      a     : createargs;
      len   : INTEGER;
      pa    : POINTER TO ARRAY [0..7] OF WORD;
BEGIN
  pos:=0;
  pa :=SYSTEM.ADR(a.attributes);
  IF    (NOT getfhandle  (parm,pos,a.where.dir     ))
     OR (NOT getstring0  (parm,pos,a.where.name,len))
     OR (NOT getfixiarray(parm,pos,pa^))
     OR (pos#SIZE(parm))
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  IF len>32 THEN
    r.state:=NFSERR_NAMETOOLONG
  ELSE
    __create(a,r,d)
  END;
  replydiropres(nc,r)
END _create;

PROCEDURE getintres(VAR r: INTEGER; VAR rp: rpc.buffer): INTEGER;
  VAR buf: DYNARR OF WORD;
      pos: INTEGER;
BEGIN
  buf^.ADR :=rp.adr;
  buf^.HIGH:=rp.sz-1;
  pos:=0;
  IF NOT rpc.readnum(buf,pos,r) THEN RETURN err.inconsistency END;
  IF pos#rp.sz                  THEN RETURN err.inconsistency END;
  RETURN err.ok
END getintres;

PROCEDURE remove0(VAR ia: nfa; VAR a: diropargs; VAR r: INTEGER; d: BOOLEAN): INTEGER;
  VAR buf: ARRAY [0..16] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  IF d THEN
    fillna(adr,ia.iadr,15,ia.tout)
  ELSE
    fillna(adr,ia.iadr,10,ia.tout)
  END;
  putdiropargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getintres(r,rp);
  u(rp);
  RETURN res
END remove0;

(* 10 *)
PROCEDURE remove(VAR ia: nfa; VAR a: diropargs; VAR r: INTEGER): INTEGER;
BEGIN
  RETURN remove0(ia,a,r,FALSE)
END remove;

PROCEDURE __remove(VAR a: diropargs; VAR r: INTEGER; d: BOOLEAN);
BEGIN
  r:=funlink(a.dir,a.name)
END __remove;

PROCEDURE replyint(nc: NCTX; VAR r: INTEGER);
  VAR pos: INTEGER;
      rp : rpc.buffer;
      buf: DYNARR OF WORD;
BEGIN
  NEW(buf,1);
  IF buf^.ADR=NIL THEN RETURN END;
  pos:=0;
  rpc.writenum(buf,pos,r);
  rp.adr:=buf^.ADR;
  rp.sz :=buf^.HIGH+1;
  ASSERT(pos=rp.sz);
  rpc.reply(nc^.port,rp)
END replyint;

PROCEDURE _remove(nc: NCTX; VAR parm: ARRAY OF WORD; d: BOOLEAN);
  VAR pos: INTEGER;
      r  : INTEGER;
      a  : diropargs;
      len: INTEGER;
BEGIN
  pos:=0;
  IF    (NOT getfhandle  (parm,pos,a.dir     ))
     OR (NOT getstring0  (parm,pos,a.name,len))
     OR (pos#SIZE(parm))
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  IF len>32 THEN
    r:=NFSERR_NAMETOOLONG
  ELSE
    __remove(a,r,d)
  END;
  replyint(nc,r)
END _remove;

PROCEDURE putrenameargs(VAR buf: ARRAY OF WORD; VAR a: renameargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
BEGIN
  pos:=0;
  putfhandle(buf,pos,a.from.dir);
  putstring (buf,pos,a.from.name);
  putfhandle(buf,pos,a.to  .dir);
  putstring (buf,pos,a.to  .name);
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putrenameargs;

(* 11 *)
PROCEDURE rename(VAR ia: nfa; VAR a: renameargs ; VAR r: INTEGER ): INTEGER;
  VAR buf: ARRAY [0..33] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,11,ia.tout);
  putrenameargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getintres(r,rp);
  u(rp);
  RETURN res
END rename;

PROCEDURE __rename(VAL a: renameargs; VAR r: INTEGER);
  VAR dir0: fs.FILE;
      dir1: fs.FILE;
      file: fs.FILE;
      igno: INTEGER;
      d   : BOOLEAN;
BEGIN
  enter;

    r:=open(dir0,a.from.dir);
    IF r#NFS_OK THEN exit; RETURN END;


    r:=fserror(fs.open(dir0,file,a.from.name,{}));
    IF r#NFS_OK THEN close(dir0); exit; RETURN END;

    r:=open(dir1,a.to.dir);
    IF r#NFS_OK THEN close(dir0); close(file); exit; RETURN END;

    d:=fs.isdir(file);

    IF d THEN
      r:=fserror(fs.move_dir(dir0,a.from.name,dir1,a.to.name,FALSE))
    ELSE
      r:=fserror(fs.link(dir1,file,a.to.name,FALSE))
    END;
    close(file);
    close(dir1);
    IF (NOT d) & (r=NFS_OK) THEN
      igno:=fs.unlink(dir0,a.from.name)
    END;
    close(dir0);
  exit
END __rename;

PROCEDURE _rename(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR pos : INTEGER;
      r   : INTEGER;
      a   : renameargs;
      len0: INTEGER;
      len1: INTEGER;
BEGIN
  pos:=0;
  IF    (NOT getfhandle  (parm,pos,a.from.dir      ))
     OR (NOT getstring0  (parm,pos,a.from.name,len0))
     OR (NOT getfhandle  (parm,pos,a.to  .dir      ))
     OR (NOT getstring0  (parm,pos,a.to  .name,len1))
     OR (pos#SIZE(parm))
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  IF (len0>32) OR (len1>32) THEN
    r:=NFSERR_NAMETOOLONG
  ELSE
    __rename(a,r)
  END;
  replyint(nc,r)
END _rename;

PROCEDURE putlinkargs(VAR buf: ARRAY OF WORD; VAR a: linkargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
BEGIN
  pos:=0;
  putfhandle(buf,pos,a.from);
  putfhandle(buf,pos,a.to.dir);
  putstring (buf,pos,a.to.name);
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putlinkargs;

(* 12 *)
PROCEDURE link  (VAR ia: nfa; VAR a: linkargs; VAR r: INTEGER ): INTEGER;
  VAR buf: ARRAY [0..25] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,12,ia.tout);
  putlinkargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getintres(r,rp);
  u(rp);
  RETURN res
END link;

PROCEDURE __link(VAL a: linkargs; VAR r: INTEGER);
  VAR dir : fs.FILE;
      file: fs.FILE;
BEGIN
  enter;

    r:=open(file,a.from);
    IF r#NFS_OK THEN exit; RETURN END;

    r:=open(dir,a.to.dir);
    IF r#NFS_OK THEN close(file); exit; RETURN END;

    r:=fserror(fs.link(dir,file,a.to.name,FALSE));
    close(dir );
    close(file);
  exit
END __link;

PROCEDURE _link(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR pos: INTEGER;
      r  : INTEGER;
      a  : linkargs;
      len: INTEGER;
BEGIN
  pos:=0;
  IF    (NOT getfhandle  (parm,pos,a.from       ))
     OR (NOT getfhandle  (parm,pos,a.to.dir     ))
     OR (NOT getstring0  (parm,pos,a.to.name,len))
     OR (pos#SIZE(parm))
  THEN
    rpc.garbageargs(nc^.port); RETURN
  END;
  IF len>32 THEN
    r:=NFSERR_NAMETOOLONG
  ELSE
    __link(a,r)
  END;
  replyint(nc,r)
END _link;

(* 13 *)
PROCEDURE symlink(VAR ia: nfa; VAR a: symlinkargs; VAR r: INTEGER): INTEGER;
BEGIN
  RETURN err.inv_op
END symlink;

(* 14 *)
PROCEDURE mkdir  (VAR ia: nfa; VAR a: createargs; VAR r: diropres): INTEGER;
BEGIN
  RETURN create0(ia,a,r,TRUE)
END mkdir;

(* 15 *)
PROCEDURE rmdir  (VAR ia: nfa; VAR a: diropargs; VAR r: INTEGER): INTEGER;
BEGIN
  RETURN remove0(ia,a,r,TRUE)
END rmdir;

PROCEDURE putrdirargs(VAR buf: ARRAY OF WORD; VAR a: readdirargs; VAR b: nos.buffer);
  VAR pos: INTEGER;
BEGIN
  pos:=0;
  putfhandle  (buf,pos,a.dir);
  putfixbarray(buf,pos,a.cookie);
  rpc.writenum(buf,pos,a.count );
  b.buf :=SYSTEM.ADR(buf);
  b.pos :=0;
  b.len :=pos*4;
  b.next:=NIL
END putrdirargs;

PROCEDURE getrdirres(VAR r: readdirres; VAR rp: rpc.buffer): INTEGER;
  VAR pos: INTEGER;
      wp : DYNARR OF WORD;

  PROCEDURE getentry(VAR buf: ARRAY OF WORD; VAR pos: INTEGER; VAR e: entry): BOOLEAN;
  BEGIN
    IF NOT rpc.readnum  (buf,pos,e.fileid) THEN RETURN FALSE END;
    IF NOT rpc.readnum  (buf,pos,e.mode  ) THEN RETURN FALSE END;
    IF NOT getstring    (buf,pos,e.name)   THEN RETURN FALSE END;
    IF NOT getfixbarray (buf,pos,e.cookie) THEN RETURN FALSE END;
    RETURN TRUE
  END getentry;

  PROCEDURE calcentries(VAR wp: ARRAY OF WORD; p: INTEGER): INTEGER;
    VAR co,s: INTEGER;
        e   : entry;
  BEGIN
    co:=0;
    LOOP
      IF NOT rpc.readnum(wp,p,s) THEN RETURN -1 END;
      IF s=0 THEN RETURN co END;
      IF NOT getentry(wp,p,e)    THEN RETURN -1 END;
      INC(co)
    END
  END calcentries;

  PROCEDURE getentries(VAR e: ARRAY OF entry): BOOLEAN;
    VAR i,s: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(e) DO
      IF NOT rpc.readnum(wp,pos,s) THEN RETURN FALSE END;
      IF s#1 THEN RETURN FALSE END;
      IF NOT getentry(wp,pos,e[i]) THEN RETURN FALSE END
    END;
    RETURN TRUE
  END getentries;

  VAR     len: INTEGER;

BEGIN
  wp^.ADR :=rp.adr;
  wp^.HIGH:=rp.sz-1;
  pos:=0;
  IF NOT rpc.readnum(wp,pos,r.state) THEN RETURN err.inconsistency END;
  IF r.state#NFS_OK THEN RETURN err.ok END;
  len:=calcentries(wp,pos);
  IF len<0 THEN RETURN err.inconsistency END;
  NEW(r.entries,len);
  IF r.entries^.ADR=NIL THEN RETURN err.no_memory END;
  IF    (NOT getentries (r.entries))
     OR (NOT rpc.readnum(wp,pos,len))
     OR (NOT rpc.readnum(wp,pos,len))
     OR (len#0)&(len#1)
     OR (pos#rp.sz)
  THEN
    DISPOSE(r.entries); RETURN err.inconsistency
  END;
  r.eof:=BOOLEAN(len);
  RETURN err.ok
END getrdirres;

(* 16 *)
PROCEDURE readdir(VAR ia: nfa; VAR a: readdirargs; VAR r: readdirres): INTEGER;
  VAR buf: ARRAY [0..9] OF WORD;
      adr: rpc.rpcadr;
      b  : nos.buffer;
      rp : rpc.buffer;
      res: INTEGER;
BEGIN
  IF a.count>(1024*8) THEN RETURN err.bad_parm END;
  IF nfscontext.port=rpc.null THEN RETURN err.undef END;
  fillna(adr,ia.iadr,16,ia.tout);
  putrdirargs(buf,a,b);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getrdirres(r,rp);
  u(rp);
  RETURN res
END readdir;

TYPE directory = DYNARR OF fs.dnode;
     entries   = DYNARR OF entry;

PROCEDURE dir_walk(VAR fh: fhandle; VAR dir: directory): INTEGER;
  VAR f  : fs.FILE;
      res: INTEGER;
      len: INTEGER;
BEGIN
  NEW(dir);
  res:=open(f,fh);
  IF res#NFS_OK      THEN           RETURN res           END;
  IF NOT fs.isdir(f) THEN close(f); RETURN NFSERR_NOTDIR END;
  fs.getattr(f,fs.a_eof,len);
  NEW(dir,len DIV 64);
  IF dir^.ADR=NIL THEN close(f); RETURN -1 END;
  enter;
    res:=fs.read(f,0,dir^.ADR,0,len);
  exit;
  close(f);
  IF res#err.ok THEN DISPOSE(dir); RETURN NFSERR_IO END;
  RETURN NFS_OK
END dir_walk;

PROCEDURE get_entry(VAR dir   : ARRAY OF fs.dnode;
                    VAR c     : INTEGER;
                    VAR s     : ARRAY OF CHAR;
                    VAR fileid: INTEGER;
                    VAR fmode : INTEGER): BOOLEAN;
  VAR k: BITSET;
BEGIN
  ASSERT(c>=0);
  WHILE c<=HIGH(dir) DO
    WITH dir[c] DO
      INC(c);
      IF (kind*fs.d_entry#{}) & (kind*fs.d_del={}) THEN
        fileid:=inod;
        fmode :=INTEGER(kind);
        str.copy(s,name);
        RETURN TRUE
      END
    END
  END;
  RETURN FALSE
END get_entry;

PROCEDURE __readdir(VAR f: fhandle; c,co: INTEGER; VAR e: entries;
                    VAR eof: BOOLEAN): INTEGER;
  VAR dir : directory;
      res : INTEGER;
      coo : INTEGER;
      cnt : INTEGER;
      s   : ARRAY [0..31] OF CHAR;
      fi,i: INTEGER;
      fm  : INTEGER;
      p   : POINTER TO INTEGER;
BEGIN
  res:=dir_walk(f,dir);
  IF res#NFS_OK THEN RETURN res END;
  i  :=0;
  coo:=c;
  cnt:=0;
  eof:=FALSE;
  LOOP
    IF NOT get_entry(dir,coo,s,fi,fm) THEN eof:=TRUE; EXIT END;
    IF i+BYTES(entry)+8>co THEN EXIT END;
    i  :=i+BYTES(entry)+8;
    cnt:=cnt+1
  END;
  NEW(e,cnt);
  IF e^.ADR=NIL THEN DISPOSE(dir); RETURN -1 END;

  --tty.print('%d files\n',cnt);

  coo:=c;
  FOR i:=0 TO cnt-1 DO
    ASSERT(get_entry(dir,coo,e[i].name,e[i].fileid,e[i].mode));
    p:=SYSTEM.ADR(e[i].cookie); p^:=coo
  END;

  DISPOSE(dir);

--;tty.print('got entries\n');

  RETURN NFS_OK
END __readdir;

PROCEDURE _readdir(nc: NCTX; VAR parm: ARRAY OF WORD);

  PROCEDURE getparm(VAR fh: fhandle; VAR c: nfscookie; VAR co: INTEGER): BOOLEAN;
    VAR pos: INTEGER;
  BEGIN

    --tty.print('try to get parms\n');

    pos:=0;
    IF NOT getfhandle  (parm,pos,fh) THEN RETURN FALSE END;
    IF NOT getfixbarray(parm,pos,c ) THEN RETURN FALSE END;
    IF NOT rpc.readnum (parm,pos,co) THEN RETURN FALSE END;
    IF (co<0) OR (pos#HIGH(parm)+1)  THEN RETURN FALSE END;

    --tty.print('parameters ok\n');

    RETURN TRUE
  END getparm;

  PROCEDURE reply(VAR e: ARRAY OF entry; r: INTEGER; eof: BOOLEAN);
    VAR wp : DYNARR OF WORD;
        rp : rpc.buffer;
        pos: INTEGER;
        i  : INTEGER;
  BEGIN

    --tty.print('e: start reply\n');

    IF r<0 THEN RETURN END;
    IF r=NFS_OK THEN
      NEW(wp,(HIGH(e)+1)*(SIZE(entry)+2)+3);
    ELSE
      NEW(wp,1);
    END;
    IF wp^.ADR=NIL THEN RETURN END;
    pos:=0;
    rpc.writenum(wp,pos,r);
    IF r=NFS_OK THEN
      FOR i:=0 TO HIGH(e) DO
        rpc.writenum(wp,pos,1);
        rpc.writenum(wp,pos,e[i].fileid);
        rpc.writenum(wp,pos,e[i].mode  );
        putstring   (wp,pos,e[i].name);
        putfixbarray(wp,pos,e[i].cookie)
      END;
      rpc.writenum(wp,pos,0);
      IF eof THEN rpc.writenum(wp,pos,1) ELSE rpc.writenum(wp,pos,0) END
    END;
    rp.adr:=wp^.ADR;
    rp.sz :=wp^.HIGH+1;
    ASSERT(pos=rp.sz);

    --tty.print('send reply %d entries\n',HIGH(e)+1);

    rpc.reply(nc^.port,rp)
  END reply;

  VAR fh : fhandle;
      c  : nfscookie;
      co : INTEGER;
      p  : POINTER TO INTEGER;
      e  : entries;
      r  : INTEGER;
      eof: BOOLEAN;
BEGIN
  IF NOT getparm(fh,c,co) THEN rpc.garbageargs(nc^.port); RETURN END;
  p :=SYSTEM.ADR(c);
  NEW(e);
  r:=__readdir(fh,p^,co,e,eof);
  reply(e,r,eof);
  DISPOSE(e)
END _readdir;

PROCEDURE getstatfsres(VAR r: statfsres; VAR rp: rpc.buffer): INTEGER;
  VAR buf: DYNARR OF WORD;
      pos: INTEGER;
      pw : POINTER TO ARRAY [0..4] OF INTEGER;
BEGIN
  buf^.ADR :=rp.adr;
  buf^.HIGH:=rp.sz-1;
  pos:=0;
  IF NOT rpc.readnum(buf,pos,r.state) THEN RETURN err.inconsistency END;
  IF r.state#NFS_OK THEN RETURN err.ok END;
  pw:=ADDRESS(SYSTEM.ADR(r)+1);
  IF NOT getfixiarray(buf,pos,pw^) THEN RETURN err.inconsistency END;
  RETURN err.ok
END getstatfsres;

(* 17 *)
PROCEDURE statfs (VAR ia: nfa; VAR a: fhandle; VAR r: statfsres): INTEGER;
  VAR b  : nos.buffer;
      rp : rpc.buffer;
      adr: rpc.rpcadr;
      res: INTEGER;
BEGIN
  b.buf :=SYSTEM.ADR(a);
  b.pos :=0;
  b.len :=8*4;
  b.next:=NIL;
  fillna(adr,ia.iadr,17,ia.tout);
  res:=rpc.call(adr,nfscontext.port,b,b.len,rp);
  IF res#err.ok THEN RETURN res END;
  IF (adr.stat#rpc.MSG_ACCEPTED) OR (adr.res#rpc.SUCCESS) THEN
    RETURN err.bad_parm
  END;
  res:=getstatfsres(r,rp);
  u(rp);
  RETURN res
END statfs;

PROCEDURE __statfs(VAR fh: fhandle; VAR r: statfsres);
  VAR f   : fs.FILE;
      free: INTEGER;
      used: INTEGER;
      res : INTEGER;
BEGIN
  r.state:=open(f,fh);
  IF r.state#NFS_OK THEN RETURN END;
  res:=fs.du(f,free,used);
  IF res#err.ok THEN
    IF res=err.not_dir THEN r.state:=NFSERR_NOTDIR
    ELSE                    r.state:=NFSERR_IO
    END;
    close(f);
    RETURN
  END;
  close(f);
  r.tsize :=1024*8;
  r.bsize :=1024*4;
  r.blocks:=(free+used) DIV r.bsize;
  r.bfree :=free DIV r.bsize;
  r.bavail:=r.bfree
END __statfs;

PROCEDURE _statfs(nc: NCTX; VAR parm: ARRAY OF WORD);
  VAR pos: INTEGER;
      fh : fhandle;
      r  : statfsres;
      rp : rpc.buffer;
      buf: DYNARR OF WORD;
      pa : POINTER TO ARRAY [0..4] OF INTEGER;
BEGIN
  pos:=0;
  IF NOT getfhandle(parm,pos,fh) THEN rpc.garbageargs(nc^.port); RETURN END;
  IF HIGH(parm)+1#pos             THEN rpc.garbageargs(nc^.port); RETURN END;
  __statfs(fh,r);
  IF r.state=NFS_OK THEN
    NEW(buf,6);
  ELSE
    NEW(buf,1)
  END;
  IF buf^.ADR=NIL THEN RETURN END;
  pos:=0;
  rpc.writenum(buf,pos,r.state);
  IF r.state=NFS_OK THEN
    pa:=ADDRESS(SYSTEM.ADR(r)+1);
    putfixiarray(buf,pos,pa^)
  END;
  rp.adr:=buf^.ADR;
  rp.sz :=buf^.HIGH+1;
  ASSERT(pos=rp.sz);

  --tty.print('send statfs reply %d\n',rp.sz);

  rpc.reply(nc^.port,rp)
END _statfs;

PROCEDURE nfsdoit(nc: NCTX; proc,vers: INTEGER; VAR parm: ARRAY OF WORD);
BEGIN

--  tty.print('nfs doit proc=%d\n',proc);

  IF nc#SYSTEM.ADR(nfscontext) THEN RETURN END;
  CASE proc OF
  |01: _getattr(nc,parm)
  |02: _setattr(nc,parm)
  |04: _lookup (nc,parm)
  |06: _read   (nc,parm)
  |08: _write  (nc,parm)
  |09: _create (nc,parm,FALSE)
  |10: _remove (nc,parm,FALSE)
  |11: _rename (nc,parm)
  |12: _link   (nc,parm)
  |14: _create (nc,parm,TRUE)
  |15: _remove (nc,parm,TRUE)
  |16: _readdir(nc,parm)
  |17: _statfs (nc,parm)
  ELSE
    RETURN
  END;

END nfsdoit;

----------------------------- MOUNT ----------------------------
                             -------
TYPE
     MCTX = POINTER TO mctx;
     mctx = RECORD
              port: rpc.PORT;
            END;

VAR     mntcontext: mctx;

PROCEDURE fillma(VAR a: rpc.rpcadr; host,proc,tout: INTEGER);
BEGIN
  a.iadr:=host;
  a.port:=MNTPORT;
  a.prog:=MNTPROG;
  a.proc:=proc;
  a.vers:=1;
  a.tout:=tout;
  a.try :=10;
END fillma;

(* 00 *)
PROCEDURE mnull;
BEGIN
END mnull;

(* 01 *)
PROCEDURE mount(VAR ia: nfa; VAR dpath: ARRAY OF CHAR; VAR r: fhstatus): INTEGER;
  VAR buf: ARRAY [0..0] OF WORD;
      pos: INTEGER;
      a  : rpc.rpcadr;
      b  : ARRAY [0..1] OF nos.buffer;
      rp : rpc.buffer;
      wp : POINTER TO ARRAY [0..0] OF WORD;
      res: INTEGER;
BEGIN
  IF mntcontext.port=rpc.null THEN RETURN err.undef END;
  r.status:=-1;
  fillma(a,ia.iadr,01,ia.tout);
  pos:=0;
  rpc.writenum(buf,pos,BYTES(dpath));
  b[0].buf :=SYSTEM.ADR(buf);
  b[0].pos :=0;
  b[0].len :=BYTES(buf);
  b[0].next:=SYSTEM.ADR(b[1]);
  b[1].buf :=SYSTEM.ADR(dpath);
  b[1].pos :=0;
  b[1].len :=(BYTES(dpath)+3) DIV 4*4;
  b[1].next:=NIL;
  res:=rpc.call(a,mntcontext.port,b[0],b[0].len+b[1].len,rp);
  IF res#err.ok THEN RETURN res END;
  IF rp.sz<1 THEN RETURN err.bad_parm END;
  pos:=0;
  wp :=rp.adr;
  IF NOT rpc.readnum(wp^,pos,r.status) THEN HALT(1)  END;
  IF r.status#0 THEN u(rp); RETURN err.ok            END;
  IF rp.sz   #9 THEN u(rp); RETURN err.inconsistency END;
  nos.move(SYSTEM.ADR(r.directory),0,rp.adr,4,8*4);
  u(rp);
  RETURN err.ok
END mount;

PROCEDURE __mount(VAL dpath: ARRAY OF CHAR; VAR fh: fhandle): INTEGER;

  PROCEDURE openpath(VAL dapth: ARRAY OF CHAR; r: fs.FILE;
                     VAR f: fs.FILE): INTEGER;
    PROCEDURE u;
      VAR res: INTEGER;
    BEGIN
      res:=fs.close(f)
    END u;

    VAR name: ARRAY [0..31] OF CHAR;
        res : INTEGER;
        i,j : INTEGER;
        stop: BOOLEAN;
  BEGIN
    i:=1; f:=r;

    IF (dpath[i]=0c)OR(i>HIGH(dpath)) THEN RETURN err.ok END;
    LOOP
      j:=0;
      LOOP
        IF (i>HIGH(dpath))OR(j>HIGH(name)) THEN u; RETURN err.bad_name END;
        IF dpath[i]=0c   THEN stop:=TRUE; EXIT END;
        IF dpath[i]='/'  THEN
          IF i=HIGH(dpath) THEN u; RETURN err.bad_name END;
          stop:=FALSE;
          i:=i+1;
          EXIT
        END;
        name[j]:=dpath[i];
        INC(i); INC(j)
      END;
      IF j<=HIGH(name) THEN name[j]:=0c END;
      res:=fs.open(r,f,name,{});
      IF res#err.ok THEN u; RETURN res END;
      res:=fs.close(r);
      IF res#err.ok THEN u; RETURN res END;
      r:=f;
      IF stop THEN RETURN err.ok END
    END
  END openpath;

  VAR res: INTEGER;
      FH : POINTER TO fs.FHANDLE;
      f  : fs.FILE;
BEGIN
  ASSERT(err.ok=0);
  IF dpath[0]#'/' THEN RETURN err.bad_name END;
  res:=fs.reopen(fs.root());
  IF res#err.ok THEN RETURN res END;
  res:=openpath(dpath,fs.root(),f);
  IF res#err.ok THEN RETURN res END;
  FH:=SYSTEM.ADR(fh);
  fs.getfhandle(f,FH^);
  IF fs.close(f)#0 THEN END;
  RETURN err.ok
END __mount;

PROCEDURE _mount(mc: MCTX; VAR parm: ARRAY OF WORD);
  VAR rp : rpc.buffer;
      pos: INTEGER;
      len: INTEGER;
      str: DYNARR OF CHAR;
      res: INTEGER;
      wp : POINTER TO ARRAY [0..8] OF WORD;
      fh : fhandle;
BEGIN
  IF SIZE(parm)<1 THEN rpc.garbageargs(mc^.port); RETURN END;
  pos:=0;
  IF NOT rpc.readnum(parm,pos,len) THEN HALT(1) END;
  IF len<0 THEN rpc.garbageargs(mc^.port); RETURN END;
  (*$<$U+*)
  str^.ADR :=SYSTEM.ADR(parm)+1;
  str^.HIGH:=len-1;
  (*$>*)
  IF (len+3) DIV 4 + 1 # SIZE(parm) THEN rpc.garbageargs(mc^.port); RETURN END;
  res:=__mount(str,fh);
  IF res=0 THEN rp.sz:=9 ELSE rp.sz:=1 END;
  nos.allocate(rp.adr,rp.sz);
  IF rp.adr=NIL THEN RETURN END;
  wp:=rp.adr; pos:=0;
  rpc.writenum(wp^,pos,res);
  IF res=0 THEN
    nos.move(rp.adr,4,SYSTEM.ADR(fh),0,8*4)
  END;
  rpc.reply(mc^.port,rp)
END _mount;

(* 02 *)
PROCEDURE mdump(VAR ia: nfa; VAR ml: MOUNTLIST): INTEGER;
BEGIN
END mdump;

(* 03 *)
PROCEDURE unmount(VAR ia: nfa; VAR dirpath: ARRAY OF CHAR): INTEGER;
BEGIN
END unmount;

(* 04 *)
PROCEDURE unmountall(VAR ia: nfa): INTEGER;
BEGIN
END unmountall;

(* 05 *)
PROCEDURE export(VAR ia: nfa; VAR el: exportlist): INTEGER;
BEGIN
END export;

PROCEDURE mntdoit(mc: MCTX; proc,vers: INTEGER; VAR parm: ARRAY OF WORD);
BEGIN

  --tty.print('mnt doit proc=%d\n',proc);

  IF mc#SYSTEM.ADR(mntcontext) THEN RETURN END;
  CASE proc OF
  |1: _mount(mc,parm)
  ELSE
    RETURN
  END;

END mntdoit;

PROCEDURE install;
  VAR i: rpc.install;
BEGIN
  i.port :=MNTPORT;
  i.prog :=MNTPROG;
  i.nproc:=6;
  i.vlow :=1;
  i.vhigh:=1;
  i.doit :=mntdoit;
  i.obj  :=SYSTEM.ADR(mntcontext);

  IF rpc.create(mntcontext.port,i)#err.ok THEN RETURN END;

  i.port :=NFSPORT;
  i.prog :=NFSPROG;
  i.nproc:=18;
  i.vlow :=2;
  i.vhigh:=2;
  i.doit :=nfsdoit;
  i.obj  :=SYSTEM.ADR(nfscontext);

  IF rpc.create(nfscontext.port,i)#err.ok THEN END

END install;

BEGIN
  install
END nfs.
