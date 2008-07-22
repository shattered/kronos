MODULE MSDOS; (* Igo        02-Mar-92. (c) KRONOS *)
              (* Igo & John 09-Mar-92. (c) KRONOS *)

(* $N+ *)
(* $U+ *)

IMPORT  SYSTEM;
IMPORT  ASCII;
IMPORT  os : osKernel;
IMPORT  fs : osFiles;
IMPORT  fs0: osFileSystem;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  req: defRequest;
IMPORT  str: Strings;
IMPORT  env: tskEnv;
IMPORT  low: lowLevel;
IMPORT  tim: Time;

IMPORT  tty: Terminal;

CONST -- disk modes
  _dir     = {1};

CONST -- open file modes
  dirty    =  {0};

CONST ROOT = 0;

TYPE

  ADDRESS = SYSTEM.ADDRESS;
  WORD    = SYSTEM.WORD;
  str32   = ARRAY [0..31] OF CHAR;

  PORT = POINTER TO port;
  port = RECORD
           dev  : fs.FILE;
           opens: INTEGER;

           bps  : INTEGER; (* длина сектора в байтах    *)
           bpc  : INTEGER; (* длина кластера в байтах   *)
           spc  : INTEGER; (* число секторов в кластере *)
           rsec : INTEGER; (* число зарезервированных секторов *)
           nFATs: INTEGER; (* число FAT-ов  *)
           rdent: INTEGER; (* макс.число файлов в корневой директории *)
           tsec : INTEGER; (* общее число секторов на диске *)
           mdesc: INTEGER; (* тип носителя *)
           spFAT: INTEGER; (* число секторов в FAT-е *)
           spTRK: INTEGER; (* число секторов в треке *)
           heads: INTEGER; (* количество головок и, соответственно, сторон *)
           hsec : INTEGER; (* число "невидимых" секторов *)

--------------------  производные значения  --------------------
                    ------------------------

           offs : INTEGER; (* число секторов, занятых служебной информацией, *)
                           (* то есть номер первого информационного сектора  *)
           mclu : INTEGER; (* номер последнего кластера *)

           FAT12: BOOLEAN;

           FATs : POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
           FATsz: INTEGER;

           fd   : BOOLEAN;

           savspec: req.REQUEST;
         END;

  FILE = POINTER TO file_desc;
  file_desc =
  RECORD
    file  : fs0._file;
    magic : INTEGER;
    fwd   : FILE;
    bck   : FILE;

    dir   : INTEGER;
    entry : INTEGER;
    ffat  : INTEGER;
    dev   : PORT;

    flock : os.signal_rec; (* file system lock *)
    ulock : os.mutex_rec;  (* user system lock *)
    opens : INTEGER;
    state : BITSET;
    host  : fs0._FILE;  (* ".." for mounted fsys *)

    eof   : INTEGER;
    time  : INTEGER;
    links : INTEGER;
    mode  : BITSET;

  END;

  INODE = POINTER TO inode;
  inode = RECORD
            ffat : INTEGER;
            dir  : INTEGER;
            entry: INTEGER;
            links: INTEGER;
            eof  : INTEGER;
            mode : BITSET;
            state: BITSET;
            time : INTEGER;
            name : str32;
          END;

  MSDIR = DYNARR OF str32;

---------------------  LOW LEVEL SUPPORT  ----------------------
                     ---------------------

PROCEDURE dpw2(x,n: INTEGER): INTEGER;  CODE cod.shr END dpw2;
PROCEDURE mpw2(x,n: INTEGER): INTEGER;  CODE cod.shl END mpw2;

PROCEDURE ei(m: BITSET); CODE cod.setm END ei;
PROCEDURE di(VAR m: BITSET);
CODE cod.getm cod.copt cod.li3 cod.bic cod.setm cod.ssw0 END di;

PROCEDURE TR(VAR d: BITSET): BITSET; CODE cod.tr END TR;

PROCEDURE quit(n: WORD); CODE cod.quit cod.drop END quit;


VAR MAGIC: INTEGER;

    msdesc_ptr: fs0.FS;
    msdesc    : fs0.fsdesc;
    mshalt    : os.signal_rec;

CONST _MAGIC = 954C4946h;

PROCEDURE check_p_stack(size: INTEGER);
CODE cod.copt cod.alloc cod.drop cod.decs END check_p_stack;
(* if "S register" + "size" > "H register" then TRAP(40h) *)

----------------------------------------------------------------

VAR Flist: FILE;
    Flock: os.signal_rec;

PROCEDURE tie_file(f: FILE);
BEGIN
  f^.file.fmagic:=fs0.FMAGIC;
  f^.file.fsid  :=msdesc_ptr;
  f^.magic:=MAGIC;
  f^.opens:=1;
  (* NB: Only primary "_open" increments "port^.opens" *)
  INC(f^.dev^.opens);
  (* include "f" in "Flist": *)

  IF Flist=NIL THEN
    f^.fwd:=f; f^.bck:=f;
  ELSE
    f^.bck:=Flist^.bck;
    f^.fwd:=Flist;
    f^.fwd^.bck:=f;
    f^.bck^.fwd:=f;
  END;
  Flist:=f;
  os.ini_signal(f^.flock,os.guard,1);
  os.ini_mutex (f^.ulock);
END tie_file;

PROCEDURE _open(VAR f: FILE; VAL file: inode; dev: PORT): INTEGER;
  VAR l: FILE;
     io: INTEGER;
BEGIN
  l:=Flist;
  IF l#NIL THEN
    LOOP
      IF (l^.dev=dev) & (l^.ffat=file.ffat) THEN
        f:=l;
        INC(f^.opens);
        RETURN err.ok
      END;
      l:=l^.fwd;
      IF l=Flist THEN l:=NIL; EXIT END;
    END
  END;
  os.ALLOCATE(os.system,f,SIZE(file_desc));
  IF f=NIL THEN RETURN err.no_memory END;
  f^.ffat :=file.ffat;  f^.host :=fs0._FILE(f);
  f^.magic:=0;          f^.mode :=file.mode;
  f^.dir  :=file.dir;   f^.entry:=file.entry;
  f^.time :=file.time;  f^.eof  :=file.eof;
  f^.state:=file.state; f^.links:=file.links;
                        f^.dev  :=dev;
  tie_file(f);
  RETURN err.ok
END _open;

PROCEDURE destroy(f: FILE);
  VAR dev: INTEGER;
BEGIN
  IF f^.dev#NIL THEN
    IF  f^.dev^.opens>0 THEN DEC(f^.dev^.opens) ELSE f^.dev^.opens:=0 END
  END;
  (* delete out of "Flist" *)
  IF Flist=f THEN Flist:=Flist^.fwd END;
  IF Flist=f THEN Flist:=NIL
  ELSE
    f^.fwd^.bck:=f^.bck;
    f^.bck^.fwd:=f^.fwd
  END;
  f^.magic:=0;
  f^.file.fmagic:=0;
  f^.file.fsid  :=NIL;
  os.DEALLOCATE(os.system,f,SIZE(file_desc));
END destroy;

PROCEDURE getattr(f: FILE; atr: INTEGER; VAR val: SYSTEM.WORD);
BEGIN
  ASSERT((f#NIL)&(f^.magic=MAGIC));
  CASE atr OF
  |fs.a_eof  : IF f^.mode*_dir#{} THEN val:=64 ELSE val:=f^.eof END;
  |fs.a_pro  : val:={}
  |fs.a_ctime: val:=f^.time
  |fs.a_wtime: val:=f^.time
  |fs.a_links: val:=f^.links
  |fs.a_mode : val:=f^.mode
  |fs.a_inode: val:=f^.ffat
  END
END getattr;

PROCEDURE setattr(f: FILE; atr: INTEGER;     val: SYSTEM.WORD);
BEGIN
  CASE atr OF
  |fs.a_eof  : f^.eof :=val
  |fs.a_pro  : RETURN
  |fs.a_ctime: f^.time:=val
  |fs.a_wtime: f^.time:=val
  |fs.a_links: RETURN
  |fs.a_mode : RETURN
  END;
  f^.state:=f^.state+dirty
END setattr;

PROCEDURE getfh(f: FILE; VAR fh: fs.FHANDLE);
BEGIN
  fh.fsys:='MSDOS'
END getfh;

PROCEDURE isdir(f: FILE): BOOLEAN;
BEGIN RETURN f^.mode*_dir#{} END isdir;

PROCEDURE state(f: FILE): BITSET;
BEGIN RETURN f^.state END state;

PROCEDURE usrlock(f: FILE; to: INTEGER): INTEGER;
  VAR res: INTEGER;
BEGIN
  IF os.acquire_del(to,f^.ulock) THEN res:=err.ok ELSE res:=err.time_out END;
  RETURN res
END usrlock;

PROCEDURE usrunlock(f: FILE);
BEGIN
  os.release(f^.ulock)
END usrunlock;

PROCEDURE loadFATs(dev: PORT): INTEGER;
  VAR l: INTEGER;
BEGIN
  l:=dev^.spFAT*dev^.bps;
  os.ALLOCATE(os.system,dev^.FATs,(l+3) DIV 4);
  IF dev^.FATs=NIL THEN RETURN err.no_memory END;
  dev^.FATsz:=(l+3) DIV 4;
  RETURN fs.read(dev^.dev,dev^.rsec*dev^.bps,dev^.FATs,0,l)
END loadFATs;

PROCEDURE saveFATs(dev: PORT): INTEGER;
  VAR l   : INTEGER;
      res0: INTEGER;
      res1: INTEGER;
BEGIN
  IF NOT dev^.fd THEN RETURN err.ok END;
  l:=dev^.spFAT*dev^.bps;
  res0:=fs.write(dev^.dev,dev^.rsec*dev^.bps,dev^.FATs,0,l);
  l:=dev^.spFAT*dev^.bps;
  res1:=fs.write(dev^.dev,(dev^.rsec+dev^.spFAT)*dev^.bps,dev^.FATs,0,l);
  IF res0#err.ok THEN RETURN res0 END;
  RETURN res1
END saveFATs;

PROCEDURE FAT(p: PORT; i: INTEGER): INTEGER;
  VAR k: INTEGER;
BEGIN
  ASSERT( (2<=i) & (i<=p^.mclu) );
  IF p^.FAT12 THEN
    k:=i + i DIV 2;
    k:=ORD(p^.FATs^[k])+ORD(p^.FATs^[k+1])*100h;
    IF ODD(i) THEN RETURN k DIV 10h ELSE RETURN k MOD 1000h END;
  ELSE
    k:=i*2;
    RETURN ORD(p^.FATs^[k])+ORD(p^.FATs^[k+1])*100h
  END
END FAT;

PROCEDURE putFAT(p: PORT; i,x:INTEGER);
  VAR k,e: INTEGER;
BEGIN
  IF p^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  ASSERT( (2<=i) & (i<=p^.mclu) );
  ASSERT( (0<=x) & (x<=e) );
  IF p^.FAT12 THEN
    k:=i + i DIV 2;
    IF ODD(i)THEN
      p^.FATs^[k]  :=CHAR(ORD(p^.FATs^[k]) MOD 10h + (x MOD 10h)*10h);
      p^.FATs^[k+1]:=CHAR(x DIV 16);
    ELSE
      p^.FATs^[k]  :=CHAR(x MOD 100h);
      p^.FATs^[k+1]:=CHAR(BITSET(p^.FATs^[k+1])*{4..7} + BITSET(x DIV 256))
    END
  ELSE
    k:=i*2;
    p^.FATs^[k  ]:=CHAR(x MOD 100h);
    p^.FATs^[k+1]:=CHAR(x DIV 100h)
  END
END putFAT;

PROCEDURE remove_cluster(p: PORT; clu: INTEGER; save: BOOLEAN);
  VAR next,e: INTEGER;
BEGIN
  IF p^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;

  WHILE (clu#0) & (clu#e) DO
    next:=FAT(p,clu); putFAT(p,clu,0); clu:=next; p^.fd:=TRUE
  END;

  IF save THEN next:=saveFATs(p); p^.fd:=FALSE END

END remove_cluster;

PROCEDURE alloc_cluster(p: PORT; no: INTEGER;
                        VAR first: INTEGER; fat: INTEGER): BOOLEAN;
  VAR i,cl: INTEGER;
      fd  : BOOLEAN;
BEGIN
  ASSERT(no>0);
  i:=2; first:=0;
  REPEAT
    IF FAT(p,i)=0 THEN
      IF first = 0 THEN first:=i ELSE putFAT(p,cl,i) END;
      cl:= i; DEC(no)
    END;
    INC(i);
  UNTIL (no=0) OR (i>p^.mclu);
  IF no=0 THEN
    IF p^.FAT12 THEN putFAT(p,cl,0FFFh) ELSE putFAT(p,cl,0FFFFh) END;

    IF fat#0 THEN putFAT(p,fat,first) END;
    p^.fd:=TRUE;
    i:=saveFATs(p); p^.fd:=FALSE;
    RETURN TRUE
  ELSIF first#0 THEN
    fd:=p^.fd;
    remove_cluster(p,first,FALSE);
    p^.fd:=fd
  END;
  RETURN FALSE
END alloc_cluster;

PROCEDURE free(p: PORT): INTEGER;
  VAR i,s: INTEGER;
BEGIN
  i:=2; s:=0;
  REPEAT
    IF FAT(p,i)=0 THEN INC(s) END; INC(i)
  UNTIL i=p^.mclu;
  RETURN s*p^.bpc
END free;

PROCEDURE readfile(dev: PORT; ffat,len: INTEGER; buf: ADDRESS): INTEGER;
  VAR res: INTEGER;
      n  : INTEGER;
      ofs: INTEGER;
      e  : INTEGER;
BEGIN
  ofs:=0;
  IF dev^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  WHILE len>0 DO
    IF (ffat<=1) OR (ffat>=e) THEN RETURN err.no_data END;

    n  :=dev^.bpc;
    res:=fs.read(dev^.dev,((ffat-2)*dev^.spc+dev^.offs)*dev^.bps,buf,ofs,n);

    IF res#err.ok THEN RETURN res END;

    ofs :=ofs+n;
    len :=len-1;
    ffat:=FAT(dev,ffat)

  END;
  RETURN err.ok
END readfile;

PROCEDURE packname(VAR p: str32; VAL name: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO 10 DO p[i]:=' ' END;
  i:=0;
  WHILE (i<8) & (name[i]#0c) & (name[i]#'.') DO
    p[i]:=ASCII.CAPITAL(name[i]); INC(i)
  END;
  WHILE (i<=HIGH(p))&(name[i]#'.')&(name[i]#0c) DO INC(i) END;
  IF (i>HIGH(p)) OR (name[i]=0c) THEN RETURN END;
  i:=i+1;
  j:=8;
  WHILE (j<11) & (i<=HIGH(name)) & (name[i]#0c) DO
    p[j]:=ASCII.CAPITAL(name[i]); INC(i); INC(j)
  END
END packname;

PROCEDURE packentry(VAR p: str32; f: FILE);
  VAR i: INTEGER;                   s,m,h: INTEGER;
                                    d,n,y: INTEGER;
BEGIN

  low._zero(SYSTEM.ADR(p),BYTES(p) DIV 4);

  p[11]:=CHAR({5});
  IF f^.mode *_dir#{} THEN p[11]:=CHAR({4}) END;
  tim.unpack(f^.time,y,n,d,h,m,s); y:=y-1980;
  s:=s DIV 2 + (m+h*64)*32;
  p[22]:=CHAR(s);       p[23]:=CHAR(s DIV 256);
  d:=d+(n+y*16)*32;
  p[24]:=CHAR(d);       p[25]:=CHAR(d DIV 256);
  p[26]:=CHAR(f^.ffat); p[27]:=CHAR(f^.ffat DIV 256);
  p[28]:=CHAR(f^.eof ); p[29]:=CHAR(f^.eof  DIV 256);
  p[30]:=CHAR(f^.eof DIV 10000h);  p[31]:=0c
END packentry;

PROCEDURE findfat(p: PORT; fat: INTEGER; ofs: INTEGER): INTEGER;
  VAR e: INTEGER;
BEGIN
  IF p^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;

  WHILE (ofs>=p^.bpc)&(fat>1)&(fat<e)  DO
    DEC(ofs,p^.bpc);
    fat:=FAT(p,fat)
  END;
  RETURN fat
END findfat;

PROCEDURE writeentry(p: PORT; fat: INTEGER; buf: ADDRESS;
                                   ofs,bpos,len: INTEGER): INTEGER;
  VAR n  : INTEGER;
      res: INTEGER;
      e  : INTEGER;
BEGIN
  IF p^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  IF fat=ROOT THEN
    n:=len;
    RETURN fs.write(p^.dev,(p^.nFATs*p^.spFAT + p^.rsec)*p^.bps+ofs,buf,bpos,n)
  ELSE
    IF (fat<=1) OR (fat>=e) THEN RETURN err.bad_fsys END;
    fat:=findfat(p,fat,ofs);
    IF (fat<=1) OR (fat>=e) THEN RETURN err.bad_fsys END;
    ofs:=ofs MOD p^.bpc;
    WHILE len>0 DO
      n:=p^.bpc-ofs;
      IF n>len THEN n:=len END;
      res:=fs.write(p^.dev,((fat-2)*p^.spc+p^.offs)*p^.bps+ofs,buf,bpos,n);
      IF res#err.ok THEN RETURN res END;
      ofs:=0;
      INC(bpos,n); DEC(len,n); fat:=FAT(p,fat)
    END
  END;
  RETURN err.ok
END writeentry;

PROCEDURE updateentry(f: FILE; VAL name: ARRAY OF CHAR): INTEGER;
  VAR entry: str32;
      res  : INTEGER;
BEGIN
  IF (f^.state*dirty={}) OR (f^.links<=0) THEN RETURN err.ok END;
  packentry(entry,f);
  packname (entry,name);
  res:=writeentry(f^.dev,f^.dir,SYSTEM.ADR(entry),f^.entry*32,0,32);
  IF res=err.ok THEN f^.state:=f^.state-dirty END;
  RETURN res
END updateentry;

PROCEDURE updatefile(f: FILE): INTEGER;
  VAR entry: str32;
      res  : INTEGER;
BEGIN
  IF (f^.state*dirty={}) OR (f^.links<=0) THEN RETURN err.ok END;
  packentry(entry,f);
  res:=writeentry(f^.dev,f^.dir,SYSTEM.ADR(entry),f^.entry*32+11,11,32-11);
  IF res=err.ok THEN f^.state:=f^.state-dirty END;
  RETURN res
END updatefile;

PROCEDURE close (f: FILE): INTEGER;
  VAR res: INTEGER;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  os.wait(Flock);
    os.wait(f^.flock);
    res:=err.ok;
    IF f^.opens>0 THEN DEC(f^.opens) ELSE f^.opens:=0 END;
    IF f^.opens=0 THEN
      IF    f^.links<=0       THEN remove_cluster(f^.dev,f^.ffat,TRUE)
      ELSIF f^.state*dirty#{} THEN
        res:=updatefile(f)
      END;
      destroy(f); os.unlock
    ELSE
      os.send(f^.flock)
    END;
  os.send(Flock);
  RETURN res
END close;

-------------------  D I R E C T O R I E S  --------------------
                   -------------------------

CONST
  -- ATTRIB --           -- STATUS --
  read_only = 000;       deleted=0E5h;
  hidden    = 001;       empty  =000h;
  system    = 002;       used   =0FFh;
  label     = 003;
  subdir    = 004;
  non_arch  = 005;

PROCEDURE get_entry(VAL dir: MSDIR; entry: INTEGER; VAR f: inode): BOOLEAN;
  VAR i,j: INTEGER;                 s,m,h: INTEGER;
      p: str32;                     d,n,y: INTEGER;
BEGIN
  p:=dir[entry];
  IF ORD(p[0])=empty    THEN RETURN FALSE END;
  IF ORD(p[0])=deleted  THEN RETURN FALSE END;
  IF label IN BITSET(p[11]) THEN RETURN FALSE END;

  f.name :="";
  f.mode :={};
  f.state:={};
  f.eof  :=0;
  f.time :=0;
  f.links:=1;
  f.entry:=entry;

  i:=0;
  WHILE (i<8) & (p[i]#' ') DO f.name[i]:=ASCII.SMALL(p[i]); INC(i) END;
  IF p[8]#' ' THEN
    f.name[i]:='.'; INC(i);
    j:=8;
    WHILE (j<11) & (p[j]#' ') DO
      f.name[i]:=ASCII.SMALL(p[j]); INC(i); INC(j)
    END
  END;
  f.name[i]:=0c;

  IF hidden IN BITSET(p[11]) THEN f.state:=f.state+fs.hidden END;
  IF system IN BITSET(p[11]) THEN f.mode :=f.mode +fs.sys    END;
  IF subdir IN BITSET(p[11]) THEN f.mode :=f.mode +_dir      END;
  i:=ORD(p[22]) + ORD(p[23])*256;
  s:=i MOD 32 * 2;  i:=i DIV 32;
  m:=i MOD 64;      i:=i DIV 64;
  h:=i MOD 32;
  i:=ORD(p[24]) + ORD(p[25])*256;
  d:=i MOD 32;      i:=i DIV 32;
  n:=i MOD 16;      i:=i DIV 16;
  y:=i MOD 128 + 1980;
  IF y<1986 THEN d:= 1; n:=1;  y:=1986 END;
  IF y>2017 THEN d:=31; n:=12; y:=2017 END;
  f.time:=tim.pack(y,n,d,h,m,s);
  f.ffat:=ORD(p[26]) + ORD(p[27])*256;
  f.eof :=ORD(p[28]) + ORD(p[29])*256 + ORD(p[30])*(256*256);
  RETURN TRUE
END get_entry;

PROCEDURE readdirectory(d: FILE; VAR dir: MSDIR): INTEGER;
  VAR n,clu,sec,res,e: INTEGER;
BEGIN
  IF d^.dev^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  IF d^.ffat=ROOT THEN
    n:=( d^.dev^.rdent * 32 ) DIV d^.dev^.bps * d^.dev^.bps DIV 32
  ELSE
    sec:=0; clu:=d^.ffat;
    WHILE (1<clu) & (clu<e) DO INC(sec); clu:=FAT(d^.dev,clu) END;
    n:=sec * d^.dev^.spc * d^.dev^.bps DIV 32
  END;

  os.ALLOCATE(os.system,dir^.ADR,n*8);
  IF dir^.ADR=NIL THEN RETURN err.no_memory END;
  dir^.HIGH:=n-1;

  IF d^.ffat=ROOT THEN
    WITH d^.dev^ DO
      n:=n*32;
      res:=fs.read(dev,(nFATs*spFAT + rsec)*bps,dir^.ADR,0,n)
    END
  ELSE
    res:=readfile(d^.dev,d^.ffat,sec,dir^.ADR)
  END;
  n:=0;
  WHILE (n<=HIGH(dir)) & (dir[n][0]#0c) DO INC(n) END;
  FOR e:=n TO HIGH(dir) DO
    FOR sec:=0 TO 31 DO dir[e][sec]:=0c END
  END;
  RETURN res
END readdirectory;

PROCEDURE tomsd(VAR name: ARRAY OF CHAR; VAL ename: ARRAY OF CHAR): INTEGER;
  VAR i,j: INTEGER;
      nm : str32;
BEGIN
  i:=fs.copy_fname(nm,ename);
  IF i#err.ok THEN RETURN i END;
  IF (nm='..') OR (nm='.'0c) THEN name:=nm; RETURN err.ok END;
  i:=0;
  WHILE (i<=7) & (nm[i]#0c) & (nm[i]#'.') DO
    name[i]:=ASCII.SMALL(nm[i]); INC(i)
  END;
  j:=i;
  WHILE (i<=HIGH(nm)) & (nm[i]#0c) & (nm[i]#'.') DO INC(i) END;
  IF (i<=HIGH(nm)) & (nm[i]#0c) THEN
    name[j]:='.'; INC(j); INC(i);
    WHILE (j<=11) & (i<=HIGH(nm)) & (nm[i]#'.') & (nm[i]#0c) DO
      name[j]:=ASCII.SMALL(nm[i]); INC(i); INC(j)
    END
  END;
  name[j]:=0c;
  RETURN err.ok
END tomsd;

PROCEDURE find(d: FILE; VAL name: ARRAY OF CHAR; VAR f: inode): INTEGER;
  VAR dir  : MSDIR;
      entry: INTEGER;
      res  : INTEGER;
BEGIN
  res:=readdirectory(d,dir);
  IF res#err.ok THEN RETURN res END;
  entry:=0;
  res  :=err.no_entry;
  WHILE entry<=HIGH(dir) DO
    IF get_entry(dir,entry,f) THEN
      IF name=f.name THEN
        res  :=err.ok;
        f.dir:=d^.ffat;
        entry:=HIGH(dir)
      END
    END;
    INC(entry)
  END;
  os.DEALLOCATE(os.system,dir^.ADR,(dir^.HIGH+1)*8);
  RETURN res
END find;

PROCEDURE open  (dir: FILE;          VAR f: FILE;
            VAL name: ARRAY OF CHAR; state: BITSET): INTEGER;
  VAR res : INTEGER;
      file: inode;
      nm  : str32;
BEGIN
  IF (dir=NIL) OR (dir^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF dir^.mode*_dir={}               THEN RETURN err.not_dir  END;

  f:=NIL;
  IF (name="..")&(dir^.ffat=ROOT) THEN
    res:=fs.reopen(fs.FILE(dir^.host));
    IF res=err.ok THEN f:=FILE(dir^.host) END;
    RETURN res
  END;

  res:=tomsd(nm,name);
  IF res#err.ok THEN RETURN res END;
  os.wait(Flock);
    os.wait(dir^.flock);
      res:=find(dir,nm,file);
    os.send(dir^.flock);
    IF res=err.ok THEN res:=_open(f,file,dir^.dev) END;
  os.send(Flock);
  RETURN res
END open;

PROCEDURE allocfile(f: FILE; size: INTEGER): INTEGER;
  VAR e: INTEGER;
BEGIN
  IF f^.dev^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  IF size<=0 THEN f^.ffat:=e; RETURN err.ok END;
  IF alloc_cluster(f^.dev,(size+f^.dev^.bpc-1) DIV f^.dev^.bpc,f^.ffat,0)
  THEN
    RETURN err.ok
  ELSE
    RETURN err.no_space
  END
END allocfile;

PROCEDURE create(dir: FILE;          VAR f: FILE;
                size: INTEGER;       state: BITSET): INTEGER;
  VAR res: INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF dir^.mode*_dir={}               THEN RETURN err.not_dir  END;
  os.lock;
    os.ALLOCATE(os.system,f,SIZE(file_desc));
    IF f=NIL THEN os.unlock; RETURN err.no_memory END;

    f^.dev:=dir^.dev; (* see NB.new_desc *)

    IF size<0 THEN size:=0 END;

    res:=allocfile(f,size);

    IF res#err.ok THEN
      os.DEALLOCATE(os.system,f,SIZE(file_desc)); f:=NIL
    ELSE
      f^.host :=fs0._FILE(f);
      f^.eof  :=size;  f^.time:=os.time;   f^.dir  :=-1; f^.mode:={};
      f^.links:=0;     f^.state:={};       f^.entry:=-1;
      os.wait(Flock);
        tie_file(f);
      os.send(Flock)
    END;
  os.unlock;
  RETURN res
END create;

PROCEDURE reopen(f: FILE): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  INC(f^.opens); RETURN err.ok
END reopen;

PROCEDURE flush (f: FILE): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  RETURN err.ok
END flush;

PROCEDURE extendfile(f: FILE; eof: INTEGER): INTEGER;
  VAR pf,nf,e: INTEGER;
BEGIN
  IF f^.dev^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  eof:=(eof+f^.dev^.bpc-1) DIV f^.dev^.bpc;
  pf:=0;
  nf:=f^.ffat;
  WHILE (eof>0)&(nf>1)&(nf<e) DO pf:=nf; nf:=FAT(f^.dev,nf); DEC(eof) END;
  IF eof<=0 THEN RETURN err.ok END;
  IF NOT alloc_cluster(f^.dev,eof,nf,pf) THEN RETURN err.no_space END;
  IF pf=0 THEN f^.ffat:=nf; f^.state:=f^.state+dirty END;
  RETURN err.ok
END extendfile;

PROCEDURE cutfile(f: FILE; eof: INTEGER): INTEGER;
BEGIN
END cutfile;

PROCEDURE extenddir(d,bd: FILE; e: INTEGER): INTEGER;
  VAR dir: DYNARR OF CHAR;
      cl : BOOLEAN;          y,n,dt: INTEGER;
      ent: str32;            h,m,s : INTEGER;
      res: INTEGER;
      efat: INTEGER;
BEGIN
  IF d^.ffat=ROOT THEN RETURN err.no_space END;
  IF d^.dev^.FAT12 THEN efat:=0FFFh ELSE efat:=0FFFFh END;
  cl :=(d^.ffat<=1) OR (d^.ffat>=efat);

  IF cl & (bd=NIL) THEN RETURN err.bad_fsys END;

  res:=extendfile(d,e*32+d^.dev^.bpc);
  IF res#err.ok THEN RETURN res END;

  os.ALLOCATE(os.system,dir^.ADR,d^.dev^.bpc DIV 4);
  IF dir^.ADR=NIL THEN RETURN cutfile(d,e*32) END;

  dir^.HIGH:=d^.dev^.bpc-1;

  low._zero(SYSTEM.ADR(dir),BYTES(dir) DIV 4);

  IF cl THEN

    ent[0]:='.';
    ent[11]:=CHAR({4});
    FOR s:=1 TO 10 DO ent[s]:=' ' END;
    tim.unpack(d^.time,y,n,dt,h,m,s); y:=y-1980;
    s:=s+(m+h*64)*32;
    ent[22]:=CHAR(s);       ent[23]:=CHAR(s DIV 256);
    dt:=dt+(n+y*128)*16;
    ent[24]:=CHAR(dt);      ent[25]:=CHAR(dt DIV 256);
    ent[26]:=CHAR(d^.ffat); ent[27]:=CHAR(d^.ffat DIV 256);
    ent[28]:=CHAR(d^.eof ); ent[29]:=CHAR(d^.eof  DIV 256);
    ent[30]:=CHAR(d^.eof DIV 10000h);  ent[31]:=0c;

    low.cmove(SYSTEM.ADR(dir),0,SYSTEM.ADR(ent),0,32);

    ent[0]:='.'; ent[1]:='.';
    tim.unpack(bd^.time,y,n,dt,h,m,s); y:=y-1980;
    s:=s+(m+h*64)*32;
    ent[22]:=CHAR(s);        ent[23]:=CHAR(s DIV 256);
    dt:=dt+(n+y*128)*16;
    ent[24]:=CHAR(dt);       ent[25]:=CHAR(dt DIV 256);
    ent[26]:=CHAR(bd^.ffat); ent[27]:=CHAR(bd^.ffat DIV 256);
    ent[28]:=CHAR(bd^.eof ); ent[29]:=CHAR(bd^.eof  DIV 256);
    ent[30]:=CHAR(bd^.eof DIV 10000h);  ent[31]:=0c;

    low.cmove(SYSTEM.ADR(dir),32,SYSTEM.ADR(ent),0,32)

  END;

  res:=writeentry(d^.dev,d^.ffat,SYSTEM.ADR(dir),e*32,0,BYTES(dir));

  os.DEALLOCATE(os.system,dir^.ADR,BYTES(dir) DIV 4);

  RETURN res
END extenddir;

PROCEDURE getclusters(p: PORT; VAR clu,len: INTEGER);
  VAR pclu,e: INTEGER;
BEGIN
  IF p^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;
  len:=0; pclu:=clu-1;
  WHILE (clu>1) & (clu<e) & (clu-1=pclu) DO
    pclu:=clu; clu:=FAT(p,clu); INC(len)
  END
END getclusters;

PROCEDURE write(f: FILE; fpos: INTEGER;     buf: SYSTEM.ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;
  VAR res : INTEGER;
      n,l : INTEGER;
      fat : INTEGER;
      pfat: INTEGER;
      e   : INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF f^.mode*_dir#{}             THEN RETURN err.is_dir   END;
  IF len=0 THEN RETURN err.ok       END;
  IF len<0 THEN RETURN err.bad_parm END;

  IF f^.dev^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;

  l   :=len;
  len :=0;

  os.wait(Flock);
  os.wait(f^.flock);
    res:=extendfile(f,fpos+l);
  os.send(Flock);

    IF res#err.ok THEN os.send(f^.flock); RETURN res END;

    fat :=f^.ffat;
    WHILE fpos>=f^.dev^.bpc DO
      IF (fat<=1) OR (fat>=e) THEN
        os.send(f^.flock); RETURN err.no_data
      END;
      DEC(fpos,f^.dev^.bpc);
      fat:=FAT(f^.dev,fat)
    END;
    WHILE l>0 DO
      IF (fat<=1) OR (fat>=e) THEN
        os.send(f^.flock); RETURN err.no_data
      END;

      pfat:=fat;
      getclusters(f^.dev,fat,n);

      n:=n*f^.dev^.bpc-fpos;

      IF n>l THEN n:=l END;
      res:=fs.write(f^.dev^.dev,((pfat-2)*f^.dev^.spc+f^.dev^.offs)*f^.dev^.bps+fpos
                               ,buf,bpos,n);
      IF res#err.ok THEN os.send(f^.flock); RETURN res END;
      fpos:=0;
      INC(bpos,n); INC(len,n); DEC(l,n)
    END;
  os.send(f^.flock);
  RETURN err.ok
END write;

PROCEDURE makedir(VAR d: FILE; where: FILE): INTEGER;
  VAR res,igno: INTEGER;
BEGIN
  IF (where=NIL) OR (where^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF where^.mode*_dir={}                 THEN RETURN err.not_dir  END;

  d:=NIL;

  os.lock;
    res:=create(where,d,0,{});
    IF res#err.ok THEN os.unlock; RETURN err.ok END;

    os.wait(Flock);
      os.wait(d^.flock);

        d^.mode:=d^.mode+_dir;

        res:=extenddir(d,where,0);

      os.send(d^.flock);
    os.send(Flock);

    IF res#err.ok THEN igno:=close(d) END;

  os.unlock;

  RETURN res

END makedir;

PROCEDURE movedir(from: FILE; fname: ARRAY OF CHAR;
                    to: FILE; tname: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;
BEGIN
  RETURN err.inv_op
END movedir;

PROCEDURE readdir(d: FILE; VAR dr: SYSTEM.ADDRESS; VAR sz: INTEGER): INTEGER;
  VAR mdir: MSDIR;
      edir: DYNARR OF fs.dnode;
      res : INTEGER;
      task: os.task_ptr;
      file: inode;
      ent0: INTEGER;
      ent1: INTEGER;
      kind: BITSET;
BEGIN
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF d^.mode*_dir={}             THEN RETURN err.not_dir  END;

  dr:=NIL;
  sz:=0;

  os.wait(d^.flock);
    res:=readdirectory(d,mdir);
  os.send(d^.flock);

  IF res#err.ok THEN RETURN res END;

  task:=os.self();
  IF d^.ffat=ROOT THEN
    ent0:=SIZE(fs.dnode)*(HIGH(mdir)+2)
  ELSE
    ent0:=SIZE(fs.dnode)*(HIGH(mdir)+1)
  END;
  os.ALLOCATE(task^.area,edir^.ADR,ent0);
  res:=err.no_memory;
  IF edir^.ADR#NIL THEN
    res:=err.ok;
    edir^.HIGH:=HIGH(mdir);
    ent0:=0;
    ent1:=0;
    IF d^.ffat=ROOT THEN
      edir^.HIGH:=edir^.HIGH+1;
      edir[0].name:='..';
      edir[0].kind:=fs.d_dir;
      edir[0].inod:=ROOT;
      ent1:=1
    END;
    WHILE ent0<=HIGH(mdir) DO
      IF get_entry(mdir,ent0,file) & (file.name#'.'0c) THEN
        edir[ent1].name:=file.name;
        edir[ent1].inod:=file.ffat;
        kind:={};
        IF file.state*fs.hidden#{} THEN kind:=kind+fs.d_hidden END;
        IF file.mode *fs.sys   #{} THEN kind:=kind+fs.d_sys    END;
        IF file.mode *_dir     #{} THEN kind:=kind+fs.d_dir
        ELSE                            kind:=kind+fs.d_file
        END;
        edir[ent1].kind:=kind
      ELSE
        edir[ent1].name:='';
        edir[ent1].inod:=-1;
        edir[ent1].kind:={}
      END;
      INC(ent0); INC(ent1)
    END;
    dr:=edir^.ADR;
    sz:=edir^.HIGH+1
  END;
  os.DEALLOCATE(os.system,mdir^.ADR,(HIGH(mdir)+1)*8);
  RETURN res
END readdir;

PROCEDURE read (f: FILE; fpos: INTEGER;     buf: SYSTEM.ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;
  VAR res : INTEGER;
      n,l : INTEGER;
      fat : INTEGER;
      pfat: INTEGER;
      e   : INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF f^.mode*_dir#{}             THEN RETURN err.is_dir   END;
  IF f^.dev^.FAT12 THEN e:=0FFFh ELSE e:=0FFFFh END;

  os.wait(f^.flock);
    l   :=len;
    len :=0;
    fat :=f^.ffat;
    WHILE fpos>=f^.dev^.bpc DO
      IF (fat<=1) OR (fat>=e) THEN os.send(f^.flock); RETURN err.no_data END;
      DEC(fpos,f^.dev^.bpc);
      fat:=FAT(f^.dev,fat)
    END;
    WHILE l>0 DO
      IF (fat<=1) OR (fat>=e) THEN os.send(f^.flock); RETURN err.no_data END;

      pfat:=fat;
      getclusters(f^.dev,fat,n);

      n:=n*f^.dev^.bpc-fpos;

      IF n>l THEN n:=l END;
      res:=fs.read(f^.dev^.dev,((pfat-2)*f^.dev^.spc+f^.dev^.offs)*f^.dev^.bps+fpos
                              ,buf,bpos,n);
      IF res#err.ok THEN os.send(f^.flock); RETURN res END;
      fpos:=0;
      INC(bpos,n); INC(len,n); DEC(l,n)
    END;
  os.send(f^.flock);
  RETURN err.ok
END read;

PROCEDURE link  (d,file: FILE; VAL nm: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;

  VAR dir  : MSDIR;
      entry: INTEGER;
      e    : INTEGER;
      res  : INTEGER;
      igno : INTEGER;
      f    : inode;
      name : str32;
      f0   : FILE;

  PROCEDURE u;
  BEGIN
    IF dir^.ADR#NIL THEN
      os.DEALLOCATE(os.system,dir^.ADR,(dir^.HIGH+1)*8)
    END;
    os.send(d^.flock); os.send(Flock); os.unlock
  END u;

BEGIN
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF d^.mode*_dir={}             THEN RETURN err.not_dir  END;

  res:=tomsd(name,nm);
  IF res#err.ok THEN RETURN res END;

  IF (name='..') OR (name='.'0c) THEN RETURN err.is_dir END;

  f0:=NIL; dir^.ADR:=NIL;

  os.lock;
  os.wait(Flock);
  os.wait(d^.flock);

    IF file^.links>0 THEN u; RETURN err.xcross END;
    IF file^.links<0 THEN file^.links:=0 END;

    res:=readdirectory(d,dir);
    IF res#err.ok THEN u; RETURN res END;
    entry:=0; e:=-1;
    WHILE entry<=HIGH(dir) DO
      IF get_entry(dir,entry,f) THEN
        IF name=f.name THEN
          f.dir:=d^.ffat;
          e:=entry; entry:=HIGH(dir)
        END
      END;
      INC(entry)
    END;
    IF e=-1 THEN
      e:=0;
      WHILE (e<=HIGH(dir))&(ORD(dir[e][0])#empty)&(ORD(dir[e][0])#deleted) DO
        INC(e)
      END;
      IF e>HIGH(dir) THEN
        IF d^.ffat=ROOT THEN u; RETURN err.no_space END;

        e:=HIGH(dir)+1;
        os.DEALLOCATE(os.system,dir^.ADR,(dir^.HIGH+1)*8);

        res:=extenddir(d,NIL,e)

      END
    ELSE
      res:=_open(f0,f,d^.dev)
    END;

    os.DEALLOCATE(os.system,dir^.ADR,(dir^.HIGH+1)*8);

    IF res=err.ok THEN
      IF f0#NIL THEN DEC(f0^.links) END;
      file^.dir:=d^.ffat; file^.entry:=e; file^.state:=file^.state+dirty;
      IF hid THEN
        file^.state:=file^.state+fs.hidden
      ELSE
        file^.state:=file^.state-fs.hidden
      END;
      file^.links:=1;
      res:=updateentry(file,name);
    END;
  os.send(d^.flock);
  os.send(Flock);

  IF f0#NIL THEN igno:=close(f0) END;

  os.unlock;

  RETURN res
END link;

PROCEDURE empty?(d: FILE): INTEGER;
  VAR dir  : MSDIR;
      entry: INTEGER;
      f    : inode;
      res  : INTEGER;
BEGIN
  res:=readdirectory(d,dir);
  IF res#err.ok THEN RETURN res END;
  entry:=0;
  res  :=err.ok;
  WHILE entry<=HIGH(dir) DO
    IF get_entry(dir,entry,f) THEN
      IF (f.name#'..') & (f.name#'.'0c) THEN
        res  :=err.no_entry;
        entry:=HIGH(dir)
      END
    END;
    INC(entry)
  END;
  os.DEALLOCATE(os.system,dir^.ADR,(dir^.HIGH+1)*8);
  RETURN res
END empty?;

PROCEDURE unlink(d: FILE; VAL nm: ARRAY OF CHAR): INTEGER;
  VAR name: str32;
      file: FILE;
      ch  : CHAR;
      clo : INTEGER;
      res : INTEGER;
BEGIN
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF d^.mode*_dir={}                 THEN RETURN err.not_dir  END;

  res:=tomsd(name,nm);
  IF res#err.ok THEN RETURN res END;

  os.lock;
  res:=open(d,file,name,{});
  IF res#err.ok THEN os.unlock; RETURN res END;
  os.wait(Flock);
  os.wait(d^.flock);
    IF file^.links>0 THEN
      IF file^.mode*_dir#{} THEN
        res:=empty?(file)
      END;
      IF res=err.ok THEN
        ch:=CHAR(deleted);
        res:=writeentry(file^.dev,file^.dir,SYSTEM.ADR(ch),file^.entry*32,0,1);
        IF res=err.ok THEN
          file^.links:=0; file^.dir:=-1; file^.entry:=-1
        END
      END
    END;
  os.send(d^.flock);
  os.send(Flock);
  clo:=close(file);
  os.unlock;
  IF res=err.ok THEN res:=clo END;
  RETURN res
END unlink;

PROCEDURE du(dir: FILE; VAR f,u: INTEGER): INTEGER;
BEGIN
  IF (dir=NIL) OR (dir^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF dir^.mode*_dir={}               THEN RETURN err.not_dir  END;

  f:=free(dir^.dev);
  u:=(dir^.dev^.mclu+1)*dir^.dev^.bpc - f;

  RETURN err.ok
END du;

PROCEDURE cut(f: FILE; size: INTEGER): INTEGER;
BEGIN
  RETURN err.ok
END cut;

PROCEDURE extend(f: FILE; size: INTEGER): INTEGER;
  VAR res: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF f^.mode*_dir#{}             THEN RETURN err.is_dir   END;
  IF size<0                      THEN RETURN err.bad_parm END;

  os.wait(Flock);
    os.wait(f^.flock);
      res:=extendfile(f,size);
    os.send(f^.flock);
  os.send(Flock);

  RETURN res
END extend;

PROCEDURE makefs(disk: fs0._FILE;
                label: ARRAY OF CHAR;
                 bads: ARRAY OF INTEGER): INTEGER;

CONST

  SPC   = 2;
  NFATS = 2;
  RDENT = 112;

  VAR r: req.REQUEST;
      b: DYNARR OF CHAR;

  PROCEDURE u;
    VAR ignore: INTEGER;
  BEGIN
    IF b^.ADR#NIL THEN os.DEALLOCATE(os.system,b^.ADR,(BYTES(b)+3) DIV 4) END;
    ignore:=fs0.umountdev(disk);
    os.unlock
  END u;

  PROCEDURE byte(i,n: INTEGER);
  BEGIN b[i]:=CHAR(n) END byte;

  PROCEDURE word(i,n: INTEGER);
  BEGIN b[i]:=CHAR(n); b[i+1]:=CHAR(n DIV 256) END word;

  VAR res    : INTEGER;
      mdesc  : BITSET;
      spfat  : INTEGER;
      rsec   : INTEGER;
      rdsize : INTEGER;
      n,i,len: INTEGER;

BEGIN

  NEW(b);

  os.lock;
    res:=fs0.mountdev(disk);
    IF res#err.ok THEN os.unlock; RETURN res END;

    r.op:=req.GET_SPEC;
    res:=fs.doio(fs.FILE(disk),r);
    IF res#err.ok THEN u; RETURN res END;
    WITH r DO
      IF   (ssc#9) OR (secsize#512)
        OR ((cyls#40) & (cyls#80))
        OR ((heads#1) & (heads#2))
        OR (ressec#0)
        OR (maxsec<minsec)
        OR (cyls*heads*(maxsec-minsec+1)#dsecs)
        OR (dsecs>0FFFFh)
      THEN
        res:=err.bad_parm
      END
    END;
    IF res#err.ok THEN u; RETURN res END;

    os.ALLOCATE(os.system,b^.ADR,(r.secsize+3) DIV 4);
    IF b^.ADR=NIL THEN u; RETURN err.no_memory END;
    b^.HIGH:=r.secsize-1;

    low._zero(SYSTEM.ADR(b),(BYTES(b)+3) DIV 4);

    spfat:=((((r.dsecs+SPC-1) DIV SPC + 2)*3 + 1 ) DIV 2  + r.secsize-1) DIV r.secsize;

    IF spfat<=0 THEN u; RETURN err.bad_parm END;

    mdesc:={3..7};
    mdesc:=mdesc+{2}; (* removable *)
    IF (r.maxsec-r.minsec+1)=8 THEN mdesc:=mdesc+{1} END;
    IF r.heads=2               THEN mdesc:=mdesc+{0} END;

    rsec :=1;

    byte(00h,0E9h);                (* jump near for boot code *)
    word(0Bh,r.secsize);           (* bps   *)
    byte(0Dh,SPC);                 (* spc   *)
    word(0Eh,rsec);                (* rsec  *)
    byte(10h,NFATS);               (* nFATs *)
    word(11h,RDENT);               (* rdent *)
    word(13h,r.dsecs);             (* tsec  *)
    byte(15h,INTEGER(mdesc));      (* mdesc *)
    word(16h,spfat);               (* spFAT *)
    word(18h,r.maxsec-r.minsec+1); (* spTRK *)
    word(1Ah,r.heads);             (* heads *)
    word(1Ch,0);                   (* hsec  *)


    len:=r.secsize;
    res:=fs.write(fs.FILE(disk),0,SYSTEM.ADR(b),0,len);
    IF res#err.ok THEN u; RETURN res END;

    low._zero(SYSTEM.ADR(b),(BYTES(b)+3) DIV 4);

    i:=0;
    WHILE i<NFATS DO
      byte(0,INTEGER(mdesc));
      n:=0;
      WHILE n<spfat DO
        len:=r.secsize;
        res:=fs.write(fs.FILE(disk),(rsec+i*spfat+n)*r.secsize,SYSTEM.ADR(b),0,len);
        IF res#err.ok THEN u; RETURN res END;
        low._zero(SYSTEM.ADR(b),(BYTES(b)+3) DIV 4);
        INC(n)
      END;
      INC(i)
    END;

    rdsize:=RDENT*32 DIV r.secsize;

    n:=0;
    WHILE n<rdsize DO
      len:=r.secsize;
      res:=fs.write(fs.FILE(disk),(rsec+NFATS*spfat+n)*r.secsize,SYSTEM.ADR(b),0,len);
      IF res#err.ok THEN u; RETURN res END;
      INC(n)
    END;
  u;
  RETURN res
END makefs;

PROCEDURE makenode(d: FILE; VAR f: FILE; ref: ARRAY OF CHAR): INTEGER;
BEGIN
  RETURN err.inv_op
END makenode;

PROCEDURE doio(f: FILE; VAR r: req.REQUEST): INTEGER;
BEGIN
  RETURN err.inv_op
END doio;

PROCEDURE mount  (dir: FILE; name: ARRAY OF CHAR;   dev: FILE;
                           fsname: ARRAY OF CHAR;
                             info: ARRAY OF CHAR;
                          VAR lab: ARRAY OF CHAR;    ro: BOOLEAN): INTEGER;
BEGIN
  RETURN err.unsuitable
END mount;

---------------------  R E A D    B O O T  ---------------------
                     ----------------------

PROCEDURE unmountdevice(VAR p: PORT): INTEGER;
  VAR res,ignore: INTEGER;
BEGIN
  res:=saveFATs(p);
  ignore:=fs.doio(p^.dev,p^.savspec);
  res:=fs0.umountdev(fs0._FILE(p^.dev));
  IF res#err.ok THEN RETURN res END;
  IF p^.FATsz#0 THEN
    os.DEALLOCATE(os.system,p^.FATs,p^.FATsz)
  END;
  os.DEALLOCATE(os.system,p,SIZE(p^));
  RETURN err.ok
END unmountdevice;

PROCEDURE mountdevice(VAR p: PORT; dev: fs0._FILE): INTEGER;

  VAR boot: ARRAY [0..511] OF CHAR;

  PROCEDURE u;
    VAR ignore: INTEGER;
  BEGIN
    IF p=NIL THEN RETURN END;
    IF p^.dev#fs.null THEN
      ignore:=fs.doio(p^.dev,p^.savspec);
      ignore:=fs0.umountdev(fs0._FILE(p^.dev))
    END;
    IF p^.FATsz#0 THEN
      os.DEALLOCATE(os.system,p^.FATs,p^.FATsz)
    END;
    os.DEALLOCATE(os.system,p,SIZE(p^))
  END u;

  PROCEDURE byte(i: INTEGER): INTEGER;
  BEGIN RETURN INTEGER(boot[i]) END byte;

  PROCEDURE word(i: INTEGER): INTEGER;
  BEGIN  RETURN INTEGER(boot[i])+INTEGER(boot[i+1])*256 END word;

  VAR r  : req.REQUEST;
      len: INTEGER;
      res: INTEGER;
     flop: BOOLEAN;
BEGIN

  os.ALLOCATE(os.system,p,SIZE(p^));
  IF p=NIL THEN RETURN err.no_memory END;
  p^.FATs:=NIL;
  p^.FATsz:=0;
  p^.savspec.op:=req.NOP;

  p^.dev:=fs.null;
  p^.fd :=FALSE;

  res:=fs0.mountdev(dev);
  IF res#err.ok THEN u; RETURN res END;

  p^.dev  :=fs.FILE(dev);
  p^.opens:=0;

  r.op:=req.GET_SPEC;
  res:=fs.doio(p^.dev,r);
  IF res#err.ok THEN u; RETURN res END;

  p^.savspec:=r;
  p^.savspec.op:=req.SET_SPEC;

  flop:=FALSE;
  IF ( r.dmode * req.floppy # {} ) THEN
    flop:=TRUE;
    r.op:=req.SET_SPEC;
    WITH r DO
      dsecs  :=360*2;
      ssc    :=9;
      secsize:=512;
      cyls   :=40;
      heads  :=2;
      minsec :=1;
      maxsec :=9;
      ressec :=0;
    END;
    res:=fs.doio(p^.dev,r);
    IF res#err.ok THEN u; RETURN res END;
  END;

  len:=512;
  res:=fs.read(p^.dev,0,SYSTEM.ADR(boot),0,len);
  IF res#err.ok THEN u; RETURN res END;

  IF ( ORD(boot[0]) # 0E9h ) & ( ORD(boot[0]) # 0EBh )
  THEN u; RETURN err.bad_fsys
  END;

  p^.bps  := word(0Bh);
  p^.spc  := byte(0Dh);
  p^.rsec := word(0Eh);
  p^.nFATs:= byte(10h);
  p^.rdent:= word(11h);
  p^.tsec := word(13h);
  p^.mdesc:= byte(15h);
  p^.spFAT:= word(16h);
  p^.spTRK:= word(18h);
  p^.heads:= word(1Ah);
  p^.hsec := word(1Ch);

  IF p^.tsec=0 THEN
    p^.FAT12:=FALSE;
    p^.hsec:=p^.hsec+word(1Eh)*10000h;
    p^.tsec:=word(20h)+word(22h)*10000h
  ELSE
    p^.FAT12:=TRUE
  END;

  ---- производные значения ----

  IF p^.bps#512       THEN u; RETURN err.bad_fsys END;
  IF p^.spTRK<=0      THEN u; RETURN err.bad_fsys END;
  IF p^.spFAT<=0      THEN u; RETURN err.bad_fsys END;
  IF p^.nFATs<=0      THEN u; RETURN err.bad_fsys END;
  IF p^.heads<=0      THEN u; RETURN err.bad_fsys END;
  IF p^.nFATs>8       THEN u; RETURN err.bad_fsys END;
  IF p^.spFAT*p^.nFATs>p^.tsec THEN u; RETURN err.bad_fsys END;

  p^.bpc  :=p^.bps * p^.spc;
  p^.offs :=p^.rsec + p^.spFAT * p^.nFATs
                    + p^.rdent * 32 DIV p^.bps;
  p^.mclu :=(p^.tsec - p^.offs) DIV p^.spc + 1;

  r.cyls:=p^.tsec DIV (p^.spTRK*p^.heads);
  IF r.cyls<=0  THEN u; RETURN err.bad_fsys END;

  IF flop THEN
    r.minsec :=1;   r.maxsec:=p^.spTRK;
    r.secsize:=512; r.ssc:=9;
    r.heads  :=p^.heads;
    r.ressec :=0;
    r.dsecs  :=p^.tsec;

    r.op:=req.SET_SPEC;
    res:=fs.doio(p^.dev,r);
    IF res#err.ok THEN u; RETURN res END
  END;

  res:=loadFATs(p);

  IF res#err.ok THEN u END;

  RETURN res
END mountdevice;

PROCEDURE mount0(VAR root: FILE; dev,host: fs0._FILE;
                 VAL info: ARRAY OF CHAR;
                 VAR lab : ARRAY OF CHAR; ro: BOOLEAN): INTEGER;
  VAR res : INTEGER;
      igno: INTEGER;
      r   : FILE;
      p   : PORT;
      f   : inode;
BEGIN
  root:=NIL;

  os.wait(Flock);

    res:=mountdevice(p,dev);
    IF res#err.ok THEN os.send(Flock); RETURN res END;

    f.ffat :=ROOT;
    f.dir  :=-1;
    f.entry:=-1;
    f.mode :=_dir;
    f.state:={};
    f.eof  :=0;
    f.time :=0;
    f.links:=1;

    res:=_open(r,f,p);
    IF res#err.ok THEN igno:=unmountdevice(p); os.send(Flock); RETURN res END;

    r^.host:=host;

  os.send(Flock);

  root:=r;
  str.copy(lab,"");

  RETURN err.ok
END mount0;

PROCEDURE unmount(dir: FILE; name: ARRAY OF CHAR; flush: INTEGER): INTEGER;
BEGIN
  RETURN err.unsuitable
END unmount;

PROCEDURE unmount0(root: FILE; flush: INTEGER): INTEGER;
  VAR x  : FILE;
      res: INTEGER;
BEGIN
  IF (root=NIL) OR (root^.magic#MAGIC) THEN RETURN err.bad_parm END;
  IF root^.mode*_dir={}                THEN RETURN err.not_dir  END;

  os.wait(Flock);
    os.wait(root^.flock);
    res:=err.ok;
    IF (root^.opens>1) OR (root^.dev^.opens>1) THEN
      res:=err.busy; os.send(root^.flock)
    ELSIF (root^.opens=1) & (root^.dev^.opens=1) THEN
      res:=unmountdevice(root^.dev);
      IF res=err.ok THEN
        destroy(root); os.unlock (* because f^.flock destroed *)
      ELSE
        os.send(root^.flock)
      END
    ELSE
      res:=err.busy; os.send(root^.flock)
    END;
  os.send(Flock);
  RETURN res
END unmount0;

PROCEDURE openfh(VAR f: FILE; VAR fh: fs.FHANDLE): INTEGER;
BEGIN
  RETURN err.inv_op
END openfh;

PROCEDURE initmsdos;
  VAR fsd: fs0.fsdesc0;
BEGIN

   msdesc_ptr:=SYSTEM.ADR(msdesc);

   msdesc.getattr   := fs0.GETATTR     (getattr);
   msdesc.setattr   := fs0.SETATTR     (setattr);
   msdesc.isdir     := fs0.ISDIR       (isdir);
   msdesc.state     := fs0.STATE       (state);
   msdesc.usrlock   := fs0.USRLOCK     (usrlock);
   msdesc.usrunlock := fs0.USRUNLOCK   (usrunlock);
   msdesc.open      := fs0.OPEN        (open);
   msdesc.create    := fs0.CREATE      (create);
   msdesc.close     := fs0.CLOSE       (close);
   msdesc.reopen    := fs0.REOPEN      (reopen);
   msdesc.flush     := fs0.FLUSH       (flush);
   msdesc.makedir   := fs0.MAKEDIR     (makedir);
   msdesc.movedir   := fs0.MOVEDIR     (movedir);
   msdesc.readdir   := fs0.READDIR     (readdir);
   msdesc.read      := fs0.READ        (read);
   msdesc.write     := fs0.WRITE       (write);
   msdesc.doio      := fs0.fDOIO       (doio);
   msdesc.link      := fs0.LINK        (link);
   msdesc.unlink    := fs0.UNLINK      (unlink);
   msdesc.du        := fs0.DU          (du);
   msdesc.cut       := fs0.CUT         (cut);
   msdesc.extend    := fs0.EXTEND      (extend);
   msdesc.makenode  := fs0.MAKENODE    (makenode);
   msdesc.getfh     := fs0.GETFH       (getfh);
   msdesc.mount     := fs0.MOUNT       (mount);
   msdesc.unmount   := fs0.UNMOUNT     (unmount);
   msdesc.unmount0  := fs0.UNMOUNT0    (unmount0);

   fsd.fsid   := msdesc_ptr;
   fsd.openfh := fs0.OPENFH(openfh);
   fsd.makefs := fs0.MAKEFS(makefs);
   fsd.mount0 := fs0.MOUNT0(mount0);
   fsd.halt   := SYSTEM.ADR(mshalt);

   os.ini_signal(mshalt,{},0);

   IF fs0.define_filesys("MSDOS",fsd)#err.ok THEN quit(1) END

END initmsdos;

BEGIN

  MAGIC:=_MAGIC;
  Flist:=NIL;
  os.ini_signal(Flock, os.guard,1);

  initmsdos;

  env.become_ipr;
  os.suspend(os.active(),-1)

END MSDOS.
