IMPLEMENTATION MODULE osFiles[1]; (* Leo 02-Jan-86. (c) KRONOS *)
                                  (* Leo 24-Aug-89. (c) KRONOS *)
                                  (* Leo 26-Feb-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT   os: osKernel;

IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  req: defRequest;

IMPORT  low: lowLevel;
IMPORT  str: Strings;
IMPORT  fmt: Formats;


---------------------  GLOBAL DEFINITIONS  ---------------------
                     ----------------------

CONST   ok = err.ok;
        KB = 256;
  EXTERNAL = nocash+wait_wr+nodelay+fsys;
  MAXFILE  = 32*1024*1024;

TYPE   set = BITSET;
   ADDRESS = SYSTEM.ADDRESS;
   REQUEST = req.REQUEST;
     str8  = ARRAY [0.. 7] OF CHAR;
     str32 = ARRAY [0..31] OF CHAR;
   set_ptr = POINTER TO ARRAY [0..0FFFFh] OF BITSET;
   ref_ptr = POINTER TO ARRAY [0..1023] OF INTEGER;
   vol_ptr = POINTER TO str8;
 dnode_ptr = POINTER TO dnode;
 DIRTYMARK = ARRAY [0..3] OF BITSET; (* DON'T change HIGH, see: update_dirty *)

  PORT = POINTER TO port;
  port =
  RECORD
    magic: INTEGER;
    next : PORT;
    task : os.task_ptr;          (* driver task  *)
    name : str8;                 (* drive name   *)
    drn  : INTEGER;              (* drive number *)
    doio : DOIO;
    lock : os.signal_ptr;
    mode : BITSET;
    state: BITSET;
    opens: INTEGER;              (* # opened files     *)

    (* next only for mode=disk *)
    secs : INTEGER;              (* # secs on volume   *)
    ssc  : INTEGER;              (* lg2(secsz)         *)
    secsz: INTEGER;              (* bytes              *)
    errsc: INTEGER;              (* last error sector  *)
    error: INTEGER;              (* last error code    *)

    (* next only for mounted disk *)
    du   : INTEGER;              (* disk usage         *)
    super: ADDRESS;              (* memory for sb      *)
    supsz: INTEGER;              (* words              *)
    slock: os.signal_rec;        (* super block lock   *)
    vol  : vol_ptr;              (* volume name        *)

    iset : set_ptr; (* "inodes bitset" *)
    ihigh: INTEGER;
    sb_i : INTEGER;
    bset : set_ptr; (* "blocks bitset" *)
    bhigh: INTEGER;
    sb_b : INTEGER;

    wssc : INTEGER; (* words sector size code *)
    inoL : INTEGER; (* low  inode block *)
    inoH : INTEGER; (* high inode block *)
    res  : INTEGER;
    updt : BOOLEAN;
    dirty: DIRTYMARK
  END;

CONST -- disk modes
  dir     = {1};
  long    = {2};
  escape  = {3};

CONST -- open file modes
  dirty    =  {0};
  alias    =  {1};
  bad      =  {2};

TYPE
  FILE = POINTER TO file_desc;
  REF  = ARRAY [0..7] OF INTEGER;
  file_desc =                       (* 28 words *)
  RECORD
    ---  inode  ---
    ref  : REF;
    mode : BITSET;
    links: INTEGER;
    eof  : INTEGER;
    ctime: INTEGER;
    wtime: INTEGER;
    pro  : INTEGER;
    res1 : INTEGER;
    res2 : INTEGER;
    --- internal ---
    magic: INTEGER;
    fwd  : FILE;
    bck  : FILE;
    dev  : PORT;
    flock: os.signal_rec; (* file system lock *)
    ulock: os.mutex_rec;  (* user system lock *)
    opens: INTEGER;
    inode: INTEGER;
    state: BITSET;
    host : FILE;  (* ".." for mounted fsys *)
  END;

CONST (* state *)
  _dead = {0};
  _host = {1};
  _wpro = {2};
  _sync = {3};

VAR ROOT: str8;
    SYNC: BOOLEAN;
  update: os.signal_rec;
  PMAGIC: INTEGER;

---------------------  LOW LEVEL SUPPORT  ----------------------
                     ---------------------

PROCEDURE dpw2(x,n: INTEGER): INTEGER;  CODE cod.shr END dpw2;
PROCEDURE mpw2(x,n: INTEGER): INTEGER;  CODE cod.shl END mpw2;

PROCEDURE ei(m: BITSET); CODE cod.setm END ei;
PROCEDURE di(VAR m: BITSET);
CODE cod.getm cod.copt cod.li3 cod.bic cod.setm cod.ssw0 END di;

PROCEDURE TR(VAR d: BITSET): BITSET; CODE cod.tr END TR;

---------------------------- DEVICES ---------------------------
                            ---------

VAR Plock: os.signal_rec;
    ports: PORT;


PROCEDURE dummy_io(VAR r: req.REQUEST);
BEGIN r.res:=err.undef END dummy_io;

PROCEDURE _get_spec(dev: PORT): INTEGER;
  VAR r: req.REQUEST;
BEGIN
  IF dev^.mode=spec THEN RETURN ok END;
  r.op :=req.GET_SPEC;
  r.drn:=dev^.drn;       r.res:=ok;
  os.wait(dev^.lock^);
    dev^.doio(r);
  os.send(dev^.lock^);
  IF (r.res#ok) OR (dev^.mode#disk) THEN RETURN r.res END;
  WITH dev^ DO
    secs:=r.dsecs;      ssc:=r.ssc;     secsz:=(1<<r.ssc);
    IF (secsz<128) OR (secsz>4096) THEN RETURN err.bad_parm END;
    IF r.dmode*req.sync#{} THEN state:=state+_sync ELSE state:=state-_sync END;
    IF r.dmode*req.wpro#{} THEN state:=state+_wpro ELSE state:=state-_wpro END
  END;
  RETURN ok
END _get_spec;

PROCEDURE final_driver; FORWARD;

PROCEDURE define_driver(name,host: ARRAY OF CHAR; drn: INTEGER;
                             mode: BITSET;       doio: DOIO): INTEGER;
  VAR d,h: PORT; r: INTEGER;

  PROCEDURE undo;
  BEGIN
    IF d^.state*_host#{} THEN
      os.DEALLOCATE(os.system,d^.lock,SIZE(d^.lock^))
    END;
    os.DEALLOCATE(os.system,d,SIZE(d^))
  END undo;

BEGIN
  IF name="" THEN RETURN err.bad_name END;
  IF (mode#disk) & (mode#tty) & (mode#spec) THEN RETURN err.bad_parm END;
  os.wait(Plock);
    d:=ports;
    WHILE (d#NIL) & (d^.name#name) DO d:=d^.next END;
    IF d#NIL THEN os.send(Plock); RETURN err.duplicate END;
    os.ALLOCATE(os.system,d,SIZE(d^));
    IF d=NIL THEN os.send(Plock); RETURN err.no_memory END;
    d^.state:={}; d^.lock:=NIL;
    IF (host="") OR (host=name) THEN
      d^.state:=d^.state+_host;
      os.ALLOCATE(os.system,d^.lock,SIZE(d^.lock^));
      IF d^.lock=NIL THEN undo; os.send(Plock); RETURN err.no_memory END;
      os.ini_signal(d^.lock^,os.guard,1)
    ELSE
      h:=ports;
      WHILE (h#NIL) & (h^.name#host) DO h:=h^.next END;
      IF h=NIL THEN undo; os.send(Plock); RETURN err.no_entry END;
      d^.lock:=h^.lock
    END;
    d^.task:=os.self();
    r:=os.final(d^.task,final_driver);
    IF r#ok THEN undo; os.send(Plock); RETURN r END;

    os.ini_signal(d^.slock,os.guard,1);
    d^.magic:=PMAGIC;
    d^.mode:=mode;      d^.opens:=0;
    d^.drn :=drn;       str.copy(d^.name,name);
    d^.doio:=doio;      d^.next :=ports;

    ports:=d;

    d^.errsc:=-1;       d^.error:=ok;
    d^.secs :=0;        d^.ssc  :=0;
    d^.secsz:=0;        d^.wssc :=0;
    d^.super:=NIL;      d^.du   :=0;
    d^.vol  :=NIL;      d^.supsz:=0;
    d^.iset :=NIL;      d^.ihigh:=-1;
    d^.bset :=NIL;      d^.bhigh:=-1;
    d^.inoL :=-1;       d^.res  :=ok;
    d^.inoH :=-2;       d^.updt :=FALSE;
    low.zero(d^.dirty);

  os.send(Plock);
  RETURN ok
END define_driver;

PROCEDURE open_driver(VAL name: ARRAY OF CHAR; VAR d: PORT): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF name="" THEN RETURN err.bad_name END;
  os.wait(Plock);
    d:=ports;
    WHILE (d#NIL) & (d^.name#name) DO d:=d^.next END;
    IF d#NIL THEN INC(d^.opens) END;
  os.send(Plock);
  IF d=NIL      THEN RETURN err.no_entry END;
  IF d^.opens#1 THEN RETURN ok           END;
  r:=_get_spec(d);
  IF r#ok THEN DEC(d^.opens); d:=NIL END;
  RETURN r
END open_driver;

PROCEDURE close_driver(VAR d: PORT);
  VAR l,p: PORT;
BEGIN
  os.wait(Plock);
    DEC(d^.opens);
    IF (d^.state*_dead={}) OR (d^.opens>0) THEN os.send(Plock); RETURN END;
    l:=ports; p:=NIL;
    WHILE (l#d) & (l#NIL) DO p:=l; l:=l^.next END;
    IF l#d   THEN os.send(Plock); RETURN END;
    IF p=NIL THEN ports:=d^.next  ELSE p^.next:=d^.next  END;
    IF d^.state*_host#{} THEN
      os.DEALLOCATE(os.system,d^.lock,SIZE(d^.lock^))
    END;
    d^.magic:=0;
    os.DEALLOCATE(os.system,d^.super,d^.supsz);
    os.DEALLOCATE(os.system,d,SIZE(d^));
  os.send(Plock)
END close_driver;

PROCEDURE remove_driver(VAL name: ARRAY OF CHAR): INTEGER;
  VAR r: INTEGER;
      d: PORT;
BEGIN
  IF name="" THEN RETURN err.bad_name END;
  os.wait(Plock);
    d:=ports;
    WHILE (d#NIL) & (d^.name#name) DO d:=d^.next END;
    IF d=NIL THEN os.send(Plock); RETURN err.no_entry END;
    INC(d^.opens); (* open *)     ------
    d^.doio:=dummy_io;  d^.state:=d^.state+_dead;
    d^.name:="";        (* driver can't be open after this *)
  os.send(Plock);
  close_driver(d);
  RETURN ok
END remove_driver;

PROCEDURE final_driver;
  VAR d: PORT;
BEGIN
  LOOP
    os.wait(Plock);
      d:=ports;
      WHILE (d#NIL) & (d^.task#os.self()) DO d:=d^.next END;
      IF d=NIL THEN os.send(Plock); RETURN END;
      INC(d^.opens); (* open *)     ------
      d^.doio:=dummy_io;  d^.state:=d^.state+_dead;
      d^.name:="";        (* driver can't be open after this *)
    os.send(Plock);
    close_driver(d)
  END
END final_driver;

----------------------------  CASH  ----------------------------
                            --------

TYPE
  cash_item  = POINTER TO cash_entry;
  cash_entry =
  RECORD
    dev   : PORT;
    blk   : INTEGER;
    size  : INTEGER;  (* words *)
    buf   : ADDRESS;
    use   : os.mutex_rec;
    t_fwd : cash_item;
    t_bck : cash_item;
    i_fwd : cash_item;
    i_bck : cash_item;
    dirty : BITSET;    (* dirty sectors in block *)
    u_bck : cash_item; (* update dirty ring      *)
    u_fwd : cash_item;
    activ : BOOLEAN;
    writen: os.signal_rec;
  END;

VAR    c_ring: cash_item;
       u_ring: cash_item;
      c_index: ARRAY [0..63] OF cash_item;
     c_search: os.mutex_rec;
   exist_free: os.signal_rec;
   cash_area : os.AREA;

PROCEDURE syslog_write(x: SYSTEM.WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR r: req.REQUEST;
   name: str8;
 syslog: PORT;
BEGIN
  syslog:=ports;
  WHILE (syslog#NIL) & (syslog^.magic=PMAGIC) DO
    name:=syslog^.name; name[6]:=0c;
    IF (name="SYSLOG") & (syslog^.mode=tty) THEN
      r.op :=req.WRITE;
      r.pos:=pos;   r.buf:=SYSTEM.ADR(s);
      r.len:=len;   r.res:=ok;
      r.ofs:=0;     r.drn:=syslog^.drn;
      os.wait(syslog^.lock^);
        syslog^.doio(r);
      os.send(syslog^.lock^)
    END;
    syslog:=syslog^.next
  END
END syslog_write;

PROCEDURE syslogprint(VAL s: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
BEGIN
  fmt.format(0,syslog_write,s,args)
END syslogprint;

PROCEDURE fix_write_error(dev: PORT; block,res: INTEGER);
BEGIN
  syslogprint('\nDISK WRITE ERROR "%s"[%d] block=%05d %08h [sec%05d err%08h]\n'
              ,dev^.name,dev^.drn,block,res,dev^.errsc,dev^.error);
  dev^.error:=ok
END fix_write_error;

PROCEDURE update_dirty_block(c: cash_item): INTEGER; FORWARD;
PROCEDURE update_super(dev: PORT): INTEGER; FORWARD;

PROCEDURE jump(VAR from,to: os.mutex_rec);
  VAR m: BITSET;
BEGIN
  di(m);
    os.release(from); os.acquire(to);
  ei(m);
END jump;

PROCEDURE cash_update(c: cash_item);
  VAR m: BITSET;  res: INTEGER;
BEGIN
  di(m);
  IF c^.activ THEN os.wait(c^.writen) END;
  ei(m);
  IF c^.dirty#{} THEN res:=update_dirty_block(c) END
END cash_update;

PROCEDURE cash_lock(dev: PORT; blk: INTEGER; VAR csh: cash_item);
  VAR c: cash_item;
    f,b: cash_item;
    inx: cash_item;
    i,j: INTEGER;
  found: BOOLEAN;
BEGIN
  os.acquire(c_search);
  os.wait(exist_free);
  i:=ABS(blk) MOD SIZE(c_index);
  inx:=c_index[i];  c:=inx;
  IF c#NIL THEN
    LOOP
      found:=(set(c^.blk=blk)*set(c^.dev=dev)#{});
      IF found THEN EXIT END;
      c:=c^.i_fwd;
      IF c=inx THEN EXIT END
    END;
    IF found THEN
      IF c_ring=c THEN
        c_ring:=c^.t_fwd
      ELSE
        b:=c^.t_bck;     f:=c^.t_fwd;
        b^.t_fwd:=f;     f^.t_bck:=b;

        b:=c_ring^.t_bck;
        c^.t_fwd:=c_ring;  c^.t_bck:=b;
        c_ring^.t_bck:=c;  b^.t_fwd:=c
      END;
      c_index[i]:=c;  csh:=c;  jump(c_search,c^.use);  RETURN
    END
  END;

  c:=c_ring;
  WHILE c^.use.prs#NIL DO c:=c^.t_fwd END;
  (* wait(exist_free) garantied that this loop finished! *)
  c_ring:=c^.t_fwd;

  j:=ABS(c^.blk) MOD SIZE(c_index);  (* "j" old position of "c" element *)
  (* are new & old positions equal? *)
  IF i#j THEN (* if not equal then untie from old index list *)
    IF c_index[j]=c THEN c_index[j]:=c^.i_fwd END;
    IF c=c^.i_fwd THEN (* "c" is single element of index list *)
      c_index[j]:=NIL
    ELSE (* untie "c" out of index list *)
      f:=c^.i_fwd;        b:=c^.i_bck;
      b^.i_fwd:=f;        f^.i_bck:=b
    END; (* and insert "c" in the new index list *)
    IF inx=NIL THEN
      c^.i_fwd:=c; c^.i_bck:=c
    ELSE
      b:=inx^.i_bck;
      c^.i_fwd:=inx;   c^.i_bck:=b;
      inx^.i_bck:=c;   b^.i_fwd:=c
    END
  END;
  c_index[i]:=c;

  cash_update(c);

  (* now we may update informtaion in cash element: *)
  c^.dev:=dev;  c^.size :=0;
  c^.blk:=blk;  c^.dirty:={};
  csh:=c;
  jump(c_search,c^.use)
END cash_lock;

PROCEDURE cash_unlock(c: cash_item);
BEGIN
  os.release(c^.use); os.send(exist_free)
END cash_unlock;

(* erase_cash will be called only when dev.opens=0 *)
(* so it has no need acquire(c^.use)               *)

PROCEDURE erase_cash(dev: PORT);
  VAR m: BITSET;
      c: cash_item;
    res: INTEGER;
BEGIN
  os.acquire(c_search);
  c:=c_ring;
  REPEAT
    IF c^.dev=dev THEN
      di(m);
      IF c^.use.prs=NIL THEN
        cash_update(c);
        c^.dev:=NIL; c^.size:=0; c^.dirty:={}
      ELSE
        ei(m)
      END
    END;
    c:=c^.t_fwd
  UNTIL c=c_ring;
  os.release(c_search);
  IF dev^.updt THEN res:=update_super(dev) END
END erase_cash;

PROCEDURE add_to_cash(address,size: INTEGER);
  VAR c: cash_item; adr: ADDRESS;
BEGIN
  adr:=address;
  os.lock;
  os.acquire(c_search);
  WHILE size>=1024 DO
    os.ALLOCATE(cash_area,c,SIZE(c^));
    IF c=NIL THEN low.QUIT END;
    INC(chsize,4*256);
    c^.buf :=adr;    c^.dirty:={ };
    c^.dev :=NIL;    c^.u_fwd:=NIL;
    c^.size:= 0;     c^.u_bck:=NIL;
    c^.blk := 0;     c^.activ:=FALSE;
    os.ini_signal(c^.writen,os.sendup,0);
    IF c_ring=NIL THEN
      c^.t_fwd:=c;            c^.t_bck:=c;
    ELSE
      c^.t_fwd:=c_ring;    c^.t_bck:=c_ring^.t_bck;
      c^.t_fwd^.t_bck:=c;  c^.t_bck^.t_fwd:=c;
    END;
    c_ring:=c;
    IF c_index[0]=NIL THEN
      c_index[0]:=c; c^.i_fwd:=c; c^.i_bck:=c
    ELSE
      c^.i_fwd:=c_index[0];
      c^.i_bck:=c_index[0]^.i_bck;
      c^.i_fwd^.i_bck:=c;
      c^.i_bck^.i_fwd:=c
    END;
    os.ini_mutex(c^.use);
    os.send(exist_free);
    INC(adr, 1024);
    DEC(size,1024)
  END;
  os.release(c_search);
  os.unlock
END add_to_cash;

(* WARNING!    "wssc" is undefined for not mounted devices          *)
(*             cash_io_xxxx procedures works with such devices too! *)

(* in 'sync_process' acquire(c_search) must be optimized *)
(* in future Ned, Leo & Igo know                         *)

PROCEDURE sync_process;
  VAR dev: PORT;
    i,res: INTEGER;
BEGIN
  os.lock;
  os.set_prio(os.active(),-1);
  LOOP
    i:=os.wait_del(200,update);
    WHILE u_ring#NIL DO res:=update_dirty_block(NIL) END;
    os.wait(Plock);
      dev:=ports;
      WHILE dev#NIL DO
        IF (dev^.mode=disk) & dev^.updt & (dev^.slock.cou>0) THEN
          os.wait(dev^.slock);
            res:=update_super(dev);
          os.send(dev^.slock)
        END;
        dev:=dev^.next
      END;
    os.send(Plock)
  END
END sync_process;

PROCEDURE send_dirty(c: cash_item; add: BITSET);
  VAR m: BITSET;
BEGIN
  di(m);
  c^.dirty:=c^.dirty+add;
  IF c^.u_fwd#NIL THEN
    IF c^.u_fwd#u_ring THEN
      IF u_ring=c THEN u_ring:=u_ring^.u_fwd END;
      (* untie *)
      c^.u_fwd^.u_bck:=c^.u_bck;
      c^.u_bck^.u_fwd:=c^.u_fwd;
      (* retie *)
      c^.u_fwd:=u_ring;
      c^.u_bck:=u_ring^.u_bck;
      c^.u_bck^.u_fwd:=c;
      c^.u_fwd^.u_bck:=c
    END;
    ei(m); RETURN
  END;     ------
  IF u_ring=NIL THEN
    u_ring:=c;  c^.u_fwd:=c;  c^.u_bck:=c
  ELSE
    c^.u_fwd:=u_ring;
    c^.u_bck:=u_ring^.u_bck;
    c^.u_bck^.u_fwd:=c;
    c^.u_fwd^.u_bck:=c
  END;
  IF update.cou<1 THEN os.send(update) END;
  ei(m)
END send_dirty;

(* NB: update_dirty_block may be called when and only when   *)
(*     'c^.use' locked. In procedures 'erase_cash' and       *)
(*     'sync_process' 'c_search' & 'c^.use' locked together, *)
(*     but in fixed order to prevent deadlocks.              *)

PROCEDURE update_dirty_block(c: cash_item): INTEGER;

  PROCEDURE exit(c: cash_item);
    VAR m: BITSET;
  BEGIN
    IF c^.dev^.error#ok THEN fix_write_error(c^.dev,c^.blk,c^.dev^.error) END;
    di(m);
      c^.activ:=FALSE;
      IF c^.writen.queue#os.null THEN os.send(c^.writen) END;
    ei(m)
  END exit;

  VAR  p: PORT;        r: REQUEST;
     end: INTEGER;   res: INTEGER;
   b,len: INTEGER;  Sdrt: BITSET;
  s,from: INTEGER; drt,m: BITSET;

BEGIN
  di(m);
    IF c=NIL THEN c:=u_ring        END; (* see: syn_process *)
    IF c=NIL THEN ei(m); RETURN ok END; (* u_ring empty now *)
    drt:=TR(c^.dirty);   ------
    IF c^.u_fwd#NIL THEN
      IF c^.u_fwd=c THEN u_ring:=NIL
      ELSE
        IF u_ring=c THEN u_ring:=u_ring^.u_fwd END;
        c^.u_fwd^.u_bck:=c^.u_bck;
        c^.u_bck^.u_fwd:=c^.u_fwd
      END;
      c^.u_fwd:=NIL; c^.u_bck:=NIL
    END;
    IF drt={} THEN ei(m); RETURN ok END;
    c^.activ:=TRUE;       ------
  ei(m);

  Sdrt:=drt;
  p:=c^.dev;
  IF p^.state*_wpro#{} THEN
    p^.error:=err.write_pro; p^.errsc:=mpw2(c^.blk,12-p^.ssc); exit(c);
    RETURN p^.error
  END;
  r.op :=req.WRITE;       res:=ok;
  r.drn:=p^.drn;        r.pos:=0;
  end:=mpw2(1     ,12-p^.ssc);
  b  :=mpw2(c^.blk,12-p^.ssc);
  s  :=0;
  LOOP
    WHILE (s<end) & (drt*{0}={}) DO s:=s+1; drt:=drt>>1 END;
    IF s=end THEN EXIT END;
    from:=s;
    REPEAT s:=s+1;    drt:=drt>>1   UNTIL (s=end) OR (drt*{0}={});
    r.ofs:=b+from;    r.buf:=c^.buf+mpw2(from,p^.ssc-2);
    r.len:=s-from;    r.res:=ok;
    os.wait(p^.lock^);
      p^.doio(r);
    os.send(p^.lock^);
    INC(dkwrite,r.len);
    IF r.res#ok THEN res:=r.res END
  END;
  IF res=ok THEN exit(c); RETURN ok END;

  (* Sector per Sector RERTY *)
  res:=ok;
  s:=0;  drt:=Sdrt;
  LOOP
    WHILE (s<end) & (drt*{0}={}) DO s:=s+1; drt:=drt>>1 END;
    IF s=end THEN EXIT END;
    r.ofs:=b+s;       r.buf:=c^.buf+mpw2(s,p^.ssc-2);
    r.len:=1;         r.res:=ok;
    os.wait(p^.lock^);
      p^.doio(r);
    os.send(p^.lock^);
    IF r.res#ok THEN res:=r.res; p^.errsc:=s; p^.error:=r.res END;
    INC(s)
  END;
  exit(c);
  RETURN res
END update_dirty_block;

PROCEDURE cash_read(c: cash_item; len: INTEGER): INTEGER;
  VAR r,t: REQUEST; s,h,sl: INTEGER; p: PORT;
BEGIN
  p:=c^.dev;
  h:=p^.secsz-1;
  s:=dpw2(c^.size,p^.ssc);
  len:=INTEGER( set(len+h) - set(h));
  sl:=dpw2(len,p^.ssc);
  IF len<=c^.size THEN INC(chread,sl); RETURN ok END;

  r.op :=req.READ;  r.ofs:=mpw2(c^.blk,12-p^.ssc)+s; (* 2**12=4096 *)
  r.drn:=p^.drn;    r.buf:=c^.buf+mpw2(s,p^.ssc-2);
  r.pos:=0;         r.res:=ok;
  r.len:=sl-s;
  os.wait(p^.lock^);
    p^.doio(r);
  os.send(p^.lock^);
  INC(dkread,r.len); INC(chread,s);
  IF r.res=ok THEN c^.size:=len; RETURN r.res END;

  (* Sector per Sector RERTY *)
  r.res:=ok;
  r.op :=req.READ;  r.ofs:=mpw2(c^.blk,12-p^.ssc)+s; (* 2**12=4096 *)
  r.drn:=p^.drn;    r.buf:=c^.buf+mpw2(s,p^.ssc-2);
  r.pos:=0;         r.res:=ok;
  r.len:=1;
  sl:=sl-s;
  WHILE sl>0 DO
    t:=r;  t.res:=ok;
    os.wait(p^.lock^);
      p^.doio(t);
    os.send(p^.lock^);
    IF t.res#ok THEN r.res:=t.res END;
    INC(r.buf,mpw2(1,p^.ssc-2));
    INC(r.ofs);
    DEC(sl)
  END;
  RETURN r.res
END cash_read;

PROCEDURE cash_write(c: cash_item; pos,len: INTEGER; wait: BOOLEAN): INTEGER;
  VAR first,last,s,h: INTEGER; p: PORT;
BEGIN
  p:=c^.dev;
  IF p^.state*_wpro#{} THEN RETURN err.write_pro END;
  first:=dpw2(pos,p^.ssc);
  last :=dpw2(pos+len-1,p^.ssc);
  h:=p^.secsz-1;
  s:=INTEGER(set(pos+len+h)-set(h));
  IF c^.size<s THEN c^.size:=s END;
  IF wait OR SYNC OR (c^.dev^.state*_sync#{}) THEN
    c^.dirty:=c^.dirty+{first..last};
    RETURN update_dirty_block(c)
  ELSE
    send_dirty(c,{first..last}); RETURN ok
  END
END cash_write;

PROCEDURE cash_write_sec(c: cash_item; w_ofs: INTEGER; wait: BOOLEAN): INTEGER;
  VAR r: REQUEST;  p: PORT;  sec: INTEGER;
BEGIN
  p:=c^.dev;
  IF p^.state*_wpro#{} THEN RETURN err.write_pro END;
  sec  :=dpw2(w_ofs,p^.ssc-2);
  w_ofs:=mpw2(sec  ,p^.ssc-2);
  IF c^.size<w_ofs*4+p^.secsz THEN c^.size:=w_ofs*4+p^.secsz END;
  IF wait OR SYNC OR (c^.dev^.state*_sync#{}) THEN
    r.op :=req.WRITE;
    r.ofs:=mpw2(c^.blk,12-p^.ssc)+sec;
    r.drn:=p^.drn;    r.buf:=c^.buf+w_ofs;
    r.pos:=0;         r.len:=1;
    r.res:=ok;
    EXCL(c^.dirty,sec);
    os.wait(p^.lock^);
      p^.doio(r);
    os.send(p^.lock^);
    INC(dkwrite);
    RETURN r.res
  ELSE
    send_dirty(c,{sec}); RETURN ok
  END
END cash_write_sec;

(* any cash_item always must have legal contence        *)
(* of .buf[0..c^.size-1]. CASH_READ_SEC is an exception *)
(* from this rule! But it used only in 'write' proc     *)
(* (see) in lock mode and immidiatly after read_sec     *)
(* the buffer contence will be filled by 'cmove' in     *)
(* 'write'.                                             *)

PROCEDURE cash_read_sec(c: cash_item; w_ofs: INTEGER): INTEGER;
  VAR sec: INTEGER; r: REQUEST; p: PORT;
BEGIN
  sec  :=dpw2(w_ofs,p^.ssc-2);
  w_ofs:=mpw2(sec  ,p^.ssc-2);
  IF w_ofs*4+p^.secsz<=c^.size THEN INC(chread); RETURN ok END;

  p:=c^.dev;
  r.op :=req.READ;  r.ofs:=mpw2(c^.blk,12-p^.ssc)+sec;
  r.drn:=p^.drn;    r.buf:=c^.buf+w_ofs;
  r.pos:=0;         r.len:=1;
  r.res:=ok;
  os.wait(p^.lock^);
    p^.doio(r);
  os.send(p^.lock^);
  INC(dkread);
  (* Note c^.size not changed! *)
  RETURN r.res
END cash_read_sec;

PROCEDURE cash_def(VAL s: ARRAY OF CHAR;
                   VAR a: ADDRESS;  VAR size: INTEGER): BOOLEAN;
  VAR i: INTEGER;
      t: ADDRESS;
   done: BOOLEAN;
   item: cash_item;
BEGIN
  i:=1;
  str.skip(s,i,' ');
  IF i+4>HIGH(s) THEN RETURN FALSE END;
  IF (s[i]='C')&(s[i+1]='A')&(s[i+2]='S')&(s[i+3]='H')&(s[i+4]=' ') THEN
    i:=i+4
  ELSE
    RETURN FALSE
  END;
  str.skip(s,i,' ');
  IF (i+3<=HIGH(s)) & (s[i]='M')&(s[i+1]='E')&(s[i+2]='M')&(s[i+3]=' ') THEN
    i:=i+3; a:=NIL
  ELSIF (i+3<=HIGH(s)) & (s[i]='S')&(s[i+1]='Y')&(s[i+2]='N')&(s[i+3]='C') THEN
    SYNC:=TRUE; a:=NIL; size:=0; RETURN FALSE
  ELSE
    str.iscan(a,s,i,done);
    IF NOT done THEN RETURN FALSE END;
  END;
  str.skip(s,i,' ');
  str.iscan(size,s,i,done);
  IF NOT done THEN RETURN FALSE END;
  IF (i<=HIGH(s)) & (s[i]='K') THEN size:=size*KB END;
  IF size<=0 THEN RETURN FALSE END;
  IF a=NIL THEN
    os.ALLOCATE(cash_area,a,size);
    IF a=NIL THEN RETURN FALSE END;
  END;
  item:=c_ring;
  IF item#NIL THEN
    REPEAT
      IF (item^.buf<=a) & (a<item^.buf+4*KB)
      OR (a<=item^.buf) & (item^.buf<a+size) THEN
        os.print("cash memory segment overlaps %08h..%08h\n",a,a+size-1);
        RETURN FALSE
      END;
      item:=item^.t_fwd
    UNTIL item=c_ring
  END;
  FOR i:=0 TO size-1 BY KB DO
    t:=a+i; t^:=12345678h;
    IF t^#12345678h THEN
      os.print("cash memory error at address %08h\n",t); RETURN FALSE
    END
  END;
  RETURN TRUE
END cash_def;

PROCEDURE root_def(VAL s: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=1;
  str.skip(s,i,' ');
  IF i+4>HIGH(s) THEN RETURN END;
  IF (s[i]='R')&(s[i+1]='O')&(s[i+2]='O')&(s[i+3]='T')&(s[i+4]=' ') THEN
    i:=i+4
  ELSE
    RETURN
  END;
  str.skip(s,i,' '); j:=i; str.search(s,j,' ');
  IF i=j THEN RETURN END;
  str.sub_str(ROOT,(s),i,j-i+1)
END root_def;

PROCEDURE system_cash;
  VAR i: INTEGER;
     co: INTEGER;
    adr: ADDRESS;
    str: STRING;
   size: INTEGER;
BEGIN
  i:=0; co:=0;
  WHILE os.get_sys_parm(i,str) DO
    IF cash_def(str,adr,size) THEN
      size:=size DIV (4*KB);
      IF size>0 THEN
        co:=co+size; add_to_cash(adr,size*4*KB)
      END
    ELSE
      root_def(str)
    END
  END;
  IF co<3 THEN
    size:=16*KB;
    os.ALLOCATE(cash_area,adr,size);
    IF adr=NIL THEN low.QUIT END;
    add_to_cash(adr,size)
  END
END system_cash;

PROCEDURE init_cash;
  VAR i: INTEGER; p: os.process;
BEGIN
  os.ini_mutex(c_search);
  os.ini_signal(exist_free,{},0);
  c_ring:=NIL;
  u_ring:=NIL;
  FOR i:=0 TO HIGH(c_index) DO c_index[i]:=NIL END;
  IF NOT os.new(cash_area) THEN ASSERT(FALSE) END;
  system_cash;

  i:=os.make_process(p,sync_process,512);
  IF i#ok THEN HALT(i) END;
  p^.pp^.M:=os.MASK;
  os.start(p)
END init_cash;

------------------------  SETS MANAGER  ------------------------
                        ----------------

PROCEDURE _mount(dev: PORT; info: ADDRESS; len: INTEGER): INTEGER;
  VAR r: req.REQUEST;
BEGIN
  IF dev^.mode#disk THEN RETURN err.unsuitable END;
  r.op :=req.MOUNT;
  r.drn:=dev^.drn;         r.res:=ok;
  r.buf:=info;             r.len:=len;
  r.pos:=0;                r.ofs:=0;
  os.wait(dev^.lock^);
    dev^.doio(r);
  os.send(dev^.lock^);
  RETURN r.res
END _mount;

PROCEDURE _unmount(dev: PORT);
  VAR r: req.REQUEST;
BEGIN
  IF dev^.mode#disk THEN RETURN END;
  r.op :=req.UNMOUNT;
  r.drn:=dev^.drn;  r.pos:=0;
  r.res:=ok;        r.len:=0;
  r.buf:=NIL;       r.ofs:=0;
  os.wait(dev^.lock^);
    dev^.doio(r);
  os.send(dev^.lock^)
END _unmount;

CONST CXE = 00455843h; (*"CXE"0c*)
    LABEL = 16;

PROCEDURE lock_super(dev: PORT);
BEGIN
  os.wait(dev^.slock); dev^.res:=ok
END lock_super;

PROCEDURE unlock_super(dev: PORT);  (* not changed dev^.res ! *)
BEGIN
  os.send(dev^.slock)
END unlock_super;

PROCEDURE mount_super(dev: PORT; VAL info: ARRAY OF CHAR);

  VAR r: req.REQUEST;
     du: INTEGER;       w: BITSET;
    buf: ADDRESS;
    lab: POINTER TO ARRAY [0..LABEL-1] OF INTEGER;
   size: INTEGER;
  i,b,n: INTEGER;

  PROCEDURE undo; BEGIN os.DEALLOCATE(os.system,buf,size) END undo;

BEGIN
  IF dev^.mode#disk THEN dev^.res:=err.unsuitable; RETURN END;
  erase_cash(dev);
  dev^.res:=_get_spec(dev);
  IF dev^.res#ok THEN RETURN END;
  size:=dev^.secsz DIV 4;
  os.ALLOCATE(os.system,buf,size);
  IF buf=NIL THEN dev^.res:=err.no_memory; RETURN END;
  i:=HIGH(info)+1;
  i:=(i+3) DIV 4;
  IF i>size THEN i:=size END;
  low.move(buf,SYSTEM.ADR(info),i);
  dev^.res:=_mount(dev,buf,i*4);
  IF dev^.res#ok THEN undo; RETURN END;
  dev^.res:=_get_spec(dev);
  IF dev^.res#ok THEN undo; RETURN END;
  dev^.wssc:=dev^.ssc-2;                        (* ! *)
  r.op :=req.READ;
  r.buf:=buf;   r.ofs:=mpw2(1,12-dev^.ssc);
  r.len:=1;     r.drn:=dev^.drn;
  r.pos:=0;     r.res:=ok;
  os.wait(dev^.lock^);
    dev^.doio(r);  dev^.res:=r.res;
  os.send(dev^.lock^);
  IF dev^.res#ok THEN undo; RETURN END;
  lab:=buf;
  IF lab^[6]#CXE THEN undo; dev^.res:=err.bad_fsys; RETURN END;
  i:=lab^[4];
  b:=lab^[5];
  n:=mpw2((HIGH(dev^.dirty)+1)*32*8,dev^.ssc);
  IF i+31+b+31>=n-LABEL*32 THEN undo; dev^.res:=err.too_large; RETURN END;

  n:=( (i+31) DIV 32 + (b+31) DIV 32 + LABEL )*4;
  n:=INTEGER( BITSET(n+dev^.secsz-1) - BITSET(dev^.secsz-1) ) DIV 4;
  IF n>size THEN
    os.DEALLOCATE(os.system,buf,size);
    size:=n;
    os.ALLOCATE(os.system,buf,size);
    IF buf=NIL THEN dev^.res:=err.no_memory; RETURN END;
    r.op :=req.READ;
    r.pos:=0;         r.ofs:=mpw2(1,12-dev^.ssc);
    r.res:=ok;        r.buf:=buf;
    r.drn:=dev^.drn;  r.len:=dpw2(size,dev^.wssc);
    os.wait(dev^.lock^);
      dev^.doio(r);  dev^.res:=r.res;
    os.send(dev^.lock^);
    IF dev^.res#ok THEN undo; RETURN END
  END;

  dev^.super:=buf;
  dev^.supsz:=size;

  dev^.vol  :=buf;
  dev^.bset :=buf+LABEL;
  dev^.iset :=buf+LABEL + (b+31) DIV 32;
  dev^.bhigh:=(b+31) DIV 32 - 1;
  dev^.ihigh:=(i+31) DIV 32 - 1;
  dev^.inoL :=1 + (dev^.supsz+1023) DIV 1024;
  dev^.inoH :=dev^.inoL + (i+63) DIV 64 - 1;
  dev^.res  :=ok;       low.zero(dev^.dirty);

  n:=b MOD 32; (* tail word *)
  IF n # 0 THEN (* last blocks don`t exist, so will be marked as busy *)
    dev^.bset^[dev^.bhigh]:=dev^.bset^[dev^.bhigh]-{n..31}
  END;
  du:=0;
  FOR n:=0 TO dev^.bhigh DO
    w:=dev^.bset^[n];
    IF    w={0..31} THEN INC(du,32);
    ELSIF w#{}      THEN
      REPEAT INC(du,INTEGER(w*{0})); w:=(w-{0})>>1 UNTIL w={}
    END
  END;
  dev^.du:=dev^.secs-mpw2(du,12-dev^.ssc);
  n:=0;
  WHILE (n<dev^.bhigh) & (dev^.bset^[n]={}) DO INC(n) END;
  dev^.sb_b:=n;
  n:=dev^.ihigh;
  WHILE (n>0) & (dev^.iset^[n]={}) DO DEC(n) END;
  dev^.sb_i:=dev^.ihigh-n;
  dev^.res :=ok
END mount_super;

PROCEDURE update_super(dev: PORT): INTEGER;
  VAR r: req.REQUEST;
      w: BITSET;
     xx: DIRTYMARK;
    d,x: ADDRESS;
    n,s: INTEGER;
    res: INTEGER;
   from: INTEGER;
BEGIN
  xx:=dev^.dirty;
  IF (xx[0]+xx[1]+xx[2]+xx[3]={}) THEN RETURN ok END;
  IF dev^.state*_wpro#{} THEN
    dev^.error:=err.write_pro; dev^.errsc:=mpw2(1,12-dev^.ssc);
    fix_write_error(dev,1,dev^.error);
    RETURN dev^.error
  END;
  dev^.updt:=FALSE;
  res:=ok;
  n:=dpw2(dev^.supsz,dev^.wssc);
  (* n = number of sectors in super block *)
  r.op :=req.WRITE;
  r.drn:=dev^.drn;
  r.pos:=0;
  d:=SYSTEM.ADR(dev^.dirty[0]);
  x:=SYSTEM.ADR(xx);
  s:=0; w:={0};
  LOOP
    WHILE (s<n) & (BITSET(d^)*w={}) DO
      w:=w<<1; INC(s); INC(d,INTEGER(w*{0})); INC(x,INTEGER(w*{0}))
    END;
    IF s>=n THEN EXIT END;
    from:=s;
    REPEAT
      x^:=BITSET(d^)-w; w:=w<<1; INC(s);
      INC(d,INTEGER(w*{0})); INC(x,INTEGER(w*{0}))
    UNTIL (s=n) OR (BITSET(d^)*w={});
    r.len:=s-from;      r.ofs:=mpw2(1,12-dev^.ssc)+from;
    r.res:=ok;          r.buf:=dev^.super+mpw2(from,dev^.wssc);
    os.wait(dev^.lock^);
      dev^.doio(r);
    os.send(dev^.lock^);
    INC(dkwrite,r.len);
    IF r.res#ok THEN res:=r.res END
  END;
  IF res=ok THEN dev^.dirty:=xx; RETURN ok END;

  s:=0; w:={0}; xx:=dev^.dirty; res:=ok;
  LOOP
    WHILE (s<n) & (BITSET(d^)*w={}) DO
      w:=w<<1; INC(s); INC(d,INTEGER(w*{0})); INC(x,INTEGER(w*{0}))
    END;
    IF s>=n THEN EXIT END;
    r.len:=1;           r.ofs:=mpw2(1,12-dev^.ssc)+s;
    r.res:=ok;          r.buf:=dev^.super+mpw2(s,dev^.wssc);
    os.wait(dev^.lock^);
      dev^.doio(r);
    os.send(dev^.lock^);
    IF r.res#ok THEN res:=r.res; dev^.errsc:=s; dev^.error:=res END;
    x^:=BITSET(d^)-w;      INC(s);       w:=w<<1;
    INC(d,INTEGER(w*{0})); INC(x,INTEGER(w*{0}))
  END;
  IF res=ok THEN dev^.dirty:=xx
  ELSE fix_write_error(dev,1,dev^.error)
  END;
  RETURN res
END update_super;

PROCEDURE unmount_super(dev: PORT);
BEGIN
  erase_cash(dev);
  os.DEALLOCATE(os.system,dev^.super,dev^.supsz);
  dev^.supsz:=0;
  dev^.ihigh:=-1;
  dev^.bhigh:=-1;
  dev^.bset :=NIL;
  dev^.iset :=NIL;
  dev^.vol  :=NIL;
  _unmount(dev)
END unmount_super;

PROCEDURE new_super(dev: PORT;
               VAL name: ARRAY OF CHAR;
                    i,b: INTEGER);
  VAR n: INTEGER;          c: CHAR;
    lab: ref_ptr;        vol: vol_ptr;
BEGIN
  n:=0;
  WHILE (n<=HIGH(name)) & (name[n]#0c) DO
    c:=name[n];
    IF (c=' ') OR (c='/') OR (c=':') OR (c='\') THEN
      dev^.res:=err.bad_name; RETURN
    END; INC(n)
  END;
  IF n>7                   THEN dev^.res:=err.bad_name;  RETURN END;
  IF dev^.super#NIL        THEN dev^.res:=err.bad_fsys;  RETURN END;
  n:=mpw2((HIGH(dev^.dirty)+1)*32*8,dev^.ssc);
  IF i+31+b+31>=n-LABEL*32 THEN dev^.res:=err.too_large; RETURN END;
  dev^.wssc:=dev^.ssc-2;                        (* ! *)

  n:=( (i+31) DIV 32 + (b+31) DIV 32 + LABEL )*4;
  dev^.supsz:=INTEGER( BITSET(n+dev^.secsz-1) - BITSET(dev^.secsz-1) ) DIV 4;
  os.ALLOCATE(os.system,dev^.super,dev^.supsz);
  IF dev^.super=NIL THEN dev^.res:=err.no_memory; dev^.supsz:=0; RETURN END;

  low._zero(dev^.super,dev^.supsz);    low.zero(dev^.dirty);
  vol:=dev^.super;
  lab:=dev^.super;
  lab^[4]:=i;
  lab^[5]:=b;
  lab^[6]:=CXE;
  str.copy(vol^,name);

  dev^.bset :=ADDRESS(dev^.super)+LABEL;
  dev^.iset :=ADDRESS(dev^.super)+LABEL+(b+31) DIV 32;
  dev^.bhigh:=(b+31) DIV 32 - 1;
  dev^.ihigh:=(i+31) DIV 32 - 1;
  dev^.sb_i :=0;
  dev^.sb_b :=0;
  dev^.inoL :=1 + (dev^.supsz+1023) DIV 1024;
  dev^.inoH :=dev^.inoL + (i+63) DIV 64 - 1;
  low._fill(dev^.bset,dev^.bhigh+1,{0..31});
  low._fill(dev^.iset,dev^.ihigh+1,{0..31});
END new_super;

(*$<*) (*$W+*) (*$T-*)

PROCEDURE b_deall0(dev: PORT; f,no: INTEGER; VAR b: ARRAY OF INTEGER);
  VAR i,j,n: INTEGER;
BEGIN
  FOR i:=f TO f+no-1 DO
    j:=b[i];
    IF j>dev^.inoH THEN
      n:=j DIV 32;
      IF n<dev^.sb_b THEN dev^.sb_b:=n END;
      INCL(dev^.bset^[n],j MOD 32);
      DEC (dev^.du,mpw2(1,12-dev^.ssc));
      n:=dpw2(n+LABEL,dev^.wssc); (* changed sector no *)
      INCL(dev^.dirty[n DIV 32],n MOD 32); dev^.updt:=TRUE
    END;
    b[i]:=-1
  END
END b_deall0;

PROCEDURE b_alloc0(dev: PORT; f,no: INTEGER; VAR b: ARRAY OF INTEGER);
  VAR set: set_ptr;
    i,j,n: INTEGER;
   k,high: INTEGER;
        w: BITSET;
BEGIN
  FOR n:=f TO f+no-1 DO b[n]:=-1 END;
  i   :=dev^.sb_b;
  high:=dev^.bhigh;
  set :=dev^.bset;
  FOR n:=f TO f+no-1 DO
    WHILE (i<=high) & (set^[i]={}) DO i:=i+1 END;
    IF i>high THEN
      b_deall0(dev,f,n,b); dev^.res:=err.no_space; RETURN
    END;
    j:=0; w:=set^[i];
    WHILE w*{0}={} DO INC(j); w:=w>>1 END;
    k:=i*32+j;
    IF k<=dev^.inoH THEN
      b_deall0(dev,f,n,b); dev^.res:=err.bad_fsys; RETURN
    END;
    b[n]:=k;
    EXCL(set^[i],j);
    INC(dev^.du,mpw2(1,12-dev^.ssc));
    dev^.sb_b:=i;
    k:=dpw2(i+LABEL,dev^.wssc); (* changed sector *)
    INCL(dev^.dirty[k DIV 32],k MOD 32); dev^.updt:=TRUE
  END
END b_alloc0;

PROCEDURE clear_ref(VAR r: ARRAY OF INTEGER);
BEGIN
  low.fill(r,-1)
END clear_ref;

PROCEDURE extend_short(f: FILE; n: INTEGER);
(* extend short file upto 'n' blocks *)
(* 'f^.eof' not changed              *)
  VAR i: INTEGER;
BEGIN
  i:=HIGH(f^.ref)+1;
  REPEAT i:=i-1 UNTIL (i<0) OR (f^.ref[i]>0); INC(i);
  n:=n-i;
  IF n<=0 THEN RETURN END;
  b_alloc0(f^.dev,i,n,f^.ref);
  IF f^.dev^.res=ok THEN f^.state:=f^.state+dirty END;
END extend_short;

PROCEDURE extend_long(f: FILE; n: INTEGER);
(* extend long  file upto 'n' blocks *)
(* 'f^.eof' not changed              *)
  VAR rr: ref_ptr;
     new: BOOLEAN;
     r,p: INTEGER;
    wlen: INTEGER;
    l,io: INTEGER;
   r_csh: cash_item;
BEGIN
  IF n>=1024*SIZE(REF) THEN f^.dev^.res:=err.too_large; RETURN  END;
  r:=HIGH(f^.ref)+1;
  REPEAT r:=r-1 UNTIL (r<0) OR (f^.ref[r]>0);
  IF r>=0 THEN
    cash_lock(f^.dev,f^.ref[r],r_csh);
    io:=cash_read(r_csh,4096);
    IF io#ok THEN cash_unlock(r_csh); f^.dev^.res:=io; RETURN END;
    rr:=r_csh^.buf;
    p:=1024;
    REPEAT p:=p-1 UNTIL (p<0) OR (rr^[p]#-1); INC(p);
    cash_unlock(r_csh);
  ELSE
    r:=0; p:=0; (* empty long file *)
  END;
  n:=n-(r*1024+p);
  IF n<=0 THEN RETURN END;
  (* here: r*1024+p<1024*SIZE(REF)! *)
  IF p=1024 THEN  r:=r+1;  p:=0  END;
  WHILE n>0 DO
    IF p+n<=1024 THEN l:=n ELSE l:=1024-p END;
    new:=(f^.ref[r]<=0);
    IF new THEN
      b_alloc0(f^.dev,r,1,f^.ref);
      IF f^.dev^.res#ok THEN RETURN END;
      f^.state:=f^.state+dirty
    END;
    cash_lock(f^.dev,f^.ref[r],r_csh);
    rr:=r_csh^.buf;
    wlen:=l*4;
    IF p=0 THEN
      clear_ref(rr^);
      IF new THEN wlen:=4096 END
    ELSE (* p>0 *)
      io:=cash_read(r_csh,p*4);
      IF io#ok THEN  cash_unlock(r_csh);  f^.dev^.res:=io;  RETURN  END
    END;
    b_alloc0(f^.dev,p,l,rr^);
    IF f^.dev^.res#ok THEN cash_unlock(r_csh); RETURN END;
    io:=cash_write(r_csh,p*4,wlen,f^.state*wait_wr#{});
    IF io#ok THEN
      b_deall0(f^.dev,0,l+p,rr^);  cash_unlock(r_csh);
      f^.dev^.res:=io;             RETURN
    END;
    cash_unlock(r_csh);
    p:=0;  r:=r+1;  n:=n-l
  END
END extend_long;

PROCEDURE short2long(f: FILE);
(* convert short file to long file *)
(* 'f^.eof' not changed            *)
  VAR rr: ref_ptr;
      io: INTEGER;
      rf: REF;
   r_csh: cash_item;
BEGIN
  ASSERT(f^.mode*long={},4Fh);
  clear_ref(rf);
  b_alloc0(f^.dev,0,1,rf);
  IF f^.dev^.res#ok THEN RETURN END;
  cash_lock(f^.dev,rf[0],r_csh);
  rr:=r_csh^.buf;
  clear_ref(rr^);
  low.move(rr,SYSTEM.ADR(f^.ref),SIZE(rf));
  io:=cash_write(r_csh,0,4096,f^.state*wait_wr#{});
  cash_unlock(r_csh);
  IF io#ok THEN b_deall0(f^.dev,0,1,rf);  f^.dev^.res:=io;  RETURN  END;
  f^.ref:=rf;  f^.mode:=f^.mode+long;     f^.state:=f^.state+dirty
END short2long;

PROCEDURE extend_file0(f: FILE; n: INTEGER);
BEGIN
  IF (n>HIGH(f^.ref)) & (f^.mode*long={}) THEN
    short2long(f);
    IF f^.dev^.res#ok THEN RETURN END;
  END;
  IF f^.mode*long={} THEN extend_short(f,n) ELSE extend_long(f,n) END
END extend_file0;

(*
  "extend_file" procedure extend file;
    it changes only .ref and .mode fields;
    INCL dirty IN .state iff .ref changed;
    iff 'n'>SIZE(REF) and no errors occured
    'long' adds to .mode;
    file 'f' may have a "holes" in its layout;
    "extend_file" procedure does not fill
    this hole it allocates only blocks
    from last allocated block of 'f' to 'n-1'.
*)

PROCEDURE extend_file(f: FILE; size: INTEGER; VAR res: INTEGER);
BEGIN
  IF size<=0           THEN res:=ok;            RETURN END;
  IF size>MAXFILE THEN res:=err.too_large; RETURN END;
  lock_super(f^.dev);
    extend_file0(f,(size+4095) DIV 4096);
    res:=f^.dev^.res;
    IF SYNC OR (f^.dev^.state*_sync#{}) THEN
      f^.dev^.res:=update_super(f^.dev);
      IF res=ok THEN res:=f^.dev^.res END
    ELSE
      os.send(update)
    END;
  unlock_super(f^.dev)
END extend_file;

PROCEDURE cut_short(f: FILE; n: INTEGER);
(* cut short file downto 'n' blocks *)
(* 'f^.eof' not changed             *)
  VAR i: INTEGER;
BEGIN
  i:=HIGH(f^.ref)+1;
  REPEAT i:=i-1 UNTIL (i<0) OR (f^.ref[i]>0);
  i:=i+1;
  IF i<=n THEN RETURN END;
  b_deall0(f^.dev,n,i-n,f^.ref);  f^.state:=f^.state+dirty
END cut_short;

PROCEDURE cut_long(f: FILE; n: INTEGER);
(* cut long file downto 'n' blocks *)
(* 'f^.eof' not changed            *)
  VAR rr: ref_ptr;
     l,p: INTEGER;
    r,io: INTEGER;
   r_csh: cash_item;
BEGIN
  r:=HIGH(f^.ref)+1;
  REPEAT r:=r-1 UNTIL (r<0) OR (f^.ref[r]>0);
  IF r<0 THEN (* empty long file *);
    f^.mode:=f^.mode-long; f^.state:=f^.state+dirty; RETURN
  END;
  cash_lock(f^.dev,f^.ref[r],r_csh);
  rr:=r_csh^.buf;
  io:=cash_read(r_csh,4096);
  IF io#ok THEN cash_unlock(r_csh); f^.dev^.res:=io; RETURN END;
  p:=1024;
  REPEAT DEC(p) UNTIL (p<0) OR (rr^[p]#-1); INC(p);
  cash_unlock(r_csh);
  IF r*1024+p<=n THEN RETURN END;
  (* here: r>0 OR p>0 *)
  IF p=0 THEN p:=1024; r:=r-1 END;
  n:=r*1024+p-n;
  WHILE n>0 DO
    IF n>p THEN l:=p ELSE l:=n END;
    IF f^.ref[r]>0 THEN
      cash_lock(f^.dev,f^.ref[r],r_csh);
      rr:=r_csh^.buf;
      io:=cash_read(r_csh,4096);
      IF io#ok THEN cash_unlock(r_csh); f^.dev^.res:=io; RETURN END;
      b_deall0(f^.dev,p-l,p,rr^);
      io:=cash_write(r_csh,(p-l)*4,l*4,f^.state*wait_wr#{});
      cash_unlock(r_csh);
      IF p-l=0 THEN
        b_deall0(f^.dev,r,1,f^.ref);
        f^.state:=f^.state+dirty
      END;
      IF io#ok THEN f^.dev^.res:=io; RETURN END;
    END;
    n:=n-l;  p:=1024;  r:=r-1
  END
END cut_long;

PROCEDURE long2short(f: FILE);
  VAR i: INTEGER;
     rr: ref_ptr;
     rf: POINTER TO REF;
     io: INTEGER;
  r_csh: cash_item;
BEGIN
  i:=HIGH(f^.ref)+1;
  REPEAT i:=i-1 UNTIL (i<0) OR (f^.ref[i]>0);
  IF i<0 THEN f^.mode:=f^.mode-long; f^.state:=f^.state+dirty; RETURN END;
  IF i>0 THEN f^.dev^.res:=err.bad_fsys; RETURN END;
  cash_lock(f^.dev,f^.ref[0],r_csh);
  rr:=r_csh^.buf;
  io:=cash_read(r_csh,BYTES(REF));
  IF io#ok THEN cash_unlock(r_csh); f^.dev^.res:=io; RETURN END;
  i:=1024;
  REPEAT i:=i-1 UNTIL (i<0) OR (rr^[i]>0);
  IF i>HIGH(f^.ref) THEN
    cash_unlock(r_csh); f^.dev^.res:=err.bad_fsys; RETURN
  END;
  b_deall0(f^.dev,0,1,f^.ref);
  rf:=r_csh^.buf;
  f^.ref:=rf^;
  f^.mode:=f^.mode-long;
  f^.state:=f^.state+dirty;
  cash_unlock(r_csh)
END long2short;

PROCEDURE cut_file0(f: FILE; n: INTEGER);
BEGIN
  IF f^.mode*long={} THEN cut_short(f,n) ELSE cut_long(f,n) END;
  IF (f^.dev^.res=ok) & (n<=SIZE(REF)) & (f^.mode*long#{}) THEN
    long2short(f)
  END;
END cut_file0;

(*
  "cut_file" procedure cuts file;
    it changes only .ref and .mode fields;
    INCL dirty IN .state iff .ref changed;
    iff 'n'<=SIZE(REF) and no errors occured
    'long' excludes out of .mode;
    file 'f' may have a "holes" in its layout.
    after "cut_file" not more then 'n' blocks
    are allocated to file (may be less);
*)

PROCEDURE cut_file(f: FILE; size: INTEGER; VAR res: INTEGER);
BEGIN
  IF size<0             THEN size:=0            END;
  IF size>=MAXFILE THEN size:=MAXFILE END;
  lock_super(f^.dev);
    cut_file0(f,(size+4095) DIV 4096);
    res:=f^.dev^.res;
    IF SYNC OR (f^.dev^.state*_sync#{}) THEN
      f^.dev^.res:=update_super(f^.dev);
      IF res=ok THEN res:=f^.dev^.res END
    ELSE
      os.send(update)
    END;
  unlock_super(f^.dev)
END cut_file;

PROCEDURE i_alloc0(dev: PORT; VAR inode: INTEGER);
  VAR w: BITSET;
    i,j: INTEGER;
    set: set_ptr;
BEGIN
  i  :=dev^.ihigh-dev^.sb_i;
  set:=dev^.iset;
  WHILE (i>=0) & (set^[i]={}) DO DEC(i) END;
  IF i<0 THEN dev^.res:=err.fsys_full; RETURN END;
  j:=0;  w:=set^[i];
  WHILE w*{0}={} DO INC(j); w:=w>>1 END;
  EXCL(set^[i],j);          (* next search will start from  *)
  dev^.sb_i:=dev^.ihigh-i;  (* ihigh-sb_i=ihigh-(ihigh-i)=i *)
  inode:=i*32+j;
  i:=dpw2(i+LABEL+dev^.bhigh+1,dev^.wssc); (* changed sector *)
  INCL(dev^.dirty[i DIV 32],i MOD 32); dev^.updt:=TRUE
END i_alloc0;

PROCEDURE i_deall0(dev: PORT; inode: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=inode DIV 32;
  INCL(dev^.iset^[i],inode MOD 32);
  IF i>dev^.ihigh-dev^.sb_i THEN dev^.sb_i:=dev^.ihigh-i END;
  i:=dpw2(i+LABEL+dev^.bhigh+1,dev^.wssc); (* changed sector *)
  INCL(dev^.dirty[i DIV 32],i MOD 32); dev^.updt:=TRUE
END i_deall0;

(*
  "alloc_file" procedure: file fields used:
        .dev   on which device
        .drn            create file?
        .eof   to determine size (>=0)
  in any result next fields changed:
                   OK              BAD
        .mode   {[long]}           { }
        .inode  allocated inode    -1
        .links      0               0
        .ref    file layout    [-1,-1,...,-1]
        .dirty    TRUE           FALSE

  not used fields:
        .type   .ctime  .wtime  .opens  .links
        .w_pro  .r_pro  .fwd    .bck    .flock
  unchanged:
        .dev    .eof    .drn
*)

PROCEDURE alloc_file(f: FILE; VAR res: INTEGER);

  PROCEDURE undo;
  BEGIN
    cut_file0(f,0);     i_deall0(f^.dev,f^.inode);  clear_ref(f^.ref);
    f^.mode :={};       f^.state:={};               f^.inode:=-1
  END undo;

  VAR n: INTEGER;

BEGIN
  IF f^.eof<0 THEN f^.eof:=0 ELSIF f^.eof>MAXFILE THEN f^.eof:=MAXFILE END;
  n:=(f^.eof+4095) DIV 4096;
  clear_ref(f^.ref);
  f^. mode:={};  f^.inode:=-1;
  f^.state:={};  f^.host :=NIL;
  lock_super(f^.dev);
    i_alloc0(f^.dev,f^.inode);
    res:=f^.dev^.res;
    IF res#ok THEN f^.inode:=-1; unlock_super(f^.dev); RETURN END;
    IF n>0 THEN
      extend_file0(f,n);
      res:=f^.dev^.res;
      (* update_super has no need, because "undo" restore it correct *)
      IF res#ok THEN undo; unlock_super(f^.dev); RETURN END
    END;
    IF SYNC OR (f^.dev^.state*_sync#{}) THEN
      res:=update_super(f^.dev);
      IF res#ok THEN undo; n:=update_super(f^.dev) END
    ELSE
      os.send(update)
    END;
    IF res=ok THEN f^.state:=f^.state+dirty END;
  unlock_super(f^.dev)
END alloc_file;

(*
  "dealloc_file" procedure release
  resources was allocated to file;
    if no errors occure after it:
        .opens=0        .mode={ }       .inode=  -1
        .links=0        .eof = 0        .ref  = [-1,-1,...,-1]
      other fields unchanged
    if errors occured
        .ref    may be changed
        .mode   added tag {bad}
*)

PROCEDURE dealloc_file(f: FILE; VAR res: INTEGER);
BEGIN
  lock_super(f^.dev);
    cut_file0(f,0);
    res:=f^.dev^.res;
    i_deall0(f^.dev,f^.inode);
    IF SYNC OR (f^.dev^.state*_sync#{}) THEN
      f^.dev^.res:=update_super(f^.dev);
      IF res=ok THEN res:=f^.dev^.res END
    ELSE
      os.send(update)
    END;
  unlock_super(f^.dev);
  IF res=ok THEN
    f^.opens:=0; f^.mode:={ }; f^.inode:=-1;
    f^.links:=0; f^.eof := 0;  clear_ref(f^.ref)
  ELSE
    f^.state:=f^.state+bad
  END
END dealloc_file;

(*$>*)

PROCEDURE make_volume(direct: FILE;
                    VAL name: ARRAY OF CHAR;
                    VAL bads: ARRAY OF INTEGER
                    ): INTEGER;
  VAR   io: INTEGER;
       csh: cash_item;    dnod: dnode_ptr;
       adr: ADDRESS;    i,size: INTEGER;
       dev: PORT;       inodes: INTEGER;
       new: file_desc;    root: file_desc;
       bad: file_desc;    boot: file_desc;
      inoL: INTEGER;      inoH: INTEGER;

   PROCEDURE unlock;
   BEGIN
     os.DEALLOCATE(os.system,dev^.super,dev^.supsz); dev^.supsz:=0;
     unlock_super(dev)
   END unlock;

BEGIN
  dev:=direct^.dev;
  IF direct^.state*disk={} THEN RETURN err.not_blocked END;
  IF dev^.opens>1 THEN RETURN err.busy END;
  erase_cash(dev);
  io:=_get_spec(dev);
  IF io#ok THEN RETURN io END;
  size:=dpw2(dev^.secs,12-dev^.ssc);
  IF size<4 THEN RETURN err.bad_fsys END;
  inodes:=size - size DIV (4096 DIV 64) - 2;
  inodes:=(inodes+63) DIV 64 * 64;
  IF inodes <= 0 THEN RETURN err.bad_fsys END;
  lock_super(dev);
    new_super(dev,name,inodes,size);
    IF dev^.res#ok THEN unlock; RETURN dev^.res END;
    (* next blocks busy by booter, superblock, inodes & root dir *)
    FOR i:=0 TO dev^.inoH+1 DO
      EXCL(dev^.bset^[i DIV 32],i MOD 32) (* mark as busy *)
    END;
    (* next 3 inodes occupied by root,boot & bad *)
    FOR i:=0 TO 2 DO EXCL(dev^.iset^[0],i) END;
    FOR i:=0 TO dpw2(dev^.supsz,dev^.wssc)-1 DO
      INCL(dev^.dirty[i DIV 32],i MOD 32); dev^.updt:=TRUE
    END;
    dev^.res:=update_super(dev);
    inoL:=dev^.inoL;  inoH:=dev^.inoH;
  unlock;
  IF dev^.res#ok THEN RETURN dev^.res END;

  clear_ref(new.ref);
  new.mode :={ };       new.ctime:= 0;
  new.eof  := 0 ;       new.wtime:= 0;
  new.links:= 0 ;       new.pro  := 0;
  new.res1 := 0 ;       new.res2 := 0;

  root:=new;
  root.ref[0]:=inoH+1;
  root.mode :=dir;      root.ctime:=os.time;    root.eof:=64*3;
  root.links:= 1 ;      root.wtime:=os.time;

  boot:=new;
  boot.links:=1;        boot.ctime:=os.time;    boot.wtime:=os.time;

  bad :=new;
  bad.links:=1;         bad .ctime:=os.time;    bad .wtime:=os.time;

  FOR i:=inoL TO inoH DO
    cash_lock(dev,i,csh);
    low.move(csh^.buf,SYSTEM.ADR(new),16);
    low.move(csh^.buf+16,csh^.buf,63*16);
    IF i=inoL THEN
      low.move(csh^.buf+00,SYSTEM.ADR(root),16);
      low.move(csh^.buf+16,SYSTEM.ADR(boot),16);
      low.move(csh^.buf+32,SYSTEM.ADR(bad ),16)
    END;
    io:=cash_write(csh,0,4096,direct^.state*wait_wr#{});
    cash_unlock(csh);
    IF io#ok THEN RETURN io END
  END;
  cash_lock(dev,root.ref[0],csh);

  low._zero(csh^.buf,1024);
  dnod:=csh^.buf;
  dnod^.name:="..";          dnod^.inod:=0;  dnod^.kind:=d_dir+d_hidden;
  dnod:=csh^.buf+16;
  dnod^.name:="SYSTEM.BOOT"; dnod^.inod:=1;  dnod^.kind:=d_file+d_hidden+d_sys;
  dnod:=csh^.buf+32;
  dnod^.name:="BAD.BLOCKS";  dnod^.inod:=2;  dnod^.kind:=d_file+d_hidden+d_sys;
  io:=cash_write(csh,0,4096,direct^.state*wait_wr#{});
  cash_unlock(csh);
  RETURN io
END make_volume;

---------------------------  INODES  ---------------------------
                           ----------

PROCEDURE read_inode(f: FILE): INTEGER;
  VAR csh: cash_item; i,j,io: INTEGER;
BEGIN
  i:=f^.inode;
  j:=i DIV 64 + f^.dev^.inoL;
  IF (i<0) OR (j>f^.dev^.inoH) THEN RETURN err.bad_fsys END;
  cash_lock(f^.dev,j,csh);
  i:=i MOD 64;
  io:=cash_read(csh,(i+1)*64);
  IF io=ok THEN
    low.move(f,csh^.buf+i*16,16); f^.state:=f^.state-dirty
  ELSE
    clear_ref(f^.ref); f^.state:={}; f^.eof:=0; f^.mode:={}
  END;
  cash_unlock(csh);
  RETURN io
END read_inode;

PROCEDURE write_inode(f: FILE): INTEGER;
  VAR csh: cash_item;
   i,j,io: INTEGER;
BEGIN
  i:=f^.inode;
  i:=f^.inode;
  j:=i DIV 64 + f^.dev^.inoL;
  IF (i<0) OR (j>f^.dev^.inoH) THEN RETURN err.bad_fsys END;
  cash_lock(f^.dev,j,csh);
  i:=i MOD 64;
  io:=cash_read(csh,(i+1)*64); (* whole sector will be read *)
  IF io#ok THEN cash_unlock(csh); RETURN io END;
  low.move(csh^.buf+i*16,f,16);
  io:=cash_write_sec(csh,i*16,FALSE);
  IF io=ok THEN f^.state:=f^.state-dirty ELSE f^.state:=f^.state+bad END;
  cash_unlock(csh);
  RETURN io
END write_inode;

PROCEDURE index(f: FILE; b: INTEGER; VAR r: INTEGER; alloc: BOOLEAN): INTEGER;

  VAR alb: ARRAY [0..1] OF INTEGER;

  PROCEDURE alloc_blocks(n: INTEGER): INTEGER;
    VAR res: INTEGER;
  BEGIN
    lock_super(f^.dev);
      b_alloc0(f^.dev,0,n,alb);
      res:=f^.dev^.res;
      IF SYNC OR (f^.dev^.state*_sync#{}) THEN
        f^.dev^.res:=update_super(f^.dev);
        IF res=ok THEN res:=f^.dev^.res END
      ELSE
        os.send(update)
      END;
      IF res#ok THEN b_deall0(f^.dev,0,n,alb); alb[0]:=-1; alb[1]:=-1 END;
    unlock_super(f^.dev);
    RETURN res
  END alloc_blocks;

  PROCEDURE deall_blocks;
    VAR i: INTEGER;
  BEGIN
    lock_super(f^.dev);
    b_deall0(f^.dev,0,1+ORD(alb[1]>0),alb);
    (* 1+ORD(alb[1]>0) not nessesary may be simple 2, see: b_deall0 *)
    IF SYNC OR (f^.dev^.state*_sync#{}) THEN
      i:=update_super(f^.dev)
    ELSE
      os.send(update)
    END;
    unlock_super(f^.dev)
  END deall_blocks;

  VAR  csh: cash_item;
  x,io,len: INTEGER;
    sec,sc: INTEGER;
        rr: ref_ptr;
BEGIN
  IF b>=8*1024 THEN RETURN err.too_large END;
  alb[0]:=-1; alb[1]:=-1;
  r:=-1;
  IF (f^.mode*long={}) & (b>HIGH(f^.ref)) THEN
    IF alloc THEN extend_file(f,b*4096,io);
    ELSE          io:=err.no_data
    END;
    IF io#ok THEN RETURN io END;
  END;
  IF f^.mode*long={} THEN
    (* short file --> (b<=HIGH(f^.ref)) *)
    IF f^.ref[b]>0 THEN
      r:=f^.ref[b]; RETURN ok
    ELSIF alloc THEN
      io:=alloc_blocks(1);
      IF io=ok THEN
        r:=alb[0];   f^.ref[b]:=r;   f^.state:=f^.state+dirty
      END;
      RETURN io
    ELSE
      RETURN err.no_data
    END
  END;

  (* long file *)
  x:=b DIV 1024; b:=b MOD 1024;
  IF f^.ref[x]<=0 THEN (* we must alloc 2 blocks! *)
    IF NOT alloc THEN RETURN err.no_data END;
    io:=alloc_blocks(2);
    IF io#ok THEN RETURN io END;
    f^.ref[x]:=alb[0];
    cash_lock(f^.dev,f^.ref[x],csh);
    rr:=csh^.buf;    clear_ref(rr^);
    rr^[b]:=alb[1];
    io:=cash_write(csh,0,4096,f^.state*wait_wr#{});
    cash_unlock(csh);
    IF io=ok THEN
      r:=alb[1];     f^.state:=f^.state+dirty
    ELSE
      deall_blocks;  f^.ref[x]:=-1
    END;
    RETURN io
  END;

  cash_lock(f^.dev,f^.ref[x],csh);
  io:=cash_read(csh,(b+1)*4); (* whole sector will be read *)
  IF io#ok THEN cash_unlock(csh); RETURN io END;
  rr:=csh^.buf;
  IF rr^[b]>0 THEN r:=rr^[b]; cash_unlock(csh); RETURN ok END;

  (* rr^[b]<=0 : we must alloc 1 block *)
  IF NOT alloc THEN RETURN err.no_data END;
  io:=alloc_blocks(1);
  IF io#ok THEN cash_unlock(csh); RETURN io END;
  rr^[b]:=alb[0];
  io:=cash_write_sec(csh,b,f^.state*wait_wr#{});
  IF io=ok THEN r:=alb[0] ELSE rr^[b]:=-1; deall_blocks; END;
  cash_unlock(csh);
  RETURN io
END index;

----------------------  DIRECTORY NODES  -----------------------
                      -------------------

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
  IF HIGH(name)<HIGH(str32) THEN RETURN err.bad_name END;
  IF hash_name(name,str)<0  THEN RETURN err.bad_name END;
  RETURN ok
END copy_fname;

PROCEDURE cash_lock_and_read(d: FILE; b: INTEGER; VAR csh: cash_item): INTEGER;
  VAR r,io: INTEGER;
BEGIN
  io:=index(d,(b),r,FALSE);
  IF io#ok THEN RETURN io END;
  cash_lock(d^.dev,r,csh);
  io:=cash_read(csh,4096);
  IF io#ok THEN cash_unlock(csh) END;
  RETURN io
END cash_lock_and_read;

PROCEDURE find(d: FILE; VAL fname: ARRAY OF CHAR;
                        VAR inode: INTEGER;
                        VAR  kind: BITSET): INTEGER;
  VAR io: INTEGER;
       k: BITSET;
     i,h: INTEGER;
       b: INTEGER;
     csh: cash_item;
    name: str32;
    dnod: dnode_ptr;
   count: INTEGER;
     lim: INTEGER;
BEGIN
  h:=hash_name(name,fname);
  IF h<0 THEN RETURN err.bad_name END;
  IF d^.eof MOD 64 # 0 THEN RETURN err.bad_fsys END;
  count:=d^.eof DIV 64;
  b:=0; kind:={}; inode:=-1;
  WHILE count>0 DO
    io:=cash_lock_and_read(d,b,csh);
    IF io#ok THEN RETURN io END;
    IF count>64 THEN
      lim:=64
    ELSE
      lim:=count;
      IF h>=lim THEN h:=0 END
    END;
    i:=h;
    REPEAT
      dnod:=csh^.buf+i*16; k:=dnod^.kind;
      IF (k*d_del={}) & (k*d_entry#{}) & (name=dnod^.name) THEN
        kind:=k; inode:=dnod^.inod;
        cash_unlock(csh); RETURN ok
      END;
      IF i=lim-1 THEN i:=0 ELSE i:=i+1 END
    UNTIL i=h;
    cash_unlock(csh);
    DEC(count,lim); INC(b);
  END;
  RETURN err.no_entry
END find;

----------------------------------------------------------------

VAR MAGIC: INTEGER;

CONST _MAGIC = 454C4946h; (* "FILE" *)

PROCEDURE check_p_stack(size: INTEGER);
CODE cod.copt cod.alloc cod.drop cod.decs END check_p_stack;
(* if "S register" + "size" > "H register" then TRAP(40h) *)

----------------------------------------------------------------

VAR Flist: FILE;
   Flock: os.signal_rec;

(* "tie_file", "_open", "destroy" may be *)
(* called ONLY when "Flist" is locked      *)

(* NB.new_desc: it is no need to lock "f", when "f"   *)
(*              is new and "Flist" is locked;         *)
(*              internal operations don't check magic *)
(*              so, may be called before MAGIC set    *)

(* NB.deadlock: some procedures ("close" for example) *)
(*              lock file & "Flist" together; to      *)
(*              prevent deadlock Flist locked first   *)

PROCEDURE tie_file(f: FILE);
BEGIN
  f^.magic:=MAGIC;
  f^.opens:=1;
  (* NB: Only primary "_open" increments "port^.opens" *)
  INC(f^.dev^.opens);
  (* include "f" in "Flist": *)

  IF Flist=NIL THEN
    f^.fwd:=f;                  f^.bck:=f;
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

PROCEDURE _open(VAR f: FILE; dev: PORT; inode: INTEGER; read_i: BOOLEAN
               ): INTEGER;
  VAR l: FILE;
     io: INTEGER;
BEGIN
  l:=Flist;
  IF l#NIL THEN
    LOOP
      IF (set(l^.dev=dev)*set(l^.inode=inode)#{}) THEN
        IF l^.state*alias#{} THEN f:=FILE(l^.ref[0]) ELSE f:=l END;
        INC(f^.opens); RETURN ok
      END;
      l:=l^.fwd;
      IF l=Flist THEN l:=NIL; EXIT END;
    END
  END;
  os.ALLOCATE(os.system,f,SIZE(file_desc));
  IF f=NIL THEN RETURN err.no_memory END;
  f^.dev:=dev;  f^.inode:=inode; f^.mode:={}; f^.state:={}; f^.magic:=0;
  f^.host:=f;
  IF read_i THEN
    io:=read_inode(f);
    IF io#ok THEN
      os.DEALLOCATE(os.system,f,SIZE(file_desc)); f:=NIL; RETURN io
    END;
  END;
  tie_file(f);
  RETURN ok
END _open;

PROCEDURE destroy(f: FILE);
  VAR dev: INTEGER;
BEGIN
  IF  f^.dev^.opens>0 THEN DEC(f^.dev^.opens) ELSE f^.dev^.opens:=0 END;
  IF  f^.dev^.opens=0 THEN erase_cash(f^.dev) END;
  (* delete out of "Flist" *)
  IF Flist=f THEN Flist:=Flist^.fwd END;
  IF Flist=f THEN Flist:=NIL
  ELSE
    f^.fwd^.bck:=f^.bck;
    f^.bck^.fwd:=f^.fwd
  END;
  f^.magic:=0;
  os.DEALLOCATE(os.system,f,SIZE(file_desc));
END destroy;

PROCEDURE _open_dev(VAR f: FILE; dev: PORT; state: BITSET): INTEGER;
  VAR r: INTEGER;
BEGIN
  r:=_open(f,dev,-1,FALSE);
  IF r#ok THEN f:=NIL; RETURN r END;
  f^.state:=EXTERNAL*state;
  f^.links:=999; (* to suppress dealloc_file in close *)
  f^.eof  :=0;
  IF    dev^.mode=disk THEN f^.eof:=mpw2(dev^.secs,dev^.ssc);
                            f^.state:=f^.state+disk
  ELSIF dev^.mode=tty  THEN f^.state:=f^.state+tty
  ELSIF dev^.mode=spec THEN f^.state:=f^.state+spec
  ELSE
    RETURN err.bad_fsys
  END;
  RETURN ok
END _open_dev;

PROCEDURE open_dev(VAR f: FILE;
                   VAL name: ARRAY OF CHAR; state: BITSET): INTEGER;
  VAR dev: PORT;
      res: INTEGER;
BEGIN
  check_p_stack(128);
  os.wait(Flock);
    res:=open_driver(name,dev);
    IF res=ok THEN res:=_open_dev(f,dev,state) END;
    close_driver(dev);
  os.send(Flock);
  RETURN res
END open_dev;

PROCEDURE _root(VAR f: FILE; VAL dev: PORT; state: BITSET): INTEGER;
  VAR res: INTEGER;
BEGIN
  IF dev^.mode#disk THEN RETURN err.not_blocked END;
  IF dev^.opens<=1  THEN
    lock_super(dev);
    mount_super(dev,"");
    res:=dev^.res;
    unlock_super(dev);
    IF res#ok THEN RETURN res END
  END;
  res:=_open(f,dev,0,TRUE);
  IF res#ok THEN RETURN res END;
  IF f^.mode*dir={} THEN
    res:=close(f);  RETURN err.not_dir
  ELSE
    f^.state:=f^.state+EXTERNAL*state; RETURN res
  END
END _root;

PROCEDURE _open_root;
  VAR res: INTEGER; dev: PORT;
BEGIN
  res:=open_driver(ROOT,dev);
  IF res#ok THEN RETURN END;
  res:=_root(root,dev,{});
  close_driver(dev)
END _open_root;

PROCEDURE open(d: FILE; VAR f: FILE; VAL name: ARRAY OF CHAR;
                                        state: BITSET): INTEGER;
  VAR x: FILE;
    res: INTEGER;         clo: INTEGER;
   kind: BITSET;        inode: INTEGER;

  PROCEDURE esc;
    VAR dvc: POINTER TO str32;  dev: PORT;
  BEGIN
    x:=f; f:=NIL;
    dvc:=SYSTEM.ADR(x^.ref);
    res:=open_driver(dvc^,dev);
    IF res#ok THEN RETURN END;
    IF (dev^.mode=disk) & (fsys*state#{}) THEN
      res:=_root(f,dev,state)
    ELSE
      res:=_open_dev(f,dev,state)
    END;
    close_driver(dev);
    IF res#ok THEN RETURN END;
    f^.pro  :=x^.pro;       f^.ctime:=x^.ctime;
    f^.links:=x^.links;     f^.wtime:=x^.wtime
  END esc;

  VAR dev: PORT;

BEGIN
  IF (root=NIL) & (ROOT#"") THEN _open_root END;
  check_p_stack(128);
  IF (d=NIL) & (fsys*state#{}) THEN
    res:=open_driver(name,dev);
    IF res#ok THEN RETURN res END;
    os.wait(Flock);
      res:=_root(f,dev,state);
    os.send(Flock);
    close_driver(dev);
    RETURN res
  END;
  IF (d=NIL) OR (d^.magic#MAGIC) OR (d^.mode*dir={}) THEN
    RETURN err.not_dir
  END;
  x:=NIL;
  os.wait(Flock);
    os.wait(d^.flock);
      res:=find(d,name,inode,kind);
    os.send(d^.flock);
    IF res=ok THEN res:=_open(f,d^.dev,inode,TRUE) END;
    IF (res=ok) & (f^.mode*escape#{}) THEN
      esc
    ELSE
      state:=state-fsys
    END;
  os.send(Flock);
  IF res#ok THEN
    f:=NIL
  ELSE
    IF d_hidden*kind#{} THEN f^.state:=f^.state+hidden END;
    f^.state:=f^.state+EXTERNAL*state
  END;
  IF x#NIL THEN
    clo:=close(x);
    IF res=ok THEN res:=clo END;
  END;
  IF (res=ok) & (d^.inode=0) & (name="..") THEN
    x:=f^.host; INC(x^.opens); res:=close(f); f:=x
  END;
  IF (res#ok) & (f#NIL) THEN clo:=close(f) END;
  RETURN res
END open;

PROCEDURE create(d: FILE; VAR f: FILE; len: INTEGER; state: BITSET): INTEGER;
  VAR res: INTEGER;
BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) OR (d^.mode*dir={}) THEN
    RETURN err.not_dir
  END;
  IF d^.dev^.state*_wpro#{} THEN RETURN err.write_pro END;
  os.lock;
  os.ALLOCATE(os.system,f,SIZE(file_desc));
  IF f=NIL THEN os.unlock; RETURN err.no_memory END;
  f^.dev:=d^.dev; (* see NB.new_desc *)
  f^.host:=f;
  IF len<=0 THEN f^.eof:=0 ELSE f^.eof:=len END;
  alloc_file(f,res);
  IF res#ok THEN
    os.DEALLOCATE(os.system,f,SIZE(file_desc)); f:=NIL;
  ELSE
    f^.pro  :=0;
    f^.eof  :=0;  f^.ctime:=os.time;
    f^.links:=0;  f^.wtime:=os.time;
    f^.res1 :=0;  f^.res2 :=0;
    f^.state:=f^.state+EXTERNAL*state;
    os.wait(Flock);
      tie_file(f);
    os.send(Flock)
  END;
  os.unlock;
  RETURN res
END create;

PROCEDURE make_node(d: FILE; VAR f: FILE; VAL ref: ARRAY OF CHAR): INTEGER;
  VAR res: INTEGER;
      ptr: POINTER TO str32;
BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc  END;
  IF  d^.mode*dir={}             THEN RETURN err.not_dir   END;
  IF  d^.dev^.state*_wpro#{}     THEN RETURN err.write_pro END;
  os.lock;
    os.ALLOCATE(os.system,f,SIZE(file_desc));
    IF f=NIL THEN os.unlock; RETURN err.no_memory END;
    f^.dev:=d^.dev; f^.eof:=0;  f^.host:=f;
    alloc_file(f,res);
    IF res#ok THEN
      os.DEALLOCATE(os.system,f,SIZE(file_desc)); f:=NIL;
      os.unlock; RETURN res
    END;         ------
    f^.state:={}; f^.mode :=escape;
    f^.pro  :=0;  f^.ctime:=os.time;
    f^.links:=0;  f^.wtime:=os.time;
    f^.res1 :=0;  f^.res2 :=0;
    ptr:=SYSTEM.ADR(f^.ref);    str.copy(ptr^,ref);
    os.wait(Flock);
      tie_file(f);
    os.send(Flock);
  os.unlock;
  RETURN res
END make_node;

PROCEDURE make_dir(VAR f: FILE; where: FILE): INTEGER;
  VAR res: INTEGER;
    dnode: dnode_ptr;
      csh: cash_item;
        r: INTEGER;
BEGIN
  check_p_stack(128+16); (* +16 for 'create' satisfaction *)
  IF (where=NIL) OR (where^.magic#MAGIC) THEN
    RETURN err.bad_desc
  END;
  IF where^.mode*dir={}         THEN RETURN err.not_dir END;
  IF where^.dev^.state*_wpro#{} THEN RETURN err.write_pro END;
  os.lock;
  res:=create((where),f,64,{});
  IF res=ok THEN
    f^.mode :=f^.mode+dir;
    f^.eof:=64;
    f^.state:=f^.state+dirty;
    res:=index(f,0,r,FALSE);
    IF res=ok THEN
      cash_lock(f^.dev,r,csh);
      low._zero(csh^.buf,1024);
      dnode:=csh^.buf;
      dnode^.name:="..";
      dnode^.inod:=where^.inode;
      dnode^.kind:=d_dir+d_hidden;
      res:=cash_write(csh,0,4096,where^.state*wait_wr#{});
      cash_unlock(csh);
      IF res=ok THEN res:=write_inode(f) END
    END;
    IF res#ok THEN dealloc_file(f,r); destroy(f); f:=NIL END
  ELSE
    f:=NIL
  END;
  os.unlock;
  RETURN res
END make_dir;

PROCEDURE move_dir(from: FILE; VAL fname: ARRAY OF CHAR;
                     to: FILE; VAL tname: ARRAY OF CHAR; h: BOOLEAN): INTEGER;
  VAR f: FILE;
      r: INTEGER;
    res: INTEGER;
    csh: cash_item;
  dnode: dnode_ptr;
BEGIN
  check_p_stack(128+128);
  IF (from=NIL) OR (from^.magic#MAGIC) OR (from^.mode*dir={})
  OR (to  =NIL) OR (to^  .magic#MAGIC) OR (to  ^.mode*dir={}) THEN
    RETURN err.not_dir
  END;
  IF from^.dev#to^.dev               THEN RETURN err.xcross END;
  IF to^.dev^.state*_wpro#{}         THEN RETURN err.write_pro END;
  IF (HIGH(fname)>=0) & (fname="..") THEN RETURN err.bad_name  END;
  IF (from=to) & (fname=tname) THEN (* hidden can't be changed by such way *)
    RETURN ok
  END;
  os.lock;
  res:=open((from),f,fname,{});
  IF res#ok THEN os.unlock; RETURN res END;
  IF f^.mode*dir={} THEN    ------
    r:=close(f); os.unlock; RETURN err.not_dir
  END;                      ------
  res:=link(to,f,tname,h);
  IF res#ok THEN
    r:=close(f); os.unlock; RETURN res
  END;                      ------
  res:=unlink(from,fname);
  IF res#ok THEN
    r:=unlink(to,tname); r:=close(f); os.unlock; RETURN res
  END;                                           ------
  os.wait(Flock);
  os.wait(f^.flock);
  res:=index(f,0,r,FALSE);
  IF res=ok THEN
    cash_lock(f^.dev,r,csh);
    dnode:=csh^.buf;
    res:=cash_read(csh,64);
    IF res=ok THEN
      IF (dnode^.name#"..") OR (dnode^.inod#from^.inode) THEN
        res:=err.bad_fsys
      ELSE
        dnode^.inod:=to^.inode;
        res:=cash_write_sec(csh,0,(from^.state+to^.state)*wait_wr#{})
      END
    END
  END;
  os.send(f^.flock);
  os.send(Flock);
  IF res#ok THEN r:=unlink(to,tname); r:=link(from,f,fname,h) END;
  r:=close(f);
  IF res=ok THEN res:=r END;
  os.unlock;
  RETURN res
END move_dir;

PROCEDURE close(f: FILE): INTEGER;
  VAR res,dea: INTEGER;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  os.wait(Flock);
  os.wait(f^.flock);
  res:=ok;
  IF f^.state*(special+dirty)=dirty THEN res:=write_inode(f) END;
  IF f^.opens>0 THEN DEC(f^.opens) ELSE f^.opens:=0 END;
  IF (f^.opens+f^.links=0) & (f^.state*(special+alias)={})
   & (f^.mode*escape={})
  THEN
    (* NB: inode with .links=0 already writen on disk! *)
    dealloc_file(f,dea);
    IF res=ok THEN res:=dea END
  END;
  IF (res=ok) & (f^.opens=0) THEN
    destroy(f); os.unlock
  ELSE
    os.send(f^.flock)
  END;
  os.send(Flock);
  RETURN res
END close;

PROCEDURE mount(d: FILE; VAL name: ARRAY OF CHAR;  v: FILE;
                         VAL info: ARRAY OF CHAR;
                         VAR  lab: ARRAY OF CHAR; ro: BOOLEAN): INTEGER;
  VAR res,ignore: INTEGER; f,r,h: FILE; save: BITSET;
BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF (v=NIL) OR (v^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF d^.mode*dir={}              THEN RETURN err.not_dir    END;
  IF v^.state*disk={}            THEN RETURN err.unsuitable END;
  os.lock;
    IF v^.dev^.mode#disk THEN os.unlock; RETURN err.not_blocked END;
    os.wait(Flock);                      ------
      IF v^.dev^.opens#1 THEN res:=err.busy
      ELSE
        lock_super (v^.dev);
        save:=v^.dev^.state;
        v^.dev^.state:=v^.dev^.state-_wpro-_sync;
        mount_super(v^.dev,info);
        res:=v^.dev^.res;
        unlock_super(v^.dev)
      END;
    os.send(Flock);
    IF res#ok THEN v^.dev^.state:=save; os.unlock; RETURN res END;
    res:=open((d),f,name,{});                      ------
    IF res=ok THEN
      IF    f^.mode *dir={}  THEN ignore:=close(f); res:=err.not_dir
      ELSIF f^.state*fsys#{} THEN ignore:=close(f); res:=err.bad_desc
      ELSIF f^.dev#d^.dev    THEN ignore:=close(f); res:=err.xcross
      ELSIF f^.opens>1       THEN ignore:=close(f); res:=err.busy
      END
    END;
    IF res#ok THEN os.unlock; RETURN res END;
    res:=open((f),h,"..",{}); ------
    IF (res=ok) & ((h^.mode*dir={}) OR (h#d)) THEN
      ignore:=close(h); res:=err.bad_fsys
    END;
    IF res#ok THEN ignore:=close(f); os.unlock; RETURN res END;
    os.wait(Flock);                           ------
    os.wait(f^.flock);
      IF (f^.opens>1) OR (v^.dev^.opens>1) THEN
        res:=err.busy
      ELSE
        res:=_open(r,v^.dev,0,TRUE);
        IF (res=ok) & (r^.mode*dir={}) THEN
          destroy(r); res:=err.not_dir
        ELSIF res=ok THEN (* see NB.new_desc *)
          f^.state:=f^.state+alias; f^.ref[0]:=INTEGER(r); r^.host:=h
        END
      END;
    os.send(f^.flock);
    os.send(Flock);
    IF res#ok THEN
      ignore:=close(f)
    ELSE
      str.copy(lab,v^.dev^.vol^);
      IF ro  THEN v^.dev^.state:=v^.dev^.state+_wpro END;
    END;
  os.unlock;
  RETURN res
END mount;

PROCEDURE unmount(d: FILE; name: ARRAY OF CHAR; flush: INTEGER): INTEGER;
  VAR dev: PORT;
     kind: BITSET;
    l,f,x: FILE;
    res,i: INTEGER;
BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) OR (d^.mode*dir={}) THEN
    RETURN err.not_dir
  END;
  os.wait(Flock);
    os.wait(d^.flock);
      res:=find(d,name,i,kind);
    os.send(d^.flock);
    IF res#ok THEN os.send(Flock); RETURN res END;
    dev:=d^.dev;                   ------
    l:=Flist;
    WHILE (l#NIL) & ((l^.dev#dev) OR (l^.inode#i)) DO
      l:=l^.fwd;
      IF l=Flist THEN l:=NIL END
    END;
    IF (l=NIL) OR (l^.state*alias={}) THEN
      os.send(Flock); RETURN err.undef
    END;              ------
    f:=FILE(l^.ref[0]);
    x:=Flist;
    LOOP
      IF x=NIL THEN EXIT END;
      IF (x#f) & (x^.state*bad#{}) & (x^.dev=f^.dev) THEN
        IF flush=0 THEN os.send(Flock);    RETURN err.busy END;
        IF flush=1 THEN res:=err.busy END; ------
        destroy(x); (* Flist changed so: *)
        x:=Flist
      ELSE
        x:=x^.fwd;
        IF x=Flist THEN EXIT END
      END
    END;

    os.wait(f^.flock);
     IF (l^.opens>1) OR (f^.opens>1) OR (f^.dev^.opens>1) THEN
       res:=err.busy; os.send(f^.flock)
     ELSIF (f^.opens=1) & (f^.dev^.opens=1) THEN
       unmount_super(f^.dev);
       DEC(f^.host^.opens);
       f^.dev^.opens:=1;
       destroy(f); destroy(l); os.unlock (* because f^.flock destroed *)
     ELSE
       res:=err.busy; os.send(f^.flock)
     END;
  os.send(Flock);
  RETURN res
END unmount;

PROCEDURE flush(f: FILE): INTEGER;
  VAR res: INTEGER;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  res:=ok;
  os.wait(f^.flock);
    IF (f^.state*special={}) & (f^.state*dirty#{}) THEN
      res:=write_inode(f)
    END;
  os.send(f^.flock);
  RETURN res
END flush;

PROCEDURE reopen(f: FILE): INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  INC(f^.opens); RETURN ok
END reopen;

(* NB: link_unlink:                                        *)
(*     'read_inode' always executing in range of 'Flock' *)
(*     INC(.links)  always executing in range of 'Flock' *)
(*     so 'not empty directory' check is correct!          *)

PROCEDURE unlink(d: FILE; VAL fname: ARRAY OF CHAR): INTEGER;

  VAR UPDATE: BOOLEAN;

  PROCEDURE delete(VAR f: FILE; hash: INTEGER; VAL name: str32): INTEGER;
    VAR k: BITSET;
      i,b: INTEGER;            count: INTEGER;
      csh: cash_item;          lim,n: INTEGER;
     dnod: dnode_ptr;        io,last: INTEGER;
  BEGIN
    b:=0;       last:=0;
    f:=NIL;
    count:=d^.eof DIV 64;
    WHILE count>0 DO
      io:=cash_lock_and_read(d,b,csh);
      IF io#ok THEN RETURN io END;
      IF count>64 THEN
        lim:=64
      ELSE
        lim:=count;
        IF hash>=lim THEN hash:=0 END
      END;
      DEC(count,lim);
      i:=hash;
      REPEAT
        dnod:=csh^.buf+i*16; k:=dnod^.kind;
        IF (k*d_del={}) & (k*d_entry#{}) THEN
          IF name#dnod^.name THEN
            n:=b*64+i;
            IF last<n THEN last:=n END;
          ELSE
            io:=_open(f,d^.dev,dnod^.inod,TRUE);
            IF io#ok THEN cash_unlock(csh); f:=NIL; RETURN io END;
            IF (f^.mode*dir#{}) & (f^.eof>64) & (f^.links=1) THEN
              cash_unlock(csh);
              RETURN err.is_dir
            END;
            dnod^.kind:=dnod^.kind+d_del;
            dnod^.name[31]:=dnod^.name[0]; dnod^.name[0]:=0c;
            io:=cash_write_sec(csh,i*16,d^.state*wait_wr#{});
            IF io#ok THEN cash_unlock(csh); RETURN io END;
            IF f^.links>0 THEN DEC(f^.links) ELSE f^.links:=0 END;
            f^.state:=f^.state+dirty;
            d^.state:=d^.state+dirty;
            IF (count>0) OR (i#lim-1) THEN cash_unlock(csh); RETURN io END;
            LOOP
              DEC(i);
              IF i<0 THEN EXIT END;
              dnod:=csh^.buf+i*16; k:=dnod^.kind;
              IF (k*d_del={}) & (k*d_entry#{}) THEN
                last:=b*64+i; EXIT
              END
            END;
            UPDATE:=(d^.eof#(last+1)*64);
            d^.eof:=(last+1)*64;
            cash_unlock(csh);
            RETURN io
          END;
        END;
        IF i=lim-1 THEN i:=0 ELSE i:=i+1 END
      UNTIL i=hash;
      cash_unlock(csh);
      INC(b)
    END;
    RETURN err.no_entry
  END delete;

  VAR f: FILE;
    res: INTEGER;       clo: INTEGER;
   name: str32;         hash: INTEGER;

BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) OR (d^.mode*dir={}) THEN
    RETURN err.not_dir
  END;
  IF (name="..") OR (name="."0c) THEN RETURN err.is_dir    END;
  IF d^.dev^.state*_wpro#{}      THEN RETURN err.write_pro END;
  hash:=hash_name(name,fname);
  IF hash<0 THEN RETURN err.bad_name END;
  os.lock;
    os.wait(Flock);
    os.wait(d^.flock);
     UPDATE:=FALSE;
     res:=delete(f,hash,name);
     IF (res=ok) & (d^.state*dirty#{}) THEN
       d^.wtime:=os.time;
       IF UPDATE THEN res:=write_inode(d) END
     END;
    os.send(d^.flock);
    os.send(Flock);
    IF f#NIL THEN
      IF f^.mode*escape#{} THEN low.fill(f^.ref,-1); f^.mode:={}; f^.eof:=0 END;
      clo:=close(f);
      IF res=ok THEN res:=clo END
    END;
  os.unlock;
  RETURN res
END unlink;

PROCEDURE link(d,f: FILE; VAL fname: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;

  VAR UPDATE: BOOLEAN;  (* d's i-node must be update *)
      ins_b: INTEGER;
      ins_p: INTEGER;
    old_eof: INTEGER;
   inserted: BOOLEAN;

  PROCEDURE delete_inserted;
    VAR io: INTEGER;
       csh: cash_item;
      dnod: dnode_ptr;
  BEGIN
    io:=cash_lock_and_read(d,ins_b,csh);
    IF io=ok THEN
      dnod:=csh^.buf+ins_p*16;
      WITH dnod^ DO kind:=kind+d_del; name[31]:=name[0]; name[0]:=0c END;
      io:=cash_write_sec(csh,ins_p*16,d^.state*wait_wr#{});
      cash_unlock(csh);
    END;
    UPDATE:=FALSE;
    d^.eof:=old_eof
  END delete_inserted;

  PROCEDURE insert(VAR del: FILE; hash0: INTEGER;
                   VAL name: str32; kind: BITSET): INTEGER;
    VAR io: INTEGER;
       csh: cash_item;        count,lim: INTEGER;
      dnod: dnode_ptr;      hash,last,n: INTEGER;
     i,b,r: INTEGER;                  k: BITSET;
  BEGIN
    del:=NIL;
    b:=0;
    last:=0;
    hash:=hash0;
    count:=d^.eof DIV 64;
    WHILE count>0 DO
      io:=cash_lock_and_read(d,b,csh);
      IF io#ok THEN RETURN io END;
      IF    count>=64   THEN lim:=64
      ELSIF hash<=count THEN lim:=count+1
      ELSE                   lim:=hash +1
      END;
      DEC(count,lim);
      i:=hash;
      REPEAT
        dnod:=csh^.buf+i*16; k:=dnod^.kind;
        IF (k*d_del={}) & (k*d_entry#{}) THEN
          IF name=dnod^.name THEN
            IF del#NIL THEN cash_unlock(csh); RETURN err.bad_fsys END;
            io:=_open(del,d^.dev,dnod^.inod,TRUE);
            IF io#ok THEN cash_unlock(csh); del:=NIL; RETURN io END;
            IF (del^.mode*dir={}) # (k*d_dir={}) THEN
              cash_unlock(csh); RETURN err.bad_fsys
            END;
            IF (del^.mode*dir#{}) & (del^.eof>64) & (del^.links=1) THEN
              cash_unlock(csh); RETURN err.is_dir
            END;
            IF inserted THEN
              dnod^.kind:=dnod^.kind+d_del;
              dnod^.name[31]:=dnod^.name[0]; dnod^.name[0]:=0c
            ELSE
              dnod^.inod:=f^.inode; dnod^.kind:=kind
            END;
            io:=cash_write_sec(csh,i*16,d^.state*wait_wr#{});
            IF io#ok THEN cash_unlock(csh); RETURN io END;
            IF del^.links>0 THEN DEC(del^.links) ELSE del^.links:=0 END;
            del^.state:=del^.state+dirty;
            f^.state:=f^.state+dirty;
            d^.state:=d^.state+dirty;
            IF (NOT inserted) OR (count>0) OR (i#lim-1) THEN
              cash_unlock(csh); RETURN io
            END;
            LOOP
              DEC(i);
              IF i<0 THEN EXIT END;
              dnod:=csh^.buf+i*16; k:=dnod^.kind;
              IF (k*d_del={}) & (k*d_entry#{}) THEN
                last:=b*64+i; EXIT
              END
            END;
            cash_unlock(csh);
            UPDATE:=(d^.eof#(last+1)*64);
            d^.eof:=(last+1)*64;
            RETURN io
          ELSE
            n:=b*64+i;
            IF last<n THEN last:=n END
          END
        ELSIF NOT inserted & ( (k*d_del#{}) OR (k={}) ) THEN
          dnod^.kind:=kind;
          dnod^.inod:=f^.inode;
          str.copy(dnod^.name,name);
          io:=cash_write_sec(csh,i*16,d^.state*wait_wr#{});
          IF io#ok THEN cash_unlock(csh); RETURN io END;
          f^.state:=f^.state+dirty;
          d^.state:=d^.state+dirty;
          inserted:=TRUE;
          ins_b:=b; ins_p:=i;
          n:=b*4096+(i+1)*64; old_eof:=d^.eof;
          UPDATE:=(old_eof<n);
          IF UPDATE THEN d^.eof:=n END
        END;
        IF i=lim-1 THEN i:=0 ELSE i:=i+1 END
      UNTIL i=hash;
      cash_unlock(csh);
      INC(b)
    END;
    IF inserted THEN RETURN ok END;
    (* extend directory: *)
    IF d^.eof MOD 4096 # 0 THEN RETURN err.bad_fsys END;
    b:=d^.eof DIV 4096;
    io:=index(d,b,r,TRUE);
    IF io#ok THEN RETURN io END;
    cash_lock(d^.dev,r,csh);
    low._zero(csh^.buf,1024);
    dnod:=csh^.buf+hash0*16;
    dnod^.inod:=f^.inode; str.copy(dnod^.name,name);
    dnod^.kind:=kind;
    io:=cash_write(csh,0,4096,d^.state*wait_wr#{});
    cash_unlock(csh);
    IF io#ok THEN RETURN io END;
    INC(d^.eof,(hash0+1)*64);
    UPDATE:=TRUE;
    d^.state:=d^.state+dirty;
    f^.state:=f^.state+dirty;
    RETURN io
  END insert;

  VAR del: FILE;         clo: INTEGER;
      res: INTEGER;     hash: INTEGER;
     name: str32;       kind: BITSET;

BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF  d^.mode*dir={}             THEN RETURN err.not_dir    END;
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF f^.dev#d^.dev               THEN RETURN err.xcross     END;
  IF (name="..") OR (name="."0c) THEN RETURN err.is_dir     END;
  IF d^.dev^.state*_wpro#{}      THEN RETURN err.write_pro  END;
  IF f^.state*special#{}         THEN RETURN err.unsuitable END;
  hash:=hash_name(name,fname);
  IF hash<0 THEN RETURN err.bad_name END;

  IF    f^.mode*dir#{}    THEN kind:=d_dir
  ELSIF f^.mode*escape#{} THEN kind:=d_esc
  ELSE                         kind:=d_file
  END;
  IF hid THEN kind:=kind+d_hidden END;

  UPDATE  :=FALSE;
  inserted:=FALSE;
  os.lock;
    os.wait(Flock);
      os.wait(d^.flock);
        res:=insert(del,hash,name,kind);
        IF     res=ok  THEN INC(f^.links)
        ELSIF inserted THEN delete_inserted
        END;
      os.send(d^.flock);
    os.send(Flock);
    IF del#NIL THEN
      clo:=close(del);
      IF res=ok THEN res:=clo END
    END;
    IF (res=ok) & (d^.state*dirty#{}) THEN
      d^.wtime:=os.time;
      IF UPDATE THEN res:=write_inode(d) END
    END;
    IF (res=ok) & (f^.state*dirty#{}) THEN res:=write_inode(f) END;
  os.unlock;
  RETURN res
END link;

(*
   NOTE: s_read & s_write & doio don't enter dev^.lock^!
   If s_read enter f^.dev^.lock^ then no one
   can open this serial file until operation done!!!
   It may be very bad for keyboard (thinking about "ls /dev/key0 -l")
*)

PROCEDURE doio(f: FILE; VAR r: REQUEST): INTEGER;
  VAR dev: PORT;
     lock: BOOLEAN;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF  f^.state*special={}        THEN RETURN err.unsuitable END;
  dev:=f^.dev;
  r.drn:=dev^.drn;  r.res:=ok;
  lock:=(r.op#req.GET_SPEC) & (r.op#req.READY);
  IF r.op=req.POWER_OFF THEN erase_cash(dev) END;
  IF lock THEN os.wait(dev^.lock^) END;
    dev^.doio(r);
  IF lock THEN os.send(dev^.lock^) END;
  RETURN r.res
END doio;

PROCEDURE s_ready(f: FILE; VAR len: INTEGER): INTEGER;
  VAR r: REQUEST;
BEGIN
  r.op:=req.READY; r.res:=ok;
  r.pos:=0;        r.buf:=NIL;
  r.ofs:=0;        r.len:=0;
  r.drn:=f^.dev^.drn;
  r.res:=os.wait_del(-1,f^.dev^.lock^);
    IF r.res#0 THEN
      IF r.res<0 THEN RETURN err.time_out ELSE RETURN err.ipted_op END
    END;
    f^.dev^.doio(r);
  os.send(f^.dev^.lock^);
  len:=r.len;
  RETURN r.res
END s_ready;

PROCEDURE s_read(f: FILE; fpos: INTEGER;     buf: ADDRESS;
                          bpos: INTEGER; VAR len: INTEGER): INTEGER;
  VAR r: REQUEST; i: INTEGER;
BEGIN
  r.res:=ok;
  IF f^.state*nodelay#{} THEN
    r.res:=s_ready(f,i);
    IF i<len THEN i:=len END;
    IF i=0   THEN RETURN ok END
  END;
  r.op:=req.READ;
  r.pos:=bpos;      r.buf:=buf;
  r.ofs:=fpos;      r.len:=len;
  r.drn:=f^.dev^.drn;
  r.res:=os.wait_del(-1,f^.dev^.lock^);
  IF r.res#0 THEN
    IF r.res<0 THEN RETURN err.time_out ELSE RETURN err.ipted_op END
  END;
  f^.dev^.doio(r);
  os.send(f^.dev^.lock^);
  len:=r.len;
  RETURN r.res
END s_read;

PROCEDURE read(f: FILE; fpos: INTEGER;     buf: ADDRESS;
                        bpos: INTEGER; VAR len: INTEGER): INTEGER;
  VAR    c: cash_item;   spc: BOOLEAN;
   b,r,p,l: INTEGER;   reset: BOOLEAN;
  res,size: INTEGER;   short: BOOLEAN;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF (fpos<0) OR (bpos<0)        THEN RETURN err.bad_parm END;
  IF len=0                       THEN RETURN ok           END;
  IF len<0                       THEN RETURN err.bad_parm END;
  spc:=(f^.state*special#{});
  IF spc & (f^.state*disk={})    THEN RETURN s_read(f,fpos,buf,bpos,len) END;

  os.wait(f^.flock);
  short:=(f^.mode*long={});
  IF spc THEN reset:=(f^.state*nocash#{})
  ELSE        reset:=FALSE
  END;
  IF fpos+len<=f^.eof THEN size:=len ELSE size:=f^.eof-fpos END;
  IF size<=0 THEN
    len:=0; os.send(f^.flock);
    IF size<0 THEN RETURN err.no_data ELSE RETURN ok END
  END;
  res:=ok;
  b:=fpos DIV 4096;
  p:=fpos MOD 4096;
  len:=0;
  LOOP
    IF size=0 THEN EXIT END;
    l:=4096-p;
    IF size<l THEN l:=size END;
    IF spc    THEN r:=b
    ELSIF short & (b<=HIGH(f^.ref)) THEN
      r:=f^.ref[b];
      IF r<=0 THEN res:=err.no_data; EXIT END
    ELSE
      res:=index(f,b,r,FALSE);
      IF res#ok THEN EXIT END;
    END;
    cash_lock(f^.dev,r,c);
    IF reset THEN cash_update(c); c^.size:=0 END;
    res:=cash_read(c,l+p);
(*  IF res=ok THEN low.cmove(buf,bpos,c^.buf,p,l) END;  *)
    low.cmove(buf,bpos,c^.buf,p,l);     (* 30-Jun-91    *)
    cash_unlock(c);
    IF res#ok THEN EXIT END;
    INC(bpos,l); INC(len,l);
    DEC(size,l); INC(b);     p:=0
  END;
  INC(fpos,len);
  os.send(f^.flock);
  RETURN res
END read;

PROCEDURE s_write(f: FILE; fpos: INTEGER;     buf: ADDRESS;
                           bpos: INTEGER; VAR len: INTEGER): INTEGER;
  VAR r: REQUEST;
BEGIN
  r.op :=req.WRITE; r.res:=ok;
  r.pos:=bpos;      r.ofs:=fpos;
  r.buf:=buf;       r.len:=len;
  r.drn:=f^.dev^.drn;
  r.res:=os.wait_del(-1,f^.dev^.lock^);
  IF r.res#0 THEN
    IF r.res<0 THEN RETURN err.time_out ELSE RETURN err.ipted_op END
  END;
  f^.dev^.doio(r);
  os.send(f^.dev^.lock^);
  len:=r.len;
  RETURN r.res
END s_write;

PROCEDURE write(f: FILE; fpos: INTEGER;     buf: ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;
  VAR    c: cash_item;           spc: BOOLEAN;
   b,r,p,l: INTEGER;            wait: BOOLEAN;
       eof: INTEGER;           reset: BOOLEAN;
  res,size: INTEGER;           dsize: INTEGER;
   sechigh: INTEGER;        tail,pre: INTEGER;
BEGIN
  check_p_stack(128);
  IF (f=NIL)  OR (f^.magic#MAGIC)     THEN RETURN err.bad_desc END;
  IF (len<=0) OR (fpos<0) OR (bpos<0) THEN RETURN err.bad_parm END;
  IF f^.mode*dir#{}                   THEN RETURN err.is_dir   END;
  spc:=(f^.state*special#{});
  IF spc & (f^.state*disk={}) THEN
    RETURN s_write(f,fpos,buf,bpos,len)
  ELSIF spc THEN
    dsize:=mpw2(f^.dev^.secs,f^.dev^.ssc)
  END;

  IF f^.dev^.state*_wpro#{} THEN RETURN err.write_pro END;
  reset:=(f^.state*nocash#{});
  wait:=reset OR (f^.state*wait_wr#{});
  os.wait(f^.flock);
  res:=ok;      size:=len;
  sechigh:=f^.dev^.secsz-1;
  b:=fpos DIV 4096;
  p:=fpos MOD 4096;
  len:=0;
  eof:=f^.eof-INTEGER(set(fpos)-set(4095));
  LOOP
    IF size=0 THEN EXIT END;
    l:=4096-p;
    IF size<l THEN l:=size END;
    IF spc    THEN r:=b;
      IF r*4096+l>dsize THEN l:=dsize-r*4096        END;
      IF r*4096 >=dsize THEN res:=err.no_data; EXIT END
    ELSIF (f^.mode*long={}) & (b<=HIGH(f^.ref)) & (f^.ref[b]>0) THEN
      r:=f^.ref[b];
    ELSE
      res:=index(f,(b),r,TRUE);
      IF res#ok THEN EXIT END;
    END;
    cash_lock(f^.dev,r,c);

    IF c^.size<p+l THEN
      (* preliminary read: Leo & Igo 31-Aug-89                         *)
      (* local 'eof' in block: <0 if f^.eof left; >=4096 if right      *)
      (* it's nessesary pre-read data upto [0..pre[ ( pre=min(eof,p) ) *)
      pre:=p;
      IF eof<p THEN pre:=eof END;
      (* it's also nessesary pre-read data in [tail..tail+secsz[ *)
      IF (p+l<eof) & (set(p+l)*set(sechigh)#{}) THEN
        tail:=INTEGER(set(p+l)-set(sechigh));
      ELSE
        tail:=-1
      END;
      IF (pre>0) & (tail>=0) THEN
        res:=cash_read(c,tail+sechigh+1)
      ELSIF pre>0 THEN
        res:=cash_read(c,pre)
      ELSIF tail>=0 THEN
        res:=cash_read_sec(c,tail DIV 4)
      ELSE
        (* both pieces are empty: res=ok *)
      END;
      IF res#ok THEN cash_unlock(c); EXIT END;
    END;
    low.cmove(c^.buf,p,buf,bpos,l);
    res:=cash_write(c,p,l,wait);
    IF reset THEN ASSERT((c^.dirty={}) & NOT c^.activ); c^.size:=0 END;
    cash_unlock(c);
    IF res#ok THEN EXIT END;
    INC(bpos,l);  INC(len ,l);  p:=0;
    DEC(size,l);  INC(b);       DEC(eof,4096)
  END;
  INC(fpos,len);
  IF fpos>f^.eof THEN f^.eof:=fpos END;
  f^.wtime:=os.time;
  f^.state:=f^.state+dirty;
  os.send(f^.flock);
  RETURN res
END write;

PROCEDURE du(d: FILE; VAR free,used: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  check_p_stack(128);
  IF (d=NIL) OR (d^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF  d^.mode*dir={}             THEN RETURN err.not_dir  END;
  free:=mpw2(d^.dev^.secs-d^.dev^.du,d^.dev^.ssc);
  used:=mpw2(d^.dev^.du,d^.dev^.ssc);
  RETURN ok
END du;

PROCEDURE cut(f: FILE; size: INTEGER): INTEGER;
  VAR res: INTEGER;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF  f^.state*special#{}        THEN RETURN err.unsuitable END;
  IF  f^.mode*dir     #{}        THEN RETURN err.is_dir     END;
  IF  f^.dev^.state*_wpro#{}     THEN RETURN err.write_pro  END;
  os.wait(f^.flock);
  cut_file(f,size,res);
  os.send(f^.flock);
  RETURN res
END cut;

PROCEDURE extend(f: FILE; size: INTEGER): INTEGER;
  VAR res: INTEGER;
BEGIN
  check_p_stack(128);
  IF (f=NIL) OR (f^.magic#MAGIC) THEN RETURN err.bad_desc   END;
  IF  f^.state*special#{}        THEN RETURN err.unsuitable END;
  IF  f^.mode*dir     #{}        THEN RETURN err.is_dir     END;
  IF  f^.dev^.state*_wpro#{}     THEN RETURN err.write_pro  END;
  IF size<=0 THEN RETURN ok END;
  os.wait(f^.flock);
  extend_file(f,size,res);
  os.send(f^.flock);
  RETURN res
END extend;

PROCEDURE make_fs(dsk: FILE; VAL name: ARRAY OF CHAR;
                             VAL bads: ARRAY OF INTEGER): INTEGER;
  VAR res: INTEGER;
BEGIN
  check_p_stack(128);
  IF (dsk=NIL) OR (dsk^.magic#MAGIC) THEN RETURN err.bad_desc    END;
  IF  dsk^.state*special={}          THEN RETURN err.unsuitable  END;
  IF  dsk^.state*disk={}             THEN RETURN err.not_blocked END;
  IF  dsk^.dev^.state*_wpro#{}       THEN RETURN err.write_pro   END;
  IF  dsk^.opens>1                   THEN RETURN err.busy        END;
  os.wait(dsk^.flock);
   res:=make_volume(dsk,name,bads);
  os.send(dsk^.flock);
  RETURN res
END make_fs;

PROCEDURE getattr(f: FILE; atr: INTEGER; VAR val: SYSTEM.WORD);
BEGIN
  CASE atr OF
  |a_eof  : val:=f^.eof
  |a_pro  : val:=f^.pro
  |a_ctime: val:=f^.ctime
  |a_wtime: val:=f^.wtime
  |a_links: val:=f^.links
  |a_mode : val:=f^.mode
  |a_inode: val:=f^.inode
  END
END getattr;

PROCEDURE setattr(f: FILE; atr: INTEGER;     val: SYSTEM.WORD);
BEGIN
  CASE atr OF
  |a_eof  : f^.eof  :=val; f^.state:=f^.state+dirty
  |a_pro  : f^.pro  :=val; f^.state:=f^.state+dirty
  |a_ctime: f^.ctime:=val; f^.state:=f^.state+dirty
  |a_wtime: f^.wtime:=val; f^.state:=f^.state+dirty
  |a_links:
  |a_mode :
  END
END setattr;

PROCEDURE isdir(f: FILE): BOOLEAN;
BEGIN RETURN f^.mode*dir#{} END isdir;

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

BEGIN
  null  :=NIL;
  PMAGIC:=54524F50h;
  MAGIC :=_MAGIC;
  ROOT  :="";
  SYNC  :=FALSE;
  Flist :=NIL;
  root  :=NIL;
  ports :=NIL;
  os.ini_signal(update,{},0);
  os.ini_signal(Plock, os.guard,1);
  os.ini_signal(Flock, os.guard,1);
  dkread:=0;  dkwrite:=0;
  chread:=0;  chsize :=0;
  init_cash;
  os.set_debug(syslogprint)
END osFiles.
