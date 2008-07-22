MODULE fschk; (* Hady. 10-Nov-89. (c) KRONOS *)
              (* Leo   12-Dec-89. (c) KRONOS *)
              (* Hady  23-Nov-90. (c) KRONOS *)

(*$U+*)
(*$A8 Leo 12-Jan-91 *)

IMPORT  BIO, ASCII;

IMPORT       SYSTEM;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  low: lowLevel;
IMPORT  arg: tskArgs;
IMPORT  env: tskEnv;
IMPORT  mem: Heap;
IMPORT  lex: Lexicon;

WITH STORAGE: mem;

TYPE
  WORD    = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;
  str32   = ARRAY [0..31] OF CHAR;
  str80   = ARRAY [0..79] OF CHAR;

VAR
   SAVE: BOOLEAN; (* SAVE errors       *)
  AGAIN: BOOLEAN;
  ABORT: BOOLEAN;
  WRITE: BOOLEAN; (* WRITE corrections *)

-------------------------  DEVICE I/O  -------------------------
                         --------------

MODULE dio; (* Hady. 20-Oct-89. (c) KRONOS *)

  IMPORT  tty; -- for debug purposes ONLY !

  IMPORT  bio: BIO;
  IMPORT  str;
  IMPORT  lex;

  IMPORT   SYSTEM, ADDRESS, WORD, str32, str80, WRITE, ABORT;

  EXPORT QUALIFIED dir, long, esc, sysf, all_modes
                 , d_del, d_file, d_dir, d_hidden, d_entry, d_esc, d_sys
                 , all_kinds, sys_0
                 , i_node, dir_node, Dir, no_i, no_b, iSET, bSET
                 , s_put, message, error, reset, get_inode, put_inode
                 , get_block, put_block, dir_walk, end_walk, mount
                 , release, dNode;

  CONST
    dir = {1};  long = {2};  esc = {3}; sysf = {4};
    all_modes = dir + long + esc + sysf;

  TYPE
    i_node =
    RECORD
      Ref   : ARRAY [0..7] OF INTEGER;            -- 32
      Mode  : BITSET;                             -- 36
      Links : INTEGER;                            -- 40
      Eof   : INTEGER;                            -- 44
      cTime : INTEGER;  (* Creation time *)       -- 48
      wTime : INTEGER;  (* Last write time *)     -- 52
      pro   : INTEGER;                            -- 56
      gen   : INTEGER;  (* generation number *)   -- 60
      res   : ARRAY [0..0] OF INTEGER;            -- 64
    END (* i_node *);

    b_ptr = POINTER TO ARRAY [0..1023] OF WORD;

  CONST
    d_del = {0};   d_file = {1};   d_dir = {2};   d_hidden = {3};
    d_esc = {4};   d_sys  = {5};

    d_entry   = d_dir + d_file + d_esc ;

    all_kinds = d_del + d_entry + d_hidden + d_sys;

  TYPE  dir_node = RECORD
                     name: str32;                     -- 32
                     rfe0: ARRAY [0..3] OF INTEGER;   -- 48
                     inod: INTEGER;                   -- 52
                     kind: BITSET;                    -- 56
                     rfe1: ARRAY [0..1] OF INTEGER;   -- 64
                   END;

        dNode = POINTER TO dir_node;

       DIR = RECORD
                 no: INTEGER;
                  n: i_node;
               buff: DYNARR OF dir_node;
                REF: DYNARR OF INTEGER;
             END;

        Dir = POINTER TO DIR;

  CONST inods = 64; -- no inodes per one 4KB block;

  VAR no_i: INTEGER; -- inodes account on device
      no_b: INTEGER; -- blocks account on device
     sys_0: INTEGER; -- blocks occupied by system

  VAR iSET: DYNARR OF BITSET; -- free iNodes
      bSET: DYNARR OF BITSET; -- free blocks

  VAR s_put: BOOLEAN; -- IF TRUE THEN to write superblock buffer

  VAR error: BOOLEAN;
    message: str80;

  PROCEDURE check();
  BEGIN
    IF bio.done THEN RETURN END;
    error:=TRUE; lex.perror(message,bio.error,"%%s");
  END check;

  VAR drv: bio.FILE;
    super: b_ptr;

  PROCEDURE get_block(no: INTEGER; buff: ADDRESS; sz: INTEGER);
    VAR r: BOOLEAN; i: INTEGER;
  BEGIN
    bio.seek(drv,no*4096,0); check;
    IF error THEN RETURN END;
    bio.read(drv,buff,sz); check;
  END get_block;

  PROCEDURE put_block(no: INTEGER; buff: ADDRESS; sz: INTEGER);
  BEGIN
    IF NOT WRITE THEN RETURN END;
    bio.seek(drv,no*4096,0); check;
    IF error THEN RETURN END;
    bio.write(drv,buff,sz); check;
    --tty.print('put_block: no: %5d, size: %4d\n',no,sz);
  END put_block;

  PROCEDURE unpack_sup;
    VAR lab: POINTER TO ARRAY [0..7] OF INTEGER;
    isz,bsz: INTEGER;
          s: POINTER TO ARRAY [0..3] OF CHAR;
          i: INTEGER;
  BEGIN
    lab:=ADDRESS(super);
    no_b:=lab^[5];
    no_i:=lab^[4];
    -- s:=sys.ADR(lab^[6]); -- CHECK "CXE ?";
    isz:=(no_i+31) DIV 32;
    bsz:=(no_b+31) DIV 32;
    bSET^.ADR:=ADDRESS(super)+16; bSET^.HIGH:=bsz-1;
    iSET^.ADR:=bSET^.ADR+bsz;     iSET^.HIGH:=isz-1;
    sys_0:=1+((no_i+inods-1) DIV inods);
  END unpack_sup;

  PROCEDURE mount(drv_name: ARRAY OF CHAR);
    VAR a: ADDRESS;
  BEGIN
    NEW(super);
    IF WRITE THEN
      bio.open(drv,drv_name,"mc"); check;
    ELSE
      bio.open(drv,drv_name,"r"); check;
    END;
    IF error THEN ABORT:=TRUE; RETURN END;
    IF bio.is_disk*bio.kind(drv)={} THEN
      str.print(message,'"%s" is not disk device',drv_name);
      error:=TRUE; ABORT:=TRUE; RETURN
    END;
    get_block(1,super,4096);
    IF error THEN ABORT:=TRUE; RETURN END;
    bio.lock(-1,drv); check;
    IF error THEN ABORT:=TRUE; RETURN END;
    unpack_sup;
  END mount;

  TYPE iNode = POINTER TO i_node;

  VAR ibuff: b_ptr;
       iblk: INTEGER;
      i_put: BOOLEAN;

  PROCEDURE get_inode0(no: INTEGER): iNode;
    VAR b: INTEGER;
        s: str80;
  BEGIN
    IF ibuff=NIL THEN NEW(ibuff); i_put:=FALSE; END;
    b:=2+(no DIV inods);
    IF b#iblk THEN
      IF i_put THEN -- flush inode buffer
        put_block(iblk,ibuff,4096); i_put:=FALSE;
      END;
      get_block(b,ibuff,4096);
      IF error THEN
        s:=message;
        str.print(message,"reading inode block %05d: %s",b,s);
        RETURN NIL;
      END;
      iblk:=b;
    END;
    RETURN ADDRESS(ibuff)+(no MOD inods)*SIZE(i_node);
  END get_inode0;

  PROCEDURE get_inode(no: INTEGER; VAR in: i_node);  VAR n: iNode;
  BEGIN n:=get_inode0(no); IF n#NIL THEN in:=n^ END END get_inode;

  PROCEDURE put_inode(no: INTEGER; VAL in: i_node);
    VAR n: iNode;
  BEGIN
    n:=get_inode0(no);
    IF n#NIL THEN n^:=in; i_put:=TRUE END;
  END put_inode;

  PROCEDURE min(x,y: INTEGER): INTEGER;
  BEGIN IF x>y THEN RETURN y ELSE RETURN x END END min;

  PROCEDURE dir_walk(ino: INTEGER): Dir;

    VAR p_n: iNode; d: Dir; max,b: INTEGER;
        free: INTEGER;

    PROCEDURE acc_ref(VAR ref: ARRAY OF INTEGER): INTEGER;
      VAR i: INTEGER;
    BEGIN i:=0;
      WHILE (i<=HIGH(ref)) & (ref[i]>0) & (ref[i]<no_b) DO
        IF free>HIGH(d^.REF) THEN
          RESIZE(d^.REF,SIZE(d^.REF)+8);
          IF free>HIGH(d^.REF) THEN
            error:=TRUE; message:='No memory'; RETURN -1
          END;
        END;
        d^.REF[free]:=ref[i]; i:=i+1; free:=free+1;
      END;
      RETURN i*4096;
    END acc_ref;

    PROCEDURE acc_eof(VAR N: i_node);
      VAR buff: POINTER TO ARRAY [0..1023] OF INTEGER;
             i: INTEGER;
    BEGIN
      NEW(d^.REF,8);
      WITH N DO
        IF long*Mode#{} THEN
          buff:=NIL; NEW(buff);
          IF buff=NIL THEN error:=TRUE; message:='No memory'; RETURN END;
          i:=0; max:=0;
          LOOP
            IF i>7 THEN EXIT END;
            IF (Ref[i]<=0) OR (Ref[i]>=no_b) THEN EXIT END;
            get_block(Ref[i],buff,4096);
            IF error THEN EXIT END;
            max:=max+acc_ref(buff^);
            IF (max<i*1024*4096) THEN EXIT END;
            i:=i+1;
          END;
          DISPOSE(buff);
        ELSE
          max:=acc_ref(Ref);
        END;
        IF (Eof>0) & (Eof<max) THEN max:=Eof END;
      END;
      max:=max DIV BYTES(dir_node);
      RESIZE(d^.REF,free);
    END acc_eof;

  BEGIN
    p_n:=get_inode0(ino);
    IF p_n=NIL THEN RETURN NIL END;
    IF p_n^.Mode*dir={} THEN
      error:=TRUE; str.print(message,"Is not directory");
      RETURN NIL;
    END;
    NEW(d);  NEW(d^.REF); free:=0;
    WITH d^ DO  no:=ino;  n:=p_n^;
      acc_eof(n);
      IF error THEN DISPOSE(REF); DISPOSE(d); RETURN NIL END;
      IF max=0 THEN DISPOSE(d);
        error:=TRUE; str.print(message,'Bad Eof in dir %d ',ino);
        RETURN NIL
      END;
      NEW(buff,max); b:=0;
      WHILE max>0 DO
        get_block(REF[b],buff^.ADR+b*1024,min(4096,max*BYTES(dir_node)));
        IF error THEN DISPOSE(REF); DISPOSE(d); RETURN NIL END;
        max:=max-min(max,4096 DIV BYTES(dir_node)); INC(b);
      END;
    END;
    RETURN d;
  END dir_walk;

  PROCEDURE end_walk(d: Dir; write: BOOLEAN);
    VAR max,b: INTEGER; n: iNode;
  BEGIN
    IF d=NIL THEN RETURN END;
    IF write THEN
      max:=HIGH(d^.buff)+1; b:=0;
      WHILE max>0 DO
        put_block(d^.REF[b],d^.buff^.ADR+b*1024,
                                   min(4096,max*BYTES(dir_node)));
        max:=max-min(4096 DIV BYTES(dir_node),max); INC(b);
      END;
      put_inode(d^.no,d^.n);
      IF error THEN RETURN END;
    END;
    DISPOSE(d^.buff); DISPOSE(d^.REF); DISPOSE(d);
  END end_walk;

  PROCEDURE reset; BEGIN error:=FALSE; message:="" END reset;

  PROCEDURE release;
    VAR s: str80;
  BEGIN
    IF s_put THEN
      put_block(1, super, 4096); s_put:=FALSE;
      IF error THEN
        s:=message;
        str.print(message,"ATTENTION! writting super block: %s",s);
        RETURN
      END;
      DISPOSE(super);
    END;
    IF i_put THEN
      put_block(iblk,ibuff,4096); i_put:=FALSE;
      IF error THEN
        s:=message;
        str.print(message,"ATTENTION! writting inode block %05d: %s",iblk,s);
        RETURN
      END;
      DISPOSE(ibuff);
    END;
    bio.unlock(drv); check;
    bio.close(drv) ; check; drv:=bio.null;
  END release;
  
BEGIN
  reset; drv:=bio.null; iblk:=-1; i_put:=FALSE; s_put:=FALSE;
  NEW(iSET); NEW(bSET); ibuff:=NIL; super:=NIL;
END dio;

----------------------  ERRORS KEEPING  ------------------------
                      ------------------

MODULE err; (* Hady. 05-Nov-89. (c) KRONOS *)

  IMPORT  str;
  IMPORT  SAVE, WORD, str80, ASCII;
  IMPORT  tty;

  EXPORT QUALIFIED release  , put_dbl  , get_dbl
                 , no_anon  , put_anon , get_anon
                 , put_error, get_error, set_head;

  TYPE item = POINTER TO error;
      error = RECORD
                one : INTEGER;
                two : INTEGER;
                next: item;
              END;

  VAR HEAD: str80;
   no_anon: INTEGER;
  dbl,anon: item;

  PROCEDURE make_err(one,two: INTEGER): item;
    VAR e: item;
  BEGIN NEW(e);
    e^.one :=one;  e^.two:=two;
    e^.next:=NIL;  RETURN  e  ;
  END make_err;

  PROCEDURE put_err(VAR to: item; one,two: INTEGER);
    VAR e,l: item;
  BEGIN
    e:=make_err(one,two);
    IF to=NIL THEN to:=e;
    ELSE l:=to;
      IF (l^.one=one) & (l^.two=two) THEN DISPOSE(e); RETURN END;
      WHILE l^.next#NIL DO
        l:=l^.next;
        IF (l^.one=one) & (l^.two=two) THEN DISPOSE(e); RETURN END;
      END;
      l^.next:=e;
    END;
  END put_err;

  PROCEDURE put_dbl(f,no: INTEGER); BEGIN put_err(dbl,f,no) END put_dbl;

  PROCEDURE put_anon(no: INTEGER);
  BEGIN put_err(anon,no,-1); INC(no_anon) END put_anon;

  PROCEDURE get_err(VAR from: item; VAR o,t: INTEGER): BOOLEAN;
    VAR e: item;
  BEGIN
    IF from=NIL THEN RETURN FALSE END;
    e:=from;    from:=from^.next;
    o:=e^.one;  t:=e^.two;
    DISPOSE(e); RETURN TRUE;
  END get_err;

  PROCEDURE get_dbl(VAR f,no: INTEGER): BOOLEAN;
  BEGIN RETURN get_err(dbl,f,no) END get_dbl;

  PROCEDURE get_anon(VAR no: INTEGER): BOOLEAN;
    VAR b: INTEGER;
  BEGIN DEC(no_anon); RETURN get_err(anon,no,b) END get_anon;

  TYPE errort = POINTER TO report;
       report = RECORD
                  rep: DYNARR OF CHAR;
                 next: errort;
                END;

  VAR reps: errort;

  PROCEDURE set_head(VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
  BEGIN
    tty.print("\r"); tty.erase_line(0);
    str.print(HEAD,fmt,args);
    tty.print("%s\r",HEAD);
  END set_head;

  PROCEDURE put_error(VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
    VAR l: INTEGER;
        e: errort;
     bump: str80;
  BEGIN
    str.print(bump,fmt,args);
    tty.print("\r%s",bump); tty.erase_line(0); tty.print("\n%s\r",HEAD);

    IF NOT SAVE THEN RETURN END;

    NEW(e);
    e^.next:=reps; reps:=e;
    l:=str.len(bump);
    IF l>HIGH(bump) THEN DEC(l) END;
    bump[l]:=ASCII.NL; INC(l);
    bump[l]:=000c;     INC(l);
    NEW(e^.rep,l);
    str.copy(e^.rep,bump);
  END put_error;

  PROCEDURE get_error(VAR s: ARRAY OF CHAR; VAR len: INTEGER): BOOLEAN;
    VAR e: errort;
  BEGIN
    IF reps=NIL THEN RETURN FALSE END;
    e:=reps;
    reps:=reps^.next;
    str.copy(s,e^.rep); len:=HIGH(e^.rep);
    DISPOSE(e^.rep); DISPOSE(e);
    RETURN TRUE
  END get_error;

  PROCEDURE release;
    VAR e: item;
  BEGIN
    WHILE dbl #NIL DO e:=dbl;  dbl:=dbl^.next;   DISPOSE(e) END;
    WHILE anon#NIL DO e:=anon; anon:=anon^.next; DISPOSE(e) END
  END release;

BEGIN
  anon:=NIL; dbl:=NIL; no_anon:=0; reps:=NIL; HEAD:="";
END err;

-------------------------  SERVICE  ----------------------------
                         -----------

PROCEDURE _query(): BOOLEAN;
  VAR ch,cch: CHAR;
BEGIN
  LOOP
    key.read(ch); cch:=CAP(ch);
    IF (cch='Y') OR (cch='N') THEN tty.print("%c\n",ch); RETURN cch='Y' END;
  END
END _query;

PROCEDURE warning(VAL s: ARRAY OF CHAR; SEQ arg: WORD);
BEGIN
  tty.print(s,arg);
  IF ABORT THEN tty.print("\n"); HALT END;
  tty.print('\nIs it dangerous?');
  IF _query() THEN HALT END;
END warning;

PROCEDURE warn(VAL s: ARRAY OF CHAR; SEQ args: WORD);
BEGIN
  IF NOT dio.error THEN RETURN END;
  tty.print('%s ',dio.message);
  warning(s,args);
  dio.reset
END warn;

CONST spaces = "         ";

----------------------  GENERAL STATE  -------------------------
                      -----------------

VAR iBusy: DYNARR OF BITSET;  -- busy      inodes set iterated
    iDirs: DYNARR OF BITSET;  -- directory inodes set iterated
   inDirs: DYNARR OF BITSET;  -- inodes, that linked with dirs
    bBusy: DYNARR OF BITSET;  -- busy blocks set
   iLinks: DYNARR OF INTEGER; -- number of links of iteration

   b_err_busy, b_err_free: INTEGER;
   f_err_busy, f_err_free: INTEGER;

PROCEDURE incl(VAR b: ARRAY OF BITSET; no: INTEGER);
  VAR i,j: INTEGER;
BEGIN i:=no DIV 32; j:=no MOD 32; INCL(b[i],j) END incl;

PROCEDURE excl(VAR b: ARRAY OF BITSET; no: INTEGER);
  VAR i,j: INTEGER;
BEGIN i:=no DIV 32; j:=no MOD 32; EXCL(b[i],j) END excl;

PROCEDURE excl?(VAR b: ARRAY OF BITSET; no: INTEGER): BOOLEAN;
  VAR i,j: INTEGER; r: BOOLEAN;
BEGIN
  i:=no DIV 32; j:=no MOD 32;
  r:=NOT (j IN b[i]); EXCL(b[i],j); RETURN r;
END excl?;

PROCEDURE in?(VAL b: ARRAY OF BITSET; no: INTEGER): BOOLEAN;
BEGIN RETURN (no MOD 32) IN b[no DIV 32] END in?;

PROCEDURE create_sets;
  VAR i: INTEGER;
  CONST inods = 4096 DIV BYTES(dio.i_node);
BEGIN
  NEW(iBusy , HIGH(dio.iSET)+1);
  NEW(inDirs, HIGH(dio.iSET)+1);
  NEW(bBusy , HIGH(dio.bSET)+1);
  NEW(iLinks, dio.no_i);
  NEW(iDirs, HIGH(dio.iSET)+1);
  low.fill( iDirs,{});
  low.fill( iBusy,{0..31});
  low.fill(inDirs,{0..31});
  excl(inDirs,0);
  low.fill( bBusy,{0..31});
  FOR i:=0 TO 1+((dio.no_i+inods-1) DIV inods) DO excl(bBusy,i) END;
  low.fill(iLinks,0);
  b_err_busy:=0; b_err_free:=0;
  f_err_busy:=0; f_err_free:=0
END create_sets;

-------------------  FILE SYSTEM SERVICE  ----------------------
                   -----------------------

PROCEDURE alloc_blk(): INTEGER; --  RETURN -1 if impossible
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO dio.no_b-1 DO
    IF in?(dio.bSET,i) THEN
      excl(dio.bSET,i); dio.s_put:=TRUE; RETURN i
    END;
  END;
  RETURN -1;
END alloc_blk;

PROCEDURE alloc_slot(dir: dio.Dir): INTEGER;
(* in successful final the directory is changed, but not written
*)
  CONST dn_size = BYTES(dio.dir_node);
         dnodes = 4096 DIV dn_size;

  VAR i,b,eof: INTEGER;

BEGIN
  FOR i:=1 TO HIGH(dir^.buff) DO
    WITH dir^.buff[i] DO
      (* current slot in directory is free *)
      IF (dio.d_del*kind#{}) OR (kind*dio.d_entry={}) OR (name="") THEN
        kind:=kind-dio.d_del; RETURN i
      END
    END
  END;
  eof:=dir^.n.Eof;
  IF (eof=0) OR (((eof DIV dn_size)+1) MOD 64 = 0) THEN
    FOR b:=0 TO HIGH(dir^.n.Ref) DO
      IF    dir^.n.Ref[b]<=0   THEN
        dir^.n.Ref[b]:=alloc_blk();
        IF dir^.n.Ref[b]=-1 THEN
          tty.print('NO FREE BLOCK ON VOLUME\n');
          RETURN -1
        END;
      ELSIF b=HIGH(dir^.n.Ref) THEN
        tty.print('е умею работать с длинными директориями\n');
        RETURN -1;
      END;
    END;
  END;
  dir^.n.Eof:=eof+dn_size;
  RESIZE(dir^.buff, HIGH(dir^.buff)+2);
  i:=HIGH(dir^.buff);
  WITH dir^.buff[i] DO
    FOR b:=0 TO HIGH(name) DO name[b]:=0c END;
    FOR b:=0 TO HIGH(rfe0) DO rfe0[b]:=0  END;
    FOR b:=0 TO HIGH(rfe1) DO rfe1[b]:=0  END;
    kind:={}; inod:=-1;
  END;
  RETURN i;
END alloc_slot;

------------------------  ITERATION  ---------------------------
                        -------------

PROCEDURE iter_inodes;

(* REQUIRES:
     1. ARRAYs OF BITSETs have to be initialised by "create_sets" call;
     2. Volume I/O initialised
   EFFECTS:
     1. iBusy, bBusy & iDirs start to contain proper information
*)


  PROCEDURE check_inode(ino: INTEGER; VAR n: dio.i_node): BOOLEAN;
    VAR r: BOOLEAN;    (* return TRUE when modified    *)
        i: INTEGER;
  BEGIN
    r:=FALSE;
    WITH n DO
      IF Mode-dio.all_modes#{} THEN
        err.put_error("inode %d modes=%{} -> %{}",ino,Mode,Mode*dio.all_modes);
        Mode:=Mode*dio.all_modes; r:=TRUE
      END;
      IF Eof<0 THEN
        err.put_error("inode %d eof=%d -> 0",ino,Eof);
        Eof:=0; r:=TRUE
      END;
      IF (Mode*dio.dir#{}) THEN
        IF Eof MOD 64 # 0 THEN
          i:=(Eof DIV 64)*64;
          err.put_error("inode %d is dir (eof MOD 64 # 0) %d -> %d",ino,Eof,i);
          Eof:=i; r:=TRUE
        END;
        IF Links>1 THEN
          err.put_error("inode %d is dir (links > 1) %d -> 1",ino,Links);
          Links:=1; r:=TRUE
        END
      END;
      IF Links<0 THEN
        err.put_error("inode %d links=%d -> 0",ino,Links);
        Links:=0; iLinks[ino]:=0; r:=TRUE
      END;
      FOR i:=0 TO HIGH(res) DO
        IF res[i]#0 THEN
          err.put_error("inode %d rfe%d=%08h -> 0",ino,i,res[i]);
          res[i]:=0; r:=TRUE
        END
      END
    END;
    RETURN r
  END check_inode;

  VAR buff: POINTER TO ARRAY [0..1023] OF INTEGER;

  PROCEDURE check_ref(VAR   n: dio.i_node;
                      VAR max: INTEGER;
                          ino: INTEGER): BOOLEAN;

    CONST
      IND = " %s indirect-block %05d.Ref[%d]=%05d [%#h]";
      CLR = "Illegal block %05d.ref[%d]=%05d (will be cleared)";

    VAR yes: BOOLEAN;

    PROCEDURE check_blk(VAR  no: INTEGER;
                          l_no: INTEGER): BOOLEAN;
      VAR i: INTEGER;
    BEGIN
      IF (no<-1) OR (no>=dio.no_b) THEN
        IF l_no>=0 THEN i:=l_no ELSE i:=HIGH(n.Ref)+1-l_no END;
        err.put_error(CLR,ino,i,no); no:=-1;
        RETURN TRUE
      END;
      IF no>0 THEN
        IF excl?(bBusy,no) THEN err.put_dbl(ino,l_no) END;
        IF yes THEN max:=max+1 END
      ELSE yes:=FALSE
      END;
      RETURN FALSE
    END check_blk;

    PROCEDURE check_ind(i: INTEGER): BOOLEAN;
      VAR j,no: INTEGER;   r: BOOLEAN;
    BEGIN
      WITH n DO
        no:=Ref[i];
        IF no<=dio.sys_0 THEN
          err.put_error("Incorrect block number i%d.Ref[%d]=%d -> -1."
                        ,ino,i,no);
          Ref[i]:=-1; RETURN TRUE
        END;
        dio.get_block(no, buff, 4096);
        r:=dio.error;
        warn(IND,"reading",ino,i,no,no);
        IF r THEN RETURN FALSE END;
        r:=FALSE; ------
        FOR j:=0 TO HIGH(buff^) DO
          IF (buff^[j]<-1) OR (buff^[j]>0) THEN
            r:=check_blk(buff^[j],i*1024+j) OR r
          END
        END;
        IF NOT r THEN RETURN FALSE END;
        dio.put_block(no,buff,4096);
        warn(IND,"writeing",ino, i, no, no)
      END;
      RETURN FALSE
    END check_ind;

    VAR i,j: INTEGER;
        mod: BOOLEAN;

  BEGIN
    mod:=FALSE;
    yes:=TRUE; max:=0;
    WITH n DO
      FOR i:=0 TO HIGH(Ref) DO
        mod:=check_blk(Ref[i],i-HIGH(Ref)-1) OR mod
      END;
      IF n.Mode*dio.long={} THEN RETURN mod END;
      IF buff=NIL THEN           ------
        NEW(buff)
      END;
      yes:=TRUE; max:=0;
      FOR i:=0 TO HIGH(Ref) DO
        IF Ref[i]>0 THEN mod:=check_ind(i) OR mod END
      END
    END;
    RETURN mod
  END check_ref;

  VAR i: INTEGER;
      N: dio.i_node;
      r: BOOLEAN;
    max: INTEGER;
  write: BOOLEAN;

BEGIN
  buff:=NIL;
  FOR i:=0 TO dio.no_i-1 DO
    IF i MOD 64 = 0 THEN tty.print(' %d%%\r',i*100 DIV dio.no_i) END;
    dio.get_inode(i,N); r:=dio.error;
    warn("reading inode %d",i);
    IF NOT r THEN
      write:=check_inode(i,N);
      IF N.Links>0 THEN
        WITH N DO
          excl(iBusy,i);
          iLinks[i]:=Links;
          IF Mode*dio.esc={} THEN write:=check_ref(N,max,(i)) OR write END;
          IF Mode*dio.dir#{} THEN
            incl(iDirs,i); max:=max*4096;
            IF max<Eof THEN
              err.put_error(
                "dir %d - broken persistense; eof(%d -> %d)",
                            i,Eof,max);
              Eof:=max; write:=TRUE
            END
          END
        END
      END;
      IF write THEN dio.put_inode(i,N); warn("writting inode %d",i) END
    END
  END;
  IF buff#NIL THEN DISPOSE(buff) END
END iter_inodes;

VAR invalid: ARRAY CHAR OF CHAR;

CONST
  ILG_NM = 'Illegal name  "%.32s" (i=%05d) in dir %05d corrected to "%.32s"';
  TOO_LG = 'Too long name "%.32s" (i=%05d) in dir %05d truncated to "%.31s"';
  CLR_EN = 'Illegal entry "%.32s" (i=%05d) in dir %05d (dnode will be cleared)';
  DRT_WG = 'Warning: dirty "" in dir %05d (node will be corrected)';
  DRT_EN = 'Warning: dirty entry "%.32s" (i=%05d) in dir %05d (dnode '
           'will be corrected)';

PROCEDURE iter_dirs;

  PROCEDURE check_node(VAR n: dio.dir_node; dir: INTEGER): BOOLEAN;

    VAR mod: BOOLEAN; (* node modified *)

    PROCEDURE check_name;
      VAR i: INTEGER;  ch: CHAR;  new: str32;  bad: BOOLEAN;
    BEGIN
      WITH n DO
        i:=0; bad:=FALSE; low.fill(new,0);
        WHILE (i<=HIGH(name)) & (name[i]#0c) DO
          ch:=name[i];
          IF invalid[ch]=0c THEN new[i]:=ch
          ELSE      bad:=TRUE;   new[i]:=CHAR(100b+ORD(ch) MOD 32);
          END; INC(i)
        END;
        IF bad THEN
          new[HIGH(new)]:=0c;
          err.put_error(ILG_NM, name, inod, dir, new);
          name:=new; mod:=TRUE;
          RETURN
        END;
        IF i<=HIGH(name) THEN RETURN END;
        err.put_error(TOO_LG, name, inod, dir, name);
        name[HIGH(name)]:=0c
      END
    END check_name;

    VAR i: INTEGER;  occu: BOOLEAN; (* node occupied *)

  BEGIN
    mod:=FALSE;
    WITH n DO
--      occu:=(name#"") & (kind*dio.d_del={}) & (kind*dio.d_entry#{});
      occu:=(name#"");
      IF occu & (kind*dio.d_del#{}) THEN
        err.put_error(
          'Illegal "del" bit usage (i=%05d) in dir %05d. Bit wil be cleared.',
                      inod,dir);
          kind:=kind-dio.d_del+dio.d_file; mod:=TRUE
      END;
      IF occu & (inod<0) OR (inod>=dio.no_i) THEN
        err.put_error(CLR_EN,name,inod,dir);
        inod:=00; name:=""; kind:={};
        RETURN TRUE
      END;
      IF NOT occu & ((inod<-1) OR (inod>=dio.no_i)) THEN
        err.put_error(DRT_WG,dir);
        inod:=00; name:=""; kind:={};
        RETURN TRUE
      END;
      IF kind-dio.all_kinds#{} THEN kind:=kind*dio.all_kinds; mod:=TRUE END;
      FOR i:=0 TO HIGH(rfe0) DO
        IF rfe0[i]#0 THEN rfe0[i]:=0; mod:=TRUE END
      END;
      FOR i:=0 TO HIGH(rfe1) DO
        IF rfe1[i]#0 THEN rfe1[i]:=0; mod:=TRUE END
      END;
      IF occu & mod THEN err.put_error(DRT_WG,dir);
      ELSIF     mod THEN err.put_error(DRT_EN,name,inod,dir)
      END;
      IF occu THEN check_name END
    END; (* WITH n DO *)
    RETURN mod
  END check_node;

  PROCEDURE one_node(VAR dn: dio.dir_node): BOOLEAN;
  BEGIN
    WITH dn DO
      IF (inod>0) & (name#"..") THEN
        excl(inDirs,inod);
        DEC(iLinks[inod]);
        IF (dio.d_esc*kind={}) & in?(iDirs,inod) # (dio.d_dir*kind#{}) THEN
          IF dio.d_dir*kind={} THEN kind:=kind+dio.d_dir-dio.d_file
          ELSE                      kind:=kind-dio.d_dir+dio.d_file
          END;
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END one_node;

  VAR myDirs: DYNARR OF BITSET; -- local copy of iDirs
     forward: BOOLEAN;

  PROCEDURE iter_tree(prev,no,level: INTEGER);
    VAR   d: dio.Dir;           -- walking directory
      entry: INTEGER;           -- no of current entry
      write: BOOLEAN;           -- was directory changed ?
      inode: INTEGER;
  BEGIN
    tty.print('%8d\r',no);
    d:=dio.dir_walk(no); warn("reading directory %d ",no);
    IF (d=NIL) THEN RETURN END;

    write:=FALSE; entry:=0;
    IF (prev>=0) & (level>0) THEN            -- проверить имя ".."
      write:=check_node(d^.buff[entry],no);
      WITH d^.buff[entry] DO
        IF name#".." THEN
          name:=".."; kind:=dio.d_dir+dio.d_hidden;
          write:=TRUE
        END;
        IF inod#prev THEN inod:=prev; write:=TRUE END;
      END;
      INC(entry)
    ELSIF prev<=-2 THEN -- поиск корня
      inode:=d^.buff[entry].inod;
      IF (inode>no) & in?(iDirs,inode) & in?(myDirs,inode) THEN
        dio.end_walk(d,FALSE); warn("close directory %d",no);
        forward:=FALSE;
        RETURN
      END
    END;
    excl(myDirs,no);
    WHILE entry<=HIGH(d^.buff) DO
      write:=check_node(d^.buff[entry],no) OR write;
      WITH d^.buff[entry] DO
        IF (name="") # (kind*dio.d_del#{}) THEN
          IF name="" THEN kind:=kind+dio.d_del-dio.d_entry
          ELSE            kind:=kind-dio.d_del;
            IF kind*dio.d_entry={} THEN kind:=kind+dio.d_file END
          END
        END;
        IF (kind*dio.d_del={}) & (kind*dio.d_entry#{}) THEN
          IF in?(dio.iSET,inod) OR in?(iBusy,inod) THEN
            err.put_error('%d/"%s" points to free inode %d correct it to busy'
                          ,no,name,inod);
            INC(f_err_free); excl(dio.iSET,inod); excl(iBusy,inod);
            dio.s_put:=TRUE
          END;
          write:=one_node(d^.buff[entry]) OR write;
          IF in?(myDirs,inod) & (name#"..") THEN
            iter_tree(no,inod,level+1);
            warn("iterate directory %d",inod);
          END
        END
      END;
      INC(entry)
    END;
    dio.end_walk(d,write); warn("close directory %d",no)
  END iter_tree;

  VAR i: INTEGER;

BEGIN
  NEW(myDirs,HIGH(iDirs)+1);
(*$<$X+*)
  myDirs:=iDirs;
(*$>*)
  iter_tree(-1,0,0);             -- итерация основного поддерева от корня
  FOR i:=1 TO dio.no_i-1 DO      -- поиск анонимного поддерева с
    IF in?(myDirs,i) THEN        -- нахождением корня
      forward := TRUE; iter_tree(-2,i,0);
      IF forward THEN err.put_anon(i) END
    END
  END;
  FOR i:=1 TO dio.no_i-1 DO       -- сборка "мусора" от двух пердыдущих
    IF in?(myDirs,i) THEN err.put_anon(i); iter_tree(-1,i,0) END
  END;
  DISPOSE(myDirs);
END iter_dirs;

PROCEDURE iterate_volume;
BEGIN
  err.set_head('%sChecking index nodes\r',spaces);
  iter_inodes;
  err.set_head('%sChecking file tree\r',spaces);
  iter_dirs;
  err.set_head('');
END iterate_volume;

---------------------  CHECK & CORRECT  ------------------------
                     -------------------

PROCEDURE check_blocks;
  VAR i: INTEGER;
    res: INTEGER;
BEGIN
  FOR i:=0 TO dio.no_b-1 DO
    res:=ORD(in?(bBusy,i))
        +ORD(in?(dio.bSET,i))*2;
    CASE res OF
      |0: (* ok *)
      |1: (* bBusy=1 bSET=0 *)
          err.put_error('block %d wrongly reported used, must be free; SET corrected',i);
          INC(b_err_busy); incl(dio.bSET,i); dio.s_put:=TRUE
      |2: (* bBusy=0 bSET=1 *)
          err.put_error('block %d wrongly reported free, must be busy; SET corrected',i);
          INC(b_err_free); excl(dio.bSET,i); dio.s_put:=TRUE
      |3: (* ok *)
    END
  END
END check_blocks;

PROCEDURE check_dbl;
  VAR N: dio.i_node;
   f,no: INTEGER;
    lno: INTEGER;
    ref: ARRAY [0..7] OF CHAR;
   b,br: INTEGER;
    new: INTEGER;
  write: BOOLEAN;
   buff: POINTER TO ARRAY [0..1023] OF INTEGER;
BEGIN
  buff:=NIL;
  WHILE err.get_dbl(f,no) DO
    IF no<0 THEN
      lno:=HIGH(N.Ref)+1+no; ref:="Ref."
    ELSE
      lno:=no;               ref:=""
    END;
    err.put_error('Duplicate block %s%05d in file %05d '
                  '(will be reallocated and copied)',ref,lno,f);
    dio.get_inode(f,N);
    warn("reading inode %d",f);
    write:=FALSE;
    IF no<0 THEN
      br:=N.Ref[lno] ; new:=alloc_blk();
      N.Ref[lno]:=new; write:=TRUE
    ELSE
      IF buff=NIL THEN NEW(buff) END;
      b:=N.Ref[no DIV 1024];
      dio.get_block(b,buff,4096);  warn("reading indirect-block %d",b);
      br:=buff^[no MOD 1024];
      new:=alloc_blk();
      buff^[no MOD 1024]:=new;
      dio.put_block(b,buff,4096);  warn("writting indirect-block %d",b);
    END;
    IF new>0 THEN
      IF buff=NIL THEN NEW(buff) END;
      dio.get_block(br ,buff,4096); warn("reading  block %d",br);
      dio.put_block(new,buff,4096); warn("writting block %d",new)
    END;
    IF write THEN dio.put_inode(f,N) END
  END;
  IF buff#NIL THEN DISPOSE(buff) END
END check_dbl;

PROCEDURE check_sets;
  VAR i: INTEGER;
    res: INTEGER;
BEGIN
  FOR i:=0 TO dio.no_i-1 DO
    res:=  ORD(in?(iBusy   ,i))
         + ORD(in?(dio.iSET,i))*2
         + ORD(in?(inDirs  ,i))*4;
    CASE res OF
      |0,7: (* ok *)
      |1: (* iBusy=1 iSET=0 inDirs=0 *) AGAIN:=TRUE;
          err.put_error('inode %d.links=0 must be >0',i)
      |2: (* iBusy=0 iSET=1 inDirs=0 *) AGAIN:=TRUE;
          err.put_error('inode %d wrong free, must be busy; SET corrected',i);
          INC(f_err_free); excl(dio.iSET,i); dio.s_put:=TRUE;
      |3: (* iBusy=1 iSET=1 inDirs=0 *) AGAIN:=TRUE;
          err.put_error('inode %d free & empty but used in some dir\n',i)
      |4: (* iBusy=0 iSET=0 inDirs=1 *)
          err.put_anon(i);
          err.put_error('inode %d anonim, will be linked',i);
          err.put_error('inode %d wrong free, must be busy; SET corrected',i);
          INC(f_err_free); excl(dio.iSET,i); dio.s_put:=TRUE;
      |5: (* iBusy=1 iSET=0 inDirs=1 *)
          err.put_error('inode %d wrong busy, must be free; SET corrected',i);
          INC(f_err_busy); incl(dio.iSET,i); dio.s_put:=TRUE;
      |6: (* iBusy=0 iSET=1 inDirs=1 *);
          err.put_error('inode %d anonim',i);
          err.put_anon(i)
    END
  END
END check_sets;

CONST collector = "lost+find";
      anon_name = "%d";

PROCEDURE collect_anonims();

VAR coll: INTEGER; -- NO of collector directory

  PROCEDURE find_collector(): BOOLEAN; -- TRUE if found;
    VAR root: dio.Dir; entry: INTEGER;
  BEGIN
    root:=dio.dir_walk(0);
    IF dio.error OR (root=NIL) THEN
      warn('open root "/" directory'); RETURN FALSE
    END;
    entry:=0;
    WHILE entry<=HIGH(root^.buff) DO
      WITH root^.buff[entry] DO
        IF (name=collector) & ((dio.d_dir+dio.d_del)*kind=dio.d_dir) THEN
          coll:=inod;
          dio.end_walk(root,FALSE);
          warn('close root "/" directory'); RETURN TRUE;
        END;
      END;
      INC(entry);
    END;
    dio.end_walk(root,FALSE);
    warn('close root "/" directory'); RETURN FALSE;
  END find_collector;

  VAR   no: INTEGER;
      colr: dio.Dir; write: BOOLEAN;
     entry: INTEGER;
     inode: dio.i_node;
       rep: str80;
       dir: dio.Dir;
BEGIN
  IF err.no_anon<=0 THEN RETURN END;

  IF NOT find_collector() THEN
    err.put_error('Can not find "%s" on root',collector);
    WHILE err.get_anon(no) DO
      IF in?(iDirs,no) THEN str.print(rep,"dir")
      ELSE                  str.print(rep,"file")
      END;
      err.put_error('Anonymous %s %05d. Can not link to "%s"',rep,no,collector)
    END;
    RETURN
  END;

  colr:=NIL; colr:=dio.dir_walk(coll); write:=FALSE;
  warn('open "%s"',collector);

  WHILE err.get_anon(no) DO
    entry:=alloc_slot(colr); write:=TRUE;
    IF entry<=0 THEN
      err.put_error('no free entry in "%s"',collector);
      dio.end_walk(colr,write); warn('close "%s"',collector);
      RETURN
    END;
    dio.get_inode(no,inode);
    IF dio.error THEN warn("reading inode %05d", no); RETURN END;
    IF (inode.Eof>0) OR (inode.Mode*dio.dir#{}) THEN
      WITH colr^.buff[entry] DO
        str.print(name,anon_name,no);
        IF inode.Mode*dio.dir#{} THEN
          kind:=kind+dio.d_dir-dio.d_file;
          dir:=dio.dir_walk(no);
          warn("reading directory %05d",no);
          dir^.buff[0].inod:=coll;
          dio.end_walk(dir,TRUE);
          warn("close directory %05d",no);
          str.print(rep,'dir');
        ELSE
          kind:=kind+dio.d_file-dio.d_dir;
          str.print(rep,'file');
        END;
        inod:=no;
        err.put_error('Anonymous %s %05d. Linked as "%s/%s"'
                     ,rep,no,collector,name);
      END;
      DEC(iLinks[no])
    ELSE
      WITH colr^.buff[entry] DO kind:=kind+dio.d_del END;
      excl(dio.iSET,no); dio.s_put:=TRUE
    END;
  END;
  IF colr#NIL THEN
    dio.end_walk(colr,write); warn('close "%s"',collector)
  END
END collect_anonims;

PROCEDURE correct_links;
  VAR i: INTEGER;
      r: BOOLEAN;
      N: dio.i_node;
BEGIN
  FOR i:=1 TO dio.no_i-1 DO
    IF (iLinks[i]#0) & NOT (in?(iDirs,i) & (iLinks[i]<0)) THEN
      dio.get_inode(i,N); r:=dio.error; warn("reading inode %d",i);
      IF NOT r THEN
        err.put_error("%d.links=%d -> %d",i,N.Links,N.Links-iLinks[i]);
        N.Links:=N.Links-iLinks[i];
        dio.put_inode(i,N); warn("writting inode %d",i);
        IF N.Links=0 THEN incl(iBusy,i) ELSE excl(iBusy,i) END;
        AGAIN:=TRUE
      END
    END
  END
END correct_links;

PROCEDURE correct_volume;
BEGIN
  err.set_head('%sCorrect block map\r',spaces);
  check_blocks;
  err.set_head('%sCorrect double usage of blocks\r',spaces);
  check_dbl;
  err.set_head('%sCorrect inode map\r',spaces);
  check_sets;
  err.set_head('%sCollect anonim files and subtrees\r',spaces);
  collect_anonims;
  err.set_head('%sCorrect links\n',spaces);
  correct_links;
  err.set_head('')
END correct_volume;

----------------------  ERROR REPORTS  -------------------------
                      -----------------

PROCEDURE ss(n: INTEGER): CHAR;
BEGIN
  IF (n=0) OR (n MOD 100=11) THEN RETURN "s" END;
  IF n MOD 10 = 1 THEN RETURN " " ELSE RETURN "s" END
END ss;

PROCEDURE app_errs;
BEGIN
  IF b_err_busy>0 THEN
    err.put_error('%3d wrong used block%c corrected',b_err_busy,ss(b_err_busy))
  END;
  IF b_err_free>0 THEN
    err.put_error('%3d wrong free block%c corrected',b_err_free,ss(b_err_free))
  END;
  IF f_err_busy>0 THEN
    err.put_error('%3d wrong used inode%c corrected',f_err_busy,ss(f_err_busy))
  END;
  IF f_err_free>0 THEN
    err.put_error('%3d wrong free inode%c corrected',f_err_free,ss(f_err_free))
  END;
END app_errs;

PROCEDURE write_errs(VAL name: ARRAY OF CHAR);
  VAR s: str80;
    len: INTEGER;
    out: BIO.FILE;
BEGIN
  BIO.open(out,name,"wa");
  IF NOT BIO.done THEN BIO.create(out,name,"w",4096) END;
  IF NOT BIO.done THEN RETURN END;
  WHILE err.get_error(s,len) DO
    BIO.write(out,SYSTEM.ADR(s),len);
    BIO.putch(out,ASCII.NL);
    IF NOT BIO.done THEN RETURN END
  END;
  BIO.close(out)
END write_errs;

PROCEDURE flush_errs;
  VAR s: str80;
    len: INTEGER;
BEGIN
  WHILE err.get_error(s,len) DO END
END flush_errs;

PROCEDURE help;
  VAR out: BIO.FILE;

  PROCEDURE open_out();
    VAR i: INTEGER;
       fn: str80;
      app: BOOLEAN;
       en: STRING;
  BEGIN
    FOR i:=0 TO HIGH(arg.words) DO
      IF (HIGH(arg.words[i])>0) & (arg.words[i,0]=">") THEN
        str.sub_str(fn,arg.words[i],1,HIGH(arg.words[i]));
        app:=(fn[0]=">");
        IF app THEN
          str.delete(fn,0,1); BIO.open(out,fn,"wa");
          IF BIO.done THEN RETURN END
        END;
        BIO.create(out,fn,'w',4096);
        IF BIO.done THEN RETURN END
      END
    END;
    env.get_str(env.tty,en);
    IF env.done THEN
      BIO.open(out,en,'w');
      IF BIO.done THEN RETURN END
    END;
    HALT
  END open_out;

BEGIN
  open_out;
  BIO.print(out,
    '  "fschk"  file system check utility programm (c) KRONOS\n'
    'usage:\n'
    '  fschk  drv_name [ reports_file ] [+W] [-h]\n'
    'samples:\n'
    '  "fschk /dev/dk0   " -- check file system at "/dev/dk0"\n');
  BIO.print(out,
    '  "fschk /dev/dk0 +W" -- check & correct "/dev/dk0"\n'
    '  "fschk /dev/dk0 /lost/errors" -- check "/dev/dk0";\n'
    '                  writing reports to "/lost/errors"\n'
    "                                      Hady Nov 23 90.\n");
  BIO.close(out)
END help;

PROCEDURE CHECK;
BEGIN
  AGAIN:=FALSE;
  create_sets;
  iterate_volume;  correct_volume;
  IF NOT AGAIN THEN dio.release END;
  app_errs;
  IF SAVE & NOT AGAIN & (HIGH(arg.words)>0) THEN
    write_errs(arg.words[1]); flush_errs
  END;
  err.release
END CHECK;

PROCEDURE init;
  VAR c: CHAR;
BEGIN
  ABORT:=FALSE;
  low.fill(invalid,0);
  invalid[00c]:=1c;     invalid["/"]:=1c;
  invalid[" "]:=1c;     invalid[":"]:=1c
END init;

BEGIN
  WRITE:=arg.flag('+','W');
  init;
  IF (HIGH(arg.words)<0) OR arg.flag("-","h") THEN help; HALT END;
  dio.mount(arg.words[0]); warn('device "%s"',arg.words[0]);
  tty.print('Device "%s": blocks %d, files %d\n',
                     arg.words[0], dio.no_b, dio.no_i);
  SAVE :=(HIGH(arg.words)>0) & (HIGH(arg.words[1])>=0) & (arg.words[1]#"");
  tty.set_cursor(0);
  REPEAT CHECK UNTIL NOT AGAIN OR NOT WRITE;
END fschk.
