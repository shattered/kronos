MODULE DKwsFD; (*$U+ Leo 05-Mar-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT   os: osKernel;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  fs : osFiles;
IMPORT  env: tskEnv;


CONST
  ok     = err.ok;
  VEC    = 10h;
  TRIES  = 3;
  (* DEFAULT DRIVE SPEC *)
  SSC    = 10;
  HEADS  =  2;
  TRACKS = 40;
  SECS   =  5;
  SECSIZE= INTEGER(1<<SSC);

TYPE  set = BITSET;
      int = INTEGER;
     WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE dt(): BITSET;  CODE cod.getm cod.copt 2 cod.bic cod.setm END dt;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE self(): ADDRESS; CODE cod.activ END self;

PROCEDURE transfer(VAR f,t: os.PROCESS);    CODE cod.tra  END transfer;


CONST
  (* step rate *)
  _ms3  = {   };     _ms10 = {1  };
  _ms6  = {  0};     _ms15 = {1,0};

  (* seek comands *)
  _restore    = {      3    };
  _seek       = {    4,3,2  };
  _step_fwd   = {6,  4,3,1,0};
     _down    = {      3    };       (* head down        *)
     _chktrk  = {        2  };       (* check after seek *)

  (* i/o  comands *)
  _read       = { 7         };
  _write      = { 7,5       };
     _long    = {    4      };
     _side0   = {           };
     _side1   = {     3     };
     _delay   = {      2    };       (* 15ms delay before  i/o  *)
     _chkside = {       1   };       (* check side durring i/o  *)
  _format     = { 7,6,5,4,2 };

  _reset      = { 7,6, 4    };      (* may be executed any time *)

  (* mode *)
  _off = { };
  _fd0 = {0};
  _fd1 = {1};
  _sd0 = { };
  _sd1 = {2};
  _mtr = {3};  (* motor on      *)
  _ssr = {4};  (* set step rate *)
  _FM  = {5};
  _MFM = { };
  _in5 = { };
  _in8 = {6};
  _ei  = {7};

  SIDE0 = _ei+_mtr+_ssr+_sd0;
  SIDE1 = _ei+_mtr+_ssr+_sd1;

TYPE DMA =
  RECORD
    adr0: INTEGER;  cnt0: INTEGER;
    adr1: INTEGER;  cnt1: INTEGER;
    adr2: INTEGER;  cnt2: INTEGER;
    adr3: INTEGER;  cnt3: INTEGER;
    csr : INTEGER;
  END;

VAR
  BUF    : ADDRESS;
  csr,slu: POINTER TO BITSET;
  cyl,rsc: POINTER TO INTEGER;
  dtr    : POINTER TO INTEGER;
  dma    : POINTER TO DMA;
  cdrv   : INTEGER;
  ipted  : os.PROCESS;
  driver : os.PROCESS;
  ready  : os.signal_rec;
  pause  : INTEGER;
  ports  : ARRAY [0..1] OF
           RECORD
             ssc    : INTEGER;
             minsec : INTEGER;
             maxsec : INTEGER;
             cyls   : INTEGER;
             heads  : INTEGER;
             ressec : INTEGER;
             secsize: INTEGER;

             ctrk   : INTEGER;
             motor  : BOOLEAN;
             trk80  : BOOLEAN;  -- hard drive type (40 trk -- FALSE; 80 trk TRUE);
             sft40  : BOOLEAN;  -- soft simulation of 48 tpi
           END;

  chain  : INTEGER;     chain_cmd: BITSET;

PROCEDURE interrupt;
BEGIN
  LOOP
    IF (csr^*{0..5,7}) + set(chain<=0) = {} THEN
      INC(rsc^);
      IF chain>1 THEN csr^:=chain_cmd; DEC(chain);
        IF chain_cmd*{5..7}=_read THEN os.send(ready) END
      ELSE
        chain:=0; os.send(ready)
      END
    ELSIF chain<=0 THEN
      os.send(ready)
    ELSE
      IF chain_cmd*{5..7}=_write THEN chain:=1 END;
      IF chain>0 THEN
        os.send(ready); DEC(chain); INC(ready.cou,chain)
      END
    END;
    transfer(driver,ipted)
  END
END interrupt;

PROCEDURE argus;
BEGIN
  IF pause<=0 THEN RETURN END;
  DEC(pause);
  IF pause>0  THEN RETURN END;
  slu^:=_fd0; slu^:=_off; ports[0].motor:=FALSE;
  slu^:=_fd1; slu^:=_off; ports[1].motor:=FALSE; cdrv:=-1
END argus;

PROCEDURE error(cmd,erc: BITSET): INTEGER;
  VAR x: BITSET;
BEGIN
  x:=set(err.io_error);
  IF erc*{7}#{} THEN x:=x+set(err.not_ready) END;
  IF erc*{3}#{} THEN x:=x+set(err.data_crc ) END;
  IF erc*{0}#{} THEN x:=x+set(err.time_out ) END;
  IF cmd*{7}={} THEN (* seek commands *)
    IF erc*{4}#{} THEN x:=x+set(err.seek_err) END
  ELSE               (* i/o  commands *)
    IF erc*{4}#{} THEN x:=x+set(err.miss_sid) END;
    IF erc*{2}#{} THEN x:=x+set(err.over_run) END;
    IF erc*{1}#{} THEN x:=x+set(err.frm_err)  END;
    IF    cmd*_read=_read THEN
      IF erc*{5}#{} THEN x:=x+set(err.bad_block) END
    ELSIF cmd*_write=_write THEN
      IF erc*{5}#{} THEN x:=x+set(err.hw_fail  ) END;
      IF erc*{6}#{} THEN x:=x+set(err.write_pro) END
    END
  END;
  RETURN int(x)
END error;

PROCEDURE exec(cmd: BITSET; t: INTEGER): INTEGER;
  VAR r: INTEGER;
   mask: BITSET;
  m,erc: BITSET;
BEGIN (* note: _read command never exec()'s *)
  m:=di();
    ready.cou:=0; csr^:=cmd;
    IF    cmd*{7}={}        THEN mask:={0,3,4,7}
    ELSE                         mask:={0..7}
    END;
    r:=os.wait_del(t,ready); erc:=csr^;
  ei(m);
  IF (r=0) & (erc*mask={}) THEN RETURN ok END;
  (* ERROR OCCURED: *)
  csr^:=_reset;
  IF r<0 THEN r:=err.time_out END;
  IF r>0 THEN r:=err.ipted_op END;
  RETURN int(set(r)+set(error(cmd,erc)))
END exec;

PROCEDURE select(d: INTEGER): INTEGER;

  VAR res: INTEGER;

  PROCEDURE _select;
  BEGIN
    WITH ports[d] DO
      cdrv:=d;
      IF motor THEN slu^:=_mtr+_ssr; slu^:=SIDE0+{d}; RETURN END;
      slu^:=_ssr+_ei;     slu^:=_ssr+_ei+{d};
      IF csr^*{7}={} THEN slu^:=SIDE0+{d}; RETURN END;
      ready.cou:=0;  (* wait until motor will be running *)
      slu^:=SIDE0+{d};
      csr^:=_reset+{0}; (* {0} interrupt will be happen when drive ready *)
      IF os.wait_del(500,ready)=0 THEN RETURN END;
      slu^ :={d};    slu^:={};  csr^:=_reset;
      motor:=FALSE;  cdrv:=-1;  ctrk:=-1;  res:=err.not_ready
    END
  END _select;

  VAR m: BITSET;

BEGIN
  m:=dt(); (* disable timer to prevent "argus" activity *)
    pause:=0;   chain:=0;  chain_cmd:={};
    IF cdrv=d THEN ei(m);  RETURN ok END;
    res:=ok;  _select;
  ei(m);
  WITH ports[d] DO
    IF res#ok  THEN RETURN res END;
    IF ctrk>=0 THEN cyl^:=ctrk END;  motor:=TRUE
  END;
  RETURN ok
END select;

PROCEDURE seek00(d: INTEGER): INTEGER;
  VAR r: INTEGER;
BEGIN
  WITH ports[d] DO
    ctrk:=-1;
    r:=select(d);
    IF r#ok THEN RETURN r END;
    r:=exec(_restore,500); (* recalibrate *)
    IF r#ok THEN RETURN r END;
    IF csr^*{2}#{} THEN cyl^:=0; ctrk:=0; RETURN ok END;
    RETURN err.seek_0
  END
END seek00;

PROCEDURE step(d: INTEGER): INTEGER;
  (* this procedure for future format proc only *)
  VAR r: INTEGER;
BEGIN
  r:=select(d);            IF r#ok THEN RETURN r END;
  r:=exec(_step_fwd,50);   IF r#ok THEN RETURN r END;
  WITH ports[d] DO
    INC(ctrk);
    IF NOT sft40 THEN RETURN ok END;
    r:=exec(_step_fwd,50)
  END;
  RETURN r
END step;

PROCEDURE move_head(d,trk: INTEGER): INTEGER;
  VAR m: BITSET;
      r: INTEGER;
BEGIN
  WITH ports[d] DO
    IF trk>=cyls THEN RETURN err.seek_err END;
    r:=select(d);
    IF (ctrk=trk) OR (r#ok) THEN RETURN r END;
    IF ctrk<0  THEN
      r:=seek00(d);
      IF (trk=0) OR (r#ok) THEN RETURN r END
    END;
    IF sft40 THEN
      cyl^:=ctrk*2;  dtr^:=trk*2;
      r:=exec(_seek-_chktrk,100);
      IF r#ok THEN RETURN r END;
      ctrk:=trk
    END;
    cyl^:=ctrk;    dtr^:=trk;
    r:=exec(_seek,100);
    IF r#ok THEN RETURN r END;
    ctrk:=trk;   cyl^:=trk
  END;
  RETURN ok
END move_head;

PROCEDURE tracks(drn: INTEGER): INTEGER;
  CONST TIMEOUT=50000;
  VAR t0,t1: INTEGER;
  PROCEDURE delay; BEGIN END delay;
  PROCEDURE off  ; BEGIN slu^:={drn}; slu^:=_off END off;
BEGIN
  slu^:=_ssr;     slu^:=_ssr+{drn};
  slu^:=_ssr+_mtr+{drn};
  csr^:=_reset; delay;
  t0:=0;
  REPEAT INC(t0) UNTIL (csr^*{0}={}) OR (t0>TIMEOUT);
  IF t0>TIMEOUT  THEN  off; RETURN 40 END;

  csr^:=_restore; delay; t0:=0;
  REPEAT INC(t0) UNTIL (csr^*{0,2}={2}) OR (t0>TIMEOUT);
  cyl^:=0;
  IF t0>TIMEOUT  THEN  off; RETURN 40 END;

  cyl^:=0;  dtr^:=79;  csr^:=_seek-_chktrk;  delay; t0:=0;
  REPEAT INC(t0) UNTIL (csr^*{0}={}) OR (t0>TIMEOUT);
  IF t0>TIMEOUT  THEN  off; RETURN 0 END;
  cyl^:=79;            csr^:=_restore; delay; t1:=0;
  REPEAT INC(t1) UNTIL (csr^*{0,2}={2}) OR (t1>TIMEOUT);
  IF t1>TIMEOUT  THEN  off; RETURN 40 END;
  cyl^:=0; off;
  IF t1<t0*3 DIV 4 THEN RETURN 40 ELSE RETURN 80 END
END tracks;

PROCEDURE cpu_buf(no,size: INTEGER);
BEGIN
  WITH dma^ DO
    csr :=85h;
    adr1:=no;     adr1:=no   DIV 100h;
    cnt1:=size;   cnt1:=size DIV 100h + 40h;
    csr :=87h
  END
END cpu_buf;

PROCEDURE buf_cpu(no,size: INTEGER);
BEGIN
  WITH dma^ DO
    csr:=85h;
    adr1:=no;     adr1:=no   DIV 100h;
    cnt1:=size;   cnt1:=size DIV 100h + 80h;
    csr:=87h
  END
END buf_cpu;

PROCEDURE buf_fd;
BEGIN
  WITH dma^ DO
    csr :=86h;
    adr0:=00;   adr0:=00h;
    cnt0:=00;   cnt0:=94h;
    csr :=87h
  END
END buf_fd;

PROCEDURE fd_buf;
BEGIN
  WITH dma^ DO
    csr :=86h;
    adr0:=00;  adr0:=00h;
    cnt0:=00;  cnt0:=54h;
    csr :=87h
  END
END fd_buf;

PROCEDURE fd2buf(size: INTEGER);
BEGIN
  WITH dma^ DO
    csr :=86h;
    adr0:=0;             adr0:=0;
    cnt0:=size MOD 256;  cnt0:=80h+size DIV 256;
    csr :=87h
  END
END fd2buf;

PROCEDURE read(VAR r: req.REQUEST);

  PROCEDURE word(a: ADDRESS): WORD;
  CODE
    cod.copt cod.lsw0 cod.swap
    cod.copt cod.lsw0 cod.swap
    cod.copt cod.lsw0 cod.swap
             cod.lsw0
    cod.li8  cod.rol  cod.or
    cod.li8  cod.rol  cod.or
    cod.li8  cod.rol  cod.or
  END word;

  VAR i: INTEGER;       w: INTEGER;
    len: INTEGER;     end: ADDRESS;
    trk: INTEGER;     ptr: ADDRESS;
    sec: INTEGER;    side: INTEGER;
    spt: INTEGER;    secs: INTEGER;

BEGIN
  WITH ports[r.drn] DO
    sec :=r.ofs + ressec;
    secs:=(maxsec-minsec+1);
    trk :=sec DIV secs;
    sec :=sec MOD secs;
    side:=trk MOD heads;
    trk :=trk DIV heads;
    ptr :=r.buf;
    len :=r.len; r.len:=0;
    r.res:=move_head(r.drn,trk);
    IF r.res#ok THEN RETURN END;
    REPEAT
      IF side=0 THEN slu^:=SIDE0+{r.drn}; chain_cmd:=_read+_side0
      ELSE           slu^:=SIDE1+{r.drn}; chain_cmd:=_read+_side1
      END;
      spt:=len;
      IF spt>secs-sec THEN spt:=secs-sec END;
      chain:=spt;
      fd_buf;
      buf_cpu(0,spt<<ssc);
      rsc^:=sec+minsec;
      ready.cou:=0;  csr^:=chain_cmd;
      i:=spt;
      REPEAT
        w:=os.wait_del(50,ready);
        IF w#0 THEN
          IF w<0 THEN r.res:=err.time_out ELSE r.res:=err.ipted_op END;
          INC(r.len,spt-i); RETURN
        END;
        end:=ptr+secsize DIV 4;
        REPEAT ptr^:=word(BUF); INC(ptr) UNTIL ptr=end;
        i:=i-1
      UNTIL i=0;
      IF csr^*{0..5,7}#{} THEN r.res:=error(chain_cmd,csr^); RETURN END;
      IF rsc^#sec+spt+1   THEN r.res:=err.prog_err;          RETURN END;
      INC(r.len,spt);     DEC(len,spt);
      IF len=0 THEN RETURN END;
      sec:=0;
      INC(side);
      IF side=heads THEN
        side:=0; INC(trk); r.res:=move_head(r.drn,trk)
      END
    UNTIL r.res#ok
  END
END read;

PROCEDURE four_bytes(w: SYSTEM.WORD);
CODE
  cod.copt cod.li8    cod.rol cod.swap
  cod.copt cod.lib 16 cod.rol cod.swap
  cod.copt cod.li8    cod.ror cod.swap
END four_bytes;

PROCEDURE out(reg: ADDRESS); CODE cod.swap cod.ssw0 END out;

PROCEDURE write(VAR r: req.REQUEST);
  VAR i: INTEGER;     end: ADDRESS;
    len: INTEGER;     ptr: ADDRESS;
    trk: INTEGER;    side: INTEGER;
    sec: INTEGER;    secs: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    sec :=r.ofs + ressec;
    secs:=(maxsec-minsec+1);
    trk :=sec DIV secs;
    sec :=sec MOD secs;
    side:=trk MOD heads;
    trk :=trk DIV heads;
    ptr :=r.buf;
    len :=r.len; r.len:=0;
    r.res:=move_head(r.drn,trk);
    IF r.res#ok THEN RETURN END;
    IF csr^*{6}#{} THEN r.res:=err.write_pro; RETURN END;
    REPEAT
      i:=len;
      IF i>secs-sec THEN i:=secs-sec END;
      cpu_buf(0,i<<ssc);
      end:=ptr+int(i<<(ssc-2));
      REPEAT
        four_bytes(ptr^);
        out(BUF); out(BUF); out(BUF); out(BUF);
        ptr:=ptr+1
      UNTIL ptr=end;
      buf_fd;
      IF side=0 THEN slu^:=SIDE0+{r.drn}; chain_cmd:=_write+_side0
      ELSE           slu^:=SIDE1+{r.drn}; chain_cmd:=_write+_side1
      END;
      chain:=i;
      rsc^ :=sec+minsec;
      r.res:=exec(chain_cmd+_delay+_chkside,500);
      IF r.res#ok     THEN RETURN END;
      IF rsc^#sec+i+1 THEN r.res:=err.prog_err; RETURN END;
      DEC(len,i); INC(r.len,i);
      IF len=0 THEN RETURN END;
      sec:=0;
      INC(side);
      IF side=heads THEN
        side:=0; INC(trk); r.res:=move_head(r.drn,trk)
      END
    UNTIL r.res#ok
  END
END write;

VAR FLUSH: BOOLEAN;
    FIRST: BOOLEAN;

PROCEDURE READ(VAR r: req.REQUEST);
  VAR t,len: INTEGER;
BEGIN
  IF r.buf=NIL THEN r.res:=err.bad_parm; RETURN END;
  t:=TRIES; len:=r.len;  FLUSH:=TRUE;
  REPEAT
    read(r); DEC(t);
    IF (r.res=ok) OR (t=0) THEN pause:=200; RETURN END;
    IF r.res=err.not_ready THEN pause:= 20; RETURN END;
    IF ODD(t) THEN r.res:=seek00(r.drn) ELSE csr^:=_reset END;
    ports[r.drn].ctrk:=-1; r.len:=len
  UNTIL t=0
END READ;

PROCEDURE WRITE(VAR r: req.REQUEST);
  VAR t,len: INTEGER;
BEGIN
  t:=TRIES; len:=r.len;  FLUSH:=TRUE;
  REPEAT
    write(r); DEC(t);
    IF (r.res=ok) OR (t=0) THEN pause:=200; RETURN END;
    IF r.res=err.not_ready THEN pause:= 20; RETURN END;
    IF ODD(t) THEN r.res:=seek00(r.drn) ELSE csr^:=_reset END;
    ports[r.drn].ctrk:=-1; r.len:=len
  UNTIL t=0
END WRITE;

PROCEDURE _prefmt(drn: INTEGER;
                  VAR format: ARRAY OF CHAR; VAR id: ARRAY OF INTEGER);
  VAR i: INTEGER;     gap1: INTEGER;
     dd: BOOLEAN;     gap3: INTEGER;
    sec: INTEGER;     scod: INTEGER;
    pos: INTEGER;     secs: INTEGER;

   PROCEDURE put(c: INTEGER; len: INTEGER);
   BEGIN
     WHILE len>0 DO format[pos]:=CHAR(c); INC(pos); DEC(len) END;
   END put;

BEGIN
  FLUSH:=TRUE;
  pos:=0;
  WITH ports[drn] DO
    scod:=ssc-7;
    secs:=(maxsec-minsec+1);
    IF secs*secsize>3000 THEN
      gap1:=32; gap3:=32;
      IF scod=2 THEN
        IF secs=8 THEN gap1:=60 ELSIF secs=10 THEN gap3:=30 END
      ELSIF scod=3 THEN gap1:=48; gap3:=40
      END;
      put(4Eh,gap1);
      FOR sec:=minsec TO maxsec DO
        put(0,10);   put(0F5h,3);  put(0FEh,1);
        id[sec-minsec]:=pos;
        put(0,2);    put(sec,1);   put(scod,1);   put(0F7h,1);
        put(4Eh,22); put(0,12);    put(0F5h,3);   put(0FBh,1);
        put(0E5h,secsize);  put(0F7h,1);  put(4Eh,gap3);
      END;
      put(4Eh,HIGH(format)+1-pos)
    ELSE
      gap3:=10;
      IF scod<2 THEN gap1:=40 ELSE gap1:=20 END;
      put(0FFh,gap1);
      FOR sec:=minsec TO maxsec DO
        put(0,6); put(0FEh,1);
        id[sec-minsec]:=pos;
        put(0,2);     put(sec,1);  put(scod,1);   put(0F7h,1);
        put(0FFh,11); put(0,6);    put(0FBh,1);
        put(0E5h,secsize);         put(0F7h,1);   put(0FFh,gap3);
      END;
      put(0FFh,HIGH(format)+1-pos)
    END
  END
END _prefmt;

CONST MAGIC=44467377h;

PROCEDURE FORMAT(VAR r: req.REQUEST);
  VAR i: INTEGER;
     id: POINTER TO ARRAY [0..31] OF INTEGER;
    ptr: ADDRESS;
    end: ADDRESS;     sec: INTEGER;
    buf: STRING;     secs: INTEGER;
    trk: INTEGER;    side: INTEGER;
BEGIN
  IF (r.ofs<0) & (r.buf=NIL) THEN
    r.len:=16*1024;  r.res:=err.not_enough;  RETURN
  END;
  IF (r.buf=NIL) OR (r.len<12*1024+BYTES(id^)) THEN
    r.len:=16*1024;  r.res:=err.bad_parm;  RETURN
  END;
  r.buf^:=MAGIC;
  id:=r.buf+1;
  buf^.ADR :=r.buf+SIZE(id^)+1;
  buf^.HIGH:=12*1024-1;
  WITH ports[r.drn] DO
    IF r.ofs<0      THEN
      _prefmt(r.drn,buf,id^); r.buf^:=MAGIC;
      FIRST:=TRUE;
      RETURN
    END;
    IF r.buf^#MAGIC THEN r.res:=err.bad_parm;                    RETURN END;
    r.ofs:=r.ofs+ressec;
    secs:=(maxsec-minsec+1);
    trk :=r.ofs DIV secs;
    side:=trk   MOD heads;
    trk :=trk   DIV heads;
    IF FLUSH THEN
      cpu_buf(0,BYTES(buf));
      ptr:=SYSTEM.ADR(buf);
      end:=ptr + SIZE(buf);
      REPEAT
        four_bytes(ptr^); out(BUF); out(BUF); out(BUF); out(BUF); ptr:=ptr+1
      UNTIL ptr=end;
      FLUSH:=FALSE
    END;
    IF FIRST THEN
      FIRST:=FALSE;
      r.res:=seek00(r.drn); IF r.res#ok THEN RETURN END;
      FOR i:=1 TO trk DO
        r.res:=step(r.drn);
        IF r.res#ok THEN RETURN END
      END
    ELSIF ports[r.drn].ctrk=trk-1 THEN
      r.res:=step(r.drn);
      IF r.res#ok THEN RETURN END
    END;
    IF {6}*csr^#{}           THEN r.res:=err.write_pro;  RETURN END;
    IF ports[r.drn].ctrk#trk THEN r.res:=err.hw_fail;    RETURN END;
    chain:=0;  chain_cmd:={};
    FOR sec:=minsec TO maxsec DO
      cpu_buf(id^[sec-minsec],2);  BUF^:=trk;  BUF^:=side
    END;
    fd2buf(BYTES(buf));
    IF side=0 THEN slu^:={r.drn}+SIDE0 ELSE slu^:={r.drn}+SIDE1 END;
    r.res:=exec(_format,100);  pause:=200
  END
END FORMAT;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  WITH ports[r.drn] DO
    r.dmode :=req.ready+req.floppy+req.fmttrk;
    r.ssc   :=ssc;     r.secsize:=INTEGER(1<<ssc);
    r.minsec:=minsec;  r.maxsec :=maxsec;
    r.cyls  :=cyls;    r.heads  :=heads;
    r.ressec:=ressec;
    r.dsecs :=heads*cyls*(maxsec-minsec+1)
  END
END get_spec;

PROCEDURE set_spec(VAR r: req.REQUEST);
BEGIN
  WITH ports[r.drn] DO
    IF NOT trk80 & (r.cyls>40)        THEN r.res:=err.bad_parm; RETURN END;
    IF NOT (r.heads IN {1,2})         THEN r.res:=err.bad_parm; RETURN END;
    IF (r.ssc<7) OR (r.ssc>10)        THEN r.res:=err.bad_parm; RETURN END;
    IF (r.ressec>255)                 THEN r.res:=err.bad_parm; RETURN END;
    IF (r.minsec<1) OR (r.maxsec>255) THEN r.res:=err.bad_parm; RETURN END;
    ssc   :=r.ssc;     secsize:=INTEGER(1<<ssc);
    minsec:=r.minsec;  maxsec :=r.maxsec;
    cyls  :=r.cyls;    heads  :=r.heads;
    ressec:=r.ressec;
    sft40 :=(cyls<=40) & trk80; ctrk:=-1
  END
END set_spec;

PROCEDURE mount(VAR r: req.REQUEST);
  VAR s: req.REQUEST;
    LAB: POINTER TO ARRAY [0..31] OF CHAR;
BEGIN
  LAB:=r.buf;
  r.len:=1; r.ofs:=0;  FLUSH:=TRUE;
  read(r);    pause:=200;
  IF r.res#ok THEN read(r) END;
  IF r.res#ok THEN RETURN  END;
  IF (LAB^[8+0]#"X") OR (LAB^[8+1]#"D") OR (LAB^[8+2]#"0") THEN RETURN END;
  s.drn   :=r.drn;
  get_spec(s);
  s.res   :=ok;
  s.cyls  :=ORD(LAB^[8+4])+ORD(LAB^[8+5])*256;
  s.minsec:=ORD(LAB^[8+6]);      s.heads:=ORD(LAB^[8+8]);
  s.maxsec:=ORD(LAB^[8+7]);      s.ssc  :=ORD(LAB^[8+9]);
  s.ressec:=ORD(LAB^[8+10]);
  set_spec(s);
  r.res:=s.res
END mount;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  r.res:=ok;
  CASE r.op OF
    |req.READ     : READ (r)
    |req.WRITE    : WRITE(r)
    |req.MOUNT    : mount(r)
    |req.UNMOUNT  :
    |req.GET_SPEC : get_spec(r)
    |req.SET_SPEC : set_spec(r)
    |req.POWER_OFF:
    |req.FORMAT   : FORMAT(r)
  ELSE
    r.res:=err.inv_op
  END
END doio;

PROCEDURE stop;
BEGIN
  csr^:={7,6,4};
  slu^:={0}; slu^:={}; ports[0].motor:=FALSE;
  slu^:={1}; slu^:={}; ports[1].motor:=FALSE;
  cdrv:=-1;
  os.remove_action(argus)
END stop;

PROCEDURE start_refresh;
BEGIN
  WITH dma^ DO
    csr :=000h;
    adr3:=000h;  adr3:=000h;
    cnt3:=0FFh;  cnt3:=0BFh;
    adr2:=000h;  adr2:=000h;
    cnt2:=0FFh;  cnt2:=0BFh;
    csr :=084h
  END
END start_refresh;

PROCEDURE init_driver;
  VAR r: INTEGER;
BEGIN
  dma:=ADDRESS(840000h);                 cdrv:=-1;
  csr:=ADDRESS(840010h);        ports[0].ctrk:=-1;
  cyl:=ADDRESS(840011h);        ports[1].ctrk:=-1;
  rsc:=ADDRESS(840012h);
  dtr:=ADDRESS(840013h);
  slu:=ADDRESS(840014h);
  BUF:=ADDRESS(850000h);
  csr^:=_reset;
  cyl^:=1;
  FOR r:=0 TO 15 DO END;
  IF cyl^=0 THEN HALT(err.not_ready) END;
  cyl^:=0;

  pause:=0;
  chain:=0;  chain_cmd:={};
  os.ini_signal(ready,os.sendup+os.break,0);
  r:=os.insert_action(argus);
  IF r#ok THEN HALT(r) END;
  start_refresh;
  slu^:=_fd0; slu^:=_off; ports[0].motor:=FALSE;
  slu^:=_fd1; slu^:=_off; ports[1].motor:=FALSE;
  csr^:=_reset;
END init_driver;

PROCEDURE define;
  VAR i: INTEGER;
BEGIN
  init_driver;
  FOR i:=0 TO 1 DO
    WITH ports[i] DO
      ressec :=0;         ssc  :=SSC;
      minsec :=1;         heads:=HEADS ;
      maxsec:=SECS;       cyls :=tracks(i);
      secsize:=SECSIZE;   trk80:=(cyls=80);
      sft40  :=FALSE
    END
  END;
  env.final(stop);
  i:=fs.define_driver("fd0","fd0",0,fs.disk,doio);
  IF i#err.ok THEN HALT(i) END;
  i:=fs.define_driver("fd1","fd0",1,fs.disk,doio);
  IF i#err.ok THEN HALT(i) END;
  env.put_str(env.info,"fd0 fd1",TRUE);
END define;

PROCEDURE waitIPT;
  VAR m: BITSET;
    adr: ADDRESS;
BEGIN
  m:=di();
  driver:=self();
  adr:=VEC*2; adr^:=driver;
  adr:=adr+1; adr^:=SYSTEM.ADR(ipted);
  env.become_ipr;
  os.suspend(os.active(),-1);
  interrupt
END waitIPT;

BEGIN
  ASSERT(TRIES>0); FLUSH:=TRUE; FIRST:=TRUE;
  define;
  waitIPT
END DKwsFD.
