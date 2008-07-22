MODULE DK6fd4110; (* Igo 12-Jan-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT   os: osKernel;
IMPORT  drv: osDrivers;

TYPE ADDRESS = SYSTEM.ADDRESS;
     PROCESS = ADDRESS;
     WORD    = SYSTEM.WORD;

     bs      = BITSET;
     int     = INTEGER;

PROCEDURE inp(reg: INTEGER): WORD;        CODE cod.inp END inp;
PROCEDURE out(reg: INTEGER; data: WORD); CODE cod.out END out;

PROCEDURE setm(m: BITSET); CODE cod.setm END setm;
PROCEDURE getm(): BITSET;  CODE cod.getm END getm;

PROCEDURE move(to, from: ADDRESS; size: INTEGER); CODE cod.move END move;
PROCEDURE transfer(VAR from, to: PROCESS); CODE cod.tra END transfer;

CONST
(*******************************************************************
                    REGISTERS  & IPT VECTOR
********************************************************************)
   CSR=177170b DIV 2;
   DTR=CSR+1;
   VEC=264b DIV 4;

   double=400b;
   single=000b;
   double_sector_size=256;
   single_sector_size=128;
   sectors=26;
   tracks=77;

(*******************************************************************
                            COMMANDS
********************************************************************)

   fillbuf  = 001b;
   readbuf  = 003b;
   write    = 005b;
   read     = 007b;
   format   = 011b;
   readstat = 013b;
   readerr  = 017b;

TYPE Ref256 = POINTER TO ARRAY [0..255] OF CHAR;

VAR fdDMA: Ref256;
   tryDMA: Ref256;
      bps: INTEGER; -- bytes per sector
  density: INTEGER;
    tries: INTEGER;
    ipted: ADDRESS;
   driver: ADDRESS;
      end: os.signal_rec;

PROCEDURE rdy(i: BITSET);
BEGIN
  REPEAT UNTIL (i*BITSET(inp(CSR))#{})
END rdy;

PROCEDURE geterror(drv:INTEGER): BITSET;
CONST ERR = ARRAY OF INTEGER
  {
  err.ok       , err.seek_0  , err.seek_0   , err.hw_fail ,
  err.bad_block, err.hw_fail , err.hw_fail  , err.miss_sid,
  err.write_pro, err.miss_sid, err.miss_sid , err.miss_sid,
  err.head_crc , err.seek_err, err.miss_sid , err.miss_sid,
  err.data_crc , err.hw_fail , err.hw_fail  , err.inv_dad ,
  err.io_error , err.inv_dad , err.io_error , err.hw_fail ,
  err.hw_fail  , err.hw_fail , err.hw_fail  , err.hw_fail ,
  err.hw_fail  , err.inv_dma , err.not_ready, err.hw_fail
  };

  VAR m: BITSET;
BEGIN
  m:=getm();
  rdy({5});
  setm(m-{0,1});
  out(CSR,readstat+density+drv*20b+100b);
  transfer(driver,ipted);
  setm(m);
  IF {15}*BITSET(inp(CSR))#{} THEN RETURN bs(err.io_error)  END;
  IF {7} *BITSET(inp(DTR))={} THEN RETURN bs(err.not_ready) END;
  rdy({5}); out(CSR,readerr+100b);
  rdy({7});
  setm(m-{0,1});
  out(DTR,INTEGER(fdDMA)*4);
  transfer(driver,ipted);
  setm(m);
  RETURN bs(ERR[int(fdDMA^[0]) DIV 8])
END geterror;

PROCEDURE reset(drv: INTEGER);
BEGIN out(CSR,40000b+drv*20b); rdy({5}) END reset;

PROCEDURE put_cmd(cmd,drv: INTEGER; sec,track: WORD): BITSET;
  VAR m: BITSET;
BEGIN
  m:=getm();
  rdy({5});  out(CSR,cmd+100b+drv*20b+density);
  rdy({7});  out(DTR,sec);
  rdy({7});
  setm(m-{0,1}); out(DTR,track);
  transfer(driver,ipted);
  setm(m);
  IF {15}*BITSET(inp(CSR))#{} THEN RETURN geterror(drv) END;
  RETURN bs(err.ok)
END put_cmd;

TYPE REQUEST = RECORD
                 cmd: INTEGER;
                 drv: INTEGER;
                 sec: INTEGER;
               track: INTEGER;
                 buf: ADDRESS;
                 err: BITSET;
               END;

PROCEDURE read_sec(VAR req: REQUEST);
BEGIN
  req.err:=bs(err.ok);
  req.err:=put_cmd(read   ,req.drv,req.sec+1,req.track);
  IF req.err#bs(err.ok) THEN RETURN END;
  req.err:=put_cmd(readbuf,req.drv,bps DIV 2,INTEGER(fdDMA)*4);
  move(req.buf,fdDMA,bps DIV 4);
END read_sec;

PROCEDURE write_sec(VAR req: REQUEST);
BEGIN
  req.err:=bs(err.ok);
  move(fdDMA,req.buf,bps DIV 4);
  req.err:=put_cmd(fillbuf,req.drv,bps DIV 2,INTEGER(fdDMA)*4);
  IF req.err#bs(err.ok) THEN RETURN END;
  req.err:=put_cmd(write,req.drv,req.sec+1,req.track);
END write_sec;

PROCEDURE Retry(VAR REQ: REQUEST; try: INTEGER);
  VAR r: REQUEST;
BEGIN
  r.drv  :=REQ.drv;         r.buf  :=tryDMA;
  r.sec  :=REQ.sec;
  r.track:=REQ.track;
  CASE try MOD 3 OF
    |0: INC(r.track);
        IF  r.track>=tracks THEN RETURN END;
    |1: DEC(r.track);
        IF  r.track<0 THEN RETURN END;
    |2: reset(r.drv); RETURN
  ELSE END;
  read_sec(r)
END Retry;

PROCEDURE TryIO(VAR r: REQUEST);
  CONST
    stop = bs(err.write_pro)+bs(err.seek_0)
          +bs(err.hw_fail)+bs(err.not_ready)-{8};
  VAR  try: INTEGER;
BEGIN
  r.err:=bs(err.ok);
  try:=tries*3;
  LOOP
    IF r.cmd=req.READ THEN  read_sec(r)
    ELSE                   write_sec(r)
    END;
    IF (r.err*stop#{}) OR (r.err=bs(err.ok)) OR (try<=0) THEN RETURN END;
    Retry(r,try);
    DEC(try)
  END;
END TryIO;

VAR drv_req: POINTER TO req.REQUEST;

PROCEDURE rw;
VAR       REQ: REQUEST;
    sizeINsec: INTEGER;
        f_sec: INTEGER;
     f_ph_sec: INTEGER;
    f_log_sec: INTEGER;
      log_sec: INTEGER;
      max_sec: INTEGER;
        round: INTEGER;
BEGIN
  WITH drv_req^ DO
    sizeINsec:=len;
    f_sec    :=ofs MOD sectors;
    f_ph_sec :=f_sec;
    f_log_sec:=0;

    REQ.cmd  :=op;
    REQ.drv  :=drn;
    REQ.track:=ofs DIV sectors;

    max_sec  :=len+f_sec;
    IF max_sec>sectors THEN max_sec:=sectors END;

    WHILE sizeINsec>0 DO
      log_sec:=f_log_sec+1;
      REQ.sec:=f_ph_sec +1;
      round  :=0;
      WHILE round<2 DO
        WHILE REQ.sec<max_sec DO
          REQ.buf:=buf+log_sec*(bps DIV 4);
          TryIO(REQ);
          res:=int(bs(res)+REQ.err);
          INC(log_sec,2); INC(REQ.sec,2); DEC(sizeINsec);
        END;
        INC(round); log_sec:=f_log_sec; REQ.sec:=f_ph_sec;
      END;
      INC(REQ.track);
      f_log_sec:=sectors-f_sec;
      f_ph_sec :=0;
      max_sec  :=len+f_sec-sectors;
    END;
  END;
END rw;

PROCEDURE form;
  VAR m: BITSET;
BEGIN
  m:=getm();
  rdy({5}); out(CSR,format+drv_req^.drn*16+double+100b);
  rdy({7});
  setm(m-{0,1});
  out(DTR,111b);
  transfer(driver,ipted);
  setm(m);
  IF {15}*BITSET(inp(CSR))#{} THEN
    drv_req^.res:=INTEGER(geterror(drv_req^.drn))
  END;
END form;

PROCEDURE monitor;
BEGIN
  LOOP
    IF (drv_req^.op=req.READ) OR (drv_req^.op=req.WRITE) THEN rw
    ELSIF drv_req^.op=req.FORMAT                         THEN form
    ELSE  drv_req^.res:=err.bad_parm;
    END;
    os.send(end);
    transfer(driver,ipted);
  END;
END monitor;

VAR  port: drv.port_ptr;

PROCEDURE fill_spec;
BEGIN
  port^.drn:=0;
  port^.state:={}; -- drv.wr_pro};
  port^.dsize  :=26*256*77;
  port^.cyls   :=77;
  port^.heads  :=1;
  port^.secs   :=26;
  port^.ssc    :=8;
  port^.secsize:=256;
  port^.res_sec:=0;
END fill_spec;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  r.res:=err.ok;
  CASE r.op OF
    |req.NOP      :
    |req.READ,
     req.WRITE    : drv_req:=SYSTEM.ADR(r);
                    transfer(ipted,driver);
                    os.wait(end);
    |req.MOUNT    :
    |req.UNMOUNT  :
    |req.POWER_OFF:
    |req.GET_SPEC : fill_spec
    |req.SET_SPEC : fill_spec; r.res:=err.inv_op ; RETURN
    |req.FORMAT   : IF r.ofs#0 THEN r.res:=err.ok; RETURN END;
                    drv_req:=SYSTEM.ADR(r);
                    transfer(ipted,driver);
                    os.wait(end);
  ELSE r.res:=err.inv_op; RETURN
  END;
END doio;

PROCEDURE define;
  VAR i: INTEGER;
  ports: ARRAY [0..0] OF drv.port_ptr;
BEGIN
   fdDMA:=ADDRESS(401000h);
  tryDMA:=ADDRESS(401200h);
  density:=double;
  bps:=double_sector_size;
  tries:=2;
  IF drv.prepare(TRUE,{0},ports)#err.ok THEN HALT(1) END;
  port:=ports[0]; ASSERT(port#NIL);
  port^.mode:=drv.disk;
  port^.doio:=doio;
  port^.name:='fd';
   fill_spec;
(* --------- Hady. 27-Jul-90
  port^.drn:=0;
  port^.state:={}; -- drv.wr_pro};
  port^.dsize  :=26*256*77;
  port^.cyls   :=77;
  port^.heads  :=1;
  port^.secs   :=26;
  port^.ssc    :=8;
  port^.secsize:=256;
  port^.res_sec:=0;
*)
  IF drv.define(ports)#err.ok THEN HALT(1) END;
  reset(0);
END define;

VAR m: BITSET;

PROCEDURE waitIPT;
  PROCEDURE self(): ADDRESS; CODE cod.activ END self;
  PROCEDURE setIPT;
    VAR adr: ADDRESS;
  BEGIN
    driver:=self();
    adr:=VEC*2; adr^:=driver;
    adr:=adr+1; adr^:=SYSTEM.ADR(ipted)
  END setIPT;
BEGIN
  os.ini_signal(end,{},0);
  setIPT;
  os.suspend(os.active(),-1);
  setm(m-{1});
  monitor;
END waitIPT;

BEGIN
  m:=getm();
  setm(m-{0..1});
  define;
  waitIPT;
END DK6fd4110.
