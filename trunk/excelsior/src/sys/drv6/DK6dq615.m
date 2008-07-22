MODULE DK6dq615; (* Igo 05-Apr-88. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT   os: osKernel;
IMPORT  drv: osDrivers;

---------------------  Р Е Г И С Т Р Ы  ------------------------
                     -------------------

CONST
  VEC   = 210b DIV 4;

  RKCS1 = 177440b DIV 2;  -- Control & Status 1
  RKWC  = 177442b DIV 2;  -- Word counter
  RKBA  = 177444b DIV 2;  -- Bus  Address
  RKDA  = 177446b DIV 2;  -- Disk Address
  RKCS2 = 177450b DIV 2;  -- Control & Status 2 (ERR0 from bit 10:)
   ERR0 = ARRAY OF INTEGER
          { err.prog_err, err.inv_dma, err.inv_dad, err.ok, err.chk_err};

  RKDS  = 177452b DIV 2;  -- Drive Status
  RKER  = 177454b DIV 2;  -- Error register
   ERR1 = ARRAY OF INTEGER
          { err.inv_op,   err.seek_err,   err.ok,       err.ok,
            err.ok,       err.hw_fail,    err.ecc_err,  err.bad_block,
            err.head_crc, err.inv_dad,    err.inv_dad,  err.write_pro,
            err.ok,       err.miss_sid,   err.unsafe,   err.data_crc };

  RKAS_OF = 177456b DIV 2;  -- Attantion Summary and Offset
  RKDC    = 177460b DIV 2;  -- Desired Cylinder Address
  RKXMA   = 177462b DIV 2;  -- Extended memory Address
  RKDB    = 177464b DIV 2;  -- Read/Write Buffer
  RKMR1   = 177466b DIV 2;  -- Maintenance 1
  RKECPS  = 177470b DIV 2;  -- ECC position
  RKECPT  = 177472b DIV 2;  -- ECC pattern
  RKMR2   = 177474b DIV 2;  -- Maintenance 2
  RKMR3   = 177476b DIV 2;  -- Maintenance 3

----------------------------------------------------------------

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;
  PROCESS = SYSTEM.ADDRESS;
       bs = BITSET;
      int = INTEGER;

CONST
  KB       = 100h;

  SSC      = 9;
  HEADS    = 5;
  SECSIZE  = int(1<<SSC);
  SECWORDS = SECSIZE DIV 4;
  SECS     = 16;
  TRACKS   = 1024;

  TRIES    = 16;

  CLEAR       = 105b;
  RECAL       = 113b;  -- recalibrate
  READ        = 121b;
  WRITE       = 123b;
  CHECK       = 131b;
  RESET       =  40b;

  q_base = 400000h;
  q_size = 100000h;
  q_buf  = 400000h;

VAR end: os.signal_rec;
  ipted: os.process;
 driver: os.process;


PROCEDURE inp(reg: INTEGER): BITSET;     CODE cod.inp END inp;
PROCEDURE out(reg: INTEGER; data: WORD); CODE cod.out END out;

PROCEDURE getm(): BITSET;  CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;

PROCEDURE transfer(VAR from,to: PROCESS); CODE cod.tra END transfer;

PROCEDURE move(to,from: ADDRESS; size: INTEGER); CODE cod.move END move;

PROCEDURE not_ready(time: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=128;
  REPEAT DEC(i) UNTIL ({7}*inp(RKCS1)#{}) OR (i=0);
  WHILE ({7}*inp(RKCS1)={}) & (time>0) DO os.delay(1); DEC(time) END;
  RETURN {7}*inp(RKCS1)={}
END not_ready;

PROCEDURE reset;
  VAR m : BITSET;
BEGIN
  m:=getm();
  setm(m-{0,1});
  out(RKCS2,RESET);
  setm(m);
  IF not_ready(100) THEN RETURN END;
  out(RKCS1,CLEAR-100b);
  IF not_ready(50) THEN END;
  out(RKCS1,100000b)
END reset;


PROCEDURE error(): BITSET;

  VAR x: BITSET;

  PROCEDURE unpack(E: BITSET; VAL err: ARRAY OF INTEGER);
    VAR i: INTEGER;
  BEGIN
    i:=0;
    WHILE i<=HIGH(err) DO
      IF E*{0}#{} THEN x:=x+bs(err[i]) END; E:=E>>1; INC(i)
    END
  END unpack;

BEGIN
  x:=bs(err.io_error);
  unpack(inp(RKCS2)>>10,ERR0);   unpack(inp(RKER),ERR1);
  out(RKCS1,100000b);
  RETURN x
END error;

PROCEDURE dma(op,drn,ofs: INTEGER; buf: ADDRESS; len: INTEGER): BITSET;
  VAR sec: INTEGER;
      cyl: INTEGER;    XMA: INTEGER;
      try: INTEGER;    XMB: INTEGER;
     head: INTEGER;
BEGIN
  sec :=ofs  MOD SECS;
  head:=ofs  DIV SECS;
  cyl :=head DIV HEADS;
  head:=head MOD HEADS;
  XMA :=int(bs(buf*4>>16)*{0..5});
  XMB :=int(bs(XMA)*{0..1})<<8;
  try :=TRIES;
  LOOP
    IF not_ready(50) THEN RETURN bs(err.time_out) END;
    out(RKBA,buf*4);
    out(RKWC,-(len*(SECSIZE DIV 2)));
    out(RKDC,cyl+100000b);
    out(RKDA,sec+int(head<<8));
    out(RKXMA,XMA);
    out(RKCS1,int(bs(op))+XMB);
    os.wait(end);
    IF {15}*inp(RKCS1)={} THEN RETURN {} END;
    IF try=0 THEN RETURN error() END;
    out(RKCS1,100000b);
    DEC(try);
    IF try MOD 4 = 0 THEN reset END
  END;
  RETURN bs(err.ok)
END dma;

PROCEDURE read(VAR r: req.REQUEST);
  VAR l: INTEGER;
    try: INTEGER;       qbus: BOOLEAN;
    adr: ADDRESS;       size: INTEGER;
    err: BITSET;       piece: INTEGER;
BEGIN
  qbus:=(r.buf>=q_base) & (r.buf+r.len*SECWORDS<q_base+q_size);
  IF qbus THEN piece:=64*KB DIV SECWORDS ELSE piece:=4*KB DIV SECWORDS END;
  WITH r DO
    adr:=buf; size:=len; err:={};
    WHILE size>0 DO
      IF size>piece THEN l:=piece ELSE l:=size END;
      IF qbus THEN
        err:=dma(READ,drn,ofs,adr,l)+err
      ELSE
        err:=dma(READ,drn,ofs,q_buf,l)+err;
        move(adr,q_buf,l*SECWORDS)
      END;
      INC(ofs,l);   DEC(size,l);   INC(adr,SECWORDS)
    END;
    res:=int(err)
  END
END read;

PROCEDURE write(VAR r: req.REQUEST);
  VAR l: INTEGER;         io: ADDRESS;
    try: INTEGER;       qbus: BOOLEAN;
    adr: ADDRESS;       size: INTEGER;
    err: BITSET;       piece: INTEGER;
BEGIN
  qbus:=(r.buf>=q_base) & (r.buf+r.len*SECWORDS<q_base+q_size);
  IF qbus THEN piece:=64*KB DIV SECWORDS ELSE piece:=4*KB DIV SECWORDS END;
  WITH r DO
    adr:=buf; size:=len; err:={};
    WHILE size>0 DO
      IF size>piece THEN l:=piece ELSE l:=size END;
      IF     qbus   THEN io:=adr  ELSE io:=q_buf; move(io,adr,l*SECWORDS) END;
      err:=dma(WRITE,drn,ofs,io,l)+err;
      INC(ofs,l);   DEC(size,l);   INC(adr,SECWORDS)
    END;
    res:=int(err)
  END
END write;

VAR PWOFF: BOOLEAN;

PROCEDURE park_drv;
  VAR r: req.REQUEST;
BEGIN
  r.buf:=ADDRESS(q_buf);  r.drn:=0;
  r.op :=req.READ;        r.pos:=0;
  r.res:=err.ok;          r.len:=1;

  r.ofs:=( 0           -- head
          +HEADS*1024  -- track
         ) *SECS
         +0;           -- sector

  read(r); PWOFF:=TRUE

END park_drv;

VAR  port: drv.port_ptr;

PROCEDURE fill_spec;
BEGIN
  port^.state:={}; -- drv.wr_pro};
  port^.dsize  :=HEADS*TRACKS*SECS*SECSIZE;
  port^.cyls   :=TRACKS;
  port^.heads  :=HEADS;
  port^.secs   :=SECS;
  port^.ssc    :=SSC;
  port^.secsize:=SECSIZE;
  port^.res_sec:=0;
END fill_spec;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  IF PWOFF & (r.op#req.POWER_OFF) THEN r.res:=err.ill_access; RETURN END;
  r.res:=err.ok;
  CASE r.op OF
    |req.NOP      :
    |req.READ     : read (r);
    |req.WRITE    : write(r);
    |req.MOUNT    : reset
    |req.UNMOUNT  : reset
    |req.POWER_OFF: park_drv
    |req.GET_SPEC : fill_spec
    |req.SET_SPEC : fill_spec; r.res:=err.inv_op; RETURN
--  |req.FORMAT   :
  ELSE r.res:=err.inv_op; RETURN
  END;
END doio;

PROCEDURE WaitIPT;

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
  setm(getm()-{0..1});
  setIPT;
  os.suspend(os.active(),-1);
  LOOP os.send(end); transfer(driver,ipted) END
END WaitIPT;

PROCEDURE define;
  VAR i: INTEGER;
  ports: ARRAY [0..0] OF drv.port_ptr;
BEGIN
  PWOFF:=FALSE;
  IF drv.prepare(TRUE,{0},ports)#err.ok THEN HALT(1) END;
  port:=ports[0]; ASSERT(port#NIL);
  port^.mode:=drv.disk;
  port^.doio:=doio;
  port^.name:='dk';
  port^.drn:=0;
   fill_spec;
(* --------- Hady. 27-Jul-90.
  port^.state:={}; -- drv.wr_pro};
  port^.dsize  :=HEADS*TRACKS*SECS*SECSIZE;
  port^.cyls   :=TRACKS;
  port^.heads  :=HEADS;
  port^.secs   :=SECS;
  port^.ssc    :=SSC;
  port^.secsize:=SECSIZE;
  port^.res_sec:=0;
*)
  IF drv.define(ports)#err.ok THEN HALT(1) END;
  out(RKCS2,RESET);
END define;

BEGIN
  ASSERT(TRIES>0);
  define;
  WaitIPT;
END DK6dq615.

(********************** К О М А Н Д Ы **********************************

        Bit
       4321     0(Go)  Octal    Command
       0000      1      01      SELECT DRIVE
       0001      1      03      PACK ACKNOWLEDGE (No  operation)
       0010      1      05      DRIVE CLEAR (Reset drive FAULT)
       0011      1      07      UNLOAD (No operation)
       0100      1      11      START SPINDLE(No operation)
       0101      1      13      RECALIBRATE (Restore DRIVE and reset FAULT)
       0110      1      15      OFFSET
       0111      1      17      SEEK (No operation)
       1000      1      21      READ DATA
       1001      1      23      WRITE DATA
       1010      1      25      READ HEADERS (1 TRACK of HEADERS)
       1011      1      27      WRITE HEADERS (FORMAT TRACK)
       1100      1      31      WRITE CHECK

************************************************************************)
