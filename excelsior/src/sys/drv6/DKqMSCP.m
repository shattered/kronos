MODULE DKqMSCP;

IMPORT  sys: SYSTEM;
IMPORT  low: lowLevel;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  cod: defCodes;
IMPORT  os : osKernel;
IMPORT  fs : osFiles;
IMPORT  env: tskEnv;

CONST
  IP  = 172150b DIV 2; -- initialization and polling register
  SA  = IP+1;          -- status and address register
  VEC = 154b DIV 4;    -- interrupt number

TYPE
  ADDRESS = sys.ADDRESS;

  ring_item=POINTER TO
  RECORD
    sz: INTEGER;
    CASE : INTEGER OF
    |0: b: ARRAY [0..59] OF CHAR;
    |1: w: ARRAY [0..14] OF INTEGER;
    END;
  END;

VAR
  ready      : os.signal_rec;
  ipted      : os.PROCESS;
  driver     : os.PROCESS;

  intr_id    : ADDRESS; -- pointer to interrupt word
  m_ring     : ADDRESS; -- pointer to messages ring
  c_ring     : ADDRESS; -- pointer to commands ring
  m_buf      : ring_item;
  c_buf      : ring_item;
  ident      : INTEGER;
  activ      : BOOLEAN;
  inited     : BOOLEAN;
  port_type  : INTEGER;
  tries      : INTEGER;
  qbuf       : INTEGER;
  sec_no     : ARRAY [0..1] OF INTEGER;
  PWOFF      : BOOLEAN;

PROCEDURE move(to,from: ADDRESS; size: INTEGER); CODE cod.move END move;
PROCEDURE transfer(VAR from,to: os.PROCESS); CODE cod.tra END transfer;

PROCEDURE getm(): BITSET;  CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;

PROCEDURE inp(reg: INTEGER): sys.WORD;          CODE cod.inp END inp;
PROCEDURE out(reg: INTEGER; data: sys.WORD);    CODE cod.out END out;

PROCEDURE msg(VAL s: ARRAY OF CHAR);
BEGIN
  env.put_str(env.info,s,TRUE);
END msg;

PROCEDURE sa?(n: INTEGER);
  -- detect a fatal error during operation
BEGIN
  IF n=0 THEN RETURN END;
  CASE n MOD 100h OF
    | 0b : msg('No information in message packet.');
    | 1b : msg('Parity or timeout error'
               ' when read data from a message packet.');
    | 2b : msg('Parity or timeout error'
               ' when write data to a message packet.');
    | 4b : msg('Self-test indicated a controller RAM error.');
    | 5b : msg('Self-test indicated a controller firmware checksum error.');
    | 6b : msg('Parity or timeout error'
               '  when read envelope address from a command ring.');
    | 7b : msg('Parity or timeout error'
               '  when write envelope address from a command ring.');
    |11b : msg('Host did not communicate with controller'
               '  within the time frame established while'
               '  bringing the controller online.');
    |12b : msg('Operating system sent more commands'
               '  than the controller can accept.');
    |13b : msg('Controller unable to perform DMA transfer operation.');
    |14b : msg('Self-test indicated controller fatal error.');
    |16b : msg('The MSCP connection identifier is invalid.');
    |23b : msg('An error during initialization sequence.');
    |111b: msg('Autoboot timeout.');
    |121b: msg('F.R.D load to memory failed.');
  ELSE
    msg('Unknown fatal error.');
  END;
  activ:=FALSE;
END sa?;

PROCEDURE do;
  VAR i: INTEGER;
BEGIN
  IF NOT activ THEN RETURN END;
  m_ring^:=0;
  INC(ident);
  c_buf^.w[0]:=ident;
  c_ring^:=BITSET(INTEGER(sys.ADR(c_buf^.b))*4)*{0..21}+{31};
  i:=inp(IP);
END do;

PROCEDURE wait;
  VAR i,t: INTEGER;
BEGIN
  t:=8;
  REPEAT
    m_buf^.sz:=HIGH(m_buf^.b)+1;
    low.zero(m_buf^.w);
    m_ring^:=BITSET(INTEGER(sys.ADR(m_buf^.b))*4)*{0..21}+{30,31};
    i:=os.wait_del(2000,ready);
    IF i#err.ok THEN activ:=FALSE END;
    i:=inp(SA);
    IF i#0 THEN sa?(i) END;
    IF NOT activ THEN RETURN END;
    DEC(t);
  UNTIL (t<=0) OR (INTEGER(m_ring^)>=0);
  IF m_ring<0 THEN
    msg('No response from controller.'); activ:=FALSE;
  ELSIF m_buf^.w[0]#ident THEN
    msg('Operation aborted: strange respose.'); activ:=FALSE;
  END;
END wait;

PROCEDURE size(u: INTEGER; VAR sz: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  low.zero(c_buf^.w);
  c_buf^.sz  :=BYTES(c_buf^.b);
  c_buf^.b[4]:=CHAR(u);
  c_buf^.b[8]:=CHAR(9); -- online
  do;
  REPEAT
    wait; IF NOT activ THEN RETURN err.hw_fail END;
  UNTIL m_buf^.b[8]=CHAR(89h);
  IF  m_buf^.b[10]=0c THEN sz:=m_buf^.w[9]; RETURN err.ok
  ELSE RETURN err.io_error
  END;
END size;

PROCEDURE init_cntrl;
  VAR i,j: INTEGER; a: ADDRESS; s: BITSET;
  CONST go=1;
BEGIN
  inited:=TRUE; ident:=334;

  intr_id:=ADDRESS(401D00h);
  m_ring :=intr_id+1;
  c_ring :=m_ring+1;
  m_buf  :=c_ring+1;
  c_buf  :=ADDRESS(m_buf)+SIZE(m_buf^);

--контроллер сам обнуляет эти адреса при инициализации
  intr_id^:=0;
  a:=m_ring; a^:=0;
  a:=c_ring; a^:=0;

  out(IP,4000b); -- initialize the QD01/D
  j:=10000;
  REPEAT s:=inp(SA); DEC(j) UNTIL (s#{}) OR (j=0);
  IF NOT (11 IN s) THEN msg('Port not found'); RETURN END;
  IF 15 IN s THEN msg('Self-test error (initialization)'); RETURN END;

  out(SA,8080h+VEC);
  j:=os.wait_del(1000,ready);
  IF j#err.ok THEN msg('Port interrup lost'); RETURN END;
  s:=inp(SA);
  IF 15 IN s THEN msg('Self-test error (initialization)'); RETURN END;
  IF NOT (12 IN s) THEN msg('Port error'); RETURN END;
  port_type:=INTEGER((s*{8..10})>>8);

  out(SA,INTEGER(m_ring)*4 MOD 10000h);
  j:=os.wait_del(1000,ready);
  IF j#err.ok THEN msg('Time out (initialization 2)'); RETURN END;
  s:=inp(SA);
  IF 15 IN s THEN msg('Self-test error (initialization 2)'); RETURN END;
  IF NOT (13 IN s) THEN msg('Port error 2'); RETURN END;
  IF INTEGER(s*{0..7})#VEC+80h THEN
    msg('Port error (vector)'); RETURN
  END;

  out(SA,INTEGER((INTEGER(m_ring)*4)>>16) MOD 40h);
  j:=os.wait_del(1000,ready);
  IF j#err.ok THEN msg('Time out (initialization 3)'); RETURN END;
  s:=inp(SA);
  IF 15 IN s THEN msg('Self-test error (initialization 3)'); RETURN END;
  IF NOT (14 IN s) THEN msg('Port error'); RETURN END;

  out(SA,1);
  j:=os.wait_del(1000,ready);
  i:=inp(SA);
  activ:=TRUE;
  IF i#0 THEN sa?(i) END;
  IF NOT activ THEN RETURN END;

  low.zero(c_buf^.w);
  c_buf^.sz  :=BYTES(c_buf^.b);
  c_buf^.b[8]:=CHAR(04h); -- set controller char.
  do;
  REPEAT
    wait; IF NOT activ THEN RETURN END;
  UNTIL m_buf^.b[8]=CHAR(084h);

  j:=size(0,sec_no[0]);
  j:=size(1,sec_no[1]);
END init_cntrl;

-----------------------  D R I V E R  --------------------------
                       ---------------

PROCEDURE read_write(VAR r: req.REQUEST);
  VAR t,adr,len,l,cmd: INTEGER; mv: BOOLEAN;
BEGIN
  IF NOT activ THEN r.res:=err.hw_fail; RETURN END;
  len:=r.len; adr:=r.buf; r.res:=err.ok;
  mv:=(adr<400000h) OR (adr+len*128>500000h);
  REPEAT
    t:=tries; l:=len;
    IF l>8 THEN l:=8 END;
    REPEAT
      IF r.op=req.WRITE THEN
        IF mv THEN move(qbuf,adr,l*128) END;
        cmd:=22h;
      ELSE
        cmd:=21h;
      END;
      low.zero(c_buf^.w);
      c_buf^.sz  :=BYTES(c_buf^.b);
      c_buf^.b[4]:=CHAR(r.drn);
      c_buf^.w[3]:=l*512;
      IF mv THEN
        c_buf^.w[4]:=INTEGER(qbuf)*4 MOD 400000h;
      ELSE
        c_buf^.w[4]:=INTEGER(adr)*4 MOD 400000h;
      END;
      c_buf^.w[7]:=r.ofs;
      c_buf^.b[8]:=CHAR(cmd);
      do; cmd:=cmd+80h;
      REPEAT wait UNTIL NOT activ OR (ORD(m_buf^.b[8])=cmd);
      IF (r.op=req.READ) & mv THEN move(adr,qbuf,l*128) END;
      IF NOT activ THEN r.res:=err.hw_fail; RETURN END;
      IF m_buf^.b[10]#0c THEN r.res:=err.io_error END;
      DEC(t);
    UNTIL (r.res=err.ok) OR (t<=0);
    DEC(len,l); INC(adr,l*128);
  UNTIL len=0;
END read_write;

PROCEDURE park(r: req.REQUEST);
BEGIN
  PWOFF:=TRUE;
  r.op :=req.READ;
  r.ofs:=sec_no[r.drn]-1;
  r.len:=1;
  r.buf:=qbuf;
  read_write(r);
END park;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    dsecs  :=sec_no[drn];
    minsec := -1;
    minsec := -1;
    cyls   := -1;
    ssc    :=  9;
    secsize:=512;
    ressec :=  0;
  END
END get_spec;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  IF NOT inited THEN init_cntrl END;
  IF PWOFF & (r.op#req.POWER_OFF) THEN r.res:=err.undef; RETURN END;
  r.res:=err.ok;
  CASE r.op OF
    |req.NOP      :
    |req.READ     : read_write(r)
    |req.WRITE    : read_write(r)
    |req.MOUNT    : r.res:=size(r.drn,sec_no[r.drn]);
    |req.UNMOUNT  :
    |req.POWER_OFF: park(r)
    |req.GET_SPEC : get_spec(r)
  ELSE
    r.res:=err.inv_op
  END
END doio;

PROCEDURE install;
  VAR i,r: INTEGER;
BEGIN
  r:=fs.define_driver("du0",""   ,0,fs.disk,doio);
  IF r#err.ok THEN HALT(r) END;
  r:=fs.define_driver("du1","du0",1,fs.disk,doio);
  IF r#err.ok THEN HALT(r) END;
  env.put_str(env.info,"du0 du1",TRUE)
END install;

PROCEDURE self(): ADDRESS; CODE cod.activ END self;

PROCEDURE setIPT;
  VAR adr: ADDRESS;
BEGIN
  driver:=self();
  adr:=VEC*2; adr^:=driver;
  adr:=adr+1; adr^:=sys.ADR(ipted)
END setIPT;

BEGIN
  qbuf:=ADDRESS(401800h);
  sec_no[0]:=0; sec_no[1]:=0;
  tries:=2; PWOFF:=FALSE;
  activ:=FALSE; inited:=FALSE;
  install;
  env.become_ipr;
  os.ini_signal(ready,{},0);
  setm(getm()-{0..1});
  setIPT;
  os.suspend(os.active(),-1);
  LOOP os.send(ready); transfer(driver,ipted) END
END DKqMSCP.
