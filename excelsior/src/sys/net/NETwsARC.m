MODULE NETwsARC; (*$N+ Igo  02-Aug-91. (c) KRONOS *)
                 (*    Igo  10-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  drv: netdrv;
IMPORT  cod: defCodes;
IMPORT  req: defRequest;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  env: tskEnv;
IMPORT  err: defErrors;

--IMPORT  spo: SPOws;
--IMPORT  tty: Terminal;

TYPE ADDRESS  = SYSTEM.ADDRESS;
     WORD     = SYSTEM.WORD;
     int      = INTEGER;
     bs       = BITSET;

     PROTOCOL = POINTER TO protocol;
     protocol = RECORD
                  alloc  : drv.allocproc;
                  input  : drv.inputproc;
                  move   : drv.moveproc;
                  next   : drv.nextproc;
                  ready  : drv.readyproc;
                  channel: WORD
                END;

    BUFNODE  = POINTER TO bufnode;
    bufnode  = RECORD
                 node: drv.NODE;
                 stat: INTEGER;
                 last: BOOLEAN;
                 res : INTEGER;
                 bno : INTEGER;
                 next: BUFNODE
               END;

CONST ibufsno  = 2;
      obufsno  = 2;

      MAGIC    = 0435241h;

      SHORT    = 253;
      LONG     = 508;

      otimeout = 20 ; (* it depends of number of active stations *)

      TRYES    = 64;

      _err     = {0};
      _stop    = {1};
      _clear   = {2};

VAR
  CSR      : POINTER TO BITSET;
  CNTR     : POINTER TO INTEGER;
  RAM      : ADDRESS;
  IMASK    : BITSET;

  driver   : ADDRESS;
  ipted    : ADDRESS;

  im       : nos.process;
  id       : os .process;
  idsg     : os .signal_rec;

  alloc    : drv.BUFFER;
  input    : drv.BUFFER;

  ibeg,iend: INTEGER;

  otime    : INTEGER;

  om       : nos.process;
  otran    : BUFNODE;
  omove    : BUFNODE;

  odbeg    : BUFNODE;
  odend    : BUFNODE;

  oCMD     : BITSET;

  oERROR   : INTEGER;
  oSTOP    : drv.NODE;
  oRES     : INTEGER;
  oPROT    : INTEGER;

  oque     : drv.NODE;

  remlock  : os.signal_rec;

  obufr    : ARRAY [0..obufsno-1] OF bufnode;
  obufd    : ARRAY [0..obufsno  ] OF bufnode;

  prots    : ARRAY [0..255] OF PROTOCOL;


CONST
  TRAP = 13h;

  (* masks *)

  TxIE  = {0};
  RxIE  = {7};
  RECIE = {2};

  (* control *)
  DS = 1;    -- DISABLE TRANSSMITTER;
  DR = 2;    -- DISABLE RECEIVER;

  ER = 4;
  ET = 3;


  DC254 = 05h; -- DEFINE CONFIGURATION;
  DC510 = 0Dh;

  CF = 1Eh;   -- CLEAR FLAGS;
  CP = 0Eh;   -- CLEAR POR;
  CR = 16h;   -- CLEAR RECONF;

  (* status  *)

    RI    = {7};
    ETS1  = {6};
    ETS2  = {5};
    POR   = {4};
    TEST  = {3};
    RECON = {2};
    TMA   = {1};
    TA    = {0};

PROCEDURE di(): BITSET;    CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET);   CODE cod.setm END ei;
PROCEDURE self(): ADDRESS; CODE cod.activ END self;
PROCEDURE transfer(VAR f,t: ADDRESS); CODE cod.tra END transfer;

PROCEDURE reset;
BEGIN
  REPEAT CSR^:={}; CNTR^:=CF UNTIL RECON*CSR^={};
  REPEAT CSR^:={}; CNTR^:=CF UNTIL POR  *CSR^={};
  CNTR^:=DC510;
  CSR ^:={};
  CNTR^:=ER;
  CNTR^:=DS;
  IMASK:=RxIE;
  CSR^ :=IMASK;
  IF INTEGER(RAM^)#0 THEN END
END reset;

PROCEDURE interrupt;
  VAR csr: BITSET;
BEGIN
  LOOP
    csr:=CSR^;
    IF    (IMASK*RxIE#{})&(RI*csr#{}) THEN (* recieve ready *)
      CSR^:=IMASK-RxIE;
      iend:=(iend+1) MOD ibufsno;
      IF iend#ibeg THEN  CNTR^:=ER+iend*8
      ELSE               IMASK:=IMASK-RxIE
      END;
      CSR^:=IMASK;
      nos.switch(im,ipted)
    ELSIF (IMASK*TxIE#{})&(TA*csr#{}) THEN (* transmit ready *)
      CSR^:=IMASK-TxIE;
      IF otran^.node#NIL THEN
        IF TMA*csr={}  THEN
          IMASK:=IMASK-TxIE;
          oCMD :=oCMD+_err;
          IF otime>0 THEN oERROR:=err.write_fail ELSE oERROR:=err.time_out END;
          otime:=-1
        ELSIF otran^.last THEN
          odend^.node:=otran^.node;
          odend      :=odend^.next
        END
      END;
      IF oCMD*_err={} THEN
        LOOP
          otran:=otran^.next;
          IF otran=omove     THEN
            IMASK:=IMASK-TxIE;      otime:=-1;       EXIT
          END;
          IF otran^.node#NIL THEN
            CNTR^:=ET+otran^.bno*8; otime:=otimeout; EXIT
          END
        END
      END;
      CSR^:=IMASK;
      nos.switch(om,ipted)
    ELSIF POR*csr#{} THEN
      CNTR^:=CP;
      transfer(driver,ipted)
    ELSIF RECON*csr#{} THEN
      CNTR^:=CR;
      transfer(driver,ipted)
    ELSE
      CSR^:=IMASK;
      transfer(driver,ipted)
    END
  END
END interrupt;

PROCEDURE error;

  PROCEDURE untiebad(VAR bad: drv.NODE; prot,dest: INTEGER);
    VAR n,n0: drv.NODE;
        end : BOOLEAN;
  BEGIN
    IF oque=NIL THEN RETURN END;
    n:=oque;
    REPEAT
      n0 :=n^.f;
      end:=n0=oque;
      IF (n^.prot=n^.prot)&(n^.dest=dest) THEN
        nos.untien(oque,n);
        nos.tie   (bad ,n)
      END;
      n:=n0
    UNTIL end
  END untiebad;

  PROCEDURE tiebad(bad: drv.NODE);
    VAR n: drv.NODE;
  BEGIN
    IF bad =NIL THEN            RETURN END;
    IF oque=NIL THEN oque:=bad; RETURN END;
    n      :=oque^.b;
    oque^.b:=bad^.b; oque^.b^.f:=oque;
    bad ^.b:=n;      bad ^.b^.f:=bad
  END tiebad;

  PROCEDURE restartdriver;
    VAR m: BITSET;
  BEGIN
    ASSERT(otran^.node=NIL);
    m:=di();
      oCMD  :=oCMD-_err;
      IMASK:=IMASK+TxIE;
      CNTR^:=DS;
      CSR^ :=IMASK;
      otime:=-1;
    ei(m)
  END restartdriver;

  VAR n   : BUFNODE;
      p   : drv.NODE;
      bad : drv.NODE;
      prot: INTEGER;
      dest: INTEGER;
      pr  : PROTOCOL;
BEGIN
  ASSERT(otran^.node#NIL);
  IF oERROR#err.time_out THEN DEC(otran^.node^.try) END;
  IF    ( oERROR=err.time_out)
     OR ((oERROR=err.write_fail)&(otran^.node^.try>=0)) THEN
    prot:=otran^.node^.prot;
    dest:=otran^.node^.dest;
    bad :=NIL;
    n   :=otran;
    REPEAT
      IF (n^.node#NIL)&(n^.node^.prot=prot)&(n^.node^.dest=dest) THEN
        p:=n^.node;
        IF p=oque THEN nos.untie(oque) END;
        nos.tie(bad,p);
        p^.stat:=n^.stat;
        REPEAT
          n^.node:=NIL; n:=n^.next
        UNTIL (n=omove)OR(n^.node#p)
      ELSE
        n:=n^.next
      END
    UNTIL n=omove;
    untiebad(bad,prot,dest);
    tiebad  (bad)
  ELSE
    p:=otran^.node;
    n:=otran;
    IF p=oque THEN nos.untie(oque) END;
    REPEAT
      n^.node:=NIL; n:=n^.next
    UNTIL (n=omove)OR(n^.node#p);
    nos.deltout(p^.time);
    pr:=prots[p^.prot];
    pr^.ready(pr^.channel,p,oERROR)
  END;
  restartdriver
END error;

PROCEDURE clearprot(m: BITSET);

  PROCEDURE done(n: drv.NODE);
  BEGIN
    n^.magic:=-1;
    nos.deltout(n^.time);
    IF prots[n^.prot]#NIL THEN
      prots[n^.prot]^.ready(prots[n^.prot]^.channel,n,err.time_out)
    END
  END done;

  VAR b   : BUFNODE;
      n,n0: drv.NODE;
      end : BOOLEAN;
BEGIN
  b :=otran;
  n0:=NIL;
  IF otran#omove THEN
    REPEAT
      IF (b^.node#NIL)&(b^.node^.prot=oPROT) THEN
        n      :=b^.node;
        b^.node:=NIL;
        IF b^.last THEN nos.tie(n0,n) END
      END;
      b:=b^.next
    UNTIL b=omove
  END;
  ei(m);
  WHILE n0#NIL DO
    n:=n0;
    nos.untie(n0);
    done(n)
  END;
  IF oque#NIL THEN
    n:=oque;
    REPEAT
      n0 :=n^.f;
      end:=oque=n0;
      IF n^.prot=oPROT THEN
        nos.untien(oque,n);
        done(n)
      END;
      n:=n0
    UNTIL end
  END;
  m:=di();
    oCMD:=oCMD-_clear;
  ei(m)
END clearprot;

PROCEDURE clearnode(m: BITSET);
  VAR f: BOOLEAN;
      n: BUFNODE;
      r: INTEGER;
      o: drv.NODE;
BEGIN
  f:=FALSE;

  IF IMASK*TxIE#{} THEN
    IF oSTOP=otran^.node THEN CNTR^:=DS END;
    n:=otran;
    REPEAT
      IF n^.node=oSTOP THEN
        IF n^.last THEN f:=TRUE END;
        n^.node:=NIL
      END;
      n:=n^.next
    UNTIL n=omove
  END;

  IF NOT f THEN nos.untien(oque,oSTOP) END;
  nos.deltout(oSTOP^.time);
  r:=oRES;
  o:=oSTOP;
  oCMD:=oCMD-_stop;
  ei(m);
  IF prots[o^.prot]=NIL THEN RETURN END;
  prots[o^.prot]^.ready(prots[o^.prot]^.channel,o,r)
END clearnode;

PROCEDURE moveto;
  VAR m   : BITSET;
      ram : ADDRESS;
      l,l0: INTEGER;
      dest: INTEGER;
      p   : PROTOCOL;
      pb  : POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  m:=di();
  LOOP
    IF (oCMD={})&(oque#NIL)&((omove#otran) OR (IMASK*TxIE={})) THEN
      ei(m);
      p:=prots[oque^.prot];
      l:=LONG-1;
      omove^.stat:=oque^.stat;
      p^.next(p^.channel,oque,omove^.last,l);
      ram:=RAM+omove^.bno*128; pb:=ram;
      IF l<=(SHORT-1) THEN
        ram^:=bs((bs(oque^.dest)*{0..7})<<8)+bs((bs((255-l))*{0..7})<<16);
        pb^[255-l]:=CHAR(oque^.prot);
        p^.move(p^.channel,ram,256-l)
      ELSE
        IF l<256 THEN l0:=256 ELSE l0:=l END;
        ram^:=bs((bs(oque^.dest)*{0..7})<<8)+bs((bs((511-l0))*{0..7})<<24);
        pb^[511-l0]:=CHAR(oque^.prot);
        p^.move(p^.channel,ram,512-l0)
      END;
      m:=di();
      IF (IMASK*TxIE={}) & (oCMD*_err={}) THEN
        CNTR^:=ET + otran^.bno*8;
        otime:=otimeout;
        IMASK:=IMASK+TxIE;
        CSR^ :=IMASK
      END;
      omove^.node:=oque;
      IF omove^.last THEN nos.untie(oque) END;
      omove:=omove^.next
    ELSIF odend#odbeg THEN
      ei(m);
        IF odbeg^.node#NIL THEN
          odbeg^.node^.magic:=-1;
          nos.deltout(odbeg^.node^.time);
          p:=prots[odbeg^.node^.prot];
          p^.ready(p^.channel,odbeg^.node,err.ok)
        END;
        odbeg:=odbeg^.next;
      m:=di()
    ELSIF oCMD#{} THEN
      IF oCMD*_err#{} THEN
        ei(m);
          error;
      ELSIF oCMD*_stop#{} THEN
        clearnode(m)
      ELSIF oCMD*_clear#{} THEN
        clearprot(m)
      END;
      m:=di()
    ELSE
      nos.wait
    END
  END
END moveto;

PROCEDURE startproc;
BEGIN
  nos.switch(im,self())
END startproc;

PROCEDURE movefrom;
  VAR m: BITSET;
      n: drv.BUFFER;
    ram: SYSTEM.ADDRESS;
    l,b: INTEGER;
      p: PROTOCOL;
   prot: INTEGER;
     pb: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  LOOP
    m:=di();
      WHILE ((ibeg=iend)&(IMASK*RxIE#{})) DO nos.wait END;
    ei(m);
    ram:=RAM + ibeg*128; pb:=ram;
    m  :=bs(ram^);
    l  :=int((m>>16)*{0..7});
    IF l=0 THEN l:=int((m>>24)*{0..7}); b:=512
    ELSE                                b:=256
    END;
    prot:=INTEGER(pb^[l]); INC(l);
    p:=prots[prot];
    n:=NIL;
    IF p#NIL THEN p^.alloc(p^.channel,n,startproc,ram,l,b-l) END;
    m:=di();
      IF IMASK*RxIE={} THEN
        IMASK:=IMASK+RxIE;
        CNTR^:=ER+iend*8;
        CSR^ :=IMASK
      END;
      ibeg:=(ibeg+1) MOD ibufsno;
      IF n#NIL THEN
        n^.pro:=prot;
        n^.f  :=NIL;
        IF alloc=NIL THEN
          alloc   :=n;
          alloc^.b:=n;
          IF idsg.queue#os.null THEN os.send(idsg) END
        ELSE
          alloc^.b^.f:=n;
          alloc^.b   :=n
        END
      END;
    ei(m)
  END
END movefrom;

PROCEDURE idone;
  VAR m: BITSET;
      n: drv.BUFFER;
      p: PROTOCOL;
BEGIN
  LOOP
    m:=di();
      WHILE alloc=NIL DO os.wait(idsg) END;
      n:=alloc;
      alloc:=alloc^.f;
      IF alloc#NIL THEN alloc^.b:=n^.b END;
    ei(m);
    p:=prots[n^.pro];
    IF p=NIL THEN RETURN END;
    p^.input(p^.channel,n)
  END
END idone;

PROCEDURE otout;
BEGIN
  DEC(otime);
  IF otime=0 THEN CNTR^:=DS END
END otout;

PROCEDURE stopnode(n: drv.NODE; res: INTEGER);
  VAR m: BITSET;
      f: BOOLEAN;
BEGIN
  ASSERT(n#NIL);
  m:=di();
    ASSERT(oCMD*_stop={});
    IF n^.magic#MAGIC THEN ei(m); RETURN END;
    n^.magic:=-1;
    oSTOP:=n;
    oRES :=res;
    oCMD:=oCMD+_stop;
    nos.switch(om,self());
  ei(m)
END stopnode;

PROCEDURE otimeproc(t: nos.TIME);
  VAR n: drv.NODE;
BEGIN
  n:=drv.NODE(t^.obj);
  stopnode(n,err.time_out)
END otimeproc;

PROCEDURE install(VAR r: drv.request);
  VAR p: PROTOCOL;
BEGIN
  IF prots[r.prot]#NIL THEN r.res:=err.duplicate; RETURN END;
  nos.allocate(p,SIZE(p^));
  IF p=NIL THEN r.res:=err.no_memory; RETURN END;
  p^.alloc  :=r.alloc;
  p^.input  :=r.input;
  p^.next   :=r.next;
  p^.move   :=r.move;
  p^.ready  :=r.ready;
  p^.channel:=r.channel;
  prots[r.prot]:=p
END install;

PROCEDURE clearprotocol(prot: INTEGER);
  VAR m: BITSET;
BEGIN
  m:=di();
    oCMD:=oCMD+_clear;
    oPROT:=prot;
    nos.call(om);
    prots[prot]:=NIL;
  ei(m);
END clearprotocol;

PROCEDURE doio(VAR r: drv.request): INTEGER;
  VAR m: BITSET;
      p: PROTOCOL;
      n: drv.NODE;
      a: ADDRESS;
BEGIN
  IF (r.prot>255) OR (r.prot<0) THEN r.res:=err.bad_parm; RETURN r.res END;
  r.res :=err.ok;
  IF r.op=drv._transmit THEN
    m:=di();

--spo.print('doio transmit n=%h r=%h tout=%d d=%h\n',r.node,SYSTEM.ADR(r),r.node^.tout,r.node^.dest);

      a:=oque;
      n:=r.node;
      p:=prots[r.prot];
      IF p=NIL THEN ei(m); r.res:=err.no_entry; RETURN r.res END;
      n^.magic:=MAGIC;
      nos.tie(oque,n);
      n^.try      :=TRYES;
      n^.time.done:=otimeproc;
      n^.time.obj :=n;
      nos.settout(n^.time,n^.tout);
      IF (a=NIL)&((omove#otran) OR (IMASK*TxIE={})) THEN nos.call(om) END;
    ei(m);
    RETURN err.ok
  END;

  IF os.wait_del(-1,remlock)#0 THEN r.res:=err.ipted_op; RETURN r.res END;

  CASE r.op OF
  |drv._install: install(r)
  ELSE
    r.res:=err.inv_op
  END;

  os.send(remlock);

  RETURN r.res

END doio;

VAR save: ARRAY [0..  1] OF WORD;
    wsp0: ARRAY [0..511] OF WORD;

PROCEDURE finishprots;
  VAR i: INTEGER;
      p: PROTOCOL;
BEGIN
  os.wait(remlock);
  FOR i:=0 TO HIGH(prots) DO
    p:=prots[i];
    IF p#NIL THEN
      clearprotocol(i);
      p^.ready(p^.channel,NIL,drv.dead);
      nos.deallocate(p,SIZE(p^))
    END
  END;
  os.send(remlock)
END finishprots;

VAR refdrv: drv.REFDOIO;

PROCEDURE stop;
  VAR m   : BITSET;
      trap: ADDRESS;
      his : ARRAY [0..255] OF CHAR;
      i   : INTEGER;
BEGIN


  (*
  os.extract(his,id^.pp ); tty.print('idone\n%s\n',his);
  os.extract(his,im.self); tty.print('imove\n%s\n',his);
  os.extract(his,om.self); tty.print('omove\n%s\n',his);
  os.extract(his,driver ); tty.print('int  \n%s\n',his);
  *)

  os.remove_action(otout);
  os.stop(id,TRUE);

  IF refdrv#NIL THEN
    IF drv.removedriver(refdrv)#err.ok THEN END;
    finishprots;
  END;

  trap:=TRAP*2;
  m:=di();
    CSR^:={};  trap^:=save[0];  CSR^:={};  trap:=trap+1;
    CSR^:={};  trap^:=save[1];  CSR^:={};
    CNTR^:=CF; CNTR^:=DS; CNTR^:=DR;
    IMASK:={};
  ei(m);

--;tty.print('end of driver stop\n');

END stop;

PROCEDURE initbufs;
  VAR i: INTEGER;
BEGIN
  oCMD  :={};
  otime :=-1;

  FOR i:=0 TO HIGH(prots) DO prots[i]:=NIL END;

  FOR i:=0 TO HIGH(obufr)-1 DO
    obufr[i].next:=SYSTEM.ADR(obufr[i+1]);
    obufr[i].bno :=i+ibufsno
  END;
  obufr[HIGH(obufr)].next:=SYSTEM.ADR(obufr[0]);
  obufr[HIGH(obufr)].bno :=HIGH(obufr)+ibufsno;
  otran:=SYSTEM.ADR(obufr[0]);
  omove:=otran;

  FOR i:=0 TO HIGH(obufd)-1 DO
    obufd[i].next:=SYSTEM.ADR(obufd[i+1])
  END;
  obufd[HIGH(obufd)].next:=SYSTEM.ADR(obufd[0]);
  odend:=SYSTEM.ADR(obufd[0]);
  odbeg:=odend;

  oque  :=NIL;
  alloc :=NIL;    ibeg  :=0;
  input :=NIL;    iend  :=0;
END initbufs;

PROCEDURE saveVEC;
  VAR m: BITSET;
      a: ADDRESS;
BEGIN
  m:=di();
    a:=TRAP*2; save[0]:=a^;
    a:=a+1;    save[1]:=a^;
  ei(m)
END saveVEC;

PROCEDURE init;
  VAR m: BITSET; a: ADDRESS; i: INTEGER;
BEGIN

  initbufs;

  RAM  :=ADDRESS(134000h);
  CSR  :=ADDRESS(1800B8h);
  CNTR :=ADDRESS(1900B8h);

  saveVEC;

  IF os.final(os.self(),stop)#err.ok THEN HALT(1) END;
  IF os.insert_action(otout) #err.ok THEN HALT(1) END;

    IF NOT nos.new(im,movefrom,128,31) THEN HALT(1) END;
    IF NOT nos.new(om,moveto  ,128,30) THEN HALT(1) END;

  os.ini_signal(idsg   ,{}       ,0);
  os.ini_signal(remlock,os.break ,1);

  IF os.make_process(id,idone,512)#err.ok THEN HALT(1) END;

  a:=TRAP;
  m:=di();
    os.new_process(interrupt,SYSTEM.ADR(wsp0),SIZE(wsp0),driver);
    a:=a*2; a^:=driver;
    a:=a+1; a^:=SYSTEM.ADR(ipted);
  ei(m);
  os.start(id);

  i:=drv.definedriver("arc0",refdrv,doio);
  IF i#err.ok THEN HALT(i) END;

  env.become_ipr;
  reset;
  os.suspend(os.active(),-1)

END init;

BEGIN
  refdrv:=NIL;
  init
END NETwsARC.
