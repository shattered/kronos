IMPLEMENTATION MODULE SIOws; (*$U+$T- Leo 11-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT   os: osKernel;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT  req: defRequest;


TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

CONST
  XOFF = BITSET(23c);
  XON  = BITSET(21c);

VAR   NL: BITSET;
    NULL: BITSET;
     Xon: BITSET;
    Xoff: BITSET;
  on_off: BITSET;

VAR
  CSR      : POINTER TO BITSET;
  DTR      : POINTER TO BITSET;

  drv,ipt  : os.PROCESS;

  idle     : BOOLEAN;
  mdrv,mipt: os.PROCESS;

  kbuf     : ARRAY [0..31] OF BITSET;
  kbeg,kend: INTEGER;

  ibuf     : ARRAY [0..255] OF BITSET;
  ibeg,iend: INTEGER;
  irdy     : os.signal_rec;

  CMD      : BITSET;
  obuf     : ARRAY [0..1023] OF CHAR;
  obeg,oend: INTEGER;
  ordy     : os.signal_rec;

  ocnt     : INTEGER;

PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE transfer(VAR from,to: os.PROCESS); CODE cod.tra END transfer;

CONST
  (* control *)
  TxEN=0;  RxIE=1; RxEN=2; SBRK=3; ER=4; TxIE=5; RESET=6; EH=7;

  (* status  *)
  TxRDY=0; RxRDY=1; TxE=2; PE=3; OE=4; FE=5; SINDET=6; DSR=7;

PROCEDURE reset(mode: BITSET);
  PROCEDURE d;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO 99 DO END
  END d;

BEGIN
  CSR^:={}; d;    CSR^:={}; d;
  CSR^:={}; d;    CSR^:={}; d;
  CSR^:={RESET};  d; d;
  CSR^:= mode;    d;
  CMD :={RxEN,RxIE,TxEN,TxIE}; d;
  CSR^:=CMD; d
END reset;

PROCEDURE driver;
  VAR i: INTEGER;
   byte: BITSET;
   errc: BITSET;
BEGIN
  LOOP
    IF {RxRDY}*CSR^#{} THEN (* recieve byte *)
      byte:=DTR^*{0..7};
--      errc:=CSR^*{0..7};
--      IF errc*{OE,PE,FE}#{} THEN byte:=byte+errc<<8 END;
      IF byte=on_off THEN
        IF byte=Xon THEN
          IF obeg#oend THEN CMD:=CMD+{TxIE}; CSR^:=CMD
          ELSE
          END;
          on_off:=Xoff;
        ELSE
          CMD:=CMD-{TxIE}; CSR^:=CMD;
          on_off:=Xon;
        END;
      ELSE
        i:=(kend+1) MOD (HIGH(kbuf)+1);
        IF i#kbeg THEN
          kbuf[kend]:=byte; kend:=i;
          IF idle THEN idle:=FALSE; mipt:=ipt; ipt:=mdrv END
        END;
      END;
    ELSE -- IF {TxRDY}*CSR^#{} THEN (* transmit byte *)
      IF obeg=oend THEN   CMD:=CMD-{TxIE}; CSR^:=CMD;
      ELSE
        byte:=BITSET(obuf[obeg]);
        IF byte=NL THEN
          DTR^:=BITSET(15c); obuf[obeg]:=12c;
        ELSE
          DTR^:=byte;
          obeg:=(obeg+1) MOD (HIGH(obuf)+1);
          ocnt:=ocnt-1; IF ocnt=0 THEN os.send(ordy) END
        END;
      END
    END;
    transfer(drv,ipt)
  END
END driver;

PROCEDURE put(ch: BITSET);
  VAR i: INTEGER; m: BITSET;
BEGIN
--cnf.print("PUT=%03b\n",ch);
  m:=di();
   i:=(iend+1) MOD (HIGH(ibuf)+1);
   IF i#ibeg THEN ibuf[iend]:=ch; iend:=i END;
   IF irdy.queue#os.null THEN os.send(irdy) END;
  ei(m)
END put;

PROCEDURE get(): BITSET;
  VAR m,ch: BITSET;
BEGIN
  m:=di();
    IF kbeg=kend THEN idle:=TRUE; transfer(mdrv,mipt) END;
    ch:=kbuf[kbeg];
    kbeg:=(kbeg+1) MOD (HIGH(kbuf)+1);
  ei(m);
  RETURN ch
END get;

PROCEDURE error(VAR res: INTEGER; byte: BITSET);
BEGIN
  IF    byte*{OE}#{} THEN res:=err.over_run
  ELSIF byte*{PE}#{} THEN res:=err.par_err
  ELSIF byte*{FE}#{} THEN res:=err.frm_err
  ELSE                    res:=err.io_error
  END
END error;

PROCEDURE read(VAR r: req.REQUEST);
  VAR m: BITSET;
    str: STRING;          i: INTEGER;
   byte: BITSET;        len: INTEGER;
BEGIN
  str^.ADR:=r.buf;  str^.HIGH:=r.pos+r.len-1;
  len:=r.len;       r.len:=0;
  WHILE len>0 DO
    m:=di();
      IF iend=ibeg THEN
        i:=os.wait_del(-1,irdy);
        IF i#0 THEN r.res:=err.ipted_op; ei(m); RETURN END;
      END;                               -----
      byte:=ibuf[ibeg];
      ibeg:=(ibeg+1) MOD (HIGH(ibuf)+1);
    ei(m);
    str[r.pos]:=CHAR(byte*{0..7});
    DEC(len); INC(r.pos); INC(r.len);
--    IF (byte-{0..7})#{} THEN error(r.res,byte>>8); len:=0 END
  END
END read;

PROCEDURE write(VAR r: req.REQUEST);
  VAR s: POINTER TO ARRAY [0..0] OF CHAR;
  p,n,l: INTEGER; m: BITSET;
BEGIN
  WITH r DO
    l:=len; len:=0; p:=pos; s:=buf;
    WHILE l>0 DO
      m:=di();
      n:=BYTES(obuf)-(BYTES(obuf)+oend-obeg) MOD BYTES(obuf) - 2;
      IF n>0 THEN
        ei(m);
        IF n>l THEN n:=l END;
        DEC(l,n); INC(len,n);
        n:=p+n;
        REPEAT
          obuf[oend]:=s^[p];
          oend:=(oend+1) MOD BYTES(obuf);
          p:=p+1
        UNTIL p=n;
        m:=di();
          IF on_off#Xon THEN CMD:=CMD+{TxIE}; CSR^:=CMD END;
        ei(m);
      ELSE (* here l>0! and interrupts in CSR enable! *)
        ocnt:=153;
        IF os.wait_del(0,ordy)=1 THEN
          res:=err.ipted_op; pos:=p; ei(m); RETURN
        END;                         -----
        ei(m);
      END;
    END;
    pos:=p;
  END;
END write;

PROCEDURE wait(VAR r: req.REQUEST);
  VAR i: INTEGER;  m: BITSET;
BEGIN
  m:=di();
    IF ibeg#iend THEN ei(m); RETURN END;
    i:=(r.len+19) DIV 20;
    i:=os.wait_del(i,irdy);
  ei(m);
  IF    i>0 THEN r.res:=err.ipted_op
  ELSIF i<0 THEN r.res:=err.time_out
  ELSE
  END
END wait;

PROCEDURE power_off;
  VAR i: INTEGER;
    csr: ADDRESS;
BEGIN
  FOR i:=0 TO 3 DO
    csr :=ADDRESS(8200F1h+i*2);
    csr^:={};  csr^:={};
    csr^:={};  csr^:={};  csr^:={6}
  END
END power_off;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR m: BITSET;
BEGIN
  WITH r DO
    res:=err.ok;
    CASE op OF
      |req.READY    : m:=di();
                      IF ibeg<=iend THEN len:=iend-ibeg
                      ELSE               len:=HIGH(ibuf)+1-(ibeg-iend)
                      END;
                      ei(m)
      |req.READ     : read (r)
      |req.WRITE    : write(r)
      |req.WAIT     : wait (r)
      |req.POWER_OFF: power_off
      |req.GET_SPEC : r.smode:=req.stops1+req.parNO;
                      r.baud:=19200;
                      r.xon :=CHAR(Xon);  r.xoff :=CHAR(Xoff)
    ELSE
      res:=err.inv_op
    END
  END
END doio;

PROCEDURE raw_out(on: BOOLEAN);
BEGIN
  IF on THEN NL:=NULL ELSE NL:=BITSET(36c) END
END raw_out;

PROCEDURE raw_inp;
BEGIN put(get()) END raw_inp;

PROCEDURE x_inp(on: BOOLEAN);
BEGIN
  IF on=(on_off#NULL) THEN RETURN END;
  IF on THEN on_off:=Xon ELSE on_off:=NULL END
END x_inp;

PROCEDURE self(): os.PROCESS; CODE cod.activ END self;

PROCEDURE monitor(mon: PROC);
  VAR m: BITSET;
BEGIN
  m:=di();
    mdrv:=self();   idle:=TRUE;   os.suspend(os.active(),-1);
  ei(m-{1}); (* -{1} foolScheduller don't touch me *)
  LOOP mon END
END monitor;

VAR
  sav : ARRAY [0..1] OF INTEGER;
  trap: ADDRESS;

PROCEDURE stop;
  VAR m: BITSET;
BEGIN
  IF trap=NIL THEN RETURN END;
  m:=di();
    CSR^:={};  trap^:=sav[0];  CSR^:={};  trap:=trap+1;
    CSR^:={};  trap^:=sav[1];  CSR^:={};  trap:=NIL;
    CSR^:={RESET};
  ei(m)
END stop;

VAR wsp : ARRAY [0..255] OF INTEGER;

PROCEDURE init(chan: INTEGER; mode: BITSET): INTEGER;
  VAR m: BITSET; a: ADDRESS;
BEGIN
  IF (chan<0) OR (chan>3) THEN RETURN err.bad_parm END;
  DTR:=ADDRESS(8200F0h+chan*2);
  CSR:=ADDRESS(8200F1h+chan*2);
  a:=14h+chan;
  m:=di();
    os.new_process(driver,SYSTEM.ADR(wsp),SIZE(wsp),drv);
    a:=a*2;  sav[0]:=a^;  a^:=drv;    trap:=a;
    a:=a+1;  sav[1]:=a^;  a^:=SYSTEM.ADR(ipt);
  ei(m);
  reset(mode);
  RETURN err.ok
END init;

VAR i: INTEGER;

BEGIN
  trap:=NIL;
  i:=os.final(os.self(),stop);
  IF i#err.ok THEN HALT(i) END;
  NULL:=BITSET(-1);     Xoff:=XOFF;     idle:=FALSE;
  NL  :=NULL;           Xon :=XON ;     on_off:=Xoff;
  kbeg:=0;              ibeg:=0;        obeg:=0;
  kend:=0;              iend:=0;        oend:=0;
  ocnt:=-1;
  os.ini_signal(irdy,os.break+os.sendup,0);
  os.ini_signal(ordy,os.break+os.sendup,0)
END SIOws.
