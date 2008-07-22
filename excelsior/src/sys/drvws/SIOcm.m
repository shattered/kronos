IMPLEMENTATION MODULE SIOcm; (*$U+$T- Leo 11-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  err: defErrors;
IMPORT   os: osKernel;
IMPORT  req: defRequest;


TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

VAR   NL: CHAR;
    NULL: CHAR;
     xon: CHAR;
    xoff: CHAR;
  on_off: CHAR;

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
  iRdy,iEi : BITSET;
  CMD      : BITSET;
  obuf     : ARRAY [0..255] OF CHAR;
  obeg,oend: INTEGER;
  ordy     : os.signal_rec;

PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE transfer(VAR from,to: os.PROCESS); CODE cod.tra END transfer;

PROCEDURE reset;
BEGIN
  CSR^:=CSR^*{6,7}+iEi;
END reset;

PROCEDURE driver;
  VAR i: INTEGER;
   byte: BITSET;
BEGIN
  LOOP
    IF CSR^*iRdy#{} THEN (* recieve byte *)
      byte:=DTR^*{0..7};
      i:=(kend+1) MOD (HIGH(kbuf)+1);
      IF i#kbeg THEN
        kbuf[kend]:=byte; kend:=i;
        IF idle THEN idle:=FALSE; mipt:=ipt; ipt:=mdrv END
      END;
    END;
    transfer(drv,ipt)
  END
END driver;

PROCEDURE put(ch: BITSET);
  VAR i: INTEGER; m: BITSET;
BEGIN
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
  END
END read;

PROCEDURE write(VAR r: req.REQUEST);
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

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    res:=err.ok;
    CASE op OF
      |req.READY   : IF ibeg<=iend THEN len:=iend-ibeg
                     ELSE               len:=HIGH(ibuf)+1-(ibeg-iend)
                     END
      |req.READ    : read (r)
      |req.WAIT    : wait (r)
    ELSE
      res:=err.inv_op
    END
  END
END doio;

PROCEDURE raw_out(on: BOOLEAN);
BEGIN
  IF on THEN NL:=NULL ELSE NL:=36c END
END raw_out;

PROCEDURE raw_inp;
BEGIN put(get()) END raw_inp;

PROCEDURE x_inp(on: BOOLEAN);
BEGIN
  IF on=(on_off#NULL) THEN RETURN END;
  IF on THEN on_off:=xon ELSE on_off:=NULL END
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
    trap^:=sav[0]; trap:=trap+1;
    trap^:=sav[1]; trap:=NIL; CSR^:=CSR^*{6,7};
  ei(m)
END stop;

VAR wsp : ARRAY [0..255] OF INTEGER;

PROCEDURE init;
  VAR m: BITSET; a: ADDRESS; VEC: INTEGER;
BEGIN
  iEi:={5}; iRdy:={13}; DTR:=ADDRESS(17E002h);
  CSR:=ADDRESS(17E003h); VEC:=30;
  m:=di();
    os.new_process(driver,SYSTEM.ADR(wsp),SIZE(wsp),drv);
    a:=VEC*2; sav[0]:=a^; a^:=drv; trap:=a;
    a:=a+1;   sav[1]:=a^; a^:=SYSTEM.ADR(ipt);
    reset;
  ei(m);
END init;

BEGIN
  trap:=NIL;
  NULL:=CHAR(-1);       idle:=FALSE;
  NL  :=NULL;           on_off:=NULL;
  kbeg:=0;              ibeg:=0;
  kend:=0;              iend:=0;
  os.ini_signal(irdy,os.break+os.sendup,0);
END SIOcm.
