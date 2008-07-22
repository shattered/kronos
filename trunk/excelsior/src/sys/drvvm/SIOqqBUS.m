IMPLEMENTATION MODULE SIOqqBUS; (* Igo & Leo 27-Nov-89. (c) KRONOS *)

(*$T-*)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  cod: defCodes;
IMPORT   os: osKernel;

CONST
  XOFF = 23c;
  XON  = 21c;

VAR   NL: CHAR;
    NULL: CHAR;
     xon: CHAR;
    xoff: CHAR;
  on_off: CHAR;

VAR     kbuf: ARRAY [0.. 63] OF CHAR;
        ibuf: ARRAY [0..255] OF CHAR;
   kbeg,kend: INTEGER;
   ibeg,iend: INTEGER;
      iready: os.signal_rec;

  kcsr, kdtr: INTEGER;
  ocsr, odtr: INTEGER;

TYPE str_ptr = POINTER TO ARRAY [0..0] OF CHAR;

PROCEDURE read(VAR r: req.REQUEST);
  VAR l,n: INTEGER;
    str: str_ptr;
BEGIN
  WITH r DO
    str:=buf; l:=len; len:=0;
    WHILE l>0 DO
      n:=os.wait_del(0,iready);
      ASSERT(n IN {0,1},n+100h);
      IF n=1 THEN res:=err.ipted_op; RETURN END;
      str^[pos]:=coder[ibuf[ibeg]];
                 ----- Hady. 06-Mar-90.
      DEC(l); INC(len); INC(pos);
      ibeg:=(ibeg+1) MOD BYTES(ibuf)
    END
  END
END read;

PROCEDURE calc_io_adr(q_adr: INTEGER): INTEGER;
  VAR i,j: INTEGER;
BEGIN
  i:=INTEGER(BITSET(q_adr)*{0..11});
  IF 0 IN BITSET(i) THEN
    j:=783800h;
  ELSE
    j:=683800h;
  END;
  i:=i DIV 2;
  RETURN INTEGER(BITSET(i)+BITSET(j));
END calc_io_adr;

PROCEDURE di(): BITSET; CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE inp(reg: INTEGER): CHAR;        CODE cod.inp END inp;
PROCEDURE out(reg, data: SYSTEM.WORD);    CODE cod.out END out;
PROCEDURE FE(n: SYSTEM.WORD); CODE 0FEh END FE;

VAR RUSSIAN,STOP: BOOLEAN;
VAR koi2ansi: ARRAY [0..255] OF CHAR;

PROCEDURE write(VAR r: req.REQUEST);
  VAR s: str_ptr;  ch: CHAR;
  p,n,l: INTEGER;
BEGIN
  l:=r.len; p:=r.pos; s:=r.buf;
  IF l<=0 THEN RETURN END;
  ch:=s^[p];
  LOOP
    REPEAT UNTIL BITSET(inp(ocsr))*{7}#{};
    IF STOP THEN os.delay(1) END;
    IF ch=NL THEN
      out(odtr,15c); ch:=12c;
      REPEAT UNTIL BITSET(inp(ocsr))*{7}#{}
    END;
    IF ch >= 240c THEN
       ch := koi2ansi[ORD(ch)];
    END;
    out(odtr,ch); INC(p); DEC(l);
    IF l=0 THEN r.pos:=p; RETURN END;
    ch:=s^[p]
  END
END write;

PROCEDURE wait(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=(r.len+19) DIV 20;
  i:=os.wait_del(i,iready);
  IF    i=1 THEN r.res:=err.ipted_op
  ELSIF i<0 THEN r.res:=err.time_out
  ELSE  os.send(iready)
  END;
END wait;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  r.smode :=req.parNO+req.stops1;
  r.baud  :=19200;
  r.xon   :=xon;
  r.xoff  :=xoff;
  r.trtin :=NIL;
  r.trtout:=NIL;
END get_spec;

PROCEDURE ready_no(VAR r: req.REQUEST);
  VAR m: BITSET;
BEGIN
  m:=di();
  IF ibeg<=iend THEN
    r.len:=iend-ibeg
  ELSE
    r.len:=BYTES(ibuf)-(ibeg-iend)
  END;
  ei(m)
END ready_no;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  r.res:=err.ok;
  CASE r.op OF
  |req.NOP      :
  |req.READ     : read (r)
  |req.WAIT     : wait (r)
  |req.READY    : ready_no(r);
  |req.WRITE    : write(r)
  |req.POWER_OFF:
  |req.GET_SPEC : get_spec(r)
  ELSE
    r.res:=err.inv_op
  END
END doio;

TYPE PROCESS = SYSTEM.ADDRESS;

VAR
  kdrv,kipt: PROCESS;
  mdrv,mipt: PROCESS;

      midle: BOOLEAN;

PROCEDURE transfer(VAR f,t: PROCESS); CODE cod.tra END transfer;

PROCEDURE kdriver;
  VAR ch: CHAR; i: INTEGER;
BEGIN
  LOOP
    ch:=CHAR(BITSET(inp(kdtr))*{0..7});
--FE(ch);
(* Leo: Feb-06-2001 removed Xon/Xoff for VM
    IF ch=on_off THEN
      IF ch=xon THEN
        STOP:=FALSE; on_off:=xoff
      ELSE
        STOP:=TRUE;  on_off:=xon
      END
    ELSE
*)
      i:=(kend+1) MOD BYTES(kbuf);
      IF i#kbeg THEN
        kbuf[kend]:=ch; kend:=i;
        IF midle THEN mipt:=kipt; kipt:=mdrv; midle:=FALSE END
      END;
--  END;
    transfer(kdrv,kipt)
  END
END kdriver;

PROCEDURE put(ch: CHAR);
  VAR i: INTEGER;
BEGIN
  i:=(iend+1) MOD BYTES(ibuf);
  IF i#ibeg THEN ibuf[iend]:=ch; iend:=i; os.send(iready) END
END put;

PROCEDURE get(): CHAR;
  VAR ch: CHAR; m: BITSET;
BEGIN
  m:=di();
    IF kbeg=kend THEN midle:=TRUE; transfer(mdrv,mipt) END;
  ei(m);
  ch:=kbuf[kbeg];
  kbeg:=(kbeg+1) MOD BYTES(kbuf);
  RETURN ch
END get;

PROCEDURE init_state;
  VAR ch: CHAR;
BEGIN
  NULL:=CHAR(-1);    kbeg:=0;   midle :=TRUE;
  xoff:=XOFF;        kend:=0;   on_off:=xoff;
  xon :=XON ;        ibeg:=0;   RUSSIAN:=FALSE;
  NL  :=36c ;        iend:=0;   STOP   :=FALSE;
  FOR ch:=MIN(CHAR) TO MAX(CHAR) DO coder[ch]:=ch END
END init_state;

VAR
  kwsp: ARRAY [0..63] OF INTEGER;

PROCEDURE init(csr: INTEGER; trap: SYSTEM.ADDRESS): INTEGER;
  VAR m,tst: BITSET;
BEGIN
  kcsr:=csr DIV 2;
  kdtr:=kcsr+1;
  ocsr:=kcsr+2;
  odtr:=kcsr+3;
  m:=di();
  out(kcsr,{6}); tst:=BITSET(inp(kcsr)); out(kcsr,{});
  IF tst*{6}={}  THEN ei(m); RETURN err.not_ready END;
                             ------
  os.ini_signal(iready,os.break+os.sendup,0);
  os.new_process(kdriver,SYSTEM.ADR(kwsp),SIZE(kwsp),kdrv);
  trap:=trap DIV 4 * 2;
  trap^:=kdrv; INC(trap); trap^:=SYSTEM.ADR(kipt);
  out(ocsr,000b);
  ei(m);
  RETURN err.ok
END init;

PROCEDURE stop;
  VAR m: BITSET;
BEGIN
  m:=di(); out(ocsr,000b); out(kcsr,000b); ei(m)
END stop;

PROCEDURE raw_out(on: BOOLEAN);
BEGIN
  IF on THEN NL:=NULL ELSE NL:=36c END
END raw_out;

PROCEDURE x_inp(on: BOOLEAN);
BEGIN
  IF on=(on_off#NULL) THEN RETURN END;
  IF on THEN on_off:=xon ELSE on_off:=NULL END
END x_inp;

PROCEDURE self(): PROCESS; CODE cod.activ END self;

PROCEDURE monitor(mon: PROC);
  VAR m: BITSET;
BEGIN
  mdrv:=self();
  m:=di();
  out(kcsr,100b);
  ei(m-{1});
  os.suspend(os.active(),-1);
  LOOP mon END
END monitor;

PROCEDURE initKOI;
  VAR i: INTEGER;
BEGIN
  FOR i:=0   TO 255 DO koi2ansi[i] := CHR(i) END;
RETURN;
  FOR i:=128 TO 255 DO koi2ansi[i] := "*" END;
  FOR i:=340b TO 255 DO koi2ansi[i] := "+" END;

  FOR i:=300b TO 317b  DO koi2ansi[i] := CHR(240b+i-300b) END;
  FOR i:=320b TO 337b  DO koi2ansi[i] := CHR(340b+i-320b) END;

  FOR i:=340b TO 377b  DO koi2ansi[i] := CHR(200b+i-340b) END;

END initKOI;


BEGIN
  initKOI;
  init_state
END SIOqqBUS.
