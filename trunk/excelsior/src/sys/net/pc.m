MODULE pc; (* $N+ Igo&John 02-Oct-91. (c) KRONOS *)
           (*     Igo      13-Dec-91. (c) KRONOS *)
           (*     Igo      08-Apr-92. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  drv: netdrv;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  err: defErrors;
IMPORT  env: tskEnv;
IMPORT  bio: BIO;

IMPORT  tty: Terminal;

TYPE oheader = RECORD
                 xid: INTEGER;
                 pos: INTEGER;
                 len: INTEGER;
                 ttl: INTEGER;
               END;

CONST PROT = 10h;

VAR

  m0FFFF: BITSET;

  DOIO  : drv.REFDOIO;
  FILE  : bio.FILE;
  LOCK  : BOOLEAN;

(* output *)

  odsg  : os.signal_rec;
  OHDR  : oheader;
  OLEN  : INTEGER;
  OTTL  : INTEGER;
  OPOS  : INTEGER;
  OXID  : INTEGER;
  OBUF  : SYSTEM.ADDRESS;

PROCEDURE ready(c: SYSTEM.WORD; n: drv.NODE; res: INTEGER);
BEGIN
  IF n=NIL THEN RETURN END;
  os.send(odsg)
END ready;

PROCEDURE next(c: SYSTEM.WORD; n: drv.NODE; VAR last: BOOLEAN; VAR len: INTEGER);
BEGIN

  OLEN:=len-BYTES(OHDR);
  IF OLEN>(OTTL-n^.stat) THEN
    OLEN:=OTTL-n^.stat; last:=TRUE
  ELSE
    last:=FALSE
  END;
  len:=OLEN+BYTES(OHDR);

  OHDR.xid:=OXID;
  OHDR.pos:=n^.stat;
  OHDR.len:=OLEN;
  OHDR.ttl:=OTTL;

  OPOS:=n^.stat;
  INC(n^.stat,OLEN)

END next;

PROCEDURE omove(c: SYSTEM.WORD; a: SYSTEM.ADDRESS; do: INTEGER);
BEGIN
  nos.move(a,do,SYSTEM.ADR(OHDR),0,BYTES(OHDR));
  INC(do,BYTES(OHDR));
  nos.move(a,do,OBUF,OPOS,OLEN)
END omove;

PROCEDURE transmit(a: SYSTEM.ADDRESS; to,xid,pos,len: INTEGER): INTEGER;
  VAR dr : drv.request;
      nd : drv.node;
      res: INTEGER;
BEGIN
  IF len<0 THEN len:=0 END;
  OTTL :=len;
  OBUF :=a;
  OXID :=xid;
  WITH nd DO
    dest:=4;
    try :=64;
    stat:=0;
    prot:=PROT;
    tout:=to;
    obj :=NIL
  END;
  dr.prot:=PROT;
  dr.op  :=drv._transmit;
  dr.node:=SYSTEM.ADR(nd);
  res:=DOIO^(dr);
  IF res#err.ok THEN RETURN res END;
  os.wait(odsg);
  RETURN dr.res
END transmit;

PROCEDURE alloc(c: INTEGER; VAR b: drv.BUFFER; is: drv.startproc;
                              adr: SYSTEM.ADDRESS; p,l: INTEGER);
FORWARD;

PROCEDURE input(c: INTEGER; VAR b: drv.BUFFER);
FORWARD;

PROCEDURE definedriver(VAL nm,file: ARRAY OF CHAR): INTEGER;


  PROCEDURE installme(): INTEGER;
    VAR d: drv.request;
  BEGIN
    d.op      :=drv._install;
    d.prot    :=PROT;
    d.alloc   :=alloc;
    d.input   :=input;
    d.move    :=omove;
    d.next    :=next;
    d.ready   :=ready;
    d.channel :=NIL;
    d.res     :=DOIO^(d);
    RETURN d.res
  END installme;

  VAR r: INTEGER;

BEGIN
  bio.open(FILE,file,'rw');
  IF NOT bio.done THEN RETURN bio.error END;

  r:=drv.open (nm,DOIO); IF r#err.ok THEN RETURN r END;
  r:=installme();        IF r#err.ok THEN RETURN r END;
  RETURN err.ok
END definedriver;

----------------------------- INPUT ----------------------------
                             -------
CONST READ  = 1;
      WRITE = 2;

TYPE iheader = RECORD
                 op : INTEGER;
                 xid: INTEGER;
                 ofs: INTEGER;
                 pos: INTEGER;
                 len: INTEGER;
                 ttl: INTEGER;
               END;

VAR IDLE: BOOLEAN;
    IOP : INTEGER;
    IPOS: INTEGER;
    IOFS: INTEGER;
    ITTL: INTEGER;
    IXID: INTEGER;


    idsg: os.signal_rec;

    readbuf : ARRAY [0..1024*64-1] OF CHAR;
    writebuf: ARRAY [0..1024*64-1] OF CHAR;

PROCEDURE alloc(c: INTEGER; VAR b: drv.BUFFER; is: drv.startproc;
                              adr: SYSTEM.ADDRESS; p,l: INTEGER);
  VAR h: iheader;
BEGIN
  b:=NIL;
  IF LOCK THEN RETURN END;

--spo.print('get %d bytes\n',l);

  IF l<BYTES(h) THEN RETURN END;
  nos.move(SYSTEM.ADR(h),0,adr,p,BYTES(h)); INC(p,BYTES(h)); DEC(l,BYTES(h));

(*
spo.print("len = %d\n",h.len);
spo.print("pos = %d\n",h.pos);
spo.print("ttl = %d\n",h.ttl);
spo.print("xid = %d\n",h.xid);
spo.print("op  = %d\n",h.op);
spo.print("ofs = %d\n",h.ofs);
*)
  IF h.len<0 THEN RETURN END;
  IF h.ttl<0 THEN RETURN END;
  IF h.pos<0 THEN RETURN END;
  IF h.len<l THEN RETURN END;

  IF h.op=READ THEN
    IDLE:=TRUE;
    IF l#0 THEN RETURN END;
    IOP :=READ;
    IXID:=h.xid;
    IOFS:=h.ofs;
    ITTL:=h.ttl;
    IF ITTL>BYTES(readbuf) THEN RETURN END;
    LOCK:=TRUE;
    os.send(idsg);
    RETURN
  ELSIF h.op=WRITE THEN
    IF NOT IDLE THEN
      IF (h.pos#IPOS) OR (h.ofs#IOFS) OR (h.ttl#ITTL) OR (h.xid#IXID)
      THEN
        IDLE:=TRUE; RETURN
      END
    ELSE
      IOP :=WRITE;
      IF (h.pos#0) OR (h.ttl>BYTES(writebuf)) THEN IDLE:=TRUE; RETURN END;
      IPOS:=0;
      IXID:=h.xid;
      IOFS:=h.ofs;
      ITTL:=h.ttl
    END;
    IF IPOS+h.len>ITTL THEN IDLE:=TRUE; RETURN END;
    nos.move(SYSTEM.ADR(writebuf),IPOS,adr,p,h.len);
    INC(IPOS,h.len);
    IF IPOS=ITTL THEN
      LOCK:=TRUE;
      IDLE:=TRUE;
      os.send(idsg);
      RETURN
    END;
    IDLE:=FALSE;
  ELSE
    IDLE:=FALSE
  END
END alloc;

PROCEDURE input(c: INTEGER; VAR b: drv.BUFFER);
BEGIN
END input;

PROCEDURE install;
  VAR r: INTEGER;
BEGIN

  IDLE:=TRUE;
  LOCK:=FALSE;

  os.ini_signal(odsg,{},0);
  os.ini_signal(idsg,{},0);

  r:=definedriver("arc0","/dev/dk5");
  IF r#err.ok THEN HALT(r) END;
  env.become_ipr
END install;

PROCEDURE mainloop;
  VAR ignore: INTEGER;
      xid   : INTEGER;
      ttl   : INTEGER;
BEGIN
  LOOP

    os.wait(idsg);

 --tty.print('get message\n');

    xid:=IXID;
    ttl:=ITTL;

    IF    IOP=READ THEN

 --tty.print('read\n');

      bio.seek(FILE,IOFS,0);
      IF bio.done THEN
        bio.read(FILE,SYSTEM.ADR(readbuf),ttl);
        LOCK:=FALSE;
        IF bio.done THEN
          ignore:=transmit(SYSTEM.ADR(readbuf),200,xid,0,ttl)
        END
      ELSE
        LOCK:=FALSE
      END
    ELSIF IOP=WRITE THEN

 --tty.print('write\n');

      bio.seek(FILE,IOFS,0);
      IF bio.done THEN
        bio.write(FILE,SYSTEM.ADR(writebuf),ttl);
        LOCK:=FALSE;
        IF bio.done THEN
          ignore:=transmit(NIL,200,xid,0,0)
        END
      ELSE
        LOCK:=FALSE
      END
    ELSE
      LOCK:=FALSE
 --tty.print('unknown message %d\n',IOP);

    END
  END
END mainloop;

BEGIN
  m0FFFF:={0..15};
  install;
  mainloop
END pc.
