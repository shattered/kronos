MODULE pcn; (* $N+ Igo&John 02-Oct-91. (c) KRONOS *)
            (*     Igo      13-Dec-91. (c) KRONOS *)
            (*     Igo      08-Apr-92. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  drv: netdrv;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;
IMPORT  err: defErrors;
IMPORT  env: tskEnv;
IMPORT  bio: BIO;
IMPORT  mem: Heap;
IMPORT  low: lowLevel;
IMPORT  arg: tskArgs;

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
 bFILE  : bio.FILE;
    WE  : BOOLEAN;  (* write enable *)

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

  len:=len DIV 2 * 2;

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

PROCEDURE transmit(a: SYSTEM.ADDRESS; adr,to,xid,pos,len: INTEGER): INTEGER;
  VAR dr : drv.request;
      nd : drv.node;
      res: INTEGER;
BEGIN
  IF len<0 THEN len:=0 END;
  OTTL :=len;
  OBUF :=a;
  OXID :=xid;
  WITH nd DO
    dest:=adr;
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

PROCEDURE stop;
BEGIN
  IF drv.removedriver(DOIO)#0 THEN END
END stop;


TYPE spec = RECORD
              heads  : INTEGER;
              tracks : INTEGER;
              sectors: INTEGER;
              total  : INTEGER;
            END;

VAR drv_spec: spec;

PROCEDURE definedriver(VAL nm: ARRAY OF CHAR): INTEGER;

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
  DOIO:=NIL;
  r:=drv.open (nm,DOIO); IF r#err.ok THEN RETURN r END;
  r:=installme();        IF r#err.ok THEN RETURN r END;
  RETURN err.ok
END definedriver;

----------------------------- INPUT ----------------------------
                             -------
CONST READ  = 1;
      WRITE = 2;
      MOUNT = 3;

TYPE iheader = RECORD
                 op : INTEGER;
                 xid: INTEGER;
                 ofs: INTEGER;
                 pos: INTEGER;
                 len: INTEGER;
                 ttl: INTEGER;
               END;

     CHANNEL = POINTER TO channel;
     channel = RECORD
                 f,b : CHANNEL;
                 adr : INTEGER; (* remote arcnet address *)
                 lock: BOOLEAN;
                 idle: BOOLEAN;
                 op  : INTEGER; (* net operation *)
                 pos : INTEGER;
                 ofs : INTEGER;
                 ttl : INTEGER;
                 xid : INTEGER;
                 wbuf: ARRAY [0..1024*64-1] OF CHAR;
                 fat : ARRAY [0..1024*48-1] OF CHAR;
                 nfat: INTEGER; (* first free block           *)
                 ff  : INTEGER; (* first byte of write buffer *)
                 fsz : INTEGER; (* buffer size                *)
               END;

VAR chan: CHANNEL;

    idsg: os.signal_rec;

    RBUF: ARRAY [0..64*1024-1] OF CHAR;

PROCEDURE alloc(cc: INTEGER; VAR buf: drv.BUFFER; is: drv.startproc;
                                 adr: SYSTEM.ADDRESS; p,l: INTEGER);

  TYPE bptr = POINTER TO ARRAY [0..3] OF CHAR;

  VAR h: iheader;
      a: INTEGER;
      b: bptr;
      c: CHANNEL;
BEGIN
  buf:=NIL;

  IF l<BYTES(h) THEN RETURN END;
  nos.move(SYSTEM.ADR(h),0,adr,p,BYTES(h)); INC(p,BYTES(h)); DEC(l,BYTES(h));

  IF h.len<0 THEN RETURN END;
  IF h.ttl<0 THEN RETURN END;
  IF h.pos<0 THEN RETURN END;
  IF h.len<l THEN RETURN END;

  b:=bptr(adr);
  a:=INTEGER(b^[0]);

  c:=chan;
  IF c=NIL THEN RETURN END;
  REPEAT
    c:=c^.f
  UNTIL (c=chan) OR (c^.adr=a);
  IF a#c^.adr THEN RETURN END;
  IF c^.lock  THEN RETURN END;

  IF h.op=READ THEN
    c^.idle:=TRUE;
    IF l#0 THEN RETURN END;
    c^.op  :=READ;
    c^.xid :=h.xid;
    c^.ofs :=h.ofs;
    c^.ttl :=h.ttl;
    IF c^.ttl>BYTES(RBUF) THEN RETURN END;
    c^.lock:=TRUE;
    os.send(idsg);
    RETURN
  ELSIF h.op=WRITE THEN
    IF NOT c^.idle THEN
      IF    (h.pos#c^.pos) OR (h.ofs#c^.ofs) OR (h.ttl#c^.ttl)
         OR (h.xid#c^.xid) OR (c^.op#WRITE)
      THEN
        c^.idle:=TRUE; RETURN
      END
    ELSE
      c^.op :=WRITE;
      IF (h.pos#0) OR (h.ttl>BYTES(c^.wbuf)) THEN c^.idle:=TRUE; RETURN END;
      c^.pos:=0;
      c^.xid:=h.xid;
      c^.ofs:=h.ofs;
      c^.ttl:=h.ttl
    END;
    IF c^.pos+h.len>c^.ttl THEN c^.idle:=TRUE; RETURN END;
    nos.move(SYSTEM.ADR(c^.wbuf),c^.pos,adr,p,h.len);
    INC(c^.pos,h.len);
    IF c^.pos=c^.ttl THEN
      c^.lock:=TRUE;
      c^.idle:=TRUE;
      os.send(idsg);
      RETURN
    END;
    c^.idle:=FALSE;
  ELSIF h.op=MOUNT THEN
    c^.idle:=TRUE;
    IF l#0 THEN RETURN END;
    c^.op  :=h.op;
    c^.xid :=h.xid;
    c^.ofs :=h.ofs;
    c^.ttl :=h.ttl;
    IF c^.ttl>BYTES(RBUF) THEN RETURN END;
    c^.lock:=TRUE;
    os.send(idsg);
    RETURN
  ELSE
    c^.idle:=TRUE
  END
END alloc;

PROCEDURE input(c: INTEGER; VAR b: drv.BUFFER);
BEGIN
END input;

PROCEDURE create(adr,ofs,len: INTEGER);
  VAR c: CHANNEL;
BEGIN
  c:=chan;
  IF c#NIL THEN
    REPEAT
      IF c^.adr=adr THEN HALT(1) END; c:=c^.f
    UNTIL c=chan
  END;
  mem.allocate(c,SIZE(c^));
  IF c=NIL THEN HALT(mem.error) END;
  low.fill(c^.fat,-1);
  c^.lock:=FALSE;
  c^.idle:=TRUE;
  c^.adr :=adr;
  c^.ff  :=ofs;
  c^.fsz :=len;
  c^.nfat:=0;
  nos.tie(chan,c)
END create;

CONST  file = "/dev/dos0";
      bfile = "/DATA.NET";

PROCEDURE install;
  VAR r,len,c,pos: INTEGER;  h: CHANNEL;
BEGIN
  chan:=NIL;

  IF arg.number("c",c) THEN WE:=TRUE ELSE WE:=FALSE END;

  IF WE THEN
    bio.open(FILE,file,'rw')
  ELSE
    bio.open(FILE,file,'r')
  END;
  IF NOT bio.done THEN HALT(bio.error) END;

  bFILE:=bio.null;
  IF NOT WE THEN
    bio.open(bFILE,bfile,'rw');
    IF NOT bio.done THEN HALT(bio.error) END
  END;

drv_spec.heads  :=12;
drv_spec.sectors:=14;
drv_spec.tracks :=970;
drv_spec.total  :=drv_spec.heads*drv_spec.sectors*drv_spec.tracks;

  IF WE THEN
    create(c,0,0)
  ELSE
--  len:=bio.eof(bFILE) DIV 6 DIV 4096;
    len:=bio.eof(bFILE) DIV 5 DIV 4096;
    pos:=0;

    create(1,pos,len); INC(pos,len);
    create(3,pos,len); INC(pos,len);
--  create(4,pos,len); INC(pos,len);
    create(5,pos,len); INC(pos,len);
    create(7,pos,len); INC(pos,len);
    create(8,pos,len); INC(pos,len)
  END;

  os.ini_signal(odsg,{},0);
  os.ini_signal(idsg,{},0);

  env.final(stop);

  r:=definedriver("arc0");
  IF r#err.ok THEN HALT(r) END;
  env.become_ipr
END install;

PROCEDURE index(c: CHANNEL; b: INTEGER): INTEGER;
BEGIN
  RETURN INTEGER(c^.fat[b*2])+INTEGER(c^.fat[b*2+1])*100h
END index;

PROCEDURE allocindex(c: CHANNEL; b: INTEGER): INTEGER;
BEGIN
  IF c^.nfat>=c^.fsz THEN RETURN -1 END;
  c^.fat[b*2  ]:=CHAR(c^.nfat);
  c^.fat[b*2+1]:=CHAR(c^.nfat DIV 100h);
  INC(c^.nfat);
  RETURN c^.nfat-1
END allocindex;

PROCEDURE read(c: CHANNEL; offs,len: INTEGER): BOOLEAN;
  VAR pos,bofs,b,l: INTEGER;
BEGIN
  IF WE THEN
    bio.seek(FILE,offs,0);               IF NOT bio.done THEN RETURN FALSE END;
    bio.read(FILE,SYSTEM.ADR(RBUF),len); IF NOT bio.done THEN RETURN FALSE END;
    RETURN TRUE
  END;
  bofs:=offs DIV 4096;
  offs:=offs MOD 4096; pos:=0;
  WHILE len>0 DO
    b:=index(c,bofs);
    l:=4096-offs;
    IF l>len THEN l:=len END;
    IF b=0FFFFh THEN
      bio. seek(FILE,bofs*4096+offs,0);       IF NOT bio.done THEN RETURN FALSE END;
      bio.fread(FILE,SYSTEM.ADR(RBUF),pos,l); IF NOT bio.done THEN RETURN FALSE END;
    ELSE
      b:=b+c^.ff;
      bio. seek(bFILE,b*4096+offs,0);          IF NOT bio.done THEN RETURN FALSE END;
      bio.fread(bFILE,SYSTEM.ADR(RBUF),pos,l); IF NOT bio.done THEN RETURN FALSE END;
    END;
    DEC(len,l); INC(bofs); INC(pos,l);
    offs:=0
  END;
  RETURN TRUE
END read;

VAR BBUF: ARRAY [0..4095] OF CHAR;

PROCEDURE write(c: CHANNEL; offs,len: INTEGER): BOOLEAN;
  VAR pos,bofs,b,l: INTEGER;
BEGIN
  IF WE THEN
    bio.seek (FILE,offs,0);                  IF NOT bio.done THEN RETURN FALSE END;
    bio.write(FILE,SYSTEM.ADR(c^.wbuf),len); IF NOT bio.done THEN RETURN FALSE END;
    RETURN TRUE
  END;
  bofs:=offs DIV 4096;
  offs:=offs MOD 4096; pos:=0;
  WHILE len>0 DO
    b:=index(c,bofs);
    l:=4096-offs;
    IF l>len THEN l:=len END;
    IF b=0FFFFh THEN
      b:=allocindex(c,bofs); IF b<0 THEN RETURN FALSE END;
      b:=b+c^.ff;
      IF l<4096 THEN
        bio.seek (FILE,bofs*4096,0);            IF NOT bio.done THEN RETURN FALSE END;
        bio.read (FILE,SYSTEM.ADR(BBUF),4096);  IF NOT bio.done THEN RETURN FALSE END;
        bio.seek (bFILE,b*4096,0);              IF NOT bio.done THEN RETURN FALSE END;
        bio.write(bFILE,SYSTEM.ADR(BBUF),4096); IF NOT bio.done THEN RETURN FALSE END;
      END
    ELSE
      b:=b+c^.ff
    END;

    bio.seek  (bFILE,b*4096+offs,0);
    IF NOT bio.done THEN RETURN FALSE END;
    bio.fwrite(bFILE,SYSTEM.ADR(c^.wbuf),pos,l);
    IF NOT bio.done THEN RETURN FALSE END;

    DEC(len,l); INC(bofs); INC(pos,l);
    offs:=0
  END;
  RETURN TRUE
END write;

PROCEDURE mainloop;
  VAR ignore: INTEGER;
      xid   : INTEGER;
      ttl   : INTEGER;
      c     : CHANNEL;
      retry : BOOLEAN;
BEGIN
  retry:=FALSE;
  LOOP

    os.wait(idsg);

    c:=chan;
    IF c#NIL THEN
      REPEAT
        c:=c^.f
      UNTIL (c=chan) OR (c^.lock=TRUE);
      IF c^.lock THEN

        xid:=c^.xid;
        ttl:=c^.ttl;

        IF c^.op=READ THEN

          IF read(c,c^.ofs,ttl) THEN
            c^.lock:=FALSE;
            ignore:=transmit(SYSTEM.ADR(RBUF),c^.adr,200,xid,0,ttl)
          ELSE
            c^.lock:=FALSE
          END
        ELSIF c^.op=WRITE THEN

          IF write(c,c^.ofs,ttl) THEN
            c^.lock:=FALSE;
            ignore:=transmit(NIL,c^.adr,200,xid,0,0)
          ELSE
            c^.lock:=FALSE
          END
        ELSIF c^.op=MOUNT THEN
          c^.lock:=FALSE;
          ignore:=transmit(SYSTEM.ADR(drv_spec),c^.adr,200,xid,0,BYTES(drv_spec))
        ELSE
          c^.lock:=FALSE
        END
      END
    END
  END
END mainloop;

BEGIN
  m0FFFF:={0..15};
  install;
  mainloop
END pcn.
