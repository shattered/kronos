MODULE ipecho; (*  08-Dec-92. (c) KRONOS *)

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

CONST PROT = 31;
      TOUT = 100;

VAR

  DOIO  : drv.REFDOIO;

  free  : drv.BUFFER;
  todo  : drv.BUFFER;

(* output *)

  idsg,odsg: os.signal_rec;

  tbuf  : drv.BUFFER;

PROCEDURE ready(c: SYSTEM.WORD; n: drv.NODE; res: INTEGER);
BEGIN
  IF n=NIL THEN RETURN END;
  os.send(odsg)
END ready;

PROCEDURE next(c: SYSTEM.WORD; n: drv.NODE; VAR last: BOOLEAN; VAR len: INTEGER);
BEGIN

  len := tbuf^.len;
  last:= TRUE;

END next;

PROCEDURE omove(c: SYSTEM.WORD; a: SYSTEM.ADDRESS; do: INTEGER);
BEGIN
  nos.move(a,do,tbuf^.buf,tbuf^.pos,tbuf^.len);
END omove;

PROCEDURE transmit(bu: drv.BUFFER): INTEGER;
  VAR dr : drv.request;
      nd : drv.node;
      res: INTEGER;
BEGIN
  tbuf := bu;
  WITH nd DO
    dest:=bu^.ofs;
    try :=64;
    stat:=0;
    prot:=PROT;
    tout:=TOUT;
    obj :=NIL
  END;
  dr.prot:=PROT;
  dr.op  :=drv._transmit;
  dr.node:=SYSTEM.ADR(nd);
  res:=DOIO^(dr);
  IF res#err.ok THEN RETURN res END;
  os.wait(odsg);
  nos.tie( free, bu );
  RETURN dr.res
END transmit;

PROCEDURE alloc(cc: INTEGER; VAR buf: drv.BUFFER; is: drv.startproc;
                                 adr: SYSTEM.ADDRESS; p,l: INTEGER);

TYPE arr3 = POINTER TO ARRAY [0..3] OF CHAR;

VAR b: drv.BUFFER;
    a: arr3;

BEGIN
  buf:=NIL;
  IF free=NIL THEN RETURN END;
  b:=free;
  nos.untie( free );
  b^.pos := 0;
  b^.len := l;
  a := arr3( adr );
  b^.ofs := ORD( a^[0] );
  nos.move( b^.buf,0,adr,p,l );
  buf := b
END alloc;

PROCEDURE input(c: INTEGER; VAR b: drv.BUFFER);
BEGIN
  nos.tie( todo, b );
  os.send( idsg );
END input;

PROCEDURE stop;
BEGIN
  IF drv.removedriver(DOIO)#0 THEN END
END stop;

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
WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

PROCEDURE install;

  PROCEDURE new_buf(): BOOLEAN;
    VAR b: drv.BUFFER;
  BEGIN
    NEW(b); IF b=NIL THEN RETURN TRUE END;
    mem.allocate( b^.buf, 128 );
    IF b^.buf=NIL THEN DISPOSE(b); RETURN TRUE END;
    nos.tie( free, b );
    RETURN FALSE
  END new_buf;

  VAR r: INTEGER;
BEGIN
  free := NIL;
  todo := NIL;
  tbuf := NIL;
  FOR r:=0 TO 63 DO IF new_buf( ) THEN HALT( err.no_memory ) END END;

  os.ini_signal(odsg,{},0);
  os.ini_signal(idsg,{},0);

  env.final(stop);

  r:=definedriver("arc0");
  IF r#err.ok THEN HALT(r) END;
  env.become_ipr
END install;

PROCEDURE mainloop;
  VAR c: drv.BUFFER;
BEGIN
  LOOP
    os.wait(idsg);
    c := todo;
    IF c#NIL THEN
      nos.untie( todo );
      IF transmit( c )=0 THEN END
    END
  END
END mainloop;

BEGIN

  install;
  mainloop
END ipecho.
