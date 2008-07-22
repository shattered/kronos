IMPLEMENTATION MODULE netdrv[1]; (* Igo 14-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  err: defErrors;
IMPORT  nos: netKernel;
IMPORT  os : osKernel;

TYPE DRIVER = POINTER TO driver;
     driver = RECORD
                f,b : DRIVER;
                name: ARRAY [0..31] OF CHAR;
                doio: DOIO
              END;

VAR lock: os.signal_rec;
    dque: DRIVER;

PROCEDURE empty(VAR r: request): INTEGER;
BEGIN
  r.res:=err.undef;
  RETURN r.res
END empty;

PROCEDURE unlock;
BEGIN
  os.send(lock);
  os.unlock
END unlock;

PROCEDURE removedriver(r: REFDOIO): INTEGER;
  VAR d: DRIVER;
BEGIN
  os.lock;
    os.wait(lock);
    IF dque=NIL THEN unlock; RETURN err.undef END;
    d:=dque;
    REPEAT
      IF SYSTEM.ADR(d^.doio)=r THEN
        nos.untien(dque,d);
        d^.doio:=empty;
        unlock;
        RETURN err.ok
      END;
      d:=d^.f
    UNTIL d=dque;
  unlock;
  RETURN err.undef
END removedriver;

PROCEDURE definedriver(name: ARRAY OF CHAR; VAR r: REFDOIO; doio: DOIO): INTEGER;
  VAR d: DRIVER;
BEGIN
  r:=NIL;
  IF HIGH(name)>31 THEN RETURN err.bad_parm END;
  os.lock;
    IF os.wait_del(-1,lock)#0 THEN os.unlock; RETURN err.ipted_op END;
    name[HIGH(name)]:=0c;
    IF dque#NIL THEN
      d:=dque;
      REPEAT
        IF d^.name=name THEN unlock; RETURN err.duplicate END;
        d:=d^.f
      UNTIL d=dque
    END;
    nos.allocate(d,SIZE(d^));
    IF d=NIL THEN unlock; RETURN err.no_memory END;
    d^.name:=name;
    d^.doio:=doio;
    r:=SYSTEM.ADR(d^.doio);
    nos.tie(dque,d);
    os.send(lock);
  os.unlock;
  RETURN err.ok
END definedriver;

PROCEDURE open(name: ARRAY OF CHAR; VAR doio: REFDOIO): INTEGER;
  VAR d: DRIVER;
BEGIN
  doio:=NIL;
  os.lock;
    IF os.wait_del(-1,lock)#0 THEN os.unlock; RETURN err.ipted_op END;
    name[HIGH(name)]:=0c;
    IF dque#NIL THEN
      d:=dque;
      REPEAT
        IF d^.name=name THEN
          doio:=SYSTEM.ADR(d^.doio);
          unlock;
          RETURN err.ok
        END;
        d:=d^.f
      UNTIL d=dque
    END;
    os.send(lock);
  os.unlock;
  RETURN err.no_entry
END open;

BEGIN
  dque:=NIL;
  os.ini_signal(lock,os.break,1)
END netdrv.
