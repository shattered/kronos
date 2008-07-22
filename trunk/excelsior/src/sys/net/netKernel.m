IMPLEMENTATION MODULE netKernel[1]; (*  $N+ 01-Oct-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  os : osKernel;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;

--IMPORT  tty: Terminal;

PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;
PROCEDURE _self(): ADDRESS; CODE cod.activ END _self;
PROCEDURE transfer(VAR f,t: ADDRESS); CODE cod.tra END transfer;


--------------------------- BYTE MOVE --------------------------
                           -----------

PROCEDURE bmv(t: ADDRESS; to: INTEGER; f: ADDRESS; fo,len: INTEGER);
CODE cod.bmv END bmv;

PROCEDURE move(t: ADDRESS; to: INTEGER; f: ADDRESS; fo,len: INTEGER);
BEGIN
  bmv(t,to*8,f,fo*8,len*8)
END move;

---------------------------- BUFFERS ---------------------------
                            ---------

PROCEDURE bmove(t: ADDRESS; to: INTEGER; VAR f: buffer; len: INTEGER);
BEGIN
  WHILE len>0 DO
    IF f.buf =NIL THEN RETURN END;
    IF f.len<0    THEN RETURN END;
    IF f.len>len THEN
      move(t,to,f.buf,f.pos,len);
      DEC(f.len,len);
      INC(f.pos,len);
      RETURN
    END;
    move(t,to,f.buf,f.pos,f.len);
    INC(to,f.len);
    DEC(len,f.len);
    IF f.next=NIL THEN
      INC(f.pos,f.len); f.len:=0; f.buf:=NIL;
      RETURN
    END;
    f:=f.next^
  END
END bmove;

PROCEDURE bfind(VAR f: buffer; pos: INTEGER);
BEGIN
  WHILE pos>0 DO
    IF f.len>pos THEN
      DEC(f.len,pos);
      INC(f.pos,pos);
      RETURN
    END;
    DEC(pos,f.len);
    IF f.next=NIL THEN
      INC(f.pos,f.len); f.len:=0; f.buf:=NIL;
      RETURN
    END;
    f:=f.next^
  END
END bfind;

---------------------------- MEMORY ----------------------------
                            --------

VAR mlock: os.signal_rec;
    marea: os.AREA;

PROCEDURE allocate(VAR a: ADDRESS; sz: INTEGER);
BEGIN
  os.ALLOCATE(marea,a,sz);
END allocate;

PROCEDURE deallocate(VAR a: ADDRESS; sz: INTEGER);
BEGIN
  os.DEALLOCATE(marea,a,sz);
END deallocate;

PROCEDURE reallocate(VAR a: ADDRESS; VAR high: INTEGER;
                                 len,byte_size: INTEGER);
  VAR res     : ADDRESS;
      size    : INTEGER;
      new_size: INTEGER;

  CONST b=BYTES(SYSTEM.WORD);

BEGIN
  size    := ((high+1)*byte_size+b-1) DIV b;
  new_size:= (len*byte_size+b-1)  DIV b;
  IF (size<0) OR (new_size<0) THEN RETURN END;
  IF (size=0) OR (a=NIL) THEN
    IF new_size=0 THEN a:=NIL; high:=len-1; RETURN END;
    allocate(a,new_size);
    IF a=NIL THEN high:=-1 ELSE high:=len-1 END;
    RETURN
  END;
  IF new_size=0 THEN
    deallocate(a,size);
    a:=NIL; high:=-1;
    RETURN
  END;
  allocate(res,new_size);
  IF res=NIL THEN RETURN END;
  move(res,0,a,0,new_size*b);
  deallocate(a,size);
  a:=res; high:=len-1
END reallocate;

---------------------------- TIMEOUT ---------------------------
                            ---------

VAR h    : TIME;
    CLOCK: INTEGER;
    to   : process;
    to0  : os.process;
    tsg  : os.signal_rec;

PROCEDURE settout(VAR n: time; t: INTEGER);
  VAR p: TIME;
      m: BITSET;
BEGIN
  m:=di();
    IF t<0 THEN n.tout:=-1; ei(m); RETURN END;
    n.tout:=CLOCK + t;
    IF h=NIL THEN
      h:=SYSTEM.ADR(n); h^.f:=h; h^.b:=h
    ELSE
      p:=h;

      IF n.tout>=p^.tout THEN
        p:=p^.b;
        WHILE n.tout<p^.tout DO p:=p^.b END;
        n.f:=p^.f; n.b:=p
      ELSE
        h:=SYSTEM.ADR(n);
        n.f:=p   ; n.b:=p^.b
      END;

      n.f^.b:=SYSTEM.ADR(n);
      n.b^.f:=SYSTEM.ADR(n)

    END;
  ei(m)
END settout;

PROCEDURE tuntie;
BEGIN
  h^.tout:=-1; untie(h)
END tuntie;

PROCEDURE deltout(VAR n: time);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF n.tout<0 THEN ei(m); RETURN END;
    n.tout:=-1;
    IF h=NIL THEN ei(m); RETURN END;
    IF SYSTEM.ADR(n)=h THEN h:=h^.f END;
    IF SYSTEM.ADR(n)=h THEN h:=NIL
    ELSE
      n.f^.b:=n.b; n.b^.f:=n.f
    END;
  ei(m)
END deltout;

PROCEDURE timer;
  VAR t: TIME;
      m: BITSET;
BEGIN
  LOOP
    m:=di();
      WHILE (h=NIL) OR (CLOCK<=h^.tout) DO wait END;
      t:=h; tuntie;
      lock(63);
    ei(m);
    t^.done(t);
    unlock;
  END
END timer;

PROCEDURE timer0;
  VAR m: BITSET;
BEGIN
  m:=di();
  LOOP os.wait(tsg); call(to) END
END timer0;

PROCEDURE clock;
  VAR a: ADDRESS;
BEGIN
  INC(CLOCK);
  a:=_self();
  IF (h#NIL)&(CLOCK>h^.tout) THEN
    IF tsg.queue#os.null THEN os.send(tsg) END
  END
END clock;

PROCEDURE stop;
  VAR his: ARRAY [0..255] OF CHAR;
BEGIN
  os.remove_action(clock);

--  os.extract(his,to0^.pp ); tty.print('to0\n%s\n',his);
--  os.extract(his,to.self ); tty.print('to\n%s\n',his);

  os.stop(to0,TRUE)
END stop;

VAR ipted: SYSTEM.ADDRESS;
    ready: PROCESS;

PROCEDURE new(VAR prs: process; p: PROC; size,prio: INTEGER): BOOLEAN;
  VAR osp: os.process;
BEGIN
  IF os.make_process(osp,p,size)#err.ok THEN RETURN FALSE END;
  prs.pri0:=prio;
  prs.prio:=prio;
  prs.work:=FALSE;
  prs.self:=osp^.pp;
  osp^.prio:=-1;
  RETURN TRUE
END new;

PROCEDURE active(): PROCESS;
BEGIN
  RETURN ready
END active;

PROCEDURE lock(prio: INTEGER);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF ready^.prio>=prio THEN ei(m); RETURN END;
    ready^.prio:=prio;
  ei(m)
END lock;

PROCEDURE unlock;
  VAR m   : BITSET;
      p,pp: PROCESS;
BEGIN
  m:=di();
    ready^.prio:=ready^.pri0;
    p:=ready^.next; pp:=ready;
    IF (p=NIL) OR (p^.prio<=ready^.prio) THEN ei(m); RETURN END;
    WHILE (p#NIL)&(p^.prio>ready^.prio) DO pp:=p; p:=p^.next END;
    p    :=ready;
    ready:=ready^.next;
    p ^.next:=pp^.next;
    pp^.next:=p;
    transfer(p^.self,ready^.self);
  ei(m)
END unlock;

PROCEDURE wait;
  VAR a: ADDRESS;
      m: BITSET;
BEGIN
  m:=di();
    ready^.work:=FALSE;
    ready:=ready^.next;
    IF ready#NIL THEN transfer(a,ready^.self)
    ELSE              transfer(a,ipted)
    END;
  ei(m)
END wait;

PROCEDURE send(VAR p: process);
  VAR n: PROCESS; m: BITSET; a: ADDRESS;
BEGIN
  m:=di();
    IF p.work THEN ei(m); RETURN END;

    p.work:=TRUE;
    IF (ready=NIL) OR (p.prio>ready^.prio) THEN
      p.next:=ready; ready:=SYSTEM.ADR(p);
      transfer(a,ready^.self)
    ELSE
      n:=ready;
      WHILE (n^.next#NIL) & (n^.next^.prio>=p.prio) DO n:=n^.next END;
      p.next:=n^.next;
      n^.next:=SYSTEM.ADR(p)
    END;
  ei(m)
END send;

PROCEDURE call(VAR p: process);
  VAR n  : PROCESS;
      a  : ADDRESS;
      m  : BITSET;
BEGIN
  m:=di();
    p.work:=TRUE;
    p.next:=ready;
    ready :=SYSTEM.ADR(p);
    ipted :=_self();
    transfer(a,ready^.self);
  ei(m)
END call;

PROCEDURE switch(VAR p: process; i: ADDRESS);
  VAR n   : PROCESS;
      a   : ADDRESS;
      m   : BITSET;
      last: PROCESS;
BEGIN
  m:=di();
    IF NOT p.work THEN
      last  :=ready;
      p.work:=TRUE;
      IF (ready=NIL) OR (p.prio>ready^.prio) THEN
        p.next:=ready; ready:=SYSTEM.ADR(p)
      ELSE
        n:=ready;
        WHILE (n^.next#NIL) & (n^.next^.prio>=p.prio) DO n:=n^.next END;
        p .next:=n^.next;
        n^.next:=SYSTEM.ADR(p)
      END;
      IF last=NIL THEN
        ipted:=i;
        transfer(a,ready^.self)
      ELSIF last^.self=i THEN
        transfer(a,ready^.self)
      ELSE
        transfer(a,i)
      END
    ELSE
      transfer(a,i)
    END;
  ei(m)
END switch;

PROCEDURE self(): SYSTEM.ADDRESS;
BEGIN RETURN _self()
END self;

PROCEDURE tie(VAR h: QUEUE; n: QUEUE);
BEGIN
  IF h=NIL THEN
    h:=n; h^.f:=h; h^.b:=h
  ELSE
    n^.b:=h^.b; n^.b^.f:=n;
    n^.f:=h   ; n^.f^.b:=n
  END
END tie;

PROCEDURE untie(VAR h: QUEUE);
BEGIN
  IF   h^.f=h THEN h:=NIL
  ELSE
    h^.f^.b:=h^.b; h^.b^.f:=h^.f; h:=h^.f
  END
END untie;

PROCEDURE untien(VAR h: QUEUE; n: QUEUE);
BEGIN
  IF h=NIL THEN RETURN END;
  IF n=h THEN h:=h^.f END;
  IF n=h THEN h:=NIL
  ELSE
    n^.f^.b:=n^.b; n^.b^.f:=n^.f
  END
END untien;

VAR p: os.process;

BEGIN
  CLOCK:=0;
  h    :=NIL;
  ready:=NIL;
  ipted:=NIL;
  os.ini_signal(tsg  ,{}      ,0);
  os.ini_signal(mlock,os.break,1);
  IF os.make_process(to0,timer0,256)#err.ok THEN HALT(1) END;
  IF NOT new(to,timer,256,toutprio)  THEN HALT(1) END;
  os.lock;

    p:=os.active();
    marea:=p^.task^.area;

    --IF NOT os.new(marea) THEN os.unlock; HALT(1) END;
    IF os.final(os.self(),stop)#err.ok THEN
    --  os.delete(marea);
    (*  os.unlock;
      HALT(1) *)
    END;
  os.unlock;
  IF os.insert_action(clock)#err.ok THEN HALT(1) END;
  os.start(to0)
END netKernel.
