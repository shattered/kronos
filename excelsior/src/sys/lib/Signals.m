IMPLEMENTATION MODULE Signals; (* Ned 23-Jan-90. (c) KRONOS *)

IMPORT  cod: defCodes;
IMPORT  os : osKernel;

PROCEDURE di(): BITSET;  CODE cod.getm cod.copt 3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

TYPE
  SIGNAL = os.signal_ptr;
  MUTEX  = os.mutex_ptr;

PROCEDURE new_signal(VAR s: SIGNAL; no: INTEGER; VAR done: BOOLEAN);
  VAR x: SIGNAL; task: os.task_ptr;
BEGIN
  ASSERT(no>=0);
  task:=os.self();
  os.ALLOCATE(task^.area,x,SIZE(x^));
  done:=(x#NIL);
  IF done THEN os.ini_signal(x^,{},no) END;
  s:=x;
END new_signal;

PROCEDURE rem_signal(VAR  s: SIGNAL);
  VAR task: os.task_ptr; m: BITSET;
BEGIN
  m:=di();
   ASSERT(s^.queue=os.null);
   s^.magic:=0;
  ei(m);
  task:=os.self();
  os.DEALLOCATE(task^.area,s,SIZE(s^));
END rem_signal;

PROCEDURE new_mutex(VAR m: MUTEX; VAR done: BOOLEAN);
  VAR x: MUTEX; task: os.task_ptr;
BEGIN
  task:=os.self();
  os.ALLOCATE(task^.area,x,SIZE(x^));
  done:=(x#NIL);
  IF done THEN os.ini_mutex(x^) END;
  m:=x;
END new_mutex;

PROCEDURE rem_mutex(VAR m: MUTEX);
  VAR task: os.task_ptr; mask: BITSET;
BEGIN
  mask:=di();
    ASSERT(m^.prs=NIL);
    m^.magic:=0;
  ei(mask);
  task:=os.self();
  os.DEALLOCATE(task^.area,m,SIZE(m^));
END rem_mutex;

---------------------------------------------------------------

PROCEDURE send(s: SIGNAL);
BEGIN
  os.send(s^)
END send;

PROCEDURE wait(s: SIGNAL);
BEGIN
  os.wait(s^)
END wait;

PROCEDURE clear(s: SIGNAL);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF s^.cou>0 THEN s^.cou:=0 END;
  ei(m);
END clear;

PROCEDURE broadcast(s: SIGNAL);
BEGIN
  os.broadcast(s^);
END broadcast;

PROCEDURE signal(s: SIGNAL; n: INTEGER);
BEGIN
  os.signal(s^,n)
END signal;

PROCEDURE delay_wait(VAR cause: INTEGER; milisec: INTEGER; s: SIGNAL);
BEGIN
  cause:=os.wait_del((milisec+os.tick-1) DIV os.tick,s^)
END delay_wait;

PROCEDURE alts(VAR n: INTEGER; milisec: INTEGER; ss: ARRAY OF SIGNAL);
BEGIN
  n:=os.alt_wait((milisec+os.tick-1) DIV os.tick,ss)
END alts;

PROCEDURE alt(VAR s: SIGNAL; milisec: INTEGER; SEQ ss: SIGNAL);
  VAR n: INTEGER;
BEGIN
  n:=os.alt_wait((milisec+os.tick-1) DIV os.tick,ss);
  IF n<0 THEN s:=null ELSE s:=ss[n] END
END alt;

PROCEDURE awaited(s: SIGNAL): BOOLEAN;
BEGIN RETURN s^.queue#os.null END awaited;

PROCEDURE sendings(s: SIGNAL): INTEGER;
BEGIN
  RETURN s^.cou
END sendings;

PROCEDURE acquire(m: MUTEX);
BEGIN
  os.acquire(m^);
END acquire;

PROCEDURE delay_acquire(m: MUTEX; milisec: INTEGER; VAR done: BOOLEAN);
BEGIN
  done:=os.acquire_del((milisec+os.tick-1) DIV os.tick,m^)
END delay_acquire;

PROCEDURE release(m: MUTEX);
BEGIN
  os.release(m^)
END release;

BEGIN
  null:=NIL
END Signals.
