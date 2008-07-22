IMPLEMENTATION MODULE Threads; (*$N+ Ned 20-Oct-89. (c) KRONOS *)

(*$U+*)

IMPORT  sys: SYSTEM;
IMPORT  mcd: defCodes;
IMPORT   os: osKernel;
IMPORT  syn: Signals;
IMPORT  env: tskEnv;

WITH STORAGE (NEW    : env.allocate;
              DISPOSE: env.deallocate;
              RESIZE : env.reallocate);

CONST
  ok       = 0;
  MAGIC    = os.MAGIC;
  ill_parm = 4Fh;

TYPE THREAD = os.process;

VAR task: os.task_ptr;

PROCEDURE _fork(VAR new: THREAD;
                   proc: Forkee;
                  stack: INTEGER;
               VAR done: BOOLEAN;
               SEQ args: sys.WORD);
  VAR pp: os.PROCESS; a: sys.ADDRESS;
BEGIN
  done:=FALSE;
  IF os.make_process(new,PROC(proc),stack)#ok THEN RETURN END;
  IF HIGH(args)<0 THEN NEW(new^.parm)
  ELSE
    NEW(new^.parm,SIZE(args));
    IF new^.parm^.ADR=NIL THEN RETURN END;
(*$<X+*)
    new^.parm:=args;
(*$>*)
  END;
  pp:=new^.pp;
  a:=pp^.S-1;
  a^:=new^.parm^.HIGH;    INC(a);
  a^:=new^.parm^.ADR;     INC(a);
  a^:=2;                  INC(a);
  pp^.S:=a;
  done:=TRUE;
END _fork;

PROCEDURE fork (VAR new: THREAD;
                   proc: Forkee;
                  stack: INTEGER;
               VAR done: BOOLEAN;
               SEQ args: sys.WORD);
BEGIN
  os.acquire(task^.lock);
    _fork(new,proc,stack,done,args);
    IF done THEN os.start(new) END;
  os.release(task^.lock);
END fork;

PROCEDURE xfork(VAR new: THREAD;
                   proc: Forkee;
                  stack: INTEGER;
                   halt: syn.SIGNAL;
               VAR done: BOOLEAN;
               SEQ args: sys.WORD);
BEGIN
  os.acquire(task^.lock);
    _fork(new,proc,stack,done,args);
    IF done THEN
      new^.halt:=os.signal_ptr(halt);
      os.start(new);
    END;
  os.release(task^.lock);
END xfork;

PROCEDURE suspend(thread: THREAD; milisec: INTEGER);
BEGIN
  IF milisec<=0 THEN
    os.suspend(thread,milisec);
  ELSE
    os.suspend(thread,(milisec+os.tick-1) DIV os.tick);
  END;
END suspend;

PROCEDURE resume(thread: THREAD);
BEGIN
  os.resume(thread);
END resume;

PROCEDURE delay(milisec: INTEGER);
BEGIN
  IF milisec>0 THEN
    os.delay((milisec+os.tick-1) DIV os.tick);
  END;
END delay;

PROCEDURE abort(thread: THREAD; how: INTEGER);
BEGIN
  IF    how=0 THEN
    os.abort(thread,FALSE);
    os.stop(thread,FALSE);
  ELSIF how=1 THEN
    os.stop(thread,FALSE);
  ELSE ASSERT(FALSE,ill_parm);
  END;
END abort;

----------------------------------------------------------------

PROCEDURE self(): THREAD;
BEGIN
  RETURN os.active();
END self;

PROCEDURE get_prio(thread: THREAD; VAR prio: INTEGER);
BEGIN
  ASSERT((thread#NIL) & (thread^.magic=MAGIC),ill_parm);
  prio:=thread^.prio;
END get_prio;

PROCEDURE set_prio(thread: THREAD;     prio: INTEGER);
BEGIN
  ASSERT((thread#NIL) & (thread^.magic=MAGIC),ill_parm);
--os.set_prio(thread,prio);
END set_prio;

----------------------------------------------------------------

PROCEDURE cause(thread: THREAD; VAR n: INTEGER);
BEGIN
  ASSERT((thread#NIL) & (thread^.magic=MAGIC),ill_parm);
  n:=thread^.pp^.T
END cause;

PROCEDURE status(thread: THREAD; VAR n: INTEGER);
BEGIN
  ASSERT((thread#NIL) & (thread^.magic=MAGIC),ill_parm);
  CASE thread^.status OF
    |os._ready    : n:=ready
    |os._suspended: n:=suspended
    |os._wait     : n:=blocked
    |os._brk_wait : n:=blocked
    |os._aborted  : n:=aborted
  ELSE              n:=invalid
  END;
END status;

PROCEDURE history(thread: THREAD; VAR s: ARRAY OF CHAR);
BEGIN
  ASSERT((thread#NIL) & (thread^.magic=MAGIC),ill_parm);
  os.extract(s,thread^.pp);
END history;

PROCEDURE rem_thread(VAR thread: THREAD; VAR done: BOOLEAN);
BEGIN
  ASSERT((thread#NIL) & (thread^.magic=MAGIC),ill_parm);
  DISPOSE(thread^.parm);
  done:=os.rem_process(thread)=ok;
END rem_thread;

BEGIN
  null:=NIL;
  task:=os.self();
END Threads.
