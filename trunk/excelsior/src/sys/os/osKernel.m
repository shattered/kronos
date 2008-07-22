IMPLEMENTATION MODULE osKernel[1]; (* Ned 27-Apr-90. (c) KRONOS *)
                                   (* Leo 14-May-90. (c) KRONOS *)
                                   (* Leo 22-Feb-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  def: defTasks;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  low: lowLevel;
FROM SYSTEM  IMPORT ADR;

IMPORT  bug: deBug;

(*$U+*)

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

VAR
  sMAGIC: INTEGER;
  mMAGIC: INTEGER;
  aMAGIC: INTEGER;

----------------------  CODE PROCEDUREs  ----------------------
                      -------------------

PROCEDURE _activ(): PROCESS; CODE cod.li0 cod.lsw0 END _activ;

PROCEDURE getm(): BITSET;  CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;

PROCEDURE di; CODE cod.getm cod.copt cod.slw 3 cod.li3 cod.bic cod.setm END di;
PROCEDURE ei; CODE cod.llw 3 cod.setm END ei;

PROCEDURE tr(VAR var: WORD): BOOLEAN; CODE cod.tr END tr;

PROCEDURE trap(n: INTEGER); CODE cod.trap END trap;

PROCEDURE transfer(VAR from,to: PROCESS); CODE cod.tra END transfer;

PROCEDURE quit(n: WORD); CODE cod.quit cod.drop END quit;

PROCEDURE check_p_stack(n: INTEGER);
CODE cod.copt cod.alloc cod.drop cod.decs END check_p_stack;

PROCEDURE assert(a,b: WORD);         -- равенство слов
CODE cod.copt cod.chk cod.drop END assert;

-----------------  HARDWARE PROCESSES SUPPORT  ----------------
                 ------------------------------

VAR WS: BOOLEAN;  -- WorkStation instruction set

PROCEDURE ws(): BOOLEAN;
CODE cod.getm cod.setm cod.lib 30 cod.llw 2 cod.in cod.not END ws;

PROCEDURE Halt0;
BEGIN
  IF WS THEN setm({31}) END;
  trap(47h);
END Halt0;

PROCEDURE new_process(proc: PROC;
                       wsp: ADDRESS;
                      size: INTEGER;
                   VAR prs: PROCESS);

  TYPE proc_link = POINTER TO
                     RECORD
                       G : ADDRESS;
                       L : ADDRESS;
                       PC: BITSET;
                       M : BITSET;
                     END;

  CONST ExternalCall = 31;  MaskChanged  = 30;

  PROCEDURE procEntry(proc: WORD): ADDRESS;
  CODE cod.lib 0FFh 8 cod.ror cod.bic END procEntry;

  PROCEDURE procNo(proc: WORD): INTEGER;
  CODE 8 cod.rol cod.lib 0FFh cod.and END procNo;

  VAR my: PROCESS; adr: ADDRESS; l: proc_link;
BEGIN
  prs:=wsp;
  IF (wsp=NIL) OR (size<=SIZE(HW_descriptor)+16+5) THEN prs:=NIL; RETURN END;
  adr:=procEntry(proc);
  prs^.G :=adr^;
  prs^.L :=wsp+SIZE(HW_descriptor);
  adr:=INTEGER(prs^.G^)+procNo(proc); (* F+proc_no *)
  prs^.PC:=adr^;
  prs^.M :=getm();
  prs^.S :=prs^.L+5;
    adr:=prs^.L+4; adr^:=0; (* Expression stack size *)
  prs^.H :=wsp+size-1;
  prs^.T :=0;
  prs^.XX:=NIL;
  my:=_activ();
  prs^.prs:=my^.prs;
  l:=prs^.L;
  adr:=procEntry(Halt0);
  l^.G :=adr^;
  l^.L :=NIL;
  adr:=INTEGER(l^.G^)+procNo(Halt0);
  l^.PC:=BITSET(adr^)+{ExternalCall};
  IF NOT WS THEN
    INCL(l^.PC,MaskChanged);
    l^.M:={31};
  END
END new_process;

PROCEDURE extract(VAR his: ARRAY OF CHAR; p: PROCESS);

  PROCEDURE find_proc(VAL tab: ARRAY OF INTEGER; pc: INTEGER): INTEGER;
    VAR i,npc,cpc: INTEGER;
         no,first: INTEGER;
  BEGIN
    i:=0; no:=0; npc:=0;
    first:=tab[0];
    REPEAT
      IF i=256 THEN RETURN -1 END;
      cpc:=tab[i];
      IF first > cpc THEN first:=cpc END;
      IF (pc >= cpc) & (cpc >= npc) THEN npc:=cpc; no:=i END;
      i:=i+1;
    UNTIL i*4 = first;
    RETURN no
  END find_proc;

  TYPE
    glo_ptr = POINTER TO
                RECORD
                  tab: POINTER TO ARRAY [0..255] OF INTEGER;
                  stp: POINTER TO ARRAY [0..31] OF CHAR;
                END;

    proc_ptr = POINTER TO proc_rec;

    proc_rec = RECORD
                 glo : glo_ptr;
                 pre : proc_ptr;
                 pc  : BITSET;
                 mask: BITSET;
               END;

  CONST external_call_bit = 31;

  VAR pos: INTEGER;

  PROCEDURE inv(a: ADDRESS): BOOLEAN;
  BEGIN
    RETURN (a<=0) OR (a>=NIL)
  END inv;

  PROCEDURE inv_g(g: glo_ptr): BOOLEAN;
  BEGIN
    IF inv(g) OR inv(g^.tab) OR inv(g^.stp) THEN
      str.image(his,pos,'illegal global links\n'); RETURN TRUE
    END;
    RETURN FALSE
  END inv_g;

  PROCEDURE get;
    VAR l: proc_ptr; g: glo_ptr; pc,no: INTEGER;
  BEGIN
    g:=p^.G;  l:=p^.L;  pc:=p^.PC;
    str.image(his,pos,"#process: %$8h cause: [%$#2h]\n",p,p^.T);
    IF (l=NIL) OR inv_g(g) THEN RETURN END;
    str.image(his,pos,'#%s:',g^.stp^);
    LOOP
      IF inv(l) THEN
        str.image(his,pos,"\nillegal register L=%8h",l); EXIT
      END;
      no:=find_proc(g^.tab^,pc);
      IF no<0 THEN str.image(his,pos,"\ninvalid P-tab! "); EXIT END;
      str.image(his,pos," %0#h [%0#h]",no,pc-g^.tab^[no]);
      IF l^.pre=NIL THEN EXIT END;
      IF external_call_bit IN l^.pc THEN
        g:=l^.glo;
        IF inv_g(g) THEN EXIT END;
        str.image(his,pos," { \n%s:",g^.stp^);
      END;
      pc:=INTEGER(l^.pc*{0..15});
      l:=l^.pre;
    END;
    str.image(his,pos,"\n");
  END get;

  VAR prio: INTEGER; i: process;

BEGIN
  pos:=0;
  IF inv(p) THEN str.image(his,pos,"#ILLEGAL process: %8h\n",p); RETURN END;
  i:=active();
  prio:=i^.prio;
  set_prio(i,-1);
    get;
  set_prio(i,prio);
END extract;

-------------------------  READY RING  ------------------------
                         --------------

CONST              -- признаки процесса --
  abort_tag   = 0; -- должен быть уничтожен
  suspend_tag = 1; -- должен быть задержан

TYPE
  queue_ptr     = POINTER TO queue_rec;
  QUEUE         = queue_ptr;
  queue_ref     = POINTER TO queue_ptr;
  queue_rec     = RECORD
                    fwd : queue_ptr;
                    bck : queue_ptr;
                    next: queue_ptr;
                    sig : queue_ref;
                    prs : process;
                    info: INTEGER;      -- for signal number, delay, etc
                  END;

VAR
  ready: queue_ptr;      -- кольцо готовых
  IDLE : BOOLEAN;
  idler: PROCESS;
  dummy: PROCESS;

VAR
  ini_process: OS_descriptor;
  ini_queue  : queue_rec;
  ini_node   : node_rec;

PROCEDURE prs(): process; CODE cod.activ cod.lsw8 END prs;

PROCEDURE queue(p: process): queue_ptr;
CODE cod.lsa SIZE(OS_descriptor)
END queue;


PROCEDURE IdleProc;
  PROCEDURE idle; CODE cod.idle END idle;
  VAR IDLE_MASK: BITSET;
BEGIN
  IF low.cpu=5 THEN IDLE_MASK:={1} ELSE IDLE_MASK:={0,1} END;
  LOOP
    IDLE:=TRUE;
    assert(ready,NIL);
    setm(IDLE_MASK);
    idle;
    IF ready#NIL THEN transfer(idler,ready^.prs^.pp) END;
  END
END IdleProc;

PROCEDURE active(): process;
  VAR p: PROCESS;
BEGIN p:=_activ(); RETURN p^.prs END active;

PROCEDURE start(p: process);
BEGIN
  di;
  IF p^.status=_new THEN resume(p) END;
  ei
END start;

---------------------------  QUEUEs  --------------------------
                           ----------

CONST W_q_wait = 4+3;

PROCEDURE q_wait(VAR q: queue_ptr; status: INTEGER);
  VAR n: queue_ptr;
BEGIN
  n:=ready;
  n^.prs^.pp:=_activ();
  n^.prs^.status:=status;
  IF ready^.fwd=ready THEN
    ready:=NIL;
  ELSE
    ready:=ready^.bck;
    n^.bck^.fwd:=n^.fwd;
    n^.fwd^.bck:=n^.bck;
  END;
  IF q=NIL THEN q:=n; q^.bck:=q; q^.fwd:=q;
  ELSE
    n^.fwd:=q^.fwd;
    n^.bck:=q;
    n^.bck^.fwd:=n;
    n^.fwd^.bck:=n;
  END;
  n^.sig:=REF(q,queue_ref);
  n^.info:=0; n^.next:=n;
END q_wait;

CONST W_q_send = 4+4;

PROCEDURE q_send(x: queue_ptr; sendup: BOOLEAN);  -- x любой узел
  VAR n,head: queue_ptr; p: process;
BEGIN
  p:=x^.prs;
  n:=queue(p);   -- self node of p
  n^.info:=x^.info;
  head:=n;
  REPEAT
    IF n=n^.fwd THEN n^.sig^:=NIL;
    ELSE
      IF n=n^.sig^ THEN n^.sig^:=n^.bck END;
      n^.bck^.fwd:=n^.fwd;
      n^.fwd^.bck:=n^.bck;
    END;
    n:=n^.next;
  UNTIL n=head;
  p^.status:=_ready;
  n:=head;
  IF ready=NIL THEN
    IF tr(IDLE) THEN INC(idler^.PC); idler^.M:=idler^.M-{0,1} END;
    ready:=n; n^.bck:=n; n^.fwd:=n;
  ELSIF sendup OR (p^.prio<0) THEN
    n^.bck:=ready^.bck;
    n^.fwd:=ready;
    n^.fwd^.bck:=n;
    n^.bck^.fwd:=n;
  ELSE
    n^.fwd:=ready^.fwd;
    n^.bck:=ready;
    n^.fwd^.bck:=n;
    n^.bck^.fwd:=n;
  END;
  n^.sig:=REF(ready,queue_ref);
  n^.next:=n
END q_send;

---------------------------  ABORTs  ---------------------------
                           ----------

CONST W_finish = 4 + 6 + W_q_send;

PROCEDURE finish(p: process);
  VAR n,head: queue_ptr; l: mutex_ptr; x: process;
BEGIN
  p^.status:=_aborted;
-- free mutex
  l:=p^.mutex;
  WHILE l#NIL DO
    IF l^.queue=NIL THEN
      l^.prs:=NIL; l^.cou:=0; l^.next:=NIL;
    ELSE
      x:=l^.queue^.prs;  l^.prs:=x;
      l^.next:=x^.mutex; x^.mutex:=l;
      q_send(l^.queue,FALSE)
    END;
    l:=l^.next;
  END;
  p^.mutex:=NIL;
-- untie from all queues
  n:=queue(p);
  IF n^.sig#NIL THEN
    head:=n;
    REPEAT
      IF n=n^.fwd THEN n^.sig^:=NIL;
      ELSE
        IF n=n^.sig^ THEN n^.sig^:=n^.bck END;
        n^.bck^.fwd:=n^.fwd;
        n^.fwd^.bck:=n^.bck;
      END;
      n:=n^.next;
    UNTIL n=head;
    n^.sig:=NIL;
  END;
  IF p^.halt#NIL THEN send(p^.halt^) END
END finish;

PROCEDURE wait_halt(p: process);
  VAR halt: signal_rec; save: signal_ptr;
BEGIN
  save:=p^.halt;
  ini_signal(halt,{},0);
  p^.halt:=REF(halt,signal_ptr);
  wait(halt);
  assert(p^.status,_aborted);
  p^.halt:=save;
  IF save#NIL THEN send(save^) END;
END wait_halt;

PROCEDURE stop(p: process; wait: BOOLEAN);
  VAR n: queue_ptr;
BEGIN
  check_p_stack(W_finish+8);
  di;
    CASE p^.status OF
      |_suspended,_new:
                  finish(p);
      |_aborted : (* nothing *)
      |_brk_wait: INCL(p^.tags,abort_tag);
                  n:=queue(p); n^.info:=1;
                  q_send(n,FALSE);
      |_wait    : IF p^.guard>0 THEN INCL(p^.tags,abort_tag);
                  ELSE p^.pp^.T:=50h; finish(p);
                  END;
      |_ready   : IF (p^.guard>0) OR (p=ready^.prs) THEN
                    INCL(p^.tags,abort_tag);
                  ELSE p^.pp^.T:=50h; finish(p);
                  END;
    END;
    IF wait & (p^.status#_aborted) THEN wait_halt(p) END;
  ei;
END stop;

---------------------------  DELAYs  --------------------------
                           ----------

VAR timeq: queue_ptr;

CONST W_tie_to_timeq = 4+3;

PROCEDURE tie_to_timeq(r_time: INTEGER; n: queue_ptr);
  VAR l: queue_ptr;
BEGIN
  n^.info:=r_time;
  IF timeq=NIL THEN timeq:=n; timeq^.bck:=n; timeq^.fwd:=n;
  ELSE l:=timeq;
    LOOP
      IF n^.info<l^.info THEN
        IF l=timeq THEN timeq:=n END;
        EXIT
      END;
      l:=l^.bck;
      IF l=timeq THEN EXIT END;
    END;
    n^.fwd:=l^.fwd;
    n^.bck:=l;
    n^.bck^.fwd:=n;
    n^.fwd^.bck:=n;
  END;
  n^.sig:=REF(timeq,queue_ref);
END tie_to_timeq;

PROCEDURE delay(ticks: INTEGER);
  VAR n: queue_ptr;
BEGIN
  IF ticks<=0 THEN
    di;
    IF (ticks=0) & (ready#NIL) & (ready^.bck#ready) THEN
      ready^.prs^.pp:=_activ();
      ready:=ready^.bck;
      transfer(dummy,ready^.prs^.pp)
    END;
    ei;
    RETURN
  END;
  check_p_stack(W_tie_to_timeq+8);
  di;
    n:=ready;
    n^.prs^.pp:=_activ();
    n^.prs^.status:=_suspended;
    IF ready^.fwd=ready THEN ready:=NIL;
    ELSE ready:=ready^.bck;
      n^.bck^.fwd:=n^.fwd;
      n^.fwd^.bck:=n^.bck;
    END;
    tie_to_timeq(timer+ticks,n);
    n^.next:=n;
    IF ready=NIL THEN transfer(dummy,idler)
    ELSE              transfer(dummy,ready^.prs^.pp)
    END;
  ei;
END delay;

CONST W_suspend = 4+2+W_tie_to_timeq;

PROCEDURE _suspend(p: process);
  VAR n: queue_ptr;
BEGIN
  IF (p^.r_time>0) & (p^.r_time<=timer) THEN RETURN END;
  p^.status:=_suspended;
  EXCL(p^.tags,suspend_tag);
  n:=queue(p);
  IF n^.fwd=n THEN ready:=NIL
  ELSE
    IF n=ready THEN ready:=ready^.bck END;
    n^.bck^.fwd:=n^.fwd;
    n^.fwd^.bck:=n^.bck;
  END;
  IF p^.r_time<0 THEN n^.sig:=NIL;
  ELSE tie_to_timeq(p^.r_time,n);
    n^.next:=n;
  END;
END _suspend;

PROCEDURE suspend(p: process; ticks: INTEGER);
  VAR myself: BOOLEAN; n: queue_ptr;
BEGIN
  check_p_stack(W_suspend+8);
  di;
    IF ticks>=0 THEN p^.r_time:=ticks+timer ELSE p^.r_time:=-1 END;
    myself:=(p=prs());
    IF (p^.status=_ready) & (p^.guard=0) OR myself THEN
      _suspend(p);
      IF myself THEN
        p^.pp:=_activ();
        IF ready=NIL THEN transfer(dummy,idler)
        ELSE              transfer(dummy,ready^.prs^.pp)
        END;
      END;
    ELSIF p^.status=_new THEN
      IF ticks>=0 THEN
        p^.status:=_suspended;
        n:=queue(p);
        tie_to_timeq(p^.r_time,n);
        n^.next:=n;
      END;
    ELSE INCL(p^.tags,suspend_tag);
    END;
  ei;
END suspend;

PROCEDURE resume(p: process);
  VAR n: queue_ptr;
BEGIN
--check_p_stack(0);
  di;
  EXCL(p^.tags,suspend_tag);
  p^.r_time:=-1;
  IF p^.status IN {_new,_suspended} THEN
    p^.status:=_ready;
    n:=queue(p);
    IF n^.sig#NIL THEN -- process in time queue
      assert(n^.sig,REF(timeq,queue_ref));
      IF n=n^.fwd THEN n^.sig^:=NIL;
      ELSE
        IF n=n^.sig^ THEN n^.sig^:=n^.bck END;
        n^.bck^.fwd:=n^.fwd;
        n^.fwd^.bck:=n^.bck;
      END;
    END;
    IF ready=NIL THEN
      IF tr(IDLE) THEN INC(idler^.PC); idler^.M:=idler^.M-{0,1} END;
      ready:=n; n^.fwd:=ready; n^.bck:=ready;
    ELSE
      n^.fwd:=ready^.fwd;
      n^.bck:=ready;
      n^.bck^.fwd:=n;
      n^.fwd^.bck:=n;
    END;
    n^.next:=n;
    n^.sig:=REF(ready,queue_ref);
    n^.prs:=p
  END;
  ei;
END resume;

----------------------------------------------------------------

PROCEDURE lock;
  VAR p: process;
BEGIN
  p:=prs(); INC(p^.guard)
END lock;

PROCEDURE unlock;
  CONST marks = {abort_tag,suspend_tag};
  VAR p: process;
BEGIN
  p:=prs();
  IF p^.guard<=0 THEN RETURN END;
  DEC(p^.guard);
  IF (p^.guard=0) & (p^.tags*marks#{}) THEN
    IF abort_tag IN p^.tags THEN trap(50h)
    ELSE
      check_p_stack(W_suspend+8);
      di;
        _suspend(p);
        IF ready=NIL THEN transfer(dummy,idler)
        ELSE              transfer(dummy,ready^.prs^.pp)
        END;
      ei
    END
  END
END unlock;

PROCEDURE set_prio(p: process; prio: INTEGER);
  VAR prev: INTEGER;
BEGIN
  di;
   prev:=p^.prio;
   p^.prio:=prio;
   IF (prev<0) & (prio>=0) THEN
     IF (ready#NIL) & (ready^.prs=p) & (ready^.fwd#ready) THEN
       ready^.prs^.pp:=_activ();
       ready:=ready^.bck;
       transfer(dummy,ready^.prs^.pp)
     END
   END;
  ei
END set_prio;

--------------------------  SIGNALs  --------------------------
                          -----------

PROCEDURE wait(VAR s: signal_rec);
BEGIN
  assert(s.magic,sMAGIC);
  check_p_stack(W_q_wait+8);
  di;
    IF s.cou>0 THEN DEC(s.cou);
    ELSE
      q_wait(s.queue,_wait);
      IF ready=NIL THEN transfer(dummy,idler)
      ELSE              transfer(dummy,ready^.prs^.pp)
      END;
    END;
    IF s.tags*guard#{} THEN INC(ready^.prs^.guard) END;
  ei;
END wait;

PROCEDURE wait_del(delay: INTEGER; VAR s: signal_rec): INTEGER;
  VAR n: queue_ptr; delay_node: queue_rec; res: INTEGER;
BEGIN
  assert(s.magic,sMAGIC);
  IF W_q_wait>W_tie_to_timeq THEN check_p_stack(W_q_wait+8);
  ELSE                            check_p_stack(W_tie_to_timeq+8);
  END;
  di;
    IF s.cou>0 THEN
      DEC(s.cou); res:=0;
    ELSE
      n:=ready;
      IF s.tags*break#{} THEN
        IF abort_tag IN n^.prs^.tags THEN ei; RETURN 1 END;
        q_wait(s.queue,_brk_wait);
      ELSE
        q_wait(s.queue,_wait);
      END;
      IF delay>0 THEN
        tie_to_timeq(delay+timer,REF(delay_node,queue_ptr));
        delay_node.prs:=n^.prs;
        delay_node.next:=n; n^.next:=REF(delay_node,queue_ptr);
      END;
      IF ready=NIL THEN transfer(dummy,idler)
      ELSE              transfer(dummy,ready^.prs^.pp)
      END;
      res:=ready^.info;
    END;
    IF s.tags*guard#{} THEN INC(ready^.prs^.guard) END;
  ei;
  RETURN res
END wait_del;

PROCEDURE alt_wait(delay: INTEGER; VAL s: ARRAY OF signal_ptr): INTEGER;

  PROCEDURE alloc(n: INTEGER): ADDRESS; CODE cod.alloc END alloc;

  VAR l,n: queue_ptr; p: process; i,co: INTEGER; o: signal_ptr;
      wsp: ADDRESS;
BEGIN
  check_p_stack(W_tie_to_timeq+8);
  di;
  co:=0;
  FOR i:=0 TO HIGH(s) DO
    o:=s[i];
    IF o#NIL THEN INC(co);
      assert(o^.magic,sMAGIC);
      IF o^.cou>0 THEN DEC(o^.cou); ei; RETURN i END;
    END;                            --
  END;

  p:=ready^.prs; n:=queue(p);

  IF (co=0) & (delay<=0) THEN ei; RETURN -1 END;
  IF delay<=0 THEN i:=co-1    --
  ELSE             i:=co
  END; (* i = wsp size *)

  IF i>0 THEN wsp:=alloc(SIZE(l^)*i); l:=wsp ELSE l:=n END;
  n^.next:=l;
  WHILE i>1 DO
    l^.next:=ADDRESS(l)+SIZE(l^);
    l:=l^.next;
    DEC(i);
  END;
  l^.next:=n;

  p^.pp:=_activ();
  p^.status:=_wait;
  IF ready^.fwd=ready THEN ready:=NIL;
  ELSE
    ready:=ready^.bck;
    n^.bck^.fwd:=n^.fwd;
    n^.fwd^.bck:=n^.bck;
  END;

  l:=n;
  IF delay>0 THEN tie_to_timeq(delay+timer,l); l:=l^.next END;
  i:=0;
  WHILE co>0 DO
    o:=s[i];
    IF o#NIL THEN
      l^.prs:=p; l^.info:=i;
      IF o^.queue=NIL THEN
        o^.queue:=l;
        o^.queue^.bck:=o^.queue;
        o^.queue^.fwd:=o^.queue;
      ELSE
        l^.fwd:=o^.queue^.fwd;
        l^.bck:=o^.queue;
        l^.bck^.fwd:=l;
        l^.fwd^.bck:=l;
      END;
      l^.sig:=REF(o^.queue,queue_ref);
      l:=l^.next;
      DEC(co);
    END;
    INC(i);
  END;
  IF ready=NIL THEN transfer(dummy,idler)
  ELSE              transfer(dummy,ready^.prs^.pp)
  END;
  i:=ready^.info;
  ei;
  RETURN i
END alt_wait;

PROCEDURE send(VAR s: signal_rec);
BEGIN
  assert(s.magic,sMAGIC);
  check_p_stack(W_q_send+8);
  di;
    IF s.queue#NIL THEN
      q_send(s.queue,s.tags*sendup#{});
    ELSE
      IF s.cou<0 THEN s.cou:=0 END;
      INC(s.cou)
    END;
    IF s.tags*guard#{} THEN unlock END;
  ei
END send;

PROCEDURE signal(VAR s: signal_rec; no: INTEGER);
BEGIN
  IF no<=0 THEN RETURN END;
  assert(s.magic,sMAGIC);
  di;
    WHILE (no>0) & (s.queue#NIL) DO
      q_send(s.queue,s.tags*sendup#{}); DEC(no)
    END;
    IF s.cou<0 THEN s.cou:=0 END;
    INC(s.cou,no);
  ei
END signal;

PROCEDURE broadcast(VAR s: signal_rec);
BEGIN
  assert(s.magic,sMAGIC);
  di;
    WHILE s.queue#NIL DO q_send(s.queue,s.tags*sendup#{}) END;
  ei;
END broadcast;

PROCEDURE ini_signal(VAR s: signal_rec; tags: BITSET; no: INTEGER);
BEGIN
  IF no<0 THEN no:=0 END;
  s.magic:=sMAGIC;
  s.queue:=NIL;
  s.cou  :=no;
  s.tags :=tags;
END ini_signal;

---------------------------  MUTEX  ---------------------------
                           ---------

PROCEDURE acquire(VAR m: mutex_rec);
  VAR p: process;
BEGIN
  assert(m.magic,mMAGIC);
  di;
    p:=prs();
    IF m.prs=p THEN INC(m.cou);
    ELSE
      IF m.prs=NIL THEN
        m.prs :=p;           m.cou:=1;
        m.next:=p^.mutex;    p^.mutex:=REF(m,mutex_ptr)
      ELSE
        q_wait(m.queue,_wait);
        IF ready=NIL THEN transfer(dummy,idler)
        ELSE              transfer(dummy,ready^.prs^.pp)
        END
      END
    END;
  ei;
END acquire;

PROCEDURE acquire_del(delay: INTEGER; VAR m: mutex_rec): BOOLEAN;
  VAR p: process; n: queue_ptr; delay_node: queue_rec; res: BOOLEAN;
BEGIN
  assert(m.magic,mMAGIC);
  di;
    p:=prs();
    IF m.prs=p THEN INC(m.cou); res:=TRUE;
    ELSE
      IF m.prs=NIL THEN
        m.prs :=p;           m.cou:=1;
        m.next:=p^.mutex;    p^.mutex:=REF(m,mutex_ptr);
        res:=TRUE;
      ELSIF delay=0 THEN res:=FALSE;
      ELSE
        n:=ready;
        q_wait(m.queue,_wait);
        IF delay>0 THEN
          tie_to_timeq(delay+timer,REF(delay_node,queue_ptr));
          delay_node.prs:=n^.prs;
          delay_node.next:=n; n^.next:=REF(delay_node,queue_ptr);
        END;
        IF ready=NIL THEN transfer(dummy,idler)
        ELSE              transfer(dummy,ready^.prs^.pp)
        END;
        res:=(ready^.info>=0);
      END
    END;
  ei;
  RETURN res
END acquire_del;

PROCEDURE release(VAR m: mutex_rec);
  VAR p: process;
BEGIN
  assert(m.magic,mMAGIC);
  di;
    p:=prs();
    assert(m.prs,p);
    IF m.cou=1 THEN
      assert(p^.mutex,REF(m,mutex_ptr));
      p^.mutex:=m.next; m.next:=NIL;
      IF m.queue=NIL THEN
        m.prs:=NIL; m.cou:=0
      ELSE
        p:=m.queue^.prs;  m.prs:=p;
        m.next:=p^.mutex; p^.mutex:=REF(m,mutex_ptr);
        q_send(m.queue,FALSE)
      END
    ELSE
      IF m.cou>1 THEN DEC(m.cou) ELSE m.cou:=1 END
    END;
  ei
END release;

PROCEDURE ini_mutex(VAR m: mutex_rec);
BEGIN
  m.magic:=mMAGIC;
  m.cou  :=0;
  m.prs  :=NIL;
  m.queue:=NIL;
  m.next :=NIL;
END ini_mutex;

---------------------------  CLOCK  ---------------------------
                           ---------

CONST
  frequency = 50;
  interval  = 2;

VAR clocker, ipted: PROCESS;

VAR Act: ARRAY [0..31] OF PROC;
  noAct: INTEGER;
 second: INTEGER;

PROCEDURE Clock;
  CONST marks = {abort_tag,suspend_tag};
  VAR p: process; changed: BOOLEAN; new: PROCESS; next,i: INTEGER;
    add: BITSET;    flush: INTEGER;
BEGIN
  IF (low.cpu=6) & ws() THEN add:={12} ELSE add:={} END;
  next:=timer+interval; second:=timer+frequency; flush:=second+2;
  LOOP
    INC(timer);
    IF timer>=second THEN
      INC(time); second:=timer+frequency;
      idler^.M:=idler^.M+add; flush:=timer+2
    END;
    IF timer>=flush  THEN
      idler^.M:=idler^.M-add;  flush:=second+2
    END;

    i:=noAct;
    WHILE i>0 DO DEC(i); Act[i]() END;

    WHILE (timeq#NIL) & (timeq^.info<=timer) DO
      timeq^.info:=-1; q_send(timeq,FALSE);
    END;

    IF timer<next    THEN
      new:=ipted;
    ELSE
      next:=timer+interval;
      IF (ready#NIL) & (ipted^.prs^.prio>=0) THEN
        IF ipted#idler THEN ipted^.prs^.pp:=ipted END;
        changed:=FALSE;
        LOOP
          IF ready=NIL THEN EXIT END;
          p:=ready^.prs;
          IF (p^.guard=0) & (p^.tags*marks#{}) THEN
            IF abort_tag IN p^.tags THEN finish(p);
            ELSE _suspend(p);
            END;
            changed:=TRUE
          ELSIF changed THEN EXIT
          ELSE
            ready:=ready^.bck;
            changed:=TRUE
          END
        END
      END;
      IF ready=NIL THEN new:=idler ELSE new:=ready^.prs^.pp END
    END;
    transfer(clocker,new)
  END
END Clock;

PROCEDURE insert_action(p: PROC): INTEGER;
  VAR done: BOOLEAN;
BEGIN
  di;
    done:=(noAct<=HIGH(Act));
    IF done THEN Act[noAct]:=p; INC(noAct) END;
  ei;
  IF done THEN RETURN err.ok ELSE RETURN err.not_enough END
END insert_action;

PROCEDURE remove_action(p: PROC);
  VAR i,j,k: INTEGER;
BEGIN
  di;
    i:=0;
    WHILE i<noAct DO
      IF Act[i]=p THEN DEC(noAct);
        FOR j:=i TO noAct-1 DO Act[j]:=Act[j+1] END;
      END;
      INC(i)
    END;
  ei;
END remove_action;


--------------------  INTERRUPTS SUBSYSTEM  -------------------
                    ------------------------

VAR       traps: ARRAY [0..03Fh] OF PROCESS;
        history: ARRAY [0..127] OF CHAR;
  system_driver: PROCESS;


PROCEDURE next_ready(ipted: PROCESS): PROCESS;
  VAR sch: process; n: INTEGER;
BEGIN
  n:=2;
  WHILE (n<=HIGH(traps)) & (traps[n]=NIL) DO INC(n) END;
  IF (n>HIGH(traps)) OR (traps[n]#ipted) THEN
    print("\nScheduler: processor fix interrupt error\n"); RETURN ipted
  END;
  traps[n]:=NIL;
  IF (n IN {2,3,7,0Bh}) OR (n=3Fh) THEN
    sch:=ipted^.prs;
    IF (sch=NIL) OR (sch^.guard>0) & (ipted^.T#47h) THEN
      print("\nosScheduler: illegal process %08h\n",ipted);
      extract(history,ipted); print("\n%s\n",history); quit(0F010h);
    END;
    sch^.pp:=ipted;
    IF (sch^.server#NIL) & (sch^.server#ipted) & (ipted^.T#47h) THEN
      IF ipted^.T=0 THEN ipted^.T:=n END;
      system_driver^.prs:=sch;
      transfer(system_driver,sch^.server);
      system_driver^.prs:=NIL;
      IF ipted^.T=0 THEN RETURN ipted END;
    END;
    finish(sch);
(*
IF ipted^.T#47h THEN
  print('kill process %h cause %h\n',ipted,ipted^.T);
  extract(h,ipted); print('%s\n',h);
--  quit(0);
END;
*)
    IF system_driver^.T#0 THEN quit(0F011h) END;
    IF ready=NIL THEN RETURN idler ELSE RETURN ready^.prs^.pp END;
  ELSE
    print("\nosScheduler: unexpected interrupt %02h\n",n); RETURN ipted
  END
END next_ready;

PROCEDURE interrupt;
  VAR ptr: ADDRESS; next: PROCESS;
BEGIN
  ptr:=1;
  LOOP
    next:=next_ready(ptr^);
    transfer(system_driver,next);
  END
END interrupt;

---------------------------------------------------------------

TYPE
  slice_ptr = POINTER TO slice_rec;
  slice_rec = RECORD
                size: INTEGER;
                next: slice_ptr;
              END;
  block_ptr = POINTER TO block_rec;
  block_rec = RECORD
                head: slice_ptr;
                size: INTEGER;
                free: INTEGER;
                next: block_ptr;
              END;
  area_rec  = RECORD
                magic: INTEGER;
                lock : mutex_rec;
                total: INTEGER;
                busy : block_ptr;
                work : block_ptr;
              END;
  AREA      = POINTER TO area_rec;

CONST
  slice_size = SIZE(slice_rec);
  block_size = SIZE(block_rec);
  req_size   = 256;

VAR
  mem_lock: mutex_rec;
  common  : slice_ptr;

----------------------------------------------------------------

PROCEDURE _alloc(VAR head: slice_ptr;
                      min: INTEGER;
                 VAR a   : ADDRESS;
                 VAR w   : INTEGER);
  VAR l,p,n: slice_ptr;
BEGIN
  l:=head; p:=NIL;
  WHILE (l#NIL) & (l^.size<w) DO p:=l; l:=l^.next END;
  IF l=NIL THEN a:=NIL; RETURN END;
  IF l^.size-w<min THEN
    w:=l^.size;
    IF p=NIL THEN head:=l^.next ELSE p^.next:=l^.next END;
  ELSE
    n:=ADDRESS(l)+w;
    n^.next:=l^.next; n^.size:=l^.size-w;
    IF p=NIL THEN head:=n ELSE p^.next:=n END;
    l^.size:=w;
  END;
  a:=l;
END _alloc;

PROCEDURE _dealloc(VAR head: slice_ptr; a: ADDRESS; w: INTEGER);
  VAR l,p,n: slice_ptr;
BEGIN n:=a; n^.size:=w;
  l:=head; p:=NIL;
  WHILE (l#NIL) & (l<a) DO p:=l; l:=l^.next END;
  n^.next:=l;
  IF    p=NIL THEN head:=n;
  ELSIF INTEGER(p)+p^.size=INTEGER(n) THEN INC(p^.size,w); n:=p
  ELSE  p^.next:=n
  END;
  IF l=NIL THEN RETURN END;
  IF INTEGER(n)+n^.size=INTEGER(l) THEN
    n^.next:=l^.next; INC(n^.size,l^.size)
  END;
END _dealloc;

----------------------------------------------------------------

PROCEDURE ini_area(VAR a: area_rec);
BEGIN
  a.magic:=aMAGIC;  ini_mutex(a.lock);
  a.busy :=NIL;
  a.work :=NIL;
  a.total:=0;
END ini_area;

PROCEDURE info(area: AREA; VAR total,free: INTEGER);
  VAR b: block_ptr;
BEGIN
  IF (area=NIL) OR (area^.magic#aMAGIC) THEN RETURN END;
  acquire(area^.lock);
    total:=area^.total;
    b:=area^.work; free:=0;
    WHILE b#NIL DO INC(free,b^.free); b:=b^.next END;
  release(area^.lock);
END info;

----------------------------------------------------------------

PROCEDURE vis(l: slice_ptr);
BEGIN
  WHILE l#NIL DO
    print('    %08h %05h (%5d)\n',l,l^.size,l^.size);
    l:=l^.next;
  END;
END vis;

PROCEDURE vis_common;
BEGIN
  acquire(mem_lock);
    vis(common);
  release(mem_lock);
END vis_common;

PROCEDURE vis_block(b: block_ptr; ch: CHAR);
BEGIN
  WHILE b#NIL DO
    print('  %c %08h size=%05h (%5d) free=%05h (%5d)\n'
          ,ch,b,b^.size,b^.size,b^.free,b^.free);
    vis(b^.head);
    b:=b^.next;
  END;
END vis_block;

PROCEDURE vis_area(a: AREA);
BEGIN
  IF (a=NIL) OR (a^.magic#aMAGIC) THEN RETURN END;
  acquire(a^.lock);
    vis_block(a^.busy,'B');
    vis_block(a^.work,'W');
  release(a^.lock);
END vis_area;

----------------------------------------------------------------

PROCEDURE ALLOCATE(area: AREA; VAR a: ADDRESS; words: INTEGER);

  PROCEDURE common_alloc;
    VAR size,rem: INTEGER; b,l: block_ptr;
  BEGIN
    IF words>req_size THEN size:=words ELSE size:=req_size END;
    INC(size,block_size);
    acquire(mem_lock);
      _alloc(common,req_size+block_size,a,size);
    release(mem_lock);
    IF a=NIL THEN RETURN END;
    DEC(free,size);
    INC(area^.total,size);
    b:=a; INC(a,block_size); DEC(size,block_size);
    b^.size:=size;
    b^.head:=NIL;
    b^.next:=NIL;
    b^.free:=0;
    rem:=size-words;
    IF rem>=slice_size THEN
      _dealloc(b^.head,a,rem);
      INC(a,rem); INC(b^.free,rem);

--    b^.next:=area^.work; area^.work:=b;

      IF area^.work=NIL THEN area^.work:=b
      ELSE
        l:=area^.work;
        WHILE l^.next#NIL DO l:=l^.next END;
        l^.next:=b;
      END;

      a^:=words-1;
    ELSE
      b^.next:=area^.busy; area^.busy:=b;
      a^:=-(size-1);
    END;
    INC(a);
  END common_alloc;

  VAR b: block_ptr; req: INTEGER;
BEGIN
  IF (area=NIL) OR (area^.magic#aMAGIC) OR (words<=0) THEN a:=NIL; RETURN END;
  INC(words);
  acquire(area^.lock);
    lock;
      IF words<req_size THEN
        b:=area^.work;
        LOOP
          IF b=NIL THEN common_alloc; EXIT END;
          IF b^.free>=words THEN
            _alloc(b^.head,slice_size,a,words);
            IF a#NIL THEN
              a^:=words-1; INC(a); DEC(b^.free,words);
              EXIT
            END
          END;
          b:=b^.next
        END
      ELSE common_alloc
      END;
    unlock;
  release(area^.lock)
END ALLOCATE;

PROCEDURE DEALLOCATE(area: AREA; VAR a: ADDRESS; words: INTEGER);
  VAR i: POINTER TO INTEGER; size: INTEGER;
    p,l,b: block_ptr;
BEGIN
  IF (area=NIL) OR (area^.magic#aMAGIC) THEN a:=NIL; RETURN END;
  IF (a=NIL) OR (words<=0) THEN a:=NIL; RETURN END;
  b:=NIL;
  i:=a-1; size:=i^;
  IF NOT (ABS(size)-words IN {0..slice_size-1}) THEN a:=NIL; RETURN END;
  lock;
  acquire(area^.lock);
    IF size<0 THEN -- from busy list
      b:=a-1-block_size;
      l:=area^.busy; p:=NIL;
      WHILE (l#NIL) & (l#b) DO p:=l; l:=l^.next END;
      IF l#NIL THEN
        IF p=NIL THEN area^.busy:=l^.next ELSE p^.next:=l^.next END
      END;
      DEC(area^.total,b^.size+block_size);
    ELSE
      l:=area^.work; p:=NIL;
      WHILE (l#NIL) & ((a<l) OR (a>=ADDRESS(l)+block_size+l^.size)) DO
        p:=l; l:=l^.next
      END;
      IF l#NIL THEN
        INC(l^.free,size+1);
        IF l^.free>=l^.size THEN
          IF p=NIL THEN area^.work:=l^.next ELSE p^.next:=l^.next END;
          b:=l;
          DEC(area^.total,b^.size+block_size);
        ELSE
          _dealloc(l^.head,i,size+1)
        END
      END
    END;
  release(area^.lock);
  IF b#NIL THEN
    acquire(mem_lock);
      INC(free,b^.size+block_size);
      _dealloc(common,b,b^.size+block_size);
    release(mem_lock)
  END;
  unlock; a:=NIL
END DEALLOCATE;

PROCEDURE new(VAR area: AREA): BOOLEAN;
BEGIN
  ALLOCATE(system,area,SIZE(area^));
  IF area=NIL THEN RETURN FALSE END;
  ini_area(area^);
  RETURN TRUE
END new;

PROCEDURE delete(VAR area: AREA);

  PROCEDURE _release(l: block_ptr);
    VAR a: ADDRESS; size: INTEGER;
  BEGIN
    WHILE l#NIL DO
      a:=l;  size:=l^.size+block_size;  l:=l^.next;
      _dealloc(common,a,size);  INC(free,size)
    END
  END _release;

BEGIN
  IF (area=NIL) OR (area^.magic#aMAGIC) THEN area:=NIL; RETURN END;
  IF  area=system   THEN  RETURN   END;
  lock;
  acquire(area^.lock);
    area^.magic:=0;
    acquire(mem_lock);
      _release(area^.busy);  area^.busy:=NIL;
      _release(area^.work);  area^.work:=NIL;
    release(mem_lock);
  release(area^.lock);
  DEALLOCATE(system,area,SIZE(area_rec));
  unlock
END delete;

PROCEDURE ini_memory;

  CONST KB = 256;

  PROCEDURE memory(VAL s: ARRAY OF CHAR;
                   VAR a: ADDRESS;  VAR size: INTEGER): BOOLEAN;
    VAR i: INTEGER;
        t: ADDRESS;
     done: BOOLEAN;
     list: slice_ptr;
  BEGIN
    i:=1;
    str.skip(s,i,' ');
    str.scan(s,i,'MEM ',done);
    IF NOT done THEN RETURN FALSE END;
    str.skip(s,i,' ');
    str.iscan(a,s,i,done);
    IF NOT done THEN RETURN FALSE END;
    str.skip(s,i,' ');
    str.iscan(size,s,i,done);
    IF NOT done THEN RETURN FALSE END;
    IF (i<=HIGH(s)) & (s[i]='K') THEN size:=size*KB END;
    IF size<=0 THEN RETURN FALSE END;
    list:=common;
    WHILE list#NIL DO
      IF (list<=a) & (a<ADDRESS(list)+list^.size)
      OR (a<=list) & (list<a+size)       THEN
        print("memory segment overlaps %08h..%08h\n",a,a+size-1);
        RETURN FALSE
      END;
      list:=list^.next
    END;
    FOR i:=0 TO size-1 BY KB DO t:=a+i; t^:=BITSET(12345678h)/BITSET(t) END;
    FOR i:=0 TO size-1 BY KB DO t:=a+i;
      IF BITSET(t^)#BITSET(12345678h)/BITSET(t) THEN
        print("memory error at address %08h\n",t); RETURN FALSE
      END
    END;
    RETURN TRUE
  END memory;

  PROCEDURE append(a: ADDRESS; words: INTEGER);
  BEGIN
    IF words<slice_size THEN RETURN END;
    _dealloc(common,a,words);
    INC(total,words);  INC(free ,words)
  END append;

  VAR a: ADDRESS;
      i: INTEGER;
    str: STRING;
   size: INTEGER;

BEGIN
  a:=88h; memtop:=a^;
  a:=85h; a:=a^; size:=memtop-a+1;
  ini_mutex(mem_lock);
  common:=NIL;
  system:=a;
  INC(a,SIZE(system^)); DEC(size,SIZE(system^));
  ini_area(system^);
  total:=0; free:=0;
  append(a,size);
  core:=a;
  i:=0;
  WHILE get_sys_parm(i,str) DO
    IF memory(str,a,size) THEN append(a,size) END
  END
END ini_memory;

---------------------------------------------------------------

VAR
  idle_wsp: ADDRESS;
  ipts_wsp: ADDRESS;
  clk_wsp : ADDRESS;


PROCEDURE ini_scheduler;

  PROCEDURE malloc(VAR a: ADDRESS; size: INTEGER);
  BEGIN
    _alloc(common,0,a,size);
    IF a=NIL THEN quit(0F005h) END
  END malloc;

  VAR p: process;
      q: queue_ptr;
      n: node_ptr;
      N: node_ptr;
      x: HW_descriptor;
      i: INTEGER;
      a: ADDRESS;
BEGIN
  IF SIZE(OS_descriptor)>=256          THEN quit(0F001h) END;
  IF ADR(x.prs)-ADR(x)#8 THEN quit(0F002h) END;
  WS:=ws();
  setm({});
---------------------------------------------------------------
  WITH ini_process DO
    magic:=MAGIC;
    pp    :=NIL;        r_time:=-1;     NEW(parm);
    task  :=NIL;        node  :=NIL;
    status:=_new;       tags  :={};
    guard :=0;          mutex :=NIL;
    server:=NIL;        snares:=NIL;
    halt  :=NIL;
    prio  :=0;          ticks :=0
  END;
  WITH ini_queue DO
    fwd    :=NIL;       bck    :=NIL;
    next   :=NIL;       sig    :=NIL;
    prs    :=NIL;       info   :=-1
  END;
  WITH ini_node DO
    papa  :=NIL;       sons   :=NIL;
    bro   :=NIL;       next   :=NIL;
    exts  :=NIL
  END;

----------------------------------------------------------------

  malloc(p,SIZE(p^));    p^:=ini_process;
  malloc(q,SIZE(q^));    q^:=ini_queue;
  malloc(n,SIZE(n^));    n^:=ini_node;
  malloc(N,SIZE(N^));    N^:=ini_node;
  q^.prs:=p;
  p^.pp:=_activ(); p^.pp^.prs:=p;
  p^.node:=n; n^.prs:=p;
  INC(p^.guard);
  p^.status:=_ready;
  ready:=q;
  ready^.fwd :=ready;  ready^.bck:=ready;
  ready^.next:=ready;  ready^.sig:=REF(ready,queue_ref);

  malloc(task0,SIZE(task0^));
  WITH task0^ DO
    magic :=MAGIC;
    node  :=N;         N^.task:=task0;
    prs   :=p;         p^.task:=task0;
    id    :=0;
    user  :=0;
    opens :=999999;
    status:=def.running;
    area  :=system;
    env   :=NIL;
    main  :=NIL;
    reso  :=NIL;
    res   :=0;
    ipr   :=FALSE;
    ini_mutex(lock);
    NEW(wspX);
    FOR i:=0 TO HIGH(inp) DO
      malloc(inp[i],SIZE(inp[i]^)); ini_signal(inp[i]^,{},0);
      malloc(out[i],SIZE(out[i]^)); ini_signal(out[i]^,{},0)
    END
  END;

----------------------------------------------------------------

  IDLE:=FALSE;
  malloc(idle_wsp,64);
  new_process(IdleProc,idle_wsp,64,idler);

---------------------------------------------------------------

  noAct:=0;
  timeq:=NIL;
  timer:=0; tick:=1000 DIV frequency; second:=timer+frequency;
  time :=0; zone:=0;
  malloc(clk_wsp,256);
  new_process(Clock,clk_wsp,256,clocker);
  a:=2; a^:=clocker;
  a:=3; a^:=ADR(ipted);

---------------------------------------------------------------

  malloc(ipts_wsp,256);
  new_process(interrupt,ipts_wsp,256,system_driver);
  FOR i:=2 TO 3Fh DO
    a:=i*2; a^:=system_driver;
    a:=a+1; a^:=ADR(traps[i]); traps[i]:=NIL
  END

END ini_scheduler;


-----------------------  ENVIRONMENT  --------------------------
                       ---------------


CONST
  ALIVE  = {def.new,def.loaded,def.ready,def.running};
  NORMAL = ALIVE + {def.stopped};

PROCEDURE new_env(VAR e   : env_ptr;
                      task: task_ptr;
                  VAL name: ARRAY OF CHAR;
                      len : INTEGER;
                  ): INTEGER;
  VAR new: WORDs;
BEGIN
  e:=task^.env;
  WHILE (e#NIL) & (e^.name#name) DO e:=e^.next END;
  IF e=NIL THEN
    ALLOCATE(task^.area,e,SIZE(e^));
    IF e=NIL THEN RETURN err.no_memory END;
    e^.name^.ADR:=NIL; e^.data^.ADR:=NIL;
    e^.name^.HIGH:=str.len(name);
    ALLOCATE(task^.area,e^.name^.ADR,SIZE(e^.name));
    IF e^.name^.ADR=NIL THEN RETURN err.no_memory END;
    str.copy(e^.name,name);
    e^.data^.ADR:=NIL; e^.data^.HIGH:=-1;
    IF len>0 THEN
      ALLOCATE(task^.area,e^.data^.ADR,len);
      IF e^.data^.ADR=NIL THEN RETURN err.no_memory END;
      e^.data^.HIGH:=len-1;
    END;
    e^.next:=task^.env; task^.env:=e;
  ELSIF len#SIZE(e^.data) THEN
    new^.ADR:=NIL; new^.HIGH:=-1;
    IF len>0 THEN
      ALLOCATE(task^.area,new^.ADR,len);
      IF new^.ADR=NIL THEN e:=NIL; RETURN err.no_memory END;
                          ------  save old data
      new^.HIGH:=len-1;
    END;
    DEALLOCATE(task^.area,e^.data^.ADR,SIZE(e^.data));
    e^.data^:=new^;
  END;
  RETURN err.ok
END new_env;

PROCEDURE put_env(t: task_ptr;
           VAL name: ARRAY OF CHAR;
           VAL data: ARRAY OF WORD;
            private: BOOLEAN;
                  ): INTEGER;

  VAR e: env_ptr; res: INTEGER;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF NOT (t^.status IN NORMAL)    THEN RETURN err.ill_desc END;
  acquire(t^.lock);
    res:=new_env(e,t,name,SIZE(data));
    IF res=err.ok THEN
      e^.priv:=private;
      IF SIZE(data)>0 THEN
        (*$<X+*)
          e^.data:=data
        (*$>*)
      END;
    ELSIF e#NIL THEN
      DEALLOCATE(t^.area,e^.name^.ADR,SIZE(e^.name));
      DEALLOCATE(t^.area,e,SIZE(e^));
    END;
  release(t^.lock);
  RETURN res
END put_env;

PROCEDURE put_str(t: task_ptr;
           VAL name: ARRAY OF CHAR;
           VAL data: ARRAY OF CHAR;
            private: BOOLEAN;
                  ): INTEGER;
  VAR e: env_ptr; res,len: INTEGER; s: STRING;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF NOT (t^.status IN NORMAL)   THEN RETURN err.ill_desc END;
  acquire(t^.lock);
    len:=str.len(data) DIV 4 +1;
    res:=new_env(e,t,name,len);
    IF res=err.ok THEN
      e^.priv:=private;
      IF len>0 THEN
        s^.ADR:=e^.data^.ADR; s^.HIGH:=BYTES(e^.data)-1;
        str.copy(s,data);
      END;
    ELSIF e#NIL THEN
      DEALLOCATE(t^.area,e^.name^.ADR,SIZE(e^.name));
      DEALLOCATE(t^.area,e,SIZE(e^));
    END;
  release(t^.lock);
  RETURN res
END put_str;

PROCEDURE get_env(t: task_ptr;
              delay: INTEGER;
           VAL name: ARRAY OF CHAR;
           VAR data: STRING;
           VAR priv: BOOLEAN): INTEGER;
  VAR e: env_ptr; res: INTEGER;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF NOT acquire_del(delay,t^.lock) THEN RETURN err.busy  END;
    e:=t^.env;
    WHILE (e#NIL) & (e^.name#name) DO e:=e^.next END;
    IF e#NIL THEN
      data^.ADR:=e^.data^.ADR;
      data^.HIGH:=BYTES(e^.data)-1;
      priv:=e^.priv;
      res:=err.ok;
    ELSE
      data^.ADR:=NIL; data^.HIGH:=-1;
      res:=err.no_entry
    END;
  release(t^.lock);
  RETURN res
END get_env;

PROCEDURE del_env(t: task_ptr; VAL name: ARRAY OF CHAR): INTEGER;
  VAR e,x: env_ptr; res: INTEGER;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF NOT (t^.status IN NORMAL)    THEN RETURN err.ill_desc END;
  acquire(t^.lock);
    e:=t^.env; x:=NIL;
    WHILE (e#NIL) & (e^.name#name) DO x:=e; e:=e^.next END;
    IF e=NIL THEN res:=err.no_entry;
    ELSE
      IF x=NIL THEN t^.env:=e^.next ELSE x^.next:=e^.next END;
      DEALLOCATE(t^.area,e^.data^.ADR,SIZE(e^.data));
      DEALLOCATE(t^.area,e^.name^.ADR,SIZE(e^.name));
      DEALLOCATE(t^.area,e,SIZE(e^));
      res:=err.ok;
    END;
  release(t^.lock);
  RETURN res
END del_env;

PROCEDURE copy_env(f,t: task_ptr): INTEGER;
  VAR e: env_ptr; res: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.magic#MAGIC)  THEN RETURN err.bad_desc  END;
  IF NOT (f^.status IN NORMAL)    THEN RETURN err.ill_desc  END;
  IF f=t                          THEN RETURN err.duplicate END;
  acquire(f^.lock);
    e:=f^.env; res:=err.ok;
    LOOP
      IF e=NIL THEN EXIT END;
      IF NOT e^.priv THEN
        res:=put_env(t,e^.name,e^.data,FALSE);
        IF res#err.ok THEN EXIT END;
      END;
      e:=e^.next;
    END;
  release(f^.lock);
  RETURN res
END copy_env;

PROCEDURE show_env(       t: task_ptr;
                      entry: INTEGER;
                      delay: INTEGER;
                   VAR name: STRING;
                   VAR data: STRING;
                   VAR priv: BOOLEAN): INTEGER;
  VAR e: env_ptr;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF NOT (t^.status IN NORMAL)   THEN RETURN err.ill_desc END;
  name^.ADR:=NIL; name^.HIGH:=-1;
  data^.ADR:=NIL; data^.HIGH:=-1;
  acquire(t^.lock);
    e:=t^.env;
    WHILE (e#NIL) & (entry>0) DO
      e:=e^.next; entry:=entry-1
    END;
    IF e=NIL THEN
      release(t^.lock); RETURN err.no_entry
    END;
    name^:=e^.name^;
    priv:=e^.priv;
    data^.ADR :=e^.data^.ADR;
    data^.HIGH:=BYTES(e^.data)-1;
  release(t^.lock);
  RETURN err.ok
END show_env;

PROCEDURE final(t: task_ptr; proc: PROC): INTEGER;
  VAR r: reso_ptr;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  IF NOT (t^.status IN ALIVE)    THEN RETURN err.ill_desc END;
  ALLOCATE(t^.area,r,SIZE(r^));
  IF r=NIL THEN RETURN err.no_memory END;
  acquire(t^.lock);
    r^.proc:=proc;
    r^.next:=t^.reso;
    t^.reso:=r;
  release(t^.lock);
  RETURN err.ok
END final;

--------------------------  TASKs  -----------------------------
                          ---------

VAR
  task_lock: mutex_rec;
     cur_id: INTEGER;
     closeq: node_ptr;      -- queue for close process
  not_empty: signal_rec;    -- signal for start close process


PROCEDURE trav(papa: node_ptr; VAR list: node_ptr);
  VAR a,b,d,l,lim: node_ptr;
BEGIN
  l:=papa^.sons;
  a:=NIL;
  WHILE l#NIL DO l^.next:=a; a:=l; l:=l^.bro END;
  lim:=NIL; b:=NIL;
  WHILE b#a DO
    b:=a; d:=a;
    WHILE b#lim DO
      l:=b^.sons;
      WHILE l#NIL DO l^.next:=a; a:=l; l:=l^.bro END;
      b:=b^.next;
    END;
    lim:=d;
  END;
  list:=a;
END trav;

PROCEDURE open(VAR task: task_ptr; papa: task_ptr; id: INTEGER): INTEGER;
  VAR node,l: node_ptr; res: INTEGER;
BEGIN
  task:=NIL;
  IF (papa=NIL) OR (papa^.magic#MAGIC) THEN RETURN err.bad_desc END;
  lock; acquire(task_lock);
    node:=papa^.node;
    IF id=papa^.id THEN task:=papa
    ELSE
      trav((node),l);
      WHILE (l#NIL) & (l^.task^.id#id) DO l:=l^.next END;
      IF l#NIL THEN task:=l^.task END
    END;
    IF task#NIL THEN INC(task^.opens); res:=err.ok ELSE res:=err.no_entry END;
  release(task_lock); unlock;
  RETURN res
END open;

PROCEDURE reopen(t: task_ptr);
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN END;
  lock; acquire(task_lock);
    INC(t^.opens);
  release(task_lock); unlock
END reopen;

PROCEDURE close(VAR t: task_ptr);
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) OR (t^.opens<=0) THEN RETURN END;
  lock; acquire(task_lock);
    DEC(t^.opens);
    IF (t^.opens=0) & (t^.status=def.killed) THEN
      t^.node^.next:=closeq; closeq:=t^.node;
      send(not_empty)
    END;
  release(task_lock); unlock
END close;

PROCEDURE self(): task_ptr;
  VAR my: PROCESS;
BEGIN
  my:=_activ(); RETURN my^.prs^.task
END self;

PROCEDURE papa(t: task_ptr; VAR id: INTEGER);
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN id:=-1; RETURN END;
  acquire(task_lock);
    IF t^.node^.papa=NIL THEN id:=-1 ELSE id:=t^.node^.papa^.task^.id END;
  release(task_lock)
END papa;

PROCEDURE son(t: task_ptr; VAR id: INTEGER);
  VAR n: node_ptr;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN id:=-1; RETURN END;
  acquire(task_lock);
    n:=t^.node^.sons;
    IF (n=NIL) OR (n^.task=NIL) THEN id:=-1 ELSE id:=n^.task^.id END;
  release(task_lock)
END son;

PROCEDURE brother(t: task_ptr; VAR id: INTEGER);
  VAR n: node_ptr;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN id:=-1; RETURN END;
  acquire(task_lock);
    n:=t^.node^.bro;
    IF (n=NIL) OR (n^.task=NIL) THEN id:=-1 ELSE id:=n^.task^.id END;
  release(task_lock)
END brother;

---------------------------------------------------------------

CONST min_size = SIZE(HW_descriptor)
                +SIZE(OS_descriptor)
                +SIZE(queue_rec)
                +5                   -- proc link
                +16                  -- for saving stack
                ;

PROCEDURE make_process0(VAR p   : process;
                            task: task_ptr;
                            base: PROC;
                             wsp: ADDRESS;
                            size: INTEGER
                        );
  VAR q: queue_ptr;
BEGIN
  IF size<min_size THEN p:=NIL; RETURN END;
  p:=wsp; p^:=ini_process; INC(wsp,SIZE(p^)); DEC(size,SIZE(p^));
  q:=wsp; q^:=ini_queue;   INC(wsp,SIZE(q^)); DEC(size,SIZE(q^));
  new_process(base,wsp,size,p^.pp);
  p^.pp^.prs:=p; q^.prs:=p;
  p^.task:=task;
  p^.halt:=task^.inp[def.stop];
END make_process0;

PROCEDURE make_process1(VAR p   : process;
                            task: task_ptr;
                            base: PROC;
                             wsp: ADDRESS;
                            size: INTEGER): BOOLEAN;
  VAR n: node_ptr;
BEGIN
  ALLOCATE(task^.area,n,SIZE(n^));
  IF n=NIL THEN RETURN TRUE END;
  n^:=ini_node;
  make_process0(p,task,base,wsp,size);
  n^.prs:=p; p^.node:=n;
  p^.wsp^.ADR :=wsp;
  p^.wsp^.HIGH:=size-1;
  RETURN FALSE
END make_process1;

PROCEDURE tie(papa,n: node_ptr);
BEGIN
  n^.papa:=papa;
  n^.bro:=papa^.sons; papa^.sons:=n;
END tie;

PROCEDURE make_process(VAR new: process; base: PROC; size: INTEGER): INTEGER;
  VAR task: task_ptr; prs: process; res: INTEGER; wsp: ADDRESS;
BEGIN
  prs:=active(); task:=prs^.task;
  acquire(task^.lock); lock;
    INC(size,min_size);
    ALLOCATE(task^.area,wsp,size);
    IF wsp=NIL THEN res:=err.no_memory
    ELSE
      IF make_process1(new,task,base,wsp,size) THEN
        DEALLOCATE(task^.area,wsp,size);
        res:=err.no_memory;
      ELSE
        new^.server:=prs^.server;
        tie(prs^.node,new^.node);
        res:=err.ok;
      END;
    END;
  release(task^.lock); unlock;
  RETURN res
END make_process;

PROCEDURE rem_process(VAR p: process): INTEGER;
  VAR
    task: task_ptr;
    res : INTEGER;
    n,l : node_ptr;
    a   : ADDRESS;
    pn  : POINTER TO node_ptr;
BEGIN
  IF p=NIL THEN RETURN err.ok END;
  task:=p^.task;
  acquire(task^.lock);
--  lock;
  IF (p^.magic#MAGIC) OR (p^.status#_aborted) THEN
    res:=err.bad_desc
  ELSE
    n:=p^.node; res:=err.ok;
    ASSERT(n#NIL);
    ASSERT(n^.prs=p);
    WHILE (res=err.ok) & (n^.sons#NIL) DO
      l:=n^.sons^.bro;
      res:=rem_process(n^.sons^.prs);
      IF res=err.ok THEN n^.sons:=l END;
    END;
    IF res=err.ok THEN
      IF n^.papa#NIL THEN
        pn:=ADR(n^.papa^.sons);
        LOOP
          IF pn^=NIL THEN EXIT END;
          IF pn^=n THEN pn^:=n^.bro; EXIT END;
          pn:=ADR(pn^^.bro);
        END;
      END;
      a:=p^.wsp^.ADR;
      DEALLOCATE(task^.area,a,p^.wsp^.HIGH+1);
      DEALLOCATE(task^.area,n,SIZE(n^));
    END;
  END;
  release(task^.lock);
--  unlock;
  RETURN res;
END rem_process;

PROCEDURE abort(prs: process; wait: BOOLEAN);
  VAR a,l: node_ptr; t: task_ptr;
BEGIN
  IF (prs=NIL) OR (prs^.magic#MAGIC) THEN RETURN END;
  t:=prs^.task;
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN END;
  acquire(t^.lock); lock;
    trav((prs^.node),a);
    l:=a;
    WHILE l#NIL DO
      IF l^.prs#NIL THEN INCL(l^.prs^.tags,abort_tag) END;
      l:=l^.next;
    END;
    l:=a;
    WHILE l#NIL DO
      IF l^.prs#NIL THEN stop(l^.prs,wait) END;
      l:=l^.next;
    END;
  release(t^.lock); unlock;
END abort;

---------------------------------------------------------------

PROCEDURE header; FORWARD;

PROCEDURE create(VAR task: task_ptr; father: task_ptr): INTEGER;

  PROCEDURE _self_user(): INTEGER;
    VAR p: process; t: task_ptr;
  BEGIN
    p:=active(); t:=p^.task;
    IF t=NIL THEN RETURN 0 ELSE RETURN t^.user END
  END _self_user;

  PROCEDURE new_signal(area: AREA; VAR s: signal_ptr): BOOLEAN;
  BEGIN
    ALLOCATE(area,s,SIZE(s^));
    IF s=NIL THEN RETURN TRUE END;
    ini_signal(s^,{},0);
    RETURN FALSE
  END new_signal;

  PROCEDURE ini(VAR t: task_ptr; area: AREA): BOOLEAN;
    CONST size=min_size+128;
    VAR i: INTEGER; wsp: ADDRESS;
  BEGIN
    ALLOCATE(area,t,SIZE(t^));
    IF t=NIL THEN RETURN TRUE END;
    ALLOCATE(area,t^.node,SIZE(t^.node^));
    IF t^.node=NIL THEN RETURN TRUE END;
    t^.node^:=ini_node;
    t^.node^.task:=t;
    t^.area:=area;
    WITH t^ DO
      magic :=MAGIC;
      id    :=cur_id;     INC(cur_id);
      user  :=_self_user();
      mucode:=0;
      opens :=1;
      status:=def.new;
      env   :=NIL;
      prs   :=NIL;
      main  :=NIL;
      res   :=0;
      ipr   :=TRUE;
      reso  :=NIL;
      ini_mutex(lock);
      FOR i:=0 TO HIGH(inp) DO
        IF new_signal(area,inp[i]) THEN RETURN TRUE END;
        IF new_signal(area,out[i]) THEN RETURN TRUE END;
      END;
      wspX^.HIGH:=min_size+400;
      ALLOCATE(area,wspX^.ADR,SIZE(wspX));
      IF wspX^.ADR=NIL THEN RETURN TRUE END;
      loader:=NIL;
    END;
    ALLOCATE(area,wsp,size);
    IF wsp=NIL THEN RETURN TRUE END;
    IF make_process1(t^.prs,t,header,wsp,size) THEN RETURN TRUE END;
    tie(father^.node,t^.node);
    RETURN FALSE
  END ini;

  VAR res: INTEGER; t: task_ptr; area: AREA;

BEGIN
  task:=NIL;
  IF (father=NIL) OR (father^.magic#MAGIC) THEN RETURN err.bad_desc END;
  lock;
  IF NOT new(area) THEN unlock; RETURN err.no_memory END;
  acquire(task_lock);
    IF ini(t,area) THEN res:=err.no_memory;
    ELSE
      start(t^.prs);
      task:=t;
      res:=err.ok;
    END;
  release(task_lock);
  unlock;
  RETURN res
END create;

PROCEDURE run(t: task_ptr; proc: PROC; size: INTEGER): INTEGER;

  PROCEDURE _run(): INTEGER;
    VAR p: process; wsp: ADDRESS; res: INTEGER;
  BEGIN
    INC(size,min_size);
    ALLOCATE(t^.area,wsp,size);
    IF wsp=NIL THEN RETURN err.no_memory END;
    IF make_process1(p,t,proc,wsp,size) THEN
      DEALLOCATE(t^.area,wsp,size);
      RETURN err.no_memory
    END;
    tie(t^.prs^.node,p^.node);
    t^.main:=p;
    t^.status:=def.ready;
    RETURN err.ok
  END _run;

  VAR res: INTEGER;
BEGIN
  IF (t=NIL) OR (t^.magic#MAGIC) THEN RETURN err.bad_desc END;
  acquire(t^.lock); lock;
    IF t^.status#def.loaded THEN res:=err.ill_desc
    ELSE
      res:=_run();
    END;
  release(t^.lock); unlock;
  RETURN res
END run;

---------------------------------------------------------------

PROCEDURE closure;
  VAR r: reso_ptr; prs: process; proc: PROC;
BEGIN
  prs:=active();
  r:=prs^.parm^.ADR;
  WHILE r#NIL DO
    proc:=r^.proc;
    r:=r^.next;
    prs^.parm^.ADR:=r;
    proc;
  END;
END closure;

PROCEDURE header;

  PROCEDURE reaction(t: task_ptr; n: node_ptr; igno: BOOLEAN): BOOLEAN;
    VAR prs: process; s: signal_rec;
  BEGIN
    WHILE t^.reso#NIL DO
      make_process0(prs,t,closure,t^.wspX^.ADR,SIZE(t^.wspX));
      n^.prs:=prs; prs^.node:=n;
      ini_signal(s,{},0);
      prs^.halt:=REF(s,signal_ptr);
      prs^.parm^.ADR:=t^.reso;
      prs^.task:=t;
      t^.main:=prs;
      start(prs);
      release(t^.lock);
        wait(s);
      acquire(t^.lock);
      t^.reso:=prs^.parm^.ADR;
      IF NOT igno & (prs^.pp^.T#47h) THEN RETURN FALSE END;
    END;
    RETURN TRUE
  END reaction;

  PROCEDURE ipr_sons(t: task_ptr);
    VAR l: node_ptr;
  BEGIN
    acquire(task_lock);
      l:=t^.node^.sons;
      WHILE l#NIL DO
        send(l^.task^.inp[def.ipr]^);
        l:=l^.bro;
      END;
    release(task_lock)
  END ipr_sons;

  PROCEDURE kill_sons(t: task_ptr);
    VAR l: node_ptr;
  BEGIN
    acquire(task_lock);
      l:=t^.node^.sons;
      WHILE l#NIL DO  send(l^.task^.inp[def.kill]^);  l:=l^.bro  END;
    release(task_lock);
    LOOP
      acquire(task_lock);
        l:=t^.node^.sons;
        IF l#NIL THEN INC(l^.task^.opens) END;
      release(task_lock);
      IF l=NIL THEN EXIT END;
      wait(l^.task^.out[def.kill]^);   close(l^.task)
    END
  END kill_sons;

  PROCEDURE kill(t: task_ptr);
    VAR papa,p,l: node_ptr;
  BEGIN
    IF  t^.node^.papa=NIL THEN RETURN END;
    acquire(task_lock);
      papa:=t^.node^.papa;
      IF papa#NIL THEN
        l:=papa^.sons; p:=NIL;
        WHILE (l#NIL) & (l^.task#t) DO p:=l; l:=l^.bro END;
        IF l#NIL THEN
          IF p=NIL THEN papa^.sons:=t^.node^.bro ELSE p^.bro:=t^.node^.bro END
        END
      END;
      IF t^.opens=0 THEN
        t^.node^.next:=closeq; closeq:=t^.node;
        t^.prs^.halt:=REF(not_empty,signal_ptr)
      END;
    release(task_lock);
    t^.status:=def.killed;
    signal(t^.out[def.kill]^,MAX(INTEGER))
  END kill;

  PROCEDURE stop(t: task_ptr; igno: BOOLEAN): BOOLEAN;
    VAR l: task_ptr; a: ADDRESS;
  BEGIN
    abort(t^.prs,TRUE);
    IF (t^.res=0) & (t^.main^.pp^.T=4Dh) THEN
      a:=t^.main^.pp^.S; DEC(a);
      IF INTEGER(a^)>0 THEN
        DEC(a);
        t^.res:=a^;
      END;
    END;
    t^.status:=def.stopped;
    RETURN igno OR (t^.main^.pp^.T=47h)
  END stop;

  VAR prs: process;   t: task_ptr;  no: INTEGER;
     node: node_rec;  n: node_ptr;

BEGIN
  lock;
  prs:=active();
  set_prio(prs,-1);
  t:=prs^.task;
  IF (t=NIL) OR (t^.magic#MAGIC) THEN unlock; RETURN END;
  n:=REF(node,node_ptr);
  node:=ini_node;
  acquire(t^.lock);
    tie(t^.prs^.node,n);
  release(t^.lock);
  LOOP
    no:=alt_wait(-1,t^.inp);
    acquire(t^.lock);
      IF no=def.stop THEN
        IF t^.status=def.running THEN
          IF stop(t,FALSE) & reaction(t,n,FALSE) THEN EXIT END;
          signal(t^.out[def.stop]^,MAX(INTEGER))
        END;
      ELSIF no=def.kill THEN
        IF (t^.status=def.running) & stop(t,TRUE) THEN END;
        IF reaction(t,n,TRUE) THEN END;
        EXIT
      ELSIF no=def.ipr THEN
        IF NOT t^.ipr THEN ipr_sons(t) END;
        t^.ipr:=TRUE
      ELSIF no=def.start THEN
        IF t^.status=def.ready THEN
          t^.status:=def.running;
          start(t^.main);
          DEALLOCATE(t^.area,t^.inp[def.start],SIZE(signal_rec))
        END
      END;
    release(t^.lock)
  END;
  kill_sons(t); kill(t);
  unlock
END header;

---------------------------------------------------------------

PROCEDURE task_manager;
  VAR t: task_ptr; area: AREA;
BEGIN
  LOOP
    wait(not_empty);
    acquire(task_lock);
      IF closeq=NIL THEN
        t:=NIL;
      ELSE
        t:=closeq^.task;  closeq:=closeq^.next
      END;
    release(task_lock);
    IF t#NIL THEN t^.magic:=0; area:=t^.area; delete(area) END
  END
END task_manager;

---------------------------------------------------------------

PROCEDURE _print(format: ARRAY OF CHAR; SEQ args: WORD); END _print;

PROCEDURE set_debug(p: print_proc); BEGIN print:=p END set_debug;

PROCEDURE debug_off; BEGIN print:=_print END debug_off;

PROCEDURE get_sys_parm(VAR n: INTEGER; VAR str: STRING): BOOLEAN;
  VAR i: INTEGER;
      a: ADDRESS;
BEGIN
  a:=83h; a:=a^; i:=n;
  str^.ADR:=a+3; str^.HIGH:=0;
  LOOP
    IF (a=0) THEN RETURN FALSE END;
    IF str[0]='$' THEN
      IF i<=0 THEN EXIT ELSE DEC(i) END
    END;
    a:=a^; str^.ADR:=a+3
  END;
  a:=a+2; str^.HIGH:=a^; INC(n);
  RETURN TRUE
END get_sys_parm;

---------------------------------------------------------------

PROCEDURE set_mask;
BEGIN
  MASK:={1,3..7,31};
  IF low.cpu#5          THEN MASK:=MASK+{0}    END;
  IF ws() & (low.cpu=6) THEN MASK:=MASK+{0,12} END
END set_mask;

BEGIN
  sMAGIC:=4953h; (* SI *)
  mMAGIC:=554Dh; (* MU *)
  aMAGIC:=5241h; (* AR *)
  debug_off;
  set_mask;
set_debug(bug.print);
  ini_memory;
  ini_scheduler;
  ini_mutex(task_lock);
  cur_id:=1;
  closeq:=NIL;
  ini_signal(not_empty,{},0);
  version:=1310;
  IF put_env(task0,"NAME","Excelsior iV 1.31",TRUE)#0 THEN END
END osKernel.
