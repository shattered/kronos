IMPLEMENTATION MODULE Exceptions; (* Ned 20-Apr-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  cod: defCodes;
IMPORT  os : osKernel;
IMPORT  env: tskEnv;

WITH STORAGE (NEW: env.allocate);

CONST MAGIC = 524E5324h; -- $SNR

CONST external = 31;

TYPE
  trap_ptr  = POINTER TO trap_rec;
  trap_rec  = RECORD
                G   : sys.ADDRESS;
                L   : sys.ADDRESS;
                S   : sys.ADDRESS;
                PC  : INTEGER;
                sG  : sys.ADDRESS;
                sPC : BITSET;
                sM  : BITSET;
                exc : POINTER TO INTEGER;
                soft: BOOLEAN;
                all : BOOLEAN;
                id  : INTEGER;
                next: trap_ptr;
              END;
  snare_ptr = POINTER TO snare_rec;
  snare_rec = RECORD
                magic: INTEGER;
                traps: trap_ptr;
                rems : trap_ptr;
              END;
  proc_ptr  = POINTER TO proc_rec;
  proc_rec  = RECORD
                G : sys.ADDRESS;
                L : proc_ptr;
                PC: BITSET;
                M : BITSET;
              END;

VAR
  slave: os.PROCESS;
  ipts : os.PROCESS;
  dummy: os.PROCESS;

PROCEDURE proc_glo(proc: sys.WORD): sys.ADDRESS;
CODE cod.lib 0FFh 8 cod.ror cod.bic cod.lsw0 END proc_glo;

PROCEDURE proc_no(proc: sys.WORD): INTEGER;
CODE 8 cod.rol cod.lib 0FFh cod.and END proc_no;

PROCEDURE link(): proc_ptr; CODE cod.lla 0 END link;
PROCEDURE depth(): INTEGER; CODE cod.store cod.lodt END depth;
PROCEDURE lodt (): INTEGER; CODE cod.lodt END lodt;

PROCEDURE transfer(VAR from,to: os.PROCESS); CODE cod.tra END transfer;
PROCEDURE push(n: INTEGER); CODE END push;

PROCEDURE restore;

  PROCEDURE restore(func: BOOLEAN; res: INTEGER);
    VAR prs: os.process; s: snare_ptr; p: proc_ptr; x: trap_ptr;
  BEGIN
    prs:=os.active();
    s:=prs^.snares;
    p:=link();
    IF (s^.magic#MAGIC) OR (s^.traps=NIL) OR (s^.traps^.L#p) THEN
      prs^.server:=NIL; ASSERT(FALSE);
    END;
    x:=s^.traps; s^.traps:=x^.next;
    WHILE (s^.traps#NIL) & (s^.traps^.L=p) DO
      x^.next:=s^.rems; s^.rems:=x;
      x:=s^.traps;
      s^.traps:=x^.next;
    END;
    p^.G :=x^.sG;
    p^.PC:=x^.sPC+{external};
    p^.M :=x^.sM;
    x^.next:=s^.rems; s^.rems:=x;
    IF func THEN push(res) END;
  END restore;

BEGIN
  IF depth()=0 THEN restore(FALSE,0) ELSE restore(TRUE,lodt()) END;
END restore;

PROCEDURE set(VAR e: INTEGER; soft,all: BOOLEAN);
  VAR prs: os.process; s: snare_ptr; x: trap_ptr; p: proc_ptr;
    g: POINTER TO POINTER TO ARRAY [0..255] OF BITSET;
    n: INTEGER;
BEGIN
  prs:=os.active();
  s:=prs^.snares;
  IF s=NIL THEN
    NEW(s);
    IF s=NIL THEN ASSERT(FALSE,4Eh) END;
    s^.magic:=MAGIC; s^.traps:=NIL; s^.rems:=NIL;
    prs^.snares:=s;
    prs^.server:=ipts;
  ELSE ASSERT(s^.magic=MAGIC)
  END;
  IF s^.rems#NIL THEN
    x:=s^.rems; s^.rems:=x^.next
  ELSE
    NEW(x);
    IF x=NIL THEN ASSERT(FALSE,4Eh) END;
  END;
  p:=link(); p:=p^.L;
  x^.soft:=soft;
  x^.all:=all;
  IF all THEN x^.exc:=sys.ADR(e) ELSE x^.exc:=NIL; x^.id:=e END;
  x^.G:=p^.G;  x^.L:=p^.L;  x^.S:=p;
  x^.PC:=INTEGER(p^.PC*{0..15});
  p:=p^.L;
  x^.sM :=p^.M;
  x^.sPC:=p^.PC;
  IF external IN p^.PC THEN x^.sG:=p^.G ELSE x^.sG:=x^.G END;
  x^.next:=s^.traps; s^.traps:=x;
  g:=proc_glo(restore);
  n:=proc_no (restore);
  p^.G :=g;
  p^.PC:=g^^[n]+{external};
END set;

PROCEDURE exceptions(VAR e: INTEGER): BOOLEAN;
BEGIN
  set(e,TRUE,TRUE);
  RETURN FALSE
END exceptions;

PROCEDURE traps(VAR e: INTEGER): BOOLEAN;
BEGIN
  set(e,FALSE,TRUE);
  RETURN FALSE
END traps;

PROCEDURE exception(e: INTEGER): BOOLEAN;
BEGIN
  set(e,TRUE,FALSE);
  RETURN FALSE
END exception;

PROCEDURE trap(e: INTEGER): BOOLEAN;
BEGIN
  set(e,FALSE,FALSE);
  RETURN FALSE
END trap;

PROCEDURE raise(e: INTEGER);
  VAR prs: os.process; s: snare_ptr; x: trap_ptr; p: proc_ptr;
BEGIN
  prs:=os.active();
  s:=prs^.snares;
  IF (s=NIL) OR (s^.magic#MAGIC) THEN prs^.server:=NIL; ASSERT(FALSE,54h) END;
  x:=s^.traps;
  LOOP
    IF x=NIL THEN ASSERT(FALSE,54h); RETURN END;
    IF x^.soft & (x^.all OR (x^.id=e)) THEN EXIT END;
    p:=x^.L;
    p^.G :=x^.sG;
    p^.PC:=x^.sPC;
    s^.traps:=x^.next;
    x^.next:=s^.rems; s^.rems:=x;
    x:=s^.traps;
  END;
  IF x^.all THEN x^.exc^:=e END;
  transfer(dummy,slave);
END raise;

----------------------------------------------------------------

PROCEDURE server;
  VAR x: trap_ptr; s: snare_ptr; p: proc_ptr;
      a: sys.ADDRESS; ipted: os.PROCESS;
BEGIN
  LOOP
    a:=1;
    ipted:=a^;
    s:=ipted^.prs^.snares;
    x:=s^.traps; s^.traps:=x^.next;
    ipted^.G :=x^.G;
    ipted^.L :=x^.L;
    ipted^.PC:=x^.PC;
    (* push TRUE on ipted E-stack *)
    ipted^.S:=x^.S;
    ipted^.S^:=1;    INC(ipted^.S);
    ipted^.S^:=1;    INC(ipted^.S);
    IF (s^.traps=NIL) OR (s^.traps^.L#x^.L) THEN
      p:=x^.L;
      p^.G :=x^.sG;
      p^.PC:=x^.sPC;
    END;
    x^.next:=s^.rems; s^.rems:=x;
    transfer(dummy,ipted);
  END;
END server;

PROCEDURE on_trap;
  VAR x: trap_ptr; s: snare_ptr; p: proc_ptr;
      a: sys.ADDRESS; ipted,pp: os.PROCESS;
BEGIN
  LOOP
    a:=1;
    ipted:=a^;
    s :=ipted^.prs^.snares;
    pp:=ipted^.prs^.pp;
    IF (s#NIL) & (s^.magic=MAGIC) THEN
      x:=s^.traps;
      LOOP
        IF x=NIL THEN EXIT END;
        IF NOT x^.soft & (x^.all OR (x^.id=pp^.T)) THEN
          s^.traps:=x^.next;
          pp^.G :=x^.G;
          pp^.L :=x^.L;
          pp^.PC:=x^.PC;
          IF x^.all THEN x^.exc^:=pp^.T END;
          pp^.T:=0;
          (* push TRUE on ipted E-stack *)
          pp^.S:=x^.S;
          pp^.S^:=1;    INC(pp^.S);
          pp^.S^:=1;    INC(pp^.S);
          IF (s^.traps=NIL) OR (s^.traps^.L#x^.L) THEN
            p:=x^.L;
            p^.G :=x^.sG;
            p^.PC:=x^.sPC;
          END;
          x^.next:=s^.rems; s^.rems:=x;
          EXIT
        END;
        p:=x^.L;
        p^.G :=x^.sG;
        p^.PC:=x^.sPC;
        s^.traps:=x^.next;
        x^.next:=s^.rems; s^.rems:=x;
        x:=s^.traps;
      END;
    END;
    transfer(dummy,ipted);
  END;
END on_trap;

VAR
  wsp0 : ARRAY [0..63] OF INTEGER;
  wsp1 : ARRAY [0..63] OF INTEGER;

BEGIN
  os.new_process(server,sys.ADR(wsp0),SIZE(wsp0),slave);
  slave^.M:=slave^.M-{0,1};
  os.new_process(on_trap,sys.ADR(wsp1),SIZE(wsp1),ipts);
  ipts^.M:={};
END Exceptions.
