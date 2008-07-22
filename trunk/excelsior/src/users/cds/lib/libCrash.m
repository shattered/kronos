IMPLEMENTATION MODULE libCrash; (* Sem 11-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, WORD, ADDRESS;

IMPORT  mcd: defCodes;
IMPORT  os : osKernel;
IMPORT  str: Strings;

VAR
  zero  : POINTER TO os.PROCESS;
  wsp   : ARRAY [0..99] OF WORD;
  demon : os.PROCESS;
  ipted : os.PROCESS;

PROCEDURE TRAP(n: WORD; VAR from,to: os.PROCESS); CODE mcd.tra END TRAP;
PROCEDURE TRANSFER(VAR from,to: os.PROCESS); CODE mcd.tra END TRANSFER;
PROCEDURE GETM():BITSET; CODE mcd.getm END GETM;
PROCEDURE SETM(m: BITSET); CODE mcd.setm END SETM;
PROCEDURE MYSELF(): ADDRESS; CODE mcd.li0 mcd.lsw0 END MYSELF;
PROCEDURE LLA(off: INTEGER): ADDRESS; CODE mcd.lla 00 mcd.add END LLA;
PROCEDURE LGA(off: INTEGER): ADDRESS; CODE mcd.lga 00 mcd.add END LGA;
PROCEDURE PUSH(w: WORD); CODE END PUSH;
PROCEDURE POP(): WORD; CODE END POP;
PROCEDURE MOVE(from,to: ADDRESS; n: INTEGER); CODE mcd.move END MOVE;

PROCEDURE enter(VAR e: trap): BOOLEAN;
  VAR ei: BITSET;
BEGIN
  ei:=GETM(); SETM(ei-{0..1});
  e.pp  :=MYSELF();
  e.prs :=os.active();
  e.next:=e.prs^.snares;
  e.prs^.snares:=ADR(e);
  e.gate:=e.prs^.mutex;
  e.prs^.mutex:=NIL;
  e.G:=LGA(0);
  e.L:=LLA(0);
  e.M:=GETM();
  MOVE(ADR(e.PL),e.L,6); (* Two lacal variable *)
  TRAP(0,zero^,zero^);
  e.PC:=e.prs^.pp^.PC;
  SETM(ei);
  RETURN POP();
END enter;

PROCEDURE demon_proc();
  VAR i: trap_ref; p: os.PROCESS; ps: os.process; h: os.signal_ptr;
BEGIN
  LOOP
    DEC(ipted^.S); ASSERT(ipted^.S^=1);
    DEC(ipted^.S); ps:=ipted^.S^;
    ipted^.S^:=0;  INC(ipted^.S);
    i:=ps^.snares; ps^.snares:=i^.next;
    h:=ps^.halt;
    ps^.halt:=NIL;
    ps^.status:=os._suspended;
    os.stop(ps,FALSE);
    ps^.status:=os._new;
    ps^.halt:=h;
    ps^.mutex:=i^.gate;
    os.start(ps);
    p:=i^.pp;
    p^.G :=i^.G;
    p^.L :=i^.L;
    p^.PC:=i^.PC;
    p^.M :=i^.M;
    p^.S :=i^.L+6;
    MOVE(p^.L,ADR(i^.PL),6);
    p^.S^:=1;  INC(p^.S);
    p^.S^:=1;  INC(p^.S);
    ps^.pp:=p;
    ps:=os.active();
    TRANSFER(demon,ps^.pp);
  END;
END demon_proc;

PROCEDURE raise(fmt: ARRAY OF CHAR; SEQ x: WORD);
  VAR p: trap_ref; ps: os.process;
BEGIN
  ps:=os.active();
  p:=ps^.snares;
  IF p=NIL THEN HALT(50h) END;
  str.print(p^.txt,fmt,x);
  TRAP(ps,ipted,demon);
END raise;

PROCEDURE re_raise(VAR e: trap);
  VAR ps: os.process; p: trap_ref;
BEGIN
  ps:=os.active();
  p:=ps^.snares;
  IF p=NIL THEN HALT(50h) END;
  p^.txt:=e.txt;
  TRAP(ps,ipted,demon);
END re_raise;

PROCEDURE exit(VAR e: trap);
  VAR ps: os.process;
BEGIN
  ps:=os.active();
  ASSERT(ADR(e)=ps^.snares);
  ps^.snares:=e.next;
  ASSERT(ps^.mutex=NIL);
  ps^.mutex:=e.gate;
END exit;

VAR ei: BITSET;

BEGIN
  zero:=ADDRESS(0);
  ei:=GETM(); SETM(ei-{0,1});
  os.new_process(demon_proc,ADR(wsp),SIZE(wsp),demon);
  SETM(ei);
END libCrash.
