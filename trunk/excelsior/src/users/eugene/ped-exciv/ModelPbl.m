IMPLEMENTATION MODULE ModelPbl; (* Sem 11-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, WORD, ADDRESS;
IMPORT  mcd: defCodes;
IMPORT  os : osKernel;

VAR zero: POINTER TO os.PROCESS;

PROCEDURE TRANSFER(VAR from,to: os.PROCESS); CODE mcd.tra END TRANSFER;
PROCEDURE GETM():BITSET;                  CODE mcd.getm END GETM;
PROCEDURE SETM(m: BITSET);                CODE mcd.setm END SETM;
PROCEDURE MySelf(): ADDRESS;              CODE mcd.li0 mcd.lsw0 END MySelf;
PROCEDURE LLA(off: INTEGER): ADDRESS;     CODE mcd.lla 00 mcd.add END LLA;
PROCEDURE LGA(off: INTEGER): ADDRESS;     CODE mcd.lga 00 mcd.add END LGA;
PROCEDURE PushW(w: WORD);                 CODE END PushW;
PROCEDURE PopW(): WORD;                   CODE END PopW;
PROCEDURE MOVE(from,to: ADDRESS; n: INTEGER); CODE mcd.move END MOVE;
PROCEDURE llw3(): BITSET;  CODE mcd.llw 03 END llw3;
PROCEDURE slw3(m: BITSET); CODE mcd.slw 03 END slw3;

PROCEDURE Exception?(VAR e: Reaction): Exception;
  VAR ps: os.process;
BEGIN
  slw3(GETM()); SETM(llw3()-{0..1});
  ps:=os.active();
  e.Next :=ps^.snares;
  ps^.snares:=ADR(e);
  e.G_reg:=LGA(0);
  e.L_reg:=LLA(0);
  e.M_reg:=GETM();
  MOVE(ADR(e.PL),e.L_reg,6); (* Two lacal variable *)
  PushW(0);
  TRANSFER(zero^,zero^);
  e.PC_reg:=ps^.pp^.PC;
  SETM(llw3());
  RETURN Exception(PopW());
END Exception?;

VAR
  ExcNo  : Exception;
  Process: os.process;
  Wsp    : ARRAY [0..99] OF WORD;
  Demon  : os.PROCESS;

PROCEDURE Raise1();
  VAR i: pReaction; p: os.PROCESS;
BEGIN
  LOOP
    SETM(GETM()-{0,1});
    IF Process^.snares=NIL THEN RETURN END;
    i:=Process^.snares; Process^.snares:=i^.Next; p:=Process^.pp;
    p^.G :=i^.G_reg;
    p^.L :=i^.L_reg;
    p^.PC:=i^.PC_reg;
    p^.M :=i^.M_reg;
    p^.S :=i^.L_reg+6;
    MOVE(p^.L,ADR(i^.PL),6);
    p^.S^:=ExcNo;  INC(p^.S);
    p^.S^:=1;  INC(p^.S);
    IF Demon#p THEN
      os.stop(Process,FALSE);
  (*-----------------------------------------------*)
                Process^.pp:=p;  (* Маразм крепчал *)
  (*-----------------------------------------------*)
      os.start(Process);
    END;
    TRANSFER(Demon,Demon);
  END;
END Raise1;

PROCEDURE Raise(pr: os.process; n: Exception);
BEGIN
  Process:=pr; ExcNo:=n;
  TRANSFER(Demon,Demon);
END Raise;

PROCEDURE KillReaction(VAR e: Reaction);
  VAR ps: os.process;
BEGIN
  ps:=os.active();
  ASSERT(ADR(e)=ps^.snares);
  ps^.snares:=e.Next;
END KillReaction;

PROCEDURE RaiseInMe(n: Exception);
  VAR ps: os.process;
BEGIN
  ps:=os.active();
  IF ps^.snares=NIL THEN HALT(1) END;
  Raise(ps,n);
END RaiseInMe;

BEGIN
  zero:=ADDRESS(0);
  os.new_process(Raise1,ADR(Wsp),SIZE(Wsp),Demon);
  Message:='CRASH';
END ModelPbl.
