IMPLEMENTATION MODULE ModelPbl; (* Sem 11-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, TRANSFER, PROCESS, WORD, ADDRESS;
FROM Universe  IMPORT   ProcessDesc, NewPrs;
FROM Scheduler IMPORT   MyTask, Untie, Start, Signal, ProcessId;
FROM KRONOS    IMPORT   GETM, SETM, LLA, LGA, PushW, PopW, MySelf, MOVE;

VAR zero: POINTER TO PROCESS;

PROCEDURE Exception?(VAR e: Reaction): Exception;
  VAR ps: ProcessId;
BEGIN
  SETM(GETM()-{0,1});       ps:=MyTask();
  e.Next:=ps^.ExceptLink;   ps^.ExceptLink:=ADR(e);
  e.G_reg:=LGA(0);          e.L_reg:=LLA(0);
  e.M_reg:=GETM();
  MOVE(ADR(e.PL),e.L_reg,6); (* Two lacal variable *)
  PushW(0);
  TRANSFER(zero^,zero^);
  e.PC_reg:=ProcessDesc(ps^.pp)^.PC_reg;
  RETURN Exception(PopW());
END Exception?;

VAR ExcNo: Exception;
    Process: ProcessId;
    Wsp: ARRAY [0..99] OF WORD;
    Demon: PROCESS;

PROCEDURE Raise1();
  VAR i: pReaction; p: ProcessDesc;
BEGIN
  LOOP
    SETM(GETM()-{0,1});
    IF Process^.ExceptLink=NIL THEN RETURN END;
    i:=Process^.ExceptLink; Process^.ExceptLink:=i^.Next; p:=Process^.pp;
    p^.G_reg:=i^.G_reg;      p^.L_reg:=i^.L_reg;
    p^.PC_reg:=i^.PC_reg;    p^.M_reg:=i^.M_reg;
    p^.S_reg:=i^.L_reg+6;
    MOVE(p^.L_reg,ADR(i^.PL),6);
    p^.S_reg^:=ExcNo;  INC(p^.S_reg);
    p^.S_reg^:=1;  INC(p^.S_reg);
    IF Demon#p THEN
      Process:=ProcessId(Untie(Signal(Process)));
  (*-----------------------------------------------*)
                     Process^.pp:=p;  (* Маразм крепчал *)
  (*-----------------------------------------------*)
      Start(Process);
    END;
    TRANSFER(Demon,Demon);
  END;
END Raise1;

PROCEDURE Raise(pr: ProcessId; n: Exception);
BEGIN
  Process:=pr; ExcNo:=n;
  TRANSFER(Demon,Demon);
END Raise;

PROCEDURE KillReaction(VAR e: Reaction);
  VAR ps: ProcessId;
BEGIN ps:=MyTask();
  ASSERT(ADR(e)=ps^.ExceptLink);
  ps^.ExceptLink:=pReaction(ps^.ExceptLink)^.Next;
END KillReaction;

PROCEDURE RaiseInMe(n: Exception);
  VAR ps: ProcessId;
BEGIN
  ps:=MyTask();
  IF ps^.ExceptLink=NIL THEN HALT(1) END;
  Raise(ps,n);
END RaiseInMe;

BEGIN
  zero:=ADDRESS(0);
  NewPrs(Raise1,ADR(Wsp),SIZE(Wsp),Demon);
END ModelPbl.
