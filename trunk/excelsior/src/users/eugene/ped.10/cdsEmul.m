IMPLEMENTATION MODULE cdsEmul; (* 21-Jul-86 (c)KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Model      IMPORT  Objects, Lsize, Lget, Iterate,
                        Object, Tag, Poz, setPoz;
FROM Terminal   IMPORT  print, Read;
FROM Args       IMPORT  Flag?;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM cdsEmulLib IMPORT  InitEmulation;

CONST MaxIvent=1999;
      MemOvr='Ivents table overflow, sorry...\n';

TYPE Ivent=POINTER TO IventRec;
     IventRec=RECORD
       fwd     : Ivent;
       pTime   : INTEGER;
       mTime   : INTEGER;
       state   : BOOLEAN;
       sig     : Object;
       pin     : INTEGER;
     END;

VAR Wsp : ARRAY [0..MaxIvent] OF IventRec;
    Free: ARRAY [0..MaxIvent] OF Ivent;
    FreePtr: INTEGER;
    Head: Ivent;
    Next: Ivent;
    Chip: Object;
    PinProc: PROCEDURE (Object);
    mlrd: INTEGER;

PROCEDURE NewIvent(pinno,time: INTEGER; state: BOOLEAN);
  VAR p,o: Object; r: BOOLEAN;
      iv,hd,e: Ivent; ahd: POINTER TO Ivent;
BEGIN
  IF pinno>0 THEN
    p:=Lget(Chip^.Pins,pinno-1);
    o:=p^.Signal; IF o=NIL THEN RETURN END;
    IF o^.Name='..free..' THEN RETURN END;
    IF FreePtr=0 THEN print(MemOvr); HALT END;
    DEC(FreePtr); iv:=Free[FreePtr];
    iv^.sig:=o;
    iv^.pin:=1;
  ELSE
    IF FreePtr=0 THEN print(MemOvr); HALT END;
    DEC(FreePtr); iv:=Free[FreePtr];
    iv^.sig:=Chip;
    iv^.pin:=pinno;
  END;
  iv^.state:=state;
  iv^.pTime:=TMps+time;
  iv^.mTime:=TMms;
  WHILE iv^.pTime>=mlrd DO DEC(iv^.pTime,mlrd); INC(iv^.mTime) END;
  hd:=Head; ahd:=ADR(Head);
  LOOP
    IF hd=NIL THEN EXIT END;
    IF BITSET(hd^.mTime>iv^.mTime)+
       BITSET(hd^.mTime=iv^.mTime)*BITSET(hd^.pTime>=iv^.pTime)#{} THEN
      EXIT;
    END;
    ahd:=ADR(hd^.fwd); hd:=hd^.fwd;
  END;
  iv^.fwd:=hd; ahd^:=iv;
END NewIvent;

PROCEDURE State?(n: INTEGER): BOOLEAN;
BEGIN
  RETURN BOOLEAN(Lget(Chip^.Pins,n-1)^.State);
END State?;

PROCEDURE InitPin(p: Object);
BEGIN
  ASSERT(Tag(p)=pin); p^.State:=INTEGER(Undef);
END InitPin;

VAR PinState: BOOLEAN;

PROCEDURE CalcNext(p: Object);
  VAR pr: JobPROC;
BEGIN
  Chip:=p^.Chip;
  pr:=JobPROC(Chip^.cBefor);
  pr(p^.No+1,PinState);
END CalcNext;

PROCEDURE PinSt(p: Object);
BEGIN
  ASSERT(Tag(p)=pin);
  p^.State:=INTEGER(PinState);
END PinSt;

PROCEDURE SetPin(no: INTEGER; st: BOOLEAN);
  VAR i: BOOLEAN; c,p: Object;
BEGIN
  p:=Lget(Chip^.Pins,no-1); ASSERT(p#NIL);
  IF Flag?('c') THEN
    print('set pin %s %d to %d\n',Chip^.Name,no,st);
  END;
  IF BOOLEAN(p^.State)#st THEN
    i:=PinState; c:=Chip; PinState:=st;
    IF p^.Signal^.Name#'..free..' THEN
      Iterate(p^.Signal^.TiedPins,PinSt);
      Iterate(p^.Signal^.TiedPins,CalcNext);
    END;
    PinState:=i; Chip:=c;
  END;
END SetPin;

PROCEDURE CalcPin(p: Object);
  VAR pr: JobPROC;
BEGIN
  ASSERT(Tag(p)=pin);
  IF BOOLEAN(p^.State)=Undef THEN
    p^.State:=INTEGER(p^.Signal^.Name#'GND');
    Chip:=p^.Chip;
    pr:=JobPROC(Chip^.cBefor);
    pr(p^.No+1,TRUE);
  END;
END CalcPin;

PROCEDURE CreIvent(p: Object);
  VAR pr: JobPROC;
BEGIN
  Chip:=p^.Chip;
  pr:=JobPROC(Chip^.cInit);
  pr(p^.No+1,BOOLEAN(p^.State));
END CreIvent;

PROCEDURE ShowPin(p: Object);
BEGIN
  print('%5s %2d - %d\n',p^.Chip^.Name,p^.No+1,p^.State);
END ShowPin;

PROCEDURE AllPin(s: Object);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  Iterate(s^.TiedPins,PinProc);
END AllPin;

VAR chain: POINTER TO ARRAY [0..0] OF Object;
    cptr,csz: INTEGER;

PROCEDURE CrePin(p: Object);
BEGIN
  (*$T-*) chain^[cptr]:=p; INC(cptr); (*$T+*)
END CrePin;

PROCEDURE CreChain(s: Object);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  setPoz(s,cptr);
  Iterate(s^.TiedPins,CrePin);
  ASSERT(cptr<csz);
  (*$T-*) chain^[cptr]:=NIL; INC(cptr); (*$T+*)
END CreChain;

PROCEDURE ClcPin(p: Object);
BEGIN
  INC(csz);
END ClcPin;

PROCEDURE ClcChain(s: Object);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  Iterate(s^.TiedPins,ClcPin);
  INC(csz);
END ClcChain;

PROCEDURE CalculateState(o: Object);
  VAR ch: CHAR;
BEGIN
  InitEmulation(o);
  PinProc:=InitPin;  Iterate(o^.All,AllPin); -- for all pins State:=-1
  PinProc:=CalcPin;  Iterate(o^.All,AllPin);
  IF Flag?('f') THEN
    PinProc:=ShowPin; Iterate(o^.All,AllPin);
    print('Press any key for continue...\r'); ch:=Read(); print('\n');
  END;
  PinProc:=CreIvent; Iterate(o^.All,AllPin);
  IF csz>0 THEN Deallocate(chain,csz) END;
  csz:=0; Iterate(o^.All,ClcChain);
  Allocate(chain,csz);
  cptr:=0; Iterate(o^.All,CreChain);
END CalculateState;

PROCEDURE Emulate(o: Object);
  VAR pr: JobPROC; n: INTEGER; p: Object;
BEGIN
  Stop:=FALSE;
  WHILE (Head#NIL)&(NOT Stop) DO
    Next:=Head; Head:=Next^.fwd;
    TMps:=Next^.pTime; TMms:=Next^.mTime;
    IF Next^.pin>0 THEN
      n:=Poz(Next^.sig);
      (*$T-*)
      WHILE chain^[n]#NIL DO
        p:=chain^[n]; INC(n);
        IF BOOLEAN(p^.State)#Next^.state THEN
          Chip:=p^.Chip;
          pr:=JobPROC(Chip^.cAfter);
          Value:=Chip^.cValue;
          pr(p^.No+1,Next^.state);
          p^.State:=INTEGER(Next^.state);
        END;
      END;
      (*$T+*)
    ELSE
      Chip:=Next^.sig;
      pr:=JobPROC(Chip^.cAfter);
      pr(Next^.pin,Next^.state);
    END;
    Free[FreePtr]:=Next; INC(FreePtr);
  END;
END Emulate;

BEGIN
  Head:=NIL; TMps:=0; TMms:=0; mlrd:=1000000000;
  FOR FreePtr:=0 TO MaxIvent DO Free[FreePtr]:=ADR(Wsp[FreePtr]) END;
  FreePtr:=MaxIvent+1; csz:=0;
END cdsEmul.
