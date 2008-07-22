MODULE THG; (*$T- Sem 02-Jun-87. (c) KRONOS *)

-- Драйвер для работы с принтером Toshiba
-- в режиме поточечной графики.

FROM Qbus       IMPORT  QIN, QOUT;
FROM SYSTEM    IMPORT   WORD, ADR, ADDRESS, TRANSFER, PROCESS;
FROM KRONOS    IMPORT   SETM, GETM, MOVE;
FROM Scheduler IMPORT   Gate, Signal, ProcessId, MyTask,
                        InitGate, EnterGate, ExitGate,
                        Send, Wait, InitSignal, ?1, ?0, Wait?1;
FROM FsDrv     IMPORT   IamDriver, dMode;
FROM FsPublic  IMPORT   NoSuchCFE, NoSuchDrive;
FROM ASCII     IMPORT   CR, LF, NL, FF, BS;
FROM mCodeMnem IMPORT   tr;
FROM Universe  IMPORT   NewPrs;
FROM Ipts      IMPORT   OnIpt;

TYPE Buffer=ARRAY [0..4095] OF CHAR;

CONST ok = FALSE;

VAR
  iCSR,oCSR,iDTR,oDTR: CARDINAL;
  iTrap,oTrap     : CARDINAL;
  Mode            : BITSET;
  oBegin,  oEnd   : CARDINAL;
  oBuf            : Buffer;
  Activ           : BOOLEAN;
  oSignal         : Signal;
  MyName          : ARRAY [0..3] OF CHAR;
  oGate           : Gate;
  Look            : Gate;
  OutEmpty        : BOOLEAN;
  Input,Output    : PROCESS;
  iIpted,oIpted   : PROCESS;

CONST (* Моды *)
  outXoff = 0;
  crp     = 1;

PROCEDURE TR(VAR w: BOOLEAN): BOOLEAN;
CODE tr END TR;

PROCEDURE Keyboard;
  VAR Ch: CHAR;
BEGIN
  LOOP
    Ch:=CHAR(BITSET(QIN(iDTR))*{0..4});
    IF Ch=23c THEN
      INCL(Mode,outXoff);
    ELSIF Ch=21c THEN
      EXCL(Mode,outXoff); QOUT(oCSR,40h);
    END;
    TRANSFER(Input,iIpted);
  END;
END Keyboard;

PROCEDURE Screen;
  VAR Ch: CHAR;
BEGIN
  LOOP
    WHILE OutEmpty DO QOUT(oCSR,0h); TRANSFER(Output,oIpted) END;
    REPEAT
      WHILE outXoff IN Mode DO QOUT(oCSR,0h); TRANSFER(Output,oIpted) END;
      QOUT(oDTR,oBuf[oBegin]); INC(oBegin);
      IF BOOLEAN({7}*BITSET(QIN(iCSR))) THEN
        Ch:=CHAR(BITSET(QIN(iDTR))*{0..4});
        IF Ch=23c THEN
          INCL(Mode,outXoff); QOUT(oCSR,0h);
        ELSIF Ch=21c THEN
          EXCL(Mode,outXoff)
        END;
      END;
      TRANSFER(Output,oIpted);
    UNTIL oBegin=oEnd;
    Send(oSignal); OutEmpty:=TRUE;
  END;
END Screen;

TYPE Ptr=POINTER TO Buffer;

PROCEDURE DrvRd(dev,block: CARDINAL; s: Ptr; l: CARDINAL): BOOLEAN;
BEGIN
  RETURN NoSuchDrive
END DrvRd;

PROCEDURE DrvWr(dev,block: CARDINAL; s: Ptr; l: CARDINAL): BOOLEAN;
BEGIN
  IF NOT Activ THEN RETURN NoSuchDrive END;
  IF (l<0)OR(l>4096) THEN RETURN TRUE END;
  IF l=0 THEN RETURN ok END;
  EnterGate(oGate);
  Wait?1(oSignal);
  oBuf:=s^; oBegin:=0; oEnd:=l; OutEmpty:=FALSE; QOUT(oCSR,40h);
  ExitGate(oGate); ?0;
  RETURN ok
END DrvWr;

VAR iWsp,oWsp: ARRAY [0..99] OF CARDINAL;
    Sv: ARRAY [0..5] OF INTEGER;

PROCEDURE InitTT;
  VAR i: INTEGER; a: ADDRESS;
BEGIN
  EnterGate(oGate);
  Mode:={};
  OutEmpty:=TRUE;
  oBegin:=0; oEnd:=0; InitSignal(oSignal); Send(oSignal);
  SETM(GETM()-{0,1});
  a:=ADDRESS(iTrap*2); Sv[0]:=a^; INC(a); Sv[1]:=a^;
  a:=ADDRESS(oTrap*2); Sv[2]:=a^; INC(a); Sv[3]:=a^;
  Sv[4]:=QIN(iCSR); Sv[5]:=QIN(oCSR);
  NewPrs(Keyboard,ADR(iWsp),SIZE(iWsp),Input);
  NewPrs(Screen,ADR(oWsp),SIZE(oWsp),Output);
  OnIpt(iIpted,Input,iTrap);
  OnIpt(oIpted,Output,oTrap);
  QOUT(oCSR,40h); QOUT(iCSR,40h);
  ExitGate(oGate);
  Activ:=TRUE;
END InitTT;

PROCEDURE ClearTT;
  VAR a: ADDRESS;
BEGIN
  EnterGate(oGate);
  Activ:=FALSE;
  SETM(GETM()-{0..1});
  a:=ADDRESS(iTrap*2); a^:=Sv[0]; INC(a); a^:=Sv[1];
  a:=ADDRESS(oTrap*2); a^:=Sv[2]; INC(a); a^:=Sv[3];
  QOUT(iCSR,Sv[4]); QOUT(oCSR,Sv[5]);
  ExitGate(oGate);
END ClearTT;

VAR MemStart: ADDRESS;
    MemSize : CARDINAL;

PROCEDURE DrvCFE(dev,op: CARDINAL; info: ADDRESS): BOOLEAN;
  VAR s: Ptr; i,d: INTEGER;
BEGIN
  CASE op OF
      0: info^:=0
    | 4: InitTT;
    | 8: RETURN DrvRd(0,0,info,1);
    | 9: RETURN DrvWr(0,0,ADR(info),1);
    |10: ClearTT;
    |11: info^:=MemStart; INC(info); info^:=MemSize;
    |20: EnterGate(Look);
    |21: ExitGate(Look);
  ELSE RETURN NoSuchCFE;
  END;
  RETURN ok
END DrvCFE;

VAR p : ProcessId;
    info: POINTER TO
            RECORD
              Name : ARRAY [0..3] OF CHAR;
              TermN: INTEGER;
            END;

BEGIN
  Activ:=FALSE;
  InitGate(oGate);
  InitGate(Look);
  p:=MyTask();
  MemStart:=p^.MemStart;
  MemSize :=p^.MemSize;
  iCSR:=176500b DIV 2; iTrap:=300b DIV 4;
  iDTR:=iCSR+1; oCSR:=iCSR+2; oDTR:=iCSR+3; oTrap:=iTrap+1;
  info:=p^.ParmLink;
  MyName:=info^.Name; MyName[2]:=0c;
  IF IamDriver(MyName,{0},serial,DrvRd,DrvWr,DrvCFE) THEN HALT(1) END;
END THG.
