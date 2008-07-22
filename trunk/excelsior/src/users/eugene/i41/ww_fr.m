MODULE ww_fr;

--        Инсталяция:  &ww_fr tt 0

FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Scheduler  IMPORT  Gate, Signal, ProcessId, MyTask, Sleep,
                        InitGate, EnterGate, ExitGate, Send, new_process,
                        SendUp, Wait, InitSignal, Awaited, ?1, ?0, Wait?1,
                        InsertAction, RemoveAction;
FROM FsDrv      IMPORT  IamDriver, dMode, RemDriver;
FROM TTYs       IMPORT  IamTerminal;
FROM ASCII      IMPORT  CR, LF, NL;
FROM FsPublic   IMPORT  VisFSerr, NoSuchCFE;
FROM Resource   IMPORT  Final;
IMPORT Image, mcd: mCodeMnem, cnf: osConfig;

PROCEDURE SETM(m: BITSET); CODE mcd.setm END SETM;
PROCEDURE GETM(): BITSET;  CODE mcd.getm END GETM;

CONST
  oBufSize =  256;
  iBufSize =  256;

TYPE
  oBuffer  = ARRAY [0..oBufSize-1] OF CHAR;
  iBuffer  = ARRAY [0..iBufSize-1] OF CHAR;
  block_ref= POINTER TO ARRAY [0..4095] OF CHAR;

VAR
  Mode            : BITSET;
  iBegin,  iEnd   : INTEGER;
  oBegin,  oEnd   : INTEGER;
  iBuf            : iBuffer;
  oBuf            : oBuffer;
  iSignal, oSignal: Signal;
  Press           : Signal;
  iGate           : Gate;
  oGate           : Gate;
  TermType        : INTEGER;
  TermNo          : INTEGER;
  Abort           : POINTER TO Signal;
  MyName          : ARRAY [0..3] OF CHAR;
  ESC             : BOOLEAN;

CONST (* Моды терминала: *)
  InpKr   = 0;    OutKr  = 1;
  ctrl    = 2;    pend   = 3;
  caplock = 4;    edit   = 5;
  Фрящ    = 6; (* reduce ESC ^S to ESC S *)
  in8bit  = 7;    out8bit= 8;

TYPE
  cs=SET OF (TxEN,RxIE,RxEN,SBRK,ER,TxIE,RESET,EH);
  ss=SET OF (TxRDY,RxRDY,TxE,PE,OE,FE,SINDET,DSR);

CONST
  csr  = 0Ah<<30;
  dtr  = 08h<<30;
  tcmd = 1Eh<<30;
  tcnt = 18h<<30;

VAR
  cmd : cs;

CONST
(* MODE *)
  x1  = {0};            bits5 = {};             odd     = {4};
  x16 = {1};            bits6 = {2};            even    = {4,5};
  x64 = {0,1};          bits7 = {3};            stop1   = {6};
                        bits8 = {2,3};          stop1_5 = {7};
                                                stop2   = {6,7};

PROCEDURE inp(n: INTEGER): WORD; CODE 92h END inp;
PROCEDURE out(n: INTEGER; v: WORD); CODE 93h END out;

PROCEDURE Keyboard;
  VAR Ch: CHAR; NewEnd,x: INTEGER; r: BOOLEAN;
BEGIN
  IF RxRDY IN ss(inp(csr)) THEN
    Ch:=CHAR(inp(dtr));
    IF OE IN ss(inp(csr)) THEN ESC:=TRUE END;
    out(csr,cmd+cs{ER});
    IF in8bit IN Mode THEN
      r:=TRUE
    ELSIF ESC THEN
      ESC:=FALSE; Ch:=CHAR(BITSET(Ch)*{0..4}+{7}); r:=TRUE;
    ELSE
      Ch:=CHAR(BITSET(Ch)*{0..6});
      CASE Ch OF
        |017c: EXCL(Mode,InpKr); r:=FALSE
        |016c: INCL(Mode,InpKr); r:=FALSE
        |003c: r:=edit IN Mode;
               IF NOT r & (Abort#NIL) THEN Send(Abort^) END
        |006c: Mode:=Mode/{caplock}; r:=FALSE
        |021c: r:=NOT (ctrl IN Mode) OR NOT (pend IN Mode);
               IF NOT r THEN EXCL(Mode,pend) END;
        |023c: r:=NOT (ctrl IN Mode) OR (pend IN Mode);
               IF NOT r THEN INCL(Mode,pend) END;
        |031c: Ch:=033c;
        |033c: ESC:=TRUE; r:=FALSE;
      ELSE
        r:=TRUE;
        IF (Ch>=100c) & (Ch<177c) THEN
          IF (InpKr IN Mode) # (OutKr IN Mode) THEN
            Ch:=CHAR(BITSET(Ch)/{5})
          END;
          IF caplock IN Mode THEN
            x:=ORD(CHAR(BITSET(Ch)-{5}))-ORD('A');
            IF (InpKr IN Mode) OR (x IN {0..25}) THEN
              Ch:=CHAR(BITSET(Ch)/{5})
            END;
          END;
          IF InpKr IN Mode THEN Ch:=CHAR(BITSET(Ch)+{7}) END;
        END;
      END;
    END;
    IF r THEN
      NewEnd:=(iEnd+1) MOD iBufSize;
      IF iBegin # NewEnd THEN
        iBuf[iEnd]:=Ch; iEnd:=NewEnd; Send(iSignal);
      END;
      Send(Press);
    END;
  END
END Keyboard;

PROCEDURE put_char(ch: CHAR);
  PROCEDURE put(c: CHAR);
  BEGIN
    WHILE pend IN Mode DO Sleep END;
    REPEAT UNTIL TxRDY IN ss(inp(csr));
    out(dtr,c);
    IF c=17c THEN EXCL(Mode,OutKr) END;
    IF c=16c THEN INCL(Mode,OutKr) END;
  END put;
BEGIN
  IF out8bit IN Mode THEN
    put(ch);
  ELSIF ch=NL THEN
    put(CR); put(LF);
  ELSIF (OutKr IN Mode)#(ch>=300c) THEN
    IF ch>=300c THEN put(16c) ELSE put(17c) END;
    put(CHAR(BITSET(ch)*{0..6}));
  ELSE
    put(CHAR(BITSET(ch)*{0..6}));
  END;
END put_char;

PROCEDURE DrvRd(dev,block: INTEGER; s: block_ref; l: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  EnterGate(iGate);
  FOR i:=0 TO l-1 DO
    Wait?1(iSignal);
    s^[i]:=iBuf[iBegin]; iBegin:=(iBegin+1) MOD iBufSize;
    ?0;
  END;
  ExitGate(iGate);
  RETURN FALSE;
END DrvRd;

PROCEDURE DrvWr(dev,block: INTEGER; s: block_ref; l: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  EnterGate(oGate);
  ?1;
  i:=0;
  LOOP
    IF (i>=l) OR (s^[i]=0c) THEN EXIT END;
    put_char(s^[i]); INC(i);
  END;
  ExitGate(oGate);
  ?0;
  RETURN FALSE;
END DrvWr;

PROCEDURE InitTT;
  VAR i,n: INTEGER; r: BOOLEAN; a: ADDRESS;
BEGIN
  iBegin:=0; iEnd:=0;   InitGate(iGate);   InitSignal(iSignal);
  oBegin:=0; oEnd:=0;   InitGate(oGate);   InitSignal(oSignal);
  TermType:=1;
  InitSignal(Press);
  FOR i:=0 TO oBufSize-3 DO Send(oSignal) END;
  Mode:={ctrl,caplock};
  ESC:=FALSE;
  out(tcmd,{4,5,2,1});
  out(tcnt,13);
  out(tcnt,0);
  out(csr,{});
  out(csr,{});
  out(csr,{});
  out(csr,{});
  out(csr,cs{RESET});
  out(csr,x16+stop2+bits7);
  cmd:=cs{RxEN,RxIE,TxEN};
  out(csr,cmd);
  IF InsertAction(Keyboard) THEN HALT(1) END;
END InitTT;

PROCEDURE DrvCFE(dev,op: INTEGER; info: ADDRESS): BOOLEAN;
  VAR s: block_ref; d,i: INTEGER; r: BOOLEAN;
BEGIN
  CASE op OF
      0: info^:=0
    | 1: IF info#NIL THEN s:=info; s^:=MyName END;
    | 4: Abort:=NIL;
         IF info#NIL THEN s:=info; s^:=MyName END;
         InitTT;
    | 8: r:=DrvRd(0,0,info,1);
    | 9: r:=DrvWr(0,0,ADR(info),1);
    |19: EnterGate(iGate); InitSignal(Press); Wait(Press); ExitGate(iGate);
    |21: INCL(Mode,INTEGER(info^))
    |22: EXCL(Mode,INTEGER(info^));
         IF INTEGER(info^)=ctrl THEN EXCL(Mode,pend) END
    |23: EnterGate(iGate);
         d:=-INTEGER(iSignal);
         IF d<1 THEN d:=0 END;
         info^:=d;
         ExitGate(iGate)
    |28: EnterGate(iGate); i:=info^;
         IF INTEGER(iSignal)>=0 THEN
           info^:=0; INC(info); info^:=0;
           ExitGate(iGate); RETURN FALSE;
         ELSE
           d:=-ORD(INTEGER(iSignal)); info^:=d;
         END;
         INC(info);
         IF i>d THEN
           info^:=0;
         ELSE
           i:=(iBegin+i) MOD iBufSize;
           info^:=iBuf[i];
         END;
         ExitGate(iGate)
    |24: info^:=Abort
    |25: Abort:=info;
    |26: info^:=TermType;
    |27:
   ELSE RETURN NoSuchCFE;
  END;
  RETURN FALSE;
END DrvCFE;

PROCEDURE parms;
  PROCEDURE default; BEGIN TermNo:=0 END default;
  VAR
    ps : POINTER TO ARRAY [0..255] OF CHAR;
    p  : ProcessId;
    i,j: INTEGER;
BEGIN
  p:=MyTask();
  i:=0; j:=0; ps:=p^.ParmLink;
  WHILE (j<HIGH(ps^)) & (ps^[j]=' ') DO INC(j) END;
  WHILE (i<HIGH(MyName)) & (j<=HIGH(ps^)) & (ps^[j]#0c) & (ps^[j]#' ') DO
    MyName[i]:=ps^[j]; INC(j); INC(i);
  END;
  MyName[2]:=0c;
  j:=Image.PeekNum(ps^,j,TermNo);
  IF j<0 THEN default; RETURN END;
END parms;

PROCEDURE instal;
  VAR
    r    : BOOLEAN;
    s    : ARRAY [0..79] OF CHAR;
BEGIN
  r:=IamDriver(MyName,{TermNo},serial,DrvRd,DrvWr,DrvCFE);
  IF r THEN VisFSerr(r,s); cnf.debug(s); HALT(1) END;
  r:=IamTerminal(TermNo,MyName);
  IF r THEN VisFSerr(r,s); cnf.debug(s); HALT(1) END;
END instal;

PROCEDURE stop;
BEGIN
  RemoveAction(Keyboard);
  IF RemDriver(MyName) THEN END;
END stop;

VAR
  never: Signal;

BEGIN
  parms;
  Final(stop);
  instal;
  InitSignal(never);
  Wait(never)
END ww_fr.
