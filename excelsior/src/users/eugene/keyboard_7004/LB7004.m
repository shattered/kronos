MODULE LB7004; (*$T- Sem 02-Jun-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   WORD, ADR, ADDRESS;
FROM KRONOS    IMPORT   SETM, GETM, MOVE;
FROM Scheduler IMPORT   Gate, Signal, ProcessId, MyTask,
                        InitGate, EnterGate, ExitGate,
                        Send, Wait, InitSignal, ?1, ?0, Wait?1,
                        InsertAction, RemoveAction;
FROM FsDrv     IMPORT   IamDriver, dMode;
FROM TTYs      IMPORT   IamTerminal;
FROM ASCII     IMPORT   CR, LF, NL;
FROM E7004     IMPORT   Ch, get;
FROM mCodeMnem IMPORT   liw, add, ror, rol, lgw2, lgw3, lxb, sxb, lib, li1,
                        li2, li3, li0E, li0F, copt, stot, jbsc, lodt,
                        li0, li8, li5, li4, li0C, lgw4;

CONST
  i_free     =   0; (* byte   1 => free ; 0 => busy *)
  i_stopped  =   1;
  i_in       =   2; (* word   next char goes here   *)
  i_out      =   4; (* word   next char from here   *)
  i_size     =   6; (* word   buffer size           *)
  i_buff_ptr =   8; (* word   16 bit offset adr of sys buffer *)
  i_buff_end =  10; (* word   offset of last byte in buffer   *)
  o_free     =  12; (* byte   1 => free ; 0 => busy *)
  o_stopped  =  13;
  o_in       =  14; (* word   next char goes here   *)
  o_out      =  16; (* word   next char from here   *)
  o_size     =  18; (* word   buffer size           *)
  o_buff_ptr =  20; (* word   16 bit offset adr of sys buffer *)
  o_buff_end =  22; (* word   offset of last byte in buffer   *)

  unknown=0; Э15_013=1; labtam=2;

  (*     abcdefghijklmnopqrstuvwxyz  *)
  QWERT="фисвуапршолдьтщзйкыегмцчня";

  iBufSize=512;

TYPE
  Buffer = ARRAY [0..iBufSize-1] OF CHAR;
  Block  = POINTER TO ARRAY [0..4095] OF CHAR;

VAR
  BCB_i_byte_adr  : ADDRESS;
  BCB_o_byte_adr  : ADDRESS;
  BCB_word_adr    : POINTER TO ARRAY [0..0FFh] OF CHAR;

  Mode: BITSET;
  segF000         : POINTER TO ARRAY [0..0FFFFh] OF CHAR;
  iBegin          : INTEGER;
  iEnd            : INTEGER;
  iEndNext        : INTEGER;
  iBuf            : Buffer;
  oBegin,  oEnd   : INTEGER;
  oFull           : INTEGER;
  OutActiv        : BOOLEAN;
  oBuf            : ARRAY [0..4096*2-1] OF CHAR;
  optr,oend,oinp  : INTEGER;
  iptr,iend,iout  : INTEGER;
  Activ           : BOOLEAN;
  Abort           : POINTER TO Signal;
  qwert           : ARRAY [0..31] OF CHAR;
  iSignal,oSignal : Signal;
  MyName          : ARRAY [0..3] OF CHAR;
  TermType        : INTEGER;
  TermNo          : INTEGER;
  iGate           : Gate;
  ESC             : BOOLEAN;

CONST (* Моды терминала: *)
  InpKr   = 0;    OutKr  = 1;
  ctrl    = 2;    pend   = 3;
  caplock = 4;    edit   = 5;
  Фрящ    = 6; (* reduce ESC ^S to ESC S *)
  in8bit  = 7;    out8bit= 8;

PROCEDURE inp?(): INTEGER;
CODE
  lgw4 li2 lgw2 93h jbsc 04h lxb lgw4 li3 lxb
  lgw4 li0 li1 sxb li8 rol add
END inp?;

PROCEDURE out!(h: INTEGER);
CODE
  copt stot li8 ror stot lgw4 li5 lodt lgw2 93h jbsc 04h sxb
  lgw4 li4 lodt sxb lgw4 li0 li1 sxb
END out!;

PROCEDURE out?(): INTEGER;
CODE
  lgw4 lib 10h lgw3 93h jbsc 04h lxb
  lgw4 lib 11h lxb lgw4 li0C li1 sxb li8 rol add
END out?;

PROCEDURE inp!(h: INTEGER);
CODE
  copt stot li8 ror stot lgw4 li0F lodt lgw3 93h jbsc 04h sxb
  lgw4 li0E lodt sxb lgw4 li0C li1 sxb
END inp!;

PROCEDURE WriteChar;
  VAR ninp,sinp,out: INTEGER;
BEGIN
  SETM(GETM()-{0..1});
  IF oFull>0 THEN DEC(oFull); RETURN END;
  IF (pend IN Mode)OR NOT OutActiv THEN RETURN END;
  out:=out?(); sinp:=oinp;
  LOOP
    IF oBegin=oEnd THEN OutActiv:=FALSE; Send(oSignal); EXIT END;
    IF oinp=oend   THEN ninp:=optr ELSE ninp:=oinp+1 END;
    IF ninp=out    THEN oFull:=4; EXIT END;
    segF000^[oinp]:=oBuf[oBegin];
    INC(oBegin); oinp:=ninp;
  END;
  IF oinp#sinp THEN inp!(oinp) END;
END WriteChar;

PROCEDURE Translate(VAR Ch: CHAR): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  IF in8bit IN Mode THEN
    RETURN TRUE;
  ELSE
    CASE Ch OF
    |03c  : r:=(edit IN Mode) OR (Abort=NIL);
            IF NOT r THEN Send(Abort^) END;
    |21c  : r:=NOT (ctrl IN Mode) OR NOT (pend IN Mode);
            IF NOT r THEN EXCL(Mode,pend) END
    |23c  : r:=NOT (ctrl IN Mode) OR (pend IN Mode);
            IF NOT r THEN INCL(Mode,pend) END
    ELSE r:=TRUE
    END;
  END;
  RETURN r;
END Translate;

PROCEDURE ReadChar;
BEGIN
  IF iEndNext=iBegin  THEN RETURN END;
  get;
  IF Ch=0c THEN RETURN END;
  IF Translate(Ch) THEN
    iBuf[iEnd]:=Ch; Send(iSignal);
    iEnd:=iEndNext;
    iEndNext:=(iEndNext+1) MOD iBufSize;
  END;
  Ch:=0c;
END ReadChar;

PROCEDURE Keyboard;
BEGIN
  IF    Activ THEN ReadChar  END;
  WriteChar;
END Keyboard;

TYPE Ptr=POINTER TO ARRAY [0..0] OF CHAR;

PROCEDURE DrvRd(dev,block: INTEGER; s: Ptr; l: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  EnterGate(iGate);
  FOR i:=0 TO l-1 DO
    Wait?1(iSignal);
    s^[i]:=iBuf[iBegin];
    iBegin:=(iBegin+1) MOD iBufSize;
    ?0;
  END;
  ExitGate(iGate);
  RETURN FALSE;
END DrvRd;

PROCEDURE TranslateOut(p: Block; l: INTEGER);
  VAR i: INTEGER; ch: CHAR;
BEGIN
  oBegin:=0; oEnd:=0;
  FOR i:=0 TO l-1 DO
    ch:=p^[i];
    IF (ch=0c)&((oEnd#0)OR(l#1)) THEN RETURN END;
    IF ch=NL THEN
      oBuf[oEnd]:=CR; INC(oEnd);
      oBuf[oEnd]:=LF; INC(oEnd);
    ELSE
      oBuf[oEnd]:=ch; INC(oEnd);
    END;
  END;
END TranslateOut;

PROCEDURE DrvWr(dev,block: INTEGER; s: ADDRESS; l: INTEGER): BOOLEAN;
BEGIN
  IF NOT Activ OR (l=0) THEN RETURN FALSE END;
  Wait?1(oSignal);
  TranslateOut(s,l);
  OutActiv:=TRUE;
  WriteChar;
  ?0;
  RETURN FALSE;
END DrvWr;

PROCEDURE Word16(a: INTEGER): INTEGER;
BEGIN
  RETURN INTEGER(segF000^[a])+256*INTEGER(segF000^[a+1]);
END Word16;

PROCEDURE WordBCB(a: INTEGER): INTEGER;
BEGIN
  RETURN INTEGER(BCB_word_adr^[a])+256*INTEGER(BCB_word_adr^[a+1]);
END WordBCB;

PROCEDURE InitTT;
  VAR base,channel: INTEGER;
BEGIN
  channel:=9;
  segF000:=ADDRESS(0BC000h);
  base:=08010h+4*channel;
  IF (segF000^[base+2]#1c)&(segF000^[base+2]#2c) THEN RETURN END;
  BCB_i_byte_adr:=Word16(base)+2F0000h;
  BCB_o_byte_adr:=BCB_i_byte_adr+12;
  BCB_word_adr  :=ADDRESS(BCB_i_byte_adr DIV 4);
  optr:=WordBCB(o_buff_ptr);
  iptr:=WordBCB(i_buff_ptr);
  oend:=WordBCB(o_buff_end);
  iend:=WordBCB(i_buff_end);
  oinp:=WordBCB(o_in);
  iout:=WordBCB(i_out);
  Mode:={ctrl};
  iBegin:=0; iEnd:=0; InitSignal(iSignal); iEndNext:=1;
  oBegin:=0; oEnd:=0; InitSignal(oSignal); Send(oSignal);
  OutActiv:=FALSE;
  oFull:=0;
  IF InsertAction(Keyboard) THEN RETURN END;
  Activ:=TRUE;
END InitTT;

PROCEDURE ClearTT;
BEGIN
  EnterGate(iGate);
  Activ:=FALSE;
  iBegin:=0; iEnd:=0; InitSignal(iSignal);
  RemoveAction(Keyboard);
  Abort:=NIL;
  ExitGate(iGate);
END ClearTT;

VAR MemStart: ADDRESS;
    MemSize : INTEGER;

PROCEDURE DrvCFE(dev,op: INTEGER; info: ADDRESS): BOOLEAN;
  VAR s: Ptr; i,d: INTEGER;
BEGIN
  CASE op OF
      0: info^:=0
    | 4: InitTT;
    | 8: RETURN DrvRd(0,0,info,1);
    | 9: RETURN DrvWr(0,0,ADR(info),1);
    |10: ClearTT;
    |11: info^:=MemStart; INC(info); info^:=MemSize;
    |21: INCL(Mode,INTEGER(info^))
    |22: EXCL(Mode,INTEGER(info^));
         IF info^=ctrl THEN EXCL(Mode,pend) END
    |23: (* Depth *)
         i:=INTEGER(iSignal);
         IF i>0 THEN info^:=0 ELSE info^:=-i END;
    |24: info^:=Abort;
    |25: Abort:=info;
    |26: info^:=TermType;
    |27: TermType:=info^;
    |28: (* Peek *)
         EnterGate(iGate); i:=info^;
         IF INTEGER(iSignal)>0 THEN
           info^:=0; INC(info); info^:=0;
           ExitGate(iGate); RETURN FALSE
         ELSE info^:=-ORD(iSignal); d:=info^;
         END;
         INC(info);
         IF i>d THEN info^:=0;
         ELSE
           i:=(iBegin+i) MOD iBufSize;
           info^:=iBuf[i];
         END;
         ExitGate(iGate)
  ELSE RETURN TRUE;
  END;
  RETURN FALSE;
END DrvCFE;

VAR p : ProcessId;
    info: POINTER TO
            RECORD
              Name : ARRAY [0..3] OF CHAR;
              TermN: INTEGER;
            END;

BEGIN
  TermType:=2;
  Activ:=FALSE;
  InitGate(iGate);
  InitSignal(iSignal);
  InitSignal(oSignal);
  Abort:=NIL;
  p:=MyTask();
  MemStart:=p^.MemStart;
  MemSize :=p^.MemSize;
  info:=p^.ParmLink;
  TermNo:=0;
  MyName:=info^.Name; MyName[2]:=0c;
  IF IamDriver(MyName,{TermNo},serial,DrvRd,DrvWr,DrvCFE) THEN HALT(1) END;
  IF IamTerminal(TermNo,MyName) THEN HALT(1) END;
END LB7004.
