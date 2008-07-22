MODULE gd;

FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Scheduler  IMPORT  Gate, Signal, ProcessId, MyTask, Sleep,
                        InitGate, EnterGate, ExitGate, Send,
                        Wait, InitSignal, Awaited, ?1, ?0, Wait?1,
                        KillProcess, MakeProcess, Start,
                        InsertAction, RemoveAction;
FROM FsDrv      IMPORT  IamDriver, dMode, RemDriver;
FROM FsPublic   IMPORT  NoSuchCFE, VisFSerr;
FROM TTYs       IMPORT  IamTerminal;
FROM Resource   IMPORT  Final;
FROM Terminal   IMPORT  print;
FROM SCR_GD     IMPORT  write;
IMPORT  mcd: mCodeMnem;
IMPORT  Image;

TYPE
  block_ptr=POINTER TO ARRAY [0..4095] OF CHAR;

CONST
  TermType=2;

VAR
  Mode            : BITSET;
  iBegin,iEnd     : INTEGER;
  iBuf            : ARRAY [0..255] OF CHAR;
  iGate, oGate    : Gate;
  iSignal         : Signal;
  wsp             : ARRAY [0..299] OF WORD;
  TermNo          : INTEGER;
  Abort           : POINTER TO Signal;
  MyName          : ARRAY [0..3] OF CHAR;
  x_off           : BOOLEAN; -- требование остановить передачу
  prs             : ProcessId;

CONST (* Моды терминала: *)
  ctrl    = 2; -- разрешение обработки символов ^Q, ^S
  edit    = 5; -- запрет обработки символа ^C
  in8bit  = 7; -- запрет перекодировок при вводе
  out8bit = 8; -- запрет перекодировок при выводе

TYPE
  st_set=SET OF (st_ctrl,st_shift,st_lat,st_caps);
  ch_type=(code,digit,letter,sign);

VAR
  st: st_set;

CONST
  csr=46h<<30;
  cha=40h<<30;
  chb=42h<<30;
  chc=44h<<30;

VAR
  dt              : ARRAY [0..99] OF CHAR;
  beg,end         : INTEGER;
  ready           : Signal;

PROCEDURE setm(m: BITSET); CODE mcd.setm END setm;
PROCEDURE getm(): BITSET; CODE mcd.getm END getm;
PROCEDURE inp(n: INTEGER): WORD; CODE 92h END inp;
PROCEDURE out(n: INTEGER; v: WORD); CODE 93h END out;

PROCEDURE ipt_proc;
  VAR i: INTEGER;
BEGIN
  IF NOT (0 IN BITSET(inp(cha))) THEN
    dt[end]:=CHAR(INTEGER(inp(cha)) DIV 2);
    out(csr,0Eh);
    out(csr,0Fh);
    i:=(end+1) MOD (HIGH(dt)+1);
    IF i#beg THEN end:=i END;
    IF ready#NIL THEN Send(ready) END;
  END;
END ipt_proc;

PROCEDURE read(): CHAR;
  VAR ei: BITSET; ch: CHAR;
BEGIN
  ei:=getm(); setm(ei-{0..1});
  IF end=beg THEN Wait(ready) END;
  setm(ei);
  ch:=dt[beg]; beg:=(beg+1) MOD (HIGH(dt)+1);
  RETURN ch;
END read;

PROCEDURE put(Ch: CHAR; ct: ch_type);
  VAR NewEnd: INTEGER;
BEGIN
  CASE ct OF
    |code  :
    |letter: IF NOT (st_lat IN st) THEN
               Ch:=CHAR(ORD(Ch)+200b);
               Ch:=CHAR(BITSET(Ch)/{5});
             END;
  END;
  CASE Ch OF
    |17c  : INCL(st,st_lat); RETURN
    |16c  : EXCL(st,st_lat); RETURN
    |03c  : IF NOT (edit IN Mode) THEN
              IF (Abort#NIL) THEN Send(Abort^) END;
              RETURN
            END;
    |21c  : IF (ctrl IN Mode) & x_off THEN x_off:=FALSE; RETURN END;
    |23c  : IF (ctrl IN Mode) & NOT x_off THEN x_off:=TRUE; RETURN END;
  ELSE
  END;
  NewEnd:=(iEnd+1) MOD (HIGH(iBuf)+1);
  IF iBegin # NewEnd THEN
    iBuf[iEnd]:=Ch; iEnd:=NewEnd; Send(iSignal);
  END;
END put;

PROCEDURE ipt_procKB;
  VAR Ch: CHAR;
BEGIN
  LOOP
    Ch:=read();
    CASE INTEGER(Ch) OF
      |014b: put(206c,code);            -- gold
      |037b: put(205c,code);            -- silver
      |013b: put(216c,code);            -- bronze
      |034b: put(201c,code);            -- up
      |032b: put(204c,code);            -- left
      |035b: put(202c,code);            -- down
      |031b: put(203c,code);            -- right
      |010b: put(220c,code);            -- pg up
      |002b: put(222c,code);            -- pg down
      |023b: put(224c,code);            -- ins ch
      |024b: put(177c,code);            -- del ch
      |026b: put(217c,code);            -- ins ln
      |022b: put(231c,code);            -- del ln
      |020b: put(232c,code);            -- swap up
      |001b: put(233c,code);            -- swap dw
      |036b: put(234c,code);            -- dup ln
      |021b: put(235c,code);            -- eol
      |033b: put(033c,code);            -- esc
      |025b: put(210c,code);            -- home
      |012b: put(012c,code);            -- lf
      |030b: put(211c,code);
      |177b: put(010c,code);
    ELSE
      IF Ch>=100c THEN put(Ch,letter) ELSE put(Ch,code) END;
    END;
  END;
END ipt_procKB;

PROCEDURE DrvRd(dev,block: INTEGER; s: block_ptr; l: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  EnterGate(iGate);
  FOR i:=0 TO l-1 DO
    Wait?1(iSignal);
    s^[i]:=iBuf[iBegin];
    iBegin:=(iBegin+1) MOD (HIGH(iBuf)+1);
    ?0;
  END;
  ExitGate(iGate);
  RETURN FALSE;
END DrvRd;

PROCEDURE DrvWr(dev,block: INTEGER; s: block_ptr; l: INTEGER): BOOLEAN;
BEGIN
  EnterGate(oGate); ?1;
  WHILE x_off DO Sleep END;
  write(s^,l);
  ExitGate(oGate); ?0;
  RETURN FALSE;
END DrvWr;

PROCEDURE finish;
BEGIN
  RemoveAction(ipt_proc);
  IF RemDriver(MyName) THEN END;
  IF KillProcess(prs^.pp)=0 THEN END;
END finish;

PROCEDURE InitTT;
  VAR i,n: INTEGER; r: BOOLEAN; adr: ADDRESS; ei: BITSET;
BEGIN
  InitSignal(ready);
  beg:=0; end:=0;
  out(csr,0Fh);
  IF InsertAction(ipt_proc) THEN HALT(1) END;
  iBegin:=0; iEnd:=0; InitGate(iGate); InitSignal(iSignal);
  Abort:=NIL; Mode:={ctrl}; x_off:=FALSE;
  st:=st_set{st_lat,st_caps};
  prs:=MakeProcess(ipt_procKB,ADR(wsp),SIZE(wsp));
  Start(prs);
  Final(finish);
END InitTT;

PROCEDURE DrvCFE(dev,op: INTEGER; info: ADDRESS): BOOLEAN;
  VAR d,i: INTEGER; ei: BITSET;
BEGIN
  CASE op OF
    | 4: InitTT;
    | 8: IF DrvRd(0,0,info,1) THEN END;
    | 9: IF DrvWr(0,0,ADR(info),1) THEN END;
    |10:
    |19: EnterGate(iGate);
         Wait?1(iSignal); Send(iSignal); ?0;
         ExitGate(iGate);
    |21: INCL(Mode,INTEGER(info^))
    |22: EXCL(Mode,INTEGER(info^));
         IF info^=ctrl THEN x_off:=FALSE END;
    |23: (* Depth *)
         EnterGate(iGate);
         IF INTEGER(iSignal)>0 THEN info^:=0 ELSE info^:=-INTEGER(iSignal) END;
         ExitGate(iGate)
    |28: (* Peek *)
         EnterGate(iGate); i:=info^;
         IF INTEGER(iSignal)>0 THEN
           info^:=0; INC(info); info^:=0;
           ExitGate(iGate); RETURN FALSE
         ELSE info^:=-INTEGER(iSignal); d:=info^;
         END;
         INC(info);
         IF i>d THEN info^:=0;
         ELSE
           i:=(iBegin+i) MOD (HIGH(iBuf)+1);
           info^:=iBuf[i];
         END;
         ExitGate(iGate)
    |24: info^:=Abort
    |25: Abort:=info;
    |26: info^:=TermType;
    |27: -- set term type
  ELSE RETURN NoSuchCFE
  END;
  RETURN FALSE;
END DrvCFE;

PROCEDURE help;
BEGIN
  print('gd: error in parameters.\n'); HALT;
END help;

PROCEDURE parms;
  VAR ps : POINTER TO ARRAY [0..255] OF CHAR;
      p  : ProcessId;
      i,j: INTEGER;
BEGIN
  p:=MyTask();
  i:=0; j:=0; ps:=p^.ParmLink;
  WHILE (j<HIGH(ps^)) & (ps^[j]=' ') DO INC(j) END;
  IF ps^[j]#0c THEN
    WHILE (i<HIGH(MyName)) & (j<=HIGH(ps^)) & (ps^[j]#0c) & (ps^[j]#' ') DO
      MyName[i]:=ps^[j]; INC(j); INC(i);
    END;
    IF i#2 THEN help END;
    MyName[2]:=0c;
    j:=Image.PeekNum(ps^,j,TermNo);
    IF j<0 THEN help END;
  ELSE
    TermNo:=0; MyName:='tt';
  END;
END parms;

PROCEDURE init_driver;
  VAR r: BOOLEAN; s: ARRAY [0..79] OF CHAR;
BEGIN
  InitGate(iGate);
  InitGate(oGate);
  parms;
  r:=IamDriver(MyName,{TermNo},serial,DrvRd,DrvWr,DrvCFE);
  IF r THEN VisFSerr(r,s); print('gd: %s\n',s); HALT(1) END;
  r:=IamTerminal(TermNo,MyName);
  IF r THEN VisFSerr(r,s); print('gd: %s\n',s); HALT(1) END;
END init_driver;

VAR never: Signal;

BEGIN
  init_driver;
  InitSignal(never);
  Wait(never);
END gd.
