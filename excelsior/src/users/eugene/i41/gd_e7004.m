MODULE gd;

FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Scheduler  IMPORT  Gate, Signal, ProcessId, MyTask, Sleep,
                        InitGate, EnterGate, ExitGate, Send,
                        Wait, InitSignal, Awaited, ?1, ?0, Wait?1,
                        KillProcess, MakeProcess, Start;
FROM FsDrv      IMPORT  IamDriver, dMode, RemDriver;
FROM FsPublic   IMPORT  NoSuchCFE, VisFSerr;
FROM TTYs       IMPORT  IamTerminal;
FROM Resource   IMPORT  Final;
FROM Terminal   IMPORT  print;
FROM SCR_GD     IMPORT  write;
FROM KB_GD      IMPORT  read, stop;
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

PROCEDURE put(Ch: CHAR; ct: ch_type);
  VAR NewEnd: INTEGER;
BEGIN
  CASE ct OF
    |code  :
    |letter: IF st_ctrl IN st THEN
               Ch:=CHAR(ORD(Ch)-100b);
             ELSIF st_lat IN st THEN
               IF (st_shift IN st)#(st_caps IN st) THEN
                 Ch:=CHAR(ORD(Ch)+40b);
               END;
             ELSE
               Ch:=CHAR(ORD(Ch)+200b);
               IF (st_shift IN st)=(st_caps IN st) THEN
                 Ch:=CHAR(ORD(Ch)+40b);
               END;
             END;
    |digit : IF st_shift IN st THEN Ch:=CHAR(ORD(Ch)-20b) END;
    |sign  : IF st_shift IN st THEN Ch:=CHAR(ORD(Ch)+20b) END;
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
  VAR Ch,sv: CHAR;
BEGIN
  LOOP
    Ch:=read();
    IF Ch=264c THEN Ch:=sv ELSE sv:=Ch END;
    CASE INTEGER(Ch) OF
      |222b: put('0',code);
      |224b: put('.',code);
      |225b: put(15c,code);
      |226b: put('1',code);
      |227b: put('2',code);
      |230b: put('3',code);
      |231b: put('4',code);
      |232b: put('5',code);
      |233b: put('6',code);
      |234b: put(',',code);
      |235b: put('7',code);
      |236b: put('8',code);
      |237b: put('9',code);
      |240b: put('-',code);
      |274b: put(10c,code);
      |275b: put(15c,code);
      |276b: put(11c,code);
      |277b: put(';',digit);
      |300b: put('1',digit);
      |301b: put('J',letter);
      |302b: put('F',letter);
      |303b: put('Q',letter);
      |375b: put(337c,code);
      |305b: put('2',digit);
      |306b: put('C',letter);
      |307b: put('Y',letter);
      |310b: put('^',letter);
      |312b: put('/',sign);
      |313b: put('3',digit);
      |314b: put('U',letter);
      |315b: put('W',letter);
      |316b: put('S',letter);
      |320b: put('4',digit);
      |321b: put('K',letter);
      |322b: put('A',letter);
      |323b: put('M',letter);
      |324b: put(' ',code);
      |326b: put('5',digit);
      |327b: put('E',letter);
      |330b: put('P',letter);
      |331b: put('I',letter);
      |333b: put('6',digit);
      |334b: put('N',letter);
      |335b: put('R',letter);
      |336b: put('T',letter);
      |340b: put('7',digit);
      |341b: put('G',letter);
      |342b: put('O',letter);
      |343b: put('X',letter);
      |345b: put('8',digit);
      |346b: put('[',letter);
      |347b: put('L',letter);
      |350b: put('B',letter);
      |352b: put('9',digit);
      |353b: put(']',letter);
      |354b: put('D',letter);
      |355b: put('@',letter);
      |357b: put('0',code);
      |360b: put('Z',letter);
      |361b: put('_',code);
      |362b: put('V',letter);
      |363b: put(',',sign);
      |365b: --- ????
      |366b: put('H',letter);
      |367b: put('.',sign);
      |371b: put('-',sign);
      |372b: put(':',digit);
      |373b: put('\',letter);
      |374b: --- ????
      |304b: put('~',code);
      |126b: put(206c,code);            -- gold
      |127b: put(205c,code);            -- silver
      |130b: put(216c,code);            -- bronze
      |252b: put(201c,code);            -- up
      |247b: put(204c,code);            -- left
      |251b: put(202c,code);            -- down
      |250b: put(203c,code);            -- right
      |216b: put(220c,code);            -- pg up
      |217b: put(222c,code);            -- pg down
      |213b: put(224c,code);            -- ins ch
      |214b: put(177c,code);            -- del ch
      |131b: put(217c,code);            -- ins ln
      |132b: put(231c,code);            -- del ln
      |144b: put(232c,code);            -- swap up
      |145b: put(233c,code);            -- swap dw
      |146b: put(234c,code);            -- dup ln
      |147b: put(235c,code);            -- eol
      |150b: put(033c,code);            -- esc
      |241b: put(210c,code);            -- home
      |242b: put(012c,code);            -- lf
      |257b: INCL(st,st_ctrl);
      |256b: INCL(st,st_shift);
      |263b: EXCL(st,st_ctrl); EXCL(st,st_shift);
      |260b: st:=st/st_set{st_caps};
      |262b: st:=st/st_set{st_lat};
    ELSE
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
  IF RemDriver(MyName) THEN END; stop;
  IF KillProcess(prs^.pp)=0 THEN END;
END finish;

PROCEDURE InitTT;
  VAR i,n: INTEGER; r: BOOLEAN; adr: ADDRESS; ei: BITSET;
BEGIN
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
