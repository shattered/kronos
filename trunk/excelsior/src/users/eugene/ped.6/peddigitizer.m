IMPLEMENTATION MODULE pedMouse; (* Sem 09-Oct-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM Resource   IMPORT  Final;
FROM Scheduler  IMPORT  Signal, InitSignal, Send, Wait, InsertAction,
                        RemoveAction, Start, MakeProcess, ProcessId,
                        SetHaltSignal;
IMPORT  mcd: mCodeMnem;
IMPORT  sch: Scheduler;
IMPORT  kb : Keyboard;
IMPORT  T  : Terminal;

CONST bufsz=32;
TYPE  item=RECORD dx,dy: INTEGER; ch: CHAR END;
      PROCESS=ADDRESS;

VAR buf  : ARRAY [0..bufsz-1] OF item;
    key  : BOOLEAN;
    beg  : INTEGER;
    end  : INTEGER;
    hei  : Signal;
    last : INTEGER;
    next : INTEGER;
    app  : BOOLEAN;
    halt : Signal;
    prs  : ProcessId;
    Xs,Ys: INTEGER;

PROCEDURE inp (a: ADDRESS): WORD;     CODE mcd.inp  END inp;
PROCEDURE out (a: ADDRESS; w: WORD);  CODE mcd.out  END out;
PROCEDURE setm(m: BITSET);            CODE mcd.setm END setm;
PROCEDURE getm(): BITSET;             CODE mcd.getm END getm;
PROCEDURE TRANSFER(VAR f,t: PROCESS); CODE mcd.tra  END TRANSFER;

PROCEDURE put(x,y: INTEGER; c: CHAR);
  VAR m: BITSET;
BEGIN
  m:=getm();
  setm(m-{0..1});
  IF hei#NIL THEN Send(hei) END;
  IF next=beg THEN RETURN END;
  last:=end; end:=next; next:=(next+1) MOD bufsz;
  buf[last].dx:=x; buf[last].dy:=y; buf[last].ch:=c;
  app:=(c=0c)&(last#beg);
  setm(m);
END put;

VAR
    iDTR,iCSR: INTEGER;
    iTrap    : INTEGER;
    Key,Ipt  : PROCESS;
    iWsp     : ARRAY [0..149] OF INTEGER;

PROCEDURE read;
  VAR Ch       : CHAR;
      i        : INTEGER;
      State    : INTEGER;
      X,Y,Z    : INTEGER;
      Xneg,Yneg: BOOLEAN;
      incXs, incYs, incX, incY: BOOLEAN;
BEGIN
  State:=0;
  LOOP
    Ch:=CHAR(BITSET(inp(iDTR))*{0..6});
    CASE State OF
      0:i:=ORD(Ch)-ORD('0');
        IF i IN {0..4} THEN Z:=i; INC(State); END;
     |1:IF Ch='+' THEN Xneg:=FALSE; X:=0; INC(State) END;
        IF Ch='-' THEN Xneg:=TRUE ; X:=0; INC(State) END;
     |2..6:
        i:=ORD(Ch)-ORD('0');
        IF i IN {0..9} THEN
          IF Xneg THEN X:=X*10-i ELSE X:=X*10+i END;
          INC(State);
        ELSE
          State:=0;
        END;
     |7:IF Ch='+' THEN Yneg:=FALSE; Y:=0; INC(State) END;
        IF Ch='-' THEN Yneg:=TRUE ; Y:=0; INC(State) END;
     |8..12:
        i:=ORD(Ch)-ORD('0');
        IF i IN {0..9} THEN
          IF Yneg THEN Y:=Y*10-i ELSE Y:=Y*10+i END;
          INC(State);
        ELSE
          State:=0;
        END;
     |13:
        IF Ch<' ' THEN
          X:=X DIV 20; Y:=Y DIV 20;
          IF (X#Xs) OR (Y#Ys) THEN
            incX:=(X-Xs)>0;
            incY:=(Y-Ys)>0;
            IF (incX#incXs) & (ABS(X-Xs)<2) THEN X:=Xs
            ELSE incXs:=incX END;
            IF (incY#incYs) & (ABS(Y-Ys)<2) THEN Y:=Ys
            ELSE incYs:=incY END;
            IF (X#Xs) OR (Y#Ys) THEN
              IF app THEN
                buf[last].dx:=X; buf[last].dy:=Y;
              ELSE
                put(X,Y,0c);
              END;
              Xs:=X; Ys:=Y;
            END;
          END;
          IF (Z=4) # key THEN
            key:=NOT key;
            IF key THEN put(Xs,Ys,266c) ELSE put(Xs,Ys,267c) END;
          END;
          State:=0;
        END;
    END;
    out(iCSR,0);
    out(iCSR,100b);
    TRANSFER(Key,Ipt);
  END;
END read;

PROCEDURE OnIpt(VAR Pfrom: ADDRESS; Pto: ADDRESS; No: INTEGER);
  VAR A: ADDRESS;
BEGIN
  IF (No>0) & (No<=3Fh) THEN A:=No*2;
    A^:=Pto; INC(A); A^:=ADR(Pfrom)
  END
END OnIpt;

PROCEDURE init;
  VAR m: BITSET;
BEGIN
  iCSR:=176530b DIV 2;   iTrap:=330b DIV 4;
  iDTR:=iCSR+1;
  m:=getm();
  setm(m-{0..1});
  out(iCSR,0);
  sch.new_process(read,ADR(iWsp),SIZE(iWsp),Key);
  OnIpt(Ipt,Key,iTrap);
  out(iCSR,100b);
  setm(m);
END init;

PROCEDURE finish;
  VAR m: BITSET;
BEGIN
  m:=getm();
  setm(m-{0..1});
  out(iCSR,0);
  IF sch.KillProcess(prs^.pp)=0 THEN END;
  Wait(halt);
  setm(m);
END finish;

PROCEDURE empty?(): BOOLEAN;
BEGIN
  RETURN beg=end;
END empty?;

PROCEDURE first(VAR x,y: INTEGER; VAR ch: CHAR);
BEGIN
  x:=buf[beg].dx;
  y:=buf[beg].dy;
  ch:=buf[beg].ch;
END first;

PROCEDURE drop;
  VAR m: BITSET;
BEGIN
  m:=getm();
  setm(m-{0..1});
  ASSERT(beg#end);
  beg:=(beg+1) MOD bufsz;
  IF beg=last THEN app:=FALSE END;
  setm(m);
END drop;

PROCEDURE wait;
  VAR m: BITSET;
BEGIN
  m:=getm();
  setm(m-{0..1});
  IF beg=end THEN Wait(hei) END;
  setm(m);
END wait;

PROCEDURE Read(): CHAR;
  VAR x,y: INTEGER; c: CHAR;
BEGIN
  REPEAT wait; first(x,y,c); drop UNTIL c#0c;
  RETURN c
END Read;

PROCEDURE keyboard;
  VAR ch: CHAR;
BEGIN
  LOOP
    ch:=T.Read(); put(Xs,Ys,ch);
  END;
END keyboard;

VAR wsp: ARRAY [0..299] OF INTEGER;
      m: BITSET;

BEGIN
  rel:=FALSE; Xs:=0; Ys:=0; key:=FALSE;
  beg:=0; end:=0; last:=0; next:=1; app:=FALSE;
  InitSignal(hei);
  InitSignal(halt);
  init;
  prs:=MakeProcess(keyboard,ADR(wsp),SIZE(wsp));
  SetHaltSignal(prs,halt);
  m:=getm();
  setm(m-{0..1});
  Start(prs);
  Final(finish);
  setm(m);
END pedMouse.
