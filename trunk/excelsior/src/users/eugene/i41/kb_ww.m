IMPLEMENTATION MODULE KB_GD; (* 11-Nov-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Resource   IMPORT  Final;
FROM Terminal   IMPORT  print;
FROM Scheduler  IMPORT  Wait, Send, Signal, InitSignal,
                        InsertAction, RemoveAction;
IMPORT  mcd: mCodeMnem;

TYPE
  cs=SET OF (TxEN,RxIE,RxEN,SBRK,ER,TxIE,RESET,EH);
  ss=SET OF (TxRDY,RxRDY,TxE,PE,OE,FE,SINDET,DSR);

CONST
  csr=0Ah<<30;
  dtr=08h<<30;
  tcmd=1Eh<<30;
  tcnt=18h<<30;

VAR
  cmd             : cs;
  dt              : ARRAY [0..99] OF CHAR;
  beg,end         : INTEGER;
  ready           : Signal;

CONST

(* MODE *)
  x1  = {0};            bits5 = {};             odd     = {4};
  x16 = {1};            bits6 = {2};            even    = {4,5};
  x64 = {0,1};          bits7 = {3};            stop1   = {6};
                        bits8 = {2,3};          stop1_5 = {7};
                                                stop2   = {6,7};

PROCEDURE setm(m: BITSET); CODE mcd.setm END setm;
PROCEDURE getm(): BITSET; CODE mcd.getm END getm;
PROCEDURE inp(n: INTEGER): WORD; CODE 92h END inp;
PROCEDURE out(n: INTEGER; v: WORD); CODE 93h END out;

PROCEDURE reset;
BEGIN
  out(tcmd,{4,5,2});
  out(tcmd,{6,4,5,2,1});
  out(tcmd,{7,4,5,2,1});
  out(tcnt,26);
  out(tcnt,0);
  out(csr,{});
  out(csr,{});
  out(csr,{});
  out(csr,{});
  out(csr,cs{RESET});
  out(csr,x16+stop1+bits8+odd);
  cmd:=cs{RxEN,RxIE};
  out(csr,cmd);
END reset;

PROCEDURE ipt_proc;
  VAR i: INTEGER;
BEGIN
  IF RxRDY IN ss(inp(csr)) THEN
    dt[end]:=inp(dtr);
    out(csr,cmd+cs{ER});
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

PROCEDURE ini_ipt;
BEGIN
  IF InsertAction(ipt_proc) THEN HALT(1) END;
END ini_ipt;

PROCEDURE stop;
BEGIN
  RemoveAction(ipt_proc);
END stop;

VAR ch: CHAR;

BEGIN
  Final(stop);
  InitSignal(ready);
  beg:=0; end:=0;
  reset;
  ini_ipt;
--  LOOP
--    ch:=read();
--    print('%$3h\n',ch);
--  END;
END KB_GD.
