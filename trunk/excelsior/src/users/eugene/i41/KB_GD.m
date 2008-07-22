IMPLEMENTATION MODULE KB_GD; (* 11-Nov-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Resource   IMPORT  Final;
FROM Terminal   IMPORT  print;
FROM Scheduler  IMPORT  Wait, Send, Signal, InitSignal,
                        InsertAction, RemoveAction;
IMPORT  mcd: mCodeMnem;

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

PROCEDURE reset;
BEGIN
END reset;

PROCEDURE ipt_proc;
  VAR i: INTEGER;
BEGIN
(*
  out(csr,0Fh);
  IF NOT (0 IN BITSET(inp(cha))) THEN
    dt[end]:=CHAR(INTEGER(inp(cha)) DIV 2);
    out(csr,0Eh);
    out(csr,0Fh);
    i:=(end+1) MOD (HIGH(dt)+1);
    IF i#beg THEN end:=i END;
    IF ready#NIL THEN Send(ready) END;
  END;
*)
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
