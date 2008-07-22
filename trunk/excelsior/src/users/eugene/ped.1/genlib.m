MODULE genlib; (* 13-Feb-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM StdIO      IMPORT  print;
FROM Args       IMPORT  TakeWord;

FROM pedModel   IMPORT  board, string, ctype, tpin_rec,
                        cre_board, WriteModel, cre_ctype;

VAR mdl: board;

PROCEDURE dip(nm: string; size: INTEGER; no: INTEGER);
  VAR t: ctype; i: INTEGER; p: POINTER TO tpin_rec;
BEGIN
  print('%s\n',nm);
  cre_ctype(t,no,mdl); t^.name:=nm;
  t^.x:=(no DIV 2-1)*96; t^.y:=96*size;
  FOR i:=0 TO no-1 DO
    p:=ADR(t^.pins[i]);
    IF i<(no DIV 2) THEN p^.x:=i*96; p^.y:=0
    ELSE p^.x:=(no-i-1)*96; p^.y:=96*size;
    END;
    p^.cu:=NIL; p^.cno:=0; p^.tool:=4;
  END;
END dip;

BEGIN
  cre_board(mdl);
  mdl^.name:='lib';
  dip('DIP8',3,8);
  dip('DIP14',3,14);
  dip('DIP16',3,16);
  dip('DIP20',3,20);
  dip('DIP24',6,24);
  dip('DIP28',6,28);
  dip('DIP40',6,40);
  WriteModel('LIB',mdl);
END genlib.
