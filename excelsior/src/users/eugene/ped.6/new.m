MODULE new; (* 22-Sep-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;
IMPORT T: Terminal;
IMPORT  Model;
FROM Model      IMPORT  Objects;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
IMPORT  mcd: mCodeMnem;
IMPORT  io : ModelIO;
IMPORT  arg: Args;

VAR mdl: Model.Object;
    nm : ARRAY [0..15] OF CHAR;

PROCEDURE Tag(o: Model.Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE setTag(o: Model.Object; t: Objects);
CODE mcd.stot 0 mcd.lodt mcd.sxb END setTag;

PROCEDURE Poz(o: Model.Object): INTEGER;
CODE mcd.lsw0 mcd.lib 0FFh mcd.bic 8 mcd.shr END Poz;

PROCEDURE setPoz(o: Model.Object; p: INTEGER);
CODE
  mcd.stot mcd.copt mcd.lsw0 mcd.lib 0FFh
  mcd.and mcd.lodt 8 mcd.shl mcd.add mcd.ssw0
END setPoz;

PROCEDURE move(t,f: ADDRESS; s: INTEGER); CODE mcd.move END move;

VAR cht: Model.Object;
VAR cnt,max: INTEGER;

PROCEDURE ip(o: Model.Object; i: INTEGER);
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  IF NOT (o^.RB+1 IN {0..4}) THEN
    T.print('Warning !!! Chip %s rotate=%d\n',o^.Name,o^.RB);
    o^.RB:=o^.RB MOD 4;
  END;
  IF Poz(o)#cnt  THEN RETURN END;
  ASSERT(Poz(o)=Poz(o^.ChipType));
  o^.ChipType:=cht;
END ip;

PROCEDURE mark(o: Model.Object; i: INTEGER);
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  IF Poz(o^.ChipType)<0 THEN setPoz(o^.ChipType,cnt); INC(cnt) END;
  setPoz(o,Poz(o^.ChipType));
END mark;

VAR findcht: Model.Object;

PROCEDURE find(o: Model.Object; i: INTEGER);
BEGIN
  IF Tag(o)#chiptype THEN RETURN END;
  IF Poz(o)#cnt THEN RETURN END;
  findcht:=o;
END find;

VAR e: Reaction;

PROCEDURE chain(mdl: Model.Object);
  VAR i: INTEGER;
BEGIN
  ASSERT(Tag(mdl)=chiptype);
  FOR i:=0 TO max DO
    cnt:=i;
    findcht:=NIL;
    Model.Iterate(mdl^.All,find,0);
    IF Exception?(e) THEN
      cht:=findcht; T.print(Message);
    ELSE
      cht:=io.ReadModel(findcht^.Name);
    END;
    T.print(' %s\n',findcht^.Name);
    setPoz(cht,cnt);
    Model.Iterate(mdl^.All,ip,0);
    Model.UnTie(mdl^.All,findcht); Model.Tie(mdl^.All,cht);
  END;
END chain;

BEGIN
  arg.ScanFlags; arg.TakeWord(nm);
  mdl:=io.ReadModel(nm); IF mdl=NIL THEN HALT END;
  Model.CleareModel(mdl,0); cnt:=0;
  Model.Iterate(mdl^.All,mark,0); max:=cnt-1;
  chain(mdl);
  io.WriteModel(mdl);
END new.
