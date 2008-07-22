MODULE cnv; (* 16-Dec-88. (c) KRONOS *)

IMPORT FROM Model;
IMPORT pedModel;
IMPORT ModelIO;
FROM Terminal   IMPORT  print;
FROM SYSTEM     IMPORT  ADR;
FROM KRONOS     IMPORT  MOVE;
FROM pedModel   IMPORT  tpin_rec, ctype, cre_board, cre_ctype, board, string,
                        cre_chip, tie, cre_signal, signal_type;
FROM Args       IMPORT  ScanFlags, TakeWord;


VAR mdl: Object;
    brd: board;
    nm : string;
    no: INTEGER;
    ep: ARRAY [0..255] OF tpin_rec;
    sg: pedModel.signal;

PROCEDURE CalcEP(o: Object);
BEGIN
  ASSERT(Tag(o)=externalpin);
  IF no<o^.EPinNo+1 THEN no:=o^.EPinNo+1 END;
  WITH ep[o^.EPinNo] DO
    x:=o^.PinX; y:=o^.PinY;
    cu:=NIL; cno:=0;
    tool:=o^.TrackWidth;
  END;
END CalcEP;

PROCEDURE CreTyp(o: Object);
  VAR t: ctype;
BEGIN
  IF Tag(o)#chiptype THEN RETURN END;
  print('typ: %15s\r',o^.Name);
  no:=0;
  Iterate(o^.ExternalPins,CalcEP);
  cre_ctype(t,no,brd);
  t^.x:=o^.ctX; t^.y:=o^.ctY;
  t^.name:=o^.Name;
  t^.xnm:=0; t^.ynm:=0;
  MOVE(ADR(t^.pins),ADR(ep),t^.pno*SIZE(tpin_rec));
END CreTyp;

PROCEDURE CreChip(o: Object);
  VAR c: pedModel.chip; t: ctype; i: INTEGER;
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  print('chp: %15s\r',o^.Name);
  t:=NIL;
  FOR i:=0 TO brd^.tno-1 DO
    IF o^.ChipType^.Name=brd^.typs^[i]^.name THEN t:=brd^.typs^[i] END;
  END;
  ASSERT(t#NIL);
  cre_chip(c,brd,t);
  c^.x:=o^.XB;
  c^.y:=o^.YB;
  c^.r:=o^.RB;
  c^.name:=o^.Name;
END CreChip;

PROCEDURE DoPin(o: Object);
  VAR c: pedModel.chip; i: INTEGER;
BEGIN
  ASSERT(Tag(o)=pin);
  c:=NIL;
  FOR i:=0 TO brd^.cno-1 DO
    IF o^.Chip^.Name=brd^.chps^[i]^.name THEN
      ASSERT(c=NIL); c:=brd^.chps^[i];
    END;
  END;
  ASSERT(c#NIL);
  tie(sg,c,o^.No,brd);
END DoPin;

PROCEDURE CreSig(o: Object);
  VAR s: pedModel.signal;
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  print('sig: %15s\r',o^.Name);
  cre_signal(s,brd);
  IF o^.ChainB=NIL THEN
    s^.cu:=NIL; s^.cno:=0;
  ELSE
    s^.cu:=ADR(o^.ChainB^.cType); s^.cno:=o^.ChainB^.cLen;
  END;
  s^.type:=signal_type{};
  IF power  IN o^.sType THEN INCL(s^.type,pedModel.power) END;
  IF fantom IN o^.sType THEN INCL(s^.type,pedModel.fantom) END;
  IF fixed  IN o^.sType THEN INCL(s^.type,pedModel.fixed) END;
  s^.name:=o^.Name;
  sg:=s;
  Iterate(o^.TiedPins,DoPin);
END CreSig;

BEGIN
  TakeWord(nm);
  mdl:=ModelIO.ReadModel(nm);
  cre_board(brd);
  brd^.name:=nm;
  brd^.x:=mdl^.ctX;
  brd^.y:=mdl^.ctY;
  brd^.lays:={0..1};
  Iterate(mdl^.All,CreTyp);
  Iterate(mdl^.All,CreChip);
  Iterate(mdl^.All,CreSig);
  pedModel.WriteModel(nm,brd);
END cnv.
