MODULE bad; (* Sem 19-Nov-87. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM Model      IMPORT  Object, Tag, Iterate, Objects, setPoz, Poz, Lget,
                        NewObject, Tie, Lset, String, UnTie;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM Args       IMPORT  TakeWord;

VAR Name        : String;
    mdl         : Object;
    CheckedChip : Object;
    FreeSignal  : Object;
    CurSig      : Object;
    CurChp      : Object;

PROCEDURE SetSig(p: Object);
BEGIN
  IF Tag(p)#pin THEN UnTie(CurSig^.TiedPins,p); RETURN END;
  p^.Signal:=CurSig;
END SetSig;

PROCEDURE SetChp(p: Object);
BEGIN
  IF Tag(p)#pin THEN UnTie(CurChp^.Pins,p); RETURN END;
  p^.Chip:=CurChp;
END SetChp;

PROCEDURE SetRel(s: Object);
BEGIN
  IF Tag(s)=signal THEN
    CurSig:=s; Iterate(s^.TiedPins,SetSig);
  ELSIF Tag(s)=chip THEN
    CurChp:=s; Iterate(s^.Pins,SetChp);
  END;
END SetRel;

PROCEDURE ChkSig(p: Object);
BEGIN
  ASSERT(Tag(p)=pin);
  IF p^.Signal#CurSig THEN UnTie(CurSig^.TiedPins,p) END;
END ChkSig;

PROCEDURE ChkChp(p: Object);
BEGIN
  ASSERT(Tag(p)=pin);
  IF p^.Chip#CurChp THEN UnTie(CurChp^.Pins,p) END;
END ChkChp;

PROCEDURE ChkRel(s: Object);
BEGIN
  IF Tag(s)=signal THEN
    CurSig:=s; Iterate(s^.TiedPins,ChkSig);
  ELSIF Tag(s)=chip THEN
    CurChp:=s; Iterate(s^.Pins,ChkChp);
  END;
END ChkRel;

PROCEDURE FreeSig(s: Object);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF s^.Name='..free..' THEN FreeSignal:=s END;
END FreeSig;

PROCEDURE CheckPin(o: Object);
  VAR i: INTEGER; p: Object;
BEGIN
  IF Tag(o)#externalpin THEN
    UnTie(CheckedChip^.ChipType^.ExternalPins,o);
    RETURN
  END;
  i:=o^.EPinNo;
  p:=Lget(CheckedChip^.Pins,i);
  IF p=NIL THEN
    print('create free pin in chip %s\n',CheckedChip^.Name);
    p:=NewObject(pin);
    IF FreeSignal=NIL THEN Iterate(mdl^.All,FreeSig) END;
    ASSERT(FreeSignal#NIL);
    p^.Signal:=FreeSignal;
    Lset(CheckedChip^.Pins,i,p);
    Tie(mdl^.All,p);
  END;
  p^.No:=i; p^.Chip:=CheckedChip;
END CheckPin;

PROCEDURE Component(o: Object);
BEGIN
  IF Tag(o)#chip THEN RETURN END;
  CheckedChip:=o;
  Iterate(o^.ChipType^.ExternalPins,CheckPin);
END Component;

PROCEDURE DelSig(p: Object);
  VAR s: Object;
BEGIN
  s:=p^.Signal;
  IF s=NIL THEN RETURN END;
  UnTie(s^.TiedPins,p);
END DelSig;

PROCEDURE Pin(p: Object);
  VAR c,pc: Object;
BEGIN
  IF Tag(p)#pin THEN RETURN END;
  c:=p^.Chip;
  IF c=NIL THEN DelSig(p); UnTie(mdl^.All,p); RETURN END;
  pc:=Lget(c^.Pins,p^.No);
  IF pc=NIL THEN Lset(c^.Pins,p^.No,p); RETURN END;
  IF pc#p THEN DelSig(p); UnTie(mdl^.All,p) END;
END Pin;

BEGIN
  FreeSignal:=NIL;
  TakeWord(Name);
  mdl:=ReadModel(Name);
  Iterate(mdl^.All,SetRel);
  Iterate(mdl^.All,ChkRel);
  Iterate(mdl^.All,Component);
  Iterate(mdl^.All,Pin);
  WriteModel(mdl);
END bad.
