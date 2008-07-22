IMPLEMENTATION MODULE pedChips; (* Ilx 14-Jun-89. (c) KRONOS *)

FROM Model      IMPORT  Objects;

FROM SYSTEM     IMPORT  WORD;
IMPORT  Model;
IMPORT  mm: ModelMisc;
IMPORT  pedScreen;
IMPORT  mcd: defCodes;
IMPORT  tp: pedTopology;
IMPORT  heap: cdsHeap;
FROM pedEditor  IMPORT  Sheet;
FROM pedTools   IMPORT  Tools;

PROCEDURE Tag(o: Model.Object): Model.Objects;
CODE 0 mcd.lxb END Tag;

VAR FindCp  : Model.Object;
    FindName: Model.String;
    X,Y     : INTEGER;

PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
CODE 0F9h 00h END in_rect;

PROCEDURE LookUpChip0(o: Model.Object; info: WORD);
BEGIN
  IF Tag(o)#Model.chip THEN RETURN END;
  IF (o^.ChipType=NIL)OR(o^.RB<0) THEN RETURN END;
  CASE o^.RB OF
     0: IF NOT in_rect(X-o^.XB,Y-o^.YB,
                       o^.ChipType^.ctX,o^.ChipType^.ctY) THEN RETURN END;
    |1: IF NOT in_rect(X-o^.XB,Y-(o^.YB-o^.ChipType^.ctX),
                       o^.ChipType^.ctY,o^.ChipType^.ctX) THEN RETURN END;
    |2: IF NOT in_rect(X-(o^.XB-o^.ChipType^.ctX),Y-(o^.YB-o^.ChipType^.ctY),
                       o^.ChipType^.ctX,o^.ChipType^.ctY) THEN RETURN END;
    |3: IF NOT in_rect(X-(o^.XB-o^.ChipType^.ctY),Y-o^.YB,
                       o^.ChipType^.ctY,o^.ChipType^.ctX) THEN RETURN END;
  ELSE END;
  FindCp:=o;
END LookUpChip0;

PROCEDURE LookUpChip(mdl: Model.Object; x,y: INTEGER): Model.Object;
BEGIN
  IF mdl=NIL THEN RETURN NIL END;
  FindCp:=NIL; X:=x; Y:=y;
  Model.Iterate(mdl^.All,LookUpChip0,0);
  RETURN FindCp
END LookUpChip;

PROCEDURE FindChip0(o: Model.Object; info: WORD);
BEGIN
  IF Tag(o)#Model.chip THEN RETURN END;
  IF o^.Name=FindName THEN FindCp:=o END;
END FindChip0;

PROCEDURE FindChip(mdl: Model.Object; name: ARRAY OF CHAR): Model.Object;
BEGIN
  IF (mdl=NIL) OR (name[0]=0c) THEN RETURN NIL END;
  FindCp:=NIL; FindName:=name;
  Model.Iterate(mdl^.All,FindChip0,0);
  RETURN FindCp
END FindChip;

VAR cnd: Model.Object;
    Ident: INTEGER;
    Empty: BOOLEAN;

(*$T-*)
PROCEDURE next_conductor;
BEGIN
  IF Empty THEN RETURN END;
  INC(Ident);
  IF (cnd=NIL)OR(Ident>=cnd^.cFree) THEN
    Empty:=TRUE; RETURN;
  ELSE
    Empty:=FALSE;
    mm.UnPackSeg(cnd^.cType[Ident]);
  END;
END next_conductor;
(*$T+*)

PROCEDURE start_conductor(s: Model.Object);
BEGIN
  IF s=NIL THEN Empty:=TRUE; RETURN END;
  ASSERT(Tag(s)=Model.externalpin);
  cnd:=s^.PinType; Ident:=-1; Empty:=FALSE;
  next_conductor;
END start_conductor;

VAR l,Size,ViasSize: INTEGER;

PROCEDURE set_conductor(o: Model.Object);
BEGIN
  CASE o^.Chip^.RB OF
    |0: tp.LineXe:= mm.X1+o^.Chip^.XB; tp.LineXs:= mm.X2+o^.Chip^.XB;
        tp.LineYe:= mm.Y1+o^.Chip^.YB; tp.LineYs:= mm.Y2+o^.Chip^.YB;
    |1: tp.LineXe:= mm.Y1+o^.Chip^.XB; tp.LineXs:= mm.Y2+o^.Chip^.XB;
        tp.LineYe:=-mm.X1+o^.Chip^.YB; tp.LineYs:=-mm.X2+o^.Chip^.YB;
    |2: tp.LineXe:=-mm.X1+o^.Chip^.XB; tp.LineXs:=-mm.X2+o^.Chip^.XB;
        tp.LineYe:=-mm.Y1+o^.Chip^.YB; tp.LineYs:=-mm.Y2+o^.Chip^.YB;
    |3: tp.LineXe:=-mm.Y1+o^.Chip^.XB; tp.LineXs:=-mm.Y2+o^.Chip^.XB;
        tp.LineYe:= mm.X1+o^.Chip^.YB; tp.LineYs:= mm.X2+o^.Chip^.YB;
  END;
  Size:=mm.Size; ViasSize:=mm.ViasSize;
  IF    mm.Layer={}   THEN l:=-1
  ELSIF 0 IN mm.Layer THEN l:=0
  ELSIF 1 IN mm.Layer THEN l:=1
  ELSE l:=-1 END;
END set_conductor;

PROCEDURE cre_pin(o,sig: Model.Object; sht: Sheet; chk: BOOLEAN): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  ASSERT(Tag(o)=Model.pin);
  start_conductor(Model.Lget(o^.Chip^.ChipType^.ExternalPins,o^.No));
  r:=FALSE;
  WHILE NOT Empty DO
    set_conductor(o);
    IF l>=0 THEN
      IF (tp.LineXe=tp.LineXs)&(tp.LineYe=tp.LineYs) THEN
        r:=r OR tp.InsertVias(Size,ViasSize,TRUE,sht,sig,chk);
      ELSE
        r:=r OR tp.InsertRange(Size,l,TRUE,sht,sig,chk);
      END;
    END;
    next_conductor;
  END;
  RETURN r;
END cre_pin;

PROCEDURE CrePin(o:Model.Object; sht: Sheet);
BEGIN
  IF cre_pin(o,o^.Signal,sht,sht^.PublicContext^.check_on) THEN END;
END CrePin;

PROCEDURE DelPin(o: Model.Object; sht: Sheet);
BEGIN
  ASSERT(Tag(o)=Model.pin);
  IF o^.Signal=NIL THEN RETURN END;
  start_conductor(Model.Lget(o^.Chip^.ChipType^.ExternalPins,o^.No));
  WHILE NOT Empty DO
    set_conductor(o);
    IF l>=0 THEN
      IF (tp.LineXe=tp.LineXs)&(tp.LineYe=tp.LineYs) THEN
        tp.DeleteVias(o^.Signal,sht);
      ELSE
        tp.DeleteRange(l,o^.Signal,sht);
      END;
    END;
    next_conductor;
  END;
END DelPin;

PROCEDURE LookUpPin(chip: Model.Object; x,y: INTEGER): Model.Object;
  VAR sht: Sheet; cb: INTEGER;
BEGIN
  ASSERT(Tag(chip)=Model.chip);
  IF (chip=NIL) OR (chip^.ChipType=NIL) OR (chip^.RB<0) THEN RETURN NIL END;
  heap.Allocate(sht,SIZE(sht^));
  heap.Allocate(sht^.PublicContext,SIZE(sht^.PublicContext^));
  sht^.wnd:=NIL; sht^.ScreenContext:=NIL;
  sht^.mdl:=chip^.ChipType;
  sht^.PublicContext^.ExtPin:=TRUE;
  CASE chip^.RB OF
    |0: x:= x-chip^.XB; y:= y-chip^.YB;
    |1: cb:=x;
        x:=-y+chip^.YB; y:=cb-chip^.XB;
    |2: x:=-x+chip^.XB; y:=-y+chip^.YB;
    |3: cb:=x;
        x:= y-chip^.YB; y:=-cb+chip^.XB;
  END;
  tp.FindSignal(x,y,0,0,sht,NIL);
  IF mm.Signal=NIL THEN
    tp.FindSignal(x,y,1,0,sht,NIL);
  END;
  sht^.PublicContext^.ExtPin:=FALSE;
  heap.Deallocate(sht^.PublicContext,SIZE(sht^.PublicContext^));
  heap.Deallocate(sht,SIZE(sht^));
  IF mm.Signal=NIL THEN RETURN NIL END;
  RETURN Model.Lget(chip^.Pins,mm.Signal^.EPinNo);
END LookUpPin;

PROCEDURE UpdatePin(sht: Sheet; pin: Model.Object);
BEGIN;
  WITH sht^.PublicContext^ DO
    IF LastPin#NIL THEN
      start_conductor(Model.Lget(LastPin^.Chip^.ChipType^.ExternalPins,
                                 LastPin^.No));
      WHILE NOT Empty DO
        set_conductor(LastPin);
        mm.X1:=tp.LineXs; mm.X2:=tp.LineXe;
        mm.Y1:=tp.LineYs; mm.Y2:=tp.LineYe;
        mm.Layer:={2}; pedScreen.Delete(sht);
        next_conductor;
      END;
    END;
    IF pin#NIL THEN
      start_conductor(Model.Lget(pin^.Chip^.ChipType^.ExternalPins,
                                 pin^.No));
      WHILE NOT Empty DO
        set_conductor(pin);
        mm.X1:=tp.LineXs; mm.X2:=tp.LineXe;
        mm.Y1:=tp.LineYs; mm.Y2:=tp.LineYe;
        INCL(mm.Layer,2); pedScreen.Drow(sht);
        next_conductor;
      END;
    END;
  END;
END UpdatePin;

PROCEDURE Connect(sig,pn: Model.Object);
  VAR sg: Model.Object;
BEGIN
  ASSERT((Tag(sig)=Model.signal) & (Tag(pn)=Model.pin));
  IF pn^.Signal=sig THEN RETURN END;
  Model.UnTie(pn^.Signal^.TiedPins,pn);
  Model.Tie(sig^.TiedPins,pn); pn^.Signal:=sig;
END Connect;

PROCEDURE RenamePin(sht: Sheet; p,s: Model.Object; chk: BOOLEAN): BOOLEAN;
  VAR x,y: INTEGER; sg: Model.Object;
BEGIN
  ASSERT((Tag(p)=Model.pin) & (Tag(s)=Model.signal));
  DelPin(p,sht);
  IF chk THEN
    IF cre_pin(p,s,sht,TRUE) THEN
      sg:=p^.Signal; p^.Signal:=s; DelPin(p,sht);
      p^.Signal:=sg; CrePin(p,sht); RETURN TRUE
    END;
    Connect(s,p);
  ELSE
    Connect(s,p); IF cre_pin(p,s,sht,FALSE) THEN END;
  END;
  RETURN FALSE
END RenamePin;

PROCEDURE KillPin(p: Model.Object; info: WORD);
  VAR sht: Sheet;
BEGIN
  sht:=info;
  Model.UnTie(p^.Signal^.TiedPins,p);
  Model.UnTie(sht^.mdl^.All,p);
  Model.KillObject(p);
END KillPin;

PROCEDURE MoveChip(sht: Sheet; chip: Model.Object; x,y,r: INTEGER): BOOLEAN;
BEGIN
  IF chip^.RB>=0 THEN Model.Iterate(chip^.Pins,DelPin,sht) END;
  chip^.XB:=x; chip^.YB:=y; chip^.RB:=r;
  IF chip^.RB>=0 THEN Model.Iterate(chip^.Pins,CrePin,sht) END;
  RETURN FALSE;
END MoveChip;

PROCEDURE KillChip(sht: Sheet; chip: Model.Object): BOOLEAN;
BEGIN
  IF (sht^.mdl=NIL) OR (chip=NIL) THEN RETURN TRUE END;
  IF chip^.RB>=0 THEN Model.Iterate(chip^.Pins,DelPin,sht) END;
  Model.Iterate(chip^.Pins,KillPin,sht);
  Model.UnTie(sht^.mdl^.All,chip);
  Model.KillObject(chip);
  RETURN TRUE
END KillChip;

VAR Res: Model.Object; Key: ARRAY [0..15] OF CHAR;

PROCEDURE IterProc(o: Model.Object; info: INTEGER);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name=Key THEN
    Res:=o;
  END;
END IterProc;

PROCEDURE FindSignal(nm: ARRAY OF CHAR; mdl: Model.Object): Model.Object;
BEGIN
  Key:=nm; Res:=NIL;
  Model.Iterate(mdl^.All,IterProc,0);
  IF Res#NIL THEN RETURN Res END;
  Res:=Model.NewObject(signal);
  Res^.Name:=nm; Model.Tie(mdl^.All,Res);
  RETURN Res;
END FindSignal;

VAR CreChip: Model.Object;

PROCEDURE NewPin(p: Model.Object; sht: Sheet);
  VAR pn, sig: Model.Object;
BEGIN
  ASSERT(Tag(p)=externalpin);
  pn:=Model.Lget(CreChip^.Pins,p^.EPinNo);
  IF pn=NIL THEN
    pn:=Model.NewObject(pin);
    pn^.No:=p^.EPinNo; pn^.Chip:=CreChip;
    Model.Lset(CreChip^.Pins,p^.EPinNo,pn);
    Model.Tie(sht^.mdl^.All,pn);
    sig:=FindSignal('..free..',sht^.mdl);
    pn^.Signal:=sig;
    Model.Tie(sig^.TiedPins,pn);
  END;
END NewPin;

PROCEDURE NewChip (sht: Sheet; chip_type: Model.Object;
                   name: ARRAY OF CHAR; VAR chp: Model.Object): BOOLEAN;
BEGIN
  IF (sht^.mdl=NIL) OR (chip_type=NIL) OR (name[0]=0c) THEN RETURN TRUE END;
  chp:=FindChip(sht^.mdl,name);
  IF chp#NIL THEN RETURN TRUE END;
  chp:=Model.NewObject(chip); chp^.Name:=name;
  chp^.ChipType:=chip_type; Model.Tie(sht^.mdl^.All,chp);
  CreChip:=chp;
  Model.Iterate(chip_type^.ExternalPins,NewPin,sht);
  RETURN FALSE;
END NewChip;

PROCEDURE DeleteMetal(sht: Sheet; chip: Model.Object): BOOLEAN;
BEGIN
  IF chip^.RB>=0 THEN Model.Iterate(chip^.Pins,DelPin,sht) END;
  RETURN FALSE;
END DeleteMetal;

PROCEDURE InsertMetal(sht: Sheet; chip: Model.Object): BOOLEAN;
BEGIN
  IF chip^.RB>=0 THEN Model.Iterate(chip^.Pins,CrePin,sht) END;
  RETURN FALSE;
END InsertMetal;

PROCEDURE IAP(c: Model.Object; sht: Sheet);
BEGIN
  IF Tag(c)#chip THEN RETURN END;
  IF MoveChip(sht,c,c^.XB,c^.YB,c^.RB) THEN END;
END IAP;

PROCEDURE InsertAllPins(sht: Sheet; check?: BOOLEAN): BOOLEAN;
BEGIN
  Model.Iterate(sht^.mdl^.All,IAP,sht);
  RETURN FALSE;
END InsertAllPins;

END pedChips.
