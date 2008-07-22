IMPLEMENTATION MODULE pedChipType; (*$U+ 09-Sep-89. (c) KRONOS *)

                IMPORT  mcd: defCodes;
FROM SYSTEM     IMPORT  ADDRESS, WORD;
FROM Model      IMPORT  Object, Objects, NewObject, Iterate, Tie, Lset, Lget,
                        List, Lsize, Osize, DoObject, DoList, DoNumber,
                        InitList, Do, CleareModel, Segment;
                IMPORT  ex: ModelPbl;
FROM cdsHeap    IMPORT  Allocate, Deallocate, Reallocate;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE Poz(o: Object): INTEGER;
CODE mcd.lsw0 mcd.lib 0FFh mcd.bic 8 mcd.shr END Poz;

PROCEDURE setPoz(o: Object; p: INTEGER);
CODE
  mcd.stot mcd.copt mcd.lsw0 mcd.lib 0FFh
  mcd.and mcd.lodt 8 mcd.shl mcd.add mcd.ssw0
END setPoz;

PROCEDURE move(t,f: ADDRESS; s: INTEGER); CODE mcd.move END move;

PROCEDURE CreateChipType(name: ARRAY OF CHAR): Object;
  VAR cht: Object;
BEGIN
  cht:=NewObject(chiptype); cht^.Name:=name; RETURN cht
END CreateChipType;

PROCEDURE GetFromFile (name: ARRAY OF CHAR; VAR cht: Object): BOOLEAN;
END GetFromFile;

PROCEDURE GetFromModel
          (name: ARRAY OF CHAR; mdl: Object; VAR cht: Object): BOOLEAN;
END GetFromModel;

VAR Res: Object;
    Key: ARRAY [0..15] OF CHAR;

PROCEDURE IterProc(o: Object; info: WORD);
BEGIN
  IF Tag(o)#chiptype THEN RETURN END;
  IF o^.Name=Key THEN Res:=o END;
END IterProc;

PROCEDURE FindChipType(nm: ARRAY OF CHAR; mdl: Object): Object;
BEGIN
  Key:=nm; Res:=NIL; Iterate(mdl^.All,IterProc,0); RETURN Res
END FindChipType;

VAR Table: DYNARR OF Object;
    cnt  : INTEGER;

PROCEDURE CpObject(of: Object; VAR ot: Object);

  PROCEDURE CpList(VAR lf, lt: List);
    VAR o: Object; i: INTEGER;
  BEGIN
    InitList(lt);
    FOR i:=0 TO Lsize(lf)-1 DO
      CpObject(Lget(lf,i),o); Lset(lt,i,o)
    END;
  END CpList;

BEGIN
  IF of=NIL THEN ot:=NIL; RETURN END;
  IF Poz(of)>=0 THEN ot:=Table[Poz(of)]; RETURN END;
  setPoz(of,cnt);
  IF Tag(of)#conductor THEN
    ot:=NewObject(Tag(of));
    move(ot,of,Osize(Tag(of)));
  ELSE
    Allocate(ot,3+of^.cLen*SIZE(Segment));
    move(ot,of,3+of^.cLen*SIZE(Segment));
  END;
  IF cnt>Table^.HIGH THEN
    IF NOT Reallocate(Table^.ADR,Table^.HIGH+1,Table^.HIGH+17) THEN
      ex.Message:='Переполнена динамическая память, очень жаль...';
      ex.RaiseInMe(ex.MemoryOverflow);
    END;
    INC(Table^.HIGH,16);
  END;
  Table[cnt]:=ot; INC(cnt);
  CASE Tag(of) OF
    signal:
      CpList  (of^.TiedPins,ot^.TiedPins);
      CpObject(of^.Bus,ot^.Bus);
      CpObject(of^.ChainB,ot^.ChainB);
      CpObject(of^.ChainD,ot^.ChainD);
   |pin:
      CpObject(of^.Signal,ot^.Signal);
      CpObject(of^.Chip,ot^.Chip);
   |externalpin:
      CpObject(of^.PinType,ot^.PinType);
      CpObject(of^.Host,ot^.Host);
   |chip:
      CpObject(of^.ChipType,ot^.ChipType);
      CpList  (of^.Pins,ot^.Pins);
   |chiptype:
      CpList  (of^.All,ot^.All);
      CpList  (of^.ExternalPins,ot^.ExternalPins);
   |bus:
      CpList  (of^.Signals,ot^.Signals);
      CpList  (of^.BusImage,ot^.BusImage);
   |conductor:
   |picture:
      CpObject(of^.pUp,ot^.pUp);
      CpObject(of^.pDown,ot^.pDown);
      CpObject(of^.pRight,ot^.pRight);
      CpObject(of^.pLeft,ot^.pLeft);
      CpObject(of^.pLines,ot^.pLines);
  END;
END CpObject;

PROCEDURE CopyToModel (cht,mdl: Object): BOOLEAN;
  VAR c: Object; e: ex.Reaction; i: INTEGER; r: BOOLEAN;
BEGIN
  IF FindChipType(mdl^.Name,cht)#NIL THEN RETURN TRUE END;
  CleareModel(cht,0);
  cnt:=0;
  r:=ex.Exception?(e);
  IF r THEN
    FOR i:=0 TO cnt-1 DO
      IF Tag(Table[i])=conductor THEN
        Deallocate(Table[i],Table[i]^.cLen*SIZE(Segment)+3);
      ELSE
        Deallocate(Table[i],Osize(Tag(Table[i])))
      END;
    END;
    Deallocate(Table^.ADR,Table^.HIGH+1); Table^.HIGH:=-1;
    ex.RaiseInMe(r)
  END;
  CpObject(cht,c);
  Deallocate(Table^.ADR,Table^.HIGH+1); Table^.HIGH:=-1;
  Tie(mdl^.All,c);
  ex.KillReaction(e);
  RETURN FALSE
END CopyToModel;

PROCEDURE LinkToModel (cht,mdl: Object): BOOLEAN;
BEGIN
  IF FindChipType(cht^.Name,mdl)#NIL THEN RETURN TRUE END;
  Tie(mdl^.All,cht); RETURN FALSE
END LinkToModel;

PROCEDURE CreateExtPin(no,x,y: INTEGER; cht: Object): BOOLEAN;
  VAR p: Object;
BEGIN
  IF Lget(cht^.ExternalPins,no)#NIL THEN RETURN TRUE END;
  p:=NewObject(externalpin);
  Lset(cht^.ExternalPins,no,p);
  p^.EPinNo:=no;
  p^.Host:=cht;
  p^.PinX:=x; p^.PinY:=y;
  RETURN FALSE;
END CreateExtPin;

PROCEDURE KillExtPin  (no: INTEGER; cht: Object): BOOLEAN;
END KillExtPin;

END pedChipType.

