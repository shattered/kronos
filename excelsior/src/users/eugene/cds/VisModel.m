IMPLEMENTATION MODULE VisModel;(* Sem 22-Jul-86. (c) KRONOS *)

FROM StdIO     IMPORT Show, WriteString, WriteLn, Write, WriteInt,
                      WriteLeft, StdOut, WriteNum;
FROM Model     IMPORT Iterate;

VAR i  : CARDINAL;
    Poz: CARDINAL;

PROCEDURE ShowPins(o: Object);
BEGIN
  IF Poz>=4 THEN Poz:=0; WriteLn END;
  WriteInt(o^.No+1,3); Write(' ');
  IF o^.Chip#NIL THEN
    WriteLeft(o^.Chip^.Name,15)
  ELSE
    WriteString(' +++++++++++++ ')
  END;
  INC(Poz);
END ShowPins;

PROCEDURE ShowPins1(o: Object);
BEGIN
  IF Poz>=4 THEN Poz:=0; WriteLn END;
  WriteInt(o^.No+1,3); Write(' ');
  IF o^.Signal#NIL THEN
    WriteLeft(o^.Signal^.Name,15)
  ELSE
    WriteString(' +++++++++++++ ')
  END;
  INC(Poz);
END ShowPins1;

PROCEDURE ShowPins2(o: Object);
BEGIN
  IF Poz>=4 THEN Poz:=0; WriteLn END;
  WriteInt(o^.EPinNo+1,3); Write(' ');
  IF o^.Host#NIL THEN
    WriteLeft(o^.Host^.Name,15)
  ELSE
    WriteString(' +++++++++++++ ')
  END;
  INC(Poz);
END ShowPins2;

PROCEDURE VisObject(o: Object);
  VAR Sv: Stream;
BEGIN
  Sv:=StdOut; StdOut:=Output;
  CASE o^.Tag OF
    signal:
      WriteString('Signal '); Show(o^.Name);
      Poz:=0; Iterate(o^.TiedPins,ShowPins); IF Poz>0 THEN WriteLn END;
   |pin:
      WriteString('Pin '); WriteString(o^.Name);
      WriteInt(o^.No,4); Write(' ');
      IF o^.Chip#NIL THEN
        WriteLeft(o^.Chip^.Name,15)
      ELSE
        WriteString(' +++++++++++++ ')
      END;
      WriteLn;
   |chip:
      WriteString('Chip '); WriteString(o^.Name);
      IF o^.ChipType#NIL THEN
        WriteString(', type '); Show(o^.ChipType^.Name);
      ELSE
        WriteLn;
      END;
      WriteNum('    page %d*%\n',o^.Image);
      Poz:=0; Iterate(o^.Pins,ShowPins1); IF Poz>0 THEN WriteLn; END;
   |chiptype:
      WriteString('Chip type '); Show(o^.Name);
      Poz:=0; Iterate(o^.ExternalPins,ShowPins2); IF Poz>0 THEN WriteLn; END;
   |bus:
   |conductor:
  END;
  StdOut:=Sv;
END VisObject;

BEGIN
  Output:=StdOut;
END VisModel.
