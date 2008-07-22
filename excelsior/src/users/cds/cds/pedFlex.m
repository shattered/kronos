IMPLEMENTATION MODULE pedFlex; (*  15-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;

IMPORT  cod : defCodes;

IMPORT  mio : ModelIO;
IMPORT  mod : Model;
IMPORT  mdl : pedModel;
IMPORT  cu  : pedCU;

VAR
  old  : mod.Object;
  new  : mdl.board;
  X1,Y1: INTEGER;
  X2,Y2: INTEGER;
  Size : INTEGER;
  ViasSize: INTEGER;
  Layer: BITSET;
  Fixed: BOOLEAN;

PROCEDURE Tag(o: mod.Object): mod.Objects; CODE 0 cod.lxb END Tag;

PROCEDURE Poz(o: mod.Object): INTEGER;
CODE
  cod.lsw0 cod.lib 0FFh cod.bic 8 cod.shr
END Poz;

PROCEDURE setPoz(o: mod.Object; p: INTEGER);
CODE
  cod.stot cod.copt cod.lsw0 cod.lib 0FFh
  cod.and cod.lodt 8 cod.shl cod.add cod.ssw0
END setPoz;

PROCEDURE copy_types(t: mod.Object; x: WORD);
  VAR i,no: INTEGER; ct: mdl.ctype; ep: mod.Object;
BEGIN
  IF Tag(t)#mod.chiptype THEN RETURN END;
  no:=0;
  WHILE mod.Lget(t^.ExternalPins,no)#NIL DO INC(no) END;
  mdl.cre_ctype(ct,no,new);
  i:=0;
  WHILE (i<HIGH(ct^.name)) & (i<=HIGH(t^.Name)) & (t^.Name[i]#0c) DO
    ct^.name[i]:=t^.Name[i]; INC(i);
  END;
  ct^.name[i]:=0c;
  ct^.x:=t^.ctX; ct^.y:=t^.ctY;
  FOR no:=0 TO HIGH(ct^.pins) DO
    ep:=mod.Lget(t^.ExternalPins,no);
    ASSERT(Tag(ep)=mod.externalpin);
    ASSERT(ep^.EPinNo=no);
    ct^.pins[no].x:=ep^.PinX;
    ct^.pins[no].y:=ep^.PinY;
    ct^.pins[no].tool:=ep^.TrackWidth;
  END;
  i:=HIGH(new^.typs);
  WHILE new^.typs[i]#ct DO DEC(i) END;
  setPoz(t,i);
END copy_types;

PROCEDURE UnPackSeg(VAR s: mod.Segment);
  CONST mask={0..15};
BEGIN
  WITH s DO
    Size:=INTEGER(BITSET(size<<12)*{0..11});
    IF 1 IN BITSET(size) THEN
      X1:=INTEGER(BITSET(start)*mask)+Size;
      Y1:=INTEGER(BITSET(start>>16)*mask)+Size;
      X2:=INTEGER(BITSET(end)*mask)-Size;
      Y2:=INTEGER(BITSET(end>>16)*mask)-Size;
    ELSE
      X1:=INTEGER(BITSET(start)*mask)+Size;
      Y1:=INTEGER(BITSET(end>>16)*mask)-Size;
      Y2:=INTEGER(BITSET(start>>16)*mask)+Size;
      X2:=INTEGER(BITSET(end)*mask)-Size;
    END;
    DEC(X1,8000h); DEC(X2,8000h); DEC(Y1,8000h); DEC(Y2,8000h);
    Layer:=BITSET(size>>12)*{0..7};
    ViasSize:=INTEGER(BITSET(size>>4)*{0..7});
    Fixed:=0 IN BITSET(size);
  END;
END UnPackSeg;

PROCEDURE copy_sigs(sg: mod.Object; x: WORD);
  VAR i,no: INTEGER; seg: cu.segment; s: mdl.signal;
BEGIN
  IF Tag(sg)#mod.signal THEN RETURN END;
  mdl.cre_signal(s,new);
  i:=0;
  WHILE (i<HIGH(s^.name)) & (i<=HIGH(sg^.Name)) & (sg^.Name[i]#0c) DO
    s^.name[i]:=sg^.Name[i]; INC(i);
  END;
  s^.name[i]:=0c;
  s^.type:=mdl.signal_type{};
  IF mod.power  IN sg^.sType THEN INCL(s^.type,mdl.power) END;
  IF mod.fantom IN sg^.sType THEN INCL(s^.type,mdl.fantom) END;
  IF mod.fixed  IN sg^.sType THEN INCL(s^.type,mdl.fixed) END;
  IF sg^.ChainB#NIL THEN
    no:=0;
    FOR i:=0 TO sg^.ChainB^.cLen-1 DO
(*$T-*)
      IF sg^.ChainB^.cType[i].size#0 THEN
        UnPackSeg(sg^.ChainB^.cType[i]);
(*$T+*)
        IF (ViasSize#0) & ((X1#X2) OR (Y1#Y2)) THEN INC(no) END;
        INC(no);
      END;
    END;
    cu.extend(s,no); no:=0;
    FOR i:=0 TO sg^.ChainB^.cLen-1 DO
(*$T-*)
      IF sg^.ChainB^.cType[i].size#0 THEN
        UnPackSeg(sg^.ChainB^.cType[i]);
(*$T+*)
        IF (ViasSize#0) & ((X1#X2) OR (Y1#Y2)) THEN
          seg.x1:=X1; seg.y1:=Y1;
          seg.x2:=X1; seg.y2:=Y1;
          seg.size:=Size;
          seg.lays:=Layer;
          seg.pipe:=ViasSize;
          seg.fix:=Fixed;
          cu.pack(s,no,seg); INC(no);
          ViasSize:=0;
        END;
        seg.x1:=X1; seg.y1:=Y1;
        seg.x2:=X2; seg.y2:=Y2;
        seg.size:=Size;
        seg.lays:=Layer;
        seg.pipe:=ViasSize;
        seg.fix:=Fixed;
        cu.pack(s,no,seg); INC(no);
      END;
    END;
  END;
  i:=HIGH(new^.sigs);
  WHILE new^.sigs[i]#s DO DEC(i) END;
  setPoz(sg,i);
END copy_sigs;

PROCEDURE copy_chips(oc: mod.Object; x: WORD);
  VAR
    i : INTEGER;
    t : mdl.ctype;
    s : mdl.signal;
    c : mdl.chip;
    op: mod.Object;
BEGIN
  IF Tag(oc)#mod.chip THEN RETURN END;
  t:=new^.typs[Poz(oc^.ChipType)];
  mdl.cre_chip(c,new,t);
  i:=0;
  WHILE (i<HIGH(c^.name)) & (i<=HIGH(oc^.Name)) & (oc^.Name[i]#0c) DO
    c^.name[i]:=oc^.Name[i]; INC(i);
  END;
  c^.name[i]:=0c;
  c^.x:=oc^.XB;
  c^.y:=oc^.YB;
  c^.r:=oc^.RB;
  FOR i:=0 TO HIGH(c^.pins) DO
    op:=mod.Lget(oc^.Pins,i);
    IF op^.Signal^.Name#'..free..' THEN
      s:=new^.sigs[Poz(op^.Signal)];
      mdl.tie(s,c,i,new);
    END;
  END;
END copy_chips;

PROCEDURE read_model(nm: ARRAY OF CHAR): mdl.board;
  VAR i: INTEGER;
BEGIN
  old:=mio.ReadModel(nm);
  mdl.cre_board(new);
  i:=0;
  WHILE (i<HIGH(new^.name)) & (i<=HIGH(old^.Name)) & (old^.Name[i]#0c) DO
    new^.name[i]:=old^.Name[i]; INC(i);
  END;
  new^.name[i]:=0c;
  new^.x:=old^.ctX; new^.y:=old^.ctY; new^.lays:={0..1};
  mod.Iterate(old^.All,copy_types,0);
  mod.Iterate(old^.All,copy_sigs,0);
  mod.Iterate(old^.All,copy_chips,0);
  mod.RemoveModel(old,0);
  RETURN new;
END read_model;

END pedFlex.
