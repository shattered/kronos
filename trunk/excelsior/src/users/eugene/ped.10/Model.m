IMPLEMENTATION  MODULE Model; (* Sem 13-Sep-86. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM KRONOS    IMPORT   SHR;
FROM cdsHeap   IMPORT   Allocate, Deallocate, HardAllocate;
FROM ModelPbl  IMPORT   CrashInModel, RaiseInMe, Message;

CONST ListSize=16;

TYPE
ListBody=POINTER TO  ListItem;
ListItem=RECORD Nxt: ListBody; Obj: ARRAY [0..ListSize-1] OF Object END;

PROCEDURE InitList(VAR l: List);
BEGIN l.Size:=0; l.Body:=NIL; END InitList;

PROCEDURE Alloc(): ListBody;
  VAR i: CARDINAL; l: ListBody;
BEGIN
  HardAllocate(l,SIZE(l^));
  FOR i:=0 TO ListSize-1 DO l^.Obj[i]:=NIL  END;
  l^.Nxt:=NIL;
  RETURN l
END Alloc;

PROCEDURE Dispoze(VAR a: ListBody);
BEGIN
  Deallocate(a,SIZE(a^));
END Dispoze;

PROCEDURE Tie(VAR l: List; o: Object);
  VAR p: POINTER TO ListBody; i: CARDINAL;
BEGIN
  IF o=NIL THEN RETURN END;
  p:=ADR(l.Body);
  LOOP
    i:=0;
    IF p^=NIL THEN
      p^:=Alloc();
      INC(l.Size,ListSize);
      EXIT;
    END;
    WHILE (i<ListSize)&(p^^.Obj[i]#NIL) DO INC(i) END;
    IF (i<ListSize) THEN EXIT END;
    p:=ADR(p^^.Nxt);
  END;
  p^^.Obj[i]:=o;
END Tie;

PROCEDURE Truncate(VAR l: List);
  VAR p: POINTER TO ListBody; i: CARDINAL;
BEGIN
  LOOP
    p:=ADR(l.Body);
    IF p^=NIL THEN RETURN END;
    WHILE p^^.Nxt#NIL DO p:=ADR(p^^.Nxt) END;
    FOR i:=0 TO ListSize-1 DO IF p^^.Obj[i]#NIL THEN RETURN END; END;
    Dispoze(p^); DEC(l.Size,ListSize);
  END;
END Truncate;

PROCEDURE KillList(VAR l: List);
  VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO Lsize(l)-1 DO Lset(l,i,NIL) END;
  Truncate(l);
END KillList;

PROCEDURE UnTie(VAR l: List; o: Object);
  VAR p: POINTER TO ListBody; i: CARDINAL;
BEGIN
  ASSERT(o#NIL);
  p:=ADR(l.Body);
  LOOP
    IF p^=NIL THEN RETURN END;
    i:=0;
    FOR i:=0 TO ListSize-1 DO
      IF p^^.Obj[i]=o THEN p^^.Obj[i]:=NIL END;
    END;
    p:=ADR(p^^.Nxt);
  END;
END UnTie;

PROCEDURE Lset(VAR ls: List; n: CARDINAL; o: Object);
  VAR p: POINTER TO ListBody; i,l,h: CARDINAL;
BEGIN
  p:=ADR(ls.Body); l:=CARDINAL(BITSET(n)*{0..3}); h:=SHR(n,4);
  i:=0;
  LOOP
    IF p^=NIL THEN
      IF o=NIL THEN RETURN END;
      p^:=Alloc(); INC(ls.Size,ListSize);
    END;
    IF i=h THEN EXIT END;
    p:=ADR(p^^.Nxt); INC(i);
  END;
  p^^.Obj[l]:=o;
  IF o=NIL THEN Truncate(ls) END;
END Lset;

PROCEDURE Lget(VAR ls: List; n: CARDINAL): Object;
  VAR p: ListBody; i,l,h: CARDINAL;
BEGIN
  p:=ls.Body;
  l:=CARDINAL(BITSET(n)*{0..3}); h:=SHR(n,4);
  i:=0;
  LOOP
    IF p=NIL THEN RETURN NIL END;
    IF i=h   THEN RETURN p^.Obj[l] END;
    p:=p^.Nxt; INC(i);
  END;
END Lget;

PROCEDURE Iterate(VAR l: List; ip: IterProc);
  VAR p: ListBody; i: CARDINAL; o: Object;
BEGIN
  p:=l.Body;
  WHILE p#NIL DO
    FOR i:=0 TO ListSize-1 DO
      o:=p^.Obj[i];
      IF o#NIL THEN
--      IF (CARDINAL(o)<0)OR(CARDINAL(o)>=100000h) THEN
--        Message:='Object with illegal address.';
--        RaiseInMe(CrashInModel);
--      END;
        ip(o);
      END;
    END;
    p:=p^.Nxt;
  END;
END Iterate;

PROCEDURE Lsize(VAR l: List): CARDINAL;
BEGIN RETURN l.Size END Lsize;

PROCEDURE Osize(o: Objects): CARDINAL;
BEGIN
  CASE o OF
     signal     : RETURN 15
    |pin        : RETURN 5
    |externalpin: RETURN 15
    |chip       : RETURN 15
    |chiptype   : RETURN 15
    |bus        : RETURN 15
    |picture    : RETURN 15
  END;
END Osize;

PROCEDURE InitObject(VAR o: Object);
BEGIN o:=NIL END InitObject;

PROCEDURE NewObject(t: Objects): Object;
  VAR o: Object; i,s: CARDINAL;
      p: POINTER TO ARRAY [0..100] OF CARDINAL;
BEGIN
  IF t=conductor THEN s:=3 ELSE s:=Osize(t) END;
  Allocate(o,s);
  setTag(o,t); setPoz(o,0);
  p:=ADDRESS(o);
  FOR i:=1 TO s-1 DO p^[i]:=0 END;
  DoList:=InitList; DoObject:=InitObject;
  Do(Object(o));
  IF t=picture THEN o^.pRight:=o; o^.pLeft:=o END;
  INC(ObjectsNo);
  RETURN o;
END NewObject;

PROCEDURE KillObject(VAR o: Object);
  VAR s: INTEGER;
BEGIN
  IF Tag(o)=conductor THEN s:=3+o^.cLen*3 ELSE s:=Osize(Tag(o)) END;
  Deallocate(o,s);
  DEC(ObjectsNo);
END KillObject;

PROCEDURE CleareModel(o: Object);
  VAR c,e,l: Object;
BEGIN
  IF o=NIL THEN RETURN END;
  CASE Tag(o) OF
    signal:   setPoz(o,-1); c:=o^.ChainB;
              IF c#NIL THEN setPoz(c,-1) END;
   |picture:  e:=o;
              REPEAT
                setPoz(o,-1); l:=o^.pLines;
                IF l#NIL THEN setPoz(l,-1) END;
                l:=o^.pDown;
                IF l#NIL THEN CleareModel(l) END;
                o:=o^.pRight;
              UNTIL o=e;
   |chiptype: setPoz(o,-1);
              Iterate(o^.All,CleareModel);
              Iterate(o^.ExternalPins,CleareModel);
  ELSE        setPoz(o,-1);
  END;
END CleareModel;

PROCEDURE RemoveObject(o: Object);
  VAR c,c1,b,b1,e: Object;
BEGIN
  IF o=NIL THEN RETURN END;
  CASE Tag(o) OF
    chiptype: RemoveModel(o);
   |signal:
        c:=o^.ChainB;
        KillObject(c);
        KillList(o^.TiedPins);
        KillObject(o);
   |chip:
        KillList(o^.Pins);
        KillObject(o);
   |bus:
        KillList(o^.Signals);
        KillList(o^.BusImage);
        KillObject(o);
   |conductor: ASSERT(FALSE);
   |picture:
        b:=o; e:=o;
        REPEAT
          KillObject(b^.pLines);
          b1:=b^.pRight;
          KillObject(b);
          b:=b1;
        UNTIL b=e;
  ELSE
    KillObject(o);
  END;
END RemoveObject;

PROCEDURE RemoveModel(VAR o: Object);
BEGIN
  ASSERT(Tag(o)=chiptype);
  Iterate(o^.All,RemoveObject);
  Iterate(o^.ExternalPins,RemoveObject);
  KillList(o^.All);
  KillList(o^.ExternalPins);
  KillObject(o);
  ObjectsNo:=0;
END RemoveModel;

PROCEDURE Do(o: Object);
BEGIN
  CASE Tag(o) OF
    signal:
      DoList  (o^.TiedPins);
      DoObject(o^.Bus);
      DoObject(o^.ChainB);
      DoObject(o^.ChainD);
   |pin:
      DoObject(o^.Signal);
      DoObject(o^.Chip);
   |externalpin:
      DoObject(o^.PinType);
      DoObject(o^.Host);
   |chip:
      DoObject(o^.ChipType);
      DoList  (o^.Pins);
   |chiptype:
      DoList  (o^.All);
      DoList  (o^.ExternalPins);
   |bus:
      DoList  (o^.Signals);
      DoList  (o^.BusImage);
   |conductor:
   |picture:
      DoObject(o^.pUp);
      DoObject(o^.pDown);
      DoObject(o^.pRight);
      DoObject(o^.pLeft);
      DoObject(o^.pLines);
  END;
END Do;

BEGIN
  ObjectsNo:=0;
END Model.
