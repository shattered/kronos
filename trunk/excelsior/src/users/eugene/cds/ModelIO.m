IMPLEMENTATION MODULE ModelIO; (* Sem 22-Jul-86. (c) KRONOS *)

IMPORT Model, ModelFS;
FROM Model     IMPORT  List, Lsize, Lget, Lset, Tie, InitList, Iterate,
                       ObjectRec, Objects, Osize, NewObject, ObjectsNo,
                       KillList, ListBody, CleareModel, Poz, setPoz, Tag,
                       setTag, DoObject, DoNumber, DoList, Do, Segment,
                       Object;
FROM KRONOS    IMPORT  MOVE;
FROM SYSTEM    IMPORT  ADDRESS, WORD, ADR;
FROM ModelFS   IMPORT  Open, Close, Create, sRead, sWrite, Seek;
FROM ModelPbl  IMPORT  RaiseInMe, IOerror, Message,
                       Exception?, Reaction, KillReaction;
FROM Strings   IMPORT  Str1;
FROM cdsHeap   IMPORT  Allocate, HardAllocate, Deallocate;

CONST magic=22;

(*$T-*)

VAR Table   : POINTER TO ARRAY [0..0] OF Object;
    Wsp     : ARRAY [0..2999] OF INTEGER;
    TableCnt: INTEGER;
    Dummy   : INTEGER;
    ReadObject, WriteObject: PROCEDURE (VAR Object);

PROCEDURE Empty(VAR i: INTEGER);
END Empty;

PROCEDURE RdList(VAR l: List);
  VAR i,size,poz: CARDINAL; o: Object;
BEGIN
  Seek(INTEGER(l.Body)); InitList(l);
  sRead(ADR(size),1);
  FOR i:=0 TO size-1 DO
    sRead(ADR(o),1); poz:=ModelFS.Poz();
    ReadObject(o); Lset(l,i,o); Seek(poz);
  END;
END RdList;

PROCEDURE RdObject(VAR o: Object);
  VAR info,n,s,s1: CARDINAL;
BEGIN
  IF o=Object(Dummy) THEN o:=NIL; RETURN END;
  n:=CARDINAL(o);
  ASSERT(n>=0);
  ASSERT(n<TableCnt);
  IF INTEGER(Table^[n])>0 THEN o:=Table^[n]; RETURN END;
  Seek(-INTEGER(Table^[n]));
  sRead(ADR(info),1);
  IF Tag(ADR(info))=conductor THEN
    sRead(ADR(s1),1);
    s:=s1*SIZE(Segment);
    Allocate(o,s+3);
    o^.cLen :=s1;
    o^.cFree:=s1;
    sRead(ADDRESS(INTEGER(o)+2),s+1);
  ELSE
    s:=Osize(Tag(ADR(info)));
    Allocate(o,s);
    sRead(ADDRESS(INTEGER(o)+1),s-1);
  END;
  o^.Info:=info;
  INC(ObjectsNo);
  Table^[n]:=o;
  IF Tag(o)=chiptype THEN
    InitList(o^.All);
    RdList(o^.ExternalPins);
    RETURN
  END;
  DoObject:=RdObject; DoList:=RdList; DoNumber:=Empty; Do(o);
END RdObject;

PROCEDURE WrObject(VAR o: Object);
BEGIN
  IF o=NIL THEN o:=Object(Dummy) ELSE o:=Object(Poz(o)) END;
END WrObject;

PROCEDURE WrList(VAR l: List);
  VAR i,size,poz,n: CARDINAL; o: Object;
BEGIN
  size:=Lsize(l); poz:=ModelFS.Poz();
  sWrite(ADR(size),1);
  FOR i:=0 TO size-1 DO
    o:=Lget(l,i);
    IF o#NIL THEN n:=Poz(o); sWrite(ADR(n),1) ELSE sWrite(ADR(Dummy),1) END;
  END;
  l.Body:=ListBody(poz);
END WrList;

VAR MarkObjectV: PROCEDURE (VAR Object);

PROCEDURE MarkList(VAR l: List);
  VAR i: CARDINAL; o: Object;
BEGIN
  FOR i:=0 TO Lsize(l)-1 DO o:=Lget(l,i); MarkObjectV(o) END;
END MarkList;

PROCEDURE MarkObject(VAR o: Object);
BEGIN
  IF o=NIL THEN RETURN END;
  IF Poz(o)>=0 THEN
    ASSERT(Poz(o)<TableCnt);
    ASSERT(Table^[Poz(o)]=o);
    RETURN
  END;
  ASSERT(TableCnt<ObjectsNo);
  setPoz(o,TableCnt); Table^[TableCnt]:=o; INC(TableCnt);
  IF Tag(o)#chiptype THEN
    Model.DoObject:=MarkObject; Model.DoList  :=MarkList;
    Model.DoNumber:=Empty; Model.Do(o)
  ELSE
    MarkList(o^.ExternalPins);
  END;
END MarkObject;

PROCEDURE MarkModel(o: Object);
BEGIN
  CleareModel(o);
  TableCnt:=0;
  MarkObject(o);
  Model.DoObject:=MarkObject; Model.DoList  :=MarkList;
  Model.DoNumber:=Empty; Model.Do(o)
END MarkModel;

PROCEDURE ReadModelBody(o: Object);
  VAR i: INTEGER;
BEGIN
  ASSERT(Tag(o)=chiptype);
  KillList(o^.ExternalPins);
  Open(o^.Name);
  sRead(ADR(i),1);
  IF i#magic THEN
    Close;
    Str1(Message,'Reading model - version conflict.');
    RaiseInMe(IOerror);
  END;
  sRead(ADR(TableCnt),1);
  HardAllocate(Table,TableCnt);
  sRead(Table,TableCnt);
  Seek(CARDINAL(Table^[0]));
  FOR i:=0 TO TableCnt-1 DO Table^[i]:=Object(-INTEGER(Table^[i])) END;
  sRead(o,Osize(Tag(o)));
  Table^[0]:=o;
  DoObject:=RdObject; DoList:=RdList; DoNumber:=Empty; Do(o);
  Close();
  Deallocate(Table,TableCnt);
END ReadModelBody;

PROCEDURE IniObject(VAR o: Object);
BEGIN o:=NIL END IniObject;

PROCEDURE IniList(VAR l: List);
BEGIN InitList(l) END IniList;

PROCEDURE ReadShortModel(nm: ARRAY OF CHAR): Object;
  VAR i: INTEGER; o: Object;
BEGIN
  Open(nm);
  o:=NewObject(chiptype);
  MOVE(ADR(o^.Name),ADR(nm),SIZE(o^.Name));
  o^.Name[HIGH(o^.Name)]:=0c;
  sRead(ADR(i),1);
  IF i#magic THEN
    Close;
    Str1(Message,'Несоответствие версий при чтениии модели.');
    RaiseInMe(IOerror);
  END;
  sRead(ADR(TableCnt),1);
  HardAllocate(Table,TableCnt);
  sRead(Table,TableCnt);
  Seek(CARDINAL(Table^[0]));
  FOR i:=0 TO TableCnt-1 DO Table^[i]:=Object(-INTEGER(Table^[i])) END;
  sRead(o,Osize(Tag(o)));
  Table^[0]:=o;
  InitList(o^.All);
  RdList(o^.ExternalPins);
  Close();
  Deallocate(Table,TableCnt);
  RETURN o;
END ReadShortModel;

PROCEDURE ReadModel(nm: ARRAY OF CHAR): Object;
  VAR o: Object;
BEGIN
  o:=NewObject(chiptype);
  MOVE(ADR(o^.Name),ADR(nm),SIZE(o^.Name));
  o^.Name[HIGH(o^.Name)]:=0c;
  ReadModelBody(o);
  RETURN o;
END ReadModel;

PROCEDURE WriteModel(o: Object);
  VAR i,j,s,poz: INTEGER; Buf: ARRAY [0..29] OF WORD;
      re: Reaction; r: BOOLEAN;
BEGIN
  ASSERT(Tag(o)=chiptype);
  IF ObjectsNo>SIZE(Wsp) THEN
    HardAllocate(Table,ObjectsNo);
  ELSE
    Table:=ADR(Wsp);
  END;
  r:=Exception?(re);
  IF r THEN
    IF Table#ADR(Wsp) THEN Deallocate(Table,ObjectsNo) ELSE Table:=NIL END;
    RaiseInMe(r)
  END;
  Create(o^.Name);
  MarkModel(o);
  i:=magic; sWrite(ADR(i),1);
  sWrite(ADR(TableCnt),1);
  poz:=ModelFS.Poz(); Seek(poz+TableCnt);
  FOR i:=0 TO TableCnt-1 DO
    o:=Table^[i];
    IF Tag(o)=conductor THEN
      o^.cFree:=o^.cLen;
      j:=0;
      WHILE j<o^.cFree DO
        IF o^.cType[j].size=0 THEN
          o^.cType[j]:=o^.cType[o^.cFree-1];
          DEC(o^.cFree); o^.cType[o^.cFree].size:=0;
        ELSE
          INC(j);
        END;
      END;
      s:=3+o^.cFree*SIZE(Segment);
    ELSE
      s:=Osize(Tag(o));
      MOVE(ADR(Buf),o,s);
      o:=ADR(Buf);
      DoObject:=WrObject; DoList:=WrList; DoNumber:=Empty; Do(o);
    END;
    Table^[i]:=Object(ModelFS.Poz());
    sWrite(o,s);
  END;
  Seek(poz); sWrite(Table,TableCnt);
  Close();
  IF Table#ADR(Wsp) THEN Deallocate(Table,ObjectsNo) ELSE Table:=NIL END;
  KillReaction(re);
END WriteModel;

BEGIN
  Table:=NIL;
  Dummy:=-1;
  ReadObject:=RdObject;
  WriteObject:=WrObject;
  MarkObjectV:=MarkObject;
END ModelIO.
