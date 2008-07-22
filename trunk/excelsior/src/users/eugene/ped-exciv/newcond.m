MODULE newcond; (* 19-Sep-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;
IMPORT  SYSTEM;
IMPORT  mcd: mCodeMnem;
IMPORT  mCodeMnem;
IMPORT  Args;
IMPORT  Model;
FROM Model      IMPORT  Objects;
IMPORT  io: ModelIO;
IMPORT  mm: ModelMisc;
IMPORT  tp: pedTopology;
IMPORT  ModelFS, ModelPbl, Strings, cdsHeap;

VAR mdl: Model.Object;
    nm : ARRAY [0..15] OF CHAR;

PROCEDURE Tag(o: Model.Object): Objects;
CODE 0 mcd.lxb END Tag;

MODULE oldModelIO;

IMPORT Model, ModelFS;
FROM Model     IMPORT  List, Lsize, Lget, Lset, Tie, InitList, Iterate,
                       ObjectRec, Objects, Osize, NewObject,
                       KillList, ListBody, CleareModel,
                       DoObject, DoNumber, DoList, Do, Segment,
                       Object;
FROM SYSTEM    IMPORT  ADDRESS, WORD, ADR;
FROM ModelFS   IMPORT  Open, Close, Create, sRead, sWrite, Seek;
FROM ModelPbl  IMPORT  RaiseInMe, IOerror, Message,
                       Exception?, Reaction, KillReaction;
FROM Strings   IMPORT  Str1;
FROM cdsHeap   IMPORT  Allocate, Deallocate, Reallocate;
IMPORT  mcd: mCodeMnem;

EXPORT ReadModel;

CONST magic=21;

VAR Dummy   : INTEGER;
    TableCnt: INTEGER;
    Table   : DYNARR OF Object;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE setTag(o: Object; t: Objects);
CODE mcd.stot 0 mcd.lodt mcd.sxb END setTag;

PROCEDURE Poz(o: Object): INTEGER;
CODE mcd.lsw0 mcd.lib 0FFh mcd.bic 8 mcd.shr END Poz;

PROCEDURE setPoz(o: Object; p: INTEGER);
CODE
  mcd.stot mcd.copt mcd.lsw0 mcd.lib 0FFh
  mcd.and mcd.lodt 8 mcd.shl mcd.add mcd.ssw0
END setPoz;

PROCEDURE move(from,to: ADDRESS; n: INTEGER); CODE mcd.move END move;

PROCEDURE Empty(VAR i: INTEGER); END Empty;

PROCEDURE RdObject(VAR o: Object); FORWARD;

PROCEDURE RdList(VAR l: List);
  VAR i,size,poz: INTEGER; o: Object;
BEGIN
  Seek(INTEGER(l.Body)); InitList(l);
  sRead(ADR(size),1);
  FOR i:=0 TO size-1 DO
    sRead(ADR(o),1); poz:=ModelFS.Poz();
    RdObject(o); Lset(l,i,o); Seek(poz);
  END;
END RdList;

PROCEDURE RdObject(VAR o: Object);
  VAR info,n,s,s1: INTEGER;
BEGIN
  IF o=Object(Dummy) THEN o:=NIL; RETURN END;
  n:=INTEGER(o);
  ASSERT(n>=0);
  ASSERT(n<TableCnt);
  IF INTEGER(Table^[n])>0 THEN o:=Table^[n]; RETURN END;
  Seek(-INTEGER(Table^[n]));
  sRead(ADR(info),1);
  IF Tag(ADR(info))=conductor THEN
    sRead(ADR(s1),1);
    s:=s1*SIZE(Segment);
    Allocate(o,s+2);
    o^.cLen:=s1; o^.cFree:=s1;
    sRead(ADDRESS(o)+2,s);
  ELSE
    s:=Osize(Tag(ADR(info)));
    Allocate(o,s);
    sRead(ADDRESS(o)+1,s-1);
  END;
  o^.Info:=info;
  Table^[n]:=o;
  DoObject:=RdObject; DoList:=RdList; DoNumber:=Empty; Do(o);
END RdObject;

PROCEDURE WrObject(VAR o: Object);
BEGIN
  IF o=NIL THEN o:=Object(Dummy) ELSE o:=Object(Poz(o)) END;
END WrObject;

PROCEDURE WrList(VAR l: List);
  VAR i,size,poz,n: INTEGER; o: Object;
BEGIN
  size:=Lsize(l); poz:=ModelFS.Poz();
  sWrite(ADR(size),1);
  FOR i:=0 TO size-1 DO
    o:=Lget(l,i);
    IF o#NIL THEN n:=Poz(o); sWrite(ADR(n),1) ELSE sWrite(ADR(Dummy),1) END;
  END;
  l.Body:=ListBody(poz);
END WrList;

PROCEDURE MarkObject(VAR o: Object); FORWARD;

PROCEDURE MarkList(VAR l: List);
  VAR i: INTEGER; o: Object;
BEGIN
  FOR i:=0 TO Lsize(l)-1 DO o:=Lget(l,i); MarkObject(o) END;
END MarkList;

PROCEDURE MarkObject(VAR o: Object);
  VAR i: INTEGER;
BEGIN
  IF o=NIL THEN RETURN END;
  IF Poz(o)>=0 THEN
    ASSERT(Poz(o)<=TableCnt,43h);
    ASSERT(Table[Poz(o)]=o,44h);
    RETURN
  END;
  setPoz(o,TableCnt);
  IF TableCnt>Table.HIGH THEN
    IF NOT Reallocate(Table.ADR,Table.HIGH+1,Table.HIGH+17) THEN HALT(1) END;
    INC(Table.HIGH,16);
  END;
  Table^[TableCnt]:=o;
  INC(TableCnt);
  Model.DoObject:=MarkObject; Model.DoList:=MarkList;
  Model.DoNumber:=Empty; Model.Do(o)
END MarkObject;

PROCEDURE MarkModel(o: Object);
BEGIN
  CleareModel(o,0);
  TableCnt:=0;
  MarkObject(o);
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
  Allocate(Table.ADR,TableCnt+1); Table.HIGH:=TableCnt;
  sRead(Table.ADR,TableCnt);
  Seek(INTEGER(Table^[0]));
  FOR i:=0 TO TableCnt-1 DO Table^[i]:=Object(-INTEGER(Table^[i])) END;
  sRead(o,Osize(Tag(o)));
  Table^[0]:=o;
  DoObject:=RdObject; DoList:=RdList; DoNumber:=Empty; Do(o);
  Close();
  Deallocate(Table.ADR,Table.HIGH+1); Table.HIGH:=-1;
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
  move(ADR(o^.Name),ADR(nm),SIZE(o^.Name));
  o^.Name[HIGH(o^.Name)]:=0c;
  sRead(ADR(i),1);
  IF i#magic THEN
    Close;
    Str1(Message,'Несоответствие версий при чтениии модели.');
    RaiseInMe(IOerror);
  END;
  sRead(ADR(TableCnt),1);
  Allocate(Table.ADR,TableCnt+1); Table.HIGH:=TableCnt;
  sRead(Table.ADR,TableCnt);
  Seek(INTEGER(Table^[0]));
  FOR i:=0 TO TableCnt-1 DO Table^[i]:=Object(-INTEGER(Table^[i])) END;
  sRead(o,Osize(Tag(o)));
  Table^[0]:=o;
  InitList(o^.All);
  RdList(o^.ExternalPins);
  Close();
  Deallocate(Table.ADR,Table.HIGH+1); Table.HIGH:=-1;
  RETURN o;
END ReadShortModel;

PROCEDURE ReadModel(nm: ARRAY OF CHAR): Object;
  VAR o: Object;
BEGIN
  o:=NewObject(chiptype);
  move(ADR(o^.Name),ADR(nm),SIZE(o^.Name));
  o^.Name[HIGH(o^.Name)]:=0c;
  ReadModelBody(o);
  RETURN o;
END ReadModel;

PROCEDURE WriteModel(o: Object);
  VAR i,s,poz: INTEGER; Buf: ARRAY [0..29] OF WORD;
      re: Reaction; r: BOOLEAN;
BEGIN
  ASSERT(Tag(o)=chiptype);
  r:=Exception?(re);
  IF r THEN
    Deallocate(Table.ADR,Table.HIGH+1); Table.HIGH:=-1;
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
      s:=3+o^.cLen*SIZE(Segment);
    ELSE
      s:=Osize(Tag(o));
      move(ADR(Buf),o,s);
      o:=ADR(Buf);
      DoObject:=WrObject; DoList:=WrList; DoNumber:=Empty; Do(o);
    END;
    Table^[i]:=Object(ModelFS.Poz());
    sWrite(o,s);
  END;
  Seek(poz); sWrite(Table.ADR,TableCnt);
  Close();
  Deallocate(Table.ADR,Table.HIGH+1); Table.HIGH:=-1;
  KillReaction(re);
END WriteModel;

BEGIN
  Dummy:=-1;
END oldModelIO;

PROCEDURE Seek(mdl: Model.Object); FORWARD;

PROCEDURE bidir_move(t,f: ADDRESS; s: INTEGER); CODE 096h END bidir_move;

PROCEDURE Seek0(s: Model.Object; ExtPin: BOOLEAN);
  TYPE pSegment = POINTER TO Model.Segment;
  VAR fId: INTEGER; c: Model.Object;
BEGIN
(*$T-*)
  IF s=NIL THEN RETURN END;
  IF ExtPin THEN
    IF (Tag(s)#externalpin)OR(s^.PinType=NIL) THEN RETURN END;
    c:=s^.PinType;
  ELSE
    IF Tag(s)=chiptype THEN Seek(s) END;
    IF (Tag(s)#signal)     OR(s^.ChainB =NIL) THEN RETURN END;
    c:=s^.ChainB;
  END;
  IF NOT cdsHeap.Reallocate(c,c^.cLen*SIZE(Model.Segment)+2,
                              c^.cLen*SIZE(Model.Segment)+3) THEN
    HALT(1)
  END;
  bidir_move(ADDRESS(c)+2,ADDRESS(c)+1,c^.cLen*SIZE(Model.Segment)+1);
  fId:=-1;
  LOOP
    REPEAT INC(fId) UNTIL (fId=c^.cFree) OR (c^.cType[fId].size=0);
    IF fId=c^.cFree THEN EXIT
    ELSE
      REPEAT DEC(c^.cFree) UNTIL (c^.cFree=fId) OR
                                 (c^.cType[c^.cFree].size#0);
      IF fId=c^.cFree THEN EXIT END;
      c^.cType[fId]:=c^.cType[c^.cFree]
    END;
  END;
  IF cdsHeap.Reallocate(c,c^.cLen*SIZE(Model.Segment)+3,
                          c^.cFree*SIZE(Model.Segment)+3) THEN END;
  c^.cLen:=c^.cFree;
  IF ExtPin THEN s^.PinType:=c ELSE s^.ChainB:=c END;
(*$T+*)
END Seek0;

PROCEDURE Seek(mdl: Model.Object);
  VAR i: INTEGER;
BEGIN
  Model.Iterate(mdl^.ExternalPins,Seek0,TRUE);
  Model.Iterate(mdl^.All,Seek0,FALSE);
END Seek;

BEGIN
  Args.ScanFlags;
  Args.TakeWord(nm); IF nm[0]=0c THEN HALT END;
  mdl:=ReadModel(nm);
  Seek(mdl);
  io.WriteModel(mdl);
END newcond.
