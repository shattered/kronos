IMPLEMENTATION MODULE ModelIO; (* Sem 22-Jul-86. (c) KRONOS *)

IMPORT Model, ModelFS;
FROM Model     IMPORT  List, Lsize, Lget, Lset, Tie, InitList, Iterate,
                       ObjectRec, Objects, Osize, NewObject,
                       KillList, ListBody, CleareModel,
                       DoObject, DoNumber, DoList, Do, Segment,
                       Object;
FROM SYSTEM    IMPORT  ADDRESS, WORD, ADR;
FROM ModelFS   IMPORT  Open, Close, Create, sRead, sWrite, Seek;

IMPORT  mcd: defCodes;
IMPORT  mem: libHeap;
IMPORT  err: libCrash;

WITH STORAGE : mem;

CONST magic=22;

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
  IF INTEGER(Table[n])>0 THEN o:=Table[n]; RETURN END;
  Seek(-INTEGER(Table[n]));
  sRead(ADR(info),1);
  IF Tag(ADR(info))=conductor THEN
    sRead(ADR(s1),1);
    s:=s1*SIZE(Segment);
    mem.ALLOCATE(o,s+3);
    o^.cLen:=s1; o^.cFree:=s1;
    sRead(ADDRESS(o)+2,s+1);
  ELSE
    s:=Osize(Tag(ADR(info)));
    mem.ALLOCATE(o,s);
    sRead(ADDRESS(o)+1,s-1);
  END;
  o^.Info:=info;
  Table[n]:=o;
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
  IF TableCnt>HIGH(Table) THEN
    RESIZE(Table,HIGH(Table)+65);
  END;
  Table[TableCnt]:=o;
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
    Close; err.raise('Reading model - version conflict.');
  END;
  sRead(ADR(TableCnt),1);
  NEW(Table,TableCnt+1);
  sRead(ADR(Table),TableCnt);
  Seek(INTEGER(Table[0]));
  FOR i:=0 TO TableCnt-1 DO Table[i]:=Object(-INTEGER(Table[i])) END;
  sRead(o,Osize(Tag(o)));
  Table[0]:=o;
  DoObject:=RdObject; DoList:=RdList; DoNumber:=Empty; Do(o);
  Close();
  DISPOSE(Table);
END ReadModelBody;

PROCEDURE IniObject(VAR o: Object);
BEGIN o:=NIL END IniObject;

PROCEDURE IniList(VAR l: List);
BEGIN InitList(l) END IniList;

PROCEDURE ReadModel(nm: ARRAY OF CHAR): Object;
  VAR o: Object; r: BOOLEAN; re: err.trap; i: INTEGER;
BEGIN
  NEW(Table);
  r:=err.enter(re);
  IF r THEN
    IF ADR(Table)#NIL THEN
      FOR i:=0 TO HIGH(Table) DO
        IF INTEGER(Table[i])>0 THEN
          IF Tag(Table[i])=conductor THEN
            mem.DEALLOCATE(Table[i],Table[i]^.cLen*SIZE(Segment)+3);
          ELSE
            mem.DEALLOCATE(Table[i],Osize(Tag(Table[i])))
          END;
        END;
      END;
      DISPOSE(Table);
    END;
    err.re_raise(re);
  END;
  o:=NewObject(chiptype);
  move(ADR(o^.Name),ADR(nm),SIZE(o^.Name));
  o^.Name[HIGH(o^.Name)]:=0c;
  ReadModelBody(o);
  err.exit(re);
  RETURN o;
END ReadModel;

PROCEDURE WriteModel(o: Object);
  VAR i,s,poz: INTEGER; Buf: ARRAY [0..29] OF WORD;
      re: err.trap; r: BOOLEAN;
BEGIN
  ASSERT(Tag(o)=chiptype);
  r:=err.enter(re);
  IF r THEN
    DISPOSE(Table); err.re_raise(re);
  END;
  Create(o^.Name);
  MarkModel(o);
  i:=magic; sWrite(ADR(i),1);
  sWrite(ADR(TableCnt),1);
  poz:=ModelFS.Poz(); Seek(poz+TableCnt);
  FOR i:=0 TO TableCnt-1 DO
    o:=Table[i];
    IF Tag(o)=conductor THEN
      s:=3+o^.cLen*SIZE(Segment);
    ELSE
      s:=Osize(Tag(o));
      move(ADR(Buf),o,s);
      o:=ADR(Buf);
      DoObject:=WrObject; DoList:=WrList; DoNumber:=Empty; Do(o);
    END;
    Table[i]:=Object(ModelFS.Poz());
    sWrite(o,s);
  END;
  Seek(poz); sWrite(ADR(Table),TableCnt);
  Close();
  DISPOSE(Table);
  err.exit(re);
END WriteModel;

BEGIN
  Dummy:=-1;
END ModelIO.
