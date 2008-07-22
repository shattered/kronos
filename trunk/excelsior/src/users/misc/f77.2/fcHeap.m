IMPLEMENTATION MODULE fcHeap;

FROM SYSTEM  IMPORT ADDRESS, WORD, ADR;
FROM fcScan  IMPORT Fault;

FROM StdIO      IMPORT print;

CONST Wsz=6; CurSz=100;

VAR
    stgive:ARRAY [0..8] OF INTEGER;
    stfree:ARRAY [0..8] OF INTEGER;
    maxgive,totgive,totfree:INTEGER;
(*
    heap:ADDRESS;
    First:ARRAY [0..Wsz] OF WORD;
    Hfree,Sz:INTEGER;
    OS: ARRAY [0..99] OF ADDRESS; (* адреса взятых сегментов *)
    ns:INTEGER; (* счетчик OS *)
*)
    HEAP : ARRAY [0..9999] OF INTEGER;
    free,last,markpos:INTEGER;

PROCEDURE Give(VAR p:ADDRESS; sz:INTEGER);
BEGIN
(*
  IF sz<9 THEN INC(stgive[sz]); END;
  IF sz>maxgive THEN maxgive:=sz END;
  INC(totgive,sz);
  IF First[sz]#NIL THEN
    (*$T-*) p:=First[sz]; First[sz]:=p^; (*$T+*)
    RETURN ;
  END;
  IF (Sz-Hfree) < sz THEN Sz:=CurSz;
    Allocate(heap,Sz); IF heap=NIL THEN Fault(13); END;
    OS[ns]:=heap; INC(ns);
    Hfree:=0;
  END;
  INTEGER(p):=INTEGER(heap)+Hfree; INC(Hfree,sz);
*)
  p:=ADR(HEAP[free]); INC(free,sz);
  IF free>last THEN Fault(13) END;
END Give;

PROCEDURE Free(VAR p:ADDRESS; sz:INTEGER);
BEGIN
(*
  IF sz<9 THEN INC(stfree[sz]); END;
  INC(totfree,sz);
  p^:=First[sz]; (*$T-*) First[sz]:=p; (*$T+*)
*)
  p:=NIL;
END Free;

PROCEDURE InitHeap;
  VAR i: INTEGER;
BEGIN
(*
  FOR i:=0 TO 8 DO stgive[i]:=0; stfree[i]:=0; END;
  maxgive:=0; totgive:=0; totfree:=0;
  Hfree:=0; Sz:=CurSz;
  Allocate(heap,Sz); IF heap=NIL THEN Fault(13); END;
  OS[0]:=heap; ns:=1;
  FOR i:=1 TO Wsz DO First[i]:=NIL; END
*)
  free:=0; last:=HIGH(HEAP);
END InitHeap;

PROCEDURE ReleaseHeap;
  VAR i: INTEGER;
BEGIN
(*
print('Heap statistic: \n');
print('totgive=%-10d maxgive=%-4d totfree=%-8d \n',
       totgive,maxgive,totfree);
  FOR i:=0 TO 8 DO
  print(' i=%-2d give[i]=%-6d free[i]=%-6d \n',
          i, stgive[i], stfree[i]);
  stgive[i]:=0; stfree[i]:=0;
  END;
  maxgive:=0; totgive:=0; totfree:=0;
  Sz:=CurSz;
  FOR i:=0 TO ns-1 DO DeAllocate(OS[i],Sz); END;
*)
  free:=0;
END ReleaseHeap;

PROCEDURE MarkHeap;
BEGIN
  markpos:=free;
END MarkHeap;

PROCEDURE BackHeap;
BEGIN
  free:=markpos;
END BackHeap;

END fcHeap.
