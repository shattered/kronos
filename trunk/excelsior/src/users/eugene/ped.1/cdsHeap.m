IMPLEMENTATION MODULE cdsHeap; (* Sem 04-Jun-87. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM SYSTEM    IMPORT   ADR, PROCESS, TRANSFER, ADDRESS;
FROM KRONOS    IMPORT   MySelf;
FROM mCodeMnem IMPORT   abs, copt, lsw0, jfsc, jfs, lss, drop, lsw1, alloc;
FROM Universe  IMPORT   NewPrs, ProcessDesc;

FROM ModelPbl  IMPORT   RaiseInMe, MemoryOverflow, Message;

CONST delta =10000;
      deltaR=5000;
      Sorry ='Переполнена динамическая память, очень жаль...\n';
      SorryR='Переполнена динамическая память (включая Н.З.), очень жаль...\n';

VAR  setH,main,req: PROCESS;
     H  : INTEGER;
     ws : ARRAY [0..49] OF INTEGER;

PROCEDURE Setter;
  VAR PD: ProcessDesc;
BEGIN
  LOOP
    PD:=ProcessDesc(main); PD^.H_reg:=H;
    TRANSFER(setH,req)
  END;
END Setter;

PROCEDURE SetH(i: INTEGER);
BEGIN
  H:=i; TRANSFER(req,setH);
END SetH;

PROCEDURE GetH(): INTEGER;
BEGIN
  RETURN ProcessDesc(main)^.H_reg
END GetH;

PROCEDURE GetMyS(): INTEGER;
CODE 0 alloc END GetMyS;

PROCEDURE GetS(): INTEGER;
BEGIN
  IF MySelf()=main THEN
    RETURN GetMyS()
  ELSE
    RETURN ProcessDesc(main)^.S_reg
  END;
END GetS;

TYPE List=POINTER TO body;
     body=RECORD
            next: List;
            size: INTEGER;
          END;

VAR free: List;
    H_reg0: ADDRESS;

PROCEDURE next(l: List): List;
CODE abs lsw0 END next;

PROCEDURE adr(l: List): ADDRESS;
CODE abs END adr;

PROCEDURE size(l: List): CARDINAL;
CODE copt 0 lss jfsc 4 drop 1 jfs 1 lsw1 END size;

PROCEDURE ShowMem;
  VAR l: List; n: INTEGER;
BEGIN
  l:=free; n:=GetH()-GetS()+1;
  print('Free memory in heap:\n');
  print('  main area: adr=%8$h size=%8$h (%d).\n',GetS(),n,n);
  WHILE l#NIL DO
    print('  adr=%8$h size=%8$h next=%8$h.\n',adr(l),size(l),next(l));
    INC(n,size(l)); l:=next(l);
  END;
  print('Total %d words free.\n',n);
END ShowMem;

PROCEDURE ALLOCATE(VAR a: ADDRESS; szW: INTEGER);
  VAR h: INTEGER;
BEGIN
  h:=GetH(); a:=h-szW+1;
  IF a-GetS()<delta THEN a:=NIL; RETURN END;
  SetH(a-1);
END ALLOCATE;

PROCEDURE rALLOCATE(VAR a: ADDRESS; szW: INTEGER);
  VAR h: INTEGER;
BEGIN
  h:=GetH(); a:=h-szW+1;
  IF a-GetS()<deltaR THEN a:=NIL; RETURN END;
  SetH(h-szW);
END rALLOCATE;

PROCEDURE Allocate(VAR mm: ADDRESS; sz: INTEGER);
  VAR l: List; szK: INTEGER; a: ADDRESS; p: POINTER TO List;
BEGIN
  ASSERT(sz>=0);
  IF sz=0 THEN mm:=NIL; RETURN END;
  p:=ADR(free); l:=free;
  LOOP
    IF l=NIL THEN
      szK:=CARDINAL(BITSET(sz+4095)-{0..11});
      ALLOCATE(l,szK);
      IF l=NIL THEN
        Message:=Sorry;
        mm:=NIL; RaiseInMe(MemoryOverflow)
      END;
      l^.size:=szK;
      EXIT
    END;
    IF size(l)>=sz THEN
      p^:=next(l);
      EXIT
    END;
    p:=adr(l); l:=next(l);
  END;
  mm:=adr(l)+size(l)-sz;
  IF size(l)>sz THEN a:=adr(l); Deallocate(a,size(l)-sz) END;
  RETURN
END Allocate;

PROCEDURE HardAllocate(VAR mm: ADDRESS; sz: INTEGER);
  VAR l: List; szK: INTEGER; a: ADDRESS; p: POINTER TO List;
BEGIN
  ASSERT(sz>0);
  p:=ADR(free); l:=free;
  LOOP
    IF l=NIL THEN
      szK:=CARDINAL(BITSET(sz+4095)-{0..11});
      rALLOCATE(l,szK);
      IF l=NIL THEN
        Message:=SorryR;
        mm:=NIL; RaiseInMe(MemoryOverflow)
      END;
      l^.size:=szK;
      EXIT
    END;
    IF size(l)>=sz THEN
      p^:=next(l);
      EXIT
    END;
    p:=adr(l); l:=next(l);
  END;
  IF size(l)>sz THEN a:=adr(l)+sz; Deallocate(a,size(l)-sz) END;
  mm:=adr(l);
  RETURN
END HardAllocate;

PROCEDURE Deallocate(VAR a: ADDRESS; sz: INTEGER);
  VAR s: ADDRESS; szK: INTEGER; t,l: List; p: POINTER TO List;
BEGIN
  IF sz=0 THEN a:=NIL; RETURN END;
  l:=free; p:=ADR(free);
  LOOP
    IF a+sz=adr(l) THEN
      INC(sz,size(l)); p^:=next(l); l:=p^;
    ELSIF a<adr(l) THEN
      EXIT
    ELSIF adr(l)+size(l)=a THEN
      INC(sz,size(l)); a:=adr(l); p^:=next(l); l:=p^;
    ELSE
      p:=adr(l); l:=next(l);
    END;
  END;
  IF sz>1 THEN
    p^:=a; p^^.next:=l; p^^.size:=sz
  ELSE
    p^:=-a; a^:=l;
  END;
  a:=NIL;
  IF size(free)>=4096 THEN
    H:=GetH();
    IF H+1=adr(free) THEN SetH(H+size(free)); free:=next(free) END;
  END;
END Deallocate;

VAR n: INTEGER;

BEGIN
  NewPrs(Setter,ADR(ws),SIZE(ws),setH); main:=MySelf();
  free:=NIL;
  n:=GetH()-GetS()+1-delta-100000;
  IF n>1 THEN ALLOCATE(free,n) END;
  IF free#NIL THEN free^.size:=n; free^.next:=NIL END;
END cdsHeap.
