IMPLEMENTATION MODULE libHeap; (* Sem 04-Jun-87. (c) KRONOS *)

IMPORT  os  : osKernel;
IMPORT  err : libCrash;
IMPORT  cod : defCodes;
IMPORT  mem : Heap;

FROM SYSTEM    IMPORT   ADR, WORD, ADDRESS;

CONST
  magic = 4783990Bh;

TYPE
  List=POINTER TO body;
  body=RECORD
    mag: INTEGER;
    sz : INTEGER;
    tag: POINTER TO List;
    fwd: List;
    bck: List;
  END;
  AREA=List;

VAR
  door    : os.mutex_rec;
  area    : AREA;

PROCEDURE adr(l: List): ADDRESS; CODE SIZE(body) cod.add END adr;
PROCEDURE move(a,b: ADDRESS; sz: INTEGER); CODE cod.move END move;

PROCEDURE fail;
BEGIN
  err.raise('Memory overflow.');
END fail;

PROCEDURE show_mem(tag: AREA; p: proc);
  VAR l: List;
BEGIN
  os.acquire(door);
  IF tag#NIL THEN
    l:=tag;
    REPEAT p(adr(l),l^.sz); l:=l^.fwd UNTIL l=tag;
  END;
  os.release(door);
END show_mem;

PROCEDURE Dealloc_seg(l: List);
BEGIN
  ASSERT(l^.mag=magic);
  ASSERT(l^.tag^^.mag=magic);
  IF l^.tag^=l THEN l^.tag^:=l^.fwd END;
  IF l^.tag^=l THEN l^.tag^:=NIL
  ELSE l^.fwd^.bck:=l^.bck; l^.bck^.fwd:=l^.fwd;
  END;
  l^.mag:=0;
  mem.deallocate(l,l^.sz+SIZE(l^));
END Dealloc_seg;

PROCEDURE Alloc_tg(VAR host: AREA; VAR mm: ADDRESS; sz: INTEGER);
  VAR l: List;
BEGIN
  ASSERT(sz>0);
  mem.allocate(l,sz+SIZE(l^));
  IF l=NIL THEN mm:=NIL; RETURN END;
  l^.mag:=magic; l^.sz:=sz; l^.tag:=ADR(host);
  IF host=NIL THEN
    host:=l; l^.fwd:=l; l^.bck:=l;
  ELSE
    ASSERT(host^.mag=magic);
    l^.fwd:=host; l^.bck:=host^.bck;
    l^.fwd^.bck:=l; l^.bck^.fwd:=l;
  END;
  mm:=adr(l);
END Alloc_tg;

PROCEDURE alloc_tag(VAR a: ADDRESS; sz: INTEGER; VAR tg: AREA);
BEGIN
  IF sz=0 THEN a:=NIL; RETURN END;
  os.acquire(door);
  Alloc_tg(tg,a,sz);
  os.release(door);
END alloc_tag;

PROCEDURE ALLOCATE(VAR a: ADDRESS; sz: INTEGER);
BEGIN
  IF sz=0 THEN a:=NIL; RETURN END;
  os.acquire(door);
  Alloc_tg(area,a,sz);
  os.release(door);
  IF a=NIL THEN fail END;
END ALLOCATE;

PROCEDURE allocate(VAR a: ADDRESS; sz: INTEGER);
BEGIN
  IF sz=0 THEN a:=NIL; RETURN END;
  os.acquire(door);
  Alloc_tg(area,a,sz);
  os.release(door);
END allocate;

PROCEDURE dealloc_tag(VAR a: ADDRESS; sz: INTEGER; VAR tag: AREA);
  VAR t: List;
BEGIN
  IF a=NIL THEN RETURN END;
  os.acquire(door);
  t:=a-SIZE(body); a:=NIL;
  ASSERT(sz=t^.sz);
  ASSERT(t^.tag=ADR(tag));
  Dealloc_seg(t);
  os.release(door);
END dealloc_tag;

PROCEDURE dealloc_adr(VAR a: ADDRESS);
  VAR t: List;
BEGIN
  IF a=NIL THEN RETURN END;
  os.acquire(door);
  t:=a-SIZE(body); a:=NIL;
  Dealloc_seg(t);
  os.release(door);
END dealloc_adr;

PROCEDURE DEALLOCATE(VAR a: ADDRESS; sz: INTEGER);
  VAR t: List;
BEGIN
  IF a=NIL THEN RETURN END;
  os.acquire(door);
  t:=a-SIZE(body); a:=NIL;
  ASSERT(t^.sz=sz);
  ASSERT(t^.tag=ADR(area));
  ASSERT(t^.fwd^.bck=t);
  ASSERT(t^.fwd^.tag=ADR(area));
  Dealloc_seg(t);
  os.release(door);
END DEALLOCATE;

PROCEDURE resize(a: ADDRESS; sz: INTEGER; VAR new_sz: INTEGER);
BEGIN
  new_sz:=sz;
END resize;

PROCEDURE realloc(VAR a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR r: BOOLEAN; l: List;
BEGIN
  ASSERT(a#NIL);
  os.acquire(door);
  l:=a-SIZE(l^); r:=TRUE;
  IF sz<=0 THEN
    Dealloc_seg(l); a:=NIL;
  ELSIF l^.sz#sz THEN
    Alloc_tg(l^.tag^,a,sz);
    IF a#NIL THEN
      IF sz<l^.sz THEN move(a,adr(l),sz) ELSE move(a,adr(l),l^.sz) END;
      Dealloc_seg(l);
    ELSE
      r:=FALSE; a:=adr(l);
    END;
  END;
  os.release(door);
  RETURN r;
END realloc;

PROCEDURE REALLOCATE(VAR a: ADDRESS; VAR high: INTEGER; len,bytes: INTEGER);
BEGIN
  IF high<0 THEN
    ALLOCATE(a,(len*bytes+3) DIV 4); high:=len-1;
  ELSE
    IF realloc(a,(len*bytes+3) DIV 4) THEN high:=len-1 ELSE fail END;
  END;
END REALLOCATE;

PROCEDURE dealloc_all(VAR tag: AREA);
BEGIN
  os.acquire(door);
  WHILE tag#NIL DO Dealloc_seg(tag) END;
  os.release(door);
END dealloc_all;

PROCEDURE tag?(a: ADDRESS): pAREA;
  VAR l: List;
BEGIN
  ASSERT(a#NIL);
  l:=a-SIZE(l^);
  RETURN pAREA(l^.tag);
END tag?;

PROCEDURE change_tag(a: ADDRESS; VAR to: AREA);
  VAR l: List; fr: POINTER TO List;
BEGIN
  IF a=NIL THEN RETURN END;
  l:=a-SIZE(l^); fr:=ADDRESS(l^.tag);
  ASSERT(l^.mag=magic);
  ASSERT(fr^^.mag=magic);
  os.acquire(door);
  IF fr^=l THEN fr^:=l^.fwd END;
  IF fr^=l THEN fr^:=NIL
  ELSE l^.fwd^.bck:=l^.bck; l^.bck^.fwd:=l^.fwd;
  END;
  IF to=NIL THEN
    l^.fwd:=l; l^.bck:=l; to:=l;
  ELSE
    ASSERT(to^.mag=magic);
    l^.fwd:=to; l^.bck:=to^.bck;
    l^.fwd^.bck:=l; l^.bck^.fwd:=l;
  END;
  l^.tag:=ADR(to);
  os.release(door);
END change_tag;

PROCEDURE change_all(VAR from,to: AREA);
  VAR a: ADDRESS;
BEGIN
  os.acquire(door);
  WHILE from#NIL DO
    a:=adr(from); change_tag(a,to);
  END;
  os.release(door);
END change_all;

BEGIN
  os.ini_mutex(door);
  area:=NIL;
END libHeap.
