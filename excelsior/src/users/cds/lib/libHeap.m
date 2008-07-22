IMPLEMENTATION MODULE libHeap; (* Sem 04-Jun-87. (c) KRONOS *)

IMPORT  os  : osKernel;
IMPORT  err : libCrash;
IMPORT  cod : defCodes;

FROM SYSTEM    IMPORT   ADR, WORD, ADDRESS;

CONST
  alloc_size=16*256;

TYPE
  List=POINTER TO body;
  body=RECORD
    sz : INTEGER;
    tag: POINTER TO List;
    upm: List;  -- кольцо по ВСЕМ сегментам памяти
    dwm: List;  -- кольцо по ВСЕМ сегментам памяти
    upt: List;  -- кольцо по рангу или задаче
    dwt: List;  -- кольцо по рангу или задаче
  END;
  AREA=List;

VAR
  free    : ARRAY [0..21] OF List;
  mem_ring: List; -- содержит ВСЕ сегменты памяти
  door    : os.mutex_rec;
  area    : AREA;

PROCEDURE adr(l: List): ADDRESS; CODE SIZE(body) cod.add END adr;
PROCEDURE move(a,b: ADDRESS; sz: INTEGER); CODE cod.move END move;

PROCEDURE fail;
BEGIN
  err.raise('Memory overflow.');
END fail;

PROCEDURE rang(i: INTEGER): INTEGER;
  VAR k,r: INTEGER;
BEGIN
  IF i<=16 THEN RETURN i-1 END;
  k:=HIGH(free); r:=INTEGER({k-12});
  WHILE i<=r DO DEC(k); r:=r>>1 END;
  RETURN k;
END rang;

PROCEDURE show_mem(tag: AREA; p: proc);
  VAR l,k: List; i: INTEGER;
BEGIN
  os.acquire(door);
  IF tag#NIL THEN
    l:=tag; k:=l;
    IF l#NIL THEN
      REPEAT
        p(adr(l),l^.sz);
        l:=l^.upt;
      UNTIL l=k;
    END;
  ELSE
    FOR i:=HIGH(free) TO 0 BY -1 DO
      l:=free[i]; k:=l;
      IF l#NIL THEN
        REPEAT
          p(adr(l),l^.sz);
          l:=l^.upt;
        UNTIL l=k;
      END;
    END;
  END;
  os.release(door);
END show_mem;

PROCEDURE tie_free(l: List; n: INTEGER);
  TYPE i=INTEGER;
  VAR f: List;
BEGIN
  f:=free[n];
  IF f=NIL THEN
    l^.upt:=l; l^.dwt:=l; free[n]:=l;
  ELSE
    LOOP
      IF i(f)>i(l) THEN
        IF i(f^.dwt)<i(l) THEN EXIT END;
        IF i(f^.dwt)>=i(f) THEN free[n]:=l; EXIT END;
        f:=f^.dwt;
      ELSE
        f:=f^.upt;
        IF i(f)<=i(f^.dwt) THEN EXIT END;
      END;
    END;
    l^.upt:=f; l^.dwt:=f^.dwt;
    l^.upt^.dwt:=l; l^.dwt^.upt:=l;
  END;
END tie_free;

PROCEDURE untie_free(l: List; n: INTEGER);
BEGIN
  IF free[n]=l THEN free[n]:=l^.upt END;
  IF free[n]=l THEN free[n]:=NIL
  ELSE l^.upt^.dwt:=l^.dwt; l^.dwt^.upt:=l^.upt;
  END;
  l^.upt:=l; l^.dwt:=l;
END untie_free;

PROCEDURE untie(l: List);
  -- вывязывает сегмент из mem_ring и free
  -- то есть сегмент становится свободным как муха в полете.
  -- используется для слияния свободных сегментов.
BEGIN
  ASSERT(mem_ring#l); -- глубокая мысль!!!
  l^.upm^.dwm:=l^.dwm; l^.dwm^.upm:=l^.upm;
  untie_free(l,rang(l^.sz));
END untie;

PROCEDURE Dealloc_seg(VAR host: List; l: List);
  VAR dw,up: List; i,j: INTEGER;
BEGIN
  ASSERT(l^.tag#NIL);
  -- вывязываем из host
  IF host=l THEN host:=host^.upt END;
  IF host=l THEN host:=NIL
  ELSE l^.upt^.dwt:=l^.dwt; l^.dwt^.upt:=l^.upt;
  END;
  l^.upt:=l; l^.dwt:=l;
  -- заносим в free
  tie_free(l,rang(l^.sz));
  l^.tag:=NIL;
  -- укрупняем
  dw:=l^.dwm; up:=l^.upm;
  LOOP
    IF (adr(dw)+dw^.sz=l) & (dw^.tag=NIL) THEN
      untie(l);
      i:=rang(dw^.sz); j:=rang(dw^.sz+l^.sz+SIZE(l^));
      IF i#j THEN untie_free(dw,i); tie_free(dw,j) END;
      INC(dw^.sz,l^.sz+SIZE(l^)); l:=dw; dw:=l^.dwm;
    ELSIF (adr(l)+l^.sz=up) & (up^.tag=NIL) THEN
      untie(up);
      i:=rang(l^.sz); j:=rang(l^.sz+up^.sz+SIZE(up^));
      IF i#j THEN untie_free(l,i); tie_free(l,j) END;
      INC(l^.sz,up^.sz+SIZE(up^)); up:=l^.upm;
    ELSE
      EXIT
    END;
  END;
END Dealloc_seg;

PROCEDURE create_mem(a: ADDRESS; sz: INTEGER);
  VAR l,k: List;
BEGIN
  ASSERT(a#NIL); ASSERT(sz>SIZE(l^));
  os.acquire(door);
  l:=a; DEC(sz,SIZE(l^)); l^.sz:=sz;
  IF mem_ring=NIL THEN
    mem_ring:=l; l^.upm:=l; l^.dwm:=l;
  ELSIF a<adr(mem_ring) THEN
    l^.upm:=mem_ring; l^.dwm:=mem_ring^.dwm;
    l^.upm^.dwm:=l; l^.dwm^.upm:=l;
    mem_ring:=l;
  ELSIF a>adr(mem_ring^.dwm) THEN
    l^.upm:=mem_ring; l^.dwm:=mem_ring^.dwm;
    l^.upm^.dwm:=l; l^.dwm^.upm:=l;
  ELSE
    k:=mem_ring;
    LOOP
      ASSERT(k^.upm#mem_ring);
      IF (adr(k)<a) & (a<adr(k^.upm)) THEN
        l^.dwm:=k; l^.upm:=k^.upm;
        l^.upm^.dwm:=l; l^.dwm^.upm:=l; EXIT
      END;
      k:=k^.upm;
    END;
  END;
  l^.upt:=l; l^.dwt:=l; l^.tag:=ADR(l);
  Dealloc_seg(l,l);
  os.release(door);
END create_mem;

PROCEDURE Alloc_tg(VAR host: AREA; VAR mm: ADDRESS; sz: INTEGER);
  VAR l,k: List; n,m: INTEGER; task: os.task_ptr;
BEGIN
  ASSERT(sz>0);
  n:=rang(sz); l:=free[n];
  LOOP
    WHILE l=NIL DO
      IF n=HIGH(free) THEN
        m:=(sz+SIZE(l^)+alloc_size-1) DIV alloc_size * alloc_size;
        task:=os.self();
        os.ALLOCATE(task^.area,l,m);
        IF l=NIL THEN
          mm:=NIL; RETURN
        ELSE
          create_mem(l,m); n:=rang(sz); l:=free[n];
        END;
      ELSE
        INC(n); l:=free[n]
      END;
    END;
    IF l^.sz>sz+SIZE(l^) THEN
      k:=adr(l)+sz; k^.tag:=NIL;
      k^.sz:=l^.sz-sz-SIZE(l^);
      l^.sz:=sz;
      k^.upm:=l^.upm; k^.dwm:=l;
      k^.upm^.dwm:=k; k^.dwm^.upm:=k;
      tie_free(k,rang(k^.sz));
    END;
    IF l^.sz=sz THEN
      untie_free(l,n);
      IF host=NIL THEN
        host:=l; l^.upt:=l; l^.dwt:=l;
      ELSE
        l^.upt:=host; l^.dwt:=host^.dwt;
        l^.upt^.dwt:=l; l^.dwt^.upt:=l;
      END;
      l^.tag:=ADR(host); mm:=adr(l); EXIT
    END;
    l:=l^.upt;
    IF l=free[n] THEN l:=NIL END;
  END;
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
  Dealloc_seg(tag,t);
  os.release(door);
END dealloc_tag;

PROCEDURE dealloc_adr(VAR a: ADDRESS);
  VAR t: List;
BEGIN
  IF a=NIL THEN RETURN END;
  os.acquire(door);
  t:=a-SIZE(body); a:=NIL;
  ASSERT(t^.tag#NIL);
  Dealloc_seg(t^.tag^,t);
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
  ASSERT(t^.upt^.dwt=t);
  ASSERT(t^.upt^.tag#NIL);
  ASSERT(t^.upt^.tag=ADR(area));
  Dealloc_seg(area,t);
  os.release(door);
END DEALLOCATE;

PROCEDURE resize(a: ADDRESS; sz: INTEGER; VAR new_sz: INTEGER);
  VAR l,k: List;
BEGIN
  ASSERT(a#NIL);
  l:=a-SIZE(l^);
  IF sz=0 THEN new_sz:=l^.sz; RETURN END;
  os.acquire(door);
  LOOP
    IF l^.sz-sz-SIZE(l^)>0 THEN
      -- разбиваем семент на два
      k:=adr(l)+sz;
      k^.sz:=l^.sz-sz-SIZE(k^);
      l^.sz:=sz;
      k^.upm:=l^.upm; k^.dwm:=l;
      k^.upm^.dwm:=k; k^.dwm^.upm:=k;
      k^.tag:=ADR(area); k^.upt:=k; k^.dwt:=k;
      Dealloc_seg(k,k);
    ELSIF (sz>l^.sz) & (adr(l)+l^.sz=l^.upm) & (l^.upm^.tag=NIL) THEN
      -- добавляем следующий свободный
      k:=l^.upm;
      untie(k);
      INC(l^.sz,k^.sz+SIZE(k^));
    ELSE
      EXIT
    END;
  END;
  os.release(door);    
  new_sz:=l^.sz;
END resize;

PROCEDURE realloc(VAR a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR l,k: List; k_sz: INTEGER; b: ADDRESS;
BEGIN
  ASSERT(a#NIL);
  os.acquire(door);
  l:=a-SIZE(l^);
  IF sz=0 THEN
    Dealloc_seg(l^.tag^,l); a:=NIL;
  ELSIF sz=l^.sz THEN
    -- ничего
  ELSIF l^.sz-sz-SIZE(l^)>0 THEN
    -- разбиваем семент на два
    k:=adr(l)+sz;
    k^.sz:=l^.sz-sz-SIZE(k^);
    l^.sz:=sz;
    k^.upm:=l^.upm; k^.dwm:=l;
    k^.upm^.dwm:=k; k^.dwm^.upm:=k;
    k^.upt:=k; k^.dwt:=k; k^.tag:=ADR(k);
    Dealloc_seg(k,k);
  ELSIF (adr(l)+l^.sz=l^.upm) & (l^.upm^.tag=NIL)
      & (l^.sz+l^.upm^.sz+SIZE(l^)=sz) THEN
    -- добавляем следующий свободный
    k:=l^.upm; untie(k); l^.sz:=sz;
  ELSIF (adr(l)+l^.sz=l^.upm) & (l^.upm^.tag=NIL)
      & (l^.sz+l^.upm^.sz-sz>0) THEN
    -- добавляем следующий свободный, разбиваем семент на два
    k_sz:=l^.upm^.sz;
    untie(l^.upm);
    k:=adr(l)+sz;
    k^.sz:=k_sz+l^.sz-sz;
    l^.sz:=sz;
    k^.upm:=l^.upm; k^.dwm:=l;
    k^.upm^.dwm:=k; k^.dwm^.upm:=k;
    k^.upt:=k; k^.dwt:=k; k^.tag:=ADR(k);
    Dealloc_seg(k,k);
  ELSE
    Alloc_tg(l^.tag^,b,sz);
    IF b=NIL THEN os.release(door); RETURN FALSE END;
    IF sz>l^.sz THEN move(b,a,l^.sz) ELSE move(b,a,sz) END;
    a:=b;
    Dealloc_seg(l^.tag^,l);
  END;
  os.release(door);
  RETURN TRUE;
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
  WHILE tag#NIL DO
    Dealloc_seg(tag,tag);
  END;
  os.release(door);
END dealloc_all;

PROCEDURE tag?(a: ADDRESS): pAREA;
  VAR l: List;
BEGIN
  ASSERT(a#NIL);
  l:=a-SIZE(l^);
  RETURN ADDRESS(l^.tag);
END tag?;

PROCEDURE change_tag(a: ADDRESS; VAR to: AREA);
  VAR l: List; fr: POINTER TO List;
BEGIN
  IF a=NIL THEN RETURN END;
  l:=a-SIZE(l^); fr:=ADDRESS(l^.tag);
  os.acquire(door);
  IF fr^=l THEN fr^:=l^.upt END;
  IF fr^=l THEN fr^:=NIL
  ELSE l^.upt^.dwt:=l^.dwt; l^.dwt^.upt:=l^.upt;
  END;
  IF to=NIL THEN
    l^.upt:=l; l^.dwt:=l;
  ELSE
    ASSERT(to^.tag=ADR(to));
    ASSERT(to^.upt^.dwt=to);
    l^.upt:=to; l^.dwt:=to^.dwt;
    l^.upt^.dwt:=l; l^.dwt^.upt:=l;
  END;
  to:=l;
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

VAR n: INTEGER;

BEGIN
  os.ini_mutex(door);
  FOR n:=0 TO HIGH(free) DO free[n]:=NIL END;
  mem_ring:=NIL;
  area:=NIL;
END libHeap.
