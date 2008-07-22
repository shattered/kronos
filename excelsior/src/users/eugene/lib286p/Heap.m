IMPLEMENTATION MODULE Heap;

FROM SYSTEM      IMPORT ADDRESS, WORD, ADR;

IMPORT  err: defErrors;
IMPORT  env: def_GDT;
IMPORT  low: lowLevel;

CONST
  magic = 854339h;

TYPE
  list = POINTER TO list_body;
  list_body = RECORD
    mag : INTEGER;
    next: list;
    size: INTEGER;
  END;

VAR
  free  : list;
  os_cnt: INTEGER;
  my_cnt: INTEGER;

(*
PROCEDURE show_free;
  VAR l: list;
BEGIN
  l:=free;
  WHILE l#NIL DO
    tty.print('adr %$8h, size %$8h\n',l,l^.size+SIZE(l^));
    l:=l^.next;
  END;
END show_free;
*)

PROCEDURE ALLOCATE(VAR a: ADDRESS; size: INTEGER);
BEGIN
  allocate(a,size);
  IF (size>0) & (a=NIL) THEN HALT(err.no_memory) END;
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: ADDRESS; size: INTEGER);
BEGIN
  deallocate(a,size);
END DEALLOCATE;

PROCEDURE REALLOCATE(VAR a: ADDRESS; VAR high: INTEGER; len,sz: INTEGER);
BEGIN
  reallocate(a,high,len,sz);
  IF high<len-1 THEN HALT(err.no_memory) END;
END REALLOCATE;

PROCEDURE allocate(VAR a: ADDRESS; size: INTEGER);
  VAR p: POINTER TO list; l: list;
BEGIN
  IF size<=0 THEN a:=NIL; RETURN END;
  p:=ADR(free);
  LOOP
    IF p^=NIL THEN a:=NIL; RETURN END;
    ASSERT(p^^.mag=magic);
    IF p^^.size=size THEN
      l:=p^; p^:=p^^.next; l^.next:=NIL; DEC(my_cnt,size+SIZE(l^));
      a:=ADDRESS(l)+SIZE(l^); RETURN
    END;
    IF p^^.size>size+SIZE(l^) THEN
      l:=p^;
      a:=ADDRESS(l)+SIZE(l^);
      p^:=a+size;
      p^^.mag:=magic;
      p^^.next:=l^.next; l^.next:=NIL;
      p^^.size:=l^.size-size-SIZE(l^); l^.size:=size;
      DEC(my_cnt,size+SIZE(l^));
      RETURN
    END;
    p:=ADR(p^^.next);
  END;
END allocate;

PROCEDURE deallocate(VAR a: ADDRESS; size: INTEGER);
  VAR l: list; p: POINTER TO list; fr,to: ADDRESS;
BEGIN
  IF size<=0 THEN a:=NIL; RETURN END;
  l:=a-SIZE(l^); a:=NIL;
  ASSERT(l^.mag=magic);
  ASSERT(l^.next=NIL);
  ASSERT(l^.size=size);
  INC(my_cnt,size+SIZE(l^));
  l^.mag:=0;
  fr:=ADDRESS(l); to:=fr+(SIZE(l^)+size);
  p:=ADR(free);
  LOOP
    IF p^=NIL THEN EXIT
    ELSIF ADDRESS(p^)=to THEN
      p^^.mag:=0; to:=to+(SIZE(l^)+p^^.size);
      size:=size+(SIZE(l^)+p^^.size); p^:=p^^.next;
    ELSIF ADDRESS(p^)>to THEN EXIT
    ELSIF fr=ADDRESS(p^)+(SIZE(l^)+p^^.size) THEN
      p^^.mag:=0; fr:=p^;
      size:=size+(SIZE(l^)+p^^.size); p^:=p^^.next;
    ELSE
      p:=ADR(p^^.next);
    END;
  END;
  l:=fr; l^.next:=p^; p^:=l;
  l^.mag:=magic; l^.size:=size;
END deallocate;

PROCEDURE reallocate(VAR a: ADDRESS; VAR high: INTEGER; len,sz: INTEGER);
  VAR b: ADDRESS; new_sz,old_sz: INTEGER;
BEGIN
  new_sz:=len*sz;
  old_sz:=(high+1)*sz;
  allocate(b,new_sz);
  IF new_sz>0 THEN
    IF b=NIL THEN RETURN END;
    IF old_sz>0 THEN
      IF new_sz>old_sz THEN low.move(b,a,old_sz)
      ELSE low.move(b,a,new_sz);
      END;
    END;
  END;
  deallocate(a,old_sz);
  a:=b; high:=len-1;
END reallocate;

PROCEDURE set_credit(size: INTEGER);
BEGIN
  credit:=size;
END set_credit;

PROCEDURE set_limit(total: INTEGER);
BEGIN
  limit:=total;
END set_limit;

PROCEDURE install_gc(gc: GARBAGE_COLLECTOR);
BEGIN
END install_gc;

PROCEDURE  remove_gc(gc: GARBAGE_COLLECTOR);
BEGIN
END remove_gc;

PROCEDURE statistics(VAR  from_os: INTEGER;
                     VAR     free: INTEGER;
                     VAR user_sum: INTEGER);

BEGIN
  from_os:=os_cnt;
  free:=my_cnt;
  user_sum:=from_os-free;
END statistics;

BEGIN
  credit:=100;
  limit :=100;
  os_cnt:=60000h;
  my_cnt:=os_cnt;
  free:=env.mem_a+100000h;
  free^.mag:=magic;
  free^.next:=NIL;
  free^.size:=os_cnt-SIZE(free^);
END Heap.
