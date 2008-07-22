IMPLEMENTATION MODULE Heap;             (*   Andy 13-Dec-89. (c) KRONOS *)
                                        (*   Andy 20-Dec-89. (c) KRONOS *)
                                        (*   Andy 23-Nov-90. (c) KRONOS *)
                                        -- block size check corrected --
                                        (*   Leo 06-Feb-91. (c) KRONOS  *)

                IMPORT  sys: SYSTEM;
                IMPORT  cmd: defCodes;
                IMPORT  err: defErrors;
                IMPORT  env: tskEnv;

TYPE
  adr   = sys.ADDRESS;
  int   = INTEGER;
  bit   = BITSET;

  list  = POINTER TO node;
  node  = RECORD
            size: INTEGER;
            next: list;
            prev: list;
          END;

PROCEDURE min(a,b:int):int; BEGIN IF a<b THEN RETURN a ELSE RETURN b END END min;
PROCEDURE max(a,b:int):int; BEGIN IF a<b THEN RETURN b ELSE RETURN a END END max;

PROCEDURE bad_parm;  BEGIN done:=FALSE; error:=err.bad_parm  END bad_parm;
PROCEDURE no_memory; BEGIN done:=FALSE; error:=err.no_memory END no_memory;

CONST minsize= SIZE(node)+1;

CONST hbit= BITS(BITSET)-1;
      busy_mask= {hbit};

VAR    curr: list;
         FB: list;

VAR from_os: int;
       free: int;
   user_sum: int;

-----------------------  SLOTS  MANAGER  ----------------------
                       ------------------
TYPE slot  = POINTER TO scrap;
     scrap = RECORD
               size: INTEGER;
               next: slot;
               prev: slot;
             END;

VAR pool: slot;

CONST slot_extra=4;

PROCEDURE add_slot(VAR s: slot; size: int);
  VAR req: int;
  CONST addsz=SIZE(scrap);
BEGIN INC(size,addsz+slot_extra);
  req:=max(credit+addsz+slot_extra,size);
  IF limit>=0 THEN
      IF limit<size THEN s:=NIL; RETURN END;
      req:=min(req,limit);
  END;
  env.allocate(s,req);
  IF s=NIL THEN
      IF req<=size THEN RETURN END;
      req:=size;
      env.allocate(s,req);
      IF s=NIL THEN RETURN END;
  END;
  s^.size:=req-addsz;
  s^.next:=pool; s^.prev:=NIL;
  IF pool#NIL THEN pool^.prev:=s END; pool:=s;
  IF limit>=0 THEN DEC(limit,req) END;
  INC(from_os,req);
  INC(free,s^.size-slot_extra);
END add_slot;

PROCEDURE free_slot(s: slot);
  VAR n,p: slot; sz: int;
BEGIN n:=s^.next; p:=s^.prev;
  IF p#NIL THEN p^.next:=n END;
  IF n#NIL THEN n^.prev:=p END;
  IF pool=s THEN pool:=n END;
  sz:=s^.size+SIZE(scrap);
  IF limit>=0 THEN INC(limit,sz) END;
  DEC(from_os,sz);
  DEC(free,s^.size-slot_extra);
  env.deallocate(s,sz);
END free_slot;

PROCEDURE flush_ring(bound: int): BOOLEAN;
  VAR p,l,r: list;
      s,next: slot; res: BOOLEAN;
BEGIN res:=FALSE;
  s:=pool;
  WHILE s#NIL DO
     next:=s^.next;
     p:=list(int(s)+SIZE(scrap)+1);
     IF NOT (hbit IN bit(p^.size)) AND
            (s^.size=p^.size+slot_extra) AND (p^.size>=bound) THEN
         l:=p^.prev; r:=p^.next;
         l^.next:=r; r^.prev:=l;
         IF curr=p THEN curr:=r END;
         free_slot(s); res:=TRUE;
     END;
     s:=next;
  END;
  RETURN res
END flush_ring;

PROCEDURE garbage_collector(): BOOLEAN; FORWARD;

PROCEDURE user_collectors(): BOOLEAN; FORWARD;

----------------------  H E A P  MANAGER  ---------------------
                      --------------------
PROCEDURE set_end(l: list);
(* end:=l+clean(l^.size)+1; end^:=l^.size; *)
CODE cmd.copt cmd.copt
     cmd.lsw0 cmd.li1 cmd.copt cmd.ror cmd.bic cmd.add
     cmd.swap cmd.lsw0 cmd.ssw1
END set_end;

PROCEDURE marked(sz:int):int;
(* RETURN sz+busy_mask *)
CODE cmd.li1 cmd.copt cmd.ror cmd.or
END marked;

PROCEDURE move(trg:adr; src:adr; sz:int); CODE cmd.move END move;

PROCEDURE chk(blk_sz: INTEGER; req: INTEGER);
CODE cmd.sub cmd.li0+minsize-1 cmd.chkz cmd.drop
END chk;

VAR _to_os: BOOLEAN;

PROCEDURE try_ring(VAR p: list; w: int): BOOLEAN;
BEGIN
  p:=curr;
  LOOP IF p^.size>=w THEN RETURN TRUE END;
       p:=p^.next;
       IF p=curr THEN RETURN FALSE END;
  END;
END try_ring;

PROCEDURE alloc_large(VAR a: adr; w: int);
  VAR p,l,r: list;
      s: POINTER TO int;
      new: slot;
BEGIN
  p:=curr;
  LOOP IF p^.size>=w THEN EXIT END;
       p:=p^.next;
       IF p=curr THEN
           add_slot(new,w);
           IF new=NIL THEN
               IF NOT user_collectors() THEN a:=NIL; RETURN END;
               IF try_ring(p,w) THEN EXIT END;
               IF flush_ring(0) THEN END;
               add_slot(new,w);
               IF new=NIL THEN a:=NIL; RETURN END;
           END;
           p:=list(int(new)+SIZE(scrap)+1);
           p^.size:=new^.size-4; set_end(p);
           s:=adr(p)-1;         s^:=marked(0);
           s:=adr(s)+p^.size+3; s^:=marked(0);
           r:=FB^.next;
           p^.prev:=FB; p^.next:=r; FB^.next:=p; r^.prev:=p;
           curr:=p;
           EXIT
       END;
  END;
  IF (p^.size-w) < minsize THEN -- whole block
       l:=p^.prev; r:=p^.next; curr:=r;
       l^.next:=r; r^.prev:=l;
       DEC(free,p^.size);
       p^.size:=marked(p^.size); set_end(p);
       a:=adr(p)+1; RETURN
  ELSE -- split block
       s:=adr(p)+p^.size-w; a:=adr(s)+1;
       s^:=marked(w); set_end(list(s));
       DEC(p^.size,w+2); set_end(p);
       DEC(free,w+2);
       curr:=p; RETURN
  END;
END alloc_large;

PROCEDURE dealloc_large(a: adr; w: int);
  VAR b: list; l,r,n: list; rr,rl: list;
BEGIN
  b:=list(int(a)-1);
  IF NOT (hbit IN bit(b^.size)) THEN bad_parm; RETURN END;
  b^.size:=int(bit(b^.size)-busy_mask);
  chk(b^.size,w);
  l:=list(int(b)-1); r:=list(int(b)+b^.size+2);
  IF NOT (hbit IN bit(l^.size)) THEN -- left is free
       l:=list(int(l)-l^.size-1);
       IF NOT (hbit IN bit(r^.size)) THEN -- right is free
            rr:=r^.next; rl:=r^.prev;
            rl^.next:=rr; rr^.prev:=rl;
            INC(free,b^.size+4);
            INC(l^.size,b^.size+r^.size+4); set_end(l); curr:=l;
       ELSE INC(free,b^.size+2);
            INC(l^.size,b^.size+2); set_end(l); curr:=l;
       END;
  ELSE IF NOT (hbit IN bit(r^.size)) THEN -- right is free
            rr:=r^.next; rl:=r^.prev;
            b^.next:=rr; b^.prev:=rl;
            rl^.next:=b; rr^.prev:=b;
            INC(free,b^.size+2);
            INC(b^.size,r^.size+2); set_end(b); curr:=b;
       ELSE set_end(b); curr:=b;
            INC(free,b^.size);
            n:=FB^.next;
            b^.prev:=FB; b^.next:=n;
            FB^.next:=b; n^.prev:=b;
       END;
  END;
  IF (curr^.size>=credit) THEN
      _to_os:= flush_ring(credit) OR _to_os;
  END;
END dealloc_large;

PROCEDURE realloc_large(a: adr; size,new_size: int; VAR res: adr);
  VAR p,e,l,r,nxt: list; rsz,nsz: int;
BEGIN
  done:=TRUE;
  p:=list(int(a)-1);
  IF NOT (hbit IN bit(p^.size)) THEN bad_parm; RETURN END;
  rsz:=int(bit(p^.size)-busy_mask);
  chk(rsz,size);
  IF size<new_size THEN
       IF new_size<=rsz THEN res:=a; RETURN END;
       nxt:=list(int(p)+rsz+2);
       IF NOT (hbit IN bit(nxt^.size)) AND
              ((rsz+2+nxt^.size)>=new_size) THEN
            res:=a; l:=nxt^.prev; r:=nxt^.next; nsz:=nxt^.size;
            IF (rsz+2+nsz-new_size)<minsize THEN
                 DEC(free,nsz+2);
                 l^.next:=r; r^.prev:=l; IF curr=nxt THEN curr:=r END;
                 p^.size:=marked(rsz+2+nsz); set_end(p);
            ELSE e:=list(int(p)+new_size+2);
                 p^.size:=marked(new_size); set_end(p);
                 e^.size:=rsz+nsz-new_size; set_end(e);
                 DEC(free,new_size-rsz);
                 l^.next:=e; e^.prev:=l; r^.prev:=e; e^.next:=r;
                 curr:=e
            END
       ELSE
         alloc_large(res,new_size);
         IF res=NIL THEN no_memory; RETURN END;
         move(res,a,size);
         dealloc_large(a,size)
       END
  ELSE
    IF (rsz-new_size)<minsize THEN res:=a; RETURN END;
    res:=a; e:=list(int(p)+new_size+2);
    p^.size:=marked(new_size);       set_end(p);
    e^.size:=marked(rsz-new_size-2); set_end(e);
    dealloc_large(adr(int(e)+1),rsz-new_size-2);
  END;
END realloc_large;

---------------------- GARBAGE COLLECTION ------------------------------
                       ------------------
CONST NULL = GARBAGE_COLLECTOR(NIL);

VAR gcs: ARRAY [0..15] OF GARBAGE_COLLECTOR;

PROCEDURE install_gc(proc: GARBAGE_COLLECTOR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(gcs)) & (gcs[i]#NULL) DO INC(i) END;
  IF i>HIGH(gcs) THEN
      FOR j:=0 TO HIGH(gcs)-1 DO gcs[j]:=gcs[j+1] END;
      i:=HIGH(gcs);
  END;
  gcs[i]:=proc;
  done:=TRUE;
END install_gc;

PROCEDURE remove_gc(proc: GARBAGE_COLLECTOR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(gcs)) & (gcs[i]#proc) DO INC(i) END;
  IF i>HIGH(gcs) THEN RETURN END;
  FOR j:=i TO HIGH(gcs)-1 DO gcs[j]:=gcs[j+1] END;
  gcs[HIGH(gcs)]:=NULL;
  done:=TRUE;
END remove_gc;

PROCEDURE user_collectors(): BOOLEAN;
  VAR i: INTEGER; res: BOOLEAN;
BEGIN
  res:=FALSE;
  FOR i:=0 TO HIGH(gcs) DO
     IF gcs[i]#NULL THEN res:= gcs[i]() OR res END;
  END;
  RETURN res
END user_collectors;

PROCEDURE garbage_collector(): BOOLEAN;
BEGIN
  done:=TRUE;
  _to_os:=FALSE;
  IF user_collectors() THEN END;
  RETURN flush_ring(0) OR _to_os
END garbage_collector;

----------------------------------------------------------------

PROCEDURE ALLOCATE(VAR a: adr; words: int);
  VAR w: int;
BEGIN
  done:=TRUE;
  IF words=0 THEN a:=NIL; RETURN END;
  IF words<0 THEN HALT(err.bad_parm) END;
  w:=words; IF w=1 THEN w:=2 END;
  alloc_large(a,w);
  done:=(a#NIL);
  IF done THEN INC(user_sum,words)
  ELSE
    HALT(err.no_memory)
  END;
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: adr; words: int);
  VAR w: int;
BEGIN
  done:=TRUE;
  IF words<0 THEN HALT(err.bad_parm) END;
  IF a=NIL THEN RETURN END;
  IF words=0 THEN HALT(err.bad_parm) END;
  w:=words; IF w=1 THEN w:=2 END;
  dealloc_large(a,w);   a:=NIL;
  IF NOT done THEN HALT(error) END;
  DEC(user_sum,words);
END DEALLOCATE;

PROCEDURE allocate(VAR a: adr; words: int);
  VAR w: int;
BEGIN
  done:=TRUE;
  IF words<0 THEN bad_parm; RETURN  END;
  IF words=0 THEN a:=NIL; RETURN END;
  w:=words; IF w=1 THEN w:=2 END;
  alloc_large(a,w);
  done:=(a#NIL);
  IF done THEN INC(user_sum,words) ELSE no_memory END;
END allocate;

PROCEDURE deallocate(VAR a: adr; words: int);
  VAR w: int;
BEGIN
  done:=TRUE;
  IF words<0 THEN bad_parm; RETURN  END;
  IF a=NIL THEN RETURN END;
  IF words=0 THEN bad_parm; RETURN  END;
  w:=words;
  IF w=1 THEN w:=2 END;
  dealloc_large(a,w);   a:=NIL;
  IF done THEN DEC(user_sum,words) END;
END deallocate;

PROCEDURE try_reallocate(a: adr; size,new_size: int;
                                       VAR res: adr);
  VAR sz,nsz: int;
BEGIN
  done:=TRUE;
  IF (size<0) OR (new_size<0) THEN bad_parm; RETURN END;
  IF (size=0) OR (a=NIL) THEN
    IF new_size=0 THEN res:=NIL; RETURN END;
    nsz:=new_size; IF nsz=1 THEN nsz:=2 END;
    alloc_large(res,nsz);
    IF res#NIL THEN INC(user_sum,new_size); RETURN END;
    no_memory; RETURN
  END;
  IF new_size=0 THEN
      sz:=size; IF sz=1 THEN sz:=2 END;
      dealloc_large(a,sz);
      IF NOT done THEN RETURN END;
      res:=NIL; DEC(user_sum,size); RETURN
  END;
  nsz:=new_size; IF nsz=1 THEN nsz:=2 END;
  sz:=size;      IF sz=1  THEN sz:=2  END;
  IF sz=nsz THEN res:=a; INC(user_sum,new_size-size); RETURN END;
  realloc_large(a,sz,nsz,res);
  IF done THEN
    INC(user_sum,new_size-size);
  ELSE
    res:=a
  END
END try_reallocate;

PROCEDURE REALLOCATE(VAR a: adr; VAR high: int;
                                 len,byte_size: int);
  VAR res: adr; size,new_size: int;
  CONST b=BYTES(sys.WORD);
BEGIN
  done:=TRUE;
  size:= ((high+1)*byte_size+b-1) DIV b;
  new_size:= (len*byte_size+b-1)  DIV b;
  try_reallocate(a,size,new_size,res);
  IF done THEN a:=res; high:=len-1 ELSE HALT(error) END
END REALLOCATE;

PROCEDURE reallocate(VAR a: adr; VAR high: int;
                                 len,byte_size: int);
  VAR res: adr; size,new_size: int;
  CONST b=BYTES(sys.WORD);
BEGIN
  done:=TRUE;
  size:= ((high+1)*byte_size+b-1) DIV b;
  new_size:= (len*byte_size+b-1)  DIV b;
  try_reallocate(a,size,new_size,res);
  IF done THEN a:=res; high:=len-1 END
END reallocate;

PROCEDURE set_limit(total:int);
BEGIN
  done:=TRUE;
  limit:=total;
END set_limit;

PROCEDURE set_credit(words: INTEGER);
BEGIN
  done:=TRUE;
  credit:=max(words,2);
END set_credit;

PROCEDURE statistics(VAR os: int; VAR fr: int; VAR sum: int);
BEGIN
  os:=from_os; fr:=free; sum:=user_sum;
  done:=TRUE;
END statistics;

VAR   i: INTEGER;
  dummy: ARRAY [0..5] OF sys.WORD;

BEGIN
  ASSERT(NOT (hbit IN bit(NIL)));
  ASSERT(minsize<=010h);
  done:=TRUE; error:=0;

  pool:=NIL;
  FOR i:=0 TO HIGH(gcs) DO gcs[i]:=NULL END;

  _to_os:=FALSE;
  from_os:=0; free:=0; user_sum:=0;
  set_limit(-1);
  set_credit(4*256);

  ------- False Block Initialization -------
  dummy[0]:=marked(0); dummy[5]:=marked(0);
  dummy[1]:=0; dummy[4]:=0;
  FB:=sys.ADR(dummy[1]);
  FB^.next:=FB; FB^.prev:=FB; curr:=FB;
  ------------------------------------------

END Heap.
