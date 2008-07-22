IMPLEMENTATION MODULE cdsHeap;          (* Andy 04-Aug-89. (c) KRONOS *)
                                        (* Andy 11-Aug-89. (c) KRONOS *)
-- Для старой osMemory!

IMPORT  mem: Heap;
IMPORT  sys: SYSTEM;
IMPORT  cmd: defCodes;
FROM ModelPbl   IMPORT  RaiseInMe, Message, MemoryOverflow;

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

   area = POINTER TO piece;
  piece = RECORD
            size: INTEGER;
            next: area;
          END;

PROCEDURE min(a,b:int):int; BEGIN IF a<b THEN RETURN a ELSE RETURN b END END min;
PROCEDURE max(a,b:int):int; BEGIN IF a<b THEN RETURN b ELSE RETURN a END END max;

CONST no_mem= 4Eh;
       abort= 4Fh;

CONST minsize= SIZE(node)+1;
        bsize= 32;

CONST hbit= BITS(BITSET)-1;
      busy_mask= {hbit};

CONST Sorry ='Переполнена динамическая память, очень жаль...';

VAR    curr: list;
         FB: list;
  free_area: area;

-----------------------  SLOTS  MANAGER  ----------------------
                       ------------------
TYPE slot  = POINTER TO scrap;
     scrap = RECORD
               size: INTEGER;
               next: slot;
               prev: slot;
             END;

VAR StdSize: int;

VAR larges: slot;
    smalls: slot;

VAR mem_allowed: INTEGER;

PROCEDURE add_slot(VAR pool: slot; VAR s: slot; size: int; stdsize: int);
  VAR req: int;
  CONST addsz=SIZE(scrap);
BEGIN INC(size,addsz);
  req:=max(stdsize+addsz,size);
  IF mem_allowed>=0 THEN
      IF mem_allowed<size THEN s:=NIL; RETURN END;
      req:=min(req,mem_allowed);
  END;
--  sch.?1;
  mem.ALLOCATE(s,req);
  WHILE s=NIL DO
      IF req<=size THEN (* sch.?0;*) RETURN END;
      DEC(stdsize,256);
      req:=max(stdsize+addsz,size);
      IF mem_allowed>=0 THEN req:=min(req,mem_allowed) END;
      mem.ALLOCATE(s,req);
  END;
  s^.size:=req-addsz;
  s^.next:=pool; s^.prev:=NIL;
  IF pool#NIL THEN pool^.prev:=s END; pool:=s;
  IF mem_allowed>=0 THEN DEC(mem_allowed,req) END;
(*  sch.?0; *)
END add_slot;

PROCEDURE free_slot(VAR pool: slot; s: slot);
  VAR n,p: slot;
BEGIN n:=s^.next; p:=s^.prev;
(*  sch.?1; *)
  IF p#NIL THEN p^.next:=n END;
  IF n#NIL THEN n^.prev:=p END;
  IF pool=s THEN pool:=n END;
  IF mem_allowed>=0 THEN INC(mem_allowed,s^.size+SIZE(scrap)) END;
  mem.DEALLOCATE(s,s^.size+SIZE(scrap));
(*  sch.?0; *)
END free_slot;

PROCEDURE min_slot(pool: slot):int;
  VAR s: slot; m: int;
BEGIN m:=0; s:=pool;
  WHILE s#NIL DO IF m>s^.size THEN m:=s^.size END; s:=s^.next END;
  RETURN m
END min_slot;

PROCEDURE flush_ring(bound: int): BOOLEAN;
  VAR p,l,r: list;
      s,next: slot; res: BOOLEAN;
BEGIN res:=FALSE;
  s:=larges;
  WHILE s#NIL DO
     next:=s^.next;
     p:=list(int(s)+SIZE(scrap)+1);
     IF NOT (hbit IN bit(p^.size)) AND
            (s^.size=p^.size+4) AND (p^.size>=bound) THEN
         l:=p^.prev; r:=p^.next;
         l^.next:=r; r^.prev:=l;
         IF curr=p THEN curr:=r END;
         free_slot(larges,s); res:=TRUE;
     END;
     s:=next;
  END;
  RETURN res
END flush_ring;

PROCEDURE flush_area(): BOOLEAN;
  VAR p: piece; pc,c: area; s: slot; bnd: int;
  PROCEDURE check(a: area; VAR s: slot):BOOLEAN;
  BEGIN s:=smalls;
    WHILE s#NIL DO
       IF (int(s)+SIZE(scrap)=int(a)) AND (s^.size=a^.size+SIZE(piece))
       THEN RETURN TRUE END;
       s:=s^.next;
    END;
    RETURN FALSE
  END check;
  VAR res: BOOLEAN;
BEGIN
  res:=FALSE;
  bnd:=min_slot(smalls)-SIZE(piece);
  p.size:=0; p.next:=free_area;
  pc:=sys.ADR(p); c:=pc^.next;
  WHILE c#NIL DO
      IF (c^.size>=bnd) AND check(c,s) THEN
           pc^.next:=c^.next; c:=c^.next;
           free_slot(smalls,s); res:=TRUE;
      ELSE pc:=c; c:=c^.next END;
  END;
  free_area:=p.next;
  RETURN res
END flush_area;

PROCEDURE release_all;
  PROCEDURE free_pool(VAR pool: slot);
    VAR next: slot;
  BEGIN
    WHILE pool#NIL DO
      next:=pool^.next;
      mem.DEALLOCATE(pool,pool^.size+SIZE(scrap));
      pool:=next;
    END;
  END free_pool;
BEGIN
  free_pool(larges); free_pool(smalls);
END release_all;

PROCEDURE garbage_collector(): BOOLEAN; FORWARD;

PROCEDURE finish_heap;
BEGIN release_all;
      -- mem.remove_collector(garbage_collector);
END finish_heap;

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

PROCEDURE alloc_large(VAR a: adr; w: int);
  VAR p,l,r: list;
      s: POINTER TO int;
      new: slot;
BEGIN
  p:=curr;
  LOOP IF p^.size>=w THEN EXIT END;
       p:=p^.next;
       IF p=curr THEN
           add_slot(larges,new,w+4,StdSize);
           IF new=NIL THEN
             IF garbage_collector() THEN add_slot(larges,new,w+4,StdSize) END;
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
       p^.size:=marked(p^.size); set_end(p);
       a:=adr(p)+1; RETURN
  ELSE -- split block
       s:=adr(p)+p^.size-w; a:=adr(s)+1;
       s^:=marked(w); set_end(list(s));
       DEC(p^.size,w+2); set_end(p);
       curr:=p; RETURN
  END;
END alloc_large;

PROCEDURE dealloc_large(a: adr; w: int);
  VAR b: list; l,r,n: list; rr,rl: list;
BEGIN
  b:=list(int(a)-1); ASSERT(hbit IN bit(b^.size),abort);
  b^.size:=int(bit(b^.size)-busy_mask);
  ASSERT((b^.size-w) < minsize,abort);
  l:=list(int(b)-1); r:=list(int(b)+b^.size+2);
  IF NOT (hbit IN bit(l^.size)) THEN -- left is free
       l:=list(int(l)-l^.size-1);
       IF NOT (hbit IN bit(r^.size)) THEN -- right is free
            rr:=r^.next; rl:=r^.prev;
            rl^.next:=rr; rr^.prev:=rl;
            INC(l^.size,b^.size+r^.size+4); set_end(l); curr:=l;
       ELSE INC(l^.size,b^.size+2); set_end(l); curr:=l END;
  ELSE IF NOT (hbit IN bit(r^.size)) THEN -- right is free
            rr:=r^.next; rl:=r^.prev;
            b^.next:=rr; b^.prev:=rl;
            rl^.next:=b; rr^.prev:=b;
            INC(b^.size,r^.size+2); set_end(b); curr:=b;
       ELSE set_end(b); curr:=b;
            n:=FB^.next;
            b^.prev:=FB; b^.next:=n;
            FB^.next:=b; n^.prev:=b;
       END;
  END;
  IF (curr^.size>=(StdSize-4)) AND flush_ring(StdSize-4) THEN END;
END dealloc_large;

PROCEDURE realloc_large(a: adr; size,new_size: int;
                                      VAR res: adr): BOOLEAN;
  VAR p,e,l,r,nxt: list; rsz,nsz: int;
BEGIN


  p:=list(int(a)-1);
  rsz:=int(bit(p^.size)-busy_mask);
  IF size<new_size THEN
       IF new_size<=rsz THEN res:=a; RETURN TRUE END;
       nxt:=list(int(p)+rsz+2);
       IF NOT (hbit IN bit(nxt^.size)) AND
              ((rsz+2+nxt^.size)>=new_size) THEN
            res:=a; l:=nxt^.prev; r:=nxt^.next; nsz:=nxt^.size;
            IF (rsz+2+nsz-new_size)<minsize THEN
                 l^.next:=r; r^.prev:=l; IF curr=nxt THEN curr:=r END;
                 p^.size:=marked(rsz+2+nsz); set_end(p);
            ELSE e:=list(int(p)+new_size+2);
                 p^.size:=marked(new_size); set_end(p);
                 e^.size:=rsz+nsz-new_size; set_end(e);
                 l^.next:=e; e^.prev:=l; r^.prev:=e; e^.next:=r;
                 curr:=e;
            END;
            RETURN TRUE
       ELSE alloc_large(res,new_size); IF res=NIL THEN RETURN FALSE END;
            move(res,a,size);
            dealloc_large(a,size); RETURN TRUE
       END;
  ELSE IF (rsz-new_size)<minsize THEN res:=a; RETURN TRUE END;
       res:=a; e:=list(int(p)+new_size+2);
       p^.size:=marked(new_size);       set_end(p);
       e^.size:=marked(rsz-new_size-2); set_end(e);
       dealloc_large(adr(int(e)+1),rsz-new_size-2); RETURN TRUE
  END;
END realloc_large;

--------------------------  TITBITS  ---------------------------
                          -----------
TYPE
  titbit = POINTER TO slice;
   slice = RECORD next: titbit END;

VAR slices: ARRAY [0..bsize] OF titbit;

PROCEDURE order_list(VAR list: area);
  VAR p: piece;
      pstart,start,pmin,min,pc,c: area;
BEGIN IF list=NIL THEN RETURN END;
  p.size:=0; p.next:=list;
  pstart:=sys.ADR(p); start:=list;
  WHILE start#NIL DO
       pmin:=pstart; min:=start;
       pc:=start; c:=start^.next;
       WHILE c#NIL DO
            IF int(c)<int(min) THEN min:=c; pmin:=pc END;
            pc:=c; c:=c^.next;
       END;
       IF start#min THEN
           pmin^.next:=min^.next;
           min^.next:=start; pstart^.next:=min; start:=min;
       END;
       pstart:=start; start:=start^.next;
  END;
  list:=p.next;
END order_list;

PROCEDURE sz(a:area):int;
BEGIN IF hbit IN bit(a^.size) THEN RETURN 1
      ELSE RETURN a^.size+SIZE(piece) END;
END sz;

PROCEDURE merge(l: area; sl: int; r: area);
  VAR sr: int;
BEGIN
  IF int(l)+sl=int(r) THEN -- merge
       sr:=sz(r);
       l^.size:=sl+sr-SIZE(piece);
       IF sr=1 THEN l^.next:=area(bit(r^.size)-busy_mask)
       ELSE l^.next:=r^.next END;
  ELSE IF sl=1 THEN l^.size:=int(bit(r)+busy_mask)
       ELSE l^.size:=sl-SIZE(piece); l^.next:=r END;
  END;
END merge;

PROCEDURE put(VAR list: area; a: area; w: int);
  CONST M=busy_mask;
  PROCEDURE mk(next: area);
  BEGIN IF w=1 THEN a^.size:=int(bit(next)+M)
        ELSE a^.size:=w-SIZE(piece); a^.next:=next END;
  END mk;
  VAR l,r: area;
BEGIN
  IF list=NIL THEN mk(NIL); list:=a; RETURN END;
  IF int(a)<int(list) THEN merge(a,w,list); list:=a; RETURN END;
  l:=list;
  IF hbit IN bit(l^.size) THEN r:=area(bit(l^.size)-M)
  ELSE r:=l^.next END;
  WHILE r#NIL DO
    IF int(a)<int(r) THEN merge(a,w,r); merge(l,sz(l),a); RETURN END;
    l:=r;
    IF hbit IN bit(l^.size) THEN r:=area(bit(l^.size)-M)
    ELSE r:=l^.next END;
  END;
  mk(NIL); merge(l,sz(l),a);
END put;

PROCEDURE clean_list(VAR list: area);
  VAR p: piece;
      pc,c,nxt: area; tiny?: BOOLEAN;
  PROCEDURE link(a:titbit); BEGIN a^.next:=slices[1]; slices[1]:=a END link;
BEGIN IF list=NIL THEN RETURN END;
  p.size:=0; p.next:=list;
  pc:=sys.ADR(p); c:=pc^.next;
  WHILE c#NIL DO
      tiny?:= hbit IN bit(c^.size);
      IF tiny? THEN nxt:=area(bit(c^.size)-busy_mask);
           link(titbit(c)); pc^.next:=nxt; c:=nxt;
      ELSE pc:=c; c:=c^.next END;
  END;
  list:=p.next;
END clean_list;

PROCEDURE garbage_collector(): BOOLEAN;
  VAR el,nxt: titbit; sz: int; res: BOOLEAN;
BEGIN
  res:=flush_ring(0);
  order_list(free_area);
  FOR sz:=HIGH(slices) TO 1 BY -1 DO
      el:=slices[sz];
      WHILE el#NIL DO
          nxt:=el^.next; put(free_area,area(el),sz); el:=nxt;
      END;
      slices[sz]:=NIL;
  END;
  clean_list(free_area);
  RETURN flush_area() OR res;
END garbage_collector;

PROCEDURE try_split(VAR a: titbit; w: int);
  VAR sz: int; rem: titbit;
BEGIN
  FOR sz:=w+1 TO HIGH(slices) DO
      IF slices[sz]#NIL THEN
          a:=slices[sz]; slices[sz]:=a^.next;
          rem:=titbit(int(a)+w);
          rem^.next:=slices[sz-w]; slices[sz-w]:=rem;
          RETURN
      END;
  END;
  a:=NIL;
END try_split;

PROCEDURE allocate_small(VAR a: titbit; w: int);
  CONST hsz=SIZE(piece);
  VAR d: titbit; s: slot; sz: int;
      GC_used?: BOOLEAN;
BEGIN GC_used?:=FALSE;
  LOOP
     LOOP IF free_area#NIL THEN EXIT END;
          add_slot(smalls,s,w,StdSize DIV 2);
          IF s=NIL THEN IF GC_used? THEN a:=NIL; RETURN END;
               try_split(a,w); IF a#NIL THEN RETURN END;
               IF garbage_collector() THEN END; GC_used?:=TRUE;
          ELSE IF s^.size<hsz THEN
                   a:=titbit(int(s)+SIZE(scrap)); RETURN
               END;
               free_area:=area(int(s)+SIZE(scrap));
               free_area^.next:=NIL;
               free_area^.size:=s^.size-hsz;
               EXIT
          END;
     END;
     IF free_area^.size>=w THEN
         DEC(free_area^.size,w);
         a:=titbit(int(free_area)+hsz+free_area^.size); RETURN
     END;
     d:=titbit(free_area); sz:=hsz+free_area^.size;
     free_area:=free_area^.next;
     IF sz>=w THEN a:=d; DEC(sz,w);
          IF sz>0 THEN d:=titbit(int(d)+w);
               d^.next:=slices[sz]; slices[sz]:=d;
          END;
          RETURN
     ELSE d^.next:=slices[sz]; slices[sz]:=d END;
  END;
END allocate_small;
------------------------------------------------------------------------

PROCEDURE ALLOCATE(VAR a: adr; words: int);
  VAR t: titbit;
BEGIN
  ASSERT(words>=0,abort);
  IF words=0 THEN a:=NIL; RETURN END;
  IF words>bsize THEN -- large request
       alloc_large(a,words);    IF a#NIL THEN RETURN END;
       Message:=Sorry; RaiseInMe(MemoryOverflow);
  ELSE -- small request
       t:=slices[words];
       IF t#NIL THEN slices[words]:=t^.next; a:=t; RETURN END;
       allocate_small(a,words); IF a#NIL THEN RETURN END;
       Message:=Sorry; RaiseInMe(MemoryOverflow);
  END;
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: adr; words: int);
  VAR p: list; t: titbit;
BEGIN
  ASSERT(words>=0,abort);
  IF words=0 THEN ASSERT(a=NIL,abort); RETURN END;
  IF words>bsize THEN -- large request
       dealloc_large(a,words); a:=NIL; RETURN
  ELSE -- small request
       t:=titbit(a); a:=NIL;
       t^.next:=slices[words]; slices[words]:=t;
       RETURN
  END;
END DEALLOCATE;

PROCEDURE Allocate(VAR a: adr; words: int);
  VAR t: titbit;
BEGIN
  ASSERT(words>=0,abort);
  IF words=0 THEN a:=NIL; RETURN END;
  IF words>bsize THEN -- large request
       alloc_large(a,words); IF a#NIL THEN RETURN END;
       Message:=Sorry; RaiseInMe(MemoryOverflow);
  ELSE -- small request
       t:=slices[words];
       IF t#NIL THEN slices[words]:=t^.next; a:=t; RETURN END;
       allocate_small(a,words); IF a#NIL THEN RETURN END;
       Message:=Sorry; RaiseInMe(MemoryOverflow);
  END;
END Allocate;

PROCEDURE Deallocate(VAR a: adr; words: int);
  VAR p: list; t: titbit;
BEGIN
  ASSERT(words>=0,abort);
  IF words=0 THEN ASSERT(a=NIL,abort); RETURN END;
  IF words>bsize THEN -- large request
       dealloc_large(a,words); a:=NIL; RETURN
  ELSE -- small request
       t:=titbit(a); a:=NIL;
       t^.next:=slices[words]; slices[words]:=t;
       RETURN
  END;
END Deallocate;

PROCEDURE try_reallocate(a: adr; size,new_size: int;
                                       VAR res: adr): BOOLEAN;
  VAR tiny_old?,tiny_new?: BOOLEAN;
      t: titbit;
  VAR p: list;
BEGIN ASSERT((size>=0) AND (new_size>=0),abort);
  IF size=0 THEN Allocate(res,new_size); RETURN (res#NIL) OR (new_size=0) END;
  IF new_size=0 THEN res:=a; DEALLOCATE(res,size); RETURN TRUE END;
  IF size=new_size THEN res:=a; RETURN TRUE END;
  tiny_old?:= size<=bsize; tiny_new?:= new_size<=bsize;
  IF tiny_old?#tiny_new? THEN
       IF tiny_old? THEN
            alloc_large(res,new_size); IF res=NIL THEN RETURN FALSE END;
            move(res,a,size);
            t:=titbit(a); t^.next:=slices[size]; slices[size]:=t;
            RETURN TRUE
       ELSE t:=slices[new_size];
            IF t#NIL THEN slices[new_size]:=t^.next; res:=t;
            ELSE allocate_small(res,new_size);
                 IF res=NIL THEN RETURN FALSE END;
            END;
            move(res,a,new_size);
            dealloc_large(a,size); RETURN TRUE
       END;
  ELSE IF tiny_old? THEN
            IF size<new_size THEN
                 t:=slices[new_size];
                 IF t#NIL THEN slices[new_size]:=t^.next; res:=t;
                 ELSE allocate_small(res,new_size);
                      IF res=NIL THEN RETURN FALSE END;
                 END;
                 move(res,a,size); t:=titbit(a);
            ELSE res:=a; t:=titbit(int(a)+new_size); DEC(size,new_size) END;
            t^.next:=slices[size]; slices[size]:=t; RETURN TRUE
       ELSE RETURN realloc_large(a,size,new_size,res) END;
  END;
END try_reallocate;

PROCEDURE REALLOCATE(VAR a: adr; size,new_size: int);
  VAR res: adr;
BEGIN IF try_reallocate(a,size,new_size,res) THEN a:=res
      ELSE Message:=Sorry; RaiseInMe(MemoryOverflow) END;
END REALLOCATE;

PROCEDURE Reallocate(VAR a: adr; size,new_size: int): BOOLEAN;
  VAR res: adr;
BEGIN
  IF try_reallocate(a,size,new_size,res) THEN
       a:=res; RETURN TRUE
  ELSE         RETURN FALSE END;
END Reallocate;

PROCEDURE set_limit(total:int); BEGIN mem_allowed:=total END set_limit;

PROCEDURE set_os_request_size(words: INTEGER);
BEGIN ASSERT(words>=2,abort); StdSize:=words;
END set_os_request_size;

PROCEDURE mem_free(with_os?:BOOLEAN):int;
  VAR sum,i,s:int;
  VAR p: list; f: area; t: titbit;
BEGIN sum:=0;
  IF with_os? THEN
  (*
     mem.lock(mem.common_area);
     s:=mem.free(mem.common_area);
     IF mem_allowed<0 THEN INC(sum,s)
                      ELSE INC(sum,min(mem_allowed,s))
     END;
     mem.unlock(mem.common_area);
  *)
  END;
  p:=curr;
  LOOP INC(sum,p^.size); p:=p^.next; IF p=curr THEN EXIT END END;
  FOR i:=1 TO HIGH(slices) DO
     t:=slices[i]; WHILE t#NIL DO INC(sum,i); t:=t^.next END;
  END;
  f:=free_area;
  WHILE f#NIL DO INC(sum,f^.size+SIZE(piece)); f:=f^.next END;
  RETURN sum
END mem_free;

VAR
  dummy: ARRAY [0..5] OF sys.WORD;
  i: INTEGER;

BEGIN
  ASSERT(NOT (hbit IN bit(NIL)));
  ASSERT(SIZE(piece)=2);
  ASSERT((bsize+1)>=(minsize-2));

  --  reso.Final(finish_heap);
  --  mem.install_collector(garbage_collector);

  larges:=NIL; smalls:=NIL;
  FOR i:=0 TO HIGH(slices) DO slices[i]:=NIL END;
  free_area:=NIL;
  set_limit(-1);
  set_os_request_size(8*256);
  dummy[0]:=marked(0); dummy[5]:=marked(0);
  dummy[1]:=0; dummy[4]:=0;
  FB:=sys.ADR(dummy[1]);
  FB^.next:=FB; FB^.prev:=FB; curr:=FB;
END cdsHeap.
