DEFINITION MODULE libHeap; (* Sem 10-Jan-91. (c) KRONOS *)

FROM SYSTEM    IMPORT ADDRESS;

TYPE
  AREA;
  pAREA = POINTER TO AREA;

CONST FREE=AREA(NIL);

-- this procedures raise exception if no memory
PROCEDURE ALLOCATE  (VAR a: ADDRESS; words: INTEGER);
PROCEDURE DEALLOCATE(VAR a: ADDRESS; words: INTEGER);
PROCEDURE REALLOCATE(VAR a: ADDRESS; VAR high: INTEGER;
                                     e_no,e_bytes: INTEGER);
------------------------------------------------------------------------

-- this procedures return NIL  if no memory
PROCEDURE allocate   (VAR a: ADDRESS; words: INTEGER);
PROCEDURE alloc_tag  (VAR a: ADDRESS; words: INTEGER; VAR tg: AREA);
PROCEDURE dealloc_tag(VAR a: ADDRESS; words: INTEGER; VAR tg: AREA);

PROCEDURE dealloc_adr(VAR a: ADDRESS);
PROCEDURE dealloc_all(VAR tag: AREA);

------------------------------------------------------------------------

PROCEDURE realloc(VAR a: ADDRESS; sz: INTEGER): BOOLEAN;
-- return FALSE if no memory

PROCEDURE resize (a: ADDRESS; sz: INTEGER; VAR new_sz: INTEGER);

PROCEDURE change_tag(a: ADDRESS; VAR to: AREA);
PROCEDURE change_all(VAR from,to: AREA);
PROCEDURE tag?(a: ADDRESS): pAREA;

TYPE proc=PROCEDURE (ADDRESS,INTEGER);

PROCEDURE show_mem(tag: AREA; p: proc);

END libHeap.
