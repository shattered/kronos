DEFINITION MODULE inDesig; (* Sem 06-Oct-90. (c) KRONOS *)

IMPORT sym : inSym;

FROM pcTab      IMPORT  ref;
FROM inSym      IMPORT  access;
FROM inFlow     IMPORT  node;

TYPE
  trap = RECORD
    cnt  : INTEGER;
    scnt : INTEGER;
    vcnt : INTEGER;
    pcnt : INTEGER;
    mcnt : INTEGER;
    rcnt : INTEGER;
    mgcnt: INTEGER;
    block: node;
  END;

VAR
  expression: PROCEDURE (ref, VAR access);

PROCEDURE add_offset(VAR a: access; offset: INTEGER);
PROCEDURE alloc_var (VAR a: access; size: INTEGER);

PROCEDURE save   (VAR t: trap);
PROCEDURE restore(t: trap);

PROCEDURE min    (l: ref; VAR a: access);
PROCEDURE max    (l: ref; VAR a: access);
PROCEDURE low    (l: ref; VAR a: access);
PROCEDURE high   (l: ref; VAR a: access);
PROCEDURE len    (l: ref; VAR a: access);
PROCEDURE bytes  (l: ref; VAR a: access);
PROCEDURE bytes_u(l: ref; VAR a: access);
PROCEDURE adr    (l: ref; VAR a: access);
PROCEDURE adr_u  (l: ref; VAR a: access);
PROCEDURE index  (l: ref; VAR a: access);
PROCEDURE field  (l: ref; VAR a: access);

PROCEDURE check_range (range: ref);
-- вычитает fr !!!

PROCEDURE make_bitset (sz: INTEGER);

PROCEDURE designator  (l: ref; VAR a: access);
PROCEDURE designator_u(l: ref; VAR a: access);

END inDesig.
