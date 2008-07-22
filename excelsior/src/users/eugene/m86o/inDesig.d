DEFINITION MODULE inDesig; (* Sem 06-Oct-90. (c) KRONOS *)

FROM pcTab      IMPORT  ref;
FROM inSym      IMPORT  access;

VAR
  expression: PROCEDURE (ref, VAR access);

PROCEDURE add_offset(VAR a: access; offset: INTEGER);
PROCEDURE alloc_var (VAR a: access; size: INTEGER);

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
