DEFINITION MODULE libCrash; (* Sem 11-Feb-87. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, WORD;
FROM osKernel    IMPORT process;

TYPE
  trap_ref=POINTER TO trap;
  trap=RECORD
    prs   : process;
    next  : trap_ref;
    gate  : ADDRESS;
    pp    : ADDRESS;
    G     : ADDRESS;
    L     : ADDRESS;
    M     : BITSET;
    PC    : INTEGER;
    PL    : ARRAY [0..5] OF WORD;
    txt   : ARRAY [0..255] OF CHAR;
  END;

PROCEDURE enter(VAR r: trap): BOOLEAN;

PROCEDURE exit (VAR r: trap);

PROCEDURE raise(fmt: ARRAY OF CHAR; SEQ arg: WORD);

PROCEDURE re_raise(VAR e: trap);

END libCrash.
