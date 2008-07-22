DEFINITION MODULE pedCU; (* Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD;
FROM pedModel    IMPORT signal, board;

TYPE
  range=RECORD
    x1,y1: INTEGER;
    x2,y2: INTEGER;
  END;
  segment=RECORD
    x1,y1: INTEGER;
    x2,y2: INTEGER;
    size : INTEGER;
    lays : BITSET;
    pipe : INTEGER;
    fix  : BOOLEAN;
  END;
-- IF pipe#0 THEN x1=x2; y1=y2 END; size>=pipe;

PROCEDURE unpack(s: signal; n: INTEGER; VAR sg: segment);
PROCEDURE pack  (s: signal; n: INTEGER; sg: segment);
PROCEDURE extend(s: signal; n: INTEGER);
PROCEDURE truncate(s: signal);

PROCEDURE app(s: signal; sg: segment);
PROCEDURE app_n(s: signal; sg: segment): INTEGER;
PROCEDURE del(s: signal; n: INTEGER);
-- без проверки технологических норм,

PROCEDURE Len       (x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE Side      (x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

-------------------------------------------------------------------------
--  редактирование
TYPE e_proc = PROCEDURE (WORD,signal,INTEGER,BOOLEAN);

PROCEDURE InsertRange(mdl: board; sig: signal; p: e_proc; x: WORD;
                      rng: range; sz,lay: INTEGER; fix: BOOLEAN): BOOLEAN;
PROCEDURE InsertVias (mdl: board; sig: signal; p: e_proc; xx: WORD;
                      x,y,s,vs: INTEGER; lays: BITSET; fix: BOOLEAN): BOOLEAN;
PROCEDURE DeleteRange(sig: signal; p: e_proc; x: WORD;
                      rng: range; lay: INTEGER);
PROCEDURE DeleteVias (sig: signal; p: e_proc; xx: WORD;
                      x,y: INTEGER);
-------------------------------------------------------------------------

PROCEDURE FindSignal
          (mdl: board; sig: signal; x,y,l,r: INTEGER; VAR n: INTEGER): signal;

TYPE IterProc=PROCEDURE (signal,INTEGER): BOOLEAN;

PROCEDURE SkipSegments(s: signal; box: range; VAR n: INTEGER);
PROCEDURE SeekInBox   (mdl: board; box: range; ip: IterProc);

PROCEDURE Areas(sig: signal): INTEGER;

PROCEDURE Shoted?(mdl: board; sig: signal; rng: range; sz,lay: INTEGER;
                  VAR shot: signal; VAR shn: INTEGER): BOOLEAN;

END pedCU.
