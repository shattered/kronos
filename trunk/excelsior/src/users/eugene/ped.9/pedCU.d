DEFINITION MODULE pedCU; (* Sem 06-Mar-87. (c) KRONOS *)

FROM pedModel   IMPORT  signal, board;

TYPE range=RECORD
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
PROCEDURE extend(s: signal; n: INTEGER);
PROCEDURE truncate(s: signal);

PROCEDURE app(s: signal; VAL sg: segment);
PROCEDURE del(s: signal; n: INTEGER);
-- без проверки технологических норм,
-- с отрисовкой изменений на экране.

PROCEDURE Len       (x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE Side      (x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE InsertRange(mdl: board; sig: signal;
                      rng: range; sz,lay: INTEGER; fix: BOOLEAN): BOOLEAN;
PROCEDURE InsertViasF(mdl: board; sig: signal;
                      x,y,sz,vsz: INTEGER; lays: BITSET): BOOLEAN;
PROCEDURE InsertViasV(mdl: board; sig: signal;
                      x,y,sz,vsz: INTEGER; lays: BITSET): BOOLEAN;

PROCEDURE DeleteRange(sig: signal; rng: range; lay: INTEGER);
PROCEDURE DeleteVias (sig: signal; x,y: INTEGER);

PROCEDURE FindSignal(mdl: board; sig: signal; x,y,l,r: INTEGER): signal;

TYPE IterProc=PROCEDURE (): BOOLEAN;

PROCEDURE SeekInBox(mdl: board; box: range; ip: IterProc);

PROCEDURE Areas(sig: signal): INTEGER;

PROCEDURE Shoted?(mdl: board; sig: signal; rng: range; sz,lay: INTEGER;
                  VAR shot: signal; VAR shn: INTEGER): BOOLEAN;

END pedCU.
