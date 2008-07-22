DEFINITION MODULE pedTopology; (* Sem 06-Mar-87. (c) KRONOS *)

FROM pedModel   IMPORT  signal, board;

VAR X1,Y1   : INTEGER;
    X2,Y2   : INTEGER;
    Size    : INTEGER;
    ViasSize: INTEGER;
    Layer   : BITSET;
    LineXs  : INTEGER;
    LineYs  : INTEGER;
    LineXe  : INTEGER;
    LineYe  : INTEGER;
    Fixed   : BOOLEAN;
    Empty   : BOOLEAN;
    Undef   : BOOLEAN;
    Ident   : INTEGER;
    Signal  : signal;

PROCEDURE StartConductor(c: signal);
PROCEDURE NextConductor;
PROCEDURE FindConductor(id: INTEGER);

PROCEDURE App;
PROCEDURE Del;

PROCEDURE Side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
PROCEDURE OnLine(x1,y1,x2,y2: INTEGER): BOOLEAN;
PROCEDURE Len(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE InsertRange
  (size,layer: INTEGER; fix: BOOLEAN; mdl: board; sig: signal): BOOLEAN;

PROCEDURE DeleteRange(layer: INTEGER; sig: signal);

PROCEDURE InsertVias
  (size,vsize: INTEGER; fix: BOOLEAN; mdl: board; sig: signal): BOOLEAN;

PROCEDURE DeleteVias(sig: signal);

PROCEDURE FindSignal(x,y,l,size: INTEGER; mdl: board; sig: signal);

TYPE IterProc=PROCEDURE (): BOOLEAN;

PROCEDURE SeekInBox(mdl: board; ip: IterProc);

PROCEDURE Areas(sig: signal): INTEGER;

PROCEDURE Shoted?(size,layer: INTEGER; mdl: board; sig: signal): BOOLEAN;

VAR ShotedSignal: signal;
    ShotedIdent : INTEGER;

END pedTopology.
