DEFINITION MODULE veSeg; (* Sem 17-Feb-88. (c) KRONOS *)

FROM Model      IMPORT  Object;

VAR Level: INTEGER;

PROCEDURE New(): Object;

PROCEDURE Tie(s: Object; VAR to: Object);

TYPE IterProc = PROCEDURE (Object): BOOLEAN;

PROCEDURE Ls(s: Object; ip: IterProc);

PROCEDURE AppLine(x1,y1,x2,y2: INTEGER);

PROCEDURE Drow(p: Object; x,y: INTEGER);
(* x,y - смещение системы координат линий
   рисунка p относительно абсолютной
   системы координат *)

PROCEDURE DrowRingTree(p,c: Object; x,y: INTEGER);
(* x,y - смещение системы координат кольца p относительно абсолютной
   системы координат *)

PROCEDURE DrowTree(p,c: Object; x,y: INTEGER);
(* x,y - смещение системы координат линий
   рисунка p относительно абсолютной
   системы координат *)

PROCEDURE CopyTree(s: Object): Object;

PROCEDURE vect(x1,y1,x2,y2: INTEGER);

PROCEDURE cursor(x,y,n: INTEGER);

END veSeg.
