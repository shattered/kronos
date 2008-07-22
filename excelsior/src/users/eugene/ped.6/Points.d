DEFINITION MODULE Points; (* Sem 27-Jun-87. (c) KRONOS *)

FROM pedEditor  IMPORT  Sheet;

TYPE pPoint=POINTER TO Point;
     ppPoint=POINTER TO pPoint;
     Point=RECORD
       x,y,l      : INTEGER; (* Координаты точки *)
       v          : INTEGER; (* Расстояние от источника *)
       d          : INTEGER; (* Направление *)
       id         : INTEGER;
       stopped    : BOOLEAN;
       hard       : BOOLEAN;
       Xfwd,Dfwd  : pPoint;
       Mfwd       : pPoint;
       Mback      : pPoint;
       Xback      : ppPoint;
       Dback      : pPoint;
     END;

PROCEDURE NewPoint(x,y,l,v,d,id: INTEGER; hd: BOOLEAN; sht: Sheet);

PROCEDURE NewIdent(): INTEGER;

PROCEDURE FindPoint(x,y,l,id: INTEGER): pPoint;

PROCEDURE FindOther(x,y,l,id: INTEGER): pPoint;

PROCEDURE FindMin(): pPoint;

PROCEDURE NewVeight(p: pPoint; v: INTEGER);

PROCEDURE KilLastPoint;

PROCEDURE Cat(from,to: INTEGER);

PROCEDURE RemovePoints;

PROCEDURE DeallocPoints;

PROCEDURE ShotWave;

END Points.
