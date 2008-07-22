DEFINITION MODULE Points; (* Sem 27-Jun-87. (c) KRONOS *)

TYPE pPoint=POINTER TO Point;
     ppPoint=POINTER TO pPoint;
     Point=RECORD
       x,y,l      : CARDINAL; (* Координаты точки *)
       v          : CARDINAL; (* Расстояние от источника *)
       d          : CARDINAL; (* Направление *)
       id         : CARDINAL;
       stopped    : BOOLEAN;
       hard       : BOOLEAN;
       Xfwd,Dfwd  : pPoint;
       Xback      : ppPoint;
       Dback      : pPoint;
     END;

PROCEDURE NewPoint(x,y,l,v,d,id: CARDINAL; hd: BOOLEAN);

PROCEDURE NewIdent(): CARDINAL;

PROCEDURE FindPoint(x,y,l,id: CARDINAL): pPoint;

PROCEDURE FindOther(x,y,l,id: CARDINAL): pPoint;

PROCEDURE FindMin(): pPoint;

PROCEDURE KilLastPoint;

PROCEDURE Cat(from,to: CARDINAL);

PROCEDURE RemovePoints;

PROCEDURE DeallocPoints;

PROCEDURE ShotWave;

END Points.
