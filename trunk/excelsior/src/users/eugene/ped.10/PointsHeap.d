DEFINITION MODULE PointsHeap; (* Sem 31-Dec-87. (c) KRONOS *)

FROM Points     IMPORT  pPoint;

PROCEDURE Allocate(VAR p: pPoint);

PROCEDURE Deallocate(VAR p: pPoint);

END PointsHeap.
