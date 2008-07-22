DEFINITION MODULE RoutDeb; (* Sem 24-Dec-87. (c) KRONOS *)

FROM Model      IMPORT  Object;

PROCEDURE StartDeb(sig: Object);

PROCEDURE DebPnt(x,y,l,v,d: INTEGER);

PROCEDURE FinishDeb;

END RoutDeb.
