DEFINITION MODULE pedTopology; (* Sem 06-Mar-87. (c) KRONOS *)

FROM Model     IMPORT   Object;

PROCEDURE App;

PROCEDURE Del;

PROCEDURE Side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

VAR LineXs,LineYs,LineXe,LineYe: INTEGER;
    check_on: BOOLEAN;

PROCEDURE OnLine(x1,y1,x2,y2: INTEGER): BOOLEAN;

PROCEDURE InsertRange
  (size,layer: INTEGER; fix: BOOLEAN; mdl,sig: Object): BOOLEAN;

PROCEDURE DeleteRange(layer: INTEGER; sig: Object);

PROCEDURE InsertVias
  (size,vsize: INTEGER; fix: BOOLEAN; mdl,sig: Object): BOOLEAN;

PROCEDURE DeleteVias(sig: Object);

PROCEDURE FindSignal(x,y,l,size: INTEGER; mdl,sig: Object);

TYPE IterProc=PROCEDURE (): BOOLEAN;

PROCEDURE SeekInBox(mdl: Object; ip: IterProc);

PROCEDURE Areas(sig: Object): INTEGER;

PROCEDURE Len(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE Shoted?(size,layer: INTEGER; mdl,sig: Object): BOOLEAN;

VAR ShotedSignal: Object;
    ShotedIdent : INTEGER;

END pedTopology.
