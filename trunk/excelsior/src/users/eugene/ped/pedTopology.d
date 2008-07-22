DEFINITION MODULE pedTopology; (* Sem 06-Mar-87. (c) KRONOS *)

FROM Model      IMPORT  Object;
FROM pedEditor  IMPORT  Sheet;

CONST
  Clearens =8;   (* 200 mkm *)
  Overlay  =2;   (*  50 mkm *)
  LayersNo =2;
  MaxLayers=6;
  Grid     =24;  (* 625 mkm *)

PROCEDURE App(sht: Sheet);

PROCEDURE Del(sht: Sheet);

PROCEDURE Side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

VAR LineXs,LineYs,LineXe,LineYe: INTEGER;

PROCEDURE OnLine(x1,y1,x2,y2: INTEGER): BOOLEAN;

PROCEDURE InsertRange
  (size,layer: INTEGER; fix: BOOLEAN; sht: Sheet; sig: Object;
   chk: BOOLEAN): BOOLEAN;

PROCEDURE DeleteRange(layer: INTEGER; sig: Object; sht: Sheet);

PROCEDURE InsertVias
  (size,vsize: INTEGER; fix: BOOLEAN; sht: Sheet; sig: Object;
   chk: BOOLEAN): BOOLEAN;

PROCEDURE DeleteVias(sig: Object; sht: Sheet);

PROCEDURE FindSignal(x,y,l,size: INTEGER; sht: Sheet; sig: Object);

TYPE IterProc=PROCEDURE (): BOOLEAN;

PROCEDURE SeekInBox(sht: Sheet; ip: IterProc);

PROCEDURE Areas(sig: Object; sht: Sheet): INTEGER;

PROCEDURE Len(x1,y1,x2,y2,x,y: INTEGER): INTEGER;

PROCEDURE Shoted?(size,layer: INTEGER; sht: Sheet; sig: Object): BOOLEAN;

VAR ShotedSignal: Object;
    ShotedIdent : INTEGER;

END pedTopology.
