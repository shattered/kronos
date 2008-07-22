DEFINITION MODULE pedPbl; (* Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   WORD;
FROM Model     IMPORT   Object;

VAR
  Layer:   CARDINAL;
  CursorX: CARDINAL;
  CursorY: CARDINAL;
  Fixed:   BOOLEAN;
  Signal:  Object;
  Boxed:   BOOLEAN;
  BoxX1,BoxX2,BoxY1,BoxY2: CARDINAL;

CONST
  Clearens =8;   (* 200 mkm *)
  Overlay  =2;   (*  50 mkm *)
  LayersNo =2;
  MaxLayers=6;
  Grid     =24;  (* 625 mkm *)

PROCEDURE ShowStat;

END pedPbl.
