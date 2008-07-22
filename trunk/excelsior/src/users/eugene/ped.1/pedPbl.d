DEFINITION MODULE pedPbl; (* Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   WORD;
FROM pedModel  IMPORT   signal;

VAR
  Layer  : INTEGER;
  CursorX: INTEGER;
  CursorY: INTEGER;
  Fixed  : BOOLEAN;
  Signal : signal;
  Boxed  : BOOLEAN;
  BoxX1,BoxX2,BoxY1,BoxY2: INTEGER;

CONST
  Clearens =8;   (* 200 mkm *)
  Overlay  =2;   (*  50 mkm *)
  LayersNo =2;
  MaxLayers=6;
  Grid     =24;  (* 625 mkm *)

PROCEDURE ShowStat;

END pedPbl.
