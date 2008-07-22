DEFINITION MODULE pedGD; (* Sem 08-Dec-88. (c) KRONOS *)

TYPE
  Line=ARRAY [0..24] OF BITSET;
  Lay =ARRAY [0..299] OF Line;

VAR
  src: ARRAY [0..1] OF POINTER TO Lay;

PROCEDURE seg(x1,y1,x2,y2,sz: INTEGER);

END pedGD.
