DEFINITION MODULE LabtamVDU; (* Leo 27-Jun-88. (c) KRONOS *)

TYPE
  Line = ARRAY [0..24] OF BITSET;
  Lay  = POINTER TO ARRAY [0..299] OF Line;

VAR
  layer: ARRAY [0..1] OF Lay;

PROCEDURE auto(refresh: BOOLEAN);
(* start/stop auto refresh. DEFAULT: TRUE *)


------------------------ Addition procedures ---------------------

PROCEDURE runVDU;                       PROCEDURE stopVDU;

PROCEDURE running(): BOOLEAN;

PROCEDURE block(lay,x,y: INTEGER; w,h: INTEGER);
(* transfer rectangle to bitmap *)

PROCEDURE update;
(* waits until previos -block- transfered on to bitmap *)

PROCEDURE ready(): BOOLEAN;
(* transfer of previos -block- finished? *)

(* for super-hackers only: *)

TYPE bcb86 = POINTER TO
  RECORD
    flag: INTEGER;
    len : INTEGER;
    hei : INTEGER;
    ofs : INTEGER;
  END;

VAR control: bcb86;

END LabtamVDU.
