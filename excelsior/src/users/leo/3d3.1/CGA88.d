DEFINITION MODULE CGA88; (* Leo 23-Mar-90. (c) KRONOS *)

IMPORT  defBMG;

VAL C: INTEGER;

CONST
  W = 240; Imax = 10;
  H = 180; Imin =  1;

VAL bmd: defBMG.BITMAP;

PROCEDURE erase;

PROCEDURE color(c: INTEGER);
(*
   intensity=[0..8]  color=[0..7]

   0-black, 1-red,       2-green,  3-yellow,
   4-blue,  5-king blue, 6-violet, 7-white
*)

PROCEDURE line(c,x,y,x1,y1: INTEGER);

PROCEDURE hline(c,i,x1,y,x2: INTEGER);

END CGA88.
