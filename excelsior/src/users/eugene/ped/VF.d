DEFINITION MODULE VF; IMPORT SYSTEM; (* Leo 23-Aug-88. (c) KRONOS *)

(******************************************************************)
(*                                                                *)
(*                      V i d e o   F o n t                       *)
(*                                                                *)
(******************************************************************)

TYPE
  Font = POINTER TO RECORD
    w,h : INTEGER;
    base: SYSTEM.ADDRESS;
    p_w : POINTER TO ARRAY CHAR OF INTEGER;
  END;

VAL
  main : Font;
  micro: Font;
  font : Font;

PROCEDURE make_font(name: ARRAY OF CHAR);

END VF.
