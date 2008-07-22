DEFINITION MODULE E7004; (* 28-Sep-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;

VAR
  BCB_i_byte_adr  : ADDRESS;
  BCB_o_byte_adr  : ADDRESS;
  BCB_word_adr    : POINTER TO ARRAY [0..0FFh] OF CHAR;
  Ch              : CHAR;

PROCEDURE get;

END E7004.
