DEFINITION MODULE adaNum; (* Sem 09-Sep-90. (c) KRONOS *)

TYPE
  long_int=RECORD
    CASE : INTEGER OF
      |0: i : ARRAY [0..3] OF INTEGER;
      |1: s : ARRAY [0..3] OF BITSET;
      |2: c : ARRAY [0..11] OF CHAR;
    END;
  END;
  long_float=RECORD
    man: long_int;
    exp: INTEGER;
    sig: BOOLEAN;
  END;

CONST
  bits_no=BYTES(long_int)*8;

VAR
  ovr : BOOLEAN;

END adaNum.
