DEFINITION MODULE exSetUp; (* 11-Jun-87. (c) KRONOS *)

VAR public:
  RECORD
    high,width: INTEGER; (* screen size             *)
    upmargin  : INTEGER; (* free lines at screen up *)
    dwmargin  : INTEGER; (* free lines at screen dw *)
    pagesize  : INTEGER; (* size of page skip       *)
    adjust    : INTEGER; (* left/right at bos/eos   *)
    ins       : BOOLEAN;
    bell      : BOOLEAN;
    info      : BOOLEAN;
    --- для форматирования текста ---
    fmargin   : INTEGER; -- отступ в первой строке абзаца от lmargin (+-)
    lmargin   : INTEGER; -- левый  отступ
    rmargin   : INTEGER; -- правый отступ
  END;

PROCEDURE set_up;

END exSetUp.
