DEFINITION MODULE pedWindow; (* Sem 03-Dec-87. (c) KRONOS *)

VAR InChain,InMacro: BOOLEAN;
    Scales         : ARRAY [0..5] OF INTEGER;
    Speed, Zoom    : INTEGER;
    LastX,LastY    : INTEGER;

PROCEDURE Command(): CHAR;

END pedWindow.
