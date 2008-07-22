DEFINITION MODULE ns_bcb; (* Ned 13-Jan-89. (c) KRONOS *)

VAL
  buffer: DYNARR OF  CHAR;
  buf_ma: INTEGER;

VAR
  bcb   : POINTER TO RECORD
    magic: INTEGER;  -- 00h
    free : BOOLEAN;  -- 04h
    cycle: INTEGER;  -- 08h
    cmd  : INTEGER;  -- 0Ch
    to   : INTEGER;  -- 10h  move or call
    from : INTEGER;  -- 14h  move
    len  : INTEGER;  -- 18h  of move
    res  : INTEGER;  -- 1Ch  res of call
  END;

PROCEDURE vis_bcb;

PROCEDURE move(from,to,bytes: INTEGER);

PROCEDURE call(pc: INTEGER; VAR res,time: INTEGER);

END ns_bcb.
