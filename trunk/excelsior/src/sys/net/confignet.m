MODULE confignet; (*  17-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  err: defErrors;
IMPORT  drv: netdrv;
IMPORT  ip : ip;

VAR r: INTEGER;
    h: ARRAY [0..0] OF ip.range;

BEGIN
  h[0].from:=001h;
  h[0].to  :=002h;
  r:=ip.definedriver("arc0",h,FALSE);
  IF r#err.ok THEN HALT(r) END
END confignet.
