MODULE sethost; (*  17-Dec-91. (c) KRONOS *)

IMPORT  err: defErrors;
IMPORT  arg: tskArgs;
IMPORT  ip : ip;

VAR r: INTEGER;

BEGIN
  IF NOT arg.number("h",r) THEN HALT(1) END;
  r:=ip.sethost(r);
  IF r#err.ok THEN HALT(r) END;
END sethost.
