MODULE ascii; (* ??? ??-Aug-86. (c) KRONOS *)
              (* Ned 03-Mar-90. (c) KRONOS *)

IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  std: StdIO;
IMPORT args: tskArgs;

PROCEDURE pusage;
BEGIN
  std.print('usage:\n  ascii [-tre]\n');
END pusage;

PROCEDURE table;
  VAR i: INTEGER;
BEGIN
  std.print("\n       ");
  FOR i:=0 TO 15 DO std.print(" %$2b",i) END;
  FOR i:=0 TO 255 DO
    IF i MOD 16 = 0 THEN std.print("\n%$3bc:  ",i) END;
    IF i MOD 128 < 40b THEN
      std.print(" ^%c",ORD("@")+(i MOD 32)+INTEGER(BITSET(i)*{7}));
    ELSE
      std.print("  %c",i);
    END;
  END;
  std.print("\n");
END table;

PROCEDURE vis(ch: CHAR);
BEGIN
  IF ORD(ch) MOD 128 < 40b THEN
    tty.print('"^%c" = ',ORD(ch) MOD 32 + ORD('@'));
  ELSE
    tty.print(' "%c" = ',ch);
  END;
  tty.print("%$3bc [%$3#h,%$3d]\n",ch,ch,ch);
END vis;

VAR i: INTEGER;
   ch: CHAR;
  raw: BOOLEAN;

BEGIN
  IF args.flag('-','h') THEN pusage; HALT END;
  IF args.flag('-','t') THEN table;  HALT END;
  raw :=args.flag('-','r');
  i:=0;
  LOOP
    IF raw THEN key.set_raw(1) END;
    key.read(ch);
    IF raw THEN key.set_raw(0) END;
    vis(ch);
    IF ch=3c THEN EXIT END
  END
END ascii.
