MODULE mou; (* Leg 12-Apr-90. (c) KRONOS *)

IMPORT  bio: BIO;
IMPORT  err: defErrors;
IMPORT  arg: tskArgs;
IMPORT  tty: Terminal;

PROCEDURE pusage;
BEGIN
  tty.print(
      '   "mou"  device mount utility program  (c) KRONOS\n'
      'usage:\n'
      '    mou file_name [dev_name] [-wra12]\n'
      '      mount device "dev_name" to directory "file_name"\n'
      '      or unmount directory "dev_name"\n'
      '\n                                     Leg, Apr 12 90\n');
  HALT
END pusage;

PROCEDURE unmount(dir: ARRAY OF CHAR; how: INTEGER);
BEGIN
  bio.unmount(dir,how);
  IF bio.done THEN
    tty.print('"%s" is released\n',dir);
  ELSE
    IF (how=1) & (bio.error=err.busy) THEN
      tty.print('"%s" is released, but it was busy\n',dir);
    ELSE
      tty.perror(bio.error,"can't unmount "'"%s": %%s\n',dir);
    END;
  END;
END unmount;

PROCEDURE mount(dir,dev: ARRAY OF CHAR; ro: BOOLEAN);
  VAR label: ARRAY [0..15] OF CHAR;
      i    : INTEGER;

  PROCEDURE ready;
  BEGIN
    IF (label#"UL") & (label#"") THEN
      tty.print('"%s" [%s] mounted at "%s"\n',dev,label,dir)
    ELSE
      tty.print('"%s" mounted at "%s"\n',dev,dir)
    END
  END ready;

BEGIN
  bio.mount(dir,dev,'',label,ro);
  IF bio.done THEN ready; RETURN END;
  IF NOT arg.flag('-','a') OR (bio.error#err.busy) THEN
    tty.perror(bio.error,"Can't mount "'"%s" at "%s": %%s\n',dev,dir);
    HALT(bio.error)
  END;
  bio.unmount(dir,2);
  IF NOT bio.done THEN
    tty.perror(bio.error,"Can't unmount "'"%s" at "%s": %%s\n',dev,dir);
    HALT(bio.error)
  END;
  bio.mount(dir,dev,'',label,ro);
  IF bio.done THEN ready; RETURN END;
  tty.perror(bio.error,"Can't mount "'"%s" at "%s": %%s\n',dev,dir);
END mount;

VAR m: INTEGER;

BEGIN
  IF (HIGH(arg.words)<0) OR arg.flag('-','h') THEN pusage END;
  IF arg.flag('-','r') THEN
    m:=0;
    IF arg.flag('-','1') THEN m:=1 END;
    IF arg.flag('-','2') THEN m:=2 END;
    unmount(arg.words[0],m);
  ELSE
    IF (HIGH(arg.words)<1) THEN pusage END;
    mount(arg.words[0],arg.words[1],arg.flag('-','w'))
  END;
END mou.
